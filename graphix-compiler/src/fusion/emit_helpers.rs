#![allow(improper_ctypes_definitions)]
//! `extern "C"` entry points the JIT calls for ops that cannot be
//! lowered in CLIF. Every helper is declared through [`jit_helpers!`],
//! which derives its CLIF wire signature from the Rust types;
//! `emit/jit.rs` registers each symbol by pointer from [`all_helpers`]
//! and `emit/lower.rs` declares its signature.
//!
//! Two-word values cross the helper seam through pointers on every host
//! (`design/helper_abi_portability.md`).
//! `improper_ctypes_definitions` is suppressed because the outer layout
//! is stable even where a payload is not `repr(C)`.
//!
//! Pointer arguments must stay valid for the call. Element/field reads
//! are total: an out-of-bounds index or an unexpected slot shape reads
//! as the return shape's placeholder (0 / "" / empty array / `Null`),
//! because tainted placeholders are `Value::Null` in composite slots.

use crate::{
    FastFn, Tag,
    fusion::kernel_abi::{ActivationLayout, SiteLeaf},
    node::{
        array::{array_index, array_slice, value_index},
        collection::{self, Flavor},
        error::{QopSite, report_failure, report_ignored, unhandled_msg},
        list,
        map::map_get,
        op::{ArithOp, arith, neg},
    },
    rep,
    typ::{IsAFlags, Type},
};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    any::Any,
    cell::{Cell, RefCell},
    mem::{ManuallyDrop, MaybeUninit},
    panic::AssertUnwindSafe,
    sync::atomic::{AtomicU64, Ordering::Relaxed},
};

/// The Value ABI the helpers' two-`I64` signatures depend on: two
/// 8-byte words, and every externally-defined payload fits the second.
const _: () = {
    assert!(std::mem::size_of::<Value>() == 16);
    assert!(std::mem::align_of::<Value>() == 8);
    assert!(
        std::mem::size_of::<netidx_value::Map>() <= 8,
        "netidx_value::Map must fit in Value's 8-byte payload word"
    );
    assert!(
        std::mem::size_of::<arcstr::ArcStr>() <= 8,
        "arcstr::ArcStr must fit in Value's 8-byte payload word"
    );
};

pub use crate::tval::TagValue;

/// One register slot of a helper's wire signature. The C ABI has the
/// CALLER extend sub-register integers, so `emit/lower.rs` applies the u/s
/// flags to parameters only; returns are read at their narrow type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AbiTy {
    I64,
    I32,
    F64,
    F32,
    /// I16, zero-extended by the caller when a parameter.
    I16u,
    /// I16, sign-extended by the caller when a parameter.
    I16s,
    /// I8, zero-extended by the caller when a parameter (u8, bools).
    I8u,
    /// I8, sign-extended by the caller when a parameter.
    I8s,
    /// A two-word struct: two values at the call site, and on the wire a
    /// pointer to them as a parameter, or a pointer the helper writes
    /// them through as a return. No C ABI agrees on a struct by value.
    Pair,
}

/// The wire slots a Rust type occupies as a helper parameter, one per
/// register, and the word it crosses the seam as. A type without an impl
/// cannot appear in a helper signature.
pub(crate) trait HelperArg: Sized {
    const ABI: &'static [AbiTy];
    type Wire;

    /// SAFETY: `w` is what a call site passes for this parameter.
    unsafe fn from_wire(w: Self::Wire) -> Self;
}

/// The wire slots of a helper return; `()` returns nothing.
pub(crate) trait HelperRet {
    const ABI: &'static [AbiTy];
}

impl<T: HelperArg> HelperRet for T {
    const ABI: &'static [AbiTy] = T::ABI;
}

impl HelperRet for () {
    const ABI: &'static [AbiTy] = &[];
}

macro_rules! impl_helper_arg {
    ($($t:ty => $abi:expr;)*) => {
        $(impl HelperArg for $t {
            const ABI: &'static [AbiTy] = $abi;
            type Wire = $t;

            unsafe fn from_wire(w: $t) -> $t {
                w
            }
        })*
    };
}

impl_helper_arg! {
    u64 => &[AbiTy::I64];
    i64 => &[AbiTy::I64];
    usize => &[AbiTy::I64];
    u32 => &[AbiTy::I32];
    i32 => &[AbiTy::I32];
    u16 => &[AbiTy::I16u];
    i16 => &[AbiTy::I16s];
    u8 => &[AbiTy::I8u];
    i8 => &[AbiTy::I8s];
    f64 => &[AbiTy::F64];
    f32 => &[AbiTy::F32];
}

/// A pair parameter arrives as a pointer to the call site's two words,
/// which the helper takes over.
macro_rules! impl_pair_arg {
    ($($t:ty => $read:expr;)*) => {
        $(impl HelperArg for $t {
            const ABI: &'static [AbiTy] = &[AbiTy::Pair];
            type Wire = *const [u64; 2];

            unsafe fn from_wire(w: *const [u64; 2]) -> $t {
                $read(w)
            }
        })*
    };
}

impl_pair_arg! {
    TagValue => |w: *const [u64; 2]| unsafe { std::ptr::read(w.cast::<TagValue>()) };
    // a borrowed value: the helper reads it and never drops it
    ManuallyDrop<TagValue> => |w: *const [u64; 2]| {
        ManuallyDrop::new(unsafe { std::ptr::read(w.cast::<TagValue>()) })
    };
    ChildBlock => |w: *const [u64; 2]| unsafe { std::ptr::read(w.cast::<ChildBlock>()) };
}

/// A self-call's per-activation block and whether it was just made: a
/// fresh activation, born with the init view.
#[repr(C)]
pub struct ChildBlock {
    block: *mut u64,
    fresh: u64,
}

impl<T> HelperArg for *mut T {
    const ABI: &'static [AbiTy] = &[AbiTy::I64];
    type Wire = *mut T;

    unsafe fn from_wire(w: *mut T) -> *mut T {
        w
    }
}

impl<T> HelperArg for *const T {
    const ABI: &'static [AbiTy] = &[AbiTy::I64];
    type Wire = *const T;

    unsafe fn from_wire(w: *const T) -> *const T {
        w
    }
}

/// One registered helper: symbol name, address, and wire signature
/// (per-parameter slot lists + return slots).
pub(crate) struct HelperSpec {
    pub(crate) name: &'static str,
    pub(crate) ptr: *const u8,
    pub(crate) params: &'static [&'static [AbiTy]],
    pub(crate) ret: &'static [AbiTy],
}

macro_rules! jit_helper_ret {
    () => {
        <() as HelperRet>::ABI
    };
    ($t:ty) => {
        <$t as HelperRet>::ABI
    };
}

/// One helper's `unsafe extern "C"` definition: each parameter crosses
/// as its [`HelperArg::Wire`], and a pair return (`-> TagValue`,
/// `-> ChildBlock`) through a trailing out-pointer. Every helper is
/// unsafe to call: the wire forms carry pointers its body reads.
macro_rules! jit_helper_fn {
    ([$($attr:meta)*] $name:ident ($($p:ident: $t:ty),*) [pair $ret:ty] $body:block) => {
        $(#[$attr])*
        pub(crate) unsafe extern "C" fn $name($($p: <$t as HelperArg>::Wire,)* out: *mut $ret) {
            // SAFETY: the call site passes each parameter's wire form
            $(let $p: $t = unsafe { <$t as HelperArg>::from_wire($p) };)*
            let r: $ret = (move || $body)();
            // SAFETY: the call site passes a two-word slot
            unsafe { out.write(r) }
        }
    };
    ([$($attr:meta)*] $name:ident ($($p:ident: $t:ty),*) [$($ret:ty)?] $body:block) => {
        $(#[$attr])*
        pub(crate) unsafe extern "C" fn $name($($p: <$t as HelperArg>::Wire),*) $(-> $ret)? {
            // a pair return is spelled `-> TagValue` or `-> ChildBlock`, the
            // out-pointer arm: by value, its words would never be written
            $(const {
                assert!(
                    !matches!(<$ret as HelperRet>::ABI, [AbiTy::Pair]),
                    "a pair return must be written -> TagValue or -> ChildBlock"
                )
            };)?
            // SAFETY: the call site passes each parameter's wire form
            $(let $p: $t = unsafe { <$t as HelperArg>::from_wire($p) };)*
            $body
        }
    };
}

/// Declare JIT helpers: each entry is an ordinary `fn`. Emits the
/// `pub unsafe extern "C"` definitions plus a `$registry()` function
/// pushing one [`HelperSpec`] per helper.
macro_rules! jit_helpers {
    (@parse [$registry:ident] [$({ $name:ident; ($($t:ty),*); ($($ret:ty)?) })*]) => {
        fn $registry(v: &mut Vec<HelperSpec>) {
            $(v.push(HelperSpec {
                name: stringify!($name),
                ptr: $name as *const u8,
                params: &[$(<$t as HelperArg>::ABI),*],
                ret: jit_helper_ret!($($ret)?),
            });)*
        }
    };
    (@parse [$registry:ident] [$($acc:tt)*]
        $(#[$attr:meta])*
        fn $name:ident($($p:ident: $t:ty),* $(,)?) -> TagValue $body:block
        $($rest:tt)*
    ) => {
        jit_helper_fn!([$($attr)*] $name ($($p: $t),*) [pair TagValue] $body);
        jit_helpers!(@parse [$registry]
            [$($acc)* { $name; ($($t),*); (TagValue) }] $($rest)*);
    };
    (@parse [$registry:ident] [$($acc:tt)*]
        $(#[$attr:meta])*
        fn $name:ident($($p:ident: $t:ty),* $(,)?) -> ChildBlock $body:block
        $($rest:tt)*
    ) => {
        jit_helper_fn!([$($attr)*] $name ($($p: $t),*) [pair ChildBlock] $body);
        jit_helpers!(@parse [$registry]
            [$($acc)* { $name; ($($t),*); (ChildBlock) }] $($rest)*);
    };
    (@parse [$registry:ident] [$($acc:tt)*]
        $(#[$attr:meta])*
        fn $name:ident($($p:ident: $t:ty),* $(,)?) $(-> $ret:ty)? $body:block
        $($rest:tt)*
    ) => {
        jit_helper_fn!([$($attr)*] $name ($($p: $t),*) [$($ret)?] $body);
        jit_helpers!(@parse [$registry]
            [$($acc)* { $name; ($($t),*); ($($ret)?) }] $($rest)*);
    };
    (registry = $registry:ident; $($items:tt)*) => {
        jit_helpers!(@parse [$registry] [] $($items)*);
    };
}

/// Every registered helper.
pub(crate) fn all_helpers() -> Vec<HelperSpec> {
    let mut v = Vec::new();
    buf_helpers(&mut v);
    value_buf_push_helpers(&mut v);
    control_helpers(&mut v);
    value_helpers(&mut v);
    variant_payload_helpers(&mut v);
    abstract_get_helpers(&mut v);
    string_helpers(&mut v);
    string_buf_push_helpers(&mut v);
    string_bool_helpers(&mut v);
    collection_helpers(&mut v);
    elem_helpers(&mut v);
    valarray_get_helpers(&mut v);
    struct_get_helpers(&mut v);
    #[cfg(debug_assertions)]
    debug_helpers(&mut v);
    v
}

// A composite travels through the JIT as its `ValArray` bits in a `u64`:
// the zero pending sentinel would violate `ValArray`'s NonNull niche as a
// typed parameter. Owned bits drop exactly once; borrowed bits never do.

/// Borrow ValArray bits for the duration of a call.
///
/// SAFETY: `bits` are the bits of a `ValArray` that is alive for the
/// borrow.
#[inline]
unsafe fn va_ref(bits: &u64) -> &ValArray {
    assert!(*bits != 0, "graphix: zero ValArray bits — JIT codegen bug");
    unsafe { &*(bits as *const u64 as *const ValArray) }
}

/// Take ownership of ValArray bits.
///
/// SAFETY: `bits` are the bits of a `ValArray` the caller owns.
#[inline]
unsafe fn va_owned(bits: u64) -> ValArray {
    assert!(bits != 0, "graphix: zero ValArray bits — JIT codegen bug");
    unsafe { std::mem::transmute::<u64, ValArray>(bits) }
}

/// Release an ArcStr as owned bits.
#[inline]
fn arcstr_bits(s: arcstr::ArcStr) -> u64 {
    unsafe { std::mem::transmute::<arcstr::ArcStr, u64>(s) }
}

/// The ArcStr a kernel's string word holds. A zero word is the pending
/// sentinel, never a string: a codegen bug `helper` names.
///
/// # Safety
///
/// `bits` came from an ArcStr-producing helper; a borrowed word's
/// result is forgotten, never dropped.
#[inline]
unsafe fn arcstr_from_bits(bits: u64, helper: &str) -> arcstr::ArcStr {
    assert!(bits != 0, "{helper}: null ArcStr — JIT codegen bug");
    unsafe { std::mem::transmute::<u64, arcstr::ArcStr>(bits) }
}

/// Release a ValArray as owned bits.
#[inline]
fn va_bits(a: ValArray) -> u64 {
    unsafe { std::mem::transmute::<ValArray, u64>(a) }
}

/// The bits of a borrowed `&ValArray`, no refcount bump; never dropped.
#[inline]
fn va_borrowed_bits(a: &ValArray) -> u64 {
    unsafe { *(a as *const ValArray as *const u64) }
}

// Producer ops build a `Vec<Value>` through `graphix_value_buf_*` and
// finalize it into a `ValArray`. Every buf pointer is owned and must be
// finalized or dropped exactly once.

type ValueBuf = LPooled<Vec<Value>>;
type StringBuf = LPooled<String>;

/// The boxes a builder's pointer names, kept empty for reuse. The
/// builder itself comes from its pool and goes back to it, under the
/// pool's limits; only the box is recycled here.
struct Shells<T>(RefCell<Vec<Box<MaybeUninit<T>>>>);

const SHELLS_MAX: usize = 64;

impl<T> Shells<T> {
    const fn new() -> Self {
        Self(RefCell::new(Vec::new()))
    }

    fn take(&self, builder: T) -> *mut T {
        let shell = self.0.borrow_mut().pop();
        let mut shell = shell.unwrap_or_else(Box::new_uninit);
        shell.write(builder);
        Box::into_raw(shell).cast()
    }

    /// Drop the builder at `p`, which came from `take`.
    unsafe fn give(&self, p: *mut T) {
        let mut shell = unsafe { Box::from_raw(p.cast::<MaybeUninit<T>>()) };
        unsafe { shell.assume_init_drop() };
        let mut s = self.0.borrow_mut();
        if s.len() < SHELLS_MAX {
            s.push(shell);
        }
    }
}

thread_local! {
    static VALUE_SHELLS: Shells<ValueBuf> = const { Shells::new() };
    static STRING_SHELLS: Shells<StringBuf> = const { Shells::new() };
}

fn buf_take(cap: usize) -> *mut ValueBuf {
    let buf = VALUE_SHELLS.with(|s| s.take(LPooled::take()));
    unsafe { (*buf).reserve(cap) };
    buf
}

unsafe fn buf_give(buf: *mut ValueBuf) {
    VALUE_SHELLS.with(|s| unsafe { s.give(buf) })
}

jit_helpers! { registry = buf_helpers;

fn graphix_value_buf_new(cap: usize) -> *mut LPooled<Vec<Value>> {
    buf_take(cap)
}

/// Push a `Value::Array` slot, taking ownership of `inner`.
fn graphix_value_buf_push_array(buf: *mut LPooled<Vec<Value>>, inner: u64) {
    unsafe { (*buf).push(Value::Array(va_owned(inner))) }
}

/// Extend `buf` with the elements of an owned ValArray, then drop it.
fn graphix_value_buf_extend_from_array(buf: *mut LPooled<Vec<Value>>, inner: u64) {
    let v = Value::Array(unsafe { va_owned(inner) });
    Flavor::Array.extend(unsafe { &mut *buf }, &v)
}

/// Push a clone of borrowed ValArray bits; the caller keeps its ref.
fn graphix_value_buf_push_array_borrowed(buf: *mut LPooled<Vec<Value>>, src: u64) {
    unsafe { (*buf).push(Value::Array(va_ref(&src).clone())) }
}

/// Push a clone of a borrowed Value; the caller keeps its ref.
fn graphix_value_buf_push_value_borrowed(buf: *mut LPooled<Vec<Value>>, v: ManuallyDrop<TagValue>) {
    let dup = v.with_value(|v| v.clone());
    unsafe { (*buf).push(dup) }
}

/// Push an owned Value, consuming it. The buffer holds clean Values:
/// the tag is stripped here.
fn graphix_value_buf_push_value(buf: *mut LPooled<Vec<Value>>, tv: TagValue) {
    unsafe { (*buf).push(tv.value()) }
}

/// Push a clone of a borrowed filter_map result's kept element.
fn graphix_value_buf_push_kept_borrowed(buf: *mut LPooled<Vec<Value>>, v: ManuallyDrop<TagValue>) {
    let dup = v.with_value(|v| collection::kept(v).clone());
    unsafe { (*buf).push(dup) }
}

/// Push an owned filter_map result's kept element, consuming the result.
fn graphix_value_buf_push_kept(buf: *mut LPooled<Vec<Value>>, tv: TagValue) {
    let v = tv.value();
    let k = collection::kept(&v);
    let k = if std::ptr::eq(k, &v) { v } else { k.clone() };
    unsafe { (*buf).push(k) }
}

/// Drop a buf that never reached `finalize`.
fn graphix_value_buf_drop(buf: *mut LPooled<Vec<Value>>) {
    assert!(!buf.is_null(), "graphix_value_buf_drop: null buf — JIT codegen bug");
    unsafe { buf_give(buf) }
}

/// Push the marker of the kind `kind` (a `rep::Kind`), which opens a
/// tuple, struct or variant.
fn graphix_value_buf_push_marker(buf: *mut LPooled<Vec<Value>>, kind: u64) {
    let kind = rep::Kind::from_u8(kind as u8).expect("a kind marker");
    unsafe { (*buf).push(rep::marker(kind)) }
}

/// Push a `Value::String` cloned from a kernel strings-table slot.
fn graphix_value_buf_push_arcstr(
    buf: *mut LPooled<Vec<Value>>,
    ptr: *const arcstr::ArcStr,
) {
    unsafe { (*buf).push(Value::String((*ptr).clone())) }
}

/// Push an owned `ArcStr` as `Value::String`, consuming it.
fn graphix_value_buf_push_string(buf: *mut LPooled<Vec<Value>>, s: u64) {
    let s = unsafe { arcstr_from_bits(s, "graphix_value_buf_push_string") };
    unsafe { (*buf).push(Value::String(s)) }
}

/// Finalize the buffer into owned `ValArray` bits, consuming the buf.
fn graphix_valarray_finalize(buf: *mut LPooled<Vec<Value>>) -> u64 {
    unsafe {
        let bits = va_bits(ValArray::from_iter_exact((*buf).drain(..)));
        buf_give(buf);
        bits
    }
}

/// Borrowed ValArray bits → owned bits (refcount bump).
fn graphix_valarray_clone(bits: u64) -> u64 {
    va_bits(unsafe { va_ref(&bits) }.clone())
}

/// Drop owned ValArray bits.
fn graphix_valarray_drop(bits: u64) {
    drop(unsafe { va_owned(bits) })
}

/// Extend `buf` with a list value's elements, consuming it, as the
/// node-walk's `flat_map` does (`Flavor::extend`).
fn graphix_value_buf_extend_from_list(
    buf: *mut LPooled<Vec<Value>>,
    tv: TagValue,
) {
    Flavor::List.extend(unsafe { &mut *buf }, &tv.value())
}

}

jit_helpers! { registry = control_helpers;

/// A fused call returned a Value whose shape is not its declared return
/// type: a builtin violating its signature, or a compiler bug. Either
/// way an invariant is gone, and a panic here aborts (the helper ABI
/// cannot unwind), which is what the fuzzer records as a crash.
fn graphix_shape_mismatch(got_disc: u64) {
    panic!(
        "fused call returned a Value whose shape (disc {got_disc:#x}) does not \
         match its declared return type"
    )
}

/// Rust's float `%`, which cranelift has no instruction for.
fn graphix_f64_rem(a: f64, b: f64) -> f64 {
    a % b
}

/// Rust's float `%`, which cranelift has no instruction for.
fn graphix_f32_rem(a: f32, b: f32) -> f32 {
    a % b
}

/// A collection init count past the element limit, on the cycle it fired.
fn graphix_init_oversize(n: i64) {
    crate::node::collection::log_init_oversize(n)
}

/// Set `KERNEL_ABORT`; emitted on every whole-kernel abort path.
fn graphix_abort_set() {
    KERNEL_ABORT.with(|c| c.set(true))
}

/// A fused select matched no arm: a hole in the check's coverage, said
/// once per site, as the node-walk says it once per select.
fn graphix_select_no_match(site: *const super::emit::NoMatchSite) {
    let site = unsafe { &*site };
    if !site.said.swap(true, Relaxed) {
        crate::node::error::report_coverage_hole(&format_args!(
            "{}: no arm matches",
            site.spec
        ));
    }
}

/// Run an outlined loop's `len` slots through its `chunk` over `frame`,
/// in order or forked (`fusion::par_loop`), filling `out`; 1 when a
/// chunk aborted the kernel.
fn graphix_par_loop(
    chunk: u64,
    frame: u64,
    len: u64,
    site: *const crate::cost::LoopSite,
    kind: u64,
    out: *mut u64,
) -> i8 {
    unsafe { super::par_loop::run(chunk, frame, len, site, kind, out) }
}

/// A `$` or handler-less `?` site swallowing a fresh error: the same
/// diagnostic the node-walk emits. `site` is the interned "origin at
/// position" string; `(disc, payload)` is the error Value, borrowed.
fn graphix_swallowed_error(
    site: *const arcstr::ArcStr,
    unhandled: u8,
    disc: u64,
    payload: u64,
) {
    // SAFETY: the words are a valid clean `Value`; viewed, never owned.
    let tv = ManuallyDrop::new(unsafe { TagValue::from_raw(disc, payload) });
    // SAFETY: the kernel's interned string outlives this invocation.
    let site = unsafe { &*site };
    guard_panic((), || {
        tv.with_value(|v| {
            if let Value::Error(e) = v {
                if unhandled == 0 {
                    report_ignored!(site, e)
                } else {
                    report_failure!(&unhandled_msg(site, e))
                }
            }
        })
    })
}

/// Raise a `?` site's error onto the invocation's delivery queue
/// (`DEFERRED`). `(disc, payload)` is the error Value, borrowed: the
/// queue takes a clone. `FusedKernel::update` drains the queue in order.
fn graphix_qop_raise(site: u64, disc: u64, payload: u64) {
    // SAFETY: the words are a valid clean `Value`; viewed, never owned.
    let tv = ManuallyDrop::new(unsafe { TagValue::from_raw(disc, payload) });
    let v = tv.value_cloned();
    // SAFETY: the kernel's interned QopSite outlives this invocation.
    let site = unsafe { &*(site as *const QopSite) };
    DEFERRED.with(|q| q.borrow_mut().raises.push((site, v)));
}

/// Raise a `?` site's `NullError` for a fresh null operand onto the
/// invocation's delivery queue (`DEFERRED`).
fn graphix_qop_raise_null(site: u64) {
    // SAFETY: the kernel's interned QopSite outlives this invocation.
    let site = unsafe { &*(site as *const QopSite) };
    let e = Value::Error(site.values.null_error(&site.spec).into());
    DEFERRED.with(|q| q.borrow_mut().raises.push((site, e)));
}

/// A handler-less `?` site meeting a fresh null: the same diagnostic
/// the node-walk emits, interned whole as `msg`.
fn graphix_unhandled_null(msg: *const arcstr::ArcStr) {
    // SAFETY: the kernel's interned string outlives this invocation.
    report_failure!(unsafe { &*msg });
}

/// 1 if the active runtime has an `interrupt()`/`abort()` pending or
/// the run recorded a panic ([`guard_panic`]), else 0. Emitted at every
/// JIT loop head.
fn graphix_interrupted() -> i8 {
    i8::from(run_stops())
}

/// The kernel twin of `stack::ensure_sufficient`, asked at every native
/// self-call: 0 = interrupted or panicked (skip the call), 1 = call
/// directly, 2 = re-enter the callee on a fresh segment via
/// [`graphix_grow_stack`].
fn graphix_stack_check() -> i8 {
    if run_stops() {
        0
    } else if stacker::remaining_stack().unwrap_or(0) < crate::stack::RED_ZONE {
        if crate::stack::grow_exceeds_budget() {
            crate::stack::budget_abort();
            0
        } else {
            2
        }
    } else {
        1
    }
}

/// Run a kernel's `__spill` thunk on a fresh stack segment: `args` is
/// the caller's spilled parameter words in signature order, `out` two
/// words the thunk fills with the (disc, payload) result.
fn graphix_grow_stack(thunk: i64, args: i64, out: i64) {
    // SAFETY: `thunk` has the fixed spill signature this crate emits;
    // `args`/`out` are the calling kernel's stack slots, live for the call.
    let f: extern "C" fn(i64, i64) = unsafe { std::mem::transmute(thunk as usize) };
    crate::stack::grow(|| f(args, out))
}

/// Call a builtin's registered `FastFn` directly. `args`/`n` is the
/// call site's stack buffer of (disc, payload) pairs, borrowed. See
/// [`fast_dispatch`] for the tag rules.
fn graphix_fastcall(
    fn_ptr: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> TagValue {
    // SAFETY: `fn_ptr` is the `FastFn` constant the kernel's record names.
    let f: FastFn =
        unsafe { std::mem::transmute::<usize, FastFn>(fn_ptr as usize) };
    unsafe { fast_dispatch(|args| f(args), args, n, taint_mask, stale_mask) }
}

/// Call an `Effect::Io` builtin's result fn with [`graphix_fastcall`]'s
/// buffer and tag rules, and when its trigger (`trigger`, a mask bit)
/// fired with no argument tainted, queue the call (`DEFERRED`) for the
/// wrapper to perform after the run.
fn graphix_iocall(
    fn_ptr: u64,
    site: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
    trigger: u64,
) -> TagValue {
    // SAFETY: `fn_ptr` is the `FastFn` constant the kernel's record names.
    let f: FastFn =
        unsafe { std::mem::transmute::<usize, FastFn>(fn_ptr as usize) };
    let tv = unsafe { fast_dispatch(|args| f(args), args, n, taint_mask, stale_mask) };
    if taint_mask == 0 && stale_mask & trigger == 0 {
        // SAFETY: the call site's buffer holds `n` clean Values for the call.
        let args = unsafe { std::slice::from_raw_parts(args as *const Value, n as usize) };
        let call = (site as *const crate::IoSite, args.iter().cloned().collect());
        DEFERRED.with(|q| q.borrow_mut().io.push(call));
    }
    tv
}

/// Call a `TypedFastFn` with the site's interned `Type` and the
/// invoking kernel's env loan ([`with_kernel_env`]). Same buffer and
/// tag rules as [`graphix_fastcall`].
fn graphix_typedcall(
    fn_ptr: u64,
    typ: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> TagValue {
    // SAFETY: `fn_ptr` and `typ` are constants of the kernel's record,
    // which outlives its code; the site interns its function type.
    let f: crate::TypedFastFn =
        unsafe { std::mem::transmute::<usize, crate::TypedFastFn>(fn_ptr as usize) };
    let Type::Fn(ft) = (unsafe { &*(typ as *const Type) }) else {
        unreachable!("a typed fast call's type is its site's function type")
    };
    let env = unsafe { kernel_env() };
    unsafe { fast_dispatch(|args| f(env, ft, args), args, n, taint_mask, stale_mask) }
}

/// A cast site's call: [`graphix_typedcall`] over its `(target, source)`
/// pair.
fn graphix_castcall(
    fn_ptr: u64,
    typ: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> TagValue {
    // SAFETY: as graphix_typedcall's
    let f: crate::fusion::lowering::CastFn = unsafe {
        std::mem::transmute::<usize, crate::fusion::lowering::CastFn>(fn_ptr as usize)
    };
    let typ = unsafe { &*(typ as *const Type) };
    let env = unsafe { kernel_env() };
    unsafe { fast_dispatch(|args| f(env, typ, args), args, n, taint_mask, stale_mask) }
}

}

/// Run `f`, which may run Graphix code (a fast fn, a value's `Eq`/`Ord`
/// impl): a panic there is recorded and `placeholder` stands for the
/// result. The run leaves by its abort path at its next poll (a loop
/// head, a self-call), releasing what it owns as it would; the kernel
/// then discards what it deferred and resumes the panic.
// XCR claude for claude: [risk/medium] A recorded panic does not stop the run. Nothing
// in the JIT code reads KERNEL_PANIC again: graphix_interrupted and graphix_stack_check
// check only stack::interrupted(). The kernel goes on with the placeholder, and several
// placeholders are untainted values it branches on (graphix_value_eq and
// graphix_value_cmp give 0, graphix_string_buf_push_value appends nothing). Example: a
// fused tail loop `select K(i) == K(13) { true => i, false => go(i + 1) }` whose Eq
// hook panics at 13 loops forever on "not equal", and a non-tail self-call recurses
// until memory runs out. The node-walk unwinds and the runtime dies. Fix: once a panic
// is recorded, have graphix_interrupted and graphix_stack_check report "interrupted" so
// the run leaves by its abort path at the next poll; then drop the !kernel_panicked()
// check in graphix_iocall and make fast_dispatch's doc ("it aborts the kernel") true.
// Found by reading; not run (only graphix-tests registers a panicking builtin).
// (f-helpers-01)
// 2026-10-27 claude: graphix_interrupted and graphix_stack_check now ask run_stops():
// an interrupt or a panic the run recorded, so a kernel leaves by its abort path at its
// next loop poll or self-call once a hook or fast fn panicked; graphix_iocall's own
// panicked check is gone (a panicked run's Io is cleared anyway), and guard_panic's doc
// says the run leaves. Pin: graphix-tests
// lang::test_builtins::a_panic_stops_a_kernel_loop (a fused tail loop and a non-tail
// self-call whose Eq hook panics at K(13); jit/jit_par count 990 panics with the fix
// undone, under 100 with it, the loop polling every 64 iterations).
fn guard_panic<T>(placeholder: T, f: impl FnOnce() -> T) -> T {
    match std::panic::catch_unwind(AssertUnwindSafe(f)) {
        Ok(r) => r,
        Err(payload) => {
            set_kernel_panic(payload);
            placeholder
        }
    }
}

/// The trampoline core. The argument discs decide the tag: a tainted
/// argument bottoms the result without calling, all-stale arguments
/// make it STALE, and `None` from the fn is this cycle's bottom. A
/// panic in the fn cannot unwind through the kernel: it aborts the
/// kernel and `FusedKernel::update` resumes it ([`guard_panic`]), as the
/// node-walk's call would have unwound.
///
/// SAFETY: `args` is `n` valid clean `Value`s on the call site's stack,
/// viewed and never owned; the site releases what it owned afterwards.
unsafe fn fast_dispatch(
    call: impl FnOnce(&[Value]) -> Option<Value>,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> TagValue {
    let args_vec: &[Value] =
        unsafe { std::slice::from_raw_parts(args as *const Value, n as usize) };
    let n = n as usize;
    // A zero-argument call site is refused at emission.
    let all_stale = n > 0 && stale_mask == u64::MAX >> (64 - n);
    let bottom = if all_stale { Tag::STALE_BOTTOM } else { Tag::FRESH_BOTTOM };
    let tv = if taint_mask != 0 {
        TagValue::tagged(Value::Null, bottom)
    } else {
        guard_panic(TagValue::tagged(Value::Null, bottom), || match call(args_vec) {
            Some(v) => {
                TagValue::tagged(v, if all_stale { Tag::STALE } else { Tag::FIRED })
            }
            None => TagValue::tagged(Value::Null, bottom),
        })
    };
    if crate::dbgenv::gxdbg_dync() {
        eprintln!(
            "FASTCALL n={n} taint={taint_mask:b} stale={stale_mask:b} -> {:?}",
            tv.tag()
        );
    }
    tv
}

/// Borrowed read of payload `payload_idx` (0 is the first payload,
/// `rep::payload`). A placeholder or short array reads as the default.
fn variant_payload_read<T: Default>(
    v: &TagValue,
    payload_idx: usize,
    read: impl Fn(&Value) -> T,
) -> T {
    v.with_value(|v| rep::payload(v, payload_idx).map(&read).unwrap_or_default())
}

/// The j-th spine cell of a list value; `None` on a short or malformed chain.
fn list_walk(v: &Value, j: usize) -> Option<&Value> {
    let mut cur = v;
    for _ in 0..j {
        match list::split(cur) {
            Some((_, t)) => cur = t,
            None => return None,
        }
    }
    Some(cur)
}

/// `l op r` over operands with no register form, borrowed: unchecked, a
/// failure is bottom as the node-walk's; checked, the catchable
/// `ArithError` value.
fn value_arith(op: ArithOp, checked: bool, l: &TagValue, r: &TagValue) -> TagValue {
    match l.with_value(|a| r.with_value(|b| arith(op, checked, a.clone(), b.clone()))) {
        Value::Error(_) if !checked => TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
        v => TagValue::fired(v),
    }
}

jit_helpers! { registry = value_helpers;

/// Unwrap an owned `Value::Array` into owned ValArray bits. Its caller
/// sends a tainted operand elsewhere; the shape check only stops a
/// codegen bug from reading another value's payload as array bits.
fn graphix_value_into_array(v: TagValue) -> u64 {
    match v.value() {
        Value::Array(a) => va_bits(a),
        v => panic!("graphix_value_into_array: expected Value::Array, got {v:?}"),
    }
}

/// Borrowed form of [`graphix_value_into_array`]: owned bits out, the
/// caller keeps its Value.
fn graphix_value_into_array_borrowed(v: ManuallyDrop<TagValue>) -> u64 {
    let bits = v.with_value(|v| match v {
        Value::Array(a) => va_bits(a.clone()),
        v => {
            panic!("graphix_value_into_array_borrowed: expected Value::Array, got {v:?}")
        }
    });
    bits
}

/// Drop an owned Value. Disc 0 is never a real Value, so the pending
/// sentinel is rejected before an invalid `Value` materializes.
fn graphix_value_drop(tv: TagValue) {
    // out of drop's reach while the assert may unwind: a sentinel is no Value
    let tv = ManuallyDrop::new(tv);
    assert!(
        !tv.is_sentinel(),
        "graphix_value_drop: zero discriminant — JIT codegen bug \
         (a pending sentinel leaked into a drop)"
    );
    drop(ManuallyDrop::into_inner(tv))
}

/// Clone a borrowed Value (tag preserved); the caller keeps its ref.
fn graphix_value_clone(tv: ManuallyDrop<TagValue>) -> TagValue {
    TagValue::clone(&tv)
}

/// Clone a `Value` from a kernel's value-constants table slot.
///
/// # Safety
/// `ptr` must point to a live `Value` that outlives the JIT'd code.
fn graphix_value_clone_from_static(ptr: *const Value) -> TagValue {
    TagValue::fired(unsafe { (*ptr).clone() })
}

/// The abstract constructor `T(v)`: box `tv` under the abstract type
/// `typ` (a `Type::Abstract` constant of the kernel's record). Consumes
/// `tv`. A bottom `tv` stays the placeholder: a value of `T` built from
/// one would reach `T`'s core-trait impls, which run only on values that
/// exist.
fn graphix_abstract_wrap(
    typ: *const Type,
    name: *const arcstr::ArcStr,
    tv: TagValue,
) -> TagValue {
    if tv.is_bottom() {
        return tv;
    }
    let name = unsafe { (*name).clone() };
    let (id, params) = match unsafe { &*typ } {
        Type::Abstract { id, params } => (*id, params.clone()),
        t => panic!("graphix_abstract_wrap: {t} is not an abstract type — JIT codegen bug"),
    };
    TagValue::fired(crate::abstract_value::wrap(id, name, params, tv.value()))
}

fn graphix_abstract_get_arcstr(tv: ManuallyDrop<TagValue>) -> u64 {
    let r = tv.with_value(|v| slot_arcstr(payload_of(v)));
    arcstr_bits(r)
}

fn graphix_abstract_get_array(tv: ManuallyDrop<TagValue>) -> u64 {
    let r = tv.with_value(|v| va_bits(slot_array(payload_of(v)).clone()));
    r
}

fn graphix_abstract_get_value(tv: ManuallyDrop<TagValue>) -> TagValue {
    let r = tv.with_value(|v| {
        TagValue::fired(payload_of(v).cloned().unwrap_or(Value::Null))
    });
    r
}

fn graphix_value_neg(v: ManuallyDrop<TagValue>) -> TagValue {
    v.with_value(neg).map(TagValue::fired).unwrap_or_else(|| TagValue::bottom_null(true).clone())
}

/// # Safety
/// `typ` must point to a type that outlives the kernel invocation.
fn graphix_inferred_type_matches(v: ManuallyDrop<TagValue>, typ: *const Type) -> u8 {
    let typ = unsafe { &*typ };
    guard_panic(0, || v.with_value(|v| {
        typ.is_a_with(unsafe { kernel_env() }, IsAFlags::MatchAbstract.into(), v) as u8
    }))
}

fn graphix_value_add(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Add, false, &l, &r)
}

fn graphix_value_sub(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Sub, false, &l, &r)
}

fn graphix_value_mul(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Mul, false, &l, &r)
}

fn graphix_value_div(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Div, false, &l, &r)
}

fn graphix_value_rem(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Mod, false, &l, &r)
}

fn graphix_value_checked_add(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Add, true, &l, &r)
}

fn graphix_value_checked_sub(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Sub, true, &l, &r)
}

fn graphix_value_checked_mul(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Mul, true, &l, &r)
}

fn graphix_value_checked_div(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Div, true, &l, &r)
}

fn graphix_value_checked_rem(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> TagValue {
    value_arith(ArithOp::Mod, true, &l, &r)
}

/// `l` against `r` in the total order of values: -1, 0 or 1. Borrows
/// both operands.
fn graphix_value_cmp(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> i8 {
    if l.is_bottom() || r.is_bottom() {
        return 0;
    }
    guard_panic(0, || l.with_value(|a| r.with_value(|b| a.cmp(b) as i8)))
}

/// Value equality; borrows both operands.
fn graphix_value_eq(l: ManuallyDrop<TagValue>, r: ManuallyDrop<TagValue>) -> u8 {
    if l.is_bottom() || r.is_bottom() {
        return 0;
    }
    guard_panic(0, || l.with_value(|a| r.with_value(|b| (a == b) as u8)))
}

/// `bytes[i]`: the `u8` or the index error, via the node-walk's own
/// [`value_index`]. Borrows `v`.
fn graphix_bytes_index(v: ManuallyDrop<TagValue>, i: i64) -> TagValue {
    TagValue::fired(v.with_value(|v| value_index(v, i)))
}

/// `m{key}`: the value or the not-found error, via the shared
/// [`map_get`]. Borrows both operands.
fn graphix_map_ref(map: ManuallyDrop<TagValue>, key: ManuallyDrop<TagValue>) -> TagValue {
    guard_panic(TagValue::phantom(), || {
        TagValue::fired(map.with_value(|m| key.with_value(|k| map_get(m, k))))
    })
}

/// `a[i..j]` over an array or bytes: `flags` bit0 = `start` present,
/// bit1 = `end` present. Borrows `src`.
fn graphix_array_slice(src: ManuallyDrop<TagValue>, start: i64, end: i64, flags: i64) -> TagValue {
    let s = if flags & 1 != 0 { Some(start) } else { None };
    let e = if flags & 2 != 0 { Some(end) } else { None };
    TagValue::fired(src.with_value(|v| array_slice(v, s, e)))
}

/// Whether `v` is the string `*lit`. Borrows `v`.
fn graphix_value_is_str(v: ManuallyDrop<TagValue>, lit: *const arcstr::ArcStr) -> u8 {
    let lit = unsafe { &*lit };
    v.with_value(|v| matches!(v, Value::String(s) if s == lit)) as u8
}

/// Borrowed test of a variant's tag AND arity against `expected`
/// (`rep::is_variant`): the tag alone does not discriminate
/// `` [`A, `A(i64)] ``.
fn graphix_variant_tag_eq(
    v: ManuallyDrop<TagValue>,
    expected: *const arcstr::ArcStr,
    arity: usize,
) -> u8 {
    let exp = unsafe { &*expected };
    v.with_value(|v| rep::is_variant(v, exp, arity) as u8)
}

/// Owned clone of a variant payload slot as a Value; a shape mismatch
/// yields the drop-safe `Value::Null`.
fn graphix_variant_payload_value(v: ManuallyDrop<TagValue>, payload_idx: usize) -> TagValue {
    let r = v.with_value(|v| rep::payload(v, payload_idx).cloned().unwrap_or(Value::Null));
    TagValue::fired(r)
}

/// Owned `ArcStr` clone of a string variant payload slot; mismatch
/// yields the static empty string.
fn graphix_variant_payload_string(v: ManuallyDrop<TagValue>, payload_idx: usize) -> u64 {
    let r = v.with_value(|v| match rep::payload(v, payload_idx) {
        Some(Value::String(s)) => s.clone(),
        _ => arcstr::ArcStr::new(),
    });
    arcstr_bits(r)
}

/// A variant payload slot's words, borrowed. A shape mismatch yields
/// `Value::Null`.
///
/// # Safety
///
/// The result aliases the parent's slot: it is valid while the parent
/// variant is alive and is never dropped or passed to a consuming
/// helper.
fn graphix_variant_payload_borrowed(v: ManuallyDrop<TagValue>, payload_idx: usize) -> TagValue {
    let r = v.with_value(|v| match rep::payload(v, payload_idx) {
        // SAFETY: a bitwise alias of the slot; the kernel never drops
        // it, so the parent keeps the only ownership.
        Some(p) => TagValue::fired(unsafe { std::ptr::read(p) }),
        None => TagValue::fired(Value::Null),
    });
    r
}

/// Owned `ArcStr` clone of a nullable's string payload; null yields the
/// static empty string.
fn graphix_nullable_string(v: ManuallyDrop<TagValue>) -> u64 {
    let r = v.with_value(|v| match v {
        Value::String(s) => s.clone(),
        _ => arcstr::ArcStr::new(),
    });
    arcstr_bits(r)
}

/// Owned `ValArray` clone of a nullable's composite payload; null
/// yields the empty array.
fn graphix_nullable_array(v: ManuallyDrop<TagValue>) -> u64 {
    let r = v.with_value(|v| match v {
        Value::Array(a) => a.clone(),
        _ => EMPTY_ARR.clone(),
    });
    va_bits(r)
}

/// List-pattern structure test: `k` cells exist; `exact` also requires
/// nil after them. A non-list fails the walk.
fn graphix_list_match(v: ManuallyDrop<TagValue>, k: usize, exact: u8) -> u8 {
    let r = v.with_value(|v| match list_walk(v, k) {
        None => 0,
        Some(cur) => {
            if exact != 0 {
                list::is_nil(cur) as u8
            } else {
                1
            }
        }
    });
    r
}

/// Owned clone of the j-th head of a list as a Value; `Null` on a short chain.
fn graphix_list_get_value(v: ManuallyDrop<TagValue>, j: usize) -> TagValue {
    let r = v.with_value(|v| match list_walk(v, j).and_then(|c| list::split(c)) {
        Some((h, _)) => h.clone(),
        None => Value::Null,
    });
    TagValue::fired(r)
}

/// The j-th head of a list's words, borrowed; `Value::Null` on a short
/// chain.
///
/// # Safety
///
/// The result aliases the list's cell: it is valid while the list is
/// alive and is never dropped or passed to a consuming helper.
fn graphix_list_head_borrowed(v: ManuallyDrop<TagValue>, j: usize) -> TagValue {
    let r = v.with_value(|v| match list_walk(v, j).and_then(|c| list::split(c)) {
        // SAFETY: a bitwise alias of the cell's head; the kernel never
        // drops it, so the list keeps the only ownership.
        Some((h, _)) => TagValue::fired(unsafe { std::ptr::read(h) }),
        None => TagValue::fired(Value::Null),
    });
    r
}

/// Owned `ValArray` bits of the j-th head; the empty array on mismatch.
fn graphix_list_get_array(v: ManuallyDrop<TagValue>, j: usize) -> u64 {
    let r = v.with_value(|v| match list_walk(v, j).and_then(|c| list::split(c)) {
        Some((Value::Array(a), _)) => a.clone(),
        _ => EMPTY_ARR.clone(),
    });
    va_bits(r)
}

/// Owned `ArcStr` of the j-th head; the empty string on mismatch.
fn graphix_list_get_string(v: ManuallyDrop<TagValue>, j: usize) -> u64 {
    let r = v.with_value(|v| {
        match list_walk(v, j).and_then(|c| list::split(c)) {
            Some((Value::String(s), _)) => s.clone(),
            _ => arcstr::ArcStr::new(),
        }
    });
    arcstr_bits(r)
}

/// Owned clone of the k-th tail (the `[<h, rest..>]` rest bind), O(1)
/// shared structure; `Null` on a short chain.
fn graphix_list_tail(v: ManuallyDrop<TagValue>, k: usize) -> TagValue {
    let r = v.with_value(|v| match list_walk(v, k) {
        Some(cur) => cur.clone(),
        None => Value::Null,
    });
    TagValue::fired(r)
}

/// Owned `ValArray` bits of a composite variant payload slot; the
/// empty array on mismatch.
fn graphix_variant_payload_array(v: ManuallyDrop<TagValue>, payload_idx: usize) -> u64 {
    let r = v.with_value(|v| match rep::payload(v, payload_idx) {
        Some(Value::Array(inner)) => inner.clone(),
        _ => EMPTY_ARR.clone(),
    });
    va_bits(r)
}

}

// A String SSA value is the raw `ArcStr` pointer; every owned one is
// consumed by a helper or returned across the kernel boundary.

/// Render a primitive as `Value::<T>(v).to_string()` does.
fn push_display<T: std::fmt::Display>(buf: *mut StringBuf, v: T) {
    use std::fmt::Write;
    let _ = write!(unsafe { &mut **buf }, "{v}");
}

jit_helpers! { registry = string_helpers;

/// Owned clone of a kernel strings-table slot.
fn graphix_arcstr_clone_from_static(p: *const arcstr::ArcStr) -> u64 {
    arcstr_bits(unsafe { (*p).clone() })
}

/// Drop an owned ArcStr. Takes raw bits so the zero pending sentinel is
/// rejected before an invalid `ArcStr` (NonNull) materializes.
fn graphix_arcstr_drop(s: u64) {
    drop(unsafe { arcstr_from_bits(s, "graphix_arcstr_drop") })
}

/// Clone a borrowed ArcStr; the caller keeps its ref.
fn graphix_arcstr_clone(s: u64) -> u64 {
    let s = ManuallyDrop::new(unsafe { arcstr_from_bits(s, "graphix_arcstr_clone") });
    arcstr_bits((*s).clone())
}

/// The empty-`ArcStr` placeholder for a tainted String position, as bits.
fn graphix_arcstr_empty() -> u64 {
    arcstr_bits(arcstr::ArcStr::new())
}

/// The empty-`ValArray` placeholder for a tainted composite position,
/// as owned bits.
fn graphix_valarray_empty() -> u64 {
    va_bits(EMPTY_ARR.clone())
}

/// An empty `ValArray` to read borrowed, as bits: one reference held for
/// the process, so a read that never drops it is sound.
fn graphix_valarray_empty_borrowed() -> u64 {
    static HELD: std::sync::LazyLock<u64> =
        std::sync::LazyLock::new(|| va_bits(EMPTY_ARR.clone()));
    *HELD
}

/// Whether `v` is a `*typ`, as a select arm's written type test asks
/// (`Type::is_a`), under the invoking kernel's env loan. Borrows `v`.
fn graphix_value_is_a(typ: *const Type, v: ManuallyDrop<TagValue>) -> u8 {
    let (typ, env) = unsafe { (&*typ, kernel_env()) };
    v.with_value(|v| typ.is_a(env, v)) as u8
}

/// Start an owned string buffer; pair with `graphix_string_buf_finalize`
/// or `graphix_string_buf_drop`.
fn graphix_string_buf_new() -> *mut StringBuf {
    STRING_SHELLS.with(|s| s.take(LPooled::take()))
}

/// Drop a string buf without finalizing.
fn graphix_string_buf_drop(buf: *mut StringBuf) {
    assert!(!buf.is_null(), "graphix_string_buf_drop: null buf — JIT codegen bug");
    STRING_SHELLS.with(|s| unsafe { s.give(buf) })
}

/// Finalize a string buf into an owned ArcStr, consuming the buf.
fn graphix_string_buf_finalize(buf: *mut StringBuf) -> u64 {
    let s = arcstr::ArcStr::from(unsafe { (*buf).as_str() });
    STRING_SHELLS.with(|st| unsafe { st.give(buf) });
    arcstr_bits(s)
}

/// Append an ArcStr's contents to the buf, consuming the ArcStr.
fn graphix_string_buf_push_arcstr(buf: *mut StringBuf, s: u64) {
    let s = unsafe { arcstr_from_bits(s, "graphix_string_buf_push_arcstr") };
    unsafe { &mut *buf }.push_str(&s);
}

/// Append `tv` as string interpolation renders a part of type `typ`
/// (`StringInterpolate::update`), under the invoking kernel's env loan;
/// a bottom part renders nothing, its taint the result's. Consumes `tv`.
fn graphix_string_buf_push_value(buf: *mut StringBuf, typ: *const Type, tv: TagValue) {
    use std::fmt::Write;
    if tv.is_bottom() {
        return;
    }
    let (buf, typ, env) = unsafe { (&mut **buf, &*typ, kernel_env()) };
    guard_panic((), || {
        tv.with_value(|v| match v {
            Value::String(s) => buf.push_str(s),
            v => write!(buf, "{}", crate::typ::TVal { env, typ, v }).unwrap(),
        })
    })
}

}

// The scaffold loops iterate a ValArray; List and Map sources flatten on
// entry and rebuild on exit through the same `node::collection`
// functions the interpreted MapQ/FoldQ use.

jit_helpers! { registry = collection_helpers;

/// Flatten a List value into owned ValArray bits, consuming it. A
/// non-list (the tainted placeholder) yields the empty array; the
/// source disc's taint rides the loop's SlotFlags.
fn graphix_list_to_valarray(tv: TagValue) -> u64 {
    let v = tv.value();
    let arr = list::to_array(&v).unwrap_or_else(|| EMPTY_ARR.clone());
    va_bits(arr)
}

/// Finalize a value buf into the List of its elements, consuming the buf.
fn graphix_list_finalize(buf: *mut LPooled<Vec<Value>>) -> TagValue {
    let list = list::from_iter(unsafe { (*buf).drain(..) });
    unsafe { buf_give(buf) };
    TagValue::fired(list)
}

/// Finalize a value buf of keys and values, alternating, into the Map of
/// its entries in written order, a later key replacing an earlier one as
/// the node-walk's literal does; consumes the buf. Key order is a core
/// trait's under the kernel's loaned hooks.
fn graphix_map_finalize(buf: *mut LPooled<Vec<Value>>) -> TagValue {
    let map = guard_panic(TagValue::phantom(), || {
        let mut m = netidx_value::Map::new();
        let mut entries = unsafe { (*buf).drain(..) };
        while let (Some(k), Some(v)) = (entries.next(), entries.next()) {
            m.insert_cow(k, v);
        }
        TagValue::fired(Value::Map(m))
    });
    unsafe { buf_give(buf) };
    map
}

/// Consume finalized ValArray bits and build the List value.
fn graphix_valarray_into_list(bits: u64) -> TagValue {
    let arr = unsafe { va_owned(bits) };
    TagValue::fired(list::from_iter(arr.iter().cloned()))
}

/// Flatten a Map value into owned ValArray bits of `[k, v]` pairs in
/// key order, consuming it; a non-map yields the empty array.
fn graphix_cmap_to_pairs(tv: TagValue) -> u64 {
    let v = tv.value();
    let arr = match &v {
        Value::Map(m) => ValArray::from_iter(
            m.into_iter().map(|(k, v)| crate::node::collection::make_pair(k, v)),
        ),
        _ => EMPTY_ARR.clone(),
    };
    va_bits(arr)
}

/// Consume finalized ValArray bits of `[k, v]` pairs and build the
/// `Value::Map` ([`crate::node::collection::pairs_to_map`]).
fn graphix_valarray_into_cmap(bits: u64) -> TagValue {
    let arr = unsafe { va_owned(bits) };
    guard_panic(TagValue::phantom(), || {
        TagValue::fired(crate::node::collection::pairs_to_map(arr.iter()))
    })
}

}

/// The payload `.0` reads: an abstract value's, or an error's.
fn payload_of(v: &Value) -> Option<&Value> {
    match v {
        Value::Error(e) => Some(e),
        v => crate::abstract_value::payload(v),
    }
}

/// Borrowed read of an abstract value's payload (`.0`); a non-abstract
/// value reads as the shape's placeholder.
fn abstract_payload_read<T: Default>(tv: &TagValue, f: fn(&Value) -> T) -> T {
    tv.with_value(|v| payload_of(v).map(f).unwrap_or_default())
}

macro_rules! slot_readers {
    ($($fn:ident, $ty:ty, [$($variant:ident)|+];)+) => {
        $(fn $fn(v: &Value) -> $ty {
            match v {
                $(Value::$variant(x) => *x as $ty,)+
                _ => Default::default(),
            }
        })+
    };
}

// Total scalar slot readers: the payload if the slot carries the
// family (fixed-width and varint alike), else 0.
slot_readers! {
    read_slot_i64, i64, [I64 | Z64];
    read_slot_u64, u64, [U64 | V64];
    read_slot_i32, i32, [I32 | Z32];
    read_slot_u32, u32, [U32 | V32];
    read_slot_i16, i16, [I16];
    read_slot_u16, u16, [U16];
    read_slot_i8, i8, [I8];
    read_slot_u8, u8, [U8];
    read_slot_f32, f32, [F32];
    read_slot_f64, f64, [F64];
}

fn read_slot_bool(v: &Value) -> u8 {
    match v {
        Value::Bool(b) => *b as u8,
        _ => 0,
    }
}

/// The placeholder a mismatched or out-of-bounds composite read returns;
/// static so borrowed pointers into it stay valid.
pub(crate) static EMPTY_ARR: std::sync::LazyLock<ValArray> =
    std::sync::LazyLock::new(|| ValArray::from_iter_exact(std::iter::empty()));

/// Free a slot-state chain: `word` is 0 or a `Box<Vec<u64>>`. With
/// `own_levels > 0` each entry is a chain one level down; at 0 the Vec
/// is plain words, or call-site blocks owning chains when `leaf` is given.
///
/// SAFETY: `word` is 0 or the sole owner of a chain the kernel's state
/// words describe; it is not read again.
pub unsafe fn free_slot_chain(word: u64, own_levels: u64, leaf: Option<&SiteLeaf>) {
    if word == 0 {
        return;
    }
    let v = unsafe { Box::from_raw(word as *mut Vec<u64>) };
    if own_levels > 0 {
        for e in v.iter() {
            unsafe { free_slot_chain(*e, own_levels - 1, leaf) };
        }
    } else if let Some(l) = leaf {
        unsafe { free_blocks(&v, l) };
    }
}

/// Free the per-activation block tree rooted at `root` (one
/// `Box<Vec<u64>>` per activation) whose blocks have `layout`: each
/// block's chains, the trees nested in it and its child activations.
/// Iterative: the tree is as deep as the recursion was. Returns the
/// blocks freed.
///
/// SAFETY: `root` is 0 or the sole owner of a tree laid out by `layout`;
/// it is not read again.
pub unsafe fn free_self_block_tree(root: u64, layout: &ActivationLayout) -> u64 {
    let mut freed = 0u64;
    let mut work: LPooled<Vec<(u64, &ActivationLayout)>> = LPooled::take();
    work.push((root, layout));
    while let Some((p, l)) = work.pop() {
        if p == 0 {
            continue;
        }
        let v = unsafe { Box::from_raw(p as *mut Vec<u64>) };
        freed += 1;
        crate::stack::record_self_blocks(-1);
        let word = |rel: u32| v.get(rel as usize).copied().unwrap_or(0);
        for a in l.anchors.iter() {
            unsafe {
                free_slot_chain(word(a.rel), a.own_levels as u64, a.leaf.as_deref())
            };
        }
        work.extend(l.nested.iter().map(|n| (word(n.rel), &*n.layout)));
        work.extend(l.slots.iter().map(|s| (word(*s), l)));
    }
    freed
}

thread_local! {
    /// The reach generation of the running kernel invocation; every
    /// activation block reached is stamped with it and the reclaim
    /// frees the rest. Saved/restored around every kernel invocation.
    pub(crate) static SELF_BLOCK_GEN: Cell<u64> = const { Cell::new(0) };
    /// Activation reaches this invocation; the reclaim runs only when
    /// it is below the live tree size. Saved/restored like
    /// [`SELF_BLOCK_GEN`].
    pub(crate) static SELF_BLOCK_REACHED: Cell<u64> = const { Cell::new(0) };
    /// Activation blocks this invocation allocated. Saved/restored like
    /// [`SELF_BLOCK_GEN`].
    pub(crate) static SELF_BLOCK_MADE: Cell<u64> = const { Cell::new(0) };
}

/// Free the subtrees of the per-activation block tree at `root` not
/// stamped with `generation`, nulling each freed subtree's root word;
/// the trees nested in a kept block are reclaimed the same way.
/// `root` is the address of the tree's root word. Returns the blocks
/// freed.
///
/// SAFETY: `*root` is 0 or the owner of a tree laid out by `layout`.
pub unsafe fn reclaim_self_block_tree(
    root: *mut u64,
    layout: &ActivationLayout,
    generation: u64,
) -> u64 {
    let mut freed = 0u64;
    let mut work: LPooled<Vec<(*mut u64, &ActivationLayout)>> = LPooled::take();
    work.push((root, layout));
    while let Some((wp, l)) = work.pop() {
        let p = unsafe { *wp };
        if p == 0 {
            continue;
        }
        let v: &mut Vec<u64> = unsafe { &mut *(p as *mut Vec<u64>) };
        // A block with no stamp word counts as reached: never free what
        // is not proven shed.
        let stamp = v.get(l.words as usize).copied().unwrap_or(generation);
        if stamp != generation {
            unsafe { *wp = 0 };
            freed += unsafe { free_self_block_tree(p, l) };
        } else {
            let len = v.len();
            let base = v.as_mut_ptr();
            let at = |rel: u32| (rel as usize) < len;
            for n in l.nested.iter().filter(|n| at(n.rel)) {
                work.push((unsafe { base.add(n.rel as usize) }, &*n.layout));
            }
            for s in l.slots.iter().filter(|s| at(**s)) {
                work.push((unsafe { base.add(*s as usize) }, l));
            }
            for a in l.anchors.iter().filter(|a| at(a.rel)) {
                freed += unsafe {
                    reclaim_slot_chain(
                        base.add(a.rel as usize),
                        a.own_levels as u64,
                        a.leaf.as_deref(),
                        generation,
                    )
                };
            }
        }
    }
    freed
}

/// [`reclaim_self_block_tree`] for every tree rooted in the call-site
/// blocks at the leaf of the slot-state chain whose word is at `word`,
/// and in the chains those blocks root. Returns the blocks freed.
///
/// SAFETY: `word` addresses a chain word laid out as `own_levels` and
/// `leaf` describe.
pub unsafe fn reclaim_slot_chain(
    word: *mut u64,
    own_levels: u64,
    leaf: Option<&SiteLeaf>,
    generation: u64,
) -> u64 {
    let p = unsafe { *word };
    let Some(leaf) = leaf.filter(|_| p != 0) else { return 0 };
    let v: &mut Vec<u64> = unsafe { &mut *(p as *mut Vec<u64>) };
    let mut freed = 0u64;
    if own_levels > 0 {
        for e in v.iter_mut() {
            freed +=
                unsafe { reclaim_slot_chain(e, own_levels - 1, Some(leaf), generation) };
        }
        return freed;
    }
    for block in v.chunks_exact_mut(leaf.stride as usize) {
        for b in leaf.self_blocks.iter() {
            freed += unsafe {
                reclaim_self_block_tree(&mut block[b.rel as usize], &b.layout, generation)
            };
        }
        for a in leaf.anchors.iter() {
            freed += unsafe {
                reclaim_slot_chain(
                    &mut block[a.rel as usize],
                    a.own_levels as u64,
                    a.leaf.as_deref(),
                    generation,
                )
            };
        }
    }
    freed
}

/// Free what a run of call-site blocks owns: the chains its anchors
/// root and the activation trees its self blocks root.
///
/// SAFETY: `words` are blocks laid out by `leaf`, which own what it names.
unsafe fn free_blocks(words: &[u64], leaf: &SiteLeaf) {
    for block in words.chunks_exact(leaf.stride as usize) {
        for a in leaf.anchors.iter() {
            unsafe {
                free_slot_chain(
                    block[a.rel as usize],
                    a.own_levels as u64,
                    a.leaf.as_deref(),
                )
            };
        }
        for b in leaf.self_blocks.iter() {
            unsafe { free_self_block_tree(block[b.rel as usize], &b.layout) };
        }
    }
}

/// The value of struct field `sorted_idx`; `None` past the end, where no
/// field is, or in a tainted position's empty placeholder.
fn struct_field(p: &ValArray, sorted_idx: usize) -> Option<&Value> {
    rep::field_at(p.get(rep::STRUCT_BASE..)?, sorted_idx).map(|(_, v)| v)
}

/// The returned words borrow the slot; no consumer may drop them or outlive it.
unsafe fn slot_value_borrowed(v: Option<&Value>) -> TagValue {
    TagValue::fired(match v {
        Some(v) => unsafe { std::ptr::read(v) },
        None => Value::Null,
    })
}

/// A slot as an array; the static empty array on mismatch.
fn slot_array(v: Option<&Value>) -> &ValArray {
    match v {
        Some(Value::Array(a)) => a,
        _ => &EMPTY_ARR,
    }
}

fn slot_arcstr(v: Option<&Value>) -> arcstr::ArcStr {
    match v {
        Some(Value::String(s)) => s.clone(),
        _ => arcstr::ArcStr::new(),
    }
}

// Element reads return an owned clone (except the `_borrowed` variants),
// so the source array keeps its own ref.

/// The per-prim helper families: one helper per scalar prim, the
/// family fixing the shape. Scalar reads are total (`slot_readers!`);
/// a bool travels as a `u8`.
macro_rules! prim_family {
    (valarray_get $registry:ident: $($name:ident, $t:ty, $read:ident;)*) => {
        jit_helpers! { registry = $registry;
            $(fn $name(bits: u64, idx: usize) -> $t {
                unsafe { va_ref(&bits) }.get(idx).map($read).unwrap_or_default()
            })*
        }
    };
    (struct_get $registry:ident: $($name:ident, $t:ty, $read:ident;)*) => {
        jit_helpers! { registry = $registry;
            $(fn $name(bits: u64, sorted_idx: usize) -> $t {
                struct_field(unsafe { va_ref(&bits) }, sorted_idx)
                    .map($read)
                    .unwrap_or_default()
            })*
        }
    };
    (variant_payload $registry:ident: $($name:ident, $t:ty, $read:ident;)*) => {
        jit_helpers! { registry = $registry;
            $(fn $name(v: ManuallyDrop<TagValue>, payload_idx: usize) -> $t {
                variant_payload_read(&v, payload_idx, $read)
            })*
        }
    };
    (abstract_get $registry:ident: $($name:ident, $t:ty, $read:ident;)*) => {
        jit_helpers! { registry = $registry;
            $(fn $name(tv: ManuallyDrop<TagValue>) -> $t {
                abstract_payload_read(&tv, $read)
            })*
        }
    };
    (value_buf_push $registry:ident: $($name:ident, $t:ty, $ctor:expr;)*) => {
        jit_helpers! { registry = $registry;
            $(fn $name(buf: *mut LPooled<Vec<Value>>, v: $t) {
                unsafe { (*buf).push($ctor(v)) }
            })*
        }
    };
    (string_buf_push $registry:ident: $($name:ident, $t:ty;)*) => {
        jit_helpers! { registry = $registry;
            $(fn $name(buf: *mut StringBuf, v: $t) {
                push_display(buf, v)
            })*
        }
    };
}

prim_family! { valarray_get valarray_get_helpers:
    graphix_valarray_get_i64, i64, read_slot_i64;
    graphix_valarray_get_u64, u64, read_slot_u64;
    graphix_valarray_get_i32, i32, read_slot_i32;
    graphix_valarray_get_u32, u32, read_slot_u32;
    graphix_valarray_get_i16, i16, read_slot_i16;
    graphix_valarray_get_u16, u16, read_slot_u16;
    graphix_valarray_get_i8, i8, read_slot_i8;
    graphix_valarray_get_u8, u8, read_slot_u8;
    graphix_valarray_get_f64, f64, read_slot_f64;
    graphix_valarray_get_f32, f32, read_slot_f32;
    graphix_valarray_get_bool, u8, read_slot_bool;
}

prim_family! { struct_get struct_get_helpers:
    graphix_struct_get_i64, i64, read_slot_i64;
    graphix_struct_get_u64, u64, read_slot_u64;
    graphix_struct_get_i32, i32, read_slot_i32;
    graphix_struct_get_u32, u32, read_slot_u32;
    graphix_struct_get_i16, i16, read_slot_i16;
    graphix_struct_get_u16, u16, read_slot_u16;
    graphix_struct_get_i8, i8, read_slot_i8;
    graphix_struct_get_u8, u8, read_slot_u8;
    graphix_struct_get_f64, f64, read_slot_f64;
    graphix_struct_get_f32, f32, read_slot_f32;
    graphix_struct_get_bool, u8, read_slot_bool;
}

prim_family! { variant_payload variant_payload_helpers:
    graphix_variant_payload_i64, i64, read_slot_i64;
    graphix_variant_payload_u64, u64, read_slot_u64;
    graphix_variant_payload_i32, i32, read_slot_i32;
    graphix_variant_payload_u32, u32, read_slot_u32;
    graphix_variant_payload_i16, i16, read_slot_i16;
    graphix_variant_payload_u16, u16, read_slot_u16;
    graphix_variant_payload_i8, i8, read_slot_i8;
    graphix_variant_payload_u8, u8, read_slot_u8;
    graphix_variant_payload_f64, f64, read_slot_f64;
    graphix_variant_payload_f32, f32, read_slot_f32;
    graphix_variant_payload_bool, u8, read_slot_bool;
}

prim_family! { abstract_get abstract_get_helpers:
    graphix_abstract_get_i64, i64, read_slot_i64;
    graphix_abstract_get_u64, u64, read_slot_u64;
    graphix_abstract_get_i32, i32, read_slot_i32;
    graphix_abstract_get_u32, u32, read_slot_u32;
    graphix_abstract_get_i16, i16, read_slot_i16;
    graphix_abstract_get_u16, u16, read_slot_u16;
    graphix_abstract_get_i8, i8, read_slot_i8;
    graphix_abstract_get_u8, u8, read_slot_u8;
    graphix_abstract_get_f64, f64, read_slot_f64;
    graphix_abstract_get_f32, f32, read_slot_f32;
    graphix_abstract_get_bool, u8, read_slot_bool;
}

prim_family! { value_buf_push value_buf_push_helpers:
    graphix_value_buf_push_i64, i64, Value::I64;
    graphix_value_buf_push_u64, u64, Value::U64;
    graphix_value_buf_push_i32, i32, Value::I32;
    graphix_value_buf_push_u32, u32, Value::U32;
    graphix_value_buf_push_i16, i16, Value::I16;
    graphix_value_buf_push_u16, u16, Value::U16;
    graphix_value_buf_push_i8, i8, Value::I8;
    graphix_value_buf_push_u8, u8, Value::U8;
    graphix_value_buf_push_f64, f64, Value::F64;
    graphix_value_buf_push_f32, f32, Value::F32;
    graphix_value_buf_push_bool, u8, |v: u8| Value::Bool(v != 0);
}

prim_family! { string_buf_push string_buf_push_helpers:
    graphix_string_buf_push_i64, i64;
    graphix_string_buf_push_u64, u64;
    graphix_string_buf_push_i32, i32;
    graphix_string_buf_push_u32, u32;
    graphix_string_buf_push_i16, i16;
    graphix_string_buf_push_u16, u16;
    graphix_string_buf_push_i8, i8;
    graphix_string_buf_push_u8, u8;
    graphix_string_buf_push_f64, f64;
    graphix_string_buf_push_f32, f32;
}

jit_helpers! { registry = string_bool_helpers;

fn graphix_string_buf_push_bool(buf: *mut StringBuf, v: u8) {
    push_display(buf, v != 0)
}

}

/// The table at `word` when an ensure of `len` words would leave it as
/// it is, read without a write: an outlined loop's chunks ensure a
/// level the kernel sized before forking them, all at once.
unsafe fn sized_table(
    word: *const u64,
    len: usize,
    source_present: u64,
) -> Option<*mut u64> {
    let table = unsafe { *word } as *const Vec<u64>;
    if table.is_null() {
        return None;
    }
    let v = unsafe { &*table };
    let unchanged = v.len() == len || (v.len() > len && source_present == 0);
    unchanged.then(|| v.as_ptr() as *mut u64)
}

jit_helpers! { registry = elem_helpers;

fn graphix_valarray_len(bits: u64) -> usize {
    unsafe { va_ref(&bits) }.len()
}

/// Owned subslice `[start, len - back)` of borrowed ValArray bits; a
/// range the array is too short for yields the empty array (a masked
/// bind reads an array its pattern did not match).
fn graphix_valarray_subslice(bits: u64, start: usize, back: usize) -> u64 {
    let a = unsafe { va_ref(&bits) };
    let r = match a.len().checked_sub(back) {
        Some(end) if end >= start => a.subslice(start..end).unwrap_or_else(|_| EMPTY_ARR.clone()),
        _ => EMPTY_ARR.clone(),
    };
    va_bits(r)
}

/// Source-level `arr[idx]` via the shared [`array_index`]: the element
/// or the index error; negative `idx` counts from the end.
fn graphix_valarray_index(bits: u64, idx: i64) -> TagValue {
    TagValue::fired(array_index(unsafe { va_ref(&bits) }, idx))
}

/// Resize a scaffold loop's per-slot state table (a boxed `Vec<u64>`
/// owned by `*word`, one word per slot, 0 = no prior observation) to
/// `len` with prefix retention; truncation frees the dropped slots'
/// subtrees (`own_levels`, `leaf` as in [`free_slot_chain`]). With a
/// tainted source (`source_present == 0`) the table only grows,
/// zero-filled, so in-loop accesses up to `len` stay in bounds.
fn graphix_slot_state_table(
    word: *mut u64,
    len: u64,
    source_present: u64,
    own_levels: u64,
    leaf: *const SiteLeaf,
) -> *mut u64 {
    let len = len as usize;
    if let Some(table) = unsafe { sized_table(word, len, source_present) } {
        return table;
    }
    let leaf = unsafe { leaf.as_ref() };
    let word = unsafe { &mut *word };
    if *word == 0 {
        *word = Box::into_raw(Box::new(Vec::<u64>::new())) as u64;
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    if source_present != 0 && len < v.len() {
        if own_levels > 0 {
            for e in v[len..].iter() {
                unsafe { free_slot_chain(*e, own_levels - 1, leaf) };
            }
        }
        v.truncate(len)
    } else if len > v.len() {
        v.resize(len, 0)
    }
    v.as_mut_ptr()
}

/// [`graphix_slot_state_table`] for the level a forked chunk shares with
/// the others: sized before the fork, so read-only here.
fn graphix_slot_state_table_shared(
    word: *mut u64,
    len: u64,
    source_present: u64,
) -> *mut u64 {
    unsafe { sized_table(word, len as usize, source_present) }.expect(
        "graphix_slot_state_table_shared: a shared chain level not sized before \
         the fork — JIT codegen bug",
    )
}

/// The per-activation block for a self-call, allocated on first use
/// and stamped with the reach generation. `word` is the root word in
/// the caller's block; `desc` is the callee's
/// `KernelSig::site_block_words`, read at run time because a self-call's
/// block size is unknown while its body is still emitting.
fn graphix_site_child_block(
    word: *mut u64,
    desc: *const AtomicU64,
) -> ChildBlock {
    let words = unsafe { (*desc).load(Relaxed) } as usize;
    if words == 0 {
        return ChildBlock { block: std::ptr::null_mut(), fresh: 0 };
    }
    let word = unsafe { &mut *word };
    let fresh = *word == 0;
    if fresh {
        // Index `words`, past the emitted layout, holds the generation stamp.
        *word = Box::into_raw(Box::new(vec![0u64; words + 1])) as u64;
        crate::stack::record_self_blocks(1);
        SELF_BLOCK_MADE.set(SELF_BLOCK_MADE.get() + 1);
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    v[words] = SELF_BLOCK_GEN.get();
    SELF_BLOCK_REACHED.set(SELF_BLOCK_REACHED.get() + 1);
    ChildBlock { block: v.as_mut_ptr(), fresh: fresh as u64 }
}

/// [`graphix_slot_state_table`] for a chain leaf of per-slot call-site
/// blocks (`stride` words each); truncation frees the dropped blocks'
/// anchor-owned chains.
fn graphix_slot_state_blocks(
    word: *mut u64,
    slots: u64,
    source_present: u64,
    leaf: *const SiteLeaf,
) -> *mut u64 {
    let leaf_ref = unsafe { &*leaf };
    let len = (slots as usize) * (leaf_ref.stride as usize);
    if let Some(table) = unsafe { sized_table(word, len, source_present) } {
        return table;
    }
    let word = unsafe { &mut *word };
    if *word == 0 {
        *word = Box::into_raw(Box::new(Vec::<u64>::new())) as u64;
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    if source_present != 0 && len < v.len() {
        unsafe { free_blocks(&v[len..], leaf_ref) };
        v.truncate(len)
    } else if len > v.len() {
        v.resize(len, 0)
    }
    v.as_mut_ptr()
}

/// `arr[idx]` as owned ValArray bits.
fn graphix_valarray_get_array(bits: u64, idx: usize) -> u64 {
    va_bits(slot_array(unsafe { va_ref(&bits) }.get(idx)).clone())
}

/// `arr[idx]` as borrowed ValArray bits: valid only while the parent
/// array is alive, never passed to a consuming or dropping helper.
fn graphix_valarray_get_array_borrowed(bits: u64, idx: usize) -> u64 {
    va_borrowed_bits(slot_array(unsafe { va_ref(&bits) }.get(idx)))
}

/// A struct field as borrowed ValArray bits; same lifetime contract as
/// [`graphix_valarray_get_array_borrowed`].
fn graphix_struct_get_array_borrowed(bits: u64, sorted_idx: usize) -> u64 {
    va_borrowed_bits(slot_array(struct_field(unsafe { va_ref(&bits) }, sorted_idx)))
}

/// `arr[idx]` as an owned `ArcStr` (String elem).
fn graphix_valarray_get_arcstr(bits: u64, idx: usize) -> u64 {
    arcstr_bits(slot_arcstr(unsafe { va_ref(&bits) }.get(idx)))
}

/// The result borrows the array slot and must never be dropped.
fn graphix_valarray_get_value_borrowed(bits: u64, idx: usize) -> TagValue {
    unsafe { slot_value_borrowed(va_ref(&bits).get(idx)) }
}

/// The result borrows the struct field and must never be dropped.
fn graphix_struct_get_value_borrowed(bits: u64, idx: usize) -> TagValue {
    unsafe { slot_value_borrowed(struct_field(va_ref(&bits), idx)) }
}

/// `arr[idx]` as an owned `Value`.
fn graphix_valarray_get_value(bits: u64, idx: usize) -> TagValue {
    TagValue::fired(unsafe { va_ref(&bits) }.get(idx).cloned().unwrap_or(Value::Null))
}

/// A struct field as owned ValArray bits.
fn graphix_struct_get_array(bits: u64, sorted_idx: usize) -> u64 {
    va_bits(slot_array(struct_field(unsafe { va_ref(&bits) }, sorted_idx)).clone())
}

fn graphix_struct_get_arcstr(bits: u64, sorted_idx: usize) -> u64 {
    arcstr_bits(slot_arcstr(struct_field(unsafe { va_ref(&bits) }, sorted_idx)))
}

fn graphix_struct_get_value(bits: u64, sorted_idx: usize) -> TagValue {
    TagValue::fired(
        struct_field(unsafe { va_ref(&bits) }, sorted_idx).cloned().unwrap_or(Value::Null),
    )
}

}

/// What a `GXDBG_CALLRET` print shows.
#[cfg(debug_assertions)]
#[derive(Clone, Copy)]
#[repr(u64)]
pub(crate) enum CallRetTag {
    /// A cross-kernel call's result disc.
    CallResult = 1,
    /// A kernel's return disc before the tail folds.
    Return = 2,
    /// The tail-select scrutinee STALE accumulator at a return.
    TailScrutAcc = 3,
    /// The init flag at kernel entry.
    InitFlag = 4,
}

#[cfg(debug_assertions)]
impl CallRetTag {
    fn name(tag: u64) -> &'static str {
        match tag {
            1 => "call-result",
            2 => "return",
            3 => "tail-scrut-acc",
            4 => "init-flag",
            _ => "?",
        }
    }
}

#[cfg(debug_assertions)]
jit_helpers! { registry = debug_helpers;

/// Print a disc word from inside JIT'd code (`GXDBG_CALLRET`); `tag`
/// is a [`CallRetTag`].
fn graphix_dbg_disc(tag: u64, disc: u64) {
    eprintln!("CLIF-DISC {} disc={disc:x}", CallRetTag::name(tag));
}

/// Count a JIT wrapper entry against the running runtime; emitted at the
/// start of every wrapper.
fn graphix_record_jit_invocation() {
    crate::stack::record_invocation(true)
}

}

/// One `?` raise a kernel queued: the site's interned `QopSite` and the
/// error.
pub(crate) type QopRaise = (*const QopSite, Value);

/// An `Effect::Io` call a kernel queued: its site and its arguments.
pub(crate) type IoQueued = (*const crate::IoSite, SmallVec<[Value; 2]>);

/// What a kernel run leaves its wrapper to do, each in execution order:
/// deliver its `?` raises and perform its `Io` calls.
pub(crate) struct Deferred {
    pub(crate) raises: LPooled<Vec<QopRaise>>,
    pub(crate) io: LPooled<Vec<IoQueued>>,
}

impl Deferred {
    pub(crate) fn new() -> Self {
        Self { raises: LPooled::take(), io: LPooled::take() }
    }
}

thread_local! {
    /// Sticky abort flag: set on a whole-kernel abort path, reset by
    /// `FusedKernel::update` before each wrapper call and read after; set
    /// means the result is the abort sentinel.
    pub static KERNEL_ABORT: Cell<bool> = const { Cell::new(false) };

    /// A panic a fast fn or a value hook raised inside the running
    /// kernel, which `FusedKernel::update` resumes after the run.
    static KERNEL_PANIC: RefCell<Option<Box<dyn Any + Send>>> = const { RefCell::new(None) };

    /// The invoking kernel's type environment, loaned for one wrapper
    /// call ([`with_kernel_env`]); null when no kernel is in flight.
    pub static KERNEL_ENV: Cell<*const crate::env::Env> =
        const { Cell::new(std::ptr::null()) };

    /// The invocation's deferred work, drained after the wrapper returns
    /// ([`with_deferred`]); a kernel never delivers or performs mid-run.
    static DEFERRED: RefCell<Deferred> = RefCell::new(Deferred::new());
}

/// What a kernel run reports through this thread (its abort, panic and
/// activation reach and made counts), cleared for a run nested in an
/// enclosing one, whose own is set aside and put back when this drops, an
/// unwind included.
pub(crate) struct RunLocals {
    abort: bool,
    panic: Option<Box<dyn Any + Send>>,
    reached: u64,
    made: u64,
}

/// What a kernel run reported.
pub(crate) struct RunReport {
    pub(crate) aborted: bool,
    pub(crate) panic: Option<Box<dyn Any + Send>>,
    pub(crate) reached: u64,
    pub(crate) made: u64,
}

impl RunLocals {
    pub(crate) fn enter() -> Self {
        Self {
            abort: KERNEL_ABORT.with(|c| c.replace(false)),
            panic: take_kernel_panic(),
            reached: SELF_BLOCK_REACHED.with(|c| c.replace(0)),
            made: SELF_BLOCK_MADE.with(|c| c.replace(0)),
        }
    }

    /// What the run reported, taken before the enclosing run's comes back.
    pub(crate) fn report(&self) -> RunReport {
        RunReport {
            aborted: KERNEL_ABORT.with(|c| c.get()),
            panic: take_kernel_panic(),
            reached: SELF_BLOCK_REACHED.with(|c| c.get()),
            made: SELF_BLOCK_MADE.with(|c| c.get()),
        }
    }
}

impl Drop for RunLocals {
    fn drop(&mut self) {
        KERNEL_ABORT.with(|c| c.set(self.abort));
        KERNEL_PANIC.with(|q| *q.borrow_mut() = self.panic.take());
        SELF_BLOCK_REACHED.with(|c| c.set(self.reached));
        SELF_BLOCK_MADE.with(|c| c.set(self.made));
    }
}

pub(crate) fn take_kernel_panic() -> Option<Box<dyn Any + Send>> {
    KERNEL_PANIC.with(|p| p.borrow_mut().take())
}

/// Report `p` as the running kernel's panic; the run's first stands.
pub(crate) fn set_kernel_panic(p: Box<dyn Any + Send>) {
    KERNEL_PANIC.with(|q| {
        let mut q = q.borrow_mut();
        if q.is_none() {
            *q = Some(p)
        }
    })
}

/// Whether the running kernel must leave at its next poll: the runtime
/// asked it to, or it recorded a panic.
fn run_stops() -> bool {
    crate::stack::interrupted() || KERNEL_PANIC.with(|q| q.borrow().is_some())
}

/// Append what a chunk run deferred to this thread's queue.
pub(crate) fn queue_deferred(d: &mut Deferred) {
    DEFERRED.with(|q| {
        let mut q = q.borrow_mut();
        q.raises.extend(d.raises.drain(..));
        q.io.extend(d.io.drain(..))
    })
}

/// A chunk's value buf of capacity `cap`, taken on the thread that will
/// finalize or discard it, so a pool's take and give stay on one thread.
pub(crate) fn value_buf_new(cap: usize) -> u64 {
    buf_take(cap) as u64
}

/// Drop a chunk's value buf whose slots are not taken.
pub(crate) unsafe fn value_buf_discard(buf: u64) {
    unsafe { buf_give(buf as *mut ValueBuf) }
}

/// One chunk's value buf as owned `ValArray` bits.
pub(crate) unsafe fn value_buf_finalize(buf: u64) -> u64 {
    unsafe { graphix_valarray_finalize(buf as *mut ValueBuf) }
}

/// Concatenate chunks' value bufs, in order, into owned `ValArray`
/// bits, consuming the bufs.
pub(crate) unsafe fn value_bufs_finalize(bufs: impl Iterator<Item = u64>) -> u64 {
    let bufs: LPooled<Vec<*mut ValueBuf>> = bufs.map(|b| b as *mut ValueBuf).collect();
    let left = bufs.iter().map(|b| unsafe { (**b).len() }).sum();
    let values = bufs.iter().flat_map(|b| unsafe { (**b).drain(..) });
    let bits = va_bits(crate::valarray_of(left, values));
    for b in bufs.iter() {
        unsafe { buf_give(*b) }
    }
    bits
}

/// Count a fused kernel run against the running runtime once a kernel
/// commits to running.
#[cfg(debug_assertions)]
pub fn record_fusion_invocation() {
    crate::stack::record_invocation(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_arithmetic_borrows_operands() {
        let input = TagValue::fired("decimal:1.5".parse().unwrap());
        let result = value_arith(ArithOp::Add, false, &input, &input);
        assert_eq!(result.value_cloned(), "decimal:3.0".parse::<Value>().unwrap());
        assert_eq!(input.value_cloned(), "decimal:1.5".parse::<Value>().unwrap());
    }

    /// A builder dropped with a large capacity leaves none of it behind
    /// for the next builder.
    #[test]
    fn a_builder_keeps_no_outsized_capacity() {
        unsafe {
            let large = graphix_value_buf_new(1_000_000);
            graphix_value_buf_drop(large);
            let small = graphix_value_buf_new(1);
            assert!((*small).capacity() < 1_000_000);
            graphix_value_buf_drop(small);
            let large = graphix_string_buf_new();
            (*large).reserve(1_000_000);
            graphix_string_buf_drop(large);
            let small = graphix_string_buf_new();
            assert!((*small).capacity() < 1_000_000);
            graphix_string_buf_drop(small);
        }
    }

    /// `graphix_fastcall`'s tag rules: the arg masks decide the tag,
    /// a tainted arg bottoms without calling, `None` is a bottom.
    #[test]
    fn fastcall_tags_follow_the_arg_discs() {
        fn len(args: &[Value]) -> Option<Value> {
            match args {
                [Value::Array(a)] => Some(Value::I64(a.len() as i64)),
                _ => None,
            }
        }
        fn never(args: &[Value]) -> Option<Value> {
            panic!("a tainted arg must not reach the fn ({} args)", args.len())
        }
        fn none(_: &[Value]) -> Option<Value> {
            None
        }
        let arr = Value::Array(ValArray::from_iter_exact(
            [Value::I64(1), Value::I64(2), Value::I64(3)].into_iter(),
        ));
        let args = vec![arr];
        let (ap, n) = (args.as_ptr() as u64, args.len() as u64);
        let fp = len as *const () as u64;
        let fastcall = |f: u64, taint: u64, stale: u64| {
            let mut out = std::mem::MaybeUninit::<TagValue>::uninit();
            unsafe {
                graphix_fastcall(f, ap, n, taint, stale, out.as_mut_ptr());
                out.assume_init()
            }
        };
        let tv = fastcall(fp, 0, 0);
        assert_eq!(tv.tag(), Tag::FIRED);
        assert_eq!(tv.value_cloned(), Value::I64(3));
        let tv = fastcall(fp, 0, 0b1);
        assert_eq!(tv.tag(), Tag::STALE);
        assert_eq!(tv.value_cloned(), Value::I64(3));
        let never_p = never as *const () as u64;
        let tv = fastcall(never_p, 0b1, 0);
        assert_eq!(tv.tag(), Tag::FRESH_BOTTOM);
        let tv = fastcall(never_p, 0b1, 0b1);
        assert_eq!(tv.tag(), Tag::STALE_BOTTOM);
        let tv = fastcall(none as *const () as u64, 0, 0);
        assert_eq!(tv.tag(), Tag::FRESH_BOTTOM);
        assert_eq!(args.len(), 1);
    }
}

/// The invoking kernel's env loan ([`with_kernel_env`]).
///
/// SAFETY: the loan is scoped to the enclosing wrapper call, during which
/// `Env` is not touched mutably.
unsafe fn kernel_env<'a>() -> &'a crate::env::Env {
    let env = KERNEL_ENV.with(|c| c.get());
    if env.is_null() {
        panic!(
            "no kernel env loaned — FusedKernel::update must run the wrapper \
             under `with_kernel_env`"
        );
    }
    unsafe { &*env }
}

/// Whether a fused kernel is running on this thread: a fast fn's
/// arithmetic diagnostics are the node-walk's alone, as an operator's are.
pub fn kernel_running() -> bool {
    !KERNEL_ENV.with(|c| c.get()).is_null()
}

/// Loan `env` (`KERNEL_ENV`) to the JIT'd code `f` runs; nested
/// invocations stack.
pub(crate) fn with_kernel_env<T>(env: &crate::env::Env, f: impl FnOnce() -> T) -> T {
    let _env = crate::Restore::replace(&KERNEL_ENV, env as *const _);
    f()
}

/// Run `f` against a fresh deferred queue and return what it deferred,
/// in order; an enclosing invocation's queue is set aside and restored.
pub(crate) fn with_deferred<T>(f: impl FnOnce() -> T) -> (T, Deferred) {
    struct Outer(Option<Deferred>);
    impl Drop for Outer {
        fn drop(&mut self) {
            if let Some(outer) = self.0.take() {
                DEFERRED.with(|q| *q.borrow_mut() = outer)
            }
        }
    }
    let mut outer = Outer(Some(
        DEFERRED.with(|q| std::mem::replace(&mut *q.borrow_mut(), Deferred::new())),
    ));
    let r = f();
    let outer = outer.0.take().expect("set at the start");
    let mine = DEFERRED.with(|q| std::mem::replace(&mut *q.borrow_mut(), outer));
    (r, mine)
}
