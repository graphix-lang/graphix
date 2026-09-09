#![allow(improper_ctypes_definitions)]
//! `extern "C"` entry points the JIT calls for ops that cannot be
//! lowered in CLIF. Every helper is declared through [`jit_helpers!`],
//! which derives its CLIF wire signature from the Rust types; `emit.rs`
//! registers each symbol by pointer from [`all_helpers`].
//!
//! `Value` is `#[repr(u64)]`, two 8-byte words `(disc, payload)`, passed
//! in two integer registers; `improper_ctypes_definitions` is suppressed
//! because the outer layout is stable even where a payload is not `repr(C)`.
//!
//! Pointer arguments must stay valid for the call. Element/field reads
//! are total: an out-of-bounds index or an unexpected slot shape reads
//! as the return shape's placeholder (0 / "" / empty array / `Null`),
//! because tainted placeholders are `Value::Null` in composite slots.

use crate::{
    fusion::kernel_abi::SiteLeaf,
    node::{
        array::{array_index, array_slice_i64, bytes_index},
        map::map_get,
        op::wrap_arith_error,
    },
};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;

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
/// CALLER extend sub-register integers, so `emit.rs` applies the u/s
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
}

/// The wire slots a Rust type occupies as a helper parameter, one per
/// register. A type without an impl cannot appear in a helper signature.
pub(crate) trait HelperArg {
    const ABI: &'static [AbiTy];
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
    arcstr::ArcStr => &[AbiTy::I64];
    TagValue => &[AbiTy::I64, AbiTy::I64];
    DynCallRet => &[AbiTy::I64, AbiTy::I64];
}

impl<T> HelperArg for *mut T {
    const ABI: &'static [AbiTy] = &[AbiTy::I64];
}

impl<T> HelperArg for *const T {
    const ABI: &'static [AbiTy] = &[AbiTy::I64];
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

/// Declare JIT helpers: each entry is `safe`/`unsafe` followed by an
/// ordinary `fn`. Emits the `pub [unsafe] extern "C"` definitions plus
/// a `$registry()` function pushing one [`HelperSpec`] per helper.
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
        unsafe fn $name:ident($($p:ident: $t:ty),* $(,)?) $(-> $ret:ty)? $body:block
        $($rest:tt)*
    ) => {
        $(#[$attr])*
        pub unsafe extern "C" fn $name($($p: $t),*) $(-> $ret)? $body

        jit_helpers!(@parse [$registry]
            [$($acc)* { $name; ($($t),*); ($($ret)?) }] $($rest)*);
    };
    (@parse [$registry:ident] [$($acc:tt)*]
        $(#[$attr:meta])*
        safe fn $name:ident($($p:ident: $t:ty),* $(,)?) $(-> $ret:ty)? $body:block
        $($rest:tt)*
    ) => {
        $(#[$attr])*
        pub extern "C" fn $name($($p: $t),*) $(-> $ret)? $body

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
    control_helpers(&mut v);
    value_helpers(&mut v);
    string_helpers(&mut v);
    collection_helpers(&mut v);
    elem_helpers(&mut v);
    #[cfg(debug_assertions)]
    debug_helpers(&mut v);
    v
}

// A composite travels through the JIT as its `ValArray` bits in a `u64`:
// the zero pending sentinel would violate `ValArray`'s NonNull niche as a
// typed parameter. Owned bits drop exactly once; borrowed bits never do.

/// Borrow ValArray bits for the duration of a call.
#[inline]
fn va_ref(bits: &u64) -> &ValArray {
    assert!(*bits != 0, "graphix: zero ValArray bits — JIT codegen bug");
    unsafe { &*(bits as *const u64 as *const ValArray) }
}

/// Take ownership of ValArray bits.
#[inline]
fn va_owned(bits: u64) -> ValArray {
    assert!(bits != 0, "graphix: zero ValArray bits — JIT codegen bug");
    unsafe { std::mem::transmute::<u64, ValArray>(bits) }
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

jit_helpers! { registry = buf_helpers;

safe fn graphix_value_buf_new(cap: usize) -> *mut LPooled<Vec<Value>> {
    let mut buf: LPooled<Vec<Value>> = LPooled::take();
    buf.reserve(cap);
    Box::into_raw(Box::new(buf))
}

unsafe fn graphix_value_buf_push_i64(buf: *mut LPooled<Vec<Value>>, v: i64) {
    unsafe { (*buf).push(Value::I64(v)) }
}

unsafe fn graphix_value_buf_push_f64(buf: *mut LPooled<Vec<Value>>, v: f64) {
    unsafe { (*buf).push(Value::F64(v)) }
}

unsafe fn graphix_value_buf_push_i32(buf: *mut LPooled<Vec<Value>>, v: i32) {
    unsafe { (*buf).push(Value::I32(v)) }
}

unsafe fn graphix_value_buf_push_u32(buf: *mut LPooled<Vec<Value>>, v: u32) {
    unsafe { (*buf).push(Value::U32(v)) }
}

unsafe fn graphix_value_buf_push_f32(buf: *mut LPooled<Vec<Value>>, v: f32) {
    unsafe { (*buf).push(Value::F32(v)) }
}

/// Push a bool; any nonzero is true.
unsafe fn graphix_value_buf_push_bool(buf: *mut LPooled<Vec<Value>>, v: u8) {
    unsafe { (*buf).push(Value::Bool(v != 0)) }
}

unsafe fn graphix_value_buf_push_i8(buf: *mut LPooled<Vec<Value>>, v: i8) {
    unsafe { (*buf).push(Value::I8(v)) }
}

unsafe fn graphix_value_buf_push_i16(buf: *mut LPooled<Vec<Value>>, v: i16) {
    unsafe { (*buf).push(Value::I16(v)) }
}

unsafe fn graphix_value_buf_push_u8(buf: *mut LPooled<Vec<Value>>, v: u8) {
    unsafe { (*buf).push(Value::U8(v)) }
}

unsafe fn graphix_value_buf_push_u16(buf: *mut LPooled<Vec<Value>>, v: u16) {
    unsafe { (*buf).push(Value::U16(v)) }
}

unsafe fn graphix_value_buf_push_u64(buf: *mut LPooled<Vec<Value>>, v: u64) {
    unsafe { (*buf).push(Value::U64(v)) }
}

/// Push a `Value::Array` slot, taking ownership of `inner`.
unsafe fn graphix_value_buf_push_array(buf: *mut LPooled<Vec<Value>>, inner: u64) {
    unsafe { (*buf).push(Value::Array(va_owned(inner))) }
}

/// Extend `buf` with the elements of an owned ValArray, then drop it.
unsafe fn graphix_value_buf_extend_from_array(buf: *mut LPooled<Vec<Value>>, inner: u64) {
    unsafe {
        let owned = va_owned(inner);
        (*buf).extend(owned.iter().cloned());
    }
}

/// Push a clone of borrowed ValArray bits; the caller keeps its ref.
unsafe fn graphix_value_buf_push_array_borrowed(buf: *mut LPooled<Vec<Value>>, src: u64) {
    unsafe { (*buf).push(Value::Array(va_ref(&src).clone())) }
}

/// Push a clone of a borrowed Value; the caller keeps its ref.
safe fn graphix_value_buf_push_value_borrowed(buf: *mut LPooled<Vec<Value>>, v: TagValue) {
    let dup = v.with_value(|v| v.clone());
    std::mem::forget(v);
    unsafe { (*buf).push(dup) }
}

/// Push an owned Value, consuming it. The buffer holds clean Values:
/// the tag is stripped here.
safe fn graphix_value_buf_push_value(buf: *mut LPooled<Vec<Value>>, tv: TagValue) {
    unsafe { (*buf).push(tv.value()) }
}

/// Drop a buf that never reached `finalize`.
unsafe fn graphix_value_buf_drop(buf: *mut LPooled<Vec<Value>>) {
    assert!(!buf.is_null(), "graphix_value_buf_drop: null buf — JIT codegen bug");
    unsafe { drop(Box::from_raw(buf)) }
}

/// Push a `Value::String` cloned from a kernel strings-table slot.
unsafe fn graphix_value_buf_push_arcstr(
    buf: *mut LPooled<Vec<Value>>,
    ptr: *const arcstr::ArcStr,
) {
    unsafe { (*buf).push(Value::String((*ptr).clone())) }
}

/// Push an owned `ArcStr` as `Value::String`, consuming it.
unsafe fn graphix_value_buf_push_string(buf: *mut LPooled<Vec<Value>>, s: arcstr::ArcStr) {
    unsafe { (*buf).push(Value::String(s)) }
}

/// Finalize the buffer into owned `ValArray` bits, consuming the buf.
unsafe fn graphix_valarray_finalize(buf: *mut LPooled<Vec<Value>>) -> u64 {
    unsafe {
        let mut owned = *Box::from_raw(buf);
        va_bits(ValArray::from_iter_exact(owned.drain(..)))
    }
}

/// Borrowed ValArray bits → owned bits (refcount bump).
safe fn graphix_valarray_clone(bits: u64) -> u64 {
    va_bits(va_ref(&bits).clone())
}

/// Drop owned ValArray bits.
safe fn graphix_valarray_drop(bits: u64) {
    drop(va_owned(bits))
}

/// Extend `buf` with a list value's elements, consuming it; a non-list
/// value pushes as one element, as `ListFlatMap::finish` does.
unsafe fn graphix_value_buf_extend_from_list(
    buf: *mut LPooled<Vec<Value>>,
    tv: TagValue,
) {
    use crate::node::collection::list;
    let v = tv.value();
    let buf = unsafe { &mut *buf };
    if list::is_list(&v) {
        buf.extend(list::Iter::new(v));
    } else {
        buf.push(v);
    }
}

}

jit_helpers! { registry = control_helpers;

/// Read `KERNEL_ABORT` without clearing it: 1 if set, else 0. Emitted
/// after every cross-kernel call; a set flag means the callee aborted
/// and the caller must take its own abort exit.
safe fn graphix_abort_peek() -> u8 {
    KERNEL_ABORT.with(|c| if c.get() { 1 } else { 0 })
}

/// Logged on a fused call's return-shape mismatch branch, where the
/// wrong-shaped Value is dropped as bottom.
safe fn graphix_shape_mismatch_warn(got_disc: u64) {
    log::warn!(
        "fused call returned a Value whose shape (disc {got_disc:#x}) doesn't \
         match its declared return type — dropped as bottom (a stdlib builtin \
         violating its declared type, or a compiler bug)"
    );
}

/// Set `KERNEL_ABORT`; emitted on every whole-kernel abort path.
safe fn graphix_abort_set() {
    KERNEL_ABORT.with(|c| c.set(true))
}

/// Raise a `?` site's error onto the invocation's delivery queue
/// (`QOP_RAISES`). `(disc, payload)` is the error Value, borrowed: the
/// queue takes a clone. `Kernel::update` drains the queue in order.
unsafe fn graphix_qop_raise(site: u64, disc: u64, payload: u64) {
    // SAFETY: the words are a valid clean `Value`; viewed, never owned.
    let tv = unsafe { crate::TagValue::from_raw(disc, payload) };
    let v = tv.value_cloned();
    std::mem::forget(tv);
    // SAFETY: the kernel's interned QopSite outlives this invocation.
    let site = unsafe { &*(site as *const crate::node::error::QopSite) };
    site.handler.raise();
    QOP_RAISES.with(|q| q.borrow_mut().push((site, v)));
}

/// 1 if the active runtime has an `interrupt()`/`abort()` pending, else
/// 0. Emitted at every JIT loop head.
safe fn graphix_interrupted() -> i8 {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if p.is_null() {
            0
        } else {
            // SAFETY: `p` is the running `ExecCtx.control`, which outlives
            // the cycle; null when no cycle is running.
            i8::from(unsafe { (*p).interrupted() })
        }
    })
}

/// The kernel twin of `stack::ensure_sufficient`, asked at every native
/// self-call: 0 = interrupted (skip the call), 1 = call directly, 2 =
/// re-enter the callee on a fresh segment via [`graphix_grow_stack`].
safe fn graphix_stack_check() -> i8 {
    let interrupted = INTERRUPT_PTR.with(|c| {
        let p = c.get();
        // SAFETY: see `graphix_interrupted`.
        !p.is_null() && unsafe { (*p).interrupted() }
    });
    if interrupted {
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
safe fn graphix_grow_stack(thunk: i64, args: i64, out: i64) {
    // SAFETY: `thunk` has the fixed spill signature this crate emits;
    // `args`/`out` are the calling kernel's stack slots, live for the call.
    let f: extern "C" fn(i64, i64) = unsafe { std::mem::transmute(thunk as usize) };
    crate::stack::grow(|| f(args, out))
}

/// Call a builtin's registered `FastFn` directly. `args`/`n` is the
/// call site's stack buffer of (disc, payload) pairs, borrowed. See
/// [`fast_dispatch`] for the tag rules.
unsafe fn graphix_fastcall(
    fn_ptr: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> DynCallRet {
    // SAFETY: `fn_ptr` is the `FastFn` the emitter baked as an immediate.
    let f: crate::FastFn =
        unsafe { std::mem::transmute::<usize, crate::FastFn>(fn_ptr as usize) };
    unsafe { fast_dispatch(|args| f(args), args, n, taint_mask, stale_mask) }
}

/// Call a `TypedFastFn` with the site's interned `Type` and the
/// invoking kernel's env loan ([`with_kernel_env`]). Same buffer and
/// tag rules as [`graphix_fastcall`].
unsafe fn graphix_typedcall(
    fn_ptr: u64,
    typ: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> DynCallRet {
    // SAFETY: `fn_ptr` and `typ` are baked by the emitter and live as
    // long as the kernel's code.
    let f: crate::TypedFastFn =
        unsafe { std::mem::transmute::<usize, crate::TypedFastFn>(fn_ptr as usize) };
    let typ = unsafe { &*(typ as *const crate::typ::Type) };
    let env = KERNEL_ENV.with(|c| c.get());
    if env.is_null() {
        panic!(
            "graphix_typedcall: no kernel env loaned — Kernel::update must \
             run the wrapper under `with_kernel_env`"
        );
    }
    // SAFETY: the loan is scoped to the enclosing wrapper call, during
    // which `Env` is not touched mutably.
    let env = unsafe { &*env };
    unsafe { fast_dispatch(|args| f(env, typ, args), args, n, taint_mask, stale_mask) }
}

}

/// The trampoline core. The argument discs decide the tag: a tainted
/// argument bottoms the result without calling, all-stale arguments
/// make it STALE, and `None` from the fn is this cycle's bottom.
///
/// SAFETY: `args` is `n` valid clean `Value`s on the call site's stack,
/// viewed and never owned; the site releases what it owned afterwards.
unsafe fn fast_dispatch(
    call: impl FnOnce(&[Value]) -> Option<Value>,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> DynCallRet {
    let args_vec: &[Value] =
        unsafe { std::slice::from_raw_parts(args as *const Value, n as usize) };
    let n = n as usize;
    let all_stale = n > 0 && stale_mask == u64::MAX >> (64 - n);
    let bottom =
        if all_stale { crate::Tag::STALE_BOTTOM } else { crate::Tag::FRESH_BOTTOM };
    let tv = if taint_mask != 0 {
        crate::TagValue::tagged(Value::Null, bottom)
    } else {
        match call(args_vec) {
            Some(v) => crate::TagValue::tagged(
                v,
                if all_stale { crate::Tag::STALE } else { crate::Tag::FIRED },
            ),
            None => crate::TagValue::tagged(Value::Null, bottom),
        }
    };
    if crate::dbgenv::gxdbg_dync() {
        eprintln!(
            "FASTCALL n={n} taint={taint_mask:b} stale={stale_mask:b} -> {:?}",
            tv.tag()
        );
    }
    // SAFETY: TagValue is `#[repr(C)]` (disc, payload); ownership of the
    // bits transfers to the caller.
    let tv = std::mem::ManuallyDrop::new(tv);
    let words: [u64; 2] = unsafe { std::mem::transmute_copy(&*tv) };
    DynCallRet { word0: words[0], word1: words[1] }
}

// Value-shaped helpers take and return `Value` by value in two registers.
// Readers `mem::forget` their input so the caller keeps its ref; only the
// consuming helpers (drop, arith, index) take ownership.

/// Unchecked value arithmetic through netidx's operators. An Error
/// result becomes bottom, as the node-walk's BinOp does; consumes both.
fn value_arith_op(
    l: TagValue,
    r: TagValue,
    f: impl FnOnce(Value, Value) -> Value,
) -> TagValue {
    match f(l.value(), r.value()) {
        Value::Error(_) => TagValue::tainted(Value::Null),
        v => TagValue::clean(v),
    }
}

/// Borrowed read of payload `payload_idx` (slot 0 is the tag). A
/// placeholder or short array reads as the default.
fn variant_payload_read<T: Default>(
    v: TagValue,
    payload_idx: usize,
    read: impl Fn(&Value) -> T,
) -> T {
    let r = v.with_value(|v| match v {
        Value::Array(a) => a.get(payload_idx + 1).map(&read).unwrap_or_default(),
        _ => T::default(),
    });
    std::mem::forget(v);
    r
}

/// The j-th spine cell of a list value; `None` on a short or malformed chain.
fn list_walk(v: &Value, j: usize) -> Option<&Value> {
    use crate::node::collection::list;
    let mut cur = v;
    for _ in 0..j {
        match list::split(cur) {
            Some((_, t)) => cur = t,
            None => return None,
        }
    }
    Some(cur)
}

jit_helpers! { registry = value_helpers;

/// Unwrap an owned `Value::Array` into owned ValArray bits. The shape
/// check stays: a tainted `Null` placeholder is reachable here and its
/// zero payload must never be read as array bits.
safe fn graphix_value_into_array(v: TagValue) -> u64 {
    match v.value() {
        Value::Array(a) => va_bits(a),
        v => panic!("graphix_value_into_array: expected Value::Array, got {v:?}"),
    }
}

/// Borrowed form of [`graphix_value_into_array`]: owned bits out, the
/// caller keeps its Value.
safe fn graphix_value_into_array_borrowed(v: TagValue) -> u64 {
    let bits = v.with_value(|v| match v {
        Value::Array(a) => va_bits(a.clone()),
        v => {
            panic!("graphix_value_into_array_borrowed: expected Value::Array, got {v:?}")
        }
    });
    std::mem::forget(v);
    bits
}

/// Drop an owned Value. Disc 0 is never a real Value, so the pending
/// sentinel is rejected before an invalid `Value` materializes.
safe fn graphix_value_drop(tv: TagValue) {
    assert!(
        !tv.is_sentinel(),
        "graphix_value_drop: zero discriminant — JIT codegen bug \
         (a pending sentinel leaked into a drop)"
    );
    drop(tv)
}

/// Clone a borrowed Value (tag preserved); the caller keeps its ref.
safe fn graphix_value_clone(tv: TagValue) -> TagValue {
    let dup = tv.clone();
    std::mem::forget(tv);
    dup
}

/// Clone a `Value` from a kernel's value-constants table slot.
///
/// # Safety
/// `ptr` must point to a live `Value` that outlives the JIT'd code.
unsafe fn graphix_value_clone_from_static(ptr: *const Value) -> TagValue {
    TagValue::clean(unsafe { (*ptr).clone() })
}

/// The abstract constructor `T(v)`: box `tv` under abstract type `id`.
/// Consumes `tv`.
unsafe fn graphix_abstract_wrap(id: u64, name: *const arcstr::ArcStr, tv: TagValue) -> TagValue {
    let name = unsafe { (*name).clone() };
    let id = crate::typ::AbstractId::from_inner(id);
    TagValue::clean(crate::abstract_value::wrap(id, name, tv.value()))
}

safe fn graphix_abstract_get_i64(tv: TagValue) -> i64 {
    abstract_payload_read(tv, read_slot_i64)
}

safe fn graphix_abstract_get_u64(tv: TagValue) -> u64 {
    abstract_payload_read(tv, read_slot_u64)
}

safe fn graphix_abstract_get_i32(tv: TagValue) -> i32 {
    abstract_payload_read(tv, read_slot_i32)
}

safe fn graphix_abstract_get_u32(tv: TagValue) -> u32 {
    abstract_payload_read(tv, read_slot_u32)
}

safe fn graphix_abstract_get_i16(tv: TagValue) -> i16 {
    abstract_payload_read(tv, read_slot_i16)
}

safe fn graphix_abstract_get_u16(tv: TagValue) -> u16 {
    abstract_payload_read(tv, read_slot_u16)
}

safe fn graphix_abstract_get_i8(tv: TagValue) -> i8 {
    abstract_payload_read(tv, read_slot_i8)
}

safe fn graphix_abstract_get_u8(tv: TagValue) -> u8 {
    abstract_payload_read(tv, read_slot_u8)
}

safe fn graphix_abstract_get_f64(tv: TagValue) -> f64 {
    abstract_payload_read(tv, read_slot_f64)
}

safe fn graphix_abstract_get_f32(tv: TagValue) -> f32 {
    abstract_payload_read(tv, read_slot_f32)
}

safe fn graphix_abstract_get_bool(tv: TagValue) -> u8 {
    abstract_payload_read(tv, read_slot_bool)
}

safe fn graphix_abstract_get_arcstr(tv: TagValue) -> arcstr::ArcStr {
    let r = tv.with_value(|v| slot_arcstr(crate::abstract_value::payload(v)));
    std::mem::forget(tv);
    r
}

safe fn graphix_abstract_get_array(tv: TagValue) -> u64 {
    let r = tv.with_value(|v| va_bits(slot_array(crate::abstract_value::payload(v)).clone()));
    std::mem::forget(tv);
    r
}

safe fn graphix_abstract_get_value(tv: TagValue) -> TagValue {
    let r = tv.with_value(|v| {
        TagValue::clean(crate::abstract_value::payload(v).cloned().unwrap_or(Value::Null))
    });
    std::mem::forget(tv);
    r
}

// Value arithmetic consumes both operands; codegen passes them owned.

safe fn graphix_value_add(l: TagValue, r: TagValue) -> TagValue {
    value_arith_op(l, r, |a, b| a + b)
}

safe fn graphix_value_sub(l: TagValue, r: TagValue) -> TagValue {
    value_arith_op(l, r, |a, b| a - b)
}

safe fn graphix_value_mul(l: TagValue, r: TagValue) -> TagValue {
    value_arith_op(l, r, |a, b| a * b)
}

safe fn graphix_value_div(l: TagValue, r: TagValue) -> TagValue {
    value_arith_op(l, r, |a, b| a / b)
}

safe fn graphix_value_rem(l: TagValue, r: TagValue) -> TagValue {
    value_arith_op(l, r, |a, b| a % b)
}

// Checked arithmetic yields the catchable `ArithError` value, never
// bottom; consumes both operands.

safe fn graphix_value_checked_add(l: TagValue, r: TagValue) -> TagValue {
    TagValue::clean(wrap_arith_error(l.value().checked_add(r.value())))
}

safe fn graphix_value_checked_sub(l: TagValue, r: TagValue) -> TagValue {
    TagValue::clean(wrap_arith_error(l.value().checked_sub(r.value())))
}

safe fn graphix_value_checked_mul(l: TagValue, r: TagValue) -> TagValue {
    TagValue::clean(wrap_arith_error(l.value().checked_mul(r.value())))
}

safe fn graphix_value_checked_div(l: TagValue, r: TagValue) -> TagValue {
    TagValue::clean(wrap_arith_error(l.value().checked_div(r.value())))
}

safe fn graphix_value_checked_rem(l: TagValue, r: TagValue) -> TagValue {
    TagValue::clean(wrap_arith_error(l.value().checked_rem(r.value())))
}

/// Value equality; consumes both operands.
safe fn graphix_value_eq(l: TagValue, r: TagValue) -> u8 {
    (l.value() == r.value()) as u8
}

/// `bytes[i]`: the `u8` or the index error, via the shared
/// [`bytes_index`]. Consumes `v`.
safe fn graphix_bytes_index(v: TagValue, i: i64) -> TagValue {
    TagValue::clean(match v.value() {
        Value::Bytes(b) => bytes_index(&b, i),
        _ => Value::error("ArrayIndexError: expected bytes"),
    })
}

/// `m{key}`: the value or the not-found error, via the shared
/// [`map_get`]. Consumes both operands.
safe fn graphix_map_ref(map: TagValue, key: TagValue) -> TagValue {
    TagValue::clean(map_get(&map.value(), &key.value()))
}

/// `a[i..j]` over an array or bytes: `flags` bit0 = `start` present,
/// bit1 = `end` present. Consumes `src`.
safe fn graphix_array_slice(src: TagValue, start: i64, end: i64, flags: i64) -> TagValue {
    let s = if flags & 1 != 0 { Some(start) } else { None };
    let e = if flags & 2 != 0 { Some(end) } else { None };
    TagValue::clean(array_slice_i64(&src.value(), s, e))
}

/// Borrowed `Value::Null` test. Lowering inlines the disc compare; the
/// helper stays registered for direct callers.
safe fn graphix_value_is_null(v: TagValue) -> u8 {
    let r = v.with_value(|v| matches!(v, Value::Null) as u8);
    std::mem::forget(v);
    r
}

/// Borrowed test of a variant's tag AND arity against `expected`. As
/// in `StructurePattern::is_match`, arity selects the representation
/// (`String(tag)` at 0, else an array of arity + 1 with the tag at
/// slot 0); the tag alone does not discriminate `` [`A, `A(i64)] ``.
safe fn graphix_variant_tag_eq(
    v: TagValue,
    expected: *const arcstr::ArcStr,
    arity: usize,
) -> u8 {
    let r = v.with_value(|v| {
        let exp = unsafe { &*expected };
        match v {
            Value::String(s) => (arity == 0 && s.as_str() == exp.as_str()) as u8,
            Value::Array(a) => match a.get(0) {
                Some(Value::String(tag)) => (arity > 0
                    && a.len() == arity + 1
                    && tag.as_str() == exp.as_str()) as u8,
                _ => 0,
            },
            _ => 0,
        }
    });
    std::mem::forget(v);
    r
}

safe fn graphix_variant_payload_i64(v: TagValue, payload_idx: usize) -> i64 {
    variant_payload_read(v, payload_idx, read_slot_i64)
}

safe fn graphix_variant_payload_f64(v: TagValue, payload_idx: usize) -> f64 {
    variant_payload_read(v, payload_idx, read_slot_f64)
}

safe fn graphix_variant_payload_i32(v: TagValue, payload_idx: usize) -> i32 {
    variant_payload_read(v, payload_idx, read_slot_i32)
}

safe fn graphix_variant_payload_u32(v: TagValue, payload_idx: usize) -> u32 {
    variant_payload_read(v, payload_idx, read_slot_u32)
}

safe fn graphix_variant_payload_f32(v: TagValue, payload_idx: usize) -> f32 {
    variant_payload_read(v, payload_idx, read_slot_f32)
}

safe fn graphix_variant_payload_i8(v: TagValue, payload_idx: usize) -> i8 {
    variant_payload_read(v, payload_idx, read_slot_i8)
}

safe fn graphix_variant_payload_i16(v: TagValue, payload_idx: usize) -> i16 {
    variant_payload_read(v, payload_idx, read_slot_i16)
}

safe fn graphix_variant_payload_u8(v: TagValue, payload_idx: usize) -> u8 {
    variant_payload_read(v, payload_idx, read_slot_u8)
}

/// Owned clone of a variant payload slot as a Value; a shape mismatch
/// yields the drop-safe `Value::Null`.
safe fn graphix_variant_payload_value(v: TagValue, payload_idx: usize) -> TagValue {
    let r = v.with_value(|v| match v {
        Value::Array(a) => a.get(payload_idx + 1).cloned().unwrap_or(Value::Null),
        _ => Value::Null,
    });
    std::mem::forget(v);
    TagValue::clean(r)
}

/// Owned `ArcStr` clone of a string variant payload slot; mismatch
/// yields the static empty string.
safe fn graphix_variant_payload_string(v: TagValue, payload_idx: usize) -> u64 {
    let r = v.with_value(|v| match v {
        Value::Array(a) => match a.get(payload_idx + 1) {
            Some(Value::String(s)) => s.clone(),
            _ => arcstr::ArcStr::new(),
        },
        _ => arcstr::ArcStr::new(),
    });
    std::mem::forget(v);
    unsafe { std::mem::transmute::<arcstr::ArcStr, u64>(r) }
}

/// List-pattern structure test: `k` cells exist; `exact` also requires
/// nil after them. A non-list fails the walk.
safe fn graphix_list_match(v: TagValue, k: usize, exact: u8) -> u8 {
    use crate::node::collection::list;
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
    std::mem::forget(v);
    r
}

/// Owned clone of the j-th head of a list as a Value; `Null` on a short chain.
safe fn graphix_list_get_value(v: TagValue, j: usize) -> TagValue {
    use crate::node::collection::list;
    let r = v.with_value(|v| match list_walk(v, j).and_then(|c| list::split(c)) {
        Some((h, _)) => h.clone(),
        None => Value::Null,
    });
    std::mem::forget(v);
    TagValue::clean(r)
}

/// Owned `ValArray` bits of the j-th head; the empty array on mismatch.
safe fn graphix_list_get_array(v: TagValue, j: usize) -> u64 {
    use crate::node::collection::list;
    let r = v.with_value(|v| match list_walk(v, j).and_then(|c| list::split(c)) {
        Some((Value::Array(a), _)) => a.clone(),
        _ => EMPTY_ARR.clone(),
    });
    std::mem::forget(v);
    va_bits(r)
}

/// Owned `ArcStr` of the j-th head; the empty string on mismatch.
safe fn graphix_list_get_string(v: TagValue, j: usize) -> u64 {
    let r = v.with_value(|v| {
        match list_walk(v, j).and_then(|c| crate::node::collection::list::split(c)) {
            Some((Value::String(s), _)) => s.clone(),
            _ => arcstr::ArcStr::new(),
        }
    });
    std::mem::forget(v);
    unsafe { std::mem::transmute::<arcstr::ArcStr, u64>(r) }
}

/// Owned clone of the k-th tail (the `[<h, rest..>]` rest bind), O(1)
/// shared structure; `Null` on a short chain.
safe fn graphix_list_tail(v: TagValue, k: usize) -> TagValue {
    let r = v.with_value(|v| match list_walk(v, k) {
        Some(cur) => cur.clone(),
        None => Value::Null,
    });
    std::mem::forget(v);
    TagValue::clean(r)
}

/// Owned `ValArray` bits of a composite variant payload slot; the
/// empty array on mismatch.
safe fn graphix_variant_payload_array(v: TagValue, payload_idx: usize) -> u64 {
    let r = v.with_value(|v| match v {
        Value::Array(a) => match a.get(payload_idx + 1) {
            Some(Value::Array(inner)) => inner.clone(),
            _ => EMPTY_ARR.clone(),
        },
        _ => EMPTY_ARR.clone(),
    });
    std::mem::forget(v);
    va_bits(r)
}

safe fn graphix_variant_payload_u16(v: TagValue, payload_idx: usize) -> u16 {
    variant_payload_read(v, payload_idx, read_slot_u16)
}

safe fn graphix_variant_payload_u64(v: TagValue, payload_idx: usize) -> u64 {
    variant_payload_read(v, payload_idx, read_slot_u64)
}

safe fn graphix_variant_payload_bool(v: TagValue, payload_idx: usize) -> u8 {
    variant_payload_read(v, payload_idx, read_slot_bool)
}

}

// A String SSA value is the raw `ArcStr` pointer; every owned one is
// consumed by a helper or returned across the kernel boundary.

/// Render a primitive as `Value::<T>(v).to_string()` does.
fn push_display<T: std::fmt::Display>(buf: *mut String, v: T) {
    use std::fmt::Write;
    let _ = write!(unsafe { &mut *buf }, "{v}");
}

jit_helpers! { registry = string_helpers;

/// Owned clone of a kernel strings-table slot.
unsafe fn graphix_arcstr_clone_from_static(p: *const arcstr::ArcStr) -> arcstr::ArcStr {
    unsafe { (*p).clone() }
}

/// Drop an owned ArcStr. Takes raw bits so the zero pending sentinel is
/// rejected before an invalid `ArcStr` (NonNull) materializes.
safe fn graphix_arcstr_drop(s: u64) {
    assert!(
        s != 0,
        "graphix_arcstr_drop: null ArcStr — JIT codegen bug \
         (a pending sentinel leaked into a drop)"
    );
    // SAFETY: nonzero bits that came from an ArcStr-producing helper.
    drop(unsafe { std::mem::transmute::<u64, arcstr::ArcStr>(s) })
}

/// Clone a borrowed ArcStr; the caller keeps its ref.
safe fn graphix_arcstr_clone(s: arcstr::ArcStr) -> arcstr::ArcStr {
    let dup = s.clone();
    std::mem::forget(s);
    dup
}

/// The empty-`ArcStr` placeholder for a tainted String position, as bits.
safe fn graphix_arcstr_empty() -> u64 {
    unsafe { std::mem::transmute::<arcstr::ArcStr, u64>(arcstr::ArcStr::new()) }
}

/// The empty-`ValArray` placeholder for a tainted composite position,
/// as owned bits.
safe fn graphix_valarray_empty() -> u64 {
    va_bits(EMPTY_ARR.clone())
}

/// Start an owned string buffer; pair with `graphix_string_buf_finalize`
/// or `graphix_string_buf_drop`.
safe fn graphix_string_buf_new() -> *mut String {
    Box::into_raw(Box::new(String::new()))
}

/// Drop a string buf without finalizing.
unsafe fn graphix_string_buf_drop(buf: *mut String) {
    assert!(!buf.is_null(), "graphix_string_buf_drop: null buf — JIT codegen bug");
    drop(unsafe { Box::from_raw(buf) })
}

/// Finalize a string buf into an owned ArcStr, consuming the buf.
unsafe fn graphix_string_buf_finalize(buf: *mut String) -> arcstr::ArcStr {
    let s = unsafe { *Box::from_raw(buf) };
    arcstr::ArcStr::from(s.as_str())
}

/// Append an ArcStr's contents to the buf, consuming the ArcStr.
unsafe fn graphix_string_buf_push_arcstr(buf: *mut String, s: arcstr::ArcStr) {
    unsafe { &mut *buf }.push_str(&s);
}

unsafe fn graphix_string_buf_push_i64(buf: *mut String, v: i64) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_u64(buf: *mut String, v: u64) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_i32(buf: *mut String, v: i32) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_u32(buf: *mut String, v: u32) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_i16(buf: *mut String, v: i16) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_u16(buf: *mut String, v: u16) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_i8(buf: *mut String, v: i8) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_u8(buf: *mut String, v: u8) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_f64(buf: *mut String, v: f64) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_f32(buf: *mut String, v: f32) {
    push_display(buf, v)
}

unsafe fn graphix_string_buf_push_bool(buf: *mut String, v: u8) {
    push_display(buf, v != 0)
}

}

// The scaffold loops iterate a ValArray; List and Map sources flatten on
// entry and rebuild on exit through the same `node::collection`
// functions the interpreted MapQ/FoldQ use.

jit_helpers! { registry = collection_helpers;

/// Flatten a List value into owned ValArray bits, consuming it. A
/// non-list (the tainted placeholder) yields the empty array; the
/// source disc's taint rides the loop's SlotFlags.
safe fn graphix_list_to_valarray(tv: TagValue) -> u64 {
    let v = tv.value();
    let arr =
        crate::node::collection::list::to_array(&v).unwrap_or_else(|| ValArray::from([]));
    va_bits(arr)
}

/// Consume finalized ValArray bits and build the List value.
safe fn graphix_valarray_into_list(bits: u64) -> TagValue {
    let arr = va_owned(bits);
    TagValue::clean(crate::node::collection::list::from_iter(arr.iter().cloned()))
}

/// Flatten a Map value into owned ValArray bits of `[k, v]` pairs in
/// key order, consuming it; a non-map yields the empty array.
safe fn graphix_cmap_to_pairs(tv: TagValue) -> u64 {
    let v = tv.value();
    let arr = match &v {
        Value::Map(m) => ValArray::from_iter(
            m.into_iter().map(|(k, v)| crate::node::collection::make_pair(k, v)),
        ),
        _ => ValArray::from([]),
    };
    va_bits(arr)
}

/// Consume finalized ValArray bits of `[k, v]` pairs and build a
/// `Value::Map`; a malformed pair is logged and skipped.
safe fn graphix_valarray_into_cmap(bits: u64) -> TagValue {
    let arr = va_owned(bits);
    let m = netidx_value::Map::from_iter(arr.iter().filter_map(|v| {
        let pair = crate::node::collection::split_pair(v);
        if pair.is_none() {
            log::error!("graphix_valarray_into_cmap: malformed pair {v:?}");
        }
        pair
    }));
    TagValue::clean(Value::Map(m))
}

}

/// Borrowed read of an abstract value's payload (`.0`); a non-abstract
/// value reads as the shape's placeholder.
fn abstract_payload_read<T: Default>(tv: TagValue, f: fn(&Value) -> T) -> T {
    let r =
        tv.with_value(|v| crate::abstract_value::payload(v).map(f).unwrap_or_default());
    std::mem::forget(tv);
    r
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
static EMPTY_ARR: std::sync::LazyLock<ValArray> =
    std::sync::LazyLock::new(|| ValArray::from_iter_exact(std::iter::empty()));

/// Free a slot-state chain: `word` is 0 or a `Box<Vec<u64>>`. With
/// `own_levels > 0` each entry is a chain one level down; at 0 the Vec
/// is plain words, or call-site blocks owning chains when `leaf` is given.
pub fn free_slot_chain(word: u64, own_levels: u64, leaf: Option<&SiteLeaf>) {
    if word == 0 {
        return;
    }
    let v = unsafe { Box::from_raw(word as *mut Vec<u64>) };
    if own_levels > 0 {
        for e in v.iter() {
            free_slot_chain(*e, own_levels - 1, leaf);
        }
    } else if let Some(l) = leaf {
        free_blocks(&v, l);
    }
}

/// Free a per-activation block tree rooted at `vecptr` (one
/// `Box<Vec<u64>>` per activation, children at `slots`). Iterative:
/// the tree is as deep as the recursion was. Returns the blocks freed.
pub fn free_self_block_tree(vecptr: u64, slots: &[u32]) -> u64 {
    let mut freed = 0u64;
    let mut work: poolshark::local::LPooled<Vec<u64>> = poolshark::local::LPooled::take();
    work.push(vecptr);
    while let Some(p) = work.pop() {
        if p == 0 {
            continue;
        }
        let v = unsafe { Box::from_raw(p as *mut Vec<u64>) };
        freed += 1;
        LIVE_SELF_BLOCKS.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
        work.extend(slots.iter().filter_map(|s| v.get(*s as usize).copied()));
    }
    freed
}

/// Live per-activation `SelfBlock` count (a test instrument for the
/// reclaim).
pub static LIVE_SELF_BLOCKS: std::sync::atomic::AtomicI64 =
    std::sync::atomic::AtomicI64::new(0);

thread_local! {
    /// The reach generation of the running kernel invocation; every
    /// activation block reached is stamped with it and the reclaim
    /// frees the rest. Saved/restored around every kernel invocation.
    pub(crate) static SELF_BLOCK_GEN: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
    /// Activation reaches this invocation; the reclaim runs only when
    /// it is below the tree size. Saved/restored like [`SELF_BLOCK_GEN`].
    pub(crate) static SELF_BLOCK_REACHED: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// Free the subtrees of a per-activation block tree not stamped with
/// `generation`, nulling each freed subtree's root word. `root` is the
/// address of the tree's root word; the stamp lives at index `words`,
/// one past the block's emitted layout. Returns the blocks freed.
pub fn reclaim_self_block_tree(
    root: *mut u64,
    words: usize,
    slots: &[u32],
    generation: u64,
) -> u64 {
    let mut freed = 0u64;
    let mut work: poolshark::local::LPooled<Vec<*mut u64>> =
        poolshark::local::LPooled::take();
    work.push(root);
    while let Some(wp) = work.pop() {
        let p = unsafe { *wp };
        if p == 0 {
            continue;
        }
        let v: &mut Vec<u64> = unsafe { &mut *(p as *mut Vec<u64>) };
        // A block with no stamp word counts as reached: never free what
        // is not proven shed.
        let stamp = v.get(words).copied().unwrap_or(generation);
        if stamp != generation {
            unsafe { *wp = 0 };
            freed += free_self_block_tree(p, slots);
        } else {
            let base = v.as_mut_ptr();
            for s in slots.iter() {
                work.push(unsafe { base.add(*s as usize) });
            }
        }
    }
    freed
}

/// Free the anchor-owned chains inside a run of call-site blocks.
fn free_blocks(words: &[u64], leaf: &SiteLeaf) {
    for block in words.chunks_exact(leaf.stride as usize) {
        for a in leaf.anchors.iter() {
            free_slot_chain(
                block[a.rel as usize],
                a.own_levels as u64,
                a.leaf.as_deref(),
            );
        }
    }
}

/// The value of struct field `sorted_idx` (`arr[sorted_idx]` is a
/// `[name, value]` pair); `None` on OOB or a non-pair slot.
fn struct_field(p: &ValArray, sorted_idx: usize) -> Option<&Value> {
    match p.get(sorted_idx)? {
        Value::Array(kv) => kv.get(1),
        _ => None,
    }
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

jit_helpers! { registry = elem_helpers;

safe fn graphix_valarray_get_i64(bits: u64, idx: usize) -> i64 {
    va_ref(&bits).get(idx).map(read_slot_i64).unwrap_or_default()
}

safe fn graphix_valarray_get_f64(bits: u64, idx: usize) -> f64 {
    va_ref(&bits).get(idx).map(read_slot_f64).unwrap_or_default()
}

safe fn graphix_valarray_get_i32(bits: u64, idx: usize) -> i32 {
    va_ref(&bits).get(idx).map(read_slot_i32).unwrap_or_default()
}

safe fn graphix_valarray_get_u32(bits: u64, idx: usize) -> u32 {
    va_ref(&bits).get(idx).map(read_slot_u32).unwrap_or_default()
}

safe fn graphix_valarray_get_f32(bits: u64, idx: usize) -> f32 {
    va_ref(&bits).get(idx).map(read_slot_f32).unwrap_or_default()
}

safe fn graphix_valarray_get_bool(bits: u64, idx: usize) -> u8 {
    va_ref(&bits).get(idx).map(read_slot_bool).unwrap_or_default()
}

safe fn graphix_valarray_get_i8(bits: u64, idx: usize) -> i8 {
    va_ref(&bits).get(idx).map(read_slot_i8).unwrap_or_default()
}

safe fn graphix_valarray_get_i16(bits: u64, idx: usize) -> i16 {
    va_ref(&bits).get(idx).map(read_slot_i16).unwrap_or_default()
}

safe fn graphix_valarray_get_u8(bits: u64, idx: usize) -> u8 {
    va_ref(&bits).get(idx).map(read_slot_u8).unwrap_or_default()
}

safe fn graphix_valarray_get_u16(bits: u64, idx: usize) -> u16 {
    va_ref(&bits).get(idx).map(read_slot_u16).unwrap_or_default()
}

safe fn graphix_valarray_get_u64(bits: u64, idx: usize) -> u64 {
    va_ref(&bits).get(idx).map(read_slot_u64).unwrap_or_default()
}

safe fn graphix_valarray_len(bits: u64) -> usize {
    va_ref(&bits).len()
}

/// Source-level `arr[idx]` via the shared [`array_index`]: the element
/// or the index error; negative `idx` counts from the end.
safe fn graphix_valarray_index(bits: u64, idx: i64) -> TagValue {
    TagValue::clean(array_index(va_ref(&bits), idx))
}

/// Resize a scaffold loop's per-slot state table (a boxed `Vec<u64>`
/// owned by `*word`, one word per slot, 0 = no prior observation) to
/// `len` with prefix retention; truncation frees the dropped slots'
/// subtrees (`own_levels`, `leaf` as in [`free_slot_chain`]). With
/// `valid == 0` (tainted source) the table only grows, zero-filled,
/// so in-loop accesses up to `len` stay in bounds.
unsafe fn graphix_slot_state_table(
    word: *mut u64,
    len: u64,
    valid: u64,
    own_levels: u64,
    leaf: u64,
) -> *mut u64 {
    let leaf =
        if leaf == 0 { None } else { Some(unsafe { &*(leaf as *const SiteLeaf) }) };
    let word = unsafe { &mut *word };
    if *word == 0 {
        *word = Box::into_raw(Box::new(Vec::<u64>::new())) as u64;
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    let len = len as usize;
    if valid != 0 && len < v.len() {
        if own_levels > 0 {
            for e in v[len..].iter() {
                free_slot_chain(*e, own_levels - 1, leaf);
            }
        }
        v.truncate(len)
    } else if len > v.len() {
        v.resize(len, 0)
    }
    v.as_mut_ptr()
}

/// The per-activation block for a self-call, allocated on first use
/// and stamped with the reach generation. `word` is the root word in
/// the caller's block (null = no memory); `desc` is the callee's
/// `KernelSig::site_desc`, read at run time because a self-call's
/// block size is unknown while its body is still emitting.
unsafe fn graphix_site_child_block(word: *mut u64, desc: *const u64) -> *mut u64 {
    use std::sync::atomic::{AtomicU64, Ordering::Relaxed};
    if word.is_null() {
        return std::ptr::null_mut();
    }
    let words = unsafe { (*(desc as *const AtomicU64)).load(Relaxed) } as usize;
    if words == 0 {
        return std::ptr::null_mut();
    }
    let word = unsafe { &mut *word };
    if *word == 0 {
        // Index `words`, past the emitted layout, holds the generation stamp.
        *word = Box::into_raw(Box::new(vec![0u64; words + 1])) as u64;
        LIVE_SELF_BLOCKS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    v[words] = SELF_BLOCK_GEN.get();
    SELF_BLOCK_REACHED.set(SELF_BLOCK_REACHED.get() + 1);
    v.as_mut_ptr()
}

/// [`graphix_slot_state_table`] for a chain leaf of per-slot call-site
/// blocks (`stride` words each); truncation frees the dropped blocks'
/// anchor-owned chains.
unsafe fn graphix_slot_state_blocks(
    word: *mut u64,
    slots: u64,
    valid: u64,
    leaf: u64,
) -> *mut u64 {
    let leaf_ref = unsafe { &*(leaf as *const SiteLeaf) };
    let word = unsafe { &mut *word };
    if *word == 0 {
        *word = Box::into_raw(Box::new(Vec::<u64>::new())) as u64;
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    let len = (slots as usize) * (leaf_ref.stride as usize);
    if valid != 0 && len < v.len() {
        free_blocks(&v[len..], leaf_ref);
        v.truncate(len)
    } else if len > v.len() {
        v.resize(len, 0)
    }
    v.as_mut_ptr()
}

safe fn graphix_struct_get_i64(bits: u64, sorted_idx: usize) -> i64 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_i64).unwrap_or_default()
}

safe fn graphix_struct_get_f64(bits: u64, sorted_idx: usize) -> f64 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_f64).unwrap_or_default()
}

safe fn graphix_struct_get_i32(bits: u64, sorted_idx: usize) -> i32 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_i32).unwrap_or_default()
}

safe fn graphix_struct_get_u32(bits: u64, sorted_idx: usize) -> u32 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_u32).unwrap_or_default()
}

safe fn graphix_struct_get_f32(bits: u64, sorted_idx: usize) -> f32 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_f32).unwrap_or_default()
}

safe fn graphix_struct_get_bool(bits: u64, sorted_idx: usize) -> u8 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_bool).unwrap_or_default()
}

safe fn graphix_struct_get_i8(bits: u64, sorted_idx: usize) -> i8 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_i8).unwrap_or_default()
}

safe fn graphix_struct_get_i16(bits: u64, sorted_idx: usize) -> i16 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_i16).unwrap_or_default()
}

safe fn graphix_struct_get_u8(bits: u64, sorted_idx: usize) -> u8 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_u8).unwrap_or_default()
}

safe fn graphix_struct_get_u16(bits: u64, sorted_idx: usize) -> u16 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_u16).unwrap_or_default()
}

safe fn graphix_struct_get_u64(bits: u64, sorted_idx: usize) -> u64 {
    struct_field(va_ref(&bits), sorted_idx).map(read_slot_u64).unwrap_or_default()
}

/// `arr[idx]` as owned ValArray bits.
safe fn graphix_valarray_get_array(bits: u64, idx: usize) -> u64 {
    va_bits(slot_array(va_ref(&bits).get(idx)).clone())
}

/// `arr[idx]` as borrowed ValArray bits: valid only while the parent
/// array is alive, never passed to a consuming or dropping helper.
safe fn graphix_valarray_get_array_borrowed(bits: u64, idx: usize) -> u64 {
    va_borrowed_bits(slot_array(va_ref(&bits).get(idx)))
}

/// A struct field as borrowed ValArray bits; same lifetime contract as
/// [`graphix_valarray_get_array_borrowed`].
safe fn graphix_struct_get_array_borrowed(bits: u64, sorted_idx: usize) -> u64 {
    va_borrowed_bits(slot_array(struct_field(va_ref(&bits), sorted_idx)))
}

/// `arr[idx]` as an owned `ArcStr` (String elem).
safe fn graphix_valarray_get_arcstr(bits: u64, idx: usize) -> arcstr::ArcStr {
    slot_arcstr(va_ref(&bits).get(idx))
}

/// `arr[idx]` as an owned `Value`.
safe fn graphix_valarray_get_value(bits: u64, idx: usize) -> TagValue {
    TagValue::clean(va_ref(&bits).get(idx).cloned().unwrap_or(Value::Null))
}

/// A struct field as owned ValArray bits.
safe fn graphix_struct_get_array(bits: u64, sorted_idx: usize) -> u64 {
    va_bits(slot_array(struct_field(va_ref(&bits), sorted_idx)).clone())
}

safe fn graphix_struct_get_arcstr(bits: u64, sorted_idx: usize) -> arcstr::ArcStr {
    slot_arcstr(struct_field(va_ref(&bits), sorted_idx))
}

safe fn graphix_struct_get_value(bits: u64, sorted_idx: usize) -> TagValue {
    TagValue::clean(
        struct_field(va_ref(&bits), sorted_idx).cloned().unwrap_or(Value::Null),
    )
}

}

#[cfg(debug_assertions)]
jit_helpers! { registry = debug_helpers;

/// Print a tagged disc word from inside JIT'd code (`GXDBG_CALLRET`).
safe fn graphix_dbg_disc(tag: u64, disc: u64) {
    eprintln!("CLIF-DISC tag={tag} disc={disc:x}");
}

/// Bump `JIT_INVOCATIONS`; emitted at the start of every wrapper.
safe fn graphix_record_jit_invocation() {
    JIT_INVOCATIONS.with(|c| c.set(c.get().wrapping_add(1)));
}

}

use std::cell::Cell;

/// The trampolines' return: `word0` = the Value disc (tag in-band),
/// `word1` = the Value payload word for every return type; the call
/// site adapts it to its static type.
#[repr(C)]
pub struct DynCallRet {
    pub word0: u64,
    pub word1: u64,
}

thread_local! {
    /// Sticky abort flag: set on a whole-kernel abort path, reset by
    /// `Kernel::update` before each wrapper call and read after; set
    /// means the result is the abort sentinel.
    pub static KERNEL_ABORT: Cell<bool> = const { Cell::new(false) };

    /// The invoking kernel's type environment, loaned for one wrapper
    /// call ([`with_kernel_env`]); null when no kernel is in flight.
    pub static KERNEL_ENV: Cell<*const crate::env::Env> =
        const { Cell::new(std::ptr::null()) };

    /// The invocation's `?` delivery queue, in execution order, drained
    /// after the wrapper returns ([`with_qop_raises`]); a kernel never
    /// delivers mid-run.
    pub static QOP_RAISES: std::cell::RefCell<Vec<(*const crate::node::error::QopSite, Value)>> =
        const { std::cell::RefCell::new(Vec::new()) };

    /// The active runtime's [`crate::Control`], set per cycle by
    /// `do_cycle`; null when no cycle is in flight.
    pub static INTERRUPT_PTR: Cell<*const crate::Control> =
        const { Cell::new(std::ptr::null()) };

    /// Per-thread count of JIT'd wrapper runs; the test harness's `jit`
    /// mode asserts it is nonzero.
    #[cfg(debug_assertions)]
    pub static JIT_INVOCATIONS: Cell<u64> = const { Cell::new(0) };

    /// Per-thread count of fused-kernel executions
    /// ([`record_fusion_invocation`]).
    #[cfg(debug_assertions)]
    pub static FUSION_INVOCATIONS: Cell<u64> = const { Cell::new(0) };
}

/// Point `graphix_interrupted` at `control` on the current thread;
/// called at the start of each cycle since the task may migrate.
pub fn set_interrupt_ptr(control: &crate::Control) {
    INTERRUPT_PTR.with(|c| c.set(control as *const crate::Control));
}

/// Abort the runtime this thread is running under (the stack budget's
/// containment); a no-op with no runtime on this thread.
pub(crate) fn abort_current_control_budget() {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if !p.is_null() {
            // SAFETY: see `graphix_interrupted`.
            unsafe { (*p).abort_budget() }
        }
    });
}

/// Bump the per-thread fused-kernel execution counter once a kernel
/// commits to running.
#[cfg(debug_assertions)]
pub fn record_fusion_invocation() {
    FUSION_INVOCATIONS.with(|c| c.set(c.get().wrapping_add(1)));
}

/// Read the current thread's fused-kernel execution count.
#[cfg(debug_assertions)]
pub fn fusion_invocations() -> u64 {
    FUSION_INVOCATIONS.with(|c| c.get())
}

/// Reset the current thread's fused-kernel execution count to zero.
#[cfg(debug_assertions)]
pub fn reset_fusion_invocations() {
    FUSION_INVOCATIONS.with(|c| c.set(0));
}

/// Read the current thread's JIT invocation count.
#[cfg(debug_assertions)]
pub fn jit_invocations() -> u64 {
    JIT_INVOCATIONS.with(|c| c.get())
}

/// Reset the current thread's JIT invocation count to zero.
#[cfg(debug_assertions)]
pub fn reset_jit_invocations() {
    JIT_INVOCATIONS.with(|c| c.set(0));
}

#[cfg(debug_assertions)]
thread_local! {
    /// Per-thread log of fusion-bail tags (e.g. `node:Sample`,
    /// `call:json::read`), capped; harvested per fixture by `run!`.
    static FUSE_BAILS: std::cell::RefCell<Vec<arcstr::ArcStr>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Record a fusion-bail reason.
#[cfg(debug_assertions)]
pub fn record_fuse_bail(reason: arcstr::ArcStr) {
    FUSE_BAILS.with(|b| {
        let mut v = b.borrow_mut();
        if v.len() < 128 {
            v.push(reason);
        }
    });
}

/// Drain and return the current thread's recorded fusion-bail tags.
#[cfg(debug_assertions)]
pub fn take_fuse_bails() -> Vec<arcstr::ArcStr> {
    FUSE_BAILS.with(|b| std::mem::take(&mut *b.borrow_mut()))
}

/// Clear the current thread's fusion-bail log.
#[cfg(debug_assertions)]
pub fn reset_fuse_bails() {
    FUSE_BAILS.with(|b| b.borrow_mut().clear());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abort_peek_does_not_clear() {
        KERNEL_ABORT.with(|c| c.set(false));
        assert_eq!(graphix_abort_peek(), 0, "peek on cleared flag returns 0");
        KERNEL_ABORT.with(|c| c.set(true));
        assert_eq!(graphix_abort_peek(), 1, "peek on set flag returns 1");
        assert_eq!(graphix_abort_peek(), 1, "second peek still returns 1");
        assert!(KERNEL_ABORT.with(|c| c.get()), "flag remains set after multiple peeks");
        KERNEL_ABORT.with(|c| c.set(false));
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
        let decode =
            |r: DynCallRet| unsafe { crate::TagValue::from_raw(r.word0, r.word1) };
        let tv = decode(unsafe { graphix_fastcall(fp, ap, n, 0, 0) });
        assert_eq!(tv.tag(), crate::Tag::FIRED);
        assert_eq!(tv.value_cloned(), Value::I64(3));
        let tv = decode(unsafe { graphix_fastcall(fp, ap, n, 0, 0b1) });
        assert_eq!(tv.tag(), crate::Tag::STALE);
        assert_eq!(tv.value_cloned(), Value::I64(3));
        let never_p = never as *const () as u64;
        let tv = decode(unsafe { graphix_fastcall(never_p, ap, n, 0b1, 0) });
        assert_eq!(tv.tag(), crate::Tag::FRESH_BOTTOM);
        let tv = decode(unsafe { graphix_fastcall(never_p, ap, n, 0b1, 0b1) });
        assert_eq!(tv.tag(), crate::Tag::STALE_BOTTOM);
        let tv =
            decode(unsafe { graphix_fastcall(none as *const () as u64, ap, n, 0, 0) });
        assert_eq!(tv.tag(), crate::Tag::FRESH_BOTTOM);
        assert_eq!(args.len(), 1);
    }
}

/// Loan `env` (`KERNEL_ENV`) to the JIT'd code `f` runs; nested
/// invocations stack.
pub(crate) fn with_kernel_env<T>(env: &crate::env::Env, f: impl FnOnce() -> T) -> T {
    let prev = KERNEL_ENV.with(|c| c.replace(env as *const _));
    let r = f();
    KERNEL_ENV.with(|c| c.set(prev));
    r
}

/// Run `f` against a fresh `?` delivery queue and return what it raised,
/// in order; an enclosing invocation's queue is set aside and restored.
pub(crate) fn with_qop_raises<T>(
    f: impl FnOnce() -> T,
) -> (T, Vec<(*const crate::node::error::QopSite, Value)>) {
    let outer = QOP_RAISES.with(|q| std::mem::take(&mut *q.borrow_mut()));
    let r = f();
    let mine = QOP_RAISES.with(|q| std::mem::replace(&mut *q.borrow_mut(), outer));
    (r, mine)
}
