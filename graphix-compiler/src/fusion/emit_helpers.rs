#![allow(improper_ctypes_definitions)]
//! Stable `extern "C"` entry points the JIT calls into for ops that
//! can't be lowered purely in CLIF (composite reads/writes, layout-
//! sensitive operations).
//!
//! Value passing: `netidx_value::Value` is `#[repr(u64)]` with
//! explicit discriminant values and a fixed 16-byte layout
//! (`u64 disc`, `u64 payload`), so the SysV AMD64 ABI passes it in
//! two integer registers — same shape Cranelift sees when a helper
//! signature declares two `I64` params/returns. The
//! `improper_ctypes_definitions` lint flags Value because some of
//! its payload types (e.g. `PBytes`) are not themselves `repr(C)`,
//! but the OUTER layout is fully specified by `repr(u64)` and
//! stable; suppressed module-wide.
//!
//! Every helper is declared through [`jit_helpers!`] — the SINGLE
//! source of truth for its name, safety, Rust signature, body, and
//! (derived from the Rust types) its CLIF wire signature. `emit.rs`
//! consumes [`all_helpers`] to register each symbol by POINTER
//! (`JITBuilder::symbol`) and to build its cranelift `Signature`;
//! nothing resolves helpers by name, so there is no `no_mangle`.
//!
//! Safety contract: pointers (`arr`) must be valid for the duration
//! of the call — that part is the caller's (the kernel's) burden.
//! Element/field READS, however, are TOTAL: an out-of-bounds index or
//! an unexpected slot shape reads as the return shape's PLACEHOLDER
//! (0 / "" / the empty array / `Value::Null`) instead of trusting the
//! typechecker's shape proof. The proof is real for values built from
//! typed data, but #219 taint placeholders are `Value::Null` in
//! composite element slots by design — a destructure leaf read of a
//! placeholder element is REACHABLE from user programs (jul16a
//! crash_000003: `str::parse("42")?` as an `Array<(string, i64)>`
//! element SIGABRT'd `graphix_valarray_get_arcstr`), and the values
//! read on those paths are dead under the taint discipline, so any
//! well-formed value is correct. Totality here is the "helper-safe
//! placeholder" contract applied to the readers themselves.

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

/// Compile-time checks pinning the Value-ABI layout the JIT relies
/// on. If any of these fire on a netidx / upstream-crate upgrade,
/// the by-value Value plumbing would silently mis-pack the bits —
/// catch it at compile time instead.
///
/// Top check: the outer `Value` is exactly two machine words,
/// 8-byte aligned. This is the assumption the helpers' two-`I64`
/// CLIF signature and `Kernel::update`'s slot-pair pack/unpack
/// depend on.
///
/// Per-payload checks: each non-primitive variant payload must fit
/// in `Value`'s single 8-byte payload word. The Value-size check
/// above catches a regression transitively, but the per-payload
/// asserts pin each externally-defined type so a future upgrade
/// (a `Map` that grows past one word via niche-opt collapse loss,
/// a wider `ArcStr` rep, etc.) trips at the specific type rather
/// than via the indirect Value-size cascade.
///
/// `Bytes`, `Array`, `Abstract` are in-tree (`netidx-value`)
/// newtypes over thin-pointer types (`PArc` / `Arc`) and now carry
/// `#[repr(transparent)]`, so their size equals the inner pointer
/// and is guaranteed one word. `Map` and `ArcStr` come from
/// external crates without a layout guarantee in their public
/// contract — we assert here.
const _: () = {
    assert!(std::mem::size_of::<Value>() == 16);
    assert!(std::mem::align_of::<Value>() == 8);
    // Externally-defined payload types — assert one machine word.
    // `Map = immutable_chunkmap::map::Map<Value, Value, 32>` is an
    // enum that relies on Rust's niche optimization to collapse to
    // a thin pointer; if a future chunkmap release breaks that
    // assumption (adds a discriminant byte, etc.) this fails before
    // we silently mis-pack the second word of Value.
    assert!(
        std::mem::size_of::<netidx_value::Map>() <= 8,
        "netidx_value::Map must fit in Value's 8-byte payload word"
    );
    assert!(
        std::mem::size_of::<arcstr::ArcStr>() <= 8,
        "arcstr::ArcStr must fit in Value's 8-byte payload word"
    );
};

// ─── TagValue: the tagged Value at the JIT↔runtime boundary ──────────
//
// Promoted to `crate::tval` when it became the interpreter's value
// currency as well (design/replay_frames.md v2) — the tag byte is the
// same disc tag region the kernel uses, so the JIT↔interp seam is
// representation-identity. Re-imported here for the helpers; the
// layout checks above are what its transmutes rely on.
pub use crate::tval::TagValue;

// ─── The helper registry ─────────────────────────────────────────
//
// This is among the most safety-critical code in the runtime: a
// helper whose registered CLIF signature disagrees with its Rust
// definition is silent UB at the call boundary, not an error. The
// registry therefore has exactly ONE key — the [`jit_helpers!`]
// declaration — which emits the `extern "C"` item AND its
// [`HelperSpec`] (name, pointer, wire signature), the signature
// derived mechanically from the Rust parameter/return types via
// [`HelperArg`]/[`HelperRet`]. (The old scheme kept three
// hand-synchronized tables joined by string names: the definition, a
// name→pointer list, and a name→`Signature` string-match in
// `emit.rs`.)
//
// The C ABI requires integer arguments narrower than the register
// (i8/i16) to be zero-/sign-extended by the CALLER; cranelift emits
// the extension only when the `AbiParam` records it (an x86 `setcc`
// leaves the upper register bits dirty — a `false` comparison pushed
// into a composite once arrived as `true`). Here the extension is a
// property of the Rust TYPE, stated once: `i8`/`i16` sign-extend,
// `u8`/`u16` zero-extend, `i32`/`u32` need none (a 32-bit register
// write clears the upper bits).

/// One register slot of a helper's wire signature. `emit.rs`
/// translates these to cranelift `AbiParam`s, applying the u/s
/// extension flags to PARAMETERS only — returns are read at their
/// narrow type, per the C ABI.
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

/// The wire slots a Rust type occupies as a helper PARAMETER — one
/// entry per register. Every one-word type (integers, pointers, the
/// `ArcStr` / ValArray-bits words) is a single slot; [`TagValue`] and
/// [`DynCallRet`] are the two-register `(disc, payload)` Value pair.
/// A type without an impl cannot appear in a helper signature — a
/// compile error, forcing a conscious mapping decision.
pub(crate) trait HelperArg {
    const ABI: &'static [AbiTy];
}

/// The wire slots of a helper RETURN. Blanket-follows [`HelperArg`];
/// `()` returns nothing.
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

/// One registered helper: the symbol name, its address, and its wire
/// signature (per-Rust-parameter slot lists + return slots). Built by
/// [`jit_helpers!`]; consumed by `emit.rs` for symbol registration
/// and `Signature` construction.
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
/// ordinary `fn` (attributes and doc comments attach to the emitted
/// item). Emits the `pub [unsafe] extern "C"` definitions plus a
/// `$registry()` function pushing one [`HelperSpec`] per helper —
/// name, pointer, and the type-derived wire signature. The
/// per-section registries are concatenated by [`all_helpers`].
///
/// A tt-muncher: items are parsed one at a time (the explicit
/// `safe`/`unsafe` leading keyword keeps the grammar decidable after
/// an attribute list), each emitting its item immediately while its
/// registry facts accumulate into the bracketed list the terminal
/// rule turns into the registry fn.
macro_rules! jit_helpers {
    // Terminal: emit the registry fn from the accumulated facts.
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

/// Every registered helper — the single registry `emit.rs` builds
/// symbol registrations and CLIF signatures from.
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

// ─── The ValArray bits currency ──────────────────────────────────
//
// A composite (array/tuple/struct) travels through the JIT as ONE
// machine word: the `ValArray` bits — `#[repr(transparent)]` over a
// thin Arc, byte-identical to `Value::Array`'s payload word (the
// unified Value ABI, design/unified_value_abi.md). There is no box.
// Signatures use `u64` for the word (the `graphix_arcstr_drop`
// pattern): `ValArray` has a NonNull niche, so a typed parameter
// holding the 0 pending sentinel would be UB at the boundary —
// helpers assert non-zero and transmute internally. Ownership is a
// call-site convention exactly as the old box was: owned bits drop
// exactly once (`graphix_valarray_drop`) or transfer into a
// consuming helper; borrowed bits are the same word un-bumped and
// must not be dropped.

/// Borrow ValArray bits for the duration of a call. Zero is always a
/// codegen bug (the pending sentinel leaked into a read).
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

/// The bits of a BORROWED `&ValArray` — same word, no refcount bump.
/// The caller must not drop them.
#[inline]
fn va_borrowed_bits(a: &ValArray) -> u64 {
    unsafe { *(a as *const ValArray as *const u64) }
}

// ─── Producer-op builder ─────────────────────────────────────────
//
// Producer ops (TupleNew, StructNew, VariantNew, ArrayInit, etc.)
// build a `Vec<Value>` field-by-field then finalize into a
// `ValArray`. The JIT emits this as:
//
//   buf  = call graphix_value_buf_new(cap)
//   call graphix_value_buf_push_<T>(buf, field0_value)
//   ...
//   arr  = call graphix_valarray_finalize(buf)  // consumes buf
//
// The buf is an `LPooled<Vec<Value>>` so re-allocation is amortized
// across calls — same shape the interp uses for the same ops.
//
// All pointers passed in/out are owned (Box::into_raw / Box::from_raw
// transfers ownership across the FFI boundary). The CLIF lowering
// must drop or transfer each owned pointer exactly once.

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

/// Push a bool. JIT passes `1` for true / `0` for false (CLIF Bool
/// is `I8`); we treat any nonzero as true.
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

/// Push a `Value::Array(inner)` slot, taking ownership of `inner`
/// (owned ValArray bits). Used by StructNew (each field is a
/// `[name, value]` inner array) and by VariantNew with payload.
unsafe fn graphix_value_buf_push_array(buf: *mut LPooled<Vec<Value>>, inner: u64) {
    unsafe { (*buf).push(Value::Array(va_owned(inner))) }
}

/// Flatten an owned ValArray (bits) into `buf`: clone each element
/// into the buf, then drop the array. Used by the flat_map loop's
/// JIT codegen — the body produces an owned array per element whose
/// contents are concatenated into the output. (`ValArray` is an
/// immutable `Arc<[Value]>`, so elements are cloned, not moved.)
unsafe fn graphix_value_buf_extend_from_array(buf: *mut LPooled<Vec<Value>>, inner: u64) {
    unsafe {
        let owned = va_owned(inner);
        (*buf).extend(owned.iter().cloned());
    }
}

/// Borrow-mode array push: refcount-bumps the caller's borrowed bits
/// and pushes `Value::Array(clone)`. Used for DynCall composite args
/// where the caller still owns its local and the dispatcher needs a
/// separately-tracked clone.
unsafe fn graphix_value_buf_push_array_borrowed(buf: *mut LPooled<Vec<Value>>, src: u64) {
    unsafe { (*buf).push(Value::Array(va_ref(&src).clone())) }
}

/// Borrow-mode value push: the caller passes their `Value` bits by
/// value (Value is `#[repr(u64)]`, 16 bytes — two integer registers
/// on the SysV ABI). We refcount-bump the inner, push the clone, and
/// `mem::forget` the input so the caller's bits stay valid (the
/// caller's local retains its ref).
safe fn graphix_value_buf_push_value_borrowed(buf: *mut LPooled<Vec<Value>>, v: TagValue) {
    // Refcount-bump the MASKED value, push the clean clone, forget the
    // input (the caller's local keeps its ref). A tainted disc can't
    // reach the buffer.
    let dup = v.with_value(|v| v.clone());
    std::mem::forget(v);
    unsafe { (*buf).push(dup) }
}

/// Move-mode value push: consumes `v` (the caller transfers
/// ownership) and pushes it into the buf without an extra refcount
/// bump. Used for DynCall variant args sourced from an owned producer
/// (VariantNew, composite-return DynCall result) — the value isn't
/// referenced anywhere else, so a borrow-mode push would leak the
/// extra ref.
safe fn graphix_value_buf_push_value(buf: *mut LPooled<Vec<Value>>, tv: TagValue) {
    // Strip the tag — the buffer holds clean Values (the builtin that
    // consumes it must never see a tagged disc). A tainted field is the
    // JIT-side force's job to bottom; masking here is the net that turns
    // a forgotten force from UB into a (fuzzer-caught) value divergence.
    unsafe { (*buf).push(tv.value()) }
}

/// Drop a partially-built (still-`Box`'d) `LPooled<Vec<Value>>`.
/// Used by the JIT cleanup_stack on pending paths: a producer op
/// that allocated a buf but never reached `finalize` needs to free
/// it explicitly. Null is always a codegen bug (a pending sentinel
/// leaked into a drop) — panic loudly instead of UB.
unsafe fn graphix_value_buf_drop(buf: *mut LPooled<Vec<Value>>) {
    assert!(!buf.is_null(), "graphix_value_buf_drop: null buf — JIT codegen bug");
    unsafe { drop(Box::from_raw(buf)) }
}

/// Push a `Value::String` slot cloned from an interned static: `ptr`
/// is a stable `*const ArcStr` (a kernel strings-table slot — struct
/// field names, variant tags); the clone bumps the refcount and the
/// caller's table entry stays owned.
unsafe fn graphix_value_buf_push_arcstr(
    buf: *mut LPooled<Vec<Value>>,
    ptr: *const arcstr::ArcStr,
) {
    unsafe { (*buf).push(Value::String((*ptr).clone())) }
}

/// Push an owned `ArcStr` onto the dyncall arg buffer, wrapping it in
/// `Value::String`. Used for `Type::String` DynCall args — the
/// caller's SSA holds an owned ArcStr (bit-equivalent to its raw
/// thin pointer) and transfers ownership into the buf.
unsafe fn graphix_value_buf_push_string(buf: *mut LPooled<Vec<Value>>, s: arcstr::ArcStr) {
    unsafe { (*buf).push(Value::String(s)) }
}

/// Finalize the buffer into an owned `ValArray`, returned as its
/// bits. Consumes the buf (drops the `LPooled<Vec<Value>>` so its
/// allocation returns to the pool).
unsafe fn graphix_valarray_finalize(buf: *mut LPooled<Vec<Value>>) -> u64 {
    unsafe {
        let mut owned = *Box::from_raw(buf);
        va_bits(ValArray::from_iter_exact(owned.drain(..)))
    }
}

/// Reference-count bump of borrowed ValArray bits → owned bits. Used
/// at kernel entry and by `ensure_owned_composite_src` to convert a
/// borrowed composite read into an owned local — one relaxed atomic
/// increment, no allocation.
safe fn graphix_valarray_clone(bits: u64) -> u64 {
    va_bits(va_ref(&bits).clone())
}

/// Drop owned ValArray bits. Use when a local goes out of scope or is
/// overwritten by a tail-call rebind. Zero is always a codegen bug (a
/// pending sentinel leaked into a drop) — panic loudly instead of UB.
safe fn graphix_valarray_drop(bits: u64) {
    drop(va_owned(bits))
}

/// flat_map extend for LIST-valued callback results: walk the cons
/// chain and push each element. A NON-list value pushes as a single
/// element — `ListFlatMap::finish`'s fallback, bit-for-bit. Consumes
/// the value (the loop wraps the body result owned).
unsafe fn graphix_value_buf_extend_from_list(
    buf: *mut LPooled<Vec<Value>>,
    tv: TagValue,
) {
    use crate::node::collection::list;
    let v = tv.value(); // consume; masks the tag bits
    let buf = unsafe { &mut *buf };
    if list::is_list(&v) {
        buf.extend(list::Iter::new(v));
    } else {
        buf.push(v);
    }
}

}

// ─── Control: pending / interrupt / call-depth / dispatch ────────

jit_helpers! { registry = control_helpers;

/// Read the `DYNCALL_PENDING` thread-local *without clearing it*.
/// JIT-emitted code calls this after every cross-kernel lambda call:
/// a set flag means the CALLEE genuinely aborted (interrupt, depth
/// trip) and returned the pending sentinel, so the caller drops its
/// owned set and jumps to its own `pending_exit`. The flag stays set
/// so that `Kernel::update`'s wrapper-level check sees it and returns
/// `None`. `Kernel::update` resets the flag to `false` at the top of
/// every kernel invocation, so a stale `true` from a previous run
/// never leaks across.
///
/// Returns 1 if pending was set, 0 otherwise.
safe fn graphix_dyncall_pending_take() -> u8 {
    DYNCALL_PENDING.with(|c| if c.get() { 1 } else { 0 })
}

/// Read AND clear the `DYNCALL_PENDING` thread-local. JIT-emitted
/// code calls this immediately after every builtin `graphix_dyncall`:
/// a set flag means THIS dispatch returned no value ("bottom this
/// cycle" — e.g. `buffer::encode`'s Pad guard), which the site
/// converts to a #219 tainted placeholder that CONTINUES — bottoming
/// only the consumers that read it, like the node-walk, where a
/// builtin's `None` silences only its downstream. Clearing is what
/// keeps the pending flag meaning "genuine whole-kernel abort"
/// (interrupt, the return-gate force) for
/// `Kernel::update`'s wrapper check.
///
/// Returns 1 if pending was set, 0 otherwise.
safe fn graphix_dyncall_pending_take_clear() -> u8 {
    DYNCALL_PENDING.with(|c| if c.replace(false) { 1 } else { 0 })
}

/// Set `DYNCALL_PENDING` to true. Called by the JIT-emitted code
/// at the qop-unwrap error branch — same pending signal as
/// the dispatcher uses, just driven by a kernel-internal check
/// instead of a `dispatch` return.
/// The dyncall return-shape mismatch path was SILENT: the emitted
/// check drops the wrong-shaped Value and substitutes a tainted
/// placeholder, so a stdlib builtin whose eval violates its declared
/// return type (sprintf-error-return-shape-aug2026) — or a genuine
/// compiler bug — silently lost a value. Called on that branch to
/// make the loss attributable.
safe fn graphix_shape_mismatch_warn(got_disc: u64) {
    log::warn!(
        "fused call returned a Value whose shape (disc {got_disc:#x}) doesn't \
         match its declared return type — dropped as bottom (a stdlib builtin \
         violating its declared type, or a compiler bug)"
    );
}

safe fn graphix_dyncall_set_pending() {
    DYNCALL_PENDING.with(|c| c.set(true))
}

/// Read the active runtime's interrupt/abort control (set in
/// `INTERRUPT_PTR` by `do_cycle`). Returns 1 if a wedged loop should
/// abort (an `interrupt()` or `abort()` is pending), else 0. Emitted at
/// every JIT loop head; on 1 the kernel jumps to its pending-exit (drops
/// in-flight buffers, returns the sentinel) so `Kernel::update` yields
/// `None`.
safe fn graphix_interrupted() -> i8 {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if p.is_null() {
            0
        } else {
            // SAFETY: `do_cycle` sets `p` to its `ExecCtx.control`, which
            // outlives the cycle; null when no cycle is running.
            i8::from(unsafe { (*p).interrupted() })
        }
    })
}

/// The kernel twin of `stack::ensure_sufficient` — depth is bounded
/// by memory, not a counter (`design/recursive_activations.md` §4b):
/// at every native self-call site the kernel asks whether the
/// remaining stack is inside the red zone. 0 = interrupted (skip the
/// dispatch), 1 = call directly, 2 = re-enter the callee on a fresh
/// segment through [`graphix_grow_stack`]. Same red zone and segment
/// as the node-walk's guard, so a derivation that interleaves the two
/// grows the same way.
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

/// Run a kernel's spill thunk on a fresh stack segment. `thunk` is the
/// address of the kernel's `__spill` entry (`jit::define_spill_thunk`),
/// `args` the caller's spilled parameter words (one 8-byte slot per
/// CLIF param, in signature order), `out` two words the thunk fills
/// with the (disc, payload) result.
safe fn graphix_grow_stack(thunk: i64, args: i64, out: i64) {
    // SAFETY: `thunk` is a JIT function of the fixed spill signature
    // this crate emits; `args`/`out` are stack slots of the calling
    // kernel, live for the duration of the call.
    let f: extern "C" fn(i64, i64) = unsafe { std::mem::transmute(thunk as usize) };
    crate::stack::grow(|| f(args, out))
}

/// The single registered DynCall entry point. Indirects through
/// the thread-local handle to the monomorphized dispatcher and
/// returns the inner production as a two-word [`DynCallRet`] pair —
/// the unified Value ABI with the production's TAINT/STALE tag riding
/// the disc's high byte in-band (Seam B of the 5c flip; the call site
/// adapts per its static type and never adopts a bottom's payload).
///
/// On a genuine dispatch abort (instance init failure): returns
/// `(0, 0)` and sets `DYNCALL_PENDING`; JIT-emitted code
/// take-and-clears it after the dyncall and takes the placeholder
/// path.
///
/// `taint_mask`: bit `i` set = arg slot `i` is BOTTOM (the arg
/// produced no usable value). The dispatcher delivers those slots as
/// the honest bottom (FreshBottom / StaleBottom by the stale bit),
/// and the wrapper's Q1 arm bottoms the invocation without calling
/// eval — bottom propagates, authors never see it
/// (design/dense_delivery.md; the pre-dense absence delivery and its
/// ride-own-history semantics are the re-blessed
/// dyncall-partial-args delta). The buf still carries a placeholder
/// Value at that position (arity is fixed); it drops with the buf.
/// Lambda-callee dispatch sites pass 0 (formals poison via their own
/// protocol, unchanged).
///
/// `stale_mask`: bit `i` set = arg slot `i` is present but did NOT
/// fire this cycle (its disc carries STALE). The dispatcher delivers
/// those slots as `TagValue::stale`, so a builtin whose production
/// gates on argument FIRING (`printfn!`'s per-arg update, `str::
/// escape`'s `seam_arg` fired flags, `CachedArgs`' eval re-run) sees the
/// per-argument truth instead of a phantom fire per kernel
/// invocation (dyncall-stale-arg-fired-aug2026: `rand` re-randomized
/// and `now` resampled the clock on every invocation).
///
/// `site_word`: address of the emission site's claimed SITE-IDENTITY
/// word (dyncall-site-identity-jul2026 — the dispatcher mints an id
/// into it and keys a per-site inner Apply on the value, so one
/// static dyncall instruction reached from several logical call
/// sites gets per-site cache and state, matching the node-walk's
/// per-callsite instantiation). 0 = no identity (v1 scaffold-loop
/// sites, recursive back-edges, qop-deliver): the shared key-0
/// bucket, the pre-identity behavior.
unsafe fn graphix_dyncall(
    fn_index: u32,
    args: *mut LPooled<Vec<Value>>,
    taint_mask: u64,
    stale_mask: u64,
    site_word: *mut u64,
) -> DynCallRet {
    let handle = DYN_DISPATCH_HANDLE.with(|c| c.get());
    if handle.is_null() {
        panic!(
            "graphix_dyncall: no DynDispatchHandle set — Kernel::update \
             must populate the thread-local before invoking JIT'd code \
             that calls HOFs"
        );
    }
    unsafe {
        let h = &*handle;
        (h.dispatch)(h.state, fn_index, args, taint_mask, stale_mask, site_word)
    }
}

/// The FASTCALL path (`BuiltIn::FASTCALL`): a stateless sync builtin
/// called directly — no site identity, no per-site inner Apply, no
/// `CachedArgs` memo. The kernel's argument discs decide the tag: any
/// tainted argument bottoms the result WITHOUT calling (bottom
/// propagates — the fn never sees a bottom), all-stale arguments make
/// the result STALE, and `None` from the fn is this cycle's bottom.
/// `fn_ptr` is the registered `FastFn`; `args`/`n` is the call site's
/// stack buffer of (disc, payload) pairs, borrowed. Returns the production's
/// two words exactly as `graphix_dyncall` does (tag in-band on the
/// disc), so the call site's decode is shared; never touches the
/// pending flag.
unsafe fn graphix_fastcall(
    fn_ptr: u64,
    args: u64,
    n: u64,
    taint_mask: u64,
    stale_mask: u64,
) -> DynCallRet {
    // SAFETY: `args` is the call site's stack buffer of `n` (disc,
    // payload) pairs, each a valid clean `Value` the site built for
    // this call (scalars with their variant's discriminant, composite
    // and string words borrowed, value shapes with a cleaned disc).
    // Viewed, never owned: nothing here drops them; the site releases
    // what it owned after the call.
    let args_vec: &[Value] =
        unsafe { std::slice::from_raw_parts(args as *const Value, n as usize) };
    let n = n as usize;
    let all_stale = n > 0 && stale_mask == u64::MAX >> (64 - n);
    let bottom = if all_stale { crate::Tag::STALE_BOTTOM } else { crate::Tag::FRESH_BOTTOM };
    let tv = if taint_mask != 0 {
        crate::TagValue::tagged(Value::Null, bottom)
    } else {
        // SAFETY: `fn_ptr` is the `FastFn` the builtin registered
        // (`BuiltinFacts::fastcall`), embedded by the emitter as an
        // immediate; a fn pointer round-trips through usize.
        let f: crate::FastFn = unsafe { std::mem::transmute::<usize, crate::FastFn>(fn_ptr as usize) };
        match f(args_vec) {
            Some(v) => crate::TagValue::tagged(
                v,
                if all_stale { crate::Tag::STALE } else { crate::Tag::FIRED },
            ),
            None => crate::TagValue::tagged(Value::Null, bottom),
        }
    };
    if crate::dbgenv::gxdbg_dync() {
        eprintln!(
            "FASTCALL fn={fn_ptr:x} n={n} taint={taint_mask:b} stale={stale_mask:b} -> {:?}",
            tv.tag()
        );
    }
    // SAFETY: as in `dispatch_typed` — TagValue is the (disc, payload)
    // pair; the bits transfer to the caller and the local's Drop is
    // suppressed.
    let tv = std::mem::ManuallyDrop::new(tv);
    let words: [u64; 2] = unsafe { std::mem::transmute_copy(&*tv) };
    DynCallRet { word0: words[0], word1: words[1] }
}

/// Write a reactive variable from inside a JIT'd kernel — the fused
/// form of `connect` (`x <- expr`) and a handler-ful `?`'s error
/// delivery. Indirects through the per-call dispatch handle to a
/// monomorphized `set_var_typed::<R, E>` (which reaches `ctx`). A `disc`
/// that is `#219`-tainted (no value) OR STALE (did not fire this cycle)
/// means the RHS did not fire with a value — the write is skipped,
/// mirroring the node-walk's `if let Some(v) = ..` guard. Does NOT set
/// the pending flag: a variable write is a side effect, not a reason to
/// abort the kernel.
unsafe fn graphix_set_var(bind_id: u64, disc: u64, payload: u64) {
    let handle = DYN_DISPATCH_HANDLE.with(|c| c.get());
    if handle.is_null() {
        panic!(
            "graphix_set_var: no DynDispatchHandle set — Kernel::update \
             must populate the thread-local before invoking JIT'd code \
             that writes variables"
        );
    }
    unsafe {
        let h = &*handle;
        (h.set_var)(h.state, bind_id, disc, payload)
    }
}

}

// ─── Value-as-aggregate helpers ────────────────────────────────────
//
// Value is `#[repr(u64)]`, 16 bytes — a `(u64 discriminant, u64
// payload)` aggregate that the SysV AMD64 ABI passes in two integer
// registers (RDI/RSI for args, RAX/RDX for return). All Value-shaped
// helpers below take/return `Value` directly via the two-register
// `extern "C"` ABI — no `*mut Value` boxing, no separate heap
// allocation for the outer Value (the inner ArcStr/ValArray/etc. is
// already heap-allocated by its own refcount).
//
// Ownership convention: helpers that READ (`is_null`, `tag_eq`,
// `payload_*`, the `_borrowed` push) `mem::forget` the input so the
// caller's bits stay valid — the caller's local retains its ref.
// Only `graphix_value_drop` actually consumes (`drop`s) the input.
// This lets the JIT pass `b.use_var(disc), b.use_var(payload)`
// repeatedly without worrying about ownership: every call site is
// either an explicit drop, or implicitly borrowed.

/// The shared value-arith core: compute through netidx's Value
/// operators (byte-identical to the non-fused arith node), with an
/// Error result converted to BOTTOM for these UNCHECKED ops — the
/// node-walk's BinOp converts `Value::Error` to log+None
/// (node/op.rs), so return a tainted Null, never the Error as a
/// value (soak finding corpus-fuzz/divergence_000018:
/// `duration:1.s / i64:0`). The checked `+?` family keeps its
/// catchable ArithError. Consumes both operands (netidx's operators
/// take them by value).
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

/// The shared variant-payload read: slot 0 is the tag; payloads start
/// at slot 1, so the JIT passes the 0-based payload position and we
/// add 1. Borrowed read — the input is `mem::forget`ed so the caller
/// retains ownership. Total: a placeholder (`Value::Null`) or short
/// array reads as the default (see the module's totality contract).
fn variant_payload_read<T: Default>(
    v: TagValue,
    payload_idx: usize,
    read: impl Fn(&Value) -> T,
) -> T {
    let r = v.with_value(|v| match v {
        Value::Array(a) => a.get(payload_idx + 1).map(&read).unwrap_or_default(),
        _ => T::default(),
    });
    std::mem::forget(v); // borrowed read — caller keeps owning it
    r
}

jit_helpers! { registry = value_helpers;

/// Unwrap an owned `Value::Array` into owned ValArray bits — the
/// CHECKED value→composite narrowing. Under the unified Value ABI the
/// bits ARE `Value::Array`'s payload word, but the check must stay: a
/// tainted `Value::Null` placeholder is reachable on `?`-unwrap paths
/// and reinterpreting its zero payload as array bits would be UB at
/// the first touch. A non-Array here is a CODEGEN bug (the emit's
/// taint/shape gates failed) — abort defined rather than UB;
/// extern "C" makes the panic a nounwind abort.
safe fn graphix_value_into_array(v: TagValue) -> u64 {
    match v.value() {
        Value::Array(a) => va_bits(a),
        v => panic!("graphix_value_into_array: expected Value::Array, got {v:?}"),
    }
}

/// Borrowed-read variant of [`graphix_value_into_array`]: clones the
/// inner ValArray (refcount bump — owned bits out) and forgets the
/// input so the caller's bits stay valid.
safe fn graphix_value_into_array_borrowed(v: TagValue) -> u64 {
    let bits = v.with_value(|v| match v {
        Value::Array(a) => va_bits(a.clone()),
        v => {
            panic!("graphix_value_into_array_borrowed: expected Value::Array, got {v:?}")
        }
    });
    std::mem::forget(v); // borrowed read — the caller keeps owning it
    bits
}

/// Consume a Value and decrement the inner refcount (for
/// String/Array/Variant/etc. — a no-op for scalar variants like
/// I64/Bool/Null).  Use at scope exit for owned Value locals.
///
/// The all-zero pending sentinel is REJECTED before an invalid
/// `Value` materializes: `Value`'s discriminants are bitmasks
/// starting at 0x1, so disc 0 is never a real value. A zero disc
/// here is always a codegen bug (a pending sentinel leaked into a
/// drop); the panic aborts at the `extern "C"` boundary with the
/// message printed, instead of UB.
safe fn graphix_value_drop(tv: TagValue) {
    assert!(
        !tv.is_sentinel(),
        "graphix_value_drop: zero discriminant — JIT codegen bug \
         (a pending sentinel leaked into a drop)"
    );
    // `tv` drops here: TagValue::Drop masks the tag and drops the clean
    // `Value`, so a tagged (tainted) disc can't corrupt the drop.
    drop(tv)
}

/// "Borrowed clone": bump the inner refcount and return a fresh
/// `Value` with valid ownership. The caller's bits stay valid — we
/// `mem::forget` the input so its ref isn't decremented. Net effect:
/// caller now has two valid refs (original + returned clone) and the
/// inner refcount has incremented by one.
safe fn graphix_value_clone(tv: TagValue) -> TagValue {
    // Refcount-bump (preserving the tag); the input stays owned by the
    // caller (forget, don't run our Drop which would decrement).
    let dup = tv.clone();
    std::mem::forget(tv);
    dup
}

/// Clone a `Value` from a stable `*const Value` static — a kernel's
/// value-constants table slot (a value-shape constant:
/// datetime/duration/bytes/map). Bumps any inner `Arc`. Returns the
/// clone by value (two registers).
///
/// # Safety
/// `ptr` must point to a live `Value` that outlives the JIT'd code.
/// The per-kernel `KernelValues` table owns it (like `KernelStrings`
/// for `ArcStr`), kept alive on the `CachedKernel`.
unsafe fn graphix_value_clone_from_static(ptr: *const Value) -> TagValue {
    TagValue::clean(unsafe { (*ptr).clone() })
}

/// Box `tv` as a value of the Graphix-minted abstract type `id` —
/// the constructor `T(v)` (`design/nominal_abstract_types.md`).
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

// Value arithmetic (datetime/duration `ValueArith`) — see
// [`value_arith_op`]. Codegen passes OWNED Values
// (`ensure_owned_value` clones a Borrowed Local read, scalar operands
// are freshly promoted, producer ops like a value-shape `Const` are
// already owned), so there's no leak or double-free.

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

// Checked arithmetic (`+?`/`-?`/`*?`/`/?`/`%?`) — netidx's `checked_*`
// inherent methods, with any raw error wrapped into the catchable
// `ArithError` error VALUE through the SAME [`wrap_arith_error`] core
// the node-walk's checked update uses (never bottom, unlike unchecked
// div0). Each helper CONSUMES both args, same contract as the
// unchecked family above.

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

/// Value equality (`ValueEq`). Compares via netidx's
/// `impl PartialEq for Value` — byte-identical to the non-fused `==`
/// node. CONSUMES both args (they're dropped at function end), matching
/// the owned-operand contract `compile_owned_value_operand` produces.
safe fn graphix_value_eq(l: TagValue, r: TagValue) -> u8 {
    (l.value() == r.value()) as u8
}

/// `bytes[i]` indexing. Extracts the `PBytes` from a
/// `Value::Bytes`, indexes via the shared `node::array::bytes_index`
/// (bounds-checked, negative-from-end), and returns `Nullable<u8>`'s
/// `Value` (the `u8` or the out-of-bounds error). CONSUMES the bytes
/// Value (passed owned by `compile_owned_value_operand`).
safe fn graphix_bytes_index(v: TagValue, i: i64) -> TagValue {
    TagValue::clean(match v.value() {
        Value::Bytes(b) => bytes_index(&b, i),
        _ => Value::error("ArrayIndexError: expected bytes"),
    })
}

/// Map access `m{key}`. Looks up `key` in the
/// `Value::Map` via the shared `node::map::map_get`, returning the
/// value or the `map key not found` error (`Nullable<V>`'s `Value`).
/// CONSUMES both operands (passed owned by
/// `compile_owned_value_operand`).
safe fn graphix_map_ref(map: TagValue, key: TagValue) -> TagValue {
    TagValue::clean(map_get(&map.value(), &key.value()))
}

/// Array/bytes slice `a[i..j]`. `flags` bit0 =
/// `start` present, bit1 = `end` present (absent bounds pass 0). Routes
/// to the shared `node::array::array_slice_i64`, returning the
/// sub-array/sub-bytes or an error (`Nullable<source>`'s `Value`).
/// CONSUMES the source Value (passed owned by
/// `compile_owned_value_operand`).
safe fn graphix_array_slice(src: TagValue, start: i64, end: i64, flags: i64) -> TagValue {
    let s = if flags & 1 != 0 { Some(start) } else { None };
    let e = if flags & 2 != 0 { Some(end) } else { None };
    TagValue::clean(array_slice_i64(&src.value(), s, e))
}

/// Test whether a `Value` is `Value::Null`. Borrowed read — caller
/// retains ownership.
///
/// Today is-null lowering inlines this test as `icmp_imm
/// (disc, NULL_DISC)` rather than calling the helper; the helper
/// remains registered so out-of-tree code and direct interp tests
/// keep working.
safe fn graphix_value_is_null(v: TagValue) -> u8 {
    let r = v.with_value(|v| matches!(v, Value::Null) as u8);
    std::mem::forget(v); // borrowed read — caller keeps owning it
    r
}

// Variant consumer ops: variants at runtime are either
// `Value::String(tag)` for nullary or `Value::Array([tag, p0, ...])`
// for with-payload; the JIT dispatches on the outer Value shape via
// these helpers.

/// Test whether a variant's runtime tag matches `expected`. Returns
/// 1 (true) or 0 (false). Takes the variant `Value` by value (16
/// bytes / two registers); we `mem::forget` after the borrowed read
/// so the caller's bits stay valid — variant locals are dropped at
/// scope exit via the dedicated drop helper.
safe fn graphix_variant_tag_eq(
    v: TagValue,
    expected: *const arcstr::ArcStr,
    arity: usize,
) -> u8 {
    // Mirrors `StructurePattern::is_match` (node/pattern.rs): the
    // pattern's ARITY selects the representation — a 0-payload variant
    // is `Value::String(tag)`, an n-payload one is `Value::Array` of
    // n + 1 with the tag at slot 0. Tag alone is NOT a discriminator:
    // `[`A, `A(i64)]` is a legal union whose arms differ only by arity
    // (variant-arity-tag-only-aug2026).
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
    std::mem::forget(v); // borrowed read — caller keeps owning it
    r
}

// Variant payload reads — see [`variant_payload_read`].

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

// ─── String / ArcStr helpers ──────────────────────────────────────
//
// `ArcStr` is `repr(transparent)` over a thin pointer, so it travels
// across the JIT/Rust boundary as a single 8-byte value. Codegen
// treats `Type::String` SSA values as `i64` CLIF values holding
// the ArcStr's raw pointer. Lifetime tracking matches the variant /
// nullable scheme — every owned ArcStr SSA either feeds a consumer
// helper that takes ownership (e.g. `graphix_string_buf_push_arcstr`
// drops on push) or is returned across the kernel boundary (the
// wrapper hands it to `Kernel::update` which wraps it into a
// `Value::String`). On the pending path the in-flight string buf
// (still owned by the kernel) drops via `graphix_string_buf_drop`.

/// The shared Display-render core of the `string_buf_push_<prim>`
/// family — matches `Value::<T>(v).to_string()` for every primitive
/// (netidx `Value`'s Display delegates to the inner type's Display).
fn push_display<T: std::fmt::Display>(buf: *mut String, v: T) {
    use std::fmt::Write;
    let _ = write!(unsafe { &mut *buf }, "{v}");
}

jit_helpers! { registry = string_helpers;

/// Clone an interned static `ArcStr` — refcount bump on the slot at
/// `p`, returning a fresh owned ArcStr. Caller has ownership; drops
/// when no longer needed via `graphix_arcstr_drop`. Used by
/// string-constant lowering.
unsafe fn graphix_arcstr_clone_from_static(p: *const arcstr::ArcStr) -> arcstr::ArcStr {
    unsafe { (*p).clone() }
}

/// Drop an owned ArcStr. Refcount decrement; frees the underlying
/// buffer when the last clone goes away.
///
/// Takes the raw pointer bits rather than `ArcStr` itself
/// (bit-identical ABI — `repr(transparent)` over a pointer) so the
/// zero pending sentinel can be REJECTED before an invalid `ArcStr`
/// materializes (zero violates its `NonNull` niche — a typed
/// parameter holding it would be UB at the boundary). Zero here is
/// always a codegen bug (a pending sentinel leaked into a drop);
/// the panic aborts at the `extern "C"` boundary with the message
/// printed, instead of UB. This was #214's crash site —
/// `drop_in_place(NULL)` SIGSEGV.
safe fn graphix_arcstr_drop(s: u64) {
    assert!(
        s != 0,
        "graphix_arcstr_drop: null ArcStr — JIT codegen bug \
         (a pending sentinel leaked into a drop)"
    );
    // SAFETY: nonzero, and JIT'd code only ever passes bits it
    // received from an ArcStr-producing helper (same decode as
    // `Kernel::update`'s String return path).
    drop(unsafe { std::mem::transmute::<u64, arcstr::ArcStr>(s) })
}

/// Borrowed-clone an ArcStr by value: bump the refcount, return the
/// fresh clone. The caller's bits stay valid (we `mem::forget` so
/// the input's ref isn't decremented). Net effect: caller now has
/// two valid refs (original + returned clone). Used by
/// local reads of String slots and by anywhere else we
/// need to take an additional ref to an in-register ArcStr.
safe fn graphix_arcstr_clone(s: arcstr::ArcStr) -> arcstr::ArcStr {
    let dup = s.clone();
    std::mem::forget(s);
    dup
}

/// The empty-`ArcStr` placeholder for a tainted String position — the
/// static empty (clone/drop are no-ops on it), returned as the raw
/// thin-pointer bits the String ABI uses. Helper-safe by construction.
safe fn graphix_arcstr_empty() -> u64 {
    unsafe { std::mem::transmute::<arcstr::ArcStr, u64>(arcstr::ArcStr::new()) }
}

/// The empty-`ValArray` placeholder for a tainted composite position —
/// an owned refcount clone of the static empty (the consumer chain
/// drops it through the normal scope machinery; the static never
/// reaches zero). No allocation.
safe fn graphix_valarray_empty() -> u64 {
    va_bits(EMPTY_ARR.clone())
}

/// Start a fresh string-buffer for interpolation/concat. Returns a
/// heap-owned `*mut String`; caller eventually pairs with
/// `graphix_string_buf_finalize` (success) or
/// `graphix_string_buf_drop` (pending path).
safe fn graphix_string_buf_new() -> *mut String {
    Box::into_raw(Box::new(String::new()))
}

/// Drop a string buf without finalizing — used on pending paths
/// when a DynCall short-circuits an in-flight Concat. Null is
/// always a codegen bug (a pending sentinel leaked into a drop) —
/// panic loudly instead of UB.
unsafe fn graphix_string_buf_drop(buf: *mut String) {
    assert!(!buf.is_null(), "graphix_string_buf_drop: null buf — JIT codegen bug");
    drop(unsafe { Box::from_raw(buf) })
}

/// Finalize a string buf into an owned ArcStr. Consumes the buf
/// (frees the Box) and returns the resulting ArcStr.
unsafe fn graphix_string_buf_finalize(buf: *mut String) -> arcstr::ArcStr {
    let s = unsafe { *Box::from_raw(buf) };
    arcstr::ArcStr::from(s.as_str())
}

/// Append an ArcStr's contents to the buf, consuming the ArcStr.
unsafe fn graphix_string_buf_push_arcstr(buf: *mut String, s: arcstr::ArcStr) {
    unsafe { &mut *buf }.push_str(&s);
    // s drops here.
}

// Per-prim Display renders — see [`push_display`].

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

// ─── List / Map collection HOF boundary ───────────────────────────
//
// The scaffold loops iterate a ValArray; List (a Cons/Nil chain) and
// Map (a CMap) sources cross this seam by FLATTENING on entry and
// REBUILDING on exit, through the same canonical functions the
// interpreted MapQ/FoldQ use (`node::collection::list`,
// `make_pair`/`split_pair`) — one semantic seam, so the two
// evaluators agree bit-for-bit. Semantically `list::map(l, f)`
// lowers as `from_array(array::map(to_array(l), f))`, and the
// SlotFlags firing rule over the flattened length IS the interpreted
// ordinal-slot rule (the interpreted MapQ/FoldQ walk is
// collection-generic).

jit_helpers! { registry = collection_helpers;

/// Flatten a graphix List value into an owned ValArray of its
/// elements. CONSUMES the value (callers marshal via
/// `emit_owned_value_operand_node`, like the map/value-arith
/// helpers). A non-list input — the #219 tainted placeholder (Null),
/// or a malformed chain (unreachable through the typechecker) —
/// yields an EMPTY array: the source disc's TAINT rides the loop's
/// SlotFlags, so the result taints and its payload is unobservable,
/// matching the interpreted forced-taint semantics.
safe fn graphix_list_to_valarray(tv: TagValue) -> u64 {
    let v = tv.value(); // consume; masks the tag bits
    let arr =
        crate::node::collection::list::to_array(&v).unwrap_or_else(|| ValArray::from([]));
    va_bits(arr)
}

/// The exit boundary for list-returning HOF loops: consume the
/// finalize'd ValArray and build the cons chain via
/// `list::from_iter` — the same constructor the interpreted finishes
/// use.
safe fn graphix_valarray_into_list(bits: u64) -> TagValue {
    let arr = va_owned(bits);
    TagValue::clean(crate::node::collection::list::from_iter(arr.iter().cloned()))
}

/// Flatten a Map value into an owned ValArray of 2-element `[k, v]`
/// pair arrays in key-sorted order — exactly the interpreted
/// `ValueMap::values` element encoding (`make_pair`). Consumes the
/// value; non-map input (the tainted placeholder) → empty array, see
/// [`graphix_list_to_valarray`].
safe fn graphix_cmap_to_pairs(tv: TagValue) -> u64 {
    let v = tv.value(); // consume; masks the tag bits
    let arr = match &v {
        Value::Map(m) => ValArray::from_iter(
            m.into_iter().map(|(k, v)| crate::node::collection::make_pair(k, v)),
        ),
        _ => ValArray::from([]),
    };
    va_bits(arr)
}

/// The exit boundary for map-returning HOF loops: consume a
/// finalize'd ValArray of `[k, v]` pairs and build a `Value::Map` via
/// the same `split_pair` + `CMap::from_iter` the interpreted
/// `MapMap::finish` uses (identical duplicate-key semantics). A
/// malformed element is unreachable through the typechecker —
/// contribute nothing and log rather than abort.
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

// ─── Element / slot-state reads ───────────────────────────────────

/// Total scalar slot readers: the slot's payload if it carries the
/// named primitive family (fixed-width and varint encodings of the
/// same width read alike — the unchecked reads they replace read the
/// raw payload word regardless of encoding), the shape's placeholder
/// (0) otherwise. One reader per family; every scalar element / struct
/// field / variant payload helper composes these over a bounds-checked
/// `.get()`.
/// A BORROWED read of a Graphix-minted abstract value's payload (`.0`):
/// the caller keeps owning `tv`. A non-abstract value reads as the
/// shape's placeholder (the TOTAL-reads contract).
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

/// The canonical borrowed-array placeholder — what a shape-mismatched
/// or out-of-bounds composite read returns (see the module's totality
/// contract). Static so borrowed interior pointers stay valid forever.
static EMPTY_ARR: std::sync::LazyLock<ValArray> =
    std::sync::LazyLock::new(|| ValArray::from_iter_exact(std::iter::empty()));

/// Free a slot-state chain: `word` is (0 or) a `Box<Vec<u64>>` raw
/// pointer. `own_levels == 0` means the Vec holds plain data (leaf
/// selection words) UNLESS `leaf` is given, in which case the Vec is
/// per-slot call-site BLOCKS whose anchor words own further chains
/// (see [`SiteLeaf`]); `own_levels > 0` means each entry is itself a
/// chain with one less level (same leaf at the bottom). Shared by the
/// resize helpers' truncate paths and `Kernel::drop`.
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

/// Free a PER-ACTIVATION block tree ([`kernel_abi::SelfBlock`]) rooted
/// at `vecptr` — one `Box<Vec<u64>>` per activation, each holding its
/// own children at `slots`. Iterative over an explicit worklist: the
/// tree is as deep as the recursion was, and depth is bounded by
/// memory. Called from `Kernel::drop` (whole-tree teardown) and from
/// [`reclaim_self_block_tree`] (a shed subtree). Returns the number of
/// blocks freed.
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

/// Global count of live per-activation `SelfBlock`s (test instrument):
/// `graphix_site_child_block` bumps it on allocation,
/// `free_self_block_tree` drops it per block freed. A fused
/// deep-then-shallow recursion must return this toward its shallow
/// depth — proof the reclaim actually runs — not hold the high-water.
pub static LIVE_SELF_BLOCKS: std::sync::atomic::AtomicI64 =
    std::sync::atomic::AtomicI64::new(0);

thread_local! {
    /// The reach GENERATION of the currently-executing fused
    /// recursion's invocation. [`graphix_site_child_block`] stamps every
    /// activation block it reaches with this; `Kernel::update`'s reclaim
    /// frees any block NOT stamped with the current generation — the JIT
    /// twin of the interp's shrink-delete (a depth not reached this
    /// cycle is shed). Saved/restored around every kernel invocation so
    /// a nested kernel does not clobber its caller's.
    pub(crate) static SELF_BLOCK_GEN: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
    /// Count of activation reaches this invocation (one per
    /// [`graphix_site_child_block`] call). `Kernel::update` compares it
    /// to the tree size to GATE the reclaim: no walk unless the tree
    /// shrank. Same save/restore discipline as [`SELF_BLOCK_GEN`].
    pub(crate) static SELF_BLOCK_REACHED: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// Reclaim the parts of a per-activation block tree NOT reached this
/// invocation — the JIT twin of the interp's shrink-delete. `root` is
/// the address of the word holding the tree's root pointer (an entry in
/// the kernel's `state`/`site` buffer). A block stamped with `gen` was
/// reached this invocation → recurse into its children; a block stamped
/// otherwise is a shed depth → free its whole subtree
/// ([`free_self_block_tree`]) and NULL the word that pointed at it, so
/// `Kernel::drop`/`reset_replay` see 0 and never double-free. `words` is
/// the block's emitted size; the gen stamp lives one past it (at index
/// `words`, allocated by [`graphix_site_child_block`] and invisible to
/// emitted code). Returns the number of blocks freed. Iterative — depth
/// is bounded by memory.
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
        // A block with no stamp word (should not happen post-alloc) is
        // treated as reached — never free what we cannot prove is shed.
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

/// Zero the REPLAY words through a per-activation block tree — the
/// `replay_state_words` rule applied to every activation, so a value
/// cached on one evaluation-frame iteration can't bridge the next at
/// any depth. Semantic words (selection memory, first-call flags) and
/// the honor header survive, exactly as they do in the flat case.
/// Iterative like [`free_self_block_tree`], for the same reason.
pub fn reset_self_block_tree(vecptr: u64, slots: &[u32], replay: &[u32]) {
    let mut work: poolshark::local::LPooled<Vec<u64>> = poolshark::local::LPooled::take();
    work.push(vecptr);
    while let Some(p) = work.pop() {
        if p == 0 {
            continue;
        }
        let v = unsafe { &mut *(p as *mut Vec<u64>) };
        for r in replay.iter() {
            if let Some(w) = v.get_mut(*r as usize) {
                *w = 0;
            }
        }
        work.extend(slots.iter().filter_map(|s| v.get(*s as usize).copied()));
    }
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

/// The struct field's VALUE slot: `arr[sorted_idx]` is a
/// `Value::Array([name, value])` kv-pair; slot 1 is the value.
/// Total — `None` on OOB or a non-pair slot (a placeholder struct).
fn struct_field(p: &ValArray, sorted_idx: usize) -> Option<&Value> {
    match p.get(sorted_idx)? {
        Value::Array(kv) => kv.get(1),
        _ => None,
    }
}

/// Total slot-as-array reads (see the module's totality contract):
/// the placeholder is the static EMPTY array.
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

// Non-primitive element reads mirror the scalar families: composite
// (Array/Tuple/Struct → ValArray bits), String (→ ArcStr bits), and
// value-shape (Variant/Nullable/DateTime/Duration/Bytes → a
// two-register `Value`). Each returns an OWNED value — a
// refcount-bumped clone (except the explicitly `_borrowed` variants)
// — so the source ValArray keeps its own ref and the kernel's
// scope-exit drop of the source plus the consumer's drop of the
// result don't double-free.

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

/// `arr[idx]` with the full source-level `array[i]` semantics — bounds
/// check, negative-from-end indexing, and the `ArrayIndexError` value
/// on out-of-bounds — by delegating to the shared [`array_index`].
/// Used by the JIT's `ArrayGet` (whose result type is
/// `Nullable<elem>`); returns the element on success or the error
/// Value otherwise, as a two-register `Value`. `idx` is a signed
/// `i64` (negatives index from the end).
safe fn graphix_valarray_index(bits: u64, idx: i64) -> TagValue {
    TagValue::clean(array_index(va_ref(&bits), idx))
}

/// Per-slot cross-invocation state table for a scaffold loop (see
/// `BodyCx::open_slot_tables`). The word at `word` — a claimed static
/// state word, or an entry of an enclosing loop's directory table —
/// owns a boxed `Vec<u64>`, one word per slot ordinal, zero = "no
/// previous observation", resized here with prefix retention: exactly
/// the interpreted MapQ/FoldQ slot rule (shrink truncates — dropped
/// slots' memory is gone; regrow re-creates FRESH zeroed slots).
/// `own_levels == 0` is a LEAF table of selection words; `own_levels
/// > 0` is a DIRECTORY whose entries own the next nesting level's
/// tables (one owning level per enclosing loop), so truncation frees
/// the dropped slots' subtrees — the interp rule applied per level.
/// `valid == 0` (tainted source — the node-walk saw no event) skips
/// the logical resize, mirroring `SlotFlags::apply`'s prev-len word;
/// the table still GROWS zero-filled so in-loop accesses up to `len`
/// stay in bounds. The chain is freed by `Kernel::drop` via
/// `WrappedKernel::slot_table_words`.
/// `leaf` is 0 for plain chains (selection-word leaves), or a baked
/// `*const SiteLeaf` (kept alive by the kernel cache) when the chain
/// bottoms out in per-slot call-site BLOCKS — truncation at any
/// directory level must free through it.
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

/// The per-activation block for a SELF-CALL
/// ([`kernel_abi::SelfBlock`]): allocate-on-first-use, retained
/// thereafter, so a recursive activation gets the instance memory a
/// statically-carved call-site block cannot give it. `word` is the
/// root word in the CALLER's own block (null when the caller has no
/// block — the no-memory degrade stands); `desc` points at the
/// callee's `KernelSig::site_desc`, which is where the block's size
/// lives because a self-call's size is its own body's, unknown while
/// that body is still emitting.
///
/// The honor header is copied down from the parent every call: the
/// child's caches are live exactly when its parent's are, which is
/// what makes the reset contract hold for the whole tree (whoever
/// resets the root walks it — `reset_self_block_tree`).
unsafe fn graphix_site_child_block(
    word: *mut u64,
    desc: *const u64,
    parent: *const u64,
) -> *mut u64 {
    use std::sync::atomic::{AtomicU64, Ordering::Relaxed};
    if word.is_null() {
        return std::ptr::null_mut();
    }
    let d = unsafe { (*(desc as *const AtomicU64)).load(Relaxed) };
    let words = (d & 0xffff_ffff) as usize;
    if words == 0 {
        return std::ptr::null_mut();
    }
    let word = unsafe { &mut *word };
    if *word == 0 {
        // One word PAST the emitted layout (index `words`) holds the
        // reach GENERATION stamp — invisible to emitted code (which uses
        // `0..words`) and to the free/reset walks (which use `slots`,
        // all `< words`). `Kernel::update`'s reclaim reads it to shed the
        // subtrees this invocation did not reach (recursion shrink).
        *word = Box::into_raw(Box::new(vec![0u64; words + 1])) as u64;
        LIVE_SELF_BLOCKS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    let hdr = (d >> 32) as usize;
    if hdr != 0 && !parent.is_null() {
        v[hdr - 1] = unsafe { *parent.add(hdr - 1) };
    }
    // Reached this invocation: stamp the current generation and count
    // the reach (the reclaim gate in `Kernel::update`).
    v[words] = SELF_BLOCK_GEN.get();
    SELF_BLOCK_REACHED.set(SELF_BLOCK_REACHED.get() + 1);
    v.as_mut_ptr()
}

/// The chain-leaf resize for per-slot CALL-SITE BLOCKS (`SiteLeaf` —
/// `stride` words per slot ordinal): the Vec's logical length is
/// `slots * stride`, resized with prefix retention at BLOCK
/// granularity; truncation frees the dropped blocks' anchor-owned
/// chains. Same `valid` rule as `graphix_slot_state_table`.
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

// Two-level struct field reads by sorted index — total, composing
// [`struct_field`] with the slot readers.

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

/// `arr[idx]` as OWNED ValArray bits (Array/Tuple/Struct elem —
/// refcount clone of the slot).
safe fn graphix_valarray_get_array(bits: u64, idx: usize) -> u64 {
    va_bits(slot_array(va_ref(&bits).get(idx)).clone())
}

/// `arr[idx]` as BORROWED ValArray bits — the slot's own handle word
/// (or the static empty placeholder), no refcount bump. Valid for
/// exactly as long as the parent array is alive; used by select's
/// nested structural patterns, whose scrutinee is a borrowed env slot
/// pinned across the whole arm chain (values are immutable, so the
/// slot word is stable). NEVER pass this to a consuming/dropping
/// helper.
safe fn graphix_valarray_get_array_borrowed(bits: u64, idx: usize) -> u64 {
    va_borrowed_bits(slot_array(va_ref(&bits).get(idx)))
}

/// Struct field read (`arr[sorted_idx]` is a `[name, value]` kv-pair;
/// slot 1) as BORROWED ValArray bits — same lifetime contract as
/// [`graphix_valarray_get_array_borrowed`].
safe fn graphix_struct_get_array_borrowed(bits: u64, sorted_idx: usize) -> u64 {
    va_borrowed_bits(slot_array(struct_field(va_ref(&bits), sorted_idx)))
}

/// `arr[idx]` as an owned `ArcStr` (String elem).
safe fn graphix_valarray_get_arcstr(bits: u64, idx: usize) -> arcstr::ArcStr {
    slot_arcstr(va_ref(&bits).get(idx))
}

/// `arr[idx]` as an owned `Value` (value-shape elem). Clones the slot.
safe fn graphix_valarray_get_value(bits: u64, idx: usize) -> TagValue {
    TagValue::clean(va_ref(&bits).get(idx).cloned().unwrap_or(Value::Null))
}

/// Struct field read (two-level: `arr[sorted_idx]` is a `[name,
/// value]` kv-pair; read slot 1) producing OWNED ValArray bits.
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

/// Emission-gated disc probe (GXDBG_CALLRET at COMPILE time): prints
/// a tagged disc word from inside JIT'd code. Debug builds only.
safe fn graphix_dbg_disc(tag: u64, disc: u64) {
    eprintln!("CLIF-DISC tag={tag} disc={disc:x}");
}

/// JIT-emitted code calls this at the start of every wrapper
/// function to bump the per-thread `JIT_INVOCATIONS` counter.
/// `cfg(debug_assertions)`-gated; in release builds the helper
/// is absent, the registration is dead-coded out, and the
/// codegen call site is `#[cfg]`-disabled — zero overhead.
safe fn graphix_record_jit_invocation() {
    JIT_INVOCATIONS.with(|c| c.set(c.get().wrapping_add(1)));
}

}

// ─── DynCall (HOF) dispatch plumbing ─────────────────────────────
//
// JIT'd kernels invoke fn-typed params (HOF args) via the
// `graphix_dyncall` helper. The dispatch is type-erased through a
// `DynDispatchHandle` set on a thread-local by `Kernel::update`:
//
//   1. Before calling the wrapper, Kernel::update builds a
//      `DynDispatchHandle` whose `dispatch` is a monomorphized
//      `dispatch_typed::<R, E>` function pointer and whose `state`
//      points to a per-call struct holding the dyn_slots, ctx,
//      event, and fn_arg_values references.
//   2. The thread-local `DYN_DISPATCH_HANDLE` is saved and replaced
//      with a pointer to the new handle (save-restore lets nested
//      JIT calls stack properly).
//   3. The wrapper runs. JIT'd code emits calls to `graphix_dyncall`
//      with `fn_index` + an args buffer. The helper reads the
//      handle and calls its `dispatch` function pointer.
//   4. If the inner Apply returns `None` ("no value this cycle" —
//      e.g. `buffer::encode`'s Pad guard), `dispatch` returns
//      `(0, 0)` and sets `DYNCALL_PENDING`. The JIT'd call site
//      take-and-CLEARS the flag right after the call and converts
//      it to a #219 tainted placeholder that continues — only the
//      consumers of that result bottom, matching the node-walk.
//   5. After the wrapper returns, Kernel::update checks
//      `DYNCALL_PENDING` (and resets it). A set flag now only means
//      a GENUINE whole-kernel abort (interrupt poll,
//      the return-gate force): the kernel result is discarded and
//      Kernel::update returns `None`.

use std::cell::Cell;

/// Two-word return shape for `graphix_dyncall` / `dispatch_typed` —
/// the unified Value ABI: `word0 = Value disc`, `word1 = the genuine
/// Value payload word` for EVERY return type (a scalar's widened
/// bits, a composite's ValArray bits, a string's ArcStr bits, a
/// value-shape's payload; Unit returns the Null disc). Matches the
/// cranelift sig `(I64, I64)` and the SysV ABI's RAX/RDX return
/// regs. The call site adapts per its static type (narrow a scalar,
/// adopt owned bits, discard Unit). On pending both words are `0`.
#[repr(C)]
pub struct DynCallRet {
    pub word0: u64,
    pub word1: u64,
}

/// Type-erased per-call dispatch handle, lifetime-tied to one
/// `Kernel::update` invocation. Built on the stack there and
/// pointed at via the thread-local.
#[repr(C)]
pub struct DynDispatchHandle {
    /// Function pointer to a monomorphized `dispatch_typed::<R, E>`.
    /// Takes `(state, fn_index, args, taint_mask, stale_mask,
    /// site_word)` and returns the result as a [`DynCallRet`] Value
    /// pair (unified Value ABI). Returns `(0, 0)` and sets
    /// `DYNCALL_PENDING` if the inner Apply returned None.
    /// `taint_mask` bit `i` set = deliver arg slot `i` as ABSENCE;
    /// `stale_mask` bit `i` set = deliver it as `TagValue::stale`;
    /// `site_word` is the emission site's identity-word address, null
    /// for the key-0 bucket (see `graphix_dyncall`).
    pub dispatch: unsafe extern "C" fn(
        state: *mut u8,
        fn_index: u32,
        args: *mut LPooled<Vec<Value>>,
        taint_mask: u64,
        stale_mask: u64,
        site_word: *mut u64,
    ) -> DynCallRet,
    /// Function pointer to a monomorphized `set_var_typed::<R, E>`. A
    /// fused `connect` (or a handler-ful `?`'s error delivery) writes a
    /// reactive variable mid-kernel: `(bind_id, disc, payload)` →
    /// `ctx.set_var`. A `#219`-tainted disc means "no value this cycle"
    /// and is skipped (the node-walk's `if let Some(v) = ..` guard).
    /// Unlike `dispatch` this does NOT touch the pending flag — a
    /// variable write is a side effect that mustn't abort the kernel.
    pub set_var:
        unsafe extern "C" fn(state: *mut u8, bind_id: u64, disc: u64, payload: u64),
    /// Type-erased pointer to the per-call state struct that holds
    /// `&mut [DynCallSlot<R, E>]`, `&[Value]` (fn_arg_values),
    /// `&mut ExecCtx<R, E>`, `&mut Event<E>`.
    pub state: *mut u8,
}

thread_local! {
    /// Pointer to the active `DynDispatchHandle` for the JIT'd
    /// kernel currently on the call stack. Set/restored by
    /// `Kernel::update`. Null when no JIT'd kernel is in flight.
    pub static DYN_DISPATCH_HANDLE: Cell<*const DynDispatchHandle> =
        const { Cell::new(std::ptr::null()) };

    /// Sticky abort flag. `dispatch_typed` sets it when an inner
    /// Apply returns `None`, but the JIT'd call site immediately
    /// take-and-clears that and converts it to a #219 tainted
    /// placeholder — so by the time `Kernel::update` reads (and
    /// resets) the flag after the wrapper returns, a set flag only
    /// means a GENUINE whole-kernel abort (interrupt poll, depth
    /// trip, the return-gate force, a callee abort propagated at the
    /// call site): the kernel's result is discarded and `update`
    /// itself returns `None`.
    pub static DYNCALL_PENDING: Cell<bool> = const { Cell::new(false) };

    /// Raw pointer to the active runtime's [`crate::Control`], set per
    /// cycle by `do_cycle` on the thread running the node loop. Read by
    /// `graphix_interrupted` (emitted at every JIT loop head) so a wedged
    /// kernel aborts on `interrupt()`/`abort()`. Null when no cycle is in
    /// flight; valid for the runtime's lifetime when set (the `Control`
    /// lives inside the runtime's `ExecCtx`).
    pub static INTERRUPT_PTR: Cell<*const crate::Control> =
        const { Cell::new(std::ptr::null()) };

    /// Per-thread JIT invocation counter, debug-build only.
    /// Bumped by `graphix_record_jit_invocation` (called inline
    /// at the start of every JIT'd wrapper). Read by the test
    /// harness's `jit` mode to prove the JIT actually ran for
    /// each fixture — kernels that silently fell back to interp
    /// would leave this at zero and the test asserts `> 0`.
    ///
    /// `cfg(debug_assertions)`-gated so production release builds
    /// pay no instrumentation overhead. The codegen call site and
    /// the helper registration are gated the same way.
    #[cfg(debug_assertions)]
    pub static JIT_INVOCATIONS: Cell<u64> = const { Cell::new(0) };

    /// Per-thread *fused-kernel* execution counter, debug-build only.
    /// Bumped by [`record_fusion_invocation`] at the commit point of
    /// every fused-kernel execution — `Kernel::update` once it has
    /// decided to run — regardless of whether the kernel runs via the
    /// JIT or the interpreter. Together with [`JIT_INVOCATIONS`] this
    /// lets the test harness distinguish three observable fusion
    /// outcomes for a fixture: `FUSION > 0 && JIT > 0` (fused + JIT),
    /// `FUSION > 0 && JIT == 0` (fused but ran on interp — the JIT
    /// can't lower this shape yet), and `FUSION == 0` (no fusion at
    /// all). A JIT'd kernel bumps both (its `Kernel::update` runs,
    /// then its JIT wrapper runs); an interp-fused kernel bumps only
    /// this one.
    #[cfg(debug_assertions)]
    pub static FUSION_INVOCATIONS: Cell<u64> = const { Cell::new(0) };
}

/// Point `graphix_interrupted` at `control` on the CURRENT thread. The
/// runtime calls this at the start of each cycle (on whatever worker the
/// cycle runs on, since the task may migrate) so JIT kernels poll the
/// right runtime's control. The pointer stays valid because `control`
/// lives in the runtime's `ExecCtx` for its whole lifetime.
pub fn set_interrupt_ptr(control: &crate::Control) {
    INTERRUPT_PTR.with(|c| c.set(control as *const crate::Control));
}

/// Abort the runtime whose control this thread is running under (the
/// stack budget's containment — `stack::grow`). No runtime on this
/// thread: nothing to abort.
pub(crate) fn abort_current_control_budget() {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if !p.is_null() {
            // SAFETY: see `graphix_interrupted`.
            unsafe { (*p).abort_budget() }
        }
    });
}

/// Bump the per-thread fused-kernel execution counter. Called from
/// `Kernel::update` once a fused kernel commits to running (after
/// the "did any input update" gate). `cfg(debug_assertions)`-gated.
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

/// Read the current thread's JIT invocation count. Returns `0`
/// if no JIT'd kernel has run since the last reset. Available
/// only under `cfg(debug_assertions)`.
#[cfg(debug_assertions)]
pub fn jit_invocations() -> u64 {
    JIT_INVOCATIONS.with(|c| c.get())
}

/// Reset the current thread's JIT invocation count to zero.
/// Available only under `cfg(debug_assertions)`.
#[cfg(debug_assertions)]
pub fn reset_jit_invocations() {
    JIT_INVOCATIONS.with(|c| c.set(0));
}

#[cfg(debug_assertions)]
thread_local! {
    /// Per-thread fusion-bail reason log (debug-build only,
    /// instrumentation for the gap-map harvest). Each `record_fuse_bail`
    /// call pushes a short tag (e.g. `node:Sample`, `dostmt:Connect`,
    /// `call:json::read`) at a site where fusion lowering gives up.
    /// Harvested per-fixture by the `run!` discovery branch to map every
    /// `None` fixture to its actual blocker. Capped to avoid unbounded
    /// growth on a deeply-recursive failing compile.
    static FUSE_BAILS: std::cell::RefCell<Vec<arcstr::ArcStr>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Record a fusion-bail reason (debug instrumentation). No-op in
/// release (the call sites are `cfg(debug_assertions)`-gated).
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

/// Clear the current thread's fusion-bail log (called after runtime
/// init in the test harness, so only the fixture's own bails count).
#[cfg(debug_assertions)]
pub fn reset_fuse_bails() {
    FUSE_BAILS.with(|b| b.borrow_mut().clear());
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regression: `graphix_dyncall_pending_take` must PEEK, not
    /// clear. The clearing variant (former behavior) caused a
    /// latent UB on composite-DynCall pending paths — the JIT pre_
    /// pending block consumed the flag from inside the kernel,
    /// confusing `Kernel::update`'s wrapper-level pending check
    /// into decoding the kernel's null sentinel as a real Value
    /// (`Box::from_raw(0)` or `transmute([0, 0]) -> Value`).
    /// Multiple calls in succession must all observe the same
    /// state; the flag stays set until `Kernel::update` resets
    /// it at the top of the NEXT kernel invocation.
    #[test]
    fn pending_take_is_peek_not_clear() {
        DYNCALL_PENDING.with(|c| c.set(false));
        assert_eq!(graphix_dyncall_pending_take(), 0, "peek on cleared flag returns 0");
        DYNCALL_PENDING.with(|c| c.set(true));
        assert_eq!(graphix_dyncall_pending_take(), 1, "peek on set flag returns 1");
        assert_eq!(
            graphix_dyncall_pending_take(),
            1,
            "second peek still returns 1 — the take is not clearing"
        );
        assert!(
            DYNCALL_PENDING.with(|c| c.get()),
            "flag remains set after multiple peeks"
        );
        // Tidy up so other tests don't see a stale set flag.
        DYNCALL_PENDING.with(|c| c.set(false));
    }

    /// `graphix_dyncall_pending_take_clear` must read AND clear —
    /// it converts a per-site "this dispatch returned no value" into
    /// a #219 tainted placeholder, and leaving the flag set would
    /// make `Kernel::update`'s wrapper check discard the whole
    /// kernel result (the item-28 whole-kernel bottom this variant
    /// exists to prevent).
    #[test]
    fn pending_take_clear_clears() {
        DYNCALL_PENDING.with(|c| c.set(false));
        assert_eq!(graphix_dyncall_pending_take_clear(), 0, "clear on cleared flag");
        DYNCALL_PENDING.with(|c| c.set(true));
        assert_eq!(graphix_dyncall_pending_take_clear(), 1, "reads the set flag");
        assert_eq!(
            graphix_dyncall_pending_take_clear(),
            0,
            "second take sees the flag cleared by the first"
        );
        assert!(!DYNCALL_PENDING.with(|c| c.get()), "flag is clear after take_clear");
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
        let fp = len as usize as u64;
        let decode =
            |r: DynCallRet| unsafe { crate::TagValue::from_raw(r.word0, r.word1) };
        let tv = decode(unsafe { graphix_fastcall(fp, ap, n, 0, 0) });
        assert_eq!(tv.tag(), crate::Tag::FIRED);
        assert_eq!(tv.value_cloned(), Value::I64(3));
        let tv = decode(unsafe { graphix_fastcall(fp, ap, n, 0, 0b1) });
        assert_eq!(tv.tag(), crate::Tag::STALE);
        assert_eq!(tv.value_cloned(), Value::I64(3));
        let never_p = never as usize as u64;
        let tv = decode(unsafe { graphix_fastcall(never_p, ap, n, 0b1, 0) });
        assert_eq!(tv.tag(), crate::Tag::FRESH_BOTTOM);
        let tv = decode(unsafe { graphix_fastcall(never_p, ap, n, 0b1, 0b1) });
        assert_eq!(tv.tag(), crate::Tag::STALE_BOTTOM);
        let tv = decode(unsafe { graphix_fastcall(none as usize as u64, ap, n, 0, 0) });
        assert_eq!(tv.tag(), crate::Tag::FRESH_BOTTOM);
        // The view never took ownership: the array is still ours.
        assert_eq!(args.len(), 1);
    }
}
