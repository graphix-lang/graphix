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
//! Each helper is `#[no_mangle]` + `extern "C"` so cranelift can
//! resolve its address through `JITBuilder::symbol(name, ptr)` and
//! emit a normal indirect call from JIT'd code. All pointers are
//! borrowed for the duration of the call — no ownership transfer.
//!
//! Safety contract is the same as `ValArray::get_unchecked`:
//! - `arr` must be a valid pointer to a `ValArray` that lives at
//!   least until the call returns.
//! - `idx` must be `< arr.len()`.
//! - The slot at `idx` must actually carry a payload of the
//!   helper's named primitive type.
//!
//! Fusion-built kernels statically guarantee all three (the
//! typechecker pinned the param's `GirType::Array<T>` /
//! `GirType::Tuple([..., T, ...])` shape, and the runtime hands
//! us a `Value::Array(ValArray)` that matches).

use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;

/// Compile-time checks pinning the Value-ABI layout the JIT relies
/// on. If any of these fire on a netidx / upstream-crate upgrade,
/// the by-value Value plumbing would silently mis-pack the bits —
/// catch it at compile time instead.
///
/// Top check: the outer `Value` is exactly two machine words,
/// 8-byte aligned. This is the assumption the helpers' two-`I64`
/// CLIF signature and `GirNode::update`'s slot-pair pack/unpack
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

#[inline]
unsafe fn arr<'a>(p: *const ValArray) -> &'a ValArray {
    unsafe { &*p }
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

#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_buf_new(cap: usize) -> *mut LPooled<Vec<Value>> {
    let mut buf: LPooled<Vec<Value>> = LPooled::take();
    buf.reserve(cap);
    Box::into_raw(Box::new(buf))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_i64(
    buf: *mut LPooled<Vec<Value>>,
    v: i64,
) {
    unsafe { (*buf).push(Value::I64(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_f64(
    buf: *mut LPooled<Vec<Value>>,
    v: f64,
) {
    unsafe { (*buf).push(Value::F64(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_i32(
    buf: *mut LPooled<Vec<Value>>,
    v: i32,
) {
    unsafe { (*buf).push(Value::I32(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_u32(
    buf: *mut LPooled<Vec<Value>>,
    v: u32,
) {
    unsafe { (*buf).push(Value::U32(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_f32(
    buf: *mut LPooled<Vec<Value>>,
    v: f32,
) {
    unsafe { (*buf).push(Value::F32(v)) }
}

/// Push a bool. JIT passes `1` for true / `0` for false (CLIF Bool
/// is `I8`); we mask to one bit so any nonzero is treated as true.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_bool(
    buf: *mut LPooled<Vec<Value>>,
    v: u8,
) {
    unsafe { (*buf).push(Value::Bool(v != 0)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_i8(buf: *mut LPooled<Vec<Value>>, v: i8) {
    unsafe { (*buf).push(Value::I8(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_i16(
    buf: *mut LPooled<Vec<Value>>,
    v: i16,
) {
    unsafe { (*buf).push(Value::I16(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_u8(buf: *mut LPooled<Vec<Value>>, v: u8) {
    unsafe { (*buf).push(Value::U8(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_u16(
    buf: *mut LPooled<Vec<Value>>,
    v: u16,
) {
    unsafe { (*buf).push(Value::U16(v)) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_u64(
    buf: *mut LPooled<Vec<Value>>,
    v: u64,
) {
    unsafe { (*buf).push(Value::U64(v)) }
}

/// Push a `Value::Array(inner)` slot, taking ownership of `inner`.
/// Used by StructNew (each field is a `[name, value]` inner array)
/// and by VariantNew with payload.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_array(
    buf: *mut LPooled<Vec<Value>>,
    inner: *mut ValArray,
) {
    unsafe {
        let owned = *Box::from_raw(inner);
        (*buf).push(Value::Array(owned));
    }
}

/// Borrow-mode array push: refcount-bumps the caller's
/// `*const ValArray` and pushes `Value::Array(clone)`. Used for
/// DynCall composite args where the caller still owns its local
/// and the dispatcher needs a separately-tracked clone.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_array_borrowed(
    buf: *mut LPooled<Vec<Value>>,
    src: *const ValArray,
) {
    unsafe {
        let cloned: ValArray = (*src).clone();
        (*buf).push(Value::Array(cloned));
    }
}

/// Borrow-mode value push: the caller passes their `Value` bits by
/// value (Value is `#[repr(u64)]`, 16 bytes — two integer registers
/// on the SysV ABI). We refcount-bump the inner, push the clone, and
/// `mem::forget` the input so the caller's bits stay valid (the
/// caller's local retains its ref).
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_buf_push_value_borrowed(
    buf: *mut LPooled<Vec<Value>>,
    v: Value,
) {
    let dup = v.clone();
    std::mem::forget(v);
    unsafe { (*buf).push(dup) }
}

/// Move-mode value push: consumes `v` (the caller transfers
/// ownership) and pushes it into the buf without an extra refcount
/// bump. Used for DynCall variant args sourced from an owned producer
/// (VariantNew, composite-return DynCall result) — the value isn't
/// referenced anywhere else, so a borrow-mode push would leak the
/// extra ref.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_buf_push_value(buf: *mut LPooled<Vec<Value>>, v: Value) {
    unsafe { (*buf).push(v) }
}

/// Drop a partially-built (still-`Box`'d) `LPooled<Vec<Value>>`.
/// Used by the JIT cleanup_stack on pending paths: a producer op
/// that allocated a buf but never reached `finalize` needs to free
/// it explicitly.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_drop(buf: *mut LPooled<Vec<Value>>) {
    unsafe { drop(Box::from_raw(buf)) }
}

/// Read the `DYNCALL_PENDING` thread-local *without clearing it*.
/// JIT-emitted code calls this:
///   * after every `graphix_dyncall` to branch into `pre_pending_<n>`
///     (cleanup + jump to `pending_exit`),
///   * at every `GirStmt::Return` to drop the about-to-return result
///     and jump to `pending_exit` instead of returning a leaked
///     allocation (when an earlier scalar DynCall pended silently
///     and the kernel's return path produced an owned heap value).
///
/// The flag stays set so that `GirNode::update`'s wrapper-level
/// check sees it and returns `None`. `GirNode::update` resets the
/// flag to `false` at the top of every kernel invocation, so a
/// stale `true` from a previous run never leaks across.
///
/// Returns 1 if pending was set, 0 otherwise.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_dyncall_pending_take() -> u8 {
    DYNCALL_PENDING.with(|c| if c.get() { 1 } else { 0 })
}

/// Set `DYNCALL_PENDING` to true. Called by the JIT-emitted code
/// at `GirOp::QopUnwrap`'s error branch — same pending signal as
/// the dispatcher uses, just driven by a kernel-internal check
/// instead of a `dispatch` return.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_dyncall_set_pending() {
    DYNCALL_PENDING.with(|c| c.set(true))
}

/// Push a `Value::String(s)` slot. The string is identified by an
/// `ArcStr` already interned somewhere; we pass the raw pointer
/// (which is `Arc<str>` data — see `ArcStr::as_ptr`) plus length.
/// For now this codepath isn't used; we add it when VariantNew /
/// StructNew names need to be threaded through. (Field names for
/// StructNew are compile-time constants — handled in the codegen
/// by emitting a different helper that takes the ArcStr by ptr.)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_arcstr(
    buf: *mut LPooled<Vec<Value>>,
    ptr: *const arcstr::ArcStr,
) {
    unsafe { (*buf).push(Value::String((*ptr).clone())) }
}

/// Push an owned `ArcStr` onto the dyncall arg buffer, wrapping it in
/// `Value::String`. Used for `GirType::String` DynCall args — the
/// caller's SSA holds an owned ArcStr (bit-equivalent to its raw
/// thin pointer) and transfers ownership into the buf.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_push_string(
    buf: *mut LPooled<Vec<Value>>,
    s: arcstr::ArcStr,
) {
    unsafe { (*buf).push(Value::String(s)) }
}

/// Finalize the buffer into an owned `ValArray`. The returned
/// pointer is `Box::into_raw(Box::new(arr))` — caller must
/// `Box::from_raw` to reclaim or `graphix_valarray_drop` to free.
/// Consumes the buf (drops the `LPooled<Vec<Value>>` so its
/// allocation returns to the pool).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_finalize(
    buf: *mut LPooled<Vec<Value>>,
) -> *mut ValArray {
    unsafe {
        let mut owned = *Box::from_raw(buf);
        let arr = ValArray::from_iter_exact(owned.drain(..));
        Box::into_raw(Box::new(arr))
    }
}

/// Reference-count bump of an existing ValArray. Returns an owned
/// pointer (Box::into_raw of a fresh `Box<ValArray>`). Used at
/// kernel entry to convert borrowed composite params into owned
/// locals — keeps ownership uniform inside the JIT'd body.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_clone(arr: *const ValArray) -> *mut ValArray {
    unsafe { Box::into_raw(Box::new((*arr).clone())) }
}

/// Drop an owned ValArray pointer. Use when a local goes out of
/// scope or is overwritten by a tail-call rebind.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_drop(arr: *mut ValArray) {
    unsafe { drop(Box::from_raw(arr)) }
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

/// Wrap an owned ValArray into a `Value::Array` for variant
/// construction (with-payload variants — the outer value is
/// `Value::Array([tag, ...])`). Caller relinquishes the ValArray
/// pointer; helper transfers it into the returned Value.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_new_from_array(arr: *mut ValArray) -> Value {
    Value::Array(unsafe { *Box::from_raw(arr) })
}

/// Build a `Value::String(tag)` for nullary variant construction.
/// `tag` is a pointer to an interned `ArcStr` (compile-time known).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_new_string_from_arcstr(
    tag: *const arcstr::ArcStr,
) -> Value {
    Value::String(unsafe { (*tag).clone() })
}

/// Consume a Value and decrement the inner refcount (for
/// String/Array/Variant/etc. — a no-op for scalar variants like
/// I64/Bool/Null).  Use at scope exit for owned Value locals.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_drop(v: Value) {
    drop(v)
}

/// "Borrowed clone": bump the inner refcount and return a fresh
/// `Value` with valid ownership. The caller's bits stay valid — we
/// `mem::forget` the input so its ref isn't decremented. Net effect:
/// caller now has two valid refs (original + returned clone) and the
/// inner refcount has incremented by one.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_clone(v: Value) -> Value {
    let dup = v.clone();
    std::mem::forget(v);
    dup
}

/// Clone a `Value` from a stable `*const Value` static — a kernel's
/// value-constants table slot (datetime/duration `ConstValue`). Bumps
/// any inner `Arc`. Returns the clone by value (two registers).
///
/// # Safety
/// `ptr` must point to a live `Value` that outlives the JIT'd code.
/// The per-kernel `KernelValues` table owns it (like `KernelStrings`
/// for `ArcStr`), kept alive on the `CachedKernel`.
pub unsafe extern "C" fn graphix_value_clone_from_static(
    ptr: *const Value,
) -> Value {
    unsafe { (*ptr).clone() }
}

// ─── Value arithmetic (datetime/duration `ValueArith`) ────────────
//
// Compute through netidx's `impl {Add,Sub,Mul,Div,Rem} for Value` —
// byte-identical to the non-fused arith node (`lhs $op rhs`). Each
// helper CONSUMES both args (netidx's operators take `self`/`rhs` by
// value); codegen passes OWNED Values (`ensure_owned_value` clones a
// Borrowed Local read, scalar operands are freshly promoted, producer
// ops like `ConstValue` are already owned), so there's no leak or
// double-free.
macro_rules! value_arith_helper {
    ($name:ident, $op:tt) => {
        pub extern "C" fn $name(l: Value, r: Value) -> Value {
            l $op r
        }
    };
}
value_arith_helper!(graphix_value_add, +);
value_arith_helper!(graphix_value_sub, -);
value_arith_helper!(graphix_value_mul, *);
value_arith_helper!(graphix_value_div, /);
value_arith_helper!(graphix_value_rem, %);

// ─── String / ArcStr helpers ──────────────────────────────────────
//
// `ArcStr` is `repr(transparent)` over a thin pointer, so it travels
// across the JIT/Rust boundary as a single 8-byte value. Codegen
// treats `GirType::String` SSA values as `i64` CLIF values holding
// the ArcStr's raw pointer. Lifetime tracking matches the variant /
// nullable scheme — every owned ArcStr SSA either feeds a consumer
// helper that takes ownership (e.g. `graphix_string_buf_push_arcstr`
// drops on push) or is returned across the kernel boundary (the
// wrapper hands it to `GirNode::update` which wraps it into a
// `Value::String`). On the pending path the in-flight string buf
// (still owned by the kernel) drops via `graphix_string_buf_drop`.

/// Clone an interned static `ArcStr` — refcount bump on the slot at
/// `p`, returning a fresh owned ArcStr. Caller has ownership; drops
/// when no longer needed via `graphix_arcstr_drop`. Used by
/// `GirOp::ConstStr` lowering.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_arcstr_clone_from_static(
    p: *const arcstr::ArcStr,
) -> arcstr::ArcStr {
    unsafe { (*p).clone() }
}

/// Drop an owned ArcStr. Refcount decrement; frees the underlying
/// buffer when the last clone goes away.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_arcstr_drop(s: arcstr::ArcStr) {
    drop(s)
}

/// Borrowed-clone an ArcStr by value: bump the refcount, return the
/// fresh clone. The caller's bits stay valid (we `mem::forget` so
/// the input's ref isn't decremented). Net effect: caller now has
/// two valid refs (original + returned clone). Used by
/// `GirOp::Local` reads of String slots and by anywhere else we
/// need to take an additional ref to an in-register ArcStr.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_arcstr_clone(s: arcstr::ArcStr) -> arcstr::ArcStr {
    let dup = s.clone();
    std::mem::forget(s);
    dup
}

/// Build a `Value::String` from an owned ArcStr — boundary
/// marshaling for kernel-return-type `GirType::String`. Consumes the
/// ArcStr (transfers ownership into the Value).
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_new_string(s: arcstr::ArcStr) -> Value {
    Value::String(s)
}

/// Start a fresh string-buffer for `GirOp::Concat`. Returns a heap-
/// owned `*mut String`; caller eventually pairs with
/// `graphix_string_buf_finalize` (success) or `graphix_string_buf_drop`
/// (pending path).
#[unsafe(no_mangle)]
pub extern "C" fn graphix_string_buf_new() -> *mut String {
    Box::into_raw(Box::new(String::new()))
}

/// Drop a string buf without finalizing — used on pending paths
/// when a DynCall short-circuits an in-flight Concat.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_string_buf_drop(buf: *mut String) {
    drop(unsafe { Box::from_raw(buf) })
}

/// Finalize a string buf into an owned ArcStr. Consumes the buf
/// (frees the Box) and returns the resulting ArcStr.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_string_buf_finalize(
    buf: *mut String,
) -> arcstr::ArcStr {
    let s = unsafe { *Box::from_raw(buf) };
    arcstr::ArcStr::from(s.as_str())
}

/// Append an ArcStr's contents to the buf, consuming the ArcStr.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_string_buf_push_arcstr(
    buf: *mut String,
    s: arcstr::ArcStr,
) {
    unsafe { &mut *buf }.push_str(&s);
    // s drops here.
}

/// Per-prim push helpers — format the value via its `Display`
/// (which matches `Value::<T>(v).to_string()` for every primitive
/// since netidx `Value`'s Display just delegates to the inner
/// type's Display).
macro_rules! string_buf_push_prim {
    ($name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $name(buf: *mut String, v: $ty) {
            use std::fmt::Write;
            let _ = write!(unsafe { &mut *buf }, "{}", v);
        }
    };
}
string_buf_push_prim!(graphix_string_buf_push_i64, i64);
string_buf_push_prim!(graphix_string_buf_push_u64, u64);
string_buf_push_prim!(graphix_string_buf_push_i32, i32);
string_buf_push_prim!(graphix_string_buf_push_u32, u32);
string_buf_push_prim!(graphix_string_buf_push_i16, i16);
string_buf_push_prim!(graphix_string_buf_push_u16, u16);
string_buf_push_prim!(graphix_string_buf_push_i8, i8);
string_buf_push_prim!(graphix_string_buf_push_u8, u8);
string_buf_push_prim!(graphix_string_buf_push_f64, f64);
string_buf_push_prim!(graphix_string_buf_push_f32, f32);

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_string_buf_push_bool(buf: *mut String, v: u8) {
    use std::fmt::Write;
    let _ = write!(unsafe { &mut *buf }, "{}", v != 0);
}

/// Test whether a `Value` is `Value::Null`. Borrowed read — caller
/// retains ownership.
///
/// Today `GirOp::IsNull` lowering inlines this test as `icmp_imm
/// (disc, NULL_DISC)` rather than calling the helper; the helper
/// remains registered so out-of-tree code and direct interp tests
/// keep working.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_is_null(v: Value) -> u8 {
    let r = matches!(v, Value::Null) as u8;
    std::mem::forget(v);
    r
}

// ─── Variant consumer ops ────────────────────────────────────────
//
// Variants at runtime are either `Value::String(tag)` for nullary
// or `Value::Array([tag, payload0, ...])` for with-payload. The
// JIT'd code receives a `*const Value` and dispatches on the
// outer Value shape via these helpers.

/// Test whether a variant's runtime tag matches `expected`. Returns
/// Returns 1 (true) or 0 (false). Mirrors the interp's
/// `VariantTagEq`. Takes the variant `Value` by value (16 bytes /
/// two registers); we `mem::forget` after the borrowed read so the
/// caller's bits stay valid — variant locals are dropped at scope
/// exit via the dedicated drop helper.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_variant_tag_eq(
    v: Value,
    expected: *const arcstr::ArcStr,
) -> u8 {
    let r = {
        let exp = unsafe { &*expected };
        match &v {
            Value::String(s) => (s.as_str() == exp.as_str()) as u8,
            Value::Array(a) => unsafe {
                let tag = a.get_ref_unchecked::<arcstr::ArcStr>(0);
                (tag.as_str() == exp.as_str()) as u8
            },
            _ => 0,
        }
    };
    std::mem::forget(v);
    r
}

macro_rules! variant_payload_impl {
    ($name:ident, $ty:ty) => {
        /// Read payload slot `payload_idx` of a with-payload variant
        /// as `$ty`. Slot 0 is the tag; payloads start at slot 1, so
        /// the JIT-emitted code passes the 0-based payload position
        /// and we add 1. Borrowed read — the input `Value` is
        /// `mem::forget`ed so the caller retains ownership.
        #[unsafe(no_mangle)]
        pub extern "C" fn $name(v: Value, payload_idx: usize) -> $ty {
            let r = match &v {
                Value::Array(a) => unsafe { a.get_unchecked::<$ty>(payload_idx + 1) },
                _ => unsafe { std::hint::unreachable_unchecked() },
            };
            std::mem::forget(v);
            r
        }
    };
}

variant_payload_impl!(graphix_variant_payload_i64, i64);
variant_payload_impl!(graphix_variant_payload_f64, f64);
variant_payload_impl!(graphix_variant_payload_i32, i32);
variant_payload_impl!(graphix_variant_payload_u32, u32);
variant_payload_impl!(graphix_variant_payload_f32, f32);
variant_payload_impl!(graphix_variant_payload_i8, i8);
variant_payload_impl!(graphix_variant_payload_i16, i16);
variant_payload_impl!(graphix_variant_payload_u8, u8);
variant_payload_impl!(graphix_variant_payload_u16, u16);
variant_payload_impl!(graphix_variant_payload_u64, u64);

/// Bool payload is read from the slot's tag bit (true if the slot
/// holds `Value::Bool(true)`), not as a generic `bool` —
/// `ValArray::get_unchecked` doesn't support `bool` directly. Kept
/// as a separate impl to avoid teaching the macro a special case.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_variant_payload_bool(v: Value, payload_idx: usize) -> u8 {
    let r = match &v {
        Value::Array(a) => unsafe { a.get_unchecked::<bool>(payload_idx + 1) as u8 },
        _ => unsafe { std::hint::unreachable_unchecked() },
    };
    std::mem::forget(v);
    r
}

// ─── DynCall (HOF) dispatch ──────────────────────────────────────
//
// JIT'd kernels invoke fn-typed params (HOF args) via the
// `graphix_dyncall` helper. The dispatch is type-erased through a
// `DynDispatchHandle` set on a thread-local by `GirNode::update`:
//
//   1. Before calling the wrapper, GirNode::update builds a
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
//   4. If the inner Apply returns `None` (callee not ready this
//      cycle), `dispatch` returns 0 and sets `DYNCALL_PENDING`.
//      Otherwise it returns the scalar result's raw u64 bits.
//   5. After the wrapper returns, GirNode::update checks
//      `DYNCALL_PENDING` (and resets it). If set, the kernel
//      result is discarded and GirNode::update returns `None` so
//      the runtime re-fires next cycle.
//
// Restrictions: this v1 only supports DynCalls where both args
// and return are scalar primitives (no composite/variant args or
// returns). The kernel itself must also have scalar return — a
// pending DynCall produces garbage 0s downstream, and a composite
// kernel return would mean reading a null pointer.

use std::cell::Cell;

/// Two-word return shape for `graphix_dyncall` / `dispatch_typed`.
/// Matches the cranelift sig `(I64, I64)` and the SysV ABI's RAX/RDX
/// return regs. For `ret_kind=2` (Value-shape return) both words
/// are meaningful: `word0 = Value disc`, `word1 = Value payload`.
/// For `ret_kind=0` (scalar) the value lives in `word0` and `word1`
/// is `0`; for `ret_kind=1` (composite ValArray pointer) `word0`
/// holds the owned `*mut ValArray` and `word1 = 0`; for `ret_kind=3`
/// (Unit) both are `0`. The JIT's call site reads `inst_results[0]`
/// for the single-word cases and both for the Value-shape case;
/// the second slot stays `0` on pending too, so a stale value can't
/// leak through.
#[repr(C)]
pub struct DynCallRet {
    pub word0: u64,
    pub word1: u64,
}

/// Type-erased per-call dispatch handle, lifetime-tied to one
/// `GirNode::update` invocation. Built on the stack there and
/// pointed at via the thread-local.
#[repr(C)]
pub struct DynDispatchHandle {
    /// Function pointer to a monomorphized `dispatch_typed::<R, E>`.
    /// Takes `(state, fn_index, args, ret_kind)` and returns a
    /// [`DynCallRet`] encoded per `ret_kind`. Returns `(0, 0)` and
    /// sets `DYNCALL_PENDING` if the inner Apply returned None.
    pub dispatch: unsafe extern "C" fn(
        state: *mut u8,
        fn_index: u32,
        args: *mut LPooled<Vec<Value>>,
        ret_kind: u8,
    ) -> DynCallRet,
    /// Type-erased pointer to the per-call state struct that holds
    /// `&mut [DynCallSlot<R, E>]`, `&[Value]` (fn_arg_values),
    /// `&mut ExecCtx<R, E>`, `&mut Event<E>`.
    pub state: *mut u8,
}

thread_local! {
    /// Pointer to the active `DynDispatchHandle` for the JIT'd
    /// kernel currently on the call stack. Set/restored by
    /// `GirNode::update`. Null when no JIT'd kernel is in flight.
    pub static DYN_DISPATCH_HANDLE: Cell<*const DynDispatchHandle> =
        const { Cell::new(std::ptr::null()) };

    /// Sticky flag set by `dispatch_typed` when an inner Apply
    /// returns `None`. Read and reset by `GirNode::update` after
    /// the wrapper returns; if true, the kernel's result is
    /// discarded and `update` itself returns `None`.
    pub static DYNCALL_PENDING: Cell<bool> = const { Cell::new(false) };

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
    /// every fused-kernel execution — `GirNode::update` once it has
    /// decided to run — regardless of whether the kernel runs via the
    /// JIT or the interpreter. Together with [`JIT_INVOCATIONS`] this
    /// lets the test harness distinguish three observable fusion
    /// outcomes for a fixture: `FUSION > 0 && JIT > 0` (fused + JIT),
    /// `FUSION > 0 && JIT == 0` (fused but ran on interp — the JIT
    /// can't lower this shape yet), and `FUSION == 0` (no fusion at
    /// all). A JIT'd kernel bumps both (its `GirNode::update` runs,
    /// then its JIT wrapper runs); an interp-fused kernel bumps only
    /// this one.
    #[cfg(debug_assertions)]
    pub static FUSION_INVOCATIONS: Cell<u64> = const { Cell::new(0) };
}

/// Bump the per-thread fused-kernel execution counter. Called from
/// `GirNode::update` once a fused kernel commits to running (after
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

/// JIT-emitted code calls this at the start of every wrapper
/// function to bump the per-thread `JIT_INVOCATIONS` counter.
/// `cfg(debug_assertions)`-gated; in release builds the helper
/// is absent, the registration is dead-coded out, and the
/// codegen call site is `#[cfg]`-disabled — zero overhead.
#[cfg(debug_assertions)]
#[unsafe(no_mangle)]
pub extern "C" fn graphix_record_jit_invocation() {
    JIT_INVOCATIONS.with(|c| c.set(c.get().wrapping_add(1)));
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

/// The single registered DynCall entry point. Indirects through
/// the thread-local handle to the monomorphized dispatcher and
/// returns a two-word [`DynCallRet`].
///
/// `ret_kind`:
/// - `0`: scalar (`Prim`) — `word0` holds the result's bits.
/// - `1`: composite ValArray (`Array` / `Tuple` / `Struct`) —
///   `word0` holds an owned `*mut ValArray` (from `Box::into_raw`).
/// - `2`: Value-shape (`Variant` / `Nullable`) — `word0` holds
///   the Value's discriminant, `word1` the payload word.
/// - `3`: Unit — both words are `0`, caller discards.
///
/// `word1` is `0` for every kind except `2`.
///
/// On pending (inner Apply returned `None`): returns `(0, 0)`,
/// sets `DYNCALL_PENDING`. JIT-emitted code calls
/// `graphix_dyncall_pending_take` after the dyncall to decide
/// whether to branch to its `pre_pending_<n>` cleanup block.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_dyncall(
    fn_index: u32,
    args: *mut LPooled<Vec<Value>>,
    ret_kind: u8,
) -> DynCallRet {
    let handle = DYN_DISPATCH_HANDLE.with(|c| c.get());
    if handle.is_null() {
        panic!(
            "graphix_dyncall: no DynDispatchHandle set — GirNode::update \
             must populate the thread-local before invoking JIT'd code \
             that calls HOFs"
        );
    }
    unsafe {
        let h = &*handle;
        (h.dispatch)(h.state, fn_index, args, ret_kind)
    }
}

/// Read element `idx` of `arr` as an `i64`. JIT-side counterpart of
/// `GirOp::ArrayGet` / `GirOp::TupleGet` for scalar i64 elements.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i64(p: *const ValArray, idx: usize) -> i64 {
    unsafe { arr(p).get_unchecked::<i64>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_f64(p: *const ValArray, idx: usize) -> f64 {
    unsafe { arr(p).get_unchecked::<f64>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i32(p: *const ValArray, idx: usize) -> i32 {
    unsafe { arr(p).get_unchecked::<i32>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u32(p: *const ValArray, idx: usize) -> u32 {
    unsafe { arr(p).get_unchecked::<u32>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_f32(p: *const ValArray, idx: usize) -> f32 {
    unsafe { arr(p).get_unchecked::<f32>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_bool(p: *const ValArray, idx: usize) -> u8 {
    unsafe { arr(p).get_unchecked::<bool>(idx) as u8 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i8(p: *const ValArray, idx: usize) -> i8 {
    unsafe { arr(p).get_unchecked::<i8>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i16(p: *const ValArray, idx: usize) -> i16 {
    unsafe { arr(p).get_unchecked::<i16>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u8(p: *const ValArray, idx: usize) -> u8 {
    unsafe { arr(p).get_unchecked::<u8>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u16(p: *const ValArray, idx: usize) -> u16 {
    unsafe { arr(p).get_unchecked::<u16>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u64(p: *const ValArray, idx: usize) -> u64 {
    unsafe { arr(p).get_unchecked::<u64>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_len(p: *const ValArray) -> usize {
    unsafe { arr(p).len() }
}

/// Two-level struct field read: `arr[sorted_idx]` is itself a
/// `Value::Array([name, value])` kv-pair; we read slot 1 (the value)
/// as the named primitive. Mirrors the interp's `GirOp::StructGet`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_i64(
    p: *const ValArray,
    sorted_idx: usize,
) -> i64 {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv.get_unchecked::<i64>(1)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_f64(
    p: *const ValArray,
    sorted_idx: usize,
) -> f64 {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv.get_unchecked::<f64>(1)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_i32(
    p: *const ValArray,
    sorted_idx: usize,
) -> i32 {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv.get_unchecked::<i32>(1)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_u32(
    p: *const ValArray,
    sorted_idx: usize,
) -> u32 {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv.get_unchecked::<u32>(1)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_f32(
    p: *const ValArray,
    sorted_idx: usize,
) -> f32 {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv.get_unchecked::<f32>(1)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_bool(
    p: *const ValArray,
    sorted_idx: usize,
) -> u8 {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv.get_unchecked::<bool>(1) as u8
    }
}

macro_rules! struct_get_impl {
    ($name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $name(p: *const ValArray, sorted_idx: usize) -> $ty {
            unsafe {
                let kv = match &arr(p)[sorted_idx] {
                    Value::Array(a) => a,
                    _ => std::hint::unreachable_unchecked(),
                };
                kv.get_unchecked::<$ty>(1)
            }
        }
    };
}

struct_get_impl!(graphix_struct_get_i8, i8);
struct_get_impl!(graphix_struct_get_i16, i16);
struct_get_impl!(graphix_struct_get_u8, u8);
struct_get_impl!(graphix_struct_get_u16, u16);
struct_get_impl!(graphix_struct_get_u64, u64);

/// Build the list of (name, fn pointer) pairs to register with the
/// JIT. Called once at `JitCtx::new`; no caching needed.
pub fn all_symbols() -> Vec<(&'static str, *const u8)> {
    vec![
        ("graphix_valarray_get_i64", graphix_valarray_get_i64 as *const u8),
        ("graphix_valarray_get_f64", graphix_valarray_get_f64 as *const u8),
        ("graphix_valarray_get_i32", graphix_valarray_get_i32 as *const u8),
        ("graphix_valarray_get_u32", graphix_valarray_get_u32 as *const u8),
        ("graphix_valarray_get_f32", graphix_valarray_get_f32 as *const u8),
        ("graphix_valarray_get_bool", graphix_valarray_get_bool as *const u8),
        ("graphix_valarray_len", graphix_valarray_len as *const u8),
        ("graphix_struct_get_i64", graphix_struct_get_i64 as *const u8),
        ("graphix_struct_get_f64", graphix_struct_get_f64 as *const u8),
        ("graphix_struct_get_i32", graphix_struct_get_i32 as *const u8),
        ("graphix_struct_get_u32", graphix_struct_get_u32 as *const u8),
        ("graphix_struct_get_f32", graphix_struct_get_f32 as *const u8),
        ("graphix_struct_get_bool", graphix_struct_get_bool as *const u8),
        // Producer-op builder.
        ("graphix_value_buf_new", graphix_value_buf_new as *const u8),
        ("graphix_value_buf_push_i64", graphix_value_buf_push_i64 as *const u8),
        ("graphix_value_buf_push_f64", graphix_value_buf_push_f64 as *const u8),
        ("graphix_value_buf_push_i32", graphix_value_buf_push_i32 as *const u8),
        ("graphix_value_buf_push_u32", graphix_value_buf_push_u32 as *const u8),
        ("graphix_value_buf_push_f32", graphix_value_buf_push_f32 as *const u8),
        ("graphix_value_buf_push_bool", graphix_value_buf_push_bool as *const u8),
        ("graphix_value_buf_push_array", graphix_value_buf_push_array as *const u8),
        ("graphix_value_buf_push_arcstr", graphix_value_buf_push_arcstr as *const u8),
        ("graphix_value_buf_push_string", graphix_value_buf_push_string as *const u8),
        ("graphix_valarray_finalize", graphix_valarray_finalize as *const u8),
        ("graphix_valarray_clone", graphix_valarray_clone as *const u8),
        ("graphix_valarray_drop", graphix_valarray_drop as *const u8),
        ("graphix_value_new_from_array", graphix_value_new_from_array as *const u8),
        (
            "graphix_value_new_string_from_arcstr",
            graphix_value_new_string_from_arcstr as *const u8,
        ),
        ("graphix_value_drop", graphix_value_drop as *const u8),
        ("graphix_value_clone", graphix_value_clone as *const u8),
        (
            "graphix_value_clone_from_static",
            graphix_value_clone_from_static as *const u8,
        ),
        ("graphix_value_add", graphix_value_add as *const u8),
        ("graphix_value_sub", graphix_value_sub as *const u8),
        ("graphix_value_mul", graphix_value_mul as *const u8),
        ("graphix_value_div", graphix_value_div as *const u8),
        ("graphix_value_rem", graphix_value_rem as *const u8),
        ("graphix_variant_tag_eq", graphix_variant_tag_eq as *const u8),
        ("graphix_variant_payload_i64", graphix_variant_payload_i64 as *const u8),
        ("graphix_variant_payload_f64", graphix_variant_payload_f64 as *const u8),
        ("graphix_variant_payload_i32", graphix_variant_payload_i32 as *const u8),
        ("graphix_variant_payload_u32", graphix_variant_payload_u32 as *const u8),
        ("graphix_variant_payload_f32", graphix_variant_payload_f32 as *const u8),
        ("graphix_variant_payload_bool", graphix_variant_payload_bool as *const u8),
        // Smaller-width helpers added in a follow-up to fill out
        // the PrimType matrix. CLIF return widths: I8/U8 → I8,
        // I16/U16 → I16, U64 → I64 (CLIF doesn't carry signedness).
        ("graphix_valarray_get_i8", graphix_valarray_get_i8 as *const u8),
        ("graphix_valarray_get_i16", graphix_valarray_get_i16 as *const u8),
        ("graphix_valarray_get_u8", graphix_valarray_get_u8 as *const u8),
        ("graphix_valarray_get_u16", graphix_valarray_get_u16 as *const u8),
        ("graphix_valarray_get_u64", graphix_valarray_get_u64 as *const u8),
        ("graphix_struct_get_i8", graphix_struct_get_i8 as *const u8),
        ("graphix_struct_get_i16", graphix_struct_get_i16 as *const u8),
        ("graphix_struct_get_u8", graphix_struct_get_u8 as *const u8),
        ("graphix_struct_get_u16", graphix_struct_get_u16 as *const u8),
        ("graphix_struct_get_u64", graphix_struct_get_u64 as *const u8),
        ("graphix_value_buf_push_i8", graphix_value_buf_push_i8 as *const u8),
        ("graphix_value_buf_push_i16", graphix_value_buf_push_i16 as *const u8),
        ("graphix_value_buf_push_u8", graphix_value_buf_push_u8 as *const u8),
        ("graphix_value_buf_push_u16", graphix_value_buf_push_u16 as *const u8),
        ("graphix_value_buf_push_u64", graphix_value_buf_push_u64 as *const u8),
        ("graphix_variant_payload_i8", graphix_variant_payload_i8 as *const u8),
        ("graphix_variant_payload_i16", graphix_variant_payload_i16 as *const u8),
        ("graphix_variant_payload_u8", graphix_variant_payload_u8 as *const u8),
        ("graphix_variant_payload_u16", graphix_variant_payload_u16 as *const u8),
        ("graphix_variant_payload_u64", graphix_variant_payload_u64 as *const u8),
        ("graphix_dyncall", graphix_dyncall as *const u8),
        ("graphix_dyncall_pending_take", graphix_dyncall_pending_take as *const u8),
        ("graphix_dyncall_set_pending", graphix_dyncall_set_pending as *const u8),
        (
            "graphix_value_buf_push_array_borrowed",
            graphix_value_buf_push_array_borrowed as *const u8,
        ),
        (
            "graphix_value_buf_push_value_borrowed",
            graphix_value_buf_push_value_borrowed as *const u8,
        ),
        ("graphix_value_buf_push_value", graphix_value_buf_push_value as *const u8),
        ("graphix_value_buf_drop", graphix_value_buf_drop as *const u8),
        // `graphix_value_is_null` is kept for direct interp/test
        // call paths even though the JIT inlines the disc compare
        // (`icmp_imm Equal disc, NULL_DISC`) and never calls it.
        ("graphix_value_is_null", graphix_value_is_null as *const u8),
        // ─── String helpers ──────────────────────────────────────
        ("graphix_arcstr_clone_from_static",
         graphix_arcstr_clone_from_static as *const u8),
        ("graphix_arcstr_clone", graphix_arcstr_clone as *const u8),
        ("graphix_arcstr_drop", graphix_arcstr_drop as *const u8),
        ("graphix_value_new_string", graphix_value_new_string as *const u8),
        ("graphix_string_buf_new", graphix_string_buf_new as *const u8),
        ("graphix_string_buf_drop", graphix_string_buf_drop as *const u8),
        ("graphix_string_buf_finalize",
         graphix_string_buf_finalize as *const u8),
        ("graphix_string_buf_push_arcstr",
         graphix_string_buf_push_arcstr as *const u8),
        ("graphix_string_buf_push_i64",
         graphix_string_buf_push_i64 as *const u8),
        ("graphix_string_buf_push_u64",
         graphix_string_buf_push_u64 as *const u8),
        ("graphix_string_buf_push_i32",
         graphix_string_buf_push_i32 as *const u8),
        ("graphix_string_buf_push_u32",
         graphix_string_buf_push_u32 as *const u8),
        ("graphix_string_buf_push_i16",
         graphix_string_buf_push_i16 as *const u8),
        ("graphix_string_buf_push_u16",
         graphix_string_buf_push_u16 as *const u8),
        ("graphix_string_buf_push_i8",
         graphix_string_buf_push_i8 as *const u8),
        ("graphix_string_buf_push_u8",
         graphix_string_buf_push_u8 as *const u8),
        ("graphix_string_buf_push_f64",
         graphix_string_buf_push_f64 as *const u8),
        ("graphix_string_buf_push_f32",
         graphix_string_buf_push_f32 as *const u8),
        ("graphix_string_buf_push_bool",
         graphix_string_buf_push_bool as *const u8),
        // ─── Debug-build instrumentation ────────────────────────
        // Bumps the per-thread `JIT_INVOCATIONS` counter on every
        // wrapper entry; lets the test harness's `jit` mode assert
        // the JIT actually executed. Excluded from release builds.
        #[cfg(debug_assertions)]
        ("graphix_record_jit_invocation",
         graphix_record_jit_invocation as *const u8),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regression: `graphix_dyncall_pending_take` must PEEK, not
    /// clear. The clearing variant (former behavior) caused a
    /// latent UB on composite-DynCall pending paths — the JIT pre_
    /// pending block consumed the flag from inside the kernel,
    /// confusing `GirNode::update`'s wrapper-level pending check
    /// into decoding the kernel's null sentinel as a real Value
    /// (`Box::from_raw(0)` or `transmute([0, 0]) -> Value`).
    /// Multiple calls in succession must all observe the same
    /// state; the flag stays set until `GirNode::update` resets
    /// it at the top of the NEXT kernel invocation.
    #[test]
    fn pending_take_is_peek_not_clear() {
        DYNCALL_PENDING.with(|c| c.set(false));
        assert_eq!(
            graphix_dyncall_pending_take(),
            0,
            "peek on cleared flag returns 0"
        );
        DYNCALL_PENDING.with(|c| c.set(true));
        assert_eq!(
            graphix_dyncall_pending_take(),
            1,
            "peek on set flag returns 1"
        );
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
}
