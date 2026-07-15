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
//! typechecker pinned the param's `Type::Array<T>` /
//! `Type::Tuple([..., T, ...])` shape, and the runtime hands
//! us a `Value::Array(ValArray)` that matches).

use crate::node::{
    array::{array_index, array_slice_i64, bytes_index},
    map::map_get,
    op::wrap_arith_error,
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

/// Flatten an owned `*mut ValArray` into `buf`: clone each element into
/// the buf, then drop the array box. Used by the flat_map loop's
/// JIT codegen — the body produces an owned array per element whose
/// contents are concatenated into the output. (`ValArray` is an
/// immutable `Arc<[Value]>`, so elements are cloned, not moved.)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_extend_from_array(
    buf: *mut LPooled<Vec<Value>>,
    inner: *mut ValArray,
) {
    unsafe {
        let owned = *Box::from_raw(inner);
        (*buf).extend(owned.iter().cloned());
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
    v: TagValue,
) {
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
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_buf_push_value(
    buf: *mut LPooled<Vec<Value>>,
    tv: TagValue,
) {
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_drop(buf: *mut LPooled<Vec<Value>>) {
    assert!(!buf.is_null(), "graphix_value_buf_drop: null buf — JIT codegen bug");
    unsafe { drop(Box::from_raw(buf)) }
}

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
#[unsafe(no_mangle)]
pub extern "C" fn graphix_dyncall_pending_take() -> u8 {
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
/// (interrupt, depth trip, the return-gate force) for
/// `Kernel::update`'s wrapper check.
///
/// Returns 1 if pending was set, 0 otherwise.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_dyncall_pending_take_clear() -> u8 {
    DYNCALL_PENDING.with(|c| if c.replace(false) { 1 } else { 0 })
}

/// Set `DYNCALL_PENDING` to true. Called by the JIT-emitted code
/// at the qop-unwrap error branch — same pending signal as
/// the dispatcher uses, just driven by a kernel-internal check
/// instead of a `dispatch` return.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_dyncall_set_pending() {
    DYNCALL_PENDING.with(|c| c.set(true))
}

/// Record the not-fresh (`TAINT`/`STALE`) disc bits of a cross-kernel
/// callee's result. Called by the callee's return path immediately
/// before `return`; the caller takes the bits right after the call.
/// See [`CALLEE_RESULT_FLAGS`].
#[unsafe(no_mangle)]
pub extern "C" fn graphix_callee_flags_set(bits: u64) {
    CALLEE_RESULT_FLAGS.with(|c| c.set(bits))
}

/// Take (read and clear) the callee-result flag bits. Called by the
/// caller immediately after every cross-kernel call returns, keeping
/// the cell clean call-to-call. See [`CALLEE_RESULT_FLAGS`].
#[unsafe(no_mangle)]
pub extern "C" fn graphix_callee_flags_take() -> u64 {
    CALLEE_RESULT_FLAGS.with(|c| c.replace(0))
}

/// Read the active runtime's interrupt/abort control (set in
/// `INTERRUPT_PTR` by `do_cycle`). Returns 1 if a wedged loop should
/// abort (an `interrupt()` or `abort()` is pending), else 0. Emitted at
/// every JIT loop head; on 1 the kernel jumps to its pending-exit (drops
/// in-flight buffers, returns the sentinel) so `Kernel::update` yields
/// `None`.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_interrupted() -> i8 {
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

/// Point `graphix_interrupted` at `control` on the CURRENT thread. The
/// runtime calls this at the start of each cycle (on whatever worker the
/// cycle runs on, since the task may migrate) so JIT kernels poll the
/// right runtime's control. The pointer stays valid because `control`
/// lives in the runtime's `ExecCtx` for its whole lifetime.
pub fn set_interrupt_ptr(control: &crate::Control) {
    INTERRUPT_PTR.with(|c| c.set(control as *const crate::Control));
}

/// Enter one nested lambda dispatch against the shared call-depth
/// guard (`Control::depth_push` — the SAME counter the node-walk's
/// `GXLambda::update` pushes, so interleaved kernel/node-walk frames
/// of an impure program are bounded together). Returns 1 when the
/// dispatch may proceed (pair with `graphix_depth_pop` after the
/// call), 0 at the limit — the call site must skip the call and abort
/// the kernel to bottom, the same observable as the node-walk's
/// guarded dispatch producing nothing. With no cycle in flight (null
/// ptr) the dispatch proceeds unguarded.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_depth_push() -> i8 {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if p.is_null() {
            1
        } else {
            // Cooperative interrupt: a runaway call TREE (exponential
            // breadth under the depth limit) only polled at loop
            // backedges — checking here makes native non-tail
            // recursion abortable, twinning GXLambda::update's poll.
            // SAFETY: see `graphix_interrupted`.
            if unsafe { (*p).interrupted() } {
                return 0;
            }
            let ok = unsafe { (*p).depth_push() };
            if !ok {
                // Native code can't push an `RtDiagnostic` — flag the
                // trip on the Control; `FusedKernel::update` takes it
                // after the invocation and reports the kernel's spec.
                unsafe { (*p).set_depth_trip() };
            }
            i8::from(ok)
        }
    })
}

/// The fused HOF-loop twin of [`graphix_depth_push`]: enter the
/// callback-dispatch level a scaffold loop's inlined body runs at
/// (`Control::depth_enter`). UNCONDITIONAL increment — pair with an
/// unconditional `graphix_depth_pop` after the loop closes. 0 = the
/// limit is reached: the loop must be skipped (bound zeroed) and its
/// result tainted, matching the node-walk's per-element dispatch trip.
/// The interrupt is not polled here — the loop head polls per
/// iteration.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_depth_enter() -> i8 {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if p.is_null() {
            1
        } else {
            // SAFETY: see `graphix_interrupted`.
            let ok = unsafe { (*p).depth_enter() };
            if !ok {
                unsafe { (*p).set_depth_trip() };
            }
            i8::from(ok)
        }
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn graphix_depth_pop() {
    INTERRUPT_PTR.with(|c| {
        let p = c.get();
        if !p.is_null() {
            // SAFETY: see `graphix_interrupted`.
            unsafe { (*p).depth_pop() }
        }
    })
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
/// `Value::String`. Used for `Type::String` DynCall args — the
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
/// scope or is overwritten by a tail-call rebind. Null is always a
/// codegen bug (a pending sentinel leaked into a drop) — panic
/// loudly instead of UB.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_drop(arr: *mut ValArray) {
    assert!(!arr.is_null(), "graphix_valarray_drop: null ValArray — JIT codegen bug");
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
pub unsafe extern "C" fn graphix_value_new_from_array(arr: *mut ValArray) -> TagValue {
    TagValue::clean(Value::Array(unsafe { *Box::from_raw(arr) }))
}

/// Build a `Value::String(tag)` for nullary variant construction.
/// `tag` is a pointer to an interned `ArcStr` (compile-time known).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_new_string_from_arcstr(
    tag: *const arcstr::ArcStr,
) -> TagValue {
    TagValue::clean(Value::String(unsafe { (*tag).clone() }))
}

/// Unwrap an owned `Value::Array` into the composite ABI's owned
/// `*mut ValArray`. The payload word INSIDE a Value is the ValArray
/// bits themselves, not a box — handing it directly to a composite
/// consumer is a type confusion (the consumer's drop would
/// `Box::from_raw` the Arc's data pointer). Consumes the Value;
/// ownership of the inner ValArray transfers into the box.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_into_array(v: TagValue) -> *mut ValArray {
    match v.value() {
        Value::Array(a) => Box::into_raw(Box::new(a)),
        // A non-Array here is a CODEGEN bug (the emit's taint/shape gates
        // failed) — abort defined rather than UB; extern "C" makes the
        // panic a nounwind abort.
        v => panic!("graphix_value_into_array: expected Value::Array, got {v:?}"),
    }
}

/// Borrowed-read variant of [`graphix_value_into_array`]: clones the
/// inner ValArray (refcount bump) and forgets the input so the
/// caller's bits stay valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_into_array_borrowed(v: TagValue) -> *mut ValArray {
    let ptr = v.with_value(|v| match v {
        Value::Array(a) => Box::into_raw(Box::new(a.clone())),
        v => {
            panic!("graphix_value_into_array_borrowed: expected Value::Array, got {v:?}")
        }
    });
    std::mem::forget(v); // borrowed read — the caller keeps owning it
    ptr
}

/// Consume a Value and decrement the inner refcount (for
/// String/Array/Variant/etc. — a no-op for scalar variants like
/// I64/Bool/Null).  Use at scope exit for owned Value locals.
///
/// Takes the two raw register words rather than `Value` itself
/// (bit-identical ABI — the 16-byte aggregate passes as the same two
/// integer registers) so the all-zero pending sentinel can be
/// REJECTED before an invalid `Value` materializes: `Value`'s
/// discriminants are bitmasks starting at 0x1, so disc 0 is never a
/// real value, and a typed `v: Value` parameter holding it would be
/// UB at the boundary. A zero disc here is always a codegen bug (a
/// pending sentinel leaked into a drop); the panic aborts at the
/// `extern "C"` boundary with the message printed, instead of UB.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_drop(tv: TagValue) {
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
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_clone(tv: TagValue) -> TagValue {
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
pub unsafe extern "C" fn graphix_value_clone_from_static(ptr: *const Value) -> TagValue {
    TagValue::clean(unsafe { (*ptr).clone() })
}

// ─── Value arithmetic (datetime/duration `ValueArith`) ────────────
//
// Compute through netidx's `impl {Add,Sub,Mul,Div,Rem} for Value` —
// byte-identical to the non-fused arith node (`lhs $op rhs`). Each
// helper CONSUMES both args (netidx's operators take `self`/`rhs` by
// value); codegen passes OWNED Values (`ensure_owned_value` clones a
// Borrowed Local read, scalar operands are freshly promoted, producer
// ops like a value-shape `Const` are already owned), so there's no
// leak or double-free.
macro_rules! value_arith_helper {
    ($name:ident, $op:tt) => {
        pub extern "C" fn $name(l: TagValue, r: TagValue) -> TagValue {
            // Mask both operands (a tainted disc is not a clean tag); the
            // result is a clean netidx Value, the JIT re-applies taint.
            // An Error result (duration/datetime arith failure) is BOTTOM
            // for these UNCHECKED ops — the node-walk's BinOp converts
            // Value::Error to log+None (node/op.rs), so return a tainted
            // Null, never the Error as a value (soak finding
            // corpus-fuzz/divergence_000018: `duration:1.s / i64:0`). The
            // checked `+?` family has its own helpers and keeps its
            // catchable ArithError.
            match l.value() $op r.value() {
                Value::Error(_) => {
                    TagValue::tainted(Value::Null)
                }
                v => TagValue::clean(v),
            }
        }
    };
}
value_arith_helper!(graphix_value_add, +);
value_arith_helper!(graphix_value_sub, -);
value_arith_helper!(graphix_value_mul, *);
value_arith_helper!(graphix_value_div, /);
value_arith_helper!(graphix_value_rem, %);

// Checked arithmetic (`+?`/`-?`/`*?`/`/?`/`%?`) — netidx's `checked_*`
// inherent methods, with any raw error wrapped into the catchable
// `ArithError` error VALUE through the SAME
// [`wrap_arith_error`] core the node-walk's checked
// update uses (never bottom, unlike unchecked div0). Each helper
// CONSUMES both args, same contract as the unchecked family above.
macro_rules! value_checked_arith_helper {
    ($name:ident, $method:ident) => {
        pub extern "C" fn $name(l: TagValue, r: TagValue) -> TagValue {
            TagValue::clean(wrap_arith_error(l.value().$method(r.value())))
        }
    };
}
value_checked_arith_helper!(graphix_value_checked_add, checked_add);
value_checked_arith_helper!(graphix_value_checked_sub, checked_sub);
value_checked_arith_helper!(graphix_value_checked_mul, checked_mul);
value_checked_arith_helper!(graphix_value_checked_div, checked_div);
value_checked_arith_helper!(graphix_value_checked_rem, checked_rem);

/// Value equality (`ValueEq`). Compares via netidx's
/// `impl PartialEq for Value` — byte-identical to the non-fused `==`
/// node. CONSUMES both args (they're dropped at function end), matching
/// the owned-operand contract `compile_owned_value_operand` produces.
pub extern "C" fn graphix_value_eq(l: TagValue, r: TagValue) -> u8 {
    (l.value() == r.value()) as u8
}

/// `bytes[i]` indexing. Extracts the `PBytes` from a
/// `Value::Bytes`, indexes via the shared `node::array::bytes_index`
/// (bounds-checked, negative-from-end), and returns `Nullable<u8>`'s
/// `Value` (the `u8` or the out-of-bounds error). CONSUMES the bytes
/// Value (passed owned by `compile_owned_value_operand`).
pub extern "C" fn graphix_bytes_index(v: TagValue, i: i64) -> TagValue {
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
pub extern "C" fn graphix_map_ref(map: TagValue, key: TagValue) -> TagValue {
    TagValue::clean(map_get(&map.value(), &key.value()))
}

/// Array/bytes slice `a[i..j]`. `flags` bit0 =
/// `start` present, bit1 = `end` present (absent bounds pass 0). Routes
/// to the shared `node::array::array_slice_i64`, returning the
/// sub-array/sub-bytes or an error (`Nullable<source>`'s `Value`).
/// CONSUMES the source Value (passed owned by
/// `compile_owned_value_operand`).
pub extern "C" fn graphix_array_slice(
    src: TagValue,
    start: i64,
    end: i64,
    flags: i64,
) -> TagValue {
    let s = if flags & 1 != 0 { Some(start) } else { None };
    let e = if flags & 2 != 0 { Some(end) } else { None };
    TagValue::clean(array_slice_i64(&src.value(), s, e))
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

/// Clone an interned static `ArcStr` — refcount bump on the slot at
/// `p`, returning a fresh owned ArcStr. Caller has ownership; drops
/// when no longer needed via `graphix_arcstr_drop`. Used by
/// string-constant lowering.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_arcstr_clone_from_static(
    p: *const arcstr::ArcStr,
) -> arcstr::ArcStr {
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
#[unsafe(no_mangle)]
pub extern "C" fn graphix_arcstr_drop(s: u64) {
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
#[unsafe(no_mangle)]
pub extern "C" fn graphix_arcstr_clone(s: arcstr::ArcStr) -> arcstr::ArcStr {
    let dup = s.clone();
    std::mem::forget(s);
    dup
}

/// Build a `Value::String` from an owned ArcStr — boundary
/// marshaling for kernel-return-type `Type::String`. Consumes the
/// ArcStr (transfers ownership into the Value).
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_new_string(s: arcstr::ArcStr) -> TagValue {
    TagValue::clean(Value::String(s))
}

/// Start a fresh string-buffer for interpolation/concat. Returns a heap-
/// owned `*mut String`; caller eventually pairs with
/// `graphix_string_buf_finalize` (success) or `graphix_string_buf_drop`
/// (pending path).
/// The empty-`ArcStr` placeholder for a tainted String position — the
/// static empty (clone/drop are no-ops on it), returned as the raw
/// thin-pointer bits the String ABI uses. Helper-safe by construction.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_arcstr_empty() -> u64 {
    unsafe { std::mem::transmute::<arcstr::ArcStr, u64>(arcstr::ArcStr::new()) }
}

/// The empty-`ValArray` placeholder for a tainted composite position —
/// a fresh owned box (the consumer chain drops it through the normal
/// scope machinery). Allocates, but only on the (rare) tainted path.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_valarray_empty_boxed() -> *mut ValArray {
    Box::into_raw(Box::new(ValArray::from([])))
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

/// Flatten a graphix List value into an owned ValArray of its
/// elements. CONSUMES the value (callers marshal via
/// `emit_owned_value_operand_node`, like the map/value-arith
/// helpers). A non-list input — the #219 tainted placeholder (Null),
/// or a malformed chain (unreachable through the typechecker) —
/// yields an EMPTY array: the source disc's TAINT rides the loop's
/// SlotFlags, so the result taints and its payload is unobservable,
/// matching the interpreted forced-taint semantics.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_list_to_valarray(tv: TagValue) -> *mut ValArray {
    let v = tv.value(); // consume; masks the tag bits
    let arr =
        crate::node::collection::list::to_array(&v).unwrap_or_else(|| ValArray::from([]));
    Box::into_raw(Box::new(arr))
}

/// The exit boundary for list-returning HOF loops: consume the
/// finalize'd ValArray and build the cons chain via
/// `list::from_iter` — the same constructor the interpreted finishes
/// use.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_into_list(arr: *mut ValArray) -> TagValue {
    let arr = unsafe { *Box::from_raw(arr) };
    TagValue::clean(crate::node::collection::list::from_iter(arr.iter().cloned()))
}

/// Flatten a Map value into an owned ValArray of 2-element `[k, v]`
/// pair arrays in key-sorted order — exactly the interpreted
/// `ValueMap::values` element encoding (`make_pair`). Consumes the
/// value; non-map input (the tainted placeholder) → empty array, see
/// [`graphix_list_to_valarray`].
#[unsafe(no_mangle)]
pub extern "C" fn graphix_cmap_to_pairs(tv: TagValue) -> *mut ValArray {
    let v = tv.value(); // consume; masks the tag bits
    let arr = match &v {
        Value::Map(m) => ValArray::from_iter(
            m.into_iter().map(|(k, v)| crate::node::collection::make_pair(k, v)),
        ),
        _ => ValArray::from([]),
    };
    Box::into_raw(Box::new(arr))
}

/// The exit boundary for map-returning HOF loops: consume a
/// finalize'd ValArray of `[k, v]` pairs and build a `Value::Map` via
/// the same `split_pair` + `CMap::from_iter` the interpreted
/// `MapMap::finish` uses (identical duplicate-key semantics). A
/// malformed element is unreachable through the typechecker —
/// contribute nothing and log rather than abort.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_into_cmap(arr: *mut ValArray) -> TagValue {
    let arr = unsafe { *Box::from_raw(arr) };
    let m = netidx_value::Map::from_iter(arr.iter().filter_map(|v| {
        let pair = crate::node::collection::split_pair(v);
        if pair.is_none() {
            log::error!("graphix_valarray_into_cmap: malformed pair {v:?}");
        }
        pair
    }));
    TagValue::clean(Value::Map(m))
}

/// flat_map extend for LIST-valued callback results: walk the cons
/// chain and push each element. A NON-list value pushes as a single
/// element — `ListFlatMap::finish`'s fallback, bit-for-bit. Consumes
/// the value (the loop wraps the body result owned).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_buf_extend_from_list(
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

#[unsafe(no_mangle)]
pub extern "C" fn graphix_string_buf_new() -> *mut String {
    Box::into_raw(Box::new(String::new()))
}

/// Drop a string buf without finalizing — used on pending paths
/// when a DynCall short-circuits an in-flight Concat. Null is
/// always a codegen bug (a pending sentinel leaked into a drop) —
/// panic loudly instead of UB.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_string_buf_drop(buf: *mut String) {
    assert!(!buf.is_null(), "graphix_string_buf_drop: null buf — JIT codegen bug");
    drop(unsafe { Box::from_raw(buf) })
}

/// Finalize a string buf into an owned ArcStr. Consumes the buf
/// (frees the Box) and returns the resulting ArcStr.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_string_buf_finalize(buf: *mut String) -> arcstr::ArcStr {
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
/// Today is-null lowering inlines this test as `icmp_imm
/// (disc, NULL_DISC)` rather than calling the helper; the helper
/// remains registered so out-of-tree code and direct interp tests
/// keep working.
#[unsafe(no_mangle)]
pub extern "C" fn graphix_value_is_null(v: TagValue) -> u8 {
    let r = v.with_value(|v| matches!(v, Value::Null) as u8);
    std::mem::forget(v); // borrowed read — caller keeps owning it
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
    v: TagValue,
    expected: *const arcstr::ArcStr,
) -> u8 {
    let r = v.with_value(|v| {
        let exp = unsafe { &*expected };
        match v {
            Value::String(s) => (s.as_str() == exp.as_str()) as u8,
            Value::Array(a) => unsafe {
                let tag = a.get_ref_unchecked::<arcstr::ArcStr>(0);
                (tag.as_str() == exp.as_str()) as u8
            },
            _ => 0,
        }
    });
    std::mem::forget(v); // borrowed read — caller keeps owning it
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
        pub extern "C" fn $name(v: TagValue, payload_idx: usize) -> $ty {
            let r = v.with_value(|v| match v {
                Value::Array(a) => unsafe { a.get_unchecked::<$ty>(payload_idx + 1) },
                _ => unsafe { std::hint::unreachable_unchecked() },
            });
            std::mem::forget(v); // borrowed read — caller keeps owning it
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
pub extern "C" fn graphix_variant_payload_bool(v: TagValue, payload_idx: usize) -> u8 {
    let r = v.with_value(|v| match v {
        Value::Array(a) => unsafe { a.get_unchecked::<bool>(payload_idx + 1) as u8 },
        _ => unsafe { std::hint::unreachable_unchecked() },
    });
    std::mem::forget(v); // borrowed read — caller keeps owning it
    r
}

// ─── DynCall (HOF) dispatch ──────────────────────────────────────
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
//      a GENUINE whole-kernel abort (interrupt poll, depth trip,
//      the return-gate force): the kernel result is discarded and
//      Kernel::update returns `None`.

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
/// `Kernel::update` invocation. Built on the stack there and
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

    /// Not-fresh (`TAINT`/`STALE`) disc bits of a cross-kernel CALLEE's
    /// result. The scalar/composite return ABI carries no disc word, so
    /// the callee's return path records the bits here and the caller
    /// takes (reads + clears) them immediately after the call, OR-ing
    /// them into its synthesized result disc. A bottomed or unfired
    /// callee RESULT thus rides back as data — bottoming the caller only
    /// if its taken output path consumes it (#219) — while genuine
    /// aborts (depth trip, interrupt, an async pend) keep the
    /// [`DYNCALL_PENDING`] whole-region path.
    pub static CALLEE_RESULT_FLAGS: Cell<u64> = const { Cell::new(0) };

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
            "graphix_dyncall: no DynDispatchHandle set — Kernel::update \
             must populate the thread-local before invoking JIT'd code \
             that calls HOFs"
        );
    }
    unsafe {
        let h = &*handle;
        (h.dispatch)(h.state, fn_index, args, ret_kind)
    }
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_set_var(bind_id: u64, disc: u64, payload: u64) {
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

/// Read element `idx` of `arr` as an `i64`. JIT-side counterpart of
/// array/tuple element reads for scalar i64 elements.
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

/// Free a slot-state chain: `word` is (0 or) a `Box<Vec<u64>>` raw
/// pointer. `own_levels == 0` means the Vec holds plain data (leaf
/// selection words); `own_levels > 0` means each entry is itself a
/// chain with one less level. Shared by `graphix_slot_state_table`'s
/// truncate path and `Kernel::drop`.
pub fn free_slot_chain(word: u64, own_levels: u64) {
    if word == 0 {
        return;
    }
    let v = unsafe { Box::from_raw(word as *mut Vec<u64>) };
    if own_levels > 0 {
        for e in v.iter() {
            free_slot_chain(*e, own_levels - 1);
        }
    }
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_slot_state_table(
    word: *mut u64,
    len: u64,
    valid: u64,
    own_levels: u64,
) -> *mut u64 {
    let word = unsafe { &mut *word };
    if *word == 0 {
        *word = Box::into_raw(Box::new(Vec::<u64>::new())) as u64;
    }
    let v = unsafe { &mut *(*word as *mut Vec<u64>) };
    let len = len as usize;
    if valid != 0 && len < v.len() {
        if own_levels > 0 {
            for e in v[len..].iter() {
                free_slot_chain(*e, own_levels - 1);
            }
        }
        v.truncate(len)
    } else if len > v.len() {
        v.resize(len, 0)
    }
    v.as_mut_ptr()
}

/// Two-level struct field read: `arr[sorted_idx]` is itself a
/// `Value::Array([name, value])` kv-pair; we read slot 1 (the value)
/// as the named primitive. Struct field read by sorted index.
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

// ─── Non-primitive element reads ──────────────────────────────────
//
// Mirror the interp's `extract_composite_or_scalar` for element types
// the scalar `get_<prim>` helpers can't handle: composite
// (Array/Tuple/Struct → a `*mut ValArray`), String (→ an `ArcStr`),
// and value-shape (Variant/Nullable/DateTime/Duration/Bytes → a
// two-register `Value`). Each returns an OWNED value — a fresh
// box / refcount-bumped clone — so the source ValArray keeps its own
// ref and the kernel's scope-exit drop of the source plus the
// consumer's drop of the result don't double-free.

/// `arr[idx]` with the full source-level `array[i]` semantics — bounds
/// check, negative-from-end indexing, and the `ArrayIndexError` value
/// on out-of-bounds — by delegating to the shared
/// [`array_index`]. Used by the JIT's `ArrayGet`
/// (whose result type is `Nullable<elem>`); returns the element on
/// success or the error Value otherwise, as a two-register `Value`.
/// `idx` is a signed `i64` (negatives index from the end).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_index(
    p: *const ValArray,
    idx: i64,
) -> TagValue {
    TagValue::clean(array_index(unsafe { arr(p) }, idx))
}

/// `arr[idx]` as an owned `*mut ValArray` (Array/Tuple/Struct elem).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_array(
    p: *const ValArray,
    idx: usize,
) -> *mut ValArray {
    unsafe {
        match &arr(p)[idx] {
            Value::Array(a) => Box::into_raw(Box::new(a.clone())),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

/// `arr[idx]` as a BORROWED `*const ValArray` — an interior pointer
/// into the parent's element slot. Valid for exactly as long as the
/// parent array is alive and unmutated; used by select's nested
/// structural patterns, whose scrutinee is a borrowed env slot pinned
/// across the whole arm chain (values are immutable, so the interior
/// pointer is stable). NEVER pass this to a consuming/dropping helper.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_array_borrowed(
    p: *const ValArray,
    idx: usize,
) -> *const ValArray {
    unsafe {
        match &arr(p)[idx] {
            Value::Array(a) => a as *const ValArray,
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

/// Struct field read (`arr[sorted_idx]` is a `[name, value]` kv-pair;
/// slot 1) as a BORROWED `*const ValArray` interior pointer — same
/// lifetime contract as [`graphix_valarray_get_array_borrowed`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_array_borrowed(
    p: *const ValArray,
    sorted_idx: usize,
) -> *const ValArray {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        match &kv[1] {
            Value::Array(a) => a as *const ValArray,
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

/// `arr[idx]` as an owned `ArcStr` (String elem).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_arcstr(
    p: *const ValArray,
    idx: usize,
) -> arcstr::ArcStr {
    unsafe {
        match &arr(p)[idx] {
            Value::String(s) => s.clone(),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

/// `arr[idx]` as an owned `Value` (value-shape elem). Clones the slot.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_value(
    p: *const ValArray,
    idx: usize,
) -> TagValue {
    TagValue::clean(unsafe { arr(p)[idx].clone() })
}

/// Struct field read (two-level: `arr[sorted_idx]` is a `[name,
/// value]` kv-pair; read slot 1) producing an owned `*mut ValArray`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_array(
    p: *const ValArray,
    sorted_idx: usize,
) -> *mut ValArray {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        match &kv[1] {
            Value::Array(a) => Box::into_raw(Box::new(a.clone())),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_arcstr(
    p: *const ValArray,
    sorted_idx: usize,
) -> arcstr::ArcStr {
    unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        match &kv[1] {
            Value::String(s) => s.clone(),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_struct_get_value(
    p: *const ValArray,
    sorted_idx: usize,
) -> TagValue {
    TagValue::clean(unsafe {
        let kv = match &arr(p)[sorted_idx] {
            Value::Array(a) => a,
            _ => std::hint::unreachable_unchecked(),
        };
        kv[1].clone()
    })
}

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
        ("graphix_slot_state_table", graphix_slot_state_table as *const u8),
        ("graphix_struct_get_i64", graphix_struct_get_i64 as *const u8),
        ("graphix_struct_get_f64", graphix_struct_get_f64 as *const u8),
        ("graphix_struct_get_i32", graphix_struct_get_i32 as *const u8),
        ("graphix_struct_get_u32", graphix_struct_get_u32 as *const u8),
        ("graphix_struct_get_f32", graphix_struct_get_f32 as *const u8),
        ("graphix_struct_get_bool", graphix_struct_get_bool as *const u8),
        // Bounds-checked `array[i]` (returns elem-or-error Value).
        ("graphix_valarray_index", graphix_valarray_index as *const u8),
        // Non-primitive element reads (composite / string / value-shape).
        ("graphix_valarray_get_array", graphix_valarray_get_array as *const u8),
        (
            "graphix_valarray_get_array_borrowed",
            graphix_valarray_get_array_borrowed as *const u8,
        ),
        (
            "graphix_struct_get_array_borrowed",
            graphix_struct_get_array_borrowed as *const u8,
        ),
        ("graphix_valarray_get_arcstr", graphix_valarray_get_arcstr as *const u8),
        ("graphix_valarray_get_value", graphix_valarray_get_value as *const u8),
        ("graphix_struct_get_array", graphix_struct_get_array as *const u8),
        ("graphix_struct_get_arcstr", graphix_struct_get_arcstr as *const u8),
        ("graphix_struct_get_value", graphix_struct_get_value as *const u8),
        // Producer-op builder.
        ("graphix_value_buf_new", graphix_value_buf_new as *const u8),
        ("graphix_value_buf_push_i64", graphix_value_buf_push_i64 as *const u8),
        ("graphix_value_buf_push_f64", graphix_value_buf_push_f64 as *const u8),
        ("graphix_value_buf_push_i32", graphix_value_buf_push_i32 as *const u8),
        ("graphix_value_buf_push_u32", graphix_value_buf_push_u32 as *const u8),
        ("graphix_value_buf_push_f32", graphix_value_buf_push_f32 as *const u8),
        ("graphix_value_buf_push_bool", graphix_value_buf_push_bool as *const u8),
        ("graphix_value_buf_push_array", graphix_value_buf_push_array as *const u8),
        (
            "graphix_value_buf_extend_from_array",
            graphix_value_buf_extend_from_array as *const u8,
        ),
        ("graphix_value_buf_push_arcstr", graphix_value_buf_push_arcstr as *const u8),
        ("graphix_value_buf_push_string", graphix_value_buf_push_string as *const u8),
        ("graphix_valarray_finalize", graphix_valarray_finalize as *const u8),
        ("graphix_valarray_clone", graphix_valarray_clone as *const u8),
        ("graphix_valarray_drop", graphix_valarray_drop as *const u8),
        ("graphix_value_new_from_array", graphix_value_new_from_array as *const u8),
        ("graphix_value_into_array", graphix_value_into_array as *const u8),
        (
            "graphix_value_into_array_borrowed",
            graphix_value_into_array_borrowed as *const u8,
        ),
        (
            "graphix_value_new_string_from_arcstr",
            graphix_value_new_string_from_arcstr as *const u8,
        ),
        ("graphix_value_drop", graphix_value_drop as *const u8),
        ("graphix_value_clone", graphix_value_clone as *const u8),
        ("graphix_value_clone_from_static", graphix_value_clone_from_static as *const u8),
        ("graphix_value_add", graphix_value_add as *const u8),
        ("graphix_value_sub", graphix_value_sub as *const u8),
        ("graphix_value_mul", graphix_value_mul as *const u8),
        ("graphix_value_div", graphix_value_div as *const u8),
        ("graphix_value_rem", graphix_value_rem as *const u8),
        ("graphix_value_checked_add", graphix_value_checked_add as *const u8),
        ("graphix_value_checked_sub", graphix_value_checked_sub as *const u8),
        ("graphix_value_checked_mul", graphix_value_checked_mul as *const u8),
        ("graphix_value_checked_div", graphix_value_checked_div as *const u8),
        ("graphix_value_checked_rem", graphix_value_checked_rem as *const u8),
        ("graphix_value_eq", graphix_value_eq as *const u8),
        ("graphix_bytes_index", graphix_bytes_index as *const u8),
        ("graphix_map_ref", graphix_map_ref as *const u8),
        ("graphix_array_slice", graphix_array_slice as *const u8),
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
        ("graphix_set_var", graphix_set_var as *const u8),
        ("graphix_dyncall_pending_take", graphix_dyncall_pending_take as *const u8),
        (
            "graphix_dyncall_pending_take_clear",
            graphix_dyncall_pending_take_clear as *const u8,
        ),
        ("graphix_dyncall_set_pending", graphix_dyncall_set_pending as *const u8),
        ("graphix_callee_flags_set", graphix_callee_flags_set as *const u8),
        ("graphix_callee_flags_take", graphix_callee_flags_take as *const u8),
        ("graphix_interrupted", graphix_interrupted as *const u8),
        ("graphix_depth_push", graphix_depth_push as *const u8),
        ("graphix_depth_enter", graphix_depth_enter as *const u8),
        ("graphix_depth_pop", graphix_depth_pop as *const u8),
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
        (
            "graphix_arcstr_clone_from_static",
            graphix_arcstr_clone_from_static as *const u8,
        ),
        ("graphix_arcstr_clone", graphix_arcstr_clone as *const u8),
        ("graphix_arcstr_drop", graphix_arcstr_drop as *const u8),
        ("graphix_value_new_string", graphix_value_new_string as *const u8),
        ("graphix_arcstr_empty", graphix_arcstr_empty as *const u8),
        ("graphix_valarray_empty_boxed", graphix_valarray_empty_boxed as *const u8),
        ("graphix_list_to_valarray", graphix_list_to_valarray as *const u8),
        ("graphix_valarray_into_list", graphix_valarray_into_list as *const u8),
        ("graphix_cmap_to_pairs", graphix_cmap_to_pairs as *const u8),
        ("graphix_valarray_into_cmap", graphix_valarray_into_cmap as *const u8),
        (
            "graphix_value_buf_extend_from_list",
            graphix_value_buf_extend_from_list as *const u8,
        ),
        ("graphix_string_buf_new", graphix_string_buf_new as *const u8),
        ("graphix_string_buf_drop", graphix_string_buf_drop as *const u8),
        ("graphix_string_buf_finalize", graphix_string_buf_finalize as *const u8),
        ("graphix_string_buf_push_arcstr", graphix_string_buf_push_arcstr as *const u8),
        ("graphix_string_buf_push_i64", graphix_string_buf_push_i64 as *const u8),
        ("graphix_string_buf_push_u64", graphix_string_buf_push_u64 as *const u8),
        ("graphix_string_buf_push_i32", graphix_string_buf_push_i32 as *const u8),
        ("graphix_string_buf_push_u32", graphix_string_buf_push_u32 as *const u8),
        ("graphix_string_buf_push_i16", graphix_string_buf_push_i16 as *const u8),
        ("graphix_string_buf_push_u16", graphix_string_buf_push_u16 as *const u8),
        ("graphix_string_buf_push_i8", graphix_string_buf_push_i8 as *const u8),
        ("graphix_string_buf_push_u8", graphix_string_buf_push_u8 as *const u8),
        ("graphix_string_buf_push_f64", graphix_string_buf_push_f64 as *const u8),
        ("graphix_string_buf_push_f32", graphix_string_buf_push_f32 as *const u8),
        ("graphix_string_buf_push_bool", graphix_string_buf_push_bool as *const u8),
        // ─── Debug-build instrumentation ────────────────────────
        // Bumps the per-thread `JIT_INVOCATIONS` counter on every
        // wrapper entry; lets the test harness's `jit` mode assert
        // the JIT actually executed. Excluded from release builds.
        #[cfg(debug_assertions)]
        ("graphix_record_jit_invocation", graphix_record_jit_invocation as *const u8),
    ]
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
}
