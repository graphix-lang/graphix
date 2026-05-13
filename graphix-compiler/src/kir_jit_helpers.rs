//! Stable `extern "C"` entry points the JIT calls into for ops that
//! can't be lowered purely in CLIF (composite reads/writes, layout-
//! sensitive operations).
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
//! typechecker pinned the param's `KirType::Array<T>` /
//! `KirType::Tuple([..., T, ...])` shape, and the runtime hands
//! us a `Value::Array(ValArray)` that matches).

use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;

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
pub unsafe extern "C" fn graphix_value_buf_push_i8(
    buf: *mut LPooled<Vec<Value>>,
    v: i8,
) {
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
pub unsafe extern "C" fn graphix_value_buf_push_u8(
    buf: *mut LPooled<Vec<Value>>,
    v: u8,
) {
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
pub unsafe extern "C" fn graphix_valarray_clone(
    arr: *const ValArray,
) -> *mut ValArray {
    unsafe { Box::into_raw(Box::new((*arr).clone())) }
}

/// Drop an owned ValArray pointer. Use when a local goes out of
/// scope or is overwritten by a tail-call rebind.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_drop(arr: *mut ValArray) {
    unsafe { drop(Box::from_raw(arr)) }
}

/// Wrap an owned ValArray into a `Value::Array` for variant
/// construction (with-payload variants — the outer value is
/// `Value::Array([tag, ...])`).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_new_from_array(
    arr: *mut ValArray,
) -> *mut Value {
    unsafe { Box::into_raw(Box::new(Value::Array(*Box::from_raw(arr)))) }
}

/// Build a `Value::String(tag)` for nullary variant construction.
/// `tag` is a pointer to an interned `ArcStr` (compile-time known).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_new_string_from_arcstr(
    tag: *const arcstr::ArcStr,
) -> *mut Value {
    unsafe { Box::into_raw(Box::new(Value::String((*tag).clone()))) }
}

/// Drop an owned Value pointer (for variant locals/results).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_drop(v: *mut Value) {
    unsafe { drop(Box::from_raw(v)) }
}

/// Clone a borrowed `Value` (e.g. a variant Local being returned)
/// into a fresh owned `*mut Value`. For composite variants the
/// underlying ArcStr/ValArray refcounts get bumped; for scalar
/// variants this is just a memcpy.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_value_clone(v: *const Value) -> *mut Value {
    unsafe { Box::into_raw(Box::new((*v).clone())) }
}

// ─── Variant consumer ops ────────────────────────────────────────
//
// Variants at runtime are either `Value::String(tag)` for nullary
// or `Value::Array([tag, payload0, ...])` for with-payload. The
// JIT'd code receives a `*const Value` and dispatches on the
// outer Value shape via these helpers.

/// Test whether a variant's runtime tag matches `expected`. Returns
/// 1 (true) or 0 (false). Mirrors the interp's `VariantTagEq`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_tag_eq(
    v: *const Value,
    expected: *const arcstr::ArcStr,
) -> u8 {
    unsafe {
        let exp = &*expected;
        match &*v {
            Value::String(s) => (s.as_str() == exp.as_str()) as u8,
            Value::Array(a) => {
                let tag = a.get_ref_unchecked::<arcstr::ArcStr>(0);
                (tag.as_str() == exp.as_str()) as u8
            }
            _ => 0,
        }
    }
}

/// Read payload slot `payload_idx` of a with-payload variant as
/// `i64`. Slot 0 is the tag; payloads start at slot 1, so the
/// JIT-emitted code passes `payload_idx` as the 0-based payload
/// position (we add 1 here).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_payload_i64(
    v: *const Value,
    payload_idx: usize,
) -> i64 {
    unsafe {
        match &*v {
            Value::Array(a) => a.get_unchecked::<i64>(payload_idx + 1),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_payload_f64(
    v: *const Value,
    payload_idx: usize,
) -> f64 {
    unsafe {
        match &*v {
            Value::Array(a) => a.get_unchecked::<f64>(payload_idx + 1),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_payload_i32(
    v: *const Value,
    payload_idx: usize,
) -> i32 {
    unsafe {
        match &*v {
            Value::Array(a) => a.get_unchecked::<i32>(payload_idx + 1),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_payload_u32(
    v: *const Value,
    payload_idx: usize,
) -> u32 {
    unsafe {
        match &*v {
            Value::Array(a) => a.get_unchecked::<u32>(payload_idx + 1),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_payload_f32(
    v: *const Value,
    payload_idx: usize,
) -> f32 {
    unsafe {
        match &*v {
            Value::Array(a) => a.get_unchecked::<f32>(payload_idx + 1),
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_variant_payload_bool(
    v: *const Value,
    payload_idx: usize,
) -> u8 {
    unsafe {
        match &*v {
            Value::Array(a) => a.get_unchecked::<bool>(payload_idx + 1) as u8,
            _ => std::hint::unreachable_unchecked(),
        }
    }
}

macro_rules! variant_payload_impl {
    ($name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $name(
            v: *const Value,
            payload_idx: usize,
        ) -> $ty {
            unsafe {
                match &*v {
                    Value::Array(a) => a.get_unchecked::<$ty>(payload_idx + 1),
                    _ => std::hint::unreachable_unchecked(),
                }
            }
        }
    };
}

variant_payload_impl!(graphix_variant_payload_i8, i8);
variant_payload_impl!(graphix_variant_payload_i16, i16);
variant_payload_impl!(graphix_variant_payload_u8, u8);
variant_payload_impl!(graphix_variant_payload_u16, u16);
variant_payload_impl!(graphix_variant_payload_u64, u64);

/// Read element `idx` of `arr` as an `i64`. JIT-side counterpart of
/// `KirOp::ArrayGet` / `KirOp::TupleGet` for scalar i64 elements.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i64(
    p: *const ValArray,
    idx: usize,
) -> i64 {
    unsafe { arr(p).get_unchecked::<i64>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_f64(
    p: *const ValArray,
    idx: usize,
) -> f64 {
    unsafe { arr(p).get_unchecked::<f64>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i32(
    p: *const ValArray,
    idx: usize,
) -> i32 {
    unsafe { arr(p).get_unchecked::<i32>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u32(
    p: *const ValArray,
    idx: usize,
) -> u32 {
    unsafe { arr(p).get_unchecked::<u32>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_f32(
    p: *const ValArray,
    idx: usize,
) -> f32 {
    unsafe { arr(p).get_unchecked::<f32>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_bool(
    p: *const ValArray,
    idx: usize,
) -> u8 {
    unsafe { arr(p).get_unchecked::<bool>(idx) as u8 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i8(
    p: *const ValArray,
    idx: usize,
) -> i8 {
    unsafe { arr(p).get_unchecked::<i8>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_i16(
    p: *const ValArray,
    idx: usize,
) -> i16 {
    unsafe { arr(p).get_unchecked::<i16>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u8(
    p: *const ValArray,
    idx: usize,
) -> u8 {
    unsafe { arr(p).get_unchecked::<u8>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u16(
    p: *const ValArray,
    idx: usize,
) -> u16 {
    unsafe { arr(p).get_unchecked::<u16>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_get_u64(
    p: *const ValArray,
    idx: usize,
) -> u64 {
    unsafe { arr(p).get_unchecked::<u64>(idx) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn graphix_valarray_len(p: *const ValArray) -> usize {
    unsafe { arr(p).len() }
}

/// Two-level struct field read: `arr[sorted_idx]` is itself a
/// `Value::Array([name, value])` kv-pair; we read slot 1 (the value)
/// as the named primitive. Mirrors the interp's `KirOp::StructGet`.
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
        pub unsafe extern "C" fn $name(
            p: *const ValArray,
            sorted_idx: usize,
        ) -> $ty {
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
        ("graphix_valarray_finalize", graphix_valarray_finalize as *const u8),
        ("graphix_valarray_clone", graphix_valarray_clone as *const u8),
        ("graphix_valarray_drop", graphix_valarray_drop as *const u8),
        ("graphix_value_new_from_array", graphix_value_new_from_array as *const u8),
        ("graphix_value_new_string_from_arcstr", graphix_value_new_string_from_arcstr as *const u8),
        ("graphix_value_drop", graphix_value_drop as *const u8),
        ("graphix_value_clone", graphix_value_clone as *const u8),
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
    ]
}
