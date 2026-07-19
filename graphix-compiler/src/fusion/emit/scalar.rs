//! Scalar codegen primitives: per-`PrimType` helper-name
//! pickers, element reads, payload widening, and
//! `compile_const`/`compile_bin`/`compile_cmp`/`compile_cast`.

use crate::{
    fusion::kernel_abi::{self, AbiKind, AbstractRegistry, PrimType},
    node::op::{BinOp, CmpOp},
    typ::Type,
};
use anyhow::{Result, anyhow};
use cranelift_codegen::ir::{
    InstBuilder, MemFlags, Type as ClifType, Value as ClifValue,
    condcodes::{FloatCC, IntCC},
    types,
};
use cranelift_frontend::FunctionBuilder;
use netidx_value::Value;

use super::{
    abi::{CompiledExpr, scalar_disc, value_disc},
    lower::LowerCtx,
};

/// Map a [`PrimType`] to the `graphix_value_buf_push_<T>` helper.
pub(super) fn value_buf_push_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_value_buf_push_i8",
        PrimType::I16 => "graphix_value_buf_push_i16",
        PrimType::I32 => "graphix_value_buf_push_i32",
        PrimType::I64 => "graphix_value_buf_push_i64",
        PrimType::U8 => "graphix_value_buf_push_u8",
        PrimType::U16 => "graphix_value_buf_push_u16",
        PrimType::U32 => "graphix_value_buf_push_u32",
        PrimType::U64 => "graphix_value_buf_push_u64",
        PrimType::F32 => "graphix_value_buf_push_f32",
        PrimType::F64 => "graphix_value_buf_push_f64",
        PrimType::Bool => "graphix_value_buf_push_bool",
    })
}

/// Map an element [`PrimType`] to the `graphix_valarray_get_<T>`
/// helper symbol name. Used by ArrayGet / TupleGet lowering.
pub(super) fn valarray_get_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_valarray_get_i8",
        PrimType::I16 => "graphix_valarray_get_i16",
        PrimType::I32 => "graphix_valarray_get_i32",
        PrimType::I64 => "graphix_valarray_get_i64",
        PrimType::U8 => "graphix_valarray_get_u8",
        PrimType::U16 => "graphix_valarray_get_u16",
        PrimType::U32 => "graphix_valarray_get_u32",
        PrimType::U64 => "graphix_valarray_get_u64",
        PrimType::F32 => "graphix_valarray_get_f32",
        PrimType::F64 => "graphix_valarray_get_f64",
        PrimType::Bool => "graphix_valarray_get_bool",
    })
}

/// Map a struct field [`PrimType`] to the `graphix_struct_get_<T>`
/// helper symbol name.
pub(super) fn struct_get_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_struct_get_i8",
        PrimType::I16 => "graphix_struct_get_i16",
        PrimType::I32 => "graphix_struct_get_i32",
        PrimType::I64 => "graphix_struct_get_i64",
        PrimType::U8 => "graphix_struct_get_u8",
        PrimType::U16 => "graphix_struct_get_u16",
        PrimType::U32 => "graphix_struct_get_u32",
        PrimType::U64 => "graphix_struct_get_u64",
        PrimType::F32 => "graphix_struct_get_f32",
        PrimType::F64 => "graphix_struct_get_f64",
        PrimType::Bool => "graphix_struct_get_bool",
    })
}

/// Map an element [`Type`] to its element-read helper symbol —
/// primitive (`get_<prim>`), String (`get_arcstr`), composite
/// (`get_array`, owned ValArray bits), or value-shape (`get_value`, a
/// two-word `Value`). `struct_access` picks the `struct_get_*` (two-
/// level kv-pair read) family over the flat `valarray_get_*` family.
pub(super) fn element_read_helper(
    reg: &AbstractRegistry,
    elem: &Type,
    struct_access: bool,
) -> Result<&'static str> {
    Ok(match kernel_abi::abi_kind(reg, elem) {
        Some(AbiKind::Scalar(p)) => {
            if struct_access {
                struct_get_helper(p)?
            } else {
                valarray_get_helper(p)?
            }
        }
        Some(AbiKind::String) => {
            if struct_access {
                "graphix_struct_get_arcstr"
            } else {
                "graphix_valarray_get_arcstr"
            }
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            if struct_access {
                "graphix_struct_get_array"
            } else {
                "graphix_valarray_get_array"
            }
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            if struct_access {
                "graphix_struct_get_value"
            } else {
                "graphix_valarray_get_value"
            }
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => {
            return Err(anyhow!(
                "element read of Unit/Null/non-fusable slot — emission is malformed"
            ));
        }
    })
}

/// Emit an element read: `arr_ptr[idx]` (or struct field) of the given
/// element `Type`, dispatching to the right `..._get_*` helper. The
/// result is OWNED (fresh box / refcount-bumped clone). Returns a
/// `CompiledExpr` (disc + payload) whose disc tag marks a value-shape
/// element (two-register Value) vs a scalar / string / composite-pointer
/// element — one routine serves both the scalar and value-shape reads.
pub(super) fn compile_element_read(
    b: &mut FunctionBuilder,
    arr_ptr: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    struct_access: bool,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    let helper_name = element_read_helper(ctx.registry, elem, struct_access)?;
    let helper = ctx
        .helper_refs
        .get(helper_name)
        .ok_or_else(|| anyhow!("missing JIT helper `{helper_name}`"))?;
    let call = b.ins().call(helper, &[arr_ptr, idx_val]);
    if kernel_abi::is_value_shape(ctx.registry, elem) {
        // Value-shape element read returns two words (disc, payload).
        let (r0, r1) = {
            let r = b.inst_results(call);
            (r[0], r[1])
        };
        Ok(CompiledExpr::new(r0, r1))
    } else {
        // Scalar / string / composite element read returns one word.
        let r0 = b.inst_results(call)[0];
        // The element read is untainted (a valid array element); the disc
        // is a const taint-carrier matching the element kind.
        let disc = match kernel_abi::abi_kind(ctx.registry, elem) {
            Some(AbiKind::Scalar(p)) => scalar_disc(b, p),
            Some(AbiKind::String) => b.ins().iconst(types::I64, value_disc::STRING),
            _ => b.ins().iconst(types::I64, value_disc::ARRAY),
        };
        Ok(CompiledExpr::new(disc, r0))
    }
}

/// Widen a CLIF value to i64. Helpers expect a usize index; if the
/// caller's index expression was narrower (e.g. `i32` from a
/// `cast`), we zero/sign extend here.
pub(super) fn widen_to_i64(
    b: &mut FunctionBuilder,
    v: ClifValue,
    p: PrimType,
) -> Result<ClifValue> {
    Ok(match p {
        PrimType::I64 | PrimType::U64 => v,
        PrimType::I8 | PrimType::I16 | PrimType::I32 => b.ins().sextend(types::I64, v),
        PrimType::U8 | PrimType::U16 | PrimType::U32 | PrimType::Bool => {
            b.ins().uextend(types::I64, v)
        }
        PrimType::F32 | PrimType::F64 => {
            return Err(anyhow::anyhow!(
                "widen_to_i64: float index — emission malformed"
            ));
        }
    })
}

/// Promote a scalar CLIF value to the 8-byte payload word of a
/// `repr(u64)` Value. Integers smaller than i64 get zero/sign-
/// extended (we use unsigned `uextend` because the payload's
/// interpretation is fixed by the discriminant; truncation back
/// preserves bits). f32/f64 bitcast through their integer mirror.
// CR claude for eric: `pack_value_to_u64` (the Rust twin) SIGN-extends
// signed prims, so a kernel-produced payload word differs from a
// runtime-packed one in the upper bytes. Harmless today (every
// consumer truncates), but it breaks the design doc's "the payload
// word IS the Value encoding" invariant — either sextend signed prims
// here or amend design/unified_value_abi.md. See review doc C1.
pub(super) fn scalar_to_payload_i64(
    b: &mut FunctionBuilder,
    p: PrimType,
    v: ClifValue,
) -> ClifValue {
    match p {
        PrimType::I64 | PrimType::U64 => v,
        PrimType::I32 | PrimType::U32 => b.ins().uextend(types::I64, v),
        PrimType::I16 | PrimType::U16 => b.ins().uextend(types::I64, v),
        PrimType::I8 | PrimType::U8 | PrimType::Bool => b.ins().uextend(types::I64, v),
        PrimType::F32 => {
            let bits = b.ins().bitcast(
                types::I32,
                cranelift_codegen::ir::MemFlags::new()
                    .with_endianness(cranelift_codegen::ir::Endianness::Little),
                v,
            );
            b.ins().uextend(types::I64, bits)
        }
        PrimType::F64 => b.ins().bitcast(
            types::I64,
            cranelift_codegen::ir::MemFlags::new()
                .with_endianness(cranelift_codegen::ir::Endianness::Little),
            v,
        ),
    }
}

/// The `graphix_string_buf_push_*` helper that Display-renders a
/// scalar of `p` into a Concat / string-interpolate buffer. Used by
/// [`emit_string_interpolate_node`].
pub(super) fn string_buf_push_helper(p: PrimType) -> &'static str {
    match p {
        PrimType::I64 => "graphix_string_buf_push_i64",
        PrimType::U64 => "graphix_string_buf_push_u64",
        PrimType::I32 => "graphix_string_buf_push_i32",
        PrimType::U32 => "graphix_string_buf_push_u32",
        PrimType::I16 => "graphix_string_buf_push_i16",
        PrimType::U16 => "graphix_string_buf_push_u16",
        PrimType::I8 => "graphix_string_buf_push_i8",
        PrimType::U8 => "graphix_string_buf_push_u8",
        PrimType::F64 => "graphix_string_buf_push_f64",
        PrimType::F32 => "graphix_string_buf_push_f32",
        PrimType::Bool => "graphix_string_buf_push_bool",
    }
}

/// Lower a scalar [`Value`] constant of the given `prim` to a CLIF
/// `iconst`/`f32const`/`f64const`. `prim` comes from the constant's
/// frozen type; `v` must be the matching scalar (`Z*`/`V*`
/// accepted for their fixed-width prim). Returns `Err` otherwise (a
/// malformed kernel — de-fuses to the node-walk instead of panicking).
pub(super) fn compile_const(
    b: &mut FunctionBuilder,
    v: &Value,
    prim: PrimType,
) -> Result<ClifValue> {
    macro_rules! bad {
        () => {
            return Err(anyhow::anyhow!("compile_const: {v:?} isn't a {prim:?} scalar"))
        };
    }
    Ok(match prim {
        PrimType::I8 => match v {
            Value::I8(x) => b.ins().iconst(types::I8, *x as i64),
            _ => bad!(),
        },
        PrimType::I16 => match v {
            Value::I16(x) => b.ins().iconst(types::I16, *x as i64),
            _ => bad!(),
        },
        PrimType::I32 => match v {
            Value::I32(x) | Value::Z32(x) => b.ins().iconst(types::I32, *x as i64),
            _ => bad!(),
        },
        PrimType::I64 => match v {
            Value::I64(x) | Value::Z64(x) => b.ins().iconst(types::I64, *x),
            _ => bad!(),
        },
        PrimType::U8 => match v {
            Value::U8(x) => b.ins().iconst(types::I8, *x as i64),
            _ => bad!(),
        },
        PrimType::U16 => match v {
            Value::U16(x) => b.ins().iconst(types::I16, *x as i64),
            _ => bad!(),
        },
        PrimType::U32 => match v {
            Value::U32(x) | Value::V32(x) => b.ins().iconst(types::I32, *x as i64),
            _ => bad!(),
        },
        PrimType::U64 => match v {
            Value::U64(x) | Value::V64(x) => b.ins().iconst(types::I64, *x as i64),
            _ => bad!(),
        },
        PrimType::F32 => match v {
            Value::F32(x) => b.ins().f32const(*x),
            _ => bad!(),
        },
        PrimType::F64 => match v {
            Value::F64(x) => b.ins().f64const(*x),
            _ => bad!(),
        },
        PrimType::Bool => match v {
            Value::Bool(true) => b.ins().iconst(types::I8, 1),
            Value::Bool(false) => b.ins().iconst(types::I8, 0),
            _ => bad!(),
        },
    })
}

/// A zero / false constant of the given prim type. Used for the
/// `pending_exit` block's sentinel return value (never observed —
/// `Kernel::update` discards the result on the pending path — but
/// CLIF needs a well-typed value of the right width).
pub(super) fn zero_const(b: &mut FunctionBuilder, p: PrimType) -> ClifValue {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => b.ins().iconst(types::I8, 0),
        PrimType::I16 | PrimType::U16 => b.ins().iconst(types::I16, 0),
        PrimType::I32 | PrimType::U32 => b.ins().iconst(types::I32, 0),
        PrimType::I64 | PrimType::U64 => b.ins().iconst(types::I64, 0),
        PrimType::F32 => b.ins().f32const(0.0),
        PrimType::F64 => b.ins().f64const(0.0),
    }
}

pub(super) fn compile_bin(
    b: &mut FunctionBuilder,
    op: BinOp,
    typ: PrimType,
    l: ClifValue,
    r: ClifValue,
) -> Result<ClifValue> {
    Ok(if typ.is_integer() {
        match op {
            BinOp::Add => b.ins().iadd(l, r),
            BinOp::Sub => b.ins().isub(l, r),
            BinOp::Mul => b.ins().imul(l, r),
            BinOp::Div => {
                if typ.is_signed() {
                    b.ins().sdiv(l, r)
                } else {
                    b.ins().udiv(l, r)
                }
            }
            BinOp::Mod => {
                if typ.is_signed() {
                    b.ins().srem(l, r)
                } else {
                    b.ins().urem(l, r)
                }
            }
        }
    } else {
        // float
        match op {
            BinOp::Add => b.ins().fadd(l, r),
            BinOp::Sub => b.ins().fsub(l, r),
            BinOp::Mul => b.ins().fmul(l, r),
            BinOp::Div => b.ins().fdiv(l, r),
            BinOp::Mod => {
                // Cranelift has no `frem` — float `%` would need an fmod
                // libcall, and `compile_bin` has no module handle to emit
                // one here. Bail so the kernel falls back to the
                // interpreter (which computes float `%` correctly),
                // instead of emitting a runtime trap that crashed the
                // whole runtime. (The trap was a latent crash found by
                // graphix-fuzz on `f64:7.0 % f64:3.0`; wiring the fmod
                // libcall so it JITs is a follow-up.)
                let _ = (l, r);
                return Err(anyhow!(
                    "JIT: float modulo unsupported (no cranelift frem); \
                     kernel runs on the interpreter"
                ));
            }
        }
    })
}

pub(super) fn compile_cmp(
    b: &mut FunctionBuilder,
    op: CmpOp,
    operand_typ: PrimType,
    l: ClifValue,
    r: ClifValue,
) -> ClifValue {
    if operand_typ.is_float() {
        // Float comparison uses graphix's TOTAL order, matching
        // `Value::partial_cmp` / the node-walk: NaN == NaN, and NaN sorts
        // below every non-NaN value. `FloatCC::Equal`/`LessThan`/etc. are
        // the IEEE *ordered* predicates (any NaN operand → false); a NaN
        // is the only value unordered with itself, so `fcmp Unordered x x`
        // tests "x is NaN". fcmp yields an I8 0/1, so `bxor_imm(v, 1)` is
        // logical NOT. We build `eq` and `lt` under the total order and
        // derive the rest.
        let l_nan = b.ins().fcmp(FloatCC::Unordered, l, l);
        let r_nan = b.ins().fcmp(FloatCC::Unordered, r, r);
        let not_l_nan = b.ins().bxor_imm(l_nan, 1);
        let not_r_nan = b.ins().bxor_imm(r_nan, 1);
        // eq: ordered-equal, OR both NaN.
        let ord_eq = b.ins().fcmp(FloatCC::Equal, l, r);
        let both_nan = b.ins().band(l_nan, r_nan);
        let eq = b.ins().bor(ord_eq, both_nan);
        // lt: ordered IEEE l<r, OR (l is NaN and r is not) since NaN is least.
        let ord_lt = b.ins().fcmp(FloatCC::LessThan, l, r);
        let nan_lt = b.ins().band(l_nan, not_r_nan);
        let lt = b.ins().bor(ord_lt, nan_lt);
        // gt: ordered IEEE l>r, OR (r is NaN and l is not).
        let ord_gt = b.ins().fcmp(FloatCC::GreaterThan, l, r);
        let nan_gt = b.ins().band(r_nan, not_l_nan);
        let gt = b.ins().bor(ord_gt, nan_gt);
        match op {
            CmpOp::Eq => eq,
            CmpOp::Ne => b.ins().bxor_imm(eq, 1),
            CmpOp::Lt => lt,
            CmpOp::Gt => gt,
            CmpOp::Lte => b.ins().bxor_imm(gt, 1), // not gt
            CmpOp::Gte => b.ins().bxor_imm(lt, 1), // not lt
        }
    } else {
        let cc = if operand_typ.is_signed() || operand_typ == PrimType::Bool {
            // Bool comparisons are fine via signed (or unsigned) — but
            // signed eq/ne behaves identically on an I8 holding 0/1.
            match op {
                CmpOp::Eq => IntCC::Equal,
                CmpOp::Ne => IntCC::NotEqual,
                CmpOp::Lt => IntCC::SignedLessThan,
                CmpOp::Gt => IntCC::SignedGreaterThan,
                CmpOp::Lte => IntCC::SignedLessThanOrEqual,
                CmpOp::Gte => IntCC::SignedGreaterThanOrEqual,
            }
        } else {
            match op {
                CmpOp::Eq => IntCC::Equal,
                CmpOp::Ne => IntCC::NotEqual,
                CmpOp::Lt => IntCC::UnsignedLessThan,
                CmpOp::Gt => IntCC::UnsignedGreaterThan,
                CmpOp::Lte => IntCC::UnsignedLessThanOrEqual,
                CmpOp::Gte => IntCC::UnsignedGreaterThanOrEqual,
            }
        };
        b.ins().icmp(cc, l, r)
    }
}

pub(super) fn compile_cast(
    b: &mut FunctionBuilder,
    v: ClifValue,
    src: PrimType,
    dst: PrimType,
) -> ClifValue {
    if prim_to_clif(src) == prim_to_clif(dst) && src.is_float() == dst.is_float() {
        // Same underlying CLIF type and same float/int family — no-op.
        return v;
    }
    let dst_ty = prim_to_clif(dst);
    let src_size = clif_size(src);
    let dst_size = clif_size(dst);
    if src.is_integer() && dst.is_integer() {
        if src_size < dst_size {
            if src.is_signed() {
                b.ins().sextend(dst_ty, v)
            } else {
                b.ins().uextend(dst_ty, v)
            }
        } else if src_size > dst_size {
            b.ins().ireduce(dst_ty, v)
        } else {
            // Same size — bit reinterpretation only.
            v
        }
    } else if src.is_integer() && dst.is_float() {
        // x64 int→float converts need a 32/64-bit source — widen a
        // narrow int first (the backend has no encoding for an i8/i16
        // fcvt source; sibling of the narrow fcvt-to-int unreachable
        // below).
        let v = if src_size < 4 {
            if src.is_signed() {
                b.ins().sextend(types::I32, v)
            } else {
                b.ins().uextend(types::I32, v)
            }
        } else {
            v
        };
        if src.is_signed() {
            b.ins().fcvt_from_sint(dst_ty, v)
        } else {
            b.ins().fcvt_from_uint(dst_ty, v)
        }
    } else if src.is_float() && dst.is_integer() {
        // Saturating to match Rust `as` semantics on out-of-range.
        // The x64 backend can only encode fcvt to i32/i64
        // (`fcvt_to_uint_sat.i8` hit cranelift's emit unreachable —
        // jit_generated_sweep, `cast<u8>(f64)$`), so narrow targets
        // convert at i32, clamp to the TARGET's range (the i32-width
        // saturation alone would wrap 300 → u8:44 where Rust `as` —
        // and the node-walk's `Value::cast` — clamp to 255), then
        // reduce.
        if dst_size < 4 {
            if dst.is_signed() {
                let wide = b.ins().fcvt_to_sint_sat(types::I32, v);
                let (lo, hi) = if dst_size == 1 { (-128, 127) } else { (-32768, 32767) };
                let lo = b.ins().iconst(types::I32, lo);
                let hi = b.ins().iconst(types::I32, hi);
                let clamped = b.ins().smax(wide, lo);
                let clamped = b.ins().smin(clamped, hi);
                b.ins().ireduce(dst_ty, clamped)
            } else {
                let wide = b.ins().fcvt_to_uint_sat(types::I32, v);
                let hi = if dst_size == 1 { 255 } else { 65535 };
                let hi = b.ins().iconst(types::I32, hi);
                let clamped = b.ins().umin(wide, hi);
                b.ins().ireduce(dst_ty, clamped)
            }
        } else if dst.is_signed() {
            b.ins().fcvt_to_sint_sat(dst_ty, v)
        } else {
            b.ins().fcvt_to_uint_sat(dst_ty, v)
        }
    } else if src.is_float() && dst.is_float() {
        if src_size < dst_size {
            b.ins().fpromote(dst_ty, v)
        } else {
            b.ins().fdemote(dst_ty, v)
        }
    } else {
        // bool ↔ integer/float — `emit_cast_node` refuses these
        // before emitting; reaching this branch means a caller
        // bypassed that gate.
        unreachable!("compile_cast: bool casts should be rejected before emission");
    }
}

// ─── Type plumbing ───────────────────────────────────────────────

pub(super) fn prim_to_clif(p: PrimType) -> ClifType {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => types::I8,
        PrimType::I16 | PrimType::U16 => types::I16,
        PrimType::I32 | PrimType::U32 => types::I32,
        PrimType::I64 | PrimType::U64 => types::I64,
        PrimType::F32 => types::F32,
        PrimType::F64 => types::F64,
    }
}

/// Width in bytes of the underlying CLIF type — used to pick between
/// extend / reduce / promote / demote in casts.
pub(super) fn clif_size(p: PrimType) -> u32 {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => 1,
        PrimType::I16 | PrimType::U16 => 2,
        PrimType::I32 | PrimType::U32 | PrimType::F32 => 4,
        PrimType::I64 | PrimType::U64 | PrimType::F64 => 8,
    }
}

/// Cast a u64 (typically the raw bits of a scalar primitive packed
/// via [`pack_value_to_u64`] or returned from `graphix_dyncall`) to a
/// CLIF value of the target prim type. Integer truncations use
/// `ireduce`; floats route through a same-width integer then
/// `bitcast`.
pub(super) fn cast_u64_to_prim(
    b: &mut FunctionBuilder,
    raw: ClifValue,
    p: PrimType,
) -> ClifValue {
    match p {
        PrimType::I64 | PrimType::U64 => raw,
        PrimType::I32 | PrimType::U32 => b.ins().ireduce(types::I32, raw),
        PrimType::I16 | PrimType::U16 => b.ins().ireduce(types::I16, raw),
        PrimType::I8 | PrimType::U8 | PrimType::Bool => b.ins().ireduce(types::I8, raw),
        PrimType::F32 => {
            let bits32 = b.ins().ireduce(types::I32, raw);
            b.ins().bitcast(
                types::F32,
                MemFlags::new()
                    .with_endianness(cranelift_codegen::ir::Endianness::Little),
                bits32,
            )
        }
        PrimType::F64 => b.ins().bitcast(
            types::F64,
            MemFlags::new().with_endianness(cranelift_codegen::ir::Endianness::Little),
            raw,
        ),
    }
}

pub(super) fn variant_payload_helper(p: PrimType) -> Result<&'static str> {
    Ok(match p {
        PrimType::I8 => "graphix_variant_payload_i8",
        PrimType::I16 => "graphix_variant_payload_i16",
        PrimType::I32 => "graphix_variant_payload_i32",
        PrimType::I64 => "graphix_variant_payload_i64",
        PrimType::U8 => "graphix_variant_payload_u8",
        PrimType::U16 => "graphix_variant_payload_u16",
        PrimType::U32 => "graphix_variant_payload_u32",
        PrimType::U64 => "graphix_variant_payload_u64",
        PrimType::F32 => "graphix_variant_payload_f32",
        PrimType::F64 => "graphix_variant_payload_f64",
        PrimType::Bool => "graphix_variant_payload_bool",
    })
}
