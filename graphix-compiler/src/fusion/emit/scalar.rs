//! Scalar codegen primitives: per-`PrimType` helper-name
//! pickers, element reads, payload widening, and
//! `compile_const`/`compile_bin`/`compile_cmp`/`compile_cast`.

use crate::{
    fusion::kernel_abi::{self, AbiKind, PrimType},
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

/// Map an element [`PrimType`] to the `graphix_valarray_get_<T>` helper.
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

/// The helper that reads a Graphix-minted abstract value's payload
/// (`.0`) at the representation's shape.
pub(super) fn abstract_read_helper(rep: &Type) -> Result<&'static str> {
    Ok(match kernel_abi::abi_kind(rep) {
        Some(AbiKind::Scalar(p)) => match p {
            PrimType::I8 => "graphix_abstract_get_i8",
            PrimType::I16 => "graphix_abstract_get_i16",
            PrimType::I32 => "graphix_abstract_get_i32",
            PrimType::I64 => "graphix_abstract_get_i64",
            PrimType::U8 => "graphix_abstract_get_u8",
            PrimType::U16 => "graphix_abstract_get_u16",
            PrimType::U32 => "graphix_abstract_get_u32",
            PrimType::U64 => "graphix_abstract_get_u64",
            PrimType::F32 => "graphix_abstract_get_f32",
            PrimType::F64 => "graphix_abstract_get_f64",
            PrimType::Bool => "graphix_abstract_get_bool",
        },
        Some(AbiKind::String) => "graphix_abstract_get_arcstr",
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            "graphix_abstract_get_array"
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            "graphix_abstract_get_value"
        }
        other => {
            return Err(anyhow!(
                "emit_clif: abstract payload of shape {other:?} — not representable"
            ));
        }
    })
}

/// Map a struct field [`PrimType`] to the `graphix_struct_get_<T>` helper.
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

/// Map an element [`Type`] to its element-read helper by ABI kind.
/// `struct_access` picks the `struct_get_*` (kv-pair read) family over
/// the flat `valarray_get_*` family.
pub(super) fn element_read_helper(
    elem: &Type,
    struct_access: bool,
) -> Result<&'static str> {
    Ok(match kernel_abi::abi_kind(elem) {
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

/// Emit an element read `arr_ptr[idx]` (or struct field) of element
/// type `elem`. The result is owned (fresh box or refcount-bumped clone).
pub(super) fn compile_element_read(
    b: &mut FunctionBuilder,
    arr_ptr: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    struct_access: bool,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    let helper_name = element_read_helper(elem, struct_access)?;
    let helper = ctx
        .helper_refs
        .get(helper_name)
        .ok_or_else(|| anyhow!("missing JIT helper `{helper_name}`"))?;
    let call = b.ins().call(helper, &[arr_ptr, idx_val]);
    if kernel_abi::is_value_shape(elem) {
        let (r0, r1) = {
            let r = b.inst_results(call);
            (r[0], r[1])
        };
        Ok(CompiledExpr::new(r0, r1))
    } else {
        let r0 = b.inst_results(call)[0];
        // An element read is never tainted; the disc only carries the kind.
        let disc = match kernel_abi::abi_kind(elem) {
            Some(AbiKind::Scalar(p)) => scalar_disc(b, p),
            Some(AbiKind::String) => b.ins().iconst(types::I64, value_disc::STRING),
            _ => b.ins().iconst(types::I64, value_disc::ARRAY),
        };
        Ok(CompiledExpr::new(disc, r0))
    }
}

/// Widen an integer CLIF value to the i64 index the helpers expect.
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

/// Promote a scalar CLIF value to the 8-byte payload word of a Value,
/// following `pack_value_to_u64`: signed ints sign-extend, unsigned
/// ints and bool zero-extend, floats bitcast through their integer mirror.
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
        PrimType::I8 | PrimType::I16 | PrimType::I32 => b.ins().sextend(types::I64, v),
        PrimType::U8 | PrimType::U16 | PrimType::U32 | PrimType::Bool => {
            b.ins().uextend(types::I64, v)
        }
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
/// scalar of `p` into a string buffer.
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

/// Lower a scalar [`Value`] constant of `prim` to a CLIF constant.
/// `v` must be the matching scalar (`Z*`/`V*` accepted for their
/// fixed-width prim); anything else is `Err`, which de-fuses.
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

/// A zero / false constant of `p`: the well-typed sentinel for a
/// return whose value is never observed.
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
        match op {
            BinOp::Add => b.ins().fadd(l, r),
            BinOp::Sub => b.ins().fsub(l, r),
            BinOp::Mul => b.ins().fmul(l, r),
            BinOp::Div => b.ins().fdiv(l, r),
            BinOp::Mod => {
                // Cranelift has no `frem`; refuse so the kernel node-walks.
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
        // Graphix's total order (`Value::partial_cmp`): NaN == NaN and
        // NaN sorts below every non-NaN. `fcmp Unordered x x` tests NaN;
        // fcmp yields an I8 0/1, so `bxor_imm(v, 1)` is NOT.
        let l_nan = b.ins().fcmp(FloatCC::Unordered, l, l);
        let r_nan = b.ins().fcmp(FloatCC::Unordered, r, r);
        let not_l_nan = b.ins().bxor_imm(l_nan, 1);
        let not_r_nan = b.ins().bxor_imm(r_nan, 1);
        let ord_eq = b.ins().fcmp(FloatCC::Equal, l, r);
        let both_nan = b.ins().band(l_nan, r_nan);
        let eq = b.ins().bor(ord_eq, both_nan);
        let ord_lt = b.ins().fcmp(FloatCC::LessThan, l, r);
        let nan_lt = b.ins().band(l_nan, not_r_nan);
        let lt = b.ins().bor(ord_lt, nan_lt);
        let ord_gt = b.ins().fcmp(FloatCC::GreaterThan, l, r);
        let nan_gt = b.ins().band(r_nan, not_l_nan);
        let gt = b.ins().bor(ord_gt, nan_gt);
        match op {
            CmpOp::Eq => eq,
            CmpOp::Ne => b.ins().bxor_imm(eq, 1),
            CmpOp::Lt => lt,
            CmpOp::Gt => gt,
            CmpOp::Lte => b.ins().bxor_imm(gt, 1),
            CmpOp::Gte => b.ins().bxor_imm(lt, 1),
        }
    } else {
        let cc = if operand_typ.is_signed() || operand_typ == PrimType::Bool {
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
            v
        }
    } else if src.is_integer() && dst.is_float() {
        // x64 has no fcvt encoding for an i8/i16 source.
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
        // Saturate like Rust `as`. x64 encodes fcvt only to i32/i64, so
        // narrow targets convert at i32, clamp to the target's range,
        // then reduce.
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
        unreachable!("compile_cast: bool casts should be rejected before emission");
    }
}

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

/// Width in bytes of the underlying CLIF type.
pub(super) fn clif_size(p: PrimType) -> u32 {
    match p {
        PrimType::I8 | PrimType::U8 | PrimType::Bool => 1,
        PrimType::I16 | PrimType::U16 => 2,
        PrimType::I32 | PrimType::U32 | PrimType::F32 => 4,
        PrimType::I64 | PrimType::U64 | PrimType::F64 => 8,
    }
}

/// Narrow a `pack_value_to_u64` payload word to a CLIF value of prim
/// type `p`; floats bitcast from their same-width integer.
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
