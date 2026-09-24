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
    jit::pack_value_to_u64,
    lower::LowerCtx,
};

/// The eleven members of a per-prim helper family `<prefix><prim>`, in
/// [`PrimType`] declaration order (pinned by `helper_families_follow_prims`).
macro_rules! prim_family {
    ($prefix:literal) => {
        [
            concat!($prefix, "i8"),
            concat!($prefix, "i16"),
            concat!($prefix, "i32"),
            concat!($prefix, "i64"),
            concat!($prefix, "u8"),
            concat!($prefix, "u16"),
            concat!($prefix, "u32"),
            concat!($prefix, "u64"),
            concat!($prefix, "f32"),
            concat!($prefix, "f64"),
            concat!($prefix, "bool"),
        ]
    };
}

const VALUE_BUF_PUSH: [&str; 11] = prim_family!("graphix_value_buf_push_");
const VALARRAY_GET: [&str; 11] = prim_family!("graphix_valarray_get_");
const ABSTRACT_GET: [&str; 11] = prim_family!("graphix_abstract_get_");
const STRUCT_GET: [&str; 11] = prim_family!("graphix_struct_get_");
const STRING_BUF_PUSH: [&str; 11] = prim_family!("graphix_string_buf_push_");
const VARIANT_PAYLOAD: [&str; 11] = prim_family!("graphix_variant_payload_");

/// The `graphix_value_buf_push_<T>` helper for a [`PrimType`].
pub(super) fn value_buf_push_helper(p: PrimType) -> &'static str {
    VALUE_BUF_PUSH[p as usize]
}

/// The `graphix_valarray_get_<T>` helper for an element [`PrimType`].
pub(super) fn valarray_get_helper(p: PrimType) -> &'static str {
    VALARRAY_GET[p as usize]
}

/// The helper that reads a Graphix-minted abstract value's payload
/// (`.0`) at the representation's shape.
pub(super) fn abstract_read_helper(rep: &Type) -> Result<&'static str> {
    Ok(match kernel_abi::abi_kind(rep) {
        Some(AbiKind::Scalar(p)) => ABSTRACT_GET[p as usize],
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

/// The `graphix_struct_get_<T>` helper for a struct field [`PrimType`].
pub(super) fn struct_get_helper(p: PrimType) -> &'static str {
    STRUCT_GET[p as usize]
}

/// Which family of element-read helpers an accessor uses.
#[derive(Clone, Copy)]
pub(super) enum ElementRead {
    /// The flat `valarray_get_*` family.
    ArrayIndex,
    /// The `struct_get_*` family, which reads the value of a
    /// `[name, value]` pair.
    StructField,
}

/// An element [`Type`]'s read helper by ABI kind, in the `read` family.
pub(super) fn element_read_helper(
    elem: &Type,
    read: ElementRead,
) -> Result<&'static str> {
    let struct_access = matches!(read, ElementRead::StructField);
    Ok(match kernel_abi::abi_kind(elem) {
        Some(AbiKind::Scalar(p)) => {
            if struct_access {
                struct_get_helper(p)
            } else {
                valarray_get_helper(p)
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
/// type `elem`. The result is owned: a scalar, or a refcount-bumped
/// clone.
pub(super) fn compile_element_read(
    b: &mut FunctionBuilder,
    arr_ptr: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    read: ElementRead,
    ctx: &LowerCtx,
) -> Result<CompiledExpr> {
    let helper_name = element_read_helper(elem, read)?;
    let helper = ctx.helper(b, helper_name)?;
    let call = b.ins().call(helper, &[arr_ptr, idx_val]);
    if kernel_abi::is_value_shape(elem) {
        let (r0, r1) = {
            let r = b.inst_results(call);
            (r[0], r[1])
        };
        Ok(CompiledExpr::new(r0, r1))
    } else {
        let r0 = b.inst_results(call)[0];
        Ok(CompiledExpr::new(kind_disc(b, elem), r0))
    }
}

/// The clean disc of a value of `t`'s kind: an element read is never
/// tainted, so its disc only carries the kind.
pub(super) fn kind_disc(b: &mut FunctionBuilder, t: &Type) -> ClifValue {
    match kernel_abi::abi_kind(t) {
        Some(AbiKind::Scalar(p)) => scalar_disc(b, p),
        Some(AbiKind::String) => b.ins().iconst(types::I64, value_disc::STRING),
        _ => b.ins().iconst(types::I64, value_disc::ARRAY),
    }
}

/// Widen an integer CLIF value to the i64 index the helpers expect, as
/// `node::array::index_i64` does: a u64 above `i64::MAX` saturates.
pub(super) fn widen_to_i64(
    b: &mut FunctionBuilder,
    v: ClifValue,
    p: PrimType,
) -> Result<ClifValue> {
    Ok(match p {
        PrimType::I64 => v,
        PrimType::U64 => {
            let max = b.ins().iconst(types::I64, i64::MAX);
            b.ins().umin(v, max)
        }
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
    STRING_BUF_PUSH[p as usize]
}

/// Lower a scalar [`Value`] constant of `prim` to a CLIF constant.
/// `v` must be the matching scalar (`Z*`/`V*` accepted for their
/// fixed-width prim); anything else is `Err`, which de-fuses.
pub(super) fn compile_const(
    b: &mut FunctionBuilder,
    v: &Value,
    prim: PrimType,
) -> Result<ClifValue> {
    let bits = pack_value_to_u64(v, prim)
        .ok_or_else(|| anyhow!("compile_const: {v:?} isn't a {prim:?} scalar"))?;
    Ok(match prim {
        PrimType::F32 => b.ins().f32const(f32::from_bits(bits as u32)),
        PrimType::F64 => b.ins().f64const(f64::from_bits(bits)),
        p => b.ins().iconst(prim_to_clif(p), bits as i64),
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
                return Err(anyhow!("compile_bin: a float `%` emits through its helper"));
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

/// x64 encodes fcvt only to i32/i64, so a narrow target converts at
/// i32, clamps to the target's range, then reduces.
fn fcvt_sat_narrow(
    b: &mut FunctionBuilder,
    v: ClifValue,
    dst: PrimType,
    dst_ty: ClifType,
    dst_size: u32,
) -> ClifValue {
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
        // Saturate like Rust `as`.
        if dst_size < 4 {
            fcvt_sat_narrow(b, v, dst, dst_ty, dst_size)
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

/// The `graphix_variant_payload_<T>` helper for a payload [`PrimType`].
pub(super) fn variant_payload_helper(p: PrimType) -> &'static str {
    VARIANT_PAYLOAD[p as usize]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn helper_families_follow_prims() {
        use PrimType::*;
        for p in [I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, Bool] {
            let suffix = format!("_{}", format!("{p:?}").to_lowercase());
            for family in [
                VALUE_BUF_PUSH,
                VALARRAY_GET,
                ABSTRACT_GET,
                STRUCT_GET,
                STRING_BUF_PUSH,
                VARIANT_PAYLOAD,
            ] {
                assert!(
                    family[p as usize].ends_with(&suffix),
                    "{p:?}: {}",
                    family[p as usize]
                );
            }
        }
    }
}
