//! Per-node value emitters: constants, refs, scalar operators,
//! casts, connect, string interpolation, and the composite
//! producers/accessors (tuple/struct/variant/array/map).

use crate::{
    BindId, Node, NodeView, Rt, UserEvent,
    expr::{Expr, ExprId},
    fusion::{
        kernel_abi::{self, AbiKind, PrimType},
        lowering::{self},
    },
    node::op::{ArithOp, BoolOp, CmpOp},
    rep,
    typ::Type,
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    BlockArg, InstBuilder, Value as ClifValue, condcodes::IntCC, types,
};
use netidx_value::Value;
use smallvec::{SmallVec, smallvec};

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, const_stale_gate, is_tainted,
        prim_to_value_disc, propagate_flags, scalar_disc, value_disc,
    },
    body::{
        BodyCx, ensure_owned_composite_src, ensure_owned_value_src,
        node_composite_source, ref_local_name,
    },
    call::{
        BufKind, CompositeSource, close_buf, emit_builtin_call_node, emit_owned_drop,
        finalize_valarray, open_buf, open_value_buf, owned_drop_kind,
    },
    lower::{freeze_node_typ, resolve_node_typ},
    scaffold::{self, ArraySrc},
    scalar::{
        ElementRead, abstract_read_helper, compile_bin, compile_cast, compile_cmp,
        compile_const, compile_element_read, kind_disc, prim_to_clif,
        scalar_to_payload_i64, string_buf_push_helper, widen_to_i64, zero_const,
    },
};

/// Constant literal, dispatched on its runtime shape:
///
/// - Scalar: inline `iconst`/`f64const`.
/// - String: stable interned `*const ArcStr` + refcount bump via
///   `graphix_arcstr_clone_from_static` → an OWNED ArcStr word.
/// - Value-shape (datetime/duration/bytes/map literals): stable
///   interned `*const Value` + `graphix_value_clone_from_static` →
///   an OWNED two-word Value.
pub(crate) fn emit_const_node(
    cx: &mut BodyCx,
    value: &Value,
    typ: &Type,
) -> Result<CompiledExpr> {
    // A constant fires at init only and is never tainted.
    let init = cx.born_flag();
    match kernel_abi::abi_kind(typ) {
        Some(AbiKind::Scalar(prim)) => {
            let disc = scalar_disc(cx.b, prim);
            let disc = const_stale_gate(cx.b, init, disc);
            Ok(CompiledExpr::new(disc, compile_const(cx.b, value, prim)?))
        }
        Some(AbiKind::String) => {
            let s = match value {
                Value::String(s) => s,
                v => {
                    return Err(anyhow!("emit_clif: String-typed Constant holds {v:?}"));
                }
            };
            let ptr = cx.interned_str(s)?;
            let call = cx.call_helper("graphix_arcstr_clone_from_static", &[ptr])?;
            let payload = call[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = const_stale_gate(cx.b, init, disc);
            Ok(CompiledExpr::new(disc, payload))
        }
        Some(AbiKind::Value) => {
            let ptr = cx.interned_value(value)?;
            let call = cx.call_helper("graphix_value_clone_from_static", &[ptr])?;
            let (r0, r1) = (call[0], call[1]);
            let disc = const_stale_gate(cx.b, init, r0);
            Ok(CompiledExpr::new(disc, r1))
        }
        Some(AbiKind::Null) => {
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL);
            let disc = const_stale_gate(cx.b, init, disc);
            let payload = cx.b.ins().iconst(types::I64, 0);
            Ok(CompiledExpr::new(disc, payload))
        }
        other => {
            Err(anyhow!("emit_clif: Constant of shape {other:?} — not yet supported"))
        }
    }
}

/// A `{k => v, ...}` map literal: one whose every key and value is a
/// compile-time constant is built here and emitted as an interned Value
/// constant; otherwise its keys and values, alternating, go into a value
/// buf that `graphix_map_finalize` turns into the map.
pub(crate) fn emit_map_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    entries: &[Node<R, E>],
    typ: &Type,
) -> Result<CompiledExpr> {
    if let Some(v) = lowering::const_map(entries) {
        let typ = kernel_abi::freeze_for_abi(typ).unwrap_or_else(kernel_abi::map_type);
        return emit_const_node(cx, &v, &typ);
    }
    let cap = cx.b.ins().iconst(types::I64, entries.len() as i64);
    let buf = open_value_buf(cx, cap)?;
    let mut discs: SmallVec<[ClifValue; 8]> = SmallVec::new();
    for e in entries {
        discs.push(emit_push_field_node(cx, buf, e)?);
    }
    close_buf(cx);
    let call = cx.call_helper("graphix_map_finalize", &[buf])?;
    let (base, payload) = (call[0], call[1]);
    let disc = producer_disc(cx, base, &discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// A binding read: the local's disc (with any `TAINT`/`STALE`) and its
/// payload, in the representation the read's type `typ` declares.
pub(crate) fn emit_ref_node(
    cx: &mut BodyCx,
    spec: &Expr,
    id: BindId,
    typ: &Type,
) -> Result<CompiledExpr> {
    // BindId first (exact under shadowing); a synthetic Ref has no name
    // and resolves by id alone.
    let name = ref_local_name(spec);
    let (index, vv, kind) = {
        let index = cx.env.position(id, name);
        if index.is_none() && crate::dbgenv::gxdbg_refmiss() {
            eprintln!(
                "REFMISS `{name:?}` id={id:?} locals={:?}",
                cx.env
                    .locals
                    .iter()
                    .map(|l| (l.name.as_str(), l.bind_id))
                    .collect::<Vec<_>>()
            );
        }
        let index = index.ok_or_else(|| {
            anyhow!(
                "emit_clif: undefined local `{}` ({id:?})",
                name.unwrap_or("<synthetic>")
            )
        })?;
        let l = &cx.env.locals[index];
        (index, l.words, l.kind)
    };
    // A wake reads a standing binding with its STALE disc intact; only a
    // genuine init upgrades: the kernel's at the boundary, a slot's here.
    let disc = cx.read_disc(index);
    match kind {
        // Each consumer gets its own ArcStr ref; the slot keeps its own
        // until scope exit.
        LocalKind::String => {
            let s = cx.b.use_var(vv.payload);
            let call = cx.call_helper("graphix_arcstr_clone", &[s])?;
            Ok(CompiledExpr::new(disc, call[0]))
        }
        LocalKind::Scalar(p) => {
            let cv = CompiledExpr::new(disc, cx.b.use_var(vv.payload));
            Ok(widen_to_declared_repr(cx, typ, p, cv))
        }
        // Non-scalar kinds are borrowed reads: the env owns the slot and
        // consumers clone when they need ownership.
        LocalKind::Composite | LocalKind::Value => {
            Ok(CompiledExpr::new(disc, cx.b.use_var(vv.payload)))
        }
    }
}

/// A node must emit the representation its own type declares, since
/// consumers classify on `abi_kind(node.typ())`. Arithmetic computes
/// in the operands' scalar; if the node's type froze to a Value shape,
/// widen the payload (a scalar's disc is already its Value disc).
fn widen_to_declared_repr(
    cx: &mut BodyCx,
    out_typ: &Type,
    prim: PrimType,
    cv: CompiledExpr,
) -> CompiledExpr {
    match kernel_abi::freeze_for_abi_normalized(out_typ)
        .as_ref()
        .and_then(|t| kernel_abi::abi_kind(t))
    {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let payload = scalar_to_payload_i64(cx.b, prim, cv.payload);
            CompiledExpr::new(cv.disc, payload)
        }
        _ => cv,
    }
}

/// Arithmetic: `compile_bin` on register scalars with the integer
/// div/mod guard; a numeric type with no register form (a decimal, a
/// varint) through the Value helpers.
pub(crate) fn emit_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: ArithOp,
    out_typ: &Type,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    if cx.node_kind(lhs.typ()) == Some(AbiKind::Value) {
        return emit_value_arith_node(cx, op, false, lhs, rhs);
    }
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let l = lcv.payload;
    let r = rcv.payload;
    // the operand type may be an un-normalized union; Err means no fusion
    let prim = freeze_node_typ(cx.ctx, lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t))
        .ok_or_else(|| {
            anyhow!("emit_clif: arith operand of non-scalar type {}", lhs.typ())
        })?;
    let base = scalar_disc(cx.b, prim);
    // Cranelift has no `frem`: a float `%` is Rust's, the node-walk's.
    if matches!(op, ArithOp::Mod) && prim.is_float() {
        let helper = match prim {
            PrimType::F32 => "graphix_f32_rem",
            _ => "graphix_f64_rem",
        };
        let call = cx.call_helper(helper, &[l, r])?;
        let value = call[0];
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        return Ok(widen_to_declared_repr(
            cx,
            out_typ,
            prim,
            CompiledExpr::new(disc, value),
        ));
    }
    if matches!(op, ArithOp::Div | ArithOp::Mod)
        && prim.is_integer()
        && node_int_div_may_bottom(lhs, rhs)
    {
        let is_zero = cx.b.ins().icmp_imm(IntCC::Equal, r, 0);
        // `x % -1` is `x % 1`, 0, without the MIN ÷ -1 the hardware traps on
        let (bad, divisor_one) = match (prim.is_signed(), op) {
            (false, _) => (is_zero, is_zero),
            (true, ArithOp::Mod) => {
                let is_neg1 = cx.b.ins().icmp_imm(IntCC::Equal, r, -1);
                (is_zero, cx.b.ins().bor(is_zero, is_neg1))
            }
            (true, _) => {
                let min: i64 = match prim {
                    PrimType::I8 => i8::MIN as i64,
                    PrimType::I16 => i16::MIN as i64,
                    PrimType::I32 => i32::MIN as i64,
                    _ => i64::MIN,
                };
                let is_min = cx.b.ins().icmp_imm(IntCC::Equal, l, min);
                let is_neg1 = cx.b.ins().icmp_imm(IntCC::Equal, r, -1);
                let overflow = cx.b.ins().band(is_min, is_neg1);
                let bad = cx.b.ins().bor(is_zero, overflow);
                (bad, bad)
            }
        };
        let one = cx.b.ins().iconst(prim_to_clif(prim), 1);
        let safe_r = cx.b.ins().select(divisor_one, one, r);
        let value = compile_bin(cx.b, op, prim, l, safe_r)?;
        // A div0 / signed MIN÷-1 taints the result, as does a tainted operand.
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        let taint_word = cx.b.ins().iconst(types::I64, TAINT);
        let zero = cx.b.ins().iconst(types::I64, 0);
        let bad_taint = cx.b.ins().select(bad, taint_word, zero);
        let disc = cx.b.ins().bor(disc, bad_taint);
        let cv = CompiledExpr::new(disc, value);
        return Ok(widen_to_declared_repr(cx, out_typ, prim, cv));
    }
    let value = compile_bin(cx.b, op, prim, l, r)?;
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    let cv = CompiledExpr::new(disc, value);
    Ok(widen_to_declared_repr(cx, out_typ, prim, cv))
}

/// An operand as a Value pair for a borrowing helper, uncloned; `owned`
/// is the kind to drop once the helper returned, `None` when there is
/// nothing to drop.
#[derive(Clone, Copy)]
struct ValueOperand {
    cv: CompiledExpr,
    owned: Option<LocalKind>,
}

fn emit_value_operand_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<ValueOperand> {
    let kind = cx.node_kind(node.typ());
    let cv = match kind {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            node.emit_clif(cx)?
        }
        Some(AbiKind::Scalar(_) | AbiKind::Null | AbiKind::String) => {
            emit_owned_value_operand_node(cx, node)?
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let cv = node.emit_clif(cx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            CompiledExpr::new(propagate_flags(cx.b, base, &[cv.disc]), cv.payload)
        }
        other => bail!("emit_clif: value operand has unexpected type {other:?}"),
    };
    let owned = kind.and_then(|k| owned_drop_kind(k, node_composite_source(node)));
    Ok(ValueOperand { cv, owned })
}

/// Two operands for a borrowing helper, the first held while the
/// second emits, so a pending exit there frees it.
fn emit_value_pair<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<(ValueOperand, ValueOperand)> {
    let l = emit_value_operand_node(cx, lhs)?;
    let r = with_held(cx, l, |cx| emit_value_operand_node(cx, rhs))?;
    Ok((l, r))
}

/// `f`'s emission with `op`, when owned, held for a pending exit there.
fn with_held<T>(
    cx: &mut BodyCx,
    op: ValueOperand,
    f: impl FnOnce(&mut BodyCx) -> Result<T>,
) -> Result<T> {
    let Some(kind) = op.owned else { return f(cx) };
    cx.hold(kind, op.cv);
    let r = f(cx);
    cx.release();
    r
}

/// Drop what a borrowing helper's operands own.
fn drop_operands(cx: &mut BodyCx, ops: &[ValueOperand]) -> Result<()> {
    for op in ops {
        if let Some(kind) = op.owned {
            emit_owned_drop(cx.b, cx.ctx, kind, op.cv)?;
        }
    }
    Ok(())
}

/// Checked arithmetic (`+?` / `-?` / `*?` / `/?` / `%?`). The result is
/// a Value: the scalar, or the catchable `ArithError` (Nullable wire
/// shape) — never bottom. Over one integer prim the success path is
/// inline, and a failure builds its error through
/// `graphix_value_checked_<op>`, the node-walk's `Value::checked_*`
/// core; any other operands take that helper every time, borrowed.
pub(crate) fn emit_checked_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: ArithOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    match (cx.node_kind(lhs.typ()), cx.node_kind(rhs.typ())) {
        (Some(AbiKind::Scalar(p)), Some(AbiKind::Scalar(q)))
            if p == q && p.is_integer() =>
        {
            emit_checked_int_arith(cx, op, p, lhs, rhs)
        }
        _ => emit_value_arith_node(cx, op, true, lhs, rhs),
    }
}

/// [`emit_checked_arith_node`] over two integers of `prim`: the operation
/// and whether it fails (an overflow, a zero divisor, `MIN / -1` and
/// `MIN % -1`) in plain instructions every host lowers.
fn emit_checked_int_arith<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: ArithOp,
    prim: PrimType,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let (l, r) = (lcv.payload, rcv.payload);
    let signed = prim.is_signed();
    let ty = prim_to_clif(prim);
    let (value, bad) = match op {
        ArithOp::Div | ArithOp::Mod => {
            let mut bad = cx.b.ins().icmp_imm(IntCC::Equal, r, 0);
            if signed {
                let min: i64 = match prim {
                    PrimType::I8 => i8::MIN as i64,
                    PrimType::I16 => i16::MIN as i64,
                    PrimType::I32 => i32::MIN as i64,
                    _ => i64::MIN,
                };
                let is_min = cx.b.ins().icmp_imm(IntCC::Equal, l, min);
                let is_neg1 = cx.b.ins().icmp_imm(IntCC::Equal, r, -1);
                let overflow = cx.b.ins().band(is_min, is_neg1);
                bad = cx.b.ins().bor(bad, overflow);
            }
            let one = cx.b.ins().iconst(ty, 1);
            let safe_r = cx.b.ins().select(bad, one, r);
            (compile_bin(cx.b, op, prim, l, safe_r)?, bad)
        }
        // narrower than 64 bits: the exact result in 64, then whether it
        // fits back
        _ if ty != types::I64 => {
            let (wl, wr) = match signed {
                true => {
                    (cx.b.ins().sextend(types::I64, l), cx.b.ins().sextend(types::I64, r))
                }
                false => {
                    (cx.b.ins().uextend(types::I64, l), cx.b.ins().uextend(types::I64, r))
                }
            };
            let wide = match op {
                ArithOp::Add => cx.b.ins().iadd(wl, wr),
                ArithOp::Sub => cx.b.ins().isub(wl, wr),
                _ => cx.b.ins().imul(wl, wr),
            };
            let value = cx.b.ins().ireduce(ty, wide);
            let back = match signed {
                true => cx.b.ins().sextend(types::I64, value),
                false => cx.b.ins().uextend(types::I64, value),
            };
            (value, cx.b.ins().icmp(IntCC::NotEqual, back, wide))
        }
        ArithOp::Add => {
            let v = cx.b.ins().iadd(l, r);
            let bad = match signed {
                // both operands' signs differ from the result's
                true => {
                    let a = cx.b.ins().bxor(l, v);
                    let b = cx.b.ins().bxor(r, v);
                    let both = cx.b.ins().band(a, b);
                    cx.b.ins().icmp_imm(IntCC::SignedLessThan, both, 0)
                }
                false => cx.b.ins().icmp(IntCC::UnsignedLessThan, v, l),
            };
            (v, bad)
        }
        ArithOp::Sub => {
            let v = cx.b.ins().isub(l, r);
            let bad = match signed {
                // the operands' signs differ and the result's is not l's
                true => {
                    let a = cx.b.ins().bxor(l, r);
                    let b = cx.b.ins().bxor(l, v);
                    let both = cx.b.ins().band(a, b);
                    cx.b.ins().icmp_imm(IntCC::SignedLessThan, both, 0)
                }
                false => cx.b.ins().icmp(IntCC::UnsignedLessThan, l, r),
            };
            (v, bad)
        }
        ArithOp::Mul => {
            let v = cx.b.ins().imul(l, r);
            let bad = match signed {
                // the high word is not the low word's sign
                true => {
                    let hi = cx.b.ins().smulhi(l, r);
                    let sign = cx.b.ins().sshr_imm(v, 63);
                    cx.b.ins().icmp(IntCC::NotEqual, hi, sign)
                }
                false => {
                    let hi = cx.b.ins().umulhi(l, r);
                    cx.b.ins().icmp_imm(IntCC::NotEqual, hi, 0)
                }
            };
            (v, bad)
        }
    };
    let merge = cx.b.create_block();
    cx.b.append_block_param(merge, types::I64);
    cx.b.append_block_param(merge, types::I64);
    let (ok_bl, bad_bl) = (cx.b.create_block(), cx.b.create_block());
    cx.b.ins().brif(bad, bad_bl, &[], ok_bl, &[]);
    cx.b.switch_to_block(ok_bl);
    cx.b.seal_block(ok_bl);
    let disc = scalar_disc(cx.b, prim);
    let payload = scalar_to_payload_i64(cx.b, prim, value);
    cx.b.ins().jump(merge, &[BlockArg::Value(disc), BlockArg::Value(payload)]);
    cx.b.switch_to_block(bad_bl);
    cx.b.seal_block(bad_bl);
    let helper = match op {
        ArithOp::Add => "graphix_value_checked_add",
        ArithOp::Sub => "graphix_value_checked_sub",
        ArithOp::Mul => "graphix_value_checked_mul",
        ArithOp::Div => "graphix_value_checked_div",
        ArithOp::Mod => "graphix_value_checked_rem",
    };
    let (ld, lp) = (scalar_disc(cx.b, prim), scalar_to_payload_i64(cx.b, prim, l));
    let (rd, rp) = (scalar_disc(cx.b, prim), scalar_to_payload_i64(cx.b, prim, r));
    let call = cx.call_helper(helper, &[ld, lp, rd, rp])?;
    let (ed, ep) = (call[0], call[1]);
    cx.b.ins().jump(merge, &[BlockArg::Value(ed), BlockArg::Value(ep)]);
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let (rdisc, rpay) = (cx.b.block_params(merge)[0], cx.b.block_params(merge)[1]);
    let disc = propagate_flags(cx.b, rdisc, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `lhs op rhs` through the Value helpers, which borrow both operands.
fn emit_value_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: ArithOp,
    checked: bool,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let (l, r) = emit_value_pair(cx, lhs, rhs)?;
    let helper = match (op, checked) {
        (ArithOp::Add, false) => "graphix_value_add",
        (ArithOp::Sub, false) => "graphix_value_sub",
        (ArithOp::Mul, false) => "graphix_value_mul",
        (ArithOp::Div, false) => "graphix_value_div",
        (ArithOp::Mod, false) => "graphix_value_rem",
        (ArithOp::Add, true) => "graphix_value_checked_add",
        (ArithOp::Sub, true) => "graphix_value_checked_sub",
        (ArithOp::Mul, true) => "graphix_value_checked_mul",
        (ArithOp::Div, true) => "graphix_value_checked_div",
        (ArithOp::Mod, true) => "graphix_value_checked_rem",
    };
    let call =
        cx.call_helper(helper, &[l.cv.disc, l.cv.payload, r.cv.disc, r.cv.payload])?;
    let (rdisc, rpay) = (call[0], call[1]);
    drop_operands(cx, &[l, r])?;
    let disc = propagate_flags(cx.b, rdisc, &[l.cv.disc, r.cv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Comparison: `compile_cmp` on scalar operands; on others, `==`/`!=`
/// by `Value` equality and the orderings by the total order of values,
/// through helpers that borrow both operands.
pub(crate) fn emit_cmp_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: CmpOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    if [lhs.typ(), rhs.typ()].iter().any(|t| t.compares_refs(cx.ctx.type_env)) {
        return Err(anyhow!(
            "emit_clif: == over references compares what they name, which the runtime knows"
        ));
    }
    let lprim = kernel_abi::freeze_for_abi_normalized(lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t));
    let rprim = kernel_abi::freeze_for_abi_normalized(rhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t));
    if let (Some(lp), Some(rp)) = (lprim, rprim)
        && lp == rp
    {
        let lcv = lhs.emit_clif(cx)?;
        let rcv = rhs.emit_clif(cx)?;
        let value = compile_cmp(cx.b, op, lp, lcv.payload, rcv.payload);
        let base = scalar_disc(cx.b, PrimType::Bool);
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        return Ok(CompiledExpr::new(disc, value));
    }
    for t in [lhs.typ(), rhs.typ()] {
        if matches!(cx.node_kind(t), Some(AbiKind::Unit | AbiKind::Null) | None) {
            return Err(anyhow!(
                "emit_clif: operand of type {t} has no comparable runtime form"
            ));
        }
    }
    let (l, r) = emit_value_pair(cx, lhs, rhs)?;
    let args = [l.cv.disc, l.cv.payload, r.cv.disc, r.cv.payload];
    let result = match op {
        CmpOp::Eq | CmpOp::Ne => {
            let call = cx.call_helper("graphix_value_eq", &args)?;
            let eq = call[0];
            match op {
                CmpOp::Ne => cx.b.ins().bxor_imm(eq, 1),
                _ => eq,
            }
        }
        CmpOp::Lt | CmpOp::Gt | CmpOp::Lte | CmpOp::Gte => {
            let call = cx.call_helper("graphix_value_cmp", &args)?;
            let ord = call[0];
            let cc = match op {
                CmpOp::Lt => IntCC::SignedLessThan,
                CmpOp::Gt => IntCC::SignedGreaterThan,
                CmpOp::Lte => IntCC::SignedLessThanOrEqual,
                _ => IntCC::SignedGreaterThanOrEqual,
            };
            cx.b.ins().icmp_imm(cc, ord, 0)
        }
    };
    drop_operands(cx, &[l, r])?;
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[l.cv.disc, r.cv.disc]);
    Ok(CompiledExpr::new(disc, result))
}

/// Strict `band`/`bor`: both operands always evaluated, like the
/// node-walk's `bool_op!`.
pub(crate) fn emit_bool_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BoolOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let value = match op {
        BoolOp::And => cx.b.ins().band(lcv.payload, rcv.payload),
        BoolOp::Or => cx.b.ins().bor(lcv.payload, rcv.payload),
    };
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// Logical NOT.
pub(crate) fn emit_not_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let one = cx.b.ins().iconst(types::I8, 1);
    let value = cx.b.ins().bxor(cv.payload, one);
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[cv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// `-x` on a register scalar or a borrowed Value.
pub(crate) fn emit_neg_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    out_typ: &Type,
) -> Result<CompiledExpr> {
    let Some(prim) = freeze_node_typ(cx.ctx, inner.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t))
    else {
        let operand = emit_value_operand_node(cx, inner)?;
        let call =
            cx.call_helper("graphix_value_neg", &[operand.cv.disc, operand.cv.payload])?;
        let (disc, payload) = (call[0], call[1]);
        drop_operands(cx, &[operand])?;
        let disc = propagate_flags(cx.b, disc, &[operand.cv.disc]);
        return Ok(CompiledExpr::new(disc, payload));
    };
    let cv = inner.emit_clif(cx)?;
    let value = if prim.is_integer() {
        cx.b.ins().ineg(cv.payload)
    } else {
        cx.b.ins().fneg(cv.payload)
    };
    let base = scalar_disc(cx.b, prim);
    let disc = propagate_flags(cx.b, base, &[cv.disc]);
    Ok(widen_to_declared_repr(cx, out_typ, prim, CompiledExpr::new(disc, value)))
}

/// `cast<T>(x)`.
pub(crate) fn emit_cast_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    target: &Type,
    expr_id: ExprId,
) -> Result<CompiledExpr> {
    if let Some(cast) = lowering::inline_cast(inner.typ(), target) {
        let tgt = cast.tgt();
        let cv = inner.emit_clif(cx)?;
        let value = compile_cast(cx.b, cv.payload, cast);
        let base = scalar_disc(cx.b, tgt);
        let disc = propagate_flags(cx.b, base, &[cv.disc]);
        // The cast node's static type is the fallible `[T, Error]`
        // union, so consumers expect the 2-word Value payload; the
        // qop unwrap narrows back.
        let payload = scalar_to_payload_i64(cx.b, tgt, value);
        return Ok(CompiledExpr::new(disc, payload));
    }
    // Otherwise the discovered `SiteDispatch::Cast` site calls
    // `target.cast_value`, the node-walk's own fn; the `[T, Error]`
    // result rides the 2-word Value shape.
    let info = match cx.builtin_site(expr_id) {
        Some(i) => i.clone(),
        None => {
            return Err(anyhow!(
                "emit_clif: cast site {expr_id:?} not discovered — doesn't fuse"
            ));
        }
    };
    emit_builtin_call_node(cx, &info, &[inner], &[0])
}

/// String interpolation `"x is [x]"`: push each part into a heap
/// `String` (string parts consumed, scalars Display-rendered, any other
/// part rendered at its type `typs[i]` as the node-walk renders it) and
/// finalize to an owned ArcStr.
pub(crate) fn emit_string_interpolate_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    args: &[Node<R, E>],
    typs: &[Type],
) -> Result<CompiledExpr> {
    let call = cx.call_helper("graphix_string_buf_new", &[])?;
    let buf = call[0];
    open_buf(cx, BufKind::String, buf);
    // A tainted part renders harmlessly; its taint folds into the result.
    let mut part_discs: SmallVec<[ClifValue; 8]> = SmallVec::new();
    for (part, typ) in args.iter().zip(typs.iter()) {
        // Normalized so a select-valued part (an arm union) still classifies.
        let frozen = kernel_abi::freeze_for_abi_normalized(part.typ());
        match frozen.as_ref().and_then(|t| kernel_abi::abi_kind(t)) {
            Some(AbiKind::String) => {
                let cv = part.emit_clif(cx)?;
                part_discs.push(cv.disc);
                cx.call_helper("graphix_string_buf_push_arcstr", &[buf, cv.payload])?;
            }
            Some(AbiKind::Scalar(p)) => {
                let cv = part.emit_clif(cx)?;
                part_discs.push(cv.disc);
                cx.call_helper(string_buf_push_helper(p), &[buf, cv.payload])?;
            }
            Some(_) => {
                let cv = emit_owned_value_operand_node(cx, part)?;
                part_discs.push(cv.disc);
                let typ = cx.interned_type(&typ.resolve_tvars())?;
                cx.call_helper(
                    "graphix_string_buf_push_value",
                    &[buf, typ, cv.disc, cv.payload],
                )?;
            }
            None => {
                return Err(anyhow!(
                    "emit_clif: string-interpolate part of type {} has no ABI kind",
                    part.typ()
                ));
            }
        }
    }
    close_buf(cx);
    let call = cx.call_helper("graphix_string_buf_finalize", &[buf])?;
    let payload = call[0];
    let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
    let disc = propagate_flags(cx.b, base, &part_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Compile an operand as an owned `(disc, payload)` Value for a
/// consuming helper. A scalar widens its payload (its disc is already
/// the Value disc); String/composite bits are already the Value payload
/// word, so only the disc is minted with the source's flags folded on.
pub(crate) fn emit_owned_value_operand_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<CompiledExpr> {
    // Normalized: a select's type is its raw arm union, its emission
    // follows the normalized one.
    let kind = cx.node_kind(node.typ());
    match kind {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            // A missing 2-word input is a `Value::Null` placeholder the
            // helpers run harmlessly on; its taint guards the result.
            let cv = node.emit_clif(cx)?;
            let (disc, payload) = ensure_owned_value_src(
                cx,
                node_composite_source(node),
                cv.disc,
                cv.payload,
            )?;
            Ok(CompiledExpr::new(disc, payload))
        }
        Some(AbiKind::Scalar(p)) => {
            let cv = node.emit_clif(cx)?;
            let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
            Ok(CompiledExpr::new(cv.disc, payload))
        }
        Some(AbiKind::String) => {
            // Fold the source's flags on, else a placeholder input's `""`
            // computes an untainted result.
            let cv = node.emit_clif(cx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            Ok(CompiledExpr::new(disc, cv.payload))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            // Same flag fold as the String arm: an empty placeholder must
            // not compute an untainted result.
            let cv = node.emit_clif(cx)?;
            let bits =
                ensure_owned_composite_src(cx, node_composite_source(node), cv.payload)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            Ok(CompiledExpr::new(disc, bits))
        }
        // A Null node's raw pair is already a valid Value.
        Some(AbiKind::Null) => node.emit_clif(cx),
        other => Err(anyhow!("emit_clif: value operand has unexpected type {other:?}")),
    }
}

/// Widen a call result emitted in the callee's return shape into the
/// owned Value the callsite node's type promises (inference may widen
/// a call's type to a union the callee's return is one member of). The
/// result stays owned; flags fold from the produced disc.
pub(crate) fn widen_result_to_value(
    cx: &mut BodyCx,
    produced: &Type,
    cv: CompiledExpr,
) -> Result<CompiledExpr> {
    match kernel_abi::abi_kind(produced) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null) => {
            Ok(cv)
        }
        Some(AbiKind::Scalar(p)) => {
            let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
            Ok(CompiledExpr::new(cv.disc, payload))
        }
        Some(AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            // A composite/string pair is already a genuine Value.
            Ok(cv)
        }
        other => Err(anyhow!(
            "emit_clif: call result shape {other:?} cannot widen to a \
             value-typed node — subtree node-walks"
        )),
    }
}

/// An owned value-shaped production bound where its type is a composite
/// (a select over arrays of different element types, under an array
/// annotation): a present value's payload, which the types make the
/// composite's bits, and an owned empty placeholder for a tainted one,
/// whose payload need not be an array.
pub(crate) fn narrow_value_to_composite(
    cx: &mut BodyCx,
    cv: CompiledExpr,
) -> Result<CompiledExpr> {
    let tainted = is_tainted(cx.b, cv.disc);
    let keep_bl = cx.b.create_block();
    let skip_bl = cx.b.create_block();
    let merge = cx.b.create_block();
    cx.b.append_block_param(merge, types::I64);
    cx.b.ins().brif(tainted, skip_bl, &[], keep_bl, &[]);
    cx.b.switch_to_block(keep_bl);
    cx.b.seal_block(keep_bl);
    cx.b.ins().jump(merge, &[BlockArg::Value(cv.payload)]);
    cx.b.switch_to_block(skip_bl);
    cx.b.seal_block(skip_bl);
    emit_owned_drop(cx.b, cx.ctx, LocalKind::Value, cv)?;
    let empty = cx.call_helper("graphix_valarray_empty", &[])?[0];
    cx.b.ins().jump(merge, &[BlockArg::Value(empty)]);
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let payload = cx.b.block_params(merge)[0];
    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, base, &[cv.disc]);
    Ok(CompiledExpr::new(disc, payload))
}

/// True iff a callsite whose node type is `node_typ` needs
/// [`widen_result_to_value`] on a result produced in the callee's `ret`
/// shape. `Null` is exempt: its pair is already a valid Value.
pub(crate) fn call_result_needs_value_widening(
    cx: &BodyCx,
    node_typ: &Type,
    ret: &Type,
) -> Result<bool> {
    let Some(node) = cx.node_kind(node_typ) else {
        return Err(anyhow!(
            "emit_clif: a call site of type {node_typ} has no kernel shape"
        ));
    };
    Ok(matches!(node, AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
        && !matches!(
            cx.node_kind(ret),
            Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null)
        ))
}

/// Compile one producer field and push it into `buf`
/// ([`scaffold::push_field`]); returns the field's disc.
fn emit_push_field_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    buf: ClifValue,
    field: &Node<R, E>,
) -> Result<ClifValue> {
    let typ = freeze_node_typ(cx.ctx, field.typ()).ok_or_else(|| {
        anyhow!("emit_clif: producer field of type {} — not representable", field.typ())
    })?;
    let cv = field.emit_clif(cx)?;
    scaffold::push_field(cx, buf, cv, &typ, node_composite_source(field))?;
    Ok(cv.disc)
}

/// Emit `fields` into a fresh registered value buf; returns the buf
/// and the fields' discs.
fn emit_fields_into_buf<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    fields: &[Node<R, E>],
) -> Result<(ClifValue, SmallVec<[ClifValue; 8]>)> {
    let cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let buf = open_value_buf(cx, cap)?;
    let mut field_discs: SmallVec<[ClifValue; 8]> = SmallVec::new();
    for f in fields {
        field_discs.push(emit_push_field_node(cx, buf, f)?);
    }
    Ok((buf, field_discs))
}

/// A producer's disc: fires iff any field fired; zero fields is a
/// constant and fires at init only.
fn producer_disc(
    cx: &mut BodyCx,
    base: ClifValue,
    field_discs: &[ClifValue],
) -> ClifValue {
    if field_discs.is_empty() {
        let init = cx.born_flag();
        const_stale_gate(cx.b, init, base)
    } else {
        propagate_flags(cx.b, base, field_discs)
    }
}

/// `[<a, b, c>]`: the elements are pushed into a value buf that
/// `graphix_list_finalize` turns into the list.
pub(crate) fn emit_list_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    fields: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let (buf, field_discs) = emit_fields_into_buf(cx, fields)?;
    close_buf(cx);
    let call = cx.call_helper("graphix_list_finalize", &[buf])?;
    let (base, payload) = (call[0], call[1]);
    let disc = producer_disc(cx, base, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Push the marker of `kind` onto `buf`: a tuple, struct or variant
/// opens with it.
pub(super) fn push_marker(
    cx: &mut BodyCx,
    buf: ClifValue,
    kind: rep::Kind,
) -> Result<()> {
    let kind = cx.b.ins().iconst(types::I64, kind as i64);
    cx.call_helper("graphix_value_buf_push_marker", &[buf, kind])?;
    Ok(())
}

/// Tuple literal (`kind` is the tuple marker) or array literal (none):
/// push each field, finalize into an owned ValArray.
pub(crate) fn emit_seq_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    kind: Option<rep::Kind>,
    fields: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let cap =
        cx.b.ins().iconst(types::I64, (fields.len() + kind.is_some() as usize) as i64);
    let buf = open_value_buf(cx, cap)?;
    if let Some(kind) = kind {
        push_marker(cx, buf, kind)?;
    }
    let mut field_discs: SmallVec<[ClifValue; 8]> = SmallVec::new();
    for f in fields {
        field_discs.push(emit_push_field_node(cx, buf, f)?);
    }
    let payload = finalize_valarray(cx, buf)?;
    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = producer_disc(cx, base, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Push a struct's `[name, value]` pair onto its `outer` buf; `value`
/// pushes the field into the pair's buf and returns its disc.
fn push_struct_pair(
    cx: &mut BodyCx,
    outer: ClifValue,
    name: &ArcStr,
    value: impl FnOnce(&mut BodyCx, ClifValue) -> Result<ClifValue>,
) -> Result<ClifValue> {
    let cap = cx.b.ins().iconst(types::I64, 2);
    let pair = open_value_buf(cx, cap)?;
    let name_ptr = cx.interned_str(name)?;
    cx.call_helper("graphix_value_buf_push_arcstr", &[pair, name_ptr])?;
    let disc = value(cx, pair)?;
    let pair_bits = finalize_valarray(cx, pair)?;
    cx.call_helper("graphix_value_buf_push_array", &[outer, pair_bits])?;
    Ok(disc)
}

/// Struct literal: an outer ValArray of `[name, value]` pairs sorted by
/// name (the canonical struct layout).
pub(crate) fn emit_struct_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    names: &[ArcStr],
    fields: &[Node<R, E>],
) -> Result<CompiledExpr> {
    if names.len() != fields.len() {
        return Err(anyhow!("emit_clif: struct literal name/field arity mismatch"));
    }
    let mut indexed: SmallVec<[(&ArcStr, &Node<R, E>); 8]> =
        names.iter().zip(fields.iter()).collect();
    indexed.sort_by(|a, b| a.0.cmp(b.0));
    let cap = cx.b.ins().iconst(types::I64, indexed.len() as i64 + 1);
    let outer = open_value_buf(cx, cap)?;
    push_marker(cx, outer, rep::Kind::Struct)?;
    let mut field_discs: SmallVec<[ClifValue; 8]> = SmallVec::new();
    for (name, field) in indexed {
        // Names are interned constants; only the value discs gate freshness.
        let disc = push_struct_pair(cx, outer, name, |cx, pair| {
            emit_push_field_node(cx, pair, field)
        })?;
        field_discs.push(disc);
    }
    let payload = finalize_valarray(cx, outer)?;
    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = producer_disc(cx, base, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// `{ source with f: v, ... }`: a new struct copying the source's sorted
/// pairs, reading unchanged fields from the source and emitting each
/// replacement. `Replace.index` is the field's sorted position. The bufs
/// and an owned source are registered for pending-exit cleanup so a
/// may-bottom replacement does not leak them.
pub(crate) fn emit_struct_with_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    replace: &[crate::node::data::Replace<R, E>],
) -> Result<CompiledExpr> {
    // Cloned out of the deref before emitting (lock discipline).
    let fields: poolshark::local::LPooled<Vec<(ArcStr, Type)>> =
        resolve_node_typ(cx.ctx, source.typ()).with_deref(|t| match t {
            Some(Type::Struct(flds)) => {
                Ok(flds.iter().map(|(n, t, _)| (n.clone(), t.clone())).collect())
            }
            _ => Err(anyhow!("emit_clif: struct-with source isn't a struct")),
        })?;
    let arr = emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    let (arr_ptr, src_disc) = (arr.ptr, arr.disc);
    // A tainted source does not abort: the reads below are guarded and
    // its taint folds into the result. An owned source is held so a
    // bottom-abort before the finalize frees it.
    arr.hold(cx);
    let cap = cx.b.ins().iconst(types::I64, fields.len() as i64 + 1);
    let outer = open_value_buf(cx, cap)?;
    push_marker(cx, outer, rep::Kind::Struct)?;
    // Fires iff the source or any replacement fired.
    let mut field_discs: SmallVec<[ClifValue; 8]> = smallvec![src_disc];
    // a field's `[name, value]` pair is an unmarked array
    let pair = Type::Array(triomphe::Arc::new(Type::Any));
    for (i, (name, _)) in fields.iter().enumerate() {
        match replace.iter().find(|r| r.index == Some(i)) {
            Some(r) => {
                let disc = push_struct_pair(cx, outer, name, |cx, pair| {
                    emit_push_field_node(cx, pair, &r.n)
                })?;
                field_discs.push(disc);
            }
            // a kept field's `[name, value]` pair is the source's, shared
            None => {
                let idx =
                    cx.b.ins().iconst(types::I64, (crate::rep::STRUCT_BASE + i) as i64);
                // Guarded: a tainted source's placeholder has no fields.
                let cv = emit_taint_guarded_read(cx, src_disc, &pair, |cx| {
                    let read = ElementRead::ArrayIndex;
                    compile_element_read(cx.b, arr_ptr, idx, &pair, read, cx.ctx)
                })?;
                scaffold::push_field(cx, outer, cv, &pair, CompositeSource::Owned)?;
            }
        }
    }
    let payload = finalize_valarray(cx, outer)?;
    arr.drop_held(cx)?;
    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, base, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Variant constructor: `[marker, tag, p0, ...]`. With no payload it is
/// a constant and fires at init only; else iff any payload fired.
pub(crate) fn emit_variant_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    tag: &ArcStr,
    payloads: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let tag_ptr = cx.interned_str(tag)?;
    let cap = cx.b.ins().iconst(types::I64, (payloads.len() + 2) as i64);
    let buf = open_value_buf(cx, cap)?;
    push_marker(cx, buf, rep::Kind::Variant)?;
    cx.call_helper("graphix_value_buf_push_arcstr", &[buf, tag_ptr])?;
    let mut payload_discs: SmallVec<[ClifValue; 8]> = SmallVec::new();
    for p in payloads {
        payload_discs.push(emit_push_field_node(cx, buf, p)?);
    }
    let bits = finalize_valarray(cx, buf)?;
    let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = producer_disc(cx, base, &payload_discs);
    Ok(CompiledExpr::new(disc, bits))
}

fn emit_accessor_source_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    want: AbiKind,
) -> Result<ArraySrc> {
    if cx.node_kind(source.typ()) != Some(want) {
        return Err(anyhow!(
            "emit_clif: accessor source of type {} isn't {want:?}",
            source.typ()
        ));
    }
    let ownership = node_composite_source(source);
    let cv = source.emit_clif(cx)?;
    Ok(ArraySrc { ptr: cv.payload, ownership, disc: cv.disc })
}

/// A shape-safe, owned, tainted bottom of `elem`'s ABI kind for a
/// position with no value this cycle.
///
/// A bottom is a production, so its STALE bit folds from the governing
/// discs here (a fresh bottom returned unfolded re-fires every cycle).
/// Pass `&[]` only where the whole run is being torn down. The absent-delivery twin,
/// `select::placeholder_for_kind`, is unconditionally standing.
pub(super) fn emit_bottom_placeholder(
    cx: &mut BodyCx,
    elem: &Type,
    governing_discs: &[ClifValue],
) -> Result<CompiledExpr> {
    let kind = kernel_abi::abi_kind(elem)
        .ok_or_else(|| anyhow!("emit_clif: no placeholder for {elem}"))?;
    emit_bottom_placeholder_of_kind(cx, kind, governing_discs)
}

/// [`emit_bottom_placeholder`] for a placeholder of ABI kind `kind`.
pub(super) fn emit_bottom_placeholder_of_kind(
    cx: &mut BodyCx,
    kind: AbiKind,
    governing_discs: &[ClifValue],
) -> Result<CompiledExpr> {
    let cv = emit_bottom_of_kind(cx, kind)?;
    let disc = propagate_flags(cx.b, cv.disc, governing_discs);
    Ok(CompiledExpr::new(disc, cv.payload))
}

/// `never(args)` as a value of ABI kind `kind`: its arguments run and
/// are discarded, and it is a STALE bottom, so its fire is the
/// enclosing select's scrutinee and consulted guards alone.
pub(super) fn emit_never<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    args: &[Node<R, E>],
    kind: AbiKind,
) -> Result<CompiledExpr> {
    let cv = emit_bottom_of_kind(cx, kind)?;
    for arg in args.iter() {
        let av = arg.emit_clif(cx)?;
        super::flow::emit_discard_result(cx, arg, av)?;
    }
    Ok(CompiledExpr::new(cx.b.ins().bor_imm(cv.disc, STALE), cv.payload))
}

/// A fresh tainted placeholder of the given ABI kind: an owned empty
/// payload under a bottom disc, with no STALE bit.
pub(super) fn emit_bottom_of_kind(
    cx: &mut BodyCx,
    kind: AbiKind,
) -> Result<CompiledExpr> {
    Ok(match kind {
        AbiKind::Scalar(p) => {
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            CompiledExpr::new(disc, zero_const(cx.b, p))
        }
        AbiKind::String => {
            let call = cx.call_helper("graphix_arcstr_empty", &[])?;
            let s = call[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT);
            CompiledExpr::new(disc, s)
        }
        AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => {
            let call = cx.call_helper("graphix_valarray_empty", &[])?;
            let a = call[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT);
            CompiledExpr::new(disc, a)
        }
        AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Unit => {
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let zero = cx.b.ins().iconst(types::I64, 0);
            CompiledExpr::new(disc, zero)
        }
        other => return Err(anyhow!("emit_clif: no placeholder for shape {other:?}")),
    })
}

/// `read` of a value of type `elem` out of a source with disc
/// `src_disc`, guarded on its taint: a tainted source holds a placeholder
/// the unchecked read helpers cannot touch, so the read is skipped and the
/// result is a bottom placeholder. `is_tainted` folds to false for proven-
/// untainted sources, so the branch disappears.
pub(super) fn emit_taint_guarded_read(
    cx: &mut BodyCx,
    src_disc: ClifValue,
    elem: &Type,
    read: impl FnOnce(&mut BodyCx) -> Result<CompiledExpr>,
) -> Result<CompiledExpr> {
    let tainted = is_tainted(cx.b, src_disc);
    let read_bl = cx.b.create_block();
    let skip_bl = cx.b.create_block();
    let merge = cx.b.create_block();
    let pay_ty = match kernel_abi::abi_kind(elem) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, types::I64);
    cx.b.append_block_param(merge, pay_ty);
    cx.b.ins().brif(tainted, skip_bl, &[], read_bl, &[]);
    cx.b.switch_to_block(read_bl);
    cx.b.seal_block(read_bl);
    let rv = read(cx)?;
    cx.b.ins().jump(merge, &[BlockArg::Value(rv.disc), BlockArg::Value(rv.payload)]);
    cx.b.switch_to_block(skip_bl);
    cx.b.seal_block(skip_bl);
    let ph = emit_bottom_placeholder(cx, elem, &[src_disc])?;
    cx.b.ins().jump(merge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let params = cx.b.block_params(merge);
    Ok(CompiledExpr::new(params[0], params[1]))
}

/// `T(v)`: box an owned Value operand with the abstract type's tag.
pub(crate) fn emit_construct_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    typ: &Type,
    name: &ArcStr,
    arg: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = emit_owned_value_operand_node(cx, arg)?;
    // the type as Construct::update stores it: its params resolved
    let typ_ptr = cx.interned_type(&resolve_node_typ(cx.ctx, typ).resolve_tvars())?;
    let name_ptr = cx.interned_str(name)?;
    let call = cx.call_helper(
        "graphix_abstract_wrap",
        &[typ_ptr, name_ptr, cv.disc, cv.payload],
    )?;
    let (rdisc, rpay) = (call[0], call[1]);
    let disc = propagate_flags(cx.b, rdisc, &[cv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `x.0` on a Graphix-minted abstract value: a taint-guarded read of the
/// payload at the representation's shape `rep` that borrows the source
/// and returns an owned clone.
pub(crate) fn emit_abstract_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    rep: &Type,
) -> Result<CompiledExpr> {
    let rep = resolve_node_typ(cx.ctx, rep);
    let helper = abstract_read_helper(&rep)?;
    let src = node_composite_source(source);
    let cv = source.emit_clif(cx)?;
    let rv = emit_taint_guarded_read(cx, cv.disc, &rep, |cx| {
        let call = cx.call_helper(helper, &[cv.disc, cv.payload])?;
        Ok(if kernel_abi::is_value_shape(&rep) {
            let (d, p) = (call[0], call[1]);
            CompiledExpr::new(d, p)
        } else {
            let r0 = call[0];
            CompiledExpr::new(kind_disc(cx.b, &rep), r0)
        })
    })?;
    let (rdisc, rpay) = (rv.disc, rv.payload);
    if matches!(src, CompositeSource::Owned) {
        cx.call_helper("graphix_value_drop", &[cv.disc, cv.payload])?;
    }
    let disc = propagate_flags(cx.b, rdisc, &[cv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// What a field read reads out of.
#[derive(Clone, Copy)]
pub(crate) enum FieldOf {
    Tuple,
    /// A struct; the field's index is its sorted position.
    Struct,
}

/// `t.<idx>` and `s.field`: a field read of a tuple or a struct.
pub(crate) fn emit_field_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: usize,
    elem_typ: &Type,
    of: FieldOf,
) -> Result<CompiledExpr> {
    // a tuple's elements start at its base in its array; a struct field
    // is read by its sorted index through `rep`
    let (kind, read, base) = match of {
        FieldOf::Tuple => {
            (AbiKind::Tuple, ElementRead::ArrayIndex, crate::rep::TUPLE_BASE)
        }
        FieldOf::Struct => (AbiKind::Struct, ElementRead::StructField, 0),
    };
    // a tuple bound as its fields reads the field's own local
    if let (FieldOf::Tuple, NodeView::Ref(r)) = (of, source.view())
        && let Some(field) = cx.env.unpacked_field(r.id, idx)
    {
        return emit_ref_node(cx, source.spec(), field, elem_typ);
    }
    // an element type may be a Ref to an abstract type name
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let arr = emit_accessor_source_node(cx, source, kind)?;
    let idx_const = cx.b.ins().iconst(types::I64, (base + idx) as i64);
    let result = emit_taint_guarded_read(cx, arr.disc, &elem_typ, |cx| {
        compile_element_read(cx.b, arr.ptr, idx_const, &elem_typ, read, cx.ctx)
    })?;
    arr.drop(cx)?;
    // The element read's disc is fresh; the source's STALE gates it.
    let disc = propagate_flags(cx.b, result.disc, &[arr.disc]);
    Ok(CompiledExpr::new(disc, result.payload))
}

/// `a[i]` / `bytes[i]`: always `Nullable<elem>` (out-of-bounds is the
/// `ArrayIndexError` Value) via the bounds-checked helpers the
/// node-walk shares.
pub(crate) fn emit_array_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: &Node<R, E>,
) -> Result<CompiledExpr> {
    let idx_prim = kernel_abi::scalar_prim(idx.typ())
        .filter(|p| p.is_integer())
        .ok_or_else(|| anyhow!("emit_clif: index of non-integer type {}", idx.typ()))?;
    if cx.node_kind(source.typ()) == Some(AbiKind::Array) {
        // Unforced: the helper is bounds-checked (safe on a placeholder)
        // and the source's taint folds into the result below.
        let arr = emit_accessor_source_node(cx, source, AbiKind::Array)?;
        let (arr_ptr, src_disc) = (arr.ptr, arr.disc);
        arr.hold(cx);
        let idx_cv = idx.emit_clif(cx);
        if arr.ownership == CompositeSource::Owned {
            cx.release();
        }
        let idx_cv = idx_cv?;
        let idx_i64 = widen_to_i64(cx.b, idx_cv.payload, idx_prim)?;
        let call = cx.call_helper("graphix_valarray_index", &[arr_ptr, idx_i64])?;
        let r = call;
        let (rdisc, rpay) = (r[0], r[1]);
        arr.drop(cx)?;
        // Fires iff the array or the index fired.
        let disc = propagate_flags(cx.b, rdisc, &[src_disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    if lowering::is_bytes(source.typ()) {
        let b = emit_value_operand_node(cx, source)?;
        let bcv = b.cv;
        let idx_cv = with_held(cx, b, |cx| idx.emit_clif(cx))?;
        // The helper takes an i64; a narrow payload fails cranelift's verifier.
        let idx_i64 = widen_to_i64(cx.b, idx_cv.payload, idx_prim)?;
        let call =
            cx.call_helper("graphix_bytes_index", &[bcv.disc, bcv.payload, idx_i64])?;
        let (rdisc, rpay) = (call[0], call[1]);
        drop_operands(cx, &[b])?;
        // Fires iff the bytes or the index fired.
        let disc = propagate_flags(cx.b, rdisc, &[bcv.disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    Err(anyhow!(
        "emit_clif: index source of type {} isn't an array or bytes",
        source.typ()
    ))
}

/// `m{key}`: `graphix_map_ref` borrows both operands, shares
/// `node::map::map_get` and returns `Nullable<V>`.
pub(crate) fn emit_map_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    key: &Node<R, E>,
) -> Result<CompiledExpr> {
    if !lowering::is_map(source.typ()) {
        return Err(anyhow!(
            "emit_clif: map-ref source of type {} isn't a Map",
            source.typ()
        ));
    }
    let (m, k) = emit_value_pair(cx, source, key)?;
    let (mcv, kcv) = (m.cv, k.cv);
    let call = cx.call_helper(
        "graphix_map_ref",
        &[mcv.disc, mcv.payload, kcv.disc, kcv.payload],
    )?;
    let (rdisc, rpay) = (call[0], call[1]);
    drop_operands(cx, &[m, k])?;
    // Fires iff the map or the key fired.
    let disc = propagate_flags(cx.b, rdisc, &[mcv.disc, kcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `a[i..j]` — the source as a Value the helper borrows, present bounds as integer scalars with a flag bit each, absent
/// bounds pass 0 with the bit cleared. Result is `Nullable<source>`
/// (shared `node::array::array_slice` semantics).
pub(crate) fn emit_array_slice_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    start: Option<&Node<R, E>>,
    end: Option<&Node<R, E>>,
) -> Result<CompiledExpr> {
    if !(cx.node_kind(source.typ()) == Some(AbiKind::Array)
        || lowering::is_bytes(source.typ()))
    {
        return Err(anyhow!(
            "emit_clif: slice source of type {} isn't an array or bytes",
            source.typ()
        ));
    }
    let src = emit_value_operand_node(cx, source)?;
    let scv = src.cv;
    let mut taint_discs: SmallVec<[ClifValue; 8]> = smallvec![scv.disc];
    let emit_bound = |cx: &mut BodyCx,
                      n: Option<&Node<R, E>>,
                      flag: i64,
                      flags: &mut i64,
                      taint: &mut SmallVec<[ClifValue; 8]>|
     -> Result<ClifValue> {
        match n {
            None => Ok(cx.b.ins().iconst(types::I64, 0)),
            Some(n) => {
                let Some(p) = kernel_abi::scalar_prim(n.typ()).filter(|p| p.is_integer())
                else {
                    return Err(anyhow!(
                        "emit_clif: slice bound of non-integer type {}",
                        n.typ()
                    ));
                };
                *flags |= flag;
                let cv = n.emit_clif(cx)?;
                taint.push(cv.disc);
                // The helper takes an i64; a narrow payload fails
                // cranelift's verifier.
                widen_to_i64(cx.b, cv.payload, p)
            }
        }
    };
    let mut flags = 0i64;
    let (start_v, end_v) = with_held(cx, src, |cx| {
        let s = emit_bound(cx, start, 1, &mut flags, &mut taint_discs)?;
        Ok((s, emit_bound(cx, end, 2, &mut flags, &mut taint_discs)?))
    })?;
    let flags_v = cx.b.ins().iconst(types::I64, flags);
    let call = cx.call_helper(
        "graphix_array_slice",
        &[scv.disc, scv.payload, start_v, end_v, flags_v],
    )?;
    let r = call;
    let (rdisc, rpay) = (r[0], r[1]);
    drop_operands(cx, &[src])?;
    // Fires iff the source or any present bound fired.
    let disc = propagate_flags(cx.b, rdisc, &taint_discs);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Whether an integer `/` or `%` needs its divisor guarded: false only
/// when the divisor is a constant that provably cannot be zero or meet
/// a MIN dividend as -1. Sees through `ExplicitParens`.
fn node_int_div_may_bottom<R: Rt, E: UserEvent>(
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> bool {
    fn const_value<'a, R: Rt, E: UserEvent>(n: &'a Node<R, E>) -> Option<&'a Value> {
        match n.view() {
            NodeView::Constant(c) => Some(&c.value),
            NodeView::ExplicitParens(p) => const_value(&p.n),
            _ => None,
        }
    }
    let Some(rv) = const_value(rhs) else {
        return true;
    };
    if value_is_zero(rv) {
        return true;
    }
    if !value_is_neg_one(rv) {
        return false;
    }
    // Divisor -1: only a MIN dividend bottoms; a non-constant dividend is
    // conservatively unsafe.
    match const_value(lhs) {
        Some(lv) => value_is_int_min(lv),
        None => true,
    }
}

fn value_is_zero(v: &Value) -> bool {
    matches!(
        v,
        Value::I8(0)
            | Value::I16(0)
            | Value::I32(0)
            | Value::I64(0)
            | Value::U8(0)
            | Value::U16(0)
            | Value::U32(0)
            | Value::U64(0)
            | Value::Z32(0)
            | Value::Z64(0)
            | Value::V32(0)
            | Value::V64(0)
    )
}

fn value_is_neg_one(v: &Value) -> bool {
    matches!(
        v,
        Value::I8(-1)
            | Value::I16(-1)
            | Value::I32(-1)
            | Value::I64(-1)
            | Value::Z32(-1)
            | Value::Z64(-1)
    )
}

fn value_is_int_min(v: &Value) -> bool {
    match v {
        Value::I8(x) => *x == i8::MIN,
        Value::I16(x) => *x == i16::MIN,
        Value::I32(x) | Value::Z32(x) => *x == i32::MIN,
        Value::I64(x) | Value::Z64(x) => *x == i64::MIN,
        _ => false,
    }
}
