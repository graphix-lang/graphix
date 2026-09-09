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
    node::op::{BinOp, BoolOp, CmpOp},
    typ::{AbstractId, Type},
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    BlockArg, InstBuilder, Value as ClifValue, condcodes::IntCC, types,
};
use netidx_value::Value;

use super::{
    abi::{
        CompiledExpr, LocalKind, TAINT, const_stale_gate, is_tainted, prim_to_value_disc,
        propagate_flags, scalar_disc, value_disc,
    },
    body::{
        BodyCx, ensure_owned_composite_src, ensure_owned_value_src,
        node_composite_source, ref_local_name,
    },
    call::{CompositeSource, emit_builtin_call_node},
    lower::{freeze_node_typ, resolve_node_typ},
    scaffold,
    scalar::{
        ElementRead, abstract_read_helper, compile_bin, compile_cast, compile_cmp,
        compile_const, compile_element_read, kind_disc, prim_to_clif,
        scalar_to_payload_i64, string_buf_push_helper, value_buf_push_helper,
        widen_to_i64, zero_const,
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
    let init = cx.init_flag();
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
            let ptr = cx.interned_str(s);
            let clone = cx.helper("graphix_arcstr_clone_from_static")?;
            let call = cx.b.ins().call(clone, &[ptr]);
            let payload = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = const_stale_gate(cx.b, init, disc);
            Ok(CompiledExpr::new(disc, payload))
        }
        Some(AbiKind::Value) => {
            let ptr = cx.interned_value(value);
            let clone = cx.helper("graphix_value_clone_from_static")?;
            let call = cx.b.ins().call(clone, &[ptr]);
            let (r0, r1) = {
                let r = cx.b.inst_results(call);
                (r[0], r[1])
            };
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

/// A `{k => v, ...}` map literal. Fuses only when every key and value
/// is a compile-time constant: the `CMap` is built here and emitted as
/// an interned Value constant. A dynamic entry de-fuses.
pub(crate) fn emit_map_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    keys: &[Node<R, E>],
    vals: &[Node<R, E>],
    typ: &Type,
) -> Result<CompiledExpr> {
    let v = lowering::const_map(keys, vals).ok_or_else(|| {
        anyhow!(
            "emit_clif: map literal with non-constant entries — \
             subtree node-walks"
        )
    })?;
    let typ = kernel_abi::freeze_for_abi(typ).unwrap_or_else(kernel_abi::map_type);
    emit_const_node(cx, &v, &typ)
}

/// A binding read: the local's disc (with any `TAINT`/`STALE`) and its
/// payload.
pub(crate) fn emit_ref_node(
    cx: &mut BodyCx,
    spec: &Expr,
    typ: &Type,
    id: BindId,
) -> Result<CompiledExpr> {
    let _ = typ;
    // BindId first (exact under shadowing); a synthetic Ref has no name
    // and resolves by id alone.
    let name = ref_local_name(spec);
    let (vv, kind) = {
        let l = match name {
            Some(name) => cx.env.lookup(id, name),
            None => cx.env.lookup_id(id),
        };
        if l.is_none() && crate::dbgenv::gxdbg_refmiss() {
            eprintln!(
                "REFMISS `{name:?}` id={id:?} locals={:?}",
                cx.env
                    .locals
                    .iter()
                    .map(|l| (l.name.as_str(), l.bind_id))
                    .collect::<Vec<_>>()
            );
        }
        let l = l.ok_or_else(|| {
            anyhow!(
                "emit_clif: undefined local `{}` ({id:?})",
                name.unwrap_or("<synthetic>")
            )
        })?;
        (l.words, l.kind)
    };
    // A wake reads a standing binding with its STALE disc intact; only
    // genuine init upgrades, and that happens at the boundary.
    let disc = cx.b.use_var(vv.disc);
    match kind {
        // Each consumer gets its own ArcStr ref; the slot keeps its own
        // until scope exit.
        LocalKind::String => {
            let s = cx.b.use_var(vv.payload);
            let clone = cx.helper("graphix_arcstr_clone")?;
            let call = cx.b.ins().call(clone, &[s]);
            Ok(CompiledExpr::new(disc, cx.b.inst_results(call)[0]))
        }
        // Non-scalar kinds are borrowed reads: the env owns the slot and
        // consumers clone when they need ownership.
        LocalKind::Scalar(_)
        | LocalKind::Composite
        | LocalKind::Variant
        | LocalKind::Nullable
        | LocalKind::Value => Ok(CompiledExpr::new(disc, cx.b.use_var(vv.payload))),
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

/// Arithmetic. A datetime/duration operand routes to the
/// `graphix_value_<op>` helpers (both operands owned); otherwise
/// `compile_bin` on register scalars with the integer div/mod guard.
pub(crate) fn emit_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
    out_typ: &Type,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    if lowering::is_datetime_or_duration(lhs.typ())
        || lowering::is_datetime_or_duration(rhs.typ())
    {
        let lcv = emit_owned_value_operand_node(cx, lhs)?;
        let rcv = emit_owned_value_operand_node(cx, rhs)?;
        let helper = match op {
            BinOp::Add => "graphix_value_add",
            BinOp::Sub => "graphix_value_sub",
            BinOp::Mul => "graphix_value_mul",
            BinOp::Div => "graphix_value_div",
            BinOp::Mod => "graphix_value_rem",
        };
        let fref = cx.helper(helper)?;
        let call = cx.b.ins().call(fref, &[lcv.disc, lcv.payload, rcv.disc, rcv.payload]);
        let (rdisc, rpay) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        let disc = propagate_flags(cx.b, rdisc, &[lcv.disc, rcv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    let lcv = lhs.emit_clif(cx)?;
    let rcv = rhs.emit_clif(cx)?;
    let l = lcv.payload;
    let r = rcv.payload;
    // Not `prim_of` (panics): the operand type may be an un-normalized
    // union; Err means no fusion.
    let prim = freeze_node_typ(cx.ctx, lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t))
        .ok_or_else(|| {
            anyhow!("emit_clif: arith operand of non-scalar type {:?}", lhs.typ())
        })?;
    let base = scalar_disc(cx.b, prim);
    if matches!(op, BinOp::Div | BinOp::Mod)
        && prim.is_integer()
        && node_int_div_may_bottom(lhs, rhs)
    {
        let is_zero = cx.b.ins().icmp_imm(IntCC::Equal, r, 0);
        let bad = if prim.is_signed() {
            let min: i64 = match prim {
                PrimType::I8 => i8::MIN as i64,
                PrimType::I16 => i16::MIN as i64,
                PrimType::I32 => i32::MIN as i64,
                _ => i64::MIN,
            };
            let is_min = cx.b.ins().icmp_imm(IntCC::Equal, l, min);
            let is_neg1 = cx.b.ins().icmp_imm(IntCC::Equal, r, -1);
            let overflow = cx.b.ins().band(is_min, is_neg1);
            cx.b.ins().bor(is_zero, overflow)
        } else {
            is_zero
        };
        let one = cx.b.ins().iconst(prim_to_clif(prim), 1);
        let safe_r = cx.b.ins().select(bad, one, r);
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

/// Checked arithmetic (`+?` / `-?` / `*?` / `/?` / `%?`). Both
/// operands are owned Values; `graphix_value_checked_<op>` shares the
/// node-walk's `Value::checked_*` core. The result is a Value: the
/// scalar, or the catchable `ArithError` (Nullable wire shape) — never
/// bottom.
pub(crate) fn emit_checked_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lcv = emit_owned_value_operand_node(cx, lhs)?;
    let rcv = emit_owned_value_operand_node(cx, rhs)?;
    let helper = match op {
        BinOp::Add => "graphix_value_checked_add",
        BinOp::Sub => "graphix_value_checked_sub",
        BinOp::Mul => "graphix_value_checked_mul",
        BinOp::Div => "graphix_value_checked_div",
        BinOp::Mod => "graphix_value_checked_rem",
    };
    let fref = cx.helper(helper)?;

    let call = cx.b.ins().call(fref, &[lcv.disc, lcv.payload, rcv.disc, rcv.payload]);
    let (rdisc, rpay) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    let disc = propagate_flags(cx.b, rdisc, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Comparison: `compile_cmp` on scalar operands; non-scalar `==`/`!=`
/// via netidx `Value` equality on owned operands. Ordering on
/// non-scalar operands is not lowered (Err, the region node-walks).
pub(crate) fn emit_cmp_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: CmpOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lprim = kernel_abi::freeze_for_abi_normalized(lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t));
    let rprim = kernel_abi::freeze_for_abi_normalized(rhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t));
    if let (Some(lp), Some(_)) = (lprim, rprim) {
        let lcv = lhs.emit_clif(cx)?;
        let rcv = rhs.emit_clif(cx)?;
        let value = compile_cmp(cx.b, op, lp, lcv.payload, rcv.payload);
        let base = scalar_disc(cx.b, PrimType::Bool);
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        return Ok(CompiledExpr::new(disc, value));
    }
    let ne = match op {
        CmpOp::Eq => false,
        CmpOp::Ne => true,
        other => {
            return Err(anyhow!(
                "emit_clif: ordering cmp {other:?} on non-scalar operands \
                 — not lowered (mirrors kernel_abi::cmp)"
            ));
        }
    };
    for t in [lhs.typ(), rhs.typ()] {
        if matches!(kernel_abi::abi_kind(t), Some(AbiKind::Unit | AbiKind::Null) | None) {
            return Err(anyhow!(
                "emit_clif: ==/!= operand of type {t:?} has no comparable \
                 runtime form (mirrors kernel_abi::cmp)"
            ));
        }
    }
    let lcv = emit_owned_value_operand_node(cx, lhs)?;
    let rcv = emit_owned_value_operand_node(cx, rhs)?;
    let helper = cx.helper("graphix_value_eq")?;

    let call = cx.b.ins().call(helper, &[lcv.disc, lcv.payload, rcv.disc, rcv.payload]);
    let eq = cx.b.inst_results(call)[0]; // I8 bool
    let result = if ne {
        let one = cx.b.ins().iconst(types::I8, 1);
        cx.b.ins().bxor(eq, one)
    } else {
        eq
    };
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
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

/// `-x`. A non-register-scalar operand (`decimal`) Errs and de-fuses.
pub(crate) fn emit_neg_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let prim = freeze_node_typ(cx.ctx, inner.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(t))
        .ok_or_else(|| {
            anyhow!("emit_neg: operand of non-scalar type {:?}", inner.typ())
        })?;
    let value = if prim.is_integer() {
        cx.b.ins().ineg(cv.payload)
    } else {
        cx.b.ins().fneg(cv.payload)
    };
    let base = scalar_disc(cx.b, prim);
    let disc = propagate_flags(cx.b, base, &[cv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// `cast<T>(x)`.
pub(crate) fn emit_cast_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    target: &Type,
    expr_id: ExprId,
) -> Result<CompiledExpr> {
    // Numeric scalar→scalar casts stay inline (branchless, infallible).
    // `compile_cast` cannot lower a bool cast; bool takes the call below.
    if let (Some(src), Some(tgt)) =
        (kernel_abi::scalar_prim(inner.typ()), PrimType::from_type(target))
    {
        if src.is_numeric() && tgt.is_numeric() {
            let cv = inner.emit_clif(cx)?;
            let value = compile_cast(cx.b, cv.payload, src, tgt);
            let base = scalar_disc(cx.b, tgt);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            // The cast node's static type is the fallible `[T, Error]`
            // union, so consumers expect the 2-word Value payload; the
            // qop unwrap narrows back.
            let payload = scalar_to_payload_i64(cx.b, tgt, value);
            return Ok(CompiledExpr::new(disc, payload));
        }
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
    emit_builtin_call_node(cx, &info, &[inner])
}

/// String interpolation `"x is [x]"`: push each part into a heap
/// `String` (string parts consumed, scalars Display-rendered) and
/// finalize to an owned ArcStr. Only String and scalar parts are
/// lowered; any other shape Errs and the subtree node-walks.
pub(crate) fn emit_string_interpolate_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    args: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let new_buf = cx.helper("graphix_string_buf_new")?;
    let call = cx.b.ins().call(new_buf, &[]);
    let buf = cx.b.inst_results(call)[0];
    // A tainted part renders harmlessly; its taint folds into the result.
    let mut part_discs: smallvec::SmallVec<[ClifValue; 8]> = smallvec::SmallVec::new();
    for a in args {
        let part = a;
        // Normalized so a select-valued part (an arm union) still classifies.
        let frozen = kernel_abi::freeze_for_abi_normalized(part.typ());
        match frozen.as_ref().and_then(|t| kernel_abi::abi_kind(t)) {
            Some(AbiKind::String) => {
                let cv = part.emit_clif(cx)?;
                part_discs.push(cv.disc);
                let push = cx.helper("graphix_string_buf_push_arcstr")?;
                cx.b.ins().call(push, &[buf, cv.payload]);
            }
            Some(AbiKind::Scalar(p)) => {
                let cv = part.emit_clif(cx)?;
                part_discs.push(cv.disc);
                let push = cx.helper(string_buf_push_helper(p))?;
                cx.b.ins().call(push, &[buf, cv.payload]);
            }
            other => {
                return Err(anyhow!(
                    "emit_clif: string-interpolate part of shape {other:?} \
                     — only String and scalar parts are lowered"
                ));
            }
        }
    }
    let finalize = cx.helper("graphix_string_buf_finalize")?;
    let call = cx.b.ins().call(finalize, &[buf]);
    let payload = cx.b.inst_results(call)[0];
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
    match kernel_abi::abi_kind(node.typ()) {
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

/// True iff a callsite whose node type is `node_typ` needs
/// [`widen_result_to_value`] on a result produced in the callee's `ret`
/// shape. `Null` is exempt: its pair is already a valid Value.
pub(crate) fn call_result_needs_value_widening(node_typ: &Type, ret: &Type) -> bool {
    matches!(
        kernel_abi::abi_kind(node_typ),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
    ) && !matches!(
        kernel_abi::abi_kind(ret),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null)
    )
}

/// Compile one producer field and emit its `graphix_value_buf_push_*`
/// call into `buf`; returns the field's disc. The Node twin of
/// `scaffold::push_field`.
fn emit_push_field_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    buf: ClifValue,
    field: &Node<R, E>,
) -> Result<ClifValue> {
    let helper_name: &str = match kernel_abi::abi_kind(field.typ()) {
        Some(AbiKind::Scalar(p)) => value_buf_push_helper(p)?,
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            match node_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_array",
                CompositeSource::Borrowed => "graphix_value_buf_push_array_borrowed",
            }
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            match node_composite_source(field) {
                CompositeSource::Owned => "graphix_value_buf_push_value",
                CompositeSource::Borrowed => "graphix_value_buf_push_value_borrowed",
            }
        }
        // String SSA is owned ArcStr bits, which `_push_string` consumes;
        // `_push_arcstr` derefs a `*const ArcStr` and would be UB here.
        Some(AbiKind::String) => "graphix_value_buf_push_string",
        Some(AbiKind::Null) => "graphix_value_buf_push_value",
        other => {
            return Err(anyhow!(
                "emit_clif: producer field of shape {other:?} — not \
                 representable"
            ));
        }
    };
    let push = cx.helper(helper_name)?;
    let cv = field.emit_clif(cx)?;
    // A tainted field does not abort the kernel: the composite comes out
    // tainted and its consumer gates. Pushing it is safe because every
    // push helper masks the tag byte before cloning the value.
    if kernel_abi::is_value_shape(field.typ())
        || matches!(kernel_abi::abi_kind(field.typ()), Some(AbiKind::Null))
    {
        cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
    } else {
        cx.b.ins().call(push, &[buf, cv.payload]);
    }
    Ok(cv.disc)
}

/// `[<a, b, c>]`: build the elements as a ValArray, then convert through
/// `graphix_valarray_into_list`. The disc is the tuple's.
pub(crate) fn emit_list_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    fields: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let cv = emit_tuple_new_node(cx, fields)?;
    let into = cx.helper("graphix_valarray_into_list")?;
    let call = cx.b.ins().call(into, &[cv.payload]);
    let payload = cx.b.inst_results(call)[1];
    Ok(CompiledExpr::new(cv.disc, payload))
}

/// Tuple / array literal: push each field, finalize into an owned
/// ValArray. Both share this emission; only the static type differs.
pub(crate) fn emit_tuple_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    fields: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let mut field_discs: smallvec::SmallVec<[ClifValue; 8]> = smallvec::SmallVec::new();
    for f in fields {
        field_discs.push(emit_push_field_node(cx, buf, f)?);
    }
    let call = cx.b.ins().call(finalize, &[buf]);
    let payload = cx.b.inst_results(call)[0];
    // Fires iff any field fired; zero fields is a constant and fires at
    // init only.
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = if field_discs.is_empty() {
        let init = cx.init_flag();
        const_stale_gate(cx.b, init, disc)
    } else {
        propagate_flags(cx.b, disc, &field_discs)
    };
    Ok(CompiledExpr::new(disc, payload))
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
    let mut indexed: smallvec::SmallVec<[(&ArcStr, &Node<R, E>); 8]> =
        names.iter().zip(fields.iter()).collect();
    indexed.sort_by(|a, b| a.0.cmp(b.0));
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
    let push_array = cx.helper("graphix_value_buf_push_array")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let outer_cap = cx.b.ins().iconst(types::I64, indexed.len() as i64);
    let call = cx.b.ins().call(buf_new, &[outer_cap]);
    let outer = cx.b.inst_results(call)[0];
    let mut field_discs: smallvec::SmallVec<[ClifValue; 8]> = smallvec::SmallVec::new();
    for (name, field) in indexed {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        // Names are interned constants; only the value discs gate freshness.
        field_discs.push(emit_push_field_node(cx, inner, field)?);
        let call = cx.b.ins().call(finalize, &[inner]);
        let inner_arr = cx.b.inst_results(call)[0];
        cx.b.ins().call(push_array, &[outer, inner_arr]);
    }
    let call = cx.b.ins().call(finalize, &[outer]);
    let payload = cx.b.inst_results(call)[0];
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, disc, &field_discs);
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
        source.typ().with_deref(|t| match t {
            Some(Type::Struct(flds)) => {
                Ok(flds.iter().map(|(n, t)| (n.clone(), t.clone())).collect())
            }
            _ => Err(anyhow!("emit_clif: struct-with source isn't a struct")),
        })?;
    let AccessorSrc { ptr: arr_ptr, ownership: src, disc: src_disc } =
        emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    // A tainted source does not abort: the reads below are guarded and
    // its taint folds into the result. An owned source is registered so
    // a bottom-abort before the finalize frees it.
    let src_var = match src {
        CompositeSource::Owned => {
            let v = cx.b.declare_var(types::I64);
            cx.b.def_var(v, arr_ptr);
            cx.ctx.owned_input_stack.borrow_mut().push(v);
            Some(v)
        }
        CompositeSource::Borrowed => None,
    };
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
    let push_array = cx.helper("graphix_value_buf_push_array")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let outer_cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let call = cx.b.ins().call(buf_new, &[outer_cap]);
    let outer = cx.b.inst_results(call)[0];
    let outer_var = cx.b.declare_var(types::I64);
    cx.b.def_var(outer_var, outer);
    cx.ctx.value_buf_stack.borrow_mut().push(outer_var);
    // Fires iff the source or any replacement fired.
    let mut field_discs: smallvec::SmallVec<[ClifValue; 8]> =
        smallvec::smallvec![src_disc];
    for (i, (name, field_typ)) in fields.iter().enumerate() {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let inner_var = cx.b.declare_var(types::I64);
        cx.b.def_var(inner_var, inner);
        cx.ctx.value_buf_stack.borrow_mut().push(inner_var);
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        match replace.iter().find(|r| r.index == Some(i)) {
            Some(r) => {
                field_discs.push(emit_push_field_node(cx, inner, &r.n)?);
            }
            None => {
                // Guarded: a tainted source's placeholder has no fields.
                let ftyp = resolve_node_typ(cx.ctx, field_typ);
                let idx = cx.b.ins().iconst(types::I64, i as i64);
                let cv = emit_guarded_element_read(
                    cx,
                    arr_ptr,
                    src_disc,
                    idx,
                    &ftyp,
                    ElementRead::StructField,
                )?;
                scaffold::push_field(cx, inner, cv, &ftyp, CompositeSource::Owned)?;
            }
        }
        let call = cx.b.ins().call(finalize, &[inner]);
        let inner_arr = cx.b.inst_results(call)[0];
        cx.ctx.value_buf_stack.borrow_mut().pop(); // inner consumed by finalize
        cx.b.ins().call(push_array, &[outer, inner_arr]);
    }
    let call = cx.b.ins().call(finalize, &[outer]);
    let payload = cx.b.inst_results(call)[0];
    cx.ctx.value_buf_stack.borrow_mut().pop(); // outer consumed by finalize
    // Dropped exactly once: the pending path drops it via `owned_input_stack`.
    if src_var.is_some() {
        let drop = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop, &[arr_ptr]);
        cx.ctx.owned_input_stack.borrow_mut().pop();
    }
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, disc, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Variant constructor. Nullary: `Value::String(tag)`, a clone of the
/// interned tag. With payloads: `Value::Array([tag, p0, ...])`.
pub(crate) fn emit_variant_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    tag: &ArcStr,
    payloads: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let tag_ptr = cx.interned_str(tag);
    if payloads.is_empty() {
        let clone_static = cx.helper("graphix_arcstr_clone_from_static")?;
        let call = cx.b.ins().call(clone_static, &[tag_ptr]);
        let bits = cx.b.inst_results(call)[0];
        let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
        // A nullary variant is a constant: fires at init only.
        let init = cx.init_flag();
        let disc = const_stale_gate(cx.b, init, base);
        Ok(CompiledExpr::new(disc, bits))
    } else {
        let buf_new = cx.helper("graphix_value_buf_new")?;
        let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
        let finalize = cx.helper("graphix_valarray_finalize")?;
        let cap = cx.b.ins().iconst(types::I64, (payloads.len() + 1) as i64);
        let call = cx.b.ins().call(buf_new, &[cap]);
        let buf = cx.b.inst_results(call)[0];
        cx.b.ins().call(push_arcstr, &[buf, tag_ptr]);
        let mut payload_discs: smallvec::SmallVec<[ClifValue; 8]> =
            smallvec::SmallVec::new();
        for p in payloads {
            payload_discs.push(emit_push_field_node(cx, buf, p)?);
        }
        let call = cx.b.ins().call(finalize, &[buf]);
        let bits = cx.b.inst_results(call)[0];
        // Fires iff any payload fired.
        let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
        let disc = propagate_flags(cx.b, base, &payload_discs);
        Ok(CompiledExpr::new(disc, bits))
    }
}

/// Drop an accessor's temporary Owned source after the element read.
fn emit_accessor_source_drop(
    cx: &mut BodyCx,
    ptr: ClifValue,
    src: CompositeSource,
) -> Result<()> {
    if matches!(src, CompositeSource::Owned) {
        let drop = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop, &[ptr]);
    }
    Ok(())
}

/// An accessor's compiled composite source. The caller drops an Owned
/// pointer after the read. The disc may carry TAINT; callers guard the
/// read and fold it.
struct AccessorSrc {
    /// ValArray bits.
    ptr: ClifValue,
    ownership: CompositeSource,
    disc: ClifValue,
}

fn emit_accessor_source_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    want: AbiKind,
) -> Result<AccessorSrc> {
    if kernel_abi::abi_kind(source.typ()) != Some(want) {
        return Err(anyhow!(
            "emit_clif: accessor source of type {:?} isn't {want:?}",
            source.typ()
        ));
    }
    let ownership = node_composite_source(source);
    let cv = source.emit_clif(cx)?;
    Ok(AccessorSrc { ptr: cv.payload, ownership, disc: cv.disc })
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
    let cv = match kernel_abi::abi_kind(elem) {
        Some(AbiKind::Scalar(p)) => {
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            CompiledExpr::new(disc, zero_const(cx.b, p))
        }
        Some(AbiKind::String) => {
            let helper = cx.helper("graphix_arcstr_empty")?;
            let call = cx.b.ins().call(helper, &[]);
            let s = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT);
            CompiledExpr::new(disc, s)
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let helper = cx.helper("graphix_valarray_empty")?;
            let call = cx.b.ins().call(helper, &[]);
            let a = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT);
            CompiledExpr::new(disc, a)
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
        | Some(AbiKind::Unit) => {
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let zero = cx.b.ins().iconst(types::I64, 0);
            CompiledExpr::new(disc, zero)
        }
        other => {
            return Err(anyhow!("emit_clif: no placeholder for shape {other:?}"));
        }
    };
    let disc = propagate_flags(cx.b, cv.disc, governing_discs);
    Ok(CompiledExpr::new(disc, cv.payload))
}

/// Element read guarded on the source's taint: a tainted source holds
/// a placeholder the unchecked read helpers cannot touch, so the read
/// is skipped for a tainted placeholder. `is_tainted` folds to false
/// for proven-untainted sources, so the branch disappears.
fn emit_guarded_element_read(
    cx: &mut BodyCx,
    arr_ptr: ClifValue,
    src_disc: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    read: ElementRead,
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
    let rv = compile_element_read(cx.b, arr_ptr, idx_val, elem, read, cx.ctx)?;
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
    id: AbstractId,
    name: &ArcStr,
    arg: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = emit_owned_value_operand_node(cx, arg)?;
    let wrap = cx.helper("graphix_abstract_wrap")?;
    let id = cx.b.ins().iconst(types::I64, id.inner() as i64);
    let name_ptr = cx.interned_str(name);
    let call = cx.b.ins().call(wrap, &[id, name_ptr, cv.disc, cv.payload]);
    let (rdisc, rpay) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    let disc = propagate_flags(cx.b, rdisc, &[cv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `x.0` on a Graphix-minted abstract value: a guarded, borrowed read
/// of the payload at the representation's shape `rep` (the
/// abstract twin of [`emit_guarded_element_read`]).
pub(crate) fn emit_abstract_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    rep: &Type,
) -> Result<CompiledExpr> {
    let rep = resolve_node_typ(cx.ctx, rep);
    let helper = cx.helper(abstract_read_helper(&rep)?)?;
    let src = node_composite_source(source);
    let cv = source.emit_clif(cx)?;
    let tainted = is_tainted(cx.b, cv.disc);
    let read_bl = cx.b.create_block();
    let skip_bl = cx.b.create_block();
    let merge = cx.b.create_block();
    let pay_ty = match kernel_abi::abi_kind(&rep) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, types::I64);
    cx.b.append_block_param(merge, pay_ty);
    cx.b.ins().brif(tainted, skip_bl, &[], read_bl, &[]);
    cx.b.switch_to_block(read_bl);
    cx.b.seal_block(read_bl);
    let call = cx.b.ins().call(helper, &[cv.disc, cv.payload]);
    let rv = if kernel_abi::is_value_shape(&rep) {
        let r = cx.b.inst_results(call);
        CompiledExpr::new(r[0], r[1])
    } else {
        let r0 = cx.b.inst_results(call)[0];
        CompiledExpr::new(kind_disc(cx.b, &rep), r0)
    };
    cx.b.ins().jump(merge, &[BlockArg::Value(rv.disc), BlockArg::Value(rv.payload)]);
    cx.b.switch_to_block(skip_bl);
    cx.b.seal_block(skip_bl);
    let ph = emit_bottom_placeholder(cx, &rep, &[cv.disc])?;
    cx.b.ins().jump(merge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let (rdisc, rpay) = {
        let params = cx.b.block_params(merge);
        (params[0], params[1])
    };
    if matches!(src, CompositeSource::Owned) {
        let drop = cx.helper("graphix_value_drop")?;
        cx.b.ins().call(drop, &[cv.disc, cv.payload]);
    }
    let disc = propagate_flags(cx.b, rdisc, &[cv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `t.<idx>`: a statically-valid index read through `compile_element_read`.
pub(crate) fn emit_tuple_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Elem types may be Refs to abstract type names; resolve before classifying.
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let AccessorSrc { ptr: arr_ptr, ownership: src, disc: src_disc } =
        emit_accessor_source_node(cx, source, AbiKind::Tuple)?;
    let idx_const = cx.b.ins().iconst(types::I64, idx as i64);
    let result = emit_guarded_element_read(
        cx,
        arr_ptr,
        src_disc,
        idx_const,
        &elem_typ,
        ElementRead::ArrayIndex,
    )?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    // The element read's disc is fresh; the source's STALE gates it.
    let disc = propagate_flags(cx.b, result.disc, &[src_disc]);
    Ok(CompiledExpr::new(disc, result.payload))
}

/// `s.field`: the kv-pair read via the `struct_get_*` helpers;
/// `sorted_idx` is the field's position in the sorted layout.
pub(crate) fn emit_struct_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    sorted_idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Same abstract-Ref resolution as the tuple read.
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let AccessorSrc { ptr: arr_ptr, ownership: src, disc: src_disc } =
        emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    let idx_const = cx.b.ins().iconst(types::I64, sorted_idx as i64);
    let result = emit_guarded_element_read(
        cx,
        arr_ptr,
        src_disc,
        idx_const,
        &elem_typ,
        ElementRead::StructField,
    )?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    // The element read's disc is fresh; the source's STALE gates it.
    let disc = propagate_flags(cx.b, result.disc, &[src_disc]);
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
        .ok_or_else(|| anyhow!("emit_clif: index of non-integer type {:?}", idx.typ()))?;
    if matches!(kernel_abi::abi_kind(source.typ()), Some(AbiKind::Array)) {
        // Unforced: the helper is bounds-checked (safe on a placeholder)
        // and the source's taint folds into the result below.
        let AccessorSrc { ptr: arr_ptr, ownership: src, disc: src_disc } =
            emit_accessor_source_node(cx, source, AbiKind::Array)?;
        let idx_cv = idx.emit_clif(cx)?;
        let idx_i64 = widen_to_i64(cx.b, idx_cv.payload, idx_prim)?;
        let helper = cx.helper("graphix_valarray_index")?;
        let call = cx.b.ins().call(helper, &[arr_ptr, idx_i64]);
        let r = cx.b.inst_results(call);
        let (rdisc, rpay) = (r[0], r[1]);
        emit_accessor_source_drop(cx, arr_ptr, src)?;
        // Fires iff the array or the index fired.
        let disc = propagate_flags(cx.b, rdisc, &[src_disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    if lowering::is_bytes(source.typ()) {
        // The helper consumes the bytes operand.
        let bcv = emit_owned_value_operand_node(cx, source)?;
        let idx_cv = idx.emit_clif(cx)?;
        // The helper takes an i64; a narrow payload fails cranelift's verifier.
        let idx_i64 = widen_to_i64(cx.b, idx_cv.payload, idx_prim)?;
        let helper = cx.helper("graphix_bytes_index")?;
        let call = cx.b.ins().call(helper, &[bcv.disc, bcv.payload, idx_i64]);
        let (rdisc, rpay) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        // Fires iff the bytes or the index fired.
        let disc = propagate_flags(cx.b, rdisc, &[bcv.disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    Err(anyhow!(
        "emit_clif: index source of type {:?} isn't an array or bytes",
        source.typ()
    ))
}

/// `m{key}`: both operands owned; `graphix_map_ref` shares
/// `node::map::map_get` and returns `Nullable<V>`.
pub(crate) fn emit_map_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    key: &Node<R, E>,
) -> Result<CompiledExpr> {
    if !lowering::is_map(source.typ()) {
        return Err(anyhow!(
            "emit_clif: map-ref source of type {:?} isn't a Map",
            source.typ()
        ));
    }
    let mcv = emit_owned_value_operand_node(cx, source)?;
    let kcv = emit_owned_value_operand_node(cx, key)?;
    let helper = cx.helper("graphix_map_ref")?;

    let call = cx.b.ins().call(helper, &[mcv.disc, mcv.payload, kcv.disc, kcv.payload]);
    let (rdisc, rpay) = {
        let r = cx.b.inst_results(call);
        (r[0], r[1])
    };
    // Fires iff the map or the key fired.
    let disc = propagate_flags(cx.b, rdisc, &[mcv.disc, kcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// `a[i..j]` — the source as an OWNED Value (the helper consumes it),
/// present bounds as integer scalars with a flag bit each, absent
/// bounds pass 0 with the bit cleared. Result is `Nullable<source>`
/// (shared `node::array::array_slice` semantics).
pub(crate) fn emit_array_slice_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    start: Option<&Node<R, E>>,
    end: Option<&Node<R, E>>,
) -> Result<CompiledExpr> {
    if !(matches!(kernel_abi::abi_kind(source.typ()), Some(AbiKind::Array))
        || lowering::is_bytes(source.typ()))
    {
        return Err(anyhow!(
            "emit_clif: slice source of type {:?} isn't an array or bytes",
            source.typ()
        ));
    }
    let scv = emit_owned_value_operand_node(cx, source)?;
    let mut taint_discs: smallvec::SmallVec<[ClifValue; 8]> =
        smallvec::smallvec![scv.disc];
    let emit_bound = |cx: &mut BodyCx,
                      n: Option<&Node<R, E>>,
                      flag: i64,
                      flags: &mut i64,
                      taint: &mut smallvec::SmallVec<[ClifValue; 8]>|
     -> Result<ClifValue> {
        match n {
            None => Ok(cx.b.ins().iconst(types::I64, 0)),
            Some(n) => {
                let Some(p) = kernel_abi::scalar_prim(n.typ()).filter(|p| p.is_integer())
                else {
                    return Err(anyhow!(
                        "emit_clif: slice bound of non-integer type {:?}",
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
    let start_v = emit_bound(cx, start, 1, &mut flags, &mut taint_discs)?;
    let end_v = emit_bound(cx, end, 2, &mut flags, &mut taint_discs)?;
    let flags_v = cx.b.ins().iconst(types::I64, flags);
    let helper = cx.helper("graphix_array_slice")?;
    let call = cx.b.ins().call(helper, &[scv.disc, scv.payload, start_v, end_v, flags_v]);
    let r = cx.b.inst_results(call);
    let (rdisc, rpay) = (r[0], r[1]);
    // Fires iff the source or any present bound fired.
    let disc = propagate_flags(cx.b, rdisc, &taint_discs);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Node analog of `kernel_abi::int_div_may_bottom`: false only when the
/// divisor is a constant that provably cannot bottom. Sees through
/// `ExplicitParens`.
fn node_int_div_may_bottom<R: Rt, E: UserEvent>(
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> bool {
    use NodeView;
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
