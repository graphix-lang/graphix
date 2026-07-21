//! Per-node value emitters: constants, refs, scalar operators,
//! casts, connect, string interpolation, and the composite
//! producers/accessors (tuple/struct/variant/array/map).

use crate::{
    BindId, Node, NodeView, Rt, UserEvent,
    expr::{Expr, ExprId},
    fusion::{
        kernel_abi::{self, AbiKind, AbstractRegistry, PrimType},
        lowering::{self},
    },
    node::{
        Cached,
        op::{BinOp, BoolOp, CmpOp},
    },
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    BlockArg, InstBuilder, MemFlags, Value as ClifValue, condcodes::IntCC, types,
};
use netidx_value::Value;

use super::{
    abi::{
        CompiledExpr, LocalKind, TAINT, const_stale_gate, emit_scalar_taint_cache,
        is_tainted, prim_to_value_disc, propagate_flags, scalar_disc, value_disc,
    },
    body::{
        BodyCx, ensure_owned_composite_src, ensure_owned_value_src,
        node_composite_source, ref_local_name,
    },
    call::{CompositeSource, emit_dyncall_node},
    lower::{freeze_node_typ, resolve_node_typ},
    scaffold,
    scalar::{
        compile_bin, compile_cast, compile_cmp, compile_const, compile_element_read,
        prim_to_clif, scalar_to_payload_i64, string_buf_push_helper,
        value_buf_push_helper, widen_to_i64, zero_const,
    },
};

// ─── Distributed node-emission relays ─────────────────────────────
//
// The per-node `Update::emit_clif` impls (node/*.rs) are thin shims
// over these crate-internal helpers, which own the CLIF mechanics
// (`JitEnv`/`LowerCtx` stay private to this module).

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
    // A literal fires only at init, then carries a cached (stale) value
    // (see `const_stale_gate`). The disc gate is the only flag change —
    // a constant is never tainted.
    let init = cx.init_flag();
    match kernel_abi::abi_kind(cx.registry(), typ) {
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

/// A `{k => v, ...}` map literal. Mirrors the classic path's
/// `emit_map_new`: fuses ONLY when every key and value is a
/// compile-time constant — the `CMap` is built at compile time
/// (`insert_cow` in entry order, exactly as `Map::update` does at
/// runtime) and emitted as an interned Value constant. A dynamic
/// entry de-fuses; the runtime map producer isn't lowered on either
/// path.
pub(crate) fn emit_map_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    keys: &[Cached<R, E>],
    vals: &[Cached<R, E>],
    typ: &Type,
) -> Result<CompiledExpr> {
    let v = lowering::const_map(keys, vals).ok_or_else(|| {
        anyhow!(
            "emit_clif: map literal with non-constant entries — \
             subtree node-walks"
        )
    })?;
    let typ = kernel_abi::freeze_for_abi(cx.registry(), typ)
        .unwrap_or_else(kernel_abi::map_type);
    emit_const_node(cx, &v, &typ)
}

/// A binding read. Resolve the Ref's source name to the kernel param
/// / block-let slot in the env (same name the params were bound under
/// — see `compile_into_function`'s entry binder). Surfaces the local's
/// disc (carrying any `TAINT`/`STALE`) alongside its payload.
pub(crate) fn emit_ref_node(
    cx: &mut BodyCx,
    spec: &Expr,
    typ: &Type,
    id: BindId,
) -> Result<CompiledExpr> {
    // Resolve BindId-first (exact under shadowing — an outer capture
    // and an inner let sharing a basename resolve to different slots,
    // the #162/#167 bug class), then by name (id-less synthetic
    // locals). The disc already carries the binding's #219 taint (a
    // tainted param disc from the ABI, or a let's computed disc).
    let _ = typ;
    let name = ref_local_name(spec)
        .ok_or_else(|| anyhow!("emit_clif: Ref spec isn't an ExprKind::Ref"))?;
    let (vv, kind) = {
        if cx.env.lookup(id, name).is_none()
            && std::env::var_os("GXDBG_REFMISS").is_some()
        {
            eprintln!(
                "REFMISS `{name}` id={id:?} locals={:?}",
                cx.env
                    .locals
                    .iter()
                    .map(|l| (l.name.as_str(), l.bind_id))
                    .collect::<Vec<_>>()
            );
        }
        let l = cx
            .env
            .lookup(id, name)
            .ok_or_else(|| anyhow!("emit_clif: undefined local `{name}` ({id:?})"))?;
        (l.vv, l.kind)
    };
    let disc = cx.b.use_var(vv.disc);
    match kind {
        // String: read the slot and refcount-bump — each consumer gets
        // an independently-owned ArcStr; the slot keeps its own ref
        // until scope exit.
        LocalKind::String => {
            let s = cx.b.use_var(vv.payload);
            let clone = cx.helper("graphix_arcstr_clone")?;
            let call = cx.b.ins().call(clone, &[s]);
            Ok(CompiledExpr::new(disc, cx.b.inst_results(call)[0]))
        }
        // Scalar: read both words. Composite / Variant / Nullable /
        // Value: BORROWED read — the env still owns the slot; consumers
        // clone via `ensure_owned_*_src` when they need ownership.
        LocalKind::Scalar(_)
        | LocalKind::Composite
        | LocalKind::Variant
        | LocalKind::Nullable
        | LocalKind::Value => Ok(CompiledExpr::new(disc, cx.b.use_var(vv.payload))),
    }
}

/// Arithmetic — compile both operands, then the shared `compile_bin`
/// helper (including the div/mod taint/guard), propagating operand
/// validity. A datetime/duration operand
/// routes to the `ValueArith` mirror first (netidx `Value` arithmetic
/// via the `graphix_value_<op>` helpers, both operands OWNED since the
/// helpers consume).
pub(crate) fn emit_arith_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: BinOp,
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
        // Clean the operand discs before the netidx-Value helper (a
        // tainted disc is an invalid tag); the helper ran on the value
        // bits regardless. #219: the result disc re-absorbs the operands'
        // taint, guarding a garbage result a missing input produced.

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
    // Fallible prim derivation (not `prim_of`, which panics): an
    // operand's type may be typecheck's un-normalized union (a select
    // result) — scalar after `freeze_for_abi_normalized` (with the abstract-
    // Ref resolution retry, #218), or Err → no fusion.
    let prim = freeze_node_typ(cx.ctx, lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
        .ok_or_else(|| {
            anyhow!("emit_clif: arith operand of non-scalar type {:?}", lhs.typ())
        })?;
    let base = scalar_disc(cx.b, prim);
    // Integer div/mod taint/guard.
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
        // #219: a div0 / signed-MIN÷-1 taints the result; so does a
        // tainted operand. `is_tainted` resolves at the output.
        let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
        let taint_word = cx.b.ins().iconst(types::I64, TAINT);
        let zero = cx.b.ins().iconst(types::I64, 0);
        let bad_taint = cx.b.ins().select(bad, taint_word, zero);
        let disc = cx.b.ins().bor(disc, bad_taint);
        // Interior-bottom exactness: with prior history this degrades a
        // tainted result to STALE + the cached value, matching the
        // node-walk's cached div node.
        return Ok(emit_scalar_taint_cache(cx, prim, CompiledExpr::new(disc, value)));
    }
    let value = compile_bin(cx.b, op, prim, l, r)?;
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, value))
}

/// Checked arithmetic (`+?` / `-?` / `*?` / `/?` / `%?`). Both
/// operands are compiled as OWNED `(disc, payload)` Values (the same
/// route as `emit_arith_node`'s ValueArith dispatch — the helpers
/// consume), then the `graphix_value_checked_<op>` helper computes via
/// the SAME netidx `Value::checked_*` + [`op::
/// wrap_arith_error`] core the node-walk's update uses. The result is
/// a Value: the success scalar, or the catchable `ArithError` error
/// VALUE (`[T, Error<`ArithError(string)>]` freezes to the Nullable
/// wire shape) — never bottom, unlike unchecked div0.
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
    // #219: propagate operand taint into the checked-arith result Value.
    let disc = propagate_flags(cx.b, rdisc, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Comparison — `compile_cmp` (total-order floats) on scalar operands,
/// propagating operand validity. Non-scalar `==`/`!=` (String,
/// composite, value-shape) compiles both operands as OWNED
/// `(disc, payload)` Values (the helper consumes them),
/// compared via netidx `Value` PartialEq. Ordering operators on
/// non-scalar operands aren't lowered (mirrors `kernel_abi::cmp`) — Err, the
/// region node-walks.
pub(crate) fn emit_cmp_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    op: CmpOp,
    lhs: &Node<R, E>,
    rhs: &Node<R, E>,
) -> Result<CompiledExpr> {
    let lprim = kernel_abi::freeze_for_abi_normalized(cx.registry(), lhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t));
    let rprim = kernel_abi::freeze_for_abi_normalized(cx.registry(), rhs.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t));
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
        if matches!(
            kernel_abi::abi_kind(cx.registry(), t),
            Some(AbiKind::Unit | AbiKind::Null) | None
        ) {
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
    // #219: a tainted operand taints the bool result.
    let base = scalar_disc(cx.b, PrimType::Bool);
    let disc = propagate_flags(cx.b, base, &[lcv.disc, rcv.disc]);
    Ok(CompiledExpr::new(disc, result))
}

/// Logical — STRICT `band`/`bor` (both operands always compiled),
/// taint-propagating, matching the node-walk's `bool_op!`.
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

/// Logical NOT — taint-propagating.
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

/// `-x` — integer `ineg` / float `fneg`, taint-propagating. The operand's
/// PrimType drives int-vs-float; a non-register-scalar operand (e.g.
/// `decimal`) has no `scalar_prim` and Errs, de-fusing to the node-walk.
pub(crate) fn emit_neg_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let prim = freeze_node_typ(cx.ctx, inner.typ())
        .as_ref()
        .and_then(|t| kernel_abi::scalar_prim(cx.registry(), t))
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

/// `cast<T>(x)` — `compile_cast`, taint-propagating.
pub(crate) fn emit_cast_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    target: &Type,
    expr_id: ExprId,
) -> Result<CompiledExpr> {
    // Scalar→scalar fast path: pure register arithmetic, branchless,
    // infallible — a cast in a hot loop (e.g. `cast<f64>(2*n-1)`) must
    // stay inline, not pay a runtime call per iteration. Restricted to
    // NUMERIC prims: `compile_cast` can't lower a `bool` cast (it
    // `unreachable!`s), so a bool source/target falls through to the
    // machinery DynCall below, which casts via `Value::cast`.
    if let (Some(src), Some(tgt)) =
        (kernel_abi::scalar_prim(cx.registry(), inner.typ()), PrimType::from_type(target))
    {
        if src.is_numeric() && tgt.is_numeric() {
            let cv = inner.emit_clif(cx)?;
            let value = compile_cast(cx.b, cv.payload, src, tgt);
            let base = scalar_disc(cx.b, tgt);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            // The cast NODE's static type is the fallible `[T, Error]`
            // union, so every consumer classifies it as a 2-word Value
            // and expects the I64 payload word — a raw F64-typed result
            // here type-mismatched `bind_local`'s declared payload var
            // (cranelift frontend panic; jul17c katana divergence
            // 000000: `let v = cast<f64>(f64:0.)`). `$`/`?` only worked
            // by accident (same-width bitcast is an identity for F64,
            // passthrough for I64). Widen to the wire shape; the qop
            // unwrap narrows back with `cast_u64_to_prim`.
            let payload = scalar_to_payload_i64(cx.b, tgt, value);
            return Ok(CompiledExpr::new(disc, payload));
        }
    }
    // Any other cast (non-scalar source like `datetime`, or non-scalar
    // target): the discovery pass registered a `FnSource::Cast` slot —
    // lower it as a one-argument DynCall to the cast machinery
    // (`CastApply` → `target.cast_value`, the SAME fn the node-walk
    // uses). The fallible `[T, Error]` result rides the 2-word Value
    // wire shape; a surrounding `$`/`?` unwraps it via `emit_qop_node`.
    let info = match cx.builtin_site(expr_id) {
        Some(i) => i.clone(),
        None => {
            return Err(anyhow!(
                "emit_clif: cast site {expr_id:?} not discovered — doesn't fuse"
            ));
        }
    };
    emit_dyncall_node(cx, &info, &[inner])
}

/// `connect` (`x <- expr`) fused: compute the RHS and write the reactive
/// variable mid-kernel via `graphix_set_var`. The write is a side
/// effect (the node returns bottom — `Connect::update` returns `None`),
/// and the read side is unchanged: a downstream region reading the
/// written variable already sees it next cycle through its feeder Ref
/// (keyed to `fusion.top_id`). A `#219`-tainted RHS is skipped by the
/// helper (no value this cycle = no write), mirroring the node-walk's
/// `if let Some(v) = ..` guard.
///
/// The RHS is marshaled to an OWNED `(disc, payload)` Value of any shape
/// (`emit_owned_value_operand_node`) and handed to `graphix_set_var`, which
/// CONSUMES it — or drops it on a tainted/stale skip. So a composite/string
/// RHS (`data <- array::push(data, x)`) fuses without a leak, uniform with the
/// scalar case.
pub(crate) fn emit_connect_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    rhs: &Node<R, E>,
    bind_id: BindId,
) -> Result<CompiledExpr> {
    // Read-after-write-same-var guard: if the connect TARGET is a
    // kernel-local (let-bound inside this region) that was NOT lifted, a
    // read of it in the same kernel resolves to the stale local value,
    // not the written one — de-fuse (the block node-walks, correct). A
    // LIFTED target is the local-counter case: it was routed in as a
    // feeder and `emit_let_node` bound it to a seed-select reading that
    // feeder, so a read DOES see the variable's value. The fired-bit
    // makes the write correct (`set_var_typed` skips a non-fired RHS), so
    // the lifted connect fuses. A connect to an EXTERNAL variable (not in
    // the env at all) also fuses.
    if cx.env.lookup(bind_id, "").is_some() && !cx.is_lifted(bind_id) {
        return Err(anyhow!(
            "emit_clif: connect target is a non-lifted kernel-local — \
             read-after-write unsafe, node-walks"
        ));
    }
    // Marshal the RHS to an OWNED `(disc, payload)` Value of any shape; a
    // scalar's `(disc, payload)` is already its Value wire form (payload
    // widened to i64). `set_var` consumes the payload (or drops it on the
    // tainted/stale skip), so a composite/string RHS transfers ownership
    // without leaking; #219 taint / STALE ride `cv.disc` and the helper honors
    // them (skip the write).
    let cv = emit_owned_value_operand_node(cx, rhs)?;
    // A LIFTED target's identity is per-INSTANCE: load the BindId from
    // the reserved state word written at construction. External targets
    // are shared by design and stay immediates.
    let id_val = match cx.lifted_state_off(bind_id) {
        Some(off) => {
            let sp = cx.state_ptr();
            cx.b.ins().load(types::I64, MemFlags::trusted(), sp, off)
        }
        None => cx.b.ins().iconst(types::I64, bind_id.inner() as i64),
    };
    let set_var = cx.helper("graphix_set_var")?;
    cx.b.ins().call(set_var, &[id_val, cv.disc, cv.payload]);
    // `connect` produces no value — a tainted-null bottom. Discarded as a
    // block statement (a connect is never a kernel's published result;
    // its `typ()` is `Bottom`).
    let disc = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
    let zero = cx.b.ins().iconst(types::I64, 0);
    Ok(CompiledExpr::new(disc, zero))
}

/// String interpolation `"x is [x]"` — build a heap-owned
/// `*mut String`,
/// push each part (append-as-str for string parts — reads are already
/// owned clones, the push consumes; Display-rendered for scalars via
/// the shared [`string_buf_push_helper`]), finalize into an OWNED
/// ArcStr. Keeps the restriction on part shapes: a non-scalar /
/// non-string part (a Nullable from `a[i]`, a composite, a value-shape
/// — see findings "StringInterpolate non-scalar part") is Err, the
/// subtree node-walks.
pub(crate) fn emit_string_interpolate_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    args: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    let new_buf = cx.helper("graphix_string_buf_new")?;
    let call = cx.b.ins().call(new_buf, &[]);
    let buf = cx.b.inst_results(call)[0];
    // #219: a tainted part (a div0 inside `[..]`) renders harmlessly into
    // the buffer; its taint accumulates and forces the result at the
    // output. Collect part discs and fold them into the result disc.
    let mut part_discs: Vec<ClifValue> = Vec::with_capacity(args.len());
    for a in args {
        let part = &a.node;
        // `freeze_for_abi_normalized` so a select-valued part (whose type is
        // the un-normalized arm union) still classifies.
        let frozen = kernel_abi::freeze_for_abi_normalized(cx.registry(), part.typ());
        match frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t)) {
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

/// Compile a `ValueArith` / `ValueEq` / bytes / map / slice operand as
/// an OWNED `(disc, payload)` Value — the consuming helpers take both
/// operands by value. A value-shape operand clones a Borrowed read via
/// `ensure_owned_value_src`; a scalar widens its payload to the 8-byte
/// Value word by inline packing (its disc already IS the Value disc);
/// a String's owned ArcStr bits and a composite's owned ValArray bits
/// ARE the Value payload word under the unified Value ABI — only the
/// disc is minted, with the source's TAINT/STALE folded on.
pub(crate) fn emit_owned_value_operand_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<CompiledExpr> {
    match kernel_abi::abi_kind(cx.registry(), node.typ()) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            // #219: the disc carries the operand's taint through. A missing
            // 2-word input is a `Value::Null` placeholder (nonzero disc), so
            // clone and the value-arith helpers run harmlessly on it; the
            // taint guards the garbage result, which the consumer resolves.
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
            // A scalar's disc IS its value disc (`scalar_disc` ==
            // `prim_to_value_disc`), so taint carries forward unchanged;
            // only the payload is widened to the 8-byte Value word.
            let cv = node.emit_clif(cx)?;
            let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
            Ok(CompiledExpr::new(cv.disc, payload))
        }
        Some(AbiKind::String) => {
            // Const/Ref/Concat reads all produce an owned ArcStr, and
            // its bits ARE `Value::String`'s payload word (unified
            // Value ABI) — mint the disc inline. Fold the source's
            // TAINT/STALE on, else a placeholder input's garbage
            // ("" at init) computes an untainted result that escapes the
            // output gate.
            let cv = node.emit_clif(cx)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            Ok(CompiledExpr::new(disc, cv.payload))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            // Owned ValArray bits ARE `Value::Array`'s payload word
            // (unified Value ABI) — mint the disc inline. Same flag
            // fold as the String arm: without it, slicing the
            // EMPTY placeholder of a not-yet-fired array input emitted a
            // real ArrayIndexError value on the init cycle (soak finding
            // divergence_000004, 2026-07-03).
            let cv = node.emit_clif(cx)?;
            let bits =
                ensure_owned_composite_src(cx, node_composite_source(node), cv.payload)?;
            let base = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            Ok(CompiledExpr::new(disc, bits))
        }
        // A Null node's raw (disc, payload) IS a valid Value pairing
        // (`Value::Null` never reads its payload word, and dropping it
        // is a no-op) — same treatment as `emit_push_field_node`.
        Some(AbiKind::Null) => node.emit_clif(cx),
        other => Err(anyhow!("emit_clif: value operand has unexpected type {other:?}")),
    }
}

/// Widen a call result emitted per the CALLEE's return shape into the
/// owned `(disc, payload)` Value the callsite NODE's type promises its
/// consumers. Inference may widen a call expression's type to a union
/// the callee's return type is one member of (`f(g())` where `g`
/// returns `Array<i64>` and `f`'s param is `[null, Array<i64>]`);
/// every consumer classifies the node by `node.typ()` (value-shape),
/// so an emission left in the callee's own shape hands out a raw
/// ValArray box pointer as a Value payload (jul18d fuzz crash_000000:
/// the runtime later dereferenced box header words as a ThinArc).
/// Ownership: the produced result is owned (call results and loop
/// finalizes always are), and the wrap helpers CONSUME it — the
/// resulting Value is owned, matching how value-shaped consumers
/// (arg drops, scope drops, return clones) already treat callsites.
/// Flags (TAINT/STALE) fold from the produced disc onto the fresh one.
pub(crate) fn widen_result_to_value(
    cx: &mut BodyCx,
    produced: &Type,
    cv: CompiledExpr,
) -> Result<CompiledExpr> {
    match kernel_abi::abi_kind(cx.registry(), produced) {
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null) => {
            Ok(cv)
        }
        Some(AbiKind::Scalar(p)) => {
            // The scalar's disc IS its value disc — only the payload
            // widens to the 8-byte Value word.
            let payload = scalar_to_payload_i64(cx.b, p, cv.payload);
            Ok(CompiledExpr::new(cv.disc, payload))
        }
        Some(AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            // Unified Value ABI: a composite/string result's pair is
            // ALREADY a genuine Value — the disc carries
            // ARRAY/STRING|flags and the payload word is the
            // ValArray/ArcStr bits. Identity.
            Ok(cv)
        }
        other => Err(anyhow!(
            "emit_clif: call result shape {other:?} cannot widen to a \
             value-typed node — subtree node-walks"
        )),
    }
}

/// True iff a callsite whose node type is `node_typ` needs
/// [`widen_result_to_value`] applied to a result produced per the
/// callee's `ret` shape: the node promises a 2-word Value while the
/// callee ABI delivers its own narrower encoding. `Null` is exempt —
/// a Null (disc, payload) already IS a valid Value pairing.
pub(crate) fn call_result_needs_value_widening(
    reg: &AbstractRegistry,
    node_typ: &Type,
    ret: &Type,
) -> bool {
    matches!(
        kernel_abi::abi_kind(reg, node_typ),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
    ) && !matches!(
        kernel_abi::abi_kind(reg, ret),
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value | AbiKind::Null)
    )
}

/// Compile one producer-op field and emit the matching
/// `graphix_value_buf_push_*` call into `buf` — the Node twin of
/// `scaffold::push_field` (same helper choice per shape, same
/// owned/borrowed push variant via `node_composite_source`, same
/// bottom-abort for a may-bottom (tainted-disc) field).
fn emit_push_field_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    buf: ClifValue,
    field: &Node<R, E>,
) -> Result<ClifValue> {
    let helper_name: &str = match kernel_abi::abi_kind(cx.registry(), field.typ()) {
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
        // String SSA is the ArcStr's raw thin-pointer bits (owned);
        // `_push_string` takes it by value (consumes). `_push_arcstr`
        // (which derefs a `*const ArcStr`) would be UB here.
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
    // A tainted (bottom) field does NOT abort the kernel: the node-walk
    // only bottoms the PRODUCER node (nothing downstream that doesn't
    // read it is affected), so the composite must come out TAINTED, not
    // the whole kernel bottom — the caller's `propagate_flags` ORs the
    // field discs into the result disc, and the output path forces
    // bottom only if it CONSUMES the tainted composite (#219). Pushing
    // the tainted field is safe: its payload is the helper-safe
    // placeholder, and every push helper goes through the `TagValue`
    // gateway, which MASKS the tag byte before the value is cloned or
    // stored (a tainted disc can never materialize as a corrupt
    // `Value`). Previously this aborted to `pending_exit`, escalating a
    // locally-unconsumed bottom into whole-kernel bottom
    // (fuzz/triage-fuzzer-v2/divergence_000001: an UNUSED tuple binding
    // with a bottom element bottomed an unrelated const output).
    if kernel_abi::is_value_shape(cx.registry(), field.typ())
        || matches!(kernel_abi::abi_kind(cx.registry(), field.typ()), Some(AbiKind::Null))
    {
        cx.b.ins().call(push, &[buf, cv.disc, cv.payload]);
    } else {
        cx.b.ins().call(push, &[buf, cv.payload]);
    }
    // Return the field's disc so the composite result can OR-reduce
    // TAINT and AND-reduce STALE (a composite bottoms if any field
    // bottomed, and fires iff any field fired).
    Ok(cv.disc)
}

/// Tuple / array literal — build a `Vec<Value>` field-by-field via the
/// producer helpers, then finalize into an owned `*mut ValArray`.
/// Tuples and array literals share this emission — the runtime shape
/// is identical, only the static type differs.
pub(crate) fn emit_tuple_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    fields: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let cap = cx.b.ins().iconst(types::I64, fields.len() as i64);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let mut field_discs = Vec::with_capacity(fields.len());
    for f in fields {
        field_discs.push(emit_push_field_node(cx, buf, &f.node)?);
    }
    let call = cx.b.ins().call(finalize, &[buf]);
    let payload = cx.b.inst_results(call)[0];
    // The composite fires iff any field fired → STALE = AND(field stales).
    // ZERO fields (`[]`) is a constant: fires at init only, like
    // `emit_const_node` — the bare ARRAY disc read as fired-every-
    // invocation, so an empty literal feeding a HOF re-fired the HOF's
    // `src_fired ∧ empty` rule on every unrelated kernel input event
    // (soak jul07d divergence_000000: a dead select arm's captured
    // async feeder re-fired `find([], …)` through the enclosing map —
    // interp 1, jit 5).
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = if field_discs.is_empty() {
        let init = cx.init_flag();
        const_stale_gate(cx.b, init, disc)
    } else {
        propagate_flags(cx.b, disc, &field_discs)
    };
    Ok(CompiledExpr::new(disc, payload))
}

/// Struct literal — an outer ValArray of inner `[name, value]` pairs,
/// fields sorted alphabetically by name (graphix's canonical struct
/// layout). Field names are interned lazily via
/// [`BodyCx::interned_str`].
pub(crate) fn emit_struct_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    names: &[ArcStr],
    fields: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    if names.len() != fields.len() {
        return Err(anyhow!("emit_clif: struct literal name/field arity mismatch"));
    }
    let mut indexed: Vec<(&ArcStr, &Cached<R, E>)> =
        names.iter().zip(fields.iter()).collect();
    indexed.sort_by(|a, b| a.0.cmp(b.0));
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let push_arcstr = cx.helper("graphix_value_buf_push_arcstr")?;
    let push_array = cx.helper("graphix_value_buf_push_array")?;
    let finalize = cx.helper("graphix_valarray_finalize")?;
    let outer_cap = cx.b.ins().iconst(types::I64, indexed.len() as i64);
    let call = cx.b.ins().call(buf_new, &[outer_cap]);
    let outer = cx.b.inst_results(call)[0];
    let mut field_discs = Vec::with_capacity(indexed.len());
    for (name, field) in indexed {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        // The struct fires iff any field VALUE fired — the names are
        // interned constants, so only the value discs gate freshness.
        field_discs.push(emit_push_field_node(cx, inner, &field.node)?);
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

/// `{ source with f: v, ... }` — build a NEW struct that copies the
/// source's sorted `[name, value]` pairs, overriding the replaced
/// fields. Mirrors [`emit_struct_new_node`], but each unchanged field's
/// value is READ from the source struct (one source read, N element
/// reads) while a replaced field emits its replacement node. `Replace.index`
/// is the field's sorted position (set by typecheck0). The outer/inner
/// bufs and an Owned source are registered for pending-exit cleanup so a
/// may-bottom replacement (`{s with x: a / b}`) frees them instead of
/// leaking.
pub(crate) fn emit_struct_with_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    replace: &[crate::node::data::Replace<R, E>],
) -> Result<CompiledExpr> {
    // Sorted (name, type) fields of the source struct — cloned out of the
    // deref before emitting (lock discipline).
    let fields: Vec<(ArcStr, Type)> = source.typ().with_deref(|t| match t {
        Some(Type::Struct(flds)) => {
            Ok(flds.iter().map(|(n, t)| (n.clone(), t.clone())).collect())
        }
        _ => Err(anyhow!("emit_clif: struct-with source isn't a struct")),
    })?;
    let (arr_ptr, src, src_disc) =
        emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    // A tainted source (a #219 placeholder — e.g. a region input that
    // never fired) does NOT abort: the unchanged-field reads below are
    // guarded, the built struct is shape-safe, and `src_disc`'s TAINT
    // folds into the result disc — the with-update's consumers gate,
    // exactly like the accessors (jul19i divergence_000087: the old
    // whole-kernel abort here killed a fused call whose const-body
    // callee never consumed the arg).
    // Register an Owned source: a replaced-field bottom-abort between here
    // and the finalize would otherwise leak it (a Borrowed source is
    // env-owned — nothing to drop).
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
    cx.ctx.dyncall_buf_stack.borrow_mut().push(outer_var);
    // The struct fires iff the source fired OR any replacement fired: fold
    // the source disc once (all unchanged fields share its freshness) and
    // each replacement's disc.
    let mut field_discs: Vec<ClifValue> = vec![src_disc];
    for (i, (name, field_typ)) in fields.iter().enumerate() {
        let inner_cap = cx.b.ins().iconst(types::I64, 2);
        let call = cx.b.ins().call(buf_new, &[inner_cap]);
        let inner = cx.b.inst_results(call)[0];
        let inner_var = cx.b.declare_var(types::I64);
        cx.b.def_var(inner_var, inner);
        cx.ctx.dyncall_buf_stack.borrow_mut().push(inner_var);
        let name_ptr = cx.interned_str(name);
        cx.b.ins().call(push_arcstr, &[inner, name_ptr]);
        match replace.iter().find(|r| r.index == Some(i)) {
            Some(r) => {
                // Replacement value — may bottom-abort (both bufs + Owned
                // source are registered, so `emit_pending_cleanup` frees them).
                field_discs.push(emit_push_field_node(cx, inner, &r.n.node)?);
            }
            None => {
                // Unchanged field — read the value from the source struct
                // (an owned clone; guarded — a tainted source's placeholder
                // has no fields to read) and push it.
                let ftyp = resolve_node_typ(cx.ctx, field_typ);
                let idx = cx.b.ins().iconst(types::I64, i as i64);
                let cv = emit_guarded_element_read(cx, arr_ptr, src_disc, idx, &ftyp, true)?;
                scaffold::push_field(cx, inner, cv, &ftyp, CompositeSource::Owned)?;
            }
        }
        let call = cx.b.ins().call(finalize, &[inner]);
        let inner_arr = cx.b.inst_results(call)[0];
        cx.ctx.dyncall_buf_stack.borrow_mut().pop(); // inner consumed by finalize
        cx.b.ins().call(push_array, &[outer, inner_arr]);
    }
    let call = cx.b.ins().call(finalize, &[outer]);
    let payload = cx.b.inst_results(call)[0];
    cx.ctx.dyncall_buf_stack.borrow_mut().pop(); // outer consumed by finalize
    // Drop the Owned source on the normal path (exactly once — the pending
    // path drops it via `owned_input_stack`).
    if src_var.is_some() {
        let drop = cx.helper("graphix_valarray_drop")?;
        cx.b.ins().call(drop, &[arr_ptr]);
        cx.ctx.owned_input_stack.borrow_mut().pop();
    }
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    let disc = propagate_flags(cx.b, disc, &field_discs);
    Ok(CompiledExpr::new(disc, payload))
}

/// Variant constructor. Nullary →
/// `Value::String(tag)` — a cloned interned tag with the STRING disc
/// (clones the interned tag — the borrowed interned pointer makes the
/// clone mandatory). With payloads → `Value::Array([tag, p0, ...])` built
/// via the buf helpers; the finalize'd ValArray bits are the Value's
/// payload word (unified Value ABI). The tag is interned lazily via
/// [`BodyCx::interned_str`].
pub(crate) fn emit_variant_new_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    tag: &ArcStr,
    payloads: &[Cached<R, E>],
) -> Result<CompiledExpr> {
    let tag_ptr = cx.interned_str(tag);
    if payloads.is_empty() {
        // A nullary variant is `Value::String(tag)`: clone the interned
        // tag (owned ArcStr bits = the payload word, unified Value ABI).
        let clone_static = cx.helper("graphix_arcstr_clone_from_static")?;
        let call = cx.b.ins().call(clone_static, &[tag_ptr]);
        let bits = cx.b.inst_results(call)[0];
        let base = cx.b.ins().iconst(types::I64, value_disc::STRING);
        // A nullary variant `` `Tag `` is a constant — fires once at init.
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
        let mut payload_discs = Vec::with_capacity(payloads.len());
        for p in payloads {
            payload_discs.push(emit_push_field_node(cx, buf, &p.node)?);
        }
        let call = cx.b.ins().call(finalize, &[buf]);
        let bits = cx.b.inst_results(call)[0];
        // The finalize'd ValArray bits ARE `Value::Array`'s payload
        // word (unified Value ABI) — mint the disc inline. A
        // `Tag(a, b)` variant fires iff any payload fired.
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

/// Compile an accessor's composite source to borrowed ValArray bits,
/// returning the pointer plus its ownership classification. Shared by
/// the tuple/struct/array element-read relays and the struct-with; the
/// caller drops an Owned pointer after the read (the element helpers
/// clone the slot out, so the temporary producer would otherwise leak —
/// a Borrowed read stays owned by its env slot). The returned disc may
/// carry TAINT (a #219 placeholder source) — callers guard the actual
/// reads ([`emit_guarded_element_read`]) and fold the disc so taint
/// gates at the consumer, never a whole-kernel abort.
fn emit_accessor_source_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    want: AbiKind,
) -> Result<(ClifValue, CompositeSource, ClifValue)> {
    if kernel_abi::abi_kind(cx.registry(), source.typ()) != Some(want) {
        return Err(anyhow!(
            "emit_clif: accessor source of type {:?} isn't {want:?}",
            source.typ()
        ));
    }
    let src = node_composite_source(source);
    let cv = source.emit_clif(cx)?;
    Ok((cv.payload, src, cv.disc))
}

/// A helper-safe placeholder (payload of `elem`'s CLIF type) plus the
/// matching TAINTED disc for a skipped read — what a tainted source's
/// consumer gets instead of an unchecked out-of-bounds read (#219: it
/// runs harmlessly downstream; the taint gates at the output).
pub(super) fn emit_elem_placeholder(
    cx: &mut BodyCx,
    elem: &Type,
) -> Result<CompiledExpr> {
    match kernel_abi::abi_kind(cx.registry(), elem) {
        Some(AbiKind::Scalar(p)) => {
            let disc = cx.b.ins().iconst(types::I64, prim_to_value_disc(p) | TAINT);
            Ok(CompiledExpr::new(disc, zero_const(cx.b, p)))
        }
        Some(AbiKind::String) => {
            let helper = cx.helper("graphix_arcstr_empty")?;
            let call = cx.b.ins().call(helper, &[]);
            let s = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::STRING | TAINT);
            Ok(CompiledExpr::new(disc, s))
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let helper = cx.helper("graphix_valarray_empty")?;
            let call = cx.b.ins().call(helper, &[]);
            let a = cx.b.inst_results(call)[0];
            let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY | TAINT);
            Ok(CompiledExpr::new(disc, a))
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
        | Some(AbiKind::Unit) => {
            let disc = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let zero = cx.b.ins().iconst(types::I64, 0);
            Ok(CompiledExpr::new(disc, zero))
        }
        other => Err(anyhow!("emit_clif: no placeholder for shape {other:?}")),
    }
}

/// Taint-guarded structural element read: a tainted source carries a
/// #219 PLACEHOLDER (e.g. the empty array a bottomed `$`/`?` now
/// produces), which the UNCHECKED read helpers cannot touch at fixed
/// offsets — so branch: skip the read and produce a tainted shape-safe
/// placeholder instead (the node-walk's accessor simply doesn't fire;
/// downstream consumers see taint and the output gates). `is_tainted`
/// folds to const-false for proven-untainted sources, so the branch
/// AND the placeholder path fold away entirely on the hot path.
fn emit_guarded_element_read(
    cx: &mut BodyCx,
    arr_ptr: ClifValue,
    src_disc: ClifValue,
    idx_val: ClifValue,
    elem: &Type,
    struct_access: bool,
) -> Result<CompiledExpr> {
    let tainted = is_tainted(cx.b, src_disc);
    let read_bl = cx.b.create_block();
    let skip_bl = cx.b.create_block();
    let merge = cx.b.create_block();
    let pay_ty = match kernel_abi::abi_kind(cx.registry(), elem) {
        Some(AbiKind::Scalar(p)) => prim_to_clif(p),
        _ => types::I64,
    };
    cx.b.append_block_param(merge, types::I64);
    cx.b.append_block_param(merge, pay_ty);
    cx.b.ins().brif(tainted, skip_bl, &[], read_bl, &[]);
    cx.b.switch_to_block(read_bl);
    cx.b.seal_block(read_bl);
    let rv = compile_element_read(cx.b, arr_ptr, idx_val, elem, struct_access, cx.ctx)?;
    cx.b.ins().jump(merge, &[BlockArg::Value(rv.disc), BlockArg::Value(rv.payload)]);
    cx.b.switch_to_block(skip_bl);
    cx.b.seal_block(skip_bl);
    let ph = emit_elem_placeholder(cx, elem)?;
    cx.b.ins().jump(merge, &[BlockArg::Value(ph.disc), BlockArg::Value(ph.payload)]);
    cx.b.switch_to_block(merge);
    cx.b.seal_block(merge);
    let params = cx.b.block_params(merge);
    Ok(CompiledExpr::new(params[0], params[1]))
}

/// `t.<idx>` — a statically-valid index, read through
/// `compile_element_read` (owned result; Value shape for a value-shape
/// element, Single otherwise).
pub(crate) fn emit_tuple_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Node-carried elem types can be Refs to abstract type names
    // (#218) — resolve before the read classifies by abi_kind.
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let (arr_ptr, src, src_disc) =
        emit_accessor_source_node(cx, source, AbiKind::Tuple)?;
    let idx_const = cx.b.ins().iconst(types::I64, idx as i64);
    let result =
        emit_guarded_element_read(cx, arr_ptr, src_disc, idx_const, &elem_typ, false)?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    // `t.0` fires iff the source tuple fired (the index is a constant); the
    // element read synthesizes a fresh disc, so the source's STALE gates it.
    let disc = propagate_flags(cx.b, result.disc, &[src_disc]);
    Ok(CompiledExpr::new(disc, result.payload))
}

/// `s.field` — the two-level kv-pair read via the `struct_get_*`
/// helper family. `sorted_idx` is the
/// field's position in the struct type's canonical (sorted) layout —
/// resolved by the node's typecheck.
pub(crate) fn emit_struct_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    sorted_idx: usize,
    elem_typ: &Type,
) -> Result<CompiledExpr> {
    // Same abstract-Ref resolution as the tuple read (#218).
    let elem_typ = resolve_node_typ(cx.ctx, elem_typ);
    let (arr_ptr, src, src_disc) =
        emit_accessor_source_node(cx, source, AbiKind::Struct)?;
    let idx_const = cx.b.ins().iconst(types::I64, sorted_idx as i64);
    let result =
        emit_guarded_element_read(cx, arr_ptr, src_disc, idx_const, &elem_typ, true)?;
    emit_accessor_source_drop(cx, arr_ptr, src)?;
    // `s.field` fires iff the source struct fired (the field index is
    // static); fold the source's STALE onto the fresh element read.
    let disc = propagate_flags(cx.b, result.disc, &[src_disc]);
    Ok(CompiledExpr::new(disc, result.payload))
}

/// `a[i]` / `bytes[i]` — the result type is always `Nullable<elem>`
/// (out-of-bounds →
/// the `ArrayIndexError` Value), produced by the shared bounds-checked
/// helpers (`graphix_valarray_index` routes through the node-walk's
/// own `node::array::array_index`, `graphix_bytes_index` through
/// `bytes_index` — all backends agree bit-for-bit).
pub(crate) fn emit_array_ref_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    idx: &Node<R, E>,
) -> Result<CompiledExpr> {
    let idx_prim = kernel_abi::scalar_prim(cx.registry(), idx.typ())
        .filter(|p| p.is_integer())
        .ok_or_else(|| anyhow!("emit_clif: index of non-integer type {:?}", idx.typ()))?;
    if matches!(kernel_abi::abi_kind(cx.registry(), source.typ()), Some(AbiKind::Array)) {
        // Unforced: `graphix_valarray_index` is bounds-checked (safe on
        // a placeholder), and the source's taint folds into the result
        // disc below — forcing here kernel-bottomed live chains through
        // a tainted literal (triage item 23).
        let (arr_ptr, src, src_disc) =
            emit_accessor_source_node(cx, source, AbiKind::Array)?;
        let idx_cv = idx.emit_clif(cx)?;
        let idx_i64 = widen_to_i64(cx.b, idx_cv.payload, idx_prim)?;
        let helper = cx.helper("graphix_valarray_index")?;
        let call = cx.b.ins().call(helper, &[arr_ptr, idx_i64]);
        let r = cx.b.inst_results(call);
        let (rdisc, rpay) = (r[0], r[1]);
        emit_accessor_source_drop(cx, arr_ptr, src)?;
        // #219: a tainted index taints the result. STALE folds the source
        // array AND the index (`a[i]` fires iff a OR i fired) — both
        // operands present now that the accessor returns the source disc.
        let disc = propagate_flags(cx.b, rdisc, &[src_disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    if lowering::is_bytes(source.typ()) {
        // The helper consumes the bytes operand — owned.
        let bcv = emit_owned_value_operand_node(cx, source)?;
        let idx_cv = idx.emit_clif(cx)?;
        let helper = cx.helper("graphix_bytes_index")?;
        let call = cx.b.ins().call(helper, &[bcv.disc, bcv.payload, idx_cv.payload]);
        let (rdisc, rpay) = {
            let r = cx.b.inst_results(call);
            (r[0], r[1])
        };
        // A tainted bytes operand or index taints the result; STALE folds
        // too (the index fires iff bytes OR idx fired — both operands
        // present here, so the AND-reduce is complete).
        let disc = propagate_flags(cx.b, rdisc, &[bcv.disc, idx_cv.disc]);
        return Ok(CompiledExpr::new(disc, rpay));
    }
    Err(anyhow!(
        "emit_clif: index source of type {:?} isn't an array or bytes",
        source.typ()
    ))
}

/// `m{key}` — both operands as OWNED `(disc, payload)` Values (the
/// helper consumes them);
/// `graphix_map_ref` does the lookup (shared `node::map::map_get`
/// semantics), returning `Nullable<V>` as two words.
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
    // A tainted map or key taints the lookup result; STALE folds too (the
    // lookup fires iff map OR key fired — both operands present here).
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
    if !(matches!(
        kernel_abi::abi_kind(cx.registry(), source.typ()),
        Some(AbiKind::Array)
    ) || lowering::is_bytes(source.typ()))
    {
        return Err(anyhow!(
            "emit_clif: slice source of type {:?} isn't an array or bytes",
            source.typ()
        ));
    }
    let scv = emit_owned_value_operand_node(cx, source)?;
    // #219: source + present-bound taint propagates into the slice
    // result (forced at the output).
    let mut taint_discs: Vec<ClifValue> = vec![scv.disc];
    let emit_bound = |cx: &mut BodyCx,
                      n: Option<&Node<R, E>>,
                      flag: i64,
                      flags: &mut i64,
                      taint: &mut Vec<ClifValue>|
     -> Result<ClifValue> {
        match n {
            None => Ok(cx.b.ins().iconst(types::I64, 0)),
            Some(n) => {
                if !kernel_abi::scalar_prim(cx.registry(), n.typ())
                    .is_some_and(|p| p.is_integer())
                {
                    return Err(anyhow!(
                        "emit_clif: slice bound of non-integer type {:?}",
                        n.typ()
                    ));
                }
                *flags |= flag;
                let cv = n.emit_clif(cx)?;
                taint.push(cv.disc);
                Ok(cv.payload)
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
    // The slice fires iff the source OR any present bound fired — and
    // `taint_discs` already holds the source disc plus each present bound,
    // so STALE folds completely (a const-bound slice fires iff the source
    // fired).
    let disc = propagate_flags(cx.b, rdisc, &taint_discs);
    Ok(CompiledExpr::new(disc, rpay))
}

/// Node-graph analog of `kernel_abi::int_div_may_bottom` — true unless the
/// divisor is a non-zero constant (and, for signed, the dividend isn't
/// the MIN/-1 overflow pair). Conservative `true` keeps the runtime
/// guard; a provable non-bottom skips it. Sees through `ExplicitParens`.
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
    // A zero divisor always bottoms.
    if value_is_zero(rv) {
        return true;
    }
    // A non-(-1) divisor can't hit the signed MIN/-1 overflow; safe.
    if !value_is_neg_one(rv) {
        return false;
    }
    // Divisor is -1: only the dividend == MIN bottoms. A non-MIN
    // constant dividend is safe; anything else is conservatively unsafe.
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
