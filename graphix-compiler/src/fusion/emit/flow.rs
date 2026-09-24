//! Statement-position emission: blocks, lets, scope drops, the
//! body tail (tail selects and self-tail-calls), and the `?`/`$`
//! error-propagation (qop) nodes.

use crate::{
    BindId, Node, NodeView, Refs, Rt, Update, UserEvent,
    expr::ExprKind,
    fusion::{
        self,
        kernel_abi::{self, AbiKind},
    },
    node::{callsite::CallSite, select::Select},
    typ::Type,
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    BlockArg, InstBuilder, Value as ClifValue, condcodes::IntCC, types,
};
use netidx_value::Typ;
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, ValueVar, bind_local, clean_disc,
        is_fresh, is_tainted, propagate_flags, scalar_disc, taint_if, value_disc,
    },
    body::{
        BodyCx, TailRebind, emit_kernel_return, emit_return_from_node,
        emit_tail_rebind_jump, ensure_owned_composite_src, ensure_owned_value_src,
        node_composite_source,
    },
    call::{CompositeSource, emit_drop_local},
    nodes::{emit_bottom_of_kind, emit_bottom_placeholder},
    scalar::cast_u64_to_prim,
    select::{classify_select_scrutinee, emit_select_arms},
};

/// True only when no node in the subtree could carry an effect.
/// Every CallSite counts as effectful. Handler-less `?` and `$` are
/// not effects: what they do with an error is a diagnostic.
pub(super) fn stmt_subtree_effect_free<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut ok = true;
    fusion::for_each_node(node, &mut |n| {
        // a covered `?` delivers to its handler; a call site may be an effect
        let delivers = match n.view() {
            NodeView::CallSite(_) => true,
            NodeView::Qop(q) => q.handler.is_some(),
            _ => false,
        };
        ok &= !delivers && fusion::effect_blocker(n).is_none();
    });
    ok
}

/// Which statements of a block emit: the tail, every statement that
/// may carry an effect, and every one binding a name a later emitted
/// statement reads. A skipped statement's reads keep nothing alive.
fn block_live<R: Rt, E: UserEvent>(children: &[Node<R, E>]) -> LPooled<Vec<bool>> {
    let mut needed: LPooled<IntSet<BindId>> = LPooled::take();
    let mut live: LPooled<Vec<bool>> = LPooled::take();
    live.resize(children.len(), false);
    let last = children.len().saturating_sub(1);
    for (i, child) in children.iter().enumerate().rev() {
        let mut refs = Refs::default();
        child.refs(&mut refs);
        let mut read = i == last;
        refs.with_bound(|id| read |= needed.contains(&id));
        live[i] = read || !stmt_subtree_effect_free(child);
        if live[i] {
            refs.with_refs(|id| {
                needed.insert(id);
            });
        }
    }
    live
}

/// A block: bind each let, compile the tail, clone it out if it
/// borrows a local about to drop, emit the scope drops.
pub(crate) fn emit_block_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    children: &[Node<R, E>],
) -> Result<CompiledExpr> {
    let Some((tail, init)) = children.split_last() else {
        bail!("emit_clif: empty block");
    };
    let mark = cx.env.mark();
    let live = block_live(children);
    for (child, live) in init.iter().zip(live.iter()) {
        if *live {
            emit_block_stmt(cx, child)?;
        }
    }
    let tail_cv = tail.emit_clif(cx)?;
    // The tail may alias a block-scoped local about to drop; clone
    // borrowed results out. An unclassifiable tail is an error, never a
    // passthrough (the passthrough is a use-after-free).
    let src = node_composite_source(tail);
    let frozen = kernel_abi::freeze_for_abi_normalized(tail.typ());
    let result = match frozen.as_ref().and_then(|t| kernel_abi::abi_kind(t)) {
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let v = ensure_owned_composite_src(cx, src, tail_cv.payload)?;
            CompiledExpr::new(tail_cv.disc, v)
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let (disc, payload) =
                ensure_owned_value_src(cx, src, tail_cv.disc, tail_cv.payload)?;
            CompiledExpr::new(disc, payload)
        }
        // Scalars need no clone; a String read is already an owned
        // clone (the Ref/Const arms bump the refcount).
        Some(AbiKind::Scalar(_) | AbiKind::String | AbiKind::Unit | AbiKind::Null) => {
            tail_cv
        }
        None => bail!(
            "emit_clif: block tail type {:?} doesn't classify — can't make the \
             result outlive the scope drops",
            tail.typ()
        ),
    };
    emit_scope_drops(cx, mark)?;
    cx.env.truncate(mark);
    Ok(result)
}

/// Emit one non-tail block child: a `let` binds, declarations are
/// skipped, anything else evaluates and discards.
fn emit_block_stmt<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    child: &Node<R, E>,
) -> Result<()> {
    match child.view() {
        NodeView::Bind(bind) => {
            let bspec = match &bind.spec.kind {
                ExprKind::Bind(be) => be,
                _ => {
                    return Err(anyhow!(
                        "emit_clif: Bind node spec isn't ExprKind::Bind"
                    ));
                }
            };
            // A rec fn binding is a function-valued let (`emit_let_node`
            // reports it); a rec non-fn let depends on its own previous cycle.
            if bspec.rec && !matches!(bind.node.typ(), Type::Fn(_)) {
                return Err(anyhow!(
                    "emit_clif: recursive non-function let not supported"
                ));
            }
            let name = bspec.pattern.single_bind().ok_or_else(|| {
                anyhow!("emit_clif: non-single-bind let pattern not supported")
            })?;
            let bind_id = bind.pattern.single_bind_id();
            emit_let_node(cx, name, bind_id, &bind.node)?;
        }
        // Compile-time-only declarations — nothing to emit.
        NodeView::Nop(_) | NodeView::TypeDef(_) => {}
        // A discarded result is consumed; owned non-scalar results drop.
        _ => {
            let cv = child.emit_clif(cx)?;
            emit_discard_result(cx, child, cv)?;
        }
    }
    Ok(())
}

/// Tail-position body emission for a self-recursive kernel. Tail
/// positions are [`fusion::TailPosition`]'s; a self-call leaf becomes
/// the rebind-and-jump loop, every other leaf returns. Every path
/// leaves the current block terminated.
pub(super) fn emit_body_tail<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
    ret: &Type,
) -> Result<()> {
    use fusion::TailPosition;
    match fusion::tail_position(node) {
        TailPosition::Block(blk) => {
            let mark = cx.env.mark();
            let (last, init) = blk
                .children
                .split_last()
                .ok_or_else(|| anyhow!("emit_clif: empty block in tail position"))?;
            let live = block_live(&blk.children);
            for (child, live) in init.iter().zip(live.iter()) {
                if *live {
                    emit_block_stmt(cx, child)?;
                }
            }
            emit_body_tail(cx, last, ret)?;
            // Every path terminated and dropped its owned locals; pop the
            // compile-time scope.
            cx.env.truncate(mark);
            Ok(())
        }
        TailPosition::Parens(ep) => emit_body_tail(cx, &ep.n, ret),
        TailPosition::Select(s) => {
            // Without a loop head no arm can tail-jump, so the select is an
            // ordinary value expression.
            if cx.ctx.tail.loop_head.is_none() {
                emit_return_from_node(cx, ret, node)
            } else {
                emit_select_node_tail(cx, s, ret)
            }
        }
        TailPosition::Leaf(n) => {
            // Matched by the self BindId. Only a looping kernel jumps; a
            // stateful body has no loop head and calls itself natively.
            if let Some((sb, _)) = cx.ctx.self_call
                && cx.ctx.tail.loop_head.is_some()
            {
                if let NodeView::CallSite(cs) = n.view() {
                    if matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == *sb) {
                        return emit_self_tail_call(cx, cs);
                    }
                }
            }
            emit_return_from_node(cx, ret, n)
        }
    }
}

/// Tail-position select: the shared pattern chain with arms that
/// TERMINATE (return or self tail-call jump) instead of widening to a
/// merge block. An adopted owned scrutinee is dropped by every
/// terminator with the rest of the env.
fn emit_select_node_tail<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    ret: &Type,
) -> Result<()> {
    if sel.arms.is_empty() {
        bail!("emit_clif: select with no arms");
    }
    let mark = cx.env.mark();
    let (scrut, scrut_kind, scrut_typ, _adopted) = classify_select_scrutinee(cx, sel)?;
    let scrut_disc = scrut.disc();
    // A fired scrutinee is one of this select's own fires; on the tail
    // spine the accumulator is the only channel that carries it to the
    // return. band keeps a fired bit cleared across iterations.
    let scrut_stale_bit = cx.b.ins().band_imm(scrut_disc, STALE);
    let cur = cx.b.use_var(cx.ctx.tail.tail_scrut_stale_acc);
    let n = cx.b.ins().band(cur, scrut_stale_bit);
    cx.b.def_var(cx.ctx.tail.tail_scrut_stale_acc, n);
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        &mut |cx, body, mark, guards_stale| {
            // A consulted guard's fire is an own fire too.
            let cur = cx.b.use_var(cx.ctx.tail.tail_scrut_stale_acc);
            let n = cx.b.ins().band(cur, guards_stale);
            cx.b.def_var(cx.ctx.tail.tail_scrut_stale_acc, n);
            emit_body_tail(cx, body, ret)?;
            // The terminator already dropped the arm's owned binds; this
            // truncate is compile-time scope only.
            cx.env.truncate(mark);
            Ok(())
        },
        // A standing bottom scrutinee is not an event: the return's
        // freshness is the scrutinee's.
        &mut |cx| {
            let ph = emit_bottom_placeholder(cx, ret, &[scrut_disc])?;
            emit_kernel_return(cx, ret, ph, CompositeSource::Owned)
        },
        // A bottomed guard with no history stops the chain: return the
        // bottom with the outcome's freshness.
        &mut |cx, stale_bits| {
            let ph = emit_bottom_placeholder(cx, ret, &[stale_bits])?;
            emit_kernel_return(cx, ret, ph, CompositeSource::Owned)
        },
    )?;
    cx.env.truncate(mark);
    Ok(())
}

/// A self-call in tail position: evaluate the new formal values,
/// rebind the leading tail-call slots via `emit_tail_rebind_jump`,
/// and jump to the loop head.
fn emit_self_tail_call<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    cs: &CallSite<R, E>,
) -> Result<()> {
    let spec_apply = match &cs.spec().kind {
        ExprKind::Apply(a) => a,
        _ => bail!("emit_clif: self tail-call spec isn't an Apply"),
    };
    // Labeled args would need default materialization in source order;
    // de-fuse.
    if spec_apply.args.iter().any(|(label, _)| label.is_some()) {
        bail!("emit_clif: labeled args on a self tail-call");
    }
    let Some((_, info)) = cx.ctx.self_call else {
        bail!("emit_clif: a self tail-call outside a self-recursive body");
    };
    let (skipped, invariant) = (&info.kernel.skipped_args, &info.kernel.tail_invariant);
    let mut rebinds: SmallVec<[TailRebind; 8]> = SmallVec::new();
    let mut slot_idx = 0usize;
    for i in 0..spec_apply.args.len() {
        let iu = i as u32;
        // A skipped formal has no slot; an invariant formal keeps its slot
        // but is never rebound.
        if skipped.contains(&iu) {
            continue;
        }
        let slot = slot_idx;
        slot_idx += 1;
        if invariant.contains(&iu) {
            continue;
        }
        let arg = cs
            .arg_positional(i)
            .ok_or_else(|| anyhow!("emit_clif: self tail-call arg {i} missing"))?;
        let cv = arg.emit_clif(cx)?;
        let source = node_composite_source(arg);
        rebinds.push(TailRebind { slot, val: cv, source });
    }
    emit_tail_rebind_jump(cx.b, cx.env, cx.ctx, rebinds)
}

/// Bind one `let` into the env by the value's runtime shape.
/// Composite/value lets clone borrowed sources so this scope owns
/// them.
fn emit_let_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    name: &ArcStr,
    bind_id: Option<BindId>,
    value: &Node<R, E>,
) -> Result<()> {
    // `freeze_for_abi_normalized` so a select-valued let (whose type is the
    // un-normalized arm union) still classifies.
    let frozen = kernel_abi::freeze_for_abi_normalized(value.typ());
    let ak = frozen.as_ref().and_then(|t| kernel_abi::abi_kind(t));
    match ak {
        Some(AbiKind::Scalar(p)) => {
            // The disc carries the binding's taint; an unconsumed bottom is
            // dropped, never bottoming.
            let cv = value.emit_clif(cx)?;
            bind_local(
                cx,
                name.clone(),
                cv.disc,
                cv.payload,
                LocalKind::Scalar(p),
                bind_id,
            );
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let cv = value.emit_clif(cx)?;
            let owned =
                ensure_owned_composite_src(cx, node_composite_source(value), cv.payload)?;
            bind_local(cx, name.clone(), cv.disc, owned, LocalKind::Composite, bind_id);
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let cv = value.emit_clif(cx)?;
            let (disc, payload) = ensure_owned_value_src(
                cx,
                node_composite_source(value),
                cv.disc,
                cv.payload,
            )?;
            let kind = match ak {
                Some(AbiKind::Variant) => LocalKind::Variant,
                Some(AbiKind::Nullable) => LocalKind::Nullable,
                _ => LocalKind::Value,
            };
            bind_local(cx, name.clone(), disc, payload, kind, bind_id);
        }
        Some(AbiKind::String) => {
            // String reads/consts are already owned clones.
            let cv = value.emit_clif(cx)?;
            bind_local(cx, name.clone(), cv.disc, cv.payload, LocalKind::String, bind_id);
        }
        other => {
            // A lambda is not a kernel value: the binding node-walks, its call
            // sites fuse. Distinct message so probes can tell it from a gap.
            if matches!(value.typ(), Type::Fn(_)) {
                return Err(anyhow!(
                    "emit_clif: function-valued let — the binding \
                     node-walks, call sites fuse"
                ));
            }
            return Err(anyhow!(
                "emit_clif: let value of shape {other:?} — not yet supported"
            ));
        }
    }
    Ok(())
}

/// Drop a discarded statement's result if it owns an allocation.
/// Borrowed reads (a bare `Ref` statement) own nothing; strings are
/// always owned at production (reads clone).
pub(super) fn emit_discard_result<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
    cv: CompiledExpr,
) -> Result<()> {
    let owned = node_composite_source(node) == CompositeSource::Owned;
    let kind = match kernel_abi::freeze_for_abi_normalized(node.typ()) {
        Some(t) => kernel_abi::abi_kind(&t),
        None => kernel_abi::abi_kind(node.typ()),
    };
    match kind {
        Some(AbiKind::Scalar(_) | AbiKind::Unit | AbiKind::Null) => {}
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            if owned {
                cx.call_helper("graphix_valarray_drop", &[cv.payload])?;
            }
        }
        Some(AbiKind::String) => {
            cx.call_helper("graphix_arcstr_drop", &[cv.payload])?;
        }
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            if owned {
                cx.call_helper("graphix_value_drop", &[cv.disc, cv.payload])?;
            }
        }
        None if owned => bail!(
            "emit_clif: discarded result of type {:?} doesn't classify — can't \
             drop it",
            node.typ()
        ),
        None => {}
    }
    Ok(())
}

/// Drop every owned local above `mark`.
pub(super) fn emit_scope_drops(cx: &mut BodyCx, mark: usize) -> Result<()> {
    // Snapshot so the `cx.env` borrow ends before driving `cx.b`.
    let drops: SmallVec<[(LocalKind, ValueVar); 8]> = cx.env.locals_above(mark).collect();
    for (kind, vv) in drops {
        emit_drop_local(cx.b, cx.ctx, kind, vv)?;
    }
    Ok(())
}

/// True iff a frozen type has any `marker` (error or null) member.
fn type_may_be(t: &Type, marker: Typ) -> bool {
    match t {
        Type::Error(_) => marker == Typ::Error,
        Type::Primitive(p) => p.contains(marker),
        Type::Set(members) => members.iter().any(|m| type_may_be(m, marker)),
        _ => false,
    }
}

/// True iff every value of a frozen type is a `marker` (error or null).
fn type_always(t: &Type, marker: Typ) -> bool {
    match t {
        Type::Error(_) => marker == Typ::Error,
        Type::Primitive(p) => p.contains(marker) && p.iter().count() == 1,
        Type::Set(members) => {
            !members.is_empty() && members.iter().all(|m| type_always(m, marker))
        }
        _ => false,
    }
}

/// Where a `?`/`$` site's fresh stripped value goes. The first two
/// strip the operand's errors, the rest its null.
#[derive(Clone, Copy)]
pub(crate) enum QopSink {
    /// A handler-ful `?`: raised onto the invocation's delivery queue,
    /// keyed by the interned `QopSite`.
    Deliver(ClifValue),
    /// `$` (or a handler-less `?` when `unhandled`): logged against the
    /// interned "origin at position" string, then dropped.
    Log { site: ClifValue, unhandled: bool },
    /// A handler-ful `?` over a nullable: `NullError` raised onto the
    /// delivery queue, keyed by the interned `QopSite`.
    DeliverNull(ClifValue),
    /// A handler-less `?` over a nullable: the interned diagnostic is
    /// logged whole.
    UnhandledNull(ClifValue),
    /// `$` over a nullable: a null is not a failure, nothing is said.
    DropNull,
}

impl QopSink {
    fn marker(self) -> Typ {
        match self {
            Self::Deliver(_) | Self::Log { .. } => Typ::Error,
            Self::DeliverNull(_) | Self::UnhandledNull(_) | Self::DropNull => Typ::Null,
        }
    }

    /// The clean disc of the value this site strips.
    fn bad_disc(self) -> i64 {
        match self.marker() {
            Typ::Null => value_disc::NULL,
            _ => value_disc::ERROR,
        }
    }
}

/// The `?`/`$` bad path: a deliverable (real, fresh) error or null goes
/// to its sink, then it is dropped if the inner owns it (a borrowed
/// inner is dropped by its env slot).
fn emit_qop_error_disposal(
    cx: &mut BodyCx,
    sink: QopSink,
    deliverable: ClifValue,
    clean: ClifValue,
    payload: ClifValue,
    inner_owned: bool,
) -> Result<()> {
    let sink_bl = cx.b.create_block();
    let cont_bl = cx.b.create_block();
    cx.b.ins().brif(deliverable, sink_bl, &[], cont_bl, &[]);
    cx.b.switch_to_block(sink_bl);
    cx.b.seal_block(sink_bl);
    match sink {
        QopSink::Deliver(site) => {
            let raise = cx.helper("graphix_qop_raise")?;
            cx.b.ins().call(raise, &[site, clean, payload]);
        }
        QopSink::Log { site, unhandled } => {
            let log = cx.helper("graphix_swallowed_error")?;
            let unhandled = cx.b.ins().iconst(types::I8, unhandled as i64);
            cx.b.ins().call(log, &[site, unhandled, clean, payload]);
        }
        QopSink::DeliverNull(site) => {
            let raise = cx.helper("graphix_qop_raise_null")?;
            cx.b.ins().call(raise, &[site]);
        }
        QopSink::UnhandledNull(msg) => {
            let log = cx.helper("graphix_unhandled_null")?;
            cx.b.ins().call(log, &[msg]);
        }
        QopSink::DropNull => {}
    }
    cx.b.ins().jump(cont_bl, &[]);
    cx.b.switch_to_block(cont_bl);
    cx.b.seal_block(cont_bl);
    if inner_owned {
        let value_drop = cx.helper("graphix_value_drop")?;
        cx.b.ins().call(value_drop, &[clean, payload]);
    }
    Ok(())
}

/// `?`/`$` over an inner that is always what the site strips: a fresh
/// one goes to its sink; the result is a fresh bottom, stale iff the
/// inner was.
fn emit_qop_always_bad<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    sink: QopSink,
) -> Result<CompiledExpr> {
    let cv = inner.emit_clif(cx)?;
    let clean = clean_disc(cx.b, cv.disc);
    let is_err = cx.b.ins().icmp_imm(IntCC::Equal, clean, sink.bad_disc());
    let fresh = is_fresh(cx.b, cv.disc);
    let deliverable = cx.b.ins().band(is_err, fresh);
    let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
    emit_qop_error_disposal(cx, sink, deliverable, clean, cv.payload, inner_owned)?;
    let bottom = emit_bottom_of_kind(cx, AbiKind::Value)?;
    let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
    let disc = cx.b.ins().bor(bottom.disc, stale_bit);
    Ok(CompiledExpr::new(disc, bottom.payload))
}

/// `?` / `$`: unwrap a `[T, Error<E>]` inner to `T`, or a `[T, null]`
/// inner under a null sink; an inner with nothing to strip passes
/// through. `result_typ` is the qop node's static type, which selects
/// the arm: the typechecker strips every error member of the flattened
/// inner union, so it can differ from the inner's one-layer success type.
pub(crate) fn emit_qop_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    result_typ: &Type,
    sink: QopSink,
) -> Result<CompiledExpr> {
    // Normalized, in lockstep with the discovery-side freeze.
    let Some(inner_typ) = kernel_abi::freeze_for_abi_normalized(inner.typ()) else {
        return Err(anyhow!(
            "emit_clif: `?` inner type {:?} doesn't freeze concrete",
            inner.typ()
        ));
    };
    let marker = sink.marker();
    if kernel_abi::nullable_inner(&inner_typ).is_none() {
        // `nullable_inner` only detects a two member union.
        if !type_may_be(&inner_typ, marker) {
            // Nothing to strip — passthrough; the sink never fires.
            return inner.emit_clif(cx);
        }
        if !type_always(&inner_typ, marker) {
            return Err(anyhow!(
                "emit_clif: `?`/`$` inner type {inner_typ:?} can be {marker:?} \
                 but is not a two member union — node-walk handles it"
            ));
        }
        return emit_qop_always_bad(cx, inner, sink);
    }
    let Some(success_typ) = kernel_abi::freeze_for_abi_normalized(result_typ) else {
        return Err(anyhow!(
            "emit_clif: `?` result type {:?} doesn't freeze concrete",
            result_typ
        ));
    };
    let cv = inner.emit_clif(cx)?;
    let (disc, payload) = (cv.disc, cv.payload);
    // `clean(disc) == sink.bad_disc()` means bottom (mask taint first —
    // a tainted disc is not a structural Error).
    let clean = clean_disc(cx.b, disc);
    let is_err = cx.b.ins().icmp_imm(IntCC::Equal, clean, sink.bad_disc());
    // Delivery requires a fresh error: a tainted error is a phantom
    // computed from placeholders and a stale one never fired.
    let fresh = is_fresh(cx.b, disc);
    let deliverable = cx.b.ins().band(is_err, fresh);
    match kernel_abi::abi_kind(&success_typ) {
        // Branchless on the success path. The error case is a heap-boxed
        // ValError even for a scalar success, so an owned error must be
        // disposed or it leaks once per cycle.
        Some(AbiKind::Scalar(p)) => {
            let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
            let err_block = cx.b.create_block();
            let after = cx.b.create_block();
            cx.b.ins().brif(is_err, err_block, &[], after, &[]);
            cx.b.switch_to_block(err_block);
            cx.b.seal_block(err_block);
            emit_qop_error_disposal(cx, sink, deliverable, clean, payload, inner_owned)?;
            cx.b.ins().jump(after, &[]);
            cx.b.switch_to_block(after);
            cx.b.seal_block(after);
            let value = cast_u64_to_prim(cx.b, payload, p);
            let base = scalar_disc(cx.b, p);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            let disc = taint_if(cx.b, disc, is_err);
            // A dropped error is a fresh bottom.
            Ok(CompiledExpr::new(disc, value))
        }
        // The bad path produces a tainted placeholder and continues.
        Some(
            k @ (AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct),
        ) => {
            let is_string = k == AbiKind::String;
            let base_disc =
                if is_string { value_disc::STRING } else { value_disc::ARRAY };
            let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
            let bad_bl = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let qmerge = cx.b.create_block();
            cx.b.append_block_param(qmerge, types::I64); // disc
            cx.b.append_block_param(qmerge, types::I64); // payload
            // A tainted inner may carry the Value::Null placeholder, whose
            // clean disc is not Error; the success path would unbox it as an
            // Array/ArcStr.
            let tainted = is_tainted(cx.b, disc);
            let bad = cx.b.ins().bor(is_err, tainted);
            cx.b.ins().brif(bad, bad_bl, &[], continue_block, &[]);
            cx.b.switch_to_block(bad_bl);
            cx.b.seal_block(bad_bl);
            emit_qop_error_disposal(cx, sink, deliverable, clean, payload, inner_owned)?;
            // The inner's STALE carries; TAINT marks no-value.
            let ph_helper = if is_string {
                cx.helper("graphix_arcstr_empty")?
            } else {
                cx.helper("graphix_valarray_empty")?
            };
            let call = cx.b.ins().call(ph_helper, &[]);
            let ph = cx.b.inst_results(call)[0];
            let tainted_base = cx.b.ins().iconst(types::I64, base_disc | TAINT);
            let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
            let ph_disc = cx.b.ins().bor(tainted_base, stale_bit);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(ph)]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // A Borrowed inner's success must be cloned (the unwrap result is
            // Owned). The composite narrowing stays checked: the unboxers
            // abort on a non-Array.
            let v = match k {
                AbiKind::String => {
                    if inner_owned {
                        payload
                    } else {
                        let clone = cx.helper("graphix_arcstr_clone")?;
                        let call = cx.b.ins().call(clone, &[payload]);
                        cx.b.inst_results(call)[0]
                    }
                }
                _ => {
                    let helper_name = if inner_owned {
                        "graphix_value_into_array"
                    } else {
                        "graphix_value_into_array_borrowed"
                    };
                    let unbox = cx.helper(helper_name)?;
                    let call = cx.b.ins().call(unbox, &[clean, payload]);
                    cx.b.inst_results(call)[0]
                }
            };
            let base = cx.b.ins().iconst(types::I64, base_disc);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(disc), BlockArg::Value(v)]);
            cx.b.switch_to_block(qmerge);
            cx.b.seal_block(qmerge);
            let params = cx.b.block_params(qmerge);
            // A dropped error is a fresh bottom.
            Ok(CompiledExpr::new(params[0], params[1]))
        }
        // The non-error Value is the result; the bad path continues on a
        // tainted Null placeholder.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let src = node_composite_source(inner);
            let inner_owned = src == CompositeSource::Owned;
            let bad_bl = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let qmerge = cx.b.create_block();
            cx.b.append_block_param(qmerge, types::I64); // disc
            cx.b.append_block_param(qmerge, types::I64); // payload
            cx.b.ins().brif(is_err, bad_bl, &[], continue_block, &[]);
            cx.b.switch_to_block(bad_bl);
            cx.b.seal_block(bad_bl);
            emit_qop_error_disposal(cx, sink, deliverable, clean, payload, inner_owned)?;
            // Tainted Value::Null placeholder (helper-safe by
            // construction); the inner's STALE carries.
            let tainted_base = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
            let ph_disc = cx.b.ins().bor(tainted_base, stale_bit);
            let zero = cx.b.ins().iconst(types::I64, 0);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(zero)]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // A Borrowed inner aliases its env slot, which is dropped at scope
            // exit; the result is Owned, so clone it.
            let (od, op) = ensure_owned_value_src(cx, src, clean, payload)?;
            // `e?` fires iff its operand fired (single input); STALE folds.
            let disc = propagate_flags(cx.b, od, &[cv.disc]);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(disc), BlockArg::Value(op)]);
            cx.b.switch_to_block(qmerge);
            cx.b.seal_block(qmerge);
            let params = cx.b.block_params(qmerge);
            // A dropped error is a fresh bottom.
            Ok(CompiledExpr::new(params[0], params[1]))
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => {
            Err(anyhow!("emit_clif: `?` with unsupported success type {:?}", success_typ))
        }
    }
}
