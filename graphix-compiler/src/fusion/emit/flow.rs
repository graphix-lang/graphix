//! Statement-position emission: blocks, lets, scope drops, the
//! body tail (tail selects and self-tail-calls), and the `?`/`$`
//! error-propagation (qop) nodes.

use crate::{
    BindId, Node, NodeView, Refs, Rt, Update, UserEvent,
    expr::{ExprId, ExprKind},
    fusion::{
        self,
        kernel_abi::{self, AbiKind},
    },
    node::{callsite::CallSite, select::Select},
    typ::Type,
};
use anyhow::{Result, anyhow};
use arcstr::ArcStr;
use cranelift_codegen::ir::{BlockArg, InstBuilder, condcodes::IntCC, types};
use netidx_value::Typ;
use nohash::IntSet;
use poolshark::local::LPooled;

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, ValueVar, bind_local, clean_disc,
        is_fresh, is_tainted, is_untainted, propagate_flags, scalar_disc, taint_if,
        value_disc,
    },
    body::{
        BodyCx, TailRebind, emit_kernel_bottom, emit_kernel_return,
        emit_return_from_node, emit_tail_rebind_jump, ensure_owned_composite_src,
        ensure_owned_value_src, node_composite_source,
    },
    call::{CompositeSource, emit_drop_local},
    lower::SelFire,
    nodes::emit_bottom_placeholder,
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

/// A block: bind each let, compile the tail, clone it out if it
/// borrows a local about to drop, emit the scope drops.
pub(crate) fn emit_block_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    children: &[Node<R, E>],
) -> Result<CompiledExpr> {
    if children.is_empty() {
        return Err(anyhow!("emit_clif: empty block"));
    }
    let mark = cx.env.mark();
    let last = children.len() - 1;
    let mut needed: LPooled<IntSet<BindId>> = LPooled::take();
    let mut live: LPooled<Vec<bool>> = LPooled::take();
    live.resize(children.len(), false);
    // CR claude for eric: [perf] A statement's refs join `needed` even when the
    // statement itself will be skipped, so `let a = [x]; let b = a; 0` keeps and
    // emits `a` for a dead `b`. Add the refs only when the statement is live or
    // not effect-free (the skip test below), which makes the pass transitive.
    for (i, child) in children.iter().enumerate().rev() {
        let mut refs = Refs::default();
        child.refs(&mut refs);
        refs.with_bound(|id| live[i] |= needed.contains(&id));
        refs.with_refs(|id| {
            needed.insert(id);
        });
        // CR claude for eric: [dead] Suspected: a Connect is an effect_blocker, so
        // its statement is never skipped and its emission always Errs; a block
        // holding one never finishes emitting, and the ids collected here decide
        // nothing.
        fusion::for_each_node(child, &mut |n| {
            if let NodeView::Connect(c) = n.view() {
                needed.insert(c.id);
            }
        });
    }
    for (i, child) in children.iter().enumerate() {
        if i == last {
            let tail_cv = child.emit_clif(cx)?;
            // The tail may alias a block-scoped local about to drop; clone
            // borrowed results out. An unclassifiable tail is an error, never a
            // passthrough (the passthrough is a use-after-free).
            let src = node_composite_source(child);
            let frozen = kernel_abi::freeze_for_abi_normalized(child.typ());
            let shape = frozen.as_ref().and_then(|t| kernel_abi::abi_kind(t));
            let result = match shape {
                Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
                    let v = ensure_owned_composite_src(cx, src, tail_cv.payload)?;
                    CompiledExpr::new(tail_cv.disc, v)
                }
                Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
                    let (disc, payload) =
                        ensure_owned_value_src(cx, src, tail_cv.disc, tail_cv.payload)?;
                    CompiledExpr::new(disc, payload)
                }
                // Scalars need no clone; a String read is already an
                // owned clone (the Ref/Const arms bump the refcount).
                Some(
                    AbiKind::Scalar(_) | AbiKind::String | AbiKind::Unit | AbiKind::Null,
                ) => tail_cv,
                None => {
                    return Err(anyhow!(
                        "emit_clif: block tail type {:?} doesn't classify — \
                         can't make the result outlive the scope drops",
                        child.typ()
                    ));
                }
            };
            emit_scope_drops(cx, mark)?;
            cx.env.truncate(mark);
            return Ok(result);
        }
        if !live[i] && stmt_subtree_effect_free(child) {
            continue;
        }
        emit_block_stmt(cx, child)?;
    }
    unreachable!("emit_block_node: last child not handled")
}

/// Emit one non-tail block child: a `let` binds, declarations are
/// skipped, anything else evaluates and discards.
fn emit_block_stmt<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    child: &Node<R, E>,
) -> Result<()> {
    // CR claude for eric: [style] `use NodeView;` is a no-op: NodeView is already
    // imported at the top. Same in select.rs's emit_select_value_arm and
    // emit_select_arm_value.
    use NodeView;
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
            // CR claude for eric: [structure] The tail-position block emits every
            // statement, while emit_block_node skips dead effect-free ones: a dead
            // let whose shape emit_let_node refuses de-fuses a recursive body but not
            // the same block in value position. Share one liveness pass between the
            // two block emitters.
            for child in init {
                emit_block_stmt(cx, child)?;
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
/// merge block.
fn emit_select_node_tail<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    sel: &Select<R, E>,
    ret: &Type,
) -> Result<()> {
    if sel.arms.is_empty() {
        return Err(anyhow!("emit_clif: select with no arms"));
    }
    let (scrut, scrut_kind, scrut_typ, _none) =
        classify_select_scrutinee(cx, sel, false)?;
    // The delivery's fresh-bottomness; false on every arms path once
    // a tainted scrutinee has returned.
    let scrut_bfired = {
        let d = scrut.disc();
        let ts = cx.b.ins().band_imm(d, TAINT | STALE);
        Some(cx.b.ins().icmp_imm(IntCC::Equal, ts, TAINT))
    };
    // A fired scrutinee is one of this select's own fires; on the tail
    // spine the accumulator is the only channel that carries it to the
    // return. band keeps a fired bit cleared across iterations.
    let scrut_stale_bit = cx.b.ins().band_imm(scrut.disc(), STALE);
    {
        let cur = cx.b.use_var(cx.ctx.tail.tail_scrut_stale_acc);
        let n = cx.b.ins().band(cur, scrut_stale_bit);
        cx.b.def_var(cx.ctx.tail.tail_scrut_stale_acc, n);
    }
    // A tainted scrutinee returns a value-level bottom early; the arms
    // then run on a valid scrutinee, so the final-arm miss is
    // unreachable.
    {
        let valid = is_untainted(cx.b, scrut.disc());
        let arms_bl = cx.b.create_block();
        let taint_bl = cx.b.create_block();
        cx.b.ins().brif(valid, arms_bl, &[], taint_bl, &[]);
        cx.b.switch_to_block(taint_bl);
        cx.b.seal_block(taint_bl);
        // A standing bottom scrutinee is not an event: the return's
        // freshness is the scrutinee's.
        let ph = emit_bottom_placeholder(cx, ret, &[scrut.disc()])?;
        emit_kernel_return(cx, ret, ph, CompositeSource::Owned)?;
        cx.b.switch_to_block(arms_bl);
        cx.b.seal_block(arms_bl);
    }
    // CR claude for eric: [dead] `arm_index` is incremented per arm and never read.
    let arm_index = std::cell::Cell::new(0usize);
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        scrut_bfired,
        &mut |cx, body, mark, fires| {
            arm_index.set(arm_index.get() + 1);
            // A prologue guard's sound fire is an own fire too; bottom fires
            // accumulate separately for `emit_kernel_return`.
            if let Some(gs) = fires.sound_stale {
                let cur = cx.b.use_var(cx.ctx.tail.tail_scrut_stale_acc);
                let n = cx.b.ins().band(cur, gs);
                cx.b.def_var(cx.ctx.tail.tail_scrut_stale_acc, n);
            }
            // This select's own-fire scope for the returns inside its arm.
            let sound_lvl = match fires.sound_stale {
                Some(gs) => cx.b.ins().band(scrut_stale_bit, gs),
                None => scrut_stale_bit,
            };
            cx.ctx
                .sel_fires
                .borrow_mut()
                .push(SelFire { sound_stale: sound_lvl, bfired: fires.bfired });
            let arm_res = emit_body_tail(cx, body, ret);
            cx.ctx.sel_fires.borrow_mut().pop();
            arm_res?;
            // The terminator already dropped the arm's owned binds; this
            // truncate is compile-time scope only.
            cx.env.truncate(mark);
            Ok(())
        },
        // Unreachable (scrutinee forced valid → exhaustive matches); a
        // terminator is still required.
        &mut |cx| emit_kernel_bottom(cx),
        // A bottomed guard with no history stops the chain: return the
        // bottom with the outcome's freshness.
        &mut |cx, stale_bits| {
            let ph = emit_bottom_placeholder(cx, ret, &[stale_bits])?;
            emit_kernel_return(cx, ret, ph, CompositeSource::Owned)
        },
    )
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
        _ => {
            return Err(anyhow!("emit_clif: self tail-call spec isn't an Apply"));
        }
    };
    // Labeled args would need default materialization in source order;
    // de-fuse.
    if spec_apply.args.iter().any(|(label, _)| label.is_some()) {
        return Err(anyhow!("emit_clif: labeled args on a self tail-call"));
    }
    // CR claude for eric: [perf] Two Vec clones per tail call only to be read;
    // borrow them as `&[u32]` from the self-call info.
    let (skipped, invariant) = match cx.ctx.self_call {
        Some((_, info)) => {
            (info.kernel.skipped_args.clone(), info.kernel.tail_invariant.clone())
        }
        None => (Vec::new(), Vec::new()),
    };
    let n = spec_apply.args.len();
    let mut rebinds: smallvec::SmallVec<[TailRebind; 8]> = smallvec::SmallVec::new();
    let mut slot_idx = 0usize;
    for i in 0..n {
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
    // CR claude for eric: [bug] the tail rebind use-after-free (probe and fix in
    // the CR at body.rs emit_tail_rebind_jump) can be fixed here instead: make every
    // Borrowed composite/value arg owned before any old slot drops.
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
    let owned = matches!(node_composite_source(node), CompositeSource::Owned);
    // CR claude for eric: [risk] This classifies the raw type, while the block tail
    // and emit_let_node classify `freeze_for_abi_normalized` (their comment: a
    // select's type is the un-normalized arm union). A shape abi_kind cannot name
    // falls to `_ => {}` and the owned result is never dropped, silently. Classify
    // the same way, and make an unclassifiable owned result an Err.
    match kernel_abi::abi_kind(node.typ()) {
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) if owned => {
            let drop = cx.helper("graphix_valarray_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
        }
        Some(AbiKind::String) => {
            let drop = cx.helper("graphix_arcstr_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
        }
        // CR claude for eric: [readability] Stale: the disc is passed as is, and
        // graphix_value_drop takes a TagValue, which carries the tag bits.
        // A tainted value drops like an untainted one; clean the disc so
        // the helper sees a valid tag.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) if owned => {
            let drop = cx.helper("graphix_value_drop")?;
            cx.b.ins().call(drop, &[cv.disc, cv.payload]);
        }
        _ => {}
    }
    Ok(())
}

// CR claude for eric: [structure] call.rs's drop_owned_composites is this function
// at mark 0 with the BodyCx unpacked; keep one.
/// Drop every owned non-scalar local above `mark`.
pub(super) fn emit_scope_drops(cx: &mut BodyCx, mark: usize) -> Result<()> {
    // Snapshot so the `cx.env` borrow ends before driving `cx.b`.
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        cx.env.locals_above(mark).collect();
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

// CR claude for eric: [style] `cranelift_codegen::ir::Value` is spelled out seven
// times in QopSink and emit_qop_error_disposal; the rest of the emitter imports it
// as ClifValue.
/// Where a `?`/`$` site's fresh stripped value goes. The first two
/// strip the operand's errors, the rest its null.
#[derive(Clone, Copy)]
pub(crate) enum QopSink {
    /// A handler-ful `?`: raised onto the invocation's delivery queue,
    /// keyed by the interned `QopSite`.
    Deliver(cranelift_codegen::ir::Value),
    /// `$` (or a handler-less `?` when `unhandled`): logged against the
    /// interned "origin at position" string, then dropped.
    Log { site: cranelift_codegen::ir::Value, unhandled: bool },
    /// A handler-ful `?` over a nullable: `NullError` raised onto the
    /// delivery queue, keyed by the interned `QopSite`.
    DeliverNull(cranelift_codegen::ir::Value),
    /// A handler-less `?` over a nullable: the interned diagnostic is
    /// logged whole.
    UnhandledNull(cranelift_codegen::ir::Value),
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
        // CR claude for eric: [readability] 0x2000_0000 is netidx's Error
        // discriminant written bare; add value_disc::ERROR beside NULL.
        match self.marker() {
            Typ::Null => value_disc::NULL,
            _ => 0x2000_0000,
        }
    }
}

/// The `?`/`$` bad path: a deliverable (real, fresh) error or null goes
/// to its sink, then it is dropped if the inner owns it (a borrowed
/// inner is dropped by its env slot).
fn emit_qop_error_disposal(
    cx: &mut BodyCx,
    sink: QopSink,
    deliverable: cranelift_codegen::ir::Value,
    clean: cranelift_codegen::ir::Value,
    payload: cranelift_codegen::ir::Value,
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
    let bottom = super::nodes::emit_bottom_of_kind(cx, AbiKind::Value)?;
    let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
    let disc = cx.b.ins().bor(bottom.disc, stale_bit);
    Ok(CompiledExpr::new(disc, bottom.payload))
}

/// `?` / `$`: unwrap a `[T, Error<E>]` inner to `T`, or a `[T, null]`
/// inner under a null sink; an inner with nothing to strip passes
/// through. `result_typ` is the qop node's static type, which selects
/// the arm: the typechecker strips every error member of the flattened
/// inner union, so it can differ from the inner's one-layer success type.
// CR claude for eric: [dead] `_spec_id` is unused; both callers (node/error.rs)
// pass `self.spec.id` for nothing.
pub(crate) fn emit_qop_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    _spec_id: ExprId,
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
        // CR claude for eric: [style] The arm already matched the kind, yet
        // abi_kind(&success_typ) is recomputed twice inside it (is_string and the
        // unbox match); bind it with `Some(k @ (..))`.
        Some(AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let is_string =
                matches!(kernel_abi::abi_kind(&success_typ), Some(AbiKind::String));
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
            let v = match kernel_abi::abi_kind(&success_typ) {
                Some(AbiKind::String) => {
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
