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

use super::{
    abi::{
        CompiledExpr, LocalKind, STALE, TAINT, ValueVar, bind_local, clean_disc,
        emit_scalar_taint_cache, is_tainted, is_untainted, propagate_flags, scalar_disc,
        taint_if, value_disc,
    },
    body::{
        BodyCx, emit_kernel_bottom, emit_kernel_return, emit_return_from_node,
        emit_tail_rebind_jump, ensure_owned_composite_src, ensure_owned_value_src,
        node_composite_source,
    },
    call::CompositeSource,
    lower::SelWord,
    nodes::emit_elem_placeholder,
    scalar::cast_u64_to_prim,
    select::{classify_select_scrutinee, emit_select_arms},
};

/// A `do` / `{ ... }` block — let prefix then a tail expression.
/// Mirrors `compile_block_scalar`/`_value`: bind each let by its
/// runtime shape (cloning borrowed composite/value sources so the
/// block exclusively owns its locals), compile the tail, clone the
/// tail out if it borrows a local we're about to drop, emit the
/// scope-exit drops for this block's owned locals, then pop the
/// block-scoped env entries.
/// Conservative effect-freedom for dead-statement elimination: TRUE
/// only when no node in the subtree could carry an effect the
/// node-walk would have performed. Connect/ConnectDeref write
/// variables; a `?` WITH a catch handler writes the handler's
/// variable; a CallSite may target an async/effectful builtin (we
/// can't consult `builtin_effects` at emit time, so ALL call sites
/// are conservatively effectful). Handler-less `?` and `$` only emit
/// the swallowed-error diagnostics, which are node-walk-only by
/// design (a fused kernel drops them even when live) — and treating
/// them as effects is actively WRONG here: their non-scalar error
/// paths abort the whole kernel, so emitting a dead `m{k}$` bind
/// wrong-bottoms the region (soak finding
/// corpus-generate/divergence_000000, 2026-07-03). Everything else
/// the direct emitter can encounter is value-only.
fn stmt_subtree_effect_free<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut ok = true;
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::Connect(_) | NodeView::ConnectDeref(_) | NodeView::CallSite(_) => {
            ok = false
        }
        // A Module PUBLISHES its binds into the persistent env —
        // readable by any Ref outside the region and by every
        // later-installed top expression — so a `mod m;` statement is
        // never dead. Eliminating one deadlocked the spliced region:
        // the whole-file Do fused with the module's constant routed IN
        // as a feeder while the module (that feeder's only producer)
        // was eliminated as a dead statement and deleted with the
        // splice — the kernel waited forever on its own input
        // (`mod m0; m0::c` under the shell's file wrap, found probing
        // the fuzzer's cross-module vocabulary, 2026-07-08). The
        // node-walk runs the "dead" statement and delivers.
        NodeView::Module(_) => ok = false,
        NodeView::Qop(q) => {
            if q.id.is_some() {
                ok = false
            }
        }
        NodeView::OrNever(_) => {}
        _ => {}
    });
    ok
}

pub(crate) fn emit_block_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    children: &[Node<R, E>],
) -> Result<CompiledExpr> {
    if children.is_empty() {
        return Err(anyhow!("emit_clif: empty block"));
    }
    let mark = cx.env.mark();
    let last = children.len() - 1;
    for (i, child) in children.iter().enumerate() {
        if i == last {
            let tail_cv = child.emit_clif(cx)?;
            // The tail may alias a block-scoped local we're about to
            // drop — clone borrowed results so they outlive the block.
            // Taint rides in the disc through the clone.
            //
            // `freeze_for_abi_normalized`, NOT `abi_kind(child.typ())`
            // raw: a select-valued tail's type is the un-normalized
            // arm union, which doesn't classify — the old raw call
            // fell through to "no clone needed" while the scope drop
            // below still freed the local, so the caller's later clone
            // read freed memory (soak jul08g generate crashes 0/1,
            // SIGSEGV in `graphix_valarray_clone`). An unclassifiable
            // tail is an ERROR (de-fuse), never a silent passthrough —
            // the passthrough IS the use-after-free.
            let src = node_composite_source(child);
            let frozen =
                kernel_abi::freeze_for_abi_normalized(cx.registry(), child.typ());
            let shape =
                frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t));
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
        // Dead-statement elimination — the classic prune pass's
        // semantics at the emission seam. A non-tail statement whose
        // value nobody reads AND whose subtree is provably effect-free
        // contributes NOTHING canonical: its node-walk value (or
        // bottom) flows to no consumer. Emitting it anyway is worse
        // than useless — a dead bottom (div0 inside a discarded
        // tuple, an unused let holding an aborting array literal)
        // poisons the WHOLE kernel via the composite producers'
        // bottom-abort, a value divergence vs the node-walk
        // (findings/flip-jun2026). A Bind is dead iff no LATER
        // sibling or the tail references a bound id; a bare
        // expression statement's value is always unread.
        //
        // The effect-free gate is LOAD-BEARING: a statement whose
        // subtree contains a Connect (`<-`), handler-ful `?`, `$`
        // (logs), or ANY CallSite (a builtin may be async/effectful)
        // must NOT be skipped — skipping converts "emission fails →
        // region de-fuses → node-walk runs the effect" into silently
        // DROPPING the effect (the env-accounting probe caught
        // exactly that with a skipped `counter <- v`). Those emit
        // normally and de-fuse via their emit Errs as before.
        let dead = if matches!(child.view(), NodeView::Bind(_)) {
            let mut bound = Refs::default();
            child.refs(&mut bound);
            let mut suffix = Refs::default();
            for later in &children[i + 1..] {
                later.refs(&mut suffix);
            }
            let mut alive = false;
            bound.with_bound(|id| {
                if suffix.is_refed(id) {
                    alive = true;
                }
            });
            !alive
        } else {
            true
        };
        if dead && stmt_subtree_effect_free(child) {
            continue;
        }
        emit_block_stmt(cx, child)?;
    }
    // The loop always returns on the last child.
    unreachable!("emit_block_node: last child not handled")
}

/// Emit one non-last block child as a statement: a `let` binds into
/// the env, compile-time-only declarations are skipped, anything else
/// evaluates and discards. Shared by the value-position block emitter
/// and the tail-position walk (`emit_body_tail`).
fn emit_block_stmt<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    child: &Node<R, E>,
) -> Result<()> {
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
            // A rec FN binding is just a function-valued let (the
            // binding node-walks, call sites fuse as cross-kernel
            // calls — recursion included); `emit_let_node`'s
            // fallthrough produces that message. A rec NON-fn let
            // (`let rec x = x + 1` — a reactive feedback loop) is
            // genuinely unfusable: its value depends on its own
            // previous cycle.
            if bspec.rec && !matches!(bind.node.typ(), Type::Fn(_)) {
                return Err(anyhow!(
                    "emit_clif: recursive non-function let not supported"
                ));
            }
            let name = bspec.pattern.single_bind().ok_or_else(|| {
                anyhow!("emit_clif: non-single-bind let pattern not supported")
            })?;
            let bind_id = bind.single_bind_id();
            emit_let_node(cx, name, bind_id, &bind.node)?;
        }
        // Compile-time-only declarations — nothing to emit.
        NodeView::Nop(_) | NodeView::TypeDef(_) | NodeView::Use(_) => {}
        // Expression statement — evaluate, discard the result. A
        // discarded may-bottom scalar is fine — the bottom is never
        // consumed. Owned non-scalar results are dropped: discarding is
        // consuming. This includes a CALL in statement position: a
        // no-value fire is a #219 tainted placeholder now, so
        // discarding it discards the bottom exactly like the
        // node-walk (`{println("hi"); 100}` used to pending-abort the
        // kernel and lose the 100 — soak finding 2026-07-04, item 28).
        // Effects that can't emit (println's bare-Null return) still
        // Err out of `emit_dyncall_node` and de-fuse the region —
        // effects de-fuse, never silently skip.
        _ => {
            let cv = child.emit_clif(cx)?;
            emit_discard_result(cx, child, cv)?;
        }
    }
    Ok(())
}

/// Tail-position body emission for a self-recursive kernel. Tail
/// positions are [`fusion::TailPosition`]'s — the shared definition
/// the analysis pre-scans walk (review A5), so the emitter's tail set
/// can't drift from the set `body_has_self_tail_call` promised. A
/// self-call leaf becomes the rebind-and-jump loop
/// (`emit_self_tail_call`), every other leaf returns directly
/// (`emit_kernel_return`, which drops ALL owned locals at any depth —
/// nested-scope returns can't leak). Every path through this function
/// leaves the current block TERMINATED.
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
            for child in init {
                emit_block_stmt(cx, child)?;
            }
            emit_body_tail(cx, last, ret)?;
            // Every path through the tail terminated — returns dropped
            // all owned locals, tail-jumps dropped the non-slot locals
            // (and already truncated to the param mark, making this a
            // no-op). Pop the compile-time scope.
            cx.env.truncate(mark);
            Ok(())
        }
        TailPosition::Parens(ep) => emit_body_tail(cx, &ep.n, ret),
        TailPosition::Select(s) => emit_select_node_tail(cx, s, ret),
        TailPosition::Leaf(n) => {
            // Self tail-call — checked BEFORE value emission. Matching
            // is by the self BindId (names shadow, ids don't — #206).
            if let Some((sb, _)) = cx.ctx.self_call {
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
    // The tail emitter has no selection memory (recursive bodies claim
    // no state), so an arm-lifted connect target's wake re-seed can't
    // be reproduced here — de-fuse. The value-position emitter handles
    // this shape via the state word (see `emit_select_node`).
    let has_arm_lift = sel.arms.iter().any(|(_, body)| {
        let mut found = false;
        fusion::for_each_node(&body.node, &mut |n| {
            if let NodeView::Bind(b) = n.view() {
                if b.single_bind_id().is_some_and(|id| cx.ctx.lifted.contains(&id)) {
                    found = true;
                }
            }
        });
        found
    });
    if has_arm_lift {
        return Err(anyhow!(
            "emit_clif: tail select arm holds a lifted connect target — \
             no selection memory in a recursive body"
        ));
    }
    let (scrut, scrut_kind, scrut_typ, _none) =
        classify_select_scrutinee(cx, sel, false)?;
    // Fold this scrutinee's firing into the kernel's tail-firing
    // accumulator (see `LowerCtx::tail_scrut_stale`): the arms
    // terminate individually, so the return path is where the
    // scrutinee's control-dependence firing gets applied. band keeps
    // a cleared (fired) bit cleared across loop iterations.
    {
        let cur = cx.b.use_var(cx.ctx.tail_scrut_stale);
        let ss = cx.b.ins().band_imm(scrut.disc(), STALE);
        let n = cx.b.ins().band(cur, ss);
        cx.b.def_var(cx.ctx.tail_scrut_stale, n);
    }
    // GUARDED tail selects need SELECTION MEMORY (jul17c capture-
    // dispatch pin): a guard reading a non-entry input (a capture) can
    // flip the selection while the scrutinee stays quiet, and the
    // scrutinee fold above can never see it — the node-walk fires the
    // becoming-selected arm from its persistent `selected` memory.
    // Same feeder rule as the value-position emitter: guards reading
    // only pattern binds change selection only when the scrutinee
    // fires (already folded), so they claim nothing (the recursion-hot
    // guardless/bind-guard shapes stay word-free). The word records
    // TERMINATING-arm indices only (`tail_sel_path` — recorded at
    // `emit_kernel_return`), so the compare is final selection vs the
    // previous invocation's final selection.
    let has_feeder_guard = sel.arms.iter().any(|(pat, _)| {
        pat.guard.as_ref().is_some_and(|g| {
            let mut refs = crate::Refs::default();
            g.node.refs(&mut refs);
            let mut found = false;
            refs.with_external_refs(|id| {
                if cx.env.lookup_by_id(id).is_some() {
                    found = true;
                }
            });
            found
        })
    });
    let sel_word: Option<SelWord> = if has_feeder_guard {
        match cx.claim_state_word() {
            Some(off) => {
                let sp = cx.state_ptr();
                Some(SelWord::Sure(cx.b.ins().iadd_imm(sp, off as i64)))
            }
            None => cx.claim_site_word().map(|off| {
                let base = cx.site_ptr();
                let addr = cx.b.ins().iadd_imm(base, off as i64);
                SelWord::Guarded { base, addr }
            }),
        }
    } else {
        None
    };
    // Tail position: a missing (tainted) scrutinee RETURNS the tainted
    // placeholder early — a value-level bottom. In a callee it rides
    // back in-band in the returned disc and bottoms only the call's
    // consumers (the node-walk's select-on-⊥ produces nothing for THIS
    // result only: a recursive callee's depth-tripped tail select used
    // to emit_bottom_abort here, which the caller escalated to a
    // whole-kernel abort — soak-jul08c dv1, where the node-walk's fold
    // recovered from the bottomed init). In a gated parent the return
    // force bottoms the kernel — the same observable as the abort this
    // replaces. The arms then run on a known-valid scrutinee, so the
    // final-arm miss is unreachable.
    {
        let valid = is_untainted(cx.b, scrut.disc());
        let arms_bl = cx.b.create_block();
        let taint_bl = cx.b.create_block();
        cx.b.ins().brif(valid, arms_bl, &[], taint_bl, &[]);
        cx.b.switch_to_block(taint_bl);
        cx.b.seal_block(taint_bl);
        let ph = emit_elem_placeholder(cx, ret)?;
        emit_kernel_return(cx, ret, ph, CompositeSource::Owned)?;
        cx.b.switch_to_block(arms_bl);
        cx.b.seal_block(arms_bl);
    }
    let arm_index = std::cell::Cell::new(0usize);
    emit_select_arms(
        cx,
        sel,
        scrut,
        scrut_kind,
        &scrut_typ,
        &mut |cx, body, mark| {
            let idx = arm_index.get();
            arm_index.set(idx + 1);
            // Scope this select's (word, arm) onto the terminating-path
            // stack: a return inside this arm records it; a tail JUMP
            // never returns, leaving the word untouched by loop
            // mechanics.
            if let Some(w) = sel_word {
                cx.ctx.tail_sel_path.borrow_mut().push((w, idx));
            }
            emit_body_tail(cx, &body.node, ret)?;
            if sel_word.is_some() {
                cx.ctx.tail_sel_path.borrow_mut().pop();
            }
            // The arm terminated; pop its binds for the next arm's
            // compile-time scope. (Arm binds are scalars — no drops.)
            cx.env.truncate(mark);
            Ok(())
        },
        // Unreachable (scrutinee forced valid → exhaustive matches); a
        // terminator is still required.
        &mut |cx| emit_kernel_bottom(cx),
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
    let n = spec_apply.args.len();
    let mut new_vals = Vec::with_capacity(n);
    let mut sources = Vec::with_capacity(n);
    let mut taints = Vec::with_capacity(n);
    for i in 0..n {
        let arg = cs
            .arg_positional(i)
            .ok_or_else(|| anyhow!("emit_clif: self tail-call arg {i} missing"))?;
        let cv = arg.emit_clif(cx)?;
        // A bottomed arg does NOT abort the call: the node-walk's
        // dispatch backfills a quiet-or-failed arg from its cached
        // value (combineLatest — Eric's ruling 2026-07-15: bottom is
        // "no event this cycle", never a NaN-like poison, so
        // `sum_to(n - 1, parse(s)? + n)` keeps looping on the last
        // good acc). The kernel twin: a TAINTED new formal keeps the
        // loop-carried previous value at the rebind. `is_tainted`
        // folds to const-false for a proven-fresh disc — no branch on
        // the hot path.
        taints.push(is_tainted(cx.b, cv.disc));
        new_vals.push(cv);
        sources.push(node_composite_source(arg));
    }
    emit_tail_rebind_jump(cx.b, cx.env, cx.ctx, new_vals, &sources, &taints)
}

/// Bind one `let local = value` into the env by the value's runtime
/// shape — the direct-path mirror of `compile_block_scalar`'s let
/// arms (composite/value lets clone borrowed sources so this scope
/// exclusively owns them; the scope-exit drop would otherwise free a
/// buffer the enclosing scope still holds).
fn emit_let_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    name: &ArcStr,
    bind_id: Option<BindId>,
    value: &Node<R, E>,
) -> Result<()> {
    // LIFTED connect target (a let-bound scalar counter routed in as a
    // feeder): bind it to a SEED-SELECT — the feeder (entry param) when it
    // has a value (untainted, i.e. ever fired), else the constant SEED
    // (this let's value). Reproduces the node-walk exactly: the variable's
    // value is the last `set_var`'d value, or the one-shot `Bind` seed
    // until the first write. The seed is a STALE-gated constant
    // (`emit_const_node` — fresh at init, stale after), so reads see the
    // node-walk's firing; the feeder carries its own fresh/stale bit. The
    // entry param is the binding the kernel-entry binder installed under
    // this same `bind_id` (the lift added it to `inputs`); this let then
    // SHADOWS it, so post-let reads resolve to the seed-select result.
    if let Some(id) = bind_id {
        if cx.is_lifted(id) {
            let vv = {
                let l = cx.env.lookup(id, name).ok_or_else(|| {
                    anyhow!("emit_clif: lifted target `{name}` has no feeder param")
                })?;
                l.vv
            };
            let pdisc = cx.b.use_var(vv.disc);
            let ppay = cx.b.use_var(vv.payload);
            let valid = is_untainted(cx.b, pdisc);
            // Inside a select ARM body (init-override active), the
            // node-walk RE-FIRES this bind's seed on every arm init
            // view (wake/entry — node/select.rs sets `event.init`),
            // preferring it over the feeder's retained value; the
            // connect then rebuilds from the seed (soak jul08g fuzz
            // divergence 6). Outside arms the feeder wins whenever it
            // has ever fired, exactly as before (at kernel init the
            // feeder is tainted, so the seed wins there too).
            let use_feeder = match cx.ctx.init_override.get() {
                None => valid,
                Some(eff) => {
                    let quiet = cx.b.ins().icmp_imm(IntCC::Equal, eff, 0);
                    cx.b.ins().band(valid, quiet)
                }
            };
            let frozen =
                kernel_abi::freeze_for_abi_normalized(cx.registry(), value.typ());
            let ak = frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t));
            match ak {
                Some(AbiKind::Scalar(p)) => {
                    // Register scalars are branch-free: both sides are
                    // plain values, select the feeder or the seed.
                    let seed = value.emit_clif(cx)?;
                    let disc = cx.b.ins().select(use_feeder, pdisc, seed.disc);
                    let payload = cx.b.ins().select(use_feeder, ppay, seed.payload);
                    bind_local(
                        cx,
                        name.clone(),
                        disc,
                        payload,
                        LocalKind::Scalar(p),
                        Some(id),
                    );
                }
                // A composite / string accumulator (`let data = []; data <-
                // array::push(data, x)`) needs OWNERSHIP on both sides:
                // the feeder path CLONES the entry param (the param local
                // keeps its own allocation — both drop at the return via
                // `drop_owned_composites`), the seed path emits the fresh
                // literal only when taken (no allocation to discard on the
                // other side).
                Some(k @ (AbiKind::Array | AbiKind::Tuple | AbiKind::Struct))
                | Some(k @ AbiKind::String) => {
                    let is_string = matches!(k, AbiKind::String);
                    let use_param = cx.b.create_block();
                    let use_seed = cx.b.create_block();
                    let merge = cx.b.create_block();
                    cx.b.append_block_param(merge, types::I64); // disc
                    cx.b.append_block_param(merge, types::I64); // ptr/bits
                    cx.b.ins().brif(use_feeder, use_param, &[], use_seed, &[]);
                    cx.b.switch_to_block(use_param);
                    cx.b.seal_block(use_param);
                    let clone_helper = if is_string {
                        cx.helper("graphix_arcstr_clone")?
                    } else {
                        cx.helper("graphix_valarray_clone")?
                    };
                    let call = cx.b.ins().call(clone_helper, &[ppay]);
                    let cloned = cx.b.inst_results(call)[0];
                    cx.b.ins()
                        .jump(merge, &[BlockArg::Value(pdisc), BlockArg::Value(cloned)]);
                    cx.b.switch_to_block(use_seed);
                    cx.b.seal_block(use_seed);
                    let seed = value.emit_clif(cx)?;
                    let seed_owned = if is_string {
                        // Strings are owned at production.
                        seed.payload
                    } else {
                        ensure_owned_composite_src(
                            cx,
                            node_composite_source(value),
                            seed.payload,
                        )?
                    };
                    cx.b.ins().jump(
                        merge,
                        &[BlockArg::Value(seed.disc), BlockArg::Value(seed_owned)],
                    );
                    cx.b.switch_to_block(merge);
                    cx.b.seal_block(merge);
                    let (disc, payload) = {
                        let params = cx.b.block_params(merge);
                        (params[0], params[1])
                    };
                    let kind =
                        if is_string { LocalKind::String } else { LocalKind::Composite };
                    bind_local(cx, name.clone(), disc, payload, kind, Some(id));
                }
                other => {
                    return Err(anyhow!(
                        "emit_clif: lifted connect target `{name}` of shape \
                         {other:?} — not yet supported"
                    ));
                }
            }
            return Ok(());
        }
    }
    // `freeze_for_abi_normalized` so a select-valued let (whose type is the
    // un-normalized arm union) still classifies.
    let frozen = kernel_abi::freeze_for_abi_normalized(cx.registry(), value.typ());
    let ak = frozen.as_ref().and_then(|t| kernel_abi::abi_kind(cx.registry(), t));
    match ak {
        Some(AbiKind::Scalar(p)) => {
            // The disc carries the binding's taint — a `let`-bound bottom
            // flows to its uses; an unconsumed one is dropped, never
            // bottoming (#219).
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
            // A function-valued let can NEVER emit by design — a
            // lambda isn't a kernel value; its call sites fuse as
            // cross-kernel calls while the binding itself node-walks
            // (publishing the LambdaDef). Distinct message so probe
            // assertions can treat it as structural recurse noise,
            // not a coverage gap.
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
fn emit_discard_result<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
    cv: CompiledExpr,
) -> Result<()> {
    let owned = matches!(node_composite_source(node), CompositeSource::Owned);
    match kernel_abi::abi_kind(cx.registry(), node.typ()) {
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) if owned => {
            let drop = cx.helper("graphix_valarray_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
        }
        Some(AbiKind::String) => {
            let drop = cx.helper("graphix_arcstr_drop")?;
            cx.b.ins().call(drop, &[cv.payload]);
        }
        // #219: a tainted 2-word value drops its (disc, payload) like an
        // untainted one — clean the disc so the helper sees a valid tag.
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) if owned => {
            let drop = cx.helper("graphix_value_drop")?;
            cx.b.ins().call(drop, &[cv.disc, cv.payload]);
        }
        _ => {}
    }
    Ok(())
}

/// Emit runtime drops for every owned composite / variant / nullable /
/// string local this scope introduced above `mark` — the direct-path
/// mirror of `compile_block_scalar`'s scope-exit drop block. Scalars
/// need no drop.
fn emit_scope_drops(cx: &mut BodyCx, mark: usize) -> Result<()> {
    let arr_drop = cx.helper("graphix_valarray_drop")?;
    let val_drop = cx.helper("graphix_value_drop")?;
    let str_drop = cx.helper("graphix_arcstr_drop")?;
    // Snapshot the (kind, vv) of every local above the mark so the
    // `cx.env` borrow ends before we drive `cx.b`.
    let drops: smallvec::SmallVec<[(LocalKind, ValueVar); 8]> =
        cx.env.locals[mark..].iter().map(|l| (l.kind, l.vv)).collect();
    for (kind, vv) in drops {
        match kind {
            LocalKind::Scalar(_) => {}
            LocalKind::Composite => {
                let ptr = cx.b.use_var(vv.payload);
                cx.b.ins().call(arr_drop, &[ptr]);
            }
            LocalKind::String => {
                let ptr = cx.b.use_var(vv.payload);
                cx.b.ins().call(str_drop, &[ptr]);
            }
            LocalKind::Variant | LocalKind::Nullable | LocalKind::Value => {
                let disc = cx.b.use_var(vv.disc);
                let payload = cx.b.use_var(vv.payload);
                cx.b.ins().call(val_drop, &[disc, payload]);
            }
        }
    }
    Ok(())
}

/// Emit the error-delivery DynCall for a handler-ful `?` (a `?` caught
/// by an enclosing `try` — `FnSource::QopDeliver`). Marshals the error
/// value `cv` as the single Value-shape argument and dispatches the
/// pre-bound `QopDeliverApply`, which runs `wrap_error` + writes the
/// catch handler's variable (exactly `Qop::update`'s handler path). Unit
/// return, discarded — the caller's error branch bottoms the result
/// separately (the scalar taint). `inner_owned` selects owned vs
/// borrowed push: the error rides the inner's ownership, and the owned
/// push hands it to `QopDeliverApply` to drop (no separate drop here).
fn emit_qop_deliver(
    cx: &mut BodyCx,
    site_id: ExprId,
    cv: &CompiledExpr,
    inner_owned: bool,
) -> Result<()> {
    let info = cx
        .builtin_site(site_id)
        .ok_or_else(|| anyhow!("emit_clif: qop-deliver site {site_id:?} not discovered"))?
        .clone();
    let buf_new = cx.helper("graphix_value_buf_new")?;
    let cap = cx.b.ins().iconst(types::I64, 1);
    let call = cx.b.ins().call(buf_new, &[cap]);
    let buf = cx.b.inst_results(call)[0];
    let buf_var = cx.b.declare_var(types::I64);
    cx.b.def_var(buf_var, buf);
    cx.ctx.dyncall_buf_stack.borrow_mut().push(buf_var);
    // The error rides the 2-word Value wire shape; the pushed disc must be
    // CLEAN (a tainted disc is an invalid tag).
    let clean = clean_disc(cx.b, cv.disc);
    let push_name = if inner_owned {
        "graphix_value_buf_push_value"
    } else {
        "graphix_value_buf_push_value_borrowed"
    };
    let push = cx.helper(push_name)?;
    cx.b.ins().call(push, &[buf, clean, cv.payload]);
    let dyncall = cx.helper("graphix_dyncall")?;
    // Region-wide slot index: the site's local fn_index plus this body's
    // base offset into the combined `dyn_slots` table.
    let region_idx = info.fn_index + cx.fn_index_offset();
    let fn_idx = cx.b.ins().iconst(types::I32, region_idx as i64);
    // QopDeliverApply returns Value::Null; the pair is discarded.
    cx.b.ins().call(dyncall, &[fn_idx, buf]);
    // `QopDeliverApply::update` structurally returns `Some(Null)` (the
    // marshalled arg is always present), but every dyncall site clears
    // the pending flag so it can only ever mean "genuine abort".
    let take = cx.helper("graphix_dyncall_pending_take_clear")?;
    cx.b.ins().call(take, &[]);
    cx.ctx.dyncall_buf_stack.borrow_mut().pop();
    Ok(())
}

/// `?` / `$` — both unwrap a Nullable<T> to T (else pass the value
/// through unchanged for a non-Nullable inner, mirroring `wrap_qop`'s
/// None branch). Scalar / string / composite success returns the
/// unwrapped element; Value-shape success returns the (disc, payload)
/// pair.
///
/// `result_typ` is the qop NODE's static type — the arm selection
/// keys on it, NOT on the inner's one-layer option success. The
/// typechecker (and the node-walk, which drops ANY error value)
/// strips EVERY error member of the flattened inner union — so for a
/// nested fallible union like `[[string, Error<E>], Error<AIE>]` the
/// node's type is `string` and every consumer (the kernel return
/// included) expects the STRING convention, while the inner's
/// one-layer success `[string, Error<E>]` freezes value-shape. Arm-
/// selecting on the latter minted the value-shape `(Null, 0)`
/// placeholder on the error path, and the String-conventioned
/// consumer dropped payload 0 — `graphix_arcstr_drop(NULL)` SIGABRT
/// (soak jul05 item 15, crash_000014).
/// True iff a FROZEN (concrete, deref'd) type has any error member —
/// the gate for `emit_qop_node`'s no-union passthrough.
fn type_may_error(t: &Type) -> bool {
    match t {
        Type::Error(_) => true,
        Type::Primitive(p) => p.contains(netidx_value::Typ::Error),
        Type::Set(members) => members.iter().any(type_may_error),
        _ => false,
    }
}

pub(crate) fn emit_qop_node<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    inner: &Node<R, E>,
    result_typ: &Type,
    handler_site: Option<ExprId>,
) -> Result<CompiledExpr> {
    // Lockstep with the discovery-side freeze (lowering.rs
    // try_register_qop_deliver): both normalized, so a site the
    // discovery registered always emits.
    let Some(inner_typ) =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), inner.typ())
    else {
        return Err(anyhow!(
            "emit_clif: `?` inner type {:?} doesn't freeze concrete",
            inner.typ()
        ));
    };
    if kernel_abi::nullable_inner(cx.registry(), &inner_typ).is_none() {
        // Passthrough is only sound when the inner CANNOT be an error.
        // `nullable_inner` answers "is this a `[T, Error<E>]` union" —
        // a bare `Error<T>` inner (`error(x)$`) is not a union but is
        // ALWAYS an error, and passing it through handed the error
        // Value's payload word to the success consumer as a scalar
        // (ASLR-varying garbage output; soak jul12f/jul12g fuzz
        // divergence_000000). The node-walk checks `Value::Error` at
        // runtime regardless of the static type — an error-bearing
        // non-union inner de-fuses to it.
        if type_may_error(&inner_typ) {
            return Err(anyhow!(
                "emit_clif: `?`/`$` inner type {inner_typ:?} can be an error                  but is not a [T, Error<E>] union — node-walk handles it"
            ));
        }
        // No error possible — passthrough; the handler (if any) never fires.
        return inner.emit_clif(cx);
    }
    let Some(success_typ) =
        kernel_abi::freeze_for_abi_normalized(cx.registry(), result_typ)
    else {
        return Err(anyhow!(
            "emit_clif: `?` result type {:?} doesn't freeze concrete",
            result_typ
        ));
    };
    let cv = inner.emit_clif(cx)?;
    let (disc, payload) = (cv.disc, cv.payload);
    // `clean(disc) == Typ::Error` (`0x2000_0000`) means bottom (mask
    // taint first — a tainted disc is not a structural Error).
    let clean = clean_disc(cx.b, disc);
    let is_err = cx.b.ins().icmp_imm(IntCC::Equal, clean, 0x2000_0000_i64);
    // A TAINTED error is a PHANTOM — computed from #219 placeholders
    // after an upstream bottom (e.g. `(a[BAD]? /? y)?`: the index raise
    // taints, the checked div then computes `0 /? 0` on placeholders
    // and mints a REAL ArithError value). The node-walk never runs the
    // div at all, so delivering it to a catch handler invents an error
    // the program never raised (soak finding corpus-fuzz/
    // divergence_000030: the wrong catch arm fired). Delivers below
    // gate on `deliverable`; the abort/taint paths still treat it as
    // no-value.
    let untainted = is_untainted(cx.b, disc);
    let deliverable = cx.b.ins().band(is_err, untainted);
    match kernel_abi::abi_kind(cx.registry(), &success_typ) {
        // Prim success — BRANCHLESS per-value taint. The payload word
        // holds the success bits when !is_err; on the error path the
        // bits are garbage but the disc's TAINT means they're never
        // used (forced at the output). The error Value isn't dropped
        // here: a scalar `Nullable` inner is a by-value scalar (no
        // heap). #219: the inner's own taint also flows through.
        Some(AbiKind::Scalar(p)) => {
            // Handler-ful `?` (a `?` caught by an enclosing `try`): on the
            // error path, deliver the error to the catch handler's
            // variable before bottoming (mirrors `Qop::update`'s handler
            // path). The catch handler reading that variable is a separate
            // kernel (next cycle), so no read-after-write hazard.
            if let Some(site) = handler_site {
                let deliver_block = cx.b.create_block();
                let after = cx.b.create_block();
                cx.b.ins().brif(deliverable, deliver_block, &[], after, &[]);
                cx.b.switch_to_block(deliver_block);
                cx.b.seal_block(deliver_block);
                let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
                emit_qop_deliver(cx, site, &cv, inner_owned)?;
                cx.b.ins().jump(after, &[]);
                cx.b.switch_to_block(after);
                cx.b.seal_block(after);
            }
            let value = cast_u64_to_prim(cx.b, payload, p);
            let base = scalar_disc(cx.b, p);
            let disc = propagate_flags(cx.b, base, &[cv.disc]);
            let disc = taint_if(cx.b, disc, is_err);
            // Interior-bottom exactness: a `$`/`?`-dropped error with
            // prior success degrades to STALE + the cached value,
            // matching the node-walk's cached qop node.
            Ok(emit_scalar_taint_cache(cx, p, CompiledExpr::new(disc, value)))
        }
        // String / composite success — branch: the bad path (error OR
        // tainted) produces a tainted shape-safe PLACEHOLDER and
        // CONTINUES (interior-bottom v2, 2026-07-04) instead of the old
        // whole-kernel pending-exit, which escalated a locally
        // unconsumed bottom into no-output-at-all (the live-chain
        // findings, triage item 11).
        Some(AbiKind::String | AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let is_string = matches!(
                kernel_abi::abi_kind(cx.registry(), &success_typ),
                Some(AbiKind::String)
            );
            let base_disc =
                if is_string { value_disc::STRING } else { value_disc::ARRAY };
            let value_drop = cx.helper("graphix_value_drop")?;
            let inner_owned = node_composite_source(inner) == CompositeSource::Owned;
            let pre_pending = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let qmerge = cx.b.create_block();
            cx.b.append_block_param(qmerge, types::I64); // disc
            cx.b.append_block_param(qmerge, types::I64); // payload
            // #219: a TAINTED inner may carry the helper-safe Value::Null
            // placeholder, whose CLEAN disc isn't Error — without folding
            // taint into this branch the success path unboxes Null as an
            // Array/ArcStr (`unreachable_unchecked` UB in the unboxers;
            // soak crash 2026-07-04, `x$` of a tainted slice). Tainted =
            // "no value" = the same abort as the error path.
            let tainted = is_tainted(cx.b, disc);
            let bad = cx.b.ins().bor(is_err, tainted);
            cx.b.ins().brif(bad, pre_pending, &[], continue_block, &[]);
            cx.b.switch_to_block(pre_pending);
            cx.b.seal_block(pre_pending);
            // Abort path. Handler-ful `?`: deliver a REAL error to the
            // catch handler — the deliver CONSUMES the owned error (or
            // clones a borrowed one), so it replaces the `value_drop`
            // (dropping AND delivering an owned error would double-free).
            // A tainted non-error must NOT deliver (the handler would
            // receive placeholder garbage) but an owned one still drops.
            // Handler-less: drop the owned value (a Borrowed Local is
            // owned by its env slot, which `emit_pending_cleanup` drops —
            // dropping here too would double-free).
            if let Some(site) = handler_site {
                let deliver_block = cx.b.create_block();
                let no_deliver = cx.b.create_block();
                let merged = cx.b.create_block();
                cx.b.ins().brif(deliverable, deliver_block, &[], no_deliver, &[]);
                cx.b.switch_to_block(deliver_block);
                cx.b.seal_block(deliver_block);
                emit_qop_deliver(cx, site, &cv, inner_owned)?;
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(no_deliver);
                cx.b.seal_block(no_deliver);
                if inner_owned {
                    cx.b.ins().call(value_drop, &[clean, payload]);
                }
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(merged);
                cx.b.seal_block(merged);
            } else if inner_owned {
                cx.b.ins().call(value_drop, &[clean, payload]);
            }
            // Tainted shape-safe placeholder; the inner's STALE carries
            // (the qop fires iff its inner fired) and TAINT marks
            // no-value — downstream consumers run harmlessly on the
            // placeholder and the output gates only if it's consumed.
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
            // Extract success T (now known non-error). The unwrap
            // result is Owned, so a String/composite success from a
            // Borrowed inner must be cloned. String: the ArcStr bits
            // inside a Value ARE the string ABI's word. Composite: the
            // payload word IS the ValArray bits, but the narrowing
            // stays CHECKED — unwrap via `graphix_value_into_array`
            // (consumes) / `_borrowed` (clones inner), which abort
            // defined on a non-Array (the tainted Null placeholder is
            // reachable here, #199).
            let v = match kernel_abi::abi_kind(cx.registry(), &success_typ) {
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
            Ok(CompiledExpr::new(params[0], params[1]))
        }
        // Value-shape success. The bad path (error) produces a tainted
        // Value::Null placeholder and CONTINUES (interior-bottom v2);
        // otherwise the non-error Value IS the result T (its own
        // `(disc, payload)`, passed through to the consumer which takes
        // ownership).
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value) => {
            let value_drop = cx.helper("graphix_value_drop")?;
            let src = node_composite_source(inner);
            let inner_owned = src == CompositeSource::Owned;
            let pre_pending = cx.b.create_block();
            let continue_block = cx.b.create_block();
            let qmerge = cx.b.create_block();
            cx.b.append_block_param(qmerge, types::I64); // disc
            cx.b.append_block_param(qmerge, types::I64); // payload
            cx.b.ins().brif(is_err, pre_pending, &[], continue_block, &[]);
            cx.b.switch_to_block(pre_pending);
            cx.b.seal_block(pre_pending);
            // Error path. Handler-ful `?`: deliver a REAL (untainted)
            // error to the catch handler (consumes the owned error /
            // clones a borrowed one), so it replaces the `value_drop`;
            // a PHANTOM (tainted) error must not deliver — see
            // `deliverable` above — but an owned one still drops.
            // Handler-less: drop the owned error before aborting — but
            // ONLY when `inner` is an owned producer. A Borrowed (Ref)
            // inner is owned by its env slot, which
            // `emit_pending_cleanup` -> `drop_owned_composites` already
            // drops; dropping it here too would double-free.
            if let Some(site) = handler_site {
                let deliver_block = cx.b.create_block();
                let no_deliver = cx.b.create_block();
                let merged = cx.b.create_block();
                cx.b.ins().brif(deliverable, deliver_block, &[], no_deliver, &[]);
                cx.b.switch_to_block(deliver_block);
                cx.b.seal_block(deliver_block);
                emit_qop_deliver(cx, site, &cv, inner_owned)?;
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(no_deliver);
                cx.b.seal_block(no_deliver);
                if inner_owned {
                    cx.b.ins().call(value_drop, &[clean, payload]);
                }
                cx.b.ins().jump(merged, &[]);
                cx.b.switch_to_block(merged);
                cx.b.seal_block(merged);
            } else if inner_owned {
                cx.b.ins().call(value_drop, &[clean, payload]);
            }
            // Tainted Value::Null placeholder (helper-safe by
            // construction); the inner's STALE carries.
            let tainted_base = cx.b.ins().iconst(types::I64, value_disc::NULL | TAINT);
            let stale_bit = cx.b.ins().band_imm(cv.disc, STALE);
            let ph_disc = cx.b.ins().bor(tainted_base, stale_bit);
            let zero = cx.b.ins().iconst(types::I64, 0);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(ph_disc), BlockArg::Value(zero)]);
            cx.b.switch_to_block(continue_block);
            cx.b.seal_block(continue_block);
            // The non-error Value IS the result T. Ensure it's owned: a
            // Borrowed (Ref) inner aliases its env slot, which is also
            // dropped at scope exit — handing those bits to the consumer
            // (the unwrap result is Owned) would double-free. An Owned
            // inner passes through unchanged. Clean for the clone; the
            // inner's taint re-attaches to the result.
            let (od, op) = ensure_owned_value_src(cx, src, clean, payload)?;
            // `e?` fires iff its operand fired (single input); STALE folds.
            let disc = propagate_flags(cx.b, od, &[cv.disc]);
            cx.b.ins().jump(qmerge, &[BlockArg::Value(disc), BlockArg::Value(op)]);
            cx.b.switch_to_block(qmerge);
            cx.b.seal_block(qmerge);
            let params = cx.b.block_params(qmerge);
            Ok(CompiledExpr::new(params[0], params[1]))
        }
        Some(AbiKind::Unit | AbiKind::Null) | None => {
            Err(anyhow!("emit_clif: `?` with unsupported success type {:?}", success_typ))
        }
    }
}

// (The old `emit_dyncall_arg_taint_abort` — post-call whole-kernel
// bottom on a tainted arg — is gone: `emit_dyncall_node` now gates the
// CALL itself and produces a tainted placeholder, so the callee never
// observes placeholder garbage and live chains keep flowing. Its doc's
// "taint can't survive a composite/string let" caveat was stale:
// `bind_local` stores the RHS disc for every LocalKind.)
