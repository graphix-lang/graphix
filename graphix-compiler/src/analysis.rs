//! Compile-time function-property analysis.
//!
//! Runs once after `typecheck1`, before fusion — ALWAYS (not gated on
//! `ctx.fusion.enabled`), because the node-walk interpreter consumes its
//! results too. It computes per-lambda and per-call-site facts that BOTH
//! backends read, so the canonical node-walk and the cranelift JIT agree
//! on which functions LOOP vs RECURSE.
//!
//! Motivation: a sync self-tail-recursive lambda (`let rec f = |v| f(v+1)`)
//! compiles to a native loop in the JIT (constant stack), but the
//! interpreter dispatches the recursive call on the Rust stack and
//! overflows. The shared facts let the interpreter loop in place instead
//! (`GXLambda::update`), matching the JIT.
//!
//! Two phases over the reachable call graph:
//!   1. **Effect inference (M6)** — a greatest fixpoint writing each
//!      reachable lambda's `LambdaDef::intrinsic_effect`. Optimistic start
//!      (`Sync`), monotonically degrading to `Async`, so mutual recursion
//!      of pure functions settles on `Sync`.
//!   2. **Recursion / tail marking** — per resolved-lambda call site, set
//!      the callee's `GXLambda::tail_loop` (the operational interpreter
//!      gate = structural tail-loop AND sync), mark the body's
//!      tail-position self-call(s) (`CallSite::is_self_tail_call` +
//!      `tail_arg_order` + `callee_lambda_id`), and record the
//!      `RecursionKind` summary.
//!
//! The STRUCTURAL tail-loop predicate is shared with the JIT
//! (`fusion::lowering::structural_tail_loop`) — one seam, so the backends
//! can't disagree on which lambdas are loop-able.

use crate::{
    ApplyView, BindId, ExecCtx, LambdaId, Node, NodeView, Refs, Rt, UserEvent,
    effects::{EffectKind, RecursionKind},
    expr::ExprKind,
    fusion::{self, lowering},
    node::{
        callsite::{ArgKey, CallSite},
        lambda::{GXLambda, LambdaDef},
    },
};
use anyhow::Result;
use nohash::{IntMap, IntSet};
use std::sync::atomic::Ordering;

/// Run the analysis over the whole compiled program. `root` is the
/// top-level node; `ctx` is read immutably — every result lands via
/// interior mutability (atomics / `Mutex`) on the nodes it reaches.
pub fn analyze<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    // Pass 1: discover every reachable statically-resolved lambda call
    // site, descending through callee bodies. Each site is a (callee,
    // self_bind) pair — `self_bind` is the call's `fnode` Ref id, exactly
    // what `build_lambda_kernel` derives.
    let sites = collect_resolved_sites(root);
    // Pass 2: effect fixpoint over the reachable bodies.
    let (eff, self_ids) = infer_effects(&sites, ctx);
    // Pass 3: recursion + tail marking (reads the effects from pass 2).
    for (g, self_bind) in &sites {
        mark_recursion(g, *self_bind, ctx);
    }
    // Pass 4: For-loop body effects (sync-subset P4 final). An
    // async-effect body flips the For to per-index instantiation +
    // re-evaluation (the never-until-complete async elaboration);
    // reads the pass-2 effect facts through the same node_effect walk.
    // MUST be fed pass 2's maps: with empty maps every resolved lambda
    // call read as the Async default, so every in-language HOF body
    // (`f(acc, v)`) took the per-index instantiation path — 100k body
    // compiles for a 100k fold (bench/fold_sum, 2026-07-10).
    mark_for_bodies(root, ctx, &eff, &self_ids);
    Ok(())
}

/// [`mark_for_bodies`] for a subtree with no precomputed effect facts
/// (a runtime-compiled `For` instance): run the site discovery +
/// effect fixpoint over the subtree first.
pub(crate) fn mark_for_bodies_standalone<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) {
    let sites = collect_resolved_sites(root);
    let (eff, self_ids) = infer_effects(&sites, ctx);
    mark_for_bodies(root, ctx, &eff, &self_ids);
}

/// Mark every reachable `For` node whose BODY has an async effect —
/// walks the root and, like the effect passes, descends resolved
/// callee bodies (a For inside an in-language HOF's instance body is
/// the primary customer).
pub(crate) fn mark_for_bodies<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
    eff: &IntMap<LambdaId, EffectKind>,
    self_ids: &IntMap<BindId, LambdaId>,
) {
    let mut bodies: Vec<&Node<R, E>> = vec![root];
    let mut seen: ahash::AHashSet<*const u8> = ahash::AHashSet::default();
    while let Some(b) = bodies.pop() {
        fusion::for_each_node(b, &mut |n| match n.view() {
            NodeView::For(fl) => {
                let e = body_effect(&fl.body, eff, self_ids, ctx);
                fl.set_async_body(matches!(e, EffectKind::Async));
            }
            NodeView::CallSite(cs) => {
                if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
                    let ptr = (g as *const GXLambda<R, E>).cast::<u8>();
                    if seen.insert(ptr) {
                        bodies.push(g.body());
                    }
                }
            }
            _ => {}
        });
    }
}

/// Analyze a callee bound at RUNTIME (`CallSite::bind`): the lazy-bound
/// apply compiles its body fresh, AFTER the program-wide [`analyze`]
/// pass ran, so nothing in that subtree carries effect/recursion/tail
/// facts — a tail-recursive `let rec` nested in the body stack-recursed
/// into the call-depth guard (bottom at ~256) where the same lambda
/// dispatched through a compile-time-resolved site tail-looped
/// (soak-jul06c B8: rec lambda inside an HOF callback slot). Same three
/// phases as [`analyze`], seeded with the outer `(callee, self_bind)`
/// pair — a lazy-bound site is `DynamicBound`, which
/// `collect_resolved_sites` skips.
pub(crate) fn analyze_bound_callee<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
    ctx: &ExecCtx<R, E>,
) {
    let mut sites = collect_resolved_sites(g.body());
    if let Some(sb) = self_bind {
        sites.push((g, sb));
    }
    let (eff, self_ids) = infer_effects(&sites, ctx);
    for (g, sb) in &sites {
        mark_recursion(g, *sb, ctx);
    }
    mark_for_bodies(g.body(), ctx, &eff, &self_ids);
}

/// Every reachable resolved-lambda call site, as `(callee, self_bind)`.
/// Mirrors `fusion::discover_lambda_calls`' traversal: walk with
/// `for_each_node` (which does NOT descend lambda bodies), and at each
/// resolved-lambda call site push the callee body for a further walk —
/// deduped by `LambdaId` so recursion terminates.
fn collect_resolved_sites<'a, R: Rt, E: UserEvent>(
    root: &'a Node<R, E>,
) -> Vec<(&'a GXLambda<R, E>, BindId)> {
    let mut seen: IntSet<LambdaId> = IntSet::default();
    let mut sites: Vec<(&'a GXLambda<R, E>, BindId)> = Vec::new();
    let mut stack: Vec<&'a Node<R, E>> = vec![root];
    while let Some(node) = stack.pop() {
        // Collect bodies to descend separately, so the closure doesn't
        // borrow `stack` while the outer loop pops it.
        let mut to_descend: Vec<&'a Node<R, E>> = Vec::new();
        fusion::for_each_node(node, &mut |n| {
            let NodeView::CallSite(cs) = n.view() else { return };
            let Some(ApplyView::Lambda(g)) = cs.resolved_apply() else { return };
            if let NodeView::Ref(r) = cs.fnode().view() {
                sites.push((g, r.id));
            }
            if seen.insert(g.id()) {
                to_descend.push(g.body());
            }
        });
        stack.extend(to_descend);
    }
    sites
}

// ── Phase 1: effect inference ────────────────────────────────────────

/// Greatest-fixpoint effect inference. Every reachable lambda starts
/// `Sync` and monotonically degrades to `Async` until stable.
fn infer_effects<R: Rt, E: UserEvent>(
    sites: &[(&GXLambda<R, E>, BindId)],
    ctx: &ExecCtx<R, E>,
) -> (IntMap<LambdaId, EffectKind>, IntMap<BindId, LambdaId>) {
    // Dedup the reachable bodies by LambdaId (a lambda may have many call
    // sites; its body + effect are one).
    //
    // The (callee, self_bind) pairs double as a BACK-EDGE resolution
    // table: a #203-unresolved self-call's fnode Ref can miss
    // `ctx.bind_to_lambda` — a per-slot HOF clone re-mints the `let rec`
    // binding's BindId without a typecheck0 pass, so the map has no
    // entry for the re-minted id — and the fallthrough classified the
    // recursion Async, which disabled the interpreter tail loop and
    // stack-recursed the residue into the depth guard where `--no-fusion`
    // tail-looped to the value (soak jul08g fuzz divergence 4).
    let mut bodies: IntMap<LambdaId, &Node<R, E>> = IntMap::default();
    let mut self_ids: IntMap<BindId, LambdaId> = IntMap::default();
    for (g, sb) in sites {
        bodies.entry(g.id()).or_insert_with(|| g.body());
        self_ids.entry(*sb).or_insert_with(|| g.id());
    }
    let mut eff: IntMap<LambdaId, EffectKind> =
        bodies.keys().map(|id| (*id, EffectKind::Sync)).collect();
    loop {
        let mut changed = false;
        for (lid, body) in &bodies {
            let e = body_effect(body, &eff, &self_ids, ctx);
            if eff.get(lid).copied() != Some(e) {
                eff.insert(*lid, e);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    for (lid, e) in &eff {
        if let Some(d) = lambda_def(ctx, *lid) {
            *d.intrinsic_effect.lock() = *e;
        }
    }
    (eff, self_ids)
}

/// Fold the effect over one lambda body. `for_each_node` does not descend
/// nested lambda bodies, so this sees only THIS body's own operations.
fn body_effect<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    eff: &IntMap<LambdaId, EffectKind>,
    self_ids: &IntMap<BindId, LambdaId>,
    ctx: &ExecCtx<R, E>,
) -> EffectKind {
    let mut acc = EffectKind::Sync;
    fusion::for_each_node(body, &mut |n| {
        let e = node_effect(n, eff, self_ids, ctx);
        if matches!(e, EffectKind::Async) && std::env::var_os("GXDBG_FOR").is_some() {
            eprintln!("EFFECT-ASYNC-NODE node={}", n.spec());
        }
        acc = acc.join(e);
    });
    acc
}

/// The intrinsic effect of a single node. `Async` means "delivers a
/// value on a LATER cycle than its trigger" — a call into an async
/// callee, a sample/`any`, a TryCatch (its catch reads an error variable
/// next cycle), a fused kernel that may pend. A variable WRITE
/// (`connect`, a handler-ful `?`'s error delivery) is NOT async: the
/// write happens this cycle, and the cross-cycle boundary is the READ of
/// the written variable (a feeder, a separate kernel). So those, and
/// same-cycle error handling (`$`, handler-less `?`), are `Sync` — which
/// lets them fuse (their `emit_clif` performs the write) and lets the
/// interpreter tail-loop a recursion containing them. The match is
/// exhaustive on purpose — a new node variant is a compile error here,
/// forcing a sync/async decision rather than a silent default. (Note:
/// EffectKind is orthogonal to `stmt_subtree_effect_free` — a connect is
/// *sync* yet NOT effect-free; dead-statement elimination must still keep
/// it.)
fn node_effect<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    eff: &IntMap<LambdaId, EffectKind>,
    self_ids: &IntMap<BindId, LambdaId>,
    ctx: &ExecCtx<R, E>,
) -> EffectKind {
    match n.view() {
        NodeView::CallSite(cs) => callee_effect(cs, eff, self_ids, ctx),
        // Genuinely cross-cycle: a sample (`~`) or `any` delivers on a
        // later cycle than its trigger; a TryCatch's catch handler reads
        // an error variable a cycle after the `?` writes it; a fused
        // kernel may pend. These stay async.
        NodeView::Sample(_)
        | NodeView::TryCatch(_)
        | NodeView::Any(_)
        | NodeView::FusedKernel(_) => EffectKind::Async,
        // Variable WRITES (`connect`, a handler-ful `?`'s error delivery)
        // and same-cycle error handling (`$`, a handler-less `?`) are
        // SYNC: the write/log happens this cycle; the genuine boundary is
        // the READ of a written variable (handled by feeders, a separate
        // kernel). Classifying them sync lets them fuse and lets the
        // interpreter tail-loop a recursion that contains them. (The
        // *write itself* fusing is gated structurally by `emit_clif`:
        // connect/qop-deliver emit the write; an unfusable case de-fuses
        // gracefully.)
        NodeView::Connect(_)
        | NodeView::ConnectDeref(_)
        | NodeView::Qop(_)
        | NodeView::OrNever(_) => EffectKind::Sync,
        // Pure same-cycle compute / construction / access / control flow.
        NodeView::Bind(_)
        | NodeView::Module(_)
        | NodeView::Block(_)
        | NodeView::For(_)
        | NodeView::Select(_)
        | NodeView::ExplicitParens(_)
        | NodeView::TypeCast(_)
        | NodeView::Not(_)
        | NodeView::Neg(_)
        | NodeView::StringInterpolate(_)
        | NodeView::Struct(_)
        | NodeView::StructWith(_)
        | NodeView::Tuple(_)
        | NodeView::Variant(_)
        | NodeView::Array(_)
        | NodeView::Map(_)
        | NodeView::StructRef(_)
        | NodeView::TupleRef(_)
        | NodeView::ArrayRef(_)
        | NodeView::ArraySlice(_)
        | NodeView::MapRef(_)
        | NodeView::ByRef(_)
        | NodeView::Deref(_)
        | NodeView::Add(_)
        | NodeView::Sub(_)
        | NodeView::Mul(_)
        | NodeView::Div(_)
        | NodeView::Mod(_)
        | NodeView::CheckedAdd(_)
        | NodeView::CheckedSub(_)
        | NodeView::CheckedMul(_)
        | NodeView::CheckedDiv(_)
        | NodeView::CheckedMod(_)
        | NodeView::Eq(_)
        | NodeView::Ne(_)
        | NodeView::Lt(_)
        | NodeView::Gt(_)
        | NodeView::Lte(_)
        | NodeView::Gte(_)
        | NodeView::And(_)
        | NodeView::Or(_)
        | NodeView::Lambda(_)
        | NodeView::Ref(_)
        | NodeView::Constant(_)
        | NodeView::Use(_)
        | NodeView::TypeDef(_)
        | NodeView::Nop(_) => EffectKind::Sync,
    }
}

/// The effect contributed by a call: the callee's effect. A resolved or
/// `bind_to_lambda`-known user lambda contributes its (fixpoint) effect —
/// this is also how a `#203`-unresolved self-call resolves, so a
/// self-recursive sync body stays `Sync`. A builtin contributes its
/// declared `EFFECT`. Anything else (a fn-typed parameter call, a dynamic
/// dispatch) is conservatively `Async`.
fn callee_effect<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    eff: &IntMap<LambdaId, EffectKind>,
    self_ids: &IntMap<BindId, LambdaId>,
    ctx: &ExecCtx<R, E>,
) -> EffectKind {
    // A resolved lambda missing from the LOCAL fixpoint map is one a
    // PRIOR pass analyzed (the subtree walks of `analyze_bound_callee`
    // / `mark_for_bodies_standalone` only cover their subtree): read
    // its STORED fact instead of defaulting Async. Silently defaulting
    // flipped every runtime-bound in-language HOF instance's For to
    // the per-index async path (the callback resolved fine but lived
    // outside the local walk), where a const-valued callback re-fired
    // per dispatch — the p7/p9 over-fire class.
    let known = |lid: LambdaId| -> EffectKind {
        eff.get(&lid).copied().unwrap_or_else(|| {
            lambda_def(ctx, lid).map(|d| *d.intrinsic_effect.lock()).unwrap_or_default()
        })
    };
    // Resolved user lambda.
    if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
        return known(g.id());
    }
    if let NodeView::Ref(r) = cs.fnode().view() {
        // A seeded back-edge (this pass's own (callee, self_bind)
        // pairs) — the ONLY resolution for a runtime-cloned `let rec`
        // whose re-minted binding id is absent from `bind_to_lambda`.
        // Checked first: where both tables know the binding, this one
        // names the ACTUAL instance at the analyzed site (the stale
        // template def in `bind_to_lambda` isn't in `eff`, so it would
        // otherwise read as its stored fact).
        if let Some(lid) = self_ids.get(&r.id) {
            return known(*lid);
        }
        // fnode Ref → a known user-lambda binding (incl. #203 self-calls).
        if let Some(v) = ctx.bind_to_lambda.get(&r.id) {
            if let Some(d) = v.downcast_ref::<LambdaDef<R, E>>() {
                return known(d.id);
            }
        }
    }
    // Builtin callee (resolved or via Ref) → its declared effect.
    if let ExprKind::Ref { name } = &cs.fnode().spec().kind {
        if let Some((_, bind)) = ctx.env.lookup_bind(&cs.scope().lexical, name) {
            let key = (bind.scope.clone(), bind.name.clone());
            if let Some(info) = ctx.builtin_bindings.get(&key) {
                return ctx.builtin_effect(info.name.as_str());
            }
        }
    }
    // Unknown dynamic dispatch / fn-typed parameter call.
    if std::env::var_os("GXDBG_FOR").is_some() {
        eprintln!("EFFECT-ASYNC-FALLBACK cs={}", cs.fnode().spec());
    }
    EffectKind::Async
}

// ── Phase 2: recursion + tail marking ────────────────────────────────

/// For one resolved-lambda call site `(g, self_bind)`: record `g`'s
/// recursion summary, and — when `g` is structurally tail-loop-able AND
/// sync — set the operational `tail_loop` gate and mark the body's
/// tail-position self-call(s) so the interpreter loops them.
fn mark_recursion<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: BindId,
    ctx: &ExecCtx<R, E>,
) {
    // Recursion summary (diagnostic; the operational facts are tail_loop
    // + is_self_tail_call). Computed regardless of loop-ability.
    let tail = lowering::body_has_self_tail_call(g.body(), self_bind);
    let summary = if tail {
        RecursionKind::TailRecursive
    } else if is_self_recursive(g, self_bind) {
        RecursionKind::Recursive
    } else {
        RecursionKind::NotRecursive
    };
    if summary != RecursionKind::NotRecursive {
        if let Some(d) = lambda_def(ctx, g.id()) {
            let mut r = d.recursion.lock();
            if rank(summary) > rank(*r) {
                *r = summary;
            }
        }
    }
    // The interpreter's tail-loop gate: structural tail-loop (shared with
    // the JIT) AND sync (so we never loop an async-dependent recursion —
    // that must advance cycle-by-cycle). Only set when the body's tail
    // self-call(s) actually get marked, so `tail_loop` never promises a
    // loop the interpreter can't perform.
    let structural = lowering::structural_tail_loop(g, self_bind, ctx);
    let sync = lambda_is_sync(ctx, g.id());
    if std::env::var("GRAPHIX_DBG_DEPTH").is_ok() {
        eprintln!(
            "MARK_RECURSION id={:?} self_bind={:?} structural={structural} sync={sync}",
            g.id(),
            self_bind
        );
    }
    if structural && sync && mark_tail_sites(g.body(), self_bind, g.id()) {
        g.set_tail_loop(true);
    }
}

/// Walk the body's tail positions (mirroring `body_has_self_tail_call`)
/// and mark each tail-position self-call. Returns whether at least one
/// site was marked.
fn mark_tail_sites<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    self_bind: BindId,
    callee: LambdaId,
) -> bool {
    match node.view() {
        NodeView::Block(b) => {
            b.children.last().is_some_and(|c| mark_tail_sites(c, self_bind, callee))
        }
        NodeView::ExplicitParens(ep) => mark_tail_sites(&ep.n, self_bind, callee),
        NodeView::Select(s) => {
            let mut any = false;
            for (_, body) in s.arms.iter() {
                any |= mark_tail_sites(&body.node, self_bind, callee);
            }
            any
        }
        NodeView::CallSite(cs) => {
            if !matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == self_bind) {
                return false;
            }
            let Some(order) = positional_arg_order(cs) else {
                return false;
            };
            cs.is_self_tail_call.store(true, Ordering::Relaxed);
            *cs.tail_arg_order.lock() = Some(order);
            *cs.callee_lambda_id.lock() = Some(callee);
            true
        }
        _ => false,
    }
}

/// The call's positional argument `BindId`s in order — the tail-loop's
/// per-iteration rebind list. `None` unless the call is purely positional
/// (which a self-call to an all-positional-formal callee always is — and
/// `structural_tail_loop` already gated on all-positional formals).
fn positional_arg_order<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
) -> Option<Box<[BindId]>> {
    let mut order: Vec<BindId> = Vec::new();
    while let Some(a) = cs.args.get(&ArgKey::Positional(order.len())) {
        order.push(a.id);
    }
    if order.is_empty() || cs.args.len() != order.len() {
        // Empty, or some labeled arg present — not a simple positional call.
        return None;
    }
    Some(order.into_boxed_slice())
}

// ── helpers ──────────────────────────────────────────────────────────

fn is_self_recursive<R: Rt, E: UserEvent>(g: &GXLambda<R, E>, self_bind: BindId) -> bool {
    let mut refs = Refs::default();
    g.body().refs(&mut refs);
    let mut found = false;
    refs.with_external_refs(|id| {
        if id == self_bind {
            found = true;
        }
    });
    found
}

fn rank(k: RecursionKind) -> u8 {
    match k {
        RecursionKind::NotRecursive => 0,
        RecursionKind::Recursive => 1,
        RecursionKind::TailRecursive => 2,
    }
}

fn lambda_def<'a, R: Rt, E: UserEvent>(
    ctx: &'a ExecCtx<R, E>,
    lid: LambdaId,
) -> Option<&'a LambdaDef<R, E>> {
    ctx.lambda_defs.get(&lid).and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
}

fn lambda_is_sync<R: Rt, E: UserEvent>(ctx: &ExecCtx<R, E>, lid: LambdaId) -> bool {
    lambda_def(ctx, lid).map(|d| d.intrinsic_effect.lock().is_sync()).unwrap_or(false)
}
