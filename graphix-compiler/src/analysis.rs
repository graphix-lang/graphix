//! Compile-time function-property analysis, run after `typecheck1` in
//! both fusion modes. Three passes over the reachable call graph:
//! effect inference (a greatest fixpoint from `Sync` down to `Async`),
//! the instance call graph, and recursion/tail marking (SCCs,
//! `GXLambda::tail_loop`, `CallSite::is_self_tail_call`,
//! `RecursionKind`). Both engines read the facts; the structural
//! tail-loop predicate is `fusion::lowering::structural_tail_loop`.

use crate::{
    ApplyView, BindId, ExecCtx, LambdaId, LambdaInstanceId, Node, NodeView, Rt,
    UserEvent,
    effects::{EffectKind, RecursionKind},
    expr::ExprKind,
    fusion::{self, lowering},
    node::{
        callsite::{ArgKey, CallSite},
        lambda::{GXLambda, LambdaDef},
    },
    profile::{self, Phase},
};
use ahash::AHashSet;
use anyhow::Result;
use nohash::{IntMap, IntSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::sync::atomic::Ordering;

struct StaticEdge<'a, R: Rt, E: UserEvent> {
    caller: Option<LambdaInstanceId>,
    callee: LambdaInstanceId,
    site: &'a CallSite<R, E>,
}

struct StaticCallGraph<'a, R: Rt, E: UserEvent> {
    instances: LPooled<IntMap<LambdaInstanceId, &'a GXLambda<R, E>>>,
    edges: LPooled<Vec<StaticEdge<'a, R, E>>>,
}

fn collect_static_graph<'a, R: Rt, E: UserEvent>(
    root: &'a Node<R, E>,
    seed: Option<&'a GXLambda<R, E>>,
) -> StaticCallGraph<'a, R, E> {
    let _profile = profile::phase(Phase::CallGraph);
    let mut instances: LPooled<IntMap<LambdaInstanceId, &'a GXLambda<R, E>>> =
        LPooled::take();
    let mut edges: LPooled<Vec<StaticEdge<'a, R, E>>> = LPooled::take();
    let mut seen: LPooled<IntSet<LambdaInstanceId>> = LPooled::take();
    let mut stack: LPooled<Vec<(&'a Node<R, E>, Option<LambdaInstanceId>)>> =
        LPooled::take();
    match seed {
        Some(g) => {
            instances.insert(g.instance_id(), g);
            seen.insert(g.instance_id());
            stack.push((g.body(), Some(g.instance_id())));
        }
        None => stack.push((root, None)),
    }
    while let Some((node, caller)) = stack.pop() {
        let mut descend: LPooled<Vec<(&'a Node<R, E>, LambdaInstanceId)>> =
            LPooled::take();
        fusion::for_each_node(node, &mut |n| {
            let NodeView::CallSite(site) = n.view() else { return };
            if let Some(target) = site.static_target() {
                edges.push(StaticEdge { caller, callee: target.instance, site });
            }
            let Some(ApplyView::Lambda(g)) = site.resolved_apply() else {
                return;
            };
            let instance = g.instance_id();
            instances.entry(instance).or_insert(g);
            if seen.insert(instance) {
                descend.push((g.body(), instance));
            }
        });
        stack.extend(descend.drain(..).map(|(body, instance)| (body, Some(instance))));
    }
    StaticCallGraph { instances, edges }
}

fn strongly_connected<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
) -> (
    LPooled<IntMap<LambdaInstanceId, usize>>,
    LPooled<IntSet<usize>>,
    LPooled<IntMap<usize, usize>>,
) {
    let mut forward: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    let mut reverse: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    for id in graph.instances.keys().copied() {
        forward.entry(id).or_default();
        reverse.entry(id).or_default();
    }
    for edge in graph.edges.iter() {
        let Some(caller) = edge.caller else { continue };
        if graph.instances.contains_key(&caller)
            && graph.instances.contains_key(&edge.callee)
        {
            forward.entry(caller).or_default().push(edge.callee);
            reverse.entry(edge.callee).or_default().push(caller);
        }
    }
    let mut visited: LPooled<IntSet<LambdaInstanceId>> = LPooled::take();
    let mut order: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
    let mut stack: LPooled<Vec<(LambdaInstanceId, bool)>> = LPooled::take();
    for root in graph.instances.keys().copied() {
        if visited.contains(&root) {
            continue;
        }
        stack.push((root, false));
        while let Some((id, finish)) = stack.pop() {
            if finish {
                order.push(id);
            } else if visited.insert(id) {
                stack.push((id, true));
                if let Some(next) = forward.get(&id) {
                    stack.extend(next.iter().copied().map(|id| (id, false)));
                }
            }
        }
    }
    let mut components: LPooled<IntMap<LambdaInstanceId, usize>> = LPooled::take();
    let mut component = 0usize;
    let mut walk: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
    while let Some(root) = order.pop() {
        if components.contains_key(&root) {
            continue;
        }
        walk.push(root);
        while let Some(id) = walk.pop() {
            // An already-claimed node belongs to its own SCC.
            if components.contains_key(&id) {
                continue;
            }
            components.insert(id, component);
            if let Some(next) = reverse.get(&id) {
                walk.extend(next.iter().copied());
            }
        }
        component += 1;
    }
    let mut sizes: LPooled<IntMap<usize, usize>> = LPooled::take();
    for component in components.values().copied() {
        *sizes.entry(component).or_default() += 1;
    }
    let mut cyclic: LPooled<IntSet<usize>> = sizes
        .iter()
        .filter_map(|(component, size)| (*size > 1).then_some(*component))
        .collect();
    for edge in graph.edges.iter() {
        if edge.caller == Some(edge.callee)
            && let Some(component) = components.get(&edge.callee)
        {
            cyclic.insert(*component);
        }
    }
    (components, cyclic, sizes)
}

/// Run the analysis over the whole compiled program. Results land via
/// interior mutability on the nodes reached.
pub fn analyze<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    let _profile = profile::phase(Phase::Analysis);
    let graph = collect_static_graph(root, None);
    let sites = collect_resolved_sites(root);
    infer_effects(&sites, ctx);
    mark_recursion(&graph, ctx);
    // An assertion whose definition is not yet reached stays pending
    // for a later compile.
    check_def_assertions(&graph, &sites, ctx)?;
    Ok(())
}

fn assertion_error(spec: &crate::expr::Expr, msg: &str) -> anyhow::Error {
    anyhow::anyhow!("{msg}").context(crate::expr::ErrorContext(spec.clone()))
}

fn check_def_assertions<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    sites: &[(&GXLambda<R, E>, BindId)],
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    let mut pending = ctx.def_assertions.lock();
    if pending.is_empty() {
        return Ok(());
    }
    let mut covered: LPooled<IntSet<LambdaId>> = LPooled::take();
    covered.extend(sites.iter().map(|(g, _)| g.id()));
    covered.extend(graph.instances.values().map(|g| g.id()));
    let mut err: Option<anyhow::Error> = None;
    pending.retain(|a| {
        if err.is_some() || !covered.contains(&a.id) {
            return true;
        }
        let Some(d) = lambda_def(ctx, a.id) else { return true };
        let failed: Option<anyhow::Error> =
            match a.kind {
                crate::DefAssertionKind::Sync => (!d.intrinsic_effect.lock().is_sync())
                    .then(|| {
                        assertion_error(
                            &a.spec,
                            "#[sync]: this function is async — its body reaches \
                         an async builtin or an async callee",
                        )
                    }),
                crate::DefAssertionKind::Async => {
                    d.intrinsic_effect.lock().is_sync().then(|| {
                        assertion_error(
                            &a.spec,
                            "#[async]: this function is sync — nothing in its \
                         body defers an output to a later cycle",
                        )
                    })
                }
                crate::DefAssertionKind::TailRecursive => match *d.recursion.lock() {
                    RecursionKind::TailRecursive => (!lambda_is_stateless(ctx, d.id))
                        .then(|| {
                            assertion_error(
                                &a.spec,
                                "#[tail_recursive]: this function's body is stateful or \
                         async (a stateful builtin such as `count`, a `<-` to one \
                         of its own bindings, an async operation, or such a \
                         callee) — every iteration then keeps its own \
                         activation and the loop is not constant-space",
                            )
                        }),
                    RecursionKind::Recursive => Some(assertion_error(
                        &a.spec,
                        "#[tail_recursive]: this function recurses through a \
                     non-tail self-call or mutual recursion — every \
                     recursive call must be in tail position",
                    )),
                    RecursionKind::NotRecursive => Some(assertion_error(
                        &a.spec,
                        "#[tail_recursive]: this function is not recursive",
                    )),
                },
            };
        match failed {
            Some(e) => {
                err = Some(e);
                false
            }
            None => false,
        }
    });
    err.map_or(Ok(()), Err)
}

/// [`analyze`] for a callee bound at runtime (`CallSite::bind`), whose
/// body compiled after the program-wide pass. Seeded with the outer
/// `(callee, self_bind)` pair, which `collect_resolved_sites` skips.
pub(crate) fn analyze_bound_callee<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
    ctx: &ExecCtx<R, E>,
) {
    let _profile = profile::phase(Phase::Analysis);
    let graph = collect_static_graph(g.body(), Some(g));
    let mut sites = collect_resolved_sites(g.body());
    if let Some(sb) = self_bind {
        sites.push((g, sb));
    }
    infer_effects(&sites, ctx);
    mark_recursion(&graph, ctx);
}

/// Every reachable resolved-lambda call site, as `(callee, self_bind)`;
/// callee bodies are walked once each.
fn collect_resolved_sites<'a, R: Rt, E: UserEvent>(
    root: &'a Node<R, E>,
) -> LPooled<Vec<(&'a GXLambda<R, E>, BindId)>> {
    let _profile = profile::phase(Phase::ResolvedSites);
    let mut seen: LPooled<IntSet<LambdaId>> = LPooled::take();
    let mut sites: LPooled<Vec<(&'a GXLambda<R, E>, BindId)>> = LPooled::take();
    let mut stack: LPooled<Vec<&'a Node<R, E>>> = LPooled::take();
    stack.push(root);
    while let Some(node) = stack.pop() {
        let mut to_descend: LPooled<Vec<&'a Node<R, E>>> = LPooled::take();
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
        stack.extend(to_descend.drain(..));
    }
    sites
}

/// Greatest-fixpoint effect inference. Every reachable lambda starts
/// `Sync` and monotonically degrades to `Async` until stable.
fn infer_effects<R: Rt, E: UserEvent>(
    sites: &[(&GXLambda<R, E>, BindId)],
    ctx: &ExecCtx<R, E>,
) {
    let _profile = profile::phase(Phase::Effects);
    // The (callee, self_bind) pairs double as a back-edge table: a
    // dynamically-bound recursive callee can be analyzed before its
    // self-call is in `ctx.bind_to_lambda`.
    let mut bodies: LPooled<IntMap<LambdaId, &Node<R, E>>> = LPooled::take();
    let mut self_ids: LPooled<IntMap<BindId, LambdaId>> = LPooled::take();
    for (g, sb) in sites {
        bodies.entry(g.id()).or_insert_with(|| g.body());
        self_ids.entry(*sb).or_insert_with(|| g.id());
    }
    let mut eff: LPooled<IntMap<LambdaId, LambdaFacts>> =
        bodies.keys().map(|id| (*id, LambdaFacts::PURE)).collect();
    loop {
        let _profile = profile::phase(Phase::EffectRound);
        let mut changed = false;
        for (lid, body) in &*bodies {
            let e = body_facts(body, &eff, &self_ids, ctx);
            if eff.get(lid).copied() != Some(e) {
                eff.insert(*lid, e);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    for (lid, e) in &*eff {
        if let Some(d) = lambda_def(ctx, *lid) {
            *d.intrinsic_effect.lock() = e.effect;
            d.stateless.store(e.stateless, Ordering::Relaxed);
        }
    }
}

/// The two facts the fixpoint infers per lambda from an optimistic
/// start: `effect` (`Sync` degrading to `Async`) and `stateless` (no
/// per-activation state: every builtin reached is `Effect::Stateless`,
/// no `<-` targets an own binding, every callee is stateless).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LambdaFacts {
    effect: EffectKind,
    stateless: bool,
}

impl LambdaFacts {
    const PURE: Self = Self { effect: EffectKind::Sync, stateless: true };
    const ASYNC: Self = Self { effect: EffectKind::Async, stateless: false };

    fn join(self, other: Self) -> Self {
        Self {
            effect: self.effect.join(other.effect),
            stateless: self.stateless && other.stateless,
        }
    }
}

/// Fold the facts over one lambda body (nested lambda bodies excluded).
/// A `<-` counts as state only when its target is one of the body's own
/// bindings.
fn body_facts<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    eff: &IntMap<LambdaId, LambdaFacts>,
    self_ids: &IntMap<BindId, LambdaId>,
    ctx: &ExecCtx<R, E>,
) -> LambdaFacts {
    let p = profile::phase(Phase::EffectRefs);
    let mut refs = crate::Refs::default();
    body.refs(&mut refs);
    let mut local: LPooled<IntSet<BindId>> = LPooled::take();
    refs.with_bound(|id| {
        local.insert(id);
    });
    drop(p);
    let mut acc = LambdaFacts::PURE;
    fusion::for_each_node(body, &mut |n| {
        let e = node_facts(n, eff, self_ids, &local, ctx);
        if crate::dbgenv::gxdbg_effect() {
            if e.effect.is_async() {
                eprintln!("EFFECT-ASYNC-NODE node={}", n.spec());
            }
            if !e.stateless {
                eprintln!("EFFECT-STATEFUL-NODE node={}", n.spec());
            }
        }
        acc = acc.join(e);
    });
    acc
}

/// The intrinsic facts of a single node. A variable write is not
/// async: the write happens this cycle and the cross-cycle boundary is
/// the read. Exhaustive on purpose: a new node variant must decide.
fn node_facts<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    eff: &IntMap<LambdaId, LambdaFacts>,
    self_ids: &IntMap<BindId, LambdaId>,
    local: &IntSet<BindId>,
    ctx: &ExecCtx<R, E>,
) -> LambdaFacts {
    match n.view() {
        NodeView::CallSite(cs) => callee_facts(cs, eff, self_ids, ctx),
        // Cross-cycle. Catch's Async is also what keeps catch-covered
        // recursion off the tail-loop machinery (a self-call after a
        // catch is a tail leaf).
        NodeView::Sample(_)
        | NodeView::Catch(_)
        | NodeView::SeqGuard(_)
        | NodeView::Any(_)
        | NodeView::Never(_)
        | NodeView::FusedKernel(_) => LambdaFacts::ASYNC,
        NodeView::Connect(c) => {
            LambdaFacts { effect: EffectKind::Sync, stateless: !local.contains(&c.id) }
        }
        NodeView::ConnectDeref(_) => {
            LambdaFacts { effect: EffectKind::Sync, stateless: false }
        }
        NodeView::Qop(_) | NodeView::OrNever(_) => LambdaFacts::PURE,
        NodeView::Bind(_)
        | NodeView::Module(_)
        | NodeView::Block(_)
        | NodeView::MapQ(_)
        | NodeView::FoldQ(_)
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
        | NodeView::Construct(_)
        | NodeView::Array(_)
        | NodeView::ListLit(_)
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
        | NodeView::TypeDef(_)
        | NodeView::Impl(_)
        | NodeView::Nop(_) => LambdaFacts::PURE,
    }
}

/// The facts a call contributes: a known user lambda's fixpoint facts,
/// a builtin's declared `EFFECT`, otherwise `Async`.
fn callee_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    eff: &IntMap<LambdaId, LambdaFacts>,
    self_ids: &IntMap<BindId, LambdaId>,
    ctx: &ExecCtx<R, E>,
) -> LambdaFacts {
    // A lambda missing from the local map was analyzed by a prior pass:
    // read its stored fact.
    let known = |lid: LambdaId| -> LambdaFacts {
        eff.get(&lid).copied().unwrap_or_else(|| {
            lambda_def(ctx, lid)
                .map(|d| LambdaFacts {
                    effect: *d.intrinsic_effect.lock(),
                    stateless: d.stateless.load(Ordering::Relaxed),
                })
                .unwrap_or(LambdaFacts::ASYNC)
        })
    };
    if let Some(target) = cs.static_target() {
        return known(target.definition);
    }
    if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
        return known(g.id());
    }
    if let NodeView::Ref(r) = cs.fnode().view() {
        // The seeded back-edge table names the actual instance at the
        // analyzed site, so it is checked before `bind_to_lambda`.
        if let Some(lid) = self_ids.get(&r.id) {
            return known(*lid);
        }
        if let Some(v) = ctx.bind_to_lambda.get(&r.id) {
            if let Some(d) = v.downcast_ref::<LambdaDef<R, E>>() {
                return known(d.id);
            }
        }
    }
    if let ExprKind::Ref { name } = &cs.fnode().spec().kind {
        if let Some((_, bind)) =
            ctx.env.lookup_bind(&cs.scope().lexical, name).ok().flatten()
        {
            let key = (bind.scope.clone(), bind.name.clone());
            if let Some(info) = ctx.builtin_bindings.get(&key) {
                return LambdaFacts {
                    effect: ctx.builtin_effect(info.name.as_str()),
                    stateless: ctx.builtin_stateless(info.name.as_str()),
                };
            }
        }
    }
    if crate::dbgenv::gxdbg_effect() {
        eprintln!("EFFECT-ASYNC-FALLBACK cs={}", cs.fnode().spec());
    }
    LambdaFacts::ASYNC
}

fn mark_recursion<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &ExecCtx<R, E>,
) {
    let _profile = profile::phase(Phase::Recursion);
    let (components, cyclic, component_sizes) = strongly_connected(graph);
    let mut self_info: LPooled<IntMap<LambdaInstanceId, Option<BindId>>> =
        LPooled::take();
    for edge in graph.edges.iter() {
        if edge.caller == Some(edge.callee) {
            let bind = self_info.entry(edge.callee).or_insert(None);
            if bind.is_none()
                && let NodeView::Ref(r) = edge.site.fnode().view()
            {
                *bind = Some(r.id);
            }
        }
    }
    for edge in graph.edges.iter() {
        let recursive = edge.caller.is_some_and(|caller| {
            components.get(&caller) == components.get(&edge.callee)
                && components
                    .get(&caller)
                    .is_some_and(|component| cyclic.contains(component))
        });
        edge.site.set_recursive_edge(recursive);
    }
    for (instance, g) in &*graph.instances {
        g.set_tail_loop(false);
        let component = components.get(instance).copied();
        let recursive = component.is_some_and(|component| cyclic.contains(&component));
        let self_bind = self_info.get(instance).copied().flatten();
        g.set_self_recursive(self_info.contains_key(instance));
        g.set_self_bind(self_bind);
        let only_self = component
            .and_then(|component| component_sizes.get(&component).copied())
            == Some(1);
        // Tail means every self-call is in tail position; tail sites
        // loop regardless.
        let tail = only_self
            && body_has_self_tail_call(g.body(), *instance)
            && !body_has_non_tail_self_call(g.body(), *instance);
        let summary = if tail {
            RecursionKind::TailRecursive
        } else if recursive {
            RecursionKind::Recursive
        } else {
            RecursionKind::NotRecursive
        };
        if summary != RecursionKind::NotRecursive
            && let Some(d) = lambda_def(ctx, g.id())
        {
            let mut r = d.recursion.lock();
            if rank(summary) > rank(*r) {
                *r = summary;
            }
        }
        let structural = self_bind
            .is_some_and(|self_bind| lowering::structural_tail_loop(g, self_bind, ctx));
        if structural
            && lambda_is_stateless(ctx, g.id())
            && mark_tail_sites(g.body(), *instance, g.id())
        {
            g.set_tail_loop(true);
        }
    }
}

/// Mark each tail-position self-call ([`fusion::for_each_tail_leaf`]).
/// Returns whether at least one site was marked.
fn mark_tail_sites<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    instance: LambdaInstanceId,
    callee: LambdaId,
) -> bool {
    fusion::for_each_tail_leaf(
        node,
        &mut |n| match n.view() {
            NodeView::CallSite(cs) => {
                if cs.static_target().map(|target| target.instance) != Some(instance) {
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
        },
        // A select with a marked arm is on the tail spine; see
        // `Select::tail_dispatch_select`.
        &mut |s| s.tail_dispatch_select.store(true, Ordering::Relaxed),
    )
}

fn body_has_self_tail_call<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    instance: LambdaInstanceId,
) -> bool {
    fusion::for_each_tail_leaf(
        node,
        &mut |n| match n.view() {
            NodeView::CallSite(site) => {
                site.static_target().map(|target| target.instance) == Some(instance)
            }
            _ => false,
        },
        &mut |_| (),
    )
}

/// Whether any self-call site sits outside tail position.
fn body_has_non_tail_self_call<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    instance: LambdaInstanceId,
) -> bool {
    let mut tail_sites: LPooled<AHashSet<crate::ExprId>> = LPooled::take();
    fusion::for_each_tail_leaf(
        node,
        &mut |n| match n.view() {
            NodeView::CallSite(site)
                if site.static_target().map(|t| t.instance) == Some(instance) =>
            {
                tail_sites.insert(n.spec().id);
                true
            }
            _ => false,
        },
        &mut |_| (),
    );
    let mut non_tail = false;
    fusion::for_each_node(node, &mut |n| {
        if let NodeView::CallSite(site) = n.view() {
            if site.static_target().map(|t| t.instance) == Some(instance)
                && !tail_sites.contains(&n.spec().id)
            {
                non_tail = true;
            }
        }
    });
    non_tail
}

/// The call's positional argument `BindId`s in order, the tail-loop's
/// per-iteration rebind list. `None` unless the call is purely positional.
fn positional_arg_order<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
) -> Option<Box<[BindId]>> {
    let mut order: LPooled<Vec<BindId>> = LPooled::take();
    while let Some(a) = cs.args.get(&ArgKey::Positional(order.len())) {
        order.push(a.id);
    }
    if order.is_empty() || cs.args.len() != order.len() {
        return None;
    }
    Some(order.drain(..).collect())
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

fn callee_lambda<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Option<LambdaId> {
    if let Some(target) = cs.static_target() {
        return Some(target.definition);
    }
    if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
        return Some(g.id());
    }
    if let NodeView::Ref(r) = cs.fnode().view() {
        if let Some(v) = ctx.bind_to_lambda.get(&r.id) {
            if let Some(d) = v.downcast_ref::<LambdaDef<R, E>>() {
                return Some(d.id);
            }
        }
    }
    None
}

/// Whether an untaken arm must sleep: pure computation with no
/// recursive call has nothing to pause. A `<-`, a catch, a sample, an
/// `any`, a stateful/async callee, an unresolved callee or a fused
/// kernel all sleep.
pub(crate) fn arm_sleeps_on_deselect<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    node: &Node<R, E>,
) -> bool {
    let eff: IntMap<LambdaId, LambdaFacts> = IntMap::default();
    let self_ids: IntMap<BindId, LambdaId> = IntMap::default();
    let mut pure = true;
    let mut recurses = false;
    fusion::for_each_node(node, &mut |n| match n.view() {
        NodeView::Sample(_)
        | NodeView::Any(_)
        | NodeView::Never(_)
        | NodeView::Connect(_)
        | NodeView::ConnectDeref(_)
        | NodeView::Catch(_) => pure = false,
        NodeView::FusedKernel(_) => recurses = true,
        NodeView::CallSite(cs) => {
            if callee_lambda(cs, ctx)
                .and_then(|lid| lambda_def(ctx, lid))
                .is_some_and(|d| *d.recursion.lock() != RecursionKind::NotRecursive)
            {
                recurses = true;
            }
            let f = callee_facts(cs, &eff, &self_ids, ctx);
            pure &= f.effect.is_sync() && f.stateless;
        }
        _ => (),
    });
    !pure || recurses
}

/// The tail-loop collapse gate: a tail loop reuses one activation only
/// when its body is stateless; any other body gets an activation per
/// iteration.
fn lambda_is_stateless<R: Rt, E: UserEvent>(ctx: &ExecCtx<R, E>, lid: LambdaId) -> bool {
    lambda_def(ctx, lid)
        .map(|d| {
            d.intrinsic_effect.lock().is_sync() && d.stateless.load(Ordering::Relaxed)
        })
        .unwrap_or(false)
}
