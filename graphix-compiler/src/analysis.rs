//! Compile-time function-property analysis, run after `typecheck1` in
//! both fusion modes. Three passes over the reachable call graph:
//! effect inference (a greatest fixpoint from `Sync` down to `Async`),
//! the instance call graph, and recursion/tail marking (SCCs,
//! `GXLambda::tail_loop`, `CallSite::is_self_tail_call`,
//! `RecursionKind`). Both engines read the facts; the structural
//! tail-loop predicate is `fusion::lowering::structural_tail_loop`.

use crate::{
    ApplyView, BindId, DefAssertionKind, ExecCtx, LambdaId, LambdaInstanceId, Node,
    NodeView, Refs, Rt, UserEvent,
    dbgenv::gxdbg_effect,
    effects::{EffectKind, RecursionKind},
    expr::{At, ExprKind},
    fusion::{self, lowering},
    node::{
        callsite::{ArgKey, CallSite},
        lambda::{GXLambda, LambdaDef},
        select::Select,
    },
    profile::{self, Phase},
};
use anyhow::{Result, anyhow};
use nohash::{IntMap, IntSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{collections::hash_map::Entry, ptr, sync::atomic::Ordering};

struct StaticEdge<'a, R: Rt, E: UserEvent> {
    caller: Option<LambdaInstanceId>,
    callee: LambdaInstanceId,
    site: &'a CallSite<R, E>,
}

/// The instances reachable from an analysis root, the statically
/// resolved calls between them, and, per bind a resolved call names
/// its callee through, the instances it reached: the back-edge table
/// for a self-call that is not yet in `ctx.bind_to_lambda` (a
/// dynamically bound recursive callee).
struct StaticCallGraph<'a, R: Rt, E: UserEvent> {
    instances: LPooled<IntMap<LambdaInstanceId, &'a GXLambda<R, E>>>,
    edges: LPooled<Vec<StaticEdge<'a, R, E>>>,
    self_binds: LPooled<IntMap<BindId, SmallVec<[LambdaInstanceId; 2]>>>,
}

impl<'a, R: Rt, E: UserEvent> StaticCallGraph<'a, R, E> {
    fn add_self_bind(&mut self, bind: BindId, instance: LambdaInstanceId) {
        let ids = self.self_binds.entry(bind).or_default();
        if !ids.contains(&instance) {
            ids.push(instance)
        }
    }
}

/// Walk `root` (or `seed`'s body) and every resolved callee's instance
/// body once.
fn collect_static_graph<'a, R: Rt, E: UserEvent>(
    root: &'a Node<R, E>,
    seed: Option<&'a GXLambda<R, E>>,
) -> StaticCallGraph<'a, R, E> {
    let _profile = profile::phase(Phase::CallGraph);
    let mut graph = StaticCallGraph {
        instances: LPooled::take(),
        edges: LPooled::take(),
        self_binds: LPooled::take(),
    };
    let mut stack: LPooled<Vec<(&'a Node<R, E>, Option<LambdaInstanceId>)>> =
        LPooled::take();
    match seed {
        Some(g) => {
            graph.instances.insert(g.instance_id(), g);
            stack.push((g.body(), Some(g.instance_id())));
        }
        None => stack.push((root, None)),
    }
    while let Some((node, caller)) = stack.pop() {
        fusion::for_each_node(node, &mut |n| {
            let NodeView::CallSite(site) = n.view() else { return };
            if let Some(target) = site.static_target() {
                graph.edges.push(StaticEdge { caller, callee: target.instance, site });
            }
            let Some(ApplyView::Lambda(g)) = site.resolved_apply() else {
                return;
            };
            let instance = g.instance_id();
            if let NodeView::Ref(r) = site.fnode().view() {
                graph.add_self_bind(r.id, instance);
            }
            if let Entry::Vacant(e) = graph.instances.entry(instance) {
                e.insert(g);
                stack.push((g.body(), Some(instance)));
            }
        });
    }
    graph
}

#[derive(Clone, Copy, Default)]
struct Component {
    size: usize,
    cyclic: bool,
}

/// The strongly connected components of a call graph; component ids
/// are dense.
struct Components {
    of: LPooled<IntMap<LambdaInstanceId, usize>>,
    components: LPooled<Vec<Component>>,
}

impl Components {
    fn get(&self, id: &LambdaInstanceId) -> Option<(usize, Component)> {
        self.of.get(id).map(|c| (*c, self.components[*c]))
    }
}

fn strongly_connected<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
) -> Components {
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
    let mut res = Components { of: LPooled::take(), components: LPooled::take() };
    let mut walk: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
    while let Some(root) = order.pop() {
        if res.of.contains_key(&root) {
            continue;
        }
        let component = res.components.len();
        let mut size = 0;
        walk.push(root);
        while let Some(id) = walk.pop() {
            // An already-claimed node belongs to its own SCC.
            if res.of.contains_key(&id) {
                continue;
            }
            res.of.insert(id, component);
            size += 1;
            if let Some(next) = reverse.get(&id) {
                walk.extend(next.iter().copied());
            }
        }
        res.components.push(Component { size, cyclic: size > 1 });
    }
    for edge in graph.edges.iter() {
        if edge.caller == Some(edge.callee)
            && let Some(component) = res.of.get(&edge.callee)
        {
            res.components[*component].cyclic = true;
        }
    }
    res
}

/// Run the analysis over the whole compiled program. Results land via
/// interior mutability on the nodes reached.
pub fn analyze<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    let _profile = profile::phase(Phase::Analysis);
    let graph = collect_static_graph(root, None);
    let facts = infer_effects(&graph, ctx);
    mark_recursion(&graph, &facts, ctx);
    // An assertion whose definition is not yet reached stays pending
    // for a later compile or a runtime bind.
    check_def_assertions(&graph, ctx)
}

/// [`analyze`] for a callee bound at runtime (`CallSite::bind`), whose
/// body compiled after the program-wide pass, seeded with the outer
/// `(callee, self_bind)` pair. A definition assertion first reached
/// here is checked here; the program already runs, so a failure is
/// reported as an unhandled error is.
pub(crate) fn analyze_bound_callee<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
    ctx: &ExecCtx<R, E>,
) {
    let _profile = profile::phase(Phase::Analysis);
    let mut graph = collect_static_graph(g.body(), Some(g));
    if let Some(sb) = self_bind {
        graph.add_self_bind(sb, g.instance_id());
    }
    let facts = infer_effects(&graph, ctx);
    mark_recursion(&graph, &facts, ctx);
    if let Err(e) = check_def_assertions(&graph, ctx) {
        log::error!("{e:#}");
        eprintln!("{e:#}");
    }
}

/// Check and retire every pending assertion whose definition this
/// analysis reached; drop those whose definition is gone. Stops at the
/// first failure.
fn check_def_assertions<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    let mut pending = ctx.def_assertions.lock();
    if pending.is_empty() {
        return Ok(());
    }
    let covered: LPooled<IntSet<LambdaId>> =
        graph.instances.values().map(|g| g.id()).collect();
    let mut i = 0;
    while i < pending.len() {
        let Some(d) = lambda_def(ctx, pending[i].id) else {
            pending.remove(i);
            continue;
        };
        if !covered.contains(&d.id) {
            i += 1;
            continue;
        }
        let a = pending.remove(i);
        if let Some(msg) = assertion_failure(a.kind, d) {
            return Err(anyhow!("{msg}").at(&a.spec));
        }
    }
    Ok(())
}

fn assertion_failure<R: Rt, E: UserEvent>(
    kind: DefAssertionKind,
    d: &LambdaDef<R, E>,
) -> Option<&'static str> {
    let effect = *d.intrinsic_effect.lock();
    match kind {
        DefAssertionKind::Sync => effect.is_async().then_some(
            "#[sync]: this function is async — its body reaches an async builtin \
             or an async callee",
        ),
        DefAssertionKind::Async => effect.is_sync().then_some(
            "#[async]: this function is sync — nothing in its body defers an \
             output to a later cycle",
        ),
        DefAssertionKind::TailRecursive => match *d.recursion.lock() {
            RecursionKind::TailRecursive => None,
            RecursionKind::TailCalls if !def_facts(d).is_pure() => Some(
                "#[tail_recursive]: this function's body is stateful or async (a \
                 stateful builtin such as `count`, a `<-` to one of its own \
                 bindings, an async operation, or such a callee) — every \
                 iteration then keeps its own activation and the loop is not \
                 constant-space",
            ),
            RecursionKind::TailCalls => Some(
                "#[tail_recursive]: every recursive call is in tail position, but \
                 no loop is built — the loop rebinds positional formals of types \
                 a fused kernel can carry, so with a labeled, variadic or opaque \
                 formal every call keeps its own activation",
            ),
            RecursionKind::Recursive => Some(
                "#[tail_recursive]: this function recurses through a non-tail \
                 self-call or mutual recursion — every recursive call must be in \
                 tail position",
            ),
            RecursionKind::NotRecursive => {
                Some("#[tail_recursive]: this function is not recursive")
            }
        },
    }
}

/// The facts inferred for the instances of this analysis, by instance.
type InstanceFacts = LPooled<IntMap<LambdaInstanceId, LambdaFacts>>;

/// One instance body reduced to what its facts depend on: the join of
/// everything already known, and the instances of this analysis whose
/// facts it reads.
struct BodyFacts {
    lambda: LambdaId,
    known: LambdaFacts,
    callees: SmallVec<[LambdaInstanceId; 4]>,
}

/// Greatest-fixpoint effect inference over every reachable instance.
/// An instance starts `Sync` and monotonically degrades to `Async`
/// until stable; a definition's stored facts are the join over its
/// instances and never improve (an instance analyzed later is one more
/// instance, not a better view of the definition).
fn infer_effects<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &ExecCtx<R, E>,
) -> InstanceFacts {
    let _profile = profile::phase(Phase::Effects);
    let mut bodies: LPooled<IntMap<LambdaInstanceId, BodyFacts>> = LPooled::take();
    let mut callers: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    for (iid, g) in graph.instances.iter() {
        let b = body_facts(g, graph, ctx);
        for callee in b.callees.iter() {
            callers.entry(*callee).or_default().push(*iid);
        }
        bodies.insert(*iid, b);
    }
    let mut eff: InstanceFacts =
        bodies.keys().map(|id| (*id, LambdaFacts::PURE)).collect();
    let mut work: LPooled<Vec<LambdaInstanceId>> = bodies.keys().copied().collect();
    while let Some(iid) = work.pop() {
        let _profile = profile::phase(Phase::EffectRound);
        let b = &bodies[&iid];
        let e = b.callees.iter().fold(b.known, |acc, c| acc.join(eff[c]));
        if eff[&iid] != e {
            eff.insert(iid, e);
            if let Some(callers) = callers.get(&iid) {
                work.extend(callers.iter().copied());
            }
        }
    }
    for (iid, b) in bodies.iter() {
        if let Some(d) = lambda_def(ctx, b.lambda) {
            let e = def_facts(d).join(eff[iid]);
            *d.intrinsic_effect.lock() = e.effect;
            d.stateless.store(e.stateless, Ordering::Relaxed);
        }
    }
    eff
}

fn def_facts<R: Rt, E: UserEvent>(d: &LambdaDef<R, E>) -> LambdaFacts {
    LambdaFacts {
        effect: *d.intrinsic_effect.lock(),
        stateless: d.stateless.load(Ordering::Relaxed),
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
    const STATEFUL: Self = Self { effect: EffectKind::Sync, stateless: false };

    fn join(self, other: Self) -> Self {
        Self {
            effect: self.effect.join(other.effect),
            stateless: self.stateless && other.stateless,
        }
    }

    /// Sync and stateless: what a tail loop's single activation and a
    /// sleep-free arm need.
    fn is_pure(self) -> bool {
        self.effect.is_sync() && self.stateless
    }
}

/// Reduce one instance body (nested lambda bodies excluded) to its
/// [`BodyFacts`]. A `<-` counts as state only when its target is one
/// of the body's own bindings.
fn body_facts<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &ExecCtx<R, E>,
) -> BodyFacts {
    let body = g.body();
    let local: LPooled<IntSet<BindId>> = {
        let _profile = profile::phase(Phase::EffectRefs);
        let mut refs = Refs::default();
        body.refs(&mut refs);
        let mut local: LPooled<IntSet<BindId>> = LPooled::take();
        refs.with_bound(|id| {
            local.insert(id);
        });
        local
    };
    let mut res =
        BodyFacts { lambda: g.id(), known: LambdaFacts::PURE, callees: SmallVec::new() };
    fusion::for_each_node(body, &mut |n| {
        let e = node_facts(n, OwnTargets::Bound(&local), &mut |cs| {
            callee_facts(cs, Some(graph), ctx, &mut |iid| res.callees.push(iid))
        });
        if gxdbg_effect() {
            if e.effect.is_async() {
                eprintln!("EFFECT-ASYNC-NODE node={}", n.spec());
            }
            if !e.stateless {
                eprintln!("EFFECT-STATEFUL-NODE node={}", n.spec());
            }
        }
        res.known = res.known.join(e);
    });
    res
}

/// Which `<-` targets count as state of the code being classified.
#[derive(Clone, Copy)]
enum OwnTargets<'a> {
    /// A lambda body's own bindings.
    Bound(&'a IntSet<BindId>),
    /// Every target: a select arm's writes are its own state.
    All,
}

/// The intrinsic facts of a single node; a call's come from `call`. A
/// variable write is not async: the write happens this cycle and the
/// cross-cycle boundary is the read. Exhaustive on purpose: a new node
/// variant must decide.
fn node_facts<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    own: OwnTargets,
    call: &mut dyn FnMut(&CallSite<R, E>) -> LambdaFacts,
) -> LambdaFacts {
    match n.view() {
        NodeView::CallSite(cs) => call(cs),
        NodeView::Sample(_)
        | NodeView::Catch(_)
        | NodeView::SeqGuard(_)
        | NodeView::SeqAbort(_)
        | NodeView::Any(_)
        | NodeView::Never(_)
        | NodeView::FusedKernel(_) => LambdaFacts::ASYNC,
        NodeView::Connect(c) => match own {
            OwnTargets::Bound(local) if !local.contains(&c.id) => LambdaFacts::PURE,
            OwnTargets::Bound(_) | OwnTargets::All => LambdaFacts::STATEFUL,
        },
        NodeView::ConnectDeref(_) => LambdaFacts::STATEFUL,
        NodeView::Qop(_) | NodeView::OrNever(_) => LambdaFacts::PURE,
        // CR claude for eric: [bug] A dynamic Module is PURE here and
        // fusion::for_each_node never visits its `source`, so an async source is
        // invisible. Probe: `#[sync] let f = |x: i64| { let s = mod t dynamic {
        // sandbox whitelist [core]; sig { val foo: i64 }; source
        // sys::time::after_idle(duration:0.01s, "let foo = 42") }; (s, x) }` is
        // accepted; the same after_idle outside the module is refused. The same
        // hole makes arm_sleeps_on_deselect call such an arm pure.
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

/// The facts a call contributes: a callee instance of `graph` is still
/// being inferred, so it goes to `pending` and contributes nothing here;
/// an instance outside the analysis contributes its definition's stored
/// facts, a builtin its declared `EFFECT`, anything else `Async`.
fn callee_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    graph: Option<&StaticCallGraph<'_, R, E>>,
    ctx: &ExecCtx<R, E>,
    pending: &mut dyn FnMut(LambdaInstanceId),
) -> LambdaFacts {
    let of_def = |lid: LambdaId| -> LambdaFacts {
        lambda_def(ctx, lid).map(def_facts).unwrap_or(LambdaFacts::ASYNC)
    };
    let mut instance = |iid: LambdaInstanceId, lid: LambdaId| match graph {
        Some(graph) if graph.instances.contains_key(&iid) => {
            pending(iid);
            LambdaFacts::PURE
        }
        _ => of_def(lid),
    };
    if let Some(target) = cs.static_target() {
        return instance(target.instance, target.definition);
    }
    if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
        return instance(g.instance_id(), g.id());
    }
    if let NodeView::Ref(r) = cs.fnode().view() {
        if let Some(ids) = graph.and_then(|graph| graph.self_binds.get(&r.id)) {
            ids.iter().for_each(|iid| pending(*iid));
            return LambdaFacts::PURE;
        }
        if let Some(d) = ctx
            .bind_to_lambda
            .get(&r.id)
            .and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
        {
            return of_def(d.id);
        }
    }
    if let ExprKind::Ref { name } = &cs.fnode().spec().kind
        && let Some((_, bind)) =
            ctx.env.lookup_bind(&cs.scope().lexical, name).ok().flatten()
        && let Some(info) =
            ctx.builtin_bindings.get(&(bind.scope.clone(), bind.name.clone()))
    {
        let effect = ctx.builtin_effect(info.name.as_str());
        return LambdaFacts { effect: effect.kind(), stateless: effect.is_stateless() };
    }
    if gxdbg_effect() {
        eprintln!("EFFECT-ASYNC-FALLBACK cs={}", cs.fnode().spec());
    }
    LambdaFacts::ASYNC
}

fn mark_recursion<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    facts: &InstanceFacts,
    ctx: &ExecCtx<R, E>,
) {
    let _profile = profile::phase(Phase::Recursion);
    let components = strongly_connected(graph);
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
        let recursive = edge.caller.and_then(|c| components.get(&c)).is_some_and(
            |(caller, component)| {
                component.cyclic
                    && components.get(&edge.callee).map(|(c, _)| c) == Some(caller)
            },
        );
        edge.site.set_recursive_edge(recursive);
    }
    for (instance, g) in graph.instances.iter() {
        let component = components.get(instance).map(|(_, c)| c).unwrap_or_default();
        let self_bind = self_info.get(instance).copied().flatten();
        g.set_self_recursive(self_info.contains_key(instance));
        g.set_self_bind(self_bind);
        let tail_calls = TailSelfCalls::collect(g.body(), *instance);
        let tail = component.size == 1
            && !tail_calls.sites.is_empty()
            && !tail_calls.misses_one(g.body(), *instance);
        let pure = facts
            .get(instance)
            .copied()
            .unwrap_or_else(|| {
                lambda_def(ctx, g.id()).map_or(LambdaFacts::ASYNC, def_facts)
            })
            .is_pure();
        let looped =
            pure && self_bind.is_some_and(|self_bind| {
                lowering::structural_tail_loop(g, self_bind, ctx)
            }) && tail_calls.mark();
        g.set_tail_loop(looped);
        let summary = match (tail, looped) {
            (true, true) => RecursionKind::TailRecursive,
            (true, false) => RecursionKind::TailCalls,
            (false, _) if component.cyclic => RecursionKind::Recursive,
            (false, _) => RecursionKind::NotRecursive,
        };
        if let Some(d) = lambda_def(ctx, g.id()) {
            let mut r = d.recursion.lock();
            *r = (*r).max(summary);
        }
    }
}

/// An instance's self-calls in tail position ([`fusion::for_each_tail_leaf`])
/// and the selects on the tail spine above them.
struct TailSelfCalls<'a, R: Rt, E: UserEvent> {
    sites: SmallVec<[&'a CallSite<R, E>; 4]>,
    spine: SmallVec<[&'a Select<R, E>; 4]>,
}

impl<'a, R: Rt, E: UserEvent> TailSelfCalls<'a, R, E> {
    fn collect(body: &'a Node<R, E>, instance: LambdaInstanceId) -> Self {
        let mut res = Self { sites: SmallVec::new(), spine: SmallVec::new() };
        fusion::for_each_tail_leaf(
            body,
            &mut |n| match n.view() {
                NodeView::CallSite(cs) if is_call_to(cs, instance) => {
                    res.sites.push(cs);
                    true
                }
                _ => false,
            },
            &mut |s| res.spine.push(s),
        );
        res
    }

    /// Whether a self-call sits outside tail position.
    fn misses_one(&self, body: &Node<R, E>, instance: LambdaInstanceId) -> bool {
        let mut missed = false;
        fusion::for_each_node(body, &mut |n| {
            if let NodeView::CallSite(cs) = n.view()
                && is_call_to(cs, instance)
                && !self.sites.iter().any(|s| ptr::eq(*s, cs))
            {
                missed = true;
            }
        });
        missed
    }

    /// Mark every site as a loop iteration and every spine select as a
    /// tail dispatch (`Select::tail_dispatch_select`). Refused, marking
    /// nothing, unless every site is purely positional.
    fn mark(&self) -> bool {
        let orders: Option<SmallVec<[Box<[BindId]>; 4]>> =
            self.sites.iter().map(|cs| positional_arg_order(cs)).collect();
        let Some(orders) = orders.filter(|o| !o.is_empty()) else { return false };
        for (cs, order) in self.sites.iter().zip(orders) {
            cs.mark_self_tail_call(order);
        }
        for s in self.spine.iter() {
            s.tail_dispatch_select.store(true, Ordering::Relaxed);
        }
        true
    }
}

fn is_call_to<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    instance: LambdaInstanceId,
) -> bool {
    cs.static_target().map(|target| target.instance) == Some(instance)
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
    if let NodeView::Ref(r) = cs.fnode().view()
        && let Some(d) = ctx
            .bind_to_lambda
            .get(&r.id)
            .and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
    {
        return Some(d.id);
    }
    None
}

/// Whether an untaken arm must sleep: pure computation with no
/// recursive call has nothing to pause. Anything [`node_facts`] calls
/// stateful or async sleeps, every `<-` in the arm included.
pub(crate) fn arm_sleeps_on_deselect<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    node: &Node<R, E>,
) -> bool {
    let mut pure = true;
    let mut recurses = false;
    fusion::for_each_node(node, &mut |n| {
        let facts = node_facts(n, OwnTargets::All, &mut |cs| {
            recurses |= callee_lambda(cs, ctx)
                .and_then(|lid| lambda_def(ctx, lid))
                .is_some_and(|d| *d.recursion.lock() != RecursionKind::NotRecursive);
            callee_facts(cs, None, ctx, &mut |_| ())
        });
        pure &= facts.is_pure();
    });
    !pure || recurses
}
