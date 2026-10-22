//! Compile-time analysis, run after `typecheck1` in both fusion modes,
//! over the reachable call graph: effect inference (a greatest fixpoint
//! from `Sync` down to `Async`), recursion/tail marking (SCCs,
//! `GXLambda::tail_loop`, `RecursionKind`), the definition assertions,
//! and the dependency summaries that plan seq steps and block runs. Also
//! the `#[parallel]` check and the arm-sleep rule. Both engines read the
//! facts; the structural tail-loop predicate is
//! `fusion::lowering::tail_loop_blocker`.

use crate::{
    ApplyView, BindId, CompileCtx, DefAssertion, DefAssertionKind, LambdaId,
    LambdaInstanceId, Node, NodeView, Refs, Rt, Update, UserEvent,
    cost::{ForkSite, Siblings},
    dbgenv::{gxdbg_effect, gxdbg_seqplan},
    effects::{LambdaFacts, NoLoop, RecursionKind},
    env::Env,
    expr::{At, Expr, ExprKind, ModuleKind},
    fusion::{self, lowering},
    node::{
        Runs,
        callsite::{ArgKey, CallSite},
        fork_control::{ForkControl, ForkKind},
        lambda::{GXLambda, LambdaDef, ParamCalls, SelfCall},
        module::Module,
        seq_machine::{SeqCapture, SeqMachine},
    },
    profile::{self, Phase},
    typ::{FnType, Type},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow};
use bytes::{Buf, BufMut};
use compact_str::{CompactString, format_compact};
use netidx_core::pack::{Pack, PackError};
use nohash::{IntMap, IntSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{cell::OnceCell, collections::hash_map::Entry, ptr, sync::atomic::Ordering};

struct StaticEdge<'a, R: Rt, E: UserEvent> {
    caller: Option<LambdaInstanceId>,
    callee: LambdaInstanceId,
    site: &'a CallSite<R, E>,
}

/// The instance a node belongs to; `None` outside every instance.
type Holder = Option<LambdaInstanceId>;

/// The instances reachable from an analysis root, the statically
/// resolved calls between them, and, per bind a resolved call names
/// its callee through, the instances it reached: the back-edge table
/// for a self-call that is not yet in `ctx.bind_to_lambda` (a
/// dynamically bound recursive callee). Also the seq machines met, and
/// the `seqq` captures by the id of their machine and the instance that
/// holds them (every instance of a definition shares the machine's id),
/// with the bind each is the value of.
struct StaticCallGraph<'a, R: Rt, E: UserEvent> {
    instances: LPooled<IntMap<LambdaInstanceId, &'a GXLambda<R, E>>>,
    edges: LPooled<Vec<StaticEdge<'a, R, E>>>,
    self_binds: LPooled<IntMap<BindId, SmallVec<[LambdaInstanceId; 2]>>>,
    machines: LPooled<Vec<(&'a SeqMachine<R, E>, Holder)>>,
    /// Statement lists of two or more statements (a block's, a module's)
    /// with their runs to plan.
    blocks: LPooled<Vec<(&'a [Node<R, E>], &'a [usize], &'a Runs)>>,
    /// The `#[parallel]` sites.
    parallels: LPooled<Vec<&'a ForkControl<R, E>>>,
    /// Fork points other than blocks whose siblings nothing decided yet.
    forks: LPooled<Vec<(&'a Siblings, ForkKids<'a, R, E>)>>,
    captures: LPooled<
        AHashMap<
            (crate::expr::ExprId, Holder),
            SmallVec<[(BindId, &'a SeqCapture<R, E>); 4]>,
        >,
    >,
}

impl<'a, R: Rt, E: UserEvent> StaticCallGraph<'a, R, E> {
    fn new() -> Self {
        StaticCallGraph {
            instances: LPooled::take(),
            edges: LPooled::take(),
            self_binds: LPooled::take(),
            machines: LPooled::take(),
            blocks: LPooled::take(),
            parallels: LPooled::take(),
            forks: LPooled::take(),
            captures: LPooled::take(),
        }
    }

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
    let mut graph = StaticCallGraph::new();
    let mut stack: LPooled<Vec<(&'a Node<R, E>, Option<LambdaInstanceId>)>> =
        LPooled::take();
    match seed {
        Some(g) => {
            graph.instances.insert(g.instance_id(), g);
            stack.push((g.body(), Some(g.instance_id())));
        }
        None => stack.push((root, None)),
    }
    walk_static_graph(&mut graph, stack);
    graph
}

/// [`collect_static_graph`] from several roots.
fn collect_static_graph_of<'a, R: Rt, E: UserEvent>(
    roots: impl IntoIterator<Item = &'a Node<R, E>>,
) -> StaticCallGraph<'a, R, E> {
    let mut graph = StaticCallGraph::new();
    walk_static_graph(&mut graph, roots.into_iter().map(|r| (r, None)).collect());
    graph
}

fn walk_static_graph<'a, R: Rt, E: UserEvent>(
    graph: &mut StaticCallGraph<'a, R, E>,
    mut stack: LPooled<Vec<(&'a Node<R, E>, Option<LambdaInstanceId>)>>,
) {
    while let Some((node, caller)) = stack.pop() {
        fusion::for_each_node(node, &mut |n| {
            if let Some((siblings, kids)) = fork_point(n)
                && !siblings.decided()
            {
                graph.forks.push((siblings, kids))
            }
            let site = match n.view() {
                NodeView::CallSite(site) => site,
                NodeView::SeqMachine(m) => return graph.machines.push((m, caller)),
                NodeView::Block(b) if b.children.len() > 1 => {
                    return graph.blocks.push((&b.children, &b.catches, &b.runs));
                }
                NodeView::Module(m) if m.nodes.len() > 1 => {
                    return graph.blocks.push((&m.nodes, &m.catches, &m.runs));
                }
                NodeView::ForkControl(f) if matches!(f.kind, ForkKind::Parallel(_)) => {
                    return graph.parallels.push(f);
                }
                NodeView::Bind(b) => {
                    if let NodeView::SeqCapture(c) = b.node.view()
                        && let Some(id) = b.pattern.single_bind_id()
                    {
                        graph
                            .captures
                            .entry((c.machine, caller))
                            .or_default()
                            .push((id, c))
                    }
                    return;
                }
                _ => return,
            };
            if let Some(target) = site.static_target() {
                graph.edges.push(StaticEdge { caller, callee: target.instance, site });
            }
            let Some(ApplyView::Lambda(g)) = site.resolved_apply() else {
                return;
            };
            let instance = g.instance_id();
            // a self-call binds an instance per activation; any other call
            // with no static target is opaque
            if site.static_target().is_some()
                && let NodeView::Ref(r) = site.fnode().view()
            {
                graph.add_self_bind(r.id, instance);
            }
            if let Entry::Vacant(e) = graph.instances.entry(instance) {
                e.insert(g);
                stack.push((g.body(), Some(instance)));
            }
        });
    }
}

#[derive(Clone, Copy, Default)]
struct Component {
    start: usize,
    size: usize,
    cyclic: bool,
}

/// The strongly connected components of a call graph, callers before
/// callees; component ids are dense.
struct Components {
    of: LPooled<IntMap<LambdaInstanceId, usize>>,
    components: LPooled<Vec<Component>>,
    /// Each component's instances, contiguous.
    members: LPooled<Vec<LambdaInstanceId>>,
}

impl Components {
    fn get(&self, id: &LambdaInstanceId) -> Option<(usize, Component)> {
        self.of.get(id).map(|c| (*c, self.components[*c]))
    }

    fn members(&self, c: &Component) -> &[LambdaInstanceId] {
        &self.members[c.start..c.start + c.size]
    }
}

fn strongly_connected<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
) -> Components {
    let mut forward: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    for id in graph.instances.keys().copied() {
        forward.entry(id).or_default();
    }
    for edge in graph.edges.iter() {
        let Some(caller) = edge.caller else { continue };
        if graph.instances.contains_key(&caller)
            && graph.instances.contains_key(&edge.callee)
        {
            forward.entry(caller).or_default().push(edge.callee);
        }
    }
    components(&forward)
}

/// The strongly connected components of `forward`, whose keys are every
/// node and whose lists name only keys.
fn components(
    forward: &IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>,
) -> Components {
    let mut reverse: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    for (caller, callees) in forward.iter() {
        reverse.entry(*caller).or_default();
        for callee in callees.iter() {
            reverse.entry(*callee).or_default().push(*caller);
        }
    }
    let mut visited: LPooled<IntSet<LambdaInstanceId>> = LPooled::take();
    let mut order: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
    let mut stack: LPooled<Vec<(LambdaInstanceId, bool)>> = LPooled::take();
    for root in forward.keys().copied() {
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
    let mut res = Components {
        of: LPooled::take(),
        components: LPooled::take(),
        members: LPooled::take(),
    };
    let mut walk: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
    while let Some(root) = order.pop() {
        if res.of.contains_key(&root) {
            continue;
        }
        let component = res.components.len();
        let start = res.members.len();
        walk.push(root);
        while let Some(id) = walk.pop() {
            // An already-claimed node belongs to its own SCC.
            if res.of.contains_key(&id) {
                continue;
            }
            res.of.insert(id, component);
            res.members.push(id);
            if let Some(next) = reverse.get(&id) {
                walk.extend(next.iter().copied());
            }
        }
        let size = res.members.len() - start;
        res.components.push(Component { start, size, cyclic: size > 1 });
    }
    for (caller, callees) in forward.iter() {
        if callees.contains(caller) {
            res.components[res.of[caller]].cyclic = true;
        }
    }
    res
}

/// The `#[tail_recursive]` assertions an analysis passed: each holds
/// once fusion has built its loop ([`check_tail_loops`]).
#[derive(Default)]
pub struct TailLoops(LPooled<Vec<DefAssertion>>);

/// Run the analysis over the whole compiled program. Results land via
/// interior mutability on the nodes reached.
pub fn analyze<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> Result<TailLoops> {
    let _profile = profile::phase(Phase::Analysis);
    let graph = collect_static_graph(root, None);
    let facts = infer_effects(&graph, ctx);
    mark_recursion(&graph, &facts, ctx);
    plan_machines(&graph, ctx);
    plan_forks(&graph, ctx);
    for f in graph.parallels.iter() {
        check_parallel(&f.spec, &f.n, ctx)?
    }
    // An assertion whose definition is not yet reached stays pending
    // for a later compile or a runtime bind.
    let mut due = TailLoops::default();
    check_def_assertions(&graph, ctx, &mut due)?;
    Ok(due)
}

/// Refuse each passed `#[tail_recursive]` whose loop fusion did not
/// build: only a fused kernel runs a tail recursion as a loop.
pub fn check_tail_loops<R: Rt, E: UserEvent>(
    due: TailLoops,
    ctx: &CompileCtx<R, E>,
) -> Result<()> {
    match due.0.iter().find(|a| !ctx.fusion.has_tail_loop(a.id)) {
        None => Ok(()),
        Some(a) => Err(anyhow!(
            "#[tail_recursive]: no loop is built — the body did not fuse, and \
             only a fused kernel runs a tail recursion as a loop \
             (GXDBG_NATIVE_ALL=1 lists the fusion failures)"
        )
        .at(&a.spec)),
    }
}

/// [`analyze`] for a callee bound at runtime (`CallSite::bind`), whose
/// body compiled after the program-wide pass, seeded with the outer
/// `(callee, self_bind)` pair. A definition assertion first reached
/// here is checked here; the program already runs, so a failure is
/// reported as an unhandled error is.
pub(crate) fn analyze_bound_callee<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
    ctx: &CompileCtx<R, E>,
) {
    let _profile = profile::phase(Phase::Analysis);
    let mut graph = collect_static_graph(g.body(), Some(g));
    if let Some(sb) = self_bind {
        graph.add_self_bind(sb, g.instance_id());
    }
    let facts = infer_effects(&graph, ctx);
    mark_recursion(&graph, &facts, ctx);
    plan_machines(&graph, ctx);
    plan_forks(&graph, ctx);
    let parallels =
        graph.parallels.iter().try_for_each(|f| check_parallel(&f.spec, &f.n, ctx));
    let mut due = TailLoops::default();
    if let Err(e) = parallels.and_then(|()| check_def_assertions(&graph, ctx, &mut due)) {
        crate::node::error::report_failure!(&compact_str::format_compact!("{e:#}"));
    }
}

/// Check every pending assertion whose definition this analysis
/// reached, retiring all but a `#[sync]`, a passed `#[tail_recursive]`
/// into `due`; drop those whose definition is gone. Stops at the first
/// failure.
fn check_def_assertions<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
    due: &mut TailLoops,
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
        if let Some(msg) = assertion_failure(pending[i].kind, d) {
            let a = pending.remove(i);
            return Err(anyhow!("{msg}").at(&a.spec));
        }
        // an instance a later analysis reaches can still make the
        // function async; the other facts are the definition's alone
        match pending[i].kind {
            DefAssertionKind::Sync => i += 1,
            DefAssertionKind::TailRecursive => due.0.push(pending.remove(i)),
            DefAssertionKind::Async => drop(pending.remove(i)),
        }
    }
    Ok(())
}

/// The heads of the refusals only a build makes: a definition
/// assertion (`assertion_failure`), `#[parallel]` (`check_parallel`) or
/// `#[native]`, which `--check` (`CFlag::CheckOnly`) leaves unverified.
const BUILD_ONLY_REFUSALS: [&str; 5] = [
    "#[parallel] has nothing to run in parallel",
    "#[sync]:",
    "#[async]:",
    "#[tail_recursive]:",
    "#[native] expression did not fully fuse",
];

/// Whether `msg`, a compile error's text with its context chain, holds a
/// refusal only a build makes.
pub fn build_only_refusal(msg: &str) -> bool {
    BUILD_ONLY_REFUSALS.iter().any(|h| msg.contains(h))
}

fn assertion_failure<R: Rt, E: UserEvent>(
    kind: DefAssertionKind,
    d: &LambdaDef<R, E>,
) -> Option<CompactString> {
    let effect = d.facts.lock().effect;
    let said = |s: &str| Some(CompactString::from(s));
    match kind {
        DefAssertionKind::Sync => effect.is_async().then(|| {
            CompactString::from(
                "#[sync]: this function is async — its body reaches an async builtin \
                 or an async callee",
            )
        }),
        DefAssertionKind::Async => effect.is_sync().then(|| {
            CompactString::from(
                "#[async]: this function is sync — nothing in its body defers an \
                 output to a later cycle",
            )
        }),
        DefAssertionKind::TailRecursive => match *d.recursion.lock() {
            RecursionKind::TailRecursive => None,
            RecursionKind::TailCalls if !def_facts(d).is_pure() => said(
                "#[tail_recursive]: this function's body is stateful or async (a \
                 stateful builtin such as `count`, a `<-` to one of its own \
                 bindings, an async operation, or such a callee) — every \
                 iteration then keeps its own activation and the loop is not \
                 constant-space",
            ),
            RecursionKind::TailCalls => Some(no_loop_message(d.no_loop.lock().as_ref())),
            RecursionKind::Recursive => said(
                "#[tail_recursive]: this function recurses through a non-tail \
                 self-call or mutual recursion — every recursive call must be in \
                 tail position",
            ),
            RecursionKind::NotRecursive => {
                said("#[tail_recursive]: this function is not recursive")
            }
        },
    }
}

/// Why every recursive call is in tail position and still no loop is built.
fn no_loop_message(why: Option<&NoLoop>) -> CompactString {
    const HEAD: &str = "#[tail_recursive]: every recursive call is in tail position, but \
                        no loop is built";
    let site = |e: &Expr| {
        format_compact!("`{e}` (line {}, column {})", e.pos.line, e.pos.column)
    };
    match why {
        Some(NoLoop::Io(e)) => format_compact!(
            "{HEAD}: {} performs Io that does not fire with the formals, so a \
             fresh activation performs it at its birth where a loop pass would not",
            site(e)
        ),
        Some(NoLoop::Raise(e)) => format_compact!(
            "{HEAD}: {} raises to a handler on something that does not fire with \
             the formals, which a fresh activation raises at its birth and a loop \
             pass would not",
            site(e)
        ),
        Some(NoLoop::Formals) => format_compact!(
            "{HEAD}: a labeled, variadic or opaque formal, or one of a type a fused \
             kernel cannot carry, is one the loop cannot rebind"
        ),
        Some(NoLoop::Memory) | None => format_compact!(
            "{HEAD}: the body holds per-call memory (a collection operation, a \
             non-tail self-call, or a callee holding one), which every call keeps \
             in its own activation"
        ),
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
    ctx: &CompileCtx<R, E>,
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
            d.join_facts(eff[iid]);
        }
    }
    eff
}

fn def_facts<R: Rt, E: UserEvent>(d: &LambdaDef<R, E>) -> LambdaFacts {
    *d.facts.lock()
}

/// Reduce one instance body (nested lambda bodies excluded) to its
/// [`BodyFacts`]. A `<-` counts as state only when its target is one
/// of the body's own bindings.
fn body_facts<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
) -> BodyFacts {
    let mut callees = SmallVec::new();
    let known = walk_body(g, &mut |cs| {
        let unresolved = Unresolved::ASYNC;
        callee_facts(cs, Some(graph), ctx, &unresolved, &mut |iid| callees.push(iid))
    });
    BodyFacts { lambda: g.id(), known, callees }
}

/// The facts a definition's check gives its body before any instance is
/// analyzed: what it knows. A call through one of the definition's own
/// function parameters, or a builtin handed one, contributes nothing,
/// since the caller's argument joins at the call, nor does a call not
/// resolved yet (a `let rec`'s own), which an instance's analysis judges.
pub(crate) fn def_body_facts<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    ctx: &CompileCtx<R, E>,
) -> (LambdaFacts, ParamCalls) {
    let mut params: LPooled<IntSet<BindId>> = LPooled::take();
    for p in g.args().iter() {
        p.ids(&mut |id| {
            params.insert(id);
        });
    }
    let own = |id: BindId| params.contains(&id);
    // a call through one of the definition's own parameters is the
    // caller's to judge, and a recursive call (to a definition whose check
    // is open) an instance's analysis judges; any other call it cannot
    // resolve may be async
    let recursive = |cs: &CallSite<R, E>| {
        cs.fnode().typ().with_deref(|t| match t {
            Some(Type::Fn(ft)) => {
                ft.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id))
            }
            _ => false,
        })
    };
    let unresolved = Unresolved { params: &own, facts: LambdaFacts::ASYNC };
    let mut calls = ParamCalls::default();
    let param = |id: BindId, calls: &mut ParamCalls| {
        if own(id) {
            calls.own = true;
            true
        } else if ctx.def_gate_params.contains_key(&id) {
            if !calls.captured.contains(&id) {
                calls.captured.push(id)
            }
            true
        } else {
            false
        }
    };
    let facts = walk_body(g, &mut |cs| match cs.fnode().view() {
        NodeView::Ref(r) if param(r.id, &mut calls) => LambdaFacts::PURE,
        // a trait method resolves by its receiver at typecheck1
        NodeView::Ref(r) if ctx.env.trait_methods.contains_key(&r.id) => {
            LambdaFacts::PURE
        }
        _ if recursive(cs) => LambdaFacts::PURE,
        _ => {
            if calls_builtin(cs, ctx) {
                for a in cs.arg_nodes() {
                    if let NodeView::Ref(r) = a.view() {
                        param(r.id, &mut calls);
                    }
                }
            }
            callee_facts(cs, None, ctx, &unresolved, &mut |_| ())
        }
    });
    (facts, calls)
}

/// One body's facts, nested lambda bodies excluded, a call's from
/// `call`. A `<-` counts as state only when its target is one of the
/// body's own bindings.
fn walk_body<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    call: &mut dyn FnMut(&CallSite<R, E>) -> LambdaFacts,
) -> LambdaFacts {
    let body = g.body();
    let local: OnceCell<LPooled<IntSet<BindId>>> = OnceCell::new();
    let is_local = |id: BindId| {
        local
            .get_or_init(|| {
                let _profile = profile::phase(Phase::EffectRefs);
                let mut refs = Refs::without_callees();
                body.refs(&mut refs);
                let mut local: LPooled<IntSet<BindId>> = LPooled::take();
                refs.with_bound(|id| {
                    local.insert(id);
                });
                local
            })
            .contains(&id)
    };
    let mut known = LambdaFacts::PURE;
    fusion::for_each_node(body, &mut |n| {
        let e = node_facts(n, OwnTargets::Bound(&is_local), call);
        if gxdbg_effect() {
            if e.effect.is_async() {
                eprintln!("EFFECT-ASYNC-NODE node={}", n.spec());
            }
            if !e.stateless {
                eprintln!("EFFECT-STATEFUL-NODE node={}", n.spec());
            }
        }
        known = known.join(e);
    });
    known
}

/// A dynamic module runs code its source delivers at run time, whose
/// effects nothing here can see.
fn is_dynamic_module<R: Rt, E: UserEvent>(m: &Module<R, E>) -> bool {
    matches!(&m.spec().kind, ExprKind::Module { value, .. }
        if matches!(&**value, ModuleKind::Dynamic { .. }))
}

/// Which `<-` targets count as state of the code being classified.
#[derive(Clone, Copy)]
enum OwnTargets<'a> {
    /// A lambda body's own bindings.
    Bound(&'a dyn Fn(BindId) -> bool),
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
        | NodeView::SeqMachine(_)
        | NodeView::SeqCapture(_)
        | NodeView::Any(_)
        | NodeView::FusedKernel(_) => LambdaFacts::ASYNC,
        NodeView::Connect(c) => match own {
            OwnTargets::Bound(local) if !local(c.id) => LambdaFacts::EVENT,
            OwnTargets::Bound(_) | OwnTargets::All => LambdaFacts::STATEFUL,
        },
        NodeView::ConnectDeref(_) => LambdaFacts::STATEFUL,
        // a body's raise goes to its caller's handler, which only a call knows
        NodeView::Qop(q) => match own {
            _ if !q.raises() => LambdaFacts::PURE,
            OwnTargets::Bound(_) => LambdaFacts::EVENT,
            OwnTargets::All if q.handler().is_some() => LambdaFacts::EVENT,
            OwnTargets::All => LambdaFacts::PURE,
        },
        NodeView::Module(m) if is_dynamic_module(m) => LambdaFacts::ASYNC,
        NodeView::Bind(_)
        | NodeView::Never(_)
        | NodeView::Module(_)
        | NodeView::Block(_)
        | NodeView::MapQ(_)
        | NodeView::FoldQ(_)
        | NodeView::Select(_)
        | NodeView::ExplicitParens(_)
        | NodeView::ForkControl(_)
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

/// A call's facts, its callee's joined with every function argument's.
pub(crate) fn call_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
) -> LambdaFacts {
    callee_facts(cs, None, ctx, &Unresolved::ASYNC, &mut |_| ())
}

/// The facts a call contributes: a callee instance of `graph` is still
/// being inferred, so it goes to `pending` and contributes nothing here;
/// an instance outside the analysis contributes its definition's stored
/// facts, a builtin its declared `EFFECT` joined with its function
/// arguments' facts (`callback_facts`), an unresolved callee
/// `unresolved.facts`.
fn callee_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    graph: Option<&StaticCallGraph<'_, R, E>>,
    ctx: &CompileCtx<R, E>,
    unresolved: &Unresolved,
    pending: &mut dyn FnMut(LambdaInstanceId),
) -> LambdaFacts {
    let facts = callee_own_facts(cs, graph, ctx, unresolved.facts, pending);
    match calls_builtin(cs, ctx) {
        true => facts.join(callback_facts(cs, ctx, unresolved)),
        false => facts,
    }
}

/// What a callee or a callback no analysis resolves contributes.
struct Unresolved<'a> {
    /// The parameters of the definition being checked: the caller's.
    params: &'a dyn Fn(BindId) -> bool,
    facts: LambdaFacts,
}

impl Unresolved<'_> {
    /// An analysis of instances: anything unresolved may be async.
    const ASYNC: Unresolved<'static> =
        Unresolved { params: &|_| false, facts: LambdaFacts::ASYNC };
}

/// The definition `cs` calls, where the check resolved it.
fn callee_def<'a, R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &'a CompileCtx<R, E>,
) -> Option<&'a LambdaDef<R, E>> {
    let lid = cs
        .static_target()
        .map(|t| t.definition)
        .or_else(|| match cs.resolved_apply() {
            Some(ApplyView::Lambda(g)) => Some(g.id()),
            _ => None,
        })
        .or_else(|| {
            let NodeView::Ref(r) = cs.fnode().view() else { return None };
            let v = ctx.bind_to_lambda.get(&r.id)?;
            v.downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
        })?;
    lambda_def(ctx, lid)
}

/// Whether `cs` calls a builtin, which calls back the functions it is
/// handed at run time, where no analysis sees them.
fn calls_builtin<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
) -> bool {
    match callee_def(cs, ctx) {
        Some(d) => d.is_builtin(),
        None => match cs.fnode().view() {
            NodeView::Ref(r) => ctx.env.by_id.get(&r.id).is_some_and(|b| {
                ctx.builtin_bindings.contains_key(&(b.scope.clone(), b.name.clone()))
            }),
            _ => false,
        },
    }
}

/// Whether a builtin call can read or write through an argument: one whose
/// value can hold a reference or a function, at a parameter its declared
/// signature types as one, as `Any` or by a bounded variable (a builtin
/// has nothing else to reach through).
fn builtin_reaches_out<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
) -> bool {
    let env = &ctx.env;
    let ft = callee_def(cs, ctx).map(|d| &d.typ);
    let formal = |ft: &FnType, key: &ArgKey| -> Option<Type> {
        let mut formals = ArgKey::of_formals(&ft.args).zip(ft.args.iter());
        match formals.find(|(k, _)| k == key) {
            Some((_, a)) => Some(a.typ.clone()),
            None => ft.vargs.clone(),
        }
    };
    let reaches = |ft: &FnType, t: &Type| {
        if t.reaches_out(env) {
            return true;
        }
        let bounds = ft.constraint_view();
        if bounds.is_empty() {
            return false;
        }
        let mut tvs = AHashMap::new();
        t.collect_tvars(&mut tvs);
        bounds.iter().any(|(tv, _)| tvs.contains_key(&tv.name))
    };
    cs.args
        .iter()
        .filter(|(_, a)| a.node.as_ref().is_some_and(|n| n.typ().reaches_out(env)))
        .any(|(key, _)| match ft {
            None => true,
            Some(ft) => formal(ft, key).is_none_or(|t| reaches(ft, &t)),
        })
}

/// The facts of the functions a builtin call is handed: each argument of
/// function type, by the definition it is, else `unresolved`'s.
fn callback_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
    unresolved: &Unresolved,
) -> LambdaFacts {
    let function = |n: &Node<R, E>| -> LambdaFacts {
        let lid = match n.view() {
            NodeView::Lambda(l) => {
                let Some(d) = l.def_value().downcast_ref::<LambdaDef<R, E>>() else {
                    return unresolved.facts;
                };
                let calls = d.param_calls.lock();
                // the builtin supplies the literal's own parameters from data;
                // an enclosing definition's resolve per instance, or are its
                // caller's to judge
                let captured = |id: &BindId| match (unresolved.params)(*id) {
                    true => LambdaFacts::PURE,
                    false => ctx
                        .fn_forward_resolutions
                        .get(id)
                        .and_then(|lid| lambda_def(ctx, *lid))
                        .map_or(unresolved.facts, def_facts),
                };
                let own = match calls.own {
                    true => LambdaFacts::ASYNC,
                    false => LambdaFacts::PURE,
                };
                let base = def_facts(d).join(own);
                return calls.captured.iter().map(captured).fold(base, |a, f| a.join(f));
            }
            NodeView::Ref(r) if (unresolved.params)(r.id) => return LambdaFacts::PURE,
            NodeView::Ref(r) => ctx
                .bind_to_lambda
                .get(&r.id)
                .and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
                .map(|d| d.id)
                .or_else(|| ctx.fn_forward_resolutions.get(&r.id).copied()),
            _ => None,
        };
        lid.and_then(|lid| lambda_def(ctx, lid)).map_or(unresolved.facts, def_facts)
    };
    cs.args
        .values()
        .filter_map(|a| a.node.as_ref())
        .filter(|n| n.typ().with_deref(|t| matches!(t, Some(Type::Fn(_)))))
        .fold(LambdaFacts::PURE, |acc, n| acc.join(function(n)))
}

/// A call's facts by its callee alone.
fn callee_own_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    graph: Option<&StaticCallGraph<'_, R, E>>,
    ctx: &CompileCtx<R, E>,
    unresolved: LambdaFacts,
    pending: &mut dyn FnMut(LambdaInstanceId),
) -> LambdaFacts {
    let of_def = |lid: LambdaId| -> LambdaFacts {
        lambda_def(ctx, lid).map(def_facts).unwrap_or(unresolved)
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
        if let Some(bind) = ctx.env.by_id.get(&r.id)
            && let Some(info) =
                ctx.builtin_bindings.get(&(bind.scope.clone(), bind.name.clone()))
        {
            return ctx.builtin_effect(info.name.as_str()).facts();
        }
    }
    if gxdbg_effect() {
        eprintln!("EFFECT-ASYNC-FALLBACK cs={}", cs.fnode().spec());
    }
    unresolved
}

fn mark_recursion<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    facts: &InstanceFacts,
    ctx: &CompileCtx<R, E>,
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
        g.set_self_call(self_bind.map(|bind| SelfCall { bind, tail_loop: false }));
        let tail_calls = TailSelfCalls::collect(g.body(), *instance);
        let tail = component.size == 1
            && !tail_calls.sites.is_empty()
            && !tail_calls.misses_one(g.body(), *instance);
        let body = facts.get(instance).copied().unwrap_or_else(|| {
            lambda_def(ctx, g.id()).map_or(LambdaFacts::ASYNC, def_facts)
        });
        // one native loop runs every pass under the kernel's init view: an
        // Io call a depth reached afresh would fire at its birth does not,
        // so a body whose Io does not follow its formals recurses natively,
        // each depth a fresh block
        let blocker = match body.is_pure() {
            false => None,
            true => match body.io {
                true => {
                    unfollowed_io(g.body(), &g.formal_ids(), ctx, &mut IntSet::default())
                        .map(NoLoop::Io)
                }
                false => None,
            }
            .or_else(|| match self_bind {
                Some(self_bind) => lowering::tail_loop_blocker(g, self_bind, ctx),
                None => Some(NoLoop::Memory),
            }),
        };
        let looped = body.is_pure() && blocker.is_none();
        g.set_self_call(self_bind.map(|bind| SelfCall { bind, tail_loop: looped }));
        let summary = match (tail, looped) {
            (true, true) => RecursionKind::TailRecursive,
            (true, false) => RecursionKind::TailCalls,
            (false, _) if component.cyclic => RecursionKind::Recursive,
            (false, _) => RecursionKind::NotRecursive,
        };
        if let Some(d) = lambda_def(ctx, g.id()) {
            let mut r = d.recursion.lock();
            *r = (*r).max(summary);
            if summary == RecursionKind::TailCalls
                && let Some(b) = blocker
            {
                d.no_loop.lock().get_or_insert(b);
            }
        }
    }
}

/// An instance's self-calls in tail position ([`fusion::for_each_tail_leaf`]).
struct TailSelfCalls<'a, R: Rt, E: UserEvent> {
    sites: SmallVec<[&'a CallSite<R, E>; 4]>,
}

impl<'a, R: Rt, E: UserEvent> TailSelfCalls<'a, R, E> {
    fn collect(body: &'a Node<R, E>, instance: LambdaInstanceId) -> Self {
        let mut res = Self { sites: SmallVec::new() };
        fusion::for_each_tail_leaf(
            body,
            &mut |n| match n.view() {
                NodeView::CallSite(cs) if is_call_to(cs, instance) => {
                    res.sites.push(cs);
                    true
                }
                _ => false,
            },
            &mut |_| (),
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
}

fn is_call_to<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    instance: LambdaInstanceId,
) -> bool {
    cs.static_target().map(|target| target.instance) == Some(instance)
}

fn lambda_def<'a, R: Rt, E: UserEvent>(
    ctx: &'a CompileCtx<R, E>,
    lid: LambdaId,
) -> Option<&'a LambdaDef<R, E>> {
    ctx.lambda_defs.get(&lid).and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
}

fn callee_lambda<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
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
    ctx: &CompileCtx<R, E>,
    node: &Node<R, E>,
) -> bool {
    let mut sleeps = false;
    arm_sleeps(ctx, node, &mut sleeps);
    sleeps
}

/// An arm sleeps when it holds state, an effect, a raise to a handler (an
/// event as a write is) or a recursive call; a kernel by the facts of the
/// region it replaced and its feeders, so fusion decides nothing here.
fn arm_sleeps<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    node: &Node<R, E>,
    sleeps: &mut bool,
) {
    fusion::for_each_node(node, &mut |n| {
        if *sleeps {
            return;
        }
        match n.view() {
            NodeView::FusedKernel(k) => {
                let f = k.facts();
                *sleeps = f.raises || f.recurses || f.io;
                for feeder in k.feeders() {
                    arm_sleeps(ctx, feeder, &mut *sleeps)
                }
            }
            _ => {
                let facts = node_facts(n, OwnTargets::All, &mut |cs| {
                    callee_facts(cs, None, ctx, &Unresolved::ASYNC, &mut |_| ())
                });
                *sleeps = facts.sleeps() || calls_recursive(n, ctx);
            }
        }
    });
}

/// Variables named by id, those some reference points to (`refs`), or
/// every variable (`all`) (`design/dependency_summaries.md` §2).
#[derive(Default)]
struct Vars {
    all: bool,
    refs: bool,
    ids: LPooled<IntSet<BindId>>,
}

impl Vars {
    fn is_empty(&self) -> bool {
        !self.all && !self.refs && self.ids.is_empty()
    }

    fn unknown(&self) -> bool {
        self.all || self.refs
    }

    /// Whether `id` may be among these; a reference's target is not
    /// counted, a `seqq` capture of it keeping its queued value.
    fn names(&self, id: &BindId) -> bool {
        self.all || self.ids.contains(id)
    }

    /// Add `other`; whether anything was added.
    fn union(&mut self, other: &Vars) -> bool {
        if self.all {
            return false;
        }
        if other.all {
            self.all = true;
            self.ids.clear();
            return true;
        }
        let refs = !self.refs && other.refs;
        self.refs |= other.refs;
        let n = self.ids.len();
        self.ids.extend(other.ids.iter().copied());
        refs || self.ids.len() != n
    }

    /// Where `self` and `other` meet: `Some(None)` when either is a
    /// reference or an unknown, else the id they share.
    fn meeting(&self, other: &Vars) -> Option<Option<BindId>> {
        match (self.unknown(), other.unknown()) {
            (true, _) => (!other.is_empty()).then_some(None),
            (_, true) => (!self.is_empty()).then_some(None),
            _ => {
                let (small, large) = if self.ids.len() <= other.ids.len() {
                    (&self.ids, &other.ids)
                } else {
                    (&other.ids, &self.ids)
                };
                small.iter().find(|id| large.contains(id)).map(|id| Some(*id))
            }
        }
    }
}

/// What a summary keeps: the variables read and written, or only
/// whether reads are unknown (through a reference, an opaque call), with
/// the ordered and unbound bits.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Detail {
    Ids,
    Flags,
}

/// What code reads and writes, following statically resolved calls,
/// and whether it may call an ordered builtin ([`crate::BuiltIn::ORDERED`]).
#[derive(Default)]
struct Summary {
    reads: Vars,
    writes: Vars,
    ordered: bool,
    /// It calls a site not bound yet (opaque until it is).
    unbound: bool,
    /// It makes a reference, which publishes its cell in the cycle it
    /// fires, a callee's included.
    makes_refs: bool,
}

impl Summary {
    /// Drop `id` from its reads and writes.
    fn forget(&mut self, id: BindId) {
        self.reads.ids.remove(&id);
        self.writes.ids.remove(&id);
    }

    fn opaque(&mut self) {
        self.reads.all = true;
        self.writes.all = true;
        self.ordered = true;
    }

    fn union(&mut self, other: &Summary) -> bool {
        let r = self.reads.union(&other.reads);
        let o = !self.ordered && other.ordered;
        self.ordered |= other.ordered;
        let u = !self.unbound && other.unbound;
        self.unbound |= other.unbound;
        let m = !self.makes_refs && other.makes_refs;
        self.makes_refs |= other.makes_refs;
        self.writes.union(&other.writes) || r || o || u || m
    }
}

/// Whether `n` itself may run a core-trait impl (`Eq`, `Ord`,
/// `Display`): a comparison or a print of a value that can hold an
/// abstract one, or a kernel whose region holds one.
fn runs_hooks<R: Rt, E: UserEvent>(n: &Node<R, E>, env: &Env) -> bool {
    let holds = |n: &Node<R, E>| n.typ().holds_abstract(env);
    match n.view() {
        NodeView::Eq(o) => holds(&o.lhs) || holds(&o.rhs),
        NodeView::Ne(o) => holds(&o.lhs) || holds(&o.rhs),
        NodeView::Lt(o) => holds(&o.lhs) || holds(&o.rhs),
        NodeView::Gt(o) => holds(&o.lhs) || holds(&o.rhs),
        NodeView::Lte(o) => holds(&o.lhs) || holds(&o.rhs),
        NodeView::Gte(o) => holds(&o.lhs) || holds(&o.rhs),
        NodeView::StringInterpolate(si) => si.args.iter().any(holds),
        NodeView::FusedKernel(k) => k.facts().hooks,
        _ => false,
    }
}

/// What a fused region keeps of the nodes it replaced, callees
/// excluded, for the analyses that walk past it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct RegionFacts {
    /// It may run a core-trait impl, whose reads its feeders do not show.
    pub(crate) hooks: bool,
    /// A raise in it goes to a handler.
    pub(crate) raises: bool,
    /// It calls a recursive definition.
    pub(crate) recurses: bool,
    /// It performs an `Effect::Io` call, its callees' included.
    pub(crate) io: bool,
}

impl Pack for RegionFacts {
    fn encoded_len(&self) -> usize {
        1
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        let bits = self.hooks as u8
            | (self.raises as u8) << 1
            | (self.recurses as u8) << 2
            | (self.io as u8) << 3;
        Ok(buf.put_u8(bits))
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let bits = u8::decode(buf)?;
        if bits > 15 {
            return Err(PackError::InvalidFormat);
        }
        Ok(Self {
            hooks: bits & 1 != 0,
            raises: bits & 2 != 0,
            recurses: bits & 4 != 0,
            io: bits & 8 != 0,
        })
    }
}

/// The [`RegionFacts`] of the region `n`.
pub(crate) fn region_facts<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> RegionFacts {
    let mut f = RegionFacts::default();
    fusion::for_each_node(n, &mut |x| {
        f.hooks |= runs_hooks(x, &ctx.env);
        f.recurses |= calls_recursive(x, ctx);
        let facts = node_facts(x, OwnTargets::All, &mut |cs| {
            callee_facts(cs, None, ctx, &Unresolved::ASYNC, &mut |_| ())
        });
        f.raises |= facts.event;
        f.io |= facts.io;
    });
    f
}

fn calls_recursive<R: Rt, E: UserEvent>(n: &Node<R, E>, ctx: &CompileCtx<R, E>) -> bool {
    let NodeView::CallSite(cs) = n.view() else { return false };
    callee_lambda(cs, ctx)
        .and_then(|lid| lambda_def(ctx, lid))
        .is_some_and(|d| *d.recursion.lock() != RecursionKind::NotRecursive)
}

/// `n`'s own reads and writes into `s`, and the instances its calls
/// reach into `callees`. A write or read through a reference, a call
/// with no static target, a dynamic module, and a builtin handed a
/// function or a reference touch every variable.
/// A call with no static target is opaque: what it calls may change at
/// any bind. Only `current`, a slot site's own call, which the site
/// decides again when its callback rebinds, counts as the instance it
/// calls now.
fn local_summary<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    current: Option<&Node<R, E>>,
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
    detail: Detail,
    s: &mut Summary,
    callees: &mut SmallVec<[LambdaInstanceId; 4]>,
) {
    fusion::for_each_node(n, &mut |x| {
        own_access(x, current, graph, ctx, detail, s, callees);
        if let NodeView::FusedKernel(k) = x.view() {
            k.feeders()
                .iter()
                .for_each(|f| local_summary(f, None, graph, ctx, detail, s, callees))
        }
    })
}

/// The children an access summary walks: a kernel reads only through
/// its feeders.
fn access_children<'a, R: Rt, E: UserEvent>(
    n: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    match n.view() {
        NodeView::FusedKernel(k) => k.feeders().iter().for_each(f),
        _ => fusion::for_each_child(n, f),
    }
}

/// [`local_summary`] of `x` alone, its children aside.
fn own_access<R: Rt, E: UserEvent>(
    x: &Node<R, E>,
    current: Option<&Node<R, E>>,
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
    detail: Detail,
    s: &mut Summary,
    callees: &mut SmallVec<[LambdaInstanceId; 4]>,
) {
    let ids = detail == Detail::Ids;
    match x.view() {
        NodeView::ByRef(_) => s.makes_refs = true,
        NodeView::Ref(r) => {
            if ids {
                s.reads.ids.insert(r.id);
            }
        }
        // through the core-trait impls its region may run
        NodeView::FusedKernel(k) => {
            if k.facts().hooks {
                s.opaque()
            }
        }
        NodeView::Connect(c) => {
            if ids {
                s.writes.ids.insert(c.id);
            }
        }
        // the write reads the reference to find its target
        NodeView::ConnectDeref(c) => {
            s.writes.refs = true;
            if ids {
                s.reads.ids.insert(c.src_id);
            }
        }
        NodeView::Deref(_) => s.reads.refs = true,
        NodeView::Module(m) if is_dynamic_module(m) => s.opaque(),
        // a comparison or a print of an abstract value runs its core-trait
        // impl, whose reads no summary holds
        _ if runs_hooks(x, &ctx.env) => s.opaque(),
        NodeView::CallSite(cs) => {
            if let Some(t) = cs.static_target()
                && graph.instances.contains_key(&t.instance)
            {
                return callees.push(t.instance);
            }
            let current = current.is_some_and(|c| std::ptr::eq(c, x));
            match cs.resolved_apply() {
                Some(ApplyView::Lambda(g))
                    if current && graph.instances.contains_key(&g.instance_id()) =>
                {
                    return callees.push(g.instance_id());
                }
                Some(ApplyView::Opaque) => {
                    s.ordered = true;
                    return s.opaque();
                }
                Some(ApplyView::BuiltIn(name)) if current || cs.bound_statically() => {
                    s.ordered |= ctx.builtin_ordered(name);
                    if builtin_reaches_out(cs, ctx) {
                        if gxdbg_seqplan() {
                            eprintln!("SEQPLAN   opaque builtin {}", cs.fnode().spec());
                        }
                        s.opaque()
                    }
                    return;
                }
                _ => (),
            }
            if let NodeView::Ref(r) = cs.fnode().view()
                && let Some(ids) = graph.self_binds.get(&r.id)
            {
                return callees.extend(ids.iter().copied());
            }
            if gxdbg_seqplan() {
                eprintln!(
                    "SEQPLAN   opaque call {} at {} static={} applied={}",
                    cs.spec(),
                    cs.spec().pos,
                    cs.static_target().is_some(),
                    match cs.resolved_apply() {
                        Some(ApplyView::Lambda(_)) => "lambda",
                        Some(ApplyView::BuiltIn(_)) => "builtin",
                        Some(ApplyView::Opaque) => "opaque",
                        None => "none",
                    }
                );
            }
            s.unbound |= cs.resolved_apply().is_none();
            s.opaque()
        }
        _ => (),
    }
}

/// The summary of every instance `roots` reach: its body's reads and
/// writes joined with its callees', less its own bindings (its formals and
/// what its body binds), which only a fresh activation of it reaches.
fn instance_summaries<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
    detail: Detail,
    roots: impl IntoIterator<Item = LambdaInstanceId>,
) -> LPooled<IntMap<LambdaInstanceId, Summary>> {
    let mut callees: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    let mut sums: LPooled<IntMap<LambdaInstanceId, Summary>> = LPooled::take();
    let mut stack: LPooled<Vec<LambdaInstanceId>> = roots.into_iter().collect();
    while let Some(i) = stack.pop() {
        if sums.contains_key(&i) {
            continue;
        }
        let Some(g) = graph.instances.get(&i) else { continue };
        let mut s = Summary::default();
        let mut cs = SmallVec::new();
        local_summary(g.body(), None, graph, ctx, detail, &mut s, &mut cs);
        if detail == Detail::Ids {
            let mut refs = Refs::without_callees();
            g.body().refs(&mut refs);
            refs.with_bound(|id| s.forget(id));
            g.args().iter().for_each(|p| p.ids(&mut |id| s.forget(id)));
        }
        stack.extend(cs.iter().copied());
        sums.insert(i, s);
        callees.insert(i, cs);
    }
    for cs in callees.values_mut() {
        cs.retain(|c| sums.contains_key(c));
    }
    let components = components(&callees);
    for c in components.components.iter().rev() {
        let members = components.members(c);
        let mut acc = Summary::default();
        for i in members.iter() {
            acc.union(&sums[i]);
            for callee in callees[i].iter().filter(|c| !members.contains(c)) {
                acc.union(&sums[callee]);
            }
        }
        for i in members.iter() {
            let s = sums.get_mut(i).unwrap();
            *s = Summary::default();
            s.union(&acc);
        }
    }
    sums
}

/// A block's statements as runs for forking (`design/parallel_eval.md`
/// §3.3): `(start, end)` ranges over `children`, catches left out. A run
/// is contiguous and holds no catch, and no statement in it reads what
/// an earlier statement of the run publishes, nor shares an ordered
/// call with one. A module, trait or impl statement is a run of its own:
/// what a later statement reads of it (a core-trait method a comparison
/// dispatches to) no summary sees. A seq's abort and its machine are
/// ordered: the abort fails the machine's guards through its handler,
/// which no summary sees either.
pub(crate) fn plan_block<R: Rt, E: UserEvent>(
    children: &[Node<R, E>],
    catches: &[usize],
    ctx: &CompileCtx<R, E>,
) -> Box<[(u32, u32)]> {
    plan_block_explained(children, catches, ctx, &mut |_, _| ())
}

/// What each of `children` reads, writes and calls, its callees
/// included.
fn accesses<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ctx: &CompileCtx<R, E>,
    detail: Detail,
    sites: Sites,
) -> LPooled<Vec<Summary>> {
    let graph = collect_static_graph_of(children.clone());
    let locals = local_summaries(children, &graph, ctx, detail, sites);
    let sums = instance_summaries(
        &graph,
        ctx,
        detail,
        locals.iter().flat_map(|(_, cs)| cs.iter().copied()),
    );
    with_callees(&locals, &sums)
}

/// Whether the children are a slot site's slot calls, each of which
/// counts as the instance it calls now.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Sites {
    Statements,
    Slots,
}

/// Each of `children`'s own reads, writes and calls, and its callees.
fn local_summaries<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>>,
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
    detail: Detail,
    sites: Sites,
) -> LPooled<Vec<(Summary, SmallVec<[LambdaInstanceId; 4]>)>> {
    children
        .map(|n| {
            let mut s = Summary::default();
            let mut cs = SmallVec::new();
            let current = (sites == Sites::Slots).then_some(n);
            local_summary(n, current, graph, ctx, detail, &mut s, &mut cs);
            (s, cs)
        })
        .collect()
}

/// Each local summary joined with its callees' `sums`.
fn with_callees(
    locals: &[(Summary, SmallVec<[LambdaInstanceId; 4]>)],
    sums: &IntMap<LambdaInstanceId, Summary>,
) -> LPooled<Vec<Summary>> {
    locals
        .iter()
        .map(|(local, cs)| {
            let mut access = Summary::default();
            access.union(local);
            cs.iter().filter_map(|c| sums.get(c)).for_each(|sum| {
                access.union(sum);
            });
            access
        })
        .collect()
}

/// Plan each block, and decide each other fork point, the analysis
/// reached, with the whole graph in hand, once: what a fork point's
/// first update would decide from its own children misses a call to the
/// instance holding it (an activation's call to the next one is not
/// bound yet), and a warm start, whose callees are still imaged; and
/// each would walk everything below it again.
fn plan_forks<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
) {
    let mut cx = ForkCx::new(graph, ctx);
    for (children, catches, runs) in graph.blocks.iter() {
        if runs.planned.get().is_some() {
            continue;
        }
        let accesses = with_callees(
            &local_summaries(children.iter(), graph, ctx, Detail::Ids, Sites::Statements),
            cx.sums(Detail::Ids),
        );
        let plan = plan_runs(children, catches, &accesses, &mut |_, _| ());
        let _ = runs.planned.set(plan);
    }
    let mut kids: SmallVec<[&Node<R, E>; 8]> = SmallVec::new();
    for (siblings, of) in graph.forks.iter() {
        kids.clear();
        of.collect(&mut kids);
        siblings.decided_by_analysis(cx.independent(&kids));
    }
}

/// The instance summaries a [`plan_forks`] computes once, at each detail
/// it needs, and each subtree's [`Shallow`] facts it needed.
struct ForkCx<'g, 'a, R: Rt, E: UserEvent> {
    graph: &'g StaticCallGraph<'a, R, E>,
    ctx: &'g CompileCtx<R, E>,
    ids: Option<LPooled<IntMap<LambdaInstanceId, Summary>>>,
    flags: Option<LPooled<IntMap<LambdaInstanceId, Summary>>>,
    shallow: LPooled<IntMap<usize, Shallow>>,
}

impl<'g, 'a, R: Rt, E: UserEvent> ForkCx<'g, 'a, R, E> {
    fn new(graph: &'g StaticCallGraph<'a, R, E>, ctx: &'g CompileCtx<R, E>) -> Self {
        Self { graph, ctx, ids: None, flags: None, shallow: LPooled::take() }
    }

    /// Every reached instance's summary at `detail`.
    fn sums(&mut self, detail: Detail) -> &IntMap<LambdaInstanceId, Summary> {
        let (graph, ctx) = (self.graph, self.ctx);
        let slot = match detail {
            Detail::Ids => &mut self.ids,
            Detail::Flags => &mut self.flags,
        };
        slot.get_or_insert_with(|| {
            instance_summaries(graph, ctx, detail, graph.instances.keys().copied())
        })
    }

    /// [`independent`] over the graph's summaries.
    fn independent(&mut self, children: &[&'a Node<R, E>]) -> Option<bool> {
        if !two_work(children.iter().copied()) {
            return Some(true);
        }
        let shallow: SmallVec<[Shallow; 8]> =
            children.iter().map(|c| self.shallow_of(c)).collect();
        shallow_verdict(&shallow).unwrap_or_else(|| {
            let (graph, ctx) = (self.graph, self.ctx);
            let locals = local_summaries(
                children.iter().copied(),
                graph,
                ctx,
                Detail::Ids,
                Sites::Statements,
            );
            ids_verdict(
                children.iter().copied(),
                &with_callees(&locals, self.sums(Detail::Ids)),
            )
        })
    }

    /// The [`Shallow`] facts of the subtree `n`, its callees included,
    /// each subtree's once.
    fn shallow_of(&mut self, n: &'a Node<R, E>) -> Shallow {
        let key = ptr::from_ref(n) as usize;
        if let Some(s) = self.shallow.get(&key) {
            return *s;
        }
        let s = crate::stack::ensure_sufficient(|| {
            let mut own = Summary::default();
            let mut callees = SmallVec::new();
            own_access(
                n,
                None,
                self.graph,
                self.ctx,
                Detail::Flags,
                &mut own,
                &mut callees,
            );
            let mut s = Shallow::of(&own);
            s.makes_ref = matches!(n.view(), NodeView::ByRef(_));
            if !callees.is_empty() {
                let sums = self.sums(Detail::Flags);
                callees
                    .iter()
                    .filter_map(|c| sums.get(c))
                    .for_each(|c| s.join(Shallow::of(c)));
            }
            access_children(n, &mut |c| {
                let c = self.shallow_of(c);
                s.join(c)
            });
            s
        });
        self.shallow.insert(key, s);
        s
    }
}

/// Whether every Io call `body` reaches fires whenever one of `formals`
/// fires: an Io builtin's trigger fires with them, and a function called
/// with arguments that each do performs only Io that follows its own
/// formals. A native loop's pass, which runs under the kernel's one init
/// view, then performs as the node-walk's fresh activation does.
pub(crate) fn io_follows<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    formals: &IntSet<BindId>,
    ctx: &CompileCtx<R, E>,
    seen: &mut IntSet<LambdaInstanceId>,
) -> bool {
    unfollowed_io(body, formals, ctx, seen).is_none()
}

/// The first call in `body` that breaks [`io_follows`]: an Io call, or a
/// call of a function performing Io that does not follow its formals.
fn unfollowed_io<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    formals: &IntSet<BindId>,
    ctx: &CompileCtx<R, E>,
    seen: &mut IntSet<LambdaInstanceId>,
) -> Option<Expr> {
    let mut first = None;
    fusion::for_each_emitted_node(body, &mut |n| {
        let NodeView::CallSite(cs) = n.view() else { return };
        if first.is_some() {
            return;
        }
        let Some(triggers) = lowering::performing_args(cs, ctx) else { return };
        let follows = triggers.iter().all(|t| fires_with(t, formals))
            && match cs.resolved_apply() {
                Some(ApplyView::Lambda(g)) => {
                    !seen.insert(g.instance_id())
                        || io_follows(g.body(), &g.formal_ids(), ctx, seen)
                }
                _ => true,
            };
        if !follows {
            first = Some(n.spec().clone());
        }
    });
    first
}

/// Whether `n` fires whenever one of `ids` does: a read of one of them,
/// or a part that reads one, a select through its scrutinee only. A
/// constant does not, and neither, here, does a function's result.
pub(crate) fn fires_with<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    ids: &IntSet<BindId>,
) -> bool {
    crate::stack::ensure_sufficient(|| match n.view() {
        NodeView::Ref(r) => ids.contains(&r.id),
        NodeView::Constant(_) => false,
        NodeView::Select(s) => fires_with(&s.arg.node, ids),
        NodeView::CallSite(cs) => {
            !matches!(cs.resolved_apply(), Some(ApplyView::Lambda(_)))
                && cs.arg_nodes().any(|a| fires_with(a, ids))
        }
        _ => {
            let mut fires = false;
            n.for_each_child(&mut |c| fires = fires || fires_with(c, ids));
            fires
        }
    })
}

/// What a statement or sibling publishes for later ones to read: the
/// ids it binds, and each reference's cell, which a reference (and a
/// moving place its path) publishes in the cycle it fires for a later
/// `*r` to read.
fn publishes<R: Rt, E: UserEvent>(n: &Node<R, E>, published: &mut Vars) {
    let mut refs = Refs::without_callees();
    n.refs(&mut refs);
    refs.with_bound(|id| {
        published.ids.insert(id);
    });
    fusion::for_each_node(n, &mut |x| {
        if let NodeView::ByRef(b) = x.view() {
            published.ids.insert(b.id);
        }
    });
}

/// Whether siblings forked at a fork point other than a block's (a
/// constructor's fields, a call's arguments, an operator's operands, a
/// collection's slots) compute in parallel what they do in order: none
/// reads what an earlier one publishes, and no two make ordered calls.
/// A call through an application nothing names is opaque and ordered; a
/// call not bound yet (a warm start's imaged callee, a slot not prebound)
/// is too, and `None` is the answer when the children depend only through
/// one, to ask again once it is bound. A call site decides its arguments
/// again at each bind (`CallSite::clear_prepared_bind`), a collection its
/// slots once they are bound to a new callback (`update_slots`).
pub(crate) fn independent<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ctx: &CompileCtx<R, E>,
    sites: Sites,
) -> Option<bool> {
    if !two_work(children.clone()) {
        return Some(true);
    }
    let flags = accesses(children.clone(), ctx, Detail::Flags, sites);
    let shallow: SmallVec<[Shallow; 8]> = children
        .clone()
        .zip(flags.iter())
        .map(|(c, a)| {
            let mut s = Shallow::of(a);
            fusion::for_each_node(c, &mut |x| {
                s.makes_ref |= matches!(x.view(), NodeView::ByRef(_))
            });
            s
        })
        .collect();
    shallow_verdict(&shallow).unwrap_or_else(|| {
        ids_verdict(children.clone(), &accesses(children, ctx, Detail::Ids, sites))
    })
}

/// Whether two of `children` are more than a constant or a variable
/// read, which publish nothing and call nothing: beside at most one child
/// that is, there is nothing to decide.
pub(crate) fn two_work<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>>,
) -> bool {
    let inert =
        |n: &&Node<R, E>| matches!(n.view(), NodeView::Constant(_) | NodeView::Ref(_));
    children.filter(|n| !inert(n)).nth(1).is_some()
}

/// What deciding siblings needs of each first: whether it reads through
/// a reference or an opaque call, makes an ordered call, calls something
/// not bound yet, or makes a reference.
#[derive(Clone, Copy, Default)]
struct Shallow {
    unknown_reads: bool,
    ordered: bool,
    unbound: bool,
    makes_ref: bool,
}

impl Shallow {
    fn of(s: &Summary) -> Self {
        Self {
            unknown_reads: s.reads.unknown(),
            ordered: s.ordered,
            unbound: s.unbound,
            makes_ref: false,
        }
    }

    fn join(&mut self, other: Self) {
        self.unknown_reads |= other.unknown_reads;
        self.ordered |= other.ordered;
        self.unbound |= other.unbound;
        self.makes_ref |= other.makes_ref;
    }
}

/// The verdict on siblings from their [`Shallow`] facts, or `None` when
/// it needs what they read. A sibling's lets are named by no other
/// sibling, so only a reference reaches what one publishes: with no
/// reference made before the last sibling and none read through or
/// opaque, the ordered calls decide.
fn shallow_verdict(siblings: &[Shallow]) -> Option<Option<bool>> {
    let (_, init) = siblings.split_last()?;
    if init.iter().any(|s| s.makes_ref) || siblings.iter().any(|s| s.unknown_reads) {
        return None;
    }
    let ordered = siblings.iter().filter(|s| s.ordered).nth(1).is_some();
    Some(match ordered {
        false => Some(true),
        true => unbound_or_dependent(siblings.iter().any(|s| s.unbound)),
    })
}

/// The verdict on `children` from what each accesses.
fn ids_verdict<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>>,
    accesses: &[Summary],
) -> Option<bool> {
    let mut published = Vars::default();
    let mut made_refs = false;
    let mut ordered = false;
    for (n, access) in children.zip(accesses.iter()) {
        if access.reads.meeting(&published).is_some()
            || (access.reads.refs && made_refs)
            || (access.ordered && ordered)
        {
            return unbound_or_dependent(accesses.iter().any(|a| a.unbound));
        }
        publishes(n, &mut published);
        made_refs |= access.makes_refs;
        ordered |= access.ordered;
    }
    Some(true)
}

/// Dependent siblings, unless that rests on a call not bound yet.
fn unbound_or_dependent(unbound: bool) -> Option<bool> {
    (!unbound).then_some(false)
}

/// What a fork point other than a block's runs as siblings.
#[derive(Clone, Copy)]
enum ForkKids<'a, R: Rt, E: UserEvent> {
    Args(&'a CallSite<R, E>),
    Nodes(&'a [Node<R, E>]),
    Pair(&'a Node<R, E>, &'a Node<R, E>),
}

impl<'a, R: Rt, E: UserEvent> ForkKids<'a, R, E> {
    fn collect(&self, out: &mut SmallVec<[&'a Node<R, E>; 8]>) {
        match *self {
            Self::Args(cs) => {
                out.extend(cs.args.values().filter_map(|a| a.node.as_ref()))
            }
            Self::Nodes(ns) => out.extend(ns.iter()),
            Self::Pair(l, r) => out.extend([l, r]),
        }
    }
}

/// `n` as a fork point other than a block's: its siblings verdict and
/// the children it decides it over.
fn fork_point<'a, R: Rt, E: UserEvent>(
    n: &'a Node<R, E>,
) -> Option<(&'a Siblings, ForkKids<'a, R, E>)> {
    use NodeView as V;
    let (site, kids): (&ForkSite, _) = match n.view() {
        V::CallSite(cs) => (&cs.fork, ForkKids::Args(cs)),
        V::Struct(c) => (&c.fork, ForkKids::Nodes(&c.n)),
        V::Tuple(c) => (&c.fork, ForkKids::Nodes(&c.n)),
        V::Variant(c) => (&c.fork, ForkKids::Nodes(&c.n)),
        V::Array(c) => (&c.fork, ForkKids::Nodes(&c.n)),
        V::ListLit(c) => (&c.fork, ForkKids::Nodes(&c.n)),
        V::Map(c) => (&c.fork, ForkKids::Nodes(&c.n)),
        V::StringInterpolate(c) => (&c.fork, ForkKids::Nodes(&c.args)),
        V::FusedKernel(k) => (&k.fork, ForkKids::Nodes(k.feeders())),
        v => {
            let (l, r, site) = fusion::binary_operands(&v)?;
            (site, ForkKids::Pair(l, r))
        }
    };
    Some((&site.1, kids))
}

/// Why a block plan starts a new run at a statement.
#[derive(Debug, Clone, Copy)]
pub(crate) enum RunBreak {
    /// A catch, which runs after what it covers.
    Catch,
    /// A module, trait or impl statement, or the statement after one.
    Module,
    /// It reads what an earlier statement of the run publishes: that
    /// variable, or `None` through a reference or an unresolved call.
    Reads(Option<BindId>),
    /// It and an earlier statement of the run both make ordered calls.
    Ordered,
}

/// [`plan_block`], telling `explain` each statement that starts a run
/// after another, and why.
pub(crate) fn plan_block_explained<R: Rt, E: UserEvent>(
    children: &[Node<R, E>],
    catches: &[usize],
    ctx: &CompileCtx<R, E>,
    explain: &mut dyn FnMut(usize, RunBreak),
) -> Box<[(u32, u32)]> {
    let accesses = accesses(children.iter(), ctx, Detail::Ids, Sites::Statements);
    plan_runs(children, catches, &accesses, explain)
}

/// The runs of `children` given what each accesses.
fn plan_runs<R: Rt, E: UserEvent>(
    children: &[Node<R, E>],
    catches: &[usize],
    accesses: &[Summary],
    explain: &mut dyn FnMut(usize, RunBreak),
) -> Box<[(u32, u32)]> {
    let mut runs: LPooled<Vec<(u32, u32)>> = LPooled::take();
    let mut start: Option<usize> = None;
    let mut published = Vars::default();
    let mut made_refs = false;
    let mut run_ordered = false;
    let mut after_module = false;
    for (i, n) in children.iter().enumerate() {
        if catches.contains(&i) {
            if let Some(a) = start.take() {
                runs.push((a as u32, i as u32));
                explain(i, RunBreak::Catch);
            }
            continue;
        }
        let mut access = Summary::default();
        access.union(&accesses[i]);
        let module = matches!(
            n.spec().kind,
            ExprKind::Module { .. } | ExprKind::Trait(_) | ExprKind::Impl(_)
        );
        access.ordered |=
            matches!(n.view(), NodeView::SeqAbort(_) | NodeView::SeqMachine(_));
        let met = match access.reads.meeting(&published) {
            None if access.reads.refs && made_refs => Some(None),
            met => met,
        };
        let why = if module || after_module {
            Some(RunBreak::Module)
        } else if let Some(id) = met {
            Some(RunBreak::Reads(id))
        } else if access.ordered && run_ordered {
            Some(RunBreak::Ordered)
        } else {
            None
        };
        let joins = start.is_some() && why.is_none();
        if !joins {
            if let Some(a) = start.replace(i) {
                runs.push((a as u32, i as u32));
                explain(i, why.expect("a break has a reason"));
            }
            published = Vars::default();
            made_refs = false;
            run_ordered = false;
        }
        publishes(n, &mut published);
        made_refs |= access.makes_refs;
        run_ordered |= access.ordered;
        after_module = module;
    }
    if let Some(a) = start {
        runs.push((a as u32, children.len() as u32));
    }
    runs.drain(..).collect()
}

/// Decide each seq machine's step boundaries: a step enters in the cycle
/// its predecessor completes unless it reads or writes a variable a write
/// since the last next-cycle boundary is still carrying there
/// (`design/dependency_summaries.md` §3). A `seqq` capture of a variable
/// some step writes is live (§4).
fn plan_machines<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
) {
    let StaticCallGraph { machines, captures, .. } = graph;
    if machines.is_empty() {
        return;
    }
    let _profile = profile::phase(Phase::SeqPlan);
    let steps: LPooled<Vec<SmallVec<[(Summary, SmallVec<[LambdaInstanceId; 4]>); 8]>>> =
        machines
            .iter()
            .map(|(m, _)| {
                m.steps
                    .iter()
                    .map(|s| {
                        let mut sum = Summary::default();
                        let mut cs = SmallVec::new();
                        s.nodes.iter().for_each(|n| {
                            local_summary(
                                n,
                                None,
                                graph,
                                ctx,
                                Detail::Ids,
                                &mut sum,
                                &mut cs,
                            )
                        });
                        (sum, cs)
                    })
                    .collect()
            })
            .collect();
    let sums = instance_summaries(
        graph,
        ctx,
        Detail::Ids,
        steps.iter().flat_map(|m| m.iter().flat_map(|(_, cs)| cs.iter().copied())),
    );
    for ((m, holder), local) in machines.iter().zip(steps.iter()) {
        let mut access: SmallVec<[Summary; 8]> = local
            .iter()
            .map(|(local, cs)| {
                let mut s = Summary::default();
                s.union(local);
                cs.iter().filter_map(|c| sums.get(c)).for_each(|c| {
                    s.union(c);
                });
                s.reads.ids.remove(&m.pc_id);
                s.writes.ids.remove(&m.pc_id);
                s
            })
            .collect();
        if let Some(caps) = captures.get(&(m.id, *holder)) {
            let mut written = Vars::default();
            access.iter().for_each(|s| {
                written.union(&s.writes);
            });
            for (id, c) in caps.iter() {
                // CR claude for eric: [risk/medium] A write through a reference never
                // makes a seqq capture live (Vars::names skips reference targets on
                // purpose). A body doing `*r <- a + 1` with `r = &mut a` reads `a` from
                // the enqueue-time snapshot on every run, so three queued requests all
                // write 1: a ends at 1 while `direct <- direct + 1` beside it reaches
                // 3. That is the read-modify-write loss live captures exist to prevent
                // (seq_blocks.md §8). The summaries already count a deref write as
                // every variable for step timing. Either count it here too (every
                // capture live once a step writes through a reference), or document
                // that such a body reads its enqueue-time value. probe:
                // design/review-2026-10-19/repro/x-engine-seq-errors-seqq-ref-write-stale-capture.gx
                // (x-engine-seq-errors-04)
                let live = written.names(&c.live_id);
                c.is_live.store(live, Ordering::Relaxed);
                if live {
                    for a in access.iter_mut().filter(|a| a.reads.ids.contains(id)) {
                        a.reads.ids.insert(c.live_id);
                    }
                }
            }
        }
        let mut pending: SmallVec<[Vars; 8]> =
            (0..access.len()).map(|_| Vars::default()).collect();
        for (k, s) in m.steps.iter().enumerate() {
            let same = match s.next {
                Some(n) if n > k => {
                    let mut out = Vars::default();
                    out.union(&pending[k]);
                    out.union(&access[k].writes);
                    let same = out.meeting(&access[n].reads).is_none()
                        && out.meeting(&access[n].writes).is_none();
                    if same {
                        pending[n].union(&out);
                    }
                    same
                }
                _ => false,
            };
            s.same_cycle.store(same, Ordering::Relaxed);
        }
        if m.expand {
            println!("// seq at {} steps: {}\n", m.spec().pos, m.plan());
        }
        if gxdbg_seqplan() {
            eprintln!("SEQPLAN {} {}", m.spec().pos, m.plan());
            for (k, a) in access.iter().enumerate() {
                eprintln!(
                    "SEQPLAN   S{k} reads {} writes {}",
                    if a.reads.all {
                        "all".into()
                    } else {
                        format!("{:?} refs {}", a.reads.ids, a.reads.refs)
                    },
                    if a.writes.all {
                        "all".into()
                    } else {
                        format!("{:?} refs {}", a.writes.ids, a.writes.refs)
                    },
                );
            }
            for (_, c) in captures.get(&(m.id, *holder)).into_iter().flatten() {
                eprintln!(
                    "SEQPLAN   capture {:?} live {}",
                    c.live_id,
                    c.is_live.load(Ordering::Relaxed)
                );
            }
        }
    }
}

/// Whether `n` is work to run beside something else: not a constant, a
/// variable read, a function literal or a declaration.
fn is_work<R: Rt, E: UserEvent>(n: &Node<R, E>) -> bool {
    let n = match n.view() {
        NodeView::Bind(b) => &b.node,
        _ => n,
    };
    !matches!(
        n.view(),
        NodeView::Constant(_)
            | NodeView::Ref(_)
            | NodeView::Lambda(_)
            | NodeView::TypeDef(_)
            | NodeView::Nop(_)
    )
}

/// Whether a fork point's siblings fork: two of them are work, and they
/// are [`independent`].
fn forks_two<'a, R: Rt, E: UserEvent>(
    siblings: &Siblings,
    of: ForkKids<'a, R, E>,
    ctx: &CompileCtx<R, E>,
) -> bool {
    let mut kids: SmallVec<[&Node<R, E>; 8]> = SmallVec::new();
    of.collect(&mut kids);
    kids.iter().filter(|n| is_work(n)).nth(1).is_some()
        && match siblings.decided() {
            true => siblings.independent(),
            false => {
                independent(kids.iter().copied(), ctx, Sites::Statements) == Some(true)
            }
        }
}

/// `#[parallel]`'s assertion: something within `node`, callees excluded,
/// can run beside something else. A block (the decorated expression, or
/// a `let`'s value) needs a run of two statements, and the error names
/// why its first break was made; anything else needs a fork point with
/// two children that are more than a constant or a variable read.
pub(crate) fn check_parallel<R: Rt, E: UserEvent>(
    spec: &crate::expr::Expr,
    node: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> Result<()> {
    let mut target = node;
    loop {
        target = match target.view() {
            NodeView::Bind(b) => &b.node,
            NodeView::ExplicitParens(p) => &p.n,
            _ => break,
        }
    }
    if let NodeView::Block(b) = target.view() {
        let two = |&(a, z): &(u32, u32)| {
            (a..z).filter(|i| is_work(&b.children[*i as usize])).count() >= 2
        };
        if b.runs.planned.get().is_some_and(|runs| runs.iter().any(two)) {
            return Ok(());
        }
        let mut first: Option<(usize, RunBreak)> = None;
        let runs = plan_block_explained(&b.children, &b.catches, ctx, &mut |i, why| {
            first.get_or_insert((i, why));
        });
        if runs.iter().any(two) {
            return Ok(());
        }
        let Some((i, why)) = first else {
            crate::bailat!(
                spec,
                "#[parallel] has nothing to run in parallel: one statement"
            )
        };
        let reason: compact_str::CompactString = match why {
            RunBreak::Reads(Some(id)) => {
                let name = ctx.env.by_id.get(&id).map(|b| b.name.clone());
                let name = name.as_deref().unwrap_or("a variable");
                compact_str::format_compact!(
                    "reads `{name}`, which an earlier statement publishes"
                )
            }
            RunBreak::Reads(None) => {
                "reads through a reference or a call the compiler cannot resolve".into()
            }
            RunBreak::Ordered => "makes an ordered call after another one".into(),
            RunBreak::Module => "is, or follows, a module, trait or impl".into(),
            RunBreak::Catch => "is a catch, which runs after what it covers".into(),
        };
        crate::bailat!(
            spec,
            "#[parallel] has nothing to run in parallel: statement {} {reason}",
            i + 1
        )
    }
    let mut forks = false;
    fusion::for_each_node(target, &mut |n| {
        forks = forks
            || match n.view() {
                NodeView::MapQ(_) => true,
                NodeView::Block(b) => {
                    let forks =
                        |runs: &[(u32, u32)]| runs.iter().any(|(a, z)| z - a >= 2);
                    match b.runs.planned.get() {
                        Some(runs) => forks(runs),
                        None => forks(&plan_block(&b.children, &b.catches, ctx)),
                    }
                }
                NodeView::CallSite(cs)
                    if matches!(
                        cs.resolved_apply(),
                        Some(ApplyView::Lambda(g)) if matches!(g.body().view(), NodeView::MapQ(_))
                    ) =>
                {
                    true
                }
                _ => fork_point(n)
                    .is_some_and(|(siblings, of)| forks_two(siblings, of, ctx)),
            }
    });
    if !forks {
        crate::bailat!(spec, "#[parallel] has nothing to run in parallel")
    }
    Ok(())
}
