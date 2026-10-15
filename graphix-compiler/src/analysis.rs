//! Compile-time analysis, run after `typecheck1` in both fusion modes,
//! over the reachable call graph: effect inference (a greatest fixpoint
//! from `Sync` down to `Async`), recursion/tail marking (SCCs,
//! `GXLambda::tail_loop`, `RecursionKind`), the definition assertions,
//! and the dependency summaries that plan seq steps and block runs. Also
//! the `#[parallel]` check and the arm-sleep rule. Both engines read the
//! facts; the structural tail-loop predicate is
//! `fusion::lowering::structural_tail_loop`.

use crate::{
    ApplyView, BindId, CompileCtx, DefAssertion, DefAssertionKind, LambdaId,
    LambdaInstanceId, Node, NodeView, Refs, Rt, Update, UserEvent,
    dbgenv::{gxdbg_effect, gxdbg_seqplan},
    effects::{LambdaFacts, RecursionKind},
    env::Env,
    expr::{At, ExprKind, ModuleKind},
    fusion::{self, lowering},
    node::{
        Block,
        callsite::CallSite,
        fork_control::{ForkControl, ForkKind},
        lambda::{GXLambda, LambdaDef},
        module::Module,
        seq_machine::{SeqCapture, SeqMachine},
    },
    profile::{self, Phase},
    typ::Type,
};
use ahash::AHashMap;
use anyhow::{Result, anyhow};
use bytes::{Buf, BufMut};
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
    /// Blocks of two or more statements, by the instance holding them.
    blocks: LPooled<Vec<(&'a Block<R, E>, Holder)>>,
    /// The `#[parallel]` sites.
    parallels: LPooled<Vec<&'a ForkControl<R, E>>>,
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
            let site = match n.view() {
                NodeView::CallSite(site) => site,
                NodeView::SeqMachine(m) => return graph.machines.push((m, caller)),
                NodeView::Block(b) if b.children.len() > 1 => {
                    return graph.blocks.push((b, caller));
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
            if let NodeView::Ref(r) = site.fnode().view() {
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
    plan_machines(&graph, &ctx.env);
    plan_blocks(&graph, ctx);
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
    plan_machines(&graph, &ctx.env);
    plan_blocks(&graph, ctx);
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
) -> Option<&'static str> {
    let effect = d.facts.lock().effect;
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
                 a fused kernel can carry and shares nothing between its passes, \
                 so with a labeled, variadic or opaque formal, or a body holding \
                 per-call memory (a collection operation, or a callee holding \
                 one), every call keeps its own activation",
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
) -> LambdaFacts {
    let mut params: LPooled<IntSet<BindId>> = LPooled::take();
    for p in g.args().iter() {
        p.ids(&mut |id| {
            params.insert(id);
        });
    }
    let own = |id: BindId| params.contains(&id);
    // CR claude for claude: [bug/medium] Every call this definition check cannot
    // resolve counts as PURE, not only a `let rec`'s own call. Take `let k = pick(x);
    // k(x)`, a call through a local bound to a call's result: nothing later corrects
    // its facts for a callback literal that only a builtin calls (filter, opt::map),
    // because infer_effects never reaches that literal. callback_facts then joins these
    // optimistic def facts into the builtin call. The result is that `#[sync] let g =
    // |v: i64| filter(v, |x| { let k = pick(x > 0); k(x) })` is accepted while its
    // result lands a timer reply later. The same body written without filter is
    // refused. The same wrong facts also feed arm_sleeps and the `stateless` bit. Fix:
    // keep PURE only for the definition's own parameters and its recursive self-binds,
    // and give any other unresolved call `LambdaFacts::ASYNC`. The doc comment on
    // call_facts ("anything else `Async`") would then be true again. This is the same
    // root cause as c-lambda-01 (parameters); fix them together. probe:
    // design/review-2026-10-12/repro/x-fix-nodes-02.gx (x-fix-nodes-02)
    let unresolved = Unresolved { params: &own, facts: LambdaFacts::PURE };
    walk_body(g, &mut |cs| match cs.fnode().view() {
        NodeView::Ref(r) if own(r.id) => LambdaFacts::PURE,
        _ => callee_facts(cs, None, ctx, &unresolved, &mut |_| ()),
    })
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
        // CR claude for eric: [perf/medium] never() is classified ASYNC here, which
        // marks any function with a `never()` arm (the idiom the book teaches for 'no
        // value here') as stateful or async. Its tail recursion then gets no loop head
        // and keeps one activation per level, and #[tail_recursive] refuses it with a
        // misleading message about stateful builtins. OrNever, which delivers the same
        // standing bottom, is PURE, and the select emitter already lowers a never() arm
        // as a pure standing bottom. Deciding: should never() with pure args be PURE?
        // That also changes what #[sync]/#[async] and the arm-sleep skip see. If yes,
        // emit_body_tail needs the never-arm rule that emit_select_value_arm uses
        // (consume the args, return a STALE bottom), shared as one helper, or the
        // change would de-fuse. probe: design/review-2026-10-12/repro/f-select-05.gx
        // (f-select-05)
        | NodeView::Never(_)
        | NodeView::FusedKernel(_) => LambdaFacts::ASYNC,
        NodeView::Connect(c) => match own {
            OwnTargets::Bound(local) if !local(c.id) => LambdaFacts::EVENT,
            OwnTargets::Bound(_) | OwnTargets::All => LambdaFacts::STATEFUL,
        },
        NodeView::ConnectDeref(_) => LambdaFacts::STATEFUL,
        // a body's raise goes to its caller's handler, which only a call knows
        NodeView::Qop(q) => match own {
            OwnTargets::Bound(_) => LambdaFacts::EVENT,
            OwnTargets::All if q.raises_to_handler() => LambdaFacts::EVENT,
            OwnTargets::All => LambdaFacts::PURE,
        },
        NodeView::OrNever(_) => LambdaFacts::PURE,
        NodeView::Module(m) if is_dynamic_module(m) => LambdaFacts::ASYNC,
        NodeView::Bind(_)
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

/// Whether `cs` calls a builtin, which calls back the functions it is
/// handed at run time, where no analysis sees them.
fn calls_builtin<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
) -> bool {
    let fref = match cs.fnode().view() {
        NodeView::Ref(r) => Some(r.id),
        _ => None,
    };
    let def = cs
        .static_target()
        .map(|t| t.definition)
        .or_else(|| match cs.resolved_apply() {
            Some(ApplyView::Lambda(g)) => Some(g.id()),
            _ => None,
        })
        .or_else(|| {
            let v = ctx.bind_to_lambda.get(&fref?)?;
            v.downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
        });
    match def {
        Some(lid) => lambda_def(ctx, lid).is_some_and(|d| d.is_builtin()),
        None => fref.and_then(|id| ctx.env.by_id.get(&id)).is_some_and(|b| {
            ctx.builtin_bindings.contains_key(&(b.scope.clone(), b.name.clone()))
        }),
    }
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
            // CR claude for claude: [bug/medium] A lambda literal handed to a builtin
            // is judged here by its definition's facts alone. `def_body_facts` counts
            // every call it cannot resolve as PURE: a parameter captured from the
            // enclosing function, and the literal's own function parameters, which the
            // builtin supplies from data. No instance analysis ever sees the literal's
            // body, because the builtin instantiates it at run time. So `#[sync] |y, k|
            // opt::map(y, |v| k(v))` and `#[sync] |m| opt::map(m, |g| g(3))` are
            // accepted with an async `k` or `g`, while `opt::map(y, k)` and `k(y)` are
            // correctly refused. Fix: for a literal argument, walk its checked body
            // with `Unresolved::ASYNC`. Resolve captured function parameters through
            // `fn_forward_resolutions` as `callee_own_facts` does, and count the
            // literal's own function-typed parameters as unresolved. Alternatively,
            // record in the definition whether its body made an unresolved call, and
            // join ASYNC here when it did. Add pins for both shapes beside
            // `sync_on_async_builtin_callback`. Probe:
            // design/review-2026-10-12/repro/c-lambda-01.gx (c-lambda-01)
            NodeView::Lambda(l) => {
                l.def_value().downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
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
        g.set_self_recursive(self_info.contains_key(instance));
        g.set_self_bind(self_bind);
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
        let loopable = body.is_pure()
            && (!body.io
                || io_follows(g.body(), &g.formal_ids(), ctx, &mut IntSet::default()));
        let looped = loopable
            && self_bind.is_some_and(|self_bind| {
                lowering::structural_tail_loop(g, self_bind, ctx)
            });
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

    // CR claude for claude: [structure/low] `meets` repeats `meeting`'s unknown and
    // small-into-large logic line for line. A change made to one copy only would make
    // seq step planning (`plan_machines` uses `meets`) disagree with block and fork
    // planning (which use `meeting`). Fix: make the body
    // `self.meeting(other).is_some()`. (c-analysis-branch-09)
    fn meets(&self, other: &Vars) -> bool {
        match (self.unknown(), other.unknown()) {
            (true, _) => !other.is_empty(),
            (_, true) => !self.is_empty(),
            _ => {
                let (small, large) = if self.ids.len() <= other.ids.len() {
                    (&self.ids, &other.ids)
                } else {
                    (&other.ids, &self.ids)
                };
                small.iter().any(|id| large.contains(id))
            }
        }
    }
}

/// What code reads and writes, following statically resolved calls,
/// and whether it may call an ordered builtin ([`crate::BuiltIn::ORDERED`]).
#[derive(Default)]
struct Summary {
    reads: Vars,
    writes: Vars,
    ordered: bool,
}

impl Summary {
    /// This summary less its reads of `ids`.
    fn without_reads(&self, ids: &IntSet<BindId>) -> Summary {
        let mut s = Summary::default();
        s.union(self);
        s.reads.ids.retain(|id| !ids.contains(id));
        s
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
        self.writes.union(&other.writes) || r || o
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
fn local_summary<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    graph: &StaticCallGraph<'_, R, E>,
    env: &Env,
    ordered: &dyn Fn(&str) -> bool,
    s: &mut Summary,
    callees: &mut SmallVec<[LambdaInstanceId; 4]>,
) {
    fusion::for_each_node(n, &mut |x| match x.view() {
        NodeView::Ref(r) => {
            s.reads.ids.insert(r.id);
        }
        // a kernel reads only through its feeders, and through the core-trait
        // impls its region may run
        NodeView::FusedKernel(k) => {
            if k.facts().hooks {
                s.opaque()
            }
            k.feeders()
                .iter()
                .for_each(|f| local_summary(f, graph, env, ordered, s, callees))
        }
        NodeView::Connect(c) => {
            s.writes.ids.insert(c.id);
        }
        // the write reads the reference to find its target
        NodeView::ConnectDeref(c) => {
            s.writes.refs = true;
            s.reads.ids.insert(c.src_id);
        }
        NodeView::Deref(_) => s.reads.refs = true,
        NodeView::Module(m) if is_dynamic_module(m) => s.opaque(),
        // a comparison or a print of an abstract value runs its core-trait
        // impl, whose reads no summary holds
        _ if runs_hooks(x, env) => s.opaque(),
        // CR claude for claude: [perf/medium] After a warm image start, a child call's
        // callee is still `Callee::Imaged` when a gather, argument, operator or slot
        // site first updates. Its instance is not in the graph and `resolved_apply` is
        // None, so this arm makes the call opaque. `ForkSite::decide_siblings` and
        // `SlotSite::decide_siblings` then fix the site Dependent for the life of the
        // program. The same program forks cold and never forks warm. Block plans avoid
        // this only because `plan_blocks` runs at analysis and `Block::planned` is
        // imaged. Fix: summarize an Imaged callee from the reference summary it carries
        // (or its definition's check table), or keep the site Undecided while any child
        // callee is still Imaged. Probe:
        // design/review-2026-10-12/repro/c-analysis-branch-04.sh (cold prints "g 1"
        // first, warm prints "f ..." first). (c-analysis-branch-03)
        NodeView::CallSite(cs) => {
            if let Some(t) = cs.static_target()
                && graph.instances.contains_key(&t.instance)
            {
                return callees.push(t.instance);
            }
            match cs.resolved_apply() {
                Some(ApplyView::Lambda(g))
                    if graph.instances.contains_key(&g.instance_id()) =>
                {
                    return callees.push(g.instance_id());
                }
                // CR claude for claude: [bug/medium] The seq lowering itself trips this
                // arm. issue_call routes every issued call's arguments through
                // `core::once(seqargs)`. When an argument is a reference, `reaches_out`
                // holds and the whole step summarizes as opaque (reads all, writes
                // all). plan_machines then makes every `seqq` capture live, including
                // ones nothing writes. `seqq request { look(q); e }` with a read-only
                // `look = |p: &mut i64| *p` reads `e` live (30 30 30) where `look2(1);
                // e` keeps the enqueue snapshots (0 10 20). That contradicts
                // design/seq_blocks.md §8: a dereferenced write makes no capture live.
                // Callees that write through a captured reference already follow that
                // rule. The step plan also over-serializes these steps. Fix: treat the
                // pass-through builtins the seq lowering adds (`once`, and `hold` in
                // the seqq preamble) as transparent here, summarizing their arguments
                // rather than going opaque, or have issue_call snapshot the arguments
                // without a builtin. Add a seqq pin to seq_steps.rs with a reference
                // argument and an unwritten capture. probe:
                // design/review-2026-10-12/repro/tests-seq-01.gx (tests-seq-01)
                Some(ApplyView::Opaque) => {
                    s.ordered = true;
                    return s.opaque();
                }
                Some(ApplyView::BuiltIn(name)) => {
                    s.ordered |= ordered(name);
                    let reaches = cs
                        .args
                        .values()
                        .filter_map(|a| a.node.as_ref())
                        .any(|a| a.typ().reaches_out(env));
                    if reaches {
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
            s.opaque()
        }
        _ => (),
    })
}

/// The summary of every instance `roots` reach: its body's own reads and
/// writes joined with its callees', to a fixpoint over recursion.
fn instance_summaries<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    env: &Env,
    ordered: &dyn Fn(&str) -> bool,
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
        local_summary(g.body(), graph, env, ordered, &mut s, &mut cs);
        stack.extend(cs.iter().copied());
        sums.insert(i, s);
        callees.insert(i, cs);
    }
    let mut callers: LPooled<IntMap<LambdaInstanceId, SmallVec<[LambdaInstanceId; 4]>>> =
        LPooled::take();
    for (i, cs) in callees.iter() {
        for c in cs.iter() {
            callers.entry(*c).or_default().push(*i);
        }
    }
    // CR claude for claude: [perf/medium] `instance_summaries` is quadratic even for
    // one block, for two reasons. First, `local_summary` records every `Ref`, including
    // the instance's own formals and lets, so each summary holds every bind id of its
    // whole subtree. Second, this worklist pops in hash order, copies the instance's
    // whole set on each pop (`acc.union(&sums[&i])`), and re-queues every caller after
    // any change, so ancestors are re-unioned again and again. A binary call tree of
    // 32768 instances spends about 3.9 s in Analysis, and the time grows 4x per
    // doubling. A callee's own binds can never meet what a sibling statement publishes,
    // so drop each instance's bound ids (its formals plus its body's
    // `Refs::without_callees` bound set) before propagating. Then union each instance's
    // callees once, in reverse topological order over `strongly_connected`'s
    // components, iterating only the cyclic ones and making no full copy per pop.
    // probe: design/review-2026-10-12/repro/x-perf-compile-04-summaries-fixpoint.gx
    // (x-perf-compile-04)
    let mut work: LPooled<Vec<LambdaInstanceId>> = sums.keys().copied().collect();
    while let Some(i) = work.pop() {
        let mut acc = Summary::default();
        acc.union(&sums[&i]);
        let mut changed = false;
        for c in callees[&i].iter() {
            if let Some(s) = sums.get(c) {
                changed |= acc.union(s);
            }
        }
        if changed {
            sums.insert(i, acc);
            if let Some(cs) = callers.get(&i) {
                work.extend(cs.iter().copied());
            }
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
) -> LPooled<Vec<Summary>> {
    let graph = collect_static_graph_of(children.clone());
    accesses_in(children, &graph, None, ctx)
}

/// [`accesses`] over `graph`, where `holder`'s instance holds the
/// children: a call to it is a fresh activation, whose own bindings no
/// earlier statement publishes.
fn accesses_in<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>>,
    graph: &StaticCallGraph<'_, R, E>,
    holder: Holder,
    ctx: &CompileCtx<R, E>,
) -> LPooled<Vec<Summary>> {
    let ordered = |name: &str| ctx.builtin_ordered(name);
    let locals: LPooled<Vec<(Summary, SmallVec<[LambdaInstanceId; 4]>)>> = children
        .map(|n| {
            let mut s = Summary::default();
            let mut cs = SmallVec::new();
            local_summary(n, graph, &ctx.env, &ordered, &mut s, &mut cs);
            (s, cs)
        })
        .collect();
    let sums = instance_summaries(
        graph,
        &ctx.env,
        &ordered,
        locals.iter().flat_map(|(_, cs)| cs.iter().copied()),
    );
    let own: LPooled<IntSet<BindId>> = match holder.and_then(|h| graph.instances.get(&h))
    {
        None => LPooled::take(),
        Some(g) => {
            let mut refs = Refs::without_callees();
            g.body().refs(&mut refs);
            let mut own: LPooled<IntSet<BindId>> = LPooled::take();
            refs.with_bound(|id| {
                own.insert(id);
            });
            g.args().iter().for_each(|p| {
                p.ids(&mut |id| {
                    own.insert(id);
                })
            });
            own
        }
    };
    locals
        .iter()
        .map(|(local, cs)| {
            let mut access = Summary::default();
            access.union(local);
            for c in cs.iter() {
                let Some(sum) = sums.get(c) else { continue };
                // a self-call's reads of the activation's own names are the
                // new activation's
                match Some(*c) == holder {
                    true => access.union(&sum.without_reads(&own)),
                    false => access.union(sum),
                };
            }
            access
        })
        .collect()
}

/// Plan each block the analysis reached with the whole graph in hand,
/// once: what a block's first update would plan from its own children
/// misses a call to the instance holding it, and a warm start, whose
/// callees are still imaged.
fn plan_blocks<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
) {
    for (b, holder) in graph.blocks.iter() {
        if b.planned.get().is_some() {
            continue;
        }
        // CR claude for claude: [perf/medium] Each block runs `accesses_in`, and
        // `accesses_in` runs `instance_summaries` from scratch over every instance the
        // block reaches. With B blocks over a chain of N instances whose read sets are
        // O(N), the build is cubic: a chain of 400 block-bodied functions builds in
        // about 3 s (6 s in debug) against 0.4 s for `--check`, and 800 takes 32 s.
        // Compute the summaries once per analysis, over `graph.instances`, before the
        // block loop. Then let `accesses_in` take that shared map and keep only the
        // per-block `own` filtering. With x-perf-compile-04's fix each block then costs
        // only its statements. probe:
        // design/review-2026-10-12/repro/x-perf-compile-02-plan-blocks-cubic.gx
        // (x-perf-compile-02)
        let accesses = accesses_in(b.children.iter(), graph, *holder, ctx);
        let plan = plan_runs(&b.children, &b.catches, &accesses, &mut |_, _| ());
        let _ = b.planned.set(plan);
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
    let mut follows = true;
    fusion::for_each_emitted_node(body, &mut |n| {
        let NodeView::CallSite(cs) = n.view() else { return };
        if !follows {
            return;
        }
        let Some(triggers) = lowering::performing_args(cs, ctx) else { return };
        follows = triggers.iter().all(|t| fires_with(t, formals))
            && match cs.resolved_apply() {
                Some(ApplyView::Lambda(g)) => {
                    !seen.insert(g.instance_id())
                        || io_follows(g.body(), &g.formal_ids(), ctx, seen)
                }
                _ => true,
            };
    });
    follows
}

/// Whether `n` fires whenever one of `ids` does: a read of one of them,
/// or a part that reads one, a select through its scrutinee only. A
/// constant does not, and neither, here, does a function's result.
fn fires_with<R: Rt, E: UserEvent>(n: &Node<R, E>, ids: &IntSet<BindId>) -> bool {
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
/// A call not resolved yet, or through an application nothing names, is
/// opaque and ordered; a call site decides its arguments again at each
/// bind (`CallSite::clear_prepared_bind`).
// CR claude for claude: [bug/high] The premise on this doc ("a call not resolved yet is
// opaque, so this is decided before binds") is false for collection slots. update_slots
// asks this after the slots are bound, and local_summary follows
// CallSite::resolved_apply, which is whatever instance the callback holds right now.
// SlotSite::decide_siblings and ForkSite::decide_siblings keep the answer forever. So
// when a dynamic callback changes from one that calls nothing ordered to one that calls
// a queuefn wrapper, the slots keep forking, and the queue sees the calls out of order:
// GRAPHIX_PAR=force (also auto with GRAPHIX_PAR_T=1) prints [301, 304, ..] where the
// serial run prints [303, 304, ..]. Probe:
// design/review-2026-10-12/repro/c-cost-misc-01.gx. There are two possible fixes. One
// is to make the answer invariant under rebinding: here, a call with no static_target
// counts as opaque even while it is bound. The other is to have a rebind of a slot's or
// a call's callee reset the owning site's Siblings to Undecided. The second also covers
// c-cost-misc-02's Nop-default case. (c-cost-misc-01)
// 2026-10-14 claude: a call site's own argument decision is reset at every bind now
// (CallSite::clear_prepared_bind, for c-cost-misc-02 / c-callsite-09), and an apply
// nothing names (the queuefn wrapper) summarizes as opaque and ordered
// (c-analysis-branch-01); a collection's SlotSite still keeps its first verdict when
// its callback rebinds, so this probe stands for slots.
pub(crate) fn independent<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ctx: &CompileCtx<R, E>,
) -> bool {
    // CR claude for claude: [perf/medium] Every fork site that is not a block (an
    // operator's operands, a call's arguments, a constructor's fields, a collection's
    // slots) runs `independent` at its first update. Each run rebuilds
    // `collect_static_graph_of` and `instance_summaries` over everything reachable
    // below the site. A call chain with an operator at each level therefore pays
    // quadratically in the first cycle under the default `GRAPHIX_PAR=auto`, and each
    // new slot or activation pays again. At n=800 the first value takes 22 s with auto
    // against 1.5 s with off (40 s against 3 s in debug). Decide these sites from
    // summaries computed once. Either `analyze` keeps each instance's summary where
    // `independent` can read it, or analysis decides each fork site's siblings the way
    // `plan_blocks` decides blocks, and the decision rides the image. probe:
    // design/review-2026-10-12/repro/x-perf-compile-03-independent-first-cycle.gx
    // (compare GRAPHIX_PAR=auto with off) (x-perf-compile-03)
    let accesses = accesses(children.clone(), ctx);
    let mut published = Vars::default();
    let mut ordered = false;
    for (n, access) in children.zip(accesses.iter()) {
        if access.reads.meeting(&published).is_some() || (access.ordered && ordered) {
            return false;
        }
        publishes(n, &mut published);
        ordered |= access.ordered;
    }
    true
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
    let accesses = accesses(children.iter(), ctx);
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
        let met = access.reads.meeting(&published);
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
            run_ordered = false;
        }
        publishes(n, &mut published);
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
fn plan_machines<R: Rt, E: UserEvent>(graph: &StaticCallGraph<'_, R, E>, env: &Env) {
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
                            local_summary(n, graph, env, &|_| false, &mut sum, &mut cs)
                        });
                        (sum, cs)
                    })
                    .collect()
            })
            .collect();
    let sums = instance_summaries(
        graph,
        env,
        &|_| false,
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
                    let same =
                        !out.meets(&access[n].reads) && !out.meets(&access[n].writes);
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

/// Whether siblings fork: two of them are work, and the runtime finds
/// them [`independent`].
fn forks_two<'a, R: Rt, E: UserEvent>(
    ns: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ctx: &CompileCtx<R, E>,
) -> bool {
    ns.clone().filter(|n| is_work(n)).count() >= 2 && independent(ns, ctx)
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
        if b.planned.get().is_some_and(|runs| runs.iter().any(two)) {
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
                // CR claude for claude: [bug/medium] Every map counts as a fork here,
                // but a map whose callback is a function value never forks. Its
                // prototype has no static target, so each slot calls through the
                // callback parameter (`CallKind::Slot(None)`), `CallSite::prebind`
                // leaves that call unbound, and `analysis::independent` reads it as
                // opaque. `SlotSite::decide_siblings` then marks the site Dependent for
                // good, and the slots run in order even under #[parallel] or
                // GRAPHIX_PAR=force. So `#[parallel] array::map(xs, cb)` compiles but
                // never runs in parallel, which breaks the assertion's contract (a
                // compile error where nothing forks). Fix: count a MapQ as forking only
                // when its prototype call resolves statically, and refuse the others
                // with a reason ("the callback is a function value the compiler cannot
                // resolve"). The alternative is to let a function-value callback fork
                // by deciding slot independence after the slots bind, and deciding it
                // again when the callback value changes. probe:
                // design/review-2026-10-12/repro/x-parallel-04.gx Also reported as
                // (c-analysis-branch-05). (x-parallel-04)
                NodeView::MapQ(_) => true,
                NodeView::Block(b) => {
                    let forks =
                        |runs: &[(u32, u32)]| runs.iter().any(|(a, z)| z - a >= 2);
                    match b.planned.get() {
                        Some(runs) => forks(runs),
                        None => forks(&plan_block(&b.children, &b.catches, ctx)),
                    }
                }
                NodeView::CallSite(cs) => {
                    let slots = matches!(
                        cs.resolved_apply(),
                        Some(ApplyView::Lambda(g)) if matches!(g.body().view(), NodeView::MapQ(_))
                    );
                    slots
                        || forks_two(
                            cs.args.values().filter_map(|a| a.node.as_ref()),
                            ctx,
                        )
                }
                NodeView::Struct(c) => forks_two(c.n.iter(), ctx),
                NodeView::Tuple(c) => forks_two(c.n.iter(), ctx),
                NodeView::Variant(c) => forks_two(c.n.iter(), ctx),
                NodeView::Array(c) => forks_two(c.n.iter(), ctx),
                NodeView::ListLit(c) => forks_two(c.n.iter(), ctx),
                NodeView::Map(c) => forks_two(c.n.iter(), ctx),
                NodeView::StringInterpolate(c) => forks_two(c.args.iter(), ctx),
                v => fusion::binary_operands(&v)
                    .is_some_and(|(l, r)| forks_two([l, r].into_iter(), ctx)),
            }
    });
    if !forks {
        crate::bailat!(spec, "#[parallel] has nothing to run in parallel")
    }
    Ok(())
}
