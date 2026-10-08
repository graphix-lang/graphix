//! Compile-time analysis, run after `typecheck1` in both fusion modes,
//! over the reachable call graph: effect inference (a greatest fixpoint
//! from `Sync` down to `Async`), recursion/tail marking (SCCs,
//! `GXLambda::tail_loop`, `RecursionKind`), the definition assertions,
//! and the dependency summaries that plan seq steps and block runs. Also
//! the `#[parallel]` check and the arm-sleep rule. Both engines read the
//! facts; the structural tail-loop predicate is
//! `fusion::lowering::structural_tail_loop`.

use crate::{
    ApplyView, BindId, CompileCtx, DefAssertionKind, LambdaId, LambdaInstanceId, Node,
    NodeView, Refs, Rt, Update, UserEvent,
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
};
use ahash::AHashMap;
use anyhow::{Result, anyhow};
use nohash::{IntMap, IntSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{cell::OnceCell, collections::hash_map::Entry, ptr, sync::atomic::Ordering};

struct StaticEdge<'a, R: Rt, E: UserEvent> {
    caller: Option<LambdaInstanceId>,
    callee: LambdaInstanceId,
    site: &'a CallSite<R, E>,
}

/// The instances reachable from an analysis root, the statically
/// resolved calls between them, and, per bind a resolved call names
/// its callee through, the instances it reached: the back-edge table
/// for a self-call that is not yet in `ctx.bind_to_lambda` (a
/// dynamically bound recursive callee). Also the seq machines met, and
/// the `seqq` captures by the id of their machine and the instance that
/// holds them (every instance of a definition shares the machine's id),
/// with the bind each is the value of.
/// The instance a node belongs to; `None` outside every instance.
type Holder = Option<LambdaInstanceId>;

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

/// Run the analysis over the whole compiled program. Results land via
/// interior mutability on the nodes reached.
pub fn analyze<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> Result<()> {
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
    if let Err(e) = parallels.and_then(|()| check_def_assertions(&graph, ctx)) {
        crate::node::error::report_failure!(&compact_str::format_compact!("{e:#}"));
    }
}

/// Check every pending assertion whose definition this analysis
/// reached, retiring all but a `#[sync]`; drop those whose definition
/// is gone. Stops at the
/// first failure.
fn check_def_assertions<R: Rt, E: UserEvent>(
    graph: &StaticCallGraph<'_, R, E>,
    ctx: &CompileCtx<R, E>,
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
            _ => drop(pending.remove(i)),
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
            // joined under the lock: binds in forked branches join here
            // at once, and the facts only ever degrade
            let mut facts = d.facts.lock();
            *facts = facts.join(eff[iid]);
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
    let mut res =
        BodyFacts { lambda: g.id(), known: LambdaFacts::PURE, callees: SmallVec::new() };
    fusion::for_each_node(body, &mut |n| {
        let e = node_facts(n, OwnTargets::Bound(&is_local), &mut |cs| {
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

/// A dynamic module runs code its source delivers at run time, whose
/// effects nothing here can see.
fn is_dynamic_module<R: Rt, E: UserEvent>(m: &Module<R, E>) -> bool {
    matches!(&m.spec().kind, ExprKind::Module { value: ModuleKind::Dynamic { .. }, .. })
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
        | NodeView::Never(_)
        // CR claude for eric: [bug] A fused arm body is a FusedKernel. This line calls
        // it ASYNC and for_each_node does not look inside it, so under fusion
        // arm_sleeps_on_deselect puts a pure arm to sleep that the node-walk never
        // sleeps. At re-entry the node-walk runs that arm as a birth (standing reads
        // fresh, select.rs:1007-1014) and the JIT runs it as a wake (standing reads
        // stale). So a handler-ful `?` over a standing error raises again at every
        // re-entry on the node-walk and not on the JIT: probe
        // design/review-2026-10-05/repro/f-kernel-02.gx (graphix-fuzz check:
        // DIVERGENCE, 2 raises vs 1). The node-walk's own count also changes with
        // unrelated purity: adding `let c = count(s);` to the arm gives 1 on both
        // engines, which is the count CLAUDE.md's wake rule predicts (a reselected arm
        // reads standing values stale). Which re-entry is intended needs a ruling:
        // classifying a kernel PURE, joined with its feeders and keeping the recursion
        // check, makes both engines re-raise; counting a handler-ful `?` as impure
        // makes both read stale. (f-kernel-02)
        | NodeView::FusedKernel(_) => LambdaFacts::ASYNC,
        NodeView::Connect(c) => match own {
            OwnTargets::Bound(local) if !local(c.id) => LambdaFacts::PURE,
            OwnTargets::Bound(_) | OwnTargets::All => LambdaFacts::STATEFUL,
        },
        NodeView::ConnectDeref(_) => LambdaFacts::STATEFUL,
        NodeView::Qop(_) | NodeView::OrNever(_) => LambdaFacts::PURE,
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

/// The facts a call contributes: a callee instance of `graph` is still
/// being inferred, so it goes to `pending` and contributes nothing here;
/// an instance outside the analysis contributes its definition's stored
/// facts, a builtin its declared `EFFECT`, anything else `Async`.
// CR claude for eric: [bug] A builtin call contributes only its declared EFFECT, and a
// lambda passed to it counts as a PURE literal, so the effect of a callback the builtin
// calls (filter's predicate, opt::map's f, array::group's f) never reaches the caller's
// facts. `#[sync] let g = |v: i64| filter(v, |x| sys::time::after_idle(duration:10.ms,
// x > 0))` is accepted although g answers a cycle late, and the same g under #[async]
// is refused as sync. These builtins are already not stateless, so today only the
// #[sync]/#[async] verdicts are wrong. A builtin call's facts should join the facts of
// the function arguments it calls back. probe:
// design/review-2026-10-05/repro/core-lib-08.gx (core-lib-08)
// 2026-10-08 claude: re-addressed, a design call (one ruling covers core-aux-11 below). A
// builtin calls its callback through a call site bound at run time, so the check has no
// instance of the callback to analyze, and the definition check's own body is pessimistic
// (a call to a function parameter reads as async there). Options: (a) a builtin HOF call
// materializes its callback's instance at the check, as MapQ keeps a prototype, and the
// analysis walks it (memory and check time per call); (b) a builtin declares which
// arguments it calls back, and those arguments' facts join the call's, computed by a body
// walk at each literal's definition check where calls to parameters count as their
// declared types' effects (a new walk; named loss: wrong #[sync] verdicts and
// core-aux-11's map of len 1); (c) treat every function argument of a builtin as async
// unless it is a known pure definition (cheap, but de-classifies today's sync uses of
// opt::map and filter). I lean to (b).
fn callee_facts<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    graph: Option<&StaticCallGraph<'_, R, E>>,
    ctx: &CompileCtx<R, E>,
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
    // CR claude for eric: [bug] A call to a builtin contributes only the builtin's
    // declared EFFECT, either through the builtin-bodied def that bind_to_lambda names
    // (opt::map takes this branch) or through builtin_bindings below. The functions
    // handed to the builtin are never consulted, and a lambda literal argument counts
    // as PURE without its body being walked. So a Sync HOF builtin given an async
    // callback (opt::map, flat_map, filter, or_else, ok_or_else, is_some_and,
    // is_none_or, core filter, array::group) leaves its caller Sync: #[sync] passes and
    // #[async] is refused, while array::map or a Graphix HOF with the same callback is
    // refused. The same hole lets a core-trait method escape its implicit #[sync]: an
    // Ord impl whose cmp goes through opt::map with an after_idle callback compiles,
    // and a map of three distinct keys of that type has len 1. probe:
    // design/review-2026-10-05/repro/core-aux-11.gx (core-aux-11)
    // 2026-10-08 claude: re-addressed with core-lib-08 above: one ruling covers both.
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
            let effect = ctx.builtin_effect(info.name.as_str());
            return LambdaFacts {
                effect: effect.kind(),
                stateless: effect.is_stateless(),
            };
        }
    }
    if gxdbg_effect() {
        eprintln!("EFFECT-ASYNC-FALLBACK cs={}", cs.fnode().spec());
    }
    LambdaFacts::ASYNC
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
        let pure = facts
            .get(instance)
            .copied()
            .unwrap_or_else(|| {
                lambda_def(ctx, g.id()).map_or(LambdaFacts::ASYNC, def_facts)
            })
            .is_pure();
        let looped = pure
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

/// `n`'s own reads and writes into `s`, and the instances its calls
/// reach into `callees`. A write or read through a reference, a call
/// with no static target, a dynamic module, and a builtin handed a
/// function or a reference touch every variable.
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
        NodeView::FusedKernel(k) => k.runs_hooks(),
        _ => false,
    }
}

/// Whether anything in the region `n`, callees excluded, may run a
/// core-trait impl.
pub(crate) fn region_runs_hooks<R: Rt, E: UserEvent>(n: &Node<R, E>, env: &Env) -> bool {
    let mut runs = false;
    fusion::for_each_node(n, &mut |x| runs = runs || runs_hooks(x, env));
    runs
}

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
            if k.runs_hooks() {
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
        let accesses = accesses_in(b.children.iter(), graph, *holder, ctx);
        let plan = plan_runs(&b.children, &b.catches, &accesses, &mut |_, _| ());
        let _ = b.planned.set(plan);
    }
}

/// Whether siblings forked at a fork point other than a block's (a
/// constructor's fields, a call's arguments, an operator's operands, a
/// collection's slots) compute in parallel what they do in order: none
/// reads what an earlier one publishes, and no two make ordered calls.
/// A call not resolved yet is opaque, so this is decided before binds.
pub(crate) fn independent<'a, R: Rt, E: UserEvent>(
    children: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ctx: &CompileCtx<R, E>,
) -> bool {
    let accesses = accesses(children.clone(), ctx);
    let mut published = Vars::default();
    let mut ordered = false;
    for (n, access) in children.zip(accesses.iter()) {
        if access.reads.meeting(&published).is_some() || (access.ordered && ordered) {
            return false;
        }
        let mut refs = Refs::without_callees();
        n.refs(&mut refs);
        refs.with_bound(|id| {
            published.ids.insert(id);
        });
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
        let mut refs = Refs::without_callees();
        n.refs(&mut refs);
        refs.with_bound(|id| {
            published.ids.insert(id);
        });
        // a reference publishes its cell (and a moving place its path)
        // in the cycle it fires, which a later `*r` reads
        fusion::for_each_node(n, &mut |x| {
            if let NodeView::ByRef(b) = x.view() {
                published.ids.insert(b.id);
            }
        });
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
    // a constant, a variable read, a function literal or a declaration
    // is nothing to run beside something else
    let work = |n: &Node<R, E>| {
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
    };
    if let NodeView::Block(b) = target.view() {
        let two = |&(a, z): &(u32, u32)| {
            (a..z).filter(|i| work(&b.children[*i as usize])).count() >= 2
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
    let two =
        |ns: &mut dyn Iterator<Item = &Node<R, E>>| ns.filter(|n| work(n)).count() >= 2;
    let mut forks = false;
    fusion::for_each_node(target, &mut |n| {
        forks = forks
            || match n.view() {
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
                    slots || two(&mut cs.args.values().filter_map(|a| a.node.as_ref()))
                }
                NodeView::Struct(c) => two(&mut c.n.iter()),
                NodeView::Tuple(c) => two(&mut c.n.iter()),
                NodeView::Variant(c) => two(&mut c.n.iter()),
                NodeView::Array(c) => two(&mut c.n.iter()),
                NodeView::ListLit(c) => two(&mut c.n.iter()),
                NodeView::Map(c) => two(&mut c.n.iter()),
                NodeView::StringInterpolate(c) => two(&mut c.args.iter()),
                v => fusion::binary_operands(&v).is_some_and(|(l, r)| work(l) && work(r)),
            }
    });
    if !forks {
        crate::bailat!(spec, "#[parallel] has nothing to run in parallel")
    }
    Ok(())
}
