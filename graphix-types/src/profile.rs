use crate::{
    LambdaId, LambdaInstanceId,
    dbgenv::{graphix_profile, graphix_profile_instances},
    expr::{Expr, ModPath},
    typ::{FnType, Type},
};
use ahash::AHashMap;
use compact_str::{CompactString, format_compact};
use nohash::IntMap;
use poolshark::local::LPooled;
use std::{
    cell::RefCell,
    marker::PhantomData,
    rc::Rc,
    time::{Instant, SystemTime, UNIX_EPOCH},
};
use triomphe::Arc;

macro_rules! phases {
    ($($phase:ident),+ $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Phase { $($phase),+ }
        const PHASES: &[Phase] = &[$(Phase::$phase),+];
    };
}

phases! {
    Parse, Decode, Compile, BuildGraph, Typecheck0, Typecheck1, Settle,
    Analysis, CallGraph, ResolvedSites, Effects, Recursion, SeqPlan, SeedTypes,
    Fusion, ReturnType, Inputs, Builtins, Callees, Emit, JitInit,
    JitBuild, Clif, Link,
    Finalize, Freeze, Normalize, ExpandRefs, StaticBind, InstanceGraph,
    InstanceCheck, ModuleCheck, ModuleSignature, LambdaFinalize, EffectRefs, EffectRound,
    InstanceCensus, TaskFork, TaskJoin, ImageEnv, ImageDefs, ImageNodes,
    ImageEncode, ImageHeap, ImageTrailer,
}

#[derive(Clone, Copy, Default)]
struct Metric {
    calls: u64,
    self_ns: u64,
    total_ns: u64,
    failed_calls: u64,
    failed_ns: u64,
    completed_self_ns: u64,
}

impl Metric {
    fn merge(&mut self, other: &Metric) {
        self.calls += other.calls;
        self.self_ns += other.self_ns;
        self.total_ns += other.total_ns;
        self.failed_calls += other.failed_calls;
        self.failed_ns += other.failed_ns;
    }
}

#[derive(Default)]
struct Instance {
    definition: Option<LambdaId>,
    label: CompactString,
    graph_ns: u64,
    check_ns: u64,
    graph_calls: u64,
    check_calls: u64,
    signature: usize,
    callbacks: usize,
    /// The instance whose check or elaboration built this one.
    parent: Option<LambdaInstanceId>,
    /// Wall time while this instance was open, its check and its
    /// elaboration, the instances built and checked meanwhile included.
    open_ns: u64,
}

#[derive(Default)]
struct Census {
    instances: LPooled<IntMap<LambdaInstanceId, Instance>>,
    signatures: LPooled<AHashMap<Arc<FnType>, usize>>,
    callbacks: LPooled<AHashMap<u64, usize>>,
    /// The instances whose check or elaboration is running, innermost
    /// last.
    open: Vec<LambdaInstanceId>,
}

impl Census {
    /// Open `id`; whether this is its outermost opening, the one whose
    /// time counts.
    fn open(&mut self, id: LambdaInstanceId) -> bool {
        let outermost = !self.open.contains(&id);
        self.open.push(id);
        outermost
    }

    /// Close the innermost opening of `id`, `elapsed` after it opened.
    fn close(&mut self, id: LambdaInstanceId, outermost: bool, elapsed: u64) {
        debug_assert_eq!(self.open.last(), Some(&id), "instance closed out of order");
        self.open.pop();
        if outermost {
            self.instances.entry(id).or_default().open_ns += elapsed;
        }
    }
}

struct Profile {
    current: Option<Phase>,
    origin: Instant,
    last: Instant,
    epoch_ns: u128,
    metrics: [Metric; PHASES.len()],
    /// What the joined compile tasks cost on their own threads, by phase;
    /// `metrics` is this thread's wall time alone.
    tasks: [Metric; PHASES.len()],
    census: Option<Census>,
    /// The module whose code the running phase is working for, an index
    /// into `modules`; `None` is the root's own statements.
    module: Option<usize>,
    modules: Vec<(ModPath, [u64; PHASES.len()])>,
}

thread_local! {
    static PROFILE: RefCell<Profile> = RefCell::new(Profile {
        current: None,
        origin: Instant::now(),
        last: Instant::now(),
        epoch_ns: 0,
        metrics: [Metric::default(); PHASES.len()],
        tasks: [Metric::default(); PHASES.len()],
        census: None,
        module: None,
        modules: Vec::new(),
    });
}

impl Profile {
    fn switch(&mut self, next: Option<Phase>, now: Instant) -> Option<Phase> {
        if let Some(current) = self.current {
            let elapsed = now.duration_since(self.last).as_nanos() as u64;
            self.metrics[current as usize].self_ns += elapsed;
            if let Some(m) = self.module {
                self.modules[m].1[current as usize] += elapsed;
            }
        }
        self.last = now;
        std::mem::replace(&mut self.current, next)
    }
}

#[derive(Clone, Copy)]
enum InstanceCost {
    Graph,
    Check { outermost: bool },
}

pub struct Span {
    phase: Phase,
    parent: Option<Phase>,
    start: Instant,
    failed: bool,
    active_self_ns: u64,
    instance: Option<(LambdaInstanceId, InstanceCost)>,
    // The saved parent belongs to this thread, and spans must drop in LIFO order.
    thread: PhantomData<Rc<()>>,
}

pub fn phase(phase: Phase) -> Option<Span> {
    if !graphix_profile() {
        return None;
    }
    PROFILE.with_borrow_mut(|p| {
        let start = Instant::now();
        if p.current.is_none() {
            p.metrics.fill(Metric::default());
            p.tasks.fill(Metric::default());
            p.census = graphix_profile_instances().then(Census::default);
            p.epoch_ns = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
            p.origin = start;
            p.module = None;
            p.modules.clear();
        }
        let parent = p.switch(Some(phase), start);
        p.metrics[phase as usize].calls += 1;
        let m = &p.metrics[phase as usize];
        Some(Span {
            phase,
            parent,
            start,
            failed: false,
            thread: PhantomData,
            active_self_ns: m.self_ns - m.completed_self_ns,
            instance: None,
        })
    })
}

/// A compile task's share of its parent's profile: what was open where it
/// forked (the phase, the module, the instance being elaborated), entered
/// on the thread that runs the task, and its costs merged back at the
/// join, so a task's spans count under the parent's root and module.
#[derive(Default)]
pub struct Task {
    fork: Option<(Phase, Option<ModPath>, Option<LambdaInstanceId>, bool)>,
    done: Option<TaskCosts>,
}

struct TaskCosts {
    metrics: [Metric; PHASES.len()],
    tasks: [Metric; PHASES.len()],
    modules: Vec<(ModPath, [u64; PHASES.len()])>,
    /// The task's census, its rows numbering signatures and callbacks
    /// in its own interning.
    census: Option<(
        Vec<(LambdaInstanceId, Instance)>,
        Vec<(Arc<FnType>, usize)>,
        Vec<(u64, usize)>,
    )>,
}

impl Task {
    /// Where a task forks from this thread's open profile.
    pub fn fork() -> Self {
        if !graphix_profile() {
            return Self::default();
        }
        PROFILE.with_borrow(|p| {
            let fork = p.current.map(|phase| {
                let module = p.module.map(|m| p.modules[m].0.clone());
                let census = p.census.as_ref();
                let open = census.and_then(|c| c.open.last().copied());
                (phase, module, open, census.is_some())
            });
            Self { fork, done: None }
        })
    }

    /// Run `f`, the task, on this thread under the profile it forked.
    pub fn run<T>(&mut self, f: impl FnOnce() -> T) -> T {
        let Some((phase, module, open, census)) = self.fork.clone() else {
            return f();
        };
        let now = Instant::now();
        let fresh = Profile {
            current: Some(phase),
            origin: now,
            last: now,
            epoch_ns: 0,
            metrics: [Metric::default(); PHASES.len()],
            tasks: [Metric::default(); PHASES.len()],
            census: census.then(|| Census {
                open: open.into_iter().collect(),
                ..Census::default()
            }),
            module: module.is_some().then_some(0),
            modules: module.into_iter().map(|m| (m, [0; PHASES.len()])).collect(),
        };
        // the outgoing profile is charged up to the task, and its clock
        // restarts after it: the task's costs reach it by the join alone
        let saved = PROFILE.with_borrow_mut(|p| {
            p.switch(p.current, now);
            std::mem::replace(p, fresh)
        });
        let r = f();
        let now = Instant::now();
        let mut ran = PROFILE.with_borrow_mut(|p| {
            let mut ran = std::mem::replace(p, saved);
            p.last = now;
            ran.switch(None, now);
            ran
        });
        let census = ran.census.as_mut().map(|c| {
            (
                c.instances.drain().collect(),
                c.signatures.drain().collect(),
                c.callbacks.drain().collect(),
            )
        });
        self.done = Some(TaskCosts {
            metrics: ran.metrics,
            tasks: ran.tasks,
            modules: std::mem::take(&mut ran.modules),
            census,
        });
        r
    }

    /// Merge what the task `self` cost into this thread's task costs.
    pub fn join(self) {
        let Some(t) = self.done else { return };
        PROFILE.with_borrow_mut(|p| {
            if p.current.is_none() {
                return;
            }
            for (m, (tm, tt)) in p.tasks.iter_mut().zip(t.metrics.iter().zip(&t.tasks)) {
                m.merge(tm);
                m.merge(tt);
            }
            for (path, costs) in t.modules {
                match p.modules.iter_mut().find(|(m, _)| *m == path) {
                    Some((_, c)) => c.iter_mut().zip(costs).for_each(|(a, b)| *a += b),
                    None => p.modules.push((path, costs)),
                }
            }
            if let (Some(c), Some((rows, sigs, cbs))) = (p.census.as_mut(), t.census) {
                let mut sig: IntMap<usize, usize> = IntMap::default();
                for (typ, i) in sigs {
                    let next = c.signatures.len() + 1;
                    sig.insert(i, *c.signatures.entry(typ).or_insert(next));
                }
                let mut cb: IntMap<usize, usize> = IntMap::default();
                for (identity, i) in cbs {
                    let next = c.callbacks.len() + 1;
                    cb.insert(i, *c.callbacks.entry(identity).or_insert(next));
                }
                for (id, t) in rows {
                    let row = c.instances.entry(id).or_default();
                    if row.definition.is_none() {
                        row.definition = t.definition;
                        row.label = t.label;
                        row.parent = t.parent;
                    }
                    row.graph_ns += t.graph_ns;
                    row.check_ns += t.check_ns;
                    row.graph_calls += t.graph_calls;
                    row.check_calls += t.check_calls;
                    row.open_ns += t.open_ns;
                    if let Some(s) = sig.get(&t.signature) {
                        row.signature = *s;
                    }
                    if let Some(c) = cb.get(&t.callbacks) {
                        row.callbacks = *c;
                    }
                }
            }
        });
    }
}

/// Attributes the time until the guard drops to the module `path`
/// (innermost wins), inside a root span only.
pub struct ModuleSpan {
    parent: Option<usize>,
    thread: PhantomData<Rc<()>>,
}

#[doc(hidden)]
pub fn module(path: &ModPath) -> Option<ModuleSpan> {
    if !graphix_profile() {
        return None;
    }
    PROFILE.with_borrow_mut(|p| {
        p.current?;
        p.switch(p.current, Instant::now());
        let i = match p.modules.iter().position(|(m, _)| m == path) {
            Some(i) => i,
            None => {
                p.modules.push((path.clone(), [0; PHASES.len()]));
                p.modules.len() - 1
            }
        };
        let parent = p.module.replace(i);
        Some(ModuleSpan { parent, thread: PhantomData })
    })
}

impl Drop for ModuleSpan {
    fn drop(&mut self) {
        PROFILE.with_borrow_mut(|p| {
            p.switch(p.current, Instant::now());
            p.module = self.parent;
        })
    }
}

#[doc(hidden)]
pub fn instance(
    span: &mut Option<Span>,
    id: LambdaInstanceId,
    definition: LambdaId,
    body: &Expr,
) {
    let Some(span) = span.as_mut().filter(|_| graphix_profile_instances()) else {
        return;
    };
    if !matches!(span.phase, Phase::InstanceGraph | Phase::InstanceCheck) {
        return;
    }
    let _p = phase(Phase::InstanceCensus);
    span.instance = PROFILE.with_borrow_mut(|p| {
        let c = p.census.as_mut().unwrap();
        // XCR claude for claude: [bug/low] `elaborating` is pushed only by
        // profile::elaboration (GXLambda::typecheck1), but a row is created here by the
        // instance's InstanceGraph/InstanceCheck span, when no elaboration guard is
        // open. Every INSTANCE line prints parent=0 (267 of 267 for table_basic.gx
        // under --expand, serial too). bench/instances.py's tree() then reports 'chain
        // depth 1, widest 0 children' for every root, and its critical path and Nx
        // figures mean nothing. Fix: read the parent from a census stack of the open
        // spans' instance ids (pushed here, popped when the span drops), or delete
        // tree(), parent, elaboration_ns and the Elaboration guard. probe:
        // design/review-2026-10-26/repro/ide-tooling-instances-flat-tree.sh
        // (ide-tooling-07)
        // 2026-10-27 claude: Census::open replaces elaborating: an instance is open
        // while its InstanceCheck span (profile::instance pushes, Span::drop pops) or
        // its Elaboration guard lives, and a new row's parent is the innermost open
        // instance. elaboration_ns became open_ns, the outermost opening's wall time
        // (check and elaboration, the instances built meanwhile included);
        // bench/instances.py's tree() reads graph_ns + open_ns. table_basic --expand:
        // 45 of 267 rows now have a parent (0 before). Pin:
        // graphix-shell/tests/profile_instances.rs (a HOF callback's instance names the
        // instance of the function whose check built it; fails on the old profile.rs).
        let parent = c.open.last().copied();
        let row = c.instances.entry(id).or_default();
        if row.definition.is_none() {
            row.definition = Some(definition);
            row.label = format_compact!("{:?}:{}", body.ori.source, body.pos);
            row.parent = parent;
        }
        // the id exists only once the graph is built, so nothing opens under it
        let cost = match span.phase {
            Phase::InstanceCheck => InstanceCost::Check { outermost: c.open(id) },
            _ => InstanceCost::Graph,
        };
        Some((id, cost))
    });
}

/// An instance's elaboration (its `typecheck1`, which builds the
/// instances its call sites reach), open while the guard lives.
#[doc(hidden)]
pub struct Elaboration(Option<(LambdaInstanceId, bool, Instant)>);

#[doc(hidden)]
pub fn elaboration(id: LambdaInstanceId) -> Elaboration {
    if !graphix_profile_instances() {
        return Elaboration(None);
    }
    PROFILE.with_borrow_mut(|p| match p.census.as_mut() {
        Some(c) => Elaboration(Some((id, c.open(id), Instant::now()))),
        None => Elaboration(None),
    })
}

impl Drop for Elaboration {
    fn drop(&mut self) {
        let Some((id, outermost, start)) = self.0 else { return };
        let elapsed = start.elapsed().as_nanos() as u64;
        PROFILE.with_borrow_mut(|p| {
            if let Some(c) = p.census.as_mut() {
                c.close(id, outermost, elapsed)
            }
        });
    }
}

/// `identity` hashes the call's instantiation identity, when it has one.
#[doc(hidden)]
pub fn instance_signature(
    id: LambdaInstanceId,
    typ: &FnType,
    identity: impl FnOnce() -> Option<u64>,
) {
    if !graphix_profile_instances() {
        return;
    }
    let Some(_p) = phase(Phase::InstanceCensus) else { return };
    let typ = Arc::new(typ.resolve_tvars());
    let closed = Type::Fn(typ.clone()).tvar_free();
    PROFILE.with_borrow_mut(|p| {
        let c = p.census.as_mut().unwrap();
        let signature = if closed {
            let next = c.signatures.len() + 1;
            *c.signatures.entry(typ).or_insert(next)
        } else {
            0
        };
        let callbacks = identity().map_or(0, |identity| {
            let next = c.callbacks.len() + 1;
            *c.callbacks.entry(identity).or_insert(next)
        });
        let row = c.instances.entry(id).or_default();
        row.signature = signature;
        row.callbacks = callbacks;
    });
}

pub fn failed(span: &mut Option<Span>) {
    if let Some(span) = span {
        span.failed = true;
    }
}

impl Drop for Span {
    fn drop(&mut self) {
        let end = Instant::now();
        PROFILE.with_borrow_mut(|p| {
            debug_assert_eq!(p.current, Some(self.phase), "profile span dropped out of order");
            p.switch(self.parent, end);
            let elapsed = end.duration_since(self.start).as_nanos() as u64;
            let metric = &mut p.metrics[self.phase as usize];
            // Completed descendants account for their own exclusive time,
            // including descendants in this same phase.
            let self_ns = metric.self_ns - metric.completed_self_ns - self.active_self_ns;
            metric.completed_self_ns += self_ns;
            metric.total_ns += elapsed;
            if self.failed {
                metric.failed_calls += 1;
                metric.failed_ns += elapsed;
            }
            if let Some((id, cost)) = self.instance {
                let c = p.census.as_mut().unwrap();
                let row = c.instances.entry(id).or_default();
                match cost {
                    InstanceCost::Graph => {
                        row.graph_ns += self_ns;
                        row.graph_calls += 1;
                    }
                    InstanceCost::Check { outermost } => {
                        row.check_ns += self_ns;
                        row.check_calls += 1;
                        c.close(id, outermost, elapsed);
                    }
                }
            }
            if matches!(self.parent, Some(Phase::Compile)) {
                let start_ns = p.epoch_ns + self.start.duration_since(p.origin).as_nanos();
                eprintln!(
                    "PROFILE interval={:?} thread={:?} start_ns={start_ns} duration_ns={elapsed}",
                    self.phase, std::thread::current().id(),
                );
            }
            if self.parent.is_none() {
                let thread = std::thread::current().id();
                eprintln!(
                    "PROFILE root={:?} thread={thread:?} start_ns={} duration_ns={elapsed}",
                    self.phase, p.epoch_ns,
                );
                for (phase, (m, t)) in PHASES.iter().zip(p.metrics.iter().zip(&p.tasks)) {
                    if m.calls > 0 || t.self_ns > 0 {
                        eprintln!(
                            "PROFILE phase={phase:?} thread={thread:?} calls={} self_ns={} total_ns={} failed_calls={} failed_ns={} task_calls={} task_ns={} task_failed_calls={} task_failed_ns={}",
                            m.calls, m.self_ns, m.total_ns, m.failed_calls, m.failed_ns,
                            t.calls, t.self_ns, t.failed_calls, t.failed_ns,
                        );
                    }
                }
                for (path, costs) in p.modules.iter() {
                    for (phase, ns) in PHASES.iter().zip(costs) {
                        if *ns > 0 {
                            eprintln!(
                                "PROFILE module={path} thread={thread:?} mphase={phase:?} self_ns={ns}",
                            );
                        }
                    }
                }
                if let Some(c) = &p.census {
                    for (id, i) in c.instances.iter() {
                        eprintln!(
                            "INSTANCE thread={thread:?} root_ns={} id={} definition={} signature={} callbacks={} graph_calls={} check_calls={} graph_ns={} check_ns={} parent={} open_ns={} label={:?}",
                            p.epoch_ns, id.inner(), i.definition.map_or(0, |d| d.inner()),
                            i.signature, i.callbacks, i.graph_calls, i.check_calls,
                            i.graph_ns, i.check_ns, i.parent.map_or(0, |d| d.inner()),
                            i.open_ns, i.label,
                        );
                    }
                }
            }
        });
    }
}
