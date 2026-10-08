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
    /// The instance whose elaboration built this one.
    parent: Option<LambdaInstanceId>,
    /// Wall time of this instance's elaboration, the instances it built
    /// included.
    elaboration_ns: u64,
}

#[derive(Default)]
struct Census {
    instances: LPooled<IntMap<LambdaInstanceId, Instance>>,
    signatures: LPooled<AHashMap<Arc<FnType>, usize>>,
    callbacks: LPooled<AHashMap<u64, usize>>,
    /// The instances whose elaboration is running, innermost last.
    elaborating: Vec<LambdaInstanceId>,
}

struct Profile {
    current: Option<Phase>,
    origin: Instant,
    last: Instant,
    epoch_ns: u128,
    metrics: [Metric; PHASES.len()],
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
    Check,
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
                let elaborating = census.and_then(|c| c.elaborating.last().copied());
                (phase, module, elaborating, census.is_some())
            });
            Self { fork, done: None }
        })
    }

    /// Run `f`, the task, on this thread under the profile it forked.
    pub fn run<T>(&mut self, f: impl FnOnce() -> T) -> T {
        let Some((phase, module, elaborating, census)) = self.fork.clone() else {
            return f();
        };
        let now = Instant::now();
        let fresh = Profile {
            current: Some(phase),
            origin: now,
            last: now,
            epoch_ns: 0,
            metrics: [Metric::default(); PHASES.len()],
            census: census.then(|| Census {
                elaborating: elaborating.into_iter().collect(),
                ..Census::default()
            }),
            module: module.is_some().then_some(0),
            modules: module.into_iter().map(|m| (m, [0; PHASES.len()])).collect(),
        };
        let saved = PROFILE.with_borrow_mut(|p| std::mem::replace(p, fresh));
        let r = f();
        let mut ran = PROFILE.with_borrow_mut(|p| std::mem::replace(p, saved));
        ran.switch(None, Instant::now());
        let census = ran.census.as_mut().map(|c| {
            (
                c.instances.drain().collect(),
                c.signatures.drain().collect(),
                c.callbacks.drain().collect(),
            )
        });
        self.done = Some(TaskCosts {
            metrics: ran.metrics,
            modules: std::mem::take(&mut ran.modules),
            census,
        });
        r
    }

    /// Merge what the task `self` cost into this thread's profile.
    pub fn join(self) {
        let Some(t) = self.done else { return };
        PROFILE.with_borrow_mut(|p| {
            if p.current.is_none() {
                return;
            }
            p.switch(p.current, Instant::now());
            for (m, tm) in p.metrics.iter_mut().zip(t.metrics.iter()) {
                m.calls += tm.calls;
                // a task's time is a finished descendant of whatever is open
                m.self_ns += tm.self_ns;
                m.completed_self_ns += tm.self_ns;
                m.total_ns += tm.total_ns;
                m.failed_calls += tm.failed_calls;
                m.failed_ns += tm.failed_ns;
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
                    row.elaboration_ns += t.elaboration_ns;
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
    let cost = match span.phase {
        Phase::InstanceGraph => InstanceCost::Graph,
        Phase::InstanceCheck => InstanceCost::Check,
        _ => return,
    };
    let _p = phase(Phase::InstanceCensus);
    span.instance = Some((id, cost));
    PROFILE.with_borrow_mut(|p| {
        let c = p.census.as_mut().unwrap();
        let parent = c.elaborating.last().copied();
        let row = c.instances.entry(id).or_default();
        if row.definition.is_none() {
            row.definition = Some(definition);
            row.label = format_compact!("{:?}:{}", body.ori.source, body.pos);
            row.parent = parent;
        }
    });
}

/// An instance's elaboration (its `typecheck1`, which builds the
/// instances its call sites reach), open while the guard lives.
#[doc(hidden)]
pub struct Elaboration(Option<(LambdaInstanceId, Instant)>);

#[doc(hidden)]
pub fn elaboration(id: LambdaInstanceId) -> Elaboration {
    if !graphix_profile_instances() {
        return Elaboration(None);
    }
    PROFILE.with_borrow_mut(|p| match p.census.as_mut() {
        Some(c) => {
            c.elaborating.push(id);
            Elaboration(Some((id, Instant::now())))
        }
        None => Elaboration(None),
    })
}

impl Drop for Elaboration {
    fn drop(&mut self) {
        let Some((id, start)) = self.0 else { return };
        let elapsed = start.elapsed().as_nanos() as u64;
        PROFILE.with_borrow_mut(|p| {
            if let Some(c) = p.census.as_mut() {
                c.elaborating.pop();
                c.instances.entry(id).or_default().elaboration_ns += elapsed;
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
                let row = p.census.as_mut().unwrap().instances.entry(id).or_default();
                match cost {
                    InstanceCost::Graph => {
                        row.graph_ns += self_ns;
                        row.graph_calls += 1;
                    }
                    InstanceCost::Check => {
                        row.check_ns += self_ns;
                        row.check_calls += 1;
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
                for (phase, m) in PHASES.iter().zip(&p.metrics) {
                    if m.calls > 0 {
                        eprintln!(
                            "PROFILE phase={phase:?} thread={thread:?} calls={} self_ns={} total_ns={} failed_calls={} failed_ns={}",
                            m.calls, m.self_ns, m.total_ns, m.failed_calls, m.failed_ns,
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
                            "INSTANCE thread={thread:?} root_ns={} id={} definition={} signature={} callbacks={} graph_calls={} check_calls={} graph_ns={} check_ns={} parent={} elaboration_ns={} label={:?}",
                            p.epoch_ns, id.inner(), i.definition.map_or(0, |d| d.inner()),
                            i.signature, i.callbacks, i.graph_calls, i.check_calls,
                            i.graph_ns, i.check_ns, i.parent.map_or(0, |d| d.inner()),
                            i.elaboration_ns, i.label,
                        );
                    }
                }
            }
        });
    }
}
