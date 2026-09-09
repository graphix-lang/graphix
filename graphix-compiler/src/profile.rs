use crate::{
    FnArgIdentity, LambdaId, LambdaInstanceId,
    expr::Expr,
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
    sync::LazyLock,
    time::{Instant, SystemTime, UNIX_EPOCH},
};
use triomphe::Arc;

macro_rules! phases {
    ($($phase:ident),+ $(,)?) => {
        #[derive(Clone, Copy, Debug)]
        pub enum Phase { $($phase),+ }
        const PHASES: &[Phase] = &[$(Phase::$phase),+];
    };
}

phases! {
    Parse, Decode, Compile, BuildGraph, Typecheck0, Typecheck1, Settle,
    Analysis, CallGraph, ResolvedSites, Effects, Recursion, SeedTypes,
    Fusion, ReturnType, Inputs, Builtins, Callees, Emit, JitInit,
    JitBuild, Clif, BackendBody, BackendWrapper, BackendStub, BackendSpill,
    Finalize, Freeze, Normalize, ExpandRefs, StaticBind, InstanceGraph,
    InstanceCheck, ModuleCheck, ModuleSignature, LambdaFinalize, EffectRefs, EffectRound,
    InstanceCensus,
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
}

#[derive(Default)]
struct Census {
    instances: LPooled<IntMap<LambdaInstanceId, Instance>>,
    signatures: LPooled<AHashMap<Arc<FnType>, usize>>,
    callbacks: LPooled<AHashMap<FnArgIdentity, usize>>,
}

static CENSUS: LazyLock<bool> =
    LazyLock::new(|| std::env::var_os("GRAPHIX_PROFILE_INSTANCES").is_some());

struct Profile {
    current: Option<Phase>,
    origin: Instant,
    last: Instant,
    epoch_ns: u128,
    metrics: [Metric; PHASES.len()],
    census: Option<Census>,
}

thread_local! {
    static PROFILE: RefCell<Profile> = RefCell::new(Profile {
        current: None,
        origin: Instant::now(),
        last: Instant::now(),
        epoch_ns: 0,
        metrics: [Metric::default(); PHASES.len()],
        census: None,
    });
}

impl Profile {
    fn switch(&mut self, next: Option<Phase>, now: Instant) -> Option<Phase> {
        if let Some(current) = self.current {
            self.metrics[current as usize].self_ns +=
                now.duration_since(self.last).as_nanos() as u64;
        }
        self.last = now;
        std::mem::replace(&mut self.current, next)
    }
}

pub struct Span {
    phase: Phase,
    parent: Option<Phase>,
    start: Instant,
    failed: bool,
    active_self_ns: u64,
    instance: Option<LambdaInstanceId>,
    // The saved parent belongs to this thread, and spans must drop in LIFO order.
    thread: PhantomData<Rc<()>>,
}

pub fn phase(phase: Phase) -> Option<Span> {
    static ENABLED: LazyLock<bool> =
        LazyLock::new(|| std::env::var_os("GRAPHIX_PROFILE").is_some());
    if !*ENABLED {
        return None;
    }
    PROFILE.with_borrow_mut(|p| {
        if p.current.is_none() {
            p.metrics.fill(Metric::default());
            p.census = CENSUS.then(Census::default);
            p.epoch_ns = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        }
        let start = Instant::now();
        if p.current.is_none() {
            p.origin = start;
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

pub(crate) fn instance(
    span: &mut Option<Span>,
    id: LambdaInstanceId,
    definition: LambdaId,
    body: &Expr,
) {
    if !*CENSUS || span.is_none() {
        return;
    }
    let _p = phase(Phase::InstanceCensus);
    span.as_mut().unwrap().instance = Some(id);
    PROFILE.with_borrow_mut(|p| {
        let row = p.census.as_mut().unwrap().instances.entry(id).or_default();
        if row.definition.is_none() {
            row.definition = Some(definition);
            row.label = format_compact!("{:?}:{}", body.ori.source, body.pos);
        }
    });
}

pub(crate) fn instance_signature(
    id: LambdaInstanceId,
    typ: &FnType,
    identity: Option<&FnArgIdentity>,
) {
    if !*CENSUS {
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
        let callbacks = identity.map_or(0, |identity| {
            let next = c.callbacks.len() + 1;
            *c.callbacks.entry(identity.clone()).or_insert(next)
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
            if let Some(id) = self.instance {
                let row = p.census.as_mut().unwrap().instances.entry(id).or_default();
                match self.phase {
                    Phase::InstanceGraph => {
                        row.graph_ns += self_ns;
                        row.graph_calls += 1;
                    }
                    Phase::InstanceCheck => {
                        row.check_ns += self_ns;
                        row.check_calls += 1;
                    }
                    _ => unreachable!(),
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
                if let Some(c) = &p.census {
                    for (id, i) in c.instances.iter() {
                        eprintln!(
                            "INSTANCE thread={thread:?} root_ns={} id={} definition={} signature={} callbacks={} graph_calls={} check_calls={} graph_ns={} check_ns={} label={:?}",
                            p.epoch_ns, id.inner(), i.definition.map_or(0, |d| d.inner()),
                            i.signature, i.callbacks, i.graph_calls, i.check_calls,
                            i.graph_ns, i.check_ns, i.label,
                        );
                    }
                }
            }
        });
    }
}
