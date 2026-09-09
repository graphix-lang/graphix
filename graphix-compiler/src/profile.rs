use std::{
    cell::RefCell,
    marker::PhantomData,
    rc::Rc,
    sync::LazyLock,
    time::{Instant, SystemTime, UNIX_EPOCH},
};

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
}

#[derive(Clone, Copy, Default)]
struct Metric {
    calls: u64,
    self_ns: u64,
    total_ns: u64,
    failed_calls: u64,
    failed_ns: u64,
}

struct Profile {
    current: Option<Phase>,
    origin: Instant,
    last: Instant,
    epoch_ns: u128,
    metrics: [Metric; PHASES.len()],
}

thread_local! {
    static PROFILE: RefCell<Profile> = RefCell::new(Profile {
        current: None,
        origin: Instant::now(),
        last: Instant::now(),
        epoch_ns: 0,
        metrics: [Metric::default(); PHASES.len()],
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
            p.epoch_ns = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        }
        let start = Instant::now();
        if p.current.is_none() {
            p.origin = start;
        }
        let parent = p.switch(Some(phase), start);
        p.metrics[phase as usize].calls += 1;
        Some(Span { phase, parent, start, failed: false, thread: PhantomData })
    })
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
            metric.total_ns += elapsed;
            if self.failed {
                metric.failed_calls += 1;
                metric.failed_ns += elapsed;
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
            }
        });
    }
}
