//! Phase counters for the interpreter's lazy-bind path, gated by
//! GRAPHIX_DBG_PERF=1; a background thread dumps them to stderr every
//! 250ms while they change.
use std::{
    sync::{
        LazyLock,
        atomic::{AtomicU64, Ordering::Relaxed},
    },
    time::{Duration, Instant},
};

// XCR claude for eric: kept apart from profile.rs on purpose: profile reports
// per root span at its close, so a runtime bind (no enclosing compile) would
// print a full PROFILE block per bind, while this sums binds across cycles.
// Folding needs a runtime-cumulative mode in profile first; your call.
pub(crate) static BIND_CALLS: AtomicU64 = AtomicU64::new(0);
pub(crate) static BIND_NS: AtomicU64 = AtomicU64::new(0);
pub(crate) static SETUP_NS: AtomicU64 = AtomicU64::new(0);
pub(crate) static TC1_NS: AtomicU64 = AtomicU64::new(0);
pub(crate) static ANALYZE_NS: AtomicU64 = AtomicU64::new(0);

pub(crate) fn enabled() -> bool {
    static E: LazyLock<bool> = LazyLock::new(|| {
        let on = crate::dbgenv::graphix_dbg_perf();
        if on {
            std::thread::spawn(dumper);
        }
        on
    });
    *E
}

pub(crate) struct Span {
    start: Instant,
    ctr: &'static AtomicU64,
}

impl Drop for Span {
    fn drop(&mut self) {
        self.ctr.fetch_add(self.start.elapsed().as_nanos() as u64, Relaxed);
    }
}

pub(crate) fn span(ctr: &'static AtomicU64) -> Option<Span> {
    enabled().then(|| Span { start: Instant::now(), ctr })
}

fn dumper() {
    let mut last = 0;
    loop {
        std::thread::sleep(Duration::from_millis(250));
        let sum = BIND_CALLS.load(Relaxed) + BIND_NS.load(Relaxed);
        if sum != last {
            last = sum;
            eprintln!(
                "PERF binds={} bind_ms={} setup_ms={} tc1_ms={} analyze_ms={}",
                BIND_CALLS.load(Relaxed),
                BIND_NS.load(Relaxed) / 1_000_000,
                SETUP_NS.load(Relaxed) / 1_000_000,
                TC1_NS.load(Relaxed) / 1_000_000,
                ANALYZE_NS.load(Relaxed) / 1_000_000,
            );
        }
    }
}
