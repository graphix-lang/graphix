//! Process-wide phase counters gated by GRAPHIX_DBG_PERF=1. Fusion
//! reports deltas after each pass; a background thread reports lazy-bind
//! counters every 250ms while they change.
use std::{
    sync::{
        LazyLock,
        atomic::{AtomicU64, Ordering::Relaxed},
    },
    time::{Duration, Instant},
};

pub static FUSION_RETURN_NS: AtomicU64 = AtomicU64::new(0);
pub static FUSION_INPUTS_NS: AtomicU64 = AtomicU64::new(0);
pub static FUSION_BUILTINS_NS: AtomicU64 = AtomicU64::new(0);
pub static FUSION_CALLEES_NS: AtomicU64 = AtomicU64::new(0);
pub static FUSION_EMIT_NS: AtomicU64 = AtomicU64::new(0);

pub fn fusion_snapshot() -> [u64; 5] {
    [
        FUSION_RETURN_NS.load(Relaxed),
        FUSION_INPUTS_NS.load(Relaxed),
        FUSION_BUILTINS_NS.load(Relaxed),
        FUSION_CALLEES_NS.load(Relaxed),
        FUSION_EMIT_NS.load(Relaxed),
    ]
}

pub fn report_fusion(before: [u64; 5], total: Duration) {
    let after = fusion_snapshot();
    let ms: [f64; 5] = std::array::from_fn(|i| (after[i] - before[i]) as f64 / 1e6);
    eprintln!(
        "FUSION total_ms={:.3} return_ms={:.3} inputs_ms={:.3} builtins_ms={:.3} callees_ms={:.3} emit_ms={:.3}",
        total.as_secs_f64() * 1000.,
        ms[0],
        ms[1],
        ms[2],
        ms[3],
        ms[4]
    );
}

pub static BIND_CALLS: AtomicU64 = AtomicU64::new(0);
pub static BIND_NS: AtomicU64 = AtomicU64::new(0);
pub static SETUP_NS: AtomicU64 = AtomicU64::new(0);
pub static TC1_NS: AtomicU64 = AtomicU64::new(0);
pub static ANALYZE_NS: AtomicU64 = AtomicU64::new(0);
pub static TBO_NS: AtomicU64 = AtomicU64::new(0);
pub static PRIME_CALLS: AtomicU64 = AtomicU64::new(0);
pub static PRIME_NS: AtomicU64 = AtomicU64::new(0);
pub static REPLAY_NS: AtomicU64 = AtomicU64::new(0);
pub static CLONE_ENTRIES: AtomicU64 = AtomicU64::new(0);
pub static DELETE_NS: AtomicU64 = AtomicU64::new(0);
pub static REFS_NS: AtomicU64 = AtomicU64::new(0);

pub fn enabled() -> bool {
    static E: LazyLock<bool> = LazyLock::new(|| {
        let on = std::env::var_os("GRAPHIX_DBG_PERF").is_some();
        if on {
            std::thread::spawn(dumper);
        }
        on
    });
    *E
}

pub struct Span {
    start: Instant,
    ctr: &'static AtomicU64,
}

impl Drop for Span {
    fn drop(&mut self) {
        self.ctr.fetch_add(self.start.elapsed().as_nanos() as u64, Relaxed);
    }
}

pub fn span(ctr: &'static AtomicU64) -> Option<Span> {
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
                "PERF binds={} bind_ms={} setup_ms={} tc1_ms={} analyze_ms={} \
                 tbo_ms={} primes={} prime_ms={} replay_ms={} clone_entries={} \
                 delete_ms={} refs_ms={}",
                BIND_CALLS.load(Relaxed),
                BIND_NS.load(Relaxed) / 1_000_000,
                SETUP_NS.load(Relaxed) / 1_000_000,
                TC1_NS.load(Relaxed) / 1_000_000,
                ANALYZE_NS.load(Relaxed) / 1_000_000,
                TBO_NS.load(Relaxed) / 1_000_000,
                PRIME_CALLS.load(Relaxed),
                PRIME_NS.load(Relaxed) / 1_000_000,
                REPLAY_NS.load(Relaxed) / 1_000_000,
                CLONE_ENTRIES.load(Relaxed),
                DELETE_NS.load(Relaxed) / 1_000_000,
                REFS_NS.load(Relaxed) / 1_000_000,
            );
        }
    }
}
