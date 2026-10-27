//! The parallel evaluator's cost model (`design/parallel_eval.md` §5):
//! what a fork point measures of its children under [`ParMode::Auto`],
//! and where it forks.

use crate::{ExecCtx, Node, Rt, UserEvent, analysis, branch::saturated};
use bytes::{Buf, BufMut};
use graphix_types::stack::ParMode;
use netidx_core::pack::{Pack, PackError};
use parking_lot::Mutex;
use std::{
    sync::{
        LazyLock, OnceLock,
        atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering},
    },
    time::{Duration, Instant},
};

/// The platform's cheapest monotonic counter. Ticks are never converted
/// to time: the fork threshold is calibrated in ticks.
#[inline(always)]
pub fn ticks() -> u64 {
    #[cfg(target_arch = "x86_64")]
    {
        // SAFETY: rdtsc has no preconditions on x86_64.
        unsafe { core::arch::x86_64::_rdtsc() }
    }
    #[cfg(target_arch = "aarch64")]
    {
        let t: u64;
        // SAFETY: the virtual counter is readable from user mode.
        unsafe {
            core::arch::asm!("mrs {}, cntvct_el0", out(reg) t, options(nomem, nostack))
        };
        t
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        static START: std::sync::LazyLock<std::time::Instant> =
            std::sync::LazyLock::new(std::time::Instant::now);
        START.elapsed().as_nanos() as u64
    }
}

/// The ticks since `t0`; a counter that reads backwards (a thread moved
/// between unsynchronised TSCs) reads as none.
#[inline(always)]
pub fn ticks_since(t0: u64) -> u64 {
    ticks().saturating_sub(t0)
}

/// The fork threshold `t`: a side of a fork must be estimated at `t`
/// ticks or more. Buckets are `log2(ticks) - shift`, with `t` in bucket
/// [`T_BUCKET`].
#[derive(Debug, Clone, Copy)]
pub struct Calibration {
    pub t: u64,
    shift: u32,
}

/// The bucket `T` falls in: bucket 0 starts between `T/8192` and
/// `T/4096`, near the cost of a cheap slot, and bucket 15 at `8T`.
const T_BUCKET: u32 = 12;

/// How many threshold multiples of a stolen job's start latency.
const T_PER_WAKE: u64 = 4;

/// The least threshold, in time: the counter's rate varies by three
/// orders of magnitude between hosts.
const T_FLOOR: Duration = Duration::from_micros(1);

static CALIBRATION: OnceLock<Calibration> = OnceLock::new();

/// The calibration, or `None` while it is being measured or when it
/// could not be (no thread to measure on): until then nothing forks
/// under `Auto`, and nothing waits for it. The first call starts the
/// measurement, unless [`start_calibration`] did.
pub fn calibration() -> Option<&'static Calibration> {
    if let Some(c) = CALIBRATION.get() {
        return Some(c);
    }
    start_calibration();
    CALIBRATION.get()
}

/// Start measuring the fork threshold on a thread of its own, once per
/// process (`GRAPHIX_PAR_T` fixes it instead). A runtime that runs under
/// `Auto` starts it when it is constructed, so the measurement is
/// usually over by its first fork point.
pub fn start_calibration() {
    static STARTED: AtomicBool = AtomicBool::new(false);
    static FIXED: LazyLock<Option<u64>> = LazyLock::new(|| {
        graphix_types::stack::env_count("GRAPHIX_PAR_T", 0, "measuring the threshold")
    });
    if let Some(t) = *FIXED {
        return fix_calibration(t);
    }
    if !STARTED.load(Ordering::Relaxed) && !STARTED.swap(true, Ordering::Relaxed) {
        let spawned = std::thread::Builder::new()
            .name("graphix-par-calibrate".into())
            .spawn(|| {
                if let Some(c) = calibrate() {
                    let _ = CALIBRATION.set(c);
                }
            });
        if let Err(e) = spawned {
            log::warn!(
                "the fork threshold is not measured, nothing forks under Auto: {e}"
            )
        }
    }
}

/// Fix the fork threshold at `t` ticks for the process, in place of the
/// measurement, unless one landed already. A tiny `t` makes every
/// `Auto` site probe and then fork (the fuzzer's `Auto` runs).
pub fn fix_calibration(t: u64) {
    let t = t.max(1);
    let shift = (63 - t.leading_zeros()).saturating_sub(T_BUCKET);
    let _ = CALIBRATION.set(Calibration { t, shift });
}

/// The workers of the pool the calibration measures, at most.
const CALIBRATION_WORKERS: usize = 2;

/// The latency, in ticks, from handing an idle evaluation pool a job to
/// the job starting: the cost a fork pays when its right side is
/// stolen. `None` when no pool to measure could be built.
fn calibrate() -> Option<Calibration> {
    // a pool of its own, which nothing else forks onto
    let workers = CALIBRATION_WORKERS.min(graphix_types::stack::eval_threads());
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(workers)
        .thread_name(|_| "graphix-par-calibrate-worker".into())
        .build()
        .inspect_err(|e| {
            log::warn!(
                "the fork threshold is not measured, nothing forks under Auto: {e}"
            )
        })
        .ok()?;
    let (tick0, start) = (ticks(), Instant::now());
    let mut wakes = [0u64; 9];
    for w in wakes.iter_mut() {
        // let the worker park
        std::thread::sleep(Duration::from_millis(2));
        let t0 = ticks();
        *w = pool.install(ticks).saturating_sub(t0);
    }
    wakes.sort_unstable();
    // contention only adds to a wake: the lower quartile is the pool's
    let per_ns = ticks_since(tick0) as f64 / start.elapsed().as_nanos().max(1) as f64;
    let t = threshold(wakes[wakes.len() / 4], per_ns);
    if crate::dbgenv::graphix_dbg_par() {
        eprintln!("PAR calibrated: wakes {wakes:?} ticks, {per_ns:.3} ticks/ns, T = {t}");
    }
    Some(Calibration { t, shift: (63 - t.leading_zeros()).saturating_sub(T_BUCKET) })
}

/// The fork threshold for a pool that wakes in `wake` ticks, on a counter
/// running at `per_ns` ticks a nanosecond.
fn threshold(wake: u64, per_ns: f64) -> u64 {
    let floor = (T_FLOOR.as_nanos() as f64 * per_ns) as u64;
    (wake * T_PER_WAKE).max(floor).max(1)
}

impl Calibration {
    fn bucket(&self, ticks: u64) -> usize {
        let log = 63u32.saturating_sub(ticks.max(1).leading_zeros());
        log.saturating_sub(self.shift).min(15) as usize
    }

    /// The least tick count bucket `b` holds.
    fn floor(&self, b: u8) -> u64 {
        1u64 << (b as u32 + self.shift)
    }
}

/// A log2 histogram of tick counts. Old samples fade: the counts halve
/// whenever their total reaches [`Hist::DECAY`].
#[derive(Debug, Default, Clone)]
struct Hist {
    counts: [u16; 16],
    n: u16,
}

impl Hist {
    const DECAY: u16 = 64;

    fn add(&mut self, cal: &Calibration, ticks: u64) {
        self.counts[cal.bucket(ticks)] += 1;
        self.n += 1;
        if self.n >= Self::DECAY {
            self.n = 0;
            for c in self.counts.iter_mut() {
                *c /= 2;
                self.n += *c;
            }
        }
    }

    /// The bucket holding the 75th percentile.
    fn p75(&self) -> u8 {
        let want = (self.n as u32 * 3).div_ceil(4).max(1);
        let mut seen = 0u32;
        for (b, c) in self.counts.iter().enumerate() {
            seen += *c as u32;
            if seen >= want {
                return b as u8;
            }
        }
        15
    }
}

/// Samples a site takes before its estimates decide anything.
const SETTLE: u16 = 4;

/// Updates between re-probes of a site no fork can pay at.
const RECHECK: u16 = 1024;

/// The longest sampling period at a settled site is `2^MAX_PERIOD`.
const MAX_PERIOD: u8 = 8;

/// Probes a fresh site takes before it is judged serial.
const PROBES: u8 = 2;

/// A sampling schedule: due every update until settled, then the period
/// doubles while the decision holds and drops to one when it changes.
#[derive(Debug, Default)]
struct Sampler {
    countdown: u16,
    period: u8,
}

impl Sampler {
    fn due(&mut self) -> bool {
        match self.countdown.checked_sub(1) {
            None => true,
            Some(c) => {
                self.countdown = c;
                false
            }
        }
    }

    fn sampled(&mut self, settled: bool, changed: bool) {
        self.period = match (settled, changed) {
            (false, _) | (true, true) => 0,
            (true, false) => (self.period + 1).min(MAX_PERIOD),
        };
        self.countdown = (1u16 << self.period) - 1;
    }
}

/// What a fork point does this update.
pub enum Plan<'a> {
    /// Update the children in order.
    Serial,
    /// Update the children in order, timing each, then [`Meter::done`].
    Measure(Meter<'a>),
    /// Fork where [`Splits::split`] says.
    Fork(Splits<'a>),
}

/// A fork point over children that differ (a block's run, a call's
/// arguments, a constructor's fields, an operator's operands): its
/// state under `Auto`.
#[derive(Debug, Default)]
pub struct ForkSite(Site, pub(crate) Siblings);

/// Whether a fork point's children may run in parallel at all
/// ([`crate::analysis::independent`]): decided once, by the analysis
/// that reached the fork point or else at its first update. A question
/// whose answer rests on a call not bound yet is asked again only once
/// the runtime has bound something since ([`crate::Control::rebuilds`]).
/// Children not known independent run in order.
#[derive(Debug, Default)]
pub struct Siblings {
    decided: OnceLock<bool>,
    /// The runtime's rebuild count when the question was last left open.
    open_at: Option<u64>,
}

impl Siblings {
    /// Whether the siblings are known independent.
    pub fn independent(&self) -> bool {
        self.decided.get() == Some(&true)
    }

    pub(crate) fn decided(&self) -> bool {
        self.decided.get().is_some()
    }

    /// Take the analysis's verdict, unless one stands.
    pub(crate) fn decided_by_analysis(&self, independent: Option<bool>) {
        if let Some(v) = independent {
            let _ = self.decided.set(v);
        }
    }

    /// Decide whether `n` siblings are independent, unless that is
    /// decided, or open with nothing bound since it was asked.
    pub(crate) fn decide<'a, R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
        children: impl Iterator<Item = &'a Node<R, E>> + Clone,
        sites: analysis::Sites,
    ) {
        if self.decided() || n < 2 || ctx.fork_mode() == ParMode::Off {
            return;
        }
        let rebuilds = ctx.control.rebuilds();
        if self.open_at == Some(rebuilds) {
            return;
        }
        if !analysis::two_work(children.clone()) {
            let _ = self.decided.set(true);
            return;
        }
        ctx.control.verdict_asked();
        match analysis::independent(children, ctx, sites) {
            Some(v) => {
                let _ = self.decided.set(v);
            }
            None => self.open_at = Some(rebuilds),
        }
    }
}

#[derive(Debug)]
enum Site {
    /// Fresh: the next updates measure the children's total.
    Probe {
        left: u8,
    },
    /// No fork pays here; probe again after `countdown` updates.
    Serial {
        countdown: u16,
    },
    Measured(Box<Measured>),
}

impl Default for Site {
    fn default() -> Self {
        Site::Probe { left: PROBES }
    }
}

#[derive(Debug)]
struct Measured {
    hist: Box<[Hist]>,
    /// Each child's p75 bucket at the last sample.
    p75: Box<[u8]>,
    /// Prefix sums of the children's estimates: `prefix[i]` is the
    /// estimate of children `0..i`.
    prefix: Box<[u64]>,
    sampler: Sampler,
    settled: bool,
}

impl ForkSite {
    /// The site of a run a block plan made, whose statements the plan
    /// already found independent.
    pub fn planned() -> Self {
        let site = Self::default();
        site.1.decided_by_analysis(Some(true));
        site
    }

    /// Decide whether the site's `n` children are independent
    /// ([`Siblings::decide`]); dependent ones always run in order.
    #[inline]
    pub fn decide_siblings<'a, R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
        children: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ) {
        self.1.decide(ctx, n, children, analysis::Sites::Statements)
    }

    /// The site's verdict, which an image carries: a warm start's first
    /// cycle decides as the cold compile's analysis did.
    pub(crate) fn image_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.1.decided.get().copied().encode(buf)
    }

    /// A fresh site holding the verdict [`Self::image_encode`] wrote.
    pub(crate) fn image_decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        let site = Self::default();
        site.1.decided_by_analysis(Option::<bool>::decode(buf)?);
        Ok(site)
    }

    /// What this update does with the site's `n` children.
    #[inline]
    pub fn plan<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
    ) -> Plan<'_> {
        match ctx.fork_mode() {
            ParMode::Off => Plan::Serial,
            _ if n < 2 || !self.1.independent() => Plan::Serial,
            ParMode::Force => Plan::Fork(Splits::Halves),
            ParMode::Auto => self.plan_auto(n),
        }
    }

    fn plan_auto(&mut self, n: usize) -> Plan<'_> {
        match calibration() {
            None => Plan::Serial,
            Some(cal) => self.plan_with(cal, n),
        }
    }

    fn plan_with(&mut self, cal: &'static Calibration, n: usize) -> Plan<'_> {
        match &mut self.0 {
            Site::Probe { .. } => Plan::Measure(Meter::new(self, cal, n)),
            Site::Serial { countdown } => match countdown.checked_sub(1) {
                Some(c) => {
                    *countdown = c;
                    Plan::Serial
                }
                None => {
                    self.0 = Site::Probe { left: PROBES };
                    Plan::Measure(Meter::new(self, cal, n))
                }
            },
            Site::Measured(m) if m.hist.len() != n => {
                self.0 = Site::default();
                Plan::Measure(Meter::new(self, cal, n))
            }
            Site::Measured(m) => {
                if m.sampler.due() || !m.settled {
                    return Plan::Measure(Meter::new(self, cal, n));
                }
                if saturated() {
                    return Plan::Serial;
                }
                let Site::Measured(m) = &self.0 else { unreachable!() };
                Plan::Fork(Splits::Weighted { prefix: &m.prefix, t: cal.t })
            }
        }
    }
}

/// Times a measured update's children.
pub struct Meter<'a> {
    site: &'a mut ForkSite,
    cal: &'static Calibration,
    total: u64,
}

impl<'a> Meter<'a> {
    fn new(site: &'a mut ForkSite, cal: &'static Calibration, n: usize) -> Self {
        if let Site::Measured(m) = &site.0 {
            debug_assert_eq!(m.hist.len(), n);
        }
        Self { site, cal, total: 0 }
    }

    /// Run child `i`'s update `f`, timed.
    #[inline]
    pub fn time<T>(&mut self, i: usize, f: impl FnOnce() -> T) -> T {
        let t0 = ticks();
        let r = f();
        self.timed(i, ticks_since(t0));
        r
    }

    /// Child `i`'s update took `dt` ticks.
    fn timed(&mut self, i: usize, dt: u64) {
        self.total += dt;
        if let Site::Measured(m) = &mut self.site.0 {
            m.hist[i].add(self.cal, dt);
        }
    }

    /// The update is over: revise the estimates and the schedule.
    pub fn done(self, n: usize) {
        let cal = self.cal;
        match &mut self.site.0 {
            Site::Probe { left } => {
                if self.total >= 2 * cal.t {
                    self.site.0 = Site::Measured(Box::new(Measured {
                        hist: vec![Hist::default(); n].into(),
                        p75: vec![0; n].into(),
                        prefix: vec![0; n + 1].into(),
                        sampler: Sampler::default(),
                        settled: false,
                    }));
                } else if *left <= 1 {
                    self.site.0 = Site::Serial { countdown: RECHECK };
                } else {
                    *left -= 1;
                }
            }
            Site::Serial { .. } => unreachable!("a serial site is not measured"),
            Site::Measured(m) => {
                let mut changed = false;
                for (i, h) in m.hist.iter().enumerate() {
                    let b = h.p75();
                    changed |= m.p75[i] != b;
                    m.p75[i] = b;
                    // a child the meter never timed (an omitted default) costs nothing
                    let est = if h.n == 0 { 0 } else { cal.floor(b) };
                    m.prefix[i + 1] = m.prefix[i] + est;
                }
                m.settled |= m.hist.iter().all(|h| h.n == 0 || h.n >= SETTLE);
                // CR claude for eric: [perf/medium] A settled site resets its sampling
                // period only when a p75 bucket moves. After its children turn cheap,
                // each sample (1 in 256 updates) lands far below the p75 without moving
                // it until cheap samples are about 3x the heavy ones, so the site forks
                // cheap children on every update for roughly 7700 updates. Treat this
                // update's own times disagreeing with the estimates as a change: a
                // child's sample two or more buckets from its p75, or a total under 2T
                // at a site that says it pays. That resets the period to 0, so the
                // histogram turns over in tens of updates. SlotSite::measured_with has
                // the same `e != *est` trigger. probe:
                // design/review-2026-10-26/repro/c-cost-misc-fork-fade.gx (cheap window
                // 0.15 s after a heavy phase against 0.055 s without one or under Off;
                // 0.048 s with forking disabled). (c-cost-misc-01)
                m.sampler.sampled(m.settled, changed);
                let pays = Splits::Weighted { prefix: &m.prefix, t: cal.t }
                    .split(0, n)
                    .is_some();
                if m.settled && !pays {
                    self.site.0 = Site::Serial { countdown: RECHECK };
                }
            }
        }
    }
}

/// Where a forking update splits its children.
#[derive(Clone, Copy)]
pub enum Splits<'a> {
    /// Every range splits in half (`Force`).
    Halves,
    /// A range splits at its estimated midpoint when both sides reach `t`.
    Weighted { prefix: &'a [u64], t: u64 },
}

impl Splits<'_> {
    /// Where to fork children `lo..hi`, absolute indices, or `None` to
    /// update them in order.
    pub fn split(&self, lo: usize, hi: usize) -> Option<usize> {
        if hi - lo < 2 {
            return None;
        }
        match *self {
            Splits::Halves => Some(lo + (hi - lo) / 2),
            Splits::Weighted { prefix, t } => {
                let (base, total) = (prefix[lo], prefix[hi] - prefix[lo]);
                if total < 2 * t {
                    return None;
                }
                let half = base + total / 2;
                let m = lo + 1 + prefix[lo + 1..hi].partition_point(|p| *p < half);
                let m =
                    if m > lo + 1 && half - prefix[m - 1] < prefix[m.min(hi)] - half {
                        m - 1
                    } else {
                        m
                    }
                    .clamp(lo + 1, hi - 1);
                (prefix[m] - base >= t && prefix[hi] - prefix[m] >= t).then_some(m)
            }
        }
    }

    /// The ranges `lo..hi` splits into, in order: the parts a flat fork
    /// runs as siblings.
    pub fn ranges(&self, lo: usize, hi: usize, out: &mut Vec<(usize, usize)>) {
        match self.split(lo, hi) {
            Some(m) => {
                self.ranges(lo, m, out);
                self.ranges(m, hi, out)
            }
            None => out.push((lo, hi)),
        }
    }
}

/// A fork point over slots that run one body (a collection's slots):
/// one estimate per standing slot, and a [`ProbeSite`] each for fresh
/// slots' first updates and for building their instances.
#[derive(Debug, Default)]
pub struct SlotSite {
    standing: Slots,
    pub fresh: ProbeSite,
    pub build: ProbeSite,
    siblings: Siblings,
}

#[derive(Debug)]
enum Slots {
    Probe {
        left: u8,
    },
    /// In order until `countdown` runs out or the slots reach `below`,
    /// the count their estimate would pay a fork at.
    Serial {
        countdown: u16,
        below: usize,
    },
    Measured {
        hist: Hist,
        sampler: Sampler,
        est: u64,
    },
}

impl Default for Slots {
    fn default() -> Self {
        Slots::Probe { left: PROBES }
    }
}

/// The ranges per worker a collection forks into at most: enough for
/// stealing to even out the workers, few enough that the branches'
/// forks and merges stay small beside the slots.
const RANGES_PER_WORKER: usize = 4;

/// The ranges per worker a kernel loop forks into at most: a chunk
/// costs a call and a buffer, so more ranges even out slots of uneven
/// cost.
pub(crate) const CHUNKS_PER_WORKER: usize = 16;

/// The slots per range of a forced collection: `#[parallel(g)]`'s `g`,
/// one range per worker under a bare `#[parallel]`, one slot each under
/// `GRAPHIX_PAR=force`.
pub(crate) fn forced_grain(forced: Option<u32>, n: usize) -> usize {
    match forced {
        None => 1,
        Some(0) => n.div_ceil(crate::branch::eval_workers()),
        Some(g) => g as usize,
    }
}

/// The slots per range of `n` slots estimated at `est` ticks each, in
/// at most `per_worker` ranges per worker.
fn grain(cal: &Calibration, est: u64, n: usize, per_worker: usize) -> usize {
    let workers = crate::branch::eval_workers();
    (cal.t.div_ceil(est.max(1)) as usize).max(n.div_ceil(workers * per_worker))
}

/// What a collection's update does with its standing slots.
pub enum SlotPlan {
    Serial,
    /// Update in order and time the whole: [`SlotSite::measured`].
    Measure(u64),
    /// Fork in ranges of `grain` slots.
    Fork {
        grain: usize,
    },
}

/// The slot count at which slots of `est` ticks each reach `2T`.
fn serial_below(cal: &Calibration, est: u64) -> usize {
    usize::try_from((2 * cal.t).div_ceil(est.max(1))).unwrap_or(usize::MAX)
}

impl SlotSite {
    /// Decide, once, whether the slots are independent: they share one
    /// callback, so two of them stand for all; dependent slots always
    /// run in order.
    pub fn decide_siblings<'a, R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
        children: impl Iterator<Item = &'a Node<R, E>> + Clone,
    ) {
        self.siblings.decide(ctx, n, children, analysis::Sites::Slots)
    }

    /// Decide the siblings again at the next update: the slots' callee
    /// changed.
    pub fn redecide(&mut self) {
        self.siblings = Siblings::default()
    }

    /// Whether the slots must run in order: they are not known to be
    /// independent.
    pub fn in_order(&self) -> bool {
        !self.siblings.independent()
    }

    /// The plan for the `n` standing slots.
    #[inline]
    pub fn plan<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
    ) -> SlotPlan {
        match ctx.fork_mode() {
            ParMode::Off => SlotPlan::Serial,
            _ if n < 2 || !self.siblings.independent() => SlotPlan::Serial,
            ParMode::Force => SlotPlan::Fork { grain: forced_grain(ctx.fork.forced, n) },
            ParMode::Auto => self.plan_auto(n),
        }
    }

    fn plan_auto(&mut self, n: usize) -> SlotPlan {
        match calibration() {
            None => SlotPlan::Serial,
            Some(cal) => self.plan_with(cal, n),
        }
    }

    fn plan_with(&mut self, cal: &Calibration, n: usize) -> SlotPlan {
        match &mut self.standing {
            Slots::Probe { .. } => SlotPlan::Measure(ticks()),
            Slots::Serial { countdown, below } => match countdown.checked_sub(1) {
                Some(c) if n < *below => {
                    *countdown = c;
                    SlotPlan::Serial
                }
                _ => {
                    self.standing = Slots::Probe { left: PROBES };
                    SlotPlan::Measure(ticks())
                }
            },
            Slots::Measured { hist, sampler, est } => {
                if sampler.due() || hist.n < SETTLE {
                    SlotPlan::Measure(ticks())
                } else if saturated() {
                    SlotPlan::Serial
                } else {
                    // CR claude for eric: [perf/medium] This fork is taken for any loop
                    // whose slots cost over T, and T counts only a worker's wake
                    // latency. It leaves out each branch's fork and merge, the slower
                    // slot updates inside a branch, and the CPU the woken workers spin
                    // away. 2000 idle node-walked slots with one 1 ms timer finish
                    // fewer cycles under Auto than serially (1327 vs 1445 in 3 s) on
                    // 12.7 CPU-s instead of 1.8. Decide the policy: verify that a
                    // forked run beats its serial estimate and back off to Serial when
                    // it does not, and/or include the branch overhead in the minimum
                    // range cost. probe:
                    // design/review-2026-10-26/repro/x-perf-runtime-auto-cpu.sh
                    // (x-perf-runtime-02)
                    SlotPlan::Fork { grain: grain(cal, *est, n, RANGES_PER_WORKER) }
                }
            }
        }
    }

    /// A measured update of `n` standing slots that started at tick `t0`
    /// is over.
    pub fn measured(&mut self, t0: u64, n: usize) {
        if let Some(cal) = calibration() {
            self.measured_with(cal, ticks_since(t0), n)
        }
    }

    fn measured_with(&mut self, cal: &Calibration, total: u64, n: usize) {
        match &mut self.standing {
            Slots::Probe { left } => {
                if total >= 2 * cal.t {
                    self.standing = Slots::Measured {
                        hist: Hist::default(),
                        sampler: Sampler::default(),
                        est: 0,
                    };
                } else if *left <= 1 {
                    let below = serial_below(cal, total / n.max(1) as u64);
                    self.standing = Slots::Serial { countdown: RECHECK, below };
                } else {
                    *left -= 1;
                }
            }
            Slots::Serial { .. } => unreachable!("a serial site is not measured"),
            Slots::Measured { hist, sampler, est } => {
                hist.add(cal, total / n.max(1) as u64);
                let e = cal.floor(hist.p75());
                let settled = hist.n >= SETTLE;
                sampler.sampled(settled, e != *est);
                *est = e;
                if settled && e.saturating_mul(n as u64) < 2 * cal.t {
                    self.standing =
                        Slots::Serial { countdown: RECHECK, below: serial_below(cal, e) };
                }
            }
        }
    }
}

/// A fork point over many like items that decides in the cycle it sees
/// them (a collection's growth, a kernel's loop): it times its first
/// items in order, one once its estimate has settled, and forks the rest
/// on what they cost.
#[derive(Debug, Default)]
pub struct ProbeSite {
    hist: Hist,
}

/// What a fork point over `n` like items does with them, a collection's
/// ([`ProbeSite`]) and a kernel loop's ([`LoopSite`]) alike.
pub enum ProbePlan {
    Serial,
    /// Run the first items in order, as many as the site's estimate still
    /// needs, timing them, then decide about the rest.
    Probe,
    Fork {
        grain: usize,
    },
}

impl ProbePlan {
    /// The plan for `n` items under `mode`, `forced` the grain a forced
    /// fork takes.
    pub(crate) fn of(mode: ParMode, n: usize, forced: Option<u32>) -> Self {
        match mode {
            ParMode::Off => Self::Serial,
            _ if n < 2 => Self::Serial,
            ParMode::Force => Self::Fork { grain: forced_grain(forced, n) },
            ParMode::Auto if calibration().is_none() => Self::Serial,
            ParMode::Auto => Self::Probe,
        }
    }
}

impl ProbeSite {
    /// How many of `n` items to time before deciding about the rest.
    fn probes(&self, n: usize) -> usize {
        match self.hist.n < SETTLE {
            true => ((SETTLE - self.hist.n) as usize).min(n),
            false => 1,
        }
    }

    /// The range size for the `n` items a probe left, at most
    /// `per_worker` ranges per worker, or `None` when they run in order.
    fn grain(&self, cal: &Calibration, n: usize, per_worker: usize) -> Option<usize> {
        let est = cal.floor(self.hist.p75());
        (self.hist.n >= SETTLE && est.saturating_mul(n as u64) >= 2 * cal.t)
            .then(|| grain(cal, est, n, per_worker))
    }

    /// [`Self::grain`], unless every worker already has a part.
    fn grain_now(&self, cal: &Calibration, n: usize, per_worker: usize) -> Option<usize> {
        if saturated() { None } else { self.grain(cal, n, per_worker) }
    }

    /// One probe of `ticks` ticks.
    fn probed(&mut self, cal: &Calibration, ticks: u64) {
        self.hist.add(cal, ticks)
    }

    /// Run `items`, the first at index `at`, folding their results into
    /// `acc` with `join`: `probe` runs one item in order; `rest` runs the
    /// items a probe left, forked in ranges of the size it is given, or
    /// in order without one. `None` when either returns `None` (an
    /// interrupt).
    pub fn run<R: Rt, E: UserEvent, S, T>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        items: &mut [S],
        at: usize,
        mut acc: T,
        mut probe: impl FnMut(&mut ExecCtx<'_, R, E>, &mut S, usize) -> Option<T>,
        rest: impl FnOnce(&mut ExecCtx<'_, R, E>, &mut [S], usize, Option<usize>) -> Option<T>,
        join: impl Fn(T, T) -> T,
    ) -> Option<T> {
        let (mut items, mut at) = (items, at);
        let (cal, k) = match ProbePlan::of(ctx.fork_mode(), items.len(), ctx.fork.forced)
        {
            ProbePlan::Serial => (None, 0),
            ProbePlan::Fork { grain } => {
                let r = rest(ctx, items, at, Some(grain))?;
                return Some(join(acc, r));
            }
            ProbePlan::Probe => (
                Some(calibration().expect("a probe is planned once calibrated")),
                self.probes(items.len()),
            ),
        };
        let grain = match cal {
            None => None,
            Some(cal) => {
                let probed;
                (probed, items) = items.split_at_mut(k);
                for item in probed {
                    let t0 = ticks();
                    let r = probe(ctx, item, at)?;
                    self.probed(cal, ticks_since(t0));
                    acc = join(acc, r);
                    at += 1;
                }
                self.grain_now(cal, items.len(), RANGES_PER_WORKER)
            }
        };
        if items.is_empty() {
            return Some(acc);
        }
        let r = rest(ctx, items, at, grain)?;
        Some(join(acc, r))
    }
}

/// A kernel loop's fork point (`design/parallel_eval.md` §10), one per
/// compiled loop, shared by every instance and thread running it: a
/// [`ProbeSite`] that, once its estimate says a loop shorter than some
/// length cannot pay for a fork, lets such loops run untimed for
/// [`RECHECK`] fork thresholds of time.
#[derive(Debug, Default)]
pub struct LoopSite {
    probe: Mutex<ProbeSite>,
    /// The slot count below which a loop runs untimed.
    below: AtomicU32,
    /// The tick the untimed stretch ends.
    until: AtomicU64,
}

/// The fewest slots a probe times as one run: a slot timed alone carries
/// the run's fixed cost, which makes a cheap loop look worth a fork.
pub(crate) const PROBE_BATCH: usize = 16;

impl LoopSite {
    /// Whether a loop of `n` slots runs in order, untimed.
    pub(crate) fn untimed(&self, n: usize) -> bool {
        n < self.below.load(Ordering::Relaxed) as usize
            && ticks() < self.until.load(Ordering::Relaxed)
    }

    /// How many of `n` slots to time, as one run, before deciding about
    /// the rest; `None` while another run is deciding, when this one runs
    /// in order. A slot timed alone carries
    /// the run's fixed cost, so while the estimate is unknown or under
    /// `T / PROBE_BATCH` a probe times at least [`PROBE_BATCH`] slots; a
    /// settled loop of dearer slots probes the one it asks for.
    pub(crate) fn probes(&self, cal: &Calibration, n: usize) -> Option<usize> {
        self.probe.try_lock().map(|p| match p.probes(n) {
            0 => 0,
            k if p.hist.n == 0
                || cal.floor(p.hist.p75()).saturating_mul(PROBE_BATCH as u64) < cal.t =>
            {
                k.max(PROBE_BATCH).min(n)
            }
            k => k,
        })
    }

    /// The range size for the `n` slots that `probes` left, or `None`
    /// when they run in order; `probed` is the probe run's ticks per slot
    /// and its slots, if it ran any. A settled estimate that no fork pays
    /// at starts a stretch of untimed runs.
    pub(crate) fn grain(
        &self,
        cal: &Calibration,
        probed: Option<(u64, usize)>,
        n: usize,
    ) -> Option<usize> {
        let mut p = self.probe.lock();
        if let Some((t, k)) = probed {
            for _ in 0..k {
                p.probed(cal, t);
            }
        }
        match p.grain(cal, n, CHUNKS_PER_WORKER) {
            None if p.hist.n >= SETTLE => {
                let est = cal.floor(p.hist.p75());
                let below = serial_below(cal, est).min(u32::MAX as usize) as u32;
                self.below.store(below, Ordering::Relaxed);
                let until = ticks().saturating_add(RECHECK as u64 * cal.t);
                self.until.store(until, Ordering::Relaxed);
                None
            }
            Some(_) if saturated() => None,
            g => g,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn weighted_split_balances_and_refuses_small_sides() {
        let t = 10;
        // estimates 1, 1, 20, 1, 20
        let prefix = [0, 1, 2, 22, 23, 43];
        let s = Splits::Weighted { prefix: &prefix, t };
        assert_eq!(s.split(0, 5), Some(3));
        assert_eq!(s.split(0, 3), None);
        assert_eq!(s.split(3, 5), None);
        assert_eq!(Splits::Halves.split(2, 7), Some(4));
        assert_eq!(Splits::Halves.split(2, 3), None);
    }

    #[test]
    fn probe_site_decides_a_growth_in_its_cycle() {
        let cal = Calibration { t: 1 << 10, shift: 4 };
        let workers = crate::branch::eval_workers();
        // items at 4T: four probes, then one per growth; the rest forks
        let mut p = ProbeSite::default();
        assert_eq!(p.probes(100), 4);
        assert_eq!(p.probes(3), 3);
        p.hist.add(&cal, 1 << 12);
        assert_eq!(p.probes(100), 3);
        assert_eq!(
            p.grain(&cal, 96, RANGES_PER_WORKER),
            None,
            "an unsettled estimate forks nothing"
        );
        for _ in 0..3 {
            p.hist.add(&cal, 1 << 12);
        }
        assert_eq!(p.probes(100), 1);
        assert_eq!(
            p.grain(&cal, 96, RANGES_PER_WORKER),
            Some(96usize.div_ceil(workers * RANGES_PER_WORKER))
        );
        // ranges of at least ceil(T / estimate) items
        let mut q = ProbeSite::default();
        for _ in 0..4 {
            q.hist.add(&cal, 1 << 8);
        }
        assert_eq!(
            q.grain(&cal, 64, RANGES_PER_WORKER),
            Some(4.max(64usize.div_ceil(workers * RANGES_PER_WORKER)))
        );
        // items too cheap to fork: 96 items at T/64 is under 2T
        let mut r = ProbeSite::default();
        for _ in 0..4 {
            r.hist.add(&cal, 1 << 4);
        }
        assert_eq!(r.grain(&cal, 96, RANGES_PER_WORKER), None);
    }

    #[test]
    fn loop_site_runs_cheap_loops_untimed() {
        let cal = Calibration { t: 1 << 10, shift: 4 };
        let l = LoopSite::default();
        assert!(!l.untimed(3), "a fresh site times its loops");
        assert_eq!(l.probes(&cal, 3), Some(3));
        // slots at T/64: loops under 128 slots cannot pay for a fork
        assert_eq!(l.grain(&cal, Some((1 << 4, 4)), 0), None);
        assert!(l.untimed(127));
        assert!(!l.untimed(128), "a long enough loop is timed");
        let guard = l.probe.lock();
        assert_eq!(l.probes(&cal, 3), None, "a site being decided is not probed again");
        drop(guard);
        l.until.store(ticks(), Ordering::Relaxed);
        assert!(!l.untimed(3), "a stretch over, the site probes again");
    }

    #[test]
    fn serial_slots_reprobe_when_they_grow() {
        let cal = Calibration { t: 1 << 10, shift: 4 };
        let mut site = SlotSite::default();
        // four slots at T/8 each: half of 2T, serial until twice as many
        for _ in 0..PROBES {
            assert!(matches!(site.plan_with(&cal, 4), SlotPlan::Measure(_)));
            site.measured_with(&cal, 4 * (cal.t / 8), 4);
        }
        assert!(matches!(site.plan_with(&cal, 4), SlotPlan::Serial));
        assert!(matches!(site.plan_with(&cal, 15), SlotPlan::Serial));
        assert!(
            matches!(site.plan_with(&cal, 16), SlotPlan::Measure(_)),
            "a serial site that grew to pay for a fork probes again at once"
        );
    }

    #[test]
    fn threshold_floors_in_time() {
        // a 24 MHz counter (Apple's CNTVCT_EL0): a 10 us wake is 240 ticks
        assert_eq!(threshold(240, 0.024), 960);
        assert_eq!(threshold(1, 0.024), 24, "the floor is a microsecond");
        // a 3 GHz TSC
        assert_eq!(threshold(100, 3.0), 3000);
        assert_eq!(threshold(30_000, 3.0), 120_000);
    }

    #[test]
    fn hist_p75_and_decay() {
        let cal = Calibration { t: 1 << 10, shift: 4 };
        let mut h = Hist::default();
        for _ in 0..3 {
            h.add(&cal, 1 << 12);
        }
        h.add(&cal, 1 << 20);
        assert_eq!(h.p75(), 8);
        for _ in 0..200 {
            h.add(&cal, 1 << 6);
        }
        assert!(h.n < Hist::DECAY);
        assert_eq!(h.p75(), 2);
    }

    /// A kernel loop's probe runs a batch of slots only while its slots
    /// are unknown or cheap: a settled loop of dear slots probes one and
    /// forks the rest.
    #[test]
    fn loop_site_batches_only_cheap_probes() {
        let cal = Calibration { t: 1 << 10, shift: 4 };
        let heavy = LoopSite::default();
        assert_eq!(heavy.probes(&cal, 32), Some(PROBE_BATCH));
        assert_eq!(heavy.probes(&cal, 8), Some(8));
        heavy.grain(&cal, Some((1 << 12, 4)), 28);
        assert_eq!(heavy.probes(&cal, 32), Some(1));
        let cheap = LoopSite::default();
        cheap.grain(&cal, Some((1 << 4, 4)), 28);
        assert_eq!(cheap.probes(&cal, 32), Some(PROBE_BATCH));
        let unsettled = LoopSite::default();
        unsettled.grain(&cal, Some((1 << 12, 1)), 31);
        assert_eq!(unsettled.probes(&cal, 32), Some(3));
    }

    /// A loop as long as its estimate's serial bound, `2T / est` rounded
    /// down, cannot pay for a fork and runs untimed.
    #[test]
    fn loop_site_runs_the_longest_unpaying_loop_untimed() {
        let cal = Calibration { t: 1000, shift: 0 };
        let l = LoopSite::default();
        // slots at 64 ticks: 31 of them are 1984 ticks, under 2T
        assert_eq!(l.grain(&cal, Some((64, 4)), 0), None);
        assert!(l.untimed(31));
        assert!(!l.untimed(32), "32 slots reach 2T");
    }

    static CAL: Calibration = Calibration { t: 1 << 10, shift: 4 };

    /// One measured update of `site`'s children at `each` ticks apiece.
    fn measure(site: &mut ForkSite, each: &[u64]) {
        let n = each.len();
        let Plan::Measure(mut m) = site.plan_with(&CAL, n) else {
            panic!("the site did not measure: {:?}", site.0)
        };
        for (i, dt) in each.iter().enumerate() {
            m.timed(i, *dt);
        }
        m.done(n);
    }

    fn forks(site: &mut ForkSite, n: usize) -> bool {
        matches!(site.plan_with(&CAL, n), Plan::Fork(_))
    }

    #[test]
    fn fork_site_measures_what_reaches_2t_and_forks_what_pays() {
        let mut site = ForkSite::default();
        measure(&mut site, &[CAL.t, CAL.t]);
        assert!(matches!(site.0, Site::Measured(_)), "2T total measures each child");
        for _ in 0..SETTLE {
            measure(&mut site, &[4 * CAL.t, 4 * CAL.t]);
        }
        assert!(forks(&mut site, 2), "two settled children at 4T fork");
        assert!(
            matches!(site.plan_with(&CAL, 3), Plan::Measure(_)),
            "a change in the children's count probes again"
        );
        assert!(matches!(site.0, Site::Probe { left: PROBES }));
    }

    #[test]
    fn fork_site_turns_serial_after_its_probes_and_reprobes() {
        let mut site = ForkSite::default();
        for _ in 0..PROBES {
            measure(&mut site, &[CAL.t / 2, CAL.t]);
        }
        assert!(matches!(site.0, Site::Serial { countdown: RECHECK }));
        for _ in 0..RECHECK {
            assert!(matches!(site.plan_with(&CAL, 2), Plan::Serial));
        }
        assert!(
            matches!(site.plan_with(&CAL, 2), Plan::Measure(_)),
            "RECHECK updates on, the site probes again"
        );
        assert!(matches!(site.0, Site::Probe { left: PROBES }));
    }

    #[test]
    fn fork_site_settles_over_a_child_never_timed() {
        let mut site = ForkSite::default();
        measure(&mut site, &[2 * CAL.t, 0, 0]);
        for _ in 0..SETTLE {
            let Plan::Measure(mut m) = site.plan_with(&CAL, 3) else { panic!() };
            m.timed(0, 4 * CAL.t);
            m.timed(2, 4 * CAL.t);
            m.done(3);
        }
        let Site::Measured(m) = &site.0 else { panic!("{:?}", site.0) };
        assert!(m.settled, "a child never timed does not hold the site unsettled");
        assert_eq!(m.prefix[2], m.prefix[1], "and is estimated at nothing");
        assert!(forks(&mut site, 3));
    }

    #[test]
    fn fork_site_that_settles_unpaying_turns_serial() {
        let mut site = ForkSite::default();
        measure(&mut site, &[3 * CAL.t, CAL.t / 8]);
        for _ in 0..SETTLE - 1 {
            measure(&mut site, &[3 * CAL.t, CAL.t / 8]);
            assert!(matches!(site.0, Site::Measured(_)), "unsettled, it keeps measuring");
        }
        measure(&mut site, &[3 * CAL.t, CAL.t / 8]);
        assert!(
            matches!(site.0, Site::Serial { countdown: RECHECK }),
            "settled, a side under T pays for no fork: {:?}",
            site.0
        );
    }

    /// A process that may start no thread measures no threshold, and
    /// neither starting the measurement nor asking for it panics.
    #[cfg(unix)]
    #[test]
    fn no_thread_leaves_the_threshold_unmeasured() {
        const CHILD: &str = "GRAPHIX_COST_NO_THREAD_CHILD";
        if std::env::var_os(CHILD).is_some() {
            let none = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
            // SAFETY: setrlimit reads the struct it is handed.
            assert_eq!(unsafe { libc::setrlimit(libc::RLIMIT_NPROC, &none) }, 0);
            if std::thread::Builder::new().spawn(|| ()).is_ok() {
                // a privileged user is not held to the limit
                return;
            }
            assert!(calibrate().is_none());
            assert!(calibration().is_none());
            return;
        }
        let out = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "cost::tests::no_thread_leaves_the_threshold_unmeasured",
                "--nocapture",
                "--include-ignored",
            ])
            .env(CHILD, "1")
            .env_remove("GRAPHIX_PAR_T")
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    }
}
