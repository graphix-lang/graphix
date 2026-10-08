//! The parallel evaluator's cost model (`design/parallel_eval.md` §5):
//! what a fork point measures of its children under [`ParMode::Auto`],
//! and where it forks.

use crate::{
    ExecCtx, Rt, UserEvent,
    branch::{eval_pool, pool_idle, saturated},
};
use graphix_types::stack::ParMode;
use parking_lot::Mutex;
use smallvec::SmallVec;
use std::{
    sync::{
        OnceLock,
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

static CALIBRATION: OnceLock<Calibration> = OnceLock::new();

/// The calibration, or `None` while it is being measured: until then
/// nothing forks under `Auto`. The first call starts the measurement on
/// a thread of its own.
pub fn calibration() -> Option<&'static Calibration> {
    static STARTED: AtomicBool = AtomicBool::new(false);
    if let Some(c) = CALIBRATION.get() {
        return Some(c);
    }
    if !STARTED.swap(true, Ordering::Relaxed) {
        std::thread::Builder::new()
            .name("graphix-par-calibrate".into())
            .spawn(|| {
                let _ = CALIBRATION.set(calibrate());
            })
            .expect("spawn the calibration thread");
    }
    None
}

/// How many times a sample of the calibration waits for an idle pool.
const IDLE_TRIES: usize = 500;

/// How long a run must be estimated to take to wait for the calibration
/// rather than run in order: several times the measurement's length.
const WAIT_FOR_CALIBRATION: Duration = Duration::from_millis(100);

/// The calibration for a run of `n` more items like one that took
/// `first`. A run long enough to pay for it waits for the measurement,
/// so a one-shot growth forks in the cycle that builds it; a pool worker
/// never waits, since the measurement needs a free one.
pub fn calibration_for(n: usize, first: Duration) -> Option<&'static Calibration> {
    calibration().or_else(|| {
        let long =
            first.as_nanos().saturating_mul(n as u128) >= WAIT_FOR_CALIBRATION.as_nanos();
        (long && eval_pool().current_thread_index().is_none()).then(|| CALIBRATION.wait())
    })
}

/// The latency, in ticks, from handing an idle evaluation pool a job to
/// the job starting: the cost a fork pays when its right side is
/// stolen.
fn calibrate() -> Calibration {
    let pool = eval_pool();
    let mut wakes = [0u64; 9];
    for w in wakes.iter_mut() {
        // a wake measures an idle pool: a sample taken while a part ran
        // measures the part, so it is taken again
        for _ in 0..IDLE_TRIES {
            // let the workers park
            std::thread::sleep(Duration::from_millis(2));
            if !pool_idle() {
                continue;
            }
            let t0 = ticks();
            *w = pool.install(ticks).saturating_sub(t0);
            if pool_idle() {
                break;
            }
        }
    }
    wakes.sort_unstable();
    // contention only adds to a wake: the lower quartile is the pool's
    let t = (wakes[wakes.len() / 4] * T_PER_WAKE).max(1 << T_BUCKET);
    if crate::dbgenv::graphix_dbg_par() {
        eprintln!("PAR calibrated: wakes {wakes:?} ticks, T = {t}");
    }
    Calibration { t, shift: (63 - t.leading_zeros()).saturating_sub(T_BUCKET) }
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
pub struct ForkSite(Site, Siblings);

/// Whether a fork point's children may run in parallel at all, decided
/// once ([`crate::analysis::independent`]).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Siblings {
    #[default]
    Undecided,
    Independent,
    Dependent,
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
    /// Decide, once, whether the site's `n` children are independent;
    /// dependent ones always run in order.
    #[inline]
    pub fn decide_siblings<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
        independent: impl FnOnce() -> bool,
    ) {
        if self.1 == Siblings::Undecided && n >= 2 && ctx.fork_mode() != ParMode::Off {
            self.1 = match independent() {
                true => Siblings::Independent,
                false => Siblings::Dependent,
            }
        }
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
            _ if n < 2 || self.1 == Siblings::Dependent => Plan::Serial,
            ParMode::Force => Plan::Fork(Splits::Halves),
            ParMode::Auto => self.plan_auto(n),
        }
    }

    fn plan_auto(&mut self, n: usize) -> Plan<'_> {
        let Some(cal) = calibration() else { return Plan::Serial };
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
        let dt = ticks().wrapping_sub(t0);
        self.total += dt;
        if let Site::Measured(m) = &mut self.site.0 {
            m.hist[i].add(self.cal, dt);
        }
        r
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
                    m.prefix[i + 1] = m.prefix[i] + cal.floor(b);
                }
                m.settled = m.hist[0].n >= SETTLE || m.settled;
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

    /// Whether a fork within `lo..hi` may still be taken by a context at
    /// `ctx`'s depth.
    #[inline]
    pub fn fork<R: Rt, E: UserEvent>(
        &self,
        ctx: &ExecCtx<'_, R, E>,
        lo: usize,
        hi: usize,
    ) -> Option<usize> {
        match (ctx.fork_mode(), self) {
            (ParMode::Off, _) => None,
            (_, Splits::Weighted { .. }) if saturated() => None,
            _ => self.split(lo, hi),
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
    Probe { left: u8 },
    Serial { countdown: u16 },
    Measured { hist: Hist, sampler: Sampler, est: u64 },
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
        Some(0) => n.div_ceil(crate::branch::eval_pool().current_num_threads()),
        Some(g) => g as usize,
    }
}

/// The slots per range of `n` slots estimated at `est` ticks each, in
/// at most `per_worker` ranges per worker.
fn grain(cal: &Calibration, est: u64, n: usize, per_worker: usize) -> usize {
    let workers = crate::branch::eval_pool().current_num_threads();
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

impl SlotSite {
    /// Decide, once, whether the slots are independent: they share one
    /// callback, so two of them stand for all; dependent slots always
    /// run in order.
    pub fn decide_siblings<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
        independent: impl FnOnce() -> bool,
    ) {
        if self.siblings == Siblings::Undecided
            && n >= 2
            && ctx.fork_mode() != ParMode::Off
        {
            self.siblings = match independent() {
                true => Siblings::Independent,
                false => Siblings::Dependent,
            }
        }
    }

    /// Whether the slots must run in order.
    pub fn dependent(&self) -> bool {
        self.siblings == Siblings::Dependent
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
            _ if n < 2 || self.siblings == Siblings::Dependent => SlotPlan::Serial,
            ParMode::Force => SlotPlan::Fork { grain: forced_grain(ctx.fork.forced, n) },
            ParMode::Auto => self.plan_auto(n),
        }
    }

    fn plan_auto(&mut self, n: usize) -> SlotPlan {
        let Some(cal) = calibration() else { return SlotPlan::Serial };
        match &mut self.standing {
            Slots::Probe { .. } => SlotPlan::Measure(ticks()),
            Slots::Serial { countdown } => match countdown.checked_sub(1) {
                Some(c) => {
                    *countdown = c;
                    SlotPlan::Serial
                }
                None => {
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
                    SlotPlan::Fork { grain: grain(cal, *est, n, RANGES_PER_WORKER) }
                }
            }
        }
    }

    /// A measured update of `n` standing slots that started at tick `t0`
    /// is over.
    pub fn measured(&mut self, t0: u64, n: usize) {
        let Some(cal) = calibration() else { return };
        let total = ticks().wrapping_sub(t0);
        match &mut self.standing {
            Slots::Probe { left } => {
                if total >= 2 * cal.t {
                    self.standing = Slots::Measured {
                        hist: Hist::default(),
                        sampler: Sampler::default(),
                        est: 0,
                    };
                } else if *left <= 1 {
                    self.standing = Slots::Serial { countdown: RECHECK };
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
                    self.standing = Slots::Serial { countdown: RECHECK };
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

/// What a [`ProbeSite`] does with its items.
pub enum ProbePlan {
    Serial,
    /// Time the first item, then wait for the calibration if the rest is
    /// long enough ([`calibration_for`]).
    Uncalibrated,
    /// Run the first `n` in order, timing each, then decide about the
    /// rest.
    Probe(usize),
    Fork {
        grain: usize,
    },
}

impl ProbeSite {
    /// The plan for `n` items.
    fn plan<R: Rt, E: UserEvent>(&self, ctx: &ExecCtx<'_, R, E>, n: usize) -> ProbePlan {
        match ctx.fork_mode() {
            ParMode::Off => ProbePlan::Serial,
            _ if n < 2 => ProbePlan::Serial,
            ParMode::Force => ProbePlan::Fork { grain: forced_grain(ctx.fork.forced, n) },
            ParMode::Auto if calibration().is_none() => ProbePlan::Uncalibrated,
            ParMode::Auto => ProbePlan::Probe(self.probes(n)),
        }
    }

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
        let (cal, k) = match self.plan(ctx, items.len()) {
            ProbePlan::Serial => (None, 0),
            ProbePlan::Fork { grain } => {
                let r = rest(ctx, items, at, Some(grain))?;
                return Some(join(acc, r));
            }
            ProbePlan::Probe(k) => {
                (Some(calibration().expect("a probe is planned once calibrated")), k)
            }
            ProbePlan::Uncalibrated => {
                let first;
                (first, items) = items.split_at_mut(1);
                let (t0, started) = (ticks(), Instant::now());
                let r = probe(ctx, &mut first[0], at)?;
                let (dt, took) = (ticks().wrapping_sub(t0), started.elapsed());
                acc = join(acc, r);
                at += 1;
                match calibration_for(items.len(), took) {
                    None => (None, 0),
                    Some(cal) => {
                        self.probed(cal, dt);
                        (Some(cal), self.probes(items.len()))
                    }
                }
            }
        };
        let grain = match cal {
            None => None,
            Some(cal) => {
                let probed;
                (probed, items) = items.split_at_mut(k);
                for item in probed {
                    let t0 = ticks();
                    let r = probe(ctx, item, at)?;
                    self.probed(cal, ticks().wrapping_sub(t0));
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

/// A loop's probes: their count, then their ticks.
pub(crate) type Probes = SmallVec<[u64; SETTLE as usize]>;

impl LoopSite {
    /// Whether a loop of `n` slots runs in order, untimed.
    pub(crate) fn untimed(&self, n: usize) -> bool {
        n < self.below.load(Ordering::Relaxed) as usize
            && ticks() < self.until.load(Ordering::Relaxed)
    }

    /// How many of `n` slots to time before deciding about the rest;
    /// `None` while another run is deciding, when this one runs in order.
    pub(crate) fn probes(&self, n: usize) -> Option<usize> {
        self.probe.try_lock().map(|p| p.probes(n))
    }

    /// The range size for the `n` slots that `probes` left, or `None`
    /// when they run in order. A settled estimate that no fork pays at
    /// starts a stretch of untimed runs.
    pub(crate) fn grain(
        &self,
        cal: &Calibration,
        probes: &Probes,
        n: usize,
    ) -> Option<usize> {
        let mut p = self.probe.lock();
        for t in probes.iter() {
            p.probed(cal, *t);
        }
        match p.grain(cal, n, CHUNKS_PER_WORKER) {
            None if p.hist.n >= SETTLE => {
                let est = cal.floor(p.hist.p75()).max(1);
                let below = ((2 * cal.t) / est).min(u32::MAX as u64) as u32;
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
        let workers = crate::branch::eval_pool().current_num_threads();
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
        assert_eq!(l.probes(3), Some(3));
        // slots at T/64: loops under 128 slots cannot pay for a fork
        let probes: Probes = [1 << 4; 4].into_iter().collect();
        assert_eq!(l.grain(&cal, &probes, 0), None);
        assert!(l.untimed(127));
        assert!(!l.untimed(128), "a long enough loop is timed");
        let guard = l.probe.lock();
        assert_eq!(l.probes(3), None, "a site being decided is not probed again");
        drop(guard);
        l.until.store(ticks(), Ordering::Relaxed);
        assert!(!l.untimed(3), "a stretch over, the site probes again");
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
}
