//! The parallel evaluator's cost model (`design/parallel_eval.md` §5):
//! what a fork point measures of its children under [`ParMode::Auto`],
//! and where it forks.

use crate::{ExecCtx, Rt, UserEvent};
use graphix_types::stack::{Control, ParMode};
use std::sync::{
    OnceLock,
    atomic::{AtomicBool, Ordering},
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

const T_BUCKET: u32 = 6;

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

/// The median latency, in ticks, from handing an idle evaluation pool a
/// job to the job starting: the cost a fork pays when its right side is
/// stolen.
fn calibrate() -> Calibration {
    let pool = crate::branch::eval_pool();
    let mut wakes = [0u64; 9];
    for w in wakes.iter_mut() {
        // let the workers park
        std::thread::sleep(std::time::Duration::from_millis(2));
        let t0 = ticks();
        *w = pool.install(ticks).saturating_sub(t0);
    }
    wakes.sort_unstable();
    let t = (wakes[wakes.len() / 2] * T_PER_WAKE).max(1 << T_BUCKET);
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
pub struct ForkSite(Site);

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
    /// What this update does with the site's `n` children.
    #[inline]
    pub fn plan<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
    ) -> Plan<'_> {
        match ctx.fork_mode() {
            ParMode::Off => Plan::Serial,
            _ if n < 2 => Plan::Serial,
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
                    Control::promote_current();
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
        if ctx.fork_mode() == ParMode::Off { None } else { self.split(lo, hi) }
    }
}

/// A fork point over slots that run one body (a collection's slots):
/// one estimate per slot.
#[derive(Debug, Default)]
pub struct SlotSite(Slots);

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

/// What a collection's update does with its slots.
pub enum SlotPlan {
    Serial,
    /// Update in order and time the whole: [`SlotSite::measured`].
    Measure(u64),
    /// Split in halves down to `grain` slots.
    Fork {
        grain: usize,
    },
}

impl SlotSite {
    #[inline]
    pub fn plan<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        n: usize,
    ) -> SlotPlan {
        match ctx.fork_mode() {
            ParMode::Off => SlotPlan::Serial,
            _ if n < 2 => SlotPlan::Serial,
            ParMode::Force => SlotPlan::Fork { grain: 1 },
            ParMode::Auto => self.plan_auto(),
        }
    }

    fn plan_auto(&mut self) -> SlotPlan {
        let Some(cal) = calibration() else { return SlotPlan::Serial };
        match &mut self.0 {
            Slots::Probe { .. } => SlotPlan::Measure(ticks()),
            Slots::Serial { countdown } => match countdown.checked_sub(1) {
                Some(c) => {
                    *countdown = c;
                    SlotPlan::Serial
                }
                None => {
                    self.0 = Slots::Probe { left: PROBES };
                    SlotPlan::Measure(ticks())
                }
            },
            Slots::Measured { hist, sampler, est } => {
                if sampler.due() || hist.n < SETTLE {
                    SlotPlan::Measure(ticks())
                } else {
                    SlotPlan::Fork { grain: cal.t.div_ceil((*est).max(1)) as usize }
                }
            }
        }
    }

    /// A measured update of `n` slots that started at tick `t0` is over.
    pub fn measured(&mut self, t0: u64, n: usize) {
        let Some(cal) = calibration() else { return };
        let total = ticks().wrapping_sub(t0);
        match &mut self.0 {
            Slots::Probe { left } => {
                if total >= 2 * cal.t {
                    Control::promote_current();
                    self.0 = Slots::Measured {
                        hist: Hist::default(),
                        sampler: Sampler::default(),
                        est: 0,
                    };
                } else if *left <= 1 {
                    self.0 = Slots::Serial { countdown: RECHECK };
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
                    self.0 = Slots::Serial { countdown: RECHECK };
                }
            }
        }
    }
}

/// Whether a cycle runs on the evaluation pool under `Auto`: a cycle
/// whose p75 cost could pay for a fork does, while cycles there fork.
/// Entering the pool wakes a worker, which a cheap cycle must not pay.
#[derive(Debug, Default)]
pub struct CycleSite {
    hist: Hist,
    /// Consecutive cycles in the pool that forked nothing.
    idle: u16,
    /// Cycles left out of the pool after too many idle ones.
    backoff: u16,
    period: u8,
}

/// Pooled cycles that neither fork nor find a site worth measuring
/// before the cycle backs off.
const IDLE: u16 = 4;

impl CycleSite {
    /// Whether the next cycle enters the pool.
    pub fn enter(&mut self) -> bool {
        let Some(cal) = calibration() else { return false };
        if let Some(b) = self.backoff.checked_sub(1) {
            self.backoff = b;
            return false;
        }
        let enter = self.hist.n >= SETTLE && cal.floor(self.hist.p75()) >= 2 * cal.t;
        if crate::dbgenv::graphix_dbg_par() {
            eprintln!(
                "PAR cycle p75 >= {} ticks (T = {}): {}",
                cal.floor(self.hist.p75()),
                cal.t,
                if enter { "pool" } else { "inline" }
            );
        }
        enter
    }

    /// A cycle took `ticks`; in the pool, it made `forks` forks or
    /// promoted sites to measured.
    pub fn record(&mut self, ticks: u64, pooled: bool, forks: u64) {
        let Some(cal) = calibration() else { return };
        self.hist.add(cal, ticks);
        match (pooled, forks) {
            (false, _) => (),
            (true, 0) => {
                self.idle += 1;
                if self.idle >= IDLE {
                    self.idle = 0;
                    self.backoff = 64 << self.period;
                    self.period = (self.period + 1).min(6);
                    if crate::dbgenv::graphix_dbg_par() {
                        eprintln!(
                            "PAR cycles forked nothing: out of the pool for {}",
                            self.backoff
                        );
                    }
                }
            }
            (true, _) => {
                self.idle = 0;
                self.period = 0;
            }
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
