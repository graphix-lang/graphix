//! Outlined kernel loops on the evaluation pool
//! (`design/parallel_eval.md` §10): a map-family loop at a kernel's top
//! level runs as chunks of slots, here, in order or forked.

use super::emit_helpers::{
    self, Deferred, KERNEL_ABORT, KERNEL_ENV, SELF_BLOCK_GEN, SELF_BLOCK_MADE,
    SELF_BLOCK_REACHED,
};
use crate::{
    branch::{self, Live},
    cost::{self, LoopSite, ProbePlan, Probes},
    tval::TagValue,
};
use graphix_types::{
    abstract_value,
    stack::{Control, ParMode},
};
use poolshark::local::LPooled;
use rayon::prelude::*;
use std::{any::Any, cell::Cell, time::Instant};

/// A chunk: runs slots `lo..hi` of its loop over the kernel's `frame`
/// and fills `out`.
type Chunk = unsafe extern "C" fn(frame: u64, lo: u64, hi: u64, out: *mut u64);

/// A chunk's out record: its TAINT (OR) and STALE (AND) accumulators,
/// then its sink: an unfinalized value buf, or a find's found flag and
/// taken `(disc, payload)`.
pub(crate) const OUT_WORDS: usize = 5;

/// `kind` bits: the loop is a find (else its slots fill a buf), and the
/// body is the region's root (else a callee, which `#[parallel]`'s grain
/// does not reach).
pub(crate) const FIND: u64 = 1;
pub(crate) const ROOT: u64 = 2;

/// What a kernel run may fork: the invoking context's mode and forced
/// grain (`ExecCtx::fork_mode`, `fork.forced`), and its runtime.
#[derive(Clone, Copy)]
pub(crate) struct ParLoan {
    pub(crate) mode: ParMode,
    /// The mode of a callee body's loops: `#[parallel]` forces only the
    /// region's own.
    pub(crate) body_mode: ParMode,
    pub(crate) forced: Option<u32>,
    pub(crate) control: *const Control,
}

// SAFETY: `control` is the invoking runtime's, which outlives every
// chunk its kernel run starts.
unsafe impl Send for ParLoan {}
unsafe impl Sync for ParLoan {}

thread_local! {
    static PAR_LOAN: Cell<Option<ParLoan>> = const { Cell::new(None) };
}

/// Loan `loan` to the kernel code `f` runs; nested runs stack.
pub(crate) fn with_par_loan<T>(loan: Option<ParLoan>, f: impl FnOnce() -> T) -> T {
    let _loan = crate::Restore::replace(&PAR_LOAN, loan);
    f()
}

/// One run of a chunk: its out record and what it left in the
/// thread-locals a kernel run reports through.
struct Run {
    lo: usize,
    hi: usize,
    out: [u64; OUT_WORDS],
    aborted: bool,
    panic: Option<Box<dyn Any + Send>>,
    reached: u64,
    made: u64,
    deferred: Deferred,
}

impl Run {
    /// A run of slots `lo..hi`; a map-family run's value buf is taken
    /// here, on the invoking thread, which finalizes or discards it.
    fn new(lo: usize, hi: usize, find: bool) -> Self {
        let mut out = [0; OUT_WORDS];
        if !find {
            out[2] = emit_helpers::value_buf_new(hi - lo);
        }
        Run {
            lo,
            hi,
            out,
            aborted: false,
            panic: None,
            reached: 0,
            made: 0,
            deferred: Deferred::new(),
        }
    }

    /// Free the sink of a run whose result is not taken.
    fn discard(&self, find: bool) {
        if self.aborted {
            return;
        }
        match find {
            false => unsafe { emit_helpers::value_buf_discard(self.out[2]) },
            true if self.out[2] != 0 => {
                drop(unsafe { TagValue::from_raw(self.out[3], self.out[4]) })
            }
            true => (),
        }
    }
}

// SAFETY: a run's out record names values a worker built for the
// invoking thread; what it deferred names the kernel's own sites and values.
unsafe impl Send for Run {}

/// Run `chunk` over slots `lo..hi` on this thread, the run's loans as
/// the invoking thread had them.
unsafe fn run_here(chunk: Chunk, frame: u64, r: &mut Run) {
    let locals = emit_helpers::RunLocals::enter();
    let ((), deferred) = emit_helpers::with_deferred(|| unsafe {
        chunk(frame, r.lo as u64, r.hi as u64, r.out.as_mut_ptr())
    });
    r.deferred = deferred;
    let report = locals.report();
    (r.aborted, r.panic, r.reached, r.made) =
        (report.aborted, report.panic, report.reached, report.made);
}

/// Run an outlined loop of `len` slots over `frame`, filling `out`: in
/// one chunk unless the loan lets it fork and the slots pay for it. 1
/// when a chunk aborted the kernel.
pub(crate) unsafe fn run(
    chunk: u64,
    frame: u64,
    len: u64,
    site: *const LoopSite,
    kind: u64,
    out: *mut u64,
) -> i8 {
    let chunk: Chunk = unsafe { std::mem::transmute(chunk as usize) };
    let len = len as usize;
    let find = kind & FIND != 0;
    let root = kind & ROOT != 0;
    let mode = |l: &ParLoan| if root { l.mode } else { l.body_mode };
    let loan =
        PAR_LOAN.with(|c| c.get()).filter(|_| !abstract_value::value_hooks_loaned());
    let Some(loan) = loan else {
        return unsafe { in_order(chunk, frame, len, find, out) };
    };
    let mut runs: LPooled<Vec<Run>> = LPooled::take();
    // SAFETY: the site is a constant of the running code's record.
    let site = unsafe { &*site };
    let forced = if root { loan.forced } else { None };
    let (at, grain) = match ProbePlan::of(mode(&loan), len, forced) {
        ProbePlan::Serial => return unsafe { in_order(chunk, frame, len, find, out) },
        ProbePlan::Fork { grain } => (0, Some(grain)),
        plan @ (ProbePlan::Uncalibrated | ProbePlan::Probe) => 'auto: {
            if site.untimed(len) {
                return unsafe { in_order(chunk, frame, len, find, out) };
            }
            let mut probes = Probes::new();
            let mut at = 0;
            let cal = match plan {
                ProbePlan::Probe => {
                    cost::calibration().expect("a probe is planned once calibrated")
                }
                _ => {
                    let mut r = Run::new(0, 1, find);
                    let (t0, started) = (cost::ticks(), Instant::now());
                    unsafe { run_here(chunk, frame, &mut r) };
                    let (dt, took) = (cost::ticks_since(t0), started.elapsed());
                    let aborted = r.aborted;
                    runs.push(r);
                    if aborted {
                        return unsafe { finish(&mut runs, find, out) };
                    }
                    at = 1;
                    match cost::calibration_for(len - 1, took) {
                        None => break 'auto (1, None),
                        Some(cal) => {
                            probes.push((dt, 1));
                            cal
                        }
                    }
                }
            };
            // the first slot an uncalibrated loop timed is one of the probes
            // XCR claude for claude: [perf/high] A settled site asks for one probe
            // (ProbeSite::probes returns 1), but this widens it to PROBE_BATCH, so
            // every run of a loop whose slots pay for a fork executes 16 slots in order
            // before forking the rest. A 32-slot loop of heavy slots stays half serial
            // forever: about 3.8 s under Auto against 0.95 s under Force, and the trace
            // shows '32 slots, 16 probed' on every run. Cheap loops, which the batch
            // was added for, are already in LoopSite's untimed stretch. So either time
            // the forked Runs on their workers and probe in order only while unsettled,
            // or keep the batch only while the estimate is below T / PROBE_BATCH.
            // Update design/parallel_eval.md §10, which does not mention the batch.
            // probe:
            // design/review-2026-10-19/repro/c-cost-misc-kernel-loop-probe-batch.gx
            // (c-cost-misc-01)
            // 2026-10-19 claude: LoopSite::probes now takes the calibration and the
            // slots already timed and widens a probe to PROBE_BATCH only while the
            // site's estimate is unknown or under T / PROBE_BATCH; a settled site of
            // dearer slots probes the one slot it asks for. design/parallel_eval.md §10
            // describes the batch. Pin:
            // cost::tests::loop_site_batches_only_cheap_probes. The repro now traces
            // '32 slots, 1 probed, 31 ranges' on every settled run: auto 2.5 s against
            // force 2.2 s (the review binary's auto 8.8 s, a loaded machine).
            let Some(k) = site.probes(cal, len - at, probes.len()) else {
                if at == 0 {
                    return unsafe { in_order(chunk, frame, len, find, out) };
                }
                break 'auto (at, None);
            };
            if k > 0 {
                let mut r = Run::new(at, at + k, find);
                let t0 = cost::ticks();
                unsafe { run_here(chunk, frame, &mut r) };
                let each = cost::ticks_since(t0) / k as u64;
                probes.push((each, k as u16));
                let aborted = r.aborted;
                runs.push(r);
                if aborted {
                    return unsafe { finish(&mut runs, find, out) };
                }
            }
            (at + k, site.grain(cal, &probes, len - at - k))
        }
    };
    match grain {
        None if at < len => {
            let mut r = Run::new(at, len, find);
            unsafe { run_here(chunk, frame, &mut r) };
            runs.push(r);
        }
        None => (),
        Some(g) => {
            let g = g.max(1);
            let first = runs.len();
            let mut lo = at;
            while lo < len {
                let hi = (lo + g).min(len);
                runs.push(Run::new(lo, hi, find));
                lo = hi;
            }
            let forked = &mut runs[first..];
            if crate::dbgenv::graphix_dbg_par() {
                eprintln!(
                    "PAR kernel loop: {len} slots, {at} probed, {} ranges",
                    forked.len()
                );
            }
            // SAFETY: the loan's runtime outlives this kernel run.
            let control = unsafe { &*loan.control };
            control.forked();
            let env = KERNEL_ENV.with(|c| c.get()) as usize;
            let generation = SELF_BLOCK_GEN.with(|c| c.get());
            let live = Live::start(forked.len());
            branch::on_pool(control, || {
                forked.par_iter_mut().with_max_len(1).for_each(|r| {
                    branch::as_part(control, None, &live, || {
                        let _env = crate::Restore::replace(&KERNEL_ENV, env as *const _);
                        let _gen = crate::Restore::replace(&SELF_BLOCK_GEN, generation);
                        with_par_loan(Some(loan), || unsafe {
                            run_here(chunk, frame, r)
                        });
                    })
                })
            });
        }
    }
    unsafe { finish(&mut runs, find, out) }
}

/// Run all `len` slots in one chunk on this thread, under its own loans.
unsafe fn in_order(
    chunk: Chunk,
    frame: u64,
    len: usize,
    find: bool,
    out: *mut u64,
) -> i8 {
    if !find {
        unsafe { *out.add(2) = emit_helpers::value_buf_new(len) };
    }
    unsafe { chunk(frame, 0, len as u64, out) };
    if KERNEL_ABORT.with(|c| c.get()) {
        return 1;
    }
    if !find {
        unsafe { *out.add(2) = emit_helpers::value_buf_finalize(*out.add(2)) };
    }
    0
}

/// Merge `runs`, in slot order, into `out`, and report through this
/// thread's loans what they reported through theirs: what every run up
/// to the first that aborted deferred, its panic, the activations reached
/// and made.
unsafe fn finish(runs: &mut [Run], find: bool, out: *mut u64) -> i8 {
    let aborted = runs.iter().position(|r| r.aborted);
    let reached: u64 = runs.iter().map(|r| r.reached).sum();
    SELF_BLOCK_REACHED.with(|c| c.set(c.get() + reached));
    let made: u64 = runs.iter().map(|r| r.made).sum();
    SELF_BLOCK_MADE.with(|c| c.set(c.get() + made));
    let delivered = aborted.map_or(runs.len(), |i| i + 1);
    for r in runs[..delivered].iter_mut() {
        emit_helpers::queue_deferred(&mut r.deferred);
    }
    // the first panic in slot order is the run's; the merge goes on
    if let Some(p) = runs.iter_mut().find_map(|r| r.panic.take()) {
        emit_helpers::set_kernel_panic(p);
    }
    if aborted.is_some() {
        for r in runs.iter() {
            r.discard(find);
        }
        KERNEL_ABORT.with(|c| c.set(true));
        return 1;
    }
    let out = unsafe { &mut *(out as *mut [u64; OUT_WORDS]) };
    let (mut taint, mut stale) = (0u64, u64::MAX);
    for r in runs.iter() {
        taint |= r.out[0];
        stale &= r.out[1];
    }
    out[0] = taint;
    out[1] = stale;
    match find {
        false => {
            let bufs = runs.iter().map(|r| r.out[2]);
            out[2] = unsafe { emit_helpers::value_bufs_finalize(bufs) };
        }
        true => {
            let taken = runs.iter().position(|r| r.out[2] != 0).unwrap_or(0);
            out[2..].copy_from_slice(&runs[taken].out[2..]);
            for (i, r) in runs.iter().enumerate() {
                if i != taken {
                    r.discard(true);
                }
            }
        }
    }
    0
}
