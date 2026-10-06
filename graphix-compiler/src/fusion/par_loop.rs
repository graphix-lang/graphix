//! Outlined kernel loops on the evaluation pool
//! (`design/parallel_eval.md` §10): a map-family loop at a kernel's top
//! level runs as chunks of slots, here, in order or forked.

use super::emit_helpers::{
    self, KERNEL_ABORT, KERNEL_ENV, QopRaise, SELF_BLOCK_GEN, SELF_BLOCK_REACHED,
};
use crate::{
    branch::{self, Live},
    cost::{self, LoopSite, Probes},
    tval::TagValue,
};
use graphix_types::{
    abstract_value,
    stack::{Control, InterruptScope, ParMode},
};
use poolshark::local::LPooled;
use rayon::prelude::*;
use std::{any::Any, cell::Cell};

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
    let prev = PAR_LOAN.with(|c| c.replace(loan));
    let r = f();
    PAR_LOAN.with(|c| c.set(prev));
    r
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
    raises: LPooled<Vec<QopRaise>>,
}

impl Run {
    fn new(lo: usize, hi: usize) -> Self {
        Run {
            lo,
            hi,
            out: [0; OUT_WORDS],
            aborted: false,
            panic: None,
            reached: 0,
            raises: LPooled::take(),
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
// invoking thread; the raises are the kernel's own sites and values.
unsafe impl Send for Run {}

/// Run `chunk` over slots `lo..hi` on this thread, the run's loans as
/// the invoking thread had them.
unsafe fn run_here(chunk: Chunk, frame: u64, r: &mut Run) {
    let prev_abort = KERNEL_ABORT.with(|c| c.replace(false));
    let prev_reached = SELF_BLOCK_REACHED.with(|c| c.replace(0));
    let ((), raises) = emit_helpers::with_qop_raises(|| unsafe {
        chunk(frame, r.lo as u64, r.hi as u64, r.out.as_mut_ptr())
    });
    r.raises = raises;
    r.aborted = KERNEL_ABORT.with(|c| c.replace(prev_abort));
    if r.aborted {
        r.panic = emit_helpers::take_kernel_panic();
    }
    r.reached = SELF_BLOCK_REACHED.with(|c| c.replace(prev_reached));
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
    let loan = PAR_LOAN.with(|c| c.get()).filter(|l| {
        l.mode != ParMode::Off && len >= 2 && !abstract_value::value_hooks_loaned()
    });
    let Some(loan) = loan else {
        return unsafe { in_order(chunk, frame, len, find, out) };
    };
    let mut runs: LPooled<Vec<Run>> = LPooled::take();
    // SAFETY: the site is a constant of the running code's record.
    let site = unsafe { &*site };
    let (at, grain) = match loan.mode {
        ParMode::Off => unreachable!("an Off loan is filtered"),
        ParMode::Force => {
            // CR claude for eric: [perf] Under #[parallel] the loaned mode is Force.
            // For a callee's loop (no ROOT bit) this line keeps Force and drops only
            // the grain, so cost::forced_grain(None, len) gives every slot a pool job
            // of its own. That is GRAPHIX_PAR=force behaviour, while the node-walk runs
            // a callee body under ForkFlags::body() (node/lambda.rs:754), which is the
            // runtime's Auto: #[parallel] is meant to exclude callees (parallel_eval.md
            // section 7 and section 10, CLAUDE.md). Two 1M-slot loops in a callee fork
            // into 1M ranges each instead of Auto's 256, and the program takes 1.22 s
            // instead of 0.32 s without the attribute (debug build, same results). Loan
            // the callee-body mode next to the region's (ctx.fork_mode() under
            // ctx.fork.body(): Off under #[serial] or past the depth limit, otherwise
            // ctx.par) and use it for loops without the ROOT bit. probe:
            // design/review-2026-10-05/repro/f-kernel-03.gx (f-kernel-03)
            let forced = if kind & ROOT != 0 { loan.forced } else { None };
            (0, Some(cost::forced_grain(forced, len)))
        }
        ParMode::Auto => match (cost::calibration(), site.untimed(len)) {
            (Some(cal), false) => {
                let Some(k) = site.probes(len) else {
                    return unsafe { in_order(chunk, frame, len, find, out) };
                };
                let mut probes = Probes::new();
                // CR claude for eric: [perf] Once its site has settled, each run times
                // one slot, slot 0, as a one-slot chunk run. Every sample after the
                // first four therefore carries the run's fixed costs (the loan swaps,
                // with_qop_raises, the chunk's own buffer) and, after an idle gap, the
                // cold start. The estimate inflates, and the first loop of a cycle
                // looks costlier than an equally cheap later one. Probe:
                // design/review-2026-10-05/repro/c-cost-misc-02.gx, a 100-slot init on
                // a 2 ms timer, short enough that the bucket floor alone would not fork
                // it: 1467-1479 of 1500 cycles forked at 8192 ticks a slot, with CPU at
                // 1.3-1.8 s against 0.62 s under off. Two equally cheap loops in one
                // cycle (150 and 170 slots, 20 ms timer) were estimated at 16384 and
                // 2048 ticks a slot. Timing the probed slots as one run and recording
                // elapsed / k would spread both costs across the slots.
                // (c-cost-misc-02)
                for i in 0..k {
                    let mut r = Run::new(i, i + 1);
                    let t0 = cost::ticks();
                    unsafe { run_here(chunk, frame, &mut r) };
                    probes.push(cost::ticks().wrapping_sub(t0));
                    let aborted = r.aborted;
                    runs.push(r);
                    if aborted {
                        return unsafe { finish(&mut runs, find, out) };
                    }
                }
                (k, site.grain(cal, &probes, len - k))
            }
            _ => return unsafe { in_order(chunk, frame, len, find, out) },
        },
    };
    match grain {
        None if at < len => {
            let mut r = Run::new(at, len);
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
                runs.push(Run::new(lo, hi));
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
                    let _interrupt = InterruptScope::new(control);
                    let prev_env = KERNEL_ENV.with(|c| c.replace(env as *const _));
                    let prev_gen = SELF_BLOCK_GEN.with(|c| c.replace(generation));
                    with_par_loan(Some(loan), || unsafe { run_here(chunk, frame, r) });
                    SELF_BLOCK_GEN.with(|c| c.set(prev_gen));
                    KERNEL_ENV.with(|c| c.set(prev_env));
                    live.done();
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
/// thread's loans what they reported through theirs: the raises of every
/// run up to the first that aborted, its panic, the activations reached.
unsafe fn finish(runs: &mut [Run], find: bool, out: *mut u64) -> i8 {
    let aborted = runs.iter().position(|r| r.aborted);
    let reached: u64 = runs.iter().map(|r| r.reached).sum();
    SELF_BLOCK_REACHED.with(|c| c.set(c.get() + reached));
    let delivered = aborted.map_or(runs.len(), |i| i + 1);
    for r in runs[..delivered].iter_mut() {
        emit_helpers::queue_qop_raises(&mut r.raises);
    }
    if let Some(i) = aborted {
        for r in runs.iter() {
            r.discard(find);
        }
        if let Some(p) = runs[i].panic.take() {
            emit_helpers::set_kernel_panic(p);
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
            // CR claude for eric: [perf] A forked chunk opens its value buf on a pool
            // worker: emit/outline.rs:328-329 takes a VALUE_SHELLS box and an
            // LPooled<Vec<Value>> from the worker's pools. This call gives both back to
            // the invoking thread's pools. Nothing flows the other way, so the workers'
            // pools drain and every forked chunk allocates a fresh box and Vec, while
            // the invoking thread's pools sit at their caps and free the surplus. Hand
            // each range a buf taken on the invoking thread before the fork, so take
            // and give stay on one thread. (x-engine-collections-10)
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
