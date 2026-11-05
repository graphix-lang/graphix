//! The live heap bytes a program holds, every thread's, as it runs
//! cycles whose state repeats, on an 8-worker runtime: a recursion
//! through a collection that rebuilds the same tree each cycle, forked
//! at every fork point over 16 eval workers, node-walked, fused and
//! partly fused, and one whose collections shrink and grow each cycle,
//! so its slots are made and deleted, node-walked serially and forked.
//! Nodes run on the runtime's one thread and the eval workers, a set
//! fixed by the first fork, and each keeps of what it frees a bounded
//! amount, so once each holds its share the heap stops growing: between
//! two late cycle counts it may grow `MAX_BYTES_PER_CYCLE` a cycle, a
//! small part of the arrays a cycle builds.

use anyhow::Result;
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{CFlag, ParMode, PruneMode};
use graphix_package_core::testing::{self, PackageRef, TestCtx, next_update};
use netidx_value::Value;
use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicI64, Ordering},
    thread,
    time::Duration,
};
use tokio::{sync::mpsc, time::Instant};

const REGISTER: &[PackageRef] = graphix_package::package_refs!();

/// Bytes allocated less those freed, on every thread.
static LIVE: AtomicI64 = AtomicI64::new(0);

struct Counting;

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let p = unsafe { System.alloc(layout) };
        if !p.is_null() {
            LIVE.fetch_add(layout.size() as i64, Ordering::Relaxed);
        }
        p
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let p = unsafe { System.alloc_zeroed(layout) };
        if !p.is_null() {
            LIVE.fetch_add(layout.size() as i64, Ordering::Relaxed);
        }
        p
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) };
        LIVE.fetch_sub(layout.size() as i64, Ordering::Relaxed);
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let p = unsafe { System.realloc(ptr, layout, size) };
        if !p.is_null() {
            LIVE.fetch_add(size as i64 - layout.size() as i64, Ordering::Relaxed);
        }
        p
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

const EVAL_THREADS: &str = "16";
const RUNTIME_THREADS: usize = 8;
/// Cycles by which every thread holds its share of what it frees.
const SETTLED: i64 = 6_000;
const LATER: i64 = 12_000;
const MAX_BYTES_PER_CYCLE: i64 = 1024;

const FLAT_MAP: &str = "array::push(array::flat_map([rest, rest], t), h)";
/// A node whose arrays a kernel builds: `join` fuses, fed by the
/// node-walk's `map`.
const JOIN: &str = "{\n      let join = |xs: Array<Array<i64>>, h: i64| array::push(array::flatten(xs), h);\n      \
                    join(array::map([rest, rest], t), h)\n    }";

/// A node whose children number 1 or 2 as `n` counts.
const SHRINK: &str =
    "array::push(array::flat_map(array::init(1 + n % 2, |i| rest), t), h)";
/// The fixed tree: depth 5, 32 leaves.
const FIXED: &str = "[1, 2, 3, 4, 5]";
/// Depth 1 to 5 as `n` counts: with `SHRINK`, 1 to 32 leaves.
const SHRINKING: &str = "array::init(1 + n % 5, |i| i)";

/// A tree rooted at `root` whose leaves read `n`, which counts cycles.
/// Under `FIXED` and `FLAT_MAP` 31 arrays of 2 to 62 elements are joined
/// and 31 pushed onto them each cycle.
fn program(leaf: &str, node: &str, root: &str) -> CompactString {
    format_compact!(
        "{{\n  let n = 0;\n  n <- n + 1;\n  \
         let rec t = |a: Array<i64>| -> Array<i64> select a {{\n    \
         [] => [{leaf}],\n    \
         [h, rest..] => {node}\n  }};\n  \
         let l = array::len(t({root}));\n  l ~ n\n}}"
    )
}

/// The live heap at `SETTLED` and `LATER` cycles, and what fused.
async fn grow(
    program: CompactString,
    fused: bool,
    par: ParMode,
) -> Result<(i64, i64, usize, usize)> {
    let (tx, mut rx) = mpsc::channel(1024);
    let cfg = testing::config(tx, REGISTER, vec![], |ctx| {
        ctx.control.set_par_mode(par);
        ctx.control.set_prune_mode(PruneMode::On);
    })?;
    let flags = match fused {
        true => Default::default(),
        false => CFlag::FusionDisabled.into(),
    };
    let ctx = TestCtx::start(cfg.flags(flags)).await?;
    let res = ctx.rt.compile(ArcStr::from(program.as_str())).await?;
    let deadline = Instant::now() + Duration::from_secs(600);
    let mut at = [0; 2];
    for (cycles, live) in [SETTLED, LATER].into_iter().zip(at.iter_mut()) {
        loop {
            match next_update(&mut rx, res.exprs[0].id, deadline).await? {
                Value::I64(n) if n >= cycles => break,
                _ => (),
            }
        }
        *live = LIVE.load(Ordering::Relaxed);
    }
    let stats = ctx.fusion_stats().await?;
    drop(res);
    ctx.shutdown().await;
    Ok((at[0], at[1], stats.fused, stats.failed.len()))
}

fn assert_heap_settles(
    what: &str,
    leaf: &str,
    node: &str,
    root: &str,
    fused: bool,
    par: ParMode,
) -> (usize, usize) {
    let program = program(leaf, node, root);
    let (settled, later, regions, failed) = thread::spawn(move || {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(RUNTIME_THREADS)
            .enable_all()
            .build()
            .unwrap()
            .block_on(grow(program, fused, par))
            .unwrap()
    })
    .join()
    .unwrap();
    let per_cycle = (later - settled) / (LATER - SETTLED);
    eprintln!(
        "{what}: live heap {settled} bytes at {SETTLED} cycles, {later} at {LATER}: \
         {per_cycle} bytes a cycle; {regions} regions fused, {failed} failed"
    );
    assert!(
        per_cycle <= MAX_BYTES_PER_CYCLE,
        "{what}: the heap grew {per_cycle} bytes a cycle, over {MAX_BYTES_PER_CYCLE}"
    );
    (regions, failed)
}

/// One test, so the worker count is set before any runtime reads it.
#[test]
#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]
fn forked_heap_settles() {
    // SAFETY: nothing else runs in this binary yet.
    unsafe { std::env::set_var("GRAPHIX_EVAL_THREADS", EVAL_THREADS) };
    use ParMode::{Force, Off};
    assert_heap_settles("node-walk", "n", FLAT_MAP, FIXED, false, Force);
    // XCR claude for claude: [perf/low] The shrinking case runs serially
    // because, forked over the 16 eval workers, its live heap still grows
    // under the thread fix: with the runtime of option (c)
    // (runtime-thread-spread-01) it grew 267 to 559 bytes a cycle between
    // 6000 and 12000 cycles (6 runs, load 48 to 230) and 186 and 240 between
    // 12000 and 24000; with netidx-value's lists bounded too, 198 and 216,
    // and 5.3 MB from 3000 to 48000 cycles in windows of 16 to 256 bytes a
    // cycle, not settled. Serially it is flat (29.21 to 29.22 MB from 3000
    // to 48000 cycles under (c) and the bound), so the slots' own churn
    // keeps nothing: some per-thread state of the eval workers grows with
    // forked slot churn. Whether that is a bounded cache filling slowly (the
    // bounded lists alone can hold 4 MB over 16 workers; poolshark's pools,
    // `FastMemo`) or unbounded was not found. Main grows more on this case
    // forked (833 to 1185 bytes a cycle), which hides it. Find what grows
    // once runtime-thread-spread-01 lands, then fork this case. Probes:
    // ~/ptmp/cr29/scratch/r10-M/r13/ (expc_toggles.diff, shrink_expc*.txt,
    // final_expc*.txt, shrink_long_expc_bound*.txt). (forked-slot-churn-01)
    // 2026-11-03 claude: Measured under (c) as built (a65acf104) with netidx
    // 09c5f584's bound: this shrinking case forked (Force, 16 eval workers; a
    // CHURN_ONLY variant of the test) read 176 and 251 bytes a cycle between
    // 6000 and 12000 cycles (load 82 and 43), under the 1024 limit but not
    // shown to settle. In the shell, s_shrink.gx --no-fusion GRAPHIX_PAR=force
    // went 336 to 458 MB RSS in 5 min under (c) and 356 to 470 on fe2cdbcde's
    // runtime, so the runtime thread does not move it; beside it under (c) with
    // MALLOC_ARENA_MAX=2 it held at 156 to 167 MB (373 to 454 without): most of
    // the forced growth is glibc's per-thread arenas over the eval workers, not
    // live data. Probes: ~/ptmp/cr29/scratch/r11-M/ (forked_churn.txt,
    // rss_s_shrink.txt, rss_arena.txt).
    // 2026-11-03 claude: runtime-thread-spread-01, which this CR waits on, has
    // landed as option (c) (a65acf104) and its XCR is deleted; netidx
    // 09c5f584's bound is in too. What is left is this CR's own question: find
    // what the eval workers keep under forked slot churn, then fork the case.
    // 2026-11-05 claude: Found: the step growth was FusionCtx::dead, unbounded.
    // A compile task's forget_def records every deleted definition, and its
    // join appended them to the root even where the root had emitted nothing
    // (fusion off, or before any fusion), whose dead list no link ever clears:
    // forked here it held 7.9 MB of LambdaIds at 60000 cycles and doubled at
    // about 28000 and 54000 cycles (+2 MB, +4 MB; serial never records).
    // FusionCtx::join now keeps them only where the context records its own
    // (records_dead). With it the case forked reads 51.8 to 54.0 MB from 6000
    // to 120000 cycles in two runs, no steps. The rest is bounded per-thread
    // caches filling within the first few thousand cycles: the eval workers'
    // poolshark pools (about 38 MB idle here, the new eval-worker-pools-01) and
    // netidx-value's free arrays (256 KiB a thread). The shrinking case now
    // also runs forked in forked_heap_settles (251 bytes a cycle between 6000
    // and 12000, under 1024; it cannot see a leak this slow, so the pin is
    // fusion::tests::a_root_that_emitted_nothing_keeps_no_dead_definitions,
    // which fails without the fix: 6 dead definitions kept). Probes:
    // ~/ptmp/cr29/scratch/r17-o2/ (churn_probe.rs, H_large.txt, A_long.txt,
    // A_fix_long*.txt).
    assert_heap_settles("shrinking, serial", "n", SHRINK, SHRINKING, false, Off);
    assert_heap_settles("shrinking, forked", "n", SHRINK, SHRINKING, false, Force);
    let (regions, _) = assert_heap_settles("fused", "n", FLAT_MAP, FIXED, true, Force);
    assert!(regions > 0, "nothing fused");
    let (regions, failed) =
        assert_heap_settles("partly fused", "uniq(n)", JOIN, FIXED, true, Force);
    assert!(regions > 0 && failed > 0, "{regions} regions fused, {failed} failed");
}
