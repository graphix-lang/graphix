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
    assert_heap_settles("shrinking, serial", "n", SHRINK, SHRINKING, false, Off);
    assert_heap_settles("shrinking, forked", "n", SHRINK, SHRINKING, false, Force);
    let (regions, _) = assert_heap_settles("fused", "n", FLAT_MAP, FIXED, true, Force);
    assert!(regions > 0, "nothing fused");
    let (regions, failed) =
        assert_heap_settles("partly fused", "uniq(n)", JOIN, FIXED, true, Force);
    assert!(regions > 0 && failed > 0, "{regions} regions fused, {failed} failed");
}
