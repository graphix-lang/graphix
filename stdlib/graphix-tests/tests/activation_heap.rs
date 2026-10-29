//! The live heap bytes a call activation and a collection slot cost the
//! node-walk, serial and pruned as a script runs: the same on every host,
//! so the caps sit a few percent over them. The count is the runtime's
//! thread's (a current-thread tokio runtime): the worker pools' own
//! allocations scale with the host's cores and their scheduling.
//! `graphix-shell/tests/recursion_memory.rs` holds the peak RSS, stacks
//! included, to a loose cap.

use anyhow::Result;
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{CFlag, ParMode, PruneMode};
use graphix_package_core::testing::{self, PackageRef, TestCtx, next_update};
use netidx_value::{ValArray, Value};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    cell::Cell,
    thread,
    time::Duration,
};
use tokio::{sync::mpsc, time::Instant};

const REGISTER: &[PackageRef] = graphix_package::package_refs!();

/// Bytes this thread allocated less those it freed, and their peak.
#[derive(Clone, Copy)]
struct Heap {
    live: i64,
    peak: i64,
}

thread_local! {
    static HEAP: Cell<Heap> = const { Cell::new(Heap { live: 0, peak: 0 }) };
}

fn grew(bytes: usize) {
    let _ = HEAP.try_with(|h| {
        let Heap { live, peak } = h.get();
        let live = live + bytes as i64;
        h.set(Heap { live, peak: peak.max(live) })
    });
}

fn shrank(bytes: usize) {
    let _ = HEAP.try_with(|h| {
        let Heap { live, peak } = h.get();
        h.set(Heap { live: live - bytes as i64, peak })
    });
}

struct Counting;

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let p = unsafe { System.alloc(layout) };
        if !p.is_null() {
            grew(layout.size())
        }
        p
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let p = unsafe { System.alloc_zeroed(layout) };
        if !p.is_null() {
            grew(layout.size())
        }
        p
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) };
        shrank(layout.size())
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let p = unsafe { System.realloc(ptr, layout, size) };
        if !p.is_null() {
            match size.checked_sub(layout.size()) {
                Some(more) => grew(more),
                None => shrank(layout.size() - size),
            }
        }
        p
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

const SIZES: [i64; 3] = [1, 5_000, 20_000];
const MAX_BYTES_PER_ACTIVATION: i64 = 6_200;
const MAX_BYTES_PER_SLOT: i64 = 3_300;

/// The most heap bytes, over what was live before it compiled, that
/// `program` held until its value arrived, in a runtime of its own on a
/// thread of its own, so no thread-local pool outlives it.
fn peak_bytes(program: CompactString, expected: Value) -> i64 {
    thread::spawn(move || {
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(run(program, expected))
            .unwrap()
    })
    .join()
    .unwrap()
}

async fn run(program: CompactString, expected: Value) -> Result<i64> {
    let (tx, mut rx) = mpsc::channel(1024);
    let cfg = testing::config(tx, REGISTER, vec![], |ctx| {
        ctx.control.set_par_mode(ParMode::Off);
        ctx.control.set_prune_mode(PruneMode::On);
    })?;
    let ctx = TestCtx::start(cfg.flags(CFlag::FusionDisabled.into())).await?;
    let base = HEAP.with(|h| {
        let live = h.get().live;
        h.set(Heap { live, peak: live });
        live
    });
    let res = ctx.rt.compile(ArcStr::from(program.as_str())).await?;
    let deadline = Instant::now() + Duration::from_secs(300);
    let v = next_update(&mut rx, res.exprs[0].id, deadline).await?;
    let peak = HEAP.with(|h| h.get().peak);
    assert_eq!(v, expected, "{program}");
    drop(res);
    ctx.shutdown().await;
    Ok(peak - base)
}

fn assert_linear_cost(
    what: &str,
    ceiling: i64,
    program: impl Fn(i64) -> (CompactString, Value),
) {
    let [base, mid, deep] = SIZES.map(|n| {
        let (text, expected) = program(n);
        peak_bytes(text, expected)
    });
    let low = (mid - base) / (SIZES[1] - SIZES[0]);
    let high = (deep - mid) / (SIZES[2] - SIZES[1]);
    eprintln!(
        "{what}: peak live heap at {SIZES:?}: {base} {mid} {deep} bytes; \
         {low}/{high} bytes per {what}"
    );
    assert!(high <= ceiling, "one {what} costs {high} bytes, over {ceiling}");
    assert!(
        high <= 2 * low.max(1024) && low <= 2 * high.max(1024),
        "{what} cost {low} bytes below {} deep, {high} bytes above",
        SIZES[1]
    );
}

#[test]
fn activations_and_slots_cost_linear_heap() {
    assert_linear_cost("activation", MAX_BYTES_PER_ACTIVATION, |depth| {
        let program = format_compact!(
            "{{\n  let rec f = |n: i64| -> i64 select n {{\n    0 => 0,\n    \
             _ => n + f(n - 1)\n  }};\n  f({depth})\n}}"
        );
        (program, Value::I64(depth * (depth + 1) / 2))
    });
    assert_linear_cost("slot", MAX_BYTES_PER_SLOT, |n| {
        let program = format_compact!(
            "{{\n  let values = array::init({n}, |i| i);\n  \
             (array::len(values), values[0]$, values[-1]$)\n}}"
        );
        let expected = [n, 0, n - 1].map(Value::I64);
        (program, Value::Array(ValArray::from_iter_exact(expected.into_iter())))
    });
}
