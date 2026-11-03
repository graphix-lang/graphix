//! The live heap bytes a call activation and a collection slot cost the
//! node-walk, and a fused activation the JIT (the blocks and tables its
//! layout fixes; the code is outside the heap), serial and pruned as a
//! script runs: the same on every host,
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
/// Deep enough that the tree outweighs what compiling held at its peak.
const FUSED_SIZES: [i64; 3] = [20_000, 80_000, 160_000];
const MAX_BYTES_PER_ACTIVATION: i64 = 5_640;
const MAX_BYTES_PER_SLOT: i64 = 3_140;
const MAX_BYTES_PER_FUSED_ACTIVATION: i64 = 192;

/// The most heap bytes, over what was live before it compiled, that
/// `program` held until its value arrived, in a runtime of its own on a
/// thread of its own, so no thread-local pool outlives it; fused or not.
fn peak_bytes(program: CompactString, expected: Value, fused: bool) -> i64 {
    thread::spawn(move || {
        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(run(program, expected, fused))
            .unwrap()
    })
    .join()
    .unwrap()
}

async fn run(program: CompactString, expected: Value, fused: bool) -> Result<i64> {
    let (tx, mut rx) = mpsc::channel(1024);
    let cfg = testing::config(tx, REGISTER, vec![], |ctx| {
        ctx.control.set_par_mode(ParMode::Off);
        ctx.control.set_prune_mode(PruneMode::On);
    })?;
    let flags = match fused {
        true => Default::default(),
        false => CFlag::FusionDisabled.into(),
    };
    let ctx = TestCtx::start(cfg.flags(flags)).await?;
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

/// One `what` costs at most `ceiling` bytes, and about as much between
/// the first two `sizes` as between the last two.
fn assert_linear_cost(
    what: &str,
    ceiling: i64,
    fused: bool,
    sizes: [i64; 3],
    program: impl Fn(i64) -> (CompactString, Value),
) {
    let [base, mid, deep] = sizes.map(|n| {
        let (text, expected) = program(n);
        peak_bytes(text, expected, fused)
    });
    let low = (mid - base) / (sizes[1] - sizes[0]);
    let high = (deep - mid) / (sizes[2] - sizes[1]);
    eprintln!(
        "{what}: peak live heap at {sizes:?}: {base} {mid} {deep} bytes; \
         {low}/{high} bytes per {what}"
    );
    assert!(high <= ceiling, "one {what} costs {high} bytes, over {ceiling}");
    let floor = ceiling / 4;
    assert!(
        high <= 2 * low.max(floor) && low <= 2 * high.max(floor),
        "{what} cost {low} bytes below {} deep, {high} bytes above",
        sizes[1]
    );
}

#[test]
fn activations_and_slots_cost_linear_heap() {
    assert_linear_cost("activation", MAX_BYTES_PER_ACTIVATION, false, SIZES, |depth| {
        let program = format_compact!(
            "{{\n  let rec f = |n: i64| -> i64 select n {{\n    0 => 0,\n    \
             _ => n + f(n - 1)\n  }};\n  f({depth})\n}}"
        );
        (program, Value::I64(depth * (depth + 1) / 2))
    });
    assert_linear_cost("slot", MAX_BYTES_PER_SLOT, false, SIZES, |n| {
        let program = format_compact!(
            "{{\n  let values = array::init({n}, |i| i);\n  \
             (array::len(values), values[0]$, values[-1]$)\n}}"
        );
        let expected = [n, 0, n - 1].map(Value::I64);
        (program, Value::Array(ValArray::from_iter_exact(expected.into_iter())))
    });
}

/// A fused recursion whose every activation is a loop slot's: the slot's
/// root word, its chain table and the activation's block.
#[test]
fn fused_slot_activations_cost_linear_heap() {
    assert_linear_cost(
        "fused activation",
        MAX_BYTES_PER_FUSED_ACTIVATION,
        true,
        FUSED_SIZES,
        |depth| {
            let program = format_compact!(
                "{{\n  let rec t = |d: i64| -> i64 select d {{\n    0 => 1,\n    \
             _ => 1 + array::fold([d - 1], 0, |a, c| a + t(c))\n  }};\n  \
             #[native] t({depth})\n}}"
            );
            (program, Value::I64(depth + 1))
        },
    );
}
