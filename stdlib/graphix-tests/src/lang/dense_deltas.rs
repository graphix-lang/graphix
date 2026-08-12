// The dense-delivery RULED-DELTA fixtures (design/dense_delivery.md).
//
// Each test encodes a POST-FLIP expectation for an observable-behavior
// delta Eric ruled intended IN ADVANCE (2026-08-11). They are #[ignore]d
// until the flip (plan phase 5b) and each was verified RED (or
// pass-state-documented) against the pre-flip build — the red→green
// discipline that makes them the flip's adjudication key rather than
// post-hoc blessings. Do NOT un-ignore or adjust expectations without a
// ruling.
//
// Delta-list disposition (numbers from design/dense_delivery.md):
//   1, 2  -> print_const_once, print_hof_callback_once (below)
//   3     -> already AGREEs on main; guarded by the regress corpus
//            finding dyncall-fire-gate-aug2026 (must stay green)
//   4     -> depth_trip_agrees (below)
//   5     -> re-pin verification at the flip (array::group)
//   6, 14 -> stderr log cadence: not sink-capturable; adjudicated via
//            the stdout-baseline diff + manual review at 5b
//   7, 8, 12 -> frame-internal; adjudicated by the 5b desync
//            enumeration over the trace corpus
//   9, 10 -> fusion-coverage churn (FuseExpect/fusecheck), not values
//   11    -> an oracle statement, not a fixture
//   13    -> builtin_bottom_propagates (below)

use anyhow::{Result, bail};
use graphix_compiler::CFlag;
use graphix_package_core::{PrintSink, testing::init_with_flags_and_setup};
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::sync::mpsc;

/// Run `code` (wrapped as `let result = {code}`) to quiescence in one
/// mode, collecting every update of the result expression and the
/// captured print output. Quiescence = no events for 700ms.
async fn run_delta(code: &str, fusion_disabled: bool) -> Result<(Vec<Value>, String)> {
    let (tx, mut rx) = mpsc::channel(10);
    let gx_code = format!("let result = {code}");
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(gx_code)),
    )]);
    let resolver = graphix_compiler::expr::VfsResolver::new(tbl);
    let flags = if fusion_disabled {
        CFlag::FusionDisabled.into()
    } else {
        graphix_compiler::BitFlags::empty()
    };
    let sink = PrintSink::default();
    let seeded = sink.clone();
    let ctx = init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![resolver],
        flags,
        move |ctx| {
            *ctx.libstate.get_or_default::<PrintSink>() = seeded;
        },
    )
    .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let mut values = Vec::new();
    let deadline = tokio::time::sleep(std::time::Duration::from_secs(20));
    tokio::pin!(deadline);
    loop {
        let quiet = tokio::time::sleep(std::time::Duration::from_millis(700));
        tokio::pin!(quiet);
        tokio::select! {
            _ = &mut deadline => bail!("global deadline: program did not quiesce"),
            _ = &mut quiet => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        match e {
                            GXEvent::Updated(id, v) if id == eid => values.push(v),
                            GXEvent::Diagnostic(_, d) => eprintln!("{d}"),
                            _ => (),
                        }
                    }
                }
            }
        }
    }
    let out = sink.take();
    ctx.shutdown().await;
    Ok((values, out))
}

fn as_i64s(values: &[Value]) -> Vec<i64> {
    values
        .iter()
        .map(|v| match v {
            Value::I64(n) => *n,
            v => panic!("expected i64, got {v:?}"),
        })
        .collect()
}

// ── Delta 1: a constant print message fires once, in BOTH engines ──
//
// The dyncall-tagblind-print-aug2026 witness 00. Pre-flip: interp
// prints one "A"; jit prints one per kernel invocation (six). Post-flip
// the print gates on Fired and the fused DynCall delivers the constant
// stale, so both print exactly once.
const PRINT_CONST_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 5 => n <- (x ~ n) + 1, _ => never() };
  let r = { println("A"); n };
  r
}"#;

async fn print_const_once(fusion_disabled: bool) -> Result<()> {
    let (values, out) = run_delta(PRINT_CONST_ONCE, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![0, 1, 2, 3, 4, 5]);
    assert_eq!(out, "A\n");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn print_const_once_interp() -> Result<()> {
    print_const_once(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn print_const_once_jit() -> Result<()> {
    print_const_once(false).await
}

// ── Deltas 1+2: a callback's print fires once per element, not per
// kernel invocation ──
//
// Witness 04: the same class inside a collection loop, where it
// multiplies (per element per invocation), plus the HOF laundering
// angle — the callback subgraph must see honest tags.
const PRINT_HOF_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 3 => n <- (x ~ n) + 1, _ => never() };
  let a = [i64:1, i64:2];
  let r = { let m = array::map(a, |x| { println("@P[x]"); x * i64:2 }); (n, array::len(m)) };
  println("@R[r]");
  i64:0
}"#;

async fn print_hof_once(fusion_disabled: bool) -> Result<()> {
    let (values, out) = run_delta(PRINT_HOF_ONCE, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![0]);
    assert_eq!(out, "@P1\n@P2\n@R(0, 2)\n@R(1, 2)\n@R(2, 2)\n@R(3, 2)\n");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn print_hof_once_interp() -> Result<()> {
    print_hof_once(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn print_hof_once_jit() -> Result<()> {
    print_hof_once(false).await
}

// ── Delta 4: a callee depth trip is a DELIVERED FreshBottom on both
// engines, never a whole-kernel abort ──
//
// The missing_fire_epoch3_aug08e witness. Pre-flip: interp
// [1, -1, 2] (the outer activation's `n - f(n-1)` rides its operand
// cache past the second tripped f(-1)), jit [1, 2] (whole-kernel
// abort discards the invocation; the first f(-1) agrees by
// coincidence — fresh, no ride history). Post-flip BOTH engines
// produce [1, 2]: the trip delivers FreshBottom at the tripping call
// site, and under the bottom-propagates ruling (Q1) the operand-cache
// ride is gone — `n - ⊥` is ⊥, no emission on either f(-1). (Derived,
// not separately ruled: agreement is the ruled requirement; if the
// flip lands agreeing on [1, -1, 2] instead, that needs a ruling
// before this constant changes.)
const DEPTH_TRIP: &str = r#"{
  let x = array::iter([i64:0, i64:2, i64:0, i64:4]);
  let m = x / i64:3;
  let f = |k| select i64:-1 {i64:0 if m == i64:0 => i64:1, _ => i64:1};
  let m = count(let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, _ => n - f(n - i64:1)});
  f(x - i64:1)
}"#;

async fn depth_trip_agrees(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(DEPTH_TRIP, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn depth_trip_agrees_interp() -> Result<()> {
    depth_trip_agrees(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn depth_trip_agrees_jit() -> Result<()> {
    depth_trip_agrees(false).await
}

// ── Delta 13: a bottomed builtin arg bottoms the invocation — the arg
// slot no longer rides its previous value ──
//
// The sleep-preserves-caches-jul2026/02 shape, re-expressed with an
// in-language epoch driver. Epochs: ep0 in1=false in0=0 → else arm,
// max(0, 1/1) = 1; ep1 in1=true → arm flip, 9; ep2 in1=false in0=1 →
// re-select else arm, 1/(1-1) bottoms. Pre-flip both engines emit 10
// (the ruled slot ride: max(10, ridden 1)). Post-flip (Q1: bottom
// propagates, no seam registers) the invocation bottoms and epoch 2
// emits nothing: [1, 9].
const BOTTOM_PROPAGATES: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 2 => n + 1, _ => never() };
  let in0 = select ep { 2 => i64:1, _ => i64:0 };
  let in1 = select ep { 1 => true, _ => false };
  let v0 = i64:1 - in0;
  select in1 { true => i64:9, _ => max(in0 * i64:10, i64:1 / v0) }
}"#;

async fn builtin_bottom_propagates(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(BOTTOM_PROPAGATES, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 9]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn builtin_bottom_propagates_interp() -> Result<()> {
    builtin_bottom_propagates(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn builtin_bottom_propagates_jit() -> Result<()> {
    builtin_bottom_propagates(false).await
}
