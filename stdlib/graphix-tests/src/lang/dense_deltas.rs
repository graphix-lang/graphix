// Dense delivery (design/dense_delivery.md): bottom is a production and
// a builtin arg never rides its previous value. Do not adjust these
// expectations without a ruling.

use anyhow::Result;
use graphix_package_core::{
    PrintSink,
    testing::{
        Mode, compile_result, fixture_runtime, result_source, updates_until_quiet,
    },
};
use netidx::publisher::Value;
use std::time::Duration;
use tokio::time::Instant;

/// Run `code` (wrapped as `let result = {code}`) in `mode` until no
/// event arrives for 700ms, collecting every update of the result and the
/// captured print output.
pub(super) async fn run_delta(code: &str, mode: Mode) -> Result<(Vec<Value>, String)> {
    let sink = PrintSink::default();
    let seeded = sink.clone();
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        mode,
        move |ctx| {
            ctx.libstate.set(seeded);
        },
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let deadline = Instant::now() + Duration::from_secs(20);
    let values = updates_until_quiet(
        &mut rx,
        res.exprs[0].id,
        Duration::from_millis(700),
        deadline,
    )
    .await?;
    let out = sink.take();
    ctx.shutdown().await;
    Ok((values, out))
}

pub(super) fn as_i64s(values: &[Value]) -> Vec<i64> {
    values
        .iter()
        .map(|v| match v {
            Value::I64(n) => *n,
            v => panic!("expected i64, got {v:?}"),
        })
        .collect()
}

// A constant print message fires once in both engines.
const PRINT_CONST_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 5 => n <- (x ~ n) + 1, _ => never() };
  let r = { println("A"); n };
  r
}"#;

async fn print_const_once(mode: Mode) -> Result<()> {
    let (values, out) = run_delta(PRINT_CONST_ONCE, mode).await?;
    assert_eq!(as_i64s(&values), vec![0, 1, 2, 3, 4, 5]);
    assert_eq!(out, "A\n");
    Ok(())
}

modes!(print_const_once);

// A callback's print fires once per element, not per kernel invocation.
// The slots print unordered.
const PRINT_HOF_ONCE: &str = r#"{
  let n = 0;
  select n { x if x < 3 => n <- (x ~ n) + 1, _ => never() };
  let a = [i64:1, i64:2];
  let r = { let m = array::map(a, |x| { println("@P[x]"); x * i64:2 }); (n, array::len(m)) };
  println("@R[r]");
  i64:0
}"#;

async fn print_hof_once(mode: Mode) -> Result<()> {
    let (values, out) = run_delta(PRINT_HOF_ONCE, mode).await?;
    assert_eq!(as_i64s(&values), vec![0]);
    let mut lines: Vec<&str> = out.lines().collect();
    lines[..2].sort();
    assert_eq!(
        lines,
        ["@P1", "@P2", "@R(0, 2)", "@R(1, 2)", "@R(2, 2)", "@R(3, 2)"],
        "{out}"
    );
    Ok(())
}

modes!(print_hof_once);

// A bottomed builtin arg bottoms the invocation: epochs give
// [1, 9] and the bottoming third epoch emits nothing.
const BOTTOM_PROPAGATES: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 2 => n + 1, _ => never() };
  let in0 = select ep { 2 => i64:1, _ => i64:0 };
  let in1 = select ep { 1 => true, _ => false };
  let v0 = i64:1 - in0;
  select in1 { true => i64:9, _ => max(in0 * i64:10, i64:1 / v0) }
}"#;

async fn builtin_bottom_propagates(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(BOTTOM_PROPAGATES, mode).await?;
    assert_eq!(as_i64s(&values), vec![1, 9]);
    Ok(())
}

modes!(builtin_bottom_propagates);

// A dynamic call whose function is briefly bottom: the argument that
// moved in the window reaches the instance when the same function
// returns, which fires the call: [0, 14].
const CALLEE_BACK_FROM_BOTTOM: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 4 => n + 1, _ => never() };
  let g = |n: i64| n * 2;
  let cb: [fn(n: i64) -> i64, null] = select ep { 2 | 3 => null, _ => g };
  let x = 0;
  x <- select ep { 2 => 7, _ => never() };
  let f = |cb: [fn(n: i64) -> i64, null], x| cb$(x);
  f(cb, x)
}"#;

async fn callee_back_from_bottom(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(CALLEE_BACK_FROM_BOTTOM, mode).await?;
    assert_eq!(as_i64s(&values), vec![0, 14]);
    Ok(())
}

modes!(callee_back_from_bottom);

// A write through a moving reference lands once, where the reference
// points when its value fires: a retarget writes nothing already
// written, and a write after its arm wakes goes where the reference
// points now.
const MOVING_REFERENCE_WRITES_ONCE: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 6 => n + 1, _ => never() };
  let a = [1, 2, 3];
  let i = 0;
  let ra = &mut a[i];
  let x = 1;
  let y = 2;
  let c = false;
  let rx = select c { false => &mut x, true => &mut y };
  let v = select ep { 1 => 99, _ => never() };
  *ra <- v;
  *rx <- v;
  i <- select ep { 3 => 2, _ => never() };
  c <- select ep { 3 => true, _ => never() };
  let rows = [1, 2, 3];
  let in0 = 0;
  in0 <- select ep { 1 => 1, 2 => 0, 4 => 1, _ => never() };
  let in1 = 0;
  in1 <- select ep { 3 => 2, _ => never() };
  let in2 = 0;
  in2 <- select ep { 5 => 7, _ => never() };
  let r = &mut rows[in1];
  let o = select in0 { 1 => { *r <- in2; -1 }, _ => *r + in2 };
  select ep { 6 => (a, x, y, rows, o), _ => never() }
}"#;

async fn moving_reference_writes_once(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(MOVING_REFERENCE_WRITES_ONCE, mode).await?;
    let last = values.last().map(|v| v.to_string());
    assert_eq!(
        last.as_deref(),
        Some("[[i64:99, i64:2, i64:3], i64:99, i64:2, [i64:1, i64:2, i64:7], i64:-1]")
    );
    Ok(())
}

modes!(moving_reference_writes_once);

// An any whose source went bottom while its arm slept is bottom at the
// wake: [10, 5, -1] and nothing at the wake.
const ANY_WAKES_BOTTOM: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 5 => n + 1, _ => never() };
  let in0 = 0;
  in0 <- select ep { 3 => 1, 5 => 0, _ => never() };
  let in1 = 1;
  in1 <- select ep { 2 => 2, 4 => 0, _ => never() };
  let v0 = 10 / in1;
  select in0 { 0 => any(v0, never<i64>()), _ => -1 }
}"#;

async fn any_wakes_bottom(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(ANY_WAKES_BOTTOM, mode).await?;
    assert_eq!(as_i64s(&values), vec![10, 5, -1]);
    Ok(())
}

modes!(any_wakes_bottom);

// A window with no arm (here a bottom scrutinee) pauses the selected
// arm, which resumes with its state: presses goes on from where it
// stood and entries counts the re-entry.
const NO_ARM_WINDOW_PAUSES: &str = r#"{
  let ev = 0;
  ev <- select ev { k if k < 8 => k + 1, _ => never() };
  let mode: [`Detail, `List] = `List;
  let sel: [i64, null] = 1;
  mode <- select ev { 2 => `Detail, 5 => `List, _ => never() };
  sel <- select ev { 5 => null, 6 => 1, _ => never() };
  let entries = 0;
  select mode {
    `List => select sel$ {
      i => {
        let presses = 0;
        presses <- ev ~ presses + 1;
        entries <- (1 ~ entries) + 1;
        (i, entries, presses)
      }
    },
    `Detail => never()
  }
}"#;

async fn no_arm_window_pauses(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(NO_ARM_WINDOW_PAUSES, mode).await?;
    let shown: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(
        shown,
        [
            "[i64:1, i64:0, i64:0]",
            "[i64:1, i64:1, i64:1]",
            "[i64:1, i64:1, i64:2]",
            "[i64:1, i64:1, i64:3]",
            "[i64:1, i64:2, i64:4]",
            "[i64:1, i64:2, i64:5]"
        ]
    );
    Ok(())
}

modes!(no_arm_window_pauses);

// An untaken guarded arm's binds leave no value behind: a closure made
// in the arm reads only what the arm matched, and fires once.
const UNTAKEN_GUARD_BINDS_NOTHING: &str = r#"{
  let n = 0;
  n <- select n { k if k < 6 => k + 1, _ => never() };
  let f: fn(u: i64) -> i64 = never();
  f <- select (n, n == 2) { (k, b) if b => |u: i64| k * 100 + u, _ => never() };
  (f(n), f(0), count(f(0)))
}"#;

async fn untaken_guard_binds_nothing(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(UNTAKEN_GUARD_BINDS_NOTHING, mode).await?;
    let last = values.last().map(|v| v.to_string());
    assert_eq!(last.as_deref(), Some("[i64:206, i64:200, i64:1]"));
    Ok(())
}

modes!(untaken_guard_binds_nothing);
