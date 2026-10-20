// Walk pruning (design/walk_pruning.md): a boundary nothing it reads
// delivered to is not walked, and the program computes what the unpruned
// node-walk does.

use super::dense_deltas::run_delta_with;
use anyhow::Result;
use graphix_package_core::testing::{Mode, data};
use netidx::publisher::Value;
use std::time::Duration;

/// Every update of `code`'s result and the boundaries walked, in `mode`.
// XCR claude for claude: [structure/low] This, test_builtins.rs's `run` and
// dense_deltas::run_delta_with are one harness copied three times (fixture_runtime,
// compile_result, updates_until_quiet, shutdown). They differ only in the setup closure
// and one counter read. fusion_review.rs::late_callee_dispatch_primes_constants
// hand-rolls the same loop over Mode::ALL. Let run_delta_with take a setup closure in
// place of `par` (par_attrs passes one that sets the par mode) and record `boundaries`
// in Delta. Then delete this function and test_builtins::run, and write the late-callee
// test as a run_delta fixture under modes!. Found by reading; not run.
// (tests-lang-rest-a-05)
// 2026-10-19 claude: run_delta_with now takes a setup closure in place of par and
// records boundaries in Delta; par_attrs passes one that sets the par mode,
// test_builtins::run and prune::walks are thin callers of it (registering the test
// builtins; reading boundaries).
// fusion_review.rs::late_callee_dispatch_primes_constants stays hand-rolled over
// Mode::ALL: that file is compiled as its own integration test (tests/fusion_review.rs
// includes it by path), where dense_deltas and modes! are not in scope. Pins:
// lang::prune::*, lang::par_attrs::*, lang::test_builtins::*.
async fn walks(code: &str, mode: Mode) -> Result<(Vec<Value>, u64)> {
    let d = run_delta_with(code, mode, |_| (), Duration::from_millis(300)).await?;
    Ok((d.values, d.boundaries))
}

/// An idle map of `size` slots beside a counter ticking 100 times, in a
/// statement of its own, `(n, array::len(m))`.
fn idle_map(size: usize) -> String {
    format!(
        "{{
  let n = 0;
  select n {{ x if x < 100 => n <- (x ~ n) + 1, _ => never() }};
  let m = array::map(array::init({size}, |i| i), |x| once(x) * 2);
  (n, array::len(m))
}}"
    )
}

/// The same map in the arm of a select whose scrutinee every tick
/// delivers.
fn idle_map_in_an_arm(size: usize) -> String {
    format!(
        "{{
  let n = 0;
  select n {{ x if x < 100 => n <- (x ~ n) + 1, _ => never() }};
  let m = select n >= 0 {{
    true => array::map(array::init({size}, |i| i), |x| once(x) * 2),
    false => []
  }};
  (n, array::len(m))
}}"
    )
}

/// The same map in the body of a function every tick calls.
fn idle_map_in_an_instance(size: usize) -> String {
    format!(
        "{{
  let n = 0;
  select n {{ x if x < 100 => n <- (x ~ n) + 1, _ => never() }};
  let g = |k: i64| {{
    let m = array::map(array::init({size}, |i| i), |x| once(x) * 2);
    (k, array::len(m))
  }};
  g(n)
}}"
    )
}

// The counter's ticks walk none of the idle map's slots: the walks a
// thousand more slots cost are their first ones, where unpruned every
// tick walks them all again.
async fn ticks_skip_the_idle_map(program: fn(usize) -> String) -> Result<()> {
    let (small, small_walks) = walks(&program(10), Mode::Interp).await?;
    let (big, big_walks) = walks(&program(1010), Mode::Interp).await?;
    let (unpruned, unpruned_walks) = walks(&program(1010), Mode::Unpruned).await?;
    let expected: Vec<Value> =
        (0..=100).map(|n| Value::from(vec![Value::I64(n), Value::I64(1010)])).collect();
    assert_eq!(big, expected);
    assert_eq!(unpruned, expected);
    assert_eq!(small.len(), expected.len());
    assert!(
        big_walks - small_walks <= 3 * 1000,
        "pruned: {small_walks} walks at 10 slots, {big_walks} at 1010"
    );
    assert!(
        unpruned_walks - small_walks >= 100 * 1000,
        "unpruned: {unpruned_walks} walks at 1010 slots"
    );
    Ok(())
}

/// An update's data form, printed without its integer type prefixes.
fn show(v: Value) -> String {
    data(v).to_string().replace("i64:", "")
}

/// `code`'s updates in every mode, which must agree with each other and
/// with `expected` (each update's data form, printed).
async fn agrees(code: &str, expected: &[&str]) -> Result<()> {
    for mode in [Mode::Interp, Mode::Unpruned, Mode::Jit] {
        let (values, _) = walks(code, mode).await?;
        let got: Vec<String> = values.into_iter().map(|v| show(v)).collect();
        assert_eq!(got, expected, "{mode:?}");
    }
    Ok(())
}

// A standing refresh of a reference cell at an arm's wake (ByRef) marks
// the cell's readers, so a pruned reader reads the refreshed cell.
#[tokio::test(flavor = "multi_thread")]
async fn standing_reference_refresh_marks_its_readers() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let r = select sel { 0 => &(n + 1), _ => &(n + 100) };
  let holder = once(r);
  let k = 0;
  let a = clk ~ { let u = 1; *holder };
  let b = clk ~ select k { 0 => *holder, _ => -1 };
  (clk, n, sel, a, b)
}";
    let (unpruned, _) = walks(code, Mode::Unpruned).await?;
    let expected: Vec<String> = unpruned.into_iter().map(|v| show(v)).collect();
    let expected: Vec<&str> = expected.iter().map(|s| s.as_str()).collect();
    assert!(expected.contains(&"[6, 3, 0, 4, 4]"), "{expected:?}");
    agrees(code, &expected).await
}

// A callee's formal refreshed standing at its wake marks its readers.
#[tokio::test(flavor = "multi_thread")]
async fn standing_formal_refresh_marks_its_readers() -> Result<()> {
    let code = "{
  let clk = array::iter(array::init(10, |i| i));
  let n = 0;
  n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
  let sel = 0;
  sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
  let mk = |x: i64| |y: i64| { let z = y; x };
  let g = select sel { 0 => mk(n), _ => mk(n + 100) };
  let h = once(g);
  let k = 0;
  let a = clk ~ select k { 0 => h(1), _ => -1 };
  (clk, n, sel, a)
}";
    let (unpruned, _) = walks(code, Mode::Unpruned).await?;
    let expected: Vec<String> = unpruned.into_iter().map(|v| show(v)).collect();
    let expected: Vec<&str> = expected.iter().map(|s| s.as_str()).collect();
    assert!(expected.contains(&"[6, 3, 0, 3]"), "{expected:?}");
    agrees(code, &expected).await
}

// A write that lands in the cycle its let's arm is deselected is a fire
// no reader saw: the let re-raises it at the wake, and `count` sees 30.
#[tokio::test(flavor = "multi_thread")]
async fn write_landing_at_the_deselect_is_reraised() -> Result<()> {
    let code = "{
  let n = 0;
  n <- select n { x if x < 6 => (x ~ n) + 1, _ => never() };
  let m = 0;
  m <- n;
  let sel = select m { 2 => false, 3 => false, _ => true };
  select sel {
    true => { let x = 0; x <- n ~ x + 10; (x, count(x)) },
    false => (-1, -1)
  }
}";
    agrees(
        code,
        &[
            "[0, 1]", "[10, 2]", "[20, 3]", "[-1, -1]", "[-1, -1]", "[30, 4]", "[40, 5]",
            "[50, 6]",
        ],
    )
    .await
}

// An enclosing deselect sleeps an arm already asleep: a write that
// landed between the two sleeps is still re-raised at the wake.
#[tokio::test(flavor = "multi_thread")]
async fn second_sleep_keeps_the_first_sleeps_cycle() -> Result<()> {
    let code = "{
  let n = 0;
  n <- select n { x if x < 8 => (x ~ n) + 1, _ => never() };
  let si = select n { 0 => true, 1 => true, k if k >= 6 => true, _ => false };
  let so = select n { 4 => false, 5 => false, _ => true };
  let holder: [&mut i64, null] = null;
  let inner = select so {
    true => select si {
      true => { let x = 0; holder <- &mut x; (x, count(x)) },
      false => (-1, -1)
    },
    false => (-2, -2)
  };
  let h = holder$;
  *h <- select n { 3 => n ~ 100, _ => never() };
  inner
}";
    let (values, _) = walks(code, Mode::Interp).await?;
    let last = values.into_iter().map(|v| show(v)).last();
    assert_eq!(last.as_deref(), Some("[100, 2]"));
    agrees_last(code, "[100, 2]").await
}

async fn agrees_last(code: &str, expected: &str) -> Result<()> {
    for mode in [Mode::Unpruned, Mode::Jit] {
        let (values, _) = walks(code, mode).await?;
        let last = values.into_iter().map(|v| show(v)).last();
        assert_eq!(last.as_deref(), Some(expected), "{mode:?}");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn idle_map_beside_a_ticking_counter() -> Result<()> {
    ticks_skip_the_idle_map(idle_map).await
}

// The ticks walk into the arm, and skip its slots.
#[tokio::test(flavor = "multi_thread")]
async fn idle_map_in_a_walked_arm() -> Result<()> {
    ticks_skip_the_idle_map(idle_map_in_an_arm).await
}

// The ticks walk into the instance, and skip its slots.
#[tokio::test(flavor = "multi_thread")]
async fn idle_map_in_a_walked_instance() -> Result<()> {
    ticks_skip_the_idle_map(idle_map_in_an_instance).await
}
