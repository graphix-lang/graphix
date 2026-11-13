// Organic firing (design/organic_firing.md): a node fires iff a consumed
// input fires; no node stores a previous value or selection to decide a
// tag. Do not adjust these expectations without a ruling.

use super::dense_deltas::run_delta_fused;
use anyhow::Result;
use graphix_package_core::testing::{FuseExpect, Mode, as_i64s};
use netidx::publisher::Value;

// A scrutinee re-fire on the same arm emits, in the kernel too: x
// delivers 1, 2, 2 into a const `_` arm; count [1, 2, 3].
const SAME_ARM_REFIRE: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:2]);
  let s = #[native] select x {i64:0 => i64:0, _ => i64:7};
  count(s)
}"#;

async fn same_arm_refire_emits(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(SAME_ARM_REFIRE, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

modes!(same_arm_refire_emits);

// A guard-dep fire with unchanged selection emits. count [1, 2, 3].
const GUARD_FIRE: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:3]);
  let g = x % i64:2;
  count(#[native] select i64:5 {n if g >= i64:0 => n, _ => i64:0})
}"#;

async fn guard_fire_emits(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(GUARD_FIRE, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

modes!(guard_fire_emits);

// A guard that never produced is unknown, not false: the select it
// decides is bottom, so `any` takes the 99 (guard-as-false would give 0).
const NEVER_PRODUCED_GUARD: &str = r#"{
  let g: i64 = never();
  let s = select i64:5 { n if g >= i64:0 => n, _ => i64:0 };
  any(s, 99)
}"#;

async fn never_produced_guard_is_unknown(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(NEVER_PRODUCED_GUARD, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![99]);
    Ok(())
}

modes!(never_produced_guard_is_unknown);

// A gating select samples its arm: the scrutinee fires per x delivery
// and re-emits the quiet arm's value; count [1, 2, 3].
const GATING_SELECT: &str = r#"{
  let x = array::iter([i64:1, i64:1, i64:1]);
  let data = i64:42;
  count(select x > i64:0 {true => data, false => never()})
}"#;

async fn gating_select_samples(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(GATING_SELECT, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

modes!(gating_select_samples);

// A recursive call on fired same-value args fires; count [1, 2, 3].
const REC_SAME_ARGS: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:2]);
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0 + f(n - i64:1)};
  count(f(x))
}"#;

async fn rec_same_args_fires(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(REC_SAME_ARGS, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

modes!(rec_same_args_fires);

// A tail same-args re-dispatch fires at any iteration count;
// count [1, 2, 3, 4].
const TAIL_SAME_ARGS: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:3, i64:4]);
  let m = x / i64:3;
  let rec f = |n: i64| -> i64 select n {
    i64:0 => select i64:0 {i64:0 if m == i64:0 => i64:1, _ => i64:2},
    _ => f(n - i64:1)
  };
  count(#[native] f(m))
}"#;

async fn tail_same_args_fires(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(TAIL_SAME_ARGS, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3, 4]);
    Ok(())
}

modes!(tail_same_args_fires);

// A const-terminal recursion fires per delivery on both engines;
// count [1, 2, 3, 4].
const CONST_TERMINAL: &str = r#"{
  let x = array::iter([i64:0, i64:2, i64:0, i64:4]);
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, _ => i64:8 - f(n % i64:1)};
  count(f(x - i64:1))
}"#;

async fn const_terminal_agrees(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(CONST_TERMINAL, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3, 4]);
    Ok(())
}

modes!(const_terminal_agrees);

// The hand-inlined chain (h2 -> h1 -> h0) has the same cadence as
// rec_same_args_fires; count [1, 2, 3].
const CHAIN_TWIN: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:2]);
  let h0 = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0};
  let h1 = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0 + h0(n - i64:1)};
  let h2 = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0 + h1(n - i64:1)};
  count(h2(x))
}"#;

async fn chain_matches_rec(mode: Mode) -> Result<()> {
    let (values, _) = run_delta_fused(CHAIN_TWIN, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

modes!(chain_matches_rec);

// The rule (design/organic_firing.md): every value is bottom before the
// program's first cycle, a value that leaves bottom fires, and a body
// that does not run misses what fires meanwhile. `ep` runs the program
// on past the cycles each fixture judges.

// A top-level constant and a constant write act once, at the start: x
// is [0, 1] and the print happens once over four cycles.
const CONSTANT_ACTS_ONCE: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 3 => n + 1, _ => never() };
  let x = 0;
  x <- 1;
  println("hello");
  x
}"#;

async fn constant_acts_once(mode: Mode) -> Result<()> {
    let (values, out) =
        run_delta_fused(CONSTANT_ACTS_ONCE, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0, 1]);
    assert_eq!(out, "hello\n");
    Ok(())
}

modes!(constant_acts_once);

// `10 / y` is bottom while y is 0 and fires each time y leaves it, its
// consumer with it: y runs 0, 1, 0, 2 and then stands for three cycles.
const LEAVING_BOTTOM_FIRES: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 6 => n + 1, _ => never() };
  let y = 0;
  y <- select ep { 1 => 1, 2 => 0, 3 => 2, _ => never() };
  let q = 10 / y;
  println("q=[q]");
  q + 1
}"#;

async fn leaving_bottom_fires(mode: Mode) -> Result<()> {
    let (values, out) =
        run_delta_fused(LEAVING_BOTTOM_FIRES, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![11, 6]);
    assert_eq!(out, "q=10\nq=5\n");
    Ok(())
}

modes!(leaving_bottom_fires);

// An arm selected in the program's first cycle fires its constants and
// its calls, once.
const FIRST_CYCLE_ARM_FIRES: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 3 => n + 1, _ => never() };
  let s: [`A, `B] = `A;
  select s {
    `A => { println("arm A"); 1 },
    `B => { println("arm B"); 2 }
  }
}"#;

async fn first_cycle_arm_fires(mode: Mode) -> Result<()> {
    let (values, out) =
        run_delta_fused(FIRST_CYCLE_ARM_FIRES, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![1]);
    assert_eq!(out, "arm A\n");
    Ok(())
}

modes!(first_cycle_arm_fires);

// A call over a standing value in an arm entered in the second cycle
// never fires and its twin sampled on the bind does; the arm reads x
// where it stands, so the select gives [0, 5].
const LATE_ARM_CALL_IS_QUIET: &str = r#"{
  let x = 5;
  let s: [`A, `B] = `A;
  s <- `B;
  select s {
    `A => 0,
    k@ `B => { println("unsampled [x]"); println(k ~ "sampled [x]"); x }
  }
}"#;

async fn late_arm_call_is_quiet(mode: Mode) -> Result<()> {
    let (values, out) =
        run_delta_fused(LATE_ARM_CALL_IS_QUIET, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0, 5]);
    assert_eq!(out, "sampled 5\n");
    Ok(())
}

modes!(late_arm_call_is_quiet);

// A function called from an arm entered in the second cycle starts
// nothing over no argument or a standing one, and runs over the bind.
const LATE_CALLEE_IS_QUIET: &str = r#"{
  let x = 5;
  let f = || println("callee ran");
  let g = |v| println("callee got [v]");
  let s: [`A, `B] = `A;
  s <- `B;
  select s {
    `A => 0,
    k@ `B => { f(); g(x); g(k ~ x + 1); x }
  }
}"#;

async fn late_callee_is_quiet(mode: Mode) -> Result<()> {
    let (values, out) =
        run_delta_fused(LATE_CALLEE_IS_QUIET, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0, 5]);
    assert_eq!(out, "callee got 6\n");
    Ok(())
}

modes!(late_callee_is_quiet);

// x leaves bottom (7) while arm A sleeps: at the wake the call over x
// is quiet and the one sampled on the bind reads 7. The 8 that arrives
// while the arm runs fires the unsampled call.
const WAKE_AFTER_ARRIVAL_IS_QUIET: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 7 => n + 1, _ => never() };
  let x: i64 = never();
  x <- select ep { 2 => 7, 5 => 8, _ => never() };
  let s: [`A, `B] = `A;
  s <- select ep { 0 => `B, 3 => `A, _ => never() };
  select s {
    k@ `A => { println("unsampled [x]"); println(k ~ "sampled [x]"); null },
    `B => null
  };
  x
}"#;

async fn wake_after_arrival_is_quiet(mode: Mode) -> Result<()> {
    let (values, out) =
        run_delta_fused(WAKE_AFTER_ARRIVAL_IS_QUIET, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![7, 8]);
    assert_eq!(out, "sampled 7\nunsampled 8\n");
    Ok(())
}

modes!(wake_after_arrival_is_quiet);

// A constant write in a seq step writes at each run: three runs, each
// started by the one before it.
const SEQ_STEP_CONSTANT_WRITES: &str = r#"{
  let busy = false;
  let go = 0;
  let done = seq let n = go { busy <- true; busy <- false; n };
  go <- select done { n if n < 2 => n + 1, _ => never() };
  busy
}"#;

async fn seq_step_constant_writes(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(SEQ_STEP_CONSTANT_WRITES, mode, FuseExpect::Jit).await?;
    let busy = [false, true, false, true, false, true, false];
    assert_eq!(values, busy.map(Value::Bool));
    Ok(())
}

modes!(seq_step_constant_writes);

// `clock ~ n + 1` steps on a tick alone: n + 1 leaving bottom and each
// write of n are no event to the sample, so three ticks in nine cycles
// give [0, 1, 2, 3].
const SAMPLED_COUNTER_STEPS_ON_TICK: &str = r#"{
  let ep = 0;
  ep <- select ep { n if n < 8 => n + 1, _ => never() };
  let clock: i64 = never();
  clock <- select ep { 1 | 4 | 7 => ep, _ => never() };
  let n = 0;
  n <- clock ~ n + 1;
  n
}"#;

async fn sampled_counter_steps_on_tick(mode: Mode) -> Result<()> {
    let (values, _) =
        run_delta_fused(SAMPLED_COUNTER_STEPS_ON_TICK, mode, FuseExpect::Jit).await?;
    assert_eq!(as_i64s(&values), vec![0, 1, 2, 3]);
    Ok(())
}

modes!(sampled_counter_steps_on_tick);
