// Organic firing (design/organic_firing.md): a node fires iff a consumed
// input fires; no node stores a previous value or selection to decide a
// tag. Do not adjust these expectations without a ruling.

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;

// A scrutinee re-fire on the same arm emits: x delivers 1, 2, 2 into a
// const `_` arm; count [1, 2, 3].
const SAME_ARM_REFIRE: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:2]);
  count(select x {i64:0 => i64:0, _ => i64:7})
}"#;

async fn same_arm_refire_emits(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(SAME_ARM_REFIRE, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn same_arm_refire_emits_interp() -> Result<()> {
    same_arm_refire_emits(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn same_arm_refire_emits_jit() -> Result<()> {
    same_arm_refire_emits(false).await
}

// A guard-dep fire with unchanged selection emits; a never-produced
// guard bottoms the init cycle. count [1, 2, 3].
const GUARD_FIRE: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:3]);
  let g = x % i64:2;
  count(select i64:5 {n if g >= i64:0 => n, _ => i64:0})
}"#;

async fn guard_fire_emits(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(GUARD_FIRE, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn guard_fire_emits_interp() -> Result<()> {
    guard_fire_emits(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn guard_fire_emits_jit() -> Result<()> {
    guard_fire_emits(false).await
}

// A gating select samples its arm: the scrutinee fires per x delivery
// and re-emits the quiet arm's value; count [1, 2, 3].
const GATING_SELECT: &str = r#"{
  let x = array::iter([i64:1, i64:1, i64:1]);
  let data = i64:42;
  count(select x > i64:0 {true => data, false => never()})
}"#;

async fn gating_select_samples(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(GATING_SELECT, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn gating_select_samples_interp() -> Result<()> {
    gating_select_samples(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn gating_select_samples_jit() -> Result<()> {
    gating_select_samples(false).await
}

// A recursive call on fired same-value args fires; count [1, 2, 3].
const REC_SAME_ARGS: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:2]);
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0 + f(n - i64:1)};
  count(f(x))
}"#;

async fn rec_same_args_fires(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(REC_SAME_ARGS, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rec_same_args_fires_interp() -> Result<()> {
    rec_same_args_fires(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn rec_same_args_fires_jit() -> Result<()> {
    rec_same_args_fires(false).await
}

// A tail same-args re-dispatch fires at any iteration count;
// count [1, 2, 3, 4].
const TAIL_SAME_ARGS: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:3, i64:4]);
  let m = x / i64:3;
  let rec f = |n: i64| -> i64 select n {
    i64:0 => select i64:0 {i64:0 if m == i64:0 => i64:1, _ => i64:2},
    _ => f(n - i64:1)
  };
  count(f(m))
}"#;

async fn tail_same_args_fires(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(TAIL_SAME_ARGS, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3, 4]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn tail_same_args_fires_interp() -> Result<()> {
    tail_same_args_fires(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn tail_same_args_fires_jit() -> Result<()> {
    tail_same_args_fires(false).await
}

// A const-terminal recursion fires per delivery on both engines;
// count [1, 2, 3, 4].
const CONST_TERMINAL: &str = r#"{
  let x = array::iter([i64:0, i64:2, i64:0, i64:4]);
  let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, _ => i64:8 - f(n % i64:1)};
  count(f(x - i64:1))
}"#;

async fn const_terminal_agrees(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(CONST_TERMINAL, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3, 4]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn const_terminal_agrees_interp() -> Result<()> {
    const_terminal_agrees(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn const_terminal_agrees_jit() -> Result<()> {
    const_terminal_agrees(false).await
}

// The hand-inlined chain (h2 -> h1 -> h0) has the same cadence as
// rec_same_args_fires; count [1, 2, 3].
const CHAIN_TWIN: &str = r#"{
  let x = array::iter([i64:1, i64:2, i64:2]);
  let h0 = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0};
  let h1 = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0 + h0(n - i64:1)};
  let h2 = |n: i64| -> i64 select n {i64:0 => i64:7, _ => i64:0 + h1(n - i64:1)};
  count(h2(x))
}"#;

async fn chain_matches_rec(fusion_disabled: bool) -> Result<()> {
    let (values, _) = run_delta(CHAIN_TWIN, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), vec![1, 2, 3]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn chain_matches_rec_interp() -> Result<()> {
    chain_matches_rec(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn chain_matches_rec_jit() -> Result<()> {
    chain_matches_rec(false).await
}
