// The organic-firing RULED-DELTA fixtures (design/organic_firing.md).
//
// Each test encodes a POST-FLIP expectation for an observable-behavior
// delta Eric ruled intended IN ADVANCE (2026-08-14): a node fires iff a
// consumed input fires — selects emit per fired input (scrutinee, guard
// dep, or arm production), calls fire organically, no node stores a
// previous value or selection to decide a tag. Each was verified RED against the
// pre-flip build (baselines in the header of each fixture) and went
// green at the P1/P2 flips — the red→green discipline that made them
// the flip's adjudication key rather than post-hoc blessings. Do NOT
// adjust expectations without a ruling.
//
// Delta-list disposition (numbers from design/organic_firing.md):
//   1     -> same_arm_refire_emits (below)
//   2     -> guard_fire_emits (below)
//   3, 9  -> gating_select_samples (below)
//   4     -> NOT language-observable (FreshBottom vs ride on a
//            never()-arm re-fire: bottoms don't reach count/~/traces).
//            An internal-consistency choice enforced by the
//            differential oracle, not a fixture.
//   5     -> rec_same_args_fires (below); repeals
//            recursion-fires-like-chain/00's cadence
//   6     -> tail_same_args_fires (below); repeals
//            tail-zero-iteration-quiet/00+01's cadence
//   7     -> const_terminal_agrees (below); the known-kernel-gaps
//            witnesses flip to kernel-was-right agreement
//   8     -> observed via count() throughout these fixtures
//   9     -> chain_matches_rec (below): chain-equivalence restored,
//            both per-delivery

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;

// ── Delta 1: a scrutinee re-fire on the same arm EMITS ──
//
// x delivers 1, 2, 2 — same `_` arm throughout, const arm body.
// Pre-flip (strict select): one emission (becoming-selected at init);
// count [1]. Post-flip: every scrutinee delivery emits; count [1,2,3].
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

// ── Delta 2: a guard-dep fire with unchanged selection EMITS ──
//
// Const scrutinee (fires once at init); the guard reads g = x % 2,
// which fires per x delivery; the guard verdict never changes the
// selection. Every guard-dep fire emits (organic firing) — and the
// INIT-PHANTOM guard bottoms the init cycle (activation_state.md,
// 2026-08-20: a never-produced guard is unknown, not false — the old
// init emission took the wildcard on an invented false), so the
// count is [1, 2, 3]: one per guard fire, none at init.
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

// ── Deltas 3+9: the gating-select idiom is a sampler of its arm ──
//
// select-as-sampler lives again: the scrutinee (x > 0) fires per x
// delivery and re-emits the taken arm's current value even though the
// arm body (a const-bound ref) is quiet. Pre-flip: count [1].
// Post-flip: count [1, 2, 3]. `uniq` on the scrutinee is the
// documented remedy for callers who want the old cadence.
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

// ── Delta 5: a recursive call on fired-same-value args FIRES ──
//
// The recursion-fires-like-chain/00 shape with count: x delivers
// 1, 2, 2. Pre-flip: the third (same-value) delivery washes out at the
// const base arm; count [1, 2]. Post-flip: fired args fire the call;
// count [1, 2, 3]. "We have a combinator for gating firing on
// uniqueness and the compiler should never implicitly do it."
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

// ── Delta 6: a tail same-args re-dispatch FIRES at any iteration
// count ──
//
// The aug13h shape (tail-zero-iteration-quiet/01): m = x/3 refires per
// x delivery as an op (values 0, 0, 1, 1). Pre-flip: count [1, 2] (the
// init dispatch and the m-flip). Post-flip: every fired dispatch
// emits; count [1, 2, 3, 4].
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

// ── Delta 7: the const-terminal witnesses become AGREEMENT — the
// kernel was right ──
//
// fuzz/known-kernel-gaps/const-terminal-changed-args-aug2026/00 with
// count. Pre-flip this class DIVERGES: interp [1] (wash-out quiet),
// jit [1, 2, 3, 4]. Post-flip both engines count [1, 2, 3, 4] and the
// known-kernel-gaps dir empties.
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

// ── Delta 9: chain-equivalence restored — the hand-inlined chain has
// the SAME cadence as the recursive form ──
//
// The retained twin of rec_same_args_fires (h2 -> h1 -> h0). Pre-flip:
// count [1, 2] (same wash-out as the rec form pre-flip — but only
// because both were quiet; the equivalence broke in the kernel's
// derivation machinery, see delta 7). Post-flip: count [1, 2, 3],
// matching rec_same_args_fires — recursion fires like the chain, with
// zero machinery.
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
