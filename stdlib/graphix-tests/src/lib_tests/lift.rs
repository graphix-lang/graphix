//! Stage-B connect LIFT: a let-bound scalar counter / accumulator that is
//! a `connect` (`<-`) target now fuses — it is routed in as a kernel
//! INPUT (a feeder) and seeded, so its READS see the connect-written
//! variable (not the stale let-local). Correctness rests on the Stage-A
//! fired-bit: the constant seed fires once at init (STALE after) and is
//! used only while the feeder has never fired, reproducing the node-walk's
//! one-shot `Bind` + its downstream combineLatest cache; `set_var_typed`'s
//! fresh-gate reproduces `Connect::update`'s `if let Some(v)` guard.
//!
//! The `run!` fixtures below assert the program FUSES (`FuseExpect::Jit`)
//! and that the FIRST published value is the seed. But the first value
//! alone can't tell a working counter from one stuck at its seed — so the
//! `assert_stream` tests drive several self-feeding cycles and assert the
//! value STREAM agrees between interp (node-walk, the oracle) and jit.

use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, expr::ModuleResolver};
use graphix_package_core::{run, testing::FuseExpect};
use graphix_rt::GXEvent;
use netidx_value::Value;
use std::time::Duration;

/// Drive `code` until `n` published values of the result are collected,
/// returning the value stream. A self-feeding counter produces a value
/// each cycle; a connect that goes quiescent (e.g. `x <- 5`) produces
/// fewer — pass exactly the number it emits.
async fn collect_n(code: &str, flags: BitFlags<CFlag>, n: usize) -> Result<Vec<Value>> {
    let (tx, mut rx) = tokio::sync::mpsc::channel(1024);
    let gx = format!("let result = {code}");
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(ArcStr::from(gx)),
    )]);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![ModuleResolver::VFS(tbl)],
        flags,
        |_| {},
    )
    .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let mut out = Vec::new();
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    while out.len() < n {
        let mut batch = tokio::time::timeout_at(deadline, rx.recv())
            .await
            .map_err(|_| anyhow!("timeout: collected {}/{n} → {out:?}", out.len()))?
            .ok_or_else(|| anyhow!("runtime died"))?;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                if id == eid {
                    out.push(v);
                }
            }
        }
    }
    ctx.shutdown().await;
    Ok(out)
}

fn as_i64(vs: &[Value]) -> Result<Vec<i64>> {
    vs.iter()
        .map(|v| match v {
            Value::I64(n) => Ok(*n),
            other => Err(anyhow!("non-i64 value {other:?}")),
        })
        .collect()
}

/// The node-walk and jit produce the SAME first `n` values (the
/// differential — independent of what the exact values "should" be).
async fn assert_agree(code: &str, n: usize) -> Result<()> {
    let interp = as_i64(&collect_n(code, CFlag::FusionDisabled.into(), n).await?)?;
    let jit = as_i64(&collect_n(code, BitFlags::empty(), n).await?)?;
    if interp != jit {
        bail!("interp {interp:?} != jit {jit:?}");
    }
    Ok(())
}

/// Both modes agree, and the i64 stream equals `expected`.
async fn assert_stream(code: &str, expected: &[i64]) -> Result<()> {
    let interp =
        as_i64(&collect_n(code, CFlag::FusionDisabled.into(), expected.len()).await?)?;
    if interp != expected {
        bail!("interp (node-walk) stream {interp:?} != expected {expected:?}");
    }
    let jit = as_i64(&collect_n(code, BitFlags::empty(), expected.len()).await?)?;
    if jit != expected {
        bail!("jit stream {jit:?} != expected {expected:?} (interp matched)");
    }
    Ok(())
}

// ── FuseExpect + first-value fixtures ───────────────────────────────────
// The local counter and friends now FUSE (the lift). First published
// value is the seed.

run!(
    counter_lifts_and_fuses,
    "{ let x = 0; x <- x + 1; x }",
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

run!(
    connect_const_lifts,
    "{ let x = 0; x <- 5; x }",
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

run!(
    counter_with_extra_input,
    "{ let x = 0; x <- x + 1; x * 2 }",
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

run!(
    two_independent_counters,
    "{ let a = 0; let b = 0; a <- a + 1; b <- b + 2; a + b }",
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

// ── Multi-cycle STREAM differentials (interp == jit) ─────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn counter_counts_up() -> Result<()> {
    assert_stream("{ let x = 0; x <- x + 1; x }", &[0, 1, 2, 3, 4, 5, 6]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn counter_step_two() -> Result<()> {
    assert_stream("{ let x = 0; x <- x + 2; x }", &[0, 2, 4, 6, 8]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn counter_times_two_return() -> Result<()> {
    // The return reads x but isn't x itself — `x*2` over the counter.
    assert_stream("{ let x = 0; x <- x + 1; x * 2 }", &[0, 2, 4, 6, 8]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn connect_const_then_quiesces() -> Result<()> {
    // `x <- 5`: the constant RHS fires ONLY at init (STALE after), so
    // `set_var` runs once — x goes 0 → 5 then the kernel goes quiescent.
    // No busy-spin (the fired-bit), and the stream is exactly [0, 5].
    assert_stream("{ let x = 0; x <- 5; x }", &[0, 5]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn mutually_referencing_counters() -> Result<()> {
    // a is a plain counter; b accumulates a. Both self-feed every cycle —
    // the exact interleaving is subtle, so just pin the differential:
    // the node-walk and the fused kernel must produce the SAME stream.
    assert_agree("{ let a = 0; let b = 0; a <- a + 1; b <- b + a; b }", 8).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn two_counters_summed() -> Result<()> {
    // Two independent counters consumed together in the return.
    assert_stream(
        "{ let a = 0; let b = 0; a <- a + 1; b <- b + 2; a + b }",
        &[0, 3, 6, 9, 12],
    )
    .await
}

// ── >64 region inputs (the deleted validity-bitmask cap) ─────────────────
// Each lifted counter is a kernel INPUT (a feeder). A block of `n` of them
// summed is a single region with `n` inputs. The old `>64` de-fuse — tied
// to a one-u64 validity bitmask that no longer exists (taint rides each
// param's disc) — would have node-walked this; it now fuses.

/// `{ let a0=0; …; let a{n-1}=0; a0<-a0+1; …; a0 + a1 + … + a{n-1} }`.
/// First published value is the sum of the `n` seeds = 0.
fn many_counters(n: usize) -> String {
    let mut s = String::from("{ ");
    for i in 0..n {
        s.push_str(&format!("let a{i} = 0; "));
    }
    for i in 0..n {
        s.push_str(&format!("a{i} <- a{i} + 1; "));
    }
    for i in 0..n {
        if i > 0 {
            s.push_str(" + ");
        }
        s.push_str(&format!("a{i}"));
    }
    s.push_str(" }");
    s
}

run!(
    region_over_64_inputs_fuses,
    many_counters(70),
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_into_connect_quiesces() -> Result<()> {
    // Regression (findings/hof-connect-jun2026): a scalar `array::fold`
    // result wired straight into a self-`connect` once crashed (ARRAY disc
    // over a scalar payload) and over-fired (no source STALE). The fold's
    // source is a CONSTANT array (STALE after init), so the fold fires once:
    // s goes 0 (seed) → 6, then quiesces. Both modes agree.
    assert_stream(
        "{ let a = [1, 2, 3]; let s = 0; s <- array::fold(a, 0, |acc, e| acc + e); s }",
        &[0, 6],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_captured_init_fires_then_quiesces() -> Result<()> {
    // Capture-aware HOF firing: the fold's INIT is a feeder (`s`), so the
    // fold must RE-FIRE when s lands (a source-STALE-only fix would
    // under-fire → the JIT never publishes). s: 0 → 5; fold(a, s, +) =
    // s + 6 → the result stream is [6, 11], then quiesces.
    assert_stream(
        "{ let a = [1, 2, 3]; let s = 0; s <- 5; array::fold(a, s, |acc, e| acc + e) }",
        &[6, 11],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_captured_body_fires_then_quiesces() -> Result<()> {
    // Same, but the feeder `k` is captured in the BODY (`acc + e + k`).
    // fold(a, 0, |acc, e| acc + e + k) = 6 + 3*k → [6, 21], then quiesces.
    assert_stream(
        "{ let a = [1, 2, 3]; let k = 0; k <- 5; array::fold(a, 0, |acc, e| acc + e + k) }",
        &[6, 21],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn region_over_64_inputs_agrees() -> Result<()> {
    // 70 self-feeding counters summed: every cycle each +1, so the stream
    // steps by 70. Pin the differential (node-walk == jit) over a few cycles.
    assert_stream(&many_counters(70), &[0, 70, 140, 210, 280]).await
}

// ── Phase 7: COMPOSITE / STRING lifted accumulators ──────────────────────
// The lift now accepts a constant-foldable composite / string seed, and
// `emit_let_node` emits a branch-based clone-vs-seed (the feeder path
// CLONES the entry param so the shadow local owns its allocation; the
// seed literal is emitted only on the never-fired path). The connect's
// owned marshal (Phase 2) writes the new value; the runtime feeds it
// back next cycle. This is the reactive sliding-window idiom.

run!(
    array_accumulator_lifts_and_fuses,
    "{ let data: Array<i64> = []; \
       data <- array::push(data, array::len(data)); array::len(data) }",
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn array_accumulator_grows() -> Result<()> {
    // Each cycle pushes the current length: len goes 0, 1, 2, ... — the
    // canonical growing-array accumulator, now fused end to end.
    assert_stream(
        "{ let data: Array<i64> = []; \
           data <- array::push(data, array::len(data)); array::len(data) }",
        &[0, 1, 2, 3, 4],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn array_connect_const_quiesces() -> Result<()> {
    // A constant composite RHS fires only at init: data goes [] → [1, 2]
    // then the kernel quiesces — stream [0, 2].
    assert_stream(
        "{ let data: Array<i64> = []; data <- [1, 2]; array::len(data) }",
        &[0, 2],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn string_accumulator_grows() -> Result<()> {
    // A string accumulator via interpolation: s grows by one char per
    // cycle; the observed stream is its length.
    assert_stream("{ let s = \"\"; s <- \"[s]x\"; str::len(s) }", &[0, 1, 2, 3, 4]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn struct_accumulator_grows() -> Result<()> {
    // A struct accumulator through StructWith (Phase 1) over a lifted
    // struct local (Phase 7): the classic reactive-state-threading idiom
    // `st <- { st with n: st.n + 1 }`.
    assert_stream(
        "{ let st = { n: 0 }; st <- { st with n: st.n + 1 }; st.n }",
        &[0, 1, 2, 3, 4],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn array_window_accumulator_agrees() -> Result<()> {
    // A bounded sliding window: grow to 3 elements, then push+shift
    // (`[1..]` is only taken once the array is full, so the fallible
    // slice is always in bounds — a `[-3..]` from the start would error
    // on the short array, `$`-drop the write, and quiesce). Exercises a
    // select + push + slice pipeline into the lifted connect across
    // cycles; pin the node-walk == jit differential.
    assert_agree(
        "{ let data: Array<i64> = []; \
           data <- select array::len(data) { \
             3 => (array::push(data, array::len(data)))[1..]$, \
             _ => array::push(data, array::len(data)) \
           }; \
           array::len(data) }",
        6,
    )
    .await
}
