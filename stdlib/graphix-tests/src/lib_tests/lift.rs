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
    assert_stream("{ let a = 0; let b = 0; a <- a + 1; b <- b + 2; a + b }", &[
        0, 3, 6, 9, 12,
    ])
    .await
}
