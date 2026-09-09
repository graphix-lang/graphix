//! Connect lift: a let-bound counter / accumulator that is a `<-`
//! target fuses as a kernel input. The `run!` fixtures assert fusion
//! and the first value; the `assert_stream` tests drive several
//! self-feeding cycles and compare the value stream across engines.

use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, expr::VfsResolver};
use graphix_package_core::{run, testing::FuseExpect};
use graphix_rt::GXEvent;
use netidx_value::Value;
use std::time::Duration;

/// Drive `code` until `n` published values of the result are collected.
/// A connect that goes quiescent produces fewer — pass exactly the
/// number it emits.
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
        vec![VfsResolver::new(tbl)],
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

/// Like [`collect_n`] but samples the live per-activation `SelfBlock`
/// count after each collected cycle.
async fn collect_n_blocks(code: &str, n: usize) -> Result<Vec<i64>> {
    use std::sync::atomic::Ordering::Relaxed;
    let (tx, mut rx) = tokio::sync::mpsc::channel(1024);
    let gx = format!("let result = {code}");
    let tbl = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(ArcStr::from(gx)),
    )]);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![VfsResolver::new(tbl)],
        BitFlags::empty(),
        |_| {},
    )
    .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let eid = compiled.exprs[0].id;
    let mut blocks = Vec::new();
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    while blocks.len() < n {
        let mut batch = tokio::time::timeout_at(deadline, rx.recv())
            .await
            .map_err(|_| anyhow!("timeout: collected {}/{n}", blocks.len()))?
            .ok_or_else(|| anyhow!("runtime died"))?;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, _) = e
                && id == eid
            {
                blocks.push(
                    graphix_compiler::fusion::emit_helpers::LIVE_SELF_BLOCKS
                        .load(Relaxed),
                );
            }
        }
    }
    ctx.shutdown().await;
    Ok(blocks)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fused_recursion_sheds_unreached_blocks() -> Result<()> {
    // A fused recursion that goes deep then shallow frees the activation
    // blocks it no longer reaches: `deep` ends at depth 50 (~50 live),
    // `shallow` reaches 50 then ends at depth 2 (~2 live).
    let deep = *collect_n_blocks(
        "{ let x = array::iter([i64:50]); \
           let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; \
           f(x) }",
        1,
    )
    .await?
    .last()
    .unwrap();
    let shallow = *collect_n_blocks(
        "{ let x = array::iter([i64:50, i64:2]); \
           let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; \
           f(x) }",
        2,
    )
    .await?
    .last()
    .unwrap();
    if deep < 30 || deep - shallow < 20 {
        bail!(
            "recursion shrink reclaim off: live SelfBlocks deep-quiesced={deep} \
             shallow-quiesced={shallow} (expected deep~50, shallow~2)"
        );
    }
    Ok(())
}

/// The node-walk and jit produce the same first `n` values.
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
    assert_stream("{ let x = 0; x <- x + 1; x * 2 }", &[0, 2, 4, 6, 8]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn connect_const_then_quiesces() -> Result<()> {
    // A constant RHS fires only at init, so the stream is exactly [0, 5].
    assert_stream("{ let x = 0; x <- 5; x }", &[0, 5]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn mutually_referencing_counters() -> Result<()> {
    // Both self-feed every cycle; pin the differential.
    assert_agree("{ let a = 0; let b = 0; a <- a + 1; b <- b + a; b }", 8).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn two_counters_summed() -> Result<()> {
    assert_stream(
        "{ let a = 0; let b = 0; a <- a + 1; b <- b + 2; a + b }",
        &[0, 3, 6, 9, 12],
    )
    .await
}

// Each lifted counter is a kernel input; a region with more than 64
// inputs fuses.

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
    // A fold result wired into a self-connect over a constant source
    // fires once: 0 -> 6, then quiesces.
    assert_stream(
        "{ let a = [1, 2, 3]; let s = 0; s <- array::fold(a, 0, |acc, e| acc + e); s }",
        &[0, 6],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_captured_init_fires_then_quiesces() -> Result<()> {
    // The fold's init is a feeder, so the fold re-fires when s lands:
    // [6, 11].
    assert_stream(
        "{ let a = [1, 2, 3]; let s = 0; s <- 5; array::fold(a, s, |acc, e| acc + e) }",
        &[6, 11],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_captured_body_fires_then_quiesces() -> Result<()> {
    // The feeder `k` is captured in the body: [6, 21].
    assert_stream(
        "{ let a = [1, 2, 3]; let k = 0; k <- 5; array::fold(a, 0, |acc, e| acc + e + k) }",
        &[6, 21],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn tail_fold_retrigger_reads_rebound_formal() -> Result<()> {
    // A hand-written tail-loop fold re-triggered by a fresh seed reads
    // the rebound loop formal, not the stale entry formal: [55, 61].
    assert_stream(
        "{ let rec fold_go = |f: fn(acc: i64, x: i64) -> i64, i: i64, acc: i64| -> i64 \
           select i { 0 => acc, _ => fold_go(f, i - 1, f(acc, i)) }; \
           fold_go(|a, x| a + x, 10, { let s = 0; s <- array::fold([1, 2, 3], 0, |acc, e| acc + e); s }) }",
        &[55, 61],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn connect_in_discarded_fn_arg_still_runs() -> Result<()> {
    // A `<-` spinner inside a discarded fn-typed argument still runs.
    assert_stream(
        "{ let z = 0; let f = |a, b| z; f({ z <- z + 1; str::len }, true) }",
        &[0, 1, 2, 3, 4],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn recursion_shrink_deletes_unreached_activations() -> Result<()> {
    // A depth not reached this cycle is deleted; re-reaching it is a
    // fresh activation whose `hits` counts from zero again.
    assert_stream(
        "{ let step = 0; \
           step <- select step { s if s < 5 => s + 1, _ => never() }; \
           let n = select step { 0 => 3, 1 => 1, _ => 3 }; \
           let rec f = |k: i64| -> i64 { \
             let hits = 0; hits <- k ~ hits + 1; \
             select k { 0 => hits, _ => hits + f(k - 1) } }; \
           f(n) }",
        &[0, 2, 4, 8, 12, 16],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn region_over_64_inputs_agrees() -> Result<()> {
    // 70 self-feeding counters summed step by 70.
    assert_stream(&many_counters(70), &[0, 70, 140, 210, 280]).await
}

// Composite / string lifted accumulators: the reactive sliding-window
// idiom.

run!(
    array_accumulator_lifts_and_fuses,
    "{ let data: Array<i64> = []; \
       data <- array::push(data, array::len(data)); array::len(data) }",
    |v: ::anyhow::Result<&Value>| matches!(v, Ok(Value::I64(0)));
    FuseExpect::Jit
);

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn array_accumulator_grows() -> Result<()> {
    // Each cycle pushes the current length.
    assert_stream(
        "{ let data: Array<i64> = []; \
           data <- array::push(data, array::len(data)); array::len(data) }",
        &[0, 1, 2, 3, 4],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn array_connect_const_quiesces() -> Result<()> {
    // A constant composite RHS fires only at init: [0, 2].
    assert_stream(
        "{ let data: Array<i64> = []; data <- [1, 2]; array::len(data) }",
        &[0, 2],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn string_accumulator_grows() -> Result<()> {
    // A string accumulator via interpolation; the stream is its length.
    assert_stream("{ let s = \"\"; s <- \"[s]x\"; str::len(s) }", &[0, 1, 2, 3, 4]).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn struct_accumulator_grows() -> Result<()> {
    // A struct accumulator: `st <- { st with n: st.n + 1 }`.
    assert_stream(
        "{ let st = { n: 0 }; st <- { st with n: st.n + 1 }; st.n }",
        &[0, 1, 2, 3, 4],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn array_window_accumulator_agrees() -> Result<()> {
    // A bounded sliding window: grow to 3, then push+shift (`[1..]` is
    // only taken once the array is full, so the slice is in bounds).
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
