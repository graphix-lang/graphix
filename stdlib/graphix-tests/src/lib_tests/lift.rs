//! Connect lift: a let-bound counter / accumulator that is a `<-`
//! target fuses as a kernel input. The `run!` fixtures assert fusion
//! and the first value; the `assert_stream` tests drive several
//! self-feeding cycles and compare the value stream across engines.

use anyhow::{Result, anyhow, bail};
use graphix_package_core::{
    run,
    testing::{
        FuseExpect, Mode, compile_result, fixture_runtime, next_update, result_source,
        updates_until_quiet,
    },
};
use netidx_value::Value;
use std::time::Duration;
use tokio::time::Instant;

/// The first `n` values of the result of `code` run in `mode`.
// CR claude for claude: [structure/low] collect_n, collect_settled and collect_n_blocks
// repeat a sequence that is also written out in net.rs (net_rpc_missing_path,
// net_no_stale_value), tls.rs (tls_accept_times_out), module_stmt.rs and several lang/
// tests: fixture_runtime over result_source, compile_result, a deadline, next_update or
// updates_until_quiet, then shutdown. testing.rs's private first_value and
// eval_converged do the same, fixed to Jit. Each copy picks its own modes, which is how
// the net.rs tests skip Par and JitPar. Fix: export one helper from testing.rs that
// takes a Mode, the code, and either n values or a quiet window, generalising
// first_value and eval_converged. Loop over Mode::ALL at the callers and delete the
// copies. Also fix tcp.rs's 'the three modes' comment: there are four.
// (tests-lib-b2-16)
async fn collect_n(code: &str, mode: Mode, n: usize) -> Result<Vec<Value>> {
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        mode,
        |_| {},
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let id = res.exprs[0].id;
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut out = Vec::new();
    while out.len() < n {
        out.push(next_update(&mut rx, id, deadline).await?);
    }
    ctx.shutdown().await;
    Ok(out)
}

/// Every value of the result of `code` run in `mode`, to quiescence.
async fn collect_settled(code: &str, mode: Mode) -> Result<Vec<Value>> {
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        mode,
        |_| {},
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let id = res.exprs[0].id;
    let deadline = Instant::now() + Duration::from_secs(10);
    let out =
        updates_until_quiet(&mut rx, id, Duration::from_millis(300), deadline).await?;
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

/// Like [`collect_n`] in `jit` mode, but samples the runtime's live
/// per-activation `SelfBlock` count after each value.
async fn collect_n_blocks(code: &str, n: usize) -> Result<Vec<i64>> {
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        Mode::Jit,
        |_| {},
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let id = res.exprs[0].id;
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut blocks = Vec::new();
    while blocks.len() < n {
        next_update(&mut rx, id, deadline).await?;
        blocks.push(ctx.rt.control().live_self_blocks());
    }
    ctx.shutdown().await;
    Ok(blocks)
}

#[tokio::test(flavor = "current_thread")]
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

#[tokio::test(flavor = "current_thread")]
async fn fused_loop_recursion_sheds_unreached_blocks() -> Result<()> {
    // Two slots of a fused loop take turns going 50 deep: the slot that
    // went shallow frees its tree, so one deep tree is live, not two.
    let blocks = collect_n_blocks(
        "{ let x = array::iter([i64:0, i64:1]); \
           let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; \
           let src = array::init(i64:2, |i| select i == x { true => i64:50, false => i64:1 }); \
           array::map(src, |y| f(y)) }",
        2,
    )
    .await?;
    if blocks[0] < 40 || blocks[1] > 70 {
        bail!(
            "per-slot recursion shrink reclaim off: live SelfBlocks {blocks:?} \
             (expected ~51 after each turn)"
        );
    }
    Ok(())
}

/// Every mode produces the same first `n` values.
// CR claude for claude: [test-gap/low] assert_agree only checks that every mode matches
// Interp, so a regression in shared code (write landing, the connect node, the array
// helpers) that all four modes reproduce goes unnoticed. The testing rule is that a
// predicate pins the value exactly; comparing engines is the fuzzer's job.
// mutually_referencing_counters is deterministic: replace its call with
// assert_stream(code, &[0, 0, 1, 3, 6, 10, 15, 21]). Fix the window test as
// tests-lib-b2-02 describes, then delete this helper. (tests-lib-b2-03)
async fn assert_agree(code: &str, n: usize) -> Result<()> {
    let interp = as_i64(&collect_n(code, Mode::Interp, n).await?)?;
    for mode in &Mode::ALL[1..] {
        let got = as_i64(&collect_n(code, *mode, n).await?)?;
        if got != interp {
            bail!("{mode:?} {got:?} != Interp {interp:?}");
        }
    }
    Ok(())
}

/// In every mode the i64 stream starts with `expected`.
async fn assert_stream(code: &str, expected: &[i64]) -> Result<()> {
    for mode in Mode::ALL {
        let got = as_i64(&collect_n(code, mode, expected.len()).await?)?;
        if got != expected {
            bail!("{mode:?} stream {got:?} != expected {expected:?}");
        }
    }
    Ok(())
}

/// In every mode the stream starts with values that print as `expected`.
async fn assert_values(code: &str, expected: &[&str]) -> Result<()> {
    for mode in Mode::ALL {
        let got = collect_n(code, mode, expected.len()).await?;
        let got: Vec<String> = got.iter().map(|v| v.to_string()).collect();
        if got != expected {
            bail!("{mode:?} stream {got:?} != expected {expected:?}");
        }
    }
    Ok(())
}

/// In every mode the i64 stream is exactly `expected`, and then the
/// program quiesces.
async fn assert_settles(code: &str, expected: &[i64]) -> Result<()> {
    for mode in Mode::ALL {
        let got = as_i64(&collect_settled(code, mode).await?)?;
        if got != expected {
            bail!("{mode:?} settled at {got:?} != expected {expected:?}");
        }
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
    assert_settles("{ let x = 0; x <- 5; x }", &[0, 5]).await
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

// CR claude for claude: [test-gap/medium] region_over_64_inputs_fuses does not pin what
// its name says. FuseExpect::Jit only checks that some kernel ran, and
// many_counters(70) has 70 other fusable regions, one per `a{i} <- a{i} + 1` right-hand
// side (graphix-fuzz run: fused=71). If the 70-input sum stopped fusing, for example
// because a 64-input ABI cap came back, this test would stay green.
// counter_lifts_and_fuses and connect_const_lifts have the same problem: the connect
// node-walks, so only the trivial `x + 1` fuses. Fix: have many_counters wrap the sum
// in `#[native] (...)` so a de-fused sum is a compile error. Also rewrite the module
// doc and the test names, because no 'connect lift' mechanism exists any more. What
// these tests check is that a `<-` target is read as an ordinary kernel input.
// (tests-lib-b2-01)
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
    assert_settles(
        "{ let a = [1, 2, 3]; let s = 0; s <- array::fold(a, 0, |acc, e| acc + e); s }",
        &[0, 6],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_captured_init_fires_then_quiesces() -> Result<()> {
    // The fold's init is a feeder, so the fold re-fires when s lands:
    // [6, 11].
    assert_settles(
        "{ let a = [1, 2, 3]; let s = 0; s <- 5; array::fold(a, s, |acc, e| acc + e) }",
        &[6, 11],
    )
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fold_captured_body_fires_then_quiesces() -> Result<()> {
    // The feeder `k` is captured in the body: [6, 21].
    assert_settles(
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
    assert_settles(
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
async fn array_window_accumulator_slides() -> Result<()> {
    // A bounded sliding window over a moving counter: grow to 3, then
    // push and drop the head (`[1..]` is only taken once the array is
    // full, so the slice is in bounds).
    assert_values(
        "{ let i = 0; i <- i + 1; let data: Array<i64> = []; \
           data <- select array::len(data) { \
             3 => (array::push(data, i))[1..]$, \
             _ => array::push(data, i) \
           }; \
           data }",
        &[
            "[]",
            "[i64:0]",
            "[i64:0, i64:1]",
            "[i64:0, i64:1, i64:2]",
            "[i64:1, i64:2, i64:3]",
            "[i64:2, i64:3, i64:4]",
        ],
    )
    .await
}
