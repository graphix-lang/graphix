//! A step sees the world as it stood at its entry; `live` reads it live
//! (`design/seq_blocks.md` §5).

use super::dense_deltas::run_delta;
use anyhow::Result;
use arcstr::literal;
use graphix_compiler::CFlag;
use graphix_package_core::testing::{Mode, TestCtx, as_i64s, config};
use tokio::sync::mpsc;

const CLOCK: &str =
    "let step = 0; step <- select step { n if n < 12 => n + 1, _ => never() };";

async fn values(body: &str, mode: Mode) -> Result<Vec<i64>> {
    let code = format!("{{ {CLOCK} {body} }}");
    Ok(as_i64s(&run_delta(&code, mode).await?.0))
}

// A filter written inline and the same filter as a call read the step's
// entry snapshot alike, so both stall where the value at entry fails it;
// `live` follows the variable.
async fn entry_snapshot_and_live(mode: Mode) -> Result<()> {
    for (body, want) in [
        ("seq { select step > 5 { true => step, false => never() } }", &[][..]),
        (
            "let big = |v| v > 5; seq { select big(step) { true => step, false => never() } }",
            &[],
        ),
        ("seq { select live(step) > 5 { true => live(step), false => never() } }", &[6]),
        ("seq { until live(step) > 5; 1 }", &[1]),
    ] {
        assert_eq!(values(body, mode).await?, want, "{body}");
    }
    Ok(())
}

// A variable absent at the step's entry is read at its first arrival, as a
// call's missing argument is.
async fn absent_at_entry_is_read_at_arrival(mode: Mode) -> Result<()> {
    let body = "let x = never(); x <- select step { 3 => 42, _ => never() }; \
                seq { let v = x + 1; v }";
    assert_eq!(values(body, mode).await?, [43]);
    Ok(())
}

// A run sees its own effects: a later step is entered after the write
// landed, under seq, and a written variable is read live under seqq.
async fn a_run_reads_its_own_writes(mode: Mode) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let body = format!(
            "let n = 0; let t = select step {{ 1 => 1, _ => never() }}; \
             {kw} t {{ n <- 5; let m = n; m }}"
        );
        assert_eq!(values(&body, mode).await?, [5], "{kw}");
    }
    Ok(())
}

// The snapshot is lexical: what the seq's text reads is the request's, what
// a function it calls reads is live.
async fn a_callee_reads_live(mode: Mode) -> Result<()> {
    let body = "let x = 0; x <- select step { 3 => 200, _ => never() }; \
                let g = |k: i64| k + x; \
                let t = select step { 1 => 1, _ => never() }; \
                seqq t { until live(step) > 5; let a = x; let c = g(0); a * 1000 + c }";
    assert_eq!(values(body, mode).await?, [200]);
    Ok(())
}

// Entering a step is a birth for the functions it calls too, so a callee
// whose answer starts from a constant completes the step on it, every run.
async fn a_callee_born_with_a_constant_answers_it(mode: Mode) -> Result<()> {
    let body = "let counter = |t| { let n = 0; n <- t ~ n + 1; n }; \
                let t = select step { 1 | 4 | 8 => step, _ => never() }; \
                seq t { let a = counter(t); a }";
    assert_eq!(values(body, mode).await?, [0, 0, 0]);
    Ok(())
}

// A write through a reference goes where the reference pointed at the
// writing step's entry, a statement's and a try's alike: both land in `a`
// though `target` moves to `b` before their values fire.
async fn a_write_through_a_reference_reads_it_at_entry(mode: Mode) -> Result<()> {
    let body = "let a = 0; let b = 0; \
                let target = select step { n if n < 4 => &mut a, _ => &mut b }; \
                let t = select step { 1 => 1, _ => never() }; \
                seq t { *target <- live(select step { 5 => 42, _ => never() }); 1 }; \
                seq t { \
                  *target <- try { live(select step { 6 => 43, _ => never() }) } \
                  with(e) { 0 }; \
                  1 \
                }; \
                select step { 9 => a * 100 + b, _ => never() }";
    assert_eq!(values(body, mode).await?, [4300]);
    Ok(())
}

// A seqq capture that a callee writes is read at the step's entry, as the
// same write spelled in the seq is: the step stalls on the entry value.
async fn a_capture_a_callee_writes_is_read_at_entry(mode: Mode) -> Result<()> {
    for write in ["put(1)", "x <- 1"] {
        let body = format!(
            "let x = 0; x <- select step {{ 6 => 10, _ => never() }}; \
             let put = |v| {{ x <- v; v }}; \
             let t = select step {{ 1 => 1, _ => never() }}; \
             seqq t {{ {write}; select x > 2 {{ true => x, false => never() }} }}"
        );
        assert_eq!(values(&body, mode).await?, [0i64; 0], "{write}");
    }
    Ok(())
}

// A callee stays at its call site whatever its binding's type before the
// check: a call through an alias resolves statically, so it writes nothing
// and the capture keeps the request's value.
async fn a_callee_is_never_captured(mode: Mode) -> Result<()> {
    let body = "let x = 0; x <- select step { 3 => 200, _ => never() }; \
                let g0 = |k: i64| k + 1; let g = g0; \
                let t = select step { 1 => 1, _ => never() }; \
                seqq t { until live(step) > 5; let c = g(0); x * 1000 + c }";
    assert_eq!(values(body, mode).await?, [1]);
    Ok(())
}

// A builtin handed a function that writes nothing (a pure callback, a
// nested seq's busy gate) leaves the captures queued; one handed a
// function that writes a capture makes it live.
async fn a_callback_that_writes_nothing_keeps_the_captures(mode: Mode) -> Result<()> {
    for (stmt, want) in [
        ("let y = filter(t, |v| v > 0)", 0),
        ("let y = seq { 1 }", 0),
        ("let y = filter(t, |v| { x <- v + 100; v > 0 })", 6),
    ] {
        let body = format!(
            "let x = 0; x <- step ~ step; \
             let t = select step {{ 1 => 1, _ => never() }}; \
             seqq t {{ until live(step) > 5; {stmt}; x }}"
        );
        assert_eq!(values(&body, mode).await?, [want], "{stmt}");
    }
    Ok(())
}

// An until that reads only the snapshot and calls nothing cannot change
// after its entry, and the check warns of it; one with a live part, or
// one reading through a reference, is quiet.
async fn a_fixed_until_warns(mode: Mode) -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let flags = mode.flags() | CFlag::WarningsAreErrors;
    let ctx = TestCtx::start(
        config(tx, &crate::TEST_REGISTER, vec![], |ctx| {
            ctx.control.set_par_mode(mode.par())
        })?
        .flags(flags),
    )
    .await?;
    ctx.rt.compile(literal!("{ let r = false; seq { until live(r); 1 } }")).await?;
    ctx.rt
        .compile(literal!("{ let x = 0; let r = &x; seq { until *r > 3; 1 } }"))
        .await?;
    let error = ctx
        .rt
        .compile(literal!("{ let r = false; seq { until r; 1 } }"))
        .await
        .unwrap_err();
    assert!(
        format!("{error:#}").contains("cannot change after the step is entered"),
        "{error:#}"
    );
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn live_outside_a_seq_is_refused() -> Result<()> {
    super::refused_everywhere("live(1)", "means nothing outside a seq block").await
}

modes!(entry_snapshot_and_live);
modes!(absent_at_entry_is_read_at_arrival);
modes!(a_run_reads_its_own_writes);
modes!(a_callee_reads_live);
modes!(a_callee_born_with_a_constant_answers_it);
modes!(a_fixed_until_warns);
modes!(a_write_through_a_reference_reads_it_at_entry);
modes!(a_capture_a_callee_writes_is_read_at_entry);
modes!(a_callee_is_never_captured);
modes!(a_callback_that_writes_nothing_keeps_the_captures);
