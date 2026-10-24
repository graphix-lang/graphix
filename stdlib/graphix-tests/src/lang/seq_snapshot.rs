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

// An until that reads only the snapshot and calls nothing cannot change
// after its entry, and the check warns of it; one with a live part is
// quiet.
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
