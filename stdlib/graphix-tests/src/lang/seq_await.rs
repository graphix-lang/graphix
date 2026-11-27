//! `await(e)`: a seq step that waits for the next event of `e`
//! (`design/seq_blocks.md`). It completes on the first fire of `e` since
//! its run was started that no earlier step of the run read.

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::CFlag;
use graphix_package_core::{
    run,
    testing::{Mode, TestCtx, config, prints, refused},
};

/// A clock of twelve steps and a stream `ev`, with no event yet.
macro_rules! clocked {
    ($body:literal) => {
        concat!(
            "{ let step = 0; step <- select step { n if n < 12 => n + 1, _ => never() }; ",
            "let ev = never<i64>(); ",
            $body,
            " }"
        )
    };
}

// Two awaits in a row take two events, the second one cycle after the
// first or later: an event completes one await.
run!(
    two_awaits_take_two_events,
    clocked!(
        "ev <- select step { 4 => 40, 5 => 50, 9 => 90, _ => never() }; \
         let t = select step { 2 => null, _ => never() }; \
         seq t { let a = await(ev); let b = await(ev); let c = await(ev); \
                 (a, b, c, live(step)) }"
    ),
    prints("[i64:40, i64:50, i64:90, i64:10]")
);

// With one event the second await waits: the seq does not complete.
run!(
    one_event_completes_one_await,
    clocked!(
        "ev <- select step { 4 => 40, _ => never() }; \
         let t = select step { 2 => null, _ => never() }; \
         let v = seq t { await(ev); let r = await(ev); r }; \
         any(v, select step { 12 => -1, _ => never() })"
    ),
    prints("i64:-1")
);

// An event that fired while an earlier step of the run was under way is
// not lost: the await takes it when it is entered.
run!(
    an_event_during_an_earlier_step_is_taken,
    clocked!(
        "ev <- select step { 3 => 30, 8 => 80, _ => never() }; \
         let t = select step { 1 => null, _ => never() }; \
         seq t { until live(step) >= 5; let got = await(ev); (got, live(step)) }"
    ),
    prints("[i64:30, i64:6]")
);

// An event from before the run was started is not this run's: the await
// waits for the next, where `live(once(ev))` takes the one that stands
// unread.
run!(
    an_event_before_the_run_is_waited_past,
    clocked!(
        "ev <- select step { 1 => 10, 8 => 80, _ => never() }; \
         let t = select step { 5 => null, _ => never() }; \
         let a = seq t { let got = await(ev); (got, live(step)) }; \
         let b = seq t { let got = live(once(ev)); (got, live(step)) }; \
         (a, a ~ b)"
    ),
    prints("[[i64:80, i64:9], [i64:10, i64:6]]")
);

// The cycle the run's trigger fires in counts as the run.
run!(
    an_event_in_the_triggers_cycle_is_the_runs,
    clocked!(
        "ev <- select step { 4 => 40, _ => never() }; \
         let t = select step { 5 => null, _ => never() }; \
         seq t { let got = await(ev); (got, live(step)) }"
    ),
    prints("[i64:40, i64:6]")
);

// An event between two runs is neither's.
run!(
    an_event_between_two_runs_is_waited_past,
    clocked!(
        "ev <- select step { 3 => 30, 6 => 60, 10 => 100, _ => never() }; \
         let t = select step { 1 => null, 8 => null, _ => never() }; \
         let v = seq t { let got = await(ev); (got, live(step)) }; \
         array::group(v, |i, _| i == 2)"
    ),
    prints("[[i64:30, i64:4], [i64:100, i64:11]]")
);

// Each queued run starts its own.
run!(
    a_queued_run_awaits_from_its_own_start,
    clocked!(
        "ev <- select step { 3 => 30, 6 => 60, _ => never() }; \
         let t = select step { 1 => 1, 2 => 2, _ => never() }; \
         let v = seqq t { let got = await(ev); (t, got, live(step)) }; \
         array::group(v, |i, _| i == 2)"
    ),
    prints("[[i64:1, i64:30, i64:4], [i64:2, i64:60, i64:8]]")
);

// A bare `await(e);` waits and discards, and one is a step in a try body.
run!(
    await_is_a_statement_and_a_step_of_a_try,
    clocked!(
        "ev <- select step { 3 => 30, 6 => 60, _ => never() }; \
         let t = select step { 1 => null, _ => never() }; \
         seq t { await(ev); let r = try { let got = await(ev); got + 1 } with(e) { -1 }; \
                 (r, live(step)) }"
    ),
    prints("[i64:61, i64:8]")
);

run!(
    await_nested_in_a_call_is_refused,
    "{ let t = 1; let ev = 1; seq t { let n = str::len(\"[await(ev)]\"); n } }",
    refused("write it as a statement of the seq")
);

run!(
    await_outside_a_seq_is_refused,
    "{ let ev = 1; await(ev) }",
    refused("as a step of a seq block")
);

run!(
    await_in_an_on_block_is_refused,
    "{ let k = 1; let ev = 1; on k { await(ev) } }",
    refused("as a step of a seq block")
);

run!(
    await_is_no_name,
    "{ let await = 1; await }",
    refused("can't use keyword as a function or variable name")
);

// A step that is a bare `live` read of a variable waits for nothing, and
// is warned of; one that awaits, one that reads through `live` in a
// larger expression and an `until live(..)` are not.
async fn idle_live_step_warning(mode: Mode) -> Result<()> {
    let (tx, _rx) = tokio::sync::mpsc::channel(100);
    let flags = mode.flags() | CFlag::WarningsAreErrors;
    let ctx = TestCtx::start(
        config(tx, &crate::TEST_REGISTER, vec![], |ctx| {
            ctx.control.set_par_mode(mode.par())
        })?
        .flags(flags),
    )
    .await?;
    const HEAD: &str = "let t = 1; let ev = 1; let ready = true;";
    for body in [
        "seq t { let r = await(ev); r }",
        "seq t { let r = live(ev) + 1; r }",
        "seq t { until live(ready); ev }",
        "seq t { let r = live(once(ev)); r }",
    ] {
        let src = ArcStr::from(format!("{{ {HEAD} {body} }}"));
        ctx.rt.compile(src.clone()).await.map_err(|e| anyhow::anyhow!("{src}: {e:#}"))?;
    }
    for body in [
        "seq t { let r = live(ev); r }",
        "seq t { live(ev); ev }",
        "seq t { let r = try { let x = live(ev); x } with(e) { 0 }; r }",
    ] {
        let src = ArcStr::from(format!("{{ {HEAD} {body} }}"));
        let error = ctx.rt.compile(src.clone()).await.expect_err(&src);
        let error = format!("{error:#}");
        assert!(error.contains("which waits for nothing"), "{src}: {error}");
    }
    ctx.shutdown().await;
    Ok(())
}

modes!(idle_live_step_warning);
