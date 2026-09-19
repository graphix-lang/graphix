// seq abort and flush (design/seq_blocks.md §9), and the machine's reset
// when its arm sleeps.

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::testing::init_with_flags_and_setup;
use tokio::sync::mpsc;

const STEPS: &str = r#"
    let step = 0;
    step <- select step { n if n < 60 => n + 1, _ => never() };
    let go = select step { 1 | 30 => step, _ => never() };
"#;

async fn values(body: &str, fusion_disabled: bool) -> Result<Vec<i64>> {
    let code = format!("{{ {STEPS} {body} }}");
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    Ok(as_i64s(&values))
}

async fn abort_ends_run(fusion_disabled: bool) -> Result<()> {
    let body = r#"
        let cancel = select step { 8 => step, _ => never() };
        seq go abort(cancel) { until step > go + 15; go }
    "#;
    assert_eq!(values(body, fusion_disabled).await?, [30]);
    Ok(())
}

// A fire between runs belongs to no run, and neither does the value it
// leaves standing.
async fn idle_fire_is_dropped(fusion_disabled: bool) -> Result<()> {
    let body = r#"
        let cancel = select step { 20 => step, _ => never() };
        seq go abort(cancel) { until step > go + 5; go }
    "#;
    assert_eq!(values(body, fusion_disabled).await?, [1, 30]);
    Ok(())
}

// The abort fires in the cycle the last step would complete in.
async fn abort_beats_completion(fusion_disabled: bool) -> Result<()> {
    for cancel_at in 5..12 {
        let body = format!(
            r#"
            let cancel = select step {{ {cancel_at} => step, _ => never() }};
            let ran = 0;
            let r = seq go abort(cancel) {{ until step > go + 6; ran <- ran + 1; go }};
            select step {{ 60 => ran, _ => never() }}
        "#
        );
        let ran = values(&body, fusion_disabled).await?;
        let expected = if cancel_at < 10 { 1 } else { 2 };
        assert_eq!(ran, [expected], "cancel at {cancel_at}");
    }
    Ok(())
}

async fn abort_bypasses_try(fusion_disabled: bool) -> Result<()> {
    for cancel_at in 5..10 {
        let body = format!(
            r#"
            let cancel = select step {{ {cancel_at} => step, _ => never() }};
            let handled = 0;
            let r = seq go abort(cancel) {{
                let v = try {{ until step > go + 6; go }} with(_) {{ handled <- handled + 1; -1 }};
                v
            }};
            select step {{ 60 => handled, _ => never() }}
        "#
        );
        assert_eq!(values(&body, fusion_disabled).await?, [0], "cancel at {cancel_at}");
        let body = format!(
            r#"
            let cancel = select step {{ {cancel_at} => step, _ => never() }};
            seq go abort(cancel) {{
                let v = try {{ until step > go + 6; go }} with(_) {{ -1 }};
                v
            }}
        "#
        );
        assert_eq!(values(&body, fusion_disabled).await?, [30], "cancel at {cancel_at}");
    }
    Ok(())
}

// The event is an initial step: a timer in it starts with each run.
async fn abort_timer_starts_with_the_run(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:40.ms, 16)?);
        let go = select tick { 1 | 9 => tick, _ => never() };
        seq go abort(sys::time::timer(duration:140.ms, false)) {
            until tick > go + select go { 1 => 6, _ => 1 };
            go
        }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [9]);
    Ok(())
}

async fn abort_reads_the_trigger(fusion_disabled: bool) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let code = format!(
            r#"{{
            let tick = count(sys::time::timer(duration:40.ms, 16)?);
            let go = select tick {{ 1 | 9 => tick, _ => never() }};
            {kw} let c = go abort(sys::time::after_idle(duration:140.ms, c)) {{
                sys::time::after_idle(select c {{ 1 => duration:2.s, _ => duration:5.ms }}, c)
            }}
        }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [9], "{kw}");
    }
    Ok(())
}

// Requests 1, 2 and 3 arrive together and 1 is slow; 12 arrives after
// the cancel.
const BURST: &str = r#"
    let tick = count(sys::time::timer(duration:40.ms, 16)?);
    let request = select tick { 1 | 2 | 3 | 12 => tick, _ => never() };
    let cancel = select tick { 6 => tick, _ => never() };
    let work = |r: i64| sys::time::after_idle(select r { 1 => duration:2.s, _ => duration:5.ms }, r);
"#;

async fn seqq_abort_starts_the_next(fusion_disabled: bool) -> Result<()> {
    let code = format!("{{ {BURST} seqq request abort(cancel) {{ work(request) }} }}");
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [2, 3, 12]);
    Ok(())
}

async fn seqq_flush_empties_the_queue(fusion_disabled: bool) -> Result<()> {
    let code = format!("{{ {BURST} seqq request flush(cancel) {{ work(request) }} }}");
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [12]);
    Ok(())
}

// A machine whose arm sleeps mid-run is idle when the arm wakes.
async fn sleep_resets_the_machine(fusion_disabled: bool) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let code = format!(
            r#"{{
            let step = 0;
            step <- select step {{ n if n < 60 => n + 1, _ => never() }};
            let on = uniq(select step {{ s if s < 8 => true, s if s < 20 => false, _ => true }});
            select on {{
                true => {kw} {{ let a = step; until step > a + 12; a }},
                false => never()
            }}
        }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        let values = as_i64s(&values);
        assert_eq!(values.len(), 1, "{kw}: {values:?}");
        assert!(values[0] >= 20, "{kw}: {values:?}");
    }
    Ok(())
}

// An outer run's abort leaves a nested machine mid-run; the next outer
// run starts it afresh.
async fn nested_machine_restarts(fusion_disabled: bool) -> Result<()> {
    let body = r#"
        let cancel = select step { 8 => step, _ => never() };
        let slow = |x: i64| seq x { until step > x + 15; x };
        seq go abort(cancel) { let a = slow(go); a }
    "#;
    assert_eq!(values(body, fusion_disabled).await?, [30]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn flush_needs_a_queue() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![],
        graphix_compiler::BitFlags::empty(),
        |_| (),
    )
    .await?;
    let error = ctx
        .rt
        .compile(arcstr::literal!("{ let t = 1; seq t flush(t) { t } }"))
        .await
        .unwrap_err();
    assert!(format!("{error:#}").contains("a seq has none"), "{error:#}");
    ctx.shutdown().await;
    Ok(())
}

macro_rules! modes {
    ($test:ident, $interp:ident, $jit:ident) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $interp() -> Result<()> {
            $test(true).await
        }
        #[tokio::test(flavor = "current_thread")]
        async fn $jit() -> Result<()> {
            $test(false).await
        }
    };
}

modes!(abort_ends_run, abort_ends_run_interp, abort_ends_run_jit);
modes!(idle_fire_is_dropped, idle_fire_is_dropped_interp, idle_fire_is_dropped_jit);
modes!(abort_beats_completion, abort_beats_completion_interp, abort_beats_completion_jit);
modes!(abort_bypasses_try, abort_bypasses_try_interp, abort_bypasses_try_jit);
modes!(
    abort_timer_starts_with_the_run,
    abort_timer_starts_with_the_run_interp,
    abort_timer_starts_with_the_run_jit
);
modes!(
    abort_reads_the_trigger,
    abort_reads_the_trigger_interp,
    abort_reads_the_trigger_jit
);
modes!(
    seqq_abort_starts_the_next,
    seqq_abort_starts_the_next_interp,
    seqq_abort_starts_the_next_jit
);
modes!(
    seqq_flush_empties_the_queue,
    seqq_flush_empties_the_queue_interp,
    seqq_flush_empties_the_queue_jit
);
modes!(
    sleep_resets_the_machine,
    sleep_resets_the_machine_interp,
    sleep_resets_the_machine_jit
);
modes!(
    nested_machine_restarts,
    nested_machine_restarts_interp,
    nested_machine_restarts_jit
);
