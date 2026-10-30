// seq abort and flush (design/seq_blocks.md §9), and the machine's reset
// when its arm sleeps.

use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::testing::{Mode, TestCtx, as_i64s, config};
use tokio::sync::mpsc;

const STEPS: &str = r#"
    let step = 0;
    step <- select step { n if n < 60 => n + 1, _ => never() };
    let go = select step { 1 | 30 => step, _ => never() };
"#;

async fn values(body: &str, mode: Mode) -> Result<Vec<i64>> {
    let code = format!("{{ {STEPS} {body} }}");
    let (values, _) = run_delta(&code, mode).await?;
    Ok(as_i64s(&values))
}

async fn abort_ends_run(mode: Mode) -> Result<()> {
    let body = r#"
        let cancel = select step { 8 => step, _ => never() };
        seq go; abort(cancel) { until live(step) > go + 15; go }
    "#;
    assert_eq!(values(body, mode).await?, [30]);
    Ok(())
}

// A fire between runs belongs to no run, and neither does the value it
// leaves standing.
async fn idle_fire_is_dropped(mode: Mode) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let body = format!(
            r#"
            let cancel = never();
            cancel <- select step {{ 20 => step, _ => never() }};
            {kw} go; abort(cancel) {{ until live(step) > go + 5; go }}
        "#
        );
        assert_eq!(values(&body, mode).await?, [1, 30], "{kw}");
    }
    Ok(())
}

// An event firing in its run's entry cycle does not count: `c` fires as
// each run enters, and both runs finish.
async fn entry_fire_is_not_an_abort(mode: Mode) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let code = format!(
            r#"{{
            let step = 0;
            step <- select step {{ n if n < 60 => n + 1, _ => never() }};
            let go = select step {{ 1 | 10 => step, _ => never() }};
            {kw} let c = go; abort(c) {{ until live(step) > c + 3; c }}
        }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 10], "{kw}");
    }
    Ok(())
}

// The abort fires in the cycle the last step would complete in.
async fn abort_beats_completion(mode: Mode) -> Result<()> {
    for cancel_at in 5..12 {
        let body = format!(
            r#"
            let cancel = select step {{ {cancel_at} => step, _ => never() }};
            let ran = 0;
            let r = seq go; abort(cancel) {{ until live(step) > go + 6; ran <- ran + 1; go }};
            select step {{ 60 => ran, _ => never() }}
        "#
        );
        let ran = values(&body, mode).await?;
        let expected = if cancel_at < 9 { 1 } else { 2 };
        assert_eq!(ran, [expected], "cancel at {cancel_at}");
    }
    Ok(())
}

async fn abort_bypasses_try(mode: Mode) -> Result<()> {
    for cancel_at in 5..10 {
        let body = format!(
            r#"
            let cancel = select step {{ {cancel_at} => step, _ => never() }};
            let handled = 0;
            let r = seq go; abort(cancel) {{
                let v = try {{ until live(step) > go + 6; go }} with(_) {{ handled <- handled + 1; -1 }};
                v
            }};
            select step {{ 60 => handled, _ => never() }}
        "#
        );
        assert_eq!(values(&body, mode).await?, [0], "cancel at {cancel_at}");
        let body = format!(
            r#"
            let cancel = select step {{ {cancel_at} => step, _ => never() }};
            seq go; abort(cancel) {{
                let v = try {{ until live(step) > go + 6; go }} with(_) {{ -1 }};
                v
            }}
        "#
        );
        assert_eq!(values(&body, mode).await?, [30], "cancel at {cancel_at}");
    }
    Ok(())
}

// The event is an initial step: a timer in it starts with each run. Run 1
// finishes inside its 140 ms; run 2 outlasts its own. A timer started once
// at init would fire while idle and let run 2 finish.
async fn abort_timer_starts_with_the_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:40.ms, 16)?);
        let go = select tick { 1 | 9 => tick, _ => never() };
        seq go; abort(sys::time::timer(duration:140.ms, false)) {
            until live(tick) > go + select go { 1 => 1, _ => 6 };
            go
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1]);
    Ok(())
}

// The abort's event reads the run's trigger `c`, not the live `go`: run 1
// is aborted 140 ms after its start, go = 3 arrives mid-run (dropped by
// seq, queued by seqq), and go = 9 runs alone. Only run 1 has an abort,
// which a live `go` would take away when 3 arrives.
async fn abort_reads_the_trigger(mode: Mode) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let code = format!(
            r#"{{
            let tick = count(sys::time::timer(duration:40.ms, 16)?);
            let go = select tick {{ 1 | 3 | 9 => tick, _ => never() }};
            let stop = |c| select c {{ 1 => sys::time::after_idle(duration:140.ms, c), _ => never() }};
            {kw} let c = go; abort(stop(c)) {{
                sys::time::after_idle(select c {{ 1 => duration:200.ms, _ => duration:5.ms }}, c)
            }}
        }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        let expected: &[i64] = if kw == "seq" { &[9] } else { &[3, 9] };
        assert_eq!(as_i64s(&values), expected, "{kw}");
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

async fn seqq_abort_starts_the_next(mode: Mode) -> Result<()> {
    let code = format!("{{ {BURST} seqq request; abort(cancel) {{ work(request) }} }}");
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [2, 3, 12]);
    Ok(())
}

async fn seqq_flush_empties_the_queue(mode: Mode) -> Result<()> {
    let code = format!("{{ {BURST} seqq request; flush(cancel) {{ work(request) }} }}");
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [12]);
    Ok(())
}

// A request arriving with the flush is kept, whether the flush reads the
// run's request (it reaches the queue a cycle late) or not: request 1 is
// running, 2 waits, the flush at step 5 clears 2, and 5 and 8 run.
async fn seqq_flush_keeps_a_request_arriving_with_it(mode: Mode) -> Result<()> {
    for flush in ["req", "true"] {
        let code = format!(
            r#"{{
            let step = 0;
            step <- select step {{ s if s < 30 => s + 1, _ => never() }};
            let req = select step {{ 1 => 1, 2 => 2, 5 => 5, 8 => 8, _ => never() }};
            seqq req; flush(select step {{ 5 => {flush}, _ => never() }}) {{
                until req != 1;
                req
            }}
        }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [5, 8], "flush({flush})");
    }
    Ok(())
}

// A flush drops the requests still waiting for a capture, as it drops
// the queued ones: 1 and 2 wait for `ready`, the flush at step 4 drops
// them, and 8, requested after it, runs once `ready` arrives.
async fn seqq_flush_drops_a_request_waiting_for_a_capture(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 20 => s + 1, _ => never() };
        let req = select step { 1 => 1, 2 => 2, 8 => 8, _ => never() };
        let ready = never<i64>();
        ready <- select step { 6 => 7, _ => never() };
        seqq req; flush(select step { 4 => true, _ => never() }) {
            req * 10 + ready
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [87]);
    Ok(())
}

// A seqq in a seq step is born again at each run, its flush with it. Run 1
// requests 1, flushes it while it waits for `ready` and is aborted; run 2's
// request 2, made after that flush, runs.
async fn seqq_flush_in_a_step_drops_nothing_of_the_next_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 30 => s + 1, _ => never() };
        let go = select step { 1 => 1, 10 => 2, _ => never() };
        let cancel = select step { 6 => true, _ => never() };
        let req = select step { 2 => 1, 12 => 2, _ => never() };
        let ready = never<i64>();
        ready <- select step { 8 => 7, _ => never() };
        let fl = select step { 4 => true, _ => never() };
        seq go; abort(cancel) {
            seqq req; flush(fl) { req * 10 + ready }
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [27]);
    Ok(())
}

// Run 1 serves request 1; in run 2, request 2 waits for `ready` and the
// flush drops it, and request 3, made after the flush, runs.
async fn seqq_flush_in_a_step_drops_a_waiting_request_of_its_run(
    mode: Mode,
) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 30 => s + 1, _ => never() };
        let go = select step { 1 => 1, 8 => 2, _ => never() };
        let req = select step { 2 => 1, 9 => 2, 20 => 3, _ => never() };
        let ready = select step { s if s < 5 => 7, s if s > 15 => 7, _ => never() };
        let fl = select step { 11 => true, _ => never() };
        seq go {
            seqq req; flush(fl) { req * 10 + ready }
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [17, 37]);
    Ok(())
}

// A machine whose arm sleeps mid-run is idle when the arm wakes.
async fn sleep_resets_the_machine(mode: Mode) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let code = format!(
            r#"{{
            let step = 0;
            step <- select step {{ n if n < 60 => n + 1, _ => never() }};
            let on = uniq(select step {{ s if s < 8 => true, s if s < 20 => false, _ => true }});
            select on {{
                true => {kw} on {{ let a = step; until live(step) > a + 12; a }},
                false => never()
            }}
        }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        let values = as_i64s(&values);
        assert_eq!(values.len(), 1, "{kw}: {values:?}");
        assert!(values[0] >= 20, "{kw}: {values:?}");
    }
    Ok(())
}

// An outer run's abort leaves a nested machine mid-run; the next outer
// run starts it afresh.
async fn nested_machine_restarts(mode: Mode) -> Result<()> {
    let body = r#"
        let cancel = select step { 8 => step, _ => never() };
        let slow = |x: i64| seq x { until live(step) > x + 15; x };
        seq go; abort(cancel) { let a = slow(go); a }
    "#;
    assert_eq!(values(body, mode).await?, [30]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn flush_needs_a_queue() -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let ctx = TestCtx::start(
        config(tx, &crate::TEST_REGISTER, vec![], |_| ())?
            .flags(graphix_compiler::BitFlags::empty()),
    )
    .await?;
    let error = ctx
        .rt
        .compile(arcstr::literal!("{ let t = 1; seq t; flush(t) { t } }"))
        .await
        .unwrap_err();
    assert!(format!("{error:#}").contains("a seq has none"), "{error:#}");
    ctx.shutdown().await;
    Ok(())
}

// A nested seq's abort is its own machine's step: a call in it reads
// its inputs live, not a snapshot the outer step took at its entry.
async fn nested_abort_reads_live(mode: Mode) -> Result<()> {
    let body = r#"
        let stop = |c: bool| select c { true => c, false => never() };
        let ran = 0;
        let r = seq go { seq abort(stop(step > 8)) { until live(step) > go + 15; ran <- ran + 1; go } };
        select step { 60 => ran, _ => never() }
    "#;
    assert_eq!(values(body, mode).await?, [0]);
    Ok(())
}

// A run raises 30 errors at once, so their deliveries queue a cycle each;
// its arm sleeps while they drain, giving them up. An abort after the wake,
// in a cycle one of them still arrives in, neither counts that stale
// delivery as a raise (the enclosing catch would lose one it waits on) nor
// leaves the machine busy: a later trigger starts a run.
async fn abort_after_given_up_errors(mode: Mode) -> Result<()> {
    let code = r#"{
  let n = 0;
  n <- select n { x if x < 60 => x + 1, _ => never() };
  let xs = array::init(30, |i| i);
  let trig = filter(n, |x| (x == 3) || (x == 14) || (x == 40));
  let ab = filter(n, |x| x == 18);
  let out = select n {
    x if (x >= 8) && (x < 12) => -1,
    _ => {
      catch(e) println("outer [n]");
      seq let t = trig; abort(ab) {
        println("run [t]");
        let r = select t {
          3 => array::map(xs, |x| (x /? (x - x))?),
          40 => [1, 2],
          _ => never<Array<i64>>()
        };
        array::len(r)
      }
    }
  };
  select n { 60 => out, _ => never() }
}"#;
    let (values, out) = run_delta(code, mode).await?;
    assert!(out.contains("run 40"), "the trigger after the abort started no run: {out}");
    assert_eq!(as_i64s(&values), [2]);
    Ok(())
}

// The budget restarts with every run, not only the first: runs 1, 8 and
// 15 each outlast their 140 ms and are aborted, each trigger after an
// abort starts a run, and run 22 finishes inside its own budget.
async fn abort_timer_budgets_every_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:40.ms, 30)?);
        let go = select tick { 1 | 8 | 15 | 22 => tick, _ => never() };
        seq go; abort(sys::time::timer(duration:140.ms, false)) {
            until live(tick) > go + select go { 22 => 1, _ => 6 };
            go
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [22]);
    Ok(())
}

modes!(abort_after_given_up_errors);
modes!(abort_timer_budgets_every_run);
modes!(abort_ends_run);
modes!(idle_fire_is_dropped);
modes!(entry_fire_is_not_an_abort);
modes!(abort_beats_completion);
modes!(abort_bypasses_try);
modes!(abort_timer_starts_with_the_run);
modes!(abort_reads_the_trigger);
modes!(seqq_abort_starts_the_next);
modes!(seqq_flush_empties_the_queue);
modes!(seqq_flush_keeps_a_request_arriving_with_it);
modes!(seqq_flush_drops_a_request_waiting_for_a_capture);
modes!(seqq_flush_in_a_step_drops_nothing_of_the_next_run);
modes!(seqq_flush_in_a_step_drops_a_waiting_request_of_its_run);
modes!(sleep_resets_the_machine);
modes!(nested_machine_restarts);
modes!(nested_abort_reads_live);

// An abort that alone reads a bare trigger reads the run's snapshot of it:
// the busy-dropped fires that follow do not move it.
async fn abort_reads_trigger_snapshot(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 40 => n + 1, _ => never() };
        let go = select step { n if n < 20 && n % 2 == 1 => n, _ => never() };
        seq go; abort(select step - go { 6 => step, _ => never() }) {
            until live(step) > 30;
            0
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), Vec::<i64>::new());
    Ok(())
}
modes!(abort_reads_trigger_snapshot);
