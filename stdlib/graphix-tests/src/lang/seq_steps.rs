// What a step sees of the steps before it (`design/dependency_summaries.md`):
// every write an earlier step makes is seen by a later step that reads it,
// through a call or a reference as well as a `<-`, and a passed step is
// asleep.

use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, as_i64s},
};
use netidx::publisher::Value;

async fn run(code: &str, mode: Mode) -> Result<Vec<i64>> {
    Ok(as_i64s(&run_delta(code, mode).await?.0))
}

// A closure's write to a variable it captured is seen by the next step.
async fn closure_write_is_seen(mode: Mode) -> Result<()> {
    let code = r#"{
        let b = 0;
        let put = |v| { b <- v; v };
        let go = 1;
        seq go { put(5); let s = b; s }
    }"#;
    assert_eq!(run(code, mode).await?, [5]);
    Ok(())
}

// So is a callee's write through a reference that reached it through a
// variable, not a `&` written in the seq.
async fn ref_write_through_a_variable_is_seen(mode: Mode) -> Result<()> {
    let code = r#"{
        let c = 0;
        let r = &mut c;
        let set = |p: &mut i64, v| { *p <- v; v };
        let go = 1;
        seq go { set(r, 5); let s = c; s }
    }"#;
    assert_eq!(run(code, mode).await?, [5]);
    Ok(())
}

// A variable only a callee writes is live under `seqq`, not captured.
async fn seqq_callee_write_is_live(mode: Mode) -> Result<()> {
    let code = r#"{
        let b = 0;
        let put = |v| { b <- v; v };
        let go = 1;
        seqq go { put(5); until true; let s = b; s }
    }"#;
    assert_eq!(run(code, mode).await?, [5]);
    Ok(())
}

// A reference handed to a callee that only reads through it makes no
// capture live: each queued run reads `e` as it was enqueued.
async fn seqq_ref_argument_keeps_captures(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 4 => n + 1, _ => never() };
        let request = select step { 1 | 2 | 3 => step, _ => never() };
        let e = 0;
        e <- request ~ e + 10;
        let d = 0;
        let q = &mut d;
        let look = |p: &mut i64| *p;
        seqq request { look(q); e }
    }"#;
    assert_eq!(run(code, mode).await?, [0, 10, 20]);
    Ok(())
}

// A let binds its step's production for the rest of the run: a later
// change of its source while a later step waits is not seen.
async fn passed_let_does_not_track(mode: Mode) -> Result<()> {
    let code = r#"{
        let v = 0;
        let w = never();
        v <- sys::time::after_idle(duration:50.ms, 7);
        w <- sys::time::after_idle(duration:100.ms, 1);
        let go = 1;
        seq go { let a = v; let b = w; a + b }
    }"#;
    assert_eq!(run(code, mode).await?, [1]);
    Ok(())
}

// An `until` whose condition already holds when it is entered completes
// at its entry.
async fn until_true_at_entry(mode: Mode) -> Result<()> {
    let code = r#"{
        let f = |x| x + 1;
        let go = 1;
        seq go { let a = f(0); until a == 1; a + 1 }
    }"#;
    assert_eq!(run(code, mode).await?, [2]);
    Ok(())
}

// A let's fire reaches the first step that reads it in a later cycle,
// though a step of its own cycle read it too: `count` under `live` takes
// `a`'s one fire at the last step's entry.
async fn let_fire_reaches_a_later_cycle(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 8 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ready = select step { n if n >= 4 => true, _ => false };
        seq go { let a = go; let b = live(a); until live(ready); live(count(a)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1]);
    Ok(())
}

// A let's fire a step read live at its own entry is not raised again to
// that step in a later cycle: `a` fires once, so `count(a)` stays 1 and
// the `until` never completes.
async fn let_fire_read_live_is_not_raised_again(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 8 => n + 1, _ => never() };
        let go = select step { 2 => step, _ => never() };
        seq go { let a = go; until count(a) >= 2; a }
    }"#;
    assert_eq!(run(code, mode).await?, Vec::<i64>::new());
    Ok(())
}

// `until` starts a cycle after a write it reads: it sees `false`, and
// the run waits.
async fn until_sees_the_write_before_it(mode: Mode) -> Result<()> {
    let code = r#"{
        let flag = true;
        seq { flag <- false; until flag; 42 }
    }"#;
    assert_eq!(run(code, mode).await?, [] as [i64; 0]);
    Ok(())
}

// So does `try`: its body reads the 7.
async fn try_sees_the_write_before_it(mode: Mode) -> Result<()> {
    let code = r#"{
        let f = |x: i64| -> Result<i64, `E> select x { 0 => error(`E), n => n };
        let x = 0;
        seq { x <- 7; let v = try { f(x)? } with(_) { -1 }; v }
    }"#;
    assert_eq!(run(code, mode).await?, [7]);
    Ok(())
}

// A call with no static target counts as every variable: the next step
// waits for the write it may make, under seq and seqq alike.
async fn dynamic_callee_write_is_seen(mode: Mode) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                let b = 0;
                let k = 1;
                let f = select k {{ 1 => |v| {{ b <- v; v }}, _ => |v| v }};
                let go = 1;
                {form} go {{ f(5); let s = b; s }}
            }}"#
        );
        assert_eq!(run(&code, mode).await?, [5], "{form}");
    }
    Ok(())
}

// A run whose last write targets its own trigger ends before the write
// lands, so the write starts the next run.
async fn own_trigger_write_starts_the_next_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let t = 0;
        seq t { t <- select t { n if n < 5 => n + 1, _ => never() } };
        t
    }"#;
    assert_eq!(run(code, mode).await?, [0, 1, 2, 3, 4, 5]);
    Ok(())
}

async fn kernels_are_reborn_at_each_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let x = select step { 1 | 10 | 20 => step, _ => never() };
        let acc = 0;
        let runs = 0;
        seq x {
            acc <- acc + x;
            acc + array::fold([1], 42, |v0, kv| x);
            runs <- runs + 1;
            runs
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    let printed: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(printed, ["i64:1", "i64:2", "i64:3"]);
    Ok(())
}

async fn restart_builtins_are_reborn_at_each_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let request = select step { 1 | 20 => step, _ => never() };
        let k = 5;
        seq request {
            let c = count(request);
            let t = take(#n: 1, request);
            let u = uniq(request ~ k);
            let h = hold(#clock: request, request);
            let o = once(request);
            let q = queue(#clock: request, array::iter([request, request + 1]));
            let i = array::iterq(#clock: request, [request, request + 1]);
            let r = throttle(#rate: duration:10.s, request);
            (c, t, u, h, o, q, i, r)
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    let printed: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(
        printed,
        [
            "[i64:1, i64:1, i64:5, i64:1, i64:1, i64:1, i64:1, i64:1]",
            "[i64:1, i64:20, i64:5, i64:20, i64:20, i64:20, i64:20, i64:20]"
        ]
    );
    Ok(())
}

modes!(
    kernels_are_reborn_at_each_run,
    closure_write_is_seen,
    ref_write_through_a_variable_is_seen,
    seqq_callee_write_is_live,
    seqq_ref_argument_keeps_captures,
    passed_let_does_not_track,
    until_true_at_entry,
    let_fire_reaches_a_later_cycle,
    let_fire_read_live_is_not_raised_again,
    until_sees_the_write_before_it,
    try_sees_the_write_before_it,
    dynamic_callee_write_is_seen,
    own_trigger_write_starts_the_next_run,
    restart_builtins_are_reborn_at_each_run,
);

// A read still in flight when a run aborts answers nothing after the
// abort and its bytes go back to the stream: the next run reads its own,
// not the one the aborted run asked for.
#[cfg(unix)]
run!(
    an_aborted_runs_read_is_not_the_next_runs_answer,
    r#"{
  use sys::io::Read;
  let opts = |script| sys::process::options(
    #stdio: sys::process::stdio(#stdout: `Pipe), #args: ["-c", script], "sh");
  // A's data lands after run 1 is aborted; B's stands ready for run 2.
  let a = sys::process::spawn(opts("sleep 0.5; printf AAAA"))$.stdout$;
  let b = sys::process::spawn(opts("printf BBBB"))$.stdout$;
  let k = count(sys::time::timer(duration:300.ms, 2));
  seq let n = k; abort(select n == 1 {
    true => sys::time::after_idle(duration:100.ms, n),
    false => never()
  }) {
    let s = select n == 1 { true => a, false => b };
    buffer::to_string_lossy(Read::read(s, u64:100)$)
  }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "BBBB");
    FuseExpect::Jit
);

async fn arm_born_at_each_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let request = array::iter([1, 2, 3, 4]);
        let status = -1;
        seqq let n = request {
            status <- -1;
            let chosen = select sys::time::after_idle(duration:1.ms, n % 2) {
                0 => { status <- 0; 0 },
                _ => { status <- 1; 1 }
            };
            status
        }
    }"#;
    assert_eq!(run(code, mode).await?, [1, 0, 1, 0]);
    Ok(())
}

modes!(arm_born_at_each_run);

async fn local_let_is_sampled_at_each_later_step(mode: Mode) -> Result<()> {
    let code = r#"{
        let note = 0;
        let r = &mut note;
        let a = 0;
        let b = 0;
        let go = 1;
        seq go {
            let v = go + 1;
            a <- v ~ 5;
            *r <- 7;
            b <- v ~ 6;
            v ~ 9
        }
    }"#;
    assert_eq!(run(code, mode).await?, [9]);
    Ok(())
}

modes!(local_let_is_sampled_at_each_later_step);

// A step reads its inputs through its entry snapshot and issues each call
// as a fresh frame, so a fire an input made while the step waited its turn
// reaches only what reads the input raw, once, at the step's entry: here a
// builtin under `live`, which counts `ev`'s one fire.
async fn a_fire_while_a_step_waits_reaches_live(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 8 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { 2 => step, _ => never() };
        let ready = select step { n if n >= 4 => true, _ => false };
        seq go { until live(ready); live(count(ev)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1]);
    Ok(())
}

// So does a nested seq's trigger: `t` fired while the `until` waited, and
// the inner seq runs at its step's entry.
async fn a_trigger_fired_while_its_step_waited_starts_a_nested_seq(
    mode: Mode,
) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 8 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let t = never();
        t <- select step { 2 => step, _ => never() };
        let ready = select step { n if n >= 4 => true, _ => false };
        seq go { until live(ready); seq let k = t { k * 10 } }
    }"#;
    assert_eq!(run(code, mode).await?, [20]);
    Ok(())
}

// A reference's target is learned as its step first runs, and a write to it
// while the step waited is caught up then. The first step takes the
// reference's own fire, so the target's is the one left to owe.
async fn a_target_written_while_a_step_waited_is_caught_up(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 8 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let x = never();
        x <- select step { 2 => step, _ => never() };
        let p = &x;
        let ready = select step { n if n >= 4 => true, _ => false };
        seq go { let q = p; until live(ready); live(count(*p)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1]);
    Ok(())
}

// So is what a callee bound at its dispatch captures.
async fn a_callee_bound_at_a_step_entry_is_caught_up(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 8 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { 2 => step, _ => never() };
        let k = 1;
        let f = select k { 1 => |u: i64| count(ev), _ => |u: i64| 0 };
        let ready = select step { n if n >= 4 => true, _ => false };
        seq go { until live(ready); live(f(0)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1]);
    Ok(())
}

// A fire stays owed until a step reads it: one made before the first run
// and one made between two runs each reach the next entry of the step that
// reads the input.
async fn a_fire_no_step_read_is_owed_to_the_next_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 14 => n + 1, _ => never() };
        let go = select step { 3 | 10 => step, _ => never() };
        let ev = never();
        ev <- select step { 1 | 7 => step, _ => never() };
        seq go { live(count(ev)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1, 1]);
    Ok(())
}

// The step awake when an input fires reads it then, and spends it: the
// next run is owed nothing and waits.
async fn a_fire_an_awake_step_read_is_not_owed_again(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 14 => n + 1, _ => never() };
        let go = select step { 1 | 8 => step, _ => never() };
        let ev = never();
        ev <- select step { 3 => step, _ => never() };
        seq go { live(count(ev)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1]);
    Ok(())
}

// So does a nested seq: its trigger fired once, in the first run, so the
// second run waits for the next fire and the third is dropped.
async fn a_nested_seq_waits_for_its_trigger_in_every_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 24 => n + 1, _ => never() };
        let go = select step { 1 | 9 | 17 => step, _ => never() };
        let t = never();
        t <- select step { TS => step, _ => never() };
        seq go { seq let k = t { k * 10 } }
    }"#;
    assert_eq!(run(&code.replace("TS", "3"), mode).await?, [30]);
    assert_eq!(run(&code.replace("TS", "3 | 12"), mode).await?, [30, 120]);
    assert_eq!(run(&code.replace("TS", "3 | 8"), mode).await?, [30, 80]);
    Ok(())
}

// A function written in a step is born with it at each run, which writes
// the call that holds it nothing: a sample in a builtin's callback fires
// on its trigger alone, as it does in a function the step names.
async fn a_callback_born_with_its_step_fires_nothing(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 24 => n + 1, _ => never() };
        let go = select step { 1 | 9 | 17 => step, _ => never() };
        let t = never();
        t <- select step { 3 => step, _ => never() };
        let p = |x| x ~ true;
        seq go { live(count(CALL)) }
    }"#;
    let calls = [
        "filter(t, |x| x ~ true)",
        "filter(t, p)",
        "array::map([t], |x| x ~ 1)",
        "array::map([t], p)",
        "array::map([t], |x| x)",
        "array::fold([t], 0, |a, x| x ~ a)",
        "p(t)",
    ];
    let mut counts = vec![];
    for call in calls {
        counts.push(run(&code.replace("CALL", call), mode).await?);
    }
    assert_eq!(counts, [[1]; 7]);
    Ok(())
}

// Nor does it give the call's last value a fire: a function the step reads
// from a variable answers what its body computes in this run.
async fn a_call_born_with_its_step_answers_this_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 24 => n + 1, _ => never() };
        let go = select step { 1 | 9 | 17 => step, _ => never() };
        let k = 1;
        let f = select k {
            1 => |u: i64| { let s = never(); s <- u + 100; s },
            _ => |u: i64| 0
        };
        seq go { f(go) }
    }"#;
    assert_eq!(run(code, mode).await?, [101, 109, 117]);
    Ok(())
}

// So does a step that read the input only through a reference whose
// target it learned as it ran: the first step counts `x`'s 3 live, and the
// last, which names `x`, waits for the 9.
async fn a_fire_read_through_a_reference_is_spent(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 14 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let x = never();
        x <- select step { 3 | 9 => step, _ => never() };
        let p = &x;
        seq go { let a = live(count(*p)); until live(step) >= 6; live(uniq(x)) }
    }"#;
    assert_eq!(run(code, mode).await?, [9]);
    Ok(())
}

// A callee a step bound in an earlier run is the step's from then on: a
// fire of what it captures between two runs is owed to the next entry.
async fn a_callee_bound_in_an_earlier_run_is_caught_up(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 20 => n + 1, _ => never() };
        let go = select step { 1 | 10 => step, _ => never() };
        let ev = never();
        ev <- select step { 2 | 7 => step, _ => never() };
        let k = 1;
        let f = select k { 1 => |u: i64| count(ev), _ => |u: i64| 0 };
        let ready = select step {
            n if (n >= 4 && n < 8) || n >= 12 => true,
            _ => false
        };
        seq go { until live(ready); live(f(0)) }
    }"#;
    assert_eq!(run(code, mode).await?, [1, 1]);
    Ok(())
}

// A step that stood awake when its machine fell asleep saw every write
// until then, walked or not: a callee it binds in the next run is owed
// nothing of the 5 `ev` took meanwhile, and answers the 22.
async fn a_step_asleep_with_its_machine_saw_what_came_before(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let go = select step { 1 | 14 => step, _ => never() };
        let on = true;
        on <- select step { 9 => false, 11 => true, _ => never() };
        let ev = never();
        ev <- select step { 5 | 22 => step, _ => never() };
        let k = 1;
        k <- select step { 12 => 2, _ => never() };
        let f = select k { 1 => |u: i64| -> i64 never(), _ => |u: i64| uniq(ev) };
        select on { true => seq go { live(f(0)) }, false => never() }
    }"#;
    assert_eq!(run(code, mode).await?, [22]);
    Ok(())
}

// So does a step that read the input only through a callee it bound: the
// callee counted `ev`'s fires, so the last step, which names `ev`, waits
// for the next one, whether the first step completed in the cycle of the
// fire or in a later one.
async fn a_fire_read_through_a_bound_callee_is_spent(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 16 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { EV => step, _ => never() };
        let k = 1;
        let f = select k { 1 => |u: i64| count(ev), _ => |u: i64| 0 };
        seq go { STEPS; live(uniq(ev)) }
    }"#;
    let run = |ev: &str, steps: &str| {
        let code = code.replace("EV", ev).replace("STEPS", steps);
        async move { run(&code, mode).await }
    };
    let completes_at_once = "let a = live(f(0)); until live(step) >= 6";
    assert_eq!(run("3 | 9", completes_at_once).await?, [9]);
    assert_eq!(run("3 | 9 | 12", "until live(f(0)) >= 2").await?, [12]);
    Ok(())
}

// A fire from before a step's entry is not one its callee read: the 2 came
// while the first step waited, the second step bound the callee that
// holds `ev` after its own entry, and the last step is still owed it.
async fn a_fire_before_a_steps_entry_is_not_its_callees(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 16 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { 2 | 13 => step, _ => never() };
        let k = 1;
        k <- select step { 6 => 2, _ => never() };
        let f = select k { 1 => |u: i64| -> i64 u, _ => |u: i64| { ev; u } };
        seq go { until live(step) >= 4; until live(f(0) + step) >= 9; live(uniq(ev)) }
    }"#;
    assert_eq!(run(code, mode).await?, [2]);
    Ok(())
}

// An arm of a step's select spends what its bound callee read as a step
// does: the first arm's callee counted the 3, so the last arm waits for
// the 9.
async fn a_fire_an_arms_bound_callee_read_is_spent(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 14 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { 3 | 9 => step, _ => never() };
        let k = 1;
        let f = select k { 1 => |u: i64| count(ev), _ => |u: i64| 0 };
        seq go {
            live(select live(step) < 6 {
                true => filter(f(0), |c| c < 0),
                false => uniq(ev)
            })
        }
    }"#;
    assert_eq!(run(code, mode).await?, [9]);
    Ok(())
}

// A fire in the cycle an arm is deselected is one the arm did not read:
// the 6 lands as the first arm falls asleep, no arm that names `ev` runs
// in that cycle, and the last arm is owed it.
async fn a_fire_as_an_arm_falls_asleep_stays_owed(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 14 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { 3 | 6 | 11 => step, _ => never() };
        let k = 1;
        let f = select k { 1 => |u: i64| count(ev), _ => |u: i64| 0 };
        seq go {
            live(select live(step) {
                n if n < 7 => filter(f(0), |c| c < 0),
                7 => never(),
                _ => uniq(ev)
            })
        }
    }"#;
    assert_eq!(run(code, mode).await?, [6]);
    Ok(())
}

// A passed step is asleep from the cycle it is passed, whatever its
// machine does after: the callee it binds in the next run is owed the 5
// `ev` took after the step completed, though the machine slept and woke
// in between.
async fn a_passed_step_is_asleep_before_its_machine_sleeps(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let go = select step { 1 | 14 => step, _ => never() };
        let on = true;
        on <- select step { 9 => false, 11 => true, _ => never() };
        let ev = never();
        ev <- select step { 5 | 22 => step, _ => never() };
        let k = 1;
        k <- select step { 12 => 2, _ => never() };
        let f = select k { 1 => |u: i64| -> i64 u, _ => |u: i64| uniq(ev) };
        filter(
            select on { true => seq go { live(f(0)) }, false => never() },
            |v| v > 0
        )
    }"#;
    assert_eq!(run(code, mode).await?, [5]);
    Ok(())
}

// A step an abort passes spends what its callee read, and no more. The 5
// its callee counted is not owed to the next run, whose count reaches 2 at
// the 14: its last step waits for the 17. The 6 lands in the cycle the
// abort passes the step, which did not run then: the next run is owed it,
// its count reaches 2 at the 12 and its last step waits for the 14.
async fn a_fire_as_a_step_is_aborted_stays_owed(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let go = select step { 1 | 10 => step, _ => never() };
        let ev = never();
        ev <- select step { EV | 12 | 14 | 17 => step, _ => never() };
        let cancel = select step { 6 => step, _ => never() };
        let k = 1;
        let f = select k { 1 => |u: i64| count(ev), _ => |u: i64| 0 };
        seq go; abort(cancel) { until live(f(0)) >= 2; live(uniq(ev)) }
    }"#;
    assert_eq!(run(&code.replace("EV", "5"), mode).await?, [17]);
    assert_eq!(run(&code.replace("EV", "6"), mode).await?, [14]);
    Ok(())
}

// A step entered in the cycle of a fire reads it live, whoever read it
// before: the callee the compiler resolves lets the last step start in
// the cycle the 9 ends the `until`, and it answers the 9.
async fn a_step_entered_in_the_cycle_of_a_fire_reads_it_live(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 16 => n + 1, _ => never() };
        let go = select step { 1 => step, _ => never() };
        let ev = never();
        ev <- select step { 3 | 9 | 12 => step, _ => never() };
        let f = |u: i64| count(ev);
        seq go { until live(f(0)) >= 2; live(uniq(ev)) }
    }"#;
    assert_eq!(run(code, mode).await?, [9]);
    Ok(())
}

modes!(
    a_fire_while_a_step_waits_reaches_live,
    a_trigger_fired_while_its_step_waited_starts_a_nested_seq,
    a_target_written_while_a_step_waited_is_caught_up,
    a_callee_bound_at_a_step_entry_is_caught_up,
    a_fire_no_step_read_is_owed_to_the_next_run,
    a_fire_an_awake_step_read_is_not_owed_again,
    a_nested_seq_waits_for_its_trigger_in_every_run,
    a_callback_born_with_its_step_fires_nothing,
    a_call_born_with_its_step_answers_this_run,
    a_fire_read_through_a_reference_is_spent,
    a_callee_bound_in_an_earlier_run_is_caught_up,
    a_step_asleep_with_its_machine_saw_what_came_before,
    a_fire_read_through_a_bound_callee_is_spent,
    a_fire_before_a_steps_entry_is_not_its_callees,
    a_fire_an_arms_bound_callee_read_is_spent,
    a_fire_as_an_arm_falls_asleep_stays_owed,
    a_passed_step_is_asleep_before_its_machine_sleeps,
    a_fire_as_a_step_is_aborted_stays_owed,
    a_step_entered_in_the_cycle_of_a_fire_reads_it_live,
);
