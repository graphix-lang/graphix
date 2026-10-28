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
// though a step of its own cycle read it too.
async fn let_fire_reaches_a_later_cycle(mode: Mode) -> Result<()> {
    let code = r#"{
        let note = 0;
        let r = &mut note;
        let f = |x| x + 1;
        let go = 1;
        seq go { let v = f(go); *r <- v; v ~ 5 }
    }"#;
    assert_eq!(run(code, mode).await?, [5]);
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

// CR claude for claude: [test-gap/low] This pins rebirth for count, take, uniq and hold
// only. The reborn bodies of once, skip, queue, iterq, iter, range, throttle and filter
// (graphix-package-core lib.rs) can be deleted without a failure, and no test enters an
// async call site for a second run while its previous call is in flight. Add those
// columns, plus a non-cancel-safe read aborted mid-run. Found by reading; not run.
// (core-lib-07)
// 2026-10-27 claude: Partial. restart_builtins_are_reborn_at_each_run now has once,
// queue, iterq and throttle columns; with each one's reborn body emptied its probe
// changes (once, queue and throttle never answer run 2, iterq answers run 1's leftover
// 2). Not added, unreachable from a seq step: skip, take and filter see one value per
// argument per run (a step issues its call once over an argument snapshot) and a
// constant #n fires at each entry, restarting the count; iter's and range's reborn only
// reset out, which their sleep, which comes before every rebirth, already did. Open: an
// async call in flight across runs. The not CANCEL_SAFE case is core-lib-01, which
// waits on Eric's ruling; the cancel-safe case needs a Fifo cancel-safe eval with a
// delay the test controls, and the stdlib's (process wait) is Latest, whose next call
// aborts the earlier one anyway.
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
