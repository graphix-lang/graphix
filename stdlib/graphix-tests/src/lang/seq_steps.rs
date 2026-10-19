// What a step sees of the steps before it (`design/dependency_summaries.md`):
// every write an earlier step makes is seen by a later step that reads it,
// through a call or a reference as well as a `<-`, and a passed step is
// asleep.

use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::testing::{Mode, as_i64s};

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
// CR claude for claude: [test-gap/low] Two seq rules have no pin. One is CLAUDE.md's "a
// call with no static target counts as every variable": only the reference half and the
// static-callee half are tested here. The other is design/seq_blocks.md §5's "a run
// whose last write targets its own trigger ... starts the next run";
// seq_let::trigger_writes_reach_the_variable deliberately avoids it. Add `let f =
// select k { 1 => |v| { b <- v; v }, _ => |v| v }; seq { f(5); let s = b; s }` with its
// seqq twin, both answering 5, and `let t = 0; seq t { t <- t + 1 }`, which counts t up
// to 5. probe: ran both with the review binary; both behave as the rules say today.
// (tests-seq-03)
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

// A fire of an input while its step waits its turn is re-raised when the
// step is entered.
async fn entry_reraises_an_earlier_fire(mode: Mode) -> Result<()> {
    let code = r#"{
        let click = never();
        click <- sys::time::after_idle(duration:20.ms, 1);
        let go = 1;
        seq go { sys::time::after_idle(duration:60.ms, 0); let r = click ~ 7; r }
    }"#;
    assert_eq!(run(code, mode).await?, [7]);
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

// CR claude for claude: [test-gap/medium] Nothing in lang/seq*.rs pins that a restart
// builtin in a step is reborn at each run (CLAUDE.md: entering a step is its birth for
// the run). Emptying Count::reborn, Counted::reborn, Uniq::reborn, Hold::reborn or
// Throttle::reborn would still pass. Add a fixture firing `request` at steps 1 and 20
// over the body `let c = count(request); let t = take(#n: 1, request); let u =
// uniq(request ~ k); let h = hold(#clock: request, request); (c, t, u, h)` with `let k
// = 5`. It should answer (1, 1, 5, 1) then (1, 2, 5, 2), which both engines do today.
// probe: ran that fixture with the review binary. (tests-seq-02)
modes!(
    closure_write_is_seen,
    ref_write_through_a_variable_is_seen,
    seqq_callee_write_is_live,
    seqq_ref_argument_keeps_captures,
    passed_let_does_not_track,
    entry_reraises_an_earlier_fire,
    until_true_at_entry,
    let_fire_reaches_a_later_cycle,
    let_fire_read_live_is_not_raised_again,
    until_sees_the_write_before_it,
    try_sees_the_write_before_it,
);
