// What a step sees of the steps before it (`design/dependency_summaries.md`):
// every write an earlier step makes is seen by a later step that reads it,
// through a call or a reference as well as a `<-`, and a passed step is
// asleep.

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::testing::Mode;

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

modes!(
    closure_write_is_seen,
    ref_write_through_a_variable_is_seen,
    seqq_callee_write_is_live,
    passed_let_does_not_track,
    entry_reraises_an_earlier_fire,
    until_true_at_entry,
    let_fire_reaches_a_later_cycle,
);
