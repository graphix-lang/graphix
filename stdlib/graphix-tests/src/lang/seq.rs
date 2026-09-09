// seq (`design/seq_blocks.md`): pc-machine atoms, then the surface
// construct. Straight-line only — no if/loop.

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

// §7.2: a nested presence-watch samples a FREE read of `pc`. `pc`
// fires at entry while the inner scrutinee is bottom; the inner select
// has no arm taken, so its tracker holds the bit; when the delayed
// value arrives the catch-up injects `pc` FIRED and the sample pays.
const SEQ_PC_FREE_READ_WAKES_NESTED: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  type Pc = [`Idle, `Wait];
  let pc: Pc = `Idle;
  pc <- select step { 1 => `Wait, _ => never() };
  let delayed = never<i64>();
  delayed <- select step { 4 => 42, _ => never() };
  let issued = 0;
  select pc {
    `Idle => never(),
    `Wait => select delayed {
      v => issued <- pc ~ v
    }
  };
  select step { 8 => issued, _ => never() }
}
"#;

run!(seq_pc_free_read_wakes_nested, SEQ_PC_FREE_READ_WAKES_NESTED, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Contrast: a pattern bind of the OUTER scrutinee is a facet of that
// match and is not re-raised into the nested watch.
const SEQ_PC_PATTERN_BIND_NOT_RERAISED: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  type Pc = [`Idle, `Wait];
  let pc: Pc = `Idle;
  pc <- select step { 1 => `Wait, _ => never() };
  let delayed = never<i64>();
  delayed <- select step { 4 => 42, _ => never() };
  let issued = 0;
  select pc {
    `Idle => never(),
    p@ `Wait => select delayed {
      v => issued <- p ~ v
    }
  };
  select step { 8 => issued, _ => never() }
}
"#;

run!(seq_pc_pattern_bind_not_reraised, SEQ_PC_PATTERN_BIND_NOT_RERAISED, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// §7.3 presence select: two runs. First run waits for `x`; second run
// `x` is bottom-after-having-been-a-value at entry, then returns. The
// presence watch issues both times. A bare `pc ~ x` would consume the
// second entry's debt against a materialized bottom and stall.
const SEQ_PRESENCE_SECOND_RUN: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 16 => s + 1, _ => never() };
  type Pc = [`Idle, `Wait];
  let pc: Pc = `Idle;
  pc <- select step { 1 => `Wait, 7 => `Idle, 8 => `Wait, _ => never() };
  let x = select step {
    s if s < 4 => never(),
    s if s < 6 => 1,
    s if s < 12 => never(),
    _ => 2
  };
  let n = 0;
  select pc {
    `Idle => never(),
    `Wait => select x {
      v => {
        n <- pc ~ (n + 1);
        pc <- pc ~ `Idle
      }
    }
  };
  select step { 16 => n, _ => never() }
}
"#;

run!(seq_presence_second_run, SEQ_PRESENCE_SECOND_RUN, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// The stall the presence select avoids: a bare `pc ~ x` at an entry
// that finds `x` bottom consumes the sample's debt (even the first
// wait — `never()` in the producer is a materialized bottom). Issues
// nothing; the presence pin above is the one that counts both runs.
const SEQ_BARE_SAMPLE_STALLS_SECOND_RUN: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 16 => s + 1, _ => never() };
  type Pc = [`Idle, `Wait];
  let pc: Pc = `Idle;
  pc <- select step { 1 => `Wait, 7 => `Idle, 8 => `Wait, _ => never() };
  let x = select step {
    s if s < 4 => never(),
    s if s < 6 => 1,
    s if s < 12 => never(),
    _ => 2
  };
  let n = 0;
  select pc {
    `Idle => never(),
    `Wait => {
      n <- (pc ~ x) ~ (n + 1);
      pc <- pc ~ `Idle
    }
  };
  select step { 16 => n, _ => never() }
}
"#;

run!(seq_bare_sample_stalls_second_run, SEQ_BARE_SAMPLE_STALLS_SECOND_RUN, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// R1 busy-drop: `filter(trig, |_| idle)` lets the first trigger through
// and drops the two that arrive while the run is in `A`.
const SEQ_BUSY_DROPS_RETRIGGER: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  type Pc = [`Idle, `A];
  let pc: Pc = `Idle;
  let idle = select pc { `Idle => true, `A => false };
  let trig = select step { 1 | 2 | 3 => step, _ => never() };
  let t = filter(trig, |x| x ~ idle);
  let starts = 0;
  starts <- t ~ (starts + 1);
  pc <- t ~ `A;
  select pc {
    `Idle => never(),
    `A => pc <- select step { 6 => `Idle, _ => never() }
  };
  select step { 8 => starts, _ => never() }
}
"#;

run!(seq_busy_drops_retrigger, SEQ_BUSY_DROPS_RETRIGGER, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Same-arm re-entry: writing `A` again is sampled on the trigger, not a
// constant RHS. Three entries, three issues.
const SEQ_SAME_ARM_REENTRY: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  type Pc = [`Idle, `A];
  let pc: Pc = `Idle;
  pc <- select step { 1 | 3 | 5 => step ~ `A, _ => never() };
  let n = 0;
  select pc {
    `Idle => never(),
    `A => n <- pc ~ (n + 1)
  };
  select step { 8 => n, _ => never() }
}
"#;

run!(seq_same_arm_reentry, SEQ_SAME_ARM_REENTRY, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// until: enter while the level is false, then it flips. The nested
// watch's transition samples `pc`; catch-up has to deliver the entry.
const SEQ_UNTIL_LEVEL_FLIPS: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  type Pc = [`Idle, `Until, `Done];
  let pc: Pc = `Idle;
  pc <- select step { 1 => `Until, _ => never() };
  let released = false;
  released <- select step { 4 => true, _ => never() };
  select pc {
    `Idle => never(),
    `Until => select released {
      true => pc <- pc ~ `Done,
      false => never()
    },
    `Done => never()
  };
  select step { 8 => pc, _ => never() }
}
"#;

run!(seq_until_level_flips, SEQ_UNTIL_LEVEL_FLIPS, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "Done",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_VALUE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 4 => s + 1, _ => never() };
  let y = seq { 7 };
  select step { 4 => y, _ => never() }
}
"#;

run!(seq_value, SEQ_VALUE, |v: Result<&Value>| match v {
    Ok(Value::I64(7)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_LET_THEN_USE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let y = seq {
    let x = 3;
    x + 1
  };
  select step { 6 => y, _ => never() }
}
"#;

run!(seq_let_then_use, SEQ_LET_THEN_USE, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_TRIGGER_AND_UNTIL: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 10 => s + 1, _ => never() };
  let go = select step { 2 => true, _ => never() };
  let ready = false;
  ready <- select step { 5 => true, _ => never() };
  let y = seq go {
    until ready;
    9
  };
  select step { 10 => y, _ => never() }
}
"#;

run!(seq_trigger_and_until, SEQ_TRIGGER_AND_UNTIL, |v: Result<&Value>| match v {
    Ok(Value::I64(9)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_BUSY_DROPS: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 12 => s + 1, _ => never() };
  let trig = select step { 1 | 2 | 3 => step, _ => never() };
  let go = false;
  go <- select step { 8 => true, _ => never() };
  let n = 0;
  n <- seq trig {
    until go;
    n + 1
  };
  select step { 12 => n, _ => never() }
}
"#;

run!(seq_busy_drops, SEQ_BUSY_DROPS, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_QOP_ABORTS: &str = r#"
{
  let caught = never();
  catch(e) caught <- e ~ 1;
  seq { [0][1]?; 99 };
  caught
}
"#;

run!(seq_qop_aborts, SEQ_QOP_ABORTS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_DO_FANOUT_RERUNS: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 10 => s + 1, _ => never() };
  let trig = select step { 1 | 5 => step, _ => never() };
  let n = 0;
  seq trig {
    do {
      n <- n + 1
    }
  };
  select step { 10 => n, _ => never() }
}
"#;

run!(seq_do_fanout_reruns, SEQ_DO_FANOUT_RERUNS, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

#[tokio::test]
async fn seq_nested_connect_completion() -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 20 => s + 1, _ => never() };
        let go = select step { 1 | 10 => step, _ => never() };
        let refresh = never<i64>();
        seq go {
            do {
                let r = go;
                select r { r => { refresh <- r; null } }
            };
            go
        }
    }"#;
    for fusion_disabled in [true, false] {
        let (values, _) = run_delta(code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 10]);
    }
    Ok(())
}

const SEQ_DO_LET_INSIDE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let delayed = never<i64>();
  delayed <- select step { 4 => 42, _ => never() };
  let y = 0;
  seq go {
    do {
      let x = delayed;
      y <- x
    }
  };
  select step { 8 => y, _ => never() }
}
"#;

run!(seq_do_let_inside, SEQ_DO_LET_INSIDE, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_DO_TWO_WRITES: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let a = 0;
  let b = 0;
  seq go {
    do {
      let x = 3;
      a <- x;
      b <- x + 1
    }
  };
  select step { 6 => a * 10 + b, _ => never() }
}
"#;

run!(seq_do_two_writes, SEQ_DO_TWO_WRITES, |v: Result<&Value>| match v {
    Ok(Value::I64(34)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_DO_VALUE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let y = seq {
    do {
      let x = 3;
      x + 1
    }
  };
  select step { 6 => y, _ => never() }
}
"#;

run!(seq_do_value, SEQ_DO_VALUE, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_DO_VALUE_AFTER_WAIT: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let delayed = never<i64>();
  delayed <- select step { 4 => 42, _ => never() };
  let y = seq go {
    do {
      let r = delayed;
      r + 1
    }
  };
  select step { 8 => y, _ => never() }
}
"#;

run!(seq_do_value_after_wait, SEQ_DO_VALUE_AFTER_WAIT, |v: Result<&Value>| match v {
    Ok(Value::I64(43)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_DO_VALUE_NOT_LAST: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let y = seq {
    do {
      let x = 3;
      x + 1
    };
    99
  };
  select step { 6 => y, _ => never() }
}
"#;

run!(seq_do_value_not_last, SEQ_DO_VALUE_NOT_LAST, |v: Result<&Value>| match v {
    Ok(Value::I64(99)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

async fn do_trailing_semicolon(fusion_disabled: bool) -> Result<()> {
    use arcstr::format;

    for form in ["seq", "seqq"] {
        let requests =
            if form == "seq" { "1 => 1, 15 => 2, 30 => 3" } else { "1 | 2 | 3 => step" };
        for semi in ["", ";"] {
            for (body, tail) in [
                ("n <- n + 1", ""),
                ("let x = request", ""),
                ("let x = request; x", ""),
                ("let x = request; n <- x", "; n"),
            ] {
                let code = format!(
                    r#"{{
                        let step = 0;
                        step <- select step {{ n if n < 40 => n + 1, _ => never() }};
                        let request = select step {{ {requests}, _ => never() }};
                        let n = 0;
                        {form} request {{ do {{ {body}{semi} }}{tail} }}
                    }}"#
                );
                let (values, _) = run_delta(&code, fusion_disabled).await?;
                assert_eq!(as_i64s(&values), [1, 2, 3], "{code}");
            }
        }
        for body in ["request; never();", "{ let x = request; x; };"] {
            let code = format!("{form} {{ do {{ let request = 1; {body} }}; 42 }}");
            let (values, _) = run_delta(&code, fusion_disabled).await?;
            assert!(values.is_empty(), "{code}: {values:?}");
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn do_trailing_semicolon_interp() -> Result<()> {
    do_trailing_semicolon(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn do_trailing_semicolon_jit() -> Result<()> {
    do_trailing_semicolon(false).await
}

// R10 (design/seq_review_2026-09-06.md): a step completes on a FIRED
// production after its entry, never on a standing value. A call is
// re-issued at entry and its own fire is the answer; a level is fired
// at entry as it stands (R2) and tracked if absent. Each fixture runs
// three requests; a machine that accepts the previous run's resident
// answers one behind.
async fn reentry_fired_only(fusion_disabled: bool) -> Result<()> {
    use arcstr::format;

    const CLOCK: &str = r#"
        let step = 0;
        step <- select step { n if n < 60 => n + 1, _ => never() };
        let request = select step { 1 | 2 | 3 => step, _ => never() };
    "#;
    const BAD: &str =
        "let bad = |v| -> [i64, Error<`Oops>] select v { 1 => error(`Oops), _ => v };";
    let cases: [(&str, &str, Vec<i64>); 8] = [
        // the `~` resident is re-presented at wake; the timer's fire is the answer
        (
            "held_sample",
            "let x = try { bad(request)? } with(e) { 0 }; sys::time::timer(duration:2.ms, false) ~ x",
            vec![0, 2, 3],
        ),
        // the instance's `r` is standing at re-dispatch; its write is the answer
        (
            "lambda_state",
            "let f = |v| { let r = never(); catch(e) r <- e ~ -2; r <- bad(v)?; r }; f(request)",
            vec![-2, 2, 3],
        ),
        // an async writer inside the lambda: bottom at dispatch, fired a cycle later
        (
            "lambda_writer",
            "let g = |v| { let r = never(); r <- v * 2; r }; g(request)",
            vec![2, 4, 6],
        ),
        // a level absent at the first entry is waited for, then read as it stands
        (
            "absent_level",
            "let lvl = sys::time::timer(duration:20.ms, false) ~ 42; let q = request; let y = lvl; q * 100 + y",
            vec![142, 242, 342],
        ),
        // a carried cell bound two steps earlier is standing at the reading step
        ("old_cell", "let a = request; let b = request + 10; a", vec![1, 2, 3]),
        (
            "old_cell_in_do",
            "let a = request; let b = request + 10; do { let c = b; a }",
            vec![1, 2, 3],
        ),
        // `until` on a level: false then flipped for run 1, present-true after
        (
            "until_present",
            "let flag = false; flag <- sys::time::timer(duration:20.ms, false) ~ true; until flag; request",
            vec![1, 2, 3],
        ),
        // the effect helper: its `null` is born at each dispatch
        (
            "effect_null",
            "let w = 0; let e = |v| { w <- v; null }; e(request); request",
            vec![1, 2, 3],
        ),
    ];
    for (name, body, expected) in cases {
        let code = format!("{{ {CLOCK} {BAD} seqq request {{ {body} }} }}");
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), expected, "{name}\n{out}");
    }
    // A lambda whose result is a standing level it does not derive from
    // its argument produces no fire when re-called, in a seq as anywhere
    // else: the run never completes. Sampling the level on the argument
    // is the spelling.
    let stalls =
        format!("{{ {CLOCK} let k = 7; let f = |v| k; seqq request {{ f(request) }} }}");
    let (values, _) = run_delta(&stalls, fusion_disabled).await?;
    assert!(values.len() <= 1, "{values:?}");
    let sampled = format!(
        "{{ {CLOCK} let k = 7; let f = |v| v ~ k; seqq request {{ f(request) }} }}"
    );
    let (values, _) = run_delta(&sampled, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [7, 7, 7]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn reentry_fired_only_interp() -> Result<()> {
    reentry_fired_only(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn reentry_fired_only_jit() -> Result<()> {
    reentry_fired_only(false).await
}
