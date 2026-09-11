// seq (design/seq_blocks.md): pc-machine atoms, then the surface
// construct. Straight-line only.

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::{run, testing::eval};
use netidx::publisher::Value;

// A nested presence-watch samples a free read of `pc`: the catch-up
// delivers the entry when the delayed value arrives.
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

// A pattern bind of the outer scrutinee is a facet of that match and
// is not re-raised into the nested watch.
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

// The presence select issues on both runs, including an entry that
// finds `x` bottom after it had been a value.
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

// A bare `pc ~ x` at an entry that finds `x` bottom consumes the
// sample's debt and issues nothing.
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

// Busy-drop: the first trigger runs; two arriving mid-run are dropped.
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

// Same-arm re-entry sampled on the trigger: three entries, three issues.
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

// until: enter while the level is false, then it flips.
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

const SEQ_BLOCK_RERUNS: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 10 => s + 1, _ => never() };
  let trig = select step { 1 | 5 => step, _ => never() };
  let n = 0;
  seq trig {
    {
      n <- n + 1;
      n
    }
  };
  select step { 10 => n, _ => never() }
}
"#;

run!(seq_block_reruns, SEQ_BLOCK_RERUNS, |v: Result<&Value>| match v {
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
            {
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

const SEQ_BLOCK_LET_INSIDE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let delayed = never<i64>();
  delayed <- select step { 4 => 42, _ => never() };
  let y = 0;
  seq go {
    {
      let x = delayed;
      y <- x
    }
  };
  select step { 8 => y, _ => never() }
}
"#;

run!(seq_block_let_inside, SEQ_BLOCK_LET_INSIDE, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_BLOCK_TWO_WRITES: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let a = 0;
  let b = 0;
  seq go {
    {
      let x = 3;
      a <- x;
      b <- x + 1
    }
  };
  select step { 6 => a * 10 + b, _ => never() }
}
"#;

run!(seq_block_two_writes, SEQ_BLOCK_TWO_WRITES, |v: Result<&Value>| match v {
    Ok(Value::I64(34)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_BLOCK_VALUE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let y = seq {
    {
      let x = 3;
      x + 1
    }
  };
  select step { 6 => y, _ => never() }
}
"#;

run!(seq_block_value, SEQ_BLOCK_VALUE, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_BLOCK_VALUE_AFTER_WAIT: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let delayed = never<i64>();
  delayed <- select step { 4 => 42, _ => never() };
  let y = seq go {
    {
      let r = delayed;
      r + 1
    }
  };
  select step { 8 => y, _ => never() }
}
"#;

run!(seq_block_value_after_wait, SEQ_BLOCK_VALUE_AFTER_WAIT, |v: Result<&Value>| match v {
    Ok(Value::I64(43)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const SEQ_BLOCK_VALUE_NOT_LAST: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let y = seq {
    {
      let x = 3;
      x + 1
    };
    99
  };
  select step { 6 => y, _ => never() }
}
"#;

run!(seq_block_value_not_last, SEQ_BLOCK_VALUE_NOT_LAST, |v: Result<&Value>| match v {
    Ok(Value::I64(99)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Two writes with no read between them share an arm and land in one
// cycle; the statement that reads them is the next arm, the next cycle.
const SEQ_ARM_WRITES_LAND_TOGETHER: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let a = 0;
  let b = 0;
  let s = 0;
  seq go {
    a <- 10;
    b <- 20;
    s <- a + b
  };
  let first = once(select a { 0 => never(), _ => (a, b) });
  select step { 8 => (first, s), _ => never() }
}
"#;

run!(seq_arm_writes_land_together, SEQ_ARM_WRITES_LAND_TOGETHER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a.len() == 2
        && matches!(&a[0], Value::Array(p) if p.len() == 2
            && p[0] == Value::I64(10) && p[1] == Value::I64(20))
        && a[1] == Value::I64(30))
}; graphix_package_core::testing::FuseExpect::Jit);

// A statement that reads what an earlier one wrote runs a cycle later
// and sees the write.
const SEQ_ARM_READ_AFTER_WRITE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let n = 0;
  let m = 0;
  seq go {
    n <- n + 1;
    n <- n + 1;
    m <- n
  };
  select step { 8 => n * 10 + m, _ => never() }
}
"#;

run!(seq_arm_read_after_write, SEQ_ARM_READ_AFTER_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(22)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A second write to one variable is the next arm, else the reader
// would see the first write while the second is still queued.
const SEQ_ARM_WRITE_AFTER_WRITE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  let go = select step { 1 => true, _ => never() };
  let a = 0;
  let r = 0;
  seq go {
    a <- 1;
    a <- 2;
    r <- a
  };
  select step { 8 => a * 10 + r, _ => never() }
}
"#;

run!(seq_arm_write_after_write, SEQ_ARM_WRITE_AFTER_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(22)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A block's lets are its own; the statement after it reads the outer x.
const SEQ_BLOCK_LET_LOCAL: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let x = 100;
  let y = seq {
    { let x = 5; x };
    x
  };
  select step { 6 => y, _ => never() }
}
"#;

run!(seq_block_let_local, SEQ_BLOCK_LET_LOCAL, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(100)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A block issues its statements at entry, so two calls in one block are
// in flight together; at the seq level a call is opaque, so the second
// waits for the write before it.
async fn block_issues_together(fusion_disabled: bool) -> Result<()> {
    for (body, expected) in
        [("{ a <- f(1); b <- f(2) }", "done"), ("a <- f(1); b <- f(2)", "late")]
    {
        let code = format!(
            r#"{{
                let f = |v| sys::time::after_idle(duration:30.ms, v);
                let a = never<i64>();
                let b = never<i64>();
                seq {{ {body} }};
                let late = sys::time::timer(duration:50.ms, false);
                let first = once(any(late ~ "late", (a + b) ~ "done"));
                sys::time::after_idle(duration:100.ms, first)
            }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(values.len(), 1, "{body}: {values:?}");
        assert!(
            matches!(&values[0], Value::String(s) if &**s == expected),
            "{body}: {values:?}"
        );
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn block_issues_together_interp() -> Result<()> {
    block_issues_together(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn block_issues_together_jit() -> Result<()> {
    block_issues_together(false).await
}

async fn block_trailing_semicolon(fusion_disabled: bool) -> Result<()> {
    use arcstr::format;

    for form in ["seq", "seqq"] {
        let requests =
            if form == "seq" { "1 => 1, 15 => 2, 30 => 3" } else { "1 | 2 | 3 => step" };
        for semi in ["", ";"] {
            for (body, tail) in [
                ("null; n <- n + 1", ""),
                ("let x = request; x", ""),
                ("let x = request; n <- x", "; n"),
            ] {
                let code = format!(
                    r#"{{
                        let step = 0;
                        step <- select step {{ n if n < 40 => n + 1, _ => never() }};
                        let request = select step {{ {requests}, _ => never() }};
                        let n = 0;
                        {form} request {{ {{ {body}{semi} }}{tail} }}
                    }}"#
                );
                let (values, _) = run_delta(&code, fusion_disabled).await?;
                assert_eq!(as_i64s(&values), [1, 2, 3], "{code}");
            }
        }
        for body in ["request; never();", "{ let x = request; never(); };"] {
            let code = format!("{form} {{ {{ let request = 1; {body} }}; 42 }}");
            let (values, _) = run_delta(&code, fusion_disabled).await?;
            assert!(values.is_empty(), "{code}: {values:?}");
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn block_trailing_semicolon_interp() -> Result<()> {
    block_trailing_semicolon(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn block_trailing_semicolon_jit() -> Result<()> {
    block_trailing_semicolon(false).await
}

// A step completes on a fired production after its entry, never on a
// standing value. Each fixture runs three requests; a machine that
// accepts the previous run's resident answers one behind.
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
            "old_cell_in_block",
            "let a = request; let b = request + 10; { let c = b; a }",
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
    // A lambda whose result is a standing level not derived from its
    // argument produces no fire when re-called: the run never completes.
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

// The machine's generated calls name `core::` explicitly, so a user
// binding called `filter` does not capture them.
const SEQ_SHADOWED_CORE_NAMES: &str = r#"
{
  let filter = 42;
  let once = 1;
  let out = 0;
  seq { out <- once + filter };
  select out { 0 => never(), n => n }
}
"#;

run!(seq_shadowed_core_names, SEQ_SHADOWED_CORE_NAMES, |v: Result<&Value>| match v {
    Ok(Value::I64(43)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// `until` has no value: using the statement's value is refused.
#[tokio::test(flavor = "current_thread")]
async fn until_last_refused() -> Result<()> {
    for src in [
        "{ let go = true; seq go { let x = 1; until (x > 0) } }",
        "{ let go = true; seq go { let y = try { until go } with(e) { 1 }; y } }",
        "{ let go = true; seq go { let y = try { 1 } with(e) { until go }; y } }",
    ] {
        let msg = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains("until has no value"), "wrong refusal for {src}: {msg}");
    }
    // A bare try in the middle of a seq uses no value: until may end it.
    let (v, _) = eval(
        "{ let go = true; seq go { try { until go } with(e) { 1 }; 5 } }",
        crate::TEST_REGISTER,
    )
    .await?;
    assert_eq!(v, Value::I64(5));
    Ok(())
}
