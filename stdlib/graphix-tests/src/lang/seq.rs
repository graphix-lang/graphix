// seq (design/seq_blocks.md): pc-machine atoms, then the surface
// construct. Straight-line only.

use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, as_i64s, eval_modes, is, prints, refused},
};
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

run!(seq_pc_free_read_wakes_nested, SEQ_PC_FREE_READ_WAKES_NESTED, is(Value::I64(42)); FuseExpect::Jit);

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

run!(seq_pc_pattern_bind_not_reraised, SEQ_PC_PATTERN_BIND_NOT_RERAISED, is(Value::I64(0)); FuseExpect::Jit);

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

run!(seq_presence_second_run, SEQ_PRESENCE_SECOND_RUN, is(Value::I64(2)); FuseExpect::Jit);

// A bare `pc ~ x` at an entry that finds `x` bottom banks the fire. An `x`
// that arrives and goes bottom again while the arm sleeps does not pay the
// bank, at the wake or after it: neither entry issues anything.
const SEQ_BARE_SAMPLE_BANK_OUTLIVES_A_PASSING_VALUE: &str = r#"
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

run!(
    seq_bare_sample_bank_outlives_a_passing_value,
    SEQ_BARE_SAMPLE_BANK_OUTLIVES_A_PASSING_VALUE,
    is(Value::I64(0));
    FuseExpect::Jit
);

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

run!(seq_busy_drops_retrigger, SEQ_BUSY_DROPS_RETRIGGER, is(Value::I64(1)); FuseExpect::Jit);

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

run!(seq_same_arm_reentry, SEQ_SAME_ARM_REENTRY, is(Value::I64(3)); FuseExpect::Jit);

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

run!(seq_until_level_flips, SEQ_UNTIL_LEVEL_FLIPS, prints(r#""Done""#); FuseExpect::Jit);

const SEQ_VALUE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 4 => s + 1, _ => never() };
  let y = seq { 7 };
  select step { 4 => y, _ => never() }
}
"#;

run!(seq_value, SEQ_VALUE, is(Value::I64(7)); FuseExpect::Jit);

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

run!(seq_let_then_use, SEQ_LET_THEN_USE, is(Value::I64(4)); FuseExpect::Jit);

const SEQ_TRIGGER_AND_UNTIL: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 10 => s + 1, _ => never() };
  let go = select step { 2 => true, _ => never() };
  let ready = false;
  ready <- select step { 5 => true, _ => never() };
  let y = seq go {
    until live(ready);
    9
  };
  select step { 10 => y, _ => never() }
}
"#;

run!(seq_trigger_and_until, SEQ_TRIGGER_AND_UNTIL, is(Value::I64(9)); FuseExpect::Jit);

const SEQ_BUSY_DROPS: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 12 => s + 1, _ => never() };
  let trig = select step { 1 | 2 | 3 => step, _ => never() };
  let go = false;
  go <- select step { 8 => true, _ => never() };
  let n = 0;
  n <- seq trig {
    until live(go);
    n + 1
  };
  select step { 12 => n, _ => never() }
}
"#;

run!(seq_busy_drops, SEQ_BUSY_DROPS, is(Value::I64(1)); FuseExpect::Jit);

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

run!(seq_block_reruns, SEQ_BLOCK_RERUNS, is(Value::I64(2)); FuseExpect::Jit);

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
    for mode in Mode::ALL {
        let (values, _) = run_delta(code, mode).await?;
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

run!(seq_block_let_inside, SEQ_BLOCK_LET_INSIDE, is(Value::I64(42)); FuseExpect::Jit);

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

run!(seq_block_two_writes, SEQ_BLOCK_TWO_WRITES, is(Value::I64(34)); FuseExpect::Jit);

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

run!(seq_block_value, SEQ_BLOCK_VALUE, is(Value::I64(4)); FuseExpect::Jit);

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

run!(seq_block_value_after_wait, SEQ_BLOCK_VALUE_AFTER_WAIT, is(Value::I64(43)); FuseExpect::Jit);

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

run!(seq_block_value_not_last, SEQ_BLOCK_VALUE_NOT_LAST, is(Value::I64(99)); FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A nested seq without a trigger runs at every entry of its statement;
// the step waits for that run's result, never the previous run's.
const SEQ_NESTED_REENTRY: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 30 => s + 1, _ => never() };
  let go = select step { 1 | 12 => step, _ => never() };
  let out = seq go {
    let r = seq { let a = go; a * 10 };
    r + 1
  };
  select step { 30 => out, _ => never() }
}
"#;

run!(seq_nested_reentry, SEQ_NESTED_REENTRY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(121)))
}; FuseExpect::Jit);

// The same with the nested seq inside a select arm, an operand and a
// call argument of its step: each waits for its run's result.
const SEQ_NESTED_REENTRY_IN_EXPR: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 30 => s + 1, _ => never() };
  let go = select step { 1 | 12 => step, _ => never() };
  let id = |x: i64| x;
  let out = seq go {
    let a = select go { 0 => 0, _ => seq { let a = go; a * 10 } };
    let b = seq { let b = go; b } + 0;
    let c = id(seq { let c = go; c });
    a + b + c
  };
  select step { 30 => out, _ => never() }
}
"#;

run!(seq_nested_reentry_in_expr, SEQ_NESTED_REENTRY_IN_EXPR, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(144)))
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A block issues its statements at entry, so both calls in one block have
// started before either answers; as separate steps the second starts only
// once the first has answered. Each call answers with the last start it
// sees once the runtime is idle, so `a` is 2 together and 1 apart.
async fn block_issues_together(mode: Mode) -> Result<()> {
    for (body, first) in [("{ a <- f(1); b <- f(2) }", 2), ("a <- f(1); b <- f(2)", 1)] {
        let code = format!(
            r#"{{
                let started = 0;
                let f = |v| {{
                    started <- v;
                    sys::time::after_idle(duration:20.ms, v) ~ started
                }};
                let a = never<i64>();
                let b = never<i64>();
                seq {{ {body} }};
                b ~ a
            }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(values, [Value::I64(first)], "{body}");
    }
    Ok(())
}

modes!(block_issues_together);

// A block's connect writes its call's answer on every run, and a level
// as it stands at entry, once.
async fn block_connect_every_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let t = sys::time::timer(duration:300.ms, 3) ~ 1;
        let n = 0;
        n <- t ~ n + 1;
        let a = 0;
        let l = 0;
        let lw = 0;
        lw <- l ~ lw + 1;
        let r = seq t {
            { a <- sys::time::after_idle(duration:30.ms, n); l <- n; 0 };
            n
        };
        let out = sys::time::after_idle(duration:50.ms, r) ~ (r, a, l, lw);
        array::group(out, |c, _| c == 3)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(values.len(), 1, "{values:?}");
    assert_eq!(
        values[0].to_string(),
        "[[i64:1, i64:1, i64:1, i64:2], [i64:2, i64:2, i64:2, i64:3], [i64:3, i64:3, i64:3, i64:4]]"
    );
    Ok(())
}

modes!(block_connect_every_run);

async fn block_trailing_semicolon(mode: Mode) -> Result<()> {
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
                let (values, _) = run_delta(&code, mode).await?;
                assert_eq!(as_i64s(&values), [1, 2, 3], "{code}");
            }
        }
        for body in ["request; never();", "{ let x = request; never(); };"] {
            let code = format!("{form} {{ {{ let request = 1; {body} }}; 42 }}");
            let (values, _) = run_delta(&code, mode).await?;
            assert!(values.is_empty(), "{code}: {values:?}");
        }
    }
    Ok(())
}

modes!(block_trailing_semicolon);

// A step completes on a fired production after its entry, never on a
// standing value. Each fixture runs three requests; a machine that
// accepts the previous run's resident answers one behind.
async fn reentry_fired_only(mode: Mode) -> Result<()> {
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
        // a step's own level, a timer born at each run's entry, is waited for
        // in every run
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
        // `until` on a level the step before it writes: entered a cycle after
        // the write, it reads `flag` true in every run
        (
            "until_present",
            "let flag = false; flag <- sys::time::timer(duration:20.ms, false) ~ true; until live(flag); request",
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
        let (values, out) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), expected, "{name}\n{out}");
    }
    // The level of `absent_level` bound outside the body instead: born
    // once, before the first entry, it stands as it is for every run.
    let outside = format!(
        "{{ {CLOCK} {BAD} let lvl = sys::time::timer(duration:20.ms, false) ~ 42; \
         seqq request {{ let q = request; let y = lvl; q * 100 + y }} }}"
    );
    let (values, out) = run_delta(&outside, mode).await?;
    assert_eq!(as_i64s(&values), [142, 242, 342], "absent_level_outside\n{out}");
    // A lambda whose result is a standing level not derived from its
    // argument produces no fire when re-called: the run never completes.
    let stalls =
        format!("{{ {CLOCK} let k = 7; let f = |v| k; seqq request {{ f(request) }} }}");
    let (values, _) = run_delta(&stalls, mode).await?;
    assert_eq!(as_i64s(&values), [7]);
    let sampled = format!(
        "{{ {CLOCK} let k = 7; let f = |v| v ~ k; seqq request {{ f(request) }} }}"
    );
    let (values, _) = run_delta(&sampled, mode).await?;
    assert_eq!(as_i64s(&values), [7, 7, 7]);
    Ok(())
}

modes!(reentry_fired_only);

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

run!(seq_shadowed_core_names, SEQ_SHADOWED_CORE_NAMES, is(Value::I64(43)); FuseExpect::Jit);

// `until` has no value: using the statement's value is refused.
#[tokio::test(flavor = "current_thread")]
async fn until_last_refused() -> Result<()> {
    for src in [
        "{ let go = true; seq go { let x = 1; until (x > 0) } }",
        "{ let go = true; seq go { let y = try { until go } with(e) { 1 }; y } }",
        "{ let go = true; seq go { let y = try { 1 } with(e) { until go }; y } }",
    ] {
        super::refused_everywhere(src, "until has no value").await?;
    }
    // A bare try in the middle of a seq uses no value: until may end it.
    let v = eval_modes(
        "{ let go = true; seq go { try { until go } with(e) { 1 }; 5 } }",
        crate::TEST_REGISTER,
    )
    .await?;
    assert_eq!(v, Ok(Value::I64(5)));
    Ok(())
}

// A `?` in a seq step raises the NullError of its operand as written, as
// the same `?` outside a seq does.
run!(
    seq_null_error_names_the_operand,
    r#"{
    let x: [i64, null] = null;
    let go = sys::time::timer(duration:10.ms, false);
    seq go {
        let v = try { let y = x?; "[y]" } with(e) {
            select (e.0).error { `NullError("x") => "x was null", `NullError(s) => "names [s]" }
        };
        v
    }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "x was null")
);

// A level `x?` in a step reads `x` once, at entry, or at its first
// arrival when absent then. Only `live(x)?` sees an error `x` takes after
// the entry while a sibling holds the step (the next fixture).
run!(seq_qop_level_waits, r#"{
    let n = 0;
    n <- select n { s if s < 10 => s + 1, _ => never() };
    let trig = select n { 1 => n, _ => never() };
    let x: [i64, Error<`E>] = never();
    x <- select n { 3 => 42, _ => never() };
    seq let t = trig { let v = x?; (t, v) }
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:42]"); FuseExpect::Jit);

run!(seq_qop_level_raises_after_entry, r#"{
    let n = 0;
    n <- select n { s if s < 10 => s + 1, _ => never() };
    let trig = select n { 1 => n, _ => never() };
    let late: [i64, Error<`Late>] = select n { 5 => error(`Late), s => s };
    let caught = never();
    catch(e) caught <- select (e.0).error { `Late => "late" };
    let r = seq let t = trig {
        let w = select live(n) > 6 { true => live(n), false => never() } + live(late)?;
        w
    };
    caught
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "late"); FuseExpect::Jit);

// A step whose call sits only in an untaken select arm re-enters on every
// run: its call-free scrutinee fires at entry, so the standing level arm
// re-emits.
async fn select_level_arm_every_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let fetch = |k: i64| k * 100;
        let cached = 7;
        let hit = true;
        let t = sys::time::timer(duration:20.ms, 4) ~ 3;
        let runs = 0;
        let r = seq let k = t {
            let v = select hit { true => cached, false => fetch(k) };
            v
        };
        runs <- r ~ runs + 1;
        sys::time::after_idle(duration:100.ms, runs)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [4]);
    Ok(())
}

modes!(select_level_arm_every_run);

// `seqq let x = e` binds x as the run's own: a body write to it updates
// the run's value and enqueues nothing.
run!(
    seqq_let_name_write_stays_in_run,
    r#"{
    let t = sys::time::timer(duration:10.ms, false) ~ 1;
    let n = 0;
    let r = seqq let x = t { x <- x + 1; n <- n + 1; x };
    sys::time::after_idle(duration:100.ms, (r, n))
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:2, i64:1]")
);

// A write through a reference makes no capture live: three queued
// requests reading the target through the reference accumulate, while
// reading it by name reads each request's snapshot.
run!(
    seqq_ref_write_reads_live_only_through_the_ref,
    r#"{
    let step = 0;
    step <- select step { n if n < 60 => n + 1, _ => never() };
    let request = select step { 1 | 2 | 3 => step, _ => never() };
    let open = step > 10;
    let a = 0;
    let r = &mut a;
    let b = 0;
    let s = &mut b;
    let done = seqq request { until live(open); *r <- *r + 1; *s <- b + 1; request };
    sys::time::after_idle(duration:100.ms, (a, b))
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:3, i64:1]")
);

// A `{ .. }` statement of declarations alone is no step, and neither is a
// `let rec`, a try's included.
run!(seq_declaration_block_refused, r#"seq { { type T = i64; use str }; 1 }"#, refused("a declaration is not a seq step"); FuseExpect::None);

run!(seq_let_rec_try_refused, r#"seq { let rec x = try { 1 } with(e) { 2 }; x }"#, refused("let rec is not a seq step"); FuseExpect::None);

// A dynamic module is no seq step; inside a lambda in a seq body its
// source reads the body's names, the trigger's included.
run!(seq_dynamic_module_refused, r#"seq { mod foo dynamic { sandbox unrestricted; sig { val x: i64 }; source "let x = 1" }; foo::x }"#,
    refused("a dynamic module is not allowed inside a seq"); FuseExpect::None);

run!(seq_dynamic_source_reads_trigger, r#"{
    let t = 1;
    seq let s = (t ~ "let x = 5") {
        let f = |u: i64| {
            mod foo dynamic { sandbox unrestricted; sig { val x: i64 }; source s };
            foo::x + u
        };
        f(1)
    }
}"#, is(Value::I64(6)); FuseExpect::Jit);

// A select arm's bind that shadows a function is a value: the call reads
// it from the entry snapshot and is issued once.
run!(
    seq_arm_bind_shadowing_function_is_snapshot,
    r#"{
    let g = |x| x;
    let v = 0;
    v <- sys::time::timer(duration:20.ms, 5)$ ~ v + 1;
    let t = 1;
    let r = seq t { select v { g => sys::time::after_idle(duration:150.ms, g) } };
    sys::time::after_idle(duration:300.ms, r)
}"#,
    is(Value::I64(0))
);

// A trigger spelled as a module path is snapshotted as a bare name is:
// the run reads the value it started with, not m::t live.
run!(
    qualified_trigger_is_snapshotted,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "started 0"),
    "/test.gx" => r#"
        mod m;
        let result = seq m::t {
            let w = sys::time::timer(duration:70.ms, false);
            "started [m::t]"
        }
    "#,
    "/test/m.gx" => r#"
        let t = 0;
        t <- select t { k if k < 4 => (k ~ t) + 1, _ => never() }
    "#; FuseExpect::Jit);

// A seqq abort clause that spells the trigger's variable another way
// reads the run's request, never the variable live: the first run is
// not aborted when the variable moves on.
run!(seqq_abort_reads_the_request_by_any_spelling,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(0))),
    "/test.gx" => r#"
        mod m;
        use m::t;
        let result = seqq t; abort(select m::t { 3 => true, _ => never() }) {
            let w = sys::time::timer(duration:70.ms, false);
            m::t
        }
    "#,
    "/test/m.gx" => r#"
        let t = 0;
        t <- select t { k if k < 4 => (k ~ t) + 1, _ => never() }
    "#);

// A read that spells the trigger's variable another way (`use m::t;
// seq t { .. m::t .. }`) is snapshotted too.
run!(trigger_read_by_another_spelling_is_snapshotted,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "started 0"),
    "/test.gx" => r#"
        mod m;
        use m::t;
        let result = seq t {
            let w = sys::time::timer(duration:70.ms, false);
            "started [m::t]"
        }
    "#,
    "/test/m.gx" => r#"
        let t = 0;
        t <- select t { k if k < 4 => (k ~ t) + 1, _ => never() }
    "#; FuseExpect::Jit);

run!(sample_bank_cleared_at_seq_rebirth, r#"{
    let step = 0;
    step <- select step { s if s < 16 => s + 1, _ => never() };
    let go = select step { 1 | 8 => step, _ => never() };
    let ev = select step { 3 => step, _ => never() };
    let cancel = select step { 5 => null, _ => never() };
    let v = never();
    v <- select step { 10 => 100, _ => never() };
    let result = seq go; abort(cancel) { let r = live(ev ~ v); r };
    let completed = 0;
    completed <- result ~ completed + 1;
    select step { 16 => completed, _ => never() }
}"#, is(Value::I64(0)); FuseExpect::Jit);

run!(nested_seq_in_seqq_takes_own_entry, r#"{
    let step = 0;
    step <- select step { n if n < 12 => n + 1, _ => never() };
    let x = 1;
    let ready = false;
    x <- select step { 4 => 2, _ => never() };
    ready <- select step { 5 => true, _ => never() };
    let go = 0;
    seqq go {
        until live(ready);
        let a = x;
        let b = seq { x };
        (a, b)
    }
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

run!(step_waits_for_untaken_arm, r#"{
    let step = 0;
    step <- select step { n if n < 8 => n + 1, _ => never() };
    let mode: [`A, `B] = `A;
    let present = 1;
    let absent = never<i64>();
    let completed = 0;
    let go = 0;
    let result = seq go { select mode { `A => present, `B => absent } };
    completed <- result ~ completed + 1;
    select step { 8 => completed, _ => never() }
}"#, is(Value::I64(0)); FuseExpect::Jit);

run!(step_reads_present_values_when_untaken_arm_arrives, r#"{
    let step = 0;
    step <- select step { n if n < 8 => n + 1, _ => never() };
    let mode: [`A, `B] = `A;
    let present = step;
    let absent = never<i64>();
    absent <- select step { 4 => 100, _ => never() };
    let go = 0;
    seq go { select mode { `A => present, `B => absent } }
}"#, is(Value::I64(5)); FuseExpect::Jit);
