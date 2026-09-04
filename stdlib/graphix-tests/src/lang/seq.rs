// seq (`design/seq_blocks.md`): pc-machine atoms, then the surface
// construct. Straight-line only — no if/loop.

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
