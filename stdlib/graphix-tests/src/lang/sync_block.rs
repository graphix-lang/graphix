//! The sync subset (design/sync_subset.md): `sync { … }` blocks with
//! `let mut`, assignment, and `for` loops, desugared into the existing
//! vocabulary (assignment = shadowing, for = fold over the assigned
//! set, arm-assigns hoist to tuple yields). Every fixture runs
//! DIFFERENTIALLY (interp vs jit) via `run!` — the desugar is the
//! single specification, the two evaluators the two elaborations.

use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::publisher::Value;

// ── rung 1: sync bodies → one kernel ────────────────────────────────

const SUM_FOR: &str = r#"
sync {
  let mut res = 0;
  for v in [1, 2, 3] { res = res + v };
  res
}
"#;

run!(sum_for, SUM_FOR, |v: Result<&Value>| matches!(v, Ok(Value::I64(6)));
     FuseExpect::Jit);

const FILTER_FOR: &str = r#"
sync {
  let mut res = [];
  for v in [1, 2, 3, 4] {
    select v % 2 { 0 => res = array::push(res, v), _ => null }
  };
  res
}
"#;

run!(filter_for, FILTER_FOR, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(2), Value::I64(4)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

const TWO_MUTS: &str = r#"
sync {
  let mut a = 0;
  let mut b = 1;
  for v in [1, 2, 3] { a = a + v; b = b * v };
  (a, b)
}
"#;

run!(two_muts, TWO_MUTS, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::I64(6), Value::I64(6)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

const NESTED_FOR_MAX: &str = r#"
sync {
  let mut m = 0;
  for row in [[1, 9], [3, 2]] {
    for v in row {
      select v > m { true => m = v, false => null }
    }
  };
  m
}
"#;

run!(nested_for_max, NESTED_FOR_MAX, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(9))
); FuseExpect::Jit);

const STRAIGHT_LINE_REBIND: &str = r#"
sync {
  let mut x = 1;
  x = x + 1;
  x = x * 10;
  x
}
"#;

run!(straight_line_rebind, STRAIGHT_LINE_REBIND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(20))
); FuseExpect::Jit);

// ── rung 2: an async call in the body rides the impure-fold path ────
// `once(v)` is async → the desugared fold's callback is impure →
// MapQ/FoldQ per-slot machinery (slots, re-evaluation, taint). No
// effect analysis anywhere — the ladder is implicit in the desugar.
// The impure split even PARTIALLY fuses: each slot's sync sub-region
// compiles per slot while the `once` residue node-walks — hence Jit.

const ASYNC_BODY: &str = r#"
sync {
  let mut res = 0;
  for v in [1, 2, 3] { res = res + once(v) };
  res
}
"#;

run!(async_body, ASYNC_BODY, |v: Result<&Value>| matches!(v, Ok(Value::I64(6)));
     FuseExpect::Jit);

// ── select in statement position assigning from arms ────────────────

const ARM_ASSIGN_STMT: &str = r#"
sync {
  let mut hi = 0;
  let mut lo = 100;
  for v in [5, 50, 7] {
    select v > hi { true => hi = v, false => null };
    select v < lo { true => lo = v, false => null }
  };
  (lo, hi)
}
"#;

run!(arm_assign_stmt, ARM_ASSIGN_STMT, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::I64(5), Value::I64(50)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);
