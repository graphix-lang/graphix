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
#[native]
sync {
  let mut res = 0;
  for v in [1, 2, 3] { res = res + v };
  res
}
"#;

run!(sum_for, SUM_FOR, |v: Result<&Value>| matches!(v, Ok(Value::I64(6)));
     FuseExpect::Jit);

const FILTER_FOR: &str = r#"
#[native]
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
#[native]
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
#[native]
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
#[native]
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

// async in ONE arm only — the elaboration ladder is per-shape, not
// per-block: the impure-fold split still applies with the other arm
// pure, and both arms' assigns thread through the same acc
const ASYNC_ARM: &str = r#"
sync {
  let mut res = 0;
  for v in [1, 2, 3, 4] {
    select v % 2 { 0 => res = res + once(v * 10), _ => res = res + v }
  };
  res
}
"#;

run!(async_arm, ASYNC_ARM, |v: Result<&Value>| matches!(v, Ok(Value::I64(64)));
     FuseExpect::Jit);

// async in the INNER loop of a nested for — the outer fold's callback
// contains the impure inner fold
const ASYNC_NESTED: &str = r#"
sync {
  let mut m = 0;
  for row in [[1, 9], [3, 2]] {
    for v in row { m = m + once(v) }
  };
  m
}
"#;

run!(async_nested, ASYNC_NESTED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(15))
); FuseExpect::Jit);

// ── select in statement position assigning from arms ────────────────

const ARM_ASSIGN_STMT: &str = r#"
#[native]
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

// ── accumulator shapes beyond scalars ───────────────────────────────

const STRING_ACC: &str = r#"
#[native]
sync {
  let mut s = "";
  for v in [1, 2, 3] { s = "[s][v]" };
  s
}
"#;

run!(string_acc, STRING_ACC, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "123",
    _ => false,
}; FuseExpect::Jit);

const STRUCT_ACC: &str = r#"
#[native]
sync {
  let mut st = {n: 0, sum: 0};
  for v in [1, 2, 3] { st = {n: st.n + 1, sum: st.sum + v} };
  st.n * 100 + st.sum
}
"#;

run!(struct_acc, STRUCT_ACC, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(306))
); FuseExpect::Jit);

// iterating a computed array — the desugared fold's source is itself a HOF
const RUNTIME_ITER: &str = r#"
#[native]
sync {
  let mut res = 0;
  for v in array::map([1, 2, 3], |x| x * 2) { res = res + v };
  res
}
"#;

run!(runtime_iter, RUNTIME_ITER, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(12))
); FuseExpect::Jit);

// multi-mut mixing a composite and a string accumulator — the fold's
// acc is a tuple with Composite and String destructure leaves, cloned
// borrowed body results, and per-iteration old-acc drops. The res
// annotation is REQUIRED for now: an unannotated `let mut res = []`
// leaves the no-assign arm's element tvar unbound, the arm-tuple
// union can't collapse, and `union.0` is a compile error (pending
// Eric ruling — fuzz/pending-ruling/sync_multimut_union_tuple_ref.md).
const MIXED_ACC: &str = r#"
#[native]
sync {
  let mut res: Array<i64> = [];
  let mut s = "";
  for v in [1, 2, 3, 4] {
    select v % 2 { 0 => { res = array::push(res, v); s = "[s]." }, _ => null }
  };
  (array::len(res), str::len(s))
}
"#;

run!(mixed_acc, MIXED_ACC, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::I64(2), Value::I64(2)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

// the List accumulator idiom (cons + reverse) — List is a recursive ADT,
// no fixed ABI layout, so this is correct-None territory for now
const LIST_ACC: &str = r#"
sync {
  let mut res = list::nil(null);
  for v in [1, 2, 3] { res = list::cons(v * 10, res) };
  list::to_array(list::reverse(res))
}
"#;

run!(list_acc, LIST_ACC, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(10), Value::I64(20), Value::I64(30)] => true,
        _ => false,
    },
    _ => false,
}; FuseExpect::None);
