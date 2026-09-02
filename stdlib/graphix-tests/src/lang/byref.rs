// Tests for by-reference operations

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

const BYREF_DEREF: &str = r#"
{
  let a = 42;
  let x = &a;
  *x
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(byref_deref, BYREF_DEREF, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const BYREF_TUPLE: &str = r#"
{
  let r = &(1, 2);
  let t = *r;
  t.0 + t.1
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(byref_tuple, BYREF_TUPLE, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const BYREF_PATTERN: &str = r#"
{
  let r = &42;
  select r {
    &i64 as v => *v
  }
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(byref_pattern, BYREF_PATTERN, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const CONNECT_DEREF0: &str = r#"
{
  let v = 41;
  let r = &v;
  *r <- *r + 1;
  array::group(v, |n, _| n == 2)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(connect_deref0, CONNECT_DEREF0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const CONNECT_DEREF1: &str = r#"
{
  let f = |x: &i64| *x <- *x + 1;
  let v = 41;
  f(&v);
  array::group(v, |n, _| n == 2)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(connect_deref1, CONNECT_DEREF1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Refs are first-class runtime values (`Value::U64(bind_id)` — Deref
// re-registers lazily off the value), so a ref read back out of a
// CONTAINER derefs like any other. `Deref::typecheck0` used to match
// the child type structurally (`Type::ByRef` only) and rejected the
// TVar-bound `&T` an accessor/`$` read produces — `*(a[0]$)` over
// `Array<&i64>` was "expected reference" at compile time (2026-07-08).
const DEREF_FROM_ARRAY: &str = r#"
{
  let v = 42;
  let a = [&v];
  *(a[0]$)
}
"#;

run!(deref_from_array, DEREF_FROM_ARRAY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(42))
); graphix_package_core::testing::FuseExpect::Jit);

const DEREF_FROM_TUPLE_FIELD: &str = r#"
{
  let v = 7;
  let p = (&v, 1);
  let s = { x: &v };
  *(p.0) + *(s.x)
}
"#;

run!(deref_from_tuple_field, DEREF_FROM_TUPLE_FIELD, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(14))
); graphix_package_core::testing::FuseExpect::Jit);

// ── place references (design/place_references.md, 2026-09-02) ──

// Reads through every accessor kind, and writes through each — the
// root value rebuilt along the path.
const PLACE_READ_WRITE: &str = r#"
{
  let a = [10, 20, 30];
  let s = { x: 5, tags: ["p", "q"] };
  let t = (7, 8);
  let m = {"k" => 9};
  let ra = &a[1];
  let rs = &s.x;
  let rn = &s.tags[0];
  let rt = &t.1;
  let rm = &m{"k"};
  let before = once((*ra, *rs, *rn, *rt, *rm));
  let t1 = sys::time::timer(duration:0.05s, false);
  *ra <- t1 ~ 21;
  *rs <- t1 ~ 6;
  *rn <- t1 ~ "P";
  *rt <- t1 ~ 88;
  *rm <- t1 ~ 99;
  let t2 = sys::time::timer(duration:0.2s, false);
  t2 ~ (before, a, s, t, m{"k"}$, (*ra, *rs, *rn, *rt, *rm))
}
"#;

run!(place_read_write, PLACE_READ_WRITE, |v: Result<&Value>| {
    format!("{}", v.unwrap())
        == r#"[[i64:20, i64:5, "p", i64:8, i64:9], [i64:10, i64:21, i64:30], [["tags", ["P", "q"]], ["x", i64:6]], [i64:7, i64:88], i64:99, [i64:21, i64:6, "P", i64:88, i64:99]]"#
}; graphix_package_core::testing::FuseExpect::Jit);

// A moving reference points where its key says when it fires; two
// writes to one root in one cycle both land (patches resolve at
// delivery, each on the other's result); a write into a place that
// does not exist is dropped and the root is untouched.
const PLACE_MOVE_SIBLINGS_BAD: &str = r#"
{
  let a = [1, 2, 3];
  let i = 0;
  let r = &a[i];
  let r0 = &a[0];
  let r2 = &a[2];
  let bad = &a[7];
  let first = once(*r);
  let t1 = sys::time::timer(duration:0.05s, false);
  i <- t1 ~ 2;
  let t2 = sys::time::timer(duration:0.12s, false);
  *r <- t2 ~ 30;
  let t3 = sys::time::timer(duration:0.2s, false);
  *r0 <- t3 ~ 100;
  *r2 <- t3 ~ 300;
  *bad <- t3 ~ 9;
  let t4 = sys::time::timer(duration:0.3s, false);
  t4 ~ (first, a, *r)
}
"#;

run!(place_move_siblings_bad, PLACE_MOVE_SIBLINGS_BAD, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:1, [i64:100, i64:2, i64:300], i64:300]"
}; graphix_package_core::testing::FuseExpect::Jit);

// The form's shape: a lambda over `&State` reaches an editor held in
// an array through a place reference passed as its argument.
const PLACE_THROUGH_PARAM: &str = r#"
{
  type State = { value: string, cursor: i64 };
  let vals: Array<State> = [{ value: "a", cursor: 1 }, { value: "b", cursor: 1 }];
  let bump = |st: &State, t: Any| -> null {
    let s = t ~ *st;
    *st <- { value: "[s.value]!", cursor: s.cursor + 1 };
    null
  };
  let t1 = sys::time::timer(duration:0.05s, false);
  let go = t1 ~ 1;
  bump(&vals[go], go);
  let t2 = sys::time::timer(duration:0.2s, false);
  t2 ~ vals
}
"#;

run!(place_through_param, PLACE_THROUGH_PARAM, |v: Result<&Value>| {
    format!("{}", v.unwrap())
        == r#"[[["cursor", i64:1], ["value", "a"]], [["cursor", i64:2], ["value", "b!"]]]"#
}; graphix_package_core::testing::FuseExpect::Jit);
