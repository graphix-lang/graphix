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
