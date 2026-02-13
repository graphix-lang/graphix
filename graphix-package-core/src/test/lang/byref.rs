use crate::test::{REGISTER, ROOT};
// Tests for by-reference operations

use crate::run;
use anyhow::Result;
use netidx::publisher::Value;

const BYREF_DEREF: &str = r#"
{
  let a = 42;
  let x = &a;
  *x
}
"#;

run!(byref_deref, BYREF_DEREF, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const BYREF_TUPLE: &str = r#"
{
  let r = &(1, 2);
  let t = *r;
  t.0 + t.1
}
"#;

run!(byref_tuple, BYREF_TUPLE, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const BYREF_PATTERN: &str = r#"
{
  let r = &42;
  select r {
    &i64 as v => *v
  }
}
"#;

run!(byref_pattern, BYREF_PATTERN, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const CONNECT_DEREF0: &str = r#"
{
  let v = 41;
  let r = &v;
  *r <- *r + 1;
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref0, CONNECT_DEREF0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
});

const CONNECT_DEREF1: &str = r#"
{
  let f = |x: &i64| *x <- *x + 1;
  let v = 41;
  f(&v);
  array::group(v, |n, _| n == 2)
}
"#;

run!(connect_deref1, CONNECT_DEREF1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(41), Value::I64(42)] => true,
        _ => false,
    },
    _ => false,
});
