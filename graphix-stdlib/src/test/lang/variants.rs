// Tests for variant types

use crate::run;
use anyhow::{bail, Result};
use netidx::publisher::Value;

const VARIANTS0: &str = r#"
{
  let a = select array::iter([`Foo, `Bar("hello world")]) {
    `Foo => 0,
    `Bar(s) if s == "hello world" => 1,
     _ => 2
  };
  array::group(a, |n, _| n == 2)
}
"#;

run!(variants0, VARIANTS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(0), Value::I64(1)] => true,
        _ => false,
    },
    _ => false,
});

const VARIANTS1: &str = r#"
{
    let mode = select 0 {
        0 => `List,
        _ => `Table
    };
    select mode {
        `List => 0,
        `Table => 1
    }
}
"#;

run!(variants1, VARIANTS1, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});
