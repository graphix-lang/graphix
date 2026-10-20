// Tests for variant types

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints},
};
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

run!(variants0, VARIANTS0, prints("[i64:0, i64:1]"); FuseExpect::Jit);

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

run!(variants1, VARIANTS1, is(Value::I64(0)); FuseExpect::Jit);
