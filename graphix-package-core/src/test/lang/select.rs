use crate::test::{REGISTER, ROOT};
// Tests for select/match expressions

use crate::run;
use anyhow::Result;
use netidx::publisher::Value;

const SELECT0: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  select any(x, y, z) {
    v if v == 1 => "first [v]",
    v if v == 2 => "second [v]",
    v => "third [v]"
  }
}
"#;

run!(select0, SELECT0, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "first 1",
    _ => false,
});

const LOOPING_SELECT: &str = r#"
{
  let v: [Number, string, error] = "1";
  let v = select v {
    Number as i => i,
    string as s => v <- cast<i64>(s),
    error as e => never(e)
  };
  v + 1
}
"#;

run!(looping_select, LOOPING_SELECT, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const SELECTSTRUCT: &str = r#"
{
  type T = { foo: string, bar: i64, baz: f64 };
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  select x {
    T as { foo: "foo", bar: 8, baz } => baz,
    T as { bar, baz, .. } => bar + baz
  }
}
"#;

run!(selectstruct, SELECTSTRUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(126.0)) => true,
    _ => false,
});

const MATCH_EXHAUST0: &str = r#"
select 42 {
    1 => never(),
    2 => never(),
    5 => never()
}
"#;

run!(match_exhaust0, MATCH_EXHAUST0, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const MATCH_EXHAUST1: &str = r#"
select 42 {
    1 => never(),
    2 => never(),
    _ => 42
}
"#;

run!(match_exhaust1, MATCH_EXHAUST1, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const NESTEDMATCH0: &str = r#"
{
  type T = { foo: (string, i64, f64), bar: i64, baz: f64 };
  let x = { foo: ("bar", 42, 5.0), bar: 42, baz: 84.0 };
  let { foo: (_, x, y), .. }: T = x;
  x + y
}
"#;

run!(nestedmatch0, NESTEDMATCH0, |v: Result<&Value>| match v {
    Ok(Value::F64(47.0)) => true,
    _ => false,
});

const NESTEDMATCH1: &str = r#"
{
  type T = { foo: {x: string, y: i64, z: f64}, bar: i64, baz: f64 };
  let x = { foo: { x: "bar", y: 42, z: 5.0 }, bar: 42, baz: 84.0 };
  select x {
    T as { foo: { y, z, .. }, .. } => y + z
  }
}
"#;

run!(nestedmatch1, NESTEDMATCH1, |v: Result<&Value>| match v {
    Ok(Value::F64(47.0)) => true,
    _ => false,
});

const NESTEDMATCH2: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let { foo: [x, y, ..], ..}: T = x;
  x + y
}
"#;

run!(nestedmatch2, NESTEDMATCH2, |v: Result<&Value>| match v {
    Err(e) => {
        dbg!(e);
        true
    }
    _ => false,
});

const NESTEDMATCH3: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  select x {
    { foo: [x, y, ..], bar: _, baz: _ } => x + y,
    _ => never()
  }
}
"#;

run!(nestedmatch3, NESTEDMATCH3, |v: Result<&Value>| match v {
    Ok(Value::F64(3.0)) => true,
    _ => false,
});
