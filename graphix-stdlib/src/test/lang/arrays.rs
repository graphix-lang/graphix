// Tests for arrays: indexing, matching, operations

use crate::run;
use anyhow::{bail, Result};
use arcstr::ArcStr;
use netidx::publisher::Value;

const ARRAY_INDEXING0: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[0]
}
"#;

run!(array_indexing0, ARRAY_INDEXING0, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

const ARRAY_INDEXING1: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[0..3]
}
"#;

run!(array_indexing1, ARRAY_INDEXING1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(0), Value::I64(1), Value::I64(2)] =>
        true,
    _ => false,
});

const ARRAY_INDEXING2: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[..2]
}
"#;

run!(array_indexing2, ARRAY_INDEXING2, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(0), Value::I64(1)] => true,
    _ => false,
});

const ARRAY_INDEXING3: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[5..]
}
"#;

run!(array_indexing3, ARRAY_INDEXING3, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &a[..] == [Value::I64(5), Value::I64(6)] => true,
    _ => false,
});

const ARRAY_INDEXING4: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  a[..]
}
"#;

run!(array_indexing4, ARRAY_INDEXING4, |v: Result<&Value>| match v {
    Ok(Value::Array(a))
        if &a[..]
            == [
                Value::I64(0),
                Value::I64(1),
                Value::I64(2),
                Value::I64(3),
                Value::I64(4),
                Value::I64(5),
                Value::I64(6)
            ] =>
        true,
    _ => false,
});

const ARRAY_INDEXING5: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  let out = select array::iter(a) {
    i64 as i => a[i] + 1
  };
  array::group(out, |i, x| i == 7)
}
"#;

run!(array_indexing5, ARRAY_INDEXING5, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const ARRAY_INDEXING6: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  let out = select array::iter(a) {
    i64 as i => a[i]? + 1
  };
  array::group(out, |i, x| i == 7)
}
"#;

run!(array_indexing6, ARRAY_INDEXING6, |v: Result<&Value>| match v {
    Ok(Value::Array(a))
        if &a[..]
            == [
                Value::I64(1),
                Value::I64(2),
                Value::I64(3),
                Value::I64(4),
                Value::I64(5),
                Value::I64(6),
                Value::I64(7)
            ] =>
        true,
    _ => false,
});

const ARRAY_MATCH0: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6];
  select a {
    [a, b, c, d, ..] => a + b + c + d,
    _ => never()
  }
}
"#;

run!(array_match0, ARRAY_MATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
});

const ARRAY_MATCH1: &str = r#"
{
  let a = [0, 1, 2, 3, 4, 5, 6, 7];
  let out = select a {
    [x, y, tl..] => {
      a <- tl;
      [x, y]
    },
    _ => never()
  };
  array::group(out, |i, x| i == 4)
}
"#;

run!(array_match1, ARRAY_MATCH1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        a.len() == 4 && {
            a.iter().enumerate().all(|(i, a)| match a {
                Value::Array(a) => {
                    a.len() == 2
                        && match &a[0] {
                            Value::I64(x) => *x as usize == i * 2,
                            _ => false,
                        }
                        && match &a[1] {
                            Value::I64(x) => *x as usize == i * 2 + 1,
                            _ => false,
                        }
                }
                _ => false,
            })
        }
    }
    _ => false,
});

const ARRAY_MATCH2: &str = r#"
{
    let a = [];
    let b = [0, 1, 2, 3, 4, 5, 6];
    let r = select uniq(array::iter([a, a, a, b])) {
        [] => `Empty,
        _ => `Nonempty
    };
    array::group(r, |n, _| n == 2)
}
"#;

run!(array_match2, ARRAY_MATCH2, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[ArcStr; 2]>() {
        Ok([s0, s1]) if &*s0 == "Empty" && &*s1 == "Nonempty" => true,
        _ => false,
    },
    _ => false,
});
