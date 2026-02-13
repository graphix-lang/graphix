use crate::run;
use crate::test::{REGISTER, ROOT};
use anyhow::Result;
use netidx::subscriber::Value;

const IS_ERR: &str = r#"
{
  let errors: Error<Any> = never();
  try
    let a = [42, 43, 44];
    let y = a[0]? + a[3]?
  catch(e) => errors <- e;
  is_err(errors)
}
"#;

run!(is_err, IS_ERR, |v: Result<&Value>| match v {
    Ok(Value::Bool(b)) => *b,
    _ => false,
});

const FILTER_ERR: &str = r#"
{
  let a = [42, 43, 44, error("foo")];
  filter_err(array::iter(a))
}
"#;

run!(filter_err, FILTER_ERR, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const ERROR: &str = r#"
  error("foo")
"#;

run!(error, ERROR, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const ONCE: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  once(array::iter(x))
}
"#;

run!(once, ONCE, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
});

const SKIP: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  array::group(skip(#n: 3, array::iter(x)), |n, _| n == 3)
}
"#;

run!(skip, SKIP, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(4), Value::I64(5), Value::I64(6)] => true,
            _ => false,
        },
        _ => false,
    }
});

const SKIP_ZERO: &str = r#"
{
  let x = [1, 2, 3];
  array::group(skip(#n: 0, array::iter(x)), |n, _| n == 3)
}
"#;

run!(skip_zero, SKIP_ZERO, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const SKIP_ALL: &str = r#"
{
  let timeout = time::timer(1, false) ~ 0;
  any(skip(#n: 5, array::iter([1, 2, 3])), timeout)
}
"#;

run!(skip_all, SKIP_ALL, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

const TAKE: &str = r#"
{
  let x = [1, 2, 3, 4, 5, 6];
  array::group(take(#n: 3, array::iter(x)), |n, _| n == 3)
}
"#;

run!(take, TAKE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const TAKE_ZERO: &str = r#"
{
  let timeout = time::timer(1, false) ~ 0;
  any(take(#n: 0, array::iter([1, 2, 3])), timeout)
}
"#;

run!(take_zero, TAKE_ZERO, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

const TAKE_MORE: &str = r#"
{
  let x = [1, 2, 3];
  array::group(take(#n: 10, array::iter(x)), |n, _| n == 3)
}
"#;

run!(take_more, TAKE_MORE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const ALL: &str = r#"
{
  let x = 1;
  let y = x;
  let z = y;
  all(x, y, z)
}
"#;

run!(all, ALL, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
});

const SUM: &str = r#"
{
  let tweeeeenywon = [1, 2, 3, 4, 5, 6];
  sum(tweeeeenywon)
}
"#;

run!(sum, SUM, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
});

const PRODUCT: &str = r#"
{
  let tweeeeenywon = [5, 2, 2, 1.05];
  product(tweeeeenywon)
}
"#;

run!(product, PRODUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(21.0)) => true,
    _ => false,
});

const DIVIDE: &str = r#"
{
  let tweeeeenywon = [84, 2, 2];
  divide(tweeeeenywon)
}
"#;

run!(divide, DIVIDE, |v: Result<&Value>| match v {
    Ok(Value::I64(21)) => true,
    _ => false,
});

const MIN: &str = r#"
   min(1, 2, 3, 4, 5, 6, 0)
"#;

run!(min, MIN, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

const MAX: &str = r#"
   max(1, 2, 3, 4, 5, 6, 0)
"#;

run!(max, MAX, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
});

const AND: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  and(x < y, y < z, x > 0, z < 10)
}
"#;

run!(and, AND, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
});

const OR: &str = r#"
  or(false, false, true)
"#;

run!(or, OR, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
});

const INDEX: &str = r#"
{
  let a = ["foo", "bar", 1, 2, 3];
  cast<i64>(a[2]?)? + cast<i64>(a[3]?)?
}
"#;

run!(index, INDEX, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const SLICE: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  [sum(a[2..4]?), sum(a[6..]?), sum(a[..2]?)]
}
"#;

run!(slice, SLICE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(7), Value::I64(15), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const FILTER0: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  filter(array::iter(a), |x| x > 7)
}
"#;

run!(filter0, FILTER0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(8)) => true,
        _ => false,
    }
});

const FILTER1: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  filter(array::iter(a), |x| str::len(x) > 7)
}
"#;

run!(filter1, FILTER1, |v: Result<&Value>| {
    match v {
        Ok(_) => false,
        Err(_) => true,
    }
});

const QUEUE: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::map(a, |v| net::publish("/local/[v]", v));
  let v = array::iter(a);
  let clock: Any = once(v);
  let q = queue(#clock, v);
  let out = net::subscribe("/local/[q]")?;
  clock <- out;
  array::group(out, |n, _| n == 8)
}
"#;

run!(queue, QUEUE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5), Value::I64(6), Value::I64(7), Value::I64(8)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
});

const COUNT: &str = r#"
{
  let a = [0, 1, 2, 3];
  array::group(count(array::iter(a)), |n, _| n == 4)
}
"#;

run!(count, COUNT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4)] => true,
            _ => false,
        },
        _ => false,
    }
});

const SAMPLE: &str = r#"
{
  let a = [0, 1, 2, 3];
  let x = "tweeeenywon!";
  array::group(array::iter(a) ~ x, |n, _| n == 4)
}
"#;

run!(sample, SAMPLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2), Value::String(s3)] => {
                s0 == s1 && s1 == s2 && s2 == s3 && &**s3 == "tweeeenywon!"
            }
            _ => false,
        },
        _ => false,
    }
});

const UNIQ: &str = r#"
{
  let a = [1, 1, 1, 1, 1, 1, 1];
  uniq(array::iter(a))
}
"#;

run!(uniq, UNIQ, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(1)) => true,
        _ => false,
    }
});

const SEQ: &str = r#"
  array::group(seq(0, 4), |n, _| n == 4)
"#;

run!(seq, SEQ, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(0), Value::I64(1), Value::I64(2), Value::I64(3)] => true,
            _ => false,
        },
        _ => false,
    }
});

const THROTTLE: &str = r#"
{
    let data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let data = throttle(array::iter(data));
    array::group(data, |n, _| n == 2)
}
"#;

run!(throttle, THROTTLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(10)] => true,
            _ => false,
        },
        _ => false,
    }
});

const NEVER: &str = r#"
{
   let x = never(100);
   any(x, 0)
}
"#;

run!(never, NEVER, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(0)) => true,
        _ => false,
    }
});

const MEAN: &str = r#"
{
  let a = [0, 1, 2, 3];
  mean(a)
}
"#;

run!(mean, MEAN, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(1.5)) => true,
        _ => false,
    }
});

const RAND: &str = r#"
  rand::rand(#clock:null)
"#;

run!(rand, RAND, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(v)) if *v >= 0. && *v < 1.0 => true,
        _ => false,
    }
});

const RAND_PICK: &str = r#"
  rand::pick(["Chicken is coming", "Grape", "Pilot!"])
"#;

run!(rand_pick, RAND_PICK, |v: Result<&Value>| {
    match v {
        Ok(Value::String(v)) => v == "Chicken is coming" || v == "Grape" || v == "Pilot!",
        _ => false,
    }
});

const RAND_SHUFFLE: &str = r#"
  rand::shuffle(["Chicken is coming", "Grape", "Pilot!"])
"#;

run!(rand_shuffle, RAND_SHUFFLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) if a.len() == 3 => {
            a.contains(&Value::from("Chicken is coming"))
                && a.contains(&Value::from("Grape"))
                && a.contains(&Value::from("Pilot!"))
        }
        _ => false,
    }
});

const HOLD_BASIC: &str = r#"
{
  let clock = 1;
  let value = 42;
  hold(#clock, value)
}
"#;

run!(hold_basic, HOLD_BASIC, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const HOLD_MULTIPLE: &str = r#"
{
  let values = [10, 20, 30];
  let triggers = [1, 1, 1];
  let v = hold(#clock:array::iter(triggers), array::iter(values));
  let held_values = array::group(v, |n, _| n == 3);
  array::len(held_values)
}
"#;

run!(hold_multiple, HOLD_MULTIPLE, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const HOLD_NO_TRIGGER: &str = r#"
{
  let clock = never();
  let value = 42;
  any(count(hold(#clock, value)), 0)
}
"#;

run!(hold_no_trigger, HOLD_NO_TRIGGER, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

const HOLD_MULTIPLE_VALUES: &str = r#"
{
  let clock = time::timer(0.5, false) ~ 1;
  let values = [100, 200, 300];
  // Only the last value should be held when clock fires
  hold(#clock, array::iter(values))
}
"#;

run!(hold_multiple_values, HOLD_MULTIPLE_VALUES, |v: Result<&Value>| match v {
    Ok(Value::I64(300)) => true,
    _ => false,
});

const NOW: &str = r#"time::now(null)"#;

run!(now, NOW, |v: Result<&Value>| match v {
    Ok(Value::DateTime(_)) => true,
    _ => false,
});
