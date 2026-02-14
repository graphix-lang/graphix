use anyhow::Result;
use arcstr::literal;
use graphix_package_core::run;
use netidx::subscriber::Value;

const MAP_LEN: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::len(m)
}
"#;

run!(map_len, MAP_LEN, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const MAP_GET_PRESENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(m, "b")
}
"#;

run!(map_get_present, MAP_GET_PRESENT, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const MAP_GET_ABSENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(m, "d")
}
"#;

run!(map_get_absent, MAP_GET_ABSENT, |v: Result<&Value>| match v {
    Ok(Value::Null) => true,
    _ => false,
});

const MAP_MAP: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::map(m, |(k, v)| (k, v * 2))
}
"#;

run!(map_map, MAP_MAP, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 3
            && m[&Value::String(literal!("a"))] == Value::I64(2)
            && m[&Value::String(literal!("b"))] == Value::I64(4)
            && m[&Value::String(literal!("c"))] == Value::I64(6),
    _ => false,
});

const MAP_FILTER: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3, "d" => 4};
  map::filter(m, |(k, v)| v > 2)
}
"#;

run!(map_filter, MAP_FILTER, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 2
            && m[&Value::String(literal!("c"))] == Value::I64(3)
            && m[&Value::String(literal!("d"))] == Value::I64(4),
    _ => false,
});

const MAP_FILTER_MAP: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3, "d" => 4};
  map::filter_map(m, |(k, v)| select v { v if v > 2 => (k, v * 10), _ => null })
}
"#;

run!(map_filter_map, MAP_FILTER_MAP, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 2
            && m[&Value::String(literal!("c"))] == Value::I64(30)
            && m[&Value::String(literal!("d"))] == Value::I64(40),
    _ => false,
});

const MAP_FOLD: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::fold(m, 0, |acc, (k, v)| acc + v)
}
"#;

run!(map_fold, MAP_FOLD, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
});

const MAP_ITER: &str = r#"
{
  let m = {"a" => 1, "b" => 2};
  let (_, v) = map::iter(m);
  array::group(v, |n, _| n == 2)
}
"#;

run!(map_iter, MAP_ITER, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2)] => true,
        _ => false,
    },
    _ => false,
});

const MAP_ITERQ: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  m <- {"d" => 4, "e" => 5};
  let clock = 1;
  let (_, v) = map::iterq(#clock, m);
  array::group(v, |n, _| {
    clock <- n;
    n == 5
  })
}
"#;

run!(map_iterq, MAP_ITERQ, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5)] =>
            true,
        _ => false,
    },
    _ => false,
});

const MAP_INSERT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  let m = map::insert(m, "d", 4);
  let m = map::insert(m, "e", 5);
  m == { "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5 }
}
"#;

run!(map_insert, MAP_INSERT, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
});

const MAP_REMOVE: &str = r#"
{
  let m = { "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5 };
  let m = map::remove(m, "d");
  let m = map::remove(m, "e");
  m == {"a" => 1, "b" => 2, "c" => 3}
}
"#;

run!(map_remove, MAP_REMOVE, |v: Result<&Value>| match v {
    Ok(Value::Bool(true)) => true,
    _ => false,
});
