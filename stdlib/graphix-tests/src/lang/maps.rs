// Tests for map literals and operations

use graphix_package_core::run;
use anyhow::Result;
use arcstr::ArcStr;
use netidx::publisher::Value;

const MAP0: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  m
}
"#;

run!(map0, MAP0, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 3
            && m.get(&Value::String("a".into()))
                .map(|v| *v == Value::I64(1))
                .unwrap_or(false)
            && m.get(&Value::String("b".into()))
                .map(|v| *v == Value::I64(2))
                .unwrap_or(false)
            && m.get(&Value::String("c".into()))
                .map(|v| *v == Value::I64(3))
                .unwrap_or(false),
    _ => false,
});

const MAP1: &str = r#"
{
  let m = {1 => "one", 2 => "two", 3 => "three"};
  m
}
"#;

run!(map1, MAP1, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 3
            && m.get(&Value::I64(1))
                .map(|v| *v == Value::String("one".into()))
                .unwrap_or(false)
            && m.get(&Value::I64(2))
                .map(|v| *v == Value::String("two".into()))
                .unwrap_or(false)
            && m.get(&Value::I64(3))
                .map(|v| *v == Value::String("three".into()))
                .unwrap_or(false),
    _ => false,
});

const MAP2: &str = r#"
{
  let m = {true => "yes", false => "no"};
  m
}
"#;

run!(map2, MAP2, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 2
            && m.get(&Value::Bool(true))
                .map(|v| *v == Value::String("yes".into()))
                .unwrap_or(false)
            && m.get(&Value::Bool(false))
                .map(|v| *v == Value::String("no".into()))
                .unwrap_or(false),
    _ => false,
});

const MAP_EMPTY: &str = r#"
{
  let m = {};
  m
}
"#;

run!(map_empty, MAP_EMPTY, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) => m.len() == 0,
    _ => false,
});

const MAP_REF0: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  m{"b"}
}
"#;

run!(map_ref0, MAP_REF0, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const MAP_REF1: &str = r#"
{
  let m = {1 => "one", 2 => "two", 3 => "three"};
  m{2}
}
"#;

run!(map_ref1, MAP_REF1, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s.as_str() == "two" => true,
    _ => false,
});

const MAP_REF2: &str = r#"
{
  let m = {true => "yes", false => "no"};
  m{true}
}
"#;

run!(map_ref2, MAP_REF2, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s.as_str() == "yes" => true,
    _ => false,
});

const MAP_REF_MISSING: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  m{"d"}
}
"#;

run!(map_ref_missing, MAP_REF_MISSING, |v: Result<&Value>| match v {
    Ok(Value::Error(e)) => {
        if let Ok((tag, msg)) = e.as_ref().clone().cast_to::<(ArcStr, ArcStr)>() {
            tag.as_str() == "MapKeyError" && msg.as_str().contains("not found")
        } else {
            false
        }
    }
    _ => false,
});

const MAP_REF_WRONG_TYPE: &str = r#"
{
  let m = {1 => "1", 2 => "2", 3 => "3"};
  m{"a"}
}
"#;

run!(map_ref_wrong_type, MAP_REF_WRONG_TYPE, |v: Result<&Value>| match v {
    Err(_) => true, // Type error at compile time is expected
    _ => false,
});

const MAP_NESTED: &str = r#"
{
  let m = {"outer" => {"inner" => 42}};
  m{"outer"}
}
"#;

run!(map_nested, MAP_NESTED, |v: Result<&Value>| match v {
    Ok(Value::Map(inner_map)) => {
        inner_map
            .get(&Value::String("inner".into()))
            .map(|v| *v == Value::I64(42))
            .unwrap_or(false)
    }
    _ => false,
});

const MAP_COMPLEX_KEYS: &str = r#"
{
  let key1 = {name: "john", age: 30};
  let key2 = {name: "jane", age: 25};
  let m = {key1 => "john_value", key2 => "jane_value"};
  (m{key1}, m{key2})
}
"#;

run!(map_complex_keys, MAP_COMPLEX_KEYS, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<(Value, Value)>() {
        Ok((Value::String(s1), Value::String(s2)))
            if s1.as_str() == "john_value" && s2.as_str() == "jane_value" =>
            true,
        _ => false,
    },
    _ => false,
});

const MAP_WITH_ARRAYS: &str = r#"
{
  let m = {"nums" => [1, 2, 3], "strs" => ["a", "b", "c"]};
  m{"nums"}
}
"#;

run!(map_with_arrays, MAP_WITH_ARRAYS, |v: Result<&Value>| match v {
    Ok(Value::Array(arr)) => {
        arr.len() == 3
            && arr.get(0).map(|v| *v == Value::I64(1)).unwrap_or(false)
            && arr.get(1).map(|v| *v == Value::I64(2)).unwrap_or(false)
            && arr.get(2).map(|v| *v == Value::I64(3)).unwrap_or(false)
    }
    _ => false,
});
