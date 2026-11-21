// Tests for map literals and operations

use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
};
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

run!(map_ref0, MAP_REF0, is(Value::I64(2)));

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

// A missing key is a `MapKeyError`.
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

// A lookup is held to the comparison rule: a key no key of the map can
// equal is refused, as `1 == "a"` is.
run!(map_ref_of_a_key_of_another_type_is_refused, MAP_REF_WRONG_TYPE, refused("cannot compare i64 with string"); FuseExpect::None);

// A lookup is held to one rule whatever calls it: the key's type and the
// map's key type must compare, at `m{k}` and at map::get, get_or and
// remove alike, and through a generic function at each call.
run!(
    map_get_of_a_key_of_another_type_is_refused,
    r#"{ let m = {"a" => 1}; map::get(m, 1) }"#,
    refused("cannot compare string with i64");
    FuseExpect::None
);
run!(
    map_get_or_of_a_key_of_another_type_is_refused,
    r#"{ let m = {"a" => 1}; map::get_or(m, 1, 0) }"#,
    refused("cannot compare string with i64");
    FuseExpect::None
);
run!(
    map_remove_of_a_key_of_another_type_is_refused,
    r#"{ let m = {"a" => 1}; map::remove(m, 1) }"#,
    refused("cannot compare string with i64");
    FuseExpect::None
);
run!(
    map_get_through_a_generic_function_is_judged_at_the_call,
    r#"{ let g = |m, k| map::get(m, k); g({"a" => 1}, 1) }"#,
    refused("cannot compare string with i64");
    FuseExpect::None
);
run!(
    map_get_through_a_generic_function_finds_its_key,
    r#"{ let g = |m, k| map::get(m, k); g({"a" => 1}, "a") }"#,
    prints("i64:1")
);
run!(
    map_ref_through_a_generic_function_is_judged_at_the_call,
    r#"{ let g = |m, k| m{k}; g({"a" => 1}, 1) }"#,
    refused("cannot compare string with i64");
    FuseExpect::None
);
run!(
    map_ref_through_a_generic_function_finds_its_key,
    r#"{ let g = |m, k| m{k}; g({"a" => 1}, "a")$ }"#,
    prints("i64:1")
);

const MAP_NESTED: &str = r#"
{
  let m = {"outer" => {"inner" => 42}};
  m{"outer"}
}
"#;

// A nested map literal folds to one constant; `m{key}` reads it.
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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A key narrower than the map's key type reads; the map's key type holds
// the key, not the other way around.
run!(
    map_ref_narrower_key,
    r#"{
    let m = {`Red => "r", `Green => "g"};
    m{`Red}$
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "r")
);

// A map literal's entries are a fork point like a tuple's elements.
run!(
    map_literal_forks,
    r#"{
    let f = |x: i64| x * 2;
    let a = 3;
    let m = #[parallel] {"x" => f(a), "y" => f(a + 1)};
    m{"x"}$ + m{"y"}$
}"#,
    is(Value::I64(14))
);
