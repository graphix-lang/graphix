use super::init;
use crate::run;
use anyhow::{bail, Result};
use arcstr::ArcStr;
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use std::time::Duration;
use tokio::sync::mpsc;

#[tokio::test(flavor = "current_thread")]
async fn bind_ref_arith() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(10);
    let ctx = init(tx).await?;
    let gx = ctx.rt;
    let e = r#"
{
  let v = (((1 + 1) * 2) / 2) - 1;
  v
}
"#;
    let e = gx.compile(ArcStr::from(e)).await?;
    let eid = e.exprs[0].id;
    match rx.recv().await {
        None => bail!("runtime died"),
        Some(mut ev) => {
            for e in ev.drain(..) {
                match e {
                    GXEvent::Env(_) => (),
                    GXEvent::Updated(id, v) => {
                        assert_eq!(id, eid);
                        assert_eq!(v, Value::I64(1))
                    }
                }
            }
        }
    }
    Ok(())
}

const MOD0: &str = r#"
{
  let v = 8;
  v % 10
}
"#;

run!(mod0, MOD0, |v: Result<&Value>| match v {
    Ok(&Value::I64(8)) => true,
    _ => false,
});

const SCOPE: &str = r#"
{
  let v = (((1 + 1) * 2) / 2) - 1;
  let x = {
     let v = 42;
     v * 2
  };
  v + x
}
"#;

run!(scope, SCOPE, |v: Result<&Value>| match v {
    Ok(&Value::I64(85)) => true,
    _ => false,
});

const CORE_USE: &str = r#"
{
  let v = (((1 + 1) * 2) / 2) - 1;
  let x = {
     let v = 42;
     once(v * 2)
  };
  [v, x]
}
"#;

run!(core_use, CORE_USE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if &**a == &[Value::I64(1), Value::I64(84)] => true,
    _ => false,
});

const NAME_MODPATH: &str = r#"
{
  let z = "baz";
  str::join(#sep:", ", "foo", "bar", z)
}
"#;

run!(name_modpath, NAME_MODPATH, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "foo, bar, baz",
    _ => false,
});

const LAMBDA: &str = r#"
{
  let y = 10;
  let f = |x| x + y;
  f(10)
}
"#;

run!(lambda, LAMBDA, |v: Result<&Value>| match v {
    Ok(Value::I64(20)) => true,
    _ => false,
});

const STATIC_SCOPE: &str = r#"
{
  let f = |x| x + y;
  let y = 10;
  f(10)
}
"#;

run!(static_scope, STATIC_SCOPE, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const UNDEFINED: &str = r#"
{
  let y = 10;
  let z = x + y;
  let x = 10;
  z
}
"#;

run!(undefined, UNDEFINED, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const FIRST_CLASS_LAMBDAS: &str = r#"
{
  let doit = |x: Number| x + 1;
  let g = |f: fn<'a: Number>('a) -> 'a, y| f(y) + 1;
  g(doit, 1)
}
"#;

run!(first_class_lambdas, FIRST_CLASS_LAMBDAS, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

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

const SIMPLE_TYPECHECK: &str = r#"
{
  "foo" + 1
}
"#;

run!(simple_typecheck, SIMPLE_TYPECHECK, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const FUNCTION_TYPES: &str = r#"
{
  let f = |x: Number, y: Number| -> string "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(function_types, FUNCTION_TYPES, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const PARTIAL_FUNCTION_TYPES: &str = r#"
{
  let f = |x: Number, y| "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(partial_function_types, PARTIAL_FUNCTION_TYPES, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const FUNCTION_RTYPE: &str = r#"
{
  let f = |x, y| -> Number "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(function_rtype, FUNCTION_RTYPE, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const INFERRED_RTYPE: &str = r#"
{
  let f = |x, y| "x is [x] and y is [y]";
  let v = f("foo", 3);
  let g = |x| x + 1;
  g(v)
}
"#;

run!(inferred_rtype, INFERRED_RTYPE, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const LAMBDA_CONSTRAINT: &str = r#"
{
  let f = |f: fn(string, string) -> string, a| f("foo", a);
  f(|x, y: Number| "[x] and [y]", "foo")
}
"#;

run!(lambda_constraint, LAMBDA_CONSTRAINT, |v: Result<&Value>| match v {
    Err(_) => true,
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

const LABELED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#foo: 0)
}
"#;

run!(labeled_args, LABELED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const REQUIRED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  f(#bar: 0)
}
"#;

run!(required_args, REQUIRED_ARGS, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const MIXED_ARGS: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42, baz| foo + bar + baz;
  f(#foo: 0, 0)
}
"#;

run!(mixed_args, MIXED_ARGS, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const ARG_SUBTYPING: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  let g = |f: fn(#foo: Number) -> Number| f(#foo: 3);
  g(f)
}
"#;

run!(arg_subtyping, ARG_SUBTYPING, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
});

const ARG_NAME_SHORT: &str = r#"
{
  let f = |#foo: Number, #bar: Number = 42| foo + bar;
  let foo = 3;
  f(#foo)
}
"#;

run!(arg_name_short, ARG_NAME_SHORT, |v: Result<&Value>| match v {
    Ok(Value::I64(45)) => true,
    _ => false,
});

const EXPLICIT_TYPE_VARS0: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  f("foo", "bar")
}
"#;

run!(explicit_type_vars0, EXPLICIT_TYPE_VARS0, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const EXPLICIT_TYPE_VARS1: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  f(u32:1, i64:2)
}
"#;

run!(explicit_type_vars1, EXPLICIT_TYPE_VARS1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const EXPLICIT_TYPE_VARS2: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  select f(1, 1) {
    i64 as t => t
  }
}
"#;

run!(explicit_type_vars2, EXPLICIT_TYPE_VARS2, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const EXPLICIT_TYPE_VARS3: &str = r#"
{
  let f = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ['a, 'b] x + y;
  select f(u32:1, u64:1) {
    [u32, u64] as t => t
  }
}
"#;

run!(explicit_type_vars3, EXPLICIT_TYPE_VARS3, |v: Result<&Value>| match v {
    Ok(Value::U32(2) | Value::U64(2)) => true,
    _ => false,
});

const TYPED_ARRAYS0: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [1, 2, 3])
}
"#;

run!(typed_arrays0, TYPED_ARRAYS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &**a {
        [Value::Array(a0), Value::Array(a1)] => match (&**a0, &**a1) {
            (
                [Value::I64(1), Value::I64(2), Value::I64(3)],
                [Value::I64(1), Value::I64(2), Value::I64(3)],
            ) => true,
            _ => false,
        },
        _ => false,
    },
    _ => false,
});

const TYPED_ARRAYS1: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [u32:1, 2, 3])
}
"#;

run!(typed_arrays1, TYPED_ARRAYS1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

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

const TUPLES0: &str = r#"
{
  let t: (string, Number, Number) = ("foo", 42, 23.5);
  t
}
"#;

run!(tuples0, TUPLES0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s), Value::I64(42), Value::F64(23.5)] => &*s == "foo",
        _ => false,
    },
    _ => false,
});

const TUPLES1: &str = r#"
{
  let t: (string, Number, Number) = ("foo", 42, 23.5);
  let (_, y, z) = t;
  y + z
}
"#;

run!(tuples1, TUPLES1, |v: Result<&Value>| match v {
    Ok(Value::F64(65.5)) => true,
    _ => false,
});

const TUPLES2: &str = r#"
{
  let t = ("foo", 42, 23.5);
  select t {
    ("foo", x, y) => x + y,
    _ => never()
  }
}
"#;

run!(tuples2, TUPLES2, |v: Result<&Value>| match v {
    Ok(Value::F64(65.5)) => true,
    _ => false,
});

const STRUCTS0: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  x
}
"#;

run!(structs0, STRUCTS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if a.len() == 3 => match &a[..] {
        [Value::Array(f0), Value::Array(f1), Value::Array(f2)]
            if f0.len() == 2 && f1.len() == 2 && f2.len() == 2 =>
        {
            let f0 = match &f0[..] {
                [Value::String(n), Value::I64(42)] if n == "bar" => true,
                _ => false,
            };
            let f1 = match &f1[..] {
                [Value::String(n), Value::F64(84.0)] if n == "baz" => true,
                _ => false,
            };
            let f2 = match &f2[..] {
                [Value::String(n), Value::String(s)] if n == "foo" && s == "bar" => true,
                _ => false,
            };
            f0 && f1 && f2
        }
        _ => false,
    },
    _ => false,
});

const BINDSTRUCT: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let { foo: _, bar, baz } = x;
  bar + baz
}
"#;

run!(bindstruct, BINDSTRUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(126.0)) => true,
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

const STRUCTACCESSOR: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  x.foo
}
"#;

run!(structaccessor, STRUCTACCESSOR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => s == "bar",
    _ => false,
});

const TUPLEACCESSOR: &str = r#"
{
  let x = ( "bar", 42, 84.0 );
  x.1
}
"#;

run!(tupleaccessor, TUPLEACCESSOR, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const STRUCTWITH0: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let x = { x with foo: 1 };
  x.foo
}
"#;

run!(structwith0, STRUCTWITH0, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const STRUCTWITH1: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let x = { x with bar: 1 };
  x.bar + x.baz
}
"#;

run!(structwith1, STRUCTWITH1, |v: Result<&Value>| match v {
    Ok(Value::F64(85.0)) => true,
    _ => false,
});

const STRUCTWITH2: &str = r#"
{
  let selected = { x: 0, y: 0 };
  let y = 1;
  { selected with y }
}
"#;

run!(structwith2, STRUCTWITH2, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[(ArcStr, i64); 2]>() {
        Ok([(s0, 0), (s1, 1)]) if &*s0 == "x" && &*s1 == "y" => true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH3: &str = r#"
{
  let selected = { x: 0, y: 0 };
  { selected with y: selected.y + 1 }
}
"#;

run!(structwith3, STRUCTWITH3, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[(ArcStr, i64); 2]>() {
        Ok([(s0, 0), (s1, 1)]) if &*s0 == "x" && &*s1 == "y" => true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH4: &str = r#"
{
    let selected = { x: 0, y: 0 };
    let handle = |e: [`Up, `Down, `Left, `Right]| -> `Stop select e {
        e@ `Left => {
            selected <- e ~ { selected with x: selected.x - 1 };
            `Stop
        },
        e@ `Right => {
            selected <- e ~ { selected with x: selected.x + 1 };
            `Stop
        },
        e@ `Down => {
            selected <- e ~ { selected with y: selected.y + 1 };
            `Stop
        },
        e@ `Up => {
            selected <- e ~ { selected with y: selected.y - 1 };
            `Stop
        }
    };
    handle(array::iter([`Up, `Down, `Left, `Right]));
    (array::group(selected, |n, _| n == 5))[1..]
}
"#;

run!(structwith4, STRUCTWITH4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[[(ArcStr, i64); 2]; 4]>() {
        Ok(
            [[(f00, 0), (f01, -1)], [(f10, 0), (f11, 0)], [(f20, -1), (f21, 0)], [(f30, 0), (f31, 0)]],
        ) if f00 == "x"
            && f01 == "y"
            && f10 == f00
            && f20 == f00
            && f30 == f00
            && f11 == f01
            && f21 == f01
            && f31 == f01 =>
            true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH5: &str = r#"
{
    let selected = { x: 0, y: 0 };
    let handle = |e: [`Up]| -> `Stop select e {
        e@ `Up => {
            selected <- e ~ { selected with y: selected.y - 1 };
            `Stop
        }
    };
    handle(array::iter([`Up]));
    (array::group(selected, |n, _| n == 2))[1..]
}
"#;

run!(structwith5, STRUCTWITH5, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[[(ArcStr, i64); 2]; 1]>() {
        Ok([[(f00, 0), (f01, -1)]]) if f00 == "x" && f01 == "y" => true,
        _ => false,
    },
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

const LAMBDAMATCH0: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}: T| bar + bar;
  f(x)
}
"#;

run!(lambdamatch0, LAMBDAMATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
});

const LAMBDAMATCH1: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{bar, ..}| bar + bar;
  f(x)
}
"#;

run!(lambdamatch1, LAMBDAMATCH1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const LAMBDAMATCH2: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f(x)
}
"#;

run!(lambdamatch2, LAMBDAMATCH2, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
});

const LAMBDAMATCH3: &str = r#"
{
  let f = |{foo: _, bar, baz: _}| bar + bar;
  f({bar: 42, baz: 1})
}
"#;

run!(lambdamatch3, LAMBDAMATCH3, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const LAMBDAMATCH4: &str = r#"
{
  let f = |(i, _)| i * 2;
  f((42, "foo"))
}
"#;

run!(lambdamatch4, LAMBDAMATCH4, |v: Result<&Value>| match v {
    Ok(Value::I64(84)) => true,
    _ => false,
});

const LAMBDAMATCH5: &str = r#"
{
  let f = |(i, _)| i * 2;
  f("foo")
}
"#;

run!(lambdamatch5, LAMBDAMATCH5, |v: Result<&Value>| match v {
    Err(_) => true,
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

const ANY0: &str = r#"
{
  let x = 1;
  let y = x + 1;
  let z = y + 1;
  any(z, x, y)
}
"#;

run!(any0, ANY0, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

const ANY1: &str = r#"
{
  let x = 1;
  let y = "[x] + 1";
  let z = [y, y];
  any(z, x, y)
}
"#;

run!(any1, ANY1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s0), Value::String(s1)] => {
            &**s0 == "1 + 1" && s0 == s1
        }
        _ => false,
    },
    _ => false,
});

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

const LATE_BINDING0: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  let u: T = { foo: "hello foo", bar: 42, f: |#c: i64 = 1, #y: i64, #x: i64| x - y + c };
  let f = t.f;
  f(#y: 3, #x: 4)
}
"#;

run!(late_binding0, LATE_BINDING0, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
});

const LATE_BINDING1: &str = r#"
{
  type F = fn(#x: i64, #y: i64) -> i64;
  type T = { foo: string, bar: i64, f: F };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  let u: T = { foo: "hello foo", bar: 42, f: |#c: i64 = 1, #y: i64, #x: i64| (x - y) + c };
  let f: F = select array::iter([0, 1]) {
    0 => t.f,
    1 => u.f,
    _ => never()
  };
  array::group(f(#y: 3, #x: 4), |n, _| n == 2)
}
"#;

run!(late_binding1, LATE_BINDING1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::I64(1), Value::I64(2)] => true,
        _ => false,
    },
    _ => false,
});

const LATE_BINDING2: &str = r#"
{
  type T = { foo: string, bar: i64, f: fn(#x: i64, #y: i64) -> i64 };
  let t: T = { foo: "hello world", bar: 3, f: |#x: i64, #y: i64| x - y };
  (t.f)(#y: 3, #x: 4)
}
"#;

run!(late_binding2, LATE_BINDING2, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
});

const LATE_BINDING3: &str = r#"
{
    let f: fn(i64) -> i64 = never();
    let res = f(1);
    f <- |i: i64| i + 1;
    res
}
"#;

run!(late_binding3, LATE_BINDING3, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const LATE_BINDING4: &str = r#"
{
    let f = |#foo: i64 = 0, #bar: i64 = 1, baz| (foo - bar) + baz;
    let g = |#bar: i64 = 1, #foo: i64 = 0, baz| (foo - bar) + baz;
    let h = |#bar: i64 = 1, #zam: i64 = 55, #foo: i64 = 0, baz| (foo - bar) + baz + zam;
    let fs = [f, g, h];
    let f: fn(i64) -> i64 = never();
    f <- array::iter(fs);
    array::group(f(1), |n, _| n == 3)
}
"#;

run!(late_binding4, LATE_BINDING4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[i64; 3]>() {
        Ok([0, 0, 55]) => true,
        Ok(_) | Err(_) => false,
    },
    _ => false,
});

const RECTYPES0: &str = r#"
{
  type List = [
    `Cons(Any, List),
    `Nil
  ];
  let l: List = `Cons(42, `Cons(3, `Nil));
  l
}
"#;

run!(rectypes0, RECTYPES0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s), Value::I64(42), Value::Array(a)] if &**s == "Cons" =>
            match &a[..] {
                [Value::String(s0), Value::I64(3), Value::String(s1)]
                    if &**s0 == "Cons" && s1 == "Nil" =>
                    true,
                _ => false,
            },
        _ => false,
    },
    _ => false,
});

const RECTYPES1: &str = r#"
{
  type List<'a> = [
    `Cons('a, List<'a>),
    `Nil
  ];
  let l: List<Any> = `Cons(42, `Cons(3, `Nil));
  l
}
"#;

run!(rectypes1, RECTYPES1, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s), Value::I64(42), Value::Array(a)] if &**s == "Cons" =>
            match &a[..] {
                [Value::String(s0), Value::I64(3), Value::String(s1)]
                    if &**s0 == "Cons" && s1 == "Nil" =>
                    true,
                _ => false,
            },
        _ => false,
    },
    _ => false,
});

const RECTYPES2: &str = r#"
{
  type List<'a> = [
    `Cons('a, List<'a>),
    `Nil
  ];
  let l: List<string> = `Cons(42, `Cons(3, `Nil));
  l
}
"#;

run!(rectypes2, RECTYPES2, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const TYPEDEF_TVAR_ERR: &str = r#"
{
  type T<'a, 'b> = { foo: 'a, bar: 'b, baz: 'c };
  0
}
"#;

run!(typedef_tvar_err, TYPEDEF_TVAR_ERR, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const TYPEDEF_TVAR_OK: &str = r#"
{
  type T<'a, 'b> = { foo: 'a, bar: 'b, f: fn('a, 'b, 'c) -> 'a };
  0
}
"#;

run!(typedef_tvar_ok, TYPEDEF_TVAR_OK, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

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

const NESTED_OPTIONAL0: &str = r#"
{
    type T = { foo: i64, bar: i64 };
    let f = |#foo: i64 = 42, #bar: i64 = 42| -> T { foo, bar };
    type U = { f: T, baz: i64 };
    let g = |#f: T = f(), baz: i64| -> U { f, baz };

    let r = g(42);
    r.baz
}
"#;

run!(nested_optional0, NESTED_OPTIONAL0, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});

const DYNAMIC_MODULE0: &str = r#"
{
    let source = "
        let add = |x| x + 1;
        let sub = |x| x - 1;
        let cfg = \[1, 2, 3, 4, 5\];
        let hidden = 42
    ";
    net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn(i64) -> i64;
            val sub: fn(i64) -> i64;
            val cfg: Array<i64>
        };
        source cast<string>(net::subscribe("/local/foo")$)$
    };
    select status {
        error as e => never(dbg(e)),
        null as _ => foo::add(foo::cfg[0]?)
    }
}
"#;

run!(dynamic_module0, DYNAMIC_MODULE0, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const DYNAMIC_MODULE1: &str = r#"
{
    let source = "
        let add = |x| x + 1.;
        let sub = |x| x - 1;
        let cfg = \[1, 2, 3, 4, 5\];
        let hidden = 42
    ";
    net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn(i64) -> i64;
            val sub: fn(i64) -> i64;
            val cfg: Array<i64>
        };
        source cast<string>(net::subscribe("/local/foo"))
    };
    select status {
        error as e => dbg(e),
        null as _ => foo::add(foo::cfg[0]?)
    }
}
"#;

run!(dynamic_module1, DYNAMIC_MODULE1, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const DYNAMIC_MODULE2: &str = r#"
{
    let source = "let add = 'a: Number |x: 'a| -> 'a x + x";
    net::publish("/local/foo", source)?;
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig {
            val add: fn(i64) -> i64
        };
        source cast<string>(net::subscribe("/local/foo"))
    };
    select status {
        error as e => dbg(e),
        null as _ => foo::add(2)
    }
}
"#;

run!(dynamic_module2, DYNAMIC_MODULE2, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
});

const DYNAMIC_MODULE3: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) }
    ";
    net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core];
        sig {
            val foo: string;
            val bar: string
        };
        source cast<string>(net::subscribe("/local/test"))
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module3, DYNAMIC_MODULE3, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s == "hello world" => true,
    _ => false,
});

const DYNAMIC_MODULE4: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) }
    ";
    net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core];
        sig {
            val foo: string;
            val bar: string;
            val baz: string
        };
        source cast<string>(net::subscribe("/local/test"))
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module4, DYNAMIC_MODULE4, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const DYNAMIC_MODULE5: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        net::subscribe(\"/local/test\")
    ";
    net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core];
        sig {
            val foo: string;
            val bar: string
        };
        source cast<string>(net::subscribe("/local/test"))
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module5, DYNAMIC_MODULE5, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const DYNAMIC_MODULE6: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never(); select foo { x => bar <- dbg(x) };
        net::subscribe(\"/local/test\")
    ";
    net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox blacklist [net::publish];
        sig {
            val foo: string;
            val bar: string
        };
        source cast<string>(net::subscribe("/local/test"))
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module6, DYNAMIC_MODULE6, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s == "hello world" => true,
    _ => false,
});

const DYNAMIC_MODULE7: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        net::publish(\"/local/test\", 42)
    ";
    net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox blacklist [net::publish];
        sig {
            val foo: string;
            val bar: string
        };
        source cast<string>(net::subscribe("/local/test"))
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module7, DYNAMIC_MODULE7, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const DYNAMIC_MODULE8: &str = r#"
{
    let source = "
        let foo = never();
        let bar = never();
        select foo { x => bar <- dbg(x) };
        net::subscribe(\"/local/test\")
    ";
    net::publish("/local/test", source)?;
    let status = mod test dynamic {
        sandbox whitelist [core, net::subscribe];
        sig {
            val foo: string;
            val bar: string
        };
        source cast<string>(net::subscribe("/local/test"))
    };
    select status {
        error as e => dbg(e),
        null as _ => {
            test::foo <- dbg("hello world");
            test::bar
        }
    }
}
"#;

run!(dynamic_module8, DYNAMIC_MODULE8, |v: Result<&Value>| match v {
    Ok(Value::String(s)) if s == "hello world" => true,
    _ => false,
});

const RECURSIVE_LAMBDA0: &str = r#"
{
    let rec f = |x: i64| select x { x if x < 10 => f(x + 1), x => x };
    f(0)
}
"#;

run!(recursive_lambda0, RECURSIVE_LAMBDA0, |v: Result<&Value>| match v {
    Ok(Value::I64(10)) => true,
    _ => false,
});

use chrono::prelude::*;

const DATETIME_ARITH00: &str = r#"
{
    let x: datetime = datetime:"2024-11-05T00:00:00Z" + duration:3600.s;
    x
}
"#;

run!(datetime_arith00, DATETIME_ARITH00, |v: Result<&Value>| match v {
    Ok(Value::DateTime(dt))
        if **dt == "2024-11-05T01:00:00Z".parse::<DateTime<Utc>>().unwrap() =>
        true,
    _ => false,
});

const DATETIME_ARITH01: &str = r#"
{
    let x: datetime = datetime:"2024-11-05T00:00:00Z" - duration:3600.s;
    x
}
"#;

run!(datetime_arith01, DATETIME_ARITH01, |v: Result<&Value>| match v {
    Ok(Value::DateTime(dt))
        if **dt == "2024-11-04T23:00:00Z".parse::<DateTime<Utc>>().unwrap() =>
        true,
    _ => false,
});

const DATETIME_ARITH02: &str = r#"
{
    let x: duration = u32:2 * duration:3600.s;
    x
}
"#;

run!(datetime_arith02, DATETIME_ARITH02, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(7200) => true,
    _ => false,
});

const DATETIME_ARITH03: &str = r#"
{
    let x: duration = duration:3600.s * u32:2;
    x
}
"#;

run!(datetime_arith03, DATETIME_ARITH03, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(7200) => true,
    _ => false,
});

const DATETIME_ARITH04: &str = r#"
{
    let x: duration = duration:3600.s / u32:2;
    x
}
"#;

run!(datetime_arith04, DATETIME_ARITH04, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
});

const DATETIME_ARITH05: &str = r#"
{
    let x: duration = duration:3600.s - duration:1800.s;
    x
}
"#;

run!(datetime_arith05, DATETIME_ARITH05, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
});

const DATETIME_ARITH06: &str = r#"
{
    let x: duration = duration:0.s + duration:1800.s;
    x
}
"#;

run!(datetime_arith06, DATETIME_ARITH06, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
});

const DATETIME_ARITH07: &str = r#"
{
    let x: duration = duration:2.s * duration:1800.s;
    x
}
"#;

run!(datetime_arith07, DATETIME_ARITH07, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH08: &str = r#"
{
    let x: duration = duration:2.s / duration:1800.s;
    x
}
"#;

run!(datetime_arith08, DATETIME_ARITH08, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH09: &str = r#"
{
    let x: duration = duration:2.s % duration:1800.s;
    x
}
"#;

run!(datetime_arith09, DATETIME_ARITH09, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH10: &str = r#"
{
    let x: duration = duration:2.s + u32:1;
    x
}
"#;

run!(datetime_arith10, DATETIME_ARITH10, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH11: &str = r#"
{
    let x: duration = duration:2.s - u32:1;
    x
}
"#;

run!(datetime_arith11, DATETIME_ARITH11, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH12: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" - 1;
    x
}
"#;

run!(datetime_arith12, DATETIME_ARITH12, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH13: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" + 1;
    x
}
"#;

run!(datetime_arith13, DATETIME_ARITH13, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH14: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" * 2;
    x
}
"#;

run!(datetime_arith14, DATETIME_ARITH14, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH15: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" / 2;
    x
}
"#;

run!(datetime_arith15, DATETIME_ARITH15, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH16: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" % 2;
    x
}
"#;

run!(datetime_arith16, DATETIME_ARITH16, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH17: &str = r#"
{
    let x: datetime = duration:1.0s - datetime:"2024-11-05T00:00:00Z";
    x
}
"#;

run!(datetime_arith17, DATETIME_ARITH17, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH18: &str = r#"
{
    let errors = never();
    try
        let you_have_been_in_suspention_for = duration:9999999999999.s * 99999999999999;
        any(you_have_been_in_suspention_for, errors)
    catch(e: `ArithError(string)) => errors <- e
}
"#;

run!(datetime_arith18, DATETIME_ARITH18, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});

const CATCH0: &str = r#"
try 2 + 2
catch(e) => select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]")
}
"#;

run!(catch0, CATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
});

const CATCH1: &str = r#"
try
    let a = [1, 2, 3];
    a[0]? + a[1]?
catch(e) => select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]")
}
"#;

run!(catch1, CATCH1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const CATCH2: &str = r#"
try 2 + 2
catch(e) => select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]"),
    `ArrayIndexError(s) => println("array index error [s]")
}
"#;

run!(catch2, CATCH2, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const CATCH3: &str = r#"
{
    let f = |x| x / x;
    let res = never();
    try any(f(0), res)
    catch(e) => select (e.0).error {
        `ArithError(s) => res <- s
    }
}
"#;

run!(catch3, CATCH3, |v: Result<&Value>| match v {
    Ok(Value::String(_)) => true,
    _ => false,
});

const CATCH4: &str = r#"
{
    let a = [0, 1, 2, 3, 4, 5];
    let err0: Error<ErrChain<`ArrayIndexError(string)>> = never();
    let err1: Error<ErrChain<`ArithError(string)>> = never();
    try
       try
           a[5]? / a[0]?;
           a[6]?
       catch(e) => select (e.0).error {
          `ArithError(_) => err1 <- e,
          _ => e?
       }
    catch(e) => err0 <- e;
    [err0, err1]
}
"#;

run!(catch4, CATCH4, |v: Result<&Value>| match v
    .and_then(|v| v.clone().cast_to::<[Value; 2]>())
{
    Ok([Value::Error(_), Value::Error(_)]) => true,
    _ => false,
});

const CATCH5: &str = r#"
{
    let f = |x| x / x;
    let res = never();
    try any(f(0), res)
    catch(e) => select (e.0).error {
        `ArithError(s) => res <- s,
        `ArrayIndexError(s) => res <- s
    }
}
"#;

run!(catch5, CATCH5, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const OR_NEVER: &str = r#"
{
    let a = [error("foo"), 42];
    array::iter(a)$
}
"#;

run!(or_never, OR_NEVER, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
});
