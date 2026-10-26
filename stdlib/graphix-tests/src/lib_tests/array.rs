use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::{
    run,
    testing::{Collect, FuseExpect, Mode, collect, is, prints, refused},
};
use netidx::subscriber::Value;
use std::time::Duration;

const ARRAY_MAP0: &str = r#"
{
  let a = [1, 2, 3, 4];
  array::map(a, |x| x > 3)
}
"#;

run!(array_map0, ARRAY_MAP0, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [
                Value::Bool(false),
                Value::Bool(false),
                Value::Bool(false),
                Value::Bool(true),
            ] => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_MAP1: &str = r#"
{
  let a = [1, 2];
  let b = [1, 2];
  array::map(a, |x| array::map(b, |y| x + y))
}
"#;

run!(array_map1, ARRAY_MAP1, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 2]; 2]>() {
            Ok([[2, 3], [3, 4]]) => true,
            _ => false,
        },
        Err(_) => false,
    }
}; FuseExpect::Jit);

// A nested map with a constant callback body over a loop-invariant
// captured source still fuses.
const ARRAY_MAP_NESTED_CONST: &str = r#"
{
  let a = [1, 2];
  let b = [3, 4];
  #[native] array::map(a, |x| array::map(b, |y| 7))
}
"#;

run!(array_map_nested_const, ARRAY_MAP_NESTED_CONST, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 2]; 2]>() {
            Ok([[7, 7], [7, 7]]) => true,
            _ => false,
        },
        Err(_) => false,
    }
}; FuseExpect::Jit);

// Composite-output `array::map`: `Array<(i64, i64)>`.
const ARRAY_MAP_TUPLE: &str = r#"
{
  let a = [1, 2, 3];
  array::map(a, |x| (x, x * 2))
}
"#;

run!(array_map_tuple, ARRAY_MAP_TUPLE, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(1, 2), (2, 4), (3, 6)]) => true,
            _ => false,
        },
        Err(_) => false,
    }
});

// A composite element with a `|(k, v)|` destructure callback.
const ARRAY_MAP_DESTRUCTURE: &str = r#"
{
  let a = [(1, 2), (3, 4)];
  array::map(a, |(k, v)| k + v)
}
"#;

run!(array_map_destructure, ARRAY_MAP_DESTRUCTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => &a[..] == [Value::I64(3), Value::I64(7)],
        _ => false,
    }
});

// `array::fold` with a `|acc, (k, v)|` destructure over a composite
// element.
const ARRAY_FOLD_DESTRUCTURE: &str = r#"
{
  let a = [(1, 2), (3, 4)];
  array::fold(a, 0, |acc, (k, v)| acc + k + v)
}
"#;

run!(array_fold_destructure, ARRAY_FOLD_DESTRUCTURE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
});

// `array::filter` with a `|(k, v)|` destructure keeps the original
// composite elements.
const ARRAY_FILTER_DESTRUCTURE: &str = r#"
{
  let a = [(1, 2), (3, 4), (5, 6)];
  array::filter(a, |(k, v)| v > 3)
}
"#;

run!(array_filter_destructure, ARRAY_FILTER_DESTRUCTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::Array(p0), Value::Array(p1)] => {
                &p0[..] == [Value::I64(3), Value::I64(4)]
                    && &p1[..] == [Value::I64(5), Value::I64(6)]
            }
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_MAP2: &str = r#"
  array::map([1, 2], |x| str::len(x))
"#;

run!(array_map2, ARRAY_MAP2, refused("string does not contain"); FuseExpect::None);

const ARRAY_FILTER: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter(a, |x| x > 3)
}
"#;

run!(array_filter, ARRAY_FILTER, prints("[i64:4, i64:5, i64:6, i64:7, i64:8]"));

const ARRAY_FLAT_MAP: &str = r#"
{
  let a = [1, 2];
  array::flat_map(a, |x| [x, x + 1])
}
"#;

run!(array_flat_map, ARRAY_FLAT_MAP, prints("[i64:1, i64:2, i64:2, i64:3]"));

const ARRAY_FLAT_MAP_DESTRUCTURE: &str = r#"
{
  let a = [(1, 10), (2, 20)];
  array::flat_map(a, |(k, v)| [k, v])
}
"#;

run!(array_flat_map_destructure, ARRAY_FLAT_MAP_DESTRUCTURE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => matches!(
            &a[..],
            [Value::I64(1), Value::I64(10), Value::I64(2), Value::I64(20)]
        ),
        _ => false,
    }
});

const ARRAY_FILTER_MAP: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter_map(a, |x: i64| -> [i64, null] select x > 5 {
    true => x + 1,
    false => x ~ null
  })
}
"#;

// `x ~ null` is a sample, so this callback node-walks.
run!(array_filter_map, ARRAY_FILTER_MAP, prints("[i64:7, i64:8, i64:9]"));

// Scalar `array::filter_map` whose body is an option-typed `select`.
const ARRAY_FILTER_MAP_SCALAR: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  array::filter_map(a, |x: i64| -> [i64, null] select x > 5 {
    true => x + 1,
    false => null
  })
}
"#;

run!(array_filter_map_scalar, ARRAY_FILTER_MAP_SCALAR, prints("[i64:7, i64:8, i64:9]"));

const ARRAY_FIND: &str = r#"
{
  type T = (string, i64);
  let a: Array<T> = [("foo", 1), ("bar", 2), ("baz", 3)];
  array::find(a, |(k, _): T| k == "bar")
}
"#;

// A composite `(string, i64)` element with destructure; `find` returns
// the matched element as `Nullable<(string, i64)>`.
run!(array_find, ARRAY_FIND, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s), Value::I64(2)] => &**s == "bar",
            _ => false,
        },
        _ => false,
    }
});

// Scalar-element `array::find` lowers to the array-find loop (result
// `Nullable<i64>`), which scans every slot and keeps the first match.
const ARRAY_FIND_SCALAR: &str = r#"
{
  let a = [1, 2, 3, 4, 5];
  #[native] array::find(a, |x| x > 3)
}
"#;

run!(array_find_scalar, ARRAY_FIND_SCALAR, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(4)) => true,
        _ => false,
    }
});

// No element matches: `null`.
const ARRAY_FIND_SCALAR_NONE: &str = r#"
{
  let a = [1, 2, 3];
  array::find(a, |x| x > 10)
}
"#;

run!(array_find_scalar_none, ARRAY_FIND_SCALAR_NONE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

// A composite element and composite output without a string leaf.
const ARRAY_FIND_COMPOSITE: &str = r#"
{
  let a = [(1, 10), (2, 20), (3, 30)];
  array::find(a, |(k, _)| k == 2)
}
"#;

run!(array_find_composite, ARRAY_FIND_COMPOSITE, prints("[i64:2, i64:20]"));

// No composite element matches: every element is fetched and dropped.
const ARRAY_FIND_COMPOSITE_NONE: &str = r#"
{
  let a = [(1, 10), (2, 20), (3, 30)];
  array::find(a, |(k, _)| k == 99)
}
"#;

run!(array_find_composite_none, ARRAY_FIND_COMPOSITE_NONE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

const ARRAY_FIND_MAP: &str = r#"
{
  type T = (string, i64);
  let a: Array<T> = [("foo", 1), ("bar", 2), ("baz", 3)];
  array::find_map(a, |(k, v): T| select k == "bar" {
    true => v,
    false => v ~ null
  })
}
"#;

// A composite `(string, i64)` element, a destructure and a `v ~ null`
// arm through the find-map loop.
run!(array_find_map, ARRAY_FIND_MAP, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(2)) => true,
        _ => false,
    }
});

// An all-prim composite element through the find-map loop.
const ARRAY_FIND_MAP_PRIM: &str = r#"
{
  let a = [(1, 10), (2, 20), (3, 30)];
  array::find_map(a, |(k, v)| select k == 2 {
    true => v,
    false => null
  })
}
"#;

run!(array_find_map_prim, ARRAY_FIND_MAP_PRIM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(20)))
});

const ARRAY_ITER: &str = r#"
   filter(array::iter([1, 2, 3, 4]), |x| x == 4)
"#;

run!(array_iter, ARRAY_ITER, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(4)) => true,
        _ => false,
    }
}; FuseExpect::None);

const ARRAY_ITERQ: &str = r#"
{
   let a = [1, 2, 3, 4];
   a <- [5, 6, 7, 8];
   let clock: Any = once(null);
   let v = array::iterq(#clock, a);
   clock <- v;
   array::group(v, |n, _| n == 8)
}
"#;

run!(array_iterq, ARRAY_ITERQ, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[i64; 8]>()), Ok(Ok([1, 2, 3, 4, 5, 6, 7, 8])))
}; FuseExpect::Jit);

const ARRAY_FOLD0: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  array::fold(a, 0, |acc, x| x + acc)
}
"#;

run!(array_fold0, ARRAY_FOLD0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(55)) => true,
        _ => false,
    }
});

// A may-bottom predicate (`10 / x`) over a runtime-clean array fuses.
const ARRAY_FILTER_MAY_BOTTOM: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8];
  #[native]
  array::filter(a, |x| 10 / x > 2)
}
"#;

run!(array_filter_may_bottom, ARRAY_FILTER_MAY_BOTTOM, prints("[i64:1, i64:2, i64:3]"));

// A may-bottom fold body (`acc / x`) over a runtime-clean array fuses.
const ARRAY_FOLD_MAY_BOTTOM: &str = r#"
{
  let a = [2, 5, 10];
  #[native]
  array::fold(a, 1000, |acc, x| acc / x)
}
"#;

run!(array_fold_may_bottom, ARRAY_FOLD_MAY_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
});

// The fold's firing is per slot, not the acc carry: slot 0 consumes
// each fired init while the final carry stays stale, and the fold
// still fires per init re-fire. The counter pins the cadence.
const ARRAY_FOLD_MIDCHAIN_FIRE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let evens: Array<i64> = array::fold([1, 2], [x], |acc, v| select v % 2 {
    0 => [0],
    _ => acc
  });
  let c = 0;
  c <- evens ~ c + 1;
  filter(c, |n| n == 5)
}
"#;

run!(array_fold_midchain_fire, ARRAY_FOLD_MIDCHAIN_FIRE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
}; FuseExpect::Jit);

// A Value-shaped fold acc (`[Array, Error]` from a slice init) whose
// body's own shape is a narrower union member.
const ARRAY_FOLD_VALUE_ACC_ELEM_BODY: &str = r#"
{
  let a = [1, 2, 3, 4];
  array::fold([[9], [7]], a[1..3], |acc, v| v)
}
"#;

run!(array_fold_value_acc_elem_body, ARRAY_FOLD_VALUE_ACC_ELEM_BODY, prints("[i64:7]"));

// The same seam with a fresh-literal body.
const ARRAY_FOLD_VALUE_ACC_LITERAL_BODY: &str = r#"
{
  let a = [1, 2, 3, 4];
  array::fold([[9], [7]], a[0..3], |acc, v| [5, 6])
}
"#;

run!(
    array_fold_value_acc_literal_body,
    ARRAY_FOLD_VALUE_ACC_LITERAL_BODY,
    prints("[i64:5, i64:6]")
);

// The same seam with a scalar body over an `[i64, Error]` acc.
const ARRAY_FOLD_VALUE_ACC_SCALAR_BODY: &str = r#"
{
  let a = [1, 2, 3];
  array::fold([4, 5], a[0], |acc, x| x)
}
"#;

run!(array_fold_value_acc_scalar_body, ARRAY_FOLD_VALUE_ACC_SCALAR_BODY, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(5)))
});

// A may-bottom find predicate: `10 / x > 4` matches x = 2.
const ARRAY_FIND_MAY_BOTTOM: &str = r#"
{
  let a = [4, 2, 1];
  array::find(a, |x| 10 / x > 4)
}
"#;

run!(array_find_may_bottom, ARRAY_FIND_MAY_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
});

// A may-bottom flat_map body (`[10 / x]`) fuses.
const ARRAY_FLAT_MAP_MAY_BOTTOM: &str = r#"
{
  let a = [1, 2, 5];
  #[native]
  array::flat_map(a, |x| [10 / x])
}
"#;

run!(
    array_flat_map_may_bottom,
    ARRAY_FLAT_MAP_MAY_BOTTOM,
    prints("[i64:10, i64:5, i64:2]")
);

// A scalar `array::fold` result flowing into a `connect` sets `s`
// (lift.rs fold_into_connect_quiesces pins that it then quiesces).
const FOLD_INTO_CONNECT: &str = r#"
{ let a = [1, 2, 3]; let s = 0; s <- array::fold(a, 0, |acc, e| acc + e); filter(s, |v| v == 6) }
"#;

run!(fold_into_connect, FOLD_INTO_CONNECT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(6)))
});

// A fold whose input array grows each cycle re-emits per resize.
const FOLD_REACTIVE_SIZE: &str = r#"
{
  let a = array::init(array::iter([1, 2, 3, 4]), |i| i + 1);
  let f = array::fold(a, 0, |acc, x| acc + 2);
  array::group(f, |n, _| n == 4)
}
"#;

run!(fold_reactive_size, FOLD_REACTIVE_SIZE, prints("[i64:2, i64:4, i64:6, i64:8]"));

// The shrink sibling: sizes [1, 2, 1, 4]; a resize is a firing event
// even when every remaining slot is quiet.
const FOLD_REACTIVE_SHRINK: &str = r#"
{
  let a = array::init(array::iter([1, 2, 1, 4]), |i| i + 1);
  let f = array::fold(a, 0, |acc, x| acc + 2);
  array::group(f, |n, _| n == 4)
}
"#;

run!(fold_reactive_shrink, FOLD_REACTIVE_SHRINK, prints("[i64:2, i64:4, i64:2, i64:8]"));

const ARRAY_FOLD1: &str = r#"
{
  let a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  array::fold(a, 0, |acc, x| str::len(x) + acc)
}
"#;

run!(array_fold1, ARRAY_FOLD1, refused("string does not contain"); FuseExpect::None);

const ARRAY_CONCAT: &str = r#"
  array::concat([1, 2, 3], [4, 5], [6])
"#;

run!(array_concat, ARRAY_CONCAT, prints("[i64:1, i64:2, i64:3, i64:4, i64:5, i64:6]"));

const ARRAY_PUSH: &str = r#"
  array::push([(1, 2), (3, 4)], (5, 6))
"#;

run!(array_push, ARRAY_PUSH, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(1, 2), (3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; FuseExpect::Jit);

const ARRAY_PUSH_FRONT: &str = r#"
  array::push_front([(1, 2), (3, 4)], (5, 6))
"#;

run!(array_push_front, ARRAY_PUSH_FRONT, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(5, 6), (1, 2), (3, 4)]) => true,
        Ok(_) | Err(_) => false,
    }
}; FuseExpect::Jit);

// A select-union callback (`[string, Spec]`) whose result array feeds
// push_front with a bare Spec compiles regardless of typecheck order.
const ARRAY_MAP_UNION_CALLBACK_PUSH_FRONT: &str = r#"
{
  type Spec = { name: string };
  let mk = |n: string| -> Spec { name: n };
  let a = array::map(["a", "b"], |n| select n { "a" => mk(n), n => n });
  array::push_front(a, mk("z"))
}
"#;

run!(
    array_map_union_callback_push_front,
    ARRAY_MAP_UNION_CALLBACK_PUSH_FRONT,
    prints(r#"[[["name", "z"]], [["name", "a"]], "b"]"#)
);

const ARRAY_WINDOW0: &str = r#"
  array::window(#n:1, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window0, ARRAY_WINDOW0, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 1]>()) {
        Ok([(5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; FuseExpect::Jit);

const ARRAY_WINDOW1: &str = r#"
  array::window(#n:2, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window1, ARRAY_WINDOW1, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 2]>()) {
        Ok([(3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; FuseExpect::Jit);

const ARRAY_WINDOW2: &str = r#"
  array::window(#n:3, [(1, 2), (3, 4)], (5, 6))
"#;

run!(array_window2, ARRAY_WINDOW2, |v: Result<&Value>| {
    match v.and_then(|v| v.clone().cast_to::<[(u64, u64); 3]>()) {
        Ok([(1, 2), (3, 4), (5, 6)]) => true,
        Ok(_) | Err(_) => false,
    }
}; FuseExpect::Jit);

const ARRAY_LEN: &str = r#"
{
  use array::*;
  #[native]
  len(concat([1, 2, 3], [4, 5], [6]))
}
"#;

// Builtins called by their unqualified imported names (`use array::*;
// len(…)`) fuse.
run!(array_len, ARRAY_LEN, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
}; FuseExpect::Jit);

// A per-slot HOF callback whose body is a non-numeric `cast` (bool ->
// i64).
const CAST_CALLBACK_PER_SLOT: &str = r#"
  array::map([true, false, true], |b| cast<i64>(b))
"#;

run!(cast_callback_per_slot, CAST_CALLBACK_PER_SLOT, prints("[i64:1, i64:0, i64:1]"); FuseExpect::Jit);

const ARRAY_FLATTEN: &str = r#"
  array::flatten([[1, 2, 3], [4, 5], [6]])
"#;

run!(array_flatten, ARRAY_FLATTEN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(1), Value::I64(2), Value::I64(3), Value::I64(4), Value::I64(5), Value::I64(6)] => {
                true
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_GROUP0: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |_, v| v == 3)
}
"#;

run!(array_group0, ARRAY_GROUP0, prints("[i64:1, i64:2, i64:3]"); FuseExpect::None);

const ARRAY_GROUP1: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |x, v| (str::len(x) == 2) || (v == 3))
}
"#;

run!(array_group1, ARRAY_GROUP1, refused("string does not contain"); FuseExpect::None);

const ARRAY_GROUP2: &str = r#"
{
    let a = array::iter([1, 2, 3]);
    array::group(a, |v| v == 3)
}
"#;

// A callback of the wrong arity is told so.
run!(array_group2, ARRAY_GROUP2,
    refused("a function taking 1 argument where 2 arguments are passed"); FuseExpect::None);

// A predicate that answers bottom holds group until it answers: the
// elements that arrive meanwhile are judged once th has a value.
run!(array_group_waits_for_an_answer, r#"
{
    let th: [i64, null] = null;
    th <- sys::time::timer(duration:50.ms, false) ~ 2;
    let a = array::iter([1, 2, 3, 4, 5, 6]);
    array::group(a, |n, _| n >= th$)
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(1), Value::I64(2)])
}; FuseExpect::None);

const ARRAY_INIT0: &str = r#"
  array::init(5, |i| i * 2)
"#;

run!(array_init0, ARRAY_INIT0, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([0, 2, 4, 6, 8]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_INIT1: &str = r#"
  array::init(0, |i| i)
"#;

run!(array_init1, ARRAY_INIT1, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
});

// A negative count clamps to an empty array.
const ARRAY_INIT_NEGATIVE: &str = r#"
{
  let k = -1;
  array::init(k, |i| i)
}
"#;

run!(array_init_negative, ARRAY_INIT_NEGATIVE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
});

const ARRAY_INIT2: &str = r#"
{
  let a = array::init(3, |i| i + 1);
  array::fold(a, 0, |acc, x| acc + x)
}
"#;

run!(array_init2, ARRAY_INIT2, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(6)) => true,
        _ => false,
    }
});

const ARRAY_INIT3: &str = r#"
  array::init(4, |i| (i, i * i))
"#;

run!(array_init3, ARRAY_INIT3, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 4]>() {
            Ok([(0, 0), (1, 1), (2, 4), (3, 9)]) => true,
            _ => false,
        },
        _ => false,
    }
});

const ARRAY_INIT4: &str = r#"
  array::init(3, |i| str::len(i))
"#;

run!(array_init4, ARRAY_INIT4, refused("string does not contain"); FuseExpect::None);

const ARRAY_SORT0: &str = r#"
{
   let a = [5, 4, 3, 2, 1];
   array::sort(a)
}
"#;

run!(array_sort0, ARRAY_SORT0, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([1, 2, 3, 4, 5]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

// Both labels defaulted: the plain spelling is a native fastcall.
const ARRAY_SORT_NATIVE_DEFAULTS: &str = r#"{
   let f = |a: Array<i64>| { let r = #[native] array::sort(a); r };
   f([3, 1, 2])
}"#;

run!(array_sort_native_defaults, ARRAY_SORT_NATIVE_DEFAULTS, |v: Result<&Value>| {
    match v {
        Ok(v) => matches!(v.clone().cast_to::<[i64; 3]>(), Ok([1, 2, 3])),
        _ => false,
    }
});

const ARRAY_SORT1: &str = r#"
{
   let a = [5, 4, 3, 2, 1];
   array::sort(#dir:`Descending, a)
}
"#;

run!(array_sort1, ARRAY_SORT1, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([5, 4, 3, 2, 1]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_SORT2: &str = r#"
{
   let a = ["5", "6", "50", "60", "40", "4", "3", "2", "1"];
   array::sort(#numeric:true, a)
}
"#;

run!(array_sort2, ARRAY_SORT2, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 9]>() {
            Ok([a0, a1, a2, a3, a4, a5, a6, a7, a8]) => {
                &*a0 == "1"
                    && &*a1 == "2"
                    && &*a2 == "3"
                    && &*a3 == "4"
                    && &*a4 == "5"
                    && &*a5 == "6"
                    && &*a6 == "40"
                    && &*a7 == "50"
                    && &*a8 == "60"
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_SORT3: &str = r#"
{
   let a = ["5", "6", "50", "60", "40", "4", "3", "2", "1"];
   array::sort(#dir:`Descending, #numeric:true, a)
}
"#;

run!(array_sort3, ARRAY_SORT3, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 9]>() {
            Ok([a0, a1, a2, a3, a4, a5, a6, a7, a8]) => {
                &*a0 == "60"
                    && &*a1 == "50"
                    && &*a2 == "40"
                    && &*a3 == "6"
                    && &*a4 == "5"
                    && &*a5 == "4"
                    && &*a6 == "3"
                    && &*a7 == "2"
                    && &*a8 == "1"
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_DEDUP0: &str = r#"
{
   let a = [1, 2, 2, 3, 1, 4, 3, 5];
   array::dedup(a)
}
"#;

run!(array_dedup0, ARRAY_DEDUP0, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[i64; 5]>() {
            Ok([1, 2, 3, 4, 5]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_DEDUP1: &str = r#"
{
   let a: Array<i64> = [];
   array::dedup(a)
}
"#;

run!(array_dedup1, ARRAY_DEDUP1, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_DEDUP2: &str = r#"
{
   let a = ["a", "b", "a", "c", "b", "d"];
   array::dedup(a)
}
"#;

run!(array_dedup2, ARRAY_DEDUP2, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[ArcStr; 4]>() {
            Ok([s0, s1, s2, s3]) => {
                &*s0 == "a" && &*s1 == "b" && &*s2 == "c" && &*s3 == "d"
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

// HOFs over String / Value-shape elements: the no-match, found-at-last
// and used-twice paths.

const HOF_STR_MAP_LEN: &str = r#"array::map(["a", "bb", "ccc"], |s| str::len(s))"#;
run!(hof_str_map_len, HOF_STR_MAP_LEN, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 3]>()),
    Ok(Ok([1, 2, 3]))
));

// A String element and String output.
const HOF_STR_MAP_UPPER: &str = r#"array::map(["hi", "yo"], |s| str::to_upper(s))"#;
run!(hof_str_map_upper, HOF_STR_MAP_UPPER, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "HI" && &*b == "YO")
}; FuseExpect::Jit);

// filter moves the kept string element into the output.
const HOF_STR_FILTER: &str = r#"array::filter(["a", "bb", "ccc"], |s| str::len(s) > 1)"#;
run!(hof_str_filter, HOF_STR_FILTER, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "bb" && &*b == "ccc")
});

// A no-match filter: every string element is dropped.
const HOF_STR_FILTER_NONE: &str = r#"array::filter(["a", "b"], |s| str::len(s) > 5)"#;
run!(hof_str_filter_none, HOF_STR_FILTER_NONE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::Array(a)) if a.is_empty()
));

const HOF_STR_FOLD: &str =
    r#"array::fold(["a", "bb", "ccc"], 0, |acc, s| acc + str::len(s))"#;
run!(hof_str_fold, HOF_STR_FOLD, is(Value::I64(6));
    FuseExpect::Jit);

// find returns the matched string element; non-matches drop.
const HOF_STR_FIND: &str = r#"array::find(["a", "bb", "ccc"], |s| str::len(s) == 2)"#;
run!(hof_str_find, HOF_STR_FIND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "bb"
));

// found at the last element
const HOF_STR_FIND_LAST: &str =
    r#"array::find(["a", "bb", "ccc"], |s| str::len(s) == 3)"#;
run!(hof_str_find_last, HOF_STR_FIND_LAST, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "ccc"
));

const HOF_STR_FIND_NONE: &str = r#"array::find(["a", "b"], |s| str::len(s) == 9)"#;
run!(hof_str_find_none, HOF_STR_FIND_NONE, is(Value::Null));

const HOF_STR_FLATMAP: &str = r#"array::flat_map(["a", "b"], |s| [s, s])"#;
run!(hof_str_flatmap, HOF_STR_FLATMAP, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 4]>()),
        Ok(Ok([a, b, c, d])) if &*a == "a" && &*b == "a" && &*c == "b" && &*d == "b")
});

// The element is read twice in the body.
const HOF_STR_USED_TWICE: &str = r#"array::map(["a", "b"], |s| "[s][s]")"#;
run!(hof_str_used_twice, HOF_STR_USED_TWICE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "aa" && &*b == "bb")
});

// A Value-shape (nullable) element whose body selects over it.
const HOF_NULLABLE_MAP: &str = r#"
array::map([1, null], |v| select v { i64 as n => n, null as _ => i64:0 })
"#;
run!(hof_nullable_map, HOF_NULLABLE_MAP, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 2]>()),
    Ok(Ok([1, 0]))
); FuseExpect::Jit);

// A Value-shape (variant) element in a filter whose predicate is a select.
const HOF_VARIANT_FILTER: &str = r#"
array::filter([`Red, `Green, `Red], |v| select v { `Red => true, _ => false })
"#;
run!(hof_variant_filter, HOF_VARIANT_FILTER, prints(r#"["Red", "Red"]"#));

// A Value-shape variant element returned by find after a dropped
// non-match.
const HOF_VARIANT_FIND: &str = r#"
array::find([`Red, `Green, `Blue], |v| select v { `Green => true, _ => false })
"#;
run!(hof_variant_find, HOF_VARIANT_FIND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "Green"
));

const ARRAY_ENUMERATE: &str = r#"
{
   let a = [1, 2, 3];
   array::enumerate(a)
}
"#;

run!(array_enumerate, ARRAY_ENUMERATE, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(0, 1), (1, 2), (2, 3)]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_ZIP: &str = r#"
{
   let a0 = [1, 2, 5];
   let a1 = [1, 2, 3];
   array::zip(a0, a1)
}
"#;

run!(array_zip, ARRAY_ZIP, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[(i64, i64); 3]>() {
            Ok([(1, 1), (2, 2), (5, 3)]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_UNZIP: &str = r#"
{
   let a = [(1, 1), (2, 2), (5, 3)];
   array::unzip(a)
}
"#;

run!(array_unzip, ARRAY_UNZIP, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<([i64; 3], [i64; 3])>() {
            Ok(([1, 2, 5], [1, 2, 3])) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_ROTATE_FORWARD: &str = r#"
{
   let a = [1, 2, 3];
   [
     array::rotate(a),
     array::rotate(#n: 1, a),
     array::rotate(#n: 2, a),
     array::rotate(#n: 3, a),
     array::rotate(#n: 4, a)
   ]
}
"#;

run!(array_rotate_forward, ARRAY_ROTATE_FORWARD, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 3]; 5]>() {
            Ok([[3, 1, 2], [3, 1, 2], [2, 3, 1], [1, 2, 3], [3, 1, 2]]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_ROTATE_BACKWARD: &str = r#"
{
   let a = [1, 2, 3];
   [
     array::rotate(#n: -1, a),
     array::rotate(#n: -2, a),
     array::rotate(#n: -3, a),
     array::rotate(#n: -4, a)
   ]
}
"#;

run!(array_rotate_backward, ARRAY_ROTATE_BACKWARD, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 3]; 4]>() {
            Ok([[2, 3, 1], [3, 1, 2], [1, 2, 3], [2, 3, 1]]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_ROTATE_ZERO_AND_SINGLE: &str = r#"
(array::rotate(#n: 0, [1, 2, 3]), array::rotate([7]), array::rotate(#n: -5, [7]))
"#;

run!(array_rotate_zero_and_single, ARRAY_ROTATE_ZERO_AND_SINGLE, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<([i64; 3], [i64; 1], [i64; 1])>() {
            Ok(([1, 2, 3], [7], [7])) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_ROTATE_EMPTY: &str = r#"
{
   let a: Array<i64> = [];
   [array::rotate(a), array::rotate(#n: 3, a), array::rotate(#n: -3, a)]
}
"#;

run!(array_rotate_empty, ARRAY_ROTATE_EMPTY, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(r)) => {
            r.len() == 3 && r.iter().all(|v| matches!(v, Value::Array(a) if a.is_empty()))
        }
        _ => false,
    }
}; FuseExpect::Jit);

const ARRAY_ROTATE_EXTREME_N: &str = r#"
{
   let a = [1, 2, 3];
   let min = -9223372036854775807 - 1;
   let max = 9223372036854775807;
   [array::rotate(#n: min, a), array::rotate(#n: max, a)]
}
"#;

run!(array_rotate_extreme_n, ARRAY_ROTATE_EXTREME_N, |v: Result<&Value>| {
    match v {
        Ok(v) => match v.clone().cast_to::<[[i64; 3]; 2]>() {
            Ok([[3, 1, 2], [3, 1, 2]]) => true,
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

// `|(k, v)|` callbacks whose leaf is itself composite/string/value.

const HOF_LEAF_COMPOSITE: &str = r#"
array::map([((1, 2), 10), ((3, 4), 20)], |(pt, n)| pt.0 + pt.1 + n)
"#;
run!(hof_leaf_composite, HOF_LEAF_COMPOSITE, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 2]>()),
    Ok(Ok([13, 27]))
));

const HOF_LEAF_STRING: &str = r#"
array::fold([("a", 1), ("bb", 2)], 0, |acc, (s, n)| acc + str::len(s) + n)
"#;
run!(hof_leaf_string, HOF_LEAF_STRING, is(Value::I64(6)); FuseExpect::Jit);

// filter over a tuple leaf keeps a match whole; a no-match and an
// all-match filter too.
const HOF_LEAF_FILTER: &str = r#"
array::filter([((1, 2), 0), ((5, 6), 1)], |(pt, n)| pt.1 > 3)
"#;
run!(hof_leaf_filter, HOF_LEAF_FILTER, prints("[[[i64:5, i64:6], i64:1]]"));

const HOF_LEAF_FILTER_NONE: &str = r#"
array::filter([((1, 2), 0), ((5, 6), 1)], |(pt, n)| pt.1 > 9)
"#;
run!(hof_leaf_filter_none, HOF_LEAF_FILTER_NONE, prints("[]"));

const HOF_LEAF_FILTER_ALL: &str = r#"
array::filter([((1, 2), 0), ((5, 6), 1)], |(pt, n)| pt.1 > 0)
"#;
run!(
    hof_leaf_filter_all,
    HOF_LEAF_FILTER_ALL,
    prints("[[[i64:1, i64:2], i64:0], [[i64:5, i64:6], i64:1]]")
);

// A string leaf read twice in the body.
const HOF_LEAF_STRING_TWICE: &str = r#"
array::map([("x", 1), ("y", 2)], |(s, n)| "[s][s][n]")
"#;
run!(hof_leaf_string_twice, HOF_LEAF_STRING_TWICE, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[ArcStr; 2]>()),
        Ok(Ok([a, b])) if &*a == "xx1" && &*b == "yy2")
});

// A tuple element whose callback tests its i64 key and returns its value or
// null.
const HOF_LEAF_NULLABLE: &str = r#"
array::filter_map([(1, 10), (2, 20)], |(k, v)| select k == 2 { true => v, false => null })
"#;
run!(hof_leaf_nullable, HOF_LEAF_NULLABLE, |v: Result<&Value>| matches!(
    v.map(|v| v.clone().cast_to::<[i64; 1]>()),
    Ok(Ok([20]))
); FuseExpect::Jit);

// Composite-returning callbacks: a `['b, null]` return over a composite
// body widens to an owned Value.
const FIND_MAP_CAPTURED_ARRAY: &str = r#"
{let a = [i64:1, i64:2]; array::find_map(a, |x: i64| a)}
"#;
run!(find_map_captured_array, FIND_MAP_CAPTURED_ARRAY, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[i64; 2]>()), Ok(Ok([1, 2])))
}; FuseExpect::Jit);

const FILTER_MAP_FRESH_ARRAY: &str = r#"
{
  let a = [i64:1, i64:2, i64:3];
  array::filter_map(a, |x| [x, x + i64:1])
}
"#;
run!(filter_map_fresh_array, FILTER_MAP_FRESH_ARRAY, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<[[i64; 2]; 3]>()),
        Ok(Ok([[1, 2], [2, 3], [3, 4]])))
}; FuseExpect::Jit);

// The select-arm variant: null in one arm, a tuple in the other.
const FIND_MAP_TUPLE_ARM: &str = r#"
{let a = [i64:1, i64:2]; array::find_map(a, |x: i64| select x { i64:1 => null, _ => (x, "s") })}
"#;
run!(find_map_tuple_arm, FIND_MAP_TUPLE_ARM, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<(i64, ArcStr)>()),
        Ok(Ok((2, s))) if &*s == "s")
}; FuseExpect::Jit);

/// A predicate that answers late leaves elements queued behind the group
/// it closes; each starts as soon as the group before it is out.
#[tokio::test(flavor = "multi_thread")]
async fn group_takes_what_queued_while_it_waited() -> Result<()> {
    const CODE: &str = r#"{
  let x = never();
  x <- array::iter([1, 2, 3, 4, 5, 6]);
  array::group(x, |n, x| sys::time::after_idle(duration:20.ms, n == 2))
}"#;
    let pair = |a, b| Value::Array([Value::I64(a), Value::I64(b)].into_iter().collect());
    for mode in Mode::ALL {
        let got = collect(
            CODE,
            crate::TEST_REGISTER,
            mode,
            Collect::First(3),
            Duration::from_secs(10),
        )
        .await?;
        assert_eq!(got, [pair(1, 2), pair(3, 4), pair(5, 6)], "{mode:?}");
    }
    Ok(())
}

// A predicate re-fired by what else it reads, with no element waiting,
// answers nothing: no empty group.
// XCR claude for claude: [test-gap/medium] This pin cannot fail. The `limit` write lands
// at c = 6, which is when the old Group delivered its empty group. The result `select c
// { 6 => log, .. }` also fires at c = 6, one cycle before `log <- g ~ ..` lands the [].
// run! judges only the first update, so the fixture sees [[1, 2, 3]] with or without
// the `Some(_) if self.ready` arm in Group::update. Run the counter to 8 and select `8
// => log` (or assert on the settled log via Collect::Quiet); then the bug shows as the
// first value [[1, 2, 3], []]. probe:
// design/review-2026-10-26/repro/tests-lib-a-group-vacuous.gx (tests-lib-a-01)
// 2026-10-26 claude: The counter runs to 8 and the result selects 8 => log, so the []
// the old Group delivered lands before the result fires. With the Some(_) if self.ready
// arm removed from Group::update every mode fails with [[1, 2, 3], []] (checked). Pin:
// lib_tests::array::group_answers_only_a_waiting_element.
run!(group_answers_only_a_waiting_element, r#"{
    let c = 0;
    c <- select c { n if n < 8 => n + 1, _ => never() };
    let limit = 3;
    let x = array::iter([1, 2, 3]);
    let g = array::group(x, |n, x| n >= limit);
    limit <- select c { 5 => 1, _ => never() };
    let log: Array<Array<i64>> = [];
    log <- g ~ array::push(log, g);
    select c { 8 => log, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[[i64:1, i64:2, i64:3]]")
}; FuseExpect::Jit);

// A predicate that answers a cycle late, while elements queue behind it:
// each answer is the element's in flight, so every group ends at an even
// element.
run!(group_late_answer_belongs_to_its_element, r#"{
    let c = 0;
    c <- select c < 8 { true => c + 1, false => never() };
    let pred = |n: i64, x: i64| { let r = never(); r <- x; r % 2 == 0 };
    let g = array::group(c, pred);
    let log: Array<Array<i64>> = [];
    log <- g ~ array::push(log, g);
    sys::time::after_idle(duration: 200.ms, log) ~ log
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[[i64:0], [i64:1, i64:2], [i64:3, i64:4], [i64:5, i64:6], [i64:7, i64:8]]")
}; FuseExpect::Jit);

// A seq step's next run is a rebirth: group forgets what an aborted run
// buffered (collections-str-05).
run!(group_is_reborn_with_a_seq_run, r#"{
    let t = sys::time::timer(duration: 60.ms, 3);
    let k = count(t);
    let r = seq t; abort(sys::time::after_idle(duration: 20.ms, t)) {
        let g = array::group(k, |n, v| n == 2);
        g
    };
    let log: Array<Array<i64>> = [];
    log <- r ~ array::push(log, r);
    sys::time::after_idle(duration: 300.ms, t) ~ log
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[]")
}; FuseExpect::Jit);

// A numeric sort orders integers exactly, past f64's 2^53, and an
// integer against a float by value (collections-str-03).
run!(numeric_sort_is_exact, r#"{
    let a = [9007199254740993, 9007199254740992];
    let s = ["9007199254740993", "9007199254740992", "1.5", "1", "2"];
    let l = list::from_array(a);
    (array::sort(#numeric: true, a), array::sort(#numeric: true, s), list::to_array(list::sort(#numeric: true, l)))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"[[i64:9007199254740992, i64:9007199254740993], ["1", "1.5", "2", "9007199254740992", "9007199254740993"], [i64:9007199254740992, i64:9007199254740993]]"#)
}; FuseExpect::Jit);
