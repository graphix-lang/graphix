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

const MAP_GET_OR_PRESENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get_or(m, "b", 99)
}
"#;

run!(map_get_or_present, MAP_GET_OR_PRESENT, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
});

const MAP_GET_OR_ABSENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get_or(m, "d", 99)
}
"#;

run!(map_get_or_absent, MAP_GET_OR_ABSENT, |v: Result<&Value>| match v {
    Ok(Value::I64(99)) => true,
    _ => false,
});

const MAP_CHANGE_PRESENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(map::change(m, "b", 0, |v| v + 10), "b")
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
// CR claude for claude: [doc-drift] The `ASPIRE: Jit — the body does not fuse into a
// kernel yet` line above is false here: this body fuses whole today (it compiles under
// `#[native]`), and so do map.rs:87, 101, 117 and str.rs:317, 335. In typecheck.rs (27,
// 62, 75, 87, 139, 151, 243) the bodies call json::read or pack::read, which are async
// and never fuse under strict fusion, so 'yet' promises something the design rules out.
// FuseExpect::Jit itself means only that some kernel ran. The same line sits above
// FuseExpect::Jit in 76 fixtures across stdlib/graphix-tests and above FuseExpect::None
// in 15 more. Delete them; a fixture that must fuse whole says so with `#[native]` on
// its body. (tests-lib-b2-09)
run!(map_change_present, MAP_CHANGE_PRESENT, |v: Result<&Value>| match v {
    Ok(Value::I64(12)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const MAP_CHANGE_ABSENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(map::change(m, "d", 100, |v| v + 10), "d")
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(map_change_absent, MAP_CHANGE_ABSENT, |v: Result<&Value>| match v {
    Ok(Value::I64(110)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const MAP_CHANGE_PRESERVES_OTHERS: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  let m2 = map::change(m, "b", 0, |v| v * 100);
  map::get(m2, "a")
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(map_change_preserves_others, MAP_CHANGE_PRESERVES_OTHERS, |v: Result<&Value>| match v {
    Ok(Value::I64(1)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const MAP_CHANGE_CHAINED: &str = r#"
{
  let m: Map<string, i64> = {};
  let m = map::change(m, "count", 0, |n| n + 1);
  let m = map::change(m, "count", 0, |n| n + 1);
  let m = map::change(m, "count", 0, |n| n + 1);
  map::get(m, "count")
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(map_change_chained, MAP_CHANGE_CHAINED, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

const MAP_FOLD: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::fold(m, 0, |acc, (k, v)| acc + v)
}
"#;

run!(map_fold, MAP_FOLD, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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

// Direct Map-HOF lowering.

// The whole HOF as one kernel; `#[native]` proves the loop is in the
// kernel.
const MAP_FOLD_NATIVE: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  #[native] map::fold(m, 0, |acc, (k, v)| acc + v)
}
"#;
run!(map_fold_native, MAP_FOLD_NATIVE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(6)))
});

const MAP_MAP_NATIVE: &str = r#"
{
  let m = {"a" => 1, "b" => 2};
  #[native] map::map(m, |(k, v)| (k, v * 2))
}
"#;
run!(map_map_native, MAP_MAP_NATIVE, |v: Result<&Value>| match v {
    Ok(Value::Map(m)) =>
        m.len() == 2
            && m[&Value::String(literal!("a"))] == Value::I64(2)
            && m[&Value::String(literal!("b"))] == Value::I64(4),
    _ => false,
});

// A key collision through map::map: both engines rebuild through the
// one CMap::from_iter seam, so they agree on the duplicate policy.
const MAP_MAP_KEY_COLLISION: &str = r#"
{
  let m = {"a" => 1, "b" => 2};
  let collided = map::map(m, |(k, v)| ("same", v));
  (map::len(collided), map::get(collided, "same"))
}
"#;
// CR claude for claude: [test-gap] The predicate accepts 1 or 2. run! checks each mode
// against the predicate separately and never compares one mode's value with another's,
// so this test cannot see the engines disagree, which is what the comment above says it
// pins. For loose predicates like this one, CLAUDE.md's 'asserting equal values' for
// run! does not hold. The policy also differs by constructor: a literal `{k => 1, k =>
// 2}` keeps the last value, while map::map and map::filter_map keep the first when two
// keys collapse, through CMap::from_iter in pairs_to_map
// (graphix-compiler/src/node/collection.rs:205). Choose one policy, then pin the exact
// value (`Ok(Ok((1, 1)))` today). Probe: `let k = "same"; (map::get({k => 1, k => 2},
// k), map::get(map::map({"a" => 1, "b" => 2}, |(kk, v)| (k, v)), k))` gives (2, 1) in
// both engines. (tests-lib-b2-03)
run!(map_map_key_collision, MAP_MAP_KEY_COLLISION, |v: Result<&Value>| {
    matches!(
        v.map(|v| v.clone().cast_to::<(i64, i64)>()),
        Ok(Ok((1, n))) if n == 1 || n == 2
    )
});

const MAP_KEY_MIXED_ABSTRACT_KINDS: &str = r#"
{
  type T = Abstract<i64>;
  let f = |x: i64| x;
  let u: [T, fn(x: i64) -> i64] = T(1);
  let w: [T, fn(x: i64) -> i64] = f;
  let m = {u => 1, w => 2};
  map::len(m)
}
"#;

run!(map_key_mixed_abstract_kinds, MAP_KEY_MIXED_ABSTRACT_KINDS, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(2)) => true,
        _ => false,
    }
});
