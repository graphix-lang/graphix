use anyhow::Result;
use arcstr::literal;
use graphix_package_core::{
    run,
    testing::{Collect, FuseExpect, Mode, collect, is, prints, refused},
};
use netidx::subscriber::Value;
use std::time::Duration;

const MAP_LEN: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::len(m)
}
"#;

run!(map_len, MAP_LEN, is(Value::I64(3)));

const MAP_GET_PRESENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(m, "b")
}
"#;

run!(map_get_present, MAP_GET_PRESENT, is(Value::I64(2)));

const MAP_GET_ABSENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(m, "d")
}
"#;

run!(map_get_absent, MAP_GET_ABSENT, is(Value::Null));

const MAP_GET_OR_PRESENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get_or(m, "b", 99)
}
"#;

run!(map_get_or_present, MAP_GET_OR_PRESENT, is(Value::I64(2)));

const MAP_GET_OR_ABSENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get_or(m, "d", 99)
}
"#;

run!(map_get_or_absent, MAP_GET_OR_ABSENT, is(Value::I64(99)));

const MAP_CHANGE_PRESENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(map::change(m, "b", 0, |v| v + 10), "b")
}
"#;

run!(map_change_present, MAP_CHANGE_PRESENT, is(Value::I64(12)); FuseExpect::Jit);

const MAP_CHANGE_ABSENT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::get(map::change(m, "d", 100, |v| v + 10), "d")
}
"#;

run!(map_change_absent, MAP_CHANGE_ABSENT, is(Value::I64(110)); FuseExpect::Jit);

const MAP_CHANGE_PRESERVES_OTHERS: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  let m2 = map::change(m, "b", 0, |v| v * 100);
  map::get(m2, "a")
}
"#;

run!(map_change_preserves_others, MAP_CHANGE_PRESERVES_OTHERS, is(Value::I64(1)); FuseExpect::Jit);

const MAP_CHANGE_CHAINED: &str = r#"
{
  let m: Map<string, i64> = {};
  let m = map::change(m, "count", 0, |n| n + 1);
  let m = map::change(m, "count", 0, |n| n + 1);
  let m = map::change(m, "count", 0, |n| n + 1);
  map::get(m, "count")
}
"#;

run!(map_change_chained, MAP_CHANGE_CHAINED, is(Value::I64(3)); FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

const MAP_FOLD: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  map::fold(m, 0, |acc, (k, v)| acc + v)
}
"#;

run!(map_fold, MAP_FOLD, is(Value::I64(6)); FuseExpect::Jit);

const MAP_ITER: &str = r#"
{
  let m = {"a" => 1, "b" => 2};
  let (_, v) = map::iter(m);
  array::group(v, |n, _| n == 2)
}
"#;

run!(map_iter, MAP_ITER, prints("[i64:1, i64:2]"); FuseExpect::Jit);

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

run!(map_iterq, MAP_ITERQ, prints("[i64:1, i64:2, i64:3, i64:4, i64:5]"); FuseExpect::Jit);

const MAP_INSERT: &str = r#"
{
  let m = {"a" => 1, "b" => 2, "c" => 3};
  let m = map::insert(m, "d", 4);
  let m = map::insert(m, "e", 5);
  m == { "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5 }
}
"#;

run!(map_insert, MAP_INSERT, is(Value::Bool(true)));

const MAP_REMOVE: &str = r#"
{
  let m = { "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5 };
  let m = map::remove(m, "d");
  let m = map::remove(m, "e");
  m == {"a" => 1, "b" => 2, "c" => 3}
}
"#;

run!(map_remove, MAP_REMOVE, is(Value::Bool(true)));

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

// When keys collide the last pair wins, in a literal as through map::map
// and map::filter_map.
const MAP_MAP_KEY_COLLISION: &str = r#"
{
  let m = {"a" => 1, "b" => 2};
  let collided = map::map(m, |(k, v)| ("same", v));
  let filtered = map::filter_map(m, |(k, v)| ("same", v));
  (
    map::len(collided),
    map::get(collided, "same")$,
    map::get(filtered, "same")$,
    map::get({"same" => 1, "same" => 2}, "same")$
  )
}
"#;
run!(map_map_key_collision, MAP_MAP_KEY_COLLISION, |v: Result<&Value>| {
    matches!(v.map(|v| v.clone().cast_to::<(i64, i64, i64, i64)>()), Ok(Ok((1, 2, 2, 2))))
});

// A function has no order, so no map key type holds one, even beside a
// type that orders.
const MAP_KEY_REFUSES_A_FUNCTION: &str = r#"
{
  type T = Abstract<i64>;
  let f = |x: i64| x;
  let u: [T, fn(x: i64) -> i64] = T(1);
  let w: [T, fn(x: i64) -> i64] = f;
  let m = {u => 1, w => 2};
  map::len(m)
}
"#;

run!(
    map_key_refuses_a_function,
    MAP_KEY_REFUSES_A_FUNCTION,
    refused("a map key type can't hold the function fn(x: i64) -> i64");
    FuseExpect::None
);

// Functions have no order: a sort and an ordering over them are refused.
run!(
    sort_refuses_functions,
    "array::sort([|x: i64| x, |x: i64| x + 1])",
    refused("functions have no order, and only == and != compare them, by identity");
    FuseExpect::None
);

run!(
    ordering_refuses_functions,
    "{ let f = |x: i64| x; let g = |x: i64| x; f < g }",
    refused("can't order functions");
    FuseExpect::None
);

const MAP_UNION_SECOND_WINS: &str = r#"
{
  let big = {1 => "a", 2 => "a", 3 => "a", 4 => "a", 5 => "a", 6 => "a", 7 => "a", 8 => "a"};
  let one = {1 => "b"};
  let ab = map::union(big, one);
  let ba = map::union(one, big);
  let eq = map::union({1 => "a"}, {1 => "b"});
  "[map::get(ab, 1)$][map::get(ba, 1)$][map::get(eq, 1)$][map::get(ab, 8)$]"
}
"#;

run!(map_union_second_wins, MAP_UNION_SECOND_WINS, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => s == "baba",
    _ => false,
});

/// `map::iter` walks every pair of a map keyed by a user `Ord`, in that
/// order.
#[tokio::test(flavor = "multi_thread")]
async fn iter_walks_a_user_ordered_map() -> Result<()> {
    const CODE: &str = r#"{
  type T = Abstract<i64>;
  impl Ord for T {
    let cmp = |x, y| select (x.0, y.0) {
      (a, b) if a < b => `Greater,
      (a, b) if a > b => `Less,
      _ => `Equal
    }
  };
  let m = {T(0) => 0, T(1) => 1, T(2) => 2, T(3) => 3};
  let (k, v) = map::iter(m);
  v
}"#;
    let want: Vec<Value> = [3, 2, 1, 0].into_iter().map(Value::I64).collect();
    for mode in Mode::ALL {
        let quiet = Collect::Quiet(Duration::from_millis(300));
        let got =
            collect(CODE, crate::TEST_REGISTER, mode, quiet, Duration::from_secs(10))
                .await?;
        assert_eq!(got, want, "{mode:?}");
    }
    Ok(())
}

// A missing key is named in the error as a message prints it: an abstract
// key by its Display impl, never by its packed bytes.
run!(
    missing_abstract_key_prints_its_display,
    r#"{
    type K = Abstract<i64>;
    impl Display for K { let fmt = |k| "K#[k.0]" };
    let m = {K(5) => "a"};
    m{K(1)}
}"#,
    |v: Result<&Value>| format!("{}", v.unwrap()).contains("map key K#1 not found")
);

// A string and a bare variant are two keys, in a map a cast or a type
// test makes as in a literal.
run!(written_map_key_cast, r#"{
    let raw: Map<string, i64> = {"A" => 1};
    let m = cast<Map<[string, `A], i64>>(raw)$;
    let a = select m{`A} { error as _ => 0, n => n };
    (m{"A"}$, a)
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:0]"); FuseExpect::Jit);

run!(written_map_key_type_test, r#"{
    let v: Any = {"A" => 1};
    select v { Map<[string, `A], i64> as m => m{"A"}$, _ => 0 }
}"#, is(Value::I64(1)); FuseExpect::Jit);
