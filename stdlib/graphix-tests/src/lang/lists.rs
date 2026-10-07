// Native List literals and list-slice patterns (design/list_native.md).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::publisher::Value;

const LIST_LIT_BASIC: &str = r#"
  list::to_array([<1, 2, 3>])
"#;

run!(list_lit_basic, LIST_LIT_BASIC, |v: Result<&Value>| {
    match v {
        Ok(v) => matches!(v.clone().cast_to::<[i64; 3]>(), Ok([1, 2, 3])),
        _ => false,
    }
}; FuseExpect::Jit);

const LIST_LIT_EMPTY: &str = r#"
  list::len([<>])
"#;

run!(list_lit_empty, LIST_LIT_EMPTY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(0))
); FuseExpect::Jit);

const LIST_LIT_NESTED: &str = r#"
{
  let nested = [<[<1>], [<2, 3>]>];
  list::fold(nested, 0, |a, x| a + list::fold(x, 0, |a, y| a + y))
}
"#;

run!(list_lit_nested, LIST_LIT_NESTED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); FuseExpect::Jit);

// The canonical ladder: `[<>]` + `[<h, t..>]` is exhaustive and the
// tail bind is the k-th tail, O(1); fuses end to end.
const LIST_PAT_SUM: &str = r#"
{
  let rec sum = |l: List<i64>, acc: i64| -> i64
    select l { [<>] => acc, [<h, t..>] => sum(t, acc + h) };
  sum([<1, 2, 3>], 0)
}
"#;

run!(list_pat_sum, LIST_PAT_SUM, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); FuseExpect::Jit);

// Exact-length arms miss on other lengths; anonymous rest `..`.
const LIST_PAT_SHAPES: &str = r#"
{
  let l = [<1, 2, 3>];
  let two = select l { [<a, b>] => a + b, [<>] => -1, [<_, ..>] => -2 };
  let first = select l { [<>] => -1, [<h, ..>] => h };
  two * 100 + first
}
"#;

run!(list_pat_shapes, LIST_PAT_SHAPES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(-199))
); FuseExpect::Jit);

// Guards consult after structure; the @-bind captures the whole list.
const LIST_PAT_GUARD_AT: &str = r#"
{
  let l = [<10, 20>];
  select l {
    [<>] => -1,
    w@ [<h, ..>] if h > 5 => h + list::len(w),
    [<_, ..>] => -2
  }
}
"#;

run!(list_pat_guard_at, LIST_PAT_GUARD_AT, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(12))
); FuseExpect::Jit);

// The tail bind shares the spine: it is the k-th tail.
const LIST_PAT_TAIL: &str = r#"
{
  let l = [<1, 2, 3>];
  select l { [<>] => -1, [<_, t..>] => list::len(t) }
}
"#;

run!(list_pat_tail, LIST_PAT_TAIL, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(2))
); FuseExpect::Jit);

const LIST_PAT_NONEXHAUSTIVE: &str = r#"
{
  let l = [<1, 2>];
  select l { [<>] => 0 }
}
"#;

run!(list_pat_nonexhaustive, LIST_PAT_NONEXHAUSTIVE, refused("missing match cases");
    FuseExpect::None);

const LIST_PAT_DEAD_WILDCARD: &str = r#"
{
  let l = [<1, 2>];
  select l { [<>] => 0, [<h, t..>] => h, _ => -1 }
}
"#;

run!(list_pat_dead_wildcard, LIST_PAT_DEAD_WILDCARD, refused("unreachable arm");
    FuseExpect::None);

// The suffix form is refused for lists: the front is an O(n) walk.
const LIST_PAT_SUFFIX_REFUSED: &str = r#"
{
  let l = [<1, 2>];
  select l { [<init.., last>] => last, _ => -1 }
}
"#;

run!(list_pat_suffix_refused, LIST_PAT_SUFFIX_REFUSED, refused("list patterns have no suffix form");
    FuseExpect::None);

// `list::flat_map` fuses: its callback's List return is an opaque value
// the extend helper walks.
const LIST_FLAT_MAP_NATIVE: &str = r#"
{
  let l = [<1, 2>];
  let r = #[native] list::flat_map(l, |x| [<x, x + 1>]);
  list::to_array(r)
}
"#;

run!(list_flat_map_native, LIST_FLAT_MAP_NATIVE, |v: Result<&Value>| {
    match v {
        Ok(v) => matches!(v.clone().cast_to::<[i64; 4]>(), Ok([1, 2, 2, 3])),
        _ => false,
    }
}; FuseExpect::Jit);

// A union whose array-shaped members are all lists says what the value
// is, so a cast reads the list as a list (and an array as an array).
const CAST_UNION_OF_LISTS: &str = r#"
{
    let opt = |x: [List<i64>, null]| cast<Array<i64>>(x);
    let rev = |x: [Array<Array<i64>>, null]| cast<List<Array<i64>>>(x);
    let a = opt([<1, 2, 3>]);
    let t = cast<Array<i64>>(list::tail([<1, 2, 3, 4>]));
    (a$, t$, list::len(rev([[1], []])$))
}
"#;

run!(cast_union_of_lists, CAST_UNION_OF_LISTS, |v: Result<&Value>| {
    let arr = |xs: &[i64]| Value::Array(xs.iter().map(|x| Value::I64(*x)).collect());
    matches!(v, Ok(Value::Array(r))
        if r[0] == arr(&[1, 2, 3]) && r[1] == arr(&[2, 3, 4]) && r[2] == Value::I64(2))
}; FuseExpect::Jit);

// A cast of a constructor-trait parameter is decided by the instance.
const CAST_OF_A_COLLECTION_PARAM: &str = r#"
{
    let f = |c: Collection| cast<Array<i64>>(c);
    f([1, 2, 3])
}
"#;

run!(cast_of_a_collection_param, CAST_OF_A_COLLECTION_PARAM, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a.len() == 3)
}; FuseExpect::Jit);
