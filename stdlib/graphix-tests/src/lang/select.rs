// Tests for select/match expressions

use anyhow::Result;
use graphix_package_core::{run, testing::eval};
use netidx::publisher::Value;

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

// ASPIRE: Jit — string interpolation in a select expression.
run!(select0, SELECT0, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "first 1",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

const SELECTSTRUCT: &str = r#"
{
  type T = { foo: string, bar: f64, baz: f64 };
  let x = { foo: "bar", bar: 42.0, baz: 84.0 };
  select x {
    T as { foo: "foo", bar: 8.0, baz } => baz,
    T as { bar, baz, .. } => bar + baz
  }
}
"#;

// ASPIRE: Jit — nested composite / variant payload composite.
run!(selectstruct, SELECTSTRUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(126.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::None);

const MATCH_EXHAUST1: &str = r#"
select 42 {
    1 => never(),
    2 => never(),
    _ => 42
}
"#;

// The never() arms de-fuse individually; the wildcard arm and scrutinee
// fuse.
run!(match_exhaust1, MATCH_EXHAUST1, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const NESTEDMATCH0: &str = r#"
{
  type T = { foo: (string, f64, f64), bar: i64, baz: f64 };
  let x = { foo: ("bar", 42.0, 5.0), bar: 42, baz: 84.0 };
  let { foo: (_, x, y), .. }: T = x;
  x + y
}
"#;

// ASPIRE: Jit — nested composite / variant payload composite.
run!(nestedmatch0, NESTEDMATCH0, |v: Result<&Value>| match v {
    Ok(Value::F64(47.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const NESTEDMATCH1: &str = r#"
{
  type T = { foo: {x: string, y: f64, z: f64}, bar: i64, baz: f64 };
  let x = { foo: { x: "bar", y: 42.0, z: 5.0 }, bar: 42, baz: 84.0 };
  select x {
    T as { foo: { y, z, .. }, .. } => y + z
  }
}
"#;

// ASPIRE: Jit — nested composite / variant payload composite.
run!(nestedmatch1, NESTEDMATCH1, |v: Result<&Value>| match v {
    Ok(Value::F64(47.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::None);

const NESTEDMATCH3: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  select x {
    { foo: [x, y, ..], bar: _, baz: _ } => x + y,
    _ => never()
  }
}
"#;

// The `_ => never()` arm is async, so this select's region de-fuses;
// sibling regions satisfy Jit.
run!(nestedmatch3, NESTEDMATCH3, |v: Result<&Value>| match v {
    Ok(Value::F64(3.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A missing region input consumed only on a dead arm yields a real
// value, not bottom.
const MISSING_ON_DEAD_ARM_SCALAR: &str = r#"
{ let x: i64 = never(); select i64:0 { i64:0 => i64:5, _ => x } }
"#;

run!(missing_on_dead_arm_scalar, MISSING_ON_DEAD_ARM_SCALAR, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
});

const MISSING_ON_DEAD_ARM_STRING: &str = r#"
{ let x: string = never(); select i64:0 { i64:0 => "live", _ => x } }
"#;

run!(missing_on_dead_arm_string, MISSING_ON_DEAD_ARM_STRING, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "live")
});

// Structural destructuring over a borrowed composite scrutinee (tuple /
// struct / slice patterns with scalar leaves) fuses.

const SELECT_TUPLE_DESTRUCTURE: &str = r#"
{
  let t = (3, 4);
  select t {
    (0, y) => y,
    (x, y) => x + y
  }
}
"#;

run!(select_tuple_destructure, SELECT_TUPLE_DESTRUCTURE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
});

const SELECT_TUPLE_LITERAL_ARM: &str = r#"
{
  let t = (0, 9);
  select t {
    (0, y) => y,
    (x, y) => x + y
  }
}
"#;

run!(select_tuple_literal_arm, SELECT_TUPLE_LITERAL_ARM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(9)))
});

const SELECT_TUPLE_GUARD: &str = r#"
{
  let t = (5, 2);
  select t {
    (x, y) if x > y => x - y,
    (x, y) => y - x
  }
}
"#;

run!(select_tuple_guard, SELECT_TUPLE_GUARD, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
});

const SELECT_STRUCT_DESTRUCTURE: &str = r#"
{
  let p = { x: 0, y: 42 };
  select p {
    { x: 0, y } => y,
    { x, y } => x + y
  }
}
"#;

run!(select_struct_destructure, SELECT_STRUCT_DESTRUCTURE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

const SELECT_SLICE_LEN_DISPATCH: &str = r#"
{
  let a = [10, 20];
  select a {
    [x] => x,
    [x, y] => x + y,
    _ => 0
  }
}
"#;

run!(select_slice_len_dispatch, SELECT_SLICE_LEN_DISPATCH, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(30)))
});

// Wrong-length arms fall through to the catch-all.
const SELECT_SLICE_MISS: &str = r#"
{
  let a = [1, 2, 3, 4];
  select a {
    [x] => x,
    [x, y, z] => x + y + z,
    _ => -1
  }
}
"#;

run!(select_slice_miss, SELECT_SLICE_MISS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-1)))
});

// An anonymous-rest prefix `[x, ..]`.
const SELECT_SLICE_PREFIX: &str = r#"
{
  let a = [7, 8, 9];
  select a {
    [x, ..] => x,
    _ => 0
  }
}
"#;

run!(select_slice_prefix, SELECT_SLICE_PREFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
});

// An anonymous-head suffix `[.., x]` reads `a[len - 1]`.
const SELECT_SLICE_SUFFIX: &str = r#"
{
  let a = [7, 8, 9];
  select a {
    [.., x] => x,
    _ => 0
  }
}
"#;

run!(select_slice_suffix, SELECT_SLICE_SUFFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(9)))
});

// `[]` matches the empty array; the sized arms fall through.
const SELECT_SLICE_EMPTY: &str = r#"
{
  let a: Array<i64> = [];
  select a {
    [x] => x,
    [] => -7,
    _ => 0
  }
}
"#;

run!(select_slice_empty, SELECT_SLICE_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-7)))
});

// A named rest binding de-fuses the select (pinned by
// `native_select_named_rest_defuses`); sibling regions still fuse.
const SELECT_SLICE_NAMED_REST: &str = r#"
{
  let a = [1, 2, 3];
  select a {
    [x, rest..] => x + array::len(rest),
    _ => 0
  }
}
"#;

run!(select_slice_named_rest, SELECT_SLICE_NAMED_REST, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
});

// `[init.., x]` over [7,8,9] binds x=9, init=[7,8] on both engines.
const SELECT_SUFFIX_NAMED_HEAD: &str = r#"
{
  let a = [7, 8, 9];
  select a {
    [init.., x] => x * 100 + array::len(init),
    _ => 0
  }
}
"#;

run!(select_suffix_named_head, SELECT_SUFFIX_NAMED_HEAD, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(902)))
});

// `[.., x, y]` over a 2-element array binds x=1, y=2.
const SELECT_SUFFIX_EXACT_LEN: &str = r#"
{
  let a = [1, 2];
  select a {
    [.., x, y] => x * 10 + y,
    _ => 0
  }
}
"#;

run!(select_suffix_exact_len, SELECT_SUFFIX_EXACT_LEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(12)))
});

// Slice-pattern length coverage: unguarded all-bind slice arms cover an
// array scrutinee when their lengths cover every length.
// ASPIRE: Jit on the lambda-wrapped fixtures — a composite scrutinee in
// an instance kernel; `select_slice_cover_fused` pins the root form.

// The region-root form: a wildcard-less slice-covered select fuses.
const SELECT_SLICE_COVER_FUSED: &str = r#"
{
  let a = [7, 8, 9];
  select a {
    [x, ..] => x,
    [] => -1
  }
}
"#;

run!(select_slice_cover_fused, SELECT_SLICE_COVER_FUSED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
});

const SELECT_SLICE_COVER_SUFFIX: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [init.., last] => last
  };
  f([7, 8, 9]) * 10 + f([])
}
"#;

run!(select_slice_cover_suffix, SELECT_SLICE_COVER_SUFFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(90)))
}; graphix_package_core::testing::FuseExpect::None);

const SELECT_SLICE_COVER_PREFIX: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [x, ..] => x,
    [] => -1
  };
  f([7, 8, 9]) * 10 + f([])
}
"#;

run!(select_slice_cover_prefix, SELECT_SLICE_COVER_PREFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(69)))
}; graphix_package_core::testing::FuseExpect::Jit);

// An exact-length ladder under the rest form.
const SELECT_SLICE_COVER_LADDER: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [a] => a,
    [a, b, rest..] => a + b + array::len(rest)
  };
  f([]) + f([5]) + f([1, 2, 3, 4])
}
"#;

run!(select_slice_cover_ladder, SELECT_SLICE_COVER_LADDER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
}; graphix_package_core::testing::FuseExpect::None);

// The pool covers the array member; the null member needs its own arm.
const SELECT_SLICE_COVER_UNION: &str = r#"
{
  let f = |xs: [Array<i64>, null]| -> i64 select xs {
    null as _ => -1,
    [] => 0,
    [init.., last] => last
  };
  f(null) + f([]) + f([1, 2, 3])
}
"#;

run!(select_slice_cover_union, SELECT_SLICE_COVER_UNION, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; graphix_package_core::testing::FuseExpect::None);

// A hole in the length ladder refuses and the message names it.
const SELECT_SLICE_HOLE: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [a, b, rest..] => a + b
  };
  f([1, 2, 3])
}
"#;

run!(select_slice_hole_rejected, SELECT_SLICE_HOLE, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// Exact-length arms alone never cover every length.
const SELECT_SLICE_NO_REST: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [a] => a
  };
  f([1])
}
"#;

run!(select_slice_no_rest_rejected, SELECT_SLICE_NO_REST, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A guarded arm claims no coverage.
const SELECT_SLICE_GUARDED_REST: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [x, rest..] if x > 0 => x
  };
  f([1])
}
"#;

run!(
    select_slice_guarded_rest_rejected,
    SELECT_SLICE_GUARDED_REST,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// A wildcard behind a complete slice ladder is dead.
const SELECT_SLICE_DEAD_WILDCARD: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [x, ..] => x,
    _ => -1
  };
  f([1])
}
"#;

run!(
    select_slice_dead_wildcard_rejected,
    SELECT_SLICE_DEAD_WILDCARD,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// A slice arm whose whole length range is taken by earlier arms is dead.
const SELECT_SLICE_DEAD_SHADOW: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [x, rest..] => x,
    [init.., y] => y,
    _ => 0
  };
  f([1])
}
"#;

run!(
    select_slice_dead_shadow_rejected,
    SELECT_SLICE_DEAD_SHADOW,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// A trailing wildcard after `true` + `false` is dead.
const SELECT_BOOL_DEAD_WILDCARD: &str = r#"
{
  let f = |x: bool| -> i64 select x {
    true => 1,
    false => 0,
    _ => 2
  };
  f(true)
}
"#;

run!(
    select_bool_dead_wildcard_rejected,
    SELECT_BOOL_DEAD_WILDCARD,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// A partial ladder keeps its wildcard; a refutable-element arm neither
// dies nor blocks the arms below it from completing coverage.
const SELECT_SLICE_PARTIAL_WILDCARD_LIVE: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [x, ..] => x,
    _ => -1
  };
  f([7]) * 10 + f([])
}
"#;

run!(
    select_slice_partial_wildcard_live,
    SELECT_SLICE_PARTIAL_WILDCARD_LIVE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(69))) };
    graphix_package_core::testing::FuseExpect::Jit
);

const SELECT_SLICE_REFUT_THEN_COVER_LIVE: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [0, ..] => -1,
    [x, ..] => x,
    [] => 0
  };
  f([0, 5]) * 100 + f([7]) * 10 + f([])
}
"#;

run!(
    select_slice_refut_then_cover_live,
    SELECT_SLICE_REFUT_THEN_COVER_LIVE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(-30))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A refutable element pattern claims no coverage.
const SELECT_SLICE_REFUTABLE_ELEM: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 select xs {
    [] => 0,
    [0, rest..] => 1
  };
  f([1])
}
"#;

run!(
    select_slice_refutable_elem_rejected,
    SELECT_SLICE_REFUTABLE_ELEM,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// Nested structural select patterns with scalar leaf binds fuse.

const SELECT_NESTED_TUPLE: &str = r#"
{
  let t = ((1, 2), 30);
  select t {
    ((0, b), c) => b + c,
    ((a, b), c) => a + b + c
  }
}
"#;

run!(select_nested_tuple, SELECT_NESTED_TUPLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(33)))
});

// A struct pattern with a nested slice-prefix leaf fuses.
const SELECT_NESTED_STRUCT_SLICE: &str = r#"
{
  let x = { foo: [1.0, 2.0, 4.5], bar: 42, baz: 8.0 };
  select x {
    { foo: [a, b, ..], bar: _, baz: _ } => a + b,
    _ => 0.0
  }
}
"#;

run!(select_nested_struct_slice, SELECT_NESTED_STRUCT_SLICE, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(3.0)))
});

// A literal inside the nested level.
const SELECT_NESTED_LITERAL: &str = r#"
{
  let t = ((7, 2), 5);
  select t {
    ((7, b), c) => b * c,
    ((a, b), c) => a + b + c
  }
}
"#;

run!(select_nested_literal, SELECT_NESTED_LITERAL, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
});

// Owned (fresh-producer) select scrutinees fuse in value position.

// An inline tuple literal scrutinee.
const SELECT_OWNED_TUPLE: &str = r#"
{
  let a = 3;
  select (a, a * 2) {
    (0, y) => y,
    (x, y) => x + y
  }
}
"#;

run!(select_owned_tuple, SELECT_OWNED_TUPLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(9)))
});

// An inlined HOF result as the scrutinee.
const SELECT_OWNED_HOF_RESULT: &str = r#"
{
  let a = [1, 2];
  select array::map(a, |x| x * 10) {
    [x, y] => x + y,
    _ => 0
  }
}
"#;

run!(select_owned_hof_result, SELECT_OWNED_HOF_RESULT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(30)))
});

// An owned variant scrutinee with a scalar payload bind.
const SELECT_OWNED_VARIANT: &str = r#"
{
  let n = 5;
  select `Foo(n + 1) {
    `Foo(x) => x * 2
  }
}
"#;

run!(select_owned_variant, SELECT_OWNED_VARIANT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(12)))
});

// The no-match edge: every length test misses and the catch-all is
// taken.
const SELECT_OWNED_MISS: &str = r#"
{
  let a = [1, 2, 3];
  select array::filter(a, |x| x > 10) {
    [x] => x,
    _ => -1
  }
}
"#;

run!(select_owned_miss, SELECT_OWNED_MISS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-1)))
});

// `_` infers a fresh TVar, so slots after a `_` still narrow their binds.

// `_` before the nested slot in a tuple.
const SELECT_IGNORE_BEFORE_NESTED: &str = r#"
{
  let t = (42, [1.0, 2.0]);
  select t {
    (_, [a, b]) => a + b,
    _ => 0.0
  }
}
"#;

run!(select_ignore_before_nested, SELECT_IGNORE_BEFORE_NESTED, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(3.0)))
});

// A struct parent whose `_` fields sort first.
const SELECT_IGNORE_SORTS_FIRST: &str = r#"
{
  let x = { foo: [1.0, 2.0, 4.5], bar: 42, baz: 8.0 };
  select x {
    { foo: [a, b, ..], bar: _, baz: _ } => a + b,
    _ => 0.0
  }
}
"#;

run!(select_ignore_sorts_first, SELECT_IGNORE_SORTS_FIRST, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(3.0)))
});

// An Array local defined by a never()-gated select threads into the
// downstream fold region as a kernel input (the `#[native]` on the fold
// is the assertion). Final fold = 9.0.
const GATED_WINDOW_FOLD: &str = r#"
{
  let tick = array::iter([1.0, 2.0, 3.0, 4.0]);
  let win: Array<f64> = [];
  win <- array::window(#n: 3, tick ~ win, tick);
  let w = select array::len(win) {
    0 => never(),
    _ => win
  };
  let total = array::fold(w, 0.0, |a, x| a + x);
  select count(total) {
    4 => total,
    _ => never()
  }
}
"#;

run!(gated_window_fold, GATED_WINDOW_FOLD, |v: Result<&Value>| matches!(
    v,
    Ok(Value::F64(9.0))
); graphix_package_core::testing::FuseExpect::Jit);

// A builtin call whose arg is a never()-gated string local fuses.
const GATED_STRING_BUILTIN: &str = r#"
{
  let tick = array::iter([1, 2, 3, 4]);
  let acc = "";
  acc <- tick ~ "[acc]x";
  let s = select str::len(acc) {
    0 => never(),
    _ => acc
  };
  let l = #[native] str::len(s) * 2;
  select count(l) {
    4 => l,
    _ => never()
  }
}
"#;

run!(gated_string_builtin, GATED_STRING_BUILTIN, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(8))
); graphix_package_core::testing::FuseExpect::Jit);

// ASPIRE: Jit — the unannotated scalar gate: the never arm's open cell
// binds wide under the downstream arith. `let m: i64` fuses today.
const GATED_SCALAR_UNANNOTATED: &str = r#"
{
  let c = array::iter([1, 2, 3, 4]);
  let m = select c {
    0 => never(),
    _ => c
  };
  let r = m * 2 + 1;
  select count(r) {
    4 => r,
    _ => never()
  }
}
"#;

run!(gated_scalar_unannotated, GATED_SCALAR_UNANNOTATED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(9))
); graphix_package_core::testing::FuseExpect::Jit);

// A guarded arm before a bind-all final arm is exhaustive.
const GUARDED_ARM_THEN_BINDALL: &str = r#"
{
  let v: [`A(i64), `B] = `A(i64:1);
  select v { `A(x) if x > i64:0 => x, y => i64:0 }
}
"#;

run!(guarded_arm_then_bindall, GUARDED_ARM_THEN_BINDALL, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); graphix_package_core::testing::FuseExpect::Jit);

// A guard must be `bool`.
const GUARD_STRING: &str = r#"
{
  let x = i64:1;
  select x { v if "" => i64:0, _ => i64:1 }
}
"#;

run!(guard_string_rejected, GUARD_STRING, |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None);

const GUARD_INT: &str = r#"
{
  let x = i64:1;
  select x { v if x => i64:0, _ => i64:1 }
}
"#;

run!(guard_int_rejected, GUARD_INT, |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None);

// A nullable bool is not a bool guard either.
const GUARD_NULLABLE_BOOL: &str = r#"
{
  let b: [bool, null] = true;
  select i64:1 { v if b => i64:0, _ => i64:1 }
}
"#;

run!(guard_nullable_bool_rejected, GUARD_NULLABLE_BOOL, |v: Result<&Value>| matches!(
    v,
    Err(_)
); graphix_package_core::testing::FuseExpect::None);

// An unannotated lambda used as a guard infers a bool return.
const GUARD_INFERS_BOOL: &str = r#"
{
  let p = |x| x > i64:0;
  select i64:7 { v if p(v) => i64:0, _ => i64:1 }
}
"#;

run!(guard_infers_bool, GUARD_INFERS_BOOL, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(0))
); graphix_package_core::testing::FuseExpect::Jit);

// The dual shape: the guarded arm names a different tag than the value.
const GUARDED_OTHER_TAG_THEN_BINDALL: &str = r#"
{
  let v: [`A(i64), `B] = `A(i64:7);
  select v { `B if true => i64:1, y => i64:2 }
}
"#;

run!(
    guarded_other_tag_then_bindall,
    GUARDED_OTHER_TAG_THEN_BINDALL,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2)));
    graphix_package_core::testing::FuseExpect::Jit
);

// A select's result union re-collapses once an arm's `$`-result TVar
// binds, so field access on the result typechecks.
const ARM_UNION_TVAR_COLLAPSE: &str = r#"
{
  let v0 = select i64:100 {
    42 => { b: f64:1.0, y: cast<i64>(u8:2)$ },
    _ => { b: f64:0.0, y: i64:42 }
  };
  v0.y
}
"#;

run!(arm_union_tvar_collapse, ARM_UNION_TVAR_COLLAPSE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(42))
); graphix_package_core::testing::FuseExpect::Jit);

// Bind-all arm types narrow by position: `s` after an unguarded
// irrefutable arm cannot be null, so it is `string`.
const BINDALL_NARROWS_BY_POSITION: &str = r#"
{
  let o: [string, null] = "x";
  let n = select o { null as _ => "", s => s };
  str::len(n)
}
"#;

run!(bindall_narrows_by_position, BINDALL_NARROWS_BY_POSITION, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); graphix_package_core::testing::FuseExpect::Jit);

// A variant arm with a payload bind narrows the arms after it: the
// residual reaching `n` cannot be `` `Bad ``.
const VARIANT_PAYLOAD_ARM_NARROWS: &str = r#"
{
  let g = |x: [i64, `Bad(string)]| -> i64 select x {
    `Bad(m) => str::len(m),
    n => n + 1
  };
  g(41) + g(`Bad("xyz"))
}
"#;

run!(variant_payload_arm_narrows, VARIANT_PAYLOAD_ARM_NARROWS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(45))
); graphix_package_core::testing::FuseExpect::None);

// The same through a named member and an ignored payload.
const VARIANT_IGNORED_PAYLOAD_ARM_NARROWS: &str = r#"
{
  type T = [`OnStart, `OnAccess(Array<string>)];
  let p = |s: string| -> [T, `Bad(string)] select s {
    "" => `OnStart,
    "bad" => `Bad("bad"),
    t => `OnAccess([t])
  };
  let g = |s: string| -> [{ t: T }, `Bad(string)] select p(s) {
    `Bad(_) => `Bad("no"),
    t => { t }
  };
  select g("x") {
    `Bad(_) => 0,
    { t: `OnAccess(ps) } => array::len(ps),
    { t: `OnStart } => -1
  }
}
"#;

run!(variant_ignored_payload_arm_narrows, VARIANT_IGNORED_PAYLOAD_ARM_NARROWS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); graphix_package_core::testing::FuseExpect::Jit);

// A guarded select fires only when an input feeding it fired: count 1
// on both engines despite an unrelated reactive input in the region.
const GUARDED_SELECT_FIRING_COUNT: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let sel = select 0 { 0 if true => 42, _ => x };
  let c = count(sel);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(guarded_select_firing_count, GUARDED_SELECT_FIRING_COUNT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A guard-dep fire emits whether or not the selection changes; a guard
// that has never produced bottoms the select. m fires per x delivery: 4.
const GUARDED_SELECT_SELECTION_MEMORY: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x % 2;
  let sel = select 0 { 0 if m == 0 => 1, _ => 2 };
  let c = count(sel);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(guarded_select_selection_memory, GUARDED_SELECT_SELECTION_MEMORY, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(4)))
}; graphix_package_core::testing::FuseExpect::Jit);

// The same inside a collection loop: 4.
const GUARDED_SELECT_IN_LOOP_SELECTION_MEMORY: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let a = array::map([10], |i| select i { 10 if m == 0 => 1, _ => 2 });
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_in_loop_selection_memory,
    GUARDED_SELECT_IN_LOOP_SELECTION_MEMORY,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Two slots with different stable selections both emit per guard fire: 4.
const GUARDED_SELECT_PER_SLOT_INDEPENDENCE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let a = array::map([0, 1], |i| select 0 { 0 if x - x + i == 0 => 1, _ => 2 });
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_per_slot_independence,
    GUARDED_SELECT_PER_SLOT_INDEPENDENCE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Across a source resize (1 -> 2 mid-run): 4.
const GUARDED_SELECT_SLOT_TABLE_RESIZE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let src = [0];
  src <- select count(x) { 2 => [0, 1], _ => never() };
  let a = array::map(src, |i| select 0 { 0 if m == 0 => 1, _ => 2 });
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_slot_table_resize,
    GUARDED_SELECT_SLOT_TABLE_RESIZE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Two loops deep: 4.
const GUARDED_SELECT_NESTED_LOOP_SELECTION_MEMORY: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let a = array::map([0], |i| array::map([0], |j| select j { 0 if m == 0 => 1, _ => 2 }));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_nested_loop_selection_memory,
    GUARDED_SELECT_NESTED_LOOP_SELECTION_MEMORY,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Four slot pairs with different stable selections: 4.
const GUARDED_SELECT_NESTED_PER_PAIR_INDEPENDENCE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let a = array::map([0, 1], |i| array::map([0, 1], |j| select 0 { 0 if (i + j + x - x) % 2 == 0 => 1, _ => 2 }));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_nested_per_pair_independence,
    GUARDED_SELECT_NESTED_PER_PAIR_INDEPENDENCE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Ragged inner lengths plus an outer resize mid-run: 4.
const GUARDED_SELECT_NESTED_RAGGED_RESIZE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let src = [[10]];
  src <- select count(x) { 2 => [[10], [20, 30]], _ => never() };
  let a = array::map(src, |ys| array::map(ys, |y| select 0 { 0 if m == 0 => 1, _ => 2 }));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_nested_ragged_resize,
    GUARDED_SELECT_NESTED_RAGGED_RESIZE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Loop depth 3: 4.
const GUARDED_SELECT_TRIPLE_NESTED: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let a = array::map([0], |i| array::map([0], |j| array::map([0], |k| select k { 0 if m == 0 => 1, _ => 2 })));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_triple_nested,
    GUARDED_SELECT_TRIPLE_NESTED,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// In a callee body: 5 (init + 4).
const GUARDED_SELECT_IN_CALLEE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let f = |k| select k { 0 if m == 0 => 1, _ => 2 };
  let c = count(f(0));
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(guarded_select_in_callee, GUARDED_SELECT_IN_CALLEE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(4)))
}; graphix_package_core::testing::FuseExpect::Jit);

// One callee at two call sites with different stable selections:
// 44 = 4*10 + 4.
const GUARDED_SELECT_CALLEE_TWO_SITES: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let f = |k| select 0 { 0 if (x - x + k) % 2 == 0 => 1, _ => 2 };
  let a = f(0);
  let b = f(1);
  let c = count(a) * 10 + count(b);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_callee_two_sites,
    GUARDED_SELECT_CALLEE_TWO_SITES,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(44))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A callee called inside a loop: 4.
const GUARDED_SELECT_CALLEE_IN_LOOP: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let f = |k| select 0 { 0 if (x - x + k) % 2 == 0 => 1, _ => 2 };
  let a = array::map([0, 1], |i| f(i));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_callee_in_loop,
    GUARDED_SELECT_CALLEE_IN_LOOP,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A callee whose own body has a loop-select: 4.
const GUARDED_SELECT_CALLEE_INTERNAL_LOOP: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let f = |n| array::map([n], |i| select i { 0 if m == 0 => 1, _ => 2 });
  let c = count(f(0));
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_callee_internal_loop,
    GUARDED_SELECT_CALLEE_INTERNAL_LOOP,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A callee with an internal loop-select, called from inside a loop: 4.
const GUARDED_SELECT_CALLEE_LOOP_IN_LOOP: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let f = |n| array::map([n], |i| select 0 { 0 if m == 0 => 1, _ => 2 });
  let a = array::map([0, 1], |j| f(j));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_callee_loop_in_loop,
    GUARDED_SELECT_CALLEE_LOOP_IN_LOOP,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Inside a tail-recursive callee: 4.
const GUARDED_SELECT_IN_TAIL_RECURSIVE_CALLEE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let rec f = |n| select n {
    0 => select 0 { 0 if m == 0 => 1, _ => 2 },
    _ => f(n - 1)
  };
  let c = count(f(3));
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guarded_select_in_tail_recursive_callee,
    GUARDED_SELECT_IN_TAIL_RECURSIVE_CALLEE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// An arm-local `<-` target persists across the arm's sleep: a wake
// resumes the arm, so the written 11 survives.
// findings/arm-local-bind-aug2026/
const SELECT_ARM_LOCAL_PERSISTS: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x % 2;
  let sel = select m {
    0 => 0,
    _ => { let s = 10; s <- (x ~ s) + 1; s }
  };
  array::group(sel, |n, _| n == 4)
}
"#;

run!(select_arm_local_persists, SELECT_ARM_LOCAL_PERSISTS, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            a.iter().map(|v| v.clone().cast_to::<i64>().unwrap()).collect::<Vec<_>>()
                == vec![10, 0, 11, 0]
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// Same shape with the connect RHS computed by a fold: re-entry sees 6.
const SELECT_ARM_LOCAL_PERSISTS_FOLD: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x % 2;
  let sel = select m {
    0 => 100,
    _ => { let s = 0; s <- array::fold([1, 2, 3], 0, |acc, e| acc + e); s }
  };
  array::group(sel, |n, _| n == 4)
}
"#;

run!(select_arm_local_persists_fold, SELECT_ARM_LOCAL_PERSISTS_FOLD, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            a.iter().map(|v| v.clone().cast_to::<i64>().unwrap()).collect::<Vec<_>>()
                == vec![0, 100, 6, 100]
        }
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// A guard reading a capture inside a rec callee's tail select emits per
// m fire; the init-phantom guard bottoms init: [1, 1, 2].
const TAIL_SELECT_GUARD_CAPTURE_MEMORY: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let rec f = |n| select n {
    0 => select 0 { 0 if m == 0 => 1, _ => 2 },
    _ => f(n - 1)
  };
  array::group(f(3), |n, _| n == 3)
}
"#;

run!(
    tail_select_guard_capture_memory,
    TAIL_SELECT_GUARD_CAPTURE_MEMORY,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                a.iter().map(|v| v.clone().cast_to::<i64>().unwrap()).collect::<Vec<_>>()
                    == vec![1, 1, 2]
            }
            _ => false,
        }
    };
    graphix_package_core::testing::FuseExpect::Jit
);

// A guard flip wakes a catch-all arm whose fold callback reads only the
// captured scrutinee bind: the becoming-selected fire emits 1.
const ARM_WAKE_CAPTURE_ONLY_CALLBACK: &str = r#"
{
  let k = true;
  k <- false;
  let r = select 1 {
    _ if k => 42,
    v3 => array::fold([1], -100, |v4, v5| v3)
  };
  array::group(r, |n, _| n == 2)
}
"#;

run!(
    arm_wake_capture_only_callback,
    ARM_WAKE_CAPTURE_ONLY_CALLBACK,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                a.iter().map(|v| v.clone().cast_to::<i64>().unwrap()).collect::<Vec<_>>()
                    == vec![42, 1]
            }
            _ => false,
        }
    };
    graphix_package_core::testing::FuseExpect::Jit
);

const SELECT_GUARD_AFTER_TAINTED_INIT: &str = r#"
{
  let rec f = |n: i64| -> i64 select n {
    m if m <= i64:0 => (m / m),
    m => f(m - i64:1)
  };
  let v = f(i64:1);
  let r = &v;
  *r <- i64:1;
  select v {
    x if true => i64:200,
    x => x
  }
}
"#;

run!(
    select_guard_after_tainted_init,
    SELECT_GUARD_AFTER_TAINTED_INIT,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(200)));
    graphix_package_core::testing::FuseExpect::Jit
);

// A pattern's inferred type predicate over a recursive type must not
// refuse a value because an earlier union member already walked it.
// A regression here wedges; the harness timeout is the failure.
const SELECT_RECURSIVE_TYPE_TUPLE_ARMS: &str = r#"
{
  type T = [`N(f64), `A(T, T)];
  let id = |e: T| -> T e;
  let v = `A(`N(1.0), `N(2.0));
  select (id(v), id(v)) {
    (`N(x), `N(y)) => "nn",
    (a, b) => "other"
  }
}
"#;

run!(
    select_recursive_type_tuple_arms,
    SELECT_RECURSIVE_TYPE_TUPLE_ARMS,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "other");
    graphix_package_core::testing::FuseExpect::Jit
);

// The shape it was found in: two recursive functions over a recursive
// ADT, where the second's select re-checks nodes the first walked.
const SELECT_RECURSIVE_ADT_CHAIN: &str = r#"
{
  type T = [`N(f64), `V, `A(T, T), `M(T, T)];
  let rec g = |e: T| -> T select e {
    `N(_) => `N(0.0),
    `V => `N(1.0),
    `A(a, b) => `A(g(a), g(b)),
    `M(a, b) => `A(`M(g(a), b), `M(a, g(b)))
  };
  let rec f = |e: T| -> T select e {
    `N(x) => `N(x),
    `V => `V,
    `A(a, b) => select (f(a), f(b)) {
      (`N(x), `N(y)) => `N(x + y),
      (sa, sb) => `A(sa, sb)
    },
    `M(a, b) => select (f(a), f(b)) {
      (`N(x), `N(y)) => `N(x * y),
      (sa, sb) => `M(sa, sb)
    }
  };
  f(g(`M(`M(`A(`V, `N(1.0)), `V), `V)))
}
"#;

run!(
    select_recursive_adt_chain,
    SELECT_RECURSIVE_ADT_CHAIN,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(_)));
    graphix_package_core::testing::FuseExpect::Jit
);

// A partial struct pattern `{x, ..}` completes from the scrutinee.

const SELECT_PARTIAL_STRUCT: &str = r#"
{
  type S = { x: i64, y: string };
  let v: S = { x: 1, y: "z" };
  select v { { x, .. } => x }
}
"#;

run!(
    select_partial_struct,
    SELECT_PARTIAL_STRUCT,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1)));
    graphix_package_core::testing::FuseExpect::Jit
);

// `y` names the second field of the member; the binder reads "z".
const SELECT_PARTIAL_IN_VARIANT: &str = r#"
{
  type E = [`A({ x: i64, y: string }), `B({ x: i64, z: i64 }), `C];
  let v: E = `A({ x: 1, y: "z" });
  select v {
    `A({ y, .. }) => y,
    `B({ z, .. }) => "[z]",
    `C => "c"
  }
}
"#;

run!(
    select_partial_in_variant,
    SELECT_PARTIAL_IN_VARIANT,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "z");
    graphix_package_core::testing::FuseExpect::Jit
);

const SELECT_PARTIAL_UNION_MEMBER: &str = r#"
{
  type S = { x: i64, y: string };
  let v: [S, i64] = { x: 1, y: "z" };
  select v { { x, .. } => x, i64 as n => n }
}
"#;

run!(
    select_partial_union_member,
    SELECT_PARTIAL_UNION_MEMBER,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1)));
    graphix_package_core::testing::FuseExpect::Jit
);

// A partial matching several union members must be annotated.
const SELECT_PARTIAL_AMBIGUOUS: &str = r#"
{
  type S = { x: i64, y: string };
  type T = { x: i64, z: i64 };
  let v: [S, T] = { x: 1, y: "a" };
  select v { { x, .. } => x }
}
"#;

run!(
    select_partial_ambiguous_refused,
    SELECT_PARTIAL_AMBIGUOUS,
    |v: Result<&Value>| {
        matches!(&v, Err(e) if format!("{e:#}").contains("matches more than one member"))
    };
    graphix_package_core::testing::FuseExpect::None
);

// An explicit predicate on a Rust-backed abstract type is a nominal tag
// test answered by the wrapper UUID its package registered.
const SELECT_ABSTRACT_PREDICATE: &str = r#"
{
  let td: [sys::fs::tempdir::T, i64] = sys::fs::tempdir::create(null)?;
  select td {
    sys::fs::tempdir::T as _ => 1,
    i64 as _ => 0
  }
}
"#;

run!(
    select_abstract_predicate,
    SELECT_ABSTRACT_PREDICATE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(1))) };
    graphix_package_core::testing::FuseExpect::None
);

// A union-typed arm plus a never() arm returns the declared union.
const SELECT_UNION_RETURN_NEVER_ARM: &str = r#"
{
  type Ev<'a> = [`Q(i64), `Done(['a, `E(string)])];
  let f = |e: Ev<'r>| -> ['r, `E(string)] select e {
    `Done(r) => r,
    _ => never()
  };
  f(`Done(42))
}
"#;

run!(
    select_union_return_never_arm,
    SELECT_UNION_RETURN_NEVER_ARM,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42)));
    graphix_package_core::testing::FuseExpect::None
);

// The bare-cell face: the produced union carries the signature's own
// `'r` cell as a member.
const SELECT_UNION_PARAM_NEVER_ARM: &str = r#"
{
  let f = |x: ['r, i64]| -> ['r, i64] select 0 { 0 => x, _ => never() };
  f("a")
}
"#;

run!(
    select_union_param_never_arm,
    SELECT_UNION_PARAM_NEVER_ARM,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a");
    graphix_package_core::testing::FuseExpect::Jit
);

// A select's type is the union of its arm types; a free tvar arm beside
// an `i64` arm stays free, whether the `i64` is a literal or arrives
// through a bound tvar.
#[tokio::test]
async fn free_union_arm_is_not_inferred_from_sibling() {
    for code in [
        r#"{let y = i64:0; select i64:1 {i64:1 => str::parse("42")?, _ => y}}"#,
        r#"{let y = array::iter([i64:0, i64:2]); let m = array::map([i64:1], |x| select i64:1 {i64:1 => str::parse("42")?, _ => y}); m}"#,
    ] {
        let r = eval(code, crate::TEST_REGISTER).await;
        assert!(
            r.is_err(),
            "parse's result must not be inferred from a sibling arm: {code} -> {:?}",
            r.map(|(v, _)| v)
        );
    }
}

// The annotation types the union: `i64 ⊇ ['b, i64]` binds `'b`.
const UNION_ARM_ANNOTATED: &str = r#"{let y = i64:0; let v: i64 = select i64:1 {i64:1 => str::parse("42")?, _ => y}; v}"#;
run!(union_arm_annotated, UNION_ARM_ANNOTATED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(42))
));

// Shallow arm discriminators: an ambiguous union (same tag, same arity)
// stays on the deep walk; a mixed union dispatches the same shallow.
// Union type-test dispatch interprets, hence None.

run!(
    shallow_ambiguous_same_tag_union,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(21))),
    "/test.gx" => r#"
        type E = [`A(i64), `A(string)];
        let pick = |v: E| -> i64 select v {
            `A(i64) as `A(x) => x * 10,
            `A(string) as `A(s) => str::len(s)
        };
        let result = pick(`A(2)) + pick(`A("x"))
    "#;
    graphix_package_core::testing::FuseExpect::None
);

run!(
    shallow_mixed_union_dispatch,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(3241))),
    "/test.gx" => r#"
        type V = [`Pair(i64, i64), `One(i64), `Nil, string, Array<i64>];
        let score = |v: V| -> i64 select v {
            `Pair(a, b) => a + b,
            `One(x) => x * 10,
            `Nil => 1000,
            string as s => str::len(s) * 100,
            Array<i64> as a => array::len(a) * 1000
        };
        let result = score(`Pair(20, 1)) + score(`One(2))
            + score(`Nil) + score("xx") + score([1, 2])
    "#;
    graphix_package_core::testing::FuseExpect::None
);

// Or-patterns

const OR_LITERALS: &str = r#"
select 2 { 1 | 2 | 3 => "small", _ => "big" }
"#;

run!(or_literals, OR_LITERALS, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "small")
}; graphix_package_core::testing::FuseExpect::Jit);

const OR_TUPLE_BINDS: &str = r#"
select (0, 5) { (0, y) | (y, 0) => y, _ => 0 - 1 }
"#;

run!(or_tuple_binds, OR_TUPLE_BINDS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
});

const OR_GUARD: &str = r#"
select (5, 0) { (0, y) | (y, 0) if y > 3 => y, _ => 0 - 1 }
"#;

run!(or_guard, OR_GUARD, |v: Result<&Value>| { matches!(v, Ok(Value::I64(5))) });

// An or-arm covers its alternatives' variants without a wildcard.
const OR_VARIANT_EXHAUST: &str = r#"
{
  let v: [`A(i64), `B(i64), `C] = `B(7);
  select v { `A(x) | `B(x) => x, `C => 0 }
}
"#;

run!(or_variant_exhaust, OR_VARIANT_EXHAUST, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
});

// Alternatives try left to right: (1, 2) binds the first (y = 2).
const OR_FIRST_MATCH: &str = r#"
select (1, 2) { (1, y) | (y, 2) => y, _ => 0 }
"#;

run!(or_first_match, OR_FIRST_MATCH, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
});

const OR_NESTED: &str = r#"
select `A(2) { `A(1 | 2) => "y", _ => "n" }
"#;

run!(or_nested, OR_NESTED, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "y")
});

// `[] | [_, ..]` covers every length; the bound spelling `[] | [x, r..]`
// is ill-typed by same-binds.
const OR_SLICE_LADDER: &str = r#"
{
  let a = [1, 2, 3];
  select a { [] | [_, ..] => "ok" }
}
"#;

run!(or_slice_ladder, OR_SLICE_LADDER, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "ok")
});

// A different alternative matches on different cycles (1 + 2 + 3 = 6).
const OR_REACTIVE: &str = r#"
{
  let x = array::iter([(0, 1), (2, 0), (0, 3)]);
  let y = select x { (0, y) | (y, 0) => y, _ => 0 - 1 };
  let s = 0;
  s <- y ~ (s + y);
  filter(s, |v| v == 6)
}
"#;

run!(or_reactive, OR_REACTIVE, |v: Result<&Value>| { matches!(v, Ok(Value::I64(6))) });

// Every alternative must bind the same names.
const OR_SAME_BINDS_ERR: &str = r#"
select 1 { 1 | x => 0, _ => 1 }
"#;

run!(or_same_binds_err, OR_SAME_BINDS_ERR, |v: Result<&Value>| v.is_err();
 graphix_package_core::testing::FuseExpect::None);

// Payload binds must have exactly equal types across alternatives.
const OR_EQUAL_TYPES_ERR: &str = r#"
select (1, "a") { (1, y) | (y, "b") => 1, _ => 0 }
"#;

run!(or_equal_types_err, OR_EQUAL_TYPES_ERR, |v: Result<&Value>| v.is_err();
 graphix_package_core::testing::FuseExpect::None);

// Dead alternatives are errors, like dead arms.
const OR_DUP_ALT_ERR: &str = r#"
select 1 { 1 | 1 => 0, _ => 2 }
"#;

run!(or_dup_alt_err, OR_DUP_ALT_ERR, |v: Result<&Value>| v.is_err();
 graphix_package_core::testing::FuseExpect::None);

const OR_DEAD_ALT_ERR: &str = r#"
select 1 { _ | 1 => 0 }
"#;

run!(or_dead_alt_err, OR_DEAD_ALT_ERR, |v: Result<&Value>| v.is_err();
 graphix_package_core::testing::FuseExpect::None);

// Zero-residue: the whole or-select compiles native.
const OR_NATIVE: &str = r#"
{
  let p = (1, 2);
  #[native] select p { (1, y) | (y, 2) if y > 0 => y, _ => 0 }
}
"#;

run!(or_native, OR_NATIVE, |v: Result<&Value>| { matches!(v, Ok(Value::I64(2))) };
graphix_package_core::testing::FuseExpect::Jit);

// Owned binds through the or-chain are dropped at arm exit.
const OR_OWNED_BINDS: &str = r#"
{
  let v: [`A(Array<i64>), `B(Array<i64>), `C] = `B([1, 2, 3]);
  #[native] select v { `A(xs) | `B(xs) => array::len(xs), `C => 0 }
}
"#;

run!(or_owned_binds, OR_OWNED_BINDS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A calling guard on an or-arm runs the guard prologue; on a `C
// scrutinee no alternative matches and the guard is not consulted.
const OR_GUARD_PROLOGUE: &str = r#"
{
  let lim = 2;
  let v: [`A(Array<i64>), `B(Array<i64>), `C] = `C;
  select v { `A(xs) | `B(xs) if array::len(xs) > lim => 1, _ => 0 }
}
"#;

run!(or_guard_prologue, OR_GUARD_PROLOGUE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(0)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A scrutinee member whose variant payload is a union is exhausted by
// per-member arms: `[`P(A), `P(B)]` covers `P([A, B])`; fuses.
const SELECT_VARIANT_UNION_PAYLOAD: &str = r#"
{
  type Panel = [`Q, `D, `R];
  type Screen = [`Connect, `Panel(Panel)];
  let s: Screen = `Panel(`D);
  select s {
    `Connect => 0,
    `Panel(`Q) => 1,
    `Panel(`D) => 2,
    `Panel(`R) => 3
  }
}
"#;

run!(
    select_variant_union_payload_exhausts,
    SELECT_VARIANT_UNION_PAYLOAD,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(2))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// Multi-argument distribution pools through one position: rectangular
// arms cover.
const SELECT_VARIANT_UNION_RECT: &str = r#"
{
  type T = [`P([`A, `B], [`X, `Y]), `N];
  let v: T = `P(`B, `Y);
  select v {
    `P(`A, x) => 1,
    `P(`B, x) => 2,
    `N => 0
  }
}
"#;

run!(
    select_variant_union_rect_exhausts,
    SELECT_VARIANT_UNION_RECT,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(2))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// A diagonal arm set claims nothing: `P(`A, `Y) matches neither arm.
const SELECT_VARIANT_UNION_DIAGONAL: &str = r#"
{
  type T = [`P([`A, `B], [`X, `Y]), `N];
  let v: T = `P(`A, `X);
  select v {
    `P(`A, `X) => 1,
    `P(`B, `Y) => 2,
    `N => 0
  }
}
"#;

run!(
    select_variant_union_diagonal_rejected,
    SELECT_VARIANT_UNION_DIAGONAL,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// The same through a tuple head.
const SELECT_TUPLE_UNION_MEMBER: &str = r#"
{
  let v: [([`A, `B], i64), null] = (`B, 7);
  select v {
    null as _ => 0,
    (`A, n) => n + 1,
    (`B, n) => n + 2
  }
}
"#;

run!(
    select_tuple_union_member_exhausts,
    SELECT_TUPLE_UNION_MEMBER,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(9))) };
    graphix_package_core::testing::FuseExpect::Jit
);

// An @-capture in an or-arm types as the union of its per-alternative
// narrowed types.
const OR_CAPTURE_UNION: &str = r#"
{
  let sel = 0;
  let go = |c: [`Up, `Down, `Char(string), `Enter]| -> string select c {
    kk@ `Up | kk@ `Char("k") => {
      sel <- (kk ~ sel) - 1;
      "up"
    },
    kk@ `Down | kk@ `Char("j") => {
      sel <- (kk ~ sel) + 1;
      "down"
    },
    _ => "other"
  };
  go(`Char("k"))
}
"#;

run!(or_capture_union, OR_CAPTURE_UNION, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "up")
}; graphix_package_core::testing::FuseExpect::Jit);

// Payload binds keep the exactly-equal rule.
const OR_PAYLOAD_UNEQUAL: &str = r#"
{
  let v: (i64, string) = (0, "x");
  select v {
    (0, y) | (y, "z") => "[y]",
    _ => "other"
  }
}
"#;

run!(or_payload_unequal_rejected, OR_PAYLOAD_UNEQUAL, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// An enclosing select's pattern binds are facets of one delivery: arm 0
// handles the key through `ev`, so `k` is spent and the flip to arm 1
// does not re-raise it.
const SELECT_SIBLING_BINDS_SPENT: &str = r#"
{
  type Ev = [`Key([`Enter, `Other]), `Mouse];
  let screen = 0;
  let fired = 0;
  let seen = 0;
  let keys = |k: [`Enter, `Other]| -> null select k {
    kk@ `Enter => { fired <- (kk ~ fired) + 1; null },
    `Other => null
  };
  let landing = |e: Ev| -> null select e {
    `Key(k) => select k {
      kk@ `Enter => { seen <- (kk ~ seen) + 1; null },
      `Other => null
    },
    `Mouse => null
  };
  let handle = |e: Ev| -> null select e {
    ev@ `Key(k) => select screen {
      0 => landing(ev),
      _ => keys(k)
    },
    `Mouse => null
  };
  let e: Ev = never();
  let out = handle(e);
  let t1 = sys::time::timer(duration:0.05s, false);
  e <- t1 ~ `Key(`Enter);
  let t2 = sys::time::timer(duration:0.15s, false);
  screen <- t2 ~ 1;
  let t3 = sys::time::timer(duration:0.3s, false);
  t3 ~ (screen, seen, fired)
}
"#;

run!(
    select_sibling_binds_spent,
    SELECT_SIBLING_BINDS_SPENT,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) =>
            a[0] == Value::I64(1) && a[1] == Value::I64(1) && a[2] == Value::I64(0),
        _ => false,
    };
    graphix_package_core::testing::FuseExpect::Jit
);

// A select over an optional callback `[fn(..), null]` compiles: the
// bind arm can match the function member.
const SELECT_OPTIONAL_FN: &str = r#"
{
  let f: [fn(x: i64) -> i64, null] = |x: i64| x + 1;
  select f {
    null as _ => 0,
    g => g(41)
  }
}
"#;

#[tokio::test]
async fn select_optional_fn_member_matches() -> Result<()> {
    let (v, _ctx) = eval(SELECT_OPTIONAL_FN, crate::TEST_REGISTER).await?;
    assert_eq!(v, Value::I64(42));
    Ok(())
}

// Bool literals pool coverage per position inside a composite pattern.
const BOOL_PAIR_LADDER: &str = r#"
{
  let f = |a: bool, b: bool| -> i64 select (a, b) {
    (true, true) => 3,
    (true, false) => 2,
    (false, _) => 1
  };
  f(true, true) * 100 + f(true, false) * 10 + f(false, true)
}
"#;

run!(bool_pair_ladder_covers, BOOL_PAIR_LADDER, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(321))
); graphix_package_core::testing::FuseExpect::Jit);

const VARIANT_BOOL_LADDER: &str = r#"
{
  let g = |x: [`Join(bool), `Other]| -> i64 select x {
    `Join(true) => 1,
    `Join(false) => 0,
    `Other => -1
  };
  g(`Join(true)) * 100 + g(`Join(false)) * 10 + g(`Other)
}
"#;

run!(variant_bool_ladder_covers, VARIANT_BOOL_LADDER, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(99))
); graphix_package_core::testing::FuseExpect::None);

// A wildcard behind a complete bool ladder is dead.
const BOOL_PAIR_LADDER_DEAD_TAIL: &str = r#"
{
  let f = |a: bool, b: bool| -> i64 select (a, b) {
    (true, true) => 3,
    (true, false) => 2,
    (false, _) => 1,
    _ => 0
  };
  f(true, true)
}
"#;

run!(bool_pair_ladder_dead_tail, BOOL_PAIR_LADDER_DEAD_TAIL, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A destructuring `let`'s siblings are facets of one delivery: arm 0
// handles the pair through `a`, so `b` is spent.
const LET_SIBLING_BINDS_SPENT: &str = r#"
{
  let screen = 0;
  let seen = 0;
  let fired = 0;
  let pair: (i64, i64) = never();
  let (a, b) = pair;
  select screen {
    0 => seen <- a ~ (seen + 1),
    _ => fired <- b ~ (fired + 1)
  };
  let t1 = sys::time::timer(duration:0.05s, false);
  pair <- t1 ~ (1, 2);
  let t2 = sys::time::timer(duration:0.15s, false);
  screen <- t2 ~ 1;
  let t3 = sys::time::timer(duration:0.3s, false);
  t3 ~ (screen, seen, fired)
}
"#;

run!(
    let_sibling_binds_spent,
    LET_SIBLING_BINDS_SPENT,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) =>
            a[0] == Value::I64(1) && a[1] == Value::I64(1) && a[2] == Value::I64(0),
        _ => false,
    };
    graphix_package_core::testing::FuseExpect::Jit
);

// Nested never() arms absorb at typecheck0: the type test below is
// exhaustive only if `u` is exactly `string`.
const NEVER_ARMS_ABSORB: &str = r#"
{
  let m: [string, null] = "b";
  let u = select 1 {
    1 => select m { null as _ => never(), at => at },
    _ => never()
  };
  select u { string as s => str::len(s) }
}
"#;

run!(never_arms_absorb, NEVER_ARMS_ABSORB, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); graphix_package_core::testing::FuseExpect::Jit);

// `never<T>()` carries `T` where nothing else fixes the type.
const NEVER_TYPED: &str = r#"
{
  let p = never<i64>();
  let s = select 2 { 1 => p, _ => 5 };
  s + 1
}
"#;

run!(never_typed, NEVER_TYPED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); graphix_package_core::testing::FuseExpect::Jit);

// never's arguments stay live: a connect inside them keeps writing.
const NEVER_ARGS_LIVE: &str = r#"
{
  let x = 0;
  x <- select x { n if n < 3 => n + 1, _ => never() };
  let seen = 0;
  never(seen <- x);
  select seen { 3 => seen, _ => never() }
}
"#;

run!(never_args_live, NEVER_ARGS_LIVE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(3))
); graphix_package_core::testing::FuseExpect::Jit);

// A sampled write in an arm keeps the sample as its trigger: `x` is 30
// at step 6.
const ARM_SAMPLED_WRITE_KEEPS_TRIGGER: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let mode: [`On, `Off] = `On;
  mode <- select step { 3 => `Off, _ => never() };
  let x = 0;
  select mode { `On => x <- step ~ (step * 10), `Off => never() };
  select step { 6 => x, _ => never() }
}
"#;

run!(arm_sampled_write_keeps_trigger, ARM_SAMPLED_WRITE_KEEPS_TRIGGER, |v: Result<&Value>| match v {
    Ok(Value::I64(30)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// An ungated write is not sampled on the scrutinee: three `Go`
// deliveries write the sampled counter three times, the constant once.
const ARM_UNGATED_CONST_WRITES_ONCE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 6 => s + 1, _ => never() };
  let k = select step { 1 | 2 | 3 => `Go, _ => never() };
  let sampled = 0;
  let once = 0;
  select k {
    `Go => {
      sampled <- k ~ (sampled + 1);
      once <- 1
    }
  };
  select step { 6 => (sampled, once), _ => never() }
}
"#;

run!(arm_ungated_const_writes_once, ARM_UNGATED_CONST_WRITES_ONCE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(3), Value::I64(1)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const HANDLER_WRITE_ON_ERROR_ONLY: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 3 => s + 1, _ => never() };
  let err: [`None, `Bad] = `None;
  let x = { catch(e) err <- e ~ `Bad; select step { 2 => error(`E)?, _ => 0 } };
  select step { 3 => (err, x), _ => never() }
}
"#;

run!(handler_write_on_error_only, HANDLER_WRITE_ON_ERROR_ONLY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::String(t), _] if &**t == "Bad"),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// `~` banks triggers that find `v` absent (three writes); `~!` drops
// them (one write).
const STRICT_SAMPLE_NO_BANK: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 12 => s + 1, _ => never() };
  let t = select step { 1 | 2 | 3 | 6 => step, _ => never() };
  let v = never<string>();
  v <- select step { 5 => "v", _ => never() };
  let banked = 0;
  banked <- (t ~ v) ~ (banked + 1);
  let strict = 0;
  strict <- (t ~! v) ~ (strict + 1);
  select step { 12 => (banked, strict), _ => never() }
}
"#;

run!(strict_sample_no_bank, STRICT_SAMPLE_NO_BANK, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(4), Value::I64(1)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A write arm under a scrutinee that starts as `never()` wakes on the
// first delivery and the nested write fires.
const ARM_WRITE_FROM_NEVER: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 4 => s + 1, _ => never() };
  type Act = [`Go, `Skip];
  let chosen = never<Act>();
  chosen <- select step { 2 => `Go, _ => never() };
  let confirm: [{message: string, action: Act}, null] = null;
  let text = |a: Act| -> [string, null] select a {
    `Go => "Remove this install?",
    _ => null
  };
  select chosen {
    a => select text(a) {
      null as _ => never(),
      m => confirm <- { message: m, action: a }
    }
  };
  select step {
    4 => select confirm { null as _ => "", c => c.message },
    _ => never()
  }
}
"#;

run!(arm_write_from_never, ARM_WRITE_FROM_NEVER, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "Remove this install?",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A pure (skip-sleep) arm over a delayed scrutinee computes on first
// take.
const SKIP_SLEEP_ARM_COMPUTES_ON_FIRST_TAKE: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 4 => s + 1, _ => never() };
  let detected: [string, null] = null;
  detected <- select step { 2 => "CA", _ => never() };
  let title = select detected {
    null as _ => "",
    d => str::concat(d, " (tls)")
  };
  select step { 4 => title, _ => never() }
}
"#;

run!(skip_sleep_arm_computes_on_first_take, SKIP_SLEEP_ARM_COMPUTES_ON_FIRST_TAKE, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => &**s == "CA (tls)",
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);
