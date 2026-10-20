// Tests for select/match expressions

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, as_i64s, eval, is, prints, refused},
};
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

run!(select0, SELECT0, prints(r#""first 1""#); FuseExpect::Jit);

const LOOPING_SELECT: &str = r#"
{
  let v: [i64, string, error] = "1";
  let v = select v {
    i64 as i => i,
    string as s => v <- cast<i64>(s),
    error as e => never(e)
  };
  v + 1
}
"#;

run!(looping_select, LOOPING_SELECT, is(Value::I64(2)); FuseExpect::Jit);

const SELECTSTRUCT: &str = r#"
{
  type T = { foo: string, bar: f64, baz: f64 };
  let x = { foo: "bar", bar: 42.0, baz: 84.0 };
  #[native] select x {
    T as { foo: "foo", bar: 8.0, baz } => baz,
    T as { bar, baz, .. } => bar + baz
  }
}
"#;

run!(selectstruct, SELECTSTRUCT, is(Value::F64(126.0)); FuseExpect::Jit);

const MATCH_EXHAUST0: &str = r#"
select 42 {
    1 => never(),
    2 => never(),
    5 => never()
}
"#;

run!(match_exhaust0, MATCH_EXHAUST0, refused("missing match cases"); FuseExpect::None);

/// Coverage expands an alias chain however deep it goes.
const DEEP_ALIAS_COVERAGE: &str = r#"
{
    type W<'a> = [`L, 'a];
    let f = |v: W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<`Z>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>, b: bool| select (v, b) {
        (`L, true) => 1,
        (`Z, true) => 3,
        (`L, false) => 2
    };
    f(`Z, false)
}
"#;

run!(deep_alias_coverage, DEEP_ALIAS_COVERAGE, refused("missing match cases"); FuseExpect::None);

const MATCH_EXHAUST1: &str = r#"
#[native] select 42 {
    1 => never(),
    2 => never(),
    _ => 42
}
"#;

run!(match_exhaust1, MATCH_EXHAUST1, is(Value::I64(42)); FuseExpect::Jit);

const NESTEDMATCH0: &str = r#"
{
  type T = { foo: (string, f64, f64), bar: i64, baz: f64 };
  let x = { foo: ("bar", 42.0, 5.0), bar: 42, baz: 84.0 };
  let { foo: (_, x, y), .. }: T = x;
  x + y
}
"#;

run!(nestedmatch0, NESTEDMATCH0, is(Value::F64(47.0)); FuseExpect::Jit);

// A partial pattern annotated through an alias of an alias expands the
// whole chain to the struct.
const NESTEDMATCH0_ALIAS: &str = r#"
{
  type T = { foo: (string, f64, f64), bar: i64, baz: f64 };
  type U = T;
  let x = { foo: ("bar", 42.0, 5.0), bar: 42, baz: 84.0 };
  let { foo: (_, x, y), .. }: U = x;
  x + y
}
"#;

run!(nestedmatch0_alias, NESTEDMATCH0_ALIAS, is(Value::F64(47.0)); FuseExpect::Jit);

const NESTEDMATCH1: &str = r#"
{
  type T = { foo: {x: string, y: f64, z: f64}, bar: i64, baz: f64 };
  let x = { foo: { x: "bar", y: 42.0, z: 5.0 }, bar: 42, baz: 84.0 };
  #[native] select x {
    T as { foo: { y, z, .. }, .. } => y + z
  }
}
"#;

run!(nestedmatch1, NESTEDMATCH1, is(Value::F64(47.0)); FuseExpect::Jit);

const NESTEDMATCH2: &str = r#"
{
  type T = { foo: Array<f64>, bar: i64, baz: f64 };
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  let { foo: [x, y, ..], ..}: T = x;
  x + y
}
"#;

run!(nestedmatch2, NESTEDMATCH2, refused("refutable patterns are not allowed in let");
    FuseExpect::None);

const NESTEDMATCH3: &str = r#"
{
  let x = { foo: [ 1.0, 2.0, 4.3, 55.23 ], bar: 42, baz: 84.0 };
  #[native] select x {
    { foo: [x, y, ..], bar: _, baz: _ } => x + y,
    _ => never()
  }
}
"#;

run!(nestedmatch3, NESTEDMATCH3, is(Value::F64(3.0)); FuseExpect::Jit);

// A missing region input consumed only on an untaken arm yields a real
// value, not bottom.
const MISSING_ON_UNTAKEN_ARM_SCALAR: &str = r#"
{ let x: i64 = never(); select i64:0 { i64:0 => i64:5, _ => x } }
"#;

run!(missing_on_untaken_arm_scalar, MISSING_ON_UNTAKEN_ARM_SCALAR, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(5)))
});

const MISSING_ON_UNTAKEN_ARM_STRING: &str = r#"
{ let x: string = never(); select i64:0 { i64:0 => "live", _ => x } }
"#;

run!(missing_on_untaken_arm_string, MISSING_ON_UNTAKEN_ARM_STRING, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::String(s)) if &**s == "live")
});

// Structural destructuring over a borrowed composite scrutinee (tuple /
// struct / slice patterns with scalar leaves) fuses.

const SELECT_TUPLE_DESTRUCTURE: &str = r#"
{
  let t = (3, 4);
  #[native] select t {
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
  #[native] select t {
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
  #[native] select t {
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
  #[native] select p {
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
  #[native] select a {
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
  #[native] select a {
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
  #[native] select a {
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
  #[native] select a {
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
  #[native] select a {
    [x] => x,
    [] => -7,
    _ => 0
  }
}
"#;

run!(select_slice_empty, SELECT_SLICE_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-7)))
});

const SELECT_SLICE_NAMED_REST: &str = r#"
{
  let a = [1, 2, 3];
  #[native] select a {
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
  #[native] select a {
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
  #[native] select a {
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

// The region-root form: a wildcard-less slice-covered select fuses.
const SELECT_SLICE_COVER_FUSED: &str = r#"
{
  let a = [7, 8, 9];
  #[native] select a {
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
  let f = |xs: Array<i64>| -> i64 #[native] select xs {
    [] => 0,
    [init.., last] => last
  };
  f([7, 8, 9]) * 10 + f([])
}
"#;

run!(select_slice_cover_suffix, SELECT_SLICE_COVER_SUFFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(90)))
}; FuseExpect::Jit);

const SELECT_SLICE_COVER_PREFIX: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 #[native] select xs {
    [x, ..] => x,
    [] => -1
  };
  f([7, 8, 9]) * 10 + f([])
}
"#;

run!(select_slice_cover_prefix, SELECT_SLICE_COVER_PREFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(69)))
}; FuseExpect::Jit);

// An exact-length ladder under the rest form.
const SELECT_SLICE_COVER_LADDER: &str = r#"
{
  let f = |xs: Array<i64>| -> i64 #[native] select xs {
    [] => 0,
    [a] => a,
    [a, b, rest..] => a + b + array::len(rest)
  };
  f([]) + f([5]) + f([1, 2, 3, 4])
}
"#;

run!(select_slice_cover_ladder, SELECT_SLICE_COVER_LADDER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
}; FuseExpect::Jit);

// The pool covers the array member; the null member needs its own arm.
const SELECT_SLICE_COVER_UNION: &str = r#"
{
  let f = |xs: [Array<i64>, null]| -> i64 #[native] select xs {
    null as _ => -1,
    [] => 0,
    [init.., last] => last
  };
  f(null) + f([]) + f([1, 2, 3])
}
"#;

run!(select_slice_cover_union, SELECT_SLICE_COVER_UNION, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; FuseExpect::Jit);

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

run!(select_slice_hole_rejected, SELECT_SLICE_HOLE, refused("leave length 1 of Array<i64> uncovered"); FuseExpect::None);

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

run!(select_slice_no_rest_rejected, SELECT_SLICE_NO_REST, refused("cover finitely many lengths of Array<i64>"); FuseExpect::None);

// The length hint names only a ladder slice arms claimed and that still
// reaches past the last arm: type-test arms alone get none.
run!(select_type_tested_collections_no_ladder_hint, r#"{
  let x: [Array<i64>, i64, null] = null;
  select x { Array<i64> as a => array::len(a), i64 as n => n }
}"#, |v: Result<&Value>| matches!(v, Err(e) if {
    let e = format!("{e:?}");
    e.contains("no arm covers null") && !e.contains("lengths")
}); FuseExpect::None);

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
    refused("a guarded slice arm cannot establish length");
    FuseExpect::None
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
    refused("unreachable arm");
    FuseExpect::None
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
    refused("every array length this slice pattern can match is covered");
    FuseExpect::None
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
    refused("unreachable arm");
    FuseExpect::None
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
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    refused("refutable element patterns");
    FuseExpect::None
);

// Nested structural select patterns with scalar leaf binds fuse.

const SELECT_NESTED_TUPLE: &str = r#"
{
  let t = ((1, 2), 30);
  #[native] select t {
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
  #[native] select x {
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
  #[native] select t {
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
  #[native] select (a, a * 2) {
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
  #[native] select array::map(a, |x| x * 10) {
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
  #[native] select `Foo(n + 1) {
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
  #[native] select array::filter(a, |x| x > 10) {
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
  let total = #[native] array::fold(w, 0.0, |a, x| a + x);
  select count(total) {
    4 => total,
    _ => never()
  }
}
"#;

run!(gated_window_fold, GATED_WINDOW_FOLD, is(Value::F64(9.0)); FuseExpect::Jit);

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

run!(gated_string_builtin, GATED_STRING_BUILTIN, is(Value::I64(8)); FuseExpect::Jit);

const GATED_SCALAR_UNANNOTATED: &str = r#"
{
  let c = array::iter([1, 2, 3, 4]);
  let m = select c {
    0 => never(),
    _ => c
  };
  let r = #[native] m * 2 + 1;
  select count(r) {
    4 => r,
    _ => never()
  }
}
"#;

run!(gated_scalar_unannotated, GATED_SCALAR_UNANNOTATED, is(Value::I64(9)); FuseExpect::Jit);

// A guarded arm before a bind-all final arm is exhaustive.
const GUARDED_ARM_THEN_BINDALL: &str = r#"
{
  let v: [`A(i64), `B] = `A(i64:1);
  select v { `A(x) if x > i64:0 => x, y => i64:0 }
}
"#;

run!(guarded_arm_then_bindall, GUARDED_ARM_THEN_BINDALL, is(Value::I64(1)); FuseExpect::Jit);

// A guard must be `bool`.
const GUARD_STRING: &str = r#"
{
  let x = i64:1;
  select x { v if "" => i64:0, _ => i64:1 }
}
"#;

run!(guard_string_rejected, GUARD_STRING, refused("bool does not contain string");
    FuseExpect::None);

const GUARD_INT: &str = r#"
{
  let x = i64:1;
  select x { v if x => i64:0, _ => i64:1 }
}
"#;

run!(guard_int_rejected, GUARD_INT, refused("bool does not contain i64");
    FuseExpect::None);

// A nullable bool is not a bool guard either.
const GUARD_NULLABLE_BOOL: &str = r#"
{
  let b: [bool, null] = true;
  select i64:1 { v if b => i64:0, _ => i64:1 }
}
"#;

run!(guard_nullable_bool_rejected, GUARD_NULLABLE_BOOL, refused("bool does not contain [bool, null]"); FuseExpect::None);

// An unannotated lambda used as a guard infers a bool return.
const GUARD_INFERS_BOOL: &str = r#"
{
  let p = |x| x > i64:0;
  select i64:7 { v if p(v) => i64:0, _ => i64:1 }
}
"#;

run!(guard_infers_bool, GUARD_INFERS_BOOL, is(Value::I64(0)); FuseExpect::Jit);

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
    is(Value::I64(2));
    FuseExpect::Jit
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

run!(arm_union_tvar_collapse, ARM_UNION_TVAR_COLLAPSE, is(Value::I64(42)); FuseExpect::Jit);

// Bind-all arm types narrow by position: `s` after an unguarded
// irrefutable arm cannot be null, so it is `string`.
const BINDALL_NARROWS_BY_POSITION: &str = r#"
{
  let o: [string, null] = "x";
  let n = select o { null as _ => "", s => s };
  str::len(n)
}
"#;

run!(bindall_narrows_by_position, BINDALL_NARROWS_BY_POSITION, is(Value::I64(1)); FuseExpect::Jit);

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

run!(variant_payload_arm_narrows, VARIANT_PAYLOAD_ARM_NARROWS, is(Value::I64(45)); FuseExpect::Jit);

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

run!(variant_ignored_payload_arm_narrows, VARIANT_IGNORED_PAYLOAD_ARM_NARROWS, is(Value::I64(1)); FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A guard on an arm below the taken one is not consulted, so its input's
// fires emit nothing: m fires per x delivery, the select once.
const UNCONSULTED_GUARD_IS_QUIET: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x % 2;
  let c = count(select 0 { 0 => 1, k if m == 0 => 2, _ => 3 });
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(unconsulted_guard_is_quiet, UNCONSULTED_GUARD_IS_QUIET, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::Jit);

// An or-arm's one guard is judged once, on the first alternative that
// matches the structure: (0, 9) binds y = 9 by `(0, y)`, the guard fails,
// and `(y, 9)` is not tried.
const OR_GUARD_JUDGED_ONCE: &str = r#"
{
  let f = |p: (i64, i64)| -> i64 #[native] select p { (0, y) | (y, 9) if y < 5 => y, _ => -1 };
  f((0, 9))
}
"#;

run!(or_guard_judged_once, OR_GUARD_JUDGED_ONCE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-1)))
}; FuseExpect::Jit);

// A guard-dep fire emits whether or not the selection changes. m fires
// per x delivery: 4.
const GUARD_FIRE_EMITS_UNCHANGED_SELECTION: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x % 2;
  let sel = select 0 { 0 if m == 0 => 1, _ => 2 };
  let c = count(sel);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(guard_fire_emits_unchanged_selection, GUARD_FIRE_EMITS_UNCHANGED_SELECTION, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(4)))
}; FuseExpect::Jit);

// The same inside a collection loop: 4.
const GUARD_FIRE_EMITS_IN_LOOP: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let a = array::map([10], |i| select i { 10 if m == 0 => 1, _ => 2 });
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guard_fire_emits_in_loop,
    GUARD_FIRE_EMITS_IN_LOOP,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    FuseExpect::Jit
);

// Two loops deep: 4.
const GUARD_FIRE_EMITS_IN_NESTED_LOOP: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x / 3;
  let a = array::map([0], |i| array::map([0], |j| select j { 0 if m == 0 => 1, _ => 2 }));
  let c = count(a);
  select count(x) { 4 => c, _ => never() }
}
"#;

run!(
    guard_fire_emits_in_nested_loop,
    GUARD_FIRE_EMITS_IN_NESTED_LOOP,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(4))) };
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    FuseExpect::Jit
);

// In a callee body: 4.
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
}; FuseExpect::Jit);

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
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    FuseExpect::Jit
);

// An arm that slept through a let's fire catches it up even when, in the
// cycle it wakes, the enclosing arm's wake republished that let stale: its
// `<-` target takes the value.
async fn wake_over_a_republished_let_catches_up(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
  let ep = 0;
  ep <- select ep { n if n < 5 => n + 1, _ => never() };
  let in0 = 0;
  in0 <- select ep { 3 => 1, 4 => 0, _ => never() };
  let in1 = 0;
  in1 <- select ep { 2 => 7, _ => never() };
  let in3 = 1;
  in3 <- select ep { 1 => 0, 4 => 1, _ => never() };
  let f = |c, a, d, e| select c {
    0 => {
      let v = a * 1;
      select d { 0 => -5, _ => { let t = v + 0; t <- e ~ 99; t } }
    },
    _ => -1
  };
  f(in0, in1, in3, 0)
}"#,
        mode,
    )
    .await?;
    assert_eq!(values.last(), Some(&Value::I64(7)), "{values:?}");
    Ok(())
}

modes!(wake_over_a_republished_let_catches_up);

// `error as e` types `e` as the errors that reach the arm, so its
// payload reads and it passes where the scrutinee's errors are taken.
run!(
    error_test_capture_is_the_reaching_errors,
    r#"{
    let g = |x: i64| -> [i64, Error<`Boom>] select x { 0 => error(`Boom), x => x };
    let handle = |e: Error<`Boom>| select e.0 { `Boom => -1 };
    let two = |x: [i64, Error<`A>, Error<`B(string)>]| select x {
      error as e => select e.0 { `A => "a", `B(s) => s },
      i64 as _ => "n"
    };
    (select g(0) { error as e => handle(e), v => v }, two(error(`B("b"))))
}"#,
    prints(r#"[i64:-1, "b"]"#)
);

// A struct pattern's field is the pair at its index that names it: an
// or-alternative over a struct of another shape with that index fails.
run!(
    struct_pattern_checks_field_names,
    r#"{
    let g = |p: [{a: i64, x: i64}, {x: i64, z: string}]| select p {
      {a: 0, x} | {x, z: "q"} => x,
      _ => -1
    };
    let h = |p: [{y: i64}, {x: i64, y: string}]| select p {
      {y} | {x: 1, y} => y,
      _ => "none"
    };
    (g({x: 0, z: "q"}), h({x: 1, y: "s"}))
}"#,
    prints(r#"[i64:0, "s"]"#);
    FuseExpect::None
);

// An abstract's payload pattern compiles against its written rep: an
// or-name shared across its members holds their union, and a capture
// narrows to its alternative.
run!(
    abstract_payload_binds_keep_their_rep_types,
    r#"{
    type T = Abstract<[`A(i64), `B(string)]>;
    let t: T = T(`B("hello"));
    let r: i64 = select t { T(`A(x) | `B(x)) => x };
    r
}"#,
    refused("i64 does not contain [i64, string]");
    FuseExpect::None
);

run!(
    abstract_payload_capture_narrows,
    r#"{
    type T = Abstract<[`A(i64), `B(string)]>;
    let t: T = T(`A(3));
    select t {
      T(v@ `A(_)) => select v { `A(m) => m },
      T(`B(_)) => 0
    }
}"#,
    is(Value::I64(3));
    FuseExpect::Jit
);

// A bottom initializer that an arm's wake only re-reads, through `$`, a
// cast or a select arm, does not fire: the `<-` target keeps its written
// value, as with `never()`.
async fn wake_bottom_initializers_keep_the_written_value(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let run = |init: &str| {
        format!(
            "{{
  let sw = 0;
  sw <- select sw {{ n if n < 8 => n + 1, _ => never() }};
  let r = select sw % 3 {{
     0 => {{ let t: i64 = {init}; t <- sw ~ (100 + sw); t }},
     _ => -1
  }};
  (sw, r)
}}"
        )
    };
    let (expected, _) = run_delta(&run("never()"), mode).await?;
    for init in ["error(`E)$", "cast<i64>(\"x\")$", "select 1 { 1 => 1 / 0, _ => 2 }"] {
        let (values, _) = run_delta(&run(init), mode).await?;
        assert_eq!(values, expected, "{init}");
    }
    Ok(())
}

modes!(wake_bottom_initializers_keep_the_written_value);

// A banked `~` paid when its argument first arrives is that arrival, a
// real event: the sleeping arm catches `q` up to it at the wake, as it does
// a plain fire of `v`.
run!(banked_sample_payment_is_a_real_fire, r#"{
  let k = array::iter([0, 1, 2, 3, 4, 5, 6]);
  let v = never<i64>();
  v <- select k { 3 => 42, _ => never() };
  let t = once(k);
  let p = t ~ v;
  let w = 0;
  let on = true;
  on <- select k { 2 => false, 4 => true, _ => never() };
  let r = select on {
    true => { let q = any(p, w); q <- select k { 1 => 7, _ => never() }; q },
    false => -1
  };
  select k { 6 => r, _ => never() }
}"#, is(Value::I64(42)); FuseExpect::Jit);

// A fire the taken arm reads, through a reference or directly, is spent:
// the other arm's wake does not catch it up again. The reference's target
// is an input of its arm from the arm's first evaluation.
const WAKE_SPENT_THROUGH_A_REFERENCE: &str = r#"
{
  let n = array::iter([0, 1, 2, 3, 4, 5, 6]);
  let b = 10;
  b <- select n { 2 => 20, _ => never() };
  let r = &b;
  let c = (n < 2) || (n >= 4);
  let out = select c {
    true => { let cnt = 0; cnt <- (*r) ~ cnt + 1; cnt },
    false => READ + 100
  };
  array::group(out, |k, _| k == 7)
}
"#;

run!(wake_spent_through_a_reference, &WAKE_SPENT_THROUGH_A_REFERENCE.replace("READ", "*r"), |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 110, 120, 1, 1, 1])
}; FuseExpect::Jit);

run!(wake_spent_directly, &WAKE_SPENT_THROUGH_A_REFERENCE.replace("READ", "b"), |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 110, 120, 1, 1, 1])
}; FuseExpect::Jit);

// An arm-local `<-` target persists across the arm's sleep: a wake
// resumes the arm, so the written 11 survives.
// findings/arm-local-bind-aug2026/
const SELECT_ARM_LOCAL_PERSISTS: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let m = x % 2;
  let sel = select m {
    0 => (0, 0),
    _ => { let s = 10; s <- (x ~ s) + 1; (s, count(s)) }
  };
  array::group(sel, |n, _| n == 4)
}
"#;

run!(select_arm_local_persists, SELECT_ARM_LOCAL_PERSISTS, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string()
        == "[[i64:10, i64:1], [i64:0, i64:0], [i64:11, i64:2], [i64:0, i64:0]]")
}; FuseExpect::Jit);

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
            as_i64s(a) == [0, 100, 6, 100]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// A guard reading a capture inside a rec callee's tail select emits per
// m fire; the init-phantom guard bottoms init: [1, 1, 2].
const GUARD_FIRE_EMITS_IN_TAIL_SELECT: &str = r#"
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
    guard_fire_emits_in_tail_select,
    GUARD_FIRE_EMITS_IN_TAIL_SELECT,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                as_i64s(a) == [1, 1, 2]
            }
            _ => false,
        }
    };
    FuseExpect::Jit
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
                as_i64s(a) == [42, 1]
            }
            _ => false,
        }
    };
    FuseExpect::Jit
);

const SELECT_GUARD_AFTER_TAINTED_INIT: &str = r#"
{
  let rec f = |n: i64| -> i64 select n {
    m if m <= i64:0 => (m / m),
    m => f(m - i64:1)
  };
  let v = f(i64:1);
  let r = &mut v;
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
    is(Value::I64(200));
    FuseExpect::Jit
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
    FuseExpect::Jit
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
  "[f(g(`M(`M(`A(`V, `N(1.0)), `V), `V)))]"
}
"#;

run!(
    select_recursive_adt_chain,
    SELECT_RECURSIVE_ADT_CHAIN,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s.as_str()
        == "`A(`M(`A(`M(`N(1), `V), `M(`A(`V, `N(1)), `N(1))), `V), `M(`M(`A(`V, `N(1)), `V), `N(1)))");
    FuseExpect::Jit
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
    is(Value::I64(1));
    FuseExpect::Jit
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
    FuseExpect::Jit
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
    is(Value::I64(1));
    FuseExpect::Jit
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
    refused("matches more than one member");
    FuseExpect::None
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
    FuseExpect::None
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
    is(Value::I64(42));
    FuseExpect::None
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
    FuseExpect::Jit
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
        let msg = match eval(code, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!(
                "parse's result must not be inferred from a sibling arm: {code} -> {v:?}"
            ),
        };
        assert!(msg.contains("must be fully known here"), "{code}: {msg}");
    }
}

// The annotation types the union: `i64 ⊇ ['b, i64]` binds `'b`.
const UNION_ARM_ANNOTATED: &str = r#"{let y = i64:0; let v: i64 = select i64:1 {i64:1 => str::parse("42")?, _ => y}; v}"#;
run!(union_arm_annotated, UNION_ARM_ANNOTATED, is(Value::I64(42)));

// Shallow arm discriminators: an ambiguous union (same tag, same arity)
// stays on the deep walk; a mixed union dispatches the same shallow.
// Union type-test dispatch node-walks; the rest of each program fuses.

run!(
    shallow_ambiguous_same_tag_union,
    is(Value::I64(21)),
    "/test.gx" => r#"
        type E = [`A(i64), `A(string)];
        let pick = |v: E| -> i64 select v {
            `A(i64) as `A(x) => x * 10,
            `A(string) as `A(s) => str::len(s)
        };
        let result = pick(`A(2)) + pick(`A("x"))
    "#;
    FuseExpect::Jit
);

run!(
    shallow_mixed_union_dispatch,
    is(Value::I64(3241)),
    "/test.gx" => r#"
        type V = [`Pair(i64, i64), `One(i64), `Nil, f64, Array<i64>];
        let score = |v: V| -> i64 select v {
            `Pair(a, b) => a + b,
            `One(x) => x * 10,
            `Nil => 1000,
            f64 as f => cast<i64>(f)$ * 100,
            Array<i64> as a => array::len(a) * 1000
        };
        let result = score(`Pair(20, 1)) + score(`One(2))
            + score(`Nil) + score(2.0) + score([1, 2])
    "#;
    FuseExpect::Jit
);

// Or-patterns

const OR_LITERALS: &str = r#"
select 2 { 1 | 2 | 3 => "small", _ => "big" }
"#;

run!(or_literals, OR_LITERALS, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "small")
}; FuseExpect::Jit);

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
#[native] select `A(2) { `A(1 | 2) => "y", _ => "n" }
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

run!(or_same_binds_err, OR_SAME_BINDS_ERR, refused("must bind the same names");
 FuseExpect::None);

// A name the alternatives share is the union of their types, under an
// explicit type predicate too.
const OR_UNION_TYPES: &str = r#"
select (1, "a") {
    (i64, string) as (1, y) | (y, "b") => select y { i64 as n => "i", string as s => s },
    _ => "none"
}
"#;

run!(or_union_types, OR_UNION_TYPES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "a"
); FuseExpect::Jit);

// Dead alternatives are errors, like dead arms.
const OR_DUP_ALT_ERR: &str = r#"
select 1 { 1 | 1 => 0, _ => 2 }
"#;

run!(or_dup_alt_err, OR_DUP_ALT_ERR, refused("duplicate of an earlier alternative");
 FuseExpect::None);

const OR_DEAD_ALT_ERR: &str = r#"
select 1 { _ | 1 => 0 }
"#;

run!(or_dead_alt_err, OR_DEAD_ALT_ERR, refused("already matches anything");
 FuseExpect::None);

// Zero-residue: the whole or-select compiles native.
const OR_NATIVE: &str = r#"
{
  let p = (1, 2);
  #[native] select p { (1, y) | (y, 2) if y > 0 => y, _ => 0 }
}
"#;

run!(or_native, OR_NATIVE, |v: Result<&Value>| { matches!(v, Ok(Value::I64(2))) };
FuseExpect::Jit);

// Owned binds through the or-chain are dropped at arm exit.
const OR_OWNED_BINDS: &str = r#"
{
  let v: [`A(Array<i64>), `B(Array<i64>), `C] = `B([1, 2, 3]);
  #[native] select v { `A(xs) | `B(xs) => array::len(xs), `C => 0 }
}
"#;

run!(or_owned_binds, OR_OWNED_BINDS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// A scrutinee member whose variant payload is a union is exhausted by
// per-member arms: `[`P(A), `P(B)]` covers `P([A, B])`; fuses.
const SELECT_VARIANT_UNION_PAYLOAD: &str = r#"
{
  type Panel = [`Q, `D, `R];
  type Screen = [`Connect, `Panel(Panel)];
  let s: Screen = `Panel(`D);
  #[native] select s {
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
    FuseExpect::Jit
);

// Multi-argument distribution pools through one position: rectangular
// arms cover.
const SELECT_VARIANT_UNION_RECT: &str = r#"
{
  type T = [`P([`A, `B], [`X, `Y]), `N];
  let v: T = `P(`B, `Y);
  #[native] select v {
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
    FuseExpect::Jit
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
    refused("missing match cases");
    FuseExpect::None
);

// The same through a tuple head. A structural pattern over a nullable
// composite has no kernel lowering, so this select node-walks; the
// #[native] twin pins that.
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
    FuseExpect::Jit
);

const SELECT_TUPLE_UNION_MEMBER_NATIVE: &str = r#"
{
  let v: [([`A, `B], i64), null] = (`B, 7);
  #[native] select v {
    null as _ => 0,
    (`A, n) => n + 1,
    (`B, n) => n + 2
  }
}
"#;

run!(select_tuple_union_member_native, SELECT_TUPLE_UNION_MEMBER_NATIVE,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(9))); FuseExpect::Jit);

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
}; FuseExpect::None);

// An enclosing select's pattern bind is never delivered at a wake: the
// flip to arm 1 does not re-raise `k`, which the key fire bound while
// arm 0 was selected (design/wake_catchup.md, the tracker).
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
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  e <- select step { 1 => `Key(`Enter), _ => never() };
  screen <- select step { 4 => 1, _ => never() };
  select step { 8 => (screen, seen, fired), _ => never() }
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
    FuseExpect::Jit
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

run!(bool_pair_ladder_covers, BOOL_PAIR_LADDER, is(Value::I64(321)); FuseExpect::Jit);

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

run!(variant_bool_ladder_covers, VARIANT_BOOL_LADDER, is(Value::I64(99)); FuseExpect::Jit);

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

run!(bool_pair_ladder_dead_tail, BOOL_PAIR_LADDER_DEAD_TAIL, refused("unreachable arm"); FuseExpect::None);

// A sibling of a destructuring `let` that a `<-` writes fires alone: a
// woken arm catches up exactly what separate lets would give it, writing
// either sibling.
async fn written_let_sibling_fires_alone(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let program = |lets: &str, written: &str| {
        format!(
            "{{
  let n = 0;
  n <- select n {{ k if k < 7 => k + 1, _ => never() }};
  {lets}
  {written} <- select n {{ 2 => 7, _ => never() }};
  let c = ((n < 2) || (n >= 4)) && ({written} >= 0);
  select c {{
    true => {{ let cnt = 0; cnt <- b ~ cnt + 1; cnt }},
    false => n
  }}
}}"
        )
    };
    for written in ["a", "b"] {
        let (pair, _) =
            run_delta(&program("let (a, b) = (0, 0);", written), mode).await?;
        let (apart, _) =
            run_delta(&program("let a = 0; let b = 0;", written), mode).await?;
        assert_eq!(pair, apart, "writing {written}");
    }
    Ok(())
}

modes!(written_let_sibling_fires_alone);

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
  let step = 0;
  step <- select step { s if s < 8 => s + 1, _ => never() };
  pair <- select step { 1 => (1, 2), _ => never() };
  screen <- select step { 4 => 1, _ => never() };
  select step { 8 => (screen, seen, fired), _ => never() }
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
    FuseExpect::Jit
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
  #[native] select u { string as s => str::len(s) }
}
"#;

run!(never_arms_absorb, NEVER_ARMS_ABSORB, is(Value::I64(1)); FuseExpect::Jit);

// `never<T>()` carries `T` where nothing else fixes the type.
const NEVER_TYPED: &str = r#"
{
  let p = never<i64>();
  let s = select 2 { 1 => p, _ => 5 };
  s + 1
}
"#;

run!(never_typed, NEVER_TYPED, is(Value::I64(6)); FuseExpect::Jit);

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

run!(never_args_live, NEVER_ARGS_LIVE, is(Value::I64(3)); FuseExpect::Jit);

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

run!(arm_sampled_write_keeps_trigger, ARM_SAMPLED_WRITE_KEEPS_TRIGGER, is(Value::I64(30)); FuseExpect::Jit);

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
  let writes = count(once);
  select step { 6 => (sampled, once, writes), _ => never() }
}
"#;

// `once` is written at init and at the one entry: two writes.
run!(arm_ungated_const_writes_once, ARM_UNGATED_CONST_WRITES_ONCE, prints("[i64:3, i64:1, i64:2]"); FuseExpect::Jit);

// A constant write fires at the arm's birth, its first entry, and not at
// a later one, which resumes the arm: init and the first entry.
const ARM_CONST_WRITES_AT_ITS_BIRTH: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 9 => s + 1, _ => never() };
  let k = select step { 1 | 2 | 3 | 6 | 7 => `Go, _ => `Stop };
  let once = 0;
  select k {
    `Go => { once <- 1; null },
    `Stop => null
  };
  let writes = count(once);
  select step { 9 => writes, _ => never() }
}
"#;

run!(arm_const_writes_at_its_birth, ARM_CONST_WRITES_AT_ITS_BIRTH, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; FuseExpect::Jit);

const HANDLER_WRITE_ON_ERROR_ONLY: &str = r#"
{
  let step = 0;
  step <- select step { s if s < 3 => s + 1, _ => never() };
  let err: [`None, `Bad] = `None;
  let x = { catch(e) err <- e ~ `Bad; select step { 2 => error(`E)?, _ => 0 } };
  array::group((step, err), |n, _| n == 4)
}
"#;

// The history: `Bad arrives with the error at step 3 and at no other step.
run!(handler_write_on_error_only, HANDLER_WRITE_ON_ERROR_ONLY, prints(r#"[[i64:0, "None"], [i64:1, "None"], [i64:2, "None"], [i64:3, "Bad"]]"#); FuseExpect::Jit);

// `~` banks triggers that find `v` absent (three banked writes and the
// live one, four); `~!` drops them (one write).
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

run!(strict_sample_no_bank, STRICT_SAMPLE_NO_BANK, prints("[i64:4, i64:1]"); FuseExpect::Jit);

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

run!(arm_write_from_never, ARM_WRITE_FROM_NEVER, prints(r#""Remove this install?""#); FuseExpect::Jit);

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

run!(skip_sleep_arm_computes_on_first_take, SKIP_SLEEP_ARM_COMPUTES_ON_FIRST_TAKE, prints(r#""CA (tls)""#); FuseExpect::Jit);

// A scrutinee that went bottom while its select slept is bottom at the
// wake: x=4 reselects the arm and emits nothing.
// findings/wake-stale-bottom-sep2026/
const WAKE_STALE_BOTTOM_SCRUTINEE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5]);
  let d = uniq(select x { 1 | 2 => 1, _ => 0 });
  let v0 = 10 / d;
  let r = select x {
    1 | 4 => { println("arm"); select v0 { 10 => 100, n => n } },
    _ => -1
  };
  array::group(r, |n, _| n == 4)
}
"#;

run!(wake_stale_bottom_scrutinee, WAKE_STALE_BOTTOM_SCRUTINEE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [100, -1, -1, -1]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// The same through a let inside the arm.
const WAKE_STALE_BOTTOM_LET: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5]);
  let d = uniq(select x { 1 | 2 => 1, _ => 0 });
  let v0 = 10 / d;
  let r = select x {
    1 | 4 => { println("arm"); { let y = v0; y + 1 } },
    _ => -1
  };
  array::group(r, |n, _| n == 4)
}
"#;

run!(wake_stale_bottom_let, WAKE_STALE_BOTTOM_LET, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [11, -1, -1, -1]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// The same through a call argument.
const WAKE_STALE_BOTTOM_CALL: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5]);
  let d = uniq(select x { 1 | 2 => 1, _ => 0 });
  let v0 = 10 / d;
  let f = |y: i64| -> i64 y + 1;
  let r = select x {
    1 | 4 => { println("arm"); f(v0) },
    _ => -1
  };
  array::group(r, |n, _| n == 4)
}
"#;

run!(wake_stale_bottom_call, WAKE_STALE_BOTTOM_CALL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [11, -1, -1, -1]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// The same through a collection source.
const WAKE_STALE_BOTTOM_MAP_SOURCE: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 5]);
  let d = uniq(select x { 1 | 2 => 1, _ => 0 });
  let v0 = 10 / d;
  let r = select x {
    1 | 4 => { println("arm"); array::fold(array::map([v0, 3], |y| y + 1), 0, |a, y| a + y) },
    _ => -1
  };
  array::group(r, |n, _| n == 4)
}
"#;

run!(wake_stale_bottom_map_source, WAKE_STALE_BOTTOM_MAP_SOURCE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [15, -1, -1, -1]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// A source back from bottom with no slot firing (the callback ignores
// its element): the map is present again, so x=1 re-emits the arm.
// findings/map-source-recovers-sep2026/
const MAP_SOURCE_RECOVERS_QUIETLY: &str = r#"
{
  let x = array::iter([1, 2, 3, 4, 1]);
  let d = select x { 1 | 2 => 1, _ => 0 };
  let v0 = 2 / d;
  let r = select x {
    1 | 4 => array::fold(array::map([v0, 3], |y| 2 + 1), 0, |a, y| a + y),
    _ => 0
  };
  array::group(r, |n, _| n == 4)
}
"#;

run!(map_source_recovers_quietly, MAP_SOURCE_RECOVERS_QUIETLY, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [6, 0, 0, 6]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// A connect target born `never()` keeps its written value across the
// arm's sleep: the wake at the last x=1 reads 42.
// findings/wake-connect-target-sep2026/
const WAKE_KEEPS_NEVER_BORN_CONNECT_TARGET: &str = r#"
{
  let x = array::iter([1, 3, 1, 2, 1]);
  let r = select x {
    1 | 3 => {
      let y = never();
      y <- select x { 3 => 42, _ => never() };
      select x { 1 => y, _ => never() }
    },
    _ => -1
  };
  array::group(r, |n, _| n == 3)
}
"#;

run!(
    wake_keeps_never_born_connect_target,
    WAKE_KEEPS_NEVER_BORN_CONNECT_TARGET,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                as_i64s(a) == [42, -1, 42]
            }
            _ => false,
        }
    }; FuseExpect::Jit
);

// A reference cell born in a sleepable arm delivers the cycle it is
// born: the deref fires and `dbg` runs.
// findings/byref-wake-view-sep2026/
const BYREF_BORN_IN_SLEEPABLE_ARM: &str = r#"
select 1 {
  0 => 0,
  _ => { let x = 0; x <- 1; dbg({ let r = &(1, 2); let t = *r; t.0 }) }
}
"#;

run!(byref_born_in_sleepable_arm, BYREF_BORN_IN_SLEEPABLE_ARM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::Jit);

const BYREF_BORN_IN_GUARDED_SELECT: &str = r#"
select 1 { 0 if true => 0, _ => dbg({ let r = &(1, 2); let t = *r; t.0 }) }
"#;

run!(byref_born_in_guarded_select, BYREF_BORN_IN_GUARDED_SELECT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::Jit);

// A `name@ pattern` capture has the type of what the arm matched in the
// SCRUTINEE, never the type inferred from the pattern: a partial struct
// pattern leaves the other fields readable, and a `_` slot is the
// scrutinee's type, not Any.
const CAPTURE_PARTIAL_STRUCT: &str = r#"
{
  type Pair = { left: i64, right: i64 };
  let p: Pair = { left: 1, right: 2 };
  select p { whole@ { left, .. } => left + whole.right }
}
"#;

run!(capture_partial_struct, CAPTURE_PARTIAL_STRUCT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
});

const CAPTURE_IGNORED_SLOT: &str = r#"
{
  let p = { left: 1, right: 2 };
  let t = (3, 4);
  select (p, t) {
    (w@ { left, right: _ }, u@ (_, n)) => left + w.right + u.0 + n
  }
}
"#;

run!(capture_ignored_slot, CAPTURE_IGNORED_SLOT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(10)))
});

// A slice's rest is typed from the scrutinee like a capture: under `_`
// its element is the scrutinee's, never the wildcard's `Any`, when a
// guard or the body reads it.
const CAPTURE_SLICE_REST: &str = r#"
{
  let b = true;
  let r0 = &b;
  let refs = [r0, r0];
  let bools = [false, true];
  let l = [<1, 2, 3>];
  let a = select refs { [_, r..] if *(r[-1]$) => 1, _ => 0 };
  let c = select bools { [_, r..] if r[-1]$ => 10, _ => 0 };
  let d = select l { [<_, t..>] => list::len(t) * 100, _ => 0 };
  a + c + d
}
"#;

run!(capture_slice_rest, CAPTURE_SLICE_REST, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(211)))
});

const CAPTURE_UNION_MEMBER: &str = r#"
{
  type Pair = { left: i64, right: i64 };
  let u: [Pair, null] = { left: 1, right: 2 };
  select u { null as _ => 0, w@ { left, .. } => left + w.right }
}
"#;

run!(capture_union_member, CAPTURE_UNION_MEMBER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
});

const CAPTURE_OR_PATTERN: &str = r#"
{
  let v: [`A(i64, i64), `B(i64, i64), `C] = `B(1, 2);
  select v {
    w@ `A(n, _) | w@ `B(n, _) => select w { `A(_, m) => n + m, `B(_, m) => n * 10 + m },
    `C => 0
  }
}
"#;

run!(capture_or_pattern, CAPTURE_OR_PATTERN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(12)))
});

// A guard past the 64th arm is consulted only when the chain reaches
// it: a match at arm 0 is not bottomed by a never-produced guard at
// arm 64.
fn guard_beyond_sixty_four_arms() -> String {
    let arms: Vec<String> = (0..64).map(|i| format!("{i} => {i}")).collect();
    format!(
        "{{ let x = 0; select x {{ {}, 64 if never<bool>() => 64, _ => -1 }} }}",
        arms.join(", ")
    )
}

run!(guard_beyond_sixty_four_arms, guard_beyond_sixty_four_arms(), |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::I64(0)))
}; FuseExpect::None);

// An arm input that went bottom while its arm slept is bottom at the
// wake: the inner select sets its resident bottom instead of riding 7.
const SELECT_ARM_STALE_BOTTOM_AT_WAKE: &str = r#"
{
  let clock = sys::time::timer(duration:0.01s, true);
  let n = 0;
  n <- clock ~ n + 1;
  let x = select uniq(n < 2) { true => 7, false => never() };
  let m = 1;
  let d = 0;
  let outer = select n {
    k if (k < 1) || (k > 3) => { let r = select m { _ => x }; d <- r; r },
    _ => 0
  };
  let seen = [];
  seen <- outer ~ array::push(seen, outer);
  select n { 7 => seen, _ => never() }
}
"#;

run!(
    select_arm_stale_bottom_at_wake,
    SELECT_ARM_STALE_BOTTOM_AT_WAKE,
    prints("[i64:7, i64:0, i64:0, i64:0]")
);

// At a wake a connect target's initializer republishes when one of its
// inputs fired, as it would had the arm never slept; the arm's constants
// stand, directly or through a `let`, and leave the last write standing.
const WAKE_INPUT_FIRE_REACHES_TARGET: &str = r#"
{
  let n = array::iter([0, 1, 2, 3, 4, 5, 6]);
  let y = n * 100;
  let r = select n % 2 {
    0 => { let x = y; x <- select n { 0 => 5, _ => never() }; x },
    _ => -1
  };
  array::group(r, |i, _| i == 7)
}
"#;

run!(
    wake_input_fire_reaches_target,
    WAKE_INPUT_FIRE_REACHES_TARGET,
    prints("[i64:0, i64:-1, i64:200, i64:-1, i64:400, i64:-1, i64:600]")
);

const WAKE_CONSTANT_KEEPS_TARGET: &str = r#"
{
  let n = array::iter([0, 1, 2, 3, 4, 5, 6]);
  let r = select n % 2 {
    0 => { let a = 10; let x = a; x <- n ~ x + 1; x },
    _ => -1
  };
  let q = select n % 2 {
    0 => { let x = 10; x <- n ~ x + 1; x },
    _ => -1
  };
  array::group((r, q), |i, _| i == 7)
}
"#;

run!(
    wake_constant_keeps_target,
    WAKE_CONSTANT_KEEPS_TARGET,
    prints(
        "[[i64:10, i64:10], [i64:-1, i64:-1], [i64:11, i64:11], [i64:-1, i64:-1], \
            [i64:12, i64:12], [i64:-1, i64:-1], [i64:13, i64:13]]"
    )
);

// A target that holds a value is held at its arm's wake even when its
// initializer never published: a bottom initializer whose value arrived
// during the sleep, read by the other arm, leaves the last write.
async fn wake_keeps_target_of_unpublished_let(mode: Mode) -> Result<()> {
    use super::dense_deltas::run_delta;
    let (values, _) = run_delta(
        r#"{
  let n = 0;
  n <- select n { k if k < 6 => k + 1, _ => never() };
  let y: i64 = never();
  y <- select n { 2 => 1, _ => never() };
  let c = (n < 2) || (n >= 4);
  select c {
    true => {
      let x = y;
      x <- select n { 1 => 5, _ => never() };
      x
    },
    false => y + 100
  }
}"#,
        mode,
    )
    .await?;
    assert_eq!(as_i64s(&values), vec![101, 5, 5, 5]);
    Ok(())
}

modes!(wake_keeps_target_of_unpublished_let);

// A restart builtin over a constant passes nothing at a wake, where the
// constant stands, so a `<-` target it initialises keeps its value after
// the wake as one a constant initialises does (`base`).
const WAKE_RESTART_BUILTINS_KEEP_TARGET: &str = r#"
{
  let ep = 0;
  ep <- select ep { n if n < 8 => n + 1, _ => never() };
  let mode = select ep { 3 => 1, 4 => 1, _ => 0 };
  let r = select mode {
    0 => {
      let base = 100;
      base <- ep ~ base + 1;
      let a = once(100);
      a <- ep ~ a + 1;
      let b = take(#n: 1, 100);
      b <- ep ~ b + 1;
      let c = skip(#n: 0, 100);
      c <- ep ~ c + 1;
      let d = uniq(100);
      d <- ep ~ d + 1;
      let e = hold(#clock: 1, 100);
      e <- ep ~ e + 1;
      let f = count(100) + 99;
      f <- ep ~ f + 1;
      let g = filter(100, |x| x > 0);
      g <- ep ~ g + 1;
      let h = throttle(100);
      h <- ep ~ h + 1;
      [base, a, b, c, d, e, f, g, h]
    },
    _ => [-1]
  };
  select ep { 8 => r, _ => never() }
}
"#;

run!(
    wake_restart_builtins_keep_target,
    WAKE_RESTART_BUILTINS_KEEP_TARGET,
    prints(
        "[i64:106, i64:106, i64:106, i64:106, i64:106, i64:106, i64:106, i64:106, i64:106]"
    )
);

// While a consulted guard stands bottom the selection is undecidable on
// every cycle, quiet ones included: the held arm does not run.
const SELECT_UNDECIDABLE_QUIET: &str = r#"
{
  let clock = sys::time::timer(duration:0.01s, true);
  let n = 0;
  n <- clock ~ n + 1;
  let g = select uniq((n >= 3) && (n <= 5)) { true => never(), false => false };
  let seen = [];
  let s = 1;
  select s { _ if g => null, _ => seen <- n ~ array::push(seen, n) };
  select n { 8 => seen, _ => never() }
}
"#;

run!(
    select_undecidable_quiet,
    SELECT_UNDECIDABLE_QUIET,
    prints("[i64:0, i64:1, i64:2, i64:6, i64:7]")
);

// `&&` and `||` are strict: `false && ⊥` and `true || ⊥` are bottom, in a
// consulted guard (the selection is undecidable) and as values.
const STRICT_LOGIC_OVER_BOTTOM: &str = r#"
{
  let f = |k: i64, z: i64| -> i64 select k { 0 if false && ((1 / z) > 0) => 1, _ => 2 };
  let g = |k: i64, z: i64| -> i64 select k { 0 if true || ((1 / z) > 0) => 1, _ => 2 };
  let z = 0;
  let a = false && ((1 / z) > 0);
  let b = true || ((1 / z) > 0);
  (any(f(0, 0), -1), any(g(0, 0), -1), any(a, true), any(b, false))
}
"#;

run!(
    strict_logic_over_bottom,
    STRICT_LOGIC_OVER_BOTTOM,
    prints("[i64:-1, i64:-1, true, false]")
);

// A `null` literal arm covers `null`, its type's one value, beside a
// type test or a pool of partial struct patterns.
const NULL_LITERAL_COVERS_NULL: &str = r#"
{
  type S = {c: string, d: [`A, `B]};
  let o: [null, i64] = null;
  let a = select o { null => 1, i64 as x => x };
  let s: [null, S] = {c: "x", d: `B};
  let b = select s { null => 0, {d: `A, ..} => 10, {d: `B, ..} => 20 };
  a + b
}
"#;

run!(null_literal_covers_null, NULL_LITERAL_COVERS_NULL, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(21)))
});

const NULL_LITERAL_ARM_DEAD: &str = r#"
{
  let o: [null, i64] = null;
  select o { null => 0, i64 as x => x, null => 1 }
}
"#;

run!(null_literal_arm_dead, NULL_LITERAL_ARM_DEAD, refused("unreachable arm"); FuseExpect::None);

// A nested variant head is a pooled position: two arms that miss
// `(true, `B)` do not cover the tuple.
const POOL_NESTED_VARIANT_PARTIAL: &str = r#"
{
  let v: (bool, [`A, `B]) = (true, `B);
  select v { (true, `A) => 1, (false, `B) => 2 }
}
"#;

run!(pool_nested_variant_partial, POOL_NESTED_VARIANT_PARTIAL, refused("missing match cases"); FuseExpect::None);

const POOL_NESTED_VARIANT_COMPLETE: &str = r#"
{
  let v: (bool, [`A, `B]) = (true, `B);
  select v { (true, `A) => 1, (false, `B) => 2, (true, `B) => 3, (false, `A) => 4 }
}
"#;

run!(pool_nested_variant_complete, POOL_NESTED_VARIANT_COMPLETE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
});

// A bool literal pools only over a position of type bool.
const POOL_BOOL_IN_UNION: &str = r#"
{
  let v: ([bool, i64], i64) = (5, 1);
  select v { (true, x) => x, (false, x) => x }
}
"#;

run!(pool_bool_in_union, POOL_BOOL_IN_UNION, refused("missing match cases"); FuseExpect::None);

// A tuple pattern of binds is a wildcard only over a tuple scrutinee.
const TUPLE_ARM_OVER_UNION: &str = r#"
{
  let v: [(i64, i64), string] = "x";
  select v { (a, b) => a + b }
}
"#;

run!(tuple_arm_over_union, TUPLE_ARM_OVER_UNION, refused("missing match cases"); FuseExpect::None);

// Completion picks the member the pattern can match, not the first
// struct: `{x: ..}` is exhaustive, so it cannot be the member with `a`.
const COMPLETE_MATCHING_MEMBER: &str = r#"
{
  let v: [{a: i64, x: {z: i64, w: i64}}, {x: {z: string, q: i64}}] = {x: {z: "s", q: 1}};
  select v { {x: {z, ..}} => z, _ => "two" }
}
"#;

run!(complete_matching_member, COMPLETE_MATCHING_MEMBER, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "s")
});

// A tuple over a partial struct matching two members is ambiguous.
const COMPLETE_TUPLE_AMBIGUOUS: &str = r#"
{
  let v: [(i64, {a: i64, b: i64}), (string, {a: string, c: i64})] = ("x", {a: "s", c: 1});
  select v { (s, {a, ..}) => 1 }
}
"#;

run!(complete_tuple_ambiguous, COMPLETE_TUPLE_AMBIGUOUS, refused("matches more than one member"); FuseExpect::None);

// Slice arms cover a union of arrays whichever member a type test names.
const ARRAY_UNION_TYPE_TEST: &str = r#"
{
  let v: [Array<i64>, Array<string>] = ["a"];
  select v { Array<i64> as _ => "i", [] => "empty", [x, rest..] => x }
}
"#;

run!(array_union_type_test, ARRAY_UNION_TYPE_TEST, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a")
});

// A type test over a mixed collection's elements narrows nothing after
// it: an array that is not all i64 may still hold an i64.
const ARRAY_ELEMENT_TEST_KEEPS_RESIDUAL: &str = r#"
{
  let x: Array<[i64, string]> = [1, "a"];
  let r: string = select x { Array<i64> as _ => "ints", s => s[0]$ };
  r
}
"#;

run!(array_element_test_keeps_residual, ARRAY_ELEMENT_TEST_KEEPS_RESIDUAL, refused("string does not contain [i64, string]");
 FuseExpect::None);

const ARRAY_ELEMENT_TEST_RESIDUAL_MIXED: &str = r#"
{
  let x: Array<[i64, string]> = [1, "a"];
  select x {
    Array<i64> as _ => "ints",
    s => select s[0]$ { string as t => t, i64 as n => "[n]" }
  }
}
"#;

run!(
    array_element_test_residual_mixed,
    ARRAY_ELEMENT_TEST_RESIDUAL_MIXED,
    |v: Result<&Value>| { matches!(v, Ok(Value::String(s)) if &**s == "1") }
);

const LIST_ELEMENT_TEST_RESIDUAL_MIXED: &str = r#"
{
  let x: List<[i64, string]> = [<1, "a">];
  select x {
    List<i64> as _ => "ints",
    [<h, rest..>] => select h { string as t => t, i64 as n => "[n]" },
    [<>] => "empty"
  }
}
"#;

run!(list_element_test_residual_mixed, LIST_ELEMENT_TEST_RESIDUAL_MIXED, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::String(s)) if &**s == "1")
});

// An array and a list are told apart, the empty ones included.
const ARRAY_LIST_LADDERS: &str = r#"
{
  let v: [Array<i64>, List<i64>] = [<4, 5>];
  select v { [] => 0, [x, rest..] => x, [<>] => 1, [<h, t..>] => h }
}
"#;

run!(array_list_ladders, ARRAY_LIST_LADDERS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(4)))
}; FuseExpect::Jit);

// A tuple, a payload variant or a struct is told apart from an array, a
// bare variant from a string, a reference from a number; two references
// are not told apart by any test.
const SAME_FORM_TUPLE_ARRAY: &str = r#"
{
  let f = |x: [(i64, string), Array<[i64, string]>]| select x {
    (i64, string) as t => 1,
    Array<[i64, string]> as a => 2
  };
  f((1, "a"))
}
"#;

run!(tuple_told_from_array, SAME_FORM_TUPLE_ARRAY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::Jit);

const SAME_FORM_VARIANT_ARRAY: &str = r#"
{
  let f = |x: [`A(i64), Array<[`A, i64]>]| select x {
    `A(n) => n,
    Array<[`A, i64]> as a => 2
  };
  f(`A(1))
}
"#;

run!(variant_told_from_array, SAME_FORM_VARIANT_ARRAY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::None);

const SAME_FORM_STRUCT_ARRAY: &str = r#"
{
  let f = |x: [{a: i64, b: i64}, Array<(string, i64)>]| select x {
    {a: i64, b: i64} as s => 1,
    Array<(string, i64)> as a => 2
  };
  f({a: 1, b: 2})
}
"#;

run!(struct_told_from_array, SAME_FORM_STRUCT_ARRAY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::Jit);

// A reference has a runtime form of its own: a test tells it from a
// number.
const REF_AND_NUMBER_TOLD_APART: &str = r#"
{
  let f = |x: [&i64, u64]| select x {
    &i64 as r => *r,
    u64 as n => cast<i64>(n)$
  };
  let v = 1;
  (f(&v), f(u64:2))
}
"#;

run!(ref_and_number_told_apart, REF_AND_NUMBER_TOLD_APART, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:2]")
}; FuseExpect::Jit);

const SAME_FORM_TAG_STRING: &str = r#"
{
  let f = |x: [string, `A]| select x { `A => 1, s => 2 };
  f("A")
}
"#;

run!(tag_told_from_string, SAME_FORM_TAG_STRING, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; FuseExpect::None);

const SAME_FORM_REFERENCES: &str = r#"
{
  let b = 42;
  let f = |r: [&string, &i64]| select r { &string as s => 1, r => 2 };
  f(&b)
}
"#;

run!(same_form_references_refused, SAME_FORM_REFERENCES, refused("same runtime form"); FuseExpect::None);

const SAME_FORM_TRAIT_DISPATCH: &str = r#"
{
  trait Show { val show: fn(self) -> string };
  impl Show for string { let show = |s| "string" };
  impl Show for `A { let show = |a| "tag" };
  let d = |x: [string, `A]| Show::show(x);
  d("A")
}
"#;

run!(trait_dispatch_tells_tag_from_string, SAME_FORM_TRAIT_DISPATCH, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "string")
}; FuseExpect::Jit);

// The same dispatch reached through a generic bound.
const SAME_FORM_TRAIT_BOUND: &str = r#"
{
  trait Show { val show: fn(self) -> string };
  impl Show for string { let show = |s| "string" };
  impl Show for `A { let show = |a| "tag" };
  let g = 'a: Show |x: 'a| Show::show(x);
  let v: [string, `A] = "A";
  g(v)
}
"#;

run!(trait_bound_tells_tag_from_string, SAME_FORM_TRAIT_BOUND, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "string")
}; FuseExpect::Jit);

// An open member beside one whose runtime form another type may share is
// refused at the definition: no call is checked against the arm's test
// (x-typecheck-patterns-04/05).
run!(open_member_beside_a_reference_refused, r#"{
  let f = |x: [&i64, 'b]| select x { &i64 as r => "int ref", _ => "other" };
  let s = "s";
  f(&s)
}"#, refused("annotate it"); FuseExpect::None);

run!(open_member_in_a_dispatch_refused, r#"{
  trait Show { val show: fn(self) -> string };
  impl Show for &i64 { let show = |x| "int ref" };
  impl Show for &string { let show = |x| "string ref" };
  let f = 'b: Show |x: [&i64, 'b]| Show::show(x);
  let s = "s";
  f(&s)
}"#, refused("annotate it"); FuseExpect::None);

run!(open_member_beside_an_error_refused, r#"{
  let f = |r: ['a, Error<`E>]| select r { error as e => 0, v => 1 };
  f(1)
}"#, refused("annotate it"); FuseExpect::None);

// A result's success type is never an error (`NoError`), so its error arm
// tells it apart.
run!(open_result_success_told_from_its_error, r#"{
  let f = |r: Result<'a, `E>| select r { error as e => 0, v => 1 };
  (f(1), f("s"), f(error(`E)))
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:1, i64:0]"); FuseExpect::Jit);

// A variant arm over a generic union is told apart by its tag: the open
// cells in its payload meet themselves, not a sibling's form.
run!(
    variant_arm_over_a_generic_union,
    r#"{
  type Ev<'r: NoError> = [`A(i64), `Done(Result<'r, `E>)];
  let f = |e: Ev<'r>| -> Result<'r, `E> select e { `Done(r) => r, _ => never() };
  f(`Done(1))$
}"#,
    is(Value::I64(1))
);

// `"A"` and `` `A `` are two values: unequal, and two map keys.
const SAME_FORM_COMPARE: &str = r#"
{
  let x: [string, `A] = "A";
  let y: [string, `A] = `A;
  x == y
}
"#;

run!(tag_and_string_compare_unequal, SAME_FORM_COMPARE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(false)))
}; FuseExpect::Jit);

const SAME_FORM_MAP_KEY: &str = r#"
{
  let k: [string, `A] = `A;
  let m = map::insert({"A" => 1}, k, 2);
  map::len(m)
}
"#;

run!(tag_and_string_are_two_keys, SAME_FORM_MAP_KEY, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
}; FuseExpect::Jit);

// Members that share a constructor are told apart by their parts, and
// an empty array of either element type is both.
const SAME_FORM_DISTINCT_PARTS: &str = r#"
{
  let f = |x: [Array<i64>, Array<string>, `A(i64), `B(i64), (bool, i64)]| select x {
    Array<i64> as a => 1,
    Array<string> as a => 2,
    `A(n) => 3,
    `B(n) => 4,
    (bool, i64) as t => 5
  };
  f(`B(1))
}
"#;

run!(same_form_distinct_parts, SAME_FORM_DISTINCT_PARTS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(4)))
}; FuseExpect::None);

// An or-arm narrows what the later arms see, per alternative.
const OR_ARM_NARROWS: &str = r#"
{
  let v: [`A, `B, `C] = `C;
  select v { `A | `B => 1, x => select x { `C => 2 } }
}
"#;

run!(or_arm_narrows, OR_ARM_NARROWS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(2)))
});

// Under an explicit type every alternative checks against the whole
// type, whatever order the alternatives are written in.
const OR_EXPLICIT_TYPE: &str = r#"
{
  let v = (0, 5);
  select v { (i64, i64) as (0, y) | (y, 0) => y, _ => 0 - 1 }
}
"#;

run!(or_explicit_type, OR_EXPLICIT_TYPE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
});

// A bare variant alternative under a written union narrows to its member,
// whatever order the alternatives are written in.
const OR_EXPLICIT_UNION_ORDER: &str = r#"
{
  let v: [`A, `B] = `B;
  select v { [`A, `B] as `B | `A => 1 }
}
"#;

run!(or_explicit_union_order, OR_EXPLICIT_UNION_ORDER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
});

// A pattern error is sited at the pattern.
const PATTERN_ERROR_SITE: &str = r#"select 1 {
  (a, a) => 0,
  _ => 1
}"#;

run!(pattern_error_site, PATTERN_ERROR_SITE, |v: Result<&Value>| match v {
    Err(e) => e
        .downcast_ref::<graphix_compiler::expr::ErrorSite>()
        .is_some_and(|site| (site.expr().pos.line, site.expr().pos.column) == (2, 3)),
    Ok(_) => false,
}; FuseExpect::None);

// An error in a scrutinee or a guard is placed there, not on the select.
#[tokio::test(flavor = "current_thread")]
async fn a_guard_or_scrutinee_error_is_placed_there() -> Result<()> {
    use graphix_compiler::expr::ErrorSite;
    for (src, site) in [
        (r#"select 1 { x if str::contains("a") => 1, _ => 2 }"#, r#"str::contains("a")"#),
        (
            r#"select str::contains("a") { true => 1, false => 2 }"#,
            r#"str::contains("a")"#,
        ),
    ] {
        let e = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => e,
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        let at = e.downcast_ref::<ErrorSite>().map(|s| s.expr().to_string());
        assert_eq!(at.as_deref(), Some(site), "{src}: {e:#}");
    }
    Ok(())
}

/// Every combination of two unions covers the product of the unions.
const PRODUCT_OF_UNIONS_COVERED: &str = r#"
{
    type LN = [`L, `N];
    let f = |a: LN, b: LN| select (a, b) {
        (`L, `L) => 1,
        (`L, `N) => 2,
        (`N, `L) => 3,
        (`N, `N) => 4
    };
    let g = |p: [(`L, `L), (`L, `N), (`N, `L), (`N, `N)]| select p { (`N, `L) => 30, _ => 0 };
    let ab: (LN, LN) = (`N, `L);
    f(`N, `L) + g(ab)
}
"#;

run!(product_of_unions_covered, PRODUCT_OF_UNIONS_COVERED, is(Value::I64(33)));

// A name the alternatives share is the union of what each can match:
// `A(x) gives i64, `B(x) [i64, string].
const OR_BINDS_UNEQUAL_PAYLOADS: &str = r#"
{
    let f = |v: [`A(i64), `B([i64, string]), `C]| -> i64 select v { `A(x) | `B(x) => x, _ => 0 };
    f(`B("s"))
}
"#;

run!(or_binds_unequal_payloads, OR_BINDS_UNEQUAL_PAYLOADS, |v: Result<&Value>| {
    refused("i64 does not contain [i64, string]")(v)
}; FuseExpect::None);

const OR_BINDS_UNION: &str = r#"
{
    type E = [`A(i64, i64), `A(string, i64)];
    let f = |v: [`A(i64), `B([i64, string]), `C]| select v {
        `A(x) | `B(x) => select x { i64 as n => n, string as _ => 7 },
        _ => 0
    };
    let g = |v: E| select v { `A(x, 1) | `A(_, x) => select x { i64 as n => n, string as _ => 9 } };
    [f(`B("s")), f(`A(3)), g(`A("s", 1)), g(`A(2, 5))]
}
"#;

run!(or_binds_union, OR_BINDS_UNION, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        as_i64s(a) == [7, 3, 9, 5]
    }
    _ => false,
}; FuseExpect::Jit);

// A wake keeps a `<-` target's last write but recomputes the target's
// destructured siblings.
const WAKE_RECOMPUTES_A_TARGETS_SIBLINGS: &str = r#"
{
    let n = array::iter([0, 1, 2, 3, 4]);
    let z = uniq(select n { 0 => 0, _ => 5 });
    let r = select n % 2 {
        0 => { let (a, b) = (0, z + 100); a <- n ~ a + 1; (a, b) },
        _ => (-1, z)
    };
    array::group(r, |i, _| i == 5)
}
"#;

run!(
    wake_recomputes_a_targets_siblings,
    WAKE_RECOMPUTES_A_TARGETS_SIBLINGS,
    prints(
        "[[i64:0, i64:100], [i64:-1, i64:5], [i64:1, i64:105], [i64:-1, i64:5], [i64:2, i64:105]]"
    )
);

// One walk decides what reaches each arm: binds narrow to it, an arm that
// can match none of it is dead, and nothing may reach past the last arm.

// An all-bind arm over a union narrows each bind to the union of its parts
// over every member it can match: it covers them all, its binds hold either
// member's part, and a second such arm is dead.
run!(coverage_all_bind_covers_every_member, r#"{
    let g = |p: [(i64, string), (string, i64)]| select p { (a, b) => "[a] [b]" };
    (g((1, "s")), g(("s", 1)))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::from("1 s"), Value::from("s 1")]); FuseExpect::Jit);

run!(coverage_all_bind_struct_covers_every_member, r#"{
    let g = |p: [{x: i64, y: string}, {x: string, y: i64}]| select p { {x, y} => "[x] [y]" };
    (g({x: 1, y: "s"}), g({x: "s", y: 1}))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::from("1 s"), Value::from("s 1")]); FuseExpect::Jit);

run!(coverage_all_bind_second_arm_dead, r#"{
    let g = |p: [(i64, string), (string, i64)]| select p { (a, b) => 0, (c, d) => 1 };
    g((1, "s"))
}"#, refused("unreachable arm"); FuseExpect::None);

run!(coverage_all_bind_part_is_a_union, r#"{
    let g = |p: [(i64, string), (string, i64)]| select p { q@ (_, _) => q.0 };
    let r: i64 = g(("s", 1));
    r
}"#, refused("i64 does not contain [i64, string]"); FuseExpect::None);

run!(coverage_slice_bind_covers_every_member, r#"{
    let f = |p: [Array<i64>, Array<string>]| select p { [x] => "one", _ => "other" };
    let h = |p: [Array<i64>, Array<string>]| select p { [h, rest..] => 1, [] => 0 };
    let l = |p: [List<i64>, List<string>]| select p { [<h, rest..>] => 1, [<>] => 0 };
    (f(["s"]), h(["s"]), l([<"s">]))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::from("one"), Value::I64(1), Value::I64(1)]); FuseExpect::None);

// Under a written union, a constructor pattern's binds take the union of
// their parts over every member it matches, as under an inferred one.
run!(written_union_binds_every_member, r#"{
    type P = [(i64, string), (string, i64)];
    let f = |a: [P, null]| select a { P as (x, y) => "[x] [y]", null => "none" };
    let v: P = ("s", 1);
    let w: P = (2, "t");
    "[f(v)] [f(w)] [f(null)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s 1 2 t none"); FuseExpect::Jit);

run!(written_union_bind_is_a_union, r#"{
    type P = [(i64, string), (string, i64)];
    let f = |a: [P, null]| select a { P as (x, y) => x + 1, null => 0 };
    let v: P = ("s", 1);
    f(v)
}"#, refused("cannot compute [i64, string] + i64"); FuseExpect::None);

// A constructor pattern nested in an array, tuple, struct, list or payload
// tells apart members of one runtime form by their parts, as at the top:
// ("S", "x") is not `S(i64).
run!(nested_constructor_tells_runtime_forms_apart, r#"{
    type U = [(string, string), `S(i64)];
    let p: U = ("S", "x");
    let a = select [p] { [`S(n)] => "S [n]", _ => "other" };
    let b = select (p, 0) { (`S(n), _) => "S [n]", _ => "other" };
    let c = select {f: p} { {f: `S(n)} => "S [n]", _ => "other" };
    let d = select [<p>] { [<`S(n)>] => "S [n]", _ => "other" };
    let e = select `W(p) { `W(`S(n)) => "S [n]", _ => "other" };
    let q: U = `S(4);
    let g = select [q] { [`S(n)] => "S [n]", _ => "other" };
    "[a] [b] [c] [d] [e] [g]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "other other other other other S 4"); FuseExpect::Jit);

// A partial struct pattern completes against the members that reach its
// arm: one an earlier arm matched does not make it ambiguous.
run!(partial_struct_completes_against_what_reaches, r#"{
    let v: [{x: i64, y: string}, {x: i64, z: f64}] = {x: 2, z: 1.5};
    let w: [`A({x: i64, y: string}), `A({x: i64, z: f64})] = `A({x: 3, z: 1.5});
    let a = select v { {x, y} => x, s@ {x, ..} => x + 10 };
    let b = select w { `A({x, y}) => x, `A({x, ..}) => x + 10 };
    (a, b)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(12), Value::I64(13)]); FuseExpect::Jit);

run!(partial_struct_ambiguous_at_its_arm, r#"{
    let v: [{x: i64, y: string}, {x: i64, z: f64}] = {x: 2, z: 1.5};
    select v { s@ {x, ..} => x, _ => 0 }
}"#, refused("matches more than one member"); FuseExpect::None);

// A partial struct pattern pools against the one member holding its
// fields, its unnamed fields matching anything: arms naming different
// fields pool together, a later bind narrows, a later arm can be dead.
run!(coverage_partial_struct_pool, r#"{
    let f = |s: {a: bool, b: i64}| select s { {a: true, ..} => 1, {a: false, ..} => 2 };
    f({a: false, b: 3})
}"#, is(Value::I64(2)); FuseExpect::Jit);

run!(coverage_partial_struct_subsets, r#"{
    let f = |v: {a: bool, b: [`X, `Y]}| select v {
        {a: true, ..} => 1, {b: `X, ..} => 2, {a: false, b: `Y} => 3
    };
    (f({a: true, b: `Y}), f({a: false, b: `X}), f({a: false, b: `Y}))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(1), Value::I64(2), Value::I64(3)]); FuseExpect::Jit);

run!(coverage_partial_struct_narrows, r#"{
    type S = {c: string, d: [`A, `B]};
    let s: [i64, S] = 3;
    select s { {d: `A, ..} => 10, {d: `B, ..} => 20, n => n + 1 }
}"#, is(Value::I64(4)); FuseExpect::Jit);

run!(coverage_partial_struct_dead, r#"{
    type S = {c: string, d: [`A, `B]};
    let s: S = {c: "x", d: `A};
    select s { {d: `A, ..} => 10, {d: `B, ..} => 20, _ => 0 }
}"#, refused("unreachable arm"); FuseExpect::None);

run!(coverage_partial_struct_open, r#"{
    let f = |s: {a: bool, b: i64}| select s { {a: true, ..} => 1 };
    f({a: true, b: 3})
}"#, refused("missing match cases"); FuseExpect::None);

// An irrefutable arm with a head pools with the literal arms.
run!(coverage_mixed_pool, r#"{
    let f = |v: (bool, [`A, `B])| -> i64 select v { (true, `A) => 1, (b, `B) => 2, (false, `A) => 3 };
    f((false, `B))
}"#, is(Value::I64(2)); FuseExpect::Jit);

// The full grid of heads covers its product.
run!(coverage_head_grid, r#"{
    let f = |v: ([`X, `Y], [`A, `B])| -> i64 select v {
        (`X, `A) => 1, (`X, `B) => 2, (`Y, `A) => 3, (`Y, `B) => 4
    };
    f((`Y, `B))
}"#, is(Value::I64(4)); FuseExpect::Jit);

// A nested or-pattern names a set of heads at its position.
run!(coverage_nested_or, r#"{
    let f = |v: (bool, [`A, `B])| -> i64 select v { (true, `A | `B) => 1, (false, _) => 2 };
    #[native] f((true, `B))
}"#, is(Value::I64(1)); FuseExpect::Jit);

// A later bind does not hold what an earlier `null` arm took.
run!(
    coverage_narrows_past_null,
    r#"{
    let x: [i64, null] = 3;
    select x { null => 0, v => v + 1 }
}"#,
    is(Value::I64(4))
);

// a covered product leaves the catch-all nothing
run!(covered_product_leaves_catch_all_nothing, "{ let f = |v: ([`X, `Y], [`A, `B])| -> i64 select v { (`X, `A) => 1, (`X, `B) => 2, (`Y, `A) => 3, (`Y, `B) => 4, _ => 5 }; f((`Y, `B)) }",
    refused("unreachable arm"); FuseExpect::None);

// a type test that does not hold the member covers none of it
run!(type_test_not_holding_member_covers_none, "{ let v: (bool, [i64, string]) = (true, 1); select v { (bool, i64) as (true, x) => 1, (bool, string) as (false, x) => 2 } }",
    refused("missing match cases"); FuseExpect::None);

// positions are paths, not indexes
run!(coverage_positions_are_paths, "{ let f = |v: ((bool, bool), (bool, bool))| select v { ((true, _), _) => 1, (_, (false, _)) => 2, ((_, true), _) => 3 }; f(((false, true), (true, false))) }",
    refused("missing match cases"); FuseExpect::None);

// a repeat matches nothing new
run!(repeated_literal_arm_unreachable, "{ let x = 0; select x { 0 => 1, 0 => 2, _ => 3 } }",
    refused("unreachable arm"); FuseExpect::None);

run!(repeated_bool_arm_unreachable, "{ let t = true; select t { true => 1, true => 2, false => 3 } }",
    refused("unreachable arm"); FuseExpect::None);

run!(repeated_tuple_arm_unreachable, "{ let p = (true, false); select p { (true, _) => 1, (true, false) => 2, (false, _) => 3 } }",
    refused("unreachable arm"); FuseExpect::None);

run!(repeated_payload_arm_unreachable, "{ let f = |v: [`A(i64), `B]| select v { `A(0) => 1, `A(0) => 2, `A(_) => 3, `B => 4 }; f(`A(0)) }",
    refused("unreachable arm"); FuseExpect::None);

// A guarded arm before an unguarded one over a payload holding null leaves
// the unguarded arm's bind its whole type.
run!(
    coverage_guarded_nullable_payload,
    r#"{
    let v0: [`C([string, null]), `Some] = `C(null);
    select v0 { `C(x) if false => 0, `C(v1) => select v1 { null => 2, s => 3 }, `Some => 5 }
}"#,
    is(Value::I64(2))
);

// Each slice element, each or-alternative and a slice's rest is typed over
// the scrutinee, as a separate arm would be.
run!(
    or_in_slice_element,
    r#"{
    let a: Array<[i64, null]> = [null, 2];
    let b: Array<[`A, `B]> = [`B];
    let x = #[native] select a { [x, 1 | 2] => select x { null => 10, i64 as n => n }, _ => 9 };
    let y = #[native] select b { [`B | `A] => 1, _ => 0 };
    x + y
}"#,
    is(Value::I64(11))
);

run!(
    slice_elements_typed_apart,
    r#"{
    type S = {a: i64, b: i64};
    let f = |a: Array<i64>| select a { [x, _] => x + 1, _ => 0 };
    let g = |a: Array<(i64, i64)>| select a { [_, (x, y)] => x + y, _ => 0 };
    let h = |a: Array<[`A, `B]>| select a { [`A, `B] => 1, _ => 0 };
    let i = |a: Array<(i64, i64)>| select a { [(1, x), (y, 2)] => x + y, _ => 0 };
    let j = |a: Array<S>| select a { [{b, ..}, x] => b + x.a, _ => 0 };
    let k = |a: List<i64>| select a { [<x, _, t..>] => x + 1, _ => 0 };
    [f([4, 5]), g([(1, 2), (3, 4)]), h([`A, `B]), i([(1, 5), (6, 2)]), j([{a: 1, b: 2}, {a: 3, b: 4}]), k([<4, 5>])]
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [5, 7, 1, 11, 5, 5]
        }
        _ => false,
    }
);

run!(
    slice_rest_scrutinee_type,
    r#"{
    type Tok = [`Open, `Close, `Num(i64)];
    let f = |xs: Array<[i64, null]>| select xs { [0, rest..] => array::len(rest), _ => 0 - 1 };
    let g = |xs: [Array<[i64, null]>, `A(i64)]| select xs { [0, rest..] => array::len(rest), _ => 0 - 1 };
    let h = |ts: Array<Tok>| select ts {
        [`Open, rest..] => select rest { [`Num(n), ..] => n, _ => 0 },
        _ => 0 - 1
    };
    [f([0, 5, null, 7]), g([0, 5, null, 7]), h([`Open, `Num(4), `Close])]
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [3, 3, 4]
        }
        _ => false,
    }
);

run!(
    or_wild_per_member,
    r#"{
    type U = [(i64, i64), (i64, i64, i64)];
    let f = |t: U| select t { (x, _) | (x, _, _) => x };
    f((5, 6)) + f((7, 8, 9))
}"#,
    is(Value::I64(12));
    FuseExpect::None
);

run!(
    or_capture_and_payload,
    r#"{
    let f = |v: [`A([i64, `B]), `B]| select v {
        `A(x) | x@ `B => select x { `B => "b", i64 as n => "i" }
    };
    "[f(`A(5))][f(`B)][f(`A(`B))]"
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "ibb");
    FuseExpect::None
);

// a rest shared by or-alternatives is typed from the scrutinee
run!(
    or_rest_in_alternatives,
    r#"{
    let f = |a: Array<[i64, string]>| select a {
        [1, r..] | [r.., "a"] => array::fold(r, "", |acc, x| select x {
            i64 as _ => "[acc]i",
            string as _ => "[acc]s"
        }),
        _ => "-"
    };
    "[f([1, 2])][f(["x", "a"])][f([1, "a"])][f([3])]"
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "iss-");
    FuseExpect::Jit
);

// a rest is typed from the scrutinee, not the elements beside it
run!(rest_typed_from_scrutinee, "{ let f = |xs: Array<[i64, null]>| select xs { [0, rest..] => array::fold(rest, 0, |a, x| a + x), _ => 0 }; f([0, null]) }",
    refused("cannot compute"); FuseExpect::None);

// an element bind is the scrutinee's element type
run!(element_bind_is_scrutinee_element, "{ let f = |a: Array<[i64, string, f64]>| select a { [1, x] => select x { i64 as n => 0, f64 as d => 1 }, _ => 2 }; f([1, \"q\"]) }",
    refused("missing match cases"); FuseExpect::None);

// an alternative's cells are not decided by another's literals
run!(alternative_cells_not_decided_by_another, "{ let f = |v: ([i64, null], [i64, null])| select v { p@ (0, y) | p@ (y, 0) => y + 1, _ => 0 }; f((0, null)) }",
    refused("cannot compute"); FuseExpect::None);

run!(slice_alternative_cells_not_decided_by_another, "{ let a: Array<[i64, null]> = [null, 1]; select a { [x, 1 | 2] => x + 1, _ => 0 } }",
    refused("cannot compute"); FuseExpect::None);

// a shared name is typed over what every alternative can match
run!(shared_name_typed_over_every_alternative, "{ let f = |v: [`A(i64, i64), `A(string, i64)]| -> i64 select v { `A(x, 1) | `A(_, x) => x }; f(`A(\"s\", 1)) }",
    refused("does not contain"); FuseExpect::None);

// an or-arm is a wildcard only through a bare name
run!(or_arm_wildcard_only_through_bare_name, "{ let f = |t: [(i64, i64), (i64, i64, i64)]| select t { (x, 0, _) | (x, _) => x }; f((7, 8, 9)) }",
    refused("missing match cases"); FuseExpect::None);

// a tuple alternative and a struct alternative are told apart
run!(alternative_taken_by_structure, "{ let f = |t: [(i64, i64), {x: i64, y: i64}]| select t { (x, _) | {x, ..} => x }; f({x: 1, y: 2}) }",
    is(Value::I64(1)); FuseExpect::None);

// A woken arm's constants stand (through any operator, call, builtin,
// select, formal or collection loop), so they never rewrite a `<-`
// target: each column is a target written twice, then slept (n = 3) and
// woken (n = 4).
const WAKE_FIRE_KEEPS_TARGETS: &str = r#"
{
    let n = array::iter([0, 1, 2, 3, 4, 5]);
    let tab = uniq(select n { 3 => `B, _ => `A });
    let click = select n { 1 => null, 2 => null, _ => never() };
    let f = |x: i64| x * 2;
    let g = |#start: i64 = 2, ev: Any| -> i64 { let c = start; c <- ev ~ c + 10; c };
    let h = |ev: Any| -> i64 {
        let c = array::fold([1, 1], 0, |a, x| a + x);
        c <- ev ~ c + 10;
        c
    };
    let r = select tab {
        `A => [
            { let c = array::len(array::map([1, 2], |x| x)); c <- click ~ c + 10; c },
            { let c = array::fold([1, 1], 0, |a, x| a + x); c <- click ~ c + 10; c },
            { let c = array::len(array::filter([1, 2, 3], |x| x > 1)); c <- click ~ c + 10; c },
            { let c = array::find([2, 3], |x| x > 1)$; c <- click ~ c + 10; c },
            {
                let c = array::fold(array::map([[1], [1]], |a| array::len(a)), 0, |a, x| a + x);
                c <- click ~ c + 10;
                c
            },
            h(click),
            { let c = 1 + 1; c <- click ~ c + 10; c },
            { let c = f(1); c <- click ~ c + 10; c },
            { let c = str::len("ab"); c <- click ~ c + 10; c },
            { let c = {a: 2, b: 3}.a; c <- click ~ c + 10; c },
            { let c = select 0 { 0 => 2, _ => 3 }; c <- click ~ c + 10; c },
            { let c = array::len([1, 2]); c <- click ~ c + 10; c },
            g(click),
            select 2 { s => { let c = s; c <- click ~ c + 10; c } }
        ],
        `B => []
    };
    select n { 5 => r, _ => never() }
}
"#;

run!(wake_fire_keeps_targets, WAKE_FIRE_KEEPS_TARGETS, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        as_i64s(a) == [22; 14]
    }
    _ => false,
});

// The initializer's inputs that cannot reach its production this cycle
// (an untaken arm's) do not make a wake's fire real.
const WAKE_UNREACHED_INPUT_KEEPS_TARGET: &str = r#"
{
    let n = array::iter([0, 1, 2, 3, 4, 5]);
    let o = uniq(select n { 2 => false, 3 => false, _ => true });
    let c = true;
    let ev = select n { 1 => 50, _ => never() };
    let w = select n { 9 => 7, _ => never() };
    let v = select n { 4 => 7, _ => never() };
    let bot = select o { true => { let y = select c { true => 5, false => w }; y <- ev; y }, false => -1 };
    let live = select o { true => { let y = select c { true => 5, false => v }; y <- ev; y }, false => -1 };
    let ctl = select o { true => { let y = select c { true => 5, false => 6 }; y <- ev; y }, false => -1 };
    select n { 5 => (bot, live, ctl), _ => never() }
}
"#;

run!(
    wake_unreached_input_keeps_target,
    WAKE_UNREACHED_INPUT_KEEPS_TARGET,
    prints("[i64:50, i64:50, i64:50]")
);

// An inner arm that wakes a cycle after its enclosing arm still finds the
// enclosing arm's constants standing.
const WAKE_FIRE_CAUGHT_UP_LATE: &str = r#"
{
    let n = array::iter([0, 1, 2, 3, 4, 5, 6]);
    let o = uniq(select n { 2 => false, 3 => false, _ => true });
    let late = uniq(select n { 3 => false, 4 => false, _ => true });
    let same = uniq(select n { 3 => false, _ => true });
    let r_late = select o {
        true => {
            let y = 5;
            select late { true => { let x = y; x <- select n { 1 => 50, _ => never() }; x + y }, false => 0 }
        },
        false => -1
    };
    let r_same = select o {
        true => {
            let y = 5;
            select same { true => { let x = y; x <- select n { 1 => 50, _ => never() }; x + y }, false => 0 }
        },
        false => -1
    };
    select n { 6 => (r_late, r_same), _ => never() }
}
"#;

run!(wake_fire_caught_up_late, WAKE_FIRE_CAUGHT_UP_LATE, prints("[i64:55, i64:55]"));

// A woken arm's inner select catches up a fire its new arm never saw even
// when the input's `let` republished quietly at the wake.
const WAKE_QUIET_REPUBLISH_CATCHES_UP: &str = r#"
{
    let n = 0;
    n <- select n { k if k < 12 => k + 1, _ => never() };
    let x = 0;
    x <- select n { 2 => 100, _ => never() };
    let phase = select n { k if k < 5 => `X, k if k < 8 => `Y, _ => `X };
    let t = select n { k if k < 7 => `A, _ => `B };
    let inside = 0;
    let a = select phase {
        `X => {
            let v = x * x;
            select t { `A => 0, `B => inside <- v ~ (inside + 1) }
        },
        `Y => never()
    };
    let outside = 0;
    let w = x * x;
    let b = select phase {
        `X => select t { `A => 0, `B => outside <- w ~ (outside + 1) },
        `Y => never()
    };
    let formal = 0;
    let f = |v| select t { `A => 0, `B => formal <- v ~ (formal + 1) };
    let c = select phase { `X => f(x * x), `Y => never() };
    select n { 12 => (inside, outside, formal), _ => never() }
}
"#;

run!(wake_quiet_republish_catches_up, WAKE_QUIET_REPUBLISH_CATCHES_UP, |v: Result<
    &Value,
>| format!(
    "{}",
    v.unwrap()
) == "[i64:1, i64:1, i64:1]");

// Under a written type, a constructor pattern at a union narrows to its
// member, and its own test decides it: it covers only a position whose
// type is that constructor.
run!(
    written_type_constructor_at_union,
    r#"{
    type Box = Abstract<[`A(i64), `B(i64)]>;
    type U = [(i64, i64), (i64, i64, i64)];
    let f = |a: Array<[`A, `B]>| select a { Array<[`A, `B]> as [`A, `B] => 1, _ => 0 };
    let g = |b: Box| select b { Box(`A(x)) => x, Box(`B(y)) => y * 10 };
    let h = |b: Box| select b { Box(`A(x)) => x, _ => 7 };
    let k = |t: U| select t { U as (x, _) | (x, _, _) => x };
    [f([`A, `B]), f([`B, `A]), g(Box(`A(3))), g(Box(`B(3))), h(Box(`B(3))), k((5, 6)), k((8, 0, 0))]
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            as_i64s(a) == [1, 0, 3, 30, 7, 5, 8]
        }
        _ => false,
    }
);

// a constructor over a union is refutable
run!(constructor_over_union_refutable_in_let, "{ let v: [`A(i64), `B] = `B; let `A(x) = v; x }",
    refused("refutable patterns are not allowed in let"); FuseExpect::None);

// a slice of tuples under a written type covers no length of a
// union of tuple shapes
run!(written_slice_of_tuples_covers_no_union_length, "{ type U = [(i64, i64), (i64, i64, i64)]; let f = |a: Array<U>| select a { Array<U> as [(x, _)] => x, [] => 0, [_, _, ..] => 9 }; f([(1, 2)]) }",
    refused("missing match cases"); FuseExpect::None);

// a variant's structure test fails a tuple of its shape
run!(variant_test_fails_a_tuple_of_its_shape, "{ let v: [`A(i64), (string, i64)] = (\"A\", 5); select v { [`A(i64), (string, i64)] as `A(x) => x, _ => 0 } }",
    is(Value::I64(0)); FuseExpect::Jit);

// An abstract's parameters are invariant: a value built as one
// instantiation passes a type test only for that instantiation.
run!(
    abstract_params_test_invariantly,
    r#"{
    type Box<'a> = Abstract<'a>;
    let f = |x: [Box<[i64, string]>, Box<i64>]| select x {
        Box<[i64, string]> as a => "wide",
        Box<i64> as b => "narrow"
    };
    let n: Box<i64> = Box(1);
    f(n)
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "narrow")
);

run!(
    abstract_ref_params_test_invariantly,
    r#"{
    type Cell<'a> = Abstract<&mut 'a>;
    let x: i64 = 41;
    let ci: Cell<i64> = Cell(&mut x);
    let u: [Cell<[i64, null]>, Cell<i64>] = ci;
    let r = select u {
        Cell<[i64, null]> as b => { let p = b.0; *p <- null; "arm1" },
        Cell<i64> as c => "arm2"
    };
    (r, x + 1)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"["arm2", i64:42]"#)
);

run!(
    abstract_any_test_is_invariant,
    r#"{
    type C<'a> = Abstract<&mut 'a>;
    let y: i64 = 1;
    let p: Any = C(&mut y);
    let r = select p {
        C<[i64, string]> as q => { let rr = q.0; *rr <- "hello"; 0 },
        _ => 1
    };
    (r, y + 1)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:2]")
);

// An abstract value has no primitive form, and a cast's refusal prints
// no payload, which could hold a reference's session id.
run!(
    abstract_cast_refused_without_payload,
    r#"{
    type Cell = Abstract<&i64>;
    let x: i64 = 41;
    let c = Cell(&x);
    (cast<u64>(c), cast<string>(c))
}"#,
    |v: Result<&Value>| {
        let why = "an abstract value has no primitive form";
        matches!(v, Ok(v) if v.to_string() == format!(
            r#"[error:["InvalidCast", "can't cast Cell(…) to u64: {why}"], error:["InvalidCast", "can't cast Cell(…) to string: {why}"]]"#
        ))
    }
);

// A runtime test can't tell a reference's mutability, so an arm may not
// take a `&mut` out of a union that also holds a read-only reference.
run!(select_refuses_mixed_mutability, r#"{
    let x = 1;
    let f = |r: &i64| {
        let u: [&i64, &mut i64] = r;
        select u { &mut i64 as m => { *m <- 5; "wrote" }, &i64 as s => "shared" }
    };
    f(&x)
}"#, refused("this pattern can't tell &mut i64 from &i64"); FuseExpect::None);

// A recursive pair met again inside its own difference subtracts nothing:
// Error<T> less Error<U> keeps Error<Array<T>>, which no later arm covers.
run!(
    recursive_difference_keeps_what_it_did_not_cover,
    refused("no arm covers Error<Array<T>>"),
    "/test.gx" => r#"
        type T = [`B, Array<T>];
        type U = [`A, Array<U>];
        let f = |x: [Error<T>, i64]| select x {
          Error<U> as u => "U",
          v => select v { i64 as n => "int", Error<`B> as e => "B-error" }
        };
        let result = f(error([`B]))
    "#
; FuseExpect::None);

// A guard sees the scrutinee's value only where the chain reaches its
// arm: an earlier arm that claims the value keeps it from every guard
// below, so `check(-1)?` never runs and never raises.
// CR claude for eric: [risk/low] This pin holds only for the bind spelling. With the
// guard written over the scrutinee variable, `_ if check(x)?`, the guard still ticks
// every cycle, so check(-1)? runs on every -1 that arm 0 claimed and raises to the
// catch: caught ends at 3 instead of 0, on both engines. The two spellings mean the
// same thing to a reader but differ here. Rule whether a guard below a claiming arm may
// raise in that cycle; then either change the guard's evaluation or narrow this comment
// and pin the variable spelling too. probe:
// design/review-2026-10-19/repro/tests-select-guard-reads-claimed-scrutinee.gx
// (tests-select-05)
run!(guard_below_the_claim_never_sees_the_value, r#"{
  let step = 0;
  step <- select step { n if n < 6 => n + 1, _ => never() };
  let x = step % 3 - 1;
  let caught = 0;
  let check = |n: i64| select n { k if k < 0 => error(`Neg(k)), k => k > 0 };
  let r = {
    catch(e) caught <- e ~ caught + 1;
    select x { -1 => "neg", n if check(n)? => "pos", _ => "zero" }
  };
  select step { 6 => (r, caught), _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["neg", i64:0]"#)
}; FuseExpect::Jit);

// The same for a guard whose arithmetic would fail on the claimed value.
run!(guard_below_the_claim_never_divides_by_it, r#"{
  let step = 0;
  step <- select step { n if n < 4 => n + 1, _ => never() };
  let x = step % 2;
  let caught = 0;
  let r = {
    catch(e) caught <- e ~ caught + 1;
    select x { 0 => "zero", n if (10 /? n)? > 5 => "big", _ => "small" }
  };
  select step { 4 => (r, caught), _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["zero", i64:0]"#)
}; FuseExpect::Jit);

// A guard below the claim still ticks on its other inputs: `count` sees
// every fire of `t` while its arm is not reached.
run!(guard_below_the_claim_still_ticks, r#"{
  let t = 0;
  t <- select t { n if n < 4 => n + 1, _ => never() };
  let x = select t { 4 => 2, _ => 1 };
  let r = select x { 1 => "one", n if count(t) > 3 => "counted [n]", _ => "other" };
  select t { 4 => r, _ => never() }
}"#, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "counted 2")
}; FuseExpect::Jit);

// The kernel's guard prologue reaches an impure guard past an unguarded
// arm, an or-arm and a pure guard, each of which may claim the value.
run!(native_guard_prologue_follows_the_chain, r#"{
  let f = |x: i64| -> string #[native] select x {
    0 => "zero",
    1 | 2 => "small",
    n if n < 5 => "pure",
    n if str::len(str::concat("a", "b")) + n > 10 => "impure",
    _ => "other"
  };
  array::map([0, 1, 2, 3, 4, 5, 6, 9, 20], f)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["zero", "small", "small", "pure", "pure", "other", "other", "impure", "impure"]"#)
}; FuseExpect::Jit);

// An exhaustive struct pattern matches only a struct of exactly its
// fields: `{x, y}` names every field of the first member alone, so the
// second member reaches past it.
run!(struct_pattern_width_uncovered, r#"{
  type U = [{x: i64, y: string}, {x: i64, y: i64, z: string}];
  let f = |w: U| select w { U as {x, y} => "[y]!" };
  f({x: 1, y: "s"})
}"#, refused("missing match cases"); FuseExpect::None);

run!(struct_pattern_width_exact, r#"{
  type U = [{x: i64, y: string}, {x: i64, y: i64, z: string}];
  let f = |w: U| select w { U as {x, y} => "[y]!", _ => "other" };
  array::map([{x: 1, y: "s"}, {x: 1, y: 42, z: "z"}], f)
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["s!", "other"]"#)
}; FuseExpect::Jit);

// Each alternative takes the member of its own width, so the second
// alternative binds `y` to the string field `z`.
run!(struct_pattern_width_or, r#"{
  type U = [{x: i64, y: string}, {x: i64, y: i64, z: string}];
  let vs: Array<U> = [{x: 1, y: "s"}, {x: 1, y: 42, z: "zz"}];
  array::map(vs, |v| select v { {x, y} | {x, y: _, z: y} => "[y]!" })
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == r#"["s!", "zz!"]"#)
}; FuseExpect::Jit);

// A slice pattern's element cells are the arm's own to narrow: over an
// unannotated parameter the open-member rule does not read them as types
// the arm must tell apart.
run!(generic_slice_arms_check, r#"{
  let is_empty = |a| select a { [] => true, [_, ..] => false };
  let first = |a| select a { [h, ..] => h, [] => never() };
  let head = |l| select l { [<h, t..>] => h, [<>] => 0 };
  (is_empty([1, 2]), first([1, 2]), head([<1>]))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[false, i64:1, i64:1]")
}; FuseExpect::None);

// Under a written union a slice pattern narrows to the member it can
// match, as a tuple or variant pattern does.
run!(slice_pattern_under_written_union, r#"{
  type T = Abstract<[Array<i64>, null]>;
  let f = |t: T| select t { T([x, ..]) => x, _ => 0 };
  let g = |a: [Array<i64>, null]| select a { [Array<i64>, null] as [x, ..] => x, _ => 0 };
  (f(T([1])), g([2]), g(null))
}"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:2, i64:0]")
}; FuseExpect::Jit);

// A guard the chain passed by when the scrutinee fired is given the
// value fired when a guard flip above it lets the chain reach it: `x > 5`
// recomputes over 0 rather than riding its answer for 10.
run!(passed_guard_recomputes_over_the_value_it_missed, r#"{
  let in0 = 10;
  let in1 = true;
  let step = 0;
  step <- select step { k if k < 3 => (k ~ step) + 1, _ => never() };
  in0 <- select step { 1 => step ~ 0, _ => never() };
  in1 <- select step { 2 => step ~ false, _ => never() };
  let r = select in0 { 0 if in1 => "zero-g", x if x > 5 => "big", _ => "other" };
  select step { 3 => r, _ => never() }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "other"); FuseExpect::Jit);

// The literal pool pools an arm against the member of its shape it can
// match: over two same-arity tuple members, the `A/`B arms cover the
// second, not the first.
run!(pool_takes_the_member_an_arm_matches, r#"{
  let f = |x: [(bool, i64), ([`A, `B], string)]| select x {
    (`A, s) => "a",
    (`B, s) => "b",
    (true, n) => "t",
    (false, n) => "f"
  };
  [f((true, 1)), f((false, 1)), f((`A, "s")), f((`B, "s"))]
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"["t", "f", "a", "b"]"#); FuseExpect::None);

// An abstract payload's heads are the scrutinee member's, not the
// pattern's own: `Box(`A)` alone leaves `B uncovered.
run!(pool_reads_the_abstract_payload_of_the_scrutinee, r#"{
  type Box<'a> = Abstract<'a>;
  let f = |x: Box<[`A, `B]>| select x { Box(`A) => 1 };
  f(Box(`A))
}"#, refused("missing match cases: no arm covers Box<[`A, `B]>"); FuseExpect::None);

// Bool heads under Box<bool> cover Box<bool>, never Box<i64>.
run!(pool_covers_only_its_own_abstract_instantiation, r#"{
  type Box<'a> = Abstract<'a>;
  let g = |x: [Box<bool>, Box<i64>]| select x {
    Box(true) => 1,
    Box(false) => 2,
    Box<bool> as b => 3
  };
  g(Box(true))
}"#, refused("missing match cases: no arm covers Box<i64>"); FuseExpect::None);

// An inferred abstract constructor pattern tests the tag at run time and
// takes its parameters from the scrutinee, never from its payload.
run!(abstract_pattern_params_from_the_scrutinee, r#"{
  type Box<'a> = Abstract<'a>;
  let f = |x: Box<[i64, null]>| select x { Box(null) => 0, Box(n) => 1 };
  let g = |x: Box<[`A, `B]>| select x { Box(`A) => 1, Box(`B) => 2 };
  [f(Box(null)), f(Box(5)), g(Box(`A)), g(Box(`B))]
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:1, i64:1, i64:2]"); FuseExpect::None);

run!(abstract_capture_typed_from_the_scrutinee, r#"{
  type Box<'a> = Abstract<'a>;
  let g = |b: Box<[i64, null]>| select b { Box(v) => v };
  let f = |x: Box<[i64, null]>| select x { all@ Box(null) => g(all), Box(n) => n };
  [f(Box(null)), f(Box(4))]
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[null, i64:4]"); FuseExpect::None);

// Over two instantiations an irrefutable payload matches and covers both.
run!(abstract_pattern_over_two_instantiations, r#"{
  type T<'a> = Abstract<'a>;
  let f = |v: [T<i64>, T<string>]| select v { T(x) => "T [x]" };
  let g = |x: [T<bool>, T<i64>]| select x { T(true) => 1, T(false) => 2, T(n) => cast<i64>(n)$ };
  (f(T(1)), f(T("s")), g(T(3)), g(T(true)), g(T(false)))
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"["T 1", "T s", i64:3, i64:1, i64:2]"#); FuseExpect::Jit);

// A null literal is a head the literal pool reads, beside variants and
// bools: option payload arms cover their position.
run!(null_literal_pools_beside_variants, r#"{
  let f = |x: `P([`A, null])| select x { `P(`A) => 1, `P(null) => 0 };
  let g = |x: ([`A, null], i64)| select x { (`A, n) => n, (null, n) => 0 };
  [f(`P(`A)), f(`P(null)), g((`A, 5)), g((null, 2))]
}"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:0, i64:5, i64:0]"); FuseExpect::Jit);

// A literal pattern is typed as the same literal in an expression is.
run!(an_error_literal_pattern_is_typed_by_its_payload, r#"{
  let r: [i64, Error<string>] = error("x");
  select r { error:1 => 1, _ => 2 }
}"#, refused("will never match"); FuseExpect::None);

run!(an_abstract_literal_pattern_is_refused, r#"{
  let x: Any = 1;
  select x { abstract:EgABAgMEBQYHCAkKCwwNDg8BAg== => 1, _ => 2 }
}"#, refused("an abstract value literal has no type"); FuseExpect::None);

// A null literal at a position covers that position's null beside a
// type-tested arm, and leaves the rest open.
run!(
    null_literal_covers_beside_a_type_test,
    r#"{
  let h = |x: {a: [i64, null]}| select x { {a: null} => 0, {a: i64} as s => s.a };
  [h({a: null}), h({a: 3})]
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:3]")
);

run!(null_literal_alone_leaves_the_rest_open, r#"{
  let h = |x: {a: [i64, null]}| select x { {a: null} => 0 };
  h({a: null})
}"#, refused("missing match cases"); FuseExpect::None);
