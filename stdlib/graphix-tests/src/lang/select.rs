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

// ASPIRE: Jit (currently None) — blocked on: string interpolation in select expression
// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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

// ASPIRE: Jit (currently None) — blocked on: nested composite / variant payload composite
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

// Fuses since Select::fuse (2026-08-14): the scrutinee/arm sub-region
// descent — the never() arms de-fuse individually, the wildcard arm and
// scrutinee fuse.
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

// ASPIRE: Jit (currently None) — blocked on: nested composite / variant payload composite
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

// ASPIRE: Jit (currently None) — blocked on: nested composite / variant payload composite
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

// The nested destructure itself fuses now (`_` infers a fresh TVar), but
// THIS select's `_ => never()` arm body is async — a correct de-fuse for
// the select region (program-level Jit is satisfied by sibling regions).
// `select_ignore_sorts_first` covers the same pattern shape with a
// fusable catch-all.
run!(nestedmatch3, NESTEDMATCH3, |v: Result<&Value>| match v {
    Ok(Value::F64(3.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// =============================================================================
// #219 — a MISSING region input consumed only on a DEAD arm must yield a real
// value, not bottom. The scrutinee picks a live arm; the missing input (`x`,
// fed by `never()`) is referenced only on the un-taken arm. Pre-#219 the fused
// kernel bottomed on ANY missing input; now taint rides each input's disc and
// is forced only where the taken path consumes it. (The composite case is
// value-correct too but currently de-fuses — covered by the differential
// suite; these two fuse and exercise the in-kernel taint path.)
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

// =============================================================================
// Phase 4 — structural destructuring over a BORROWED composite scrutinee
// (tuple / struct / slice patterns with SCALAR leaves) fuses. The length
// test in each arm's structure condition doubles as the #219 taint gate
// (a missing composite input is an EMPTY placeholder, so it misses every
// length-tested arm and the miss trap yields the tainted bottom).
// Deferred (still de-fuse): whole-composite/@ binds, NAMED rest binds,
// nested structural leaves (nestedmatch3), owned-producer scrutinees.

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

// Wrong-length arms fall through (the length test misses [x] and [x,y,z]),
// landing on the catch-all.
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

// Anonymous-rest prefix `[x, ..]` (tail: None) fuses; a NAMED rest
// (`[x, rest..]`) still de-fuses (owned subslice arm local — deferred).
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

// Anonymous-head suffix `[.., x]` (head: None) — the leaf reads at
// `a[len - 1]`, a runtime-relative index.
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

// Empty-slice pattern: `[]` is `len == 0` — matched here by the empty
// array, with the sized arms falling through.
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

// A NAMED rest binding: the SELECT itself de-fuses (the subslice is an
// owned composite arm local; JitEnv::truncate emits no drops — deferred),
// but sibling regions (the array literal) still fuse, so the program-level
// expectation stays Jit. The de-fuse itself is pinned by
// `native_select_named_rest_defuses` in lib_tests/native.rs.
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

// Node-walk regression (found by the Phase 4 differential): SliceSuffix
// BINDS used start-relative offsets (`a[N..]`) while `is_match` tested the
// LAST N elements — `[init.., x]` over [7,8,9] bound x=8 (and init=[7])
// instead of x=9/init=[7,8]. Named `init..` de-fuses (owned subslice arm
// local), so this exercises the node-walk binder in both modes.
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

// The old start-relative suffix binds indexed OUT OF BOUNDS (a node-walk
// panic) when `suffix.len() <= len < 2 * suffix.len()`: `[.., x, y]` over a
// 2-element array read `tail = a[2..]` (empty) then `tail[0]`. With the
// fixed end-relative split it binds x=1, y=2.
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

// =============================================================================
// Slice-pattern LENGTH coverage (2026-08-21, from the admin-TUI
// campaign's fingerprint chunker): unguarded array-slice arms whose
// element patterns match anything jointly cover an array scrutinee
// when their lengths cover ℕ — `[]` + a rest form needs no wildcard.
// The claim is per scrutinee array member, and every pool arm's type
// predicate must contain the member (runtime dispatch is type-gated
// per arm, so a differently-typed slice arm is a hole, not coverage).
//
// ASPIRE: Jit (currently None) on the lambda-wrapped value fixtures —
// the select sits in an instance kernel where a composite scrutinee
// has no ride storage ("no scrutinee-ride storage — de-fuse"; the
// value-residents-in-site-blocks ASPIRE restores them).
// `select_slice_cover_fused` pins the region-root form natively.

// The region-root form: a wildcard-less slice-covered select fuses
// (the final arm's miss trap is dead code under the new coverage).
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
}; graphix_package_core::testing::FuseExpect::None);

// An exact-length ladder under the rest form: 0 and 1 by exact arms,
// [2, ∞) by the rest arm.
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

// The pool covers the ARRAY member; the null member still needs its
// own arm — and has one.
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

// A hole in the length ladder refuses (and the message names the
// hole): [] + [a, b, rest..] leaves length 1 uncovered.
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

// Exact-length arms alone cover finitely many lengths — never ℕ.
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

// A guard makes an arm's coverage conditional — it claims nothing.
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

// Deadness is length-precise too (Eric's call — no dead arms, ever):
// a wildcard behind a complete slice ladder is unreachable, exactly
// like a wildcard behind a full variant set.
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

// A slice arm whose whole length range is matched by earlier covering
// arms can never run: [init.., y] is [1, ∞), all taken by [x, rest..].
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

// The bool literal pair subtracts like a full variant set: a trailing
// wildcard after `true` + `false` is dead.
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

// The live side of the line: a PARTIAL ladder keeps its wildcard (the
// empty array still needs it), and a refutable-element arm neither
// dies (its lengths aren't covered yet where it stands) nor blocks
// the arms below it from completing coverage.
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
    graphix_package_core::testing::FuseExpect::None
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
    graphix_package_core::testing::FuseExpect::None
);

// A refutable ELEMENT pattern only matches some arrays of its length —
// the arm claims nothing.
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

// =============================================================================
// Phase 5 — NESTED structural select patterns (scalar leaf binds) fuse:
// the intermediate composite reads are BORROWED interior pointers (the
// root scrutinee is pinned borrowed across the arm chain and values are
// immutable), staged behind each level's length test.

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

// The nestedmatch3 shape: a struct pattern with a nested slice-prefix
// leaf. The select fuses fully now that `_` infers a fresh TVar (see
// `native_select_nested_struct_ok` — the old `Type::Any` inference
// short-circuited the unification walk at the sorted-first `_` fields).
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

// A LITERAL inside the nested level (second-stage staged test).
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

// =============================================================================
// Phase 6 — OWNED (fresh-producer) select scrutinees fuse in value
// position: the scrutinee is bound as an env local (a mid-arm pending
// exit drops it via drop_owned_composites) and dropped exactly once at
// the merge every normal path crosses. Tail-position selects keep the
// borrowed-only gate (no merge point).

// An inline tuple literal scrutinee (fresh producer = Owned).
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

// An inlined-HOF result as the scrutinee — the owned array flows from
// the map loop straight into the select's length dispatch.
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

// An owned VARIANT scrutinee (fresh constructor) with a scalar payload
// bind — the two-word owned Value drops at the merge.
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

// The no-match edge: the owned scrutinee still drops when the taken path
// is the catch-all (every arm's length test missed).
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

// =============================================================================
// `_` inference regression: `_` used to infer `Type::Any`, and select's
// unification-by-contains walk short-circuits at the first false pair
// (`T.contains(Any)` = false) — so every slot AFTER a `_` (positional in
// tuples, sorted-field order in structs) never narrowed its bind TVars,
// and those selects de-fused. `_` now infers a fresh TVar like an
// anonymous bind.

// `_` BEFORE the nested slot in a tuple (the p7 probe shape).
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

// Struct parent whose `_` fields sort FIRST (bar/baz < foo) — the
// nestedmatch3 shape with a fusable catch-all.
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

// The "gate stats until the window is non-empty" idiom
// (bench/stream_stats.gx): an Array local whose defining bind is a
// never()-gated select must thread into the downstream fold region as
// a kernel input. The never() arm's fresh TVar gets bound by the
// fold's own unification AFTER the select's arm-union type was built,
// leaving Set([TVar->Array<TVar->f64>, Array<f64>]) — structurally
// unmergeable, so the plain and normalize freezes both reject it and
// the local was silently skipped as a region input ("undefined local
// `w`", ~30x on the per-event stats). freeze_for_abi_normalized's
// resolve_tvars rung collapses it. The #[native] on the fold is the
// load-bearing assertion — program-level FuseExpect::Jit passes even
// unfixed via the sibling regions.
// windows (n=3): [1] -> [1,2] -> [1,2,3] -> [2,3,4]; final fold = 9.0
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

// The discovery leg of the same gap: a builtin call whose ARG is a
// never()-gated string local — the arg freeze also runs through the
// normalized path now, so the str::len site registers and fuses.
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

// ASPIRE: Jit (currently None) — the UNANNOTATED scalar gate, the
// fixture that motivated typing `never()` as Bottom (2026-07-05).
// The ⊥ typing fixed the SEMANTIC story (⊥ unifies everywhere, the
// connect-seed idiom works, `f(never(), 5)` accepts) but the fusion
// blocker turned out to be one level deeper: the never arm's call-
// site cell stays OPEN through the select's union (the (TVar, ⊥)
// rule deliberately doesn't bind — the seed idiom needs the cell
// open for writers), and the downstream arith's containment walk
// then binds it to the WIDE Number set — multiple register classes,
// no sound freeze. The remaining fix is converting the
// (Primitive, TVar-unbound) wide-bind rule to constrain-don't-bind
// (the next Phase-B-style conversion, design/tvar_constraints.md);
// with a Number CONJUNCT instead of a wide binding, the terminal
// settle would ⊥ the never cell and the union would collapse.
// Annotating the let (`let m: i64 = ...`) fuses today. Pinned so
// drift in either direction surfaces.
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
); graphix_package_core::testing::FuseExpect::None);

// A GUARDED arm before a bind-all final was rejected "missing match
// cases": the bind-all's inferred type predicate is a fresh TVar, and
// the coverage check's greedy unifying walk bound it to the FIRST
// scrutinee union member, leaving the rest "uncovered". Coverage now
// counts an inferred irrefutable pattern as the whole scrutinee type
// (found by fuzzer-v2 gen-check; guard-first arms are idiomatic — the
// TUI examples' key handlers are exactly this shape).
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

// A guard decides whether its arm matches, so it must be `bool`. Any
// type used to be accepted, and the arm then simply never matched — so
// `select n { v if n => a, _ => b }` (someone reaching for truthiness)
// compiled to a silently dead arm. The differential fuzzer cannot see
// this class at all: both engines agree on the dead arm, and the
// generator's guards are bool by construction. It surfaced when the
// minimizer's replace-with-a-literal operator put a string in a guard
// and the program still compiled (2026-08-09).
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

// A nullable bool is not a bool either: `[bool, null]` can't decide an
// arm, and admitting it would make the null case a silent non-match.
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

// The check INFERS as well as rejects: an unannotated lambda used as a
// guard binds its return tvar to bool, exactly as `!x` and `&&` do.
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

// The dual shape: the guarded arm names a DIFFERENT tag than the value.
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

// A select's result union built over an arm still holding an unbound
// `$`-result TVar never re-collapsed once the TVar bound: the field
// access then failed "expected struct not [{..}, {..}]" on two
// since-identical members. deref_typ! now normalizes a Set through
// the TVar-aware merge before giving up (found by fuzzer-v2 gen-check).
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

// Bind-all arm types narrow by position: the value reaching `s` cannot
// be null (the earlier unguarded irrefutable arm consumed it), so `s`
// is `string`, usable where a string is required. This came out right
// before only because the coverage walk happened to greedily bind the
// wildcard's tvar to the union's first member.
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

// A GUARDED select used to force its result FRESH on every kernel
// invocation ("over-fire, safe") — but firing is observable through
// `count`: with an unrelated reactive input in the region, the fused
// kernel counted every event (interp 1, jit 5). The select's STALE now
// also ANDs a guard-feeder word (any arm's guard input fired → the
// select may fire), computed path-independently before the arm chain.
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

// ORGANIC FIRING delta 2 (design/organic_firing.md, Eric 2026-08-14):
// a guard-dep fire EMITS regardless of whether the selection changes.
// m fires per x delivery, so the select emits 5 times (init + 4 guard
// fires) on both engines — the old selection-memory cadence (4) and
// the per-instance state word that produced it are gone.
// THE INIT-PHANTOM GUARD (activation_state.md, 2026-08-20): a guard
// that has NEVER produced (its deps deliver after init) is UNKNOWN,
// not false — the old `unwrap_or(false)` took the wildcard at init;
// under the bottom-out rule the chain stops undetermined and the
// select bottoms until the guard first becomes evaluable. Every
// fixture in this family loses exactly its init emission (5 → 4,
// 55 → 44): the count starts at the guard's first sound fire.
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

// Delta 2 inside a collection loop: the slot's select emits per guard
// fire (4; the init-phantom guard bottoms the init cycle), no
// per-slot selection memory involved — the
// structural context survives as organic-cadence coverage.
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

// Delta 2, two slots with different stable selections: both slots
// emit per guard fire now (4 — the init-phantom guard bottoms the
// init cycle) — under organic firing
// per-slot independence is trivially exact because there is no
// selection memory to alias.
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

// Delta 2 across a source resize (1 → 2 mid-run): emissions follow
// deliveries through the regrow on both engines (4 total — the
// init-phantom guard bottoms the init cycle).
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

// Delta 2 two loops deep: per-delivery emission through nested loops
// (4 guard fires; the init-phantom guard bottoms the init cycle),
// no state chain involved.
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

// Delta 2, four slot pairs with different stable selections ((i+j)
// parity): all emit per guard fire (4; init-phantom bottoms init).
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

// Delta 2 with ragged inner lengths + an outer resize mid-run:
// per-delivery emission through the reshape (4 total; init-phantom
// bottoms init).
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

// Delta 2 at loop depth 3 (4 guard fires; init-phantom bottoms init).
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

// Delta 2 in a CALLEE body: a guard-dep fire emits through the
// compiled callee (5 = init + 4) — no per-call-site selection words.
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

// Delta 2, one compiled callee at two call sites with different
// stable selections: both sites emit per guard fire (44 = 4*10 + 4;
// init-phantom bottoms init).
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

// Delta 2, a callee called inside a loop (two slots, different stable
// selections): per-delivery emission (4; init-phantom bottoms init).
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

// Delta 2, a callee whose own body has a loop-select, called at root:
// per-delivery emission (4; init-phantom bottoms init).
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

// Delta 2, the deep composition — a callee with an internal
// loop-select, called from inside a loop: per-delivery emission (4;
// init-phantom bottoms init).
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

// Delta 2 inside a TAIL-RECURSIVE callee: the interior select's guard
// fires per delivery and the emission rides out through the loop
// (4; init-phantom bottoms init) — no site-block selection words.
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

// Arm-local `<-` target PERSISTS across the arm's sleep (2026-08-14,
// supersedes the jul08g re-seed this fixture used to pin): a wake
// RESUMES an arm, it does not create one, so the seed is a birth value
// and the connect-written 11 survives the take where the arm slept.
// Sleep is PAUSE (Eric 2026-07-31); `findings/arm-local-bind-aug2026/`
// carries the three faces of the seam. Both engines changed — the
// node-walk stopped re-executing the arm's binds under the wake view,
// and the kernel's lifted seed-select stopped preferring the seed when
// the init override is active (fusion/emit/flow.rs).
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

// Same shape, connect RHS computed by a fold: the write lands while
// the arm is asleep, and the re-entry now SEES it (6) instead of the
// seed — the persistence rule above.
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

// A GUARD reading a CAPTURE inside a rec callee's TAIL select.
// Delta 2 on the tail spine, capture-driven guard: every m fire
// emits (the old sequence had a quiet same-selection cycle; organic
// emits it; the init-phantom guard bottoms the init cycle
// (activation_state.md), so the sequence is [1, 1, 2] with the
// final 2 in group's open bucket). The jul17c capture-flip fire
// flows through the prologue guard fold instead of final-selection
// memory.
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

// A guard flip wakes the catch-all arm whose fold callback reads ONLY
// the captured scrutinee bind (no slot param). The wake binds v3
// STALE (honest tags) and the becoming-selected fire emits the arm's
// cached value — which the CallSite's frame-depth-0 STALE filter
// starved by eating the capture-only slot production (interp emitted
// 42 forever; the kernel correctly fired 1). The fd0 filter now
// exempts init views (jul18d ryouko divergence; pinned in
// arm-wake-body-fire-jul2026/03).
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

// A pattern's inferred type predicate must not refuse a value because
// an EARLIER union member already walked it.
//
// `Type::is_a_int`'s Ref arm kept a visited set that never popped, and a
// repeat answered "no match". `Type::Set` is a union tried with `any`,
// so one member descending into a child and failing is ordinary
// backtracking — but its leftover entries then answered "no" for every
// later member over that same child. Here the tuple's two `T`s are the
// same recursive name, so checking the first pattern poisons the second:
// NO arm matched, the select produced nothing, and everything downstream
// went bottom. Since the program's only exit is gated on that value, it
// sat idle at zero CPU forever — which is why bench/symbolic.gx read as
// `timeout` in the results table rather than as a wedge.
//
// Latent until e86d18c1 made an inferred predicate load-bearing at
// runtime. A regression here WEDGES rather than fails, so the harness
// timeout is what turns it back into a test failure.
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
// ADT, where the second's select must re-check nodes the first walked.
// Deeper than the minimal case above and correspondingly slower to
// wedge, but it is the actual bench program's core and worth pinning.
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

// ── partial struct patterns infer from the scrutinee (2026-08-18) ──
// `{x, ..}` used to infer an exact one-field struct that could never
// match (and, worse, computed field indexes into the wrong layout).
// The select typecheck now completes the inferred predicate from the
// scrutinee member and realigns the compiled binder's indexes.

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

// The realign regression: `y` names the SECOND field of the member, so
// an un-realigned binder reads slot 0 (`x = 1`) instead of "z".
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

// One binder holds one index layout, so a partial matching several
// union members must be annotated — refused with a teaching error.
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

// ── an explicit predicate on a Rust-backed abstract type ────────
// It is a NOMINAL tag test: the value answers by the path-derived
// wrapper UUID its package registered. Refused outright until the io
// migration made that registration the rule (2026-08-23) — before it
// the arm was a guaranteed-dead arm the wildcard silently won, which
// is what the netidx-admin dogfood campaign hit on 2026-08-18.
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

// ── a union-typed arm plus a never() arm returns the declared union
// (2026-08-19) ── the produced union's member that is (or contains)
// the signature's own tvar cell must be covered reflexively by the
// declared set; the bare-tvar residue arm instead captured it and the
// occurs check refused `'r := ['r, ...]`. Found by the netidx-admin
// package's `result` ceremony accessor.
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

// The bare-cell face of the same bug: the produced union carries the
// signature's `'r` cell itself as a member (plus never()'s fresh
// tvar), and the coverage walk must recognize its own cell rather
// than binding through it.
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

// A select's type is the UNION of its arm types, and a free type
// variable in one arm stays free — nothing infers `str::parse`'s result
// from a sibling `i64` arm. Both spellings must agree: with a literal
// `i64` arm this was always rejected, while a binding whose `i64`
// arrived through a bound tvar (`array::iter`'s instantiation) typed by
// accident — the instance check compared the union against a copy of
// itself and the free member absorbed its sibling's `i64` — and the
// per-slot callback instance then bottomed at runtime (aug22c class E).
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

// The annotation is what types the union: `i64 ⊇ ['b, i64]` binds `'b`.
const UNION_ARM_ANNOTATED: &str = r#"{let y = i64:0; let v: i64 = select i64:1 {i64:1 => str::parse("42")?, _ => y}; v}"#;
run!(union_arm_annotated, UNION_ARM_ANNOTATED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(42))
));
