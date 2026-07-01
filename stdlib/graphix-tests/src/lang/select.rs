// Tests for select/match expressions

use anyhow::Result;
use graphix_package_core::run;
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
  type T = { foo: string, bar: i64, baz: f64 };
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  select x {
    T as { foo: "foo", bar: 8, baz } => baz,
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

// ASPIRE: Jit (currently None) — blocked on: select pattern analysis
run!(match_exhaust1, MATCH_EXHAUST1, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const NESTEDMATCH0: &str = r#"
{
  type T = { foo: (string, i64, f64), bar: i64, baz: f64 };
  let x = { foo: ("bar", 42, 5.0), bar: 42, baz: 84.0 };
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
  type T = { foo: {x: string, y: i64, z: f64}, bar: i64, baz: f64 };
  let x = { foo: { x: "bar", y: 42, z: 5.0 }, bar: 42, baz: 84.0 };
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

// ASPIRE: Jit (currently None) — blocked on: nested composite / variant payload composite
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
// leaf. NOTE the SELECT itself still de-fuses (typecheck's pattern
// inference leaves the nested leaf TVars loose under a STRUCT parent —
// see `native_select_nested_struct_defuses`); the Jit expectation here is
// satisfied by the sibling struct-literal region, and the value agreement
// exercises the node-walk binder.
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
