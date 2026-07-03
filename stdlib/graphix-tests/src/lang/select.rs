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
  let total = #[native] array::fold(w, 0.0, |a, x| a + x);
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

// ASPIRE: Jit (currently None) — the residue of the gate idiom: an
// UNANNOTATED scalar gate. Arith's contains(Number, TVar) binds the
// never-TVar to the WIDE Number set, which genuinely denotes multiple
// register classes — no freeze can soundly pick one, and the
// contaminated type propagates into every downstream arith result, so
// nothing in this minimal program fuses (silently, at the return
// gate). Annotating the let (`let m: i64 = ...`) fixes it. Pinned so
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

// Selection memory (design/kernel_instance_state.md): the node-walk
// emits on a guard-only event only when the SELECTION changes — m
// alternates 1,0,1,0, so the guard re-selects arms on cycles 2..4 (3
// emissions) but cycle 1 re-takes the init selection (no emission):
// 4 total with the init. The kernel's per-instance state word records
// the taken arm (idx+1) and the guard term fires only on a change;
// without it the guard-feeder fold over-fired the unchanged cycle
// (interp 4, jit 5 — findings/firing-jul2026/01).
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
