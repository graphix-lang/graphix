// Tests for type system features: type checking, annotations, type variables

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

const SIMPLE_TYPECHECK: &str = r#"
{
  "foo" + 1
}
"#;

run!(simple_typecheck, SIMPLE_TYPECHECK, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const FUNCTION_TYPES: &str = r#"
{
  let f = |x: Number, y: Number| -> string "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(function_types, FUNCTION_TYPES, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const PARTIAL_FUNCTION_TYPES: &str = r#"
{
  let f = |x: Number, y| "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(partial_function_types, PARTIAL_FUNCTION_TYPES, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const FUNCTION_RTYPE: &str = r#"
{
  let f = |x, y| -> Number "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(function_rtype, FUNCTION_RTYPE, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

const LAMBDA_CONSTRAINT: &str = r#"
{
  let f = |f: fn(s1: string, s2: string) -> string, a| f("foo", a);
  f(|x, y: Number| "[x] and [y]", "foo")
}
"#;

run!(lambda_constraint, LAMBDA_CONSTRAINT, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const EXPLICIT_TYPE_VARS0: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  f("foo", "bar")
}
"#;

run!(explicit_type_vars0, EXPLICIT_TYPE_VARS0, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const EXPLICIT_TYPE_VARS1: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  f(u32:1, i64:2)
}
"#;

run!(explicit_type_vars1, EXPLICIT_TYPE_VARS1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const EXPLICIT_TYPE_VARS2: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  select f(1, 1) {
    i64 as t => t
  }
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(explicit_type_vars2, EXPLICIT_TYPE_VARS2, |v: Result<&Value>| match v {
    Ok(Value::I64(2)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const EXPLICIT_TYPE_VARS3: &str = r#"
{
  let f = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ['a, 'b] x + y;
  select f(u32:1, u64:1) {
    [u32, u64] as t => t
  }
}
"#;

// ASPIRE: Jit (currently None) — blocked on: type variable instantiation in fusion
run!(explicit_type_vars3, EXPLICIT_TYPE_VARS3, |v: Result<&Value>| match v {
    Ok(Value::U32(2) | Value::U64(2)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const TYPED_ARRAYS0: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [1, 2, 3])
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
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
}; graphix_package_core::testing::FuseExpect::Jit);

const TYPED_ARRAYS1: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [u32:1, 2, 3])
}
"#;

run!(typed_arrays1, TYPED_ARRAYS1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

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
}; graphix_package_core::testing::FuseExpect::None);

const TYPEDEF_TVAR_OK: &str = r#"
{
  type T<'a, 'b> = { foo: 'a, bar: 'b, f: fn(a: 'a, b: 'b, c: 'c) -> 'a };
  0
}
"#;

run!(typedef_tvar_ok, TYPEDEF_TVAR_OK, |v: Result<&Value>| match v {
    Ok(Value::I64(0)) => true,
    _ => false,
});

// A legitimate multi-hop type-alias chain must resolve — deref_typ!'s
// old cycle detection keyed on the STACK ADDRESS of a loop local, so
// chains of 3+ hops were spuriously (and build-dependently) rejected
// as cyclic.
const DEEP_ALIAS_CHAIN: &str = r#"
{
  type E = (i64, i64);
  type D = E;
  type C = D;
  type B = C;
  type A = B;
  let t: A = (1, 2);
  t.0
}
"#;

run!(deep_alias_chain, DEEP_ALIAS_CHAIN, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
));

// A genuinely cyclic typedef must be a compile error (bounded deref),
// not a hang.
const CYCLIC_ALIAS: &str = r#"
{
  type A = B;
  type B = A;
  let t: A = (1, 2);
  t.0
}
"#;

run!(cyclic_alias, CYCLIC_ALIAS, |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None);

// ─── #20 tvar cell constraints (design/tvar_constraints.md) ────────
//
// `|a| a + a` is type-preserving: the result ALIASES the operand cell,
// so an annotation at one call site narrows the whole signature
// instance and the args enforce it (observation #3).
const SAME_CELL_ANNOTATED: &str = r#"
{
  let f = |a| a + a;
  let x: f64 = f(f64:1.5);
  x
}
"#;

run!(same_cell_annotated, SAME_CELL_ANNOTATED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::F64(f)) if *f == 3.0
));

// An annotation the operand cell can't satisfy rejects at the ARG (the
// shared cell is already f64 when the i64 arg tries to bind).
const SAME_CELL_ANNOTATION_CONFLICT: &str = r#"
{
  let f = |a| a + a;
  let x: f64 = f(1);
  x
}
"#;

run!(
    same_cell_annotation_conflict,
    SAME_CELL_ANNOTATION_CONFLICT,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// Mixed-operand acceptance pin (decision (ii)-adjacent): `|a, b| a + b`
// keeps distinct operand cells, so mixed-type calls stay accepted —
// the result settles wide, exactly today's semantics.
const MIXED_OPERAND_ACCEPT: &str = r#"
{
  let f = |a, b| a + b;
  f(1, 2.5)
}
"#;

// Correct-None: the mixed call settles the derived result cell to the
// wide Number set — no single register class, the region node-walks.
run!(mixed_operand_accept, MIXED_OPERAND_ACCEPT, |v: Result<&Value>| matches!(
    v,
    Ok(Value::F64(f)) if *f == 3.5
); graphix_package_core::testing::FuseExpect::None);

// ...and the derived (rtype-only) result cell of a distinct-operand
// lambda is NOT externally narrowable — the eager derived-cell settle
// closes it before an annotation could lie about what the body
// computes (the soundness half of the settle split).
const DERIVED_RESULT_NOT_NARROWABLE: &str = r#"
{
  let f = |a, b| a + b;
  let x: f64 = f(1, 2);
  x
}
"#;

run!(
    derived_result_not_narrowable,
    DERIVED_RESULT_NOT_NARROWABLE,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// Observation #4, SUPERSEDED by the rigid-tvar ruling (2026-07-09,
// soak jul09c rigid_tvar_body_escape): a declared `'a` is a CONTRACT —
// the def-time body check runs with 'a rigid, so a concrete f64 body
// under `-> 'a` is a DEF-time compile error, at every call type. The
// previous semantics (accept the def, ride the f64 fact as a cell
// conjunct, reject only non-f64 calls) monomorphized the lie instead
// of rejecting it; constrain_known's conjunct machinery remains for
// INFERRED (anonymous-cell) facts.
const OBS4_DEF_FACT_REJECTS: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a f64:0.5;
  f(3)
}
"#;

run!(obs4_def_fact_rejects, OBS4_DEF_FACT_REJECTS, |v: Result<&Value>| matches!(
    v,
    Err(_)
); graphix_package_core::testing::FuseExpect::None);

// The same def rejects even at an f64 call — the def itself is the
// error now, not the call.
const OBS4_DEF_FACT_ACCEPTS: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a f64:0.5;
  f(1.25)
}
"#;

run!(obs4_def_fact_accepts, OBS4_DEF_FACT_ACCEPTS, |v: Result<&Value>| matches!(
    v,
    Err(_)
); graphix_package_core::testing::FuseExpect::None);


// =============================================================================
// Promotion obligations (Eric's ruling (a), 2026-07-12)
// =============================================================================
//
// A generic arith operand paired with a CONCRETE numeric records the
// runtime promotion's ABSORBER set as a cell conjunct
// (`typ::numeric_absorbers`, mirroring netidx `apply_op!`): the site's
// instantiation must be a type the promotion keeps. `x + f64:0.` is
// F64 at runtime whatever x is, so an i64 instantiation would make
// the static type a lie the JIT freezes (the promo-lie class,
// jul10h 000001 / jul12a 000002).

// The i64 site violates the F64 obligation — rejected.
const PROMO_OBLIGATION_REJECTS: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + f64:0.;
  (f(i64:3), f(f64:2.5))
}
"#;

run!(promo_obligation_rejects, PROMO_OBLIGATION_REJECTS, |v: Result<&Value>| matches!(
    v,
    Err(_)
); graphix_package_core::testing::FuseExpect::None);

// An f64-only use of the same def is fine.
const PROMO_OBLIGATION_F64_OK: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + f64:0.;
  f(f64:2.5)
}
"#;

run!(promo_obligation_f64_ok, PROMO_OBLIGATION_F64_OK, |v: Result<&Value>| matches!(
    v,
    Ok(Value::F64(x)) if *x == 2.5
); graphix_package_core::testing::FuseExpect::Jit);

// An UNANNOTATED formal infers MONOMORPHIC — the operand pre-bind
// makes `|x| x + i64:1` a fn(i64) -> i64, so the f64 site rejects.
// This is the deliberate line under ruling (a): promotion-obligation
// polymorphism belongs to ANNOTATED `'a: Number` formals (rigid at
// the gate, so the pre-bind is suppressed and the absorber conjunct
// carries the obligation — see param_knot_no_leak). An impl left
// generic here broke interface / fn-subsumption matching
// (dynamic_module0, first_class_lambdas), which compares signatures
// structurally. Annotate to widen.
const PROMO_OBLIGATION_UNANNOTATED_MONO: &str = r#"
{
  let f = |x| x + i64:1;
  (f(i64:3), f(f64:2.5))
}
"#;

run!(promo_obligation_unannotated_mono, PROMO_OBLIGATION_UNANNOTATED_MONO, |v: Result<
    &Value,
>| matches!(
    v,
    Err(_)
); graphix_package_core::testing::FuseExpect::None);
