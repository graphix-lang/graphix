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

// Homogeneous arithmetic (Eric's ruling, 2026-07-12): two DISTINCT
// quantified operand types can't add — `fn('a: Number, 'a) -> 'a`.
run!(explicit_type_vars3, EXPLICIT_TYPE_VARS3, |v: Result<&Value>| matches!(v, Err(_));
     graphix_package_core::testing::FuseExpect::None);

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

// Homogeneous arithmetic: `|a, b| a + b` aliases both formals into ONE
// cell (fn('a, 'a) -> 'a), so the i64×f64 call REJECTS — the runtime
// promotion this used to exercise is unreachable from well-typed code.
run!(mixed_operand_accept, MIXED_OPERAND_ACCEPT, |v: Result<&Value>| matches!(v, Err(_));
     graphix_package_core::testing::FuseExpect::None);

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

// ...and so is an f64-only use: under homogeneous arithmetic
// (fn('a: Number, 'a) -> 'a, Eric's ruling 2026-07-12) the DEF itself
// is ill-typed — `x + f64:0.` cannot be well-typed for ARBITRARY
// rigid 'a. Declare `|x: f64|` instead. This deliberately reverses
// the promotion-obligation semantics ruled in earlier the same week.
const PROMO_OBLIGATION_F64_OK: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + f64:0.;
  f(f64:2.5)
}
"#;

run!(promo_obligation_f64_ok, PROMO_OBLIGATION_F64_OK, |v: Result<&Value>| matches!(
    v,
    Err(_)
); graphix_package_core::testing::FuseExpect::None);

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

// The RESIDUE rule for set-vs-set unification: a set member that is a
// bare unbound tvar binds to the UNION of the rhs members no concrete
// lhs member covers, in one act. The old per-member walk let the tvar
// greedily capture the FIRST uncovered member and reject the second —
// `[null, 'a] ⊇ [`A, `B]` bound 'a := `A, so a polymorphic
// optional-selection formal (gui radio's `#selected: &['a, null]`
// against a multi-variant union) could never typecheck at ANY site.
const SET_RESIDUE_TVAR_BIND: &str = r#"
{
  let f = |sel: [null, 'a], v: 'a| true;
  let s: [`A, `B] = `A;
  (f(s, `A), f(s, `B))
}
"#;

run!(set_residue_tvar_bind, SET_RESIDUE_TVAR_BIND, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => matches!(&t[..], [Value::Bool(true), Value::Bool(true)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A connect whose RHS is `trigger ~ select {...}` must still check the
// RHS against the target's type. `Sample::compile` snapshotted the
// select's PRE-typecheck typ (the empty primitive set, which every
// type contains — Select::typecheck0 REPLACES its typ field, orphaning
// the snapshot: the finding-37 class), so a struct of the wrong shape
// connected into a differently-shaped binding and flowed at runtime —
// the JIT then read fields by the declared offsets (i64 42) while the
// node-walk's Value arith promoted (f64 42.0), soak-jul14b 000005.
const CONNECT_SAMPLE_SELECT_SHAPE_ERR: &str = r#"
{
  let trig = 1;
  let st = { last: 0, n: 0 };
  st <- trig ~ select 100 {
    42 => { b: 1.0, y: 2 },
    _ => { b: 0.0, y: 42 }
  };
  st.n
}
"#;

run!(
    connect_sample_select_shape_err,
    CONNECT_SAMPLE_SELECT_SHAPE_ERR,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// An UNANNOTATED `str::parse` target must reject wherever it appears.
// Direct and lambda-wrapped sites always did (the builtin's typecheck1
// bails when no concrete target extracts); inside a COLLECTION
// callback, cell aliasing left the target as the whole
// `[⊥, Error<ParseError>]` union — extract_cast_type's ⊥-guard only
// checked the top level, so the bail never fired, the runtime lazy
// binds swallowed the validation, and the two modes then diverged on
// the leftover view (interp: the catchable TypeError as a map key;
// jit: a cast-transparent naked parse — soak-jul14b 000003). The
// guard now rejects ⊥ MEMBERS too (⊥ has no surface syntax, so a ⊥
// member is always a settle artifact).
const PARSE_UNANNOTATED_IN_CALLBACK_ERR: &str = r#"
{
  let m = {"a" => 1, "b" => 2};
  map::map(m, |(k, v)| (str::parse("42"), v * 2))
}
"#;

run!(
    parse_unannotated_in_callback_err,
    PARSE_UNANNOTATED_IN_CALLBACK_ERR,
    |v: Result<&Value>| { matches!(v, Err(_)) };
    graphix_package_core::testing::FuseExpect::None
);

// A let-bound BARE cast (no `$`/`?`). The cast node's static type is
// the fallible `[T, Error]` union, so `emit_let_node` classifies it as
// a 2-word Value and declares an I64 payload var — but the scalar
// fast path in `emit_cast_node` used to return a raw F64-typed
// payload, and the def_var type mismatch panicked cranelift's
// frontend, killing the runtime thread (jul17c katana divergence
// 000000, "runtime did not respond"). `$`/`?` consumers only worked
// by accident (same-width bitcast is an identity). The fast path now
// widens to the Value wire shape.
const CAST_LET_WIRE_SHAPE: &str = r#"
{
  let v = cast<f64>(i64:-9223372036854775808);
  v;
  42
}
"#;

run!(cast_let_wire_shape, CAST_LET_WIRE_SHAPE, |v: Result<&Value>| match v {
    Ok(&Value::I64(42)) => true,
    _ => false,
});

// The f32 twin was a second latent frontend panic (declared F32 var
// def'd with the wire word), plus the `$` consumer over the widened
// shape.
const CAST_LET_WIRE_SHAPE_F32: &str = r#"
{
  let v = cast<f32>(i64:3);
  v;
  cast<f32>(f64:1.5)$
}
"#;

run!(cast_let_wire_shape_f32, CAST_LET_WIRE_SHAPE_F32, |v: Result<&Value>| match v {
    Ok(&Value::F32(f)) => f == 1.5,
    _ => false,
});

// Narrow float→int casts saturate AT THE TARGET WIDTH (Rust `as`
// semantics — the node-walk's `Value::cast`). The x64 backend has no
// encoding for an i8/i16 fcvt (cranelift emit unreachable — the
// jit_generated_sweep `cast<u8>(f64)$` crash), so `compile_cast`
// converts at i32, clamps to the target range (i32-width saturation
// alone wraps 300 → u8:44), and reduces. Narrow int→float widens
// before the fcvt for the same encoding reason.
const CAST_NARROW_SATURATES: &str = r#"
(
  cast<u8>(f64:300.5)$,
  cast<i8>(f64:-300.5)$,
  cast<i16>(f64:70000.0)$,
  cast<f64>(i8:-5)$
)
"#;

run!(cast_narrow_saturates, CAST_NARROW_SATURATES, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        &**a == &[Value::U8(255), Value::I8(-128), Value::I16(32767), Value::F64(-5.0)]
    }
    _ => false,
});

// A rec def whose base arm returns a PARAM: the return relates to the
// acc only through tvar cell aliasing, and `TVar::alias` used to
// redirect the aliasing STRUCT's cell Arc without forward-linking the
// abandoned allocation — any other struct sharing it orphaned, later
// facts forked between the allocations, and the orphan
// terminal-settled ⊥. The elem union then flattened `[i64, ⊥]` → i64
// and the kernel compared the non-i64 element's payload bits as i64
// (jul17c katana divergence 000001). With the forward link the elem
// types honestly (`[i64, string]` here), which refuses the scalar
// loop — the HOF interprets and the total order answers ("hi" > 3 is
// true: strings sort above ints).
const REC_RETURN_PARAM_ELEM: &str = r#"
{
  let a = [0, {let rec f = |n, acc| select n {0 => acc, _ => f(n - 1, acc)}; f(3, "hi")}, 4];
  array::map(a, |x| x > 3)
}
"#;

run!(
    rec_return_param_elem,
    REC_RETURN_PARAM_ELEM,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            &**a == &[Value::Bool(false), Value::Bool(true), Value::Bool(true)]
        }
        _ => false,
    };
    graphix_package_core::testing::FuseExpect::None
);

// The Fn-element twin (the original finding's shape): the element
// union is honestly `[i64, fn(...)]`, freeze refuses it, and both
// modes agree the Fn sorts above 3 in the total order.
const REC_RETURN_FN_ELEM: &str = r#"
{
  let a = [0, 0, {let rec f = |n, acc| select n {0 => acc, _ => f(n - 1, acc)}; f(3, buffer::to_string)}, 4, 0];
  array::map(a, |x| x > 3)
}
"#;

run!(
    rec_return_fn_elem,
    REC_RETURN_FN_ELEM,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => {
            &**a
                == &[
                    Value::Bool(false),
                    Value::Bool(false),
                    Value::Bool(true),
                    Value::Bool(true),
                    Value::Bool(false),
                ]
        }
        _ => false,
    };
    graphix_package_core::testing::FuseExpect::None
);
