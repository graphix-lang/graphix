// Tests for type system features: type checking, annotations, type variables

use anyhow::Result;
use graphix_package_core::{run, testing::eval};
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

// ASPIRE: Jit — the body does not fuse into a kernel yet.
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

// Two distinct quantified operand types cannot add.
run!(explicit_type_vars3, EXPLICIT_TYPE_VARS3, |v: Result<&Value>| matches!(v, Err(_));
     graphix_package_core::testing::FuseExpect::None);

const TYPED_ARRAYS0: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [1, 2, 3])
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
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
  type L = [
    `Cons(Any, L),
    `Nil
  ];
  let l: L = `Cons(42, `Cons(3, `Nil));
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
  type L<'a> = [
    `Cons('a, L<'a>),
    `Nil
  ];
  let l: L<Any> = `Cons(42, `Cons(3, `Nil));
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

// A multi-hop type-alias chain resolves.
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

// A cyclic typedef is a compile error, not a hang.
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

// `|a| a + a` is type-preserving: the result aliases the operand cell,
// so an annotation at one call site narrows the signature instance.
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

// An annotation the operand cell cannot satisfy rejects at the arg.
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

const MIXED_OPERAND_ACCEPT: &str = r#"
{
  let f = |a, b| a + b;
  f(1, 2.5)
}
"#;

// `|a, b| a + b` aliases both formals into one cell, so an i64 x f64
// call rejects.
run!(mixed_operand_accept, MIXED_OPERAND_ACCEPT, |v: Result<&Value>| matches!(v, Err(_));
     graphix_package_core::testing::FuseExpect::None);

// The derived result cell of a distinct-operand lambda is not
// externally narrowable.
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

// A declared `'a` is a contract: a concrete f64 body under `-> 'a` is a
// def-time error.
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

// The same def rejects even at an f64 call.
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

// `x + f64:0.` under a generic `'a: Number` formal is ill-typed.
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

// The same def rejects at an f64-only use: the def itself is ill-typed.
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

// An unannotated formal infers monomorphic: `|x| x + i64:1` is
// fn(i64) -> i64, so the f64 site rejects.
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

// Set-vs-set residue: a bare unbound tvar member binds to the union of
// the uncovered rhs members in one act (`[null, 'a] ⊇ [`A, `B]`).
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

// A connect whose RHS is `trigger ~ select {...}` checks the RHS
// against the target's type.
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

// An unannotated `str::parse` target rejects inside a collection
// callback as it does elsewhere.
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

// A let-bound bare cast (no `$`/`?`) has the fallible union type and
// marshals as a Value.
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

// The f32 twin, plus a `$` consumer over the widened shape.
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

// Narrow float->int casts saturate at the target width (300 -> u8:255).
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

// A rec def whose base arm returns a param: the elem union is honestly
// `[i64, string]`, and the total order answers ("hi" > 3).
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

// The Fn-element twin: the union is `[i64, fn(...)]` and the Fn sorts
// above 3.
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

// A rec fn that returns itself has an infinite type: "cannot infer a
// finite type".
const REC_RETURN_SELF_REJECTS: &str = r#"
{
  let a = [0, 0, {let rec f = |n, acc| f; f(0, buffer::to_string)}, 4, 0];
  array::map(a, |x| x > 3)
}
"#;

run!(
    rec_return_self_rejects,
    REC_RETURN_SELF_REJECTS,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// The μ-collapse (`'r ⊇ [T, 'r]` binds `'r := T`) looks through a
// binding cell, so a block-wrapped body types like the bare one.
const REC_BLOCK_BODY_COLLAPSES: &str = r#"
{
  let rec f = |x: i64| { let t = select x { 0 => 0, _ => f(x - 1) }; t };
  f(3)
}
"#;

run!(
    rec_block_body_collapses,
    REC_BLOCK_BODY_COLLAPSES,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(0)));
    graphix_package_core::testing::FuseExpect::Jit
);

const REC_NESTED_BLOCK_COLLAPSES: &str = r#"
{
  let rec f = |x: i64| {
    let t = { let u = select x { 0 => 0, _ => f(x - 1) }; u };
    t
  };
  f(3)
}
"#;

run!(
    rec_nested_block_collapses,
    REC_NESTED_BLOCK_COLLAPSES,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(0)));
    graphix_package_core::testing::FuseExpect::Jit
);

const REC_BLOCK_MULTI_MEMBER_COLLAPSES: &str = r#"
{
  let rec f = |x: i64| { let t = select x { 0 => 0, 1 => "a", _ => f(x - 1) }; t };
  f(3)
}
"#;

// ASPIRE: Jit — union/string cross-kernel return.
run!(
    rec_block_multi_member_collapses,
    REC_BLOCK_MULTI_MEMBER_COLLAPSES,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a");
    graphix_package_core::testing::FuseExpect::None
);

// A genuine infinite type through the cell is still refused.
const REC_BLOCK_INFINITE_REJECTS: &str = r#"
{
  let rec f = |x: i64| { let t = select x { 0 => [0], _ => [f(x - 1)] }; t };
  f(3)
}
"#;

run!(
    rec_block_infinite_rejects,
    REC_BLOCK_INFINITE_REJECTS,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

const REC_RETURN_SELF_BLOCK_REJECTS: &str = r#"
{
  let a = [0, 0, {let rec f = |n, acc| { let t = f; t }; f(0, buffer::to_string)}, 4, 0];
  array::map(a, |x| x > 3)
}
"#;

run!(
    rec_return_self_block_rejects,
    REC_RETURN_SELF_BLOCK_REJECTS,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// The self-returning fn is refused at its definition.
const REC_RETURN_SELF_STATEMENT_REJECTS: &str = r#"
{
  let rec f = |n, acc| f;
  f(0, 1)
}
"#;

run!(
    rec_return_self_statement_rejects,
    REC_RETURN_SELF_STATEMENT_REJECTS,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// Two bound cells, one reachable from the other's binding, are decided
// by walking the bindings: the inferred nesting is refused like the
// annotated twin.
const CONNECT_SELF_NESTING_REJECTED: &str = r#"
{
  let src = [[i64:10]];
  src <- [i64:2, src];
  array::map(src, |ys| array::map(ys, |y| y))
}
"#;

run!(
    connect_self_nesting_rejected,
    CONNECT_SELF_NESTING_REJECTED,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

const CONNECT_SELF_NESTING_ANNOTATED_REJECTED: &str = r#"
{
  let src: Array<Array<i64>> = [[i64:10]];
  src <- [i64:2, src];
  src
}
"#;

run!(
    connect_self_nesting_annotated_rejected,
    CONNECT_SELF_NESTING_ANNOTATED_REJECTED,
    |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None
);

// The well-typed resize (`array::push`) types and both engines count
// every fire of the nested map.
const CONNECT_PUSH_NESTING_TYPES: &str = r#"
{
  let x = array::iter([i64:1, i64:2, i64:3, i64:-1]);
  let m = x / i64:3;
  let src = [[i64:10]];
  src <- select count(x % i64:2) { i64:2 => array::push(src, [i64:2]), _ => never() };
  let a = array::map(src, |ys| array::map(ys, |y| select i64:0 { i64:0 if m == i64:0 => i64:42, _ => i64:2 }));
  let c = count(a);
  select count(x) { i64:4 => c, _ => never() }
}
"#;

run!(
    connect_push_nesting_types,
    CONNECT_PUSH_NESTING_TYPES,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(4)));
    graphix_package_core::testing::FuseExpect::Jit
);

// Unifying an inferred `List<'a>` against a value whose deep tail is a
// Fn rejects at every depth.
#[tokio::test]
async fn recursive_fn_tail_rejected_at_every_depth() {
    for src in [
        "{let l = `Cons(i64:0, once); list::find(l, |x| true)}",
        "{let l = `Cons(i64:0, `Cons(i64:3, once)); list::find(l, |x| true)}",
        "{let l = `Cons(i64:0, `Cons(i64:3, `Cons(i64:5, once))); list::find(l, |x| true)}",
    ] {
        let r = eval(src, crate::TEST_REGISTER).await;
        assert!(
            r.is_err(),
            "a Fn in a recursive List tail must be rejected: {src} => {:?}",
            r.map(|(v, _)| v)
        );
    }
}

// find over a well-formed API-built list.
const RECURSIVE_LIST_FIND: &str = r#"
{
  let l = list::cons(i64:1, list::cons(i64:2, list::cons(i64:3, list::nil(0))));
  select list::find(l, |x| x == i64:2) {
    i64 as n => n,
    _ => i64:-1
  }
}
"#;

run!(recursive_list_find, RECURSIVE_LIST_FIND, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(2))
));

// Non-scalar variant payload binds fuse (composite, string, value).
const VARIANT_COMPOSITE_PAYLOAD: &str = r#"
{
    type T = [`A(Array<i64>), `B];
    let v: T = `A([10, 20, 30]);
    select v { `A(xs) => xs[1]$ + array::len(xs), `B => 0 }
}"#;
run!(variant_composite_payload, VARIANT_COMPOSITE_PAYLOAD, |v: Result<&Value>| matches!(v, Ok(Value::I64(23))); graphix_package_core::testing::FuseExpect::Jit);

const VARIANT_STRING_PAYLOAD: &str = r#"
{
    type S = [`S(string), `N];
    let v: S = `S("hello");
    select v { `S(s) => "got [s]", `N => "none" }
}"#;
run!(variant_string_payload, VARIANT_STRING_PAYLOAD, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s.as_str() == "got hello"); graphix_package_core::testing::FuseExpect::Jit);

const VARIANT_RECURSIVE_PAYLOAD: &str = r#"
{
    type L<'a> = [`C('a, L<'a>), `N];
    let l: L<i64> = `C(1, `C(2, `N));
    select l {
        `N => 0,
        `C(x, rest) => x + select rest { `N => 10, `C(y, _) => y * 100 }
    }
}"#;
run!(variant_recursive_payload, VARIANT_RECURSIVE_PAYLOAD, |v: Result<&Value>| matches!(v, Ok(Value::I64(201))); graphix_package_core::testing::FuseExpect::Jit);

const VARIANT_MAP_PAYLOAD: &str = r#"
{
    type M = [`M(Map<string, i64>), `N];
    let v: M = `M({"a" => 1, "b" => 2});
    select v { `M(m) => map::len(m), `N => 0 }
}"#;
run!(variant_map_payload, VARIANT_MAP_PAYLOAD, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); graphix_package_core::testing::FuseExpect::Jit);

// A recursive union that consumes nothing names no value: refused at
// its definition, not a stack overflow at the cast.
const CAST_RECURSIVE_NO_PROGRESS: &str = r#"
{
  type Loop = [i64, Loop];
  cast<Loop>("not-a-number")
}
"#;
run!(cast_recursive_no_progress, CAST_RECURSIVE_NO_PROGRESS, |v: Result<&Value>| {
    matches!(v, Err(_))
}; graphix_package_core::testing::FuseExpect::None);

// A two-element array is not a list: converting it keeps both items.
const CAST_PAIR_TO_LIST: &str = r#"cast<List<i64>>([1, 2])$"#;
run!(cast_pair_to_list, CAST_PAIR_TO_LIST, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:1, [i64:2, []]]"
}; graphix_package_core::testing::FuseExpect::Jit);

// A struct cast converts each field; the names decide the match.
const CAST_STRUCT_FIELDS: &str = r#"cast<{x: i64, y: i64}>({y: "2", x: "1"})$"#;
run!(cast_struct_fields, CAST_STRUCT_FIELDS, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == r#"[["x", i64:1], ["y", i64:2]]"#
}; graphix_package_core::testing::FuseExpect::Jit);

// An array whose last element is list-shaped is still an array: the
// cast's source type, not the value's shape, picks the conversion.
const CAST_ARRAY_TO_LIST: &str = r#"{
  let a: Array<Array<i64>> = [[1], []];
  cast<List<Array<i64>>>(a)$
}"#;
run!(cast_array_to_list, CAST_ARRAY_TO_LIST, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[i64:1], [[], []]]"
}; graphix_package_core::testing::FuseExpect::Jit);

const CAST_NESTED_ARRAY_TO_LIST: &str = r#"{
  let b: Array<Array<Array<i64>>> = [[[1]], [[2], []]];
  cast<List<Array<Array<i64>>>>(b)$
}"#;
run!(cast_nested_array_to_list, CAST_NESTED_ARRAY_TO_LIST, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[[[i64:1]], [[[i64:2], []], []]]"
}; graphix_package_core::testing::FuseExpect::Jit);

// A list statically so converts to an array element by element.
const CAST_LIST_TO_ARRAY: &str = r#"{
  let l = [<1, 2>];
  cast<Array<i64>>(l)$
}"#;
run!(cast_list_to_array, CAST_LIST_TO_ARRAY, |v: Result<&Value>| {
    format!("{}", v.unwrap()) == "[i64:1, i64:2]"
}; graphix_package_core::testing::FuseExpect::Jit);

// Each refused program was accepted and went wrong at run time (a
// value the type excludes, a function called on the wrong type, an
// exhaustive select with no arm to take); each accepted twin shows
// what the rule still admits.
const NON_CONTRACTIVE: &str = r#"{
  type T = [i64, T];
  let v: T = "hello";
  0
}"#;
const NON_CONTRACTIVE_MUTUAL: &str = r#"{
  type A = [B, `X(i64)];
  type B = [A, `Y(i64)];
  let x: A = `Y(1);
  0
}"#;
const NON_CONTRACTIVE_ALIAS: &str = r#"{
  type Id<'a> = 'a;
  type T = [i64, Id<T>];
  0
}"#;
const CONTRACTIVE: &str = r#"{
  type T = [i64, `Wrap(T)];
  let v: T = `Wrap(`Wrap(3));
  select v { i64 as n => n, `Wrap(_) => 1 }
}"#;
const PARAM_CONTRAVARIANT: &str = r#"{
  type F<'a> = fn(x: 'a) -> i64;
  let widen = |f: Array<F<i64>>| -> Array<F<Number>> f;
  0
}"#;
const PARAM_COVARIANT: &str = r#"{
  type B<'a> = {v: 'a};
  let widen = |b: Array<B<i64>>| -> Array<B<Number>> b;
  array::len(widen([{v: 1}]))
}"#;
const REF_THROUGH_CELL: &str = r#"{
  type T = [`Cons(i64, T), `Nil];
  let mk = |a| `Cons(1, a);
  let t: T = `Cons(2, mk(mk("oops")));
  0
}"#;
const REF_THROUGH_CELL_OK: &str = r#"{
  type T = [`Cons(i64, T), `Nil];
  let mk = |a| `Cons(1, a);
  let t: T = `Cons(2, mk(mk(`Nil)));
  select t { `Cons(n, _) => n, `Nil => 0 }
}"#;
const RIGID_IN_SET: &str = r#"{
  let f = |x: ['a, i64]| -> ['b, i64] x;
  f(1.5)
}"#;
const RIGID_IN_SET_ARRAY: &str = r#"{
  let f = |x: [Array<'a>, i64]| -> [Array<'b>, i64] x;
  f(1)
}"#;
const RIGID_IN_SET_OK: &str = r#"{
  let f = |x: ['a, i64]| -> ['a, i64] x;
  f(1)
}"#;
const SET_MEMBER_PROBE: &str = r#"{
  let f = |x: [`A('a, i64), `A(Array<i64>, string)], d: 'a| -> 'a d;
  f(`A([1], "s"), 42)
}"#;
const SET_MEMBER_PROBE_BINDS: &str = r#"{
  let f = |x: [`A('a, i64), `A(Array<i64>, string)], d: 'a| -> 'a d;
  f(`A(1, 2), "s")
}"#;
const DEFAULTED_LABEL: &str = r#"{
  let f = |#x: i64| x + 1;
  let a: Array<fn(?#x: i64) -> i64> = [f];
  0
}"#;
const DEFAULTED_LABEL_OK: &str = r#"{
  let g = |#x: i64 = 1| x + 1;
  let a: Array<fn(?#x: i64) -> i64> = [g];
  let b: Array<fn(#x: i64) -> i64> = [g];
  array::len(a) + array::len(b)
}"#;
const PRODUCT_UNION: &str = r#"{
  let x: [(i64, i64), (string, string)] = (1, "a");
  0
}"#;
const PRODUCT_UNION_VARIANT: &str = r#"{
  let x: [`P(i64, i64), `P(string, string)] = `P(1, "a");
  0
}"#;
const PRODUCT_UNION_ONE_DIFFERS: &str = r#"{
  let x: [(i64, i64), (string, i64)] = ("a", 1);
  x.1
}"#;
const REF_UNION_PARAMS: &str = r#"{
  type F<'a> = fn(x: 'a) -> i64;
  let fi: F<i64> = |x: i64| -> i64 x + 1;
  let fs: F<string> = |x: string| -> i64 str::len(x);
  let flag = true;
  let ai: `A(F<i64>) = `A(fi);
  let as: `A(F<string>) = `A(fs);
  let c = select flag { true => ai, false => as };
  select c { `A(f) => f("hello") }
}"#;
const REF_UNION_SAME_PARAMS: &str = r#"{
  type F<'a> = fn(x: 'a) -> i64;
  let fi: F<i64> = |x: i64| -> i64 x + 1;
  let fj: F<i64> = |x: i64| -> i64 x * 2;
  let flag = true;
  let ai: `A(F<i64>) = `A(fi);
  let aj: `A(F<i64>) = `A(fj);
  let c = select flag { true => ai, false => aj };
  select c { `A(f) => f(1) }
}"#;
const DIFF_REF_PARAMS: &str = r#"{
  type Box<'a> = {v: 'a};
  type Both = [Array<Box<i64>>, Array<Box<string>>];
  type BS = Array<Box<string>>;
  let x: Both = [{v: 1}];
  select x { BS as s => 0, Array<Box<i64>> as b => 1 }
}"#;
const NESTED_QUANTIFIER: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  let apply = |f: F| f("hello");
  0
}"#;
const NESTED_QUANTIFIER_OK: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  let apply = |f: F| f(1);
  apply(|x| x + 1)
}"#;
const TRAIT_BESIDE_UNSATISFIABLE: &str = r#"{
  type P<'x: string> = Array<'x>;
  type Q<'x: Eq> = Array<'x>;
  type R<'x: i64> = Array<'x>;
  let g = |a: P<'q>, b: Q<'q>, c: R<'q>| 0;
  g([], [], [])
}"#;
const TRAIT_BESIDE_WITNESS: &str = r#"{
  type Q<'x: Eq> = Array<'x>;
  type R<'x: i64> = Array<'x>;
  let g = |b: Q<'q>, c: R<'q>| 0;
  g([], [])
}"#;

#[tokio::test(flavor = "current_thread")]
async fn unsound_acceptances_are_refused() -> Result<()> {
    for (src, refusal) in [
        (NON_CONTRACTIVE, "refers back to itself"),
        (NON_CONTRACTIVE_MUTUAL, "refers back to itself"),
        (NON_CONTRACTIVE_ALIAS, "refers back to itself"),
        (PARAM_CONTRAVARIANT, "does not contain"),
        (REF_THROUGH_CELL, "does not contain"),
        (RIGID_IN_SET, "does not contain"),
        (RIGID_IN_SET_ARRAY, "does not contain"),
        (SET_MEMBER_PROBE_BINDS, "does not contain"),
        (DEFAULTED_LABEL, "does not contain"),
        (PRODUCT_UNION, "does not contain"),
        (PRODUCT_UNION_VARIANT, "does not contain"),
        (REF_UNION_PARAMS, "expected fn"),
        (NESTED_QUANTIFIER, "does not contain"),
        (TRAIT_BESIDE_UNSATISFIABLE, "unsatisfiable constraints"),
    ] {
        let msg = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(refusal), "wrong refusal for {src}: {msg}");
    }
    for (src, expected) in [
        (CONTRACTIVE, 1),
        (PARAM_COVARIANT, 1),
        (REF_THROUGH_CELL_OK, 2),
        (RIGID_IN_SET_OK, 1),
        (SET_MEMBER_PROBE, 42),
        (DEFAULTED_LABEL_OK, 2),
        (PRODUCT_UNION_ONE_DIFFERS, 1),
        (REF_UNION_SAME_PARAMS, 2),
        (DIFF_REF_PARAMS, 1),
        (NESTED_QUANTIFIER_OK, 2),
        (TRAIT_BESIDE_WITNESS, 0),
    ] {
        let (v, ctx) = eval(src, crate::TEST_REGISTER).await?;
        assert_eq!(v, Value::I64(expected), "{src}");
        ctx.shutdown().await;
    }
    Ok(())
}

// A signature's declared bounds print once, by name, joined by `+`.
const PRINTED_BOUND: &str = r#"{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  let g: fn(x: string) -> bool = f;
  0
}"#;
const PRINTED_BOUNDS: &str = r#"{
  let f = 'a: Eq + Ord |x: 'a, y: 'a| -> bool x < y;
  let g: fn(x: string) -> string = f;
  0
}"#;

#[tokio::test(flavor = "current_thread")]
async fn declared_bounds_print_in_the_header() -> Result<()> {
    for (src, header) in [(PRINTED_BOUND, "fn<'a: Number>("), (PRINTED_BOUNDS, "fn<'a: Eq + Ord>(")] {
        let msg = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(header), "no {header} in: {msg}");
        assert!(!msg.contains("Number & Number"), "a doubled bound in: {msg}");
    }
    Ok(())
}
