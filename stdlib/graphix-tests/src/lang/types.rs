// Tests for type system features: type checking, annotations, type variables

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, eval_modes, is, prints, refused},
};
use netidx::publisher::Value;

const SIMPLE_TYPECHECK: &str = r#"
"foo" + 1
"#;

run!(simple_typecheck, SIMPLE_TYPECHECK, refused("cannot compute string + i64"); FuseExpect::None);

const FUNCTION_TYPES: &str = r#"
{
  let f = |x: Number, y: Number| -> string "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(function_types, FUNCTION_TYPES, refused("Number does not contain string"); FuseExpect::None);

const PARTIAL_FUNCTION_TYPES: &str = r#"
{
  let f = |x: Number, y| "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(partial_function_types, PARTIAL_FUNCTION_TYPES, refused("Number does not contain string"); FuseExpect::None);

const FUNCTION_RTYPE: &str = r#"
{
  let f = |x, y| -> Number "x is [x] and y is [y]";
  f("foo", 3)
}
"#;

run!(function_rtype, FUNCTION_RTYPE, refused("Number does not contain string"); FuseExpect::None);

const INFERRED_RTYPE: &str = r#"
{
  let f = |x, y| "x is [x] and y is [y]";
  let v = f("foo", 3);
  let g = |x| x + 1;
  g(v)
}
"#;

run!(inferred_rtype, INFERRED_RTYPE, refused("i64 does not contain"); FuseExpect::None);

const LAMBDA_CONSTRAINT: &str = r#"
{
  let f = |f: fn(s1: string, s2: string) -> string, a| f("foo", a);
  f(|x, y: Number| "[x] and [y]", "foo")
}
"#;

run!(lambda_constraint, LAMBDA_CONSTRAINT, |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("does not contain fn(x:")); FuseExpect::None);

const EXPLICIT_TYPE_VARS0: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  f("foo", "bar")
}
"#;

run!(explicit_type_vars0, EXPLICIT_TYPE_VARS0, refused("does not contain string"); FuseExpect::None);

const EXPLICIT_TYPE_VARS1: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  f(u32:1, i64:2)
}
"#;

run!(explicit_type_vars1, EXPLICIT_TYPE_VARS1, refused("u32 does not contain i64"); FuseExpect::None);

const EXPLICIT_TYPE_VARS2: &str = r#"
{
  let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
  select f(1, 1) {
    i64 as t => t
  }
}
"#;

run!(explicit_type_vars2, EXPLICIT_TYPE_VARS2, is(Value::I64(2)); FuseExpect::Jit);

const EXPLICIT_TYPE_VARS3: &str = r#"
{
  let f = 'a: Number, 'b: Number |x: 'a, y: 'b| -> ['a, 'b] x + y;
  select f(u32:1, u64:1) {
    [u32, u64] as t => t
  }
}
"#;

// Two distinct quantified operand types cannot add.
run!(explicit_type_vars3, EXPLICIT_TYPE_VARS3, refused("both operands must be one numeric type");
     FuseExpect::None);

const TYPED_ARRAYS0: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [1, 2, 3])
}
"#;

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
}; FuseExpect::Jit);

const TYPED_ARRAYS1: &str = r#"
{
  let f = |x: Array<'a>, y: Array<'a>| -> Array<Array<'a>> [x, y];
  f([1, 2, 3], [u32:1, u32:2, u32:3])
}
"#;

run!(typed_arrays1, TYPED_ARRAYS1, refused("does not contain Array<"); FuseExpect::None);

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
  type Lst<'a> = [
    `Cons('a, Lst<'a>),
    `Nil
  ];
  let l: Lst<string> = `Cons(42, `Cons(3, `Nil));
  l
}
"#;

run!(rectypes2, RECTYPES2, |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("Lst<string> does not contain `Cons(i64")); FuseExpect::None);

/// A typedef may name only its parameters (and a function type's own
/// quantifiers): an undeclared variable would be fresh at every use.
const TYPEDEF_STRAY_TVAR: &str = r#"
{
  type T<'a, 'b> = { foo: 'a, bar: 'b, f: fn(a: 'a, b: 'b, c: 'c) -> 'a };
  0
}
"#;

run!(typedef_stray_tvar_refused, TYPEDEF_STRAY_TVAR, refused("undeclared type variable"); FuseExpect::None);

/// A function type's own quantifier is declared where it is written.
const TYPEDEF_FN_QUANTIFIER: &str = r#"
{
  type T<'a> = { foo: 'a, f: fn<'c: Any>(a: 'a, c: 'c) -> 'a };
  let t: T<i64> = { foo: 1, f: |a: i64, c| a };
  let g = t.f;
  g(t.foo, "x") + g(2, 3)
}
"#;

run!(typedef_fn_quantifier, TYPEDEF_FN_QUANTIFIER, is(Value::I64(3)); FuseExpect::None);

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

run!(deep_alias_chain, DEEP_ALIAS_CHAIN, is(Value::I64(1)));

// A cyclic typedef is a compile error, not a hang.
const CYCLIC_ALIAS: &str = r#"
{
  type A = B;
  type B = A;
  let t: A = (1, 2);
  t.0
}
"#;

run!(cyclic_alias, CYCLIC_ALIAS, refused("refers back to itself through unions and aliases alone");
    FuseExpect::None);

// A renaming `use` that closes a cycle is refused at the `use`, before
// a later typedef or annotation expands it.
run!(
    use_closed_cycle_typedef,
    refused("refers back to itself through unions and aliases alone"),
    "/test.gx" => "type A = X; use self::A as X; type D = A; let result = 0"
; FuseExpect::None);

run!(
    use_closed_cycle_annotation,
    refused("refers back to itself through unions and aliases alone"),
    "/test.gx" => "type A = X; use self::A as X; let x: A = 1; let result = x"
; FuseExpect::None);

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
    refused("f64 does not contain");
    FuseExpect::None
);

const ONE_CELL_FORMALS_MIXED_CALL: &str = r#"
{
  let f = |a, b| a + b;
  f(1, 2.5)
}
"#;

// `|a, b| a + b` aliases both formals into one cell, so an i64 x f64
// call rejects.
run!(one_cell_formals_refuse_mixed_call, ONE_CELL_FORMALS_MIXED_CALL, refused("i64 does not contain f64");
     FuseExpect::None);

// A declared `'a` is a contract: a concrete f64 body under `-> 'a` is a
// def-time error.
const RIGID_TVAR_CONCRETE_BODY: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a f64:0.5;
  f(3)
}
"#;

run!(rigid_tvar_concrete_body_refused, RIGID_TVAR_CONCRETE_BODY, refused("within Number does not contain f64"); FuseExpect::None);

// The same def rejects even at an f64 call.
const RIGID_TVAR_CONCRETE_BODY_F64_CALL: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a f64:0.5;
  f(1.25)
}
"#;

run!(rigid_tvar_concrete_body_refused_at_f64_call, RIGID_TVAR_CONCRETE_BODY_F64_CALL, refused("within Number does not contain f64"); FuseExpect::None);

// `x + f64:0.` under a generic `'a: Number` formal is ill-typed.
const RIGID_TVAR_MIXED_ARITH: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + f64:0.;
  (f(i64:3), f(f64:2.5))
}
"#;

run!(rigid_tvar_mixed_arith_refused, RIGID_TVAR_MIXED_ARITH, refused("both operands must be one numeric type"); FuseExpect::None);

// The same def rejects at an f64-only use: the def itself is ill-typed.
const RIGID_TVAR_MIXED_ARITH_F64_CALL: &str = r#"
{
  let f = 'a: Number |x: 'a| -> 'a x + f64:0.;
  f(f64:2.5)
}
"#;

run!(rigid_tvar_mixed_arith_refused_at_f64_call, RIGID_TVAR_MIXED_ARITH_F64_CALL, refused("both operands must be one numeric type"); FuseExpect::None);

// An unannotated formal infers monomorphic: `|x| x + i64:1` is
// fn(i64) -> i64, so the f64 site rejects.
const UNANNOTATED_FORMAL_MONOMORPHIC: &str = r#"
{
  let f = |x| x + i64:1;
  (f(i64:3), f(f64:2.5))
}
"#;

run!(unannotated_formal_refuses_f64_call, UNANNOTATED_FORMAL_MONOMORPHIC, refused("i64 does not contain f64"); FuseExpect::None);

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
}; FuseExpect::Jit);

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
    refused("and cannot hold { b: f64, y: i64 }");
    FuseExpect::None
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
    refused("must be fully known here");
    FuseExpect::None
);

// `Concrete` is judged through a type's references: a typedef naming a
// reference or a function is no read target, as the type written out is
// not.
run!(
    concrete_refuses_a_reference_through_a_typedef,
    r#"{ type R = &i64; let r: Result<R, `ParseError(string)> = str::parse("42"); r }"#,
    refused("does not contain ['b: unbound within Concrete & NoError");
    FuseExpect::None
);
run!(
    concrete_refuses_a_function_through_a_typedef,
    r#"{ type F = fn(x: i64) -> i64;
         let r: Result<F, `ParseError(string)> = str::parse("42"); r }"#,
    refused("does not contain ['b: unbound within Concrete & NoError");
    FuseExpect::None
);
run!(
    concrete_takes_data_through_a_typedef,
    r#"{ type R = (i64, string); let r: Result<R, `ParseError(string)> = str::parse("(41, \"a\")");
         r$.0 + 1 }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(42)))
);

// A type-directed builtin's target is `'b: Concrete`: refused where it
// settles open, at the definition when the definition owns the cell (a
// function never called included), at each call when the signature
// carries it.
#[tokio::test(flavor = "current_thread")]
async fn concrete_targets_are_known_where_they_settle() -> Result<()> {
    for src in [
        r#"{ let f = |x| { let y = str::parse("1"); x }; 0 }"#,
        r#"{ let p = |s| str::parse(s); p("1") }"#,
        r#"{ let p = 'b: Concrete |s: string| -> Result<'b, `ParseError(string)> str::parse(s); p("1") }"#,
        r#"{ let m = {"a" => 1}; map::map(m, |(k, v)| (json::read("1"), v)) }"#,
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(
            msg.contains("must be fully known here"),
            "wrong refusal for {src}: {msg}"
        );
    }
    for (src, expected) in [
        (
            r#"{ let p = |s| str::parse(s); let x: Result<i64, `ParseError(string)> = p("41"); x$ + 1 }"#,
            42,
        ),
        (
            r#"{ let r: Result<i64, [`JsonErr(string), `InvalidCast(string)]> = json::read("41"); r$ + 1 }"#,
            42,
        ),
        (
            r#"{ let p = |s: string| -> Result<i64, `ParseError(string)> str::parse(s); p("41")$ + 1 }"#,
            42,
        ),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(expected), "{src}");
    }
    Ok(())
}

// A builtin that wraps a function declares it `'a: Function`: anything
// else is refused by the check, a parameter's at its call, a cell left
// open (⊥ binds nothing) at its settle, and every function type is
// admitted, a typedef's included.
#[tokio::test(flavor = "current_thread")]
async fn function_bounds_admit_only_functions() -> Result<()> {
    for src in [
        r#"queuefn(#trigger: 0, 0)"#,
        r#"queuefn(#trigger: 0, never())"#,
        r#"{ let g = |h| queuefn(#trigger: 0, h); g(1) }"#,
        r#"{ let k: [fn(x: i64) -> i64, null] = null; queuefn(#trigger: 0, k) }"#,
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(
            msg.contains("Function does not contain")
                || msg.contains("must be a function here"),
            "wrong refusal for {src}: {msg}"
        );
    }
    for src in [
        r#"{ type F = fn(x: i64) -> i64; let k: F = |x| x + 1; queuefn(#trigger: 0, k)(41) }"#,
        r#"{ let g = |h| queuefn(#trigger: 0, h); g(|x: i64| x + 1)(41) }"#,
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(42), "{src}");
    }
    Ok(())
}

// A function-bounded builtin passed as a value stays a scheme through the
// call that carries it: each call of the value picks its own 'a.
#[tokio::test(flavor = "current_thread")]
async fn function_bounded_builtin_passed_as_a_value() -> Result<()> {
    let src = r#"{
        let g = filter(queuefn, |x| true);
        let inc = g(#trigger: null, |x: i64| x + 1);
        let n = g(#trigger: null, |s: string| str::len(s));
        inc(40) + n("a")
    }"#;
    let v = eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
    assert_eq!(v, Value::I64(42));
    eval_modes("filter(queuefn, |x| true)", crate::TEST_REGISTER)
        .await?
        .map_err(anyhow::Error::msg)?;
    Ok(())
}

// A generic function's signature is what its body bound: a call's types
// follow from it, so a mismatch is refused at the call, never inside an
// instance.
#[tokio::test(flavor = "current_thread")]
async fn a_signature_types_its_calls() -> Result<()> {
    for src in [
        r#"{ let first = |xs| xs[0]$; let n: i64 = first(["a"]); n }"#,
        r#"{ let tm = |x| [x, x]; let r: Array<string> = tm(1); 0 }"#,
        r#"{ let g = |(k, v)| select k == 2 { true => v, false => null }; let r: [string, null] = g((2, 3)); 0 }"#,
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains("does not contain"), "wrong refusal for {src}: {msg}");
    }
    for (src, expected) in [
        (r#"{ let first = |xs| xs[0]$; let n: string = first(["a"]); str::len(n) }"#, 1),
        (r#"{ let tm = |x| [x, x]; let r: Array<i64> = tm(1); r[1]$ }"#, 1),
        (
            r#"{ let g = |(k, v)| select k == 2 { true => v, false => null }; let r: [i64, null] = g((2, 3)); r$ }"#,
            3,
        ),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(expected), "{src}");
    }
    Ok(())
}

// A let-bound bare cast (no `$`/`?`) has the fallible union type and
// marshals as a Value.
const CAST_LET_WIRE_SHAPE: &str = r#"
{
  let v = cast<f64>(i64:-9223372036854775808);
  (v, 42)
}
"#;

run!(
    cast_let_wire_shape,
    CAST_LET_WIRE_SHAPE,
    prints("[f64:-9223372036854776000., i64:42]")
);

// The f32 twin, plus a `$` consumer over the widened shape.
const CAST_LET_WIRE_SHAPE_F32: &str = r#"
{
  let v = cast<f32>(i64:3);
  (v, cast<f32>(f64:1.5)$)
}
"#;

run!(cast_let_wire_shape_f32, CAST_LET_WIRE_SHAPE_F32, prints("[f32:3., f32:1.5]"));

// A union member whose tag the value carries is its reading, whatever a
// tagless reading of an earlier member says; a bare string is a tag only
// of a bare variant, and a tagged value whose payload does not fit is
// refused rather than read tagless.
const CAST_READS_THE_TAG: &str = r#"
{
  let v: Array<string> = ["Zed", "5"];
  let s = "Zed";
  let bad: Array<string> = ["Zed", "x"];
  let refused = select cast<[`Abc(string, string), `Zed(i64)]>(bad) {
    error as _ => true,
    _ => false
  };
  (cast<[`Abc(string, i64), `Zed(i64)]>(v)$, cast<[string, `Zed(i64)]>(s)$, refused)
}
"#;

run!(cast_reads_the_tag, CAST_READS_THE_TAG, prints(r#"[["Zed", i64:5], "Zed", true]"#));

// A cast to an integer type holds the value exactly or refuses it,
// whatever the source: a narrower integer, a float (its fraction
// dropped) or a string; a union target takes the first member in
// canonical order (narrowest integer first) that holds it, however the
// union is written.
const CAST_TO_AN_INTEGER_IS_EXACT: &str = r#"
{
  let big = 300;
  let refused = |r: [u8, i8, i16, u16, u32, i64, Error<`InvalidCast(string)>]| select r {
    error as _ => true,
    _ => false
  };
  (
    refused(cast<u8>(f64:300.5)),
    refused(cast<i8>(f64:-300.5)),
    refused(cast<u8>(big)),
    refused(cast<u16>("70000")),
    refused(cast<u32>("4294967296")),
    refused(cast<i64>(f64:0. / f64:0.)),
    cast<u8>(f64:200.7)$,
    cast<i16>(f64:-3.9)$,
    cast<u64>("18446744073709551615")$,
    cast<[u8, i64]>("300")$,
    cast<[i64, u8]>("3")$,
    cast<f64>(i8:-5)$
  )
}
"#;

run!(
    cast_to_an_integer_is_exact,
    CAST_TO_AN_INTEGER_IS_EXACT,
    prints(
        "[true, true, true, true, true, true, u8:200, i16:-3, u64:18446744073709551615, i64:300, u8:3, f64:-5.]"
    )
);

// A union, and a primitive set, takes a member that holds the value exactly
// before one that would drop part of it; only where none does is a fraction
// dropped.
const CAST_UNION_PREFERS_EXACT: &str = r#"
(
  cast<[i64, f64]>("3.5")$,
  cast<[i64, f64]>("3")$,
  cast<Number>("3.1")$,
  cast<[i64, u8]>(f64:3.7)$
)
"#;

run!(
    cast_union_prefers_exact,
    CAST_UNION_PREFERS_EXACT,
    prints("[f64:3.5, i64:3, f64:3.1, u8:3]")
);

// An integer member holds a float or a decimal, as a value or as its text,
// only where no real member holds it exactly, so a whole float and its print
// read alike; an integer's text still takes the narrowest integer, and an
// integer target alone drops the point.
const CAST_FLOAT_VALUE_AND_TEXT_READ_ALIKE: &str = r#"
{
  let x = 3.0;
  let d = cast<decimal>(3)$;
  let p: Result<[i64, f64], `ParseError(string)> = str::parse("3.0");
  (
    cast<[i64, decimal]>(x)$,
    cast<[i64, decimal]>("[x]")$,
    cast<[u8, f32]>(x)$,
    cast<[u8, f32]>("[x]")$,
    cast<[i64, f64]>("3.0")$,
    p$,
    cast<[u8, f64]>("1e2")$,
    cast<[i64, f64]>(d)$,
    cast<[i64, u8]>("3")$,
    cast<i64>("3.0")$,
    cast<i64>(3.5)$
  )
}
"#;

run!(
    cast_float_value_and_text_read_alike,
    CAST_FLOAT_VALUE_AND_TEXT_READ_ALIKE,
    prints(
        "[decimal:3.0, decimal:3.0, f32:3., f32:3., f64:3., f64:3., f64:100., f64:3., u8:3, i64:3, i64:3]"
    )
);

// Where no member is real an integer holds a whole float, value or text
// alike, and takes a fraction too, dropping it, before a member outside
// the number's class (a datetime, a duration) that holds it whole.
const CAST_WHOLE_FLOAT_WITHOUT_A_REAL_MEMBER: &str = r#"
(
  cast<[i64, datetime]>("3.0")$,
  cast<[i64, datetime]>(3.0)$,
  cast<[i64, duration]>("3.0")$,
  cast<[i64, duration]>(3.0)$,
  cast<[i64, duration]>("3.5")$,
  cast<[i64, duration]>(3.5)$
)
"#;

run!(
    cast_whole_float_without_a_real_member,
    CAST_WHOLE_FLOAT_WITHOUT_A_REAL_MEMBER,
    prints("[i64:3, i64:3, i64:3, i64:3, i64:3, i64:3]")
);

// The rule holds at every leaf: an element union reads a float as the
// scalar union does, through a cast and through `str::parse` alike, and a
// named real member is a real member.
const CAST_FLOAT_ELEMENTS_READ_AS_THE_SCALAR: &str = r#"
{
  type F = f32;
  let p: Result<Array<[u8, f32]>, `ParseError(string)> = str::parse(r"[3.0, 4.5]");
  (p$, cast<Array<[u8, f32]>>([3.0, 4.5])$, cast<[i64, F]>(3.0)$, cast<[i64, F]>("3.0")$)
}
"#;

run!(
    cast_float_elements_read_as_the_scalar,
    CAST_FLOAT_ELEMENTS_READ_AS_THE_SCALAR,
    prints("[[f32:3., f32:4.5], [f32:3., f32:4.5], f32:3., f32:3.]")
);

// Data composites are judged by every leaf, and a reading that keeps every
// float real comes before one that reads a float as an integer (a union of
// tuples has no kernel ABI form, so the cast node-walks).
const CAST_UNION_PREFERS_EXACT_COMPOSITE: &str = r#"
(
  cast<[(i64, i64), (f64, f64)]>([f64:1.5, f64:2.5])$,
  cast<[(i64, i64), (f64, f64)]>([f64:1., f64:2.])$,
  cast<[(i64, i64), (f32, f32)]>([f64:1., f64:2.])$,
  cast<[(i64, i64), (f32, f32)]>([i64:1, i64:2])$
)
"#;

run!(
    cast_union_prefers_exact_composite,
    CAST_UNION_PREFERS_EXACT_COMPOSITE,
    prints("[[f64:1.5, f64:2.5], [f64:1., f64:2.], [f32:1., f32:2.], [i64:1, i64:2]]");
    FuseExpect::None
);

// Where no member holds a number exactly, a real member that holds it
// rounded to its precision comes before one that would drop part of it, as
// a value, as text, at every element and leaf by leaf in a union of tuples;
// an integer a real member rounds is held so too. An integer member that
// holds a float exactly still comes first, and an infinity a finite value
// overflows to is no rounding.
const CAST_ROUNDING_BEFORE_A_DROPPED_PART: &str = r#"
{
  let d = cast<decimal>("0.1000000000000000000001")$;
  let p: Result<Array<[i64, f32]>, `ParseError(string)> = str::parse(r"[0.1, 2.0]");
  (
    cast<[i64, f32]>(0.1)$,
    cast<[i64, f32]>("0.1")$,
    cast<[i64, f64]>(d)$,
    p$,
    cast<Array<[i64, f32]>>([0.1, 2.0])$,
    cast<[(i64, i64), (f32, f32)]>([0.5, 0.1])$,
    cast<[f32, u8]>(16777217)$,
    cast<[f32, u8]>("16777217")$,
    cast<[i64, f32]>(1234567900.0)$,
    cast<[i64, f32]>(1e39)$,
    cast<f32>(0.1)$
  )
}
"#;

run!(
    cast_rounding_before_a_dropped_part,
    CAST_ROUNDING_BEFORE_A_DROPPED_PART,
    prints(
        "[f32:0.1, f32:0.1, f64:0.1, [f32:0.1, f32:2.], [f32:0.1, f32:2.], [f32:0.5, f32:0.1], f32:16777216., f32:16777216., i64:1234567900, f32:inf, f32:0.1]"
    )
);

// A number keeps its kind where a member of its kind holds it exactly: an
// integer, value or text, goes to a real member only where no integer
// member holds it, however the union names or orders its members, at every
// leaf, as a float goes to an integer member only where no real one holds it
// (a union of tuples has no kernel ABI form, so the casts node-walk).
const CAST_A_NUMBER_KEEPS_ITS_KIND: &str = r#"
{
  type B = u8;
  (
    cast<[f64, B]>(2)$,
    cast<[f64, B]>("2")$,
    cast<[f64, B]>(2.0)$,
    cast<[f64, B]>(300)$,
    cast<[(B, i64), (f32, i64)]>([2, 5])$,
    cast<[(B, i64), (f32, i64)]>([2.0, 5])$,
    cast<[Array<f64>, Array<B>]>([2, 3])$
  )
}
"#;

run!(
    cast_a_number_keeps_its_kind,
    CAST_A_NUMBER_KEEPS_ITS_KIND,
    prints(
        "[u8:2, u8:2, f64:2., f64:300., [u8:2, i64:5], [f32:2., i64:5], [u8:2, u8:3]]"
    );
    FuseExpect::None
);

// The order a union reads a value in: the members of the value's class
// first, across every round (a float stays a number though a string holds
// its text exactly), then for text the members of the class of the scalar
// it parses as, then the rest; a bool is held exactly by the number 1 or
// 0, so the canonical order picks among the numbers and a string; a
// decimal made of a whole float keeps its point, so its text reads back
// as a decimal.
const CAST_UNION_ORDER: &str = r#"
{
  let d = cast<decimal>(3.0)$;
  let dt = "[d]";
  (
    cast<[f32, string]>(0.1)$,
    cast<[i64, string]>(3.5)$,
    cast<[duration, decimal]>("3.0")$,
    cast<[duration, decimal]>(3.0)$,
    cast<[i64, bool]>("true")$,
    cast<[f32, u8]>(true)$,
    cast<[f64, i64]>(false)$,
    cast<[i64, string]>(true)$,
    d,
    dt,
    cast<[i64, decimal]>(dt)$,
    cast<decimal>(f32:2.0)$
  )
}
"#;

run!(
    cast_union_order,
    CAST_UNION_ORDER,
    prints(
        r#"[f32:0.1, i64:3, decimal:3.0, decimal:3.0, true, u8:1, i64:0, i64:1, decimal:3.0, "3.0", decimal:3.0, decimal:2.0]"#
    )
);

// An infinity a finite value overflows to is a union's last resort, after
// every member of every class failed short of it: a string holding the
// text exactly comes first, a primitive set or not, in an element too.
const CAST_OVERFLOW_IS_A_LAST_RESORT: &str = r#"
{
  let k = |v: [f32, string]| select v {
    f32 as f => "[f]",
    string as s => "text, [str::len(s)] chars"
  };
  let ka = |v: [f32, string, `A]| select v {
    f32 as f => "[f]",
    string as s => "text, [str::len(s)] chars",
    `A => "A"
  };
  (
    k(cast<[f32, string]>(1e300)$),
    k(cast<[f32, string]>(-1e300)$),
    k(cast<[f32, string]>(1e300 * 1e300)$),
    ka(cast<[f32, string, `A]>(1e300)$),
    array::map(cast<Array<[f32, string, `A]>>([1e300, 0.1])$, ka),
    cast<f32>(1e300)$,
    cast<[f32, i64]>(1e300)$,
    cast<[f32, `A]>("1e300")$,
    cast<[f32, null]>(-1e300)$
  )
}
"#;

run!(
    cast_overflow_is_a_last_resort,
    CAST_OVERFLOW_IS_A_LAST_RESORT,
    prints(
        r#"["text, 303 chars", "text, 304 chars", "inf", "text, 303 chars", ["text, 303 chars", "0.1"], f32:inf, f32:inf, f32:inf, f32:-inf]"#
    )
);

// Integer text too long for every integer type parses as the decimal it
// names, else the nearest f64, so a member holding it exactly comes first
// and an infinity stays the last resort; text keeps its own class first.
const CAST_LONG_INTEGER_TEXT: &str = r#"
{
  let big = "350000000000000000000000000000000000000";
  let dec = "12345678901234567890123";
  let parsed: [f32, decimal] = str::parse(dec)$;
  let which = |v: [f32, f64]| select v {
    f64 as f => f == 3.5e38 || f == -3.5e38,
    f32 as _ => false
  };
  (
    which(cast<[f32, f64]>(big)$),
    which(cast<[f32, f64]>("-[big]")$),
    cast<[f32, decimal]>(dec)$,
    cast<[f64, decimal]>("-[dec]")$,
    cast<[f32, string]>(dec)$,
    cast<f32>(big)$,
    parsed
  )
}
"#;

run!(
    cast_long_integer_text,
    CAST_LONG_INTEGER_TEXT,
    prints(
        r#"[true, true, decimal:12345678901234567890123., decimal:-12345678901234567890123., "12345678901234567890123", f32:inf, decimal:12345678901234567890123.]"#
    )
);

// A float casts to the decimal its shortest text names, all seventeen
// significant digits of it, an f32 by its own shortest text.
run!(
    float_decimal_keeps_every_digit,
    r#"{
  let a = cast<decimal>(0.30000000000000004)$;
  let b = cast<decimal>(0.1 + 0.2)$;
  let c = cast<decimal>(1.0000000000000002)$;
  let d = cast<decimal>(f32:0.1)$;
  let e = cast<decimal>(123456.78901234567)$;
  (a, b, c, d, e, a == decimal:0.30000000000000004)
}"#,
    prints(
        "[decimal:0.30000000000000004, decimal:0.30000000000000004, decimal:1.0000000000000002, decimal:0.1, decimal:123456.78901234567, true]"
    )
);

// A float too small for a decimal's 28 places is a zero with a point and the
// float's sign, as a whole float is.
run!(
    float_decimal_too_small_is_a_signed_zero,
    r#"{
  let a = cast<decimal>(1e-29)$;
  let b = cast<decimal>(5e-324)$;
  let c = cast<decimal>(f32:1e-30)$;
  let d = cast<decimal>(-1e-30)$;
  let e = cast<decimal>(-0.0)$;
  let f = cast<decimal>(0.0)$;
  let g = cast<decimal>(1.5e-28)$;
  ("[a] [b] [c] [d] [e] [f] [g]", a == decimal:0.0, d == decimal:0.0)
}"#,
    prints(r#"["0.0 0.0 0.0 -0.0 -0.0 0.0 0.0000000000000000000000000002", true, true]"#)
);

// A member beside a primitive set changes nothing of that order: a union
// tries the members of every union and name in it, and each part of a
// primitive set with the members of its class.
const CAST_UNION_ORDER_PAST_NESTED_MEMBERS: &str = r#"
{
  type F = f32;
  type P = [f32, string];
  type N = [i64, string, `B];
  (
    cast<[f32, string, `A]>(0.1)$,
    cast<[f32, string, Array<i64>]>(0.1)$,
    cast<[i64, string, `A]>(3.5)$,
    cast<[i64, string, F]>(3.5)$,
    cast<[i64, duration, `A]>("3.5")$,
    cast<[i64, duration, `A]>(3.5)$,
    cast<[f32, u8, `A]>(true)$,
    cast<Array<[f32, string, `A]>>([0.1])$,
    cast<[P, `A]>(0.1)$,
    cast<[N, `A]>(3.5)$
  )
}
"#;

run!(
    cast_union_order_past_nested_members,
    CAST_UNION_ORDER_PAST_NESTED_MEMBERS,
    prints(
        "[f32:0.1, f32:0.1, i64:3, f32:3.5, i64:3, i64:3, u8:1, [f32:0.1], f32:0.1, i64:3]"
    );
    FuseExpect::None
);

// A union reads its primitives in canonical order however it names them:
// a name for a primitive set is the set, so `Real` and `Number` read as
// their primitives written flat do.
const CAST_UNION_PRIMITIVES_BY_NAME: &str = r#"
{
  type F = f32;
  type U = u8;
  let s: Any = "3.0";
  (
    cast<Number>(s)$,
    cast<Real>(s)$,
    cast<[f32, f64, decimal]>(s)$,
    cast<Number>("0.1")$,
    cast<[f32, f64, decimal]>("0.1")$,
    cast<Real>(u8:3)$,
    cast<[f32, f64, decimal]>(u8:3)$,
    cast<[F, U]>(true)$,
    cast<[f32, u8]>(true)$
  )
}
"#;

run!(
    cast_union_primitives_by_name,
    CAST_UNION_PRIMITIVES_BY_NAME,
    prints(
        "[f32:3., f32:3., f32:3., f64:0.1, f64:0.1, f32:3., f32:3., u8:1, u8:1]"
    );
    FuseExpect::None
);

// Unmarked data reads back as the member that reads it exactly: a tuple of
// its length, then a struct, before an array, so what pack and json write
// of a tuple or a struct reads back as it.
const DATA_READS_BACK_AS_WRITTEN: &str = r#"
{
  type U = [(i64, i64), Array<f64>];
  let v: U = (1, 2);
  let p: Result<U, [`InvalidCast(string), `PackErr(string)]> = pack::read(pack::write_bytes(v)$);
  let a: U = [f64:1.5];
  let pa: Result<U, [`InvalidCast(string), `PackErr(string)]> = pack::read(pack::write_bytes(a)$);
  (p$ == v, pa$ == a)
}
"#;

run!(data_reads_back_as_written, DATA_READS_BACK_AS_WRITTEN, prints("[true, true]"));

// JSON keeps no kind: a target union whose members carry no tag is refused
// (`JsonData`), since what was written as one could read back as the
// other. An object reads as its key/value pairs, and a tuple and an array
// are both arrays.
run!(
    json_target_union_its_data_cannot_tell_apart_refused,
    r#"{
  type S = [{a: i64}, Array<(string, i64)>];
  let r: Result<S, [`InvalidCast(string), `JsonErr(string)]> = json::read("{}");
  r
}"#,
    refused("'b: JsonData refuses [Array<(string, i64)>, { a: i64 }]")
);

run!(
    toml_target_union_its_data_cannot_tell_apart_refused,
    r#"{
  let r: Result<[u8, i64], [`InvalidCast(string), `TomlErr(string)]> = toml::read("1");
  r
}"#,
    refused("'b: TomlData refuses [u8, i64]")
);

// Members JSON writes apart read back as written: each a tag, a bare
// variant's or one with a payload, beside null.
const JSON_TARGET_MEMBERS_WRITTEN_APART: &str = r#"
{
  type V = [`I(i64), `F(f64), `A, `B(string), `S({n: string}), `T((string, i64)), null];
  let vs: Array<V> = [`I(1), `F(f64:1.0), `A, `B("x"), `S({n: "y"}), `T(("k", 1)), null];
  let back: Result<Array<V>, [`InvalidCast(string), `JsonErr(string)]> =
    json::read(json::write_str(vs)$);
  let w: [Array<string>, null] = ["k"];
  let wb: Result<[Array<string>, null], [`InvalidCast(string), `JsonErr(string)]> =
    json::read(json::write_str(w)$);
  (back$ == vs, wb$ == w)
}
"#;

run!(
    json_target_members_written_apart_read_back,
    JSON_TARGET_MEMBERS_WRITTEN_APART,
    prints("[true, true]")
);

// A cast leaves a value's runtime class only by parsing a string into a
// scalar, by printing a number, bool, datetime or duration plainly,
// between a number and a datetime (Unix seconds) or a duration
// (seconds), from a bool to a number (1 or 0), or by reading an array of
// numbers as bytes; no scalar becomes a collection, and a union tries
// the members of the value's own class first.
const CAST_STAYS_IN_CLASS: &str = r#"
(
  cast<string>(42),
  cast<string>(f64:1.5),
  cast<string>(true),
  cast<[i8, string]>(300),
  cast<[string, Array<f64>]>([1, 2]),
  cast<[Array<f64>, null]>([1, 2]),
  cast<bool>("true"),
  cast<duration>(f64:1.5),
  cast<f64>(duration:1.5s),
  cast<i64>(datetime:"2020-01-01T00:00:00Z"),
  cast<datetime>(1577836800),
  cast<i64>(true),
  cast<f64>(false),
  select cast<bool>(1) { error as _ => true, _ => false },
  select cast<bool>("1") { error as _ => true, _ => false },
  select cast<[i8, bool]>(300) { error as _ => true, _ => false },
  select cast<string>([1, 2]) { error as _ => true, _ => false },
  select cast<bool>(duration:1.s) { error as _ => true, _ => false },
  select cast<Array<i64>>(5) { error as _ => true, _ => false },
  select cast<List<i64>>(5) { error as _ => true, _ => false }
)
"#;

run!(
    cast_stays_in_class,
    CAST_STAYS_IN_CLASS,
    prints(
        r#"["42", "1.5", "true", "300", [f64:1., f64:2.], [f64:1., f64:2.], true, duration:1.5s, f64:1.5, i64:1577836800, datetime:"2020-01-01 00:00:00 UTC", i64:1, f64:0., true, true, true, true, true, true, true]"#
    )
);

// A decimal prints plainly, as a float does, and reads back exactly.
run!(
    cast_prints_a_decimal_plainly,
    r#"{
  let s = cast<string>(decimal:5.)$;
  (s == "5") && (cast<string>(decimal:1.25)$ == "1.25") && (cast<decimal>(s)$ == decimal:5.)
}"#,
    is(Value::Bool(true))
);

// A union cast keeps a held variant in the member it inhabits, as it
// keeps a held tuple, and a struct field the data omits reads as null
// through its type's Cast where the type takes null only that way.
const CAST_KEEPS_WHAT_A_MEMBER_HOLDS: &str = r#"
{
  type Opt = Abstract<[i64, null]>;
  impl Cast for Opt {};
  let v: Any = `A(1.5, 2.5);
  let t: Any = (1.5, 2.5);
  let a: Any = {a: 1};
  (
    cast<[`A(i64, i64), `A(f64, f64)]>(v)$,
    cast<[(i64, i64), (f64, f64)]>(t)$,
    cast<{a: i64, o: Opt}>(a)$.o.0
  )
}
"#;

run!(cast_keeps_what_a_member_holds, CAST_KEEPS_WHAT_A_MEMBER_HOLDS, |v: Result<
    &Value,
>| {
    format!("{}", v.unwrap()) == r#"[["A", f64:1.5, f64:2.5], [f64:1.5, f64:2.5], null]"#
});

// A rec def whose base arm returns a param: the elem union is honestly
// `[i64, string]`.
const REC_RETURN_PARAM_ELEM: &str = r#"
{
  let a = [0, {let rec f = |n, acc| select n {0 => acc, _ => f(n - 1, acc)}; f(3, "hi")}, 4];
  array::map(a, |x| select x { i64 as n => n > 3, _ => true })
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
    FuseExpect::Jit
);

// The Fn-element twin: the union is `[i64, fn(...)]`.
const REC_RETURN_FN_ELEM: &str = r#"
{
  let a = [0, 0, {let rec f = |n, acc| select n {0 => acc, _ => f(n - 1, acc)}; f(3, buffer::to_string)}, 4, 0];
  array::map(a, |x| select x { i64 as n => n > 3, _ => true })
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
    FuseExpect::Jit
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
    refused("cannot infer a finite type here");
    FuseExpect::None
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
    is(Value::I64(0));
    FuseExpect::Jit
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
    is(Value::I64(0));
    FuseExpect::Jit
);

const REC_BLOCK_MULTI_MEMBER_COLLAPSES: &str = r#"
{
  let rec f = |x: i64| { let t = select x { 0 => 0, 1 => "a", _ => f(x - 1) }; t };
  f(3)
}
"#;

run!(
    rec_block_multi_member_collapses,
    REC_BLOCK_MULTI_MEMBER_COLLAPSES,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a");
    FuseExpect::Jit
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
    refused("cannot infer a finite type here");
    FuseExpect::None
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
    refused("cannot infer a finite type here");
    FuseExpect::None
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
    refused("cannot infer a finite type here");
    FuseExpect::None
);

// A call pre-unifies its formal with the argument's type before the
// argument is checked, so the literal's element cell is reachable when
// the literal binds it: `x` would hold `Array<[i64, fn(x: <x>) -> _]>`.
const REC_SELF_IN_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f([1, f]); 0 }";

run!(
    rec_self_in_array_arg_rejects,
    REC_SELF_IN_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_ALONE_IN_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f([f]); 0 }";

run!(
    rec_self_alone_in_array_arg_rejects,
    REC_SELF_ALONE_IN_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_PARAM_IN_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f([1, x]); 0 }";

run!(
    rec_param_in_array_arg_rejects,
    REC_PARAM_IN_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_LIST_ARG_REJECTS: &str = "{ let rec f = |x| f([<f>]); 0 }";

run!(
    rec_self_in_list_arg_rejects,
    REC_SELF_IN_LIST_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_MAP_ARG_REJECTS: &str = r#"{ let rec f = |x| f({"a" => f}); 0 }"#;

run!(
    rec_self_in_map_arg_rejects,
    REC_SELF_IN_MAP_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_STRUCT_ARG_REJECTS: &str = "{ let rec f = |x| f({a: f}); 0 }";

run!(
    rec_self_in_struct_arg_rejects,
    REC_SELF_IN_STRUCT_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_STRUCT_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f({a: [f]}); 0 }";

run!(
    rec_self_in_struct_array_arg_rejects,
    REC_SELF_IN_STRUCT_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_TUPLE_ARG_REJECTS: &str = "{ let rec f = |x| f((1, f)); 0 }";

run!(
    rec_self_in_tuple_arg_rejects,
    REC_SELF_IN_TUPLE_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_TUPLE_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f(([f], 1)); 0 }";

run!(
    rec_self_in_tuple_array_arg_rejects,
    REC_SELF_IN_TUPLE_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_REF_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f(&[f]); 0 }";

run!(
    rec_self_in_ref_array_arg_rejects,
    REC_SELF_IN_REF_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_IN_LET_BOUND_ARRAY_REJECTS: &str =
    "{ let rec f = |x| { let v = [1, f]; f(v) }; 0 }";

run!(
    rec_self_in_let_bound_array_rejects,
    REC_SELF_IN_LET_BOUND_ARRAY_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const REC_SELF_LET_BOUND_IN_ARRAY_ARG_REJECTS: &str =
    "{ let rec f = |x| { let g = f; g([g]) }; 0 }";

run!(
    rec_self_let_bound_in_array_arg_rejects,
    REC_SELF_LET_BOUND_IN_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const CLOSURE_PASSES_REC_SELF_IN_ARRAY_REJECTS: &str =
    "{ let rec f = |x| { let g = |y| f([y, f]); g(x) }; 0 }";

run!(
    closure_passes_rec_self_in_array_rejects,
    CLOSURE_PASSES_REC_SELF_IN_ARRAY_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

// Without `rec` a closure reaches itself only through a parameter, whose
// function type no call can know.
const CLOSURE_PASSES_ITSELF_IN_ARRAY_REJECTS: &str = "{ let f = |g, x| g(g, [1, g]); 0 }";

run!(
    closure_passes_itself_in_array_rejects,
    CLOSURE_PASSES_ITSELF_IN_ARRAY_REJECTS,
    refused("reading a fn needs its type known here");
    FuseExpect::None
);

// A written binding is not generalized: the call's formal is `f`'s own
// cell, which the literal's element would hold.
const WRITTEN_SELF_IN_ARRAY_ARG_REJECTS: &str = "{ let f = |x| 0; f <- |x| 0; f([f]) }";

run!(
    written_self_in_array_arg_rejects,
    WRITTEN_SELF_IN_ARRAY_ARG_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

const WRITTEN_SELF_NESTED_BESIDE_LET_REJECTS: &str =
    "{ let a = [1]; let f = |x| 0; f <- |x| 0; f([a, [f]]) }";

run!(
    written_self_nested_beside_let_rejects,
    WRITTEN_SELF_NESTED_BESIDE_LET_REJECTS,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

// A literal over values another literal shares (each `let` holds its
// literal's type) binds its element cell as before.
const SHARED_LITERAL_CHAIN: &str = r#"
{
  let a = [1, 2];
  let b = [a, a];
  let c = [b, [a]];
  c
}
"#;

run!(
    shared_literal_chain,
    SHARED_LITERAL_CHAIN,
    |v: Result<&Value>| match v {
        Ok(Value::Array(c)) => {
            let one_two = Value::Array([Value::I64(1), Value::I64(2)].into());
            let b = Value::Array([one_two.clone(), one_two.clone()].into());
            let a1 = Value::Array([one_two].into());
            &**c == [b, a1]
        }
        _ => false,
    };
    FuseExpect::Jit
);

// A recursive call's element in the literal its definition returns is
// the literal's own element type: `'e ⊇ [i64, 'e]` collapses to i64.
const REC_ELEMENT_OF_OWN_LITERAL_COLLAPSES: &str = r#"
{
  let rec g = |x: i64| select x { 0 => [1], _ => [2, g(x - 1)[0]$] };
  g(3)
}
"#;

run!(
    rec_element_of_own_literal_collapses,
    REC_ELEMENT_OF_OWN_LITERAL_COLLAPSES,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if &**a == [Value::I64(2), Value::I64(2)]);
    FuseExpect::Jit
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
    refused("and cannot hold Array<");
    FuseExpect::None
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
    refused("src is Array<Array<i64>> and cannot hold");
    FuseExpect::None
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
    is(Value::I64(4));
    FuseExpect::Jit
);

// Unifying an inferred `List<'a>` against a value whose deep tail is a
// Fn rejects at every depth.
#[tokio::test]
async fn recursive_fn_tail_rejected_at_every_depth() -> Result<()> {
    let program = |tail: &str| {
        format!(
            r#"{{
  type L<'a> = [`Cons('a, L<'a>), `Nil];
  let rec find = |l: L<'a>, f: fn(x: 'a) -> bool| -> ['a, null] select l {{
    `Nil => null,
    `Cons(x, rest) => select f(x) {{ true => x, false => find(rest, f) }}
  }};
  let l = `Cons(i64:0, {tail});
  find(l, |x| true)
}}"#
        )
    };
    for tail in ["once", "`Cons(i64:3, once)", "`Cons(i64:3, `Cons(i64:5, once))"] {
        let src = program(tail);
        match eval_modes(&src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(
                    msg.contains("L<'a: unbound> does not contain `Cons(i64"),
                    "{tail}: {msg}"
                )
            }
            Ok(v) => panic!("a Fn in a recursive tail must be refused: {tail} => {v:?}"),
        }
    }
    match eval_modes(&program("`Nil"), crate::TEST_REGISTER).await? {
        Ok(Value::I64(0)) => (),
        r => panic!("a `Nil tail checks and finds 0: {r:?}"),
    }
    Ok(())
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

run!(recursive_list_find, RECURSIVE_LIST_FIND, is(Value::I64(2)));

// Non-scalar variant payload binds fuse (composite, string, value).
const VARIANT_COMPOSITE_PAYLOAD: &str = r#"
{
    type T = [`A(Array<i64>), `B];
    let v: T = `A([10, 20, 30]);
    select v { `A(xs) => xs[1]$ + array::len(xs), `B => 0 }
}"#;
run!(variant_composite_payload, VARIANT_COMPOSITE_PAYLOAD, is(Value::I64(23)); FuseExpect::Jit);

const VARIANT_STRING_PAYLOAD: &str = r#"
{
    type S = [`S(string), `N];
    let v: S = `S("hello");
    select v { `S(s) => "got [s]", `N => "none" }
}"#;
run!(variant_string_payload, VARIANT_STRING_PAYLOAD, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s.as_str() == "got hello"); FuseExpect::Jit);

const VARIANT_RECURSIVE_PAYLOAD: &str = r#"
{
    type L<'a> = [`C('a, L<'a>), `N];
    let l: L<i64> = `C(1, `C(2, `N));
    select l {
        `N => 0,
        `C(x, rest) => x + select rest { `N => 10, `C(y, _) => y * 100 }
    }
}"#;
run!(variant_recursive_payload, VARIANT_RECURSIVE_PAYLOAD, is(Value::I64(201)); FuseExpect::Jit);

const VARIANT_MAP_PAYLOAD: &str = r#"
{
    type M = [`M(Map<string, i64>), `N];
    let v: M = `M({"a" => 1, "b" => 2});
    select v { `M(m) => map::len(m), `N => 0 }
}"#;
run!(variant_map_payload, VARIANT_MAP_PAYLOAD, is(Value::I64(2)); FuseExpect::Jit);

// A recursive union that consumes nothing names no value: refused at
// its definition, not a stack overflow at the cast.
const CAST_RECURSIVE_NO_PROGRESS: &str = r#"
{
  type Loop = [i64, Loop];
  cast<Loop>("not-a-number")
}
"#;
run!(cast_recursive_no_progress, CAST_RECURSIVE_NO_PROGRESS, refused("refers back to itself through unions and aliases alone"); FuseExpect::None);

// A typedef that shadows an outer name closes a cycle through an earlier
// definition that named the outer one: that definition's walk was clean
// when it was defined, and is not once the shadow stands.
const SHADOW_CLOSES_AN_UNGUARDED_CYCLE: &str = r#"
{
  type X = i64;
  {
    type D = X;
    type X = [D, i64];
    let v: X = 3;
    v
  }
}
"#;
run!(shadow_closes_an_unguarded_cycle, SHADOW_CLOSES_AN_UNGUARDED_CYCLE, refused("refers back to itself through unions and aliases alone"); FuseExpect::None);

// A two-element array is not a list: converting it keeps both items.
const CAST_PAIR_TO_LIST: &str = r#"cast<List<i64>>([1, 2])$"#;
run!(cast_pair_to_list, CAST_PAIR_TO_LIST, prints("[i64:1, i64:2]"); FuseExpect::Jit);

// A struct cast converts each field; the names decide the match.
const CAST_STRUCT_FIELDS: &str = r#"cast<{x: i64, y: i64}>({y: "2", x: "1"})$"#;
run!(cast_struct_fields, CAST_STRUCT_FIELDS, prints(r#"[["x", i64:1], ["y", i64:2]]"#); FuseExpect::Jit);

// A reference is not a number: a cast whose source can hold one is
// refused where the check sees it, and fails where only an instance
// does.
const CAST_OF_A_REFERENCE: &str = r#"{ let v = 0; cast<f32>(&v) }"#;
run!(cast_of_a_reference, CAST_OF_A_REFERENCE, refused("can't cast a reference"); FuseExpect::None);

const CAST_OF_A_NESTED_REFERENCE: &str = r#"{ let v = 0; cast<Array<i64>>([&v]) }"#;
run!(cast_of_a_nested_reference, CAST_OF_A_NESTED_REFERENCE, refused("can't cast a reference"); FuseExpect::None);

const CAST_OF_A_REFERENCE_IN_AN_INSTANCE: &str = r#"
{
  let f = |x| cast<i64>(x);
  let v = 0;
  f(&v)
}
"#;
run!(cast_of_a_reference_in_an_instance, CAST_OF_A_REFERENCE_IN_AN_INSTANCE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if format!("{e}").contains("can't cast a reference"))
}; FuseExpect::None);

// Two applications of one constructor filled for one search are two
// types: the first, holding no reference, vouches nothing for the second.
const CAST_OF_A_REFERENCE_BESIDE_A_FILLED_APPLICATION: &str = r#"
{
  use Collection::*;
  let f = |c: Collection, y| cast<(Array<i64>, Array<Any>)>((map(c, |x| 1), map(c, |x| y)));
  let v = 5;
  f([1, 2], &v)
}
"#;
run!(cast_of_a_reference_beside_a_filled_application, CAST_OF_A_REFERENCE_BESIDE_A_FILLED_APPLICATION, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if format!("{e}").contains("can't cast a reference"))
}; FuseExpect::Jit);

// An array a union reads stays the array member it inhabits; only data
// reads as a tuple of its length or a tagged variant.
const CAST_OF_AN_ARRAY_KEEPS_ITS_MEMBER: &str = r#"
{
  type U = [(i64, i64), Array<i64>];
  type V = [`B(string), Array<string>];
  let a: Array<i64> = [1, 2];
  let s: Array<string> = ["B", "x"];
  let d: Any = [1, 2];
  let show = |u: U| select u { (i64, i64) as t => "tuple", Array<i64> as x => "array" };
  let showv = |v: V| select v { `B(_) => "variant", Array<string> as x => "array" };
  "[show(cast<U>(a)$)] [showv(cast<V>(s)$)] [show(cast<U>(d)$)]"
}
"#;
run!(
    cast_of_an_array_keeps_its_member,
    CAST_OF_AN_ARRAY_KEEPS_ITS_MEMBER,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if &**s == "array array tuple")
    }
);

// A cast reads data at most `MAX_DATA_DEPTH` (1024) values deep.
const CAST_OF_DATA_NESTED_TOO_DEEP: &str = r#"
{
  type N = Array<N>;
  let rec mk = |n: i64| -> N select n { 0 => [], n => [mk(n - 1)] };
  let read = |n: i64| {
    let a: Any = mk(n);
    select cast<N>(a) { error as e => "[e]", _ => "read" }
  };
  (read(1023), str::contains(#part: "nested too deep", read(1024)))
}
"#;
run!(
    cast_of_data_nested_too_deep,
    CAST_OF_DATA_NESTED_TOO_DEEP,
    prints(r#"["read", true]"#)
);

// An arithmetic operand that is a union with an open member narrows the
// member to the bound, never binds it to the bound: the member stays one
// type a later use picks, here ⊥ (only ⊥ was produced into `x`). A
// callback bound by `let` checks as the same one inline does.
const OPERAND_BOUND_NARROWS_AN_OPEN_MEMBER: &str = r#"
{
  let x = never();
  let f = |i| array::map([0, x], |j| i + j);
  let r = array::map([2], f);
  1
}
"#;
run!(
    operand_bound_narrows_an_open_member,
    OPERAND_BOUND_NARROWS_AN_OPEN_MEMBER,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(1))) }
);

// An open union member whose bound refuses what its known siblings leave
// takes from the whole other side: 'a is i64 in [i64, Array<string>] ⊇
// [i64, 'a: Number].
const OPEN_MEMBER_FALLS_BACK_TO_THE_WHOLE_SIDE: &str = r#"
{
  let f = 'a: Number |x: 'a, y: bool| -> ['a, i64] select y { true => x, false => 1 };
  let g = |r: [i64, Array<string>]| r;
  g(f(never(), false))
}
"#;
run!(
    open_member_falls_back_to_the_whole_side,
    OPEN_MEMBER_FALLS_BACK_TO_THE_WHOLE_SIDE,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(1))) }
);

// A callback that takes Any fixes its formal's open type to Any, so a
// Concrete bound is met; a covariant Any decides nothing of what flows
// into it: `q` is still an i64 for `h`.
const ANY_BINDS_ONLY_CONTRAVARIANTLY: &str = r#"
{
  let f = 'a: Concrete |g: fn(v: 'a) -> i64| 1;
  let q = never();
  let w: Any = q;
  q <- 2;
  let h = |z: i64| z + 1;
  [f(|v: Any| 0), h(q)]
}
"#;
run!(
    any_binds_only_contravariantly,
    ANY_BINDS_ONLY_CONTRAVARIANTLY,
    prints("[i64:1, i64:3]")
);

// A quantifier's bound reaches its uses inside a nested fn type, where the
// variable occurs more than once.
const NESTED_USES_KEEP_THE_BOUND: &str = r#"
{
  type F = fn<'a: Number>(g: fn(x: 'a) -> 'a) -> f64;
  let f: F = |g| 0.0;
  f(|x: string| x)
}
"#;
run!(
    nested_uses_keep_the_bound,
    NESTED_USES_KEEP_THE_BOUND,
    refused("does not contain fn(x: string) -> string");
    FuseExpect::None
);

// A let annotation's same-named variables are one variable, and in a
// body one the definition declares is the definition's own.
run!(
    annotation_names_one_variable_in_a_tuple,
    "{ let w: ('a, 'a) = (1, \"s\"); 0 }",
    refused("('a: i64, 'a: i64) does not contain (i64, string)");
    FuseExpect::None
);
run!(
    annotation_names_one_variable_across_a_fn_type,
    "{ let p: ('c, fn(x: 'c) -> 'c) = (1, |x: string| x); 0 }",
    refused("does not contain (i64, fn(x: string) -> string)");
    FuseExpect::None
);
run!(
    body_annotation_names_the_definitions_variable,
    "{ let f = 'e: Number |x: 'e| -> 'e { let v: 'e = 1; x }; 0 }",
    refused("'e: unbound within Number does not contain i64");
    FuseExpect::None
);
// A quantifier a definition declares is one its signature holds: no call
// can choose one only the body names.
run!(
    a_quantifier_only_the_body_reads_is_refused,
    "{ let f = 'a: Number |x| -> i64 { let v: 'a = x; 0 }; f(\"s\") }",
    refused("'a is declared but the signature does not hold it");
    FuseExpect::None
);
run!(
    a_quantifier_only_a_body_annotation_names_is_refused,
    "{ let f = 'a: Number |x: i64| -> i64 { let v: 'a = 1; 0 }; f(1) }",
    refused("'a is declared but the signature does not hold it");
    FuseExpect::None
);
run!(
    an_unused_quantifier_is_refused,
    "{ let f = 'a: Number |x: i64| -> i64 x; f(1) }",
    refused("'a is declared but the signature does not hold it");
    FuseExpect::None
);
run!(
    a_nested_definitions_unheld_quantifier_is_refused,
    "{ let f = 'a: Number |k: 'a| 'b: Int |x: 'a| [x, k]; f(1)(2) }",
    refused("'b is declared but the signature does not hold it");
    FuseExpect::None
);
run!(
    a_written_fn_types_unheld_quantifier_is_refused,
    "{ type F = fn<'a: Number>(x: i64) -> i64; let f: F = |x| x; f(1) }",
    refused("'a is declared but the signature does not hold it");
    FuseExpect::None
);
run!(
    a_quantifier_a_parameter_holds_checks,
    "{ let f = 'a: Number |x: 'a| -> 'a x; f(f32:1.5) }",
    prints("f32:1.5")
);
run!(
    a_quantifier_held_through_bounds_checks,
    "{ let f = 'a: Array<'b>, 'b: Array<'c>, 'c: Int |x: 'a| -> i64 array::len(x); \
       f([[u8:7], [u8:8]]) }",
    prints("i64:2")
);
run!(
    a_quantifier_held_through_bounds_keeps_its_bound,
    "{ let f = 'a: Array<'b>, 'b: Array<'c>, 'c: Int |x: 'a| -> i64 array::len(x); \
       f([[\"s\"]]) }",
    refused("does not contain");
    FuseExpect::None
);
// A bound that reaches its own variable is refused where it is written,
// in a definition, a written fn type and an impl head alike: a call could
// be checked against it without end. A recursive bound is a recursive type.
run!(
    a_quantifier_bounded_by_itself_is_refused,
    "{ let f = 'a: 'a |x: 'a| -> 'a x; f(1) }",
    refused("'a's bound names 'a: a bound may not reach its own variable");
    FuseExpect::None
);
run!(
    a_quantifier_bounded_under_a_constructor_is_refused,
    "{ let f = 'a: [i64, Array<'a>] |x: 'a| -> 'a x; f([]) }",
    refused("'a's bound names 'a: a bound may not reach its own variable");
    FuseExpect::None
);
run!(
    quantifiers_bounding_each_other_are_refused,
    "{ let f = 'a: 'b, 'b: 'a |x: 'a, y: 'b| -> 'a x; f(1, 2) }",
    refused("'a's bound reaches 'a through 'b");
    FuseExpect::None
);
run!(
    a_written_fn_type_bounded_by_itself_is_refused,
    "{ let g: fn<'a: Array<'a>>(x: 'a) -> 'a = |x| x; g([]) }",
    refused("'a's bound names 'a: a bound may not reach its own variable");
    FuseExpect::None
);
run!(
    an_impl_bounded_by_itself_is_refused,
    refused("'a's bound names 'a: a bound may not reach its own variable"),
    "/test.gx" => r#"
        trait Sh { val sh: fn(self) -> string };
        impl<'a: 'a> Sh for Array<'a> { let sh = |x| "arr" };
        let result = Sh::sh([1])
    "#
    ; FuseExpect::None
);
run!(
    a_recursive_bound_is_a_recursive_type,
    "{ type N = [i64, Array<N>]; let f = 'a: N |x: 'a| -> 'a x; (f([]), f([1, [2]])) }",
    prints("[[], [i64:1, [i64:2]]]")
);
run!(
    body_annotation_takes_the_definitions_variable,
    "{ let g = 'e: Number |x: 'e| -> 'e { let v: 'e = x; v }; g(2) }",
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2)))
);

// A OneNumber binding holding a numeric member beside an open one is
// refused at the definition that leaves it open: the open member may yet
// be another numeric type, which no bound can rule out.
run!(
    one_number_beside_an_open_member_refused,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y| f(select b { true => 1, false => y }); 0 }",
    refused("an open member of which may yet be another numeric type: annotate it");
    FuseExpect::None
);
// Two open members beside no numeric one are refused alike: each may yet
// be a numeric type, and two of them two different ones.
run!(
    one_number_two_open_members_refused,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y, z| f(select b { true => z, false => y }); g(false, 2.5, 1) }",
    refused("an open member of which may yet be another numeric type: annotate it");
    FuseExpect::None
);
run!(
    one_number_one_open_member_twice,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y| f(select b { true => y, false => y }); g(false, 2) }",
    |v: Result<&Value>| matches!(v, Ok(Value::I64(2)))
);
run!(
    one_number_beside_an_annotated_member,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y: [i64, null]| f(select b { true => 1, false => y }); \
       select g(true, null) { i64 as n => n, null => 0 } }",
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1)))
);

// A write through a field of a union keeps the value in its member: a
// `with` replacement fits every member's field, and a writable reference
// to a field needs the members' fields to be one type.
run!(
    union_field_with_fits_every_member,
    "{ type S = [{a: i64, b: string}, {a: string, b: i64}]; \
       let h = |s: S, k: i64| -> S {s with a: k}; 0 }",
    refused("string does not contain i64");
    FuseExpect::None
);
run!(
    union_field_place_needs_one_field_type,
    "{ type S = [{a: i64, b: string}, {a: string, b: i64}]; \
       let s: S = {a: \"y\", b: 9}; let r = &mut s.a; 0 }",
    refused("the members' fields differ");
    FuseExpect::None
);
run!(
    union_tuple_place_needs_one_field_type,
    "{ let t: [(i64, string), (string, i64)] = (\"y\", 9); let r = &mut t.0; 0 }",
    refused("the members' fields differ");
    FuseExpect::None
);
run!(
    union_field_write_where_members_agree,
    "{ type S = [{a: i64, b: string}, {a: i64, b: i64}]; \
       let s: S = {a: 1, b: 9}; let r = &mut s.a; *r <- 5; \
       let u = {s with a: 7}; \
       skip(#n: 1, select (s, u) { ({a, ..}, {a: b, ..}) => (s, a + b) }) }",
    prints(r#"[[["a", i64:5], ["b", i64:9]], i64:12]"#)
);

run!(
    default_reads_the_factorys_parameter,
    "{ let mk = |z| { let g = |#d = z, x| [d, x]; g }; let f = mk(1); \
       [f(2), f(#d: 5, 3)] }",
    refused("default reads an enclosing open type");
    FuseExpect::None
);
run!(
    default_factory_open_array,
    "{ let mk = |z| { let g = |#d: Array<'q> = [z], x| [d, [x]]; g }; \
       let f = mk(1); f(2) }",
    refused("default reads an enclosing open type"); FuseExpect::None
);
run!(
    default_factory_open_struct,
    "{ let mk = |z| { let g = |#d: {v: 'q} = {v: z}, x| (d, x); g }; \
       let f = mk(1); f(2) }",
    refused("default reads an enclosing open type"); FuseExpect::None
);
run!(
    default_factory_open_union,
    "{ let mk = |z| { let g = |#d: [null, 'q] = z, x| (d, x); g }; \
       let f = mk(1); f(2) }",
    refused("default reads an enclosing open type"); FuseExpect::None
);
run!(
    default_factory_closed_parameter,
    "{ let mk = |z| { let g = |#d: i64 = z, x| [d, x]; g }; let f = mk(1); \
       [f(2), f(#d: 5, 3)] }",
    prints("[[i64:1, i64:2], [i64:5, i64:3]]"); FuseExpect::None
);
run!(
    default_factory_closed_capture,
    "{ let mk = |z: i64| { let g = |#d = z, x| [d, x]; g }; let f = mk(1); \
       [f(2), f(#d: 5, 3)] }",
    prints("[[i64:1, i64:2], [i64:5, i64:3]]"); FuseExpect::None
);
run!(
    default_of_the_factorys_declared_variable,
    "{ let mk = |z: 'a| -> 'a { let g = |#d: 'a = z, y: i64| d; g(1) }; mk(3) }",
    is(Value::I64(3))
);
run!(
    default_of_the_factorys_declared_variable_at_a_string,
    "{ let mk = |z: 'a| -> 'a { let g = |#d: 'a = z, y: i64| d; g(1) }; mk(\"s\") }",
    is(Value::String("s".into()))
);
run!(
    default_of_an_array_of_the_factorys_declared_variable,
    "{ let mk = |z: 'a| -> Array<'a> { let g = |#d: Array<'a> = [z], y: i64| d; g(1) }; \
       (mk(3), mk(\"s\")) }",
    prints(r#"[[i64:3], ["s"]]"#)
);
run!(
    default_of_a_declared_variable_outside_the_factory,
    "{ let mk = |z: 'a| { let g = |#d: Array<'a> = [z], y: i64| (d, y); g }; \
       let f = mk(1); let h = mk(\"s\"); \
       let k: fn(y: i64) -> (Array<string>, i64) = h; \
       (f(2), h(3), f(#d: [7], 4), k(5)) }",
    prints(r#"[[[i64:1], i64:2], [["s"], i64:3], [[i64:7], i64:4], [["s"], i64:5]]"#);
    FuseExpect::None
);
run!(
    default_of_a_declared_variable_types_its_parameter,
    "{ let mk = |z: 'a| { let g = |#d = z, y: i64| (d, y); g(1) }; (mk(3), mk(\"s\")) }",
    prints(r#"[[i64:3, i64:1], ["s", i64:1]]"#)
);
run!(
    default_of_a_declared_variable_types_an_escaping_parameter,
    "{ let mk = |z: 'a| { let g = |#d = z, y: i64| (d, y); g }; \
       let f = mk(1); let h = mk(\"s\"); (f(2), h(3), f(#d: 7, 4)) }",
    prints(r#"[[i64:1, i64:2], ["s", i64:3], [i64:7, i64:4]]"#); FuseExpect::None
);
run!(
    default_of_a_declared_variable_is_the_instances,
    "{ let mk = |z: 'a| { let g = |#d = z, y: i64| (d, y); g }; \
       let f = mk(1); let r: (string, i64) = f(2); r }",
    refused("(string, i64) does not contain ('a: i64, i64)"); FuseExpect::None
);
run!(
    default_of_one_declared_variable_for_another,
    "{ let mk = |z: 'b, w: 'a| { let g = |#d: Array<'a> = [z], y: i64| d; (g, w) }; \
       let p = mk(\"s\", 1); p.0(2) }",
    refused("Array<'a: unbound> does not contain"); FuseExpect::None
);
// A type written in a default names the variables of the definitions
// around the defaulted one, a lambda's signature as a `let`'s annotation,
// at the definition's check and at each instance's bind.
run!(
    default_lambda_names_the_factorys_variable,
    "{ let mk = |z: 'a| -> 'a { let g = |#d: fn(q: i64) -> 'a = |q: i64| -> 'a z, y: i64| d(y); \
       g(1) }; \
       let mk2 = |z: 'a| -> 'a { let g = |#d: fn(q: i64) -> 'a = |#e: 'a = z, q: i64| e, y: i64| \
       d(y); g(1) }; \
       let mk3 = |z: 'a| -> 'a { let g = |#d: 'a = { let v: 'a = z; v }, y: i64| d; g(1) }; \
       (mk(1), mk(\"s\"), mk2(2), mk2(\"t\"), mk3(3), mk3(\"u\")) }",
    prints(r#"[i64:1, "s", i64:2, "t", i64:3, "u"]"#)
);
run!(
    default_lambda_names_the_factorys_variable_outside_it,
    "{ let mk = |z: 'a| { let g = |#d: fn(q: i64) -> 'a = |#e: 'a = z, q: i64| e, y: i64| d(y); \
       g }; \
       let f = mk(1); let h = mk(\"s\"); (f(2), h(3)) }",
    prints(r#"[i64:1, "s"]"#); FuseExpect::None
);
run!(
    default_lambda_names_the_factorys_variable_at_the_instance,
    "{ let mk = |z: 'a| { let g = |#d: fn(q: i64) -> 'a = |#e: 'a = z, q: i64| e, y: i64| d(y); \
       g }; \
       let f = mk(1); let r: string = f(2); r }",
    refused("string does not contain i64"); FuseExpect::None
);

const OPERAND_WITH_TWO_NUMERIC_MEMBERS: &str = r#"
{
  let x = never();
  x <- 1.5;
  let f = |i| array::map([0, x], |j| i + j);
  let r = array::map([2], f);
  1
}
"#;
run!(operand_with_two_numeric_members, OPERAND_WITH_TWO_NUMERIC_MEMBERS, refused("more than one numeric type"); FuseExpect::None);

// An alias applied twice is two types: `Id<i64>` checked does not vouch
// for `Id<&i64>`, as target or as source.
const CAST_TO_A_REFERENCE_THROUGH_AN_ALIAS: &str = r#"
{
  type Id<'a> = 'a;
  cast<(Id<i64>, Id<&i64>)>((1, u64:0))
}
"#;
run!(cast_to_a_reference_through_an_alias, CAST_TO_A_REFERENCE_THROUGH_AN_ALIAS, refused("can't cast a reference"); FuseExpect::None);

const CAST_OF_A_REFERENCE_THROUGH_AN_ALIAS: &str = r#"
{
  type Id<'a> = 'a;
  let v = 5;
  let t: (Id<i64>, Id<&i64>) = (1, &v);
  cast<(i64, u64)>(t)
}
"#;
run!(cast_of_a_reference_through_an_alias, CAST_OF_A_REFERENCE_THROUGH_AN_ALIAS, refused("can't cast a reference"); FuseExpect::None);

// An application decided once is decided for the walk: checking the
// parameter of Id<Id<&i64>> must not vouch for the whole.
const CAST_TO_A_REFERENCE_THROUGH_A_NESTED_ALIAS: &str = r#"
{
  type Id<'a> = 'a;
  cast<Id<Id<&i64>>>(u64:0)
}
"#;
run!(cast_to_a_reference_through_a_nested_alias, CAST_TO_A_REFERENCE_THROUGH_A_NESTED_ALIAS, refused("can't cast a reference"); FuseExpect::None);

const CAST_OF_A_REFERENCE_THROUGH_A_NESTED_ALIAS: &str = r#"
{
  type Id<'a> = 'a;
  let v = 5;
  let t: (i64, Id<Id<&i64>>) = (1, &v);
  cast<(i64, u64)>(t)
}
"#;
run!(cast_of_a_reference_through_a_nested_alias, CAST_OF_A_REFERENCE_THROUGH_A_NESTED_ALIAS, refused("can't cast a reference"); FuseExpect::None);

// A name expanded twice on one value with other parameters is no cycle.
run!(
    cast_to_a_nested_alias,
    is(Value::I64(42)),
    "/test.gx" => r#"
        type Id<'a> = 'a;
        let result = cast<Id<Id<i64>>>(u8:41)$ + 1
    "#
);

// A fused cast to a typedef its block declares: the block's typedef node
// goes with the region fusion replaces, and the kernel keeps the
// definitions its types name.
const CAST_TO_A_BLOCK_TYPEDEF: &str = r#"{ type T = i64; cast<T>(u8:41)$ + 1 }"#;
run!(cast_to_a_block_typedef, CAST_TO_A_BLOCK_TYPEDEF, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

const CAST_TO_A_BLOCK_NESTED_ALIAS: &str =
    r#"{ type Id<'a> = 'a; cast<Id<Id<i64>>>(u8:41)$ + 1 }"#;
run!(cast_to_a_block_nested_alias, CAST_TO_A_BLOCK_NESTED_ALIAS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// A definition whose parameters grow as it recurses is a cast target:
// every application of it holds what its parameters hold.
run!(
    cast_to_a_growing_definition,
    is(Value::Null),
    "/test.gx" => r#"
        type N<'a> = [null, ('a, N<Array<'a>>)];
        let result = cast<N<i64>>(null)$
    "#
; FuseExpect::None);

// A verdict read from an application still being decided is that
// application's assumption: A refuses, so B, which holds A, refuses too,
// though B was first reached inside A.
run!(
    cast_to_a_reference_through_a_cycle,
    refused("can't cast a reference"),
    "/test.gx" => r#"
        type Ph<'x> = [null, Array<Ph<'x>>];
        type A = (B, &i64);
        type B = Array<A>;
        let result = cast<(Ph<A>, B)>((null, [([], u64:5)]))
    "#
; FuseExpect::None);

run!(
    cast_of_a_reference_through_a_cycle,
    refused("can't cast a reference"),
    "/test.gx" => r#"
        type Ph<'x> = [null, Array<Ph<'x>>];
        type A = (B, &i64);
        type B = Array<A>;
        let v = 5;
        let s: (Ph<A>, B) = (null, [([], &v)]);
        let result = cast<(null, Array<(Array<i64>, u64)>)>(s)
    "#
; FuseExpect::None);

// `never<T>` writes a type name like an annotation does.
run!(
    never_type_names_are_defined,
    refused("undefined type Act"),
    "/test.gx" => r#"
        let result = { let x = 1; never<Act>() }
    "#
; FuseExpect::None);

// A type definition's body names something once its block is
// registered: a later sibling does, an undefined name does not.
run!(
    typedef_body_names_are_defined,
    refused("undefined type Act"),
    "/test.gx" => r#"
        type A = [`X(B), `N];
        type B = [`Y(Act), `M];
        let result = 0
    "#
; FuseExpect::None);

run!(
    typedef_body_names_a_later_sibling,
    is(Value::I64(0)),
    "/test.gx" => r#"
        let f = |x: i64| { type A = [`X(B), `N]; type B = i64; x };
        let result = f(0)
    "#
; FuseExpect::Jit);

// A body-local abstract type constructs in a call bound at run time.
run!(
    local_abstract_in_a_runtime_recursion,
    is(Value::I64(4)),
    "/test.gx" => r#"
        let f = |x: i64| {
            type T = Abstract<i64>;
            let rec g = |t: T, n: i64| select n {
                0 => t,
                n => g(T(t.0 + 1), n - 1)
            };
            select g(T(x), 3) { T(v) => v }
        };
        let result = f(1)
    "#
; FuseExpect::Jit);

// A recursive call inside a body that declares a type binds at run time,
// from the types its definition's check settled.
run!(
    local_typedef_in_a_runtime_recursion,
    is(Value::I64(6)),
    "/test.gx" => r#"
        let f = |x: i64| {
            type L = [`C(i64, L), `N];
            let rec g = |l: L, acc: i64| -> i64 select l {
                `N => acc,
                `C(x, rest) => g(rest, acc + x)
            };
            g(`C(2, `C(3, `N)), x)
        };
        let result = f(1)
    "#
; FuseExpect::Jit);

// A type may not take a trait's name in one scope, in either order.
run!(
    type_after_trait_of_one_name,
    refused("already defined"),
    "/test.gx" => r#"
        trait Foo { val show: fn(self) -> string };
        type Foo = i64;
        let result: Foo = 1
    "#
; FuseExpect::None);

// Every type name a definition writes names something once the check
// ends, however deep it sits and whether or not anything expands it; a
// name the interface imports from a later module is one.
run!(
    annotation_names_are_defined,
    |v: Result<&Value>| matches!(v, Err(e) if format!("{e:#}").contains("undefined type S")
        && !format!("{e:#}").contains("in the instance of")),
    "/test.gx" => r#"
        let f = |b: Array<i64>| array::map(b, |x| { let v: [S, i64] = never(); x });
        let result = f([1])
    "#
; FuseExpect::None);

run!(
    annotation_names_from_a_later_module,
    is(Value::I64(0)),
    "/test.gx" => r#"
mod s;
mod u;
let result = s::f([`A])
"#,
    "/test/s.gxi" => r#"
use super::u::Op;
val f: fn(o: Array<Op>) -> i64;
"#,
    "/test/s.gx" => r#"
let f = |o: Array<Op>| -> i64 0;
"#,
    "/test/u.gx" => r#"
type Op = [`A, `B];
"#
; FuseExpect::Jit);

// A select arm's scope is the same in every instance of its definition,
// so a type the arm defines names the same definition in the signature
// and in the instance.
const ARM_TYPEDEF_IN_THE_SIGNATURE: &str = r#"
{
  let f = |n: i64| select n { 0 => 0, _ => { type L = i64; let g = |l: L| -> L l; g } };
  let h = f(1);
  42
}
"#;

run!(arm_typedef_in_the_signature, ARM_TYPEDEF_IN_THE_SIGNATURE, is(Value::I64(42)); FuseExpect::None);

// A typedef's parameter constraint names types where the typedef is.
run!(
    typedef_constraint_resolves_in_its_module,
    is(Value::I64(1)),
    "/test.gx" => r#"
        mod inner;
        let result = inner::first
    "#,
    "/test/inner.gx" => r#"
        type N = i64;
        type T<'a: N> = Array<'a>;
        let x: T<i64> = [1];
        let first = x[0]$
    "#
; FuseExpect::Jit);

// A type parameter the definition never uses is refused.
run!(
    typedef_unused_parameter,
    refused("unused type parameter"),
    "/test.gx" => r#"
        type T<'a> = i64;
        let result: T<string> = 1
    "#
; FuseExpect::None);

// An array whose last element is empty converts element by element.
const CAST_ARRAY_TO_LIST: &str = r#"{
  let a: Array<Array<i64>> = [[1], []];
  cast<List<Array<i64>>>(a)$
}"#;
run!(cast_array_to_list, CAST_ARRAY_TO_LIST, prints("[[i64:1], []]"); FuseExpect::Jit);

const CAST_NESTED_ARRAY_TO_LIST: &str = r#"{
  let b: Array<Array<Array<i64>>> = [[[1]], [[2], []]];
  cast<List<Array<Array<i64>>>>(b)$
}"#;
run!(cast_nested_array_to_list, CAST_NESTED_ARRAY_TO_LIST, prints("[[[i64:1]], [[i64:2], []]]"); FuseExpect::Jit);

// A list statically so converts to an array element by element.
const CAST_LIST_TO_ARRAY: &str = r#"{
  let l = [<1, 2>];
  cast<Array<i64>>(l)$
}"#;
run!(cast_list_to_array, CAST_LIST_TO_ARRAY, prints("[i64:1, i64:2]"); FuseExpect::Jit);

// Each refused program would run a value its type excludes, call a
// function on the wrong type, or take no arm of an exhaustive select;
// each accepted twin shows what the rule still admits.
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
  let apply = |f: F| f(2);
  apply(|x| x)
}"#;
// `x + 1` is `'b + i64`, which holds only where 'b is i64.
const NESTED_QUANTIFIER_CONCRETE: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  let apply = |f: F| f(1);
  apply(|x| x + 1)
}"#;
const NESTED_QUANTIFIER_CONCRETE_INLINE: &str = r#"{
  let apply = |f: fn<'b: Number>(x: 'b) -> 'b| f(1);
  apply(|x| x + 1)
}"#;
// A parameter's declared 'a is rigid in the body: `f` takes whatever
// the caller's 'a is, never a string.
const RIGID_PARAM_CALL: &str = r#"{
  let m = |a: 'a, f: fn(x: 'a) -> 'b| -> 'b f("a");
  m(1, |x| x * 2)
}"#;
// Each call picks the formal's own quantifier anew.
const NESTED_QUANTIFIER_TWO_CALLS: &str = r#"{
  let apply = |f: fn<'b: Number>(x: 'b) -> 'b| f(1) + cast<i64>(f(2.5))$;
  apply(|x| x)
}"#;
// `apply` may call `f` at any number type, so an i64 function is not one.
const NESTED_QUANTIFIER_MONO: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  let apply = |f: F| f(1);
  let inc = |x: i64| x + 1;
  apply(inc)
}"#;
// The argument is checked with 'b rigid: well typed for every 'b.
const NESTED_QUANTIFIER_SQUARE: &str = r#"{
  type F = fn<'b: Number + Singleton>(x: 'b) -> 'b;
  let apply = |f: F| f(3);
  apply(|x| x * x)
}"#;
// A rank-2 quantifier keeps exactly its declared bounds: 'b: Number holds
// a union of numbers, which `x * x` can't take.
const NESTED_QUANTIFIER_SQUARE_UNDECLARED: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  let apply = |f: F| f(3);
  apply(|x| x * x)
}"#;
/// A type test can't tell one function signature from another, inside
/// a tuple too.
const TYPE_TEST_HOLDING_A_FN: &str = r#"{
    let fs = |x: string| "[x]!";
    let u: [(fn(x: i64) -> i64, i64), (fn(x: string) -> string, i64)] = (fs, 1);
    select u {
        (fn(x: i64) -> i64, i64) as (f, n) => f(n),
        (fn(x: string) -> string, i64) as (f, n) => 0
    }
}"#;

/// A Rust-backed abstract's values carry no parameters: two of its
/// instantiations share one runtime form.
const TYPE_TEST_RUST_BACKED_PARAMS: &str = r#"{
    let f = |x: [db::Tree<i64, string>, db::Tree<string, i64>]| select x {
        db::Tree<string, i64> as _ => 1,
        db::Tree<i64, string> as _ => 2
    };
    0
}"#;

/// An abstract pattern over a union predicate would be refutable by a
/// tag test nothing performs.
const ABSTRACT_PATTERN_OVER_A_UNION: &str = r#"{
    type Box = Abstract<i64>;
    let v: [Box, i64] = 1;
    select v { [Box, i64] as Box(x) => x }
}"#;

/// `never<T>()` holds T's applications to their parameter bounds.
const NEVER_OF_AN_UNBOUNDED_APPLICATION: &str = r#"{
    type Num<'a: Number> = 'a;
    never<Num<string>>()
}"#;

/// A callback whose type holds a top-level variable is not polymorphic:
/// aliasing the quantifier to it would carry it out.
const NESTED_QUANTIFIER_ENV_CELL: &str = r#"{
    let st = never();
    let g = |x| { st <- x; st };
    let h = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    let (a, b) = h(g);
    a * 2
}"#;

/// The same, through a lambda nested in the callback: the variable is
/// reached through a binding the callback reaches, not its own type.
const NESTED_QUANTIFIER_ENV_CELL_NESTED: &str = r#"{
    let st = never();
    let g = |x| { let k = |y| { st <- y; y }; k(x) };
    let h = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    let (a, b) = h(g);
    a * 2
}"#;

// A lambda literal argument whose body writes a variable outside it, directly,
// into a container or through a reference, or from a lambda nested in it, carries
// the quantifier out through what it writes.
const RANK2_ARG_WRITES_OUTER: &str = r#"{
    let apply = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    let r = never();
    let t = apply(|x| { r <- x; x });
    r + 1
}"#;

const RANK2_ARG_WRITES_OUTER_ARRAY: &str = r#"{
    let apply = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    let r = never();
    let t = apply(|x| { r <- [x]; x });
    r
}"#;

const RANK2_ARG_WRITES_THROUGH_A_REFERENCE: &str = r#"{
    let apply = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    let r = never();
    let p = &mut r;
    let t = apply(|x| { *p <- x; x });
    r
}"#;

const RANK2_ARG_NESTED_LAMBDA_WRITES_OUTER: &str = r#"{
    let apply = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    let r = never();
    let t = apply(|x| { let g = |y| { r <- y; y }; g(x) });
    r
}"#;

const RANK2_FIELD_WRITES_OUTER: &str = r#"{
    let use_it = |s: {f: fn<'b: Any>(x: 'b) -> 'b}| (s.f(1), s.f("s"));
    let r = never();
    let t = use_it({f: |x| { r <- x; x }});
    r
}"#;

// A call's argument made by a call carries the instance the call made: its
// signature types that instance's bindings, which outlive the argument.
const RANK2_ARG_OF_A_CALLS_INSTANCE: &str = r#"{
    let mk = |z| |x| { z <- x; z };
    let apply = |f: fn<'b: Any>(x: 'b) -> 'b| (f(1), f("s"));
    apply(mk(never()))
}"#;

// A written quantified function type holds its value wherever it stands as
// a formal holds its argument: a let annotation, a field, an element, a
// typedef, a write's target, a struct update's field, a return type and a
// labeled default are each refused a lambda that writes or returns a
// binding outside it, or a value a call's instance makes.
const RANK2_LET_WRITES_OUTER: &str = r#"{
    let y = never();
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| { y <- x; x };
    let k: fn(x: string) -> string = h;
    (k("s"), y + 1)
}"#;

const RANK2_LET_RETURNS_OUTER: &str = r#"{
    let n = never();
    let h: fn<'a: Any>(x: 'a) -> ('a, 'a) = |x| (x, n);
    n <- 3;
    h("s")
}"#;

const RANK2_LET_WRITES_A_PARAMETER: &str = r#"{
    let f = |n| {
        let h: fn<'a: Any>(x: 'a) -> 'a = |x| { n <- x; x };
        (h(1), h("s"))
    };
    f(1)
}"#;

const RANK2_LET_RETURNS_A_PARAMETER: &str = r#"{
    let f = |n| {
        let h: fn<'a: Any>(x: 'a) -> ('a, 'a) = |x| (x, n);
        h(1)
    };
    f(1)
}"#;

const RANK2_LET_FIELD_WRITES_OUTER: &str = r#"{
    let y = never();
    let s: {f: fn<'a: Any>(x: 'a) -> 'a} = {f: |x| { y <- x; x }};
    (s.f(1), s.f("s"), y + 1)
}"#;

const RANK2_LET_ELEMENT_WRITES_OUTER: &str = r#"{
    let y = never();
    let fs: Array<fn<'a: Any>(x: 'a) -> 'a> = [|x| { y <- x; x }];
    (fs[0]$(1), fs[0]$("s"), y + 1)
}"#;

const RANK2_LET_TYPEDEF_WRITES_OUTER: &str = r#"{
    type T = fn<'a: Any>(x: 'a) -> 'a;
    let y = never();
    let h: T = |x| { y <- x; x };
    (h(1), h("s"), y + 1)
}"#;

const RANK2_CONNECT_WRITES_OUTER: &str = r#"{
    let y = never();
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| x;
    h <- |x| { y <- x; x };
    (h(1), h("s"), y + 1)
}"#;

const RANK2_DEREF_WRITE_WRITES_OUTER: &str = r#"{
    let y = never();
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| x;
    let r = &mut h;
    *r <- |x| { y <- x; x };
    (h(1), h("s"), y + 1)
}"#;

const RANK2_UPDATE_WRITES_OUTER: &str = r#"{
    let y = never();
    let s: {f: fn<'a: Any>(x: 'a) -> 'a, n: i64} = {f: |x| x, n: 1};
    let t = {s with f: |x| { y <- x; x }};
    (t.f(1), t.f("s"), y + 1)
}"#;

const RANK2_RETURN_TYPE_WRITES_OUTER: &str = r#"{
    let y = never();
    let mk = |z: i64| -> fn<'a: Any>(x: 'a) -> 'a |x| { y <- x; x };
    let h = mk(0);
    (h(1), h("s"), y + 1)
}"#;

const RANK2_RETURN_FIELD_WRITES_OUTER: &str = r#"{
    let y = never();
    let mk = |z: i64| -> {f: fn<'a: Any>(x: 'a) -> 'a} {f: |x| { y <- x; x }};
    let s = mk(0);
    (s.f(1), s.f("s"), y + 1)
}"#;

const RANK2_DEFAULT_WRITES_OUTER: &str = r#"{
    let y = never();
    let g = |#f: fn<'a: Any>(x: 'a) -> 'a = |x| { y <- x; x }, z: i64| (f(z), f("s"));
    (g(1), y + 1)
}"#;

const RANK2_LET_OF_A_CALLS_INSTANCE: &str = r#"{
    let mk = |z| |x| { z <- x; z };
    let h: fn<'a: Any>(x: 'a) -> 'a = mk(never());
    (h(1), h("s"))
}"#;

// A quantified function type in a function type's result is held the same
// way, at a let, a call's formal, a labeled default, a written return type
// and a struct field, under a quantified outer type and through a struct
// result; and so is an abstract type's payload at its constructor.
const RANK2_RESULT_LET_WRITES_OUTER: &str = r#"{
    let y = never();
    let mk: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a = |z| |x| { y <- x; x };
    let h = mk(0);
    (h(1), h("s"), y + 1)
}"#;

const RANK2_RESULT_ARG_WRITES_OUTER: &str = r#"{
    let y = never();
    let app = |m: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a| { let h = m(0); (h(1), h("s")) };
    (app(|z| |x| { y <- x; x }), y + 1)
}"#;

const RANK2_RESULT_DEFAULT_WRITES_OUTER: &str = r#"{
    let y = never();
    let g = |#m: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a = |z| |x| { y <- x; x }, q: i64| {
        let h = m(q);
        (h(1), h("s"))
    };
    (g(0), y + 1)
}"#;

const RANK2_RESULT_RETURN_TYPE_WRITES_OUTER: &str = r#"{
    let y = never();
    let mk = |q: i64| -> fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a |z| |x| { y <- x; x };
    let h = mk(0)(1);
    (h(1), h("s"), y + 1)
}"#;

const RANK2_RESULT_FIELD_WRITES_OUTER: &str = r#"{
    let y = never();
    let s: {mk: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a} = {mk: |z| |x| { y <- x; x }};
    let h = s.mk(0);
    (h(1), h("s"), y + 1)
}"#;

const RANK2_RESULT_OF_A_QUANTIFIED_WRITES_OUTER: &str = r#"{
    let y = never();
    let mk: fn<'q: Any>(z: 'q) -> fn<'a: Any>(x: 'a) -> 'a = |z| |x| { y <- x; x };
    let h = mk(0);
    (h(1), h("s"), y + 1)
}"#;

const RANK2_RESULT_STRUCT_WRITES_OUTER: &str = r#"{
    let y = never();
    let mk: fn(z: i64) -> {f: fn<'a: Any>(x: 'a) -> 'a} = |z| {f: |x| { y <- x; x }};
    let h = mk(0).f;
    (h(1), h("s"), y + 1)
}"#;

const RANK2_CONSTRUCT_WRITES_OUTER: &str = r#"{
    type W = Abstract<fn<'a: Any>(x: 'a) -> 'a>;
    let y = never();
    let w = W(|x| { y <- x; x });
    let h = w.0;
    (h(1), h("s"), y + 1)
}"#;

const RANK2_CONSTRUCT_PARAMETRIC_WRITES_OUTER: &str = r#"{
    type W<'t> = Abstract<('t, fn<'a: Any>(x: 'a) -> 'a)>;
    let y = never();
    let w = W((1, |x| { y <- x; x }));
    let h = w.0.1;
    (h(1), h("s"), y + 1)
}"#;

// An abstract type applied to a quantified function type holds its payload
// as the function type would: at a let, a field, a written return type,
// core's `Transparent` included.
const RANK2_ABSTRACT_PARAMETER_WRITES_OUTER: &str = r#"{
    type W<'t> = Abstract<'t>;
    let y = never();
    let w: W<fn<'a: Any>(x: 'a) -> 'a> = W(|x| { y <- x; x });
    let h = w.0;
    (h(1), h("s"), y + 1)
}"#;

const RANK2_ABSTRACT_PARAMETER_FIELD_WRITES_OUTER: &str = r#"{
    type W<'t> = Abstract<'t>;
    let y = never();
    let s: {w: W<fn<'a: Any>(x: 'a) -> 'a>} = {w: W(|x| { y <- x; x })};
    let h = s.w.0;
    (h(1), h("s"), y + 1)
}"#;

const RANK2_ABSTRACT_PARAMETER_RETURN_WRITES_OUTER: &str = r#"{
    type W<'t> = Abstract<'t>;
    let y = never();
    let mk = |q: i64| -> W<fn<'a: Any>(x: 'a) -> 'a> W(|x| { y <- x; x });
    let h = mk(0).0;
    (h(1), h("s"), y + 1)
}"#;

const RANK2_TRANSPARENT_WRITES_OUTER: &str = r#"{
    let y = never();
    let t: Transparent<fn<'a: Any>(x: 'a) -> 'a> = Transparent(|x| { y <- x; x });
    let h = t.0;
    (h(1), h("s"), y + 1)
}"#;

// A constructor's payload bounds its parameter's cell from below, so a
// write of `W(x)` carries the quantifier out as a write of `x` does.
const RANK2_LET_WRITES_OUTER_CONSTRUCTED: &str = r#"{
    type W<'t> = Abstract<'t>;
    let y = never();
    let f: fn<'a: Any>(x: 'a) -> 'a = |x| { y <- W(x); x };
    (f(1), f("s"), y.0 + 1)
}"#;

const RANK2_FIELD_WRITES_OUTER_CONSTRUCTED: &str = r#"{
    type W<'t> = Abstract<'t>;
    let y = never();
    let s: {f: fn<'a: Any>(x: 'a) -> 'a} = {f: |x| { y <- W(x); x }};
    (s.f(1), s.f("s"), y.0 + 1)
}"#;

const RANK2_RETURN_TYPE_WRITES_OUTER_CONSTRUCTED: &str = r#"{
    type W<'t> = Abstract<'t>;
    let y = never();
    let mk = |z: i64| -> fn<'a: Any>(x: 'a) -> 'a |x| { y <- W(x); x };
    let h = mk(0);
    (h(1), h("s"), y.0 + 1)
}"#;

const RANK2_LET_WRITES_OUTER_TRANSPARENT: &str = r#"{
    let y = never();
    let f: fn<'a: Any>(x: 'a) -> 'a = |x| { y <- Transparent(x); x };
    (f(1), f("s"), y.0 + 1)
}"#;

// A holder of the function itself binds its quantifier only inside the
// function's type: the bare quantifier beside it is carried out.
const RANK2_LET_REC_WRITES_ITSELF_BESIDE_ITS_QUANTIFIER: &str = r#"{
    type W<'t> = Abstract<'t>;
    let u = never();
    let rec f: fn<'a: Any>(x: 'a) -> 'a = |x| { u <- (W(f), x); x };
    (f(1), f("s"), u.1 + 1)
}"#;

const RANK2_LET_REC_CONSTRUCTS_ITSELF_BESIDE_ITS_QUANTIFIER: &str = r#"{
    type P<'s, 't> = Abstract<('s, 't)>;
    let z = never();
    let rec f: fn<'a: Any>(x: 'a) -> 'a = |x| { z <- P((f, x)); x };
    (f(1), f("s"), z.0.1 + 1)
}"#;

const RANK2_CONNECT_WRITES_A_HOLDER_BESIDE_ITS_QUANTIFIER: &str = r#"{
    type W<'t> = Abstract<'t>;
    let f: fn<'a: Any>(x: 'a) -> 'a = |x| x;
    let y = W(f);
    let u = never();
    f <- |x| { u <- (y, x); x };
    (f(1), f("s"), u.1 + 1)
}"#;

const RANK2_CONNECT_TRANSPARENT_ITSELF_BESIDE_ITS_QUANTIFIER: &str = r#"{
    let f: fn<'a: Any>(x: 'a) -> 'a = |x| x;
    let z = never();
    f <- |x| { z <- Transparent({g: f, v: x}); x };
    (f(1), f("s"), z.0.v + 1)
}"#;

// A callee's bound ties the quantifier to an outer cell's constraint:
// carried out as a binding carries it, at a let, a call's formal and
// through a reference parameter.
const RANK2_LET_CONSTRAINS_OUTER: &str = r#"{
    let y = never();
    let g = 'b: 'c, 'c: Any |p: 'b, q: 'c| q;
    let f: fn<'a: Any>(x: 'a) -> 'a = |x| { let r = g(y, x); x };
    let t = f("s");
    y <- 5;
    (f(1), t, y + 1)
}"#;

const RANK2_ARG_CONSTRAINS_OUTER: &str = r#"{
    let y = never();
    let g = 'b: 'c, 'c: Any |p: 'b, q: 'c| q;
    let app = |k: fn<'a: Any>(x: 'a) -> 'a| (k(1), k("s"));
    let r = app(|x| { let r = g(y, x); x });
    y <- 5;
    (r, y + 1)
}"#;

const RANK2_LET_CONSTRAINS_OUTER_BY_REFERENCE: &str = r#"{
    let y = never();
    let g = 'b: 'c, 'c: Any |p: &'b, q: 'c| q;
    let f: fn<'a: Any>(x: 'a) -> 'a = |x| { let r = g(&y, x); x };
    let t = f("s");
    y <- 5;
    (f(1), t, y + 1)
}"#;

// A call's variables are one type for the call: an argument held by a
// rank-2 formal may not put the formal's quantifier into one, through the
// call's result, another formal or the formal itself, written or omitted.
const RANK2_CALL_SIGNATURE_RESULT: &str = r#"{
    type W<'t> = Abstract<'t>;
    let outer = |#k: fn<'a: Any>(x: 'a) -> W<'c>, n: i64| (k(n), k("s"));
    let r = outer(#k: |x| W(x), 1);
    let w: W<i64> = r.1;
    w.0 + 1
}"#;

const RANK2_CALL_SIGNATURE_PARAMETER: &str = r#"{
    type W<'t> = Abstract<'t>;
    let outer = |#k: fn<'a: Any>(x: 'a) -> W<'c>, z: W<'c>| (k(1), k("s"), z);
    let y = never();
    let r = outer(#k: |x| W(x), y);
    y <- W(1);
    let w: W<i64> = r.1;
    w.0 + 1
}"#;

const RANK2_CALL_SIGNATURE_FORMAL_ONLY: &str = r#"{
    type W<'t> = Abstract<'t>;
    let outer = |#k: fn<'a: Any>(x: 'a) -> W<'c>, n: i64| {
        let a = k(n);
        let b = k("s");
        n
    };
    outer(#k: |x| W(x), 1)
}"#;

const RANK2_CALL_SIGNATURE_BARE: &str = r#"{
    let outer = |#k: fn<'a: Any>(x: 'a) -> 'c, n: i64| (k(n), k("s"));
    let r = outer(#k: |x| x, 1);
    let q: (i64, i64) = r;
    q.1 + 1
}"#;

const RANK2_CALL_OMITTED_DEFAULT: &str = r#"{
    type W<'t> = Abstract<'t>;
    let outer = |#k: fn<'a: Any>(x: 'a) -> W<'c> = |x| W(x), n: i64| (k(n), k("s"));
    let r = outer(1);
    let w: W<i64> = r.1;
    w.0 + 1
}"#;

const RANK2_CALL_OMITTED_DEFAULT_FIELD: &str = r#"{
    type W<'t> = Abstract<'t>;
    let outer = |#k: {f: fn<'a: Any>(x: 'a) -> W<'c>} = {f: |x| W(x)}, z: W<'c>|
        (k.f(1), k.f("s"), z);
    let r = outer(W(1));
    let w: W<i64> = r.1;
    w.0 + 1
}"#;

const RANK2_CALL_HIDDEN_DEFAULT: &str = r#"{
    type W<'t> = Abstract<'t>;
    let f = |#k: fn<'a: Any>(x: 'a) -> W<'c> = |x| W(x), n: i64| (k(n), k("s"));
    let app = |g: fn(n: i64) -> 'r| g(1);
    let r = app(f);
    let w: W<i64> = r.1;
    w.0 + 1
}"#;

// A definition's declared variable a callee's bound ties to an outer
// cell's constraint escapes it as a binding would.
const DECLARED_CONSTRAINS_OUTER: &str = r#"{
    let y = never();
    let g = 'b: 'c, 'c: Any |p: 'b, q: 'c| q;
    let f = 'a: Any |x: 'a| -> 'a { let r = g(y, x); x };
    let t = f("s");
    y <- 5;
    (f(1), t, y + 1)
}"#;

// A rigid variable holds only itself: an open cell a merge would put
// below it is it. So a payload that bounds an enclosing definition's
// written or declared variable makes the lambda's parameter that
// variable, which no rank-2 quantifier holds and no call may choose.
const RIGID_BELOW_LET: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| { let q: W<'c> = W(x); x };
    (h(1), h("s"), z)
  };
  outer(W(1))
}"#;

const RIGID_BELOW_DECLARED: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = 'c: Any |z: W<'c>| {
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| { let q: W<'c> = W(x); x };
    (h(1), h("s"), z)
  };
  outer(W(1))
}"#;

const RIGID_BELOW_TRANSPARENT: &str = r#"{
  let outer = |z: Transparent<'c>| {
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| { let q: Transparent<'c> = Transparent(x); x };
    (h(1), h("s"), z)
  };
  outer(Transparent(1))
}"#;

const RIGID_BELOW_DEFAULT: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let run = |#k: fn<'a: Any>(x: 'a) -> 'a = |x| { let q: W<'c> = W(x); x }, n: i64|
      (k(n), k("s"));
    (run(1), z)
  };
  outer(W(1))
}"#;

const RIGID_BELOW_FIELD: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let s: {h: fn<'a: Any>(x: 'a) -> 'a} = {h: |x| { let q: W<'c> = W(x); x }};
    let h = s.h;
    (h(1), h("s"), z)
  };
  outer(W(1))
}"#;

const RIGID_BELOW_CONNECT: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let h: fn<'a: Any>(x: 'a) -> 'a = |x| x;
    h <- |x| { let q: W<'c> = W(x); x };
    (h(1), h("s"), z)
  };
  outer(W(1))
}"#;

const RIGID_BELOW_UNANNOTATED: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let inner = |v| { let q: W<'c> = W(v); v };
    (inner(1), inner("s"), z)
  };
  outer(W(true))
}"#;

// Through an alias of the quantified type, the same.
const NESTED_QUANTIFIER_ALIAS_MONO: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  type G = F;
  let apply = |f: G| f(1.5);
  let inc = |x: i64| -> i64 x + 1;
  apply(inc)
}"#;
const NESTED_QUANTIFIER_ALIAS_CONCRETE: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
  type G = F;
  let apply = |f: G| f(1.5);
  apply(|x| x + 1)
}"#;
const NESTED_QUANTIFIER_COMPARE: &str = r#"{
  type G = fn<'b: Number + Ordered>(x: 'b, y: 'b) -> bool;
  let apply = |f: G| select f(1, 2) { true => 1, false => 0 };
  apply(|x, y| x < y)
}"#;
// `Number` admits a type holding every numeric type, which has no order
const NESTED_QUANTIFIER_COMPARE_UNORDERED: &str = r#"{
  type G = fn<'b: Number>(x: 'b, y: 'b) -> bool;
  let apply = |f: G| select f(1, 2) { true => 1, false => 0 };
  apply(|x, y| x < y)
}"#;
const NESTED_QUANTIFIER_TWO_TYPES: &str = r#"{
  type F = fn<'b: Number + Singleton>(x: 'b) -> 'b;
  let apply = |f: F| f(3) + cast<i64>(f(2.5))$;
  let sq = |x| x * x;
  apply(sq)
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

// A reference type test over `Any` would take any number for a writable
// reference.
const REF_TEST_OVER_ANY: &str = r#"{
  let n = u64:42;
  let b: Any = n;
  select b { &mut string as q => { *q <- "boom"; 1 }, _ => 7 }
}"#;
const REF_TEST_OVER_ANY_NESTED: &str = r#"{
  let b: Any = u64:42;
  select (b, 0) { (&mut string, i64) as (q, _) => { *q <- "boom"; 1 }, _ => 7 }
}"#;

// An abstract's parameters are a cast target's as much as its other parts.
const CAST_TO_AN_ABSTRACT_OF_A_FUNCTION: &str = r#"{
  type Box<'a> = Abstract<'a>;
  impl<'a> Cast for Box<'a> {};
  let a: Any = Box(|x: i64| x + 1);
  let b = cast<Box<fn(x: string) -> string>>(a)$;
  str::len(b.0("hello"))
}"#;

// A declared variable is held rigid by every probe a containment decides
// with, a cell's conjuncts included.
const CONJUNCT_RIGID: &str = r#"{
  let g = 'c: Any, 'd: ['c, null] |x: 'c, y: 'd| -> ['c, null] y;
  let f = 'a: Any |x: 'a| -> ['a, null] g(x, 5);
  select f("s") { null => 0, s => str::len(s) }
}"#;
const CONJUNCT_RIGID_TRAIT: &str = r#"{
  trait Sz { val sz: fn(self) -> i64 };
  impl Sz for i64 { let sz = |x| x };
  let g = 'c: Sz |x: 'c| -> i64 Sz::sz(x);
  let f = 'a: Any |x: 'a, b: bool| -> i64 {
    let v: [i64, 'a] = select b { true => 1, false => x };
    g(v)
  };
  f("s", false)
}"#;

// A mismatch at a growing definition's first level is a mismatch, not a
// comparison that cannot end.
const GROWING_MISMATCH: &str = r#"{
  type N<'a> = [null, ('a, N<Array<'a>>)];
  let x: N<i64> = null;
  let y: N<f64> = x;
  0
}"#;

// Error is a one-argument product head: a union of errors holds the error
// its members merge to.
const ERROR_UNION_DISTRIBUTES: &str = r#"{
  let b = true;
  let e: [Error<i64>, Error<string>] = select b { true => error(1), false => error("x") };
  select e { error as _ => 1 }
}"#;

// A typedef parameter's bound reaches the open members of a union
// argument, as it reaches a bare cell: a result's success part holds no
// error, and its error arm tells it from the error.
const NO_ERROR_OPEN_MEMBER: &str = r#"{
  let r: Result<[i64, 'x], `E> = error(`Q);
  0
}"#;
const NO_ERROR_OPEN_MEMBER_TOLD: &str = r#"{
  let f = |a: 'x| -> i64 { let r: Result<[i64, 'x], `E> = a; select r { error as e => 0, v => 1 } };
  f(1)
}"#;

// A read-only reference test takes a writable reference to its type: the
// arm leaves nothing of it behind.
const SHARED_REF_TEST_OVER_MUT: &str = r#"{
  let x = 1;
  let o: [&mut i64, null] = &mut x;
  select o { &i64 as r => *r, null => 0 }
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
        (NESTED_QUANTIFIER_CONCRETE, "cannot compute"),
        (NESTED_QUANTIFIER_CONCRETE_INLINE, "cannot compute"),
        (NESTED_QUANTIFIER_MONO, "does not contain"),
        (
            NESTED_QUANTIFIER_SQUARE_UNDECLARED,
            "which the parameter's quantifier does not declare",
        ),
        (
            NESTED_QUANTIFIER_COMPARE_UNORDERED,
            "requires 'b: Ordered, which the parameter's quantifier does not declare",
        ),
        (NESTED_QUANTIFIER_ALIAS_MONO, "does not contain"),
        (NESTED_QUANTIFIER_ALIAS_CONCRETE, "cannot compute"),
        (NESTED_QUANTIFIER_ENV_CELL, "not polymorphic"),
        (NESTED_QUANTIFIER_ENV_CELL_NESTED, "not polymorphic"),
        (RANK2_ARG_WRITES_OUTER, "not polymorphic in 'b"),
        (RANK2_ARG_WRITES_OUTER_ARRAY, "not polymorphic in 'b"),
        (RANK2_ARG_WRITES_THROUGH_A_REFERENCE, "not polymorphic in 'b"),
        (RANK2_ARG_NESTED_LAMBDA_WRITES_OUTER, "not polymorphic in 'b"),
        (RANK2_FIELD_WRITES_OUTER, "not polymorphic in 'b"),
        (RANK2_ARG_OF_A_CALLS_INSTANCE, "the instance a call in it makes"),
        (RANK2_LET_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_LET_RETURNS_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_LET_WRITES_A_PARAMETER, "the value is not polymorphic in 'a"),
        (RANK2_LET_RETURNS_A_PARAMETER, "the value is not polymorphic in 'a"),
        (RANK2_LET_FIELD_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_LET_ELEMENT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_LET_TYPEDEF_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_CONNECT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_DEREF_WRITE_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_UPDATE_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RETURN_TYPE_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RETURN_FIELD_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_DEFAULT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_LET_OF_A_CALLS_INSTANCE, "the instance a call in it makes"),
        (RANK2_RESULT_LET_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RESULT_ARG_WRITES_OUTER, "the argument is not polymorphic in 'a"),
        (RANK2_RESULT_DEFAULT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RESULT_RETURN_TYPE_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RESULT_FIELD_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RESULT_OF_A_QUANTIFIED_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_RESULT_STRUCT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_CONSTRUCT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_CONSTRUCT_PARAMETRIC_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_ABSTRACT_PARAMETER_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (
            RANK2_ABSTRACT_PARAMETER_FIELD_WRITES_OUTER,
            "the value is not polymorphic in 'a",
        ),
        (
            RANK2_ABSTRACT_PARAMETER_RETURN_WRITES_OUTER,
            "the value is not polymorphic in 'a",
        ),
        (RANK2_TRANSPARENT_WRITES_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_LET_WRITES_OUTER_CONSTRUCTED, "is 'a, would carry 'a out"),
        (RANK2_FIELD_WRITES_OUTER_CONSTRUCTED, "the value is not polymorphic in 'a"),
        (
            RANK2_RETURN_TYPE_WRITES_OUTER_CONSTRUCTED,
            "the value is not polymorphic in 'a",
        ),
        (RANK2_LET_WRITES_OUTER_TRANSPARENT, "is 'a, would carry 'a out"),
        (
            RANK2_LET_REC_WRITES_ITSELF_BESIDE_ITS_QUANTIFIER,
            "the value is not polymorphic in 'a",
        ),
        (
            RANK2_LET_REC_CONSTRUCTS_ITSELF_BESIDE_ITS_QUANTIFIER,
            "the value is not polymorphic in 'a",
        ),
        (
            RANK2_CONNECT_WRITES_A_HOLDER_BESIDE_ITS_QUANTIFIER,
            "the value is not polymorphic in 'a",
        ),
        (
            RANK2_CONNECT_TRANSPARENT_ITSELF_BESIDE_ITS_QUANTIFIER,
            "the value is not polymorphic in 'a",
        ),
        (RANK2_LET_CONSTRAINS_OUTER, "the value is not polymorphic in 'a"),
        (RANK2_ARG_CONSTRAINS_OUTER, "the argument is not polymorphic in 'a"),
        (RANK2_LET_CONSTRAINS_OUTER_BY_REFERENCE, "the value is not polymorphic in 'a"),
        (RANK2_CALL_SIGNATURE_RESULT, "not polymorphic in 'a: the call's signature"),
        (RANK2_CALL_SIGNATURE_PARAMETER, "not polymorphic in 'a: the call's signature"),
        (RANK2_CALL_SIGNATURE_FORMAL_ONLY, "not polymorphic in 'a: the call's signature"),
        (RANK2_CALL_SIGNATURE_BARE, "not polymorphic in 'a: the call's signature"),
        (RANK2_CALL_OMITTED_DEFAULT, "not polymorphic in 'a: the call's signature"),
        (RANK2_CALL_OMITTED_DEFAULT_FIELD, "not polymorphic in 'a: the call's signature"),
        (RANK2_CALL_HIDDEN_DEFAULT, "not polymorphic in 'a: the call's signature"),
        (DECLARED_CONSTRAINS_OUTER, "'a escapes the definition"),
        (RIGID_BELOW_LET, "does not contain fn(x:"),
        (RIGID_BELOW_DECLARED, "does not contain fn(x:"),
        (RIGID_BELOW_TRANSPARENT, "does not contain fn(x:"),
        (RIGID_BELOW_DEFAULT, "does not contain fn(x:"),
        (RIGID_BELOW_FIELD, "does not contain { h: fn(x:"),
        (RIGID_BELOW_CONNECT, "cannot hold fn(x:"),
        (RIGID_BELOW_UNANNOTATED, "does not contain i64"),
        (NEVER_OF_AN_UNBOUNDED_APPLICATION, "Number does not contain string"),
        (TYPE_TEST_HOLDING_A_FN, "can't match on a type holding a function"),
        (TYPE_TEST_RUST_BACKED_PARAMS, "same runtime form"),
        (REF_TEST_OVER_ANY, "can't tell what the reference in &mut string refers to"),
        (REF_TEST_OVER_ANY_NESTED, "can't tell what the reference"),
        (CAST_TO_AN_ABSTRACT_OF_A_FUNCTION, "can't cast a value to a function"),
        (CONJUNCT_RIGID, "does not contain i64"),
        (CONJUNCT_RIGID_TRAIT, "does not contain [i64, 'a"),
        (GROWING_MISMATCH, "does not contain"),
        (NO_ERROR_OPEN_MEMBER, "does not contain Error<"),
        (ABSTRACT_PATTERN_OVER_A_UNION, "patterns can't match"),
        (RIGID_PARAM_CALL, "does not contain"),
        (TRAIT_BESIDE_UNSATISFIABLE, "unsatisfiable constraints"),
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
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
        (NESTED_QUANTIFIER_SQUARE, 9),
        (NESTED_QUANTIFIER_COMPARE, 1),
        (NESTED_QUANTIFIER_TWO_TYPES, 15),
        (NESTED_QUANTIFIER_TWO_CALLS, 3),
        (TRAIT_BESIDE_WITNESS, 0),
        (ERROR_UNION_DISTRIBUTES, 1),
        (NO_ERROR_OPEN_MEMBER_TOLD, 1),
        (SHARED_REF_TEST_OVER_MUT, 1),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(expected), "{src}");
    }
    Ok(())
}

// A minted cell is named short in the "must be fully known" refusal, as
// in every other diagnostic.
#[tokio::test(flavor = "current_thread")]
async fn not_concrete_names_a_minted_cell_short() -> Result<()> {
    let src = r#"{
  let rd = |s: string| -> 'a json::read(s)$;
  let g = |s| { let v = rd(s); [v] };
  g("1")
}"#;
    let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
        Err(msg) => msg,
        Ok(v) => panic!("must be refused: {v:?}"),
    };
    assert!(
        msg.contains("the type '_")
            && msg.contains("must be fully known")
            && cells_print_short(&msg),
        "{msg}"
    );
    Ok(())
}

/// Every cell `msg` names prints by its short name, `'_` and at most nine
/// digits, never the cell's full generated id.
fn cells_print_short(msg: &str) -> bool {
    msg.split("'_").skip(1).all(|named| {
        (1..10).contains(&named.chars().take_while(char::is_ascii_digit).count())
    })
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
    for (src, header) in [
        (PRINTED_BOUND, "fn<'a: Number + Singleton>("),
        (PRINTED_BOUNDS, "fn<'a: Eq + Ord + Ordered>("),
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(header), "no {header} in: {msg}");
        assert!(!msg.contains("Number & Number"), "a doubled bound in: {msg}");
    }
    Ok(())
}

// Comparison is `fn('a, 'a) -> bool` over exactly one type: neither
// operand's type may merely contain the other's.
#[tokio::test(flavor = "current_thread")]
async fn comparison_operands_are_one_type() -> Result<()> {
    for src in [
        "{ let x: [i64, null] = 3; x == 3 }",
        "{ let v: [`Green, `Red] = `Red; v == `Red }",
        "{ let r: [string, Error<`E>] = \"a\"; r != \"a\" }",
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains("cannot compare"), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

const COMPARISON_ONE_TYPE: &str = r#"{
  let s: [`Menu, `Edit] = `Menu;
  let t: [`Menu, `Edit] = `Edit;
  let f = 'a: Number |a: 'a, b: 'a| -> bool a < b;
  let g = |a, b| a == b;
  [s == t, s != t, f(3, 4), f(2.5, 1.5), g("a", "a"), g(s, t)]
}"#;

run!(comparison_one_type, COMPARISON_ONE_TYPE, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        &**a == &[
            Value::Bool(false),
            Value::Bool(true),
            Value::Bool(true),
            Value::Bool(false),
            Value::Bool(true),
            Value::Bool(false),
        ]
    }
    _ => false,
});

// A variable a nested lambda's signature writes, which a definition around
// it declares, is that definition's, unless the nested lambda declares it
// itself.
run!(a_nested_signature_names_the_enclosing_variable, r#"{
  let mk = 'a: Number + Singleton |k: 'a| |x: 'a| x + k;
  let own = 'a: Number |k: 'a| 'a: Any |x: 'a| [x];
  (mk(1)(2), own(1)("s"))
}"#, prints(r#"[i64:3, ["s"]]"#); FuseExpect::None);

run!(
    a_nested_signature_takes_no_other_type,
    "{ let mk = 'a: Number |k: 'a| |x: 'a| [x, k]; let g = mk(1); g(\"s\") }",
    refused("does not contain string");
    FuseExpect::None
);

// The definitions around a literal are those its text sits in, however
// deep, whichever compile meets it.
run!(
    a_signature_two_lambdas_down_names_the_enclosing_variable,
    "{ let mk = 'a: Number |k: 'a| |y: i64| |x: 'a| [x, k]; mk(1)(0)(\"s\") }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_nested_body_annotation_names_the_enclosing_variable,
    "{ let f = 'a: Number |x: 'a| -> 'a { let g = |y| { let v: 'a = 1; y }; g(x) }; \
       f(f32:1.5) }",
    refused("does not contain i64");
    FuseExpect::None
);

// A nested lambda's own quantifiers, the ones a trait-typed parameter or a
// constructor bound derives, are its own whatever their names.
run!(a_nested_lambdas_derived_quantifiers_are_its_own, r#"{
  use Collection::*;
  let f = |x: Display| { let g = |x: Display| x; g(1) };
  let c = 'c: Collection |xs: 'c| -> i64 {
    let g = 'c: Collection |ys: 'c| -> i64 fold(ys, 0, |a, _| a + 1);
    g([<"a", "b">]) + fold(xs, 0, |a, _| a + 1)
  };
  (f("s"), c([1, 2, 3]))
}"#, prints("[i64:1, i64:5]"); FuseExpect::Jit);

// A variable an abstract type, a type application or a reference holds is
// fixed exactly there, so that argument is checked first, whatever the
// order the call writes: "aa" fits the Bx's [string, i64] written after it.
run!(an_invariant_argument_decides_first, r#"{
  type Bx<'a> = Abstract<Array<'a>>;
  let f = |k: 'a, b: Bx<'a>| -> i64 1;
  let g = |#k: 'a, r: &['a, null]| -> i64 2;
  let b: Bx<[string, i64]> = Bx(["x"]);
  let o: [string, i64, null] = null;
  (f("aa", b), g(#k: "aa", &o))
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

// A transparent alias is its expansion: 'a held only as data in M<'a>
// settles to the widest argument, as it does under ['a, null], and a
// recursive alias stays a type application.
run!(an_alias_is_its_expansion, r#"{
  type M<'a> = ['a, null];
  type L<'a> = [null, ('a, L<'a>)];
  let n: [u64, null] = null;
  let f = |x: 'a, y: M<'a>| -> ['a, null] y;
  let g = |x: 'a, l: L<'a>| -> i64 3;
  let l: L<[string, i64]> = ("x", null);
  (f(n, u64:0), g("aa", l))
}"#, prints("[u64:0, i64:3]"); FuseExpect::Jit);

// Mixed sets compare by == like any union, numeric ones included: values
// of two numeric types are never equal.
const COMPARISON_MIXED: &str = r#"{
  let x: [i64, f64] = 3;
  let y: [i64, f64] = 2.5;
  let z: [i64, f64] = 3.0;
  let f = |a: Number, b: Number| a != b;
  type N = [i64, u8];
  let n: N = 3;
  [x == y, x == x, x == z, n == n, f(2.5, 1.5)]
}"#;

run!(comparison_mixed_numeric, COMPARISON_MIXED, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        &**a == &[
            Value::Bool(false),
            Value::Bool(true),
            Value::Bool(false),
            Value::Bool(true),
            Value::Bool(true),
        ]
    }
    _ => false,
});

// Float comparison is a total order in both engines: NaN equals NaN and
// sorts below every number, -inf included.
run!(
    nan_comparison_is_a_total_order,
    r#"{
  let f = |a: f64, b: f64| #[native] [a == b, a != b, a < b, a <= b, a > b, a >= b];
  let n = f64:0. / f64:0.;
  let ninf = f64:-1. / f64:0.;
  (
    f(n, n),
    f(n, ninf),
    f(ninf, n),
    array::sort([1.5, n, ninf, n, 0.0]),
    min(1.5, n),
    max(ninf, n)
  )
}"#,
    prints(
        "[[true, false, false, true, false, true], \
      [false, true, true, true, false, false], \
      [false, true, false, false, true, true], \
      [f64:NaN, f64:NaN, f64:-inf, f64:0., f64:1.5], f64:NaN, f64:-inf]"
    )
);

// The orderings and `Ordered` refuse a type holding two numeric types, as
// arithmetic's `Singleton` does: they have no order between them.
#[tokio::test(flavor = "current_thread")]
async fn ordering_refuses_two_numeric_types() -> Result<()> {
    for src in [
        "{ let a: [i64, f64] = 3; let b: [i64, f64] = 2.5; a < b }",
        "{ let xs: Array<[i64, f64]> = [i64:3, f64:2.5]; array::sort(xs) }",
        "{ let a: [i64, f64] = 3; let b: [i64, f64] = 2.5; min(a, b) }",
        "{ type N = [i64, u8]; let n: N = 3; n <= n }",
        "{ let f = |a: Number, b: Number| a >= b; f(2.5, 1.5) }",
        "{ let t: Array<(string, [i64, f64])> = [(\"a\", 1)]; array::sort(t) }",
        "{ let m: Map<[i64, f64], string> = {}; m }",
        "{ let a: [Array<i64>, Array<f64>] = [1]; let b: [Array<i64>, Array<f64>] = [0.5]; a < b }",
        "{ let a: [Map<i64, string>, Map<f64, string>] = {1 => \"a\"}; array::sort([a]) }",
        "{ let a: [{x: i64, y: string}, {x: f64, y: bool}] = {x: 1, y: \"a\"}; {a => 1} }",
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains("more than one numeric type"), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

// An open member beside a numeric one may yet be another numeric type, so
// a definition that orders or keys by them is refused, as an open member
// beside a type of a shared form is.
#[tokio::test(flavor = "current_thread")]
async fn ordering_refuses_an_open_member_beside_a_number() -> Result<()> {
    for src in [
        "{ let g = |y| array::sort([3, y, 1]); g(2.5) }",
        "{ let h = |y| [3, y] < [1, y]; h(2.5) }",
        "{ let k = |y| map::insert({}, [1, y], \"v\"); k(2.5) }",
        "{ let g = 'y: Int |y: 'y| array::sort([3, y]); g(i32:2) }",
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains("may be another numeric type"), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

// An annotated member, or an open one with a bound of one numeric type
// or none, orders.
run!(
    ordering_takes_a_member_that_is_one_number,
    r#"{
  let g = |y: i64| array::sort([3, y, 1]);
  let h = 'y: string |y: 'y| array::sort([y, "a"]);
  let k = 'y: i64 |y: 'y| array::sort([3, y]);
  g(2) == [1, 2, 3] && h("b") == ["a", "b"] && k(1) == [1, 3]
}"#,
    is(Value::Bool(true)); FuseExpect::None);

// `OneNumber` admits a nullable number of one numeric type.
const ONE_NUMBER: &str = r#"{
  let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x;
  let a: [i64, null] = 3;
  let b: [f64, null] = null;
  (f(a), f(b), f(u8:7))
}"#;

run!(one_number_bound, ONE_NUMBER, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => &**a == &[Value::I64(3), Value::Null, Value::U8(7)],
    _ => false,
});

// Arithmetic is `fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a` over
// exactly one type: a type holding two numeric types is refused even
// against itself.
#[tokio::test(flavor = "current_thread")]
async fn arithmetic_operands_are_one_type() -> Result<()> {
    for src in [
        "{ let x: [i64, f64] = 3; x + 1 }",
        "{ let x: [i64, f64] = 3; x * x }",
        "{ let x: [i64, null] = 3; x - 1 }",
        "{ let f = |a: Number, b: Number| a + b; f(1, 2) }",
        "{ let t: (Number, Number) = (1, 2.5); t.0 % t.1 }",
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains("cannot compute"), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

// A select arm that only raises leaves its success type open; the other
// operand decides it.
const ARITH_OVER_RAISING_ARM: &str = r#"{
  let go = 2;
  let r = select go { 1 => error(`Oops)?, _ => go };
  let f = 'a: Number |a: 'a, b: 'a| -> 'a a * b;
  [r * 100, f(3, 4), -r]
}"#;

run!(arith_over_raising_arm, ARITH_OVER_RAISING_ARM, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => &**a == &[Value::I64(200), Value::I64(12), Value::I64(-2)],
    _ => false,
});

// A ⊥ operand never produces, so the other operand's type stands.
const BOTTOM_OPERAND: &str = r#"{
  let x = 1;
  let y = x + never();
  let z = never() == x;
  let w: [i64, null] = null;
  let v = w$ * 2;
  0
}"#;

run!(bottom_operand, BOTTOM_OPERAND, is(Value::I64(0)));

// An index narrows an open operand to the integers, as arithmetic
// narrows one to the numbers: a function's parameter keeps the
// conjunction and each call's argument meets all of it.
const INDEX_NARROWS_AN_OPEN_OPERAND: &str = r#"{
  let f = |x| ([10, 20])[-x]$;
  let g = |x, y| ([10, 20])[x - y]$;
  let h = |x| -x;
  let k = |y| ([10, 20])[h(y)]$;
  [f(1), g(1, 0), k(2)]
}"#;

run!(index_narrows_an_open_operand, INDEX_NARROWS_AN_OPEN_OPERAND, |v: Result<
    &Value,
>| {
    match v {
        Ok(Value::Array(a)) => &**a == &[Value::I64(20), Value::I64(20), Value::I64(10)],
        _ => false,
    }
});

// An operand only ⊥ was produced into is ⊥, whatever the operator bounds
// it by: a let over ⊥, a call whose body is ⊥, in any arrangement. A
// writer still gives the let its type.
const BOTTOM_FED_OPERANDS: &str = r#"{
  let g = |b: bool| never();
  let t = never();
  let u = never();
  let a = t + u;
  let b = g(true) + { let v = g(false); v };
  let c = { let v = g(false); let w = g(true); v * w };
  let d = t < u;
  let w = never();
  let e = w + w;
  w <- 2.5;
  e
}"#;

run!(bottom_fed_operands, BOTTOM_FED_OPERANDS, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(5.0)))
});

// `Discernible`: no union anywhere in the type holds two members with one
// runtime form. A comparison, a map key and the stdlib functions that
// compare or hash (`uniq`, `min`, `max`, `array::sort`, `array::dedup`,
// `list::sort`, the `map::` functions that take or make keys) carry it,
// and a generic definition's variable takes it from its body, so each call
// checks it; a member a call leaves open is judged once it binds.
#[tokio::test(flavor = "current_thread")]
async fn discernible_refuses_one_runtime_form() -> Result<()> {
    const FNS: &str = "[fn(x: i64) -> i64, fn(s: string) -> string]";
    for (src, says) in [
        (
            format!("{{ let f = |x, y| x == y; let a = |x: i64| x; let v: {FNS} = a; f(v, v) }}"),
            "same runtime form",
        ),
        (
            r#"{
              let f = |x, y| x == y;
              let x = never();
              let r = f([x, |s: string| s], [x, |s: string| s]);
              x <- |n: i64| n;
              r
            }"#
            .to_string(),
            "same runtime form",
        ),
        (format!("{{ let a = |x: i64| x; let x: {FNS} = a; uniq(x) }}"), "same runtime form"),
        (
            format!(
                "{{ let f = 'a: Discernible |x: 'a| x; let a = |x: i64| x; let v: {FNS} = a; f(v) }}"
            ),
            "must be Discernible",
        ),
        (
            r#"{
              let g = |c, a| { let v = select c { true => a, false => |s: string| s }; v == v };
              g(true, |s: string| s)
            }"#
            .to_string(),
            "annotate it",
        ),
    ] {
        match eval_modes(&src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains(says), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

// `Result`'s success type holds no error (`'r: NoError`): its error member
// is the only error in it, wherever the success type comes from.
run!(result_success_holding_an_error_refused,
    "{ let r: Result<[i64, Error<`F>], `E> = 1; r }",
    refused("NoError does not contain [i64, Error<`F>]"); FuseExpect::None);

run!(result_success_bound_reaches_a_generic_call, r#"{
    let succeed = |x: 'a| -> Result<'a, `E> x;
    let e: [i64, Error<`F>] = 1;
    succeed(e)
}"#, refused("NoError does not contain [i64, Error<`F>]"); FuseExpect::None);

run!(
    result_success_without_errors,
    r#"{
    let succeed = |x: 'a| -> Result<'a, `E> x;
    let n: [i64, null] = null;
    (succeed(1)$, succeed("s")$, succeed(n)$)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == r#"[i64:1, "s", null]"#)
);

const DISCERNIBLE_ACCEPTS: &str = r#"{
  let f = |x, y| x == y;
  let g = |o: Option<'a>| o == o;
  let h = |c, a: i64| { let v = select c { true => a, false => 0 }; v == v };
  let k = 'a: Discernible |x: 'a| x;
  [f(1, 1), g(null), g(2), h(true, 3), k("x") == "x", array::sort([3, 1]) == [1, 3]]
}"#;

run!(discernible_accepts, DISCERNIBLE_ACCEPTS, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.iter().all(|v| *v == Value::Bool(true)) && a.len() == 6,
    _ => false,
});

// Under `Singleton` two members that differ in open cells alone are one
// member, whose cells merge: the writers of each must agree, and two
// declared variables, which can't merge, are two members.
run!(
    singleton_members_apart_in_open_cells_merge,
    r#"{ let h = 'a: Singleton |x: 'a| -> 'a x;
         let k = |b, y, z| h(select b { true => [y], false => [z] });
         k(false, 1, "a") }"#,
    refused("i64 does not contain string");
    FuseExpect::None
);
run!(
    singleton_members_apart_in_open_cells_one_type,
    r#"{ let h = 'a: Singleton |x: 'a| -> 'a x;
         let k = |b, y, z| h(select b { true => [y], false => [z] });
         k(false, 1, 2) }"#,
    prints("[i64:2]")
);
run!(
    singleton_members_apart_in_declared_variables_refused,
    r#"{ let h = 'a: Singleton |x: 'a| -> 'a x;
         let k = |b, y: 'y, z: 'z| h(select b { true => [y], false => [z] });
         k(false, 1, 2) }"#,
    refused("unbound within Singleton does not contain");
    FuseExpect::None
);

// Under `Singleton` the open members of a union must be one type: they
// merge into one cell, and two declared variables, which can't merge,
// are refused.
#[tokio::test(flavor = "current_thread")]
async fn singleton_open_members_merge() -> Result<()> {
    for (src, says) in [
        (
            r#"{
              let g = |c, a, b| {
                let s = select c { true => a, false => b };
                let t = select c { true => b, false => a };
                s + t
              };
              g(true, 1, 2.5)
            }"#,
            "does not contain",
        ),
        (
            r#"{
              let f = |s| s + s;
              let p = never();
              let q = never();
              let r = f(select true { true => p, false => q });
              p <- 1;
              q <- 2.0;
              r
            }"#,
            "cannot hold f64",
        ),
        (
            r#"{
              let f = 'a: Number, 'b: Number |c, a: 'a, b: 'b| {
                let s = select c { true => a, false => b };
                s + s
              };
              0
            }"#,
            "must be one type",
        ),
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains(says), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

const SINGLETON_MERGED_MEMBERS_RUN: &str = r#"{
  let g = |c, a, b| {
    let s = select c { true => a, false => b };
    let t = select c { true => b, false => a };
    s + t
  };
  g(true, 1, 2)
}"#;

run!(singleton_merged_members_run, SINGLETON_MERGED_MEMBERS_RUN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(3)))
});

// Containment: each rule below refuses an ill-typed program.

/// A conjunct reaches the open cells of what its cell is bound to.
const CONJUNCT_REACHES_OPEN_CELLS: &str = r#"
{
    let f = 'a: Array<i64> |x: 'a| -> i64 x[0]$;
    let g = |y| f([y]);
    let h: fn(y: string) -> i64 = g;
    h("hello")
}
"#;

run!(conjunct_reaches_open_cells, CONJUNCT_REACHES_OPEN_CELLS, refused("does not contain"); FuseExpect::None);

/// A quantifier admitted through its conjunct binds what holds it.
const RIGID_BY_CONJUNCT_COMMITS: &str = r#"
{
    let id = |a: Array<'e>| -> Array<'e> a;
    let f = 'r: Array<i64> |x: 'r| -> Array<string> id(x);
    f([1, 2])
}
"#;

run!(rigid_by_conjunct_commits, RIGID_BY_CONJUNCT_COMMITS, refused("does not contain"); FuseExpect::None);

/// Two quantifiers hold each other only through their conjuncts.
const RIGID_PAIR_BY_CONJUNCTS: &str = r#"
{
    let f = 'a: Any, 'b: Array<'a> |x: 'b, y: 'a| -> 'b y;
    f([1], 12345)
}
"#;

run!(rigid_pair_by_conjuncts, RIGID_PAIR_BY_CONJUNCTS, refused("does not contain 'a"); FuseExpect::None);

/// A rigid constructor variable is not any constructor.
const RIGID_CONSTRUCTOR_IS_NOT_ARRAY: &str = r#"
{
    let f = 'c: Collection |xs: 'c<i64>| {
        let a: Array<i64> = xs;
        a[1]$ + 1
    };
    f({"k" => 10, "j" => 20})
}
"#;

run!(rigid_constructor_is_not_array, RIGID_CONSTRUCTOR_IS_NOT_ARRAY, refused("does not contain"); FuseExpect::None);

/// A declared variable meeting an open one keeps its rigidity.
const RIGID_SURVIVES_ALIAS: &str = r#"
{
    let id = |a: 'x| a;
    let st: i64 = 0;
    trait Probe { val p: fn(self, x: 'a) -> bool };
    impl Probe for i64 { let p = |s, x| { st <- id(x); true } };
    Probe::p(1, f64:2.5)
}
"#;

run!(rigid_survives_alias, RIGID_SURVIVES_ALIAS, refused("cannot hold"); FuseExpect::None);

// A declared variable is each call's own: nothing outside the definition
// may hold a value of it, written directly, through a reference or by a
// function that shares the outer binding's type.
run!(
    declared_variable_escapes_by_a_write,
    r#"{
    let y = never();
    let f = 'a: Concrete |x: 'a| -> 'a { y <- x; x };
    let s = f("hello");
    let k: i64 = y;
    k
}"#,
    refused("'a escapes the definition");
    FuseExpect::None
);
run!(
    declared_variable_escapes_through_a_collection,
    r#"{
    let y = never();
    let f = 'a: Concrete |x: 'a| -> 'a { y <- x; x };
    let r = array::map([f], |g| g("hello"));
    let k: i64 = y;
    k
}"#,
    refused("'a escapes the definition");
    FuseExpect::None
);
run!(
    declared_variable_escapes_through_a_reference,
    r#"{
    let y = never();
    let r = &mut y;
    let f = 'a: Concrete |x: 'a| -> 'a { *r <- x; x };
    f(1)
}"#,
    refused("'a escapes the definition");
    FuseExpect::None
);
run!(
    declared_variable_escapes_through_a_call,
    r#"{
    let y = never();
    let put = |v| { y <- v; v };
    let f = 'a: Concrete |x: 'a| -> 'a put(x);
    f(1)
}"#,
    refused("'a escapes the definition");
    FuseExpect::None
);
// A variable the signature only names is no quantifier: a write of it
// outside the definition makes the definition monomorphic in it, so a
// second type is refused at its call.
run!(
    written_variable_written_outside_is_monomorphic,
    r#"{
    let y = never();
    let last = never();
    let h = |x: 'a| -> 'a { y <- x; x };
    let note = |x: 'b, z: 'b| -> 'b { last <- z; x };
    let v = h(1);
    let k: i64 = y;
    (v, k, note(2, 3), last)
}"#,
    prints("[i64:1, i64:1, i64:2, i64:3]");
    FuseExpect::None
);
run!(
    written_variable_written_outside_takes_one_type,
    r#"{
    let y = never();
    let h = |x: 'a| -> 'a { y <- x; x };
    (h(1), h("s"), y)
}"#,
    refused("'a: i64 does not contain string");
    FuseExpect::None
);
run!(
    declared_variable_stays_inside,
    r#"{
    let y = never();
    let f = 'a: Concrete |x: 'a| -> 'a {
      let z = never();
      let g = |u: i64| { z <- x; u };
      g(1);
      x
    };
    let h = |x| { y <- x; x };
    (f(1), f("s"), h(2))
}"#,
    prints(r#"[i64:1, "s", i64:2]"#);
    FuseExpect::None
);

// A variable a nested definition's body or signature writes, which no
// definition around it declares, is the written place's own, in an
// instance as in the check: never a cell an enclosing definition's check
// named after its own body's annotation.
run!(
    written_variable_of_a_nested_body_is_its_own,
    r#"{
  let outer = |n| {
    let a: Array<'n> = [n];
    let inner = |m| { let b: fn(y: 'n) -> 'n = |y| y; (b(m), b("s")) };
    (a, inner(0))
  };
  outer(1)
}"#,
    prints(r#"[[i64:1], [i64:0, "s"]]"#);
    FuseExpect::Jit
);

run!(
    written_variable_of_a_nested_signature_is_its_own,
    r#"{
  let outer = |n| {
    let a: Array<'n> = [n];
    let inner = |m: 'n| -> 'n m;
    (a, inner(0), inner("s"))
  };
  outer(1)
}"#,
    prints(r#"[[i64:1], i64:0, "s"]"#);
    FuseExpect::Jit
);

// A written type's own variable that an enclosing definition's parameter
// merges into is that parameter, live in the enclosing definition.
run!(
    written_variable_an_enclosing_parameter_decides,
    r#"{
  let outer = |n| {
    let inner = |m| {
      let s = |y| [y, n];
      let p: fn(y: 'n) -> Array<'n> = s;
      p(n)
    };
    inner(n)
  };
  (outer(1), outer("w"))
}"#,
    prints(r#"[[i64:1, i64:1], ["w", "w"]]"#);
    FuseExpect::Jit
);

run!(
    written_variable_an_enclosing_parameter_decides_in_a_holder,
    r#"{
  let outer = |n| {
    let inner = |m| {
      let s = (|x| (x, m), |y| [y, n]);
      let p: (fn(x: string) -> (string, 'm), fn(y: 'n) -> Array<'n>) = s;
      (s.0(1), p.0("s"), s.1(n), p.1(n))
    };
    inner(n)
  };
  (outer(1), outer("w"))
}"#,
    prints(
        r#"[[[i64:1, i64:1], ["s", i64:1], [i64:1, i64:1], [i64:1, i64:1]], [[i64:1, "w"], ["s", "w"], ["w", "w"], ["w", "w"]]]"#
    );
    FuseExpect::None
);

run!(
    written_variable_an_enclosing_parameter_decides_is_that_parameter,
    r#"{
  let outer = |n| {
    let inner = |m| { let p: fn(y: 'n) -> Array<'n> = |y| [y, n]; p(n) };
    let r: Array<string> = inner(n);
    r
  };
  outer(1)
}"#,
    refused("string does not contain i64")
);

/// A union implements no trait whose method takes `self` beyond the
/// receiver: dispatch narrows only the receiver.
const UNION_TRAIT_SELF_ARGUMENT: &str = r#"
{
    trait Comb { val comb: fn(self, other: self) -> string };
    type A = Abstract<i64>;
    type B = Abstract<string>;
    impl Comb for A { let comb = |a, o| "A [a.0 + o.0]" };
    impl Comb for B { let comb = |b, o| "B [b.0][o.0]" };
    let pick = |n: i64| -> [A, B] select n { 0 => A(1), _ => B("x") };
    Comb::comb(pick(0), pick(0))
}
"#;

run!(union_trait_self_argument, UNION_TRAIT_SELF_ARGUMENT, refused("does not contain [A, B]"); FuseExpect::None);

/// A union's uncovered primitives go to its open member at once.
const OPEN_MEMBER_TAKES_THE_RESIDUE: &str = r#"
{
    let v: [i64, string, null] = 1;
    let w: [i64, Array<i64>, null] = 1;
    (opt::is_some(w), opt::is_some(v))
}
"#;

run!(open_member_takes_the_residue, OPEN_MEMBER_TAKES_THE_RESIDUE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::Bool(true), Value::Bool(true)])
}; FuseExpect::Jit);

/// A union holding the very cell it is asked to contain binds nothing.
const UNION_HOLDS_ITS_OWN_CELL: &str = r#"
{
    let f = |y| array::push([y, 1], y);
    array::len(f("s"))
}
"#;

run!(union_holds_its_own_cell, UNION_HOLDS_ITS_OWN_CELL, is(Value::I64(3)); FuseExpect::Jit);

/// A recursive type under a union member with variables in its params
/// ends its walk: the memo meets a rebuilt expansion by its structure.
const RECURSIVE_UNION_MEMBER_ENDS: &str = r#"
{
    type T<'a> = { n: [T<'a>, null], v: 'a };
    let g = |t: T<'b>| -> T<['b, i64]> t;
    let h = |s| { catch(e) null; error(`A(s))?; error(`B(s))?; null };
    0
}
"#;

run!(recursive_union_member_ends, RECURSIVE_UNION_MEMBER_ENDS, is(Value::I64(0)));

/// A typedef whose parameters grow at every level cannot be compared:
/// refused, never a walk without end.
const GROWING_TYPEDEF_REFUSED: &str = r#"
{
    type N<'a> = [null, ('a, N<Array<'a>>)];
    let n1: N<i64> = null;
    let n2: N<[i64, string]> = n1;
    n2
}
"#;

run!(growing_typedef_refused, GROWING_TYPEDEF_REFUSED, refused("its parameters grow"); FuseExpect::None);

/// The same through a struct field, where the two sides expand on
/// alternate steps: each side's growth counts, so the walk ends.
const GROWING_FIELD_TYPEDEF_REFUSED: &str = r#"
{
    type T<'a> = {n: [T<Array<'a>>, null], v: 'a};
    let a: T<i64> = {n: null, v: 1};
    let b: T<[i64, f64]> = a;
    b
}
"#;

run!(
    growing_field_typedef_refused,
    GROWING_FIELD_TYPEDEF_REFUSED,
    refused("its parameters grow");
    FuseExpect::None
);

/// A typedef whose self-reference adds a union member reaches a fixed
/// point once a substituted union is flattened: the comparison ends.
const UNION_GROWING_TYPEDEF_ENDS: &str = r#"
{
    type T<'a> = {n: [T<['a, string]>, null], v: 'a};
    let a: T<i64> = {n: {n: null, v: "x"}, v: 1};
    let b: T<[i64, f64]> = a;
    let g = |t: T<'b>| -> T<['b, i64]> t;
    g(b).v
}
"#;

run!(union_growing_typedef_ends, UNION_GROWING_TYPEDEF_ENDS, is(Value::I64(1)));

/// A deep nominal value compares, and drops, without the thread stack.
const DEEP_ABSTRACT_EQ: &str = r#"
{
    type N = Abstract<[`Nil, `Cons(i64, N)]>;
    let rec build = |n: i64, acc: N| -> N select n {
        0 => acc,
        n => build(n - 1, N(`Cons(n, acc)))
    };
    let a = build(1000, N(`Nil));
    let b = build(1000, N(`Nil));
    a == b
}
"#;

run!(deep_abstract_eq, DEEP_ABSTRACT_EQ, is(Value::Bool(true)); FuseExpect::Jit);

/// A forward-referenced alias cycle is refused once its names exist.
const FORWARD_ALIAS_CYCLE: &str = r#"
{
    type T = G<[i64, T]>;
    type G<'a> = 'a;
    let s: T = "hello";
    let n: i64 = s;
    n
}
"#;

run!(forward_alias_cycle, FORWARD_ALIAS_CYCLE, refused("refers back to itself"); FuseExpect::None);

const FORWARD_MAYBE_CYCLE: &str = r#"
{
    type Node = Maybe<Node>;
    type Maybe<'a> = ['a, null];
    0
}
"#;

run!(forward_maybe_cycle, FORWARD_MAYBE_CYCLE, refused("refers back to itself"); FuseExpect::None);

// A renaming import is the type it names.
run!(
    renamed_alias_cycle,
    refused("refers back to itself"),
    "/test.gx" => r#"
        use self::T as V;
        type T = [i64, V];
        let result = 0
    "#;
    FuseExpect::None
);

/// Nested applications of one alias are no growth: they check and run.
const NESTED_ALIAS_APPLICATIONS: &str = r#"
{
    type W<'a> = 'a;
    let x: W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<i64>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> = 41;
    x + 1
}
"#;

run!(nested_alias_applications, NESTED_ALIAS_APPLICATIONS, is(Value::I64(42)));

/// Two blocks' typedefs of one name are two types, thrown types moved
/// into a function's scope included.
const SAME_NAME_OTHER_DEFINITION: &str = r#"
{
    let f = {
        type P<'a, 'b> = ('a, 'b);
        |x: i64| -> i64 throws Error<ErrChain<P<i64, string>>> select x {
            0 => error((1, "one"))?,
            n => n
        }
    };
    let g = {
        type P<'a, 'b> = ('b, 'a);
        |x: i64| -> i64 throws Error<ErrChain<P<i64, string>>> f(x)
    };
    g(1)
}
"#;

run!(same_name_other_definition, SAME_NAME_OTHER_DEFINITION, refused("the two P are different definitions"); FuseExpect::None);

/// An undefined type is reported where it is written.
const UNDEFINED_TYPE_SITED: &str = r#"{
    type T = {a: i64, b: NoSuch};
    let f = |x: T| x.a;
    f({a: 1, b: 2})
}"#;

run!(undefined_type_sited, UNDEFINED_TYPE_SITED, refused("undefined type NoSuch at line: 2"); FuseExpect::None);

// A type name two globs provide is ambiguous, as a value's is.
run!(
    ambiguous_glob_type,
    refused("`T` is ambiguous: both"),
    "/test.gx" => r#"
        mod a;
        mod b;
        use a::*;
        use b::*;
        let x: T = 1;
        let result = x
    "#,
    "/test/a.gx" => "type T = i64",
    "/test/b.gx" => "type T = string";
    FuseExpect::None
);

/// A probe that recovers a constructor binds nothing: the free member
/// takes what the constructor's member could not.
const CONSTRUCTOR_PROBE_BINDS_NOTHING: &str = r#"
{
    type L<'a> = [`C('a, L<'a>), `N];
    trait Ctor { val size: fn(self<'a>) -> i64 };
    impl Ctor for L<'_> { let size = |l| 0 };
    let f = 'c: Ctor |x: ['c<i64>, 'z]| 0;
    let g = |y| f(`C("a", y));
    g(1)
}
"#;

run!(constructor_probe_binds_nothing, CONSTRUCTOR_PROBE_BINDS_NOTHING, is(Value::I64(0)); FuseExpect::None);

/// A bound holds neither a hole nor a trait below its top.
const HOLE_IN_A_BOUND: &str = r#"
{
    let f = 'a: Array<'_> |x: 'a| x;
    f([1])
}
"#;

run!(hole_in_a_bound, HOLE_IN_A_BOUND, refused("'_ is the hole"); FuseExpect::None);

const TRAIT_UNDER_A_BOUND: &str = r#"
{
    trait Show { val show: fn(self) -> string };
    impl Show for i64 { let show = |x| "i" };
    let f = 'a: Array<Show> |x: 'a| x;
    f(["a"])
}
"#;

run!(trait_under_a_bound, TRAIT_UNDER_A_BOUND, refused("trait Show used as a type"); FuseExpect::None);

const HOLE_IN_A_FN_QUANTIFIER: &str = r#"
{
    let g: fn<'b: Array<'_>>(x: 'b) -> i64 = |x| 0;
    g([1])
}
"#;

run!(hole_in_a_fn_quantifier, HOLE_IN_A_FN_QUANTIFIER, refused("'_ is the hole"); FuseExpect::None);

/// A declared variable may be anything, so `_` cannot hold it.
const BOTTOM_REFUSES_A_DECLARED_VARIABLE: &str = r#"
{
    let f = 'a: Number |x: 'a| {
        let b: _ = x;
        x + 1
    };
    let r: i64 = f(2.5);
    r
}
"#;

run!(
    bottom_refuses_a_declared_variable,
    BOTTOM_REFUSES_A_DECLARED_VARIABLE,
    refused("_ does not contain 'a");
    FuseExpect::None
);

/// A function type without `throws` throws nothing, so a function that
/// throws a declared variable does not fit it.
const NO_THROWS_REFUSES_A_DECLARED_THROWS: &str = r#"
{
    let app = |f: fn(x: i64) -> i64 throws 'e, v: i64| -> i64 throws 'e {
        let g: fn(x: i64) -> i64 = f;
        g(v)
    };
    app(|x| x, 1)
}
"#;

run!(
    no_throws_refuses_a_declared_throws,
    NO_THROWS_REFUSES_A_DECLARED_THROWS,
    refused("fn(x: i64) -> i64 does not contain fn(x: i64) -> i64 throws 'e");
    FuseExpect::None
);

/// An abstract type hides its representation, so its parameters are
/// invariant: a sink of i64 is not a sink of `[i64, string]`.
const ABSTRACT_PARAMETERS_ARE_INVARIANT: &str = r#"
{
    type Sink<'a> = Abstract<fn(x: 'a) -> i64>;
    let p: Sink<i64> = Sink(|x: i64| x);
    let q: Sink<[i64, string]> = p;
    q.0("hello")
}
"#;

run!(
    abstract_parameters_are_invariant,
    ABSTRACT_PARAMETERS_ARE_INVARIANT,
    refused("Sink<[i64, string]> does not contain Sink<i64>");
    FuseExpect::None
);

/// An unknown constructor's argument is invariant too: a `'c<i64>` is no
/// `'c<[i64, string]>`.
const CONSTRUCTOR_ARGUMENTS_ARE_INVARIANT: &str = r#"
{
    let f = 'c: Collection |a: 'c<[i64, string]>| 0;
    let g = 'c: Collection |b: 'c<i64>| f(b);
    g([1])
}
"#;

run!(
    constructor_arguments_are_invariant,
    CONSTRUCTOR_ARGUMENTS_ARE_INVARIANT,
    refused("Collection<[i64, string]> does not contain 'c: unbound within Collection<i64>");
    FuseExpect::None
);

/// The annotation, not the payload, decides a constructed value's
/// parameters.
const ANNOTATION_DECIDES_CONSTRUCTED_PARAMETERS: &str = r#"
{
    type Box<'a> = Abstract<'a>;
    let b: Box<[i64, string]> = Box(1);
    let take = |b: Box<[i64, string]>| select b.0 { i64 as n => n, string as _ => 0 };
    take(b)
}
"#;

run!(
    annotation_decides_constructed_parameters,
    ANNOTATION_DECIDES_CONSTRUCTED_PARAMETERS,
    is(Value::I64(1));
    FuseExpect::Jit
);

/// A constructed value's payload bounds its parameters from below, so a
/// wider type wherever the value flows decides them: a return, an
/// element, a field, a select's arms and a connect.
const CONTEXT_DECIDES_CONSTRUCTED_PARAMETERS: &str = r#"
{
    type Box<'a> = Abstract<'a>;
    let n = |b: Box<[i64, string]>| select b.0 { i64 as n => n, string as _ => 100 };
    let ret = || -> Box<[i64, string]> Box(1);
    let t: (Box<[i64, string]>, i64) = (Box(2), 0);
    let s: {a: Box<[i64, string]>} = {a: Box(3)};
    let a: Array<Box<[i64, string]>> = [Box(4)];
    let sel: Box<[i64, string]> = select t.1 { 0 => Box(5), _ => Box("s") };
    let c: Box<[i64, string]> = never();
    c <- Box(6);
    n(ret()) + n(t.0) + n(s.a) + n(a[0]$) + n(sel) + n(c)
}
"#;

run!(
    context_decides_constructed_parameters,
    CONTEXT_DECIDES_CONSTRUCTED_PARAMETERS,
    is(Value::I64(21));
    FuseExpect::Jit
);

/// A union of two instantiations takes the one the payload fits.
const CONSTRUCTED_INTO_A_UNION_OF_INSTANTIATIONS: &str = r#"
{
    type Box<'a> = Abstract<'a>;
    let x: [Box<i64>, Box<string>] = Box("hi");
    select x { Box<i64> as _ => 1, Box<string> as _ => 2 }
}
"#;

run!(
    constructed_into_a_union_of_instantiations,
    CONSTRUCTED_INTO_A_UNION_OF_INSTANTIATIONS,
    is(Value::I64(2));
    FuseExpect::Jit
);

run!(
    constructed_parameter_below_a_narrower_type_refused,
    r#"{
    type Box<'a> = Abstract<'a>;
    let b: Box<string> = Box(1);
    0
}"#,
    refused("Box<string> does not contain Box<i64>");
    FuseExpect::None
);

// A declared variable is a constant in its body: a payload of another
// type is no value of it.
run!(
    constructed_parameter_below_a_declared_variable_refused,
    r#"{
    type Box<'a> = Abstract<'a>;
    let f = 'a: Any |x: 'a| -> Box<'a> Box(1);
    f(2)
}"#,
    refused("Box<'a: unbound within Any> does not contain Box<i64>");
    FuseExpect::None
);

/// A definition that constructs keeps the bound in its signature: each
/// call's context decides its own instance, and one nothing widens is
/// the payload's type.
const CONSTRUCTING_DEFINITION_DECIDED_PER_CALL: &str = r#"
{
    type Box<'a> = Abstract<'a>;
    let mk = |x| Box(x);
    let w: Box<[i64, string]> = mk(1);
    let i = mk(2);
    let s = mk("s");
    let n = select w.0 { i64 as n => n, string as _ => 0 };
    "[n] [i.0 + 1] [s.0]"
}
"#;

run!(
    constructing_definition_decided_per_call,
    CONSTRUCTING_DEFINITION_DECIDED_PER_CALL,
    is(Value::String("1 3 s".into()));
    FuseExpect::Jit
);

// A payload's bound outlives its parameter's binding to a type with open
// parts: `array::len` makes `'p` an `Array<'a>`, and `'a` holds the
// payload's element type.
run!(
    payload_bound_reaches_the_open_parts_of_a_binding,
    r#"{
    type B<'p> = Abstract<'p>;
    let b = B(["a"]);
    let n = array::len(b.0);
    n + select b.0 { [x, ..] => x + 1, _ => 0 }
}"#,
    refused("cannot compute string + i64");
    FuseExpect::None
);

run!(
    payload_bound_fits_the_open_parts_of_a_binding,
    r#"{
    type B<'p> = Abstract<'p>;
    let b = B([41]);
    let n = array::len(b.0);
    n + select b.0 { [x, ..] => x + 1, _ => 0 }
}"#,
    is(Value::I64(43));
    FuseExpect::Jit
);

// A definition that constructs: the call's bound reaches the binding too.
run!(
    constructing_definition_bound_reaches_a_binding,
    r#"{
    type B<'p> = Abstract<'p>;
    let mk = |x| B(x);
    let b = mk(["a"]);
    let n = array::len(b.0);
    n + select b.0 { [x, ..] => x + 1, _ => 0 }
}"#,
    refused("cannot compute string + i64");
    FuseExpect::None
);

// Two bounds reach the open parts of a binding together, in either
// order: the element is their union.
run!(
    payload_bounds_reach_a_binding_together,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |x: B<'q>, y: B<'q>| -> B<'q> y;
    let r = f(B([1]), B(["a"]));
    let n = array::len(r.0);
    n + select r.0 { [x, ..] => x + 1, _ => 0 }
}"#,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const PAYLOAD_BOUNDS_FIT_A_BINDING_TOGETHER: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let f = |x: B<'q>, y: B<'q>| -> B<'q> y;
    let r = f(B([1]), B([null, 20]));
    let n = array::len(r.0);
    let w = f(B(["a"]), B([1]));
    let m = array::len(w.0);
    let s: B<Array<[i64, string]>> = w;
    m + array::fold(r.0, n, |acc, x| select x { null => acc, i64 as k => acc + k })
}
"#;

run!(
    payload_bounds_fit_a_binding_together,
    PAYLOAD_BOUNDS_FIT_A_BINDING_TOGETHER,
    is(Value::I64(23));
    FuseExpect::Jit
);

run!(
    payload_bounds_held_by_the_binding_after_it,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |x: B<'q>, y: B<'q>| -> B<'q> y;
    let r = f(B([1]), B(["a"]));
    let n = array::len(r.0);
    let s: B<Array<i64>> = r;
    n
}"#,
    refused("B<Array<i64>> does not contain B<'q: Array<'a: unbound above i64 | string>>");
    FuseExpect::None
);

// A definition whose body binds a constructed parameter holds its
// argument to that binding.
run!(
    definition_binding_a_bounded_parameter_holds_its_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let g = |x| { let b = B(x); let n: B<[i64, string]> = b; x };
    g(true)
}"#,
    refused("[i64, string] does not contain bool");
    FuseExpect::None
);

run!(
    definition_binding_open_parts_holds_its_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let g = |x| { let b = B(x); let n = array::len(b.0); select b.0 { [y, ..] => y + n, _ => 0 } };
    g([41]) + g(["a"])
}"#,
    |v: Result<&Value>| matches!(v, Err(e) if {
        let e = format!("{e:#}");
        e.contains("in: [\"a\"]: type mismatch Array<")
            && e.ends_with("does not contain Array<string>")
    });
    FuseExpect::None
);

const DEFINITION_BINDING_A_BOUNDED_PARAMETER: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let g = |x| { let b = B(x); let n = array::len(b.0); select b.0 { [y, ..] => y + n, _ => 0 } };
    let h = |x| { let b = B(x); let n: B<[i64, string]> = b; n };
    let r = h(1);
    g([41]) + select r.0 { i64 as k => k + 1, string as _ => 0 }
}
"#;

run!(
    definition_binding_a_bounded_parameter,
    DEFINITION_BINDING_A_BOUNDED_PARAMETER,
    is(Value::I64(44));
    FuseExpect::Jit
);

// ⊥ holds no payload: a parameter bounded from below never binds to it.
run!(
    payload_bound_refuses_bottom,
    r#"{
    type B<'p> = Abstract<'p>;
    let b: B<_> = B(1);
    0
}"#,
    refused("B<_> does not contain B<i64>");
    FuseExpect::None
);

/// A select over a payload bounded by `first`'s and `then`'s, both over
/// `x`, decided after: both values of `b.0`, then both of `arm`'s.
fn select_over_two_payload_bounds(first: &str, then: &str, arm: &str) -> String {
    format!(
        r#"{{
    type B<'p> = Abstract<'p>;
    let mk = |a| B(a);
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = {first};
    b <- {then};
    let v = select b.0 {{ {arm}, _ => never() }};
    x <- t ~ 1;
    let vs = [];
    vs <- array::window(#n: 2, v ~ vs, v);
    let bs = [];
    bs <- array::window(#n: 2, b ~ bs, b.0);
    select array::len(bs) {{ 2 => (bs, vs), _ => never() }}
}}"#
    )
}

const ARRAY_THEN_ELEMENT: &str = "[[[[i64:1]], [i64:1]], [[i64:1], i64:1]]";

// An element bound beside an array of it: the union's open member takes
// both, so the arm's element holds an array and its element.
run!(
    select_element_of_two_payload_bounds,
    select_over_two_payload_bounds("B([[x]])", "B([x])", "[h, ..] => h"),
    prints(ARRAY_THEN_ELEMENT)
);

run!(
    select_element_of_two_payload_bounds_list,
    select_over_two_payload_bounds("B([[<x>]])", "B([x])", "[h, ..] => h"),
    prints(ARRAY_THEN_ELEMENT)
);

run!(
    select_element_of_two_payload_bounds_tuple,
    select_over_two_payload_bounds("B([(x, x)])", "B([x])", "[h, ..] => h"),
    prints("[[[[i64:1, i64:1]], [i64:1]], [[i64:1, i64:1], i64:1]]")
);

run!(
    select_element_of_two_payload_bounds_struct,
    select_over_two_payload_bounds("B([{a: x}])", "B([x])", "[h, ..] => h"),
    prints(r#"[[[[["a", i64:1]]], [i64:1]], [[["a", i64:1]], i64:1]]"#)
);

run!(
    select_element_of_two_payload_bounds_variant,
    select_over_two_payload_bounds("B([`V(x)])", "B([x])", "[h, ..] => h"),
    prints(r#"[[[["V", i64:1]], [i64:1]], [["V", i64:1], i64:1]]"#)
);

run!(
    select_element_of_two_payload_bounds_in_a_list,
    select_over_two_payload_bounds("B([<[x]>])", "B([<x>])", "[<h, ..>] => h"),
    prints(ARRAY_THEN_ELEMENT)
);

run!(
    select_rest_of_two_payload_bounds,
    select_over_two_payload_bounds("B([[x], [x]])", "B([x, x])", "[g, h..] => h"),
    prints("[[[[i64:1], [i64:1]], [i64:1, i64:1]], [[[i64:1]], [i64:1]]]")
);

run!(
    select_element_of_two_payload_bounds_by_a_definition,
    select_over_two_payload_bounds("mk([[x]])", "mk([x])", "[h, ..] => h"),
    prints(ARRAY_THEN_ELEMENT)
);

run!(
    select_element_of_two_payload_bounds_takes_no_arithmetic,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([[x]]);
    b <- B([x]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    x <- t ~ 1;
    v + 1
}"#,
    refused("cannot compute [i64, Array<i64>] + i64");
    FuseExpect::None
);

// A bound's cell below a union with a decided member is bound to the
// union where that holds every bound: `x` may hold either member.
run!(
    payload_cell_below_a_union_takes_it_whole,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let f = |b: B<Array<[i64, 'a]>>, y: 'a| b;
    let x = never();
    let c = f(B([x]), "s");
    x <- t ~ 1;
    c.0
}"#,
    prints("[i64:1]")
);

// Bounds a probe admits apart but no binding holds together refuse the
// select's binding whole: `x` is left to its write.
run!(
    payload_bounds_refused_together_bind_nothing,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B((x, x));
    let v = select b.0 { (1, "a") => 0, _ => 1 };
    x <- t ~ "s";
    (b.0, v)
}"#,
    prints(r#"[["s", "s"], i64:1]"#)
);

// A type test over the element: `[i64, Array<i64>]` holds `x` and
// `Array<x>` together only as `x := i64`, which the open bound takes
// after the other.
const TYPE_TEST_OVER_THE_ELEMENT: &str =
    "[h, ..] => select h { i64 as n => n + 100, Array<i64> as a => array::len(a) }";

run!(
    select_type_test_over_two_payload_bounds,
    select_over_two_payload_bounds("B([[x]])", "B([x])", TYPE_TEST_OVER_THE_ELEMENT),
    prints("[[[[i64:1]], [i64:1]], [i64:1, i64:101]]")
);

run!(
    select_type_test_over_two_payload_bounds_by_a_definition,
    select_over_two_payload_bounds("mk([[x]])", "mk([x])", TYPE_TEST_OVER_THE_ELEMENT),
    prints("[[[[i64:1]], [i64:1]], [i64:1, i64:101]]")
);

/// A select over a payload built as `first` and then `then`, with arms
/// at two depths: what each value of `b.0` selects.
fn arms_at_two_depths(first: &str, then: &str) -> String {
    format!(
        r#"{{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = {first};
    b <- t ~ {then};
    let v = select b.0 {{ [[h, ..], ..] => `A(h), [h, ..] => `B(h), _ => `C }};
    let vs = [];
    vs <- array::window(#n: 2, v ~ vs, v);
    select array::len(vs) {{ 2 => vs, _ => never() }}
}}"#
    )
}

// Arms at two depths decide the payload's type from their patterns, as
// they decide an inferred plain scrutinee's (`|x| select x { .. }`
// called with `[["a"]]` and `["b"]` is refused too): a type their binds
// cannot hold beside what was built. The annotation decides it instead.
run!(
    select_arms_at_two_depths_over_two_payloads,
    arms_at_two_depths(r#"B([["a"]])"#, r#"B(["b"])"#),
    refused(
        "the arms decide this scrutinee's type from their patterns, and their binds \
         cannot hold what was built into it, [Array<string>, Array<Array<string>>]: \
         annotate that type where it is built"
    );
    FuseExpect::None
);

run!(
    select_arms_at_two_depths_over_an_annotated_payload,
    arms_at_two_depths(
        r#"{ let b: B<[Array<Array<string>>, Array<string>]> = B([["a"]]); b }"#,
        r#"B(["b"])"#
    ),
    prints(r#"[["A", "a"], ["B", "b"]]"#)
);

// One payload: the first arm can match none of it once its type is
// written, and the arms cannot decide it either.
run!(
    select_arms_at_two_depths_over_one_payload,
    r#"{
    type B<'p> = Abstract<'p>;
    let b = B(["b"]);
    select b.0 { [[h, ..], ..] => `A(h), [h, ..] => `B(h), _ => `C }
}"#,
    refused(
        "the arms decide this scrutinee's type from their patterns, and their binds \
         cannot hold what was built into it, Array<string>: annotate that type where \
         it is built"
    );
    FuseExpect::None
);

// What was built holds a cell its variable's later write decides: the
// annotation names the variable, whose type is written there.
run!(
    select_arms_at_two_depths_over_a_variables_payloads,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([[x]]);
    b <- B([x]);
    let v = select b.0 { [[h, ..], ..] => `A(h), [h, ..] => `B(h), _ => `C };
    x <- t ~ 1;
    v
}"#,
    refused(
        "the arms decide this scrutinee's type from their patterns, and their binds \
         cannot hold what was built into it, [Array<'x>, Array<Array<'x>>]: annotate \
         that type where it is built, writing for 'x the type of `x` (line 4, column 9)"
    );
    FuseExpect::None
);

/// A select over a payload built as `[x]` ahead of a write of `[[x]]`,
/// `b` declared as `decl`: each value of `v`.
fn select_before_a_deeper_write(decl: &str) -> String {
    format!(
        r#"{{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b{decl} = B([x]);
    let v = select b.0 {{ [h, ..] => h, _ => never() }};
    b <- B([[x]]);
    x <- t ~ 1;
    let vs = [];
    vs <- array::window(#n: 2, v ~ vs, v);
    select array::len(vs) {{ 2 => vs, _ => never() }}
}}"#
    )
}

/// The hint for a write of `[[x]]` to `b`, bound on line 5, over a
/// payload a select decided from `[x]`, `x` bound on line 4: the payload's
/// union, and a recursive type where what is written reads `b`.
const DEEPER_WRITE_HINT: &str = "a use before this write decided b's type from what was \
     built into it then: declare b: B<[Array<'x>, Array<Array<'x>>]> where it is bound \
     (line 5, column 9), writing for 'x the type of `x` (line 4, column 9). If what is \
     written reads b (through a function, a reference or another binding), no finite \
     type holds both: declare a named recursive type for b where it is bound";

// The select decides the payload from `[x]` alone, so the write of
// `[[x]]` after it is refused, though no finite type is wanting: the
// payload's union, declared, holds both.
run!(
    select_before_a_deeper_write,
    select_before_a_deeper_write(""),
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

run!(
    select_before_a_deeper_write_declared,
    select_before_a_deeper_write(": B<[Array<i64>, Array<Array<i64>>]>"),
    prints("[i64:1, [i64:1]]")
);

run!(
    select_before_a_deeper_write_in_a_definition,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let f = |x| {
        let b = B([x]);
        let v = select b.0 { [h, ..] => h, _ => never() };
        b <- t ~ B([[x]]);
        v
    };
    f(1)
}"#,
    refused(
        "declare b: B<[Array<'x>, Array<Array<'x>>]> where it is bound (line 5, column \
         13), writing for 'x the type of `x` (line 4, column 14). If what is written \
         reads b (through a function, a reference or another binding), no finite type \
         holds both: declare a named recursive type for b where it is bound"
    );
    FuseExpect::None
);

// What is written reads the payload the select decided: no finite type
// holds it.
run!(
    select_before_a_write_of_its_own_payload,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([x]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    b <- B([t ~ b.0]);
    x <- t ~ 1;
    v
}"#,
    holds_itself(
        "b is B<Array<'",
        "no finite type holds both: declare a named recursive type for b"
    );
    FuseExpect::None
);

run!(
    select_before_a_write_of_its_own_payload_through_a_let,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B([]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    let c = B([t ~ b.0]);
    b <- c;
    v
}"#,
    holds_itself(
        "b is B<Array<'",
        "no finite type holds both: declare a named recursive type for b"
    );
    FuseExpect::None
);

/// A select over a payload built as `[x]`, then `between`, then a write
/// of `write`, which reads the payload through another binding.
fn select_before_a_write_reading_its_payload(between: &str, write: &str) -> String {
    format!(
        r#"{{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([x]);
    let v = select b.0 {{ [h, ..] => h, _ => never() }};
    {between}
    b <- {write};
    x <- t ~ 1;
    v
}}"#
    )
}

run!(
    select_before_a_write_reading_its_payload_through_a_function,
    select_before_a_write_reading_its_payload("let g = |u| u ~ b.0;", "B([g(t)])"),
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

run!(
    select_before_a_write_reading_its_payload_in_a_lambda,
    select_before_a_write_reading_its_payload("", "B([(|u| u ~ b.0)(t)])"),
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

run!(
    select_before_a_write_reading_its_payload_through_a_reference,
    select_before_a_write_reading_its_payload("let r = &b;", "B([t ~ (*r).0])"),
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

run!(
    select_before_a_write_reading_its_payload_through_a_write,
    select_before_a_write_reading_its_payload("let c = never(); c <- b.0;", "B([t ~ c])"),
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

run!(
    select_before_a_write_of_the_selected_element,
    select_before_a_write_reading_its_payload("", "B([[t ~ v]])"),
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

// What was built holds no variable: the element the select gives is the
// payload's own.
run!(
    select_before_a_write_of_the_selected_element_built_known,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B([1]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    b <- B([[t ~ v]]);
    v
}"#,
    refused(
        "b is B<Array<i64>> and cannot hold B<Array<Array<i64>>>; no finite type holds \
         both: declare a named recursive type for b where it is bound (line 4, column 9)"
    );
    FuseExpect::None
);

// The payload's recursive type the hint asks for holds both.
run!(
    select_before_a_write_of_the_selected_element_recursive,
    r#"{
    type B<'p> = Abstract<'p>;
    type P = [i64, Array<P>];
    let t = sys::time::timer(duration:10.ms, false);
    let b: B<Array<P>> = B([1]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    b <- B([[t ~ v]]);
    let vs = [];
    vs <- array::window(#n: 2, v ~ vs, v);
    select array::len(vs) { 2 => vs, _ => never() }
}"#,
    prints("[i64:1, [i64:1]]")
);

// A later statement writes what is built from the payload.
run!(
    select_before_a_deeper_write_of_a_variable_the_payload_writes,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([x]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    b <- B([[x]]);
    x <- t ~ v;
    v
}"#,
    refused(DEEPER_WRITE_HINT);
    FuseExpect::None
);

// A write holding its target's type under a constructor: a union of the
// two holds what is written only where it does not read the target.
run!(
    write_nesting_its_target,
    r#"{
    let src = [[i64:0]];
    src <- [i64:0, src];
    src
}"#,
    refused(
        "src is Array<Array<i64>> and cannot hold Array<[i64, Array<Array<i64>>]>; no \
         finite type holds both: declare a named recursive type for src where it is \
         bound (line 2, column 9)"
    );
    FuseExpect::None
);

run!(
    write_nesting_another_binding,
    r#"{
    let a = [[i64:0]];
    let src = [[i64:0]];
    src <- [i64:0, a];
    src
}"#,
    refused(
        "src is Array<Array<i64>> and cannot hold Array<[i64, Array<Array<i64>>]>; \
         declare src: [Array<[i64, Array<Array<i64>>]>, Array<Array<i64>>] where it is \
         bound (line 3, column 9)"
    );
    FuseExpect::None
);

// Two payloads of one abstract type: the declaration that holds both
// unions the parameter, through which the payload is read.
run!(
    payload_write_declares_the_parameters_union,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([[x]]);
    let c = B([x]);
    let w = select c.0 { [h, ..] => h + 1, _ => never() };
    b <- t ~ c;
    x <- t ~ 1;
    w
}"#,
    refused(
        "b is B<Array<Array<i64>>> and cannot hold B<Array<i64>>; declare b: \
         B<[Array<i64>, Array<Array<i64>>]> where it is bound (line 5, column 9)"
    );
    FuseExpect::None
);

// A cell the checker made up has no name a program writes: the hint
// names no type.
run!(
    write_hint_with_no_written_form,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let f = 'a: [i64, Array<'b>] |x: B<'a>, y: B<'b>| -> B<'a> x;
    let g = B(never());
    let h = B(never());
    let r = f(g, h);
    g <- h;
    h <- B(t ~ 1);
    r.0
}"#,
    refused("; declare g with a type holding both where it is bound (line 5, column 9)");
    FuseExpect::None
);

// An open bound below a union whose binding holds every bound binds to
// it whole: `y` is the arm's element set, which the other bound makes an
// array, so a later write of a string is refused. The set's two cells,
// bound to one type, print once.
run!(
    payload_cell_bound_to_an_element_set,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let y = never();
    let b = B([[x]]);
    b <- B([y]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    x <- t ~ 1;
    y <- t ~ "s";
    v
}"#,
    refused(
        "y is Array<i64>, inferred from an earlier use, and cannot hold string; declare \
         y: [string, Array<i64>] where it is bound (line 5, column 9)"
    );
    FuseExpect::None
);

run!(
    payload_cells_bound_to_an_element_set,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let y = never();
    let b = B([x]);
    b <- B([y]);
    let v = select b.0 { [h, ..] => h, _ => never() };
    x <- t ~ 1;
    y <- t ~ "s";
    v
}"#,
    refused(
        "y is i64, inferred from an earlier use, and cannot hold string; declare y: \
         [i64, string] where it is bound (line 5, column 9)"
    );
    FuseExpect::None
);

// A NoError payload's bound reaches a binding that holds an error beside it.
run!(
    no_error_payload_bound_reaches_a_binding,
    r#"{
    type B<'p: NoError> = Abstract<'p>;
    let b = B(["a"]);
    let w: [Array<'e>, Error<`X>] = b.0;
    select w { error as _ => 0, [x, ..] => x + 1, _ => 0 }
}"#,
    refused("cannot compute string + i64");
    FuseExpect::None
);

run!(
    no_error_payload_bound_fits_a_binding,
    r#"{
    type B<'p: NoError> = Abstract<'p>;
    let b = B([4]);
    let w: [Array<'e>, Error<`X>] = b.0;
    select w { error as _ => 0, [x, ..] => x + 1, _ => 0 }
}"#,
    is(Value::I64(5));
    FuseExpect::Jit
);

// A `$` over an element a payload bounds, before anything decides it.
run!(
    payload_bounded_operand_is_not_decided,
    r#"{
    type B<'p> = Abstract<'p>;
    let b = B([null, 1]);
    let n = array::len(b.0);
    select b.0 { [x, ..] => x$ + n, _ => 0 }
}"#,
    refused("the operand of $ has a type not decided here (at least [i64, null]): annotate it");
    FuseExpect::None
);

/// An input's settle binds a parameter both a payload bounded and a ⊥ was
/// produced into to the payload's type, never ⊥: a later input reading
/// the payload is held to it, whether the ⊥ reached the parameter
/// directly or through a construction's.
#[tokio::test(flavor = "current_thread")]
async fn settle_keeps_a_payload_bound_beside_bottom() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    use graphix_rt::GXEvent;
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let inputs = [
        "type B<'p> = Abstract<'p>",
        "let f = |x: B<'q>, y: B<'q>| -> B<'q> x",
        "let g = |x: B<'q>, y: 'q| x",
        "let r = f(B(\"a\"), B(never()))",
        "let q = g(B(\"b\"), never())",
    ];
    let mut held = Vec::new();
    for i in inputs {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    for read in ["let z: i64 = r.0", "let z: i64 = q.0"] {
        let Err(e) = ctx.rt.compile(ArcStr::from(read)).await else {
            anyhow::bail!("{read} was accepted")
        };
        assert!(format!("{e:#}").ends_with("i64 does not contain 'q: string"), "{e:#}");
    }
    let probe =
        ctx.rt.compile(ArcStr::from("{ let s: string = r.0; \"[s][q.0]\" }")).await?;
    let id = probe.exprs[0].id;
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        let batch = tokio::time::timeout_at(deadline, rx.recv()).await;
        let Ok(Some(batch)) = batch else { anyhow::bail!("the read never produced") };
        if let Some(v) = batch.iter().find_map(|ev| match ev {
            GXEvent::Updated(eid, v) if *eid == id => Some(v.clone()),
            _ => None,
        }) {
            assert_eq!(v, Value::String("ab".into()));
            break;
        }
    }
    drop(held);
    ctx.shutdown().await;
    Ok(())
}

/// A refusal naming the held type and the written one, whose cells'
/// ids vary.
fn holds_itself(
    held: &'static str,
    written: &'static str,
) -> impl Fn(Result<&Value>) -> bool {
    move |v: Result<&Value>| {
        matches!(v, Err(e) if {
            let e = format!("{e:#}");
            e.contains(held) && e.contains(written)
        })
    }
}

// A parameter bounded below by an array of itself has no finite type:
// the write that closes the cycle is refused, as `b <- [t ~ b]` is.
const PAYLOAD_CYCLE_UNDER_A_CONSTRUCTOR: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B([]);
    b <- B([t ~ b.0]);
    select b.0 { [x, ..] => x + 1, _ => 0 }
}
"#;

run!(
    payload_cycle_under_a_constructor,
    PAYLOAD_CYCLE_UNDER_A_CONSTRUCTOR,
    holds_itself("b is B<Array<'", "and cannot hold B<Array<Array<'");
    FuseExpect::None
);

const PAYLOAD_CYCLE_UNDER_A_CONSTRUCTOR_KNOWN: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B([1]);
    b <- B([t ~ b.0]);
    select b.0 { [x, ..] => x + 1, _ => 0 }
}
"#;

run!(
    payload_cycle_under_a_constructor_known,
    PAYLOAD_CYCLE_UNDER_A_CONSTRUCTOR_KNOWN,
    refused("b is B<Array<i64>> and cannot hold B<Array<Array<i64>>>");
    FuseExpect::None
);

const PAYLOAD_CYCLE_WRITE_ALONE: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B([]);
    b <- B([t ~ b.0]);
    b
}
"#;

run!(
    payload_cycle_write_alone,
    PAYLOAD_CYCLE_WRITE_ALONE,
    holds_itself("b is B<Array<'", "and cannot hold B<Array<Array<'");
    FuseExpect::None
);

const PAYLOAD_CYCLE_WRITE_ALONE_KNOWN: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B([1]);
    b <- B([t ~ b.0]);
    b
}
"#;

run!(
    payload_cycle_write_alone_known,
    PAYLOAD_CYCLE_WRITE_ALONE_KNOWN,
    refused("b is B<Array<i64>> and cannot hold B<Array<Array<i64>>>");
    FuseExpect::None
);

// A typedef whose body holds its parameter in a field is a constructor
// of it.
const PAYLOAD_CYCLE_THROUGH_A_TYPEDEF: &str = r#"
{
    type B<'p> = Abstract<'p>;
    type Bx<'a> = {v: 'a};
    let mk = |x: 'a| -> Bx<'a> {v: x};
    let t = sys::time::timer(duration:10.ms, false);
    let b = B(1);
    b <- B(mk(t ~ b.0));
    b
}
"#;

run!(
    payload_cycle_through_a_typedef,
    PAYLOAD_CYCLE_THROUGH_A_TYPEDEF,
    holds_itself("b is B<i64>", "and cannot hold B<{ v: '");
    FuseExpect::None
);

// The same writes without the abstract.
const ARRAY_CYCLE_KNOWN: &str = r#"
{
    let t = sys::time::timer(duration:10.ms, false);
    let b = [1];
    b <- [t ~ b];
    select b { [x, ..] => x + 1, _ => 0 }
}
"#;

run!(
    array_cycle_known,
    ARRAY_CYCLE_KNOWN,
    refused("b is Array<i64> and cannot hold Array<Array<i64>>");
    FuseExpect::None
);

const ARRAY_CYCLE: &str = r#"
{
    let t = sys::time::timer(duration:10.ms, false);
    let b = [];
    b <- [t ~ b];
    select b { [x, ..] => x + 1, _ => 0 }
}
"#;

run!(
    array_cycle,
    ARRAY_CYCLE,
    holds_itself("b is Array<'", "and cannot hold Array<Array<'");
    FuseExpect::None
);

const ARRAY_CYCLE_THROUGH_A_LET: &str = r#"
{
    let t = sys::time::timer(duration:10.ms, false);
    let b = [];
    let c = [t ~ b];
    b <- c;
    select b { [x, ..] => x + 1, _ => 0 }
}
"#;

run!(
    array_cycle_through_a_let,
    ARRAY_CYCLE_THROUGH_A_LET,
    holds_itself("b is Array<'", "and cannot hold Array<Array<'");
    FuseExpect::None
);

// A cycle through cells alone is no refusal.
const PAYLOAD_CYCLE_OF_CELLS: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    x <- y;
    x.0 + 1
}
"#;

run!(payload_cycle_of_cells, PAYLOAD_CYCLE_OF_CELLS, is(Value::I64(2)));

// The cells on a cycle through cells alone are one type: a write to one
// widens every one of them.
const BARE_CYCLE_WRITTEN: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    x <- y;
    y <- B(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_written,
    BARE_CYCLE_WRITTEN,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const BARE_CYCLE_HOLDS_EVERY_WRITE: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    x <- y;
    y <- B(t ~ "a");
    let v = x.0;
    let seen = [];
    seen <- array::window(#n: 3, v ~ seen, v);
    select array::len(seen) { 3 => seen, _ => never() }
}
"#;

run!(
    bare_cycle_holds_every_write,
    BARE_CYCLE_HOLDS_EVERY_WRITE,
    prints(r#"[i64:1, i64:1, "a"]"#)
);

const BARE_CYCLE_WRITTEN_BEFORE_IT_CLOSES: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    y <- B(t ~ "a");
    x <- y;
    x.0 + 1
}
"#;

run!(
    bare_cycle_written_before_it_closes,
    BARE_CYCLE_WRITTEN_BEFORE_IT_CLOSES,
    refused("cannot compute [string, i64] + i64");
    FuseExpect::None
);

const BARE_CYCLE_OF_THREE_CELLS: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    let z = B(t ~ y.0);
    x <- z;
    y <- B(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_of_three_cells,
    BARE_CYCLE_OF_THREE_CELLS,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const BARE_CYCLE_OF_THREE_CELLS_WRITTEN_LAST: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    let z = B(t ~ y.0);
    x <- z;
    z <- B(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_of_three_cells_written_last,
    BARE_CYCLE_OF_THREE_CELLS_WRITTEN_LAST,
    refused("cannot compute [string, i64] + i64");
    FuseExpect::None
);

const BARE_CYCLE_THROUGH_TWO_ABSTRACTS: &str = r#"
{
    type B<'p> = Abstract<'p>;
    type C<'q> = Abstract<'q>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let z = C(t ~ x.0);
    let y = B(t ~ z.0);
    x <- y;
    z <- C(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_through_two_abstracts,
    BARE_CYCLE_THROUGH_TWO_ABSTRACTS,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const BARE_CYCLE_THROUGH_A_FUNCTION: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let f = |v| B(t ~ v);
    let x = B(1);
    let y = f(x.0);
    x <- y;
    y <- B(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_through_a_function,
    BARE_CYCLE_THROUGH_A_FUNCTION,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const BARE_CYCLE_THROUGH_AN_IDENTITY: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let id = |v| v;
    let x = B(1);
    let y = B(id(t ~ x.0));
    x <- y;
    y <- B(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_through_an_identity,
    BARE_CYCLE_THROUGH_AN_IDENTITY,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const BARE_CYCLE_IN_A_DEFINITION: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let g = |a, s| {
        let t = sys::time::timer(duration:10.ms, false);
        let x = B(a);
        let y = B(t ~ x.0);
        x <- y;
        y <- B(t ~ s);
        x.0
    };
    g(1, "a") + 1
}
"#;

run!(
    bare_cycle_in_a_definition,
    BARE_CYCLE_IN_A_DEFINITION,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

const BARE_CYCLE_THROUGH_A_UNION: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let c = false;
    let x = B(1);
    let u = select c { true => t ~ x.0, false => t ~ null };
    let y = B(u);
    x <- y;
    x.0 + 1
}
"#;

run!(
    bare_cycle_through_a_union,
    BARE_CYCLE_THROUGH_A_UNION,
    refused("cannot compute [i64, null] + i64");
    FuseExpect::None
);

const BARE_CYCLE_THROUGH_A_TYPEDEF_PARAMETER: &str = r#"
{
    type Opt<'a> = ['a, null];
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let f = |v: 'a, n: bool| -> Opt<'a> select n { true => v, false => null };
    let x = B(1);
    let y = B(f(t ~ x.0, false));
    x <- y;
    x.0 + 1
}
"#;

run!(
    bare_cycle_through_a_typedef_parameter,
    BARE_CYCLE_THROUGH_A_TYPEDEF_PARAMETER,
    refused("cannot compute [i64, null] + i64");
    FuseExpect::None
);

const BARE_CYCLE_WRITTEN_THROUGH_A_REFERENCE: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = B(1);
    let y = B(t ~ x.0);
    let r = &mut y;
    x <- y;
    *r <- B(t ~ "a");
    x.0 + 1
}
"#;

run!(
    bare_cycle_written_through_a_reference,
    BARE_CYCLE_WRITTEN_THROUGH_A_REFERENCE,
    refused("cannot compute [i64, string] + i64");
    FuseExpect::None
);

// A list or a tuple literal's type is its elements' types, with no cell
// bounded below between them, so the same program closes no cycle and the
// write is refused where it is made.
const LIST_CYCLE_WRITTEN: &str = r#"
{
    let t = sys::time::timer(duration:10.ms, false);
    let x = [<1>];
    let y = [<t ~ list::head(x)$>];
    x <- y;
    y <- [<t ~ "a">];
    list::head(x)$ + 1
}
"#;

run!(
    list_cycle_written,
    LIST_CYCLE_WRITTEN,
    refused("y is List<i64> and cannot hold List<string>");
    FuseExpect::None
);

const TUPLE_CYCLE_WRITTEN: &str = r#"
{
    let t = sys::time::timer(duration:10.ms, false);
    let x = (1, 0);
    let y = (t ~ x.0, 0);
    x <- y;
    y <- (t ~ "a", 0);
    x.0 + 1
}
"#;

run!(
    tuple_cycle_written,
    TUPLE_CYCLE_WRITTEN,
    refused("y is (i64, i64) and cannot hold (string, i64)");
    FuseExpect::None
);

// A typedef that names one of its parameters' typedefs twice holds the
// parameter under that typedef's constructor at the second name too.
const PAYLOAD_CYCLE_THROUGH_A_TYPEDEF_NAMED_TWICE: &str = r#"
{
    type B<'p> = Abstract<'p>;
    type Q<'b> = Array<'b>;
    type P<'a> = [Q<i64>, Q<'a>];
    let mk = |x: 'a| -> [P<'a>, null] [x];
    let t = sys::time::timer(duration:10.ms, false);
    let b = B(1);
    b <- B(mk(t ~ b.0));
    b
}
"#;

run!(
    payload_cycle_through_a_typedef_named_twice,
    PAYLOAD_CYCLE_THROUGH_A_TYPEDEF_NAMED_TWICE,
    holds_itself(
        "b is B<i64> and cannot hold B<[null, P<'",
        "no finite type holds both: declare a named recursive type for b"
    );
    FuseExpect::None
);

// A cell above an array of another cell and above that cell is no cycle:
// a lower bound closes one only down from the cell below or up from the
// cell above.
const LOWER_BOUNDS_BESIDE_AN_ARRAY_OF_THEMSELVES: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b = B([x]);
    b <- B([[x]]);
    let v = array::map(b.0, |e| select e {
        i64 as n => n + 1,
        Array<i64> as a => array::len(a) * 100
    });
    x <- t ~ 1;
    let seen = [];
    seen <- array::window(#n: 2, v ~ seen, v);
    select array::len(seen) { 2 => seen, _ => never() }
}
"#;

run!(
    lower_bounds_beside_an_array_of_themselves,
    LOWER_BOUNDS_BESIDE_AN_ARRAY_OF_THEMSELVES,
    prints("[[i64:2], [i64:100]]")
);

// A select's two arms meeting under arithmetic merge a cell with one
// above an array of it.
const MERGE_UNDER_A_CONSTRUCTOR: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let c = true;
    let y = never();
    let b = B([y]);
    let u = select c { true => b.0, false => y };
    let r = u + u;
    y <- t ~ 1;
    r
}
"#;

run!(
    merge_under_a_constructor,
    MERGE_UNDER_A_CONSTRUCTOR,
    refused("unsatisfiable constraints on '");
    FuseExpect::None
);

const MERGE_UNDER_A_CONSTRUCTOR_IN_A_CALL: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let c = true;
    let y = never();
    let b = B([y]);
    let g = |a, z| { let u = select c { true => a, false => z }; u + u };
    let r = g(b.0, y);
    y <- t ~ 1;
    r
}
"#;

run!(
    merge_under_a_constructor_in_a_call,
    MERGE_UNDER_A_CONSTRUCTOR_IN_A_CALL,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

// A cycle only the instance closes: the argument it refuses has no
// finite type.
const PAYLOAD_CYCLE_AN_INSTANCE_CLOSES: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let pair = |a, b: B<'q>| { let c = B([t ~ b.0]); a <- c; a };
    let x = B([]);
    let r = pair(x, x);
    select r.0 { [h, ..] => h + 1, _ => 0 }
}
"#;

run!(
    payload_cycle_an_instance_closes,
    PAYLOAD_CYCLE_AN_INSTANCE_CLOSES,
    refused("cannot infer a finite type here");
    FuseExpect::None
);

// A cycle through a function's argument has no finite type either.
const PAYLOAD_CYCLE_THROUGH_AN_ARGUMENT: &str = r#"
{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let b = B(|x| 1);
    let f = |x| { x <- t ~ b.0; 2 };
    b <- B(f);
    0
}
"#;

run!(
    payload_cycle_through_an_argument,
    PAYLOAD_CYCLE_THROUGH_AN_ARGUMENT,
    holds_itself(
        "and cannot hold B<fn(x: fn(x: '",
        "no finite type holds both: declare a named recursive type for b"
    );
    FuseExpect::None
);

/// A REPL input writing a payload that holds the binding's own element:
/// the binding stands above `Array<'e>`, an open cell, so no finite type
/// holds both.
#[tokio::test(flavor = "current_thread")]
async fn repl_payload_cycle_hints_a_recursive_type() -> anyhow::Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    use tokio::sync::mpsc;
    let (tx, _rx) = mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let mut inputs = vec![];
    for input in [
        "type B<'p> = Abstract<'p>",
        "let t = sys::time::timer(duration:10.ms, false)",
        "let b = B([])",
    ] {
        inputs.push(ctx.rt.compile(ArcStr::from(input)).await?);
    }
    match ctx.rt.compile(ArcStr::from("b <- B([t ~ b.0])")).await {
        Ok(_) => anyhow::bail!("the write was accepted"),
        Err(e) => {
            let e = format!("{e:#}");
            assert!(
                e.contains("no finite type holds both: declare a named recursive"),
                "{e}"
            );
            Ok(())
        }
    }
}

/// A union of a bare variable and a constructor of it places every
/// member: when the constructor's first fit would strand one, the
/// deepest member fit decides the variable.
const UNION_PLACES_EVERY_MEMBER: &str = r#"
{
    let g = |x: ['b, Array<'b>]| -> ['b, Array<'b>] x;
    let y: [Array<i64>, Array<Array<i64>>] = [[1]];
    g(y)
}
"#;

run!(union_places_every_member, UNION_PLACES_EVERY_MEMBER, prints("[[i64:1]]"));

/// A binding something writes is not generalized: its uses and its
/// writers share one type, whatever the statement order.
const WRITTEN_BINDING_IS_MONOMORPHIC: &str = r#"
{
    let f = |x| x;
    let n = f(41);
    f <- |x| "oops";
    n + 1
}
"#;

run!(
    written_binding_is_monomorphic,
    WRITTEN_BINDING_IS_MONOMORPHIC,
    refused("f is fn(x: i64) -> i64 and cannot hold fn(x: i64) -> string");
    FuseExpect::None
);

// A written binding's type is one instance however it is written: a
// variable an annotation or `never<T>` names, or a ⊥ initializer's
// pattern, is the binding's, so a definition writing it is not
// generalized over it.
run!(
    written_annotation_variable_is_monomorphic,
    r#"{ let r: ['a, null] = null; let f = |x| { r <- x; x }; (f(1), f("a")) }"#,
    refused("does not contain string");
    FuseExpect::None
);

run!(
    written_never_annotation_is_monomorphic,
    r#"{ let r = never<['a, null]>(); let f = |x| { r <- x; x }; (f(1), f("a")) }"#,
    refused("does not contain string");
    FuseExpect::None
);

run!(
    written_bottom_pattern_is_monomorphic,
    r#"{ let (a, b) = never(); let f = |x| { a <- x; x }; let p = (f(1), f("s")); (a, b) }"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    written_body_annotation_variable_is_monomorphic,
    r#"{ let f = |x| { let r: 'a = never(); let g = |y| { r <- y; y }; (g(1), g("a"), r) };
         f(1) }"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    unwritten_annotated_lambda_stays_generic,
    r#"{ let id: fn(x: 'a) -> 'a = |x| x;
         let f = |x| { let r: ['a, null] = null; r <- x; x };
         (id(1), id("s"), f(2), f("t")) }"#,
    prints(r#"[i64:1, "s", i64:2, "t"]"#);
    FuseExpect::Jit
);

// A lambda a struct, tuple or array holds keeps its signature generic: a
// call through it handed a value holding it decides nothing of the
// signature, so its instance is typed at the call's types.
run!(
    holder_of_a_lambda_passed_to_it,
    r#"{
  let s = {f: |x| 0};
  let g = s.f;
  let t = (|x| 1, 1);
  let a = [|x| 2];
  (s.f(s), s.f([s]), s.f({a: s}), s.f(s.f), g(s), t.0([t]), a[0]$([a]))
}"#,
    prints("[i64:0, i64:0, i64:0, i64:0, i64:0, i64:1, i64:2]");
    FuseExpect::None
);

// A binding holding a lambda in a struct, its update, a tuple or an array
// is a scheme of it, as a let of the lambda is: each read copies the
// signature, so an annotation, a typed formal, a call handed the holder or
// a bound settled to ⊥ decides nothing of the definition, and every call
// is typed at its own types.
run!(
    holder_reads_copy_its_signatures,
    r#"{
  let s = {f: |x| x == x};
  let g = {f: 'a: Concrete |x: 'a| -> 'a x};
  let t = (|x| x, 0);
  let fs = [|x| x];
  let w = {s with f: |y| y == y};
  let h = |p| 0;
  let k = |p: (fn(x: i64) -> i64, i64)| p.0(p.1);
  let a: {f: fn(x: i64) -> i64} = g;
  let b: Array<fn(x: i64) -> i64> = fs;
  let c: {f: fn(x: string) -> bool} = w;
  (s.f(s.f), s.f(s), h(s), s.f(1), g.f("g"), a.f(1), t.0("t"), k(t),
   fs[0]$("f"), b[0]$(2), w.f(w), w.f(3), c.f("c"))
}"#,
    prints(r#"[true, true, i64:0, true, "g", i64:1, "t", i64:0, "f", i64:2, true, true, true]"#);
    FuseExpect::None
);

/// A holder an earlier input made stays a scheme: a later input's
/// annotation of it decides nothing for the earlier input's calls.
#[tokio::test(flavor = "current_thread")]
async fn holder_of_an_earlier_input_stays_generic() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _s = ctx.rt.compile(ArcStr::from("let s = {f: |x| x}")).await?;
    let _n = ctx.rt.compile(ArcStr::from("let n = s.f(1)")).await?;
    let t = "let t: {f: fn(x: string) -> string} = s";
    let _t = ctx.rt.compile(ArcStr::from(t)).await?;
    let _m = ctx.rt.compile(ArcStr::from("let m = s.f(2)")).await?;
    ctx.shutdown().await;
    Ok(())
}

// A destructuring let that writes one of its names reads each holder in its
// value as a copy: the written name's writer or annotation decides nothing
// of what the holder's other reads share.
run!(
    holder_destructured_beside_a_written_name,
    r#"{
  let s = {f: |x| x};
  let v = s.f("x");
  let (a, b) = (s, s);
  let {c, d} = {c: s, d: 0};
  let ((e, i), k) = ((s, 1), 0);
  a <- never<{f: fn(x: i64) -> i64}>();
  c <- never<{f: fn(x: bool) -> bool}>();
  e <- never<{f: fn(x: f64) -> f64}>();
  let t: {f: fn(x: i64) -> i64} = a;
  (v, t.f(1), b.f("b"), c.f(true), d, e.f(1.5), i, k, s.f(2))
}"#,
    prints(r#"["x", i64:1, "b", true, i64:0, f64:1.5, i64:1, i64:0, i64:2]"#);
    FuseExpect::None
);
// A holder's value reads each binding once: what a call in one part decides
// of the copy holds for the others.
run!(
    holder_value_reads_a_binding_once,
    r#"{
  let k = |p: {f: fn(x: i64) -> i64}| 0;
  let s = {f: |x| x};
  let t = {a: s, b: k(s)};
  (t.a.f("x"), s.f(true))
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

// A let forwarding a holder's scheme to a pattern that writes one of its
// names gives each name a copy of its own: the written name's writer decides
// nothing of the others, whichever comes first.
run!(
    holder_forwarded_beside_a_written_name,
    r#"{
  let s = {f: |x| x};
  let h = (s, s);
  let (a, b) = h;
  a <- never<{f: fn(x: i64) -> i64}>();
  let w = b.f("w");
  let (c, d) = h;
  let u = d.f("u");
  c <- never<{f: fn(x: i64) -> i64}>();
  let k = {p: s, q: s, n: 0};
  let {p, q, ..} = k;
  p <- never<{f: fn(x: i64) -> i64}>();
  let r = q.f("r");
  let {p: e, q: g, ..} = k;
  let v = g.f("v");
  e <- never<{f: fn(x: i64) -> i64}>();
  let all@ {p: m, ..} = k;
  m <- never<{f: fn(x: i64) -> i64}>();
  (w, u, r, v, a.f(1), c.f(2), p.f(3), e.f(4), m.f(5))
}"#,
    prints(r#"["w", "u", "r", "v", i64:1, i64:2, i64:3, i64:4, i64:5]"#);
    FuseExpect::None
);
run!(
    holder_forwarded_through_a_partial_capture,
    r#"{
  let s = {f: |x| x};
  let k = {p: s, q: s, n: 0};
  let all@ {p: m, ..} = k;
  let al2@ {p: m2, ..} = k;
  m2 <- never<{f: fn(x: i64) -> i64}>();
  (all.p.f(5), all.q.f("a"), all.n, m.f(true), m2.f(7), al2.p.f("x"), al2.n)
}"#,
    prints(r#"[i64:5, "a", i64:0, true, i64:7, "x", i64:0]"#);
    FuseExpect::None
);
run!(
    holder_forwarded_beside_a_written_name_in_a_body,
    r#"{
  let g = |z| {
    let s = {f: |x| x};
    let h = (s, s);
    let (a, b) = h;
    a <- never<{f: fn(x: i64) -> i64}>();
    let w = b.f(z);
    let (c, d) = h;
    let u = d.f(z);
    c <- never<{f: fn(x: i64) -> i64}>();
    (w, u, a.f(1), c.f(2))
  };
  (g("q"), g(true))
}"#,
    prints(r#"[["q", "q", i64:1, i64:2], [true, true, i64:1, i64:2]]"#);
    FuseExpect::None
);
run!(
    holder_destructured_beside_a_written_name_in_a_body,
    r#"{
  let g = |z| {
    let s = {f: |x| x == x};
    let (a, b) = (s, s);
    b <- never();
    (b.f(b), s.f(z), a.f("a"))
  };
  g(1)
}"#,
    prints("[true, true, true]");
    FuseExpect::None
);

/// Where the node `t` lives, a var by its cell.
fn type_node(t: &graphix_compiler::typ::Type) -> Option<usize> {
    use graphix_compiler::typ::Type;
    match t {
        Type::Tuple(a) | Type::Set(a) | Type::Variant(_, a, _) => Some(a.as_ptr().addr()),
        Type::Struct(a) => Some(a.as_ptr().addr()),
        Type::Array(a) | Type::List(a) => Some(triomphe::Arc::as_ptr(a).addr()),
        Type::Map { key, .. } => Some(triomphe::Arc::as_ptr(key).addr()),
        Type::Fn(f) => Some(triomphe::Arc::as_ptr(f).addr()),
        Type::TVar(tv) => Some(tv.cell_addr()),
        _ => None,
    }
}

/// The nodes `t` holds, a node many paths share counted once.
fn distinct_type_nodes(t: &graphix_compiler::typ::Type) -> usize {
    use graphix_compiler::typ::Type;
    use std::collections::HashSet;
    fn go(t: &Type, seen: &mut HashSet<usize>) {
        if type_node(t).is_some_and(|a| !seen.insert(a)) {
            return;
        }
        match t {
            Type::TVar(tv) => {
                if let Some(b) = tv.binding() {
                    go(&b, seen)
                }
            }
            t => t.for_each_child(&mut |c| go(c, seen)),
        }
    }
    let mut seen = HashSet::new();
    go(t, &mut seen);
    seen.len()
}

/// The node `t`'s field or element `name` is, past the vars bound to it.
fn part_node(t: &graphix_compiler::typ::Type, name: &str) -> Option<usize> {
    use graphix_compiler::typ::Type;
    fn bound(t: &Type) -> Type {
        match t {
            Type::TVar(tv) => tv.binding().map_or_else(|| t.clone(), |b| bound(&b)),
            t => t.clone(),
        }
    }
    let part = match &bound(t) {
        Type::Struct(fs) => fs.iter().find(|(n, _, _)| n.as_str() == name)?.1.clone(),
        Type::Tuple(ts) => ts.get(name.parse::<usize>().ok()?)?.clone(),
        _ => return None,
    };
    type_node(&bound(&part))
}

/// A chain of holders, each reading the last twice (in arrays, lists, a
/// map's values, a call's argument, a block, a select's arm, a lambda's body
/// or a struct update), holds one copy of its last link's type: the chain's type grows by
/// a few nodes a link, where a copy at each read would double it, and two
/// parts a link reads the last through are one type.
#[tokio::test(flavor = "current_thread")]
async fn holder_chain_stays_a_dag() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_compiler::{Scope, expr::ModPath};
    use graphix_package_core::testing::init;
    const LINKS: usize = 10;
    const PAIRS: &str = "let id = |v| v;\nlet y = (|z| z, 2);\nlet x = (|x| x, 1);\n";
    const RECORD: &str = "let x = {n: 0, f: |x| x};\n";
    for (shape, prelude, link, one) in [
        ("arrays", PAIRS, "([x], [x, y])", None),
        ("lists", PAIRS, "{a: [<x>], b: [<x, y>]}", None),
        ("maps", PAIRS, "`M({\"k\" => x}, {\"j\" => x})", None),
        ("calls", PAIRS, "{a: x, b: id(x)}", Some(["a", "b"])),
        ("call pairs", PAIRS, "(id(x), id(x), |w| w)", Some(["0", "1"])),
        ("blocks", PAIRS, "{a: x, b: {let t = x; t}}", Some(["a", "b"])),
        ("selects", PAIRS, "{a: x, b: select 1 { _ => x }}", None),
        ("lambdas", PAIRS, "{a: x, g: || x}", None),
        ("updates", RECORD, "{n: 1, a: x, b: {x with n: 2}}", None),
        ("update pairs", RECORD, "{n: 1, a: {x with n: 1}, b: {x with n: 2}}", None),
    ] {
        let (tx, _rx) = tokio::sync::mpsc::channel(1000);
        let ctx = init(tx, crate::TEST_REGISTER).await?;
        let mut code = String::from(prelude);
        for _ in 0..LINKS {
            code.push_str("let x = ");
            code.push_str(link);
            code.push_str(";\n");
        }
        let _c = ctx.rt.compile(ArcStr::from(code)).await?;
        let env = ctx.rt.get_env().await?;
        let (_, x) = env
            .lookup_bind(&Scope::root().lexical, &ModPath::from_iter(["x"]))?
            .ok_or_else(|| anyhow::anyhow!("x not in scope"))?;
        let nodes = distinct_type_nodes(&x.typ);
        assert!(nodes <= 16 * (LINKS + 1), "{shape}: {nodes} nodes");
        if let Some([p, q]) = one {
            let (p_at, q_at) = (part_node(&x.typ, p), part_node(&x.typ, q));
            assert!(p_at.is_some() && p_at == q_at, "{shape}: {p} and {q} are two types");
        }
        ctx.shutdown().await;
    }
    Ok(())
}

// A formal union of two variables is decided by the positions that hold
// each alone, in an instance's signature as in a written function type.
run!(
    formal_union_of_two_variables_instance,
    r#"{
  let f = |x: ['a, 'b], y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([y], [z]);
  let u = never();
  let r = f(u, 1, "s");
  u <- "t";
  r
}"#,
    prints(r#"[[i64:1], ["s"]]"#);
    FuseExpect::Jit
);
run!(
    formal_union_of_two_variables_written,
    r#"{
  let f = |x: ['a, 'b], y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([y], [z]);
  let g: fn(x: [i64, string], y: i64, z: string) -> (Array<i64>, Array<string>) = f;
  g("t", 1, "s")
}"#,
    prints(r#"[[i64:1], ["s"]]"#);
    FuseExpect::Jit
);

// The result decides them as well as a parameter does: an instance's
// signature holds each member only after every other position is held.
run!(
    formal_union_of_two_variables_decided_by_the_result,
    r#"{
  let f = |x: ['a, 'b]| -> (Array<'a>, Array<'b>) ([], []);
  let g = |x: ['a, 'b]| -> Array<'a> [];
  let u = never();
  let r = (f(u), g(u));
  u <- "t";
  u <- 1;
  r
}"#,
    prints("[[[], []], []]");
    FuseExpect::Jit
);

// Every position is held, whichever round checks it: a result whose union
// the parameter's binding closes is checked all the same.
run!(
    formal_union_of_two_variables_every_position_held,
    r#"{
  let f = |x: ['a, 'b]| -> ['a, 'b] x;
  let g: fn(x: bool) -> i64 = f;
  g(true) + 1
}"#,
    refused("fn(x: bool) -> i64 does not contain fn(x: ['a: bool, 'b: unbound]) -> ['a: bool, 'b: unbound]");
    FuseExpect::None
);
run!(
    formal_union_of_two_variables_every_position_held_in_a_union,
    r#"{
  let f = |x: ['a, 'b]| -> ['a, 'b] x;
  let g: fn(x: bool) -> [i64, string] = f;
  g(true)
}"#,
    refused("fn(x: bool) -> [i64, string] does not contain fn(x: ['a: bool, 'b: unbound])");
    FuseExpect::None
);
run!(
    formal_union_of_two_variables_every_position_held_in_a_field,
    r#"{
  let f = |x: ['a, 'b], y: 'a, z: 'b| -> ['a, 'b] x;
  let s: {k: fn(x: bool, y: i64, z: string) -> [i64, string]} = {k: f};
  s.k(true, 1, "s")
}"#,
    refused("does not contain { k: fn(x: ['a: i64, 'b: string], y: 'a: i64, z: 'b: string)");
    FuseExpect::None
);

// A written binding holding a function in a part, a struct field or a
// tuple element written through a place reference, shares one instance
// of it with its writers; so does a binding a lambda body writes. Each
// use comes before its writer.
run!(
    written_struct_holder_is_monomorphic,
    r#"{ let s = {f: |x| x}; let n = s.f(41); let k = s.f("a");
         s <- k ~ {f: |x| x + 1}; n }"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    written_through_a_place_is_monomorphic,
    r#"{ let t = (|x| x, 1); let r = &mut t.0; let n = t.0(41);
         *r <- |x| "oops"; n + 1 }"#,
    refused("fn(x: i64) -> i64 does not contain fn(x: i64) -> string");
    FuseExpect::None
);

run!(
    written_in_a_lambda_is_monomorphic,
    r#"{ let f = |x| x; let a = f("s"); let set = |g| f <- g;
         set(|x: i64| x + 1); a }"#,
    refused("fn(x: string) -> string does not contain fn(x: i64) -> i64");
    FuseExpect::None
);

// A write names the latest let of its name before it in the block: an
// earlier let of the same name stays generic.
run!(
    write_to_a_later_let_leaves_the_earlier_generic,
    r#"{ let f = |x| x; let a = f(1); let b = f("s"); let f = 0; f <- 1; (a, b) }"#,
    prints(r#"[i64:1, "s"]"#)
);

// A write to a name a nearer binding shadows leaves the outer let generic.
run!(
    shadowed_write_leaves_the_let_generic,
    r#"{ let f = |x| x; let a = { let f = 0; f <- 1; f }; (f(1), f("s"), a) }"#,
    prints(r#"[i64:1, "s", i64:0]"#)
);

// A writable reference in a function body to a function its block holds
// makes it a `<-` target as a write there does: one compile, nothing
// generalized it.
run!(
    mut_ref_in_a_lambda_to_a_written_function,
    prints("[i64:3, i64:1]"),
    "/test.gx" => "let f = |x| x;
let h = |t: i64| { let r = &mut f; *r <- t ~ |x| x; t };
let result = (h(3), f(1))";
    FuseExpect::None
);

/// A binding an earlier input generalized stays generic, its uses having
/// relied on it: a later write is held to its every instance, and a later
/// `&mut` of it is refused.
#[tokio::test(flavor = "current_thread")]
async fn written_after_generalized_is_held_to_it() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _f = ctx.rt.compile(ArcStr::from("let f = |x| x")).await?;
    let g = "let g = select true { true => f, false => f }; let s = g(\"s\")";
    let _s = ctx.rt.compile(ArcStr::from(g)).await?;
    let narrow = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 1")).await.unwrap_err();
    let narrow = format!("{narrow:#}");
    assert!(narrow.contains("what is written to it must be as general"), "{narrow}");
    // the type it is held to names its one variable once: fn(x: 'a) -> 'a
    let held = narrow.split("f is fn(x: ").nth(1).expect("the held type is printed");
    let (arg, rest) = held.split_once(") -> ").expect("a function type");
    assert!(rest.starts_with(arg), "{narrow}");
    let _w = ctx.rt.compile(ArcStr::from("f <- |x| x")).await?;
    let r = ctx.rt.compile(ArcStr::from("let r = &mut f")).await.unwrap_err();
    assert!(format!("{r:#}").contains("made generic by an earlier compile"));
    ctx.shutdown().await;
    Ok(())
}

/// A later input's `<-` to a function an earlier input calls statically
/// is refused, naming the call, which keeps the definition it bound; one
/// no call fixed takes the write.
#[tokio::test(flavor = "current_thread")]
async fn written_after_a_static_call_is_refused() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _f = ctx
        .rt
        .compile(ArcStr::from("let f = |x: i64| x + 1; let g = |x: i64| x + 1"))
        .await?;
    let _r = ctx.rt.compile(ArcStr::from("let r = f(1)")).await?;
    let e = ctx.rt.compile(ArcStr::from("f <- |x| x + 100")).await.unwrap_err();
    let e = format!("{e:#}");
    assert!(e.contains("f cannot be written") && e.contains("f(1)"), "{e}");
    let _w = ctx.rt.compile(ArcStr::from("g <- |x| x + 2")).await?;
    ctx.shutdown().await;
    Ok(())
}

/// The first value the expression `id` produces, within 10 s.
async fn value_of(
    rx: &mut tokio::sync::mpsc::Receiver<
        poolshark::global::GPooled<Vec<graphix_rt::GXEvent>>,
    >,
    id: graphix_compiler::expr::ExprId,
) -> Result<Value> {
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        let batch = tokio::time::timeout_at(deadline, rx.recv()).await;
        let Ok(Some(batch)) = batch else { anyhow::bail!("{id:?} never produced") };
        if let Some(v) = batch.iter().find_map(|ev| match ev {
            graphix_rt::GXEvent::Updated(eid, v) if *eid == id => Some(v.clone()),
            _ => None,
        }) {
            return Ok(v);
        }
    }
}

/// A static call holds its callee only while it lives: once the input
/// that made it is deleted, a `<-` to the callee compiles. Fused, the
/// kernel that replaced the call holds it in the call's place.
#[tokio::test(flavor = "current_thread")]
async fn a_deleted_static_call_releases_its_callee() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_compiler::CFlag;
    use graphix_package_core::testing::{TestCtx, config};
    for flags in [CFlag::FusionDisabled.into(), Default::default()] {
        let (tx, _rx) = tokio::sync::mpsc::channel(1000);
        let ctx = TestCtx::start(
            config(tx, crate::TEST_REGISTER, vec![], |_| {})?.flags(flags),
        )
        .await?;
        let _f = ctx.rt.compile(ArcStr::from("let f = |x: i64| -> i64 x + 1")).await?;
        let fused = ctx.rt.fusion_stats().await?.fused;
        let r = ctx.rt.compile(ArcStr::from("let r = f(1)")).await?;
        let fused = ctx.rt.fusion_stats().await?.fused > fused;
        assert_eq!(fused, flags.is_empty(), "{flags:?}");
        let e = ctx.rt.compile(ArcStr::from("f <- |x| x + 100")).await.unwrap_err();
        let e = format!("{e:#}");
        assert!(e.contains("f cannot be written") && e.contains("f(1)"), "{e}");
        drop(r);
        let _w = ctx.rt.compile(ArcStr::from("f <- |x| x + 100")).await?;
        ctx.shutdown().await;
    }
    Ok(())
}

/// A dynamic module's body that called `f` statically holds it until a
/// reload replaces the body: then nothing calls `f` and a `<-` to it
/// compiles.
#[tokio::test(flavor = "current_thread")]
async fn a_reloaded_module_releases_its_static_callee() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let mut held = Vec::new();
    for i in [
        "let f = |x: i64| x + 1",
        "let src = \"let v = super::f(1)\"",
        "let s = mod d dynamic { sandbox unrestricted; sig { val v: i64 }; source src }",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(value_of(&mut rx, v.exprs[0].id).await?, Value::I64(2));
    let e = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await.unwrap_err();
    assert!(format!("{e:#}").contains("super::f(1)"), "{e:#}");
    let _w = ctx.rt.compile(ArcStr::from("src <- \"let v = 5\"")).await?;
    ctx.rt.wait_settled().await?;
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(value_of(&mut rx, v.exprs[0].id).await?, Value::I64(5));
    let _w = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A reload's new body may write what the body it replaces calls
/// statically, since that body goes with the reload; a new body that
/// does not compile leaves the old one, holding its callee again.
#[tokio::test(flavor = "current_thread")]
async fn a_reload_may_write_what_the_replaced_body_calls() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let mut held = Vec::new();
    for i in [
        "let f = |x: i64| x + 1",
        "let src = \"let v = super::f(1)\"",
        "let s = mod d dynamic { sandbox unrestricted; sig { val v: i64 }; source src }",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(value_of(&mut rx, v.exprs[0].id).await?, Value::I64(2));
    let bad = "src <- \"super::f <- |x: i64| x + 100; let v = nope\"";
    let _w = ctx.rt.compile(ArcStr::from(bad)).await?;
    ctx.rt.wait_settled().await?;
    let e = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await.unwrap_err();
    assert!(format!("{e:#}").contains("super::f(1)"), "{e:#}");
    let good = "src <- \"super::f <- |x: i64| x + 100; let v = 7\"";
    let _w = ctx.rt.compile(ArcStr::from(good)).await?;
    ctx.rt.wait_settled().await?;
    let st = ctx.rt.compile(ArcStr::from("s")).await?;
    assert_eq!(value_of(&mut rx, st.exprs[0].id).await?, Value::Null);
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(value_of(&mut rx, v.exprs[0].id).await?, Value::I64(7));
    let r = ctx.rt.compile(ArcStr::from("f(1)")).await?;
    assert_eq!(value_of(&mut rx, r.exprs[0].id).await?, Value::I64(101));
    ctx.shutdown().await;
    Ok(())
}

/// A `<-` to a module's interface binding writes the implementation's
/// too: a static call a later input builds in the module calls what was
/// written.
#[tokio::test(flavor = "current_thread")]
async fn a_write_through_an_interface_reaches_a_later_static_call() -> Result<()> {
    use arcstr::{ArcStr, literal};
    use graphix_compiler::expr::{VfsEntry, VfsResolver};
    use graphix_package_core::testing::{TestCtx, config};
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let files = vec![VfsResolver::new(ahash::AHashMap::from_iter([
        (
            netidx_core::path::Path::from("/a.gxi"),
            VfsEntry::from(literal!(
                "val f: fn(x: i64) -> i64; val g: fn(x: i64) -> i64"
            )),
        ),
        (
            netidx_core::path::Path::from("/a.gx"),
            VfsEntry::from(literal!(
                "let f = |x: i64| -> i64 x + 1; let g = |x: i64| -> i64 f(x)"
            )),
        ),
    ]))];
    let ctx = TestCtx::start(config(tx, crate::TEST_REGISTER, files, |_| {})?).await?;
    let _a = ctx.rt.compile(ArcStr::from("mod a")).await?;
    let _w = ctx.rt.compile(ArcStr::from("a::f <- |x: i64| x + 100")).await?;
    ctx.rt.wait_settled().await?;
    let v = ctx.rt.compile(ArcStr::from("a::g(1)")).await?;
    assert_eq!(value_of(&mut rx, v.exprs[0].id).await?, Value::I64(101));
    ctx.shutdown().await;
    Ok(())
}

/// A dynamic module's reload reaches what an earlier input bound to its
/// interface, as it reaches a reader in its own input.
#[tokio::test(flavor = "current_thread")]
async fn a_reload_reaches_a_reader_in_an_earlier_input() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let mut held = Vec::new();
    for i in [
        "let src = \"let v = 1\"",
        "let s = mod d dynamic { sandbox unrestricted; sig { val v: i64 }; source src }",
        "let w = d::v",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let tens = ctx.rt.compile(ArcStr::from("d::v * 10")).await?;
    assert_eq!(value_of(&mut rx, tens.exprs[0].id).await?, Value::I64(10));
    let _w = ctx.rt.compile(ArcStr::from("src <- \"let v = 5\"")).await?;
    assert_eq!(value_of(&mut rx, tens.exprs[0].id).await?, Value::I64(50));
    ctx.rt.wait_settled().await?;
    let w = ctx.rt.compile(ArcStr::from("w")).await?;
    assert_eq!(value_of(&mut rx, w.exprs[0].id).await?, Value::I64(5));
    ctx.shutdown().await;
    Ok(())
}

/// What a module's body produces reaches its readers in other inputs: a
/// `let` bound to an interface value, and an instance of a function of the
/// body reading a binding a write through the interface changes.
#[tokio::test(flavor = "current_thread")]
async fn a_module_reaches_its_readers_in_other_inputs() -> Result<()> {
    use arcstr::{ArcStr, literal};
    use graphix_compiler::expr::{VfsEntry, VfsResolver};
    use graphix_package_core::testing::{TestCtx, config};
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let files = vec![VfsResolver::new(ahash::AHashMap::from_iter([
        (
            netidx_core::path::Path::from("/a.gxi"),
            VfsEntry::from(literal!("val x: i64; val y: i64; val g: fn(k: i64) -> i64")),
        ),
        (
            netidx_core::path::Path::from("/a.gx"),
            VfsEntry::from(literal!(
                "let x = 0; let y = x + 1; let g = |k: i64| -> i64 x + k"
            )),
        ),
    ]))];
    let ctx = TestCtx::start(config(tx, crate::TEST_REGISTER, files, |_| {})?).await?;
    let mut held = Vec::new();
    for i in ["mod a", "let w = a::y", "let r = a::g(1)"] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let tens = ctx.rt.compile(ArcStr::from("a::y * 10")).await?;
    assert_eq!(value_of(&mut rx, tens.exprs[0].id).await?, Value::I64(10));
    let _w = ctx.rt.compile(ArcStr::from("a::x <- 10")).await?;
    assert_eq!(value_of(&mut rx, tens.exprs[0].id).await?, Value::I64(110));
    ctx.rt.wait_settled().await?;
    let wr = ctx.rt.compile(ArcStr::from("\"[w] [r]\"")).await?;
    assert_eq!(value_of(&mut rx, wr.exprs[0].id).await?, Value::String("11 11".into()));
    ctx.shutdown().await;
    Ok(())
}

/// A write to either binding of an interface pair reaches a reader of the
/// other in an input before the module's: `r` reads the body's `x` through
/// an instance of `g`, `r2` reads `a::x`.
#[tokio::test(flavor = "current_thread")]
async fn a_write_through_an_interface_reaches_an_earlier_input() -> Result<()> {
    use arcstr::{ArcStr, literal};
    use graphix_compiler::expr::{VfsEntry, VfsResolver};
    use graphix_package_core::testing::{TestCtx, config};
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let files = vec![VfsResolver::new(ahash::AHashMap::from_iter([
        (
            netidx_core::path::Path::from("/a.gxi"),
            VfsEntry::from(literal!(
                "val x: i64; val set: fn(v: i64) -> i64; val g: fn(k: i64) -> i64"
            )),
        ),
        (
            netidx_core::path::Path::from("/a.gx"),
            VfsEntry::from(literal!(
                "let x = 0; let set = |v: i64| -> i64 { x <- v; v }; \
                 let g = |k: i64| -> i64 x + k"
            )),
        ),
    ]))];
    let ctx = TestCtx::start(config(tx, crate::TEST_REGISTER, files, |_| {})?).await?;
    let mut held = Vec::new();
    for i in [
        "let h: fn(k: i64) -> i64 = never()",
        "let r = h(1)",
        "let h2: fn(k: i64) -> i64 = never()",
        "let r2 = h2(1)",
        "mod a",
        "h <- a::g",
        "h2 <- |k: i64| -> i64 a::x + k",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    for (write, expected) in
        [(None, "1 1"), (Some("a::x <- 10"), "11 11"), (Some("a::set(20)"), "21 21")]
    {
        if let Some(write) = write {
            held.push(ctx.rt.compile(ArcStr::from(write)).await?);
        }
        ctx.rt.wait_settled().await?;
        let rr = ctx.rt.compile(ArcStr::from("\"[r] [r2]\"")).await?;
        assert_eq!(
            value_of(&mut rx, rr.exprs[0].id).await?,
            Value::String(expected.into())
        );
    }
    ctx.shutdown().await;
    Ok(())
}

/// An alias `let h = f` in a function's body holds `f` while an instance
/// of the function lives. The definition holds nothing: an instance a
/// later input builds after a `<-` to `f` calls what was written.
#[tokio::test(flavor = "current_thread")]
async fn an_alias_holds_its_callee_while_its_instance_lives() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, mut rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _f = ctx
        .rt
        .compile(ArcStr::from(
            "let f = |x: i64| -> i64 x + 1; \
             let g = |x: i64| -> i64 { let h = f; h(x) }",
        ))
        .await?;
    let r = ctx.rt.compile(ArcStr::from("let r = g(1)")).await?;
    let e = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await.unwrap_err();
    assert!(format!("{e:#}").contains("let h = f"), "{e:#}");
    drop(r);
    let _w = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await?;
    let v = ctx.rt.compile(ArcStr::from("g(1)")).await?;
    assert_eq!(value_of(&mut rx, v.exprs[0].id).await?, Value::I64(101));
    ctx.shutdown().await;
    Ok(())
}

const WRITTEN_BINDING_WRITER_FIRST: &str = r#"
{
    let f = |x| x;
    f <- |x| "oops";
    let n = f(41);
    n + 1
}
"#;

run!(
    written_binding_writer_first,
    WRITTEN_BINDING_WRITER_FIRST,
    refused("string does not contain i64");
    FuseExpect::None
);

const WRITTEN_THROUGH_A_REFERENCE: &str = r#"
{
    let f = |x| x;
    let r = &mut f;
    *r <- |x| "oops";
    let n = f(41);
    n + 1
}
"#;

run!(
    written_through_a_reference_is_monomorphic,
    WRITTEN_THROUGH_A_REFERENCE,
    refused("string does not contain i64");
    FuseExpect::None
);

/// A write of the binding's one type is fine.
const WRITTEN_BINDING_TAKES_ITS_TYPE: &str = r#"
{
    let f = |x| x;
    let t = sys::time::timer(duration:50.ms, false);
    f <- t ~ (|x: i64| x + 1);
    skip(#n: 1, f(41))
}
"#;

run!(written_binding_takes_its_type, WRITTEN_BINDING_TAKES_ITS_TYPE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::None);

/// A type error names an inferred cell by its binding, never its id.
const INFERRED_CELL_PRINTS_ITS_BINDING: &str = r#"
{
    let f = |x| x + 1;
    f("a")
}
"#;

run!(inferred_cell_prints_its_binding, INFERRED_CELL_PRINTS_ITS_BINDING, refused("type mismatch i64 does not contain string"); FuseExpect::None);

/// A nested fn type's quantifier is its own, though the enclosing
/// signature names a variable alike.
const NESTED_QUANTIFIER_SCOPED: &str = r#"
{
    let h = |x: 'a, f: fn<'a: Number>(y: 'a) -> 'a| f(f64:2.5);
    let g = |y: i64| y;
    h(1, g)
}
"#;

run!(nested_quantifier_scoped, NESTED_QUANTIFIER_SCOPED, refused("does not contain fn(y: i64) -> i64"); FuseExpect::None);

/// A value of a quantified fn type is called at any type its bound
/// admits, so a monomorphic function is not one, however it is written.
const QUANTIFIED_TYPEDEF_REFUSES_MONO: &str = r#"
{
    type F = fn<'b: Number>(x: 'b) -> 'b;
    let g = |x: i64| -> i64 x + 7;
    let h: F = g;
    h(1.5)
}
"#;

run!(quantified_typedef_refuses_mono, QUANTIFIED_TYPEDEF_REFUSES_MONO, refused("F does not contain fn(x: i64) -> i64"); FuseExpect::None);

const QUANTIFIED_FIELD_REFUSES_MONO: &str = r#"
{
    type T = { f: fn<'c: Number>(c: 'c) -> 'c };
    let g = |x: i64| -> i64 x + 7;
    let t: T = { f: g };
    let h = t.f;
    h(1.5)
}
"#;

run!(quantified_field_refuses_mono, QUANTIFIED_FIELD_REFUSES_MONO, refused("does not contain"); FuseExpect::None);

const QUANTIFIED_TYPEDEF_TAKES_POLY: &str = r#"
{
    type F = fn<'b: Number>(x: 'b) -> 'b;
    let g = 'a: Number |x: 'a| -> 'a x;
    let h: F = g;
    h(1.5)
}
"#;

run!(quantified_typedef_takes_poly, QUANTIFIED_TYPEDEF_TAKES_POLY, is(Value::F64(1.5)));

/// A cast through a typedef whose params grow over one value ends.
const CAST_GROWING_TYPEDEF: &str = r#"
{
    type W<'a> = [Array<W<Array<'a>>>, null];
    let r = cast<W<i64>>("x");
    select r { error as _ => true, _ => false }
}
"#;

run!(cast_growing_typedef, CAST_GROWING_TYPEDEF, is(Value::Bool(true)); FuseExpect::None);

/// A value under one definition nested in itself (`O<O<X>>`) prints by
/// its type, as under one level.
const NESTED_DEFINITION_PRINTS_TYPED: &str = r#"
{
    type O<'a> = ['a, null];
    type S = {a: O<O<List<i64>>>, c: O<O<`Foo(i64)>>, d: O<O<{x: i64}>>};
    let s: S = {a: [<1, 2>], c: `Foo(3), d: {x: 1}};
    "[s]"
}
"#;

run!(nested_definition_prints_typed, NESTED_DEFINITION_PRINTS_TYPED, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s == "{a: [<1, 2>], c: `Foo(3), d: {x: 1}}")
}; FuseExpect::Jit);

/// An abstract value's payload prints by its representation's type.
const ABSTRACT_PAYLOAD_PRINTS_TYPED: &str = r#"
{
    type P = Abstract<{x: i64, y: i64}>;
    type V = Abstract<[`A(i64), `B]>;
    type L = Abstract<List<i64>>;
    let v: V = V(`A(3));
    "[P({x: 1, y: 2})] [v] [L([<1, 2>])]"
}
"#;

run!(abstract_payload_prints_typed, ABSTRACT_PAYLOAD_PRINTS_TYPED, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s == "P({x: 1, y: 2}) V(`A(3)) L([<1, 2>])")
}; FuseExpect::Jit);

/// A bare variant is a marked array at run time, and the primitive
/// `array` holds unmarked arrays only: it does not cover the variant.
const ARRAY_DOES_NOT_COVER_A_BARE_VARIANT: &str = r#"
{
    let f = |x: [`Foo, i64, Array<i64>]| select x { array as _ => 1, i64 as _ => 2 };
    f(3)
}
"#;

run!(array_does_not_cover_a_bare_variant, ARRAY_DOES_NOT_COVER_A_BARE_VARIANT, |v: Result<&Value>| {
    refused("missing match cases")(v)
}; FuseExpect::None);

const BARE_VARIANT_AFTER_ARRAY: &str = r#"
{
    let f = |x: [`Foo, i64, Array<i64>]| select x {
        array as _ => 1,
        i64 as _ => 2,
        `Foo => 3
    };
    (f(3), f([1]), f(`Foo))
}
"#;

run!(bare_variant_after_array, BARE_VARIANT_AFTER_ARRAY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(2), Value::I64(1), Value::I64(3)])
}; FuseExpect::Jit);

/// Two applications of a recursive definition do not merge: L<i64> and
/// L<string> do not cover a mixed list.
const RECURSIVE_APPLICATIONS_DO_NOT_MERGE: &str = r#"
{
    type L<'a> = [`Nil, `Cons('a, L<'a>)];
    type Ints = L<i64>;
    type Strs = L<string>;
    let x: L<[i64, string]> = `Cons(1, `Cons("x", `Nil));
    select x { Ints as _ => 1, Strs as _ => 2 }
}
"#;

run!(recursive_applications_do_not_merge, RECURSIVE_APPLICATIONS_DO_NOT_MERGE, |v: Result<&Value>| {
    refused("missing match cases")(v)
}; FuseExpect::None);

/// A parameter under a collection does not merge either.
const COLLECTION_APPLICATIONS_DO_NOT_MERGE: &str = r#"
{
    type A<'a> = Array<'a>;
    type Ints = A<i64>;
    type Strs = A<string>;
    let x: A<[i64, string]> = [1, "x"];
    select x { Ints as _ => 1, Strs as _ => 2 }
}
"#;

run!(collection_applications_do_not_merge, COLLECTION_APPLICATIONS_DO_NOT_MERGE, |v: Result<&Value>| {
    refused("missing match cases")(v)
}; FuseExpect::None);

/// A union keeps two distinct cells apart: `[a]` and `[b]` are not one
/// array type.
const UNION_KEEPS_DISTINCT_CELLS: &str = r#"
{
    let pick = |c: bool, a, b| select c { true => [a], false => [b] };
    pick(true, "x", 41)[0]$ + 1
}
"#;

run!(union_keeps_distinct_cells, UNION_KEEPS_DISTINCT_CELLS, |v: Result<&Value>| {
    refused("does not contain")(v)
}; FuseExpect::None);

/// Two fn types that differ only in which annotations were written are
/// one member: a select of them is callable.
const FN_MEMBERS_MERGE_THROUGH_BINDINGS: &str = r#"
{
    let g = |x: i64| x * 2;
    let h = |x: i64| -> i64 x + 1;
    let f = select true { true => g, false => h };
    f(1)
}
"#;

run!(
    fn_members_merge_through_bindings,
    FN_MEMBERS_MERGE_THROUGH_BINDINGS,
    |v: Result<&Value>| { matches!(v, Ok(Value::I64(2))) }; FuseExpect::None
);

// A name in an alias's body means what the block finally declares, however
// an earlier pattern destructured the alias.
run!(
    alias_body_names_the_blocks_type,
    r#"{
    type B = string;
    let r = {
        type A1 = B;
        type A2 = B;
        let (u, n): (A1, i64) = never();
        type B = i64;
        let probe: [i64, string] = 1;
        let t1 = select probe { A1 as _ => "A1", _ => "not A1" };
        let t2 = select probe { A2 as _ => "A2", _ => "not A2" };
        "[t1] [t2]"
    };
    r
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "A1 A2")
);

// A literal passed where a union of collection types is expected takes the
// member its elements fit, behind a reference too.
run!(
    literal_into_a_union_formal,
    r#"{
    let f = |x: [Array<i64>, Array<string>]| 1;
    let g = |x: &[Array<i64>, Array<string>]| 10;
    let h = |x: [Array<(i64, i64)>, Array<(string, f64)>]| 100;
    f(["a"]) + g(&["a"]) + h([("a", 1.0)])
}"#,
    is(Value::I64(111)); FuseExpect::Jit
);

// A generic function beside a monomorphic one in a union meets it: the
// select of the two is callable, and the generic stays generic.
run!(
    fn_members_meet_a_generic,
    r#"{
    let b = true;
    let g = |x| x;
    let h = |x: i64| x + 1;
    let f = select b { true => g, false => h };
    "[f(1)] [g("s")]"
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "1 s")
);

// A `?` under a catch raises the operand less its success type; a variant
// whose payload is a union through a call's type variable is that success
// type, whatever order its members print in.
run!(
    raise_through_generic_union_payload,
    r#"{
    let cas = |old: ['v, null]| -> Result<`M(['v, null]), `E> `M(old);
    catch(e) never(e);
    cas(42)?
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a.len() == 2 && a[1] == Value::I64(42))
);

// A primitive cast holds the value exactly or refuses it: an array takes
// no primitive but bytes, a map or a string, nothing converts to null or
// to an error, and a string that reads as an array is no number.
run!(
    primitive_cast_exact_or_refused,
    r#"{
    let x: Any = i64:5;
    let y: Any = [i64:5, "a"];
    let s: Any = "hello";
    let refused = |r: Any| select r { error as _ => true, _ => false };
    let all = [
        refused(cast<i64>([i64:7, 8, 9])),
        refused(cast<u16>([u64:70000])),
        refused(cast<u8>("[-1]")),
        refused(cast<null>(x)),
        refused(cast<bool>([i64:7, 0])),
        refused(cast<[i64, Error<string>]>(s)),
        refused(cast<[i64, error]>(s))
    ];
    (all, cast<[null, Array<string>]>(y)$, cast<bytes>([1, 2])$)
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(v) if v.to_string()
        == r#"[[true, true, true, true, true, true, true], ["5", "a"], bytes:AQI=]"#)
    }
);

// A read is a cast: json and toml data never takes a primitive from an
// array's first element, a number is never null, and a string is never
// an error.
run!(typed_reads_exact_or_refused, r##"{
    let refused = |r: Any| select r { error as _ => true, _ => false };
    let b: Result<i64, [`JsonErr(string), `InvalidCast(string)]> = json::read(r"[[7, 8], 9]");
    let c: Result<{port: i64}, [`TomlErr(string), `InvalidCast(string)]> =
        toml::read(r"port = [8080, 1]");
    let d: Result<null, [`JsonErr(string), `InvalidCast(string)]> = json::read("42");
    let e: Result<{x: [null, i64]}, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(r#"{"x": [1, 2]}"#);
    let f: Result<{a: [i64, Error<string>]}, [`JsonErr(string), `InvalidCast(string)]> =
        json::read(r#"{"a": "oops"}"#);
    [refused(b), refused(c), refused(d), refused(e), refused(f)]
}"##, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[true, true, true, true, true]")
}; FuseExpect::None);

// A typedef whose parameters grow as it recurses has no finite walk: the
// checks over runtime forms give up and refuse, never hang.
run!(growing_typedef_compare_refused, r#"{
    type N<'a> = [null, ('a, N<Array<'a>>)];
    let x: N<i64> = null;
    let y: N<i64> = (1, null);
    x == y
}"#, refused("N grows its parameters as it recurses"); FuseExpect::None);

run!(growing_typedef_map_key_refused, r#"{
    type N<'a> = [null, ('a, N<Array<'a>>)];
    let m: Map<N<i64>, i64> = {};
    m
}"#, refused("a map key type can't hold N"); FuseExpect::None);

run!(growing_typedef_select_refused, r#"{
    type N<'a> = [null, ('a, N<Array<'a>>)];
    let f = |u: [N<i64>, N<string>]| select u { N<i64> as a => 1, N<string> as b => 2 };
    f(null)
}"#, refused("which hold N, whose parameters grow as it recurses"); FuseExpect::None);

// A finite type whose parameters grow a few times is decided as usual.
run!(
    finitely_growing_typedef_compares,
    r#"{
    type A<'a> = [null, ('a, A<Array<i64>>)];
    let x: A<string> = null;
    let y: A<string> = ("a", null);
    x == y
}"#,
    is(Value::Bool(false))
);

// A conjunct that is not exact passes its parts down to the open cells it
// meets, so the check refuses what the build would: a union-bearing bound,
// an inferred Int, and a function bound's argument.
run!(union_bound_reaches_open_cell, r#"{
    let f = 'a: Array<[i64, null]> |x: 'a| -> i64 x[0]$$;
    let g = |y| f([y]);
    g("hello")
}"#, refused("[i64, null] does not contain string"); FuseExpect::None);

run!(
    union_bound_open_cell_accepts,
    r#"{
    let f = 'a: Array<[i64, null]> |x: 'a| -> i64 x[0]$$;
    let g = |y| f([y]);
    g(i64:5)
}"#,
    is(Value::I64(5))
);

run!(inferred_int_reaches_open_cell, r#"{
    let a = [10, 20, 30];
    let f = |i| a[i]$;
    let g = |b, k| f(select b { true => 1, false => k });
    g(false, "s")
}"#, refused("Int does not contain string"); FuseExpect::None);

run!(
    inferred_int_open_cell_accepts,
    r#"{
    let a = [10, 20, 30];
    let f = |i| a[i]$;
    let g = |b, k| f(select b { true => 1, false => k });
    g(false, 2)
}"#,
    is(Value::I64(30))
);

run!(fn_bound_commits_argument, r#"{
    let f = 'a: fn(x: i64, y: [i64, string]) -> i64 |g: 'a| 0;
    f(|x, y| str::len(x))
}"#, refused("string does not contain i64"); FuseExpect::None);

run!(
    fn_bound_unused_arguments_build,
    r#"{
    let f = 'a: fn(x: i64, y: [i64, string]) -> i64 |g: 'a| 0;
    f(|x, y| 0)
}"#,
    is(Value::I64(0))
);

// A rank-2 formal's quantifier takes only an argument whose bounds its own
// imply: the callee was checked against the declared bound alone.
run!(rank2_refuses_narrower_bound, r#"{
    let app = |f: fn<'b: Number>(x: 'b, y: 'b) -> 'b| f(1.5, 2.5);
    let add = 'a: Int |x: 'a, y: 'a| -> 'a x + y;
    app(add)
}"#, refused("does not contain fn<'a: Int + Singleton>"); FuseExpect::None);

// A written fn<'b: C> means the same wherever it stands: a struct field
// and a let annotation hold a value to every 'b the bound admits, as a
// formal does, so a function that needs 'a: Int is refused at the binding.
run!(rank2_struct_field_refuses_narrower_bound, r#"{
    type S = {f: fn<'b: Number>(x: 'b, y: 'b) -> 'b};
    let add = 'a: Int |x: 'a, y: 'a| -> 'a x + y;
    let s: S = {f: add};
    s.f(1.5, 2.5)
}"#, refused("S does not contain { f: fn<'a: Int + Singleton>"); FuseExpect::None);

run!(rank2_let_annotation_refuses_narrower_bound, r#"{
    let add = 'a: Int |x: 'a, y: 'a| -> 'a x + y;
    let f: fn<'b: Number>(x: 'b, y: 'b) -> 'b = add;
    f(1, 2)
}"#, refused("does not contain fn<'a: Int + Singleton>"); FuseExpect::None);

run!(
    rank2_let_annotation_holds_each_instantiation,
    r#"{
    let f: fn<'b: Number + Singleton>(x: 'b, y: 'b) -> 'b = |x, y| x + y;
    (f(1, 2), f(1.5, 2.5))
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(3), Value::F64(4.0)])
);

run!(
    rank2_accepts_implied_bounds,
    r#"{
    let app = |f: fn<'b: Number + Singleton>(x: 'b, y: 'b) -> 'b| f(1.5, 2.5);
    let add = 'a: Number + Singleton |x: 'a, y: 'a| -> 'a x + y;
    let app2 = |f: fn<'b: Int>(x: 'b) -> 'b| f(3);
    let id = 'a: Number |x: 'a| -> 'a x;
    (app(add), app2(id))
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::Array(a)) if a[..] == [Value::F64(4.0), Value::I64(3)])
    }
);

// A quantified function type at a data position of a formal (a field, an
// element, a member) holds its argument as the formal itself would: the
// callee may call it at any type the bound admits.
run!(rank2_field_refuses_mono, r#"{
    let run = |s: {f: fn<'b: Any>(x: 'b) -> 'b}| (s.f(1), s.f("s"));
    let s = {f: |x: i64| x + 1};
    run(s)
}"#, refused("does not contain { f: fn(x: i64) -> i64 }"); FuseExpect::None);

run!(rank2_typedef_field_keeps_its_bound, r#"{
    type S = {f: fn<'b: [i64, string]>(x: 'b) -> 'b};
    let app = |s: S| s.f("a");
    app({f: |x| x * x})
}"#, refused("which the parameter's quantifier does not declare"); FuseExpect::None);

run!(rank2_element_keeps_its_bound, r#"{
    type F = fn<'b: [i64, string]>(x: 'b) -> 'b;
    let app = |gs: Array<F>| array::map(gs, |g| g("a"));
    app([|x| x * x])
}"#, refused("does not contain"); FuseExpect::None);

run!(rank2_member_keeps_its_bound, r#"{
    type F = fn<'b: [i64, string]>(x: 'b) -> 'b;
    let app = |g: [F, null]| select g { null => "none", g => g("a") };
    app(|x| x * x)
}"#, refused("which the parameter's quantifier does not declare"); FuseExpect::None);

run!(rank2_tuple_element_refuses_mono, r#"{
    let run = |s: (fn<'b: Any>(x: 'b) -> 'b, i64)| (s.0(1), s.0("s"));
    run((|x: i64| x + 1, 1))
}"#, refused("does not contain"); FuseExpect::None);

run!(
    rank2_data_positions_take_poly,
    r#"{
    type F = fn<'b: [i64, string]>(x: 'b) -> 'b;
    type S = {f: F};
    let app = |s: S| (s.f("a"), s.f(2));
    let o = |g: [F, null]| select g { null => "none", g => g("b") };
    let t = |s: (fn<'b: Any>(x: 'b) -> 'b, i64)| (s.0(1), s.0("s"), s.1);
    let (a0, a1) = app({f: |x| x});
    let (t0, t1, t2) = t((|x| x, 3));
    "[a0] [a1] [o(|x| x)] [o(null)] [t0] [t1] [t2]"
}"#,
    is(Value::from("a 2 b none 1 s 3"));
    FuseExpect::None
);

// A union of primitives the cell's bound refuses whole holds the cell in
// one of its bits, as a union holds it in one member.
run!(
    primitive_union_holds_bounded_cell,
    r#"{
    let f = 'a: Number |x: 'a| -> 'a x + x;
    let y = never();
    let r: [i64, null] = f(y);
    let z = never();
    let s: [i64, null, Array<i64>] = f(z);
    y <- 3;
    z <- 4;
    (r$, select s { i64 as n => n, _ => 0 })
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:6, i64:8]")
);

run!(primitive_union_no_bit_holds_bound, r#"{
    let f = 'a: Number |x: 'a| -> 'a x + x;
    let y = never();
    let r: [string, null] = f(y);
    y <- 3;
    r
}"#, refused("does not contain"); FuseExpect::None);

// A typedef completed by a later one is refused when that one arrives,
// before a later binding expands it.
run!(forward_alias_refused_before_use, r#"{
    type T = G<T>;
    type G<'a> = 'a;
    let x: T = 1;
    x
}"#, refused("refers back to itself"); FuseExpect::None);

// A bound naming its own quantifier inside a function type is refused
// where it is declared.
run!(self_naming_bound_refused, r#"{
    let f = 'a: fn(x: 'a) -> [string, 'b] |g: 'a| 0;
    f(|x| 0)
}"#, refused("'a's bound names 'a"); FuseExpect::None);

// `_` in a typedef's parameter gets the same hint as in Array<_>.
run!(
    bottom_param_in_a_type_test_hints_any,
    refused("`_` in a type is bottom"),
    "/test.gx" => "type Box<'a> = Abstract<'a>; let x: [Box<i64>, string] = Box(1); let result = select x { Box<_> as b => 1, string as s => 2 }"
; FuseExpect::None);

/// Two REPL inputs each declaring a block-local abstract `T` at one
/// position declare two types: neither input's value passes the other's
/// type test, and a different representation is no redefinition.
#[tokio::test(flavor = "current_thread")]
async fn repl_inputs_declare_distinct_abstract_types() -> anyhow::Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    use graphix_rt::GXEvent;
    use tokio::sync::mpsc;
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _a =
        ctx.rt.compile(ArcStr::from("let a = { type T = Abstract<i64>; T(41) }")).await?;
    let _f = ctx
        .rt
        .compile(ArcStr::from(
            "let f = { type T = Abstract<i64>; |x: Any| select x { T as o => o.0 + 1, _ => 0 } }",
        ))
        .await?;
    let _b = ctx
        .rt
        .compile(ArcStr::from("let b = { type T = Abstract<string>; T(\"x\") }"))
        .await?;
    let probe = ctx.rt.compile(ArcStr::from("f(a)")).await?;
    let id = probe.exprs[0].id;
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        let batch = tokio::time::timeout_at(deadline, rx.recv()).await;
        let Ok(Some(batch)) = batch else { anyhow::bail!("f(a) never produced") };
        for ev in batch.iter() {
            if let GXEvent::Updated(eid, v) = ev
                && *eid == id
            {
                assert_eq!(*v, netidx_value::Value::I64(0));
                return Ok(());
            }
        }
    }
}

// A renaming import of ErrChain is ErrChain: a chain of it is a chain.
run!(
    renamed_errchain_collapses,
    is(Value::I64(0)),
    "/test.gx" => "use core::ErrChain as EC; let f = |e: Error<ErrChain<`A>>| 0; let h = |g: Error<EC<EC<`A>>>| f(g); let result = 0"; FuseExpect::None
);

// An inline literal argument whose part two members of a union formal
// admit is left to its own check wherever the union sits: behind an
// alias, in a tuple, a struct field or a variant payload.
run!(
    union_formal_behind_an_alias,
    is(Value::I64(0)),
    "/test.gx" => "type D = [Array<f64>, Array<string>]; let g = |x: D| 0; let result = g([\"a\"])"; FuseExpect::Jit
);

run!(
    union_formal_in_a_tuple,
    is(Value::I64(0)),
    "/test.gx" => "let g = |x: (i64, [Array<f64>, Array<string>])| 0; let result = g((1, [\"a\"]))"; FuseExpect::Jit
);

run!(
    union_formal_in_a_struct,
    is(Value::I64(0)),
    "/test.gx" => "let g = |x: {a: [Array<f64>, Array<string>]}| 0; let result = g({a: [\"a\"]})"; FuseExpect::Jit
);

run!(
    union_formal_in_a_variant,
    is(Value::I64(0)),
    "/test.gx" => "let g = |x: [`A([Array<f64>, Array<string>]), `B]| 0; let result = g(`A([\"a\"]))"; FuseExpect::Jit
);

run!(
    union_formal_still_refuses_a_mismatch,
    refused("Array<f64> does not contain Array<string>"),
    "/test.gx" => "let g = |x: (i64, Array<f64>)| 0; let result = g((1, [\"a\"]))"
; FuseExpect::None);

// A probe of a union member answers as its commit would: the element's
// bound cell fails 'a: Number in the first member, so the second takes
// the argument and 'q stays free for y.
run!(
    probe_checks_an_open_cells_bounds_against_a_bound_cell,
    is(Value::I64(0)),
    "/test.gx" => r#"
        let f = 'a: Number |x: [(Any, 'q, Array<'a>), (string, string, Array<[string, null]>)], y: 'q| -> i64 0;
        let s = "x";
        let v = [s];
        let result = f(("s", "t", v), 3)
    "#
);

// The member probe holds a declared variable rigid as the commit will:
// the first member's 'a cannot take string, so y stays free for the
// second.
run!(
    rigid_cell_guides_the_member_probe,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
        if a[..] == [Value::String(arcstr::literal!("str")), Value::String(arcstr::literal!("s"))]),
    "/test.gx" => r#"
        let f = |x: 'a, y| -> [(i64, 'a), ([i64, string], string)] (y, "s");
        let result = f(1, "str")
    "#
);

run!(
    rigid_cell_guides_the_member_probe_two,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a))
        if a[..] == [Value::String(arcstr::literal!("str")), Value::String(arcstr::literal!("s"))]),
    "/test.gx" => r#"
        let f = |x: 'a, y| -> [(i64, 'a), (string, string)] (y, "s");
        let result = f(1, "str")
    "#
);

// A union with an open member hands it what no constructor member holds
// as the commit would decide it: Array<'d>, with 'd the declared 'a,
// cannot take Array<i64>, so 'c does.
run!(
    rigid_cell_guides_the_member_placement,
    is(Value::I64(0)),
    "/test.gx" => r#"
        let g = |w: 'd, v: ['c, Array<'d>]| 0;
        let f = |x: 'a, t: bool| -> i64 g(x, select t { true => [1, 2], false => "s" });
        let result = f(1, true)
    "#
);

// A declared variable is a constant to the distribution law: in its own
// body, `P([i64, string], 'a) is covered by `P(i64, 'a) | `P(string, 'a),
// and it still stands for no concrete type.
run!(
    rigid_variable_distributes,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a.len() == 3 && a[1] == Value::I64(1)),
    "/test.gx" => r#"
        type T<'a> = [`P(i64, 'a), `P(string, 'a)];
        let g = |x: 'a, b: bool| -> T<'a> select b { true => `P(1, x), false => `P("s", x) };
        let result = g(1, true)
    "#
);

// A member naming a union lends the law its constructors: `A(i64) in L
// and `A(string) beside it cover `A([i64, string]), as written out.
run!(
    a_named_union_member_distributes,
    prints(r#""`A(0) `X `A(\"s\")""#),
    "/test.gx" => r#"
        type L = [`A(i64), `X];
        let mk = |n: i64| -> [L, `A(string)] select n % 3 { 0 => `A(n), 1 => `X, _ => `A("s") };
        let result = "[mk(0)] [mk(1)] [mk(2)]"
    "#
);

run!(
    a_named_union_member_distributes_a_tuple,
    prints(r#""(0, 0) `Z (2, 0.5)""#),
    "/test.gx" => r#"
        type L = [(i64, i64), `Z];
        let mk = |n: i64| -> [L, (i64, f64)] select n % 3 { 0 => (n, n), 1 => `Z, _ => (n, 0.5) };
        let result = "[mk(0)] [mk(1)] [mk(2)]"
    "#
);

run!(
    written_union_members_distribute,
    prints(r#""`A(0) `X `A(\"s\")""#),
    "/test.gx" => r#"
        let mk = |n: i64| -> [`A(i64), `X, `A(string)] select n % 3 { 0 => `A(n), 1 => `X, _ => `A("s") };
        let result = "[mk(0)] [mk(1)] [mk(2)]"
    "#
);

run!(
    rigid_variable_is_no_concrete_payload,
    refused("does not contain `P([i64, string], 'a: unbound)"),
    "/test.gx" => r#"
        let g = |x: 'a, b: bool| -> [`P(i64, i64), `P(string, i64)] select b { true => `P(1, x), false => `P("s", x) };
        let result = g(1, true)
    "#
; FuseExpect::None);

// Two instances of a nested datatype whose parameters are equal are one
// type, open cells included: a generic function over it checks.
run!(
    nested_datatype_instances_relate_by_parameters,
    is(Value::I64(1)),
    "/test.gx" => r#"
        type N<'a> = [null, ('a, N<Array<'a>>)];
        let n1: N<i64> = (1, ([2], null));
        let g = |n: N<'a>| -> N<'a> n;
        let result = select g(n1) { null => 0, (x, _) => x }
    "#
);

// A union member whose comparison would grow without end is a member that
// does not match; the others still cover the value.
run!(
    a_growing_member_is_a_miss,
    is(Value::Null),
    "/test.gx" => r#"
        type N<'a> = [null, ('a, N<Array<'a>>)];
        let f = |x: N<i64>| -> [N<string>, (Any, Any), null] x;
        let result = f(null)
    "#
; FuseExpect::None);

// A union's normal form does not depend on the order its members are
// written in: f and g take one type, so [f, g] is an array of one fn type.
run!(
    union_member_order_is_not_the_type,
    is(Value::I64(1)),
    "/test.gx" => r#"
        let f = |x: [(i64, string), (f64, string), (i64, bool)]| 0;
        let g = |x: [(i64, string), (i64, bool), (f64, string)]| 1;
        let fs = [f, g];
        let result = array::fold(array::map(fs, |h| h((i64:1, true))), 0, |a, b| a + b)
    "#
);

// Two definitions of one name print alike: the refusal says which is which.
run!(
    same_name_definitions_are_told_apart,
    refused("the two E are different definitions, at line: 2, column: 16 and line: 1, column: 16"),
    "/test.gx" => r#"let f = { type E = `A(string); |x: i64| -> i64 throws Error<ErrChain<E>> select x { 0 => error(`A("one"))?, n => n } };
let g = { type E = `A(i64); |x: i64| -> i64 throws Error<ErrChain<E>> f(x) };
let result = { catch(e) println("[e]"); g(0) }"#
; FuseExpect::None);

// A refusal that names an open cell prints its short name, as every other
// message does, never the cell's full generated id.
run!(an_open_cell_prints_short, r#"
{
    let st = never();
    let g = |x| { st <- x; st };
    let h = |f: fn<'b: Any>(x: 'b) -> 'b| f(1);
    h(g)
}
"#, |v: Result<&Value>| match v {
    Err(e) => {
        let msg = format!("{e:#}");
        msg.contains("is not polymorphic in 'b")
            && msg.contains("a binding it reaches, of type fn(x: '_")
            && cells_print_short(&msg)
    }
    Ok(_) => false,
}; FuseExpect::None);

// A collection instance elaborates under /array; the caller's recursive
// typedef reaches it through the callback's signature, and its inner
// occurrence expands by its seeded cell, not by name there.
run!(instance_expands_caller_recursive_typedef, r#"{
    type T = Array<[null, T]>;
    let a: T = [null];
    any(array::map([a], never<fn(x: T) -> i64>()), [7])
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(7)]); FuseExpect::Jit);

// A declared variable beside a known member is named as the one that may
// differ.
run!(singleton_names_the_declared_variable, r#"{
  let f = 'a: Number |x: 'a, b: bool| { let v = select b { true => x, false => 1 }; v * v };
  f(1, true)
}"#, refused("the declared variable 'a may differ from its other members"); FuseExpect::None);

// A reference test under Any is refused at every place it meets one, the
// second pair of one definition included.
run!(ref_test_over_any_in_a_second_pair, r#"{
  type Box<'a> = {v: 'a};
  let s = "hi";
  let x: (Box<Any>, Box<Any>) = ({v: 1}, {v: &s});
  select x { (Box<i64>, Box<&i64>) as y => *(y.1.v) + 1, _ => 0 }
}"#, refused("can't tell what the reference"); FuseExpect::None);

// A string casts to a decimal or a float exactly: digits past f64 stay,
// a float reads anything Rust's float parse reads, and a pre-epoch
// datetime truncates toward zero as a float does.
run!(string_casts_to_decimal_and_float_exactly, r#"{
  let d1 = cast<decimal>("1.23456789012345678901234567")$;
  let d2 = cast<decimal>("12345678901234567890")$;
  let f1 = cast<f64>("9223372036854775808")$;
  let f2 = cast<f64>("inf")$;
  let f3 = cast<f32>("+3")$;
  let f4 = cast<f64>("-0")$;
  let t1 = cast<i64>(datetime:"1969-12-31T23:59:59.5Z")$;
  let w = json::write_str(d1)$;
  let r: decimal = json::read(w)$;
  "[d1] [d2] [f1] [f2] [f3] [f4] [t1] [r]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "1.23456789012345678901234567 12345678901234567890 9223372036854776000.0 inf 3.0 -0.0 0 1.23456789012345678901234567"); FuseExpect::Jit);

// A string holding a typed literal reads into a float as into an
// integer: what Rust's parse refuses is read as a value, then converted.
run!(typed_strings_cast_to_floats_as_to_integers, r#"{
  let f = cast<f64>("f64:1.5")$;
  let g = cast<f64>("i64:3")$;
  let n = cast<i64>("i64:3")$;
  "[f] [g] [n]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "1.5 3.0 3"); FuseExpect::Jit);

// A union whose members are one type once bound counts that type once:
// `[i64, 'y]` with 'y bound to i64 is a singleton for arithmetic.
run!(
    singleton_counts_a_member_once,
    r#"{
  let h = |x| x * x;
  let k = |b, y| h(select b { true => 1, false => y });
  k(false, 3)
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(9)))
);

// Distributing a tuple over a union does not let a declared variable
// admit what is not it: (`A, 5) is no (`A, 'a).
run!(distribution_holds_declared_variables_rigid, r#"{
  let f = 'a: Any |x: [`A, `B], y: 'a| -> [(`A, 'a), (`B, i64)] (x, 5);
  f(`A, "hello")
}"#, refused("does not contain ([`A, `B], i64)"); FuseExpect::None);

// A lambda literal passed to a rank-2 formal may not narrow the formal's
// quantifier: the callee may call it at any type the declared bound
// admits ("a" here), and `y * y` needs a number.
run!(rank2_literal_may_not_narrow_the_quantifier, r#"{
  let app = |g: fn<'b: [string, i64]>(y: 'b) -> 'b| g("a");
  app(|y| y * y)
}"#, refused("which the parameter's quantifier does not declare"); FuseExpect::None);

run!(rank2_literal_through_an_alias_may_not_narrow, r#"{
  type F = fn<'b: [string, i64]>(y: 'b) -> 'b;
  let app = |g: F| g("a");
  app(|y| y * y)
}"#, refused("which the parameter's quantifier does not declare"); FuseExpect::None);

// A parameter whose quantifier has one structural bound is read through
// it, as every type the bound admits has that structure.
run!(
    reads_through_a_structural_bound,
    prints("[i64:1, i64:2, i64:3, i64:4]"),
    "/test.gx" => r#"
        let idx = 'a: Array<i64> |x: 'a| -> i64 x[0]$;
        let fld = 'a: {x: i64} |s: 'a| -> i64 s.x;
        let tup = 'a: (i64, i64) |t: 'a| -> i64 t.0;
        let call = 'a: fn(x: i64) -> i64 |g: 'a| -> i64 g(1);
        let result = [idx([1]), fld({x: 2}), tup((3, 4)), call(|x| x + 3)]
    "#
);

run!(
    binary_keys_keep_kinds_text_keys_refuse_collisions,
    prints(r#"["written", "refused"]"#),
    "/test.gx" => r#"
        let m: Map<[string, `A], i64> = {"A" => 1, `A => 2};
        let m2: Map<Any, i64> = {[1, 2] => 1, (1, 2) => 2};
        let r1 = select pack::write_bytes(m) { error as _ => "refused", _ => "written" };
        let r2 = select json::write_str(m2) { error as _ => "refused", _ => "written" };
        let result = [r1, r2]
    "#
);

// A cast that merges two keys loses an entry, so it is an InvalidCast.
run!(
    key_merging_map_cast_refused,
    prints(r#"["refused", "ok"]"#),
    "/test.gx" => r#"
        let m = {"1" => "a", "01" => "b"};
        let ok = {"1" => "a", "2" => "b"};
        let c = |v| select cast<Map<i64, string>>(v) { error as _ => "refused", _ => "ok" };
        let result = [c(m), c(ok)]
    "#
);

// A map reads as the array of its (key, value) pairs, as an array of
// pairs reads as a map.
run!(
    map_reads_as_its_pairs,
    prints(r#"[["a", i64:1], ["b", i64:2]]"#),
    "/test.gx" => r#"
        let m = {"a" => 1, "b" => 2};
        let result: Array<(string, i64)> = cast<Array<(string, i64)>>(m)$
    "#
);

// A rank-2 formal passed on is any instance of itself in an instance's
// elaboration as in its definition's check: two uses at two types.
run!(
    rank2_formal_forwarded_twice,
    r#"{
  let app = |a: 'a, k: fn(x: 'a) -> 'a| k(a);
  let f = |g: fn<'b: Number + Singleton>(x: 'b) -> 'b| (app(1.5, g), app(1, g));
  f(|x| x + x)
}"#,
    prints("[f64:3., i64:2]");
    FuseExpect::Jit
);

// A call that returns its rank-2 formal returns it polymorphic: an
// annotation on the result picks an instance and leaves the formal alone.
run!(
    rank2_formal_returned_then_annotated,
    r#"{
  let app = |g: fn<'b: Any>(y: 'b) -> 'b| g;
  let h: fn(y: i64) -> i64 = app(|x| x);
  h(1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// A written rank-2 element is any instance of itself from where it is
// written: an annotation picking one instance decides nothing for a later
// use, whichever comes first.
run!(
    rank2_element_annotated_then_used_again,
    prints("[i64:3, [f64:4.]]"),
    "/test.gx" => r#"
        let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
        let fs: Array<fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a> = [f];
        let h: fn(x: i64, y: i64) -> i64 = fs[0]$;
        let result = (h(1, 2), array::map(fs, |g| g(1.5, 2.5)))
    "#;
    FuseExpect::None
);

// A rank-2 element passed to a rank-2 formal is polymorphic in it, as a
// let annotated with the element's type passes it, and so is an array
// literal of lambdas passed to a rank-2 element type: the cells a
// literal or an index makes live only in the argument.
run!(
    rank2_element_passed_to_a_rank2_formal,
    r#"{
  type T = fn<'a: Any>(x: 'a) -> 'a;
  let fs: Array<T> = [|x| x];
  let f = |k: fn<'b: Any>(x: 'b) -> 'b| (k(1), k("s"));
  let e: T = fs[0]$;
  let each = |ks: Array<T>| array::map(ks, |k| f(k));
  (f(fs[0]$), f(e), array::map(fs, |k| f(k)), each([|x| x]))
}"#,
    prints(r#"[[i64:1, "s"], [i64:1, "s"], [[i64:1, "s"]], [[i64:1, "s"]]]"#);
    FuseExpect::None
);

// A written quantified function type holds a value general in it wherever
// it stands: lambdas, an element read from a rank-2 array and lambda
// literals in data, through a let, a write, a struct update, a return type
// and a labeled default, and an array literal passed to a rank-2 element.
run!(
    rank2_written_holders_take_general_values,
    r#"{
  type T = fn<'a: Any>(x: 'a) -> 'a;
  let app = |k: T| (k(1), k("s"));
  let fs: Array<T> = [|x| x];
  let e: T = fs[0]$;
  let s: {f: T, n: i64} = {f: |x| x, n: 1};
  let t = {s with f: |x| x};
  let h: T = |x| x;
  h <- |x| x;
  let r = &mut h;
  *r <- |x| x;
  let mk = |z: i64| -> T |x| x;
  let g = |#f: T = |x| x, z: i64| f(z);
  let each = |ks: Array<T>| array::map(ks, |k| app(k));
  (app(e), app(t.f), app(h), app(mk(0)), g(2), each([|x| x]), s.n)
}"#,
    prints(r#"[[i64:1, "s"], [i64:1, "s"], [i64:1, "s"], [i64:1, "s"], i64:2, [[i64:1, "s"]], i64:1]"#);
    FuseExpect::None
);

// A quantified function type in a function type's result, or an abstract
// type's payload, holds a value general in it: a let, a call's formal, a
// quantified outer type, a struct result, a written return type, a labeled
// default and a constructor each take one.
run!(
    rank2_result_holders_take_general_values,
    r#"{
  let mk: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a = |z| |x| x;
  let h = mk(0);
  let app = |m: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a| { let k = m(0); (k(2), k("t")) };
  let mq: fn<'q: Any>(z: 'q) -> fn<'a: Any>(x: 'a) -> ('a, 'q) = |z| |x| (x, z);
  let g = mq(5);
  let ms: fn(z: i64) -> {f: fn<'a: Any>(x: 'a) -> 'a, n: i64} = |z| {f: |x| x, n: z};
  let r = ms(7);
  let mr = |q: i64| -> fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a |z| |x| x;
  let hh = mr(0)(1);
  let gd = |#m: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a = |z| |x| x, q: i64| {
    let k = m(q);
    (k(4), k("x"))
  };
  type W = Abstract<fn<'a: Any>(x: 'a) -> 'a>;
  let w = W(|x| x);
  let wf = w.0;
  type B<'t> = Abstract<'t>;
  let b: B<fn<'a: Any>(x: 'a) -> 'a> = B(|x| x);
  let bf = b.0;
  let t: Transparent<fn<'a: Any>(x: 'a) -> 'a> = Transparent(|x| x);
  let tf = t.0;
  (
    (h(1), h("s")),
    app(|z| |x| x),
    (g(1), g("u")),
    (r.f(1), r.f("v"), r.n),
    (hh(3), hh("w")),
    gd(0),
    (wf(1), wf("y")),
    (bf(1), bf("z"), tf(2), tf("q"))
  )
}"#,
    prints(
        r#"[[i64:1, "s"], [i64:2, "t"], [[i64:1, i64:5], ["u", i64:5]], [i64:1, "v", i64:7], [i64:3, "w"], [i64:4, "x"], [i64:1, "y"], [i64:1, "z", i64:2, "q"]]"#
    );
    FuseExpect::None
);

// A quantified function a constructor holds keeps its own quantifiers: a
// value of its type reading that holder is general in them, through an
// abstract type, Transparent, a struct and a recursive binding alike.
run!(
    rank2_holder_of_the_function_itself_is_general,
    r#"{
  type W<'t> = Abstract<'t>;
  let f: fn<'a: Any>(x: 'a) -> 'a = |x| x;
  let y = W(f);
  f <- |x| { let q = y; x };
  let g: fn<'a: Any>(x: 'a) -> 'a = |x| x;
  let z = Transparent(g);
  g <- |x| { let q = z; x };
  let s: {f: fn<'a: Any>(x: 'a) -> 'a} = {f: |x| x};
  let w = W(s);
  s <- {f: |x| { let q = w; x }};
  let u = never();
  let rec k: fn<'a: Any>(x: 'a) -> 'a = |x| { u <- W(k); x };
  (f(1), f("s"), g(2), g("t"), s.f(3), s.f("v"), k(4), k("w"))
}"#,
    prints(r#"[i64:1, "s", i64:2, "t", i64:3, "v", i64:4, "w"]"#);
    FuseExpect::None
);

// A call's variable a rank-2 argument decides without the formal's
// quantifier, and a quantifier its formal names where the call's variables
// do not stand, take the argument, written, omitted or hidden.
run!(
    rank2_call_signature_takes_general_arguments,
    r#"{
  type W<'t> = Abstract<'t>;
  let outer = |#k: fn<'a: Any>(x: 'a) -> W<'c>, n: i64| (k(n), k("s"));
  let r = outer(#k: |x| W(2), 1);
  let w: W<i64> = r.1;
  let same = |#k: fn<'a: Any>(x: 'a) -> W<'a> = |x| W(x), n: i64| (k(n), k("s"));
  let s = same(3);
  let t = same(#k: |x| W(x), 4);
  let id = |#k: fn<'a: Any>(x: 'a) -> 'a = |x| x, n: i64| (k(n), k("u"));
  let app = |g: fn(n: i64) -> 'q| g(7);
  (w.0, s.0.0, s.1.0, t.0.0, id(5), id(#k: |x| x, 6), app(id))
}"#,
    prints(r#"[i64:2, i64:3, "s", i64:4, [i64:5, "u"], [i64:6, "u"], [i64:7, "u"]]"#);
    FuseExpect::Jit
);

// A call's variable a later argument decides is still one type for the
// call: an argument that would make it the formal's quantifier is refused,
// naming the annotation that gives it a type of its own.
run!(
    rank2_call_signature_refusal_names_the_annotation,
    r#"{
  let z: Any = 0;
  let outer = |#k: fn<'a: Any>(x: 'a) -> 'c, d: 'c| (k(1), k("s"), d);
  outer(#k: |x| x, z)
}"#,
    refused("one type for the call; annotate the argument with the type 'c is meant to be");
    FuseExpect::None
);

run!(
    rank2_call_signature_annotated_argument_runs,
    r#"{
  let z: Any = 0;
  let outer = |#k: fn<'a: Any>(x: 'a) -> 'c, d: 'c| (k(1), k("s"), d);
  outer(#k: |x| -> Any x, z)
}"#,
    prints(r#"[i64:1, "s", i64:0]"#);
    FuseExpect::None
);

// A value written to a rank-2 holder as its copy may hand its parameter
// to a callback whose result stays inside it, and write an outer binding
// a callback decides without the parameter.
run!(
    rank2_copied_value_keeps_its_callbacks_inside,
    r#"{
  type W<'t> = Abstract<'t>;
  let y = never();
  let f: fn<'a: Any>(x: 'a) -> 'a = |x| x;
  f <- |x| { let ws = array::map([x], |e| W(e)); y <- array::map([2], |e| W(e)); x };
  let s: {g: fn<'a: Any>(x: 'a) -> 'a} = {g: |x| x};
  let rs = &mut s.g;
  *rs <- |x| { let ws = list::map([<x>], |e| W(e)); x };
  let n = select y { [w, ..] => w.0 + 1, _ => 0 };
  (f(1), f("s"), s.g(3), s.g("t"), n)
}"#,
    prints(r#"[i64:1, "s", i64:3, "t", i64:3]"#);
    FuseExpect::Jit
);

// A payload that bounds an enclosing definition's variable makes the
// parameter that variable, which a call of the enclosing variable's own
// type passes: through an abstract type, Transparent and a declared
// variable alike.
run!(
    rigid_below_takes_the_enclosing_variable,
    r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let inner = |v| { let q: W<'c> = W(v); v };
    inner(z.0)
  };
  let t = |z: Transparent<'c>| {
    let k = |v| { let q: Transparent<'c> = Transparent(v); v };
    k(z.0)
  };
  let d = 'c: Any |z: W<'c>| {
    let h: fn(x: 'c) -> 'c = |x| { let q: W<'c> = W(x); x };
    h(z.0)
  };
  (outer(W(1)), outer(W("s")), t(Transparent(2)), d(W(true)))
}"#,
    prints(r#"[i64:1, "s", i64:2, true]"#);
    FuseExpect::Jit
);

// A module holding a written rank-2 typedef of its parent's: its value is
// held to it, the escape refused as at the top level.
run!(
    rank2_module_let_writes_outer,
    refused("the value is not polymorphic in 'a"),
    "/test.gx" => r#"
type T = fn<'a: Any>(x: 'a) -> 'a;
mod m;
let result = m::r
"#,
    "/test/m.gxi" => r#"
val r: (i64, string, i64);
"#,
    "/test/m.gx" => r#"
use super::T;
let y = never();
let h: T = |x| { y <- x; x };
let g: fn(x: string) -> string = h;
let r = (h(1), g("s"), y + 1)
"#
    ; FuseExpect::None);

// A `let` annotated with a written rank-2 type holds a function that is
// any instance of itself: each annotated use picks its own.
run!(
    rank2_let_used_at_two_types,
    prints("[i64:3, f64:4.]"),
    "/test.gx" => r#"
        let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;
        let g: fn<'a: Number + Singleton>(x: 'a, y: 'a) -> 'a = f;
        let h: fn(x: i64, y: i64) -> i64 = g;
        let k: fn(x: f64, y: f64) -> f64 = g;
        let result = (h(1, 2), k(1.5, 2.5))
    "#;
    FuseExpect::Jit
);

// A rank-2 formal bounded Data takes only a generic handler; a
// monomorphic one is refused as no instance of it, not as a function
// that is not data.
run!(
    rank2_data_formal_refuses_a_monomorphic_handler,
    r#"{
  let f = |#h: fn<'a: Data>(v: 'a) -> i64, x: i64| x;
  f(#h: |v: (i64, string)| v.0, 1)
}"#,
    refused("does not contain");
    FuseExpect::None
);

// A Data cell under a formal's argument judges what the function is
// called with: a handler refused for its result is a type mismatch.
run!(
    data_argument_cell_does_not_blame_the_handler,
    r#"{
  let apply = 'a: Data |f: [fn(v: 'a) -> _, null], v: 'a| v;
  let h = |v: i64| v;
  apply(h, 1)
}"#,
    refused("does not contain fn(v: i64) -> i64");
    FuseExpect::None
);

// What no member of a union covers goes to an open member that takes it,
// whatever the order the union is written in: a rigid one takes only
// itself.
run!(
    union_residue_skips_a_rigid_member_primitives,
    r#"{
  let f = 'r: Any |x: 'r, b: bool| -> i64 {
    let v: ['r, 'q] = select b { true => 1, false => "s" };
    0
  };
  f(true, false)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    union_residue_skips_a_rigid_member,
    r#"{
  let f = 'r: Any |x: 'r, b: bool| -> i64 {
    let v: ['r, 'q] = select b { true => 1, false => ["s"] };
    0
  };
  f(true, false)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// A type name means one thing throughout its statement list: a name a
// pattern binds at it means what a later statement of the block declares,
// and a pattern that takes it apart may not name it before it.
run!(
    a_pattern_binds_at_the_blocks_final_type,
    r#"{
    type Foo = string;
    let r = { let a: Foo = 42; type Foo = i64; a };
    r + 1
}"#,
    is(Value::I64(43))
);

run!(
    a_pattern_binds_at_no_type_a_later_statement_shadows,
    r#"{
    type Foo = string;
    let r = { let a: Foo = 42; type Foo = i64; a };
    str::len(r)
}"#,
    refused("string does not contain");
    FuseExpect::None
);

run!(
    a_destructure_names_no_type_a_later_statement_declares,
    r#"{
    type P = (string, string);
    let r = { let (a, b): P = (1, 2); type P = (i64, i64); a };
    r
}"#,
    refused("P names the type a later statement of this block declares");
    FuseExpect::None
);

run!(
    a_pattern_names_the_types_declared_before_it,
    r#"{
    type Foo = string;
    let r = { type Foo = i64; let a: Foo = 42; a };
    let s = { let b: Foo = "x"; let q = { type Foo = i64; let c: Foo = 1; c }; type Bar = i64; (b, q) };
    (r + 1, s)
}"#,
    prints(r#"[i64:43, ["x", i64:1]]"#)
);

// A written type applies each name it holds to the definition's
// parameters, one each, every one that holds no open cell within its
// bound: refused at the annotation, whether anything expands it or not.
run!(written_name_short_of_parameters_refused, r#"{
    type P<'a> = ('a, 'a);
    let bad: [P, null] = null;
    bad
}"#, refused("P expects 1 type parameters"); FuseExpect::None);

run!(typedef_body_with_extra_parameters_refused, r#"{
    type P<'a> = ('a, 'a);
    type Q = [P<i64, string, bool>, null];
    let bad: Q = null;
    bad
}"#, refused("P expects 1 type parameters"); FuseExpect::None);

run!(written_parameter_outside_its_bound_refused, r#"{
    type B<'a: Number> = ('a, 'a);
    let bad: [B<string>, null] = null;
    bad
}"#, refused("type mismatch Number does not contain string"); FuseExpect::None);

run!(
    decimal_plain_print_is_unadorned,
    r#"{
    let d = decimal:12345678901234567890.;
    ["[d]" == "12345678901234567890", cast<string>(d)$ == "12345678901234567890"]
}"#,
    prints("[true, true]")
);

// A variant reads only from data that names its tag: another variant's
// written form, or an untagged array of the payload's length, is no value
// of it (x-kind-markers-03).
run!(
    a_variant_reads_only_its_own_tag,
    r#"{
  let other: Result<`A(string, i64), [`InvalidCast(string), `JsonErr(string)]> =
    json::read(json::write_str(`B(2))$);
  let bare: Result<[`A(i64, i64), `B(i64, i64)], [`InvalidCast(string), `JsonErr(string)]> =
    json::read(r"[1, 2]");
  let own: Result<`A(string, i64), [`InvalidCast(string), `JsonErr(string)]> =
    json::read(json::write_str(`A("x", 2))$);
  (select other { error as _ => "refused", _ => "read" },
   select bare { error as _ => "refused", _ => "read" },
   own$)
}"#,
    prints(r#"["refused", "refused", ["A", "x", i64:2]]"#);
    FuseExpect::Jit
);

// A union member that holds a value without converting its elements comes
// before one that would: [1, 2] reads as Array<i64>, not as (1, "2"), and
// as List<i64>, not as ["1", "2"] (t-cast-setops-06, x-rerun-5-03).
run!(
    a_union_prefers_the_member_its_elements_fit,
    r#"{
  let a: Any = [1, 2];
  let t = cast<[(i64, string), Array<i64>]>(a)$;
  let l = cast<[List<i64>, Array<string>]>([1, 2])$;
  (select t { Array<i64> as _ => "array", (i64, string) as _ => "tuple" },
   select l { List<i64> as _ => "list", Array<string> as _ => "strings" })
}"#,
    prints(r#"["array", "list"]"#);
    FuseExpect::Jit
);

// A bound naming another quantifier ('d: [null, 'c]) puts what 'd takes
// below 'c, so 'c bound later to a type without it is refused.
run!(
    quantifier_bound_holds_a_later_binding,
    r#"{
        let g = 'c: Any, 'd: [null, 'c] |y: 'd, x: 'c| -> [null, 'c] y;
        let f = 'a: Any |x: 'a| -> [null, 'a] g(5, x);
        f("s")
    }"#,
    refused("above i64 does not contain 'a")
);

run!(
    quantifier_bound_admits_a_binding_it_holds,
    r#"{
        let g = 'c: Any, 'd: [null, 'c] |y: 'd, x: 'c| -> [null, 'c] y;
        g(5, 7)
    }"#,
    is(Value::I64(5))
);

// Under Any, numbers of different types order by value (ties by type, so
// one value of two types is never Equal and the order agrees with ==):
// a sort of mixed numbers read from JSON is by value, and so is `<`.
run!(
    any_mixed_numbers_order_by_value,
    r#"{
  let xs: Result<Array<Any>, [`JsonErr(string), `InvalidCast(string)]> =
    json::read(r"[3, 2.5, 1, 0.5]");
  let a: Any = i64:3;
  let b: Any = f64:2.5;
  (array::sort(xs$), a < b, a > b)
}"#,
    prints("[[f64:0.5, i64:1, f64:2.5, i64:3], false, true]");
    FuseExpect::None
);

// A float against a decimal is compared as the binary fraction it is:
// f64 0.1 is 0.1000000000000000055511151231257827..., above the decimal
// that rounds to it, and both sorts agree.
run!(
    any_float_and_decimal_order_exactly,
    r#"{
  let a: Any = f64:0.1;
  let b: Any = decimal:0.1000000000000000055511151231;
  (a < b, a > b, array::sort([a, b]), array::sort(#numeric: true, [a, b]))
}"#,
    prints("[false, true, [decimal:0.1000000000000000055511151231, f64:0.1], [decimal:0.1000000000000000055511151231, f64:0.1]]");
    FuseExpect::None
);

// A typedef's expansion is capture-avoiding: a function type's quantifiers
// shadow the typedef's parameters, and one named like a variable its
// argument holds is renamed, so `K<'a>` below means what `K<'a>` over the
// body with its quantifier renamed `'b` means, quantifier by quantifier.
const TYPEDEF_QUANTIFIER_NAMED_LIKE_ITS_ARGUMENT: &str = r#"{
  type K<'p> = fn<'a: Any>(x: 'p, y: 'a) -> 'a;
  let f = |v: 'a| {
    let g: K<'a> = |x, y| y;
    (g(v, 1), g(v, "s"))
  };
  f(true)
}"#;

run!(
    typedef_quantifier_named_like_its_argument,
    TYPEDEF_QUANTIFIER_NAMED_LIKE_ITS_ARGUMENT,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    typedef_quantifier_named_apart_from_its_argument,
    TYPEDEF_QUANTIFIER_NAMED_LIKE_ITS_ARGUMENT.replace("'a: Any>(x: 'p, y: 'a) -> 'a", "'b: Any>(x: 'p, y: 'b) -> 'b"),
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

const TYPEDEF_ARGUMENT_CALLED_AT_TWO_TYPES: &str = r#"{
  type K<'p> = fn<'a: Any>(x: 'p) -> 'a;
  let f = |v: 'a| {
    let g: K<'a> = |x| never();
    let p = g(1);
    let q = g("s");
    v
  };
  f(true)
}"#;

run!(
    typedef_argument_named_like_a_quantifier_stays_one_type,
    TYPEDEF_ARGUMENT_CALLED_AT_TWO_TYPES,
    refused("'a: unbound does not contain i64");
    FuseExpect::None
);

run!(
    typedef_argument_named_apart_from_a_quantifier_stays_one_type,
    TYPEDEF_ARGUMENT_CALLED_AT_TWO_TYPES.replace("'a: Any>(x: 'p) -> 'a", "'b: Any>(x: 'p) -> 'b"),
    refused("'a: unbound does not contain i64");
    FuseExpect::None
);

// A renamed quantifier is renamed in a sibling's bound too.
const TYPEDEF_QUANTIFIER_NAMED_IN_A_SIBLINGS_BOUND: &str = r#"{
  type K<'p> = fn<'a: Any, 'b: ['a, null]>(x: 'p, y: 'a, z: 'b) -> 'a;
  let f = |v: 'a| {
    let g: K<'a> = |x, y, z| y;
    (g(v, 1, null), g(v, "s", "t"))
  };
  f(true)
}"#;

run!(
    typedef_quantifier_named_like_its_argument_in_a_siblings_bound,
    TYPEDEF_QUANTIFIER_NAMED_IN_A_SIBLINGS_BOUND,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    typedef_quantifier_named_apart_from_its_argument_in_a_siblings_bound,
    TYPEDEF_QUANTIFIER_NAMED_IN_A_SIBLINGS_BOUND.replace("fn<'a: Any, 'b: ['a, null]>(x: 'p, y: 'a, z: 'b) -> 'a", "fn<'q: Any, 'b: ['q, null]>(x: 'p, y: 'q, z: 'b) -> 'q"),
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

const TYPEDEF_ARGUMENT_ESCAPES_A_RANK2_ARGUMENT: &str = r#"{
  type K<'p> = fn<'a: Any>(x: 'p) -> 'a;
  let mk = |v: 'a| -> K<'a> |x| never();
  let y = never();
  let outer = |k: fn<'a: Any>(x: 'a) -> 'a| (k(1), k("s"));
  let r = outer(|x| { y <- mk(x); x });
  let z: i64 = y("s");
  let w: i64 = y(2);
  r
}"#;

run!(
    typedef_argument_named_like_a_quantifier_escapes,
    TYPEDEF_ARGUMENT_ESCAPES_A_RANK2_ARGUMENT,
    refused("the argument is not polymorphic in 'a: a binding it reaches");
    FuseExpect::None
);

run!(
    typedef_argument_named_apart_from_a_quantifier_escapes,
    TYPEDEF_ARGUMENT_ESCAPES_A_RANK2_ARGUMENT.replace("type K<'p> = fn<'a: Any>(x: 'p) -> 'a", "type K<'p> = fn<'b: Any>(x: 'p) -> 'b"),
    refused("the argument is not polymorphic in 'a: a binding it reaches");
    FuseExpect::None
);

// An argument that is itself a quantified function type keeps its own
// quantifier beside the body's of the same name.
run!(
    typedef_argument_quantified_like_the_body,
    r#"{
  type K<'p> = fn<'a: Any>(k: 'p, y: 'a) -> ('a, i64, string);
  let g: K<fn<'a: Any>(z: 'a) -> 'a> = |k: fn<'a: Any>(z: 'a) -> 'a, y| (y, k(1), k("s"));
  (g(|z| z, true), g(|z| z, 2))
}"#,
    prints(r#"[[true, i64:1, "s"], [i64:2, i64:1, "s"]]"#);
    FuseExpect::Jit
);

// A trait method's signature substituted for an impl, and named as a
// value, keeps a function type's quantifier apart from the variable of
// its name the substitution brings in.
const IMPL_SIGNATURE_QUANTIFIER_NAMED_LIKE_ITS_VARIABLE: &str = r#"{
  trait T { val m: fn(self, f: fn<'a: Any>(x: 'a, s: self) -> 'a) -> (i64, string) };
  impl<'a> T for Array<'a> { let m = |s, f| (f(1, s), f("s", s)) };
  T::m([1], |x, s| x)
}"#;

run!(
    impl_signature_quantifier_named_like_its_variable,
    IMPL_SIGNATURE_QUANTIFIER_NAMED_LIKE_ITS_VARIABLE,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    impl_signature_quantifier_named_apart_from_its_variable,
    IMPL_SIGNATURE_QUANTIFIER_NAMED_LIKE_ITS_VARIABLE.replace("fn<'a: Any>(x: 'a, s: self) -> 'a", "fn<'q: Any>(x: 'q, s: self) -> 'q"),
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

const METHOD_VALUE_QUANTIFIER_NAMED_LIKE_ITS_VARIABLE: &str = r#"{
  trait T { val m: fn(self, x: 'a, f: fn<'a: Any>(y: 'a) -> 'a) -> (i64, string, 'a) };
  impl T for i64 { let m = |s, x, f| (f(s), f("s"), x) };
  let g = T::m;
  g(1, true, |y| y)
}"#;

run!(
    method_value_quantifier_named_like_its_variable,
    METHOD_VALUE_QUANTIFIER_NAMED_LIKE_ITS_VARIABLE,
    prints(r#"[i64:1, "s", true]"#);
    FuseExpect::Jit
);

run!(
    method_value_quantifier_named_apart_from_its_variable,
    METHOD_VALUE_QUANTIFIER_NAMED_LIKE_ITS_VARIABLE.replace("fn<'a: Any>(y: 'a) -> 'a", "fn<'q: Any>(y: 'q) -> 'q"),
    prints(r#"[i64:1, "s", true]"#);
    FuseExpect::Jit
);

// A function type's quantifier shadows the typedef parameter of its name.
const TYPEDEF_PARAMETER_SHADOWED_BY_A_QUANTIFIER: &str = r#"{
  type K<'a> = (Array<'a>, fn<'a: Any>(x: 'a) -> 'a);
  let p: K<i64> = ([1], |x| x);
  let g = p.1;
  (p.0, g(1), g("s"))
}"#;

run!(
    typedef_parameter_shadowed_by_a_quantifier,
    TYPEDEF_PARAMETER_SHADOWED_BY_A_QUANTIFIER,
    prints(r#"[[i64:1], i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    typedef_parameter_shadowed_by_a_quantifier_takes_no_monomorphic_value,
    r#"{
  type K<'a> = (Array<'a>, fn<'a: Any>(x: 'a) -> 'a);
  let p: K<i64> = ([1], |x| x + 1);
  p.0
}"#,
    refused("K<i64> does not contain (Array<i64>, fn(x: i64) -> i64)");
    FuseExpect::None
);

// A rank-2 quantifier's bound names only written types and the
// quantifiers of its own function type: one naming a variable the type
// does not quantify, which the other positions or the calls decide, is
// refused wherever the type is written. Bound it by a written type.
const RANK2_BOUND_NAMES_THE_DEFINITIONS_VARIABLE: &str = r#"{
  let outer = |#k: fn<'a: 'c>(x: 'a) -> 'a, z: 'c| (k(z), z);
  outer(#k: |x| x, 1)
}"#;

const RANK2_UNQUANTIFIED: &str =
    "which this function type does not quantify: bound it by a written type";

run!(
    rank2_bound_naming_the_definitions_variable_is_refused,
    RANK2_BOUND_NAMES_THE_DEFINITIONS_VARIABLE,
    refused("a rank-2 quantifier's bound names 'c, which this function type does not quantify: bound it by a written type");
    FuseExpect::None
);

run!(
    rank2_bound_naming_the_definitions_variable_is_refused_uncalled,
    RANK2_BOUND_NAMES_THE_DEFINITIONS_VARIABLE.replace("(k(z), z)", "k(z)"),
    refused(RANK2_UNQUANTIFIED);
    FuseExpect::None
);

run!(
    rank2_bound_naming_the_definitions_variable_is_refused_unread,
    RANK2_BOUND_NAMES_THE_DEFINITIONS_VARIABLE.replace("(k(z), z)", "z"),
    refused(RANK2_UNQUANTIFIED);
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_declared_variable_is_refused,
    RANK2_BOUND_NAMES_THE_DEFINITIONS_VARIABLE.replace("let outer = |", "let outer = 'c: Any |"),
    refused(RANK2_UNQUANTIFIED);
    FuseExpect::None
);

run!(
    rank2_bound_by_a_written_type_runs,
    RANK2_BOUND_NAMES_THE_DEFINITIONS_VARIABLE.replace("fn<'a: 'c>(x: 'a) -> 'a, z: 'c", "fn<'a: i64>(x: 'a) -> 'a, z: i64"),
    prints("[i64:1, i64:1]");
    FuseExpect::Jit
);

run!(
    rank2_bound_naming_the_variable_in_a_union_is_refused,
    r#"{
  let outer = |#k: fn<'a: ['c, null]>(x: 'a) -> 'a, z: 'c| (k(z), k(null), z);
  outer(#k: |x| x, 1)
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_the_variable_in_a_constructor_is_refused,
    r#"{
  let outer = |#k: fn<'a: Array<'c>>(x: 'a) -> 'a, z: 'c| (k([z]), z);
  outer(#k: |x| x, 1)
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_variable_decided_first_is_refused,
    r#"{
  let outer = |z: 'c, k: fn<'a: ['c, null]>(x: 'a) -> 'a| (k(z), z);
  let f: fn<'b: [i64, null]>(x: 'b) -> 'b = |x| x;
  outer(1, f)
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_variable_beside_a_union_is_refused,
    r#"{
  let outer = |w: 'e, u: ['c, 'd], k: fn<'a: 'e>(x: 'a) -> 'c| (u, k(w));
  let r: ([i64, string], i64) = outer(5, 1, |x| 7);
  r
}"#,
    refused("bound names 'e, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_variable_in_a_written_alias_is_refused,
    r#"{
  let outer = |w: 'e, u: ['c, 'd], k: fn<'a: 'e>(x: 'a) -> 'c| (u, k(w));
  let g: fn(w: i64, u: [i64, string], k: fn<'a: i64>(x: 'a) -> i64) -> ([i64, string], i64) = outer;
  g(5, 1, |x| 7)
}"#,
    refused("bound names 'e, which");
    FuseExpect::None
);

const RANK2_BOUND_IN_A_UNION_MEMBER: &str = r#"{
  let outer = |k: [fn<'a: 'c>(x: 'a) -> string, fn<'a: 'd>(x: 'a) -> 'a], z: 'c| z;
  let f: fn<'b: i64>(x: 'b) -> 'b = |x| x;
  outer(f, "s")
}"#;

run!(
    rank2_bound_in_a_union_member_is_refused,
    RANK2_BOUND_IN_A_UNION_MEMBER,
    refused(RANK2_UNQUANTIFIED);
    FuseExpect::None
);

run!(
    rank2_bound_in_a_union_member_written_second_is_refused,
    RANK2_BOUND_IN_A_UNION_MEMBER.replace("[fn<'a: 'c>(x: 'a) -> string, fn<'a: 'd>(x: 'a) -> 'a]", "[fn<'a: 'd>(x: 'a) -> 'a, fn<'a: 'c>(x: 'a) -> string]"),
    refused(RANK2_UNQUANTIFIED);
    FuseExpect::None
);

run!(
    rank2_bound_in_a_union_member_beside_a_misfit_is_refused,
    r#"{
  let outer = |k: [fn<'a: 'c>(x: 'a) -> i64, fn<'a: 'd>(x: 'a) -> 'a], z: 'c| z;
  let f: fn<'b: string>(x: 'b) -> 'b = |x| x;
  outer(f, 1)
}"#,
    refused(RANK2_UNQUANTIFIED);
    FuseExpect::None
);

// A bound naming a sibling quantifier stays; the formal's other bound
// names the definition's variable.
const RANK2_BOUND_BESIDE_A_SIBLING_BOUND: &str = r#"{
  let outer = |k: fn<'p: Any, 'a: 'c>(x: 'p, y: 'a) -> ['p, null], z: 'c| k("str", z);
  let f: fn<'q: Any, 'b: ['q, null]>(x: 'q, y: 'b) -> ['q, null] = |x, y| y;
  outer(f, 1)
}"#;

run!(
    rank2_bound_beside_a_sibling_bound_is_refused,
    RANK2_BOUND_BESIDE_A_SIBLING_BOUND,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_beside_a_sibling_bound_is_refused_at_another_type,
    RANK2_BOUND_BESIDE_A_SIBLING_BOUND.replace(r#"k("str", z)"#, "k(1, z)").replace("outer(f, 1)", r#"outer(f, "s")"#),
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_beside_a_bare_sibling_bound_is_refused,
    RANK2_BOUND_BESIDE_A_SIBLING_BOUND
        .replace(r#"k("str", z)"#, "k(1, z)")
        .replace("fn<'q: Any, 'b: ['q, null]>", "fn<'q: Any, 'b: 'q>")
        .replace("outer(f, 1)", r#"outer(f, "s")"#),
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_sibling_quantifier_runs,
    r#"{
  let f: fn<'q: Any, 'b: ['q, null]>(x: 'q, y: 'b) -> ['q, null] = |x, y| y;
  (f(1, null), f("s", "t"))
}"#,
    prints(r#"[null, "t"]"#);
    FuseExpect::Jit
);

const RANK2_BOUND_FIRST_OF_TWO: &str = r#"{
  let outer = |k: fn<'a: 'c, 'b: Any>(x: 'a, y: 'b) -> i64, z: 'c| (k(z, "s"), z);
  let g: fn<'p: Any, 'q: 'p>(x: 'q, y: 'p) -> i64 = |x, y| 0;
  outer(g, 1)
}"#;

run!(
    rank2_bound_first_of_two_quantifiers_is_refused,
    RANK2_BOUND_FIRST_OF_TWO,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_variable_only_the_bound_holds_is_refused,
    r#"{
  let outer = |k: fn<'a: 'c, 'b: Any>(x: 'a, y: 'b) -> i64| k;
  let g: fn<'p: Any, 'q: 'p>(x: 'q, y: 'p) -> i64 = |x, y| 0;
  let h = outer(g);
  h(1, "s")
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

// A typedef's body is a written type: its quantifier's bound may not name
// the typedef's parameter, whatever the typedef is applied to.
const RANK2_BOUND_NAMES_A_TYPEDEF_PARAMETER: &str = r#"{
  type K2<'p> = fn<'a: 'p>(x: 'a) -> 'a;
  let outer = |#k: K2<'c>, z: 'c| (k(z), z);
  let g: fn(#k: K2<i64>, z: i64) -> (i64, i64) = outer;
  g(#k: |x| x, 1)
}"#;

run!(
    rank2_bound_naming_a_typedef_parameter_is_refused,
    RANK2_BOUND_NAMES_A_TYPEDEF_PARAMETER,
    refused("a rank-2 quantifier's bound names 'p, which this function type does not quantify");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_typedef_parameter_is_refused_under_a_written_alias,
    RANK2_BOUND_NAMES_A_TYPEDEF_PARAMETER.replace("fn(#k: K2<i64>, z: i64)", "fn(#k: fn<'a: i64>(x: 'a) -> 'a, z: i64)"),
    refused("bound names 'p, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_typedef_parameter_is_refused_at_two_types,
    RANK2_BOUND_NAMES_A_TYPEDEF_PARAMETER.replace(
        "g(#k: |x| x, 1)",
        r#"let h: fn(#k: K2<string>, z: string) -> (string, string) = outer;
  (g(#k: |x| x, 1), h(#k: |x| x, "s"))"#
    ),
    refused("bound names 'p, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_a_typedef_parameter_is_refused_applied_to_a_written_type,
    r#"{
  type K<'p> = fn<'a: 'p>(x: 'a) -> 'a;
  let f: K<i64> = |x| x;
  f(1)
}"#,
    refused("bound names 'p, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_an_abstract_types_parameter_is_refused,
    r#"{
  type W<'p> = Abstract<fn<'a: 'p>(x: 'a) -> 'a>;
  0
}"#,
    refused("bound names 'p, which");
    FuseExpect::None
);

run!(
    rank2_bound_in_a_field_is_refused,
    r#"{
  let o = |r: {f: fn<'a: 'c>(x: 'a) -> 'a, n: 'c}| r.f(r.n);
  o({f: |x| x, n: 1})
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_in_a_body_annotation_is_refused,
    r#"{
  let o = |z: 'c| { let f: fn<'a: 'c>(x: 'a) -> 'a = |x| x; (f(z), z) };
  o(1)
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_beside_a_sibling_in_a_body_annotation_is_refused,
    r#"{
  let f = |v: 'p| {
    let g: fn<'c: Any, 'd: ['c, 'p]>(y: 'd) -> ['c, 'p] = |y| y;
    g(v)
  };
  f(true)
}"#,
    refused("bound names 'p, which");
    FuseExpect::None
);

run!(
    rank2_bound_in_a_let_annotation_is_refused,
    r#"{
  let h = |x, y| x;
  let f: fn<'b: ['f, i64]>(x: 'b, y: 'f) -> 'b = h;
  f(1, 2)
}"#,
    refused("bound names 'f, which");
    FuseExpect::None
);

run!(
    rank2_bound_in_a_return_type_is_refused,
    r#"{
  let o = |z: 'c| -> fn<'a: 'c>(x: 'a) -> 'a |x| x;
  let g = o(1);
  g(2)
}"#,
    refused("bound names 'c, which");
    FuseExpect::None
);

run!(
    rank2_bound_naming_an_enclosing_quantifier_is_refused,
    r#"{
  let g: fn<'q: Any>(k: fn<'a: 'q>(x: 'a) -> 'a, z: 'q) -> 'q = |k, z| k(z);
  g(|x| x, 1)
}"#,
    refused("bound names 'q, which");
    FuseExpect::None
);

run!(
    rank2_bound_in_a_trait_methods_parameter_is_refused,
    r#"{
  trait T { val m: fn(self, f: fn<'a: 'b>(x: 'a) -> 'a, y: 'b) -> 'b };
  0
}"#,
    refused("bound names 'b, which");
    FuseExpect::None
);

// self decides a constructor trait's element, and a labeled parameter
// comes before self.
run!(
    rank2_bound_naming_a_ctor_traits_element_is_refused,
    r#"{
  trait C { val m: fn(#f: fn<'b: ['a, null]>(x: 'b) -> 'b, self<'a>) -> null };
  impl C for Array<'_> { let m = |#f, s| f(null) };
  C::m(#f: |x| x, [1])
}"#,
    refused("bound names 'a, which");
    FuseExpect::None
);

// For a self dispatched to one impl, a bound naming self is not the hole:
// this refuses a method that would run, as the rule refuses any variable
// its type does not quantify (CR rank2-self-bound-01).
run!(
    rank2_bound_naming_self_is_refused,
    r#"{
  trait T { val m: fn(#k: fn<'a: [self, null]>(x: 'a) -> 'a, self) -> (self, null) };
  impl T for i64 { let m = |#k, s| (k(s), k(null)) };
  T::m(#k: |x| x, 1)
}"#,
    refused("bound names 'self, which");
    FuseExpect::None
);

// A trait method's own quantifiers are its scheme's at each impl, as at
// its declaration, whether the impl writes the method's type or not.
run!(
    a_trait_methods_own_bound_names_any_variable_at_its_impls,
    r#"{
  trait T { val m: fn<'a: ['b, null]>(#x: 'a, self, y: 'b) -> 'a };
  impl T for i64 { let m = |#x, s, y| x };
  impl<'e> T for Array<'e> { let m = |#x, s, y| x };
  impl T for string {
    let m: fn<'a: [null, 'b]>(#x: 'a, s: string, y: 'b) -> 'a = |#x, s, y| x
  };
  (T::m(#x: 2, 1, 3), T::m(#x: null, [1], "b"), T::m(#x: "a", "s", "c"))
}"#,
    prints(r#"[i64:2, null, "a"]"#);
    FuseExpect::Jit
);

run!(
    a_trait_methods_own_bound_names_any_variable_at_its_default,
    r#"{
  trait T { val m: fn<'a: ['b, null]>(#x: 'a, self, y: 'b) -> 'a = |#x, s, y| x };
  impl T for i64 {};
  impl<'e> T for Array<'e> {};
  (T::m(#x: 2, 1, 3), T::m(#x: null, [1], "b"))
}"#,
    prints("[i64:2, null]");
    FuseExpect::Jit
);

run!(
    a_ctor_trait_methods_own_bound_names_its_element_at_its_impls_and_default,
    r#"{
  trait C {
    val m: fn<'b: ['a, null]>(#x: 'b, self<'a>) -> 'b;
    val d: fn<'b: ['a, null]>(#x: 'b, self<'a>) -> 'b = |#x, s| x
  };
  impl C for Array<'_> { let m = |#x, s| x };
  impl C for List<'_> { let m = |#x, s| x };
  (C::m(#x: 2, [1]), C::m(#x: null, [<"t">]), C::d(#x: "s", ["t"]), C::d(#x: null, [<1>]))
}"#,
    prints(r#"[i64:2, null, "s", null]"#);
    FuseExpect::Jit
);
