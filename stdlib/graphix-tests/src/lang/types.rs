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
  type G = fn<'b: Number>(x: 'b, y: 'b) -> bool;
  let apply = |f: G| select f(1, 2) { true => 1, false => 0 };
  apply(|x, y| x < y)
}"#;
const NESTED_QUANTIFIER_TWO_TYPES: &str = r#"{
  type F = fn<'b: Number>(x: 'b) -> 'b;
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
        (NESTED_QUANTIFIER_ALIAS_MONO, "does not contain"),
        (NESTED_QUANTIFIER_ALIAS_CONCRETE, "cannot compute"),
        (NESTED_QUANTIFIER_ENV_CELL, "not polymorphic"),
        (NESTED_QUANTIFIER_ENV_CELL_NESTED, "not polymorphic"),
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
    assert!(msg.contains("must be fully known") && !msg.contains("'_46116"), "{msg}");
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

// Mixed sets compare like any union, numeric ones included.
// CR claude for eric: [bug/medium] Values of a mixed numeric union order by numeric
// class before value (Value::partial_cmp compares Typ first). array::sort of [3, 2.5,
// 1, 0.5]: Array<[i64, f64]> is [1, 3, 0.5, 2.5], min(i64:3, f64:2.5) is 3, and i64:3 <
// f64:2.5 is true. This fixture compares 3 with 2.5 by == only, so it would pass under
// either ordering. Either the orderings and Ordered refuse a type holding two numeric
// types, as Singleton does for arithmetic, or mixed classes order by value. Pin
// whichever is chosen here with values where the two differ, and i64:3 == f64:3.0.
// probe: design/review-2026-10-19/repro/tests-types-mixed-numeric-order.gx
// (tests-types-04)
const COMPARISON_MIXED: &str = r#"{
  let x: [i64, f64] = 3;
  let y: [i64, f64] = 2.5;
  let f = |a: Number, b: Number| a >= b;
  type N = [i64, u8];
  let n: N = 3;
  [x == y, x == x, n <= n, f(2.5, 1.5)]
}"#;

run!(comparison_mixed_numeric, COMPARISON_MIXED, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        &**a == &[
            Value::Bool(false),
            Value::Bool(true),
            Value::Bool(true),
            Value::Bool(true),
        ]
    }
    _ => false,
});

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
}; FuseExpect::None);

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
    let _s = ctx.rt.compile(ArcStr::from("let s = f(\"s\")")).await?;
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
    let r = f(41);
    sys::time::timer(duration:200.ms, false) ~ r
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
}; FuseExpect::None);

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

// A bound naming its own quantifier is checked without looping (its
// normalized copy holds the same cells), and an argument it would make
// take itself (`'a: fn(x: 'a)`) is refused: no finite type holds it.
run!(self_naming_bound_refused, r#"{
    let f = 'a: fn(x: 'a) -> [string, 'b] |g: 'a| 0;
    f(|x| 0)
}"#, refused("does not contain fn(x:"); FuseExpect::None);

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
        let held = msg.split("its type holds '_").nth(1).unwrap_or("");
        let digits = held.chars().take_while(|c| c.is_ascii_digit()).count();
        msg.contains("is not polymorphic in 'b") && (1..10).contains(&digits)
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
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "1.23456789012345678901234567 12345678901234567890. 9223372036854776000 inf 3 -0 0 1.23456789012345678901234567"); FuseExpect::Jit);

// A string holding a typed literal reads into a float as into an
// integer: what Rust's parse refuses is read as a value, then converted.
run!(typed_strings_cast_to_floats_as_to_integers, r#"{
  let f = cast<f64>("f64:1.5")$;
  let g = cast<f64>("i64:3")$;
  let n = cast<i64>("i64:3")$;
  "[f] [g] [n]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "1.5 3 3"); FuseExpect::Jit);

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
