// Tests for type system features: type checking, annotations, type variables

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{
        FuseExpect, eval_modes, is, next_update_raw, prints, refused, refused_renumbered,
        settles,
    },
};
use netidx::publisher::Value;

const SIMPLE_TYPECHECK: &str = r#"
"foo" + 1
"#;

run!(simple_typecheck, SIMPLE_TYPECHECK, refused("type mismatch string does not contain i64"); FuseExpect::None);

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

run!(lambda_constraint, LAMBDA_CONSTRAINT, refused("type mismatch Number does not contain string"); FuseExpect::None);

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

run!(explicit_type_vars1, EXPLICIT_TYPE_VARS1, refused("cannot compute with [u32, i64]: both operands must be one numeric type"); FuseExpect::None);

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
  let f = 'a: Number, 'b: Number |x: 'a, y: 'b| -> 'a x + y;
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

run!(typed_arrays1, TYPED_ARRAYS1, prints("[[i64:1, i64:2, i64:3], [u32:1, u32:2, u32:3]]"); FuseExpect::Jit);

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

// `?` over a DAG of named unions (2^40 paths) checks in time linear in the names.
const STRIP_DAG: &str = r#"
{
  type L0 = [`A, null, Error<`E>];
  type M0 = [`B, null];
  type L1 = [L0, M0];
  type M1 = [M0, L0];
  type L2 = [L1, M1];
  type M2 = [M1, L1];
  type L3 = [L2, M2];
  type M3 = [M2, L2];
  type L4 = [L3, M3];
  type M4 = [M3, L3];
  type L5 = [L4, M4];
  type M5 = [M4, L4];
  type L6 = [L5, M5];
  type M6 = [M5, L5];
  type L7 = [L6, M6];
  type M7 = [M6, L6];
  type L8 = [L7, M7];
  type M8 = [M7, L7];
  type L9 = [L8, M8];
  type M9 = [M8, L8];
  type L10 = [L9, M9];
  type M10 = [M9, L9];
  type L11 = [L10, M10];
  type M11 = [M10, L10];
  type L12 = [L11, M11];
  type M12 = [M11, L11];
  type L13 = [L12, M12];
  type M13 = [M12, L12];
  type L14 = [L13, M13];
  type M14 = [M13, L13];
  type L15 = [L14, M14];
  type M15 = [M14, L14];
  type L16 = [L15, M15];
  type M16 = [M15, L15];
  type L17 = [L16, M16];
  type M17 = [M16, L16];
  type L18 = [L17, M17];
  type M18 = [M17, L17];
  type L19 = [L18, M18];
  type M19 = [M18, L18];
  type L20 = [L19, M19];
  type M20 = [M19, L19];
  type L21 = [L20, M20];
  type M21 = [M20, L20];
  type L22 = [L21, M21];
  type M22 = [M21, L21];
  type L23 = [L22, M22];
  type M23 = [M22, L22];
  type L24 = [L23, M23];
  type M24 = [M23, L23];
  type L25 = [L24, M24];
  type M25 = [M24, L24];
  type L26 = [L25, M25];
  type M26 = [M25, L25];
  type L27 = [L26, M26];
  type M27 = [M26, L26];
  type L28 = [L27, M27];
  type M28 = [M27, L27];
  type L29 = [L28, M28];
  type M29 = [M28, L28];
  type L30 = [L29, M29];
  type M30 = [M29, L29];
  type L31 = [L30, M30];
  type M31 = [M30, L30];
  type L32 = [L31, M31];
  type M32 = [M31, L31];
  type L33 = [L32, M32];
  type M33 = [M32, L32];
  type L34 = [L33, M33];
  type M34 = [M33, L33];
  type L35 = [L34, M34];
  type M35 = [M34, L34];
  type L36 = [L35, M35];
  type M36 = [M35, L35];
  type L37 = [L36, M36];
  type M37 = [M36, L36];
  type L38 = [L37, M37];
  type M38 = [M37, L37];
  type L39 = [L38, M38];
  type M39 = [M38, L38];
  type L40 = [L39, M39];
  type M40 = [M39, L39];
  let x: L40 = `A;
  let y = x?;
  y
}
"#;

run!(strip_any_over_name_dag, STRIP_DAG, is(Value::String("A".into())); FuseExpect::Jit);

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
run!(one_cell_formals_refuse_mixed_call, ONE_CELL_FORMALS_MIXED_CALL, refused("cannot compute with [i64, f64]: both operands must be one numeric type");
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
    refused("{ b: f64, y: i64 } has no field n");
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
    refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes");
    FuseExpect::None
);

// `Concrete` is judged through a type's references: a typedef naming a
// reference or a function is no read target, as the type written out is
// not.
run!(
    concrete_refuses_a_reference_through_a_typedef,
    r#"{ type R = &i64; let r: Result<R, `ParseError(string)> = str::parse("42"); r }"#,
    refused("R is not data: data holds no function and no reference");
    FuseExpect::None
);
run!(
    concrete_refuses_a_function_through_a_typedef,
    r#"{ type F = fn(x: i64) -> i64;
         let r: Result<F, `ParseError(string)> = str::parse("42"); r }"#,
    refused("F is not data: data holds no function and no reference");
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
// CR claude for claude: [test-gap/low] This loop and nine like it in this file repeat
// the match that unsound_acceptances::refused holds, and they hold less than run! does.
// The nine are function_bounds_admit_only_functions, a_signature_types_its_calls,
// recursive_fn_tail_rejected_at_every_depth, comparison_operands_are_one_type,
// ordering_refuses_two_numeric_types, ordering_refuses_an_open_member_beside_a_number,
// arithmetic_operands_are_one_type, discernible_refuses_one_runtime_form and
// singleton_open_members_merge. eval_modes keeps only the build's message, so the
// check's refusal (what --check and the language server show) is never held to the
// phrase, and the first failing source hides the rest. Write each source as its own
// run!(name, src, refused(phrase); FuseExpect::None), and the accepted ones with
// is(..). Found by reading; the check's messages for all 39 sources contain their
// phrases today. (tests-types-a-04)
// 2026-11-21 claude: Renamed: comparison_operands_are_one_type is
// comparison_operands_one_holding_the_other, ordering_refuses_two_numeric_types is
// ordering_takes_two_numeric_types, ordering_refuses_an_open_member_beside_a_number is
// ordering_takes_an_open_member_beside_a_number, singleton_open_members_merge is
// singleton_open_members_are_one_type_at_each_call.
// concrete_targets_are_known_where_they_settle, function_bounds_admit_only_functions
// and recursive_fn_tail_rejected_at_every_depth still hold the loop shape the CR
// describes; discernible_refuses_one_runtime_form too.
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
            msg.contains(
                "must be known here, an operation reads it to know what to make"
            ),
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
            msg.contains("must be Function here")
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

// A function-bounded builtin passed through a call is one instance of its
// scheme, which its uses decide.
#[tokio::test(flavor = "current_thread")]
async fn function_bounded_builtin_passed_as_a_value() -> Result<()> {
    let src = r#"{
        let g = filter(queuefn, |x| true);
        let inc = g(#trigger: null, |x: i64| x + 1);
        inc(41)
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
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
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
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

// `'r ⊇ [T.., 'r]` binds `'r := [T..]`: a result that is a union of
// two constructors is the whole union, never its first member.
const REC_UNION_RESULT_OF_A_PARAM: &str = r#"
{
  let rec f = |x: [Array<i64>, Array<string>], n: i64| select n { 0 => x, _ => f(x, n - 1) };
  let a = f([1], 2);
  select f(["a"], 3) { Array<i64> as _ => a, s => s }
}
"#;

run!(
    rec_union_result_of_a_param,
    REC_UNION_RESULT_OF_A_PARAM,
    prints(r#"["a"]"#);
    FuseExpect::Jit
);

const REC_UNION_RESULT_OF_ARMS: &str = r#"
{
  let rec f = |n: i64| select n { 0 => [1], 1 => ["a"], 2 => null, _ => { let t = f(n - 3); t } };
  "[f(3)] [f(4)] [f(5)]"
}
"#;

run!(
    rec_union_result_of_arms,
    REC_UNION_RESULT_OF_ARMS,
    prints(r#""[1] [\"a\"] null""#);
    FuseExpect::Jit
);

const REC_UNION_RESULT_OF_DECLARED_VARIABLES: &str = r#"
{
  type P<'p, 'q> = [Array<'p>, Map<string, 'q>];
  let rec f = |x: P<'a, 'b>, n: i64| select n { 0 => x, _ => f(x, n - 1) };
  let v: P<i64, string> = {"k" => "v"};
  f(v, 2)
}
"#;

run!(
    rec_union_result_of_declared_variables,
    REC_UNION_RESULT_OF_DECLARED_VARIABLES,
    prints(r#"{"k" => "v"}"#);
    FuseExpect::Jit
);

// A declared variable beside the cell is held with the other members, as
// it would be without the cell: `'r := ['a, Array<i64>]`.
const REC_UNION_RESULT_OF_A_DECLARED_MEMBER: &str = r#"
{
  let rec f = |x: 'a, n: i64| select n { 0 => x, 1 => [1], _ => f(x, n - 1) };
  let a = f("z", 0);
  let b = f(2, 3);
  "[a] [b]"
}
"#;

run!(
    rec_union_result_of_a_declared_member,
    REC_UNION_RESULT_OF_A_DECLARED_MEMBER,
    prints(r#""z [1]""#);
    FuseExpect::Jit
);

// The members held together still may not hold the cell.
const REC_UNION_RESULT_INFINITE_REJECTS: &str = r#"
{
  let rec f = |n: i64| select n { 0 => [1], 1 => ["a"], 2 => [f(n - 2)], _ => f(n - 2) };
  f(3)
}
"#;

run!(
    rec_union_result_infinite_rejects,
    REC_UNION_RESULT_INFINITE_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
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
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
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
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

// A call pre-unifies its formal with the argument's type before the
// argument is checked, so the literal's element cell is reachable when
// the literal binds it: `x` would hold `Array<[i64, fn(x: <x>) -> _]>`.
const REC_SELF_IN_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f([1, f]); 0 }";

run!(
    rec_self_in_array_arg_rejects,
    REC_SELF_IN_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_ALONE_IN_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f([f]); 0 }";

run!(
    rec_self_alone_in_array_arg_rejects,
    REC_SELF_ALONE_IN_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_PARAM_IN_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f([1, x]); 0 }";

run!(
    rec_param_in_array_arg_rejects,
    REC_PARAM_IN_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_LIST_ARG_REJECTS: &str = "{ let rec f = |x| f([<f>]); 0 }";

run!(
    rec_self_in_list_arg_rejects,
    REC_SELF_IN_LIST_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_MAP_ARG_REJECTS: &str = r#"{ let rec f = |x| f({"a" => f}); 0 }"#;

run!(
    rec_self_in_map_arg_rejects,
    REC_SELF_IN_MAP_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_STRUCT_ARG_REJECTS: &str = "{ let rec f = |x| f({a: f}); 0 }";

run!(
    rec_self_in_struct_arg_rejects,
    REC_SELF_IN_STRUCT_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_STRUCT_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f({a: [f]}); 0 }";

run!(
    rec_self_in_struct_array_arg_rejects,
    REC_SELF_IN_STRUCT_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_TUPLE_ARG_REJECTS: &str = "{ let rec f = |x| f((1, f)); 0 }";

run!(
    rec_self_in_tuple_arg_rejects,
    REC_SELF_IN_TUPLE_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_TUPLE_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f(([f], 1)); 0 }";

run!(
    rec_self_in_tuple_array_arg_rejects,
    REC_SELF_IN_TUPLE_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_REF_ARRAY_ARG_REJECTS: &str = "{ let rec f = |x| f(&[f]); 0 }";

run!(
    rec_self_in_ref_array_arg_rejects,
    REC_SELF_IN_REF_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_IN_LET_BOUND_ARRAY_REJECTS: &str =
    "{ let rec f = |x| { let v = [1, f]; f(v) }; 0 }";

run!(
    rec_self_in_let_bound_array_rejects,
    REC_SELF_IN_LET_BOUND_ARRAY_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const REC_SELF_LET_BOUND_IN_ARRAY_ARG_REJECTS: &str =
    "{ let rec f = |x| { let g = f; g([g]) }; 0 }";

run!(
    rec_self_let_bound_in_array_arg_rejects,
    REC_SELF_LET_BOUND_IN_ARRAY_ARG_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

const CLOSURE_PASSES_REC_SELF_IN_ARRAY_REJECTS: &str =
    "{ let rec f = |x| { let g = |y| f([y, f]); g(x) }; 0 }";

run!(
    closure_passes_rec_self_in_array_rejects,
    CLOSURE_PASSES_REC_SELF_IN_ARRAY_REJECTS,
    refused("infinite type: a value here would hold a value of its own type (a function passed or returning itself, a collection holding itself); declare a named recursive type and annotate the binding");
    FuseExpect::None
);

// Without `rec` a closure reaches itself only through a parameter, whose
// function type no call can know.
const CLOSURE_PASSES_ITSELF_IN_ARRAY_REJECTS: &str = "{ let f = |g, x| g(g, [1, g]); 0 }";

run!(
    closure_passes_itself_in_array_checks_uncalled,
    CLOSURE_PASSES_ITSELF_IN_ARRAY_REJECTS,
    prints("i64:0");
    FuseExpect::Jit
);

// (was closure_passes_itself_in_array_rejects): the call makes the type infinite.
run!(
    closure_passes_itself_in_array_called_with_itself_refused,
    "{ let f = |g, x| g(g, [1, g]); f(f, 0) }",
    refused("infinite type: a variable holds itself");
    FuseExpect::None
);

// A written binding is not generalized: the call's formal is `f`'s own
// cell, which the literal's element would hold.
const WRITTEN_SELF_IN_ARRAY_ARG_REJECTS: &str = "{ let f = |x| 0; f <- |x| 0; f([f]) }";

run!(
    written_self_in_array_arg_rejects,
    WRITTEN_SELF_IN_ARRAY_ARG_REJECTS,
    refused_renumbered("infinite type: a variable holds itself in fn(x: '_0) -> '_1");
    FuseExpect::None
);

const WRITTEN_SELF_NESTED_BESIDE_LET_REJECTS: &str =
    "{ let a = [1]; let f = |x| 0; f <- |x| 0; f([a, [f]]) }";

run!(
    written_self_nested_beside_let_rejects,
    WRITTEN_SELF_NESTED_BESIDE_LET_REJECTS,
    refused_renumbered("infinite type: a variable holds itself in fn(x: '_0) -> '_1");
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
    refused("type mismatch Array<'a: unbound within i64 above i64> does not contain i64");
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
    refused("type mismatch Array<i64> does not contain i64");
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
                assert!(msg.contains("L<'a> does not contain `Cons(i64"), "{tail}: {msg}")
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

// A typedef a function's body declares, over a type the module declares
// below the function, is judged once the check ends: the later type
// closes a cycle through unions alone.
const FN_BODY_TYPEDEF_CYCLE_THROUGH_A_LATER_TYPE: &str = r#"
{
  let f = |a: i64| { type T = [i64, Zed<T>]; let x: T = a; x };
  type Zed<'a> = ['a, string];
  f(1)
}
"#;
run!(fn_body_typedef_cycle_through_a_later_type, FN_BODY_TYPEDEF_CYCLE_THROUGH_A_LATER_TYPE, refused("refers back to itself through unions and aliases alone"); FuseExpect::None);

run!(
    fn_body_typedef_cycle_through_a_later_module_type,
    refused("refers back to itself through unions and aliases alone"),
    "/test.gx" => "let f = |a: i64| { type T = [i64, Zed<T>]; let x: T = a; x }; \
        type Zed<'a> = ['a, string]; let result = f(1)"
; FuseExpect::None);

run!(
    fn_body_typedef_through_a_later_type_guarded,
    is(Value::I64(1)),
    "/test.gx" => "let f = |a: i64| { type T = [i64, Zed<T>]; let x: T = a; x }; \
        type Zed<'a> = [`Z('a), string]; let result = f(1)"
; FuseExpect::Jit);

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
// member to the bound, never binds it to the bound: the member is one type,
// and decided after the operand it flows into, it fits what the operand
// was decided as (here i64), so the instance of `f` the build makes is the
// one the check typed. A callback bound by `let` checks as the same one
// inline does.
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

// A known primitive that only partly fills the primitive member leaves it
// whole: `x` is [i64, string, `A], as over [i64, string].
const PARTLY_FILLED_PRIMITIVE_STAYS_WHOLE: &str = r#"
{
  let f = |x| { let r: [i64, string, `A] = select 0 { 0 => 2, _ => x }; 1 };
  [f("s"), f(3), f(`A)]
}
"#;
run!(
    partly_filled_primitive_stays_whole,
    PARTLY_FILLED_PRIMITIVE_STAYS_WHOLE,
    |v: Result<&Value>| {
        match v {
            Ok(Value::Array(a)) => {
                a.iter().all(|v| matches!(v, Value::I64(1))) && a.len() == 3
            }
            _ => false,
        }
    }
);

// One it fills is taken: `x` is `A.
const FILLED_PRIMITIVE_IS_TAKEN: &str = r#"
{
  let f = |x| { let r: [i64, `A] = select 0 { 0 => 2, _ => x }; 1 };
  f(3)
}
"#;
run!(
    filled_primitive_leaves_the_whole_union_to_the_parameter,
    FILLED_PRIMITIVE_IS_TAKEN,
    prints("i64:1");
    FuseExpect::Jit
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

// A let annotation's same-named variables are one variable, and in a
// body one the definition declares is the definition's own.
run!(
    annotation_names_one_variable_in_a_tuple,
    "{ let w: ('a, 'a) = (1, \"s\"); 0 }",
    prints("i64:0");
    FuseExpect::Jit
);

// longer shows that the two 'a are one variable; with ('a, 'b) the same read checks,
run!(
    annotation_names_one_variable_in_a_tuple_read_as_two_types_refused,
    "{ let w: ('a, 'a) = (1, \"s\"); let k: (i64, string) = w; 0 }",
    refused("i64 does not contain string");
    FuseExpect::None
);
run!(
    annotation_names_one_variable_across_a_fn_type,
    "{ let p: ('c, fn(x: 'c) -> 'c) = (1, |x: string| x); 0 }",
    refused("type mismatch string does not contain i64");
    FuseExpect::None
);
// A return a call picks is not the body's type because the body's type fits
// the variable's bound: the refusal names the return type to write.
run!(
    a_returned_declared_variable_names_the_body_type,
    "{ let f = 'a: Number |x: 'a| -> 'a 1; f(2) }",
    refused(
        "a call picks 'a, and the body's value need not be of the type it picks; \
         drop `-> 'a`, or write the body's type, `-> i64`"
    );
    FuseExpect::None
);
run!(
    a_return_holding_a_declared_variable_names_the_body_type,
    "{ let f = 'a: Number |x: 'a| -> {v: 'a} {v: 1}; f(2).v }",
    refused("drop `-> { v: 'a }`, or write the body's type, `-> { v: i64 }`");
    FuseExpect::None
);
run!(
    a_return_of_one_declared_variable_names_the_other,
    "{ let f = 'a: Number, 'b: Number |x: 'a, y: 'b| -> 'a y; f(1, 2) }",
    refused("a call picks 'a, and the body's value need not be of the type it picks; drop `-> 'a`, or write the body's type, `-> 'b`");
    FuseExpect::None
);
// A handle a call copied is bound to the declared variable's cell: the
// message names that one cell once.
run!(
    a_copied_handle_of_a_declared_variable_prints_as_one_variable,
    |v: Result<&Value>| matches!(v, Err(e)
        if format!("{e:#}").contains("string does not contain Array<'a: unbound within Number>")),
    "/test.gx" => "let result = { let k = 'a: Number |x: 'a| -> 'a { let m = array::map([x], |e| e); let z: string = m; x }; k(1) }"
; FuseExpect::None);
run!(
    a_return_of_one_unbounded_variable_names_the_other,
    "{ let f = |x: 'a, y: 'b| -> 'a y; f(1, 2) }",
    refused(
        "a call picks 'a, and the body's value need not be of the type it picks; \
         drop `-> 'a`, or write the body's type, `-> 'b`"
    );
    FuseExpect::None
);
run!(
    a_return_of_two_swapped_variables_names_both,
    "{ let f = |x: 'a, y: 'b| -> ('a, 'b) (y, x); f(1, 2) }",
    refused(
        "a call picks 'a and 'b, and the body's value need not be of the types it \
         picks; drop `-> ('a, 'b)`, or write the body's type, `-> ('b, 'a)`"
    );
    FuseExpect::None
);
run!(
    a_return_of_two_swapped_variables_written_as_the_body_runs,
    "{ let f = |x: 'a, y: 'b| -> ('b, 'a) (y, x); f(1, 2).0 }",
    is(Value::I64(2));
    FuseExpect::Jit
);
run!(
    a_return_naming_the_other_declared_variable_runs,
    "{ let f = 'a: Number, 'b: Number |x: 'a, y: 'b| -> 'b y; f(1, 2) }",
    is(Value::I64(2));
    FuseExpect::Jit
);
// A declared variable standing in a callback's result is each call's to
// pick, as a parameter's is, through a nested function too.
run!(
    a_declared_variable_in_a_callbacks_result_is_each_calls,
    "{ let m = 'b: Number |s: i64, f: fn(x: i64) -> 'b| -> 'b f(s); \
     let n = 'b: Number |s: i64, f: fn(x: i64) -> 'b| -> 'b { let g = |y| f(y); g(s) }; \
     let a: f64 = m(3, |x| 1.5); let b: i64 = m(3, |x| x); let c: f64 = n(3, |x| 2.5); \
     (a, b, c) }",
    prints("[f64:1.5, i64:3, f64:2.5]")
);
run!(
    a_declared_variable_in_a_callbacks_result_computes_at_each_calls_type,
    "{ let m = 'b: Number |s: i64, f: fn(x: i64) -> 'b| -> 'b { let h = f(s); h + h }; \
     let a: f64 = m(3, |x| 1.5); (a, m(3, |x| x)) }",
    prints("[f64:3., i64:6]")
);
run!(
    a_callbacks_result_is_not_its_declared_variables_bound,
    "{ let m = 'b: [i64, null] |s: i64, f: fn(x: i64) -> 'b| -> 'b \
     { let r: [i64, null] = f(s); r }; m(1, |x| x) }",
    refused(
        "a call picks 'b, and the body's value need not be of the type it picks; \
         drop `-> 'b`, or write the body's type, `-> [i64, null]`"
    );
    FuseExpect::None
);
run!(
    a_callbacks_result_held_at_its_bound_returns_the_bound,
    "{ let m = 'b: [i64, null] |s: i64, f: fn(x: i64) -> 'b| -> [i64, null] \
     { let r: [i64, null] = f(s); r }; (m(1, |x| x), m(1, |x| null)) }",
    prints("[i64:1, null]")
);
// A declared variable standing in a callback's throws is each call's too.
run!(
    a_declared_variable_in_a_callbacks_throws_is_each_calls,
    "{ let m = 'e: Error<ErrChain<[`A, `B]>> |f: fn() -> i64 throws 'e| -> i64 throws 'e f(); \
     let c: Error<ErrChain<`A>> = never(); catch(e) c <- e; m(|| error(`A)?); c.0.error }",
    prints(r#""A""#)
);
run!(
    a_callbacks_declared_result_is_not_the_bodys_value,
    "{ let m = 'b: [i64, null] |s: i64, f: fn(x: i64) -> 'b| -> 'b { let r = f(s); 3 }; \
     m(1, |x| x) }",
    refused(
        "a call picks 'b, and the body's value need not be of the type it picks; \
         drop `-> 'b`, or write the body's type, `-> i64`"
    );
    FuseExpect::None
);
run!(
    a_return_no_bound_holds_names_no_picked_variable,
    refused("type mismatch string does not contain i64"),
    "/test.gx" => "let result = { let f = 'a: Number |x: 'a| -> ('a, string) (x, 1); f(2) }"
; FuseExpect::None);
// A method's return is its trait's: no return type written in the impl
// makes a body of another type pass, so the refusal names none.
run!(
    a_methods_return_names_no_return_to_write,
    |v: Result<&Value>| matches!(v, Err(e)
        if format!("{e:#}").contains("'a: unbound within Number does not contain i64")
            && !format!("{e:#}").contains("a call picks")),
    "/test.gx" => r#"
        type W = Abstract<i64>;
        trait Idt { val idt: fn<'a: Number>(self, x: 'a) -> 'a };
        impl Idt for W { let idt = |s, x| 1 };
        let result = Idt::idt(W(0), 2)
    "#
; FuseExpect::None);
run!(
    a_default_methods_return_names_no_return_to_write,
    |v: Result<&Value>| matches!(v, Err(e)
        if format!("{e:#}").contains("'a: unbound within Number does not contain i64")
            && !format!("{e:#}").contains("a call picks")),
    "/test.gx" => r#"
        type W = Abstract<i64>;
        trait Idt { val idt: fn<'a: Number>(self, x: 'a) -> 'a = |s, x| 1 };
        impl Idt for W {};
        let result = Idt::idt(W(0), 2)
    "#
; FuseExpect::None);
run!(
    a_lambda_in_a_methods_body_names_its_return_to_write,
    refused("drop `-> 'b`, or write the body's type, `-> i64`"),
    "/test.gx" => r#"
        type W = Abstract<i64>;
        trait Idt { val idt: fn<'a: Number>(self, x: 'a) -> 'a };
        impl Idt for W {
            let idt = |s, x| { let h = 'b: Number |y: 'b| -> 'b 1; h(x) }
        };
        let result = Idt::idt(W(0), 2)
    "#
; FuseExpect::None);
run!(
    a_method_returning_its_argument_runs,
    is(Value::I64(2)),
    "/test.gx" => r#"
        type W = Abstract<i64>;
        trait Idt { val idt: fn<'a: Number>(self, x: 'a) -> 'a };
        impl Idt for W { let idt = |s, x| x };
        let result = Idt::idt(W(0), 2)
    "#
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
    a_quantifier_only_an_enclosing_bound_names_is_refused,
    "{ let f = 'a: Number, 'z: Array<'a> |w: 'z| { \
       let g = 'a: Any |x: 'z| -> i64 0; g(w) }; f([1]) }",
    refused("'a is declared but the signature does not hold it");
    FuseExpect::None
);
run!(
    a_nested_definition_naming_an_enclosing_bounded_variable_checks,
    "{ let f = 'a: Number, 'z: Array<'a> |w: 'z| { \
       let g = |x: 'z| -> i64 0; g(w) }; f([1]) }",
    prints("i64:0")
);
// A nested definition holds its own variable rigid in its body, though a
// parameter reaches an enclosing bound that names a variable of its name.
run!(
    a_nested_quantifier_stays_rigid_beside_an_enclosing_bound_of_its_name,
    "{ let f = 'a: Number, 'b: Array<'a> |w: 'a, v: 'b| { \
       let g = 'a: Any |x: fn(q: 'b) -> i64| -> 'a x; g(|q| 0) }; f(1, [1]) }",
    refused("a call picks 'a, and the body's value need not be of the type it picks");
    FuseExpect::None
);
run!(
    a_nested_quantifier_beside_an_enclosing_bound_of_its_name_stays_generic,
    "{ let f = 'a: Number, 'b: Array<'a> |w: 'a, v: 'b| { \
       let g = 'a: Any |x: fn(q: 'b) -> i64, y: 'a| -> 'a y; \
       g(|q| 0, 1) + str::len(g(|q| 0, \"s\")) }; f(1, [1]) }",
    prints("i64:2")
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
    one_number_beside_an_open_member_is_judged_at_the_call,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y| f(select b { true => 1, false => y }); 0 }",
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    one_number_beside_an_open_member_called_with_another_number_refused,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y| f(select b { true => 1, false => y }); g(true, 2.0) }",
    refused("[i64, f64] must be OneNumber here");
    FuseExpect::None
);
// Two open members beside no numeric one are refused alike: each may yet
// be a numeric type, and two of them two different ones.
run!(
    one_number_two_open_members_refused,
    "{ let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x; \
       let g = |b, y, z| f(select b { true => z, false => y }); g(false, 2.5, 1) }",
    refused("[i64, f64] must be OneNumber here");
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
// `with` replaces the field in every member, which the union need not
// hold, and a writable reference to a field needs the members' fields to
// be one type.
run!(
    union_field_with_replaces_every_member,
    "{ type S = [{a: i64, b: string}, {a: string, b: i64}]; \
       let h = |s: S, k: i64| -> S {s with a: k}; 0 }",
    refused("type mismatch S does not contain { a: i64, b: i64 }");
    FuseExpect::None
);
run!(
    union_field_place_needs_one_field_type,
    "{ type S = [{a: i64, b: string}, {a: string, b: i64}]; \
       let s: S = {a: \"y\", b: 9}; let r = &mut s.a; 0 }",
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    union_tuple_place_needs_one_field_type,
    "{ let t: [(i64, string), (string, i64)] = (\"y\", 9); let r = &mut t.0; 0 }",
    refused("type mismatch i64 does not contain string");
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
    prints("[[i64:1, i64:2], [i64:5, i64:3]]");
    FuseExpect::None
);
run!(
    default_factory_open_array,
    "{ let mk = |z| { let g = |#d: Array<'q> = [z], x| [d, [x]]; g }; \
       let f = mk(1); f(2) }",
    prints("[[i64:1], [i64:2]]"); FuseExpect::None
);
run!(
    default_factory_open_struct,
    "{ let mk = |z| { let g = |#d: {v: 'q} = {v: z}, x| (d, x); g }; \
       let f = mk(1); f(2) }",
    prints(r#"[[["v", i64:1]], i64:2]"#); FuseExpect::None
);
run!(
    default_factory_open_union,
    "{ let mk = |z| { let g = |#d: [null, 'q] = z, x| (d, x); g }; \
       let f = mk(1); f(2) }",
    prints("[i64:1, i64:2]"); FuseExpect::None
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
    refused("type mismatch string does not contain i64"); FuseExpect::None
);
run!(
    default_of_one_declared_variable_for_another,
    "{ let mk = |z: 'b, w: 'a| { let g = |#d: Array<'a> = [z], y: i64| d; (g, w) }; \
       let p = mk(\"s\", 1); p.0(2) }",
    prints(r#"["s"]"#); FuseExpect::None
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
run!(operand_with_two_numeric_members, OPERAND_WITH_TWO_NUMERIC_MEMBERS, refused("cannot compute with [i64, f64]: both operands must be one numeric type"); FuseExpect::None);

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

// The refusal names the first unused parameter as written.
run!(
    typedef_unused_parameter_named_in_order,
    refused("unused type parameter 'g in definition of D"),
    "/test.gx" => r#"
        type D<'h, 'g, 'f, 'e, 'd, 'c, 'b, 'a> = [`W('h), `Z];
        let result = 0
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
// A parameter's declared 'a is rigid in the body: `f` takes whatever
// the caller's 'a is, never a string.
const RIGID_PARAM_CALL: &str = r#"{
  let m = |a: 'a, f: fn(x: 'a) -> 'b| -> 'b f("a");
  m(1, |x| x * 2)
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

/// An abstract pattern under a union test takes only its own member: the
/// rest must be covered.
const ABSTRACT_PATTERN_OVER_A_UNION: &str = r#"{
    type Box = Abstract<i64>;
    let v: [Box, i64] = 1;
    select v { [Box, i64] as Box(x) => x }
}"#;
const ABSTRACT_PATTERN_OVER_A_UNION_COVERED: &str = r#"{
    type Box = Abstract<i64>;
    let v: [Box, i64] = 1;
    select v { [Box, i64] as Box(x) => x, _ => 0 }
}"#;
const ABSTRACT_PATTERN_BESIDE_ITS_MEMBER: &str = r#"{
    type Box = Abstract<i64>;
    let v: [Box, i64] = 1;
    select v { [Box, i64] as Box(x) => x, i64 as n => n + 6 }
}"#;

/// `never<T>()` holds T's applications to their parameter bounds.
const NEVER_OF_AN_UNBOUNDED_APPLICATION: &str = r#"{
    type Num<'a: Number> = 'a;
    never<Num<string>>()
}"#;

// A cell of a definition that an undecided binding outside it flows into
// is decided outside, where the definition's declared variables stand for
// nothing: one flowing in beside it is refused, naming the binding.
const DECLARED_CONSTRAINS_OUTER: &str = r#"{
    let y = never();
    let g = 'b: 'c, 'c: Any |p: 'b, q: 'c| q;
    let f = 'a: Any |x: 'a| -> 'a { let r = g(y, x); x };
    let t = f("s");
    y <- 5;
    (f(1), t, y + 1)
}"#;
const DECLARED_BESIDE_AN_OUTER_ELEMENT: &str = r#"{
    let y = never();
    let f = 'a: Any |x: 'a| -> 'a { let r = [y, x]; x };
    let t = f("s");
    y <- 5;
    (t, y + 1)
}"#;
const DECLARED_CONSTRAINS_A_DECLARED_OUTER: &str = r#"{
    let y: i64 = never();
    let g = 'b: 'c, 'c: Any |p: 'b, q: 'c| q;
    let f = 'a: Any |x: 'a| -> 'a { let r = g(y, x); x };
    let t = f("s");
    y <- 5;
    f(1) + str::len(t) + y
}"#;

const RIGID_BELOW_UNANNOTATED: &str = r#"{
  type W<'t> = Abstract<'t>;
  let outer = |z: W<'c>| {
    let inner = |v| { let q: W<'c> = W(v); v };
    (inner(1), inner("s"), z)
  };
  outer(W(true))
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
const NO_ERROR_OPEN_MEMBER_HOLDS: &str = r#"{
  let r: Result<[i64, 'x], `E> = error(`E);
  0
}"#;
const NO_ERROR_OPEN_MEMBER_OF_AN_ELEMENT: &str = r#"{
  type R<'a: NoError> = Array<'a>;
  let r: R<[i64, 'x]> = [error(`Q)];
  0
}"#;
const NO_ERROR_OPEN_MEMBER_DECLARED: &str = r#"{
  let f = 'x: Any |a: 'x| -> i64 { let r: Result<[i64, 'x], `E> = a; 0 };
  f(1)
}"#;
const NO_ERROR_OPEN_MEMBER_DECLARED_NO_ERROR: &str = r#"{
  let f = 'x: NoError |a: 'x| -> i64 { let r: Result<[i64, 'x], `E> = a; 0 };
  f(1)
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

/// Programs the checker once accepted and must refuse, each in every mode,
/// by its refusal; and their sound twins, each with its value.
mod unsound_acceptances {
    use super::*;

    async fn refused(src: &str, refusal: &str) -> Result<()> {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(refusal), "wrong refusal for {src}: {msg}");
        Ok(())
    }

    async fn evaluates(src: &str, expected: i64) -> Result<()> {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(expected), "{src}");
        Ok(())
    }

    macro_rules! cases {
        ($f:ident: $($name:ident => $arg:expr,)*) => {$(
            #[allow(non_snake_case)]
            #[tokio::test(flavor = "current_thread")]
            async fn $name() -> Result<()> {
                $f(super::$name, $arg).await
            }
        )*};
    }

    cases! { refused:
        NON_CONTRACTIVE => "refers back to itself",
        NON_CONTRACTIVE_MUTUAL => "refers back to itself",
        NON_CONTRACTIVE_ALIAS => "refers back to itself",
        PARAM_CONTRAVARIANT => "does not contain",
        REF_THROUGH_CELL => "does not contain",
        RIGID_IN_SET => "does not contain",
        RIGID_IN_SET_ARRAY => "does not contain",
        SET_MEMBER_PROBE_BINDS => "does not contain",
        DEFAULTED_LABEL => "does not contain",
        PRODUCT_UNION => "does not contain",
        PRODUCT_UNION_VARIANT => "does not contain",
        REF_UNION_PARAMS => "i64 does not contain string",
        DECLARED_CONSTRAINS_OUTER => "the declared type variable 'a meets a value from \
            outside the definition that declares it, whose type the code around decides: \
            declare `y`'s type where it is bound",
        DECLARED_BESIDE_AN_OUTER_ELEMENT => "declare `y`'s type where it is bound",
        RIGID_BELOW_UNANNOTATED => "does not contain i64",
        NEVER_OF_AN_UNBOUNDED_APPLICATION => "Number does not contain string",
        TYPE_TEST_HOLDING_A_FN => "can't match on a type holding a function",
        TYPE_TEST_RUST_BACKED_PARAMS => "same runtime form",
        REF_TEST_OVER_ANY => "can't tell what the reference in &mut string refers to",
        REF_TEST_OVER_ANY_NESTED => "can't tell what the reference",
        CAST_TO_AN_ABSTRACT_OF_A_FUNCTION => "can't cast a value to a function",
        CONJUNCT_RIGID => "does not contain i64",
        CONJUNCT_RIGID_TRAIT => "[i64, 'a] does not implement Sz",
        NO_ERROR_OPEN_MEMBER => "Error<`Q> must be NoError here",
        NO_ERROR_OPEN_MEMBER_OF_AN_ELEMENT => "Error<`Q> must be NoError here",
        NO_ERROR_OPEN_MEMBER_DECLARED => "the body needs 'x within NoError, which the \
            signature does not declare: write 'x: NoError",
        ABSTRACT_PATTERN_OVER_A_UNION => "missing match cases: nothing matches i64",
        // CR claude for claude: [test-gap/low] This phrase is also what the same
        // program gives without rigidity. With 'a decided as string in the body, `m(1,
        // |x| x * 2)` is refused at the call with "string does not contain i64", so the
        // case passes whether or not a parameter's variable stays rigid through a
        // callback's parameter type. Pin "'a: unbound does not contain string", and add
        // a call only rigidity refuses, `m("s", |x| str::len(x))`. probe:
        // design/review-2026-11-16/repro/tests-types-a-rigid-param-call-vacuous.sh
        // (tests-types-a-03)
        RIGID_PARAM_CALL => "does not contain",
        TRAIT_BESIDE_UNSATISFIABLE => "no value is both: nothing can be passed for it",
    }

    cases! { evaluates:
        CONTRACTIVE => 1,
        DECLARED_CONSTRAINS_A_DECLARED_OUTER => 7,
        PARAM_COVARIANT => 1,
        REF_THROUGH_CELL_OK => 2,
        RIGID_IN_SET_OK => 1,
        SET_MEMBER_PROBE => 42,
        DEFAULTED_LABEL_OK => 2,
        PRODUCT_UNION_ONE_DIFFERS => 1,
        REF_UNION_SAME_PARAMS => 2,
        DIFF_REF_PARAMS => 1,
        TRAIT_BESIDE_WITNESS => 0,
        ERROR_UNION_DISTRIBUTES => 1,
        NO_ERROR_OPEN_MEMBER_TOLD => 1,
        NO_ERROR_OPEN_MEMBER_HOLDS => 0,
        NO_ERROR_OPEN_MEMBER_DECLARED_NO_ERROR => 0,
        SHARED_REF_TEST_OVER_MUT => 1,
        ABSTRACT_PATTERN_OVER_A_UNION_COVERED => 0,
        ABSTRACT_PATTERN_BESIDE_ITS_MEMBER => 7,
    }
}

// A declared variable a type-directed read's result is takes the read's
// `Concrete`: a call that leaves it undecided is refused, as the read's
// own would be, wherever the value goes or if it goes nowhere. One
// declared without the bound is refused, naming it.
#[tokio::test(flavor = "current_thread")]
async fn a_declared_result_a_typed_read_makes_must_be_decided() -> Result<()> {
    for src in [
        r#"{
  let rd = |s: string| -> 'a json::read(s)$;
  let g = |s| { let v = rd(s); [v] };
  g("1")
}"#,
        r#"{ let rd = |s: string| -> 'a json::read(s)$; rd("1"); 0 }"#,
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains("the type 'a must be known here"), "{src}: {msg}");
    }
    let src = "{ let rd = 'a: Any |s: string| -> 'a json::read(s)$; 0 }";
    let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
        Err(msg) => msg,
        Ok(v) => panic!("must be refused: {src} => {v:?}"),
    };
    assert!(msg.contains("add Concrete to the bound of 'a"), "{msg}");
    for (src, expected) in [
        (
            r#"{
  let rd = |s: string| -> 'a json::read(s)$;
  let g = |s| { let v = rd(s); [v] };
  let w: Array<i64> = g("1");
  w[0]$
}"#,
            1,
        ),
        (r#"{ let rd = |s: string| -> 'a str::parse(s)$; str::len(rd("12345")) }"#, 5),
        (
            r#"{ let rd = 'a: Concrete |s: string| -> 'a str::parse(s)$; let n: i64 = rd("7"); n }"#,
            7,
        ),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(expected), "{src}");
    }
    Ok(())
}

/// Every cell `msg` names prints by its short name, `'_` and at most nine
/// digits, never the cell's full generated id.
fn cells_print_short(msg: &str) -> bool {
    msg.split("'_").skip(1).all(|named| {
        (1..10).contains(&named.chars().take_while(char::is_ascii_digit).count())
    })
}

// A generic function's declared bounds print once in its header, by
// name, joined by `+`.
run!(
    declared_bounds_print_in_the_header,
    refused("but implemented fn<'a: Number + Singleton>(x: 'a: unbound within Number & Singleton, y: 'a: unbound within Number & Singleton) -> 'a: unbound within Number & Singleton:"),
    "/test.gx" => "mod m; let result = 0",
    "/test/m.gxi" => "val f: fn(x: string) -> bool;",
    "/test/m.gx" => "let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y"
);

run!(
    several_declared_bounds_print_in_the_header,
    refused("but implemented fn<'a: Eq + Ord + Ordered>(x: 'a: unbound within Eq & Ord & Ordered, y: 'a: unbound within Eq & Ord & Ordered) -> bool:"),
    "/test.gx" => "mod m; let result = 0",
    "/test/m.gxi" => "val f: fn(x: string) -> string;",
    "/test/m.gx" => "let f = 'a: Eq + Ord + Ordered |x: 'a, y: 'a| -> bool x < y"
);

// A read of it no binding names is one instance, which prints no header:
// its variable prints where it occurs.
run!(
    one_read_of_a_generic_function_prints_no_header,
    "{ let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y; let g: fn(x: string) -> bool = f; 0 }",
    refused("type mismatch Number does not contain string")
);

// A comparison takes operands one of whose types holds the other's: `x ==
// 3` over `[i64, null]` compares as a member of the union, and two types
// neither holds are refused.
#[tokio::test(flavor = "current_thread")]
async fn comparison_operands_one_holding_the_other() -> Result<()> {
    for (src, expected) in [
        ("{ let x: [i64, null] = 3; x == 3 }", true),
        ("{ let v: [`Green, `Red] = `Red; v == `Red }", true),
        ("{ let r: [string, Error<`E>] = \"a\"; r != \"a\" }", false),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::Bool(expected), "{src}");
    }
    match eval_modes("1 == \"b\"", crate::TEST_REGISTER).await? {
        Err(msg) => assert!(msg.contains("neither type holds the other"), "{msg}"),
        Ok(v) => panic!("must be refused: {v:?}"),
    }
    Ok(())
}

const COMPARISON_ONE_TYPE: &str = r#"{
  let s: [`Menu, `Edit] = `Menu;
  let t: [`Menu, `Edit] = `Edit;
  let f = 'a: Number + Ordered |a: 'a, b: 'a| -> bool a < b;
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

// A variable the bound of an enclosing definition's variable names is that
// definition's, where a nested lambda declares one of the same name.
run!(a_nested_signatures_own_variable_takes_no_enclosing_bound, r#"{
  let f = 'a: Number, 'z: Array<'a> |w: 'z, v: 'a| {
    let g = 'a: Any |x: 'z, y: 'a| -> 'a y;
    let h = 'a: Any, 'b: Array<'a> |x: 'z, y: 'b| -> 'b y;
    let e = 'a: Any |x: 'z, y: 'a| x[0]$;
    let k = 'a: Any, 'b: fn(x: 'z) -> i64 |y: 'a, c: 'b| -> 'a y;
    let m = 'a: Any |y: 'a| -> 'a { let n: fn(q: 'z) -> i64 = |q| 0; y };
    (g(w, "s"), h(w, ["t"]), e(w, "u"), k("v", |x| 0), m("x"), v)
  };
  f([1], 2)
}"#, prints(r#"["s", ["t"], i64:1, "v", "x", i64:2]"#); FuseExpect::Jit);

// A nested lambda's bound names a variable a definition around it declares,
// and the lambda does not, as that definition's.
run!(
    a_nested_lambdas_bound_names_the_enclosing_variable,
    "{ let f = 'z: Number |w: 'z| -> 'z { \
       let g = 'b: Array<'z> |y: 'b| -> 'b y; let r = g([\"s\"]); w }; f(1) }",
    refused("type mismatch 'z: unbound within Number does not contain string");
    FuseExpect::None
);

run!(
    a_nested_lambdas_bound_takes_the_enclosing_variables_value,
    "{ let f = 'z: Number |w: 'z| { let g = 'b: Array<'z> |y: 'b| -> 'b y; g([w]) }; f(1) }",
    prints("[i64:1]");
    FuseExpect::Jit
);

run!(
    a_nested_signatures_own_variable_holds_no_enclosing_value,
    "{ let f = 'a: Number, 'z: Array<'a> |w: 'z, v: 'a| { \
       let g = 'a: Any |x: 'z, y: 'a| -> 'a x[0]$; g(w, v) }; f([1], 2) }",
    refused("a call picks 'a, and the body's value need not be of the type it picks");
    FuseExpect::None
);

// A nested lambda's own variable stays its own where the bound of an
// enclosing variable it names, or is bounded by, names the enclosing
// variable of the same name.
run!(a_nested_lambdas_own_variable_is_not_the_one_an_enclosing_bound_names, r#"{
  let f = 'e: Array<'q>, 'q: Number |a: 'e, b: 'q| {
    let g = 'q: 'e |x: 'q, y: 'e| -> 'q x;
    let h = 'q: Any |x: 'q, y: 'e| -> 'q x;
    let u = 'q: 'e |x: 'q| -> 'q x;
    (g(a, a), h("s", a), b)
  };
  f([1], 2)
}"#, prints(r#"[[i64:1], "s", i64:2]"#); FuseExpect::Jit);

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

// The orderings, sorts, `min`/`max` and map keys take a type holding two
// numeric types: numbers order by value across kinds. Functions and
// references have no order.
#[tokio::test(flavor = "current_thread")]
async fn ordering_takes_two_numeric_types() -> Result<()> {
    for (src, expected) in [
        ("{ let a: [i64, f64] = 3; let b: [i64, f64] = 2.5; a < b }", "false"),
        (
            "{ let xs: Array<[i64, f64]> = [i64:3, f64:2.5]; array::sort(xs) }",
            "[f64:2.5, i64:3]",
        ),
        ("{ let a: [i64, f64] = 3; let b: [i64, f64] = 2.5; min(a, b) }", "f64:2.5"),
        ("{ type N = [i64, u8]; let n: N = 3; n <= n }", "true"),
        ("{ let f = |a: Number, b: Number| a >= b; f(2.5, 1.5) }", "true"),
        (
            "{ let t: Array<(string, [i64, f64])> = [(\"a\", 1)]; array::sort(t) }",
            "[[\"a\", i64:1]]",
        ),
        ("{ let m: Map<[i64, f64], string> = {}; m }", "{}"),
        (
            "{ let a: [Array<i64>, Array<f64>] = [1]; let b: [Array<i64>, Array<f64>] = [0.5]; a < b }",
            "false",
        ),
        (
            "{ let a: [Map<i64, string>, Map<f64, string>] = {1 => \"a\"}; array::sort([a]) }",
            "[{i64:1 => \"a\"}]",
        ),
        (
            "{ let a: [{x: i64, y: string}, {x: f64, y: bool}] = {x: 1, y: \"a\"}; {a => 1} }",
            "{[[\"x\", i64:1], [\"y\", \"a\"]] => i64:1}",
        ),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v.to_string(), expected, "{src}");
    }
    for (src, says) in [
        ("{ let f = |x: i64| x; array::sort([f, f]) }", "functions have no order"),
        ("{ let r = &1; {r => 1} }", "references have no order"),
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => assert!(msg.contains(says), "{src}: {msg}"),
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    Ok(())
}

// An open member beside a numeric one orders whatever number a call gives
// it; a function there is refused at the call.
#[tokio::test(flavor = "current_thread")]
async fn ordering_takes_an_open_member_beside_a_number() -> Result<()> {
    for (src, expected) in [
        ("{ let g = |y| array::sort([3, y, 1]); g(2.5) }", "[i64:1, f64:2.5, i64:3]"),
        ("{ let h = |y| [3, y] < [1, y]; h(2.5) }", "false"),
        (
            "{ let k = |y| map::insert({}, [1, y], \"v\"); k(2.5) }",
            "{[i64:1, f64:2.5] => \"v\"}",
        ),
        ("{ let g = 'y: Int |y: 'y| array::sort([3, y]); g(i32:2) }", "[i32:2, i64:3]"),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v.to_string(), expected, "{src}");
    }
    let src = "{ let g = |y| array::sort([3, y, 1]); g(|x: i64| x) }";
    match eval_modes(src, crate::TEST_REGISTER).await? {
        Err(msg) => assert!(msg.contains("must be Ordered"), "{msg}"),
        Ok(v) => panic!("must be refused: {v:?}"),
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
// against itself, in a generic definition where it is written, called or
// not.
#[tokio::test(flavor = "current_thread")]
async fn arithmetic_operands_are_one_type() -> Result<()> {
    for src in [
        "{ let x: [i64, f64] = 3; x + 1 }",
        "{ let x: [i64, f64] = 3; x * x }",
        "{ let x: [i64, null] = 3; x - 1 }",
        "{ let f = |a: Number, b: Number| a + b; f(1, 2) }",
        "{ let f = |a: Number, b: Number| a + b; 0 }",
        "{ let f = |a: Number, b| a * b; 0 }",
        "{ let t: (Number, Number) = (1, 2.5); t.0 % t.1 }",
    ] {
        match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains("cannot compute"), "{src}: {msg}");
                let number = "cannot compute with Number: both operands must be one \
                              numeric type";
                assert!(!src.contains("|a: Number") || msg.contains(number), "{msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    let src = "{ let f = 'a: Number |a: 'a, b: 'a| a + b; f(1, 2) }";
    let v = eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
    assert_eq!(v, Value::I64(3), "{src}");
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
// checks it; a member a call leaves open is judged once it binds, and one
// that binds to the other member's type is one type.
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
              g(true, |x: i64| x)
            }"#
            .to_string(),
            "same runtime form",
        ),
    ] {
        match eval_modes(&src, crate::TEST_REGISTER).await? {
            Err(msg) => {
                assert!(msg.contains(says), "{src}: {msg}")
            }
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        }
    }
    let one = r#"{
      let g = |c, a| { let v = select c { true => a, false => |s: string| s }; v == v };
      g(true, |s: string| s)
    }"#;
    let v = eval_modes(one, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
    assert_eq!(v, Value::Bool(true));
    Ok(())
}

// `Result`'s success type holds no error (`'r: NoError`): its error member
// is the only error in it, wherever the success type comes from.
run!(result_success_holding_an_error_refused,
    "{ let r: Result<[i64, Error<`F>], `E> = 1; r }",
    refused("type mismatch Result<[i64, Error<`F>], `E> does not contain i64"); FuseExpect::None);

run!(result_success_bound_reaches_a_generic_call, r#"{
    let succeed = |x: 'a| -> Result<'a, `E> x;
    let e: [i64, Error<`F>] = 1;
    succeed(e)
}"#, refused("NoError does not contain 'a: [i64, Error<`F>]"); FuseExpect::None);

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
    refused("[Array<i64>, Array<string>] is more than one type, where `Singleton` asks for one");
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
    singleton_members_apart_in_declared_variables,
    r#"{ let h = 'a: Singleton |x: 'a| -> 'a x;
         let k = |b, y: 'y, z: 'z| h(select b { true => [y], false => [z] });
         k(false, 1, 2) }"#,
    prints("[i64:2]");
    FuseExpect::Jit
);

// Under `Singleton` the members of a union must be one type at each call:
// two open members given two numeric types are refused, and two declared
// variables are judged where a call decides them.
#[tokio::test(flavor = "current_thread")]
async fn singleton_open_members_are_one_type_at_each_call() -> Result<()> {
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
            "both operands must be one numeric type",
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
            "both operands must be one numeric type",
        ),
        (
            r#"{
              let f = 'a: Number, 'b: Number |c, a: 'a, b: 'b| {
                let s = select c { true => a, false => b };
                s + s
              };
              f(true, 1, 2.5)
            }"#,
            "cannot compute with [i64, f64]",
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

run!(
    singleton_declared_members_one_type_at_the_call,
    r#"{
  let f = 'a: Number, 'b: Number |c, a: 'a, b: 'b| {
    let s = select c { true => a, false => b };
    s + s
  };
  (f(true, 1, 2), f(false, 1.5, 2.5))
}"#,
    prints("[i64:2, f64:5.]")
);

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

run!(rigid_survives_alias, RIGID_SURVIVES_ALIAS, refused("type mismatch i64 does not contain 'a"); FuseExpect::None);

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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    prints("i64:1");
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
    prints("i64:1");
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
    written_variable_written_outside_takes_both_types,
    r#"{
    let y = never();
    let h = |x: 'a| -> 'a { y <- x; x };
    (h(1), h("s"), y)
}"#,
    prints(r#"[i64:1, "s", i64:1]"#);
    FuseExpect::None
);

run!(
    written_variable_written_outside_read_as_one_number_refused,
    r#"{
    let y = never();
    let h = |x: 'a| -> 'a { y <- x; x };
    (h(1), h("s"), y + 1)
}"#,
    refused("both operands must be one numeric type");
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

// A nested signature's own variable stays its own in an instance whose
// signature holds an inferred cell of that name: the parameter's element,
// which `array::len` and the call of `f` named `'a`.
run!(
    written_variable_beside_an_inferred_cell_of_its_name,
    r#"{
  let n: [i64, null] = 1;
  let g = |p| {
    let f = 'b: Array<'a> |x: 'b, y: 'a| x;
    let r = f(p, n);
    let t = f(["s"], "u");
    (array::len(p), r, t)
  };
  (g([null]), g([4]))
}"#,
    prints(r#"[[i64:1, [null], ["s"]], [i64:1, [i64:4], ["s"]]]"#);
    FuseExpect::Jit
);

// A variable an enclosing definition's signature writes is that
// definition's in an instance too, found by its cell: the instance's
// signature holds an inferred cell of its name first.
run!(
    enclosing_variable_beside_an_inferred_cell_of_its_name,
    r#"{
  let g = |q, p: 'a| { let f = |x: 'a| x; (array::len(q), f(p)) };
  (g([null], "s"), g([1], null))
}"#,
    prints(r#"[[i64:1, "s"], [i64:1, null]]"#);
    FuseExpect::Jit
);

run!(
    enclosing_variable_through_two_instances,
    r#"{
  let g = |q, p: 'a| {
    let h = |r, s: 'a| { let f = |x: 'a| -> 'a x; (array::len(r), f(s)) };
    (array::len(q), h(q, p), h([p], p))
  };
  (g([null], "s"), g([1.0], 3))
}"#,
    prints(r#"[[i64:1, [i64:1, "s"], [i64:1, "s"]], [i64:1, [i64:1, i64:3], [i64:1, i64:3]]]"#);
    FuseExpect::Jit
);

run!(
    variable_a_bound_alone_writes_is_the_enclosing_definitions,
    r#"{
  let g = 'b: Array<'a> |x: 'b, q| {
    let f = |y: 'a| -> 'a y;
    (array::len(q), x, f(x[0]$))
  };
  (g([1], [null]), g(["s"], [2.0]))
}"#,
    prints(r#"[[i64:1, [i64:1], i64:1], [i64:1, ["s"], "s"]]"#);
    FuseExpect::Jit
);

// A name only the bound of a bound writes is the enclosing definition's
// variable, as one a parameter's bound writes: a call picks it, so it is
// rigid in the body.
run!(
    variable_a_bound_of_a_bound_writes_is_the_enclosing_definitions,
    r#"{
  let g = 'a0: Array<'a1>, 'a1: Array<'a2> |x: 'a0| {
    let f = |y: 'a2| y;
    f(x[0]$[0]$)
  };
  (g([["s"]]), g([[1]]))
}"#,
    prints(r#"["s", i64:1]"#);
    FuseExpect::Jit
);

run!(
    variable_a_bound_of_a_bound_writes_is_rigid_in_the_body,
    r#"{
  let g = 'a0: Array<'a1>, 'a1: Array<'a2> |x: 'a0| {
    let f = |y: 'a2| y;
    f(1)
  };
  g([["s"]])
}"#,
    refused("'a2: unbound does not contain i64");
    FuseExpect::None
);

run!(
    default_naming_an_enclosing_variable_beside_an_inferred_cell,
    r#"{
  let g = |q, p: 'a| { let f = |#d: 'a = p, y: i64| d; (array::len(q), f(1)) };
  g([null], "s")
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

// A lambda no binding names is its one instance, so the variable it
// declares stands bound in its signature. In an instance of the
// definition around it the name is still the lambda's, never that
// definition's variable of the same name.
run!(
    one_instance_lambdas_variable_shadows_in_a_nested_signature,
    r#"{
  let g = |p: 'a, n: i64| {
    let r = array::map([(p, n)], 'a: Any |s: 'a| { let f = |x: 'a| -> 'a x; f(s) });
    (p, r)
  };
  (g("s", 1), g(null, 2))
}"#,
    prints(r#"[["s", [["s", i64:1]]], [null, [[null, i64:2]]]]"#);
    FuseExpect::Jit
);

run!(
    one_instance_lambdas_variable_shadows_in_a_body_annotation,
    r#"{
  let g = |p: 'a, n: i64| {
    let r = array::map([n], 'a: Any |s: 'a| { let v: 'a = s; v });
    (p, r)
  };
  (g("s", 1), g(null, 2))
}"#,
    prints(r#"[["s", [i64:1]], [null, [i64:2]]]"#);
    FuseExpect::Jit
);

run!(
    one_instance_lambdas_variable_shadows_in_a_default,
    r#"{
  let g = |p: 'a, n: i64| {
    let r = array::map([n], 'a: Any |s: 'a| { let f = |#d: 'a = s, y: i64| d; f(1) });
    (p, r)
  };
  (g("s", 1), g(null, 2))
}"#,
    prints(r#"[["s", [i64:1]], [null, [i64:2]]]"#);
    FuseExpect::Jit
);

run!(
    returned_lambdas_variable_shadows_at_a_run_time_bind,
    r#"{
  let g = |p: 'a, n: i64| {
    let mk = |z| 'a: Any |s: 'a| { let f = |x: 'a| -> 'a { let v: 'a = x; v }; f(s) };
    let h = mk(0);
    (p, h(n))
  };
  (g("s", 1), g(null, 2))
}"#,
    prints(r#"[["s", i64:1], [null, i64:2]]"#);
    FuseExpect::None
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

run!(
    union_trait_self_argument,
    UNION_TRAIT_SELF_ARGUMENT,
    refused("type mismatch Comb does not contain [A, B]");
    FuseExpect::None
);

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

// A member of several primitive types that no constructor member of the
// holder takes whole goes a type at a time: each type to the constructor
// member that takes it and only the rest to the open member, so 'a holds no
// null here and the union written out checks as its alias spelling does.
run!(
    covered_primitive_stays_out_of_the_open_member,
    r#"{
  let f = |x: ['a, null]| -> 'a x$;
  let show = |v: [Array<i64>, i64, null]| select f(v) { i64 as i => i, Array<i64> as a => array::len(a) };
  (show(7), show([1, 2]))
}"#,
    prints("[i64:7, i64:2]")
);
run!(
    covered_primitive_stays_out_of_the_open_member_arm_refused,
    r#"{
  let f = |x: ['a, null]| -> 'a x$;
  let v: [Array<i64>, i64, null] = 1;
  select f(v) { null as _ => 0, i64 as i => i, Array<i64> as a => 2 }
}"#,
    refused("pattern null will never match: nothing reaches it (4:10; the value is [i64, Array<i64>])");
    FuseExpect::None
);
run!(
    covered_primitive_leaves_a_bounded_open_member_its_numbers,
    r#"{
  let f = 'a: Number |x: [['a, null], Array<i64>]| x;
  let v: [i64, null, Array<i64>] = 1;
  let w: [f64, i64, null, Array<i64>] = 1.5;
  (f(v), f(w))
}"#,
    prints("[i64:1, f64:1.5]")
);
run!(
    uncovered_primitive_refused_by_the_open_members_bound,
    r#"{
  let f = 'a: Number |x: [['a, null], Array<i64>]| x;
  let v: [i64, string, null, Array<i64>] = 1;
  f(v)
}"#,
    refused("type mismatch Number does not contain [i64, string]");
    FuseExpect::None
);
run!(
    open_member_beside_a_covered_primitive_widens_to_a_wider_argument,
    r#"{
  let f = |x: ['a, null], y: 'a| -> 'a select x { null as _ => y, v => v };
  let v: [Array<i64>, i64, null] = null;
  let w: [Array<i64>, i64, null] = [9];
  (f(v, 3), f(v, w))
}"#,
    prints("[i64:3, [i64:9]]")
);
run!(
    open_member_beside_a_covered_primitive_widens_to_a_nullable_argument,
    r#"{
  let f = |x: ['a, null], y: 'a| -> 'a select x { null as _ => y, v => v };
  let v: [Array<i64>, i64, null] = null;
  let n: [i64, null] = null;
  f(v, n)
}"#,
    prints("null");
    FuseExpect::Jit
);

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

/// A typedef whose parameter grows as it recurses unfolds to ever larger
/// types: refused at its definition, never a walk without end.
const GROWING_TYPEDEF_REFUSED: &str = r#"
{
    type N<'a> = [null, ('a, N<Array<'a>>)];
    let n1: N<i64> = null;
    let n2: N<[i64, string]> = n1;
    n2
}
"#;

run!(
    growing_typedef_refused,
    GROWING_TYPEDEF_REFUSED,
    refused("recursive type N grows its parameter 'a as it recurses");
    FuseExpect::None
);

// A `use` that completes a growing cycle through a typedef pending since
// before a later typedef refuses it; passed on unwrapped, the cycle is
// regular.
run!(
    growing_typedef_completed_by_use_refused,
    refused("recursive type A grows its parameter 'a as it recurses"),
    "/test.gx" => "type A<'a> = [`N, `C(X<Array<'a>>)]; type B<'b> = A<'b>; \
        use self::B as X; let x: A<i64> = `N; let result = 0"
; FuseExpect::None);

run!(
    regular_typedef_completed_by_use,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1))),
    "/test.gx" => "type A<'a> = [`N, `C(X<'a>)]; type B<'b> = A<'b>; \
        use self::B as X; let x: A<i64> = `N; let result = select x { `N => 1, `C(_) => 2 }"
; FuseExpect::Jit);

/// A parameter that doubles at each level (a perfect tree) is refused
/// at its definition, so no value of it reaches a select or a freeze.
const DOUBLING_TYPEDEF_REFUSED: &str = r#"
{
    type P<'a> = [`Leaf('a), `Node(P<('a, 'a)>)];
    let t: P<i64> = `Node(`Node(`Node(`Node(`Leaf(((((1, 2), (3, 4)), ((5, 6), (7, 8))), (((1, 2), (3, 4)), ((5, 6), (7, 8)))))))));
    select t { `Leaf(_) => 0, `Node(_) => 1 }
}
"#;

run!(
    doubling_typedef_refused,
    DOUBLING_TYPEDEF_REFUSED,
    refused("recursive type P grows its parameter 'a as it recurses");
    FuseExpect::None
);

/// The same through a struct field.
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
    refused("recursive type T grows its parameter 'a as it recurses");
    FuseExpect::None
);

/// An alias applied to a recursive type's parameter wraps nothing: the
/// type is the one with the alias's union written out, and every walk
/// over it ends (a type test, a cast, an equality, a map key, a generic
/// function over an open instance).
const ALIAS_OVER_A_RECURSIVE_PARAMETER: &str = r#"
{
    type Opt<'a> = ['a, null];
    type T<'a> = [`N, `C('a, T<Opt<'a>>)];
    type W<'a> = [`N, `C('a, W<['a, null]>)];
    let x: T<i64> = `C(1, `C(null, `C(3, `N)));
    let w: W<i64> = x;
    let back: T<i64> = w;
    let deep: T<Opt<Opt<i64>>> = back;
    let flat: T<Opt<i64>> = deep;
    let tested = select w { T<i64> as t => 1, _ => 0 };
    let c = cast<T<i64>>(`C(1, `C(null, `N)))$;
    let m: Map<T<i64>, i64> = {c => 10};
    let second = |t: T<'q>| select t { `C(_, `C(h, _)) => h, _ => never() };
    let n = select second(flat) { null as _ => 100, i64 as i => i };
    let same = select x == back { true => 1000, false => 0 };
    tested + m{c}$ + n + same
}
"#;

run!(
    alias_over_a_recursive_parameter,
    ALIAS_OVER_A_RECURSIVE_PARAMETER,
    refused("pattern _ will never match: nothing reaches it")
);

/// An alias of an alias, the identity alias and a 50-member alias are
/// looked through alike, and through a second definition on the way back.
const ALIASES_OVER_A_RECURSIVE_PARAMETER: &str = r#"
{
    type Big<'a> = ['a, null, `M0(i64), `M1(i64), `M2(i64), `M3(i64), `M4(i64), `M5(i64), `M6(i64), `M7(i64), `M8(i64), `M9(i64), `M10(i64), `M11(i64), `M12(i64), `M13(i64), `M14(i64), `M15(i64), `M16(i64), `M17(i64), `M18(i64), `M19(i64), `M20(i64), `M21(i64), `M22(i64), `M23(i64), `M24(i64), `M25(i64), `M26(i64), `M27(i64), `M28(i64), `M29(i64), `M30(i64), `M31(i64), `M32(i64), `M33(i64), `M34(i64), `M35(i64), `M36(i64), `M37(i64), `M38(i64), `M39(i64), `M40(i64), `M41(i64), `M42(i64), `M43(i64), `M44(i64), `M45(i64), `M46(i64), `M47(i64), `M48(i64), `M49(i64)];
    type Opt<'a> = ['a, null];
    type Twice<'a> = Opt<Opt<'a>>;
    type I<'b> = J<'b>;
    type J<'c> = 'c;
    type B<'a> = [`N, `C('a, B<Big<'a>>)];
    type B2<'a> = [`N, `C('a, B2<Big<'a>>)];
    type U<'a> = [`N, `C('a, U<Twice<'a>>)];
    type V<'a> = [`N('a), `C(V<I<'a>>)];
    type V2<'a> = [`N('a), `C(V2<'a>)];
    type X<'a> = [`N('a), `C(Y<'a>)];
    type Y<'b> = X<Opt<'b>>;
    type X2<'a> = [`N('a), `C(X2<['a, null]>)];
    let b: B<i64> = `C(1, `C(`M7(3), `C(null, `N)));
    let b2: B2<i64> = b;
    let u: U<string> = `C("a", `C(null, `N));
    let v: V<i64> = `C(`C(`N(1)));
    let v2: V2<i64> = v;
    let x: X<i64> = `C(`C(`N(null)));
    let x2: X2<i64> = x;
    let y: Y<i64> = x2;
    let t0 = select b2 { B<i64> as t => 1, _ => 0 };
    let t1 = select v2 { V<i64> as t => 10, _ => 0 };
    let t2 = select y { X<Opt<i64>> as t => 100, _ => 0 };
    let t3 = select u { `C(_, `C(h, _)) => select h { null as _ => 1000, string as _ => 0 }, _ => 0 };
    t0 + t1 + t2 + t3
}
"#;

run!(
    aliases_over_a_recursive_parameter,
    ALIASES_OVER_A_RECURSIVE_PARAMETER,
    refused("pattern _ will never match: nothing reaches it")
);

/// The same under an abstract type's representation.
const ALIAS_OVER_AN_ABSTRACT_PARAMETER: &str = r#"
{
    type Opt<'a> = ['a, null];
    type W<'a> = Abstract<[`N('a), `C(W<Opt<'a>>)]>;
    let v: W<i64> = W(`C(W(`N(null))));
    let inner = select v.0 { `C(t) => select t.0 { `N(x) => x, `C(_) => 7 }, `N(x) => x };
    let same = v == v;
    select inner { null as _ => select same { true => 1, false => 0 }, i64 as _ => 0 }
}
"#;

run!(
    alias_over_an_abstract_parameter,
    ALIAS_OVER_AN_ABSTRACT_PARAMETER,
    is(Value::I64(1))
);

/// An alias applied to a declared variable is its union there too: a
/// union holding it as a member is the union with the alias written out,
/// bare, under a constructor and under a named type, so a value of one
/// spelling is a value of the other.
const ALIAS_OVER_A_DECLARED_VARIABLE: &str = r#"
{
    type Opt<'b> = ['b, null];
    type V<'a> = [`Nil, `C('a, V<'a>)];
    type U<'a> = [`Nil, `C('a, U<Opt<'a>>)];
    let f = |x: [Opt<'a>, null]| -> Opt<'a> x;
    let g = |x: [Opt<'a>, null]| -> ['a, null] x;
    let h = |x: Array<[Opt<'a>, null]>| -> Array<Opt<'a>> x;
    let k = |x: V<[Opt<'a>, null]>| -> V<Opt<'a>> x;
    let l = |x: U<[Opt<'a>, null]>| -> U<Opt<'a>> x;
    let m = |x: U<Opt<Opt<'a>>>| -> U<Opt<'a>> x;
    let a = select f(1) { null as _ => 0, i64 as i => i };
    let b = select g(null) { null as _ => 10, i64 as i => i };
    let c = select h([3, null])[0]$ { null as _ => 0, i64 as i => i * 100 };
    let d = select k(`C(4, `Nil)) { `C(v, _) => select v { null as _ => 0, i64 as i => i * 1000 }, `Nil => 0 };
    let e = select l(`C(5, `Nil)) { `C(v, _) => select v { null as _ => 0, i64 as i => i * 10000 }, `Nil => 0 };
    let deep: U<Opt<Opt<i64>>> = `C(null, `Nil);
    let n = select m(deep) { `C(v, _) => select v { null as _ => 600000, i64 as i => i }, `Nil => 0 };
    a + b + c + d + e + n
}
"#;

run!(
    alias_over_a_declared_variable,
    ALIAS_OVER_A_DECLARED_VARIABLE,
    is(Value::I64(654311))
);

/// A recursive helper over a type that recurs through an alias of its
/// parameter names the tail's type, the alias applied to its own
/// variable, and calls itself on the tail, in either spelling.
const RECURSIVE_HELPER_OVER_AN_ALIAS: &str = r#"
{
    type Opt<'b> = ['b, null];
    type U<'a> = [`Nil, `C('a, U<Opt<'a>>)];
    type X<'a> = [`Nil, `C('a, X<['a, null]>)];
    let rec go = |v: U<Opt<'a>>, n: i64| -> i64 select v { `Nil => n, `C(_, t) => go(t, n + 1) };
    let rec written = |v: X<['a, null]>, n: i64| -> i64 select v { `Nil => n, `C(_, t) => written(t, n + 1) };
    let rec mixed = |v: X<Opt<'a>>, n: i64| -> i64 select v { `Nil => n, `C(_, t) => mixed(t, n + 1) };
    let u: U<Opt<i64>> = `C(1, `C(null, `C(2, `Nil)));
    let x: X<[i64, null]> = `C(1, `C(null, `Nil));
    go(u, 0) + written(x, 0) * 10 + mixed(u, 0) * 100
}
"#;

run!(recursive_helper_over_an_alias, RECURSIVE_HELPER_OVER_AN_ALIAS, is(Value::I64(323)));

/// The same helper inside an impl, over the impl's own variable.
const RECURSIVE_HELPER_OVER_AN_ALIAS_IN_AN_IMPL: &str = r#"
{
    type Opt<'b> = ['b, null];
    type U<'a> = [`Nil, `C('a, U<Opt<'a>>)];
    type W<'a> = Abstract<U<'a>>;
    trait Len { val len: fn(self) -> i64; val push: fn(self, x: 'q) -> W<'q> };
    impl<'a> Len for W<'a> {
        let len = |s: self| -> i64 {
            let rec go = |v: U<Opt<'a>>, n: i64| -> i64 select v { `Nil => n, `C(_, t) => go(t, n + 1) };
            select s.0 { `Nil => 0, `C(_, t) => go(t, 1) }
        };
        let push = |s: self, x| W(`C(x, `Nil))
    };
    let u: W<i64> = W(`C(1, `C(null, `C(2, `Nil))));
    let w: W<string> = W(`C("a", `Nil));
    Len::len(u) + Len::len(w) * 10 + Len::len(Len::push(w, 3.0)) * 100
}
"#;

run!(
    recursive_helper_over_an_alias_in_an_impl,
    RECURSIVE_HELPER_OVER_AN_ALIAS_IN_AN_IMPL,
    is(Value::I64(113))
);

/// A union holding an alias applied to a declared variable beside
/// primitives of its own is the union with the alias written out: the
/// alias's `null` and the union's `i64` are one member, so a value of
/// `[i64, null]` goes into it.
const ALIAS_BESIDE_PRIMITIVES: &str = r#"
{
    type Opt<'b> = ['b, null];
    let f = |x: [Opt<'a>, i64, null]| -> [Opt<'a>, i64] x;
    let g = |x: [Opt<'a>, f64, i64, null]| -> [Opt<'a>, i64, f64] x;
    let h = |x: Array<[Opt<'a>, i64, null]>| -> Array<[Opt<'a>, i64]> x;
    let a = select f("s") { string as _ => 1, _ => 0 };
    let b = select f(7) { i64 as i => i * 10, _ => 0 };
    let c = select g(2.5) { f64 as _ => 100, _ => 0 };
    let d = select h(["s", null, 3])[2]$ { i64 as i => i * 1000, _ => 0 };
    a + b + c + d
}
"#;

run!(alias_beside_primitives, ALIAS_BESIDE_PRIMITIVES, is(Value::I64(3171)));

const UNION_BESIDE_PRIMITIVES: &str = r#"
{
    let f = |x: [['a, null], i64, null]| -> [['a, null], i64] x;
    let g = |x: [['a, null], f64, i64, null]| -> [['a, null], i64, f64] x;
    let h = |x: Array<[['a, null], i64, null]>| -> Array<[['a, null], i64]> x;
    let a = select f("s") { string as _ => 1, _ => 0 };
    let b = select f(7) { i64 as i => i * 10, _ => 0 };
    let c = select g(2.5) { f64 as _ => 100, _ => 0 };
    let d = select h(["s", null, 3])[2]$ { i64 as i => i * 1000, _ => 0 };
    a + b + c + d
}
"#;

run!(union_beside_primitives, UNION_BESIDE_PRIMITIVES, is(Value::I64(3171)));

/// A bounded variable under an alias, in a union with another member,
/// takes its own part of an argument: `[Opt<'a>, string]` holds
/// `[Opt<i64>, string]` at `'a = i64`, and `[Opt<'a>, Array<i64>]` holds
/// `[i64, null, Array<i64>]` so, the null going to the alias's own, in
/// the check and in the instance; an argument that decides nothing of
/// `'a` builds its instance too.
const BOUNDED_VARIABLE_UNDER_AN_ALIAS: &str = r#"
{
    type Opt<'b> = ['b, null];
    let f = 'a: Number |x: [Opt<'a>, string]| x;
    let g = 'a: Number |x: [Opt<'a>, string, null]| -> [Opt<'a>, string] x;
    let h = 'a: Number |x: [Opt<'a>, Array<i64>]| x;
    let v: [Opt<i64>, string] = 1;
    let w: [i64, null, Array<i64>] = 5;
    let n: [Opt<i64>, Array<i64>] = 6;
    let a = select f(1) { i64 as i => i, _ => 0 };
    let b = select f(v) { i64 as i => i * 10, _ => 0 };
    let c = select g(2.5) { f64 as _ => 100, _ => 0 };
    let d = select g("s") { string as _ => 1000, _ => 0 };
    let e = select h(4) { i64 as i => i * 10000, _ => 0 };
    let k = select h(w) { i64 as i => i * 100000, _ => 0 };
    let l = select h(n) { i64 as i => i * 1000000, _ => 0 };
    a + b + c + d + e + k + l
}
"#;

run!(
    bounded_variable_under_an_alias,
    BOUNDED_VARIABLE_UNDER_AN_ALIAS,
    is(Value::I64(6541111))
);

const BOUNDED_VARIABLE_UNDER_A_UNION: &str = r#"
{
    let f = 'a: Number |x: [['a, null], string]| x;
    let g = 'a: Number |x: [['a, null], string, null]| -> [['a, null], string] x;
    let h = 'a: Number |x: [['a, null], Array<i64>]| x;
    let v: [[i64, null], string] = 1;
    let a = select f(1) { i64 as i => i, _ => 0 };
    let b = select f(v) { i64 as i => i * 10, _ => 0 };
    let c = select g(2.5) { f64 as _ => 100, _ => 0 };
    let d = select g("s") { string as _ => 1000, _ => 0 };
    let e = select h(4) { i64 as i => i * 10000, _ => 0 };
    a + b + c + d + e
}
"#;

run!(
    bounded_variable_under_a_union,
    BOUNDED_VARIABLE_UNDER_A_UNION,
    is(Value::I64(41111))
);

/// An alias whose body holds its parameter under a constructor still
/// grows the type, whatever else its union adds.
const ALIAS_OVER_A_CONSTRUCTOR_GROWS: &str = r#"
{
    type M<'b> = ['b, Array<'b>];
    type A<'a> = [`N('a), `C(A<M<'a>>)];
    let v: A<i64> = `N(1);
    0
}
"#;

run!(
    alias_over_a_constructor_grows,
    ALIAS_OVER_A_CONSTRUCTOR_GROWS,
    refused("recursive type A grows its parameter 'a as it recurses");
    FuseExpect::None
);

/// An alias is looked through where it is applied: one defined below the
/// recursive type is named, and defined above it the type checks
/// (`alias_over_a_recursive_parameter`).
const ALIAS_BELOW_ITS_RECURSIVE_APPLICATION: &str = r#"
{
    type T<'a> = [`N, `C('a, T<Opt<'a>>)];
    type Opt<'a> = ['a, null];
    let x: T<i64> = `C(1, `C(null, `N));
    0
}
"#;

run!(
    alias_below_its_recursive_application_refused,
    ALIAS_BELOW_ITS_RECURSIVE_APPLICATION,
    refused("passes its parameter 'a back to itself through Opt, which is not defined above the type that applies it: define Opt first");
    FuseExpect::None
);

// A name a module declares is its type in a typedef above the declaration
// too, never the one a `use` or the prelude brings: the cycle the later
// typedef completes is found, and the alias is one not defined above.
run!(
    typedef_names_its_modules_later_type_over_an_import,
    refused("recursive type M grows its parameter 'a as it recurses"),
    "/test.gx" => "type M<'a> = Array<'a>; mod m; let result = 0",
    "/test/m.gx" => "use super::M; type N<'a> = [`Nil, `Cons('a, M<'a>)]; \
        type M<'a> = N<Array<'a>>; let x: N<i64> = `Nil"
; FuseExpect::None);

run!(
    typedef_names_its_modules_later_type_over_the_prelude,
    refused("recursive type Result grows its parameter 'a as it recurses"),
    "/test.gx" => "type N<'a> = [`Nil, `Cons('a, Result<'a, `E>)]; \
        type Result<'a, 'b> = [N<Array<'a>>, 'b]; let x: N<i64> = `Nil; let result = 0"
; FuseExpect::None);

run!(
    typedef_names_its_parents_later_type,
    refused("recursive type M grows its parameter 'a as it recurses"),
    "/test.gx" => "mod m; type M<'a> = m::N<Array<'a>>; let result = 0",
    "/test/m.gx" => "use super::M; type N<'a> = [`Nil, `Cons('a, M<'a>)]"
; FuseExpect::None);

run!(
    alias_shadowing_an_import_below_its_application_refused,
    refused("passes its parameter 'a back to itself through Opt, which is not defined above the type that applies it: define Opt first"),
    "/test.gx" => "type Opt<'a> = ['a, null]; mod m; let result = m::x",
    "/test/m.gx" => "use super::Opt; type T<'a> = [`N, `C(Opt<'a>, T<Opt<'a>>)]; \
        type Opt<'a> = ['a, string]; let x: T<i64> = `C(\"s\", `C(null, `N))"
; FuseExpect::None);

run!(
    alias_shadowing_an_import_above_its_application_refuses_the_imports_member,
    refused("does not contain"),
    "/test.gx" => "type Opt<'a> = ['a, null]; mod m; let result = m::x",
    "/test/m.gx" => "use super::Opt; type Opt<'a> = ['a, string]; \
        type T<'a> = [`N, `C(Opt<'a>, T<Opt<'a>>)]; let x: T<i64> = `C(\"s\", `C(null, `N))"
; FuseExpect::None);

run!(
    alias_shadowing_an_import_above_its_application,
    is(Value::I64(2)),
    "/test.gx" => "type Opt<'a> = ['a, null]; mod m; let result = m::n",
    "/test/m.gx" => "use super::Opt; type Opt<'a> = ['a, string]; \
        type T<'a> = [`N, `C(Opt<'a>, T<Opt<'a>>)]; let x: T<i64> = `C(\"s\", `C(1, `N)); \
        let n = select x { `C(_, `C(_, _)) => 2, `C(_, `N) => 1, `N => 0 }"
);

// A name a later `use` of the list imports, or a module declared below
// may bring through a `use` above, is not read before then: a typedef
// above waits for it, so the cycle the import completes is found, and a
// statement that needs the type at once is refused.
const IMPORTED_CYCLE: &str = "use super::N; type Option<'a> = N<Array<'a>>";
const IMPORTED_OPTION: &str = "type Option<'a> = ['a, string]";
const GROWS_N: &str = "recursive type N grows its parameter 'a as it recurses";
const GROWS_OPTION: &str = "recursive type Option grows its parameter 'a as it recurses";
const NO_VALUE: &str =
    "recursive type T refers back to itself through unions and aliases alone";

run!(
    typedef_above_the_use_of_a_name_it_reads_refused,
    refused(GROWS_N),
    "/test.gx" => "type N<'a> = [`Nil, `Cons('a, Option<'a>)]; mod o; use o::Option; \
        let result = 0",
    "/test/o.gx" => IMPORTED_CYCLE
; FuseExpect::None);

run!(
    typedef_above_the_use_of_a_name_it_reads,
    is(Value::I64(1)),
    "/test.gx" => "type N<'a> = {v: Option<'a>}; mod o; use o::Option; \
        let y: N<f64> = {v: \"s\"}; \
        let result = select y.v { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    typedef_between_a_use_and_the_module_it_imports_from_refused,
    refused(GROWS_OPTION),
    "/test.gx" => "use o::Option; type N<'a> = [`Nil, `Cons('a, Option<'a>)]; mod o; \
        let result = 0",
    "/test/o.gx" => IMPORTED_CYCLE
; FuseExpect::None);

run!(
    typedef_between_a_use_and_the_module_it_imports_from,
    is(Value::I64(1)),
    "/test.gx" => "use o::Option; type N<'a> = {v: Option<'a>}; mod o; \
        let y: N<f64> = {v: \"s\"}; \
        let result = select y.v { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    typedef_above_a_glob_refused,
    refused(GROWS_N),
    "/test.gx" => "type N<'a> = [`Nil, `Cons('a, Option<'a>)]; mod o; use o::*; \
        let result = 0",
    "/test/o.gx" => IMPORTED_CYCLE
; FuseExpect::None);

run!(
    typedef_above_a_glob,
    is(Value::I64(1)),
    "/test.gx" => "type N<'a> = {v: Option<'a>}; mod o; use o::*; \
        let y: N<f64> = {v: \"s\"}; \
        let result = select y.v { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    typedef_between_a_glob_and_its_module_refused,
    refused(GROWS_OPTION),
    "/test.gx" => "use o::*; type N<'a> = [`Nil, `Cons('a, Option<'a>)]; mod o; \
        let result = 0",
    "/test/o.gx" => IMPORTED_CYCLE
; FuseExpect::None);

run!(
    typedef_between_a_glob_and_its_module,
    is(Value::I64(1)),
    "/test.gx" => "use o::*; type N<'a> = {v: Option<'a>}; mod o; \
        let y: N<f64> = {v: \"s\"}; \
        let result = select y.v { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    typedef_reading_a_globs_name_above_the_use_that_takes_it_over_refused,
    refused("recursive type Opt grows its parameter 'a as it recurses"),
    "/test.gx" => "mod a; mod b; use a::*; type N<'a> = [`Nil, `Cons('a, Opt<'a>)]; \
        use b::Opt; let result = 0",
    "/test/a.gx" => "type Opt<'a> = ['a, null]",
    "/test/b.gx" => "use super::N; type Opt<'a> = N<Array<'a>>"
; FuseExpect::None);

run!(
    block_typedef_above_the_blocks_use_refused,
    refused(NO_VALUE),
    "/test.gx" => "type Foo<'a> = Array<'a>; mod o; \
        let result = { type T = [i64, Foo<T>]; use o::Foo; let x: T = 1; 0 }",
    "/test/o.gx" => "type Foo<'a> = ['a, string]"
; FuseExpect::None);

run!(
    block_typedef_above_the_blocks_use,
    is(Value::I64(1)),
    "/test.gx" => "type Foo<'a> = Array<'a>; mod o; \
        let result = { type T = [i64, Foo<T>]; use o::Foo; let y: T = (\"s\", 1); \
        select y { i64 as _ => 0, _ => 1 } }",
    "/test/o.gx" => "type Foo<'a> = (string, 'a)"
);

run!(
    nested_block_typedef_above_an_outer_use_refused,
    refused(NO_VALUE),
    "/test.gx" => "type Foo<'a> = Array<'a>; mod o; \
        let result = { let q = { type T = [i64, Foo<T>]; let x: T = 1; 0 }; \
        use o::Foo; q }",
    "/test/o.gx" => "type Foo<'a> = ['a, string]"
; FuseExpect::None);

run!(
    typedef_through_an_imported_module_declared_below_refused,
    refused(NO_VALUE),
    "/test.gx" => "mod p; \
        let r = { use o::p; type T = [i64, p::Foo<T>]; let x: T = 1; 0 }; \
        mod o; let result = r",
    "/test/p.gx" => "type Foo<'a> = Array<'a>",
    "/test/o.gx" => "mod p",
    "/test/o/p.gx" => "type Foo<'a> = ['a, string]"
; FuseExpect::None);

run!(
    typedef_through_an_imported_module_declared_below,
    is(Value::I64(1)),
    "/test.gx" => "mod p; \
        let r = { use o::p; type T = [i64, p::Foo<T>]; let y: T = (\"s\", 1); \
        select y { i64 as _ => 0, _ => 1 } }; \
        mod o; let result = r",
    "/test/p.gx" => "type Foo<'a> = Array<'a>",
    "/test/o.gx" => "mod p",
    "/test/o/p.gx" => "type Foo<'a> = (string, 'a)"
);

run!(
    let_annotation_above_the_use_reads_the_import,
    is(Value::I64(1)),
    "/test.gx" => "let x: Option<f64> = \"s\"; mod o; use o::Option; \
        let result = select x { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    type_test_above_the_use_refused,
    refused("Option names what a later `use` of this module imports: write the `use` before the name's first use"),
    "/test.gx" => "let v: [f64, string, null] = \"s\"; \
        let result = select v { Option<f64> as _ => 1, _ => 0 }; mod o; use o::Option",
    "/test/o.gx" => IMPORTED_OPTION
; FuseExpect::None);

run!(
    type_test_below_the_use,
    is(Value::I64(1)),
    "/test.gx" => "mod o; use o::Option; let v: [f64, string, null] = \"s\"; \
        let result = select v { Option<f64> as _ => 1, _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    cast_above_the_use_refused,
    refused("Option names what a later `use` of this module imports"),
    "/test.gx" => "let result = cast<Option<f64>>(\"s\"); mod o; use o::Option",
    "/test/o.gx" => IMPORTED_OPTION
; FuseExpect::None);

run!(
    destructuring_let_above_the_blocks_use_refused,
    refused("Pair names what a later `use` of this block imports"),
    "/test.gx" => "type Pair = (i64, i64); mod o; \
        let result = { let (a, b): Pair = (1, \"s\"); use o::Pair; a }",
    "/test/o.gx" => "type Pair = (i64, string)"
; FuseExpect::None);

run!(
    destructuring_let_above_a_glob_refused,
    refused("P may name what a later glob `use` of this module imports: write the `use` before the name's first use"),
    "/test.gx" => "let (a, b): P = (1, \"s\"); mod o; use o::*; let result = a",
    "/test/o.gx" => "type P = (i64, string)"
; FuseExpect::None);

run!(
    destructuring_let_between_a_glob_and_its_module_refused,
    refused("P may name what a glob `use` of this module imports from module `/test/o`, which a later statement declares"),
    "/test.gx" => "use o::*; let (a, b): P = (1, \"s\"); mod o; let result = a",
    "/test/o.gx" => "type P = (i64, string)"
; FuseExpect::None);

run!(
    destructuring_let_between_a_use_and_its_module_refused,
    refused("P names what a `use` of this module imports from module `/test/o`, which a later statement declares"),
    "/test.gx" => "use o::P; let (a, b): P = (1, \"s\"); mod o; let result = a",
    "/test/o.gx" => "type P = (i64, string)"
; FuseExpect::None);

run!(
    destructuring_let_below_the_use_and_its_module,
    is(Value::I64(1)),
    "/test.gx" => "mod o; use o::P; let (a, b): P = (1, \"s\"); let result = a",
    "/test/o.gx" => "type P = (i64, string)"
);

// A trait as a parameter's type is taken for a bound as the function or
// the signature compiles, so above a `use` that may change the name it is
// refused; a type there is read at the check, and so is a declared bound.
run!(
    trait_parameter_above_a_glob_refused,
    refused("Show may name what a later glob `use` of this block imports: write the `use` before the name's first use"),
    "/test.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s }; \
        let result = { let f = |x: Show| Show::show(x); let v = f(1); use array::*; v }"
; FuseExpect::None);

run!(
    trait_parameter_below_the_glob,
    is(Value::I64(1)),
    "/test.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s }; \
        let result = { use array::*; let f = |x: Show| Show::show(x); f(1) }"
);

run!(
    trait_parameter_above_the_use_of_its_name_refused,
    refused("Show names what a later `use` of this block imports: write the `use` before the name's first use"),
    "/test.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s }; mod o; \
        let result = { let f = |x: Show| Show::show(x); let v = f(1); use o::Show; v }",
    "/test/o.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s + 10 }"
; FuseExpect::None);

run!(
    signature_trait_parameter_above_a_glob_refused,
    refused("Show may name what a later glob `use` of this block imports"),
    "/test.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s }; \
        let result = { trait Twice { val twice: fn(self, x: Show) -> i64 }; \
        impl Twice for i64 { let twice = |s, x| Show::show(x) + s }; \
        let v = Twice::twice(1, 1); use array::*; v }"
; FuseExpect::None);

run!(
    signature_trait_parameter_below_the_glob,
    is(Value::I64(2)),
    "/test.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s }; \
        let result = { use array::*; trait Twice { val twice: fn(self, x: Show) -> i64 }; \
        impl Twice for i64 { let twice = |s, x| Show::show(x) + s }; \
        Twice::twice(1, 1) }"
);

run!(
    declared_trait_bound_above_a_glob,
    is(Value::I64(1)),
    "/test.gx" => "trait Show { val show: fn(self) -> i64 }; \
        impl Show for i64 { let show = |s| s }; \
        let result = { let f = 'a: Show |x: 'a| Show::show(x); let v = f(1); use array::*; v }"
);

run!(
    lambda_parameter_above_the_use_reads_the_import,
    is(Value::I64(1)),
    "/test.gx" => "mod o; let f = |x: Option<f64>| x; use o::Option; \
        let result = select f(\"s\") { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    lambda_parameter_above_the_module_it_imports_from_refused,
    refused("Option names what a `use` of this module imports from module `/test/o`, which a later statement declares: declare the module before the name's first use"),
    "/test.gx" => "let f = |x: Option<f64>| x; mod o; use o::Option; \
        let result = select f(\"s\") { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
; FuseExpect::None);

run!(
    lambda_body_annotation_above_the_module_it_imports_from_refused,
    refused("Option names what a `use` of this module imports from module `/test/o`, which a later statement declares"),
    "/test.gx" => "let f = |a: [f64, string]| { let x: Option<f64> = a; x }; mod o; \
        use o::Option; let result = 0",
    "/test/o.gx" => IMPORTED_OPTION
; FuseExpect::None);

run!(
    typedef_in_a_body_above_the_module_it_imports_from_refused,
    refused("recursive type T refers back to itself through unions and aliases alone"),
    "/test.gx" => "type Foo<'a> = Array<'a>; \
        let r = { let f = |a: i64| { type T = [i64, Foo<T>]; let x: T = a; x }; \
        use o::Foo; f(1) }; mod o; let result = 0",
    "/test/o.gx" => "type Foo<'a> = ['a, string]"
; FuseExpect::None);

run!(
    typedef_in_a_body_above_the_module_a_glob_imports_from_refused,
    refused("recursive type T refers back to itself through unions and aliases alone"),
    "/test.gx" => "type Foo<'a> = Array<'a>; \
        let r = { let f = |a: i64| { type T = [i64, Foo<T>]; let x: T = a; x }; \
        use o::*; f(1) }; mod o; let result = 0",
    "/test/o.gx" => "type Foo<'a> = ['a, string]"
; FuseExpect::None);

run!(
    lambda_parameter_above_the_module_a_glob_imports_from_refused,
    refused("Option may name what a glob `use` of this module imports from module `/test/o`, which a later statement declares: declare the module before the name's first use"),
    "/test.gx" => "let f = |x: Option<f64>| x; mod o; use o::*; let result = 0",
    "/test/o.gx" => IMPORTED_OPTION
; FuseExpect::None);

run!(
    lambda_parameter_above_the_module_of_a_glob_reads_the_import,
    is(Value::I64(1)),
    "/test.gx" => "mod o; use o::*; let f = |x: Option<f64>| x; \
        let result = select f(\"s\") { string as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => IMPORTED_OPTION
);

run!(
    lambda_above_a_module_imports_nothing_it_names,
    is(Value::I64(0)),
    "/test.gx" => "let f = |x: Option<f64>| x; mod o; use o::P; \
        let result = select f(1.0) { null as _ => 1, f64 as _ => 0 }",
    "/test/o.gx" => "type P = (i64, string)"
);

/// An alias that bounds its parameter is not looked through; its body
/// written out in its place checks.
const BOUNDED_ALIAS_OVER_A_RECURSIVE_PARAMETER: &str = r#"
{
    type NumOpt<'a: Number> = ['a, null];
    type T<'a: Number> = [`N, `C('a, T<NumOpt<'a>>)];
    let x: T<i64> = `N;
    0
}
"#;

run!(
    bounded_alias_over_a_recursive_parameter_refused,
    BOUNDED_ALIAS_OVER_A_RECURSIVE_PARAMETER,
    refused("passes its parameter 'a back to itself through NumOpt, which bounds a parameter of its own");
    FuseExpect::None
);

const BOUNDED_ALIAS_WRITTEN_OUT: &str = r#"
{
    type T<'a> = [`N, `C('a, T<['a, null]>)];
    let x: T<i64> = `C(1, `C(null, `N));
    select x { `C(i, _) => i, `N => 0 }
}
"#;

run!(bounded_alias_written_out, BOUNDED_ALIAS_WRITTEN_OUT, is(Value::I64(1)));

// An alias looked through keeps the meaning its own module gives the
// names in its body: `Priv` in `Opt` is `inner`'s, never the one of the
// module that applies `Opt`.
run!(
    alias_from_another_module_keeps_its_names,
    is(Value::I64(111)),
    "/test.gx" => r#"
        mod inner;
        type Priv = string;
        type T<'a> = [`N, `C('a, T<inner::Opt<'a>>)];
        type W<'a> = [`N, `C('a, W<['a, i64, `P]>)];
        let x: T<bool> = `C(true, `C(`P, `C(3, `N)));
        let w: W<bool> = x;
        let tested = select w { T<bool> as _ => 1 };
        let third = select x { `C(_, `C(_, `C(h, _))) => h, _ => never() };
        let n = select third { i64 as i => i * 10 + 80, _ => 0 };
        let result = tested + n
    "#,
    "/test/inner.gx" => r#"
        type Priv = [i64, `P];
        type Opt<'a> = ['a, Priv]
    "#
);

run!(
    alias_from_another_module_refuses_the_local_name,
    refused("type mismatch T<bool> does not contain `C(bool, `C(string, `N))"),
    "/test.gx" => r#"
        mod inner;
        type Priv = string;
        type T<'a> = [`N, `C('a, T<inner::Opt<'a>>)];
        let x: T<bool> = `C(true, `C("s", `N));
        let result = 0
    "#,
    "/test/inner.gx" => r#"
        type Priv = [i64, `P];
        type Opt<'a> = ['a, Priv]
    "#;
    FuseExpect::None
);

// A module's typedef that shadows an imported type of its own name
// applies itself, not the import: its alias argument is looked through.
run!(
    alias_in_a_typedef_that_shadows_an_import,
    is(Value::I64(11)),
    "/test.gx" => r#"
        type T<'a> = [`Z, `L('a, T<'a>)];
        type Opt<'a> = ['a, null];
        let warm: T<i64> = `Z;
        mod inner;
        let result = inner::r
    "#,
    "/test/inner.gx" => r#"
        use super::{Opt, T};
        type T<'a> = [`N, `C('a, T<Opt<'a>>)];
        type W<'a> = [`N, `C('a, W<['a, null]>)];
        let x: T<i64> = `C(1, `C(null, `N));
        let w: W<i64> = x;
        let tested = select w { T<i64> as _ => 1 };
        let second = select x { `C(_, `C(h, _)) => h, _ => never() };
        let r = select second { null as _ => tested + 10, i64 as _ => 0 }
    "#
);

/// A non-recursive typedef applied to ever larger arguments down a finite
/// chain of typedefs compares and keys a map as its expansion does.
const FINITE_CHAIN_OF_LARGER_ARGUMENTS: &str = r#"
{
    type O<'a> = ['a, null];
    type A = Array<O<Array<B>>>;
    type B = Array<O<Array<Array<C>>>>;
    type C = Array<O<Array<Array<Array<D>>>>>;
    type D = Array<O<Array<Array<Array<Array<E>>>>>>;
    type E = Array<O<Array<Array<Array<Array<Array<F>>>>>>>;
    type F = Array<O<Array<Array<Array<Array<Array<Array<i64>>>>>>>>;
    let v: A = [null];
    let w: A = [[[null]]];
    let m: Map<A, i64> = {v => 1, w => 2};
    select v == w { true => 0, false => map::len(m) }
}
"#;

run!(
    finite_chain_of_larger_arguments,
    FINITE_CHAIN_OF_LARGER_ARGUMENTS,
    is(Value::I64(2))
);

/// A cast reads a value through one definition applied twice over it,
/// the inner application to a larger argument.
const CAST_THROUGH_A_LARGER_APPLICATION: &str = r#"
{
    type I<'a> = 'a;
    type B = I<{x: i64}>;
    type A = I<B>;
    let r = cast<A>({x: 1});
    r$.x
}
"#;

run!(
    cast_through_a_larger_application,
    CAST_THROUGH_A_LARGER_APPLICATION,
    is(Value::I64(1))
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

run!(same_name_other_definition, SAME_NAME_OTHER_DEFINITION, refused("type mismatch Error<ErrChain<P<i64, string>>> does not contain Error<ErrChain<P<i64, string>>>"); FuseExpect::None);

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

/// A typedef names no trait as a type, whichever of the two is declared first.
const TYPEDEF_ABOVE_ITS_TRAIT: &str = r#"
{
    type T = Array<Sh>;
    trait Sh { val sh: fn(self) -> i64 };
    impl Sh for i64 { let sh = |c| c * 10 };
    let v: T = [1, 2];
    v
}
"#;

run!(
    typedef_above_its_trait,
    TYPEDEF_ABOVE_ITS_TRAIT,
    refused("trait Sh used as a type");
    FuseExpect::None
);

/// Neither does a `let`'s annotation or a function's signature.
const ANNOTATION_ABOVE_ITS_TRAIT: &str = r#"
{
    let v: Array<Sh> = [1, 2];
    trait Sh { val sh: fn(self) -> i64 };
    impl Sh for i64 { let sh = |c| c * 10 };
    v
}
"#;

run!(
    annotation_above_its_trait,
    ANNOTATION_ABOVE_ITS_TRAIT,
    refused("trait Sh used as a type above its declaration");
    FuseExpect::None
);

const PARAMETER_ABOVE_ITS_TRAIT: &str = r#"
{
    let f = |a: Sh| 0;
    trait Sh { val sh: fn(self) -> i64 };
    impl Sh for i64 { let sh = |c| c * 10 };
    0
}
"#;

run!(
    parameter_above_its_trait,
    PARAMETER_ABOVE_ITS_TRAIT,
    refused("trait Sh used as a type above its declaration");
    FuseExpect::None
);

const PARAMETER_PART_ABOVE_ITS_TRAIT: &str = r#"
{
    let f = |a: Array<Sh>| 0;
    trait Sh { val sh: fn(self) -> i64 };
    0
}
"#;

run!(
    parameter_part_above_its_trait,
    PARAMETER_PART_ABOVE_ITS_TRAIT,
    refused("trait Sh used as a type above its declaration");
    FuseExpect::None
);

const TYPEDEF_ARGUMENT_ABOVE_ITS_TRAIT: &str = r#"
{
    type T<'a> = Array<'a>;
    type U = T<Sh>;
    trait Sh { val sh: fn(self) -> i64 };
    0
}
"#;

run!(
    typedef_argument_above_its_trait,
    TYPEDEF_ARGUMENT_ABOVE_ITS_TRAIT,
    refused("trait Sh used as a type");
    FuseExpect::None
);

/// The trait is declared by a block around the typedef's, below it.
const TYPEDEF_BELOW_ITS_TRAITS_BLOCK: &str = r#"
{
    let q = {
        type T = Array<Sh>;
        let v: T = [1, 2];
        v
    };
    trait Sh { val sh: fn(self) -> i64 };
    impl Sh for i64 { let sh = |c| c * 10 };
    q
}
"#;

run!(
    typedef_below_its_traits_block,
    TYPEDEF_BELOW_ITS_TRAITS_BLOCK,
    refused("trait Sh used as a type");
    FuseExpect::None
);

const TYPEDEF_AFTER_ITS_TRAIT: &str = r#"
{
    trait Sh { val sh: fn(self) -> i64 };
    type T = Array<Sh>;
    0
}
"#;

run!(
    typedef_after_its_trait,
    TYPEDEF_AFTER_ITS_TRAIT,
    refused("trait Sh used as a type");
    FuseExpect::None
);

/// A trait as a typedef parameter's bound stands above the trait's declaration.
const TYPEDEF_BOUND_ABOVE_ITS_TRAIT: &str = r#"
{
    type T<'a: Sh> = Array<'a>;
    trait Sh { val sh: fn(self) -> i64 };
    impl Sh for i64 { let sh = |c| c * 10 };
    let v: T<i64> = [1, 2];
    v
}
"#;

run!(
    typedef_bound_above_its_trait,
    TYPEDEF_BOUND_ABOVE_ITS_TRAIT,
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

const HOLE_IN_A_FN_QUANTIFIER: &str = r#"
{
    trait T { val g: fn<'b: Array<'_>>(self, x: 'b) -> i64 };
    0
}
"#;

run!(hole_in_a_fn_quantifier, HOLE_IN_A_FN_QUANTIFIER, refused("'_ is the hole"); FuseExpect::None);

const TRAIT_UNDER_A_FN_QUANTIFIER: &str = r#"
{
    trait T { val g: fn<'b: Array<Display>>(self, x: 'b) -> i64 };
    0
}
"#;

run!(
    trait_under_a_fn_quantifier,
    TRAIT_UNDER_A_FN_QUANTIFIER,
    refused("trait Display used as a type");
    FuseExpect::None
);

/// A hole in a part of a written type is refused ahead of a function type
/// with variables of its own that holds it.
const HOLE_IN_A_QUANTIFIED_FN_PART: &str = r#"
{
    let g: Array<fn<'b: Any>(x: Array<'_>) -> 'b> = [];
    0
}
"#;

run!(
    hole_in_a_quantified_fn_part,
    HOLE_IN_A_QUANTIFIED_FN_PART,
    refused("'_ is the hole");
    FuseExpect::None
);

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
    refused("type mismatch _ does not contain 'e");
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
    refused("type mismatch i64 does not contain [i64, string]");
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
    refused_renumbered("type mismatch Box<string> does not contain Box<'_0>");
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
    refused_renumbered("type mismatch Box<'a> does not contain Box<'_0>");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch Number does not contain string");
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
    refused_renumbered("type mismatch B<_> does not contain B<'_0>");
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
    refused("type mismatch i64 does not contain Array<i64>");
    FuseExpect::None
);

// Two constructions over undecided payloads that meet at a call are one
// payload type at the call's settle, so the build holds the definition at
// the instance with no member to choose.
run!(
    undecided_payloads_met_at_a_call_build,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a| { a <- B([never()]); 1 };
    let g = |a| { let c = B([never()]); a <- c; a.0 };
    let x = B([]);
    (f(B([])), g(x))
}"#,
    prints("[i64:1, []]")
);

// Two lower bounds of a payload that differ only in cells nothing has
// decided are one type and those cells one cell: the payload is one array,
// whose element its later writer decides.
run!(
    payload_bounds_apart_by_undecided_cells_are_one,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a| { a <- B([never()]); a.0 };
    let r = f(B([]));
    r <- [1];
    select r { [x] => x, _ => never() }
}"#,
    prints("i64:1")
);

// A call's settle decides the cells its payloads' bounds hold, as it does a
// cell an array holds, so the instance's result meets a union with nothing
// left to choose between its members.
run!(
    undecided_payloads_met_in_a_select_build,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a| { let c = B([never()]); select true { true => a, false => c } };
    let g = |a, c| select true { true => a, false => c };
    let r = f(B([]));
    let s = g(B([]), B([never()]));
    (r ~ 1, s ~ 2)
}"#,
    prints("[i64:1, i64:2]")
);

// Cells a settle made one are decided together: a call whose result holds
// the payload settles it while `e` and `g` are undecided, so a writer of
// one that the other's does not fit is refused, naming the binding to
// declare.
run!(
    payload_cells_a_settle_meets_take_each_writers_type,
    r#"{
    type B<'p> = Abstract<'p>;
    let e = never();
    let g = never();
    let x = B([e]);
    x <- B([g]);
    let h = |u| u ~ x.0;
    let r = h(1);
    e <- 1;
    g <- "s";
    r
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

// A declared cell is no cell nothing bounds: the two payloads stay two
// members.
run!(
    declared_payload_cell_stays_apart,
    r#"{
    type B<'p> = Abstract<'p>;
    let e = never();
    let g: [i64, string] = never();
    let x = B([e]);
    x <- B([g]);
    let h = |u| u ~ x.0;
    let r = h(1);
    e <- 1;
    g <- "s";
    r
}"#,
    prints("[i64:1]")
);

// Two parameters whose payloads a settle in the body made one are one
// type to a call; declared apart, they stay two.
run!(
    parameters_written_into_one_payload_take_two_types,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a, b| { let x = B([a]); x <- B([b]); let h = |u| u ~ x.0; h(1) };
    f(1, "s")
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

run!(
    declared_parameters_keep_two_payloads,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a: 'a, b: 'b| { let x = B([a]); x <- B([b]); let h = |u| u ~ x.0; h(1) };
    f(1, "s")
}"#,
    prints("[i64:1]")
);

// A payload nothing has decided met by one the call decides is that one
// payload type: the empty array's bound is held by the decided array, so
// the call's settle leaves no union and the build holds the definition at
// the instance.
run!(
    undecided_payload_met_by_a_decided_one_at_a_call_builds,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a, z| a <- B([z]);
    let g = |z, a| a <- B((z, [z]));
    let x = B([]);
    f(x, 1);
    g("s", B((never(), [])));
    f(B([never()]), 2);
    x.0
}"#,
    prints("[]")
);

// Two payloads a call decides apart are a union at the instance, and the
// definition's bound over its parameter is held there by that parameter's
// own type, in either order of the parameters, with the payload returned,
// and from inside another definition.
run!(
    two_payloads_decided_apart_at_a_call_build,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |a, z| { a <- B([z]); a.0 };
    let g = |z, a| { a <- B([z]); a.0 };
    let h = |u| { let x = B([]); f(x, u); x.0 };
    (f(B([1]), "s"), g(n, B([2])), f(B([]), 3), h(4))
}"#,
    prints("[[i64:1], [i64:2], [], []]")
);

// A payload declared as a union of two arrays could hold what the
// function builds into it in either member, whatever the other argument
// is and wherever it stands: refused, naming the payload, which runs
// declared as one member.
run!(
    declared_union_payload_takes_the_array_the_function_builds,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |a, z| { a <- B([z]); a.0 };
    let x: B<[Array<i64>, Array<[i64, null]>]> = B([5]);
    f(x, n)
}"#,
    prints("[i64:5]");
    FuseExpect::Jit
);

run!(
    declared_union_payload_after_its_decider,
    r#"{
    type B<'p> = Abstract<'p>;
    let g = |z, a| { a <- B([z]); a.0 };
    let x: B<[Array<i64>, Array<string>]> = B([5]);
    g(2, x)
}"#,
    prints("[i64:5]");
    FuseExpect::Jit
);

// A payload an earlier argument decided holds what the function builds
// into it: a later argument wider than the variable the payload holds
// widens nothing, and is refused naming the argument to annotate.
run!(
    decided_payload_takes_a_wider_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |a, z| { a <- B([z]); a.0 };
    let x: B<Array<i64>> = B([1]);
    f(x, n)
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

// The payload is met before an argument written ahead of it, and the
// refusal names it as the call's other argument.
run!(
    decided_payload_written_after_takes_a_wider_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |z, a| { a <- B([z]); a.0 };
    let x: B<Array<i64>> = B([1]);
    f(n, x)
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

run!(
    decided_payload_written_after_wide_enough_takes_the_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |z, a| { a <- B([z]); a.0 };
    let x: B<Array<[i64, null]>> = B([1]);
    f(n, x)
}"#,
    prints("[i64:1]")
);

run!(
    decided_payload_wide_enough_takes_the_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |a, z| { a <- B([z]); a.0 };
    let x: B<Array<[i64, null]>> = B([1]);
    f(x, n)
}"#,
    prints("[i64:1]")
);

// The same where the payload is declared empty, and where the decided type
// is the payload read back into a parameter.
run!(
    empty_declared_payload_takes_the_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = |a, z| { a <- B([z]); a.0 };
    let x: B<Array<_>> = B([]);
    f(x, 1)
}"#,
    prints("[]");
    FuseExpect::Jit
);

run!(
    payload_read_into_a_decided_parameter_takes_a_wider_write,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |a, z| { let b = B(z); a <- b.0; a };
    let x: i64 = 1;
    f(x, n)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    payload_read_into_a_parameter_declared_wide_takes_the_argument,
    r#"{
    type B<'p> = Abstract<'p>;
    let n: [i64, null] = null;
    let f = |a, z| { let b = B(z); a <- b.0; a };
    let x: [i64, null] = 1;
    f(x, n)
}"#,
    prints("i64:1")
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

// Bounds a probe admits apart but no binding holds together decide
// nothing: `x` is still undecided where the arm tests it.
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
    refused(
        r#"pattern (1, "a") will never match: nothing reaches it (6:20; the value is (string, string))"#
    )
);

run!(
    payload_bounds_annotated_where_held,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x = never();
    let b: B<([i64, string], [i64, string])> = B((x, x));
    let v = select b.0 { (1, "a") => 0, _ => 1 };
    x <- t ~ "s";
    (b.0, v)
}"#,
    prints(r#"[["s", "s"], i64:1]"#)
);

// A type test over the element: `[i64, Array<i64>]` holds `x` and
// `Array<x>` together only as `x := i64`, which the open bound takes
// after the other. The arms decide `x` so, through the payload's bounds,
// and are refused for it as over a plain scrutinee; annotated, it runs.
const TYPE_TEST_OVER_THE_ELEMENT: &str =
    "[h, ..] => select h { i64 as n => n + 100, Array<i64> as a => array::len(a) }";

run!(
    select_type_test_over_two_payload_bounds,
    select_over_two_payload_bounds("B([[x]])", "B([x])", TYPE_TEST_OVER_THE_ELEMENT),
    prints("[[[[i64:1]], [i64:1]], [i64:1, i64:101]]");
    FuseExpect::Jit
);

run!(
    select_type_test_over_two_payload_bounds_by_a_definition,
    select_over_two_payload_bounds("mk([[x]])", "mk([x])", TYPE_TEST_OVER_THE_ELEMENT),
    prints("[[[[i64:1]], [i64:1]], [i64:1, i64:101]]");
    FuseExpect::Jit
);

run!(
    select_type_test_over_two_annotated_payload_bounds,
    select_over_two_payload_bounds("B([[x]])", "B([x])", TYPE_TEST_OVER_THE_ELEMENT)
        .replace("let x = never();", "let x: i64 = never();"),
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
    prints(r#"[["A", "a"], ["B", "b"]]"#);
    FuseExpect::Jit
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
    refused("pattern [[h, ..], ..] will never match: nothing reaches it (4:12; the value is Array<string>)");
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
    prints(r#"["A", i64:1]"#);
    FuseExpect::Jit
);

// What was built holds the referent of a reference to an expression no
// binding names: the annotation names the binding holding the reference.
run!(
    select_arms_at_two_depths_over_a_referents_payloads,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let rx = &mut never();
    let b = B([[*rx]]);
    b <- B([*rx]);
    let v = select b.0 { [[h, ..], ..] => `A(h), [h, ..] => `B(h), _ => `C };
    *rx <- t ~ 1;
    v
}"#,
    prints(r#"["A", i64:1]"#);
    FuseExpect::Jit
);

run!(
    select_arms_at_two_depths_over_a_referents_payloads_declared,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let rx = &mut never();
    let b: B<[Array<i64>, Array<Array<i64>>]> = B([[*rx]]);
    b <- B([*rx]);
    let v = select b.0 { [[h, ..], ..] => `A(h), [h, ..] => `B(h), _ => `C };
    *rx <- t ~ 1;
    let vs = [];
    vs <- array::window(#n: 2, v ~ vs, v);
    select array::len(vs) { 2 => vs, _ => never() }
}"#,
    prints(r#"[["A", i64:1], ["B", i64:1]]"#)
);

// What was built holds two calls' copies of the variable their callee
// declares, which no binding names: each is a cell of its own, numbered.
run!(
    select_arms_at_two_depths_over_two_calls_payloads,
    arms_at_two_depths(
        r#"{ let g = |u: 'a| -> Array<'a> []; B([g(never()), g(never())]) }"#,
        r#"B(["s"])"#
    ),
    refused("pattern [[h, ..], ..] will never match: nothing reaches it (6:20; the value is Array<Array<_>> | Array<string>)");
    FuseExpect::None
);

run!(
    select_arms_at_two_depths_over_two_calls_payloads_declared,
    arms_at_two_depths(
        r#"{
        let g = |u: 'a| -> Array<'a> [];
        let b: B<[Array<string>, Array<Array<string>>]> = B([g(never()), g(never())]);
        b
    }"#,
        r#"B(["s"])"#
    ),
    prints(r#"[["B", []], ["B", "s"]]"#)
);

// What was built holds a `[]`'s element, which no binding and no written
// variable names: numbered.
run!(
    select_arms_at_two_depths_over_an_empty_arrays_payloads,
    arms_at_two_depths(r#"B([[]])"#, r#"B(["s"])"#),
    refused("pattern [[h, ..], ..] will never match: nothing reaches it (6:20; the value is Array<Array<_>> | Array<string>)");
    FuseExpect::None
);

run!(
    select_arms_at_two_depths_over_an_empty_arrays_payloads_declared,
    arms_at_two_depths(
        r#"{ let b: B<[Array<string>, Array<Array<string>>]> = B([[]]); b }"#,
        r#"B(["s"])"#
    ),
    prints(r#"[["B", []], ["B", "s"]]"#)
);

// A write a binding's type cannot hold, over such a referent.
run!(
    write_wider_than_a_binding_holding_a_referent,
    r#"{
    let rx = &mut never();
    let v = {a: 1, c: *rx};
    v <- {a: "s", c: *rx};
    v
}"#,
    settles();
    FuseExpect::None
);

run!(
    write_wider_than_a_binding_holding_a_referent_declared,
    r#"{
    let t = sys::time::timer(duration:10.ms, false);
    let rx = &mut never();
    let v: {a: [i64, string], c: i64} = {a: 1, c: *rx};
    v <- {a: "s", c: *rx};
    *rx <- t ~ 1;
    let vs = [];
    vs <- array::window(#n: 2, v ~ vs, v);
    select array::len(vs) { 2 => vs, _ => never() }
}"#,
    prints(r#"[[["a", i64:1], ["c", i64:1]], [["a", "s"], ["c", i64:1]]]"#)
);

/// A select over a payload built as `[x]` ahead of a write of `[[x]]`,
/// `b` declared as `decl`: each value of `v`.
fn select_before_a_deeper_write(x: &str, decl: &str) -> String {
    format!(
        r#"{{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let x{x} = never();
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

// The select decides the payload from `[x]` alone, so the write of
// `[[x]]` after it is refused, though no finite type is wanting: the
// payload's union, declared, holds both.
run!(
    select_before_a_deeper_write,
    select_before_a_deeper_write("", ""),
    prints("[i64:1, [i64:1]]");
    FuseExpect::Jit
);

// Declared so, `[x]` could go to either array of the payload while `x` is
// undecided: `x` is declared too.
run!(
    select_before_a_deeper_write_declared_over_an_element_a_later_write_decides,
    select_before_a_deeper_write("", ": B<[Array<i64>, Array<Array<i64>>]>"),
    prints("[i64:1, [i64:1]]");
    FuseExpect::Jit
);

run!(
    select_before_a_deeper_write_declared,
    select_before_a_deeper_write(": i64", ": B<[Array<i64>, Array<Array<i64>>]>"),
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
    prints("i64:1");
    FuseExpect::Jit
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
    FuseExpect::None
);

run!(
    select_before_a_write_reading_its_payload_in_a_lambda,
    select_before_a_write_reading_its_payload("", "B([(|u| u ~ b.0)(t)])"),
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
    FuseExpect::None
);

run!(
    select_before_a_write_reading_its_payload_through_a_reference,
    select_before_a_write_reading_its_payload("let r = &b;", "B([t ~ (*r).0])"),
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
    FuseExpect::None
);

run!(
    select_before_a_write_reading_its_payload_through_a_write,
    select_before_a_write_reading_its_payload("let c = never(); c <- b.0;", "B([t ~ c])"),
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
    FuseExpect::None
);

run!(
    select_before_a_write_of_the_selected_element,
    select_before_a_write_reading_its_payload("", "B([[t ~ v]])"),
    refused("type mismatch i64 does not contain Array<i64>");
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
    refused("type mismatch i64 does not contain Array<i64>");
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
    refused("type mismatch _ does not contain Array<_>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    prints("[[i64:0]]");
    FuseExpect::Jit
);

// Two payloads of one abstract type: the declaration that holds both
// unions the parameter, through which the payload is read.
run!(
    payload_write_unions_the_parameter,
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
    prints("i64:2");
    FuseExpect::Jit
);

// A cell the checker made up has no name a program writes: the hint
// names no type.
run!(
    write_between_two_undecided_payloads_fits_the_callees_bound,
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
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    write_between_two_undecided_payloads_outside_the_callees_bound_refused,
    r#"{
    type B<'p> = Abstract<'p>;
    let t = sys::time::timer(duration:10.ms, false);
    let f = 'a: [i64, Array<'b>] |x: B<'a>, y: B<'b>| -> B<'a> x;
    let g = B(never());
    let h = B(never());
    let r = f(g, h);
    g <- h;
    h <- B(t ~ "s");
    r.0
}"#,
    refused_renumbered("type mismatch B<'b> does not contain B<'_0>");
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
    prints("[i64:1]");
    FuseExpect::Jit
);

// CR claude for claude: [bug/medium] Following this fixture's hint (declare y: [i64,
// string]) gets the program refused again, at `x <- t ~ 1`: `x is [i64, string],
// inferred from an earlier use, and cannot hold i64; declare x: [i64, string]`. The
// select binds x's cell to the payload's element set, which is two open cells, each
// above [i64, string]. The containment of that set (`SET-T ['_a, '_b] >= i64` under
// GRAPHIX_DBG_BIND, Type::contains_int's Set arm) refuses i64, although both cells'
// lower bounds already hold it. write_mismatch then prints the set by its lower bound,
// so the message contradicts itself. The program checks with `y: i64`, with `x <- 1`
// above the select, or without the abstract. Fix: a type that every open member's lower
// bound already holds should go into the set without binding anything, and the hinted
// twin (y declared, x not) should be pinned as checking. probe:
// design/review-2026-11-16/repro/tests-types-b-element-set-write.gx (tests-types-b-02)
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
    prints("i64:1");
    FuseExpect::Jit
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
    refused("type mismatch i64 does not contain string");
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
    payload_bounded_operand_is_decided_by_the_payload,
    r#"{
    type B<'p> = Abstract<'p>;
    let b = B([null, 1]);
    let n = array::len(b.0);
    select b.0 { [x, ..] => x$ + n, _ => 0 }
}"#,
    settles();
    FuseExpect::Jit
);

// `$` and `?` over a union whose members are all undecided, or none of which
// holds an error, would take the null form or the error form on what an open
// member later holds.
run!(
    strip_over_two_cells_later_writes_decide_takes_the_null_form,
    r#"{
    let x = never();
    let y = never();
    let u = select true { true => x, false => y };
    let r = u$;
    x <- null;
    y <- 1;
    "[r] end"
}"#,
    settles();
    FuseExpect::Jit
);

run!(
    strip_over_two_open_cells_annotated,
    r#"{
    let x: null = never();
    let y: i64 = never();
    let u = select true { true => x, false => y };
    let r = u$;
    x <- null;
    y <- 1;
    "ok"
}"#,
    prints("\"ok\"");
    FuseExpect::Jit
);

run!(
    strip_over_open_member_beside_null_is_refused,
    r#"{
    let f = |a: [i64, null], b| {
        let u = select false { true => a, false => b };
        u?
    };
    let r = f(1, error(`E));
    "[r] end"
}"#,
    refused("the type of the operand of ? or $ is not known here: annotate it");
    FuseExpect::None
);

// The operand of `$` must be known where it is written: a member an
// unannotated parameter gives it is one only each call decides.
run!(
    strip_over_open_member_beside_error_is_refused,
    r#"{
    let f = |a: Error<`E>, b| {
        let u = select false { true => a, false => b };
        u$
    };
    let r = f(error(`E), 3);
    "[r] end"
}"#,
    refused("the type of the operand of ? or $ is not known here: annotate it");
    FuseExpect::None
);

// `?` joins the errors its operand's decided members hold into its handler
// where it is written; an open member that may hold an error itself would
// raise one the handler never took.
run!(
    qop_over_a_declared_variable_beside_an_error_is_refused,
    r#"{
    let f = |x: ['a, Error<`A>]| x?;
    let v: [i64, Error<`A>, Error<`B>] = error(`B);
    let r = { catch(e) e ~ select e.0.error { `A => -1 }; f(v) };
    "[r] end"
}"#,
    refused("declare 'a: NoError");
    FuseExpect::None
);

run!(
    qop_over_a_variable_beside_an_error_variable_is_refused,
    r#"{
    let f = |x: ['a, Error<'e>]| x?;
    let v: [i64, Error<`A>, Error<`B>] = error(`B);
    let r = { catch(e) e ~ select e.0.error { `A => -1 }; f(v) };
    "[r] end"
}"#,
    refused("declare 'a: NoError");
    FuseExpect::None
);

run!(
    qop_over_a_variable_beside_an_error_declared_no_error,
    r#"{
    let f = 'a: NoError |x: ['a, Error<`A>]| x?;
    let ok: [i64, Error<`A>] = 7;
    "[f(ok)] end"
}"#,
    prints("\"7 end\"");
    FuseExpect::Jit
);

run!(
    qop_over_a_result_of_a_variable,
    r#"{
    let f = |x: Result<'a, `A>| x?;
    let ok: [i64, Error<`A>] = 7;
    "[f(ok)] end"
}"#,
    prints("\"7 end\"");
    FuseExpect::Jit
);

run!(
    qop_over_an_undecided_member_beside_an_error_is_refused,
    r#"{
    let f = |a: Error<`A>, b| {
        let u = select false { true => a, false => b };
        u?
    };
    let r = { catch(e) e ~ select e.0.error { `A => -1 }; f(error(`A), error(`B)) };
    "[r] end"
}"#,
    refused("the type of the operand of ? or $ is not known here: annotate it");
    FuseExpect::None
);

run!(
    qop_over_an_undecided_member_beside_an_error_annotated,
    r#"{
    let f = |a: Error<`A>, b: i64| {
        let u = select false { true => a, false => b };
        u?
    };
    "[f(error(`A), 3)] end"
}"#,
    prints("\"3 end\"");
    FuseExpect::Jit
);

/// An input's settle binds a parameter both a payload bounded and a ⊥ was
/// produced into to the payload's type, never ⊥: a later input reading
/// the payload is held to it, whether the ⊥ reached the parameter
/// directly or through a construction's.
#[tokio::test(flavor = "current_thread")]
async fn settle_keeps_a_payload_bound_beside_bottom() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
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
        assert!(format!("{e:#}").ends_with("i64 does not contain string"), "{e:#}");
    }
    let probe =
        ctx.rt.compile(ArcStr::from("{ let s: string = r.0; \"[s][q.0]\" }")).await?;
    let v = next_update_raw(&ctx.rt, &mut rx, probe.exprs[0].id).await?;
    assert_eq!(v, Value::String("ab".into()));
    drop(held);
    ctx.shutdown().await;
    Ok(())
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused("infinite type: a variable holds itself in Bx<'a>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused_renumbered("infinite type: a variable holds itself in Array<'_0>");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain null");
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
    refused("type mismatch i64 does not contain Opt<'a>");
    FuseExpect::None
);

// A bound that names a union holds its parameter as the union written out
// does: an argument that is the parameter's own decided cell makes the
// bounded variable that type, and what the bound's other member would
// have admitted is refused after it.
run!(
    bare_cycle_through_a_bounds_typedef_parameter_to_a_decided_cell,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a: O<'b>, 'b: Number |t: ('b, 'b, 'a)| t.2;
  let h = |v| f((v, 1, v));
  h(2)
}"#,
    is(Value::I64(2))
);
run!(
    bare_cycle_through_a_bounds_union_to_a_decided_cell,
    r#"{
  let f = 'a: ['b, null], 'b: Number |t: ('b, 'b, 'a)| t.2;
  let h = |v| f((v, 1, v));
  h(2)
}"#,
    is(Value::I64(2))
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_widens_the_cell,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a: O<'b>, 'b: Number |t: ('b, 'b, 'a), u: 'a| t.2;
  let h = |v| f((v, 1, v), null);
  h(2)
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

// Two undecided cells on that path are one type: the result is the
// argument, whatever the bound's other member would have admitted, with a
// decided cell between them too.
run!(
    bare_cycle_through_a_bounds_typedef_parameter_of_open_cells,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a: O<'b>, 'b: Number |t: ('b, 'a)| t.1;
  let v = never();
  let r: null = f((v, v));
  v <- 2;
  select r { null as _ => 0 }
}"#,
    refused("null does not contain");
    FuseExpect::None
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_of_open_cells_is_the_argument,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a: O<'b>, 'b: Number |t: ('b, 'a)| t.1;
  let v = never();
  let r: i64 = f((v, v));
  v <- 2;
  r
}"#,
    is(Value::I64(2))
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_past_a_decided_cell,
    r#"{
  type O<'t> = ['t, null];
  type E<'p, 'q> = ['p, 'q];
  let f = 'a: O<O<'b>>, 'b: E<O<'c>, O<'c>> |t: ('b, 'b, 'b, 'c, 'a)| t;
  let h = |v, w| f((v, 1, 1, w, w));
  let r: (i64, i64, i64, i64, null) = h(1, 2);
  select r.4 { null as _ => 0 }
}"#,
    refused("type mismatch null does not contain i64");
    FuseExpect::None
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_past_a_decided_cell_is_the_argument,
    r#"{
  type O<'t> = ['t, null];
  type E<'p, 'q> = ['p, 'q];
  let f = 'a: O<O<'b>>, 'b: E<O<'c>, O<'c>> |t: ('b, 'b, 'b, 'c, 'a)| t;
  let h = |v, w| f((v, 1, 1, w, w));
  let r: (i64, i64, i64, i64, i64) = h(1, 2);
  r.4
}"#,
    is(Value::I64(2))
);

// The dropped bound's name still holds its parameter to the parameter's
// bound, once the cells are one.
run!(
    bare_cycle_through_a_bounds_typedef_parameter_holds_the_parameters_bound,
    r#"{
  type N<'t: Number> = ['t, null];
  let f = 'a: N<'b> |x: 'a, y: 'b| y;
  let h = |v| f(v, v);
  h("s")
}"#,
    refused("type mismatch N<'b> does not contain string");
    FuseExpect::None
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_holds_the_parameters_bound_written,
    r#"{
  type N<'t: Number> = ['t, null];
  let f = 'a: N<'b> |t: ('b, 'a)| t.1;
  let v = never();
  let r = f((v, v));
  v <- "s";
  r
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_within_the_parameters_bound,
    r#"{
  type N<'t: Number> = ['t, null];
  let f = 'a: N<'b> |x: 'a, y: 'b| y;
  let h = |v| f(v, v);
  h(3)
}"#,
    is(Value::I64(3))
);
run!(
    bare_cycle_through_a_bounds_typedef_parameter_holds_the_merged_cell,
    r#"{
  type N<'t: Number> = ['t, null];
  type I<'t> = 't;
  type E<'p, 'q> = ['p, 'q];
  let f = 'a: N<I<'b>>, 'b: E<'c, 'c>, 'c: [Number, null] |x0: 'c, x1: 'a, x2: 'b| (x0, x1, x2);
  let w = never();
  let r = f(w, w, 1);
  w <- 2;
  r.1
}"#,
    is(Value::I64(2))
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain [i64, string]");
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
    refused("type mismatch i64 does not contain string");
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
    refused("infinite type: a variable holds itself in [null, Q<i64>, Q<'a>]");
    FuseExpect::None
);

// A cell above an array of another cell and above that cell is no cycle:
// a lower bound closes one only down from the cell below or up from the
// cell above. The select's type tests then decide `x` through the bounds,
// refused for that, never as a cycle.
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
    prints("[[i64:2], [i64:100]]");
    FuseExpect::Jit
);

run!(
    lower_bounds_beside_an_array_of_an_annotated_cell,
    LOWER_BOUNDS_BESIDE_AN_ARRAY_OF_THEMSELVES
        .replace("let x = never();", "let x: i64 = never();"),
    prints("[[i64:2], [i64:100]]")
);

// A function value no binding names is one instance, so arithmetic over its
// calls bounds one result cell `Number & Singleton`: a set `Singleton` refuses
// is no witness, and no refusal either, since one of its members may still
// fit. The cell stays open for what later decides it.
run!(
    a_shared_result_under_arithmetic_stays_open,
    r#"{
        let g = select true { true => |b| never(), false => |b| never() };
        let a = g(true) + g(false);
        let c = g(false);
        7
    }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(7)));
    FuseExpect::None
);

run!(
    a_shared_result_under_arithmetic_takes_a_later_number,
    r#"{
        let g = select true { true => |b| never(), false => |b| never() };
        let a = g(true) + g(false);
        let c: f64 = g(false);
        7
    }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(7)));
    FuseExpect::None
);

run!(
    a_shared_result_nothing_produces_meets_arithmetic_and_a_string,
    r#"{
        let g = select true { true => |b| never(), false => |b| never() };
        let a = g(true) + g(false);
        let c: string = g(false);
        7
    }"#,
    prints("i64:7");
    FuseExpect::None
);

run!(
    a_shared_result_one_function_produces_refuses_a_later_string,
    r#"{
        let g = select true { true => |b| 1, false => |b| never() };
        let a = g(true) + g(false);
        let c: string = g(false);
        7
    }"#,
    refused("string does not contain i64");
    FuseExpect::None
);

// A `NoError` cell below a union of errors alone takes ⊥, what is left of it:
// `Result<⊥, E> ⊇ Result<'a, E>`, as an instance's signature asks of a function
// value its definition returns and nothing calls.
run!(
    a_returned_generic_function_over_a_result_nothing_calls,
    r#"{
        let i = |x: 'a| -> Result<'a, string> never();
        let f = |y| i;
        f(5);
        7
    }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::I64(7)));
    FuseExpect::None
);

run!(
    a_result_holding_only_an_error_is_an_error,
    r#"{
        let g = || -> Result<'a, `E> error(`E);
        let f = |e: Error<`E>| e;
        f(g())
    }"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Error(_)));
    FuseExpect::Jit
);

// A read's target is `Concrete` as well, which refuses ⊥.
run!(
    a_read_target_is_not_decided_by_an_error_annotation,
    r#"{
        let e: Error<`ParseError(string)> = str::parse("1");
        e
    }"#,
    refused("Error<`ParseError(string)> must be NoError here");
    FuseExpect::None
);

// A function something writes is one instance, so a part of its signature left
// unwritten is a cell outside it, and a declared variable meeting that cell is
// told the annotation that keeps it the definition's.
run!(
    a_written_definition_s_unwritten_return_needs_no_annotation,
    "{ let t = 'a: Number |x: 'a| x; let r = &mut t; t(1) }",
    prints("i64:1");
    FuseExpect::None
);

run!(
    a_written_definition_s_unwritten_return_holding_it_needs_no_annotation,
    "{ let t = 'a: Number |x: 'a| [x]; let r = &mut t; t(1) }",
    prints("[i64:1]");
    FuseExpect::None
);

run!(
    a_written_definition_s_unwritten_parameter_needs_no_annotation,
    "{ let t = 'a: Number |x: 'a, y| -> bool x == y; let r = &mut t; t(1, 2) }",
    prints("false");
    FuseExpect::None
);

run!(
    a_written_definition_s_unwritten_parameter_compared_with_another_type_refused,
    "{ let t = 'a: Number |x: 'a, y| -> bool x == y; let r = &mut t; t(1, \"s\") }",
    refused("cannot compare i64 with string: neither type holds the other");
    FuseExpect::None
);

run!(
    a_written_definition_with_its_signature_written,
    "{ let t = 'a: Number |x: 'a, y: 'a| -> 'a x + y; let r = &mut t; t(1, 2) }",
    |v: Result<&Value>| matches!(v, Ok(Value::I64(3)));
    FuseExpect::None
);

// A write to a binding outside is the escape itself, whatever else is unwritten.
run!(
    a_named_definition_writing_outside_hands_its_value_out,
    "{ let y = never(); let t = 'a: Concrete |x: 'a| { y <- x; x }; t(1) }",
    prints("i64:1");
    FuseExpect::None
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
    refused_renumbered("type mismatch Number does not contain Array<'_0: unbound>");
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
    let r = g(y, b.0);
    y <- t ~ 1;
    r
}
"#;

run!(
    merge_under_a_constructor_in_a_call,
    MERGE_UNDER_A_CONSTRUCTOR_IN_A_CALL,
    refused_renumbered("type mismatch Number does not contain Array<'_0: unbound>");
    FuseExpect::None
);

// The same arguments the other way round: neither is decided, so each
// waits for the call's end and meets the parameter in order; the array
// meets the numeric bound first.
// CR claude for claude: [doc-drift/low] The comment above says neither argument is
// decided, "so each waits for the call's end and meets the parameter in order". That is
// the order-freedom mechanism removed by D36: a call meets its arguments in parameter
// order and nothing waits (CLAUDE.md, callsite.rs::Widening). The array meets the
// numeric bound first because it is the first argument. Rewrite the comment to say so.
// Found by reading; not run. (tests-types-b-05)
const MERGE_UNDER_A_CONSTRUCTOR_IN_A_CALL_ARRAY_FIRST: &str = r#"
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
    merge_under_a_constructor_in_a_call_array_first,
    MERGE_UNDER_A_CONSTRUCTOR_IN_A_CALL_ARRAY_FIRST,
    refused_renumbered("type mismatch Number does not contain Array<'_0: unbound>");
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
    refused("type mismatch i64 does not contain Array<_>");
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
    refused_renumbered("infinite type: a variable holds itself in fn(x: '_0) -> '_1");
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
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

// A written binding's type is one instance however it is written: a
// variable an annotation or `never<T>` names, or a ⊥ initializer's
// pattern, is the binding's, so a definition writing it is not
// generalized over it.
run!(
    written_annotation_variable_is_monomorphic,
    r#"{ let r: ['a, null] = null; let f = |x| { r <- x; x }; (f(1), f("a")) }"#,
    prints(r#"[i64:1, "a"]"#);
    FuseExpect::None
);

run!(
    written_annotation_without_a_variable_refuses_a_second_type,
    r#"{ let r: [i64, null] = null; let f = |x| { r <- x; x }; (f(1), f("a")) }"#,
    refused("type mismatch [i64, null] does not contain string");
    FuseExpect::None
);

run!(
    written_never_annotation_is_monomorphic,
    r#"{ let r = never<['a, null]>(); let f = |x| { r <- x; x }; (f(1), f("a")) }"#,
    prints(r#"[i64:1, "a"]"#);
    FuseExpect::None
);

run!(
    written_never_annotation_variable_read_as_one_number_refused,
    r#"{ let r = never<['a, null]>(); let f = |x| { r <- x; x }; (f(1), f("a"), r$ + 1) }"#,
    refused("type mismatch i64 does not contain [i64, string]");
    FuseExpect::None
);

run!(
    written_bottom_pattern_is_monomorphic,
    r#"{ let (a, b) = never(); let f = |x| { a <- x; x }; let p = (f(1), f("s")); (a, b) }"#,
    settles();
    FuseExpect::None
);

run!(
    written_body_annotation_variable_is_monomorphic,
    r#"{ let f = |x| { let r: 'a = never(); let g = |y| { r <- y; y }; (g(1), g("a"), r) };
         f(1) }"#,
    prints(r#"[i64:1, "a", i64:1]"#);
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

// (was holder_value_reads_a_binding_once): a written s is one instance, so what
run!(
    holder_value_of_a_written_binding_shares_its_instance_refused,
    r#"{
  let k = |p: {f: fn(x: i64) -> i64}| 0;
  let s = {f: |x| x};
  s <- never();
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
    (b.f(2), s.f(z), a.f("a"))
  };
  g(1)
}"#,
    prints("[true, true, true]");
    FuseExpect::None
);

// A written name holds one instance, which a call through it shares: its
// function passed the name itself would hold a type containing itself.
run!(
    call_through_a_written_name_passed_itself_is_refused,
    r#"{
  let g = |z| {
    let s = {f: |x| x == x};
    let (a, b) = (s, s);
    b <- never();
    (b.f(b), s.f(z), a.f("a"))
  };
  g(1)
}"#,
    refused("infinite type: a variable holds itself");
    FuseExpect::None
);

/// Where the node `t` lives, a var by its cell.
fn type_node(t: &graphix_compiler::typ::Type) -> Option<usize> {
    use graphix_compiler::typ::Type;
    match t {
        Type::Tuple(a) | Type::Set(a) | Type::Variant(_, a, _) => Some(a.as_ptr().addr()),
        Type::Struct(a) => Some(a.as_ptr().addr()),
        Type::Array(a) | Type::List(a) => Some(triomphe::Arc::as_ptr(a).addr()),
        Type::Map(m) => Some(triomphe::Arc::as_ptr(m).addr()),
        Type::Fn(f) => Some(triomphe::Arc::as_ptr(f).addr()),
        Type::TVar(tv) => Some(tv.cell_addr()),
        _ => None,
    }
}

/// The nodes `t` holds, a node many paths share counted once: an open
/// cell holds its bounds.
fn distinct_type_nodes(t: &graphix_compiler::typ::Type) -> usize {
    use graphix_compiler::typ::Type;
    use std::collections::HashSet;
    fn go(t: &Type, seen: &mut HashSet<usize>) {
        if type_node(t).is_some_and(|a| !seen.insert(a)) {
            return;
        }
        match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => go(&b, seen),
                None => {
                    for b in tv.cell_lower().iter().chain(tv.cell_above().iter()) {
                        go(b, seen)
                    }
                }
            },
            t => t.for_each_child(&mut |c| go(c, seen)),
        }
    }
    let mut seen = HashSet::new();
    go(t, &mut seen);
    seen.len()
}

/// A chain of holders, each reading the last twice (in arrays, lists, a
/// map's values, a call's argument, a block, a select's arm or its bind, a
/// lambda's body or a struct update), holds one copy of its last link's
/// type: the chain's type, its cells' bounds included, grows by a few nodes
/// a link, where a copy at each read would double it.
#[tokio::test(flavor = "current_thread")]
async fn holder_chain_stays_a_dag() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_compiler::{Scope, expr::ModPath};
    use graphix_package_core::testing::init;
    const LINKS: usize = 10;
    const PAIRS: &str = "let id = |v| v;\nlet y = (|z| z, 2);\nlet x = (|x| x, 1);\n";
    const RECORD: &str = "let x = {n: 0, f: |x| x};\n";
    for (shape, prelude, link) in [
        ("arrays", PAIRS, "([x], [x, y])"),
        ("lists", PAIRS, "{a: [<x>], b: [<x, y>]}"),
        ("maps", PAIRS, "`M({\"k\" => x}, {\"j\" => x})"),
        ("calls", PAIRS, "{a: x, b: id(x)}"),
        ("call pairs", PAIRS, "(id(x), id(x), |w| w)"),
        ("blocks", PAIRS, "{a: x, b: {let t = x; t}}"),
        ("selects", PAIRS, "{a: x, b: select 1 { _ => x }}"),
        ("select binds", PAIRS, "{a: x, b: select x { v => v }}"),
        ("lambdas", PAIRS, "{a: x, g: || x}"),
        ("updates", RECORD, "{n: 1, a: x, b: {x with n: 2}}"),
        ("update pairs", RECORD, "{n: 1, a: {x with n: 1}, b: {x with n: 2}}"),
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
        ctx.shutdown().await;
    }
    Ok(())
}

// A written function type may hold a union of its type variables: each
// call decides them, and a value takes a member its type fits.

run!(
    variable_union_in_a_parameter,
    r#"{
  let f = |x: ['a, 'b], y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([y], [z]);
  let u = never();
  let r = f(u, 1, "s");
  u <- "t";
  r
}"#,
    prints(r#"[[i64:1], ["s"]]"#)
);
run!(
    variable_union_holds_no_type_its_members_do_not,
    r#"{
  let f = |x: ['a, 'b], y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([y], [z]);
  f(true, 1, "s")
}"#,
    refused("[i64, string] does not contain bool");
    FuseExpect::None
);
run!(
    variable_union_written_as_types,
    r#"{
  let f = |x: [i64, string], y: i64, z: string| -> (Array<i64>, Array<string>) ([y], [z]);
  let u = never();
  let r = f(u, 1, "s");
  u <- "t";
  r
}"#,
    prints(r#"[[i64:1], ["s"]]"#);
    FuseExpect::Jit
);
run!(
    one_variable_beside_written_types,
    r#"{ let f = |x: i64, k: fn(z: ['a, string, null]) -> i64| -> i64 x; f(1, |z| 0) }"#,
    prints("i64:1");
    FuseExpect::Jit
);
run!(
    variable_union_in_the_result,
    r#"{ let f = |c: bool, x: 'a, y: 'b| -> ['a, 'b] select c { true => x, false => y }; f(true, 1, "s") }"#,
    prints("i64:1")
);
// A union of variables behind a typedef meets a value at its use, where the
// arguments before it decide its members.
run!(
    typedef_union_of_variables_at_its_use,
    r#"{ type E<'p, 'q> = ['p, 'q]; let f = |x: Array<E<'a, 'b>>| -> i64 0; f([1]) }"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    typedef_union_of_variables_decided_by_an_earlier_parameter,
    r#"{ type E<'p, 'q> = ['p, 'q]; let f = |y: 'a, x: Array<E<'a, 'b>>| -> 'a y; f(2, [1]) }"#,
    prints("i64:2")
);

run!(
    omitted_default_over_a_typedef_union_of_equal_cells,
    r#"{ type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k([y]); f(0, 7) }"#,
    prints("i64:7")
);
run!(
    omitted_default_over_a_typedef_union_of_distinct_cells,
    r#"{ type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k([y]); (f(0, "s"), f("s", 0)) }"#,
    prints("[i64:7, i64:7]")
);
run!(
    omitted_default_over_a_typedef_union_through_a_generic_wrapper,
    r#"{ type P<'p, 'q> = [Array<'p>, Array<'q>]; let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k([y]); let g = |t: 'c, u: 'd| f(t, u); g(1, 2) }"#,
    prints("i64:7")
);
run!(
    omitted_default_over_a_typedef_union_of_cells_alone,
    r#"{ type P<'p, 'q> = ['p, 'q]; let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k(y); f(0, 7) }"#,
    prints("i64:7")
);
run!(
    omitted_default_over_a_typedef_union_beside_a_written_member,
    r#"{ type P<'p, 'q> = [Array<'p>, Array<'q>, string]; let f = |#k: fn(q: P<'b, 'a>) -> i64 = |q| 7, y: 'a, z: 'b| k([y]); f(0, 7) }"#,
    prints("i64:7")
);

run!(
    typedef_union_at_a_written_type,
    r#"{ type E<'p, 'q> = ['p, 'q]; let f = |x: Array<E<i64, 'a>>, y: 'a| -> 'a y; f([1], 2) }"#,
    prints("i64:2");
    FuseExpect::Jit
);
run!(
    annotated_function_at_written_types,
    r#"{
  let f: fn(x: [i64, string]) -> (Array<i64>, Array<string>) = |x| ([], []);
  let g = |p, q| { let u = never(); let r = f(u); u <- p; u <- q; r };
  g(1, "s")
}"#,
    prints("[[], []]")
);
run!(
    data_union_of_variables_in_an_annotation,
    r#"{ let v: ['r, 'q] = select true { true => 1, false => "s" }; v }"#,
    prints("i64:1");
    FuseExpect::Jit
);
run!(
    data_union_of_one_variable_in_an_annotation,
    r#"{ let v: ['r, string] = select true { true => 1, false => "s" }; v }"#,
    prints("i64:1")
);
// A variable of the definition around stands beside one of its own: the
// enclosing definition's check holds it rigid, its instances closed.
run!(
    enclosing_variable_beside_an_own_one,
    r#"{ let f = 'a: Any |x: 'a| { let g = |y: ['a, 'b]| -> ['a, 'b] y; g(x) }; f(1) }"#,
    prints("i64:1")
);
run!(
    later_typedef_at_a_written_type,
    r#"{
  let f = |x: E<i64, 'b>| -> Array<'b> [];
  type E<'p, 'q> = ['p, 'q];
  let g = |p| { let u = never(); let r = f(u); u <- p; r };
  g(1)
}"#,
    prints("[]")
);
run!(
    identity_member_at_its_use,
    r#"{ type Id<'x> = 'x; let f = |x: ['a, Id<'b>]| -> i64 0; f(1) }"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    option_member_at_its_use,
    r#"{ type M<'x> = ['x, null]; let f = |x: ['a, M<'b>]| -> i64 0; f(1) }"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    option_member_takes_its_null,
    r#"{ type M<'x> = ['x, null]; let f = |x: ['a, M<'b>]| -> i64 0; f(null) }"#,
    prints("i64:0")
);
run!(
    identity_member_at_a_written_type,
    r#"{ type Id<'x> = 'x; let f = |x: ['a, Id<i64>]| -> i64 0; f(1) }"#,
    prints("i64:0")
);

// Two members of one constructor holding two distinct variables are
// decided as a union of variables is.

run!(
    nested_variable_union_written_last,
    r#"{ let f = |y: 'd, z: 'c, x: [Array<'c>, Array<'d>]| -> i64 0; f(1, "s", [1]) }"#,
    prints("i64:0")
);
run!(
    nested_union_of_one_variable_beside_a_written_type,
    r#"{ let f = |x: [Array<'c>, Array<string>], y: 'c| -> 'c y; f([1], 2) }"#,
    prints("i64:2");
    FuseExpect::Jit
);
run!(
    nested_union_of_two_constructors,
    r#"{ let f = |x: [Array<'c>, Map<string, 'd>], y: 'c| -> 'c y; f([1], 2) }"#,
    prints("i64:2");
    FuseExpect::Jit
);
run!(
    nested_union_of_two_tags,
    r#"{ let f = |x: [`A('c), `B('d)], y: 'c| -> 'c y; f(`A(1), 2) }"#,
    prints("i64:2");
    FuseExpect::Jit
);
run!(
    nested_union_of_two_arities,
    r#"{ let f = |x: [('c, i64), ('d, i64, i64)], y: 'c| -> 'c y; f((1, 2), 3) }"#,
    prints("i64:3");
    FuseExpect::Jit
);
run!(
    nested_union_typedef_at_its_use,
    r#"{ type M<'x> = Array<'x>; let f = |x: [M<'c>, Array<'d>]| -> i64 0; f([1]) }"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    nested_union_typedef_decided_by_an_earlier_parameter,
    r#"{ type M<'x> = Array<'x>; let f = |y: 'c, x: [M<'c>, Array<'d>]| -> 'c y; f(2, [1]) }"#,
    prints("i64:2")
);

run!(
    nested_data_union_of_variables_in_an_annotation,
    r#"{ let v: [Array<'r>, Array<'q>] = [1]; v }"#,
    prints("[i64:1]");
    FuseExpect::Jit
);
run!(
    nested_data_union_of_one_variable_in_an_annotation,
    r#"{ let v: [Array<'r>, Array<string>] = [1]; v }"#,
    prints("[i64:1]")
);

// Two function types the source writes in one union are two members
// unless they name the same variables: a variable is its name there, and a
// typedef's body keeps what it wrote. A member written twice is one member.
run!(
    nested_variable_union_of_function_types,
    r#"{ let f = |k: [fn(x: 'c) -> i64, fn(x: 'd) -> i64], y: 'c, z: 'd| -> i64 0; f(|x: i64| 0, 1, "s") }"#,
    prints("i64:0");
    FuseExpect::None
);
run!(
    function_type_written_twice_is_one_member,
    r#"{ let f = |k: [fn(x: 'c) -> 'd, null, fn(x: 'c) -> 'd], y: 'c| -> 'c y; f(|x: i64| "s", 1) }"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    member_of_two_variables_written_twice_is_one_member,
    r#"{ let f = |x: [('c, 'd), ('c, 'd)], y: 'c| -> 'c y; f((1, 2), 3) }"#,
    prints("i64:3")
);
run!(
    union_of_function_types_at_written_types,
    r#"{ let f = |k: [fn(x: i64) -> i64, fn(x: string) -> i64]| -> i64 0; f(|x: string| 0) }"#,
    prints("i64:0");
    FuseExpect::None
);
run!(
    typedef_union_of_function_types_keeps_both_members,
    r#"{
  type T<'c, 'd> = [fn(x: 'c) -> i64, fn(x: 'd) -> i64];
  let a: T<i64, string> = |x: i64| 1;
  let b: T<i64, string> = |x: string| 2;
  0
}"#,
    prints("i64:0")
);
run!(
    union_of_function_types_over_enclosing_variables_keeps_both_members,
    r#"{
  let f = |y: 'c, z: 'd| {
    let g = |k: [fn(x: 'c) -> i64, fn(x: 'd) -> i64]| -> i64 0;
    g(|x: 'c| 0) + g(|x: 'd| 0)
  };
  f(1, "s")
}"#,
    prints("i64:0");
    FuseExpect::None
);
// Behind a typedef the members reach the choice, which no order makes:
// what a typedef's arguments or its body keep apart is not one member
// because a renaming would make it one. The call's other arguments make
// it where they decide the members.
run!(
    typedef_union_of_function_types_at_its_use,
    r#"{
  type T<'c, 'd> = [fn(x: 'c) -> i64, fn(x: 'd) -> i64];
  let f = |k: T<'a, 'b>| -> i64 0;
  f(|x: i64| 0)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    typedef_union_of_function_types_decided_by_the_parameters_before,
    r#"{
  type T<'c, 'd> = [fn(x: 'c) -> i64, fn(x: 'd) -> i64];
  let l = |y: 'a, z: 'b, k: T<'a, 'b>| -> i64 1;
  (l(1, "s", |x: i64| 0), l(1, "s", |x: string| 0), l(1, "s", |x: [i64, string]| 0))
}"#,
    prints("[i64:1, i64:1, i64:1]");
    FuseExpect::None
);
// An untyped lambda after the deciders would take its parameter from the
// member that stands first in the union: it is refused, and typed it runs.
run!(
    typedef_union_of_function_types_untyped_lambda_after_the_deciders_takes_both,
    r#"{
  type T<'c, 'd> = [fn(x: 'c) -> i64, fn(x: 'd) -> i64];
  let l = |y: 'a, z: 'b, k: T<'a, 'b>| -> i64 1;
  l(1, "s", |x| 0)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    typedef_union_of_function_types_no_decided_member_takes_refused,
    r#"{
  type T<'c, 'd> = [fn(x: 'c) -> i64, fn(x: 'd) -> i64];
  let l = |y: 'a, z: 'b, k: T<'a, 'b>| -> i64 0;
  l(1, "s", |x: f64| 0)
}"#,
    refused("type mismatch [fn(x: i64) -> i64, fn(x: string) -> i64] does not contain fn(x: f64) -> i64");
    FuseExpect::None
);

run!(
    function_types_as_typedef_arguments_at_their_use,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |k: E<fn(x: 'c) -> i64, fn(x: 'd) -> i64>| -> i64 0;
  f(|x: string| 0)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    function_types_as_typedef_arguments_decided_by_the_parameters_before,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |y: 'c, z: 'd, k: E<fn(x: 'c) -> i64, fn(x: 'd) -> i64>| -> i64 0;
  f(1, "s", |x: string| 0)
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    function_types_as_typedef_arguments_at_a_written_type,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |k: E<fn(x: i64) -> i64, fn(x: 'd) -> i64>, z: 'd| -> i64 0;
  f(|x: string| 0, "s")
}"#,
    prints("i64:0");
    FuseExpect::None
);
// A name applied to a function type is told apart the same way: two
// applications are one member only where their arguments name the same
// variables, as written and behind a typedef.
run!(
    named_function_type_written_twice_is_one_member,
    r#"{
  type T<'a> = {v: 'a};
  let f = |k: [T<fn(x: 'c) -> i64>, T<fn(x: 'c) -> i64>], y: 'c| -> i64 k.v(y);
  f({v: |x: i64| x}, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    named_function_types_as_typedef_arguments_at_their_use,
    r#"{
  type T<'a> = {v: 'a};
  type E<'p, 'q> = ['p, 'q];
  let f = |k: E<T<fn(x: 'c) -> i64>, T<fn(x: 'd) -> i64>>| -> i64 0;
  f({v: |x: i64| 0})
}"#,
    prints("i64:0");
    FuseExpect::None
);
run!(
    named_function_types_as_typedef_arguments_decided_by_the_parameters_before,
    r#"{
  type T<'a> = {v: 'a};
  type E<'p, 'q> = ['p, 'q];
  let f = |y: 'c, z: 'd, k: E<T<fn(x: 'c) -> i64>, T<fn(x: 'd) -> i64>>| -> i64 0;
  f(1, "s", {v: |x: i64| 0})
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    data_union_of_function_types_of_two_variables_in_an_annotation,
    r#"{ let k: [fn(x: 'c) -> i64, fn(x: 'd) -> i64] = |x: i64| 0; 0 }"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    data_union_of_function_types_of_one_variable_in_an_annotation,
    r#"{ let k: [fn(x: i64) -> i64, fn(x: 'd) -> string] = |x: i64| 0; 0 }"#,
    prints("i64:0")
);

// A type holding a cell to decide that two decided members of a union
// could take goes to neither: the member that stands first in the union,
// which is stored sorted, would decide the cell. The refusal names what to
// annotate, the value where the type is its own and the parameters to
// declare first where the union is a callback's own parameter; with the
// cell decided, or the members deciding it alike, the program runs.
// CR claude for claude: [test-gap/low] TWO_DECIDED_TAKERS is UndecidedTaker's fact,
// which every site prints. So the three fixtures that pin it alone
// (undecided_element_into_a_typedef_union_after_its_deciders_refused,
// undecided_element_written_into_a_union_of_two_decided_arrays_refused,
// typedef_union_of_function_types_untyped_lambda_after_the_deciders_refused) leave
// unpinned the hint the comment above promises. A change to TakerUnion's annotate text,
// or to the call site's naming of the binding, passes them. Today they print `annotate
// `v``, `annotate the value (bind it to a let with its type written)` and `this
// argument is undecided at this call; annotate it`. The write `x <- [v]` names no
// binding where the call site names `v`. Pin each hint, and make the write name `v` too
// if that is the intent. probe:
// design/review-2026-11-16/repro/tests-types-b-undecided-taker-hints.sh
// (tests-types-b-04)
// 2026-11-21 claude: The const TWO_DECIDED_TAKERS this sat on is gone: its last two
// users pinned refusals the check no longer makes. An empty literal's element and a
// callback parameter nothing flows into are bottom, which every member takes, so
// empty_array_into_a_typedef_union_of_two_decided_arrays and
// callback_parameter_union_of_two_decided_arrays_alone (renamed, no _refused) run. The
// hints this CR asks to pin are still printed where a value flows in.
run!(
    undecided_element_into_a_union_of_two_decided_arrays_is_decided_by_a_later_write,
    r#"{
  let f = |x: [Array<string>, Array<i64>]| x;
  let v = never();
  let r = f([v]);
  v <- 2;
  r
}"#,
    prints("[i64:2]");
    FuseExpect::Jit
);
run!(
    typed_element_into_a_union_of_two_decided_arrays,
    r#"{
  let f = |x: [Array<string>, Array<i64>]| x;
  let v: string = never();
  let r = f([v]);
  v <- "t";
  r
}"#,
    prints(r#"["t"]"#)
);
run!(
    undecided_element_into_a_typedef_union_after_its_deciders_takes_its_member,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  let v = never();
  let r = f(1, "s", [v]);
  v <- "t";
  r
}"#,
    prints(r#"["t"]"#);
    FuseExpect::Jit
);
run!(
    wrapper_undecided_element_into_a_union_of_two_decided_arrays_is_decided_by_the_call,
    r#"{
  let f = |x: [Array<i64>, Array<string>]| x;
  let g = |y| f([y]);
  g("s")
}"#,
    prints(r#"["s"]"#);
    FuseExpect::Jit
);
// Nothing flows into an empty literal's element, so it is bottom, which
// every member of the union takes: there is no choice to make.
run!(
    empty_array_into_a_declared_union_of_two_arrays,
    r#"{
  let x: [Array<i64>, Array<string>] = [];
  x
}"#,
    prints("[]");
    FuseExpect::Jit
);
run!(
    typed_empty_array_into_a_declared_union_of_two_arrays,
    r#"{
  let e: Array<i64> = [];
  let x: [Array<i64>, Array<string>] = e;
  x
}"#,
    prints("[]")
);
run!(
    undecided_element_written_into_a_union_of_two_decided_arrays_is_decided_by_a_later_write,
    r#"{
  let x: [Array<i64>, Array<string>] = [1];
  let v = never();
  x <- [v];
  v <- "s";
  x
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);
run!(
    callback_parameter_union_of_two_decided_arrays_before_its_deciders,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |k: fn(q: P<'a, 'b>) -> i64, y: 'a, z: 'b| k([y]);
  f(|q: [Array<i64>, Array<bool>]| 0, 1, true)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    callback_parameter_union_of_two_decided_arrays_after_its_deciders,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, z: 'b, k: fn(q: P<'a, 'b>) -> i64| k([y]);
  f(1, true, |q: [Array<i64>, Array<bool>]| select q { [] => 0, [_, ..] => 1 })
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    callback_parameter_union_of_two_decided_arrays_alone,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |k: fn(q: P<'a, 'b>) -> i64| -> ('a, 'b) never();
  f(|q: [Array<i64>, Array<bool>]| 0)
}"#,
    settles();
    FuseExpect::None
);
// The pre-unification of a callback passes over the choice, so a later
// position of the callback decides the variable before its own check
// meets the union; one wider than either member leaves it held by
// neither, and a function the call does not pre-unify is met in order.
run!(
    callback_parameter_union_of_two_decided_arrays_decided_by_a_later_position,
    r#"{
  let h = |k: fn(q: Array<'a>, r: 'a) -> string, z: 'a| k([z, z], z);
  let g = |w| h(|q: [Array<i64>, Array<string>], r: string| "[r]-[select q { [] => 0, [_, ..] => 1 }]", w);
  (h(|q: [Array<i64>, Array<string>], r: string| "[r]:[select q { [] => 0, [_, ..] => 1 }]", "s"), g("t"))
}"#,
    prints(r#"["s:1", "t-1"]"#)
);
run!(
    callback_parameter_union_of_two_decided_arrays_under_a_wider_later_position_refused,
    r#"{
  let h = |k: fn(q: Array<'a>, r: 'a) -> i64| 0;
  h(|q: [Array<i64>, Array<string>], r: [i64, string]| 1)
}"#,
    refused("type mismatch [Array<i64>, Array<string>] does not contain Array<[i64, string]>");
    FuseExpect::None
);
run!(
    callback_field_read_union_parameter_before_its_decider,
    r#"{
  let h = |k: fn(q: Array<'a>, r: 'a) -> i64| 0;
  let s = {c: |q: [Array<i64>, Array<string>], r: string| 1};
  h(s.c)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
// Members that decide the type alike are no choice: two names holding
// one array, and a lambda with its types written, whose one open cell is
// what it throws.
run!(
    undecided_type_into_two_decided_members_that_decide_it_alike,
    r#"{
  type A = Array<bool>;
  type P = [A, `X];
  type Q = [A, `Y];
  let f = |r: [P, Q]| select r { [] => 0, [_, ..] => 1, `X => 2, `Y => 3 };
  let g = |k: [fn(x: i64) -> i64, fn(x: string) -> i64]| 4;
  let v = never();
  let r = f([v]);
  v <- true;
  (f([]), r, g(|x: [i64, string]| 0))
}"#,
    prints("[i64:0, i64:1, i64:4]")
);
run!(
    untyped_lambda_into_a_union_of_two_function_types_takes_both,
    r#"{
  let g = |k: [fn(x: i64) -> i64, fn(x: string) -> i64]| 4;
  g(|x| 0)
}"#,
    prints("i64:4");
    FuseExpect::None
);

run!(
    write_into_a_union_of_two_undecided_holds_both,
    r#"{
  let mk = |c, a, b| (select c { true => a, false => b }, [a], [b]);
  let x = never();
  let y = never();
  let t = mk(false, x, y);
  let u = never();
  u <- t.0;
  u <- 1;
  u <- "s";
  x <- 1;
  y <- "s";
  t.0
}"#,
    prints(r#""s""#);
    FuseExpect::Jit
);

run!(
    write_of_two_types_into_a_declared_binding_refused,
    r#"{
  let mk = |c, a, b| (select c { true => a, false => b }, [a], [b]);
  let x = never();
  let y = never();
  let t = mk(false, x, y);
  let u: i64 = never();
  u <- t.0;
  u <- 1;
  u <- "s";
  x <- 1;
  y <- "s";
  t.0
}"#,
    refused("u is i64 and cannot hold string; declare u: [i64, string] where it is bound");
    FuseExpect::None
);
run!(
    write_into_a_union_of_two_undecided_holds_both_in_the_other_order,
    r#"{
  let mk = |c, a, b| (select c { true => a, false => b }, [a], [b]);
  let x = never();
  let y = never();
  let t = mk(false, x, y);
  let u = never();
  u <- t.0;
  u <- "s";
  u <- 1;
  x <- 1;
  y <- "s";
  t.0
}"#,
    prints(r#""s""#);
    FuseExpect::Jit
);

run!(
    write_of_two_types_into_a_declared_binding_the_other_order_refused,
    r#"{
  let mk = |c, a, b| (select c { true => a, false => b }, [a], [b]);
  let x = never();
  let y = never();
  let t = mk(false, x, y);
  let u: i64 = never();
  u <- t.0;
  u <- "s";
  u <- 1;
  x <- 1;
  y <- "s";
  t.0
}"#,
    refused("u is i64 and cannot hold string; declare u: [i64, string] where it is bound");
    FuseExpect::None
);
run!(
    write_into_a_declared_union,
    r#"{
  let mk = |c, a, b| (select c { true => a, false => b }, [a], [b]);
  let x = never();
  let y = never();
  let t = mk(false, x, y);
  let u: [i64, string] = never();
  u <- t.0;
  u <- 1;
  u <- "s";
  x <- 1;
  y <- "s";
  t.0
}"#,
    prints(r#""s""#)
);
run!(
    write_into_a_union_of_two_decided,
    r#"{
  let mk = |c, a, b| (select c { true => a, false => b }, [a], [b]);
  let x: i64 = never();
  let y: string = never();
  let t = mk(false, x, y);
  let u = never();
  u <- t.0;
  u <- 1;
  u <- "s";
  x <- 1;
  y <- "s";
  t.0
}"#,
    prints(r#""s""#)
);
run!(
    argument_into_a_later_typedef_union,
    r#"{
  let f = |x: E<'a, 'b>| -> (Array<'a>, Array<'b>) ([], []);
  type E<'p, 'q> = ['p, 'q];
  let g = |p, q| { let u = never(); let r = f(u); u <- p; u <- q; r };
  g(1, "s")
}"#,
    prints("[[], []]");
    FuseExpect::Jit
);
run!(
    argument_into_an_identity_argument_union,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  type Id<'x> = 'x;
  let f = |x: E<'a, Id<'b>>| -> (Array<'a>, Array<'b>) ([], []);
  let g = |p, q| { let u = never(); let r = f(u); u <- p; u <- q; r };
  g(1, "s")
}"#,
    prints("[[], []]");
    FuseExpect::Jit
);
run!(
    argument_into_an_identity_body_union,
    r#"{
  type Id<'x> = 'x;
  type E<'p, 'q> = ['p, Id<'q>];
  let f = |x: E<'a, 'b>| -> (Array<'a>, Array<'b>) ([], []);
  let g = |p, q| { let u = never(); let r = f(u); u <- p; u <- q; r };
  g(1, "s")
}"#,
    prints("[[], []]");
    FuseExpect::Jit
);
run!(
    argument_into_a_typedef_union_of_one_variable,
    r#"{
  let f = |x: E<'a, 'a>| -> (Array<'a>, Array<'a>) ([], []);
  type E<'p, 'q> = ['p, 'q];
  let g = |p, q| { let u = never(); let r = f(u); u <- p; u <- q; r };
  g(1, 2)
}"#,
    prints("[[], []]")
);
// One undecided member that can take the type takes it: a constructor
// before a bare variable, and a member that holds it as it stands first.
run!(
    one_undecided_member_takes_the_type,
    r#"{
  let f = |r: Result<'a, 'e>| select r { error as _ => 0, _ => 1 };
  (f(1), f(error(`X)), f("s"))
}"#,
    prints("[i64:1, i64:0, i64:1]")
);
// A decided member that can take an undecided type takes it before an
// undecided one and decides it, whatever the members' order or names:
// the wrapper's parameter is the decided member's element. A parameter
// with its type written goes to the member that holds that type.
run!(
    undecided_element_goes_to_the_decided_member_beside_an_undecided_one,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let h = |x0: {v: 'a}, x1: P<'a, 'b>| x1;
  let f = |y| h({v: null}, [y]);
  f(null)
}"#,
    prints("[null]")
);
run!(
    undecided_element_a_decided_member_cannot_hold_goes_to_the_open_one,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let h = |x0: {v: 'a}, x1: P<'a, 'b>| x1;
  let f = |y| h({v: null}, [y]);
  f((1, 2))
}"#,
    prints("[[i64:1, i64:2]]");
    FuseExpect::Jit
);

run!(
    undecided_element_no_member_can_hold_is_refused,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let h = |x0: {v: 'a}, x1: P<'a, 'b>| x1;
  let f = |y| h({v: null}, [y]);
  let r: [Array<null>, null] = f((1, 2));
  r
}"#,
    refused("does not contain Array<(i64, i64)>");
    FuseExpect::None
);
run!(
    undecided_element_a_decided_member_cannot_hold_goes_to_the_open_one_in_the_other_member_order,
    r#"{
  type P<'x, 'y> = [Array<'y>, Array<'x>, null];
  let h = |x0: {v: 'b}, x1: P<'b, 'a>| x1;
  let f = |y| h({v: null}, [y]);
  f((1, 2))
}"#,
    prints("[[i64:1, i64:2]]");
    FuseExpect::Jit
);

run!(
    undecided_element_no_member_can_hold_is_refused_in_the_other_member_order,
    r#"{
  type P<'x, 'y> = [Array<'y>, Array<'x>, null];
  let h = |x0: {v: 'b}, x1: P<'b, 'a>| x1;
  let f = |y| h({v: null}, [y]);
  let r: [Array<null>, null] = f((1, 2));
  r
}"#,
    refused("does not contain Array<(i64, i64)>");
    FuseExpect::None
);
run!(
    written_element_goes_to_the_undecided_member_beside_a_decided_one,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let h = |x0: {v: 'a}, x1: P<'a, 'b>| x1;
  let f = |y: (i64, i64)| h({v: null}, [y]);
  f((1, 2))
}"#,
    prints("[[i64:1, i64:2]]")
);
run!(
    undecided_element_into_a_typedef_union_before_its_decider_goes_to_the_open_member,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let h = |x1: P<'a, 'b>, x0: {v: 'a}| x1;
  let f = |y| h([y], {v: null});
  f((1, 2))
}"#,
    prints("[[i64:1, i64:2]]");
    FuseExpect::Jit
);
// An equality decides both members of the union by what must hold it.
run!(
    arithmetic_over_a_union_of_two_undecided,
    r#"{
  let f = |c, a, b| { let u = select c { true => a, false => b }; u + 1 };
  f(true, 1, 2)
}"#,
    prints("i64:2")
);
// A select's patterns stay ordered, as its arms are consulted top down, and
// type an unannotated parameter as a union of one array member an arm. The
// members hold one element cell, so they are one type and take an argument
// as one member, as the parameter written does: an arm that binds the rest,
// two arms that bind elements, a recursive function, a slice inside a tuple
// or a struct, a list, a function passed where a function type is written.
run!(
    slice_arms_over_an_unannotated_parameter,
    r#"{ let f = |x| select x { [h, t..] => h, [] => 0 }; (f([4, 5]), f(["a"])) }"#,
    prints(r#"[i64:4, "a"]"#)
);
run!(
    slice_arms_over_a_written_parameter,
    r#"{ let f = |x: Array<'e>| select x { [h, t..] => h, [] => 0 }; (f([4, 5]), f(["a"])) }"#,
    prints(r#"[i64:4, "a"]"#)
);
run!(
    slice_arms_binding_no_rest_over_an_unannotated_parameter,
    r#"{ let f = |x| select x { [h, ..] => h, [] => 0 }; (f([4, 5]), f(["a"])) }"#,
    prints(r#"[i64:4, "a"]"#)
);
run!(
    slice_elements_bound_in_two_arms_over_an_unannotated_parameter,
    r#"{ let f = |x| select x { [a, b, ..] => a, [c] => c, [] => 0 }; (f([4, 5]), f(["a"])) }"#,
    prints(r#"[i64:4, "a"]"#)
);
run!(
    slice_elements_bound_in_two_arms_over_a_written_parameter,
    r#"{ let f = |x: Array<'e>| select x { [a, b, ..] => a, [c] => c, [] => 0 }; (f([4, 5]), f(["a"])) }"#,
    prints(r#"[i64:4, "a"]"#)
);
run!(
    slice_arms_of_a_recursive_function_over_an_unannotated_parameter,
    r#"{
  let rec len = |x| select x { [h, t..] => 1 + len(t), [] => 0 };
  let rec last = |x| select x { [h] => h, [h, t..] => last(t), [] => never() };
  let rec rev = |x| select x { [h, t..] => array::push(rev(t), h), [] => [] };
  (len([1, 2]), len(["a"]), last([1, 2]), last(["a"]), rev([1, 2, 3]))
}"#,
    prints(r#"[i64:2, i64:1, i64:2, "a", [i64:3, i64:2, i64:1]]"#)
);
run!(
    slice_arms_of_a_recursive_function_over_a_written_parameter,
    r#"{
  let rec len = |x: Array<'e>| select x { [h, t..] => 1 + len(t), [] => 0 };
  (len([1, 2]), len(["a"]))
}"#,
    prints("[i64:2, i64:1]")
);
run!(
    slice_rest_passed_on_over_an_unannotated_parameter,
    r#"{
  let f = |x| select x { [h, t..] => array::push(t, h), [] => [] };
  let g = |x| select x { [h, t..] => (h, t), [] => (0, []) };
  let k = |x| select x { [init.., l] => l, [] => 0 };
  (f([1, 2]), f(["a", "b"]), g([4, 5]), k([4, 5]))
}"#,
    prints(r#"[[i64:2, i64:1], ["b", "a"], [i64:4, [i64:5]], i64:5]"#)
);
run!(
    slice_arms_inside_a_tuple_and_a_struct_over_an_unannotated_parameter,
    r#"{
  let f = |p| select p { (x, [h, t..]) => h, (x, []) => x };
  let g = |s| select s { {a: [h, t..]} => h, {a: []} => 0 };
  (f((1, [2])), f((1, [])), g({a: [3, 4]}))
}"#,
    prints("[i64:2, i64:1, i64:3]")
);
// A part of an unannotated parameter that three arms or more bind is one
// cell, as its slices' elements are: each arm's bind holds its part of
// every member the arm can match, the other arms' own among them. The
// members are one type and take an argument as one: slice arms in a tuple
// and a struct, arms that bind a rest, a later arm deciding the part,
// literal arms, a slice in each alternative of an or-arm.
run!(
    part_bound_in_three_slice_arms_over_an_unannotated_parameter,
    r#"{
  let f = |s| select s { ([a, c, ..], b) => (a, b), ([d], b) => (b, d), ([], b) => (b, b) };
  let g = |s| select s { {a: [x, y, ..], b} => b, {a: [z], b} => b, {a: [], b} => b };
  let h = |s| select s { ([a, c, t..], b) => 2, ([d, u..], b) => 1, ([], b) => 0 };
  let k = |s| select s {
    ([a, c, e, ..], b) => b, ([a, c], b) => b, ([d], b) => b + 1, ([], b) => b
  };
  (f(([1, 2], "q")), f(([3], "r")), g({a: [1], b: "q"}), h(([1, 2], "q")), k(([1], 5)))
}"#,
    prints(r#"[[i64:1, "q"], ["r", i64:3], "q", i64:2, i64:6]"#)
);
run!(
    part_bound_in_three_slice_arms_over_a_written_parameter,
    r#"{
  let f = |s: (Array<'e>, 'b)| select s {
    ([a, c, ..], b) => (a, b), ([d], b) => (b, d), ([], b) => (b, b)
  };
  (f(([1, 2], "q")), f(([3], "r")))
}"#,
    prints(r#"[[i64:1, "q"], ["r", i64:3]]"#)
);
run!(
    part_bound_in_three_literal_arms_over_an_unannotated_parameter,
    r#"{
  let f = |s| select s { (1, b) => (b, 1), (2, b) => (b, 2), (3, b) => (b, 3), (_, b) => (b, 0) };
  (f((1, "q")), f((9, 4)))
}"#,
    prints(r#"[["q", i64:1], [i64:4, i64:0]]"#)
);
run!(
    slice_in_each_alternative_of_an_or_arm_over_an_unannotated_parameter,
    r#"{
  let f = |x| select x { `A([h, ..]) | `B([h, ..]) => h, _ => 0 };
  let g = |x| select x { `A([h, ..]) | `B([h, ..]) => h, `A([]) => 0, `B([]) => 0 };
  (f(`A([1, 2])), f(`B([])), g(`B([3])), g(`A([])))
}"#,
    prints("[i64:1, i64:0, i64:3, i64:0]")
);
run!(
    slices_of_other_shapes_in_the_alternatives_of_an_or_arm_over_an_unannotated_parameter,
    r#"{
  let rest = |x| select x { `A([h, t..]) | `B([h, t..]) => (h, t), _ => (0, []) };
  let three = |x| select x { `A([h, ..]) | `B([h, ..]) | `C([h, ..]) => h, _ => 0 };
  let apart = |x| select x { `A([_, l]) | `B([l, ..]) => l, _ => 0 };
  let whole = |x| select x { `A([h, ..]) | `B(h) => h, _ => 0 };
  let beside = |x| select x {
    `A([h, ..], k) | `B([h, ..], k) => (h, k), `A([], k) => (0, k), `B([], k) => (1, k)
  };
  let field = |x| select x { `A({a: [h, ..]}) | `B({a: [h, ..]}) => h, _ => 0 };
  let inner = |x| select x { [`A([h, ..]), ..] | [`B([h, ..]), ..] => h, _ => 0 };
  let nested = |x| select x { `A([[h, ..], ..]) | `B([[h, ..], ..]) => h, _ => 0 };
  let pair = |x| select x { (1, [h, ..]) | (2, [h, ..]) => h, _ => 0 };
  (
    (rest(`A([1, 2])), three(`C([7])), apart(`A([1, 2])), apart(`B([3, 4]))),
    (whole(`B(3)), beside(`A([1, 2], "a")), beside(`B([], "b")), field(`B({a: [5]}))),
    (inner([`B([6])]), nested(`A([[8], [9]])), nested(`B([[]])), pair((2, [9])))
  )
}"#,
    prints(
        r#"[[[i64:1, [i64:2]], i64:7, i64:2, i64:3], [i64:3, [i64:1, "a"], [i64:1, "b"], i64:5], [i64:6, i64:8, i64:0, i64:9]]"#
    )
);
run!(
    slices_of_an_or_arm_over_a_written_union_whose_elements_differ_bind_their_union,
    r#"{
  let f = |x: [`A(Array<i64>), `B(Array<string>)]| select x {
    `A([h, ..]) | `B([h, ..]) => "[h]", _ => ""
  };
  let rest = |x: [`A(Array<i64>), `B(Array<string>)]| select x {
    `A([h, t..]) | `B([h, t..]) => ("[h]", t), _ => ("", [])
  };
  let nested = |x: [`X([`A(Array<i64>), `B(Array<f64>)]), `Y(Array<string>)]| select x {
    `X(`A([h, ..]) | `B([h, ..])) | `Y([h, ..]) => "[h]", _ => ""
  };
  let beside = |x: [`A(Array<i64>, bool), `B(Array<string>, bool), `C(string)]| select x {
    `A([h, ..], _) | `B([h, ..], _) | `C(h) => "[h]", _ => ""
  };
  (
    (f(`A([1])), f(`B(["a"])), f(`B([]))),
    (rest(`A([1, 2])), rest(`B(["q", "r"]))),
    (nested(`X(`A([3]))), nested(`X(`B([2.5]))), nested(`Y(["s"]))),
    (beside(`A([1], true)), beside(`B(["b"], true)), beside(`C("c")))
  )
}"#,
    prints(
        r#"[["1", "a", ""], [["1", [i64:2]], ["q", ["r"]]], ["3", "2.5", "s"], ["1", "b", "c"]]"#
    )
);
run!(
    slices_of_an_or_arm_over_a_written_union_of_other_shapes_bind_their_union,
    r#"{
  let list = |x: [Array<i64>, List<string>]| select x {
    [h, ..] | [<h, ..>] => "[h]", _ => ""
  };
  let tuple = |x: [Array<i64>, (string, i64)]| select x {
    [h, ..] | (h, _) => "[h]", _ => ""
  };
  let tuple_first = |x: [Array<i64>, (string, i64)]| select x {
    (h, _) | [h, ..] => "[h]", _ => ""
  };
  let wide = |x: [`A(Array<i64>), `B(Array<[i64, string]>)]| select x {
    `A([h, ..]) | `B([h, ..]) => "[h]", _ => ""
  };
  (
    (list([1]), list([<"a">]), list([])),
    (tuple([1]), tuple(("a", 1)), tuple_first([2]), tuple_first(("b", 1))),
    (wide(`A([1])), wide(`B(["a"])))
  )
}"#,
    prints(r#"[["1", "a", ""], ["1", "a", "2", "b"], ["1", "a"]]"#)
);
run!(
    slices_of_an_or_arm_over_a_written_union_used_as_a_number_refused,
    r#"{
  let f = |x: [`A(Array<i64>), `B(Array<string>)]| select x {
    `A([h, ..]) | `B([h, ..]) => h + 1, _ => 0
  };
  f(`A([1]))
}"#,
    refused("type mismatch i64 does not contain [i64, string]");
    FuseExpect::None
);
run!(
    slice_in_each_alternative_of_an_or_arm_binds_one_cell_in_one_instance,
    r#"{
  let mk = |z| |x| select x { `A([h, ..]) | `B([h, ..]) => h, _ => 0 };
  let g = mk(0);
  (g(`A([1, 2])), g(`B([3])), g(`B([])))
}"#,
    prints("[i64:1, i64:3, i64:0]");
    FuseExpect::None
);
run!(
    slice_in_each_alternative_of_an_or_arm_as_two_types_in_one_instance_holds_both,
    r#"{
  let mk = |z| |x| select x { `A([h, ..]) | `B([h, ..]) => h, _ => 0 };
  let g = mk(0);
  (g(`A([1, 2])), g(`B(["a"])))
}"#,
    prints(r#"[i64:1, "a"]"#);
    FuseExpect::None
);
run!(
    slice_in_each_alternative_of_an_or_arm_over_a_binding_written_later,
    r#"{
  let v = never();
  let r = select v { `A([h, ..]) | `B([h, ..]) => h, _ => 0 };
  v <- `A([1, 2]);
  r
}"#,
    prints("i64:1")
);
run!(
    slice_in_each_alternative_of_an_or_arm_used_as_a_number_then_written_a_string_refused,
    r#"{
  let v = never();
  let r = select v { `A([h, ..]) | `B([h, ..]) => h + 1, _ => 0 };
  v <- `B(["a"]);
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
/// The cells bound to another cell that the type of `name` reaches after
/// the check of `code`.
async fn forward_links(code: &str, name: &str) -> Result<usize> {
    use ahash::AHashMap;
    use graphix_compiler::{Scope, expr::ModPath, typ::Type};
    use graphix_package_core::testing::init;
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let _c = ctx.rt.compile(arcstr::ArcStr::from(code)).await?;
    let env = ctx.rt.get_env().await?;
    let (_, bind) = env
        .lookup_bind(&Scope::root().lexical, &ModPath::from_iter([name]))?
        .ok_or_else(|| anyhow::anyhow!("{name} not in scope"))?;
    let mut cells = AHashMap::default();
    bind.typ.reached_cells(&mut cells);
    let links =
        cells.values().filter(|c| matches!(c.binding(), Some(Type::TVar(_)))).count();
    ctx.shutdown().await;
    Ok(links)
}

/// `arms` slice arms of `arm`, the longest first, then `last`: `{names}`
/// is the arm's element binds.
fn slice_arms(arms: usize, arm: &str, last: &str) -> String {
    let mut code = String::from("let f = |x| select x {\n");
    for k in (1..=arms).rev() {
        let names: Vec<String> = (0..k).map(|i| format!("a{i}")).collect();
        code.push_str(&arm.replace("{names}", &names.join(", ")));
        code.push_str(",\n");
    }
    code.push_str(last);
    code.push_str("\n}");
    code
}

// The check of slice arms over an unannotated parameter costs what the arms
// hold: an arm narrows against each member the arms gave the parameter
// through a copy of its cells, and a copy holds one cell for a cell however
// many links lead to it, none for a cell the member holds alone where the
// arm does. So the links the function's type reaches are few where the arms
// share one cell, and grow with the arms and the members where an arm's copy
// takes a member's cell, never by a factor at each arm.
// CR claude for claude: [perf/low] This test bounds the links f's type reaches, which
// does not measure the check's work. The 'nested' ladder it builds checks in about 210
// ms at 24 arms, 480 ms at 32, 2.5 s at 48 and 9.4 s at 64 in the debug binary (about
// arms^4.5), while the same ladder over `|x: Array<Array<'e>>|` stays at 80-130 ms. The
// 'rest' ladder takes 3.2 s at 128 arms against 180 ms written. The cost is in the
// unannotated path alone. The comment says the members the arms give the parameter are
// one type, so type them as one member before the arms narrow against them, as the
// written parameter does. Then bound a work counter of the check at 4 and 12 arms in
// place of the links. probe:
// design/review-2026-11-16/repro/tests-types-c-nested-slice-ladder-cost.sh
// (tests-types-c-05)
#[tokio::test(flavor = "current_thread")]
async fn slice_arms_over_an_unannotated_parameter_copy_no_chain_of_cells() -> Result<()> {
    type Most = fn(usize) -> usize;
    let (few, each, pairs): (Most, Most, Most) = (|_| 2, |arms| arms, |arms| arms * arms);
    for (shape, arm, last, most) in [
        ("rest", "[{names}, t..] => t", "[] => []", few),
        ("suffix", "[t.., {names}] => t", "[] => []", few),
        ("list", "[<{names}, t..>] => t", "[<>] => [<>]", few),
        ("in a variant", "`A([{names}, t..]) => t", "`A([]) => []", few),
        ("in a tuple", "([{names}, t..], b) => (t, b)", "([], b) => ([], b)", each),
        ("nested", "[[{names}, t..], u..] => t", "_ => []", pairs),
    ] {
        for arms in [4, 12] {
            let links = forward_links(&slice_arms(arms, arm, last), "f").await?;
            assert!(links <= most(arms), "{shape}, {arms} arms: {links} links");
        }
    }
    Ok(())
}

run!(
    ladder_of_rest_slice_arms_over_an_unannotated_parameter,
    r#"{
  let g = |x| select x {
    [a0, a1, a2, a3, a4, a5, t..] => t,
    [a0, a1, a2, a3, a4, t..] => t,
    [a0, a1, a2, a3, t..] => t,
    [a0, a1, a2, t..] => t,
    [a0, a1, t..] => t,
    [a0, t..] => [a0],
    [] => []
  };
  (g([1, 2, 3, 4, 5, 6, 7, 8]), g(["a", "b", "c"]), g(["d"]), g([]))
}"#,
    prints(r#"[[i64:7, i64:8], [], ["d"], []]"#)
);
// Behind an arm that made the members one type, an arm whose slice's rest
// stands beside an array element narrows over the members, which makes
// the rest's elements the array's: it runs as its written twin does.
run!(
    slice_rest_beside_an_array_element_behind_members_of_one_type,
    r#"{
  let f = |x| select x { [[a, s..], [b, r..], t..] => a + b, [[a, s..], t..] => a, _ => 0 };
  let g = |x: Array<Array<i64>>| select x {
    [[a, s..], [b, r..], t..] => a + b, [[a, s..], t..] => a, _ => 0
  };
  (f([[5], [6]]), f([[7]]), f([]), g([[5], [6]]), g([[7]]))
}"#,
    prints("[i64:11, i64:7, i64:0, i64:11, i64:7]")
);
run!(
    ladder_of_nested_slice_arms_over_an_unannotated_parameter,
    r#"{
  let f = |x| select x {
    [[a0, a1, a2, a3, t..], u..] => (t, u),
    [[a0, a1, a2, t..], u..] => (t, u),
    [[a0, a1, t..], u..] => (t, u),
    [[a0, t..], u..] => (t, u),
    _ => ([], [])
  };
  (f([[1, 2, 3, 4, 5], [6]]), f([[1, 2], [3], [4]]), f([[]]))
}"#,
    prints("[[[i64:5], [[i64:6]]], [[], [[i64:3], [i64:4]]], [[], []]]")
);
// One cell is one type: arms that use the part as two types are refused
// where the function is defined, whichever arm a call would take.
run!(
    part_bound_in_three_literal_arms_used_as_two_types_refused,
    r#"{
  let f = |s| select s { (1, b) => 0, (2, b) => b + 1, (3, b) => str::len(b), _ => 0 };
  f((2, "q"))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    part_bound_in_three_slice_arms_used_as_two_types_only_the_empty_arm_reaches,
    r#"{
  let f = |s| select s { ([a, c, ..], b) => 0, ([d], b) => b + 1, ([], b) => str::len(b) };
  f(([], "q"))
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    part_bound_in_three_slice_arms_used_as_two_types_over_a_full_array_refused,
    r#"{
  let f = |s| select s { ([a, c, ..], b) => 0, ([d], b) => b + 1, ([], b) => str::len(b) };
  f(([1], "q"))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
// Arms that bind one part of one variant tag bind one cell too, where the
// arms' predicates join as one variant over the union of their cells: each
// arm's bind receives what the others' do.
const PAYLOAD_AS_TWO_TYPES: &str = "type mismatch string does not contain i64";

run!(
    payload_bound_in_two_literal_arms_used_as_two_types_refused,
    r#"{
  let f = |s| select s { `A(1, a) => a + 1, `A(2, a) => str::len(a), _ => 0 };
  f(`A(1, "q"))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    payload_bound_in_two_literal_arms_beside_another_tag_used_as_two_types_refused,
    r#"{
  let f = |s| select s { `A(1, a) => a + 1, `A(2, a) => str::len(a), `B(a) => a, _ => 0 };
  f(`A(2, 5))
}"#,
    refused(PAYLOAD_AS_TWO_TYPES);
    FuseExpect::None
);
run!(
    payload_bound_in_two_guarded_arms_used_as_two_types_refused,
    r#"{
  let f = |s| select s { `A(a) if a > 0 => a + 1, `A(a) if a != "" => str::len(a), _ => 0 };
  f(`A("q"))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    nested_payload_bound_in_two_literal_arms_used_as_two_types_refused,
    r#"{
  let f = |s| select s { `A(1, `B(a)) => a + 1, `A(2, `B(a)) => str::len(a), _ => 0 };
  f(`A(1, `B("q")))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    payload_bound_in_two_literal_arms_over_a_let_written_later_refused,
    r#"{
  let t = sys::time::timer(duration:10.ms, false);
  let v = never();
  let z = select v { `A(1, a) => a + 1, `A(2, a) => str::len(a), _ => 0 };
  v <- t ~ `A(1, "q");
  z
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
// A payload the scrutinee held before the arms is the arms' one cell: the
// arm that uses it decides what a later write may be.
run!(
    payload_bound_in_two_literal_arms_holds_an_undecided_payload,
    r#"{
  let t = sys::time::timer(duration:10.ms, false);
  let x = never();
  let v = never();
  v <- t ~ `A(2, x);
  let z = select v { `A(1, a) => 0, `A(2, a) => a + 1, _ => 0 };
  x <- t ~ "q";
  z
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    payload_bound_in_two_literal_arms_as_one_type,
    r#"{
  let f = |s| select s { `A(1, a) => a + 1, `A(2, a) => a + 2, _ => 0 };
  let g = |s| select s { `A(true, a) => a, `A(false, a) => a + 1 };
  (f(`A(1, 5)), f(`A(2, 5)), f(`A(3, 5)), g(`A(true, 3)), g(`A(false, 3)))
}"#,
    prints("[i64:6, i64:7, i64:0, i64:3, i64:4]")
);
// The one cell is one variable of the function: each call decides it.
run!(
    payload_bound_in_two_literal_arms_is_one_variable,
    r#"{
  let f = |s| select s { `A(1, a) => a, `A(2, a) => a, _ => never() };
  (f(`A(1, "q")), f(`A(2, 5)))
}"#,
    prints(r#"["q", i64:5]"#)
);
// An arm that tests the part another binds is no bind of it.
run!(
    payload_tested_by_one_arm_and_bound_by_the_next,
    r#"{
  let f = |s| select s { `A(null) => 0, `A(a) => a + 1 };
  let g = |s| select s { `A(`X) => "x", `A(a) => "other" };
  (f(`A(null)), f(`A(3)), g(`A(`X)), g(`A(`Y)))
}"#,
    prints(r#"[i64:0, i64:4, "x", "other"]"#)
);
// The same holds where the scrutinee is no parameter of its own: a variant
// built over one, a parameter an earlier select's arms typed, and the bind
// of an outer arm.
run!(
    payload_bound_in_two_literal_arms_of_a_built_variant_used_as_two_types_refused,
    r#"{
  let f = |x| select `A(1, x) { `A(1, a) => a + 1, `A(2, a) => str::len(a), _ => 0 };
  f("q")
}"#,
    refused("a value here is used as string and as i64, and no value is both: nothing can be passed for it");
    FuseExpect::None
);
run!(
    payload_bound_in_two_literal_arms_of_a_second_select_used_as_two_types_refused,
    r#"{
  let f = |s| {
    let u = select s { `A(1, a) => 0, `A(2, b) => 1, _ => 2 };
    let w = select s { `A(3, c) => c + 1, `A(4, d) => str::len(d), _ => 0 };
    (u, w)
  };
  f(`A(3, "q"))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    payload_bound_in_two_literal_arms_over_an_outer_arms_bind_used_as_two_types_refused,
    r#"{
  let f = |s| select s {
    `P(x, 1) => select x { `A(4, d) => str::len(d), `A(5, e) => e + 1, _ => 0 },
    `P(y, _) => 0
  };
  f(`P(`A(5, "q"), 1))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);
run!(
    payload_bound_in_two_literal_arms_of_other_scrutinees_as_one_type,
    r#"{
  let f = |x| select `A(1, x) { `A(1, a) => a + 1, `A(2, a) => a + 2, _ => 0 };
  let g = |s| {
    let u = select s { `A(1, a) => 0, `A(2, b) => 1, _ => 2 };
    let w = select s { `A(3, c) => c + 1, `A(4, d) => d + 2, _ => 0 };
    (u, w)
  };
  let h = |s| select s {
    `P(x, 1) => select x { `A(4, d) => d, `A(5, e) => e + 1, _ => 0 },
    `P(y, _) => 0
  };
  (f(5), g(`A(1, 0)), g(`A(4, 4)), h(`P(`A(4, 7), 1)), h(`P(`A(5, 7), 2)))
}"#,
    prints("[i64:6, [i64:0, i64:0], [i64:2, i64:6], i64:7, i64:0]")
);
// The arms' one cell holds what an undecided payload gives it once, however
// many arms bind the part: the check ends, and the part is one type.
run!(
    part_bound_in_four_literal_arms_over_an_undecided_payload,
    r#"{
  type B<'p> = Abstract<'p>;
  let t = sys::time::timer(duration:10.ms, false);
  let x = never();
  let b = B((2, x));
  let v = select b.0 { (1, h) => 0, (2, h) => h + 1, (3, h) => h, (4, h) => h, (_, h) => h };
  x <- t ~ 7;
  v
}"#,
    prints("i64:8")
);
run!(
    part_bound_in_four_literal_arms_over_an_undecided_payload_used_as_two_types_refused,
    r#"{
  type B<'p> = Abstract<'p>;
  let t = sys::time::timer(duration:10.ms, false);
  let x = never();
  let b = B((2, x));
  let v = select b.0 {
    (1, h) => 0, (2, h) => h + 1, (3, h) => str::len(h), (4, h) => h, (_, h) => h
  };
  x <- t ~ 7;
  v
}"#,
    refused("type mismatch string does not contain i64");
    FuseExpect::None
);
// Arms whose binds made their members one type are named once by the
// refusal of what they leave uncovered.
run!(
    missing_cases_name_members_that_are_one_type_once,
    r#"{
  let f = |s| select s { ([a], [b], c) => 0, ([a, ..], [], c) => 1, ([], [b, ..], c) => 2 };
  f(([1], [2], 3))
}"#,
    refused("missing match cases: nothing matches (Array<i64>, Array<i64>, i64) (2:22)");
    FuseExpect::None
);
// Over a parameter written with two variables, a slice arm's element cells
// are the declared element, so the arm's other binds take their parts of
// the parameter: the result is of the second variable in both shapes.
run!(
    slice_arm_inside_a_written_tuple_binds_its_other_variable,
    r#"{
  let f = |s: (Array<'e>, 'a)| select s { ([h, ..], b) => b, ([], b) => (b, b) };
  let r = f(([1], "x"));
  let q = f(([], 4));
  (r, q)
}"#,
    prints(r#"["x", [i64:4, i64:4]]"#)
);
run!(
    slice_arm_inside_a_written_struct_binds_its_other_variable,
    r#"{
  let f = |s: {a: Array<'e>, b: 'a}| select s { {a: [h, t..], b} => b, {a: [], b} => [b] };
  let r = f({a: [1, 2], b: "x"});
  let q = f({a: [], b: 4});
  (r, q)
}"#,
    prints(r#"["x", [i64:4]]"#)
);
run!(
    slice_arm_inside_a_written_tuple_types_its_result_by_its_other_variable,
    r#"{
  let f = |s: (Array<'e>, 'a)| select s { ([h, ..], b) => b, ([], b) => (b, b) };
  let r: [string, (i64, i64)] = f(([1], 3));
  r
}"#,
    refused("type mismatch [string, (i64, i64)] does not contain");
    FuseExpect::None
);
// A declared variable holds a union of two undecided types as each of
// them, and no union with a member it does not hold.
run!(
    declared_result_over_arms_of_two_undecided_parameters,
    r#"{
  let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b };
  (f(1, true, 2, 3), f("a", false, "b", "c"))
}"#,
    prints(r#"[i64:2, "c"]"#)
);
run!(
    declared_result_over_arms_of_two_undecided_parameters_takes_both_types,
    r#"{
  let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b };
  f(1, true, 2, "s")
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

run!(
    declared_result_over_arms_of_two_undecided_parameters_read_as_one_type_refused,
    r#"{
  let f = |x: 'e, c: bool, a, b| -> 'e select c { true => a, false => b };
  let r: i64 = f(1, true, 2, "s");
  r
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);
run!(
    declared_result_over_an_arm_of_another_type_refused,
    r#"{
  let f = |x: 'e, c: bool, a| -> 'e select c { true => a, false => 1 };
  f(1, true, 2)
}"#,
    refused("a call picks 'e, and the body's value need not be of the type it picks");
    FuseExpect::None
);
run!(
    slice_arm_over_a_declared_list_binds_its_other_variable,
    r#"{
  let f = |s: (List<'e>, 'a)| select s { ([<h, t..>], b) => b, ([<>], b) => (b, b) };
  let r = f(([<1, 2>], 3));
  let q = f(([<>], "x"));
  (r, q)
}"#,
    prints(r#"[i64:3, ["x", "x"]]"#)
);
run!(
    slice_arm_over_a_bounded_alias_binds_its_other_variable,
    r#"{
  type P<'x> = Array<'x>;
  let f = 'e: Number |s: (P<'e>, Array<'e>, 'a)| select s {
    ([h, ..], [i.., l], b) => b,
    (_, _, b) => (b, b)
  };
  let r = f(([1], [2, 3], 3));
  let q = f(([], [1.], "x"));
  (r, q)
}"#,
    prints(r#"[i64:3, ["x", "x"]]"#)
);
// A union that holds a declared variable beside an undecided member gives
// a value still undecided to the variable, two of them as it gives one.
run!(
    union_beside_a_declared_variable_holds_one_written_value_itself,
    r#"{
  let f = |x: 'e, c: bool, a| {
    let o = never();
    let u = select c { true => [x], false => o };
    u <- [a];
    u
  };
  f(1, false, "s")
}"#,
    prints(r#"["s"]"#);
    FuseExpect::Jit
);
run!(
    union_beside_a_declared_variable_holds_two_written_values_itself,
    r#"{
  let f = |x: 'e, c: bool, a, b| {
    let o = never();
    let u = select c { true => [x], false => o };
    u <- [a, b];
    u
  };
  f(1, false, "s", 2.)
}"#,
    prints(r#"["s", f64:2.]"#);
    FuseExpect::Jit
);
run!(
    union_beside_a_declared_variable_takes_two_written_values,
    r#"{
  let f = |x: 'e, c: bool, a: string, b: f64| {
    let o = never();
    let u = select c { true => [x], false => o };
    u <- [a, b];
    u
  };
  f(1, false, "s", 2.)
}"#,
    prints(r#"["s", f64:2.]"#)
);
run!(
    list_arms_over_an_unannotated_parameter,
    r#"{
  let rec len = |x| select x { [<h, t..>] => 1 + len(t), [<>] => 0 };
  let f = |x| select x { [<h, t..>] => list::cons(h, t), [<>] => [<>] };
  (len([<1, 2>]), list::to_array(f([<1, 2>])), list::to_array(f([<"a">])))
}"#,
    prints(r#"[i64:2, [i64:1, i64:2], ["a"]]"#)
);
run!(
    slice_arms_over_an_unannotated_parameter_passed_as_a_written_function,
    r#"{
  let k = |x| select x { [h, t..] => h, [] => 0 };
  let f = |g: fn(x: Array<i64>) -> i64, y| g(y);
  (f(k, [1, 2]), array::map([[4, 5], [6]], k))
}"#,
    prints("[i64:1, [i64:4, i64:6]]")
);
// An undecided array of two cells goes to the one member whole, and the
// instance holds the parameter where it stands, before the result, whatever
// the order of the arms: one cell standing twice is no union of two.
run!(
    array_of_two_undecided_into_slice_arms_over_an_unannotated_parameter,
    r#"{
  let f = |x| select x { [h, t..] => t, [] => [] };
  let g = |a, b| f([a, b]);
  g(1, "s")
}"#,
    prints(r#"["s"]"#)
);
run!(
    array_of_two_undecided_into_slice_arms_over_a_written_parameter,
    r#"{
  let f = |x: Array<'e>| select x { [h, t..] => t, [] => [] };
  let g = |a, b| f([a, b]);
  g(1, "s")
}"#,
    prints(r#"["s"]"#)
);
run!(
    slice_arms_after_an_empty_arm_over_an_unannotated_parameter,
    r#"{
  let f = |x| select x { [] => [], [h, t..] => t };
  let g = |x| select x { [h] => [h], [h, t..] => array::push(t, h), [] => [] };
  (f([1, 2]), f(["a", "b"]), g([1, 2]))
}"#,
    prints(r#"[[i64:2], ["b"], [i64:2, i64:1]]"#)
);
run!(
    slice_arm_returning_two_elements_over_an_unannotated_parameter,
    r#"{
  let f = |x| select x { [a, b, ..] => (a, b), _ => (0, 0) };
  let g = |x| select x { [a, b] => (b, a), _ => (0, 0) };
  (f(["b", "a"]), f([4, "a"]), g(["x", 1.5]), f([4]))
}"#,
    prints(r#"[["b", "a"], [i64:4, "a"], [f64:1.5, "x"], [i64:0, i64:0]]"#)
);
// What an argument decides for such a parameter is what it decides for the
// parameter written: the one element cell.
run!(
    slice_arms_over_an_unannotated_parameter_take_both_element_types,
    r#"{
  let f = |x| select x { [h, t..] => t, [] => [] };
  let g = |a, b| f(select true { true => a, false => b });
  g([1], ["a"])
}"#,
    prints("[]");
    FuseExpect::Jit
);

run!(
    slice_arms_over_an_unannotated_parameter_read_at_one_element_type_refused,
    r#"{
  let f = |x| select x { [h, t..] => t, [] => [] };
  let g = |a, b| f(select true { true => a, false => b });
  let r: Array<i64> = g([1], ["a"]);
  r
}"#,
    refused("type mismatch Array<i64> does not contain Array<string>");
    FuseExpect::None
);
run!(
    slice_arms_over_a_written_parameter_take_both_element_types,
    r#"{
  let f = |x: Array<'e>| select x { [h, t..] => t, [] => [] };
  let g = |a, b| f(select true { true => a, false => b });
  g([1], ["a"])
}"#,
    prints("[]");
    FuseExpect::Jit
);
// Members that hold two cells are two: an array either could take goes to
// neither.
run!(
    decided_array_into_a_typedef_union_of_two_undecided_arrays,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'b>| -> i64 0;
  f([1])
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    decided_array_into_a_typedef_union_of_one_variable,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'a>| -> i64 0;
  f([1])
}"#,
    prints("i64:0")
);
// Such a function reached another way is decided the same: through a
// function that passes it an array of two undecided values, read from a
// field, closing over an outer parameter, and with a bind beside the slice
// returned in two shapes.
run!(
    slice_arm_returning_two_elements_takes_an_array_of_two_undecided,
    r#"{
  let f = |x| select x { [a, b, ..] => (a, b), _ => (0, 0) };
  let g = |a, b| f([a, b]);
  (g(1, "s"), g(1.5, 2))
}"#,
    prints(r#"[[i64:1, "s"], [f64:1.5, i64:2]]"#)
);
run!(
    slice_arms_over_an_unannotated_parameter_read_from_a_field,
    r#"{
  let s = {f: |x| select x { [] => [], [h, t..] => t }};
  let f = s.f;
  (f([1, 2, 3]), f(["a", "b"]))
}"#,
    prints(r#"[[i64:2, i64:3], ["b"]]"#);
    FuseExpect::None
);
run!(
    slice_arms_over_an_unannotated_parameter_closing_over_an_outer_one,
    r#"{
  let outer = |y| {
    let f = |x| select x { [h, t..] => [h, y], [] => [y] };
    (f([2]), f([]))
  };
  (outer(1), outer("s"))
}"#,
    prints(r#"[[[i64:2, i64:1], [i64:1]], [[i64:2, "s"], ["s"]]]"#)
);
run!(
    slice_arms_beside_a_bind_returned_in_two_shapes_over_an_unannotated_parameter,
    r#"{
  let f = |s| select s { ([h, t..], b) => b, ([], b) => (b, b) };
  let g = |s| select s { {a: [h, t..], b} => b, {a: [], b} => [b] };
  (f(([1], 3)), f(([], "z")), g({a: [1, 2], b: 3}), g({a: [], b: "x"}))
}"#,
    prints(r#"[i64:3, ["z", "z"], i64:3, ["x"]]"#)
);

// An instance holds its definition's signature at the call's types cell by
// cell, each from its own place: a union the call left for its other
// positions to decide is decided for the instance the same way, so the
// build agrees with the check.
run!(
    undecided_argument_into_a_typedef_union_is_decided_by_the_other_parameters,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |x: E<'a, 'b>, y: 'a, z: 'b| -> (Array<'a>, Array<'b>) ([], []);
  f(never(), 1, "s")
}"#,
    prints("[[], []]")
);
run!(
    empty_array_into_a_typedef_union_is_decided_by_the_other_parameters,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |x: Array<E<'a, 'b>>, y: 'a, z: 'b| -> i64 array::len(x);
  f([], 1, "s")
}"#,
    prints("i64:0")
);
run!(
    empty_array_into_a_typedef_union_of_two_arrays_after_its_deciders,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, z: 'b, x: P<'a, 'b>| -> i64 0;
  l(1, "s", [])
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    typed_empty_array_into_a_typedef_union_of_two_arrays_after_its_deciders,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, z: 'b, x: P<'a, 'b>| -> i64 0;
  let e: Array<string> = [];
  l(1, "s", e)
}"#,
    prints("i64:0")
);

run!(
    undecided_argument_into_a_typedef_union_is_decided_by_the_result,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |x: E<'a, 'b>| -> (Array<'a>, Array<'b>) ([], []);
  let u = never();
  let r: (Array<i64>, Array<string>) = f(u);
  r
}"#,
    prints("[[], []]")
);
run!(
    two_members_one_constructor_member_could_take_are_placed_by_the_result,
    r#"{
  let f = |x: ['a, Error<'e>]| -> (Array<'a>, Array<'e>) ([], []);
  let v: [Error<`A>, Error<`B>] = error(`A);
  f(v)
}"#,
    prints("[[], []]")
);

// A type two undecided members of a bound's union could take is refused
// where the check meets it, through a wrapper or a nested function as at
// the direct call: no member takes an argument before it is decided.
run!(
    wrapper_into_a_bound_union_of_two_undecided,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a1, 'a2>, 'a1: P<'a2, 'a3>, 'a2: P<'a3, 'a4>, 'a3: P<'a4, 'a4>, 'a4: [i64, string] |x: ('a1, 'a0)| x;
  let g = |y| f(y);
  g(([null], null))
}"#,
    prints("[[null], null]");
    FuseExpect::Jit
);
run!(
    wrapper_into_a_bound_union_one_member_takes,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a1, 'a2>, 'a1: P<null, 'a3>, 'a2: P<'a3, 'a4>, 'a3: P<'a4, 'a4>, 'a4: [i64, string] |x: ('a1, 'a0)| x;
  let g = |y| f(y);
  g(([null], null))
}"#,
    prints("[[null], null]")
);
run!(
    nested_function_into_a_bound_union_of_two_undecided,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a1, 'a2>, 'a1: P<'a2, 'a3>, 'a2: P<'a3, 'a4>, 'a3: P<'a4, 'a4>, 'a4: [i64, string] |x: ('a1, 'a0)| x;
  let g = |u| { let h = |w| f(w); h(u) };
  g(([null], null))
}"#,
    prints("[[null], null]");
    FuseExpect::Jit
);
run!(
    nested_function_into_a_bound_union_one_member_takes,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a1, 'a2>, 'a1: P<null, 'a3>, 'a2: P<'a3, 'a4>, 'a3: P<'a4, 'a4>, 'a4: [i64, string] |x: ('a1, 'a0)| x;
  let g = |u| { let h = |w| f(w); h(u) };
  g(([null], null))
}"#,
    prints("[[null], null]")
);
run!(
    wrapper_argument_into_a_bound_union_beside_a_decided_member,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a1, 'a2>, 'a1: P<'a2, 'a3>, 'a2: P<'a3, 'a3>, 'a3: [i64, string] |x2: 'a2, x0: 'a0| x0;
  let g = |y2, y0| f(y2, y0);
  g(["s"], [[["s"]]])
}"#,
    prints(r#"[[["s"]]]"#)
);
run!(
    literal_into_the_second_member_of_a_named_union,
    r#"{
  type A = [Array<i64>, Array<string>];
  let f = |x: [A, null]| select x { A as [] => 0, A as [_, ..] => 1, null => 2 };
  let e: Array<i64> = [];
  [f(e), f(["a"]), f(null)]
}"#,
    prints("[i64:0, i64:1, i64:2]");
    FuseExpect::Jit
);
run!(
    empty_array_into_a_typedef_union_of_two_undecided_arrays,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'b>| -> i64 0;
  f([])
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    empty_array_into_a_typedef_union_of_two_decided_arrays,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<i64, string>| -> i64 0;
  f([])
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    typed_empty_array_into_a_typedef_union_of_two_decided_arrays,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<i64, string>| -> i64 0;
  let e: Array<i64> = [];
  f(e)
}"#,
    prints("i64:0")
);

run!(
    wrapper_empty_array_into_a_typedef_union_nothing_decides,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'b>, y: 'a, z: 'b| x;
  let g = |u, v, w| f(u, v, w);
  g([], never(), never())
}"#,
    prints("[]");
    FuseExpect::None
);

// A call's arguments are met in the order of the parameters, and nothing
// waits: an argument at a union of the callee's variables goes to the
// member the arguments before it decided, or to the one member still
// open. Before the arguments that would decide two open members it is
// refused, naming the remedy.
run!(
    argument_into_a_typedef_union_is_decided_by_the_parameters_before_it,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let m = |y: 'a, x: P<'a, 'b>, z: 'b| x;
  let l = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  (m(1, [1], "s"), l(1, "s", [1]), l(1, "s", ["t"]), l(1, 3, [2]))
}"#,
    prints(r#"[[i64:1], [i64:1], ["t"], [i64:2]]"#)
);

run!(
    argument_into_a_typedef_union_before_its_deciders,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'b>, y: 'a, z: 'b| x;
  f([1], 1, "s")
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

run!(
    decided_argument_into_a_typedef_union_no_parameter_decides,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'b>, y: 'a, z: 'b| x;
  f([1], never(), never())
}"#,
    prints("[i64:1]");
    FuseExpect::None
);
run!(
    argument_into_a_typedef_union_takes_the_member_left_open,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, x: P<'a, 'b>| x;
  l("s", [1])
}"#,
    prints(r#"[i64:1]"#)
);

// CR claude for claude: [test-gap/low] x stands first, so `f([1.5], 1, "s")` is refused
// before 1 and "s" decide anything. It gets the same AmbiguousMember message that
// argument_into_a_typedef_union_before_its_deciders_refused gets for `f([1], 1, "s")`,
// so the case the name claims, an argument no decided member holds, is never reached.
// Put the deciders first (`|y: 'a, z: 'b, x: P<'a, 'b>|`, `f(1, "s", [1.5])`, refused
// `type mismatch P<'a: i64, 'b: string> does not contain Array<f64>`), or delete the
// test as a duplicate. probe:
// design/review-2026-11-16/repro/tests-types-c-weak-union-pins.sh (tests-types-c-07)
run!(
    decided_argument_into_a_typedef_union_no_member_holds_it_refused,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: P<'a, 'b>, y: 'a, z: 'b| x;
  f([1.5], 1, "s")
}"#,
    refused("type mismatch [Array<i64>, Array<string>] does not contain Array<f64>");
    FuseExpect::None
);
run!(
    argument_into_a_typedef_union_of_bare_variables,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let l = |y: 'a, x: E<'a, 'b>| x;
  (l(2, 1), l("s", 1))
}"#,
    prints("[i64:1, i64:1]")
);

run!(
    argument_into_a_typedef_union_of_bare_variables_first,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = |x: E<'a, 'b>, y: 'a| x;
  f(1, 2)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// The order is the call's, whoever writes it: an inferred wrapper, one
// with the parameters written, a declared bound, a seq step.
run!(
    wrappers_of_a_typedef_union_function,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  let g = |v, w, u| l(v, w, u);
  let h = |a, b, c| g(a, b, c);
  let k = |v: 'a, w: 'b, u: P<'a, 'b>| l(v, w, u);
  let b = 'c: P<'a, 'b> |y: 'a, z: 'b, x: 'c| x;
  let t = 1;
  let s = seq t { let r = l(1, "s", [t]); r };
  (g(1, "s", [1]), h(1, "s", ["t"]), k(1, "s", [1]), b(1, "s", [1]), s)
}"#,
    prints(r#"[[i64:1], ["t"], [i64:1], [i64:1], [i64:1]]"#)
);
run!(
    wrapper_empty_array_into_a_typedef_union_after_its_deciders_goes_to_both,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  let g = |v, w, u| l(v, w, u);
  let h = |a, b, c| g(a, b, c);
  h(1, "s", [])
}"#,
    prints("[]");
    FuseExpect::Jit
);

// Labeled arguments are met where their parameters stand, and the
// defaults a call omits after the arguments it writes, so an omitted
// default decides no member for a written argument.
run!(
    argument_into_a_typedef_union_is_decided_by_labeled_arguments,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |#y: 'a = 1, #z: 'b = "s", x: P<'a, 'b>| x;
  (f(#y: 2, #z: "q", [3]), f(#z: "q", #y: 2, ["r"]))
}"#,
    prints(r#"[[i64:3], ["r"]]"#)
);

run!(
    argument_into_a_typedef_union_before_the_omitted_defaults,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |#y: 'a = 1, #z: 'b = "s", x: P<'a, 'b>| x;
  f([1])
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

// What one such argument decides places the next.
run!(
    arguments_into_typedef_unions_decide_each_other_in_order,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  type E<'p, 'q> = ['p, 'q];
  let l = |z: 'c, y: E<'a, 'c>, x: P<'a, 'b>| (x, y);
  l("s", 5, [1])
}"#,
    prints("[[i64:1], i64:5]")
);

run!(
    two_decided_arguments_into_typedef_unions_neither_decided,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  type E<'p, 'q> = ['p, 'q];
  let f = |x: P<'a, 'b>, y: E<'a, 'c>, z: 'd| (x, y);
  f([1], 5, "s")
}"#,
    prints("[[i64:1], i64:5]");
    FuseExpect::Jit
);
// A variable standing in a union beside another undecided member does
// not widen: the first argument that meets it decides it, so that
// argument is written as wide as the later ones need.
run!(
    variable_in_a_typedef_union_is_decided_by_the_first_argument,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let n: [i64, null] = null;
  let one: [i64, null] = 1;
  let f = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  f(one, "s", [n])
}"#,
    prints("[null]")
);

run!(
    variable_in_a_typedef_union_does_not_widen_refused,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let n: [i64, null] = null;
  let f = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  f(1, "s", [n])
}"#,
    refused("type mismatch [Array<i64>, Array<string>] does not contain Array<[i64, null]>");
    FuseExpect::None
);

// A literal's element meets the union as the arguments before it left
// it, as a bound value does.
run!(
    literal_element_into_a_typedef_union_after_its_deciders,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let id = |q| q;
  let f = |y: 'a, x: Map<string, E<'a, 'b>>| 0;
  let g = |y: 'a, x: Array<Array<E<'a, 'b>>>| 1;
  let h = |y: 'a, x: fn(q: i64) -> E<'a, 'b>| 2;
  (f("s", {"k" => 1.5}), g("s", [[1.5]]), h("s", id(|q: i64| 1.5)))
}"#,
    prints("[i64:0, i64:1, i64:2]")
);

// Of three members the one the arguments before it leave open takes the
// argument, or the one they decided that holds it; two left open are the
// choice still.
run!(
    argument_into_a_three_member_typedef_union,
    r#"{
  type P3<'p, 'q, 'r> = [Array<'p>, Array<'q>, Array<'r>];
  let l = |y: 'a, z: 'b, x: P3<'a, 'b, 'c>| x;
  (l("s", 1.5, [1]), l("s", 1, [1]))
}"#,
    prints("[[i64:1], [i64:1]]")
);

run!(
    argument_into_a_three_member_typedef_union_two_left_open,
    r#"{
  type P3<'p, 'q, 'r> = [Array<'p>, Array<'q>, Array<'r>];
  let l = |y: 'a, x: P3<'a, 'b, 'c>| x;
  l("s", [1])
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);

run!(
    three_decided_arguments_into_typedef_unions_none_decided,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  type E<'p, 'q> = ['p, 'q];
  let f = |x1: P<'a, 'b>, x2: E<'b, 'c>, x3: E<'c, 'a>| (x1, x2, x3);
  f([1], "s", 2)
}"#,
    prints(r#"[[i64:1], "s", i64:2]"#);
    FuseExpect::Jit
);
// The member the argument went to is the one the result reads: a test of
// the result's members, and a result typed by the variable of the member
// left open.
run!(
    argument_into_a_typedef_union_result_members,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, z: 'b, x: P<'a, 'b>| x;
  let r = l(1, "s", [1]);
  select r { Array<i64> as a => a[0]$ + 1, Array<string> as s => 0 }
}"#,
    prints("i64:2")
);

run!(
    argument_into_a_typedef_union_decides_the_open_member_for_the_result,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, x: P<'a, 'b>| -> Array<'b> [];
  let r: Array<i64> = l("s", [1]);
  let q: Array<string> = l(1, [1]);
  (r, q)
}"#,
    prints("[[], []]")
);

run!(
    argument_into_a_typedef_union_open_member_result_mismatch_refused,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let l = |y: 'a, x: P<'a, 'b>| -> Array<'b> [];
  let r: Array<string> = l("s", [1]);
  r
}"#,
    refused("type mismatch [Array<string>, Array<'b: unbound within string>] does not contain Array<i64>");
    FuseExpect::None
);

// A recursive call shares its definition's cells and is met the same.
run!(
    recursive_call_argument_into_a_typedef_union,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let rec f = |y: 'a, z: 'b, x: P<'a, 'b>, n: i64| -> i64 select n {
    0 => 0,
    _ => f(y, z, [y], n - 1) + 1
  };
  f(1, "s", [1], 3)
}"#,
    prints("i64:3")
);

// A wrapper's parameter passed at a formal that holds a variable the
// other arguments settle takes the formal's shape where it is passed, so
// an argument after it reads its parts.
run!(
    wrapper_parameter_read_by_a_later_argument_of_the_same_call,
    r#"{
  let f = |x: ('a, i64), z: 'a| x.1;
  let g = |y| f(y, y.0);
  let p = |x: {a: 'a, b: i64}, z: Array<'a>| x.b;
  let q = |y| p(y, [y.a]);
  (g((1, 2)), q({a: 1, b: 3}))
}"#,
    prints("[i64:2, i64:3]")
);
run!(
    wrapper_parameter_at_a_bounded_variable_read_by_a_later_argument,
    r#"{
  let f = 'a: (i64, i64) |x: 'a, z: i64| x;
  let g = |y| f(y, y.0);
  g((1, 2))
}"#,
    prints("[i64:1, i64:2]")
);

// An argument holding no undecided cell is not pre-unified with its
// formal's copy, so a declared variable passed on stands within its own
// bound and its callee's, over the instance's cells: a wrapper declaring
// its callee's chain of bounds takes what the callee takes, an undecided
// argument beside the declared ones included, and refuses what it refuses.
run!(
    declared_variables_passed_on_through_a_chain_of_union_bounds,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: 'a0, x2: 'a2, x3: 'a3| (x0, x2);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| f(y0, y2, y3);
  let k = 'c0: Q<'c2, 'c1>, 'c1: string, 'c2: Array<Q<'c4, 'c3>>, 'c3: O<'c4>, 'c4: string |z3: 'c3, z2: 'c2, z0: 'c0| h(z0, z2, z3);
  (f(`B("s"), [], "s"), h(`B("s"), [], "s"), k("s", [], `B("s")), h(`A([`B(null)]), [`A("s")], null))
}"#,
    prints(
        r#"[[["B", "s"], []], [["B", "s"], []], [["B", "s"], []], [["A", [["B", null]]], [["A", "s"]]]]"#
    )
);
run!(
    declared_variables_passed_on_through_recursive_union_bounds,
    r#"{
  type Q<'x, 'y> = [`A('x), `B('y)];
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  type L<'a> = [`Nil, `Cons('a, L<'a>)];
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  let f = 'a0: Array<G<'a2, 'a1>>, 'a1: H<'a2, 'a3>, 'a2: Array<G<'a4, 'a3>>, 'a3: H<'a4, 'a5>, 'a4: H<'a5, 'a6>, 'a5: Q<T<'a6>, L<'a6>>, 'a6: [i64, string] |x0: 'a0, x2: 'a2, x3: 'a3, x5: 'a5, x6: 'a6| (x0, x2, x3, x5, x6);
  let h = 'b0: Array<G<'b2, 'b1>>, 'b1: H<'b2, 'b3>, 'b2: Array<G<'b4, 'b3>>, 'b3: H<'b4, 'b5>, 'b4: H<'b5, 'b6>, 'b5: Q<T<'b6>, L<'b6>>, 'b6: [i64, string] |y0: 'b0, y2: 'b2, y3: 'b3, y5: 'b5, y6: 'b6| f(y0, y2, y3, y5, y6);
  let r = h([], [`Y(`X(`Y(`Y(`Z(`B(`Cons("s", `Nil))))))), `Y(`Z(`Y(`X(`B(`Cons("s", `Nil))))))], `Y(`X(`B(`Cons("s", `Nil)))), `B(`Cons("s", `Nil)), "s");
  (r.3, r.4)
}"#,
    prints(r#"[["B", ["Cons", "s", "Nil"]], "s"]"#)
);
run!(
    declared_variable_passed_on_stands_within_its_callees_bound_refused,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: 'a0, x2: 'a2, x3: 'a3| (x0, x2);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| f(y0, y2, y3);
  h(`B("s"), [`A(1)], "s")
}"#,
    refused("type mismatch Q<'b4, 'b3> does not contain `A(i64)");
    FuseExpect::None
);
run!(
    declared_variable_passed_on_stands_within_its_callees_bound,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: 'a0, x2: 'a2, x3: 'a3| (x0, x2);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| f(y0, y2, y3);
  h(`B("s"), [`A("t")], "s")
}"#,
    prints(r#"[["B", "s"], [["A", "t"]]]"#)
);
run!(
    declared_variable_passed_on_needs_its_callees_bound_declared,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: 'a0, x2: 'a2, x3: 'a3| (x0, x2);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: [i64, string] |y0: 'b0, y2: 'b2, y3: 'b3| f(y0, y2, y3);
  h(`B("s"), [`A("t")], "s")
}"#,
    refused("type mismatch Q<'a2, 'a1> does not contain 'b0");
    FuseExpect::None
);

// The same where the declared variables reach the callee through an inner
// function, a collection callback, or a literal that holds one.
run!(
    declared_variables_passed_on_through_an_inner_function_and_a_callback,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: 'a0, x2: 'a2, x3: 'a3| (x0, x2);
  let g = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: Array<'a0>, x2: 'a2, x3: 'a3| (x0, x2);
  let inner = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| { let k = |a| f(a, y2, y3); k(y0) };
  let slot = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| array::map([1], |i| f(y0, y2, y3));
  let held = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| g([y0], y2, y3);
  (inner(`B("s"), [], "s"), slot(`B("s"), [], "s"), held(`B("s"), [], "s"))
}"#,
    prints(r#"[[["B", "s"], []], [[["B", "s"], []]], [[["B", "s"]], []]]"#)
);

// A part of an argument that holds a declared variable and no undecided
// cell is not pre-unified with the formal's copy either: declared
// variables passed on beside callbacks, in a tuple, a struct and an array,
// stand within their callee's bounds as they do passed on bare.
run!(
    declared_variables_passed_on_beside_callbacks,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: ('a0, fn(v: 'a7) -> 'a7), x2: {k: fn(v: 'a8) -> 'a8, v: 'a2}, x3: Array<('a3, fn(v: 'a9) -> 'a9)>| (x0.0, x2.v);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| { let p = (y0, |v| v); f(p, {k: |v| v, v: y2}, [(y3, |v| v)]) };
  (f((`B("s"), |v| v), {k: |v| v, v: []}, [("s", |v| v)]), h(`B("s"), [], "s"), h(`A([`B(null)]), [`A("s")], null))
}"#,
    prints(
        r#"[[["B", "s"], []], [["B", "s"], []], [["A", [["B", null]]], [["A", "s"]]]]"#
    );
    FuseExpect::None
);
run!(
    declared_variables_passed_on_beside_callbacks_take_decided_arguments,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  let f = 'a0: Array<G<'a2, 'a1>>, 'a1: Array<G<'a3, 'a2>>, 'a2: P<'a3, 'a3>, 'a3: [i64, null] |x0: ('a0, fn(v: 'z0) -> 'z0), x1: ('a1, fn(v: 'z1) -> 'z1), x2: ('a2, fn(v: 'z2) -> 'z2), x3: ('a3, fn(v: 'z3) -> 'z3)| (x0.0, x1.0, x2.0, x3.0);
  let h = 'b0: Array<G<'b2, 'b1>>, 'b1: Array<G<'b3, 'b2>>, 'b2: P<'b3, 'b3>, 'b3: [i64, null] |y0: 'b0, y1: 'b1, y2: 'b2, y3: 'b3| f((y0, |v| v), (y1, |v| v), (y2, |v| v), (y3, |v| v));
  (f(([`X([null])], |v| v), ([`X(null)], |v| v), ([null], |v| v), (null, |v| v)), h([`X([null])], [`X(null)], [null], null))
}"#,
    prints(
        r#"[[[["X", [null]]], [["X", null]], [null], null], [[["X", [null]]], [["X", null]], [null], null]]"#
    );
    FuseExpect::None
);
run!(
    declared_variable_passed_on_beside_a_callback_stands_within_its_callees_bound_refused,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: ('a0, fn(v: 'a7) -> 'a7), x2: ('a2, fn(v: 'a8) -> 'a8), x3: ('a3, fn(v: 'a9) -> 'a9)| (x0.0, x2.0);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| f((y0, |v| v), (y2, |v| v), (y3, |v| v));
  h(`B("s"), [`A(1)], "s")
}"#,
    refused("type mismatch Q<'b4, 'b3> does not contain `A(i64)");
    FuseExpect::None
);
run!(
    declared_variable_passed_on_beside_a_callback_stands_within_its_callees_bound,
    r#"{
  type O<'t> = ['t, null];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a0: Q<'a2, 'a1>, 'a1: string, 'a2: Array<Q<'a4, 'a3>>, 'a3: O<'a4>, 'a4: string |x0: ('a0, fn(v: 'a7) -> 'a7), x2: ('a2, fn(v: 'a8) -> 'a8), x3: ('a3, fn(v: 'a9) -> 'a9)| (x0.0, x2.0);
  let h = 'b0: Q<'b2, 'b1>, 'b1: string, 'b2: Array<Q<'b4, 'b3>>, 'b3: O<'b4>, 'b4: string |y0: 'b0, y2: 'b2, y3: 'b3| f((y0, |v| v), (y2, |v| v), (y3, |v| v));
  h(`B("s"), [`A("t")], "s")
}"#,
    prints(r#"[["B", "s"], [["A", "t"]]]"#);
    FuseExpect::None
);

// A call over a declared chain of bounds whose first link is a union
// typedef with two members of one constructor, the second link a union
// that is recursive or not.
run!(
    bound_chain_through_a_union_of_two_arrays,
    r#"{
  type O<'t> = ['t, null];
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  type T2<'x> = [`Leaf('x), `Node(Array<'x>)];
  let f = 'a0: P<'a1, 'a2>, 'a1: T2<'a2>, 'a2: Array<'a4>, 'a4: [i64, string] |x0: 'a0, x1: 'a1, x4: 'a4| 7;
  let g = 'a0: P<'a1, 'a2>, 'a1: T<'a2>, 'a2: Array<'a4>, 'a4: [i64, string] |x0: 'a0, x1: 'a1, x4: 'a4| 8;
  let k = 'a0: O<P<'a1, 'a2>>, 'a1: T<'a2>, 'a2: P<'a4, 'a3>, 'a3: P<'a4, 'a4>, 'a4: [i64, string] |x0: 'a0, x1: 'a1, x3: 'a3, x4: 'a4| 9;
  (f([`Node([])], `Node([]), 1), g([`Node([])], `Node([]), 1), k([`Node([])], `Node([]), [], 1))
}"#,
    prints("[i64:7, i64:8, i64:9]")
);

// An undecided argument's cell that the join put inside its formal's
// variable is not made that variable: the two stay apart, and the
// argument is held as a member of what the variable settled to.
run!(
    undecided_argument_inside_what_its_formals_variable_settled_to,
    r#"{
  let h = |a: Array<'a>, b: ('a, i64)| array::len(a);
  let k = |a: Array<'a>, b: {v: 'a}| array::len(a);
  let f = |y| h([y, 1], (y, 2));
  let g = |y| k([y, 1, 2], {v: y});
  (f("s"), g("s"))
}"#,
    prints("[i64:2, i64:3]")
);

// An undecided argument takes its formal's variables as the arguments
// before it settled them, the ones only bounds hold included, so the
// instance is built at one type for each.
run!(
    seq_step_empty_array_into_a_bound_union_after_its_deciders,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a2, 'a1>, 'a1: P<'a3, 'a2>, 'a2: P<'a3, 'a3>, 'a3: [i64, string] |x2: 'a2, x1: 'a1, x0: 'a0| x0;
  let t = 1;
  seq t { let a = f(["s"], ["s"], []); a }
}"#,
    prints("[]")
);
// An undecided argument at a bound two members of which could take it is
// refused, and a typed one after the arguments that decide the members
// runs.
run!(seq_step_empty_array_into_a_bound_union_goes_to_both_members, r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a2, 'a1>, 'a1: P<'a3, 'a2>, 'a2: P<'a3, 'a3>, 'a3: [i64, string] |x0: 'a0, x1: 'a1, x2: 'a2| x0;
  let t = 1;
  seq t { let a = f([], null, ["s"]); a }
}"#, prints("[]"); FuseExpect::Jit);
run!(
    seq_step_typed_empty_array_into_a_bound_union_two_members_take,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  let f = 'a0: P<'a2, 'a1>, 'a1: P<'a3, 'a2>, 'a2: P<'a3, 'a3>, 'a3: [i64, string] |x2: 'a2, x1: 'a1, x0: 'a0| x0;
  let t = 1;
  let e: Array<null> = [];
  seq t { let a = f(["s"], null, e); a }
}"#,
    prints("[]")
);

// An instance's cell that no position holds is what its bounds settle it
// to, and one at a place holding ⊥ is ⊥, as the call's check left them.
// CR claude for claude: [test-gap/low] The comment's second rule, a cell at a place
// holding ⊥ is ⊥, has no pin. instance_cell_at_a_bottom_place_is_bottom was deleted,
// though its program (`'a0: [Array<P<'a1, 'a2>>, null], 'a1: R<'a2, 'a3>, 'a2: R<'a3,
// 'a3>, 'a3: i64 |x0: 'a0, x2: 'a2, x3: 'a3| x0` called `f(v0, never(), 1)` over `let
// v0 = []`) still checks and prints `[]`. design/tvar_constraints.md still cites it. It
// also cites
// empty_array_into_a_typedef_union_of_two_arrays_is_decided_by_the_other_parameters as
// a pin of the build agreeing with the check, but that program (`l(1, "s", [])`) is now
// refused and pinned as
// empty_array_into_a_typedef_union_of_two_arrays_after_its_deciders_refused. Restore
// the bottom-place pin and fix both citations. probe:
// design/review-2026-11-16/repro/tests-types-c-bottom-place-pin.gx (tests-types-c-06)
run!(
    instance_cells_only_bounds_hold_settle_to_their_bounds,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<'y>, null];
  type R<'x, 'y> = [Array<('x, 'y)>, null];
  let f = 'a0: R<'a1, 'a2>, 'a1: R<'a2, 'a3>, 'a2: P<'a4, 'a3>, 'a3: P<'a5, 'a4>, 'a4: P<'a5, 'a6>, 'a5: P<'a6, 'a6>, 'a6: [i64, string] |x2: 'a2, x0: 'a0| [x0];
  f(never(), [])
}"#,
    prints("[[]]")
);
// A cell only bounds hold settles to a bound naming a cell that has a
// place as that cell, which is then what stands at its place.
run!(
    instance_cell_only_bounds_hold_names_a_placed_cell,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, null], 'a2: ['a3, null], 'a3: ['a4, bool], 'a4: Number |x: ('a0, 'a2)| 7;
  let g = 'a0: ['a1, null], 'a1: ['a2, null], 'a2: ['a3, null], 'a3: Number |x: ('a0, 'a1, 'a2)| 8;
  f(never()) + g(never())
}"#,
    is(Value::I64(15));
    FuseExpect::None
);

// A cell is placed after every cell its bounds hold, however long the
// chain of bounds between them.
run!(
    instance_cells_of_a_chain_of_bounds_are_placed_deepest_first,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, string], 'a2: ['a3, bool], 'a3: ['a4, datetime], 'a4: ['a5, duration], 'a5: Number |x: ('a0, 'a1, 'a2, 'a3, 'a4)| 7;
  let g = 'a0: ['a1, null], 'a1: ['a2, string], 'a2: ['a3, bool], 'a3: ['a4, datetime], 'a4: ['a5, duration], 'a5: Number |x: ('a0, 'a1, 'a2, 'a3, 'a4)| x.0;
  let r = g(never());
  let h = 'a0: ['a1, null], 'a1: ['a2, string], 'a2: ['a3, bool], 'a3: ['a4, datetime], 'a4: ['a5, duration], 'a5: ['a6, bytes], 'a6: ['a7, null], 'a7: ['a8, string], 'a8: Number |x: ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)| 1;
  let s = h(never());
  f(never())
}"#,
    is(Value::I64(7));
    FuseExpect::Jit
);

// A cell only the members of unions hold takes what stands at its
// member's place, where the instance's union stands member for member
// beside the definition's: the ⊥ the call's check left it, which no
// member of either union would take by containment.
run!(
    instance_cell_only_union_members_hold_takes_its_members_place,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  type O<'t> = ['t, null];
  let f = |x: P<'a, string>, y: O<'a>| (x, y);
  let g = |u, v| f(u, v);
  g(["s"], null)
}"#,
    prints(r#"[["s"], null]"#)
);
// An instance's result is its call's own: the cells both leave open are
// one cell at each place, where the union's two undecided members could
// each take either.
run!(
    instance_result_holding_two_undecided_members_is_its_calls,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |k: fn(q: P<'b, 'a>) -> i64| k;
  let g = |t| f(never());
  let r = g(1);
  7
}"#,
    is(Value::I64(7));
    FuseExpect::None
);
run!(
    instance_result_holding_an_undecided_member_beside_a_decided_one_is_called,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |k: fn(q: P<'b, 'a>) -> i64, y: 'a| k;
  let g = |t| f(|q| 7, t);
  let r = g(1.5);
  r([1.5])
}"#,
    is(Value::I64(7));
    FuseExpect::None
);
run!(
    instance_cell_under_a_recursive_typedefs_members_takes_its_members_place,
    r#"{
  type G<'a> = [Error<G<['a, string]>>, Error<G<'a>>, `X('a)];
  type H<'a> = [Error<H<['a, string]>>, Error<H<'a>>, `X('a)];
  let f = |x: G<'b>| -> H<'b> x;
  let g = |y| f(y);
  let z = g(never());
  7
}"#,
    is(Value::I64(7))
);
run!(
    instance_cells_under_a_unions_member_take_a_typedefs_arguments,
    r#"{
  type R<'p, 'q, 'r> = [Array<'p>, Array<'q>, 'r];
  let f = |y: [(R<'a, 'c, 'b>, R<'a, 'b, 'c>), null], z: 'c| y;
  f((["s"], 1), 2)
}"#,
    prints(r#"[["s"], i64:1]"#)
);

// A function an instance defines takes its signature's cells from its
// definition's check place by place: a variable beside a written member
// of one constructor is not given that member's type.
run!(
    function_defined_in_an_instance_holds_a_variable_beside_a_written_member,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let o = |t| { let f = |x0: P<'a, string>, x1: 'a| (x0, x1); f(["s"], 1) };
  o(1)
}"#,
    prints(r#"[["s"], i64:1]"#)
);
run!(
    function_defined_in_an_instance_holds_a_variable_in_a_written_union,
    r#"{
  let o = |t| { let f = |x0: [Array<'a>, Array<string>], x1: ['a, null]| (x0, x1); f(["s"], 1) };
  o(1)
}"#,
    prints(r#"[["s"], i64:1]"#)
);
run!(
    function_defined_in_an_instance_holds_a_variable_no_argument_decides,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  type O<'t> = ['t, null];
  let o = |t| { let f = |x0: P<'a, string>, x1: O<'a>| (x0, x1); f(["s"], null) };
  o(1)
}"#,
    prints(r#"[["s"], null]"#)
);
run!(
    function_defined_in_an_instance_stays_generic_beside_the_outer_variable,
    r#"{
  let o = |t: 'z| { let f = |x0: [Array<'z>, Array<'a>], x1: 'a| (x0, x1); (f(["s"], "s"), f([t], t)) };
  o(1)
}"#,
    prints(r#"[[["s"], "s"], [[i64:1], i64:1]]"#)
);

// A union of function types a signature holds over its own variables is
// the function's: a read of the function through a field, a tuple
// position or a select of its copies keeps both members, as a read of
// its binding does, so the call's result is the union and the instance
// holds its definition's signature.
run!(
    field_read_keeps_a_signature_union_of_functions_over_its_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let s = {fld: |y: 'a, z: 'b, x: F2<'a, 'b>| (x, y, z)};
  let r = s.fld(1, "s", |q: i64| 0);
  (r.1, r.2)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);
run!(
    field_read_of_a_signature_union_of_functions_with_the_union_first,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let s = {fld: |x: F2<'a, 'b>, y: 'a, z: 'b| (x, y, z)};
  let j = s.fld;
  let r = j(|q: i64| 0, 1, "s");
  (r.1, r.2)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);
run!(
    tuple_read_keeps_a_signature_union_of_functions_over_its_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let s = (|y: 'a, z: 'b, x: F2<'a, 'b>| (x, y, z), 1);
  let r = s.0(1, "s", |q: i64| 0);
  (r.1, r.2)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);
run!(
    select_of_copies_keeps_a_signature_union_of_functions_over_its_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| (x, y, z);
  let s = {fld: f};
  let c = true;
  let g = select c { true => f, false => f };
  let t = select c { true => s, false => s };
  let (r, u) = (g(1, "s", |q: i64| 0), t.fld(2, "t", |q: string| 0));
  (r.1, r.2, u.1, u.2)
}"#,
    prints(r#"[i64:1, "s", i64:2, "t"]"#);
    FuseExpect::None
);
run!(
    field_read_result_of_a_signature_union_of_functions_is_no_one_function,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let s = {fld: |y: 'a, z: 'b, x: F2<'a, 'b>| x};
  let g = s.fld(1, "s", |q: i64| q + 1);
  g("s")
}"#,
    refused("type mismatch [fn(q: [i64, string]) -> i64, fn(q: string) -> i64] does not contain fn(q: i64) -> i64");
    FuseExpect::None
);

run!(
    a_select_of_a_union_of_functions_beside_null_keeps_both_members,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let r = f(u, v, w); let g = select c { true => r, false => null }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_select_of_two_calls_each_a_union_of_functions_keeps_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let g = select c { true => f(u, v, w), false => f(u, v, w) }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_select_of_two_calls_that_swap_the_variables_of_a_union_of_functions_pairs_each_with_itself,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let g = select c { true => f(u, v, w), false => f(v, u, w) }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_select_of_two_calls_over_a_union_of_functions_on_four_variables_places_each_call,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, s, t, w| { let g = select c { true => f(u, v, w), false => f(s, t, w) }; (u, v, s, t) };
  h(1, "s", "t", 2, |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s", "t", i64:2]"#)
);
run!(
    a_pairing_of_unions_of_functions_checks_whatever_the_arguments_of_the_call,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, s, t, w| { let g = select c { true => f(u, v, w), false => f(s, t, w) }; (u, v, s, t) };
  h(1, "s", 2, "t", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s", i64:2, "t"]"#)
);
run!(
    a_select_of_two_unions_of_functions_on_four_variables_places_each_call,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, s, t, w, w2| { let g = select c { true => f(u, v, w), false => f(s, t, w2) }; (u, v, s, t) };
  h(1, "s", "t", 2, |q: i64| q + 1, |q: i64| q + 2)
}"#,
    prints(r#"[i64:1, "s", "t", i64:2]"#)
);
run!(
    two_calls_with_one_union_of_functions_on_four_variables_pair_each_call,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, s, t, w| { f(u, v, w); f(s, t, w); (u, v, s, t) };
  h(1, "s", "t", 2, |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s", "t", i64:2]"#)
);
run!(
    a_select_of_two_calls_over_a_union_of_functions_on_four_decided_variables_passes,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u: i64, v: string, s: string, t: i64, w: F2<i64, string>| { let g = select c { true => f(u, v, w), false => f(s, t, w) }; (u, v, s, t) };
  h(1, "s", "t", 2, |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s", "t", i64:2]"#);
    FuseExpect::Jit
);
run!(
    a_field_over_two_shapes_each_a_union_of_functions_keeps_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let s = select c { true => {fld: f(u, v, w), a: 1}, false => {fld: f(u, v, w), b: 2} }; let g = s.fld; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_tuple_position_over_two_shapes_each_a_union_of_functions_keeps_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let r = f(u, v, w); let s = select c { true => (r, 1), false => (r, "s") }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_slice_element_of_a_union_of_functions_keeps_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let x = [f(u, v, w)]; let g = select x { [a, ..] => a, _ => never() }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_slice_over_an_array_joined_with_an_empty_one_keeps_the_variables_of_a_union_of_functions_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let g = select c { true => [f(u, v, w)], false => [] }; select g { [a, ..] => a, _ => never() }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_slice_rest_over_an_array_joined_with_an_empty_one_keeps_the_variables_of_a_union_of_functions_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let g = select c { true => [], false => [f(u, v, w)] }; select g { [a, b..] => (a, b), _ => never() }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    two_arms_naming_a_part_of_a_union_of_functions_keep_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let x = (f(u, v, w), 0); let g = select x { (a, 0) => a, (b, _) => b }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_bare_name_over_a_union_of_functions_keeps_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let r = f(u, v, w); let g = select r { k => k }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    a_seq_step_reading_a_union_of_functions_keeps_the_variables_two,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let x = f(u, v, w); seq { let g = x; (u, v) } };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    call_through_a_select_of_calls_each_a_union_of_functions,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let g = select c { true => f(u, v, w), false => f(u, v, w) }; g(u) };
  h(1, 1, |q: i64| q + 1)
}"#,
    prints("i64:2");
    FuseExpect::None
);

// Two signatures that are one type, with a typedef union of functions
// spelling the same two variables, meet in the union's members: where an
// earlier parameter has paired the variables the members pair one way
// only and the call runs, and where nothing has the members could pair
// either way and the call is refused, naming the annotation.
// CR claude for eric: [bug/medium] These tests pin verdicts that depend on how a
// typedef application spells its arguments, not on the union it names. F2<'d, 'c> and
// F2<'c, 'd> are one type, [fn(q: 'c) -> i64, fn(q: 'd) -> i64], yet app(f, 1, "s") is
// refused with one and runs with the other. G2 is F2 with its members listed in the
// other order, and it flips both verdicts. A select of f with a twin whose x2 is G2<'a,
// 'b> (the exact expansion of f's own F2<'b, 'a>) is refused, while the twin written
// F2<'b, 'a> runs. The comment above says a call runs once an earlier parameter has
// paired the variables, which the other-order test contradicts, and the Array analog is
// refused in every spelling. The acceptances come from Type::applications_hold relating
// two applications of one definition argument by argument. For a union whose members
// hold distinct parameters, that is a pairing picked by parameter position, the very
// choice AmbiguousMember refuses to make. Where it fails, the expansion runs over cells
// the attempt merged, so the refusal prints `fn(q: 'c: 'd: unbound) -> i64 could go to
// any of 2 members of the union fn(q: 'd: unbound) -> i64`. Decide the rule (pairing by
// an earlier parameter runs in every spelling, or every spelling is refused as for
// arrays), make applications_hold and the expansion agree with it, and pin a respelled
// twin of each verdict. probe:
// design/review-2026-11-16/repro/tests-types-c-union-member-order.sh (tests-types-c-01)
run!(
    union_of_functions_the_formals_spell_in_the_other_order_checks_after_a_param,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x0: 'b, x2: F2<'b, 'a>| 0;
  let app = |k: fn(x0: 'c, x2: F2<'d, 'c>) -> i64, v: 'c, w: 'd| 0;
  app(f, 1, "s")
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    union_of_functions_the_formals_spell_in_the_same_order_runs_after_a_param,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x0: 'b, x2: F2<'b, 'a>| 0;
  let app = |k: fn(x0: 'c, x2: F2<'c, 'd>) -> i64, v: 'c, w: 'd| 0;
  app(f, 1, "s")
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    union_of_functions_the_formals_spell_in_the_other_order_checks_before_a_param,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x2: F2<'b, 'a>, x0: 'b| 0;
  let app = |k: fn(x2: F2<'d, 'c>, x0: 'c) -> i64, v: 'c, w: 'd| 0;
  app(f, 1, "s")
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    union_of_functions_the_formals_spell_in_the_same_order_checks_before_a_param,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x2: F2<'b, 'a>, x0: 'b| 0;
  let app = |k: fn(x2: F2<'c, 'd>, x0: 'c) -> i64, v: 'c, w: 'd| 0;
  app(f, 1, "s")
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    select_of_two_spellings_of_a_union_of_functions_takes_a_callback_one_member_of_each_holds,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let c = true;
  let f = |x0: 'b, x2: F2<'b, 'a>| 0;
  let g = select c { true => f, false => |x0: 'b, x2: F2<'a, 'b>| 0 };
  g(1, |q: string| 0)
}"#,
    prints("i64:0");
    FuseExpect::None
);
run!(
    each_spelling_of_a_union_of_functions_alone_takes_the_call_the_select_of_both_refuses,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x0: 'b, x2: F2<'b, 'a>| 0;
  let h = |x0: 'b, x2: F2<'a, 'b>| 0;
  (f(1, |q: string| 0), h(1, |q: string| 0))
}"#,
    prints("[i64:0, i64:0]");
    FuseExpect::None
);

// One value joins nothing: a read of it through a field, a tuple
// position, an error's payload, a reference, a variant's pattern, a
// select of one binding in every arm or `any` over it alone returns its
// type as it stands, as a read of its binding does. A union of function
// types it holds over two undecided variables stays two members, and the
// variables two.
run!(
    field_read_of_one_value_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let s = {fld: f(u, v, w)}; (s.fld, u, v) };
  let r = h(1, "s", |q: i64| q + 1);
  (r.1, r.2)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);
run!(
    tuple_read_of_one_value_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let t = (f(u, v, w), u, v); let g = t.0; (t.1, t.2) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);
run!(
    error_payload_read_of_one_value_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let e = error(f(u, v, w)); let g = e.0; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    deref_of_one_value_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let x = f(u, v, w); let r = &x; let g = *r; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    variant_pattern_over_one_value_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let s = `A(f(u, v, w)); let g = select s { `A(g) => g }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    select_of_one_binding_in_every_arm_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let c = true;
  let h = |u, v, w| { let x = f(u, v, w); let g = select c { true => x, false => x }; (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    any_over_one_value_keeps_its_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let g = any(f(u, v, w)); (u, v) };
  h(1, "s", |q: i64| q + 1)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

// So a call through such a read is refused as the call of its binding
// is, and runs where one variable stands in both members.
run!(
    call_through_a_field_read_of_a_union_of_functions_over_two_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u, v, w| { let s = {fld: f(u, v, w)}; s.fld(u) };
  h(1, 2, |q: i64| q + 1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_a_field_read_of_a_union_of_functions_over_one_variable,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |y: 'a, z: 'b, x: F2<'a, 'b>| x;
  let h = |u: 'c, v: 'c, w: F2<'c, 'c>| { let s = {fld: f(u, v, w)}; s.fld(u) };
  h(1, 2, |q: i64| q + 1)
}"#,
    prints("i64:2");
    FuseExpect::None
);

// A read decides no variable either: a union one of whose members holds a
// variable no argument decided is two functions through a field or a
// tuple position, as it is through its binding.
run!(
    call_through_a_tuple_read_of_a_union_of_functions_no_argument_decided,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x0: 'a, x1: F2<'a, 'b>| (x0, x1);
  let r = f(1, |q: i64| q + 1);
  r.1(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_a_tuple_read_of_a_union_of_functions_its_arguments_decided,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x0: 'a, x1: F2<'a, 'a>| (x0, x1);
  let r = f(1, |q: i64| q + 1);
  r.1(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);

// A function that returns one of two untyped parameters returns the union
// of what they took, and the members that are one function type, the
// definitions each its own, are one function to call through the result
// of it, a reference to it, `any` over it, a select of it in every arm or
// an error's payload: it runs the definition the chooser took.
run!(
    call_through_a_deref_of_two_decided_functions,
    r#"{
  let c = false;
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(c, |x: i64| x, |x: i64| x + 1);
  let r = &p;
  (*r)(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_any_over_two_decided_functions,
    r#"{
  let c = false;
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(c, |x: i64| x, |x: i64| x + 1);
  let g = any(p);
  g(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_a_select_of_two_decided_functions_in_every_arm,
    r#"{
  let c = false;
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(c, |x: i64| x, |x: i64| x + 1);
  let g = select c { true => p, false => p };
  g(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_an_error_payload_of_two_decided_functions,
    r#"{
  let c = false;
  let pick = |c, a, b| select c { true => a, false => b };
  let e = error(pick(c, |x: i64| x, |x: i64| x + 1));
  e.0(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_one_function_twice,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let f = |x: i64| x + 1;
  let p = pick(false, f, f);
  p(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_runs_either_definition,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let f = |x: i64| x;
  let g = |x: i64| x + 100;
  let (u, v) = (pick(true, f, g), pick(false, f, g));
  (u(1), v(1), v(2))
}"#,
    prints("[i64:1, i64:101, i64:102]");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_unnamed_lambdas,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(false, |x| x + 1, |y| y + 2);
  (p(1), p(5))
}"#,
    prints("[i64:3, i64:7]");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_written_returns,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(false, |x: i64| -> i64 x, |x: i64| -> i64 x * 3);
  p(2)
}"#,
    prints("i64:6");
    FuseExpect::None
);
run!(
    call_through_any_chooser_of_two_decided_functions,
    r#"{
  let ch = |a, b| any(a, b);
  let p = ch(|x: i64| x + 1, |x: i64| x + 2);
  p(1)
}"#,
    prints("i64:2");
    FuseExpect::None
);
run!(
    call_through_an_array_chooser_of_two_decided_functions,
    r#"{
  let ch = |a, b| [a, b];
  let p = ch(|x: i64| x + 1, |x: i64| x + 2);
  (p[0]$(1), p[1]$(1))
}"#,
    prints("[i64:2, i64:3]");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_labeled_defaults,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let f = |#a: i64 = 1, x: i64| x + a;
  let g = |#a: i64 = 2, x: i64| x * a;
  let p = pick(false, f, g);
  (p(5), p(#a: 3, 5))
}"#,
    prints("[i64:10, i64:15]");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_two_signatures_refused,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(false, |x: i64| x, |x: string| x);
  p(1)
}"#,
    refused("type mismatch string does not contain i64");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_two_labels_refused,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(false, |#a: i64, x: i64| x + a, |#b: i64, x: i64| x);
  p(#a: 1, 1)
}"#,
    refused("no argument #a");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_a_default_and_a_required_label_passing_the_label,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let p = pick(false, |#a: i64 = 1, x: i64| x + a, |#a: i64, x: i64| x);
  p(#a: 1, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    call_through_a_chooser_s_result_of_one_generic_function_twice,
    r#"{
  let pick = |c, a, b| select c { true => a, false => b };
  let f = |x| x;
  let p = pick(false, f, f);
  p(1)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    call_through_reads_of_one_variable_s_two_functions,
    r#"{
  let c = true;
  let pick = |c, a: 'a, b: 'a| select c { true => a, false => b };
  let p = pick(c, |x: i64| x, |x: i64| x + 1);
  let r = &p;
  let g = any(*r);
  let e = error(select c { true => g, false => g });
  e.0(1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// So an argument both members of such a union could take is refused
// through a select of the function's copies as at a call of its binding,
// where one variable in both members takes it.
run!(
    select_of_copies_leaves_a_signature_union_of_functions_to_its_call,
    r#"{
  type K<'p, 'q> = [fn(q: i64) -> 'p, fn(q: i64) -> 'q];
  let f = |x: K<'a, 'b>| x;
  let c = true;
  let g = select c { true => f, false => f };
  let r = g(|q: i64| q);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);
run!(
    select_of_copies_of_a_signature_union_of_functions_over_one_variable,
    r#"{
  type K<'p, 'q> = [fn(q: i64) -> 'p, fn(q: i64) -> 'q];
  let f = |x: K<'a, 'a>| x;
  let c = true;
  let g = select c { true => f, false => f };
  let r = g(|q: i64| q);
  r(3)
}"#,
    prints("i64:3");
    FuseExpect::None
);

// Alternatives that are one function but for the names of its variables
// meet as that renaming, each part beside its own image, so a union the
// signature holds over two of its variables decides nothing where they
// meet: the call through the select is the call of the binding.
run!(
    select_of_copies_of_a_signature_union_under_one_constructor,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y);
  let s = {fld: f};
  let c = true;
  let g = select c { true => f, false => f };
  let t = select c { true => s, false => s };
  (g(1, ["s"]).1, t.fld("t", [2]).1)
}"#,
    prints(r#"[i64:1, "t"]"#);
    FuseExpect::None
);
run!(
    select_of_copies_of_a_signature_union_before_its_variables,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |x: (P<'a, 'b>, 'a, 'b)| x;
  let c = true;
  let g = select c { true => f, false => f };
  let r = g(([1], 1, "s"));
  (r.1, r.2)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    select_of_a_function_and_its_twin_of_a_signature_union_under_one_constructor,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y);
  let c = true;
  let g = select c { true => f, false => |y: 'a, x: P<'a, 'b>| (x, y) };
  let h = select c { true => |y: 'a, x: P<'a, 'b>| (x, y), false => f };
  (g(1, ["s"]).1, h("t", [2]).1)
}"#,
    prints(r#"[i64:1, "t"]"#);
    FuseExpect::None
);
run!(
    select_of_twins_in_composites_of_a_signature_union_under_one_constructor,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let c = true;
  let s = select c {
    true => {fld: |y: 'a, x: P<'a, 'b>| (x, y), n: 1},
    false => {fld: |y: 'a, x: P<'a, 'b>| (x, y), n: 2}
  };
  let t = select c {
    true => (|y: 'a, x: P<'a, 'b>| (x, y), 1),
    false => (|y: 'a, x: P<'a, 'b>| (x, y), "s")
  };
  (s.fld(1, ["s"]).1, t.0("t", [2]).1)
}"#,
    prints(r#"[i64:1, "t"]"#);
    FuseExpect::None
);
run!(
    select_of_twins_inside_a_function_of_a_signature_union_under_one_constructor,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let c = true;
  let h = |v, w| {
    let g = select c {
      true => |y: 'a, x: P<'a, 'b>| (x, y),
      false => |y: 'a, x: P<'a, 'b>| (x, y)
    };
    g(v, w)
  };
  h(1, ["s"]).1
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    array_of_copies_and_a_twin_of_a_signature_union_under_one_constructor,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y);
  let fs = [f, f, |y: 'a, x: P<'a, 'b>| (x, y)];
  fs[2]$(1, ["s"]).1
}"#,
    prints("i64:1");
    FuseExpect::None
);

// The twin's variables stay as many as the function's: a member of the
// union one variable decides leaves the other to the argument, and an
// argument both members could take is refused as at a call of the binding.
run!(
    select_of_a_twin_keeps_the_variables_of_a_signature_union_apart,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let f = |x0: 'b, x2: F2<'b, 'a>| x0;
  let c = true;
  let g = select c { true => f, false => |x0: 'b, x2: F2<'b, 'a>| x0 };
  g(1, |q: string| 0)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    select_of_a_twin_leaves_a_signature_union_of_functions_to_its_call,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  type FA<'p, 'q> = [fn(q: 'p) -> i64, Array<'q>];
  let f = |x0: FA<'b, 'a>, x1: F2<'a, 'b>| (x0, x1);
  let c = true;
  let g = select c { true => f, false => |x0: FA<'b, 'a>, x1: F2<'a, 'b>| (x0, x1) };
  let r = g([], |q: i64| 0);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

// The renaming is of cells, whatever the variables are called: a twin
// with the names swapped or with others, a select of selects, an
// or-pattern's name and twins whose bodies give a variable one bound.
run!(
    select_of_a_twin_under_other_names_of_a_signature_union,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y);
  let c = false;
  let g = select c { true => f, false => |y: 'b, x: P<'b, 'a>| (x, y) };
  let h = select c { true => |y: 'm, x: P<'m, 'n>| (x, y), false => f };
  let k = select c { true => select c { true => f, false => f }, false => f };
  (g(1, ["s"]).1, h("t", [2]).1, k(2, ["u"]).1)
}"#,
    prints(r#"[i64:1, "t", i64:2]"#);
    FuseExpect::None
);
run!(
    or_name_of_a_function_and_its_twin_of_a_signature_union,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y);
  let c = false;
  let t = select c { true => `A(f), false => `B(|y: 'a, x: P<'a, 'b>| (x, y)) };
  select t { `A(k) | `B(k) => k(1, ["s"]).1 }
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    select_of_bounded_twins_of_a_signature_union,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = 'a: Number |y: 'a, x: P<'a, 'b>| (x, y + y);
  let c = false;
  let g = select c { true => f, false => 'a: Number |y: 'a, x: P<'a, 'b>| (x, y + y + y) };
  g(1, ["s"]).1
}"#,
    prints("i64:3");
    FuseExpect::None
);
run!(
    select_of_bounded_twins_of_a_signature_union_holds_the_bound,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = 'a: Number |y: 'a, x: P<'a, 'b>| (x, y + y);
  let c = false;
  let g = select c { true => f, false => 'a: Number |y: 'a, x: P<'a, 'b>| (x, y + y + y) };
  g("t", ["s"]).1
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);
run!(
    select_of_twins_of_a_signature_union_one_body_bounds_a_variable,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y + y);
  let c = false;
  let g = select c { true => f, false => |y: 'a, x: P<'a, 'b>| (x, y) };
  (g(1, ["s"]).1, f(2.5, [1]).1)
}"#,
    prints("[i64:1, f64:5.]");
    FuseExpect::Jit
);
run!(
    select_of_twins_of_a_signature_union_one_body_bounds_a_variable_holds_the_bound,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = |y: 'a, x: P<'a, 'b>| (x, y + y);
  let c = false;
  let g = select c { true => f, false => |y: 'a, x: P<'a, 'b>| (x, y) };
  g("t", ["s"]).1
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);
run!(
    select_of_twins_of_a_signature_union_declared_with_two_bounds,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = 'a: Number |y: 'a, x: P<'a, 'b>| (x, y + y);
  let c = false;
  let g = select c { true => f, false => 'a: Ordered |y: 'a, x: P<'a, 'b>| (x, y) };
  g(1, ["s"]).1
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    select_of_twins_of_a_signature_union_declared_with_apart_bounds_is_refused_at_the_call,
    r#"{
  type P<'p, 'q> = [Array<'p>, Array<'q>];
  let f = 'a: Int |y: 'a, x: P<'a, 'b>| (x, y + y);
  let c = false;
  let g = select c { true => f, false => 'a: Float |y: 'a, x: P<'a, 'b>| (x, y) };
  g(1, ["s"]).1
}"#,
    refused("type mismatch Float does not contain i64");
    FuseExpect::None
);

// A function a call returned is one instance, its variables open cells
// its calls decide: read through a field it keeps its signature's union
// as well, so another argument decides the member, and an argument both
// members could take is refused as at a call of the instance itself.
run!(
    field_read_of_an_instance_keeps_a_signature_union_of_functions_over_its_variables,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let mk = |z| {fld: |y: 'a, z: 'b, x: F2<'a, 'b>| (x, y, z)};
  let s = mk(0);
  let r = s.fld(1, "s", |q: i64| 0);
  (r.1, r.2)
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);
run!(
    field_read_of_an_instance_leaves_a_signature_union_of_functions_to_its_call_refused,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let mk = |z| {fld: |x: F2<'a, 'b>| x};
  let s = mk(0);
  let r = s.fld(|q: string| 1);
  0
}"#,
    refused("fn(q: 'b) -> i64 could go to more than one member of the union [fn(q: 'a) -> i64, fn(q: 'b) -> i64], and nothing has decided which");
    FuseExpect::None
);
run!(
    field_read_of_an_instance_of_a_signature_union_of_functions_over_one_variable,
    r#"{
  type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64];
  let mk = |z| {fld: |x: F2<'a, 'a>| x};
  let s = mk(0);
  let r = s.fld(|q: string| 1);
  r("s")
}"#,
    prints("i64:1");
    FuseExpect::None
);

// An alias that adds a member, applied to a variable under an invariant
// constructor: an argument written as the alias over a type that already
// holds the member gives the variable that type, and the instance holds
// its signature there.
run!(
    alias_over_a_variable_under_a_writable_reference_builds_at_a_nested_alias,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |w: &mut Opt<'a>| -> &mut 'a never();
  let put = |w: &mut Opt<'a>, v: 'a| -> 'a { *w <- v; v };
  let n0: Opt<Opt<i64>> = null;
  let x: &mut Opt<i64> = nest(&mut n0);
  let d: Opt<i64> = 5;
  let p: Opt<i64> = put(&mut n0, d);
  p
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

run!(
    variable_beside_a_member_under_a_writable_reference_takes_what_the_result_needs,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |w: &mut Opt<'a>| -> &mut 'a never();
  let n0: Opt<i64> = null;
  let x: &mut Opt<i64> = nest(&mut n0);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    variable_beside_a_member_under_a_writable_reference_returned_at_another_type_refused,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |w: &mut Opt<'a>| -> &mut 'a never();
  let n0: Opt<i64> = null;
  let x: &mut Opt<string> = nest(&mut n0);
  0
}"#,
    refused("Opt<string> does not contain i64");
    FuseExpect::None
);
run!(
    variable_beside_a_member_under_a_writable_reference_takes_what_the_argument_leaves_it_twin,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |w: &mut Opt<'a>| -> &mut 'a never();
  let n0: Opt<i64> = null;
  let x: &mut i64 = nest(&mut n0);
  0
}"#,
    is(Value::I64(0));
    FuseExpect::None
);
run!(
    variable_beside_a_member_under_a_writable_reference_after_the_variable_takes_the_referent,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |w: &mut Opt<'a>, k: &mut 'a| -> i64 0;
  let n0: Opt<i64> = null;
  let d: Opt<i64> = 1;
  nest(&mut n0, &mut d)
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    variable_beside_a_member_under_a_writable_reference_after_a_variable_of_another_type_refused,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |w: &mut Opt<'a>, k: &mut 'a| -> i64 0;
  let n0: Opt<i64> = null;
  let d: Opt<string> = "s";
  nest(&mut n0, &mut d)
}"#,
    refused("[i64, null] does not contain Opt<string>");
    FuseExpect::None
);
run!(
    variable_beside_a_member_under_a_writable_reference_before_the_variable_twin,
    r#"{
  type Opt<'a> = ['a, null];
  let nest = |k: &mut 'a, w: &mut Opt<'a>| -> i64 0;
  let n0: Opt<i64> = null;
  let d: Opt<i64> = 1;
  nest(&mut d, &mut n0)
}"#,
    is(Value::I64(0));
    FuseExpect::None
);
run!(
    alias_over_a_variable_under_an_abstract_builds_at_a_nested_alias,
    r#"{
  type Opt<'a> = ['a, null];
  type B<'a> = Abstract<'a>;
  let nest = |w: B<Opt<'a>>| -> B<'a> never();
  let inner = |w: B<Opt<'a>>, d: 'a| -> B<'a> B(d);
  let n0: Opt<i64> = null;
  let y: B<Opt<Opt<i64>>> = B(n0);
  let x: B<Opt<i64>> = nest(y);
  let z: B<Opt<i64>> = inner(y, 3);
  z.0
}"#,
    is(Value::I64(3));
    FuseExpect::Jit
);

// A callback whose formal is a variable bounded by a union of function
// types is given to no member before its result is known, and takes its
// parameter's type from the member that then admits it.
run!(
    callback_into_the_second_member_of_a_union_bound,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a2: E<'a3, 'a3>, 'a3: Number |x: 'a2, y: 'a3| x;
  let h = f(|v| "s", 1);
  h(2)
}"#,
    prints(r#""s""#);
    FuseExpect::None
);
run!(
    callback_into_a_union_bound_takes_the_members_parameter,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a0: E<'a1, 'a2>, 'a1: E<'a2, 'a3>, 'a2: E<'a3, 'a3>, 'a3: Number |z: ['a3, null], x1: 'a2, x2: 'a0, x3: 'a3| 7;
  f(1, |v| 1, |v| "s", 1)
}"#,
    prints("i64:7")
);

// A variable that does not widen (a function over it holds it outside
// data), bounded by a union of function types, is bound to a callback by
// the callback's own check: the members' parameters are the callback's
// once its result tells them apart, and a callback that does not is
// refused for the annotation.
run!(
    callback_into_a_union_bound_under_a_function_takes_the_members_parameter,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a2: E<'a3, 'a3> |z: 'a3, x1: 'a2, k: fn(q: 'a2) -> i64| 7;
  f(1, |v| 1, |q| 0)
}"#,
    prints("i64:7")
);
run!(
    callback_into_the_second_member_of_a_union_bound_under_a_function,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a2: E<'a3, 'a3> |z: 'a3, x1: 'a2, k: fn(q: 'a2) -> i64| 7;
  f(1, |v| "s", |q| 0)
}"#,
    prints("i64:7")
);
run!(
    callback_of_no_result_into_a_union_bound_under_a_function_refused,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a2: E<'a3, 'a3> |z: 'a3, x1: 'a2, k: fn(q: 'a2) -> i64| 7;
  f(1, |v| never(), |q| 0)
}"#,
    refused("type mismatch [fn(v: i64) -> i64, fn(v: i64) -> string] does not contain fn(v: _) -> _");
    FuseExpect::None
);
run!(
    callback_of_a_written_result_into_a_union_bound_under_a_function,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a2: E<'a3, 'a3> |z: 'a3, x1: 'a2, k: fn(q: 'a2) -> i64| 7;
  f(1, |v| -> i64 never(), |q| 0)
}"#,
    prints("i64:7")
);

// A variable a bound holds under a function, exactly or by reference does
// not widen to an argument wider than the one that decided it: the bound
// no longer holds what the variable took, as the function type written in
// the parameter does not.
const HELD_BY_A_BOUND: &str = "does not contain [i64, string]";

run!(
    a_variable_held_by_a_bound_under_a_function_does_not_widen_refused,
    r#"{
  let f = 'a: fn(q: 'c) -> i64 |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(|q: string| 0, n)
}"#,
    refused(HELD_BY_A_BOUND);
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_under_a_function_does_not_widen_annotated,
    r#"{
  let f = 'a: fn(q: 'c) -> i64 |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(|q: [i64, string]| 0, n)
}"#,
    prints("i64:1")
);
run!(
    a_variable_held_by_a_bound_under_a_mutable_reference_widens_the_referent,
    r#"{
  let f = 'a: &mut 'c |v: 'a, c: 'c| c; let n: [i64, string] = 1; let s = "s"; f(&mut s, n)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_under_a_mutable_reference_does_not_widen_annotated,
    r#"{
  let f = 'a: &mut 'c |v: 'a, c: 'c| c; let n: [i64, string] = 1; let s: [i64, string] = 2; f(&mut s, n)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_union_member_under_a_function_does_not_widen_refused,
    r#"{
  let f = 'a: [fn(q: 'c) -> i64, 'b] |v: 'a, y: 'b, c: 'c| c; let n: [i64, string] = 1; f(|q: string| 0, "s", n)
}"#,
    refused("type mismatch [fn(q: [i64, string]) -> i64, string] does not contain fn(q: string) -> i64");
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_union_member_under_a_function_does_not_widen_annotated,
    r#"{
  let f = 'a: [fn(q: 'c) -> i64, 'b] |v: 'a, y: 'b, c: 'c| c; let n: [i64, string] = 1; f(|q: [i64, string]| 0, "s", n)
}"#,
    prints("i64:1")
);
run!(
    a_variable_held_by_a_bound_of_a_bound_does_not_widen_refused,
    r#"{
  let f = 'a: Array<'d>, 'd: fn(q: 'c) -> i64 |v: 'a, c: 'c| c; let n: [i64, string] = 1; f([|q: string| 0], n)
}"#,
    refused(HELD_BY_A_BOUND);
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_of_a_bound_does_not_widen_annotated,
    r#"{
  let f = 'a: Array<'d>, 'd: fn(q: 'c) -> i64 |v: 'a, c: 'c| c; let n: [i64, string] = 1; f([|q: [i64, string]| 0], n)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    a_variable_held_by_a_recursive_typedef_bound_does_not_widen_refused,
    r#"{
  type T<'x> = [`Leaf(fn(q: 'x) -> i64), `Node(Array<T<'x>>)]; let f = 'a: T<'c> |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(`Leaf(|q: string| 0), n)
}"#,
    refused(HELD_BY_A_BOUND);
    FuseExpect::None
);
run!(
    a_variable_held_by_a_recursive_typedef_bound_does_not_widen_annotated,
    r#"{
  type T<'x> = [`Leaf(fn(q: 'x) -> i64), `Node(Array<T<'x>>)]; let f = 'a: T<'c> |v: 'a, c: 'c| c; let n: [i64, string] = 1; f(`Leaf(|q: [i64, string]| 0), n)
}"#,
    prints("i64:1");
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_does_not_widen_to_an_omitted_default_refused,
    r#"{
  let n: [i64, string] = 1; let f = 'a: fn(q: 'c) -> i64 |#c: 'c = n, v: 'a| c; f(|q: string| 0)
}"#,
    refused(HELD_BY_A_BOUND);
    FuseExpect::None
);
run!(
    a_variable_held_by_a_bound_does_not_widen_to_an_omitted_default_annotated,
    r#"{
  let n: [i64, string] = 1; let f = 'a: fn(q: 'c) -> i64 |#c: 'c = n, v: 'a| c; f(|q: [i64, string]| 0)
}"#,
    prints("i64:1")
);
run!(
    a_variable_held_by_a_bound_in_data_alone_still_widens,
    r#"{
  let f = 'a: Array<'c> |v: 'a, c: 'c| c;
  let n: [i64, string] = 1;
  f(["s"], n)
}"#,
    prints("i64:1")
);

// A formal that reaches a variable an earlier argument settled only
// through another variable's bound leaves it settled: the copy the
// argument pre-unifies with opens its own cells alone.
run!(
    callback_into_a_union_bound_beside_a_function_over_the_bounded_variable,
    r#"{
  type E<'x, 'y> = [fn(v: 'x) -> i64, fn(v: 'y) -> string];
  let f = 'a1: E<'a2, 'a3>, 'a3: E<'a4, 'a4> |x0: 'a3, x1: 'a1, z: fn(q: 'a1) -> i64, x4: 'a4| 7;
  f(|v| 1, |v| "s", |q| 1, 1)
}"#,
    prints("i64:7")
);
run!(
    variable_settled_before_a_function_over_its_holder_takes_another_type,
    r#"{
  let f = 'a1: Array<'a3> |x0: 'a3, z: fn(q: 'a1) -> i64, w: 'a3| (x0, w);
  f(1, |q| 1, "s")
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);
run!(
    variable_settled_before_a_function_over_its_holder_takes_its_own_type,
    r#"{
  let f = 'a1: Array<'a3> |x0: 'a3, z: fn(q: 'a1) -> i64, w: 'a3| (x0, w);
  let (a, b) = f(1, |q| array::len(q), 2);
  a + b
}"#,
    prints("i64:3")
);

// A declared variable going into a union is judged over the union's
// members as a value's type is: a member a name stands for is its members
// there too, so the check refuses what the instance at a type would.
run!(
    declared_variable_into_a_union_with_a_named_member,
    r#"{
  type Id<'x> = 'x;
  let f = |x: ['a, Id<'b>]| -> i64 0;
  let g = |v: 'r| f(v);
  g(1)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);
run!(
    declared_variable_into_a_union_with_a_named_member_of_one_variable,
    r#"{
  type Id<'x> = 'x;
  let f = |x: ['a, Id<'a>]| -> i64 0;
  let g = |v: 'r| f(v);
  g(1)
}"#,
    prints("i64:0")
);

// Every position is held, whichever round checks it: a result whose
// variable the parameter's binding closes is checked all the same.
run!(
    function_type_every_position_held,
    r#"{
  let f = |x: 'a| -> 'a x;
  let g: fn(x: bool) -> i64 = f;
  g(true) + 1
}"#,
    refused("type mismatch i64 does not contain bool");
    FuseExpect::None
);
run!(
    function_type_every_position_held_in_a_union,
    r#"{
  let f = |x: 'a| -> 'a x;
  let g: fn(x: bool) -> [i64, string] = f;
  g(true)
}"#,
    refused("type mismatch [i64, string] does not contain bool");
    FuseExpect::None
);
run!(
    function_type_every_position_held_in_a_field,
    r#"{
  let f = |x: 'a, y: 'a, z: 'a| -> 'a x;
  let s: {k: fn(x: bool, y: i64, z: string) -> [i64, string]} = {k: f};
  s.k(true, 1, "s")
}"#,
    refused("type mismatch [i64, string] does not contain bool");
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
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    written_in_a_lambda_is_monomorphic,
    r#"{ let f = |x| x; let a = f("s"); let set = |g| f <- g;
         set(|x: i64| x + 1); a }"#,
    refused("type mismatch i64 does not contain string");
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

// A block's value reads the block's own names: one a nearer `let` binds to
// a value is no generic name around the block.
run!(
    a_blocks_value_reads_its_own_binding_not_a_generic_one_around_it,
    r#"{ let f = |x| x; let a = { let f = 0; f }; a(5) }"#,
    refused_renumbered("fn(i64) -> '_0: unbound does not contain i64");
    FuseExpect::None
);

// What a block's statement binds is made once, and every instance of the
// definition the block ends in shares it: a write through one is a write
// to all.
run!(
    a_blocks_binding_a_returned_closure_writes_is_one_binding,
    r#"{
    let h = { let y0 = []; let g0 = |x| { y0 <- [x]; x }; |u| (u, y0, g0) };
    let w: (i64, Array<string>, i64) = (0, h(0).1, 1);
    let a = h(0).2(5);
    0
}"#,
    refused("string does not contain i64");
    FuseExpect::None
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
    let g = "let s = { let g = select true { true => f, false => f }; g(\"s\") }";
    let _s = ctx.rt.compile(ArcStr::from(g)).await?;
    let narrow = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 1")).await.unwrap_err();
    let narrow = format!("{narrow:#}");
    assert!(narrow.contains("what is written to it must be as general"), "{narrow}");
    assert!(cells_print_short(&narrow), "{narrow}");
    let _w = ctx.rt.compile(ArcStr::from("f <- |x| x")).await?;
    let r = ctx.rt.compile(ArcStr::from("let r = &mut f")).await.unwrap_err();
    assert!(format!("{r:#}").contains("f is generic, and a writable reference to it"));
    ctx.shutdown().await;
    Ok(())
}

/// A variable standing only in a quantifier's bound is held rigid
/// against a later write as the others are.
#[tokio::test(flavor = "current_thread")]
async fn written_after_generalized_holds_a_bound_variable() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let f = "let f = 'b: ['a, null] |x: 'b| -> i64 0";
    let _f = ctx.rt.compile(ArcStr::from(f)).await?;
    let w = "f <- |x: [i64, null]| -> i64 x$ + 1";
    let narrow = ctx.rt.compile(ArcStr::from(w)).await.unwrap_err();
    let narrow = format!("{narrow:#}");
    assert!(narrow.contains("what is written to it must be as general"), "{narrow}");
    let _w = ctx.rt.compile(ArcStr::from("f <- |x| 0")).await?;
    let _b = ctx.rt.compile(ArcStr::from("let b = (f(1), f(\"s\"))")).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A held variable bounded from below (an abstract type's parameter its
/// constructor's payload bounds) holds what its lower bounds hold: a write
/// of the binding's own shape compiles, a narrower one is refused.
#[tokio::test(flavor = "current_thread")]
async fn written_after_generalized_holds_a_lower_bound() -> Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    let (tx, _rx) = tokio::sync::mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    let mut held = Vec::new();
    for i in [
        "type B<'p> = Abstract<'p>",
        "let f = |x| B(x)",
        "let g = |x| (x, B(3))",
        "f <- |x| B(x)",
        "g <- |x| (x, B(4))",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    for w in ["f <- |x: i64| B(x)", "g <- |x| (x, B(\"s\"))"] {
        let e = ctx.rt.compile(ArcStr::from(w)).await.unwrap_err();
        let e = format!("{e:#}");
        assert!(e.contains("what is written to it must be as general"), "{w}: {e}");
    }
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
        "let s = mod d dynamic { sig { use super::f; val v: i64 }; source src }",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, v.exprs[0].id).await?, Value::I64(2));
    let e = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await.unwrap_err();
    assert!(format!("{e:#}").contains("super::f(1)"), "{e:#}");
    let _w = ctx.rt.compile(ArcStr::from("src <- \"let v = 5\"")).await?;
    ctx.rt.wait_settled().await?;
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, v.exprs[0].id).await?, Value::I64(5));
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
        "let s = mod d dynamic { sig { use super::f; val v: i64 }; source src }",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, v.exprs[0].id).await?, Value::I64(2));
    let bad = "src <- \"super::f <- |x: i64| x + 100; let v = nope\"";
    let _w = ctx.rt.compile(ArcStr::from(bad)).await?;
    ctx.rt.wait_settled().await?;
    let e = ctx.rt.compile(ArcStr::from("f <- |x: i64| x + 100")).await.unwrap_err();
    assert!(format!("{e:#}").contains("super::f(1)"), "{e:#}");
    let good = "src <- \"super::f <- |x: i64| x + 100; let v = 7\"";
    let _w = ctx.rt.compile(ArcStr::from(good)).await?;
    ctx.rt.wait_settled().await?;
    let st = ctx.rt.compile(ArcStr::from("s")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, st.exprs[0].id).await?, Value::Null);
    let v = ctx.rt.compile(ArcStr::from("d::v")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, v.exprs[0].id).await?, Value::I64(7));
    let r = ctx.rt.compile(ArcStr::from("f(1)")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, r.exprs[0].id).await?, Value::I64(101));
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
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, v.exprs[0].id).await?, Value::I64(101));
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
        "let s = mod d dynamic { sig { val v: i64 }; source src }",
        "let w = d::v",
    ] {
        held.push(ctx.rt.compile(ArcStr::from(i)).await?);
    }
    let tens = ctx.rt.compile(ArcStr::from("d::v * 10")).await?;
    assert_eq!(
        next_update_raw(&ctx.rt, &mut rx, tens.exprs[0].id).await?,
        Value::I64(10)
    );
    let _w = ctx.rt.compile(ArcStr::from("src <- \"let v = 5\"")).await?;
    assert_eq!(
        next_update_raw(&ctx.rt, &mut rx, tens.exprs[0].id).await?,
        Value::I64(50)
    );
    ctx.rt.wait_settled().await?;
    let w = ctx.rt.compile(ArcStr::from("w")).await?;
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, w.exprs[0].id).await?, Value::I64(5));
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
    assert_eq!(
        next_update_raw(&ctx.rt, &mut rx, tens.exprs[0].id).await?,
        Value::I64(10)
    );
    let _w = ctx.rt.compile(ArcStr::from("a::x <- 10")).await?;
    assert_eq!(
        next_update_raw(&ctx.rt, &mut rx, tens.exprs[0].id).await?,
        Value::I64(110)
    );
    ctx.rt.wait_settled().await?;
    let wr = ctx.rt.compile(ArcStr::from("\"[w] [r]\"")).await?;
    assert_eq!(
        next_update_raw(&ctx.rt, &mut rx, wr.exprs[0].id).await?,
        Value::String("11 11".into())
    );
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
            next_update_raw(&ctx.rt, &mut rx, rr.exprs[0].id).await?,
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
    assert_eq!(next_update_raw(&ctx.rt, &mut rx, v.exprs[0].id).await?, Value::I64(101));
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
    refused("type mismatch i64 does not contain string");
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
    refused("type mismatch i64 does not contain string");
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

// CR claude for claude: [test-gap/low] refused("does not contain") holds whether the
// select keeps [a] and [b] apart or makes them one array. Today the refusal comes from
// the index over the union, `Array<string> does not contain [Array<string>,
// Array<i64>]`; with a and b as one cell, the same call is refused `string does not
// contain i64`, which matches too. A change that joined the arms' element cells would
// pass this test. Pin the claim directly: `(pick(true, "x", 41), pick(false, "x", 41))`
// checks and prints `[["x"], [i64:41]]`, which no merge allows. probe:
// design/review-2026-11-16/repro/tests-types-c-union-keeps-distinct-cells-vacuous.sh
// (tests-types-c-04)
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

// A union bound passes its parts to the one member the value may be: a
// struct of its field set, a read-only reference's read-only member (a
// writable member holds no read-only reference).
run!(union_bound_struct_member_reaches_open_cell, r#"{
    let f = 'a: [{x: [i64, null]}, {y: string}] |v: 'a| v;
    let g = |z| f({x: z});
    g("hello")
}"#, refused("[i64, null] does not contain string"); FuseExpect::None);

run!(
    union_bound_struct_member_accepts,
    r#"{
    let f = 'a: [{x: [i64, null]}, {y: string}] |v: 'a| v;
    let g = |z| f({x: z});
    g(i64:5).x
}"#,
    is(Value::I64(5))
);

run!(union_bound_reference_member_reaches_open_cell, r#"{
    let f = 'a: [&[i64, null], &mut string] |v: 'a| v;
    let g = |z| f(&z);
    *g("hello")
}"#, refused("[&[i64, null], &mut string] does not contain &string"); FuseExpect::None);

run!(
    union_bound_reference_member_accepts,
    r#"{
    let f = 'a: [&[i64, null], &mut string] |v: 'a| v;
    let g = |z| f(&z);
    *g(i64:5)
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

// A read-only member takes a writable reference, and decides its referent
// where it is the one member that takes it.
run!(union_bound_read_only_member_decides_writable_reference, r#"{
    let f = 'a: [&i64, null] |v: 'a| v;
    let g = |z| f(&mut z);
    *g("hello")
}"#, refused("i64 does not contain string"); FuseExpect::None);

run!(
    union_bound_read_only_member_takes_writable_reference,
    r#"{
    let f = 'a: [&i64, null] |v: 'a| v;
    let g = |z| f(&mut z);
    *g(i64:5)
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

// Two members of a union bound take a value's constructor: one that
// holds a cell to decide is refused where both could hold it, naming what
// to annotate, since which does follows what decides the cell; a decided
// one goes to the member that holds it.
run!(union_bound_writable_reference_goes_to_the_member_the_call_decides, r#"{
    let f = 'a: [&[i64, null], &mut string] |v: 'a| v;
    let g = |z| f(&mut z);
    *g("hello")
}"#, prints(r#""hello""#); FuseExpect::None);

run!(
    union_bound_decided_writable_reference_takes_writable_member,
    r#"{
    let f = 'a: [&[i64, null], &mut string] |v: 'a| v;
    let g = |z: string| f(&mut z);
    *g("hello")
}"#,
    is(Value::String("hello".into()));
    FuseExpect::None
);

run!(
    union_bound_decided_writable_reference_takes_read_only_member,
    r#"{
    let f = 'a: [&[i64, null], &mut string] |v: 'a| v;
    let g = |z: i64| f(&mut z);
    *g(5)
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

run!(union_bound_two_array_members_refuse_an_undecided_element, r#"{
    let f = 'a: [Array<i64>, Array<string>] |v: 'a| v;
    let z = never();
    let r = f([z]);
    z <- 2.5;
    r
}"#, refused("type mismatch [Array<i64>, Array<string>] does not contain Array<f64>"); FuseExpect::None);

run!(
    union_bound_two_array_members_take_a_decided_element,
    r#"{
    let f = 'a: [Array<i64>, Array<string>] |v: 'a| v;
    let z: i64 = never();
    let r = f([z]);
    z <- 7;
    r[0]$
}"#,
    is(Value::I64(7))
);

run!(union_bound_two_array_members_refuse_an_empty_array, r#"{
    let f = 'a: [Array<i64>, Array<string>] |v: 'a| v;
    let r: Array<f64> = f([]);
    array::len(r)
}"#, refused("type mismatch [Array<i64>, Array<string>] does not contain Array<f64>"); FuseExpect::None);

run!(
    union_bound_two_array_members_take_a_typed_empty_array,
    r#"{
    let f = 'a: [Array<i64>, Array<string>] |v: 'a| v;
    let e: Array<string> = [];
    array::len(f(e))
}"#,
    is(Value::I64(0))
);

run!(union_bound_two_writable_members_refuse_an_undecided_referent, r#"{
    let f = 'a: [&mut [i64, null], &mut string] |v: 'a| v;
    let z = never();
    let r = f(&mut z);
    z <- 2.5;
    *r
}"#, refused("type mismatch [&mut [i64, null], &mut string] does not contain &mut f64"); FuseExpect::None);

run!(
    union_bound_two_writable_members_take_a_decided_referent,
    r#"{
    let f = 'a: [&mut [i64, null], &mut string] |v: 'a| v;
    let z: [i64, null] = never();
    let r = f(&mut z);
    z <- 5;
    *r
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

run!(union_bound_two_function_members_refuse_an_undecided_lambda, r#"{
    let f = 'a: [fn(x: i64) -> i64, fn(x: string) -> string] |v: 'a| v;
    let h = f(|v| v);
    h(2.5)
}"#, refused("type mismatch [fn(x: i64) -> i64, fn(x: string) -> string] does not contain fn(v: f64) -> f64"); FuseExpect::None);

run!(
    union_bound_two_function_members_take_a_typed_lambda,
    r#"{
    let f = 'a: [fn(x: i64) -> i64, fn(x: string) -> string] |v: 'a| v;
    let h = f(|v: i64| v);
    h(2)
}"#,
    is(Value::I64(2));
    FuseExpect::None
);

run!(union_bound_through_a_typedef_refuses_an_undecided_array, r#"{
    type P<'x, 'y> = [Array<'x>, Array<'y>, null];
    let f = 'a0: P<'a2, 'a1>, 'a1: P<'a3, 'a2>, 'a2: P<'a3, 'a3>, 'a3: [i64, string] |x0: 'a0, x1: 'a1, x2: 'a2| x0;
    let v = never();
    let s = array::len(v);
    let r = f(v, null, [1]);
    v <- [7];
    (s, r)
}"#, refused("type mismatch [null, Array<Array<i64>>, Array<null>] does not contain Array<i64>"); FuseExpect::None);

run!(
    union_bound_through_a_typedef_takes_a_decided_array,
    r#"{
    type P<'x, 'y> = [Array<'x>, Array<'y>, null];
    let f = 'a0: P<'a2, 'a1>, 'a1: P<'a3, 'a2>, 'a2: P<'a3, 'a3>, 'a3: [i64, string] |x2: 'a2, x1: 'a1, x0: 'a0| x0;
    let v: Array<null> = never();
    let s = array::len(v);
    let r = f([1], null, v);
    v <- [null];
    (s, r)
}"#,
    prints("[i64:1, [null]]")
);

// Of two members that take the value's constructor the one a probe
// admits it into has the parts, and so does one that holds the other.
// CR claude for claude: [test-gap/low] `cannot hold f64` holds whether the admitting
// member decides z as i64 or z takes both members' first parts, [i64, string]: z <- 2.5
// is refused either way, and the twin's z <- 3 runs either way. A change that let z
// take string, after which `("s", 1)` reaches f though no member of its bound holds it,
// passes both tests. Write `z <- "s"` and pin `z is i64, inferred from an earlier use,
// and cannot hold string`. probe:
// design/review-2026-11-16/repro/tests-types-c-weak-union-pins.sh (tests-types-c-08)
run!(union_bound_one_admitting_member_decides_the_cell, r#"{
    let f = 'a: [(i64, i64), (string, string)] |v: 'a| v;
    let z = never();
    let r = f((z, 1));
    z <- 2.5;
    r
}"#, refused("type mismatch [(i64, i64), (string, string)] does not contain (f64, i64)"); FuseExpect::None);

run!(
    union_bound_one_admitting_member_accepts,
    r#"{
    let f = 'a: [(i64, i64), (string, string)] |v: 'a| v;
    let z = never();
    let r = f((z, 1));
    z <- 3;
    r.0
}"#,
    is(Value::I64(3))
);

run!(
    union_bound_members_decided_alike_take_an_empty_array,
    r#"{
    type P<'x, 'y> = [Array<'x>, Array<'y>, null];
    let f = 'a: P<'b, 'c> |y: 'b, z: 'c, x: 'a| x;
    let r = f(1, 2, []);
    array::len(array::push(r, 3))
}"#,
    is(Value::I64(1))
);

run!(union_bound_members_decided_alike_bound_an_empty_array, r#"{
    type P<'x, 'y> = [Array<'x>, Array<'y>, null];
    let f = 'a: P<'b, 'c> |y: 'b, z: 'c, x: 'a| x;
    let r = f(1, 2, []);
    array::len(array::push(r, "s"))
}"#, refused("unsatisfiable constraints: string & i64"); FuseExpect::None);

// A read-only reference bound holds a writable reference's referent
// within its own, a writable one makes it its own exactly, and a map
// bound passes its key down as it does its value.
run!(reference_bound_reaches_a_writable_referent, r#"{
    let f = 'a: &[i64, null] |v: 'a| v;
    let z = never();
    let r = f(&mut z);
    z <- "hello";
    *r
}"#, refused("type mismatch [i64, null] does not contain string"); FuseExpect::None);

run!(
    reference_bound_writable_referent_accepts,
    r#"{
    let f = 'a: &[i64, null] |v: 'a| v;
    let z = never();
    let r = f(&mut z);
    z <- 5;
    *r
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

run!(writable_reference_bound_decides_the_referent, r#"{
    let f = 'a: &mut [i64, null] |v: 'a| v;
    let z = never();
    let r = f(&mut z);
    z <- "hello";
    *r
}"#, refused("type mismatch [i64, null] does not contain string"); FuseExpect::None);

run!(
    writable_reference_bound_referent_accepts,
    r#"{
    let f = 'a: &mut [i64, null] |v: 'a| v;
    let z = never();
    let r = f(&mut z);
    z <- 5;
    *r
}"#,
    is(Value::I64(5));
    FuseExpect::None
);

run!(map_bound_reaches_an_open_key, r#"{
    let f = 'a: Map<[string, i64], [i64, null]> |v: 'a| v;
    let z = never();
    let r = f({z => 1});
    z <- 2.5;
    r
}"#, refused("type mismatch [i64, string] does not contain f64"); FuseExpect::None);

run!(
    map_bound_open_key_accepts,
    r#"{
    let f = 'a: Map<[string, i64], [i64, null]> |v: 'a| v;
    let z = never();
    let r = f({z => 1});
    z <- "k";
    r{"k"}$
}"#,
    is(Value::I64(1))
);

// A union bound's members are what its names and its decided variables
// stand for: two that take the value are a choice, one decides the cell.
run!(union_bound_two_named_members_refuse_an_undecided_element, r#"{
    type A = Array<i64>;
    type B = Array<string>;
    let f = 'a: [A, B] |v: 'a| v;
    let z = never();
    let r = f([z]);
    z <- 2.5;
    r
}"#, refused("type mismatch [A, B] does not contain Array<f64>"); FuseExpect::None);

run!(
    union_bound_two_named_members_take_a_decided_element,
    r#"{
    type A = Array<i64>;
    type B = Array<string>;
    let f = 'a: [A, B] |v: 'a| v;
    let z: i64 = never();
    let r = f([z]);
    z <- 7;
    r[0]$
}"#,
    is(Value::I64(7))
);

run!(union_bound_one_named_member_decides_the_cell, r#"{
    type A = Array<[i64, null]>;
    let f = 'a: [A, null] |v: 'a| v;
    let z = never();
    let r = f([z]);
    z <- "s";
    r
}"#, refused("type mismatch [i64, null] does not contain string"); FuseExpect::None);

run!(
    union_bound_one_named_member_accepts,
    r#"{
    type A = Array<[i64, null]>;
    let f = 'a: [A, null] |v: 'a| v;
    let z = never();
    let r = f([z]);
    z <- 5;
    r[0]$
}"#,
    is(Value::I64(5))
);

run!(union_bound_a_recursive_name_decides_the_cell, r#"{
    type T = [Array<T>, i64];
    let f = 'a: [T, null] |v: 'a| v;
    let z = never();
    let r = f([z]);
    z <- "s";
    r
}"#, refused("type mismatch T does not contain string"); FuseExpect::None);

run!(union_bound_two_decided_variable_members_refuse_an_undecided_element, r#"{
    let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v;
    let z = never();
    let r = f(["s"], [z]);
    z <- 2.5;
    r
}"#, refused("type mismatch [Array<string>, Array<i64>] does not contain Array<f64>"); FuseExpect::None);

run!(union_bound_a_decided_variable_member_leaves_one_taker, r#"{
    let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v;
    let z = never();
    let r = f("s", [z]);
    z <- 2.5;
    r
}"#, refused("type mismatch [string, Array<i64>] does not contain Array<f64>"); FuseExpect::None);

run!(
    union_bound_a_decided_variable_member_accepts,
    r#"{
    let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v;
    let z = never();
    let r = f("s", [z]);
    z <- 3;
    r[0]$
}"#,
    is(Value::I64(3))
);

// A variable member nothing has decided yet takes an undecided element
// after the structural member: the element is the structural member's.
run!(union_bound_a_variable_member_takes_what_the_structural_member_cannot, r#"{
    let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v;
    let z = never();
    let r = f([z], ["s"]);
    z <- "s";
    r
}"#, prints(r#"["s"]"#); FuseExpect::Jit);

run!(
    union_bound_a_decided_variable_member_takes_no_other_type_refused,
    r#"{
    let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v;
    let z = never();
    let r = f([z], ["s"]);
    z <- true;
    r
}"#,
    refused("Array<bool> goes to no member of ['b, Array<i64>]");
    FuseExpect::None
);

run!(
    union_bound_an_open_variable_member_structural_taker_accepts,
    r#"{
    let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v;
    let z = never();
    let r = f([z], ["s"]);
    z <- 3;
    r[0]$
}"#,
    is(Value::I64(3))
);

run!(
    union_bound_an_open_variable_member_takes_a_decided_array,
    r#"{
    let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v;
    let z: string = never();
    let x = [z];
    let r = f(x, ["s"]);
    z <- "s";
    r[0]$
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s")
);

run!(
    union_bound_a_variable_member_decided_first_takes_a_literal_array,
    r#"{
    let f = 'a: [Array<i64>, 'b] |w: 'b, v: 'a| v;
    let r = f(["s"], ["x"]);
    r[0]$
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "x")
);

// A value with no structure yet waits within the bound: what decides the
// variable member afterwards decides what the value may hold.
run!(union_bound_an_open_variable_member_holds_a_bare_undecided_value, r#"{
    let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v;
    let z = never();
    let r = f(z, ["s"]);
    z <- [1.5];
    r
}"#, refused("type mismatch [Array<string>, Array<i64>] does not contain Array<f64>"); FuseExpect::None);

run!(
    union_bound_an_open_variable_member_bare_undecided_value_accepts,
    r#"{
    let f = 'a: [Array<i64>, 'b] |v: 'a, w: 'b| v;
    let z = never();
    let r = f(z, ["s"]);
    z <- ["x"];
    r[0]$
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "x")
);

run!(union_bound_an_abstract_member_takes_no_array, r#"{
    type C = Abstract<i64>;
    let f = 'a: [Array<[i64, null]>, C] |v: 'a| v;
    let z = never();
    let r = f([z]);
    z <- "s";
    r
}"#, refused("type mismatch [i64, null] does not contain string"); FuseExpect::None);

// An abstract type's parameter is invariant: a bound's member makes the
// value's exactly its own, and two members that take it are a choice.
run!(union_bound_two_abstract_members_refuse_an_undecided_parameter, r#"{
    type B<'p> = Abstract<'p>;
    let f = 'a: [B<i64>, B<string>] |v: 'a| v;
    let z = never();
    let r = f(B(z));
    z <- 2.5;
    r.0
}"#, refused("type mismatch [B<i64>, B<string>] does not contain B<f64>"); FuseExpect::None);

run!(abstract_bound_decides_the_parameter, r#"{
    type B<'p> = Abstract<'p>;
    let f = 'a: B<[i64, null]> |v: 'a| v;
    let z = never();
    let r = f(B(z));
    z <- "s";
    r.0
}"#, refused("type mismatch [i64, null] does not contain string"); FuseExpect::None);

run!(
    abstract_bound_parameter_accepts,
    r#"{
    type B<'p> = Abstract<'p>;
    let f = 'a: B<[i64, null]> |v: 'a| v;
    let z = never();
    let r = f(B(z));
    z <- 5;
    r.0
}"#,
    is(Value::I64(5))
);

// The choice met inside the argument's own check names the binding too.
run!(union_bound_under_a_constructor_names_the_binding, r#"{
    let f = 'a: Array<[Array<i64>, Array<string>]> |v: 'a| v;
    let z = never();
    let r = f([[z]]);
    z <- 2.5;
    r
}"#, refused("type mismatch [Array<i64>, Array<string>] does not contain Array<f64>"); FuseExpect::None);

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
    refused("missing match cases: nothing matches Box<i64> (1:86)"),
    "/test.gx" => "type Box<'a> = Abstract<'a>; let x: [Box<i64>, string] = Box(1); let result = select x { Box<_> as b => 1, string as s => 2 }"
; FuseExpect::None);

/// Two REPL inputs each declaring a block-local abstract `T` at one
/// position declare two types: neither input's value passes the other's
/// type test, and a different representation is no redefinition.
#[tokio::test(flavor = "current_thread")]
async fn repl_inputs_declare_distinct_abstract_types() -> anyhow::Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
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
    let v = next_update_raw(&ctx.rt, &mut rx, probe.exprs[0].id).await?;
    assert_eq!(v, netidx_value::Value::I64(0));
    Ok(())
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
    refused("type mismatch f64 does not contain string"),
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

// The array could go to 'c or to Array<'d>, so it waits; the string goes
// to 'c alone and decides it, and a decided member takes no other type.
run!(
    a_member_decided_before_a_waiting_value_takes_no_other_refused,
    refused(
        "Array<i64> goes to no member of ['c, Array<'d>]: what reached the \
         union before it decided 'c as string, and a decided member takes no other type"
    ),
    "/test.gx" => r#"
        let g = |w: 'd, v: ['c, Array<'d>]| 0;
        let f = |x: 'a, t: bool| -> i64 g(x, select t { true => [1, 2], false => "s" });
        let result = f(1, true)
    "#
; FuseExpect::None);

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
    refused("type mismatch i64 does not contain 'a"),
    "/test.gx" => r#"
        let g = |x: 'a, b: bool| -> [`P(i64, i64), `P(string, i64)] select b { true => `P(1, x), false => `P("s", x) };
        let result = g(1, true)
    "#
; FuseExpect::None);

// A nested datatype is refused at its definition, written at the top level
// as in a block.
run!(
    nested_datatype_refused,
    refused("recursive type N grows its parameter 'a as it recurses"),
    "/test.gx" => r#"
        type N<'a> = [null, ('a, N<Array<'a>>)];
        let n1: N<i64> = (1, ([2], null));
        let g = |n: N<'a>| -> N<'a> n;
        let result = select g(n1) { null => 0, (x, _) => x }
    "#
);

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
    refused("type mismatch Error<ErrChain<E>> does not contain Error<ErrChain<E>>"),
    "/test.gx" => r#"let f = { type E = `A(string); |x: i64| -> i64 throws Error<ErrChain<E>> select x { 0 => error(`A("one"))?, n => n } };
let g = { type E = `A(i64); |x: i64| -> i64 throws Error<ErrChain<E>> f(x) };
let result = { catch(e) println("[e]"); g(0) }"#
; FuseExpect::None);

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
run!(singleton_over_a_declared_variable_beside_a_member_is_judged_per_call, r#"{
  let f = 'a: Number |x: 'a, b: bool| { let v = select b { true => x, false => 1 }; v * v };
  f(1, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(
    singleton_over_a_declared_variable_beside_a_member_refuses_another_number_at_the_call,
    r#"{
  let f = 'a: Number |x: 'a, b: bool| { let v = select b { true => x, false => 1 }; v * v };
  f(1.5, true)
}"#,
    refused("cannot compute with [i64, f64]: both operands must be one numeric type");
    FuseExpect::None
);

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

// A bound a typedef names is read as the structure it names, as the
// written bound is.
run!(
    reads_through_a_typedef_bound,
    prints("[i64:2, i64:5, i64:8, i64:10, i64:1]"),
    "/test.gx" => r#"
        type Point = {x: i64, y: i64};
        type Alias = Point;
        type Pair = (string, i64);
        type F = fn(x: i64) -> i64;
        type Box<'t> = {v: 't};
        let fld = 'a: Alias |p: 'a| p.y;
        let upd = 'a: Point |p: 'a| {p with y: 5};
        let call = 'a: F |g: 'a| g(4);
        let bx = 'a: Box<i64> |b: 'a| b.v + 1;
        let tup = 'a: Pair |t: 'a| t.1;
        let result = [fld({x: 0, y: 2}), upd({x: 1, y: 0}).y, call(|x| x * 2), bx({v: 9}), tup(("s", 1))]
    "#
);

run!(typedef_bound_update_is_no_type_a_call_picks, r#"{
  type Point = {x: i64, y: i64};
  let f = 'a: Point |p: 'a| -> 'a {p with x: 1};
  f({x: 0, y: 2})
}"#, refused("a call picks 'a"); FuseExpect::None);

// An abstract type's payload is read through a bound that names it.
run!(
    reads_an_abstract_payload_through_its_bound,
    prints("[i64:4, i64:7]"),
    "/test.gx" => r#"
        type W = Abstract<{x: i64}>;
        type B<'t> = Abstract<('t, string)>;
        let f = 'a: W |w: 'a| w.0.x;
        let g = 'a: B<i64> |b: 'a| b.0.0 + 1;
        let result = [f(W({x: 4})), g(B((6, "s")))]
    "#
);

// A trait bound names no structure: a read through it needs the type,
// never a typedef of the trait's name.
run!(
    trait_bound_is_read_through_as_no_typedef,
    refused("reading a struct needs its type known here: annotate the binding"),
    "/test.gx" => r#"
        trait Sh { val sh: fn(self) -> string };
        impl Sh for i64 { let sh = |s| "i" };
        let f = 'a: Sh |p: 'a| p.x;
        let result = f(1)
    "#
    ; FuseExpect::None
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

// A Data cell under a formal's argument judges what the function is
// called with: a handler refused for its result is a type mismatch.
run!(
    data_argument_cell_does_not_blame_the_handler,
    r#"{
  let apply = 'a: Data |f: [fn(v: 'a) -> _, null], v: 'a| v;
  let h = |v: i64| v;
  apply(h, 1)
}"#,
    refused("type mismatch _ does not contain i64");
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

run!(typedef_body_applying_a_plain_typedef_refused, r#"{
    type U = u8;
    type W<'a> = Abstract<U<'a>>;
    1
}"#, refused("U expects 0 type parameters"); FuseExpect::None);

run!(typedef_body_applying_a_plain_typedef_under_an_array_refused, r#"{
    type U = u8;
    type W<'a> = Array<U<'a>>;
    1
}"#, refused("U expects 0 type parameters"); FuseExpect::None);

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
// CR claude for claude: [style/low] [style/low] The comment above cites a review
// finding id, "(x-kind-markers-03)". a_union_prefers_the_member_its_elements_fit's
// comment does the same with "(t-cast-setops-06, x-rerun-5-03)", and
// partial_wrapper_bound_under_a_name_leaves_its_variable_undecided's opens "Beside
// conjunct-lower-cycle-settle-01".
// bound_closed_by_its_own_chain_settles_before_its_holder's comment ends "as a chain of
// bounds nothing was produced into always did". These record where a test came from,
// which the comment rule excludes. Drop the ids and the "always did" clause, and keep
// the rule each comment states. Found by reading; not run. (tests-types-d-08)
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
    refused(
        "type mismatch [null, 'a: unbound within Any] does not contain i64: a call picks 'a, and the body's value need not be of the type it picks; drop `-> [null, 'a]`, or write the body's type, `-> [i64, null, 'a]`"
    )
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

// A quantifier only the bound of a bound names is rigid in the body's check
// as one a bound names directly is.
run!(
    quantifier_named_by_a_bound_of_a_bound_is_rigid,
    r#"{
  let f = 'a: [i64, string], 'c: ['a, null], 'b: ['c, `X] |x: 'b| -> [`X, null, i64] x;
  f(`X)
}"#,
    refused("[i64, null, `X] does not contain 'b");
    FuseExpect::None
);

run!(
    quantifier_named_by_a_bound_of_a_bound_is_rigid_twin,
    r#"{
  let f = 'a: [i64, string], 'c: ['a, null], 'b: ['c, `X] |x: 'b| -> [`X, null, i64, string] x;
  (f("str"), f(3), f(null), f(`X))
}"#,
    prints(r#"["str", i64:3, null, "X"]"#);
    FuseExpect::Jit
);

// Two instances of a typedef whose parameters hold an open cell are
// compared by their expansions, so the order of a function type's
// positions does not decide the cell.
run!(
    typedef_instances_over_an_open_cell_agree_with_the_expansion,
    r#"{
  type L<'p> = fn(x: 'p) -> i64;
  let outer = |k: L<'c>, z: 'c| z;
  let g: fn(k: L<Any>, z: i64) -> i64 = outer;
  g(|x| 0, 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    typedef_instances_over_an_open_cell_agree_with_the_expansion_z_first,
    r#"{
  type L<'p> = fn(x: 'p) -> i64;
  let outer = |z: 'c, k: L<'c>| z;
  let g: fn(z: i64, k: L<Any>) -> i64 = outer;
  g(1, |x| 0)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// A typedef whose body names another, neither reaching itself, is
// compared by its expansion while the script's names are not yet filled.
run!(
    typedef_over_a_typedef_agrees_with_the_expansion,
    r#"{
  type L<'p> = fn(x: 'p) -> i64;
  type M<'p> = {a: L<'p>};
  let outer = |k: M<'c>, z: 'c| z;
  let g: fn(k: M<Any>, z: i64) -> i64 = outer;
  g({a: |x| 0}, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    typedef_over_a_typedef_agrees_with_the_expansion_z_first,
    r#"{
  type L<'p> = fn(x: 'p) -> i64;
  type M<'p> = {a: L<'p>};
  let outer = |z: 'c, k: M<'c>| z;
  let g: fn(z: i64, k: M<Any>) -> i64 = outer;
  g(1, {a: |x| 0})
}"#,
    prints("i64:1");
    FuseExpect::None
);

// Two instances of a recursive definition over an open cell are compared
// by their expansions too, alone, under another typedef or reaching
// itself through a second definition.
run!(
    recursive_typedef_instances_over_an_open_cell_agree_with_the_expansion,
    r#"{
  type T<'p> = [`Nil, `Cons(fn(x: 'p) -> i64, T<'p>)];
  let outer = |k: T<'c>, z: 'c| z;
  let g: fn(k: T<Any>, z: i64) -> i64 = outer;
  g(`Nil, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    recursive_typedef_instances_over_an_open_cell_agree_with_the_expansion_z_first,
    r#"{
  type T<'p> = [`Nil, `Cons(fn(x: 'p) -> i64, T<'p>)];
  let outer = |z: 'c, k: T<'c>| z;
  let g: fn(z: i64, k: T<Any>) -> i64 = outer;
  g(1, `Nil)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    typedef_over_a_recursive_typedef_agrees_with_the_expansion,
    r#"{
  type T<'p> = [`Nil, `Cons(fn(x: 'p) -> i64, T<'p>)];
  type W<'p> = (T<'p>, i64);
  let outer = |k: W<'c>, z: 'c| z;
  let g: fn(k: W<Any>, z: i64) -> i64 = outer;
  g((`Nil, 0), 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    typedef_over_a_recursive_typedef_agrees_with_the_expansion_z_first,
    r#"{
  type T<'p> = [`Nil, `Cons(fn(x: 'p) -> i64, T<'p>)];
  type W<'p> = (T<'p>, i64);
  let outer = |z: 'c, k: W<'c>| z;
  let g: fn(z: i64, k: W<Any>) -> i64 = outer;
  g(1, (`Nil, 0))
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    mutually_recursive_typedef_instances_over_an_open_cell_agree_with_the_expansion,
    r#"{
  type A<'p> = [`Nil, `A(fn(x: 'p) -> i64, B<'p>)];
  type B<'p> = [`Nil, `B(A<'p>)];
  let outer = |k: A<'c>, z: 'c| z;
  let g: fn(k: A<Any>, z: i64) -> i64 = outer;
  g(`Nil, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// A parameter only the recursion holds is in no value of the type, so an
// instance over a cell decides nothing of it.
run!(
    a_parameter_only_the_recursion_holds_is_not_decided,
    r#"{
  type P<'a> = [`N, `C(P<'a>)];
  let f = |k: P<'c>, z: 'c| z;
  let x: P<string> = `C(`N);
  f(x, 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// The orders of four cells are 24 applications of one definition, which
// its arguments answer for.
run!(
    open_instances_of_one_recursive_typedef_check,
    r#"{
  type D<'a0, 'a1, 'a2, 'a3> = [`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, 'a2, 'a3>), `Y(D<'a1, 'a2, 'a3, 'a0>)];
  let outer = |k: D<'q0, 'q1, 'q2, 'q3>, z: 'q0| z;
  let g: fn(k: D<Any, Any, Any, Any>, z: i64) -> i64 = outer;
  let h = |k: D<'q0, 'q1, 'q2, 'q3>| 0;
  let x: D<Any, Any, Any, Any> = `N;
  g(`N, 1) + h(x)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// One definition over six and twelve cells reaches 720 and 479001600
// applications of itself: two of them are compared by their arguments, a
// step a parameter.
run!(
    six_parameters_of_one_recursive_typedef_check,
    r#"{
  type D<'a0, 'a1, 'a2, 'a3, 'a4, 'a5> = [`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, 'a2, 'a3, 'a4, 'a5>), `Y(D<'a1, 'a2, 'a3, 'a4, 'a5, 'a0>)];
  let outer = |k: D<'q0, 'q1, 'q2, 'q3, 'q4, 'q5>, z: 'q0| z;
  let g: fn(k: D<Any, Any, Any, Any, Any, Any>, z: i64) -> i64 = outer;
  g(`N, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    twelve_parameters_of_one_recursive_typedef_check,
    r#"{
  type D<'a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11> = [`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11>), `Y(D<'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11, 'a0>)];
  let outer = |k: D<'q0, 'q1, 'q2, 'q3, 'q4, 'q5, 'q6, 'q7, 'q8, 'q9, 'q10, 'q11>, z: 'q0| z;
  let g: fn(k: D<Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any>, z: i64) -> i64 = outer;
  g(`N, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// Two definitions with one body are compared by their expansions, whose
// applications over six open cells are the cells' 720 orders, each
// reaching every other: the union of an expansion asks one trial of an
// order for its members, and the placement that trial allows walks the
// applications below with no trial of their own.
// CR claude for eric: [perf/medium] [perf/medium] This pin stops at 6 parameters, and
// the cost is already factorial there. The same two permuting definitions over open
// cells check in 0.1 s at 6 parameters, 0.3 s at 7 and 2.3 s at 8; at 9 the check
// passes a 6 GB memory cap within 11 s (debug build). The same happens when D has one
// member more than F (so not one body), and through a call `h(x, 1)` with `x: F<i64,
// ..>`. A six-line program can therefore exhaust the memory of `--check` and of the
// language server. The walk of two definitions by their expansions (ContainsHist,
// Type::place_in_order) has no bound, unlike setops::WALK_PAIRS for diff. This needs a
// decision: bound the walk and say what a cut answers, or compare two definitions'
// applications by their arguments where their bodies meet the parameters alike, as
// Type::applications_hold does for one definition. Then pin 9 or more parameters.
// probe: design/review-2026-11-16/repro/tests-types-d-two-typedefs-nine-params.gx
// (tests-types-d-01)
run!(
    six_parameters_of_two_recursive_typedefs_with_one_body_check,
    r#"{
  type D<'a0, 'a1, 'a2, 'a3, 'a4, 'a5> = [`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, 'a2, 'a3, 'a4, 'a5>), `Y(D<'a1, 'a2, 'a3, 'a4, 'a5, 'a0>)];
  type F<'a0, 'a1, 'a2, 'a3, 'a4, 'a5> = [`N, `X(fn(x: 'a0) -> i64, F<'a1, 'a0, 'a2, 'a3, 'a4, 'a5>), `Y(F<'a1, 'a2, 'a3, 'a4, 'a5, 'a0>)];
  let outer = |k: D<'q0, 'q1, 'q2, 'q3, 'q4, 'q5>, z: 'q0| z;
  let g: fn(k: F<Any, Any, Any, Any, Any, Any>, z: i64) -> i64 = outer;
  g(`N, 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// Two members of one head are a choice at every application of a
// recursive definition: the member that takes a type as it stands is
// walked with every cell held, and that walk keeps the verdicts it
// reaches, where one that keeps none takes every path through the
// applications.
run!(
    two_members_of_one_head_over_a_recursive_typedef_check,
    r#"{
  type D<'a0, 'a1, 'a2> = [`Y(D<'a0, 'a1, null>), Error<D<'a1, 'a0, 'a2>>, `Y(D<'a1, 'a2, 'a0>), `U(fn(x0: 'a0, x1: 'a1, x2: 'a2) -> i64)];
  let outer = |k: D<'q1, 'q1, 'q2>, z: i64| z;
  let g: fn(k: D<Any, Any, i64>, z: i64) -> i64 = outer;
  g(`U(|x0, x1, x2| 0), 1)
}"#,
    prints("i64:1");
    FuseExpect::None
);

// A union's trial gives the first array to `Array<'a>`, deciding 'a; the
// placement gives it to the member that holds it as it stands, deciding
// nothing, so the union after it asks a trial of its own, which places
// the `T` member first: as written, 'a would be `Array<i64>`.
run!(
    a_union_below_a_choice_between_members_asks_its_own_trial,
    r#"{
  let f = |x: [`X([Array<'a>, Array<Array<['a, i64, Array<i64>]>>], [Array<'a>, `T('a)]), `Y('d)]| 0;
  let v: [`X(Array<Array<[i64, Array<i64>]>>, [Array<Array<i64>>, `T(Array<[i64, Array<i64>]>)]), `Y(Array<Array<Array<Array<Array<Array<Array<i64>>>>>>>)] = `Y([[[[[[[1]]]]]]]);
  f(v)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// The same choice, between the members a typedef names.
run!(
    a_union_below_a_choice_between_a_typedefs_members_asks_its_own_trial,
    r#"{
  type P<'x, 'y> = [Array<'x>, Array<Array<['y, i64, Array<i64>]>>];
  let f = |x: [`X(P<'a, 'c>, [Array<'a>, `T('a)]), `Y('d)]| 0;
  let v: [`X(Array<Array<[i64, Array<i64>]>>, [Array<Array<i64>>, `T(Array<[i64, Array<i64>]>)]), `Y(Array<Array<Array<Array<Array<Array<Array<i64>>>>>>>)] = `Y([[[[[[[1]]]]]]]);
  f(v)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// Only the deepest-first order holds ('c is widest under `Y`), so the
// trial of it decides the verdict. In it the `X` part before the union
// binds 'b: `Array<'b>` is decided when the union is met, takes the
// decided array and binds nothing, and 'a is left for the strings,
// wherever the two members stand.
run!(
    a_member_decided_in_a_trial_takes_a_decided_type,
    r#"{
  type U<'x, 'y> = [Array<'x>, Array<'y>];
  let f = |x: [`X('b, 'c, U<'a, 'b>, Array<'a>), `Y(Array<Array<Array<'c>>>)]| 0;
  let v: [`X([i64, null], i64, Array<i64>, Array<string>), `Y(Array<Array<Array<[i64, string]>>>)] = `Y([[[1]]]);
  f(v)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    a_member_decided_in_a_trial_takes_a_decided_type_in_the_other_member_order,
    r#"{
  type U<'x, 'y> = [Array<'x>, Array<'y>];
  let f = |x: [`X('b, 'c, U<'b, 'a>, Array<'a>), `Y(Array<Array<Array<'c>>>)]| 0;
  let v: [`X([i64, null], i64, Array<i64>, Array<string>), `Y(Array<Array<Array<[i64, string]>>>)] = `Y([[[1]]]);
  f(v)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// The members are applications of two names: a probe of one asks whether
// it is closed and is answered from before 'b was bound, at the write
// count the union is met at. The union still reads the cells.
run!(
    a_named_member_decided_in_a_trial_takes_a_decided_type,
    r#"{
  type A<'x> = Array<'x>;
  type B<'x> = Array<'x>;
  type U<'x, 'y> = [A<'x>, B<'y>];
  let f = |x: [`X('b, 'c, U<'a, 'b>, Array<'a>), `Y(Array<Array<Array<'c>>>)]| 0;
  let v: [`X([i64, null], i64, Array<i64>, Array<string>), `Y(Array<Array<Array<[i64, string]>>>)] = `Y([[[1]]]);
  f(v)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    a_named_member_decided_in_a_trial_takes_a_decided_type_in_the_other_member_order,
    r#"{
  type A<'x> = Array<'x>;
  type B<'x> = Array<'x>;
  type U<'x, 'y> = [A<'x>, B<'y>];
  let f = |x: [`X('b, 'c, U<'b, 'a>, Array<'a>), `Y(Array<Array<Array<'c>>>)]| 0;
  let v: [`X([i64, null], i64, Array<i64>, Array<string>), `Y(Array<Array<Array<[i64, string]>>>)] = `Y([[[1]]]);
  f(v)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// A cell two arguments of one definition share holds what both ask.
run!(
    a_cell_two_arguments_of_one_typedef_share_takes_both,
    r#"{
  type D<'a0, 'a1, 'a2> = [Array<D<'a1, 'a0, 'a2>>, `U('a0, 'a1, 'a2)];
  let h = |k: D<'q1, 'q1, 'q2>| 0;
  let x: D<i64, Any, Any> = `U(1, "s", null);
  h(x)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// A variable a bound's typedef applications hold stands above what the
// first argument gave them, undecided: a later argument those hold fits
// it, recursive typedef or not, as it fits the union written out.
run!(
    an_argument_a_bound_variables_lower_bound_holds_fits,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a0: (T<'a1>, T<'a1>), 'a1: [i64, string] |x0: 'a0, x1: 'a1| x1;
  let v: (T<[i64, string]>, T<[i64, string]>) = (`Node([]), `Leaf(1));
  f(v, 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    an_argument_a_bound_variables_lower_bound_holds_fits_annotated,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a0: (T<'a1>, T<'a1>), 'a1: [i64, string] |x0: 'a0, x1: 'a1| x1;
  let v: (T<[i64, string]>, T<[i64, string]>) = (`Node([]), `Leaf(1));
  let one: [i64, string] = 1;
  f(v, one)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    an_argument_a_bound_variables_lower_bound_holds_fits_no_recursion,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<'x>)];
  let f = 'a0: (T<'a1>, T<'a1>), 'a1: [i64, string] |x0: 'a0, x1: 'a1| x1;
  let v: (T<[i64, string]>, T<[i64, string]>) = (`Node([]), `Leaf(1));
  f(v, 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    an_argument_a_bound_variables_lower_bound_holds_fits_two_definitions,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  type U<'x> = [`Leaf('x), `Node(Array<U<'x>>)];
  let f = 'a0: (T<'a1>, T<'a1>), 'a1: [i64, string] |x0: 'a0, x1: 'a1| x1;
  let v: (U<[i64, string]>, U<[i64, string]>) = (`Node([]), `Leaf(1));
  f(v, 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    an_argument_a_bound_variables_lower_bound_holds_fits_no_bound,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a0: (T<'a1>, T<'a1>) |x0: 'a0, x1: 'a1| x1;
  let v: (T<[i64, string]>, T<[i64, string]>) = (`Node([]), `Leaf(1));
  f(v, 1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_member_of_a_bound_variables_lower_bound_fits,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  type L<'a> = [`Nil, `Cons('a, L<'a>)];
  let f = 'a0: L<T<'a1>> |x0: 'a0, x1: 'a1| x1;
  let v: L<T<L<T<string>>>> = `Nil;
  f(v, `Nil)
}"#,
    prints("\"Nil\"");
    FuseExpect::Jit
);

run!(
    a_member_of_a_bound_variables_lower_bound_fits_annotated,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  type L<'a> = [`Nil, `Cons('a, L<'a>)];
  let f = 'a0: L<T<'a1>> |x0: 'a0, x1: 'a1| x1;
  let v: L<T<L<T<string>>>> = `Nil;
  let nil: L<T<string>> = `Nil;
  f(v, nil)
}"#,
    prints("\"Nil\"");
    FuseExpect::Jit
);

run!(
    a_value_of_a_bound_variables_lower_bound_fits,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  type L<'a> = [`Nil, `Cons('a, L<'a>)];
  let f = 'a0: L<T<'a1>> |x0: 'a0, x1: 'a1| x1;
  let v: L<T<T<string>>> = `Nil;
  f(v, `Leaf("s"))
}"#,
    prints("[\"Leaf\", \"s\"]");
    FuseExpect::Jit
);

// What the lower bounds do not hold is still refused.
run!(
    an_argument_a_bound_variables_lower_bound_does_not_hold_widens_it,
    r#"{
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a0: (T<'a1>, T<'a1>) |x0: 'a0, x1: 'a1| x1;
  let v: (T<[i64, string]>, T<[i64, string]>) = (`Node([]), `Leaf(1));
  f(v, 2.5)
}"#,
    prints("f64:2.5");
    FuseExpect::Jit
);

// A variable settled at its lower bounds is bound as any is: the variable
// its own bound holds stands above what it took, so the next link of a
// chain of bounds holds the argument that started it.
run!(
    a_bound_chain_hands_on_what_a_settled_link_took,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let v: ([i64, null], i64) = (1, 1);
  f(v, (1, 1), (null, 1)).1
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_bound_chain_hands_on_what_a_settled_link_took_annotated,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let v: ([i64, null], i64) = (1, 1);
  let w: ([i64, null], i64) = (null, 1);
  f(v, (1, 1), w).1
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_bound_chain_widens_past_what_a_settled_link_holds,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let v: ([i64, null], i64) = (1, 1);
  f(v, (1, 1), "s")
}"#,
    prints("\"s\"");
    FuseExpect::Jit
);

run!(
    a_bound_chain_over_undecided_elements_builds,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let r = f((1, 1), (1, never()), (never(), 1));
  1
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_bound_chain_takes_null_below_a_settled_link,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ['a2, null] |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  f((1, 1), (1, never()), null)
}"#,
    prints("null");
    FuseExpect::Jit
);

run!(
    a_result_within_a_bound_holding_one_variable_twice_takes_both_types,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2, i64) |x0: 'a0| -> 'a1 never();
  let r: (i64, string, i64) = f((1, "s", 1));
  1
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    a_result_within_a_bound_holding_one_variable_twice_takes_an_argument_of_one_type,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2, i64) |x0: 'a0| -> 'a1 never();
  let p: ([i64, string], [i64, string], i64) = (1, "s", 1);
  let r: ([i64, string], [i64, string], [i64, null]) = f(p);
  1
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// The links of a chain one argument holds settle in the order of the
// chain, whatever the variables are named.
run!(
    a_bound_chain_in_one_argument_settles_in_the_chains_order,
    r#"{
  let f = 'k: ['j, null], 'b: ('a, 'a), 'j: ['b, null]
    |x: {f0: 'k, f1: 'j, f2: 'b}| x.f2;
  let v: ([i64, null], i64) = (1, 1);
  f({f0: v, f1: (1, 1), f2: (null, 1)}).1
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// A result's variable a wider annotation settles at its lower bounds is
// bound as any is: the variable its bound holds takes its part.
run!(
    a_result_settled_at_its_lower_bounds_hands_them_to_its_bound_refused,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, i64) |x0: 'a0| -> ('a1, Array<'a2>) never();
  let r: (([i64, string], [i64, null]), Array<string>) = f((1, 1));
  1
}"#,
    refused("type mismatch string does not contain i64");
    FuseExpect::None
);

run!(
    a_result_settled_at_its_lower_bounds_hands_them_to_its_bound,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, i64) |x0: 'a0| -> ('a1, Array<'a2>) never();
  let r: (([i64, string], [i64, null]), Array<i64>) = f((1, 1));
  1
}"#,
    prints("i64:1");
    FuseExpect::None
);

// A widened variable is bound again as any is: the variable its bound
// holds, which its first argument left open, takes its part of the wider
// type.
run!(
    a_widening_hands_its_bound_a_type_a_later_argument_widens,
    r#"{
  let f = 'a2: [('a3, bool), bool] |x1: Array<'a2>, x2: 'a2, x3: 'a3| x3;
  let n: [(duration, bool), bool] = (duration:1.s, true);
  f([true], n, "s")
}"#,
    prints("\"s\"");
    FuseExpect::Jit
);

run!(
    a_widening_hands_its_bound_the_wider_type,
    r#"{
  let f = 'a2: [('a3, bool), bool] |x1: Array<'a2>, x2: 'a2, x3: 'a3| x3;
  let n: [(duration, bool), bool] = (duration:1.s, true);
  f([true], n, duration:2.s)
}"#,
    prints("duration:2.s");
    FuseExpect::Jit
);

run!(
    a_widening_over_an_undecided_element_hands_its_bound_the_wider_type,
    r#"{
  let f = 'a3: [Array<'a4>, duration], 'a1: [Array<'a2>, string], 'a4: Any,
    'a2: [('a3, bool), bool] |x1: 'a1, x2: 'a2, x3: 'a3| 7;
  f([(never(), true)], (duration:1.s, true), [1])
}"#,
    prints("i64:7");
    FuseExpect::Jit
);

// A bound that holds one variable twice takes one type for it at a
// settle, as at a bind: below a pair of two types the settle leaves its
// variable open and the call is refused; the pair annotated as one type
// twice runs.
run!(
    a_settle_under_a_bound_holding_a_variable_twice_takes_both_types,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2) |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let v: [i64, string] = 1;
  f((1, "s"), (1, never()), v)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_settle_under_a_bound_holding_a_variable_twice_read_as_one_type_refused,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2) |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let v: [i64, string] = 1;
  let r: i64 = f((1, "s"), (1, never()), v);
  r
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    a_settle_under_a_bound_holding_a_variable_twice,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2) |x0: 'a0, x1: 'a1, x2: 'a2| x2;
  let v: [i64, string] = 1;
  let p: ([i64, string], [i64, string]) = (1, "s");
  f(p, (1, never()), v)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_result_settled_under_a_bound_holding_a_variable_twice_takes_a_literal_of_two_types,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2, i64) |x0: 'a0| -> 'a1 never();
  let r: ([i64, string], [i64, string], [i64, null]) = f((1, "s", 1));
  1
}"#,
    prints("i64:1");
    FuseExpect::None
);

run!(
    a_result_settled_under_a_bound_holding_a_variable_twice,
    r#"{
  let f = 'a0: ['a1, null], 'a1: ('a2, 'a2, i64) |x0: 'a0| -> 'a1 never();
  let p: ([i64, string], [i64, string], i64) = (1, "s", 1);
  let r: ([i64, string], [i64, string], [i64, null]) = f(p);
  1
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// A recursive typedef with a bare parameter: the union of its expansion
// places its members on copies, where the definition's recursion is met
// by assumption, as the walk on the cells themselves meets it, rather
// than expanded on fresh cells again.
run!(
    a_recursive_typedef_with_a_bare_parameter_over_an_open_cell_checks,
    r#"{
  type Tree<'a> = ['a, `Node(Array<Tree<'a>>)];
  let f = |x: Tree<'q>, y: 'q| y;
  let v: Tree<i64> = `Node([1]);
  f(v, 2)
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

run!(
    a_recursive_typedef_with_a_bare_parameter_widens_its_cell,
    r#"{
  type Tree<'a> = ['a, `Node(Array<Tree<'a>>)];
  let f = |x: Tree<'q>, y: 'q| y;
  let v: Tree<i64> = `Node([1]);
  f(v, "s")
}"#,
    prints("\"s\"");
    FuseExpect::Jit
);

run!(
    a_recursive_typedef_with_a_bare_parameter_written_out_checks,
    r#"{
  type Tree<'a> = ['a, `Node(Array<Tree<'a>>)];
  let f = |x: ['q, `Node(Array<Tree<'q>>)], y: 'q| y;
  let v: Tree<i64> = `Node([1]);
  f(v, 2)
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

run!(
    a_recursive_typedef_with_a_bare_parameter_is_walked_by_a_closed_reader,
    r#"{
  type Tree<'a> = ['a, `Node(Array<Tree<'a>>)];
  let rec sum = |x: Tree<i64>| -> i64 select x {
    i64 as n => n,
    `Node(a) => array::fold(a, 0, |acc, t| acc + sum(t))
  };
  let id = |x: Tree<'q>| x;
  let v: Tree<i64> = `Node([1, `Node([2, 3])]);
  sum(id(v))
}"#,
    prints("i64:6");
    FuseExpect::Jit
);

run!(
    two_definitions_with_a_bare_parameter_check,
    r#"{
  type Tree<'a> = ['a, `Node(Array<Tree<'a>>)];
  type Twin<'a> = ['a, `Node(Array<Twin<'a>>)];
  let f = |x: Tree<'q>, y: 'q| y;
  let v: Twin<i64> = `Node([1]);
  f(v, 2)
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

run!(
    two_recursive_typedefs_with_a_bare_parameter_over_an_open_cell_check,
    r#"{
  type D0<'a0> = ['a0, `U1(D1<'a0>), `U2(&mut 'a0)];
  type D1<'a0> = [(D0<'a0>, i64), null, `Uz0(&mut 'a0)];
  let f = |x: Array<D0<'q0>>| array::len(x);
  let v: Array<D0<[i64, string, null]>> = [1, "s", `U1(null)];
  f(v)
}"#,
    prints("i64:3");
    FuseExpect::Jit
);

run!(
    a_permuting_recursive_typedef_with_a_bare_parameter_checks,
    r#"{
  type D<'a0, 'a1> = [`U0(D<'a1, 'a0>), 'a1, `Uz0(Array<'a0>)];
  let f = |x: Array<D<'q0, 'q1>>, y: 'q0, z: 'q1| y;
  let v: Array<D<i64, string>> = ["s", `U0(1), `Uz0([1])];
  f(v, 2, "t")
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

run!(
    a_bare_parameter_under_a_union_argument_of_a_recursive_typedef_checks,
    r#"{
  type D<'a0> = [`U0(Array<D<'a0>>), `U1(i64), 'a0, null];
  let f = |x: Array<D<['q0, null]>>, y: 'q0| y;
  let v: Array<D<string>> = ["s", `U1(1)];
  f(v, "t")
}"#,
    prints("\"t\"");
    FuseExpect::Jit
);

// Two recursive typedefs whose applications over cells that stay open
// are every order of the cells, each reaching every other: an application
// answered inside one still in progress is not walked again by a sibling.
run!(
    open_applications_of_recursive_typedefs_check,
    r#"{
  type D<'a0, 'a1, 'a2, 'a3> = [`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, 'a2, 'a3>), `Y(D<'a1, 'a2, 'a3, 'a0>)];
  type F<'a0, 'a1, 'a2, 'a3> = [`N, `X(fn(x: 'a0) -> i64, F<'a1, 'a0, 'a2, 'a3>), `Y(F<'a1, 'a2, 'a3, 'a0>)];
  let outer = |k: D<'q0, 'q1, 'q2, 'q3>, z: 'q0| z;
  let g: fn(k: F<Any, Any, Any, Any>, z: i64) -> i64 = outer;
  g(`N, 1)
}"#,
    is(Value::I64(1));
    FuseExpect::None
);

// Two recursive typedefs reaching each other, applied over cells the call
// binds one by one: a pair of applications answered before a cell was
// bound and again since, inside one still in progress, is not walked
// again by a sibling.
run!(
    open_applications_of_mutually_recursive_typedefs_check,
    r#"{
  type D0<'a0, 'a1, 'a2> = [Array<D0<'a1, 'a0, 'a2>>, `X(D0<'a1, 'a0, 'a2>, 'a0), `Y(D1<'a1, 'a2, 'a0>)];
  type D1<'a0, 'a1, 'a2> = [Error<D0<'a1, 'a0, 'a2>>, `X(fn(x: D0<'a1, 'a0, 'a2>) -> 'a0), `Y(D1<'a1, 'a2, 'a0>)];
  type F0<'a0, 'a1, 'a2> = [Array<F0<'a1, 'a0, 'a2>>, `X(F0<'a1, 'a0, 'a2>, 'a0), `Y(F1<'a1, 'a2, 'a0>)];
  type F1<'a0, 'a1, 'a2> = [Error<F0<'a1, 'a0, 'a2>>, `X(fn(x: F0<'a1, 'a0, 'a2>) -> 'a0), `Y(F1<'a1, 'a2, 'a0>)];
  let h = |k: D0<'q0, 'q1, 'q2>, z: 'q0| z;
  let x: F0<i64, string, i64> = `X([], 7);
  h(x, 1)
}"#,
    is(Value::I64(1));
    FuseExpect::None
);

run!(
    a_dag_of_union_bounds_types_its_instances,
    r#"{
  type A<'x> = Array<'x>;
  type B<'x> = Array<'x>;
  let f = 'a0: [A<'a1>, B<'a2>, null], 'a1: [A<'a2>, B<'a3>, null], 'a2: [A<'a3>, B<'a4>, null], 'a3: [Array<'a4>, null], 'a4: Number |x: ('a0, 'a1, 'a2, 'a3, 'a4)| 7;
  let g = 'a0: [A<'a1>, B<'a2>, null], 'a1: [A<'a2>, B<'a3>, null], 'a2: [A<'a3>, B<'a4>, null], 'a3: [Array<'a4>, null], 'a4: Number |x: {a: 'a0, b: 'a3}| 8;
  f(never()) + g(never())
}"#,
    is(Value::I64(15));
    FuseExpect::None
);

// A call settles a variable only a bound of a bound names ('a4 here) as it
// settles one a bound names: the instance holds no open cell of it.
run!(
    a_call_settles_a_variable_only_a_bound_of_a_bound_names,
    r#"{
  type A<'x> = Array<'x>;
  type B<'x> = Array<'x>;
  let f = 'a0: [A<'a1>, B<'a2>, null], 'a1: [A<'a2>, B<'a3>, null], 'a2: [A<'a3>, B<'a4>, null], 'a3: [Array<'a4>, null], 'a4: Number |x: ('a0, 'a1)| 7;
  let g = 'a0: [A<'a1>, B<'a2>, null], 'a1: [A<'a2>, B<'a3>, null], 'a2: [A<'a3>, B<'a4>, null], 'a3: [Array<'a4>, null], 'a4: Number |x: ('a1, 'a0)| 8;
  f(never()) + g(never())
}"#,
    is(Value::I64(15));
    FuseExpect::None
);

// A written binding holds one instance of each function its value holds
// once, wherever the value's type repeats it: a value of `mk(g)` is one
// instance of `g` twice, as a `let` of it is; two reads are two.
run!(
    written_value_sharing_a_node_holds_one_instance,
    r#"{
  let g = |x| x;
  let mk = |f| (f, f);
  let p = (|x: i64| x, |x: string| x);
  p <- mk(g);
  (p.0(1), p.1("s"))
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    written_holder_sharing_a_node_holds_one_instance,
    r#"{
  let h = {f: |x| x};
  let hh = (h, h);
  let q: ({f: fn(x: i64) -> i64}, {f: fn(x: string) -> string}) = hh;
  q <- never();
  (q.0.f(1), q.1.f("a"))
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    written_holder_of_one_call_result_holds_one_instance_refused,
    r#"{
  let h = {f: |x| x};
  let dup = |p| (p, p);
  let hh = dup(h);
  let q: ({f: fn(x: i64) -> i64}, {f: fn(x: string) -> string}) = hh;
  q <- never();
  (q.0.f(1), q.1.f("a"))
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    written_value_of_two_reads_holds_two_instances,
    r#"{
  let g = |x| x;
  let h = {f: |x| x};
  let p = (|x: i64| x, |x: string| x);
  p <- (g, g);
  let q: ({f: fn(x: i64) -> i64}, {f: fn(x: string) -> string}) = (h, h);
  q <- never();
  (p.0(1), p.1("s"), q.0.f(2), q.1.f("a"))
}"#,
    prints(r#"[i64:1, "s", i64:2, "a"]"#);
    FuseExpect::None
);

// A merge takes the lower of its two cells' levels: a holder read's
// generic copy that meets a written name's cells is that name's one
// instance, so a struct update, or a collection or tuple of composites,
// holding the name shares its type, and the call that decides it decides
// the name.
const ONE_INSTANCE_WRITTEN: &str = "type mismatch i64 does not contain string";

run!(
    struct_update_holding_a_written_name_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = {s with f: j};
  let r = (t.f("s"), j(1));
  j <- |x: i64| x + 1;
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    struct_update_holding_an_unwritten_name_runs,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = {s with f: j};
  (t.f("s"), j("t"))
}"#,
    prints(r#"["s", "t"]"#);
    FuseExpect::None
);

run!(
    struct_update_over_a_name_written_later_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = never();
  let t = {s with f: j};
  let r = t.f("s");
  j <- |x: i64| x + 1;
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    chained_struct_update_over_a_name_written_later_shares_its_type,
    r#"{
  let s = {f: |x| x, n: 1};
  let j = never();
  let t = {s with n: 2};
  let u = {t with f: j};
  let r = (u.f("s"), u.f(1));
  j <- |x: i64| x + 1;
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    seqq_struct_update_over_a_name_written_later_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = never();
  let t = sys::time::timer(duration:1.ms, false);
  let r = seqq t { let u = {s with f: j}; (u.f("s"), u.f(1)) };
  j <- |x: i64| x + 1;
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    struct_update_over_a_name_written_first_takes_its_type,
    r#"{
  let s = {f: |x| x};
  let j = never();
  j <- |x: i64| x + 1;
  let t = {s with f: j};
  t.f("s")
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    struct_update_over_a_declared_name_written_later_runs,
    r#"{
  let s = {f: |x| x};
  let j: fn(x: i64) -> i64 = never();
  let t = {s with f: j};
  let r = (t.f(2), s.f("s"));
  j <- |x: i64| x + 1;
  r
}"#,
    prints(r#"[i64:3, "s"]"#);
    FuseExpect::None
);

run!(
    array_of_composites_holding_a_written_name_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = [s, {f: j}];
  j <- |x: i64| x + 1;
  (t[1]$.f("s"), j(1))
}"#,
    refused(ONE_INSTANCE_WRITTEN);
    FuseExpect::None
);

run!(
    array_of_composites_holding_a_written_name_runs_at_its_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = [s, {f: j}];
  j <- never<fn(x: i64) -> i64>();
  (t[1]$.f(2), j(1))
}"#,
    prints("[i64:2, i64:1]");
    FuseExpect::None
);

run!(
    list_of_composites_holding_a_written_name_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = [<s, {f: j}>];
  j <- |x: i64| x + 1;
  (list::nth(t, 1)$.f("s"), j(1))
}"#,
    refused(ONE_INSTANCE_WRITTEN);
    FuseExpect::None
);

run!(
    map_of_composites_holding_a_written_name_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = {"a" => s, "b" => {f: j}};
  j <- |x: i64| x + 1;
  (t{"b"}$.f("s"), j(1))
}"#,
    refused(ONE_INSTANCE_WRITTEN);
    FuseExpect::None
);

run!(
    array_of_tuples_holding_a_written_name_shares_its_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let t = [(s.f, 0), (j, 1)];
  j <- |x: i64| x + 1;
  (t[1]$.0("s"), j(1))
}"#,
    refused(ONE_INSTANCE_WRITTEN);
    FuseExpect::None
);

run!(
    struct_update_holding_a_call_result_shares_its_type,
    r#"{
  let mk = || { let v = never(); |x| { v <- x; v } };
  let s = {f: |x| x, g: |y| y};
  let t = {s with g: mk()};
  (t.g(1), t.g("s"))
}"#,
    prints("[i64:1, i64:1]");
    FuseExpect::None
);

run!(
    struct_update_over_a_name_of_a_call_result_shares_its_type,
    r#"{
  let mk = || { let v = never(); |x| { v <- x; x } };
  let g0 = mk();
  let s = {f: |x| x, g: |y| y};
  let t = {s with g: g0};
  (t.g(1), t.g("s"))
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    struct_update_holding_a_call_result_runs_at_its_type,
    r#"{
  let mk = || { let v = never(); |x| { v <- x; x } };
  let s = {f: |x| x, g: |y| y};
  let t = {s with g: mk()};
  let u = {s with g: mk()};
  (t.g(1), t.g(2), t.f(3), t.f("s"), u.g("t"))
}"#,
    prints(r#"[i64:1, i64:2, i64:3, "s", "t"]"#);
    FuseExpect::None
);

run!(
    array_of_composites_holding_a_call_result_shares_its_type,
    r#"{
  let mk = || { let v = never(); |x| { v <- x; x } };
  let s = {f: |x| x};
  let t = [s, {f: mk()}];
  (t[1]$.f(1), t[1]$.f("s"))
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    holder_of_an_array_holding_a_call_result_shares_its_type,
    r#"{
  let mk = || { let v = never(); |x| { v <- x; x } };
  let s = {f: |x| x};
  let t = {a: [s, {f: mk()}]};
  (t.a[1]$.f(1), t.a[1]$.f("s"))
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    composites_holding_a_call_result_run_at_its_type,
    r#"{
  let mk = || { let v = never(); |x| { v <- x; x } };
  let s = {f: |x| x};
  let t = [s, {f: mk()}];
  let u = {a: [s, {f: mk()}]};
  (t[1]$.f(1), t[1]$.f(2), u.a[1]$.f("s"))
}"#,
    prints(r#"[i64:1, i64:2, "s"]"#);
    FuseExpect::None
);

// Two lambdas no binding names are one union member where their live cells
// pair as a renaming, so a select of composites holding them destructures.
run!(
    select_of_structs_holding_lambdas_destructures,
    r#"{
  let b = true;
  let v = select b { true => {g: |x| x + 1}, false => {g: |y| y * 2} };
  let {g} = v;
  (select v { {g} => g(3) }, g(4))
}"#,
    prints("[i64:4, i64:5]");
    FuseExpect::None
);

run!(
    select_of_tuples_and_arrays_holding_lambdas_destructures,
    r#"{
  let b = true;
  let v = select b { true => (|x| x + 1, 1), false => (|y| y * 2, 2) };
  let w = select b { true => [|x| x + 1], false => [|y| y * 2] };
  (select v { (f, n) => f(n) }, select w { [f] => f(3), _ => 0 })
}"#,
    prints("[i64:2, i64:4]");
    FuseExpect::None
);

run!(
    field_read_of_a_select_of_composites_in_a_definition,
    r#"{
  let k = |v| {
    let s = select v > 0 { true => (|x| x, 1), false => (|y| y, 2) };
    let t = select v > 0 { true => {f: |x| x}, false => {f: |y| y} };
    (s.0(v), t.f(v))
  };
  k(2)
}"#,
    prints("[i64:2, i64:2]");
    FuseExpect::Jit
);

run!(
    field_read_of_a_select_of_differing_lambdas_in_a_definition,
    r#"{
  let k = |v| {
    let t = select v > 0 { true => {f: |x| x}, false => {f: |y| y + 1} };
    let u = select v > 0 { true => (|x| x, 1), false => (|y| y * 3, 2) };
    let w = select v > 0 { true => {g: {f: |x| x}}, false => {g: {f: |y| y + 1}} };
    let a = select v > 0 { true => `A({f: |x| x}), false => `A({f: |y| y + 1}) };
    let g = t.f;
    [t.f(v), u.0(v) + u.1, w.g.f(v), select a { `A(r) => r.f(v) }, g(v), t.f(3)]
  };
  (k(2), k(-2))
}"#,
    prints("[[i64:2, i64:3, i64:2, i64:2, i64:2, i64:3], [i64:-1, i64:-4, i64:-1, i64:-1, i64:-1, i64:4]]");
    FuseExpect::Jit
);

run!(
    field_read_of_a_select_passed_to_a_function_in_a_definition,
    r#"{
  let k = |v| {
    let t = select v > 0 { true => {f: |x| x}, false => {f: |y| y + 1} };
    array::map([v, v + 1], t.f)
  };
  (k(2), k(-2))
}"#,
    prints("[[i64:2, i64:3], [i64:-1, i64:0]]");
    FuseExpect::Jit
);

run!(
    select_of_lambdas_in_a_definition_called_at_both_branches,
    r#"{
  let k = |v| {
    let s = select v > 0 { true => |x| x, false => |y| y };
    let t = select v > 0 { true => |x| x, false => |y| y + 1 };
    (s(v), t(v), array::map([v], t))
  };
  (k(2), k(-2))
}"#,
    prints("[[i64:2, i64:2, [i64:2]], [i64:-2, i64:-1, [i64:-1]]]");
    FuseExpect::Jit
);

// Three structs whose `f` a field read meets: `x`, the `i64` function and
// the string function, standing in the union in the order of `fields`.
fn field_read_beside_two(x: &str, [a, b, c]: [&str; 3]) -> String {
    format!(
        r#"{{
  let k = |b| {{
    let t = select b {{
      0 => {{f: {x}, {a}: 1}},
      1 => {{f: |y| y + 1, {b}: 1}},
      _ => {{f: |z| "[z]!", {c}: 1}}
    }};
    let u = t.f;
    select t {{ {{f, {a}: _}} => f("s"), _ => "no" }}
  }};
  k(0)
}}"#
    )
}

const MEMBER_ORDERS: [[&str; 3]; 6] = [
    ["a", "b", "c"],
    ["a", "c", "b"],
    ["b", "a", "c"],
    ["b", "c", "a"],
    ["c", "a", "b"],
    ["c", "b", "a"],
];

// A field read over three structs holds each lambda as its own member,
// wherever the three stand in the union: the arm that names the first
// struct's field calls its lambda, and a call through the whole read meets
// every member.
// CR claude for claude: [test-gap/low] [test-gap/low] This test runs each order through
// testing::refusal, and so do field_read_refuses_a_capture_two_members_could_decide,
// literal_refuses_a_capture_two_functions_could_decide_in_every_order and
// field_read_names_the_two_functions_that_are_not_one_type;
// literal_of_schemes_beside_two_functions_in_every_order uses testing::eval. Both
// helpers build in Mode::Jit and never run the check, so only the one order that is
// also a run! fixture is held to the rule that the check refuses exactly when the build
// does. A check that disagreed with the build in another order would pass here. Use
// testing::eval_modes in these loops: it holds the check to the build in Interp, and
// the assertions stay the same. The check agrees in every order today (all 108
// generated programs run with --check). (tests-types-d-09)
// 2026-11-21 claude: Renamed and moved to testing::eval_modes:
// field_read_refuses_a_lambda_two_members_could_decide is
// field_read_of_a_lambda_beside_two_functions_in_every_order,
// field_read_refuses_a_capture_two_members_could_decide is
// field_read_leaves_a_capture_to_its_call_in_every_order,
// literal_refuses_a_capture_two_functions_could_decide_in_every_order is
// literal_holds_a_capture_beside_two_functions_in_every_order,
// field_read_names_the_two_functions_that_are_not_one_type is
// field_read_holds_two_open_lambdas_beside_two_functions_in_every_order. Each loop now
// holds the check to the build in Interp;
// literal_of_schemes_beside_two_functions_in_every_order still uses testing::eval, so
// the CR stands for it.
#[tokio::test(flavor = "current_thread")]
async fn field_read_of_a_lambda_beside_two_functions_in_every_order() -> Result<()> {
    for order in MEMBER_ORDERS {
        let v = eval_modes(&field_read_beside_two("|x| x", order), crate::TEST_REGISTER)
            .await?
            .map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::String("s".into()), "{order:?}");
    }
    Ok(())
}

run!(
    field_read_call_through_three_members_refused,
    r#"{
  let k = |b| {
    let t = select b {
      0 => {f: |x| x, a: 1},
      1 => {f: |y| y + 1, b: 1},
      _ => {f: |z| "[z]!", c: 1}
    };
    let u = t.f;
    u("s")
  };
  k(0)
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    field_read_of_a_lambda_beside_two_functions,
    field_read_beside_two("|x| x", ["a", "b", "c"]),
    prints("\"s\"");
    FuseExpect::None
);

run!(
    field_read_of_a_lambda_beside_two_functions_exchanged,
    field_read_beside_two("|x| x", ["a", "c", "b"]),
    prints("\"s\"");
    FuseExpect::None
);

run!(
    field_read_of_a_written_lambda_beside_two_functions,
    field_read_beside_two("|x: string| x", ["a", "b", "c"]),
    prints("\"s\"");
    FuseExpect::None
);

run!(
    field_read_of_a_written_lambda_beside_two_functions_exchanged,
    field_read_beside_two("|x: string| x", ["a", "c", "b"]),
    prints("\"s\"");
    FuseExpect::None
);

// A select's arms and a read of a generic binding among them meet alike.
run!(
    select_of_three_lambdas_stands_uncalled,
    r#"{
  let k = |b| {
    let u = select b { 0 => |x| x, 1 => |y| y + 1, _ => |z| "[z]!" };
    0
  };
  k(0)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    select_of_a_generic_function_beside_two_lambdas_stands_uncalled,
    r#"{
  let id = |x| x;
  let k = |b| {
    let u = select b { 0 => |z| "[z]!", 1 => id, _ => |y| y + 1 };
    0
  };
  k(0)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

// What a function throws tells two functions apart as a parameter does:
// one that throws nothing beside two that throw two errors is refused, and
// runs once it says which it throws.
fn select_beside_two_errors(x: &str) -> String {
    format!(
        r#"{{
  let k = |b| {{
    let t = select b {{
      0 => {{f: {x}, a: 1}},
      1 => {{f: |c: i64| -> i64 error(`B)?, b: 1}},
      _ => {{f: |d: i64| -> i64 error(`A)?, c: 1}}
    }};
    let u = t.f;
    select t {{ {{f, a: _}} => f(1), _ => 0 }}
  }};
  k(0)
}}"#
    )
}

run!(
    field_read_of_a_lambda_beside_two_functions_that_throw,
    select_beside_two_errors("|b: i64| b + 1"),
    prints("i64:2");
    FuseExpect::None
);

run!(
    field_read_of_a_lambda_that_writes_what_it_throws,
    select_beside_two_errors("|b: i64| -> i64 throws `B b + 1"),
    is(Value::I64(2));
    FuseExpect::None
);

// A lambda of a generic holder that reads a binding of its definition leaves
// the binding to its call: nothing the other members hold decides it.
fn element_read_beside_two([a, b, c]: [&str; 3]) -> String {
    format!(
        r#"{{
  let k = |v| {{
    let t = [
      {{f: |x| (x, v), {a}: 1}},
      {{f: |y: i64| (y, 1), {b}: 1}},
      {{f: |z: i64| (z, "s"), {c}: 1}}
    ];
    let u = t[0]$.f;
    v
  }};
  k(1)
}}"#
    )
}

#[tokio::test(flavor = "current_thread")]
async fn field_read_leaves_a_capture_to_its_call_in_every_order() -> Result<()> {
    for order in MEMBER_ORDERS {
        let v = eval_modes(&element_read_beside_two(order), crate::TEST_REGISTER)
            .await?
            .map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(1), "{order:?}");
    }
    Ok(())
}

run!(
    field_read_call_reads_the_union_of_the_members_results,
    r#"{
  let k = |v| {
    let t = [
      {f: |x| (x, v), a: 1},
      {f: |y: i64| (y, 1), b: 1},
      {f: |z: i64| (z, "s"), c: 1}
    ];
    let u = t[0]$.f;
    let (p, q) = u(1);
    q + 1
  };
  k(1)
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

// The lambdas of a `let`'s literal are schemes where its elements meet, and
// each takes the first function it meets in the order written, the one an
// earlier element's instance joined too: two of them beside two functions
// that are not one type check in every order of the elements.
fn literal_beside_two(fns: [&str; 4], order: [usize; 4]) -> String {
    let [a, b, c, d] = order.map(|i| fns[i]);
    format!("{{ let k = |b| {{ let t = [{a}, {b}, {c}, {d}]; array::len(t) }}; k(0) }}")
}

const SCHEMES_BESIDE_TWO: [&str; 4] =
    ["|x| x", "|w| w", "|y: i64| y + 1", r#"|z: string| "[z]!""#];

const SILENT_BESIDE_TWO_ERRORS: [&str; 4] = [
    "|x: i64| x",
    "|x: i64| x + 1",
    "|c: i64| -> i64 error(`B)?",
    "|d: i64| -> i64 error(`A)?",
];

fn orders_of_four() -> impl Iterator<Item = [usize; 4]> {
    (0..4usize.pow(4))
        .map(|n| [n % 4, n / 4 % 4, n / 16 % 4, n / 64])
        .filter(|o| (0..4).all(|i| o.contains(&i)))
}

#[tokio::test(flavor = "current_thread")]
async fn literal_of_schemes_beside_two_functions_in_every_order() -> Result<()> {
    use graphix_package_core::testing::eval;
    for order in orders_of_four() {
        for fns in [SCHEMES_BESIDE_TWO, SILENT_BESIDE_TWO_ERRORS] {
            let code = literal_beside_two(fns, order);
            let (v, ctx) = eval(&code, crate::TEST_REGISTER).await?;
            ctx.shutdown().await;
            assert_eq!(v, Value::I64(4), "{code}");
        }
    }
    Ok(())
}

// A literal whose lambda reads `v` leaves `v` to the call of its definition,
// in every order of its elements: the functions it stands beside decide
// nothing.
#[tokio::test(flavor = "current_thread")]
async fn literal_holds_a_capture_beside_two_functions_in_every_order() -> Result<()> {
    let fns = ["|x| x", "|w| { let e = w == v; w }", "|y: i64| y + 1", "|z: string| z"];
    for order in orders_of_four() {
        let [a, b, c, d] = order.map(|i| fns[i]);
        let code =
            format!("{{ let k = |v| {{ let t = [{a}, {b}, {c}, {d}]; v }}; k(1) }}");
        let v =
            eval_modes(&code, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(1), "{order:?}");
    }
    Ok(())
}

run!(
    literal_call_through_an_element_meets_every_function,
    r#"{
  let k = |v| {
    let t = [|x| x, |w| { let e = w == v; w }, |y: i64| y + 1, |z: string| z];
    t[0]$(1)
  };
  k(1)
}"#,
    refused("string does not contain i64");
    FuseExpect::None
);

// A field read holds two open lambdas beside two functions that are not one
// type as four members, wherever the four stand in the union; a call through
// it meets every member.
#[tokio::test(flavor = "current_thread")]
async fn field_read_holds_two_open_lambdas_beside_two_functions_in_every_order()
-> Result<()> {
    for order in orders_of_four() {
        let [a, b, c, d] = order.map(|i| ["a", "b", "c", "d"][i]);
        let code = format!(
            r#"{{
  let k = |b| {{
    let t = select b {{
      0 => {{f: |x| x, {a}: 1}},
      1 => {{f: |w| w, {b}: 1}},
      2 => {{f: |y: i64| y + 1, {c}: 1}},
      _ => {{f: |z: string| "[z]!", {d}: 1}}
    }};
    let u = t.f;
    0
  }};
  k(0)
}}"#
        );
        let v =
            eval_modes(&code, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(0), "{order:?}");
    }
    Ok(())
}

run!(
    field_read_call_through_four_members_refused,
    r#"{
  let k = |b| {
    let t = select b {
      0 => {f: |x| x, a: 1},
      1 => {f: |w| w, b: 1},
      2 => {f: |y: i64| y + 1, c: 1},
      _ => {f: |z: string| "[z]!", d: 1}
    };
    let u = t.f;
    u(1)
  };
  k(0)
}"#,
    refused("string does not contain i64");
    FuseExpect::None
);

run!(
    literal_of_two_schemes_beside_two_functions,
    literal_beside_two(SCHEMES_BESIDE_TWO, [0, 1, 2, 3]),
    is(Value::I64(4));
    FuseExpect::None
);

run!(
    literal_of_two_silent_functions_beside_two_errors,
    literal_beside_two(SILENT_BESIDE_TWO_ERRORS, [0, 1, 2, 3]),
    is(Value::I64(4));
    FuseExpect::None
);

run!(
    select_of_composites_of_lambdas_in_a_definition,
    r#"{
  let k = |v| {
    let s = select v > 0 { true => {f: |x| x}, false => {f: |y| y} };
    let t = select v > 0 { true => {f: |x| x, n: 1}, false => {f: |y| y, n: 2} };
    let u = select v > 0 { true => (|x| x, 1), false => (|y| y, 2) };
    let w = select v > 0 { true => `A({f: |x| x}), false => `A({f: |y| y}) };
    let m = select v > 0 { true => {f: |x| x, n: 1}, false => {f: |y| y, n: "s"} };
    let r = select v { 2 => {f: |x| x}, -2 => {f: |y| y}, _ => {f: |z| z} };
    [
      s.f(v),
      select t { {f, n} => f(v) + n },
      u.0(v),
      select w { `A(q) => q.f(v) },
      m.f(v),
      r.f(v),
      array::fold(array::map([v, v + 1], s.f), 0, |a, x| a + x)
    ]
  };
  (k(2), k(-2))
}"#,
    prints("[[i64:2, i64:3, i64:2, i64:2, i64:2, i64:2, i64:5], [i64:-2, i64:0, i64:-2, i64:-2, i64:-2, i64:-2, i64:-3]]");
    FuseExpect::Jit
);

run!(
    pick_one_of_two_structs_of_lambdas,
    r#"{
  let s = {f: |x| x};
  let k = |b| select b { true => s, false => {f: |y| y} };
  let h = |x| x;
  let pick = |b| select b { true => (h, 1), false => (|y| y, 2) };
  let (f, n) = pick(true);
  (k(true).f(1), k(false).f(2), f(5), n, pick(false).0("s"))
}"#,
    prints(r#"[i64:1, i64:2, i64:5, i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_select_of_a_written_binding_and_a_generic_holder_is_one_type,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let pick = |b| select b { true => s, false => {f: j} };
  j <- |x: i64| x + 1;
  (pick(true).f("s"), pick(false).f(1))
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

run!(
    a_select_of_a_generic_binding_and_its_holder_at_two_types,
    r#"{
  let s = {f: |x| x};
  let j = s.f;
  let pick = |b| select b { true => s, false => {f: j} };
  (pick(true).f("s"), pick(false).f(1))
}"#,
    prints(r#"["s", i64:1]"#);
    FuseExpect::None
);

run!(
    a_select_of_lambdas_over_two_declared_variables_stays_two_members,
    r#"{
  let f = |c: bool, x: 'a, y: 'b| select c { true => {g: |p: 'a| p}, false => {g: |q: 'b| q} };
  f(false, 1, "s").g(2)
}"#,
    prints("i64:2");
    FuseExpect::None
);

run!(
    a_call_through_lambdas_over_two_declared_variables_refused_in_the_definition,
    r#"{
  let f = |c: bool, x: 'a, y: 'b| {
    let s = select c { true => |p: 'a| p, false => |q: 'b| q };
    s(x)
  };
  f(false, 1, "s")
}"#,
    refused("type mismatch 'b: unbound does not contain 'a: unbound");
    FuseExpect::None
);

run!(
    a_select_of_lambdas_over_three_declared_variables_takes_the_argument_in_each,
    r#"{
  let f = |c: i64, x: 'a, y: 'b, z: 'c| select c {
    0 => {g: |p: 'a| p},
    1 => {g: |q: 'b| q},
    _ => {g: |r: 'c| r}
  };
  f(2, 1, "s", 2.0).g(2)
}"#,
    prints("i64:2");
    FuseExpect::None
);

run!(
    a_select_of_lambdas_over_one_written_type_is_one_member,
    r#"{
  let f = |c: bool, x: 'a, y: 'b| select c { true => {g: |p: i64| p}, false => {g: |q: i64| q} };
  (f(false, 1, "s").g(2), f(true, "t", 0).g(3))
}"#,
    prints("[i64:2, i64:3]");
    FuseExpect::None
);

run!(
    a_call_through_a_function_or_null_names_the_union,
    r#"{
  let b = true;
  let f = select b { true => |x: i64| x, false => null };
  f(1)
}"#,
    refused("type mismatch fn(i64) -> i64 does not contain null");
    FuseExpect::None
);

run!(
    a_call_through_a_function_once_its_null_is_selected_off,
    r#"{
  let b = true;
  let f = select b { true => |x: i64| x, false => null };
  select f { null as _ => 0, g => g(1) }
}"#,
    prints("i64:1");
    FuseExpect::None
);

// A read through a union with an open member waits for that member's type,
// as a read through the open cell alone does, wherever the union is made:
// a name or-alternatives share, a select of two values.
run!(
    a_call_through_a_name_alternatives_share_over_open_parts_needs_no_annotation,
    r#"{
  let h = |u| select u { `A(r, n) | `B(r, n) => r(2) };
  h(`A(|x| x, 1))
}"#,
    prints("i64:2");
    FuseExpect::None
);

run!(
    a_call_through_a_name_alternatives_share_over_written_parts,
    r#"{
  let h = |u: [`A(fn(x: i64) -> i64, i64), `B(fn(x: i64) -> i64, i64)]| select u {
    `A(r, n) | `B(r, n) => r(2)
  };
  h(`A(|x| x + 1, 1))
}"#,
    prints("i64:3");
    FuseExpect::None
);

run!(
    a_call_through_a_function_or_an_open_cell_needs_no_annotation,
    r#"{
  let h = |g, b: bool| { let f = select b { true => |x: i64| x + 1, false => g }; f(2) };
  h(|x| x, true)
}"#,
    prints("i64:3");
    FuseExpect::None
);

run!(
    a_call_through_a_function_or_a_written_function,
    r#"{
  let h = |g: fn(x: i64) -> i64, b: bool| {
    let f = select b { true => |x: i64| x + 1, false => g };
    f(2)
  };
  h(|x| x, true)
}"#,
    prints("i64:3");
    FuseExpect::None
);

run!(
    a_field_read_through_a_name_alternatives_share_over_open_parts_needs_no_annotation,
    r#"{
  let h = |u| select u { `A(r, n) | `B(r, n) => r.x };
  h(`A({x: 1}, 1))
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_tuple_read_through_a_select_of_two_open_cells_needs_no_annotation,
    r#"{
  let h = |a, b, c: bool| (select c { true => a, false => b }).0;
  h((1, 2), (3, 4), true)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_tuple_read_through_a_select_of_two_written_tuples,
    r#"{
  let h = |a: (i64, i64), b: (i64, i64), c: bool| (select c { true => a, false => b }).0;
  h((1, 2), (3, 4), true)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// A read through a union of declared variables reads each member through
// its one bound, as a read through one such variable does.
run!(
    a_field_read_through_a_union_of_bounded_variables,
    r#"{
  let f = 'a: {x: i64}, 'b: {x: i64} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x;
  (f(true, {x: 1}, {x: 2}), f(false, {x: 1}, {x: 2}))
}"#,
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

run!(
    a_field_read_through_a_bounded_variable_beside_a_written_struct,
    r#"{
  let f = 'a: {x: i64} |c: bool, a: 'a, b: {x: i64}| (select c { true => a, false => b }).x;
  (f(true, {x: 1}, {x: 2}), f(false, {x: 1}, {x: 2}))
}"#,
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

run!(
    a_tuple_read_through_a_union_of_bounded_variables,
    r#"{
  let f = 'a: (i64, string), 'b: (i64, string) |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).0;
  (f(true, (1, "a"), (2, "b")), f(false, (1, "a"), (2, "b")))
}"#,
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

run!(
    a_field_read_through_bounded_variables_unions_the_fields,
    r#"{
  let f = 'a: {x: i64}, 'b: {x: string} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x;
  (f(true, {x: 1}, {x: "s"}), f(false, {x: 1}, {x: "s"}))
}"#,
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_field_read_through_bounded_variables_with_the_field_at_different_places,
    r#"{
  let f = 'a: {x: i64, z: i64}, 'b: {a: i64, x: i64} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x;
  (f(true, {x: 1, z: 5}, {a: 7, x: 2}), f(false, {x: 1, z: 5}, {a: 7, x: 2}))
}"#,
    prints("[i64:1, i64:2]");
    FuseExpect::None
);

// A read through a variable the definition declares is refused naming its
// declaration, where no annotation of a binding helps.
run!(
    a_call_through_a_declared_variable_names_its_declaration,
    r#"{
  let f = |a: 'a| a(1);
  f(|x| x + 1)
}"#,
    refused_renumbered("type mismatch fn(i64) -> '_0: unbound does not contain 'a: unbound");
    FuseExpect::None
);

run!(
    a_struct_update_through_a_union_of_bounded_variables,
    r#"{
  let f = 'a: {x: i64, z: i64}, 'b: {a: i64, x: i64} |c: bool, a: 'a, b: 'b| {(select c { true => a, false => b }) with x: 9};
  (f(true, {x: 1, z: 5}, {a: 7, x: 2}), f(false, {x: 1, z: 5}, {a: 7, x: 2}))
}"#,
    prints("[[[\"x\", i64:9], [\"z\", i64:5]], [[\"a\", i64:7], [\"x\", i64:9]]]");
    FuseExpect::None
);

run!(
    a_call_through_a_declared_variable_with_a_bound_a_fn_has,
    r#"{
  let f = 'a: fn(x: i64) -> i64 |a: 'a| a(1);
  f(|x| x + 1)
}"#,
    prints("i64:2");
    FuseExpect::Jit
);

run!(
    a_call_through_a_declared_variable_with_a_bound_no_read_takes,
    r#"{
  let f = 'a: Number |a: 'a| a(1);
  f(1)
}"#,
    refused_renumbered("type mismatch fn(i64) -> '_0: unbound does not contain 'a: unbound within Number");
    FuseExpect::None
);

run!(
    a_field_read_through_bounded_variables_needs_the_field_in_each,
    r#"{
  let f = 'a: {x: i64}, 'b: {y: i64} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x;
  f(true, {x: 1}, {y: 2})
}"#,
    refused("{ y: i64 } has no field x");
    FuseExpect::None
);

run!(
    a_call_through_a_select_of_declared_variables_names_the_declaration,
    r#"{
  let f = |c: bool, a: 'a, b: 'b| (select c { true => a, false => b })(1);
  f(true, |x| x, |x| x)
}"#,
    refused_renumbered("type mismatch fn(i64) -> '_0: unbound does not contain 'a: unbound");
    FuseExpect::None
);

run!(
    a_field_read_through_a_variable_no_bound_reads_beside_a_bounded_one_needs_its_type,
    r#"{
  let f = 'a: Number, 'b: {x: i64} |c: bool, a: 'a, b: 'b| (select c { true => a, false => b }).x;
  f(true, 1, {x: 2})
}"#,
    refused("Sint has no field x");
    FuseExpect::None
);

run!(
    a_field_read_through_a_declared_variable_names_its_declaration,
    r#"{
  let f = |a: 'a| a.x;
  f({x: 1})
}"#,
    refused("reading a struct needs its type known here: annotate the binding");
    FuseExpect::None
);

run!(
    a_field_read_through_a_declared_variable_with_a_bound_a_struct_has,
    r#"{
  let f = 'a: {x: i64} |a: 'a| a.x;
  f({x: 1})
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

run!(
    a_tuple_read_through_a_declared_variable_names_its_declaration,
    r#"{
  let f = |a: 'a| a.0;
  f((1, 2))
}"#,
    refused("reading a tuple needs its type known here: annotate the binding");
    FuseExpect::None
);

run!(
    a_tuple_read_through_a_declared_variable_with_a_bound_a_tuple_has,
    r#"{
  let f = 'a: (i64, i64) |a: 'a| a.0;
  f((1, 2))
}"#,
    prints("i64:1");
    FuseExpect::Jit
);

// A member the read refuses decides the refusal, whatever the open one
// would take.
run!(
    a_call_through_a_number_or_an_open_cell_names_the_union,
    r#"{
  let h = |g, b: bool| { let f = select b { true => 1, false => g }; f(2) };
  h(|x| x, true)
}"#,
    refused_renumbered("type mismatch fn(i64) -> '_0: unbound does not contain i64");
    FuseExpect::None
);

run!(
    a_select_of_lambdas_over_one_declared_variable_is_one_member,
    r#"{
  let f = |c: bool, x: 'a, y: 'b| select c { true => {g: |p: 'a| p}, false => {g: |q: 'a| q} };
  (f(false, 1, "s").g(2), f(true, "t", 0).g("u"))
}"#,
    prints(r#"[i64:2, "u"]"#);
    FuseExpect::None
);

// A declared bound that is a union over another of the function's
// variables, met by a type that holds that variable, says nothing of the
// variable (`'c` is in `['c, null]` whatever it is), so the check
// returns: through a generic wrapper, through a trait method, and where
// an argument's undecided element takes the bound's union.
run!(
    wrapper_passing_a_union_over_its_variable_to_a_bound_over_the_callees,
    r#"{
  let g = 'b: ['c, null] |s: 'c, x: 'b| -> 'b x;
  let f = |v: 'a, w: ['a, null]| g(v, w);
  (f(1, 2), f(1, null), f("a", "b"))
}"#,
    prints(r#"[i64:2, null, "b"]"#)
);
run!(
    wrapper_passing_a_union_over_its_variable_to_a_bound_holds_the_variable,
    r#"{
  let g = 'b: ['c, null] |s: 'c, x: 'b| -> 'b x;
  let f = |v: 'a, w: ['a, null]| g(v, w);
  f(1, "s")
}"#,
    prints(r#""s""#);
    FuseExpect::Jit
);
run!(
    wrapper_passing_a_union_over_its_variable_to_a_trait_methods_bound,
    r#"{
  trait T { val m: fn<'b: [self, null]>(self, x: 'b) -> i64 };
  impl T for i64 { let m = |s, x| s };
  let f = 'a: T |v: 'a, w: ['a, null]| T::m(v, w);
  (f(1, null), f(3, 4))
}"#,
    prints("[i64:1, i64:3]")
);
run!(
    null_arm_over_a_union_bound_s_result_is_dead,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x| y;
  let g = 'x: Array<['a, string]> |y: 'x| y;
  let h = 'x: Array<['a, Array<'a>]> |y: 'x| y;
  let u = never();
  let r = f([u]);
  u <- 1;
  let n = select f([1])[0]$ { null as _ => 0, i64 as i => i };
  (n, g([2]), h([[3]]), r)
}"#,
    refused("pattern null will never match: nothing reaches it")
);
run!(
    undecided_elements_under_two_bounds_that_name_each_others_variable,
    r#"{
  let g = 'x: Array<['a, null]>, 'y: Array<['b, string]> |p: 'a, q: 'b, x: 'x, y: 'y| (x, y);
  let u = never();
  let v = never();
  let r = g(u, v, [v], [u]);
  u <- 1;
  v <- 2;
  r
}"#,
    prints("[[i64:2], [i64:1]]")
);

// A wrapper's declared variable inside the value a callee's bound takes is
// proved against the bound and never narrowed by it: the bound's own
// variable takes it, as where the bound's type is written in the
// parameter, and the wrapper's signature keeps the variable as declared.
run!(
    declared_variable_under_a_callees_bound_over_an_undecided_variable,
    r#"{
  let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'b x;
  let h = 'b: {p: ['c, null]} |x: 'b, s: 'c| -> 'c s;
  let f = |v: 'a, w: Array<'a>| (g(w, v), h({p: v}, v));
  (f(1, [2]), f("a", ["b"]), f(null, []))
}"#,
    prints(r#"[[[i64:2], i64:1], [["b"], "a"], [[], null]]"#)
);
run!(
    declared_variable_under_a_callees_bound_keeps_its_declaration,
    r#"{
  let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'c s;
  let f = |w: Array<'a>, s| g(w, s);
  let k: bool = f;
  k
}"#,
    refused_renumbered("type mismatch bool does not contain fn(w: Array<'a: unbound>, s: '_0: unbound within 'c: unbound within 'a: unbound above 'a: unbound) -> 'a: unbound");
    FuseExpect::None
);
run!(
    declared_variable_under_a_callees_bound_that_another_argument_widens,
    r#"{
  let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'b x;
  let f = |w: Array<'a>| g(w, null);
  f([null])
}"#,
    prints("[null]");
    FuseExpect::Jit
);

// (was declared_variable_under_a_callees_bound_that_another_argument_decides_refused):
// the bound's variable takes null beside 'a, which a declared result 'a cannot hold.
run!(
    declared_variable_under_a_callees_bound_returned_beside_null_refused,
    r#"{
  let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'c s;
  let f = |w: Array<'a>| -> 'a g(w, null);
  f([null])
}"#,
    refused("'a: unbound does not contain null: a call picks 'a");
    FuseExpect::None
);
run!(
    declared_variable_under_a_callees_bound_declared_within_it,
    r#"{
  let g = 'b: Array<['c, null]> |x: 'b, s: 'c| -> 'b x;
  let f = 'a: null |w: Array<'a>| g(w, null);
  f([null])
}"#,
    prints("[null]")
);

// The wrapper's variable in a union under such a bound stands below the
// bound's variable, as a decided union does: 'c is `['a, null]`, so what
// the callee returns at 'c holds the null in the wrapper.
run!(
    declared_variable_in_a_union_under_a_callees_bound_is_below_its_variable,
    r#"{
  let g = 'b: {p: ['c, null], q: 'c} |x: 'b| -> 'c {
    let y: {p: ['c, null], q: 'c} = x;
    y.q
  };
  let f = |w: {p: ['a, null], q: ['a, null]}| g(w);
  (f({p: 2, q: null}), f({p: "s", q: "t"}))
}"#,
    prints(r#"[null, "t"]"#)
);
run!(
    declared_variable_in_a_union_under_a_callees_bound_types_its_result_with_the_union,
    r#"{
  let g = 'b: {p: ['c, null], q: 'c} |x: 'b| -> 'c {
    let y: {p: ['c, null], q: 'c} = x;
    y.q
  };
  let f = |w: {p: ['a, null], q: ['a, null]}| g(w);
  f({p: 2, q: null}) + 1
}"#,
    refused("type mismatch i64 does not contain null");
    FuseExpect::None
);

// A bound's unions over its own variable, met by a wrapper's unions over a
// declared variable, put the bound's variable above the declared one: it
// takes the null the second union holds too, as the bound's type written in
// the parameter does.
run!(
    declared_variable_under_two_unions_of_a_callees_bound_stands_below_its_variable,
    r#"{
  let g = 'b: (['c, null], ['c, i64]) |s: 'c, x: 'b| -> 'b x;
  let f = |u, w: (['a, null], ['a, null])| g(u, w);
  (f(1, (2, 2)), f("s", ("t", null)))
}"#,
    prints(r#"[[i64:2, i64:2], ["t", null]]"#)
);
run!(
    declared_variable_under_two_unions_of_a_callees_bound_types_its_wrapper_with_the_union,
    r#"{
  let g = 'b: (['c, null], ['c, i64]) |s: 'c, x: 'b| -> 'b x;
  let f = |u, w: (['a, null], ['a, null])| g(u, w);
  let k: bool = f;
  k
}"#,
    refused_renumbered("type mismatch bool does not contain fn(u: '_0: unbound within 'c: unbound above 'a: unbound | [null, 'a: unbound], w: ([null, 'a: unbound], [null, 'a: unbound])) -> ([null, 'a: unbound], [null, 'a: unbound])");
    FuseExpect::None
);
run!(
    declared_variable_under_two_unions_written_in_the_parameter,
    r#"{
  let g = |s: 'c, x: (['c, null], ['c, i64])| x;
  let f = |u, w: (['a, null], ['a, null])| g(u, w);
  (f(1, (2, 2)), f("s", ("t", null)))
}"#,
    prints(r#"[[i64:2, i64:2], ["t", null]]"#)
);
run!(
    declared_variable_under_two_unions_of_a_callees_bound_keeps_the_bounds_of_its_variable,
    r#"{
  let g = 'b: (['c, null], ['c, i64]), 'c: [string, null, i64] |s: 'c, x: 'b| -> 'b x;
  let f = 'a: Any |u, w: (['a, null], ['a, null])| g(u, w);
  f(1, (2, 2))
}"#,
    refused("type mismatch [i64, null, string] does not contain 'a");
    FuseExpect::None
);
run!(
    bare_variable_under_two_unions_of_a_callees_bound_takes_the_bound,
    r#"{
  let g = 'b: (['c, null], ['c, i64]), 'c: [string, null, i64] |s: 'c, x: 'b| -> 'b x;
  let f = |u, w: (['a, null], ['a, null])| g(u, w);
  f(1, (2, 2))
}"#,
    prints("[i64:2, i64:2]")
);
run!(
    declared_variable_under_two_unions_of_a_callees_bound_declared_within_its_bound,
    r#"{
  let g = 'b: (['c, null], ['c, i64]), 'c: [string, null, i64] |s: 'c, x: 'b| -> 'b x;
  let f = 'a: [string, null] |u, w: (['a, null], ['a, null])| g(u, w);
  f("s", ("t", null))
}"#,
    prints(r#"["t", null]"#)
);

// A chain of bounds that share declared variables, called with arrays: the
// array literals' element cell stands above the variable they are held
// against as it stands, and a variable is proved by its own lower bound
// before it is by a bound of the variable it holds.
run!(
    chain_of_bounds_sharing_a_variable_takes_arrays,
    r#"{
  let f = 'a1: ['a3, Array<'b>], 'a3: ['a4, Array<'a4>], 'a4: ['a5, Array<'b>],
    'a5: Array<i64>, 'b: Number |x: ('a1, 'a3, 'a4, 'a5)| x;
  f(([1], [], [], [1]))
}"#,
    prints(r#"[[i64:1], [], [], [i64:1]]"#)
);
run!(
    chain_of_bounds_sharing_a_variable_takes_a_nested_array,
    r#"{
  let f = 'a1: ['a3, Array<'b>], 'a3: ['a4, Array<'a4>], 'a4: ['a5, Array<'b>],
    'a5: Array<i64>, 'b: Number |x: ('a1, 'a3, 'a4, 'a5)| x;
  f(([1], [], [1], [1]))
}"#,
    prints(r#"[[i64:1], [], [i64:1], [i64:1]]"#)
);

// Two undecided elements of one array under such a bound: each stays
// within the bound's union and the bound's variable is above both, so
// each one's conjunct comes to hold its own cell, and admits whatever
// the cell takes. What else decides the variable still holds them.
run!(
    two_undecided_elements_take_the_union_of_a_bound_over_another_variable,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x| y;
  let g = |u, v| f([u, v]);
  let u = never();
  let v = never();
  let r = f([u, v]);
  u <- 1;
  v <- "s";
  (g(1, 2), g(1, null), g("s", 1), r)
}"#,
    prints(r#"[[i64:1, i64:2], [i64:1, null], ["s", i64:1], [i64:1, "s"]]"#)
);
run!(
    two_undecided_elements_of_a_bound_widen_its_variable,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x| y;
  let u = never();
  let v = never();
  let r = f([u, v]);
  u <- 1;
  v <- "s";
  select r[0]$ { i64 as i => i, null as _ => 0 }
}"#,
    refused("type mismatch [i64, null] does not contain string");
    FuseExpect::None
);
run!(
    two_undecided_elements_under_a_bound_fit_what_decides_its_variable,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x, z: fn(q: 'a) -> i64| z(y[1]$$);
  let u = never();
  let v = never();
  let r = f([u, v], |q: i64| q + 1);
  u <- 1;
  v <- 2;
  r
}"#,
    prints("i64:3")
);
run!(
    two_undecided_elements_under_a_bound_keep_what_decides_its_variable,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x, z: fn(q: 'a) -> i64| z(y[1]$$);
  let u = never();
  let v = never();
  let r = f([u, v], |q: i64| q + 1);
  u <- 1;
  v <- "s";
  r
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

// Behind a typedef the bound's union is over two of the function's
// variables: an element another argument decided goes to its member, and
// one nothing decided is the choice between two undecided members.
run!(
    undecided_element_takes_a_named_union_of_a_bound_over_a_decided_variable,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = 'tx: Array<E<'b, 'a>> |y: 'b, x: 'tx| x;
  f("s", ["t"])
}"#,
    prints(r#"["t"]"#)
);
run!(
    undecided_element_into_a_named_union_of_a_bound_over_two_open_variables,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = 'tx: Array<E<'b, 'a>> |x: 'tx, y: 'b| x;
  f(["t"], "s")
}"#,
    prints(r#"["t"]"#);
    FuseExpect::Jit
);
run!(
    written_element_into_a_named_union_of_a_bound_over_two_open_variables,
    r#"{
  type E<'p, 'q> = ['p, 'q];
  let f = 'tx: Array<E<'b, 'a>> |y: 'b, x: 'tx| x;
  let x: Array<[string, i64]> = ["t"];
  f("s", x)
}"#,
    prints(r#"["t"]"#)
);

// The union such a bound passes down to an undecided parameter is not
// kept as a bound on its own variable: the wrapper's signature is the one
// the union written in the callee's parameter gives.
run!(
    wrapper_of_a_bound_over_another_variable_infers_no_bound_on_that_variable,
    r#"{
  let f = 'x: {v: ['a, null]} |y: 'x| y;
  let g = |u| f({v: u});
  let k: bool = g;
  k
}"#,
    refused_renumbered("type mismatch bool does not contain fn(u: '_0: unbound within [null, 'a: unbound]) -> { v: '_0: unbound within [null, 'a: unbound] }");
    FuseExpect::None
);

// An undecided argument at two variables with disjoint bounds makes them
// one cell under both bounds where it is met, so the argument that would
// decide one of them is refused at the check, never at the instance.
run!(
    undecided_argument_at_two_variables_of_disjoint_bounds_refused,
    r#"{
  let f = 'b: Number, 'c: string |x0: 'c, x1: 'b, x2: 'b| (x0, x1, x2);
  let h = |v| f(v, v, 1);
  h(2)
}"#,
    refused("a value here is used as Number and as string, and no value is both: nothing can be passed for it");
    FuseExpect::None
);

run!(
    undecided_binding_at_two_variables_of_disjoint_bounds_refused,
    r#"{
  let f = 'b: Number, 'c: string |x0: 'c, x1: 'b, x2: 'b| (x0, x1, x2);
  let v = never();
  let r = f(v, v, 1);
  r
}"#,
    refused("unsatisfiable constraints: string & i64");
    FuseExpect::None
);

// A wrapper that declares its callee's chain of bounds through a mutually
// recursive typedef, called with an undecided argument at the chain's
// first link: the argument takes the formal where it is met, and the
// check returns.
run!(
    wrapper_of_bounds_through_a_recursive_typedef_takes_an_undecided_argument,
    r#"{
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  let f = 'a0: Array<G<'a2, 'a1>>, 'a1: Array<G<'a3, 'a2>> |x0: 'a0, x1: 'a1| x0;
  let h = 'b0: Array<G<'b2, 'b1>>, 'b1: Array<G<'b3, 'b2>> |y0: 'b0, y1: 'b1| f(y0, y1);
  (h([], [`X(1)]), h([], []))
}"#,
    prints("[[], []]")
);

// A bound's parts reach the open cells of what its variable takes through
// the names the value holds and the bound's own decided variables. A
// wrapper that declares its callee's chain of bounds and leaves a
// parameter the chain names inferred stands within both chains, so the
// variable its own chain holds, which no parameter does, takes what the
// callee's took, and the instance holds every bound.
run!(
    partial_wrapper_bound_reaches_a_variable_under_a_recursive_typedef,
    r#"{
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a2: Array<G<'a4, 'a3>>, 'a3: Q<'a4, 'a4>
    |x2: 'a2, x3: 'a3, x4: 'a4| (x2, x3, x4);
  let h = 'b2: Array<G<'b4, 'b3>>, 'b3: Q<'b4, 'b4>
    |y4: 'b4, y3, y2: 'b2| f(y2, y3, y4);
  h("s", `B("t"), [`X("u")])
}"#,
    prints(r#"[[["X", "u"]], ["B", "t"], "s"]"#)
);

run!(
    partial_wrapper_bound_reaches_a_variable_under_a_recursive_typedef_empty,
    r#"{
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a2: Array<G<'a4, 'a3>>, 'a3: Q<'a4, 'a4>
    |x2: 'a2, x3: 'a3, x4: 'a4| (x2, x3, x4);
  let h = 'b2: Array<G<'b4, 'b3>>, 'b3: Q<'b4, 'b4>
    |y4: 'b4, y3, y2: 'b2| f(y2, y3, y4);
  h("s", `B("t"), [])
}"#,
    prints(r#"[[], ["B", "t"], "s"]"#)
);

run!(
    partial_wrapper_bound_under_a_recursive_typedef_holds_another_variant,
    r#"{
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  type Q<'x, 'y> = [`A('x), `B('y)];
  let f = 'a2: Array<G<'a4, 'a3>>, 'a3: Q<'a4, 'a4>
    |x2: 'a2, x3: 'a3, x4: 'a4| (x2, x3, x4);
  let h = 'b2: Array<G<'b4, 'b3>>, 'b3: Q<'b4, 'b4>
    |y4: 'b4, y3, y2: 'b2| f(y2, y3, y4);
  h("s", `B("t"), [`Y(`Z(`A("q")))])
}"#,
    prints(r#"[[["Y", ["Z", ["A", "q"]]]], ["B", "t"], "s"]"#);
    FuseExpect::Jit
);

run!(
    partial_wrapper_bound_reaches_the_callees_instance_under_a_recursive_typedef,
    r#"{
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  let f = 'a0: Array<G<'a2, 'a1>>, 'a2: Array<G<'a4, 'a3>>, 'a4: [i64, null],
    'a3: Array<G<'a4, 'a4>>, 'a1: Array<G<'a3, 'a2>>
    |x0: 'a0, x1: 'a1, x2: 'a2, x3: 'a3, x4: 'a4| [x4];
  let h = 'b2: Array<G<'b4, 'b3>>, 'b4: [i64, null], 'b3: Array<G<'b4, 'b4>>,
    'b1: Array<G<'b3, 'b2>>
    |y3: 'b3, y2, y0, y1: 'b1, y4: 'b4| f(y0, y1, y2, y3, y4);
  h([`X(null)], [`Y(`Z([`X(null)]))], [], [], null)
}"#,
    prints("[null]")
);

// A variable only the bounds of the parameters hold takes no argument wider
// than the first that reached it through a bound: the refusal names the
// annotation, and the argument annotated with a type holding both runs.
run!(
    bound_held_variable_takes_the_wider_argument_after_the_narrower,
    r#"{
  let f = 'a1: Array<'a3>, 'a2: Array<'a3> |x1: 'a1, x2: 'a2| 7;
  let u: Array<i64> = [1];
  let v: Array<[i64, null]> = [1];
  f(u, v)
}"#,
    prints("i64:7");
    FuseExpect::Jit
);

run!(
    bound_held_variable_takes_the_annotated_argument,
    r#"{
  let f = 'a1: Array<'a3>, 'a2: Array<'a3> |x1: 'a1, x2: 'a2| 7;
  let u: Array<[i64, null]> = [1];
  let v: Array<[i64, null]> = [1];
  f(u, v)
}"#,
    prints("i64:7")
);

run!(
    bound_held_variable_takes_the_narrower_argument_after_the_wider,
    r#"{
  let f = 'a1: Array<'a3>, 'a2: Array<'a3> |x1: 'a1, x2: 'a2| 7;
  let u: Array<i64> = [1];
  let v: Array<[i64, null]> = [1];
  f(v, u)
}"#,
    prints("i64:7")
);

run!(
    bound_held_variable_through_a_wrapper_takes_the_wider_argument_after_the_narrower,
    r#"{
  let f = 'a1: Array<'c>, 'a2: Array<'c> |x1: 'a1, x2: 'a2| 7;
  let h = 'b1: Array<'c>, 'b2: Array<'c> |y1: 'b1, y2: 'b2| f(y1, y2);
  let u: Array<i64> = [1];
  let v: Array<[i64, null]> = [1];
  h(u, v)
}"#,
    prints("i64:7");
    FuseExpect::Jit
);

run!(
    bound_held_variable_two_deep_takes_the_wider_argument_after_the_narrower,
    r#"{
  let f = 'm: Array<'c>, 'a1: 'm, 'a2: Array<'m> |x1: 'a1, x2: 'a2| 7;
  let u: Array<i64> = [1];
  let v: Array<Array<[i64, null]>> = [[1]];
  f(u, v)
}"#,
    prints("i64:7");
    FuseExpect::Jit
);

run!(
    bound_held_variable_two_deep_takes_the_annotated_argument,
    r#"{
  let f = 'm: Array<'c>, 'a1: 'm, 'a2: Array<'m> |x1: 'a1, x2: 'a2| 7;
  let u: Array<[i64, null]> = [1];
  let v: Array<Array<[i64, null]>> = [[1]];
  f(u, v)
}"#,
    prints("i64:7")
);

// An undecided value in an argument that a declared union bound meets is
// decided by the bound before the argument is checked: the refusal names the
// value, and the value annotated runs.
run!(
    undecided_value_under_a_union_bound_takes_its_use,
    r#"{
  let f = 'x: (['a, null], i64) |y: 'x| y;
  let g = |u| f((u, u + 1));
  g(1)
}"#,
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

run!(
    undecided_value_decided_early_by_a_union_bound_takes_its_annotation,
    r#"{
  let f = 'x: (['a, null], i64) |y: 'x| y;
  let g = |u: i64| f((u, u + 1));
  g(1)
}"#,
    prints("[i64:1, i64:2]")
);

run!(
    undecided_value_in_a_struct_under_a_union_bound_takes_its_use,
    r#"{
  let f = 'x: {p: ['a, null], q: i64} |y: 'x| y;
  let g = |u| f({p: u, q: u + 1});
  g(1)
}"#,
    prints(r#"[["p", i64:1], ["q", i64:2]]"#);
    FuseExpect::Jit
);

// A refusal that arose in an expression reading no such value is of another
// cause, and names none.
run!(
    refusal_beside_a_value_decided_early_names_no_value,
    r#"{
  let f = 'x: (['a, null], i64) |y: 'x| y;
  let g = |u| f((u, 1 + "a"));
  g(1)
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

// A union bound `['a4, Array<'a4>]` takes an array literal whose elements
// are not yet checked through its structural member, so a literal of the
// bounded variable's own element type is refused at its element; the value
// bound to a `let` with its type written takes the variable's member.
run!(
    chain_bound_array_literal_takes_the_variable_member,
    r#"{
  let f = 'a3: ['a4, Array<'a4>], 'a4: ['a5, Array<'b>], 'a5: Array<i64>, 'b: Number
    |x: ('a3, 'a4, 'a5)| x;
  f(([1], [], [1]))
}"#,
    prints("[[i64:1], [], [i64:1]]");
    FuseExpect::Jit
);

run!(
    chain_bound_array_value_annotated_takes_the_variable_member,
    r#"{
  let f = 'a3: ['a4, Array<'a4>], 'a4: ['a5, Array<'b>], 'a5: Array<i64>, 'b: Number
    |x: ('a3, 'a4, 'a5)| x;
  let a: Array<i64> = [1];
  f((a, [], [1]))
}"#,
    prints("[[i64:1], [], [i64:1]]")
);

run!(
    chain_bound_nested_array_literal_takes_the_structural_member,
    r#"{
  let f = 'a3: ['a4, Array<'a4>], 'a4: Array<i64> |x: ('a3, 'a4)| x;
  f(([[1]], [1]))
}"#,
    prints("[[[i64:1]], [i64:1]]")
);

run!(
    partial_wrapper_bound_reaches_an_element_through_a_decided_variable,
    r#"{
  let f = 'a2: ('a3, i64) |x3: 'a3, x2: 'a2| (x2, x3);
  let h = 'b2: ('b3, i64), 'b3: Array<[i64, string, null]> |y3, y2: 'b2| f(y3, y2);
  let v: Array<[i64, null]> = [1];
  h(v, ([], 1))
}"#,
    prints("[[[], i64:1], [i64:1]]")
);

run!(
    partial_wrapper_bound_through_a_decided_variable_holds_another_element,
    r#"{
  let f = 'a2: ('a3, i64) |x3: 'a3, x2: 'a2| (x2, x3);
  let h = 'b2: ('b3, i64), 'b3: Array<[i64, string, null]> |y3, y2: 'b2| f(y3, y2);
  let v: Array<[i64, null]> = [1];
  h(v, (["s"], 1))
}"#,
    prints(r#"[[["s"], i64:1], [i64:1]]"#);
    FuseExpect::Jit
);

// Beside conjunct-lower-cycle-settle-01: a wrapper over a bound that is a
// name applied over a union of its variable, given a value that leaves
// that variable undecided, runs where it declares the callee's own bound
// on the variable, where the argument's type is written, and where the
// value decides the variable.
run!(
    partial_wrapper_bound_under_a_name_leaves_its_variable_undecided,
    r#"{
  type O<'t> = ['t, null];
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a3: T<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let whole = 'b3: T<O<'b4>>, 'b4: [i64, null] |y3: 'b3| f(y3);
  let v: T<null> = `Node([`Leaf(null)]);
  (whole(`Node([`Leaf(null)])), whole(`Node([])), whole(v), whole(`Node([`Leaf(1)])))
}"#,
    prints("[i64:7, i64:7, i64:7, i64:7]")
);

run!(
    partial_wrapper_bound_wider_than_the_callees_is_refused,
    r#"{
  type O<'t> = ['t, null];
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a3: T<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let wide = 'c3: T<O<'c4>>, 'c4: [i64, null, string] |y3: 'c3| f(y3);
  wide(`Node([`Leaf(1)]))
}"#,
    refused("type mismatch T<O<'a4>> does not contain 'c3");
    FuseExpect::None
);

run!(
    partial_wrapper_bound_over_a_union_of_its_variable_takes_a_value_that_decides_nothing,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: Array<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let h = 'b3: Array<O<'b4>>, 'b4: [i64, null] |y3: 'b3| f(y3);
  (h([null]), h([]), h([1]))
}"#,
    prints("[i64:7, i64:7, i64:7]")
);

run!(
    partial_wrapper_bound_over_a_union_of_its_variable_needs_the_callees_bound_declared,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: Array<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let h = 'b3: Array<O<'b4>> |y3: 'b3| f(y3);
  h([1])
}"#,
    refused("type mismatch Array<O<'a4: [i64, null]>> does not contain 'b3: unbound within Array<O<'b4: unbound>>");
    FuseExpect::None
);

run!(
    partial_wrapper_bound_over_a_union_of_its_variable_declared_wider_is_refused,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: Array<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let w = 'c3: Array<O<'c4>>, 'c4: [i64, null, string] |y3: 'c3| f(y3);
  w([1])
}"#,
    refused("type mismatch Array<O<'a4: [i64, null]>> does not contain 'c3: unbound within Array<O<'c4: unbound within [i64, null, string]>>");
    FuseExpect::None
);

run!(
    partial_wrapper_bound_under_a_name_over_a_union_takes_a_value_that_decides_nothing,
    r#"{
  type O<'t> = ['t, null];
  type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)];
  let f = 'a3: T<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let h = 'b3: T<O<'b4>>, 'b4: [i64, null] |y3: 'b3| f(y3);
  (h(`Node([`Leaf(null)])), h(`Node([])))
}"#,
    prints("[i64:7, i64:7]")
);

run!(
    partial_wrapper_bound_over_a_union_of_its_variable_refuses_a_member_the_callee_does_not_hold,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: Array<O<'a4>>, 'a4: [i64, null] |x3: 'a3| 7;
  let h = 'b3: Array<O<'b4>>, 'b4: [i64, null] |y3: 'b3| f(y3);
  h(["s"])
}"#,
    refused("type mismatch O<'b4> does not contain string");
    FuseExpect::None
);

// A call's last settle takes a variable that stands above an argument
// before the variables its bound holds, so each link of a chain of bounds
// takes its part of the argument: a `never()` at the chain's last
// variable, or a variable no argument holds, is not settled to bottom
// under it.
run!(
    never_under_a_union_bound_takes_the_holders_argument,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: Array<'a5>, 'a5: string |x5: 'a5, x3: 'a3| x3;
  f(never(), ["s"])
}"#,
    prints("[\"s\"]");
    FuseExpect::None
);

run!(
    never_under_a_union_bound_takes_the_holders_argument_holder_first,
    r#"{
  let f = 'a3: ['a4, null], 'a4: Array<'a5>, 'a5: string |x3: 'a3, x5: 'a5| x3;
  f(["s"], never())
}"#,
    prints("[\"s\"]");
    FuseExpect::None
);

run!(
    never_under_a_union_bound_takes_the_holders_argument_through_wrappers,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a2: O<'a3>, 'a3: O<'a4>, 'a4: Array<'a5>, 'a5: string |x5: 'a5, x2: 'a2| x2;
  let g = |a, b| f(a, b);
  let h = |a, b| g(a, b);
  (h(never(), ["s"]), h("u", ["t"]))
}"#,
    prints("[[\"s\"], [\"t\"]]")
);

run!(
    never_under_a_union_bound_refuses_what_the_chain_does_not_hold,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: Array<'a5>, 'a5: string |x5: 'a5, x3: 'a3| x3;
  f(never(), [1])
}"#,
    refused_renumbered("type mismatch O<'a4> does not contain Array<'_0>");
    FuseExpect::None
);

run!(
    undecided_under_a_union_bound_written_another_type_holds_both,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: Array<'a5>, 'a5: [string, i64] |x5: 'a5, x3: 'a3| (x3, x5);
  let n = never();
  let r = f(n, ["s"]);
  n <- 1;
  r
}"#,
    prints(r#"[["s"], i64:1]"#);
    FuseExpect::Jit
);

run!(
    undecided_under_a_union_bound_written_the_holders_type,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: Array<'a5>, 'a5: [string, i64] |x5: 'a5, x3: 'a3| (x3, x5);
  let n = never();
  let r = f(n, ["s"]);
  n <- "t";
  r
}"#,
    prints("[[\"s\"], \"t\"]")
);

run!(
    unheld_variables_under_a_union_bound_chain_take_the_argument,
    r#"{
  let f = 'a0: ['a1, string], 'a1: ['a2, string], 'a2: ['a3, string] |x0: 'a0| x0;
  f(duration:1.s)
}"#,
    prints("duration:1.s")
);

run!(
    unheld_variable_under_a_union_bound_holds_a_widened_argument,
    r#"{
  let f = 'a0: ['a1, string], 'a1: ['a2, string] |x0: 'a0, x1: 'a0| (x0, x1);
  let w0: [string, duration] = duration:1.s;
  (f("s", w0), f(w0, "s"))
}"#,
    prints("[[\"s\", duration:1.s], [duration:1.s, \"s\"]]")
);

run!(
    unheld_variable_under_a_structured_bound_holds_a_widened_argument,
    r#"{
  let f = 'a0: ['a1, string], 'a1: [Array<'a2>, null] |x0: 'a0, x1: 'a0| (x0, x1);
  let g = 'b0: ['b1, string], 'b1: Array<'b2> |x0: 'b0, x1: 'b0| (x0, x1);
  let w0: [string, Array<i64>] = [1];
  (f("s", w0), g("s", w0), f(w0, "s"))
}"#,
    prints("[[\"s\", [i64:1]], [\"s\", [i64:1]], [[i64:1], \"s\"]]")
);

run!(
    never_under_a_union_bound_chain_through_a_struct,
    r#"{
  let f = 'a0: ['a1, string], 'a1: {v: 'a2}, 'a2: ['a3, null], 'a3: Array<'a4>
    |x0: 'a0, x4: 'a4| x0;
  f({v: [1]}, never()).v
}"#,
    prints("[i64:1]");
    FuseExpect::None
);

// A variable no argument holds whose bound is a closed type settles to it
// before the variable that holds it, which then tells the members of its
// own bound apart; with a `never()` at it the members stay undecided, and
// the one that holds the other takes the argument and decides it.
run!(
    closed_bound_under_a_union_bound_settles_before_its_holder,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: [Array<'a5>, Array<O<'a5>>], 'a5: string |x3: 'a3| x3;
  f(["s"])
}"#,
    prints("[\"s\"]")
);

run!(
    never_under_a_union_bound_of_two_arrays_goes_to_the_member_holding_the_other,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: [Array<'a5>, Array<O<'a5>>], 'a5: string
    |x3: 'a3, x5: 'a5| x3;
  f(["s"], never())
}"#,
    prints("[\"s\"]");
    FuseExpect::None
);

run!(
    never_under_a_union_bound_of_two_arrays_written_another_type_refused,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: [Array<'a5>, Array<O<'a5>>], 'a5: string
    |x3: 'a3, x5: 'a5| x3;
  let v = never();
  let r = f(["s"], v);
  v <- 1;
  r
}"#,
    refused("type mismatch string does not contain i64");
    FuseExpect::None
);

run!(
    typed_never_under_a_union_bound_of_two_arrays,
    r#"{
  type O<'t> = ['t, null];
  let f = 'a3: O<'a4>, 'a4: [Array<'a5>, Array<O<'a5>>], 'a5: string
    |x3: 'a3, x5: 'a5| x3;
  f(["s"], never<string>())
}"#,
    prints("[\"s\"]")
);

// A variable whose bound is closed once the variables it holds have
// settled is no variable that waits: it settles before the one that holds
// it, as a chain of bounds nothing was produced into always did.
run!(
    bound_closed_by_its_own_chain_settles_before_its_holder,
    r#"{
  let f = 'a0: ('a1, 'a1), 'a1: ['a2, bool], 'a2: [('a3, 'a3), bool],
    'a3: [('a4, bool), bool], 'a4: i64 |x0: Array<'a0>, x1: 'a1| x1;
  f([never()], ((1, true), true))
}"#,
    prints("[[i64:1, true], true]")
);

// A cell of an argument that a bound's part holding an open cell meets is
// related to that cell by the bound's own containment alone, so the same
// cell bare and under a constructor both stand below the bound's
// variable: `[u, [u]]` under `'x: Array<['a, null]>`.
run!(
    undecided_value_bare_and_in_an_array_under_a_union_bound,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x| y;
  let g = |u| f([u, [u]]);
  (g(1), g("s"))
}"#,
    prints(r#"[[i64:1, [i64:1]], ["s", ["s"]]]"#)
);
run!(
    undecided_binding_bare_and_in_a_tuple_under_a_union_bound,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x| y;
  let u = never();
  let r = f([u, (u, 1)]);
  u <- 1;
  r
}"#,
    prints("[i64:1, [i64:1, i64:1]]")
);
run!(
    undecided_array_of_a_let_bare_and_in_an_array_under_a_union_bound,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x| y;
  let g = |u| { let v = [u, [u]]; f(v) };
  g(1)
}"#,
    prints("[i64:1, [i64:1]]")
);
run!(
    undecided_value_under_a_union_bound_stands_below_its_variable,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x, z: 'a| z;
  let g = |u, w| f([u, [u]], w);
  let w: [i64, Array<i64>] = 1;
  g(1, w)
}"#,
    is(Value::I64(1))
);
run!(
    undecided_value_under_a_union_bound_and_another_argument_widen_its_variable,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x, z: 'a| z;
  let g = |u, w| f([u, [u]], w);
  g(1, "s")
}"#,
    prints(r#""s""#);
    FuseExpect::Jit
);
run!(
    undecided_value_under_a_union_bound_takes_what_a_later_callback_decides,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x, k: fn(a: 'a) -> i64|
    select y[1]$ { null as _ => 0, v => k(v) };
  let g = |u| f([u, [u]], |a: [i64, Array<i64>]| select a {
    i64 as i => i,
    Array<i64> as a => a[0]$ + 100
  });
  g(1)
}"#,
    is(Value::I64(101))
);
run!(
    undecided_value_under_a_union_bound_takes_what_a_later_callback_decides_refused,
    r#"{
  let f = 'x: Array<['a, null]> |y: 'x, k: fn(a: 'a) -> i64|
    select y[1]$ { null as _ => 0, v => k(v) };
  let g = |u| f([u, [u]], |a: [i64, Array<i64>]| select a {
    i64 as i => i,
    Array<i64> as a => a[0]$ + 100
  });
  g("s")
}"#,
    refused("i64 does not contain string");
    FuseExpect::None
);

// The copy of a formal that an argument pre-unifies with opens its
// widenable variables above no lower bound, as it opens a decided one: a
// literal narrower than what a bound's holder put the variable above is
// checked on its own, and meets the variable with its bound after.
run!(
    literal_narrower_than_what_a_bounds_holder_put_its_variable_above,
    r#"{
  let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>| x2;
  let w0: [null, string] = null;
  let r = f(w0, ["s"]);
  select r[0]$ { null as _ => "n", string as s => s }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s")
);
run!(
    literal_narrower_than_what_a_bounds_holder_put_its_variable_above_nested,
    r#"{
  let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<Array<'a2>>| x2;
  let w0: [null, string] = null;
  let r = f(w0, [["s"], []]);
  select r[0]$[0]$ { null as _ => "n", string as s => s }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s")
);
run!(
    literal_narrower_than_what_a_bounds_holder_put_a_one_instance_variable_above,
    r#"{
  let mk = |z| 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>| x2;
  let h = mk(0);
  let w0: [null, string] = null;
  let r = h(w0, ["s"]);
  select r[0]$ { null as _ => "n", string as s => s }
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s")
);
run!(
    undecided_value_after_a_literal_narrower_than_what_bounds_it,
    r#"{
  let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>, x3: Array<'a2>| (x2, x3);
  let w0: [null, string] = null;
  let g = |u| f(w0, ["s"], [u]);
  g(null)
}"#,
    prints(r#"[["s"], [null]]"#)
);
run!(
    undecided_value_after_a_literal_narrower_than_what_bounds_it_widens_the_variable,
    r#"{
  let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>, x3: Array<'a2>| (x2, x3);
  let w0: [null, string] = null;
  let g = |u| f(w0, ["s"], [u]);
  g(1)
}"#,
    prints(r#"[["s"], [i64:1]]"#);
    FuseExpect::Jit
);
run!(
    literal_narrower_than_what_a_chain_of_bounds_put_its_variable_above,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration], 'a2: ['a3, null], 'a3: [i64, string]
    |x0: 'a0, x1: 'a1, x2: Array<'a2>| -> 'a0 x0;
  let w0: [null, string] = null;
  f(w0, "s", ["s"])
}"#,
    is(Value::Null)
);
run!(
    literal_narrower_than_what_a_union_bounds_variable_member_took,
    r#"{
  let f = 'a: [Array<i64>, 'b] |v: 'a, y: 'b| v;
  let x: Array<[i64, string]> = ["z"];
  f(x, ["s"])
}"#,
    prints(r#"["z"]"#)
);
run!(
    literal_outside_what_a_bounds_holder_put_its_variable_above_widens_it,
    r#"{
  let f = 'a1: ['a2, duration] |x0: 'a1, x2: Array<'a2>| x2;
  let w0: [null, string] = null;
  f(w0, [1])
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);
run!(
    literal_outside_what_a_union_bounds_variable_member_took_widens_it,
    r#"{
  let f = 'a: [Array<i64>, 'b] |v: 'a, y: 'b| y;
  let x: Array<[i64, string]> = ["z"];
  f(x, [1.5])
}"#,
    prints("[f64:1.5]");
    FuseExpect::Jit
);

// A bound that names a typedef holds the name's parameter to the parameter's
// bound in the signature that writes it, where no argument has met it.
run!(
    bound_name_parameter_beside_an_argument_breaking_its_bound_refused,
    r#"{
  type N<'t: Number> = ['t, null];
  let f = 'a: N<'b> |x: 'b, y: Array<'a>| x;
  f("s", [])
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);
run!(
    bound_name_parameter_beside_an_argument_within_its_bound,
    r#"{
  type N<'t: Number> = ['t, null];
  let f = 'a: N<'b> |x: 'b, y: Array<'a>| x;
  f(1, [])
}"#,
    is(Value::I64(1))
);
run!(
    bound_name_parameter_beside_an_argument_breaking_its_bound_through_a_wrapper_refused,
    r#"{
  type N<'t: Number> = ['t, null];
  let f = 'a: N<'b> |x: 'b, y: Array<'a>| x;
  let h = |v| f(v, []);
  h("s")
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);
run!(
    bound_name_parameter_no_error_beside_an_error_argument_refused,
    r#"{
  let f = 'a: Result<'b, `E> |x: 'b, y: Array<'a>| x;
  f(error(`Q), [])
}"#,
    refused("NoError does not contain 'b: Error<'a: `Q>");
    FuseExpect::None
);
run!(
    bound_name_parameter_no_error_beside_a_value_argument,
    r#"{
  let f = 'a: Result<'b, `E> |x: 'b, y: Array<'a>| x;
  f(1, [])
}"#,
    is(Value::I64(1))
);

// A bound's argument that is an open variable through a typedef name takes
// the bound as a conjunct, as a bare one does, and is not decided by it.
run!(
    bound_name_argument_through_an_alias_stays_open,
    r#"{
  type N<'t: Number> = ['t, null];
  type I<'t> = 't;
  let f = 'a: N<I<'b>> |x: 'b, y: 'a| (x, y);
  let h = |v| f(v, null);
  let z: i64 = h(2).0;
  z
}"#,
    is(Value::I64(2))
);
run!(
    bound_name_argument_through_an_alias_in_a_parameter_stays_open,
    r#"{
  type N<'t: Number> = ['t, null];
  type I<'t> = 't;
  let f = |x: 'b, y: N<I<'b>>| (x, y);
  let h = |v| f(v, null);
  let z: i64 = h(2).0;
  z
}"#,
    is(Value::I64(2))
);
run!(
    bound_name_argument_through_an_alias_still_refuses_what_breaks_the_bound,
    r#"{
  type N<'t: Number> = ['t, null];
  type I<'t> = 't;
  let f = 'a: N<I<'b>> |x: 'b, y: 'a| (x, y);
  let h = |v| f(v, null);
  h("s")
}"#,
    refused("type mismatch Number does not contain string");
    FuseExpect::None
);
run!(
    bound_name_argument_through_a_bound_cell_stays_open,
    r#"{
  type N<'t: Number> = ['t, null];
  type I<'t> = 't;
  let f = 'a: N<I<'b>>, 'b: Number |x0: 'a, x1: 'b, x2: 'b| (x0, x1, x2);
  let v = never();
  let w = never();
  let r = f(w, w, v);
  let z: i64 = r.0;
  v <- 2;
  w <- 2;
  z
}"#,
    is(Value::I64(2))
);

// A union of primitives written in a parameter leaves an argument's
// undecided part to the argument's own check, as a union's members do.
run!(
    written_primitive_union_leaves_a_tuple_part_to_its_check,
    r#"{
  let f = |y: ([i64, null], i64)| y;
  let g = |u| f((u, u + 1));
  g(1)
}"#,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => matches!(&a[..], [Value::I64(1), Value::I64(2)]),
        _ => false,
    }
);
run!(
    written_primitive_union_leaves_a_struct_part_to_its_check,
    r#"{
  let f = |y: {p: [i64, null], q: i64}| y;
  let g = |u| f({p: u, q: u});
  g(1).p
}"#,
    is(Value::I64(1))
);
run!(
    written_primitive_union_still_refuses_what_the_check_breaks,
    r#"{
  let f = |y: ([i64, null], i64)| y;
  let g = |u| f((u, u + 1));
  g(null)
}"#,
    refused("null");
    FuseExpect::None
);

run!(
    narrower_argument_under_a_cell_above_a_union_takes_the_union_second,
    r#"{
  let f = 'a1: ['a3, null], 'a2: ['a3, null] |x1: 'a1, x2: 'a2| 7;
  let v: [i64, string] = 1;
  let u: i64 = 1;
  f(v, u)
}"#,
    prints("i64:7")
);

run!(
    narrower_argument_under_a_cell_above_a_union_takes_the_union_first,
    r#"{
  let f = 'a1: ['a3, null], 'a2: ['a3, null] |x1: 'a1, x2: 'a2| 7;
  let v: [i64, string] = 1;
  let u: i64 = 1;
  f(u, v)
}"#,
    prints("i64:7")
);

run!(
    narrower_argument_under_a_cell_above_a_union_through_a_wrapper,
    r#"{
  let f = 'a1: ['a3, null], 'a2: ['a3, null] |x1: 'a1, x2: 'a2| 7;
  let h = |a, b| f(a, b);
  let v: [i64, string] = 1;
  let u: i64 = 1;
  h(u, v)
}"#,
    prints("i64:7")
);

run!(
    argument_outside_the_union_a_cell_stands_above_widens_it,
    r#"{
  let f = 'a1: ['a3, null], 'a2: ['a3, null] |x1: 'a1, x2: 'a2| 7;
  let v: [i64, string] = 1;
  let u: f64 = 1.;
  f(v, u)
}"#,
    prints("i64:7");
    FuseExpect::Jit
);

// A lower bound a bound's holder puts on a variable reaches the open
// cells of that variable's own bounds when it is put, so a call of the
// enclosing definition that decides one of them is refused at the check,
// as one bound alone refuses it.
run!(
    lower_bound_reaches_the_bounds_own_cells_decided_by_a_caller_widens_them,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration] |x0: 'a0, x2: 'a2| x2;
  let w0: [null, string] = null;
  let g = |u| f(w0, u);
  g(1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);
run!(
    lower_bound_reaches_the_bounds_own_cells_decided_by_a_caller_in_a_block_widens_them,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration] |x0: 'a0, x2: 'a2| x2;
  let w0: [null, string] = null;
  { let g = |u| f(w0, u); g(1) }
}"#,
    prints("i64:1");
    FuseExpect::Jit
);
run!(
    lower_bound_reaches_the_bounds_own_cells_decided_by_a_caller_within_it,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration] |x0: 'a0, x2: 'a2| x2;
  let w0: [null, string] = null;
  let g = |u| f(w0, u);
  g("s")
}"#,
    prints(r#""s""#)
);
run!(
    lower_bound_reaches_the_bounds_own_cells_decided_by_a_caller_annotated,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration] |x0: 'a0, x2: 'a2| x2;
  let w0: [null, string] = null;
  let g = |u: [null, string]| f(w0, u);
  g("s")
}"#,
    prints(r#""s""#)
);
run!(
    lower_bound_into_a_declared_variable_a_bound_holds,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration] |x0: 'a0, x2: 'a2| x2;
  let w0: [null, string] = null;
  let g = |u: 'u| f(w0, u);
  g("s")
}"#,
    prints("\"s\"");
    FuseExpect::Jit
);
run!(
    lower_bound_into_a_declared_variable_with_a_bound,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration] |x0: 'a0, x2: 'a2| x2;
  let w0: [null, string] = null;
  let g = 'u: Any |u: 'u| f(w0, u);
  g(1)
}"#,
    prints("i64:1");
    FuseExpect::Jit
);
run!(
    lower_bound_reaches_the_bounds_own_cells_through_three_links,
    r#"{
  let f = 'a0: ['a1, bool], 'a1: ['a2, duration], 'a2: ['a3, null] |x0: 'a0, x3: 'a3| x3;
  let w0: [i64, string] = 1;
  let g = |u| f(w0, u);
  g(1.5)
}"#,
    prints("f64:1.5");
    FuseExpect::Jit
);

run!(
    narrower_argument_under_a_bound_chain_over_a_recursive_typedef,
    r#"{
  type G<'a, 'b> = [`X('a), `Y(H<'b, 'a>)];
  type H<'a, 'b> = [`X('b), `Y(G<'b, 'a>), `Z('a)];
  let f = 'a0: Array<G<'a2, 'a1>>, 'a2: Array<G<'a4, 'a3>>, 'a4: [i64, null],
    'a3: Array<G<'a4, 'a4>>, 'a1: Array<G<'a3, 'a2>>
    |x0: 'a0, x1: 'a1, x2: 'a2, x3: 'a3, x4: 'a4| [x4];
  type S = Array<G<null, null>>;
  type Y2 = Array<[`Y([`Z(Array<[`X(null)]>)])]>;
  type Y1 = Array<G<S, Y2>>;
  let y3: S = [`X(null)];
  let y2: Y2 = [`Y(`Z([`X(null)]))];
  let y1: Y1 = [];
  let y0: Array<G<Y2, Y1>> = [];
  f(y0, y1, y2, y3, null)
}"#,
    prints("[null]")
);

// CR claude for claude: [structure/low] [structure/low] The 15 fixtures from here to
// two_struct_members_one_cell_are_refused_as_the_second_name_is_not_first are one
// program over five shapes (Array, variant, tuple, Map, struct) in three forms: both
// members held, the first-named type refused, and the second-named type refused. That
// is about 290 lines differing only in the two typedef bodies, the formal's member and
// `n`'s annotation. Generate them from one `fn` taking those strings, as
// field_read_beside_two does for the field-read fixtures, so that a new shape is one
// line and the three forms cannot drift apart. Found by reading; not run.
// (tests-types-d-10)
run!(
    two_array_members_one_cell_hold_it_together,
    r#"{
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [Array<i64>, Array<string>, null] = null;
  let n: [i64, string] = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_array_members_one_cell_are_not_the_first_member,
    r#"{
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [Array<i64>, Array<string>, null] = null;
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_array_members_one_cell_are_not_the_last_member,
    r#"{
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [Array<i64>, Array<string>, null] = null;
  let n: string = f(v);
  0
}"#,
    refused("string does not contain i64");
    FuseExpect::None
);

run!(
    two_named_array_members_one_cell_hold_it_together_in_either_name_order,
    r#"{
  type T0 = Array<string>;
  type T1 = Array<i64>;
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: [i64, string] = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_named_array_members_one_cell_split_over_the_union_as_the_first_name_is_not_first,
    r#"{
  type T0 = Array<string>;
  type T1 = Array<i64>;
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_named_array_members_one_cell_beside_null_are_refused_as_the_first_name_is_not_first,
    r#"{
  type T0 = Array<string>;
  type T1 = Array<i64>;
  let f = |x: [null, Array<'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_named_array_members_one_cell_split_over_the_union_as_the_second_name_is_not_first,
    r#"{
  type T1 = Array<string>;
  type T0 = Array<i64>;
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_named_array_members_one_cell_beside_null_are_refused_as_the_second_name_is_not_first,
    r#"{
  type T1 = Array<string>;
  type T0 = Array<i64>;
  let f = |x: [null, Array<'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

// CR claude for claude: [test-gap/low] [test-gap/low] refused("i64 does not contain")
// accepts the wrong answer this test exists to exclude. Had the one cell followed the
// first member alone ('b = 'p = string), the refusal would be "type mismatch i64 does
// not contain string", which this predicate accepts. The fixture's actual refusal is
// "i64 does not contain ['p: string, 'q: i64]": pin that, as the named-member twins pin
// "[i64, string]", and add the accepted twin `let n: [i64, string] = g(1, "s")`, which
// checks. written_primitive_union_still_refuses_what_the_check_breaks pins
// refused("null") in the same way; pin "i64 does not contain null" there. probe:
// design/review-2026-11-16/repro/tests-types-d-two-members-loose-pin.gx
// (tests-types-d-04)
run!(
    two_array_members_of_declared_variables_one_cell_do_not_follow_the_names,
    r#"{
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let g = |p: 'q, q: 'p| {
    let v: [Array<'p>, Array<'q>, null] = null;
    f(v)
  };
  let n: i64 = g(1, "s");
  0
}"#,
    refused("i64 does not contain");
    FuseExpect::None
);

run!(
    two_error_members_one_cell_hold_both_tags,
    r#"{
  let f = 'a: NoError |x: ['a, Error<'e>]| x?;
  let v: [i64, string, Error<`A>, Error<`B>] = error(`B);
  let r = {
    catch(e) e ~ select e.0.error { `A => -1, `B => -2 };
    f(v)
  };
  0
}"#,
    prints("i64:0")
);

run!(
    two_error_members_one_cell_are_refused_a_handler_for_one_tag,
    r#"{
  let f = 'a: NoError |x: ['a, Error<'e>]| x?;
  let v: [i64, string, Error<`A>, Error<`B>] = error(`B);
  let r = {
    catch(e) e ~ select e.0.error { `A => -1 };
    f(v)
  };
  0
}"#,
    refused("missing match cases");
    FuseExpect::None
);

run!(
    two_variant_members_one_cell_hold_it_together,
    r#"{
  type T0 = `B(i64);
  type T1 = `B(string);
  let f = |x: ['a, `B('b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: [i64, string] = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_variant_members_one_cell_split_over_the_union_as_the_first_name_is_not_first,
    r#"{
  type T0 = `B(i64);
  type T1 = `B(string);
  let f = |x: ['a, `B('b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_variant_members_one_cell_beside_null_are_refused_as_the_first_name_is_not_first,
    r#"{
  type T0 = `B(i64);
  type T1 = `B(string);
  let f = |x: [null, `B('b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_variant_members_one_cell_split_over_the_union_as_the_second_name_is_not_first,
    r#"{
  type T1 = `B(i64);
  type T0 = `B(string);
  let f = |x: ['a, `B('b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_variant_members_one_cell_beside_null_are_refused_as_the_second_name_is_not_first,
    r#"{
  type T1 = `B(i64);
  type T0 = `B(string);
  let f = |x: [null, `B('b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_tuple_members_one_cell_hold_it_together,
    r#"{
  type T0 = (i64, string);
  type T1 = (i64, i64);
  let f = |x: ['a, (i64, 'b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: [i64, string] = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_tuple_members_one_cell_split_over_the_union_as_the_first_name_is_not_first,
    r#"{
  type T0 = (i64, string);
  type T1 = (i64, i64);
  let f = |x: ['a, (i64, 'b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_tuple_members_one_cell_beside_null_are_refused_as_the_first_name_is_not_first,
    r#"{
  type T0 = (i64, string);
  type T1 = (i64, i64);
  let f = |x: [null, (i64, 'b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_tuple_members_one_cell_split_over_the_union_as_the_second_name_is_not_first,
    r#"{
  type T1 = (i64, string);
  type T0 = (i64, i64);
  let f = |x: ['a, (i64, 'b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_tuple_members_one_cell_beside_null_are_refused_as_the_second_name_is_not_first,
    r#"{
  type T1 = (i64, string);
  type T0 = (i64, i64);
  let f = |x: [null, (i64, 'b)]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_map_members_one_cell_hold_it_together,
    r#"{
  type T0 = Map<string, i64>;
  type T1 = Map<string, string>;
  let f = |x: ['a, Map<string, 'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: [i64, string] = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_map_members_one_cell_split_over_the_union_as_the_first_name_is_not_first,
    r#"{
  type T0 = Map<string, i64>;
  type T1 = Map<string, string>;
  let f = |x: ['a, Map<string, 'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_map_members_one_cell_beside_null_are_refused_as_the_first_name_is_not_first,
    r#"{
  type T0 = Map<string, i64>;
  type T1 = Map<string, string>;
  let f = |x: [null, Map<string, 'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_map_members_one_cell_split_over_the_union_as_the_second_name_is_not_first,
    r#"{
  type T1 = Map<string, i64>;
  type T0 = Map<string, string>;
  let f = |x: ['a, Map<string, 'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_map_members_one_cell_beside_null_are_refused_as_the_second_name_is_not_first,
    r#"{
  type T1 = Map<string, i64>;
  type T0 = Map<string, string>;
  let f = |x: [null, Map<string, 'b>]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_struct_members_one_cell_hold_it_together,
    r#"{
  type T0 = {a: i64};
  type T1 = {a: string};
  let f = |x: ['a, {a: 'b}]| -> 'b never();
  let v: [T0, T1] = never();
  let n: [i64, string] = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_struct_members_one_cell_split_over_the_union_as_the_first_name_is_not_first,
    r#"{
  type T0 = {a: i64};
  type T1 = {a: string};
  let f = |x: ['a, {a: 'b}]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_struct_members_one_cell_beside_null_are_refused_as_the_first_name_is_not_first,
    r#"{
  type T0 = {a: i64};
  type T1 = {a: string};
  let f = |x: [null, {a: 'b}]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    two_struct_members_one_cell_split_over_the_union_as_the_second_name_is_not_first,
    r#"{
  type T1 = {a: i64};
  type T0 = {a: string};
  let f = |x: ['a, {a: 'b}]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_struct_members_one_cell_beside_null_are_refused_as_the_second_name_is_not_first,
    r#"{
  type T1 = {a: i64};
  type T0 = {a: string};
  let f = |x: [null, {a: 'b}]| -> 'b never();
  let v: [T0, T1] = never();
  let n: i64 = f(v);
  0
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    one_array_member_one_cell_still_decides_it,
    r#"{
  let f = |x: ['a, Array<'b>]| -> 'b never();
  let v: [Array<i64>, null] = null;
  let n: i64 = f(v);
  0
}"#,
    prints("i64:0");
    FuseExpect::None
);

run!(
    two_array_members_one_cell_under_a_writable_reference_are_told_apart_by_the_other_argument,
    r#"{
  let f = |w: &mut ['a, Array<'b>], y: Array<'b>| { *w <- y; 0 };
  let n: [Array<i64>, Array<string>] = [1];
  let y: Array<i64> = [1];
  f(&mut n, y);
  0
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    two_array_members_one_cell_under_a_writable_reference_refuse_an_element_neither_holds,
    r#"{
  let f = |w: &mut ['a, Array<'b>], y: Array<'b>| { *w <- y; 0 };
  let n: [Array<i64>, Array<string>] = [1];
  let y: Array<bool> = [true];
  f(&mut n, y);
  0
}"#,
    refused("[Array<i64>, Array<string>] does not contain Array<bool>");
    FuseExpect::None
);

// A trial gives an undecided type to the first member of a union that holds
// it on copies, so the union written in either order is a verdict of its own
// on the relation. A program whose argument holds an open variable is
// pre-unified and refused in both orders.
// CR claude for claude: [test-gap/medium] [test-gap/medium] This test and its
// _in_the_other_order twin pin refused("type mismatch"), which any mismatch anywhere in
// the fixture satisfies. The refusal itself names nothing to annotate ("type mismatch
// [`X('b: [i64, null], 'c: i64, ..)..] does not contain [..]"). Yet annotating `z` with
// the type the pre-unification already gave it, `|z: [i64, null]|`, makes the program
// check. design/tvar_constraints.md says the refusal of an undecided argument whose
// cells some decision could fit says "annotate it", but the undecided-argument path
// (Widening::take) gives no note here. Pin the full message in both orders, pin the
// lift, and have the refusal name `z`, or narrow the doc's promise to the case the code
// covers. probe:
// design/review-2026-11-16/repro/tests-types-d-open-argument-names-no-fix.gx
// (tests-types-d-03)
run!(
    union_of_arrays_beside_an_open_argument_checks_in_the_first_order,
    r#"{
  type U<'x, 'y> = [Array<'x>, Array<'y>];
  let f = |x: [`X('b, 'c, U<'a, 'b>, Array<'a>, [i64, null]), `Y(Array<Array<Array<'c>>>)]| 0;
  let n: [i64, null] = null;
  let y: Array<Array<Array<[i64, string]>>> = [[[1]]];
  let g = |z| { let c = true; let v = select c { true => `X(n, 1, [z], ["s"], z), false => `Y(y) }; f(v) };
  g(null)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    union_of_arrays_beside_an_open_argument_of_another_type_is_refused_in_the_first_order,
    r#"{
  type U<'x, 'y> = [Array<'x>, Array<'y>];
  let f = |x: [`X('b, 'c, U<'a, 'b>, Array<'a>, [i64, null]), `Y(Array<Array<Array<'c>>>)]| 0;
  let n: [i64, null] = null;
  let y: Array<Array<Array<[i64, string]>>> = [[[1]]];
  let g = |z| { let c = true; let v = select c { true => `X(n, 1, [z], ["s"], z), false => `Y(y) }; f(v) };
  g("s")
}"#,
    refused("type mismatch [i64, null] does not contain string");
    FuseExpect::None
);
run!(
    union_of_arrays_beside_an_open_argument_checks_in_the_other_order,
    r#"{
  type U<'x, 'y> = [Array<'x>, Array<'y>];
  let f = |x: [`X('b, 'c, U<'b, 'a>, Array<'a>, [i64, null]), `Y(Array<Array<Array<'c>>>)]| 0;
  let n: [i64, null] = null;
  let y: Array<Array<Array<[i64, string]>>> = [[[1]]];
  let g = |z| { let c = true; let v = select c { true => `X(n, 1, [z], ["s"], z), false => `Y(y) }; f(v) };
  g(null)
}"#,
    prints("i64:0");
    FuseExpect::Jit
);

run!(
    union_of_arrays_beside_an_open_argument_of_another_type_is_refused_in_the_other_order,
    r#"{
  type U<'x, 'y> = [Array<'x>, Array<'y>];
  let f = |x: [`X('b, 'c, U<'b, 'a>, Array<'a>, [i64, null]), `Y(Array<Array<Array<'c>>>)]| 0;
  let n: [i64, null] = null;
  let y: Array<Array<Array<[i64, string]>>> = [[[1]]];
  let g = |z| { let c = true; let v = select c { true => `X(n, 1, [z], ["s"], z), false => `Y(y) }; f(v) };
  g("s")
}"#,
    refused("type mismatch [i64, null] does not contain string");
    FuseExpect::None
);

run!(
    a_union_through_a_cell_that_holds_it_is_an_infinite_type,
    r#"{
  let c = true;
  let b = never();
  let a = [b];
  b <- select c { true => a, false => null };
  a
}"#,
    refused("infinite type");
    FuseExpect::None
);

run!(
    a_union_of_two_constructions_meets_a_generic_formal_of_that_constructor,
    r#"{
  let c = true;
  let v = select c { true => [1], false => ["a"] };
  let t: [(i64, string), (f64, i64)] = (1, "z");
  let k = |u: ('a, 'b)| 1;
  let s: [{a: i64, b: f64}, {a: string, b: i64}] = {a: 1, b: 1.5};
  let m = |u: {a: 'a, b: 'b}| 2;
  let l: [List<i64>, List<string>] = [<1>];
  array::len(v) + k(t) + m(s) + list::len(l)
}"#,
    is(Value::I64(5));
    FuseExpect::Jit
);

run!(
    a_variable_two_parts_of_one_argument_share_takes_both_in_either_order,
    r#"{
  let f = |x: {a: 'a, b: 'a}| 0;
  let n: [i64, null] = null;
  let g = |x: [`A('a), `B('a)]| 1;
  let v: [`A(i64), `B([i64, null])] = `A(1);
  let w: [`B(i64), `A([i64, null])] = `B(1);
  f({a: 1, b: n}) + f({a: n, b: 1}) + g(v) + g(w)
}"#,
    is(Value::I64(2));
    FuseExpect::Jit
);

run!(
    a_cell_within_no_error_meets_a_decided_union_at_one_variable,
    r#"{
  let mk = 'r: NoError |x: 'r| -> 'r x;
  let z = never();
  let r = mk(z);
  let e: [i64, Error<`E>] = 1;
  let f = |a: 'q, b: 'q| 0;
  f(e, r)
}"#,
    is(Value::I64(0));
    FuseExpect::Jit
);

run!(
    a_function_bound_bounds_a_callback_whatever_its_parameter_is_named,
    r#"{
  let e = never();
  let f = 'a: fn(x: i64) -> [i64, null] |g: 'a| g(1)$ + 1;
  let r = f(|y| e);
  e <- "s";
  r
}"#,
    refused("[i64, null] does not contain string");
    FuseExpect::None
);

run!(
    a_bound_holding_another_variable_leaves_an_undecided_argument_open,
    r#"{
  let f = 'a: ['b, Array<[i64, null]>] |x: 'a, y: 'b| x;
  let z = never();
  let r = f([z], "q");
  z <- 1;
  z + 1
}"#,
    is(Value::I64(2));
    FuseExpect::Jit
);

run!(
    a_written_test_types_each_function_part_as_its_own,
    r#"{
  let h = |f: fn(x: 'a) -> 'a, g: fn(x: 'b) -> 'b, n: [i64, null], x: 'a| -> 'a
    select (f, g, n) { (Any, Any, i64) as (_, q, _) => q(x), _ => x };
  h(|x: i64| x + 1, |s: string| "[s]!", 1, 5) + 1
}"#,
    refused("'b: unbound does not contain 'a: unbound");
    FuseExpect::None
);

run!(
    a_written_test_over_two_function_parts_keeps_them_two_instances,
    r#"{
  let mk = |z| |x| x;
  let a = mk(0);
  let b = mk(0);
  let n: [i64, null] = 1;
  let r = select (a, b, n) { (Any, Any, i64) as (_, _, k) => k, _ => 0 };
  let s = b("s");
  a(1) + r
}"#,
    is(Value::I64(2));
    FuseExpect::None
);

run!(
    two_error_members_from_select_arms_meet_one_result,
    r#"{
  let c = 1;
  let r = select c { 0 => 1, 1 => error(`A), _ => error(`B) };
  filter_err(r).0
}"#,
    prints("\"A\"");
    FuseExpect::Jit
);

// A function whose body takes apart a union of two applications of an
// abstract type, one with an open parameter, builds as its check accepts.
run!(
    an_instance_over_a_union_of_abstract_applications_builds,
    r#"{
  type B<'p> = Abstract<'p>;
  let v = |q| {
    let b = select true { true => B([1]), false => B([]) };
    select b { B(x) => x }
  };
  v(0)
}"#,
    prints("[i64:1]");
    FuseExpect::Jit
);
