//! The `#[native]` attribute: the decorated expression must compile to
//! one fused kernel with zero node-walk residue, else it is a compile
//! error. It may decorate a value-producing computation or a call, not
//! a function definition. Its refusals are tested via `eval` (fusion
//! on); the fixtures it accepts run in every mode and pin their values.

use graphix_package_core::{
    run,
    testing::{FuseExpect, eval, refusal, refused},
};
use netidx::subscriber::Value;

// A pure computation that fully fuses satisfies `#[native]`.
#[tokio::test]
async fn native_fusable_ok() {
    let r =
        eval("#[native]\n{ let x = i64:3; x * x + i64:1 }", crate::TEST_REGISTER).await;
    assert!(
        matches!(r.as_ref().map(|(v, _)| v), Ok(Value::I64(10))),
        "expected a fused native computation to compile and yield 10, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on a bare lambda literal is an error.
#[tokio::test]
async fn native_on_lambda_literal_is_error() {
    let e = refusal("#[native]\n|x: i64| x + i64:1", crate::TEST_REGISTER).await.unwrap();
    assert!(e.contains("not a function"), "{e}");
}

// `#[native]` on a function binding is an error.
#[tokio::test]
async fn native_on_lambda_binding_is_error() {
    let e = refusal(
        "{ #[native]\nlet f = |x: i64| x + i64:1; f(i64:2) }",
        crate::TEST_REGISTER,
    )
    .await
    .unwrap();
    assert!(e.contains("not a function"), "{e}");
}

// `#[native]` on an async computation (`throttle`) is an error.
#[tokio::test]
async fn native_on_unfusable_is_error() {
    let e = refusal("#[native]\nthrottle(i64:5)", crate::TEST_REGISTER).await.unwrap();
    assert!(e.contains("did not fully fuse"), "{e}");
}

// A call whose argument the node-walk computes is residue: the argument
// is under the annotation, so `#[native]` refuses it and names why.
#[tokio::test]
async fn native_call_fed_by_node_walk_is_error() {
    let e = refusal(
        "{ let f = |x: i64| -> i64 x + 1; #[native] f(throttle(i64:5)) }",
        crate::TEST_REGISTER,
    )
    .await
    .unwrap();
    assert!(e.contains("did not fully fuse") && e.contains("throttle"), "{e}");
}

// An argument the node-walk computes with no fusion failure of its own
// (a sample) is named as residue.
#[tokio::test]
async fn native_call_fed_by_a_sample_names_it() {
    let e = refusal(
        "{ let t = 1; let f = |x: i64| -> i64 x + 1; #[native] f(t ~ 10) }",
        crate::TEST_REGISTER,
    )
    .await
    .unwrap();
    assert!(e.contains("the node-walk computes `t ~ 10`"), "{e}");
}

// The same argument bound outside the annotation is an input: the call
// is native.
#[tokio::test]
async fn native_call_over_bound_argument_ok() {
    let r = eval(
        "{ let f = |x: i64| -> i64 x + 1; let a = throttle(i64:5); #[native] f(a) }",
        crate::TEST_REGISTER,
    )
    .await;
    assert!(
        matches!(r.as_ref().map(|(v, _)| v), Ok(Value::I64(6))),
        "{:?}",
        r.map(|(v, _)| v)
    );
}

// An unregistered attribute name is a compile error.
#[tokio::test]
async fn unknown_attribute_is_error() {
    let e = refusal("#[bogus]\ni64:1", crate::TEST_REGISTER).await.unwrap();
    assert!(e.contains("unknown attribute #[bogus]"), "{e}");
}

// An attribute other than #[parallel(grain)] takes no arguments, the
// definition assertions and the registry's included.
run!(def_assertion_arguments_refused, "{ let f = #[pure(1, 2)] |a| a + 1; f(1) }",
    refused("#[pure] takes no arguments"); FuseExpect::None);
run!(native_arguments_refused, "{ let a = 1; #[native(1)] (a + 1, a + 2) }",
    refused("#[native] takes no arguments"); FuseExpect::None);

// `#[native]` inside a HOF callback body is checked: a wholly sync
// `list::map` callback satisfies it.
run!(native_hof_callback_fusable, "list::to_array(list::map(list::from_array([1, 2, 3]), \
                |x| #[native] { let y = x * 2; y + 1 }))", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == "[i64:3, i64:5, i64:7]"; FuseExpect::Jit);

// An `array::init` callback calling a recursive lambda defined in an
// earlier top-level statement fully fuses.
run!(native_hof_callback_recursive_call_fuses, "{ \
                let rec f = |n: i64| -> i64 select n { 0 => 0, _ => f(n - 1) }; \
                array::init(4, |idx| #[native] { let a = idx * 2; f(a) }) \
                }", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == "[i64:0, i64:0, i64:0, i64:0]"; FuseExpect::Jit);

// Compiler-owned collection nodes inline their statically instantiated
// callback body into the enclosing region.
run!(native_inlining_hof_callsite, "#[native]\narray::init(3, |i| i * i + i64:1)", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == "[i64:1, i64:2, i64:5]"; FuseExpect::Jit);

// A wholly synchronous callback inside a compiler-owned `list::fold`
// can satisfy `#[native]`.
run!(native_fold_callback_fusable, "list::fold(list::from_array([1, 2, 3]), 0, \
                |acc, x| #[native] { let s = x + acc; s + i64:0 })", |v: anyhow::Result<&Value>| matches!(v, Ok(Value::I64(6))); FuseExpect::Jit);

// A callee whose body contains a cast fuses; the decorated computation
// calling it is native.
run!(native_transitive_callee_cast, "{ \
                let g = |b: bool| cast<i64>(b)$; \
                #[native] (g(true) + g(false)) \
                }", |v: anyhow::Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// An all-scalar `{ s with f: v }` over a fused source struct fuses.
run!(native_structwith, "#[native]\n{ let s = { x: i64:1, y: i64:2, z: i64:3 }; { s with y: i64:10 } }", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == r#"[["x", i64:1], ["y", i64:10], ["z", i64:3]]"#; FuseExpect::Jit);

// A struct-with whose source has a string field copied unchanged fuses.
run!(native_structwith_string_field, "#[native]\n{ let s = { name: \"x\", n: i64:1 }; { s with n: i64:2 } }", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == r#"[["n", i64:2], ["name", "x"]]"#; FuseExpect::Jit);

// A connect inside a map callback.
#[tokio::test]
async fn native_connect_in_callback_is_error() {
    let prog = "{ let last = { v: i64:0 }; \
                array::map([1, 2, 3], |x| #[native] { last <- { v: x }; x }); \
                last.v }";
    // A connect is an effect and refuses emission, so `#[native]` on
    // this callback is a compile error.
    let e = refusal(prog, crate::TEST_REGISTER).await.unwrap();
    assert!(e.contains("did not fully fuse"), "{e}");
}

// String elements use the collection scaffold's owned ArcStr binding.
run!(native_hof_string_element, "#[native]\narray::map([\"a\", \"bb\", \"ccc\"], |s| str::len(s))", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == "[i64:1, i64:2, i64:3]"; FuseExpect::Jit);

// A tuple pattern over a borrowed scrutinee fuses.
run!(native_select_destructure, "#[native]\n{ let t = (3, 4); select t { (0, y) => y, (x, y) => x + y } }", |v: anyhow::Result<&Value>| matches!(v, Ok(Value::I64(7))); FuseExpect::Jit);

// A nested slice inside a tuple pattern fuses.
run!(native_select_nested_tuple, "{ let t = ([1.0, 2.0], 42); \
                #[native] select t { ([a, b], c) => a + b, _ => 0.0 } }", |v: anyhow::Result<&Value>| matches!(v, Ok(Value::F64(f)) if *f == 3.0); FuseExpect::Jit);

// The struct-parent nested case fuses (`_` infers a fresh TVar).
run!(native_select_nested_struct, "{ let x = { foo: [1.0, 2.0, 4.5], bar: 42, baz: 8.0 }; \
                #[native] select x { \
                { foo: [a, b, ..], bar: _, baz: _ } => a + b, _ => 0.0 } }", |v: anyhow::Result<&Value>| matches!(v, Ok(Value::F64(f)) if *f == 3.0); FuseExpect::Jit);

// A float-result select whose first arm tests a field and whose final
// arm takes the rest.
run!(native_select_float_conditional_final, "{ let x = { bar: 42, baz: 8.0 }; \
                #[native] select x { \
                { bar: 0, baz } => baz, { bar: _, baz: _ } => 0.0 } }", |v: anyhow::Result<&Value>| matches!(v, Ok(Value::F64(f)) if *f == 0.0); FuseExpect::Jit);

// Collection scaffolds bind destructured composite leaves by BindId.
run!(native_hof_composite_leaf, "#[native]\narray::map([((1, 2), 10), ((3, 4), 20)], |(pt, n)| pt.0 + n)", |v: anyhow::Result<&Value>| format!("{}", v.unwrap()) == "[i64:11, i64:23]"; FuseExpect::Jit);

// A named rest binding (`[x, rest..]`) is an owned subslice arm local,
// so `#[native]` on it compiles.
#[tokio::test]
async fn native_select_named_rest_fuses() {
    let prog = "{ let a = [1, 2, 3]; \
                #[native] select a { [x, rest..] => x + array::len(rest), _ => 0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        matches!(r.as_ref().map(|(v, _)| v), Ok(Value::I64(3))),
        "a named-rest select must fuse, got {:?}",
        r.map(|(v, _)| v)
    );
}

// The blocker list names the call that node-walks (`throttle`), not
// the structural `let`s whose values fused.
#[tokio::test]
async fn native_blocker_list_is_filtered() {
    let prog = "array::init(4, |idx| #[native] { let a = idx * 2; throttle(a) })";
    let e =
        eval(prog, crate::TEST_REGISTER).await.err().expect("must be a compile error");
    // `{:#}` includes anyhow's full cause chain (the `#[native]` blocker
    // detail is a CAUSE, not the top-level context).
    let err = format!("{e:#}");
    assert!(
        err.contains("builtin `throttle` has no fast-call entry"),
        "should report the real call blocker, got: {err}"
    );
    assert!(
        !err.contains("does not emit CLIF"),
        "structural `let` noise (whose values fused) must be filtered out, got: {err}"
    );
}
