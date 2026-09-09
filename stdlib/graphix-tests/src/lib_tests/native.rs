//! The `#[native]` attribute: the decorated expression must compile to
//! one fused kernel with zero node-walk residue, else it is a compile
//! error. It may decorate a value-producing computation or a call, not
//! a function definition. Tested via `eval` (fusion on), since the
//! attribute is mode-dependent.

use graphix_package_core::testing::eval;
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
    let r = eval("#[native]\n|x: i64| x + i64:1", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on a function literal must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on a function binding is an error.
#[tokio::test]
async fn native_on_lambda_binding_is_error() {
    let r =
        eval("{ #[native]\nlet f = |x: i64| x + i64:1; f(i64:2) }", crate::TEST_REGISTER)
            .await;
    assert!(
        r.is_err(),
        "#[native] on a function binding must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on an async computation (`throttle`) is an error.
#[tokio::test]
async fn native_on_unfusable_is_error() {
    let r = eval("#[native]\nthrottle(i64:5)", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on an async (non-fusing) expr must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// An unregistered attribute name is a compile error.
#[tokio::test]
async fn unknown_attribute_is_error() {
    let r = eval("#[bogus]\ni64:1", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "an unknown attribute must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` inside a HOF callback body is checked: a wholly sync
// `list::map` callback satisfies it.
#[tokio::test]
async fn native_hof_callback_fusable_ok() {
    let prog = "list::to_array(list::map(list::from_array([1, 2, 3]), \
                |x| #[native] { let y = x * 2; y + 1 }))";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a fully-fusing HOF callback body must compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// An `array::init` callback calling a recursive lambda defined in an
// earlier top-level statement fully fuses.
#[tokio::test]
async fn native_hof_callback_recursive_call_fuses_ok() {
    let prog = "{ \
                let rec f = |n: i64| -> i64 select n { 0 => 0, _ => f(n - 1) }; \
                array::init(4, |idx| #[native] { let a = idx * 2; f(a) }) \
                }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a callback that calls a recursive lambda must compile now \
         that nested cross-statement calls fuse (#203), got {:?}",
        r.map(|(v, _)| v)
    );
}

// Compiler-owned collection nodes inline their statically instantiated
// callback body into the enclosing region.
#[tokio::test]
async fn native_inlining_hof_callsite_ok() {
    let r =
        eval("#[native]\narray::init(3, |i| i * i + i64:1)", crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a compiler-owned collection call must compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A wholly synchronous callback inside a compiler-owned `list::fold`
// can satisfy `#[native]`.
#[tokio::test]
async fn native_fold_callback_fusable_ok() {
    let prog = "list::fold(list::from_array([1, 2, 3]), 0, \
                |acc, x| #[native] { let s = x + acc; s + i64:0 })";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a fully-fusing fold callback body must compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A callee whose body contains a cast fuses; the decorated computation
// calling it is native.
#[tokio::test]
async fn native_transitive_callee_dyncall_ok() {
    let prog = "{ \
                let g = |b: bool| cast<i64>(b)$; \
                #[native] (g(true) + g(false)) \
                }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a computation calling a callee with a body DynCall must \
         compile now that Stage 2 delivers transitive-callee DynCalls, got {:?}",
        r.map(|(v, _)| v)
    );
}

// An all-scalar `{ s with f: v }` over a fused source struct fuses.
#[tokio::test]
async fn native_structwith_ok() {
    let prog =
        "#[native]\n{ let s = { x: i64:1, y: i64:2, z: i64:3 }; { s with y: i64:10 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a scalar struct-with must compile now that StructWith has \
         an emit_clif, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A struct-with whose source has a string field copied unchanged fuses.
#[tokio::test]
async fn native_structwith_string_field_ok() {
    let prog = "#[native]\n{ let s = { name: \"x\", n: i64:1 }; { s with n: i64:2 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a struct-with copying an unchanged string field must \
         compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A connect inside a map callback.
#[tokio::test]
async fn native_connect_composite_rhs_ok() {
    let prog = "{ let last = { v: i64:0 }; \
                array::map([1, 2, 3], |x| #[native] { last <- { v: x }; x }); \
                last.v }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    // A connect is an effect and refuses emission, so `#[native]` on
    // this callback is a compile error.
    let e = format!("{:?}", r.as_ref().err());
    assert!(
        r.is_err() && e.contains("did not fully fuse"),
        "expected the strict-fusion cliff error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// String elements use the collection scaffold's owned ArcStr binding.
#[tokio::test]
async fn native_hof_string_element_ok() {
    let prog = "#[native]\narray::map([\"a\", \"bb\", \"ccc\"], |s| str::len(s))";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a string-element map must compile natively, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A tuple pattern over a borrowed scrutinee fuses.
#[tokio::test]
async fn native_select_destructure_ok() {
    let prog = "#[native]\n{ let t = (3, 4); select t { (0, y) => y, (x, y) => x + y } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a tuple-destructuring select must fully fuse now, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A nested slice inside a tuple pattern fuses.
#[tokio::test]
async fn native_select_nested_tuple_ok() {
    let prog = "{ let t = ([1.0, 2.0], 42); \
                #[native] select t { ([a, b], c) => a + b, _ => 0.0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a nested-slice-in-tuple select must fully fuse now, got {:?}",
        r.map(|(v, _)| v)
    );
}

// The struct-parent nested case fuses (`_` infers a fresh TVar).
#[tokio::test]
async fn native_select_nested_struct_ok() {
    let prog = "{ let x = { foo: [1.0, 2.0, 4.5], bar: 42, baz: 8.0 }; \
                #[native] select x { \
                { foo: [a, b, ..], bar: _, baz: _ } => a + b, _ => 0.0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "the struct-nested select must fully fuse now that `_` infers a \
         fresh TVar, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A float-result select with a conditional final arm.
#[tokio::test]
async fn native_select_float_conditional_final_ok() {
    let prog = "{ let x = { bar: 42, baz: 8.0 }; \
                #[native] select x { \
                { bar: 0, baz } => baz, { bar: _, baz: _ } => 0.0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a float-result select with a conditional final arm must fuse \
         (and not panic the verifier), got {:?}",
        r.map(|(v, _)| v)
    );
}

// Collection scaffolds bind destructured composite leaves by BindId.
#[tokio::test]
async fn native_hof_composite_leaf_ok() {
    let prog = "#[native]\narray::map([((1, 2), 10), ((3, 4), 20)], |(pt, n)| pt.0 + n)";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a destructured-formal map must compile natively, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A named rest binding (`[x, rest..]`) still de-fuses, so `#[native]`
// on it is a compile error.
#[tokio::test]
async fn native_select_named_rest_defuses() {
    let prog = "{ let a = [1, 2, 3]; \
                #[native] select a { [x, rest..] => x + array::len(rest), _ => 0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "a named-rest select must still de-fuse (owned subslice arm local), got {:?}",
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
        err.contains("builtin call site") && err.contains("not discovered"),
        "should report the real call blocker, got: {err}"
    );
    assert!(
        !err.contains("does not emit CLIF"),
        "structural `let` noise (whose values fused) must be filtered out, got: {err}"
    );
}
