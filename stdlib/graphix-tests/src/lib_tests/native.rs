//! The `#[native]` attribute (Part C3). The decorated expression must compile
//! to native code — one fused JIT kernel with zero node-walk residue — else it
//! is a compile error. It may only decorate a value-producing computation or a
//! call; a function definition (or any function-typed target) is rejected,
//! because a `native` requirement on a function value would be infectious and
//! brittle (it could never be stored, dynamically dispatched, or passed to a
//! non-fusing HOF) — a performance requirement belongs at the use site.
//!
//! These are compile-time assertions, so they are tested directly via `eval`
//! (which runs with fusion on) rather than the `run!` differential harness:
//! `#[native]` is deliberately mode-dependent (it cannot be verified under
//! `--no-fusion`), so the node-walk-vs-jit value-agreement harness doesn't
//! apply.

use graphix_package_core::testing::eval;
use netidx::subscriber::Value;

// A pure computation that fully fuses → `#[native]` is satisfied, and the
// program still produces its value.
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

// `#[native]` on a bare lambda literal — a function definition — is an error.
#[tokio::test]
async fn native_on_lambda_literal_is_error() {
    let r = eval("#[native]\n|x: i64| x + i64:1", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on a function literal must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on a function binding — also a function definition — is an error.
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

// `#[native]` on an async computation that cannot fuse is an error (this is
// the teeth of the attribute — it forbids exactly the constructs that
// de-fuse). `once` is classified async, so it node-walks.
#[tokio::test]
async fn native_on_unfusable_is_error() {
    let r = eval("#[native]\nonce(i64:5)", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on an async (non-fusing) expr must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// An unregistered attribute name is a compile error, independent of fusion.
#[tokio::test]
async fn unknown_attribute_is_error() {
    let r = eval("#[bogus]\ni64:1", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "an unknown attribute must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}
