//! The harness's waits for a program's values: each fails at once when
//! the runtime settles without the value, and holds for as long as the
//! program has something outstanding, whatever the wall clock says.

use anyhow::Result;
use graphix_package_core::testing::{
    Collect, Fixture, FuseExpect, Mode, RunFixture, collect, compile_result, eval,
    eval_modes, fixture_runtime, next_update, predicate, result_source, result_text,
    run_fixture, run_tempdir_fixture,
};
use netidx::subscriber::Value;

const NO_VALUE: &str = "never<i64>()";

fn settled_without_a_value(r: Result<()>) {
    let e = r.expect_err("a program that gives no value fails");
    assert!(format!("{e:#}").contains("settled without an update"), "{e:#}");
}

#[test]
fn run_fixture_fails_when_the_runtime_settles_without_a_value() {
    let f = RunFixture {
        name: module_path!(),
        files: vec![("/test.gx", result_text(NO_VALUE))],
        pred: predicate(|_| true),
        expect: FuseExpect::None,
        shape: None,
        register: crate::TEST_REGISTER,
    };
    settled_without_a_value(run_fixture(f, Mode::Interp, false))
}

#[tokio::test(flavor = "current_thread")]
async fn tempdir_fixture_fails_when_the_runtime_settles_without_a_value() {
    let r =
        run_tempdir_fixture(NO_VALUE, crate::TEST_REGISTER, Mode::Interp, &|_| Ok(()))
            .await;
    settled_without_a_value(r)
}

#[tokio::test(flavor = "current_thread")]
async fn eval_fails_when_the_runtime_settles_without_a_value() {
    settled_without_a_value(eval(NO_VALUE, crate::TEST_REGISTER).await.map(|_| ()))
}

#[tokio::test(flavor = "current_thread")]
async fn eval_modes_fails_when_the_runtime_settles_without_a_value() {
    settled_without_a_value(eval_modes(NO_VALUE, crate::TEST_REGISTER).await.map(|_| ()))
}

#[tokio::test(flavor = "current_thread")]
async fn widget_fixture_fails_when_the_runtime_settles_without_a_value() {
    let r = Fixture::new(&result_text(NO_VALUE), crate::TEST_REGISTER, |_| ()).await;
    settled_without_a_value(r.map(|_| ()))
}

/// A value that comes 11 s of wall time after its program starts, its
/// timer outstanding all along.
#[tokio::test(flavor = "current_thread")]
async fn a_late_value_is_awaited() -> Result<()> {
    let code = "sys::time::after_idle(duration:11.s, 1)";
    run_tempdir_fixture(code, crate::TEST_REGISTER, Mode::Jit, &|v| {
        assert_eq!(v, Value::I64(1));
        Ok(())
    })
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn next_update_fails_when_the_runtime_settles_without_a_later_value() -> Result<()>
{
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source("0"))],
        crate::TEST_REGISTER,
        Mode::Jit,
        |_| {},
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let id = res.exprs[0].id;
    assert_eq!(next_update(&ctx.rt, &mut rx, id).await?, Value::I64(0));
    settled_without_a_value(next_update(&ctx.rt, &mut rx, id).await.map(|_| ()));
    ctx.shutdown().await;
    Ok(())
}

/// A second value that comes 11 s of wall time after the first, its
/// timer outstanding all along, read as the first two values and as
/// every value until the settle.
#[tokio::test(flavor = "current_thread")]
async fn a_late_later_value_is_awaited() -> Result<()> {
    let code = "{ let x = 0; x <- sys::time::after_idle(duration:11.s, 1); x }";
    let (first, settled) = tokio::join!(
        collect(code, crate::TEST_REGISTER, Mode::Jit, Collect::First(2)),
        collect(code, crate::TEST_REGISTER, Mode::Jit, Collect::Settled),
    );
    assert_eq!(first?, [Value::I64(0), Value::I64(1)]);
    assert_eq!(settled?, [Value::I64(0), Value::I64(1)]);
    Ok(())
}
