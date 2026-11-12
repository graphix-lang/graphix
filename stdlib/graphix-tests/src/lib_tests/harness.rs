//! The harness's wait for a fixture's first value: it fails at once when
//! the runtime settles without one, and holds for as long as the program
//! has something outstanding, whatever the wall clock says.

use anyhow::Result;
use graphix_package_core::testing::{
    FuseExpect, Mode, RunFixture, predicate, result_text, run_fixture,
    run_tempdir_fixture,
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
