//! A check asked for types records every checked node's span and type
//! outside lambda bodies.

use anyhow::Result;
use arcstr::literal;
use graphix_compiler::{CFlag, expr::Source};
use graphix_package_core::testing::init_with_flags_and_setup;
use tokio::sync::mpsc;

const PROGRAM: &str = "let f = |x: i64| -> i64 x + 100;\nlet s = \"a\";\n(f(2), s)";

#[tokio::test(flavor = "multi_thread")]
async fn a_check_records_the_types_it_decided() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_with_flags_and_setup(
        tx,
        crate::TEST_REGISTER,
        vec![],
        CFlag::FusionDisabled.into(),
        |_| {},
    )
    .await?;
    let r = ctx
        .rt
        .check_with_types(Source::Internal(literal!(PROGRAM)), vec![], None)
        .await?;
    let at = |line: i32, column: i32| {
        r.ide
            .expr_types
            .iter()
            .filter(|s| s.pos.line == line && s.pos.column == column)
            .map(|s| s.typ.to_string())
            .collect::<Vec<_>>()
    };
    assert!(at(3, 1).contains(&"(i64, string)".to_string()), "{:?}", at(3, 1));
    assert!(at(3, 2).contains(&"i64".to_string()), "the call: {:?}", at(3, 2));
    assert!(at(2, 9).contains(&"string".to_string()), "{:?}", at(2, 9));
    // `x + 100` sits in a lambda body: a body compiles per call site
    assert!(at(1, 25).is_empty(), "a body site recorded: {:?}", at(1, 25));
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}
