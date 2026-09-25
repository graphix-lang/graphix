//! A check leaves the runtime's registered signatures as it found them:
//! the same program checks twice on one runtime.

use anyhow::Result;
use arcstr::literal;
use graphix_compiler::expr::Source;
use graphix_package_core::testing::init;
use tokio::sync::mpsc;

const PAREN_POLY_REF: &str = "filter((array::flat_map), |x| true)";

#[tokio::test(flavor = "multi_thread")]
async fn a_program_checks_twice() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    for _ in 0..2 {
        ctx.rt.check(Source::Internal(literal!(PAREN_POLY_REF)), None).await?;
    }
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}
