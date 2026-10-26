//! The lsp_mode check path runs the check alone, and an ill-typed
//! program checked in lsp_mode never panics or bricks the shared runtime.

use anyhow::Result;
use arcstr::literal;
use graphix_compiler::expr::Load;
use graphix_package_core::testing::{TestCtx, config};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

// A fusable sync region over a runtime value, so it cannot fold to a
// constant.
const FUSABLE: &str = "let seed = cast<i64>(sys::time::now(0))$; seed * 3 + 1";
// A type error lsp_mode keeps compiling past.
const ILL_TYPED: &str = "let x = 1 + \"not a number\"; x";

async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    TestCtx::start(config(sub, crate::TEST_REGISTER, vec![], |_| {})?.lsp_mode(true))
        .await
}

/// An lsp_mode check is the check alone: it builds no kernel. An
/// ill-typed check in between leaves the persistent runtime alive.
#[tokio::test(flavor = "multi_thread")]
async fn lsp_mode_checks_without_fusion_and_survives_ill_typed() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init(tx).await?;
    let before = ctx.fusion_stats().await?.attempted;
    ctx.rt.check(Load::Text(literal!(FUSABLE)), None).await?;
    // May return Err; the process must survive.
    // CR claude for claude: [test-gap/low] This throws away whether the check refused
    // the ill-typed program. An lsp_mode check that wrongly accepted `1 + "not a
    // number"` would pass here, and the language server would show no error.
    // ILL_TYPED's comment, "a type error lsp_mode keeps compiling past", describes an
    // older lsp_mode: today lsp_mode only installs the IDE sinks (GX::new), and the
    // check refuses the program. Assert that this check is an Err, and reduce the
    // comment to "a program the check refuses". Found by reading; `graphix --check`
    // refuses the program. (tests-lib-b2-04)
    let _ = ctx.rt.check(Load::Text(literal!(ILL_TYPED)), None).await;
    ctx.rt.check(Load::Text(literal!(FUSABLE)), None).await?;
    let after = ctx.fusion_stats().await?.attempted;
    assert_eq!(before, after, "a check attempted fusion ({before} -> {after})");
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}
