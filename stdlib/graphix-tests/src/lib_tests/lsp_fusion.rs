//! Fusion runs in the lsp_mode check path, and an ill-typed program
//! checked in lsp_mode never panics or bricks the shared runtime.

use anyhow::Result;
use arcstr::literal;
use enumflags2::BitFlags;
use graphix_compiler::expr::Source;
use graphix_package_core::testing::{TestCtx, init_lsp_mode};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

// A fusable sync region over a runtime value, so it cannot fold to a
// constant.
const FUSABLE: &str = "let seed = cast<i64>(sys::time::now(0))$; seed * 3 + 1";
// A type error lsp_mode keeps compiling past.
const ILL_TYPED: &str = "let x = 1 + \"not a number\"; x";

async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    init_lsp_mode(sub, crate::TEST_REGISTER, vec![], BitFlags::empty(), |_| {}).await
}

/// Fusion runs during an lsp_mode check, and an ill-typed check in
/// between leaves the persistent runtime alive.
#[tokio::test(flavor = "multi_thread")]
async fn lsp_mode_fuses_and_survives_ill_typed() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init(tx).await?;

    // The `fused` counter is a compile-time statistic; a check never
    // executes a kernel.
    let before = ctx.fusion_stats().await?.fused;
    ctx.rt.check(Source::Internal(literal!(FUSABLE)), None).await?;
    let after = ctx.fusion_stats().await?.fused;
    assert!(
        after > before,
        "fusion must run during an lsp_mode check, but `fused` did not \
         advance ({before} -> {after}) — the lsp gate still disables fusion",
    );

    // May return Err; the process must survive.
    let _ = ctx.rt.check(Source::Internal(literal!(ILL_TYPED)), None).await;

    // A subsequent well-typed check still succeeds and still fuses.
    let before2 = ctx.fusion_stats().await?.fused;
    ctx.rt.check(Source::Internal(literal!(FUSABLE)), None).await?;
    let after2 = ctx.fusion_stats().await?.fused;
    assert!(
        after2 > before2,
        "the runtime was bricked by an ill-typed lsp_mode check: fusion \
         stopped advancing afterward ({before2} -> {after2})",
    );

    ctx.shutdown().await;
    drain.abort();
    Ok(())
}
