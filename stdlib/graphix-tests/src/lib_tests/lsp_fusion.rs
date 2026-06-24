//! Part A regression: fusion runs in the **lsp_mode** check path (it was
//! disabled there), and an ill-typed program checked in lsp_mode never
//! panics or bricks the shared runtime — fusion's emit path de-fuses to
//! the node-walk on anything malformed instead of panicking. The shell's
//! LSP shares ONE persistent runtime across every edit, so this mirrors
//! that: a single lsp_mode runtime checked repeatedly.

use anyhow::Result;
use arcstr::literal;
use enumflags2::BitFlags;
use graphix_compiler::expr::Source;
use graphix_package_core::testing::{init_lsp_mode, TestCtx};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

// A fusable sync arithmetic region. `seed` is the current time cast to
// i64 — a genuine runtime value, so the compiler can't fold the region
// to a constant and it really fuses (a pure-constant expression would
// fold away and never reach a kernel).
const FUSABLE: &str = "let seed = cast<i64>(sys::time::now(0))$; seed * 3 + 1";
// A type error (i64 + string). lsp_mode keeps compiling past it; fusion
// must not panic on whatever partial graph results.
const ILL_TYPED: &str = "let x = 1 + \"not a number\"; x";

async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    init_lsp_mode(sub, crate::TEST_REGISTER, vec![], BitFlags::empty(), |_| {}).await
}

/// Fusion runs during an lsp_mode check of a well-typed, fusable program
/// (it was OFF in lsp_mode before Part A's gate change), AND an ill-typed
/// check in between never bricks the persistent runtime — the
/// de-fuse-not-panic hardening keeps it alive.
#[tokio::test(flavor = "multi_thread")]
async fn lsp_mode_fuses_and_survives_ill_typed() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init(tx).await?;

    // A1: fusion RUNS during an lsp_mode check (the `fused` counter, a
    // compile-time statistic, advances — a check never executes a kernel,
    // so the runtime invocation counter would stay flat).
    let before = ctx.fusion_stats().await?.fused;
    ctx.rt.check(Source::Internal(literal!(FUSABLE)), None).await?;
    let after = ctx.fusion_stats().await?.fused;
    assert!(
        after > before,
        "fusion must run during an lsp_mode check, but `fused` did not \
         advance ({before} -> {after}) — the lsp gate still disables fusion",
    );

    // A2: an ill-typed program checked in lsp_mode must not panic. It may
    // return Err (a type diagnostic); what matters is the process survives.
    let _ = ctx.rt.check(Source::Internal(literal!(ILL_TYPED)), None).await;

    // The persistent runtime was NOT bricked: a subsequent well-typed
    // check still succeeds and still fuses.
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
