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

/// A kernel keeps its code alive past a JIT reset: the reset retires the
/// module while the kernels compiled into it keep running.
#[tokio::test]
async fn kernel_outlives_jit_reset() -> Result<()> {
    use anyhow::Context;
    use graphix_compiler::{
        Event, NoUserEvent, Scope, Tag, TagValue, compile,
        expr::{ModPath, parser::parse_one},
    };
    use netidx_value::Value;
    let (tx, _rx) = mpsc::channel(10);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        crate::TEST_REGISTER,
        vec![],
        BitFlags::empty(),
        |_| {},
    )
    .await?;
    let result = ctx
        .rt
        .with_ctx(move |ctx| -> Result<()> {
            let flags = BitFlags::empty();
            let scope = Scope::root().append("jit_reset");
            let mut input =
                compile(ctx, flags, &scope, parse_one("let input: i64 = never()")?)?;
            let id = ctx
                .env
                .lookup_bind(&scope.lexical, &ModPath::from(["input"]))?
                .context("input binding")?
                .1
                .id;
            let mut node =
                compile(ctx, flags, &scope, parse_one("#[native]\ninput * 2 + 1")?)?;
            for (i, n) in [3i64, 5, 8].into_iter().enumerate() {
                if i > 0 {
                    ctx.fusion.reset_jit_for_check()?;
                }
                let mut event = Event::new(NoUserEvent);
                event.init = i == 0;
                event.variables.insert(id, TagValue::tagged(Value::I64(n), Tag::FIRED));
                let v = node.update(ctx, &mut event);
                assert_eq!(v.value_cloned(), Value::I64(n * 2 + 1), "step {i}");
            }
            node.delete(ctx);
            input.delete(ctx);
            Ok(())
        })
        .await?;
    ctx.shutdown().await;
    result
}
