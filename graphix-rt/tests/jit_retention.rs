//! Code an embedder compiles and drops leaves nothing in the JIT: what
//! it holds after a few hundred compiles is what it held after the first
//! few, and the code space its links burned comes back when the live
//! kernels move out of a mostly dead generation (`design/jit_memory.md`).

use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::CompExp;
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

async fn runtime() -> Result<GXHandle<NoExt>> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    GXConfig::builder(ctx, tx).build()?.start().await
}

/// `step(i)` compiles something and drops it; the JIT's holdings settle.
async fn settles<F: AsyncFnMut(i64) -> Result<()>>(
    gx: &GXHandle<NoExt>,
    mut step: F,
) -> Result<()> {
    let mut early = 0;
    for i in 0..300 {
        step(i).await?;
        if i == 20 {
            early = gx.fusion_stats().await?.retained;
        }
    }
    let stats = gx.fusion_stats().await?;
    assert!(stats.fused >= 300, "the steps did not fuse: {}", stats.fused);
    assert!(early > 0, "nothing was retained while code lived");
    assert!(
        stats.retained <= early,
        "the JIT held {early} after 20 compiles and {} after 300",
        stats.retained
    );
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn dropped_definitions_leave_nothing() -> Result<()> {
    let gx = runtime().await?;
    settles(&gx, async |i| {
        let text: ArcStr =
            format!("{{ let g = |y: i64| -> i64 y * {i} + 1; g({i}) }}").into();
        drop(gx.compile(text).await?);
        Ok(())
    })
    .await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn dropped_callables_leave_nothing() -> Result<()> {
    let gx = runtime().await?;
    let _h = gx.compile(literal!("let h = |x: i64| -> i64 x * 2 + 1")).await?;
    let env = gx.get_env().await?;
    let r = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["h"])).await?;
    let h = r.last.clone().context("h has no value")?;
    settles(&gx, async |i| {
        let c = gx.compile_callable(h.clone()).await?;
        c.call(ValArray::from_iter_exact([Value::I64(i)].into_iter())).await?;
        Ok(())
    })
    .await
}

/// The live program's next output, after `n` was set.
async fn answer(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    live: &CompExp<NoExt>,
) -> Result<Value> {
    let found = tokio::time::timeout(Duration::from_secs(10), async {
        while let Some(mut batch) = rx.recv().await {
            for e in batch.drain(..) {
                if let GXEvent::Updated(id, v) = e
                    && id == live.id
                {
                    return Some(v);
                }
            }
        }
        None
    });
    found.await?.context("the live program did not answer")
}

/// A program that lives through a few thousand compiles of others keeps
/// answering: their dead code leaves the generation mostly dead, the
/// program's kernels move to a fresh one, and the old arena is freed.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn live_kernels_leave_dead_generations() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(1000);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let program = gx
        .compile(literal!(
            "let n: i64 = 0;\nlet f = |a: i64| -> i64 a * 3 + 1;\n#[native] f(n)"
        ))
        .await?;
    let live = program.exprs.last().context("the program has a value")?;
    let env = gx.get_env().await?;
    let mut n =
        gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["n"])).await?;
    let mut peak = 0;
    for i in 0..2500i64 {
        let text: ArcStr =
            format!("{{ let g = |y: i64| -> i64 y * {i} + 1; g({i}) }}").into();
        drop(gx.compile(text).await?);
        if i % 100 == 0 {
            while rx.try_recv().is_ok() {}
            n.set(Value::I64(i))?;
            assert_eq!(answer(&mut rx, live).await?, Value::I64(3 * i + 1));
            peak = peak.max(gx.fusion_stats().await?.jit_committed);
        }
    }
    let stats = gx.fusion_stats().await?;
    assert!(stats.jit_relocations > 0, "no kernel moved: {stats:?}");
    // a link burns at least a page: 2500 would commit ~10MB unrelocated
    assert!(peak < 8 << 20, "the arena committed {peak} bytes at its peak");
    Ok(())
}
