//! Deleting a root moves no other root: dropping n refs costs O(n log n),
//! so eight times the refs take about eight times as long, not 64.

use anyhow::Result;
use arcstr::literal;
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

/// The time to drop `n` refs to one variable, oldest first.
async fn drop_refs(n: usize) -> Result<Duration> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let _x = gx.compile(literal!("let x = 0")).await?;
    let env = gx.get_env().await?;
    let r = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["x"])).await?;
    let mut refs = Vec::with_capacity(n);
    for _ in 0..n {
        refs.push(gx.compile_ref(r.bid).await?);
    }
    gx.get_env().await?;
    let st = std::time::Instant::now();
    drop(refs);
    gx.get_env().await?;
    Ok(st.elapsed())
}

async fn best_of_three(n: usize) -> Result<Duration> {
    let mut best = Duration::MAX;
    for _ in 0..3 {
        best = best.min(drop_refs(n).await?);
    }
    Ok(best)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn dropping_refs_is_not_quadratic() -> Result<()> {
    let small = best_of_three(2000).await?;
    let large = best_of_three(16000).await?;
    assert!(large < small * 24, "2000 refs {small:?}, 16000 refs {large:?}");
    Ok(())
}
