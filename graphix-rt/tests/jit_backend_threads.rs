//! The JIT compiles a link's functions on threads of its own, never on
//! rayon's global pool, which nothing in a runtime uses: a program's
//! fusion leaves that pool unbuilt.

use anyhow::Result;
use arcstr::ArcStr;
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use poolshark::global::GPooled;
use std::fmt::Write;
use tokio::sync::mpsc;

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fusion_leaves_the_global_pool_unbuilt() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let mut text = String::from("{\n");
    for i in 0..32 {
        writeln!(text, "let f{i} = |y: i64| -> i64 y * {i} + 1;")?;
    }
    text.push('(');
    for i in 0..32 {
        write!(text, "f{i}({i}), ")?;
    }
    text.push_str("0) }");
    let _e = gx.compile(ArcStr::from(text.as_str())).await?;
    let stats = gx.fusion_stats().await?;
    assert!(stats.fused >= 1, "the program did not fuse: {}", stats.fused);
    assert!(
        rayon::ThreadPoolBuilder::new().num_threads(1).build_global().is_ok(),
        "fusion built rayon's global pool"
    );
    Ok(())
}
