//! A callable's call site binds the lambda it was given when it is
//! compiled, so a pure body fuses as the same call written in Graphix
//! would.

use anyhow::{Context, Result};
use arcstr::literal;
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_pure_callable_fuses() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let _m = gx.compile(literal!("let h = |x: i64| -> i64 x * 2 + 1")).await?;
    let env = gx.get_env().await?;
    let r = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["h"])).await?;
    let before = gx.fusion_stats().await?.fused;
    let c = gx.compile_callable(r.last.clone().context("h has no value")?).await?;
    assert!(gx.fusion_stats().await?.fused > before, "the callable's body did not fuse");
    c.call(ValArray::from_iter_exact([Value::I64(20)].into_iter())).await?;
    let answer = tokio::time::timeout(Duration::from_secs(5), async {
        while let Some(mut b) = rx.recv().await {
            for e in b.drain(..) {
                if let GXEvent::Updated(id, v) = e
                    && id == c.expr
                {
                    return Some(v);
                }
            }
        }
        None
    })
    .await?;
    assert_eq!(answer, Some(Value::I64(41)));
    Ok(())
}
