//! Wake catch-up (design/wake_catchup.md): a reselected arm recomputes
//! from the world as it stands, reading standing values stale, and
//! re-raises only the fires no selected reader saw. This pins the ruled
//! values themselves (graphix-fuzz/findings/wake-catchup-sep2026/ pins
//! engine agreement).

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::VfsResolver;
use graphix_package_core::testing;
use graphix_rt::NoExt;
use netidx::{path::Path, publisher::Value};
use tokio::sync::mpsc;

const TABLE: &str = r#"
let cond: bool = false;
cond <- never(false);
let n: i64 = 1;
n <- never(1);
let result = select cond { true => n + i64:1, false => n + i64:42 }
"#;

/// cond=false, n=1 -> 43; cond->true wakes the true arm on stale n=1
/// -> 2; n->20 live -> 21; cond->false wakes the false arm on stale 20
/// -> 62, once (43 would be a ride; a second 62 a phantom re-fire).
#[tokio::test(flavor = "multi_thread")]
async fn wake_recompute_table() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(TABLE)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx = testing::TestCtx::start(testing::config(
        tx,
        crate::TEST_REGISTER,
        vec![resolver],
        |_| {},
    )?)
    .await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let cond = graphix_package_core::testing::find_bind_id(&compiled.env, "test::cond")?;
    let n = graphix_package_core::testing::find_bind_id(&compiled.env, "test::n")?;
    // settle init, then drive the table's three epochs
    gx.wait_idle().await?;
    gx.set_many([(cond, Value::Bool(true))])?;
    gx.wait_idle().await?;
    gx.set_many([(n, Value::I64(20))])?;
    gx.wait_idle().await?;
    gx.set_many([(cond, Value::Bool(false))])?;
    let mut seen: Vec<i64> = Vec::new();
    testing::drain_idle(&gx, &mut rx, async |id, v| {
        if id == expr_id {
            seen.push(as_i64(&v)?);
        }
        Ok(false)
    })
    .await?;
    assert_eq!(
        seen,
        [43, 2, 21, 62],
        "the wake catch-up table must read 43, 2, 21, 62 (62, never the pre-sleep 43 \
         ride, and once)"
    );
    ctx.shutdown().await;
    Ok(())
}

fn as_i64(v: &Value) -> Result<i64> {
    match v {
        Value::I64(n) => Ok(*n),
        v => bail!("expected an i64, got {v}"),
    }
}
