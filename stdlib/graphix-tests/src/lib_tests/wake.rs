//! Wake catch-up (design/wake_catchup.md): a reselected arm recomputes
//! from the world as it stands, reading standing values stale, and
//! re-raises only the fires no selected reader saw. This pins the ruled
//! values themselves (graphix-fuzz/findings/wake-catchup-sep2026/ pins
//! engine agreement).

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::VfsResolver;
use graphix_package_core::testing;
use graphix_rt::{GXEvent, NoExt};
use netidx::{path::Path, publisher::Value};
use std::time::Duration;
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
/// -> 62 (43 would be a ride; 63 a phantom re-fire).
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
    let deadline = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(deadline);
    let mut seen: Vec<i64> = Vec::new();
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, Value::I64(v)) = ev {
                            if id == expr_id {
                                seen.push(v);
                            }
                        }
                    }
                    if seen.last() == Some(&62) {
                        assert_eq!(
                            seen,
                            vec![43, 2, 21, 62],
                            "the wake catch-up table must read 43, 2, 21, 62 \
                             (62, never the pre-sleep 43 ride)"
                        );
                        return Ok(());
                    }
                }
            }
        }
    }
    bail!("the table never completed (saw {seen:?})")
}
