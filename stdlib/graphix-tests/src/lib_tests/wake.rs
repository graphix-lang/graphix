//! WAKE CATCH-UP (design/wake_catchup.md, ruled by Eric 2026-09-01):
//! a reselected arm always recomputes from the world as it stands; it
//! never re-raises events. The differential corpus
//! (`findings/wake-catchup-sep2026/`) pins engine AGREEMENT per
//! shape; this test pins the RULED VALUES themselves — Eric's probe
//! table, exactly as walked in the design conversation — so both
//! engines drifting together is still caught.

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

/// cond=false, n=1 → 43; cond→true (n's fire long since consumed by
/// the false arm at init) → the true arm wakes on present-but-stale
/// n=1 and the forced recompute gives 2; n→20 live → 21; cond→false →
/// the false arm wakes, n's 20-fire was consumed by the true arm
/// (once per select), so it reads present-but-stale 20 — and
/// recomputes 62. 43 here would be the ride this rule forbids; 63
/// (a phantom re-fire path) never appears because no event is
/// re-raised.
#[tokio::test(flavor = "multi_thread")]
async fn wake_recompute_table() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(TABLE)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let cond = super::callable::find_bind_id(&compiled.env, "test::cond")?;
    let n = super::callable::find_bind_id(&compiled.env, "test::n")?;
    // settle init, then drive the table's three epochs
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    gx.set_many([(cond, Value::Bool(true))])?;
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    gx.set_many([(n, Value::I64(20))])?;
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
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
