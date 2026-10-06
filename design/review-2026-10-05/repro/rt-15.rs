//! rt-15: set_many is not atomic. do_cycle (graphix-rt/src/gx.rs:470-476)
//! delivers queued writes, then every task entry one at a time through
//! push_var_event; an entry whose variable already has a delivery this
//! cycle is re-queued alone, while the rest of the same set_many lands.
//!
//! command: copy to stdlib/graphix-tests/tests/review_rt_15.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-tests --test review_rt_15 -- --nocapture
//!
//! The runtime is current_thread and the two sends have no await between
//! them, so both arrive in one input batch.
//! expected (set_many "every update is delivered in the same cycle"):
//!   (sx, sy) per cycle: [5, 0] then [1, 2]
//! observed (HEAD c722befe, dev profile):
//!   (sx, sy) per cycle: [5, 2] then [1, 2]: the set_many's sy landed a
//!   cycle before its sx. A program write to sx queued from the previous
//!   cycle takes the same path (var_updates are delivered first).

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef};
use graphix_rt::GXEvent;
use netidx::{path::Path, publisher::Value};
use std::time::Duration;
use tokio::sync::mpsc;

const PROG: &str = r#"
let sx = 0;
let sy = 0;
let result = 0
"#;

fn find_bind_id(env: &Env, module: &str, var: &str) -> Result<BindId> {
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {module}::{var}")
}

#[tokio::test(flavor = "current_thread")]
async fn set_many_splits_across_cycles() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(PROG)),
    )]);
    let ctx =
        testing::init_with_resolvers(tx, packages, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let m = gx.compile(literal!("mod test")).await?;
    let sx = find_bind_id(&m.env, "test", "sx")?;
    let sy = find_bind_id(&m.env, "test", "sy")?;
    let w = gx.compile(literal!("(test::sx, test::sy)")).await?;
    let wid = w.exprs.last().context("no expr")?.id;
    tokio::time::sleep(Duration::from_millis(300)).await;
    while rx.try_recv().is_ok() {}
    gx.set(sx, Value::I64(5))?;
    gx.set_many([(sx, Value::I64(1)), (sy, Value::I64(2))])?;
    let mut seen = vec![];
    let deadline = tokio::time::sleep(Duration::from_secs(3));
    tokio::pin!(deadline);
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            b = rx.recv() => match b {
                None => break,
                Some(mut b) => for e in b.drain(..) {
                    if let GXEvent::Updated(id, v) = e && id == wid {
                        seen.push(format!("{v}"));
                    }
                }
            }
        }
    }
    eprintln!("rt-15: (sx, sy) per cycle after set(sx, 5); set_many([(sx, 1), (sy, 2)]): {seen:?}");
    drop(w);
    drop(m);
    ctx.shutdown().await;
    if seen.iter().any(|s| s == "[i64:5, i64:2]") {
        bail!("set_many's sy landed in a cycle without its sx");
    }
    Ok(())
}
