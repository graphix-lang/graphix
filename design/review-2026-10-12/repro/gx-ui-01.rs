//! gx-ui-01: gui::radio wraps `on_select` as `|v: Any| f(*value)`, a
//! callback that ignores its argument. Through the embedder-callable
//! path (what the GUI uses for every widget handler) the wrapped callback
//! must run once per call and never at the callable's init. The wrapper
//! below is radio.gx's, verbatim.
//!
//! Run: cp design/review-2026-10-12/repro/gx-ui-01.rs stdlib/graphix-tests/tests/review_gx_ui_01.rs
//!      cargo test -p graphix-tests --test review_gx_ui_01 -- --nocapture
//! Expected: hits 0 at init, then 1, 2, 3 after each call.
//! Observed: "hits: at init i64:1, after 1 call i64:1, 2 calls i64:1, 3 calls i64:1".

use ahash::AHashMap;
use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx, find_bind_id};
use graphix_rt::GXEvent;
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use std::time::Duration;
use tokio::sync::mpsc;

const PROG: &str = r#"
let hits = 0;
let radio_like = |#on_select: [fn(v: i64) -> Any, null] = null, value: &i64| {
  let on_select = select on_select {
    null as _ => null,
    f => |v: Any| f(*value)
  };
  on_select
};
let cb = radio_like(#on_select: |v| hits <- v ~ hits + 1, &5);
let result = hits
"#;

async fn runtime(
    tx: mpsc::Sender<poolshark::global::GPooled<Vec<GXEvent>>>,
) -> Result<TestCtx> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let tbl =
        AHashMap::from_iter([(Path::from("/test.gx"), VfsEntry::from(ArcStr::from(PROG)))]);
    testing::init_with_resolvers(tx, packages, vec![VfsResolver::new(tbl)]).await
}

#[tokio::test(flavor = "multi_thread")]
async fn radio_on_select_runs_once_per_click() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let m = gx.compile(literal!("mod test")).await?;
    let hits = find_bind_id(&m.env, "test::hits")?;
    let read = async || -> Result<Value> {
        gx.wait_idle().await?;
        tokio::time::sleep(Duration::from_millis(100)).await;
        gx.wait_idle().await?;
        gx.compile_ref(hits).await?.last.clone().context("no hits")
    };
    let r = gx.compile_ref(find_bind_id(&m.env, "test::cb")?).await?;
    let cb = gx.compile_callable(r.last.clone().context("no cb")?).await?;
    let at_init = read().await?;
    cb.call(ValArray::from_iter_exact([Value::I64(1)].into_iter())).await?;
    let after_one = read().await?;
    cb.call(ValArray::from_iter_exact([Value::I64(2)].into_iter())).await?;
    let after_two = read().await?;
    cb.call(ValArray::from_iter_exact([Value::I64(3)].into_iter())).await?;
    let after_three = read().await?;
    eprintln!(
        "hits: at init {at_init}, after 1 call {after_one}, 2 calls {after_two}, 3 calls {after_three}"
    );
    drop((r, cb, m));
    ctx.shutdown().await;
    drain.abort();
    assert_eq!(
        (at_init, after_one, after_two, after_three),
        (Value::I64(0), Value::I64(1), Value::I64(2), Value::I64(3))
    );
    Ok(())
}
