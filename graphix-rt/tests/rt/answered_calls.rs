//! A call's answer comes after the writes its handler made: a control
//! that waits on the answer sees the handler's write first.

use anyhow::{Context, Result};
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

/// The updates of `v` and of the call's answer, in the order they
/// arrive, after one answered call of `h` with "a".
async fn heard(code: &'static str) -> Result<Vec<&'static str>> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<Batch>(100);
    let gx: GXHandle<NoExt> = GXConfig::builder(ctx, tx).build()?.start().await?;
    let m = gx.compile(arcstr::ArcStr::from(code)).await?;
    let vid = m.exprs.last().context("no result expression")?.id;
    tokio::time::sleep(Duration::from_millis(300)).await;
    let env = gx.get_env().await?;
    let h = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["h"])).await?;
    let h = gx.compile_callable(h.last.clone().context("h has no value")?).await?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    while rx.try_recv().is_ok() {}
    h.call_answered(ValArray::from_iter([Value::from("a")]))?;
    tokio::time::sleep(Duration::from_millis(500)).await;
    let mut order = vec![];
    while let Ok(mut b) = rx.try_recv() {
        for e in b.drain(..) {
            match e {
                GXEvent::Updated(i, v) if i == vid && v == Value::from("a") => {
                    order.push("write")
                }
                GXEvent::Updated(i, _) if i == h.answer => order.push("answer"),
                _ => (),
            }
        }
    }
    Ok(order)
}

#[tokio::test]
async fn a_handlers_write_comes_before_its_answer() -> Result<()> {
    let order = heard("let v = \"\"; let h = |s: string| v <- s; v").await?;
    assert_eq!(order, ["write", "answer"]);
    Ok(())
}

#[tokio::test]
async fn a_handler_that_writes_nothing_answers() -> Result<()> {
    let order = heard("let v = \"\"; let h = |s: string| s; v").await?;
    assert_eq!(order, ["answer"]);
    Ok(())
}
