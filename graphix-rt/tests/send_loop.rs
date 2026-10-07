//! A cycle whose batch the subscriber has not taken waits in its send
//! loop, servicing input there: a compile answered in that loop still
//! runs its init in the next cycle.

use anyhow::Result;
use arcstr::literal;
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

fn drain(mut rx: mpsc::Receiver<Batch>, out: mpsc::UnboundedSender<GXEvent>) {
    tokio::spawn(async move {
        while let Some(mut b) = rx.recv().await {
            for e in b.drain(..) {
                let _ = out.send(e);
            }
        }
    });
}

/// `40 + 2`, `k` and `40 + 3`, the first two (`let k = 7` with it)
/// compiled while the subscriber is drained or, under `stall`, while a
/// one-slot channel holds the last cycle's batch.
async fn values(stall: bool) -> Result<(Option<Value>, Option<Value>, Option<Value>)> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, rx) = mpsc::channel::<Batch>(1);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let (out_tx, mut out_rx) = mpsc::unbounded_channel();
    let mut rx = Some(rx);
    if !stall {
        drain(rx.take().unwrap(), out_tx.clone());
    }
    // each runs a cycle; the first's batch fills the slot
    gx.get_env().await?;
    gx.get_env().await?;
    let k = gx.compile(literal!("let k = 7")).await?;
    let c = gx.compile(literal!("40 + 2")).await?;
    if let Some(rx) = rx.take() {
        drain(rx, out_tx.clone());
    }
    tokio::time::sleep(Duration::from_millis(300)).await;
    let r = gx.compile(literal!("k")).await?;
    let d = gx.compile(literal!("40 + 3")).await?;
    let (cid, rid, did) = (c.exprs[0].id, r.exprs[0].id, d.exprs[0].id);
    let (mut cv, mut rv, mut dv) = (None, None, None);
    let deadline = tokio::time::sleep(Duration::from_secs(1));
    tokio::pin!(deadline);
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            e = out_rx.recv() => match e {
                Some(GXEvent::Updated(id, v)) if id == cid => cv = Some(v),
                Some(GXEvent::Updated(id, v)) if id == rid => rv = Some(v),
                Some(GXEvent::Updated(id, v)) if id == did => dv = Some(v),
                Some(_) => (),
                None => break,
            }
        }
    }
    drop((k, c, r, d));
    Ok((cv, rv, dv))
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_compile_in_the_send_loop_keeps_its_init() -> Result<()> {
    let want = (Some(Value::I64(42)), Some(Value::I64(7)), Some(Value::I64(43)));
    assert_eq!(values(false).await?, want);
    assert_eq!(values(true).await?, want);
    Ok(())
}
