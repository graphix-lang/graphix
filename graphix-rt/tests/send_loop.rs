//! A cycle whose batch the subscriber has not taken waits in its send
//! loop, servicing input there: a compile answered in that loop still
//! runs its init in the next cycle, and a set or a call received there
//! lands as soon as the subscriber takes the batch, unprompted.

use anyhow::{Context, Result};
use arcstr::literal;
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};
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

/// A runtime whose subscriber can stop taking batches, its events
/// collected while it takes them.
struct Rig {
    gx: GXHandle<NoExt>,
    open: Arc<AtomicBool>,
    events: mpsc::UnboundedReceiver<GXEvent>,
}

impl Rig {
    async fn new() -> Result<Self> {
        let ctx = GXRt::<NoExt>::new_state()?;
        let (tx, mut rx) = mpsc::channel::<Batch>(1);
        let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
        let open = Arc::new(AtomicBool::new(true));
        let (etx, events) = mpsc::unbounded_channel();
        let o = open.clone();
        tokio::spawn(async move {
            loop {
                if !o.load(Ordering::SeqCst) {
                    tokio::time::sleep(Duration::from_millis(5)).await;
                    continue;
                }
                match tokio::time::timeout(Duration::from_millis(5), rx.recv()).await {
                    Err(_) => (),
                    Ok(None) => break,
                    Ok(Some(mut b)) => {
                        for e in b.drain(..) {
                            let _ = etx.send(e);
                        }
                    }
                }
            }
        });
        Ok(Self { gx, open, events })
    }

    /// The values `id` produced since the last look.
    fn values(&mut self, id: graphix_compiler::expr::ExprId) -> Vec<Value> {
        let mut v = vec![];
        while let Ok(e) = self.events.try_recv() {
            if let GXEvent::Updated(i, x) = e
                && i == id
            {
                v.push(x)
            }
        }
        v
    }

    async fn bind(&self, name: &str) -> Result<graphix_rt::Ref<NoExt>> {
        let env = self.gx.get_env().await?;
        self.gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from([name])).await
    }

    /// Stop taking batches until two cycles fill the channel and the
    /// runtime waits in its send loop.
    async fn stall(&self) -> Result<()> {
        self.open.store(false, Ordering::SeqCst);
        tokio::time::sleep(Duration::from_millis(50)).await;
        self.gx.get_env().await?;
        self.gx.get_env().await?;
        tokio::time::sleep(Duration::from_millis(50)).await;
        Ok(())
    }

    /// Take batches again, and wait with nothing else sent.
    async fn resume(&self) {
        tokio::time::sleep(Duration::from_millis(300)).await;
        self.open.store(true, Ordering::SeqCst);
        tokio::time::sleep(Duration::from_millis(1000)).await;
    }
}

#[tokio::test]
async fn a_set_in_the_send_loop_lands_unprompted() -> Result<()> {
    let mut rig = Rig::new().await?;
    let _m = rig.gx.compile(literal!("let x = 0")).await?;
    let m2 = rig.gx.compile(literal!("x")).await?;
    let xid = m2.exprs[0].id;
    let r = rig.bind("x").await?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    rig.values(xid);
    rig.stall().await?;
    rig.gx.set(r.bid, Value::I64(5))?;
    rig.resume().await;
    assert_eq!(rig.values(xid), [Value::I64(5)]);
    Ok(())
}

#[tokio::test]
async fn a_call_in_the_send_loop_lands_unprompted() -> Result<()> {
    let mut rig = Rig::new().await?;
    let _m = rig.gx.compile(literal!("let h = |x: i64| -> i64 x + 1")).await?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    let r = rig.bind("h").await?;
    let c = rig.gx.compile_callable(r.last.clone().context("h has no value")?).await?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    rig.values(c.expr);
    rig.stall().await?;
    c.call(ValArray::from_iter_exact([Value::I64(5)].into_iter())).await?;
    rig.resume().await;
    assert_eq!(rig.values(c.expr), [Value::I64(6)]);
    Ok(())
}
