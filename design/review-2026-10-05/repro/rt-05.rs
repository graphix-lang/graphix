//! rt-05: a compile serviced while do_cycle waits on a full subscriber
//! channel loses its init update.
//!
//! Command (with this file copied to graphix-rt/tests/review_rt_05.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-rt --test review_rt_05 -- --nocapture
//!
//! A runtime with no packages and a one-slot subscriber channel. Two
//! `get_env` requests each run a cycle; the first cycle's batch fills
//! the slot, so the second cycle's do_cycle sits in its 100 ms
//! send_timeout loop, servicing input there. `let k = 7` and `40 + 2`
//! are compiled while it waits; then the channel is drained and `k`
//! and `40 + 3` are compiled in the ordinary path. The control drains
//! from the start.
//!
//! Expected (both runs): `40 + 2` delivers 42, `k` delivers 7 and
//! `40 + 3` delivers 43.
//! Observed at c722befe (debug build): the test fails with
//!   stall=false: two compiles answered in 2.482337ms
//!   control (drained):  `40 + 2` -> Some(I64(42)), `k` -> Some(I64(7)), `40 + 3` -> Some(I64(43))
//!   stall=true: two compiles answered in 203.459575ms
//!   stall=true: `k` and `40 + 3` answered in 1.470634ms
//!   stalled subscriber: `40 + 2` -> None, `k` -> None, `40 + 3` -> Some(I64(43))
//! The two compiles answered in the send loop (one 100 ms timeout each)
//! never ran their init: `40 + 2` never delivers and `k` was never set,
//! though `k` itself compiled in the ordinary path after the drain.

use anyhow::Result;
use arcstr::literal;
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::time::{Duration, Instant};
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

async fn probe(stall: bool) -> Result<(Option<Value>, Option<Value>, Option<Value>)> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, rx) = mpsc::channel::<Batch>(1);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let (out_tx, mut out_rx) = mpsc::unbounded_channel();
    let mut rx = Some(rx);
    if !stall {
        drain(rx.take().unwrap(), out_tx.clone());
    }
    gx.get_env().await?;
    gx.get_env().await?;
    let t = Instant::now();
    let k = gx.compile(literal!("let k = 7")).await?;
    let c = gx.compile(literal!("40 + 2")).await?;
    eprintln!("stall={stall}: two compiles answered in {:?}", t.elapsed());
    if let Some(rx) = rx.take() {
        drain(rx, out_tx.clone());
    }
    // the send loop has let go: `k` compiles in the ordinary path
    tokio::time::sleep(Duration::from_millis(300)).await;
    let t = Instant::now();
    let r = gx.compile(literal!("k")).await?;
    let d = gx.compile(literal!("40 + 3")).await?;
    eprintln!("stall={stall}: `k` and `40 + 3` answered in {:?}", t.elapsed());
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
async fn compile_in_send_loop_keeps_init() -> Result<()> {
    let control = probe(false).await?;
    eprintln!(
        "control (drained):  `40 + 2` -> {:?}, `k` -> {:?}, `40 + 3` -> {:?}",
        control.0, control.1, control.2
    );
    let stalled = probe(true).await?;
    eprintln!(
        "stalled subscriber: `40 + 2` -> {:?}, `k` -> {:?}, `40 + 3` -> {:?}",
        stalled.0, stalled.1, stalled.2
    );
    let want = (Some(Value::I64(42)), Some(Value::I64(7)), Some(Value::I64(43)));
    assert_eq!(control, want);
    assert_eq!(stalled, control, "a compile serviced in the send loop lost its init");
    Ok(())
}
