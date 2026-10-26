//! rt-notify-idle-full-channel: GXEvent::Idle is lost when the subscriber's
//! channel is full at the idle verdict.
//! Run: copy to graphix-rt/tests/review_rt_1.rs, then
//!   flock ~/tmp/cargo.lock cargo test -p graphix-rt --test review_rt_1 notify -- --nocapture
//! Expected: after the subscriber drains, an `Idle` event arrives (idle arrived = true).
//! Observed: batches [Updated(..1)], [], [] and no Idle within 3 s
//!   ("PROBE notify_idle_with_a_full_channel: idle arrived = false"; the test panics).
use anyhow::Result;
use arcstr::literal;
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

/// A NotifyIdle answered while the subscriber's channel is full.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn notify_idle_with_a_full_channel() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<Batch>(1);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let _c = gx.compile(literal!("1")).await?;
    // let the runtime fill the channel and go idle
    tokio::time::sleep(Duration::from_millis(300)).await;
    gx.notify_idle()?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    // now take everything; the runtime has nothing else to do
    let mut idle = false;
    let deadline = tokio::time::sleep(Duration::from_secs(3));
    tokio::pin!(deadline);
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            b = rx.recv() => match b {
                None => break,
                Some(mut b) => {
                    eprintln!("PROBE batch: {:?}", &*b);
                    for e in b.drain(..) {
                        if matches!(e, GXEvent::Idle) {
                            idle = true
                        }
                    }
                }
            }
        }
    }
    eprintln!("PROBE notify_idle_with_a_full_channel: idle arrived = {idle}");
    assert!(idle, "GXEvent::Idle never arrived after the subscriber drained");
    Ok(())
}

