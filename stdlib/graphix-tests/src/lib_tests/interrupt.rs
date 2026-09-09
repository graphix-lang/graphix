//! `GXHandle::interrupt()` / `abort()` recover from and shut down a
//! wedged runtime. These run on a `multi_thread` runtime: the wedged
//! `do_cycle` blocks one worker while the test fires the control flag
//! from another. The sync tail loop is the one program that wedges with
//! constant stack and bounded memory.

use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::CFlag;
use graphix_package_core::testing::{TestCtx, init_with_flags_and_setup};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::{sync::mpsc, time::Duration};

/// An unbounded sync tail loop: `v + 1` wraps, so it spins forever
/// within one cycle. Native in jit mode, node-walked in interp mode.
const TAIL_LOOP: &str = "{ let rec f = |v: i64| -> i64 f(v + 1); f(0) }";

async fn init_flags(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    flags: BitFlags<CFlag>,
) -> Result<TestCtx> {
    init_with_flags_and_setup(sub, crate::TEST_REGISTER, vec![], flags, |_| {}).await
}

/// Wedge the runtime with `program`, confirm a probe cannot be served,
/// then `interrupt()` and confirm the probe completes.
async fn interrupt_recovers(program: &str, flags: BitFlags<CFlag>) -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    // Drain the event channel; a full channel would let the runtime
    // service commands via its send-timeout fallback, masking the wedge.
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_flags(tx, flags).await?;
    // `compile` is answered before `do_cycle` runs the program.
    ctx.rt.compile(ArcStr::from(program)).await?;
    let mut probe = {
        let rt = ctx.rt.clone();
        tokio::spawn(async move { rt.get_env().await })
    };
    let wedged = tokio::time::timeout(Duration::from_millis(500), &mut probe).await;
    if wedged.is_ok() {
        bail!("runtime did not wedge — {program:?} is not an unbounded loop");
    }
    ctx.rt.interrupt();
    match tokio::time::timeout(Duration::from_secs(5), probe).await {
        Err(_) => bail!("runtime did not recover after interrupt() — probe still hung"),
        Ok(Err(e)) => bail!("probe task panicked: {e}"),
        Ok(Ok(Err(e))) => bail!("get_env failed after recovery: {e}"),
        Ok(Ok(Ok(_env))) => {}
    }
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_recovers_tail_loop_interp() -> Result<()> {
    interrupt_recovers(TAIL_LOOP, CFlag::FusionDisabled.into()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_recovers_tail_loop_fused() -> Result<()> {
    interrupt_recovers(TAIL_LOOP, BitFlags::empty()).await
}

/// A wedged runtime cannot be shut down by dropping the handle while
/// commands are in flight; `abort()` is a `&self` store that fires
/// anyway and every blocked caller gets an error.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn abort_unblocks_pending_commands() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_flags(tx, BitFlags::empty()).await?;
    ctx.rt.compile(ArcStr::from(TAIL_LOOP)).await?;
    // Queue several commands; each blocks on its response oneshot.
    let mut pending = Vec::new();
    for _ in 0..3 {
        let rt = ctx.rt.clone();
        pending.push(tokio::spawn(async move { rt.get_env().await.map(|_| ()) }));
    }
    for _ in 0..2 {
        let rt = ctx.rt.clone();
        pending.push(tokio::spawn(async move { rt.env_stats().await.map(|_| ()) }));
    }
    tokio::time::sleep(Duration::from_millis(300)).await;
    ctx.rt.abort();
    for (i, p) in pending.into_iter().enumerate() {
        match tokio::time::timeout(Duration::from_secs(5), p).await {
            Err(_) => bail!("command {i} hung after abort() instead of erroring"),
            Ok(Err(e)) => bail!("command {i} task panicked: {e}"),
            Ok(Ok(Ok(()))) => {
                bail!("command {i} unexpectedly served by a wedged+aborted runtime")
            }
            Ok(Ok(Err(_))) => {}
        }
    }
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}
