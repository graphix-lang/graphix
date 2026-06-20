//! Integration tests for `GXHandle::interrupt()` / `abort()` — recover
//! from and shut down a wedged runtime. These run on a `multi_thread`
//! runtime: a wedged `do_cycle` blocks one worker (inside
//! `block_in_place`), and the test body — on another worker — fires the
//! control-flag store that breaks the loop. On `current_thread` a wedge
//! would block the only thread and the test could never interrupt it.
//!
//! Coverage note: the only program that wedges with **constant stack**
//! and **bounded memory** is the sync tail loop, so that's what these
//! tests wedge with — both interp (the `GXLambda::update` loop poll) and
//! fused (the kernel's `emit_interrupt_check`). The per-builtin opt-in
//! polls in the array HOF eval loops (`MapQ`/`FoldQ`/`Init`/…) can't be
//! driven to a real timeout here without materializing millions of
//! per-slot nodes (gigabytes; the slot *build* phase is itself the slow,
//! un-polled part), and the fused HOF scaffolds call the very same
//! `emit_interrupt_check` the tail loop exercises. Those polls are
//! behavior-neutral, verified by the 1435×2 differential suite.

use anyhow::{bail, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::CFlag;
use graphix_package_core::testing::{init_with_flags_and_setup, TestCtx};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::{sync::mpsc, time::Duration};

/// An unbounded sync tail loop — `v + 1` wraps at `i64::MAX`, so it
/// never errors out; it just spins forever within one reactive cycle.
/// Fuses to a native rebind-and-jump loop in jit mode, so the jit
/// variants exercise the kernel's `emit_interrupt_check`; node-walks in
/// interp mode, exercising `GXLambda::update`'s loop poll.
const TAIL_LOOP: &str = "{ let rec f = |v: i64| -> i64 f(v + 1); f(0) }";

async fn init_flags(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    flags: BitFlags<CFlag>,
) -> Result<TestCtx> {
    init_with_flags_and_setup(sub, crate::TEST_REGISTER, vec![], flags, |_| {}).await
}

/// Wedge the runtime with `program`, confirm a probe command can't be
/// served while the node loop spins, then `interrupt()` and confirm the
/// same probe completes — the loop aborted to bottom and the runtime
/// kept running.
async fn interrupt_recovers(program: &str, flags: BitFlags<CFlag>) -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    // Drain the event channel so `do_cycle`'s post-loop send never
    // blocks; a full channel would let the runtime service commands via
    // its send-timeout fallback, masking the wedge.
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_flags(tx, flags).await?;
    // `compile` is answered before `do_cycle` runs the program, so this
    // returns even though the program wedges the very next cycle.
    ctx.rt.compile(ArcStr::from(program)).await?;
    // A probe the runtime can't answer while the node loop spins.
    let mut probe = {
        let rt = ctx.rt.clone();
        tokio::spawn(async move { rt.get_env().await })
    };
    let wedged = tokio::time::timeout(Duration::from_millis(500), &mut probe).await;
    if wedged.is_ok() {
        bail!("runtime did not wedge — {program:?} is not an unbounded loop");
    }
    // Break the wedged loop; the runtime survives.
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

/// The motivating bug: a wedged runtime can't be shut down by dropping
/// the handle while commands are in flight (the command futures borrow
/// `&self`). `abort()` is a `&self` store that fires anyway; when the
/// run loop returns, the command response channels drop and every
/// blocked caller gets an error instead of deadlocking.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn abort_unblocks_pending_commands() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let ctx = init_flags(tx, BitFlags::empty()).await?;
    ctx.rt.compile(ArcStr::from(TAIL_LOOP)).await?;
    // Queue several commands; the wedged runtime can't service any of
    // them, so each blocks on its response oneshot.
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
    // `abort()` borrows `&self`, so it fires while the pending commands
    // hold their own shared borrows of the handle — a drop couldn't.
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
