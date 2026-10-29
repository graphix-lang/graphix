//! `GXHandle::interrupt()` stops a runtime for good, wedged or idle, and
//! `abort()` shuts one down. These run on a `multi_thread` runtime: the
//! wedged `do_cycle` blocks one worker while the test fires the control
//! flag from another. The sync tail loop wedges: fused it is a native loop
//! in constant memory; node-walked every call is a retained activation,
//! so its memory grows until the interrupt lands.

use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, StopCause};
use graphix_package_core::testing::{TestCtx, config};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::{sync::mpsc, time::Duration};

/// An unbounded sync tail loop: `v + 1` wraps, so it spins forever
/// within one cycle. Native in jit mode, node-walked in interp mode.
const TAIL_LOOP: &str = "{ let rec f = |v: i64| -> i64 f(v + 1); f(0) }";

/// An unbounded recursion that is not a tail call: each level waits on
/// the next, so the interrupt lands with every level still open.
const DEEP_RECURSION: &str = "{ let rec f = |n: i64| -> i64 \
     select n < 0 { true => 0, false => 1 + f(n + 1) }; f(0) }";

/// What a call on an interrupted runtime's handle fails with.
const STOPPED: &str = "the runtime was interrupted and stopped";

async fn init_flags(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    flags: BitFlags<CFlag>,
) -> Result<TestCtx> {
    TestCtx::start(config(sub, crate::TEST_REGISTER, vec![], |_| {})?.flags(flags)).await
}

/// A runtime the interrupt stopped: its task ends, and every call on its
/// handle fails, naming the interrupt.
async fn stopped_for_good(ctx: &TestCtx) -> Result<()> {
    if tokio::time::timeout(Duration::from_secs(10), ctx.rt.ended()).await.is_err() {
        bail!("the interrupted runtime's task did not end");
    }
    if ctx.rt.control().stopped_by() != Some(StopCause::Interrupt) {
        bail!("stopped by {:?}, not the interrupt", ctx.rt.control().stopped_by());
    }
    match ctx.rt.compile(ArcStr::from("1")).await {
        Ok(_) => bail!("an interrupted runtime compiled"),
        Err(e) if format!("{e}") == STOPPED => (),
        Err(e) => bail!("a call failed with {e}, not that the runtime stopped"),
    }
    match ctx.rt.get_env().await {
        Ok(_) => bail!("an interrupted runtime answered"),
        Err(e) if format!("{e}") == STOPPED => Ok(()),
        Err(e) => bail!("a call failed with {e}, not that the runtime stopped"),
    }
}

/// Wedge the runtime with `program`, confirm a probe cannot be served,
/// then `interrupt()`: the probe fails and the runtime is gone.
async fn interrupt_stops(program: &str, flags: BitFlags<CFlag>) -> Result<()> {
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
    // well before the deep recursion reaches the tests' stack budget
    let wedged = tokio::time::timeout(Duration::from_millis(200), &mut probe).await;
    if wedged.is_ok() {
        bail!("runtime did not wedge — {program:?} is not an unbounded loop");
    }
    ctx.rt.interrupt();
    match tokio::time::timeout(Duration::from_secs(5), probe).await {
        Err(_) => bail!("the probe still hung after interrupt()"),
        Ok(Err(e)) => bail!("probe task panicked: {e}"),
        Ok(Ok(Ok(_))) => bail!("the interrupted runtime answered the probe"),
        Ok(Ok(Err(e))) if format!("{e}") == STOPPED => (),
        Ok(Ok(Err(e))) => bail!("the probe failed with {e}, not the interrupt"),
    }
    stopped_for_good(&ctx).await?;
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_stops_tail_loop_interp() -> Result<()> {
    interrupt_stops(TAIL_LOOP, CFlag::FusionDisabled.into()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_stops_tail_loop_fused() -> Result<()> {
    interrupt_stops(TAIL_LOOP, BitFlags::empty()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_stops_deep_recursion_interp() -> Result<()> {
    interrupt_stops(DEEP_RECURSION, CFlag::FusionDisabled.into()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_stops_deep_recursion_fused() -> Result<()> {
    interrupt_stops(DEEP_RECURSION, BitFlags::empty()).await
}

/// An interrupt stops an idle runtime as it stops a busy one: a program
/// that would go on counting never counts again.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn interrupt_stops_an_idle_runtime() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init_flags(tx, BitFlags::empty()).await?;
    let mut held = Vec::new();
    for stmt in ["let cnt = 0", "cnt <- sys::time::timer(duration:50.ms, true) ~ cnt + 1"]
    {
        held.push(ctx.rt.compile(ArcStr::from(stmt)).await?);
    }
    held.push(ctx.rt.compile(ArcStr::from("cnt")).await?);
    let cnt = held.last().unwrap().exprs[0].id;
    let Some(_) = value_of(&mut rx, cnt, Duration::from_secs(10)).await else {
        bail!("cnt never counted")
    };
    ctx.rt.wait_idle().await?;
    ctx.rt.interrupt();
    stopped_for_good(&ctx).await?;
    if let Some(v) = value_of(&mut rx, cnt, Duration::from_millis(500)).await {
        bail!("cnt counted to {v} after the interrupt")
    }
    drop(held);
    ctx.shutdown().await;
    Ok(())
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

/// The next value of the root `id`, within `wait`.
async fn value_of(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    id: graphix_compiler::expr::ExprId,
    wait: Duration,
) -> Option<netidx::publisher::Value> {
    let deadline = tokio::time::Instant::now() + wait;
    loop {
        let batch = tokio::time::timeout_at(deadline, rx.recv()).await.ok()??;
        for e in batch.iter() {
            if let GXEvent::Updated(eid, v) = e
                && *eid == id
            {
                return Some(graphix_package_core::testing::data(v.clone()));
            }
        }
    }
}
