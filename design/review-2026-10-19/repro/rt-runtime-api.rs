//! review 2026-10-19 (rt): the run loop misses two wakeups.
//!
//! 1. settles_when_a_watch_ends: a watch stream that ends while the run
//!    loop waits is reaped inside `SelectAll::poll_next`, which then
//!    returns Pending; `GXRt::outstanding` is never recomputed, so a
//!    pending `wait_settled` hangs until some other event.
//! 2. abort_answers_a_pending_settle: `GXHandle::abort` only sets a flag;
//!    an idle run loop never sees it, so a pending `wait_settled` (its
//!    docs: "pending commands resolve to errors") hangs, and the runtime
//!    keeps every source open, until some other event.
//!
//! Command: copy to graphix-rt/tests/review_rt_1.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-rt --test review_rt_1 -- --nocapture
//! Expected: all pass. Observed at 9445a1d21: settles_when_a_watch_ends fails
//!   ("wait_settled did not answer after the only watch ended"),
//!   abort_answers_a_pending_settle fails ("still pending 3 s after abort()"),
//!   a_wrong_typed_set_jit fails (kernel.rs panics: "kernel param `x`: runtime
//!   String(\"hello\") does not match the compiled Scalar(I64) slot"; the runtime
//!   is dead), a_wrong_typed_set_interp passes (the node-walk drops the update).

use anyhow::{Context, Result, bail};
use graphix_compiler::{BindId, Rt};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::{sync::mpsc, time::timeout};

type Watch = futures::channel::mpsc::Sender<GPooled<Vec<(BindId, Value)>>>;

async fn start() -> Result<(GXHandle<NoExt>, Watch)> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    let (wtx, wrx) = futures::channel::mpsc::channel(1);
    gx.with_ctx(move |ctx| ctx.rt.watch_var(wrx)).await?;
    Ok((gx, wtx))
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn settles_when_a_watch_ends() -> Result<()> {
    let (gx, wtx) = start().await?;
    let settled = gx.wait_settled();
    tokio::pin!(settled);
    if timeout(Duration::from_millis(300), &mut settled).await.is_ok() {
        bail!("settled with a live watch")
    }
    drop(wtx);
    timeout(Duration::from_secs(3), &mut settled)
        .await
        .context("wait_settled did not answer after the only watch ended")??;
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn abort_answers_a_pending_settle() -> Result<()> {
    let (gx, _wtx) = start().await?;
    let settled = gx.wait_settled();
    tokio::pin!(settled);
    if timeout(Duration::from_millis(300), &mut settled).await.is_ok() {
        bail!("settled with a live watch")
    }
    gx.abort();
    match timeout(Duration::from_secs(3), &mut settled).await {
        Err(_) => bail!("wait_settled still pending 3 s after abort()"),
        Ok(Ok(())) => bail!("an aborted runtime answered the settle"),
        Ok(Err(_)) => Ok(()),
    }
}

/// 3. a_wrong_typed_set_is_refused: `GXHandle::set` (and `Ref::set`,
///    `TRef::set`, `set_many`, `set_deref`) checks nothing, while
///    `Callable::call` checks its arguments; a string written into an
///    `i64` binding reaches a fused kernel's `stage`, which panics and
///    takes the runtime down. The node-walk computes on it instead.
async fn wrong_typed_set(flags: enumflags2::BitFlags<graphix_compiler::CFlag>) -> Result<Vec<Value>> {
    use graphix_compiler::{Scope, expr::ModPath};
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    let gx = GXConfig::builder(ctx, tx).flags(flags).build()?.start().await?;
    let m = gx
        .compile(arcstr::literal!(
            "let x: i64 = 0; let f = |a: i64| -> i64 a * 2 + 1; f(x)"
        ))
        .await?;
    let out = m.exprs.last().context("no result")?.id;
    tokio::time::sleep(Duration::from_millis(300)).await;
    let env = gx.get_env().await?;
    let x = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["x"])).await?;
    eprintln!("fused regions: {}", gx.fusion_stats().await?.fused);
    gx.set(x.bid, Value::from("hello"))?;
    tokio::time::sleep(Duration::from_millis(500)).await;
    let mut seen = vec![];
    while let Ok(mut b) = rx.try_recv() {
        for e in b.drain(..) {
            if let GXEvent::Updated(i, v) = e
                && i == out
            {
                seen.push(v)
            }
        }
    }
    timeout(Duration::from_secs(3), gx.get_env())
        .await
        .context("runtime wedged")?
        .context("the runtime died after a wrong-typed set")?;
    Ok(seen)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_wrong_typed_set_jit() -> Result<()> {
    let seen = wrong_typed_set(enumflags2::BitFlags::empty()).await?;
    eprintln!("jit: {seen:?}");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_wrong_typed_set_interp() -> Result<()> {
    let seen = wrong_typed_set(graphix_compiler::CFlag::FusionDisabled.into()).await?;
    eprintln!("interp: {seen:?}");
    Ok(())
}
