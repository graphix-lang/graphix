//! A runtime on a multi-thread tokio runtime runs on a thread of its own,
//! and ends whichever goes first: its last handle, or the tokio runtime,
//! whose drop ends it while its timers still run, wedged or not.

use anyhow::{Context, Result};
use arcstr::literal;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, Node, Rt, Scope, TagValue, expr::ExprId,
    image::ImageBuf, typ::FnType,
};
use graphix_rt::{GXConfig, GXEvent, GXExt, GXHandle, GXRt};
use netidx_core::pack::PackError;
use poolshark::global::GPooled;
use std::{
    panic,
    sync::{
        Once,
        atomic::{AtomicUsize, Ordering},
        mpsc as smpsc,
    },
    thread,
    time::Duration,
};
use tokio::{runtime::Runtime, sync::mpsc};

/// Long enough that only a hang outlasts it.
const HANG: Duration = Duration::from_secs(120);

/// Panics on a runtime's own thread.
static RUNTIME_PANICS: AtomicUsize = AtomicUsize::new(0);

fn count_runtime_panics() {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let prev = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if thread::current().name() == Some("graphix-runtime") {
                RUNTIME_PANICS.fetch_add(1, Ordering::Relaxed);
            }
            prev(info)
        }))
    })
}

/// An event source whose wait is a timer, and which says when the runtime
/// that owns it has dropped it.
#[derive(Debug, Default)]
struct Probe {
    dropped: Option<smpsc::Sender<()>>,
}

impl GXExt for Probe {
    async fn update_sources(&mut self) -> Result<()> {
        tokio::time::sleep(Duration::from_secs(3600)).await;
        Ok(())
    }
}

impl Drop for Probe {
    fn drop(&mut self) {
        if let Some(tx) = self.dropped.take() {
            let _ = tx.send(());
        }
    }
}

/// Where `test_wedge` says it began to spin.
#[derive(Clone)]
struct Entered(smpsc::Sender<()>);

/// `test_wedge(x)` holds its pass until the runtime stops.
#[derive(Debug)]
struct Wedge(TagValue);

impl<R: Rt> BuiltIn<R> for Wedge {
    const NAME: &str = "test_wedge";
    const EFFECT: Effect = Effect::Sync;

    fn init<'a, 'b, 'c, 'd>(
        _: &'a mut CompileCtx<R>,
        _: &'a FnType,
        _: Option<&'d FnType>,
        _: &'b Scope,
        _: &'c [Node<R>],
        _: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Wedge(TagValue::phantom())))
    }

    fn image_decode(
        _: &mut ExecCtx<'_, R>,
        _: &[Node<R>],
        _: &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R>>, PackError> {
        Err(PackError::InvalidFormat)
    }
}

impl<R: Rt> Apply<R> for Wedge {
    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let tv = from[0].update(ctx).clone();
        if let Some(Entered(tx)) = ctx.libstate.get::<Entered>() {
            let _ = tx.send(());
        }
        while !ctx.interrupted() {
            thread::sleep(Duration::from_millis(1))
        }
        self.0.set(tv)
    }

    fn image_encode(&self, _: &mut ImageBuf) -> std::result::Result<(), PackError> {
        Ok(())
    }

    fn sleep(&mut self, _: &mut ExecCtx<'_, R>) {}
}

/// A runtime on `Probe`, and what says it was dropped.
async fn start() -> Result<(GXHandle<Probe>, smpsc::Receiver<()>)> {
    let mut ctx = GXRt::<Probe>::new_state()?;
    let (dropped, gone) = smpsc::channel();
    ctx.rt.ext.dropped = Some(dropped);
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    // the loop is back at its wait, its timer armed, once this is answered
    gx.get_env().await?;
    Ok((gx, gone))
}

/// Wedge `gx` in a pass; returns once the pass is spinning.
async fn wedge(gx: &GXHandle<Probe>) -> Result<()> {
    let (tx, entered) = smpsc::channel();
    gx.with_ctx(move |ctx| {
        ctx.libstate.set(Entered(tx));
        ctx.register_builtin::<Wedge>()
    })
    .await??;
    gx.compile(literal!("let f = |x: i64| -> i64 'test_wedge;\nf(0)")).await?;
    tokio::task::spawn_blocking(move || entered.recv_timeout(HANG))
        .await?
        .context("the pass never wedged")
}

async fn dropped(gone: smpsc::Receiver<()>) -> Result<()> {
    tokio::task::spawn_blocking(move || gone.recv_timeout(HANG))
        .await?
        .context("the runtime did not end")
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn the_last_handle_ends_the_runtime() -> Result<()> {
    let (gx, gone) = start().await?;
    drop(gx);
    dropped(gone).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn the_last_handle_ends_a_wedged_runtime() -> Result<()> {
    let (gx, gone) = start().await?;
    wedge(&gx).await?;
    drop(gx);
    dropped(gone).await
}

#[tokio::test(flavor = "current_thread")]
async fn the_last_handle_ends_a_runtime_on_one_thread() -> Result<()> {
    let (gx, gone) = start().await?;
    drop(gx);
    dropped(gone).await
}

fn tokio() -> Result<Runtime> {
    Ok(tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()?)
}

/// Drop `rt`, under which the runtime that drops `gone` runs: its drop
/// returns, having ended the runtime, and the runtime's thread did not
/// panic.
fn drop_runtime(rt: Runtime, gone: smpsc::Receiver<()>) -> Result<()> {
    let (done, dropped) = smpsc::channel();
    thread::spawn(move || {
        drop(rt);
        let _ = done.send(());
    });
    dropped.recv_timeout(HANG).context("the tokio runtime's drop hung")?;
    let ended_first = gone.try_recv().is_ok();
    if !ended_first {
        gone.recv_timeout(HANG).context("the runtime did not end")?
    }
    assert_eq!(
        RUNTIME_PANICS.load(Ordering::Relaxed),
        0,
        "the runtime's thread panicked"
    );
    assert!(ended_first, "the runtime outlived the tokio runtime's drop");
    Ok(())
}

#[test]
fn a_tokio_runtime_dropped_first_ends_the_runtime() -> Result<()> {
    count_runtime_panics();
    let rt = tokio()?;
    let (gx, gone) = rt.block_on(start())?;
    drop_runtime(rt, gone)?;
    drop(gx);
    Ok(())
}

#[test]
fn a_tokio_runtime_dropped_first_ends_a_wedged_runtime() -> Result<()> {
    count_runtime_panics();
    let rt = tokio()?;
    let (gx, gone) = rt.block_on(async {
        let (gx, gone) = start().await?;
        wedge(&gx).await?;
        Ok::<_, anyhow::Error>((gx, gone))
    })?;
    drop_runtime(rt, gone)?;
    drop(gx);
    Ok(())
}
