//! A runtime runs every update pass on one thread of its own, so the
//! caches a pass keeps per thread (free arrays, pools, an allocator
//! arena) are one thread's however its task moves between workers.

use anyhow::{Context, Result};
use arcstr::literal;
use graphix_compiler::{
    Apply, BuiltIn, CompileCtx, Effect, ExecCtx, Node, ParMode, Rt, Scope, TagValue,
    expr::ExprId, image::ImageBuf, typ::FnType,
};
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use netidx_core::pack::PackError;
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::global::GPooled;
use std::{collections::HashSet, thread::ThreadId, time::Duration};
use tokio::{sync::mpsc, time::timeout};

/// The threads `test_pass_thread` ran on.
static RAN_ON: Mutex<Option<HashSet<ThreadId>>> = Mutex::new(None);

/// `test_pass_thread(x)` is `x`, and notes the thread it ran on.
#[derive(Debug)]
struct PassThread(TagValue);

impl<R: Rt> BuiltIn<R> for PassThread {
    const NAME: &str = "test_pass_thread";
    const EFFECT: Effect = Effect::Sync;

    fn init<'a, 'b, 'c, 'd>(
        _: &'a mut CompileCtx<R>,
        _: &'a FnType,
        _: Option<&'d FnType>,
        _: &'b Scope,
        _: &'c [Node<R>],
        _: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(PassThread(TagValue::phantom())))
    }

    fn image_decode(
        _: &mut ExecCtx<'_, R>,
        _: &[Node<R>],
        _: &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R>>, PackError> {
        Err(PackError::InvalidFormat)
    }
}

impl<R: Rt> Apply<R> for PassThread {
    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let tv = from[0].update(ctx).clone();
        RAN_ON.lock().get_or_insert_default().insert(std::thread::current().id());
        self.0.set(tv)
    }

    fn image_encode(&self, _: &mut ImageBuf) -> std::result::Result<(), PackError> {
        Ok(())
    }

    fn sleep(&mut self, _: &mut ExecCtx<'_, R>) {}
}

const CYCLES: i64 = 2000;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "until a runtime runs every pass on one thread: runtime-thread-spread-01"]
async fn every_pass_runs_on_one_thread() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    ctx.control.set_par_mode(ParMode::Off);
    let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    gx.with_ctx(|ctx| ctx.register_builtin::<PassThread>()).await??;
    let c = gx
        .compile(literal!(
            "let n = 0;\n\
             n <- select n { k if k < 2000 => k + 1, _ => never() };\n\
             let f = |x: i64| -> i64 'test_pass_thread;\n\
             f(n)"
        ))
        .await?;
    let out = c.exprs.last().context("no result")?.id;
    'cycles: loop {
        let mut batch = timeout(Duration::from_secs(120), rx.recv())
            .await
            .context("the program stalled")?
            .context("the runtime ended")?;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, Value::I64(n)) = e
                && id == out
                && n >= CYCLES
            {
                break 'cycles;
            }
        }
    }
    let threads = RAN_ON.lock().as_ref().map_or(0, |t| t.len());
    assert_eq!(threads, 1, "{CYCLES} passes ran on {threads} threads");
    Ok(())
}
