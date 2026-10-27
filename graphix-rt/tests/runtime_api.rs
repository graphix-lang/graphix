//! The runtime's handle: settling, aborting, a check that panics, the
//! output flag of a compiled statement and the typing of an embedder's
//! writes.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use enumflags2::BitFlags;
use graphix_compiler::{
    Apply, BindId, BuiltIn, CFlag, CompileCtx, Effect, ExecCtx, Node, Rt, Scope,
    UserEvent,
    expr::{
        Load, ModPath, ModuleResolver, Origin, Probes, Resolution, ResolverRef, VfsEntry,
        VfsResolver,
    },
    rep,
    typ::FnType,
};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_core::{pack::PackError, path::Path};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::{future::Future, pin::Pin, time::Duration};
use tokio::{sync::mpsc, time::timeout};

type Batch = GPooled<Vec<GXEvent>>;
type Watch = futures::channel::mpsc::Sender<GPooled<Vec<(BindId, Value)>>>;

async fn start(
    flags: BitFlags<CFlag>,
) -> Result<(GXHandle<NoExt>, mpsc::Receiver<Batch>)> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, rx) = mpsc::channel::<Batch>(100);
    let gx = GXConfig::builder(ctx, tx).flags(flags).build()?.start().await?;
    Ok((gx, rx))
}

/// A runtime holding one watch of its own, and the watch's sender.
async fn watched() -> Result<(GXHandle<NoExt>, Watch)> {
    let (gx, mut rx) = start(BitFlags::empty()).await?;
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let (wtx, wrx) = futures::channel::mpsc::channel(1);
    gx.with_ctx(move |ctx| ctx.rt.watch_var(wrx)).await?;
    Ok((gx, wtx))
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn settles_when_a_watch_ends() -> Result<()> {
    let (gx, wtx) = watched().await?;
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
    let (gx, _wtx) = watched().await?;
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

#[tokio::test]
async fn a_use_is_not_an_output() -> Result<()> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, _rx) = mpsc::channel::<Batch>(100);
    let vfs = [(Path::from("/m.gx"), VfsEntry::from(literal!("let x = 1")))];
    let gx = GXConfig::builder(ctx, tx)
        .resolvers(vec![VfsResolver::new(AHashMap::from_iter(vfs))])
        .build()?
        .start()
        .await?;
    let m = gx.compile(literal!("mod m")).await?;
    assert!(!m.exprs[0].output, "a mod statement is an output");
    let u = gx.compile(literal!("use m")).await?;
    assert!(!u.exprs[0].output, "a use statement is an output");
    let e = gx.compile(literal!("m::x + 1")).await?;
    assert!(e.exprs[0].output, "an expression is not an output");
    Ok(())
}

/// A resolver that panics: a bug in the check of one buffer.
#[derive(Debug)]
struct Panics;

impl ModuleResolver for Panics {
    fn resolve<'a>(
        &'a self,
        _: &'a ModPath,
        _: &'a triomphe::Arc<Origin>,
        _: &'a Path,
        _: &'a mut Vec<anyhow::Error>,
        _: Option<&'a Probes>,
    ) -> Pin<Box<dyn Future<Output = Resolution> + Send + Sync + 'a>> {
        Box::pin(async { panic!("a resolver bug") })
    }
}

/// A check that panics answers the panic, and the runtime checks and
/// compiles on.
#[tokio::test(flavor = "multi_thread")]
async fn a_panicking_check_leaves_the_runtime_checking() -> Result<()> {
    let (gx, _rx) = start(BitFlags::empty()).await?;
    let panics: ResolverRef = std::sync::Arc::new(Panics);
    let src = Load::Text(literal!("mod m;\nm::x\n"));
    let e = gx.check_with_resolvers(src, vec![panics], None).await.err();
    let e = e.context("a panicking check passed")?;
    assert!(format!("{e:#}").contains("the check panicked: a resolver bug"), "{e:#}");
    gx.check(Load::Text(literal!("let x = 1;\nx + 1\n")), None).await?;
    let c = gx.compile(literal!("1 + 1")).await?;
    assert!(c.exprs[0].output);
    Ok(())
}

/// A builtin whose binding panics: a bug in the check of a definition's
/// body.
struct PanicsAtBind;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for PanicsAtBind {
    const NAME: &str = "test_panics_at_bind";
    const EFFECT: Effect = Effect::Sync;

    fn init<'a, 'b, 'c, 'd>(
        _: &'a mut CompileCtx<R, E>,
        _: &'a FnType,
        _: Option<&'d FnType>,
        _: &'b Scope,
        _: &'c [Node<R, E>],
        _: graphix_compiler::expr::ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        panic!("a builtin bug")
    }

    fn image_decode(
        _: &mut ExecCtx<'_, R, E>,
        _: &[Node<R, E>],
        _: &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R, E>>, PackError> {
        Err(PackError::InvalidFormat)
    }
}

/// A check that panics inside a definition's body leaves no definition
/// open: the next check starts from an empty resolution scratch.
#[tokio::test(flavor = "multi_thread")]
async fn a_check_panicking_in_a_definition_leaves_none_open() -> Result<()> {
    let (gx, _rx) = start(BitFlags::empty()).await?;
    gx.with_ctx(|ctx| ctx.register_builtin::<PanicsAtBind>()).await??;
    let src = "let boom = |x: i64| -> i64 'test_panics_at_bind;\n\
               let f = |x: i64| boom(x);\nf(1)\n";
    let e = gx.check(Load::Text(ArcStr::from(src)), None).await.err();
    let e = e.context("a panicking check passed")?;
    assert!(format!("{e:#}").contains("the check panicked: a builtin bug"), "{e:#}");
    gx.check(Load::Text(literal!("let g = |x: i64| x + 1;\ng(1)\n")), None).await?;
    Ok(())
}

/// What `id` produced since the receiver was last drained.
async fn produced(
    rx: &mut mpsc::Receiver<Batch>,
    id: graphix_compiler::expr::ExprId,
) -> Vec<Value> {
    tokio::time::sleep(Duration::from_millis(500)).await;
    let mut seen = vec![];
    while let Ok(mut b) = rx.try_recv() {
        for e in b.drain(..) {
            if let GXEvent::Updated(i, v) = e
                && i == id
            {
                seen.push(v)
            }
        }
    }
    seen
}

/// A string set into an `i64` binding read by a function (fused, or
/// node-walked) is refused, alone or in a set, and the runtime goes on.
async fn wrong_typed_set(flags: BitFlags<CFlag>) -> Result<()> {
    let (gx, mut rx) = start(flags).await?;
    let m = gx
        .compile(literal!(
            "let x: i64 = 0; let y: i64 = 0; let f = |a: i64| -> i64 a * 2 + 1; f(x) + y"
        ))
        .await?;
    let out = m.exprs.last().context("no result")?.id;
    produced(&mut rx, out).await;
    let env = gx.get_env().await?;
    let x = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["x"])).await?;
    let y = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["y"])).await?;
    gx.set(x.bid, Value::from("hello"))?;
    gx.set_many([(y.bid, Value::I64(10)), (x.bid, Value::from("hello"))])?;
    assert_eq!(produced(&mut rx, out).await, vec![], "a wrong-typed write landed");
    gx.set(x.bid, Value::I64(3))?;
    assert_eq!(produced(&mut rx, out).await, vec![Value::I64(7)]);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_wrong_typed_set_is_refused_jit() -> Result<()> {
    wrong_typed_set(BitFlags::empty()).await
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_wrong_typed_set_is_refused_interp() -> Result<()> {
    wrong_typed_set(CFlag::FusionDisabled.into()).await
}

/// A write through a place reference is checked by the root it makes.
#[tokio::test]
async fn a_wrong_typed_set_deref_is_refused() -> Result<()> {
    let (gx, mut rx) = start(BitFlags::empty()).await?;
    let m = gx.compile(literal!("let s = {a: 1, b: 2}; let r = &mut s.a; s")).await?;
    let sid = m.exprs[2].id;
    produced(&mut rx, sid).await;
    let env = gx.get_env().await?;
    let r = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["r"])).await?;
    let cell = r.last.clone().context("r has no value")?.cast_to::<BindId>()?;
    gx.set_deref(cell, Value::from("hello"))?;
    assert_eq!(produced(&mut rx, sid).await, vec![], "a wrong-typed patch landed");
    gx.set_deref(cell, Value::I64(5))?;
    let st =
        rep::structure([(literal!("a"), Value::I64(5)), (literal!("b"), Value::I64(2))]);
    assert_eq!(produced(&mut rx, sid).await, vec![st]);
    // a writable reference to an expression is its own cell, of its own type
    let m = gx.compile(literal!("let c = &mut 0; let y = *c; y")).await?;
    let yid = m.exprs[2].id;
    produced(&mut rx, yid).await;
    let env = gx.get_env().await?;
    let c = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["c"])).await?;
    let cell = c.last.clone().context("c has no value")?.cast_to::<BindId>()?;
    gx.set_deref(cell, Value::from("hello"))?;
    assert_eq!(produced(&mut rx, yid).await, vec![], "a wrong-typed write landed");
    gx.set_deref(cell, Value::I64(5))?;
    assert_eq!(produced(&mut rx, yid).await, vec![Value::I64(5)]);
    Ok(())
}

/// A write through a `Ref` lands though the `Ref` drops before the write
/// is applied: the drop deletes the reference node, never the write.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_write_through_a_dropped_ref_lands() -> Result<()> {
    let (gx, mut rx) = start(BitFlags::empty()).await?;
    let m = gx.compile(literal!("let x: i64 = 0; x")).await?;
    let out = m.exprs.last().context("no result")?.id;
    produced(&mut rx, out).await;
    let env = gx.get_env().await?;
    let bid =
        gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["x"])).await?.bid;
    for i in 1..=300i64 {
        let mut r = gx.compile_ref(bid).await?;
        r.set(i)?;
        drop(r);
        let arrived = timeout(Duration::from_secs(5), async {
            loop {
                let Some(mut b) = rx.recv().await else { return false };
                for e in b.drain(..) {
                    if let GXEvent::Updated(id, Value::I64(n)) = e
                        && id == out
                        && n == i
                    {
                        return true;
                    }
                }
            }
        })
        .await;
        assert!(
            matches!(arrived, Ok(true)),
            "the write of {i} through a dropped ref was lost"
        );
    }
    Ok(())
}
