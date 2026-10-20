//! The embedder's API over a running program: callables, their
//! arguments, and `set_many`.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx, find_bind_id};
use graphix_rt::{GXEvent, GXHandle, NoExt};
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use std::time::Duration;
use tokio::sync::mpsc;

const PROG: &str = r#"
let h = 'a: Number |x: 'a| -> 'a x;
let g = |x| x;
let k = |x| x;
let i = |x: i64| -> i64 x;
let z = || 42;
let d = |#k: i64 = 5| k + 1;
let kref: &fn(x: i64) -> i64 = &k;
let sx = 0;
let sy = 0;
let result = 0
"#;

async fn runtime(
    tx: mpsc::Sender<poolshark::global::GPooled<Vec<GXEvent>>>,
) -> Result<TestCtx> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(PROG)),
    )]);
    testing::TestCtx::start(testing::config(
        tx,
        packages,
        vec![VfsResolver::new(tbl)],
        |_| {},
    )?)
    .await
}

async fn compiles(gx: &GXHandle<NoExt>, text: &'static str) -> Result<()> {
    gx.compile(ArcStr::from(text)).await.map(|_| ()).with_context(|| format!("`{text}`"))
}

/// A callable instantiates its lambda's signature as a call does, so
/// calls compiled after it check against the definition as before.
#[tokio::test(flavor = "multi_thread")]
async fn a_callable_leaves_its_definitions_signature_alone() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let m = gx.compile(literal!("mod test")).await?;
    let mut held = Vec::new();
    for (name, uses) in [
        ("test::h", ["test::h(2) + 2", "test::h(2.5) + 2.5"]),
        ("test::g", ["test::g(2) + 2", "str::len(test::g(\"t\"))"]),
        ("test::k", ["test::k(2) + 2", "str::len(test::k(\"t\"))"]),
    ] {
        let r = gx.compile_ref(find_bind_id(&m.env, name)?).await?;
        let c = gx.compile_callable(r.last.clone().context("no value")?).await?;
        for u in uses {
            compiles(&gx, u).await?;
        }
        held.push((r, c));
    }
    let (_, gc) = &held[1];
    gc.call(ValArray::from_iter_exact([Value::I64(1)].into_iter())).await?;
    drop(held);
    drop(m);
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

/// A dropped callable takes its arguments' stored values with it, called
/// or not, and dropped before its call landed or after.
#[tokio::test(flavor = "multi_thread")]
async fn a_dropped_callable_frees_its_arguments() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let m = gx.compile(literal!("mod test")).await?;
    let r = gx.compile_ref(find_bind_id(&m.env, "test::h")?).await?;
    let lambda = r.last.clone().context("h has no value")?;
    for (call, wait) in [(false, true), (true, true), (true, false)] {
        gx.wait_idle().await?;
        let s0 = gx.env_stats().await?.store_len;
        for i in 0..20 {
            let c = gx.compile_callable(lambda.clone()).await?;
            if call {
                c.call(ValArray::from_iter_exact([Value::I64(i)].into_iter())).await?;
            }
            if wait {
                gx.wait_idle().await?;
            }
            drop(c);
        }
        gx.wait_idle().await?;
        let s1 = gx.env_stats().await?.store_len;
        if s1 != s0 {
            bail!(
                "20 dropped callables (called: {call}, waited: {wait}) left {} store entries",
                s1 - s0
            );
        }
    }
    drop(r);
    drop(m);
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

/// A lambda with no argument to deliver a call with is no callable.
#[tokio::test(flavor = "multi_thread")]
async fn a_callable_needs_an_argument() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let m = gx.compile(literal!("mod test")).await?;
    for name in ["test::z", "test::d"] {
        let r = gx.compile_ref(find_bind_id(&m.env, name)?).await?;
        let e = match gx.compile_callable(r.last.clone().context("no value")?).await {
            Ok(_) => bail!("{name} compiled as a callable"),
            Err(e) => e,
        };
        if !e.to_string().contains("needs an argument") {
            bail!("{name}: {e}");
        }
    }
    drop(m);
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

/// The runtime refuses a call whose argument does not fit the callable's
/// signature, however the call was sent.
#[tokio::test(flavor = "multi_thread")]
async fn a_mistyped_call_is_refused() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let m = gx.compile(literal!("mod test")).await?;
    let r = gx.compile_ref(find_bind_id(&m.env, "test::i")?).await?;
    let c = gx.compile_callable(r.last.clone().context("i has no value")?).await?;
    let outputs = |rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>| {
        let mut out = vec![];
        while let Ok(mut b) = rx.try_recv() {
            for e in b.drain(..) {
                if let GXEvent::Updated(id, v) = e
                    && id == c.expr
                {
                    out.push(graphix_package_core::testing::data(v))
                }
            }
        }
        out
    };
    gx.wait_idle().await?;
    outputs(&mut rx);
    let bad = ValArray::from_iter_exact([Value::String(literal!("no"))].into_iter());
    c.call_unchecked(bad.clone())?;
    gx.call(c.id(), bad)?;
    gx.wait_idle().await?;
    assert_eq!(outputs(&mut rx), Vec::<Value>::new());
    c.call(ValArray::from_iter_exact([Value::I64(3)].into_iter())).await?;
    gx.wait_idle().await?;
    assert_eq!(outputs(&mut rx), [Value::I64(3)]);
    drop((c, r, m));
    ctx.shutdown().await;
    Ok(())
}

/// An input that fails to compile changes nothing: no statement before
/// the failure runs or keeps a name, and a `catch` in it handles nothing
/// later.
#[tokio::test(flavor = "multi_thread")]
async fn a_failed_input_changes_nothing() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    gx.wait_idle().await?;
    let s0 = gx.env_stats().await?.store_len;
    if gx.compile(literal!("let b = 5; let d = nosuch")).await.is_ok() {
        bail!("an undefined name compiled");
    }
    if gx.compile(literal!("catch(e) e; let w = nosuch")).await.is_ok() {
        bail!("an undefined name compiled");
    }
    gx.wait_idle().await?;
    let s1 = gx.env_stats().await?.store_len;
    if s1 != s0 {
        bail!("failed inputs left {} store entries", s1 - s0);
    }
    gx.compile(literal!("error(`Boom)?")).await.context("a `?` after a failed catch")?;
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

/// A write still waiting when its expression is deleted goes with it.
#[tokio::test(flavor = "multi_thread")]
async fn a_deleted_writer_leaves_no_store_entry() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    gx.wait_idle().await?;
    let s0 = gx.env_stats().await?.store_len;
    for _ in 0..5 {
        let e = gx.compile(literal!("{ let x = 0; x <- x + 1; x }")).await?;
        tokio::time::sleep(Duration::from_millis(20)).await;
        drop(e);
    }
    gx.wait_idle().await?;
    let s1 = gx.env_stats().await?.store_len;
    if s1 != s0 {
        bail!("5 deleted writers left {} store entries", s1 - s0);
    }
    ctx.shutdown().await;
    drain.abort();
    Ok(())
}

/// Every write of a `set_many` lands in one cycle, even one whose
/// variable already has a write waiting: the set waits with it. The two
/// sends share an input batch (a current_thread runtime, no await
/// between them).
#[tokio::test(flavor = "current_thread")]
async fn set_many_lands_in_one_cycle() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = runtime(tx).await?;
    let gx = ctx.rt.clone();
    let m = gx.compile(literal!("mod test")).await?;
    let sx = find_bind_id(&m.env, "test::sx")?;
    let sy = find_bind_id(&m.env, "test::sy")?;
    let w = gx.compile(literal!("(test::sx, test::sy)")).await?;
    let wid = w.exprs.last().context("no expr")?.id;
    gx.wait_idle().await?;
    while rx.try_recv().is_ok() {}
    gx.set(sx, Value::I64(5))?;
    gx.set_many([(sx, Value::I64(1)), (sy, Value::I64(2))])?;
    gx.wait_idle().await?;
    let mut seen = vec![];
    while let Ok(mut b) = rx.try_recv() {
        for e in b.drain(..) {
            if let GXEvent::Updated(id, v) = e
                && id == wid
            {
                seen.push(format!("{}", graphix_package_core::testing::data(v)));
            }
        }
    }
    assert_eq!(seen, ["[i64:5, i64:0]", "[i64:1, i64:2]"]);
    drop(w);
    drop(m);
    ctx.shutdown().await;
    Ok(())
}
