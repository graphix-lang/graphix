//! An embedder's writes land in the order it sent them: a set of a root
//! and a write through a place reference into it, in one input batch.

use anyhow::{Context, Result};
use arcstr::literal;
use graphix_compiler::{BindId, Scope, expr::ModPath, rep};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

fn st(a: i64, b: i64) -> Value {
    rep::structure([(literal!("a"), Value::I64(a)), (literal!("b"), Value::I64(b))])
}

async fn run(set_first: bool) -> Result<Vec<Value>> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, mut rx) = mpsc::channel::<Batch>(100);
    let gx: GXHandle<NoExt> = GXConfig::builder(ctx, tx).build()?.start().await?;
    let m = gx.compile(literal!("let s = {a: 1, b: 2}; let r = &mut s.a; s")).await?;
    let sid = m.exprs[2].id;
    tokio::time::sleep(Duration::from_millis(300)).await;
    let env = gx.get_env().await?;
    let s = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["s"])).await?;
    let r = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["r"])).await?;
    let cell = r.last.clone().context("r has no value")?.cast_to::<BindId>()?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    while rx.try_recv().is_ok() {}
    if set_first {
        gx.set(s.bid, st(10, 20))?;
        gx.set_deref(cell, Value::I64(5))?;
    } else {
        gx.set_deref(cell, Value::I64(5))?;
        gx.set(s.bid, st(10, 20))?;
    }
    tokio::time::sleep(Duration::from_millis(500)).await;
    let mut vs = vec![];
    while let Ok(mut b) = rx.try_recv() {
        for e in b.drain(..) {
            if let GXEvent::Updated(i, v) = e {
                if i == sid {
                    vs.push(v)
                }
            }
        }
    }
    Ok(vs)
}

#[tokio::test]
async fn set_then_set_deref_lands_in_order() -> Result<()> {
    let vs = run(true).await?;
    assert_eq!(vs.last(), Some(&st(5, 20)), "{vs:?}");
    Ok(())
}

#[tokio::test]
async fn set_deref_then_set_lands_in_order() -> Result<()> {
    let vs = run(false).await?;
    assert_eq!(vs.last(), Some(&st(10, 20)), "{vs:?}");
    Ok(())
}
