//! rt-04: GXHandle::set_deref through a place jumps ahead of an earlier GXHandle::set
//! of the same root sent in the same input batch.
//!
//! command: copy to graphix-rt/tests/review_rt_04.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-rt --test review_rt_04 -- --nocapture
//!
//! program: `let s = {a: 1, b: 2}; let r = &mut s.a; s`
//! embedder, one batch (current-thread runtime): set(s, {a: 10, b: 20}); set_deref(r, 5)
//! expected: s takes {a: 10, b: 20} then {a: 5, b: 20} (the writes in the order sent)
//! observed (HEAD 7324207f, dev): forward: s took [{a: 5, b: 2}, {a: 10, b: 20}], FAILED;
//!   control (set_deref then set): the same two values, passes. The write through r is lost.

use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::{BindId, Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

fn st(a: i64, b: i64) -> Value {
    let f = |n: &str, v: i64| {
        Value::Array(ValArray::from_iter_exact(
            [Value::String(ArcStr::from(n)), Value::I64(v)].into_iter(),
        ))
    };
    Value::Array(ValArray::from_iter_exact([f("a", a), f("b", b)].into_iter()))
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
    let cell: u64 = r.last.clone().context("r has no value")?.cast_to::<u64>()?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    while rx.try_recv().is_ok() {}
    if set_first {
        gx.set(s.bid, st(10, 20))?;
        gx.set_deref(BindId::from(cell), Value::I64(5))?;
    } else {
        gx.set_deref(BindId::from(cell), Value::I64(5))?;
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
    eprintln!("rt-04 set; set_deref: s took {vs:?}");
    assert_eq!(vs.last(), Some(&st(5, 20)));
    Ok(())
}

#[tokio::test]
async fn set_deref_then_set_control() -> Result<()> {
    let vs = run(false).await?;
    eprintln!("rt-04 control set_deref; set: s took {vs:?}");
    assert_eq!(vs.last(), Some(&st(10, 20)));
    Ok(())
}
