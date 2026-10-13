//! review rt (2026-10-12): runtime input handling probes, one test per finding.
//!
//! command: copy to graphix-rt/tests/review_rt_01.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-rt --test review_rt_01 -- --nocapture
//!
//! observed (HEAD 7324207f, dev profile), every assertion is the intended behaviour:
//!   set_in_send_loop: x values before an unrelated message [], after [I64(5)]
//!   call_in_send_loop: h(5) outputs before an unrelated message [], after [I64(6)]
//!   zero_arg_callable: outputs at compile 1, after two calls 0
//!   set_then_set_deref: s took [{a: 5, b: 2}, {a: 10, b: 20}]  (want last {a: 5, b: 20})
//!   called_then_dropped: store_len 1 -> 6
//!   deleted { let x = 0; x <- x + 1; x }: store_len 0 -> 5
//!   deleted { let x = 0; x + 1 }: store_len 0 -> 0   (control)

use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::{BindId, Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

struct Rig {
    gx: GXHandle<NoExt>,
    open: Arc<AtomicBool>,
    events: mpsc::UnboundedReceiver<GXEvent>,
}

impl Rig {
    async fn new(cap: usize) -> Result<Self> {
        let ctx = GXRt::<NoExt>::new_state()?;
        let (tx, mut rx) = mpsc::channel::<Batch>(cap);
        let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
        let open = Arc::new(AtomicBool::new(true));
        let (etx, events) = mpsc::unbounded_channel();
        let o = open.clone();
        tokio::spawn(async move {
            loop {
                if !o.load(Ordering::SeqCst) {
                    tokio::time::sleep(Duration::from_millis(5)).await;
                    continue;
                }
                match tokio::time::timeout(Duration::from_millis(5), rx.recv()).await {
                    Err(_) => (),
                    Ok(None) => break,
                    Ok(Some(mut b)) => {
                        for e in b.drain(..) {
                            let _ = etx.send(e);
                        }
                    }
                }
            }
        });
        Ok(Self { gx, open, events })
    }

    fn take(&mut self) -> Vec<GXEvent> {
        let mut v = vec![];
        while let Ok(e) = self.events.try_recv() {
            v.push(e)
        }
        v
    }

    async fn bind(&self, name: &str) -> Result<graphix_rt::Ref<NoExt>> {
        let env = self.gx.get_env().await?;
        self.gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from([name])).await
    }
}

fn values_of(evs: &[GXEvent], id: graphix_compiler::expr::ExprId) -> Vec<Value> {
    evs.iter()
        .filter_map(|e| match e {
            GXEvent::Updated(i, v) if *i == id => Some(v.clone()),
            _ => None,
        })
        .collect()
}

async fn settle() {
    tokio::time::sleep(Duration::from_millis(300)).await
}

#[tokio::test]
async fn set_in_send_loop_is_delivered() -> Result<()> {
    let mut rig = Rig::new(1).await?;
    let _m = rig.gx.compile(literal!("let x = 0")).await?;
    let m2 = rig.gx.compile(literal!("x")).await?;
    let xid = m2.exprs[0].id;
    let r = rig.bind("x").await?;
    settle().await;
    rig.take();
    rig.open.store(false, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(50)).await;
    rig.gx.get_env().await?;
    rig.gx.get_env().await?;
    tokio::time::sleep(Duration::from_millis(50)).await;
    rig.gx.set(r.bid, Value::I64(5))?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    rig.open.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(1000)).await;
    let before = values_of(&rig.take(), xid);
    rig.gx.get_env().await?;
    settle().await;
    let after = values_of(&rig.take(), xid);
    eprintln!("set_in_send_loop: x values before an unrelated message {before:?}, after {after:?}");
    assert_eq!(before, vec![Value::I64(5)], "the set waited for an unrelated message");
    Ok(())
}

#[tokio::test]
async fn zero_arg_callable_runs_per_call() -> Result<()> {
    let mut rig = Rig::new(100).await?;
    let _m = rig.gx.compile(literal!("let f = || 42")).await?;
    settle().await;
    let r = rig.bind("f").await?;
    let lambda = r.last.clone().context("f has no value")?;
    let c = rig.gx.compile_callable(lambda).await?;
    settle().await;
    let at_compile = values_of(&rig.take(), c.expr).len();
    c.call(ValArray::from_iter_exact(std::iter::empty())).await?;
    settle().await;
    c.call(ValArray::from_iter_exact(std::iter::empty())).await?;
    settle().await;
    let after_two_calls = values_of(&rig.take(), c.expr).len();
    eprintln!(
        "zero_arg_callable: outputs at compile {at_compile}, after two calls {after_two_calls}"
    );
    assert_eq!((at_compile, after_two_calls), (0, 2));
    Ok(())
}

fn st(a: i64, b: i64) -> Value {
    let f = |n: &str, v: i64| {
        Value::Array(ValArray::from_iter_exact(
            [Value::String(ArcStr::from(n)), Value::I64(v)].into_iter(),
        ))
    };
    Value::Array(ValArray::from_iter_exact([f("a", a), f("b", b)].into_iter()))
}

#[tokio::test]
async fn set_then_set_deref_lands_in_order() -> Result<()> {
    let mut rig = Rig::new(100).await?;
    let m = rig.gx.compile(literal!("let s = {a: 1, b: 2}; let r = &mut s.a; s")).await?;
    let sid = m.exprs[2].id;
    settle().await;
    let s = rig.bind("s").await?;
    let r = rig.bind("r").await?;
    let cell: u64 = r.last.clone().context("r has no value")?.cast_to::<u64>()?;
    settle().await;
    rig.take();
    rig.gx.set(s.bid, st(10, 20))?;
    rig.gx.set_deref(BindId::from(cell), Value::I64(5))?;
    settle().await;
    let vs = values_of(&rig.take(), sid);
    eprintln!("set_then_set_deref: s took {vs:?}");
    assert_eq!(vs.last(), Some(&st(5, 20)));
    Ok(())
}

#[tokio::test]
async fn called_then_dropped_callable_leaves_no_store() -> Result<()> {
    let mut rig = Rig::new(100).await?;
    let _m = rig.gx.compile(literal!("let h = |x: i64| -> i64 x + 1")).await?;
    settle().await;
    let r = rig.bind("h").await?;
    let lambda = r.last.clone().context("h has no value")?;
    settle().await;
    let s0 = rig.gx.env_stats().await?.store_len;
    for i in 0..5 {
        let c = rig.gx.compile_callable(lambda.clone()).await?;
        c.call(ValArray::from_iter_exact([Value::I64(i)].into_iter())).await?;
        drop(c);
    }
    settle().await;
    let s1 = rig.gx.env_stats().await?.store_len;
    rig.take();
    eprintln!("called_then_dropped: store_len {s0} -> {s1}");
    assert_eq!(s1, s0);
    Ok(())
}

#[tokio::test]
async fn deleted_writer_leaves_no_store() -> Result<()> {
    deleted_leaves_no_store("{ let x = 0; x <- x + 1; x }").await
}

#[tokio::test]
async fn deleted_reader_leaves_no_store() -> Result<()> {
    deleted_leaves_no_store("{ let x = 0; x + 1 }").await
}

async fn deleted_leaves_no_store(text: &'static str) -> Result<()> {
    let mut rig = Rig::new(100).await?;
    rig.gx.get_env().await?;
    settle().await;
    let s0 = rig.gx.env_stats().await?.store_len;
    for _ in 0..5 {
        let m = rig.gx.compile(ArcStr::from(text)).await?;
        tokio::time::sleep(Duration::from_millis(20)).await;
        drop(m);
        settle().await;
    }
    let s1 = rig.gx.env_stats().await?.store_len;
    rig.take();
    eprintln!("deleted {text}: store_len {s0} -> {s1}");
    assert_eq!(s1, s0);
    Ok(())
}

#[tokio::test]
async fn call_in_send_loop_is_delivered() -> Result<()> {
    let mut rig = Rig::new(1).await?;
    let _m = rig.gx.compile(literal!("let h = |x: i64| -> i64 x + 1")).await?;
    settle().await;
    let r = rig.bind("h").await?;
    let lambda = r.last.clone().context("h has no value")?;
    let c = rig.gx.compile_callable(lambda).await?;
    settle().await;
    rig.take();
    rig.open.store(false, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(50)).await;
    rig.gx.get_env().await?;
    rig.gx.get_env().await?;
    tokio::time::sleep(Duration::from_millis(50)).await;
    c.call(ValArray::from_iter_exact([Value::I64(5)].into_iter())).await?;
    tokio::time::sleep(Duration::from_millis(300)).await;
    rig.open.store(true, Ordering::SeqCst);
    tokio::time::sleep(Duration::from_millis(1000)).await;
    let before = values_of(&rig.take(), c.expr);
    rig.gx.get_env().await?;
    settle().await;
    let after = values_of(&rig.take(), c.expr);
    eprintln!("call_in_send_loop: h(5) outputs before an unrelated message {before:?}, after {after:?}");
    assert_eq!(before, vec![Value::I64(6)], "the call waited for an unrelated message");
    Ok(())
}
