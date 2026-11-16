//! Review probes (rt), graphix-rt's handle contracts.
//!
//! Run: copy to graphix-rt/tests/review_rt_1.rs, then
//!   cargo test -p graphix-rt --test review_rt_1 -- --nocapture
//!
//! Observed at e1be2fbf2 (debug build):
//! - review_an_armed_timer_holds_wait_replies: FAILS. Expected wait_replies
//!   to answer (its doc: "Timers and watches may still start cycles");
//!   observed "wait_replies with one timer armed answered: false" after
//!   10 s: set_timer spawns into `tasks`, which replies_pending counts.
//! - review_an_answer_comes_before_the_second_write: FAILS. Expected
//!   ["write a", "write a!", "answer a"] (call_answered's doc: the writes
//!   its cycle queued come first); observed
//!   ["write \"a\"", "answer \"a\"", "write \"a!\""].
//! - review_a_refused_ref_set_leaves_last: FAILS. Expected Ref::last to be
//!   the runtime's value after a refused write; observed
//!   ref.last=Some(String("hello")) runtime=Some(I64(0)).
//! - review_a_generic_callable: passes, printing that a callable over
//!   `|x| x * x` has type fn(x: '_0) -> '_0 and its argument check admits
//!   a string (check None, out []) beside i64 (9) and f64 (2.25).
//! - review_event_size: prints size_of GXEvent=968 Env=968 Value=16.
//! - review_a_query_runs_a_cycle: prints "10 get_env: 12 cycles, 12 empty
//!   batches".

use anyhow::{Context, Result};
use arcstr::literal;
use graphix_compiler::{BindId, Rt, Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::{sync::mpsc, time::timeout};

type Batch = GPooled<Vec<GXEvent>>;

async fn start() -> Result<(GXHandle<NoExt>, mpsc::Receiver<Batch>)> {
    let ctx = GXRt::<NoExt>::new_state()?;
    let (tx, rx) = mpsc::channel::<Batch>(100);
    let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
    Ok((gx, rx))
}

fn drain(rx: &mut mpsc::Receiver<Batch>) -> Vec<GXEvent> {
    let mut out = vec![];
    while let Ok(mut b) = rx.try_recv() {
        out.extend(b.drain(..))
    }
    out
}

/// `wait_replies` says timers may still start cycles, so a timer armed
/// for an hour must not hold it.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn review_an_armed_timer_holds_wait_replies() -> Result<()> {
    let (gx, mut rx) = start().await?;
    tokio::spawn(async move { while rx.recv().await.is_some() {} });
    gx.wait_replies().await?;
    gx.with_ctx(|ctx| ctx.rt.set_timer(BindId::new(), Duration::from_secs(3600)))
        .await?;
    let r = timeout(Duration::from_secs(10), gx.wait_replies()).await;
    println!("REVIEW wait_replies with one timer armed answered: {}", r.is_ok());
    assert!(r.is_ok(), "an armed timer held wait_replies for 10 s");
    Ok(())
}

/// A handler that writes one variable twice: the doc of `call_answered`
/// says the answer comes after the updates of the writes its cycle queued.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn review_an_answer_comes_before_the_second_write() -> Result<()> {
    let (gx, mut rx) = start().await?;
    let m = gx
        .compile(literal!(
            "let v = \"\"; let h = |s: string| { v <- s; v <- \"[s]!\"; s }; v"
        ))
        .await?;
    let vid = m.exprs.last().context("no result")?.id;
    gx.wait_idle().await?;
    let env = gx.get_env().await?;
    let h = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["h"])).await?;
    let h = gx.compile_callable(h.last.clone().context("h has no value")?).await?;
    gx.wait_idle().await?;
    drain(&mut rx);
    h.call_answered(ValArray::from_iter([Value::from("a")]))?;
    // the second write lands two cycles after the call
    for _ in 0..3 {
        gx.wait_idle().await?;
    }
    let mut order = vec![];
    for e in drain(&mut rx) {
        match e {
            GXEvent::Updated(i, v) if i == vid => order.push(format!("write {v}")),
            GXEvent::Updated(i, v) if i == h.answer => order.push(format!("answer {v}")),
            _ => (),
        }
    }
    println!("REVIEW order: {order:?}");
    assert_eq!(order, ["write \"a\"", "write \"a!\"", "answer \"a\""]);
    Ok(())
}

/// `Ref::set` records the value as `last` before the runtime refuses it.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn review_a_refused_ref_set_leaves_last() -> Result<()> {
    let (gx, mut rx) = start().await?;
    let m = gx.compile(literal!("let x: i64 = 0; x")).await?;
    let out = m.exprs.last().context("no result")?.id;
    gx.wait_idle().await?;
    let env = gx.get_env().await?;
    let mut r =
        gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["x"])).await?;
    drain(&mut rx);
    r.set(Value::from("hello"))?;
    gx.wait_idle().await?;
    gx.wait_idle().await?;
    let seen: Vec<_> = drain(&mut rx)
        .into_iter()
        .filter_map(|e| match e {
            GXEvent::Updated(i, v) if i == out => Some(v),
            _ => None,
        })
        .collect();
    let fresh = gx.compile_ref(r.bid).await?;
    println!("REVIEW ref.last={:?} runtime={:?} seen={seen:?}", r.last, fresh.last);
    assert_eq!(r.last, fresh.last, "Ref::last disagrees with the runtime");
    Ok(())
}

/// A callable compiled over a generic lambda: what its argument check
/// admits and what a call of each kind produces.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn review_a_generic_callable() -> Result<()> {
    let (gx, mut rx) = start().await?;
    let _m = gx.compile(literal!("let h = |x| x * x")).await?;
    gx.wait_idle().await?;
    let env = gx.get_env().await?;
    let h = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["h"])).await?;
    let c = match gx.compile_callable(h.last.clone().context("h has no value")?).await {
        Ok(c) => c,
        Err(e) => {
            println!("REVIEW generic callable refused: {e:#}");
            return Ok(());
        }
    };
    println!("REVIEW generic callable typ: {}", c.typ);
    gx.wait_idle().await?;
    drain(&mut rx);
    for v in [Value::from("s"), Value::I64(3), Value::F64(1.5)] {
        let r = c.call(ValArray::from_iter([v.clone()])).await;
        gx.wait_idle().await?;
        let out: Vec<_> = drain(&mut rx)
            .into_iter()
            .filter_map(|e| match e {
                GXEvent::Updated(i, v) if i == c.expr => Some(v),
                _ => None,
            })
            .collect();
        println!("REVIEW call({v}) -> check {:?}, out {out:?}", r.err().map(|e| e.to_string()));
    }
    Ok(())
}

#[test]
fn review_event_size() {
    println!(
        "REVIEW size_of GXEvent={} Env={} Value={} ExprId={}",
        std::mem::size_of::<GXEvent>(),
        std::mem::size_of::<graphix_compiler::env::Env>(),
        std::mem::size_of::<Value>(),
        std::mem::size_of::<graphix_compiler::expr::ExprId>()
    );
}

/// What a query that asks nothing of the program costs: cycles started
/// and batches sent for ten `get_env` calls on an idle runtime.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn review_a_query_runs_a_cycle() -> Result<()> {
    let (gx, mut rx) = start().await?;
    let _m = gx.compile(literal!("let x = 1; x")).await?;
    gx.wait_idle().await?;
    drain(&mut rx);
    let before = gx.control().cycles();
    let mut batches = 0;
    for _ in 0..10 {
        gx.get_env().await?;
    }
    gx.wait_idle().await?;
    while let Ok(b) = rx.try_recv() {
        assert!(b.is_empty(), "a query's batch carried {} events", b.len());
        batches += 1;
    }
    println!(
        "REVIEW 10 get_env: {} cycles, {batches} empty batches",
        gx.control().cycles() - before
    );
    Ok(())
}
