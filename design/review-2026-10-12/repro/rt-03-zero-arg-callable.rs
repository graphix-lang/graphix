//! rt-03 (review 2026-10-12): a callable with no argument to deliver
//! (`|| e`, or one whose every label has a default) runs once when
//! compiled and never when called; call_answered answers null.
//!
//! command: copy to graphix-rt/tests/review_rt_03.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-rt --test review_rt_03 -- --nocapture
//!
//! expected: each callable produces nothing at compile and one output per
//!   call; an answered call answers the call's value (42 / 6 / 8).
//! observed (HEAD 7324207f, dev profile):
//!   `let f = || 42`: outputs at compile 1, after two calls 0, answer [Null]
//!   `let f = |#k: i64 = 5| k + 1`: outputs at compile 1, after two calls 0, answer [Null]
//!   `let f = |x: i64| x + 1`: outputs at compile 0, after two calls 2, answer [I64(8)]  (control)

use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{Scope, expr::{ExprId, ModPath}};
use graphix_rt::{GXConfig, GXEvent, GXHandle, GXRt, NoExt};
use netidx_value::{ValArray, Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

struct Rig {
    gx: GXHandle<NoExt>,
    events: mpsc::UnboundedReceiver<GXEvent>,
}

impl Rig {
    async fn new() -> Result<Self> {
        let ctx = GXRt::<NoExt>::new_state()?;
        let (tx, mut rx) = mpsc::channel::<Batch>(100);
        let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
        let (etx, events) = mpsc::unbounded_channel();
        tokio::spawn(async move {
            while let Some(mut b) = rx.recv().await {
                for e in b.drain(..) {
                    let _ = etx.send(e);
                }
            }
        });
        Ok(Self { gx, events })
    }

    fn take(&mut self, id: ExprId) -> Vec<Value> {
        let mut v = vec![];
        while let Ok(e) = self.events.try_recv() {
            if let GXEvent::Updated(i, x) = e
                && i == id
            {
                v.push(x)
            }
        }
        v
    }
}

async fn settle() {
    tokio::time::sleep(Duration::from_millis(300)).await
}

async fn probe(def: &'static str, args: &[Value]) -> Result<(usize, usize, Vec<Value>)> {
    let mut rig = Rig::new().await?;
    let _m = rig.gx.compile(ArcStr::from(def)).await?;
    settle().await;
    let env = rig.gx.get_env().await?;
    let r = rig
        .gx
        .compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["f"]))
        .await?;
    let lambda = r.last.clone().context("f has no value")?;
    let c = rig.gx.compile_callable(lambda).await?;
    settle().await;
    let at_compile = rig.take(c.expr).len();
    c.call(ValArray::from_iter(args.iter().cloned())).await?;
    settle().await;
    c.call(ValArray::from_iter(args.iter().cloned())).await?;
    settle().await;
    let after_two_calls = rig.take(c.expr).len();
    c.call_answered(ValArray::from_iter(args.iter().cloned())).await?;
    settle().await;
    let answers = rig.take(c.answer);
    eprintln!(
        "rt-03 `{def}`: outputs at compile {at_compile}, after two calls {after_two_calls}, answer {answers:?}"
    );
    Ok((at_compile, after_two_calls, answers))
}

#[tokio::test]
async fn one_arg_callable_control() -> Result<()> {
    let r = probe("let f = |x: i64| x + 1", &[Value::I64(7)]).await?;
    assert_eq!(r, (0, 2, vec![Value::I64(8)]));
    Ok(())
}

#[tokio::test]
async fn zero_arg_callable_runs_per_call() -> Result<()> {
    let r = probe("let f = || 42", &[]).await?;
    assert_eq!(r, (0, 2, vec![Value::I64(42)]));
    Ok(())
}

#[tokio::test]
async fn all_defaulted_callable_runs_per_call() -> Result<()> {
    let r = probe("let f = |#k: i64 = 5| k + 1", &[]).await?;
    assert_eq!(r, (0, 2, vec![Value::I64(6)]));
    Ok(())
}
