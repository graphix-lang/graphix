// State the runtime makes for a call, an instance or a core-trait hook
// goes when the thing it was made for does: a long session (REPL,
// dynamic modules, a regrowing map) holds what is live, not its history.

use crate::{TEST_REGISTER, init};
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{Rt, expr::ExprId};
use graphix_package_core::testing::{TestCtx, init_with_registration};
use graphix_rt::{GXEvent, RegistrationImage};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::{mpsc, oneshot};

type Events = mpsc::Receiver<GPooled<Vec<GXEvent>>>;
type Compiled = graphix_rt::CompRes<graphix_rt::NoExt>;

/// Wait for `eid`'s next update satisfying `done`.
async fn await_update(
    rx: &mut Events,
    eid: ExprId,
    done: impl Fn(&Value) -> bool,
) -> Result<()> {
    let timeout = tokio::time::sleep(Duration::from_secs(5));
    tokio::pin!(timeout);
    let mut last = None;
    loop {
        tokio::select! {
            _ = &mut timeout => bail!("timeout waiting on {eid:?}, last saw {last:?}"),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => for e in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = e && id == eid {
                        if done(&v) {
                            return Ok(());
                        }
                        last = Some(v);
                    }
                }
            }
        }
    }
}

/// Compile `code`; the expression lives while the result is held.
async fn compile_one(ctx: &TestCtx, code: impl Into<ArcStr>) -> Result<(ExprId, Compiled)> {
    let res = ctx.rt.compile(code.into()).await?;
    Ok((res.exprs[0].id, res))
}

async fn store_len(ctx: &TestCtx) -> Result<usize> {
    ctx.rt.with_ctx(|ctx| ctx.rt.store().len()).await
}

/// A site whose callee alternates between lambdas with labeled defaults
/// keeps one default's store entry, not one per rebind.
#[tokio::test(flavor = "current_thread")]
async fn rebind_defaults_leave_no_store_entries() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          let f = |#a: i64 = 1, x: i64| x + a;
          let g = |#b: i64 = 2, x: i64| x * b;
          let h: fn(x: i64) -> i64 = f;
          let k = 0;
          k <- sys::time::timer(duration:0.005s, true) ~ k + 1;
          h <- select k % 2 { 0 => f, _ => g };
          (k, h(k))
        }",
    )
    .await?;
    let at = |n: i64| move |v: &Value| matches!(v, Value::Array(a) if a.first() == Some(&Value::I64(n)));
    await_update(&mut rx, eid, at(10)).await?;
    let early = store_len(&ctx).await?;
    await_update(&mut rx, eid, at(60)).await?;
    let late = store_len(&ctx).await?;
    ctx.shutdown().await;
    assert!(
        late <= early,
        "the store grew from {early} to {late} entries over 50 rebinds"
    );
    Ok(())
}

/// The fn-typed formals a static resolution records for an instance go
/// with the instance: a map whose slots regrow holds the live slots'.
#[tokio::test(flavor = "current_thread")]
async fn fn_forward_resolutions_go_with_their_instance() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let _arr = ctx.rt.compile(literal!("let arr: Array<i64> = [];")).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          let h = |f: fn(x: i64) -> i64, x: i64| f(throttle(x));
          array::map(arr, |x| h(|y| y + 1, x))
        }",
    )
    .await?;
    let env = ctx.rt.get_env().await?;
    let arr = env
        .lookup_bind(&graphix_compiler::Scope::root().lexical, &["arr"].into())?
        .ok_or_else(|| anyhow::anyhow!("arr not in scope"))?
        .1
        .id;
    let len = |n: usize| move |v: &Value| matches!(v, Value::Array(a) if a.len() == n);
    let mut bottoms = vec![];
    for _ in 0..4 {
        let four: Vec<Value> = (1..=4).map(Value::I64).collect();
        ctx.rt.set(arr, Value::Array(four.into()))?;
        await_update(&mut rx, eid, len(4)).await?;
        ctx.rt.set(arr, Value::Array(Vec::<Value>::new().into()))?;
        await_update(&mut rx, eid, len(0)).await?;
        bottoms.push(ctx.rt.with_ctx(|ctx| ctx.fn_forward_resolutions.len()).await?);
    }
    ctx.shutdown().await;
    assert!(bottoms.iter().all(|b| *b == bottoms[0]), "{bottoms:?}");
    Ok(())
}

/// A core-trait hook site rebuilt after the implementation list changed
/// takes its argument bindings with it.
#[tokio::test(flavor = "current_thread")]
async fn core_hook_sites_leave_no_bindings() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          type T = Abstract<i64>;
          impl Eq for T { let eq = |a, b| true };
          let n = 0;
          n <- sys::time::timer(duration:0.01s, true) ~ n + 1;
          (n, n ~ (T(1) == T(2)))
        }",
    )
    .await?;
    let mut decls = vec![];
    for k in 0..5 {
        let (id, decl) = compile_one(
            &ctx,
            format!(
                "{{ type U{k} = Abstract<i64>; impl Eq for U{k} {{ let eq = |a, b| true }}; null }}"
            ),
        )
        .await?;
        await_update(&mut rx, id, |_| true).await?;
        decls.push(decl);
        // two ticks: a comparison ran against the grown list
        await_update(&mut rx, eid, |_| true).await?;
        await_update(&mut rx, eid, |_| true).await?;
    }
    let env = ctx.rt.get_env().await?;
    let seams =
        env.by_id.into_iter().filter(|(_, b)| b.name.starts_with("#seam")).count();
    ctx.shutdown().await;
    assert!(seams <= 2, "{seams} hook argument bindings after 5 rebuilds");
    Ok(())
}

/// A builtin definition restored from an image rebuilds its check under
/// the definition gate, which takes its catch binding with it.
#[tokio::test(flavor = "current_thread")]
async fn restored_builtin_check_leaves_no_binding() -> Result<()> {
    let (tx, _rx) = mpsc::channel(64);
    let (image_tx, image_rx) = oneshot::channel();
    let cold =
        init_with_registration(tx, TEST_REGISTER, RegistrationImage::Save(image_tx))
            .await?;
    let image = image_rx.await??;
    cold.shutdown().await;
    let (tx, mut rx) = mpsc::channel(64);
    let warm =
        init_with_registration(tx, TEST_REGISTER, RegistrationImage::Load(image)).await?;
    let (eid, _expr) = compile_one(&warm, "str::len(\"abc\") + str::len(\"de\")").await?;
    await_update(&mut rx, eid, |v| matches!(v, Value::I64(5))).await?;
    let env = warm.rt.get_env().await?;
    let faux = env.by_id.into_iter().filter(|(_, b)| b.name == "faux").count();
    warm.shutdown().await;
    assert_eq!(faux, 0);
    Ok(())
}
