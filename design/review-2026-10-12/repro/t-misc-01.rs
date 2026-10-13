// t-misc-01: a fused recursion's per-activation kernel blocks are
// charged to the runtime's Control during a cycle
// (`stack::record_self_blocks(1)` -> `activation_born`), but released
// through `stack::current_control()`, which is null when the expression
// is deleted (`ToGX::Delete` runs outside `with_control`). The runtime's
// `live_self_blocks` and `live_activations` never come back down, so a
// REPL/embedder that re-evaluates a deep recursion under an activation
// budget eventually aborts the runtime spuriously.

use anyhow::{Result, bail};
use graphix_package_core::testing::{
    Mode, PackageRef, compile_result, fixture_runtime, next_update, result_source,
};
use std::time::Duration;
use tokio::time::Instant;

#[tokio::test(flavor = "current_thread")]
async fn deleted_kernel_releases_its_activations() -> Result<()> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let code = "{ let x = array::iter([i64:50]); \
                  let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; \
                  f(x) }";
    let (ctx, mut rx) =
        fixture_runtime([("/test.gx", result_source(code))], packages, Mode::Jit, |_| {})
            .await?;
    let before_blocks = ctx.rt.control().live_self_blocks();
    let before_acts = ctx.rt.control().live_activations();
    let res = compile_result(&ctx).await?;
    let id = res.exprs[0].id;
    let deadline = Instant::now() + Duration::from_secs(10);
    let v = next_update(&mut rx, id, deadline).await?;
    let live_blocks = ctx.rt.control().live_self_blocks();
    let live_acts = ctx.rt.control().live_activations();
    drop(res);
    // a round trip through the runtime's queue: the Delete is handled first
    let probe = ctx.rt.compile(arcstr::literal!("1")).await?;
    drop(probe);
    let _ = ctx.rt.compile(arcstr::literal!("2")).await?;
    let after_blocks = ctx.rt.control().live_self_blocks();
    let after_acts = ctx.rt.control().live_activations();
    eprintln!(
        "value={v} blocks: before={before_blocks} live={live_blocks} after_delete={after_blocks}; \
         activations: before={before_acts} live={live_acts} after_delete={after_acts}"
    );
    ctx.shutdown().await;
    if live_blocks < 30 {
        bail!("the recursion did not fuse with per-activation blocks ({live_blocks})");
    }
    if after_blocks != before_blocks || after_acts != before_acts {
        bail!(
            "deleting the expression leaked: self blocks {after_blocks} (was {before_blocks}), \
             activations {after_acts} (was {before_acts})"
        );
    }
    Ok(())
}

/// The node-walk's activations (`GXLambda::charged`) are released by
/// `ctx.control` at delete: the leak is the kernel blocks' alone.
#[tokio::test(flavor = "current_thread")]
async fn interp_delete_releases_its_activations() -> Result<()> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let code = "{ let x = array::iter([i64:50]); \
                  let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; \
                  f(x) }";
    let (ctx, mut rx) =
        fixture_runtime([("/test.gx", result_source(code))], packages, Mode::Interp, |_| {})
            .await?;
    let res = compile_result(&ctx).await?;
    let id = res.exprs[0].id;
    let deadline = Instant::now() + Duration::from_secs(10);
    next_update(&mut rx, id, deadline).await?;
    let live = ctx.rt.control().live_activations();
    drop(res);
    let _ = ctx.rt.compile(arcstr::literal!("2")).await?;
    let after = ctx.rt.control().live_activations();
    eprintln!("interp activations: live={live} after_delete={after}");
    ctx.shutdown().await;
    Ok(())
}

/// Under an activation budget of 120, evaluating a 50-deep fused
/// recursion three times in turn (each deleted before the next) aborts
/// the runtime: the deleted ones' blocks are still counted.
#[tokio::test(flavor = "current_thread")]
async fn leaked_blocks_abort_a_budgeted_runtime() -> Result<()> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let code = "{ let x = array::iter([i64:50]); \
                  let rec f = |k: i64| -> i64 select k { i64:0 => i64:0, _ => k + f(k - i64:1) }; \
                  f(x) }";
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        packages,
        Mode::Jit,
        |ctx| ctx.control.set_activation_budget(120),
    )
    .await?;
    for round in 0..4 {
        let res = compile_result(&ctx).await?;
        let id = res.exprs[0].id;
        let deadline = Instant::now() + Duration::from_secs(5);
        let r = next_update(&mut rx, id, deadline).await;
        eprintln!(
            "round {round}: {r:?} live_activations={} aborted={}",
            ctx.rt.control().live_activations(),
            ctx.rt.control().aborted()
        );
        if r.is_err() || ctx.rt.control().aborted() {
            ctx.shutdown().await;
            bail!("round {round}: the runtime aborted on leaked activations");
        }
        drop(res);
    }
    ctx.shutdown().await;
    Ok(())
}
