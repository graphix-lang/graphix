//! Title: par_attrs' `ran.forks > 0` under Force counts the stdlib root's forks.
//! Command: copy to stdlib/graphix-tests/tests/ and run
//!   flock ~/tmp/cargo.lock cargo test -p graphix-tests --test <name> -- --nocapture
//! Expected: a fixture that forks nothing ("42") leaves the count at 0.
//! Observed:
//!   REVIEW "42": forks before compile 73, after 73; build_forks 0 -> 0
//!   REVIEW "{ let a = 1; let b = 2; b }": forks before compile 73, after 74
//!   REVIEW auto 42: forks before compile 0, after 0
//! So under Force a runtime has forked 73 times before the fixture compiles,
//! and `assert!(ran.forks > 0)` in block_runs_fork and first_cycle_growth_agrees
//! holds for a fixture that forks nothing.

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::{ParMode, expr::VfsEntry};
use graphix_package_core::testing::{
    Mode, PackageRef, compile_result, fixture_runtime, result_source, updates_until_quiet,
};
use std::time::Duration;
use tokio::time::Instant;

async fn forks(code: &str, par: ParMode) -> Result<(u64, u64, u64, u64)> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let files: [(&str, VfsEntry); 1] = [("/test.gx", result_source(code))];
    let (ctx, mut rx) =
        fixture_runtime(files, packages, Mode::Interp, |ctx| ctx.control.set_par_mode(par))
            .await?;
    let before = ctx.rt.control().forks();
    let build_before = ctx.rt.control().build_forks();
    let res = compile_result(&ctx).await?;
    let _ = updates_until_quiet(
        &ctx.rt,
        &mut rx,
        res.exprs[0].id,
        Duration::from_millis(300),
        Instant::now() + Duration::from_secs(20),
    )
    .await?;
    let after = ctx.rt.control().forks();
    let build_after = ctx.rt.control().build_forks();
    ctx.shutdown().await;
    Ok((before, after, build_before, build_after))
}

#[tokio::test(flavor = "current_thread")]
async fn stdlib_root_forks_under_force() -> Result<()> {
    let _ = ArcStr::from("");
    for code in ["42", "{ let a = 1; let b = 2; b }"] {
        let (b, a, bb, ba) = forks(code, ParMode::Force).await?;
        eprintln!("REVIEW {code:?}: forks before compile {b}, after {a}; build_forks {bb} -> {ba}");
    }
    while graphix_compiler::cost::calibration().is_none() {
        tokio::time::sleep(Duration::from_millis(5)).await;
    }
    for _ in 0..3 {
        let (b, a, bb, ba) = forks("42", ParMode::Auto).await?;
        eprintln!("REVIEW auto 42: forks before compile {b}, after {a}; build_forks {bb} -> {ba}");
    }
    Ok(())
}
