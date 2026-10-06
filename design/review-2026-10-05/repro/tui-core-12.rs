//! tui-core-12: TUITYP (stdlib/graphix-package-tui/src/lib.rs:881) is a
//! process-wide `TypeRef`; its write-once resolution cell is filled, weakly,
//! by the first runtime's `contains` and never re-resolved
//! (graphix-types/src/typ/mod.rs:627-649).
//!
//! command: copy to stdlib/graphix-package-tui/tests/review_tui_core_12.rs,
//!   then RUST_LOG=error timeout -s KILL 2400 cargo test -p graphix-package-tui
//!   --test review_tui_core_12 -- --nocapture
//!
//! Two runtimes, one after the other, each compile the same TUI expression
//! and ask the package whether it is a custom display (the shell's
//! `Output::from_expr` path). No tty is attached, so the display task the
//! first answer starts fails at once; nothing draws.
//!
//! expected: both runtimes' `tui::text::text(&"x")` is a TUI (Custom).
//! observed (HEAD c722befe): "first runtime: custom=true; second runtime:
//!   custom=false", after two log lines "ERROR graphix_types::typ] type
//!   `tui::Tui` outlived its definition in ``"; the second assertion fails.
//!   (The ESC[?1049l printed before the panic message is the panic hook the
//!   first display's ratatui::try_init installed: tui-core-14.)

use anyhow::Result;
use graphix_package::{CustomResult, MainThreadHandle, Package};
use graphix_package_core::testing::{self, PackageRef};
use std::time::Duration;
use tokio::sync::mpsc;

const REG: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_tui::P,
];

async fn displayed_as_tui(round: usize) -> Result<bool> {
    let (tx, _rx) = mpsc::channel(100);
    let ctx = testing::init(tx, REG).await?;
    let mut res = ctx.rt.compile(arcstr::literal!("tui::text::text(&\"x\")")).await?;
    let env = res.env.clone();
    let e = res.exprs.pop().expect("one expression");
    eprintln!("round {round}: expression type {}", e.typ);
    let (run_on_main, _main_rx) = MainThreadHandle::new();
    let custom =
        match graphix_package_tui::P.maybe_init_custom(&ctx.rt, &env, e, &run_on_main).await? {
            CustomResult::Custom(mut cdc) => {
                cdc.custom.clear().await;
                true
            }
            CustomResult::NotCustom(_) => false,
        };
    drop(res);
    drop(env);
    ctx.shutdown().await;
    tokio::time::sleep(Duration::from_secs(1)).await;
    Ok(custom)
}

#[tokio::test(flavor = "multi_thread")]
async fn tuityp_outlives_the_runtime_that_resolved_it() -> Result<()> {
    let _ = env_logger::builder().is_test(true).try_init();
    let first = displayed_as_tui(1).await?;
    let second = displayed_as_tui(2).await?;
    eprintln!("first runtime: custom={first}; second runtime: custom={second}");
    assert!(first, "the first runtime's TUI was not recognized");
    assert!(second, "the second runtime's TUI was not recognized as a TUI");
    Ok(())
}
