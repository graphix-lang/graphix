//! A `mod m;` statement is never dead: its binds publish into the
//! persistent env, so dead-statement elimination treats a Module as an
//! effect.

use anyhow::{Result, bail};
use graphix_compiler::expr::VfsEntry;
use graphix_package_core::testing::{Mode, fixture_runtime, next_update};
use netidx_value::Value;
use std::time::Duration;
use tokio::time::Instant;

/// Mount a module exporting a constant, compile a root block that declares
/// the module and reads the constant: in every mode the first value is
/// the constant.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn module_constant_reaches_root_reader() -> Result<()> {
    for mode in Mode::ALL {
        let files = [
            ("/m0.gxi", VfsEntry::from(arcstr::literal!("val c: u16;"))),
            ("/m0.gx", VfsEntry::from(arcstr::literal!("let c = u16:1000"))),
        ];
        let (ctx, mut rx) =
            fixture_runtime(files, &crate::TEST_REGISTER, mode, |_| {}).await?;
        let res = ctx.rt.compile(arcstr::literal!("{ mod m0; m0::c }")).await?;
        let deadline = Instant::now() + Duration::from_secs(10);
        match next_update(&mut rx, res.exprs[0].id, deadline).await? {
            Value::U16(1000) => (),
            other => bail!("{mode:?}: expected u16:1000, got {other:?}"),
        }
        ctx.shutdown().await;
    }
    Ok(())
}
