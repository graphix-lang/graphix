//! A `mod m;` statement is never DEAD: its binds publish into the
//! persistent env — readable by Refs outside the fused region and by
//! every later-installed top expression — so fusion's dead-statement
//! elimination must treat a Module as an effect
//! (`stmt_subtree_effect_free`, fusion/emit.rs). The whole-file Do
//! region once eliminated the module statement while routing its
//! constant in as a FEEDER: the splice then deleted the module (the
//! feeder's only producer) with the consumed region, and the kernel
//! waited forever on its own input — `mod m0; m0::c` under the shell's
//! file wrap printed nothing under fusion and `1000` under
//! `--no-fusion` (found probing the fuzzer's cross-module vocabulary,
//! 2026-07-08).

use anyhow::{Result, anyhow};
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, expr::VfsResolver};
use graphix_rt::GXEvent;
use netidx_value::Value;
use std::time::Duration;

/// Mount a module exporting a constant, compile a root Do that
/// declares the module and reads the constant, return the first
/// published value.
async fn first_value(flags: BitFlags<CFlag>) -> Result<Value> {
    let (tx, mut rx) = tokio::sync::mpsc::channel(1024);
    let tbl = ahash::AHashMap::from_iter([
        (
            netidx_core::path::Path::from("/m0.gxi"),
            graphix_compiler::expr::VfsEntry::from(arcstr::literal!("val c: u16;")),
        ),
        (
            netidx_core::path::Path::from("/m0.gx"),
            graphix_compiler::expr::VfsEntry::from(arcstr::literal!("let c = u16:1000")),
        ),
    ]);
    let ctx = graphix_package_core::testing::init_with_flags_and_setup(
        tx,
        &crate::TEST_REGISTER,
        vec![VfsResolver::new(tbl)],
        flags,
        |_| {},
    )
    .await?;
    let compiled = ctx.rt.compile(arcstr::literal!("{ mod m0; m0::c }")).await?;
    let eid = compiled.exprs[0].id;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    let v = loop {
        let mut batch = tokio::time::timeout_at(deadline, rx.recv())
            .await
            .map_err(|_| anyhow!("timeout: module constant never published"))?
            .ok_or_else(|| anyhow!("runtime died"))?;
        let mut found = None;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                if id == eid {
                    found = Some(v);
                }
            }
        }
        if let Some(v) = found {
            break v;
        }
    };
    ctx.shutdown().await;
    Ok(v)
}

fn assert_c(v: &Value) -> Result<()> {
    match v {
        Value::U16(1000) => Ok(()),
        other => Err(anyhow!("expected u16:1000, got {other:?}")),
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn module_constant_reaches_root_reader_jit() -> Result<()> {
    let v = first_value(BitFlags::empty()).await?;
    assert_c(&v)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn module_constant_reaches_root_reader_interp() -> Result<()> {
    let v = first_value(CFlag::FusionDisabled.into()).await?;
    assert_c(&v)
}
