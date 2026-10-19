//! Title: a function defined in a block (`#do<id>` scope) re-lowers its seq
//! at every runtime instance after a warm restore: the image relocates the
//! lowering key's scope text, not the definition's own scope text.
//! Command: copy to stdlib/graphix-tests/tests/ and run
//!   cargo test -p graphix-tests --test review_c_image_2 -- --nocapture
//! Expected: E (restored) holds as many lowerings as D (cold), 2.
//! Observed: E holds 3: the restored key `/#do<new>/#fn<new>` never matches
//!   the lookup `/#do<old>/#fn<new>`, so the seq lowers again with fresh
//!   ids the check table has no rows for. Values agree.

use ahash::AHashMap;
use anyhow::Result;
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{
    CFlag,
    expr::{Load, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{PackageRef, TestCtx, config};
use graphix_rt::{GXEvent, GXRt, NoExt, RegistrationImage};
use netidx::{path::Path, publisher::Value};
use poolshark::global::GPooled;
use tokio::sync::{mpsc, oneshot};

const RV: &str = "let f = 'a: Number + Singleton |x: 'a| -> 'a seq x { let a = x + x; a * x }";
const PROG: &str = "{ let g = |x: i64| -> i64 seq x { let a = x + 1; a * 2 }; \
                    let n = 1; select n { k if k < 4 => n <- k + 1, _ => never() }; \
                    array::map(array::init(n, |i| i), |x| g(x)) }";

async fn values(rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>) -> Vec<Value> {
    let mut out = Vec::new();
    loop {
        let idle = tokio::time::sleep(std::time::Duration::from_millis(500));
        tokio::pin!(idle);
        tokio::select! {
            _ = &mut idle => return out,
            batch = rx.recv() => match batch {
                None => return out,
                Some(mut batch) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(_, v) = e {
                            out.push(graphix_package_core::testing::data(v));
                        }
                    }
                }
            }
        }
    }
}

fn root() -> Result<ArcStr> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let mut ctx = GXRt::<NoExt>::new_state()?;
    let (_, root) = graphix_package::register_packages(&mut ctx, packages.iter().copied())?;
    Ok(ArcStr::from(format!("{root};\nmod rv")))
}

fn files() -> Vec<graphix_compiler::expr::ResolverRef> {
    vec![VfsResolver::new(AHashMap::from_iter([(
        Path::from("/rv.gx"),
        VfsEntry::from(ArcStr::from(RV)),
    )]))]
}

async fn run(
    image: Option<Bytes>,
    program: bool,
) -> Result<(Vec<Value>, Option<Bytes>, Option<Bytes>, bool, usize, usize)> {
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let (tx, mut rx) = mpsc::channel(10);
    let (reg_tx, reg_rx) = oneshot::channel();
    let (prog_tx, prog_rx) = oneshot::channel();
    let mut cfg = config(tx, packages, files(), |_| {})?
        .root(root()?)
        .flags(CFlag::FusionDisabled.into());
    if program {
        cfg = cfg.program(Load::Text(ArcStr::from(PROG))).program_image(prog_tx);
    } else {
        drop(prog_tx);
    }
    cfg = match image {
        None => cfg.registration(RegistrationImage::save(reg_tx)),
        Some(i) => {
            drop(reg_tx);
            cfg.registration(RegistrationImage::load(i))
        }
    };
    let ctx = TestCtx::start(cfg).await?;
    let reg = match reg_rx.await {
        Ok(r) => Some(r?),
        Err(_) => None,
    };
    let prog = match prog_rx.await {
        Ok(p) => Some(p?.image),
        Err(_) => None,
    };
    let _program = ctx.rt.program().await?;
    let before = ctx.rt.env_stats().await?.lowerings_len;
    let vals = values(&mut rx).await;
    let stats = ctx.rt.env_stats().await?;
    ctx.shutdown().await;
    Ok((vals, reg, prog, stats.restored, before, stats.lowerings_len))
}

#[tokio::test]
async fn a_block_function_keeps_its_lowerings() -> Result<()> {
    let (d_vals, _, prog_d, _, d_before, d_after) = run(None, true).await?;
    let (e_vals, _, _, e_restored, e_before, e_after) =
        run(Some(prog_d.expect("program image")), false).await?;
    eprintln!("D lowerings {d_before} -> {d_after} values {d_vals:?}");
    eprintln!("E restored={e_restored} lowerings {e_before} -> {e_after} values {e_vals:?}");
    assert!(e_restored);
    assert_eq!(d_vals, e_vals);
    assert_eq!(e_after, d_after, "the restore lowered again what the image holds");
    Ok(())
}
