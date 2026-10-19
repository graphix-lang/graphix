//! Title: a program image written over a restored registration keeps a
//! package definition's seq lowerings under scope text that the next
//! restore does not relocate, so a runtime instance lowers again.
//! Command: copy to stdlib/graphix-tests/tests/ and run
//!   cargo test -p graphix-tests --test review_c_image_1 -- --nocapture
//! Expected: run C (the program image restored) keeps one lowering, as the
//!   unchained restore E does.
//! Observed: C holds 2 lowerings (the stale key and a fresh lowering of the
//!   same seq, whose ids the definition's check table has no rows for);
//!   B, D and E hold 1. Values agree.
//! review c-image: a program image written by a runtime that restored its
//! registration image keeps a package definition's lowerings under scope
//! text naming reserved-region expression ids, which the next restore
//! relocates; a runtime instance then misses the lowering.

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
const PROG: &str = "let n = 1; select n { k if k < 4 => n <- k + 1, _ => never() }; \
                    array::map(array::init(n, |i| i), |x| (rv::f(x), rv::f(cast<f64>(x)$)))";

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
async fn a_chained_restore_keeps_its_lowerings() -> Result<()> {
    // A: the registration compiled cold, written
    let (_, reg, _, _, _, _) = run(None, false).await?;
    let reg = reg.expect("registration image");
    // B: the registration restored, the program compiled and written
    let (b_vals, _, prog, b_restored, b_before, b_after) =
        run(Some(reg), true).await?;
    let prog = prog.expect("program image");
    eprintln!("B restored={b_restored} lowerings {b_before} -> {b_after} values {b_vals:?}");
    // C: the program image restored
    let (c_vals, _, _, c_restored, c_before, c_after) = run(Some(prog), false).await?;
    eprintln!("C restored={c_restored} lowerings {c_before} -> {c_after} values {c_vals:?}");
    // D: registration and program compiled cold in one run, written; E: restored
    let (d_vals, _, prog_d, _, d_before, d_after) = run(None, true).await?;
    let (e_vals, _, _, e_restored, e_before, e_after) =
        run(Some(prog_d.expect("program image")), false).await?;
    eprintln!("D lowerings {d_before} -> {d_after} values {d_vals:?}");
    eprintln!("E restored={e_restored} lowerings {e_before} -> {e_after} values {e_vals:?}");
    assert!(b_restored && c_restored);
    assert_eq!(b_vals, c_vals);
    assert_eq!(e_after, d_after, "an unchained restore keeps one lowering");
    assert_eq!(c_after, b_after, "the chained restore lowered again what the image holds");
    Ok(())
}
