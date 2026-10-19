// Title: cold_and_warm's `restored` check passes for a warm session whose
// image holds no program at all
// Command: copy to stdlib/graphix-tests/tests/review_tests_modules_image_1.rs, then
//   timeout -s KILL 2400 cargo test -p graphix-tests --test review_tests_modules_image_1 -- --nocapture
// Expected: a warm run that restored no program is told apart from one that did
//   (image.rs's cold_and_warm asserts `restored` to mean the program came back).
// Observed (HEAD 9445a1d21): loading a registration-only image under
//   `.program(Load::Text(..))`, exactly as image.rs's session_with builds the warm
//   run, gives
//   PROBE restored_without_the_program: restored=true
//     cold=[String("A"), String("1 1")] warm=[String("A"), String("1 1")]
//   so imaged_body_reads_are_registered (this program) and every other
//   cold_and_warm test pass when the image carries no program: the warm run
//   compiled the source cold.

use anyhow::Result;
use bytes::Bytes;
use graphix_compiler::{CFlag, ParMode, expr::Load};
use graphix_package_core::testing::{PackageRef, TestCtx, config};
use graphix_rt::{GXEvent, RegistrationImage};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use tokio::sync::{mpsc, oneshot};

fn reg() -> &'static [PackageRef] {
    graphix_package::package_refs!()
}

async fn first_values(rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>) -> Vec<Value> {
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

/// image.rs's session_with, verbatim in effect; also returns the
/// image the cold run wrote with no program when `regonly`.
async fn session(
    program: &str,
    image: Option<Bytes>,
    par: ParMode,
    budget: Option<u64>,
) -> Result<(Vec<Value>, Option<Bytes>, bool, u64)> {
    let (tx, mut rx) = mpsc::channel(10);
    let (reg_tx, _reg_rx) = oneshot::channel();
    let (prog_tx, prog_rx) = oneshot::channel();
    let cfg = config(tx, reg(), vec![], move |ctx| {
        ctx.control.set_par_mode(par);
        if let Some(b) = budget {
            ctx.control.set_activation_budget(b)
        }
    })?
    .flags(CFlag::FusionDisabled.into())
    .program(Load::Text(program.into()));
    let cfg = match image {
        None => cfg.registration(RegistrationImage::save(reg_tx)).program_image(prog_tx),
        Some(image) => {
            drop(prog_tx);
            cfg.registration(RegistrationImage::load(image))
        }
    };
    let ctx = TestCtx::start(cfg).await?;
    let written = match prog_rx.await {
        Ok(image) => Some(image?.image),
        Err(_) => None,
    };
    let _program = ctx.rt.program().await?.expect("the program compiled or restored");
    let values = first_values(&mut rx).await;
    let restored = ctx.rt.env_stats().await?.restored;
    let forks = ctx.rt.control().forks();
    ctx.shutdown().await;
    Ok((values, written, restored, forks))
}

/// A registration image holding no program.
async fn registration_only() -> Result<Bytes> {
    let (tx, _rx) = mpsc::channel(10);
    let (reg_tx, reg_rx) = oneshot::channel();
    let ctx = TestCtx::start(
        config(tx, reg(), vec![], |_| {})?
            .flags(CFlag::FusionDisabled.into())
            .registration(RegistrationImage::save(reg_tx)),
    )
    .await?;
    let image = reg_rx.await??;
    ctx.shutdown().await;
    Ok(image)
}

/// cold_and_warm's check (`restored`) passes for a warm session whose
/// image holds no program at all: the program compiles cold.
#[tokio::test]
async fn restored_without_the_program() -> Result<()> {
    let program = r#"
        let to_b = sys::time::timer(duration:300.ms, false);
        let sel = 0;
        sel <- to_b ~ 1;
        let gt = sys::time::timer(duration:150.ms, false);
        let g: i64 = never();
        g <- gt ~ 42;
        let f = |x: i64| -> string "[x] [count(g)]";
        select sel { 0 => "A", _ => f(sel) }
    "#;
    let (cold, _, _, _) = session(program, None, ParMode::Off, None).await?;
    let image = registration_only().await?;
    let (warm, _, restored, _) = session(program, Some(image), ParMode::Off, None).await?;
    eprintln!("PROBE restored_without_the_program: restored={restored} cold={cold:?} warm={warm:?}");
    Ok(())
}
