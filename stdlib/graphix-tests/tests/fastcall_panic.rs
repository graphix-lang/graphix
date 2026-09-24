// A fast fn that panics fails the runtime the same way under both
// engines: the panic unwinds out of the update that ran it. Its own test
// binary, since a panic that cannot unwind aborts the process.

use anyhow::{Result, bail};
use graphix_compiler::{BitFlags, CFlag, Effect, ExecCtx, FastCall, Rt, UserEvent};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, fast_eval, testing::init_with_flags_and_setup,
};
use graphix_rt::GXEvent;
use netidx_value::Value;
use std::time::Duration;

fn fc_panic_at_three(args: &[Value]) -> Option<Value> {
    match args {
        [Value::I64(3)] => panic!("panic_probe: three"),
        [Value::I64(n)] => Some(Value::I64(*n)),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct PanicProbe;

graphix_package_core::unit_image_state!(PanicProbe);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PanicProbe {
    const NAME: &str = "panic_probe";
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_panic_at_three)));

    fn eval(&mut self, ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(ctx, fc_panic_at_three, from)
    }
}

/// The values the program produced before its runtime died.
async fn values_until_death(flags: BitFlags<CFlag>) -> Result<Vec<Value>> {
    let packages: &[graphix_package_core::testing::PackageRef] =
        graphix_package::package_refs!();
    let (tx, mut rx) = tokio::sync::mpsc::channel(10);
    let ctx = init_with_flags_and_setup(tx, packages, vec![], flags, |ctx| {
        ctx.register_builtin::<CachedArgs<PanicProbe>>().unwrap()
    })
    .await?;
    let compiled = ctx
        .rt
        .compile(arcstr::literal!(
            "{ let probe = |x: i64| -> i64 'panic_probe; \
               let x = array::iter([1, 2, 3, 4]); \
               probe(x) + 1 }"
        ))
        .await?;
    let eid = compiled.exprs[0].id;
    let mut out = Vec::new();
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    loop {
        match tokio::time::timeout_at(deadline, rx.recv()).await {
            Err(_) => bail!("timeout: the runtime outlived the panic, saw {out:?}"),
            Ok(None) => return Ok(out),
            Ok(Some(mut batch)) => {
                for e in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = e
                        && id == eid
                    {
                        out.push(v)
                    }
                }
            }
        }
    }
}

#[tokio::test(flavor = "current_thread")]
async fn fast_fn_panic_fails_the_runtime_in_both_engines() -> Result<()> {
    let interp = values_until_death(CFlag::FusionDisabled.into()).await?;
    let jit = values_until_death(BitFlags::empty()).await?;
    assert_eq!(interp, [Value::I64(2), Value::I64(3)]);
    assert_eq!(jit, interp);
    Ok(())
}
