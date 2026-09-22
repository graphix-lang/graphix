// The value-hook loan is exclusive: a builtin's eval that takes no loan
// compares structurally, and it does so inside a core implementation
// too, where the dispatch that ran it had a loan installed.

use anyhow::Result;
use graphix_compiler::{Effect, ExecCtx, Rt, UserEvent};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, testing::eval_with_setup,
};
use netidx_value::Value;

#[derive(Debug, Default)]
struct Probe;

graphix_package_core::unit_image_state!(Probe);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for Probe {
    const NAME: &str = "hook_loan_probe";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        Some(Value::Bool(from.0[0].as_ref()? == from.0[1].as_ref()?))
    }
}

#[tokio::test(flavor = "current_thread")]
async fn nested_builtin_runs_unarmed() -> Result<()> {
    let packages: &[graphix_package_core::testing::PackageRef] =
        graphix_package::package_refs!();
    let (value, ctx) = eval_with_setup(
        r#"{
            type Key = Abstract<i64>;
            impl Eq for Key { let eq = |a, b| true };
            let probe = |a: Key, b: Key| -> bool 'hook_loan_probe;
            type Outer = Abstract<i64>;
            impl Display for Outer {
                let fmt = |x| select probe(Key(1), Key(2)) {
                    true => "armed",
                    false => "unarmed"
                }
            };
            (probe(Key(1), Key(2)), "[Outer(0)]")
        }"#,
        packages,
        |ctx| ctx.register_builtin::<CachedArgs<Probe>>().unwrap(),
    )
    .await?;
    ctx.shutdown().await;
    let Value::Array(parts) = value else { panic!("expected a tuple, got {value}") };
    assert_eq!(parts[0], Value::Bool(false));
    assert_eq!(parts[1], Value::from("unarmed"));
    Ok(())
}
