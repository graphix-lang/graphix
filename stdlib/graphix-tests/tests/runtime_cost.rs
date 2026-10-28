use anyhow::Result;
use graphix_package_core::testing::{
    Mode, compile_result, fixture_runtime, result_source, updates_until_quiet,
};
use netidx_value::{ValArray, Value};
use std::time::Duration;
use tokio::time::Instant;

const TEST_REGISTER: &[&dyn graphix_package::Package<graphix_rt::NoExt>] =
    graphix_package::package_refs!();

#[tokio::test]
async fn idle_slots_do_not_enter_their_call_sites() -> Result<()> {
    const PROGRAM: &str = r#"{
        let i = 0;
        i <- select i { x if x < 100 => x + 1, _ => never() };
        let z = 0;
        let rs = array::concat([&i], array::init(200, |_| &z));
        let mapped = array::map(rs, |r| *r + 1);
        (i, mapped[0]$, mapped[200]$)
    }"#;
    for defaults in [false, true] {
        let program = if defaults {
            r#"{
            let i = 0;
            i <- select i { x if x < 100 => x + 1, _ => never() };
            let z = 0;
            let rs = array::concat([&i], array::init(200, |_| &z));
            let mapped = array::map(rs, |#extra: i64 = i, r| *r + extra + 1);
            (i, mapped[0]$, mapped[200]$)
          }"#
        } else {
            PROGRAM
        };
        for mode in Mode::ALL {
            let (ctx, mut rx) = fixture_runtime(
                [("/test.gx", result_source(program))],
                TEST_REGISTER,
                mode,
                |ctx| ctx.control.set_prune_mode(mode.prune()),
            )
            .await?;
            let control = ctx.rt.control();
            let before = control.boundaries().1;
            let result = compile_result(&ctx).await?;
            let values = updates_until_quiet(
                &ctx.rt,
                &mut rx,
                result.exprs[0].id,
                Duration::from_millis(100),
                Instant::now() + Duration::from_secs(20),
            )
            .await?;
            let skipped = control.boundaries().1 - before;
            ctx.shutdown().await;
            assert_eq!(values.len(), 101, "{mode:?}");
            for (i, value) in values.iter().enumerate() {
                let expected = Value::Array(ValArray::from_iter_exact(
                    [
                        Value::I64(i as i64),
                        Value::I64(i as i64 + 1 + if defaults { i as i64 } else { 0 }),
                        Value::I64(1 + if defaults { i as i64 } else { 0 }),
                    ]
                    .into_iter(),
                ));
                assert_eq!(value, &expected, "{mode:?}, {i}");
            }
            if !defaults {
                assert!(skipped < 5000, "{mode:?}: entered {skipped} idle boundaries");
            }
        }
    }
    Ok(())
}
