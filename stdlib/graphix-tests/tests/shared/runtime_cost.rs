use anyhow::Result;
use graphix_package_core::testing::{
    Collect, Mode, compile_result, fixture_runtime, result_source, updates_until,
};
use netidx_value::{ValArray, Value};

use crate::TEST_REGISTER;

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
            let values =
                updates_until(&ctx.rt, &mut rx, result.exprs[0].id, Collect::Settled)
                    .await?;
            // CR claude for claude: [test-gap/low] The bound counts only boundaries a
            // `Prune` skipped. When `skip_idle_slot` does not short-circuit an idle
            // slot and the slot's body boundary is marked dirty, the body is walked,
            // and walks are counted in `boundaries().0`, which this test never reads. A
            // change that marks every slot body on each write to `i`, or that stops
            // wrapping lambda bodies in `Prune`, enters all 200 idle call sites every
            // cycle and still passes with `skipped` near zero. Take `(walked, skipped)`
            // before and after the run, and bound their sum by what the current code
            // walks (a few boundaries per cycle, not 200). Found by reading; not run.
            // (tests-lib-b-b-03)
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
