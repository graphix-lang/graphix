use anyhow::Result;
use arcstr::literal;
use graphix_compiler::FAILURE_TARGET;
use graphix_package_core::testing::{
    Mode, PackageRef, compile_result, fixture_runtime, next_update, result_source,
    updates_until_quiet,
};
use log::{Level, LevelFilter, Log, Metadata, Record};
use netidx_value::Value;
use std::{
    sync::atomic::{AtomicUsize, Ordering::Relaxed},
    time::Duration,
};
use tokio::time::Instant;

static FAILURES: AtomicUsize = AtomicUsize::new(0);
static IGNORED: AtomicUsize = AtomicUsize::new(0);

struct Failures;

impl Log for Failures {
    fn enabled(&self, meta: &Metadata<'_>) -> bool {
        meta.target() == FAILURE_TARGET
            || (meta.level() == Level::Warn
                && matches!(
                    meta.target(),
                    "graphix_compiler::node::error"
                        | "graphix_compiler::fusion::emit_helpers"
                ))
    }

    fn log(&self, record: &Record<'_>) {
        if record.target() == FAILURE_TARGET {
            FAILURES.fetch_add(1, Relaxed);
        } else if self.enabled(record.metadata()) {
            IGNORED.fetch_add(1, Relaxed);
        }
    }

    fn flush(&self) {}
}

#[tokio::test]
async fn swallowed_failure_diagnostics() -> Result<()> {
    log::set_logger(&Failures)?;
    log::set_max_level(LevelFilter::Warn);
    let packages: &[PackageRef] = graphix_package::package_refs!();
    for mode in Mode::ALL {
        let (ctx, mut rx) = fixture_runtime([], packages, mode, |_| {}).await?;
        ctx.rt.control().reset_invocations();
        FAILURES.store(0, Relaxed);
        IGNORED.store(0, Relaxed);
        let result = ctx
            .rt
            .compile(literal!(
                r#"{
                    let xs: Array<[i64, Error<`FusionDiagnostics>]> =
                        [1, error(`FusionDiagnostics), 3];
                    let h = |r: [i64, Error<`FusionDiagnostics>]| -> i64 {
                        let unused = r?;
                        5
                    };
                    let g = |r: [i64, Error<`FusionDiagnostics>]| -> i64 { r$; 7 };
                    #[native] [array::map(xs, |r| h(r)), array::map(xs, |r| g(r))]
                }"#
            ))
            .await?;
        let value = next_update(
            &mut rx,
            result.exprs[0].id,
            Instant::now() + Duration::from_secs(5),
        )
        .await?;
        let Value::Array(rows) = value else { panic!("{mode:?}: {value:?}") };
        assert_eq!(rows.len(), 2, "{mode:?}");
        for (row, n) in rows.iter().zip([5, 7]) {
            let Value::Array(row) = row else { panic!("{mode:?}: {row:?}") };
            assert_eq!(row.len(), 3, "{mode:?}");
            assert!(row.iter().all(|v| *v == Value::I64(n)), "{mode:?}: {row:?}");
        }
        assert_eq!((FAILURES.load(Relaxed), IGNORED.load(Relaxed)), (1, 1), "{mode:?}");
        assert!(mode.node_walk() || ctx.rt.control().invocations().1 > 0);
        ctx.shutdown().await;
    }
    let cases: [(&str, &[i64], (usize, usize), (usize, usize)); 3] = [
        (
            r#"{
            let step = 0;
            step <- select step { s if s < 3 => s + 1, _ => never() };
            let f = |x: i64, d: i64| select x {
                0 => 0,
                _ if (10 /? d)$ > 1 => 1,
                _ => 2
            };
            #[native] f(0, step - step)
        }"#,
            &[0],
            (0, 0),
            (0, 4),
        ),
        (
            r#"{
            let k = 0;
            k <- select k < 3 { true => k + 1, false => never() };
            let h = |x: i64| -> i64 select x {
                0 => 1, 1 => 5,
                _ => { let e: Result<i64, `E> = error(`E); e?; 2 }
            };
            #[native] h(k)
        }"#,
            &[1, 5, 2, 2],
            (1, 0),
            (0, 0),
        ),
        (
            r#"{
            let k = 0;
            k <- select k < 5 { true => k + 1, false => never() };
            let g = |m: i64| -> i64 { let e: Result<i64, `E> = error(`E); e?; m };
            let rec f = |n: i64, acc: i64| -> i64 select (n, acc) {
                (0, a) => g(a),
                (n, a) => f(n - 1, a + n)
            };
            #[native] f(k, 0)
        }"#,
            &[0, 1, 3, 6, 10, 15],
            (6, 0),
            (1, 0),
        ),
    ];
    for (program, expected, walked, fused) in cases {
        for mode in Mode::ALL {
            let (ctx, mut rx) = fixture_runtime(
                [("/test.gx", result_source(program))],
                packages,
                mode,
                |_| {},
            )
            .await?;
            ctx.rt.control().reset_invocations();
            FAILURES.store(0, Relaxed);
            IGNORED.store(0, Relaxed);
            let result = compile_result(&ctx).await?;
            let values = updates_until_quiet(
                &ctx.rt,
                &mut rx,
                result.exprs[0].id,
                Duration::from_millis(100),
                Instant::now() + Duration::from_secs(10),
            )
            .await?;
            assert!(mode.node_walk() || ctx.rt.control().invocations().1 > 0);
            ctx.shutdown().await;
            assert_eq!(values.len(), expected.len(), "{mode:?}: {values:?}");
            for (v, n) in values.iter().zip(expected) {
                assert_eq!(v, &Value::I64(*n));
            }
            let counts = (FAILURES.load(Relaxed), IGNORED.load(Relaxed));
            assert_eq!(
                counts,
                if mode.node_walk() { walked } else { fused },
                "{mode:?}: {program}"
            );
        }
    }
    Ok(())
}
