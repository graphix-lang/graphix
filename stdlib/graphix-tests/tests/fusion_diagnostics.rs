use anyhow::Result;
use arcstr::literal;
use graphix_compiler::FAILURE_TARGET;
use graphix_package_core::testing::{Mode, PackageRef, fixture_runtime, next_update};
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
async fn unused_qops_still_report_errors() -> Result<()> {
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
    Ok(())
}
