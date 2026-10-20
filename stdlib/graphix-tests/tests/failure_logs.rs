//! What a running program reports only under `FAILURE_TARGET`: a test
//! binary of its own, so its logger is the process's.

use anyhow::Result;
use arcstr::literal;
use graphix_compiler::FAILURE_TARGET;
use graphix_package_core::testing::{Mode, PackageRef, fixture_runtime, next_update};
use log::{LevelFilter, Log, Metadata, Record};
use netidx_value::Value;
use std::{sync::Mutex, time::Duration};
use tokio::time::Instant;

static LOGGED: Mutex<Vec<String>> = Mutex::new(Vec::new());

struct Failures;

impl Log for Failures {
    fn enabled(&self, meta: &Metadata<'_>) -> bool {
        meta.target() == FAILURE_TARGET
    }

    fn log(&self, record: &Record<'_>) {
        if self.enabled(record.metadata()) {
            LOGGED.lock().unwrap().push(record.args().to_string());
        }
    }

    fn flush(&self) {}
}

/// A plain write to a publication whose handler's argument type the check
/// left partly open reaches no handler, and says so.
#[tokio::test]
async fn a_write_an_open_handler_cannot_take_is_logged() -> Result<()> {
    log::set_logger(&Failures)?;
    log::set_max_level(LevelFilter::Warn);
    let packages: &[PackageRef] = graphix_package::package_refs!();
    for mode in Mode::ALL {
        let (ctx, mut rx) = fixture_runtime([], packages, mode, |_| {}).await?;
        LOGGED.lock().unwrap().clear();
        let result = ctx
            .rt
            .compile(literal!(
                r#"{
                    let n: i64 = -1;
                    let p = "/local/ow_open_logged";
                    sys::net::publish(#on_write: |v: Array<'b>| n <- v ~ array::len(v), p, 0);
                    let s: i64 = sys::net::subscribe(p)$;
                    sys::net::write(p, once(s) ~ {a: 1, b: 2});
                    sys::time::after_idle(duration: 300.ms, s) ~ n
                }"#
            ))
            .await?;
        let value = next_update(
            &mut rx,
            result.exprs[0].id,
            Instant::now() + Duration::from_secs(5),
        )
        .await?;
        assert_eq!(value, Value::I64(-1), "{mode:?}");
        let logged = LOGGED.lock().unwrap();
        assert!(
            logged.iter().any(|m| m.contains("/local/ow_open_logged")
                && m.contains("argument type is not known")),
            "{mode:?}: {logged:?}"
        );
        drop(logged);
        ctx.shutdown().await;
    }
    Ok(())
}
