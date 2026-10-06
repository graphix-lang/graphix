//! tests-ui-03: a Sparkline column's `history_seconds` is kept whatever
//! finite positive value it has (data_table/types.rs:235), and both
//! cutoff sites compute `now - Duration::from_secs_f64(hs)`
//! (subscriptions.rs:665 at widget compile, subscriptions.rs:207 in the
//! dispatch task), which panics for a huge history.
//!
//! Run with this file at stdlib/graphix-package-gui/tests/review_tests_ui_03.rs:
//!   cargo test -p graphix-package-gui --test review_tests_ui_03 -- --nocapture
//!
//! Expected: every case passes (a history longer than the clock keeps
//! everything).
//! Observed (Linux, rustc 1.98.1, HEAD c722befe): the controls pass;
//!   default source 1e19: widget compile panicked at
//!     data_table/subscriptions.rs:665:30: overflow when subtracting
//!     duration from instant
//!   default source 1e20: widget compile panicked: cannot convert float
//!     seconds to Duration: value is either too big or NaN
//!   netidx source 1e19: dispatch task panicked at
//!     data_table/subscriptions.rs:207:54 (same message); on_update never
//!     fired
//!   netidx source 1e20: dispatch task panicked (from_secs_f64); on_update
//!     never fired
//!   test result: FAILED (4 of 7 cases)

use anyhow::{Context, Result, bail};
use futures::FutureExt;
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing;
use graphix_rt::{GXEvent, NoExt};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{
    any::Any,
    panic::AssertUnwindSafe,
    sync::Mutex,
    time::{Duration, Instant},
};
use tokio::sync::mpsc;

const REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const TEMPLATE: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*;
let log = "";
sys::net::publish("/local/tests_ui_03/TAG/r0/load", 7.5);
let tbl = { rows: [ROW], columns: [
        { name: "load", typ: `Sparkline({ history_seconds: HS, min: null, max: null }),
            display_name: null,
            source: &SRC,
            on_resize: &null, width: &null }
    ] };
let result = data_table(
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #table: &tbl
)
"#;

struct Case {
    name: &'static str,
    history_seconds: &'static str,
    netidx: bool,
}

const CASES: &[Case] = &[
    Case { name: "default source, history 60 (control)", history_seconds: "60.0", netidx: false },
    Case { name: "default source, history 1e18 (control)", history_seconds: "1e18", netidx: false },
    Case { name: "default source, history 1e19", history_seconds: "1e19", netidx: false },
    Case { name: "default source, history 1e20", history_seconds: "1e20", netidx: false },
    Case { name: "netidx source, history 60 (control)", history_seconds: "60.0", netidx: true },
    Case { name: "netidx source, history 1e19", history_seconds: "1e19", netidx: true },
    Case { name: "netidx source, history 1e20", history_seconds: "1e20", netidx: true },
];

static PANICS: Mutex<Vec<String>> = Mutex::new(Vec::new());

fn payload_msg(p: &(dyn Any + Send)) -> String {
    p.downcast_ref::<&str>()
        .map(|s| s.to_string())
        .or_else(|| p.downcast_ref::<String>().cloned())
        .unwrap_or_default()
}

/// The next value of the root tuple `(result, log)`, or None at the
/// timeout.
async fn next_root(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    root: ExprId,
    timeout: Duration,
) -> Result<Option<(Value, Value)>> {
    let sleep = tokio::time::sleep(timeout);
    tokio::pin!(sleep);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                let mut found = None;
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev
                        && id == root
                    {
                        found = Some(v);
                    }
                }
                if let Some(v) = found {
                    return Ok(Some(v.cast_to::<(Value, Value)>()?));
                }
            }
            _ = &mut sleep => return Ok(None),
        }
    }
}

async fn run_case(case: &Case, tag: usize) -> Result<()> {
    let (row, src) = if case.netidx {
        (format!("\"/local/tests_ui_03/{tag}/r0\""), "`Netidx(null)".to_string())
    } else {
        ("\"r0\"".to_string(), "{\"r0\" => 7.5}".to_string())
    };
    let code = TEMPLATE
        .replace("TAG", &tag.to_string())
        .replace("ROW", &row)
        .replace("SRC", &src)
        .replace("HS", case.history_seconds);
    let (tx, mut rx) = mpsc::channel(100);
    let vfs = ahash::AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code.as_str())),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
        .await?;
    let gx = ctx.rt.clone();
    let compiled = gx
        .compile(arcstr::literal!("{ mod test; (test::result, test::log) }"))
        .await
        .context("compile graphix code")?;
    let root = compiled.exprs[0].id;
    let (widget, _) = next_root(&mut rx, root, Duration::from_secs(5))
        .await?
        .context("no initial value")?;
    let before = PANICS.lock().unwrap().len();
    let w = match AssertUnwindSafe(graphix_package_gui::widgets::compile(
        gx.clone(),
        widget,
    ))
    .catch_unwind()
    .await
    {
        Err(p) => bail!("widget compile panicked: {}", payload_msg(&*p)),
        Ok(r) => r.context("compile widget")?,
    };
    if case.netidx {
        let deadline = Instant::now() + Duration::from_secs(3);
        loop {
            let left = deadline.saturating_duration_since(Instant::now());
            match next_root(&mut rx, root, left).await? {
                Some((_, Value::String(log))) if !log.is_empty() => {
                    eprintln!("    on_update fired: {log}");
                    break;
                }
                Some(_) => continue,
                None => {
                    let panics = PANICS.lock().unwrap()[before..].to_vec();
                    bail!("on_update never fired within 3 s; panics meanwhile: {panics:?}")
                }
            }
        }
    }
    drop(w);
    drop(compiled);
    drop(ctx);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn sparkline_huge_history_seconds() -> Result<()> {
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        PANICS.lock().unwrap().push(payload_msg(info.payload()));
        prev(info);
    }));
    let mut failed = vec![];
    for (i, case) in CASES.iter().enumerate() {
        match run_case(case, i).await {
            Ok(()) => eprintln!("PASS {}", case.name),
            Err(e) => {
                eprintln!("FAIL {}: {e:#}", case.name);
                failed.push(case.name);
            }
        }
    }
    if !failed.is_empty() {
        bail!("cases failed: {failed:?}");
    }
    Ok(())
}
