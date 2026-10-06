//! gui-datatable-02: data_table's sort comparator is not a total order;
//! `slice::sort_by` panics on it once a table has 21 rows or more.
//!
//! resort_by_column (widgets/data_table/subscriptions.rs:706) compares two
//! keys numerically when both parse as f64 (`partial_cmp(..)
//! .unwrap_or(Equal)`) and as strings otherwise. A NaN cell displays as
//! "NaN", which parses, and is then Equal to every number; a digit-leading
//! string such as "5 KB" sorts between numbers lexically ("10" < "5 KB" <
//! "9" < "10"). Rust's stable sort (driftsort, any slice longer than 20)
//! panics with "user-provided comparison function does not correctly
//! implement a total order" when it detects the inconsistency. The sort
//! runs on the GUI main thread: in `DataTableW::compile` (under the event
//! loop's `block_on`), `before_view` and `handle_update`.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_02.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_02 -- --nocapture
//!
//! Cases (all ascending by one column, rows in table order):
//!   control_all_numbers: 30 virtual rows, cpu = ((i*37)%101)/2.
//!   virtual_nan: the same with r7's cpu = 0.0/0.0 (a calculated
//!     column, `Map<string, Any>` source, no netidx).
//!   virtual_mixed_units: 25 virtual rows, size = "N KB" when i%3 == 0,
//!     else the i64 N = (i*37)%101.
//!   live_nan: 30 absolute rows published with sys::net::publish, r7
//!     publishes NaN; the values land, then the next frame's before_view
//!     re-sorts.
//!
//! Expected: every case compiles and sorts (NaN placed somewhere fixed,
//! numbers and strings in some total order); no panic.
//! Observed at c722befe (dev profile, rustc 1.98.1), 1 passed, 3 failed:
//!   RESULT control_all_numbers: no panic
//!   RESULT virtual_nan: PANIC: in DataTableW::compile: user-provided
//!     comparison function does not correctly implement a total order
//!   RESULT virtual_mixed_units: PANIC: in DataTableW::compile: (same)
//!   RESULT live_nan: PANIC: in before_view: (same)
//! RUST_BACKTRACE=1 shows each panic at core smallsort.rs:854
//! (panic_on_ord_violation) under driftsort_main <- resort_by_column.
//! A standalone copy of the comparator over the same key lists panics on
//! the NaN list at every n tried from 21 to 64 and on the mixed list at
//! n = 25 (not at 21, 30, 40: whether driftsort notices depends on the
//! input order); at n <= 20 (insertion sort) it never panics.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW};
use graphix_rt::{CompRes, GXEvent, NoExt};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{
    any::Any,
    panic::{AssertUnwindSafe, catch_unwind},
    time::Duration,
};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

fn virtual_cpu(nan_row: Option<i64>) -> String {
    let nan = match nan_row {
        Some(r) => format!("{r} => z / z,"),
        None => String::new(),
    };
    format!(
        r#"
use gui::data_table::{{data_table, text_column}};
let z = 0.0;
let empty: Map<string, f64> = {{}};
let cpu = array::fold(array::init(30, |i| i), empty, |m, i| map::insert(m, "r[i]", select i {{
  {nan}
  i => cast<f64>((i * 37) % 101)$ / 2.0
}}));
let tbl = {{ rows: array::init(30, |i| "r[i]"), columns: [text_column(#name: "cpu", #source: &cpu)] }};
let result = data_table(#sort_by: &[{{ column: "cpu", direction: `Ascending }}], #table: &tbl)
"#
    )
}

const VIRTUAL_MIXED_UNITS: &str = r#"
use gui::data_table::{data_table, text_column};
let empty: Map<string, [i64, string]> = {};
let size = array::fold(array::init(25, |i| i), empty, |m, i| map::insert(m, "r[i]", {
  let v = (i * 37) % 101;
  select i % 3 { 0 => "[v] KB", _ => v }
}));
let tbl = { rows: array::init(25, |i| "r[i]"), columns: [text_column(#name: "size", #source: &size)] };
let result = data_table(#sort_by: &[{ column: "size", direction: `Ascending }], #table: &tbl)
"#;

fn live_nan_program() -> String {
    let mut publishes = String::new();
    let mut rows = Vec::new();
    for i in 0..30i64 {
        let v = if i == 7 {
            "z / z".to_string()
        } else {
            format!("f64:{:?}", (((i * 37) % 101) as f64) / 2.0)
        };
        publishes.push_str(&format!("sys::net::publish(\"/local/dt_nan_live/r{i}/cpu\", {v});\n"));
        rows.push(format!("\"/local/dt_nan_live/r{i}\""));
    }
    let rows = rows.join(", ");
    format!(
        r#"
use gui::data_table::data_table;
let z = 0.0;
{publishes}
let tbl = {{ rows: [{rows}], columns: ["cpu"] }};
let result = data_table(#sort_by: &[{{ column: "cpu", direction: `Ascending }}], #table: &tbl)
"#
    )
}

fn panic_msg(p: Box<dyn Any + Send>) -> String {
    p.downcast_ref::<&str>()
        .map(|s| s.to_string())
        .or_else(|| p.downcast_ref::<String>().cloned())
        .unwrap_or_else(|| "<non-string panic payload>".into())
}

async fn wait_for(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(10));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev {
                        if id == target {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for the widget value"),
        }
    }
}

struct Session {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    /// The widget, or the panic message of its compile.
    widget: std::result::Result<GuiW<NoExt>, String>,
}

async fn session(code: &str) -> Result<Session> {
    let (tx, mut rx) = mpsc::channel(100);
    let vfs = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
        .await
        .context("init")?;
    let gx = ctx.rt.clone();
    let compiled = gx
        .compile(arcstr::literal!("{ mod test; test::result }"))
        .await
        .context("compile graphix code")?;
    let id = compiled.exprs[0].id;
    let v = wait_for(&mut rx, id).await?;
    let widget = match tokio::spawn(widgets::compile(gx.clone(), v)).await {
        Ok(r) => Ok(r.context("compile widget")?),
        Err(e) if e.is_panic() => Err(format!("in DataTableW::compile: {}", panic_msg(e.into_panic()))),
        Err(e) => bail!("join: {e}"),
    };
    Ok(Session { _ctx: ctx, _compiled: compiled, rx, widget })
}

/// Feed runtime updates to the widget for `d` without rendering, then
/// run one frame's `before_view`. Returns the first panic message.
async fn settle_then_frame(s: &mut Session, d: Duration) -> Result<Option<String>> {
    let rt = tokio::runtime::Handle::current();
    let w = match s.widget.as_mut() {
        Ok(w) => w,
        Err(m) => return Ok(Some(m.clone())),
    };
    let deadline = tokio::time::Instant::now() + d;
    loop {
        tokio::select! {
            Some(mut batch) = s.rx.recv() => {
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev {
                        let r = catch_unwind(AssertUnwindSafe(|| w.handle_update(&rt, id, &v)));
                        match r {
                            Ok(r) => { r?; }
                            Err(p) => return Ok(Some(format!("in handle_update: {}", panic_msg(p)))),
                        }
                    }
                }
            }
            _ = tokio::time::sleep_until(deadline) => break,
        }
    }
    for _ in 0..5 {
        if let Err(p) = catch_unwind(AssertUnwindSafe(|| w.before_view())) {
            return Ok(Some(format!("in before_view: {}", panic_msg(p))));
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    Ok(None)
}

fn report(case: &str, panic: Option<String>) {
    match &panic {
        None => println!("RESULT {case}: no panic"),
        Some(m) => println!("RESULT {case}: PANIC: {m}"),
    }
    assert!(panic.is_none(), "{case}: the data_table sort panicked: {}", panic.unwrap());
}

#[tokio::test(flavor = "current_thread")]
async fn control_all_numbers() -> Result<()> {
    let mut s = session(&virtual_cpu(None)).await?;
    let p = settle_then_frame(&mut s, Duration::from_millis(300)).await?;
    report("control_all_numbers", p);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn virtual_nan() -> Result<()> {
    let mut s = session(&virtual_cpu(Some(7))).await?;
    let p = settle_then_frame(&mut s, Duration::from_millis(300)).await?;
    report("virtual_nan", p);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn virtual_mixed_units() -> Result<()> {
    let mut s = session(VIRTUAL_MIXED_UNITS).await?;
    let p = settle_then_frame(&mut s, Duration::from_millis(300)).await?;
    report("virtual_mixed_units", p);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn live_nan() -> Result<()> {
    let mut s = session(&live_nan_program()).await?;
    let p = settle_then_frame(&mut s, Duration::from_secs(3)).await?;
    report("live_nan", p);
    Ok(())
}
