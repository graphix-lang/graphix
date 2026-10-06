//! tests-ui.r2-01: data_table's sort comparator is not a total order, so
//! a NaN cell, or text cells beside numeric ones, panic `sort_by`.
//!
//! subscriptions.rs `resort_by_column` (:692-731, the comparator at :710) compares two keys
//! numerically when both parse as f64, with `partial_cmp(..)
//! .unwrap_or(Equal)`, else as text. A cell is shown with Rust's
//! Display, so an f64 NaN is "NaN", which parses back to NaN: NaN is
//! Equal to every number, and equality is not transitive. Text
//! between two numbers whose text and numeric orders disagree closes a
//! cycle ("9" < "10" by number, "10" < "10.0.1" < "9" by text). The
//! stable sort past 20 elements (driftsort) panics on such a comparator.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_tests_ui_r2_01.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_tests_ui_r2_01 -- --nocapture
//!
//! Each case compiles a 40-row `data_table` sorted ascending on column
//! `v` the way the GUI event loop does (`widgets::compile`), then feeds
//! it every runtime update (`handle_update`) and calls `before_view`
//! before each frame, for 3 s. Expected: no case panics. Observed at
//! HEAD c722befe (rustc 1.98.1), "test result: FAILED. 2 passed; 3
//! failed", each failure a panic at library/core/src/slice/sort/shared/
//! smallsort.rs:854:5 "user-provided comparison function does not
//! correctly implement a total order":
//!   a_virtual_control        ok, sorted (r39 = 1/3 drawn first)
//!   b_virtual_nan            FAILED: panic in widgets::compile (the
//!                            resort in DataTableW::compile); row 17 of
//!                            40 is 0.0 / 0.0
//!   c_subscribed_nan         FAILED: panic in before_view; the values
//!                            are published, row 17 publishes 0.0 / 0.0
//!   d_subscribed_control     ok, sorted (r21 = 0 drawn first); the same
//!                            published values without the NaN
//!   e_virtual_version_text   FAILED: panic in widgets::compile; text
//!                            cells "9", "10", "10.0.1", "1.2.3", ...,
//!                            no NaN at all
//! In the GUI, compile (window.rs block_on) and before_view
//! (event_loop.rs about_to_wait) run on the main thread, which nothing
//! catches (graphix-shell main.rs runs the event loop bare): the
//! process exits.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use futures::FutureExt;
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt};
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

const N: usize = 40;

fn rows(prefix: &str) -> String {
    (0..N).map(|i| format!("\"{prefix}r{i}\"")).collect::<Vec<_>>().join(", ")
}

/// A data_table over rows `r0..r39` whose sort column `v` is virtual:
/// row `ri`'s value is `val(i)`, a Graphix expression.
fn virtual_table(val: impl Fn(usize) -> String) -> String {
    let entries: Vec<String> = (0..N).map(|i| format!("\"r{i}\" => {}", val(i))).collect();
    format!(
        r#"
use gui::*; use gui::data_table::{{self, *}}; use sys::*;
let sel = "";
let vals = {{{}}};
let tbl = {{ rows: [{}], columns: [
    {{ name: "v", typ: `Text({{ on_edit: null }}), display_name: null,
        source: &vals, on_resize: &null, width: &null }}
] }};
let result = data_table(
    #sort_by: &[{{ column: "v", direction: `Ascending }}],
    #on_select: |#path: string| sel <- path,
    #table: &tbl
)
"#,
        entries.join(", "),
        rows("")
    )
}

/// A data_table over netidx rows `/local/<tag>/r0..r39` whose column
/// `v` is subscribed; row `ri` publishes `val(i)`.
fn subscribed_table(tag: &str, val: impl Fn(usize) -> String) -> String {
    let prefix = format!("/local/{tag}/");
    let publishes: String = (0..N)
        .map(|i| format!("sys::net::publish(\"{prefix}r{i}/v\", {});\n", val(i)))
        .collect();
    format!(
        r#"
use gui::*; use gui::data_table::{{self, *}}; use sys::*;
let sel = "";
{publishes}
let tbl = {{ rows: [{}], columns: ["v"] }};
let result = data_table(
    #sort_by: &[{{ column: "v", direction: `Ascending }}],
    #on_select: |#path: string| sel <- path,
    #table: &tbl
)
"#,
        rows(&prefix)
    )
}

fn f64_lit(x: f64) -> String {
    format!("{x:?}")
}

/// Row i of the descending column: (40 - i) / 3, row 17 NaN when `nan`.
fn descending(nan: bool) -> impl Fn(usize) -> String {
    move |i| {
        if nan && i == 17 { "0.0 / 0.0".into() } else { f64_lit((N - i) as f64 / 3.0) }
    }
}

/// Row i of the scrambled column: ((7i + 13) mod 40) / 3, row 17 NaN
/// when `nan`.
fn scrambled(nan: bool) -> impl Fn(usize) -> String {
    move |i| {
        if nan && i == 17 {
            "0.0 / 0.0".into()
        } else {
            f64_lit(((i * 7 + 13) % N) as f64 / 3.0)
        }
    }
}

fn panic_msg(p: Box<dyn Any + Send>) -> String {
    p.downcast_ref::<String>()
        .cloned()
        .or_else(|| p.downcast_ref::<&str>().map(|s| s.to_string()))
        .unwrap_or_else(|| "<non-string panic>".into())
}

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
    let (module, var) = name.split_once("::").context("module::var")?;
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name}")
}

/// Deliver one batch of updates and one `before_view`, as the event
/// loop does; `Err` is a panic.
async fn pump(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: &mut GuiW<NoExt>,
    rt: &tokio::runtime::Handle,
    sel_id: graphix_compiler::expr::ExprId,
    sel: &mut String,
) -> Result<Result<(), String>> {
    let tick = tokio::time::sleep(Duration::from_millis(20));
    tokio::select! {
        biased;
        Some(mut batch) = rx.recv() => {
            for event in batch.drain(..) {
                if let GXEvent::Updated(id, v) = event {
                    if id == sel_id {
                        *sel = match &v {
                            Value::String(s) => s.to_string(),
                            v => format!("{v}"),
                        };
                    }
                    let r = tokio::task::block_in_place(|| {
                        catch_unwind(AssertUnwindSafe(|| widget.handle_update(rt, id, &v)))
                    });
                    match r {
                        Ok(r) => {
                            r?;
                        }
                        Err(p) => return Ok(Err(format!("handle_update: {}", panic_msg(p)))),
                    }
                }
            }
        }
        _ = tick => {}
    }
    if let Err(p) = catch_unwind(AssertUnwindSafe(|| widget.before_view())) {
        return Ok(Err(format!("before_view: {}", panic_msg(p))));
    }
    Ok(Ok(()))
}

/// Compile the table, drive it for 3 s, then click row 0 of `v` to
/// learn which row is drawn first. `Ok(Ok(first_row))`, or
/// `Ok(Err(panic))`.
async fn drive(case: &str, code: &str) -> Result<Result<String, String>> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
    let gx: GXHandle<NoExt> = ctx.rt.clone();
    let compiled: CompRes<NoExt> =
        gx.compile(arcstr::literal!("{ mod test; test::result }")).await.context("compile")?;
    let root_id = compiled.exprs[0].id;
    let timeout = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(timeout);
    let root = loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                if let Some(v) = batch.drain(..).find_map(|e| match e {
                    GXEvent::Updated(id, v) if id == root_id => Some(v),
                    _ => None,
                }) {
                    break v;
                }
            }
            _ = &mut timeout => bail!("timeout waiting for the root value"),
        }
    };
    let sel_ref = gx.compile_ref(find_bind_id(&compiled.env, "test::sel")?).await?;
    let mut sel = String::new();
    let rt = tokio::runtime::Handle::current();
    let mut widget = match AssertUnwindSafe(widgets::compile(gx.clone(), root))
        .catch_unwind()
        .await
    {
        Ok(w) => w.context("compile widget")?,
        Err(p) => {
            let m = format!("widgets::compile: {}", panic_msg(p));
            println!("{case}: PANIC in {m}");
            return Ok(Err(m));
        }
    };
    let deadline = tokio::time::Instant::now() + Duration::from_secs(3);
    while tokio::time::Instant::now() < deadline {
        if let Err(m) = pump(&mut rx, &mut widget, &rt, sel_ref.id, &mut sel).await? {
            println!("{case}: PANIC in {m}");
            return Ok(Err(m));
        }
    }
    let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
    widget.on_message(&Message::CellClick(0, arcstr::literal!("v")), &mut shell);
    for m in shell.out.drain(..) {
        if let Message::Call(id, args) = m {
            gx.call(id, args)?;
        }
    }
    let deadline = tokio::time::Instant::now() + Duration::from_millis(300);
    while tokio::time::Instant::now() < deadline {
        if let Err(m) = pump(&mut rx, &mut widget, &rt, sel_ref.id, &mut sel).await? {
            println!("{case}: PANIC in {m}");
            return Ok(Err(m));
        }
    }
    println!("{case}: no panic; the row drawn first is {sel:?}");
    drop(sel_ref);
    drop(ctx);
    Ok(Ok(sel))
}

async fn expect_no_panic(case: &str, code: &str) -> Result<String> {
    match drive(case, code).await? {
        Ok(first) => Ok(first),
        Err(m) => bail!("{case}: the data_table panicked: {m}"),
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_virtual_control() -> Result<()> {
    let first = expect_no_panic("a_virtual_control", &virtual_table(descending(false))).await?;
    assert_eq!(first, "r39/v", "the smallest value, r39 = 1/3, is drawn first");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn b_virtual_nan() -> Result<()> {
    expect_no_panic("b_virtual_nan", &virtual_table(descending(true))).await?;
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn c_subscribed_nan() -> Result<()> {
    expect_no_panic("c_subscribed_nan", &subscribed_table("r201c", scrambled(true))).await?;
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn d_subscribed_control() -> Result<()> {
    let first =
        expect_no_panic("d_subscribed_control", &subscribed_table("r201d", scrambled(false)))
            .await?;
    // (7i + 13) mod 40 = 0 at i = 21.
    assert_eq!(first, "/local/r201d/r21/v", "the smallest value, r21 = 0, is drawn first");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn e_virtual_version_text() -> Result<()> {
    const POOL: [&str; 9] = ["9", "10", "10.0.1", "8", "11", "1.2.3", "2", "100", "9.1.0"];
    let code = virtual_table(|i| format!("\"{}\"", POOL[(i * 7 + 3) % POOL.len()]));
    expect_no_panic("e_virtual_version_text", &code).await?;
    Ok(())
}
