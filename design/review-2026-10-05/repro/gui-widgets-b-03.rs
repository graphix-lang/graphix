//! gui-widgets-b-03: `progress_bar` with min > max, or a NaN bound,
//! panics in `ProgressBarW::view` (src/widgets/progress_bar.rs:71).
//! iced_widget 0.14.2's `ProgressBar::new` clamps the value with
//! `f32::clamp` (progress_bar.rs:81), which asserts `min <= max` and
//! that neither bound is NaN. In the shell `view` runs on the main thread
//! (event_loop.rs:318 inside `about_to_wait`; the closure is run by
//! graphix-shell/src/main.rs:480-482 with no catch), so the GUI process
//! dies. `slider` takes the same range without a panic: iced's
//! `Slider::new` clamps by hand.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_03.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_03 -- --nocapture
//!
//! Each case compiles the widget as the shell does (`widgets::compile`),
//! calls `view()` as a frame does, feeds every runtime update to
//! `handle_update` for a while, and calls `view()` again.
//!
//! Expected: every case renders (`view()` returns).
//! Observed (HEAD c722befe): a_min_above_max, b_nan_max,
//! c_empty_items_max and d_items_emptied_at_runtime FAIL with
//! "min > max, or either was NaN. min = .., max = .." (d renders its
//! first frame, max = 2, and panics on the frame after the timer empties
//! `items`); RUST_BACKTRACE=1 shows `iced_widget::progress_bar::
//! ProgressBar::new` <- `ProgressBarW::view`. e_slider_control passes.
//! In the shell the same panic escapes `run_app` and `main` (winit on
//! Linux catches nothing): the reviewer's run of
//! `[&window(&progress_bar(#min: &100.0, #max: &0.0, &50.0))]` with a
//! display ended in a segfault, exit 139.

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW};
use graphix_rt::{CompRes, GXEvent, NoExt};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{
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

struct H {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let code = format!(
            "use gui::{{progress_bar::progress_bar, slider::slider}};\n{code}"
        );
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled: CompRes<NoExt> = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root_id = compiled.exprs[0].id;
        let root = tokio::time::timeout(Duration::from_secs(30), async {
            loop {
                let mut batch = rx.recv().await.context("channel closed")?;
                if let Some(v) = batch.drain(..).find_map(|e| match e {
                    GXEvent::Updated(id, v) if id == root_id => Some(v),
                    _ => None,
                }) {
                    return Ok::<Value, anyhow::Error>(v);
                }
            }
        })
        .await
        .context("timeout waiting for the root value")??;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        Ok(Self { _ctx: ctx, _compiled: compiled, rx, widget })
    }

    /// Deliver every runtime update for `d` as the event loop does.
    async fn pump(&mut self, d: Duration) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + d;
        while let Ok(Some(mut batch)) =
            tokio::time::timeout_at(deadline, self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(id, v) = e {
                    self.widget.handle_update(&rt, id, &v)?;
                }
            }
        }
        Ok(())
    }

    /// `view()` as a frame calls it; the panic message if it panics.
    fn view(&self) -> Option<String> {
        catch_unwind(AssertUnwindSafe(|| {
            let _ = self.widget.view();
        }))
        .err()
        .map(|p| {
            p.downcast_ref::<String>()
                .cloned()
                .or_else(|| p.downcast_ref::<&str>().map(|s| s.to_string()))
                .unwrap_or_default()
        })
    }
}

async fn case(name: &str, code: &str) -> Result<()> {
    let mut h = H::new(code).await?;
    let first = h.view();
    h.pump(Duration::from_millis(1000)).await?;
    let after = h.view();
    println!("{name}: first frame panic: {first:?}; after updates panic: {after:?}");
    assert!(
        first.is_none() && after.is_none(),
        "{name}: view() panicked: {:?}",
        first.or(after)
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_min_above_max() -> Result<()> {
    case(
        "a_min_above_max",
        "let result = progress_bar(#min: &100.0, #max: &0.0, &50.0)",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn b_nan_max() -> Result<()> {
    case("b_nan_max", "let z = 0.0;\nlet result = progress_bar(#max: &(z / z), &50.0)")
        .await
}

#[tokio::test(flavor = "current_thread")]
async fn c_empty_items_max() -> Result<()> {
    case(
        "c_empty_items_max",
        "let items: Array<string> = [];\nlet i = 0.0;\n\
         let result = progress_bar(#max: &(cast<f64>(array::len(items))$ - 1.0), &i)",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn d_items_emptied_at_runtime() -> Result<()> {
    case(
        "d_items_emptied_at_runtime",
        "let items = [\"a\", \"b\", \"c\"];\n\
         let t = sys::time::timer(duration:300.ms, false);\n\
         items <- t ~ [];\n\
         let result = progress_bar(#max: &(cast<f64>(array::len(items))$ - 1.0), &0.0)",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn e_slider_control() -> Result<()> {
    case("e_slider_control", "let result = slider(#min: &100.0, #max: &0.0, &50.0)").await
}
