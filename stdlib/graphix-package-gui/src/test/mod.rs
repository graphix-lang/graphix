use anyhow::{bail, Context, Result};
use graphix_compiler::expr::{ExprId, ModuleResolver};
use graphix_package_core::testing::{self, RegisterFn, TestCtx};
use graphix_rt::{GXEvent, NoExt};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

use crate::widgets::{self, GuiW};

mod widgets_test;

const TEST_REGISTER: &[RegisterFn] = &[
    <graphix_package_core::P as graphix_package::Package<NoExt>>::register,
    <crate::P as graphix_package::Package<NoExt>>::register,
];

/// Test harness for GUI widget integration tests.
///
/// Compiles graphix code that produces a Widget value, builds the
/// widget tree, and provides helpers for simulating interactions
/// through the reactive loop.
struct GuiTestHarness {
    _ctx: TestCtx,
    _gx: graphix_rt::GXHandle<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt_handle: tokio::runtime::Handle,
}

impl GuiTestHarness {
    /// Compile graphix code that produces a Widget value.
    ///
    /// `code` is module-level graphix code. The last binding should be
    /// named `result` and evaluate to a Widget value.
    /// Example: `"use gui; let result = gui::text(content: &\"hello\")"`.
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = fxhash::FxHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            arcstr::ArcStr::from(code),
        )]);
        let resolver = ModuleResolver::VFS(tbl);
        let ctx =
            testing::init_with_resolvers(tx, TEST_REGISTER, vec![resolver]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let expr_id = compiled.exprs[0].id;

        // Wait for the initial value
        let initial_value = wait_for_update(&mut rx, expr_id).await?;

        // Compile the widget value into a widget tree
        let widget = widgets::compile(gx.clone(), initial_value)
            .await
            .context("compile widget tree")?;

        let rt_handle = tokio::runtime::Handle::current();

        // Drain any additional updates that arrive during widget compilation
        while rx.try_recv().is_ok() {}

        Ok(Self { _ctx: ctx, _gx: gx, rx, widget, rt_handle })
    }

    /// Drain all pending reactive updates into the widget tree.
    /// Returns true if any updates were processed.
    async fn drain(&mut self) -> Result<bool> {
        let mut changed = false;
        let timeout = tokio::time::sleep(Duration::from_millis(100));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            changed |= self.widget.handle_update(
                                &self.rt_handle, id, &v
                            )?;
                        }
                    }
                    // Reset timeout after each batch
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(50)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(changed)
    }

    /// Call view() on the widget. Panicking here means the widget
    /// tree is in an inconsistent state.
    fn view(&self) -> crate::widgets::IcedElement<'_> {
        self.widget.view()
    }
}

/// Wait for a specific expression's update, with timeout.
async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target_id: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == target_id {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for initial widget value"),
        }
    }
}
