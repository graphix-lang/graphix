//! gui-chart-02: a chart `Children::update` keeps when it moves to
//! another chart's position inherits that chart's `ChartState`, whose
//! geometry cache is filled and whose `dirty` the moved chart does not
//! set, so it draws the other chart's picture and keeps its `plot_info`.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_02.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_02 -- --nocapture
//!
//! Each frame diffs one iced `Tree` against the column's new view, lays
//! it out and draws it, as iced's `UserInterface` does, then reads each
//! `ChartState`'s `plot_info` (set only when the plot is drawn, not when
//! the cache answers). `tall`'s data is y 1000..2000, `short`'s 1..2.
//! Expected: after the list changes, each chart's y range is its own.
//! Observed at 7324207f: both tests fail.
//!   kept_chart_moved_down_a_slot_redraws (fixed 200px heights):
//!     before: [Some((950.0, 2050.0)), Some((0.95, 2.05))]
//!     after [short]: [Some((950.0, 2050.0))]          <- short shows tall
//!   kept_charts_swapped_redraw (default Fill heights):
//!     before: [Some((950.0, 2050.0)), Some((0.95, 2.05))]
//!     after swap [short, tall]: [Some((950.0, 2050.0)), Some((0.95, 2.05))]
//! With default `Fill` heights the drop case does not show it: the one
//! chart left is laid out taller and `Cache::draw` misses on the size.

use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, chart::ChartState},
};
use graphix_rt::{GXEvent, NoExt};
use iced_core::{
    Color, Font, Pixels, Rectangle, Size,
    layout::{Layout, Limits},
    mouse,
    renderer::Style,
    widget::{Tree, tree::State},
};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const VIEWPORT: Size = Size::new(400.0, 600.0);

type Rx = mpsc::Receiver<GPooled<Vec<GXEvent>>>;

async fn renderer() -> widgets::Renderer {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY),
        ..Default::default()
    });
    let adapter = match instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: None,
            force_fallback_adapter: false,
            ..Default::default()
        })
        .await
    {
        Ok(a) => a,
        Err(_) => instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                compatible_surface: None,
                force_fallback_adapter: true,
                ..Default::default()
            })
            .await
            .expect("no GPU adapter"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("GPU device");
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, Font::DEFAULT, Pixels(16.0))
}

async fn first_value(rx: &mut Rx, target: ExprId) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(10));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev && id == target {
                        return Ok(v);
                    }
                }
            }
            _ = &mut timeout => bail!("no widget value"),
        }
    }
}

struct H {
    _ctx: TestCtx,
    _compiled: graphix_rt::CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    tree: Option<Tree>,
}

fn chart_states<'a>(t: &'a Tree, out: &mut Vec<&'a ChartState>) {
    if let State::Some(b) = &t.state
        && let Some(cs) = b.downcast_ref::<ChartState>()
    {
        out.push(cs)
    }
    for c in &t.children {
        chart_states(c, out)
    }
}

impl H {
    async fn new(code: &str) -> Result<H> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = ahash::AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
        let v = first_value(&mut rx, compiled.exprs[0].id).await?;
        let widget = widgets::compile(gx.clone(), v).await.context("widget")?;
        let mut h = H { _ctx: ctx, _compiled: compiled, rx, widget, renderer: renderer().await, tree: None };
        h.drain_for(Duration::from_millis(150)).await?;
        Ok(h)
    }

    async fn drain_for(&mut self, dur: Duration) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + dur;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            tokio::task::block_in_place(|| self.widget.handle_update(&rt, id, &v))?;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return Ok(()),
            }
        }
    }

    /// One frame: diff, layout, draw; each chart's drawn y range in order.
    fn frame(&mut self) -> Vec<Option<(f64, f64)>> {
        self.widget.before_view();
        let mut element = self.widget.view();
        match self.tree.as_mut() {
            Some(t) => t.diff(element.as_widget()),
            None => self.tree = Some(Tree::new(element.as_widget())),
        }
        let tree = self.tree.as_mut().unwrap();
        let node = element.as_widget_mut().layout(
            tree,
            &self.renderer,
            &Limits::new(Size::ZERO, VIEWPORT),
        );
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        element.as_widget().draw(
            tree,
            &mut self.renderer,
            &theme,
            &Style { text_color: Color::BLACK },
            Layout::new(&node),
            mouse::Cursor::Unavailable,
            &Rectangle::with_size(VIEWPORT),
        );
        let mut states = vec![];
        chart_states(tree, &mut states);
        states.iter().map(|s| s.plot_info.get().map(|p| p.y_range)).collect()
    }
}

/// Two charts of the given height and `go`, true after a second.
fn charts(height: &str) -> String {
    format!(
        r#"use gui::{{chart::{{self, *}}, column::column}};
let tall = chart(#title: &"tall", #height: &{height}, &[chart::line(&[(1.0, 1000.0), (2.0, 2000.0)])]);
let short = chart(#title: &"short", #height: &{height}, &[chart::line(&[(1.0, 1.0), (2.0, 2.0)])]);
let go = false;
go <- sys::time::after_idle(duration:1000.ms, true);
"#
    )
}

fn is_tall(y: Option<(f64, f64)>) -> bool {
    y.is_some_and(|y| y.1 > 1000.0)
}

fn is_short(y: Option<(f64, f64)>) -> bool {
    y.is_some_and(|y| y.1 < 100.0)
}

/// `[tall, short]` becomes `[short]`: `short` is kept and moves to index 0.
/// The charts are of a fixed height: under the default `Fill` the one
/// left is laid out taller, and a cache of other bounds draws again.
#[tokio::test(flavor = "multi_thread")]
async fn kept_chart_moved_down_a_slot_redraws() -> Result<()> {
    let code = format!(
        "{}let kids = select go {{ true => [short], false => [tall, short] }};\n\
         let result = column(&kids)",
        charts("`Fixed(200.0)")
    );
    let mut h = H::new(&code).await?;
    let before = h.frame();
    eprintln!("before: {before:?}");
    assert!(before.len() == 2 && is_tall(before[0]) && is_short(before[1]));
    h.drain_for(Duration::from_millis(1500)).await?;
    let after = h.frame();
    eprintln!("after [short]: {after:?}");
    assert!(after.len() == 1 && is_short(after[0]), "short drew tall's plot: {after:?}");
    Ok(())
}

/// `[tall, short]` becomes `[short, tall]`: both kept, swapped. Default
/// (`Fill`) heights: both slots keep their size.
#[tokio::test(flavor = "multi_thread")]
async fn kept_charts_swapped_redraw() -> Result<()> {
    let code = format!(
        "{}let kids = select go {{ true => [short, tall], false => [tall, short] }};\n\
         let result = column(&kids)",
        charts("`Fill")
    );
    let mut h = H::new(&code).await?;
    let before = h.frame();
    eprintln!("before: {before:?}");
    assert!(before.len() == 2 && is_tall(before[0]) && is_short(before[1]));
    h.drain_for(Duration::from_millis(1500)).await?;
    let after = h.frame();
    eprintln!("after swap [short, tall]: {after:?}");
    assert!(
        after.len() == 2 && is_short(after[0]) && is_tall(after[1]),
        "swapped charts drew each other's plots: {after:?}"
    );
    Ok(())
}
