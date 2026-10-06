//! gui-chart-20: chart colours drop alpha; scatter #stroke_width and
//! error_bar #point_size do nothing.
//!
//! ChartColor::to_plotters_rgb (stdlib/graphix-package-gui/src/widgets/
//! chart/types.rs:15) builds an RGBColor from r, g, b; the IcedBackend
//! honours alpha (an area fill made with color.mix(0.3) is pale). Scatter
//! draws filled circles of point_size; error_bar sizes its whiskers and its
//! average marker from stroke_width.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_20.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_20 -- --nocapture
//!
//! Each chart (x and y ranges 0..10) is drawn at 600x400 through the iced
//! canvas widget with a headless wgpu renderer and read back with
//! Renderer::screenshot.
//!
//! Expected: a red line with #a: 0.3 over white is pale; each parameter the
//! API takes changes the drawing.
//! Observed at c722befe (the test FAILS):
//!   control: area(#color: red) fill (color.mix(0.3)) at data (5, 2.5):
//!     (247, 208, 208)
//!   line(#color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 0.3)$,
//!     #stroke_width: 8.0) over white: reddest pixel across the line
//!     (255, 0, 0)
//!   control: line #stroke_width 1.0 vs 12.0: 4261 pixels differ
//!   control: error_bar default vs #stroke_width: 6.0: 720 pixels differ
//!   scatter #stroke_width 1.0 vs 12.0: 0 pixels differ
//!   error_bar default vs #point_size: 12.0: 0 pixels differ

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, IcedElement, Message, chart::ChartState},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Color, Event, Font, Layout, Pixels, Point, Rectangle, Renderer as _, Shell, Size,
    clipboard, layout, mouse, renderer::Style, widget::Tree,
};
use iced_wgpu::{
    graphics::{Shell as GfxShell, Viewport},
    wgpu,
};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{
    panic::{self, AssertUnwindSafe},
    sync::Mutex,
    time::Duration,
};
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const SIZE: Size = Size::new(600.0, 400.0);

static LAST_PANIC: Mutex<Option<String>> = Mutex::new(None);

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
        GfxShell::headless(),
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

/// The compiled module `test` (whose `result` is a tuple of charts) and
/// one widget per tuple element.
struct Session {
    _ctx: TestCtx,
    _rx: Rx,
    _compiled: CompRes<NoExt>,
    widgets: Vec<GuiW<NoExt>>,
}

async fn session(code: &str) -> Result<Session> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
    let parts: Vec<Value> = match first_value(&mut rx, compiled.exprs[0].id).await? {
        Value::Array(a) => a.iter().cloned().collect(),
        v => bail!("expected a tuple, got {v}"),
    };
    let mut ws: Vec<GuiW<NoExt>> = Vec::new();
    for p in parts.into_iter() {
        ws.push(widgets::compile(gx.clone(), p).await.context("widget")?);
    }
    Ok(Session { _ctx: ctx, _rx: rx, _compiled: compiled, widgets: ws })
}

fn theme() -> GraphixTheme {
    GraphixTheme { inner: iced_core::Theme::Light, overrides: None }
}

fn install_hook() {
    panic::set_hook(Box::new(|info| {
        let msg = if let Some(s) = info.payload().downcast_ref::<&str>() {
            s.to_string()
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            s.clone()
        } else {
            "<non-string payload>".to_string()
        };
        let loc = info
            .location()
            .map(|l| format!("{}:{}", l.file(), l.line()))
            .unwrap_or_default();
        *LAST_PANIC.lock().unwrap() = Some(format!("'{msg}' at {loc}"));
    }));
}

/// One chart laid out in a 600x400 window, with its own widget tree
/// (and so its own `ChartState`).
struct Chart<'a> {
    el: IcedElement<'a>,
    tree: Tree,
    node: layout::Node,
}

impl<'a> Chart<'a> {
    fn new(w: &'a GuiW<NoExt>, renderer: &widgets::Renderer) -> Self {
        let mut el = w.view();
        let mut tree = Tree::new(&el);
        let limits = layout::Limits::new(Size::ZERO, SIZE);
        let node = el.as_widget_mut().layout(&mut tree, renderer, &limits);
        Self { el, tree, node }
    }

    fn state(&self) -> &ChartState {
        self.tree.state.downcast_ref::<ChartState>()
    }

    fn draw(&self, renderer: &mut widgets::Renderer, theme: &GraphixTheme) {
        renderer.reset(Rectangle::with_size(SIZE));
        self.el.as_widget().draw(
            &self.tree,
            renderer,
            theme,
            &Style { text_color: Color::BLACK },
            Layout::new(&self.node),
            mouse::Cursor::Unavailable,
            &Rectangle::with_size(SIZE),
        );
    }

    /// Draw, catching a panic; Err carries the panic message and location.
    fn try_draw(
        &self,
        renderer: &mut widgets::Renderer,
        theme: &GraphixTheme,
    ) -> std::result::Result<(), String> {
        *LAST_PANIC.lock().unwrap() = None;
        match panic::catch_unwind(AssertUnwindSafe(|| self.draw(renderer, theme))) {
            Ok(()) => Ok(()),
            Err(_) => Err(LAST_PANIC.lock().unwrap().take().unwrap_or_default()),
        }
    }

    /// Deliver one event through the canvas widget's `update` with the
    /// cursor at `at`; returns whether the event was captured.
    fn event(&mut self, renderer: &widgets::Renderer, ev: Event, at: Point) -> bool {
        let mut msgs: Vec<Message> = Vec::new();
        let mut shell = Shell::new(&mut msgs);
        self.el.as_widget_mut().update(
            &mut self.tree,
            &ev,
            Layout::new(&self.node),
            mouse::Cursor::Available(at),
            renderer,
            &mut clipboard::Null,
            &mut shell,
            &Rectangle::with_size(SIZE),
        );
        shell.is_event_captured()
    }
}

/// The frame drawn since the last `reset`, as RGBA rows of 600 pixels.
fn shot(renderer: &mut widgets::Renderer) -> Vec<u8> {
    renderer.screenshot(&Viewport::with_physical_size(Size::new(600, 400), 1.0), Color::WHITE)
}

fn rgb(px: &[u8], x: f32, y: f32) -> (u8, u8, u8) {
    let (x, y) = (x.round() as usize, y.round() as usize);
    let i = (y * 600 + x) * 4;
    (px[i], px[i + 1], px[i + 2])
}

fn is_white((r, g, b): (u8, u8, u8)) -> bool {
    r > 240 && g > 240 && b > 240
}
const CODE: &str = r#"
use gui::{color, chart::{chart, line, scatter, error_bar, area}};
let faint = color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 0.3)$;
let red = color(#r: 1.0, #g: 0.0, #b: 0.0)$;
let w = `Fixed(600.0);
let h = `Fixed(400.0);
let xr = {min: 0.0, max: 10.0};
let yr = {min: 0.0, max: 10.0};
let flat = [(0.0, 5.0), (10.0, 5.0)];
let pts = [(2.0, 2.0), (5.0, 5.0), (8.0, 8.0)];
let eb = [{x: 2.0, min: 1.0, avg: 2.0, max: 3.0}, {x: 5.0, min: 4.0, avg: 5.0, max: 6.0}];
let result = (
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[line(#color: faint, #stroke_width: 8.0, &flat)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[area(#color: red, &flat)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[scatter(#stroke_width: 1.0, &pts)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[scatter(#stroke_width: 12.0, &pts)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[line(#stroke_width: 1.0, &pts)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[line(#stroke_width: 12.0, &pts)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[error_bar(&eb)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[error_bar(#point_size: 12.0, &eb)]),
  chart(#x_range: &xr, #y_range: &yr, #width: &w, #height: &h, &[error_bar(#stroke_width: 6.0, &eb)])
)
"#;

fn drawn(w: &GuiW<NoExt>, renderer: &mut widgets::Renderer) -> (Vec<u8>, Rectangle) {
    let c = Chart::new(w, renderer);
    c.draw(renderer, &theme());
    let rect = c.state().plot_info.get().expect("plot info").rect;
    (shot(renderer), rect)
}

fn differing(a: &[u8], b: &[u8]) -> usize {
    a.chunks(4).zip(b.chunks(4)).filter(|(p, q)| p != q).count()
}

/// The pixel with the least green + blue in a 13 px column centred on
/// data (5, y) of a 0..10 x 0..10 plot.
fn reddest(px: &[u8], r: Rectangle, y: f32) -> (u8, u8, u8) {
    let (cx, cy) = (r.x + r.width * 0.5, r.y + r.height * (1.0 - y / 10.0));
    let mut best = (255u8, 255u8, 255u8);
    for dy in -6..=6 {
        let p = rgb(px, cx, cy + dy as f32);
        if (p.1 as u32 + p.2 as u32) < (best.1 as u32 + best.2 as u32) {
            best = p;
        }
    }
    best
}

#[tokio::test(flavor = "current_thread")]
async fn chart_style_knobs() -> Result<()> {
    let s = session(CODE).await?;
    let mut renderer = renderer().await;
    let mut failures: Vec<String> = Vec::new();

    {
        let (px, r) = drawn(&s.widgets[1], &mut renderer);
        let p = reddest(&px, r, 2.5);
        eprintln!(
            "control: area(#color: red) fill, drawn by draw.rs with color.mix(0.3), \
             at data (5, 2.5): {p:?}"
        );
        let (px, r) = drawn(&s.widgets[0], &mut renderer);
        let p = reddest(&px, r, 5.0);
        eprintln!(
            "line(#color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 0.3)$, #stroke_width: 8.0) \
             over white: reddest pixel across the line {p:?}"
        );
        if p.1 < 100 && p.2 < 100 {
            failures.push(format!("alpha 0.3 drew an opaque line {p:?}"));
        }
    }

    for (a, b, name, should_differ) in [
        (4, 5, "control: line #stroke_width 1.0 vs 12.0", true),
        (6, 8, "control: error_bar default vs #stroke_width: 6.0", true),
        (2, 3, "scatter #stroke_width 1.0 vs 12.0", false),
        (6, 7, "error_bar default vs #point_size: 12.0", false),
    ] {
        let (pa, _) = drawn(&s.widgets[a], &mut renderer);
        let (pb, _) = drawn(&s.widgets[b], &mut renderer);
        let d = differing(&pa, &pb);
        eprintln!("{name}: {d} pixels differ");
        if !should_differ && d == 0 {
            failures.push(format!("{name}: the parameter changed nothing"));
        }
    }

    for f in failures.iter() {
        eprintln!("FAIL: {f}");
    }
    assert!(failures.is_empty(), "{} failure(s), see above", failures.len());
    Ok(())
}
