//! gui-chart-14: time-series padding panics on datetimes near chrono's
//! limits.
//!
//! pad_time_range (stdlib/graphix-package-gui/src/widgets/chart/ranges.rs:215)
//! pads the x range with `DateTime +/- TimeDelta`, which chrono implements
//! with expect(), so a padded range that leaves chrono's range panics the
//! draw. Graphix builds such a datetime: `datetime:"+262142-12-31T23:59:59Z"`
//! prints `+262142-12-31 23:59:59 UTC`.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_14.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_14 -- --nocapture
//!
//! Each chart is compiled with `widgets::compile`, laid out at 600x400 and
//! drawn through the iced canvas widget with a headless wgpu renderer.
//!
//! Expected: every chart draws (the padding stops at chrono's range).
//! Observed at c722befe (test profile, the test FAILS):
//!   control, 2026-01-01 .. 2026-01-02: drew, plot x range
//!     Some((1767221280000.0, 1767316320000.0))
//!   one point at +262142-12-31T23:59:59Z: draw PANICKED:
//!     '`DateTime + TimeDelta` overflowed' at .../chart/ranges.rs:224
//!   points at 2026-01-01 and +262142-12-31T23:59:59Z: draw PANICKED:
//!     '`DateTime + TimeDelta` overflowed' at .../chart/ranges.rs:228
//! The chart data is bound with `let` first: an array literal written
//! straight into `line(&[...])` with datetime x values is refused by the
//! checker (a separate problem).

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
use gui::chart::{chart, line};
let w = `Fixed(600.0);
let h = `Fixed(400.0);
let day = [(datetime:"2026-01-01T00:00:00Z", 1.0), (datetime:"2026-01-02T00:00:00Z", 2.0)];
let edge = [(datetime:"+262142-12-31T23:59:59Z", 1.0)];
let span = [(datetime:"2026-01-01T00:00:00Z", 1.0), (datetime:"+262142-12-31T23:59:59Z", 2.0)];
let result = (
  chart(#width: &w, #height: &h, &[line(&day)]),
  chart(#width: &w, #height: &h, &[line(&edge)]),
  chart(#width: &w, #height: &h, &[line(&span)])
)
"#;

#[tokio::test(flavor = "current_thread")]
async fn time_series_padding_at_chrono_limits() -> Result<()> {
    let s = session(CODE).await?;
    let mut renderer = renderer().await;
    let theme = theme();
    install_hook();
    let names = [
        "control, 2026-01-01 .. 2026-01-02",
        "one point at +262142-12-31T23:59:59Z",
        "points at 2026-01-01 and +262142-12-31T23:59:59Z",
    ];
    let mut panicked: Vec<&str> = Vec::new();
    for (w, name) in s.widgets.iter().zip(names) {
        let c = Chart::new(w, &renderer);
        match c.try_draw(&mut renderer, &theme) {
            Ok(()) => eprintln!(
                "{name}: drew, plot x range {:?}",
                c.state().plot_info.get().map(|i| i.x_range)
            ),
            Err(p) => {
                eprintln!("{name}: draw PANICKED: {p}");
                panicked.push(name);
            }
        }
    }
    let _ = panic::take_hook();
    assert!(panicked.is_empty(), "draw panicked for {panicked:?}");
    Ok(())
}
