//! gui-chart-18: numeric and datetime datasets mix silently, and a real
//! mode conflict logs an error on every event.
//!
//! chart_mode (stdlib/graphix-package-gui/src/widgets/chart/dataset.rs:142)
//! sets one has_other flag for numeric and datetime XY/OHLC/error-bar data,
//! so the first non-empty dataset picks Numeric or TimeSeries and
//! draw_chart_body skips the other kind; a bar/pie/3D/XY conflict logs
//! error! from chart_mode, which every handle_event calls.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_18.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_18 -- --nocapture
//!
//! Each chart is drawn at 600x400 through the iced canvas widget with a
//! headless wgpu renderer and read back with Renderer::screenshot; a
//! logger counts error! records.
//!
//! Expected: both series drawn, or the mix reported; a conflict reported
//! once, not per event.
//! Observed at c722befe (the test FAILS):
//!   line(numeric, blue) then line(datetime, red): 2346 blue px, 0 red px,
//!     0 error(s) logged
//!   line(datetime, red) then line(numeric, blue): 0 blue px, 2336 red px,
//!     0 error(s) logged
//!   bar + line: 1 'cannot mix' error(s) during the draw, 50 over 50 cursor
//!     moves

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
use gui::{color, chart::{chart, line, bar}};
let red = color(#r: 1.0, #g: 0.0, #b: 0.0)$;
let blue = color(#r: 0.0, #g: 0.0, #b: 1.0)$;
let nums = [(0.0, 0.0), (10.0, 10.0)];
let times = [(datetime:"2026-01-01T00:00:00Z", 0.0), (datetime:"2026-01-02T00:00:00Z", 10.0)];
let w = `Fixed(600.0);
let h = `Fixed(400.0);
let result = (
  chart(#width: &w, #height: &h, &[line(#color: blue, #stroke_width: 4.0, &nums), line(#color: red, #stroke_width: 4.0, &times)]),
  chart(#width: &w, #height: &h, &[line(#color: red, #stroke_width: 4.0, &times), line(#color: blue, #stroke_width: 4.0, &nums)]),
  chart(#width: &w, #height: &h, &[bar(&[("a", 1.0), ("b", 2.0)]), line(&nums)])
)
"#;

static ERRORS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
static MIX: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

struct Counter;

impl log::Log for Counter {
    fn enabled(&self, m: &log::Metadata) -> bool {
        m.level() <= log::Level::Error
    }
    fn log(&self, r: &log::Record) {
        use std::sync::atomic::Ordering::Relaxed;
        if r.level() == log::Level::Error {
            ERRORS.fetch_add(1, Relaxed);
            if format!("{}", r.args()).contains("cannot mix") {
                MIX.fetch_add(1, Relaxed);
            }
        }
    }
    fn flush(&self) {}
}

static COUNTER: Counter = Counter;

fn count(px: &[u8], f: impl Fn((u8, u8, u8)) -> bool) -> usize {
    px.chunks(4).filter(|p| f((p[0], p[1], p[2]))).count()
}

#[tokio::test(flavor = "current_thread")]
async fn mixed_chart_modes() -> Result<()> {
    use std::sync::atomic::Ordering::Relaxed;
    log::set_logger(&COUNTER).expect("the first logger");
    log::set_max_level(log::LevelFilter::Error);
    let s = session(CODE).await?;
    let mut renderer = renderer().await;
    let theme = theme();
    let mut failures: Vec<String> = Vec::new();

    for (i, name) in [
        (0, "line(numeric, blue) then line(datetime, red)"),
        (1, "line(datetime, red) then line(numeric, blue)"),
    ] {
        let e0 = ERRORS.load(Relaxed);
        let c = Chart::new(&s.widgets[i], &renderer);
        c.draw(&mut renderer, &theme);
        let px = shot(&mut renderer);
        let blue = count(&px, |(r, g, b)| b > 200 && r < 60 && g < 60);
        let red = count(&px, |(r, g, b)| r > 200 && g < 60 && b < 60);
        let errs = ERRORS.load(Relaxed) - e0;
        eprintln!("{name}: {blue} blue px, {red} red px, {errs} error(s) logged");
        if blue == 0 || red == 0 {
            failures.push(format!(
                "{name}: one series was not drawn (blue {blue}, red {red}) and {errs} error(s) were logged"
            ));
        }
    }

    {
        let mut c = Chart::new(&s.widgets[2], &renderer);
        let m0 = MIX.load(Relaxed);
        c.draw(&mut renderer, &theme);
        let m1 = MIX.load(Relaxed);
        for k in 0..50 {
            let p = Point::new(100.0 + k as f32 * 5.0, 200.0);
            c.event(&renderer, Event::Mouse(mouse::Event::CursorMoved { position: p }), p);
        }
        let m2 = MIX.load(Relaxed);
        eprintln!(
            "bar + line: {} 'cannot mix' error(s) during the draw, {} over 50 cursor moves",
            m1 - m0,
            m2 - m1
        );
        if m2 - m1 >= 50 {
            failures.push(format!("{} 'cannot mix' errors for 50 cursor moves", m2 - m1));
        }
    }

    for f in failures.iter() {
        eprintln!("FAIL: {f}");
    }
    assert!(failures.is_empty(), "{} failure(s), see above", failures.len());
    Ok(())
}
