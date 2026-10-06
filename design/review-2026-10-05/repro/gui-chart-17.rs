//! gui-chart-17: chart mouse handling: a quick re-press after a pan resets
//! the view, a pie captures the wheel although nothing zooms, and every
//! wheel event zooms by the same 1.1 whatever its size.
//!
//! interact.rs:129-147 treats any press within 400 ms of the previous
//! press as a double-click (positions and drags ignored); 111-127 captures
//! the wheel in every mode; handle_scroll (221) uses only the sign of the
//! delta.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_17.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_17 -- --nocapture
//!
//! Each chart is drawn at 600x400 through the iced canvas widget with a
//! headless wgpu renderer; events go through the canvas widget's `update`,
//! i.e. ChartState::handle_event.
//!
//! Expected: a press 250 ms after a pan, 126 px away, starts a second pan;
//! the wheel over a pie is not captured, so an enclosing scrollable
//! scrolls (iced's scrollable returns on a captured event); a small
//! trackpad delta zooms less than a ten-line wheel delta.
//! Observed at c722befe (the test FAILS):
//!   pan: x_view after a 60 px drag Some((-17.43, 92.57)); after a press
//!     250 ms later, 126 px away: None, a drag started: false
//!   pie: wheel over the pie captured: true; x_view None, y_view None
//!   zoom: one Pixels y=0.5 event zooms x by 1.1000
//!   zoom: one Lines y=1 event zooms x by 1.1000
//!   zoom: one Lines y=10 event zooms x by 1.1000
//!   zoom: one Pixels y=280 event zooms x by 1.1000

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
use gui::chart::{chart, line, pie};
let w = `Fixed(600.0);
let h = `Fixed(400.0);
let result = (
  chart(#width: &w, #height: &h, &[line(&[(0.0, 0.0), (50.0, 1.0), (100.0, 0.5)])]),
  chart(#width: &w, #height: &h, &[pie(&[("a", 1.0), ("b", 2.0)])])
)
"#;

fn press() -> Event {
    Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
}

fn release() -> Event {
    Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))
}

fn wheel(delta: mouse::ScrollDelta) -> Event {
    Event::Mouse(mouse::Event::WheelScrolled { delta })
}

fn center(c: &Chart) -> (Point, (f64, f64)) {
    let info = c.state().plot_info.get().expect("plot info");
    let r = info.rect;
    (Point::new(r.x + r.width / 2.0, r.y + r.height / 2.0), info.x_range)
}

#[tokio::test(flavor = "current_thread")]
async fn chart_mouse_handling() -> Result<()> {
    let s = session(CODE).await?;
    let mut renderer = renderer().await;
    let theme = theme();
    let mut failures: Vec<String> = Vec::new();

    {
        let mut c = Chart::new(&s.widgets[0], &renderer);
        c.draw(&mut renderer, &theme);
        let (p0, _) = center(&c);
        let p1 = Point::new(p0.x + 60.0, p0.y);
        let p2 = Point::new(p0.x - 120.0, p0.y + 40.0);
        c.event(&renderer, press(), p0);
        c.event(&renderer, Event::Mouse(mouse::Event::CursorMoved { position: p1 }), p1);
        c.event(&renderer, release(), p1);
        let panned = c.state().x_view;
        std::thread::sleep(Duration::from_millis(250));
        c.event(&renderer, press(), p2);
        let after = c.state().x_view;
        let dragging = c.state().drag_origin.is_some();
        eprintln!(
            "pan: x_view after a 60 px drag {panned:?}; after a press 250 ms later, \
             126 px away: {after:?}, a drag started: {dragging}"
        );
        if after != panned || !dragging {
            failures.push(
                "a press 250 ms after a pan, elsewhere, reset the view instead of starting a drag"
                    .into(),
            );
        }
    }

    {
        let mut c = Chart::new(&s.widgets[1], &renderer);
        c.draw(&mut renderer, &theme);
        let (p, _) = center(&c);
        let captured =
            c.event(&renderer, wheel(mouse::ScrollDelta::Lines { x: 0.0, y: 1.0 }), p);
        eprintln!(
            "pie: wheel over the pie captured: {captured}; x_view {:?}, y_view {:?}",
            c.state().x_view,
            c.state().y_view
        );
        if captured {
            failures.push("a pie captures the wheel although it does not zoom".into());
        }
    }

    let mut ratios: Vec<f64> = Vec::new();
    for (name, delta) in [
        ("Pixels y=0.5", mouse::ScrollDelta::Pixels { x: 0.0, y: 0.5 }),
        ("Lines y=1", mouse::ScrollDelta::Lines { x: 0.0, y: 1.0 }),
        ("Lines y=10", mouse::ScrollDelta::Lines { x: 0.0, y: 10.0 }),
        ("Pixels y=280", mouse::ScrollDelta::Pixels { x: 0.0, y: 280.0 }),
    ] {
        let mut c = Chart::new(&s.widgets[0], &renderer);
        c.draw(&mut renderer, &theme);
        let (p, (x0, x1)) = center(&c);
        c.event(&renderer, wheel(delta), p);
        let (a, b) = c.state().x_view.expect("the wheel zoomed");
        let ratio = (x1 - x0) / (b - a);
        eprintln!("zoom: one {name} event zooms x by {ratio:.4}");
        ratios.push(ratio);
    }
    if ratios.windows(2).all(|w| (w[0] - w[1]).abs() < 1e-9) {
        failures.push(format!("every wheel event zooms by the same factor: {ratios:?}"));
    }

    for f in failures.iter() {
        eprintln!("FAIL: {f}");
    }
    assert!(failures.is_empty(), "{} failure(s), see above", failures.len());
    Ok(())
}
