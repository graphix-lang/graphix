//! gui-chart-15: a pie chart with a title that is shorter than the title
//! underflows `h - title_h` (u32) in the pie layout.
//!
//! draw.rs:733-743: title_h is the estimated title height plus the margin
//! (29 px at the defaults) and the radius is `w.min(h - title_h)`.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_15.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_15 -- --nocapture
//!
//! Each chart is compiled with `widgets::compile`, laid out in a 600x400
//! window and drawn through the iced canvas widget with a headless wgpu
//! renderer.
//!
//! Expected: every chart draws.
//! Observed at c722befe (test profile, overflow checks on; the test FAILS):
//!   title, 600x400: drew, pie box Some(Rectangle { x: 170.15, y: 84.149994,
//!     width: 259.7, height: 259.7 })
//!   title, 600x20: draw PANICKED: 'attempt to subtract with overflow' at
//!     stdlib/graphix-package-gui/src/widgets/chart/draw.rs:743
//!   no title, 600x20: drew, pie box Some(Rectangle { x: 290.0, y: 0.0,
//!     width: 20.0, height: 20.0 })
//! A release build wraps instead: the radius comes from the width and the
//! centre ((h + title_h) / 2 = 24) lies below the 20 px frame.

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
use gui::chart::{chart, pie};
let d = [("a", 1.0), ("b", 2.0)];
let w = `Fixed(600.0);
let result = (
  chart(#title: &"Sales", #width: &w, #height: &`Fixed(400.0), &[pie(&d)]),
  chart(#title: &"Sales", #width: &w, #height: &`Fixed(20.0), &[pie(&d)]),
  chart(#width: &w, #height: &`Fixed(20.0), &[pie(&d)])
)
"#;

#[tokio::test(flavor = "current_thread")]
async fn short_pie_with_a_title() -> Result<()> {
    let s = session(CODE).await?;
    let mut renderer = renderer().await;
    let theme = theme();
    install_hook();
    let names = ["title, 600x400", "title, 600x20", "no title, 600x20"];
    let mut panicked: Vec<&str> = Vec::new();
    for (w, name) in s.widgets.iter().zip(names) {
        let c = Chart::new(w, &renderer);
        match c.try_draw(&mut renderer, &theme) {
            Ok(()) => eprintln!(
                "{name}: drew, pie box {:?}",
                c.state().plot_info.get().map(|i| i.rect)
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
