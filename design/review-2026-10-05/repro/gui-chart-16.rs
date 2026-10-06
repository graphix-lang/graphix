//! gui-chart-16: a pie whose #colors is shorter than its data stops drawing
//! at the first slice without a colour.
//!
//! draw.rs:748-751 hands the user's colours to plotters' Pie unchanged;
//! plotters 0.3.7 Pie::draw returns LengthMismatch at the first index with
//! no colour, after drawing the slices before it.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_16.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_16 -- --nocapture
//!
//! Data a=1, b=2, c=3. Each pie is drawn at 600x400 through the iced canvas
//! widget with a headless wgpu renderer, read back with
//! Renderer::screenshot, and sampled at 360 points on a circle at 0.6 of
//! the radius.
//!
//! Expected: the whole disc is painted (colours cycle, or the palette fills
//! in), as with the palette.
//! Observed at c722befe (the test FAILS):
//!   pie(&d) (palette): 360 of 360 samples painted, 0 of them red
//!   ERROR chart draw pie: BackendError(FontError(LengthMismatch))
//!   pie(#colors: [red], &d): 60 of 360 samples painted, 60 of them red
//!   ERROR chart draw pie: BackendError(FontError(LengthMismatch))
//!   pie(#colors: [], &d): 0 of 360 samples painted
//! So only slice a (60 degrees) is drawn, and an empty array draws nothing.

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
use gui::{color, chart::{chart, pie}};
let red = color(#r: 1.0, #g: 0.0, #b: 0.0)$;
let d = [("a", 1.0), ("b", 2.0), ("c", 3.0)];
let w = `Fixed(600.0);
let h = `Fixed(400.0);
let result = (
  chart(#width: &w, #height: &h, &[pie(&d)]),
  chart(#width: &w, #height: &h, &[pie(#colors: [red], &d)]),
  chart(#width: &w, #height: &h, &[pie(#colors: [], &d)])
)
"#;

#[tokio::test(flavor = "current_thread")]
async fn pie_colors_shorter_than_the_data() -> Result<()> {
    let s = session(CODE).await?;
    let mut renderer = renderer().await;
    let theme = theme();
    let names = ["pie(&d) (palette)", "pie(#colors: [red], &d)", "pie(#colors: [], &d)"];
    let mut painted: Vec<usize> = Vec::new();
    for (w, name) in s.widgets.iter().zip(names) {
        let c = Chart::new(w, &renderer);
        c.draw(&mut renderer, &theme);
        let px = shot(&mut renderer);
        let rect = c.state().plot_info.get().expect("pie plot info").rect;
        let (cx, cy) = (rect.x + rect.width / 2.0, rect.y + rect.height / 2.0);
        let r = rect.width / 2.0;
        let (mut filled, mut red) = (0, 0);
        for k in 0..360 {
            let a = (k as f32).to_radians();
            let p = rgb(&px, cx + 0.6 * r * a.cos(), cy + 0.6 * r * a.sin());
            if !is_white(p) {
                filled += 1;
            }
            if p.0 > 200 && p.1 < 60 && p.2 < 60 {
                red += 1;
            }
        }
        eprintln!(
            "{name}: {filled} of 360 samples on a circle at 0.6 radius are painted, {red} of them red"
        );
        painted.push(filled);
    }
    assert!(
        painted.iter().all(|&f| f > 350),
        "a pie left part of its disc unpainted: {painted:?}"
    );
    Ok(())
}
