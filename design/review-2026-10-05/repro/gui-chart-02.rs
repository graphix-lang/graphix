//! gui-chart-02: chart series are handed to plotters unclipped, so points
//! outside the view are clamped onto the plot border, and points far
//! outside it overflow plotters' i32 pixel mapping (a panic in debug
//! builds).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_02.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_02 -- --nocapture
//!
//! Every case builds a chart with `widgets::compile`, lays it out at
//! 600x400 and draws it through the iced canvas widget (draw.rs) with a
//! headless wgpu renderer; wheel events go through the canvas widget's
//! `update`, i.e. `ChartState::handle_event`.
//!
//! clamp:      #x_range {0,10}, #y_range {0,10}; a blue line
//!             (0,0)->(1000,500) and red scatter points (2,8), (5,50).
//!             The frame is read back with Renderer::screenshot.
//! xrange:     #x_range {0, 1e-5} over a line reaching x = 100.
//! yrange:     #y_range {0, 1} over a line with one sample at -1e7.
//! zoom:       auto range over x in 0..100; wheel-zoom in (one line per
//!             event) at 10% from the left of the plot, drawing after
//!             every event.
//! zoom_time:  the same over a time series two hours long.
//!
//! Expected: in `clamp` the line leaves the view at its true exit point
//! (10, 5): at data x = 5 it is 75% down the plot, and nothing is drawn
//! for (5, 50); no case panics.
//! Observed at c722befe (dev/test profile, the test FAILS):
//!   clamp: plot rect x=51 y=10 w=539 h=358, view x=(0.0, 10.0) y=(0.0, 10.0)
//!   clamp: line at data x=5.0, blue rows as a fraction of the plot height
//!     from the top: [0.494, 0.497] (true line y=2.5 -> 0.75)
//!   clamp: line at data x=9.9, blue rows: [0.006, 0.008]
//!     (true line y=4.95 -> 0.505)
//!   clamp: red pixels around scatter (2,8) [in view]: 38; around
//!     (x=5, top edge) where (5,50) [out of view] would be clamped: 52
//!   xrange {0,1e-5}, data to x=100: draw PANICKED: 'attempt to add with
//!     overflow' at plotters-0.3.7/src/coord/ranged1d/types/numeric.rs:271
//!   yrange {0,1}, sample at -1e7: draw PANICKED: same, numeric.rs:271
//!   zoom (numeric, x 0..100): draw PANICKED after wheel event 162:
//!     numeric.rs:271; x_view span 2.17e-5
//!   zoom_time (2 h time series): draw PANICKED after wheel event 160:
//!     'attempt to add with overflow' at .../types/datetime.rs:38;
//!     x_view = (1704067535138.0022, 1704067535139.8894) ms, which the
//!     draw truncates to a 1 ms window (draw.rs:551-556)
//! A release build has no overflow checks: the add wraps to about
//! -2^31 and Rect::truncate then draws the point on the opposite edge.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing;
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, IcedElement, Message, chart::ChartState},
};
use graphix_rt::{GXEvent, NoExt};
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

const CODE: &str = r#"use gui::{color, chart::{chart, line, scatter}};
let red = color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$;
let blue = color(#r: 0.0, #g: 0.0, #b: 1.0, #a: 1.0)$;
let w = `Fixed(600.0);
let h = `Fixed(400.0);
let ts = [(datetime:"2024-01-01T00:00:00Z", 1.0), (datetime:"2024-01-01T02:00:00Z", 2.0)];
let result = (
  chart(
    #x_range: &{min: 0.0, max: 10.0},
    #y_range: &{min: 0.0, max: 10.0},
    #width: &w,
    #height: &h,
    &[
      line(#color: blue, &[(0.0, 0.0), (1000.0, 500.0)]),
      scatter(#color: red, #point_size: 4.0, &[(2.0, 8.0), (5.0, 50.0)])
    ]
  ),
  chart(#x_range: &{min: 0.0, max: 0.00001}, #width: &w, #height: &h, &[line(&[(0.0, 0.0), (100.0, 1.0)])]),
  chart(
    #y_range: &{min: 0.0, max: 1.0},
    #width: &w,
    #height: &h,
    &[line(&[(0.0, 0.5), (1.0, 0.25), (2.0, -10000000.0), (3.0, 0.75)])]
  ),
  chart(#width: &w, #height: &h, &[line(&[(0.0, 0.0), (50.0, 1.0), (100.0, 0.5)])]),
  chart(#width: &w, #height: &h, &[line(&ts)])
)"#;

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

    fn wheel_in(&mut self, renderer: &widgets::Renderer, at: Point) {
        let mut msgs: Vec<Message> = Vec::new();
        let mut shell = Shell::new(&mut msgs);
        let ev = Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Lines { x: 0.0, y: 1.0 },
        });
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
    }
}

struct Shot {
    px: Vec<u8>,
    w: usize,
}

impl Shot {
    fn rgb(&self, x: usize, y: usize) -> (u8, u8, u8) {
        let i = (y * self.w + x) * 4;
        (self.px[i], self.px[i + 1], self.px[i + 2])
    }
    fn is_blue(&self, x: usize, y: usize) -> bool {
        let (r, g, b) = self.rgb(x, y);
        b > 150 && r < 100 && g < 100
    }
    fn is_red(&self, x: usize, y: usize) -> bool {
        let (r, g, b) = self.rgb(x, y);
        r > 150 && g < 100 && b < 100
    }
    fn red_near(&self, cx: f32, cy: f32, rad: i32) -> usize {
        let (cx, cy) = (cx.round() as i32, cy.round() as i32);
        let mut n = 0;
        for y in (cy - rad)..=(cy + rad) {
            for x in (cx - rad)..=(cx + rad) {
                if x >= 0 && y >= 0 && (x as usize) < self.w && (y as usize) < 400 {
                    if self.is_red(x as usize, y as usize) {
                        n += 1;
                    }
                }
            }
        }
        n
    }
}

/// Zoom in by wheel at 10% from the left and half way down the plot,
/// drawing after each event, until a draw panics or `max` events.
fn zoom_until_panic(
    w: &GuiW<NoExt>,
    renderer: &mut widgets::Renderer,
    theme: &GraphixTheme,
    max: usize,
) -> Option<(usize, String, Option<(f64, f64)>, Option<(f64, f64)>)> {
    let mut c = Chart::new(w, renderer);
    c.try_draw(renderer, theme).expect("the default view draws");
    let rect = c.state().plot_info.get().expect("plot info").rect;
    let at = Point::new(rect.x + 0.1 * rect.width, rect.y + 0.5 * rect.height);
    for step in 1..=max {
        c.wheel_in(renderer, at);
        if let Err(p) = c.try_draw(renderer, theme) {
            return Some((step, p, c.state().x_view, c.state().y_view));
        }
    }
    None
}

#[tokio::test(flavor = "current_thread")]
async fn chart_points_outside_the_view() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(CODE)),
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
    let mut renderer = renderer().await;
    let theme = GraphixTheme { inner: iced_core::Theme::Light, overrides: None };
    install_hook();
    let mut failures: Vec<String> = Vec::new();

    // clamp
    {
        let c = Chart::new(&ws[0], &renderer);
        c.try_draw(&mut renderer, &theme).expect("clamp draws");
        let info = c.state().plot_info.get().expect("plot info");
        let r = info.rect;
        eprintln!(
            "clamp: plot rect x={} y={} w={} h={}, view x={:?} y={:?}",
            r.x, r.y, r.width, r.height, info.x_range, info.y_range
        );
        let shot = Shot {
            px: renderer.screenshot(
                &Viewport::with_physical_size(Size::new(600, 400), 1.0),
                Color::WHITE,
            ),
            w: 600,
        };
        let fracs = |col: f32| -> Vec<f32> {
            let x = col.round() as usize;
            (0..400usize)
                .filter(|&y| shot.is_blue(x, y))
                .map(|y| ((y as f32 - r.y) / r.height * 1000.0).round() / 1000.0)
                .collect()
        };
        let mid = fracs(r.x + 0.5 * r.width);
        let edge = fracs(r.x + 0.99 * r.width);
        eprintln!(
            "clamp: line at data x=5.0, blue rows as a fraction of the plot height \
             from the top: {mid:?} (true line y=2.5 -> 0.75)"
        );
        eprintln!(
            "clamp: line at data x=9.9, blue rows: {edge:?} (true line y=4.95 -> 0.505)"
        );
        let control = shot.red_near(r.x + 0.2 * r.width, r.y + 0.2 * r.height, 3);
        let top = shot.red_near(r.x + 0.5 * r.width, r.y, 6);
        eprintln!(
            "clamp: red pixels around scatter (2,8) [in view]: {control}; around \
             (x=5, top edge) where (5,50) [out of view] would be clamped: {top}"
        );
        if control == 0 {
            failures.push("clamp control: in-view scatter point not found".into());
        }
        if mid.is_empty() || !mid.iter().all(|f| (f - 0.75).abs() < 0.03) {
            failures.push(format!("clamp: line drawn at {mid:?} of the height at x=5, not 0.75"));
        }
        if top > 0 {
            failures.push(format!(
                "clamp: out-of-view scatter point (5,50) drawn on the top border ({top} px)"
            ));
        }
    }

    // explicit ranges
    for (i, name) in [(1usize, "xrange {0,1e-5}, data to x=100"), (2, "yrange {0,1}, sample at -1e7")] {
        let c = Chart::new(&ws[i], &renderer);
        match c.try_draw(&mut renderer, &theme) {
            Ok(()) => eprintln!("{name}: drew"),
            Err(p) => {
                eprintln!("{name}: draw PANICKED: {p}");
                failures.push(format!("{name}: draw panicked: {p}"));
            }
        }
    }

    // wheel zoom
    for (i, name) in [(3usize, "zoom (numeric, x 0..100)"), (4, "zoom_time (2 h time series)")] {
        match zoom_until_panic(&ws[i], &mut renderer, &theme, 250) {
            None => eprintln!("{name}: 250 wheel events, every draw ok"),
            Some((step, p, xv, yv)) => {
                let span = xv.map(|(a, b)| b - a);
                eprintln!(
                    "{name}: draw PANICKED after wheel event {step}: {p}; \
                     x_view={xv:?} (span {span:?}) y_view={yv:?}"
                );
                failures.push(format!("{name}: panicked after {step} wheel events: {p}"));
            }
        }
    }

    let _ = panic::take_hook();
    for f in failures.iter() {
        eprintln!("FAIL: {f}");
    }
    assert!(failures.is_empty(), "{} failure(s), see above", failures.len());
    Ok(())
}
