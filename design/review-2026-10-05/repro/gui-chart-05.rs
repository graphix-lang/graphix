//! gui-chart-05: a chart's zoom/pan view (`ChartState::x_view`/`y_view`,
//! iced's `Program::State`) outlives the chart. iced keeps canvas state
//! by tree position (`Canvas::tag` is the State type, no `diff`), so a new
//! chart compiled into the slot of a panned one is drawn through the old
//! chart's view; `ChartW` resets only the geometry cache (`dirty`). The
//! same view also survives a mode change of one chart (its datasets go
//! from numeric to datetime). A numeric view read as ms since 1970 by a
//! time-series chart maps 2026 samples ~1e10 plot widths away.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_05.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_05 -- --nocapture
//!
//! Each case compiles a Graphix column [switch button, chart], drives it
//! through a headless iced `UserInterface` one frame per event as
//! `GuiHandler::about_to_wait` does (build over the kept cache, update,
//! draw), optionally pans the numeric chart by a mouse drag, clicks
//! "switch" (runs the `Call` on the runtime and applies the updates with
//! `handle_update`), and draws the next frame.
//!   swap_no_pan (control): select swaps a numeric chart for a time-series
//!     chart in the same slot, no pan before the swap.
//!   swap_after_pan: the same, after panning the numeric chart.
//!   mode_change_after_pan: one chart whose datasets go from numeric to
//!     datetime, after panning it.
//!
//! Expected: every draw returns; the time-series chart is drawn over its
//! own data range.
//! Observed at c722befe (dev profile, overflow checks on): the test fails.
//!   swap_no_pan (control): clicked switch, 1 call(s)
//!   swap_no_pan (control): draw after the switch returned
//!   swap_after_pan: panned the numeric chart by (60, 40) px
//!   swap_after_pan: clicked switch, 1 call(s)
//!   swap_after_pan: draw after the switch PANICKED: at
//!     plotters-0.3.7/src/coord/ranged1d/types/datetime.rs:38:24:
//!     attempt to add with overflow
//!   mode_change_after_pan: panned the numeric chart by (60, 40) px
//!   mode_change_after_pan: clicked switch, 1 call(s)
//!   mode_change_after_pan: draw after the switch PANICKED: at
//!     plotters-0.3.7/src/coord/ranged1d/types/datetime.rs:38:24:
//!     attempt to add with overflow
//! The pan left a numeric x_view about 110 wide near (0, 100); the
//! time-series chart reads it as ms since 1970 (draw.rs:548-556), so
//! plotters maps the 2026 samples ~1e12 px right: `as i32` saturates and
//! `+ limit.0` overflows. A release build wraps instead, and
//! `Rect::truncate` clamps every point onto the plot's left edge.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use poolshark::global::GPooled;
use std::{
    panic::{self, AssertUnwindSafe},
    sync::Mutex,
    time::Duration,
};
use tokio::sync::mpsc;

const REG: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const SWAP: &str = r#"
use gui::{chart, column::column, button::button, text::text};
let tab = 0;
let t = datetime:"2026-01-01T00:00:00Z";
let nd = [(0.0, 0.0), (100.0, 1.0)];
let td = [(t, 1.0), (sys::time::add(t, duration:60.s), 2.0)];
let content = select tab {
  0 => chart::chart(&[chart::line(&nd)]),
  _ => chart::chart(&[chart::line(&td)])
};
let result = column(&[button(#on_press: |c| tab <- c ~ (1 - tab), &text(&"switch")), content])
"#;

const MODE: &str = r#"
use gui::{chart, column::column, button::button, text::text};
let tab = 0;
let t = datetime:"2026-01-01T00:00:00Z";
let nd = [(0.0, 0.0), (100.0, 1.0)];
let td = [(t, 1.0), (sys::time::add(t, duration:60.s), 2.0)];
let ds = select tab {
  0 => [chart::line(&nd)],
  _ => [chart::line(&td)]
};
let result = column(&[button(#on_press: |c| tab <- c ~ (1 - tab), &text(&"switch")), chart::chart(&ds)])
"#;

const VIEWPORT: Size = Size::new(400.0, 300.0);
const BUTTON: Point = Point::new(20.0, 15.0);
const CHART_MID: Point = Point::new(200.0, 165.0);

static LAST_PANIC: Mutex<Option<String>> = Mutex::new(None);

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

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
        let id = compiled.exprs[0].id;
        let root = loop {
            let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
                .await
                .context("timeout waiting for the widget value")?
                .context("event channel closed")?;
            let found = batch.drain(..).find_map(|e| match e {
                GXEvent::Updated(i, v) if i == id => Some(v),
                _ => None,
            });
            if let Some(v) = found {
                break v;
            }
        };
        let widget = widgets::compile(gx.clone(), root).await.context("widget")?;
        let mut h = Self {
            _ctx: ctx,
            gx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: Cache::default(),
            cursor: Point::ORIGIN,
        };
        h.drain(Duration::from_millis(300)).await?;
        Ok(h)
    }

    fn apply(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        for ev in batch.drain(..) {
            if let GXEvent::Updated(id, v) = ev {
                let w = &mut self.widget;
                tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
            }
        }
        Ok(())
    }

    /// Apply updates until none arrives for `quiet`.
    async fn drain(&mut self, quiet: Duration) -> Result<()> {
        loop {
            match tokio::time::timeout(quiet, self.rx.recv()).await {
                Ok(Some(batch)) => self.apply(batch)?,
                Ok(None) => bail!("event channel closed"),
                Err(_) => return Ok(()),
            }
        }
    }

    /// One frame as the event loop renders it: build over the kept
    /// cache, update, draw.
    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(self.widget.view(), VIEWPORT, cache, &mut self.renderer);
        let cursor = mouse::Cursor::Available(self.cursor);
        let mut messages = Vec::new();
        let _ = ui.update(
            events,
            cursor,
            &mut self.renderer,
            &mut clipboard::Null,
            &mut messages,
        );
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let style = Style { text_color: theme.palette().text };
        ui.draw(&mut self.renderer, &theme, &style, cursor);
        self.cache = ui.into_cache();
        messages
    }

    fn move_to(&mut self, at: Point) -> Vec<Message> {
        self.cursor = at;
        self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: at })])
    }

    /// Drag with the left button from `from` by (dx, dy) in three steps.
    fn drag(&mut self, from: Point, dx: f32, dy: f32) {
        self.move_to(from);
        self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]);
        for i in 1..=3 {
            let k = i as f32 / 3.0;
            self.move_to(Point::new(from.x + dx * k, from.y + dy * k));
        }
        self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]);
    }

    /// Click "switch", run the calls it produced and apply the updates;
    /// the number of calls.
    async fn switch(&mut self) -> Result<usize> {
        let mut msgs = self.move_to(BUTTON);
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]),
        );
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]),
        );
        let mut calls = 0;
        for m in &msgs {
            if let Message::Call(id, args) = m {
                calls += 1;
                self.gx.call(*id, args.clone())?;
            }
        }
        self.drain(Duration::from_millis(500)).await?;
        Ok(calls)
    }
}

/// Draw one frame; Ok, or the panic's location and message.
fn checked_frame(h: &mut H) -> std::result::Result<(), String> {
    *LAST_PANIC.lock().unwrap() = None;
    match panic::catch_unwind(AssertUnwindSafe(|| {
        h.frame(&[]);
    })) {
        Ok(()) => Ok(()),
        Err(_) => {
            Err(LAST_PANIC.lock().unwrap().take().unwrap_or_else(|| "(no message)".into()))
        }
    }
}

async fn case(name: &str, code: &str, pan: bool) -> Result<std::result::Result<(), String>> {
    let mut h = H::new(code).await?;
    checked_frame(&mut h).map_err(|e| anyhow::anyhow!("{name}: first draw panicked: {e}"))?;
    if pan {
        h.drag(CHART_MID, 60.0, 40.0);
        checked_frame(&mut h)
            .map_err(|e| anyhow::anyhow!("{name}: draw after the pan panicked: {e}"))?;
        println!("{name}: panned the numeric chart by (60, 40) px");
    }
    let calls = h.switch().await?;
    println!("{name}: clicked switch, {calls} call(s)");
    let r = checked_frame(&mut h);
    match &r {
        Ok(()) => println!("{name}: draw after the switch returned"),
        Err(e) => println!("{name}: draw after the switch PANICKED: {e}"),
    }
    Ok(r)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn inherited_chart_view() -> Result<()> {
    let prev = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        let loc = info.location().map(|l| l.to_string()).unwrap_or_default();
        let msg = info
            .payload()
            .downcast_ref::<&str>()
            .map(|s| s.to_string())
            .or_else(|| info.payload().downcast_ref::<String>().cloned())
            .unwrap_or_default();
        *LAST_PANIC.lock().unwrap() = Some(format!("at {loc}: {msg}"));
        prev(info);
    }));
    let no_pan = case("swap_no_pan (control)", SWAP, false).await?;
    let swap = case("swap_after_pan", SWAP, true).await?;
    let mode = case("mode_change_after_pan", MODE, true).await?;
    assert!(no_pan.is_ok(), "control: {no_pan:?}");
    assert!(
        swap.is_ok() && mode.is_ok(),
        "a time-series chart drawn through a numeric chart's view: \
         swap_after_pan {swap:?}, mode_change_after_pan {mode:?}"
    );
    Ok(())
}
