//! gui-datatable-08: programmatic scrolls never move the overlay
//! scrollable, so the next wheel scroll jumps. Keyboard navigation and
//! `ensure_selection_visible` (`scroll_to_cell`, layout.rs:208-243) and
//! the reset in `apply_table_sync` (subscriptions.rs:283-284) change
//! `first_row`/`first_col` only; nothing moves the iced scrollable that
//! render.rs:438 stacks over the grid, so the next wheel notch scrolls from
//! the overlay's stale offset and `handle_scroll` (events.rs:276) takes
//! that offset as the new position.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_08.rs:
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_datatable_08 -- --nocapture
//!
//! Each frame runs as `GuiHandler::about_to_wait` runs it: `before_view`,
//! `view`, `UserInterface::build` + `update` over a headless wgpu
//! renderer, the messages dispatched FIFO to `on_message`, then the
//! runtime's updates delivered to `handle_update`; as in the real loop, no
//! RedrawRequested reaches iced. The first visible row is read from the
//! `CellClick` a click on the first body row produces (not dispatched).
//! Viewport 400x300: 13 rows in view, 300 rows.
//!
//! Expected: one wheel notch down moves the view down about 60 px (about 3
//! rows) from the row it shows, and the scrollbar tracks the view.
//! Observed at c722befe (dev profile), the test FAILS:
//!   A: initial first row = 0
//!   A: after a click on r0 and 40 x ArrowDown, first row = 28
//!   A: one wheel notch DOWN: the overlay reports offset (0, 60); first row = 3
//!   B: 10 wheel notches down: offset (0, 600), first row = 27
//!   B: after the table update, first row = 0
//!   B: one wheel notch DOWN: the overlay reports offset (0, 660); first row = 30
//!   panicked: ["A: a wheel notch down moved the view from row 28 to row 3",
//!              "B: a wheel notch down moved the view from row 0 to row 30"]

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    render::GpuState,
    widgets::{self, GuiW, Message, MessageShell},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Event, Point, Size, clipboard, keyboard, mouse};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::wgpu;
use netidx::{path::Path, publisher::Value};
use poolshark::global::GPooled;
use std::{collections::VecDeque, time::Duration};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const VIEWPORT: Size = Size::new(400.0, 300.0);
/// Inside column c0 (x 80..160) of the first body row (below the header).
const FIRST_ROW_PROBE: Point = Point::new(120.0, 35.0);
/// Over the table body, away from the scrollbars.
const REST: Point = Point::new(200.0, 150.0);
const N_ROWS: usize = 300;

async fn gpu() -> GpuState {
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
            .expect("no GPU adapter available"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("failed to create GPU device");
    GpuState {
        instance,
        adapter,
        device,
        queue,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
    }
}

fn rows() -> String {
    (0..N_ROWS).map(|i| format!("\"r{i}\"")).collect::<Vec<_>>().join(", ")
}

fn find_bind_id(env: &Env, var: &str) -> Result<BindId> {
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with("/test") {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding test::{var}")
}

struct Table {
    ctx: TestCtx,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt: tokio::runtime::Handle,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
}

impl Table {
    async fn new(code: String, gpu: &GpuState) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel::<GPooled<Vec<GXEvent>>>(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
                .await?;
        let compiled = ctx
            .rt
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let id = compiled.exprs[0].id;
        let root = loop {
            let mut batch = tokio::time::timeout(Duration::from_secs(5), rx.recv())
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
        let widget =
            widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
        let mut t = Table {
            ctx,
            compiled,
            rx,
            widget,
            rt: tokio::runtime::Handle::current(),
            renderer: gpu.create_renderer(),
            cache: Cache::default(),
            cursor: REST,
        };
        t.drain().await?;
        let _ = t.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: REST })]);
        Ok(t)
    }

    /// One frame as `GuiHandler::about_to_wait` runs it.
    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let mut clip = clipboard::Null;
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clip,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    /// Dispatch FIFO as `about_to_wait` does, then deliver the runtime's
    /// updates to the widget.
    async fn dispatch(&mut self, msgs: Vec<Message>) -> Result<()> {
        let mut pending: VecDeque<Message> = msgs.into();
        while let Some(m) = pending.pop_front() {
            match m {
                Message::Nop => {}
                Message::Call(id, args) => self.ctx.rt.call(id, args)?,
                other => {
                    let mut shell = MessageShell::new(self.cursor);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        self.drain().await
    }

    async fn drain(&mut self) -> Result<()> {
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(150), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    self.widget.handle_update(&self.rt, i, &v)?;
                }
            }
        }
        Ok(())
    }

    fn click(&mut self, p: Point) -> Vec<Message> {
        self.cursor = p;
        let mut msgs =
            self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: p })]);
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]),
        );
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]),
        );
        self.cursor = REST;
        let _ = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: REST })]);
        msgs
    }

    /// The row shown first in the body: the row index of the CellClick a
    /// click there produces (the click is not dispatched).
    fn first_row(&mut self) -> usize {
        self.click(FIRST_ROW_PROBE)
            .iter()
            .find_map(|m| match m {
                Message::CellClick(r, _) => Some(*r),
                _ => None,
            })
            .expect("a click on the first body row yields CellClick")
    }

    /// One wheel notch down over the table; the overlay's reported offset.
    async fn wheel_down(&mut self) -> Result<(f32, f32)> {
        let msgs = self.frame(&[Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Lines { x: 0.0, y: -1.0 },
        })]);
        let off = msgs
            .iter()
            .find_map(|m| match m {
                Message::Scroll(x, y, _, _) => Some((*x, *y)),
                _ => None,
            })
            .context("the wheel produced no Scroll message")?;
        self.dispatch(msgs).await?;
        Ok(off)
    }

    async fn key(&mut self, named: keyboard::key::Named) -> Result<()> {
        let msgs = self.frame(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: keyboard::Key::Named(named),
            modified_key: keyboard::Key::Named(named),
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers: keyboard::Modifiers::empty(),
            text: None,
            repeat: false,
        })]);
        self.dispatch(msgs).await
    }
}

#[tokio::test(flavor = "current_thread")]
async fn programmatic_scroll_then_wheel() -> Result<()> {
    let gpu = gpu().await;
    let mut failures = Vec::new();

    // A: keyboard navigation past the visible rows, then one wheel notch.
    let code = format!(
        r#"
use gui::*; use gui::data_table::{{self, *}}; use sys::*;
let sel = [];
let tbl = {{ rows: [{rows}], columns: ["c0"] }};
let result = data_table(
    #selection: &sel,
    #on_select: |#path: string| sel <- [path],
    #table: &tbl
)
"#,
        rows = rows()
    );
    let mut t = Table::new(code, &gpu).await?;
    eprintln!("A: initial first row = {}", t.first_row());
    let msgs = t.click(FIRST_ROW_PROBE);
    t.dispatch(msgs).await?;
    for _ in 0..40 {
        t.key(keyboard::key::Named::ArrowDown).await?;
    }
    let after_keys = t.first_row();
    eprintln!("A: after a click on r0 and 40 x ArrowDown, first row = {after_keys}");
    let (ox, oy) = t.wheel_down().await?;
    let after_wheel = t.first_row();
    eprintln!(
        "A: one wheel notch DOWN: the overlay reports offset ({ox}, {oy}); \
         first row = {after_wheel}"
    );
    if after_wheel <= after_keys {
        failures.push(format!(
            "A: a wheel notch down moved the view from row {after_keys} to row \
             {after_wheel}"
        ));
    }
    drop(t);

    // B: a table update resets the view to the top; then one wheel notch.
    let code = format!(
        r#"
use gui::*; use gui::data_table::{{self, *}}; use sys::*;
let trig: i64 = never();
let tbl = {{ rows: [{rows}], columns: ["c0"] }};
tbl <- trig ~ {{ rows: [{rows}], columns: ["c0", "c1"] }};
let result = data_table(#table: &tbl)
"#,
        rows = rows()
    );
    let mut t = Table::new(code, &gpu).await?;
    let mut last = (0.0, 0.0);
    for _ in 0..10 {
        last = t.wheel_down().await?;
    }
    let scrolled = t.first_row();
    eprintln!(
        "B: 10 wheel notches down: offset ({}, {}), first row = {scrolled}",
        last.0, last.1
    );
    let trig = find_bind_id(&t.compiled.env, "trig")?;
    t.ctx.rt.set(trig, Value::I64(1))?;
    t.drain().await?;
    let reset = t.first_row();
    eprintln!("B: after the table update, first row = {reset}");
    let (ox, oy) = t.wheel_down().await?;
    let after_wheel = t.first_row();
    eprintln!(
        "B: one wheel notch DOWN: the overlay reports offset ({ox}, {oy}); \
         first row = {after_wheel}"
    );
    if after_wheel > reset + 3 {
        failures.push(format!(
            "B: a wheel notch down moved the view from row {reset} to row {after_wheel}"
        ));
    }

    assert!(failures.is_empty(), "{failures:#?}");
    Ok(())
}
