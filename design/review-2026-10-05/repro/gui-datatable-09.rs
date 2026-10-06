//! gui-datatable-09: a data_table column-resize drag sticks when the left
//! button is released outside the table's bounds.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_09.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_09 -- --nocapture
//!
//! Each test compiles a data_table whose column `c0` has an `on_resize`
//! callback, then drives the real widget the way `GuiHandler::about_to_wait`
//! does: one `UserInterface` per frame over the persisted cache, the
//! frame's events, the window's last cursor position, and the produced
//! messages fed back through `GuiWidget::on_message` (a `Message::Call` is
//! the on_resize invocation the event loop would make). The drag presses
//! c0's header resize handle, moves 40 px right inside the table, then:
//!   - release_inside_ends_drag (control): releases inside the table;
//!   - release_outside_window_sticks: moves past the window's right edge
//!     (where X11/Wayland/Windows keep delivering motion during the
//!     implicit grab) and releases there;
//!   - release_over_sibling_sticks: the table sits in a row beside a
//!     100 px text; the drag overshoots onto the text and releases there.
//! After the release the mouse hovers back over the table with no button
//! held, and finally clicks inside the table.
//!
//! Expected: every test passes. A left release ends the drag wherever it
//! happens; hovering afterwards neither resizes c0 nor calls on_resize
//! (the book: on_resize fires "while the user drags").
//! Observed at c722befe (dev profile): `1 passed; 2 failed`. The control
//! passes; both other tests panic with
//!   the drag outlived the release: ColumnResizeEnd published = false,
//!   still resizing after the release = true, on_resize calls while
//!   hovering = [118.0, 80.0, 180.0], c0 handle moved from x = 182 to
//!   x = 252
//! The release outside the table publishes no ColumnResizeEnd (iced's
//! MouseArea drops a release whose cursor is not over its bounds), so
//! every later hover move resizes c0 and calls on_resize (the first move
//! back in jumps by the re-entry distance: 110 -> 118 for 192 -> 200),
//! until the next left release inside the table publishes ColumnResizeEnd.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Event, Point, Size, clipboard, mouse};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx_value::Value;
use poolshark::global::GPooled;
use std::{collections::VecDeque, fmt::Write, time::Duration};
use tokio::sync::{OnceCell, mpsc};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const HEADER_Y: f32 = 10.0;
const C0: usize = 1;

struct Gpu {
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

static GPU: OnceCell<Gpu> = OnceCell::const_new();

async fn renderer() -> widgets::Renderer {
    let gpu = GPU
        .get_or_init(|| async {
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
            Gpu { adapter, device, queue }
        })
        .await;
    let engine = iced_wgpu::Engine::new(
        &gpu.adapter,
        gpu.device.clone(),
        gpu.queue.clone(),
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

fn program(beside_text: bool) -> String {
    let table = r#"data_table(#table: &tbl)"#;
    let result = if beside_text {
        format!("row(&[{table}, text(#width: &`Fixed(100.0), &\"side panel\")])")
    } else {
        table.to_string()
    };
    format!(
        r#"use gui::{{row::row, text::text, data_table::data_table}};
let log = 0.0;
let on_w = |new_w: f64| log <- new_w;
let tbl = {{ rows: ["r0", "r1"], columns: [
        {{ name: "c0", typ: `Text({{ on_edit: null }}), display_name: null, source: &"x", on_resize: &on_w, width: &null }},
        {{ name: "c1", typ: `Text({{ on_edit: null }}), display_name: null, source: &"y", on_resize: &null, width: &null }}
    ] }};
let result = {result}
"#
    )
}

struct Window {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    viewport: Size,
    cursor: Point,
    log: String,
}

impl Window {
    async fn new(beside_text: bool) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(program(beside_text))),
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
        let mut w = Self {
            _ctx: ctx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: Cache::default(),
            viewport: Size::new(400.0, 200.0),
            cursor: Point::ORIGIN,
            log: String::new(),
        };
        for _ in 0..20 {
            w.drain().await?;
            if w.handle_x(C0).is_some() {
                return Ok(w);
            }
        }
        bail!("c0's resize handle never rendered")
    }

    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(100), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    let widget = &mut self.widget;
                    tokio::task::block_in_place(|| widget.handle_update(&rt, i, &v))?;
                }
            }
        }
        Ok(())
    }

    /// One frame of `about_to_wait`: build over the persisted cache, feed
    /// the events with the window's cursor, collect the messages.
    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        self.widget.before_view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(self.widget.view(), self.viewport, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    /// The message drain of `about_to_wait`; returns the widths passed
    /// to on_resize (the `Message::Call`s the widget published).
    fn dispatch(&mut self, msgs: &[Message]) -> Vec<f64> {
        let mut widths = Vec::new();
        let mut pending: VecDeque<Message> = msgs.iter().cloned().collect();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop => {}
                Message::Call(_, args) => match args.first() {
                    Some(Value::F64(w)) => widths.push(*w),
                    v => panic!("unexpected call argument {v:?}"),
                },
                other => {
                    let mut shell = MessageShell::new(self.cursor);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        widths
    }

    fn step(&mut self, label: &str, ev: Event) -> (Vec<Message>, Vec<f64>) {
        if let Event::Mouse(mouse::Event::CursorMoved { position }) = &ev {
            self.cursor = *position;
        }
        let msgs = self.frame(&[ev]);
        let widths = self.dispatch(&msgs);
        let resizing = self.widget.is_column_resizing();
        let _ = writeln!(
            self.log,
            "  {label:<34} cursor=({:>5.1},{:>5.1}) msgs={msgs:?} on_resize={widths:?} resizing={resizing}",
            self.cursor.x, self.cursor.y,
        );
        (msgs, widths)
    }

    fn move_to(&mut self, label: &str, x: f32, y: f32) -> (Vec<Message>, Vec<f64>) {
        self.step(label, Event::Mouse(mouse::Event::CursorMoved { position: Point::new(x, y) }))
    }

    fn press(&mut self) -> (Vec<Message>, Vec<f64>) {
        self.step("press left", Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)))
    }

    fn release(&mut self) -> (Vec<Message>, Vec<f64>) {
        self.step(
            "release left",
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
        )
    }

    /// Middle of the x range where a press in the header row starts a
    /// resize of column `ci`, probed on throwaway interfaces.
    fn handle_x(&mut self, ci: usize) -> Option<f32> {
        let mut hits = Vec::new();
        let mut x = 0.0;
        while x < self.viewport.width {
            self.widget.before_view();
            let mut ui = UserInterface::build(
                self.widget.view(),
                self.viewport,
                Cache::default(),
                &mut self.renderer,
            );
            let mut msgs = Vec::new();
            let _ = ui.update(
                &[
                    Event::Mouse(mouse::Event::CursorMoved { position: Point::new(x, HEADER_Y) }),
                    Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
                ],
                mouse::Cursor::Available(Point::new(x, HEADER_Y)),
                &mut self.renderer,
                &mut clipboard::Null,
                &mut msgs,
            );
            if msgs.iter().any(|m| matches!(m, Message::ColumnResizeStart(i) if *i == ci)) {
                hits.push(x);
            }
            x += 1.0;
        }
        Some((hits.first()? + hits.last()?) / 2.0)
    }

    /// Press c0's handle and drag it 40 px right, inside the table.
    fn start_drag(&mut self) -> f32 {
        let hx = self.handle_x(C0).expect("c0 handle");
        let _ = writeln!(self.log, "  c0 resize handle at x = {hx}");
        self.move_to("move onto c0's handle", hx, HEADER_Y);
        let (msgs, _) = self.press();
        assert!(
            msgs.iter().any(|m| matches!(m, Message::ColumnResizeStart(C0))),
            "the press did not start a resize: {msgs:?}"
        );
        assert!(self.widget.is_column_resizing());
        let mut widths = Vec::new();
        for i in 1..=4 {
            widths.extend(self.move_to("drag right (inside)", hx + 10.0 * i as f32, HEADER_Y).1);
        }
        assert!(!widths.is_empty(), "dragging inside the table never called on_resize");
        hx + 40.0
    }

    /// After the release: hover back over the table, no button held.
    fn hover_after_release(&mut self) -> Vec<f64> {
        let mut widths = Vec::new();
        widths.extend(self.move_to("hover, no button (1)", 200.0, 100.0).1);
        widths.extend(self.move_to("hover, no button (2)", 150.0, 100.0).1);
        widths.extend(self.move_to("hover, no button (3)", 250.0, 120.0).1);
        widths
    }

    fn check_released(&mut self, test: &str, release_msgs: &[Message]) {
        let ended = release_msgs.iter().any(|m| matches!(m, Message::ColumnResizeEnd));
        let still_resizing = self.widget.is_column_resizing();
        let handle_before = self.handle_x(C0).expect("c0 handle");
        let hover = self.hover_after_release();
        let handle_after = self.handle_x(C0).expect("c0 handle");
        let _ = writeln!(
            self.log,
            "  c0 handle after the release at x = {handle_before}, after hovering at x = {handle_after}"
        );
        self.move_to("move inside the table", 200.0, 100.0);
        self.press();
        self.release();
        eprintln!("{test}:\n{}", self.log);
        assert!(
            ended && !still_resizing && hover.is_empty() && handle_before == handle_after,
            "{test}: the drag outlived the release: ColumnResizeEnd published = {ended}, \
             still resizing after the release = {still_resizing}, on_resize calls while \
             hovering = {hover:?}, c0 handle moved from x = {handle_before} to x = {handle_after}"
        );
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn release_inside_ends_drag() -> Result<()> {
    let mut w = Window::new(false).await?;
    let x = w.start_drag();
    w.move_to("stay inside the table", x, HEADER_Y);
    let (msgs, _) = w.release();
    w.check_released("release_inside_ends_drag", &msgs);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn release_outside_window_sticks() -> Result<()> {
    let mut w = Window::new(false).await?;
    w.start_drag();
    w.move_to("drag past the window's right edge", 450.0, HEADER_Y);
    let (msgs, _) = w.release();
    w.check_released("release_outside_window_sticks", &msgs);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn release_over_sibling_sticks() -> Result<()> {
    let mut w = Window::new(true).await?;
    w.start_drag();
    w.move_to("drag onto the side text", 350.0, HEADER_Y);
    let (msgs, _) = w.release();
    w.check_released("release_over_sibling_sticks", &msgs);
    Ok(())
}
