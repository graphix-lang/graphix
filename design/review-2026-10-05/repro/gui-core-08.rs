//! gui-core-08: the gui shell hands iced a cursor that is always
//! `Cursor::Available`: it starts at (0,0) and keeps its last position
//! after `CursorLeft` (stdlib/graphix-package-gui/src/window.rs:105 and
//! :232-234, event_loop.rs:191-196).
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_08.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_08 -- --nocapture
//!
//! The program is a full-window `mouse_area` whose `#on_enter` / `#on_exit`
//! count into `entered` / `exited`. Frames are fed as `GuiHandler` feeds
//! them: winit events through the real `convert::window_event`, the
//! `Window::Resized` that `about_to_wait` pushes for the initial winit
//! `Resized`, one `UserInterface::update` per frame over a headless
//! renderer, every `Message::Call` sent with `gx.call`. The cursor handed to
//! iced comes from one of two models:
//!   graphix     `TrackedWindow`: `Point::ORIGIN` at creation, set by each
//!               `CursorMoved`, always `Cursor::Available`;
//!   iced_winit  iced_winit 0.14 `window::State`: `None` at creation,
//!               `Some` on `CursorMoved`, `None` on `CursorLeft`,
//!               `Cursor::Unavailable` while `None`.
//! Script, the pointer starting outside the window: the first frame (initial
//! `Resized`, `Focused(true)`); enter at the right edge and move to the
//! center; move back to the edge and leave; `Focused(false)` while outside;
//! re-enter at the center.
//!
//! Expected (the book: on_enter "called when the mouse cursor enters the
//! area", on_exit "called when the mouse cursor leaves the area"), both
//! models: (entered, exited) = (0,0) (1,0) (1,1) (1,1) (2,1).
//! Observed at c722befe (dev profile):
//!   RESULT IcedWinit: first frame, pointer outside (i64:0,i64:0)
//!   RESULT IcedWinit: entered at the right edge, now at the center (i64:1,i64:0)
//!   RESULT IcedWinit: left through the right edge (i64:1,i64:1)
//!   RESULT IcedWinit: Focused(false) while outside (i64:1,i64:1)
//!   RESULT IcedWinit: re-entered at the center (i64:2,i64:1)
//!   RESULT Graphix: first frame, pointer outside (i64:1,i64:0)
//!   RESULT Graphix: entered at the right edge, now at the center (i64:1,i64:0)
//!   RESULT Graphix: left through the right edge (i64:1,i64:0)
//!   RESULT Graphix: Focused(false) while outside (i64:1,i64:0)
//!   RESULT Graphix: re-entered at the center (i64:1,i64:0)
//! Under the graphix model on_enter fires on the first frame although the
//! pointer never entered, on_exit never fires when the pointer leaves the
//! window, and the re-entry is not reported.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    convert,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, NoExt, Ref};
use iced_core::{Event, Point, Size, clipboard, mouse, window};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::{path::Path, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;
use winit::{
    event::{DeviceId, WindowEvent},
    keyboard::ModifiersState,
};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const CODE: &str = "use gui::{container::container, mouse_area::mouse_area, text::text};
let entered = 0;
let exited = 0;
let result = mouse_area(
  #on_enter: |e| entered <- e ~ entered + 1,
  #on_exit: |e| exited <- e ~ exited + 1,
  &container(#width: &`Fill, #height: &`Fill, &text(&\"zone\"))
)";

const VIEWPORT: Size = Size::new(300.0, 200.0);

struct Gpu {
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

async fn gpu() -> Gpu {
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
}

fn renderer(g: &Gpu) -> widgets::Renderer {
    let engine = iced_wgpu::Engine::new(
        &g.adapter,
        g.device.clone(),
        g.queue.clone(),
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    let (module, var) = name.split_once("::").context("module::var")?;
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name}")
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Model {
    Graphix,
    IcedWinit,
}

struct Session {
    ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    entered: Ref<NoExt>,
    exited: Ref<NoExt>,
    values: AHashMap<ExprId, Value>,
    renderer: widgets::Renderer,
    cache: Cache,
    model: Model,
    graphix_cursor: Point,
    iced_winit_cursor: Option<Point>,
    pending: Vec<Event>,
}

impl Session {
    async fn new(model: Model, g: &Gpu) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(CODE)),
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
        let entered =
            ctx.rt.compile_ref(find_bind_id(&compiled.env, "test::entered")?).await?;
        let exited =
            ctx.rt.compile_ref(find_bind_id(&compiled.env, "test::exited")?).await?;
        let mut values = AHashMap::default();
        values.insert(entered.id, entered.last.clone().unwrap_or(Value::Null));
        values.insert(exited.id, exited.last.clone().unwrap_or(Value::Null));
        let mut s = Self {
            ctx,
            _compiled: compiled,
            rx,
            widget,
            entered,
            exited,
            values,
            renderer: renderer(g),
            cache: Cache::default(),
            model,
            graphix_cursor: Point::ORIGIN,
            iced_winit_cursor: None,
            pending: Vec::new(),
        };
        s.drain().await;
        Ok(s)
    }

    /// `GuiHandler::window_event` for an event that is neither `Resized`
    /// nor `RedrawRequested`, and iced_winit's `window::State::update`.
    fn window_event(&mut self, ev: WindowEvent) {
        match &ev {
            WindowEvent::CursorMoved { position, .. } => {
                self.iced_winit_cursor =
                    Some(Point::new(position.x as f32, position.y as f32));
            }
            WindowEvent::CursorLeft { .. } => self.iced_winit_cursor = None,
            _ => (),
        }
        let mut evs = convert::window_event(&ev, 1.0, ModifiersState::empty());
        for e in evs.drain(..) {
            if let Event::Mouse(mouse::Event::CursorMoved { position }) = &e {
                self.graphix_cursor = *position;
            }
            self.pending.push(e);
        }
    }

    /// The event `about_to_wait` pushes for the initial winit `Resized`.
    fn resized(&mut self) {
        self.pending.push(Event::Window(window::Event::Resized(VIEWPORT)));
    }

    fn cursor(&self) -> mouse::Cursor {
        match self.model {
            Model::Graphix => mouse::Cursor::Available(self.graphix_cursor),
            Model::IcedWinit => self
                .iced_winit_cursor
                .map(mouse::Cursor::Available)
                .unwrap_or(mouse::Cursor::Unavailable),
        }
    }

    /// One `about_to_wait` render: update the UI with the pending events,
    /// send every `Message::Call` to the runtime, wait for its writes.
    async fn frame(&mut self) -> Result<()> {
        let cursor = self.cursor();
        let cache = std::mem::take(&mut self.cache);
        let mut messages: Vec<Message> = Vec::new();
        {
            let mut ui = UserInterface::build(
                self.widget.view(),
                VIEWPORT,
                cache,
                &mut self.renderer,
            );
            let _ = ui.update(
                &self.pending,
                cursor,
                &mut self.renderer,
                &mut clipboard::Null,
                &mut messages,
            );
            self.cache = ui.into_cache();
        }
        self.pending.clear();
        for m in messages {
            if let Message::Call(id, args) = m {
                self.ctx.rt.call(id, args)?;
            }
        }
        self.drain().await;
        Ok(())
    }

    async fn drain(&mut self) {
        loop {
            match tokio::time::timeout(Duration::from_millis(150), self.rx.recv()).await
            {
                Ok(Some(mut batch)) => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if let Some(slot) = self.values.get_mut(&id) {
                                *slot = v;
                            }
                        }
                    }
                }
                Ok(None) | Err(_) => break,
            }
        }
    }

    fn counts(&self) -> String {
        format!("({},{})", self.values[&self.entered.id], self.values[&self.exited.id])
    }
}

async fn run(model: Model, g: &Gpu) -> Result<Vec<String>> {
    let did = DeviceId::dummy();
    let mut s = Session::new(model, g).await?;
    let mut rows = Vec::new();
    s.resized();
    s.window_event(WindowEvent::Focused(true));
    s.frame().await?;
    rows.push(format!("first frame, pointer outside {}", s.counts()));
    s.window_event(WindowEvent::CursorEntered { device_id: did });
    s.window_event(WindowEvent::CursorMoved {
        device_id: did,
        position: (299.0, 100.0).into(),
    });
    s.frame().await?;
    s.window_event(WindowEvent::CursorMoved {
        device_id: did,
        position: (150.0, 100.0).into(),
    });
    s.frame().await?;
    rows.push(format!("entered at the right edge, now at the center {}", s.counts()));
    s.window_event(WindowEvent::CursorMoved {
        device_id: did,
        position: (298.0, 100.0).into(),
    });
    s.frame().await?;
    s.window_event(WindowEvent::CursorLeft { device_id: did });
    s.frame().await?;
    rows.push(format!("left through the right edge {}", s.counts()));
    s.window_event(WindowEvent::Focused(false));
    s.frame().await?;
    rows.push(format!("Focused(false) while outside {}", s.counts()));
    s.window_event(WindowEvent::CursorEntered { device_id: did });
    s.window_event(WindowEvent::CursorMoved {
        device_id: did,
        position: (150.0, 100.0).into(),
    });
    s.frame().await?;
    rows.push(format!("re-entered at the center {}", s.counts()));
    Ok(rows)
}

#[tokio::test(flavor = "current_thread")]
async fn cursor_outside_the_window() -> Result<()> {
    let g = gpu().await;
    for model in [Model::IcedWinit, Model::Graphix] {
        for row in run(model, &g).await? {
            eprintln!("RESULT {model:?}: {row}");
        }
    }
    Ok(())
}
