//! gui-widgets-b-09: `mouse_area.gx` defaults all five handlers to
//! `|_| null`, so `MouseAreaW::view` (src/widgets/mouse_area.rs:150-199)
//! installs every one of them whatever the program asked for.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_09.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_09 -- --nocapture
//!
//! `Gui::frame` is one `about_to_wait`: build the UI from `view()`, feed
//! the events, `gx.call` every `Message::Call`. No window is opened.
//!
//! Expected: a mouse_area given only `#on_press` sends nothing to the
//! runtime while the cursor moves over it; a mouse_area given only
//! `#on_move` lets a click through to the button below it in a stack.
//! Observed (HEAD c722befe, dev profile):
//!   a_moves_over_an_on_press_area ... FAILED: 10 moves made 10 runtime
//!     calls (the default on_enter with null, then the default on_move
//!     with a fresh {x, y} struct per move) and 0 updates came back: the
//!     no-op's result never fires, so the cost is a call and a runtime
//!     cycle per move, not a widget-tree walk
//!   b_control_button_alone ... ok: clicked = true
//!   c_hover_area_above_a_button ... FAILED: the default on_press and
//!     on_release were called (`Left` twice) and captured the press
//!     (iced_widget 0.14.2 mouse_area.rs:376-379), so the button below
//!     never saw it: clicked = false

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId, CFlag,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, Renderer};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Point, Size, clipboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

async fn renderer() -> Renderer {
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
        .expect("no GPU device");
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

fn find_bind_id(env: &Env, module: &str, var: &str) -> Result<BindId> {
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if netidx::path::Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {module}::{var}")
}

struct Gui {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: Renderer,
    cache: user_interface::Cache,
    cursor: Point,
    watch: Ref<NoExt>,
    watched: Value,
    calls: Vec<Value>,
    updates: usize,
}

impl Gui {
    async fn new(code: &str, var: &str, fusion: bool) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let flags = if fusion { Default::default() } else { CFlag::FusionDisabled.into() };
        let ctx = testing::init_with_flags_and_setup(
            tx,
            REGISTER,
            vec![VfsResolver::new(tbl)],
            flags,
            |_| {},
        )
        .await?;
        let gx = ctx.rt.clone();
        let compiled: CompRes<NoExt> = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root_id = compiled.exprs[0].id;
        let root = tokio::time::timeout(Duration::from_secs(60), async {
            loop {
                let mut batch = rx.recv().await.context("channel closed")?;
                if let Some(v) = batch.drain(..).find_map(|e| match e {
                    GXEvent::Updated(id, v) if id == root_id => Some(v),
                    _ => None,
                }) {
                    return Ok::<Value, anyhow::Error>(v);
                }
            }
        })
        .await
        .context("timeout waiting for the root value")??;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        let bid = find_bind_id(&compiled.env, "test", var)?;
        let watch = gx.compile_ref(bid).await.context("ref to the watched var")?;
        let watched = watch.last.clone().unwrap_or(Value::Null);
        Ok(Self {
            _ctx: ctx,
            gx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: Point::ORIGIN,
            watch,
            watched,
            calls: vec![],
            updates: 0,
        })
    }

    fn apply(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                self.updates += 1;
                if id == self.watch.id {
                    println!("  watched <- {v}");
                    self.watched = v.clone();
                }
                self.widget.handle_update(&rt, id, &v)?;
            }
        }
        Ok(())
    }

    /// Deliver everything until the runtime has been quiet for `quiet`;
    /// Err when the runtime died.
    async fn settle(&mut self, quiet: Duration) -> Result<()> {
        loop {
            match tokio::time::timeout(quiet, self.rx.recv()).await {
                Ok(Some(batch)) => self.apply(batch)?,
                Ok(None) => bail!("the runtime died (its event channel closed)"),
                Err(_) => return Ok(()),
            }
        }
    }

    fn frame(&mut self, events: &[Event]) -> Result<()> {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(element, Size::new(300.0, 50.0), cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        for m in msgs {
            if let Message::Call(id, args) = m {
                let shown: Vec<String> = args.iter().map(|v| format!("{v}")).collect();
                println!("  callback called with {shown:?}");
                self.calls.extend(args.iter().cloned());
                self.gx.call(id, args)?;
            }
        }
        Ok(())
    }

    fn click(&mut self, pos: Point) -> Result<()> {
        self.cursor = pos;
        self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })])?;
        self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))])?;
        self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))])
    }
}

async fn settled(code: &str, var: &str) -> Result<Gui> {
    let mut g = Gui::new(code, var, true).await?;
    g.settle(Duration::from_millis(300)).await?;
    g.calls.clear();
    g.updates = 0;
    Ok(g)
}

/// Only `#on_press`: ten cursor moves over the area.
#[tokio::test(flavor = "current_thread")]
async fn a_moves_over_an_on_press_area() -> Result<()> {
    let mut g = settled(
        "use gui::{mouse_area::mouse_area, text::text};\n\
         let pressed = false;\n\
         let result = mouse_area(#on_press: |b| pressed <- b ~ true, \
             &text(&\"a click zone wide enough to move over\"))",
        "pressed",
    )
    .await?;
    for i in 0..10 {
        let p = Point::new(5.0 + 10.0 * i as f32, 8.0);
        g.cursor = p;
        g.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: p })])?;
    }
    g.settle(Duration::from_millis(300)).await?;
    println!("  10 moves: {} runtime calls, {} updates back", g.calls.len(), g.updates);
    assert_eq!(g.calls.len(), 0, "moves over an on_press-only area called the runtime");
    Ok(())
}

const STACK: &str = "use gui::{button::button, mouse_area::mouse_area, space::space, \
    stack::stack, text::text};\n\
    let clicked = false;\n\
    let pos = \"\";\n";

/// Control: the button alone in the stack takes the click.
#[tokio::test(flavor = "current_thread")]
async fn b_control_button_alone() -> Result<()> {
    let code = format!(
        "{STACK}let result = stack(#width: &`Fill, #height: &`Fill, &[\
         button(#on_press: |e| clicked <- e ~ true, #width: &`Fill, #height: &`Fill, \
             &text(&\"below\"))])"
    );
    let mut g = settled(&code, "clicked").await?;
    g.click(Point::new(10.0, 10.0))?;
    g.settle(Duration::from_millis(300)).await?;
    println!("  clicked = {}", g.watched);
    assert_eq!(g.watched, Value::Bool(true));
    Ok(())
}

/// A hover-only mouse_area above the button.
#[tokio::test(flavor = "current_thread")]
async fn c_hover_area_above_a_button() -> Result<()> {
    let code = format!(
        "{STACK}let result = stack(#width: &`Fill, #height: &`Fill, &[\
         button(#on_press: |e| clicked <- e ~ true, #width: &`Fill, #height: &`Fill, \
             &text(&\"below\")), \
         mouse_area(#on_move: |p| pos <- p ~ \"[p.x]\", \
             &space(#width: &`Fill, #height: &`Fill))])"
    );
    let mut g = settled(&code, "clicked").await?;
    g.click(Point::new(10.0, 10.0))?;
    g.settle(Duration::from_millis(300)).await?;
    println!("  clicked = {}", g.watched);
    assert_eq!(g.watched, Value::Bool(true), "the click never reached the button");
    Ok(())
}
