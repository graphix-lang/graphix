//! gui-widgets-b-16: a radio clicked before its `value` has arrived
//! calls `on_select` with null, whatever the value's type.
//!
//! `RadioW::view` (src/widgets/radio.rs:108) passes
//! `self.value.last.clone().unwrap_or(Value::Null)` to the click closure
//! and keeps the radio clickable while `value.last` is None. The event
//! loop forwards the click with `GXHandle::call`, which checks only the
//! arity (graphix-rt/src/gx.rs `call_callable`), so a null reaches a
//! parameter the checker typed from the value (`'a`).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_16.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_16 -- --nocapture
//!
//! `Gui::frame` is one `about_to_wait`: build the UI from `view()`, feed
//! the events, `gx.call` every `Message::Call`. No window is opened.
//!
//! Expected: a click on a radio whose value has not arrived does nothing
//! (or delivers a value of the value's type).
//! Observed (HEAD c722befe, dev profile):
//!   a_control_value_present ... ok: on_select(7), got = 7
//!   b_click_before_value ... FAILED: on_select(null); `|x| got <- x`
//!     with x: i64 leaves got = null
//!   c_null_into_a_function_jit, d_..._nodewalk ... ok: `dbl(x)` logs
//!     "arith error ... can't add null" and bottoms
//!   e_null_stored_in_an_i64_jit ... FAILED: `|x| chosen <- x` stores
//!     null in `chosen: i64`; the fused `triple(chosen)` then panics the
//!     runtime: "kernel param `chosen`: runtime Null does not match the
//!     compiled Scalar(I64) slot" (graphix-compiler/src/fusion/kernel.rs:243),
//!     and the runtime's event channel closes (the program is dead)
//!   f_null_stored_in_an_i64_nodewalk ... ok: the same program without
//!     fusion logs an arith error and `shown` stays 0

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
        })
    }

    fn apply(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
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
                println!("  on_select called with {shown:?}");
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

const HIT: Point = Point::new(8.0, 8.0);

/// `v` is an i64 that has not been written yet (its timer is a minute away).
fn program(handler: &str, value: &str) -> String {
    format!(
        "use gui::radio::radio;\n\
         let v: i64 = never();\n\
         v <- sys::time::timer(duration:60.s, false) ~ 7;\n\
         let got: [i64, null] = 0;\n\
         let sel: [i64, null] = null;\n\
         let dbl = |y: i64| y * 2;\n\
         let chosen: i64 = 0;\n\
         let triple = |z: i64| z * 3;\n\
         let shown = triple(chosen);\n\
         let result = radio(#label: &\"pick\", #selected: &sel, #on_select: {handler}, {value})"
    )
}

async fn run(name: &str, handler: &str, value: &str, var: &str, fusion: bool) -> Result<Value> {
    println!("{name} (fusion {fusion}): handler {handler}, value {value}");
    let mut g = Gui::new(&program(handler, value), var, fusion).await?;
    g.settle(Duration::from_millis(300)).await?;
    println!("  before the click: {var} = {}", g.watched);
    g.click(HIT)?;
    g.settle(Duration::from_millis(500)).await?;
    println!("  after the click: {var} = {}", g.watched);
    Ok(g.watched.clone())
}

/// Control: the value is present; the click delivers it.
#[tokio::test(flavor = "current_thread")]
async fn a_control_value_present() -> Result<()> {
    let got = run("a_control_value_present", "|x| got <- x", "&7", "got", true).await?;
    assert_eq!(got, Value::I64(7));
    Ok(())
}

/// The click lands before `v` has a value: `x: i64` receives null.
#[tokio::test(flavor = "current_thread")]
async fn b_click_before_value() -> Result<()> {
    let got = run("b_click_before_value", "|x| got <- x", "&v", "got", true).await?;
    assert_eq!(got, Value::I64(0), "the handler of an i64 radio was called with {got}");
    Ok(())
}

/// The same null handed on to a function of an i64.
#[tokio::test(flavor = "current_thread")]
async fn c_null_into_a_function_jit() -> Result<()> {
    let sel = run("c_null_into_a_function_jit", "|x| sel <- dbl(x)", "&v", "sel", true).await?;
    assert_eq!(sel, Value::Null);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn d_null_into_a_function_nodewalk() -> Result<()> {
    let sel =
        run("d_null_into_a_function_nodewalk", "|x| sel <- dbl(x)", "&v", "sel", false)
            .await?;
    assert_eq!(sel, Value::Null);
    Ok(())
}

/// The null stored in an i64 variable, which a function of it reads.
#[tokio::test(flavor = "current_thread")]
async fn e_null_stored_in_an_i64_jit() -> Result<()> {
    let shown =
        run("e_null_stored_in_an_i64_jit", "|x| chosen <- x", "&v", "shown", true).await?;
    assert_eq!(shown, Value::I64(0));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn f_null_stored_in_an_i64_nodewalk() -> Result<()> {
    let shown =
        run("f_null_stored_in_an_i64_nodewalk", "|x| chosen <- x", "&v", "shown", false)
            .await?;
    assert_eq!(shown, Value::I64(0));
    Ok(())
}
