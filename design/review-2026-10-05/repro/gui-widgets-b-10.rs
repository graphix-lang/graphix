//! gui-widgets-b-10: `slider` hands its step and range to iced unchecked,
//! and a null `#step` is iced's default step of 1.0, not the continuous
//! slider book/src/ui/gui/slider.md promises ("`null` means continuous
//! (no snapping)").
//!
//! `SliderW::view` (src/widgets/slider.rs:195-215) sets a step only when
//! `#step` is a number. iced_widget 0.14.2 `Slider::new` defaults `step`
//! to `T::from(1)` (slider.rs:137), and `locate` (slider.rs:260-286)
//! rounds `percent * (end - start) / step`: a step of 0 makes the value
//! NaN and `NaN.min(end)` is `end`.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_10.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_10 -- --nocapture
//!
//! Each case clicks the slider (300 px wide) at 30% of its track and
//! prints what `on_change` received. No window is opened.
//!
//! Expected: a 0.3, b 0.3 (control), c 30.0, d a value inside the range
//! or no call. (Each click also calls the default no-op `#on_release`
//! with null; `calls` records it after the on_change value.)
//! Observed (HEAD c722befe, dev profile):
//!   a_null_step_unit_range ... FAILED: on_change(0.0); a slider over
//!     0..1 without `#step` can only send 0 or 1
//!   b_control_step_001 ... ok: on_change(0.29999998)
//!   c_zero_step ... FAILED: on_change(100.0), the max, for a click at 30%
//!   d_reversed_range (min 100, max 0) ... no on_change: `Slider::new`
//!     clamps the value to the end (0) and every interior click locates
//!     the end again

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

const HIT: Point = Point::new(90.0, 8.0);

async fn click_at_30(name: &str, args: &str) -> Result<Vec<Value>> {
    println!("{name}: slider({args} ..)");
    let code = format!(
        "use gui::slider::slider;\n\
         let v = 0.5;\n\
         let got: [f64, null] = null;\n\
         let result = slider({args} #on_change: |x| got <- x, &v)"
    );
    let mut g = Gui::new(&code, "got", true).await?;
    g.settle(Duration::from_millis(300)).await?;
    g.click(HIT)?;
    g.settle(Duration::from_millis(300)).await?;
    Ok(g.calls.clone())
}

fn near(calls: &[Value], want: f64) -> bool {
    matches!(calls.first(), Some(Value::F64(x)) if (x - want).abs() < 0.01)
}

/// No `#step` over 0..1: the book says continuous.
#[tokio::test(flavor = "current_thread")]
async fn a_null_step_unit_range() -> Result<()> {
    let calls = click_at_30("a_null_step_unit_range", "#min: &0.0, #max: &1.0,").await?;
    assert!(near(&calls, 0.3), "a click at 30% of 0..1 sent {calls:?}");
    Ok(())
}

/// Control: an explicit fine step.
#[tokio::test(flavor = "current_thread")]
async fn b_control_step_001() -> Result<()> {
    let calls =
        click_at_30("b_control_step_001", "#min: &0.0, #max: &1.0, #step: &0.01,").await?;
    assert!(near(&calls, 0.3), "a click at 30% of 0..1 sent {calls:?}");
    Ok(())
}

/// A zero step.
#[tokio::test(flavor = "current_thread")]
async fn c_zero_step() -> Result<()> {
    let calls = click_at_30("c_zero_step", "#min: &0.0, #max: &100.0, #step: &0.0,").await?;
    assert!(near(&calls, 30.0), "a click at 30% of 0..100 sent {calls:?}");
    Ok(())
}

/// A reversed range.
#[tokio::test(flavor = "current_thread")]
async fn d_reversed_range() -> Result<()> {
    let calls = click_at_30("d_reversed_range", "#min: &100.0, #max: &0.0,").await?;
    println!("  d_reversed_range sent {calls:?}");
    Ok(())
}
