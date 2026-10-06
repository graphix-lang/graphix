//! gui-widgets-a-01: Canvas NaN/inf coordinates (e.g. v / 0.0) panic
//! iced/lyon on the main thread, killing the process.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_01.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_01 -- --nocapture
//!
//! Each test compiles a canvas whose one filled circle sits at
//! `x: v / total * 300.0` with `v = 0.0`, builds the widget, and draws it
//! through a headless wgpu renderer exactly as `GuiHandler::about_to_wait`
//! does (`UserInterface::build` + `draw`).
//!
//! Expected: both tests pass (a shape with a NaN coordinate is refused or
//! skipped; the draw returns).
//! Observed at c722befe (dev profile): `finite_circle_draws` passes,
//! `nan_circle_draws` panics in the draw:
//!   panicked at lyon_path-1.0.19/src/path.rs:811:5:
//!   assertion failed: p.x.is_finite()
//! reached from widgets/canvas.rs `draw_shape` -> `Path::circle`. In the
//! shell that draw runs in winit's `run_app` on the main thread, so the
//! process exits. Release builds skip lyon's debug assertion and panic one
//! step later in iced_wgpu's `Frame::fill` (`.expect("Tessellate path.")`
//! on lyon's `PositionIsNaN`).

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Size, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use std::time::Duration;
use tokio::sync::{OnceCell, mpsc};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

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

struct Canvas {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    widget: GuiW<NoExt>,
}

async fn canvas(total: &str) -> Result<Canvas> {
    let code = format!(
        "use gui::{{color, canvas::canvas}};\n\
         let total = {total};\n\
         let v = 0.0;\n\
         let result = canvas(#width: &`Fill, #height: &`Fixed(200.0), &[\
         `Circle({{ center: {{ x: v / total * 300.0, y: 50.0 }}, radius: 4.0, \
         fill: color(#r: 1.0)$, stroke: null }})])"
    );
    let (tx, mut rx) = mpsc::channel(100);
    let vfs = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(code)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
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
    let mut widget =
        widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
    let rt = tokio::runtime::Handle::current();
    while let Ok(Some(mut batch)) =
        tokio::time::timeout(Duration::from_millis(200), rx.recv()).await
    {
        for e in batch.drain(..) {
            if let GXEvent::Updated(i, v) = e {
                widget.handle_update(&rt, i, &v)?;
            }
        }
    }
    Ok(Canvas { _ctx: ctx, _compiled: compiled, widget })
}

fn draw(widget: &GuiW<NoExt>, renderer: &mut widgets::Renderer) {
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    let mut ui = UserInterface::build(
        widget.view(),
        Size::new(400.0, 300.0),
        Cache::default(),
        renderer,
    );
    ui.draw(renderer, &theme, &style, mouse::Cursor::Unavailable);
}

#[tokio::test(flavor = "current_thread")]
async fn finite_circle_draws() -> Result<()> {
    let c = canvas("1.0").await?;
    let mut r = renderer().await;
    draw(&c.widget, &mut r);
    eprintln!("finite_circle_draws: draw returned");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn nan_circle_draws() -> Result<()> {
    let c = canvas("0.0").await?;
    let mut r = renderer().await;
    eprintln!("nan_circle_draws: drawing a circle at x = 0.0 / 0.0 * 300.0");
    draw(&c.widget, &mut r);
    eprintln!("nan_circle_draws: draw returned");
    Ok(())
}
