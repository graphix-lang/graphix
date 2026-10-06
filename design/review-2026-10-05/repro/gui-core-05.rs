//! gui-core-05: the GUI renderer is built with `Shell::headless()`
//! (stdlib/graphix-package-gui/src/render.rs:58), so the notice iced_wgpu
//! sends when a raster image (a path or bytes source) finishes loading goes
//! nowhere, and the image appears only when unrelated input next causes a
//! render.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_05.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_05 -- --nocapture
//!
//! Both tests use the renderer `GpuState::create_renderer` builds and the
//! `image` widget compiled from Graphix. Each frame is rendered as
//! `GuiHandler::about_to_wait` renders one (`before_view`,
//! `UserInterface::build`, `update`, `draw`, then the renderer's draw, read
//! back with `screenshot` where the loop calls `present`), and its redraw
//! request is mapped the way `about_to_wait` maps it to `needs_redraw`.
//! Frames are rendered only where the event loop renders one when no input
//! arrives: when the window appears and when a widget update arrives.
//!
//! `single_image`: `image(&"<red png>")`, beside a control renderer that is
//! `create_renderer` with a counting shell in place of `Shell::headless()`.
//! `slideshow`: `image(&src)`, `src` cycling over three PNGs (red, green,
//! blue) on a 1500 ms timer.
//!
//! Expected: once a load finishes the event loop is told (a
//! `ToGui::Redraw`), and the next frame shows the image without input.
//! Observed at c722befe (dev profile): `single_image` passes, each
//! assertion one link of the chain; `slideshow` FAILS:
//!   single_image: frame when the window appears: red px 0,
//!     needs_redraw after it: false (the loop waits for input)
//!   single_image: 7.1ms after that frame the image worker told the control
//!     shell: request_redraw x0, invalidate_layout x1; graphix's renderer
//!     has Shell::headless(), it told nobody
//!   single_image: the frame only input would cause: red px 9216 (96x96)
//!   slideshow: the window's first frame and the frames of 5 swaps (green,
//!     blue, red, green, blue), 6 frames: every one shows 0 px of any
//!     color, needs_redraw false after each
//!   slideshow: 700 ms after the last swap, a frame input would cause:
//!     blue px 9216
//!   panicked: 6 of 6 frames the event loop rendered showed no image; no
//!     frame followed any load

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    render::GpuState,
    theme::GraphixTheme,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Color, Point, Size, clipboard, mouse, renderer::Style, window::RedrawRequest,
};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport, shell::Notifier},
    wgpu,
};
use poolshark::global::GPooled;
use std::{
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
    time::{Duration, Instant},
};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const COLORS: [(&str, [u8; 3]); 3] =
    [("red", [255, 0, 0]), ("green", [0, 255, 0]), ("blue", [0, 0, 255])];

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

/// Counts what iced_wgpu's image worker tells the shell.
#[derive(Clone, Default)]
struct Counted(Arc<[AtomicUsize; 2]>);

impl Notifier for Counted {
    fn request_redraw(&self) {
        self.0[0].fetch_add(1, Ordering::SeqCst);
    }

    fn invalidate_layout(&self) {
        self.0[1].fetch_add(1, Ordering::SeqCst);
    }
}

impl Counted {
    fn get(&self) -> (usize, usize) {
        (self.0[0].load(Ordering::SeqCst), self.0[1].load(Ordering::SeqCst))
    }
}

/// `GpuState::create_renderer` with a counting shell in place of
/// `Shell::headless()`.
fn control_renderer(gpu: &GpuState, n: &Counted) -> widgets::Renderer {
    let engine = iced_wgpu::Engine::new(
        &gpu.adapter,
        gpu.device.clone(),
        gpu.queue.clone(),
        gpu.format,
        None,
        Shell::new(n.clone()),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

fn png(dir: &Path, name: &str, rgb: [u8; 3]) -> Result<PathBuf> {
    let img = image::RgbaImage::from_pixel(96, 96, image::Rgba([rgb[0], rgb[1], rgb[2], 255]));
    let path = dir.join(format!("{name}.png"));
    img.save(&path).with_context(|| format!("write {}", path.display()))?;
    Ok(path)
}

fn scratch_dir(test: &str) -> Result<PathBuf> {
    let dir = std::env::temp_dir()
        .join(format!("review_gui_core_05_{test}_{}", std::process::id()));
    std::fs::create_dir_all(&dir)?;
    Ok(dir)
}

struct Compiled {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    widget: GuiW<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
}

async fn compile(code: String) -> Result<Compiled> {
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
    let widget =
        widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
    Ok(Compiled { _ctx: ctx, _compiled: compiled, widget, rx })
}

struct Frame {
    /// What `about_to_wait` derives from the frame's `State`; `None` leaves
    /// `needs_redraw` false and the loop in `ControlFlow::Wait`.
    redraw: Option<RedrawRequest>,
    /// Pixels of each of `COLORS` on screen.
    px: [usize; 3],
}

impl std::fmt::Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, (name, _)) in COLORS.iter().enumerate() {
            write!(f, "{name} px {:5}  ", self.px[i])?;
        }
        write!(f, "needs_redraw after it: {}", self.redraw.is_some())
    }
}

fn frame(
    widget: &mut GuiW<NoExt>,
    renderer: &mut widgets::Renderer,
    cache: &mut user_interface::Cache,
    vp: &Viewport,
) -> Frame {
    widget.before_view();
    let element = widget.view();
    let mut ui =
        UserInterface::build(element, vp.logical_size(), std::mem::take(cache), renderer);
    let mut messages: Vec<Message> = Vec::new();
    let mut clipboard = clipboard::Null;
    let cursor = mouse::Cursor::Available(Point::ORIGIN);
    let (state, _) = ui.update(&[], cursor, renderer, &mut clipboard, &mut messages);
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    ui.draw(renderer, &theme, &style, cursor);
    *cache = ui.into_cache();
    let redraw = match state {
        user_interface::State::Outdated => Some(RedrawRequest::NextFrame),
        user_interface::State::Updated { redraw_request: RedrawRequest::Wait, .. } => {
            None
        }
        user_interface::State::Updated { redraw_request, .. } => Some(redraw_request),
    };
    let rgba = renderer.screenshot(vp, Color::BLACK);
    let mut px = [0; 3];
    for p in rgba.chunks_exact(4) {
        for (i, (_, c)) in COLORS.iter().enumerate() {
            if (0..3).all(|k| (p[k] as i32 - c[k] as i32).abs() < 40) {
                px[i] += 1;
            }
        }
    }
    Frame { redraw, px }
}

fn viewport() -> Viewport {
    Viewport::with_physical_size(Size::new(200, 200), 1.0)
}

#[tokio::test(flavor = "current_thread")]
async fn single_image() -> Result<()> {
    let dir = scratch_dir("single")?;
    let red = png(&dir, "single_red", COLORS[0].1)?;
    let mut w =
        compile(format!("use gui::image::image;\nlet result = image(&\"{}\")", red.display()))
            .await?;
    let gpu = gpu().await;
    let vp = viewport();
    let mut graphix_r = gpu.create_renderer();
    let counted = Counted::default();
    let mut control_r = control_renderer(&gpu, &counted);
    let (mut graphix_cache, mut control_cache) = Default::default();
    let t0 = Instant::now();
    let first = frame(&mut w.widget, &mut graphix_r, &mut graphix_cache, &vp);
    eprintln!("single_image: frame when the window appears:  {first}");
    let control_first = frame(&mut w.widget, &mut control_r, &mut control_cache, &vp);
    eprintln!("single_image: control renderer, same frame:    {control_first}");
    while counted.get() == (0, 0) && t0.elapsed() < Duration::from_secs(10) {
        std::thread::sleep(Duration::from_millis(5));
    }
    let (redraws, relayouts) = counted.get();
    eprintln!(
        "single_image: {:?} after the frame the image worker told the control shell: \
         request_redraw x{redraws}, invalidate_layout x{relayouts}; \
         graphix's renderer has Shell::headless(), it told nobody",
        t0.elapsed()
    );
    std::thread::sleep(Duration::from_millis(500));
    let forced = frame(&mut w.widget, &mut graphix_r, &mut graphix_cache, &vp);
    eprintln!("single_image: the frame only input would cause: {forced}");
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(first.px[0], 0, "the first frame cannot show the image yet");
    assert!(first.redraw.is_none(), "the first frame asks for no further render");
    assert_eq!(relayouts, 1, "iced_wgpu announces the finished load through the shell");
    assert!(forced.px[0] > 0, "the image had loaded and only needed a render");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn slideshow() -> Result<()> {
    let dir = scratch_dir("slideshow")?;
    let mut paths = Vec::new();
    for (name, rgb) in COLORS {
        paths.push(format!("\"{}\"", png(&dir, &format!("show_{name}"), rgb)?.display()));
    }
    let code = format!(
        "use gui::image::image;\n\
         let paths = [{}];\n\
         let i = 0;\n\
         let clock = sys::time::timer(duration:1500.ms, true);\n\
         i <- clock ~ (i + 1) % 3;\n\
         let src = paths[i]$;\n\
         let result = image(&src)",
        paths.join(", ")
    );
    let mut w = compile(code).await?;
    let gpu = gpu().await;
    let vp = viewport();
    let mut r = gpu.create_renderer();
    let mut cache = Default::default();
    let rt = tokio::runtime::Handle::current();
    let t0 = Instant::now();
    let mut shown = Vec::new();
    let f = frame(&mut w.widget, &mut r, &mut cache, &vp);
    eprintln!("slideshow: {:>6.0?} window appears:   {f}", t0.elapsed());
    shown.push(f);
    while shown.len() < 6 {
        let mut batch = tokio::time::timeout(Duration::from_secs(5), w.rx.recv())
            .await
            .context("timeout waiting for a swap")?
            .context("event channel closed")?;
        let mut changed = false;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                changed |= w.widget.handle_update(&rt, id, &v)?;
            }
        }
        if changed {
            let f = frame(&mut w.widget, &mut r, &mut cache, &vp);
            eprintln!("slideshow: {:>6.0?} source swapped:   {f}", t0.elapsed());
            shown.push(f);
        }
    }
    tokio::time::sleep(Duration::from_millis(700)).await;
    let forced = frame(&mut w.widget, &mut r, &mut cache, &vp);
    eprintln!("slideshow: {:>6.0?} input-caused frame: {forced}", t0.elapsed());
    let _ = std::fs::remove_dir_all(&dir);
    assert!(forced.px.iter().any(|&n| n > 0), "the current image had loaded");
    let blank = shown.iter().filter(|f| f.px.iter().all(|&n| n == 0)).count();
    assert_eq!(
        blank,
        0,
        "{blank} of {} frames the event loop rendered showed no image; no frame \
         followed any load",
        shown.len()
    );
    Ok(())
}
