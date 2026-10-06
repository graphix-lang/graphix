//! gui-core-03: present(None) never paints the theme background; a Light
//! theme gives black text on black.
//!
//! stdlib/graphix-package-gui/src/event_loop.rs:357 presents every frame with
//! `ws.renderer.present(None, ..)`. iced_wgpu 0.14 (lib.rs:434-447) turns a
//! `None` clear color into `LoadOp::Load`, and wgpu-core 27 zero-fills a
//! freshly acquired surface texture on a Load (present.rs:225-235 creates it
//! uninitialized; command/render.rs:929 registers NeedsInitializedMemory), so
//! every frame starts as (0,0,0,0) and only widgets draw on it. iced's own
//! compositor (iced_wgpu window/compositor.rs:231) presents with
//! `Some(background_color)`.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_03.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_03 -- --nocapture
//!
//! The program below is compiled, its root `Array<&Window>` resolved the way
//! `reconcile_windows` does (`compile_ref` per bind id, then
//! `ResolvedWindow::compile`), and each window rendered headless the way
//! `GuiHandler::about_to_wait` does (event_loop.rs:316-357: the crate's own
//! `GpuState::create_renderer`, `UserInterface` build/update/draw with
//! `Style { text_color: theme.palette().text }`, the cache kept through the
//! present), into a fresh texture, once with `present(None, ..)` as graphix
//! does and once with `present(Some(Base::base(theme).background_color), ..)`
//! as iced does. `ink` counts pixels whose RGB differs from the top-left
//! corner pixel (what an opaque surface shows), `coverage` pixels whose alpha
//! does.
//!
//! Expected: each graphix frame's corner is the theme background and its text
//! is visible (ink > 0), as in the iced column.
//! Observed at c722befe (dev profile, Intel Arc B390, Vulkan); the test fails:
//!   Light           theme bg [255, 255, 255] text [0, 0, 0] | graphix present(None): corner [0, 0, 0, 0] ink    0 coverage  964 | iced present(Some(bg)): corner [255, 255, 255, 255] ink  964
//!   Dark            theme bg [43, 45, 49] text [230, 230, 230] | graphix present(None): corner [0, 0, 0, 0] ink  964 coverage  964 | iced present(Some(bg)): corner [43, 45, 49, 255] ink  964
//!   CatppuccinMocha theme bg [30, 30, 46] text [205, 214, 244] | graphix present(None): corner [0, 0, 0, 0] ink  964 coverage  964 | iced present(Some(bg)): corner [30, 30, 46, 255] ink  964
//!   CustomPalette   theme bg [242, 242, 230] text [26, 26, 26] | graphix present(None): corner [0, 0, 0, 0] ink  954 coverage  964 | iced present(Some(bg)): corner [242, 242, 230, 255] ink  963
//! Every window is black instead of its theme background; under Light the
//! text is drawn (coverage 964) in black on black (ink 0).

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef};
use graphix_package_gui::{render::GpuState, widgets::Message, window::ResolvedWindow};
use graphix_rt::{GXEvent, NoExt};
use iced_core::{Color, Point, clipboard, mouse, renderer::Style, theme::Base};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Viewport, wgpu};
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

const PROGRAM: &str = r#"
use gui::{color, window, text::text};

let light_palette = {
  background: color(#r: 0.95, #g: 0.95, #b: 0.9)$,
  text: color(#r: 0.1, #g: 0.1, #b: 0.1)$,
  primary: color(#r: 0.2, #g: 0.4, #b: 0.9)$,
  success: color(#r: 0.2, #g: 0.7, #b: 0.3)$,
  danger: color(#r: 0.9, #g: 0.2, #b: 0.2)$,
  warning: color(#r: 0.9, #g: 0.7, #b: 0.1)$
};

let result = [
  &window(#title: &"Light", #theme: &`Light, &text(#size: &40.0, &"hello")),
  &window(#title: &"Dark", #theme: &`Dark, &text(#size: &40.0, &"hello")),
  &window(#title: &"CatppuccinMocha", #theme: &`CatppuccinMocha, &text(#size: &40.0, &"hello")),
  &window(#title: &"CustomPalette", #theme: &`CustomPalette(light_palette), &text(#size: &40.0, &"hello"))
]
"#;

const W: u32 = 320;
const H: u32 = 120;
const FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8UnormSrgb;

async fn gpu() -> GpuState {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY),
        ..Default::default()
    });
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: None,
            force_fallback_adapter: false,
            ..Default::default()
        })
        .await
        .expect("no GPU adapter available");
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("failed to create GPU device");
    GpuState { instance, adapter, device, queue, format: FORMAT }
}

fn read_back(gpu: &GpuState, texture: &wgpu::Texture) -> Vec<u8> {
    let unpadded = W as usize * 4;
    let align = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT as usize;
    let padded = unpadded.div_ceil(align) * align;
    let buffer = gpu.device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("readback"),
        size: (padded * H as usize) as u64,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });
    let mut encoder =
        gpu.device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
    encoder.copy_texture_to_buffer(
        texture.as_image_copy(),
        wgpu::TexelCopyBufferInfo {
            buffer: &buffer,
            layout: wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(padded as u32),
                rows_per_image: None,
            },
        },
        wgpu::Extent3d { width: W, height: H, depth_or_array_layers: 1 },
    );
    let index = gpu.queue.submit([encoder.finish()]);
    let slice = buffer.slice(..);
    slice.map_async(wgpu::MapMode::Read, |r| r.expect("map readback buffer"));
    gpu.device
        .poll(wgpu::PollType::Wait { submission_index: Some(index), timeout: None })
        .expect("poll");
    let mapped = slice.get_mapped_range();
    let mut out = Vec::with_capacity(unpadded * H as usize);
    for row in mapped.chunks(padded) {
        out.extend_from_slice(&row[..unpadded]);
    }
    out
}

/// One frame as event_loop.rs:316-357 renders it, into a fresh texture.
fn frame(
    gpu: &GpuState,
    win: &mut ResolvedWindow<NoExt>,
    clear: impl Fn(&graphix_package_gui::theme::GraphixTheme) -> Option<Color>,
) -> Vec<u8> {
    let mut renderer = gpu.create_renderer();
    let viewport = Viewport::with_physical_size(iced_core::Size::new(W, H), 1.0);
    win.content.before_view();
    let element = win.content.view();
    let mut ui = UserInterface::build(
        element,
        viewport.logical_size(),
        Cache::default(),
        &mut renderer,
    );
    let cursor = mouse::Cursor::Available(Point::ORIGIN);
    let mut messages: Vec<Message> = Vec::new();
    let _ = ui.update(&[], cursor, &mut renderer, &mut clipboard::Null, &mut messages);
    // TrackedWindow::iced_theme
    let theme = win.theme.t.as_ref().map(|t| t.0.clone()).expect("window theme");
    let style = Style { text_color: theme.palette().text };
    ui.draw(&mut renderer, &theme, &style, cursor);
    // event_loop.rs:349 keeps the cache (the widget tree, which owns the
    // paragraphs the renderer recorded weakly) alive through present
    let _cache = ui.into_cache();
    let texture = gpu.device.create_texture(&wgpu::TextureDescriptor {
        label: Some("frame"),
        size: wgpu::Extent3d { width: W, height: H, depth_or_array_layers: 1 },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: FORMAT,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    });
    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    renderer.present(clear(&theme), FORMAT, &view, &viewport);
    read_back(gpu, &texture)
}

struct Summary {
    corner: [u8; 4],
    ink: usize,
    coverage: usize,
}

/// `ink`: pixels whose RGB differs from the top-left corner (what an opaque
/// surface shows); `coverage`: pixels whose alpha differs from it.
fn summarize(px: &[u8]) -> Summary {
    let corner = [px[0], px[1], px[2], px[3]];
    let ink = px.chunks(4).filter(|p| p[..3] != corner[..3]).count();
    let coverage = px.chunks(4).filter(|p| p[3] != corner[3]).count();
    Summary { corner, ink, coverage }
}

fn rgb8(c: Color) -> [u8; 3] {
    let [r, g, b, _] = c.into_rgba8();
    [r, g, b]
}

#[tokio::test(flavor = "current_thread")]
async fn present_none_paints_theme_background() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let vfs = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(arcstr::ArcStr::from(PROGRAM)),
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
        let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
            .await
            .context("timeout waiting for the root value")?
            .context("event channel closed")?;
        let found = batch.drain(..).find_map(|e| match e {
            GXEvent::Updated(i, v) if i == id => Some(v),
            _ => None,
        });
        if let Some(v) = found {
            break v;
        }
    };
    // reconcile_windows
    let bids = root.cast_to::<Vec<u64>>().context("root array of bind ids")?;
    let mut windows = Vec::new();
    for bid in bids {
        let wref = ctx.rt.compile_ref(bid).await?;
        let value = wref.last.clone().context("window bind has no value")?;
        let resolved = ResolvedWindow::compile(ctx.rt.clone(), value).await?;
        windows.push((wref, resolved));
    }
    let gpu = gpu().await;
    eprintln!("RESULT adapter: {:?}", gpu.adapter.get_info().name);
    let mut wrong = Vec::new();
    for (_wref, win) in windows.iter_mut() {
        let title = win.title.t.clone().unwrap_or_default();
        let theme = win.theme.t.as_ref().map(|t| t.0.clone()).expect("theme");
        let bg = Base::base(&theme.inner).background_color;
        let graphix = summarize(&frame(&gpu, win, |_| None));
        let iced = summarize(&frame(&gpu, win, |t| Some(Base::base(&t.inner).background_color)));
        eprintln!(
            "RESULT {title:15} theme bg {:?} text {:?} | graphix present(None): corner {:?} ink {:4} coverage {:4} | iced present(Some(bg)): corner {:?} ink {:4}",
            rgb8(bg),
            rgb8(theme.palette().text),
            graphix.corner,
            graphix.ink,
            graphix.coverage,
            iced.corner,
            iced.ink,
        );
        if graphix.corner[..3] != iced.corner[..3] || graphix.ink == 0 {
            wrong.push(title);
        }
    }
    assert!(
        wrong.is_empty(),
        "windows rendered without their theme background (or with invisible text): {wrong:?}"
    );
    Ok(())
}
