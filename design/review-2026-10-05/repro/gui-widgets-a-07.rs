//! gui-widgets-a-07: the markdown widget ignores `#spacing`, and draws
//! links in the Dark theme's primary color under every window theme.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_07.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_07 -- --nocapture
//!
//! The program builds four markdown widgets over one three-paragraph
//! document (default, `#spacing: &40.0`, `#spacing: &2.0`,
//! `#text_size: &32.0`) and one markdown widget holding a link. Each is
//! built by `widgets::compile` and laid out with a headless wgpu
//! renderer; the link is drawn under three window themes and read back
//! as pixels, as `GuiHandler::about_to_wait` draws a window.
//!
//! Expected: `#spacing` sets the gap between blocks (book:
//! "Vertical space in pixels between markdown elements"), so 40.0 adds
//! 2 x (40 - 14) = 52 px to the default height and 2.0 removes 24; a
//! link takes the window theme's primary color, as the paragraph's plain
//! text takes the theme's text color.
//! Observed at c722befe:
//!   layout height: default 90.4 | #spacing 40 90.4 | #spacing 2 90.4 |
//!     #text_size 32 180.8
//!   (90.4 = 3 lines x 20.8 + 2 gaps x 14, iced's default 16 x 0.875)
//!   link under CustomPalette (primary 255,0,0): 0 px at the theme's
//!     primary, 343 px at Dark's primary 88,101,242
//!   link under Dracula (primary 189,147,249): 0 px at the theme's
//!     primary, 418 px at Dark's primary
//!   plain text follows each theme (1000+ px at its text color)
//! and the test fails on "#spacing ignored".

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing;
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW},
};
use graphix_rt::{GXEvent, NoExt};
use iced_core::{
    Color, Font, Pixels, Size, layout, mouse, renderer::Style, theme::palette::Palette,
    widget::Tree,
};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const CODE: &str = r#"use gui::markdown::markdown;
let doc = "para one\n\npara two\n\npara three";
let linked = "see the \[link\](https://example.com) here";
let result = (
  markdown(&doc),
  markdown(#spacing: &40.0, &doc),
  markdown(#spacing: &2.0, &doc),
  markdown(#text_size: &32.0, &doc),
  markdown(#text_size: &40.0, &linked)
)"#;

type Rx = mpsc::Receiver<GPooled<Vec<GXEvent>>>;

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

async fn first_value(rx: &mut Rx, target: ExprId) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(10));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev && id == target {
                        return Ok(v);
                    }
                }
            }
            _ = &mut timeout => bail!("no widget value"),
        }
    }
}

/// The height iced lays the widget out at, 400 px wide.
fn height(w: &GuiW<NoExt>, renderer: &widgets::Renderer) -> f32 {
    let mut el = w.view();
    let mut tree = Tree::new(&el);
    let limits = layout::Limits::new(Size::ZERO, Size::new(400.0, 4000.0));
    el.as_widget_mut().layout(&mut tree, renderer, &limits).size().height
}

/// One 600x200 frame under `theme`, read back as sRGB RGBA bytes.
fn shot(w: &GuiW<NoExt>, renderer: &mut widgets::Renderer, theme: &GraphixTheme) -> Vec<u8> {
    let (wd, ht) = (600u32, 200u32);
    let mut ui = UserInterface::build(
        w.view(),
        Size::new(wd as f32, ht as f32),
        Cache::default(),
        renderer,
    );
    let p = theme.palette();
    ui.draw(renderer, theme, &Style { text_color: p.text }, mouse::Cursor::Unavailable);
    // A layer holds its paragraphs weakly: the tree must outlive the read-back.
    let px =
        renderer.screenshot(&Viewport::with_physical_size(Size::new(wd, ht), 1.0), p.background);
    drop(ui);
    px
}

/// Pixels within 24 of `c` on every channel.
fn near(px: &[u8], c: Color) -> usize {
    let [r, g, b, _] = c.into_rgba8();
    let close = |x: u8, y: u8| (x as i32 - y as i32).abs() <= 24;
    px.chunks_exact(4).filter(|p| close(p[0], r) && close(p[1], g) && close(p[2], b)).count()
}

#[tokio::test(flavor = "current_thread")]
async fn markdown_spacing_and_link_color() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(CODE)),
    )]);
    let ctx = testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
    let parts: Vec<Value> = match first_value(&mut rx, compiled.exprs[0].id).await? {
        Value::Array(a) => a.iter().cloned().collect(),
        v => bail!("expected a tuple, got {v}"),
    };
    let mut ws: Vec<GuiW<NoExt>> = Vec::new();
    for p in parts {
        ws.push(widgets::compile(gx.clone(), p).await.context("widget")?);
    }
    let mut renderer = renderer().await;

    let h: Vec<f32> = ws[..4].iter().map(|w| height(w, &renderer)).collect();
    eprintln!(
        "layout height: default {} | #spacing 40 {} | #spacing 2 {} | #text_size 32 {}",
        h[0], h[1], h[2], h[3]
    );

    let custom = GraphixTheme {
        inner: iced_core::Theme::custom(
            "Custom",
            Palette {
                background: Color::WHITE,
                text: Color::BLACK,
                primary: Color::from_rgb(1.0, 0.0, 0.0),
                success: Color::from_rgb(0.0, 0.6, 0.0),
                warning: Color::from_rgb(0.9, 0.9, 0.0),
                danger: Color::from_rgb(0.4, 0.4, 0.4),
            },
        ),
        overrides: None,
    };
    let dracula = GraphixTheme { inner: iced_core::Theme::Dracula, overrides: None };
    let dark = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let dark_primary = dark.palette().primary;
    let mut link = Vec::new();
    for (name, theme) in [("CustomPalette", &custom), ("Dracula", &dracula), ("Dark", &dark)] {
        let px = shot(&ws[4], &mut renderer, theme);
        let p = theme.palette();
        let (n_own, n_dark, n_text) =
            (near(&px, p.primary), near(&px, dark_primary), near(&px, p.text));
        eprintln!(
            "link under {name}: {n_own} px at the theme's primary {:?}, {n_dark} px at \
             Dark's primary {:?}; plain text: {n_text} px at the theme's text color {:?}",
            p.primary.into_rgba8(),
            dark_primary.into_rgba8(),
            p.text.into_rgba8(),
        );
        link.push((name, n_own, n_dark, n_text));
    }

    assert!(h[3] > h[0] + 10.0, "control: #text_size must change the layout");
    for (name, _, _, n_text) in &link {
        assert!(*n_text > 300, "control: plain text must follow {name}");
    }
    assert!(link[2].2 > 50, "control: under Dark the link is drawn in Dark's primary");
    assert!(
        h[1] >= h[0] + 50.0 && h[2] < h[0],
        "#spacing ignored: heights {h:?} (default, 40, 2, text_size 32)"
    );
    for (name, n_own, n_dark, _) in &link[..2] {
        assert!(
            *n_own > 50 && *n_dark == 0,
            "link under {name} drawn in Dark's primary: {n_own} px at the theme's, \
             {n_dark} at Dark's"
        );
    }
    Ok(())
}
