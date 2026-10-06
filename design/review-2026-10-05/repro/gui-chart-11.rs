//! gui-chart-11: Pie `donut` is documented as a fraction of the radius but
//! passed as pixels (and `label_offset`, documented as a percentage, is
//! pixels too).
//!
//! The book (book/src/ui/gui/chart.md, PieStyle) says `donut` is the inner
//! radius as a fraction of the outer radius (0.0-1.0) and `label_offset` the
//! labels' distance from the center as a percentage. draw.rs:777-785 hands
//! both to plotters 0.3.7 unscaled: `Pie::donut_hole` takes a hole radius in
//! pixels (ignored unless 0 < hole < radius) and `label_offset` is pixels
//! added to the outer radius.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_11.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_11 -- --nocapture
//!
//! It compiles `chart(&[pie(<args>&data)])`, draws the widget through a
//! headless wgpu renderer as `GuiHandler::about_to_wait` does
//! (`UserInterface::build` + `ui.draw`), reads the frame back with
//! `Renderer::screenshot`, and measures the pie along rays from its center.
//!
//! Expected (per the book): `#donut: 0.5` leaves a hole of half the outer
//! radius (hole/outer 0.5 at every size); `#label_offset` scales with the
//! pie.
//! Observed at c722befe (dev profile):
//!   donut: hole and outer radius in px (median over 8 rays)
//!     400x300 pie(&data): hole 0.00 px, outer 105.00 px, hole/outer 0.000, center pixel Some((31, 119, 180))
//!     400x300 pie(#donut: 0.5, &data): hole 0.00 px, outer 105.00 px, hole/outer 0.000, center pixel Some((31, 119, 180))
//!     400x300 pie(#donut: 0.9, &data): hole 1.00 px, outer 105.00 px, hole/outer 0.010, center pixel Some((255, 255, 255))
//!     400x300 pie(#donut: 52.0, &data): hole 52.75 px, outer 105.00 px, hole/outer 0.502, center pixel Some((255, 255, 255))
//!     800x600 pie(&data): hole 0.00 px, outer 209.75 px, hole/outer 0.000, center pixel Some((31, 119, 180))
//!     800x600 pie(#donut: 0.5, &data): hole 0.00 px, outer 209.75 px, hole/outer 0.000, center pixel Some((31, 119, 180))
//!     800x600 pie(#donut: 52.0, &data): hole 52.75 px, outer 209.75 px, hole/outer 0.251, center pixel Some((255, 255, 255))
//!   label_offset: gap from the pie's edge to the label's right end
//!     400x300 pie(&data): outer 105.00 px, label gap Some(12.0) px
//!     400x300 pie(#label_offset: 0.0, &data): outer 105.00 px, label gap Some(7.0) px
//!     400x300 pie(#label_offset: 50.0, &data): outer 105.00 px, label gap Some(57.0) px
//!     800x600 pie(&data): outer 210.75 px, label gap Some(16.25) px
//!     800x600 pie(#label_offset: 0.0, &data): outer 210.75 px, label gap Some(6.25) px
//!     800x600 pie(#label_offset: 50.0, &data): outer 210.75 px, label gap Some(56.25) px
//! So `#donut: 0.5` draws the same full pie as no donut, the whole
//! documented range gives at most a 1 px hole, `#donut: 52.0` is a 52 px
//! hole whatever the pie's size, and `#label_offset: 50.0` moves the labels
//! 50 px out from the edge at both sizes.

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Color, Size, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
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
            .expect("no GPU adapter available"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("failed to create GPU device");
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

struct Chart {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    widget: GuiW<NoExt>,
}

async fn pie_chart(data: &str, args: &str) -> Result<Chart> {
    let code = format!(
        "use gui::chart::{{chart, pie}};\n\
         let data = {data};\n\
         let result = chart(&[pie({args}&data)])"
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
        let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
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
    Ok(Chart { _ctx: ctx, _compiled: compiled, widget })
}

struct Shot {
    w: u32,
    h: u32,
    px: Vec<u8>,
}

impl Shot {
    fn rgb(&self, x: i32, y: i32) -> Option<(u8, u8, u8)> {
        if x < 0 || y < 0 || x as u32 >= self.w || y as u32 >= self.h {
            return None;
        }
        let i = ((y as u32 * self.w + x as u32) * 4) as usize;
        Some((self.px[i], self.px[i + 1], self.px[i + 2]))
    }

    /// The chart fills its frame white before it draws the pie.
    fn is_bg(&self, x: i32, y: i32) -> bool {
        match self.rgb(x, y) {
            Some((r, g, b)) => r >= 240 && g >= 240 && b >= 240,
            None => true,
        }
    }
}

/// One frame, drawn the way `GuiHandler::about_to_wait` draws it, read back
/// offscreen instead of presented.
fn shoot(widget: &GuiW<NoExt>, r: &mut widgets::Renderer, w: u32, h: u32) -> Shot {
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    let mut ui = UserInterface::build(
        widget.view(),
        Size::new(w as f32, h as f32),
        Cache::default(),
        r,
    );
    ui.draw(r, &theme, &style, mouse::Cursor::Unavailable);
    drop(ui);
    let px = r.screenshot(&Viewport::with_physical_size(Size::new(w, h), 1.0), Color::BLACK);
    Shot { w, h, px }
}

/// Along rays from the pie's center: the distance to the first pie pixel
/// (the hole's radius) and to the first background pixel past it (the outer
/// radius). The rays avoid the seams of a 1:2 pie that starts at 0 degrees.
fn radii(s: &Shot) -> (f64, f64) {
    let (cx, cy) = ((s.w / 2) as f64, (s.h / 2) as f64);
    let mut holes = Vec::new();
    let mut outers = Vec::new();
    for deg in [30.0f64, 60.0, 90.0, 150.0, 200.0, 250.0, 300.0, 330.0] {
        let (sin, cos) = deg.to_radians().sin_cos();
        let bg_at = |t: f64| {
            s.is_bg((cx + t * cos).round() as i32, (cy + t * sin).round() as i32)
        };
        let mut t = 0.0;
        while t < 1000.0 && bg_at(t) {
            t += 0.25;
        }
        let hole = t;
        while t < 1000.0 && !bg_at(t) {
            t += 0.25;
        }
        holes.push(hole);
        outers.push(t);
    }
    holes.sort_by(f64::total_cmp);
    outers.sort_by(f64::total_cmp);
    (holes[holes.len() / 2], outers[outers.len() / 2])
}

/// The rightmost label pixel left of the pie disc, as a gap from the disc's
/// edge: a one-slice pie puts its label at 180 degrees, right-aligned to
/// `radius + label_offset` left of the center.
fn label_gap(s: &Shot, outer: f64) -> Option<f64> {
    let (cx, cy) = ((s.w / 2) as i32, (s.h / 2) as i32);
    let limit = cx - outer as i32 - 2;
    let mut best: Option<i32> = None;
    for y in cy - 30..=cy + 30 {
        for x in 0..limit {
            if !s.is_bg(x, y) {
                best = Some(best.map_or(x, |b| b.max(x)));
            }
        }
    }
    best.map(|x| (cx - x) as f64 - outer)
}

#[tokio::test(flavor = "current_thread")]
async fn pie_donut_and_label_offset_units() -> Result<()> {
    let mut r = renderer().await;
    let two = r#"[("a", 1.0), ("b", 2.0)]"#;
    println!("donut: hole and outer radius in px (median over 8 rays)");
    for (args, w, h) in [
        ("", 400, 300),
        ("#donut: 0.5, ", 400, 300),
        ("#donut: 0.9, ", 400, 300),
        ("#donut: 52.0, ", 400, 300),
        ("", 800, 600),
        ("#donut: 0.5, ", 800, 600),
        ("#donut: 52.0, ", 800, 600),
    ] {
        let c = pie_chart(two, args).await?;
        let s = shoot(&c.widget, &mut r, w, h);
        let (hole, outer) = radii(&s);
        let center = s.rgb((w / 2) as i32, (h / 2) as i32);
        println!(
            "  {w}x{h} pie({args}&data): hole {hole:.2} px, outer {outer:.2} px, \
             hole/outer {:.3}, center pixel {center:?}",
            hole / outer
        );
    }
    let one = r#"[("LABEL", 1.0)]"#;
    println!("label_offset: gap from the pie's edge to the label's right end");
    for (args, w, h) in [
        ("", 400, 300),
        ("#label_offset: 0.0, ", 400, 300),
        ("#label_offset: 50.0, ", 400, 300),
        ("", 800, 600),
        ("#label_offset: 0.0, ", 800, 600),
        ("#label_offset: 50.0, ", 800, 600),
    ] {
        let c = pie_chart(one, args).await?;
        let s = shoot(&c.widget, &mut r, w, h);
        let (_, outer) = radii(&s);
        let gap = label_gap(&s, outer);
        println!("  {w}x{h} pie({args}&data): outer {outer:.2} px, label gap {gap:?} px");
    }
    Ok(())
}
