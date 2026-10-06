//! gui-chart-08: Surface z lookup assumes an ascending rectilinear grid; a
//! grid whose rows run in descending x (or whose columns run in descending y)
//! is drawn with the first row's (column's) z everywhere.
//!
//! stdlib/graphix-package-gui/src/widgets/chart/draw.rs:975-1010 answers
//! SurfaceSeries::xoz's (x, y) callbacks with `binary_search_by(..)
//! .unwrap_or(0)` over `x_vals` (each row's first x) and `y_vals` (row 0's
//! ys). Over a descending slice the search returns Err, so the lookup reads
//! index 0: row 0 (column 0).
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_chart_08.rs:
//!   GC08_OUT=<dir> timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_chart_08 -- --nocapture
//! GC08_OUT is optional: when set, every screenshot is saved there as a PNG.
//!
//! For each family the test draws the real chart widget through a headless
//! wgpu renderer (`UserInterface::build` + `draw`, as
//! `GuiHandler::about_to_wait` does), all three axis ranges fixed, and reads
//! each frame back:
//!   control    the grid with both axes ascending (drawn correctly),
//!   reordered  the same points with one axis listed descending,
//!   predicted  the reordered grid with every z replaced by the first row's
//!              (column's) z: what the lookup draws,
//! then compares the screenshots pixel by pixel.
//!
//! Expected: `reordered` draws the surface `control` draws, so it differs
//! from `predicted` about as much as `control` does.
//! Observed at c722befe (dev profile, 480x360 frames):
//!   == ridge (rows x = 2, 1, 0 with z = 5, 9, 1, the finding's case)
//!     surface footprint (control vs axes only): 18772 px
//!     control   vs predicted: 18062 px differ
//!     reordered vs predicted: 0 px differ
//!     reordered vs control:   18062 px differ
//!   == book_paraboloid (book/src/examples/gui/chart_3d.gx, rows x = 4 .. -4)
//!     surface footprint (control vs axes only): 35577 px
//!     control   vs predicted: 47780 px differ
//!     reordered vs predicted: 0 px differ
//!     reordered vs control:   47780 px differ
//!   == descending_y (columns y = 2, 1, 0 with z = 1, 5, 9)
//!     surface footprint (control vs axes only): 38934 px
//!     control   vs predicted: 36391 px differ
//!     reordered vs predicted: 0 px differ
//!     reordered vs control:   36391 px differ
//!   panicked: surfaces drawn pixel-identical to the first row's (column's) z
//!     everywhere: ["ridge", "book_paraboloid", "descending_y"]
//! In the saved PNGs the reordered ridge is a flat plane at z = 5, the
//! reordered paraboloid is the x = 4 row's profile extruded along x, and the
//! descending-y grid is a flat plane at z = 1.

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

const W: u32 = 480;
const H: u32 = 360;

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

/// Axis ranges (x, y, z), fixed so that charts of one family share their
/// axes and differ only in the surface.
type Ranges = ((f64, f64), (f64, f64), (f64, f64));

async fn surface_chart(grid: &str, r: Ranges) -> Result<Chart> {
    let ((x0, x1), (y0, y1), (z0, z1)) = r;
    let code = format!(
        "use gui::chart::{{chart, surface}};\n\
         let grid: Array<Array<(f64, f64, f64)>> = {grid};\n\
         let result = chart(\n\
             #x_range: &{{min: {x0:?}, max: {x1:?}}},\n\
             #y_range: &{{min: {y0:?}, max: {y1:?}}},\n\
             #z_range: &{{min: {z0:?}, max: {z1:?}}},\n\
             &[surface(&grid)]\n\
         )"
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
                tokio::task::block_in_place(|| widget.handle_update(&rt, i, &v))?;
            }
        }
    }
    Ok(Chart { _ctx: ctx, _compiled: compiled, widget })
}

/// One frame of the chart as `GuiHandler::about_to_wait` draws it, read
/// back as RGBA from a fresh headless renderer.
async fn shot(grid: &str, r: Ranges) -> Result<Vec<u8>> {
    let chart = surface_chart(grid, r).await?;
    let mut renderer = renderer().await;
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    let mut ui = UserInterface::build(
        chart.widget.view(),
        Size::new(W as f32, H as f32),
        Cache::default(),
        &mut renderer,
    );
    ui.draw(&mut renderer, &theme, &style, mouse::Cursor::Unavailable);
    drop(ui);
    Ok(renderer
        .screenshot(&Viewport::with_physical_size(Size::new(W, H), 1.0), Color::WHITE))
}

fn differing(a: &[u8], b: &[u8]) -> usize {
    assert_eq!(a.len(), b.len());
    a.chunks(4).zip(b.chunks(4)).filter(|(p, q)| p != q).count()
}

fn save(name: &str, rgba: &[u8]) {
    if let Some(dir) = std::env::var_os("GC08_OUT") {
        let path = std::path::Path::new(&dir).join(format!("{name}.png"));
        let img = image::RgbaImage::from_raw(W, H, rgba.to_vec()).expect("rgba size");
        img.save(&path).expect("save png");
        println!("    saved {}", path.display());
    }
}

struct Family {
    name: &'static str,
    ranges: Ranges,
    /// The rows in ascending order on both axes: drawn correctly.
    control: &'static str,
    /// The same points, one axis listed in descending order.
    reordered: &'static str,
    /// What `reordered` is drawn as if every lookup lands on index 0 of
    /// the descending axis: the first row's (or column's) z everywhere.
    predicted: &'static str,
}

const FAMILIES: &[Family] = &[
    Family {
        name: "ridge",
        ranges: ((-0.5, 2.5), (-0.5, 1.5), (0.0, 10.0)),
        control: "[[(0.0, 0.0, 1.0), (0.0, 1.0, 1.0)], \
                   [(1.0, 0.0, 9.0), (1.0, 1.0, 9.0)], \
                   [(2.0, 0.0, 5.0), (2.0, 1.0, 5.0)]]",
        reordered: "[[(2.0, 0.0, 5.0), (2.0, 1.0, 5.0)], \
                     [(1.0, 0.0, 9.0), (1.0, 1.0, 9.0)], \
                     [(0.0, 0.0, 1.0), (0.0, 1.0, 1.0)]]",
        predicted: "[[(2.0, 0.0, 5.0), (2.0, 1.0, 5.0)], \
                     [(1.0, 0.0, 5.0), (1.0, 1.0, 5.0)], \
                     [(0.0, 0.0, 5.0), (0.0, 1.0, 5.0)]]",
    },
    Family {
        name: "book_paraboloid",
        ranges: ((-4.5, 4.5), (-4.5, 4.5), (0.0, 35.0)),
        control: "[[(-4., -4., 32.), (-4., -2., 20.), (-4., 0., 16.), (-4., 2., 20.), (-4., 4., 32.)], \
                   [(-2., -4., 20.), (-2., -2., 8.),  (-2., 0., 4.),  (-2., 2., 8.),  (-2., 4., 20.)], \
                   [(0.,  -4., 16.), (0.,  -2., 4.),  (0.,  0., 0.),  (0.,  2., 4.),  (0.,  4., 16.)], \
                   [(2.,  -4., 20.), (2.,  -2., 8.),  (2.,  0., 4.),  (2.,  2., 8.),  (2.,  4., 20.)], \
                   [(4.,  -4., 32.), (4.,  -2., 20.), (4.,  0., 16.), (4.,  2., 20.), (4.,  4., 32.)]]",
        reordered: "[[(4.,  -4., 32.), (4.,  -2., 20.), (4.,  0., 16.), (4.,  2., 20.), (4.,  4., 32.)], \
                     [(2.,  -4., 20.), (2.,  -2., 8.),  (2.,  0., 4.),  (2.,  2., 8.),  (2.,  4., 20.)], \
                     [(0.,  -4., 16.), (0.,  -2., 4.),  (0.,  0., 0.),  (0.,  2., 4.),  (0.,  4., 16.)], \
                     [(-2., -4., 20.), (-2., -2., 8.),  (-2., 0., 4.),  (-2., 2., 8.),  (-2., 4., 20.)], \
                     [(-4., -4., 32.), (-4., -2., 20.), (-4., 0., 16.), (-4., 2., 20.), (-4., 4., 32.)]]",
        predicted: "[[(4.,  -4., 32.), (4.,  -2., 20.), (4.,  0., 16.), (4.,  2., 20.), (4.,  4., 32.)], \
                     [(2.,  -4., 32.), (2.,  -2., 20.), (2.,  0., 16.), (2.,  2., 20.), (2.,  4., 32.)], \
                     [(0.,  -4., 32.), (0.,  -2., 20.), (0.,  0., 16.), (0.,  2., 20.), (0.,  4., 32.)], \
                     [(-2., -4., 32.), (-2., -2., 20.), (-2., 0., 16.), (-2., 2., 20.), (-2., 4., 32.)], \
                     [(-4., -4., 32.), (-4., -2., 20.), (-4., 0., 16.), (-4., 2., 20.), (-4., 4., 32.)]]",
    },
    Family {
        name: "descending_y",
        ranges: ((-0.5, 2.5), (-0.5, 2.5), (0.0, 10.0)),
        control: "[[(0.0, 0.0, 9.0), (0.0, 1.0, 5.0), (0.0, 2.0, 1.0)], \
                   [(1.0, 0.0, 9.0), (1.0, 1.0, 5.0), (1.0, 2.0, 1.0)], \
                   [(2.0, 0.0, 9.0), (2.0, 1.0, 5.0), (2.0, 2.0, 1.0)]]",
        reordered: "[[(0.0, 2.0, 1.0), (0.0, 1.0, 5.0), (0.0, 0.0, 9.0)], \
                     [(1.0, 2.0, 1.0), (1.0, 1.0, 5.0), (1.0, 0.0, 9.0)], \
                     [(2.0, 2.0, 1.0), (2.0, 1.0, 5.0), (2.0, 0.0, 9.0)]]",
        predicted: "[[(0.0, 2.0, 1.0), (0.0, 1.0, 1.0), (0.0, 0.0, 1.0)], \
                     [(1.0, 2.0, 1.0), (1.0, 1.0, 1.0), (1.0, 0.0, 1.0)], \
                     [(2.0, 2.0, 1.0), (2.0, 1.0, 1.0), (2.0, 0.0, 1.0)]]",
    },
];

#[tokio::test(flavor = "multi_thread")]
async fn descending_surface_draws_its_data() -> Result<()> {
    let mut wrong = Vec::new();
    for f in FAMILIES {
        println!("== {}", f.name);
        let axes = shot("[[]]", f.ranges).await?;
        let control = shot(f.control, f.ranges).await?;
        let reordered = shot(f.reordered, f.ranges).await?;
        let predicted = shot(f.predicted, f.ranges).await?;
        save(&format!("{}_axes", f.name), &axes);
        save(&format!("{}_control", f.name), &control);
        save(&format!("{}_reordered", f.name), &reordered);
        save(&format!("{}_predicted", f.name), &predicted);
        let cvp = differing(&control, &predicted);
        let rvp = differing(&reordered, &predicted);
        let rvc = differing(&reordered, &control);
        println!("    surface footprint (control vs axes only): {} px", differing(&control, &axes));
        println!("    control   vs predicted: {cvp} px differ");
        println!("    reordered vs predicted: {rvp} px differ");
        println!("    reordered vs control:   {rvc} px differ");
        assert!(cvp > 0, "{}: the comparison cannot tell the surfaces apart", f.name);
        if rvp == 0 {
            wrong.push(f.name);
        }
    }
    assert!(
        wrong.is_empty(),
        "surfaces drawn pixel-identical to the first row's (column's) z everywhere: {wrong:?}"
    );
    Ok(())
}
