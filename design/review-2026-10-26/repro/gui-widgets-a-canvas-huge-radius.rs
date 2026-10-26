//! gui-widgets-a: a canvas tessellates every shape whole, unclipped, at a
//! fixed tolerance, so a shape's cost grows with its size, not with what
//! is visible: a filled circle of radius 1e12 in a 300 x 200 canvas takes
//! about a second and a gigabyte to draw, and each 100x of radius costs
//! 10x more (radius 1e14 or more exhausts memory).
//!
//! Expected: a shape far larger than the canvas draws in time and memory
//! bounded by the canvas, not by the shape.
//! Observed at e6ffc6eed (debug test profile), test `canvas_radius_cost`:
//!   PROBE radius 1e8: first frame 10.252841ms VmHWM:   494572 kB
//!   PROBE radius 1e10: first frame 93.596436ms VmHWM:  628076 kB
//!   PROBE radius 1e12: first frame 977.468151ms VmHWM: 1629064 kB
//! The other tests (degenerate grid, NaN/huge flex spacing and padding, a
//! 1e38 stroke width) all pass: no layout panic there.
//!
//! To run, copy this file to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_1.rs, then:
//!   flock ~/tmp/cargo.lock timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_1 -- --nocapture
use anyhow::Result;
use graphix_package_core::testing::{Fixture, PackageRef};
use graphix_package_gui::{frame, theme::GraphixTheme, widgets::{self, GuiW, Renderer}};
use graphix_rt::NoExt;
use iced_core::{Size, clipboard, mouse};
use iced_runtime::user_interface;
use iced_wgpu::{graphics::Shell, wgpu};
const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

struct NoClip;

impl clipboard::Clipboard for NoClip {
    fn read(&self, _kind: clipboard::Kind) -> Option<String> {
        None
    }
    fn write(&mut self, _kind: clipboard::Kind, _contents: String) {}
}

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
            .expect("no adapter"),
    };
    let (device, queue) =
        adapter.request_device(&wgpu::DeviceDescriptor::default()).await.unwrap();
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

struct H {
    _f: Fixture,
    widget: GuiW<NoExt>,
    renderer: Renderer,
    cache: user_interface::Cache,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (f, initial) = Fixture::new(code, REGISTER, |_| ()).await?;
        let widget = widgets::compile(f.gx.clone(), initial).await?;
        Ok(Self { _f: f, widget, renderer: renderer().await, cache: user_interface::Cache::default() })
    }

    fn frame(&mut self) {
        let mut messages = vec![];
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        frame::frame(
            &mut self.widget,
            Size::new(300.0, 600.0),
            &mut self.cache,
            &mut self.renderer,
            &[],
            mouse::Cursor::Unavailable,
            &mut NoClip,
            &mut messages,
            &theme,
        );
    }
}

const RED: &str = "color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$";

fn canvas(shapes: &str) -> String {
    format!(
        "use gui::{{color, canvas::canvas}};\n\
         let result = canvas(#width: &`Fill, #height: &`Fixed(200.0), &[{shapes}])"
    )
}

#[tokio::test(flavor = "multi_thread")]
async fn canvas_wide_stroke() -> Result<()> {
    for shape in [
        format!("`Line({{from: {{x: 0.0, y: 0.0}}, to: {{x: 100.0, y: 50.0}}, color: {RED}, width: 1e38}})"),
        format!("`Rect({{top_left: {{x: 0.0, y: 0.0}}, size: {{width: 100.0, height: 50.0}}, fill: null, stroke: {{color: {RED}, width: 1e38}}}})"),
        format!("`Circle({{center: {{x: 0.0, y: 0.0}}, radius: 10.0, fill: null, stroke: {{color: {RED}, width: 1e38}}}})"),
    ] {
        println!("PROBE wide stroke: {shape}");
        let mut h = H::new(&canvas(&shape)).await?;
        h.frame();
        println!("PROBE wide stroke ok");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn canvas_radius_cost() -> Result<()> {
    for r in ["1e8", "1e10", "1e12"] {
        let shape = format!(
            "`Circle({{center: {{x: 0.0, y: 0.0}}, radius: {r}, fill: {RED}, stroke: {{color: {RED}, width: 1.0}}}})"
        );
        let mut h = H::new(&canvas(&shape)).await?;
        let t = std::time::Instant::now();
        h.frame();
        let hwm = std::fs::read_to_string("/proc/self/status").unwrap().lines().find(|l| l.starts_with("VmHWM")).unwrap().to_string();
        println!("PROBE radius {r}: first frame {:?} {hwm}", t.elapsed());
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn grid_degenerate() -> Result<()> {
    for (cols, height, spacing) in [
        ("`Fluid(-1.0)", "`AspectRatio(1.0)", "1.0"),
        ("`Fixed(2)", "`AspectRatio(0.0)", "0.0"),
        ("`Fixed(2)", "`AspectRatio(-1.0)", "0.0"),
        ("`Fixed(2)", "`AspectRatio(1.0)", "0.0 / 0.0"),
    ] {
        println!("PROBE grid {cols} {height} {spacing}");
        let code = format!(
            "use gui::{{grid::grid, text::text, scrollable::scrollable}};\n\
             let result = scrollable(&grid(#columns: &{cols}, #spacing: &({spacing}), #height: &{height}, &[text(&\"a\"), text(&\"b\"), text(&\"c\")]))"
        );
        let mut h = H::new(&code).await?;
        h.frame();
        println!("PROBE grid ok");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn flex_nan() -> Result<()> {
    for code in [
        "use gui::{row::row, text::text};\nlet result = row(#spacing: &(0.0 / 0.0), &[text(&\"a\"), text(&\"b\")])",
        "use gui::{row::row, text::text};\nlet result = row(#spacing: &(-1e30), &[text(&\"a\"), text(&\"b\")])",
        "use gui::{container::container, text::text};\nlet result = container(#padding: &`All(0.0 / 0.0), &text(&\"c\"))",
        "use gui::{container::container, text::text};\nlet result = container(#width: &`Fixed(0.0 / 0.0), &text(&\"c\"))",
        "use gui::{column::column, text::text, scrollable::scrollable};\nlet result = scrollable(&column(#spacing: &1e38, &[text(&\"a\"), text(&\"b\"), text(&\"c\")]))",
    ] {
        println!("PROBE flex: {code}");
        let mut h = H::new(code).await?;
        h.frame();
        println!("PROBE flex ok");
    }
    Ok(())
}
