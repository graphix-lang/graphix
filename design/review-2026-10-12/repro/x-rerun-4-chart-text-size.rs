//! x-rerun-4 (variant of gui-widgets-a-02): a chart's `title_size` or a
//! mesh / legend `label_size` of 0 reaches cosmic-text unvalidated
//! (chart/draw.rs text_style -> plotters_backend.rs draw_text ->
//! canvas::Text size), where the canvas `Text` shape's size is a
//! TextSizeV.
//!
//! Command (with this file at
//! stdlib/graphix-package-gui/tests/review_x_rerun_4_chart_text_size.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_x_rerun_4_chart_text_size -- --nocapture
//!
//! Each case compiles a chart, renders one frame through crate::frame::frame
//! and presents it to a texture, in a child process of its own.
//! Expected: every case renders (a bad size is refused or draws nothing).

use ahash::AHashMap;
use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing;
use graphix_package_gui::{theme::GraphixTheme, widgets};
use graphix_rt::NoExt;
use iced_core::{Color, Font, Pixels, Size, clipboard, mouse};
use iced_runtime::user_interface::Cache;
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use std::{
    process::{Command, Stdio},
    time::{Duration, Instant},
};
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
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

async fn run(args: &str) -> Result<()> {
    let code = format!(
        "use gui::*;\nuse gui::chart::{{self, *}};\nlet result = chart({args})"
    );
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(code.as_str())),
    )]);
    let ctx = testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
    let v = testing::next_update(
        &mut rx,
        compiled.exprs[0].id,
        tokio::time::Instant::now() + Duration::from_secs(10),
    )
    .await?;
    let widget = widgets::compile(gx.clone(), v).await.context("widget")?;
    let mut renderer = renderer().await;
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    graphix_package_gui::frame::frame(
        widget.view(),
        Size::new(400.0, 300.0),
        &mut Cache::default(),
        &mut renderer,
        &mut Vec::new(),
        &[],
        mouse::Cursor::Unavailable,
        &mut clipboard::Null,
        &mut Vec::new(),
        &theme,
    );
    let _ = renderer
        .screenshot(&Viewport::with_physical_size(Size::new(400, 300), 1.0), Color::BLACK);
    eprintln!("CASE: frame rendered");
    Ok(())
}

const DATA: &str = "&[chart::line(#label: \"a\", &[(0.0, 1.0), (1.0, 2.0)])]";

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_title_ok() -> Result<()> {
    run(&format!(
        "#title: &\"T\", #style: &chart_style(#title_size: 16.0), {DATA}"
    ))
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_title_zero() -> Result<()> {
    run(&format!("#title: &\"T\", #style: &chart_style(#title_size: 0.0), {DATA}")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_title_neg() -> Result<()> {
    run(&format!("#title: &\"T\", #style: &chart_style(#title_size: -4.0), {DATA}")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_mesh_label_zero() -> Result<()> {
    run(&format!(
        "#style: &chart_style(#mesh: mesh_style(#label_size: 0.0)), {DATA}"
    ))
    .await
}

const CASES: &[&str] =
    &["case_title_ok", "case_title_zero", "case_title_neg", "case_mesh_label_zero"];

#[test]
fn chart_text_sizes_render() {
    let exe = std::env::current_exe().unwrap();
    let mut failed = Vec::new();
    for case in CASES {
        let mut child = Command::new(&exe)
            .args(["--exact", case, "--include-ignored", "--nocapture", "--test-threads=1"])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn child");
        let deadline = Instant::now() + Duration::from_secs(60);
        while child.try_wait().unwrap().is_none() && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(50));
        }
        if child.try_wait().unwrap().is_none() {
            eprintln!("{case}: HUNG (killed after 60 s)");
            let _ = child.kill();
        }
        let out = child.wait_with_output().unwrap();
        let stderr = String::from_utf8_lossy(&out.stderr);
        let frames = stderr.matches("CASE: ").count();
        let panic: Vec<&str> = stderr
            .lines()
            .skip_while(|l| !l.contains("panicked at"))
            .take(2)
            .collect();
        eprintln!(
            "{case}: exit {:?}, frames {frames}{}",
            out.status.code(),
            if panic.is_empty() { String::new() } else { format!("\n    {}", panic.join("\n    ")) }
        );
        if !out.status.success() {
            if panic.is_empty() {
                eprintln!("--- stderr\n{stderr}");
            }
            failed.push(*case);
        }
    }
    assert!(failed.is_empty(), "cases that did not render: {failed:?}");
}
