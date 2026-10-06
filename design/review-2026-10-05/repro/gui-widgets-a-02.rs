//! gui-widgets-a-02: a text size of 0 panics cosmic-text and kills the
//! GUI (markdown `#text_size`, text `#size`, canvas `` `Text `` size,
//! also text_input/text_editor `#size`); a negative size hangs layout.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_02.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_02 -- --nocapture
//!
//! Each case renders one widget headlessly the way the event loop does
//! (UserInterface::build, update, draw, present to a texture), in a
//! child process of its own: the panic poisons iced's global font
//! system lock, which would fail every later case in a shared process.
//!
//! Expected: every case renders (a bad size draws nothing, or is
//! refused with a logged error); the driver test passes.
//! Observed at c722befe (driver FAILS):
//!   *_ok (size 16.0)          exit 0, 2 frames
//!   md_zero, text_zero, text_input_zero
//!                             exit 101 at layout: cosmic-text-0.15.0
//!                             buffer.rs:253 "line height cannot be 0"
//!   md_drop (16.0, then the program writes 0.0)
//!                             1 frame, then exit 101, same assertion
//!   text_tiny (1e-50: positive, 0 as f32)
//!                             exit 101, same assertion
//!   text_editor_zero, canvas_zero
//!                             exit 101: buffer.rs:653 "font size cannot be 0"
//!                             (canvas at present, Cache::allocate)
//!   text_neg (-1.0)           HUNG: 0 frames, killed after 60 s; the
//!                             main thread spins in Buffer::shape_until_scroll
//!   text_nan (0.0 / 0.0)      exit 0, 2 frames
//!
//! By hand, with a display (the md_drop path; not run here, no display):
//! dragging the slider to 0 should end the program with the same panic.
//!   use gui::{window, column::column, slider::slider, markdown::markdown};
//!   let zoom = 16.0;
//!   [&window(&column(&[
//!     slider(#min: &0.0, #max: &32.0, #on_change: |v| zoom <- v, &zoom),
//!     markdown(#text_size: &zoom, &"hello")
//!   ]))]

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing;
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW},
};
use graphix_rt::{GXEvent, NoExt};
use iced_core::{Color, Font, Pixels, Size, clipboard, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use poolshark::global::GPooled;
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

/// One frame as `GuiHandler` renders it, presented to a texture.
fn frame(widget: &GuiW<NoExt>, renderer: &mut widgets::Renderer) {
    let mut ui = UserInterface::build(
        widget.view(),
        Size::new(400.0, 300.0),
        Cache::default(),
        renderer,
    );
    let mut messages = Vec::new();
    let _ = ui.update(
        &[],
        mouse::Cursor::Unavailable,
        renderer,
        &mut clipboard::Null,
        &mut messages,
    );
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    ui.draw(renderer, &theme, &style, mouse::Cursor::Unavailable);
    drop(ui);
    let _ = renderer
        .screenshot(&Viewport::with_physical_size(Size::new(400, 300), 1.0), Color::BLACK);
}

async fn first_value(rx: &mut Rx, target: graphix_compiler::expr::ExprId) -> Result<netidx::publisher::Value> {
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

async fn apply_updates(rx: &mut Rx, widget: &mut GuiW<NoExt>, window: Duration) -> Result<()> {
    let rt = tokio::runtime::Handle::current();
    let deadline = tokio::time::Instant::now() + window;
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for ev in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = ev {
                        widget.handle_update(&rt, id, &v)?;
                    }
                }
            }
            _ = tokio::time::sleep_until(deadline) => return Ok(()),
        }
    }
}

/// Compile `code` (a module whose `result` is a Widget), render a
/// frame, apply the program's updates for a second, render again.
async fn run(code: &str) -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        netidx_core::path::Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(code)),
    )]);
    let ctx =
        testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
    let v = first_value(&mut rx, compiled.exprs[0].id).await?;
    let mut widget = widgets::compile(gx.clone(), v).await.context("widget")?;
    let mut renderer = renderer().await;
    frame(&widget, &mut renderer);
    eprintln!("CASE: first frame rendered");
    apply_updates(&mut rx, &mut widget, Duration::from_millis(1000)).await?;
    frame(&widget, &mut renderer);
    eprintln!("CASE: second frame rendered");
    Ok(())
}

const MD: &str = "use gui::markdown::markdown;\n";
const TEXT: &str = "use gui::text::text;\n";
const CANVAS: &str = "use gui::{color, canvas::canvas};\n";

fn markdown(size: &str) -> String {
    format!("{MD}let size = {size};\nlet result = markdown(#text_size: &size, &\"hello\")")
}

fn text(size: &str) -> String {
    format!("{TEXT}let size = {size};\nlet result = text(#size: &size, &\"hello\")")
}

fn canvas(size: &str) -> String {
    format!(
        "{CANVAS}let shapes = [`Text({{content: \"hello\", position: {{x: 10.0, y: 10.0}}, \
         color: color(#r: 1.0)$, size: {size}}})];\n\
         let result = canvas(#width: &`Fixed(200.0), #height: &`Fixed(100.0), &shapes)"
    )
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_md_ok() -> Result<()> {
    run(&markdown("16.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_md_zero() -> Result<()> {
    run(&markdown("0.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_md_drop() -> Result<()> {
    let code = format!(
        "{MD}let size = 16.0;\n\
         size <- sys::time::after_idle(duration:300.ms, 0.0);\n\
         let result = markdown(#text_size: &size, &\"hello\")"
    );
    run(&code).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_ok() -> Result<()> {
    run(&text("16.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_zero() -> Result<()> {
    run(&text("0.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_tiny() -> Result<()> {
    run(&text("1e-50")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_neg() -> Result<()> {
    run(&text("-1.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_nan() -> Result<()> {
    run(&text("0.0 / 0.0")).await
}

fn inputs(size: &str) -> String {
    format!(
        "use gui::text_input::text_input;\nlet size = {size};\n\
         let result = text_input(#size: &size, &\"hello\")"
    )
}

fn editor(size: &str) -> String {
    format!(
        "use gui::text_editor::text_editor;\nlet size = {size};\n\
         let result = text_editor(#size: &size, &\"hello\")"
    )
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_input_ok() -> Result<()> {
    run(&inputs("16.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_input_zero() -> Result<()> {
    run(&inputs("0.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_editor_ok() -> Result<()> {
    run(&editor("16.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_text_editor_zero() -> Result<()> {
    run(&editor("0.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_canvas_ok() -> Result<()> {
    run(&canvas("16.0")).await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "child of driver"]
async fn case_canvas_zero() -> Result<()> {
    run(&canvas("0.0")).await
}

const CASES: &[&str] = &[
    "case_md_ok",
    "case_md_zero",
    "case_md_drop",
    "case_text_ok",
    "case_text_zero",
    "case_text_tiny",
    "case_text_neg",
    "case_text_nan",
    "case_text_input_ok",
    "case_text_input_zero",
    "case_text_editor_ok",
    "case_text_editor_zero",
    "case_canvas_ok",
    "case_canvas_zero",
];

#[test]
fn zero_text_size_renders() {
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
        let stdout = String::from_utf8_lossy(&out.stdout);
        let frames = stderr.matches("CASE: ").count();
        let panic: Vec<&str> = stderr
            .lines()
            .skip_while(|l| !l.contains("panicked at"))
            .take(2)
            .collect();
        eprintln!(
            "{case}: exit {:?}, frames rendered {frames}{}",
            out.status.code(),
            if panic.is_empty() { String::new() } else { format!("\n    {}", panic.join("\n    ")) }
        );
        if !out.status.success() {
            if panic.is_empty() {
                eprintln!("--- stdout\n{stdout}\n--- stderr\n{stderr}");
            }
            failed.push(*case);
        }
    }
    assert!(failed.is_empty(), "cases that did not render: {failed:?}");
}
