//! gui-widgets-b-02: a zero `#size` on text, text_input, text_editor or a
//! checked checkbox panics cosmic-text inside the frame, on the GUI's main
//! thread (`GuiHandler::about_to_wait` has no guard), ending the program.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_02.rs:
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_02 -- --nocapture
//!
//! `each_case_in_its_own_process` runs every case in a child process of its
//! own: the first cosmic-text panic poisons iced's process-global font
//! system lock, which would fail every later case of the same process. A
//! case compiles a widget, applies its updates and renders one frame as
//! `about_to_wait` does: `UserInterface::build` (layout), `update`, `draw`,
//! then the renderer's `draw` into an offscreen target (`screenshot` runs
//! the same `Renderer::draw` that `present` does) on a headless wgpu
//! device. No window is opened.
//!
//! Expected: every case renders its frame (a size of 0.0 is refused or
//! ignored, never handed to iced).
//! Observed at c722befe (dev profile):
//!   control_text_size_12, control_checkbox_size_12_checked: frame rendered
//!   text_size_0, text_input_size_0: panicked at
//!     cosmic-text-0.15.0/src/buffer.rs:253:9: line height cannot be 0
//!   text_editor_size_0, checkbox_size_0_checked: panicked at
//!     cosmic-text-0.15.0/src/buffer.rs:653:13: font size cannot be 0
//!   slider_left_edge_sets_text_size_0: a press at the left edge of
//!     `slider(#max: &72.0, #on_change: |v| size <- v, &size)` (default
//!     `#min` 0.0) calls on_change(0.0); the next frame of
//!     `text(#size: &size, &"Aa")` panics: line height cannot be 0
//!   text_size_0_caught_then_text_size_12: with that panic caught, a fresh
//!     size 12.0 text panics at iced_graphics-0.14.0/src/text/paragraph.rs:69:
//!     "Write font system: PoisonError", so catching the panic cannot save
//!     the GUI
//! `extra_probes_in_their_own_process` (reported, not asserted):
//!   markdown(#text_size: &0.0): line height cannot be 0
//!   text(#size: &(0.0 - 1.0)): the frame never returns, spinning at 100%
//!     CPU (cosmic-text `Buffer::shape_until_scroll` loops on a negative
//!     line height); killed by the 20 s child timeout
//!   radio and toggler `#size: &0.0`, text `#size` NaN or inf: rendered

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Color, Event, Point, Size, clipboard, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use poolshark::global::GPooled;
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

const VIEWPORT: Size = Size::new(400.0, 300.0);

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

struct Ui {
    ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
}

impl Ui {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
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
        let renderer = renderer().await;
        let mut ui = Self {
            ctx,
            _compiled: compiled,
            rx,
            widget,
            renderer,
            cache: Cache::default(),
        };
        ui.drain().await?;
        Ok(ui)
    }

    /// Apply every pending runtime update to the widget tree.
    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(300), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    self.widget.handle_update(&rt, i, &v)?;
                }
            }
        }
        Ok(())
    }

    /// One frame as `about_to_wait` renders it: layout, events, draw, then
    /// the renderer's own draw into an offscreen target.
    fn frame(&mut self, events: &[Event], cursor: mouse::Cursor) -> Vec<Message> {
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let style = Style { text_color: theme.palette().text };
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(self.widget.view(), VIEWPORT, cache, &mut self.renderer);
        let mut messages = Vec::new();
        let _ = ui.update(
            events,
            cursor,
            &mut self.renderer,
            &mut clipboard::Null,
            &mut messages,
        );
        ui.draw(&mut self.renderer, &theme, &style, cursor);
        self.cache = ui.into_cache();
        let _ = self.renderer.screenshot(
            &Viewport::with_physical_size(Size::new(400, 300), 1.0),
            Color::BLACK,
        );
        messages
    }

    /// Send every `Message::Call` to the runtime, as `about_to_wait`
    /// does, then apply the updates that follow.
    async fn dispatch(&mut self, messages: Vec<Message>) -> Result<usize> {
        let mut n = 0;
        for m in messages {
            if let Message::Call(id, args) = m {
                self.ctx.rt.call(id, args)?;
                n += 1;
            }
        }
        self.drain().await?;
        Ok(n)
    }
}

const CASES: &[&str] = &[
    "control_text_size_12",
    "control_checkbox_size_12_checked",
    "text_size_0",
    "text_input_size_0",
    "text_editor_size_0",
    "checkbox_size_0_checked",
    "slider_left_edge_sets_text_size_0",
    "text_size_0_caught_then_text_size_12",
];

/// Each case runs in a process of its own: a cosmic-text panic inside
/// `Paragraph::with_text` poisons iced's process-global font system lock,
/// and every later text layout in that process panics on the poison.
#[test]
fn each_case_in_its_own_process() {
    let exe = std::env::current_exe().expect("current_exe");
    let mut failed = Vec::new();
    for case in CASES {
        let out = std::process::Command::new("timeout")
            .args(["-s", "KILL", "60"])
            .arg(&exe)
            .args(["--exact", case, "--include-ignored", "--nocapture"])
            .output()
            .expect("spawn the test binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let ok = out.status.success();
        eprintln!("=== {case}: {}", if ok { "ok" } else { "FAILED" });
        for l in stderr.lines() {
            if l.starts_with(case) || l.contains("panicked at") || l.contains("cannot be 0")
                || l.contains("PoisonError")
            {
                eprintln!("    {l}");
            }
        }
        if !ok {
            failed.push(*case);
        }
    }
    assert!(failed.is_empty(), "cases that did not render: {failed:?}");
}

/// A panic caught around the frame does not save the GUI: the font system
/// lock stays poisoned and the next, valid text layout panics.
#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn text_size_0_caught_then_text_size_12() -> Result<()> {
    let name = "text_size_0_caught_then_text_size_12";
    let mut bad = Ui::new("use gui::text::text;\nlet result = text(#size: &0.0, &\"x\")").await?;
    let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        bad.frame(&[], mouse::Cursor::Unavailable);
    }));
    eprintln!("{name}: size 0.0 frame caught a panic: {}", caught.is_err());
    std::mem::forget(bad);
    let mut good =
        Ui::new("use gui::text::text;\nlet result = text(#size: &12.0, &\"x\")").await?;
    eprintln!("{name}: rendering a size 12.0 frame");
    good.frame(&[], mouse::Cursor::Unavailable);
    eprintln!("{name}: frame rendered");
    Ok(())
}

async fn one_frame(name: &str, code: &str) -> Result<()> {
    let mut ui = Ui::new(code).await?;
    eprintln!("{name}: rendering a frame");
    ui.frame(&[], mouse::Cursor::Unavailable);
    eprintln!("{name}: frame rendered");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn control_text_size_12() -> Result<()> {
    one_frame(
        "control_text_size_12",
        "use gui::text::text;\nlet result = text(#size: &12.0, &\"x\")",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn control_checkbox_size_12_checked() -> Result<()> {
    one_frame(
        "control_checkbox_size_12_checked",
        "use gui::checkbox::checkbox;\n\
         let result = checkbox(#label: &\"x\", #size: &12.0, &true)",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn text_size_0() -> Result<()> {
    one_frame("text_size_0", "use gui::text::text;\nlet result = text(#size: &0.0, &\"x\")")
        .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn text_input_size_0() -> Result<()> {
    one_frame(
        "text_input_size_0",
        "use gui::text_input::text_input;\nlet result = text_input(#size: &0.0, &\"x\")",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn text_editor_size_0() -> Result<()> {
    one_frame(
        "text_editor_size_0",
        "use gui::text_editor::text_editor;\n\
         let result = text_editor(#size: &0.0, &\"hello\")",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn checkbox_size_0_checked() -> Result<()> {
    one_frame(
        "checkbox_size_0_checked",
        "use gui::checkbox::checkbox;\n\
         let result = checkbox(#label: &\"x\", #size: &0.0, &true)",
    )
    .await
}

/// A font-size slider with the default `#min` of 0.0: a press at the
/// track's left edge sets the size to 0.0 and the next frame panics.
#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by each_case_in_its_own_process"]
async fn slider_left_edge_sets_text_size_0() -> Result<()> {
    let name = "slider_left_edge_sets_text_size_0";
    let mut ui = Ui::new(
        "use gui::{column::column, slider::slider, text::text};\n\
         let size = 16.0;\n\
         let result = column(&[\n\
           slider(#max: &72.0, #on_change: |v| size <- v, &size),\n\
           text(#size: &size, &\"Aa\")\n\
         ])",
    )
    .await?;
    ui.frame(&[], mouse::Cursor::Unavailable);
    eprintln!("{name}: first frame rendered at size 16.0");
    let edge = Point::new(0.0, 8.0);
    let cursor = mouse::Cursor::Available(edge);
    let mut messages =
        ui.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: edge })], cursor);
    messages.extend(ui.frame(
        &[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))],
        cursor,
    ));
    eprintln!("{name}: press at the left edge produced {messages:?}");
    let calls = ui.dispatch(messages).await?;
    if calls == 0 {
        bail!("the press produced no on_change call");
    }
    eprintln!("{name}: on_change dispatched, rendering the next frame");
    ui.frame(&[], cursor);
    eprintln!("{name}: frame rendered");
    Ok(())
}

const EXTRA: &[&str] = &[
    "markdown_text_size_0",
    "radio_size_0_selected",
    "toggler_size_0_on",
    "text_size_negative",
    "text_size_nan",
    "text_size_inf",
];

/// Neighbouring values and widgets, reported only.
#[test]
fn extra_probes_in_their_own_process() {
    let exe = std::env::current_exe().expect("current_exe");
    for case in EXTRA {
        let out = std::process::Command::new("timeout")
            .args(["-s", "KILL", "20"])
            .arg(&exe)
            .args(["--exact", case, "--include-ignored", "--nocapture"])
            .output()
            .expect("spawn the test binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let status = match out.status.code() {
            Some(0) => "ok".to_string(),
            None | Some(137) => "KILLED by the 20 s timeout (hang)".to_string(),
            Some(c) => format!("FAILED (exit {c})"),
        };
        eprintln!("=== extra {case}: {status}");
        for l in stderr.lines() {
            if l.starts_with(case) || l.contains("panicked at") || l.contains("assert")
                || l.contains("PoisonError") || l.contains("cannot be")
            {
                eprintln!("    {l}");
            }
        }
    }
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by extra_probes_in_their_own_process"]
async fn markdown_text_size_0() -> Result<()> {
    one_frame(
        "markdown_text_size_0",
        "use gui::markdown::markdown;\nlet result = markdown(#text_size: &0.0, &\"body\")",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by extra_probes_in_their_own_process"]
async fn radio_size_0_selected() -> Result<()> {
    one_frame(
        "radio_size_0_selected",
        "use gui::radio::radio;\n\
         let result = radio(#label: &\"x\", #size: &0.0, #selected: &1, &1)",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by extra_probes_in_their_own_process"]
async fn toggler_size_0_on() -> Result<()> {
    one_frame(
        "toggler_size_0_on",
        "use gui::toggler::toggler;\n\
         let result = toggler(#label: &\"x\", #size: &0.0, &true)",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by extra_probes_in_their_own_process"]
async fn text_size_negative() -> Result<()> {
    one_frame(
        "text_size_negative",
        "use gui::text::text;\nlet result = text(#size: &(0.0 - 1.0), &\"x\")",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by extra_probes_in_their_own_process"]
async fn text_size_nan() -> Result<()> {
    one_frame(
        "text_size_nan",
        "use gui::text::text;\nlet result = text(#size: &(0.0 / 0.0), &\"x\")",
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
#[ignore = "run in its own process by extra_probes_in_their_own_process"]
async fn text_size_inf() -> Result<()> {
    one_frame(
        "text_size_inf",
        "use gui::text::text;\nlet result = text(#size: &(1.0 / 0.0), &\"x\")",
    )
    .await
}
