//! gui-core-01: the GUI event loop never delivers
//! `window::Event::RedrawRequested` to the widget tree, so every
//! status-styled iced widget is drawn with its fallback status: a
//! button, text input or checkbox that has its callback is drawn
//! exactly like a `#disabled: &true` one, and never shows hover.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_01.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_01 -- --nocapture
//!
//! Each widget is compiled from a Graphix program and rendered
//! headlessly, one frame the way `GuiHandler::about_to_wait` renders it
//! (event_loop.rs:314-347: `view()`, `UserInterface::build`, `update`
//! with the pending OS events, `draw`), and, as the reference, one
//! frame that also delivers `RedrawRequested(now)` before `draw`, the
//! way iced's own shell does. A pixel region inside the widget is
//! read back from the frame (dominant colour; the brightest pixel for
//! the text input's value).
//!
//! Expected: a widget with its callback, drawn by the event loop, looks
//! like the Active reference (and like the Hovered one under the
//! cursor); the test passes.
//! Observed at c722befe (test FAILS), dominant sRGB colour inside the
//! widget, Dark theme over black:
//!   button (press + release publish 1 Call: it is enabled)
//!     event loop: idle, under the cursor, pressed  [62, 72, 178]
//!     Active [88, 101, 242]  Hovered [107, 117, 255]
//!     #disabled: &true [62, 72, 178]
//!   text_input  event loop [67, 71, 78]; Active [43, 45, 49];
//!     #disabled [67, 71, 78]; value text event loop [136, 138, 144]
//!     (the placeholder colour), Active [230, 230, 230]
//!   checkbox (checked; the press publishes 1 Call)
//!     event loop [80, 84, 93]; Active [88, 101, 242];
//!     Hovered [107, 117, 255]; #disabled [80, 84, 93]
//! The button colour is the one in book/src/ui/gui/media/button.png
//! (pixel 730,290) and the text input's the one in text_input.png.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Color, Event, Font, Pixels, Point, Size, clipboard, mouse, renderer::Style, window,
};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

const REG: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const W: u32 = 400;
const H: u32 = 120;

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

struct Harness {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
}

async fn first_value(rx: &mut Rx, target: ExprId) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(30));
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

impl Harness {
    async fn new(code: String) -> Result<Self> {
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
        let widget = widgets::compile(gx.clone(), v).await.context("widget")?;
        let mut h = Self { _ctx: ctx, _compiled: compiled, rx, widget };
        h.drain().await?;
        Ok(h)
    }

    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + Duration::from_millis(500);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return Ok(()),
            }
        }
    }
}

struct Frame {
    rgba: Vec<u8>,
    messages: Vec<Message>,
}

/// One frame. `redraw_requested: false` is `about_to_wait` as written;
/// `true` also delivers `RedrawRequested(now)` before `draw`.
fn frame(
    widget: &mut GuiW<NoExt>,
    renderer: &mut widgets::Renderer,
    cache: &mut Cache,
    events: &[Event],
    cursor: Point,
    redraw_requested: bool,
) -> Frame {
    widget.before_view();
    let mut messages = Vec::new();
    let mut ui = UserInterface::build(
        widget.view(),
        Size::new(W as f32, H as f32),
        std::mem::take(cache),
        renderer,
    );
    let cursor = mouse::Cursor::Available(cursor);
    let mut clipboard = clipboard::Null;
    let _ = ui.update(events, cursor, renderer, &mut clipboard, &mut messages);
    if redraw_requested {
        let redraw = [Event::Window(window::Event::RedrawRequested(Instant::now()))];
        let _ = ui.update(&redraw, cursor, renderer, &mut clipboard, &mut messages);
    }
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let style = Style { text_color: theme.palette().text };
    ui.draw(renderer, &theme, &style, cursor);
    *cache = ui.into_cache();
    let rgba = renderer
        .screenshot(&Viewport::with_physical_size(Size::new(W, H), 1.0), Color::BLACK);
    Frame { rgba, messages }
}

type Rgb = [u8; 3];

fn pixels(f: &Frame, r: (u32, u32, u32, u32)) -> impl Iterator<Item = Rgb> + '_ {
    let (x0, y0, x1, y1) = r;
    (y0..y1).flat_map(move |y| {
        (x0..x1).map(move |x| {
            let i = ((y * W + x) * 4) as usize;
            [f.rgba[i], f.rgba[i + 1], f.rgba[i + 2]]
        })
    })
}

fn dominant(f: &Frame, r: (u32, u32, u32, u32)) -> Rgb {
    let mut counts: AHashMap<Rgb, usize> = AHashMap::default();
    for p in pixels(f, r) {
        *counts.entry(p).or_default() += 1;
    }
    counts.into_iter().max_by_key(|(_, n)| *n).map(|(c, _)| c).unwrap()
}

fn brightest(f: &Frame, r: (u32, u32, u32, u32)) -> Rgb {
    pixels(f, r)
        .max_by_key(|p| p[0] as u32 + p[1] as u32 + p[2] as u32)
        .unwrap()
}

struct Probe {
    name: &'static str,
    program: fn(bool) -> String,
    /// x0, y0, x1, y1 inside the widget (it sits at (30, 30)).
    region: (u32, u32, u32, u32),
    center: Point,
    /// The text input's value colour is checked too.
    value_text: bool,
}

fn button_program(disabled: bool) -> String {
    format!(
        "use gui::{{button::button, column::column, text::text}};\n\
         let disabled = {disabled};\n\
         let result = column(#padding: &`All(30.0), &[button(\n\
             #on_press: |c| println(c ~ \"clicked\"),\n\
             #width: &`Fixed(100.0),\n\
             #height: &`Fixed(40.0),\n\
             #disabled: &disabled,\n\
             &text(&\"\")\n\
         )])"
    )
}

fn text_input_program(disabled: bool) -> String {
    format!(
        "use gui::{{column::column, text_input::text_input}};\n\
         let name = \"WWW\";\n\
         let disabled = {disabled};\n\
         let result = column(#padding: &`All(30.0), &[text_input(\n\
             #on_input: |v| name <- v,\n\
             #width: &`Fixed(200.0),\n\
             #size: &30.0,\n\
             #disabled: &disabled,\n\
             &name\n\
         )])"
    )
}

fn checkbox_program(disabled: bool) -> String {
    format!(
        "use gui::{{column::column, checkbox::checkbox}};\n\
         let checked = true;\n\
         let disabled = {disabled};\n\
         let result = column(#padding: &`All(30.0), &[checkbox(\n\
             #on_toggle: |b| checked <- b,\n\
             #size: &40.0,\n\
             #disabled: &disabled,\n\
             &checked\n\
         )])"
    )
}

const PROBES: &[Probe] = &[
    Probe {
        name: "button",
        program: button_program,
        region: (35, 35, 125, 65),
        center: Point::new(80.0, 50.0),
        value_text: false,
    },
    Probe {
        name: "text_input",
        program: text_input_program,
        region: (33, 33, 227, 76),
        center: Point::new(130.0, 54.0),
        value_text: true,
    },
    Probe {
        name: "checkbox",
        program: checkbox_program,
        region: (33, 33, 67, 67),
        center: Point::new(50.0, 50.0),
        value_text: false,
    },
];

fn calls(ms: &[Message]) -> usize {
    ms.iter().filter(|m| matches!(m, Message::Call(..))).count()
}

#[tokio::test(flavor = "multi_thread")]
async fn enabled_widgets_draw_as_enabled() -> Result<()> {
    let mut renderer = renderer().await;
    let mut failures = Vec::new();
    for p in PROBES {
        let mut on = Harness::new((p.program)(false)).await?;
        let mut off = Harness::new((p.program)(true)).await?;
        let w = &mut on.widget;
        let r = &mut renderer;
        let mut cache = Cache::default();
        // the event loop's frames: an idle frame, the cursor arriving,
        // a press and a release over the widget
        let cur_idle = frame(w, r, &mut cache, &[], Point::ORIGIN, false);
        let moved = [Event::Mouse(mouse::Event::CursorMoved { position: p.center })];
        let cur_hover = frame(w, r, &mut cache, &moved, p.center, false);
        let press = [Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))];
        let cur_press = frame(w, r, &mut cache, &press, p.center, false);
        let release = [Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))];
        let cur_release = frame(w, r, &mut cache, &release, p.center, false);
        // references: the same frames with RedrawRequested delivered
        let ref_active = frame(w, r, &mut Cache::default(), &[], Point::ORIGIN, true);
        let ref_hover = frame(w, r, &mut Cache::default(), &moved, p.center, true);
        let ref_disabled =
            frame(&mut off.widget, r, &mut Cache::default(), &[], Point::ORIGIN, true);
        let d = |f: &Frame| dominant(f, p.region);
        println!("== {} (callback set, drawn by the event loop's frame)", p.name);
        println!("   idle              {:?}", d(&cur_idle));
        println!("   cursor over it    {:?}", d(&cur_hover));
        println!("   pressed           {:?}", d(&cur_press));
        println!(
            "   messages from the press and the release: {} Call(s)",
            calls(&cur_press.messages) + calls(&cur_release.messages)
        );
        println!("   references, RedrawRequested delivered:");
        println!("   Active            {:?}", d(&ref_active));
        println!("   Hovered           {:?}", d(&ref_hover));
        println!("   #disabled: &true  {:?}", d(&ref_disabled));
        assert_ne!(
            d(&ref_active),
            d(&ref_disabled),
            "control: the probe cannot tell {} Active from Disabled",
            p.name
        );
        if d(&cur_idle) != d(&ref_active) {
            failures.push(format!(
                "{}: idle frame {:?}, expected Active {:?}; it is the #disabled colour {:?}",
                p.name,
                d(&cur_idle),
                d(&ref_active),
                d(&ref_disabled)
            ));
        }
        if d(&cur_hover) != d(&ref_hover) {
            failures.push(format!(
                "{}: frame under the cursor {:?}, expected Hovered {:?}",
                p.name,
                d(&cur_hover),
                d(&ref_hover)
            ));
        }
        if p.value_text {
            let b = |f: &Frame| brightest(f, p.region);
            println!(
                "   value text: event loop {:?}, Active {:?}, #disabled {:?}",
                b(&cur_idle),
                b(&ref_active),
                b(&ref_disabled)
            );
            if b(&cur_idle) != b(&ref_active) {
                failures.push(format!(
                    "{}: value text {:?}, expected the Active value colour {:?}",
                    p.name,
                    b(&cur_idle),
                    b(&ref_active)
                ));
            }
        }
    }
    for f in &failures {
        println!("FAIL: {f}");
    }
    assert!(failures.is_empty(), "enabled widgets drawn with another status: {failures:#?}");
    Ok(())
}
