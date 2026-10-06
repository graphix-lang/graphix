//! gui-core-02: winit's `ModifiersChanged` never reaches iced, so a graphix
//! `text_input`'s Ctrl and Shift bindings do nothing
//! (stdlib/graphix-package-gui/src/convert.rs:108 drops the event;
//! event_loop.rs:159 only stores it to stamp `KeyPressed.modifiers`, which
//! iced 0.14's text_input never reads).
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_02.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_02 -- --nocapture
//!
//! Part 1 runs the real `convert::window_event` / `convert::mouse_button`
//! over the winit events that carry modifier state, IME text and the
//! Back/Forward buttons, and prints how many iced events each yields.
//!
//! Part 2 compiles `text_input(#on_input: |s| null, &"hello")`, focuses it
//! with a click (headless wgpu renderer, `UserInterface` as
//! `GuiHandler::about_to_wait` drives it, a clipboard holding "PASTED"),
//! and feeds the iced events `GuiHandler::window_event` produces for a key
//! chord: each modifier change goes through the real `convert::window_event`,
//! each key press is built field for field as convert.rs:61-93 builds it
//! (winit's `KeyEvent` cannot be constructed outside winit) with
//! `modifiers: convert::convert_modifiers(state)` and the text xkb gives
//! under Ctrl. Each chord runs twice: as graphix delivers it ("graphix"),
//! and with the `keyboard::Event::ModifiersChanged` that iced_winit's
//! conversion emits for the same winit event ("forwarded").
//!
//! Expected (both rows equal to "forwarded"): ModifiersChanged converts to
//! one iced event; Ctrl+V publishes the pasted text ("hPASTEDello": the
//! focusing click leaves the cursor after the h); Ctrl+A, Ctrl+C writes
//! "hello" to the clipboard; End, Ctrl+Backspace publishes ""; End,
//! Shift+Home, x publishes "x".
//! Observed at c722befe (dev profile):
//!   convert::window_event(ModifiersChanged(CONTROL | SHIFT | 0x0)): 0 iced events
//!   convert::window_event(Ime(Enabled) | Ime(Commit("x"))): 0 iced events
//!   convert::mouse_button(Back | Forward) = None
//!   CtrlV            graphix  : on_input=[]          clipboard_writes=[]
//!   CtrlV            forwarded: on_input=["hPASTEDello"] clipboard_writes=[]
//!   CtrlACtrlC       graphix  : on_input=[]          clipboard_writes=[]
//!   CtrlACtrlC       forwarded: on_input=[]          clipboard_writes=["hello"]
//!   EndCtrlBackspace graphix  : on_input=["hell"]    clipboard_writes=[]
//!   EndCtrlBackspace forwarded: on_input=[""]        clipboard_writes=[]
//!   EndShiftHomeX    graphix  : on_input=["xhello"]  clipboard_writes=[]
//!   EndShiftHomeX    forwarded: on_input=["x"]       clipboard_writes=[]

use ahash::AHashMap;
use anyhow::{Context, Result};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    convert,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{
    Event, Point, Size, SmolStr, clipboard,
    keyboard::{
        self, Key,
        key::{Code, Named, Physical},
    },
    mouse,
};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;
use winit::{
    event::{Ime, Modifiers, MouseButton, WindowEvent},
    keyboard::ModifiersState,
};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

struct Gpu {
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

async fn gpu() -> Gpu {
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
}

fn renderer(g: &Gpu) -> widgets::Renderer {
    let engine = iced_wgpu::Engine::new(
        &g.adapter,
        g.device.clone(),
        g.queue.clone(),
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

struct TestClipboard {
    contents: String,
    writes: Vec<String>,
}

impl clipboard::Clipboard for TestClipboard {
    fn read(&self, kind: clipboard::Kind) -> Option<String> {
        match kind {
            clipboard::Kind::Standard => Some(self.contents.clone()),
            clipboard::Kind::Primary => None,
        }
    }

    fn write(&mut self, kind: clipboard::Kind, contents: String) {
        if kind == clipboard::Kind::Standard {
            self.writes.push(contents);
        }
    }
}

struct Input {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    _rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
}

async fn text_input() -> Result<Input> {
    let code = "use gui::text_input::{self, *};\n\
                let result = text_input(#on_input: |s| null, &\"hello\")";
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
    Ok(Input { _ctx: ctx, _compiled: compiled, _rx: rx, widget })
}

struct Session<'a> {
    widget: &'a GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
    clipboard: TestClipboard,
    inputs: Vec<String>,
}

impl Session<'_> {
    fn frame(&mut self, events: &[Event]) {
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(
            self.widget.view(),
            Size::new(300.0, 50.0),
            cache,
            &mut self.renderer,
        );
        let mut messages: Vec<Message> = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut self.clipboard,
            &mut messages,
        );
        self.cache = ui.into_cache();
        for m in messages {
            if let Message::Call(_, args) = m {
                if let Some(Value::String(s)) = args.iter().next() {
                    self.inputs.push(s.to_string());
                }
            }
        }
    }

    fn focus(&mut self) {
        self.cursor = Point::new(10.0, 10.0);
        self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: self.cursor })]);
        self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]);
        self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]);
    }

    /// What reaches iced for winit's `ModifiersChanged(state)`: graphix's
    /// `convert::window_event`, or iced_winit's conversion.
    fn modifiers_changed(&mut self, state: ModifiersState, forward: bool) {
        let events: Vec<Event> = if forward {
            vec![Event::Keyboard(keyboard::Event::ModifiersChanged(
                convert::convert_modifiers(state),
            ))]
        } else {
            let ev = WindowEvent::ModifiersChanged(Modifiers::from(state));
            convert::window_event(&ev, 1.0, state).iter().cloned().collect()
        };
        self.frame(&events);
    }

    /// The `KeyPressed` convert.rs:61-93 builds, `modifiers` from
    /// `GuiHandler::modifiers`.
    fn press(&mut self, key: Key, code: Code, text: Option<&str>, mods: ModifiersState) {
        self.frame(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: key.clone(),
            modified_key: key,
            physical_key: Physical::Code(code),
            location: keyboard::Location::Standard,
            modifiers: convert::convert_modifiers(mods),
            text: text.map(SmolStr::from),
            repeat: false,
        })]);
    }
}

#[derive(Clone, Copy, Debug)]
enum Chord {
    CtrlV,
    CtrlACtrlC,
    EndCtrlBackspace,
    EndShiftHomeX,
}

fn run(input: &Input, g: &Gpu, chord: Chord, forward: bool) -> (Vec<String>, Vec<String>) {
    let mut s = Session {
        widget: &input.widget,
        renderer: renderer(g),
        cache: Cache::default(),
        cursor: Point::ORIGIN,
        clipboard: TestClipboard { contents: "PASTED".into(), writes: Vec::new() },
        inputs: Vec::new(),
    };
    s.focus();
    s.inputs.clear();
    let ctrl = ModifiersState::CONTROL;
    let shift = ModifiersState::SHIFT;
    let none = ModifiersState::empty();
    match chord {
        Chord::CtrlV => {
            s.modifiers_changed(ctrl, forward);
            s.press(Key::Named(Named::Control), Code::ControlLeft, None, ctrl);
            s.press(Key::Character("v".into()), Code::KeyV, Some("\u{16}"), ctrl);
        }
        Chord::CtrlACtrlC => {
            s.modifiers_changed(ctrl, forward);
            s.press(Key::Named(Named::Control), Code::ControlLeft, None, ctrl);
            s.press(Key::Character("a".into()), Code::KeyA, Some("\u{1}"), ctrl);
            s.press(Key::Character("c".into()), Code::KeyC, Some("\u{3}"), ctrl);
        }
        Chord::EndCtrlBackspace => {
            s.press(Key::Named(Named::End), Code::End, None, none);
            s.modifiers_changed(ctrl, forward);
            s.press(Key::Named(Named::Control), Code::ControlLeft, None, ctrl);
            s.press(Key::Named(Named::Backspace), Code::Backspace, Some("\u{8}"), ctrl);
        }
        Chord::EndShiftHomeX => {
            s.press(Key::Named(Named::End), Code::End, None, none);
            s.modifiers_changed(shift, forward);
            s.press(Key::Named(Named::Shift), Code::ShiftLeft, None, shift);
            s.press(Key::Named(Named::Home), Code::Home, None, shift);
            s.modifiers_changed(none, forward);
            s.press(Key::Character("x".into()), Code::KeyX, Some("x"), none);
        }
    }
    (s.inputs, s.clipboard.writes)
}

#[tokio::test(flavor = "current_thread")]
async fn modifiers_reach_text_input() -> Result<()> {
    for state in [ModifiersState::CONTROL, ModifiersState::SHIFT, ModifiersState::empty()]
    {
        let ev = WindowEvent::ModifiersChanged(Modifiers::from(state));
        let out = convert::window_event(&ev, 1.0, state);
        eprintln!(
            "RESULT convert::window_event(ModifiersChanged({state:?})): {} iced events {:?}",
            out.len(),
            &*out
        );
    }
    for ime in [Ime::Enabled, Ime::Commit("x".into())] {
        let ev = WindowEvent::Ime(ime.clone());
        let out = convert::window_event(&ev, 1.0, ModifiersState::empty());
        eprintln!(
            "RESULT convert::window_event(Ime({ime:?})): {} iced events",
            out.len()
        );
    }
    for b in [MouseButton::Back, MouseButton::Forward] {
        eprintln!("RESULT convert::mouse_button({b:?}) = {:?}", convert::mouse_button(b));
    }
    let g = gpu().await;
    let input = text_input().await?;
    for chord in
        [Chord::CtrlV, Chord::CtrlACtrlC, Chord::EndCtrlBackspace, Chord::EndShiftHomeX]
    {
        for forward in [false, true] {
            let (inputs, writes) = run(&input, &g, chord, forward);
            eprintln!(
                "RESULT {chord:?} {}: on_input={inputs:?} clipboard_writes={writes:?}",
                if forward { "forwarded" } else { "graphix  " }
            );
        }
    }
    Ok(())
}
