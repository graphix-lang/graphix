//! gui-widgets-a-15: keyboard_area delivers keys only after a left click
//! inside it, and Tab does not focus it; keyboard_area.md never says so
//! and its example (a whole-window area reading "Press any key...")
//! ignores keys until the window is clicked.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_15.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_15 -- --nocapture
//!
//! Each case drives the widget headlessly as the event loop does
//! (UserInterface::build/update over a kept cache) and presses "a".
//!
//! Expected (the book's example): the press publishes the on_key_press
//! Call in every case.
//! Observed at c722befe (test FAILS):
//!   no click: "a" published nothing
//!   after Tab: "a" published nothing
//!   after a left click inside: "a" published Call({key: "a", ..})

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::expr::{VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message};
use graphix_rt::{GXEvent, NoExt};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, event, keyboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
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

struct H {
    _ctx: TestCtx,
    _compiled: graphix_rt::CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    cursor: Point,
    updates: usize,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
        let target = compiled.exprs[0].id;
        let timeout = tokio::time::sleep(Duration::from_secs(10));
        tokio::pin!(timeout);
        let v = loop {
            tokio::select! {
                biased;
                Some(mut batch) = rx.recv() => {
                    let mut found = None;
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev && id == target {
                            found = Some(v);
                        }
                    }
                    if let Some(v) = found { break v }
                }
                _ = &mut timeout => bail!("no widget value"),
            }
        };
        let widget = widgets::compile(gx.clone(), v).await.context("widget")?;
        Ok(Self {
            _ctx: ctx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: Point::ORIGIN,
            updates: 0,
        })
    }

    fn events(&mut self, events: &[Event]) -> (Vec<Message>, Vec<event::Status>) {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(
            element,
            Size::new(300.0, 200.0),
            cache,
            &mut self.renderer,
        );
        let mut msgs = Vec::new();
        let (_, statuses) = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        (msgs, statuses)
    }

    fn click(&mut self, pos: Point) {
        self.click_with(pos, mouse::Button::Left);
    }

    fn click_with(&mut self, pos: Point, b: mouse::Button) {
        self.cursor = pos;
        self.events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        self.events(&[Event::Mouse(mouse::Event::ButtonPressed(b))]);
        self.events(&[Event::Mouse(mouse::Event::ButtonReleased(b))]);
    }

    fn key(&mut self, key: keyboard::Key, text: Option<&str>) -> (Vec<Message>, Vec<event::Status>) {
        self.key_mods(key, text, keyboard::Modifiers::empty())
    }

    fn key_mods(
        &mut self,
        key: keyboard::Key,
        text: Option<&str>,
        modifiers: keyboard::Modifiers,
    ) -> (Vec<Message>, Vec<event::Status>) {
        self.events(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: key.clone(),
            modified_key: key,
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers,
            text: text.map(Into::into),
            repeat: false,
        })])
    }

    fn type_text(&mut self, s: &str) -> Vec<event::Status> {
        let mut all = Vec::new();
        for ch in s.chars() {
            let c: iced_core::SmolStr = ch.to_string().into();
            let (_, st) = self.key(keyboard::Key::Character(c.clone()), Some(c.as_str()));
            all.extend(st);
        }
        all
    }

    /// Feed the program's updates to the widget as the event loop does.
    async fn pump(&mut self, window: Duration) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + window;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                            self.updates += 1;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return Ok(()),
            }
        }
    }
}

fn describe(msgs: &[Message]) -> String {
    let parts: Vec<String> = msgs
        .iter()
        .map(|m| match m {
            Message::Call(_, args) => {
                let a: Vec<String> = args.iter().map(|v: &Value| format!("{v}")).collect();
                format!("Call({})", a.join(", "))
            }
            Message::Nop => "Nop".to_string(),
            other => format!("{other:?}"),
        })
        .collect();
    if parts.is_empty() { "nothing".to_string() } else { parts.join("; ") }
}


fn press_a(h: &mut H) -> String {
    let (msgs, _) = h.key(keyboard::Key::Character("a".into()), Some("a"));
    describe(&msgs)
}

const AREA: &str = "use gui::{keyboard_area::keyboard_area, text::text, container::container};\n\
    let result = keyboard_area(#on_key_press: |e| null, \
        &container(#width: &`Fill, #height: &`Fill, &text(&\"Press any key...\")))";

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn keyboard_area_takes_keys_without_a_click() -> Result<()> {
    let mut h = H::new(AREA).await?;
    let _ = h.events(&[]);
    let fresh = press_a(&mut h);
    eprintln!("no click: \"a\" published {fresh}");
    let mut h = H::new(AREA).await?;
    let _ = h.events(&[]);
    let _ = h.key(keyboard::Key::Named(keyboard::key::Named::Tab), None);
    let tabbed = press_a(&mut h);
    eprintln!("after Tab: \"a\" published {tabbed}");
    let mut h = H::new(AREA).await?;
    let _ = h.events(&[]);
    h.click(Point::new(100.0, 100.0));
    let clicked = press_a(&mut h);
    eprintln!("after a left click inside: \"a\" published {clicked}");
    assert!(fresh.starts_with("Call("), "keys ignored until a click: {fresh}");
    Ok(())
}
