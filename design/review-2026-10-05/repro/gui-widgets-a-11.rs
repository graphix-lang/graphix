//! gui-widgets-a-11: combo_box rebuilds its iced State on every fire of
//! `options`, so a re-fire with the SAME options wipes the text the user
//! is typing and the filtered list. Also gui-widgets-a-13: a
//! `#disabled: &true` combo box still takes typing and makes a selection
//! (which it drops as Message::Nop).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_11.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_11 -- --nocapture
//!
//! Each case drives the widget headlessly as the event loop does
//! (UserInterface::build/update over a kept cache): click into the box,
//! type "ban", let the program's updates reach `handle_update` for
//! 700 ms, press Enter, and report the Message the box published.
//!
//! Expected: steady and refire both select "banana" (the typed filter
//! leaves only banana); disabled ignores the typing and publishes
//! nothing.
//! Observed at c722befe (test FAILS):
//!   steady:   keys captured 3/3, updates applied 0, Enter published Call("banana")
//!   refire:   keys captured 3/3, updates applied 3, Enter published Call("apple")
//!             (three same-value fires of `options` reset the typed "ban")
//!   disabled: keys captured 3/3, updates applied 0, Enter published Nop
//!             (the disabled box took the typing and made a selection)

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
        self.cursor = pos;
        self.events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        self.events(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]);
        self.events(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]);
    }

    fn key(&mut self, key: keyboard::Key, text: Option<&str>) -> (Vec<Message>, Vec<event::Status>) {
        self.events(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: key.clone(),
            modified_key: key,
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers: keyboard::Modifiers::empty(),
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

/// Click, type "ban", pump updates, press Enter; returns what Enter published.
async fn run_case(name: &str, code: &str) -> Result<String> {
    let mut h = H::new(code).await?;
    let _ = h.events(&[]);
    h.click(Point::new(10.0, 10.0));
    let typed = h.type_text("ban");
    let captured =
        typed.iter().filter(|s| matches!(s, event::Status::Captured)).count();
    h.pump(Duration::from_millis(700)).await?;
    let (msgs, _) = h.key(keyboard::Key::Named(keyboard::key::Named::Enter), None);
    let out = describe(&msgs);
    eprintln!(
        "{name}: keys captured {captured}/{}, updates applied {}, Enter published {out}",
        typed.len(),
        h.updates
    );
    Ok(out)
}

const OPTS: &str = "[\"apple\", \"banana\", \"cherry\"]";

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn combo_box_keeps_typed_filter() -> Result<()> {
    let steady = run_case(
        "steady",
        &format!(
            "use gui::combo_box::combo_box;\n\
             let opts = {OPTS};\n\
             let result = combo_box(#on_select: |s| null, &opts)"
        ),
    )
    .await?;
    let refire = run_case(
        "refire",
        &format!(
            "use gui::combo_box::combo_box;\n\
             let opts = {OPTS};\n\
             opts <- sys::time::timer(duration:200.ms, true) ~ {OPTS};\n\
             let result = combo_box(#on_select: |s| null, &opts)"
        ),
    )
    .await?;
    let disabled = run_case(
        "disabled",
        &format!(
            "use gui::combo_box::combo_box;\n\
             let opts = {OPTS};\n\
             let result = combo_box(#disabled: &true, #on_select: |s| null, &opts)"
        ),
    )
    .await?;
    let mut bad = Vec::new();
    if steady != "Call(\"banana\")" {
        bad.push(format!("steady published {steady}"));
    }
    if refire != "Call(\"banana\")" {
        bad.push(format!("refire published {refire}"));
    }
    if disabled != "nothing" {
        bad.push(format!("disabled published {disabled}"));
    }
    assert!(bad.is_empty(), "{bad:?}");
    Ok(())
}
