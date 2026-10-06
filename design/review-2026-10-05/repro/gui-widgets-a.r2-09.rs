//! gui-widgets-a.r2-09: a focused KeyboardArea captures every key it has a
//! closure for, whether or not anything uses the key, so an enclosing
//! keyboard_area never hears it. keyboard_area.gx installs both closures
//! (`|_| null` defaults) and data_table's area answers every key it does
//! not use with Message::Nop (data_table/render.rs:511,514); the capture is
//! iced_keyboard_area.rs:139-150.
//!
//! Command (from the repository root, with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_r2_09.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_r2_09 -- --nocapture
//!
//! Each case compiles `keyboard_area(<handler>: |e| hits <- e ~ hits + 1,
//! <child>)` and drives it headlessly as GuiHandler::about_to_wait does
//! (before_view, UserInterface::build/update over a kept cache, every
//! Message::Call sent with gx.call, Nop dropped, other messages through
//! on_message); it left-clicks at (100, 40), inside every area of every
//! case, delivers one key event and reports what the event published, its
//! status and the outer handler's hit count.
//!
//! Expected: the outer handler hears every key nothing inside uses
//! (hits=1 in every case but ArrowDown, which the table uses).
//! Observed at c722befe (test FAILS):
//!   outer only, press "a": published Call, status [Captured], hits=1
//!   inner keyboard_area(#on_key_release: |e| null), press "a":
//!     published Call, status [Captured], hits=0
//!   inner keyboard_area() with no handlers, press "a":
//!     published Call, status [Captured], hits=0
//!   data_table inside, press Ctrl+F: published Nop, status [Captured], hits=0
//!   data_table inside, press ArrowDown (the table's key):
//!     published TableKey(Down), status [Captured], hits=0
//!   data_table inside, outer #on_key_release, release "a":
//!     published Call, status [Captured], hits=1
//!     (control: the table installs no release closure, so the same click
//!     did focus the outer area)

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, event, keyboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::{collections::VecDeque, time::Duration};
use tokio::sync::mpsc;

const REG: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const VIEWPORT: Size = Size::new(500.0, 200.0);

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

fn find_bind_id(env: &Env, module: &str, var: &str) -> Result<BindId> {
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if netidx::path::Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {module}::{var}")
}

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    _compiled: graphix_rt::CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    cursor: Point,
    hits_ref: Ref<NoExt>,
    hits: Value,
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
        let bid = find_bind_id(&compiled.env, "test", "hits")?;
        let hits_ref = gx.compile_ref(bid).await?;
        let hits = hits_ref.last.clone().unwrap_or(Value::Null);
        let mut h = Self {
            _ctx: ctx,
            gx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: Point::ORIGIN,
            hits_ref,
            hits,
        };
        h.pump(Duration::from_millis(300)).await?;
        let _ = h.events(&[]);
        h.pump(Duration::from_millis(100)).await?;
        let _ = h.events(&[]);
        Ok(h)
    }

    fn events(&mut self, events: &[Event]) -> (Vec<Message>, Vec<event::Status>) {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
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
                            if id == self.hits_ref.id {
                                self.hits = v.clone();
                            }
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return Ok(()),
            }
        }
    }

    /// Deliver messages as GuiHandler::about_to_wait does, then pump.
    async fn dispatch(&mut self, msgs: &[Message]) -> Result<()> {
        let mut pending: VecDeque<Message> = msgs.iter().cloned().collect();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => self.gx.call(id, args)?,
                other => {
                    let mut shell = MessageShell::new(self.cursor);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        self.pump(Duration::from_millis(300)).await
    }

    async fn click(&mut self, pos: Point) -> Result<()> {
        self.cursor = pos;
        let mut all = Vec::new();
        all.extend(
            self.events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]).0,
        );
        all.extend(
            self.events(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))])
                .0,
        );
        all.extend(
            self.events(&[Event::Mouse(mouse::Event::ButtonReleased(
                mouse::Button::Left,
            ))])
            .0,
        );
        self.dispatch(&all).await
    }

    /// Press (or release) a key; returns what it published, its status,
    /// and the outer handler's hit count after delivery.
    async fn key(&mut self, k: &Key) -> Result<String> {
        let ev = match k {
            Key::Press(key, modifiers) => {
                let text = match key {
                    keyboard::Key::Character(c) if modifiers.is_empty() => {
                        Some(c.clone())
                    }
                    _ => None,
                };
                keyboard::Event::KeyPressed {
                    key: key.clone(),
                    modified_key: key.clone(),
                    physical_key: keyboard::key::Physical::Unidentified(
                        keyboard::key::NativeCode::Unidentified,
                    ),
                    location: keyboard::Location::Standard,
                    modifiers: *modifiers,
                    text,
                    repeat: false,
                }
            }
            Key::Release(key) => keyboard::Event::KeyReleased {
                key: key.clone(),
                modified_key: key.clone(),
                physical_key: keyboard::key::Physical::Unidentified(
                    keyboard::key::NativeCode::Unidentified,
                ),
                location: keyboard::Location::Standard,
                modifiers: keyboard::Modifiers::empty(),
            },
        };
        let (msgs, statuses) = self.events(&[Event::Keyboard(ev)]);
        let published = describe(&msgs);
        self.dispatch(&msgs).await?;
        Ok(format!("published {published}, status {statuses:?}, hits={}", self.hits))
    }
}

enum Key {
    Press(keyboard::Key, keyboard::Modifiers),
    Release(keyboard::Key),
}

fn describe(msgs: &[Message]) -> String {
    let parts: Vec<String> = msgs
        .iter()
        .map(|m| match m {
            Message::Call(_, _) => "Call".to_string(),
            Message::Nop => "Nop".to_string(),
            other => format!("{other:?}"),
        })
        .collect();
    if parts.is_empty() { "nothing".to_string() } else { parts.join("; ") }
}

const PROGRAM: &str = "use gui::{keyboard_area::keyboard_area, text::text, \
    container::container, data_table::data_table};\n\
    let hits = 0;\n\
    let tbl = { rows: [\"r0\", \"r1\"], columns: [\"c0\", \"c1\"] };\n";

const ON_PRESS: &str = "#on_key_press";
const ON_RELEASE: &str = "#on_key_release";

fn fill(child: &str) -> String {
    format!("&container(#width: &`Fill, #height: &`Fill, {child})")
}

/// `keyboard_area(<handler>: |e| hits <- e ~ hits + 1, <child>)`; click
/// at (100, 40), inside every area of every case, then deliver `k`.
async fn case(name: &str, handler: &str, child: &str, k: Key) -> Result<i64> {
    let code = format!(
        "{PROGRAM}let result = keyboard_area({handler}: |e| hits <- e ~ hits + 1, {child})"
    );
    let mut h = H::new(&code).await?;
    h.click(Point::new(100.0, 40.0)).await?;
    let r = h.key(&k).await?;
    eprintln!("{name}: {r}");
    Ok(match h.hits {
        Value::I64(n) => n,
        _ => -1,
    })
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn outer_keyboard_area_hears_unused_keys() -> Result<()> {
    let a = || keyboard::Key::Character("a".into());
    let none = keyboard::Modifiers::empty();
    let inner_area = |args: &str| {
        fill(&format!("&keyboard_area({args}{})", fill("&text(&\"inner\")")))
    };
    let control = case(
        "outer only, press \"a\"",
        ON_PRESS,
        &fill("&text(&\"body\")"),
        Key::Press(a(), none),
    )
    .await?;
    let release_only = case(
        "inner keyboard_area(#on_key_release: |e| null), press \"a\"",
        ON_PRESS,
        &inner_area("#on_key_release: |e| null, "),
        Key::Press(a(), none),
    )
    .await?;
    let bare = case(
        "inner keyboard_area() with no handlers, press \"a\"",
        ON_PRESS,
        &inner_area(""),
        Key::Press(a(), none),
    )
    .await?;
    let table_ctrl_f = case(
        "data_table inside, press Ctrl+F",
        ON_PRESS,
        "&data_table(#table: &tbl)",
        Key::Press(keyboard::Key::Character("f".into()), keyboard::Modifiers::CTRL),
    )
    .await?;
    let table_down = case(
        "data_table inside, press ArrowDown (the table's key)",
        ON_PRESS,
        "&data_table(#table: &tbl)",
        Key::Press(keyboard::Key::Named(keyboard::key::Named::ArrowDown), none),
    )
    .await?;
    let table_release = case(
        "data_table inside, outer #on_key_release, release \"a\"",
        ON_RELEASE,
        "&data_table(#table: &tbl)",
        Key::Release(a()),
    )
    .await?;
    eprintln!(
        "hits: control={control} release_only={release_only} bare={bare} \
         table_ctrl_f={table_ctrl_f} table_down={table_down} \
         table_release={table_release}"
    );
    assert_eq!(control, 1, "control: the outer handler works alone");
    assert_eq!(table_release, 1, "control: the click focused the outer area too");
    assert_eq!(release_only, 1, "a release-only inner area swallowed a press");
    assert_eq!(bare, 1, "a handler-less inner area swallowed a press");
    assert_eq!(table_ctrl_f, 1, "data_table swallowed Ctrl+F as Nop");
    Ok(())
}
