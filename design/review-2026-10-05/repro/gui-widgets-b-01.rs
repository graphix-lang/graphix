//! gui-widgets-b-01: identical widget records recompile whole subtrees;
//! place-ref args trigger it on every write to the root.
//!
//! A container recompiles its children whenever its children ref fires,
//! without comparing the delivered record with the one it compiled
//! (widgets/mod.rs:347, `update_child!` at :49, stack, grid, table,
//! menu_bar, context_menu, window.rs:147/201). A place reference's VALUE
//! re-fires on every fire of its root (graphix-compiler/src/node/
//! bind.rs:977 `moved = root.tag().triggers()`), so a widget built over
//! `&doc.text` re-delivers an identical record on every write to `doc`,
//! and the column rebuilds the editor: a new `Content` with the cursor at
//! (0, 0).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_01.rs; no
//! window opens, the renderer is headless wgpu):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_01 -- --nocapture
//!
//! Observed at c722befe:
//!   a_place_ref_tick: over 1.2 s, the children array was delivered 11
//!     times (11 equal to the compiled one) and the column rebuilt its
//!     children 11 times. EXPECTED 0 rebuilds. FAILED.
//!   b_plain_ref_tick: delivered 0 times, 0 rebuilds. ok
//!   c_place_ref_typing: typed "abc" (3 on_edit calls), the column rebuilt
//!     the editor 3 times. EXPECTED "abc", OBSERVED "cba". FAILED.
//!   d_plain_ref_typing: "abc", 0 rebuilds. ok
//!
//! The re-fire itself, without a GUI (`graphix --no-cache`, 50 ms timer
//! writing `doc.a`, 420 ms): `count(text_editor(&doc.text))` and
//! `count(window(#title: &doc.title, ..))` reach 9, the same over a plain
//! `&x` stays 1.
//!
//! Each case compiles the program's `result` (a column) as the event loop
//! compiles a window's content (`widgets::compile`) and feeds it every
//! runtime update (`handle_update`). A rebuild is seen as a new address
//! of a child widget. The typing cases drive the editor through a
//! headless iced `UserInterface` (as the crate's own InteractionHarness
//! does): click to focus, then one key per frame, each edit's `Call`
//! sent to the runtime and its echo delivered before the next key, as
//! the event loop does at typing speed.
//!
//! a_place_ref_tick: a 100 ms timer writes `doc.a`; the editor shows
//!   `&doc.text`. EXPECTED 0 rebuilds; FAILS: the children array is
//!   delivered on every tick, equal to the compiled one, and the column
//!   rebuilds both children each time.
//! b_plain_ref_tick (control): the editor shows `&content`. 0 rebuilds.
//! c_place_ref_typing: type "abc" into the `&doc.text` editor. EXPECTED
//!   "abc"; FAILS with "cba": each keystroke's echo rebuilds the editor
//!   and the next key lands at (0, 0).
//! d_plain_ref_typing (control): the same over `&content` gives "abc".

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, GuiWidget, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Point, Size, clipboard, keyboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
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

const PLACE_TICK: &str = r#"
use gui::{column::column, text::text, text_editor::text_editor};
let doc = {a: 0, text: "hello"};
let clock = sys::time::timer(duration:100.ms, true);
doc <- clock ~ {doc with a: doc.a + 1};
let result = column(&[
  text_editor(#on_edit: |s| doc <- s ~ {doc with text: s}, &doc.text),
  text(&"static")
])
"#;

const PLAIN_TICK: &str = r#"
use gui::{column::column, text::text, text_editor::text_editor};
let doc = {a: 0, text: "hello"};
let content = "hello";
let clock = sys::time::timer(duration:100.ms, true);
doc <- clock ~ {doc with a: doc.a + 1};
let result = column(&[
  text_editor(#on_edit: |s| content <- s, &content),
  text(&"static")
])
"#;

const PLACE_TYPE: &str = r#"
use gui::{column::column, text::text, text_editor::text_editor};
let doc = {a: 0, text: ""};
let shown = doc.text;
let result = column(&[
  text_editor(#on_edit: |s| doc <- s ~ {doc with text: s}, &doc.text),
  text(&"static")
])
"#;

const PLAIN_TYPE: &str = r#"
use gui::{column::column, text::text, text_editor::text_editor};
let content = "";
let result = column(&[
  text_editor(#on_edit: |s| content <- s, &content),
  text(&"static")
])
"#;

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
    let (module, var) = name.split_once("::").context("module::var")?;
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name}")
}

/// The bind id a widget record's field refers to.
fn field_ref(record: &Value, field: &str) -> Result<BindId> {
    let (_tag, fields) = record.clone().cast_to::<(arcstr::ArcStr, Value)>()?;
    let pairs = fields.cast_to::<Vec<(arcstr::ArcStr, Value)>>()?;
    for (name, v) in pairs {
        if name == field {
            if let Value::U64(id) = v {
                return Ok(BindId::from(id));
            }
        }
    }
    bail!("no field {field} in {record}")
}

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    rt: tokio::runtime::Handle,
    widget: GuiW<NoExt>,
    /// The column's children ref, watched beside the widget's own, and
    /// the array the widget compiled.
    children: (ExprId, Value),
    watched: Option<(ExprId, Value)>,
    _refs: Vec<Ref<NoExt>>,
}

#[derive(Default, Debug)]
struct Pumped {
    /// Deliveries of the column's children array.
    deliveries: usize,
    /// Of those, equal to the array the column compiled.
    identical: usize,
    /// Updates after which a child widget had a new address.
    rebuilds: usize,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled: CompRes<NoExt> = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root_id = compiled.exprs[0].id;
        let timeout = tokio::time::sleep(Duration::from_secs(30));
        tokio::pin!(timeout);
        let root = loop {
            tokio::select! {
                biased;
                Some(mut batch) = rx.recv() => {
                    if let Some(v) = batch.drain(..).find_map(|e| match e {
                        GXEvent::Updated(id, v) if id == root_id => Some(v),
                        _ => None,
                    }) {
                        break v;
                    }
                }
                _ = &mut timeout => bail!("timeout waiting for the root value"),
            }
        };
        let children_ref = gx.compile_ref(field_ref(&root, "children")?).await?;
        let compiled_children = children_ref.last.clone().context("no children value")?;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            rt: tokio::runtime::Handle::current(),
            widget,
            children: (children_ref.id, compiled_children),
            watched: None,
            _refs: vec![children_ref],
        })
    }

    async fn watch(&mut self, name: &str) -> Result<()> {
        let r = self.gx.compile_ref(find_bind_id(&self.compiled.env, name)?).await?;
        self.watched = Some((r.id, r.last.clone().unwrap_or(Value::Null)));
        self._refs.push(r);
        Ok(())
    }

    fn watched(&self) -> String {
        match self.watched.as_ref().map(|(_, v)| v) {
            Some(Value::String(s)) => s.to_string(),
            Some(v) => format!("{v}"),
            None => String::new(),
        }
    }

    fn child_addrs(&self) -> Vec<usize> {
        self.widget
            .children()
            .iter()
            .map(|c| &**c as *const dyn GuiWidget<NoExt> as *const () as usize)
            .collect()
    }

    /// Deliver updates for `d` as the event loop does.
    async fn pump(&mut self, d: Duration) -> Result<Pumped> {
        let mut p = Pumped::default();
        let deadline = tokio::time::Instant::now() + d;
        loop {
            let tick = tokio::time::sleep(Duration::from_millis(20));
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            if id == self.children.0 {
                                p.deliveries += 1;
                                if v == self.children.1 {
                                    p.identical += 1;
                                }
                            }
                            if let Some((wid, wv)) = self.watched.as_mut() && *wid == id {
                                *wv = v.clone();
                            }
                            let before = self.child_addrs();
                            let (w, rt) = (&mut self.widget, &self.rt);
                            tokio::task::block_in_place(|| w.handle_update(rt, id, &v))?;
                            if self.child_addrs() != before {
                                p.rebuilds += 1;
                            }
                        }
                    }
                }
                _ = tick => {}
            }
            if tokio::time::Instant::now() >= deadline {
                return Ok(p);
            }
        }
    }

    /// Dispatch iced messages as `GuiHandler::about_to_wait` does.
    fn dispatch(&mut self, msgs: Vec<Message>) -> Result<usize> {
        let mut calls = 0;
        let mut pending: std::collections::VecDeque<Message> = msgs.into_iter().collect();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => {
                    calls += 1;
                    self.gx.call(id, args)?;
                }
                other => {
                    let mut shell = MessageShell::new(Point::ORIGIN);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        Ok(calls)
    }
}

/// A headless renderer and an iced `UserInterface` over the widget, as
/// the crate's InteractionHarness builds them.
struct Ui {
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    viewport: Size,
    cursor: Point,
}

impl Ui {
    async fn new(viewport: Size) -> Self {
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
                .expect("no GPU adapter available (not even software fallback)"),
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
        let renderer = iced_wgpu::Renderer::new(
            engine,
            iced_core::Font::DEFAULT,
            iced_core::Pixels(16.0),
        );
        Self { renderer, cache: user_interface::Cache::default(), viewport, cursor: Point::ORIGIN }
    }

    fn frame(&mut self, w: &mut GuiW<NoExt>, events: &[Event]) -> Vec<Message> {
        w.before_view();
        let element = w.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, self.viewport, cache, &mut self.renderer);
        let mut messages = Vec::new();
        let mut clip = clipboard::Null;
        let cursor = mouse::Cursor::Available(self.cursor);
        let _ = ui.update(events, cursor, &mut self.renderer, &mut clip, &mut messages);
        self.cache = ui.into_cache();
        messages
    }
}

fn key(ch: char) -> Event {
    let s: iced_core::SmolStr = ch.to_string().into();
    Event::Keyboard(keyboard::Event::KeyPressed {
        key: keyboard::Key::Character(s.clone()),
        modified_key: keyboard::Key::Character(s.clone()),
        physical_key: keyboard::key::Physical::Unidentified(
            keyboard::key::NativeCode::Unidentified,
        ),
        location: keyboard::Location::Standard,
        modifiers: keyboard::Modifiers::empty(),
        text: Some(s),
        repeat: false,
    })
}

/// Click the editor, type `text` one key per frame with each edit's
/// echo delivered before the next key; the program's text, the edits
/// sent and the rebuilds seen.
async fn type_into(code: &str, watch: &str, text: &str) -> Result<(String, usize, usize)> {
    let mut h = H::new(code).await?;
    h.watch(watch).await?;
    let mut ui = Ui::new(Size::new(300.0, 100.0)).await;
    let at = Point::new(10.0, 10.0);
    ui.cursor = at;
    for ev in [
        Event::Mouse(mouse::Event::CursorMoved { position: at }),
        Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
    ] {
        let msgs = ui.frame(&mut h.widget, &[ev]);
        h.dispatch(msgs)?;
    }
    h.pump(Duration::from_millis(100)).await?;
    let (mut calls, mut rebuilds) = (0, 0);
    for ch in text.chars() {
        let msgs = ui.frame(&mut h.widget, &[key(ch)]);
        calls += h.dispatch(msgs)?;
        rebuilds += h.pump(Duration::from_millis(250)).await?.rebuilds;
    }
    h.pump(Duration::from_millis(200)).await?;
    Ok((h.watched(), calls, rebuilds))
}

#[tokio::test(flavor = "multi_thread")]
async fn a_place_ref_tick() -> Result<()> {
    let mut h = H::new(PLACE_TICK).await?;
    let p = h.pump(Duration::from_millis(1200)).await?;
    println!(
        "a_place_ref_tick: over 1.2 s, the children array was delivered {} times ({} equal to \
         the compiled one) and the column rebuilt its children {} times",
        p.deliveries, p.identical, p.rebuilds
    );
    println!("EXPECTED: 0 rebuilds (no record changed)\nOBSERVED: {} rebuilds", p.rebuilds);
    assert_eq!(p.rebuilds, 0, "identical records rebuilt the column's children");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn b_plain_ref_tick() -> Result<()> {
    let mut h = H::new(PLAIN_TICK).await?;
    let p = h.pump(Duration::from_millis(1200)).await?;
    println!(
        "b_plain_ref_tick: over 1.2 s, the children array was delivered {} times and the column \
         rebuilt its children {} times",
        p.deliveries, p.rebuilds
    );
    assert_eq!(p.rebuilds, 0);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn c_place_ref_typing() -> Result<()> {
    let (got, calls, rebuilds) = type_into(PLACE_TYPE, "test::shown", "abc").await?;
    println!(
        "c_place_ref_typing: typed \"abc\" ({calls} on_edit calls), the column rebuilt the \
         editor {rebuilds} times\nEXPECTED: \"abc\"\nOBSERVED: {got:?}"
    );
    assert_eq!(calls, 3, "harness: every key should edit");
    assert_eq!(got, "abc");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn d_plain_ref_typing() -> Result<()> {
    let (got, calls, rebuilds) = type_into(PLAIN_TYPE, "test::content", "abc").await?;
    println!(
        "d_plain_ref_typing: typed \"abc\" ({calls} on_edit calls), {rebuilds} rebuilds, the \
         program holds {got:?}"
    );
    assert_eq!(calls, 3, "harness: every key should edit");
    assert_eq!(got, "abc");
    Ok(())
}
