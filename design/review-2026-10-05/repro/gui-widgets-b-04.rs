//! gui-widgets-b-04: text_editor's echo check remembers one push, so a
//! stale echo or a re-delivery of the current text rebuilds `Content`
//! and puts the cursor back at (0,0).
//!
//! text_editor.rs:103 compares a content update with `last_set_text`,
//! which holds only the LAST text pushed through `on_edit` and is taken
//! by the next content update of any kind; every other update rebuilds
//! `Content::with_text` (cursor (0,0), selection and scroll dropped).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_04.rs):
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_04 -- --nocapture
//!
//! Each case drives the widget the way `event_loop.rs` does: a frame
//! builds the iced UI from `view()` over a headless wgpu renderer and
//! feeds it events, the frame's messages go through `on_message` (FIFO,
//! `Call`s to the runtime) as in `about_to_wait`, and runtime batches
//! go to `handle_update` as `ToGui::Update` does. The editor is clicked
//! (focus) and typed into; after the last echo a "|" is typed, so the
//! final text shows where the cursor was. Program: `let c = ""; let
//! result = text_editor(#on_edit: |s| c <- s, &c)` (the book's usage).
//!
//! a_control_echo_between_keys: "a", echo, "b", echo, "c", echo. c ends
//!   "abc|". Passes.
//! b_two_keys_in_one_frame: "a" and "b" arrive in one event-loop batch
//!   (both are processed before the echo of "a"), then "c". Expected
//!   "abc|"; observed "c|ab": the echo "a" mismatched the remembered
//!   "ab" and rebuilt the editor, the echo "ab" found nothing remembered
//!   and rebuilt it again with the cursor at (0,0). FAILS.
//! c_third_key_between_echoes: as b, but "c" is typed after the echo of
//!   "a" and before the echo of "ab". Expected "abc|"; observed "|ca":
//!   the typed "b" is lost. FAILS.
//! d0_place_control: `&doc.text` over `let doc = {n: 0, text: ""}`,
//!   typed with an echo after every key. "abc|". Passes.
//! d_place_redelivery: as d0, but `bump` (a write of doc.n, as a button
//!   would) runs between "ab" and "c". The place's mirror re-delivers the
//!   unchanged "ab", which rebuilds the editor. Expected "abc|"; observed
//!   "c|ab". FAILS.
//! e_normalizing_on_edit: `|s| c <- str::to_upper(s)`, an echo after
//!   every key. Every echo rebuilds with the cursor at (0,0), so the text
//!   comes out reversed: expected "ABC|", observed "|CBA". FAILS.
//!
//! Observed at HEAD c722befe (2 passed, 4 failed), texts pushed through
//! on_edit / content updates delivered to handle_update:
//!   a: ["a","ab","abc","abc|"] / ["a","ab","abc","abc|"]         c = "abc|"
//!   b: ["a","ab","cab","c|ab"] / ["a","ab","cab","c|ab"]         c = "c|ab"
//!   c: ["a","ab","ca","|ca"]   / ["a","ab","ca","|ca"]           c = "|ca"
//!   d: ["a","ab","cab","c|ab"] / ["a","ab","ab","cab","c|ab"]    doc.text = "c|ab"
//!   e: ["a","bA","cBA","|CBA"] / ["A","BA","CBA","|CBA"]         c = "|CBA"

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell, Renderer};
use graphix_rt::{Callable, CallableId, CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Point, Size, clipboard, keyboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::{collections::VecDeque, time::Duration};
use tokio::sync::{OnceCell, mpsc};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const PLAIN: &str = r#"
use gui::text_editor::text_editor;
let c = "";
let result = text_editor(#on_edit: |s| c <- s, &c)
"#;

const PLACE: &str = r#"
use gui::text_editor::text_editor;
let doc = {n: 0, text: ""};
let bump = |x: i64| doc <- x ~ {doc with n: doc.n + 1};
let result = text_editor(#on_edit: |s| doc <- s ~ {doc with text: s}, &doc.text)
"#;

const UPPER: &str = r#"
use gui::text_editor::text_editor;
let c = "";
let result = text_editor(#on_edit: |s| c <- str::to_upper(s), &c)
"#;

const VIEWPORT: Size = Size::new(300.0, 100.0);
const HIT: Point = Point::new(10.0, 10.0);

struct Gpu {
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

static GPU: OnceCell<Gpu> = OnceCell::const_new();

async fn renderer() -> Result<Renderer> {
    let gpu = GPU
        .get_or_try_init(|| async {
            let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
                backends: wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY),
                ..Default::default()
            });
            let adapter = instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    compatible_surface: None,
                    force_fallback_adapter: false,
                    ..Default::default()
                })
                .await
                .context("no gpu adapter")?;
            let (device, queue) = adapter
                .request_device(&wgpu::DeviceDescriptor::default())
                .await
                .context("no gpu device")?;
            Ok::<_, anyhow::Error>(Gpu { adapter, device, queue })
        })
        .await?;
    let engine = iced_wgpu::Engine::new(
        &gpu.adapter,
        gpu.device.clone(),
        gpu.queue.clone(),
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    Ok(iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0)))
}

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

fn key(c: char) -> Event {
    let s: iced_core::SmolStr = c.to_string().into();
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

fn string_of(v: &Value) -> String {
    match v {
        Value::String(s) => s.to_string(),
        v => format!("{v}"),
    }
}

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    rt: tokio::runtime::Handle,
    widget: GuiW<NoExt>,
    renderer: Renderer,
    cache: user_interface::Cache,
    /// The content ref's id, read off the editor's own `EditorAction`s.
    content_id: Option<ExprId>,
    /// Every text the editor pushed through `on_edit`, in order.
    pushed: Vec<String>,
    /// Every content update delivered to the editor, in order.
    delivered: Vec<String>,
    watch_id: ExprId,
    watched: Option<Value>,
    _refs: Vec<Ref<NoExt>>,
    _callables: Vec<Callable<NoExt>>,
}

impl H {
    async fn new(code: &str, watch: &str) -> Result<Self> {
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
        let wref = gx.compile_ref(find_bind_id(&compiled.env, watch)?).await?;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            rt: tokio::runtime::Handle::current(),
            widget,
            renderer: renderer().await?,
            cache: user_interface::Cache::default(),
            content_id: None,
            pushed: vec![],
            delivered: vec![],
            watch_id: wref.id,
            watched: wref.last.clone(),
            _refs: vec![wref],
            _callables: vec![],
        })
    }

    /// One frame of `about_to_wait`: build the UI from `view()`, feed it
    /// `events`, and return the messages it produced.
    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        self.widget.before_view();
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let mut clip = clipboard::Null;
        let _ = ui.update(
            events,
            mouse::Cursor::Available(HIT),
            &mut self.renderer,
            &mut clip,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    /// The message loop of `about_to_wait`: FIFO, widget messages to
    /// `on_message`, calls to the runtime.
    fn dispatch(&mut self, msgs: Vec<Message>) -> Result<()> {
        let mut pending: VecDeque<Message> = msgs.into();
        while let Some(m) = pending.pop_front() {
            match m {
                Message::Nop => {}
                Message::Call(id, args) => {
                    if let Some(v) = args.iter().next() {
                        self.pushed.push(string_of(v));
                    }
                    self.gx.call(id, args)?;
                }
                other => {
                    if let Message::EditorAction(id, _) = &other {
                        self.content_id = Some(*id);
                    }
                    let mut shell = MessageShell::new(HIT);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        Ok(())
    }

    fn click(&mut self) -> Result<()> {
        let mut msgs =
            self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: HIT })]);
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))]),
        );
        msgs.extend(
            self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))]),
        );
        self.dispatch(msgs)
    }

    /// Key presses for every char of `text` in ONE frame: what the event
    /// loop processes when the keys arrive before its next
    /// `about_to_wait`.
    fn type_one_frame(&mut self, text: &str) -> Result<()> {
        let events: Vec<Event> = text.chars().map(key).collect();
        let msgs = self.frame(&events);
        if msgs.is_empty() {
            bail!("the editor produced no messages for {text:?}");
        }
        self.dispatch(msgs)
    }

    fn deliver_batch(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<bool> {
        let mut saw_content = false;
        for ev in batch.drain(..) {
            if let GXEvent::Updated(id, v) = ev {
                if Some(id) == self.content_id {
                    saw_content = true;
                    self.delivered.push(string_of(&v));
                }
                if id == self.watch_id {
                    self.watched = Some(v.clone());
                }
                let (w, rt) = (&mut self.widget, &self.rt);
                tokio::task::block_in_place(|| w.handle_update(rt, id, &v))?;
            }
        }
        Ok(saw_content)
    }

    /// Runtime batches to the widget, as `ToGui::Update` delivers them,
    /// until 200 ms pass with none.
    async fn drain(&mut self) -> Result<()> {
        while let Ok(Some(batch)) =
            tokio::time::timeout(Duration::from_millis(200), self.rx.recv()).await
        {
            self.deliver_batch(batch)?;
        }
        Ok(())
    }

    /// Runtime batches to the widget up to and including the first one
    /// that updates the content.
    async fn deliver_first_content_update(&mut self) -> Result<()> {
        loop {
            match tokio::time::timeout(Duration::from_secs(5), self.rx.recv()).await {
                Ok(Some(batch)) => {
                    if self.deliver_batch(batch)? {
                        return Ok(());
                    }
                }
                _ => bail!("no content update within 5 s"),
            }
        }
    }

    async fn call(&mut self, name: &str, arg: Value) -> Result<()> {
        let id = self.callable(name).await?;
        self.gx.call(id, ValArray::from_iter([arg]))?;
        Ok(())
    }

    async fn callable(&mut self, name: &str) -> Result<CallableId> {
        let r = self.gx.compile_ref(find_bind_id(&self.compiled.env, name)?).await?;
        let val = r.last.clone().with_context(|| format!("no value for {name}"))?;
        let cb = self.gx.compile_callable(val).await?;
        let id = cb.id();
        self._refs.push(r);
        self._callables.push(cb);
        Ok(id)
    }

    fn report(&self, case: &str, expected: &str) -> String {
        let got = self.watched.as_ref().map(string_of).unwrap_or_default();
        println!(
            "{case}:\n  pushed through on_edit: {:?}\n  content updates delivered: {:?}\n  \
             EXPECTED final value: {expected:?}\n  OBSERVED final value: {got:?}",
            self.pushed, self.delivered
        );
        got
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_control_echo_between_keys() -> Result<()> {
    let mut h = H::new(PLAIN, "test::c").await?;
    h.click()?;
    for ch in ["a", "b", "c", "|"] {
        h.type_one_frame(ch)?;
        h.drain().await?;
    }
    let got = h.report("a_control_echo_between_keys", "abc|");
    assert_eq!(got, "abc|");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn b_two_keys_in_one_frame() -> Result<()> {
    let mut h = H::new(PLAIN, "test::c").await?;
    h.click()?;
    h.type_one_frame("ab")?;
    h.drain().await?;
    h.type_one_frame("c")?;
    h.drain().await?;
    h.type_one_frame("|")?;
    h.drain().await?;
    let got = h.report("b_two_keys_in_one_frame", "abc|");
    assert_eq!(got, "abc|", "the echo of the first key reset the editor");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn c_third_key_between_echoes() -> Result<()> {
    let mut h = H::new(PLAIN, "test::c").await?;
    h.click()?;
    h.type_one_frame("ab")?;
    h.deliver_first_content_update().await?;
    h.type_one_frame("c")?;
    h.drain().await?;
    h.type_one_frame("|")?;
    h.drain().await?;
    let got = h.report("c_third_key_between_echoes", "abc|");
    assert_eq!(got, "abc|", "stale echoes overwrote the typed text");
    Ok(())
}

fn doc_text(h: &H) -> String {
    h.watched.as_ref().map(|v| format!("{v}")).unwrap_or_default()
}

#[tokio::test(flavor = "multi_thread")]
async fn d0_place_control() -> Result<()> {
    let mut h = H::new(PLACE, "test::doc").await?;
    h.click()?;
    for ch in ["a", "b", "c", "|"] {
        h.type_one_frame(ch)?;
        h.drain().await?;
    }
    h.report("d0_place_control", "{n: 0, text: \"abc|\"}");
    let doc = doc_text(&h);
    assert!(doc.contains("\"abc|\""), "doc = {doc}");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn d_place_redelivery() -> Result<()> {
    let mut h = H::new(PLACE, "test::doc").await?;
    h.click()?;
    for ch in ["a", "b"] {
        h.type_one_frame(ch)?;
        h.drain().await?;
    }
    h.call("test::bump", Value::I64(1)).await?;
    h.drain().await?;
    for ch in ["c", "|"] {
        h.type_one_frame(ch)?;
        h.drain().await?;
    }
    h.report("d_place_redelivery", "{n: 1, text: \"abc|\"}");
    let doc = doc_text(&h);
    assert!(
        doc.contains("\"abc|\""),
        "a write of doc.n re-delivered doc.text and reset the editor: doc = {doc}"
    );
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn e_normalizing_on_edit() -> Result<()> {
    let mut h = H::new(UPPER, "test::c").await?;
    h.click()?;
    for ch in ["a", "b", "c", "|"] {
        h.type_one_frame(ch)?;
        h.drain().await?;
    }
    let got = h.report("e_normalizing_on_edit", "ABC|");
    assert_eq!(got, "ABC|", "every normalized echo reset the cursor");
    Ok(())
}
