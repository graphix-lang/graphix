//! gui-datatable-14: a focused data table captures every key press, and
//! the cell editor that Space (or the CellEdit button) opens has no focus.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_14.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_14 -- --nocapture
//!
//! A data table with an editable text column (`text_column(#on_edit)`),
//! `#on_select` fed back into `#selection` and `#on_activate`, sits in a
//! column under a header text, all inside a user `keyboard_area` whose
//! `#on_key_press` records the key (an app shortcut handler). The widget
//! is driven headlessly as the event loop drives it (UserInterface
//! build/update over a kept cache, then `GuiWidget::on_message` for what
//! a frame published, FIFO); an `Operation` reads the focus of every
//! focusable and the text of every text input.
//!
//! Expected (book/src/ui/gui/data_table.md: "`Space` on an editable cell
//! opens its editor"; text_column: "clicking a selected cell opens a text
//! field; `Enter` commits the typed value via `on_edit`"; a table has no
//! use for Ctrl+D): Ctrl+D with the table focused reaches the enclosing
//! keyboard_area; after Space, typing 42 and Enter commit "42" through
//! on_edit and do not fire on_activate; the CellEdit button's editor
//! takes typing too.
//! Observed at c722befe (test FAILS with all four failures):
//!   RESULT click header -> []
//!   RESULT Ctrl+S, table unfocused -> [Call([["key", "s"], ...])]
//!   RESULT   selected="" activated="" edited="" shortcut="s"
//!   RESULT click r0/c0 -> [CellClick(0, "c0")]
//!   RESULT   selected="r0/c0" activated="" edited="" shortcut="s"
//!   RESULT Ctrl+D, table focused -> [Nop]
//!   RESULT   selected="r0/c0" activated="" edited="" shortcut="s"
//!   RESULT Space -> [TableKey(Space)]
//!   RESULT   after Space: keyboard_area [0,0 500x200] focused=true;
//!            keyboard_area [0,21 500x179] focused=true;
//!            text_input [5,48 70x16] focused=false; text_input text=""
//!   RESULT type "4" -> [Nop]
//!   RESULT type "2" -> [Nop]
//!   RESULT Enter -> [TableKey(Enter)]
//!   RESULT   selected="r0/c0" activated="r0" edited="" shortcut="s"
//!   (control) RESULT click the editor -> []   then text_input focused=true
//!   RESULT type "4" -> [CellEditInput("4")]
//!   RESULT type "2" -> [CellEditInput("42")]
//!   RESULT Enter -> [CellEditSubmit]
//!   RESULT   selected="r0/c0" activated="r0" edited="r0/c0=42" shortcut="s"
//!   RESULT click selected r0/c0 -> [CellEdit(0, "c0")]
//!   RESULT   after CellEdit: ... text_input [5,48 70x16] focused=false
//!   RESULT type "7" -> [Nop]
//! The table's KeyboardArea (render.rs wrap_keyboard) maps every key it
//! does not use to Message::Nop and iced_keyboard_area.rs:139-143
//! captures it, so Ctrl+D never reaches the enclosing keyboard_area. The
//! editor TextInput (render.rs:638) starts unfocused and nothing focuses
//! it, so typed keys fall through to the table's KeyboardArea (Nop) and
//! Enter becomes TableKey(Enter), which fires on_activate
//! (events.rs:122) instead of CellEditSubmit. The control shows the
//! editor works once a click has focused it.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, NoExt, Ref};
use iced_core::{
    Event, Font, Pixels, Point, Rectangle, Size, clipboard, keyboard, mouse,
    widget::{
        Id, Operation,
        operation::{focusable::Focusable, text_input::TextInput},
    },
};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::{path::Path, publisher::Value};
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

const VIEWPORT: Size = Size::new(500.0, 200.0);

const PROGRAM: &str = r#"
use gui::{column::column, text::text};
use gui::data_table::{data_table, text_column};
use gui::keyboard_area::{KeyEvent, keyboard_area};
let sel: Array<string> = [];
let selected = "";
let activated = "";
let edited = "";
let shortcut = "";
let tbl = {
  rows: ["r0", "r1"],
  columns: [
    text_column(
      #name: "c0",
      #on_edit: |#path: string, #value: Any| edited <- "[path]=[value]",
      #source: &"old"
    )
  ]
};
let result = keyboard_area(
  #on_key_press: |e: KeyEvent| shortcut <- "[e.key]",
  &column(&[
    text(&"header"),
    data_table(
      #show_row_name: &false,
      #selection: &sel,
      #on_select: |#path: string| {
        sel <- [path];
        selected <- path
      },
      #on_activate: |#path: string| activated <- path,
      #table: &tbl
    )
  ])
)
"#;

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

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
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

/// What one frame's widget tree reports: texts, focusables (with
/// focus) and text inputs (with their text).
#[derive(Default)]
struct Probe {
    texts: Vec<(Rectangle, String)>,
    focusables: Vec<(Rectangle, bool)>,
    inputs: Vec<(Rectangle, String)>,
}

impl Operation for Probe {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation<()>)) {
        operate(self)
    }

    fn text(&mut self, _id: Option<&Id>, bounds: Rectangle, text: &str) {
        self.texts.push((bounds, text.to_string()));
    }

    fn focusable(&mut self, _id: Option<&Id>, bounds: Rectangle, state: &mut dyn Focusable) {
        self.focusables.push((bounds, state.is_focused()));
    }

    fn text_input(&mut self, _id: Option<&Id>, bounds: Rectangle, state: &mut dyn TextInput) {
        self.inputs.push((bounds, state.text().to_string()));
    }
}

impl Probe {
    fn describe(&self) -> String {
        let fmt = |b: &Rectangle| format!("[{:.0},{:.0} {:.0}x{:.0}]", b.x, b.y, b.width, b.height);
        let mut s = String::new();
        for (b, focused) in &self.focusables {
            let kind = if self.inputs.iter().any(|(ib, _)| ib == b) {
                "text_input"
            } else {
                "keyboard_area"
            };
            s.push_str(&format!("{kind} {} focused={focused}; ", fmt(b)));
        }
        for (b, t) in &self.inputs {
            s.push_str(&format!("text_input {} text={t:?}; ", fmt(b)));
        }
        s
    }
}

struct H {
    ctx: TestCtx,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    cursor: Point,
    watched: AHashMap<ExprId, (String, Value)>,
    refs: Vec<Ref<NoExt>>,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
                .await?;
        let compiled = ctx
            .rt
            .compile(literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let id = compiled.exprs[0].id;
        let root = loop {
            let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
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
        let mut h = Self {
            ctx,
            compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: Point::ORIGIN,
            watched: AHashMap::default(),
            refs: vec![],
        };
        for v in ["selected", "activated", "edited", "shortcut"] {
            h.watch(v).await?;
        }
        h.drain().await?;
        let _ = h.events(&[]);
        Ok(h)
    }

    async fn watch(&mut self, var: &str) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, &format!("test::{var}"))?;
        let r = self.ctx.rt.compile_ref(bid).await?;
        let v = r.last.clone().unwrap_or(Value::Null);
        self.watched.insert(r.id, (var.to_string(), v));
        self.refs.push(r);
        Ok(())
    }

    fn value(&self, var: &str) -> Value {
        self.watched
            .values()
            .find(|(n, _)| n == var)
            .map(|(_, v)| v.clone())
            .unwrap_or(Value::Null)
    }

    fn state(&self) -> String {
        format!(
            "selected={} activated={} edited={} shortcut={}",
            self.value("selected"),
            self.value("activated"),
            self.value("edited"),
            self.value("shortcut")
        )
    }

    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(200), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    if let Some(slot) = self.watched.get_mut(&i) {
                        slot.1 = v.clone();
                    }
                    let widget = &mut self.widget;
                    tokio::task::block_in_place(|| widget.handle_update(&rt, i, &v))?;
                }
            }
        }
        self.widget.before_view();
        Ok(())
    }

    fn events(&mut self, events: &[Event]) -> Vec<Message> {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    /// One frame with no events, then an operation over the tree.
    fn probe(&mut self) -> Probe {
        let _ = self.events(&[]);
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut p = Probe::default();
        ui.operate(&self.renderer, &mut p);
        self.cache = ui.into_cache();
        p
    }

    /// What a left click at `pos` publishes, one UI frame per event.
    fn click(&mut self, pos: Point) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.events(&[Event::Mouse(mouse::Event::ButtonPressed(
            mouse::Button::Left,
        ))]));
        all.extend(self.events(&[Event::Mouse(mouse::Event::ButtonReleased(
            mouse::Button::Left,
        ))]));
        all
    }

    fn key(
        &mut self,
        key: keyboard::Key,
        modifiers: keyboard::Modifiers,
        text: Option<&str>,
    ) -> Vec<Message> {
        self.events(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: key.clone(),
            modified_key: key,
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers,
            text: text.map(|t| t.into()),
            repeat: false,
        })])
    }

    fn named(&mut self, k: keyboard::key::Named) -> Vec<Message> {
        self.key(keyboard::Key::Named(k), keyboard::Modifiers::empty(), None)
    }

    fn ctrl(&mut self, c: &str) -> Vec<Message> {
        self.key(keyboard::Key::Character(c.into()), keyboard::Modifiers::CTRL, None)
    }

    fn type_text(&mut self, s: &str) -> Vec<Message> {
        let mut all = vec![];
        for ch in s.chars() {
            let t = ch.to_string();
            all.extend(self.key(
                keyboard::Key::Character(t.as_str().into()),
                keyboard::Modifiers::empty(),
                Some(&t),
            ));
        }
        all
    }

    /// Deliver published messages as `GuiHandler::about_to_wait` does
    /// (FIFO, `on_message` for widget messages), then drain.
    async fn dispatch(&mut self, msgs: Vec<Message>) -> Result<()> {
        let mut pending: std::collections::VecDeque<Message> = msgs.into();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => self.ctx.rt.call(id, args)?,
                other => {
                    let mut shell = MessageShell::new(Point::ORIGIN);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        self.drain().await
    }
}

/// The messages that matter, without the resize-drag MouseArea's
/// per-move/per-release traffic.
fn shown(msgs: &[Message]) -> String {
    let v: Vec<String> = msgs
        .iter()
        .filter(|m| !matches!(m, Message::ColumnResizeMove(_) | Message::ColumnResizeEnd))
        .map(|m| match m {
            Message::Call(_, args) => format!(
                "Call({})",
                args.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ")
            ),
            m => format!("{m:?}"),
        })
        .collect();
    format!("[{}]", v.join(", "))
}

fn first_text(p: &Probe, t: &str) -> Option<Rectangle> {
    p.texts
        .iter()
        .filter(|(_, s)| s == t)
        .min_by(|(a, _), (b, _)| a.y.total_cmp(&b.y))
        .map(|(b, _)| *b)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn table_keys_and_cell_editor_focus() -> Result<()> {
    let mut failures: Vec<String> = vec![];
    let mut h = H::new(PROGRAM).await?;
    let p = h.probe();
    eprintln!(
        "RESULT texts: {:?}",
        p.texts.iter().map(|(b, t)| (t.clone(), b.x, b.y)).collect::<Vec<_>>()
    );
    let header = first_text(&p, "header").context("header text")?;
    let cell = first_text(&p, "old").context("row 0 cell text")?;

    // 1. Control: focus outside the table, inside the keyboard_area.
    let m = h.click(header.center());
    eprintln!("RESULT click header -> {}", shown(&m));
    h.dispatch(m).await?;
    let m = h.ctrl("s");
    eprintln!("RESULT Ctrl+S, table unfocused -> {}", shown(&m));
    h.dispatch(m).await?;
    eprintln!("RESULT   {}", h.state());

    // 2. Click the row 0 cell: selects it, focuses the table.
    let m = h.click(cell.center());
    eprintln!("RESULT click r0/c0 -> {}", shown(&m));
    h.dispatch(m).await?;
    eprintln!("RESULT   {}", h.state());
    let m = h.ctrl("d");
    eprintln!("RESULT Ctrl+D, table focused -> {}", shown(&m));
    h.dispatch(m).await?;
    eprintln!("RESULT   {}", h.state());
    if h.value("shortcut") != Value::String(literal!("d")) {
        failures.push(format!(
            "Ctrl+D with the table focused never reached the enclosing keyboard_area ({})",
            h.state()
        ));
    }

    // 3. Space opens the editor; type 42; Enter.
    let m = h.named(keyboard::key::Named::Space);
    eprintln!("RESULT Space -> {}", shown(&m));
    h.dispatch(m).await?;
    let p = h.probe();
    eprintln!("RESULT   after Space: {}", p.describe());
    for c in ["4", "2"] {
        let m = h.type_text(c);
        eprintln!("RESULT type {c:?} -> {}", shown(&m));
        h.dispatch(m).await?;
    }
    let p = h.probe();
    eprintln!("RESULT   after typing: {}", p.describe());
    let m = h.named(keyboard::key::Named::Enter);
    eprintln!("RESULT Enter -> {}", shown(&m));
    h.dispatch(m).await?;
    eprintln!("RESULT   {}", h.state());
    let committed = match h.value("edited") {
        Value::String(s) => s.starts_with("r0/c0=") && s.ends_with("42"),
        _ => false,
    };
    if !committed {
        failures.push(format!(
            "Space, \"42\", Enter did not commit 42 through on_edit ({})",
            h.state()
        ));
    }
    if h.value("activated") != Value::String(literal!("")) {
        failures.push(format!("Enter during the edit fired on_activate ({})", h.state()));
    }

    // 4. Control: a click into the editor focuses it; then typing and
    //    Enter commit.
    let p = h.probe();
    match p.inputs.first() {
        None => failures.push("no text input after Space".into()),
        Some((b, _)) => {
            let b = *b;
            let m = h.click(b.center());
            eprintln!("RESULT click the editor -> {}", shown(&m));
            h.dispatch(m).await?;
            let p = h.probe();
            eprintln!("RESULT   after clicking the editor: {}", p.describe());
            for c in ["4", "2"] {
                let m = h.type_text(c);
                eprintln!("RESULT type {c:?} -> {}", shown(&m));
                h.dispatch(m).await?;
            }
            let m = h.named(keyboard::key::Named::Enter);
            eprintln!("RESULT Enter -> {}", shown(&m));
            h.dispatch(m).await?;
            eprintln!("RESULT   {}", h.state());
        }
    }

    // 5. The CellEdit button (a click on the selected editable cell).
    let p = h.probe();
    let cell = first_text(&p, "old").context("row 0 cell text after submit")?;
    let m = h.click(cell.center());
    eprintln!("RESULT click selected r0/c0 -> {}", shown(&m));
    h.dispatch(m).await?;
    let p = h.probe();
    eprintln!("RESULT   after CellEdit: {}", p.describe());
    let m = h.type_text("7");
    eprintln!("RESULT type \"7\" -> {}", shown(&m));
    if !m.iter().any(|m| matches!(m, Message::CellEditInput(_))) {
        failures.push(format!(
            "typing into the editor the CellEdit button opened published {}",
            shown(&m)
        ));
    }
    h.dispatch(m).await?;

    assert!(failures.is_empty(), "{failures:#?}");
    Ok(())
}
