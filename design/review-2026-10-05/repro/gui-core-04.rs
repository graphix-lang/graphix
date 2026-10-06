//! gui-core-04: data-table messages carry no widget identity and the
//! event loop broadcasts every non-Call message to every window's
//! content, so every data table acts on every other table's clicks,
//! keys and edits, including calling its own on_edit with its own cell
//! path and the text typed into another table.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_core_04.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_core_04 -- --nocapture
//!
//! The program (CODE below) returns two windows: "one" holds tables A
//! and B side by side, "two" holds table C; each has an editable text
//! column "price" and an on_select. Each window is built as
//! `reconcile_windows` builds it (`ResolvedWindow::compile`, no OS
//! window). Input goes to window one only, through a headless iced
//! `UserInterface` over its content, as `GuiHandler::about_to_wait`
//! does; the messages it yields are drained exactly as
//! event_loop.rs:395-416 drains them (`H::dispatch`).
//!
//! Expected: only table A reacts; lb, lc, eb and ec stay "" (test
//! passes).
//! Observed at c722befe (test FAILS):
//!   STEP 1 click A (row 1, price): UI yields CellClick(1, "price")
//!     la="a1/price" lb="b1/price" lc="c1/price"
//!   STEP 2 click A's edit button: UI yields CellEdit(1, "price")
//!   STEP 3 type 42, Enter: UI yields CellEditInput("4"),
//!     CellEditInput("42"), CellEditSubmit
//!     ea="a1/price=42" eb="b1/price=42" ec="c1/price=42"
//!   STEP 4 ArrowDown in A: UI yields TableKey(Down)
//!     la="a2/price" lb="b2/price" lc="c2/price"
//!   panicked: tables B (same window) and C (other window) acted on
//!   table A's input

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    widgets::{self, Message, MessageShell},
    window::ResolvedWindow,
};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Point, Size, clipboard, keyboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::{global::GPooled, local::LPooled};
use std::{collections::VecDeque, time::Duration};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

/// Window "one" holds tables A and B side by side; window "two" holds
/// table C. All three have an editable text column named "price".
const CODE: &str = r#"
use gui::{window, row::row, data_table::data_table};
let la = "";
let lb = "";
let lc = "";
let ea = "";
let eb = "";
let ec = "";
let sel_a = [];
let sel_b = [];
let sel_c = [];
let edit_a = |#path: string, #value: Any| ea <- "[path]=[value]";
let edit_b = |#path: string, #value: Any| eb <- "[path]=[value]";
let edit_c = |#path: string, #value: Any| ec <- "[path]=[value]";
let ta = { rows: ["a0", "a1", "a2"], columns: [
    { name: "price", typ: `Text({ on_edit: edit_a }), display_name: null,
      source: &"", on_resize: &null, width: &null }
] };
let tb = { rows: ["b0", "b1", "b2"], columns: [
    { name: "price", typ: `Text({ on_edit: edit_b }), display_name: null,
      source: &"", on_resize: &null, width: &null }
] };
let tc = { rows: ["c0", "c1", "c2"], columns: [
    { name: "price", typ: `Text({ on_edit: edit_c }), display_name: null,
      source: &"", on_resize: &null, width: &null }
] };
let result = [
  &window(#title: &"one", &row(#width: &`Fill, #height: &`Fill, &[
    data_table(
      #selection: &sel_a,
      #on_select: |#path: string| { sel_a <- [path]; la <- path },
      #table: &ta
    ),
    data_table(
      #selection: &sel_b,
      #on_select: |#path: string| { sel_b <- [path]; lb <- path },
      #table: &tb
    )
  ])),
  &window(#title: &"two", &data_table(
    #selection: &sel_c,
    #on_select: |#path: string| { sel_c <- [path]; lc <- path },
    #table: &tc
  ))
]
"#;

const WATCH: &[&str] = &["la", "lb", "lc", "ea", "eb", "ec", "sel_a", "sel_b", "sel_c"];

/// One window as `reconcile_windows` builds it, minus the OS window.
struct Win {
    _wref: Ref<NoExt>,
    w: ResolvedWindow<NoExt>,
    cache: user_interface::Cache,
    viewport: Size,
    cursor: Point,
}

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    rt: tokio::runtime::Handle,
    renderer: widgets::Renderer,
    wins: Vec<Win>,
    watched: Vec<(String, ExprId, Value)>,
    refs: Vec<Ref<NoExt>>,
}

async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == target {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for the root value"),
        }
    }
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
        .expect("no GPU device");
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, iced_core::Font::DEFAULT, iced_core::Pixels(16.0))
}

fn key_pressed(key: keyboard::Key, text: Option<iced_core::SmolStr>) -> Event {
    Event::Keyboard(keyboard::Event::KeyPressed {
        key: key.clone(),
        modified_key: key,
        physical_key: keyboard::key::Physical::Unidentified(
            keyboard::key::NativeCode::Unidentified,
        ),
        location: keyboard::Location::Standard,
        modifiers: keyboard::Modifiers::empty(),
        text,
        repeat: false,
    })
}

impl H {
    async fn new() -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(CODE)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)])
                .await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root = wait_for_update(&mut rx, compiled.exprs[0].id).await?;
        // As `reconcile_windows`: the root is an array of window bind ids.
        let ids = root.cast_to::<LPooled<Vec<u64>>>().context("root array of bind ids")?;
        let mut wins = Vec::new();
        for &id in ids.iter() {
            let wref = gx.compile_ref(BindId::from(id)).await.context("window ref")?;
            let v = wref.last.clone().context("window has no value")?;
            let w = ResolvedWindow::compile(gx.clone(), v).await.context("resolve window")?;
            wins.push(Win {
                _wref: wref,
                w,
                cache: user_interface::Cache::default(),
                viewport: Size::new(800.0, 300.0),
                cursor: Point::ORIGIN,
            });
        }
        let mut h = Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            rt: tokio::runtime::Handle::current(),
            renderer: renderer().await,
            wins,
            watched: Vec::new(),
            refs: Vec::new(),
        };
        for n in WATCH {
            h.watch(n).await?;
        }
        h.drain().await?;
        Ok(h)
    }

    async fn watch(&mut self, var: &str) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, &format!("test::{var}"))?;
        let r = self.gx.compile_ref(bid).await?;
        self.watched.push((var.to_string(), r.id, r.last.clone().unwrap_or(Value::Null)));
        self.refs.push(r);
        Ok(())
    }

    fn get(&self, var: &str) -> String {
        match self.watched.iter().find(|(n, _, _)| n == var).map(|(_, _, v)| v) {
            Some(Value::String(s)) => s.to_string(),
            Some(v) => format!("{v}"),
            None => "<unwatched>".into(),
        }
    }

    fn show(&self, label: &str) {
        let vals: Vec<String> =
            WATCH.iter().map(|n| format!("{n}={:?}", self.get(n))).collect();
        println!("STATE {label}: {}", vals.join(" "));
    }

    /// As `ToGui::Update`: every window's content sees every update.
    async fn drain(&mut self) -> Result<()> {
        let timeout = tokio::time::sleep(Duration::from_millis(300));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            for (_, wid, slot) in self.watched.iter_mut() {
                                if *wid == id {
                                    *slot = v.clone();
                                }
                            }
                            let rt = self.rt.clone();
                            for win in self.wins.iter_mut() {
                                let c = &mut win.w.content;
                                tokio::task::block_in_place(|| c.handle_update(&rt, id, &v))?;
                            }
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(200)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    /// One `about_to_wait` UI pass over window `wi`: build, deliver
    /// `events`, return the messages iced produced.
    fn frame(&mut self, wi: usize, events: &[Event], at: Point) -> Vec<Message> {
        let Self { wins, renderer, .. } = self;
        let win = &mut wins[wi];
        win.cursor = at;
        win.w.content.before_view();
        let cache = std::mem::take(&mut win.cache);
        let mut ui = UserInterface::build(win.w.content.view(), win.viewport, cache, renderer);
        let mut messages: Vec<Message> = Vec::new();
        let mut clip = clipboard::Null;
        let _ = ui.update(events, mouse::Cursor::Available(at), renderer, &mut clip, &mut messages);
        win.cache = ui.into_cache();
        messages
    }

    /// The message drain of `about_to_wait`, verbatim in structure: a
    /// non-Call message goes to every window's content.
    fn dispatch(&mut self, msgs: &[Message]) -> Result<()> {
        let mut pending: VecDeque<Message> = msgs.iter().cloned().collect();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => self.gx.call(id, args)?,
                other => {
                    for win in self.wins.iter_mut() {
                        let mut shell = MessageShell::new(win.cursor);
                        win.w.content.on_message(&other, &mut shell);
                        pending.extend(shell.out.drain(..));
                    }
                }
            }
        }
        Ok(())
    }

    /// One frame of `events` in window `wi`, then the drain, then the
    /// graphix updates it caused.
    async fn step(&mut self, wi: usize, events: &[Event], at: Point) -> Result<Vec<Message>> {
        let msgs = self.frame(wi, events, at);
        self.dispatch(&msgs)?;
        self.drain().await?;
        Ok(msgs)
    }

    async fn click(&mut self, wi: usize, p: Point) -> Result<Vec<Message>> {
        let mut all = self
            .step(wi, &[Event::Mouse(mouse::Event::CursorMoved { position: p })], p)
            .await?;
        all.extend(
            self.step(wi, &[Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))], p)
                .await?,
        );
        all.extend(
            self.step(wi, &[Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left))], p)
                .await?,
        );
        Ok(all)
    }

    /// The first point of `pts` in window `wi` where a press and release
    /// in one frame produces a message `want` accepts. Nothing is
    /// dispatched.
    fn find(
        &mut self,
        wi: usize,
        pts: impl IntoIterator<Item = Point>,
        want: impl Fn(&Message) -> bool,
    ) -> Option<Point> {
        let press = [
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
        ];
        pts.into_iter().find(|p| self.frame(wi, &press, *p).iter().any(&want))
    }
}

fn is_cell_click(m: &Message) -> bool {
    matches!(m, Message::CellClick(1, c) if c.as_str() == "price")
}

fn is_cell_edit(m: &Message) -> bool {
    matches!(m, Message::CellEdit(1, c) if c.as_str() == "price")
}

#[tokio::test(flavor = "multi_thread")]
async fn tables_act_on_each_others_messages() -> Result<()> {
    let mut h = H::new().await?;
    println!("windows: {}", h.wins.len());
    for wi in 0..h.wins.len() {
        let _ = h.frame(wi, &[], Point::ORIGIN);
    }
    h.show("initial");

    // Locate table A's (row 1, "price") cell in window one: its row's y
    // band at x = 120, then its left edge on that band's middle.
    let top = h
        .find(0, (0..200).map(|y| Point::new(120.0, y as f32)), is_cell_click)
        .context("row 1 not found")?;
    let bottom = h
        .find(0, (0..200).rev().map(|y| Point::new(120.0, y as f32)), is_cell_click)
        .context("row 1 bottom not found")?;
    let y = (top.y + bottom.y) / 2.0;
    let left = h
        .find(0, (0..400).map(|x| Point::new(x as f32, y)), is_cell_click)
        .context("cell left edge not found")?;
    let cell = Point::new(left.x + 20.0, y);
    h.drain().await?;
    h.show("after locating the cell (nothing dispatched)");
    println!("table A's cell (row 1, \"price\"): rows y {}..{}, left edge x {}", top.y, bottom.y, left.x);

    println!("STEP 1: click table A's cell (row 1, price) in window one at {cell:?}");
    let msgs = h.click(0, cell).await?;
    println!("  window one's UI produced: {msgs:?}");
    h.show("after step 1");
    let (la1, lb1, lc1) = (h.get("la"), h.get("lb"), h.get("lc"));

    // A's cell is now selected and editable: a button that begins the edit.
    let edit_at = h
        .find(0, (0..80).map(|dx| Point::new(left.x + dx as f32, y)), is_cell_edit)
        .context("table A's cell did not become an edit button")?;
    h.drain().await?;
    println!("STEP 2: click table A's selected cell's edit button at {edit_at:?}");
    let msgs = h.click(0, edit_at).await?;
    println!("  window one's UI produced: {msgs:?}");
    h.show("after step 2");

    println!("STEP 3: click table A's cell editor, type 42, press Enter");
    let msgs = h.click(0, cell).await?;
    println!("  window one's UI produced: {msgs:?}");
    for ch in "42".chars() {
        let s: iced_core::SmolStr = ch.to_string().into();
        let msgs = h
            .step(0, &[key_pressed(keyboard::Key::Character(s.clone()), Some(s))], cell)
            .await?;
        println!("  typing {ch:?}: window one's UI produced: {msgs:?}");
    }
    let msgs = h
        .step(0, &[key_pressed(keyboard::Key::Named(keyboard::key::Named::Enter), None)], cell)
        .await?;
    println!("  Enter: window one's UI produced: {msgs:?}");
    h.show("after step 3");
    let (ea3, eb3, ec3) = (h.get("ea"), h.get("eb"), h.get("ec"));

    println!("STEP 4: press ArrowDown (table A's keyboard area has focus)");
    let msgs = h
        .step(0, &[key_pressed(keyboard::Key::Named(keyboard::key::Named::ArrowDown), None)], cell)
        .await?;
    println!("  window one's UI produced: {msgs:?}");
    h.show("after step 4");
    let (la4, lb4, lc4) = (h.get("la"), h.get("lb"), h.get("lc"));

    // the input itself must have reached table A
    if la1 != "a1/price" || ea3 != "a1/price=42" || la4 != "a2/price" {
        bail!("harness problem: table A did not see its own input ({la1:?} {ea3:?} {la4:?})");
    }
    println!(
        "EXPECTED: tables B and C never fire: lb=\"\" lc=\"\" after the click and the key, eb=\"\" ec=\"\" after the edit"
    );
    println!(
        "OBSERVED: click: lb={lb1:?} lc={lc1:?}; edit: eb={eb3:?} ec={ec3:?}; ArrowDown: lb={lb4:?} lc={lc4:?}"
    );
    assert!(
        lb1.is_empty() && lc1.is_empty() && eb3.is_empty() && ec3.is_empty() && lb4.is_empty() && lc4.is_empty(),
        "tables B (same window) and C (other window) acted on table A's input"
    );
    Ok(())
}
