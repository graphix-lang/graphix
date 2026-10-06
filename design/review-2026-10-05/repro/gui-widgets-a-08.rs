//! gui-widgets-a-08: a context menu opens on a right-click its child
//! already handled, and the stale `open` flag pops the menu up later,
//! unprompted (context_menu_widget.rs `OwnedContextMenu::update`).
//!
//! Command (with this file at
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_08.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_08 -- --nocapture
//!
//! Each test compiles a Graphix widget, drives it through a headless iced
//! `UserInterface` one frame per event as `GuiHandler::about_to_wait`
//! does (build, update, draw), runs the `Call` messages on the runtime and
//! watches `status`, which every menu action sets to its own label. A
//! menu opens at the right-click position, so after a right-click at
//! P = (10, 10) a left-click at Q = (30, 20) lands on its first item.
//!
//! Expected: once a menu item is chosen (or nothing was shown) no menu is
//! open, so a click at Q chooses nothing; all four tests pass.
//! Observed at c722befe (dev profile): 1 passed, 3 failed.
//!   single_menu_control (one menu, the control): passes.
//!     click at (30, 20) (the open menu's first item): 1 call(s), status = "Rename"
//!     click at (30, 20) (again; no menu should be open): 0 call(s), status = "Rename"
//!   nested_inner_item_reopens_outer: right-click the row, choose
//!   "Rename"; the outer "New folder" menu is then open at the same spot.
//!     click at (30, 20) (the open menu's first item): 1 call(s), status = "Rename"
//!     click at (30, 20) (again; no menu should be open): 1 call(s), status = "New folder"
//!   picklist_right_click_then_pick: right-click a pick list whose
//!   dropdown is open, pick "Alpha"; the context menu is then open.
//!     click at (10, 45) (the dropdown's first option): 1 call(s) ["[String(\"Alpha\")]"]
//!     click at (30, 20) (no menu should be open): 1 call(s), status = "Ctx"
//!   empty_items_then_items_arrive: right-click while `items` is empty
//!   (nothing shows); the items arrive two seconds later and the menu is
//!   open with no new right-click.
//!     click at (30, 20) (no menu should be open): 1 call(s), status = "Late"

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::{
    theme::GraphixTheme,
    widgets::{self, GuiW, Message},
};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, mouse, renderer::Style};
use iced_runtime::user_interface::{Cache, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::{path::Path, publisher::Value};
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

const P: Point = Point::new(10.0, 10.0);
const Q: Point = Point::new(30.0, 20.0);

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
        if Path::as_ref(&scope.0).ends_with(&suffix) {
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
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
    status_id: ExprId,
    status: Value,
    _status_ref: Ref<NoExt>,
    log: Vec<String>,
}

impl H {
    async fn new(name: &str, code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REG, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(literal!("{ mod test; test::result }")).await?;
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
        let widget = widgets::compile(gx.clone(), root).await.context("widget")?;
        let bid = find_bind_id(&compiled.env, "test", "status")?;
        let status_ref = gx.compile_ref(bid).await?;
        let mut h = Self {
            _ctx: ctx,
            gx,
            _compiled: compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: Cache::default(),
            cursor: Point::ORIGIN,
            status_id: status_ref.id,
            status: status_ref.last.clone().unwrap_or(Value::Null),
            _status_ref: status_ref,
            log: vec![format!("{name}:")],
        };
        h.drain(Duration::from_millis(300)).await?;
        let _ = h.frame(&[]);
        Ok(h)
    }

    fn say(&mut self, line: String) {
        self.log.push(format!("  {line}"));
    }

    /// Print this test's lines in one write: the tests run in parallel.
    fn dump(&self) {
        println!("{}\n", self.log.join("\n"));
    }

    fn apply(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        for ev in batch.drain(..) {
            if let GXEvent::Updated(id, v) = ev {
                if id == self.status_id {
                    self.status = v.clone();
                }
                let w = &mut self.widget;
                tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
            }
        }
        Ok(())
    }

    /// Apply updates until none arrives for `quiet`.
    async fn drain(&mut self, quiet: Duration) -> Result<()> {
        loop {
            match tokio::time::timeout(quiet, self.rx.recv()).await {
                Ok(Some(batch)) => self.apply(batch)?,
                Ok(None) => bail!("event channel closed"),
                Err(_) => return Ok(()),
            }
        }
    }

    /// Apply updates for `d`.
    async fn drain_for(&mut self, d: Duration) -> Result<()> {
        let deadline = tokio::time::Instant::now() + d;
        loop {
            match tokio::time::timeout_at(deadline, self.rx.recv()).await {
                Ok(Some(batch)) => self.apply(batch)?,
                Ok(None) => bail!("event channel closed"),
                Err(_) => return Ok(()),
            }
        }
    }

    /// One frame as the event loop renders it: build, update, draw.
    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(
            self.widget.view(),
            Size::new(400.0, 300.0),
            cache,
            &mut self.renderer,
        );
        let cursor = mouse::Cursor::Available(self.cursor);
        let mut messages = Vec::new();
        let _ = ui.update(
            events,
            cursor,
            &mut self.renderer,
            &mut clipboard::Null,
            &mut messages,
        );
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let style = Style { text_color: theme.palette().text };
        ui.draw(&mut self.renderer, &theme, &style, cursor);
        self.cache = ui.into_cache();
        messages
    }

    fn press(&mut self, at: Point, button: mouse::Button) -> Vec<Message> {
        self.cursor = at;
        let mut all =
            self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: at })]);
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(button))]));
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(button))]));
        all
    }

    fn right_click(&mut self, at: Point) {
        let n = self.press(at, mouse::Button::Right).len();
        self.say(format!("right-click at ({}, {}): {n} message(s)", at.x, at.y));
    }

    /// Left-click at `at` and run the calls it produced; the number of
    /// calls.
    async fn click(&mut self, at: Point, what: &str) -> Result<usize> {
        let msgs = self.press(at, mouse::Button::Left);
        let mut calls = Vec::new();
        for m in &msgs {
            if let Message::Call(id, args) = m {
                calls.push(format!("{:?}", &args[..]));
                self.gx.call(*id, args.clone())?;
            }
        }
        self.drain(Duration::from_millis(300)).await?;
        let status = self.status();
        self.say(format!(
            "click at ({}, {}) {what}: {} call(s) {calls:?}, status = {status:?}",
            at.x,
            at.y,
            calls.len()
        ));
        Ok(calls.len())
    }

    fn status(&self) -> String {
        match &self.status {
            Value::String(s) => s.to_string(),
            v => format!("{v}"),
        }
    }
}

/// Right-click at P, choose the first item at Q, click Q again; the call
/// count and status after each click.
async fn choose_then_click_again(h: &mut H) -> Result<((usize, String), (usize, String))> {
    h.right_click(P);
    let c1 = h.click(Q, "(the open menu's first item)").await?;
    let first = (c1, h.status());
    let c2 = h.click(Q, "(again; no menu should be open)").await?;
    let second = (c2, h.status());
    h.dump();
    Ok((first, second))
}

const SINGLE: &str = r#"
use gui::{column::column, text::text, menu};
let status = "none";
let result = menu::context_menu(
  &[menu::action(#on_click: |v| status <- v ~ "Rename", &"Rename")],
  &column(&[text(&"row")])
)
"#;

const NESTED: &str = r#"
use gui::{column::column, text::text, menu};
let status = "none";
let inner = menu::context_menu(
  &[menu::action(#on_click: |v| status <- v ~ "Rename", &"Rename")],
  &text(&"row")
);
let result = menu::context_menu(
  &[menu::action(#on_click: |v| status <- v ~ "New folder", &"New folder")],
  &column(&[inner])
)
"#;

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn single_menu_control() -> Result<()> {
    let mut h = H::new("single_menu_control", SINGLE).await?;
    let ((c1, s1), (c2, s2)) = choose_then_click_again(&mut h).await?;
    assert_eq!((c1, s1.as_str()), (1, "Rename"), "control: the menu did not open");
    assert_eq!((c2, s2.as_str()), (0, "Rename"), "control: the menu stayed open");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn nested_inner_item_reopens_outer() -> Result<()> {
    let mut h = H::new("nested_inner_item_reopens_outer", NESTED).await?;
    let ((c1, s1), (c2, s2)) = choose_then_click_again(&mut h).await?;
    assert_eq!((c1, s1.as_str()), (1, "Rename"), "the inner menu did not open");
    assert_eq!(
        (c2, s2.as_str()),
        (0, "Rename"),
        "after choosing Rename the outer menu was open at the same spot"
    );
    Ok(())
}

const PICKLIST: &str = r#"
use gui::{menu, pick_list::pick_list};
let status = "none";
let sel: [string, null] = null;
let result = menu::context_menu(
  &[menu::action(#on_click: |v| status <- v ~ "Ctx", &"Ctx")],
  &pick_list(#selected: &sel, #on_select: |s| sel <- s, &["Alpha", "Beta", "Gamma"])
)
"#;

/// Open the dropdown, right-click the pick list, pick "Alpha" in the
/// dropdown, then click at Q.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn picklist_right_click_then_pick() -> Result<()> {
    let mut h = H::new("picklist_right_click_then_pick", PICKLIST).await?;
    h.click(P, "(opens the dropdown)").await?;
    h.right_click(P);
    let c1 = h.click(Point::new(10.0, 45.0), "(the dropdown's first option)").await?;
    h.click(Q, "(no menu should be open)").await?;
    h.dump();
    assert_eq!(c1, 1, "the dropdown option was not picked");
    assert_eq!(h.status(), "none", "the context menu popped up after the dropdown closed");
    Ok(())
}

const EMPTY: &str = r#"
use gui::{text::text, menu};
let status = "none";
let items: Array<menu::MenuItem> = [];
items <- sys::time::after_idle(
  duration:2.s,
  [menu::action(#on_click: |v| status <- v ~ "Late", &"Late")]
);
let result = menu::context_menu(&items, &text(&"row"))
"#;

/// Right-click while the menu has no items; the items arrive two seconds
/// later; nothing else happens until a click at Q.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn empty_items_then_items_arrive() -> Result<()> {
    let mut h = H::new("empty_items_then_items_arrive", EMPTY).await?;
    h.right_click(P);
    h.drain_for(Duration::from_millis(2500)).await?;
    let _ = h.frame(&[]);
    h.say("the items arrived".into());
    h.click(Q, "(no menu should be open)").await?;
    h.dump();
    assert_eq!(h.status(), "none", "the menu popped up when its items arrived");
    Ok(())
}
