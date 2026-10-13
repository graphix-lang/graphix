//! gui-widgets-a-04: a context menu around a widget with a tooltip never opens.
//! Command (copy to stdlib/graphix-package-gui/tests/review_gui_widgets_a_04.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_04 -- --nocapture context_menu_
//! Expected: both context_menu_over_tooltip_opens and
//!   context_menu_control_no_tooltip_opens pick the item (picked = true).
//! Observed at 7324207f: "tooltip child: item click published 0 call(s),
//!   picked = false" (FAILED); "no tooltip: item click published 1 call(s),
//!   picked = true" (ok). The other tests in this harness belong to other
//!   gui-widgets-a findings.
//! gui-widgets-a review probes (2026-10-12, HEAD 7324207f). Each test
//! drives the widget tree headlessly the way `GuiHandler` does
//! (widgets::compile, handle_update per runtime update,
//! UserInterface::build/update/operate/draw per frame) and asserts the
//! intended behaviour.
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a -- --nocapture
//!
//! Observed at 7324207f (dev profile), 8 of 9 FAIL:
//!   children_survive_a_failed_child: before ["a"]; after the list gains
//!     a child whose size is 0: texts [] (the kept "a" is gone); after
//!     the size becomes 16.0: still []
//!   handler_change_redraws_button: on_press null -> fn, handle_update
//!     returned false (no redraw), though the button now draws enabled
//!   second_context_menu_closes_the_first: one left click published 2
//!     calls (copy_a = 1 and del_b = 1)
//!   one_shortcut_one_action: one Ctrl+D published 2 calls (del_a = 1,
//!     del_b = 1)
//!   operations_reach_context_menu_child: focusables 1 texts ["plain"]
//!     (the text_input and text inside the context menu are unseen)
//!   canvas_arc_to_reversal_draws: panic, lyon_path path.rs:811
//!     `assertion failed: p.x.is_finite()` (release: iced_wgpu
//!     geometry.rs:182 `.expect("Tessellate path.")` on PositionIsNaN)
//!   canvas_huge_circle_draws: same lyon assertion (inf from 3e38 + 3e38)
//!   canvas_huge_line_draws: passes
//!   context_menu_over_tooltip_opens: right-click on a tooltip'd child,
//!     then click the item: 0 calls, picked = false (the menu never
//!     opened; the same clicks without a tooltip pick the item, see
//!     second_context_menu_closes_the_first)

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
use iced_core::{
    Event, Font, Pixels, Point, Rectangle, Size, clipboard, keyboard, mouse,
    renderer::Style,
    widget::{Id, Operation, operation::Focusable},
};
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

const VIEWPORT: Size = Size::new(400.0, 300.0);

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

async fn first_value(rx: &mut Rx, target: ExprId) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(10));
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

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: Rx,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: Cache,
    cursor: Point,
    watched: AHashMap<ExprId, Value>,
    names: AHashMap<&'static str, ExprId>,
    refs: Vec<Ref<NoExt>>,
    errors: Vec<String>,
}

impl H {
    async fn new(code: &str, watch: &[&'static str]) -> Result<H> {
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
        let renderer = renderer().await;
        let mut h = H {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            widget,
            renderer,
            cache: Cache::default(),
            cursor: Point::ORIGIN,
            watched: AHashMap::default(),
            names: AHashMap::default(),
            refs: vec![],
            errors: vec![],
        };
        for &name in watch {
            let bid = find_bind_id(&h.compiled.env, name)?;
            let r = h.gx.compile_ref(bid).await?;
            h.watched.insert(r.id, r.last.clone().unwrap_or(Value::Null));
            h.names.insert(name, r.id);
            h.refs.push(r);
        }
        h.drain_for(Duration::from_millis(150)).await;
        let _ = h.frame(&[]);
        Ok(h)
    }

    /// Deliver every update for `dur`, as the event loop does: an error is
    /// logged and the next update goes on. Returns whether any update
    /// asked for a redraw.
    async fn drain_for(&mut self, dur: Duration) -> bool {
        let rt = tokio::runtime::Handle::current();
        let deadline = tokio::time::Instant::now() + dur;
        let mut changed = false;
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if let Some(slot) = self.watched.get_mut(&id) {
                                *slot = v.clone();
                            }
                            match tokio::task::block_in_place(|| self.widget.handle_update(&rt, id, &v)) {
                                Ok(c) => changed |= c,
                                Err(e) => self.errors.push(format!("{e:#}")),
                            }
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline) => return changed,
            }
        }
    }

    fn frame(&mut self, events: &[Event]) -> Vec<Message> {
        self.widget.before_view();
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

    fn draw(&mut self) {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let style = Style { text_color: theme.palette().text };
        ui.draw(&mut self.renderer, &theme, &style, mouse::Cursor::Unavailable);
        self.cache = ui.into_cache();
    }

    fn operate(&mut self, op: &mut dyn Operation) {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        ui.operate(&self.renderer, op);
        self.cache = ui.into_cache();
    }

    fn click(&mut self, pos: Point, button: mouse::Button) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(button))]));
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(button))]));
        all
    }

    async fn dispatch(&mut self, msgs: Vec<Message>) -> Result<usize> {
        let mut n = 0;
        for m in msgs {
            if let Message::Call(id, args) = m {
                self.gx.call(id, args)?;
                n += 1;
            }
        }
        self.drain_for(Duration::from_millis(300)).await;
        Ok(n)
    }

    fn get(&self, name: &str) -> Value {
        self.names
            .get(name)
            .and_then(|id| self.watched.get(id))
            .cloned()
            .unwrap_or(Value::Null)
    }
}

/// What an operation sees: every text and every focusable.
#[derive(Default)]
struct Seen {
    texts: Vec<String>,
    focusables: usize,
}

impl Operation for Seen {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation<()>)) {
        operate(self)
    }

    fn text(&mut self, _id: Option<&Id>, _bounds: Rectangle, text: &str) {
        self.texts.push(text.to_string())
    }

    fn focusable(&mut self, _id: Option<&Id>, _bounds: Rectangle, _s: &mut dyn Focusable) {
        self.focusables += 1
    }
}

/// A list's new child that fails to compile (a text size of 0, refused by
/// `TextSizeV`) takes every kept sibling with it, and the size becoming
/// valid later brings nothing back.
#[tokio::test(flavor = "multi_thread")]
async fn children_survive_a_failed_child() -> Result<()> {
    let code = r#"use gui::{column::column, text::text};
let bad = 0.0;
let show = false;
show <- sys::time::after_idle(duration:1500.ms, true);
bad <- sys::time::after_idle(duration:2500.ms, 16.0);
let a = text(&"a");
let b = text(#size: &bad, &"b");
let kids = select show { true => [a, b], false => [a] };
let result = column(&kids)"#;
    let mut h = H::new(code, &[]).await?;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("before: texts {:?} errors {:?}", seen.texts, h.errors);
    h.drain_for(Duration::from_millis(2000)).await;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("after show <- true (b's size is 0): texts {:?}, errors {:?}", seen.texts, h.errors);
    h.drain_for(Duration::from_millis(1500)).await;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("after bad <- 16.0: texts {:?}, errors {:?}", seen.texts, h.errors);
    assert!(
        seen.texts.contains(&"a".to_string()),
        "the kept child `a` vanished: texts {:?}",
        seen.texts
    );
    Ok(())
}

/// A button whose on_press goes from null to a function changes how it
/// draws (enabled), so the update must ask for a redraw.
#[tokio::test(flavor = "multi_thread")]
async fn handler_change_redraws_button() -> Result<()> {
    let code = r#"use gui::{button::button, text::text};
let hit = false;
let on = false;
on <- sys::time::after_idle(duration:200.ms, true);
let result = button(#on_press: select on { true => |e| hit <- e ~ true, false => null }, &text(&"go"))"#;
    let mut h = H::new(code, &[]).await?;
    let changed = h.drain_for(Duration::from_millis(800)).await;
    eprintln!("handle_update asked for a redraw: {changed}; errors {:?}", h.errors);
    let msgs = h.click(Point::new(10.0, 10.0), mouse::Button::Left);
    let calls = msgs.iter().filter(|m| matches!(m, Message::Call(..))).count();
    eprintln!("a click on the (now enabled) button publishes {calls} call(s)");
    assert!(changed, "the handler arrived but no redraw was asked for");
    Ok(())
}

const CTX2: &str = r#"use gui::{column::column, menu, text::text, container::container};
let del_a = 0;
let copy_a = 0;
let del_b = 0;
let copy_b = 0;
let row = |label, on_del, on_copy| menu::context_menu(
  &[
    menu::action(#shortcut: &menu::shortcut(#ctrl: true, "d")$, #on_click: on_del, &"Delete"),
    menu::action(#on_click: on_copy, &"Copy")
  ],
  &container(#width: &`Fill, #height: &`Fixed(40.0), &text(&label))
);
let result = column(&[
  row("A", |c| del_a <- c ~ del_a + 1, |c| copy_a <- c ~ copy_a + 1),
  row("B", |c| del_b <- c ~ del_b + 1, |c| copy_b <- c ~ copy_b + 1)
])"#;

const CTX2_VARS: &[&str] = &["test::del_a", "test::copy_a", "test::del_b", "test::copy_b"];

/// Right-click row A, then row B: A's menu stays open beside B's, and one
/// left click where they overlap fires an item of each.
#[tokio::test(flavor = "multi_thread")]
async fn second_context_menu_closes_the_first() -> Result<()> {
    let mut h = H::new(CTX2, CTX2_VARS).await?;
    let _ = h.click(Point::new(50.0, 20.0), mouse::Button::Right);
    let _ = h.click(Point::new(20.0, 60.0), mouse::Button::Right);
    let msgs = h.click(Point::new(100.0, 70.0), mouse::Button::Left);
    let n = h.dispatch(msgs).await?;
    let vals: Vec<_> = CTX2_VARS.iter().map(|v| (*v, h.get(v))).collect();
    eprintln!("one left click published {n} call(s): {vals:?}");
    assert!(n <= 1, "one click fired {n} menu actions: {vals:?}");
    Ok(())
}

/// Same two open menus: one Ctrl+D runs both rows' Delete.
#[tokio::test(flavor = "multi_thread")]
async fn one_shortcut_one_action() -> Result<()> {
    let mut h = H::new(CTX2, CTX2_VARS).await?;
    let _ = h.click(Point::new(50.0, 20.0), mouse::Button::Right);
    let _ = h.click(Point::new(300.0, 60.0), mouse::Button::Right);
    let key = keyboard::Key::Character("d".into());
    let msgs = h.frame(&[Event::Keyboard(keyboard::Event::KeyPressed {
        key: key.clone(),
        modified_key: key,
        physical_key: keyboard::key::Physical::Code(keyboard::key::Code::KeyD),
        location: keyboard::Location::Standard,
        modifiers: keyboard::Modifiers::CTRL,
        text: None,
        repeat: false,
    })]);
    let n = h.dispatch(msgs).await?;
    let vals: Vec<_> = CTX2_VARS.iter().map(|v| (*v, h.get(v))).collect();
    eprintln!("one Ctrl+D published {n} call(s): {vals:?}");
    assert!(n <= 1, "one shortcut fired {n} menu actions: {vals:?}");
    Ok(())
}

/// Operations (WidgetOp::Focus / ScrollTo, iced's focus traversal) must
/// reach a context menu's child as they reach any container's.
#[tokio::test(flavor = "multi_thread")]
async fn operations_reach_context_menu_child() -> Result<()> {
    let code = r#"use gui::{column::column, menu, text_input::text_input, text::text};
let result = column(&[
  text_input(&"outside"),
  text(&"plain"),
  menu::context_menu(&[menu::action(&"Copy")], &column(&[text_input(&"inside"), text(&"wrapped")]))
])"#;
    let mut h = H::new(code, &[]).await?;
    let mut seen = Seen::default();
    h.operate(&mut seen);
    eprintln!("focusables {} texts {:?}", seen.focusables, seen.texts);
    assert_eq!(seen.focusables, 2, "the text_input in the context menu is invisible");
    assert!(seen.texts.contains(&"wrapped".to_string()));
    Ok(())
}

/// A context menu around a widget with a tooltip: the hovered tooltip is
/// a child overlay, and `OwnedContextMenu::overlay` closes the menu
/// whenever a child has one, so the menu never opens.
#[tokio::test(flavor = "multi_thread")]
async fn context_menu_over_tooltip_opens() -> Result<()> {
    let code = r#"use gui::{container::container, menu, text::text, tooltip::tooltip};
let picked = false;
let result = menu::context_menu(
  &[menu::action(#on_click: |c| picked <- c ~ true, &"Delete")],
  &tooltip(#tip: &text(&"tip"), &container(#width: &`Fill, #height: &`Fill, &text(&"x")))
)"#;
    let mut h = H::new(code, &["test::picked"]).await?;
    let _ = h.click(Point::new(50.0, 50.0), mouse::Button::Right);
    let msgs = h.click(Point::new(80.0, 60.0), mouse::Button::Left);
    let n = h.dispatch(msgs).await?;
    eprintln!("tooltip child: item click published {n} call(s), picked = {}", h.get("test::picked"));
    assert_eq!(h.get("test::picked"), Value::Bool(true), "the menu never opened");
    Ok(())
}

fn canvas(shape: &str) -> String {
    format!(
        "use gui::{{color, canvas::canvas}};\nlet shapes = [{shape}];\n\
         let result = canvas(#width: &`Fixed(200.0), #height: &`Fixed(100.0), &shapes)"
    )
}

/// Finite inputs only (the `Finite` guard passes all of them): an `ArcTo`
/// whose path doubles back on itself.
#[tokio::test(flavor = "multi_thread")]
async fn canvas_arc_to_reversal_draws() -> Result<()> {
    let code = canvas(
        "`Path({segments: [`MoveTo({x: 0.0, y: 0.0}), \
         `ArcTo({a: {x: 100.0, y: 0.0}, b: {x: 0.0, y: 0.00001}, radius: 10.0}), `Close], \
         fill: color(#r: 1.0)$, stroke: null})",
    );
    let mut h = H::new(&code, &[]).await?;
    h.draw();
    eprintln!("arc_to reversal drew");
    Ok(())
}

/// Finite inputs whose sum overflows f32.
#[tokio::test(flavor = "multi_thread")]
async fn canvas_huge_circle_draws() -> Result<()> {
    let code = canvas(
        "`Circle({center: {x: 300000000000000000000000000000000000000.0, y: 50.0}, \
         radius: 300000000000000000000000000000000000000.0, fill: color(#g: 1.0)$, stroke: null})",
    );
    let mut h = H::new(&code, &[]).await?;
    h.draw();
    eprintln!("huge circle drew");
    Ok(())
}

/// Finite endpoints whose difference overflows f32.
#[tokio::test(flavor = "multi_thread")]
async fn canvas_huge_line_draws() -> Result<()> {
    let code = canvas(
        "`Line({from: {x: -300000000000000000000000000000000000000.0, y: 10.0}, \
         to: {x: 300000000000000000000000000000000000000.0, y: 20.0}, \
         color: color(#b: 1.0)$, width: 2.0})",
    );
    let mut h = H::new(&code, &[]).await?;
    h.draw();
    eprintln!("huge line drew");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn context_menu_control_no_tooltip_opens() -> Result<()> {
    let code = r#"use gui::{container::container, menu, text::text};
let picked = false;
let result = menu::context_menu(
  &[menu::action(#on_click: |c| picked <- c ~ true, &"Delete")],
  &container(#width: &`Fill, #height: &`Fill, &text(&"x"))
)"#;
    let mut h = H::new(code, &["test::picked"]).await?;
    let _ = h.click(Point::new(50.0, 50.0), mouse::Button::Right);
    let msgs = h.click(Point::new(80.0, 60.0), mouse::Button::Left);
    let n = h.dispatch(msgs).await?;
    eprintln!("no tooltip: item click published {n} call(s), picked = {}", h.get("test::picked"));
    assert_eq!(h.get("test::picked"), Value::Bool(true), "the menu never opened");
    Ok(())
}
