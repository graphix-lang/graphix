//! gui-widgets-a-06: an open menu lets clicks through. `MenuOverlay`
//! (menu_bar_widget.rs) has no `mouse_interaction`, so iced hands the
//! base layer the real cursor while the pointer is over the menu, and
//! its `update` captures only presses on enabled actions.
//!
//! Command (with this file at
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_06.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_06 -- --nocapture
//!
//! Drives the widget tree headlessly the way `GuiHandler::about_to_wait`
//! does (UserInterface::build + update per frame, one event per frame,
//! the cache carried between frames), dispatches every `Message::Call`
//! through the runtime and reads the program's variables back.
//!
//! Cases (viewport 400x300, default text size 16, so a context menu
//! opened at (50,50) has "Delete" at y 50..78 and a divider at 78..87;
//! a menu bar's "File" dropdown opens at y 32 with "Quit" at 32..60):
//!   ctx_enabled  control: click an ENABLED item over a Fill button
//!   ctx_disabled click the disabled item over the button
//!   ctx_divider  click the divider over the button
//!   bar_enabled  control: menu bar, click the ENABLED item over a checkbox
//!   bar_checkbox menu bar, click the disabled item over a checkbox
//!   bar_button   menu bar, click the disabled item over a Fill button
//!   ctx_wheel    wheel over an open context menu above a scrollable
//!
//! Expected: a press on a menu never reaches what lies under the menu
//! (hit, toggled and scrolled stay false; the controls pick their item).
//! Observed at c722befe (the test fails on its last assert):
//!   ctx_enabled : hit=false picked=true calls=1
//!   ctx_disabled: hit=true picked=false calls=1
//!   ctx_divider : hit=true picked=false calls=1
//!   bar_enabled : toggled=false picked=true calls=1
//!   bar_checkbox: toggled=true picked=false calls=1
//!   bar_button  : hit=false picked=false calls=0
//!   ctx_wheel   : scrolled=true picked=false calls=1
//!   input reached the widget under an open menu:
//!     ["ctx_disabled", "ctx_divider", "bar_checkbox", "ctx_wheel"]
//! The controls pick their item and nothing else, so the overlay is
//! routed. bar_button is spared only because the bar's own close
//! captures the press before the Button (which checks capture) sees it;
//! a checkbox does not check capture.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, mouse};
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
        };
        for &name in watch {
            let bid = find_bind_id(&h.compiled.env, name)?;
            let r = h.gx.compile_ref(bid).await?;
            h.watched.insert(r.id, r.last.clone().unwrap_or(Value::Null));
            h.names.insert(name, r.id);
            h.refs.push(r);
        }
        h.drain().await?;
        let _ = h.frame(&[]);
        Ok(h)
    }

    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        let timeout = tokio::time::sleep(Duration::from_millis(300));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if let Some(slot) = self.watched.get_mut(&id) {
                                *slot = v.clone();
                            }
                            self.widget.handle_update(&rt, id, &v)?;
                        }
                    }
                    timeout
                        .as_mut()
                        .reset(tokio::time::Instant::now() + Duration::from_millis(150));
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
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

    fn click(&mut self, pos: Point, button: mouse::Button) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonPressed(button))]));
        all.extend(self.frame(&[Event::Mouse(mouse::Event::ButtonReleased(button))]));
        all
    }

    fn wheel(&mut self, pos: Point) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.frame(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.frame(&[Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Lines { x: 0.0, y: -3.0 },
        })]));
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
        self.drain().await?;
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

fn calls(msgs: &[Message]) -> usize {
    msgs.iter().filter(|m| matches!(m, Message::Call(..))).count()
}

const CTX: &str = r#"use gui::{button::button, menu, text::text};
let hit = false;
let picked = false;
let result = menu::context_menu(
    &[
        menu::action(#disabled: &DIS, #on_click: |c| picked <- c ~ true, &"Delete"),
        menu::divider()
    ],
    &button(#width: &`Fill, #height: &`Fill, #on_press: |c| hit <- c ~ true, &text(&"under"))
)"#;

const BAR_CB: &str = r#"use gui::{checkbox::checkbox, column::column, menu};
let toggled = false;
let picked = false;
let result = column(&[
    menu::bar(&[
        menu::menu(&"File", &[
            menu::action(#disabled: &DIS, #on_click: |c| picked <- c ~ true, &"Quit"),
            menu::divider()
        ])
    ]),
    checkbox(#width: &`Fill, #label: &"under", #on_toggle: |v| toggled <- v ~ true, &false)
])"#;

const BAR_BTN: &str = r#"use gui::{button::button, column::column, menu, text::text};
let hit = false;
let picked = false;
let result = column(&[
    menu::bar(&[
        menu::menu(&"File", &[
            menu::action(#disabled: &true, #on_click: |c| picked <- c ~ true, &"Quit"),
            menu::divider()
        ])
    ]),
    button(#width: &`Fill, #height: &`Fill, #on_press: |c| hit <- c ~ true, &text(&"under"))
])"#;

const SCROLL: &str = r#"use gui::{menu, scrollable::scrollable, space::space};
let scrolled = false;
let picked = false;
let result = menu::context_menu(
    &[menu::action(#on_click: |c| picked <- c ~ true, &"Copy")],
    &scrollable(
        #width: &`Fill,
        #height: &`Fill,
        #on_scroll: |p| scrolled <- p ~ true,
        &space(#height: &`Fixed(5000.0))
    )
)"#;

/// Right-click at (50,50) to open the context menu, then left-click `at`.
async fn ctx_case(disabled: bool, at: Point) -> Result<(Value, Value, usize)> {
    let code = CTX.replace("DIS", if disabled { "true" } else { "false" });
    let mut h = H::new(&code, &["test::hit", "test::picked"]).await?;
    let open = h.click(Point::new(50.0, 50.0), mouse::Button::Right);
    assert_eq!(calls(&open), 0, "the right click itself published a call");
    let msgs = h.click(at, mouse::Button::Left);
    let n = h.dispatch(msgs).await?;
    Ok((h.get("test::hit"), h.get("test::picked"), n))
}

/// Left-click "File" at (10,10) to open the dropdown, then left-click `at`.
async fn bar_case(code: &str, under: &'static str, at: Point) -> Result<(Value, Value, usize)> {
    let mut h = H::new(code, &[under, "test::picked"]).await?;
    let open = h.click(Point::new(10.0, 10.0), mouse::Button::Left);
    assert_eq!(calls(&open), 0, "opening the menu published a call");
    let msgs = h.click(at, mouse::Button::Left);
    let n = h.dispatch(msgs).await?;
    Ok((h.get(under), h.get("test::picked"), n))
}

#[tokio::test(flavor = "current_thread")]
async fn open_menu_lets_clicks_through() -> Result<()> {
    let t = Value::Bool(true);
    let f = Value::Bool(false);
    let on_item = Point::new(80.0, 60.0);
    let on_divider = Point::new(80.0, 82.0);
    let (hit, picked, n) = ctx_case(false, on_item).await?;
    eprintln!("ctx_enabled : hit={hit} picked={picked} calls={n}");
    assert_eq!((&hit, &picked), (&f, &t), "control failed: harness does not route the overlay");
    let ctx_disabled = ctx_case(true, on_item).await?;
    eprintln!(
        "ctx_disabled: hit={} picked={} calls={}",
        ctx_disabled.0, ctx_disabled.1, ctx_disabled.2
    );
    let ctx_divider = ctx_case(true, on_divider).await?;
    eprintln!(
        "ctx_divider : hit={} picked={} calls={}",
        ctx_divider.0, ctx_divider.1, ctx_divider.2
    );
    let under_bar = Point::new(20.0, 40.0);
    let (toggled, picked, n) =
        bar_case(&BAR_CB.replace("DIS", "false"), "test::toggled", under_bar).await?;
    eprintln!("bar_enabled : toggled={toggled} picked={picked} calls={n}");
    assert_eq!((&toggled, &picked), (&f, &t), "control failed: harness does not route the overlay");
    let bar_checkbox =
        bar_case(&BAR_CB.replace("DIS", "true"), "test::toggled", under_bar).await?;
    eprintln!(
        "bar_checkbox: toggled={} picked={} calls={}",
        bar_checkbox.0, bar_checkbox.1, bar_checkbox.2
    );
    let bar_button = bar_case(BAR_BTN, "test::hit", under_bar).await?;
    eprintln!(
        "bar_button  : hit={} picked={} calls={}",
        bar_button.0, bar_button.1, bar_button.2
    );
    let mut h = H::new(SCROLL, &["test::scrolled", "test::picked"]).await?;
    let open = h.click(Point::new(50.0, 50.0), mouse::Button::Right);
    assert_eq!(calls(&open), 0, "the right click itself published a call");
    let msgs = h.wheel(on_item);
    let n = h.dispatch(msgs).await?;
    eprintln!(
        "ctx_wheel   : scrolled={} picked={} calls={n}",
        h.get("test::scrolled"),
        h.get("test::picked")
    );
    let mut leaks = vec![];
    if ctx_disabled.0 == t {
        leaks.push("ctx_disabled");
    }
    if ctx_divider.0 == t {
        leaks.push("ctx_divider");
    }
    if bar_checkbox.0 == t {
        leaks.push("bar_checkbox");
    }
    if bar_button.0 == t {
        leaks.push("bar_button");
    }
    if h.get("test::scrolled") == t {
        leaks.push("ctx_wheel");
    }
    assert!(leaks.is_empty(), "input reached the widget under an open menu: {leaks:?}");
    Ok(())
}
