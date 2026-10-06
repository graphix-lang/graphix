//! Review finding gui-widgets-a-05: context menu opens offset by the
//! scroll amount inside a scrollable, and is never kept inside the window.
//!
//! Drives iced's real `UserInterface` and `Scrollable` headlessly over a
//! Graphix widget tree (the same build/update calls as event_loop.rs),
//! window 300x200. Test 1: 100 rows, each wrapped in `menu::context_menu`
//! with three items (28px each). Test 2: row 50 of 101 is iced's own
//! `pick_list` (control) or a graphix `menu::bar`, scrolled 1000px.
//!
//! To run, copy this file to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_05.rs, then:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_05 -- --nocapture
//!
//! expected: the menu opens at the cursor (book/src/ui/gui/menu.md:
//!   "opens the menu at the cursor position") and stays inside the window;
//!   a menu bar's dropdown opens under its label, as iced's pick_list does.
//! observed (HEAD c722befe), both tests FAIL:
//!   unscrolled: click 10px under the cursor hits the menu: true
//!   bottom edge: a click at y=260 (window is 200px) hits the menu: true
//!   scrolled: on_scroll reported offset y = Some(1000.0)
//!   scrolled: click 10px under the cursor hits the menu: false
//!   scrolled: click at cursor y + 1000 (y = 1110, window is 200px) hits the menu: true
//!   iced pick_list, scrolled 1000: option under the widget (y=80) selected: true
//!   graphix menu::bar, scrolled 1000: item under the label (y=80) clicked: false
//!   graphix menu::bar, scrolled 1000: item at y = 1080 clicked: true
use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::{ExprId, VfsEntry, VfsResolver};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message};
use graphix_rt::{CompRes, GXEvent, NoExt};
use iced_core::{Event, Point, Size, clipboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const CODE: &str = r#"
use gui::{column::column, menu, scrollable::scrollable, text::text};
let result = scrollable(
    #width: &`Fill,
    #height: &`Fill,
    #on_scroll: |pos| null,
    &column(&array::init(100, |i| menu::context_menu(
        &[
            menu::action(#on_click: |_| null, &"Copy"),
            menu::action(#on_click: |_| null, &"Paste"),
            menu::action(#on_click: |_| null, &"Delete [i]")
        ],
        &text(&"row [i]")
    )))
)
"#;

struct H {
    _ctx: TestCtx,
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt: tokio::runtime::Handle,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    viewport: Size,
    cursor: Point,
}

async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(10));
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
            _ = &mut timeout => bail!("timeout waiting for the widget value"),
        }
    }
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

impl H {
    async fn new(code: &str, viewport: Size) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)])
                .await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let initial = wait_for_update(&mut rx, compiled.exprs[0].id).await?;
        let widget = widgets::compile(gx.clone(), initial).await.context("widgets")?;
        let mut h = Self {
            _ctx: ctx,
            _compiled: compiled,
            rx,
            widget,
            rt: tokio::runtime::Handle::current(),
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            viewport,
            cursor: Point::ORIGIN,
        };
        h.drain().await?;
        Ok(h)
    }

    async fn drain(&mut self) -> Result<()> {
        let timeout = tokio::time::sleep(Duration::from_millis(200));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            let rt = self.rt.clone();
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(100)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    fn process(&mut self, events: &[Event]) -> Vec<Message> {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(element, self.viewport, cache, &mut self.renderer);
        let mut messages = Vec::new();
        let mut clipboard = clipboard::Null;
        let cursor = mouse::Cursor::Available(self.cursor);
        let _ =
            ui.update(events, cursor, &mut self.renderer, &mut clipboard, &mut messages);
        self.cache = ui.into_cache();
        messages
    }

    fn press(&mut self, pos: Point, button: mouse::Button) -> Vec<Message> {
        self.cursor = pos;
        let mut all = Vec::new();
        all.extend(
            self.process(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]),
        );
        all.extend(self.process(&[Event::Mouse(mouse::Event::ButtonPressed(button))]));
        all.extend(self.process(&[Event::Mouse(mouse::Event::ButtonReleased(button))]));
        all
    }

    /// Right-click at `at`, then left-click at `probe`; true when the
    /// left click landed on a menu item (a `Call` with a null argument).
    fn menu_hit(&mut self, at: Point, probe: Point) -> bool {
        let _ = self.press(at, mouse::Button::Right);
        let msgs = self.press(probe, mouse::Button::Left);
        msgs.iter().any(|m| {
            matches!(m, Message::Call(_, args) if args.len() == 1 && args[0] == Value::Null)
        })
    }

    /// Left-click at `open_at` to open a dropdown, then left-click at
    /// `probe`; the messages of the probe click.
    fn open_and_probe(&mut self, open_at: Point, probe: Point) -> Vec<Message> {
        let _ = self.press(open_at, mouse::Button::Left);
        self.press(probe, mouse::Button::Left)
    }

    fn scroll_down(&mut self, at: Point, pixels: f32) -> Option<f64> {
        self.cursor = at;
        let _ = self.process(&[Event::Mouse(mouse::Event::CursorMoved { position: at })]);
        let msgs = self.process(&[Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Pixels { x: 0.0, y: -pixels },
        })]);
        msgs.iter().find_map(|m| match m {
            Message::Call(_, args) if args.len() == 1 => offset_y(&args[0]),
            _ => None,
        })
    }
}

fn offset_y(v: &Value) -> Option<f64> {
    let Value::Array(fields) = v else { return None };
    fields.iter().find_map(|f| match f {
        Value::Array(kv) if kv.len() == 2 && kv[0] == Value::String("y".into()) => {
            match kv[1] {
                Value::F64(y) => Some(y),
                _ => None,
            }
        }
        _ => None,
    })
}

#[tokio::test(flavor = "multi_thread")]
async fn context_menu_in_scrollable_opens_at_cursor() -> Result<()> {
    let mut h = H::new(CODE, Size::new(300.0, 200.0)).await?;
    let mut failures = Vec::new();

    // Control: unscrolled, the first item is right under the cursor.
    let control = h.menu_hit(Point::new(10.0, 100.0), Point::new(15.0, 110.0));
    println!("unscrolled: click 10px under the cursor hits the menu: {control}");
    assert!(control, "control failed: the probe does not reach the menu at all");

    // Near the bottom edge of a 200px window: the third item (y 246..274
    // if unclamped) must not lie below the window.
    let off_window = h.menu_hit(Point::new(10.0, 190.0), Point::new(15.0, 260.0));
    println!("bottom edge: a click at y=260 (window is 200px) hits the menu: {off_window}");
    if off_window {
        failures.push("a menu opened at y=190 has an item at y=260, below the 200px window");
    }

    let offset = h.scroll_down(Point::new(10.0, 100.0), 1000.0);
    println!("scrolled: on_scroll reported offset y = {offset:?}");
    let dy = offset.expect("no on_scroll offset") as f32;
    assert!(dy > 500.0, "did not scroll");

    let at_cursor = h.menu_hit(Point::new(10.0, 100.0), Point::new(15.0, 110.0));
    println!("scrolled: click 10px under the cursor hits the menu: {at_cursor}");
    if !at_cursor {
        failures.push("after scrolling, the menu is not at the cursor");
    }
    let shifted = h.menu_hit(Point::new(10.0, 100.0), Point::new(15.0, 110.0 + dy));
    println!(
        "scrolled: click at cursor y + {dy} (y = {}, window is 200px) hits the menu: {shifted}",
        110.0 + dy
    );
    if shifted {
        failures.push("after scrolling, the menu sits at the cursor plus the scroll offset");
    }
    assert!(failures.is_empty(), "{failures:#?}");
    Ok(())
}

fn row50(widget: &str) -> String {
    format!(
        r#"
use gui::{{column::column, menu, pick_list::pick_list, scrollable::scrollable, text::text}};
let result = scrollable(
    #width: &`Fill,
    #height: &`Fill,
    #on_scroll: |pos| null,
    &column(&array::init(101, |i| select i {{
        50 => {widget},
        _ => text(&"row [i]")
    }}))
)
"#
    )
}

fn has_call(msgs: &[Message], pred: impl Fn(&Value) -> bool) -> bool {
    msgs.iter()
        .any(|m| matches!(m, Message::Call(_, args) if args.len() == 1 && pred(&args[0])))
}

/// Row 50 sits at content y 1040; after a 1000px scroll it is at window
/// y 40, and its dropdown belongs just below it, around window y 80.
#[tokio::test(flavor = "multi_thread")]
async fn dropdowns_in_scrollable_iced_vs_graphix() -> Result<()> {
    let mut failures = Vec::new();

    let mut h = H::new(
        &row50(r#"pick_list(#on_select: |s| null, #placeholder: &"Choose", &["Red", "Green", "Blue"])"#),
        Size::new(300.0, 200.0),
    )
    .await?;
    let dy = h.scroll_down(Point::new(10.0, 100.0), 1000.0).expect("no scroll") as f32;
    let msgs = h.open_and_probe(Point::new(10.0, 50.0), Point::new(15.0, 80.0));
    let iced_hit = has_call(&msgs, |v| matches!(v, Value::String(_)));
    println!("iced pick_list, scrolled {dy}: option under the widget (y=80) selected: {iced_hit}");
    assert!(iced_hit, "control failed: iced's own pick_list dropdown is not under the widget");

    let mut h = H::new(
        &row50(r#"menu::bar(&[menu::menu(&"File", &[menu::action(#on_click: |_| null, &"Open")])])"#),
        Size::new(300.0, 200.0),
    )
    .await?;
    let dy = h.scroll_down(Point::new(10.0, 100.0), 1000.0).expect("no scroll") as f32;
    let msgs = h.open_and_probe(Point::new(10.0, 50.0), Point::new(15.0, 80.0));
    let bar_hit = has_call(&msgs, |v| *v == Value::Null);
    println!("graphix menu::bar, scrolled {dy}: item under the label (y=80) clicked: {bar_hit}");
    if !bar_hit {
        failures.push("menu bar dropdown is not under its label after scrolling");
    }
    let msgs = h.open_and_probe(Point::new(10.0, 50.0), Point::new(15.0, 80.0 + dy));
    let bar_shifted = has_call(&msgs, |v| *v == Value::Null);
    println!(
        "graphix menu::bar, scrolled {dy}: item at y = {} clicked: {bar_shifted}",
        80.0 + dy
    );
    if bar_shifted {
        failures.push("menu bar dropdown sits at its label plus the scroll offset");
    }
    assert!(failures.is_empty(), "{failures:#?}");
    Ok(())
}
