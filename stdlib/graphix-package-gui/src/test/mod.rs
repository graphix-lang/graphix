use anyhow::{Context, Result, bail};
use compact_str::{CompactString, format_compact};
use graphix_compiler::expr::ExprId;
use graphix_package_core::testing::{self, Fixture, find_bind_id};
use graphix_rt::{CallableId, GXEvent, GXHandle, NoExt};
use iced_core::{Event, Point, Size, clipboard, keyboard, mouse, window};
use iced_runtime::user_interface;
use iced_wgpu::{
    graphics::{Shell, Viewport},
    wgpu,
};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use std::{
    cell::{self, RefCell, RefMut},
    time::Duration,
};
use tokio::sync::OnceCell;

use crate::widgets::{self, Child, Message, MessageShell};

mod canvas_test;
mod chart_test;
mod clipboard_test;
mod data_table_test;
mod frame_test;
mod interaction_test;
mod reconcile_test;
mod theme_test;
mod widgets_test;
mod window_test;

const IMPORTS: &str = "\
use gui::*;\n\
use gui::text::{self, *};\n\
use gui::button::{self, *};\n\
use gui::checkbox::{self, *};\n\
use gui::toggler::{self, *};\n\
use gui::text_input::{self, *};\n\
use gui::slider::{self, *};\n\
use gui::radio::{self, *};\n\
use gui::pick_list::{self, *};\n\
use gui::progress_bar::{self, *};\n\
use gui::text_editor::{self, *};\n\
use gui::row::{self, *};\n\
use gui::column::{self, *};\n\
use gui::container::{self, *};\n\
use gui::scrollable::{self, *};\n\
use gui::space::{self, *};\n\
use gui::rule::{self, *};\n\
use gui::stack::{self, *};\n\
use gui::tooltip::{self, *};\n\
use gui::vertical_slider::{self, *};\n\
use gui::combo_box::{self, *};\n\
use gui::mouse_area::{self, *};\n\
use gui::canvas::{self, *};\n\
use gui::chart::{self, *};\n\
use gui::image::{self, *};\n\
use gui::grid::{self, *};\n\
use gui::qr_code::{self, *};\n\
use gui::markdown::{self, *};\n\
use gui::table::{self, *};\n\
use gui::keyboard_area::{self, *};\n\
use gui::menu::{self, *}";

fn widget_code(widget_expr: &str) -> CompactString {
    format_compact!("{IMPORTS};\nlet result = {widget_expr}")
}

async fn gui_harness(widget_expr: &str) -> Result<GuiTestHarness> {
    GuiTestHarness::new(&widget_code(widget_expr)).await
}

async fn interaction_harness(widget_expr: &str) -> Result<InteractionHarness> {
    InteractionHarness::new(&widget_code(widget_expr)).await
}

const TEST_REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &crate::P,
];

/// Deliver an update to the widget tree. A recompile blocks on the
/// runtime, which only a multi-thread flavor permits, and there only
/// inside `block_in_place`.
fn update_widget(
    root: &mut Child<NoExt>,
    gx: &GXHandle<NoExt>,
    rt: &tokio::runtime::Handle,
    id: ExprId,
    v: &Value,
) -> Result<bool> {
    match rt.runtime_flavor() {
        tokio::runtime::RuntimeFlavor::MultiThread => {
            tokio::task::block_in_place(|| root.update(rt, gx, id, v))
        }
        _ => root.update(rt, gx, id, v),
    }
}

/// Test harness for GUI widget integration tests: compiles graphix
/// code producing a Widget value, builds the widget tree, and drives
/// interactions through the reactive loop.
struct GuiTestHarness {
    f: Fixture,
    /// The program's `test::result`, rebuilt when it changes, as a
    /// window's content is.
    root: Child<NoExt>,
    rt_handle: tokio::runtime::Handle,
    /// Built at the first frame.
    frame: cell::OnceCell<RefCell<Frame>>,
}

/// What a window keeps between frames: its renderer, the interface's
/// cache, the pointer as the events moved it, and a clipboard.
struct Frame {
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    cursor: mouse::Cursor,
    clipboard: TestClipboard,
}

impl Frame {
    fn new() -> Self {
        Self {
            renderer: futures::executor::block_on(headless_gpu()).create_renderer(),
            cache: user_interface::Cache::default(),
            cursor: mouse::Cursor::Unavailable,
            clipboard: TestClipboard::default(),
        }
    }
}

impl GuiTestHarness {
    /// Compile module-level graphix code whose last binding, `result`,
    /// is a Widget value.
    async fn new(code: &str) -> Result<Self> {
        let (f, _) = Fixture::new(code, TEST_REGISTER, |_| ()).await?;
        let result =
            f.gx.compile_ref(find_bind_id(&f.compiled.env, "test::result")?).await?;
        let root = Child::compile(&f.gx, result).await.context("compile widget tree")?;
        Ok(Self {
            f,
            root,
            rt_handle: tokio::runtime::Handle::current(),
            frame: cell::OnceCell::new(),
        })
    }

    /// Deliver every event until the runtime is idle with nothing left
    /// to deliver, a `GXEvent::Idle` to the widgets' `idle` as a window
    /// does. Returns true if any changed the widget tree. A value from
    /// outside the runtime (netidx) can come later: wait for it with
    /// `wait_until`.
    async fn drain(&mut self) -> Result<bool> {
        let Self { f: Fixture { gx, rx, watches, .. }, root, rt_handle, .. } = self;
        let mut first_error = None;
        let deliver = async |e| match e {
            GXEvent::Updated(id, v) => {
                watches.note(id, &v);
                match update_widget(root, gx, rt_handle, id, &v) {
                    Ok(changed) => Ok(changed),
                    Err(e) => {
                        log::error!(target: graphix_compiler::FAILURE_TARGET, "{e:#}");
                        first_error.get_or_insert(e);
                        Ok(false)
                    }
                }
            }
            GXEvent::Idle => Ok(root.w.idle()),
            GXEvent::Env(_) => Ok(false),
        };
        let (changed, _) = testing::drain_events(gx, rx, deliver).await?;
        match first_error {
            Some(e) => Err(e),
            None => Ok(changed),
        }
    }

    /// Deliver the runtime's next batch, waiting for it, and nothing
    /// after it: the widgets see one cycle's updates and not the idle
    /// verdicts after it. Returns whether the batch held an update.
    async fn deliver_batch(&mut self) -> Result<bool> {
        let Self { f: Fixture { gx, rx, watches, .. }, root, rt_handle, .. } = self;
        let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
            .await
            .context("no batch came")?
            .context("the runtime is gone")?;
        let mut updated = false;
        for e in batch.drain(..) {
            match e {
                GXEvent::Updated(id, v) => {
                    watches.note(id, &v);
                    update_widget(root, gx, rt_handle, id, &v)?;
                    updated = true;
                }
                GXEvent::Idle => {
                    root.w.idle();
                }
                GXEvent::Env(_) => (),
            }
        }
        Ok(updated)
    }

    /// Set the program variable `name`, module-qualified, from the test.
    fn set_var(&self, name: &str, v: Value) -> Result<()> {
        self.f.gx.set(find_bind_id(&self.f.compiled.env, name)?, v)
    }

    /// Watch a module-qualified variable such as "test::released" and
    /// return its initial value; `get_watched()` reads it after a
    /// `drain()`.
    async fn watch(&mut self, name: &str) -> Result<Value> {
        let v = self.f.watch(name).await?;
        self.drain().await?;
        Ok(v)
    }

    /// Get the most recent value of a watched variable by name.
    fn get_watched(&self, name: &str) -> Option<&Value> {
        self.f.watches.get(name)
    }

    /// Dispatch iced Messages through the runtime and widget as
    /// `GuiHandler::about_to_wait` does, then drain.
    async fn dispatch_calls(&mut self, msgs: &[Message]) -> Result<()> {
        self.apply(msgs);
        self.drain().await?;
        Ok(())
    }

    fn apply(&mut self, msgs: &[Message]) {
        let widget = &mut self.root.w;
        crate::frame::apply_messages(&self.f.gx, msgs.iter().cloned(), |msg, pending| {
            let mut shell = MessageShell::default();
            widget.on_message(msg, &mut shell);
            pending.extend(shell.out.drain(..));
            shell.call_handled
        });
    }

    /// The window state frames draw with.
    fn frame_state(&self) -> RefMut<'_, Frame> {
        self.frame.get_or_init(|| RefCell::new(Frame::new())).borrow_mut()
    }

    /// One frame of the widget tree at `size` over `events`, as a window
    /// draws it on a headless renderer: the messages the widgets published.
    fn frame(&mut self, size: Size, events: &[Event]) -> Vec<Message> {
        let theme =
            crate::theme::GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        let Self { root, frame, .. } = self;
        let mut fr = frame.get_or_init(|| RefCell::new(Frame::new())).borrow_mut();
        let fr = &mut *fr;
        for ev in events {
            crate::window::track_cursor(&mut fr.cursor, ev);
        }
        let mut messages = Vec::new();
        crate::frame::frame(
            &mut root.w,
            size,
            &mut fr.cache,
            &mut fr.renderer,
            events,
            fr.cursor,
            &mut fr.clipboard,
            &mut messages,
            &theme,
        );
        messages
    }

    /// Build, lay out and draw the widget tree once at 800 x 600.
    // CR claude for claude: [readability/low] [readability/low] render always returns
    // Ok(()). frame() has no error to report, so every `h.render()?` reads as a check
    // that cannot fail. Return () (or the frame's messages) and drop the `?` at the
    // call sites. Found by reading; not run. (tests-gui-a-06)
    fn render(&mut self) -> Result<()> {
        let _ = self.frame(Size::new(800.0, 600.0), &[]);
        Ok(())
    }

    /// A frame at `width` x `height` over a black background, as RGBA.
    fn screenshot(&mut self, width: u32, height: u32) -> Vec<u8> {
        let _ = self.frame(Size::new(width as f32, height as f32), &[]);
        let viewport = Viewport::with_physical_size(Size::new(width, height), 1.0);
        self.frame_state().renderer.screenshot(&viewport, iced_core::Color::BLACK)
    }

    /// The widget's element, built and dropped.
    fn view(&self) -> crate::widgets::IcedElement<'_> {
        self.root.w.view()
    }

    /// Flush deferred per-widget state as the event loop does before a
    /// render. Call before `dt_snapshot()` after publishing to a sort
    /// column.
    fn before_view(&mut self) -> bool {
        self.root.w.before_view()
    }

    /// Drain and `before_view` in a loop until `pred(self)` holds;
    /// returns an error when `within` elapses.
    async fn wait_until<F>(
        &mut self,
        mut pred: F,
        within: Duration,
        why: &str,
    ) -> Result<()>
    where
        F: FnMut(&mut Self) -> bool,
    {
        let deadline = tokio::time::Instant::now() + within;
        let mut iters = 0;
        loop {
            self.drain().await?;
            self.before_view();
            iters += 1;
            if pred(self) {
                return Ok(());
            }
            if tokio::time::Instant::now() >= deadline {
                bail!(
                    "wait_until timed out after {iters} iterations / {:?}: {why}",
                    within,
                );
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    }

    /// The widget's `DataTableSnapshot`, if it is a data table.
    fn dt_snapshot(&self) -> crate::widgets::DataTableSnapshot {
        self.root.w.data_table_snapshot().expect("widget is not a DataTableW")
    }

    /// Downcast the root widget to `DataTableW<NoExt>`; panics if it is
    /// not a data table.
    fn dt(&self) -> &crate::widgets::data_table::DataTableW<NoExt> {
        self.root
            .w
            .as_any()
            .downcast_ref::<crate::widgets::data_table::DataTableW<NoExt>>()
            .expect("widget is not a DataTableW")
    }

    /// Mutable downcast to `DataTableW<NoExt>`.
    fn dt_mut(&mut self) -> &mut crate::widgets::data_table::DataTableW<NoExt> {
        self.root
            .w
            .as_any_mut()
            .downcast_mut::<crate::widgets::data_table::DataTableW<NoExt>>()
            .expect("widget is not a DataTableW")
    }

    /// Call a callable through the runtime and drain, as the widget
    /// itself would.
    async fn call_callback(&mut self, id: CallableId, args: ValArray) -> Result<()> {
        self.f.gx.call(id, args)?;
        self.drain().await?;
        Ok(())
    }

    /// Compile a graphix lambda by module-qualified name into a
    /// `CallableId`. The `Callable` is retained on the harness because
    /// dropping it invalidates the id.
    async fn compile_named_callable(&mut self, name: &str) -> Result<CallableId> {
        self.f.compile_named_callable(name).await
    }
}

/// A key going down, as the event loop delivers it.
fn key_press(
    key: keyboard::Key,
    modifiers: keyboard::Modifiers,
    text: Option<iced_core::SmolStr>,
) -> Event {
    Event::Keyboard(keyboard::Event::KeyPressed {
        modified_key: key.clone(),
        key,
        physical_key: keyboard::key::Physical::Unidentified(
            keyboard::key::NativeCode::Unidentified,
        ),
        location: keyboard::Location::Standard,
        modifiers,
        text,
        repeat: false,
    })
}

fn named_key(named: keyboard::key::Named) -> Event {
    key_press(keyboard::Key::Named(named), keyboard::Modifiers::empty(), None)
}

/// A key that types `c`.
fn char_key(c: &str) -> Event {
    let s: iced_core::SmolStr = c.into();
    key_press(keyboard::Key::Character(s.clone()), keyboard::Modifiers::empty(), Some(s))
}

/// A left click at `at`: the move there, the press and the release.
fn left_click(at: Point) -> [Event; 3] {
    [
        Event::Mouse(mouse::Event::CursorMoved { position: at }),
        Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)),
    ]
}

/// Shared headless wgpu adapter and device, created once for all tests.
struct HeadlessGpu {
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
    format: wgpu::TextureFormat,
}

static HEADLESS_GPU: OnceCell<HeadlessGpu> = OnceCell::const_new();

async fn headless_gpu() -> &'static HeadlessGpu {
    HEADLESS_GPU
        .get_or_init(|| async {
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
            HeadlessGpu {
                adapter,
                device,
                queue,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
            }
        })
        .await
}

impl HeadlessGpu {
    fn create_renderer(&self) -> widgets::Renderer {
        let engine = iced_wgpu::Engine::new(
            &self.adapter,
            self.device.clone(),
            self.queue.clone(),
            self.format,
            None,
            Shell::headless(),
        );
        iced_wgpu::Renderer::new(
            engine,
            iced_core::Font::DEFAULT,
            iced_core::Pixels(16.0),
        )
    }
}

/// `GuiTestHarness` plus a headless renderer and iced `UserInterface`,
/// for simulating clicks, typing and drags and collecting the
/// resulting `Message`s.
// CR claude for claude: [structure/low] [structure/low] InteractionHarness is
// GuiTestHarness plus one Size. drain/watch/get_watched/dispatch_calls/apply only
// forward to inner, and the tests use both spellings (h.watch vs h.inner.watch, h.drain
// vs h.inner.drain), sometimes in the same test. Put a viewport field on
// GuiTestHarness, move the event helpers onto it, and delete this wrapper and its
// forwarders. Found by reading; not run. (tests-gui-a-05)
struct InteractionHarness {
    inner: GuiTestHarness,
    viewport: Size,
}

/// A clipboard holding one text.
#[derive(Default)]
struct TestClipboard(Option<String>);

impl clipboard::Clipboard for TestClipboard {
    fn read(&self, _kind: clipboard::Kind) -> Option<String> {
        self.0.clone()
    }

    fn write(&mut self, _kind: clipboard::Kind, contents: String) {
        self.0 = Some(contents)
    }
}

impl InteractionHarness {
    async fn new(code: &str) -> Result<Self> {
        Self::with_viewport(code, Size::new(300.0, 50.0)).await
    }

    async fn with_viewport(code: &str, viewport: Size) -> Result<Self> {
        Ok(Self { inner: GuiTestHarness::new(code).await?, viewport })
    }

    /// One frame over `events`: the messages the widgets produced.
    fn process_events(&mut self, events: &[Event]) -> Vec<Message> {
        self.inner.frame(self.viewport, events)
    }

    async fn drain(&mut self) -> Result<bool> {
        self.inner.drain().await
    }

    /// Simulate a window resize; runs one layout pass so
    /// responsive-wrapped widgets see the new size immediately.
    fn resize(&mut self, viewport: Size) {
        self.viewport = viewport;
        let _ = self.process_events(&[Event::Window(window::Event::Resized(viewport))]);
    }

    fn view(&mut self) -> crate::widgets::IcedElement<'_> {
        // A `responsive`-wrapped widget fills its size-dependent state
        // during layout, not in `view()`, so lay out once first.
        let _ = self.process_events(&[]);
        self.inner.view()
    }

    async fn watch(&mut self, name: &str) -> Result<Value> {
        self.inner.watch(name).await
    }

    fn get_watched(&self, name: &str) -> Option<&Value> {
        self.inner.get_watched(name)
    }

    async fn dispatch_calls(&mut self, msgs: &[Message]) -> Result<()> {
        self.inner.dispatch_calls(msgs).await
    }

    fn click(&mut self, pos: Point) -> Vec<Message> {
        // One UI frame per event so pressed → released transitions.
        left_click(pos)
            .iter()
            .flat_map(|ev| self.process_events(std::slice::from_ref(ev)))
            .collect()
    }

    /// Deliver `events` one frame each, applying each frame's messages
    /// before the next as the event loop does.
    async fn live(&mut self, events: &[Event]) -> Result<()> {
        for ev in events {
            let msgs = self.process_events(std::slice::from_ref(ev));
            self.dispatch_calls(&msgs).await?;
        }
        Ok(())
    }

    /// Give the widgets `msgs` as the event loop does after a frame, the
    /// calls sent to the runtime without letting it run: the next frame
    /// comes before their echoes, as fast input does.
    fn apply(&mut self, msgs: &[Message]) {
        self.inner.apply(msgs);
    }

    /// A left-button drag from `from` to `to`, live.
    async fn drag_live(&mut self, from: Point, to: Point, steps: u32) -> Result<()> {
        let mut events = vec![
            Event::Mouse(mouse::Event::CursorMoved { position: from }),
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        ];
        for i in 1..=steps {
            let f = i as f32 / steps as f32;
            let position =
                Point::new(from.x + (to.x - from.x) * f, from.y + (to.y - from.y) * f);
            events.push(Event::Mouse(mouse::Event::CursorMoved { position }));
        }
        events.push(Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)));
        self.live(&events).await
    }

    fn type_text(&mut self, text: &str) -> Vec<Message> {
        let mut all_msgs = Vec::new();
        let mut buf = [0; 4];
        for ch in text.chars() {
            all_msgs.extend(self.process_events(&[char_key(ch.encode_utf8(&mut buf))]));
        }
        all_msgs
    }

    fn press_key(&mut self, named: keyboard::key::Named) -> Vec<Message> {
        self.process_events(&[named_key(named)])
    }

    /// Ctrl and a letter, the modifiers delivered first as the event loop
    /// does.
    fn press_ctrl(&mut self, c: &str) -> Vec<Message> {
        let ctrl = keyboard::Modifiers::CTRL;
        self.process_events(&[
            Event::Keyboard(keyboard::Event::ModifiersChanged(ctrl)),
            key_press(keyboard::Key::Character(c.into()), ctrl, None),
        ])
    }

    fn release_key(&mut self, named: iced_core::keyboard::key::Named) -> Vec<Message> {
        use iced_core::keyboard;
        self.process_events(&[Event::Keyboard(keyboard::Event::KeyReleased {
            key: keyboard::Key::Named(named),
            modified_key: keyboard::Key::Named(named),
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers: keyboard::Modifiers::empty(),
        })])
    }

    fn scroll(&mut self, delta_x: f32, delta_y: f32) -> Vec<Message> {
        self.process_events(&[Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Lines { x: delta_x, y: delta_y },
        })])
    }

    fn move_cursor(&mut self, pos: Point) -> Vec<Message> {
        self.process_events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })])
    }

    /// Drive `Message::EditorAction`s through `on_message` and collect
    /// the `Call` messages the editor publishes, one per edit.
    fn process_editor_actions(&mut self, msgs: &[Message]) -> Vec<(CallableId, Value)> {
        let mut out = Vec::new();
        for m in msgs {
            if let Message::EditorAction(_, _) = m {
                let mut shell = MessageShell::default();
                self.inner.root.w.on_message(m, &mut shell);
                for emitted in shell.out.drain(..) {
                    if let Message::Call(cid, args) = emitted {
                        if let Some(v) = args.into_iter().next() {
                            out.push((cid, v));
                        }
                    }
                }
            }
        }
        out
    }

    // CR claude for claude: [structure/low] [structure/low] This builds the same move,
    // press, interpolated moves, release sequence as drag_live, differing only in
    // whether each frame's messages are dispatched or collected. Build the events once
    // (fn drag_events(from, to, steps) -> Vec<Event>) and have both helpers use it.
    // Found by reading; not run. (tests-gui-a-07)
    fn drag_horizontal(&mut self, from: Point, to_x: f32, steps: u32) -> Vec<Message> {
        let mut all_msgs = Vec::new();
        all_msgs.extend(self.process_events(&[Event::Mouse(
            mouse::Event::CursorMoved { position: from },
        )]));
        all_msgs.extend(self.process_events(&[Event::Mouse(
            mouse::Event::ButtonPressed(mouse::Button::Left),
        )]));
        let dx = (to_x - from.x) / steps as f32;
        for i in 1..=steps {
            let pos = Point::new(from.x + dx * i as f32, from.y);
            all_msgs.extend(self.process_events(&[Event::Mouse(
                mouse::Event::CursorMoved { position: pos },
            )]));
        }
        all_msgs.extend(self.process_events(&[Event::Mouse(
            mouse::Event::ButtonReleased(mouse::Button::Left),
        )]));
        all_msgs
    }
}

fn expect_call(msgs: &[Message]) -> CallableId {
    let calls: Vec<_> = msgs
        .iter()
        .filter_map(|m| match m {
            Message::Call(id, _) => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(calls.len(), 1, "expected exactly one Call message, got {}", calls.len());
    calls[0]
}

/// The f64 first argument of the one Call in `msgs`.
fn f64_arg(msgs: &[Message]) -> f64 {
    expect_call(msgs);
    let arg = msgs.iter().find_map(|m| match m {
        Message::Call(_, args) => args.first(),
        _ => None,
    });
    match arg {
        Some(Value::F64(v)) => *v,
        v => panic!("not an f64: {v:?}"),
    }
}

fn expect_call_with_args(
    msgs: &[Message],
    pred: impl Fn(&ValArray) -> bool,
) -> CallableId {
    let calls: Vec<_> = msgs
        .iter()
        .filter_map(|m| match m {
            Message::Call(id, args) if pred(args) => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(calls.len(), 1, "expected exactly one Call matching the predicate");
    calls[0]
}
