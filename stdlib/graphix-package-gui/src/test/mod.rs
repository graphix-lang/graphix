use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::ExprId;
use graphix_compiler::expr::VfsResolver;
use graphix_package_core::testing::{self, TestCtx};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

use crate::widgets::{self, GuiW, Message, MessageShell};

mod canvas_test;
mod chart_test;
mod clipboard_test;
mod data_table_test;
mod frame_test;
mod interaction_test;
mod theme_test;
mod widgets_test;

const TEST_REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &crate::P,
];

/// Deliver an update to the widget tree. A children recompile blocks on
/// the runtime, which only a multi-thread flavor permits, and there only
/// inside `block_in_place`.
fn update_widget(
    widget: &mut GuiW<NoExt>,
    rt: &tokio::runtime::Handle,
    id: ExprId,
    v: &Value,
) -> Result<bool> {
    match rt.runtime_flavor() {
        tokio::runtime::RuntimeFlavor::MultiThread => {
            tokio::task::block_in_place(|| widget.handle_update(rt, id, v))
        }
        _ => widget.handle_update(rt, id, v),
    }
}

/// Test harness for GUI widget integration tests: compiles graphix
/// code producing a Widget value, builds the widget tree, and drives
/// interactions through the reactive loop.
struct GuiTestHarness {
    _ctx: TestCtx,
    gx: graphix_rt::GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt_handle: tokio::runtime::Handle,
    watches: testing::Watches,
    _refs: Vec<Ref<NoExt>>,
    _callables: Vec<Callable<NoExt>>,
}

impl GuiTestHarness {
    /// Compile module-level graphix code whose last binding, `result`,
    /// is a Widget value.
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let resolver = VfsResolver::new(tbl);
        let ctx = testing::init_with_resolvers(tx, TEST_REGISTER, vec![resolver]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let expr_id = compiled.exprs[0].id;

        let initial_value = testing::next_update(
            &mut rx,
            expr_id,
            tokio::time::Instant::now() + Duration::from_secs(5),
        )
        .await?;

        let widget = widgets::compile(gx.clone(), initial_value)
            .await
            .context("compile widget tree")?;

        let rt_handle = tokio::runtime::Handle::current();

        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            widget,
            rt_handle,
            watches: testing::Watches::default(),
            _refs: Vec::new(),
            _callables: Vec::new(),
        })
    }

    /// Deliver every update until the runtime is idle with nothing left
    /// to deliver. Returns true if any update changed the widget tree.
    /// A value from outside the runtime (netidx) can come later: wait for
    /// it with `wait_until`.
    async fn drain(&mut self) -> Result<bool> {
        let Self { gx, rx, widget, rt_handle, watches, .. } = self;
        let deliver = async |id, v: Value| {
            watches.note(id, &v);
            update_widget(widget, rt_handle, id, &v)
        };
        Ok(testing::drain_idle(gx, rx, deliver).await?.0)
    }

    /// Watch a module-qualified variable such as "test::released" and
    /// return its initial value; `get_watched()` reads it after a
    /// `drain()`.
    async fn watch(&mut self, name: &str) -> Result<Value> {
        let v = self.watches.watch(&self.gx, &self.compiled.env, name).await?;
        self.drain().await?;
        Ok(v)
    }

    /// Get the most recent value of a watched variable by name.
    fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watches.get(name)
    }

    /// Dispatch iced Messages through the runtime and widget as
    /// `GuiHandler::about_to_wait` does, then drain.
    async fn dispatch_calls(&mut self, msgs: &[Message]) -> Result<()> {
        // the event loop's own drain
        let widget = &mut self.widget;
        crate::frame::apply_messages(&self.gx, msgs.iter().cloned(), |msg, pending| {
            let mut shell = MessageShell::default();
            widget.on_message(msg, &mut shell);
            pending.extend(shell.out.drain(..));
        });
        self.drain().await?;
        Ok(())
    }

    /// Build, lay out and draw the widget tree once, as a window frame
    /// does, on a headless renderer.
    async fn render(&self) -> Result<()> {
        let mut renderer = headless_gpu().await.create_renderer();
        let theme =
            crate::theme::GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        crate::frame::frame(
            self.widget.view(),
            Size::new(800.0, 600.0),
            &mut user_interface::Cache::default(),
            &mut renderer,
            &mut Vec::new(),
            &[],
            mouse::Cursor::Unavailable,
            &mut TestClipboard::default(),
            &mut Vec::new(),
            &theme,
        );
        Ok(())
    }

    /// The widget's element, built and dropped.
    fn view(&self) -> crate::widgets::IcedElement<'_> {
        self.widget.view()
    }

    /// Flush deferred per-widget state as the event loop does before a
    /// render. Call before `dt_snapshot()` after publishing to a sort
    /// column.
    fn before_view(&mut self) -> bool {
        self.widget.before_view()
    }

    /// Drain and `before_view` in a loop until `pred(self)` holds;
    /// panics when `within` elapses.
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
        self.widget.data_table_snapshot().expect("widget is not a DataTableW")
    }

    /// Downcast the root widget to `DataTableW<NoExt>`; panics if it is
    /// not a data table.
    fn dt(&self) -> &crate::widgets::data_table::DataTableW<NoExt> {
        self.widget
            .as_any()
            .downcast_ref::<crate::widgets::data_table::DataTableW<NoExt>>()
            .expect("widget is not a DataTableW")
    }

    /// Mutable downcast to `DataTableW<NoExt>`.
    fn dt_mut(&mut self) -> &mut crate::widgets::data_table::DataTableW<NoExt> {
        self.widget
            .as_any_mut()
            .downcast_mut::<crate::widgets::data_table::DataTableW<NoExt>>()
            .expect("widget is not a DataTableW")
    }

    /// Call a callable through the runtime and drain, as the widget
    /// itself would.
    async fn call_callback(
        &mut self,
        id: graphix_rt::CallableId,
        args: ValArray,
    ) -> Result<()> {
        self.gx.call(id, args)?;
        self.drain().await?;
        Ok(())
    }

    /// Compile a graphix lambda by module-qualified name into a
    /// `CallableId`. The `Callable` is retained on the harness because
    /// dropping it invalidates the id.
    async fn compile_named_callable(
        &mut self,
        name: &str,
    ) -> Result<graphix_rt::CallableId> {
        let (r, cb) = testing::compile_named_callable(&self.gx, &self.compiled.env, name)
            .await
            .with_context(|| format!("compile_named_callable {name}"))?;
        let id = cb.id();
        self._refs.push(r);
        self._callables.push(cb);
        Ok(id)
    }
}

use iced_core::{Event, Point, Size, clipboard, mouse};
use iced_runtime::user_interface;
use iced_wgpu::{graphics::Shell, wgpu};
use tokio::sync::OnceCell;

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
struct InteractionHarness {
    inner: GuiTestHarness,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    viewport: Size,
    cursor_position: Point,
    /// The pointer as the event loop tracks it from the events sent.
    cursor: mouse::Cursor,
    clipboard: TestClipboard,
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
        let gpu = headless_gpu().await;
        let renderer = gpu.create_renderer();
        let inner = GuiTestHarness::new(code).await?;
        Ok(Self {
            inner,
            renderer,
            cache: user_interface::Cache::default(),
            viewport,
            cursor_position: Point::ORIGIN,
            cursor: mouse::Cursor::Unavailable,
            clipboard: TestClipboard::default(),
        })
    }

    /// Build a UserInterface, feed events, and return the messages the
    /// widgets produced.
    fn process_events(&mut self, events: &[Event]) -> Vec<Message> {
        // the event loop's own frame, the draw included
        let mut messages = Vec::new();
        let theme =
            crate::theme::GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
        for ev in events {
            crate::window::track_cursor(&mut self.cursor, ev);
        }
        let mut ops = Vec::new();
        self.inner.widget.take_ops(&mut ops);
        crate::frame::frame(
            self.inner.widget.view(),
            self.viewport,
            &mut self.cache,
            &mut self.renderer,
            &mut ops,
            events,
            self.cursor,
            &mut self.clipboard,
            &mut messages,
            &theme,
        );
        messages
    }

    async fn drain(&mut self) -> Result<bool> {
        self.inner.drain().await
    }

    /// Simulate a window resize; runs one layout pass so
    /// responsive-wrapped widgets see the new size immediately.
    fn resize(&mut self, viewport: Size) {
        self.viewport = viewport;
        self.cache = user_interface::Cache::default();
        let _ = self.process_events(&[]);
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
        self.cursor_position = pos;
        let mut all = Vec::new();
        // One UI frame per event so pressed → released transitions.
        all.extend(self.process_events(&[Event::Mouse(mouse::Event::CursorMoved {
            position: pos,
        })]));
        all.extend(self.process_events(&[Event::Mouse(mouse::Event::ButtonPressed(
            mouse::Button::Left,
        ))]));
        all.extend(self.process_events(&[Event::Mouse(mouse::Event::ButtonReleased(
            mouse::Button::Left,
        ))]));
        all
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
        let widget = &mut self.inner.widget;
        crate::frame::apply_messages(
            &self.inner.gx,
            msgs.iter().cloned(),
            |msg, pending| {
                let mut shell = MessageShell::default();
                widget.on_message(msg, &mut shell);
                pending.extend(shell.out.drain(..));
            },
        );
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
        self.cursor_position = to;
        self.live(&events).await
    }

    fn type_text(&mut self, text: &str) -> Vec<Message> {
        use iced_core::keyboard;
        let mut all_msgs = Vec::new();
        for ch in text.chars() {
            let s: iced_core::SmolStr = ch.to_string().into();
            all_msgs.extend(self.process_events(&[Event::Keyboard(
                keyboard::Event::KeyPressed {
                    key: keyboard::Key::Character(s.clone()),
                    modified_key: keyboard::Key::Character(s.clone()),
                    physical_key: keyboard::key::Physical::Unidentified(
                        keyboard::key::NativeCode::Unidentified,
                    ),
                    location: keyboard::Location::Standard,
                    modifiers: keyboard::Modifiers::empty(),
                    text: Some(s),
                    repeat: false,
                },
            )]));
        }
        all_msgs
    }

    fn press_key(&mut self, named: iced_core::keyboard::key::Named) -> Vec<Message> {
        use iced_core::keyboard;
        self.process_events(&[Event::Keyboard(keyboard::Event::KeyPressed {
            key: keyboard::Key::Named(named),
            modified_key: keyboard::Key::Named(named),
            physical_key: keyboard::key::Physical::Unidentified(
                keyboard::key::NativeCode::Unidentified,
            ),
            location: keyboard::Location::Standard,
            modifiers: keyboard::Modifiers::empty(),
            text: None,
            repeat: false,
        })])
    }

    /// Ctrl and a letter, the modifiers delivered first as the event loop
    /// does.
    fn press_ctrl(&mut self, c: &str) -> Vec<Message> {
        use iced_core::keyboard;
        let ctrl = keyboard::Modifiers::CTRL;
        let s: iced_core::SmolStr = c.into();
        self.process_events(&[
            Event::Keyboard(keyboard::Event::ModifiersChanged(ctrl)),
            Event::Keyboard(keyboard::Event::KeyPressed {
                key: keyboard::Key::Character(s.clone()),
                modified_key: keyboard::Key::Character(s),
                physical_key: keyboard::key::Physical::Unidentified(
                    keyboard::key::NativeCode::Unidentified,
                ),
                location: keyboard::Location::Standard,
                modifiers: ctrl,
                text: None,
                repeat: false,
            }),
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
        self.cursor_position = pos;
        self.process_events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })])
    }

    /// Drive `Message::EditorAction`s through `on_message` and collect
    /// the `Call` messages the editor publishes, one per edit.
    fn process_editor_actions(&mut self, msgs: &[Message]) -> Vec<(CallableId, Value)> {
        let mut out = Vec::new();
        for m in msgs {
            if let Message::EditorAction(_, _) = m {
                let mut shell = MessageShell::default();
                self.inner.widget.on_message(m, &mut shell);
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

    fn drag_horizontal(&mut self, from: Point, to_x: f32, steps: u32) -> Vec<Message> {
        let mut all_msgs = Vec::new();
        self.cursor_position = from;
        all_msgs.extend(self.process_events(&[Event::Mouse(
            mouse::Event::CursorMoved { position: from },
        )]));
        all_msgs.extend(self.process_events(&[Event::Mouse(
            mouse::Event::ButtonPressed(mouse::Button::Left),
        )]));
        let dx = (to_x - from.x) / steps as f32;
        for i in 1..=steps {
            let pos = Point::new(from.x + dx * i as f32, from.y);
            self.cursor_position = pos;
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

use graphix_rt::CallableId;

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
