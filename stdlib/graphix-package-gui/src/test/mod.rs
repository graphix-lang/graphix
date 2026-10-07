use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::expr::ExprId;
use graphix_compiler::expr::VfsResolver;
use graphix_package_core::testing::{self, TestCtx};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use nohash::IntMap;
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

/// Test harness for GUI widget integration tests: compiles graphix
/// code producing a Widget value, builds the widget tree, and drives
/// interactions through the reactive loop.
struct GuiTestHarness {
    _ctx: TestCtx,
    gx: graphix_rt::GXHandle<NoExt>,
    #[allow(dead_code)]
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt_handle: tokio::runtime::Handle,
    watched: IntMap<ExprId, Value>,
    watch_names: AHashMap<String, ExprId>,
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
            watched: IntMap::default(),
            watch_names: AHashMap::default(),
            _refs: Vec::new(),
            _callables: Vec::new(),
        })
    }

    /// Drain all pending reactive updates into the widget tree.
    /// Returns true if any updates were processed.
    // CR claude for claude: [structure] drain waits for a quiet window (100 ms with no
    // batch, 50 ms after the last), where the TUI harness drains to
    // GXHandle::wait_idle: each drain here sleeps 50-100 ms against wait_idle's 3-6 ms,
    // and viewport_metrics_update_on_resize spends 2.1 s in its 20 drains. Under a
    // multi_thread runtime (stack_children_follow_rotation) a reply that comes more
    // than 100 ms after the call is lost; the current_thread tests cannot lose one this
    // way, since the runtime finishes its cycles on the drain's own thread before the
    // timer is seen. find_bind_id, wait_for_update, compile_named_callable and
    // get_watched are verbatim copies of graphix-package-tui/src/testing.rs, watch and
    // call_callback nearly so, theme_test.rs repeats the setup and
    // graphix-tests/src/lib_tests/callable.rs has a third find_bind_id, so a fix to one
    // copy misses the others (wait_idle is in the TUI's only). One shared core in
    // graphix_package_core::testing fixes both; the drain loops that wait for netidx
    // values then need next_update or wait_until, as the TUI's do. probe:
    // design/review-2026-10-05/repro/tests-ui-06.rs (tests-ui-06)
    // 2026-10-06 claude: find_bind_id, wait_for_update (now next_update) and
    // compile_named_callable live in graphix_package_core::testing, shared with the TUI.
    async fn drain(&mut self) -> Result<bool> {
        let mut changed = false;
        let timeout = tokio::time::sleep(Duration::from_millis(100));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            if self.watched.contains_key(&id) {
                                self.watched.insert(id, v.clone());
                            }
                            changed |= self.update_widget(id, &v)?;
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(50)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(changed)
    }

    /// A children recompile blocks on the runtime, which only a
    /// multi-thread flavor permits, and there only inside
    /// `block_in_place`.
    fn update_widget(&mut self, id: ExprId, v: &Value) -> Result<bool> {
        match self.rt_handle.runtime_flavor() {
            tokio::runtime::RuntimeFlavor::MultiThread => {
                tokio::task::block_in_place(|| {
                    self.widget.handle_update(&self.rt_handle, id, v)
                })
            }
            _ => self.widget.handle_update(&self.rt_handle, id, v),
        }
    }

    /// Watch a module-qualified variable such as "test::released" and
    /// return its initial value; `get_watched()` reads it after a
    /// `drain()`.
    async fn watch(&mut self, name: &str) -> Result<Value> {
        let bid = testing::find_bind_id(&self.compiled.env, name)
            .with_context(|| format!("watch: lookup {name}"))?;
        let r = self
            .gx
            .compile_ref(bid)
            .await
            .with_context(|| format!("watch: compile ref to {name}"))?;
        let initial = r.last.clone().unwrap_or(Value::Null);
        self.watched.insert(r.id, initial.clone());
        self.watch_names.insert(name.to_string(), r.id);
        self._refs.push(r);
        self.drain().await?;
        Ok(initial)
    }

    /// Get the most recent value of a watched variable by name.
    fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watch_names.get(name).and_then(|eid| self.watched.get(eid))
    }

    /// Dispatch iced Messages through the runtime and widget as
    /// `GuiHandler::about_to_wait` does, then drain.
    async fn dispatch_calls(&mut self, msgs: &[Message]) -> Result<()> {
        // the event loop's own drain
        let widget = &mut self.widget;
        crate::frame::apply_messages(&self.gx, msgs.iter().cloned(), |msg, pending| {
            let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
            widget.on_message(msg, &mut shell);
            pending.extend(shell.out.drain(..));
        });
        self.drain().await?;
        Ok(())
    }

    /// Call `view()` on the widget.
    // CR claude for claude: [test-gap] Every *_renders test in canvas_test.rs and
    // chart_test.rs calls this and drops the element, and
    // InteractionHarness::process_events (:438) builds and updates a UserInterface but
    // never draws. So canvas.rs draw_shape never runs for any shape, and the
    // candlestick, error-bar, 3D, legend and styled-mesh chart bodies never run. Only
    // fresh_chart_redraws_an_inherited_cache and markers_draw_on_every_series_kind call
    // Program::draw. A panic in one of those bodies would take down a GUI program while
    // these tests stay green. Add a draw step (ui.draw after update in
    // InteractionHarness, or Program::draw on chart and canvas roots as
    // chart_test.rs:342-348 does) and make the *_renders tests call it. (tests-ui-08)
    // 2026-10-07 claude: InteractionHarness's frame is the event loop's
    // frame::frame now, which draws (tests-ui.r2-05, merged here). The *_renders
    // tests still build their element and drop it; they need to go through it.
    fn view(&self) -> crate::widgets::IcedElement<'_> {
        self.widget.view()
    }

    /// Flush deferred per-widget state as the event loop does before a
    /// render. Call before `dt_snapshot()` after publishing to a sort
    /// column.
    #[allow(dead_code)]
    fn before_view(&mut self) -> bool {
        self.widget.before_view()
    }

    /// Drain and `before_view` in a loop until `pred(self)` holds;
    /// panics when `within` elapses.
    #[allow(dead_code)]
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
        crate::frame::frame(
            self.inner.widget.view(),
            self.viewport,
            &mut self.cache,
            &mut self.renderer,
            events,
            self.cursor,
            &mut self.clipboard,
            &mut messages,
            &theme,
        );
        messages
    }

    #[allow(dead_code)]
    async fn drain(&mut self) -> Result<bool> {
        self.inner.drain().await
    }

    /// Simulate a window resize; runs one layout pass so
    /// responsive-wrapped widgets see the new size immediately.
    #[allow(dead_code)]
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

    #[allow(dead_code)]
    fn before_view(&mut self) -> bool {
        self.inner.before_view()
    }

    #[allow(dead_code)]
    fn viewport(&self) -> Size {
        self.viewport
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

    // CR claude for claude: [dead] click_center, click_at, InteractionHarness::viewport
    // (478) and InteractionHarness::before_view (473) have no callers, nor does
    // DataTableW::dt_snapshot_value_at (widgets/data_table/test_access.rs:150), which
    // copies data_table_snapshot's cell lookup; each is hidden by #[allow(dead_code)].
    // The allows on `compiled` (38), GuiTestHarness::before_view (192), wait_until
    // (199), InteractionHarness::drain (452) and resize (459) cover items that are
    // used. Delete the five helpers and every one of these allows, so the compiler
    // reports the next helper that goes dead. (tests-ui.r2-16)
    #[allow(dead_code)]
    fn click_center(&mut self) -> Vec<Message> {
        let center = Point::new(self.viewport.width / 2.0, self.viewport.height / 2.0);
        self.click(center)
    }

    #[allow(dead_code)]
    fn click_at(&mut self, frac_x: f32, frac_y: f32) -> Vec<Message> {
        let pos = Point::new(self.viewport.width * frac_x, self.viewport.height * frac_y);
        self.click(pos)
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
                let mut shell = MessageShell::new(Point::ORIGIN);
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
    assert!(!calls.is_empty(), "expected a Call message matching predicate, got none");
    calls[0]
}
