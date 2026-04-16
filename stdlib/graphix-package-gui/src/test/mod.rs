use anyhow::{bail, Context, Result};
use graphix_compiler::expr::{ExprId, ModuleResolver};
use graphix_compiler::BindId;
use graphix_package_core::testing::{self, RegisterFn, TestCtx};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

use crate::widgets::{self, GuiW, Message};

mod canvas_test;
mod chart_test;
mod clipboard_test;
mod data_table_test;
mod interaction_test;
mod widgets_test;

const TEST_REGISTER: &[RegisterFn] = &[
    <graphix_package_core::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_map::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_str::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_sys::P as graphix_package::Package<NoExt>>::register,
    <crate::P as graphix_package::Package<NoExt>>::register,
];

/// Test harness for GUI widget integration tests.
///
/// Compiles graphix code that produces a Widget value, builds the
/// widget tree, and provides helpers for simulating interactions
/// through the reactive loop.
struct GuiTestHarness {
    _ctx: TestCtx,
    gx: graphix_rt::GXHandle<NoExt>,
    #[allow(dead_code)]
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt_handle: tokio::runtime::Handle,
    watched: fxhash::FxHashMap<ExprId, Value>,
    watch_names: fxhash::FxHashMap<String, ExprId>,
    _refs: Vec<Ref<NoExt>>,
    _callables: Vec<Callable<NoExt>>,
}

impl GuiTestHarness {
    /// Compile graphix code that produces a Widget value.
    ///
    /// `code` is module-level graphix code. The last binding should be
    /// named `result` and evaluate to a Widget value.
    /// Example: `"use gui; let result = gui::text(content: &\"hello\")"`.
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = fxhash::FxHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            arcstr::ArcStr::from(code),
        )]);
        let resolver = ModuleResolver::VFS(tbl);
        let ctx = testing::init_with_resolvers(tx, TEST_REGISTER, vec![resolver]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let expr_id = compiled.exprs[0].id;

        // Wait for the initial value
        let initial_value = wait_for_update(&mut rx, expr_id).await?;

        // Compile the widget value into a widget tree
        let widget = widgets::compile(gx.clone(), initial_value)
            .await
            .context("compile widget tree")?;

        let rt_handle = tokio::runtime::Handle::current();

        let mut widget = widget;

        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            widget,
            rt_handle,
            watched: fxhash::FxHashMap::default(),
            watch_names: fxhash::FxHashMap::default(),
            _refs: Vec::new(),
            _callables: Vec::new(),
        })
    }

    /// Drain all pending reactive updates into the widget tree.
    /// Returns true if any updates were processed.
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
                            changed |= self.widget.handle_update(
                                &self.rt_handle, id, &v
                            )?;
                        }
                    }
                    // Reset timeout after each batch
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(50)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(changed)
    }

    /// Watch a graphix variable by name and return its initial value.
    ///
    /// The name should be a module-qualified path like "test::released".
    /// Looks up the BindId in the compiled env and creates a Ref to
    /// track updates. Use `get_watched()` to read the latest value
    /// after calling `drain()`.
    async fn watch(&mut self, name: &str) -> Result<Value> {
        let bid = find_bind_id(&self.compiled.env, name)
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
        // Drain to pick up the ref's initial update event
        self.drain().await?;
        Ok(initial)
    }

    /// Get the most recent value of a watched variable by name.
    fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watch_names.get(name).and_then(|eid| self.watched.get(eid))
    }

    /// Dispatch iced Messages back through the runtime and widget,
    /// mirroring `GuiHandler::about_to_wait` in src/event_loop.rs so
    /// tests see the same effects as production. Drains resulting
    /// reactive updates at the end.
    async fn dispatch_calls(&mut self, msgs: &[Message]) -> Result<()> {
        for msg in msgs {
            match msg {
                Message::Nop => {}
                Message::Call(id, args) => {
                    self.gx.call(*id, args.clone())?;
                }
                Message::CellClick(row, col) => {
                    self.widget.handle_cell_click(*row, col.clone());
                }
                Message::CellEdit(row, col) => {
                    self.widget.handle_cell_edit(*row, col.clone());
                }
                Message::CellEditInput(text) => {
                    self.widget.handle_cell_edit_input(text.clone());
                }
                Message::CellEditSubmit => {
                    self.widget.handle_cell_edit_submit();
                }
                Message::CellEditCancel => {
                    self.widget.handle_cell_edit_cancel();
                }
                Message::TableKey(action) => {
                    self.widget.handle_table_key(action);
                }
                Message::Scroll(v, h, vp_w, vp_h) => {
                    self.widget.handle_scroll(*v, *h, *vp_w, *vp_h);
                }
                // These are host-handled in production; tests that care
                // invoke the widget handler methods directly.
                Message::ColumnResizeStart(_)
                | Message::ColumnResizeMove(_)
                | Message::ColumnResizeEnd => {}
                Message::EditorAction(id, action) => {
                    if let Some((cid, v)) = self.widget.editor_action(*id, action) {
                        self.gx.call(cid, ValArray::from_iter([v]))?;
                    }
                }
            }
        }
        self.drain().await?;
        Ok(())
    }

    /// Call view() on the widget. Panicking here means the widget
    /// tree is in an inconsistent state.
    fn view(&self) -> crate::widgets::IcedElement<'_> {
        self.widget.view()
    }

    /// Mirror what the iced event loop does between a wake and the next
    /// render: flush any deferred per-widget state (e.g. data_table
    /// re-sort triggered by sort-column subscription updates that
    /// arrived since the last drain). Tests that publish values to a
    /// sort column should call this before reading `dt_snapshot()`.
    #[allow(dead_code)]
    fn before_view(&mut self) -> bool {
        self.widget.before_view()
    }

    /// Get a DataTableSnapshot from the widget, if it is a data table.
    fn dt_snapshot(&self) -> crate::widgets::DataTableSnapshot {
        self.widget
            .data_table_snapshot()
            .expect("widget is not a DataTableW")
    }

    /// Downcast the root widget to `DataTableW<NoExt>` for direct
    /// access to test-only accessors. Panics if the widget is not a
    /// data table — every test using this helper compiles a `data_table`
    /// at the root of its graphix code.
    fn dt(&self) -> &crate::widgets::data_table::DataTableW<NoExt> {
        self.widget
            .as_any()
            .downcast_ref::<crate::widgets::data_table::DataTableW<NoExt>>()
            .expect("widget is not a DataTableW")
    }

    /// Dispatch a callback through the runtime by its CallableId and
    /// drain resulting reactive updates. Used by data_table tests to
    /// invoke per-cell callbacks (on_edit/on_click/on_resize) without
    /// going through pixel-layout: the widget itself fires the same
    /// callable internally, so calling it through the bridge mirrors
    /// the runtime's behavior.
    async fn call_callback(
        &mut self,
        id: graphix_rt::CallableId,
        args: ValArray,
    ) -> Result<()> {
        self.gx.call(id, args)?;
        self.drain().await?;
        Ok(())
    }

    /// Compile a graphix-defined function (lambda) by its module-qualified
    /// name into a `CallableId`. Mirrors the existing `watch` lookup but
    /// returns a callable rather than a tracked ref. The `Ref` and
    /// `Callable` are retained on the harness — dropping the
    /// `Callable` immediately sends `DeleteCallable` to the runtime,
    /// invalidating the returned id.
    async fn compile_named_callable(
        &mut self,
        name: &str,
    ) -> Result<graphix_rt::CallableId> {
        let bid = find_bind_id(&self.compiled.env, name)
            .with_context(|| format!("compile_named_callable: lookup {name}"))?;
        let r = self.gx.compile_ref(bid).await
            .with_context(|| format!("compile_named_callable: compile_ref {name}"))?;
        let val = r.last.clone()
            .with_context(|| format!("compile_named_callable: no value for {name}"))?;
        let cb = self.gx.compile_callable(val).await
            .with_context(|| format!("compile_named_callable: compile_callable {name}"))?;
        let id = cb.id();
        self._refs.push(r);
        self._callables.push(cb);
        Ok(id)
    }
}

/// Wait for a specific expression's update, with timeout.
async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target_id: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == target_id {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for initial widget value"),
        }
    }
}

/// Find a BindId by a short name like "test::released" in the env.
///
/// The env stores bindings under generated scope prefixes (e.g.
/// `/do1234/test`), so we can't use `lookup_bind` with a root scope.
/// Instead, scan all scopes for one whose suffix matches the module
/// path and contains the variable name.
fn find_bind_id(env: &graphix_compiler::env::Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
    // Split "test::released" into module = "test", var = "released"
    let parts: Vec<&str> = name.split("::").collect();
    let (module, var) = match parts.as_slice() {
        [module, var] => (*module, *var),
        _ => bail!("expected module::var, got {name}"),
    };
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name} found in env")
}

// ── Headless GPU ────────────────────────────────────────────────────

use iced_core::{clipboard, mouse, Event, Point, Size};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::graphics::Shell;
use iced_wgpu::wgpu;
use tokio::sync::OnceCell;

/// Shared headless wgpu adapter + device. Creating GPU resources is
/// expensive, so we initialize once and share across all tests.
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
            // Try hardware adapter first, fall back to software
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

// ── Interaction Harness ─────────────────────────────────────────────

/// Test harness that wraps `GuiTestHarness` with a headless renderer
/// and iced `UserInterface` to simulate user interactions (clicks,
/// typing, drags) and collect the resulting `Message`s.
struct InteractionHarness {
    inner: GuiTestHarness,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    viewport: Size,
    cursor_position: Point,
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
        })
    }

    /// Build a UserInterface, feed events, and return the messages
    /// produced by widget callbacks.
    fn process_events(&mut self, events: &[Event]) -> Vec<Message> {
        let element = self.inner.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui =
            UserInterface::build(element, self.viewport, cache, &mut self.renderer);
        let mut messages = Vec::new();
        let mut clipboard = clipboard::Null;
        let cursor = mouse::Cursor::Available(self.cursor_position);
        let (_state, _statuses) =
            ui.update(events, cursor, &mut self.renderer, &mut clipboard, &mut messages);
        self.cache = ui.into_cache();
        messages
    }

    #[allow(dead_code)]
    async fn drain(&mut self) -> Result<bool> {
        self.inner.drain().await
    }

    fn view(&self) -> crate::widgets::IcedElement<'_> {
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

    // ── Interaction helpers ─────────────────────────────────────

    fn click(&mut self, pos: Point) -> Vec<Message> {
        self.cursor_position = pos;
        let mut all = Vec::new();
        // Each event needs its own UI frame so widget state machines
        // (pressed → released) transition correctly.
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
            // Each character as a separate frame
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

    /// Route `Message::EditorAction` messages through the widget's
    /// `editor_action` method and collect the callback results.
    fn process_editor_actions(&mut self, msgs: &[Message]) -> Vec<(CallableId, Value)> {
        msgs.iter()
            .filter_map(|m| match m {
                Message::EditorAction(id, action) => {
                    self.inner.widget.editor_action(*id, action)
                }
                _ => None,
            })
            .collect()
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

// ── Message assertion helpers ───────────────────────────────────────

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
