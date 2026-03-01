use anyhow::{bail, Context, Result};
use graphix_compiler::expr::{ExprId, ModuleResolver};
use graphix_compiler::BindId;
use graphix_package_core::testing::{self, RegisterFn, TestCtx};
use graphix_rt::{CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

use crate::widgets::{self, GuiW, Message};

mod canvas_test;
mod chart_test;
mod clipboard_test;
mod interaction_test;
mod widgets_test;

const TEST_REGISTER: &[RegisterFn] = &[
    <graphix_package_core::P as graphix_package::Package<NoExt>>::register,
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

        // Drain any additional updates that arrive during widget compilation
        while rx.try_recv().is_ok() {}

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

    /// Dispatch Call messages back into the runtime and drain resulting updates.
    async fn dispatch_calls(&mut self, msgs: &[Message]) -> Result<()> {
        for msg in msgs {
            if let Message::Call(id, args) = msg {
                self.gx.call(*id, args.clone())?;
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
