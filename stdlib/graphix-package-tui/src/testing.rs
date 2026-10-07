//! Headless test harness for TUI programs — this crate's widget tests
//! and any external package building a TUI app (enable the `testing`
//! feature).
//!
//! Compiles graphix code that produces a `Tui` value, builds the widget
//! tree the same way the runtime does, and drives it through a
//! `ratatui::Terminal<TestBackend>`. Events are crossterm `Event`s
//! dispatched into the widget's `handle_event`, the live runtime's path.
//! External packages register themselves via
//! [`TuiTestHarness::with_register`].

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use crossterm::event::Event;
use graphix_compiler::expr::ExprId;
use graphix_compiler::expr::VfsResolver;
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use nohash::IntMap;
use poolshark::global::GPooled;
use ratatui::{
    Terminal,
    backend::{Backend, TestBackend},
    buffer::Buffer,
};
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

use crate::{TuiW, compile, input_handler::event_to_value};

/// The register `new`/`with_viewport` use: this crate's runtime deps +
/// itself. A program needing more (map, a package under test) passes
/// its own via `with_register`.
const DEFAULT_REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &crate::P,
];

/// Default render area when the test doesn't pick its own.
const DEFAULT_VIEWPORT: (u16, u16) = (40, 10);

/// Test harness for a single TUI widget tree.
pub struct TuiTestHarness {
    _ctx: TestCtx,
    gx: graphix_rt::GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: TuiW,
    terminal: Terminal<TestBackend>,
    watched: IntMap<ExprId, Value>,
    watch_names: AHashMap<String, ExprId>,
    _refs: Vec<Ref<NoExt>>,
    _callables: Vec<Callable<NoExt>>,
}

impl TuiTestHarness {
    /// Compile graphix `code` (a module body whose last binding is
    /// named `result` and evaluates to a `Tui` value) and build the
    /// widget tree at `DEFAULT_VIEWPORT`.
    pub async fn new(code: &str) -> Result<Self> {
        Self::with_viewport(code, DEFAULT_VIEWPORT.0, DEFAULT_VIEWPORT.1).await
    }

    /// Like `new`, but with an explicit terminal width / height.
    pub async fn with_viewport(code: &str, width: u16, height: u16) -> Result<Self> {
        Self::with_register(code, DEFAULT_REGISTER, width, height).await
    }

    /// Like `with_viewport`, but registering `register` instead of the
    /// stdlib default — the entry point for a package outside this
    /// crate testing its own TUI (pass the crate's `defpackage!`
    /// TEST_REGISTER so the program can `use` the package's modules).
    pub async fn with_register(
        code: &str,
        register: &[PackageRef],
        width: u16,
        height: u16,
    ) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let resolver = VfsResolver::new(tbl);
        let ctx = testing::init_with_resolvers(tx, register, vec![resolver]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let expr_id = compiled.exprs[0].id;

        // Wait for the initial root widget value.
        let initial_value = testing::next_update(
            &mut rx,
            expr_id,
            tokio::time::Instant::now() + Duration::from_secs(5),
        )
        .await?;

        // Build the live widget tree. Same path as the runtime's `run()`
        // takes for the root expression's first delivery.
        let widget =
            compile(gx.clone(), initial_value).await.context("compile widget tree")?;

        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend).context("build TestBackend terminal")?;

        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            widget,
            terminal,
            watched: IntMap::default(),
            watch_names: AHashMap::default(),
            _refs: Vec::new(),
            _callables: Vec::new(),
        })
    }

    /// Deliver reactive updates into the widget tree until the runtime is
    /// idle and none are left. A pending timer or IO reply is not waited
    /// for: `next_update` waits for what it brings.
    pub async fn drain(&mut self) -> Result<()> {
        self.drain_timed().await.map(|_| ())
    }

    /// `drain`, answering when the last update batch arrived (`None` when
    /// there was none).
    async fn drain_timed(&mut self) -> Result<Option<Instant>> {
        let mut last = None;
        loop {
            let idle = self.gx.wait_idle();
            tokio::pin!(idle);
            let mut delivered = false;
            loop {
                tokio::select! {
                    biased;
                    Some(batch) = self.rx.recv() => {
                        if deliver(&mut self.widget, &mut self.watched, batch).await? {
                            delivered = true;
                            last = Some(Instant::now());
                        }
                    }
                    r = &mut idle => break r?,
                }
            }
            if !delivered {
                return Ok(last);
            }
        }
    }

    /// Wait up to `timeout` for the runtime to send an update, then
    /// `drain`. False when nothing arrived.
    pub async fn next_update(&mut self, timeout: Duration) -> Result<bool> {
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            match tokio::time::timeout_at(deadline, self.rx.recv()).await {
                Ok(Some(batch)) => {
                    if deliver(&mut self.widget, &mut self.watched, batch).await? {
                        self.drain().await?;
                        return Ok(true);
                    }
                }
                Ok(None) => bail!("the runtime is gone"),
                Err(_) => return Ok(false),
            }
        }
    }

    /// Track a graphix variable by name so its updates land in
    /// `watched`. `name` is module-qualified (e.g. `"test::clicks"`).
    /// Returns the initial value.
    pub async fn watch(&mut self, name: &str) -> Result<Value> {
        let bid = testing::find_bind_id(&self.compiled.env, name)
            .with_context(|| format!("watch: lookup {name}"))?;
        let r = self
            .gx
            .compile_ref(bid)
            .await
            .with_context(|| format!("watch: compile_ref {name}"))?;
        let initial = r.last.clone().unwrap_or(Value::Null);
        self.watched.insert(r.id, initial.clone());
        self.watch_names.insert(name.to_string(), r.id);
        self._refs.push(r);
        self.drain().await?;
        Ok(initial)
    }

    /// Latest value of a name passed to `watch`.
    pub fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watch_names.get(name).and_then(|eid| self.watched.get(eid))
    }

    /// Deliver a crossterm event to the widget tree, then drain any
    /// reactive updates the callbacks produce. Same path the live
    /// runtime takes in `run()`.
    pub async fn dispatch_event(&mut self, e: Event) -> Result<()> {
        let v = event_to_value(&e);
        self.widget.handle_event(e, v).await.context("widget handle_event")?;
        self.drain().await
    }

    /// Deliver a crossterm event and report how long the runtime took
    /// to settle after it: dispatch to the LAST update batch (zero when
    /// the event produced no update).
    pub async fn dispatch_event_timed(&mut self, e: Event) -> Result<Duration> {
        let start = Instant::now();
        let v = event_to_value(&e);
        self.widget.handle_event(e, v).await.context("widget handle_event")?;
        Ok(self.drain_timed().await?.map_or(Duration::ZERO, |t| t - start))
    }

    /// Render the widget into the test backend's buffer and return a
    /// reference to that buffer. The render path is exactly the one
    /// the live runtime takes — `Terminal::draw(|f| widget.draw(f, area))`
    /// — so any panic in ratatui surfaces here.
    pub fn render(&mut self) -> Result<&Buffer> {
        let mut draw_err: Option<anyhow::Error> = None;
        self.terminal
            .draw(|f| {
                let area = f.area();
                if let Err(e) = self.widget.draw(f, area) {
                    draw_err = Some(e);
                }
            })
            .context("Terminal::draw failed")?;
        if let Some(e) = draw_err {
            return Err(e.context("widget.draw returned Err"));
        }
        Ok(self.terminal.backend().buffer())
    }

    /// Render and return the buffer's lines as plain strings — handy
    /// for tests that just want to assert content without caring about
    /// styles.
    pub fn render_lines(&mut self) -> Result<Vec<String>> {
        let buf = self.render()?;
        Ok(buffer_lines(buf))
    }

    /// Render and assert the buffer matches the expected lines. Each
    /// expected line is right-padded to the terminal width before
    /// comparison.
    pub fn assert_lines(&mut self, expected: &[&str]) -> Result<()> {
        let actual = self.render_lines()?;
        let expected_padded: Vec<String> = expected
            .iter()
            .map(|s| {
                let w = self.terminal.backend().size().unwrap_or_default().width as usize;
                format!("{:width$}", s, width = w)
            })
            .collect();
        assert_eq!(
            actual, expected_padded,
            "rendered buffer didn't match expected lines"
        );
        Ok(())
    }

    /// Quick "did anything render at this cell" probe.
    pub fn cell_at(&mut self, x: u16, y: u16) -> Result<String> {
        let buf = self.render()?;
        if x >= buf.area.width || y >= buf.area.height {
            bail!("cell ({x}, {y}) outside render area {:?}", buf.area);
        }
        Ok(buf[(x, y)].symbol().to_string())
    }

    /// Compile a graphix-defined function (lambda) by its module-qualified
    /// name into a `CallableId`. The retained `Ref` and `Callable` keep the
    /// runtime side alive — dropping the `Callable` invalidates the id.
    pub async fn compile_named_callable(
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

    /// Dispatch a callback through the runtime by `CallableId` and
    /// drain resulting updates.
    pub async fn call_callback(
        &mut self,
        id: graphix_rt::CallableId,
        args: ValArray,
    ) -> Result<()> {
        self.gx.call(id, args)?;
        self.drain().await
    }

    /// Drive the widget through several reactive update cycles,
    /// rendering after each.
    pub async fn render_through_updates(&mut self, ticks: usize) -> Result<()> {
        for _ in 0..ticks {
            self.drain().await?;
            let _ = self.render()?;
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        Ok(())
    }
}

/// Deliver a batch's updates; false when it held none (the runtime sends
/// a batch every cycle, empty or not).
// CR claude for claude: [test-gap] The harness says it builds and drives the tree the way
// the runtime does, but it differs from the display in four ways that hide bugs from
// tests. An update to the root expression goes to `handle_update` instead of rebuilding
// the tree as display does (lib.rs:804-808), so a program whose root re-fires (a
// `select` over screens) keeps its first tree. `tui::size` and `tui::event` are never
// set, so a program reading them sees nothing. `dispatch_event` always drains, so no
// test can put a second event in front of the input handler while a reply is pending,
// and Ctrl-C reaches the widgets instead of stopping. Rebuild on the root id here, set
// `size` from the viewport and `event` on each dispatch, and add a dispatch that
// delivers several events before draining. (tui-core-10)
async fn deliver(
    widget: &mut TuiW,
    watched: &mut IntMap<ExprId, Value>,
    mut batch: GPooled<Vec<GXEvent>>,
) -> Result<bool> {
    let mut updated = false;
    for event in batch.drain(..) {
        if let GXEvent::Updated(id, v) = event {
            updated = true;
            if watched.contains_key(&id) {
                watched.insert(id, v.clone());
            }
            widget.handle_update(id, v).await.context("widget handle_update")?;
        }
    }
    Ok(updated)
}

/// Render a `Buffer` into one `String` per row by concatenating each
/// cell's `symbol()`. Empty / overdrawn cells render as a single space;
/// a wide glyph leaves a trailing space for the cell after it.
fn buffer_lines(buf: &Buffer) -> Vec<String> {
    let mut out = Vec::with_capacity(buf.area.height as usize);
    for y in 0..buf.area.height {
        let mut line = String::new();
        for x in 0..buf.area.width {
            line.push_str(buf[(x, y)].symbol());
        }
        out.push(line);
    }
    out
}
