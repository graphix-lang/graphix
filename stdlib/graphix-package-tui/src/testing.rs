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
use graphix_compiler::expr::VfsResolver;
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use ratatui::{
    Terminal,
    backend::{Backend, TestBackend},
    buffer::Buffer,
};
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

use crate::{Screen, SizeV, TuiControl};

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
    screen: Screen<NoExt>,
    /// Ctrl-C was dispatched: the display would have stopped.
    stopped: bool,
    terminal: Terminal<TestBackend>,
    watches: testing::Watches,
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
        gx.with_ctx(|ctx| {
            let control = ctx.libstate.get_or_default::<TuiControl>();
            control.0.headless.store(true, std::sync::atomic::Ordering::Relaxed)
        })
        .await?;
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let expr_id = compiled.exprs[0].id;

        let mut screen = Screen::new(gx.clone(), &compiled.env, expr_id)?;
        screen.resize(SizeV::new(width, height))?;
        let initial_value = testing::next_update(
            &mut rx,
            expr_id,
            tokio::time::Instant::now() + Duration::from_secs(5),
        )
        .await?;
        screen.update(expr_id, initial_value).await.context("compile widget tree")?;

        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend).context("build TestBackend terminal")?;

        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            screen,
            stopped: false,
            terminal,
            watches: testing::Watches::default(),
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
        let Self { gx, rx, screen, watches, .. } = self;
        let deliver = async |id, v: Value| {
            watches.note(id, &v);
            screen.update(id, v).await.context("widget handle_update")?;
            Ok(true)
        };
        Ok(testing::drain_idle(gx, rx, deliver).await?.1.map(|t| t.into_std()))
    }

    /// Wait up to `timeout` for the runtime to send an update, then
    /// `drain`. False when nothing arrived.
    pub async fn next_update(&mut self, timeout: Duration) -> Result<bool> {
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            match tokio::time::timeout_at(deadline, self.rx.recv()).await {
                Ok(Some(mut batch)) => {
                    let mut updated = false;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            updated = true;
                            self.watches.note(id, &v);
                            self.screen
                                .update(id, v)
                                .await
                                .context("widget handle_update")?;
                        }
                    }
                    if updated {
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
        let v = self.watches.watch(&self.gx, &self.compiled.env, name).await?;
        self.drain().await?;
        Ok(v)
    }

    /// Latest value of a name passed to `watch`.
    pub fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watches.get(name)
    }

    /// Deliver a crossterm event to the widget tree, then drain any
    /// reactive updates the callbacks produce. Same path the live
    /// runtime takes in `run()`.
    pub async fn dispatch_event(&mut self, e: Event) -> Result<()> {
        self.dispatch_events([e]).await
    }

    /// Deliver several events before draining, as a burst of input
    /// (type-ahead, a paste) reaches the display.
    pub async fn dispatch_events(
        &mut self,
        es: impl IntoIterator<Item = Event>,
    ) -> Result<()> {
        for e in es {
            self.deliver(&e).await?
        }
        self.drain().await
    }

    /// The display's path: `tui::size` on a resize, `tui::event`, the
    /// widgets; a Ctrl-C stops it.
    async fn deliver(&mut self, e: &Event) -> Result<()> {
        if !self.stopped {
            self.stopped = !self.screen.event(e).await.context("widget handle_event")?;
        }
        Ok(())
    }

    /// Whether a Ctrl-C was dispatched, which stops a display.
    pub fn stopped(&self) -> bool {
        self.stopped
    }

    /// Deliver a crossterm event and report how long the runtime took
    /// to settle after it: dispatch to the LAST update batch (zero when
    /// the event produced no update).
    pub async fn dispatch_event_timed(&mut self, e: Event) -> Result<Duration> {
        let start = Instant::now();
        self.deliver(&e).await?;
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
                if let Err(e) = self.screen.draw(f) {
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
    /// expected line is right-padded to the terminal width, and rows
    /// past the last expected one are blank.
    pub fn assert_lines(&mut self, expected: &[&str]) -> Result<()> {
        let actual = self.render_lines()?;
        let size = self.terminal.backend().size().unwrap_or_default();
        let (w, h) = (size.width as usize, size.height as usize);
        let expected_padded: Vec<String> = (0..h)
            .map(|i| format!("{:w$}", expected.get(i).copied().unwrap_or("")))
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
