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

use anyhow::{Context, Result, bail};
use crossterm::event::Event;
use graphix_package_core::testing::{self, Fixture, PackageRef, TestState};
use graphix_rt::{GXEvent, NoExt};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use ratatui::{
    Terminal,
    backend::{Backend, TestBackend},
    buffer::{Buffer, Cell},
    layout::Rect,
};
use std::time::{Duration, Instant};

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
    f: Fixture,
    screen: Screen<NoExt>,
    /// Ctrl-C was dispatched: the display would have stopped.
    stopped: bool,
    terminal: Terminal<TestBackend>,
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
        let headless = |ctx: &mut TestState| {
            let control = ctx.libstate.get_or_default::<TuiControl>();
            control.0.headless.store(true, std::sync::atomic::Ordering::Relaxed)
        };
        let (f, initial_value) = Fixture::new(code, register, headless).await?;
        let mut screen = Screen::new(f.gx.clone(), &f.compiled.env, f.result_id())?;
        screen.resize(SizeV::new(width, height))?;
        screen
            .update(f.result_id(), initial_value)
            .await
            .context("compile widget tree")?;
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend).context("build TestBackend terminal")?;
        Ok(Self { f, screen, stopped: false, terminal })
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
        let Self { f: Fixture { gx, rx, watches, .. }, screen, .. } = self;
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
            match tokio::time::timeout_at(deadline, self.f.rx.recv()).await {
                Ok(Some(mut batch)) => {
                    let mut updated = false;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            updated = true;
                            self.f.watches.note(id, &v);
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

    /// Take updates until `done` holds, for at most `timeout`; past it,
    /// an error showing the last render.
    pub async fn wait_for(
        &mut self,
        timeout: Duration,
        mut done: impl FnMut(&mut Self) -> Result<bool>,
    ) -> Result<()> {
        let deadline = tokio::time::Instant::now() + timeout;
        while !done(self)? {
            if tokio::time::Instant::now() > deadline {
                bail!("timed out; render:\n{}", self.render_lines()?.join("\n"))
            }
            self.next_update(Duration::from_millis(500)).await?;
        }
        Ok(())
    }

    /// Track a graphix variable by name so its updates land in
    /// `watched`. `name` is module-qualified (e.g. `"test::clicks"`).
    /// Returns the initial value.
    pub async fn watch(&mut self, name: &str) -> Result<Value> {
        let v = self.f.watch(name).await?;
        self.drain().await?;
        Ok(v)
    }

    /// Latest value of a name passed to `watch`.
    pub fn get_watched(&self, name: &str) -> Option<&Value> {
        self.f.watches.get(name)
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
            if let Event::Resize(width, height) = *e {
                self.terminal.backend_mut().resize(width, height);
                self.terminal.resize(Rect::new(0, 0, width, height))?;
            }
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

    pub fn assert_cell(&mut self, x: u16, y: u16, expected: &Cell) -> Result<()> {
        let buf = self.render()?;
        if x >= buf.area.width || y >= buf.area.height {
            bail!("cell ({x}, {y}) outside render area {:?}", buf.area);
        }
        let actual = &buf[(x, y)];
        assert_eq!(
            (actual.symbol(), actual.fg, actual.bg, actual.modifier),
            (expected.symbol(), expected.fg, expected.bg, expected.modifier),
            "cell ({x}, {y})"
        );
        Ok(())
    }

    /// Compile a graphix-defined function (lambda) by its module-qualified
    /// name into a `CallableId`. The retained `Ref` and `Callable` keep the
    /// runtime side alive — dropping the `Callable` invalidates the id.
    pub async fn compile_named_callable(
        &mut self,
        name: &str,
    ) -> Result<graphix_rt::CallableId> {
        self.f.compile_named_callable(name).await
    }

    pub async fn call_callback(
        &mut self,
        id: graphix_rt::CallableId,
        args: ValArray,
    ) -> Result<()> {
        self.f.gx.call(id, args)?;
        self.drain().await
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

#[cfg(test)]
mod tests {
    use super::*;
    use graphix_compiler::expr::ExprId;

    #[tokio::test]
    async fn ordinary_updates_complete_synchronously() -> Result<()> {
        let mut h = TuiTestHarness::new(
            r#"
use tui::{block::block, input_handler::input_handler, layout::{child, layout},
    overlay::{layer, overlay}, scrollbar::scrollbar, tabs::tabs, text::text};
let leaf = text(&"body");
let handle = |e: tui::input_handler::Event| e ~ `Continue;
let result = layout(&[
    child(#constraint: `Fill(1), overlay(#layers: &[layer(
        tabs(&[(tui::line("tab"), input_handler(#handle: &handle, &scrollbar(&block(&leaf))))])
    )], leaf)),
    child(#constraint: `Fill(1), tui::gauge::gauge(&0.5))
])
"#,
        )
        .await?;
        let value = Value::Null;
        assert!(h.screen.root.handle_update(ExprId::new(), &value)?.is_none());
        h.render()?;
        Ok(())
    }
    #[tokio::test]
    async fn an_initial_exit_reaches_a_later_display() -> Result<()> {
        let h = TuiTestHarness::new(
            "tui::exit(null); let result = tui::text::text(&\"hello\")",
        )
        .await?;
        let (stop, stopped) = tokio::sync::oneshot::channel();
        h.f.gx
            .with_ctx(move |ctx| {
                ctx.libstate.get_or_default::<TuiControl>().install_stop(stop);
            })
            .await?;
        tokio::time::timeout(Duration::from_millis(100), stopped).await???;
        Ok(())
    }

    #[tokio::test]
    async fn exit_never_returns_a_value() -> Result<()> {
        TuiTestHarness::new(
            "let n: i64 = tui::exit(never()); let result = tui::text::text(&\"[n]\")",
        )
        .await?;
        Ok(())
    }
}
