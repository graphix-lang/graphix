//! Test harness for TUI widget integration tests.
//!
//! Compiles graphix code that produces a `Tui` value, builds the
//! widget tree the same way the runtime does, then drives it through
//! a `ratatui::Terminal<TestBackend>` so we can render and inspect
//! the resulting `Buffer` without a TTY.
//!
//! Mirrors the GUI test harness in `graphix-package-gui/src/test/mod.rs`
//! in spirit, with two differences:
//! - No GPU / iced UserInterface — we only need a `TestBackend`.
//! - Events are crossterm `Event`s, dispatched directly into the
//!   widget's `handle_event` (the same path the live runtime takes).
//!
//! The harness compiles a wrapper expression `{ mod test; test::result }`
//! against the user-supplied graphix source, waits for the first
//! reactive update, and feeds that value into `crate::compile` to get
//! a live widget tree. Subsequent updates flow through `drain()` to
//! `widget.handle_update`, the same as in production.

use ahash::AHashMap;
use anyhow::{bail, Context, Result};
use crossterm::event::Event;
use graphix_compiler::{
    expr::{ExprId, ModuleResolver},
    BindId,
};
use graphix_package_core::testing::{self, RegisterFn, TestCtx};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use nohash::IntMap;
use poolshark::global::GPooled;
use ratatui::{
    backend::{Backend, TestBackend},
    buffer::Buffer,
    Terminal,
};
use std::time::Duration;
use tokio::sync::mpsc;

use crate::{compile, input_handler::event_to_value, TuiW};

mod panic_test;
mod widgets_test;

const TEST_REGISTER: &[RegisterFn] = &[
    <graphix_package_core::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_array::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_map::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_str::P as graphix_package::Package<NoExt>>::register,
    <graphix_package_sys::P as graphix_package::Package<NoExt>>::register,
    <crate::P as graphix_package::Package<NoExt>>::register,
];

/// Default render area used by the harness when the test doesn't pick
/// its own. 40 cols × 10 rows is wide enough that most widget types
/// have something visible to assert on but small enough for cheap
/// diffing.
const DEFAULT_VIEWPORT: (u16, u16) = (40, 10);

/// Test harness for a single TUI widget tree.
pub(crate) struct TuiTestHarness {
    _ctx: TestCtx,
    gx: graphix_rt::GXHandle<NoExt>,
    #[allow(dead_code)]
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
    pub(crate) async fn new(code: &str) -> Result<Self> {
        Self::with_viewport(code, DEFAULT_VIEWPORT.0, DEFAULT_VIEWPORT.1).await
    }

    /// Like `new`, but with an explicit terminal width / height.
    pub(crate) async fn with_viewport(
        code: &str,
        width: u16,
        height: u16,
    ) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
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

        // Wait for the initial root widget value.
        let initial_value = wait_for_update(&mut rx, expr_id).await?;

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

    /// Drain pending reactive updates into the widget tree. Returns
    /// once no new updates have arrived for ~50ms (mirrors the GUI
    /// harness's quiescence heuristic).
    pub(crate) async fn drain(&mut self) -> Result<()> {
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
                            self.widget
                                .handle_update(id, v)
                                .await
                                .context("widget handle_update")?;
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(50)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    /// Track a graphix variable by name so its updates land in
    /// `watched`. `name` is module-qualified (e.g. `"test::clicks"`).
    /// Returns the initial value.
    #[allow(dead_code)]
    pub(crate) async fn watch(&mut self, name: &str) -> Result<Value> {
        let bid = find_bind_id(&self.compiled.env, name)
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
    #[allow(dead_code)]
    pub(crate) fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watch_names.get(name).and_then(|eid| self.watched.get(eid))
    }

    /// Deliver a crossterm event to the widget tree, then drain any
    /// reactive updates the callbacks produce. Same path the live
    /// runtime takes in `run()`.
    #[allow(dead_code)]
    pub(crate) async fn dispatch_event(&mut self, e: Event) -> Result<()> {
        let v = event_to_value(&e);
        self.widget.handle_event(e, v).await.context("widget handle_event")?;
        self.drain().await
    }

    /// Render the widget into the test backend's buffer and return a
    /// reference to that buffer. The render path is exactly the one
    /// the live runtime takes — `Terminal::draw(|f| widget.draw(f, area))`
    /// — so any panic in ratatui surfaces here.
    pub(crate) fn render(&mut self) -> Result<&Buffer> {
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
    pub(crate) fn render_lines(&mut self) -> Result<Vec<String>> {
        let buf = self.render()?;
        Ok(buffer_lines(buf))
    }

    /// Render and assert the buffer matches the expected lines. Each
    /// expected line is right-padded to the terminal width before
    /// comparison.
    #[allow(dead_code)]
    pub(crate) fn assert_lines(&mut self, expected: &[&str]) -> Result<()> {
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
    #[allow(dead_code)]
    pub(crate) fn cell_at(&mut self, x: u16, y: u16) -> Result<String> {
        let buf = self.render()?;
        if x >= buf.area.width || y >= buf.area.height {
            bail!("cell ({x}, {y}) outside render area {:?}", buf.area);
        }
        Ok(buf[(x, y)].symbol().to_string())
    }

    /// Compile a graphix-defined function (lambda) by its module-qualified
    /// name into a `CallableId` we can invoke from tests. Mirrors the
    /// GUI harness method of the same name. The retained `Ref` and
    /// `Callable` keep the runtime side alive — dropping the
    /// `Callable` invalidates the id.
    #[allow(dead_code)]
    pub(crate) async fn compile_named_callable(
        &mut self,
        name: &str,
    ) -> Result<graphix_rt::CallableId> {
        let bid = find_bind_id(&self.compiled.env, name)
            .with_context(|| format!("compile_named_callable: lookup {name}"))?;
        let r = self
            .gx
            .compile_ref(bid)
            .await
            .with_context(|| format!("compile_named_callable: compile_ref {name}"))?;
        let val = r
            .last
            .clone()
            .with_context(|| format!("compile_named_callable: no value for {name}"))?;
        let cb = self.gx.compile_callable(val).await.with_context(|| {
            format!("compile_named_callable: compile_callable {name}")
        })?;
        let id = cb.id();
        self._refs.push(r);
        self._callables.push(cb);
        Ok(id)
    }

    /// Dispatch a callback through the runtime by `CallableId` and
    /// drain resulting updates.
    #[allow(dead_code)]
    pub(crate) async fn call_callback(
        &mut self,
        id: graphix_rt::CallableId,
        args: ValArray,
    ) -> Result<()> {
        self.gx.call(id, args)?;
        self.drain().await
    }

    /// Convenience for the panic-input pattern: drive the widget
    /// through several reactive update cycles, rendering after each.
    /// Used by panic-surface tests where we only care that the render
    /// doesn't blow up.
    #[allow(dead_code)]
    pub(crate) async fn render_through_updates(&mut self, ticks: usize) -> Result<()> {
        for _ in 0..ticks {
            self.drain().await?;
            let _ = self.render()?;
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        Ok(())
    }
}

/// Wait up to 5s for a specific expression's update.
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

/// Look up a `BindId` for a module-qualified name like `"test::clicks"`.
/// Same logic as the GUI harness — env scope keys are generated paths
/// (`/do…/test`), so we suffix-match.
fn find_bind_id(env: &graphix_compiler::env::Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
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

/// Render a `Buffer` into one `String` per row by concatenating each
/// cell's `symbol()`. Empty / overdrawn cells naturally render as a
/// single space (ratatui's contract for `Cell::symbol`).
///
/// Multi-width glyphs (CJK, some emoji) currently produce a trailing
/// space for the cell ratatui marks empty after the wide char — fine
/// for current TUI tests, all ASCII so far.
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
