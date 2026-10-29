//! Differential oracle for the graphix fusion/JIT backend.
//!
//! A program runs under two configurations of the same front-end:
//! **interp** (`CFlag::FusionDisabled`, the node-walk reference) and
//! **jit** (fusion + cranelift, the system under test). For any
//! deterministic program they must produce the same observable result:
//! a per-cycle trace ([`trace::Trace`]) of every value `result` emits,
//! so extra fires, missing fires and wrong pacing are divergences too.
//! A program that never emits is an instant empty-trace agreement.
//! See `design/graphix_fuzz.md`.

pub mod callable;
pub mod corpus;
pub mod files;
pub mod generate;
pub mod mustreject;
pub mod mutate;
pub mod schedule;
pub mod trace;
pub mod typemorph;

use ahash::AHashMap;
use arcstr::ArcStr;
use bytes::Bytes;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    FusionStats, ParMode, Scope,
    env::Env,
    expr::{Expr, Origin, VfsEntry, VfsResolver, parser},
    typ::Naked,
};
use graphix_package::Package;
use graphix_package_core::testing::{TestCtx, config};
use graphix_rt::{CompRes, GXEvent, NoExt, RegistrationImage};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_core::path::Path;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use std::{collections::BTreeMap, fmt, future, str::FromStr, time::Duration};
use tokio::sync::{mpsc, oneshot};

/// Every stdlib package, so generated programs can use the whole
/// language surface. Mirrors `graphix-tests`'s `TEST_REGISTER` (which is
/// `#[cfg(test)]`-gated and so not importable).
pub const REGISTER: &[&dyn Package<NoExt>] = graphix_package::package_refs!();

pub use graphix_package_core::testing::Mode;

/// The fork mode the fuzzer runs `mode` in: the node-walk's parallel runs
/// fork every fork point; the JIT's run the default mode over a tiny
/// base wake threshold ([`AUTO_T`]), exercising `Auto`'s probes, shapes
/// and feedback; the serial modes fork nothing, whatever
/// `GRAPHIX_PAR` says.
pub fn fuzz_par(mode: Mode) -> ParMode {
    match mode {
        Mode::Par => ParMode::Force,
        Mode::JitPar => ParMode::Auto,
        Mode::Interp | Mode::Jit | Mode::Unpruned => ParMode::Off,
    }
}

/// The base wake threshold, in ticks, fixed for the fuzzer's `Auto` runs.
/// Sites still account for observed overhead and unprofitable forks.
pub const AUTO_T: u64 = 4;

/// `GRAPHIX_FUZZ_FORK=0` disables the forced-fork runs (default on).
fn par_enabled() -> bool {
    std::env::var("GRAPHIX_FUZZ_FORK").map_or(true, |v| v != "0")
}

/// How a `callable-v1` program's dispatch epochs are delivered (see
/// [`callable::CallSpec`]). Programs with no callable header behave
/// identically on both routes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Route {
    /// Injections on the driver's argument bindings — the in-language
    /// call, driven by the schedule machinery verbatim.
    InLanguage,
    /// `GXHandle::compile_callable` + `Callable::call` — the embedder
    /// path every GUI/TUI handler dispatch takes.
    Dispatch,
}

/// The result of running one program under one mode.
#[derive(Debug, Clone)]
pub enum Outcome {
    /// Ran to quiescence (or the trace budget): the per-cycle history
    /// of everything `result` emitted. A bottom program is an empty
    /// trace — agreement, resolved instantly at quiescence.
    Trace(trace::Trace),
    /// Did not compile (parse / typecheck error).
    CompileErr(String),
    /// Runtime error / the runtime died before producing a result.
    RuntimeErr(String),
    /// Contained outside the language: neither quiesced nor hit the
    /// trace budget within the wall-clock backstop, or the stack budget
    /// aborted it first.
    Timeout(Containment),
    /// Accepted by the check alone ([`check_only`]): only the check side
    /// of a [`Pair::Check`] holds it.
    Checked,
}

/// `o` for a person: values by their Display, at most 2048 bytes (a
/// clipped render ends in `…clipped`), so a huge value costs neither
/// stack nor time.
pub fn render(o: &Outcome) -> String {
    const LIMIT: usize = 2048;
    struct Clip(String);
    impl fmt::Write for Clip {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            let room = LIMIT - self.0.len();
            if s.len() <= room {
                self.0.push_str(s);
                return Ok(());
            }
            let mut i = room;
            while !s.is_char_boundary(i) {
                i -= 1;
            }
            self.0.push_str(&s[..i]);
            Err(fmt::Error)
        }
    }
    let mut c = Clip(String::new());
    if fmt::write(&mut c, format_args!("{}", Rendered(o))).is_err() {
        c.0.push_str(" …clipped");
    }
    c.0
}

struct Rendered<'a>(&'a Outcome);

impl fmt::Display for Rendered<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Outcome::Trace(t) => {
                f.write_str("Trace(")?;
                for (i, e) in t.epochs.iter().enumerate() {
                    f.write_str(if i == 0 { "[" } else { "; [" })?;
                    for (j, (o, v)) in e.events.iter().enumerate() {
                        if j > 0 {
                            f.write_str(" ")?;
                        }
                        write!(f, "{o}:{}", Naked(v))?;
                    }
                    f.write_str(if e.capped { " …capped]" } else { "]" })?;
                }
                for (i, l) in t.stdout.iter().enumerate() {
                    f.write_str(if i == 0 { "; stdout=[" } else { " | " })?;
                    write!(f, "{l}")?;
                }
                if !t.stdout.is_empty() {
                    f.write_str("]")?;
                }
                if let Some(code) = t.exit {
                    write!(f, "; exit={code}")?;
                }
                f.write_str(")")
            }
            Outcome::CompileErr(e) => {
                f.write_str("CompileErr(")?;
                for (i, l) in e.split('\n').enumerate() {
                    if i > 0 {
                        f.write_str(" | ")?;
                    }
                    f.write_str(l)?;
                }
                f.write_str(")")
            }
            Outcome::RuntimeErr(e) => {
                write!(f, "RuntimeErr({})", e.lines().next().unwrap_or(""))
            }
            Outcome::Timeout(c) => write!(f, "Timeout({c:?})"),
            Outcome::Checked => f.write_str("Checked"),
        }
    }
}

/// What stopped a run. Runtime limits agree with one another because
/// the first one reached depends on descent speed; construction agrees
/// only with construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Containment {
    Deadline,
    StackBudget,
    ActivationBudget,
    /// Runtime construction (compilation, linking or image restore)
    /// outlived the deadline. A confirmed node-walk construction timeout
    /// is a finding of its own (`Pair::Construction`).
    Compile,
}

impl Containment {
    fn agrees_with(self, other: Self) -> bool {
        matches!(self, Self::Compile) == matches!(other, Self::Compile)
    }
}

/// Strip the counters fresh names carry, so drift between two independent
/// compiles neither hides nor fakes a diagnostic difference: tvar numbers
/// (`'_6070` -> `'_N`) and ids after a `#` (`<abstract#12>`, the block
/// scopes `#do123`, `#fn45`, the names a lowering binds, `#seqv4611_1` ->
/// `#seqvN_1`).
fn normalize_diag(s: &str) -> String {
    let b = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let (mut i, mut flushed) = (0, 0);
    while i < b.len() {
        let head = match b[i] {
            b'#' => 1,
            b'\'' if b.get(i + 1) == Some(&b'_') => 2,
            _ => 0,
        };
        if head > 0 {
            let mut j = i + head;
            while b.get(j).is_some_and(u8::is_ascii_lowercase) {
                j += 1;
            }
            if b.get(j).is_some_and(u8::is_ascii_digit) {
                out.push_str(&s[flushed..j]);
                while b.get(j).is_some_and(u8::is_ascii_digit) {
                    j += 1;
                }
                out.push('N');
                (flushed, i) = (j, j);
                continue;
            }
        }
        i += 1;
    }
    out.push_str(&s[flushed..]);
    out
}

impl Outcome {
    /// The trace of a pure synchronous program producing `v` once at
    /// init — offset 0, single epoch. Test convenience.
    pub fn single(v: Value) -> Outcome {
        Outcome::Trace(trace::Trace {
            epochs: vec![trace::Epoch { events: vec![(0, v)], capped: false }],
            stdout: Vec::new(),
            exit: None,
        })
    }

    /// Whether two outcomes are observably equivalent. Traces compare
    /// structurally; different outcome kinds always disagree; two
    /// rejections agree on the same diagnostics (counters aside); other
    /// same-kind outcomes agree without comparing mode-dependent messages.
    pub fn agrees_with(&self, other: &Outcome) -> bool {
        use Outcome::*;
        match (self, other) {
            (Trace(a), Trace(b)) => a.agrees_with(b),
            // both-reject agrees only on identical diagnostics (modulo
            // counter drift); RuntimeErr messages are mode-dependent
            (CompileErr(a), CompileErr(b)) => normalize_diag(a) == normalize_diag(b),
            (RuntimeErr(_), RuntimeErr(_)) => true,
            // a node-walk budget beside a JIT deadline is settled by
            // `Oracle::settle`, which knows the engines
            (Timeout(a), Timeout(b)) => a.agrees_with(*b),
            _ => false,
        }
    }

    fn needs_slow_retry(&self, other: &Self) -> bool {
        use Outcome::Timeout;
        match (self, other) {
            (Timeout(Containment::Compile), Timeout(c)) => *c != Containment::Compile,
            (Timeout(_), Timeout(_)) => false,
            (Timeout(_), _) => true,
            _ => false,
        }
    }

    /// A trace that shows anything: an event or a printed line.
    pub fn has_events(&self) -> bool {
        matches!(self, Outcome::Trace(t)
            if !t.stdout.is_empty() || t.epochs.iter().any(|e| !e.events.is_empty()))
    }

    /// [`Self::agrees_with`] at a [`Strength`].
    pub fn agrees_at(&self, other: &Outcome, s: Strength) -> bool {
        use Outcome::*;
        match (s, self, other) {
            (Strength::Final, Trace(a), Trace(b)) => a.agrees_final(b),
            // a Timeout beside a trace goes through the ladder's slow retry
            (Strength::Kind, Trace(_), Trace(_)) => true,
            _ => self.agrees_with(other),
        }
    }

    /// Coarse variant discriminant, for the "same bug" bucket key.
    pub fn kind(&self) -> u8 {
        match self {
            Outcome::Trace(_) => 0,
            Outcome::CompileErr(_) => 1,
            Outcome::RuntimeErr(_) => 2,
            Outcome::Timeout(_) => 3,
            Outcome::Checked => 4,
        }
    }
}

/// Run `code` (a wrapper: optional headers + expression body, see
/// [`Subject`]) under `mode`, returning the per-cycle trace of
/// everything `result` emitted across every epoch, or why nothing ran.
/// A fresh `ExecState` is created per call so fusion state and the
/// per-context JIT never leak between runs.
pub async fn run_program(code: &str, mode: Mode, timeout: Duration) -> Outcome {
    run_program_routed(code, mode, Route::InLanguage, timeout).await
}

/// [`run_program`] on a chosen dispatch [`Route`] (identical for
/// programs with no `callable-v1` header).
pub async fn run_program_routed(
    code: &str,
    mode: Mode,
    route: Route,
    timeout: Duration,
) -> Outcome {
    run_subject(code, mode, route, SessionImage::None, timeout, RunOpts::default())
        .await
        .outcome
}

/// Everything needed to compile and drive one subject, derived from its
/// source text in one place so the individual and batch paths cannot
/// drift (a CompileErr agrees with a CompileErr, so drift there is
/// invisible to the oracle).
pub struct Subject {
    pub sched: schedule::Schedule,
    pub spec: Option<callable::CallSpec>,
    pub tier: OracleTier,
    /// `mod <stem>;` per aux `.gx` section, for the compile text's top
    /// level.
    pub mods: String,
    /// The module VFS: the wrapped body under `modname`, plus aux files.
    pub table: AHashMap<Path, VfsEntry>,
    /// The module the body is installed as.
    pub modname: String,
}

impl Subject {
    /// `Err` is the message for a `CompileErr` outcome: a malformed
    /// header is a compile-class reject in every mode, so it agrees
    /// everywhere and surfaces in `gen-check`.
    pub fn parse(code: &str, modname: &str) -> Result<Subject, String> {
        let (sched, body) = schedule::Schedule::parse(code)
            .map_err(|e| format!("schedule header: {e}"))?;
        let (spec, body) = callable::CallSpec::parse(&body)
            .map_err(|e| format!("callable header: {e}"))?;
        let (body, files) =
            files::split(&body).map_err(|e| format!("file section: {e}"))?;
        // a submodule sees nothing of its parent implicitly; `use
        // super::*` brings in the aux `mod`s, and the injected inputs and
        // callable declarations live in their own module so a program
        // compiled as one block (the program route) still publishes them
        // by name
        let decls = format!(
            "{}{}",
            sched.decls(),
            spec.as_ref().map(|c| c.decls()).unwrap_or_default()
        );
        let inputs = if decls.is_empty() { "" } else { "use super::inputs::*; " };
        // a body of several statements is one block, its last the result
        let statements =
            parser::parse(Origin::unspecified(body)).map_or(1, |items| items.len());
        let (open, close) = if statements > 1 { ("{", "\n}") } else { ("", "") };
        let wrapped = ArcStr::from(format!(
            "use super::*; {inputs}let result = {open}\n{body}{close}"
        ));
        let mut table = AHashMap::from_iter([(
            Path::from(format!("/{modname}.gx")),
            VfsEntry::from(wrapped),
        )]);
        if !decls.is_empty() {
            let text = ArcStr::from(format!("use super::*;\n{decls}"));
            table.insert(Path::from("/inputs.gx"), VfsEntry::from(text));
        }
        for (name, text) in &files {
            table.insert(
                Path::from(format!("/{name}")),
                VfsEntry::from(ArcStr::from(text.as_str())),
            );
        }
        Ok(Subject {
            sched,
            spec,
            tier: oracle_tier(code),
            mods: files::mod_decls(&files),
            table,
            modname: modname.to_string(),
        })
    }

    /// The text handed to the compiler. The injected inputs and the
    /// callable declarations are the `inputs` module ([`input_scope`]
    /// finds it wherever the text is compiled), declared after the aux
    /// `mod`s the callable driver references into.
    pub fn compile_text(&self) -> String {
        let Subject { sched, spec, mods, modname, .. } = self;
        let inputs = if sched.inputs().is_empty() && spec.is_none() {
            ""
        } else {
            "mod inputs;\n"
        };
        format!("{mods}{inputs}{{ mod {modname}; {modname}::result }}")
    }

    /// [`Self::compile_text`] as the program a session compiles.
    pub fn program(&self) -> graphix_compiler::expr::Load {
        graphix_compiler::expr::Load::Text(ArcStr::from(self.compile_text()))
    }
}

/// Compile `code` under `mode` without driving it: `None` = compiled
/// clean, `Some(error)` = reject (or the runtime failed to init).
/// `gen-check`'s primitive.
pub async fn compile_program(code: &str, mode: Mode) -> Option<String> {
    match compile_with_stats(code, mode, Duration::from_secs(60)).await {
        CompileOutcome::Compiled(_) => None,
        CompileOutcome::Rejected(e, _) | CompileOutcome::Failed(e) => Some(e),
        CompileOutcome::BudgetAborted => {
            Some("stack budget exceeded: the runtime aborted".to_string())
        }
    }
}

/// Compile `code` the way `--check` does under [`graphix_compiler::CFlag::CheckOnly`]: the
/// check alone, no elaboration, never run. `Err` is the refusal, or a
/// runtime that failed to start.
pub async fn check_only(code: &str, timeout: Duration) -> Result<(), String> {
    let subj = Subject::parse(code, "test")?;
    let (tx, _rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    let registration = registration_image_source().await;
    let flags = Mode::Interp.flags();
    let ctx = async {
        TestCtx::start(
            config(tx, REGISTER, vec![resolver], |_| {})?
                .flags(flags)
                .registration(registration),
        )
        .await
    }
    .await
    .map_err(|e| format!("runtime init failed: {e:?}"))?;
    let src = graphix_compiler::expr::Load::Text(ArcStr::from(subj.compile_text()));
    let res = match tokio::time::timeout(timeout, ctx.rt.check(src, None)).await {
        Ok(Ok(_)) => Ok(()),
        Ok(Err(e)) => Err(format!("{e:?}")),
        Err(_) => Err("check timed out".to_string()),
    };
    ctx.shutdown().await;
    res
}

/// A compile-only measurement. `Rejected` is the compiler's verdict on
/// the program, still carrying the stats of whatever did compile;
/// `Failed` is the measurement failing (init, stats read, wedged
/// compile), from which nothing about the program follows.
enum CompileOutcome {
    Compiled(FusionStats),
    Rejected(String, FusionStats),
    /// The first update cycle recursed past the stack budget, which
    /// aborts the runtime before its stats can be read.
    BudgetAborted,
    Failed(String),
}

/// A subject's runtime, started.
struct Started {
    ctx: TestCtx,
    rx: mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    /// The program's print output, kept off the process streams.
    sink: graphix_package_core::PrintSink,
    /// The code the program asked `sys::exit` for.
    exit: oneshot::Receiver<i32>,
}

/// What a session's runtime may hold: its trace's events and cycles, and
/// its live activations where they differ from the process's budget.
#[derive(Debug, Default, Clone, Copy)]
struct Limits {
    trace: Option<(usize, u64)>,
    activations: Option<u64>,
}

/// Start `subj` under `mode` over its VFS, with a print sink, an exit
/// request and the fuzzer's fork policy. The program compiles (or
/// restores) at construction under `timeout`; a construction abandoned
/// at the deadline finishes on its own task and exits, its handle gone.
async fn start_session(
    subj: &Subject,
    mode: Mode,
    registration: RegistrationImage,
    program: Option<graphix_compiler::expr::Load>,
    program_image: Option<oneshot::Sender<anyhow::Result<graphix_rt::ProgramImage>>>,
    limits: Limits,
    timeout: Duration,
) -> Result<Started, Outcome> {
    let (tx, rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    let sink = graphix_package_core::PrintSink::default();
    let (exit_req, exit) = graphix_package_core::ExitRequest::new();
    let seeded = sink.clone();
    let init = async move {
        let mut cfg = config(tx, REGISTER, vec![resolver], move |ctx| {
            ctx.libstate.set(seeded);
            ctx.libstate.set(exit_req);
            ctx.control.set_par_mode(fuzz_par(mode));
            ctx.control.set_prune_mode(mode.prune());
            if let Some(n) = limits.activations {
                ctx.control.set_activation_budget(n);
            }
        })?
        .flags(mode.flags())
        .registration(registration);
        if let Some(p) = program {
            cfg = cfg.program(p);
        }
        if let Some(tx) = program_image {
            cfg = cfg.program_image(tx);
        }
        if let Some(trace) = limits.trace {
            cfg = cfg.trace(trace);
        }
        async { TestCtx::start(cfg).await }.await
    };
    match tokio::time::timeout(timeout, init).await {
        Ok(Ok(ctx)) => Ok(Started { ctx, rx, sink, exit }),
        Ok(Err(e)) => Err(Outcome::RuntimeErr(format!("runtime init failed: {e:?}"))),
        Err(_) => Err(Outcome::Timeout(Containment::Compile)),
    }
}

/// Compile-only core behind [`compile_program`] and [`run_fusecheck`]:
/// compile the full drive text under a fresh ctx and return the
/// program's own compile-time [`FusionStats`] delta. The timeout covers
/// the construction, where the program compiles, and the first cycle's
/// result. A stats value is never synthesized from a failure.
async fn compile_with_stats(code: &str, mode: Mode, timeout: Duration) -> CompileOutcome {
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return CompileOutcome::Rejected(e, FusionStats::default()),
    };
    let registration = registration_image_source().await;
    let program = Some(subj.program());
    let Started { ctx, .. } = match start_session(
        &subj,
        mode,
        registration,
        program,
        None,
        Limits::default(),
        timeout,
    )
    .await
    {
        Ok(s) => s,
        Err(Outcome::Timeout(_)) => {
            return CompileOutcome::Failed("compile timed out".to_string());
        }
        Err(o) => return CompileOutcome::Failed(render(&o)),
    };
    let run = async {
        // Debug format is the anyhow chain; gen-check buckets on the
        // last line, the innermost cause.
        let verdict = program_result(&ctx).await;
        if ctx.rt.budget_aborted() {
            return CompileOutcome::BudgetAborted;
        }
        match ctx.fusion_stats().await {
            Ok(s) => match verdict {
                Ok(_) => CompileOutcome::Compiled(s),
                Err(e) => CompileOutcome::Rejected(format!("{e:?}"), s),
            },
            // the first cycle's abort can land between the check above and
            // the read
            Err(_) if ctx.rt.budget_aborted() => CompileOutcome::BudgetAborted,
            Err(e) => CompileOutcome::Failed(format!("fusion stats read: {e:?}")),
        }
    };
    let res = match tokio::time::timeout(timeout, run).await {
        Ok(r) => r,
        Err(_) => {
            ctx.rt.interrupt();
            CompileOutcome::Failed("compile timed out (wedged evaluator)".to_string())
        }
    };
    let _ = tokio::time::timeout(Duration::from_secs(5), ctx.shutdown()).await;
    res
}

/// The program a runtime compiled or restored at construction, as a
/// compile verdict.
async fn program_result(ctx: &TestCtx) -> anyhow::Result<CompRes<NoExt>> {
    ctx.rt.program().await.and_then(|p| p.ok_or_else(|| anyhow::anyhow!("no program")))
}

/// [`run_program`], also returning the compile-time [`FusionStats`]
/// delta for the program itself (stats accumulate per `ExecState`, so the
/// post-init baseline is subtracted); `None` when the runtime did not
/// answer.
pub async fn run_program_with_stats(
    code: &str,
    mode: Mode,
    timeout: Duration,
) -> (Outcome, Option<FusionStats>) {
    run_program_with_stats_routed(code, mode, Route::InLanguage, timeout).await
}

pub async fn run_program_with_stats_routed(
    code: &str,
    mode: Mode,
    route: Route,
    timeout: Duration,
) -> (Outcome, Option<FusionStats>) {
    let opts = RunOpts { stats: true, ..RunOpts::default() };
    let run = run_subject(code, mode, route, SessionImage::None, timeout, opts).await;
    (run.outcome, run.stats)
}

async fn drive(
    ctx: &TestCtx,
    rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    exit: &mut oneshot::Receiver<i32>,
    subj: &Subject,
    route: Route,
    timeout: Duration,
    sink: &graphix_package_core::PrintSink,
) -> Outcome {
    let mut outcome = drive_inner(ctx, rx, subj, route, timeout, sink).await;
    if let Outcome::Trace(t) = &mut outcome {
        t.exit = exit.try_recv().ok();
    }
    // A runtime the stack budget aborted answers nothing more: whichever
    // request that failed, the outcome is the containment.
    if matches!(outcome, Outcome::RuntimeErr(_) | Outcome::CompileErr(_))
        && ctx.rt.budget_aborted()
    {
        let control = ctx.rt.control();
        return Outcome::Timeout(match control.stopped_by() {
            Some(graphix_compiler::StopCause::Activations) => {
                Containment::ActivationBudget
            }
            _ => Containment::StackBudget,
        });
    }
    outcome
}

/// The longest a settle round waits for the replies in flight.
const REPLY_WAIT: Duration = Duration::from_secs(10);

async fn drive_inner(
    ctx: &TestCtx,
    rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    subj: &Subject,
    route: Route,
    timeout: Duration,
    sink: &graphix_package_core::PrintSink,
) -> Outcome {
    let Subject { sched, spec, tier, .. } = subj;
    let (spec, tier) = (spec.as_ref(), *tier);
    // one wall-clock deadline for the whole drive (a backstop for a
    // wedged evaluator) and one concurrent drain of the event channel
    let deadline = tokio::time::sleep(timeout);
    tokio::pin!(deadline);
    let drain = async {
        while rx.recv().await.is_some() {}
        future::pending::<()>().await
    };
    tokio::pin!(drain);
    // A deadline breach is `Timeout` on every step. The interrupt stops
    // the runtime, so a step that completes after it cannot be trusted;
    // reclassifying is a race. `check()` retries one-sided timeouts at a
    // bigger budget.
    macro_rules! step_or_timeout {
        ($fut:expr, $on_ok:pat => $ok:expr, $on_err:pat => $err:expr) => {{
            let f = $fut;
            tokio::pin!(f);
            tokio::select! {
                biased;
                r = &mut f => match r { $on_ok => $ok, $on_err => $err },
                _ = &mut drain => unreachable!(),
                _ = &mut deadline => {
                    ctx.rt.interrupt();
                    let _ =
                        tokio::time::timeout(Duration::from_millis(750), &mut f).await;
                    return Outcome::Timeout(Containment::Deadline);
                }
            }
        }};
    }
    // One epoch's segment, settled. `trace_wait_idle` sees an in-flight
    // async IO task as idle, so the FinalValues tier waits out the replies
    // in flight (up to REPLY_WAIT: a program may keep one in flight for
    // ever) and re-waits after a grace sleep, for what timers and watches
    // start, until a round is quiet; Exact-tier programs have no in-flight
    // IO and skip the settle.
    macro_rules! wait_settled {
        () => {{
            let mut seg = step_or_timeout!(
                ctx.rt.trace_wait_idle(),
                Ok(s) => s,
                Err(e) => return Outcome::RuntimeErr(format!("trace_wait_idle: {e}"))
            );
            if tier == OracleTier::FinalValues {
                for _ in 0..8 {
                    step_or_timeout!(
                        tokio::time::timeout(REPLY_WAIT, ctx.rt.wait_replies()),
                        Ok(Err(e)) => return Outcome::RuntimeErr(format!("wait_replies: {e}")),
                        _ => ()
                    );
                    tokio::time::sleep(Duration::from_millis(150)).await;
                    let more = step_or_timeout!(
                        ctx.rt.trace_wait_idle(),
                        Ok(s) => s,
                        Err(e) => {
                            return Outcome::RuntimeErr(format!("trace_wait_idle: {e}"))
                        }
                    );
                    seg.end_cycle = more.end_cycle;
                    if more.events.is_empty() {
                        break;
                    }
                    seg.events.extend(more.events.iter().cloned());
                    seg.capped_cycles |= more.capped_cycles;
                    seg.capped_events |= more.capped_events;
                }
            }
            seg
        }};
    }
    // Tracing was armed at the runtime's construction, before the
    // compile (ToGX messages are FIFO), so a value emitted during the
    // compile cycle is in the trace.
    let compiled = step_or_timeout!(
        program_result(ctx),
        Ok(c) => c,
        Err(e) => return Outcome::CompileErr(format!("{e:?}"))
    );
    // dropping it deletes the program's node
    let compiled = &compiled;
    let eid = compiled.exprs.last().expect("compile returned no exprs").id;
    let mut segs = Vec::with_capacity(1 + sched.epochs.len());
    segs.push(wait_settled!());
    // Every input's ref is created up front and each epoch's injections
    // go through one `set_many`: separate `set` calls can land in
    // different cycles depending on scheduler timing.
    let mut refs: AHashMap<&str, graphix_rt::Ref<NoExt>> = AHashMap::new();
    for (name, _) in sched.inputs() {
        let scope = input_scope(&compiled.env, &name);
        let path = graphix_compiler::expr::ModPath::from([name.as_str()]);
        let r = step_or_timeout!(
            ctx.rt.compile_ref_by_name(&compiled.env, &scope, &path),
            Ok(r) => r,
            Err(e) => return Outcome::RuntimeErr(format!("input {name}: {e}"))
        );
        // keys borrow from `sched`, alive for the whole drive
        let key = sched
            .epochs
            .iter()
            .flat_map(|ep| ep.iter())
            .map(|(n, _)| n.as_str())
            .find(|n| *n == name)
            .expect("inputs() names come from the epochs");
        refs.insert(key, r);
    }
    for ep in &sched.epochs {
        let sets: Vec<(graphix_compiler::BindId, Value)> =
            ep.iter().map(|(name, v)| (refs[name.as_str()].bid, v.value())).collect();
        if let Err(e) = ctx.rt.set_many(sets) {
            return Outcome::RuntimeErr(format!("set_many: {e}"));
        }
        segs.push(wait_settled!());
    }
    // Both routes append one traced epoch per dispatch after the
    // schedule's own, so the routes' traces align epoch-for-epoch.
    if let Some(c) = spec {
        match route {
            Route::InLanguage => {
                let mut arefs: AHashMap<String, graphix_rt::Ref<NoExt>> = AHashMap::new();
                for (name, _) in c.args() {
                    let scope = input_scope(&compiled.env, &name);
                    let path = graphix_compiler::expr::ModPath::from([name.as_str()]);
                    let r = step_or_timeout!(
                        ctx.rt.compile_ref_by_name(&compiled.env, &scope, &path),
                        Ok(r) => r,
                        Err(e) => {
                            return Outcome::RuntimeErr(format!(
                                "callable arg {name}: {e}"
                            ));
                        }
                    );
                    arefs.insert(name, r);
                }
                for ep in &c.epochs {
                    let sets: Vec<(graphix_compiler::BindId, Value)> = ep
                        .iter()
                        .map(|(name, v)| (arefs[name.as_str()].bid, v.value()))
                        .collect();
                    if let Err(e) = ctx.rt.set_many(sets) {
                        return Outcome::RuntimeErr(format!("set_many: {e}"));
                    }
                    segs.push(wait_settled!());
                }
            }
            Route::Dispatch => {
                let scope = program_scope(&compiled.env);
                let path = graphix_compiler::expr::ModPath::from(c.handler.split("::"));
                let r = step_or_timeout!(
                    ctx.rt.compile_ref_by_name(&compiled.env, &scope, &path),
                    Ok(r) => r,
                    Err(e) => {
                        return Outcome::RuntimeErr(format!(
                            "handler {}: {e}",
                            c.handler
                        ));
                    }
                );
                let lambda = match r.last.clone() {
                    Some(v) => v,
                    None => {
                        return Outcome::RuntimeErr(format!(
                            "handler {} has no value",
                            c.handler
                        ));
                    }
                };
                let cb = step_or_timeout!(
                    ctx.rt.compile_callable(lambda),
                    Ok(cb) => cb,
                    Err(e) => {
                        return Outcome::RuntimeErr(format!("compile_callable: {e}"));
                    }
                );
                // The embedder timeline has cycles between building the
                // callable and the first dispatch; back-to-back dispatch
                // would let a reference delivered at init reach the instance.
                // Their segment is set aside, so the first dispatch epoch
                // anchors at its own dispatch as the later ones do.
                for _ in 0..3 {
                    step_or_timeout!(
                        ctx.rt.compile(ArcStr::from("i64:0")),
                        Ok(_) => (),
                        Err(e) => {
                            return Outcome::RuntimeErr(format!("gap compile: {e}"));
                        }
                    );
                }
                let _ = wait_settled!();
                for ep in &c.epochs {
                    let args =
                        ValArray::from_iter_exact(ep.iter().map(|(_, v)| v.value()));
                    step_or_timeout!(
                        cb.call(args),
                        Ok(()) => (),
                        Err(e) => {
                            return Outcome::RuntimeErr(format!("dispatch: {e}"));
                        }
                    );
                    segs.push(wait_settled!());
                }
            }
        }
    }
    let mut trace = trace::Trace::from_segments(&segs, eid);
    // Every tier that compares (FinalValues as a per-epoch set). The
    // runtime runs on past a capped trace, so the capture stops at the last
    // segment's cycle.
    if tier != OracleTier::Excluded {
        let mut lines = Vec::new();
        for (epoch, seg) in segs.iter().enumerate() {
            let cycles = sink.take_through(seg.end_cycle);
            let Some(base) =
                trace::Epoch::anchor(seg).or(cycles.first().map(|(c, _)| *c))
            else {
                continue;
            };
            for (cycle, text) in cycles {
                lines.push(trace::Printed {
                    epoch: epoch as u32,
                    offset: cycle as i64 - base as i64,
                    text,
                });
            }
        }
        lines.sort_unstable();
        trace.stdout = lines;
    }
    Outcome::Trace(trace)
}

/// The reserved metamorphic-twin poison tag: a generated twin program
/// settles an epoch on a `` `TwinDiverged(..) `` value when its
/// equivalent write routes disagree. Scanned on each epoch's final
/// value, through composites. No other program may produce it.
pub const TWIN_TAG: &str = "TwinDiverged";

fn value_has_tag(v: &Value, tag: &str) -> bool {
    let mut stack: LPooled<Vec<Value>> = LPooled::take();
    stack.push(v.clone());
    while let Some(v) = stack.pop() {
        match v {
            Value::String(s) if &*s == tag => return true,
            Value::Array(a) => stack.extend(a.iter().cloned()),
            Value::Error(e) => stack.push((*e).clone()),
            Value::Map(m) => {
                for (k, v) in m.into_iter() {
                    stack.push(k.clone());
                    stack.push(v.clone());
                }
            }
            _ => (),
        }
    }
    false
}

/// Scan an outcome for a settled twin violation.
fn twin_violation(o: &Outcome) -> bool {
    match o {
        Outcome::Trace(t) => {
            t.final_values().iter().any(|v| v.is_some_and(|v| value_has_tag(v, TWIN_TAG)))
        }
        _ => false,
    }
}

/// Which comparison strength a program's oracle runs at, decided from
/// its text so every protocol recomputes the same tier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OracleTier {
    /// Pure/deterministic programs: exact per-cycle trace agreement.
    Exact,
    /// Value-deterministic async: IO pacing races trace quiescence, but
    /// each epoch's settled value is deterministic, so per-epoch finals
    /// are compared.
    FinalValues,
    /// Value-nondeterministic (rand, wall time, tempdir paths): no value
    /// comparison is sound. The shapes still run for crash coverage.
    Excluded,
}

/// How strongly a comparison holds two outcomes to each other.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Strength {
    /// Whole traces: every epoch's offsets and values, and the output.
    Exact,
    /// Each epoch's settled value.
    Final,
    /// Only what does not depend on values: two traces, compatible
    /// containments, or matching rejections.
    Kind,
}

impl Strength {
    /// The program's tier on its own route; settled values where the
    /// dispatch route's cycle offsets enter, which are not contractual;
    /// the outcome's kind for a program whose values are not comparable.
    pub fn of(pair: Pair, tier: OracleTier) -> Self {
        match tier {
            OracleTier::Excluded => Strength::Kind,
            OracleTier::FinalValues => Strength::Final,
            // the routes pace a program differently
            OracleTier::Exact if pair == Pair::Route => Strength::Final,
            OracleTier::Exact => Strength::Exact,
        }
    }
}

/// Whether `code` names `marker` as a whole name: not as the tail of a
/// longer identifier (`error(` does not name `or(`); a `::member` marker
/// is a path's member wherever it stands.
fn mentions(code: &str, marker: &str) -> bool {
    code.match_indices(marker).any(|(i, _)| {
        marker.starts_with("::")
            || !code[..i]
                .chars()
                .next_back()
                .is_some_and(|c| c.is_alphanumeric() || c == '_')
    })
}

/// `code`'s code lines, string literals blanked: a comment or a string
/// that merely names an API is no use of it. Trailing comments on code
/// lines stay (over-matching is the safe direction).
fn code_only(code: &str) -> String {
    let mut stripped = String::with_capacity(code.len());
    for l in code.lines() {
        if !l.trim_start().starts_with("//") {
            stripped.push_str(l);
            stripped.push('\n');
        }
    }
    without_strings(&stripped)
}

/// `code` with every string literal's text blanked, so a marker in text
/// is not a call; what a string interpolates is code and stays. The
/// contexts are the parser's (`grow.rs::unclosed_at_end`).
fn without_strings(code: &str) -> String {
    enum Ctx {
        Str,
        Raw(usize),
        Template,
        Interp,
        Bracket,
    }
    let b = code.as_bytes();
    let mut out = b.to_vec();
    let mut stack: Vec<Ctx> = Vec::new();
    let mut i = 0;
    while i < b.len() {
        let ahead = |k: usize| b.get(i + k).copied();
        let mut advance = 1;
        let text = match stack.last() {
            Some(Ctx::Str) => match b[i] {
                b'\\' => {
                    advance = 2;
                    true
                }
                b'[' => {
                    stack.push(Ctx::Interp);
                    false
                }
                b'"' => {
                    stack.pop();
                    false
                }
                _ => true,
            },
            Some(Ctx::Raw(n)) => {
                let n = *n;
                if b[i] == b'"' && (1..=n).all(|k| ahead(k) == Some(b'#')) {
                    stack.pop();
                    advance = 1 + n;
                    false
                } else {
                    true
                }
            }
            Some(Ctx::Template) => match b[i] {
                b'\\' if ahead(1) == Some(b'[') => {
                    stack.push(Ctx::Interp);
                    advance = 2;
                    false
                }
                b'"' if ahead(1) == Some(b'"') && ahead(2) == Some(b'"') => {
                    stack.pop();
                    advance = 3;
                    false
                }
                _ => true,
            },
            Some(Ctx::Interp | Ctx::Bracket) | None => {
                match b[i] {
                    b'"' if ahead(1) == Some(b'"') && ahead(2) == Some(b'"') => {
                        stack.push(Ctx::Template);
                        advance = 3;
                    }
                    b'"' => stack.push(Ctx::Str),
                    b'r' if matches!(ahead(1), Some(b'"' | b'#')) => {
                        let n = b[i + 1..].iter().take_while(|&&c| c == b'#').count();
                        if ahead(1 + n) == Some(b'"') {
                            stack.push(Ctx::Raw(n));
                            advance = 2 + n;
                        }
                    }
                    b'[' => stack.push(Ctx::Bracket),
                    b']' => {
                        stack.pop();
                    }
                    _ => (),
                }
                false
            }
        };
        let end = (i + advance).min(b.len());
        if text {
            out[i..end].fill(b' ');
        }
        i = end;
    }
    String::from_utf8(out).expect("ASCII replaced ASCII and whole UTF-8 runs")
}

/// A program's code lines with the pure time arithmetic, which reads no
/// clock, renamed out of `sys::time`.
fn clock_free(code: &str) -> String {
    let mut code = code_only(code);
    for f in ["add_dur", "sub_dur", "add", "sub", "diff", "scale"] {
        code = code.replace(&format!("sys::time::{f}("), "pure_time(");
    }
    code
}

/// Whether a hang of `prog` may be the environment's: it reads the
/// clock, the network, a process or randomness.
fn environmental_hang(prog: &str) -> bool {
    let code = clock_free(prog);
    ["rand::", "sys::time", "sys::net", "sys::process", "http::"]
        .iter()
        .any(|m| mentions(&code, m))
}

/// Classify a program. Markers are matched on code lines only. The
/// Excluded list is empirical; selfcheck polices it — a marker missing
/// from it shows up as a final-strength flake.
pub fn oracle_tier(code: &str) -> OracleTier {
    let code = clock_free(code);
    let code = code.as_str();
    // Value-nondeterministic sources: random values, wall-clock time
    // (`throttle` reads it outside sys::), a create-new open racing the
    // sibling engine in the shared sandbox, generated temp paths, netidx
    // registration timing, OS-assigned ports, and child processes: a
    // child's lifetime is program data (`sleep 0.2; exit 3`), so the
    // epoch its exit or output lands in is the wall clock's, not the
    // settle grace's.
    let excluded = [
        "rand::",
        "throttle",
        "`CreateNew",
        "sys::time",
        "sys::net",
        "sys::process::spawn",
        "::tempdir",
        "tempdir::",
        "::listener_addr",
        "::local_addr",
        "::peer_addr",
    ];
    if excluded.iter().any(|m| mentions(code, m)) {
        return OracleTier::Excluded;
    }
    // Functions have no order, and under `Any` a closure orders by the id
    // its compile minted, which a forked compile mints in any order: a
    // program that orders `Any` values beside a lambda is nondeterministic
    let orders = ["Map<Any", "sort", "dedup", "min(", "max("];
    if mentions(code, "Any")
        && code.contains('|')
        && orders.iter().any(|m| code.contains(m))
    {
        return OracleTier::Excluded;
    }
    // An fs mutation racing an fs observation: nothing orders two async
    // IO builtins unless the program threads a data edge between them.
    // One-sided fs programs keep their value comparison.
    let fs_mutators = [
        "sys::fs::write_all",
        "sys::fs::create_dir",
        "sys::fs::remove_dir",
        "sys::fs::remove_file",
    ];
    let fs_observers = [
        "sys::fs::read_all",
        "sys::fs::readdir",
        "sys::fs::metadata",
        "sys::fs::is_file",
        "sys::fs::is_dir",
        "sys::fs::watch",
    ];
    if fs_mutators.iter().any(|m| mentions(code, m))
        && fs_observers.iter().any(|m| mentions(code, m))
    {
        return OracleTier::Excluded;
    }
    // the async readers land their reply in whichever cycle the scheduler
    // allows, like sys:: and http::
    let async_markers = [
        "sys::",
        "http::",
        "json::read",
        "toml::read",
        "pack::read",
        "xls::",
        "db::",
        "sqlite::",
    ];
    if async_markers.iter().any(|m| mentions(code, m)) {
        // FinalValues assumes the async values themselves settle
        // deterministically; a `<-` weaves arrival order into state, and
        // the fire-count/arrival-order-sensitive builtins leak it too.
        // `iter(` is a self-clocked sequence: one element per cycle,
        // so anything async landing beside it samples whichever element
        // that cycle held.
        let fire_count_sensitive = [
            "count(", "sum(", "product(", "mean(", "min(", "max(", "all(", "and(", "or(",
            "any(", "queue(", "queuefn(", "take(", "skip(", "window(", "iterq", "iter(",
            "group(", "hold(",
        ];
        if code.contains("<-") || fire_count_sensitive.iter().any(|m| mentions(code, m)) {
            return OracleTier::Excluded;
        }
        return OracleTier::FinalValues;
    }
    OracleTier::Exact
}

/// How a subject's session reaches the runtime, on the program route
/// (`GXConfig::program`, the shell's script path): compiled with no
/// image machinery, compiled cold and written to a program image, or
/// restored warm from the image the cold run wrote.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Session {
    NoCache,
    Cold,
    Warm,
}

impl Session {
    pub fn name(self) -> &'static str {
        match self {
            Session::NoCache => "nocache",
            Session::Cold => "cold",
            Session::Warm => "warm",
        }
    }
}

/// `GRAPHIX_FUZZ_SESSIONS`: `0` disables the session runs; `N` runs
/// them on every Nth batched subject (default 1, every subject). The
/// individual path runs them whenever they are enabled.
fn sessions_every() -> usize {
    std::env::var("GRAPHIX_FUZZ_SESSIONS").ok().and_then(|v| v.parse().ok()).unwrap_or(1)
}

fn sessions_enabled() -> bool {
    sessions_every() > 0
}

fn sessions_sampled(i: usize) -> bool {
    let n = sessions_every();
    n > 0 && i % n == 0
}

/// The registration image every session run restores, built once per
/// process by a throwaway runtime; `None` when the image cannot be
/// built, in which case every session compiles its registration.
async fn registration_image() -> Option<Bytes> {
    static IMAGE: tokio::sync::OnceCell<Option<Bytes>> =
        tokio::sync::OnceCell::const_new();
    IMAGE
        .get_or_init(|| async {
            let (tx, _rx) = mpsc::channel(8);
            let (reg_tx, reg_rx) = oneshot::channel();
            let ctx = async {
                TestCtx::start(
                    config(tx, REGISTER, vec![], |_| {})?
                        .registration(RegistrationImage::save(reg_tx)),
                )
                .await
            }
            .await
            .ok()?;
            let image = reg_rx.await.ok().and_then(|r| r.ok());
            let _ = tokio::time::timeout(Duration::from_secs(2), ctx.shutdown()).await;
            image
        })
        .await
        .clone()
}

/// Every runtime restores the registration image built once per
/// process; without one it compiles its registration.
async fn registration_image_source() -> RegistrationImage {
    match registration_image().await {
        Some(bytes) => RegistrationImage::load(bytes),
        None => RegistrationImage::save(oneshot::channel().0),
    }
}

/// What a run does about the program image.
enum SessionImage {
    None,
    Write,
    Read(Bytes),
}

/// One run of a subject.
struct SubjectRun {
    outcome: Outcome,
    /// The program image a `Write` produced.
    image: Result<Bytes, String>,
    /// The program's own fusion stats: the registration compiles with
    /// fusion off (or is restored), so nothing else contributes. `None`
    /// when the runtime did not answer.
    stats: Option<FusionStats>,
    /// It timed out inside one cycle: none started from the middle of its
    /// budget to its last twentieth ([`RunOpts::watch_progress`]).
    stalled: bool,
}

fn no_image() -> Result<Bytes, String> {
    Err("no program image was written".to_string())
}

impl SubjectRun {
    fn failed(outcome: Outcome) -> Self {
        SubjectRun { outcome, image: no_image(), stats: None, stalled: false }
    }
}

/// Run `code` under `mode` on the program route (`GXConfig::program`,
/// the shell's script path): the subject's module table behind a
/// resolver, the registration image restored, the program compiled or
/// restored at the runtime's construction, then driven. With `stats`,
/// the run also asks the runtime for its fusion stats.
async fn run_subject(
    code: &str,
    mode: Mode,
    route: Route,
    image: SessionImage,
    timeout: Duration,
    opts: RunOpts,
) -> SubjectRun {
    let RunOpts { stats, activations, watch_progress } = opts;
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return SubjectRun::failed(Outcome::CompileErr(e)),
    };
    let program = Some(subj.program());
    let (registration, program, program_image, image_rx) = match image {
        SessionImage::Read(bytes) => (RegistrationImage::load(bytes), None, None, None),
        SessionImage::Write => {
            let (itx, irx) = oneshot::channel();
            (registration_image_source().await, program, Some(itx), Some(irx))
        }
        SessionImage::None => (registration_image_source().await, program, None, None),
    };
    let limits = Limits {
        trace: Some((subj.sched.max_events, subj.sched.max_cycles)),
        activations,
    };
    let Started { ctx, mut rx, sink, mut exit } = match start_session(
        &subj,
        mode,
        registration,
        program,
        program_image,
        limits,
        timeout,
    )
    .await
    {
        Ok(s) => s,
        Err(o) => return SubjectRun::failed(o),
    };
    let drive = drive(&ctx, &mut rx, &mut exit, &subj, route, timeout, &sink);
    let (outcome, still) = match watch_progress {
        false => (drive.await, false),
        true => {
            // sampled before the deadline: its interrupt lets the runtime
            // start cycles again
            let mut still = false;
            let watch = async {
                tokio::time::sleep(timeout / 2).await;
                let half = ctx.rt.control().cycles();
                tokio::time::sleep(timeout / 2 - timeout / 20).await;
                still = half == ctx.rt.control().cycles();
                future::pending::<()>().await
            };
            let o = tokio::select! {
                o = drive => o,
                _ = watch => unreachable!(),
            };
            (o, still)
        }
    };
    let stalled = still && matches!(outcome, Outcome::Timeout(Containment::Deadline));
    // A wedged runtime never answers another request: abort first, then
    // never await it without a deadline.
    if matches!(outcome, Outcome::Timeout(_)) {
        ctx.rt.abort();
    }
    let grace = Duration::from_secs(2);
    let image = match image_rx {
        None => no_image(),
        Some(irx) => match tokio::time::timeout(grace, irx).await {
            Ok(Ok(Ok(p))) => Ok(p.image),
            Ok(Ok(Err(e))) => Err(format!("the program image write failed: {e:#}")),
            Ok(Err(_)) | Err(_) => no_image(),
        },
    };
    let stats = match outcome {
        _ if !stats => None,
        Outcome::Timeout(_) => None,
        _ => tokio::time::timeout(timeout, ctx.fusion_stats())
            .await
            .ok()
            .and_then(Result::ok),
    };
    let _ = tokio::time::timeout(grace, ctx.shutdown()).await;
    SubjectRun { outcome, image, stats, stalled }
}

/// What a [`run_subject`] measures besides its outcome.
#[derive(Debug, Default, Clone, Copy)]
struct RunOpts {
    /// Ask the runtime for the program's fusion stats.
    stats: bool,
    /// The live activations the runtime may hold, where they differ from
    /// the process's budget.
    activations: Option<u64>,
    /// Note whether a run that timed out started no cycle over the second
    /// half of its budget, its last twentieth aside ([`SubjectRun::stalled`]).
    watch_progress: bool,
}

/// [`run_subject`] for the image axis: the outcome and the image a
/// `Write` produced.
async fn run_session(
    code: &str,
    mode: Mode,
    route: Route,
    image: SessionImage,
    timeout: Duration,
    opts: RunOpts,
) -> (Outcome, Result<Bytes, String>) {
    let run = run_subject(code, mode, route, image, timeout, opts).await;
    (run.outcome, run.image)
}

/// `code` under `mode` cold (writing the program image) and warm
/// (restored from it). A program that does not compile, or times out
/// before writing its image, has nothing to restore, so its warm outcome
/// is its cold one; a cold run that compiled but wrote no image makes the
/// warm outcome the write's failure, which the cold/warm pair reports.
pub async fn cold_warm(
    code: &str,
    mode: Mode,
    route: Route,
    timeout: Duration,
) -> (Outcome, Outcome) {
    cold_warm_with(code, mode, route, timeout, RunOpts::default()).await
}

async fn cold_warm_with(
    code: &str,
    mode: Mode,
    route: Route,
    timeout: Duration,
    opts: RunOpts,
) -> (Outcome, Outcome) {
    let (cold, image) =
        run_session(code, mode, route, SessionImage::Write, timeout, opts).await;
    let warm = match restorable(&cold, image) {
        Ok(image) => {
            let image = SessionImage::Read(image);
            run_session(code, mode, route, image, timeout, opts).await.0
        }
        Err(warm) => warm,
    };
    (cold, warm)
}

/// The image a cold run leaves its warm run, else the warm outcome. The
/// runtime writes the image at its construction, so only a program whose
/// compile failed or outlived the deadline has none to restore; any other
/// run's missing image is the write's failure.
fn restorable(cold: &Outcome, image: Result<Bytes, String>) -> Result<Bytes, Outcome> {
    match (cold, image) {
        (Outcome::CompileErr(_) | Outcome::Timeout(Containment::Compile), _) => {
            Err(cold.clone())
        }
        (_, Ok(image)) => Ok(image),
        (_, Err(e)) => Err(Outcome::RuntimeErr(e)),
    }
}

/// The routes a subject's sessions run on.
pub fn session_routes(code: &str) -> &'static [Route] {
    if callable::has_header(code) {
        &[Route::InLanguage, Route::Dispatch]
    } else {
        &[Route::InLanguage]
    }
}

/// The scope of the subject's `inputs` module: under the program's own
/// `do` block (a restored image keeps the block's name from the cold
/// run).
fn input_scope(env: &Env, name: &str) -> Scope {
    let mut scope = Scope::root();
    for (path, names) in &env.lexical.binds {
        if Path::basename(&path.0) == Some("inputs") && names.get(name).is_some() {
            scope.lexical = path.clone();
            break;
        }
    }
    scope
}

/// The scope the subject's top level compiled in: the `inputs` module's
/// parent, where the aux modules (a callable's handler among them) are.
fn program_scope(env: &Env) -> Scope {
    let mut scope = Scope::root();
    for (path, _) in &env.lexical.binds {
        if Path::basename(&path.0) == Some("inputs") {
            let dir = Path::dirname(&path.0).unwrap_or("/");
            scope.lexical = graphix_compiler::expr::ModPath::from(
                dir.split('/').filter(|s| !s.is_empty()),
            );
            break;
        }
    }
    scope
}

/// A detected disagreement between the two runs `pair` compares; their
/// names are [`Divergence::labels`].
#[derive(Debug, Clone)]
pub struct Divergence {
    pub code: String,
    /// The run the other is judged against: the node-walk, the uncached
    /// session, the cold one, the check, the serial run, the in-language
    /// route.
    pub reference: Outcome,
    /// The run under test.
    pub tested: Outcome,
    pub tier: OracleTier,
    pub pair: Pair,
}

/// Which two runs a [`Divergence`] compares. `Engine` is the classic
/// node-walk-vs-JIT check; `EngineDispatch` and `Route` exist only for
/// `callable-v1` programs (see [`callable`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pair {
    /// node-walk (`reference`) vs fused+JIT (`tested`), in-language
    /// route.
    Engine,
    /// node-walk vs fused+JIT, embedder-dispatch route.
    EngineDispatch,
    /// in-language route (`reference`) vs embedder-dispatch route
    /// (`tested`), node-walk engine. Compared at final-values strength — route pacing is
    /// not contractual, the per-epoch settled value is.
    Route,
    /// A metamorphic twin program violated its own invariant. Detected
    /// within a single run (`reference` and `tested` both hold it), so
    /// it catches bugs that break every engine and route identically.
    Twin,
    /// No cache (`reference`) vs cold (`tested`): writing the program
    /// image changed the program.
    Cold(Mode, Route),
    /// Cold (`reference`) vs warm (`tested`): restoring the program
    /// image changed the program.
    Warm(Mode, Route),
    /// A corpus pin both engines reject (`reference` and `tested` both
    /// hold the rejection) without saying `// expect: reject`: a rejection agrees
    /// with itself, so a pin that stopped compiling would otherwise pass.
    Rejected,
    /// A corpus pin marked `// expect: reject` that both engines accept
    /// (`reference` and `tested` both hold the node-walk's outcome).
    Accepted,
    /// The check alone accepted (`reference` holds [`Outcome::Checked`])
    /// a program both engines' builds refused (`tested` holds the refusal):
    /// elaboration refused what the check accepted, a type-system bug.
    Check,
    /// Serial node-walk (`reference`) vs a forked run (`tested`): the
    /// node-walk with every fork forced (`Mode::Par`), or the fused JIT,
    /// kernel loops included, at `Auto` over [`AUTO_T`], exercising its
    /// probes and fork feedback (`Mode::JitPar`).
    Par(Mode),
    /// The node-walk walking every boundary (`reference`) vs the pruned
    /// node-walk (`tested`): skipping a boundary changed the program
    /// (`design/walk_pruning.md`).
    Prune,
    /// A corpus pin that no longer parses (`reference` and `tested` both
    /// hold the parse error): it pins nothing.
    Unparsed,
    /// A run whose construction, the program's compile and check, did not
    /// finish at the slow budget either (`reference` and `tested` both hold
    /// it): the checker or the compiler hung or went superlinear.
    Construction,
    /// A run of an `Exact`-tier program (`reference`) vs its rerun in the
    /// same engine (`tested`): a program with no source of nondeterminism
    /// answered twice differently, a determinism bug in the engine or a
    /// normalization gap in the oracle.
    Rerun(Mode),
}

/// The label of one session run, for a finding's outcome fields.
fn session_label(mode: Mode, route: Route, session: Session) -> CompactString {
    let route = match route {
        Route::InLanguage => "",
        Route::Dispatch => "/dispatch",
    };
    format_compact!("{}{route}/{}", mode_label(mode), session.name())
}

fn mode_label(mode: Mode) -> &'static str {
    match mode {
        Mode::Interp => "interp",
        Mode::Jit => "jit",
        Mode::Par => "interp/par",
        Mode::JitPar => "jit/par",
        Mode::Unpruned => "interp/unpruned",
    }
}

impl Divergence {
    /// A one-line classification.
    pub fn bisect(&self) -> &'static str {
        let engines = matches!(self.pair, Pair::Engine | Pair::EngineDispatch);
        if engines
            && matches!(self.reference, Outcome::Timeout(_))
            && self.tested.has_events()
        {
            return "asymmetric timeout (the node-walk did not finish at 8x the budget, \
                    wedged near 0% CPU or looping inside one cycle, while the JIT produced a \
                    value: a node-walk hang, or a JIT that skipped work the node-walk does)";
        }
        match (self.pair, self.tier) {
            (Pair::Twin, _) => {
                "twin invariant violated (equivalent write routes diverged \
                     in-program — a single-run finding, no cross-run comparison)"
            }
            (Pair::Rejected, _) => {
                "corpus pin rejected by both engines without `// expect: reject` \
                     (it no longer compiles or runs, so it pins nothing)"
            }
            (Pair::Accepted, _) => {
                "corpus pin marked `// expect: reject` that both engines accept \
                     (the refusal it pins regressed)"
            }
            (Pair::Check, _) => {
                "the check accepted what the build refused (elaboration refused \
                     a program the definition and call-site checks passed: a \
                     type-system bug)"
            }
            (Pair::Prune, _) => {
                "walk pruning bug (pruned node-walk != unpruned node-walk)"
            }
            (Pair::Unparsed, _) => {
                "corpus pin does not parse (whatever it was refused for once, it pins \
                     nothing now)"
            }
            (Pair::Construction, _) => {
                "the compile or check did not finish at the slow budget (a hang or a \
                     superlinear checker: the typechecker must stay instant)"
            }
            (Pair::Rerun(_), _) => {
                "nondeterminism: a deterministic program answered differently on a \
                     rerun in the same engine (an engine bug or an oracle normalization gap)"
            }
            (Pair::Par(Mode::Par), _) => {
                "parallel evaluation bug (forked node-walk != serial node-walk)"
            }
            (Pair::Par(_), _) => {
                "parallel evaluation bug (forked fused/JIT != serial node-walk)"
            }
            (Pair::Route, _) => {
                "route bug (in-language call != embedder-callable dispatch, interp)"
            }
            (Pair::EngineDispatch, OracleTier::FinalValues) => {
                "fusion/JIT bug on the dispatch route (final values, interp != jit)"
            }
            (Pair::EngineDispatch, _) => {
                "fusion/JIT bug on the dispatch route (interp != jit)"
            }
            (Pair::Engine, OracleTier::FinalValues) => {
                "fusion/JIT bug (final values, interp != jit)"
            }
            (Pair::Engine, _) => "fusion/JIT bug (interp != jit)",
            (Pair::Cold(Mode::Interp | Mode::Par | Mode::Unpruned, _), _) => {
                "image bug: writing the program image changed the program (interp)"
            }
            (Pair::Cold(Mode::Jit | Mode::JitPar, _), _) => {
                "image bug: writing the program image changed the program (jit)"
            }
            (Pair::Warm(Mode::Interp | Mode::Par | Mode::Unpruned, _), _) => {
                "image bug: the restored program differs from the cold one (interp)"
            }
            (Pair::Warm(Mode::Jit | Mode::JitPar, _), _) => {
                "image bug: the restored program differs from the cold one (jit)"
            }
        }
    }
}

/// Run `code` under interp (node-walk) and jit (fusion + cranelift); if
/// they disagree AT THE PROGRAM'S ORACLE TIER, return the `Divergence`.
/// `None` means they agree (or the program is tier-Excluded — it still
/// ran, for shape exercise and crash coverage, but no value comparison
/// is sound for it).
pub async fn check(code: &str, timeout: Duration) -> Option<Divergence> {
    check_classified(code, timeout).await.0
}

/// [`check`] plus the ring-admission classification: `true` iff the
/// programs AGREED with both outcomes being runtime traces — the bar
/// for using an agreeing mutant as a mutation ancestor (a
/// CompileErr/Timeout agreement is a fine oracle subject but a bad
/// seed, and a nondeterminism-cleared agreement is worse). Callable
/// programs stay out of the ring.
pub async fn check_classified(
    code: &str,
    timeout: Duration,
) -> (Option<Divergence>, bool) {
    let (d, v) = check_verdict(code, timeout).await;
    (d, v == Verdict::Ran && !callable::has_header(code))
}

/// How a check that found no divergence concluded: both engines ran to
/// traces, both were contained, both rejected, the oracle excludes the
/// program, or it agreed some other way (a retry, a slowness or
/// nondeterminism drop) that says nothing stable about the program.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    Ran,
    Contained,
    Rejected,
    Excluded,
    Unsure,
}

impl Verdict {
    fn of(a: &Outcome, b: &Outcome) -> Self {
        use Outcome::*;
        match (a, b) {
            (Trace(_), Trace(_)) => Verdict::Ran,
            (Timeout(a), Timeout(b)) if a.agrees_with(*b) => Verdict::Contained,
            (CompileErr(_) | RuntimeErr(_), CompileErr(_) | RuntimeErr(_)) => {
                Verdict::Rejected
            }
            _ => Verdict::Unsure,
        }
    }
}

impl fmt::Display for Verdict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Verdict::Ran => "trace",
            Verdict::Contained => "contained",
            Verdict::Rejected => "reject",
            Verdict::Excluded => "excluded",
            Verdict::Unsure => "unsure",
        })
    }
}

impl FromStr for Verdict {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        Ok(match s {
            "trace" => Verdict::Ran,
            "contained" => Verdict::Contained,
            "reject" => Verdict::Rejected,
            "excluded" => Verdict::Excluded,
            "unsure" => Verdict::Unsure,
            _ => return Err(()),
        })
    }
}

/// [`check`] with the [`Verdict`] of an agreement: every comparison the
/// subject owes, judged through the confirm ladder, stopping at the first
/// divergence. A dropped comparison says nothing of the others, so they
/// still run, and the check ends unsure if none diverged.
pub async fn check_verdict(
    code: &str,
    timeout: Duration,
) -> (Option<Divergence>, Verdict) {
    let (d, r) = check_judged(code, timeout).await;
    (d, r.verdict)
}

/// [`check_verdict`] with what a refusal said ([`Recorded`]).
pub async fn check_judged(
    code: &str,
    timeout: Duration,
) -> (Option<Divergence>, Recorded) {
    judged(code, timeout, None).await
}

/// [`check_judged`] with each run holding at most `activations` live
/// activations, else the process's budget.
async fn judged(
    code: &str,
    timeout: Duration,
    activations: Option<u64>,
) -> (Option<Divergence>, Recorded) {
    let unsure = || Recorded { verdict: Verdict::Unsure, why: None };
    let tier = oracle_tier(code);
    let pairs = comparisons(code, sessions_enabled());
    let mut o = Oracle::new(code, tier, timeout, activations);
    o.make(&[Pair::Engine]).await;
    if let Some(d) = o.construction().await {
        return (Some(d), unsure());
    }
    if let Some(d) = o.check_pair().await {
        return (Some(d), unsure());
    }
    let mut dropped = false;
    for &pair in &pairs {
        o.make_group(&pairs, pair).await;
        if let Some(d) = o.twin().await {
            return (Some(d), unsure());
        }
        match o.settle(pair).await {
            Settled::Agree => (),
            Settled::Drop => dropped = true,
            Settled::Diverge(pair, reference, tested) => {
                let d =
                    Divergence { code: code.to_string(), reference, tested, tier, pair };
                return (Some(d), unsure());
            }
        }
    }
    if let Some(d) = o.twin().await {
        return (Some(d), unsure());
    }
    match dropped {
        true => (None, unsure()),
        false => (None, o.recorded()),
    }
}

/// A pin's verdict as the outcome manifest records it: a refusal's with
/// the innermost cause it gave, normalized ([`tm_error_head`]), since one
/// refusal agrees with any other.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Recorded {
    pub verdict: Verdict,
    pub why: Option<String>,
}

impl fmt::Display for Recorded {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.why {
            None => write!(f, "{}", self.verdict),
            Some(why) => write!(f, "{}\t{why}", self.verdict),
        }
    }
}

impl FromStr for Recorded {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let (verdict, why) = match s.split_once('\t') {
            None => (s.parse()?, None),
            Some((v, why)) => (v.parse()?, Some(why.to_string())),
        };
        Ok(Recorded { verdict, why })
    }
}

/// One run the oracle makes of a subject: an engine on a route, in a
/// session.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Run {
    mode: Mode,
    route: Route,
    session: Session,
}

impl Run {
    /// `mode route session`, as [`Self::parse`] reads it.
    fn token(self) -> String {
        format!("{:?} {:?} {:?}", self.mode, self.route, self.session)
    }

    fn parse(s: &str) -> Option<Self> {
        let mut w = s.split_whitespace();
        let mode = match w.next()? {
            "Interp" => Mode::Interp,
            "Jit" => Mode::Jit,
            "Par" => Mode::Par,
            "JitPar" => Mode::JitPar,
            "Unpruned" => Mode::Unpruned,
            _ => return None,
        };
        let route = match w.next()? {
            "InLanguage" => Route::InLanguage,
            "Dispatch" => Route::Dispatch,
            _ => return None,
        };
        let session = match w.next()? {
            "NoCache" => Session::NoCache,
            "Cold" => Session::Cold,
            "Warm" => Session::Warm,
            _ => return None,
        };
        Some(Run { mode, route, session })
    }

    /// The node-walk's run of the same path.
    fn node_walk(self) -> Self {
        let mode = match self.mode {
            Mode::Jit => Mode::Interp,
            Mode::JitPar => Mode::Par,
            m => m,
        };
        Run { mode, ..self }
    }

    const fn plain(mode: Mode, route: Route) -> Self {
        Run { mode, route, session: Session::NoCache }
    }
}

impl Pair {
    /// Human labels for `reference` and `tested`.
    pub fn labels(self) -> (CompactString, CompactString) {
        let (reference, tested) = match self {
            Pair::Engine => ("interp", "jit"),
            Pair::EngineDispatch => ("interp/dispatch", "jit/dispatch"),
            Pair::Route => ("in-language", "dispatch"),
            Pair::Twin => ("trace", "trace"),
            Pair::Rejected | Pair::Accepted => ("interp", "jit"),
            Pair::Check => ("check", "build"),
            Pair::Prune => (mode_label(Mode::Unpruned), mode_label(Mode::Interp)),
            Pair::Rerun(_) => ("first run", "rerun"),
            Pair::Construction => ("construction", "construction"),
            Pair::Unparsed => ("parse", "parse"),
            Pair::Par(m) => (mode_label(Mode::Interp), mode_label(m)),
            Pair::Cold(m, r) => {
                return (
                    session_label(m, r, Session::NoCache),
                    session_label(m, r, Session::Cold),
                );
            }
            Pair::Warm(m, r) => {
                return (
                    session_label(m, r, Session::Cold),
                    session_label(m, r, Session::Warm),
                );
            }
        };
        (reference.into(), tested.into())
    }

    /// The reference and tested runs of a comparison; `None` for the
    /// findings that are not one.
    fn runs(self) -> Option<(Run, Run)> {
        let (i, j) = (Route::InLanguage, Route::Dispatch);
        let session = |mode, route, session| Run { mode, route, session };
        Some(match self {
            Pair::Engine => (Run::plain(Mode::Interp, i), Run::plain(Mode::Jit, i)),
            Pair::EngineDispatch => {
                (Run::plain(Mode::Interp, j), Run::plain(Mode::Jit, j))
            }
            Pair::Route => (Run::plain(Mode::Interp, i), Run::plain(Mode::Interp, j)),
            Pair::Par(m) => (Run::plain(Mode::Interp, i), Run::plain(m, i)),
            Pair::Prune => (Run::plain(Mode::Unpruned, i), Run::plain(Mode::Interp, i)),
            Pair::Cold(m, r) => (Run::plain(m, r), session(m, r, Session::Cold)),
            Pair::Warm(m, r) => {
                (session(m, r, Session::Cold), session(m, r, Session::Warm))
            }
            Pair::Twin
            | Pair::Rejected
            | Pair::Accepted
            | Pair::Check
            | Pair::Rerun(_)
            | Pair::Construction
            | Pair::Unparsed => {
                return None;
            }
        })
    }

    /// Comparisons whose runs are made together.
    fn group(self) -> u8 {
        match self {
            Pair::Cold(_, Route::InLanguage) | Pair::Warm(_, Route::InLanguage) => 1,
            Pair::Cold(_, Route::Dispatch) | Pair::Warm(_, Route::Dispatch) => 2,
            Pair::Par(_) => 3,
            _ => 0,
        }
    }
}

/// Every comparison a subject owes, in the order they are judged: the
/// engines, then (a callable) the dispatch route's engines and the route
/// pair, then each engine's image sessions (the forked JIT's included),
/// then the forked runs. A
/// program whose values are not comparable owes them all at kind
/// strength.
fn comparisons(code: &str, sessions: bool) -> Vec<Pair> {
    // an Excluded program still owes every comparison of outcome kinds
    let mut out = vec![Pair::Engine];
    if callable::has_header(code) {
        out.extend([Pair::EngineDispatch, Pair::Route]);
    }
    if sessions {
        // the forked JIT too: a warm start under forked evaluation is the
        // shell's default, and its Par pair ties it to the serial runs
        let forked = par_enabled().then_some(Mode::JitPar);
        for &route in session_routes(code) {
            for mode in [Mode::Interp, Mode::Jit].into_iter().chain(forked) {
                out.extend([Pair::Cold(mode, route), Pair::Warm(mode, route)]);
            }
        }
    }
    if par_enabled() {
        out.extend([Pair::Par(Mode::Par), Pair::Par(Mode::JitPar)]);
    }
    out.push(Pair::Prune);
    out
}

/// A run of `code`, or a session's cold and warm runs, which are one
/// job since the warm one restores what the cold one wrote.
#[derive(Clone, Copy)]
enum Job {
    Plain(Run),
    Sessions(Mode, Route),
}

async fn do_job(
    code: &str,
    job: Job,
    timeout: Duration,
    opts: RunOpts,
) -> Vec<(Run, Outcome)> {
    match job {
        Job::Plain(r) => {
            log_run("RUN", r);
            let o = run_subject(code, r.mode, r.route, SessionImage::None, timeout, opts)
                .await
                .outcome;
            log_run("DONE", r);
            vec![(r, o)]
        }
        Job::Sessions(mode, route) => {
            let run = |session| Run { mode, route, session };
            let (c, w) = (run(Session::Cold), run(Session::Warm));
            log_run("RUN", c);
            log_run("RUN", w);
            let (cold, warm) = cold_warm_with(code, mode, route, timeout, opts).await;
            log_run("DONE", c);
            log_run("DONE", w);
            vec![(c, cold), (w, warm)]
        }
    }
}

/// Whether this process logs each run it makes on stderr
/// (`GRAPHIX_FUZZ_RUN_LOG`), so the parent of a check child that died
/// knows which runs were in flight.
fn run_log() -> bool {
    static ON: std::sync::LazyLock<bool> =
        std::sync::LazyLock::new(|| std::env::var_os("GRAPHIX_FUZZ_RUN_LOG").is_some());
    *ON
}

/// Whether this process makes its runs one at a time
/// (`GRAPHIX_FUZZ_SERIAL_RUNS`): concurrent runs share one address-space
/// cap, so a death beside another run says nothing about either.
fn serial_runs() -> bool {
    static ON: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
        std::env::var_os("GRAPHIX_FUZZ_SERIAL_RUNS").is_some()
    });
    *ON
}

fn log_run(what: &str, r: Run) {
    if run_log() {
        eprintln!("{what} {}", r.token());
    }
}

/// The run `token` (`mode route session`, as a check child logs it) of
/// `code`.
pub async fn run_named(
    code: &str,
    token: &str,
    budget: Duration,
) -> anyhow::Result<Outcome> {
    let run = Run::parse(token).ok_or_else(|| {
        anyhow::anyhow!("run-one requires `mode route session`, not {token:?}")
    })?;
    Ok(run_one(code, run, budget).await)
}

/// `run` again at `budget`.
async fn run_one(code: &str, run: Run, budget: Duration) -> Outcome {
    log_run("RUN", run);
    let o = run_one_inner(code, run, budget).await;
    log_run("DONE", run);
    o
}

async fn run_one_inner(code: &str, run: Run, budget: Duration) -> Outcome {
    match run.session {
        Session::NoCache => run_program_routed(code, run.mode, run.route, budget).await,
        Session::Cold => {
            let opts = RunOpts::default();
            run_session(code, run.mode, run.route, SessionImage::Write, budget, opts)
                .await
                .0
        }
        Session::Warm => cold_warm(code, run.mode, run.route, budget).await.1,
    }
}

/// How one comparison came out.
enum Settled {
    Agree,
    /// The ladder found nothing stable to say: containment, slowness or
    /// the nondeterminism a non-`Exact` tier allows.
    Drop,
    Diverge(Pair, Outcome, Outcome),
}

/// A subject's runs, made as its comparisons ask for them.
struct Oracle<'a> {
    code: &'a str,
    tier: OracleTier,
    timeout: Duration,
    /// The live activations each run may hold, where they differ from the
    /// process's budget.
    activations: Option<u64>,
    runs: Vec<(Run, Outcome)>,
    /// Runs before this index have been scanned for a twin violation.
    scanned: usize,
    /// A confirm step decided a comparison, so the agreement says nothing
    /// stable about the program.
    confirmed: bool,
}

impl<'a> Oracle<'a> {
    fn new(
        code: &'a str,
        tier: OracleTier,
        timeout: Duration,
        activations: Option<u64>,
    ) -> Self {
        Oracle {
            code,
            tier,
            timeout,
            activations,
            runs: Vec::new(),
            scanned: 0,
            confirmed: false,
        }
    }

    fn opts(&self) -> RunOpts {
        RunOpts { activations: self.activations, ..RunOpts::default() }
    }

    fn get(&self, run: Run) -> &Outcome {
        &self.runs.iter().find(|(r, _)| *r == run).expect("a made run").1
    }

    /// Make `run` again at `budget`, and with a session run its sibling,
    /// since the warm run restores what the cold one wrote.
    async fn remake(&mut self, run: Run, budget: Duration) {
        let job = match run.session {
            Session::NoCache => Job::Plain(run),
            Session::Cold | Session::Warm => Job::Sessions(run.mode, run.route),
        };
        for (r, o) in do_job(self.code, job, budget, self.opts()).await {
            if let Some((_, x)) = self.runs.iter_mut().find(|(x, _)| *x == r) {
                *x = o;
            }
        }
    }

    /// `run` again at `budget`, watching its progress: its outcome when it
    /// timed out inside one cycle, else `None`.
    async fn stalled(&self, run: Run, budget: Duration) -> Option<Outcome> {
        let image = match run.session {
            Session::NoCache => SessionImage::None,
            Session::Cold => SessionImage::Write,
            Session::Warm => {
                let opts = self.opts();
                let (cold, image) = run_session(
                    self.code,
                    run.mode,
                    run.route,
                    SessionImage::Write,
                    budget,
                    opts,
                )
                .await;
                SessionImage::Read(restorable(&cold, image).ok()?)
            }
        };
        let opts = RunOpts { watch_progress: true, ..self.opts() };
        let probe =
            run_subject(self.code, run.mode, run.route, image, budget, opts).await;
        probe.stalled.then_some(probe.outcome)
    }

    /// Make every run of `pairs` not made yet, concurrently.
    async fn make(&mut self, pairs: &[Pair]) {
        let mut jobs: Vec<Job> = Vec::new();
        for (a, b) in pairs.iter().filter_map(|p| p.runs()) {
            for r in [a, b] {
                if self.runs.iter().any(|(x, _)| *x == r) {
                    continue;
                }
                let job = match r.session {
                    Session::NoCache => Job::Plain(r),
                    Session::Cold | Session::Warm => Job::Sessions(r.mode, r.route),
                };
                let same = |j: &Job| match (j, &job) {
                    (Job::Plain(x), Job::Plain(y)) => x == y,
                    (Job::Sessions(m, r), Job::Sessions(n, q)) => m == n && r == q,
                    _ => false,
                };
                if !jobs.iter().any(same) {
                    jobs.push(job);
                }
            }
        }
        let (code, timeout, opts) = (self.code, self.timeout, self.opts());
        if serial_runs() {
            for j in jobs {
                self.runs.extend(do_job(code, j, timeout, opts).await);
            }
        } else {
            let done = futures::future::join_all(
                jobs.into_iter().map(|j| do_job(code, j, timeout, opts)),
            )
            .await;
            self.runs.extend(done.into_iter().flatten());
        }
    }

    /// Make the runs of every comparison in `pair`'s group.
    async fn make_group(&mut self, pairs: &[Pair], pair: Pair) {
        let group: Vec<Pair> =
            pairs.iter().copied().filter(|p| p.group() == pair.group()).collect();
        self.make(&group).await;
    }

    /// The node-walk's construction outlived the deadline, and again at
    /// the slow budget: a single-run finding, since every run shares the
    /// checker.
    async fn construction(&self) -> Option<Divergence> {
        let (a, _) = Pair::Engine.runs()?;
        let o @ Outcome::Timeout(Containment::Compile) = self.get(a) else {
            return None;
        };
        let again = run_one(self.code, a, slow_budget(self.timeout)).await;
        matches!(again, Outcome::Timeout(Containment::Compile)).then(|| Divergence {
            code: self.code.to_string(),
            reference: o.clone(),
            tested: again,
            tier: self.tier,
            pair: Pair::Construction,
        })
    }

    /// Both engines refused the program, but the check alone accepts it:
    /// elaboration refused what the check passed. A compile verdict owes
    /// nothing to values, so an excluded program is checked too.
    async fn check_pair(&self) -> Option<Divergence> {
        let (a, b) = Pair::Engine.runs()?;
        let (Outcome::CompileErr(_), jit @ Outcome::CompileErr(e)) =
            (self.get(a), self.get(b))
        else {
            return None;
        };
        if graphix_compiler::analysis::build_only_refusal(e) {
            return None;
        }
        check_only(self.code, self.timeout).await.ok()?;
        Some(Divergence {
            code: self.code.to_string(),
            reference: Outcome::Checked,
            tested: jit.clone(),
            tier: self.tier,
            pair: Pair::Check,
        })
    }

    /// A twin violation is a single-run finding: a bug breaking every run
    /// identically agrees on the wrong answer. Confirmed by one rerun.
    async fn twin(&mut self) -> Option<Divergence> {
        if self.tier == OracleTier::Excluded {
            return None;
        }
        while self.scanned < self.runs.len() {
            let (run, o) = self.runs[self.scanned].clone();
            self.scanned += 1;
            if twin_violation(&o)
                && twin_violation(&run_one(self.code, run, self.timeout).await)
            {
                return Some(Divergence {
                    code: self.code.to_string(),
                    reference: o.clone(),
                    tested: o,
                    tier: self.tier,
                    pair: Pair::Twin,
                });
            }
        }
        None
    }

    fn runtime_contained(&self, pair: Pair, a: &Outcome, b: &Outcome) -> bool {
        let (ra, rb) = pair.runs().expect("a comparison");
        let reason = if ra.mode.node_walk()
            && !rb.mode.node_walk()
            && matches!(
                a,
                Outcome::Timeout(
                    Containment::StackBudget | Containment::ActivationBudget
                )
            )
            && matches!(b, Outcome::Trace(_))
        {
            "the node-walk exceeded its stack or activation budget; the kernel's value stands unrefuted"
        } else if matches!(pair, Pair::Par(_))
            && matches!(
                (a, b),
                (Outcome::Timeout(Containment::StackBudget), Outcome::Trace(_))
                    | (Outcome::Trace(_), Outcome::Timeout(Containment::StackBudget))
            )
        {
            "the stack budget stopped one schedule and not the other"
        } else {
            return false;
        };
        eprintln!("CONTAINED — {reason}; not recorded");
        eprintln!("    program: {}", self.code.replace('\n', "\\n"));
        true
    }

    /// One comparison through the confirm ladder.
    async fn settle(&mut self, pair: Pair) -> Settled {
        let (ra, rb) = pair.runs().expect("a comparison");
        let s = Strength::of(pair, self.tier);
        let (mut a, mut b) = (self.get(ra).clone(), self.get(rb).clone());
        // A node-walk budget beside a JIT deadline is no race between two
        // descents: the JIT's native loop counts no activations, so the
        // node-walk aborts any recursion deeper than its budget, ending or
        // not. The node-walk again with the room to finish refutes a JIT
        // that hung; another abort says nothing.
        if ra.mode.node_walk()
            && !rb.mode.node_walk()
            && ra.session == Session::NoCache
            && matches!(
                a,
                Outcome::Timeout(
                    Containment::StackBudget | Containment::ActivationBudget
                )
            )
            && matches!(b, Outcome::Timeout(Containment::Deadline))
        {
            self.confirmed = true;
            let budget = slow_budget(self.timeout);
            let opts = RunOpts {
                activations: Some(ACTIVATION_BUDGET * 4),
                ..RunOpts::default()
            };
            let again = run_subject(
                self.code,
                ra.mode,
                ra.route,
                SessionImage::None,
                budget,
                opts,
            )
            .await
            .outcome;
            if matches!(again, Outcome::Trace(_)) {
                return Settled::Diverge(pair, again, b);
            }
            eprintln!(
                "CONTAINED — the node-walk exceeded its budget beside a JIT deadline, \
                 and again with four times the activations; not recorded"
            );
            eprintln!("    program: {}", self.code.replace('\n', "\\n"));
            return Settled::Drop;
        }
        if a.agrees_at(&b, s) {
            return Settled::Agree;
        }
        self.confirmed = true;
        if self.runtime_contained(pair, &a, &b) {
            return Settled::Drop;
        }
        // `#[tail_recursive]`'s loop is the JIT's assertion: the node-walk
        // checks only its shape, so a body that does not fuse runs there,
        // to a trace or a containment, and is refused by the JIT
        if ra.mode.node_walk()
            && !rb.mode.node_walk()
            && !matches!(a, Outcome::CompileErr(_))
            && let Outcome::CompileErr(e) = &b
            && e.contains("#[tail_recursive]: no loop is built")
        {
            eprintln!("JIT ASSERTION — #[tail_recursive] built no loop; not recorded");
            eprintln!("    program: {}", self.code.replace('\n', "\\n"));
            return Settled::Drop;
        }
        let retry = if a.needs_slow_retry(&b) {
            Some(ra)
        } else if b.needs_slow_retry(&a) {
            Some(rb)
        } else {
            None
        };
        let budget =
            if retry.is_some() { slow_budget(self.timeout) } else { self.timeout };
        if let Some(run) = retry {
            let cpu = self_cpu();
            self.remake(run, budget).await;
            let burned = self_cpu().saturating_sub(cpu);
            (a, b) = (self.get(ra).clone(), self.get(rb).clone());
            if a.agrees_at(&b, s) {
                return Settled::Agree;
            }
            if self.runtime_contained(pair, &a, &b) {
                return Settled::Drop;
            }
            // A wedge sits at ~0% CPU; seconds of burn are an honestly slow
            // reference, which refutes nothing, unless it burns them inside
            // one cycle: a run that starts no cycle over the second half of
            // the slow budget loops there, which the tested side's answer
            // refutes
            if run == ra
                && matches!(a, Outcome::Timeout(_))
                && burned >= Duration::from_secs(5)
            {
                if let Some(stalled) = self.stalled(ra, budget).await {
                    return Settled::Diverge(pair, stalled, b);
                }
                eprintln!(
                    "SLOW — the reference burned {:.1}s CPU over a {:.0}s budget \
                     without finishing; not recorded",
                    burned.as_secs_f64(),
                    budget.as_secs_f64()
                );
                eprintln!("    program: {}", self.code.replace('\n', "\\n"));
                return Settled::Drop;
            }
        }
        // a reference that disagrees with itself is nondeterminism, a
        // finding where the program has no source of it
        let again = run_one(self.code, ra, budget).await;
        if !again.agrees_at(&a, s) {
            if self.tier == OracleTier::Exact {
                return Settled::Diverge(Pair::Rerun(ra.mode), a, again);
            }
            eprintln!(
                "NONDETERMINISTIC — the reference disagreed with its rerun; not recorded"
            );
            eprintln!("    program: {}", self.code.replace('\n', "\\n"));
            eprintln!("    first: {}", render(&a));
            eprintln!("    rerun: {}", render(&again));
            return Settled::Drop;
        }
        Settled::Diverge(pair, a, b)
    }

    /// How an agreement concluded: the engines' kind, unless the ladder
    /// decided something or the program is excluded. A contained callable
    /// is unsure: its runs are four, the budget's race decides it.
    /// [`Self::verdict`], a refusal's with the node-walk's innermost cause.
    fn recorded(&self) -> Recorded {
        let verdict = self.verdict();
        let (a, _) = Pair::Engine.runs().expect("a comparison");
        let why = match (verdict, self.get(a)) {
            (Verdict::Rejected, Outcome::CompileErr(e) | Outcome::RuntimeErr(e)) => {
                Some(tm_error_head(e).replace('\t', " "))
            }
            _ => None,
        };
        Recorded { verdict, why }
    }

    fn verdict(&self) -> Verdict {
        let Some((a, b)) = Pair::Engine.runs() else { unreachable!() };
        let v = Verdict::of(self.get(a), self.get(b));
        // a rejection is a compile verdict, owed at every tier
        if self.tier == OracleTier::Excluded {
            return if v == Verdict::Rejected { v } else { Verdict::Excluded };
        }
        if self.confirmed {
            return Verdict::Unsure;
        }
        match v {
            Verdict::Contained if callable::has_header(self.code) => Verdict::Unsure,
            v => v,
        }
    }
}

/// The budget that tells a starved child from a hang: 8x, with an
/// absolute floor, since the scale gap is unbounded and load stretches
/// CPU seconds into wall minutes.
fn slow_budget(timeout: Duration) -> Duration {
    (timeout * 8).max(Duration::from_secs(60))
}

/// The longest a healthy `check-one` of `code` takes: its first runs (the
/// engine group, two cold-then-warm sessions per route, the forked pair,
/// the check, a twin's rerun) and every run its comparisons' ladders may
/// remake at the slow budget (a session's cold and warm runs are one job,
/// so either remakes both), each run with its shutdown grace.
fn check_deadline(code: &str, timeout: Duration) -> Duration {
    let grace = Duration::from_secs(5);
    let mut runs: Vec<Run> = Vec::new();
    for (a, b) in comparisons(code, sessions_enabled()).into_iter().filter_map(Pair::runs)
    {
        for r in [a, b] {
            if !runs.contains(&r) {
                runs.push(r);
            }
        }
    }
    let slow =
        runs.iter().map(|r| if r.session == Session::NoCache { 1 } else { 2 }).sum();
    (timeout + grace) * 12
        + (slow_budget(timeout) + grace) * slow
        + Duration::from_secs(60)
}

/// Per-subject verdict from a batch child. Only agreement is trusted
/// from a batch; anything else re-runs through `check_isolated`, so
/// every finding still derives from a fresh single-subject process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BatchVerdict {
    /// Both outcomes were runtime traces: the ring-admission bar.
    Ran,
    /// Both builds refused it.
    Refused,
    /// Any other agreement.
    Agree,
    /// Not a clean agreement (divergence-shaped, timeout, ineligible
    /// subject) — the parent re-runs individually.
    Other,
}

pub async fn run_batch(
    progs: &[String],
    timeout: Duration,
    mut report: impl FnMut(usize, BatchVerdict),
) {
    for (i, code) in progs.iter().enumerate() {
        let verdict = match Subject::parse(code, "test") {
            Ok(s) => first_pass(code, s.tier, sessions_sampled(i), timeout).await,
            Err(_) => BatchVerdict::Other,
        };
        report(i, verdict);
    }
}

/// [`check_verdict`] without its ladder: an agreement only when every
/// comparison agrees at once and no run timed out or failed; anything
/// else goes back through the individual path, which confirms it.
async fn first_pass(
    code: &str,
    tier: OracleTier,
    sessions: bool,
    timeout: Duration,
) -> BatchVerdict {
    let pairs = comparisons(code, sessions);
    let mut o = Oracle::new(code, tier, timeout, None);
    o.make(&[Pair::Engine]).await;
    if o.check_pair().await.is_some() {
        return BatchVerdict::Other;
    }
    let suspect = |o: &Outcome| {
        matches!(o, Outcome::Timeout(_) | Outcome::RuntimeErr(_))
            || (tier != OracleTier::Excluded && twin_violation(o))
    };
    for &pair in &pairs {
        o.make_group(&pairs, pair).await;
        let (a, b) = pair.runs().expect("a comparison");
        if o.runs.iter().any(|(_, x)| suspect(x))
            || !o.get(a).agrees_at(o.get(b), Strength::of(pair, tier))
        {
            return BatchVerdict::Other;
        }
    }
    if o.runs.iter().any(|(_, x)| suspect(x)) {
        return BatchVerdict::Other;
    }
    let (a, b) = Pair::Engine.runs().expect("a comparison");
    match (o.get(a), o.get(b)) {
        // the ring-admission bar, as the individual path's
        (Outcome::Trace(_), Outcome::Trace(_))
            if tier != OracleTier::Excluded && !callable::has_header(code) =>
        {
            BatchVerdict::Ran
        }
        (Outcome::CompileErr(_), Outcome::CompileErr(_)) => BatchVerdict::Refused,
        _ => BatchVerdict::Agree,
    }
}

/// Batch size for the campaign pool's batch children. 1 disables
/// batching. Larger than the default starves the mutation ring's
/// agreement feedback and coarsens pool granularity in finite gates.
fn batch_size() -> usize {
    std::env::var("GRAPHIX_FUZZ_BATCH")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(64)
        .max(1)
}

/// The soak pool's sources, in mix order.
pub const SOURCES: usize = 4;

pub const SOURCE_KINDS: [SourceKind; SOURCES] =
    [SourceKind::Fuzz, SourceKind::Generate, SourceKind::Reactive, SourceKind::Typemorph];

/// Which generator a work order asks the child to run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    Fuzz,
    Generate,
    Reactive,
    /// Acceptance probes (`typemorph`): the corpus pins once, then
    /// generated programs and mutants; a flip is the finding.
    Typemorph,
}

impl SourceKind {
    pub fn tag(&self) -> &'static str {
        match self {
            SourceKind::Fuzz => "fuzz",
            SourceKind::Generate => "generate",
            SourceKind::Reactive => "reactive",
            SourceKind::Typemorph => "typemorph",
        }
    }

    /// What this source's findings are called in its progress line.
    pub fn findings(&self) -> &'static str {
        match self {
            SourceKind::Typemorph => "flips",
            _ => "divergences",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "fuzz" => Some(SourceKind::Fuzz),
            "generate" => Some(SourceKind::Generate),
            "reactive" => Some(SourceKind::Reactive),
            "typemorph" => Some(SourceKind::Typemorph),
            _ => None,
        }
    }
}

/// What the parent asks a child to do, instead of what to run, so the
/// parent's cost is per batch and per finding rather than per subject.
/// `ring` is a small sample of the mutation ring for the child to breed
/// from, not a snapshot. `pins` are regression corpus indices checked
/// before anything is drawn.
#[derive(Debug, Clone)]
pub struct WorkOrder {
    pub kind: SourceKind,
    pub seed: u64,
    pub count: usize,
    pub ring: Vec<String>,
    pub pins: std::ops::Range<usize>,
}

impl WorkOrder {
    pub fn encode(&self) -> String {
        let mut out = format!(
            "{} {} {} {} {} {}\n",
            self.kind.tag(),
            self.seed,
            self.count,
            self.ring.len(),
            self.pins.start,
            self.pins.end,
        );
        for p in &self.ring {
            out.push_str(&format!("{}\n", p.len()));
            out.push_str(p);
        }
        out
    }

    pub fn decode(input: &str) -> anyhow::Result<Self> {
        let (head, mut rest) =
            input.split_once('\n').ok_or_else(|| anyhow::anyhow!("work order: empty"))?;
        let mut it = head.split_whitespace();
        let kind = it
            .next()
            .and_then(SourceKind::parse)
            .ok_or_else(|| anyhow::anyhow!("work order: bad source"))?;
        let seed: u64 = it.next().unwrap_or("0").parse()?;
        let count: usize = it.next().unwrap_or("0").parse()?;
        let nring: usize = it.next().unwrap_or("0").parse()?;
        let pins: usize = it.next().unwrap_or("0").parse()?;
        let pins = pins..it.next().unwrap_or("0").parse()?;
        let mut ring = Vec::with_capacity(nring);
        for _ in 0..nring {
            let (len, tail) = rest
                .split_once('\n')
                .ok_or_else(|| anyhow::anyhow!("work order: truncated ring header"))?;
            let len: usize = len.trim().parse()?;
            if tail.len() < len {
                anyhow::bail!("work order: truncated ring body");
            }
            ring.push(tail[..len].to_string());
            rest = &tail[len..];
        }
        Ok(WorkOrder { kind, seed, count, ring, pins })
    }

    /// Build the generator this order describes. Seeded from the order,
    /// so a subject stays reproducible as (kind, seed, index).
    pub fn generator(&self) -> Box<dyn FnMut() -> String + Send> {
        let mut rng = mutate::Rng::new(self.seed);
        match self.kind {
            SourceKind::Generate => Box::new(move || generate::gen_program(&mut rng)),
            SourceKind::Reactive => {
                Box::new(move || generate::reactive::gen_reactive_program(&mut rng))
            }
            SourceKind::Fuzz => {
                let seeds = corpus::all_seeds();
                let donors = mutate::donor_pool(&seeds);
                let ring = self.ring.clone();
                Box::new(move || mutant(&mut rng, &seeds, &donors, &ring))
            }
            SourceKind::Typemorph => {
                let seeds = corpus::all_seeds();
                let donors = mutate::donor_pool(&seeds);
                let ring = self.ring.clone();
                let mut pins = self.pins.clone();
                Box::new(move || match pins.next() {
                    Some(i) => corpus::REGRESSION_CORPUS[i].1.to_string(),
                    None => match rng.below(3) {
                        0 => generate::gen_program(&mut rng),
                        1 => generate::reactive::gen_reactive_program(&mut rng),
                        _ => mutant(&mut rng, &seeds, &donors, &ring),
                    },
                })
            }
        }
    }
}

/// A mutant of a ring sample or a corpus seed, half each while the ring
/// has anything; the seed itself when eight mutation tries fail.
fn mutant(
    rng: &mut mutate::Rng,
    seeds: &[&'static str],
    donors: &[Expr],
    ring: &[String],
) -> String {
    for _ in 0..8 {
        let s = if !ring.is_empty() && rng.below(2) == 0 {
            ring[rng.below(ring.len())].clone()
        } else {
            seeds[rng.below(seeds.len())].to_string()
        };
        if let Some(p) = mutate::mutate_wrapper(&s, donors, rng, 5) {
            return p;
        }
    }
    seeds[rng.below(seeds.len())].to_string()
}

/// The `gen-batch` child body: generate the order's subjects, run them
/// against one warmed runtime pair, and report back only what the
/// parent cannot compute for itself: a verdict line per subject, and
/// the program text only for a divergence or a ring-novel shape.
pub async fn run_work_order(
    order: &WorkOrder,
    timeout: Duration,
    out: &mut impl std::io::Write,
) {
    let mut next = order.generator();
    let progs: Vec<String> = (0..order.count).map(|_| next()).collect();
    if order.kind == SourceKind::Typemorph {
        return typemorph_work_order(&progs, timeout, out).await;
    }
    // ring admission is computed here, where the program is parsed; only
    // mutants breed
    let novel: Vec<Option<(u64, usize, bool)>> = match order.kind {
        SourceKind::Fuzz => progs.iter().map(|p| mutate::shape_stats(p)).collect(),
        _ => vec![None; progs.len()],
    };
    run_batch(&progs, timeout, |i, v| {
        let tag = match v {
            BatchVerdict::Ran => "R",
            BatchVerdict::Refused => "J",
            BatchVerdict::Agree => "A",
            BatchVerdict::Other => "O",
        };
        let _ = writeln!(out, "V {i} {tag}");
        if v == BatchVerdict::Other {
            write_program(out, "P", i, &progs[i]);
        } else if v == BatchVerdict::Ran
            && let Some((sig, nodes, ok)) = novel[i]
            && ok
            && (8..=600).contains(&nodes)
        {
            write_program(out, "N", sig, &progs[i]);
        }
        let _ = out.flush();
    })
    .await;
    let _ = writeln!(out, "CPU {}", self_cpu().as_micros());
    let _ = out.flush();
}

/// The typemorph body of a work order: every subject's probes, a line
/// per subject, and the program text of each that flipped for the
/// parent to confirm in a fresh process.
async fn typemorph_work_order(
    progs: &[String],
    timeout: Duration,
    out: &mut impl std::io::Write,
) {
    for (i, p) in progs.iter().enumerate() {
        let suspect = match typemorph_subject(p, timeout, TM_CAP).await {
            Ok(rep) => match rep.flipped().collect::<Vec<_>>().join(" ") {
                ids if ids.is_empty() => None,
                ids => Some(format!("O {ids}")),
            },
            Err(_) => Some("U".to_string()),
        };
        match suspect {
            None => {
                let _ = writeln!(out, "V {i} A");
            }
            Some(v) => {
                let _ = writeln!(out, "V {i} {v}");
                write_program(out, "P", i, p);
            }
        }
        let _ = out.flush();
    }
    let _ = writeln!(out, "CPU {}", self_cpu().as_micros());
    let _ = out.flush();
}

/// A program text after its `<tag> <n> <length>` line.
fn write_program(
    out: &mut impl std::io::Write,
    tag: &str,
    n: impl fmt::Display,
    p: &str,
) {
    let _ = writeln!(out, "{tag} {n} {}", p.len());
    let _ = out.write_all(p.as_bytes());
    let _ = writeln!(out);
}

/// A program a work order hands back for the individual path.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Suspect {
    /// The child could not resolve it (a `P` line after `V i O`), and for
    /// a typemorph child the probes that flipped there.
    Flagged(String, Vec<String>),
    /// The child died before its verdict, or could not judge it (a `P`
    /// line after `V i U`).
    Unjudged(String),
}

impl Suspect {
    fn prog(&self) -> &str {
        match self {
            Suspect::Flagged(p, _) | Suspect::Unjudged(p) => p,
        }
    }
}

/// What one work order came back with. Everything here is per-BATCH or
/// per-FINDING; nothing scales with the number of agreeing subjects.
pub(crate) struct OrderResult {
    /// Subjects the child agreed on.
    pub agreed: usize,
    /// Of those, the subjects both builds refused.
    pub refused: usize,
    /// Programs the child could not resolve, and on a child that died
    /// every subject it left without a verdict; the parent re-derives
    /// them through the individual path, which records the one that
    /// killed it.
    pub suspect: Vec<Suspect>,
    /// (signature, program) the child judged ring-worthy.
    pub novel: Vec<(u64, String)>,
    pub cpu: Duration,
    pub clean: bool,
}

/// Issue ONE work order to a child and collect its summary.
async fn run_order_child(order: &WorkOrder, timeout: Duration) -> OrderResult {
    // progress-based deadline: a healthy child flushes a line per
    // subject, and a typemorph subject is its base and every probe
    let per_subject = match order.kind {
        SourceKind::Typemorph => tm_checks(TM_CAP),
        _ => 4,
    };
    let stall = timeout * per_subject + Duration::from_secs(90);
    let c =
        run_child("gen-batch", &[], &[], &order.encode(), false, Deadline::Stall(stall))
            .await;
    let exited_zero = c.status.is_some_and(|s| s.success());
    let mut res = order_result(order, c.verdict.as_deref().unwrap_or(""), exited_zero);
    res.cpu += c.cpu;
    res
}

/// What a work order's output says, from a child that exited 0 or not.
fn order_result(order: &WorkOrder, text: &str, exited_zero: bool) -> OrderResult {
    let mut res = OrderResult {
        agreed: 0,
        refused: 0,
        suspect: Vec::new(),
        novel: Vec::new(),
        cpu: Duration::ZERO,
        clean: false,
    };
    // a subject's verdict: agreed, or its program came back as a suspect
    let mut resolved = vec![false; order.count];
    // the last `V i O|U` line: the subject its `P` line hands back
    let mut handed: Option<(Option<usize>, &str, Vec<String>)> = None;
    let mut finished = false;
    let mut rest = text;
    while let Some((line, tail)) = rest.split_once('\n') {
        rest = tail;
        let mut it = line.split_whitespace();
        match it.next() {
            Some("V") => {
                let i: Option<usize> = it.next().and_then(|v| v.parse().ok());
                match it.next() {
                    Some(v @ ("O" | "U")) => {
                        handed = Some((i, v, it.map(String::from).collect()));
                    }
                    v => {
                        res.agreed += 1;
                        res.refused += (v == Some("J")) as usize;
                        if let Some(r) = i.and_then(|i| resolved.get_mut(i)) {
                            *r = true;
                        }
                    }
                }
            }
            Some(kind @ ("N" | "P")) => {
                let id: u64 = it.next().and_then(|v| v.parse().ok()).unwrap_or(0);
                let len: usize = match it.next().and_then(|v| v.parse().ok()) {
                    Some(n) => n,
                    None => break,
                };
                if rest.len() < len {
                    break;
                }
                let prog = rest[..len].to_string();
                rest = rest[len..].strip_prefix('\n').unwrap_or(&rest[len..]);
                if kind == "N" {
                    res.novel.push((id, prog));
                } else {
                    let i = id as usize;
                    if let Some(r) = resolved.get_mut(i) {
                        *r = true;
                    }
                    res.suspect.push(match handed.take() {
                        Some((Some(j), "U", _)) if j == i => Suspect::Unjudged(prog),
                        Some((Some(j), _, flips)) if j == i => {
                            Suspect::Flagged(prog, flips)
                        }
                        _ => Suspect::Flagged(prog, Vec::new()),
                    });
                }
            }
            Some("CPU") => {
                finished = true;
                if let Some(us) = it.next().and_then(|v| v.parse::<u64>().ok()) {
                    res.cpu = Duration::from_micros(us);
                }
            }
            _ => (),
        }
    }
    res.clean = exited_zero && finished;
    if !res.clean {
        let mut next = order.generator();
        for done in resolved {
            let p = next();
            if !done {
                res.suspect.push(Suspect::Unjudged(p));
            }
        }
    }
    res
}

/// Coarse "same bug" key: the bisection class, the outcome kinds and
/// the trace-difference class (final-strength for final-tier and route
/// divergences). The minimizer requires a reduction to keep the bucket.
fn bucket(d: &Divergence) -> (&'static str, u8, u8, Option<trace::TraceDiff>) {
    let td = match (&d.reference, &d.tested) {
        (Outcome::Trace(a), Outcome::Trace(b)) => match Strength::of(d.pair, d.tier) {
            Strength::Exact => a.first_difference(b),
            Strength::Final => a.first_final_difference(b),
            Strength::Kind => None,
        },
        _ => None,
    };
    (d.bisect(), d.reference.kind(), d.tested.kind(), td)
}

/// A divergence's [`Finding::key`]: its pair and [`bucket`].
fn finding_key(d: &Divergence) -> String {
    format!("{:?} {:?}", d.pair, bucket(d))
}

/// The minimizations a campaign spends on one [`Finding::key`]; later
/// findings with the key are counted, not minimized.
const MINIMIZE_PER_KEY: usize = 3;

/// What a campaign records about a divergence: its record's text and the
/// coarse key it dedups on before minimizing. A check child computes it
/// and the parent reads it back, so the parent never runs the subject.
#[derive(Debug, Clone, PartialEq)]
pub struct Finding {
    /// The pair and [`bucket`]: what the minimizer must preserve.
    pub key: String,
    pub bisect: String,
    pub labels: (String, String),
    pub reference: String,
    pub tested: String,
}

impl Finding {
    pub fn of(d: &Divergence) -> Self {
        let (la, lb) = d.pair.labels();
        Finding {
            key: finding_key(d),
            bisect: d.bisect().to_string(),
            labels: (la.to_string(), lb.to_string()),
            reference: render(&d.reference),
            tested: render(&d.tested),
        }
    }

    /// One escaped line per field.
    pub fn encode(&self) -> String {
        [
            &self.key,
            &self.bisect,
            &self.labels.0,
            &self.labels.1,
            &self.reference,
            &self.tested,
        ]
        .map(|f| escape_line(f) + "\n")
        .concat()
    }

    pub fn decode(s: &str) -> Option<Self> {
        let mut l = s.lines().map(unescape_line);
        let f = Finding {
            key: l.next()?,
            bisect: l.next()?,
            labels: (l.next()?, l.next()?),
            reference: l.next()?,
            tested: l.next()?,
        };
        Some(f)
    }
}

/// How the minimizer judges a candidate: in this process, or each in a
/// `check-one` child, which a candidate that kills the evaluator kills
/// alone.
#[derive(Clone, Copy)]
pub enum Judge {
    InProcess(Duration),
    Isolated(Duration),
}

impl Judge {
    /// The judge for minimizing a finding of key `want`: a crash's in
    /// children, else in this process.
    pub fn for_key(want: Option<&str>, timeout: Duration) -> Self {
        match want {
            Some(k) if k.starts_with(CRASH_CLASS) => Judge::Isolated(timeout),
            _ => Judge::InProcess(timeout),
        }
    }

    /// The key of what `prog` shows, `None` when it agrees: a
    /// divergence's [`Finding::key`] or a crash's [`crash_class`].
    async fn key(self, prog: &str) -> Option<String> {
        match self {
            Judge::InProcess(t) => check(prog, t).await.map(|d| finding_key(&d)),
            Judge::Isolated(t) => match check_isolated(prog, t).await.0 {
                PoolResult::Agree => None,
                PoolResult::Diverge(f) => Some(f.key),
                PoolResult::Crash(status) => Some(crash_class(&status)),
            },
        }
    }
}

const CRASH_CLASS: &str = "CRASH ";

/// The part of a crash status naming a panic's site, kept verbatim in
/// its class.
const PANIC_SITE: &str = "panicked at ";

/// A crash's key: the child's status, its panic site and last words, with
/// every number outside the site collapsed, so one panic site reached
/// with other values is one class.
fn crash_class(status: &str) -> String {
    let words = status
        .replace(" (core dumped)", "")
        .split(" | ")
        .filter(|w| !w.starts_with("note: "))
        .map(|w| match w.starts_with(PANIC_SITE) {
            true => w.to_string(),
            false => collapse_numbers(w),
        })
        .collect::<Vec<_>>()
        .join(" | ");
    format!("{CRASH_CLASS}{words}")
}

/// The key a crash of `prog` claims a minimization under: its class, or,
/// for a status with no words to tell causes apart (a hang, a silent
/// signal), its class and its program's [`crash_key`].
fn crash_claim(status: &str, prog: &str) -> String {
    let class = crash_class(status);
    match status.contains(" | ") {
        true => class,
        false => format!("{class} | {}", crash_key(prog)),
    }
}

/// Minimize a diverging or crashing wrapper: schedule reductions first
/// (drop the schedule, epochs, injections; simplify literals; caps stay
/// fixed), then the body and each `.gx` section by [`shrink`], keeping
/// any reduction that still parses and shows the same key under
/// `judge` (a divergence's bucket, a crash's class). The headers are
/// split off before parsing and reattached around every candidate.
/// Returns the minimized wrapper and the number of oracle checks spent
/// (capped by `budget`).
///
/// With `want`, a [`Finding::key`] or a crash class, a program whose
/// finding has another key comes back unchanged. `progress` sees each smaller
/// program the minimizer settles on.
pub async fn minimize(
    code: &str,
    judge: Judge,
    budget: usize,
    want: Option<&str>,
    progress: &(dyn Fn(&str) + Sync),
) -> (String, usize) {
    let Some(target) = judge.key(code).await else {
        return (code.to_string(), 1);
    };
    if want.is_some_and(|w| w != target) {
        return (code.to_string(), 1);
    }
    let Ok((mut sched, body)) = schedule::Schedule::parse(code) else {
        return (code.to_string(), 1);
    };
    let Ok((cspec, body_owned)) = callable::CallSpec::parse(&body) else {
        return (code.to_string(), 1);
    };
    let Ok((body, mut files)) = files::split(&body_owned) else {
        return (code.to_string(), 1);
    };
    let mut current = match mutate::parse_body(body) {
        Some(e) => e,
        None => return (code.to_string(), 1),
    };
    // the callable header rides every candidate verbatim
    let reattach = |text: String| match &cspec {
        Some(c) => c.render(&text),
        None => text,
    };
    // the result is a text a check judged: a reprint from the ASTs may
    // differ from every candidate (comments dropped)
    let last_hit = Mutex::new(code.to_string());
    let progress = &|text: &str| {
        *last_hit.lock() = text.to_string();
        progress(text)
    };
    let mut calls = 1;
    'sched: while calls < budget {
        let body_text = files::render(&current.to_string(), &files);
        for cand in schedule_reductions(&sched) {
            if calls >= budget {
                break 'sched;
            }
            calls += 1;
            let text = reattach(cand.render(&body_text));
            if judge.key(&text).await.as_deref() == Some(target.as_str()) {
                progress(&text);
                sched = cand;
                continue 'sched; // restart from the smaller schedule
            }
        }
        break;
    }
    // Whole-section drops, the body AST and each `.gx` section's item
    // sequence, lapped until nothing moves: the three feed each other.
    // Each subject gets an equal slice of the remaining budget per lap
    // so the body cannot starve the sections.
    while calls < budget {
        let before = reattach(sched.render(&files::render(&current.to_string(), &files)));
        // each module's section pair, then each interface alone
        'files: while calls < budget && !files.is_empty() {
            let body_text = current.to_string();
            for cand in file_reductions(&files) {
                if calls >= budget {
                    break 'files;
                }
                calls += 1;
                let text = reattach(sched.render(&files::render(&body_text, &cand)));
                if judge.key(&text).await.as_deref() == Some(target.as_str()) {
                    progress(&text);
                    files = cand;
                    continue 'files; // restart from the smaller file set
                }
            }
            break;
        }
        let sections: Vec<usize> =
            (0..files.len()).filter(|&i| files[i].0.ends_with(".gx")).collect();
        let lap = ((budget - calls) / (1 + sections.len())).max(1);
        let cap = budget.min(calls + lap);
        current = {
            let files = &files;
            let sched = &sched;
            shrink(
                current,
                &|e| {
                    let body = e.to_string();
                    mutate::parse(&body)?;
                    Some(reattach(sched.render(&files::render(&body, files))))
                },
                &target,
                judge,
                &mut calls,
                cap,
                progress,
            )
            .await
        };
        for &i in &sections {
            let Some(items) = mutate::parse_items(&files[i].1) else {
                continue;
            };
            let cap = budget.min(calls + lap);
            let reduced = {
                let sched = &sched;
                let files = &files;
                let body = current.to_string();
                shrink(
                    items,
                    &|e| {
                        let text = mutate::render_items(e);
                        mutate::parse_items(&text)?;
                        let mut fs = files.to_vec();
                        fs[i].1 = text;
                        Some(reattach(sched.render(&files::render(&body, &fs))))
                    },
                    &target,
                    judge,
                    &mut calls,
                    cap,
                    progress,
                )
                .await
            };
            files[i].1 = mutate::render_items(&reduced);
        }
        if reattach(sched.render(&files::render(&current.to_string(), &files))) == before
        {
            break; // every subject is at a fixpoint
        }
    }
    (last_hit.into_inner(), calls)
}

/// One reduction of an AST: either replace a node, or drop a statement
/// from a block. `at`/`pos` say where to apply it; `start`/`end` are the
/// preorder extent it consumes, which is how independent reductions are
/// told apart — for a drop that is the STATEMENT's extent, not the
/// block's, so the drops found in one scan are disjoint and compose.
struct Op {
    start: usize,
    end: usize,
    at: usize,
    pos: usize,
    repl: Option<Expr>,
}

impl Op {
    fn apply(&self, prog: &Expr) -> Expr {
        match &self.repl {
            Some(r) => mutate::replace(prog, self.at, r),
            None => mutate::drop_statement(prog, self.at, self.pos),
        }
    }
}

/// Apply a round's reductions. Descending `(at, pos)`, so each one acts
/// at a coordinate no earlier one has shifted: the ops are pairwise
/// disjoint, and dropping a later statement leaves earlier positions in
/// the same block untouched.
fn apply_ops(prog: &Expr, ops: &[Op]) -> Expr {
    let mut order: Vec<&Op> = ops.iter().collect();
    order.sort_by_key(|o| std::cmp::Reverse((o.at, o.pos)));
    let mut out = prog.clone();
    for o in order {
        out = o.apply(&out);
    }
    out
}

/// Every reduction to try against `prog`, widest extent first: each
/// block statement dropped, and each node replaced by a child or a
/// constant.
fn ops(prog: &Expr) -> Vec<Op> {
    let sizes = mutate::sizes(prog);
    let mut out: Vec<Op> = mutate::statements(prog)
        .into_iter()
        .map(|(at, pos, stmt)| Op {
            start: stmt,
            end: stmt + sizes[stmt],
            at,
            pos,
            repl: None,
        })
        .collect();
    for (at, repls) in mutate::reductions_all(prog).into_iter().enumerate() {
        for repl in repls {
            out.push(Op { start: at, end: at + sizes[at], at, pos: 0, repl: Some(repl) });
        }
    }
    // Drops before replacements, each widest-first: dropping a statement
    // is where the yield is, and the campaign budget only stretches to
    // the head of this list.
    out.sort_by_key(|o| (o.repl.is_some(), std::cmp::Reverse(o.end - o.start)));
    out
}

/// Hierarchical delta-debugging on one AST, to a fixpoint or the
/// budget. `build` renders a candidate to the full program text it must
/// be checked as (`None` if malformed). A round keeps every reduction
/// that works alone and applies them all at once (they are pairwise
/// disjoint by construction); if the composite fails, halving recovers
/// a prefix, so a round that found anything always makes progress.
async fn shrink(
    mut current: Expr,
    build: &impl Fn(&Expr) -> Option<String>,
    target: &str,
    judge: Judge,
    calls: &mut usize,
    budget: usize,
    progress: &(dyn Fn(&str) + Sync),
) -> Expr {
    // A candidate must render strictly shorter: several reductions are
    // identities on some nodes, and an identity reproduces the
    // divergence by definition.
    let hits = async |e: &Expr, cur: usize, calls: &mut usize| match build(e) {
        Some(text) if text.len() < cur => {
            *calls += 1;
            judge.key(&text).await.as_deref() == Some(target)
        }
        _ => false,
    };
    while *calls < budget {
        let Some(cur) = build(&current).map(|t| t.len()) else { break };
        let mut kept: Vec<Op> = Vec::new();
        for op in ops(&current) {
            if *calls >= budget {
                break;
            }
            if kept.iter().any(|k| op.start < k.end && k.start < op.end) {
                continue; // overlaps an accepted reduction
            }
            if hits(&op.apply(&current), cur, calls).await {
                kept.push(op);
            }
        }
        if kept.is_empty() {
            break;
        }
        while kept.len() > 1 && !hits(&apply_ops(&current, &kept), cur, calls).await {
            kept.truncate(kept.len() / 2);
        }
        current = apply_ops(&current, &kept);
        if let Some(t) = build(&current) {
            progress(&t);
        }
        // interactive progress only: `minimize-one` runs with stderr null
        eprintln!(
            "  minimize: {} checks, {} bytes",
            calls,
            build(&current).map_or(0, |t| t.len())
        );
    }
    current
}

/// The file-section shrink candidates for one round, most aggressive
/// first: no sections, each module's section pair dropped, each
/// interface dropped alone.
fn file_reductions(files: &[(String, String)]) -> Vec<Vec<(String, String)>> {
    let mut out = Vec::new();
    out.push(Vec::new());
    let stems: Vec<&str> =
        files.iter().filter_map(|(n, _)| n.strip_suffix(".gx")).collect();
    for stem in &stems {
        if stems.len() > 1 {
            out.push(
                files
                    .iter()
                    .filter(|(n, _)| {
                        n != &format!("{stem}.gx") && n != &format!("{stem}.gxi")
                    })
                    .cloned()
                    .collect(),
            );
        }
        let no_intf: Vec<_> =
            files.iter().filter(|(n, _)| n != &format!("{stem}.gxi")).cloned().collect();
        if no_intf.len() != files.len() {
            out.push(no_intf);
        }
    }
    out
}

/// The schedule-shrink candidates for one greedy round, most
/// aggressive first. Caps are never touched.
fn schedule_reductions(s: &schedule::Schedule) -> Vec<schedule::Schedule> {
    let mut out = Vec::new();
    if !s.epochs.is_empty() {
        // the whole header disappears if the body alone reproduces
        out.push(schedule::Schedule::default());
        let mut t = s.clone();
        t.epochs.pop();
        out.push(t);
        if s.epochs.len() > 1 {
            for i in 0..s.epochs.len() - 1 {
                let mut t = s.clone();
                t.epochs.remove(i);
                out.push(t);
            }
        }
        for (i, ep) in s.epochs.iter().enumerate() {
            if ep.len() > 1 {
                for j in 0..ep.len() {
                    let mut t = s.clone();
                    t.epochs[i].remove(j);
                    out.push(t);
                }
            }
        }
        for (i, ep) in s.epochs.iter().enumerate() {
            for (j, (_, v)) in ep.iter().enumerate() {
                let simpler: &[schedule::Lit] = match v {
                    schedule::Lit::I64(_) => {
                        &[schedule::Lit::I64(0), schedule::Lit::I64(1)]
                    }
                    schedule::Lit::F64(_) => {
                        &[schedule::Lit::F64(0.0), schedule::Lit::F64(1.0)]
                    }
                    schedule::Lit::Bool(_) => &[schedule::Lit::Bool(false)],
                };
                for sv in simpler {
                    if sv != v {
                        let mut t = s.clone();
                        t.epochs[i][j].1 = *sv;
                        out.push(t);
                    }
                }
            }
        }
    }
    out
}

/// Verdict of one acceptance check (`--check` semantics: compile +
/// typecheck + analyze, never execute).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TmVerdict {
    Accept,
    Reject(String),
    /// The check didn't finish inside its budget: a measurement failure,
    /// never a flip ([`TmReport::hang`] is the confirmed one).
    Hung,
}

/// One subject's typemorph report: the base verdict, each probe's
/// verdict (probes run only when the base accepts), and the count of
/// candidates the printer failed to round-trip.
pub struct TmReport {
    pub base: TmVerdict,
    pub probes: Vec<Probed<TmVerdict>>,
    pub noparse: usize,
    /// Each must-reject mutant (`mustreject`) and its finding, `None`
    /// when it was refused where its rule says.
    pub rejects: Vec<Probed<Option<String>>>,
    /// The check that hung, when it hung again at the slow budget on a
    /// runtime of its own: the typechecker must stay instant.
    pub hang: Option<Probed<()>>,
}

/// One mutant of a subject and what checking it said.
#[derive(Debug)]
pub struct Probed<V> {
    pub id: String,
    pub mutant: String,
    pub verdict: V,
}

impl TmReport {
    /// The line protocol the `typemorph-one` child writes to its
    /// verdict file.
    pub fn render(&self) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        match &self.base {
            TmVerdict::Accept => s.push_str("BASE accept\n"),
            TmVerdict::Reject(e) => {
                let _ = writeln!(s, "BASE reject {e}");
            }
            TmVerdict::Hung => s.push_str("BASE hung\n"),
        }
        let _ = writeln!(s, "NOPARSE {}", self.noparse);
        let mut flip = |id: &str, head: &str, mutant: &str| {
            let _ = writeln!(s, "FLIP {id} {head}");
            let _ = writeln!(s, "MUTANT {}", escape_line(mutant));
        };
        for p in &self.probes {
            if let TmVerdict::Reject(e) = &p.verdict {
                flip(&p.id, e, &p.mutant);
            }
        }
        for p in &self.rejects {
            if let Some(head) = &p.verdict {
                flip(&p.id, head, &p.mutant);
            }
        }
        if let Some(p) = &self.hang {
            flip(&p.id, "HUNG: the check outlived the slow budget", &p.mutant);
        }
        for p in &self.probes {
            match p.verdict {
                TmVerdict::Accept => {
                    let _ = writeln!(s, "PROBE {} accept", p.id);
                }
                TmVerdict::Hung => {
                    let _ = writeln!(s, "PROBE {} hung", p.id);
                }
                TmVerdict::Reject(_) => (),
            }
        }
        for p in self.rejects.iter().filter(|p| p.verdict.is_none()) {
            let _ = writeln!(s, "REJECT {} ok", p.id);
        }
        s
    }

    /// The probes that, either way, came back a finding.
    pub fn flipped(&self) -> impl Iterator<Item = &str> {
        let probes =
            self.probes.iter().filter(|p| matches!(p.verdict, TmVerdict::Reject(_)));
        let rejects = self.rejects.iter().filter(|p| p.verdict.is_some());
        probes
            .map(|p| p.id.as_str())
            .chain(rejects.map(|p| p.id.as_str()))
            .chain(self.hang.iter().map(|p| p.id.as_str()))
    }
}

/// The normalized head of a rejection: the innermost cause with digit
/// runs collapsed, so positions and fresh-counter ids don't split
/// dedup buckets.
fn tm_error_head(e: &str) -> String {
    let line = e.lines().rev().find(|l| !l.trim().is_empty()).unwrap_or("").trim();
    let mut out = String::new();
    let mut in_digits = false;
    for c in line.chars() {
        if c.is_ascii_digit() {
            if !in_digits {
                out.push('N');
                in_digits = true;
            }
        } else {
            in_digits = false;
            out.push(c);
        }
    }
    out
}

/// Candidates per transform kind a subject's typemorph probes draw.
pub const TM_CAP: usize = 3;

/// The most checks one typemorph subject runs: its base, every
/// transform kind's probes, the typed base, and every must-reject
/// family's.
pub fn tm_checks(cap: usize) -> u32 {
    (2 + (typemorph::TmKind::ALL.len() + mustreject::Family::ALL.len()) * cap) as u32
}

/// One check of a subject's full text.
enum TmCheck {
    /// The text is not a subject.
    Unparsed(String),
    /// The check outlived its budget; the runtime is still busy with it.
    Hung,
    /// The check's answer.
    Done(anyhow::Result<graphix_rt::CheckResult>),
}

/// Check `full` on `ctx`, recording its types when `types`.
async fn tm_check(
    ctx: &TestCtx,
    full: &str,
    types: bool,
    per_check: Duration,
) -> TmCheck {
    let subj = match Subject::parse(full, "test") {
        Ok(s) => s,
        Err(e) => return TmCheck::Unparsed(e),
    };
    let resolver = VfsResolver::new(subj.table.clone());
    let text = graphix_compiler::expr::Load::Text(ArcStr::from(subj.compile_text()));
    let resolvers = vec![resolver.into()];
    let r = match types {
        true => {
            tokio::time::timeout(
                per_check,
                ctx.rt.check_with_types(text, resolvers, None),
            )
            .await
        }
        false => {
            let fut = ctx.rt.check_with_resolvers(text, resolvers, None);
            tokio::time::timeout(per_check, fut).await
        }
    };
    match r {
        Ok(r) => TmCheck::Done(r),
        Err(_) => TmCheck::Hung,
    }
}

/// Run one subject's metamorphic probes, then its must-reject probes,
/// on one warmed runtime (`GXHandle::check_with_resolvers` never
/// executes and restores the env per call; a check never fuses). Probes
/// run only when the base accepts: accept→reject is the finding. A hung
/// check leaves the runtime busy, so nothing after it is measured.
pub async fn typemorph_subject(
    code: &str,
    per_check: Duration,
    cap: usize,
) -> Result<TmReport, String> {
    let (sched, body) = schedule::Schedule::parse(code)?;
    let (cspec, body) = callable::CallSpec::parse(&body)?;
    let (body, files) = files::split(&body)?;
    let (probes, noparse) = typemorph::probes(body, cap);
    let compose = |body_text: &str| {
        let t = sched.render(&files::render(body_text, &files));
        match &cspec {
            Some(c) => c.render(&t),
            None => t,
        }
    };
    let ctx = tm_runtime().await?;
    let accept = |c: TmCheck| match c {
        TmCheck::Unparsed(e) => TmVerdict::Reject(tm_error_head(&e)),
        TmCheck::Hung => TmVerdict::Hung,
        TmCheck::Done(Ok(_)) => TmVerdict::Accept,
        TmCheck::Done(Err(e)) => TmVerdict::Reject(tm_error_head(&format!("{e:?}"))),
    };
    let base = accept(tm_check(&ctx, &compose(body), false, per_check).await);
    let mut results = Vec::new();
    let mut rejects = Vec::new();
    let mut hung = match base {
        TmVerdict::Hung => Some(("base".to_string(), body.to_string())),
        _ => None,
    };
    if base == TmVerdict::Accept {
        for p in &probes {
            let verdict =
                accept(tm_check(&ctx, &compose(&p.body), false, per_check).await);
            if verdict == TmVerdict::Hung {
                hung = Some((p.id(), p.body.clone()));
            }
            results.push(Probed { id: p.id(), mutant: p.body.clone(), verdict });
            if hung.is_some() {
                break;
            }
        }
        if hung.is_none() {
            let modules: Vec<&str> =
                files.iter().filter_map(|(n, _)| n.strip_suffix(".gx")).collect();
            (rejects, hung) =
                must_reject(&ctx, body, &modules, &compose, per_check, cap).await;
        }
    }
    let _ = tokio::time::timeout(Duration::from_secs(5), ctx.shutdown()).await;
    let hang = match hung {
        None => None,
        Some((id, mutant)) => {
            let ctx = tm_runtime().await?;
            let full = compose(&mutant);
            let again = tm_check(&ctx, &full, false, slow_budget(per_check)).await;
            let hung = matches!(again, TmCheck::Hung);
            let _ = tokio::time::timeout(Duration::from_secs(5), ctx.shutdown()).await;
            hung.then_some(Probed { id, mutant, verdict: () })
        }
    };
    Ok(TmReport { base, probes: results, noparse, rejects, hang })
}

/// A runtime for typemorph's checks, warmed by its registration.
async fn tm_runtime() -> Result<TestCtx, String> {
    let (tx, _rx) = mpsc::channel(64);
    let sink = graphix_package_core::PrintSink::default();
    TestCtx::start(
        config(tx, REGISTER, vec![], move |ctx| {
            ctx.libstate.set(sink);
        })
        .map_err(|e| format!("runtime init failed: {e:?}"))?
        .flags(Mode::Interp.flags()),
    )
    .await
    .map_err(|e| format!("runtime init failed: {e:?}"))
}

/// The must-reject half of a subject (`mustreject`): the base checked
/// again with its types, and each mutant its families build. A mutant
/// accepted is a `LEAK`; one refused away from the mutation and its rigid
/// consumer is `MISPLACED`. A mutant whose check hangs ends the families,
/// and comes back (its id and text) beside what they found before it.
async fn must_reject(
    ctx: &TestCtx,
    body: &str,
    modules: &[&str],
    compose: &impl Fn(&str) -> String,
    per_check: Duration,
    cap: usize,
) -> (Vec<Probed<Option<String>>>, Option<(String, String)>) {
    use graphix_compiler::expr::ErrorSite;
    let mut out = Vec::new();
    let TmCheck::Done(Ok(checked)) = tm_check(ctx, &compose(body), true, per_check).await
    else {
        return (out, None);
    };
    let types = mustreject::TypeMap::new(&checked.ide.expr_types, "test");
    for p in mustreject::probes(body, &types, modules, cap) {
        let verdict = match tm_check(ctx, &compose(&p.body), false, per_check).await {
            TmCheck::Unparsed(_) => continue,
            TmCheck::Hung => return (out, Some((p.id(), p.body))),
            TmCheck::Done(Ok(_)) => Some(format!("LEAK: accepted at {}", p.reached)),
            TmCheck::Done(Err(e)) => {
                let site = e.downcast_ref::<ErrorSite>().map(|s| s.expr());
                let right = site.is_some_and(|x| {
                    mustreject::in_module(&x.ori, "test")
                        && p.right_site(mustreject::to_body(x.pos))
                });
                let head = tm_error_head(&format!("{e:?}"));
                match right {
                    true if p.family.refuses_with(&head) => None,
                    true => Some(format!("WRONG REFUSAL: {head}")),
                    false => Some(format!("MISPLACED: {head}")),
                }
            }
        };
        out.push(Probed { id: p.id(), mutant: p.body, verdict });
    }
    (out, None)
}

/// Run the `typemorph-one` child on one subject: its verdict-file text,
/// and the CPU it burned.
pub async fn typemorph_child(
    prog: &str,
    per_check: Duration,
) -> (Result<String, String>, Duration) {
    // every check the subject may run, a hang's confirmation, one warmed
    // init, plus slack
    let deadline =
        per_check * tm_checks(TM_CAP) + slow_budget(per_check) + Duration::from_secs(60);
    let ms = per_check.as_millis().to_string();
    let c =
        run_child("typemorph-one", &[&ms], &[], prog, false, Deadline::Fixed(deadline))
            .await;
    let r = match (c.status, c.verdict) {
        (None, _) => Err("child deadline".to_string()),
        (_, Some(v)) => tm_report(v),
        (Some(s), None) => Err(format!("no verdict ({s})")),
    };
    (r, c.cpu)
}

/// A `typemorph-one` verdict file: its report, or the `HARNESS` error of a
/// subject the child could not run.
fn tm_report(v: String) -> Result<String, String> {
    match v.strip_prefix("HARNESS ") {
        Some(e) => Err(e.trim_end().to_string()),
        None => Ok(v),
    }
}

/// A flip a verdict file reports: the probe, the normalized rejection
/// head (or `LEAK`/`MISPLACED`), and the mutant's source.
struct Flip {
    id: String,
    head: String,
    mutant: Option<String>,
}

impl Flip {
    fn harness(id: &str, head: String) -> Self {
        Flip { id: id.to_string(), head, mutant: None }
    }
}

/// `s` on one line: `\\` and `\n` escaped; `unescape_line` inverts it.
fn escape_line(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\n', "\\n")
}

fn unescape_line(l: &str) -> String {
    let mut out = String::with_capacity(l.len());
    let mut chars = l.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some(c) => out.push(c),
            None => out.push('\\'),
        }
    }
    out
}

fn tm_flips(report: &str) -> Vec<Flip> {
    let mut out: Vec<Flip> = Vec::new();
    for l in report.lines() {
        if let Some((id, head)) = l.strip_prefix("FLIP ").and_then(|l| l.split_once(' '))
        {
            out.push(Flip { id: id.to_string(), head: head.to_string(), mutant: None });
        } else if let Some(m) = l.strip_prefix("MUTANT ")
            && let Some(f) = out.last_mut()
        {
            f.mutant = Some(unescape_line(m));
        }
    }
    out
}

/// One child per program; a subject reporting flips is confirmed by a
/// second fresh child (transforms are deterministic, so the same probe
/// id must flip again). An unconfirmed flip is reported as its own class.
pub async fn typemorph_scan(
    programs: Vec<(String, String)>,
    per_check: Duration,
) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let mut noparse_total = 0usize;
    let job = |i: usize| {
        let prog = programs[i].1.clone();
        async move {
            let r = typemorph_child(&prog, per_check).await.0;
            let confirm = match &r {
                Ok(rep) if !tm_flips(rep).is_empty() => {
                    Some(typemorph_child(&prog, per_check).await.0)
                }
                _ => None,
            };
            (r, confirm)
        }
    };
    windowed(programs.len(), (parallelism() / 2).max(1), job, |i, res| {
        let name = &programs[i].0;
        let (rep, confirm) = match res {
            Err(e) => return out.push((name.clone(), format!("harness: {e}"))),
            Ok((Err(e), _)) => return out.push((name.clone(), format!("harness: {e}"))),
            Ok((Ok(rep), confirm)) => (rep, confirm),
        };
        noparse_total += rep
            .lines()
            .find_map(|l| l.strip_prefix("NOPARSE "))
            .and_then(|n| n.parse::<usize>().ok())
            .unwrap_or(0);
        let rep2 = match confirm {
            None => return,
            Some(Err(e)) => {
                return out.push((name.clone(), format!("harness (confirm): {e}")));
            }
            Some(Ok(rep2)) => rep2,
        };
        let again: std::collections::HashSet<String> =
            tm_flips(&rep2).into_iter().map(|f| f.id).collect();
        for Flip { id, head, mutant } in tm_flips(&rep) {
            let line = if again.contains(&id) {
                let mutant = escape_line(&mutant.unwrap_or_default());
                format!("{id}: {head}\n    mutant: {mutant}")
            } else {
                format!("{id}: UNCONFIRMED (fresh-process flap)")
            };
            out.push((name.clone(), line));
        }
    })
    .await;
    if noparse_total > 0 {
        eprintln!("typemorph: {noparse_total} candidates failed print->parse round trip");
    }
    out
}

/// The regression pins being checked now, so a panic that aborts the
/// process can say which pins it may be.
static PINS_IN_FLIGHT: std::sync::Mutex<BTreeMap<&'static str, usize>> =
    std::sync::Mutex::new(BTreeMap::new());

/// The regression pins being checked now.
pub fn pins_in_flight() -> Vec<&'static str> {
    PINS_IN_FLIGHT.lock().map(|m| m.keys().copied().collect()).unwrap_or_default()
}

/// A pin in [`pins_in_flight`] while this lives.
struct InFlight(&'static str);

impl InFlight {
    fn enter(pin: &'static str) -> Self {
        if let Ok(mut m) = PINS_IN_FLIGHT.lock() {
            *m.entry(pin).or_default() += 1;
        }
        InFlight(pin)
    }
}

impl Drop for InFlight {
    fn drop(&mut self) {
        if let Ok(mut m) = PINS_IN_FLIGHT.lock()
            && let Some(n) = m.get_mut(self.0)
        {
            *n -= 1;
            if *n == 0 {
                m.remove(self.0);
            }
        }
    }
}

/// What `regress` concluded: the pins that now diverge, and every pin's
/// verdict for the outcome manifest.
pub struct Regression {
    pub regressions: Vec<(String, Divergence)>,
    pub verdicts: Vec<(String, Recorded)>,
}

/// Run the embedded regression corpus (every finding under `findings/`)
/// through the oracle, in parallel. An agreement a loaded run cannot vouch
/// for (both contained or both rejected, unless the outcome manifest
/// records that class for the pin, or unsure) is retried alone at 4x,
/// where a still-quiet pin passes on its own character. `bless` retries
/// every one of them, so the verdicts it records are the unloaded ones.
pub async fn run_regression(timeout: Duration, bless: bool) -> Regression {
    let entries = corpus::REGRESSION_CORPUS;
    let recorded = manifest_rows::<Recorded>(OUTCOME_MANIFEST);
    let mut regressions = Vec::new();
    let mut verdicts: Vec<Option<Recorded>> = vec![None; entries.len()];
    let mut suspect: Vec<usize> = Vec::new();
    let job = |i: usize| async move {
        let _pin = InFlight::enter(entries[i].0);
        check_judged(entries[i].1, timeout).await
    };
    windowed(entries.len(), regress_parallelism(), job, |i, res| {
        let (d, r) = match res {
            Ok(r) => r,
            Err(e) => panic!("regress {}: {e}", entries[i].0),
        };
        let trusted = match r.verdict {
            Verdict::Ran | Verdict::Excluded => true,
            Verdict::Contained | Verdict::Rejected => {
                !bless && recorded.get(entries[i].0) == Some(&r)
            }
            Verdict::Unsure => false,
        };
        match d {
            Some(d) => regressions.push((entries[i].0.to_string(), d)),
            None if trusted => verdicts[i] = Some(r),
            None => suspect.push(i),
        }
    })
    .await;
    if !suspect.is_empty() {
        eprintln!(
            "regress: retrying {} untrusted agreement(s) sequentially at full budget",
            suspect.len()
        );
        for i in suspect {
            let (d, r) = check_judged(entries[i].1, timeout * 4).await;
            match d {
                Some(d) => regressions.push((entries[i].0.to_string(), d)),
                None => verdicts[i] = Some(r),
            }
        }
    }
    for (i, r) in verdicts.iter_mut().enumerate() {
        let Some(rec) = r else { continue };
        let d = match unparsed(entries[i].1, rec, timeout * 4).await {
            Some(d) => Some(d),
            None => misdeclared(entries[i].1, rec.verdict, timeout * 4).await,
        };
        if let Some(d) = d {
            regressions.push((entries[i].0.to_string(), d));
            *r = None;
        }
    }
    let verdicts = entries
        .iter()
        .zip(verdicts)
        .filter_map(|((name, _), v)| v.map(|v| (name.to_string(), v)))
        .collect();
    Regression { regressions, verdicts }
}

/// A pin both engines refuse because it does not parse: it pins nothing,
/// whatever it was refused for once.
async fn unparsed(prog: &str, rec: &Recorded, timeout: Duration) -> Option<Divergence> {
    let Recorded { verdict: Verdict::Rejected, .. } = rec else { return None };
    let e =
        check_only(prog, timeout).await.err().filter(|e| e.contains("Parse error"))?;
    let o = Outcome::CompileErr(e);
    Some(Divergence {
        code: prog.to_string(),
        reference: o.clone(),
        tested: o,
        tier: oracle_tier(prog),
        pair: Pair::Unparsed,
    })
}

/// The checked-in outcome manifest: one `verdict<TAB>name` line per
/// corpus program, written by `regress --bless` (then rebuild to embed).
pub static OUTCOME_MANIFEST: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/outcome.manifest"));

/// The rows of a `value<TAB>name` manifest, the name the last field.
fn manifest_rows<V: FromStr>(manifest: &str) -> BTreeMap<&str, V> {
    manifest
        .lines()
        .filter_map(|l| l.rsplit_once('\t'))
        .filter_map(|(v, n)| v.parse().ok().map(|v| (n, v)))
        .collect()
}

/// What one run measured for a pin.
pub enum Measured<'a, V> {
    Value(&'a V),
    /// The measurement failed, with this reason.
    Unreadable(&'a str),
    /// Reported elsewhere (a regression): no line here, and its row is
    /// not stale.
    Elsewhere,
}

/// `live` against a `value<TAB>name` manifest, one line per
/// disagreement: in live order, then the rows whose pin is gone.
/// `changed` words a recorded value the live one disagrees with.
fn manifest_diff<'a, V: FromStr + fmt::Display + 'a>(
    manifest: &str,
    live: impl IntoIterator<Item = (&'a str, Measured<'a, V>)>,
    changed: impl Fn(&str, &V, &V) -> Option<String>,
) -> Vec<String> {
    let mut recorded = manifest_rows::<V>(manifest);
    let mut out = Vec::new();
    for (n, m) in live {
        let rec = recorded.remove(n);
        match (m, rec) {
            (Measured::Elsewhere, _) => (),
            (Measured::Unreadable(e), _) => {
                let last = e.lines().rev().find(|l| !l.trim().is_empty()).unwrap_or(e);
                out.push(format!("unreadable: {n}: {last}"));
            }
            (Measured::Value(v), Some(r)) => out.extend(changed(n, &r, v)),
            (Measured::Value(v), None) => {
                out.push(format!("unrecorded: {n} ({v}) — bless to record"))
            }
        }
    }
    for (n, r) in recorded {
        out.push(format!("stale manifest row: {n} ({r}) — bless to drop"));
    }
    out
}

/// The live verdicts against the manifest. An `unsure` on either side
/// compares equal: it records nothing about the program.
pub fn outcome_mismatches(manifest: &str, r: &Regression) -> Vec<String> {
    let live = r.verdicts.iter().map(|(n, v)| (n.as_str(), Measured::Value(v)));
    let regressed = r.regressions.iter().map(|(n, _)| (n.as_str(), Measured::Elsewhere));
    manifest_diff(manifest, live.chain(regressed), |n, r: &Recorded, v| {
        let unsure = r.verdict == Verdict::Unsure || v.verdict == Verdict::Unsure;
        (r != v && !unsure).then(|| format!("changed outcome: {n}: {r} -> {v}"))
    })
}

/// The marker a corpus pin carries when both engines are meant to
/// reject it (a typechecker refusal pinned as a refusal).
pub const EXPECT_REJECT: &str = "// expect: reject";

/// A pin whose verdict contradicts its `// expect: reject` marker: both
/// engines reject a pin without it, or do not both reject one with it. A rejection
/// agrees with itself, so the marker is what tells the two apart.
async fn misdeclared(
    prog: &str,
    verdict: Verdict,
    timeout: Duration,
) -> Option<Divergence> {
    let marked = prog.lines().any(|l| l.trim() == EXPECT_REJECT);
    let pair = match (marked, verdict) {
        (false, Verdict::Rejected) => Pair::Rejected,
        (true, v) if v != Verdict::Rejected => Pair::Accepted,
        _ => return None,
    };
    let outcome = run_program(prog, Mode::Interp, timeout).await;
    let rejected = matches!(outcome, Outcome::CompileErr(_) | Outcome::RuntimeErr(_));
    if rejected != (pair == Pair::Rejected) {
        return None;
    }
    Some(Divergence {
        code: prog.to_string(),
        reference: outcome.clone(),
        tested: outcome,
        tier: oracle_tier(prog),
        pair,
    })
}

/// Number of programs in the embedded regression corpus.
pub fn regression_corpus_len() -> usize {
    corpus::REGRESSION_CORPUS.len()
}

/// The checked-in fusion-coverage manifest: one `count<TAB>name` line
/// per corpus program, `count` a [`FuseCount`]. `regress` diffs live
/// counts against it so a silent de-fusion regression fails loud;
/// `fusecheck --bless` rewrites the file (then rebuild to embed).
pub static FUSECHECK_MANIFEST: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/fusecheck.manifest"));

/// What a compile-only measurement of a corpus program records.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuseCount {
    /// Regions that fused.
    Fused(u64),
    /// The program's first cycle aborts the runtime by the stack budget,
    /// so it has no count; that it aborts is what the manifest pins.
    BudgetAbort,
}

impl fmt::Display for FuseCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FuseCount::Fused(n) => write!(f, "{n}"),
            FuseCount::BudgetAbort => write!(f, "abort"),
        }
    }
}

impl FromStr for FuseCount {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            "abort" => Ok(FuseCount::BudgetAbort),
            n => n.parse().map(FuseCount::Fused).map_err(|_| ()),
        }
    }
}

/// Fused-region count per corpus program, in corpus order. Jit mode,
/// compile only, so the count cannot depend on run-time pacing. A count
/// that could not be measured is `Err` and must be treated as a gate
/// failure, never as 0.
pub async fn run_fusecheck(
    timeout: Duration,
) -> Vec<(String, Result<FuseCount, String>)> {
    let entries = corpus::REGRESSION_CORPUS;
    let mut counts: Vec<Result<FuseCount, String>> =
        vec![Err("never measured".to_string()); entries.len()];
    let job = |i: usize| async move {
        match compile_with_stats(entries[i].1, Mode::Jit, timeout).await {
            CompileOutcome::Compiled(s) | CompileOutcome::Rejected(_, s) => {
                Ok(FuseCount::Fused(s.fused as u64))
            }
            CompileOutcome::BudgetAborted => Ok(FuseCount::BudgetAbort),
            CompileOutcome::Failed(e) => Err(e),
        }
    };
    // the budget-aborting pins run away in this process toward the cap
    windowed(entries.len(), regress_parallelism(), job, |i, r| {
        counts[i] = r.and_then(|r| r)
    })
    .await;
    entries.iter().zip(counts).map(|((name, _), c)| (name.to_string(), c)).collect()
}

/// Every way the live counts disagree with the `manifest`, one line
/// each: an unreadable count, a program that lost or gained fusion, one
/// the manifest does not record, or a row whose program is gone. Empty
/// means the manifest holds.
pub fn fusecheck_mismatches(
    manifest: &str,
    counts: &[(String, Result<FuseCount, String>)],
) -> Vec<String> {
    let live = counts.iter().map(|(n, c)| {
        let m = match c {
            Ok(c) => Measured::Value(c),
            Err(e) => Measured::Unreadable(e.as_str()),
        };
        (n.as_str(), m)
    });
    manifest_diff(manifest, live, |n, r: &FuseCount, c| match (r, c) {
        _ if r == c => None,
        (FuseCount::Fused(r), FuseCount::Fused(c)) if c < r => {
            Some(format!("LOST fusion: {n}: {r} -> {c}"))
        }
        _ => Some(format!("changed fusion: {n}: {r} -> {c}")),
    })
}

/// The oracle-soundness gate: same program, same mode, twice → identical
/// traces, over every corpus seed plus `iters` generated programs.
/// Returns the programs whose traces disagreed with themselves, tagged
/// with the mode that flaked. Must be empty.
pub async fn selfcheck(
    iters: usize,
    seed: u64,
    timeout: Duration,
) -> Vec<(String, &'static str)> {
    // Subjects are everything with a sound comparison at some tier;
    // tier-Excluded programs are non-subjects. The 100% bar also
    // polices the tier list: a missing marker shows up as a flake.
    let deterministic = |p: &str| oracle_tier(p) != OracleTier::Excluded;
    let mut rng = mutate::Rng::new(seed);
    let mut progs: Vec<String> = corpus::all_seeds()
        .iter()
        .filter(|s| deterministic(s))
        .map(|s| s.to_string())
        .collect();
    // half single-burst, half scheduled reactive: the injection driver
    // is part of the oracle
    for i in 0..iters {
        if i % 2 == 0 {
            progs.push(generate::gen_program(&mut rng));
        } else {
            progs.push(generate::reactive::gen_reactive_program(&mut rng));
        }
    }
    let mut flaky = Vec::new();
    let mut done = 0usize;
    let mut inconclusive = 0usize;
    let job = |i: usize| {
        let prog = progs[i].clone();
        async move { selfcheck_isolated(&prog, timeout).await }
    };
    windowed(progs.len(), parallelism(), job, |i, res| {
        let modes = res.unwrap_or_else(|e| {
            eprintln!("selfcheck worker panicked: {e}");
            vec!["panicked"]
        });
        // budget-limited subjects are counted, never failed on
        for mode in modes {
            if mode == "inconclusive" {
                inconclusive += 1;
            } else {
                // streamed as they land: a killed run must not take the list
                eprintln!("FLAKY under {mode}: {}", progs[i].replace('\n', "\\n"));
                flaky.push((progs[i].clone(), mode));
            }
        }
        done += 1;
        if done % 200 == 0 {
            eprintln!(
                "  …{done}/{} selfchecked, {} flaky, {inconclusive} inconclusive",
                progs.len(),
                flaky.len()
            );
        }
    })
    .await;
    if inconclusive > 0 {
        eprintln!(
            "selfcheck: {inconclusive}/{} subject(s) inconclusive — timed out at 4x \
             the budget on the confirm pair, so determinism was not measured for them",
            progs.len()
        );
    }
    flaky
}

/// What a fuzz campaign found.
#[derive(Debug, Default, Clone)]
pub struct FuzzStats {
    /// Mutants that were generated and run through the oracle.
    pub run: usize,
    /// Of those, the ones both builds refused: what a generator template
    /// that turned ill-typed wastes.
    pub refused: usize,
    /// Confirmed divergences (including duplicates of already-saved bugs).
    pub divergences: usize,
    /// Mutants that KILLED their (isolated) evaluator process — signal
    /// death, abort, or a wedged child (including duplicates).
    pub crashes: usize,
    /// Ring admissions: agreeing, both-modes-ran mutants whose AST shape
    /// was novel this campaign (0 in generate lanes).
    pub novel: usize,
}

/// A persistent, deduplicated divergence corpus on disk. Loaded once at
/// startup, then grown live: each new divergence is minimized, deduped
/// by its minimized text and written to its own `.gx` immediately.
/// Thread-safe.
pub struct Corpus {
    dir: std::path::PathBuf,
    seen: std::sync::Mutex<std::collections::HashSet<String>>,
    /// Findings claimed per [`Finding::key`], this campaign's and those on
    /// disk.
    keys: std::sync::Mutex<AHashMap<String, usize>>,
    counter: std::sync::atomic::AtomicUsize,
}

const KEY_LINE: &str = "// key: ";

impl Corpus {
    /// Load every `*.gx` already in `dir`, keying the dedup set on each
    /// file's minimized program. Creates `dir` if absent.
    pub fn load(dir: &std::path::Path) -> Self {
        let _ = std::fs::create_dir_all(dir);
        let mut seen = std::collections::HashSet::new();
        let mut keys: AHashMap<String, usize> = AHashMap::new();
        let mut max_idx = 0usize;
        if let Ok(rd) = std::fs::read_dir(dir) {
            for ent in rd.flatten() {
                let path = ent.path();
                if path.extension().and_then(|e| e.to_str()) != Some("gx") {
                    continue;
                }
                if let Ok(body) = std::fs::read_to_string(&path) {
                    if let Some(k) = body.lines().find_map(|l| l.strip_prefix(KEY_LINE)) {
                        *keys.entry(unescape_line(k)).or_default() += 1;
                    }
                    if let Some(m) = extract_minimized(&body) {
                        seen.insert(m);
                    } else if let Some(class) = typeflip_class_of(&body) {
                        seen.insert(class);
                    } else if let Some((_, p)) = body.split_once("// mutant:\n") {
                        // crash finding: dedup by `record_crash`'s key
                        seen.insert(crash_key(p));
                    }
                }
                if let Some(n) = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .and_then(|s| {
                        s.strip_prefix("divergence_")
                            .or_else(|| s.strip_prefix("crash_"))
                            .or_else(|| s.strip_prefix("typeflip_"))
                    })
                    .and_then(|s| s.parse::<usize>().ok())
                {
                    max_idx = max_idx.max(n + 1);
                }
            }
        }
        Corpus {
            dir: dir.to_path_buf(),
            seen: std::sync::Mutex::new(seen),
            keys: std::sync::Mutex::new(keys),
            counter: std::sync::atomic::AtomicUsize::new(max_idx),
        }
    }

    /// Number of distinct findings in the corpus.
    pub fn len(&self) -> usize {
        self.seen.lock().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Claim a minimization for `key`: false once [`MINIMIZE_PER_KEY`]
    /// findings have it.
    pub fn claim(&self, key: &str) -> bool {
        let mut keys = self.keys.lock().unwrap();
        let n = keys.entry(key.to_string()).or_default();
        *n += 1;
        *n <= MINIMIZE_PER_KEY
    }

    /// Record a divergence if its minimized form is new. Returns `true`
    /// when newly written (caller prints it), `false` for a duplicate.
    /// The dedup key is the minimized text, so distinct root causes get
    /// distinct files while many raw mutants that reduce to the same
    /// canonical repro collapse to one.
    pub fn record(&self, f: &Finding, mutant: &str, minimized: &str) -> bool {
        let key = minimized.trim().to_string();
        {
            let mut seen = self.seen.lock().unwrap();
            if !seen.insert(key) {
                return false;
            }
        }
        let n = self.counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        // The mutant is comment data: its newlines are escaped so a
        // multi-line mutant cannot land a bare program line mid-header.
        // The outcome lines are clipped by render; the traces are
        // reproducible from the program text below.
        let (la, lb) = &f.labels;
        let body = format!(
            "// bisect: {}\n{KEY_LINE}{}\n// {la}: {}\n// {lb}: {}\n\
             // mutant: {}\n// minimized:\n{}\n",
            f.bisect,
            escape_line(&f.key),
            f.reference,
            f.tested,
            mutant.replace('\n', "\\n"),
            minimized,
        );
        // a write failure is a broken harness: die loudly
        if let Err(e) =
            std::fs::write(self.dir.join(format!("divergence_{n:06}.gx")), body)
        {
            eprintln!("FATAL fuzz harness: cannot write finding: {e}");
            std::process::exit(2);
        }
        true
    }

    /// Record a process-killing program under its claim key (its
    /// [`crash_claim`]), dedup'd by its minimized form when it has one,
    /// else by its normalized text. Crash findings must not be promoted to
    /// `findings/` until fixed: regress runs in-process.
    pub fn record_crash(
        &self,
        prog: &str,
        status: &str,
        claim: &str,
        minimized: Option<&str>,
    ) -> bool {
        let key = minimized.map_or_else(|| crash_key(prog), |m| m.trim().to_string());
        {
            let mut seen = self.seen.lock().unwrap();
            if !seen.insert(key) {
                return false;
            }
        }
        let n = self.counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let program = match minimized {
            Some(m) => {
                format!("// mutant: {}\n// minimized:\n{m}", prog.replace('\n', "\\n"))
            }
            None => format!("// mutant:\n{prog}"),
        };
        let body = format!(
            "// CRASH: child {status}\n{KEY_LINE}{}\n\
             // do not promote to findings/ until fixed (regress runs in-process)\n\
             {program}\n",
            escape_line(claim),
        );
        if let Err(e) = std::fs::write(self.dir.join(format!("crash_{n:06}.gx")), body) {
            eprintln!("FATAL fuzz harness: cannot write finding: {e}");
            std::process::exit(2);
        }
        true
    }

    /// Record a confirmed acceptance flip if its class is new. The file
    /// is the subject under a comment header, so `graphix-fuzz typemorph
    /// <file>` reproduces it as it stands.
    pub fn record_typeflip(
        &self,
        prog: &str,
        id: &str,
        head: &str,
        mutant: Option<&str>,
    ) -> bool {
        let class = typeflip_class(id, head);
        {
            let mut seen = self.seen.lock().unwrap();
            if !seen.insert(class.clone()) {
                return false;
            }
        }
        let n = self.counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let mutant = mutant
            .map(|m| format!("// mutant: {}\n", escape_line(m)))
            .unwrap_or_default();
        let body = format!(
            "// typeflip {id}: {head}\n{TYPEFLIP_CLASS}{class}\n{mutant}{prog}\n"
        );
        if let Err(e) = std::fs::write(self.dir.join(format!("typeflip_{n:06}.gx")), body)
        {
            eprintln!("FATAL fuzz harness: cannot write finding: {e}");
            std::process::exit(2);
        }
        true
    }
}

/// The typeflip dedup key: the transform kind (the probe id without its
/// site) and the normalized rejection head. Shared by `record_typeflip`
/// and `Corpus::load`.
fn typeflip_class(id: &str, head: &str) -> String {
    let kind = id.split_once('#').map_or(id, |(k, _)| k);
    format!("TYPEFLIP:{kind}: {head}")
}

const TYPEFLIP_CLASS: &str = "// class: ";

fn typeflip_class_of(body: &str) -> Option<String> {
    body.lines().find_map(|l| l.strip_prefix(TYPEFLIP_CLASS)).map(str::to_string)
}

/// Extract the minimized program (the text after the `// minimized:`
/// marker) from a recorded divergence file, trimmed — the dedup key.
fn extract_minimized(body: &str) -> Option<String> {
    body.split_once("// minimized:\n").map(|(_, m)| m.trim().to_string())
}

/// The crash-dedup key: the program with every digit run collapsed to
/// one `N`, so literal variants of one crash shape share a slot. Shared
/// by `record_crash` and `Corpus::load`.
fn crash_key(prog: &str) -> String {
    format!("CRASH:{}", collapse_numbers(prog.trim()))
}

/// `s` with every digit run collapsed to one `N`.
fn collapse_numbers(s: &str) -> String {
    let mut key = String::with_capacity(s.len());
    let mut in_digits = false;
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c.is_ascii_digit()
            || (c == '-' && chars.peek().is_some_and(|n| n.is_ascii_digit()))
        {
            // a leading `-` folds into the digit run; a `-` not followed
            // by a digit keys literally
            if !in_digits {
                key.push('N');
                in_digits = true;
            }
        } else {
            in_digits = false;
            key.push(c);
        }
    }
    key
}

/// Source-A campaign: mutate corpus seeds and run each mutant through the
/// oracle, recording new divergences into `corpus` as they're found.
/// Deterministic in `seed`. `iters = None` runs forever.
pub async fn fuzz(
    iters: Option<usize>,
    seed: u64,
    timeout: Duration,
    corpus: &std::sync::Arc<Corpus>,
) -> FuzzStats {
    one_source(SourceKind::Fuzz, iters, seed, timeout, corpus).await
}

/// A campaign of one source through [`run_aggregator`].
async fn one_source(
    kind: SourceKind,
    iters: Option<usize>,
    seed: u64,
    timeout: Duration,
    corpus: &std::sync::Arc<Corpus>,
) -> FuzzStats {
    let weights = SOURCE_KINDS.map(|k| if k == kind { 1.0 } else { 0.0 });
    let mut stats = run_aggregator(corpus, seed, iters, timeout, weights).await;
    let i = SOURCE_KINDS.iter().position(|k| *k == kind).expect("a source kind");
    stats.swap_remove(i).1
}

/// Run `job(i)` for every `i` in `0..count`, at most `par` at a time,
/// and hand each result to `on_result` as it lands. A job that panicked
/// arrives as `Err` with the panic.
pub async fn windowed<T, Fut>(
    count: usize,
    par: usize,
    mut job: impl FnMut(usize) -> Fut,
    mut on_result: impl FnMut(usize, Result<T, String>),
) where
    T: Send + 'static,
    Fut: future::Future<Output = T> + Send + 'static,
{
    let mut set = tokio::task::JoinSet::new();
    let mut index: AHashMap<tokio::task::Id, usize> = AHashMap::new();
    let mut next = 0usize;
    loop {
        while next < count && set.len() < par.max(1) {
            index.insert(set.spawn(job(next)).id(), next);
            next += 1;
        }
        let (id, r) = match set.join_next_with_id().await {
            None => break,
            Some(Ok((id, v))) => (id, Ok(v)),
            Some(Err(e)) => (e.id(), Err(e.to_string())),
        };
        on_result(index.remove(&id).expect("a spawned job's id"), r);
    }
}

/// How many checks to keep in flight. The oracle is mostly wait
/// (runtime spin-up, quiescence), so the cores are oversubscribed 8x.
fn parallelism() -> usize {
    // `GRAPHIX_FUZZ_PAR` sets the checks in flight, so concurrent
    // campaigns can share a box
    if let Some(n) = std::env::var("GRAPHIX_FUZZ_PAR")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|n| *n > 0)
    {
        return n;
    }
    std::thread::available_parallelism().map(|n| n.get() * 8).unwrap_or(16)
}

/// Physical memory, best effort. `None` on an unreadable platform —
/// callers fall back conservatively.
fn total_memory_bytes() -> Option<u64> {
    #[cfg(target_os = "linux")]
    {
        let s = std::fs::read_to_string("/proc/meminfo").ok()?;
        let kb = s
            .lines()
            .find(|l| l.starts_with("MemTotal:"))?
            .split_whitespace()
            .nth(1)?
            .parse::<u64>()
            .ok()?;
        Some(kb * 1024)
    }
    #[cfg(target_os = "macos")]
    {
        let out = std::process::Command::new("sysctl")
            .args(["-n", "hw.memsize"])
            .output()
            .ok()?;
        String::from_utf8(out.stdout).ok()?.trim().parse::<u64>().ok()
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    {
        None
    }
}

/// [`parallelism`] bounded by memory for the in-process regress gate:
/// each slot runs both engines in this process and the runaway pins grow
/// toward the stack budget, so 3GB per slot against half of RAM. The
/// cap applies even under an explicit `GRAPHIX_FUZZ_PAR`, which shares
/// child-process load, not this process's memory.
fn regress_parallelism() -> usize {
    let mem = total_memory_bytes().unwrap_or(16 << 30);
    parallelism().min((((mem / 2) / (3 << 30)) as usize).max(2))
}

/// Generate valid programs from scratch (type-directed) and run each
/// through the oracle. Deterministic in `seed`.
pub async fn generate_campaign(
    iters: Option<usize>,
    seed: u64,
    timeout: Duration,
    corpus: &std::sync::Arc<Corpus>,
    reactive: bool,
) -> FuzzStats {
    let kind = if reactive { SourceKind::Reactive } else { SourceKind::Generate };
    one_source(kind, iters, seed, timeout, corpus).await
}

/// What one pool slot concluded about a program.
enum PoolResult {
    Agree,
    Diverge(Finding),
    /// The isolated child died (signal / abort / hang). String = wait
    /// status + stderr tail.
    Crash(String),
}

/// Path used to re-exec ourselves for child processes. On Linux
/// /proc/self/exe resolves to the running binary even after the file on
/// disk is replaced mid-campaign; `current_exe()` returns a "(deleted)"
/// path then and every spawn ENOENTs.
fn child_exe() -> std::path::PathBuf {
    #[cfg(target_os = "linux")]
    return std::path::PathBuf::from("/proc/self/exe");
    #[cfg(not(target_os = "linux"))]
    return std::env::current_exe().expect("current_exe");
}

/// Compile threads, and evaluation workers, per child. A campaign runs a
/// child per slot, and a fuzzed program is too small for its compile to
/// gain from more; every thread costs the child its own allocator arena
/// and pools. Two keep the compile's tasks and link batches concurrent.
const CHILD_COMPILE_THREADS: &str = "2";

/// Spawn a harness child. A spawn failure (fd or process exhaustion)
/// is a broken harness, not a program's doing: recording it would flood
/// the corpus at instant-fail speed, so the campaign dies.
fn spawn_child(cmd: &mut tokio::process::Command) -> tokio::process::Child {
    cmd.spawn().unwrap_or_else(|e| {
        eprintln!("FATAL fuzz harness: child spawn failed: {e}");
        std::process::exit(2)
    })
}

/// What a harness child left behind.
struct ChildRun {
    /// `None` when it outlived its deadline (and was killed) or was lost.
    status: Option<std::process::ExitStatus>,
    stderr: String,
    /// Its verdict file, if it wrote one.
    verdict: Option<String>,
    cpu: Duration,
}

/// How long a harness child may run.
enum Deadline {
    Fixed(Duration),
    /// As long as its verdict file keeps growing within the window.
    Stall(Duration),
}

/// Run `graphix-fuzz <sub> <verdict file> <args>` in a sandbox of its own
/// with `input` on its stdin. The verdict comes back in the file, never
/// the exit status, which a subject can set (`sys::exit`).
async fn run_child(
    sub: &str,
    args: &[&str],
    env: &[(&str, &str)],
    input: &str,
    stderr: bool,
    deadline: Deadline,
) -> ChildRun {
    use std::process::Stdio;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    let mut cmd = child_command();
    let sandbox = sandbox(&mut cmd);
    let verdict = sandbox.path().join("verdict");
    cmd.arg(sub)
        .arg(&verdict)
        .args(args)
        .envs(env.iter().copied())
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(if stderr { Stdio::piped() } else { Stdio::null() })
        .kill_on_drop(true);
    let mut child = spawn_child(&mut cmd);
    if let Some(mut stdin) = child.stdin.take() {
        // a write error means the child died instantly; the wait sees it
        let _ = stdin.write_all(input.as_bytes()).await;
    }
    let reader = child.stderr.take().map(|mut e| {
        tokio::spawn(async move {
            let mut buf = Vec::new();
            let _ = e.read_to_end(&mut buf).await;
            buf
        })
    });
    let status = match deadline {
        Deadline::Fixed(d) => match tokio::time::timeout(d, child.wait()).await {
            Ok(r) => r.ok(),
            Err(_) => {
                let _ = child.kill().await;
                None
            }
        },
        Deadline::Stall(window) => {
            let mut last = 0u64;
            loop {
                tokio::select! {
                    r = child.wait() => break r.ok(),
                    _ = tokio::time::sleep(window) => {
                        let len = std::fs::metadata(&verdict).map(|m| m.len()).unwrap_or(0);
                        if len == last {
                            let _ = child.kill().await;
                            break None;
                        }
                        last = len;
                    }
                }
            }
        }
    };
    // a grandchild may hold the pipe open past the child's exit
    let stderr = match reader {
        Some(r) => match tokio::time::timeout(Duration::from_secs(2), r).await {
            Ok(Ok(buf)) => String::from_utf8_lossy(&buf).into_owned(),
            _ => String::new(),
        },
        None => String::new(),
    };
    ChildRun {
        status,
        stderr,
        verdict: std::fs::read_to_string(&verdict).ok(),
        cpu: child_cpu(sandbox.path()),
    }
}

/// A child of this binary, its compile and evaluation threads capped
/// unless the caller set them.
fn child_command() -> tokio::process::Command {
    let mut cmd = tokio::process::Command::new(child_exe());
    // a select no arm matches is a hole in the check: a crash, a finding
    cmd.env("GRAPHIX_ABORT_ON_NO_MATCH", "1");
    if std::env::var_os("RAYON_NUM_THREADS").is_none() {
        cmd.env("RAYON_NUM_THREADS", CHILD_COMPILE_THREADS);
    }
    if std::env::var_os("GRAPHIX_EVAL_THREADS").is_none() {
        cmd.env("GRAPHIX_EVAL_THREADS", CHILD_COMPILE_THREADS);
    }
    if let Some(o) = std::env::var_os("GRAPHIX_FUZZ_CHILD_ASAN_OPTIONS") {
        cmd.env("ASAN_OPTIONS", o);
    }
    // glibc reserves 64 MB of address space per thread's arena, a gigabyte
    // over a child's threads, which the address-space cap would count as
    // the program's
    if std::env::var_os("MALLOC_ARENA_MAX").is_none() {
        cmd.env("MALLOC_ARENA_MAX", "2");
    }
    cmd
}

/// CPU (user + system) this process has burned so far: the soak
/// scheduler's currency. A slot is not a core, and the CPU a slot draws
/// differs per source, so CPU shares are allocated on measured burn.
pub fn self_cpu() -> Duration {
    // SAFETY: zeroed is a valid rusage; getrusage cannot fail for RUSAGE_SELF.
    let mut ru: libc::rusage = unsafe { std::mem::zeroed() };
    if unsafe { libc::getrusage(libc::RUSAGE_SELF, &mut ru) } != 0 {
        return Duration::ZERO;
    }
    let tv = |t: libc::timeval| {
        Duration::new(
            t.tv_sec.max(0) as u64,
            (t.tv_usec.max(0) as u32).min(999_999) * 1000,
        )
    };
    tv(ru.ru_utime) + tv(ru.ru_stime)
}

/// The file a worker child drops its own [`self_cpu`] in, beside its
/// verdict file. Tokio reaps asynchronously and `RUSAGE_CHILDREN` is
/// process-wide, so neither attributes CPU to a source.
const CPU_REPORT: &str = "cpu-usage";

/// Called by every worker child arm on its way out, with the verdict
/// file the parent named.
pub fn report_self_cpu(verdict: &std::path::Path) {
    let report = verdict.with_file_name(CPU_REPORT);
    let _ = std::fs::write(report, self_cpu().as_micros().to_string());
}

/// Read back what a child reported. Absent reads as zero, which the
/// scheduler self-corrects on the next completion.
fn child_cpu(sandbox: &std::path::Path) -> Duration {
    std::fs::read_to_string(sandbox.join(CPU_REPORT))
        .ok()
        .and_then(|s| s.trim().parse::<u64>().ok())
        .map(Duration::from_micros)
        .unwrap_or_default()
}

/// A child's address-space cap (`GRAPHIX_FUZZ_MEM_LIMIT`, 0 = none).
fn mem_limit() -> u64 {
    std::env::var("GRAPHIX_FUZZ_MEM_LIMIT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(8 << 30)
}

/// Cap this process like the children, unless the environment says
/// otherwise, so an in-process runaway recursion aborts to a containment
/// instead of growing stack segments or activations.
pub fn set_budgets() {
    if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
        graphix_compiler::set_stack_budget(1 << 30);
    }
    if std::env::var_os("GRAPHIX_ACTIVATION_BUDGET").is_none() {
        graphix_compiler::set_activation_budget(ACTIVATION_BUDGET);
    }
}

/// The child's own address-space limit, applied by the child itself
/// rather than through a `pre_exec` hook so the parent keeps
/// posix_spawn's vfork fast path. Called once at startup by every
/// process the harness spawns; a no-op when `GRAPHIX_FUZZ_MEM_LIMIT` is 0.
pub fn apply_mem_limit() {
    #[cfg(unix)]
    {
        // Children only: the driver's address space is far larger than
        // any child's, so the child's ceiling would kill it outright.
        if std::env::var_os("GRAPHIX_FUZZ_SANDBOXED").is_none() {
            return;
        }
        let limit = mem_limit();
        if limit > 0 {
            #[cfg(target_os = "macos")]
            watch_resident(limit);
            #[cfg(not(target_os = "macos"))]
            {
                let rl = libc::rlimit { rlim_cur: limit, rlim_max: limit };
                // best effort: a hard limit already below `limit` makes
                // this EPERM, and the subject timeout is the real guard
                unsafe { libc::setrlimit(libc::RLIMIT_AS, &rl) };
            }
        }
    }
}

/// macOS enforces no address-space limit (`RLIMIT_AS` aliases the
/// unenforced `RLIMIT_RSS`), so a darwin child watches its own resident
/// size and aborts past `limit`, as an allocation past the cap aborts
/// elsewhere.
#[cfg(target_os = "macos")]
#[allow(deprecated)]
fn watch_resident(limit: u64) {
    std::thread::spawn(move || {
        loop {
            std::thread::sleep(Duration::from_millis(100));
            // SAFETY: zeroed is a valid mach_task_basic_info, and task_info
            // writes at most `count` words into it.
            let mut info: libc::mach_task_basic_info = unsafe { std::mem::zeroed() };
            let mut count = libc::MACH_TASK_BASIC_INFO_COUNT;
            let r = unsafe {
                libc::task_info(
                    libc::mach_task_self(),
                    libc::MACH_TASK_BASIC_INFO,
                    &mut info as *mut libc::mach_task_basic_info as libc::task_info_t,
                    &mut count,
                )
            };
            let resident = info.resident_size;
            if r == libc::KERN_SUCCESS && resident > limit {
                eprintln!(
                    "memory limit: {resident} bytes resident, past the {limit}-byte cap"
                );
                std::process::abort();
            }
        }
    });
}

/// The stack budget aborts a runaway recursion (a Timeout, like the
/// deadline) before a box of workers runs out of memory; the address
/// space limit is applied by the child itself ([`apply_mem_limit`]).
fn child_resource_env(cmd: &mut tokio::process::Command) {
    cmd.env("GRAPHIX_FUZZ_SANDBOXED", "1");
    if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
        cmd.env("GRAPHIX_STACK_BUDGET", (1u64 << 30).to_string());
    }
    if std::env::var_os("GRAPHIX_ACTIVATION_BUDGET").is_none() {
        cmd.env("GRAPHIX_ACTIVATION_BUDGET", ACTIVATION_BUDGET.to_string());
    }
}

/// The live activations a fuzzed program may hold: a node-walked
/// activation holds about 20 KB of heap, so this bounds a runaway
/// recursion's near 1 GB, and it trips well before the 1 GiB stack budget
/// would (a recursion's frames reach that between 100k and 300k), so
/// containment follows the program, never the thread or the fork.
pub const ACTIVATION_BUDGET: u64 = 50_000;

/// Give a worker child a parent-owned sandbox (generated programs write
/// files at arbitrary relative paths, and the worker arms exit via
/// `process::exit`, so a child-owned tempdir would leak). The child runs
/// in its `work` directory, so the files the parent reads back (the
/// verdict, the CPU report) sit one level up, where no relative path a
/// subject writes lands. The guard removes the dir on drop; declare it
/// before the child handle so `kill_on_drop` reaps the child first. A
/// tempdir failure is a broken harness and exits the process.
fn sandbox(cmd: &mut tokio::process::Command) -> tempfile::TempDir {
    let made = tempfile::tempdir().and_then(|d| {
        let work = d.path().join("work");
        std::fs::create_dir(&work)?;
        cmd.current_dir(work);
        Ok(d)
    });
    match made {
        Ok(d) => {
            child_resource_env(cmd);
            d
        }
        Err(e) => {
            eprintln!("FATAL fuzz harness: sandbox tempdir failed: {e}");
            std::process::exit(2)
        }
    }
}

/// The per-subject determinism check, the `selfcheck-one` child body:
/// each mode run twice concurrently at the program's oracle tier, with
/// a sequential confirm-retry at 4x before flagging. Returns the modes
/// that stayed flaky.
pub async fn selfcheck_one(prog: &str, timeout: Duration) -> Vec<&'static str> {
    let tier = oracle_tier(prog);
    let mut bad = Vec::new();
    for (&(mode, labels), &route) in SELFCHECK_ENGINES
        .iter()
        .flat_map(|e| session_routes(prog).iter().map(move |r| (e, r)))
    {
        // the strength an engine pair on this route compares at, and the
        // engine's label there
        let (pair, label) = match route {
            Route::InLanguage => (Pair::Engine, labels[0]),
            Route::Dispatch => (Pair::EngineDispatch, labels[1]),
        };
        let strength = Strength::of(pair, tier);
        let (a, b) = tokio::join!(
            run_program_routed(prog, mode, route, timeout),
            run_program_routed(prog, mode, route, timeout),
        );
        if a.agrees_at(&b, strength) {
            continue;
        }
        // Two runs that finished and differ prove nondeterminism. A Timeout
        // or a RuntimeErr may be the budget under load: confirm at 4x.
        let budget_bound =
            |o: &Outcome| matches!(o, Outcome::Timeout(_) | Outcome::RuntimeErr(_));
        if budget_bound(&a) || budget_bound(&b) {
            let big = timeout * 4;
            let a2 = run_program_routed(prog, mode, route, big).await;
            let b2 = run_program_routed(prog, mode, route, big).await;
            if a2.agrees_at(&b2, strength) {
                continue;
            }
            if matches!(a2, Outcome::Timeout(_)) || matches!(b2, Outcome::Timeout(_)) {
                // inconclusive at this budget, not flaky
                bad.push("inconclusive");
                continue;
            }
        }
        bad.push(label);
    }
    bad
}

/// The engines a selfcheck child runs, each with its label on the
/// in-language route and on the dispatch route.
const SELFCHECK_ENGINES: [(Mode, [&str; 2]); 2] =
    [(Mode::Interp, ["interp", "interp-dispatch"]), (Mode::Jit, ["jit", "jit-dispatch"])];

/// What a selfcheck child reports, one per line of its verdict file.
fn selfcheck_label(line: &str) -> Option<&'static str> {
    SELFCHECK_ENGINES
        .iter()
        .flat_map(|(_, labels)| labels)
        .chain(&["inconclusive"])
        .find(|l| **l == line)
        .copied()
}

/// Run one selfcheck subject in a child process. Isolation is for
/// memory as much as crash containment: every in-process `run_program`
/// keeps its context's JIT pages, so the child pays the leak and exits.
async fn selfcheck_isolated(prog: &str, timeout: Duration) -> Vec<&'static str> {
    // each engine on each route: a concurrent pair, then two confirms at
    // 4x, in sequence
    let combos = 2 * session_routes(prog).len() as u32;
    let deadline = Deadline::Fixed(timeout * 9 * combos + Duration::from_secs(30));
    // a dead or wedged child left no verdict: nondeterminism by definition
    match run_child("selfcheck-one", &[], &[], prog, false, deadline).await.verdict {
        Some(v) => v.lines().map(|l| selfcheck_label(l).unwrap_or("crash")).collect(),
        None => vec!["crash"],
    }
}

/// Normalize a CLIF dump for structural comparison across processes:
/// drop log lines, blind pointer-magnitude constants (ASLR), and
/// canonicalize per-process counters (ExprIds, FuncIds, wrapper and
/// lambda ids, abstract and tvar ids) to first-seen order. What remains
/// is the structural shape.
pub fn normalize_clif(s: &str) -> String {
    let mut ids: AHashMap<String, usize> = AHashMap::new();
    let mut out = String::with_capacity(s.len());
    for line in s.lines() {
        // env_logger lines
        if line.starts_with('[') {
            continue;
        }
        let line = &{
            let mut r = String::with_capacity(line.len());
            let mut rest = line;
            'outer: loop {
                let mut best: Option<(usize, &str)> = None;
                for pat in ["ExprId(", "u0:", "kir_", "lambda#", "<abstract#", "'_"] {
                    if let Some(pos) = rest.find(pat) {
                        if best.map_or(true, |(b, _)| pos < b) {
                            best = Some((pos, pat));
                        }
                    }
                }
                let Some((pos, pat)) = best else {
                    r.push_str(rest);
                    break 'outer;
                };
                let (pre, tail) = rest.split_at(pos + pat.len());
                r.push_str(pre);
                let end = tail
                    .char_indices()
                    .find(|(_, c)| !c.is_ascii_digit())
                    .map(|(i, _)| i)
                    .unwrap_or(tail.len());
                if end == 0 {
                    // pattern not followed by digits
                    rest = tail;
                    continue;
                }
                let key = format!("{pat}{}", &tail[..end]);
                let next = ids.len();
                let k = *ids.entry(key).or_insert(next);
                r.push_str(&format!("#{k}"));
                rest = &tail[end..];
            }
            r
        };
        let mut chars = line.char_indices().peekable();
        while let Some((i, c)) = chars.next() {
            if c == '0' && matches!(chars.peek(), Some((_, 'x'))) {
                chars.next();
                let mut n = 0;
                while let Some(&(_, h)) = chars.peek() {
                    if h.is_ascii_hexdigit() || h == '_' {
                        chars.next();
                        n += 1;
                    } else {
                        break;
                    }
                }
                if n >= 8 {
                    out.push_str("PTR");
                } else {
                    let end = chars.peek().map(|&(j, _)| j).unwrap_or(line.len());
                    out.push_str(&line[i..end]);
                }
            } else if c.is_ascii_digit() {
                let mut n = 1;
                while let Some(&(_, d)) = chars.peek() {
                    if d.is_ascii_digit() {
                        chars.next();
                        n += 1;
                    } else {
                        break;
                    }
                }
                if n >= 9 {
                    out.push_str("BIGNUM");
                } else {
                    let end = chars.peek().map(|&(j, _)| j).unwrap_or(line.len());
                    out.push_str(&line[i..end]);
                }
            } else {
                out.push(c);
            }
        }
        out.push('\n');
    }
    out
}

/// The first line where two normalized dumps disagree, for the flap
/// report.
fn first_clif_difference(a: &str, b: &str) -> String {
    for (i, (la, lb)) in a.lines().zip(b.lines()).enumerate() {
        if la != lb {
            return format!("line {}: `{la}` vs `{lb}`", i + 1);
        }
    }
    format!("length: {} vs {} lines", a.lines().count(), b.lines().count())
}

/// The determinism gate: run `prog` to quiescence in two fresh child
/// processes with GRAPHIX_DUMP_CLIF=1 and compare the normalized dumps
/// and verdicts. Fusion shape must be a pure function of the program
/// text. A `timeout` verdict on either side skips the pair.
/// `Some(detail)` = flap.
pub async fn detcheck_one_pair(prog: &str, timeout: Duration) -> Option<String> {
    async fn compile_child(
        prog: &str,
        timeout: Duration,
    ) -> std::result::Result<(Option<String>, String), String> {
        let deadline = Deadline::Fixed(timeout * 2 + Duration::from_secs(30));
        let env = [("GRAPHIX_DUMP_CLIF", "1")];
        let c = run_child("detcheck-one", &[], &env, prog, true, deadline).await;
        match c.status {
            None => Err("HANG (compile)".to_string()),
            Some(_) => Ok((c.verdict, normalize_clif(&c.stderr))),
        }
    }
    let (a, b) = tokio::join!(compile_child(prog, timeout), compile_child(prog, timeout));
    match (a, b) {
        (Ok((ca, da)), Ok((cb, db))) => {
            let timeout = Some("timeout".to_string());
            if ca == timeout || cb == timeout {
                return None;
            }
            if ca != cb {
                return Some(format!("verdicts differ: {ca:?} vs {cb:?}"));
            }
            if da != db {
                return Some(first_clif_difference(&da, &db));
            }
            None
        }
        (Err(e), _) | (_, Err(e)) => Some(format!("harness: {e}")),
    }
}

/// Run [`detcheck_one_pair`] over `programs`, `parallelism()/2` pairs
/// in flight (each pair is two children). Returns the flaps.
pub async fn detcheck(
    programs: Vec<(String, String)>,
    timeout: Duration,
) -> Vec<(String, String)> {
    let mut flaps = Vec::new();
    let job = |i: usize| {
        let prog = programs[i].1.clone();
        async move { detcheck_one_pair(&prog, timeout).await }
    };
    windowed(programs.len(), (parallelism() / 2).max(1), job, |i, r| match r {
        Ok(None) => (),
        Ok(Some(detail)) => flaps.push((programs[i].0.clone(), detail)),
        Err(e) => flaps.push((programs[i].0.clone(), format!("panicked: {e}"))),
    })
    .await;
    flaps
}

/// Run one oracle check in a child process (`graphix-fuzz check-one`). A
/// program that kills the evaluator kills only the child; the campaign
/// records a crash finding and keeps running.
async fn check_isolated(prog: &str, timeout: Duration) -> (PoolResult, Duration) {
    check_child(prog, timeout, false).await
}

/// [`check_isolated`], its runs made one at a time when `serial`.
async fn check_child(
    prog: &str,
    timeout: Duration,
    serial: bool,
) -> (PoolResult, Duration) {
    // every run in the child has its own deadline; this one catches a
    // wedged child
    let deadline = Deadline::Fixed(check_deadline(prog, timeout));
    let mut env = vec![("GRAPHIX_FUZZ_RUN_LOG", "1")];
    if serial {
        env.push(("GRAPHIX_FUZZ_SERIAL_RUNS", "1"));
    }
    let ms = timeout.as_millis().to_string();
    let c = run_child("check-one", &[&ms], &env, prog, true, deadline).await;
    let res = match (c.status, c.verdict) {
        (None, _) => PoolResult::Crash("HANG (outer deadline)".into()),
        (_, Some(v)) => match v.split_once('\n') {
            Some(("agree", _)) => PoolResult::Agree,
            Some(("diverge", f)) => match Finding::decode(f) {
                Some(f) => PoolResult::Diverge(f),
                None => PoolResult::Crash(format!("unreadable verdict: {v}")),
            },
            _ => PoolResult::Crash(format!("unreadable verdict: {v}")),
        },
        (Some(status), None) => died(prog, timeout, status, &c.stderr, serial).await,
    };
    (res, c.cpu)
}

/// [`check_isolated`]'s conclusion, for a person: how a campaign reads
/// `prog`.
pub async fn check_isolated_report(prog: &str, timeout: Duration) -> String {
    match check_isolated(prog, timeout).await.0 {
        PoolResult::Agree => "AGREE".to_string(),
        PoolResult::Crash(status) => format!("CRASH — {status}"),
        PoolResult::Diverge(f) => format!(
            "DIVERGENCE — {}\n  {}: {}\n  {}: {}",
            f.bisect, f.labels.0, f.reference, f.labels.1, f.tested
        ),
    }
}

/// What a check child that left no verdict means.
async fn died(
    prog: &str,
    timeout: Duration,
    status: std::process::ExitStatus,
    stderr: &str,
    serial: bool,
) -> PoolResult {
    // a SIGTERM death is the campaign stop's own kill signal reaching a
    // mid-flight child, not a finding
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if status.signal() == Some(15) {
            return PoolResult::Agree;
        }
    }
    // the child's last stderr lines distinguish a node-walk overflow from
    // a SIGSEGV in JIT'd frames (which prints nothing)
    let output =
        || stderr.lines().filter(|l| !l.starts_with("RUN ") && !l.starts_with("DONE "));
    let crash = |what: &str| {
        let mut status = format!("{what}{status}");
        let site = output().find_map(|l| l.split_once(PANIC_SITE)).map(|(_, at)| at);
        if let Some(at) = site {
            status.push_str(" | ");
            status.push_str(PANIC_SITE);
            status.push_str(at.trim_end_matches(':'));
        }
        for l in output().collect::<Vec<_>>().into_iter().rev().take(2).rev() {
            status.push_str(" | ");
            status.push_str(l);
        }
        PoolResult::Crash(status)
    };
    // A request above the cap is a corrupt length, not a runaway (a
    // runaway fails on a request below it).
    let cap = mem_limit();
    let mut over_cap = false;
    let address_space = stderr.lines().any(|l| {
        // another thread's output may share the line
        let request = l
            .split_once("memory allocation of ")
            .and_then(|(_, r)| r.split_once(" bytes failed"))
            .and_then(|(n, _)| n.parse::<u64>().ok());
        over_cap |= cap > 0 && request.is_some_and(|n| n >= cap);
        request.is_some() || l.contains("mmap failed to allocate stack")
    });
    if over_cap {
        return crash("allocation over the address-space cap: ");
    }
    if !address_space {
        return crash("");
    }
    // The address-space cap stopped a runaway: containment, as the stack
    // budget's abort is, when the node-walk's run of the same path runs
    // away too. A run that dies alone beside the node-walk's value is
    // that engine's runaway. The runs in flight at the death are re-run
    // alone, each with its node-walk twin; with none logged, the plain
    // engine pair.
    let mut flight: Vec<Run> = Vec::new();
    for l in stderr.lines() {
        if let Some(r) = l.strip_prefix("RUN ").and_then(Run::parse) {
            flight.push(r);
        } else if let Some(r) = l.strip_prefix("DONE ").and_then(Run::parse)
            && let Some(i) = flight.iter().position(|x| *x == r)
        {
            flight.remove(i);
        }
    }
    if flight.is_empty() {
        flight.push(Run::plain(Mode::Jit, Route::InLanguage));
    }
    let crowded = flight.len() > 1;
    for r in flight.into_iter().filter(|r| !r.mode.node_walk()) {
        if run_isolated(prog, r, timeout).await.is_none()
            && run_isolated(prog, r.node_walk(), timeout).await == Some(true)
        {
            return crash(&format!(
                "the JIT alone exceeds the address-space cap beside a node-walk value \
                 ({}): ",
                r.token()
            ));
        }
    }
    // runs that die only together shared the cap: check again one at a
    // time, so every comparison is made and a death names its run
    if crowded && !serial {
        return Box::pin(check_child(prog, timeout, true)).await.0;
    }
    PoolResult::Agree
}

/// Make `run` in a child (`graphix-fuzz run-one`): whether it produced a
/// trace, `None` when the child died.
async fn run_isolated(prog: &str, run: Run, timeout: Duration) -> Option<bool> {
    let deadline = Deadline::Fixed(slow_budget(timeout) * 2 + Duration::from_secs(30));
    let (ms, token) = (timeout.as_millis().to_string(), run.token());
    let args: Vec<&str> = [ms.as_str()].into_iter().chain(token.split(' ')).collect();
    let c = run_child("run-one", &args, &[], prog, false, deadline).await;
    c.verdict.map(|v| v.trim() == "trace")
}

/// Oracle-check budget for the campaign's minimizer: a soak pays this
/// per finding, so it buys a legible reproducer, not a minimal one.
pub const CAMPAIGN_MINIMIZE_BUDGET: usize = 80;

/// Minimize a diverging program in a child process. A reduction may
/// itself be a crasher and the minimizer checks candidates in-process.
/// The child writes each verified reduction as it goes, so one that died
/// or wedged answers its last; `None` = it verified none, and the caller
/// records the unminimized mutant instead.
async fn minimize_isolated(
    prog: &str,
    key: &str,
    timeout: Duration,
) -> (Option<String>, Duration) {
    // A time cap, not a count: a killed child answers its best so far.
    // The budget's checks at twice the per-mode timeout, or the first
    // check and one candidate at this program's worst case (a one-sided
    // timeout's slow remakes, a wedged child), whichever is longer.
    let deadline = Deadline::Fixed(
        (timeout * 2 * CAMPAIGN_MINIMIZE_BUDGET as u32 + Duration::from_secs(60))
            .max(check_deadline(prog, timeout) * 2),
    );
    let c = run_child("minimize-one", &[key], &[], prog, false, deadline).await;
    let min = c.verdict.map(|m| m.trim().to_string()).filter(|m| !m.is_empty());
    (min, c.cpu)
}

/// Environment-broken backstop for the campaign pool: when a majority
/// of a recent window of subjects produce findings, the problem is the
/// environment (ENOSPC, fd exhaustion) or a broken build, not the
/// programs. Real bug classes hit well under 0.1% of subjects.
struct BreakageWindow {
    window: std::collections::VecDeque<bool>,
    findings: usize,
}

impl BreakageWindow {
    const LEN: usize = 200;

    fn new() -> Self {
        BreakageWindow {
            window: std::collections::VecDeque::with_capacity(Self::LEN),
            findings: 0,
        }
    }

    /// Record one subject outcome. `true` = abort the campaign: the
    /// window is full and most of it is findings. Never trips before
    /// a full window, so short finite runs are unaffected.
    fn note(&mut self, finding: bool) -> bool {
        self.window.push_back(finding);
        self.findings += finding as usize;
        if self.window.len() > Self::LEN {
            self.findings -= self.window.pop_front().unwrap() as usize;
        }
        self.window.len() == Self::LEN && self.findings * 2 > Self::LEN
    }
}

/// The aggregator: issue work orders and aggregate what comes back. The
/// parent never generates, classifies or ships program text; its cost
/// is per batch (issue an order, charge its CPU) and per finding
/// (derive a divergence, admit a ring shape).
pub async fn run_aggregator(
    corpus: &std::sync::Arc<Corpus>,
    seed: u64,
    iters: Option<usize>,
    timeout: Duration,
    weights: [f64; SOURCES],
) -> Vec<(&'static str, FuzzStats, Duration)> {
    use std::sync::atomic::Ordering::Relaxed;
    use tokio::task::JoinSet;
    const KINDS: [SourceKind; SOURCES] = SOURCE_KINDS;
    // findings are confirmed in `derive` tasks, so the tally crosses tasks
    let found: std::sync::Arc<[Tally; SOURCES]> =
        std::sync::Arc::new(std::array::from_fn(|_| Tally::default()));
    /// Ring ancestors per order — a sample, not a snapshot.
    const RING_SAMPLE: usize = 16;
    const RING_CAP: usize = 256;
    // orders and derivations share the slots: each is one child at a time
    let par = parallelism();
    let bsize = batch_size().max(1);
    let mut stats: [FuzzStats; SOURCES] = std::array::from_fn(|_| FuzzStats::default());
    let mut cpu = [Duration::ZERO; SOURCES];
    let mut inflight = [0usize; SOURCES];
    let mut done = [0usize; SOURCES];
    // subjects a batch child could not resolve, re-derived one process
    // each; batching everything is only right while this stays small
    let mut suspect = [0usize; SOURCES];
    let mut to_derive: std::collections::VecDeque<(usize, Suspect)> =
        std::collections::VecDeque::new();
    let mut seed_ctr = [0u64; SOURCES];
    // the regression corpus goes through typemorph once, first
    let mut pins_next = 0usize;
    let mut launched = 0usize;
    let mut ring: std::collections::VecDeque<String> = std::collections::VecDeque::new();
    let mut ring_sigs: ahash::AHashSet<u64> = ahash::AHashSet::default();
    let mut rng = mutate::Rng::new(seed ^ 0xC0FFEE);
    let mut orders: JoinSet<(usize, usize, OrderResult)> = JoinSet::new();
    let mut derive: JoinSet<(usize, bool, Duration)> = JoinSet::new();
    let mut breakage = BreakageWindow::new();
    // a source's counter line: every 1000 subjects, and at least this often
    const LINE_EVERY: Duration = Duration::from_secs(30);
    let mut last_line = [std::time::Instant::now(); SOURCES];
    let wsum = weights.iter().map(|w| w.max(0.0)).sum::<f64>().max(f64::MIN_POSITIVE);
    let want = |launched: usize| iters.map_or(true, |n| launched < n);
    loop {
        // suspects first: a finding waiting on a slot is worth more than
        // a fresh order
        while orders.len() + derive.len() < par
            && let Some((si, suspect)) = to_derive.pop_front()
        {
            let corpus = corpus.clone();
            let found = found.clone();
            derive.spawn(async move {
                let (finding, cpu) =
                    derive_suspect(KINDS[si], &suspect, &corpus, &found[si], timeout)
                        .await;
                (si, finding, cpu)
            });
        }
        // then orders, choosing the source furthest below its target
        // share of measured CPU
        while want(launched) && orders.len() + derive.len() < par {
            let total_done: usize = done.iter().sum();
            let total_cpu: f64 = cpu.iter().map(|c| c.as_secs_f64()).sum();
            let mean = if total_done > 0 { total_cpu / total_done as f64 } else { 1.0 };
            let proj: Vec<f64> = (0..SOURCES)
                .map(|i| {
                    let m = if done[i] > 0 {
                        cpu[i].as_secs_f64() / done[i] as f64
                    } else {
                        mean
                    };
                    cpu[i].as_secs_f64() + inflight[i] as f64 * m
                })
                .collect();
            let tot: f64 = proj.iter().sum();
            let mut si = 0;
            let mut best = f64::NEG_INFINITY;
            for i in (0..SOURCES).filter(|&i| weights[i] > 0.0) {
                let target = weights[i] / wsum;
                let actual = if tot > 0.0 { proj[i] / tot } else { 0.0 };
                if target - actual > best {
                    best = target - actual;
                    si = i;
                }
            }
            let count = match iters {
                Some(n) => bsize.min(n.saturating_sub(launched)),
                None => bsize,
            };
            if count == 0 {
                break;
            }
            let breeds = matches!(KINDS[si], SourceKind::Fuzz | SourceKind::Typemorph);
            let sample: Vec<String> = if breeds && !ring.is_empty() {
                (0..RING_SAMPLE.min(ring.len()))
                    .map(|_| ring[rng.below(ring.len())].clone())
                    .collect()
            } else {
                Vec::new()
            };
            seed_ctr[si] += 1;
            let pins = match KINDS[si] {
                SourceKind::Typemorph => {
                    let start = pins_next;
                    pins_next = (start + count).min(corpus::REGRESSION_CORPUS.len());
                    start..pins_next
                }
                _ => 0..0,
            };
            let order = WorkOrder {
                kind: KINDS[si],
                // distinct per (source, order) without a shared counter
                seed: seed
                    .wrapping_mul(0xD6E8_FEB8_6659_FD93)
                    .wrapping_add((si as u64 + 1).wrapping_mul(0x9E37_79B9_7F4A_7C15))
                    .wrapping_add(seed_ctr[si]),
                count,
                ring: sample,
                pins,
            };
            launched += count;
            inflight[si] += count;
            orders.spawn(
                async move { (si, count, run_order_child(&order, timeout).await) },
            );
        }
        tokio::select! {
            biased;
            Some(res) = orders.join_next() => {
                let Ok((si, count, r)) = res else { continue };
                cpu[si] += r.cpu;
                inflight[si] -= count;
                done[si] += count;
                stats[si].run += count;
                stats[si].refused += r.refused;
                for (sig, prog) in r.novel {
                    if ring_sigs.insert(sig) {
                        ring.push_back(prog);
                        stats[si].novel += 1;
                        if ring.len() > RING_CAP {
                            ring.pop_front();
                        }
                    }
                }
                for _ in 0..r.agreed {
                    breakage_note(&mut breakage, false);
                }
                if stats[si].run % 1000 < count || last_line[si].elapsed() >= LINE_EVERY {
                    last_line[si] = std::time::Instant::now();
                    stats[si].divergences = found[si].divergences.load(Relaxed);
                    stats[si].crashes = found[si].crashes.load(Relaxed);
                    let tot: f64 = cpu.iter().map(|c| c.as_secs_f64()).sum();
                    let pct = if tot > 0.0 {
                        (cpu[si].as_secs_f64() * 100.0 / tot).round() as u64
                    } else {
                        0
                    };
                    let ipct = if stats[si].run > 0 {
                        suspect[si] * 100 / stats[si].run
                    } else {
                        0
                    };
                    eprintln!(
                        "  {}…{} run, {} {} ({} known), {} crashes, {} in corpus, \
                         {} novel shapes, {}% cpu, {}% individual, {} refused",
                        KINDS[si].tag(), stats[si].run, stats[si].divergences,
                        KINDS[si].findings(), found[si].known.load(Relaxed),
                        stats[si].crashes, corpus.len(), stats[si].novel, pct, ipct,
                        stats[si].refused
                    );
                }
                suspect[si] += r.suspect.len();
                to_derive.extend(r.suspect.into_iter().map(|p| (si, p)));
            }
            Some(res) = derive.join_next() => {
                // a derivation's children are its source's CPU too
                let finding = match res {
                    Ok((si, finding, c)) => {
                        cpu[si] += c;
                        finding
                    }
                    Err(_) => true,
                };
                breakage_note(&mut breakage, finding);
            }
            else => {
                if to_derive.is_empty() {
                    break;
                }
            }
        }
    }
    for i in 0..SOURCES {
        stats[i].divergences = found[i].divergences.load(Relaxed);
        stats[i].crashes = found[i].crashes.load(Relaxed);
    }
    (0..SOURCES).map(|i| (KINDS[i].tag(), stats[i].clone(), cpu[i])).collect()
}

/// Note one subject in the breakage window; a tripped window ends the
/// campaign.
fn breakage_note(w: &mut BreakageWindow, finding: bool) {
    if w.note(finding) {
        eprintln!(
            "FATAL fuzz harness: {} of the last {} subjects were findings — the \
             environment (or the build) is broken",
            w.findings,
            BreakageWindow::LEN,
        );
        std::process::exit(2);
    }
}

/// One source's findings.
#[derive(Default)]
struct Tally {
    divergences: std::sync::atomic::AtomicUsize,
    crashes: std::sync::atomic::AtomicUsize,
    /// Divergences whose key had its minimizations: counted, not recorded.
    known: std::sync::atomic::AtomicUsize,
}

/// Derive one suspect in a fresh process and record what it is: a flip
/// for typemorph, else a crash or a divergence, minimized while its key
/// has minimizations left. Whether it is a finding, and the CPU its
/// children burned.
async fn derive_suspect(
    kind: SourceKind,
    suspect: &Suspect,
    corpus: &Corpus,
    tally: &Tally,
    timeout: Duration,
) -> (bool, Duration) {
    use std::sync::atomic::Ordering::Relaxed;
    let prog = suspect.prog();
    if kind == SourceKind::Typemorph {
        let (confirmed, cpu) = confirm_typeflip(corpus, tally, suspect, timeout).await;
        if confirmed {
            tally.divergences.fetch_add(1, Relaxed);
        }
        return (confirmed, cpu);
    }
    let (res, cpu) = check_isolated(prog, timeout).await;
    let finding = match res {
        PoolResult::Agree => false,
        // a hang of a program on the clock, the network or a process is
        // the environment's
        PoolResult::Crash(status)
            if status.contains("HANG") && environmental_hang(prog) =>
        {
            println!("HANG (environment) — not recorded");
            println!("    program: {}", prog.replace('\n', "\\n"));
            false
        }
        PoolResult::Crash(status) => {
            tally.crashes.fetch_add(1, Relaxed);
            let class = crash_class(&status);
            let claim = crash_claim(&status, prog);
            if !corpus.claim(&claim) {
                tally.known.fetch_add(1, Relaxed);
                return (true, cpu);
            }
            // a hang's candidates would each wait out their deadline
            let (min, min_cpu) = if status.contains("HANG") {
                (None, Duration::ZERO)
            } else {
                minimize_isolated(prog, &class, timeout).await
            };
            if corpus.record_crash(prog, &status, &claim, min.as_deref()) {
                println!("CRASH — child {status}");
                println!(
                    "    program: {}",
                    min.as_deref().unwrap_or(prog).replace('\n', "\\n")
                );
            }
            return (true, cpu + min_cpu);
        }
        PoolResult::Diverge(f) => {
            tally.divergences.fetch_add(1, Relaxed);
            if !corpus.claim(&f.key) {
                tally.known.fetch_add(1, Relaxed);
                return (true, cpu);
            }
            let (min, min_cpu) = minimize_isolated(prog, &f.key, timeout).await;
            let min = min.unwrap_or_else(|| prog.to_string());
            if corpus.record(&f, prog, &min) {
                println!("DIVERGENCE — {}", f.bisect);
                println!("    minimized: {min}");
                let (a, b) = &f.labels;
                println!("    {a}={} {b}={}", f.reference, f.tested);
            }
            return (true, cpu + min_cpu);
        }
    };
    (finding, cpu)
}

/// Re-probe a subject a batch child flipped, or died before judging, in
/// a fresh process and record each flip class it shows there; a flip
/// only one of the two processes shows is an acceptance flap, its own
/// class. A fresh process that died or wedged is a crash, recorded per
/// program. True when the fresh process flipped.
async fn confirm_typeflip(
    corpus: &Corpus,
    tally: &Tally,
    suspect: &Suspect,
    timeout: Duration,
) -> (bool, Duration) {
    let prog = suspect.prog();
    let (r, cpu) = typemorph_child(prog, timeout).await;
    let (flips, confirmed) = match (r, suspect) {
        (Ok(rep), Suspect::Unjudged(_)) => {
            let f = tm_flips(&rep);
            let confirmed = !f.is_empty();
            (f, confirmed)
        }
        (Ok(rep), Suspect::Flagged(_, batch)) => confirmed_flips(tm_flips(&rep), batch),
        // a confirm that measured nothing confirms nothing
        (Err(e), _) => {
            tally.crashes.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            if corpus.record_crash(prog, &e, &crash_claim(&e, prog), None) {
                println!("CRASH — typemorph child {e}");
                println!("    program: {}", prog.replace('\n', "\\n"));
            }
            return (true, cpu);
        }
    };
    for Flip { id, head, mutant } in flips {
        if corpus.record_typeflip(prog, &id, &head, mutant.as_deref()) {
            println!("TYPEFLIP — {id}: {head}");
            println!("    program: {}", prog.replace('\n', "\\n"));
        }
    }
    (confirmed, cpu)
}

/// The flips a fresh process shows that the batch child showed too, and
/// whether there are any; a flip only one of them showed is the
/// unconfirmed class.
fn confirmed_flips(fresh: Vec<Flip>, batch: &[String]) -> (Vec<Flip>, bool) {
    let shown = fresh.len();
    let mut kept: Vec<Flip> =
        fresh.into_iter().filter(|f| batch.contains(&f.id)).collect();
    let confirmed = !kept.is_empty();
    if kept.len() != shown || kept.len() != batch.len() {
        kept.push(Flip::harness("unconfirmed", "fresh-process flap".to_string()));
    }
    (kept, confirmed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construction_timeouts_only_agree_with_construction() {
        use Containment::{ActivationBudget, Compile, Deadline, StackBudget};
        let kinds = [Compile, Deadline, StackBudget, ActivationBudget];
        let agrees = [
            [true, false, false, false],
            [false, true, true, true],
            [false, true, true, true],
            [false, true, true, true],
        ];
        for (i, a) in kinds.iter().copied().enumerate() {
            for (j, b) in kinds.iter().copied().enumerate() {
                let (left, right) = (Outcome::Timeout(a), Outcome::Timeout(b));
                for strength in [Strength::Exact, Strength::Final, Strength::Kind] {
                    assert_eq!(
                        left.agrees_at(&right, strength),
                        agrees[i][j],
                        "{a:?} beside {b:?} at {strength:?}"
                    );
                }
                assert_eq!(
                    Verdict::of(&left, &right),
                    if agrees[i][j] { Verdict::Contained } else { Verdict::Unsure }
                );
            }
        }
    }

    #[tokio::test]
    async fn mixed_construction_timeouts_retry_the_construction_side() {
        for runtime in [
            Containment::Deadline,
            Containment::StackBudget,
            Containment::ActivationBudget,
        ] {
            for compile_left in [false, true] {
                let pair = Pair::Engine;
                let (ra, rb) = pair.runs().unwrap();
                let mut oracle =
                    Oracle::new("1", OracleTier::Exact, Duration::from_secs(2), None);
                let (compile, running) =
                    (Outcome::Timeout(Containment::Compile), Outcome::Timeout(runtime));
                let (a, b) =
                    if compile_left { (compile, running) } else { (running, compile) };
                oracle.runs.extend([(ra, a), (rb, b)]);
                let settled = oracle.settle(pair).await;
                assert!(!matches!(settled, Settled::Agree));
                if !compile_left && runtime != Containment::Deadline {
                    assert!(
                        matches!(settled, Settled::Drop),
                        "a recovered JIT beside a node-walk budget is still containment"
                    );
                }
                assert!(oracle.confirmed);
                assert!(
                    matches!(
                        oracle.get(if compile_left { ra } else { rb }),
                        Outcome::Trace(_)
                    ),
                    "the construction side must be remade"
                );
                assert_eq!(oracle.verdict(), Verdict::Unsure);
            }
        }
    }

    #[test]
    fn render_prints_composites_in_graphix_syntax() {
        let pair =
            graphix_compiler::rep::tuple([Value::I64(1), Value::I64(2)].into_iter());
        assert_eq!(render(&Outcome::single(pair)), "Trace([0:(1, 2)])");
    }

    /// With forking on, a subject's sessions include a warm start under
    /// the forked JIT, which its Par pair ties to the serial runs.
    #[test]
    fn sessions_include_the_forked_jit() {
        if !par_enabled() {
            return;
        }
        let pairs = comparisons("1", true);
        for p in [
            Pair::Cold(Mode::JitPar, Route::InLanguage),
            Pair::Warm(Mode::JitPar, Route::InLanguage),
            Pair::Par(Mode::JitPar),
        ] {
            assert!(pairs.contains(&p), "{p:?} missing from {pairs:?}");
        }
    }

    #[test]
    fn normalize_diag_strips_fresh_counters() {
        let n = normalize_diag;
        assert_eq!(
            n("`let [h, ..] = #seqv4611686018427390_1` in #seqpc12"),
            "`let [h, ..] = #seqvN_1` in #seqpcN"
        );
        assert_eq!(n("'_6070 <abstract#12> #do123 #fn45"), "'_N <abstract#N> #doN #fnN");
        assert_eq!(n("myseqv12 seq x2 'a1 s3"), "myseqv12 seq x2 'a1 s3");
    }

    #[test]
    fn breakage_window_trips_only_on_sustained_majority() {
        let mut w = BreakageWindow::new();
        for _ in 0..10_000 {
            assert!(!w.note(false));
        }
        // a sub-majority burst of real findings does not trip
        for _ in 0..BreakageWindow::LEN / 2 {
            assert!(!w.note(true));
        }
        for _ in 0..BreakageWindow::LEN {
            assert!(!w.note(false));
        }
        // every subject a finding: trips within one window
        assert!((0..BreakageWindow::LEN).any(|_| w.note(true)));
        // never trips before the window fills
        let mut w = BreakageWindow::new();
        for _ in 0..BreakageWindow::LEN - 1 {
            assert!(!w.note(true));
        }
        assert!(w.note(true));
    }

    #[test]
    fn addr_getters_are_excluded_tier() {
        // OS-assigned ephemeral ports leak into values through the addr
        // getters
        for getter in ["listener_addr", "local_addr", "peer_addr"] {
            let prog = format!("{{let a = sys::tcp::{getter}(s)?; a}}");
            assert_eq!(oracle_tier(&prog), OracleTier::Excluded);
        }
        assert_eq!(
            oracle_tier("sys::tcp::connect(\"127.0.0.1:5000\")"),
            OracleTier::FinalValues
        );
    }

    #[test]
    fn child_processes_are_excluded_tier() {
        // a child's lifetime is program data: `wait` and its stdout land
        // in whichever epoch the wall clock reaches
        assert_eq!(
            oracle_tier(
                "{let c = sys::process::spawn(o)?; let w = sys::process::wait(c.proc)?; w.code}"
            ),
            OracleTier::Excluded
        );
        assert_eq!(
            oracle_tier("// spawn in a comment\nsys::fs::metadata(p)"),
            OracleTier::FinalValues
        );
    }

    #[test]
    fn throttle_is_excluded_tier() {
        // `throttle` reads the wall clock and must exclude on its own,
        // with no sys:: in the program
        assert_eq!(
            oracle_tier("count(throttle(#rate: duration:0.001s, x))"),
            OracleTier::Excluded
        );
        // the other fire-count-sensitive builtins stay Exact
        assert_eq!(oracle_tier("count(x)"), OracleTier::Exact);
        assert_eq!(oracle_tier("array::iter([1, 2, 100]) > 3"), OracleTier::Exact);
    }

    #[test]
    fn a_self_clocked_sequence_beside_an_async_value_is_excluded() {
        // `iter` emits one element per cycle; the cycle an async
        // handle lands in decides which element a group's predicate
        // answers over: `[]` or `[Stderr]` by load
        assert_eq!(
            oracle_tier(
                "array::group(sys::io::stderr(null), |n, _| array::iter([1, 2, 100]) > 3)"
            ),
            OracleTier::Excluded
        );
        assert_eq!(
            oracle_tier("sys::io::stderr(null) ~ array::iter([1, 2, 3])"),
            OracleTier::Excluded
        );
        assert_eq!(
            oracle_tier("array::len(sys::fs::readdir(p))"),
            OracleTier::FinalValues
        );
        // a comment naming the builtin must not un-gate the program
        assert_eq!(
            oracle_tier("// a header naming throttle\ncount(x)"),
            OracleTier::Exact
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn must_reject_families_are_refused_where_their_rules_say() {
        let per = Duration::from_secs(60);
        for (prog, family) in [
            (
                "{ let f = |x| x; let a = array::map([1], f); let b = array::map([\"s\"], f); (a, b) }",
                "mono-reuse#",
            ),
            ("{ let v: [`A, `B] = `A; select v { `A => 1, `B => 2 } }", "variant-widen#"),
            ("{ let a = 1; let b = a + 2; b }", "widen-consumer#"),
            ("{ let s = {x: 1}; s.x }", "widen-consumer#"),
            ("{ let f = |x: i64| -> i64 x; f(3) }", "widen-consumer#"),
            ("{ let v = 1; let w = v + 1; w }", "retype#"),
            // a call's return is no cell its uses decide
            ("{ let f = |x: i64| -> i64 x; let v = f(1); let w = v + 1; w }", "retype#"),
            // nor is an operator's, a negation's or a `$`'s over concrete operands
            ("{ let v = 2 + 1; v }", "retype#"),
            ("{ let v = -(2 + 1); v }", "retype#"),
            ("{ let v = cast<i64>(2.5)$; v }", "retype#"),
            // the hop: the widened `a` reaches `+` through `w`'s let
            ("{ let a = 1; let w = a; let z = w + 2; z }", "widen-hop#"),
            ("{ let f = 'a: Number |x: 'a| -> 'a x + x; f(1) }", "rigid-var#"),
            ("{ let g = 'a: Number |x: 'a, y: 'a| -> 'a x + y; g(1, 2) }", "shared-var#"),
            (
                "{ let g = |x: 'a, y: 'a, z: 'a| -> 'a x; let r = g(1, 2, 3); r }",
                "shared-var#",
            ),
            // an alias's value uses share the wrapped binding's cells
            (
                "{ let f = |x| x; let g = f; let a = array::map([1], f); \
                 let c = array::map([\"s\"], g); let b = array::map([\"s\"], f); (a, b, c) }",
                "mono-reuse#",
            ),
            ("{ let f = |#a: i64, x: i64| -> i64 a + x; f(#a: 1, 3) }", "label-unknown#"),
            ("{ let f = |#a: i64, x: i64| -> i64 a + x; f(#a: 1, 3) }", "label-missing#"),
            (
                "{ let qf = queuefn(#trigger: never(), |x: i64| -> i64 x); qf(7) }",
                "function-bound#",
            ),
            (
                "{ let f = |#a: i64 = 1, x: i64| -> i64 a + x; \
                 let w = |h: fn(?#a: i64, x: i64) -> i64| -> i64 h(2); w(f) }",
                "label-default#",
            ),
            (
                "{ let f = |#a: i64 = 1, x: i64| -> i64 a + x; \
                 let w = |h: fn(?#a: i64, x: i64) -> i64| -> i64 h(2); \
                 let g = f; let p = w(g); let q = w(f); p + q }",
                "label-default#",
            ),
            // the retyped `m` reaches the writer of `src` through `src`'s let
            (
                "{ let m = 1; let src = [m]; src <- [2]; let w = m + 1; (src, w) }",
                "retype#",
            ),
            ("{ let x = 1; let r = &mut x; let y = 2; *r <- 5; x + y }", "ref-write#"),
            // a use before the write that needs `r` writable refuses first
            (
                "{ let x = 1; let f = |p: &mut i64| -> i64 *p; let r = &mut x; \
                 let y = f(r); *r <- 5; x + y }",
                "ref-write#",
            ),
            ("{ let x = 1; let r = &mut x; let y = 2; *r <- 5; x + y }", "ref-widen#"),
            (
                "{ let s: [string, null] = \"a\"; select s { null as _ => 0, string as v => 1 } }",
                "same-form#",
            ),
            (
                "{ let v = m0::mkb(1); m0::unb(v) }\n// file-v1: m0.gx\n\
                 type B<'a> = Abstract<'a>;\nlet mkb = |x| B(x);\nlet unb = |b: B<'a>| b.0\n",
                "abstract-param#",
            ),
            // an operator argument is no open cell
            (
                "{ let x = i64:2; let v = m0::mkb(x + i64:1); m0::unb(v) }\n// file-v1: m0.gx\n\
                 type B<'a> = Abstract<'a>;\nlet mkb = |x| B(x);\nlet unb = |b: B<'a>| b.0\n",
                "abstract-param#",
            ),
        ] {
            let rep = typemorph_subject(prog, per, TM_CAP).await.unwrap();
            assert!(rep.base == TmVerdict::Accept, "{prog}");
            assert!(
                rep.rejects
                    .iter()
                    .any(|p| p.id.starts_with(family) && p.verdict.is_none()),
                "{prog}: {:?}",
                rep.rejects
            );
        }
        // family 8 refuses a literal and a function with a defaulted label
        let qf = "{ let qf = queuefn(#trigger: never(), |x: i64| -> i64 x); qf(7) }";
        let rep = typemorph_subject(qf, per, TM_CAP).await.unwrap();
        let fb: Vec<_> =
            rep.rejects.iter().filter(|p| p.id.starts_with("function-bound#")).collect();
        assert!(fb.len() >= 2 && fb.iter().all(|p| p.verdict.is_none()), "{fb:?}");
        // a call instantiates its callee whatever the binding: no site
        let calls = "{ let f = |x| x + x; let a = f(1); let b = f(1.5); (a, b) }";
        let rep = typemorph_subject(calls, per, TM_CAP).await.unwrap();
        assert!(
            !rep.rejects.iter().any(|p| p.id.starts_with("mono-reuse#")),
            "{:?}",
            rep.rejects
        );
        // a let over ⊥ takes its first writer's type: no retype site
        let bottom = "{ let v = { catch(e) 7; (error(true))? }; let w = v + 1; w }";
        let rep = typemorph_subject(bottom, per, TM_CAP).await.unwrap();
        assert!(
            !rep.rejects.iter().any(|p| p.id.starts_with("retype#")),
            "{:?}",
            rep.rejects
        );
        // no family's mutant was accepted or refused elsewhere
        assert!(rep.rejects.iter().all(|p| p.verdict.is_none()), "{:?}", rep.rejects);
        // a parameter wider than its argument, a binding that a seq step
        // shadows: every mutant is refused in place
        for prog in [
            "{ let f = |a: [string, null]| -> i64 1; f(null) }",
            "{ let a = 0; let s = 0; seq true { let a = 7; s <- a + 1 }; (a, s) }",
        ] {
            let rep = typemorph_subject(prog, per, TM_CAP).await.unwrap();
            assert!(
                rep.rejects.iter().all(|p| p.verdict.is_none()),
                "{prog}: {:?}",
                rep.rejects
            );
        }
    }

    /// A rewrite that changes which statement first reads an open cell
    /// (`let z = never()`) changes its type: the cell is reached through a
    /// lambda capturing it or an alias as well as by name.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn typemorph_keeps_the_first_reader_of_an_open_cell() {
        let per = Duration::from_secs(60);
        for prog in [
            "{ let z = never(); let f = |x: i64| z; let b: i64 = f(2); let a: [i64, string] = f(1); (a, b) }",
            "{ let z = never(); let v = z; let b: i64 = z; let a: [i64, string] = v; (a, b) }",
            "{ let z = never(); let f = |x: i64| z; let v = { let q: i64 = f(1); q }; \
             let a: [i64, string] = f(2); let b = v; (a, b) }",
        ] {
            let rep = typemorph_subject(prog, per, TM_CAP).await.unwrap();
            assert!(rep.base == TmVerdict::Accept, "{prog}");
            let flips = tm_flips(&rep.render());
            assert!(
                flips.is_empty(),
                "{prog}: {:?}",
                flips.iter().map(|f| &f.id).collect::<Vec<_>>()
            );
        }
    }

    /// Where a family's rule does not apply it makes no mutant there, or
    /// its mutant is refused for the rule's reason: a site the rule cannot
    /// refuse would read as a leak, and a refusal for another reason
    /// would hide the rule's regression.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn must_reject_families_skip_what_their_rules_allow() {
        let per = Duration::from_secs(60);
        for (prog, family) in [
            // `Any` has no runtime form to collide with
            (
                "{ let s: Any = \"a\"; select s { string as x => 1, _ => 2 } }",
                "same-form#",
            ),
            // a bind catches all: no dead `_ => never()` arm to refuse
            (
                "{ let s: [string, i64] = \"a\"; let r = select s { string as x => 1, y => 2 }; r }",
                "same-form#",
            ),
            // a callee with an unannotated parameter widens the cell it
            // shares with the environment, however the call reaches it
            (
                "{ let z = never(); let f = |y| z <- y; let r = f(1); (r, z) }",
                "widen-consumer#",
            ),
            (
                "{ let z = never(); let s = {f: |y| z <- y}; let r = s.f(1); (r, z) }",
                "widen-consumer#",
            ),
            ("{ let z = never(); let r = (|y| z <- y)(1); (r, z) }", "widen-consumer#"),
            // nor through the subject's own module
            (
                "{ let r = m0::f(1); r }\n// file-v1: m0.gx\nlet z = never();\nlet f = |y| z <- y;\n",
                "widen-consumer#",
            ),
            (
                "{ let z = never(); let f = |y| z <- y; let g = f; let r = g(1); (r, z) }",
                "widen-consumer#",
            ),
            (
                "{ let z = never(); let f = { let k = 0; |y| z <- y }; let r = f(1); (r, z) }",
                "widen-consumer#",
            ),
            // a callee a destructuring let or a `use` binds
            (
                "{ let z = never(); let (f, k) = (|y| z <- y, 0); let r = f(1); (r, z, k) }",
                "widen-consumer#",
            ),
            (
                "{ use m0::f; let r = f(1); r }\n// file-v1: m0.gx\nlet z = never();\nlet f = |y| z <- y;\n",
                "widen-consumer#",
            ),
            (
                "{ use m0::f as g; let r = g(1); r }\n// file-v1: m0.gx\nlet z = never();\nlet f = |y| z <- y;\n",
                "widen-consumer#",
            ),
            // a call's result is the open cell its callee's body captures
            (
                "{ let z = never(); let mk = |u: i64| z; let v = mk(1); let w = v + 1; w }",
                "retype#",
            ),
            // the annotation would decide the open referent
            ("{ let x = never(); let r = &mut x; *r <- 2; x }", "ref-widen#"),
            // the call decides the open first argument's type
            (
                "{ let g = |x: 'a, y: 'a| -> 'a x; let z = never(); let r = g(z, 1); [r, z] }",
                "shared-var#",
            ),
            // a third argument of the variable holds both
            (
                "{ let g = |x: 'a, y: 'a, z: 'a| -> 'a x; let u: [i64, string] = 1; \
                 let r = g(1, 2, u); r }",
                "shared-var#",
            ),
        ] {
            let rep = typemorph_subject(prog, per, TM_CAP).await.unwrap();
            assert!(rep.base == TmVerdict::Accept, "{prog}");
            let bad: Vec<_> = rep
                .rejects
                .iter()
                .filter(|p| p.id.starts_with(family) && p.verdict.is_some())
                .map(|p| (&p.id, &p.mutant, &p.verdict))
                .collect();
            assert!(bad.is_empty(), "{prog}: {bad:?}");
        }
    }

    #[test]
    fn work_order_round_trips_its_pins() {
        let order = WorkOrder {
            kind: SourceKind::Typemorph,
            seed: 7,
            count: 3,
            ring: vec!["1 + 2".into(), "{ let x = 1; x }".into()],
            pins: 4..6,
        };
        let back = WorkOrder::decode(&order.encode()).unwrap();
        assert_eq!(back.kind, SourceKind::Typemorph);
        assert_eq!((back.seed, back.count, back.pins.clone()), (7, 3, 4..6));
        assert_eq!(back.ring, order.ring);
        let mut next = back.generator();
        for i in 4..6 {
            assert_eq!(next(), corpus::REGRESSION_CORPUS[i].1, "pins come first");
        }
    }

    #[test]
    fn a_typeflip_class_is_recorded_once_across_loads() {
        let dir = tempfile::tempdir().unwrap();
        let corpus = Corpus::load(dir.path());
        let head = "N: unreachable arm";
        assert!(corpus.record_typeflip("f(1)", "let-extract#3", head, None));
        assert!(
            !corpus.record_typeflip("g(2)", "let-extract#9", head, None),
            "same class"
        );
        assert!(
            corpus.record_typeflip("g(2)", "block-wrap#0", head, Some("g(3)")),
            "another kind"
        );
        let reloaded = Corpus::load(dir.path());
        assert!(!reloaded.record_typeflip("h(3)", "let-extract#1", head, None));
        assert!(reloaded.record_typeflip(
            "h(3)",
            "let-extract#1",
            "N: type mismatch",
            None
        ));
        let files = std::fs::read_dir(dir.path()).unwrap().count();
        assert_eq!(files, 3);
    }

    #[test]
    fn crash_key_sign_fold() {
        // a leading minus folds into the digit run
        assert_eq!(crash_key("range(-9223372036854775808, 4)"), crash_key("range(0, 4)"));
        assert_ne!(crash_key("n <= 1"), crash_key("n == 1"));
        // a minus not followed by a digit keys literally
        assert_ne!(crash_key("a - b"), crash_key("a b"));
        assert_eq!(crash_key("x -> y"), crash_key("x -> y"));
    }

    /// How much fusion a probe demands under the JIT, beyond value
    /// agreement. Value agreement alone cannot distinguish "fused
    /// correctly" from "silently never fused".
    #[derive(Clone, Copy, PartialEq)]
    enum Fuse {
        /// No fusion assertion: value agreement only.
        No,
        /// At least one region fused. An auxiliary region can satisfy
        /// this while the construct under test node-walks; use `Clean`
        /// when the whole program is expected to fuse.
        Some,
        /// `fused > 0` and no real blocker: the only tolerated failed
        /// entries are the ancestor noise ("the node-walk computes" for
        /// the Module/Bind wrappers above the fused region).
        Clean,
    }

    async fn check_jit(code: &str, fuse: Fuse) {
        let t = Duration::from_secs(10);
        let (interp, (jit, stats)) = tokio::join!(
            run_program(code, Mode::Interp, t),
            run_program_with_stats(code, Mode::Jit, t),
        );
        assert!(
            matches!(interp, Outcome::Trace(_)),
            "probe `{code}` did not run: {}",
            render(&interp)
        );
        assert!(
            interp.agrees_with(&jit),
            "Interp vs Jit disagree for `{code}`: {interp:?} vs {jit:?}"
        );
        if fuse == Fuse::No {
            return;
        }
        let stats =
            stats.unwrap_or_else(|| panic!("the runtime answered no stats: {jit:?}"));
        let why: String = stats
            .failed
            .iter()
            .map(|failure| format!("\n  {:?}: {}", failure.id, failure.reason))
            .collect();
        assert!(
            stats.fused > 0,
            "expected `{code}` to fuse under the JIT but no region \
             compiled (attempted={}); failures:{why}",
            stats.attempted,
        );
        if fuse == Fuse::Clean {
            for failure in &stats.failed {
                let id = failure.id;
                let reason = &failure.reason;
                // Structural noise, not coverage gaps: the ancestor
                // wrappers, a `mod` statement, a function-valued let, and
                // a fn-typed call site whose instance body fuses.
                assert!(
                    reason.contains("the node-walk computes")
                        || reason.contains("module statement is structure")
                        || reason.contains("function-valued let")
                        || reason.contains("not discovered")
                        || reason.contains("no static callee"),
                    "expected `{code}` to fuse cleanly under the JIT \
                     but {id:?} hit a real blocker: {reason}"
                );
            }
        }
    }

    async fn agree(code: &str) {
        check_jit(code, Fuse::No).await
    }

    /// [`agree`] + at least one region fused.
    async fn agree_fused(code: &str) {
        check_jit(code, Fuse::Some).await
    }

    /// [`agree_fused`] + nothing but ancestor noise refused. Prefer this
    /// for new probes.
    async fn agree_fused_clean(code: &str) {
        check_jit(code, Fuse::Clean).await
    }

    /// `#[tail_recursive]` over a body that does not fuse runs on the
    /// node-walk and is refused by the JIT, which alone builds the loop:
    /// the oracle records nothing.
    #[tokio::test(flavor = "multi_thread")]
    async fn tail_recursive_loop_is_the_jits() {
        let budget = Some(ACTIVATION_BUDGET);
        for code in [
            "{ #[tail_recursive] let rec f = |n: i64, acc: i64| -> i64 \
             select n { 0 => acc, _ => f(n - 1, *(&acc) + 1) }; f(10, 0) }",
            // the node-walk's run is a runaway the activation budget stops
            "{ #[tail_recursive] let rec f = |n: i64, acc: i64| -> i64 f(n, *(&acc)); \
             f(0, 0) }",
        ] {
            let (d, _) = judged(code, Duration::from_secs(10), budget).await;
            assert!(d.is_none(), "{code}: {d:?}");
        }
        let runaway = "{ #[tail_recursive] let rec f = |n: i64, acc: i64| -> i64 \
                       f(n, *(&acc)); f(0, 0) }";
        let opts = RunOpts { activations: budget, ..RunOpts::default() };
        let t = Duration::from_secs(10);
        let o = run_subject(
            runaway,
            Mode::Interp,
            Route::InLanguage,
            SessionImage::None,
            t,
            opts,
        )
        .await
        .outcome;
        assert!(matches!(o, Outcome::Timeout(Containment::ActivationBudget)), "{o:?}");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_scalar_probes() {
        agree_fused("{ let x = i64:5; x * i64:3 }").await;
        agree_fused("{ let x = i64:5; let y = i64:2; (x + y) * (x - y) }").await;
        // div-by-zero bottoms at runtime inside the compiled kernel
        agree_fused("i64:10 / i64:0").await;
        agree_fused("{ let a = i64:7; a > i64:3 && a < i64:10 }").await;
        agree_fused("f64:3.0 + f64:1.0").await;
        // a cast
        agree_fused_clean("cast<f64>(i64:7)$ + f64:1.0").await;
        // the inner block reads `outer`, a scalar kernel param
        agree_fused("{ let outer = i64:100; { let t = outer - i64:1; t * i64:2 } }")
            .await;
        agree_fused("{ let a = i64:9; { let b = a * i64:2; b + a } }").await;
    }

    /// `?`/`$` unwrap and builtin call emission. The generated sweep
    /// produces neither construct.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_qop_dyncall_probes() {
        // scalar-success `$`
        agree_fused("{ let a = [i64:1, i64:2, i64:3]; a[0]$ + a[1]$ }").await;
        // out-of-bounds bottoms at runtime
        agree_fused("{ let a = [i64:1]; a[5]$ }").await;
        agree_fused(r#"{ let m = {"a" => i64:7}; m{"a"}$ + i64:1 }"#).await;
        // value-shape success `$`
        agree_fused("{ let a = [duration:1.s]; a[0]$ }").await;
        agree_fused(r#"{ let s = "hello"; str::len(s) }"#).await;
        agree_fused(r#"{ let s = "hello"; str::len(s) + i64:1 }"#).await;
        // string return
        agree_fused(r#"{ let s = "abc"; str::to_upper(s) }"#).await;
        // composite-success `$`
        agree_fused("{ let a = [i64:1, i64:2, i64:3]; a[1..]$ }").await;
        agree_fused("{ let a = [i64:1, i64:2, i64:3]; let x = a[1..]; x$ }").await;
        agree_fused("{ let t = [(i64:1, i64:2)]; t[0]$ }").await;
    }

    /// `select` emission: literal arms, scrutinee binds, guards (one
    /// bottoming at runtime), null/Nullable type predicates in both arm
    /// orders, variant payload binds, a computed scrutinee, nested select.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_select_probes() {
        agree_fused(
            "{ let x = i64:7; select x { i64:0 => i64:100, \
             i64:7 => i64:200, _ => i64:1 } }",
        )
        .await;
        agree_fused("{ let x = i64:5; select x { i64:0 => i64:100, n => n * i64:2 } }")
            .await;
        agree_fused(
            "{ let x = i64:3; select x { n if n > i64:10 => n, \
             n => n + i64:1 } }",
        )
        .await;
        agree_fused(
            "{ let x = i64:42; select x { n if n > i64:10 => n * i64:2, \
             n => n } }",
        )
        .await;
        // a guard that bottoms at runtime does not match
        agree_fused(
            "{ let x = i64:9; select x { n if n / i64:0 == i64:1 => i64:1, \
             m => m } }",
        )
        .await;
        // nullable scrutinee, both arm orders, both runtime values
        agree_fused(
            "{ let v: [i64, null] = null; select v { i64 as _ => i64:1, \
             null as _ => i64:0 } }",
        )
        .await;
        agree_fused(
            "{ let v: [i64, null] = null; select v { null as _ => i64:0, \
             i64 as _ => i64:1 } }",
        )
        .await;
        agree_fused(
            "{ let v: [i64, null] = i64:42; select v { i64 as _ => i64:1, \
             null as _ => i64:0 } }",
        )
        .await;
        agree_fused(
            "{ let v: [i64, null] = i64:42; select v { null as _ => i64:0, \
             i64 as _ => i64:1 } }",
        )
        .await;
        // nullable result
        agree_fused(
            "{ let v: [i64, null] = i64:42; select v { i64 as _ => i64:1, \
             null as _ => null } }",
        )
        .await;
        agree_fused(
            "{ let v: [`Add(i64), `Neg] = `Add(i64:3); \
             select v { `Add(n) => n + i64:1, `Neg => i64:0 } }",
        )
        .await;
        agree_fused(
            "{ let v: [`Add(i64), `Neg] = `Neg; \
             select v { `Add(n) => n + i64:1, `Neg => i64:0 } }",
        )
        .await;
        // computed scrutinee, evaluated once
        agree_fused(
            "{ let x = i64:5; select (x * i64:2) { i64:10 => i64:1, \
             _ => i64:0 } }",
        )
        .await;
        // bottom scrutinee with an irrefutable final arm
        agree_fused("{ let x = i64:0; select (i64:10 / x) { n => n + i64:1 } }").await;
        // bool-literal pair
        agree_fused("{ let b = true; select b { true => i64:1, false => i64:0 } }").await;
        agree_fused("{ let b = false; select b { true => i64:1, false => i64:0 } }")
            .await;
        agree_fused(r#"{ let x = i64:1; select x { i64:0 => "zero", _ => "other" } }"#)
            .await;
        agree_fused(
            "{ let x = i64:5; select (select x { i64:0 => i64:1, \
             n => n + i64:1 }) { i64:6 => i64:100, m => m } }",
        )
        .await;
    }

    /// String interpolation and checked arithmetic. Checked overflow /
    /// div-by-zero is a catchable error value, never bottom.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_string_checked_probes() {
        agree_fused(r#"{ let x = i64:7; "x is [x]" }"#).await;
        agree_fused(r#"{ let a = "foo"; let b = i64:2; "[a]-[b]!" }"#).await;
        agree_fused(r#"{ let a = "foo"; let b = "bar"; "[a][b]" }"#).await;
        agree_fused(r#"{ let f = f64:1.5; let b = true; "f=[f] b=[b]" }"#).await;
        agree_fused(r#""n=[i64:42]""#).await;
        // a non-scalar part: the interpolation node-walks
        agree(r#"{ let a = [i64:1, i64:2]; "e=[a[0]]" }"#).await;
        agree_fused("{ let x = i64:5; (x +? i64:3)$ }").await;
        agree_fused("{ let x = i64:10; (x -? i64:3)$ * (i64:2 *? i64:3)$ }").await;
        agree_fused("{ let x = i64:10; (x %? i64:3)$ }").await;
        // overflow yields the ArithError value
        agree_fused("i64:9223372036854775807 +? i64:1").await;
        agree_fused("is_err(i64:9223372036854775807 +? i64:1)").await;
        // checked div0 flows as an error value
        agree_fused("is_err(i64:0 /? i64:0)").await;
        // overflow through `$` drops: bottom
        agree_fused("(i64:9223372036854775807 +? i64:1)$").await;
        // checked arith inside a select
        agree_fused(
            "{ let x = i64:6; select (x +? i64:1) { i64 as n => n * i64:2, \
             _ => i64:0 } }",
        )
        .await;
        // a may-bottom interpolated part
        agree_fused_clean(r#"{ let x = i64:5; "v=[(x +? i64:1)$]" }"#).await;
    }

    /// Composite lets and accessors fuse as one region.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_composite_probes() {
        agree_fused("{ let t = (i64:1, i64:2); t.0 + t.1 }").await;
        agree_fused("{ let s = { a: i64:4, b: i64:5 }; s.a + s.b }").await;
    }

    /// Inline `array::map` emission.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_map_probes() {
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; array::map(a, |x| x * i64:2) }",
        )
        .await;
        // composite out
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; array::map(a, |x| (x, x * i64:2)) }",
        )
        .await;
        // composite element
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |p| p.0 + p.1) }",
        )
        .await;
        // nullable out
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             array::map(a, |x| select x { i64:1 => i64:10, _ => null }) }",
        )
        .await;
        // capture
        agree_fused_clean(
            "{ let k = i64:10; let a = [i64:1, i64:2]; \
             array::map(a, |x| x * k) }",
        )
        .await;
        // nested map-in-map
        agree_fused_clean(
            "{ let a = [[i64:1, i64:2], [i64:3]]; \
             array::map(a, |row| array::map(row, |x| x + i64:1)) }",
        )
        .await;
        agree_fused_clean(r#"{ let a = [i64:1, i64:2]; array::map(a, |x| "v[x]") }"#)
            .await;
        // qop in the body
        agree_fused_clean("{ let a = [i64:1, i64:2]; array::map(a, |x| (x +? i64:1)$) }")
            .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; array::map((a[1..])$, |x| x) }",
        )
        .await;
        // destructured callback
        agree_fused_clean("{ let a = [(i64:1, i64:2)]; array::map(a, |(k, v)| k + v) }")
            .await;
    }

    /// Inline `array::filter` emission. A may-bottom predicate de-fuses
    /// at build time: there is no runtime keep-vs-drop answer for it.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_filter_probes() {
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter(a, |x| x > i64:2) }",
        )
        .await;
        agree_fused_clean("{ let a = [true, false, true]; array::filter(a, |x| x) }")
            .await;
        // composite element
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::filter(a, |p| p.0 > p.1) }",
        )
        .await;
        agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter(a, |x| x > k) }",
        )
        .await;
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter(a, |x| select x { i64:2 => false, _ => true }) }",
        )
        .await;
        // statically may-bottom predicate with no zero present
        agree_fused_clean(
            "{ let a = [i64:1, i64:5, i64:20]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // with an actual 0 the output never fires in any mode
        agree_fused_clean(
            "{ let a = [i64:0, i64:1, i64:5]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // string element
        agree_fused_clean(r#"{ let a = ["aa", "b"]; array::filter(a, |s| s == "aa") }"#)
            .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter((a[1..])$, |x| x > i64:1) }",
        )
        .await;
        // destructured predicate
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; array::filter(a, |(k, v)| k < v) }",
        )
        .await;
    }

    /// Inline `array::fold` emission. A may-bottom init or body
    /// de-fuses at build time.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_fold_probes() {
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::fold(a, i64:0, |acc, x| acc + x) }",
        )
        .await;
        agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::fold(a, k * i64:10, |acc, x| acc + x * k) }",
        )
        .await;
        // composite element
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::fold(a, i64:0, |acc, p| acc + p.0 * p.1) }",
        )
        .await;
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold(a, i64:0, |acc, x| \
               select x { i64:2 => acc, _ => acc + x }) }",
        )
        .await;
        // outer binding with the same name as the acc, used as the init
        agree_fused_clean(
            "{ let acc = i64:100; let a = [i64:1, i64:2]; \
             array::fold(a, acc, |acc, x| acc + x) }",
        )
        .await;
        // HOF call in operand position
        agree_fused_clean(
            "{ let k = i64:100; let a = [i64:1, i64:2]; \
             k + array::fold(a, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // HOF in a select arm
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; let x = i64:1; \
             select x { \
               i64:1 => array::fold(a, i64:0, |acc, y| acc + y), \
               _ => i64:0 } }",
        )
        .await;
        // HOF as an array-literal element
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             [array::fold(a, i64:0, |acc, x| acc + x), i64:5] }",
        )
        .await;
        // statically may-bottom body: de-fuses
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             array::fold(a, i64:100, |acc, x| acc / x) }",
        )
        .await;
        // statically may-bottom init
        agree_fused_clean(
            "{ let n = i64:2; let a = [i64:1, i64:2]; \
             array::fold(a, i64:10 / n, |acc, x| acc + x) }",
        )
        .await;
        // string accumulator
        agree_fused_clean(
            r#"{ let a = [i64:1, i64:2]; array::fold(a, "", |acc, x| "[acc][x]") }"#,
        )
        .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold((a[1..])$, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // destructured callback
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; \
             array::fold(a, i64:0, |acc, (k, v)| acc + k * v) }",
        )
        .await;
    }

    /// Inline `array::flat_map` emission.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_flat_map_probes() {
        // scalar element, fresh array body
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x, x * i64:10]) }",
        )
        .await;
        // composite element flattened to its fields
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::flat_map(a, |p| [p.0, p.1]) }",
        )
        .await;
        // capture
        agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x * k]) }",
        )
        .await;
        // borrowed body source, cloned per iteration
        agree_fused_clean(
            "{ let b = [i64:9]; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| b) }",
        )
        .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::flat_map((a[1..])$, |x| [x]) }",
        )
        .await;
        // destructured callback
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; \
             array::flat_map(a, |(k, v)| [k, v]) }",
        )
        .await;
    }

    /// Cross-kernel lambda calls: callee kernels, closure-converted
    /// captures, self-recursion and the tail rebind-and-jump.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_lambda_call_probes() {
        agree_fused_clean("{ let f = |x: i64| x * i64:2; f(i64:21) }").await;
        agree_fused_clean("{ let f = |x: i64| x + i64:1; f(i64:1) + f(i64:2) }").await;
        // scalar capture, closure-converted
        agree_fused_clean("{ let k = i64:10; let f = |x: i64| x * k; f(i64:4) }").await;
        agree_fused_clean("{ let f = |x: f64| x * f64:2.5; f(f64:4.0) }").await;
        // composite arg, owned caller-side
        agree_fused_clean(
            "{ let f = |a: Array<i64>| a[i64:0]$ + a[i64:1]$; \
             f([i64:1, i64:2, i64:3]) }",
        )
        .await;
        // HOF inside the callee body
        agree_fused_clean(
            "{ let f = |a: Array<i64>| \
               array::fold(a, i64:0, |acc, x| acc + x); \
             f([i64:1, i64:2, i64:3]) }",
        )
        .await;
        // labeled arg, explicit and defaulted
        agree_fused_clean(
            "{ let f = |#k: i64 = i64:5, x: i64| x + k; \
             f(#k: i64:3, i64:2) + f(i64:2) }",
        )
        .await;
        // nullable return from a select body
        agree_fused_clean(
            "{ let f = |x: i64| -> [i64, null] \
               select x { i64:0 => null, _ => x }; \
             f(i64:5) }",
        )
        .await;
        // self-recursion: native recursion
        agree_fused_clean(
            "{ let rec f = |n: i64| -> i64 \
               select n { i64:0 => i64:0, _ => n + f(n - i64:1) }; \
             f(i64:10) }",
        )
        .await;
        // double recursion
        agree_fused_clean(
            "{ let rec fib = |n: i64| -> i64 \
               select n { i64:0 => i64:0, i64:1 => i64:1, \
               _ => fib(n - i64:1) + fib(n - i64:2) }; \
             fib(i64:15) }",
        )
        .await;
        // tail recursion: a native loop; depth kept stack-safe for the
        // node-walk (the fused-only probe below runs it at 5M)
        agree_fused_clean(
            "{ let rec lp = |n: i64, acc: i64| -> i64 \
               select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) }; \
             lp(i64:500, i64:0) }",
        )
        .await;
        // tail recursion with a capture: the capture slot is loop-invariant
        agree_fused_clean(
            "{ let k = i64:3; let rec f = |n: i64| -> i64 \
               select n { i64:0 => k, _ => f(n - i64:1) }; \
             f(i64:4) }",
        )
        .await;
        // shadowed lambda name: f2's body calls the outer f; de-fuses
        agree_fused_clean(
            "{ let f = |x: i64| -> i64 x + i64:1; \
             let f = |n: i64| -> i64 f(n) * i64:2; f(i64:3) }",
        )
        .await;
        // lambda call inside a HOF callback body
        agree_fused_clean(
            "{ let f = |x: i64| x * i64:2; let a = [i64:1, i64:2]; \
             array::map(a, |x| f(x)) }",
        )
        .await;
    }

    /// `code` under the JIT with its fusion stats, holding as many
    /// activations as it reaches whatever budget the process set.
    async fn run_deep(code: &str, timeout: Duration) -> (Outcome, Option<FusionStats>) {
        let opts =
            RunOpts { stats: true, activations: Some(u64::MAX), ..RunOpts::default() };
        let run = run_subject(
            code,
            Mode::Jit,
            Route::InLanguage,
            SessionImage::None,
            timeout,
            opts,
        )
        .await;
        (run.outcome, run.stats)
    }

    /// A 5M-deep tail recursion is only runnable as the compiled
    /// rebind-and-jump loop (the node-walk overflows around ~50k), so
    /// only Jit runs; `fused > 0` pins that the loop compiled.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_deep_tail_probe() {
        let code = "{ let rec lp = |n: i64, acc: i64| -> i64 \
                     select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) }; \
                     lp(i64:5000000, i64:0) }";
        let (out, stats) = run_deep(code, Duration::from_secs(30)).await;
        let stats =
            stats.unwrap_or_else(|| panic!("the runtime answered no stats: {out:?}"));
        assert!(
            stats.fused > 0,
            "deep tail probe did not fuse (attempted={}): {:?}",
            stats.attempted,
            stats.failed,
        );
        let expected = Outcome::single(Value::I64(12_500_002_500_000));
        assert!(
            out.agrees_with(&expected),
            "deep tail loop produced {out:?}, expected {expected:?}"
        );
    }

    /// Depth is bounded by memory, not a counter: a 2M-deep non-tail
    /// recursion re-enters through the spill thunk inside the red zone.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_deep_nontail_probe() {
        let code = "{ let rec f = |n: i64| -> i64 \
                     select n { i64:0 => i64:0, _ => n + f(n - i64:1) }; \
                     f(i64:2000000) }";
        let (out, stats) = run_deep(code, Duration::from_secs(60)).await;
        let stats =
            stats.unwrap_or_else(|| panic!("the runtime answered no stats: {out:?}"));
        assert!(
            stats.fused > 0,
            "deep non-tail probe did not fuse (attempted={}): {:?}",
            stats.attempted,
            stats.failed,
        );
        let expected = Outcome::single(Value::I64(2_000_001_000_000));
        assert!(
            out.agrees_with(&expected),
            "deep non-tail recursion produced {out:?}, expected {expected:?}"
        );
    }

    /// Destructured `|(k, v)|` callbacks.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_destructure_probes() {
        // mixed-prim leaves
        agree_fused_clean(
            "{ let a = [(i64:1, f64:2.5), (i64:3, f64:0.5)]; \
             array::map(a, |(k, v)| v) }",
        )
        .await;
        // sparse leaves: `_` positions get no bind
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |(k, _)| k * i64:10) }",
        )
        .await;
        // find with a destructured predicate yields the whole tuple
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |(k, v)| k > v) }",
        )
        .await;
        // 3-leaf tuple through fold
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2, i64:3), (i64:4, i64:5, i64:6)]; \
             array::fold(a, i64:0, |acc, (x, y, z)| acc + x * y + z) }",
        )
        .await;
        // composite leaf
        agree_fused_clean(
            "{ let a = [((i64:1, i64:2), i64:3)]; \
             array::map(a, |(p, x)| x) }",
        )
        .await;
    }

    /// Inline `array::filter_map` emission.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_filter_map_probes() {
        // keep evens doubled
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter_map(a, |x| \
               select x % i64:2 { i64:0 => `Keep(x * i64:10), _ => null }) }",
        )
        .await;
        // capture
        agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter_map(a, |x| \
               select x { i64:2 => `Keep(x * k), _ => null }) }",
        )
        .await;
        // composite element
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; \
             array::filter_map(a, |p| \
               select p.0 { i64:1 => `Keep(p.1), _ => null }) }",
        )
        .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter_map((a[1..])$, |x| \
               select x { i64:2 => `Keep(x), _ => null }) }",
        )
        .await;
    }

    /// Inline `array::find` emission: early exit, `Nullable<elem>`
    /// result; a may-bottom predicate de-fuses at build.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_find_probes() {
        // found
        agree_fused_clean(
            "{ let a = [i64:1, i64:5, i64:3]; \
             array::find(a, |x| x > i64:2) }",
        )
        .await;
        // not found
        agree_fused_clean("{ let a = [i64:1, i64:2]; array::find(a, |x| x > i64:9) }")
            .await;
        // composite element
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |p| p.0 > p.1) }",
        )
        .await;
        // may-bottom predicate: de-fuses
        agree_fused_clean(
            "{ let a = [i64:1, i64:5]; \
             array::find(a, |x| i64:10 / x > i64:4) }",
        )
        .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find((a[1..])$, |x| x > i64:1) }",
        )
        .await;
    }

    /// Inline `array::find_map` emission.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_find_map_probes() {
        // found: first even, doubled
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // not found
        agree_fused_clean(
            "{ let a = [i64:1, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x, _ => null }) }",
        )
        .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find_map((a[1..])$, |x| \
               select x { i64:2 => x, _ => null }) }",
        )
        .await;
    }

    /// Fresh-producer arrays (literals, slices, inlined-HOF results)
    /// feed the loop scaffolds directly; HOF-of-HOF args fuse as
    /// multi-loop single kernels.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_owned_input_probes() {
        // array literal as the direct argument
        agree_fused_clean("array::map([i64:1, i64:2, i64:3], |x| x * i64:2)").await;
        // filter over an inlined map: two loops, one kernel
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter(array::map(a, |x| x * i64:2), |x| x > i64:2) }",
        )
        .await;
        // fold over an inlined map
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold(array::map(a, |x| x * x), i64:0, |acc, x| acc + x) }",
        )
        .await;
        // find over an inlined filter
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::find(array::filter(a, |x| x % i64:2 == i64:0), \
               |x| x > i64:2) }",
        )
        .await;
        // init's output into flat_map
        agree_fused_clean("array::flat_map(array::init(i64:3, |i| i), |x| [x, x])").await;
        // the outer map's body bottom-aborts mid-loop while the inner
        // map's result is adopted: the pending cleanup must free it
        agree_fused_clean(
            "{ let a = [i64:9223372036854775807, i64:1]; \
             array::map(array::map(a, |x| x), |x| (x +? i64:1)$) }",
        )
        .await;
    }

    /// Inline `array::init` emission.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_init_probes() {
        agree_fused_clean("array::init(i64:4, |i| i * i)").await;
        agree_fused_clean("array::init(i64:3, |i| (i, i + i64:1))").await;
        agree_fused_clean("{ let k = i64:10; array::init(i64:3, |i| i * k) }").await;
        agree_fused_clean("{ let n = i64:2; array::init(n + i64:1, |i| i) }").await;
        // negative n clamps to the empty array
        agree_fused_clean("array::init(i64:0 - i64:2, |i| i)").await;
        // may-bottom n: de-fuses
        agree_fused_clean("{ let d = i64:2; array::init(i64:4 / d, |i| i) }").await;
    }

    /// Broad differential sweep over generated programs: Interp and Jit
    /// must agree on every one. Deterministic seed.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn jit_generated_sweep() {
        use crate::{generate::gen_program, mutate::Rng};
        let t = Duration::from_secs(10);
        let mut rng = Rng::new(0xD17EC7);
        let mut fused = 0usize;
        let mut budget_skipped = 0usize;
        for _ in 0..120 {
            let code = gen_program(&mut rng);
            let (interp, (direct, stats)) = tokio::join!(
                run_program(&code, Mode::Interp, t),
                run_program_with_stats(&code, Mode::Jit, t),
            );
            fused += stats.map_or(0, |s| s.fused);
            // at the strength the oracle compares this program at: an
            // async reply lands in whichever cycle the scheduler allows
            let strength = Strength::of(Pair::Engine, oracle_tier(&code));
            // assert only when interp agrees with itself, mirroring the
            // oracle's double-run guard
            if !interp.agrees_at(&direct, strength) {
                // a Timeout on either side is the budget talking under
                // suite load: re-check at 4x before believing it
                if matches!(interp, Outcome::Timeout(_))
                    || matches!(direct, Outcome::Timeout(_))
                {
                    let big = t * 4;
                    let (i2, j2) = tokio::join!(
                        run_program(&code, Mode::Interp, big),
                        run_program(&code, Mode::Jit, big),
                    );
                    if i2.agrees_at(&j2, strength)
                        || matches!(i2, Outcome::Timeout(_))
                        || matches!(j2, Outcome::Timeout(_))
                    {
                        budget_skipped += 1;
                        continue;
                    }
                }
                let interp2 = run_program(&code, Mode::Interp, t).await;
                if !interp.agrees_at(&interp2, strength) {
                    continue; // nondeterministic — not a backend bug
                }
                panic!(
                    "Interp vs Jit diverge for `{code}`: \
                     {interp:?} vs {direct:?}"
                );
            }
        }
        // the live coverage number; budget skips reported beside it
        eprintln!(
            "sweep: {fused} regions fused across 120 programs              ({budget_skipped} skipped on budget)"
        );
    }

    /// Every hand seed passes the check: a refused seed only ever seeds
    /// compile errors.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn hand_seeds_check() {
        let t = Duration::from_secs(30);
        let mut refused = Vec::new();
        for seed in corpus::SEEDS {
            if let Err(e) = check_only(seed, t).await {
                refused.push(format!("{seed}\n{e}"));
            }
        }
        assert!(refused.is_empty(), "hand seeds refused:\n{}", refused.join("\n\n"));
    }

    /// A generated filter_map callback that keeps a payload as
    /// `` `Keep(..) `` passes the check.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn generated_keep_bodies_check() {
        let mut rng = mutate::Rng::new(0x4ee9);
        let keeps: Vec<String> =
            std::iter::repeat_with(|| generate::gen_program(&mut rng))
                .take(20_000)
                .filter(|p| p.contains("=> `Keep("))
                .take(8)
                .collect();
        assert_eq!(keeps.len(), 8);
        let mut refused = Vec::new();
        for p in &keeps {
            if let Err(e) = check_only(p, Duration::from_secs(30)).await {
                refused.push(format!("{p}\n{e}"));
            }
        }
        assert!(refused.is_empty(), "refused:\n{}", refused.join("\n\n"));
    }

    /// Generated programs that mint a reference in a select arm or a
    /// callback slot pass the check and agree across modes.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn generated_minted_refs_agree() {
        let mut rng = mutate::Rng::new(0x3ef5);
        let cfg = generate::GenCfg::default();
        let (mut arm, mut slot) = (Vec::new(), Vec::new());
        for _ in 0..20_000 {
            if arm.len() == 4 && slot.len() == 4 {
                break;
            }
            let (p, s) = generate::gen_program_stats(&cfg, &mut rng);
            if s.ref_in_arm && arm.len() < 4 {
                arm.push(p);
            } else if s.ref_in_slot && slot.len() < 4 {
                slot.push(p);
            }
        }
        assert_eq!((arm.len(), slot.len()), (4, 4));
        let minted = arm.iter().chain(&slot);
        let t = Duration::from_secs(30);
        let mut failed = Vec::new();
        for p in minted {
            if let Err(e) = check_only(p, t).await {
                failed.push(format!("{p}\nrefused: {e}"));
            } else if let Some(d) = check(p, t).await {
                failed.push(format!("{p}\ndiverged: {d:?}"));
            }
        }
        assert!(failed.is_empty(), "{}", failed.join("\n\n"));
    }

    /// Every scheduled hand seed agrees across modes at trace strength:
    /// the injection driver's permanent gate.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn scheduled_seed_sweep() {
        let t = Duration::from_secs(10);
        for seed in corpus::all_seeds() {
            if !seed.starts_with(schedule::HEADER_PREFIX) {
                continue;
            }
            if let Some(d) = check(seed, t).await {
                panic!(
                    "scheduled seed diverges:\n{seed}\n  interp={:?}\n  jit={:?}",
                    d.reference, d.tested
                );
            }
        }
    }
}

/// Probes for the runtime trace primitives (`GXHandle::{trace_start,
/// trace_wait_idle}`): a value emitted during the compile cycle is in
/// the trace, a bottom program resolves instantly, the injection
/// contract fuses and traces identically under both modes, a runaway is
/// cut deterministically by the cycle cap, and segments drain.
#[cfg(test)]
mod trace_probes {
    use super::*;
    use graphix_compiler::{Scope, expr::ModPath};
    use graphix_rt::{TraceEvent, TraceSegment};

    /// Drive one traced run: `trace_start`, compile (`prelude` top-level
    /// decls, then the `{ mod test; test::result }` wrap over `program`),
    /// wait for epoch 0, then per epoch set every named root input and
    /// wait again. Returns one segment per epoch and the program's own
    /// [`FusionStats`] delta. Input decls go in `prelude`, where they are
    /// reachable by name from root.
    async fn drive_traced(
        mode: Mode,
        prelude: &str,
        program: &str,
        max_events: usize,
        max_cycles: u64,
        epochs: &[&[(&str, i64)]],
    ) -> (Vec<TraceSegment>, FusionStats) {
        let (tx, rx) = mpsc::channel(1024);
        let tbl = AHashMap::from_iter([(
            Path::from("/test.gx"),
            graphix_compiler::expr::VfsEntry::from(ArcStr::from(format!(
                "use super::*; {program}"
            ))),
        )]);
        let resolver = VfsResolver::new(tbl);
        let ctx = async {
            TestCtx::start(
                config(tx, REGISTER, vec![resolver], move |ctx| {
                    ctx.control.set_par_mode(fuzz_par(mode));
                    ctx.control.set_prune_mode(mode.prune())
                })?
                .flags(mode.flags()),
            )
            .await
        }
        .await
        .expect("runtime init");
        let base = ctx.fusion_stats().await.expect("base stats");
        ctx.rt.trace_start(max_events, max_cycles).expect("trace_start");
        let text = format!("{prelude}\n{{ mod test; test::result }}");
        let comp = ctx.rt.compile(ArcStr::from(text)).await.expect("compile");
        let mut stats = ctx.fusion_stats().await.expect("stats");
        stats.attempted -= base.attempted;
        stats.fused -= base.fused;
        stats.failed.drain(..base.failed.len());
        let mut segs = vec![ctx.rt.trace_wait_idle().await.expect("epoch 0")];
        let mut refs = AHashMap::new();
        for sets in epochs {
            for (name, v) in sets.iter() {
                if !refs.contains_key(name) {
                    let r = ctx
                        .rt
                        .compile_ref_by_name(
                            &comp.env,
                            &Scope::root(),
                            &ModPath::from([*name]),
                        )
                        .await
                        .unwrap_or_else(|e| panic!("no input {name}: {e}"));
                    refs.insert(*name, r);
                }
                refs.get_mut(name).unwrap().set(*v).expect("set");
            }
            segs.push(ctx.rt.trace_wait_idle().await.expect("epoch segment"));
        }
        // `comp` and the refs hold GXHandle clones and must drop before
        // the channel receiver
        drop(refs);
        drop(comp);
        ctx.shutdown().await;
        drop(rx);
        (segs, stats)
    }

    /// Project a segment onto mode-comparable data: (cycle relative to
    /// the segment's first event, value), `None` for the `Compiled`
    /// anchor. ExprIds are process-local and dropped.
    fn shape(seg: &TraceSegment) -> Vec<(u64, Option<Value>)> {
        let base = match seg.events.first() {
            None => 0,
            Some(
                TraceEvent::Compiled { cycle, .. } | TraceEvent::Updated { cycle, .. },
            ) => *cycle,
        };
        seg.events
            .iter()
            .map(|e| match e {
                TraceEvent::Compiled { cycle, .. } => (*cycle - base, None),
                TraceEvent::Updated { cycle, value, .. } => {
                    (*cycle - base, Some(value.clone()))
                }
            })
            .collect()
    }

    fn shapes(segs: &[TraceSegment]) -> Vec<Vec<(u64, Option<Value>)>> {
        segs.iter().map(shape).collect()
    }

    /// A synchronous program's value, emitted during the compile cycle,
    /// is in the trace at offset 0 under both modes.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_captures_sync_result() {
        let prog = "let result = i64:2 + i64:3";
        let (i, _) = drive_traced(Mode::Interp, "", prog, 512, 64, &[]).await;
        let (j, _) = drive_traced(Mode::Jit, "", prog, 512, 64, &[]).await;
        let want = vec![vec![(0, None), (0, Some(Value::I64(5)))]];
        assert_eq!(shapes(&i), want, "interp trace");
        assert_eq!(shapes(&j), want, "jit trace");
        for s in i.iter().chain(j.iter()) {
            assert!(!s.capped_cycles && !s.capped_events, "no caps: {s:?}");
        }
    }

    /// One panic site reached with other values, dumped or not, is one
    /// crash class, and a crash's key minimizes in children.
    #[test]
    fn crash_classes_ignore_values() {
        let a = crash_class(
            "signal: 6 (SIGABRT) (core dumped) | range start index 1 out of range for \
             slice of length 0 | note: run with `RUST_BACKTRACE=1` environment variable",
        );
        let b = crash_class(
            "signal: 6 (SIGABRT) | range start index 2 out of range for slice of length 1 \
             | note: run with `RUST_BACKTRACE=1` environment variable",
        );
        assert_eq!(a, b);
        assert_eq!(
            a,
            "CRASH signal: N (SIGABRT) | range start index N out of range for slice of length N"
        );
        let t = Duration::from_secs(1);
        assert!(matches!(Judge::for_key(Some(&a), t), Judge::Isolated(_)));
        assert!(matches!(Judge::for_key(Some("Engine (..)"), t), Judge::InProcess(_)));
    }

    /// Two panic sites with one message are two classes; a crash with no
    /// words claims per program, and a recorded crash's claim counts on
    /// reload.
    #[test]
    fn crash_claims_tell_causes_apart() {
        let at = |site: &str| {
            format!(
                "signal: 6 (SIGABRT) | panicked at {site} | called `Option::unwrap()` on a \
                 `None` value | note: run with `RUST_BACKTRACE=1` environment variable"
            )
        };
        let (a, b) = (at("src/a.rs:12:5"), at("src/a.rs:40:5"));
        assert_ne!(crash_class(&a), crash_class(&b));
        assert!(
            crash_class(&a).contains("panicked at src/a.rs:12:5 | "),
            "{}",
            crash_class(&a)
        );
        assert_eq!(crash_claim(&a, "1"), crash_class(&a));
        let hang = "HANG (outer deadline)";
        assert_eq!(crash_claim(hang, "f(1)"), crash_claim(hang, "f(2)"));
        assert_ne!(crash_claim(hang, "f(1)"), crash_claim(hang, "g(1)"));
        let dir = tempfile::tempdir().unwrap();
        let corpus = Corpus::load(dir.path());
        let claim = crash_claim(hang, "f(1)");
        assert!(corpus.claim(&claim));
        assert!(corpus.record_crash("f(1)", hang, &claim, None));
        let reloaded = Corpus::load(dir.path());
        assert_eq!(reloaded.keys.lock().unwrap().get(&claim), Some(&1));
    }

    /// A marker names a builtin, not the tail of a longer name, a string's
    /// text or a binding's name; what a string interpolates is code.
    #[test]
    fn oracle_markers_are_whole_names() {
        assert_eq!(oracle_tier("{ let s = \"throttle\"; s }"), OracleTier::Exact);
        assert_eq!(oracle_tier("{ let s = r#\"rand::\"#; s }"), OracleTier::Exact);
        assert_eq!(oracle_tier("{ let local_addr = 1; local_addr }"), OracleTier::Exact);
        assert_eq!(oracle_tier("{ let s = \"\\\"rand::\"; s }"), OracleTier::Exact);
        assert_eq!(oracle_tier("rand::rand()"), OracleTier::Excluded);
        let interp = "\"[a[0]]-[json::read(\"1\")$]\"";
        assert_eq!(oracle_tier(interp), OracleTier::FinalValues);
        let template = "\"\"\"[x] \\[pack::read(b)$]\"\"\"";
        assert_eq!(oracle_tier(template), OracleTier::FinalValues);
        assert_eq!(oracle_tier("\"\\[json::read(b)]\""), OracleTier::Exact);
        assert_eq!(oracle_tier("r\"[json::read(b)]\""), OracleTier::Exact);
        let read = "sys::fs::read_all(\"/x\")";
        assert_eq!(oracle_tier(read), OracleTier::FinalValues);
        let err = "{ let e = error(`X); sys::fs::read_all(\"/x\") }";
        assert_eq!(oracle_tier(err), OracleTier::FinalValues);
        let pure = "sys::time::add(datetime:\"2020-01-01T00:00:00Z\", duration:1.s)";
        assert_eq!(oracle_tier(pure), OracleTier::Exact);
        let summed = "{ let t = m0::tsum(i64:1); sys::fs::read_all(\"/x\") }";
        assert_eq!(oracle_tier(summed), OracleTier::FinalValues);
        let counted = "{ let c = count(i64:1); sys::fs::read_all(\"/x\") }";
        assert_eq!(oracle_tier(counted), OracleTier::Excluded);
        let queued = "{ let qf = queuefn(#trigger: 1, |x: i64| x); qf(json::read(b)$) }";
        assert_eq!(oracle_tier(queued), OracleTier::Excluded);
    }

    /// A session pair's labels name its mode, route and session, the
    /// forked modes' included.
    #[test]
    fn session_labels_name_the_route_and_session() {
        let labels = |p: Pair| {
            let (a, b) = p.labels();
            (a.to_string(), b.to_string())
        };
        let cold = labels(Pair::Cold(Mode::JitPar, Route::Dispatch));
        assert_eq!(
            cold,
            ("jit/par/dispatch/nocache".into(), "jit/par/dispatch/cold".into())
        );
        let warm = labels(Pair::Warm(Mode::Par, Route::InLanguage));
        assert_eq!(warm, ("interp/par/cold".into(), "interp/par/warm".into()));
        assert_eq!(labels(Pair::Par(Mode::JitPar)), ("interp".into(), "jit/par".into()));
        let jit = labels(Pair::Cold(Mode::Jit, Route::InLanguage));
        assert_eq!(jit, ("jit/nocache".into(), "jit/cold".into()));
    }

    /// A run contained after the runtime's construction wrote its image
    /// there, so a failed write is the warm outcome, never the cold one.
    #[test]
    fn contained_runs_report_a_failed_image_write() {
        let failed = || Err("the program image write failed: x".to_string());
        for c in [
            Containment::Deadline,
            Containment::StackBudget,
            Containment::ActivationBudget,
        ] {
            let warm = restorable(&Outcome::Timeout(c), failed());
            assert!(matches!(warm, Err(Outcome::RuntimeErr(_))), "{c:?}: {warm:?}");
        }
        let compile = Outcome::Timeout(Containment::Compile);
        let warm = restorable(&compile, no_image());
        assert!(matches!(warm, Err(Outcome::Timeout(Containment::Compile))), "{warm:?}");
        let rejected = Outcome::CompileErr("e".into());
        assert!(matches!(restorable(&rejected, no_image()), Err(Outcome::CompileErr(_))));
    }

    /// A FinalValues epoch spans every settle round, so a print that
    /// follows an async reply landing after the first idle is captured in
    /// the epoch it happened in.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn settle_rounds_capture_late_prints() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("big.txt");
        std::fs::write(&path, format!("{}\n", "x".repeat(99)).repeat(200_000)).unwrap();
        let prog = format!(
            "{{ let n = str::len(sys::fs::read_all(\"{}\")$); println(\"got [n]\"); n }}",
            path.display()
        );
        assert_eq!(oracle_tier(&prog), OracleTier::FinalValues);
        let Outcome::Trace(t) =
            run_program(&prog, Mode::Interp, std::time::Duration::from_secs(60)).await
        else {
            panic!("{prog}: no trace");
        };
        let printed: Vec<_> =
            t.stdout.iter().map(|l| (l.epoch, l.text.as_str())).collect();
        assert_eq!(printed, [(0, "got 20000000\n")], "{prog}");
    }

    /// A comment above the body is legal, the reject marker included: a
    /// well-typed program under `// expect: reject` runs, so a pin whose
    /// refusal regressed to acceptance fails the gate.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn leading_comments_do_not_reject() {
        for prog in [
            "// expect: reject\ni64:2 + i64:3",
            "// a note\n{ let x = i64:1; x + i64:1 }",
        ] {
            let (d, v) = check_verdict(prog, std::time::Duration::from_secs(60)).await;
            assert!(d.is_none(), "{prog}: {d:?}");
            assert!(matches!(v, Verdict::Ran), "{prog}: {v:?}");
        }
    }

    /// A reference's bind id and a lambda's minted id differ between runs;
    /// the trace shows neither, so such a program compares like any other.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn references_and_abstract_fns_compare() {
        for prog in [
            "{ let x = i64:5; (&x, &x, &i64:1, x + i64:1) }",
            "type F = Abstract<fn(x: i64) -> i64>;\n{ let f = F(|x: i64| x + i64:1); (f, i64:2) }",
        ] {
            let (d, v) = check_verdict(prog, std::time::Duration::from_secs(60)).await;
            assert!(d.is_none(), "{prog}: {d:?}");
            assert!(matches!(v, Verdict::Ran), "{prog}: {v:?}");
        }
    }

    /// A trace whose observable is a first-class function value is
    /// compile-stable (fn values normalize to their source).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn selfcheck_fn_valued_trace_is_stable() {
        let prog = r#"
{
  let rec sum_to = |n, acc| select n {
    i64:0 => acc,
    _ => sum_to(n - i64:1, acc)
  };
  sum_to(i64:3, buffer::to_string)
}
"#;
        let flaky = selfcheck_one(prog, std::time::Duration::from_secs(60)).await;
        assert_eq!(flaky, Vec::<&'static str>::new());
    }

    /// A bottom program resolves instantly with an anchor-only trace.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_bottom_is_instant_and_empty() {
        let prog = "let result = i64:1 / i64:0";
        let (i, _) = drive_traced(Mode::Interp, "", prog, 512, 64, &[]).await;
        let (j, _) = drive_traced(Mode::Jit, "", prog, 512, 64, &[]).await;
        let want = vec![vec![(0, None)]];
        assert_eq!(shapes(&i), want, "interp trace");
        assert_eq!(shapes(&j), want, "jit trace");
    }

    /// The injection contract: a root-level `let in0: T = default; in0
    /// <- never(default)` is a settable input that fuses under Jit, each
    /// epoch's set flows through, traces agree across modes, and
    /// segments drain.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_d4_injection_contract() {
        let prelude = "let in0: i64 = 0;\nin0 <- never(0);";
        let prog = "let result = in0 * i64:2";
        let epochs: &[&[(&str, i64)]] = &[&[("in0", 21)], &[("in0", 5)]];
        let (i, _) = drive_traced(Mode::Interp, prelude, prog, 512, 64, epochs).await;
        let (j, jstats) = drive_traced(Mode::Jit, prelude, prog, 512, 64, epochs).await;
        let si = shapes(&i);
        let sj = shapes(&j);
        assert_eq!(si, sj, "interp vs jit traces");
        assert_eq!(si.len(), 3, "epoch 0 + 2 injection epochs");
        // epoch 0: one `Compiled` anchor per top-level expr plus the
        // default flowing through
        assert_eq!(
            si[0],
            vec![(0, None), (0, None), (0, None), (0, Some(Value::I64(0)))]
        );
        // injection epochs: result and the input-ref's own echo, same
        // cycle, nothing carried over
        for (seg, (r, in0)) in si[1..].iter().zip([(42, 21), (10, 5)]) {
            let vals: Vec<_> = seg.iter().filter_map(|(_, v)| v.clone()).collect();
            assert_eq!(
                vals,
                vec![Value::I64(r), Value::I64(in0)],
                "epoch events: {seg:?}"
            );
            assert!(seg.iter().all(|(c, _)| *c == seg[0].0), "single-cycle epoch");
        }
        assert!(
            jstats.fused > 0,
            "the never-gated input region must fuse; failures: {:?}",
            jstats.failed
        );
    }

    /// A runaway `x <- x + 1` is cut by the cycle cap at a point that is
    /// a pure function of the program's own event stream, in both modes.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_runaway_cap_determinism() {
        let prog = "let x = i64:0;\nx <- x + i64:1;\nlet result = x";
        let (i1, _) = drive_traced(Mode::Interp, "", prog, 512, 24, &[]).await;
        let (i2, _) = drive_traced(Mode::Interp, "", prog, 512, 24, &[]).await;
        let (j1, _) = drive_traced(Mode::Jit, "", prog, 512, 24, &[]).await;
        let (j2, _) = drive_traced(Mode::Jit, "", prog, 512, 24, &[]).await;
        assert_eq!(shapes(&i1), shapes(&i2), "interp self-determinism");
        assert_eq!(shapes(&j1), shapes(&j2), "jit self-determinism");
        assert_eq!(shapes(&i1), shapes(&j1), "interp vs jit");
        let seg = &i1[0];
        assert!(seg.capped_cycles, "runaway must hit the cycle cap: {seg:?}");
        let vals: Vec<_> = shape(seg).into_iter().filter_map(|(_, v)| v).collect();
        assert_eq!(vals.len(), 24, "one value per active cycle up to the cap");
        assert_eq!(vals[0], Value::I64(0));
        assert_eq!(vals[23], Value::I64(23));
    }

    /// An eventless spinner (a self-connect loop whose only traced
    /// output is permanently bottom) hits the worked-cycle cap in both
    /// modes with identical traces.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_eventless_spinner_caps() {
        let prog = "let x = i64:0;\nx <- x + i64:1;\nlet result = x + (x % i64:0)";
        let (i1, _) = drive_traced(Mode::Interp, "", prog, 512, 24, &[]).await;
        let (i2, _) = drive_traced(Mode::Interp, "", prog, 512, 24, &[]).await;
        let (j1, _) = drive_traced(Mode::Jit, "", prog, 512, 24, &[]).await;
        assert_eq!(shapes(&i1), shapes(&i2), "interp self-determinism");
        assert_eq!(shapes(&i1), shapes(&j1), "interp vs jit");
        let seg = &i1[0];
        assert!(seg.capped_cycles, "spinner must hit the cycle cap: {seg:?}");
        let jseg = &j1[0];
        assert!(jseg.capped_cycles, "jit spinner must cap too: {jseg:?}");
    }

    /// A reference that loops inside one cycle is probed for the stall
    /// whatever its session: a cold run's too, the warm pair's reference.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_cold_reference_that_loops_in_a_cycle_stalls() {
        let code = "{ let rec lp = |n: i64| -> i64 \
                    select n { i64:0 => i64:0, _ => lp(n - i64:1) }; \
                    lp(i64:9000000000000000000) }";
        let budget = Duration::from_secs(10);
        let o = Oracle::new(code, OracleTier::Exact, budget, None);
        let cold =
            Run { mode: Mode::Jit, route: Route::InLanguage, session: Session::Cold };
        let stalled = o.stalled(cold, budget).await;
        assert!(
            matches!(stalled, Some(Outcome::Timeout(Containment::Deadline))),
            "{stalled:?}"
        );
    }

    /// A subject typemorph cannot run comes back unjudged, for the
    /// fresh-process path to record, never as an agreement.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_typemorph_subject_that_cannot_run_is_handed_back() {
        let bad = "// schedule-v1: cap=x events=64; in0=i64:1\n{ let a = in0; a + 1 }";
        let mut out = Vec::new();
        typemorph_work_order(&[bad.to_string()], Duration::from_secs(10), &mut out).await;
        let text = String::from_utf8(out).unwrap();
        assert!(
            text.starts_with(&format!("V 0 U\nP 0 {}\n{bad}\n", bad.len())),
            "{text}"
        );
        assert_eq!(tm_report("HARNESS no runtime\n".into()), Err("no runtime".into()));
        assert_eq!(tm_report("BASE accept\n".into()), Ok("BASE accept\n".into()));
    }

    /// Only a flip both the batch child and the fresh process showed is
    /// confirmed; one only either showed is the unconfirmed class.
    #[test]
    fn a_typeflip_is_confirmed_only_by_the_probe_the_batch_flipped() {
        let flip = |id: &str| Flip { id: id.into(), head: "h".into(), mutant: None };
        let ids = |f: &[Flip]| f.iter().map(|f| f.id.clone()).collect::<Vec<_>>();
        let batch = ["a#1".to_string()];
        let (f, ok) = confirmed_flips(vec![flip("a#1"), flip("b#2")], &batch);
        assert_eq!((ids(&f), ok), (vec!["a#1".into(), "unconfirmed".into()], true));
        let (f, ok) = confirmed_flips(vec![flip("b#2")], &batch);
        assert_eq!((ids(&f), ok), (vec!["unconfirmed".into()], false));
        let (f, ok) = confirmed_flips(vec![flip("a#1")], &batch);
        assert_eq!((ids(&f), ok), (vec!["a#1".into()], true));
        let (f, ok) = confirmed_flips(vec![], &batch);
        assert_eq!((ids(&f), ok), (vec!["unconfirmed".into()], false));
    }

    /// A marked pin that compiles is misdeclared whatever its verdict
    /// says, an unsure or excluded one included.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_marked_pin_that_compiles_is_misdeclared() {
        let prog = format!("{EXPECT_REJECT}\ni64:1");
        for v in [Verdict::Ran, Verdict::Contained, Verdict::Unsure, Verdict::Excluded] {
            let d = misdeclared(&prog, v, Duration::from_secs(10)).await;
            assert!(d.is_some_and(|d| d.pair == Pair::Accepted), "{v}");
        }
        assert!(
            misdeclared(&prog, Verdict::Rejected, Duration::from_secs(10))
                .await
                .is_none()
        );
    }

    /// Both builds refusing a subject is its own batch verdict, which a
    /// campaign counts.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn a_subject_both_builds_refuse_is_counted() {
        let mut got = None;
        let prog = ["i64:1 + \"x\"".to_string()];
        run_batch(&prog, Duration::from_secs(10), |_, v| got = Some(v)).await;
        assert_eq!(got, Some(BatchVerdict::Refused));
    }

    /// A worker child runs below the directory of the files its parent
    /// reads back, so no relative path a subject writes reaches them.
    #[test]
    fn a_subject_cannot_write_its_own_verdict() {
        let mut cmd = child_command();
        let sb = sandbox(&mut cmd);
        let cwd = cmd.as_std().get_current_dir().expect("a cwd").to_path_buf();
        assert!(cwd.is_dir() && cwd.starts_with(sb.path()) && cwd != sb.path());
        report_self_cpu(&sb.path().join("verdict"));
        assert!(child_cpu(sb.path()) > Duration::ZERO);
        assert!(!cwd.join(CPU_REPORT).exists());
    }

    #[test]
    fn selfcheck_labels_are_what_its_children_write() {
        for l in ["interp", "jit", "interp-dispatch", "jit-dispatch", "inconclusive"] {
            assert_eq!(selfcheck_label(l), Some(l));
        }
        assert_eq!(selfcheck_label("par"), None);
    }

    /// The pure time arithmetic reads no clock, so its hang is no
    /// environment's.
    #[test]
    fn pure_time_arithmetic_hangs_are_findings() {
        assert!(!environmental_hang("sys::time::add_dur(d, duration:1.s)"));
        assert!(!environmental_hang("sys::time::diff(a, b)"));
        assert!(environmental_hang("sys::time::timer(duration:1.s, false)"));
        assert!(environmental_hang("rand::rand(#clock: x)"));
    }

    /// `trace_wait_idle` without `trace_start` is an error, not a hang.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_wait_without_start_errors() {
        let (tx, _rx) = mpsc::channel(64);
        let ctx = async {
            TestCtx::start(
                config(tx, REGISTER, vec![], |_| {})?.flags(Mode::Interp.flags()),
            )
            .await
        }
        .await
        .expect("runtime init");
        let r = ctx.rt.trace_wait_idle().await;
        assert!(r.is_err(), "expected an error, got {r:?}");
        ctx.shutdown().await;
    }
}

#[cfg(test)]
mod batch_files_test {
    use super::*;

    /// Two subjects with the same module name and incompatible contents
    /// both agree in one batch: files do not alias across a child's
    /// subjects. The differential oracle cannot see aliasing (both
    /// engines inherit the same wrong module), so the verdict is the
    /// observable.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn batched_files_do_not_alias() {
        let a = "{ mod m0; m0::k0 }\n// file-v1: m0.gx\nlet k0 = i64:1\n";
        let b = "{ mod m0; str::len(m0::k0) }\n// file-v1: m0.gx\nlet k0 = \"xy\"\n";
        for progs in
            [vec![a.to_string(), b.to_string()], vec![b.to_string(), a.to_string()]]
        {
            let mut verdicts: Vec<(usize, bool)> = Vec::new();
            run_batch(&progs, Duration::from_secs(10), |i, v| {
                verdicts.push((i, v != BatchVerdict::Other));
            })
            .await;
            assert_eq!(verdicts.len(), 2, "both subjects must report");
            for (i, agreed) in verdicts {
                assert!(
                    agreed,
                    "subject {i} did not agree — files aliased across the batch"
                );
            }
        }
    }

    /// A batched subject reaches the same verdict as the individual
    /// path, including `ran`: a CompileErr agrees with a CompileErr, so
    /// a batch child that never compiled the subject would still report
    /// agreement. Not covered: that the batch child drives both callable
    /// routes (a contract-satisfying program agrees across routes by
    /// construction; the fuzzer covers it).
    #[tokio::test]
    async fn batch_verdict_matches_individual() {
        let cases: [(&str, &str, bool); 4] = [
            ("plain", "i64:1 + i64:2", true),
            (
                "injected inputs",
                "// schedule-v1: cap=8 events=64; in0=i64:5; in0=i64:7\n                 { let acc = i64:0; acc <- in0; acc }",
                true,
            ),
            (
                "aux module",
                "m0::bump(i64:1)\n// file-v1: m0.gx\nlet bump = |x: i64| -> i64 x + i64:41",
                true,
            ),
            (
                "callable dispatch",
                "// callable-v1: handler=m0::handler; cx0=i64:7; cx0=i64:9\n\
                 { let o = m0::observe; o }\n\
                 // file-v1: m0.gx\n\
                 let state = { v: 0 };\n\
                 let put = |st: &mut { v: i64 }, x: i64| -> null { *st <- x ~ { v: x }; null };\n\
                 let handler = |x: i64| -> null put(&mut state, x);\n\
                 let observe = state",
                false,
            ),
        ];
        let timeout = Duration::from_secs(20);
        for (name, prog, want_ran) in cases {
            let (diverged, ran) = check_classified(prog, timeout).await;
            assert!(diverged.is_none(), "{name}: individual path diverged");
            // absolute, not just pairwise: both paths share one derivation
            assert_eq!(want_ran, ran, "{name}: individual path `ran`");
            let want = if want_ran { BatchVerdict::Ran } else { BatchVerdict::Agree };
            let mut got = None;
            run_batch(&[prog.to_string()], timeout, |_, v| got = Some(v)).await;
            assert_eq!(
                Some(want),
                got,
                "{name}: batch verdict differs from the individual path \
                 (a `ran: false` agreement means the batch child never \
                 compiled the subject)"
            );
        }
    }
}

#[cfg(test)]
mod order_test {
    use super::*;

    #[test]
    fn a_dead_order_child_hands_back_every_subject_without_a_verdict() {
        let order = WorkOrder {
            kind: SourceKind::Generate,
            seed: 5,
            count: 6,
            ring: vec![],
            pins: 0..0,
        };
        let mut next = order.generator();
        let progs: Vec<String> = (0..order.count).map(|_| next()).collect();
        let p1 = &progs[1];
        let head = format!("V 0 A\nV 1 O\nP 1 {}\n{p1}\nV 2 R\n", p1.len());
        let r = order_result(&order, &head, false);
        assert!(!r.clean);
        assert_eq!(r.agreed, 2);
        let unjudged = |i: usize| Suspect::Unjudged(progs[i].clone());
        assert_eq!(
            r.suspect,
            [Suspect::Flagged(p1.clone(), vec![]), unjudged(3), unjudged(4), unjudged(5)]
        );
        // a subject's `sys::exit(0)` is not a clean order
        assert!(!order_result(&order, &head, true).clean);
        let tail: String = (3..6).map(|i| format!("V {i} A\n")).collect();
        let r = order_result(&order, &format!("{head}{tail}CPU 7\n"), true);
        assert!(r.clean);
        assert_eq!(r.agreed, 5);
        assert_eq!(r.suspect, [Suspect::Flagged(p1.clone(), vec![])]);
    }

    /// A `V` line says how its `P` line hands the subject back: flagged
    /// with the probes that flipped, or unjudged; `J` is an agreement both
    /// builds refused.
    #[test]
    fn an_order_line_says_how_a_subject_comes_back() {
        let order = WorkOrder {
            kind: SourceKind::Typemorph,
            seed: 5,
            count: 4,
            ring: vec![],
            pins: 0..0,
        };
        let (p1, p2) = ("one", "two");
        let text = format!(
            "V 0 J\nV 1 O a#1 b#2\nP 1 3\n{p1}\nV 2 U\nP 2 3\n{p2}\nV 3 A\nCPU 1\n"
        );
        let r = order_result(&order, &text, true);
        assert!(r.clean);
        assert_eq!((r.agreed, r.refused), (2, 1));
        assert_eq!(
            r.suspect,
            [
                Suspect::Flagged(p1.into(), vec!["a#1".into(), "b#2".into()]),
                Suspect::Unjudged(p2.into())
            ]
        );
    }

    #[test]
    fn a_finding_round_trips_its_file() {
        let f = Finding {
            key: "Engine (\"x\\y\", 1, 2, None)".into(),
            bisect: "b".into(),
            labels: ("interp".into(), "jit".into()),
            reference: "Trace([0:i64:1])".into(),
            tested: "line\nbreak".into(),
        };
        assert_eq!(Finding::decode(&f.encode()), Some(f));
    }
}

#[cfg(test)]
mod fusecheck_test {
    use super::*;

    #[test]
    fn mismatches_name_every_way_the_manifest_can_be_wrong() {
        let manifest =
            "3\tholds\n2\tlost\n1\tgained\n4\tnow_aborts\nabort\tstill_aborts\n0\tgone\n";
        let live = [
            ("holds", Ok(FuseCount::Fused(3))),
            ("lost", Ok(FuseCount::Fused(1))),
            ("gained", Ok(FuseCount::Fused(2))),
            ("now_aborts", Ok(FuseCount::BudgetAbort)),
            ("still_aborts", Ok(FuseCount::BudgetAbort)),
            ("unrecorded", Ok(FuseCount::Fused(5))),
            ("wedged", Err("runtime init failed\ncompile timed out".to_string())),
        ]
        .map(|(n, c)| (n.to_string(), c));
        assert_eq!(
            fusecheck_mismatches(manifest, &live),
            [
                "LOST fusion: lost: 2 -> 1",
                "changed fusion: gained: 1 -> 2",
                "changed fusion: now_aborts: 4 -> abort",
                "unrecorded: unrecorded (5) — bless to record",
                "unreadable: wedged: compile timed out",
                "stale manifest row: gone (0) — bless to drop",
            ]
        );
        for c in [FuseCount::Fused(7), FuseCount::BudgetAbort] {
            assert_eq!(c.to_string().parse(), Ok(c));
        }
    }

    #[test]
    fn outcome_mismatches_skip_regressed_pins() {
        let manifest = "trace\tholds\ncontained\tchanged\nunsure\tvouched\ntrace\tregressed\ntrace\tgone\n";
        let o = Outcome::RuntimeErr("stub".to_string());
        let regressed = Divergence {
            code: "regressed".to_string(),
            reference: o.clone(),
            tested: o,
            tier: OracleTier::Exact,
            pair: Pair::Engine,
        };
        let r = Regression {
            regressions: vec![("regressed".to_string(), regressed)],
            verdicts: [
                ("holds", Verdict::Ran),
                ("changed", Verdict::Ran),
                ("vouched", Verdict::Rejected),
                ("new", Verdict::Excluded),
            ]
            .map(|(n, verdict)| (n.to_string(), Recorded { verdict, why: None }))
            .into(),
        };
        assert_eq!(
            outcome_mismatches(manifest, &r),
            [
                "changed outcome: changed: contained -> trace",
                "unrecorded: new (excluded) — bless to record",
                "stale manifest row: gone (trace) — bless to drop",
            ]
        );
    }
}
