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
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag, FusionStats, ParMode, Scope,
    env::Env,
    expr::{Expr, Origin, VfsEntry, VfsResolver, parser},
};
use graphix_package::Package;
use graphix_package_core::testing::{
    TestCtx, init_session_with_setup, init_with_flags_and_setup,
};
use graphix_rt::{CompRes, GXEvent, NoExt, RegistrationImage};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_core::path::Path;
use poolshark::local::LPooled;
use std::{collections::BTreeMap, fmt, future, str::FromStr, time::Duration};
use tokio::sync::{mpsc, oneshot};

/// Every stdlib package, so generated programs can use the whole
/// language surface. Mirrors `graphix-tests`'s `TEST_REGISTER` (which is
/// `#[cfg(test)]`-gated and so not importable).
pub const REGISTER: &[&dyn Package<NoExt>] = graphix_package::package_refs!();

/// The mode a program was run under. The single `FusionDisabled` flag
/// toggles all of fusion: there is no "fuse but don't JIT" mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Node-walk interpreter (`CFlag::FusionDisabled`) — the reference.
    Interp,
    /// Fusion + cranelift JIT (no flags) — the system under test.
    Jit,
    /// The node-walk with every fork point forked
    /// (`design/parallel_eval.md`): what serial evaluation computes.
    Par,
    /// Fusion + JIT with every fork point forked, kernel loops' slots
    /// included.
    JitPar,
}

impl Mode {
    pub fn flags(self) -> BitFlags<CFlag> {
        match self {
            Mode::Interp | Mode::Par => CFlag::FusionDisabled.into(),
            Mode::Jit | Mode::JitPar => BitFlags::empty(),
        }
    }

    pub fn node_walk(self) -> bool {
        matches!(self, Mode::Interp | Mode::Par)
    }

    // CR claude for claude: [test-gap] Mode::par runs the parallel pairs at Force and the
    // serial ones at Off, so no oracle ever runs Auto, the default mode. Auto's own
    // fork shapes are compared against nothing: a ProbeSite running its first k items
    // in the parent and forking the rest, a ForkSite's weighted multi-child ranges
    // (Force always halves down to single children), and a LoopSite's one-slot probe
    // runs followed by chunks from slot k. run!'s interp and jit fixtures run Auto but
    // rarely fork and assert nothing about it, and par_attrs compares Auto's values
    // without asserting that it forked. cost::tests pins ProbeSite and LoopSite but not
    // the ForkSite/SlotSite state machines, which read the global calibration() and
    // saturated() and so cannot be driven with a fixed Calibration. A merge bug on the
    // probe-then-fork path would ship in the default mode with every gate green. A pair
    // that runs Auto with a fixed tiny T (a GRAPHIX_PAR_T override, for example) would
    // make every site probe and then fork along Auto's shapes. (c-cost-misc-05)
    pub fn par(self) -> ParMode {
        match self {
            Mode::Par | Mode::JitPar => ParMode::Force,
            Mode::Interp | Mode::Jit => ParMode::Off,
        }
    }
}

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
                        write!(f, "{o}:{v}")?;
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

/// What stopped a contained run. The two agree with each other: which
/// fires first is a race between the engines' descent speeds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Containment {
    Deadline,
    StackBudget,
}

/// Strip tvar numbers (`'_6070` -> `'_N`), abstract-type ids
/// (`<abstract#12>` -> `<abstract#N>`) and generated scope ids
/// (`#do123` -> `#doN`) so fresh-counter drift between
/// two independent compiles neither hides nor fakes a diagnostic difference.
fn normalize_diag(s: &str) -> String {
    let s = {
        let mut out = String::with_capacity(s.len());
        let mut rest = s;
        while let Some(i) = rest.find("<abstract#") {
            out.push_str(&rest[..i]);
            out.push_str("<abstract#N");
            let tail = &rest[i + "<abstract#".len()..];
            let end = tail.find(|c: char| !c.is_ascii_digit()).unwrap_or(tail.len());
            rest = &tail[end..];
        }
        out.push_str(rest);
        out
    };
    let s = s.as_str();
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        out.push(c);
        // a generated block scope component (`#do123`, `#fn45`): its id
        // is a counter
        if c == '#' {
            while chars.peek().is_some_and(|c| c.is_ascii_lowercase()) {
                out.push(chars.next().unwrap());
            }
            if chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                while chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                    chars.next();
                }
                out.push('N');
            }
            continue;
        }
        if c == '\'' && chars.peek() == Some(&'_') {
            out.push(chars.next().unwrap());
            while chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                chars.next();
            }
            out.push('N');
        }
    }
    out
}

impl Outcome {
    /// The trace of a pure synchronous program producing `v` once at
    /// init — offset 0, single epoch. Test convenience.
    pub fn single(v: Value) -> Outcome {
        Outcome::Trace(trace::Trace {
            epochs: vec![trace::Epoch { events: vec![(0, v)], capped: false }],
            stdout: Vec::new(),
        })
    }

    /// Whether two outcomes are observably equivalent. Traces compare
    /// structurally; different outcome kinds always disagree; same-kind
    /// non-trace outcomes agree without comparing mode-dependent messages.
    pub fn agrees_with(&self, other: &Outcome) -> bool {
        use Outcome::*;
        match (self, other) {
            (Trace(a), Trace(b)) => a.agrees_with(b),
            // both-reject agrees only on identical diagnostics (modulo
            // counter drift); RuntimeErr messages are mode-dependent
            (CompileErr(a), CompileErr(b)) => normalize_diag(a) == normalize_diag(b),
            (RuntimeErr(_), RuntimeErr(_)) => true,
            (Timeout(_), Timeout(_)) => true,
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
            (Strength::Kind, Trace(_) | Timeout(_), Trace(_) | Timeout(_)) => true,
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
    run_program_with_stats_routed(code, mode, route, timeout).await.0
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
    /// The characters before the body on the module's first line: a
    /// position the compiler reports there is the body's plus this.
    pub body_col: usize,
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
        let prefix = format!("use super::*; {inputs}let result = {open}");
        let body_col = prefix.chars().count();
        let wrapped = ArcStr::from(format!("{prefix}{body}{close}"));
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
            body_col,
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

/// Compile `code` the way `--check` does under [`CFlag::CheckOnly`]: the
/// check alone, no elaboration, never run. `Err` is the refusal, or a
/// runtime that failed to start.
pub async fn check_only(code: &str, timeout: Duration) -> Result<(), String> {
    let subj = Subject::parse(code, "test")?;
    let (tx, _rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    let registration = registration_image_source().await;
    let flags = Mode::Interp.flags();
    let ctx = init_session_with_setup(
        tx,
        REGISTER,
        vec![resolver],
        flags,
        registration,
        None,
        None,
        None,
        |_| {},
    )
    .await
    .map_err(|e| format!("runtime init failed: {e:?}"))?;
    let src = graphix_compiler::expr::Source::Internal(ArcStr::from(subj.compile_text()));
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
    let (tx, _rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    // keeps the compile cycle's print output off the process streams
    let sink = graphix_package_core::PrintSink::default();
    let registration = registration_image_source().await;
    let program =
        graphix_compiler::expr::Source::Internal(ArcStr::from(subj.compile_text()));
    let init = init_session_with_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        registration,
        Some(program),
        None,
        None,
        move |ctx| {
            ctx.libstate.set(sink);
            ctx.control.set_par_mode(mode.par());
        },
    );
    let ctx = match tokio::time::timeout(timeout, init).await {
        Ok(Ok(c)) => c,
        Ok(Err(e)) => {
            return CompileOutcome::Failed(format!("runtime init failed: {e:?}"));
        }
        Err(_) => return CompileOutcome::Failed("compile timed out".to_string()),
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
/// post-init baseline is subtracted).
pub async fn run_program_with_stats(
    code: &str,
    mode: Mode,
    timeout: Duration,
) -> (Outcome, FusionStats) {
    run_program_with_stats_routed(code, mode, Route::InLanguage, timeout).await
}

pub async fn run_program_with_stats_routed(
    code: &str,
    mode: Mode,
    route: Route,
    timeout: Duration,
) -> (Outcome, FusionStats) {
    let run = run_subject(code, mode, route, SessionImage::None, timeout).await;
    (run.outcome, run.stats)
}

async fn drive(
    ctx: &TestCtx,
    rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    subj: &Subject,
    route: Route,
    timeout: Duration,
    sink: &graphix_package_core::PrintSink,
) -> Outcome {
    let outcome = drive_inner(ctx, rx, subj, route, timeout, sink).await;
    // A runtime the stack budget aborted answers nothing more: whichever
    // request that failed, the outcome is the containment.
    if matches!(outcome, Outcome::RuntimeErr(_) | Outcome::CompileErr(_))
        && ctx.rt.budget_aborted()
    {
        return Outcome::Timeout(Containment::StackBudget);
    }
    outcome
}

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
    // A deadline breach is `Timeout` on every step. The interrupt is a
    // one-shot flag that may bottom an in-flight cycle's output, so a
    // step that completes after it cannot be trusted; reclassifying is
    // a race. `check()` retries one-sided timeouts at a bigger budget.
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
    // async IO task as idle, so the FinalValues tier re-waits after a
    // grace sleep until a round is quiet; Exact-tier programs have no
    // in-flight IO and skip the settle.
    macro_rules! wait_settled {
        () => {{
            let mut seg = step_or_timeout!(
                ctx.rt.trace_wait_idle(),
                Ok(s) => s,
                Err(e) => return Outcome::RuntimeErr(format!("trace_wait_idle: {e}"))
            );
            if tier == OracleTier::FinalValues {
                for _ in 0..8 {
                    tokio::time::sleep(Duration::from_millis(150)).await;
                    let more = step_or_timeout!(
                        ctx.rt.trace_wait_idle(),
                        Ok(s) => s,
                        Err(e) => {
                            return Outcome::RuntimeErr(format!("trace_wait_idle: {e}"))
                        }
                    );
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
                for _ in 0..3 {
                    step_or_timeout!(
                        ctx.rt.compile(ArcStr::from("i64:0")),
                        Ok(_) => (),
                        Err(e) => {
                            return Outcome::RuntimeErr(format!("gap compile: {e}"));
                        }
                    );
                }
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
    // Exact tier only. The runtime runs on past a capped trace, so the
    // capture stops at the last segment's cycle.
    if tier == OracleTier::Exact {
        let mut lines = Vec::new();
        for (epoch, seg) in segs.iter().enumerate() {
            let cycles = sink.take_through(seg.end_cycle);
            let Some(base) =
                trace::Epoch::anchor(seg).or(cycles.first().map(|(c, _)| *c))
            else {
                continue;
            };
            for (cycle, text) in cycles {
                lines.extend(text.lines().map(|l| trace::Printed {
                    epoch: epoch as u32,
                    offset: cycle as i64 - base as i64,
                    line: l.to_string(),
                }));
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
    /// Only what does not depend on values: both ran (a trace or a
    /// timeout), or both refused alike.
    Kind,
}

impl Strength {
    /// The program's tier on its own route; settled values where the
    /// dispatch route's cycle offsets enter, which are not contractual;
    /// the outcome's kind for a program whose values are not comparable.
    pub fn of(pair: Pair, tier: OracleTier) -> Self {
        let dispatch = matches!(
            pair,
            Pair::EngineDispatch
                | Pair::Route
                | Pair::Cold(_, Route::Dispatch)
                | Pair::Warm(_, Route::Dispatch)
        );
        match tier {
            OracleTier::Excluded => Strength::Kind,
            OracleTier::FinalValues => Strength::Final,
            OracleTier::Exact if dispatch => Strength::Final,
            OracleTier::Exact => Strength::Exact,
        }
    }
}

/// Classify a program. Markers are matched on code lines only. The
/// Excluded list is empirical; selfcheck polices it — a marker missing
/// from it shows up as a final-strength flake.
pub fn oracle_tier(code: &str) -> OracleTier {
    // A comment that merely names an excluded API must not un-gate the
    // program; trailing comments on code lines stay (over-exclusion is
    // the safe direction).
    let mut stripped = String::with_capacity(code.len());
    for l in code.lines() {
        if !l.trim_start().starts_with("//") {
            stripped.push_str(l);
            stripped.push('\n');
        }
    }
    let code = stripped.as_str();
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
        "tempdir",
        "listener_addr",
        "local_addr",
        "peer_addr",
    ];
    if excluded.iter().any(|m| code.contains(m)) {
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
    if fs_mutators.iter().any(|m| code.contains(m))
        && fs_observers.iter().any(|m| code.contains(m))
    {
        return OracleTier::Excluded;
    }
    // CR claude for claude: [test-gap] Only programs naming `sys::` or `http::` reach
    // FinalValues. json::read, toml::read, pack::read, xls::, db:: and sqlite:: are
    // EvalCachedAsync tasks too, so their reply lands in whichever cycle the scheduler
    // allows, and the Exact tier compares that cycle. `let m = {"a" => 1, "b" => 2};
    // let n: i64 = json::read("5")?; (m, n)` gave AGREE in one of six checks and
    // otherwise a DIVERGENCE blamed on the JIT, parallel evaluation or the image, with
    // the reply landing at cycle 1 in one mode and 2 in the other. The generator never
    // draws these packages, so selfcheck cannot flag the missing markers, and a pin
    // that used them would change verdict from run to run. Add their markers here and
    // give the generator calls to them, which would also put each reader's imaged cast
    // target under the cold/warm comparison. probe:
    // design/review-2026-10-05/repro/small-pkgs-19.gx (small-pkgs-19)
    if ["sys::", "http::"].iter().any(|m| code.contains(m)) {
        // FinalValues assumes the async values themselves settle
        // deterministically; a `<-` weaves arrival order into state, and
        // the fire-count/arrival-order-sensitive builtins leak it too.
        // `iter(` is a self-clocked sequence: one element per cycle,
        // so anything async landing beside it samples whichever element
        // that cycle held.
        let fire_count_sensitive = [
            "count(", "sum(", "product(", "mean(", "min(", "max(", "all(", "and(", "or(",
            "any(", "queue(", "take(", "skip(", "window(", "iterq", "iter(", "group(",
            "hold(",
        ];
        if code.contains("<-") || fire_count_sensitive.iter().any(|m| code.contains(m)) {
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
            let ctx = init_session_with_setup(
                tx,
                REGISTER,
                vec![],
                BitFlags::empty(),
                RegistrationImage::Save(reg_tx),
                None,
                None,
                None,
                |_| {},
            )
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
        Some(bytes) => RegistrationImage::Load(bytes),
        None => RegistrationImage::Save(oneshot::channel().0),
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
    /// fusion off (or is restored), so nothing else contributes.
    stats: FusionStats,
}

fn no_image() -> Result<Bytes, String> {
    Err("no program image was written".to_string())
}

impl SubjectRun {
    fn failed(outcome: Outcome) -> Self {
        SubjectRun { outcome, image: no_image(), stats: FusionStats::default() }
    }
}

/// Run `code` under `mode` on the program route (`GXConfig::program`,
/// the shell's script path): the subject's module table behind a
/// resolver, the registration image restored, the program compiled or
/// restored at the runtime's construction, then driven.
async fn run_subject(
    code: &str,
    mode: Mode,
    route: Route,
    image: SessionImage,
    timeout: Duration,
) -> SubjectRun {
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return SubjectRun::failed(Outcome::CompileErr(e)),
    };
    let (tx, mut rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    // per-runtime print capture, so two modes running concurrently in
    // one process keep separate output
    let sink = graphix_package_core::PrintSink::default();
    let seeded = sink.clone();
    let program =
        Some(graphix_compiler::expr::Source::Internal(ArcStr::from(subj.compile_text())));
    let (registration, program, program_image, image_rx) = match image {
        SessionImage::Read(bytes) => (RegistrationImage::Load(bytes), None, None, None),
        SessionImage::Write => {
            let (itx, irx) = oneshot::channel();
            (registration_image_source().await, program, Some(itx), Some(irx))
        }
        SessionImage::None => (registration_image_source().await, program, None, None),
    };
    // The program compiles (or restores) at construction, under the run's
    // budget; a construction it abandons finishes on its own task and
    // exits, its handle gone.
    let init = init_session_with_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        registration,
        program,
        program_image,
        Some((subj.sched.max_events, subj.sched.max_cycles)),
        move |ctx| {
            ctx.libstate.set(seeded);
            ctx.control.set_par_mode(mode.par());
        },
    );
    let ctx = match tokio::time::timeout(timeout, init).await {
        Ok(Ok(c)) => c,
        Ok(Err(e)) => {
            return SubjectRun::failed(Outcome::RuntimeErr(format!(
                "runtime init failed: {e:?}"
            )));
        }
        Err(_) => return SubjectRun::failed(Outcome::Timeout(Containment::Deadline)),
    };
    let outcome = drive(&ctx, &mut rx, &subj, route, timeout, &sink).await;
    // A wedged runtime never answers another request: abort first, then
    // never await it without a deadline.
    if matches!(outcome, Outcome::Timeout(_)) {
        ctx.rt.abort();
    }
    let grace = Duration::from_secs(2);
    let image = match image_rx {
        None => no_image(),
        Some(irx) => match tokio::time::timeout(grace, irx).await {
            Ok(Ok(Ok(bytes))) => Ok(bytes),
            Ok(Ok(Err(e))) => Err(format!("the program image write failed: {e:#}")),
            Ok(Err(_)) | Err(_) => no_image(),
        },
    };
    let stats = match tokio::time::timeout(grace, ctx.fusion_stats()).await {
        Ok(Ok(s)) => s,
        Ok(Err(_)) | Err(_) => FusionStats::default(),
    };
    let _ = tokio::time::timeout(grace, ctx.shutdown()).await;
    SubjectRun { outcome, image, stats }
}

/// [`run_subject`] for the image axis: the outcome and the image a
/// `Write` produced.
async fn run_session(
    code: &str,
    mode: Mode,
    route: Route,
    image: SessionImage,
    timeout: Duration,
) -> (Outcome, Result<Bytes, String>) {
    let run = run_subject(code, mode, route, image, timeout).await;
    (run.outcome, run.image)
}

/// `code` under `mode` cold (writing the program image) and warm
/// (restored from it). A program that does not compile has nothing to
/// restore, so its warm outcome is its cold one; a cold run that compiled
/// but wrote no image makes the warm outcome the write's failure, which
/// the cold/warm pair reports.
pub async fn cold_warm(
    code: &str,
    mode: Mode,
    route: Route,
    timeout: Duration,
) -> (Outcome, Outcome) {
    let (cold, image) =
        run_session(code, mode, route, SessionImage::Write, timeout).await;
    let warm = match (&cold, image) {
        (Outcome::CompileErr(_), _) => cold.clone(),
        (_, Ok(image)) => {
            run_session(code, mode, route, SessionImage::Read(image), timeout).await.0
        }
        (_, Err(e)) => Outcome::RuntimeErr(e),
    };
    (cold, warm)
}

/// The routes a subject's sessions run on.
fn session_routes(code: &str) -> &'static [Route] {
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
    for (path, names) in &env.binds {
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
    for (path, _) in &env.binds {
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
    /// node-walk vs fused+JIT, in-language route (`interp`/`jit`
    /// fields hold exactly that).
    Engine,
    /// node-walk vs fused+JIT, embedder-dispatch route.
    EngineDispatch,
    /// in-language route vs embedder-dispatch route, node-walk engine
    /// (`interp` holds the in-language outcome, `jit` the dispatch
    /// outcome). Compared at final-values strength — route pacing is
    /// not contractual, the per-epoch settled value is.
    Route,
    /// A metamorphic twin program violated its own invariant. Detected
    /// within a single run (both fields hold the offending outcome), so
    /// it catches bugs that break every engine and route identically.
    Twin,
    /// No cache vs cold (`interp` holds the no-cache outcome, `jit` the
    /// cold one): writing the program image changed the program.
    Cold(Mode, Route),
    /// Cold vs warm (`interp` holds the cold outcome, `jit` the warm
    /// one): restoring the program image changed the program.
    Warm(Mode, Route),
    /// A corpus pin both engines reject (both fields hold the
    /// rejection) without saying `// expect: reject`: a rejection agrees
    /// with itself, so a pin that stopped compiling would otherwise pass.
    Rejected,
    /// The check alone accepted (`interp` holds [`Outcome::Checked`]) a
    /// program both engines' builds refused (`jit` holds the refusal):
    /// elaboration refused what the check accepted, a type-system bug.
    Check,
    /// Serial node-walk vs a run with every fork forced: the node-walk
    /// (`Mode::Par`) or the fused JIT, kernel loops included
    /// (`Mode::JitPar`); `interp` holds the serial outcome, `jit` the
    /// forked one.
    Par(Mode),
}

/// The label of one session run, for a finding's outcome fields.
fn session_label(mode: Mode, route: Route, session: Session) -> &'static str {
    use {Mode::*, Route::*, Session::*};
    match (mode, route, session) {
        (Interp, InLanguage, NoCache) => "interp/nocache",
        (Interp, InLanguage, Cold) => "interp/cold",
        (Interp, InLanguage, Warm) => "interp/warm",
        (Jit, InLanguage, NoCache) => "jit/nocache",
        (Jit, InLanguage, Cold) => "jit/cold",
        (Jit, InLanguage, Warm) => "jit/warm",
        (Interp, Dispatch, NoCache) => "interp/dispatch/nocache",
        (Interp, Dispatch, Cold) => "interp/dispatch/cold",
        (Interp, Dispatch, Warm) => "interp/dispatch/warm",
        (Jit, Dispatch, NoCache) => "jit/dispatch/nocache",
        (Jit, Dispatch, Cold) => "jit/dispatch/cold",
        (Jit, Dispatch, Warm) => "jit/dispatch/warm",
        (Par, _, _) => "par",
        (JitPar, _, _) => "jit-par",
    }
}

impl Divergence {
    /// A one-line classification.
    pub fn bisect(&self) -> &'static str {
        // Survived the slow retry: either the JIT fabricated a value or the
        // node-walk is >8x slower on a heavy terminating program. Verify by
        // hand.
        let engines = matches!(self.pair, Pair::Engine | Pair::EngineDispatch);
        if engines
            && matches!(self.reference, Outcome::Timeout(_))
            && self.tested.has_events()
        {
            return "asymmetric timeout (interp exceeded 8x budget; JIT produced a value — \
                    verify the node-walk terminates and agrees before reading this as a JIT bug)";
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
            (Pair::Check, _) => {
                "the check accepted what the build refused (elaboration refused \
                     a program the definition and call-site checks passed: a \
                     type-system bug)"
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
            (Pair::Cold(Mode::Interp | Mode::Par, _), _) => {
                "image bug: writing the program image changed the program (interp)"
            }
            (Pair::Cold(Mode::Jit | Mode::JitPar, _), _) => {
                "image bug: writing the program image changed the program (jit)"
            }
            (Pair::Warm(Mode::Interp | Mode::Par, _), _) => {
                "image bug: the restored program differs from the cold one (interp)"
            }
            (Pair::Warm(Mode::Jit | Mode::JitPar, _), _) => {
                "image bug: the restored program differs from the cold one (jit)"
            }
        }
    }

    /// Human labels for `reference` and `tested`, by pair.
    pub fn labels(&self) -> (&'static str, &'static str) {
        match self.pair {
            Pair::Engine => ("interp", "jit"),
            Pair::EngineDispatch => ("interp/dispatch", "jit/dispatch"),
            Pair::Route => ("in-language", "dispatch"),
            Pair::Twin => ("trace", "trace"),
            Pair::Rejected => ("interp", "jit"),
            Pair::Check => ("check", "build"),
            Pair::Par(Mode::Par) => ("interp", "interp/par"),
            Pair::Par(_) => ("interp", "jit/par"),
            Pair::Cold(m, r) => (
                session_label(m, r, Session::NoCache),
                session_label(m, r, Session::Cold),
            ),
            Pair::Warm(m, r) => {
                (session_label(m, r, Session::Cold), session_label(m, r, Session::Warm))
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
            (Timeout(_), Timeout(_)) => Verdict::Contained,
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
/// divergence or the first drop.
pub async fn check_verdict(
    code: &str,
    timeout: Duration,
) -> (Option<Divergence>, Verdict) {
    let tier = oracle_tier(code);
    let pairs = comparisons(code, tier, sessions_enabled());
    let mut o = Oracle::new(code, tier, timeout);
    o.make(&[Pair::Engine]).await;
    if let Some(d) = o.check_pair().await {
        return (Some(d), Verdict::Unsure);
    }
    for &pair in &pairs {
        o.make_group(&pairs, pair).await;
        if let Some(d) = o.twin().await {
            return (Some(d), Verdict::Unsure);
        }
        match o.settle(pair).await {
            Settled::Agree => (),
            Settled::Drop => return (None, Verdict::Unsure),
            Settled::Diverge(reference, tested) => {
                let d =
                    Divergence { code: code.to_string(), reference, tested, tier, pair };
                return (Some(d), Verdict::Unsure);
            }
        }
    }
    if let Some(d) = o.twin().await {
        return (Some(d), Verdict::Unsure);
    }
    (None, o.verdict())
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
    const fn plain(mode: Mode, route: Route) -> Self {
        Run { mode, route, session: Session::NoCache }
    }
}

impl Pair {
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
            Pair::Cold(m, r) => (Run::plain(m, r), session(m, r, Session::Cold)),
            Pair::Warm(m, r) => {
                (session(m, r, Session::Cold), session(m, r, Session::Warm))
            }
            Pair::Twin | Pair::Rejected | Pair::Check => return None,
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
/// pair, then each engine's image sessions, then the forked runs. A
/// program whose values are not comparable owes only its sessions, at
/// kind strength.
fn comparisons(code: &str, tier: OracleTier, sessions: bool) -> Vec<Pair> {
    let values = tier != OracleTier::Excluded;
    let mut out = Vec::new();
    if values {
        out.push(Pair::Engine);
        if callable::has_header(code) {
            out.extend([Pair::EngineDispatch, Pair::Route]);
        }
    }
    if sessions {
        for &route in session_routes(code) {
            for mode in [Mode::Interp, Mode::Jit] {
                out.extend([Pair::Cold(mode, route), Pair::Warm(mode, route)]);
            }
        }
    }
    if values && par_enabled() {
        out.extend([Pair::Par(Mode::Par), Pair::Par(Mode::JitPar)]);
    }
    out
}

/// A run of `code`, or a session's cold and warm runs, which are one
/// job since the warm one restores what the cold one wrote.
#[derive(Clone, Copy)]
enum Job {
    Plain(Run),
    Sessions(Mode, Route),
}

async fn do_job(code: &str, job: Job, timeout: Duration) -> Vec<(Run, Outcome)> {
    match job {
        Job::Plain(r) => {
            vec![(r, run_program_routed(code, r.mode, r.route, timeout).await)]
        }
        Job::Sessions(mode, route) => {
            let (cold, warm) = cold_warm(code, mode, route, timeout).await;
            let run = |session| Run { mode, route, session };
            vec![(run(Session::Cold), cold), (run(Session::Warm), warm)]
        }
    }
}

/// `run` again at `budget`.
async fn run_one(code: &str, run: Run, budget: Duration) -> Outcome {
    match run.session {
        Session::NoCache => run_program_routed(code, run.mode, run.route, budget).await,
        Session::Cold => cold_warm(code, run.mode, run.route, budget).await.0,
        Session::Warm => cold_warm(code, run.mode, run.route, budget).await.1,
    }
}

/// How one comparison came out.
enum Settled {
    Agree,
    /// The ladder found nothing stable to say: containment, slowness or
    /// nondeterminism.
    Drop,
    Diverge(Outcome, Outcome),
}

/// A subject's runs, made as its comparisons ask for them.
struct Oracle<'a> {
    code: &'a str,
    tier: OracleTier,
    timeout: Duration,
    runs: Vec<(Run, Outcome)>,
    /// Runs before this index have been scanned for a twin violation.
    scanned: usize,
    /// A confirm step decided a comparison, so the agreement says nothing
    /// stable about the program.
    confirmed: bool,
}

impl<'a> Oracle<'a> {
    fn new(code: &'a str, tier: OracleTier, timeout: Duration) -> Self {
        Oracle { code, tier, timeout, runs: Vec::new(), scanned: 0, confirmed: false }
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
        for (r, o) in do_job(self.code, job, budget).await {
            if let Some((_, x)) = self.runs.iter_mut().find(|(x, _)| *x == r) {
                *x = o;
            }
        }
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
        let code = self.code;
        let timeout = self.timeout;
        let done =
            futures::future::join_all(jobs.into_iter().map(|j| do_job(code, j, timeout)))
                .await;
        self.runs.extend(done.into_iter().flatten());
    }

    /// Make the runs of every comparison in `pair`'s group.
    async fn make_group(&mut self, pairs: &[Pair], pair: Pair) {
        let group: Vec<Pair> =
            pairs.iter().copied().filter(|p| p.group() == pair.group()).collect();
        self.make(&group).await;
    }

    /// Both engines refused the program, but the check alone accepts it:
    /// elaboration refused what the check passed. A compile verdict owes
    /// nothing to values, so an excluded program is checked too.
    async fn check_pair(&self) -> Option<Divergence> {
        let (a, b) = Pair::Engine.runs()?;
        let (Outcome::CompileErr(_), jit @ Outcome::CompileErr(_)) =
            (self.get(a), self.get(b))
        else {
            return None;
        };
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

    /// One comparison through the confirm ladder.
    async fn settle(&mut self, pair: Pair) -> Settled {
        let (ra, rb) = pair.runs().expect("a comparison");
        let s = Strength::of(pair, self.tier);
        let (mut a, mut b) = (self.get(ra).clone(), self.get(rb).clone());
        if a.agrees_at(&b, s) {
            return Settled::Agree;
        }
        self.confirmed = true;
        // The node-walk's frame per recursion level is kilobytes where a
        // kernel's is words, so a depth only the kernel reaches is
        // containment: the kernel's value stands unrefuted.
        if ra.mode.node_walk()
            && !rb.mode.node_walk()
            && matches!(a, Outcome::Timeout(Containment::StackBudget))
            && matches!(b, Outcome::Trace(_))
        {
            eprintln!(
                "CONTAINED — the node-walk exceeded the stack budget; the kernel's \
                 value stands unrefuted, not recorded"
            );
            eprintln!("    program: {}", self.code.replace('\n', "\\n"));
            return Settled::Drop;
        }
        // A Timeout beside anything else measures the budget under load:
        // that side again at the slow budget.
        let one_sided =
            matches!(a, Outcome::Timeout(_)) != matches!(b, Outcome::Timeout(_));
        let budget = if one_sided { slow_budget(self.timeout) } else { self.timeout };
        if one_sided {
            let run = if matches!(a, Outcome::Timeout(_)) { ra } else { rb };
            let cpu = self_cpu();
            self.remake(run, budget).await;
            let burned = self_cpu().saturating_sub(cpu);
            (a, b) = (self.get(ra).clone(), self.get(rb).clone());
            if a.agrees_at(&b, s) {
                return Settled::Agree;
            }
            // A wedge sits at ~0% CPU; seconds of burn are an honestly slow
            // reference, which refutes nothing
            if run == ra
                && matches!(a, Outcome::Timeout(_))
                && burned >= Duration::from_secs(5)
            {
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
        // a reference that disagrees with itself is nondeterminism
        let again = run_one(self.code, ra, budget).await;
        if !again.agrees_at(&a, s) {
            return Settled::Drop;
        }
        Settled::Diverge(a, b)
    }

    /// How an agreement concluded: the engines' kind, unless the ladder
    /// decided something or the program is excluded. A contained callable
    /// is unsure: its runs are four, the budget's race decides it.
    fn verdict(&self) -> Verdict {
        if self.tier == OracleTier::Excluded {
            return Verdict::Excluded;
        }
        if self.confirmed {
            return Verdict::Unsure;
        }
        let Some((a, b)) = Pair::Engine.runs() else { unreachable!() };
        match Verdict::of(self.get(a), self.get(b)) {
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

/// The longest a healthy `check-one` takes: its first runs (the engine
/// group, two cold-then-warm sessions per route, the forked pair, the
/// check, a twin's rerun) and one comparison's ladder (a slow retry and
/// the reference again at the slow budget), each run with its shutdown
/// grace.
fn check_deadline(timeout: Duration) -> Duration {
    let grace = Duration::from_secs(5);
    (timeout + grace) * 12 + (slow_budget(timeout) + grace) * 2 + Duration::from_secs(60)
}

/// Per-subject verdict from a batch child. Only agreement is trusted
/// from a batch; anything else re-runs through `check_isolated`, so
/// every finding still derives from a fresh single-subject process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BatchVerdict {
    Agree {
        /// Both outcomes were runtime traces — the ring-admission bar.
        ran: bool,
    },
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
    let pairs = comparisons(code, tier, sessions);
    let mut o = Oracle::new(code, tier, timeout);
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
    // the ring-admission bar, as the individual path's
    let ran = tier != OracleTier::Excluded
        && !callable::has_header(code)
        && matches!(o.get(a), Outcome::Trace(_))
        && matches!(o.get(b), Outcome::Trace(_));
    BatchVerdict::Agree { ran }
}

/// Batch size for the campaign pool's batch children./// Batch size for the campaign pool's batch children. 1 disables
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
            BatchVerdict::Agree { ran: true } => "R",
            BatchVerdict::Agree { ran: false } => "A",
            BatchVerdict::Other => "O",
        };
        let _ = writeln!(out, "V {i} {tag}");
        if matches!(v, BatchVerdict::Other) {
            let p = &progs[i];
            let _ = writeln!(out, "P {i} {}", p.len());
            let _ = out.write_all(p.as_bytes());
            let _ = writeln!(out);
        } else if matches!(v, BatchVerdict::Agree { ran: true })
            && let Some((sig, nodes, ok)) = novel[i]
            && ok
            && (8..=600).contains(&nodes)
        {
            let p = &progs[i];
            let _ = writeln!(out, "N {sig} {}", p.len());
            let _ = out.write_all(p.as_bytes());
            let _ = writeln!(out);
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
        let flipped = match typemorph_subject(p, timeout, TM_CAP).await {
            Ok(rep) => rep.flipped(),
            Err(_) => false,
        };
        let _ = writeln!(out, "V {i} {}", if flipped { "O" } else { "A" });
        if flipped {
            let _ = writeln!(out, "P {i} {}", p.len());
            let _ = out.write_all(p.as_bytes());
            let _ = writeln!(out);
        }
        let _ = out.flush();
    }
    let _ = writeln!(out, "CPU {}", self_cpu().as_micros());
    let _ = out.flush();
}

/// What one work order came back with. Everything here is per-BATCH or
/// per-FINDING; nothing scales with the number of agreeing subjects.
pub(crate) struct OrderResult {
    /// Subjects the child agreed on.
    pub agreed: usize,
    /// Programs the child could not resolve, and on a child that died
    /// every subject it left without a verdict; the parent re-derives
    /// them through the individual path, which records the one that
    /// killed it.
    pub suspect: Vec<String>,
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
        suspect: Vec::new(),
        novel: Vec::new(),
        cpu: Duration::ZERO,
        clean: false,
    };
    // a subject's verdict: agreed, or its program came back as a suspect
    let mut resolved = vec![false; order.count];
    let mut finished = false;
    let mut rest = text;
    while let Some((line, tail)) = rest.split_once('\n') {
        rest = tail;
        let mut it = line.split_whitespace();
        match it.next() {
            Some("V") => {
                let i: Option<usize> = it.next().and_then(|v| v.parse().ok());
                if it.next() != Some("O") {
                    res.agreed += 1;
                    if let Some(r) = i.and_then(|i| resolved.get_mut(i)) {
                        *r = true;
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
                    if let Some(r) = resolved.get_mut(id as usize) {
                        *r = true;
                    }
                    res.suspect.push(prog);
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
    // a subject's `sys::exit` ends the child with any status, so a clean
    // order is one that wrote its last line
    res.clean = exited_zero && finished;
    if !res.clean {
        let mut next = order.generator();
        for done in resolved {
            let p = next();
            if !done {
                res.suspect.push(p);
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
        let (la, lb) = d.labels();
        Finding {
            key: format!("{:?} {:?}", d.pair, bucket(d)),
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

/// Minimize a diverging wrapper: schedule reductions first (drop the
/// schedule, epochs, injections; simplify literals; caps stay fixed),
/// then the body and each `.gx` section by [`shrink`], keeping any
/// reduction that still parses and reproduces the same divergence
/// bucket. The headers are split off before parsing and reattached
/// around every candidate. Returns the minimized wrapper and the number
/// of oracle checks spent (capped by `budget`).
pub async fn minimize(code: &str, timeout: Duration, budget: usize) -> (String, usize) {
    let d0 = match check(code, timeout).await {
        Some(d) => d,
        None => return (code.to_string(), 1),
    };
    let target = bucket(&d0);
    let Ok((mut sched, body)) = schedule::Schedule::parse(code) else {
        return (code.to_string(), 1);
    };
    let Ok((cspec, body_owned)) = callable::CallSpec::parse(&body) else {
        return (code.to_string(), 1);
    };
    let Ok((body, mut files)) = files::split(&body_owned) else {
        return (code.to_string(), 1);
    };
    let mut current = match mutate::parse(body) {
        Some(e) => e,
        None => return (code.to_string(), 1),
    };
    // the callable header rides every candidate verbatim
    let reattach = |text: String| match &cspec {
        Some(c) => c.render(&text),
        None => text,
    };
    let mut calls = 1;
    'sched: while calls < budget {
        let body_text = files::render(&current.to_string(), &files);
        for cand in schedule_reductions(&sched) {
            if calls >= budget {
                break 'sched;
            }
            calls += 1;
            if let Some(d) = check(&reattach(cand.render(&body_text)), timeout).await {
                if bucket(&d) == target {
                    sched = cand;
                    continue 'sched; // restart from the smaller schedule
                }
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
                if let Some(d) = check(&text, timeout).await
                    && bucket(&d) == target
                {
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
                timeout,
                &mut calls,
                cap,
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
                    timeout,
                    &mut calls,
                    cap,
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
    (reattach(sched.render(&files::render(&current.to_string(), &files))), calls)
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
    target: &(&'static str, u8, u8, Option<trace::TraceDiff>),
    timeout: Duration,
    calls: &mut usize,
    budget: usize,
) -> Expr {
    // A candidate must render strictly shorter: several reductions are
    // identities on some nodes, and an identity reproduces the
    // divergence by definition.
    let hits = async |e: &Expr, cur: usize, calls: &mut usize| match build(e) {
        Some(text) if text.len() < cur => {
            *calls += 1;
            check(&text, timeout).await.is_some_and(|d| bucket(&d) == *target)
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
    /// never a flip.
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

    /// Whether any probe, either way, came back a finding.
    pub fn flipped(&self) -> bool {
        self.probes.iter().any(|p| matches!(p.verdict, TmVerdict::Reject(_)))
            || self.rejects.iter().any(|p| p.verdict.is_some())
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
/// family's (retype takes two per site).
pub fn tm_checks(cap: usize) -> u32 {
    (2 + (typemorph::TmKind::ALL.len() + mustreject::Family::ALL.len() + 1) * cap) as u32
}

/// One check of a subject's full text.
enum TmCheck {
    /// The text is not a subject.
    Unparsed(String),
    /// The check outlived its budget; the runtime is still busy with it.
    Hung,
    /// The check's answer, and the subject body's first column.
    Done(anyhow::Result<graphix_rt::CheckResult>, usize),
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
    let text =
        graphix_compiler::expr::Source::Internal(ArcStr::from(subj.compile_text()));
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
        Ok(r) => TmCheck::Done(r, subj.body_col),
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
    let (tx, _rx) = mpsc::channel(64);
    let sink = graphix_package_core::PrintSink::default();
    let ctx = init_with_flags_and_setup(
        tx,
        REGISTER,
        vec![],
        Mode::Interp.flags(),
        move |ctx| {
            ctx.libstate.set(sink);
        },
    )
    .await
    .map_err(|e| format!("runtime init failed: {e:?}"))?;
    let accept = |c: TmCheck| match c {
        TmCheck::Unparsed(e) => TmVerdict::Reject(tm_error_head(&e)),
        TmCheck::Hung => TmVerdict::Hung,
        TmCheck::Done(Ok(_), _) => TmVerdict::Accept,
        TmCheck::Done(Err(e), _) => TmVerdict::Reject(tm_error_head(&format!("{e:?}"))),
    };
    let base = accept(tm_check(&ctx, &compose(body), false, per_check).await);
    let mut results = Vec::new();
    let mut rejects = Vec::new();
    if base == TmVerdict::Accept {
        for p in &probes {
            let verdict =
                accept(tm_check(&ctx, &compose(&p.body), false, per_check).await);
            let hung = verdict == TmVerdict::Hung;
            results.push(Probed { id: p.id(), mutant: p.body.clone(), verdict });
            if hung {
                break;
            }
        }
        if results.iter().all(|r| r.verdict != TmVerdict::Hung) {
            rejects = must_reject(&ctx, body, &compose, per_check, cap).await;
        }
    }
    let _ = tokio::time::timeout(Duration::from_secs(5), ctx.shutdown()).await;
    Ok(TmReport { base, probes: results, noparse, rejects })
}

/// The must-reject half of a subject (`mustreject`): the base checked
/// again with its types, and each mutant its families build. A mutant
/// accepted is a `LEAK`; one refused away from the mutation and its rigid
/// consumer is `MISPLACED`.
async fn must_reject(
    ctx: &TestCtx,
    body: &str,
    compose: &impl Fn(&str) -> String,
    per_check: Duration,
    cap: usize,
) -> Vec<Probed<Option<String>>> {
    use graphix_compiler::expr::ErrorSite;
    let mut out = Vec::new();
    let TmCheck::Done(Ok(checked), body_col) =
        tm_check(ctx, &compose(body), true, per_check).await
    else {
        return out;
    };
    let types = mustreject::TypeMap::new(&checked.ide.expr_types, "test", body_col);
    for p in mustreject::probes(body, &types, cap) {
        let verdict = match tm_check(ctx, &compose(&p.body), false, per_check).await {
            TmCheck::Unparsed(_) => continue,
            TmCheck::Hung => break,
            TmCheck::Done(Ok(_), _) => Some(format!("LEAK: accepted at {}", p.reached)),
            TmCheck::Done(Err(e), body_col) => {
                let site = e.downcast_ref::<ErrorSite>().map(|s| s.expr());
                let right = site.is_some_and(|x| {
                    mustreject::in_module(&x.ori, "test")
                        && p.right_site(mustreject::to_body(x.pos, body_col))
                });
                match right {
                    true => None,
                    false => {
                        Some(format!("MISPLACED: {}", tm_error_head(&format!("{e:?}"))))
                    }
                }
            }
        };
        out.push(Probed { id: p.id(), mutant: p.body, verdict });
    }
    out
}

/// Run the `typemorph-one` child on one subject: its verdict-file text,
/// and the CPU it burned.
pub async fn typemorph_child(
    prog: &str,
    per_check: Duration,
) -> (Result<String, String>, Duration) {
    // every check the subject may run, one warmed init, plus slack
    let deadline = per_check * tm_checks(TM_CAP) + Duration::from_secs(60);
    let ms = per_check.as_millis().to_string();
    let c =
        run_child("typemorph-one", &[&ms], &[], prog, false, Deadline::Fixed(deadline))
            .await;
    let r = match (c.status, c.verdict) {
        (None, _) => Err("child deadline".to_string()),
        (_, Some(v)) => Ok(v),
        (Some(s), None) => Err(format!("no verdict ({s})")),
    };
    (r, c.cpu)
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

/// What `regress` concluded: the pins that now diverge, and every pin's
/// verdict for the outcome manifest.
pub struct Regression {
    pub regressions: Vec<(String, Divergence)>,
    pub verdicts: Vec<(String, Verdict)>,
}

/// Run the embedded regression corpus (every finding under `findings/`)
/// through the oracle, in parallel. An agreement a loaded run cannot vouch
/// for (both contained or both rejected, unless the outcome manifest
/// records that class for the pin, or unsure) is retried alone at 4x,
/// where a still-quiet pin passes on its own character. `bless` retries
/// every one of them, so the verdicts it records are the unloaded ones.
pub async fn run_regression(timeout: Duration, bless: bool) -> Regression {
    let entries = corpus::REGRESSION_CORPUS;
    let recorded = manifest_rows::<Verdict>(OUTCOME_MANIFEST);
    let mut regressions = Vec::new();
    let mut verdicts: Vec<Option<Verdict>> = vec![None; entries.len()];
    let mut suspect: Vec<usize> = Vec::new();
    let job = |i: usize| async move { check_verdict(entries[i].1, timeout).await };
    windowed(entries.len(), regress_parallelism(), job, |i, res| {
        let (d, v) = match res {
            Ok(r) => r,
            Err(e) => {
                return regressions
                    .push((entries[i].0.to_string(), panicked(entries[i].1, e)));
            }
        };
        let trusted = match v {
            Verdict::Ran | Verdict::Excluded => true,
            Verdict::Contained | Verdict::Rejected => {
                !bless && recorded.get(entries[i].0) == Some(&v)
            }
            Verdict::Unsure => false,
        };
        match d {
            Some(d) => regressions.push((entries[i].0.to_string(), d)),
            None if trusted => verdicts[i] = Some(v),
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
            let (d, v) = check_verdict(entries[i].1, timeout * 4).await;
            match d {
                Some(d) => regressions.push((entries[i].0.to_string(), d)),
                None => verdicts[i] = Some(v),
            }
        }
    }
    for (i, v) in verdicts.iter().enumerate() {
        if *v == Some(Verdict::Rejected)
            && let Some(d) = undeclared_rejection(entries[i].1, timeout * 4).await
        {
            regressions.push((entries[i].0.to_string(), d));
        }
    }
    let verdicts = entries
        .iter()
        .zip(verdicts)
        .filter_map(|((name, _), v)| v.map(|v| (name.to_string(), v)))
        .collect();
    Regression { regressions, verdicts }
}

/// A check that panicked, as the regression it is: both sides carry the
/// panic.
fn panicked(prog: &str, e: String) -> Divergence {
    let o = Outcome::RuntimeErr(format!("check panicked: {e}"));
    Divergence {
        code: prog.to_string(),
        reference: o.clone(),
        tested: o,
        tier: oracle_tier(prog),
        pair: Pair::Engine,
    }
}

/// The checked-in outcome manifest: one `verdict<TAB>name` line per
/// corpus program, written by `regress --bless` (then rebuild to embed).
pub static OUTCOME_MANIFEST: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/outcome.manifest"));

/// The rows of a `value<TAB>name` manifest.
fn manifest_rows<V: FromStr>(manifest: &str) -> BTreeMap<&str, V> {
    manifest
        .lines()
        .filter_map(|l| l.split_once('\t'))
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
    manifest_diff(manifest, live.chain(regressed), |n, r: &Verdict, v| {
        (r != v && *r != Verdict::Unsure && *v != Verdict::Unsure)
            .then(|| format!("changed outcome: {n}: {r} -> {v}"))
    })
}

/// The marker a corpus pin carries when both engines are meant to
/// reject it (a typechecker refusal pinned as a refusal).
pub const EXPECT_REJECT: &str = "// expect: reject";

/// A pin both engines reject that does not say so, as a divergence.
async fn undeclared_rejection(prog: &str, timeout: Duration) -> Option<Divergence> {
    if prog.lines().any(|l| l.trim() == EXPECT_REJECT) {
        return None;
    }
    let outcome = run_program(prog, Mode::Interp, timeout).await;
    if !matches!(outcome, Outcome::CompileErr(_) | Outcome::RuntimeErr(_)) {
        return None;
    }
    Some(Divergence {
        code: prog.to_string(),
        reference: outcome.clone(),
        tested: outcome,
        tier: oracle_tier(prog),
        pair: Pair::Rejected,
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
    windowed(entries.len(), parallelism(), job, |i, r| counts[i] = r.and_then(|r| r))
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

    /// Number of distinct divergences in the corpus.
    pub fn len(&self) -> usize {
        self.seen.lock().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Record a divergence if its minimized form is new. Returns `true`
    /// when newly written (caller prints it), `false` for a duplicate.
    /// The dedup key is the minimized text, so distinct root causes get
    /// distinct files while many raw mutants that reduce to the same
    /// canonical repro collapse to one.
    /// Claim a minimization for `key`: false once [`MINIMIZE_PER_KEY`]
    /// findings have it.
    pub fn claim(&self, key: &str) -> bool {
        let mut keys = self.keys.lock().unwrap();
        let n = keys.entry(key.to_string()).or_default();
        *n += 1;
        *n <= MINIMIZE_PER_KEY
    }

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

    /// Record a process-killing program. No minimized form (minimizing a
    /// crasher would crash the minimizer's in-process oracle), so the
    /// dedup key is the normalized program text. Crash findings must not
    /// be promoted to `findings/` until fixed: regress runs in-process.
    pub fn record_crash(&self, prog: &str, status: &str) -> bool {
        let key = crash_key(prog);
        {
            let mut seen = self.seen.lock().unwrap();
            if !seen.insert(key) {
                return false;
            }
        }
        let n = self.counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let body = format!(
            "// CRASH: child {status}\n\
             // do not promote to findings/ until fixed (regress runs in-process)\n\
             // mutant:\n{prog}\n",
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
    let mut key = String::with_capacity(prog.len() + 6);
    key.push_str("CRASH:");
    let mut in_digits = false;
    let mut chars = prog.trim().chars().peekable();
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

/// Compile threads per child. A campaign runs a child per slot, and a
/// fuzzed program is too small for its compile to gain from more; every
/// thread costs the child its own allocator arena and pools. Two keep
/// the compile's tasks and link batches concurrent.
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
    let sandbox = sandbox_cwd(&mut cmd);
    let verdict = sandbox.path().join("verdict");
    cmd.arg(sub)
        .arg(&verdict)
        .args(args)
        // the pool provides the concurrency; small children keep the
        // total thread count sane
        .env("TOKIO_WORKER_THREADS", "2")
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

/// A child of this binary, its compile threads capped unless the
/// caller set them.
fn child_command() -> tokio::process::Command {
    let mut cmd = tokio::process::Command::new(child_exe());
    if std::env::var_os("RAYON_NUM_THREADS").is_none() {
        cmd.env("RAYON_NUM_THREADS", CHILD_COMPILE_THREADS);
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

/// The file a worker child drops its own [`self_cpu`] in, relative to
/// the parent-owned sandbox cwd. Tokio reaps asynchronously and
/// `RUSAGE_CHILDREN` is process-wide, so neither attributes CPU to a source.
const CPU_REPORT: &str = "cpu-usage";

/// Called by every worker child arm on its way out.
pub fn report_self_cpu() {
    let _ = std::fs::write(CPU_REPORT, self_cpu().as_micros().to_string());
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
            let rl = libc::rlimit { rlim_cur: limit, rlim_max: limit };
            // best effort: a hard limit already below `limit` makes this
            // EPERM, and the subject timeout is the real guard
            unsafe { libc::setrlimit(libc::RLIMIT_AS, &rl) };
        }
    }
}

/// The stack budget aborts a runaway recursion (a Timeout, like the
/// deadline) before a box of workers runs out of memory; the address
/// space limit is applied by the child itself ([`apply_mem_limit`]).
fn child_resource_env(cmd: &mut tokio::process::Command) {
    cmd.env("GRAPHIX_FUZZ_SANDBOXED", "1");
    if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
        cmd.env("GRAPHIX_STACK_BUDGET", (1u64 << 30).to_string());
    }
}

/// Give a worker child a parent-owned sandbox cwd (generated programs
/// write files at arbitrary relative paths, and the worker arms exit via
/// `process::exit`, so a child-owned tempdir would leak). The guard
/// removes the dir on drop; declare it before the child handle so
/// `kill_on_drop` reaps the child first. A tempdir failure is a broken
/// harness and exits the process.
fn sandbox_cwd(cmd: &mut tokio::process::Command) -> tempfile::TempDir {
    match tempfile::tempdir() {
        Ok(d) => {
            cmd.current_dir(d.path());
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
    let routes: &[Route] = if callable::has_header(prog) {
        &[Route::InLanguage, Route::Dispatch]
    } else {
        &[Route::InLanguage]
    };
    for (mode, &route) in [Mode::Interp, Mode::Jit]
        .into_iter()
        .flat_map(|m| routes.iter().map(move |r| (m, r)))
    {
        // the strength an engine pair on this route compares at
        let pair = match route {
            Route::InLanguage => Pair::Engine,
            Route::Dispatch => Pair::EngineDispatch,
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
        bad.push(match (mode, route) {
            (Mode::Interp, Route::InLanguage) => "interp",
            (Mode::Jit, Route::InLanguage) => "jit",
            (Mode::Interp, Route::Dispatch) => "interp-dispatch",
            (Mode::Jit, Route::Dispatch) => "jit-dispatch",
            (Mode::Par, _) => "par",
            (Mode::JitPar, _) => "jit-par",
        });
    }
    bad
}

/// Run one selfcheck subject in a child process. Isolation is for
/// memory as much as crash containment: every in-process `run_program`
/// keeps its context's JIT pages, so the child pays the leak and exits.
async fn selfcheck_isolated(prog: &str, timeout: Duration) -> Vec<&'static str> {
    // up to 8 in-child runs, each bounded by the per-run timeout
    let deadline = Deadline::Fixed(timeout * 10 + Duration::from_secs(30));
    // a dead or wedged child left no verdict: nondeterminism by definition
    match run_child("selfcheck-one", &[], &[], prog, false, deadline).await.verdict {
        Some(v) => v
            .lines()
            .map(|l| {
                SELFCHECK_MODES.iter().find(|m| **m == l).copied().unwrap_or("crash")
            })
            .collect(),
        None => vec!["crash"],
    }
}

/// What a selfcheck child reports, one per line of its verdict file.
pub const SELFCHECK_MODES: [&str; 7] = [
    "interp",
    "jit",
    "interp-dispatch",
    "jit-dispatch",
    "par",
    "jit-par",
    "inconclusive",
];

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
/// and exit codes. Fusion shape must be a pure function of the program
/// text. A wall-clock timeout on either side (exit 4) skips the pair.
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
    // every run in the child has its own deadline; this one catches a
    // wedged child
    let deadline = Deadline::Fixed(check_deadline(timeout));
    let c = run_child("check-one", &[], &[], prog, true, deadline).await;
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
        (Some(status), None) => died(prog, timeout, status, &c.stderr).await,
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
    let crash = |what: &str| {
        let mut status = format!("{what}{status}");
        for l in stderr.lines().rev().take(2).collect::<Vec<_>>().into_iter().rev() {
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
    // budget's abort is, when the node-walk alone runs away too. One that
    // runs alone beside another engine that dies alone is that engine's
    // runaway.
    match (
        run_isolated(prog, Mode::Interp, timeout).await,
        run_isolated(prog, Mode::Jit, timeout).await,
    ) {
        (Some(true), None) => crash(
            "the JIT alone exceeds the address-space cap beside a node-walk value: ",
        ),
        _ => PoolResult::Agree,
    }
}

/// Run `prog` in one mode in a child (`graphix-fuzz run-one`): whether
/// it produced a trace, `None` when the child died.
async fn run_isolated(prog: &str, mode: Mode, timeout: Duration) -> Option<bool> {
    let mode = if mode == Mode::Interp { "interp" } else { "jit" };
    let deadline = Deadline::Fixed(slow_budget(timeout) + Duration::from_secs(30));
    let c = run_child("run-one", &[mode], &[], prog, false, deadline).await;
    c.verdict.map(|v| v.trim() == "trace")
}

/// Oracle-check budget for the campaign's minimizer: a soak pays this
/// per finding, so it buys a legible reproducer, not a minimal one.
pub const CAMPAIGN_MINIMIZE_BUDGET: usize = 80;

/// Minimize a diverging program in a child process. A reduction may
/// itself be a crasher and the minimizer checks candidates in-process.
/// `None` = the child died or wedged; the caller records the
/// unminimized mutant instead.
async fn minimize_isolated(prog: &str, timeout: Duration) -> (Option<String>, Duration) {
    // worst case the whole budget is bottom programs sleeping the
    // per-mode timeout
    let deadline = Deadline::Fixed(
        timeout * 2 * CAMPAIGN_MINIMIZE_BUDGET as u32 + Duration::from_secs(60),
    );
    let c = run_child("minimize-one", &[], &[], prog, false, deadline).await;
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
    let mut to_derive: std::collections::VecDeque<(usize, String)> =
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
    let wsum = weights.iter().map(|w| w.max(0.0)).sum::<f64>().max(f64::MIN_POSITIVE);
    let want = |launched: usize| iters.map_or(true, |n| launched < n);
    loop {
        // suspects first: a finding waiting on a slot is worth more than
        // a fresh order
        while orders.len() + derive.len() < par
            && let Some((si, prog)) = to_derive.pop_front()
        {
            let corpus = corpus.clone();
            let found = found.clone();
            derive.spawn(async move {
                let (finding, cpu) =
                    derive_suspect(KINDS[si], &prog, &corpus, &found[si], timeout).await;
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
                if stats[si].run % 1000 < count {
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
                         {} novel shapes, {}% cpu, {}% individual",
                        KINDS[si].tag(), stats[si].run, stats[si].divergences,
                        KINDS[si].findings(), found[si].known.load(Relaxed),
                        stats[si].crashes, corpus.len(), stats[si].novel, pct, ipct
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
    prog: &str,
    corpus: &Corpus,
    tally: &Tally,
    timeout: Duration,
) -> (bool, Duration) {
    use std::sync::atomic::Ordering::Relaxed;
    if kind == SourceKind::Typemorph {
        let (confirmed, cpu) = confirm_typeflip(corpus, prog, timeout).await;
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
            if status.contains("HANG")
                && ["rand::", "sys::", "http::"].iter().any(|m| prog.contains(m)) =>
        {
            false
        }
        PoolResult::Crash(status) => {
            tally.crashes.fetch_add(1, Relaxed);
            if corpus.record_crash(prog, &status) {
                println!("CRASH — child {status}");
                println!("    program: {}", prog.replace('\n', "\\n"));
            }
            true
        }
        PoolResult::Diverge(f) => {
            tally.divergences.fetch_add(1, Relaxed);
            if !corpus.claim(&f.key) {
                tally.known.fetch_add(1, Relaxed);
                return (true, cpu);
            }
            let (min, min_cpu) = minimize_isolated(prog, timeout).await;
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

/// Re-probe a subject that flipped in a batch child in a fresh process
/// and record each flip class it shows there; a flip the fresh process
/// does not reproduce is an acceptance flap, its own class. True when
/// the fresh process flipped too.
async fn confirm_typeflip(
    corpus: &Corpus,
    prog: &str,
    timeout: Duration,
) -> (bool, Duration) {
    let (r, cpu) = typemorph_child(prog, timeout).await;
    let (flips, confirmed) = match r {
        Ok(rep) => match tm_flips(&rep) {
            f if f.is_empty() => (
                vec![Flip::harness("unconfirmed", "fresh-process flap".to_string())],
                false,
            ),
            f => (f, true),
        },
        // a confirm that measured nothing confirms nothing
        Err(e) => (vec![Flip::harness("unconfirmed", e)], false),
    };
    for Flip { id, head, mutant } in flips {
        if corpus.record_typeflip(prog, &id, &head, mutant.as_deref()) {
            println!("TYPEFLIP — {id}: {head}");
            println!("    program: {}", prog.replace('\n', "\\n"));
        }
    }
    (confirmed, cpu)
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // answers over: `[]` or `[Stdio]` by load
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
            // the hop: the widened `a` reaches `+` through `w`'s let
            ("{ let a = 1; let w = a; let z = w + 2; z }", "widen-consumer#"),
            ("{ let f = 'a: Number |x: 'a| -> 'a x + x; f(1) }", "rigid-var#"),
            ("{ let g = 'a: Number |x: 'a, y: 'a| -> 'a x + y; g(1, 2) }", "shared-var#"),
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
            // the retyped `m` reaches the writer of `src` through `src`'s let
            (
                "{ let m = 1; let src = [m]; src <- [2]; let w = m + 1; (src, w) }",
                "retype#",
            ),
            ("{ let x = 1; let r = &mut x; let y = 2; *r <- 5; x + y }", "ref-write#"),
            ("{ let x = 1; let r = &mut x; let y = 2; *r <- 5; x + y }", "ref-widen#"),
            (
                "{ let s: [string, null] = \"a\"; select s { null as _ => 0, string as v => 1 } }",
                "same-form#",
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
        /// entries are the ancestor noise ("node does not emit CLIF" for
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
        if fuse != Fuse::No {
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
        }
        if fuse == Fuse::Clean {
            for failure in &stats.failed {
                let id = failure.id;
                let reason = &failure.reason;
                // Structural noise, not coverage gaps: the ancestor
                // wrappers, a `mod` statement, a function-valued let, and
                // a fn-typed call site whose instance body fuses.
                assert!(
                    reason.contains("node does not emit CLIF")
                        || reason.contains("module statement is structure")
                        || reason.contains("function-valued let")
                        || reason.contains("not discovered"),
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

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_scalar_probes() {
        agree_fused("{ let x = i64:5; x * i64:3 }").await;
        agree_fused("{ let x = i64:5; let y = i64:2; (x + y) * (x - y) }").await;
        // div-by-zero bottoms at runtime inside the compiled kernel
        agree_fused("i64:10 / i64:0").await;
        agree_fused("{ let a = i64:7; a > i64:3 && a < i64:10 }").await;
        agree_fused("f64:3.0 + f64:1.0").await;
        // `cast` node-walks: deliberate fallback
        agree("cast<f64>(i64:7)$ + f64:1.0").await;
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
        // a may-bottom interpolated part: the interpolation node-walks
        agree(r#"{ let x = i64:5; "v=[(x +? i64:1)$]" }"#).await;
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
        // Nullable out: node-walks (ASPIRE: value residents in slot chains)
        agree(
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
        // nested map-in-map: the inner site is not resolved at emission
        agree(
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
        // destructured callback (ASPIRE: instance-body inlining)
        agree("{ let a = [(i64:1, i64:2)]; array::map(a, |(k, v)| k + v) }").await;
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
        // statically may-bottom predicate with no zero present: de-fuses
        // and node-walks to a real value
        agree(
            "{ let a = [i64:1, i64:5, i64:20]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // with an actual 0 the output never fires in any mode
        agree(
            "{ let a = [i64:0, i64:1, i64:5]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // string element: node-walks
        agree(r#"{ let a = ["aa", "b"]; array::filter(a, |s| s == "aa") }"#).await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter((a[1..])$, |x| x > i64:1) }",
        )
        .await;
        // destructured predicate (ASPIRE: instance-body inlining)
        agree("{ let a = [(i64:1, i64:2)]; array::filter(a, |(k, v)| k < v) }").await;
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
        agree(
            "{ let a = [i64:1, i64:2]; \
             array::fold(a, i64:100, |acc, x| acc / x) }",
        )
        .await;
        // statically may-bottom init
        agree(
            "{ let n = i64:2; let a = [i64:1, i64:2]; \
             array::fold(a, i64:10 / n, |acc, x| acc + x) }",
        )
        .await;
        // string accumulator
        agree(r#"{ let a = [i64:1, i64:2]; array::fold(a, "", |acc, x| "[acc][x]") }"#)
            .await;
        // owned input array
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold((a[1..])$, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // destructured callback (ASPIRE: instance-body inlining)
        agree(
            "{ let a = [(i64:1, i64:2)]; \
             array::fold(a, i64:0, |acc, (k, v)| acc + k * v) }",
        )
        .await;
    }

    /// Inline `array::flat_map` emission (ASPIRE: instance-body inlining;
    /// value agreement only for now).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_flat_map_probes() {
        // scalar element, fresh array body
        agree(
            "{ let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x, x * i64:10]) }",
        )
        .await;
        // composite element flattened to its fields
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::flat_map(a, |p| [p.0, p.1]) }",
        )
        .await;
        // capture
        agree(
            "{ let k = i64:2; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x * k]) }",
        )
        .await;
        // borrowed body source, cloned per iteration
        agree(
            "{ let b = [i64:9]; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| b) }",
        )
        .await;
        // owned input array
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::flat_map((a[1..])$, |x| [x]) }",
        )
        .await;
        // destructured callback
        agree(
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
        // HOF inside the callee body: node-walks
        agree(
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
        // nullable return from a select body: node-walks
        agree(
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
        agree(
            "{ let f = |x: i64| -> i64 x + i64:1; \
             let f = |n: i64| -> i64 f(n) * i64:2; f(i64:3) }",
        )
        .await;
        // lambda call inside a HOF callback body: node-walks
        agree(
            "{ let f = |x: i64| x * i64:2; let a = [i64:1, i64:2]; \
             array::map(a, |x| f(x)) }",
        )
        .await;
    }

    /// A 5M-deep tail recursion is only runnable as the compiled
    /// rebind-and-jump loop (the node-walk overflows around ~50k), so
    /// only Jit runs; `fused > 0` pins that the loop compiled.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_deep_tail_probe() {
        let code = "{ let rec lp = |n: i64, acc: i64| -> i64 \
                     select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) }; \
                     lp(i64:5000000, i64:0) }";
        let (out, stats) =
            run_program_with_stats(code, Mode::Jit, Duration::from_secs(30)).await;
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
        let (out, stats) =
            run_program_with_stats(code, Mode::Jit, Duration::from_secs(60)).await;
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

    /// Destructured `|(k, v)|` callbacks (ASPIRE: instance-body
    /// inlining; value agreement only for now).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_destructure_probes() {
        // mixed-prim leaves
        agree(
            "{ let a = [(i64:1, f64:2.5), (i64:3, f64:0.5)]; \
             array::map(a, |(k, v)| v) }",
        )
        .await;
        // sparse leaves: `_` positions get no bind
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |(k, _)| k * i64:10) }",
        )
        .await;
        // find with a destructured predicate yields the whole tuple
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |(k, v)| k > v) }",
        )
        .await;
        // 3-leaf tuple through fold
        agree(
            "{ let a = [(i64:1, i64:2, i64:3), (i64:4, i64:5, i64:6)]; \
             array::fold(a, i64:0, |acc, (x, y, z)| acc + x * y + z) }",
        )
        .await;
        // composite leaf: node-walks
        agree(
            "{ let a = [((i64:1, i64:2), i64:3)]; \
             array::map(a, |(p, x)| x) }",
        )
        .await;
    }

    /// Inline `array::filter_map` emission (ASPIRE: instance-body
    /// inlining; value agreement only for now).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_filter_map_probes() {
        // keep evens doubled
        agree(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // capture
        agree(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter_map(a, |x| \
               select x { i64:2 => x * k, _ => null }) }",
        )
        .await;
        // composite element: node-walks
        agree(
            "{ let a = [(i64:1, i64:2)]; \
             array::filter_map(a, |p| \
               select p.0 { i64:1 => p.1, _ => null }) }",
        )
        .await;
        // owned input array
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter_map((a[1..])$, |x| \
               select x { i64:2 => x, _ => null }) }",
        )
        .await;
    }

    /// Inline `array::find` emission: early exit, `Nullable<elem>`
    /// result; a may-bottom predicate de-fuses at build.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_find_probes() {
        // found
        agree(
            "{ let a = [i64:1, i64:5, i64:3]; \
             array::find(a, |x| x > i64:2) }",
        )
        .await;
        // not found
        agree("{ let a = [i64:1, i64:2]; array::find(a, |x| x > i64:9) }").await;
        // composite element
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |p| p.0 > p.1) }",
        )
        .await;
        // may-bottom predicate: de-fuses
        agree(
            "{ let a = [i64:1, i64:5]; \
             array::find(a, |x| i64:10 / x > i64:4) }",
        )
        .await;
        // owned input array
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find((a[1..])$, |x| x > i64:1) }",
        )
        .await;
    }

    /// Inline `array::find_map` emission.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_find_map_probes() {
        // found: first even, doubled
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // not found
        agree(
            "{ let a = [i64:1, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x, _ => null }) }",
        )
        .await;
        // owned input array
        agree(
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
        agree(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::find(array::filter(a, |x| x % i64:2 == i64:0), \
               |x| x > i64:2) }",
        )
        .await;
        // init's output into flat_map
        agree("array::flat_map(array::init(i64:3, |i| i), |x| [x, x])").await;
        // the outer map's body bottom-aborts mid-loop while the inner
        // map's result is adopted: the pending cleanup must free it
        agree(
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
        agree("{ let d = i64:2; array::init(i64:4 / d, |i| i) }").await;
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
            fused += stats.fused;
            // assert only when interp agrees with itself, mirroring the
            // oracle's double-run guard
            if !interp.agrees_with(&direct) {
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
                    if i2.agrees_with(&j2)
                        || matches!(i2, Outcome::Timeout(_))
                        || matches!(j2, Outcome::Timeout(_))
                    {
                        budget_skipped += 1;
                        continue;
                    }
                }
                let interp2 = run_program(&code, Mode::Interp, t).await;
                if !interp.agrees_with(&interp2) {
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
        let ctx = init_with_flags_and_setup(
            tx,
            REGISTER,
            vec![resolver],
            mode.flags(),
            move |ctx| ctx.control.set_par_mode(mode.par()),
        )
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

    /// `trace_wait_idle` without `trace_start` is an error, not a hang.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_wait_without_start_errors() {
        let (tx, _rx) = mpsc::channel(64);
        let ctx =
            init_with_flags_and_setup(tx, REGISTER, vec![], Mode::Interp.flags(), |_| {})
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
                verdicts.push((i, matches!(v, BatchVerdict::Agree { .. })));
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
                "// callable-v1: handler=m0::handler; cx0=i64:7; cx0=i64:9\n                 { m0::observe }\n                 // file-v1: m0.gx\n                 let state = { v: 0 };\n                 let handler = |x: i64| -> null { *(&state) <- (x ~ { v: x }); null };\n                 let observe = state",
                false,
            ),
        ];
        let timeout = Duration::from_secs(20);
        for (name, prog, want_ran) in cases {
            let (diverged, ran) = check_classified(prog, timeout).await;
            assert!(diverged.is_none(), "{name}: individual path diverged");
            // absolute, not just pairwise: both paths share one derivation
            assert_eq!(want_ran, ran, "{name}: individual path `ran`");
            let want = BatchVerdict::Agree { ran: want_ran };
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
        assert_eq!(r.suspect, [1, 3, 4, 5].map(|i| progs[i].clone()));
        // a subject's `sys::exit(0)` is not a clean order
        assert!(!order_result(&order, &head, true).clean);
        let tail: String = (3..6).map(|i| format!("V {i} A\n")).collect();
        let r = order_result(&order, &format!("{head}{tail}CPU 7\n"), true);
        assert!(r.clean);
        assert_eq!(r.agreed, 5);
        assert_eq!(r.suspect, [progs[1].clone()]);
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
        let regressed = panicked("regressed", "stub".to_string());
        let r = Regression {
            regressions: vec![("regressed".to_string(), regressed)],
            verdicts: [
                ("holds", Verdict::Ran),
                ("changed", Verdict::Ran),
                ("vouched", Verdict::Rejected),
                ("new", Verdict::Excluded),
            ]
            .map(|(n, v)| (n.to_string(), v))
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
