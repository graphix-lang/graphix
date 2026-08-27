//! Differential model-checking oracle for the graphix fusion/JIT backend.
//!
//! A program is run under two compiler-flag configurations of the
//! *same* front-end:
//!   - **interp** (`CFlag::FusionDisabled`) — the node-walk interpreter,
//!     the simple, more-trusted reference model.
//!   - **jit** (no flags) — the fusion + cranelift-JIT backend, the
//!     system under test.
//!
//! For any deterministic program the configurations must produce the
//! same observable result. A difference proves a bug exists — usually in
//! fusion+JIT (it is far more complex), though the node-walk is the
//! more-trusted model, not infallible. See `design/graphix_fuzz.md`.
//!
//! The observable result is a per-cycle TRACE ([`trace::Trace`]): every
//! value `result` emits, with its cycle offset — so extra fires,
//! missing fires, and wrong pacing are divergences, not just wrong
//! first values. A program that never emits (bottom) is an instant
//! empty-trace agreement, resolved at runtime quiescence rather than by
//! waiting out a timeout.

pub mod callable;
pub mod corpus;
pub mod files;
pub mod generate;
pub mod mutate;
pub mod schedule;
pub mod trace;
pub mod typemorph;

use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{
    CFlag, FusionStats,
    expr::{Expr, VfsEntry, VfsResolver},
};
use graphix_package::Package;
use graphix_package_core::testing::{TestCtx, init_with_flags_and_setup};
use graphix_rt::{GXEvent, NoExt};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_core::path::Path;
use std::{future, time::Duration};
use tokio::sync::mpsc;

/// Every stdlib package, so generated programs can use the whole
/// language surface. Mirrors `graphix-tests`'s `TEST_REGISTER` (which is
/// `#[cfg(test)]`-gated and so not importable).
pub const REGISTER: &[&dyn Package<NoExt>] = graphix_package::package_refs!();

/// The mode a program was run under.
///
/// There are only two evaluators: the node-walk (the reference) and
/// fusion + cranelift JIT (the system under test). There is no third
/// "fuse but don't JIT" mode — fusion is JIT-only (no interpreter to
/// dispatch a built-but-unspliced kernel into), so the single
/// `FusionDisabled` flag toggles all of fusion on or off.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Node-walk interpreter (`CFlag::FusionDisabled`) — the reference.
    Interp,
    /// Fusion + cranelift JIT (no flags) — the system under test.
    /// Since the F2 flip (2026-06-13, `design/distributed_jit.md`)
    /// this is the direct node-emission path (`Update::emit_clif`
    /// recursion).
    Jit,
}

impl Mode {
    pub fn flags(self) -> BitFlags<CFlag> {
        match self {
            Mode::Interp => CFlag::FusionDisabled.into(),
            Mode::Jit => BitFlags::empty(),
        }
    }
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
    /// The runtime neither quiesced nor hit the trace budget within
    /// the wall-clock backstop — a wedged evaluator, or a program that
    /// spins forever without its `result` ever firing (only firing
    /// cycles count against the budget) — or the stack budget aborted
    /// it first: both are containment outside the language, and a
    /// runaway stopped by either is the same outcome.
    Timeout,
}

/// Strip tvar numbers (`'_6070` -> `'_N`) and abstract-type ids
/// (`<abstract#12>` -> `<abstract#N>`) so fresh-counter drift between
/// two independent compiles neither hides nor fakes a diagnostic
/// difference (the tvar half is the same normalization as
/// graphix-shell's check_mode_parity gate; the abstract half covers
/// `Type::Abstract`'s display, whose process-global id interleaves
/// between two CONCURRENT compiles — it flaked selfcheck's CompileErr
/// comparison on the list-recursion seed, 2026-08-19).
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
    /// structurally — values (graphix total equality: `-0.0 == 0.0`,
    /// `NaN == NaN`), relative pacing, and cap flags. Different outcome
    /// *kinds* (e.g. Trace vs Timeout, or Trace vs RuntimeErr) always
    /// disagree — that is the signal for an asymmetric hang or a
    /// fusion-introduced error. Same-kind non-trace outcomes agree
    /// without comparing their (mode-dependent) messages.
    pub fn agrees_with(&self, other: &Outcome) -> bool {
        use Outcome::*;
        match (self, other) {
            (Trace(a), Trace(b)) => a.agrees_with(b),
            // Both-reject is agreement only if they reject
            // IDENTICALLY (modulo tvar numbering — fresh-counter
            // drift between two compiles): fusion-mutates-tvars was a
            // --check diagnostic skew invisible to kind-only
            // comparison (fuzzer gap 4). RuntimeErr messages stay
            // kind-only — they are mode-dependent by design.
            (CompileErr(a), CompileErr(b)) => normalize_diag(a) == normalize_diag(b),
            (RuntimeErr(_), RuntimeErr(_)) => true,
            (Timeout, Timeout) => true,
            // Both-non-productive: one side wedged (a pure runaway
            // recursion the node-walk can't interrupt / a native loop),
            // the other side produced NO events at all (the depth-guard
            // or runaway-guard bottom). Neither produced a value, so
            // there is no observable VALUE divergence — only the known,
            // accepted liveness difference between the backends' runaway
            // handling (the B1 fib-mutant wedge class, soak jul06: 39 of
            // 49 findings were this noise, and the minimizer kept
            // shrinking real programs INTO it by deleting base-case
            // arms). A trace with any event still disagrees with a
            // Timeout — an asymmetric hang after partial output is a
            // real signal.
            (Timeout, Trace(t)) | (Trace(t), Timeout)
                if t.epochs.iter().all(|e| e.events.is_empty()) =>
            {
                true
            }
            _ => false,
        }
    }

    /// [`Self::agrees_with`] at a chosen [`OracleTier`]: Exact compares
    /// whole traces; FinalValues compares per-epoch settled values
    /// ([`trace::Trace::agrees_final`]); non-Trace outcome pairs (and
    /// the both-non-productive guard) follow the exact rules at every
    /// tier. Excluded never reaches a comparison (`check` returns
    /// early), but compares exactly if asked.
    pub fn agrees_with_at(&self, other: &Outcome, tier: OracleTier) -> bool {
        match tier {
            OracleTier::Exact | OracleTier::Excluded => self.agrees_with(other),
            OracleTier::FinalValues => match (self, other) {
                (Outcome::Trace(a), Outcome::Trace(b)) => a.agrees_final(b),
                _ => self.agrees_with(other),
            },
        }
    }

    /// Coarse variant discriminant, for the "same bug" bucket key.
    pub fn kind(&self) -> u8 {
        match self {
            Outcome::Trace(_) => 0,
            Outcome::CompileErr(_) => 1,
            Outcome::RuntimeErr(_) => 2,
            Outcome::Timeout => 3,
        }
    }
}

/// Run `code` — a WRAPPER (optional `// schedule-v1:` header + graphix
/// expression body, see [`schedule::Schedule`]) — under `mode`,
/// returning the per-cycle trace of everything `result` emitted across
/// every epoch (or why nothing ran). The body is wrapped as
/// `let result = {body}`; injected inputs are declared at the compile
/// text's top level per the D4 contract; each epoch's injections are
/// delivered and driven to quiescence or the trace budget in turn.
///
/// A fresh `ExecCtx` + in-process resolver is created per call — fusion
/// state and the per-context JIT do not leak between runs (matching the
/// test harness, and avoiding cranelift codegen-context poisoning across
/// programs).
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

/// Everything needed to compile and drive ONE subject, derived from
/// its source text in a single place.
///
/// The individual path and the batch child each used to derive this
/// for themselves, and drifted apart three times — the batch child's
/// wrap lost `use super::*`, so every batched subject with injected
/// inputs failed to compile; it passed no `mod` declarations, so every
/// batched subject carrying an aux file did the same; and it never
/// parsed the callable header, so callable subjects could not batch at
/// all. The first two were INVISIBLE to the oracle, because a
/// CompileErr agrees with a CompileErr: the subjects reported a clean
/// agreement having never run. Deriving this once is what makes that
/// class of drift unrepresentable.
pub struct Subject {
    pub sched: schedule::Schedule,
    pub spec: Option<callable::CallSpec>,
    pub tier: OracleTier,
    /// `mod <stem>;` per aux `.gx` section, for the compile text's top
    /// level — this is what makes an aux module resolvable at all, and
    /// what `compile_ref_by_name` walks to reach a callable handler.
    pub mods: String,
    /// The module VFS: the wrapped body under `modname`, plus aux files.
    pub table: AHashMap<Path, VfsEntry>,
    /// The module the body is installed as. Unique per subject inside a
    /// batch child (one warmed runtime serves many subjects, so a
    /// shared name would alias their graphs); `test` on the individual
    /// path, which gets a fresh runtime per subject.
    pub modname: String,
}

impl Subject {
    /// `Err` is the message for a `CompileErr` outcome: a malformed
    /// header is a compile-class reject in every mode, so it agrees
    /// everywhere and surfaces in `gen-check` rather than as a phantom
    /// divergence.
    pub fn parse(code: &str, modname: &str) -> Result<Subject, String> {
        let (sched, body) = schedule::Schedule::parse(code)
            .map_err(|e| format!("schedule header: {e}"))?;
        let (spec, body) = callable::CallSpec::parse(body)
            .map_err(|e| format!("callable header: {e}"))?;
        let (body, files) =
            files::split(&body).map_err(|e| format!("file section: {e}"))?;
        // `use super::*` imports the compile text's top-level
        // declarations — the injected inputs and the aux `mod`s. Under
        // the module system a submodule sees NOTHING of its parent
        // implicitly, so without it a subject with inputs cannot name
        // them.
        let wrapped = ArcStr::from(format!("use super::*; let result = {body}"));
        let mut table = AHashMap::from_iter([(
            Path::from(format!("/{modname}.gx")),
            VfsEntry::from(wrapped),
        )]);
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

    /// The text handed to the compiler. Injected-input and callable
    /// declarations sit at the TOP LEVEL (the D4 contract; see
    /// `schedule::Schedule::decls`) — before the module wrap, where
    /// `compile_ref_by_name` can reach them from root — and the aux
    /// `mod`s precede the callable declarations, which reference into
    /// the handler's module.
    pub fn compile_text(&self) -> String {
        let Subject { sched, spec, mods, modname, .. } = self;
        let cdecls = spec.as_ref().map(|c| c.decls()).unwrap_or_default();
        format!("{}{mods}{cdecls}{{ mod {modname}; {modname}::result }}", sched.decls())
    }
}

/// Compile `code` (a wrapper, as [`run_program`]) under `mode` WITHOUT
/// driving it — `None` = compiled clean, `Some(error)` = parse/typecheck
/// reject (or the runtime failed to init). This is `gen-check`'s
/// primitive: the generator is type-correct by construction, so the
/// compile-reject RATE is its health metric and each reject message is
/// a tuning signal.
pub async fn compile_program(code: &str, mode: Mode) -> Option<String> {
    match compile_with_stats(code, mode, Duration::from_secs(60)).await {
        CompileOutcome::Compiled(_) => None,
        CompileOutcome::Rejected(e, _) | CompileOutcome::Failed(e) => Some(e),
    }
}

/// A compile-only measurement. `Rejected` is the COMPILER's verdict on
/// the program — deterministic content, still carrying the stats of
/// whatever DID compile: the corpus's CompileErr-agree pins are real
/// measurements, and one (sample-select-orphan/00) records 1 fused
/// region because its schedule's input decls are separate top-level
/// exprs that compile and fuse before the module body rejects.
/// `Failed` is the MEASUREMENT failing (init, stats read, wedged
/// compile) — no conclusion about the program can be drawn from it.
enum CompileOutcome {
    Compiled(FusionStats),
    Rejected(String, FusionStats),
    Failed(String),
}

/// Compile-only core behind [`compile_program`] and [`run_fusecheck`]:
/// parse the wrapper, init a fresh ctx under `mode`, compile the full
/// drive text (schedule decls + module wrap — the same text `drive`
/// compiles), and return the program's own compile-time
/// [`FusionStats`] delta without ever running an injection epoch. The
/// timeout exists because the first update cycle runs inside `compile`,
/// so a runaway native loop can wedge it. A stats value is NEVER
/// synthesized from a failure: fusecheck once read stats through
/// [`run_program_with_stats`], whose unreadable-stats fallback is
/// `FusionStats::default()` — under load a timed-out DRIVE read as 0
/// fused, so the gate printed phantom LOST-fusion lines and a
/// `--bless` could bake a bogus 0 into the manifest (it did, once,
/// for sample-select-orphan/00).
async fn compile_with_stats(code: &str, mode: Mode, timeout: Duration) -> CompileOutcome {
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return CompileOutcome::Rejected(e, FusionStats::default()),
    };
    let (tx, _rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    // The sink is discarded — seeding it keeps the compile cycle's
    // print output (the first update cycle runs inside `compile`) off
    // the process streams.
    let sink = graphix_package_core::PrintSink::default();
    let ctx = match init_with_flags_and_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        move |ctx| {
            *ctx.libstate.get_or_default::<graphix_package_core::PrintSink>() = sink;
        },
    )
    .await
    {
        Ok(c) => c,
        Err(e) => return CompileOutcome::Failed(format!("runtime init failed: {e:?}")),
    };
    let text = subj.compile_text();
    let run = async {
        let base = match ctx.fusion_stats().await {
            Ok(s) => s,
            Err(e) => {
                return CompileOutcome::Failed(format!("fusion stats read: {e:?}"));
            }
        };
        // Debug format = the multi-line anyhow chain; the last line is
        // the innermost cause, which is what gen-check buckets on.
        let verdict = ctx.rt.compile(ArcStr::from(text)).await;
        match ctx.fusion_stats().await {
            Ok(mut s) => {
                s.attempted -= base.attempted;
                s.fused -= base.fused;
                s.failed.drain(..base.failed.len());
                match verdict {
                    Ok(_) => CompileOutcome::Compiled(s),
                    Err(e) => CompileOutcome::Rejected(format!("{e:?}"), s),
                }
            }
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

/// [`run_program`], also returning the compile-time [`FusionStats`]
/// delta for the program itself. Stats accumulate per `ExecCtx` across
/// every compile the runtime dispatches — including the stdlib root —
/// so the baseline snapshot taken after init is subtracted, leaving
/// only the regions of `code`'s own compile. Stats are compile-time
/// only, so fetching them after the run observes the same values as
/// fetching right after compile.
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
    // A malformed header is a COMPILE-class reject in every mode
    // (agreement) — a generator/minimizer bug surfaces in gen-check,
    // never as a phantom divergence.
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return (Outcome::CompileErr(e), FusionStats::default()),
    };
    let (tx, mut rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    // The stdout oracle's per-runtime capture: the print family
    // (`print`/`println`/`dbg`) writes here instead of the process
    // streams, so two modes running concurrently in one process keep
    // separate output (`PrintSink`, graphix-package-core).
    let sink = graphix_package_core::PrintSink::default();
    let seeded = sink.clone();
    let ctx = match init_with_flags_and_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        move |ctx| {
            *ctx.libstate.get_or_default::<graphix_package_core::PrintSink>() = seeded;
        },
    )
    .await
    {
        Ok(c) => c,
        Err(e) => {
            return (
                Outcome::RuntimeErr(format!("runtime init failed: {e:?}")),
                FusionStats::default(),
            );
        }
    };
    let tier = subj.tier;
    let base = ctx.fusion_stats().await.unwrap_or_default();
    let mut outcome = drive(&ctx, &mut rx, &subj, route, timeout).await;
    // Attach the captured print output — Exact tier only (effect
    // emissions are as deterministic as the values there; FinalValues
    // pacing legitimately varies fire counts). Sorted: within-cycle
    // emission order is an evaluation-order artifact, the multiset is
    // the semantics.
    if tier == OracleTier::Exact
        && let Outcome::Trace(t) = &mut outcome
    {
        let mut lines: Vec<String> = sink.take().lines().map(|l| l.to_string()).collect();
        lines.sort_unstable();
        t.stdout = lines;
    }
    // A stack-budget abort is CONTAINMENT, like the deadline: the
    // runtime it stops reports a RuntimeErr, and the deadline would have
    // reported Timeout on the same runaway a little later — which of the
    // two fires first is a race between how fast each engine descends
    // (the JIT exhausts a 1GB budget in under a second, the node-walk
    // in ~17s), not a property of the program. One outcome for both.
    if matches!(outcome, Outcome::RuntimeErr(_)) && ctx.rt.budget_aborted() {
        outcome = Outcome::Timeout;
    }
    // A Timeout means the evaluator may be WEDGED in sync code (a
    // runaway native loop, a huge node-walk loop) — a wedged runtime
    // never answers another request, so an un-timeouted await here
    // deadlocks the whole (in-process) campaign task. Abort first
    // (breaks a cooperative loop via the sticky flag), then never
    // await the runtime without a deadline.
    if matches!(outcome, Outcome::Timeout) {
        ctx.rt.abort();
    }
    let grace = Duration::from_secs(2);
    let stats = match tokio::time::timeout(grace, ctx.fusion_stats()).await {
        Ok(Ok(mut s)) => {
            s.attempted -= base.attempted;
            s.fused -= base.fused;
            s.failed.drain(..base.failed.len());
            s
        }
        Ok(Err(_)) | Err(_) => FusionStats::default(),
    };
    let _ = tokio::time::timeout(grace, ctx.shutdown()).await;
    (outcome, stats)
}

async fn drive(
    ctx: &TestCtx,
    rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    subj: &Subject,
    route: Route,
    timeout: Duration,
) -> Outcome {
    let Subject { sched, spec, tier, .. } = subj;
    let (spec, tier) = (spec.as_ref(), *tier);
    // The whole multi-epoch drive shares one wall-clock deadline (a
    // backstop for a wedged evaluator only — quiescence and the trace
    // budgets are the real bounds) and one concurrent drain of the
    // event subscription so a chatty program can't fill the channel
    // and stall the runtime.
    let deadline = tokio::time::sleep(timeout);
    tokio::pin!(deadline);
    let drain = async {
        while rx.recv().await.is_some() {}
        future::pending::<()>().await
    };
    tokio::pin!(drain);
    // A deadline breach IS `Timeout`, always — on EVERY step. The
    // interrupt + grace still run, but only so the runtime unwinds
    // cleanly, never to reclassify. Reclassification (an interrupted
    // step that then completes reports its normal result) was a RACE
    // on any step: `interrupt()` is a one-shot flag polled by fused
    // kernels and node-walk guards, so it may have ABORTED an in-
    // flight cycle's output to bottom before the step completed — the
    // run then looks clean but its trace is silently missing events.
    // Wait steps learned this from the deref-echo class (soak jul05
    // items 3/20/24: Timeout vs Trace([]) flipped run-to-run); the
    // compile step kept a lenient reclassify-and-continue because a
    // slow stdlib compile under gate load must not read as a wedged
    // program — but the FIRST UPDATE CYCLE runs inside compile, so an
    // interrupt landing there bottomed a fused kernel's only output
    // and recorded an empty-but-uncapped epoch 0 as a "divergence"
    // (mazikeen jul17b divergences 000000/000001 — load-only, AGREE
    // 4/4 idle on the same binary). Honest Timeout is safe now that
    // `check()`'s escalation ladder retries one-sided timeouts at a
    // 60s-floored budget: legitimately slow runs self-clear, wedged
    // ones stay Timeout.
    macro_rules! bounded {
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
                    return Outcome::Timeout;
                }
            }
        }};
    }
    // One epoch's segment, SETTLED. `trace_wait_idle` sees an in-flight
    // async IO task as idle (it isn't runtime work until its value
    // lands), so for the FinalValues tier quiescence alone races IO
    // completion — the /dev/null probe's read landed after the wait in
    // some runs and the epoch "final" was a coin flip. Grace rounds:
    // sleep, re-wait, merge any late activity into the SAME epoch;
    // stop on the first quiet round. Exact-tier programs skip the
    // settle entirely (pure programs have no in-flight IO, and the
    // 150ms/epoch tax would slow every campaign check).
    macro_rules! wait_settled {
        () => {{
            let mut seg = bounded!(
                ctx.rt.trace_wait_idle(),
                Ok(s) => s,
                Err(e) => return Outcome::RuntimeErr(format!("trace_wait_idle: {e}"))
            );
            if tier == OracleTier::FinalValues {
                for _ in 0..8 {
                    tokio::time::sleep(Duration::from_millis(150)).await;
                    let more = bounded!(
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
    // Tracing is armed BEFORE the compile (ToGX messages are FIFO), so
    // a value emitted during the compile cycle is in the trace — there
    // is no "already emitted before the watch registered" race, and no
    // event-stream fallback. The runtime-side `Compiled` marker anchors
    // epoch 0; an input ref's own echo anchors each injection epoch.
    // Budgets are schedule DATA: identical in every mode, so a cap
    // mismatch is a real divergence.
    if let Err(e) = ctx.rt.trace_start(sched.max_events, sched.max_cycles) {
        return Outcome::RuntimeErr(format!("trace_start: {e}"));
    }
    // Injected-input decls sit at the compile text's TOP LEVEL (the D4
    // contract; see `schedule::Schedule::decls`), before the module
    // wrap, where `compile_ref_by_name` can reach them from root.
    let text = subj.compile_text();
    let compiled = bounded!(
        ctx.rt.compile(ArcStr::from(text)),
        Ok(c) => c,
        Err(e) => return Outcome::CompileErr(format!("{e:?}"))
    );
    let eid = compiled.exprs.last().expect("compile returned no exprs").id;
    let mut segs = Vec::with_capacity(1 + sched.epochs.len());
    segs.push(wait_settled!());
    // Create EVERY input's ref up front (not lazily at first use) so
    // ref creation never interleaves with injection delivery, then
    // deliver each epoch's injections through ONE `set_many`: separate
    // `set` calls can land in different runtime batches (and so
    // different cycles) depending on scheduler timing, which made
    // "simultaneous" multi-input epochs nondeterministic WITHIN a mode
    // — the first overnight soak recorded dozens of phantom pacing
    // divergences before an uncontended per-mode rerun caught the
    // driver red-handed.
    let mut refs: AHashMap<&str, graphix_rt::Ref<NoExt>> = AHashMap::new();
    for (name, _, _) in sched.inputs() {
        let scope = graphix_compiler::Scope::root();
        let path = graphix_compiler::expr::ModPath::from([name.as_str()]);
        let r = bounded!(
            ctx.rt.compile_ref_by_name(&compiled.env, &scope, &path),
            Ok(r) => r,
            Err(e) => return Outcome::RuntimeErr(format!("input {name}: {e}"))
        );
        // Keys borrow from `sched` (alive for the whole drive), not
        // from the transient `inputs()` Vec.
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
            ep.iter().map(|(name, v)| (refs[name.as_str()].bid, v.clone())).collect();
        if let Err(e) = ctx.rt.set_many(sets) {
            return Outcome::RuntimeErr(format!("set_many: {e}"));
        }
        segs.push(wait_settled!());
    }
    // Dispatch epochs (callable-v1). Both routes append one traced
    // epoch per dispatch AFTER the schedule's own, so the two routes'
    // traces align epoch-for-epoch.
    if let Some(c) = spec {
        match route {
            Route::InLanguage => {
                let mut arefs: AHashMap<String, graphix_rt::Ref<NoExt>> = AHashMap::new();
                for (name, _, _) in c.args() {
                    let scope = graphix_compiler::Scope::root();
                    let path = graphix_compiler::expr::ModPath::from([name.as_str()]);
                    let r = bounded!(
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
                        .map(|(name, v)| (arefs[name.as_str()].bid, v.clone()))
                        .collect();
                    if let Err(e) = ctx.rt.set_many(sets) {
                        return Outcome::RuntimeErr(format!("set_many: {e}"));
                    }
                    segs.push(wait_settled!());
                }
            }
            Route::Dispatch => {
                let scope = graphix_compiler::Scope::root();
                let path = graphix_compiler::expr::ModPath::from(c.handler.split("::"));
                let r = bounded!(
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
                let cb = bounded!(
                    ctx.rt.compile_callable(lambda),
                    Ok(cb) => cb,
                    Err(e) => {
                        return Outcome::RuntimeErr(format!("compile_callable: {e}"));
                    }
                );
                // The embedder timeline has CYCLES between building
                // the callable and the first dispatch (a UI renders
                // before the first key) — and the lazy-instance bug
                // geometry needs them: dispatched back-to-back, a
                // reference delivered at the callable's init can still
                // reach the instance and a resolution bug never
                // fires (the ConnectDeref pin needed the same gap).
                for _ in 0..3 {
                    bounded!(
                        ctx.rt.compile(ArcStr::from("i64:0")),
                        Ok(_) => (),
                        Err(e) => {
                            return Outcome::RuntimeErr(format!("gap compile: {e}"));
                        }
                    );
                }
                for ep in &c.epochs {
                    let args =
                        ValArray::from_iter_exact(ep.iter().map(|(_, v)| v.clone()));
                    bounded!(
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
    Outcome::Trace(trace::Trace::from_segments(&segs, eid))
}

/// The reserved metamorphic-twin poison tag: a generated twin program
/// compares its equivalent write routes in-program and settles any
/// epoch on a `` `TwinDiverged(..) `` value when they disagree. The
/// scan walks each epoch's FINAL value (transient mid-epoch skew while
/// writes land is not a violation; the settled value is), through
/// composites. The tag is reserved by the generator contract — no
/// other program may produce it.
pub const TWIN_TAG: &str = "TwinDiverged";

fn value_has_tag(v: &Value, tag: &str) -> bool {
    match v {
        Value::String(s) => &**s == tag,
        Value::Array(a) => a.iter().any(|v| value_has_tag(v, tag)),
        Value::Error(e) => value_has_tag(e, tag),
        Value::Map(m) => {
            m.into_iter().any(|(k, v)| value_has_tag(k, tag) || value_has_tag(v, tag))
        }
        _ => false,
    }
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
/// its TEXT — deterministic, so every protocol (campaign / minimize /
/// regress / selfcheck) recomputes the same tier and findings need no
/// strength tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OracleTier {
    /// Pure/deterministic programs: exact per-cycle trace agreement.
    Exact,
    /// Value-deterministic ASYNC: IO pacing races trace quiescence (the
    /// same mode paces differently run to run), but each epoch's
    /// SETTLED value at quiescence is deterministic — compare per-epoch
    /// finals (Eric's ruling 2026-07-06: "we can verify that the final
    /// output is the same, we just can't verify they are cycle
    /// identical").
    FinalValues,
    /// Value-NONDETERMINISTIC: rand varies values, wall time and
    /// tempdir paths leak into results — no value comparison is sound
    /// at any strength. The shapes still run (crashes surface via
    /// child death); divergences never record.
    Excluded,
}

/// Classify a program. Markers are matched on the whole wrapper text.
/// The Excluded list is EMPIRICAL — selfcheck polices it: a marker
/// missing from it shows up as a final-strength flake (that is how
/// `sys::net` earned its place: `list` returns a registration-timing
/// snapshot, and a subscribe racing its publish takes the error arm in
/// some runs — value-visible races even at quiescence. Promoting
/// netidx needs sequenced publish/subscribe contracts in the harness —
/// the dynamic-modules work).
pub fn oracle_tier(code: &str) -> OracleTier {
    // Markers match CODE ONLY — comment lines are stripped first. The
    // scan used to cover the whole wrapper text, so a finding header
    // that merely NAMED an excluded API silently un-gated its own pin
    // (`check` reported AGREE on live divergences; three corpus pins
    // were affected — oracle-tier-comment-scan-aug2026). Trailing
    // comments on code lines are kept: over-exclusion is the safe
    // direction.
    let mut stripped = String::with_capacity(code.len());
    for l in code.lines() {
        if !l.trim_start().starts_with("//") {
            stripped.push_str(l);
            stripped.push('\n');
        }
    }
    let code = stripped.as_str();
    // Value-nondeterministic sources: random values, wall-clock time
    // (timers deliver it, `now()` returns it), generated temp paths,
    // netidx registration timing, OS-assigned socket addresses
    // (generated programs bind port 0, so the addr getters read back
    // an ephemeral port — the same environmental-value leak as a
    // tempdir path; soak jul08d found `local_addr(s)? <= addr` as a
    // coin flip that passed the interp-self-agreement filter), signal
    // delivery racing child execution (`kill` of a TERM-trapping shell
    // flips `status.success` on whether TERM lands before or after the
    // trap installs — soak jul23f found it under lane load only; the
    // class hid behind `sys::time` because every kill fixture paired
    // with a timer trigger until a mutant swapped the trigger out),
    // and OS-assigned pids (same class as the addr getters).
    // `throttle` is the one wall-clock reader OUTSIDE sys:: (core
    // lib.rs:1856 — `Instant::now() - last >= wait` decides whether a
    // delivery passes or is deferred to a timer). It was listed only as
    // fire-count-sensitive, which is checked inside the sys:: arm, so a
    // throttle program with no sys:: fell through to Exact — soak aug14e
    // hz0 divergence_000000 recorded `count(throttle(#rate:
    // duration:0.001s, x))` over a 3-element iter, where a rate near the
    // cycle time makes each pass a coin flip. Same-engine reruns flip
    // both the event COUNT and its EPOCH, and the program fuses nothing
    // at all (both sides run the same node-walk), so it is
    // nondeterminism, not a divergence. Same class as `sys::time` —
    // hence top-level, not fire-count-conditional.
    let excluded = [
        "rand::",
        "throttle",
        "sys::time",
        "sys::net",
        "sys::process::kill",
        "sys::process::pid",
        "tempdir",
        "listener_addr",
        "local_addr",
        "peer_addr",
    ];
    if excluded.iter().any(|m| code.contains(m)) {
        return OracleTier::Excluded;
    }
    // An fs MUTATION racing an fs OBSERVATION: nothing orders two
    // async IO builtins unless the program threads a data edge between
    // them, and mutation freely severs that edge (soak jul23f
    // divergence_000001: the seed fixture reads back a written file
    // with the read triggered off the write's result; the mutant
    // swapped the trigger for a constant, so the read races the
    // write's create+truncate — interp saw the content, jit the
    // just-truncated empty file; flips run-to-run under lane load).
    // One-sided fs programs (only mutators or only observers) keep
    // their value comparison.
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
    if ["sys::", "http::"].iter().any(|m| code.contains(m)) {
        // FinalValues assumes the async values themselves settle
        // deterministically. A `<-` weaves async ARRIVAL ORDER into
        // reactive state (soak jul08i divergence_000000: a `?`-consumed
        // `sys::tcp::listen` element + a connected array + a
        // self-clocked `iterq` — the final value depends on whether the
        // listener lands before or after the walk; 8x re-check AGREEs
        // on an idle machine, the recorded miss needed soak load).
        // Fire-count-sensitive builtins are the `<-`-less version of
        // the same leak: their final value counts/folds FIRES, and how
        // many times a node fires depends on where the async arrival
        // lands relative to other events (jul21a fuzz
        // divergence_000000: `count` of a guarded select whose
        // becoming-selected transitions straddle a `tcp::connect`
        // arrival — flaps 3/5 in EITHER mode; de-asynced it agrees
        // deterministically).
        // `hold(#clock, v)` is ARRIVAL-ORDER-sensitive rather than
        // fire-count-sensitive: its output is defined by where the
        // clock lands relative to v's deliveries, so an async clock
        // (`sys::io::stderr(null) ~ 1`) settles on whichever element
        // was held when the reply arrived — 300, 200, 200 across three
        // interp runs (aug24b hz0 divergence_000000).
        let fire_count_sensitive = [
            "count(", "sum(", "product(", "mean(", "min(", "max(", "all(", "and(", "or(",
            "queue(", "take(", "skip(", "window(", "iterq", "hold(",
        ];
        if code.contains("<-") || fire_count_sensitive.iter().any(|m| code.contains(m)) {
            return OracleTier::Excluded;
        }
        return OracleTier::FinalValues;
    }
    OracleTier::Exact
}

/// A detected disagreement between the reference (interp = node-walk)
/// and the system under test (jit = fusion + cranelift).
#[derive(Debug, Clone)]
pub struct Divergence {
    pub code: String,
    pub interp: Outcome,
    pub jit: Outcome,
    pub tier: OracleTier,
    pub pair: Pair,
}

/// Which two runs a [`Divergence`] compares. `Engine` is the classic
/// node-walk-vs-JIT check; the other two exist only for `callable-v1`
/// programs (see [`callable`]).
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
    /// A metamorphic twin program violated its own invariant: an
    /// in-program comparison of two equivalent write routes settled on
    /// a `` `TwinDiverged `` value. Detected within a SINGLE run —
    /// both fields hold the offending outcome — so it catches bugs
    /// that break every engine and route identically (the ConnectDeref
    /// silent-write class broke ALL FOUR runs the same way; every
    /// pairwise comparison agreed on the wrong answer).
    Twin,
}

impl Divergence {
    /// A one-line classification. With only two evaluators left, every
    /// divergence is "node-walk vs fused+JIT" — the fusion path (CLIF
    /// emit + cranelift codegen, which can't be told apart now that
    /// there's no interpreter-only mode to bisect against) produced a
    /// different result from the canonical node-walk.
    pub fn bisect(&self) -> &'static str {
        match (&self.interp, &self.jit) {
            // Survived check()'s 8x interp retry: either the JIT
            // fabricated a value the reference never produces (a real
            // bug), or the node-walk is >8x-budget slower on a heavy
            // terminating program (the accepted perf gap — soak jul06g,
            // jul08d fib(33)). Verify by hand: run the node-walk
            // unbudgeted and compare its value.
            (Outcome::Timeout, Outcome::Trace(t))
                if t.epochs.iter().any(|e| !e.events.is_empty()) =>
            {
                "asymmetric timeout (interp exceeded 8x budget; JIT produced a value — \
                 verify the node-walk terminates and agrees before reading this as a JIT bug)"
            }
            _ => match (self.pair, self.tier) {
                (Pair::Twin, _) => {
                    "twin invariant violated (equivalent write routes diverged \
                     in-program — a single-run finding, no cross-run comparison)"
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
            },
        }
    }

    /// Human labels for the two outcome fields, by pair.
    pub fn labels(&self) -> (&'static str, &'static str) {
        match self.pair {
            Pair::Engine => ("interp", "jit"),
            Pair::EngineDispatch => ("interp/dispatch", "jit/dispatch"),
            Pair::Route => ("in-language", "dispatch"),
            Pair::Twin => ("trace", "trace"),
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
/// seed, and a nondeterminism-cleared agreement is worse).
pub async fn check_classified(
    code: &str,
    timeout: Duration,
) -> (Option<Divergence>, bool) {
    let tier = oracle_tier(code);
    if callable::has_header(code) {
        return check_callable(code, tier, timeout).await;
    }
    // The two evaluators must agree, or it's a divergence. Each mode
    // spins up its own runtime, so run them concurrently — `join!`
    // overlaps their (mostly I/O-bound) execution on one task.
    let (interp, jit) = tokio::join!(
        run_program(code, Mode::Interp, timeout),
        run_program(code, Mode::Jit, timeout),
    );
    if tier == OracleTier::Excluded {
        return (None, false);
    }
    // A metamorphic twin violation is a SINGLE-RUN finding — checked
    // before agreement, which is the whole point: a bug breaking both
    // engines identically AGREES on the wrong answer (the ConnectDeref
    // silent-write class did). Confirmed by one rerun.
    for (o, mode) in [(&interp, Mode::Interp), (&jit, Mode::Jit)] {
        if twin_violation(o) {
            let again = run_program(code, mode, timeout).await;
            if twin_violation(&again) {
                let d = Divergence {
                    code: code.to_string(),
                    interp: o.clone(),
                    jit: o.clone(),
                    tier,
                    pair: Pair::Twin,
                };
                return (Some(d), false);
            }
        }
    }
    if interp.agrees_with_at(&jit, tier) {
        let ran =
            matches!(&interp, Outcome::Trace(_)) && matches!(&jit, Outcome::Trace(_));
        return (None, ran);
    }
    // Reference-side Timeout with a VALUE-BEARING jit trace: as likely
    // "the node-walk is orders of magnitude slower than native on a
    // heavy but terminating program" (the quasi-polynomial
    // `fib(n-1)/fib(n/2)` tree finished instantly under the JIT and
    // wedged the interp — soak jul06g) as a wrongly-terminating JIT.
    // Escalate: retry interp once at 8x the budget (fits the isolated
    // child's outer deadline of timeout*4+30s); completion + agreement
    // clears it. Still-Timeout keeps the finding — a JIT that
    // fabricates a value for a program the reference can't finish
    // deserves eyes. (An EMPTY jit trace against a Timeout is the
    // both-non-productive class, already agreed above.)
    if matches!(&interp, Outcome::Timeout)
        && matches!(&jit, Outcome::Trace(t) if t.epochs.iter().any(|e| !e.events.is_empty()))
    {
        // Absolute floor: the scale gap is unbounded (a 500k
        // `array::init` is ~10s of node-walk CPU vs instant native, and
        // soak load stretches CPU seconds into wall minutes — jul17a
        // divergence_000000), so a pure multiple of a small lane budget
        // under-escalates exactly when the machine is busiest.
        let slow_budget = (timeout * 8).max(Duration::from_secs(60));
        let cpu_before = self_cpu();
        let slow = run_program(code, Mode::Interp, slow_budget).await;
        if slow.agrees_with_at(&jit, tier) {
            return (None, matches!(&slow, Outcome::Trace(_)));
        }
        // Still over budget. CPU burn discriminates the two ways that
        // happens (Eric's ruling 2026-08-17): a WEDGE sits at ~0% CPU —
        // its burn is bounded by startup + subject compile, an absolute
        // cost that cannot grow with the window — while HONEST SLOWNESS
        // burns whatever the scheduler gives it, seconds of CPU over a
        // >=60s window even at 10x worker oversubscription (the
        // per-element lazy-bind gap makes fold(init(500k)) ~1000s of
        // node-walk against milliseconds of native, and each literal
        // size minted a fresh corpus slot — aug17d katana
        // divergence_000000). Seconds of burn is proof of progress:
        // log it and drop it. A real wedge (symbolic.gx, fixed
        // 0f4f6573) cannot reach the ceiling and still records. The
        // delta is process-wide, so it is only meaningful where nothing
        // else computes during the retry — true in the isolated check
        // child and the sequential regression walk, the paths that
        // matter; a concurrent non-isolated pool can only over-count,
        // which errs toward dropping timeout noise, never toward
        // recording it. The symmetric jit-timeout branch below gets no
        // such test: a wedged kernel SPINS, so burn cannot tell it
        // from a starved-but-progressing child there.
        if matches!(&slow, Outcome::Timeout) {
            let burned = self_cpu().saturating_sub(cpu_before);
            if burned >= Duration::from_secs(5) {
                eprintln!(
                    "SLOW — interp burned {:.1}s CPU over a {:.0}s budget without \
                     finishing; jit's value stands unrefuted; honest slowness, \
                     not recorded",
                    burned.as_secs_f64(),
                    slow_budget.as_secs_f64()
                );
                eprintln!("    program: {}", code.replace('\n', "\\n"));
                return (None, false);
            }
        }
    }
    // The symmetric direction — jit Timeout against a value-bearing
    // interp trace — is as likely a STARVED jit child (both modes run
    // concurrently; under full soak load the wall clock stretches past
    // the lane budget — jul17a generate/divergence_000000, which
    // AGREEd 6/6 on an idle machine on both binaries) as a genuine
    // native hang. Same escalation: a real wedged kernel (an infinite
    // pure tail loop can't yield) still times out at the bigger budget
    // and keeps the finding.
    if matches!(&jit, Outcome::Timeout)
        && matches!(&interp, Outcome::Trace(t) if t.epochs.iter().any(|e| !e.events.is_empty()))
    {
        let slow_budget = (timeout * 8).max(Duration::from_secs(60));
        let slow = run_program(code, Mode::Jit, slow_budget).await;
        if interp.agrees_with_at(&slow, tier) {
            return (None, matches!(&slow, Outcome::Trace(_)));
        }
    }
    // Suspected divergence — but first rule out nondeterminism: a value
    // whose identity/Display isn't deterministic (a lambda or abstract
    // value's id, a leaked environmental value the tier markers missed)
    // would diverge between any two runs, not just across backends.
    // Re-run interp AT THE SAME TIER; if it disagrees with itself, the
    // program is nondeterministic there, not a backend bug.
    let interp2 = run_program(code, Mode::Interp, timeout).await;
    if !interp.agrees_with_at(&interp2, tier) {
        return (None, false);
    }
    (
        Some(Divergence {
            code: code.to_string(),
            interp,
            jit,
            tier,
            pair: Pair::Engine,
        }),
        false,
    )
}

/// The callable-v1 check matrix: four runs (two engines x two
/// routes), three comparisons — each route's engine pair at the
/// program's tier, then the ROUTE pair (node-walk engine) at
/// final-values strength (route pacing is not contractual; the
/// per-epoch settled value is). Records the first divergence in that
/// order. A timeout-involved disagreement retries once at 4x and
/// drops with a log if unresolved — the full escalation ladder is an
/// engine-pair tool; noise here errs toward dropping. Every recorded
/// divergence survives a same-pair rerun (the nondeterminism guard).
/// `ran` is always false: callable programs stay out of the mutation
/// ring until mutation learns to preserve their header (the AST
/// round-trip drops comments).
async fn check_callable(
    code: &str,
    tier: OracleTier,
    timeout: Duration,
) -> (Option<Divergence>, bool) {
    let (ia, ja, ib, jb) = tokio::join!(
        run_program_routed(code, Mode::Interp, Route::InLanguage, timeout),
        run_program_routed(code, Mode::Jit, Route::InLanguage, timeout),
        run_program_routed(code, Mode::Interp, Route::Dispatch, timeout),
        run_program_routed(code, Mode::Jit, Route::Dispatch, timeout),
    );
    if tier == OracleTier::Excluded {
        return (None, false);
    }
    // Twin violations first — single-run findings (see the engine-pair
    // check's comment), scanned on every (mode, route) run.
    for (o, mode, route) in [
        (&ia, Mode::Interp, Route::InLanguage),
        (&ja, Mode::Jit, Route::InLanguage),
        (&ib, Mode::Interp, Route::Dispatch),
        (&jb, Mode::Jit, Route::Dispatch),
    ] {
        if twin_violation(o) {
            let again = run_program_routed(code, mode, route, timeout).await;
            if twin_violation(&again) {
                let d = Divergence {
                    code: code.to_string(),
                    interp: o.clone(),
                    jit: o.clone(),
                    tier,
                    pair: Pair::Twin,
                };
                return (Some(d), false);
            }
        }
    }
    async fn settle<F: Fn(&Outcome, &Outcome) -> bool>(
        code: &str,
        m1: Mode,
        r1: Route,
        m2: Mode,
        r2: Route,
        a: Outcome,
        b: Outcome,
        agrees: F,
        timeout: Duration,
    ) -> Option<(Outcome, Outcome)> {
        if agrees(&a, &b) {
            return None;
        }
        if matches!(a, Outcome::Timeout) || matches!(b, Outcome::Timeout) {
            let big = (timeout * 4).max(Duration::from_secs(60));
            let a2 = run_program_routed(code, m1, r1, big).await;
            let b2 = run_program_routed(code, m2, r2, big).await;
            if agrees(&a2, &b2) {
                return None;
            }
            if matches!(a2, Outcome::Timeout) || matches!(b2, Outcome::Timeout) {
                eprintln!(
                    "callable check: timeout-involved disagreement at 4x — dropped"
                );
                return None;
            }
            let a3 = run_program_routed(code, m1, r1, big).await;
            if !agrees(&a2, &a3) {
                return None;
            }
            let b3 = run_program_routed(code, m2, r2, big).await;
            if !agrees(&b2, &b3) {
                return None;
            }
            return Some((a2, b2));
        }
        // Nondeterminism guard: each side must agree with itself.
        let a2 = run_program_routed(code, m1, r1, timeout).await;
        if !agrees(&a, &a2) {
            return None;
        }
        let b2 = run_program_routed(code, m2, r2, timeout).await;
        if !agrees(&b, &b2) {
            return None;
        }
        Some((a, b))
    }
    fn route_agrees(a: &Outcome, b: &Outcome) -> bool {
        match (a, b) {
            (Outcome::Trace(x), Outcome::Trace(y)) => x.agrees_final(y),
            _ => a.agrees_with(b),
        }
    }
    let tier_cmp = |a: &Outcome, b: &Outcome| a.agrees_with_at(b, tier);
    // The dispatch route's cycle offsets are not comparable at Exact
    // strength: the gap compiles and callable dispatch take an engine-
    // and run-dependent number of cycles, so only per-epoch settled
    // values are contractual there (same rationale as the route pair).
    let finals_cmp =
        |a: &Outcome, b: &Outcome| a.agrees_with_at(b, OracleTier::FinalValues);
    if let Some((a, b)) = settle(
        code,
        Mode::Interp,
        Route::InLanguage,
        Mode::Jit,
        Route::InLanguage,
        ia.clone(),
        ja,
        tier_cmp,
        timeout,
    )
    .await
    {
        let d = Divergence {
            code: code.to_string(),
            interp: a,
            jit: b,
            tier,
            pair: Pair::Engine,
        };
        return (Some(d), false);
    }
    if let Some((a, b)) = settle(
        code,
        Mode::Interp,
        Route::Dispatch,
        Mode::Jit,
        Route::Dispatch,
        ib.clone(),
        jb,
        finals_cmp,
        timeout,
    )
    .await
    {
        let d = Divergence {
            code: code.to_string(),
            interp: a,
            jit: b,
            tier,
            pair: Pair::EngineDispatch,
        };
        return (Some(d), false);
    }
    if let Some((a, b)) = settle(
        code,
        Mode::Interp,
        Route::InLanguage,
        Mode::Interp,
        Route::Dispatch,
        ia,
        ib,
        route_agrees,
        timeout,
    )
    .await
    {
        let d = Divergence {
            code: code.to_string(),
            interp: a,
            jit: b,
            tier,
            pair: Pair::Route,
        };
        return (Some(d), false);
    }
    (None, false)
}

/// A module resolver whose target can be swapped between compiles —
/// the batch child's bridge between ONE warmed runtime (stdlib
/// compiled once at init) and a fresh per-subject module source.
/// `resolve` delegates to whatever resolver was `set` last; the
/// runtime holds this wrapper for its whole life, so each subject's
/// unique `/t<i>.gx` becomes visible without re-initing the context.
#[derive(Debug)]
struct SwapResolver(std::sync::Mutex<graphix_compiler::expr::ResolverRef>);

impl SwapResolver {
    fn arc(initial: graphix_compiler::expr::ResolverRef) -> std::sync::Arc<Self> {
        std::sync::Arc::new(SwapResolver(std::sync::Mutex::new(initial)))
    }

    fn set(&self, r: graphix_compiler::expr::ResolverRef) {
        *self.0.lock().unwrap() = r;
    }
}

impl graphix_compiler::expr::ModuleResolver for SwapResolver {
    fn resolve<'a>(
        &'a self,
        scope: &'a graphix_compiler::expr::ModPath,
        parent: &'a triomphe::Arc<graphix_compiler::expr::Origin>,
        name: &'a Path,
        errors: &'a mut Vec<anyhow::Error>,
    ) -> std::pin::Pin<
        Box<
            dyn std::future::Future<Output = graphix_compiler::expr::Resolution>
                + Send
                + Sync
                + 'a,
        >,
    > {
        Box::pin(async move {
            // Clone the target out and DROP the guard before awaiting.
            let r = self.0.lock().unwrap().clone();
            r.resolve(scope, parent, name, errors).await
        })
    }
}

/// Per-subject verdict from a batch child. Only AGREEMENT is ever
/// trusted from a batch — anything else re-runs through the individual
/// `check_isolated` gold path, so every FINDING still derives from a
/// fresh single-subject process (batches only fast-path the ~100%
/// agree case, amortizing the per-subject stdlib compile).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BatchVerdict {
    Agree {
        /// Both outcomes were runtime traces — the ring-admission bar.
        ran: bool,
    },
    /// Not a clean agreement (divergence-shaped, timeout, wedged
    /// runtime, ineligible subject) — the parent re-runs individually.
    Other,
}

/// The `check-batch` child body: run `progs` sequentially against ONE
/// warmed runtime pair — the stdlib compiles once per mode instead of
/// once per subject, which is the fleet-throughput constant (the
/// actual-soak profile: a trivial subject and a real one both cost the
/// same ~80ms child, ~all of it stdlib typecheck). `report` is called
/// after each subject so a mid-batch death leaves the completed prefix
/// on record. A Timeout/RuntimeErr in either mode leaves the shared
/// runtime SUSPECT: that subject reports `Other` and the batch stops —
/// unreported subjects fall back to individual runs in the parent.
pub async fn run_batch(
    progs: &[String],
    timeout: Duration,
    mut report: impl FnMut(usize, BatchVerdict),
) {
    let (tx_i, mut rx_i) = mpsc::channel(64);
    let (tx_j, mut rx_j) = mpsc::channel(64);
    let empty = || VfsResolver::new(AHashMap::new());
    let swap_i = SwapResolver::arc(empty());
    let swap_j = SwapResolver::arc(empty());
    let ctx_i = match init_with_flags_and_setup(
        tx_i,
        REGISTER,
        vec![swap_i.clone()],
        Mode::Interp.flags(),
        |_| {},
    )
    .await
    {
        Ok(c) => c,
        Err(_) => return,
    };
    let ctx_j = match init_with_flags_and_setup(
        tx_j,
        REGISTER,
        vec![swap_j.clone()],
        Mode::Jit.flags(),
        |_| {},
    )
    .await
    {
        Ok(c) => c,
        Err(_) => return,
    };
    let mut consecutive_poison = 0u32;
    for (i, code) in progs.iter().enumerate() {
        // Subject-unique module name: fresh module, fresh BindIds,
        // fresh everything — no cache aliasing between subjects. The
        // previous subject's graph was deleted when its `drive`
        // returned (`CompRes` handles delete their exprs on drop, and
        // ToGX messages are FIFO, so the delete lands before this
        // subject's TraceStart).
        //
        // Aux files ride in the subject's OWN table. They used to be
        // excluded because a batch child reuses one warmed runtime, so
        // same-named modules could alias — but a third of generated
        // programs carry files, and each exclusion cost a dedicated
        // process plus a full stdlib compile for ONE subject. Aliasing
        // would be invisible to the differential oracle (both engines
        // inherit the same wrong module and agree), so it is pinned by
        // `batched_files_do_not_alias`, which checks something other
        // than agreement.
        let subj = match Subject::parse(code, &format!("t{i}")) {
            Ok(s) => s,
            Err(_) => {
                report(i, BatchVerdict::Other);
                continue;
            }
        };
        let tier = subj.tier;
        swap_i.set(VfsResolver::new(subj.table.clone()));
        swap_j.set(VfsResolver::new(subj.table.clone()));
        // The subject's OWN tier drives both runs — FinalValues gets
        // its settle grace rounds exactly as the individual path does.
        let (interp, jit) = tokio::join!(
            drive(&ctx_i, &mut rx_i, &subj, Route::InLanguage, timeout),
            drive(&ctx_j, &mut rx_j, &subj, Route::InLanguage, timeout),
        );
        // A callable subject owes the route matrix: the point of the
        // mode is that the embedder-dispatch route agrees with the
        // in-language one, so an in-language-only agreement would skip
        // what the subject is FOR. The two routes share a runtime per
        // engine, so they run in sequence; only the engines overlap.
        //
        // These used to be held out of batching entirely, which put
        // every one of them — 15% of the reactive lane is twins, about
        // half of those callable — on the individual path at a
        // dedicated process and a full stdlib compile each. That was
        // the campaign's dominant expense (55 live individual children
        // against a design target of ~0).
        let routed = if subj.spec.is_some() {
            let (ib, jb) = tokio::join!(
                drive(&ctx_i, &mut rx_i, &subj, Route::Dispatch, timeout),
                drive(&ctx_j, &mut rx_j, &subj, Route::Dispatch, timeout),
            );
            Some((ib, jb))
        } else {
            None
        };
        let suspect =
            |o: &Outcome| matches!(o, Outcome::Timeout | Outcome::RuntimeErr(_));
        let routed_ref = routed.as_ref();
        let poisoned = suspect(&interp)
            || suspect(&jit)
            || routed_ref.is_some_and(|(a, b)| suspect(a) || suspect(b));
        // Excluded tier stops here, exactly as both individual paths
        // do: no value comparison is sound, so neither the twin scan
        // nor the route pair runs and the subject reports agreement
        // for having run at all — shape and crash coverage is what it
        // is for. Poison stays batch business whatever the tier says:
        // the shared runtime may be wedged.
        //
        // A metamorphic twin violation is a SINGLE-RUN finding — a bug
        // that breaks every engine and route IDENTICALLY agrees on the
        // wrong answer, so agreement cannot see it. Every twin carries
        // an aux file, so until this child emitted `mod` declarations
        // every batched twin compile-errored and reported a clean
        // agreement: the twin oracle was blind in the batch lane. A
        // violation goes back through the individual path, which
        // confirms it with a rerun before recording a finding.
        let comparable = tier != OracleTier::Excluded;
        let twin = comparable
            && (twin_violation(&interp)
                || twin_violation(&jit)
                || routed_ref
                    .is_some_and(|(a, b)| twin_violation(a) || twin_violation(b)));
        // The dispatch route's cycle offsets are not comparable at
        // Exact strength: the gap compiles and callable dispatch take
        // an engine- and run-dependent number of cycles, so only
        // per-epoch settled values are contractual there.
        let routes_agree = !comparable
            || routed_ref.is_none_or(|(ib, jb)| {
                let finals = |a: &Outcome, b: &Outcome| {
                    a.agrees_with_at(b, OracleTier::FinalValues)
                };
                let route_agrees = |a: &Outcome, b: &Outcome| match (a, b) {
                    (Outcome::Trace(x), Outcome::Trace(y)) => x.agrees_final(y),
                    _ => a.agrees_with(b),
                };
                finals(ib, jb) && route_agrees(&interp, ib)
            });
        let agreed = !poisoned
            && !twin
            && routes_agree
            && (!comparable || interp.agrees_with_at(&jit, tier));
        let verdict = if agreed {
            // `ran` is the parent's RING-ADMISSION bar, so it mirrors
            // the individual path exactly: a callable subject is never
            // admitted (`check_callable` returns `ran: false` on every
            // path — the ring's mutation operators do not understand a
            // callable header, so a bred mutant of one is malformed),
            // and neither is an Excluded-tier subject, which ran only
            // for shape and crash coverage.
            let ran = comparable
                && subj.spec.is_none()
                && matches!(&interp, Outcome::Trace(_))
                && matches!(&jit, Outcome::Trace(_));
            BatchVerdict::Agree { ran }
        } else {
            BatchVerdict::Other
        };
        report(i, verdict);
        // A Timeout/RuntimeErr leaves the shared runtime SUSPECT, but
        // usually RECOVERED (interrupt is a one-shot flag the guards
        // consume; the interp being slow on a heavy subject is the
        // common case, a wedged evaluator the rare one). Aborting the
        // whole batch on every timeout made the poison blast radius
        // scale with K (the K=32/64 regression) and capped useful
        // batch sizes; instead PROBE the pair with a trivial subject
        // on a short budget — responsive → keep going, wedged → stop
        // (unreported subjects fall back to individual runs).
        if poisoned {
            // Three poisons in a row = the runtime answers probes but
            // can't finish real subjects anymore — stop feeding it.
            consecutive_poison += 1;
            if consecutive_poison >= 3 {
                break;
            }
            // Give the interrupted evaluator a beat to finish
            // unwinding before probing: an interrupt breaks the loop
            // COOPERATIVELY, so a deep settle (a runaway-recursion
            // subject) can legitimately churn for seconds after the
            // drive returns Timeout — a 2s probe declared a merely-
            // slow unwind dead and dumped a 1000-batch's whole tail
            // (subject 715 of the first K=1000 measurement AGREEs
            // individually).
            tokio::time::sleep(Duration::from_secs(1)).await;
            let probe =
                Subject::parse("i64:0", &format!("p{i}")).expect("trivial probe parses");
            swap_i.set(VfsResolver::new(probe.table.clone()));
            swap_j.set(VfsResolver::new(probe.table.clone()));
            let budget = Duration::from_secs(10);
            let (pi, pj) = tokio::join!(
                drive(&ctx_i, &mut rx_i, &probe, Route::InLanguage, budget),
                drive(&ctx_j, &mut rx_j, &probe, Route::InLanguage, budget),
            );
            let healthy =
                matches!(pi, Outcome::Trace(_)) && matches!(pj, Outcome::Trace(_));
            if !healthy {
                break;
            }
        } else {
            consecutive_poison = 0;
        }
    }
    let grace = Duration::from_secs(2);
    let _ = tokio::time::timeout(grace, ctx_i.shutdown()).await;
    let _ = tokio::time::timeout(grace, ctx_j.shutdown()).await;
}

/// Batch size for the campaign pool's batch children. 1 disables
/// batching (every subject gets its own child, the pre-batching
/// behavior). Default 64: the amortization curve is ~flat past here
/// (7.2ms/subject at 16 → 5.6 at 64) while batch-child spawn churn
/// drops 4x vs 16; larger starves the mutation ring's agreement
/// feedback (par×K subjects in flight bred from stale ring state —
/// K=1000 measured novel shapes down 19%) and coarsens pool
/// granularity in finite gates.
fn batch_size() -> usize {
    std::env::var("GRAPHIX_FUZZ_BATCH")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(64)
        .max(1)
}

/// Which generator a work order asks the child to run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    Fuzz,
    Generate,
    Reactive,
}

impl SourceKind {
    pub fn tag(&self) -> &'static str {
        match self {
            SourceKind::Fuzz => "fuzz",
            SourceKind::Generate => "generate",
            SourceKind::Reactive => "reactive",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "fuzz" => Some(SourceKind::Fuzz),
            "generate" => Some(SourceKind::Generate),
            "reactive" => Some(SourceKind::Reactive),
            _ => None,
        }
    }
}

/// What the parent asks a child to DO, instead of what to run.
///
/// The parent used to generate every subject and ship its text down a
/// pipe, which made its cost per SUBJECT — generate, classify, write —
/// and pinned its single dispatch task at one core while the box sat at
/// 70%. An order is a few hundred bytes and covers a whole batch, so the
/// parent's cost becomes per BATCH and per FINDING, both rare. Generation
/// happens where the cores are.
///
/// `ring` is a small SAMPLE of the mutation ring, not a snapshot: the
/// evolutionary walk needs ancestors to breed from, but shipping all 256
/// would reinvent the pipe traffic this exists to delete.
#[derive(Debug, Clone)]
pub struct WorkOrder {
    pub kind: SourceKind,
    pub seed: u64,
    pub count: usize,
    pub ring: Vec<String>,
}

impl WorkOrder {
    pub fn encode(&self) -> String {
        let mut out = format!(
            "{} {} {} {}\n",
            self.kind.tag(),
            self.seed,
            self.count,
            self.ring.len()
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
        Ok(WorkOrder { kind, seed, count, ring })
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
                Box::new(move || {
                    for _ in 0..8 {
                        let s = if !ring.is_empty() && rng.below(2) == 0 {
                            ring[rng.below(ring.len())].clone()
                        } else {
                            seeds[rng.below(seeds.len())].to_string()
                        };
                        if let Some(p) = mutate::mutate_wrapper(&s, &donors, &mut rng, 5)
                        {
                            return p;
                        }
                    }
                    seeds[rng.below(seeds.len())].to_string()
                })
            }
        }
    }
}

/// The `gen-batch` child body: generate the order's subjects, run them
/// against one warmed runtime pair, and report back only what the parent
/// cannot compute for itself.
///
/// The report is deliberately asymmetric. A subject that AGREES costs one
/// short line; its text never leaves the child, which is the entire point
/// — that text is 99.99% of the bytes and none of the information. Only a
/// divergence (the parent must re-derive and minimize it) or a
/// ring-novel shape (the parent owns the ring) sends its program back.
pub async fn run_work_order(
    order: &WorkOrder,
    timeout: Duration,
    out: &mut impl std::io::Write,
) {
    let mut next = order.generator();
    let progs: Vec<String> = (0..order.count).map(|_| next()).collect();
    // Ring admission is computed HERE: `shape_stats` parses the program,
    // and the child has it in hand already.
    let novel: Vec<Option<(u64, usize, bool)>> =
        progs.iter().map(|p| mutate::shape_stats(p)).collect();
    let mut interesting: Vec<usize> = Vec::new();
    run_batch(&progs, timeout, |i, v| {
        let tag = match v {
            BatchVerdict::Agree { ran: true } => "R",
            BatchVerdict::Agree { ran: false } => "A",
            BatchVerdict::Other => "O",
        };
        let _ = writeln!(out, "V {i} {tag}");
        if matches!(v, BatchVerdict::Other) {
            interesting.push(i);
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
    for i in interesting {
        let p = &progs[i];
        let _ = writeln!(out, "P {i} {}", p.len());
        let _ = out.write_all(p.as_bytes());
        let _ = writeln!(out);
    }
    let _ = writeln!(out, "CPU {}", self_cpu().as_micros());
    let _ = out.flush();
}

/// Run a batch of eligible programs through ONE `check-batch` child.
/// Verdicts come back through a FILE inside the parent-owned sandbox
/// (stdout is corruptible by the programs under test — the check-one
/// lesson). A missing or non-Agree verdict — and every subject, on a
/// child that died or exited unclean — falls back to the individual
/// [`check_isolated`] path: batches only ever fast-path agreement, so
/// every finding is still derived by a fresh single-subject process
/// with the full escalation/nondeterminism ladder.
async fn batch_isolated(
    progs: Vec<String>,
    timeout: Duration,
) -> (Vec<(String, PoolResult)>, Duration) {
    let n = progs.len();
    let mut resolved: Vec<Option<PoolResult>> = (0..n).map(|_| None).collect();
    let mut individual: Vec<usize> = Vec::new();
    let mut remaining: Vec<usize> = (0..n).collect();
    // Every child this batch spends — re-batched rounds and individual
    // fallbacks included — is charged to the source that asked for it.
    let mut cpu = Duration::ZERO;
    // RE-BATCH after a clean abort: a wedged subject (an infinite
    // reactive loop leaves the runtime never-idle — no probe budget
    // recovers it) is reported `O` before the child stops, so the
    // unreported tail rides a FRESH child at the cost of one more
    // stdlib init (~250ms) instead of a tail's worth of individual
    // re-runs — this is what makes large K safe. An UNCLEAN exit
    // (crash/stall-kill) still discards the round's verdicts and
    // sends everything remaining down the individual path: a crashed
    // child's memory was suspect for the whole round, and the crasher
    // needs individual derivation anyway. A clean round that reports
    // NOTHING (e.g. runtime init failure) also falls back — no
    // progress means no third try.
    loop {
        let batch: Vec<String> = remaining.iter().map(|&i| progs[i].clone()).collect();
        let (clean, verdicts, round_cpu) = run_batch_child(&batch, timeout).await;
        cpu += round_cpu;
        if !clean || verdicts.is_empty() {
            individual.extend(remaining.drain(..));
            break;
        }
        let mut still: Vec<usize> = Vec::new();
        for (pos, &orig) in remaining.iter().enumerate() {
            match verdicts.get(&pos) {
                Some(BatchVerdict::Agree { ran }) => {
                    resolved[orig] = Some(PoolResult::Agree { ran: *ran })
                }
                Some(BatchVerdict::Other) => individual.push(orig),
                None => still.push(orig),
            }
        }
        remaining = still;
        if remaining.is_empty() {
            break;
        }
    }
    for &i in &individual {
        let (res, one_cpu) = check_isolated(&progs[i], timeout).await;
        cpu += one_cpu;
        resolved[i] = Some(res);
    }
    let out = progs
        .into_iter()
        .zip(resolved)
        .map(|(prog, res)| {
            let res = res.unwrap_or(PoolResult::Agree { ran: false });
            (prog, res)
        })
        .collect();
    (out, cpu)
}

/// What one work order came back with. Everything here is per-BATCH or
/// per-FINDING; nothing scales with the number of agreeing subjects.
pub(crate) struct OrderResult {
    /// Subjects the child ran, and how many of those both engines ran.
    pub ran: usize,
    pub agreed_ran: usize,
    /// Programs the child could not resolve — the parent re-derives
    /// these through the individual path, which owns the escalation
    /// ladder and the minimizer.
    pub suspect: Vec<String>,
    /// (signature, program) the child judged ring-worthy.
    pub novel: Vec<(u64, String)>,
    pub cpu: Duration,
    pub clean: bool,
}

/// Issue ONE work order to a child and collect its summary.
async fn run_order_child(order: &WorkOrder, timeout: Duration) -> OrderResult {
    use tokio::io::AsyncWriteExt;
    let mut res = OrderResult {
        ran: 0,
        agreed_ran: 0,
        suspect: Vec::new(),
        novel: Vec::new(),
        cpu: Duration::ZERO,
        clean: false,
    };
    let mut cmd = tokio::process::Command::new(child_exe());
    let sandbox = sandbox_cwd(&mut cmd);
    let out_path = sandbox.path().join("order-out");
    cmd.arg("gen-batch")
        .arg(&out_path)
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .kill_on_drop(true);
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("FATAL fuzz harness: child spawn failed: {e}");
            std::process::exit(2)
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(order.encode().as_bytes()).await;
    }
    // Same progress-based deadline as a check batch: a healthy child
    // flushes a line per subject, so "the file stopped growing" is the
    // wedge signal.
    let stall = timeout * 4 + Duration::from_secs(90);
    let mut last_len = 0u64;
    res.clean = loop {
        tokio::select! {
            r = child.wait() => break matches!(r, Ok(s) if s.code() == Some(0)),
            _ = tokio::time::sleep(stall) => {
                let len = std::fs::metadata(&out_path).map(|m| m.len()).unwrap_or(0);
                if len == last_len {
                    let _ = child.kill().await;
                    break false;
                }
                last_len = len;
            }
        }
    };
    let Ok(text) = std::fs::read_to_string(&out_path) else { return res };
    let mut rest = text.as_str();
    while let Some((line, tail)) = rest.split_once('\n') {
        rest = tail;
        let mut it = line.split_whitespace();
        match it.next() {
            Some("V") => {
                res.ran += 1;
                if it.nth(1) == Some("R") {
                    res.agreed_ran += 1;
                }
            }
            Some("N") | Some("P") => {
                let kind = &line[..1];
                let sig: u64 = it.next().and_then(|v| v.parse().ok()).unwrap_or(0);
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
                    res.novel.push((sig, prog));
                } else {
                    res.suspect.push(prog);
                }
            }
            Some("CPU") => {
                if let Some(us) = it.next().and_then(|v| v.parse::<u64>().ok()) {
                    res.cpu = Duration::from_micros(us);
                }
            }
            _ => (),
        }
    }
    res.cpu += child_cpu(sandbox.path());
    res
}

/// Spawn one `check-batch` child over `progs`, returning (clean-exit,
/// per-index verdicts). Verdicts come back through a FILE inside the
/// parent-owned sandbox (stdout is corruptible by the programs under
/// test — the check-one lesson), flushed per subject so a mid-batch
/// death leaves the completed prefix on record.
async fn run_batch_child(
    progs: &[String],
    timeout: Duration,
) -> (bool, AHashMap<usize, BatchVerdict>, Duration) {
    use tokio::io::AsyncWriteExt;
    let mut cmd = tokio::process::Command::new(child_exe());
    let sandbox = sandbox_cwd(&mut cmd);
    let verdict_path = sandbox.path().join("verdicts");
    cmd.arg("check-batch")
        .arg(&verdict_path)
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .kill_on_drop(true);
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("FATAL fuzz harness: child spawn failed: {e}");
            std::process::exit(2)
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        // Length-prefixed framing: programs are arbitrary text.
        let mut buf = format!("{}\n", progs.len());
        for p in progs {
            buf.push_str(&format!("{}\n", p.len()));
            buf.push_str(p);
        }
        let _ = stdin.write_all(buf.as_bytes()).await;
    }
    // PROGRESS-based deadline: a healthy batch child flushes a verdict
    // line per subject, so "the verdict file stopped growing" is the
    // wedge signal — a K-proportional wall would make large K (the
    // whole point: startup and fork/exec churn amortize toward zero)
    // either unsafe or absurdly slack. The stall budget covers the
    // one-time double stdlib init, the slowest legitimate subject
    // (one concurrent interp+jit pair at the lane budget — no
    // escalation ladder in the child), and the post-timeout health
    // probe.
    let stall = timeout * 4 + Duration::from_secs(90);
    let mut last_len = 0u64;
    let clean = loop {
        tokio::select! {
            r = child.wait() => break matches!(r, Ok(s) if s.code() == Some(0)),
            _ = tokio::time::sleep(stall) => {
                let len = std::fs::metadata(&verdict_path)
                    .map(|m| m.len())
                    .unwrap_or(0);
                if len == last_len {
                    let _ = child.kill().await;
                    break false;
                }
                last_len = len;
            }
        }
    };
    let mut verdicts: AHashMap<usize, BatchVerdict> = AHashMap::new();
    if let Ok(s) = std::fs::read_to_string(&verdict_path) {
        for line in s.lines() {
            let mut it = line.split_whitespace();
            if let (Some(i), Some(v)) = (it.next(), it.next())
                && let Ok(i) = i.parse::<usize>()
            {
                let v = match v {
                    "A" => BatchVerdict::Agree { ran: false },
                    "R" => BatchVerdict::Agree { ran: true },
                    _ => BatchVerdict::Other,
                };
                verdicts.insert(i, v);
            }
        }
    }
    (clean, verdicts, child_cpu(sandbox.path()))
}

/// Coarse "same bug" key: the bisection class + the interp/jit outcome
/// kinds + the trace-difference class (final-strength for final-tier
/// divergences — the exact TraceDiff is pacing-sensitive there and
/// would flake the minimizer's target bucket). Two divergences with the
/// same bucket are treated as the same bug — used by the minimizer to
/// ensure a reduction preserves the bug rather than reducing bug A into
/// a different bug B (e.g. morphing a missing-fire bug into a value bug).
fn bucket(d: &Divergence) -> (&'static str, u8, u8, Option<trace::TraceDiff>) {
    let td = match (&d.interp, &d.jit) {
        (Outcome::Trace(a), Outcome::Trace(b)) => match (d.pair, d.tier) {
            (Pair::Route, _) | (_, OracleTier::FinalValues) => {
                a.first_final_difference(b)
            }
            _ => a.first_difference(b),
        },
        _ => None,
    };
    (d.bisect(), d.interp.kind(), d.jit.kind(), td)
}

/// Minimize a diverging WRAPPER, schedule first, then hierarchical
/// delta-debugging on the body's typed AST. The header is split off
/// BEFORE `mutate::parse` and reattached around every candidate — the
/// AST round-trip drops comments, so parsing the raw wrapper would
/// silently strip the schedule and morph a reactive bug into a
/// single-burst one. Schedule reductions run first and cheapest-win:
/// drop the whole schedule, drop epochs (trailing, then each), drop
/// injections within an epoch, simplify literals toward 0/1. Caps stay
/// FIXED (they're the trace budgets both modes ran under; changing
/// them mid-minimize changes what "the same bug" means). Then the body
/// — and then each `.gx` section's own item sequence — shrinks by
/// [`shrink`], keeping any reduction that still parses and reproduces
/// the SAME divergence bucket. Returns the minimized wrapper and the
/// number of oracle checks spent (capped by `budget`). Accepts partial
/// minima.
pub async fn minimize(code: &str, timeout: Duration, budget: usize) -> (String, usize) {
    let d0 = match check(code, timeout).await {
        Some(d) => d,
        None => return (code.to_string(), 1),
    };
    let target = bucket(&d0);
    let Ok((mut sched, body)) = schedule::Schedule::parse(code) else {
        return (code.to_string(), 1);
    };
    let Ok((cspec, body_owned)) = callable::CallSpec::parse(body) else {
        return (code.to_string(), 1);
    };
    let Ok((body, mut files)) = files::split(&body_owned) else {
        return (code.to_string(), 1);
    };
    let mut current = match mutate::parse(body) {
        Some(e) => e,
        None => return (code.to_string(), 1),
    };
    // The callable header rides every candidate verbatim (dropping it
    // can only lose the route/dispatch machinery a callable finding
    // needs — the bucket check would reject the candidate anyway;
    // epoch-level callable reductions are future work).
    let reattach = |text: String| match &cspec {
        Some(c) => c.render(&text),
        None => text,
    };
    let mut calls = 1;
    // Phase 1 — schedule reductions.
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
    // Phase 2 — whole-section drops, then the body AST, then each
    // remaining `.gx` section's own item sequence, LAPPED until nothing
    // moves. The three feed each other: a whole-section drop only
    // succeeds once the body stops referring to it, a section's
    // internals only matter for a section that survives, and a body
    // reduction can strand a whole module. A section is an item
    // sequence, so it shrinks as a `Do` (`mutate::parse_items`) and
    // renders back bare; interfaces are left to the whole-drop pass
    // (`.gxi` is a different grammar).
    //
    // Each subject gets an equal slice of the remaining budget per lap:
    // a body that could absorb the entire budget must not starve the
    // sections, which are most of a multi-file finding's bytes once the
    // body is down.
    while calls < budget {
        let before = reattach(sched.render(&files::render(&current.to_string(), &files)));
        // Drop each module's section pair, then each interface alone
        // (no .gxi = everything public — the divergence often survives
        // the simpler layout).
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
    // Drops before replacements, each widest-first. Widest-first alone
    // spends a small budget on the least likely candidates: collapsing
    // a near-root subtree to a literal has to typecheck in context and
    // almost never survives, while dropping a statement just removes
    // one — on generated programs that is where the yield is, and the
    // campaign's budget only stretches to the head of this list.
    out.sort_by_key(|o| (o.repl.is_some(), std::cmp::Reverse(o.end - o.start)));
    out
}

/// Hierarchical delta-debugging on one AST, to a fixpoint or the
/// budget. `build` renders a candidate to the full program text it must
/// be checked as (`None` if the candidate is malformed).
///
/// The round structure is what makes the budget go anywhere. A scan
/// tries every reduction and keeps EVERY one that works, then applies
/// them all at once; the old reducer restarted the scan on the first
/// success, so a big program spent its whole budget re-testing its
/// head. Reductions are tried widest-extent-first and anything inside
/// an accepted extent is skipped, so the accepted set is pairwise
/// disjoint by construction — and since each was verified alone, the
/// composite almost always holds. When it doesn't, halving recovers a
/// prefix; a single op needs no re-check, so a round that found
/// anything always makes progress.
async fn shrink(
    mut current: Expr,
    build: &impl Fn(&Expr) -> Option<String>,
    target: &(&'static str, u8, u8, Option<trace::TraceDiff>),
    timeout: Duration,
    calls: &mut usize,
    budget: usize,
) -> Expr {
    // A candidate must render SHORTER than what it replaces, not merely
    // differently. Several reductions are identities on some nodes
    // (replacing `i64:0` by the constant `i64:0`, dropping a statement
    // from a node that isn't a block), and an identity reproduces the
    // divergence by definition — so without this the scan "succeeds"
    // forever on a program it never changes. Strictness also makes
    // termination obvious: every accepted round strictly shrinks.
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
        // Widest-first already, so halving keeps the best reductions.
        while kept.len() > 1 && !hits(&apply_ops(&current, &kept), cur, calls).await {
            kept.truncate(kept.len() / 2);
        }
        current = apply_ops(&current, &kept);
        // Progress to stderr: a big finding is minutes of silence
        // otherwise. The campaign's `minimize-one` child runs with
        // stderr null, so this is interactive-only by construction.
        eprintln!(
            "  minimize: {} checks, {} bytes",
            calls,
            build(&current).map_or(0, |t| t.len())
        );
    }
    current
}

/// The file-section shrink candidates for one greedy round, most
/// aggressive first: no sections at all, each module's section pair
/// dropped, each interface dropped alone. Section TEXT is never edited
/// (the body HDD can't reach it) — module internals only shrink by
/// whole-file drops, a deliberate v1 limit.
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
        // Drop everything (also resets caps to default — the whole
        // header disappears if the body alone reproduces).
        out.push(schedule::Schedule::default());
        // Drop the trailing epoch, then each single epoch.
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
        // Drop single injections (keeping each epoch non-empty).
        for (i, ep) in s.epochs.iter().enumerate() {
            if ep.len() > 1 {
                for j in 0..ep.len() {
                    let mut t = s.clone();
                    t.epochs[i].remove(j);
                    out.push(t);
                }
            }
        }
        // Simplify literals toward 0 then 1.
        for (i, ep) in s.epochs.iter().enumerate() {
            for (j, (_, v)) in ep.iter().enumerate() {
                let simpler: &[Value] = match v {
                    Value::I64(_) => &[Value::I64(0), Value::I64(1)],
                    Value::F64(_) => &[Value::F64(0.0), Value::F64(1.0)],
                    Value::Bool(_) => &[Value::Bool(false)],
                    _ => &[],
                };
                for sv in simpler {
                    if sv != v {
                        let mut t = s.clone();
                        t.epochs[i][j].1 = sv.clone();
                        out.push(t);
                    }
                }
            }
        }
    }
    out
}

/// Run the embedded regression corpus (every saved finding under
/// `findings/`) through the oracle. Returns any that now DIVERGE — a
/// regression, i.e. a previously-fixed bug has come back. Empty means the
/// corpus is clean. Uses a short per-program timeout: a regression
/// surfaces fast (crash / value mismatch), and a legitimately-bottom
/// program just confirms "still all-Timeout" quickly.
// ─── typemorph: metamorphic typecheck probes ───
// (typemorph.rs holds the transforms; design/typecheck_fuzzing.md P1)

/// Verdict of one acceptance check (`--check` semantics: compile +
/// typecheck + analyze, never execute).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TmVerdict {
    Accept,
    Reject(String),
    /// The check didn't finish inside its budget — a MEASUREMENT
    /// failure; no flip may be filed on it.
    Hung,
}

/// One subject's typemorph report: the base verdict, each probe's
/// verdict (probes run only when the base accepts), and the count of
/// candidates the printer failed to round-trip.
pub struct TmReport {
    pub base: TmVerdict,
    pub probes: Vec<(String, TmVerdict)>,
    pub noparse: usize,
}

impl TmReport {
    /// The line protocol the `typemorph-one` child writes to its
    /// verdict FILE (never stdout — the checked program can own the
    /// process streams).
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
        for (id, v) in &self.probes {
            match v {
                TmVerdict::Accept => {
                    let _ = writeln!(s, "PROBE {id} accept");
                }
                TmVerdict::Hung => {
                    let _ = writeln!(s, "PROBE {id} hung");
                }
                TmVerdict::Reject(e) => {
                    let _ = writeln!(s, "FLIP {id} {e}");
                }
            }
        }
        s
    }
}

/// The normalized HEAD of a rejection: the innermost cause (the last
/// non-empty line of the anyhow Debug chain) with digit runs collapsed
/// so positions and fresh-counter ids don't split dedup buckets.
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

/// Run one subject's metamorphic probes against a single warmed
/// runtime. Each acceptance check goes through
/// `GXHandle::check_with_resolvers`: a check never executes (its
/// compiled nodes are deleted) and the env is restored per call, so
/// one runtime checks the base and every candidate hermetically — the
/// stdlib init is paid once, not per probe. Probes run only when the
/// base ACCEPTS: the transforms are acceptance-preserving, so
/// accept→reject is the finding; a rejected base has nothing to
/// preserve.
pub async fn typemorph_subject(
    code: &str,
    per_check: Duration,
    cap: usize,
) -> Result<TmReport, String> {
    let (sched, body) = schedule::Schedule::parse(code)?;
    let (cspec, body) = callable::CallSpec::parse(body)?;
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
    let ctx =
        init_with_flags_and_setup(tx, REGISTER, vec![], Mode::Jit.flags(), move |ctx| {
            *ctx.libstate.get_or_default::<graphix_package_core::PrintSink>() = sink;
        })
        .await
        .map_err(|e| format!("runtime init failed: {e:?}"))?;
    async fn check_accept(ctx: &TestCtx, full: &str, per_check: Duration) -> TmVerdict {
        let subj = match Subject::parse(full, "test") {
            Ok(s) => s,
            Err(e) => return TmVerdict::Reject(tm_error_head(&e)),
        };
        let resolver = VfsResolver::new(subj.table.clone());
        let text = subj.compile_text();
        let fut = ctx.rt.check_with_resolvers(
            graphix_compiler::expr::Source::Internal(ArcStr::from(text)),
            vec![resolver.into()],
            None,
        );
        match tokio::time::timeout(per_check, fut).await {
            Err(_) => TmVerdict::Hung,
            Ok(Ok(_)) => TmVerdict::Accept,
            Ok(Err(e)) => TmVerdict::Reject(tm_error_head(&format!("{e:?}"))),
        }
    }
    let base = check_accept(&ctx, &compose(body), per_check).await;
    let mut results = Vec::new();
    if base == TmVerdict::Accept {
        for p in &probes {
            let v = check_accept(&ctx, &compose(&p.body), per_check).await;
            results.push((p.id(), v));
        }
    }
    let _ = tokio::time::timeout(Duration::from_secs(5), ctx.shutdown()).await;
    Ok(TmReport { base, probes: results, noparse })
}

/// Spawn the `typemorph-one` child on one subject and return its
/// verdict-file text. Mirrors `detcheck_one_pair`'s spawn discipline:
/// sandbox declared before the child handle, verdicts through a file
/// inside the sandbox, kill_on_drop under an outer deadline.
pub async fn typemorph_child(prog: &str, per_check: Duration) -> Result<String, String> {
    use std::process::Stdio;
    let mut cmd = tokio::process::Command::new(child_exe());
    let sandbox = sandbox_cwd(&mut cmd);
    let out_path = sandbox.path().join("tm-verdicts");
    cmd.arg("typemorph-one")
        .arg(&out_path)
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .kill_on_drop(true);
    let mut child = cmd.spawn().map_err(|e| format!("spawn: {e}"))?;
    {
        use tokio::io::AsyncWriteExt;
        let mut stdin = child.stdin.take().ok_or("no stdin")?;
        stdin.write_all(prog.as_bytes()).await.map_err(|e| format!("stdin: {e}"))?;
    }
    // base + 6 kinds × the per-kind cap, one warmed init, plus slack.
    let deadline = per_check * 25 + Duration::from_secs(60);
    match tokio::time::timeout(deadline, child.wait()).await {
        Err(_) => return Err("child deadline".into()),
        Ok(Err(e)) => return Err(format!("wait: {e}")),
        Ok(Ok(_)) => (),
    }
    std::fs::read_to_string(&out_path).map_err(|e| format!("verdicts: {e}"))
}

fn tm_flips(report: &str) -> Vec<(String, String)> {
    report
        .lines()
        .filter_map(|l| l.strip_prefix("FLIP "))
        .filter_map(|l| {
            l.split_once(' ').map(|(id, head)| (id.to_string(), head.to_string()))
        })
        .collect()
}

/// The scan: one child per program; a subject reporting flips is
/// CONFIRMED by a second fresh child (transforms are deterministic, so
/// the same probe id must flip again). An unconfirmed flip is reported
/// as its own class — the acceptance face of the jul22e determinism
/// family, never a TypeFlip.
pub async fn typemorph_scan(
    programs: Vec<(String, String)>,
    per_check: Duration,
) -> Vec<(String, String)> {
    use tokio::task::JoinSet;
    let par = (parallelism() / 2).max(1);
    let mut set: JoinSet<(usize, Result<String, String>)> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one =
        |set: &mut JoinSet<(usize, Result<String, String>)>, i: usize, prog: String| {
            set.spawn(async move { (i, typemorph_child(&prog, per_check).await) });
        };
    while next < programs.len() && set.len() < par {
        spawn_one(&mut set, next, programs[next].1.clone());
        next += 1;
    }
    let mut out = Vec::new();
    let mut noparse_total = 0usize;
    while let Some(res) = set.join_next().await {
        if let Ok((i, r)) = res {
            match r {
                Err(e) => out.push((programs[i].0.clone(), format!("harness: {e}"))),
                Ok(rep) => {
                    noparse_total += rep
                        .lines()
                        .find_map(|l| l.strip_prefix("NOPARSE "))
                        .and_then(|n| n.parse::<usize>().ok())
                        .unwrap_or(0);
                    let flips = tm_flips(&rep);
                    if !flips.is_empty() {
                        match typemorph_child(&programs[i].1, per_check).await {
                            Err(e) => out.push((
                                programs[i].0.clone(),
                                format!("harness (confirm): {e}"),
                            )),
                            Ok(rep2) => {
                                let again: std::collections::HashSet<String> =
                                    tm_flips(&rep2)
                                        .into_iter()
                                        .map(|(id, _)| id)
                                        .collect();
                                for (id, head) in flips {
                                    if again.contains(&id) {
                                        out.push((
                                            programs[i].0.clone(),
                                            format!("{id}: {head}"),
                                        ));
                                    } else {
                                        out.push((
                                            programs[i].0.clone(),
                                            format!(
                                                "{id}: UNCONFIRMED (fresh-process flap)"
                                            ),
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if next < programs.len() {
            spawn_one(&mut set, next, programs[next].1.clone());
            next += 1;
        }
    }
    if noparse_total > 0 {
        eprintln!("typemorph: {noparse_total} candidates failed print->parse round trip");
    }
    out
}

pub async fn run_regression(timeout: Duration) -> Vec<(String, Divergence)> {
    use tokio::task::JoinSet;
    let par = regress_parallelism();
    let entries = corpus::REGRESSION_CORPUS;
    let mut set: JoinSet<(usize, Option<Divergence>, bool)> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one = |set: &mut JoinSet<_>, i: usize| {
        let prog = entries[i].1.to_string();
        set.spawn(async move {
            let (d, ran) = check_classified(&prog, timeout).await;
            (i, d, ran)
        });
    };
    while next < entries.len() && set.len() < par {
        spawn_one(&mut set, next);
        next += 1;
    }
    let mut regressions = Vec::new();
    let mut suspect: Vec<usize> = Vec::new();
    while let Some(res) = set.join_next().await {
        if let Ok((i, d, ran)) = res {
            match d {
                Some(d) => regressions.push((entries[i].0.to_string(), d)),
                // A non-ran agreement from the PARALLEL pass is not
                // trusted: both-Timeout compares equal (correct for
                // the legitimately-bottom pins), so a pin whose
                // budget blew on BOTH modes under load silently
                // "passed" — the 2026-08-15 false-green, which
                // admitted a ruling-violating fold change past three
                // broken pins on consecutive runs, each run forgiving
                // a different load-starved subset. CompileErr-agree
                // pins land here too; their retry costs milliseconds.
                None if !ran => suspect.push(i),
                None => (),
            }
        }
        if next < entries.len() {
            spawn_one(&mut set, next);
            next += 1;
        }
    }
    // Retry the untrusted agreements SEQUENTIALLY at 4x budget: alone
    // on the box the load-starvation window is gone, so a still-quiet
    // pin is genuinely CompileErr/Timeout-shaped and passes on its
    // own character.
    if !suspect.is_empty() {
        eprintln!(
            "regress: retrying {} non-ran agreement(s) sequentially at full budget",
            suspect.len()
        );
        for i in suspect {
            let (d, _) = check_classified(entries[i].1, timeout * 4).await;
            if let Some(d) = d {
                regressions.push((entries[i].0.to_string(), d));
            }
        }
    }
    regressions
}

/// Number of programs in the embedded regression corpus.
pub fn regression_corpus_len() -> usize {
    corpus::REGRESSION_CORPUS.len()
}

/// The checked-in fusion-coverage manifest (fuzzer gap 6): one
/// `fused_count<TAB>name` line per corpus program, embedded at build.
/// `fusecheck` diffs live counts against it so a silent de-fusion
/// regression (a verifier error discarding a shared body, an
/// over-broad emission refusal) fails LOUD instead of quietly
/// node-walking — narrow-index-operand-verifier hid exactly this way,
/// and the value-cache storage law cost 13 fixtures before any signal
/// existed. `fusecheck --bless` rewrites the file (then rebuild to
/// embed).
pub static FUSECHECK_MANIFEST: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/fusecheck.manifest"));

/// Fused-region count per corpus program, in corpus order. Jit mode,
/// COMPILE only: fused counts are compile-time facts (fusion runs
/// inside `compile()`), so the program is never driven and the count
/// cannot depend on run-time pacing, load, or a drive timeout. A
/// count that could not be measured is `Err`, and the caller must
/// treat it as a gate failure — never as 0 (0 is a real measurement:
/// "nothing fused").
pub async fn run_fusecheck(timeout: Duration) -> Vec<(String, Result<u64, String>)> {
    use tokio::task::JoinSet;
    let par = parallelism();
    let entries = corpus::REGRESSION_CORPUS;
    let mut set: JoinSet<(usize, Result<u64, String>)> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one = |set: &mut JoinSet<_>, i: usize| {
        let prog = entries[i].1.to_string();
        set.spawn(async move {
            let r = match compile_with_stats(&prog, Mode::Jit, timeout).await {
                CompileOutcome::Compiled(s) | CompileOutcome::Rejected(_, s) => {
                    Ok(s.fused as u64)
                }
                CompileOutcome::Failed(e) => Err(e),
            };
            (i, r)
        });
    };
    while next < entries.len() && set.len() < par {
        spawn_one(&mut set, next);
        next += 1;
    }
    let mut counts: Vec<Option<Result<u64, String>>> = vec![None; entries.len()];
    while let Some(res) = set.join_next().await {
        if let Ok((i, c)) = res {
            counts[i] = Some(c);
        }
        if next < entries.len() {
            spawn_one(&mut set, next);
            next += 1;
        }
    }
    entries
        .iter()
        .zip(counts)
        .map(|((name, _), c)| {
            (name.to_string(), c.unwrap_or_else(|| Err("worker panicked".to_string())))
        })
        .collect()
}

/// The oracle-soundness gate: before any interp-vs-jit TRACE finding
/// is trusted, the trace must be shown deterministic PER MODE — same
/// program, same mode, twice → identical traces. Otherwise a flaky
/// trace (a host-timing-dependent cut point, a nondeterministic cycle
/// offset) would masquerade as a backend divergence. Runs every corpus
/// seed plus `iters` generated programs, each twice under interp and
/// twice under jit; returns the programs whose traces disagreed with
/// themselves, tagged with the mode that flaked. Must be empty.
pub async fn selfcheck(
    iters: usize,
    seed: u64,
    timeout: Duration,
) -> Vec<(String, &'static str)> {
    use tokio::task::JoinSet;
    // Subjects are everything with a SOUND comparison at some tier:
    // pure programs at exact strength, value-deterministic async at
    // final strength (`selfcheck_one` compares at the program's own
    // tier). Only tier-Excluded programs (rand / wall time / temp
    // paths — nondeterministic VALUES, unfixable by any relaxation)
    // are non-subjects; they exist as donor material for the mutator.
    // The gate's 100% bar now also polices the tier LIST itself: a
    // marker missing from `oracle_tier`'s Excluded set shows up here
    // as a final-strength flake.
    let deterministic = |p: &str| oracle_tier(p) != OracleTier::Excluded;
    let mut rng = mutate::Rng::new(seed);
    let mut progs: Vec<String> = corpus::all_seeds()
        .iter()
        .filter(|s| deterministic(s))
        .map(|s| s.to_string())
        .collect();
    // Half single-burst, half SCHEDULED reactive — the multi-epoch
    // injection driver is part of the oracle and must be just as
    // deterministic (skipping this is how the per-set delivery
    // nondeterminism shipped: single-burst selfcheck passed while
    // multi-input epochs wobbled ±1 cycle run-to-run).
    for i in 0..iters {
        if i % 2 == 0 {
            progs.push(generate::gen_program(&mut rng));
        } else {
            progs.push(generate::reactive::gen_reactive_program(&mut rng));
        }
    }
    let par = parallelism();
    let mut set: JoinSet<Vec<(String, &'static str)>> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one = |set: &mut JoinSet<Vec<(String, &'static str)>>, prog: String| {
        set.spawn(async move {
            selfcheck_isolated(&prog, timeout)
                .await
                .into_iter()
                .map(|mode| (prog.clone(), mode))
                .collect()
        });
    };
    while next < progs.len() && set.len() < par {
        spawn_one(&mut set, progs[next].clone());
        next += 1;
    }
    let mut flaky = Vec::new();
    let mut done = 0usize;
    let mut inconclusive = 0usize;
    while let Some(res) = set.join_next().await {
        if let Ok(mut bad) = res {
            // Budget-limited subjects are NOT findings: counted and
            // reported, never failed on (a Timeout compared against a
            // value measures the budget, not determinism).
            let n = bad.len();
            bad.retain(|(_, mode)| *mode != "inconclusive");
            inconclusive += n - bad.len();
            // Stream each finding as it lands — a killed or wedged run
            // must not take the collected list with it.
            for (prog, mode) in &bad {
                eprintln!("FLAKY under {mode}: {}", prog.replace('\n', "\\n"));
            }
            flaky.append(&mut bad);
        }
        done += 1;
        if done % 200 == 0 {
            eprintln!(
                "  …{done}/{} selfchecked, {} flaky, {inconclusive} inconclusive",
                progs.len(),
                flaky.len()
            );
        }
        if next < progs.len() {
            spawn_one(&mut set, progs[next].clone());
            next += 1;
        }
    }
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
    /// Ring admissions: agreeing, both-modes-ran mutants whose AST
    /// shape signature was NOVEL this campaign — the exploration
    /// metric (0 in generate lanes, which pass a no-op admitter).
    pub novel: usize,
}

/// A persistent, deduplicated divergence corpus on disk. Loaded once at
/// startup (so a campaign never re-reports a finding it already saved),
/// then grown live: each genuinely-new divergence is minimized, deduped
/// by its minimized text, and written to its own `.gx` immediately — not
/// at the end of the run, so a `forever` campaign surfaces findings as
/// they're found. Thread-safe: the worker pool records concurrently.
pub struct Corpus {
    dir: std::path::PathBuf,
    seen: std::sync::Mutex<std::collections::HashSet<String>>,
    counter: std::sync::atomic::AtomicUsize,
}

impl Corpus {
    /// Load every `*.gx` already in `dir`, keying the dedup set on each
    /// file's minimized program. Creates `dir` if absent.
    pub fn load(dir: &std::path::Path) -> Self {
        let _ = std::fs::create_dir_all(dir);
        let mut seen = std::collections::HashSet::new();
        let mut max_idx = 0usize;
        if let Ok(rd) = std::fs::read_dir(dir) {
            for ent in rd.flatten() {
                let path = ent.path();
                if path.extension().and_then(|e| e.to_str()) != Some("gx") {
                    continue;
                }
                if let Ok(body) = std::fs::read_to_string(&path) {
                    if let Some(m) = extract_minimized(&body) {
                        seen.insert(m);
                    } else if let Some((_, p)) = body.split_once("// mutant:\n") {
                        // Crash finding (no minimized form) — dedup by
                        // the same normalized key `record_crash` uses.
                        seen.insert(crash_key(p));
                    }
                }
                if let Some(n) = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .and_then(|s| {
                        s.strip_prefix("divergence_").or_else(|| s.strip_prefix("crash_"))
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
    pub fn record(&self, d: &Divergence, mutant: &str, minimized: &str) -> bool {
        let key = minimized.trim().to_string();
        {
            let mut seen = self.seen.lock().unwrap();
            if !seen.insert(key) {
                return false;
            }
        }
        let n = self.counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        // The mutant is COMMENT data — escape its newlines so a
        // multi-line mutant (a reactive schedule header + program) can't
        // land a bare program line mid-header. Un-escaped, the leading-
        // comment strip in `check <file>` / build.rs took the mutant's
        // body as the program and the real schedule became an interior
        // comment: the recorded finding re-checked as a vacuous
        // CompileErr==CompileErr AGREE (soak jul04 item 10).
        // The outcome header lines are for the human triager; a chatty
        // trace (a runaway that emitted thousands of events before its
        // cap) rendered a 5MB header over a 60-byte program (soak
        // jul05, divergence_000020). Clip — the full traces are
        // reproducible from the program text below.
        fn clip(s: String) -> String {
            const MAX: usize = 2048;
            if s.len() <= MAX {
                s
            } else {
                let mut i = MAX;
                while !s.is_char_boundary(i) {
                    i -= 1;
                }
                let mut c = s[..i].to_string();
                c.push_str(" …clipped");
                c
            }
        }
        let (la, lb) = d.labels();
        let body = format!(
            "// bisect: {}\n// {la}: {}\n// {lb}: {}\n\
             // mutant: {}\n// minimized:\n{}\n",
            d.bisect(),
            clip(format!("{:?}", d.interp)),
            clip(format!("{:?}", d.jit)),
            mutant.replace('\n', "\\n"),
            minimized,
        );
        // Persisting the finding is the campaign's whole point — a
        // write failure (full disk, dead mount) is a broken HARNESS.
        // Die loudly, as `check_isolated` does on spawn failure.
        if let Err(e) =
            std::fs::write(self.dir.join(format!("divergence_{n:06}.gx")), body)
        {
            eprintln!("FATAL fuzz harness: cannot write finding: {e}");
            std::process::exit(2);
        }
        true
    }

    /// Record a process-KILLING program (signal death / abort / hang of
    /// the isolated child). No minimized form — minimizing a crasher
    /// would crash the minimizer's in-process oracle — so the dedup key
    /// is the raw program text. Returns `true` when newly written.
    ///
    /// NOTE: crash findings must NOT be promoted to `findings/` (the
    /// embedded regression corpus runs IN-process — an unfixed crasher
    /// there kills the regress gate) until the underlying bug is fixed.
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
}

/// Extract the minimized program (the text after the `// minimized:`
/// marker) from a recorded divergence file, trimmed — the dedup key.
fn extract_minimized(body: &str) -> Option<String> {
    body.split_once("// minimized:\n").map(|(_, m)| m.trim().to_string())
}

/// The crash-dedup key: the program with every digit run collapsed to
/// one `N`. The mutator perturbs integer literals toward edge values,
/// so keying on raw text minted a fresh corpus slot per literal variant
/// of one crash SHAPE — the accepted `array::group(seq(lo, hi), …)`
/// runaway claimed four slots in one night (soak jul04 items 2/12-14).
/// Shared by `record_crash` and `Corpus::load` so restart dedup matches.
fn crash_key(prog: &str) -> String {
    let mut key = String::with_capacity(prog.len() + 6);
    key.push_str("CRASH:");
    let mut in_digits = false;
    let mut chars = prog.trim().chars().peekable();
    while let Some(c) = chars.next() {
        if c.is_ascii_digit()
            || (c == '-' && chars.peek().is_some_and(|n| n.is_ascii_digit()))
        {
            // A leading `-` folds into the digit run: `seq(-N, M)` and
            // `seq(N, M)` are the same crash shape, but keying the sign
            // separately doubled every runaway family (soak jul05
            // item 7). A `-` NOT followed by a digit (subtraction of a
            // variable, `->`) still keys literally. `x-1` keys the same
            // as `x - 1` — acceptable: dedup keys trade precision for
            // family collapse by design.
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
    // Unnamed: a single-source campaign keeps the pre-merge log format.
    let mut src = fuzz_source(seed, 1.0, gen_tasks());
    src.name = "";
    run_pool_multi(corpus, iters, timeout, vec![src])
        .await
        .pop()
        .map(|(_, stats, _)| stats)
        .unwrap_or_default()
}

/// Source A: mutate the curated seed corpus.
pub fn fuzz_source(seed: u64, weight: f64, tasks: usize) -> Source<'static> {
    let seeds = std::sync::Arc::new(corpus::all_seeds());
    let donors = std::sync::Arc::new(mutate::donor_pool(&seeds));
    // The evolutionary RING (Eric's design, 2026-07-23): agreeing
    // both-modes-ran mutants with a NOVEL AST shape join a bounded
    // pool of mutation ancestors, so the campaign walks outward from
    // the curated seeds instead of orbiting a few edits around them.
    // Guard rails against drift: the admission bar (ran + non-trivial
    // + shape-novel), a 50/50 base-seed mix so the walk can't leave
    // the bug-rich shapes behind, and FIFO eviction bounding lineage
    // depth. Trajectories are NOT seed-reproducible (pool completion
    // order feeds the ring) — findings stay reproducible from their
    // recorded program text, and seed-replay tooling (selfcheck/
    // gen-check/detcheck) never uses the ring.
    let ring = std::sync::Arc::new(std::sync::Mutex::new((
        std::collections::VecDeque::<String>::new(),
        ahash::AHashSet::<u64>::new(),
    )));
    const RING_CAP: usize = 256;
    let admit = ring.clone();
    Source {
        name: "fuzz",
        weight,
        gens: (0..tasks.max(1))
            .map(|k| {
                let seeds = seeds.clone();
                let donors = donors.clone();
                let ring = ring.clone();
                // A large odd stride keeps the per-task streams apart.
                let mut rng = mutate::Rng::new(
                    seed.wrapping_add((k as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)),
                );
                let g: Box<dyn FnMut() -> String + Send> = Box::new(move || {
                    // Mutate a random seed; retry a few times if a mutation
                    // chain didn't yield a parseable program, falling back to
                    // a raw seed (always valid) so the pool never stalls.
                    // `mutate_wrapper` preserves (and M3-mutates) schedule
                    // headers.
                    for _ in 0..8 {
                        let s = {
                            let ring = ring.lock().unwrap();
                            if !ring.0.is_empty() && rng.below(2) == 0 {
                                ring.0[rng.below(ring.0.len())].clone()
                            } else {
                                seeds[rng.below(seeds.len())].to_string()
                            }
                        };
                        if let Some(p) = mutate::mutate_wrapper(&s, &donors, &mut rng, 5)
                        {
                            return p;
                        }
                    }
                    seeds[rng.below(seeds.len())].to_string()
                });
                g
            })
            .collect(),
        on_agree: Some(Box::new(move |prog, ran| {
            if !ran {
                return false;
            }
            let Some((sig, nodes, interesting)) = mutate::shape_stats(prog) else {
                return false;
            };
            if nodes < 8 || nodes > 600 || !interesting {
                return false;
            }
            let mut ring = admit.lock().unwrap();
            if !ring.1.insert(sig) {
                return false;
            }
            ring.0.push_back(prog.to_string());
            if ring.0.len() > RING_CAP {
                ring.0.pop_front();
            }
            true
        })),
    }
}

/// How many checks to keep in flight — the oracle is mostly I/O/wait
/// (resolver + runtime spin-up, the quiescence wait), so each check only
/// keeps a fraction of a core busy. Heavily over-subscribe the cores (8×):
/// measured at 2× the campaign used ~4 of 14 cores, so ~1/7 of a core per
/// in-flight check — 8× brings that to full saturation. (True per-core
/// efficiency wants runtime reuse across checks — one shared in-process
/// resolver instead of a fresh netidx stack per program — a deeper
/// follow-up; over-subscription is the cheap win.)
fn parallelism() -> usize {
    // `GRAPHIX_FUZZ_PAR` overrides — several concurrent soak campaigns
    // (fuzz + generate + generate --reactive overnight) each spawning
    // the full 8x oversubscription would triple the child-process load;
    // the soak driver sets each to a share instead.
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

/// [`parallelism`] bounded by MEMORY for the in-process regress gate.
/// Each regress slot runs BOTH engines in this one process
/// (`check_classified` join!s them), and the runaway-recursion pins
/// legitimately grow toward the stack budget before containment stops
/// them — so the worst case is par × ~2 stacks plus each slot's
/// runtime heap, and cores×8 ignores it entirely: aieka's aug26a
/// launch (36 cores → par 288 on a 62GB box, with scale=4 timeouts
/// letting runaways reach the budget instead of dying at the 3s
/// timeout) ran this gate TWICE — soak.sh's pre-launch run passed,
/// the campaign process's own startup run was OOM-killed mid-corpus
/// (the log just stops: no panic, no summary line, and wait_for_gate
/// never saw its line). A conservative 3GB per in-flight slot against
/// HALF of RAM keeps the worst case inside the box. The cap applies
/// even under an explicit `GRAPHIX_FUZZ_PAR`: that knob shares
/// CHILD-PROCESS load between campaigns, and honoring it here would
/// re-open the same coin-flip in every campaign process's startup
/// gate.
fn regress_parallelism() -> usize {
    let mem = total_memory_bytes().unwrap_or(16 << 30);
    parallelism().min((((mem / 2) / (3 << 30)) as usize).max(2))
}

/// Source C campaign: generate valid programs from scratch (type-directed)
/// and run each through the oracle. Reaches shapes no seed contains.
/// Deterministic in `seed` (programs are generated sequentially; only the
/// oracle checks run concurrently). A generated div-by-zero produces
/// bottom = `Timeout` in all modes (agreement) — those would each waste
/// the full timeout sleeping, so running a pool of them concurrently is
/// what keeps the CPU busy.
pub async fn generate_campaign(
    iters: Option<usize>,
    seed: u64,
    timeout: Duration,
    corpus: &std::sync::Arc<Corpus>,
    reactive: bool,
) -> FuzzStats {
    let mut src = generate_source(seed, 1.0, reactive, gen_tasks());
    src.name = "";
    run_pool_multi(corpus, iters, timeout, vec![src])
        .await
        .pop()
        .map(|(_, stats, _)| stats)
        .unwrap_or_default()
}

/// Sources B and C: fresh type-directed programs, plain or scheduled.
/// Neither feeds the mutation ring — they are already exploring by
/// construction, and admitting them would change what the ring's
/// novelty counter measures.
pub fn generate_source(
    seed: u64,
    weight: f64,
    reactive: bool,
    tasks: usize,
) -> Source<'static> {
    Source {
        name: if reactive { "reactive" } else { "generate" },
        weight,
        gens: (0..tasks.max(1))
            .map(|k| {
                let mut rng = mutate::Rng::new(
                    seed.wrapping_add((k as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)),
                );
                let g: Box<dyn FnMut() -> String + Send> = Box::new(move || {
                    if reactive {
                        generate::reactive::gen_reactive_program(&mut rng)
                    } else {
                        generate::gen_program(&mut rng)
                    }
                });
                g
            })
            .collect(),
        on_agree: None,
    }
}

/// What one pool slot concluded about a program.
enum PoolResult {
    Agree {
        /// Both outcomes were runtime traces — the ring-admission bar.
        ran: bool,
    },
    Diverge(Divergence),
    /// The isolated child died (signal / abort / hang) — the program
    /// kills the evaluator itself. String = wait status + stderr tail.
    Crash(String),
}

/// Run one oracle check in a CHILD process (`graphix-fuzz check-one`:
/// program on stdin, one `VERDICT\t<AGREE|DIVERGE>` line on stdout). A
/// mutant that kills the evaluator — SIGSEGV in a JIT'd kernel, the
/// node-walk's stack-overflow abort on runaway recursion, a drop-helper
/// null panic — kills only the child; the campaign records a crash
/// finding and keeps running. (Pre-isolation, one such mutant killed the
/// whole campaign with no finding saved — twice: #214, then the runaway-
/// recursion compile-time overflow.)
/// Path used to re-exec ourselves for child processes. On Linux this is
/// /proc/self/exe, which resolves to the running binary's inode even after
/// the file on disk is rebuilt/replaced mid-campaign — `current_exe()`
/// returns a "... (deleted)" path then and every child spawn ENOENTs.
fn child_exe() -> std::path::PathBuf {
    #[cfg(target_os = "linux")]
    return std::path::PathBuf::from("/proc/self/exe");
    #[cfg(not(target_os = "linux"))]
    return std::env::current_exe().expect("current_exe");
}

/// CPU (user + system) this process has burned so far.
///
/// The soak scheduler's currency. A worker SLOT is not a core: it is one
/// check in flight, and how much CPU that draws depends on how much of
/// its life the subject spends blocked. The conversion rate differs per
/// SOURCE — reactive subjects are compute-saturated while corpus mutants
/// sit waiting on child startup and timeouts — so three lanes given 85
/// slots each split a 32-core box 13/19/66 while looking evenly
/// provisioned. Allocating on measured CPU is the fix; allocating on
/// slots is the bug.
pub fn self_cpu() -> Duration {
    // SAFETY: getrusage writes a POD struct we own; the zeroed value is
    // a valid rusage and the call cannot fail for RUSAGE_SELF.
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

/// A worker child's name for the file it drops its own [`self_cpu`] in,
/// relative to the sandbox cwd the parent owns and can read after the
/// child is gone. The child reports its OWN usage rather than the parent
/// reading rusage at reap time, because tokio reaps asynchronously and
/// `RUSAGE_CHILDREN` is process-wide — neither can attribute CPU to the
/// source that asked for the work.
const CPU_REPORT: &str = "cpu-usage";

/// Called by every worker child arm on its way out.
pub fn report_self_cpu() {
    let _ = std::fs::write(CPU_REPORT, self_cpu().as_micros().to_string());
}

/// Read back what a child reported. Absent (an old binary, a child that
/// died before reporting) reads as zero: unattributed CPU makes a source
/// look cheap, which the scheduler self-corrects on the next completion.
fn child_cpu(sandbox: &std::path::Path) -> Duration {
    std::fs::read_to_string(sandbox.join(CPU_REPORT))
        .ok()
        .and_then(|s| s.trim().parse::<u64>().ok())
        .map(Duration::from_micros)
        .unwrap_or_default()
}

/// Give a worker child a PARENT-owned sandbox cwd (generated programs
/// write files with arbitrary relative paths — an inherited cwd gets
/// littered). Parent-owned because the worker arms exit via
/// `process::exit` (drops skipped): a child-owned tempdir leaked per
/// subject, and a soak's millions of subjects exhausted /tmp's INODES
/// (jul10d — 8.1M inodes 0 free at 78MB used; every subsequent run
/// failed on ENOSPC and recorded a garbage finding). The returned
/// guard removes the dir on drop — declare it BEFORE the child handle
/// so `kill_on_drop` reaps the child first. GRAPHIX_FUZZ_SANDBOXED
/// tells the child to skip its manual-invocation self-sandbox. A
/// tempdir failure is a broken HARNESS — die loudly, exactly as child
/// spawn failure does.
/// The child's own address-space limit — the other half of
/// [`sandbox_cwd`]'s rlimit, applied here rather than through a
/// `pre_exec` hook so the parent keeps posix_spawn's vfork fast path
/// (see the comment there for the measurement). Called once at
/// startup by every process the harness spawns; a no-op when
/// `GRAPHIX_FUZZ_MEM_LIMIT` is 0.
pub fn apply_mem_limit() {
    #[cfg(unix)]
    {
        // Children only. The DRIVER must never limit itself: its own
        // address space is far larger than any child's (139GB of VA
        // reservations against 547MB resident on a live soak box), so
        // the child's ceiling would kill the parent outright.
        // `sandbox_cwd` sets this marker on everything it spawns.
        if std::env::var_os("GRAPHIX_FUZZ_SANDBOXED").is_none() {
            return;
        }
        let limit: u64 = std::env::var("GRAPHIX_FUZZ_MEM_LIMIT")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(48 << 30);
        if limit > 0 {
            let rl = libc::rlimit { rlim_cur: limit, rlim_max: limit };
            // Best effort: a hard limit already below `limit` makes
            // this EPERM, and the subject timeout is the real guard.
            unsafe { libc::setrlimit(libc::RLIMIT_AS, &rl) };
        }
    }
}

fn sandbox_cwd(cmd: &mut tokio::process::Command) -> tempfile::TempDir {
    match tempfile::tempdir() {
        Ok(d) => {
            cmd.current_dir(d.path()).env("GRAPHIX_FUZZ_SANDBOXED", "1");
            // Depth is bounded by memory, so a runaway non-tail
            // recursion in a kernel grows stack at hundreds of MB/s
            // until the subject deadline; the budget aborts it (a
            // Timeout, like the deadline) before a box of workers
            // runs out of memory. GRAPHIX_STACK_BUDGET (bytes) in the
            // parent's environment overrides.
            if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
                cmd.env("GRAPHIX_STACK_BUDGET", (1u64 << 30).to_string());
            }
            // ADDRESS-SPACE RLIMIT per child (2026-08-13): under
            // unconditional transient retention a fib-tree subject
            // materializes its whole call tree of retained instances —
            // by design ("let the user run out of memory") — but at
            // 64-85 workers per box an unbounded child is an OOM
            // recipe. The limit converts a runaway into a child
            // allocation failure (an honest per-subject verdict) long
            // before the box's OOM killer starts shooting lanes.
            // GRAPHIX_FUZZ_MEM_LIMIT (bytes) overrides; 0 disables.
            //
            // 48GB, NOT 8GB (the aug13f/g 1100-crash flood): RLIMIT_AS
            // counts VIRTUAL reservations — glibc's per-thread malloc
            // arenas (64MB VA x 8 x cores) plus ~8MB per thread stack
            // put a healthy batch child past 8GB of address space at a
            // fraction of it in RSS, and the first thread spawn to
            // lose died with EAGAIN ("spawn intern GC thread"). The
            // real memory guard is the subject TIMEOUT (allocation is
            // bind-rate-bound, ~100MB/s — a wall-clock kill fires
            // first); the rlimit is only the backstop against an
            // allocation-rate surprise.
            //
            // The child sets it ON ITSELF (`apply_mem_limit`, called
            // from main) rather than the parent setting it through a
            // `pre_exec` hook, because ANY `pre_exec` closure makes
            // std take the fork+exec path instead of posix_spawn's
            // `CLONE_VM|CLONE_VFORK`. Fork cost scales with the
            // PARENT's address space, and the soak driver's is huge:
            // measured 11.8ms per spawn at 139GB of VA / 1422 VMAs,
            // against 0.22ms for a small process. At ~61 spawns/s that
            // was ~73% of a core spent in the kernel copying page
            // tables, concurrent forks serializing on mmap_lock, and
            // the driver pinned at 100% CPU while the box sat 40%
            // idle. `current_dir` and `env` keep the fast path; only
            // `pre_exec` loses it.
            d
        }
        Err(e) => {
            eprintln!("FATAL fuzz harness: sandbox tempdir failed: {e}");
            std::process::exit(2)
        }
    }
}

/// The per-subject determinism check — the `selfcheck-one` child body:
/// each mode run twice concurrently AT THE PROGRAM'S ORACLE TIER, with
/// a sequential uncontended confirm-retry before flagging (under a
/// loaded gate a borderline run can breach the wall-clock backstop and
/// read Timeout-vs-Trace; genuine nondeterminism repeats). Returns the
/// modes that stayed flaky.
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
        // The dispatch route's cycle offsets are not comparable at
        // Exact strength even against ITSELF: the gap compiles and
        // callable dispatch take a run-dependent number of cycles
        // (async pacing), so only per-epoch settled values are
        // contractual — the same rationale (and the same tier) as
        // check_callable's dispatch-pair comparison. Without this the
        // gate flagged perfectly value-deterministic callable twins
        // as flaky on cycle-index jitter (2026-08-20).
        let route_tier = match route {
            Route::Dispatch if tier == OracleTier::Exact => OracleTier::FinalValues,
            _ => tier,
        };
        let (a, b) = tokio::join!(
            run_program_routed(prog, mode, route, timeout),
            run_program_routed(prog, mode, route, timeout),
        );
        if !a.agrees_with_at(&b, route_tier) {
            // The confirm pair runs at 4x, and a TIMEOUT on one side is
            // read as a budget artifact rather than a verdict: a
            // Timeout is not a value, so comparing it against one
            // measures the budget, not the engine's determinism. Under
            // a loaded gate (the full workspace suite is ~13x slower
            // than a solo run) a subject sitting near the budget flips
            // Timeout/Trace between runs and reported as nondeterminism
            // — a `f(i64:256)` non-tail recursion did exactly this,
            // 1-2 times per 200-subject sweep, while being perfectly
            // deterministic in isolation (8/8 identical). Same medicine
            // as `run_regression`'s sequential retry: never let a
            // budget artifact become a semantic verdict.
            let big = timeout * 4;
            let a2 = run_program_routed(prog, mode, route, big).await;
            let b2 = run_program_routed(prog, mode, route, big).await;
            if a2.agrees_with_at(&b2, route_tier) {
                continue;
            }
            if matches!(a2, Outcome::Timeout) || matches!(b2, Outcome::Timeout) {
                // Inconclusive at this budget, not flaky. The caller
                // counts these so coverage can't degrade silently.
                bad.push("inconclusive");
                continue;
            }
            bad.push(match (mode, route) {
                (Mode::Interp, Route::InLanguage) => "interp",
                (Mode::Jit, Route::InLanguage) => "jit",
                (Mode::Interp, Route::Dispatch) => "interp-dispatch",
                (Mode::Jit, Route::Dispatch) => "jit-dispatch",
            });
        }
    }
    bad
}

/// Run one selfcheck subject in a CHILD process (`graphix-fuzz
/// selfcheck-one`) — the campaign pool's isolation, needed here for
/// memory as much as crash containment: every in-process `run_program`
/// leaks its context's JIT pages BY DESIGN (spliced kernels' pointers
/// must survive resets — see the arena note in graphix-compiler
/// fusion/emit.rs), so the old in-process selfcheck accumulated the
/// whole stdlib's fused kernels per subject and a full gate run peaked
/// >20GiB resident. The child pays the leak and exits.
async fn selfcheck_isolated(prog: &str, timeout: Duration) -> Vec<&'static str> {
    use tokio::io::AsyncWriteExt;
    let mut cmd = tokio::process::Command::new(child_exe());
    let _sandbox = sandbox_cwd(&mut cmd);
    cmd.arg("selfcheck-one")
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        // A spawn IO error is a broken HARNESS — die loudly, exactly as
        // `check_isolated` does.
        Err(e) => {
            eprintln!("FATAL fuzz harness: child spawn failed: {e}");
            std::process::exit(2)
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(prog.as_bytes()).await;
    }
    // Up to 8 in-child runs (2 modes × 2 + confirm retries), each
    // bounded by the per-run timeout; margin for pool contention.
    let deadline = timeout * 10 + Duration::from_secs(30);
    let out = match tokio::time::timeout(deadline, child.wait_with_output()).await {
        Ok(Ok(out)) => out,
        // A dead or wedged child is nondeterminism by definition (the
        // gate demands clean determinism from every subject) — surface
        // it rather than dropping the subject silently.
        Ok(Err(_)) | Err(_) => return vec!["crash"],
    };
    // Verdict in the EXIT CODE (stdout is program-pollutable): 0 =
    // clean, 40+mask flags the flaky modes.
    match out.status.code() {
        Some(0) => Vec::new(),
        Some(41) => vec!["interp"],
        Some(42) => vec!["jit"],
        Some(43) => vec!["interp", "jit"],
        // Timed out at 4x on the confirm pair — the budget decided, not
        // the engine. Counted, never a failure.
        Some(50) => vec!["inconclusive"],
        _ => vec!["crash"],
    }
}

/// Normalize a CLIF dump for structural comparison ACROSS PROCESSES:
/// drop log lines, blind pointer-magnitude constants (interned
/// string/value table addresses, helper addresses — ASLR varies them
/// per process), and CANONICALIZE ExprIds to first-seen order (the id
/// counter is global and the runtime-init stdlib compile consumes a
/// per-process-varying number before the program's own ids, so raw
/// numbers differ while the structure is identical). What remains is
/// the structural shape: kernel identity (in canonical order),
/// instruction sequences, fn_index constants, slot layouts.
/// Over-blinding a large program LITERAL is harmless — both dumps
/// blind identically.
pub fn normalize_clif(s: &str) -> String {
    let mut ids: AHashMap<String, usize> = AHashMap::new();
    let mut out = String::with_capacity(s.len());
    for line in s.lines() {
        // env_logger lines: `[2026-07-08T...Z LEVEL target] ...`
        if line.starts_with('[') {
            continue;
        }
        // Canonicalize every per-process COUNTER to first-seen order:
        // ExprIds (global counter offset by the runtime-init stdlib
        // compile), FuncIds (`u0:N` — module declaration order), the
        // wrapper counter (`kir_N`) and lambda ids (`lambda#N`). Raw
        // numbers differ across processes while the structure is
        // identical; after canonicalization any remaining difference
        // is REAL shape drift (a kernel fused in one process and not
        // the other, a call binding a different callee).
        let line = &{
            let mut r = String::with_capacity(line.len());
            let mut rest = line;
            'outer: loop {
                let mut best: Option<(usize, &str)> = None;
                // `<abstract#` and `'_`: a CompileErr subject's stderr
                // carries its diagnostic, whose abstract ids and tvar
                // ids drift between fresh children (the class-6 genus
                // — concurrent stdlib compile interleaves the
                // process-global counters), which flapped detcheck on
                // abstract-opaque-overtag-jul2026/01 (2026-08-20).
                // Same first-seen canonicalization as the oracle's
                // `normalize_diag`.
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
                    // pattern not followed by digits — emit as-is
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

/// The determinism gate (detcheck): RUN `prog` to quiescence in TWO
/// fresh child processes — each gets its own ASLR — with
/// GRAPHIX_DUMP_CLIF=1, and compare the normalized dumps and exit
/// codes. Fusion SHAPE must be a pure function of the program text
/// (predictable performance is a core graphix value): any
/// cross-process difference means an allocation-order/pointer-order
/// dependence somewhere in typing, static resolution, or fusion — the
/// #19 class (the stale-layout kernel cache, the by-name clone capture
/// resolution, and the pointer-ordered callee fn-index assignment all
/// manifested exactly this way). Driving to quiescence (rather than
/// compile-only) covers lazily instantiated collection callbacks AND
/// removes the compile-vs-shutdown race that made their dumps a coin
/// flip. A wall-clock timeout on either side (exit 4) skips the pair —
/// the cut point is inherently racy. `Some(detail)` = FLAP.
pub async fn detcheck_one_pair(prog: &str, timeout: Duration) -> Option<String> {
    async fn compile_child(
        prog: &str,
        timeout: Duration,
    ) -> std::result::Result<(Option<i32>, String), String> {
        use tokio::io::AsyncWriteExt;
        let mut cmd = tokio::process::Command::new(child_exe());
        let _sandbox = sandbox_cwd(&mut cmd);
        cmd.arg("detcheck-one")
            .env("TOKIO_WORKER_THREADS", "2")
            .env("GRAPHIX_DUMP_CLIF", "1")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true);
        let mut child = cmd.spawn().map_err(|e| format!("spawn: {e}"))?;
        if let Some(mut stdin) = child.stdin.take() {
            let _ = stdin.write_all(prog.as_bytes()).await;
        }
        let deadline = timeout * 2 + Duration::from_secs(30);
        let out = tokio::time::timeout(deadline, child.wait_with_output())
            .await
            .map_err(|_| "HANG (compile)".to_string())?
            .map_err(|e| format!("wait: {e}"))?;
        Ok((out.status.code(), normalize_clif(&String::from_utf8_lossy(&out.stderr))))
    }
    let (a, b) = tokio::join!(compile_child(prog, timeout), compile_child(prog, timeout));
    match (a, b) {
        (Ok((ca, da)), Ok((cb, db))) => {
            if ca == Some(4) || cb == Some(4) {
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
    use tokio::task::JoinSet;
    let par = (parallelism() / 2).max(1);
    let mut set: JoinSet<(String, Option<String>)> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one = |set: &mut JoinSet<_>, i: usize| {
        let (name, prog) = programs[i].clone();
        set.spawn(async move {
            let r = detcheck_one_pair(&prog, timeout).await;
            (name, r)
        });
    };
    while next < programs.len() && set.len() < par {
        spawn_one(&mut set, next);
        next += 1;
    }
    let mut flaps = Vec::new();
    while let Some(res) = set.join_next().await {
        if let Ok((name, Some(detail))) = res {
            flaps.push((name, detail));
        }
        if next < programs.len() {
            spawn_one(&mut set, next);
            next += 1;
        }
    }
    flaps
}

async fn check_isolated(prog: &str, timeout: Duration) -> (PoolResult, Duration) {
    let mut cmd = tokio::process::Command::new(child_exe());
    let sandbox = sandbox_cwd(&mut cmd);
    let res = check_isolated_in(prog, timeout, &mut cmd).await;
    (res, child_cpu(sandbox.path()))
}

async fn check_isolated_in(
    prog: &str,
    timeout: Duration,
    cmd: &mut tokio::process::Command,
) -> PoolResult {
    use tokio::io::AsyncWriteExt;
    cmd.arg("check-one")
        // The pool already provides the concurrency; small children keep
        // total thread count sane at parallelism() in-flight processes.
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        // A spawn IO error is a broken HARNESS (fd exhaustion, fork
        // failure), not a program crash — recording it would flood the
        // corpus with garbage findings at instant-fail speed. Die loudly.
        Err(e) => {
            eprintln!("FATAL fuzz harness: child spawn failed: {e}");
            std::process::exit(2)
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        // A write error means the child died instantly — fall through,
        // wait_with_output captures the status.
        let _ = stdin.write_all(prog.as_bytes()).await;
    }
    // The child runs interp+jit with its own internal per-mode `timeout`
    // (Timeout is a NORMAL outcome there) — the outer deadline only
    // catches a wedged child (a compile-time hang, a runaway that dodges
    // the guard page), with margin for pool contention. It must cover
    // the child's whole legitimate worst case: the concurrent first
    // runs, `check()`'s escalation retry (whose budget has a 60s
    // floor), and the nondeterminism re-run — a compute-bound
    // single-cycle program can't be preempted by the internal timeout
    // (no await point mid-cycle), so under-margining reaps children
    // that would have self-cleared (jul17a crash_000001).
    let deadline = timeout * 4
        + (timeout * 8).max(Duration::from_secs(60))
        + Duration::from_secs(30);
    let out = match tokio::time::timeout(deadline, child.wait_with_output()).await {
        Ok(Ok(out)) => out,
        Ok(Err(e)) => return PoolResult::Crash(format!("wait: {e}")),
        // Future dropped → kill_on_drop reaps the child.
        Err(_) => return PoolResult::Crash("HANG (outer deadline)".into()),
    };
    // The verdict is the EXIT CODE (0 = agree, 10 = diverge): a line
    // protocol on stdout is corruptible by the program under test
    // (sys::io::stdout — soak jul06g false crash). Anything else is a
    // crash.
    match out.status.code() {
        Some(0) => PoolResult::Agree { ran: false },
        // Agreement where both modes RAN (exit 7, see `check-one`) —
        // the mutation ring may breed from this program.
        Some(7) => PoolResult::Agree { ran: true },
        // The child proved the program diverges WITHOUT dying, so an
        // in-process re-check of the SAME program is safe — re-run it
        // here to get the full Divergence for the record pipeline.
        Some(10) => match check(prog, timeout).await {
            Some(d) => PoolResult::Diverge(d),
            // Flaky (borderline timeout) — drop it rather than record
            // an unreproducible finding (and never breed from it).
            None => PoolResult::Agree { ran: false },
        },
        _ => {
            // A SIGTERM death is the campaign STOP's own kill signal
            // reaching a mid-flight child — a teardown artifact, not a
            // finding (aug04eaieka reactive/crash_000000: the aug04e →
            // aug04f redeploy's stop recorded itself as a crash).
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                if out.status.signal() == Some(15) {
                    return PoolResult::Agree { ran: false };
                }
            }
            // Include the child's last stderr lines — the std
            // stack-overflow handler / panic hook message is the triage
            // signal that distinguishes "node-walk overflow (known
            // class)" from "SIGSEGV in JIT'd frames (real codegen bug,
            // prints nothing)".
            let stderr = String::from_utf8_lossy(&out.stderr);
            let tail: Vec<&str> = stderr.lines().rev().take(2).collect();
            let mut status = out.status.to_string();
            for l in tail.into_iter().rev() {
                status.push_str(" | ");
                status.push_str(l);
            }
            PoolResult::Crash(status)
        }
    }
}

/// Oracle-check budget for the CAMPAIGN's minimizer. Interactive
/// `graphix-fuzz minimize` takes its own (much larger) budget: a soak
/// records thousands of findings and pays this per finding, so it buys
/// a legible reproducer, not a minimal one.
pub const CAMPAIGN_MINIMIZE_BUDGET: usize = 80;

/// Minimize a diverging program in a CHILD process (`graphix-fuzz
/// minimize-one <out-path>`: program on stdin, the reduced program
/// written to `<out-path>` — NOT stdout, which the programs the
/// minimizer runs can pollute with their own writes). Minimization is
/// the one place a proven-non-crashing divergence can still kill the
/// evaluator: a REDUCTION may itself be a crasher (e.g. dropping a
/// recursive function's base case → runaway), and the minimizer checks
/// candidates in-process. `None` = the child died or wedged — the
/// caller records the unminimized mutant instead (a finding is never
/// lost to the minimizer).
async fn minimize_isolated(prog: &str, timeout: Duration) -> Option<String> {
    use tokio::io::AsyncWriteExt;
    let mut cmd = tokio::process::Command::new(child_exe());
    let sandbox = sandbox_cwd(&mut cmd);
    // The output file lives INSIDE the sandbox (unique per child), so
    // the guard's drop cleans it along with the program's litter.
    let out_path = sandbox.path().join("min.gx");
    cmd.arg("minimize-one")
        .arg(&out_path)
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .kill_on_drop(true);
    let mut child = cmd.spawn().ok()?;
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(prog.as_bytes()).await;
    }
    // Worst case the whole budget is bottom programs sleeping the
    // per-mode timeout — bound it generously, the minims pool is
    // concurrent and a kill falls back to the unminimized mutant.
    let deadline =
        timeout * 2 * CAMPAIGN_MINIMIZE_BUDGET as u32 + Duration::from_secs(60);
    let ok = matches!(
        tokio::time::timeout(deadline, child.wait_with_output()).await,
        Ok(Ok(out)) if out.status.success()
    );
    let min = if ok { std::fs::read_to_string(&out_path).ok() } else { None };
    let min = min.map(|m| m.trim().to_string());
    match min {
        Some(m) if !m.is_empty() => Some(m),
        _ => None,
    }
}

/// Environment-broken backstop for the campaign pool: when a MAJORITY
/// of a recent window of subjects produce findings, the problem is the
/// environment (ENOSPC, fd exhaustion) or a fundamentally broken
/// build, not the programs — the worst real bug classes hit well under
/// 0.1% of subjects, while a broken environment fails EVERY subject
/// and records garbage findings at disk speed (jul10d: /tmp inode
/// exhaustion flooded the corpus at 300MB/s until killed by hand;
/// dedup can't help because the crash key varies with program text).
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

/// One work source in a soak: where its programs come from, what it does
/// with an agreeing result, and the share of the box's CPU it should
/// draw.
pub struct Source<'a> {
    /// Prefix on this source's counter lines; "" for a single-source
    /// campaign, which keeps the pre-merge log format byte-identical.
    pub name: &'static str,
    /// Relative CPU share. Normalized internally, so any positive scale
    /// works.
    pub weight: f64,
    /// Generators, run in their OWN tasks rather than on the driver —
    /// hence `Send` — and more than one, because generation is real
    /// work rather than bookkeeping: `mutate_wrapper` parses and
    /// rewrites an AST per subject, which is a whole thread's worth for
    /// the fuzz source and cannot feed a pool by itself. Three lane
    /// PROCESSES hid both facts by having three drivers.
    ///
    /// Each generator owns a DISJOINT seed stream, so a subject stays
    /// reproducible as (source, seed); only the interleaving between
    /// them is nondeterministic, which the ring already was.
    pub gens: Vec<Box<dyn FnMut() -> String + Send + 'a>>,
    /// Ring admission, run in its OWN task like generation. `None` is
    /// "this source has no ring" — the generate sources — which lets the
    /// driver skip even cloning the program. `shape_stats` PARSES the
    /// program and walks its AST, so doing this in the result loop cost
    /// the fuzz source its slots: with generation already moved off, a
    /// fuzz-only pool still ran 14 of 24 while a reactive-only pool
    /// (no admitter) ran 24 of 24.
    pub on_agree: Option<Box<dyn FnMut(&str, bool) -> bool + Send + 'a>>,
}

/// Per-source accounting. `cpu` is what the source's finished children
/// actually burned; `inflight` is what it has issued but not yet been
/// charged for, which the scheduler estimates at the source's own
/// observed mean so a burst cannot out-run its own feedback and
/// oscillate.
#[derive(Default)]
struct SourceState {
    cpu: Duration,
    inflight: usize,
    done: usize,
    stats: FuzzStats,
    pending: Vec<String>,
    /// Ring admissions in flight, and the count the admit task has made.
    /// `None` when the source has no ring.
    admit: Option<tokio::sync::mpsc::Sender<(String, bool)>>,
    novel: std::sync::Arc<std::sync::atomic::AtomicUsize>,
    /// Programs the source's generator task has already produced. The
    /// driver only pops; it never generates. Refilled by `try_recv`, so
    /// filling a 64-subject batch is a memory move rather than 64 AST
    /// rewrites blocking every other dispatch — which is what made
    /// utilization a sawtooth (a long generation stall, then one child
    /// landing 64 subjects at once).
    ready: std::collections::VecDeque<String>,
}

impl SourceState {
    /// CPU this source is expected to have drawn once everything it has
    /// issued lands. `global_mean` seeds a source that has not completed
    /// anything yet, so the very first picks still spread out.
    fn projected(&self, global_mean: f64) -> f64 {
        let mean = if self.done > 0 {
            self.cpu.as_secs_f64() / self.done as f64
        } else {
            global_mean
        };
        self.cpu.as_secs_f64() + self.inflight as f64 * mean
    }
}

/// Generator tasks per source. Generation is CPU work, not bookkeeping,
/// so one task per source cannot feed a pool: measured at par=24, the
/// fuzz source held 13 of 24 slots with a single generator while the
/// cheap reactive generator held all 24. Capped because these compete
/// with the workers themselves for cores.
pub fn gen_tasks() -> usize {
    std::thread::available_parallelism().map(|n| n.get() / 4).unwrap_or(2).clamp(2, 8)
}

/// How many programs each source's generator may run ahead. Deep enough
/// that filling a 64-subject batch is always a memory move, small enough
/// that a source cannot hoard memory when the pool is busy elsewhere.
const GEN_BUFFER: usize = 512;

/// Drain whatever the generators have produced into the ready buffers,
/// then answer which source should actually be issued.
///
/// `want` is the scheduler's choice on CPU grounds; it wins whenever it
/// has work. Otherwise any source with work beats issuing nothing, since
/// an idle slot serves no target at all — the CPU shares reassert
/// themselves as soon as the generator catches up.
fn ready_source(
    want: usize,
    states: &mut [SourceState],
    gens: &mut [tokio::sync::mpsc::Receiver<String>],
) -> Option<usize> {
    for (st, rx) in states.iter_mut().zip(gens.iter_mut()) {
        while st.ready.len() < GEN_BUFFER {
            match rx.try_recv() {
                Ok(p) => st.ready.push_back(p),
                Err(_) => break,
            }
        }
    }
    if !states[want].ready.is_empty() {
        return Some(want);
    }
    states.iter().position(|st| !st.ready.is_empty())
}

/// The aggregator: issue work ORDERS, aggregate what comes back.
///
/// The parent's per-subject cost is gone. It does not generate, classify,
/// or ship program text; a child is told which generator to run, from
/// which seed, how many, and with which ring ancestors, and answers with
/// counts plus the rare interesting program. Everything the parent still
/// does is per batch (issue an order, charge its CPU) or per finding
/// (derive a divergence, admit a ring shape), and both are rare.
///
/// That is what makes it scale: dispatch was one serial task pinned at
/// one core, which capped a 20-core box at ~70% no matter how the work
/// inside it was arranged. Three separate lane processes reached 100% by
/// having three such tasks; this reaches it by giving the task almost
/// nothing to do.
pub async fn run_aggregator(
    corpus: &std::sync::Arc<Corpus>,
    iters: Option<usize>,
    timeout: Duration,
    weights: [f64; 3],
) -> Vec<(&'static str, FuzzStats, Duration)> {
    use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
    use tokio::task::JoinSet;
    const KINDS: [SourceKind; 3] =
        [SourceKind::Fuzz, SourceKind::Generate, SourceKind::Reactive];
    // Findings are confirmed in detached `derive` tasks, so the tally has
    // to cross tasks. Without it the divergence/crash columns sat at 0
    // while the corpus grew — the dashboard's red flag never fired on the
    // first live finding (aug17d katana).
    struct Found {
        divergences: [AtomicUsize; 3],
        crashes: [AtomicUsize; 3],
    }
    let found = std::sync::Arc::new(Found {
        divergences: std::array::from_fn(|_| AtomicUsize::new(0)),
        crashes: std::array::from_fn(|_| AtomicUsize::new(0)),
    });
    /// Ring ancestors per order — a sample, not a snapshot.
    const RING_SAMPLE: usize = 16;
    const RING_CAP: usize = 256;
    let par = parallelism();
    let bsize = batch_size().max(1);
    let mut stats = [FuzzStats::default(), FuzzStats::default(), FuzzStats::default()];
    let mut cpu = [Duration::ZERO; 3];
    let mut inflight = [0usize; 3];
    let mut done = [0usize; 3];
    // The batch FAILURE RATE: subjects a batch child could not resolve,
    // which the parent re-derives one process each. Batching everything
    // is only right while this stays small, so it is reported rather
    // than assumed.
    let mut suspect = [0usize; 3];
    let mut seed_ctr = [0u64; 3];
    let mut launched = 0usize;
    let mut ring: std::collections::VecDeque<String> = std::collections::VecDeque::new();
    let mut ring_sigs: ahash::AHashSet<u64> = ahash::AHashSet::default();
    let mut rng = mutate::Rng::new(0xC0FFEE);
    let mut orders: JoinSet<(usize, OrderResult)> = JoinSet::new();
    let mut derive: JoinSet<()> = JoinSet::new();
    let mut breakage = BreakageWindow::new();
    let wsum = weights.iter().map(|w| w.max(0.0)).sum::<f64>().max(f64::MIN_POSITIVE);
    let want = |launched: usize| iters.map_or(true, |n| launched < n);
    loop {
        // Keep `par` orders in flight, choosing whichever source is
        // furthest below its target share of measured CPU.
        while want(launched) && orders.len() < par {
            let total_done: usize = done.iter().sum();
            let total_cpu: f64 = cpu.iter().map(|c| c.as_secs_f64()).sum();
            let mean = if total_done > 0 { total_cpu / total_done as f64 } else { 1.0 };
            let proj: Vec<f64> = (0..3)
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
            for i in 0..3 {
                let target = weights[i].max(0.0) / wsum;
                let actual = if tot > 0.0 { proj[i] / tot } else { 0.0 };
                if target - actual > best {
                    best = target - actual;
                    si = i;
                }
            }
            if weights[si] <= 0.0 {
                break;
            }
            let count = match iters {
                Some(n) => bsize.min(n.saturating_sub(launched)),
                None => bsize,
            };
            if count == 0 {
                break;
            }
            let sample: Vec<String> = if KINDS[si] == SourceKind::Fuzz && !ring.is_empty()
            {
                (0..RING_SAMPLE.min(ring.len()))
                    .map(|_| ring[rng.below(ring.len())].clone())
                    .collect()
            } else {
                Vec::new()
            };
            seed_ctr[si] += 1;
            let order = WorkOrder {
                kind: KINDS[si],
                // Distinct per (source, order): reproducible without a
                // shared counter the children would have to agree on.
                seed: (si as u64 + 1)
                    .wrapping_mul(0x9E37_79B9_7F4A_7C15)
                    .wrapping_add(seed_ctr[si]),
                count,
                ring: sample,
            };
            launched += count;
            inflight[si] += count;
            orders.spawn(async move { (si, run_order_child(&order, timeout).await) });
        }
        tokio::select! {
            biased;
            Some(res) = orders.join_next() => {
                let Ok((si, r)) = res else { continue };
                cpu[si] += r.cpu;
                inflight[si] = inflight[si].saturating_sub(r.ran.max(1));
                done[si] += r.ran;
                stats[si].run += r.ran;
                // Only MUTANTS breed. The children compute novelty for
                // every source (they have the program in hand), but
                // admitting generated shapes would change what the ring
                // is: its 50/50 base-seed mix exists to stop the walk
                // drifting off the bug-rich curated shapes, and feeding
                // it fresh random programs is a different experiment.
                // Worth trying deliberately, not as a side effect.
                for (sig, prog) in r.novel.into_iter().filter(|_| KINDS[si] == SourceKind::Fuzz) {
                    if ring_sigs.insert(sig) {
                        ring.push_back(prog);
                        stats[si].novel += 1;
                        if ring.len() > RING_CAP {
                            ring.pop_front();
                        }
                    }
                }
                if stats[si].run % 1000 < r.ran.max(1) {
                    stats[si].divergences = found.divergences[si].load(Relaxed);
                    stats[si].crashes = found.crashes[si].load(Relaxed);
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
                        "  {}…{} run, {} divergences, {} crashes, {} in corpus, \
                         {} novel shapes, {}% cpu, {}% individual",
                        KINDS[si].tag(), stats[si].run, stats[si].divergences,
                        stats[si].crashes, corpus.len(), stats[si].novel, pct, ipct
                    );
                }
                // A suspect is derived by the INDIVIDUAL path, which owns
                // the escalation ladder and the minimizer — the batch
                // child only ever fast-paths agreement.
                suspect[si] += r.suspect.len();
                for prog in r.suspect {
                    if derive.len() >= par {
                        let _ = derive.join_next().await;
                    }
                    let corpus = corpus.clone();
                    let found = found.clone();
                    derive.spawn(async move {
                        let (res, _) = check_isolated(&prog, timeout).await;
                        match res {
                            PoolResult::Agree { .. } => (),
                            PoolResult::Crash(status) => {
                                if status.contains("HANG")
                                    && ["rand::", "sys::", "http::"]
                                        .iter().any(|m| prog.contains(m))
                                {
                                    return;
                                }
                                found.crashes[si].fetch_add(1, Relaxed);
                                if corpus.record_crash(&prog, &status) {
                                    println!("CRASH — child {status}");
                                    println!("    program: {}", prog.replace('\n', "\\n"));
                                }
                            }
                            PoolResult::Diverge(d) => {
                                found.divergences[si].fetch_add(1, Relaxed);
                                let min = minimize_isolated(&prog, timeout)
                                    .await
                                    .unwrap_or_else(|| prog.clone());
                                if corpus.record(&d, &prog, &min) {
                                    println!("DIVERGENCE — {}", d.bisect());
                                    println!("    minimized: {min}");
                                    println!("    interp={:?} jit={:?}", d.interp, d.jit);
                                }
                            }
                        }
                    });
                }
                if breakage.note(!r.clean) {
                    eprintln!(
                        "FATAL fuzz harness: {} of the last {} orders came back \
                         unclean — the environment (or the build) is broken",
                        breakage.findings, BreakageWindow::LEN,
                    );
                    std::process::exit(2);
                }
            }
            Some(_) = derive.join_next() => {}
            else => break,
        }
    }
    while derive.join_next().await.is_some() {}
    for i in 0..3 {
        stats[i].divergences = found.divergences[i].load(Relaxed);
        stats[i].crashes = found.crashes[i].load(Relaxed);
    }
    (0..3).map(|i| (KINDS[i].tag(), stats[i].clone(), cpu[i])).collect()
}

/// Run several sources through ONE pool, dividing the box by measured
/// CPU rather than by worker slots.
///
/// Keeps `parallelism()` oracle checks in flight. Checks run in ISOLATED
/// child processes by default (see [`check_isolated`];
/// `GRAPHIX_FUZZ_INPROC=1` opts back into in-process for debugging). On a
/// divergence it fires a bounded-parallel task that minimizes, dedups
/// against `corpus`, and — if the minimized form is new — writes the
/// `.gx` and prints it immediately, all WITHOUT stalling the check pool
/// (minimization is ≈80 serial checks; running it inline drained the
/// cores). A crash records immediately (no minimization — the repro must
/// stay out-of-process). `iters = None` runs forever (until killed),
/// surfacing new divergences live; `Some(n)` stops after `n` programs
/// across all sources. Generators run on the driver task (sequential,
/// deterministic, cheap).
///
/// This is the whole reason the soak is one process. Three separate
/// lane processes could only divide the box through the OS scheduler,
/// which arbitrates between runnable PROCESSES — so equal worker counts
/// bought wildly unequal CPU (measured 13/19/66 on a three-lane box,
/// with the reactive lane taking two thirds while looking evenly
/// provisioned). Slots are not cores: a slot is one check in flight, and
/// what that draws depends on how much of its life the subject spends
/// blocked. Here every child reports its own CPU, each batch is
/// homogeneous in its source so the charge is exact, and the next batch
/// goes to whichever source is furthest below its target share.
pub async fn run_pool_multi(
    corpus: &std::sync::Arc<Corpus>,
    iters: Option<usize>,
    timeout: Duration,
    mut sources: Vec<Source<'static>>,
) -> Vec<(&'static str, FuzzStats, Duration)> {
    use tokio::task::JoinSet;
    let par = parallelism();
    let isolate = std::env::var_os("GRAPHIX_FUZZ_INPROC").is_none();
    let bsize = if isolate { batch_size() } else { 1 };
    let mut breakage = BreakageWindow::new();
    let mut checks: JoinSet<(usize, Vec<(String, PoolResult)>, Duration)> =
        JoinSet::new();
    let mut minims: JoinSet<()> = JoinSet::new();
    let mut launched = 0usize;
    let want = |launched: usize| iters.map_or(true, |n| launched < n);
    let mut states: Vec<SourceState> =
        (0..sources.len()).map(|_| SourceState::default()).collect();
    // One generator task per source. Bounded, so a source that outruns
    // the pool blocks in its own task rather than building an unbounded
    // backlog, and each source's seed stream stays strictly sequential
    // inside its own task — a subject is still (source, seed)
    // reproducible.
    let mut gens: Vec<tokio::sync::mpsc::Receiver<String>> = Vec::new();
    let mut gen_tasks: JoinSet<()> = JoinSet::new();
    for (i, src) in sources.iter_mut().enumerate() {
        let (tx, rx) = tokio::sync::mpsc::channel::<String>(GEN_BUFFER);
        for mut next in std::mem::take(&mut src.gens) {
            let tx = tx.clone();
            gen_tasks.spawn(async move {
                loop {
                    if tx.send(next()).await.is_err() {
                        break;
                    }
                }
            });
        }
        drop(tx);
        gens.push(rx);
        // Ring admission likewise. Bounded and try_send'd: an
        // admission is a heuristic — dropping one when the task is
        // behind costs a shape in the mutation ring, whereas
        // stalling the driver costs every source its slots.
        if let Some(mut admit) = src.on_agree.take() {
            let (atx, mut arx) = tokio::sync::mpsc::channel::<(String, bool)>(GEN_BUFFER);
            let novel = states[i].novel.clone();
            gen_tasks.spawn(async move {
                while let Some((prog, ran)) = arx.recv().await {
                    if admit(&prog, ran) {
                        novel.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    }
                }
            });
            states[i].admit = Some(atx);
        }
    }
    let wsum: f64 =
        sources.iter().map(|s| s.weight.max(0.0)).sum::<f64>().max(f64::MIN_POSITIVE);
    // Whichever source is furthest below its target share of projected
    // CPU. With one source this is always 0 and costs nothing.
    let pick = |sources: &[Source<'static>], states: &[SourceState]| -> usize {
        if sources.len() == 1 {
            return 0;
        }
        let done: usize = states.iter().map(|s| s.done).sum();
        let cpu: f64 = states.iter().map(|s| s.cpu.as_secs_f64()).sum();
        let global_mean = if done > 0 { cpu / done as f64 } else { 1.0 };
        let proj: Vec<f64> = states.iter().map(|s| s.projected(global_mean)).collect();
        let total: f64 = proj.iter().sum();
        let mut best = 0;
        let mut best_deficit = f64::NEG_INFINITY;
        for i in 0..sources.len() {
            let target = sources[i].weight.max(0.0) / wsum;
            let actual = if total > 0.0 { proj[i] / total } else { 0.0 };
            let deficit = target - actual;
            if deficit > best_deficit {
                best_deficit = deficit;
                best = i;
            }
        }
        best
    };
    // Spawn ONE child's worth of work: batch-eligible programs
    // accumulate (across calls, via the per-class `pending` buffers)
    // into a `check-batch` child — pure (Exact) and async
    // (FinalValues/Excluded) subjects batch SEPARATELY so a slow
    // async subject can never dump pure subjects onto the individual
    // fallback; a `Never`-class program spawns alone, exactly the
    // pre-batching behavior. Pulls from `next_prog` until something
    // spawns or `iters` runs out (leftover partial batches flush
    // then, so finite runs never strand subjects).
    // A batch is homogeneous in its SOURCE — the buffers live per
    // source — so the child's reported CPU charges to exactly one
    // account. Filling costs nothing extra: generation is synchronous
    // and cheap, so the picked source simply fills its own batch.
    let spawn_next =
        |checks: &mut JoinSet<(usize, Vec<(String, PoolResult)>, Duration)>,
         sources: &mut Vec<Source<'static>>,
         states: &mut Vec<SourceState>,
         gens: &mut Vec<tokio::sync::mpsc::Receiver<String>>,
         launched: &mut usize|
         -> bool {
            loop {
                if !want(*launched) {
                    for (si, st) in states.iter_mut().enumerate() {
                        if st.pending.is_empty() {
                            continue;
                        }
                        let buf = &mut st.pending;
                        let batch = std::mem::take(buf);
                        st.inflight += batch.len();
                        checks.spawn(async move {
                            let (r, cpu) = batch_isolated(batch, timeout).await;
                            (si, r, cpu)
                        });
                        return true;
                    }
                    return true;
                }
                let si = pick(sources, states);
                // Refill from the generator, then pop. If the picked
                // source has nothing ready we do NOT generate inline —
                // that stall is the whole point of moving generation
                // off — we take the neediest source that does have
                // work, and if none does, hand the loop back so a
                // completion can be reaped while generators catch up.
                // Dry: report it and let the caller AWAIT generation,
                // then come back. Never spawn a partial batch to fill
                // the gap — at startup every buffer is empty, so that
                // launched `par` near-empty children which finished
                // instantly and the pool never built up (5 of 24). And
                // never just return: that parked the accumulated
                // programs and shrank the pool by one permanently, which
                // is why it settled at half `par` no matter how much
                // work moved off the driver or how many threads it had.
                let si = match ready_source(si, states, gens) {
                    Some(i) => i,
                    None => return false,
                };
                let prog = match states[si].ready.pop_front() {
                    Some(p) => p,
                    None => return false,
                };
                *launched += 1;
                // Crash forensics: with GRAPHIX_FUZZ_ECHO set, print each
                // program as it dispatches. Mostly superseded by isolation
                // (a crasher now records itself), but kept for debugging
                // the DRIVER process itself.
                if std::env::var_os("GRAPHIX_FUZZ_ECHO").is_some() {
                    eprintln!("FUZZPROG\t{}", prog.replace('\n', "\\n"));
                }
                if !isolate {
                    states[si].inflight += 1;
                    checks.spawn(async move {
                        let res = match check_classified(&prog, timeout).await {
                            (Some(d), _) => PoolResult::Diverge(d),
                            (None, ran) => PoolResult::Agree { ran },
                        };
                        (si, vec![(prog, res)], Duration::ZERO)
                    });
                    return true;
                }
                let st = &mut states[si];
                // EVERY subject batches. Nothing is classified and
                // nothing is held out — the batch child falls back on
                // its own (a subject it cannot resolve comes back
                // `Other` and is re-derived individually, and a batch
                // that dies or stalls re-batches its unreported tail),
                // so an exclusion here buys nothing the fallback would
                // not handle and costs a dedicated process plus a full
                // stdlib compile for ONE subject (Eric, 2026-08-17).
                // The `% individual` counter is what says the fallback
                // rate, and it is the number to watch instead.
                if bsize <= 1 {
                    st.inflight += 1;
                    checks.spawn(async move {
                        let (r, cpu) = check_isolated(&prog, timeout).await;
                        (si, vec![(prog, r)], cpu)
                    });
                    return true;
                }
                let buf = &mut st.pending;
                buf.push(prog);
                if buf.len() >= bsize {
                    let batch = std::mem::take(buf);
                    st.inflight += batch.len();
                    checks.spawn(async move {
                        let (r, cpu) = batch_isolated(batch, timeout).await;
                        (si, r, cpu)
                    });
                    return true;
                }
            }
        };
    // Wait for a generator when nothing is ready. Declining to spawn is
    // correct (we never generate on the driver) but it must not be
    // mistaken for "no work left": at t=0 every buffer is empty, and an
    // unconditional break there exited the campaign having run nothing.
    async fn await_any(
        states: &mut [SourceState],
        gens: &mut [tokio::sync::mpsc::Receiver<String>],
    ) -> bool {
        use tokio::sync::mpsc::error::TryRecvError;
        loop {
            let mut closed = 0;
            for (st, rx) in states.iter_mut().zip(gens.iter_mut()) {
                match rx.try_recv() {
                    Ok(p) => {
                        st.ready.push_back(p);
                        return true;
                    }
                    Err(TryRecvError::Disconnected) => closed += 1,
                    Err(TryRecvError::Empty) => (),
                }
            }
            // Every generator gone means the campaign is genuinely out
            // of work; otherwise wait briefly and look again. This path
            // runs only when the pool has outrun generation, which the
            // buffers make rare.
            if closed == gens.len() {
                return false;
            }
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
    }
    // Keep asking until the slot is genuinely filled. A "not yet" is
    // generators being briefly behind, never a reason to leave a worker
    // idle for the rest of the campaign.
    while want(launched) && checks.len() < par {
        if !spawn_next(&mut checks, &mut sources, &mut states, &mut gens, &mut launched)
            && !await_any(&mut states, &mut gens).await
        {
            break;
        }
    }
    loop {
        tokio::select! {
            biased;
            Some(res) = checks.join_next() => {
                // Refill FIRST, before any result handling: `continue`
                // inside this arm targets the enclosing `loop`, and the
                // excluded-module paths below use it — with the refill at
                // the bottom, every excluded result permanently leaked a
                // worker slot, and after `par` leaks the pool drained and
                // a `forever` campaign exited "done" (soak jul04 item 6:
                // the fuzz campaign bled out nine times overnight).
                // Refill to `par`, waiting on generation as needed: a
                // slot left empty here is a core left idle for the rest
                // of the campaign.
                while want(launched) && checks.len() < par {
                    if !spawn_next(
                        &mut checks, &mut sources, &mut states, &mut gens, &mut launched,
                    ) && !await_any(&mut states, &mut gens).await
                    {
                        break;
                    }
                }
                if let Ok((si, results, cpu)) = res {
                    states[si].cpu += cpu;
                    states[si].inflight = states[si].inflight.saturating_sub(results.len());
                    states[si].done += results.len();
                    for (prog, res) in results {
                    states[si].stats.run += 1;
                    if states[si].stats.run % 1000 == 0 {
                        // Per-source counters, each on its own line, so
                        // one merged log reads exactly like the three
                        // lane logs it replaces — plus the number that
                        // matters for allocation: the share of measured
                        // CPU this source actually drew.
                        let total: f64 =
                            states.iter().map(|s| s.cpu.as_secs_f64()).sum();
                        let pct = if total > 0.0 {
                            (states[si].cpu.as_secs_f64() * 100.0 / total).round() as u64
                        } else {
                            0
                        };
                        states[si].stats.novel = states[si]
                            .novel
                            .load(std::sync::atomic::Ordering::Relaxed);
                        let st = &states[si].stats;
                        eprintln!(
                            "  {}…{} run, {} divergences, {} crashes, {} in corpus, \
                             {} novel shapes, {}% cpu",
                            sources[si].name, st.run, st.divergences, st.crashes,
                            corpus.len(), st.novel, pct
                        );
                    }
                    let finding = match res {
                        PoolResult::Agree { ran } => {
                            if let Some(tx) = &states[si].admit {
                                let _ = tx.try_send((prog.clone(), ran));
                            }
                            false
                        }
                        PoolResult::Crash(status) => {
                            // A HANG in a program touching IO/async
                            // modules is environmental, not a bug: the
                            // child has no resolver, so sys::net
                            // subscribe/rpc block past the outer
                            // deadline by design. Signal deaths and
                            // panics in those programs still record.
                            if status.contains("HANG")
                                && ["rand::", "sys::", "http::"]
                                    .iter()
                                    .any(|m| prog.contains(m))
                            {
                                continue;
                            }
                            states[si].stats.crashes += 1;
                            if corpus.record_crash(&prog, &status) {
                                println!("CRASH — child {status}");
                                println!(
                                    "    program: {}",
                                    prog.replace('\n', "\\n")
                                );
                            }
                            true
                        }
                        PoolResult::Diverge(d) => {
                            // No tier filter here: `check` compares at
                            // the program's own oracle tier (exact for
                            // pure programs, per-epoch finals for
                            // value-deterministic async, nothing for
                            // Excluded), so a Diverge from the child is
                            // a real finding at its tier.
                            states[si].stats.divergences += 1;
                            // Bound concurrent minimizations so a regressed
                            // (everything-diverges) run can't pile up
                            // unboundedly.
                            if minims.len() >= par {
                                let _ = minims.join_next().await;
                            }
                            let corpus = corpus.clone();
                            minims.spawn(async move {
                                // Isolated: a REDUCTION of a benign
                                // divergence can itself be a crasher.
                                // Child death → record unminimized.
                                let min = if isolate {
                                    minimize_isolated(&prog, timeout)
                                        .await
                                        .unwrap_or_else(|| prog.clone())
                                } else {
                                    minimize(&prog, timeout, CAMPAIGN_MINIMIZE_BUDGET).await.0
                                };
                                if corpus.record(&d, &prog, &min) {
                                    println!("DIVERGENCE — {}", d.bisect());
                                    println!("    minimized: {min}");
                                    println!(
                                        "    interp={:?} jit={:?}",
                                        d.interp, d.jit
                                    );
                                }
                            });
                            true
                        }
                    };
                    if breakage.note(finding) {
                        eprintln!(
                            "FATAL fuzz harness: {} of the last {} subjects \
                             produced findings — the environment (or the \
                             build) is broken, not the programs; aborting \
                             instead of flooding the corpus",
                            breakage.findings,
                            BreakageWindow::LEN,
                        );
                        std::process::exit(2);
                    }
                    }
                }
            }
            Some(_) = minims.join_next() => {}
            else => break,
        }
    }
    while minims.join_next().await.is_some() {}
    sources
        .iter()
        .zip(states.into_iter())
        .map(|(src, mut st)| {
            st.stats.novel = st.novel.load(std::sync::atomic::Ordering::Relaxed);
            (src.name, st.stats, st.cpu)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn breakage_window_trips_only_on_sustained_majority() {
        // A clean soak never trips.
        let mut w = BreakageWindow::new();
        for _ in 0..10_000 {
            assert!(!w.note(false));
        }
        // A sub-majority burst of real findings doesn't trip (the worst
        // genuine bug classes are orders of magnitude below this).
        for _ in 0..BreakageWindow::LEN / 2 {
            assert!(!w.note(true));
        }
        for _ in 0..BreakageWindow::LEN {
            assert!(!w.note(false));
        }
        // Environment breaks: every subject is a finding — trips within
        // one window of the breakage.
        assert!((0..BreakageWindow::LEN).any(|_| w.note(true)));
        // Never trips before the window fills (short finite runs).
        let mut w = BreakageWindow::new();
        for _ in 0..BreakageWindow::LEN - 1 {
            assert!(!w.note(true));
        }
        assert!(w.note(true));
    }

    #[test]
    fn addr_getters_are_excluded_tier() {
        // OS-assigned ephemeral ports (port-0 binds) leak into values
        // through the addr getters — soak jul08d recorded
        // `local_addr(server)? <= addr` as a phantom divergence (a coin
        // flip that happened to pass the interp-self-agreement filter).
        for getter in ["listener_addr", "local_addr", "peer_addr"] {
            let prog = format!("{{let a = sys::tcp::{getter}(s)?; a}}");
            assert_eq!(oracle_tier(&prog), OracleTier::Excluded);
        }
        // Plain tcp IO without addr readback keeps final-value coverage.
        assert_eq!(
            oracle_tier("sys::tcp::connect(\"127.0.0.1:5000\")"),
            OracleTier::FinalValues
        );
    }

    #[test]
    fn throttle_is_excluded_tier() {
        // `throttle` gates on Instant::now() and is the only wall-clock
        // reader outside sys:: — soak aug14e hz0 divergence_000000
        // recorded a rate near the cycle time as a phantom divergence
        // (same-engine reruns flip both event count and epoch). It must
        // exclude on its own, with no sys:: in the program.
        assert_eq!(
            oracle_tier("count(throttle(#rate: duration:0.001s, x))"),
            OracleTier::Excluded
        );
        // The other fire-count-sensitive builtins are NOT wall-clock
        // readers: they stay Exact when nothing async is in play.
        assert_eq!(oracle_tier("count(x)"), OracleTier::Exact);
        // A bare marker is broad enough to catch a COMMENT, which would
        // silently un-gate a pin that merely names the builtin — the
        // oracle-tier-comment-scan-aug2026 class. The comment strip must
        // hold for it: dyncall-stale-arg-fired-aug2026/01 discusses
        // throttle in its header and must stay Exact.
        assert_eq!(
            oracle_tier("// a header naming throttle\ncount(x)"),
            OracleTier::Exact
        );
    }

    #[test]
    fn crash_key_sign_fold() {
        // A leading minus folds into the digit run — `-N` and `N` key
        // identically (soak jul05 item 7's family doubling)...
        assert_eq!(crash_key("seq(-9223372036854775808, 4)"), crash_key("seq(0, 4)"));
        // ...while operator variants stay distinct...
        assert_ne!(crash_key("n <= 1"), crash_key("n == 1"));
        // ...and a minus NOT followed by a digit keys literally.
        assert_ne!(crash_key("a - b"), crash_key("a b"));
        assert_eq!(crash_key("x -> y"), crash_key("x -> y"));
    }

    /// Per-shape JIT probes (grown stage-by-stage during the fusion
    /// buildout, `design/distributed_jit.md`). Each pins the JIT
    /// (`Mode::Jit`, `Update::emit_clif` emission) against the
    /// node-walk reference (`Interp`); a program the JIT can't compile
    /// must still produce the right value by not fusing → node-walking.
    /// The `Fuse` ladder additionally asserts (via the per-program
    /// [`FusionStats`]) that fusion actually happened — value agreement
    /// alone can't distinguish "fused correctly" from "silently never
    /// fused" (the class that has cost an investigation every time it
    /// appeared: the C5 freeze gap, the missing `BuiltInLambda`
    /// delegation).
    /// How much fusion a probe demands under the JIT, beyond
    /// value agreement.
    #[derive(Clone, Copy, PartialEq)]
    enum Fuse {
        /// No fusion assertion — the probe pins value agreement only
        /// (known-fallback shapes, flip when their stage lands).
        No,
        /// At least one region fused (`fused > 0`). NOTE: an auxiliary
        /// region (an array literal, a select with `v` as input) can
        /// satisfy this while the construct under test node-walks —
        /// use `Clean` when the whole program is expected to fuse.
        Some,
        /// `fused > 0` AND no real blocker: the only tolerated failed
        /// entries are the attempt-then-recurse ancestor noise ("node
        /// does not emit CLIF" for the Module/Bind wrappers above the
        /// fused region). This is what catches the silent-loss class —
        /// the missing `BuiltInLambda::emit_clif` delegation passed
        /// every `Some`-level probe while no map ever inlined.
        Clean,
    }

    async fn check_jit(code: &str, fuse: Fuse) {
        let t = Duration::from_secs(10);
        let (interp, (jit, stats)) = tokio::join!(
            run_program(code, Mode::Interp, t),
            run_program_with_stats(code, Mode::Jit, t),
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
                // Structural recurse noise, not coverage gaps: the
                // attempt-then-recurse protocol logs the ancestor
                // wrappers ("node does not emit CLIF"), a `mod`
                // statement is structure whose region attempt always
                // refuses (modstmt-fused-no-publish-aug2026 — its
                // CHILDREN fuse per statement; the harness wrap is
                // `{ mod test; test::result }`, so every probe logs
                // one), a function-valued let can never emit by design
                // (the binding node-walks while its call sites fuse),
                // and a lambda call site with fn-typed args dispatches
                // by node-walk while its monomorphic instance body
                // fuses (the `fused > 0` assert above sees that
                // kernel).
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

    /// [`agree`] + "it really fused": the probe is KNOWN to
    /// compile under the direct path, so a `fused == 0` run is a
    /// coverage regression even though every value still agrees.
    async fn agree_fused(code: &str) {
        check_jit(code, Fuse::Some).await
    }

    /// [`agree_fused`] + "and NOTHING legitimately refused":
    /// the whole program is expected to compile into kernels, so any
    /// non-ancestor-noise blocker is a regression. Prefer this for new
    /// probes; audit a probe's full blocker profile before using it
    /// (e.g. a bare-Null `let` legitimately refuses → use `_fused`).
    async fn agree_fused_clean(code: &str) {
        check_jit(code, Fuse::Clean).await
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_scalar_probes() {
        // const + bin region
        agree_fused("{ let x = i64:5; x * i64:3 }").await;
        // multiple scalar lets + nested arithmetic with a Ref read twice
        agree_fused("{ let x = i64:5; let y = i64:2; (x + y) * (x - y) }").await;
        // div-by-zero → value-bottom (Timeout in all three via the
        // taint/guard → boundary pending → no result emitted). Fuses —
        // the bottom is a RUNTIME outcome of the compiled kernel.
        agree_fused("i64:10 / i64:0").await;
        // comparison + strict bool — `a > 3 && a < 10`
        agree_fused("{ let a = i64:7; a > i64:3 && a < i64:10 }").await;
        // float arithmetic
        agree_fused("f64:3.0 + f64:1.0").await;
        // cast then float add. The original probe (`cast<f64>(7) +
        // 1.0`, no `$`) never compiled at all — `cast` returns
        // `[f64, Error]`, so it was a typecheck error in EVERY mode and
        // CompileErr == CompileErr passed silently for the test's whole
        // life (the exact bug class FusionStats exists to catch).
        // Repaired with `$`; it still doesn't fuse — the `cast`
        // CallSite doesn't emit CLIF on the direct path yet ("node
        // does not emit CLIF"), so it node-walks: deliberate fallback.
        agree("cast<f64>(i64:7)$ + f64:1.0").await;
        // Nested block: the INNER block references `outer`, which is
        // external to the inner region — so under the JIT `outer`
        // becomes a scalar KERNEL PARAM (exercising `compile_node`'s Ref
        // arm against `env`-bound params, not just block-lets). The
        // original probe's inner block had ONE expression — a parse
        // error in every mode that `agree` accepted silently
        // (same hollow-CompileErr class as the cast probe above).
        agree_fused("{ let outer = i64:100; { let t = outer - i64:1; t * i64:2 } }")
            .await;
        agree_fused("{ let a = i64:9; { let b = a * i64:2; b + a } }").await;
    }

    /// Stage C4 probes: `?`/`$` unwrap and builtin DynCall emission on
    /// the direct path. The generated sweep can't produce these
    /// constructs (`gen_program` emits neither qop nor builtin calls),
    /// so without explicit probes a C4 regression would be invisible
    /// to the gates.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_qop_dyncall_probes() {
        // Scalar-success `$` — branchless Scalar2 unwrap of the
        // bounds-checked ArrayRef's Nullable<i64>.
        agree_fused("{ let a = [i64:1, i64:2, i64:3]; a[0]$ + a[1]$ }").await;
        // Out-of-bounds → error → bottom in every mode (the unwrap's
        // pending path). Fuses — the bottom is a runtime outcome.
        agree_fused("{ let a = [i64:1]; a[5]$ }").await;
        // MapRef result through `$` — map access + scalar unwrap.
        agree_fused(r#"{ let m = {"a" => i64:7}; m{"a"}$ + i64:1 }"#).await;
        // Value-shape success `$` (duration element) — the Value
        // unwrap arm + a Value-shape kernel return.
        agree_fused("{ let a = [duration:1.s]; a[0]$ }").await;
        // Builtin DynCall, scalar return, string arg.
        agree_fused(r#"{ let s = "hello"; str::len(s) }"#).await;
        // Builtin DynCall inside arithmetic (scalar return feeds Bin).
        agree_fused(r#"{ let s = "hello"; str::len(s) + i64:1 }"#).await;
        // Builtin DynCall with String return (ret_kind 4) + owned
        // string-return kernel boundary.
        agree_fused(r#"{ let s = "abc"; str::to_upper(s) }"#).await;
        // Composite-success `$` (#199): the unwrap must re-box the
        // Value's inline ValArray bits into the composite ABI's
        // `*mut ValArray` — owned-producer and borrowed-Local inners.
        agree_fused("{ let a = [i64:1, i64:2, i64:3]; a[1..]$ }").await;
        agree_fused("{ let a = [i64:1, i64:2, i64:3]; let x = a[1..]; x$ }").await;
        agree_fused("{ let t = [(i64:1, i64:2)]; t[0]$ }").await;
    }

    /// Stage C5 probes: `select` (expression form) emission on the
    /// direct path. Pattern coverage: literal arms, scrutinee binds,
    /// guards (incl. a runtime-bottom guard), null/Nullable type
    /// predicates in BOTH arm orders (the classic path's trivially-true
    /// non-null predicate is order-unsound — the direct path tests
    /// NOT-null explicitly, so these four probes pin order soundness),
    /// variant tag + payload binds, a computed scrutinee (evaluated
    /// once), and a nested select.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_select_probes() {
        // Literal arms + wildcard.
        agree_fused(
            "{ let x = i64:7; select x { i64:0 => i64:100, \
             i64:7 => i64:200, _ => i64:1 } }",
        )
        .await;
        // Arm bind with body arithmetic.
        agree_fused("{ let x = i64:5; select x { i64:0 => i64:100, n => n * i64:2 } }")
            .await;
        // Guard that fails at runtime, then one that passes.
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
        // A guard that BOTTOMS at runtime (div-by-zero) — the arm does
        // not match; the next arm wins.
        agree_fused(
            "{ let x = i64:9; select x { n if n / i64:0 == i64:1 => i64:1, \
             m => m } }",
        )
        .await;
        // Nullable scrutinee, both arm orders, both runtime values —
        // the trivially-true-first-arm order trap.
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
        // Nullable RESULT (Value merge): scalar arm widens, null arm
        // packs (NULL, 0).
        agree_fused(
            "{ let v: [i64, null] = i64:42; select v { i64 as _ => i64:1, \
             null as _ => null } }",
        )
        .await;
        // Variant tag-eq + scalar payload bind.
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
        // Computed scrutinee — must be evaluated exactly once and
        // reused by every arm condition.
        agree_fused(
            "{ let x = i64:5; select (x * i64:2) { i64:10 => i64:1, \
             _ => i64:0 } }",
        )
        .await;
        // Bottom scrutinee with an irrefutable final arm — no value in
        // any mode.
        agree_fused("{ let x = i64:0; select (i64:10 / x) { n => n + i64:1 } }").await;
        // Bool-literal pair (the only typecheckable conditional final
        // arm) — exercises the unreachable miss trap.
        agree_fused("{ let b = true; select b { true => i64:1, false => i64:0 } }").await;
        agree_fused("{ let b = false; select b { true => i64:1, false => i64:0 } }")
            .await;
        // String result merge.
        agree_fused(r#"{ let x = i64:1; select x { i64:0 => "zero", _ => "other" } }"#)
            .await;
        // Nested select.
        agree_fused(
            "{ let x = i64:5; select (select x { i64:0 => i64:1, \
             n => n + i64:1 }) { i64:6 => i64:100, m => m } }",
        )
        .await;
    }

    /// Stage C6 probes: string interpolation (`emit_string_interpolate_node`)
    /// and checked arithmetic
    /// (`emit_checked_arith_node` — NEW coverage, no earlier path
    /// lowered `+?` and friends). The generated sweep produces neither
    /// construct, so without probes a regression would be invisible.
    /// Checked-arith semantics under test: overflow / div-by-zero is a
    /// catchable error VALUE (flows through `is_err` / `$`), never
    /// bottom — the node-walk's `wrap_arith_error` core.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_string_checked_probes() {
        // Interpolation: scalar part rendered via Display.
        agree_fused(r#"{ let x = i64:7; "x is [x]" }"#).await;
        // Mixed string + scalar parts.
        agree_fused(r#"{ let a = "foo"; let b = i64:2; "[a]-[b]!" }"#).await;
        // Pure string concat through interpolation.
        agree_fused(r#"{ let a = "foo"; let b = "bar"; "[a][b]" }"#).await;
        // Float / bool parts (per-prim push helpers).
        agree_fused(r#"{ let f = f64:1.5; let b = true; "f=[f] b=[b]" }"#).await;
        // Interpolated literal scalar (const part).
        agree_fused(r#""n=[i64:42]""#).await;
        // A non-scalar part (Nullable from a[i]) — the restriction: the
        // INTERPOLATION doesn't fuse, node-walks to the right value in
        // every mode. Deliberate fallback, so no fused>0 assertion —
        // a SUB-region (the `a[0]` access) still fuses via the
        // attempt-then-recurse protocol, so fused>0 here would pass
        // without testing what this probe is about.
        agree(r#"{ let a = [i64:1, i64:2]; "e=[a[0]]" }"#).await;
        // Checked add/sub/mul/mod, no overflow — success unwrapped by `$`.
        agree_fused("{ let x = i64:5; (x +? i64:3)$ }").await;
        agree_fused("{ let x = i64:10; (x -? i64:3)$ * (i64:2 *? i64:3)$ }").await;
        agree_fused("{ let x = i64:10; (x %? i64:3)$ }").await;
        // Overflow → the ArithError error VALUE (catchable, not bottom).
        agree_fused("i64:9223372036854775807 +? i64:1").await;
        agree_fused("is_err(i64:9223372036854775807 +? i64:1)").await;
        // `0 /? 0` → error value through is_err — node-walk semantics:
        // checked div0 FLOWS (unlike unchecked div0, which is bottom).
        agree_fused("is_err(i64:0 /? i64:0)").await;
        // Overflow through `$` — the error drops, bottom in every mode.
        agree_fused("(i64:9223372036854775807 +? i64:1)$").await;
        // Checked arith inside a larger expression (select consumes the
        // [T, Error] union).
        agree_fused(
            "{ let x = i64:6; select (x +? i64:1) { i64 as n => n * i64:2, \
             _ => i64:0 } }",
        )
        .await;
        // Checked result interpolated after unwrap — the unwrapped part
        // is a possibly-bottom Scalar2, which the interpolate relay's
        // `.single()` refuses (a may-bottom part has no single value): the
        // INTERPOLATION doesn't fuse, node-walks to the right value in
        // every mode. Deliberate fallback (same sub-region caveat as
        // the Nullable-part probe above).
        agree(r#"{ let x = i64:5; "v=[(x +? i64:1)$]" }"#).await;
    }

    /// Originally (Stage 1) these pinned the non-scalar FALLBACK —
    /// `compile_node` bailed on a tuple/struct let and the program
    /// node-walked. Stage C's composite emission absorbed both shapes:
    /// measured via [`FusionStats`], each program now fuses its whole
    /// body region (`fused == 1`, the same signature as the wholly-
    /// fusing scalar probes — not a sub-region remnant). So the honest
    /// assertion flipped from "falls back" to "fuses"; the probes now
    /// pin composite-let + accessor coverage instead.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_composite_probes() {
        // A tuple-let + tuple accessors.
        agree_fused("{ let t = (i64:1, i64:2); t.0 + t.1 }").await;
        // A struct-let + field accessors.
        agree_fused("{ let s = { a: i64:4, b: i64:5 }; s.a + s.b }").await;
    }

    /// Inline `array::map` emission from the compiler-owned collection
    /// Node through `scaffold::emit_map_loop`. V1 scope:
    /// BORROWED input arrays + single-name callbacks; the last two
    /// probes pin the deliberate V1 fallbacks (owned input array,
    /// destructured callback) as value-agreeing node-walks — flip them
    /// to `agree_fused` when the owned-arg stage / D3 land.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_map_probes() {
        // scalar → scalar
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; array::map(a, |x| x * i64:2) }",
        )
        .await;
        // scalar → tuple (composite out, owned push)
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; array::map(a, |x| (x, x * i64:2)) }",
        )
        .await;
        // composite (tuple) element + accessors in the body
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |p| p.0 + p.1) }",
        )
        .await;
        // scalar → Nullable out (select body, Value-shape push).
        // INTERPRETS since the value-taint-cache storage law
        // (callee-value-taint-passthrough-aug2026): the select's
        // Value-shaped merge inside a loop body has no storage channel
        // and refuses rather than pass taint through. ASPIRE: value
        // residents in slot chains / site blocks restore this.
        agree(
            "{ let a = [i64:1, i64:2]; \
             array::map(a, |x| select x { i64:1 => i64:10, _ => null }) }",
        )
        .await;
        // capture: the body reads an outer scalar (a kernel param
        // under the JIT — BindId-first resolution next to the
        // BindId-bound loop element)
        agree_fused_clean(
            "{ let k = i64:10; let a = [i64:1, i64:2]; \
             array::map(a, |x| x * k) }",
        )
        .await;
        // Nested map-in-map: does NOT inline on either path — the
        // inner CallSite lives in the callback's lambda body, which the
        // static resolution in `typecheck1` never descends into, so the
        // inner collection Node has no resolved callback at emission
        // time. The runtime per-slot machinery carries correctness. Flip to
        // `agree_fused` when static resolution descends
        // into lambda bodies (Stage E callee-prepass territory).
        agree(
            "{ let a = [[i64:1, i64:2], [i64:3]]; \
             array::map(a, |row| array::map(row, |x| x + i64:1)) }",
        )
        .await;
        // string out (push_string)
        agree_fused_clean(r#"{ let a = [i64:1, i64:2]; array::map(a, |x| "v[x]") }"#)
            .await;
        // qop in the body — a may-bottom (Scalar2) field, push_field's
        // RUNTIME bottom-abort seam (no overflow here, so values flow)
        agree_fused_clean("{ let a = [i64:1, i64:2]; array::map(a, |x| (x +? i64:1)$) }")
            .await;
        // OWNED input array (a fresh slice producer) — the scaffold
        // adopts it (owned_input_stack registration: pending exits
        // free it, the normal path drops it after the loop).
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; array::map((a[1..])$, |x| x) }",
        )
        .await;
        // Destructured `|(k, v)|` callback — D3: per-leaf BindId-bound
        // reads off the composite element.
        // P4 ASPIRE (instance-body inlining): call-site shape
        // doesn't fully fuse yet — value agreement only.
        agree("{ let a = [(i64:1, i64:2)]; array::map(a, |(k, v)| k + v) }").await;
    }

    /// Stage D2 probes: inline `array::filter` emission on the direct
    /// path (`FilterImpl::emit_clif` → `scaffold::emit_filter_loop`).
    /// Same V1 scope as the map probes (borrowed input, single-name
    /// callback), plus filter's own contract probe: a may-bottom
    /// predicate must DE-FUSE at build time (node-walk in every mode,
    /// values agree) — there is no runtime keep-vs-drop answer for a
    /// bottom predicate, so runtime-abort would diverge from the
    /// canonical node-walk.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_filter_probes() {
        // scalar element, comparison predicate
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter(a, |x| x > i64:2) }",
        )
        .await;
        // bool element, bare-ref predicate
        agree_fused_clean("{ let a = [true, false, true]; array::filter(a, |x| x) }")
            .await;
        // composite (tuple) element + accessors in the predicate —
        // EXCEEDS classic: its lowering requires a register-scalar element
        // for single-name callbacks, the direct path binds composites
        // (keep MOVES the element, not-keep takes the drop_block)
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::filter(a, |p| p.0 > p.1) }",
        )
        .await;
        // capture: the predicate reads an outer scalar
        agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter(a, |x| x > k) }",
        )
        .await;
        // select in the predicate
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter(a, |x| select x { i64:2 => false, _ => true }) }",
        )
        .await;
        // STATICALLY may-bottom predicate (integer div by the element
        // → Scalar2 regardless of the runtime values): FilterImpl Errs
        // → the kernel de-fuses at build and the region node-walks to
        // a REAL value all modes agree on. No zero in the array — a
        // value-blind Timeout==Timeout agreement (the bottom case,
        // next probe) can't catch a wrong de-fuse, this can.
        agree(
            "{ let a = [i64:1, i64:5, i64:20]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // ...and with an actual 0: the pred slot for that element is
        // bottom, so filter's output NEVER fires — every mode times
        // out. Pins the canonical blocking semantics (a runtime
        // keep-vs-drop guess in a fused kernel would produce a value
        // here and diverge).
        agree(
            "{ let a = [i64:0, i64:1, i64:5]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // string element — outside bind_elem's V1 shapes (#150) →
        // Ok(None) → node-walk. Flip when string HOF elements land.
        agree(r#"{ let a = ["aa", "b"]; array::filter(a, |s| s == "aa") }"#).await;
        // OWNED input array (fresh slice producer) — adopted by the
        // scaffold, same as the map probe.
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter((a[1..])$, |x| x > i64:1) }",
        )
        .await;
        // Destructured `|(k, v)|` predicate — D3 (the kept element is
        // still the whole tuple)
        // P4 ASPIRE (instance-body inlining): call-site shape
        // doesn't fully fuse yet — value agreement only.
        agree("{ let a = [(i64:1, i64:2)]; array::filter(a, |(k, v)| k < v) }").await;
    }

    /// Inline `array::fold` emission from the compiler-owned collection
    /// Node through `scaffold::emit_fold_loop`. The
    /// accumulator threads through the loop as a register Variable
    /// (BindId-bound — the acc and elem resolve BindId-first next to
    /// any same-named outer capture). Fold's contract probes: a
    /// may-bottom INIT or BODY de-fuses at BUILD time — the plan's
    /// explicit parity fixture ("a may-bottom fold body must de-fuse,
    /// not runtime-abort").
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_fold_probes() {
        // scalar sum
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::fold(a, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // computed init + capture in the body
        agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::fold(a, k * i64:10, |acc, x| acc + x * k) }",
        )
        .await;
        // composite (tuple) element + accessors — EXCEEDS classic
        // (its lowering requires a register-scalar element for single-name
        // callbacks)
        agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::fold(a, i64:0, |acc, p| acc + p.0 * p.1) }",
        )
        .await;
        // select in the body (acc threading through arms)
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold(a, i64:0, |acc, x| \
               select x { i64:2 => acc, _ => acc + x }) }",
        )
        .await;
        // outer binding with the SAME NAME as the acc, used as the
        // INIT — the kernel param `acc` (outer BindId) feeds the init
        // read while the loop's acc bind (lambda BindId) shadows it
        // for the body's reads; BindId-first resolution keeps them
        // straight
        agree_fused_clean(
            "{ let acc = i64:100; let a = [i64:1, i64:2]; \
             array::fold(a, acc, |acc, x| acc + x) }",
        )
        .await;
        // HOF callsite in OPERAND position (under the `+`) — pre-#204
        // neither path statically resolved it (static resolution only
        // descended the Module/Block/Bind/CallSite spine). Now the
        // full-position traversal resolves it and the whole block
        // fuses as one region.
        agree_fused_clean(
            "{ let k = i64:100; let a = [i64:1, i64:2]; \
             k + array::fold(a, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // #204 position coverage: HOF in a SELECT ARM...
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; let x = i64:1; \
             select x { \
               i64:1 => array::fold(a, i64:0, |acc, y| acc + y), \
               _ => i64:0 } }",
        )
        .await;
        // ...and as an ARRAY-LITERAL ELEMENT.
        agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             [array::fold(a, i64:0, |acc, x| acc + x), i64:5] }",
        )
        .await;
        // STATICALLY may-bottom BODY (div by the element, no zero
        // present): de-fuses at build, node-walks to a real value all
        // modes agree on — the plan's explicit fold parity fixture.
        agree(
            "{ let a = [i64:1, i64:2]; \
             array::fold(a, i64:100, |acc, x| acc / x) }",
        )
        .await;
        // STATICALLY may-bottom INIT (same contract, the init seam)
        agree(
            "{ let n = i64:2; let a = [i64:1, i64:2]; \
             array::fold(a, i64:10 / n, |acc, x| acc + x) }",
        )
        .await;
        // string accumulator — not a register scalar → Ok(None) →
        // node-walk. Flip if/when value-shape accumulators land.
        agree(r#"{ let a = [i64:1, i64:2]; array::fold(a, "", |acc, x| "[acc][x]") }"#)
            .await;
        // OWNED input array (fresh slice producer) — adopted by the
        // scaffold.
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold((a[1..])$, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // Destructured `|acc, (k, v)|` callback — D3 (acc + leaves
        // all BindId-bound in the loop scope)
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2)]; \
             array::fold(a, i64:0, |acc, (k, v)| acc + k * v) }",
        )
        .await;
    }

    /// Stage D2 probes: inline `array::flat_map` emission on the
    /// direct path (`FlatMapImpl::emit_clif` →
    /// `scaffold::emit_flat_map_loop`). The body must be the
    /// array-returning shape of the `['b, Array<'b>]` callback union
    /// and hands the scaffold an OWNED array (Borrowed body sources
    /// are refcount-cloned per iteration — probed below).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_flat_map_probes() {
        // scalar element → fresh array body
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x, x * i64:10]) }",
        )
        .await;
        // composite (tuple) element flattened to its fields —
        // EXCEEDS classic (register-scalar-element gate there)
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::flat_map(a, |p| [p.0, p.1]) }",
        )
        .await;
        // capture in the body
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let k = i64:2; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x * k]) }",
        )
        .await;
        // BORROWED body source: the body is a Ref to an outer array,
        // so the scaffold's extend would consume the env's value —
        // `ensure_owned_composite_src` clones it per iteration
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let b = [i64:9]; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| b) }",
        )
        .await;
        // bare-element body — the OTHER branch of the callback union;
        // not Array-typed → Ok(None) → node-walk (classic parity)
        agree("{ let a = [i64:1, i64:2]; array::flat_map(a, |x| x) }").await;
        // OWNED input array — adopted by the scaffold
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::flat_map((a[1..])$, |x| [x]) }",
        )
        .await;
        // Destructured callback — D3
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2)]; \
             array::flat_map(a, |(k, v)| [k, v]) }",
        )
        .await;
    }

    /// Stage E probes: cross-kernel lambda calls on the direct path —
    /// `try_fuse`'s analysis discovers statically-resolved lambda call
    /// sites (full-coverage `for_each_node` walk), builds each callee
    /// kernel via the shared `build_lambda_kernel` (node body,
    /// classic-proven — including self-recursion and the tail
    /// rebind-and-jump), and `CallSite::emit_clif` emits a CLIF `call`
    /// with kind-grouped args + closure-converted captures.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_lambda_call_probes() {
        // simple scalar call
        agree_fused_clean("{ let f = |x: i64| x * i64:2; f(i64:21) }").await;
        // two call sites, one callee kernel
        agree_fused_clean("{ let f = |x: i64| x + i64:1; f(i64:1) + f(i64:2) }").await;
        // scalar capture (closure conversion: `k` rides as a trailing
        // kernel arg, marshalled from the calling kernel's env)
        agree_fused_clean("{ let k = i64:10; let f = |x: i64| x * k; f(i64:4) }").await;
        // f64 arg + return
        agree_fused_clean("{ let f = |x: f64| x * f64:2.5; f(f64:4.0) }").await;
        // composite (array) arg — the literal is an OWNED caller-side
        // arg, dropped after the call (the callee clones on entry)
        agree_fused_clean(
            "{ let f = |a: Array<i64>| a[i64:0]$ + a[i64:1]$; \
             f([i64:1, i64:2, i64:3]) }",
        )
        .await;
        // callee body containing a HOF call: the fold inside the
        // lambda body never statically resolves (#203 — the same gap
        // on both paths), so the callee kernel can't build and the
        // call node-walks. Flip when #203 lands.
        agree(
            "{ let f = |a: Array<i64>| \
               array::fold(a, i64:0, |acc, x| acc + x); \
             f([i64:1, i64:2, i64:3]) }",
        )
        .await;
        // labeled arg — used explicitly and via its default
        agree_fused_clean(
            "{ let f = |#k: i64 = i64:5, x: i64| x + k; \
             f(#k: i64:3, i64:2) + f(i64:2) }",
        )
        .await;
        // Nullable (value-shape) return from a select body — blocked
        // by #205 (pre-existing: the kernel-return emission routes on
        // the un-normalized select arm-union type; unreachable
        // classically because classic's planner never built this
        // kernel). Values agree via node-walk; flip to
        // agree_fused_clean when #205 lands.
        agree(
            "{ let f = |x: i64| -> [i64, null] \
               select x { i64:0 => null, _ => x }; \
             f(i64:5) }",
        )
        .await;
        // self-recursion (E3): the body's self-reference is excluded
        // from the captures scan (a rec binding's env type is a
        // TVar-wrapped Fn the scan can't freeze — recursive lambdas
        // never built because of it), and the non-tail self call
        // lowers to a CLIF `call` against the kernel's own FuncId —
        // real native recursion.
        agree_fused_clean(
            "{ let rec f = |n: i64| -> i64 \
               select n { i64:0 => i64:0, _ => n + f(n - i64:1) }; \
             f(i64:10) }",
        )
        .await;
        // double recursion (two self-calls per arm, operand position)
        agree_fused_clean(
            "{ let rec fib = |n: i64| -> i64 \
               select n { i64:0 => i64:0, i64:1 => i64:1, \
               _ => fib(n - i64:1) + fib(n - i64:2) }; \
             fib(i64:15) }",
        )
        .await;
        // tail recursion (E3): `body_has_self_tail_call` detects the
        // tail-position self call (BindId-matched), the kernel gets
        // `has_tail_loop`, and the tail-call emission compiles to a
        // rebind-and-jump — a native loop, constant stack. Depth kept
        // stack-safe for the NODE-WALK (each recursive call nests
        // native update frames — 50k overflows the interp); the
        // fused-only deep probe below runs the same loop at 5M.
        agree_fused_clean(
            "{ let rec lp = |n: i64, acc: i64| -> i64 \
               select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) }; \
             lp(i64:500, i64:0) }",
        )
        .await;
        // tail recursion with a CAPTURE: `tail_call_slots` covers
        // every kernel param (it doubles as the runtime arg layout)
        // but the TailCall rebinds only the leading formals — the
        // capture slot stays bound, loop-invariant.
        agree_fused_clean(
            "{ let k = i64:3; let rec f = |n: i64| -> i64 \
               select n { i64:0 => k, _ => f(n - i64:1) }; \
             f(i64:4) }",
        )
        .await;
        // shadowed lambda name (#206): f2's body calls the OUTER f.
        // `finish_kernel` registers f2's own name in known_fns before
        // the body emits, and name-only resolution matched the entry —
        // the kernel called ITSELF (infinite native self-call, stack
        // overflow). `KnownFusedFn::self_bind` now refuses the
        // mismatched binding: f2 de-fuses, the call node-walks, every
        // mode agrees on 8. Stays un-fused until known_fns re-keys by
        // BindId (the #203 follow-up).
        agree(
            "{ let f = |x: i64| -> i64 x + i64:1; \
             let f = |n: i64| -> i64 f(n) * i64:2; f(i64:3) }",
        )
        .await;
        // lambda call INSIDE a HOF callback body: the callback's body
        // isn't part of the region walk, so the inner site isn't
        // discovered — the map kernel Errs and the whole construct
        // node-walks (correct degradation). Extend discovery into HOF
        // analysis_pred bodies to flip this.
        agree(
            "{ let f = |x: i64| x * i64:2; let a = [i64:1, i64:2]; \
             array::map(a, |x| f(x)) }",
        )
        .await;
    }

    /// E3's depth dividend, fused-only: a 5M-deep tail recursion is
    /// ONLY runnable as the compiled rebind-and-jump loop — the
    /// node-walk nests a native update frame per call and overflows
    /// its stack around ~50k, so the interp mode (and the classic Jit
    /// mode, whose planner never carves this region and would
    /// node-walk it) is deliberately absent. The value is asserted
    /// against the closed form, and `fused > 0` pins that the loop
    /// actually compiled — a de-fuse here would BE the stack
    /// overflow, not a silent fallback.
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
        // sum 1..=5_000_000
        let expected = Outcome::single(Value::I64(12_500_002_500_000));
        assert!(
            out.agrees_with(&expected),
            "deep tail loop produced {out:?}, expected {expected:?}"
        );
    }

    /// Depth is bounded by memory, not a counter: a 2,000,000-deep
    /// NON-tail recursion runs as native recursion in the kernel and
    /// re-enters through the spill thunk whenever the remaining stack
    /// is inside the red zone (`graphix_stack_check`/
    /// `graphix_grow_stack`, design/recursive_activations.md §4b). The
    /// former 256 limit bottomed this at n = 256.
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

    /// D3 probes: destructured `|(k, v)|` callbacks — per-leaf
    /// BindId-bound reads off the owned composite element
    /// (`HofElem::leaves` via `scalar_leaves`).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_destructure_probes() {
        // mixed-prim leaves (i64, f64), f64 result
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, f64:2.5), (i64:3, f64:0.5)]; \
             array::map(a, |(k, v)| v) }",
        )
        .await;
        // sparse leaves: `_` positions get no bind (and no read)
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |(k, _)| k * i64:10) }",
        )
        .await;
        // find with a destructured predicate — the result is the
        // whole matched tuple
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |(k, v)| k > v) }",
        )
        .await;
        // 3-leaf tuple through fold
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2, i64:3), (i64:4, i64:5, i64:6)]; \
             array::fold(a, i64:0, |acc, (x, y, z)| acc + x * y + z) }",
        )
        .await;
        // composite leaf — outside the register-scalar V1 →
        // Ok(None) → node-walk (flip when composite leaves land)
        agree(
            "{ let a = [((i64:1, i64:2), i64:3)]; \
             array::map(a, |(p, x)| x) }",
        )
        .await;
    }

    /// Stage D2 probes: inline `array::filter_map` emission
    /// (`FilterMapImpl::emit_clif` → `scaffold::emit_filter_map_loop`,
    /// scalar in/out — the body's `Nullable<out>` Value-shape result
    /// is collected when non-null).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_filter_map_probes() {
        // select-bodied Nullable: keep evens doubled
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // capture in the body
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter_map(a, |x| \
               select x { i64:2 => x * k, _ => null }) }",
        )
        .await;
        // composite element — outside the scalar-only scaffold →
        // Ok(None) → node-walk (classic parity; widen with #150)
        agree(
            "{ let a = [(i64:1, i64:2)]; \
             array::filter_map(a, |p| \
               select p.0 { i64:1 => p.1, _ => null }) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter_map((a[1..])$, |x| \
               select x { i64:2 => x, _ => null }) }",
        )
        .await;
    }

    /// Stage D2 probes: inline `array::find` emission
    /// (`FindImpl::emit_clif` → `scaffold::emit_find_loop`, early
    /// exit, `Nullable<elem>` Value-shape result). The may-bottom
    /// predicate de-fuses at build, same contract as filter.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_find_probes() {
        // scalar element, found
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:5, i64:3]; \
             array::find(a, |x| x > i64:2) }",
        )
        .await;
        // scalar element, NOT found (null result)
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree("{ let a = [i64:1, i64:2]; array::find(a, |x| x > i64:9) }").await;
        // composite (tuple) element + accessor predicate — the found
        // element is consumed into the Nullable result, not-matched
        // ones drop per iteration. EXCEEDS classic for single-name
        // callbacks.
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |p| p.0 > p.1) }",
        )
        .await;
        // may-bottom predicate — build-time de-fuse, runtime-clean
        agree(
            "{ let a = [i64:1, i64:5]; \
             array::find(a, |x| i64:10 / x > i64:4) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold (the early-exit
        // edges and the not-found edge all route through the shared
        // exit where the input drops)
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find((a[1..])$, |x| x > i64:1) }",
        )
        .await;
    }

    /// Stage D2 probes: inline `array::find_map` emission
    /// (`FindMapImpl::emit_clif` → `scaffold::emit_find_map_loop` —
    /// the first non-null body pair IS the kernel result, so a
    /// Borrowed body source is refcount-cloned per the owned-pair
    /// contract).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_find_map_probes() {
        // found: first even, doubled
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // not found → null
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x, _ => null }) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find_map((a[1..])$, |x| \
               select x { i64:2 => x, _ => null }) }",
        )
        .await;
    }

    /// Owned-input widening probes: fresh-producer arrays (literals,
    /// slices, inlined-HOF results) feed the loop scaffolds directly —
    /// the scaffold adopts them (`owned_input_stack`: pending exits
    /// free them, the normal path drops after the loop). The pipeline
    /// probes are the composition payoff: with #204 covering arg
    /// positions and owned inputs adopted, HOF-of-HOF args fuse as
    /// MULTI-LOOP single kernels.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_owned_input_probes() {
        // array literal as the DIRECT argument
        agree_fused_clean("array::map([i64:1, i64:2, i64:3], |x| x * i64:2)").await;
        // PIPELINE: filter over an inlined map — two loops, one kernel
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter(array::map(a, |x| x * i64:2), |x| x > i64:2) }",
        )
        .await;
        // PIPELINE: fold over an inlined map
        agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold(array::map(a, |x| x * x), i64:0, |acc, x| acc + x) }",
        )
        .await;
        // PIPELINE: find over an inlined filter (early exit consumes
        // an adopted intermediate)
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::find(array::filter(a, |x| x % i64:2 == i64:0), \
               |x| x > i64:2) }",
        )
        .await;
        // PIPELINE feeding init's output into flat_map
        // P4 ASPIRE (instance-body inlining): this call-site
        // shape doesn't fully fuse yet — value agreement only.
        agree("array::flat_map(array::init(i64:3, |i| i), |x| [x, x])").await;
        // PENDING path through an adopted input: the outer map's body
        // bottom-aborts mid-loop (i64::MAX overflow via `+?` then `$`)
        // while the inner map's result is adopted — the pending
        // cleanup must free it via `owned_input_stack` (a wrong-
        // destructor or double-free would crash the JIT mode; the
        // canonical outcome is a blocked output, Timeout in every
        // mode).
        agree(
            "{ let a = [i64:9223372036854775807, i64:1]; \
             array::map(array::map(a, |x| x), |x| (x +? i64:1)$) }",
        )
        .await;
    }

    /// Stage D2 probes: inline `array::init` emission (Init's own
    /// `Apply::emit_clif` → `scaffold::emit_init_loop` — the index
    /// param binds the loop counter Variable itself; the body result
    /// pushes via `push_field`, the runtime bottom-abort seam).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn jit_init_probes() {
        // scalar body
        agree_fused_clean("array::init(i64:4, |i| i * i)").await;
        // composite (tuple) body
        agree_fused_clean("array::init(i64:3, |i| (i, i + i64:1))").await;
        // capture in the body
        agree_fused_clean("{ let k = i64:10; array::init(i64:3, |i| i * k) }").await;
        // computed n with a capture
        agree_fused_clean("{ let n = i64:2; array::init(n + i64:1, |i| i) }").await;
        // negative n clamps to the empty array (the scaffold's
        // node-walk-parity clamp)
        agree_fused_clean("array::init(i64:0 - i64:2, |i| i)").await;
        // may-bottom n (div by a binding) — build-time de-fuse,
        // runtime-clean
        agree("{ let d = i64:2; array::init(i64:4 / d, |i| i) }").await;
    }

    /// Broad differential sweep: the type-directed generator produces
    /// scalar / tuple / array / select programs. For EVERY one, `Interp`
    /// (node-walk reference) and `Jit` (the new `compile_node`
    /// path, falling back to node-walk on any unsupported shape) must
    /// agree. A scalar program exercises `compile_node`; a non-scalar one
    /// exercises the fallback. Deterministic seed → reproducible.
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
            // Skip nondeterministic programs (a value whose Display
            // embeds a process-global id, etc.): re-run interp and only
            // assert when interp agrees with itself — mirrors the
            // oracle's double-run guard.
            if !interp.agrees_with(&direct) {
                // A TIMEOUT on either side is the budget talking, not
                // the backend: this test runs inside `cargo test
                // --workspace`, where the whole suite is ~13x slower
                // than a solo run (550s vs 42s), so a heavy subject
                // near the 10s budget completes in one mode and times
                // out in the other and the assert reads it as a
                // divergence. Solo runs never showed it. Re-check at 4x
                // before believing any disagreement involving a
                // Timeout; the fuzz oracle escalates the same way.
                if matches!(interp, Outcome::Timeout)
                    || matches!(direct, Outcome::Timeout)
                {
                    let big = t * 4;
                    let (i2, j2) = tokio::join!(
                        run_program(&code, Mode::Interp, big),
                        run_program(&code, Mode::Jit, big),
                    );
                    if i2.agrees_with(&j2)
                        || matches!(i2, Outcome::Timeout)
                        || matches!(j2, Outcome::Timeout)
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
        // The live coverage number — visible in every `--nocapture`
        // run, no instrumentation ritual required. Budget skips are
        // reported beside it: a suite run that quietly stopped
        // COMPARING its subjects would otherwise look identical to a
        // clean one.
        eprintln!(
            "sweep: {fused} regions fused across 120 programs              ({budget_skipped} skipped on budget)"
        );
    }

    /// Every scheduled hand seed (Phase 3.1) agrees across modes at
    /// trace strength — the injection driver's permanent gate: the
    /// D4 contract, per-epoch anchoring, the connect lifts
    /// (scalar/array/string/struct), cross-cycle builtins over
    /// injected streams, and cap determinism under schedules.
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
                    d.interp, d.jit
                );
            }
        }
    }
}

/// Stage-2.1 gate probes for the runtime trace primitives
/// (`GXHandle::{trace_start, trace_wait_idle}`) — the foundation the
/// per-cycle trace oracle (Phase 2.2) is built on. Each probe pins a
/// property the oracle will depend on:
///   - the compile race is dead (a value emitted during the compile
///     cycle is IN the trace, unlike `wait_result_or_idle`),
///   - a bottom program resolves instantly with an anchor-only trace,
///   - the D4 injection contract (never-gated root input) works under
///     BOTH modes with identical relative traces AND fuses,
///   - a runaway `<-` program is cut deterministically by the cycle cap,
///   - segments drain (each epoch's wait returns only its own events).
#[cfg(test)]
mod trace_probes {
    use super::*;
    use graphix_compiler::{Scope, expr::ModPath};
    use graphix_rt::{TraceEvent, TraceSegment};

    /// Drive one traced run: `trace_start` → compile (`prelude`
    /// top-level decls, then the standard `{ mod test; test::result }`
    /// wrap over `program` in the VFS) → `trace_wait_idle` (epoch 0),
    /// then per epoch set every named root input and wait again.
    /// Returns one segment per epoch and the program's own
    /// [`FusionStats`] delta.
    ///
    /// Input decls go in `prelude` (top level of the compile text), NOT
    /// inside the module: a `{ … }` wrap compiles under an anonymous
    /// `do<ExprId>` scope, so module-internal bindings are not reachable
    /// by name from root — root-level decls are, and the module body
    /// still sees them lexically.
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
        let ctx =
            init_with_flags_and_setup(tx, REGISTER, vec![resolver], mode.flags(), |_| {})
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
        // `comp` and the refs hold GXHandle clones — they must drop
        // BEFORE the channel receiver, or a still-running (runaway)
        // program spams "could not send batch" into a closed channel
        // until the last handle finally drops.
        drop(refs);
        drop(comp);
        ctx.shutdown().await;
        drop(rx);
        (segs, stats)
    }

    /// Project a segment onto mode-comparable data: each event as
    /// (cycle relative to the segment's first event, value), with
    /// `None` for the `Compiled` anchor. ExprIds are process-local and
    /// deliberately dropped — cross-mode comparison is over relative
    /// pacing and values only.
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

    /// The compile race is dead: a synchronous program's value is
    /// emitted during the compile cycle — before any wait could
    /// register — and it is IN the trace, at offset 0 from the
    /// `Compiled` anchor, under both modes.
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

    /// A trace whose observable is a FIRST-CLASS FUNCTION value is
    /// compile-stable: fn values are LambdaDefs compared by minted id
    /// (never equal across two compiles), normalized to the lambda's
    /// printed source by `trace::normalize` (the 2026-07-16 selfcheck
    /// find — this program flaked under BOTH modes).
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

    /// A bottom program (div-by-zero) resolves instantly with an
    /// anchor-only trace — the empty-trace agreement that replaces the
    /// old full-timeout sleep for bottom programs.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn trace_bottom_is_instant_and_empty() {
        let prog = "let result = i64:1 / i64:0";
        let (i, _) = drive_traced(Mode::Interp, "", prog, 512, 64, &[]).await;
        let (j, _) = drive_traced(Mode::Jit, "", prog, 512, 64, &[]).await;
        let want = vec![vec![(0, None)]];
        assert_eq!(shapes(&i), want, "interp trace");
        assert_eq!(shapes(&j), want, "jit trace");
    }

    /// The D4 injection contract: a root-level `let in0: T = default`
    /// plus `in0 <- never(default)` is a settable region INPUT — the
    /// consuming region fuses under Jit (the `<-` marks the binding
    /// unstable, so fusion binds a kernel param instead of
    /// const-folding the default), each epoch's set flows through, the
    /// relative traces agree across modes, and segments DRAIN (each
    /// wait returns only its own epoch's events).
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
        // Epoch 0: one `Compiled` anchor per top-level expr (the two
        // prelude decls + the module block) plus the default flowing
        // through, all in the init cycle.
        assert_eq!(
            si[0],
            vec![(0, None), (0, None), (0, None), (0, Some(Value::I64(0)))]
        );
        // Injection epochs: result and the input-ref's own echo, same
        // cycle, nothing carried over from the previous epoch. Values
        // only — the in-cycle event order is pinned by the cross-mode
        // eq above, not re-asserted here.
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

    /// A runaway `x <- x + 1` is cut by the cycle cap at a point that
    /// is a pure function of the program's own event stream: same
    /// trace twice under one mode, and the same trace across modes
    /// (values AND relative pacing — the lifted connect counter's
    /// per-cycle firing under the JIT vs the node-walk).
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

    /// An eventless spinner — a self-connect loop whose only traced
    /// output is permanently bottom — must hit the WORKED-cycle cap
    /// and resolve the waiter. The emission-only meter left it
    /// uncapped and `trace_wait_idle` hanging until the harness's
    /// outer deadline (the jul22k reactive HANG-noise class); the
    /// worked-cycle deadline turns the class into an exact
    /// comparison: both modes cap at the same program-driven cycle
    /// count with identical traces.
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

    /// Two subjects, same module NAME, incompatible contents.
    ///
    /// The batch child reuses one warmed runtime across subjects, so if
    /// a subject's files leaked into the next one's compile the second
    /// would fail to compile and come back `Other`. Both must be
    /// `Agree`.
    ///
    /// This cannot be checked by the differential oracle: an aliased
    /// module is inherited by BOTH engines, so they would agree with
    /// each other on the wrong program. The verdict is the observable.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn batched_files_do_not_alias() {
        let a = "{ mod m0; m0::k0 }\n// file-v1: m0.gx\nlet k0 = i64:1\n";
        let b = "{ mod m0; str::len(m0::k0) }\n// file-v1: m0.gx\nlet k0 = \"xy\"\n";
        // Both orderings: aliasing in either direction must show up.
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

    /// A batched subject must reach the same verdict as the individual
    /// path — INCLUDING `ran`, which is the half that catches drift.
    ///
    /// The batch child derives its compile inputs, and for a while it
    /// derived them separately from the individual path and fell
    /// behind it three times: the wrap lost `use super::*` (so every
    /// subject with injected inputs failed to compile), no `mod`
    /// declarations were emitted (so every subject carrying an aux
    /// file did the same), and the callable header was never parsed.
    /// The first two were INVISIBLE to an agreement check — a
    /// CompileErr agrees with a CompileErr, so the subjects reported a
    /// clean agreement having never run at all, for five days.
    /// `batched_files_do_not_alias` above passes under that bug.
    ///
    /// So this asserts on the FULL verdict against the individual
    /// path, which pins the drift class rather than its three
    /// instances.
    ///
    /// One gap it does NOT close: that the batch child drives BOTH
    /// callable routes. A program satisfying the callable contract
    /// agrees across routes by construction — that is what the
    /// contract says — so no unit test can tell a child that drove
    /// both from one that drove the in-language route twice. The
    /// fuzzer is what covers it: a silently skipped dispatch route
    /// shows up as `Pair::EngineDispatch` and `Pair::Route` findings
    /// drying up.
    #[tokio::test]
    async fn batch_verdict_matches_individual() {
        // `want_ran`: the ring-admission bar this subject must reach.
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
            // ABSOLUTE, not just pairwise. Without this the check is
            // vacuous — the two paths now share one derivation, so
            // breaking it breaks them identically and they go on
            // matching each other. That is the same blind spot the
            // twin oracle exists to cover. `ran` is the ring-admission
            // bar: true for a subject that ran to a value, and false
            // for a callable one, which is deliberately never admitted.
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
