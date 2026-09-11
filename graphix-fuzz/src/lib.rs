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
pub mod mutate;
pub mod schedule;
pub mod trace;
pub mod typemorph;

use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use bytes::Bytes;
use graphix_compiler::{
    CFlag, FusionStats, Scope,
    env::Env,
    expr::{Expr, VfsEntry, VfsResolver},
};
use graphix_package::Package;
use graphix_package_core::testing::{
    TestCtx, init_session_with_setup, init_with_flags_and_setup,
};
use graphix_rt::{GXEvent, NoExt, RegistrationImage};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_core::path::Path;
use std::{future, time::Duration};
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
    /// Neither quiesced nor hit the trace budget within the wall-clock
    /// backstop, or the stack budget aborted it first: both are
    /// containment outside the language, one outcome.
    Timeout,
}

/// Strip tvar numbers (`'_6070` -> `'_N`) and abstract-type ids
/// (`<abstract#12>` -> `<abstract#N>`) so fresh-counter drift between
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
            (Timeout, Timeout) => true,
            (Timeout, Trace(_)) | (Trace(_), Timeout) => {
                !self.has_events() && !other.has_events()
            }
            _ => false,
        }
    }

    /// A trace with at least one event; a Timeout beside such a trace is
    /// a value divergence, beside an eventless one only the accepted
    /// liveness difference in the backends' runaway handling.
    pub fn has_events(&self) -> bool {
        matches!(self, Outcome::Trace(t) if t.epochs.iter().any(|e| !e.events.is_empty()))
    }

    /// [`Self::agrees_with`] at a chosen [`OracleTier`]: Exact compares
    /// whole traces; FinalValues compares per-epoch settled values
    /// ([`trace::Trace::agrees_final`]); non-Trace pairs follow the exact
    /// rules at every tier.
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

/// Run `code` (a wrapper: optional headers + expression body, see
/// [`Subject`]) under `mode`, returning the per-cycle trace of
/// everything `result` emitted across every epoch, or why nothing ran.
/// A fresh `ExecCtx` is created per call so fusion state and the
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
    /// The module the body is installed as. Unique per subject inside a
    /// batch child (one warmed runtime serves many subjects); `test` on
    /// the individual path.
    pub modname: String,
}

impl Subject {
    /// `Err` is the message for a `CompileErr` outcome: a malformed
    /// header is a compile-class reject in every mode, so it agrees
    /// everywhere and surfaces in `gen-check`.
    pub fn parse(code: &str, modname: &str) -> Result<Subject, String> {
        let (sched, body) = schedule::Schedule::parse(code)
            .map_err(|e| format!("schedule header: {e}"))?;
        let (spec, body) = callable::CallSpec::parse(body)
            .map_err(|e| format!("callable header: {e}"))?;
        let (body, files) =
            files::split(&body).map_err(|e| format!("file section: {e}"))?;
        // a submodule sees nothing of its parent implicitly; `use
        // super::*` brings in the injected inputs and the aux `mod`s
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
    /// declarations sit at the top level, where `compile_ref_by_name`
    /// reaches them from root; the aux `mod`s precede the callable
    /// declarations, which reference into the handler's module.
    pub fn compile_text(&self) -> String {
        let Subject { sched, spec, mods, modname, .. } = self;
        let cdecls = spec.as_ref().map(|c| c.decls()).unwrap_or_default();
        format!("{}{mods}{cdecls}{{ mod {modname}; {modname}::result }}", sched.decls())
    }
}

/// Compile `code` under `mode` without driving it: `None` = compiled
/// clean, `Some(error)` = reject (or the runtime failed to init).
/// `gen-check`'s primitive.
pub async fn compile_program(code: &str, mode: Mode) -> Option<String> {
    match compile_with_stats(code, mode, Duration::from_secs(60)).await {
        CompileOutcome::Compiled(_) => None,
        CompileOutcome::Rejected(e, _) | CompileOutcome::Failed(e) => Some(e),
    }
}

/// A compile-only measurement. `Rejected` is the compiler's verdict on
/// the program, still carrying the stats of whatever did compile;
/// `Failed` is the measurement failing (init, stats read, wedged
/// compile), from which nothing about the program follows.
enum CompileOutcome {
    Compiled(FusionStats),
    Rejected(String, FusionStats),
    Failed(String),
}

/// Compile-only core behind [`compile_program`] and [`run_fusecheck`]:
/// compile the full drive text under a fresh ctx and return the
/// program's own compile-time [`FusionStats`] delta. The timeout exists
/// because the first update cycle runs inside `compile`. A stats value
/// is never synthesized from a failure.
async fn compile_with_stats(code: &str, mode: Mode, timeout: Duration) -> CompileOutcome {
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return CompileOutcome::Rejected(e, FusionStats::default()),
    };
    let (tx, _rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    // keeps the compile cycle's print output off the process streams
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
        // Debug format is the anyhow chain; gen-check buckets on the
        // last line, the innermost cause.
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
/// delta for the program itself (stats accumulate per `ExecCtx`, so the
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
    // a malformed header is a compile-class reject in every mode
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return (Outcome::CompileErr(e), FusionStats::default()),
    };
    let (tx, mut rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    // per-runtime print capture, so two modes running concurrently in
    // one process keep separate output
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
    let mut outcome = drive(&ctx, &mut rx, &subj, route, timeout, Entry::Compile).await;
    // Exact tier only; sorted because within-cycle emission order is an
    // evaluation-order artifact
    if tier == OracleTier::Exact
        && let Outcome::Trace(t) = &mut outcome
    {
        let mut lines: Vec<String> = sink.take().lines().map(|l| l.to_string()).collect();
        lines.sort_unstable();
        t.stdout = lines;
    }
    // A stack-budget abort is containment like the deadline; which fires
    // first is a race between the engines' descent speeds.
    if matches!(outcome, Outcome::RuntimeErr(_)) && ctx.rt.budget_aborted() {
        outcome = Outcome::Timeout;
    }
    // A wedged runtime never answers another request: abort first, then
    // never await it without a deadline.
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
    entry: Entry,
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
                    return Outcome::Timeout;
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
    // Tracing is armed before the compile (ToGX messages are FIFO), so
    // a value emitted during the compile cycle is in the trace; a
    // program-route runtime armed it at construction.
    let compiled = match entry {
        Entry::Compile => {
            if let Err(e) = ctx.rt.trace_start(sched.max_events, sched.max_cycles) {
                return Outcome::RuntimeErr(format!("trace_start: {e}"));
            }
            let text = subj.compile_text();
            step_or_timeout!(
                ctx.rt.compile(ArcStr::from(text)),
                Ok(c) => c,
                Err(e) => return Outcome::CompileErr(format!("{e:?}"))
            )
        }
        Entry::Program => step_or_timeout!(
            async {
                ctx.rt
                    .program()
                    .await
                    .and_then(|p| p.ok_or_else(|| anyhow::anyhow!("no program")))
            },
            Ok(c) => c,
            Err(e) => return Outcome::CompileErr(format!("{e:?}"))
        ),
    };
    // dropping it deletes the program's node
    let compiled = &compiled;
    let scope_of = |name: &str| match entry {
        Entry::Compile => graphix_compiler::Scope::root(),
        Entry::Program => input_scope(&compiled.env, name),
    };
    let eid = compiled.exprs.last().expect("compile returned no exprs").id;
    let mut segs = Vec::with_capacity(1 + sched.epochs.len());
    segs.push(wait_settled!());
    // Every input's ref is created up front and each epoch's injections
    // go through one `set_many`: separate `set` calls can land in
    // different cycles depending on scheduler timing.
    let mut refs: AHashMap<&str, graphix_rt::Ref<NoExt>> = AHashMap::new();
    for (name, _, _) in sched.inputs() {
        let scope = scope_of(&name);
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
            ep.iter().map(|(name, v)| (refs[name.as_str()].bid, v.clone())).collect();
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
                for (name, _, _) in c.args() {
                    let scope = scope_of(&name);
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
                        ValArray::from_iter_exact(ep.iter().map(|(_, v)| v.clone()));
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
    Outcome::Trace(trace::Trace::from_segments(&segs, eid))
}

/// The reserved metamorphic-twin poison tag: a generated twin program
/// settles an epoch on a `` `TwinDiverged(..) `` value when its
/// equivalent write routes disagree. Scanned on each epoch's final
/// value, through composites. No other program may produce it.
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
    // (`throttle` reads it outside sys::), generated temp paths, netidx
    // registration timing, OS-assigned ports and pids, signal delivery.
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
    if ["sys::", "http::"].iter().any(|m| code.contains(m)) {
        // FinalValues assumes the async values themselves settle
        // deterministically; a `<-` weaves arrival order into state, and
        // the fire-count/arrival-order-sensitive builtins leak it too.
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

/// One mode's three session runs of a subject.
#[derive(Debug)]
pub struct Sessions {
    pub nocache: Outcome,
    pub cold: Outcome,
    pub warm: Outcome,
}

impl Sessions {
    fn get(&self, s: Session) -> &Outcome {
        match s {
            Session::NoCache => &self.nocache,
            Session::Cold => &self.cold,
            Session::Warm => &self.warm,
        }
    }

    /// Whether the three agree pairwise at `tier`.
    pub fn agree(&self, tier: OracleTier) -> bool {
        self.nocache.agrees_with_at(&self.cold, tier)
            && self.cold.agrees_with_at(&self.warm, tier)
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
    static IMAGE: tokio::sync::OnceCell<Option<Bytes>> = tokio::sync::OnceCell::const_new();
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

/// What a session run does about the program image.
enum SessionImage {
    None,
    Write,
    Read(Bytes),
}

/// Run `code` under `mode` on the program route with the given image
/// handling; the second value is the program image a `Write` produced.
async fn run_session(
    code: &str,
    mode: Mode,
    image: SessionImage,
    timeout: Duration,
) -> (Outcome, Result<Bytes, String>) {
    let no_image = Err("no program image was written".to_string());
    let subj = match Subject::parse(code, "test") {
        Ok(s) => s,
        Err(e) => return (Outcome::CompileErr(e), no_image),
    };
    let (tx, mut rx) = mpsc::channel(64);
    let resolver = VfsResolver::new(subj.table.clone());
    let sink = graphix_package_core::PrintSink::default();
    let seeded = sink.clone();
    let registration = || match registration_image_now() {
        Some(bytes) => RegistrationImage::Load(bytes),
        None => RegistrationImage::Save(oneshot::channel().0),
    };
    let program =
        Some(graphix_compiler::expr::Source::Internal(ArcStr::from(subj.compile_text())));
    let (registration, program, program_image, image_rx) = match image {
        SessionImage::Read(bytes) => (RegistrationImage::Load(bytes), None, None, None),
        SessionImage::Write => {
            let (itx, irx) = oneshot::channel();
            (registration(), program, Some(itx), Some(irx))
        }
        SessionImage::None => (registration(), program, None, None),
    };
    let ctx = match init_session_with_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        registration,
        program,
        program_image,
        Some((subj.sched.max_events, subj.sched.max_cycles)),
        move |ctx| {
            *ctx.libstate.get_or_default::<graphix_package_core::PrintSink>() = seeded;
        },
    )
    .await
    {
        Ok(c) => c,
        Err(e) => {
            return (Outcome::RuntimeErr(format!("runtime init failed: {e:?}")), no_image);
        }
    };
    let tier = subj.tier;
    let mut outcome =
        drive(&ctx, &mut rx, &subj, Route::InLanguage, timeout, Entry::Program).await;
    if tier == OracleTier::Exact
        && let Outcome::Trace(t) = &mut outcome
    {
        let mut lines: Vec<String> = sink.take().lines().map(|l| l.to_string()).collect();
        lines.sort_unstable();
        t.stdout = lines;
    }
    if matches!(outcome, Outcome::RuntimeErr(_)) && ctx.rt.budget_aborted() {
        outcome = Outcome::Timeout;
    }
    if matches!(outcome, Outcome::Timeout) {
        ctx.rt.abort();
    }
    let grace = Duration::from_secs(2);
    let image = match image_rx {
        None => no_image,
        Some(irx) => match tokio::time::timeout(grace, irx).await {
            Ok(Ok(Ok(bytes))) => Ok(bytes),
            Ok(Ok(Err(e))) => Err(format!("the program image write failed: {e:#}")),
            Ok(Err(_)) | Err(_) => no_image,
        },
    };
    let _ = tokio::time::timeout(grace, ctx.shutdown()).await;
    (outcome, image)
}

/// The registration image if it has been built; `run_sessions` builds
/// it first.
fn registration_image_now() -> Option<Bytes> {
    static NONE: Option<Bytes> = None;
    REGISTRATION_IMAGE.get().unwrap_or(&NONE).clone()
}

static REGISTRATION_IMAGE: std::sync::OnceLock<Option<Bytes>> = std::sync::OnceLock::new();

/// Run `code` under `mode` three ways: no cache, cold (writing the
/// program image) and warm (restored from it). A program that does not
/// compile has nothing to restore, so its warm outcome is its cold one;
/// a cold run that compiled but wrote no image makes the warm outcome
/// the write's failure, which the cold/warm pair reports.
pub async fn run_sessions(code: &str, mode: Mode, timeout: Duration) -> Sessions {
    if REGISTRATION_IMAGE.get().is_none() {
        let image = registration_image().await;
        let _ = REGISTRATION_IMAGE.set(image);
    }
    let (nocache, _) = run_session(code, mode, SessionImage::None, timeout).await;
    let (cold, image) = run_session(code, mode, SessionImage::Write, timeout).await;
    let warm = match (&cold, image) {
        (Outcome::CompileErr(_), _) => cold.clone(),
        (_, Ok(image)) => run_session(code, mode, SessionImage::Read(image), timeout).await.0,
        (_, Err(e)) => Outcome::RuntimeErr(e),
    };
    Sessions { nocache, cold, warm }
}

/// The session pair that diverges under `mode`, if one does and the
/// program is deterministic: a disagreement reruns the three, and a run
/// that disagrees with its own kind is nondeterminism, not a finding.
async fn session_divergence(
    code: &str,
    mode: Mode,
    first: &Sessions,
    tier: OracleTier,
    timeout: Duration,
) -> Option<Divergence> {
    let pairs = [
        (Pair::Cold(mode), Session::NoCache, Session::Cold),
        (Pair::Warm(mode), Session::Cold, Session::Warm),
    ];
    for (pair, a, b) in pairs {
        if first.get(a).agrees_with_at(first.get(b), tier) {
            continue;
        }
        let again = run_sessions(code, mode, timeout).await;
        if !again.get(a).agrees_with_at(first.get(a), tier) {
            return None;
        }
        if !again.get(a).agrees_with_at(again.get(b), tier) {
            return Some(Divergence {
                code: code.to_string(),
                interp: again.get(a).clone(),
                jit: again.get(b).clone(),
                tier,
                pair,
            });
        }
    }
    None
}

/// Run both modes' sessions and report the first diverging pair.
async fn check_sessions(code: &str, tier: OracleTier, timeout: Duration) -> Option<Divergence> {
    if !sessions_enabled() || tier == OracleTier::Excluded || callable::has_header(code) {
        return None;
    }
    let (si, sj) = tokio::join!(
        run_sessions(code, Mode::Interp, timeout),
        run_sessions(code, Mode::Jit, timeout),
    );
    for (mode, s) in [(Mode::Interp, &si), (Mode::Jit, &sj)] {
        if let Some(d) = session_divergence(code, mode, s, tier, timeout).await {
            return Some(d);
        }
    }
    None
}

/// The scope an injected input is bound in: the root when the subject
/// was compiled statement by statement, else the program's own `do`
/// block, whose name a restored image keeps from the cold run.
fn input_scope(env: &Env, name: &str) -> Scope {
    let mut scope = Scope::root();
    for (path, names) in &env.binds {
        if Path::levels(&path.0) == 1
            && Path::basename(&path.0).is_some_and(|b| b.starts_with("do"))
            && names.get(name).is_some()
        {
            scope.lexical = path.clone();
            break;
        }
    }
    scope
}

/// How a drive reaches its subject: compiled into a running runtime,
/// or compiled or restored at the runtime's construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Entry {
    Compile,
    Program,
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
    /// A metamorphic twin program violated its own invariant. Detected
    /// within a single run (both fields hold the offending outcome), so
    /// it catches bugs that break every engine and route identically.
    Twin,
    /// No cache vs cold (`interp` holds the no-cache outcome, `jit` the
    /// cold one): writing the program image changed the program.
    Cold(Mode),
    /// Cold vs warm (`interp` holds the cold outcome, `jit` the warm
    /// one): restoring the program image changed the program.
    Warm(Mode),
}

impl Divergence {
    /// A one-line classification.
    pub fn bisect(&self) -> &'static str {
        match (&self.interp, &self.jit) {
            // Survived the 8x interp retry: either the JIT fabricated a
            // value or the node-walk is >8x slower on a heavy terminating
            // program. Verify by hand.
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
                (Pair::Cold(Mode::Interp), _) => {
                    "image bug: writing the program image changed the program (interp)"
                }
                (Pair::Cold(Mode::Jit), _) => {
                    "image bug: writing the program image changed the program (jit)"
                }
                (Pair::Warm(Mode::Interp), _) => {
                    "image bug: the restored program differs from the cold one (interp)"
                }
                (Pair::Warm(Mode::Jit), _) => {
                    "image bug: the restored program differs from the cold one (jit)"
                }
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
            Pair::Cold(Mode::Interp) => ("interp/nocache", "interp/cold"),
            Pair::Cold(Mode::Jit) => ("jit/nocache", "jit/cold"),
            Pair::Warm(Mode::Interp) => ("interp/cold", "interp/warm"),
            Pair::Warm(Mode::Jit) => ("jit/cold", "jit/warm"),
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
    // each mode has its own runtime, so run them concurrently
    let (interp, jit) = tokio::join!(
        run_program(code, Mode::Interp, timeout),
        run_program(code, Mode::Jit, timeout),
    );
    if tier == OracleTier::Excluded {
        return (None, false);
    }
    // A twin violation is a single-run finding, checked before agreement:
    // a bug breaking both engines identically agrees on the wrong answer.
    // Confirmed by one rerun.
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
        if let Some(d) = check_sessions(code, tier, timeout).await {
            return (Some(d), false);
        }
        return (None, ran);
    }
    // Reference-side Timeout with a value-bearing jit trace is as likely
    // an honestly slow node-walk as a wrongly terminating JIT; a
    // still-Timeout keeps the finding unless the interp provably made
    // progress. (An empty jit trace against a Timeout already agreed above.)
    if matches!(&interp, Outcome::Timeout) && jit.has_events() {
        let retry = retry_one_sided_timeout(code, Mode::Interp, timeout).await;
        if retry.outcome.agrees_with_at(&jit, tier) {
            return (None, matches!(&retry.outcome, Outcome::Trace(_)));
        }
        if matches!(&retry.outcome, Outcome::Timeout) && interp_made_progress(&retry) {
            eprintln!(
                "SLOW — interp burned {:.1}s CPU over a {:.0}s budget without \
                 finishing; jit's value stands unrefuted; honest slowness, \
                 not recorded",
                retry.cpu_burned.as_secs_f64(),
                retry.budget.as_secs_f64()
            );
            eprintln!("    program: {}", code.replace('\n', "\\n"));
            return (None, false);
        }
    }
    // The symmetric direction is as likely a starved jit child (both
    // modes run concurrently under load) as a native hang. A wedged
    // kernel still times out at the bigger budget and keeps the finding.
    if matches!(&jit, Outcome::Timeout) && interp.has_events() {
        let retry = retry_one_sided_timeout(code, Mode::Jit, timeout).await;
        if interp.agrees_with_at(&retry.outcome, tier) {
            return (None, matches!(&retry.outcome, Outcome::Trace(_)));
        }
    }
    // Rule out nondeterminism: re-run interp at the same tier; if it
    // disagrees with itself, the program is nondeterministic there.
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

struct SlowRetry {
    outcome: Outcome,
    budget: Duration,
    cpu_burned: Duration,
}

/// Re-run the timed-out side at 8x the budget, with an absolute floor:
/// the scale gap is unbounded and load stretches CPU seconds into wall
/// minutes. The CPU delta is process-wide, so a concurrent pool can only
/// over-count, which errs toward dropping.
async fn retry_one_sided_timeout(code: &str, mode: Mode, timeout: Duration) -> SlowRetry {
    let budget = (timeout * 8).max(Duration::from_secs(60));
    let cpu_before = self_cpu();
    let outcome = run_program(code, mode, budget).await;
    let cpu_burned = self_cpu().saturating_sub(cpu_before);
    SlowRetry { outcome, budget, cpu_burned }
}

/// A wedge sits at ~0% CPU; honest slowness burns whatever the scheduler
/// gives it, so seconds of burn over the retry is proof of progress.
fn interp_made_progress(retry: &SlowRetry) -> bool {
    retry.cpu_burned >= Duration::from_secs(5)
}

/// The callable-v1 check matrix: four runs (two engines x two routes),
/// three comparisons — each route's engine pair at the program's tier,
/// then the route pair (node-walk engine) at final-values strength.
/// Records the first divergence in that order; a timeout-involved
/// disagreement retries once at 4x and drops if unresolved. `ran` is
/// always false: callable programs stay out of the mutation ring.
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
    // twin violations first: single-run findings
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
    // strength (the gap compiles and dispatch take an engine-dependent
    // number of cycles), so only settled values are contractual.
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

/// A module resolver whose target can be swapped between compiles: the
/// batch child's bridge between one warmed runtime and a fresh
/// per-subject module source.
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
            let r = self.0.lock().unwrap().clone();
            r.resolve(scope, parent, name, errors).await
        })
    }
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
    /// Not a clean agreement (divergence-shaped, timeout, wedged
    /// runtime, ineligible subject) — the parent re-runs individually.
    Other,
}

/// The `check-batch` child body: run `progs` sequentially against one
/// warmed runtime pair, so the stdlib compiles once per mode. `report`
/// is called after each subject. A Timeout/RuntimeErr in either mode
/// leaves the shared runtime suspect: that subject reports `Other` and
/// the batch stops.
pub async fn run_batch(
    progs: &[String],
    timeout: Duration,
    mut report: impl FnMut(usize, BatchVerdict),
) {
    let (tx_i, rx_i) = mpsc::channel(64);
    let (tx_j, rx_j) = mpsc::channel(64);
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
    let mut lane_i = EngineLane { ctx: ctx_i, rx: rx_i, swap: swap_i };
    let mut lane_j = EngineLane { ctx: ctx_j, rx: rx_j, swap: swap_j };
    let mut consecutive_poison = 0u32;
    for (i, code) in progs.iter().enumerate() {
        // A subject-unique module name gives a fresh module and fresh
        // BindIds; the previous subject's graph was deleted when its
        // `drive` returned. Aux files ride in the subject's own table
        // (aliasing is pinned by `batched_files_do_not_alias`).
        let subj = match Subject::parse(code, &format!("t{i}")) {
            Ok(s) => s,
            Err(_) => {
                report(i, BatchVerdict::Other);
                continue;
            }
        };
        let tier = subj.tier;
        lane_i.swap.set(VfsResolver::new(subj.table.clone()));
        lane_j.swap.set(VfsResolver::new(subj.table.clone()));
        // the subject's own tier drives both runs
        let (interp, jit) = tokio::join!(
            lane_i.drive(&subj, Route::InLanguage, timeout),
            lane_j.drive(&subj, Route::InLanguage, timeout),
        );
        // A callable subject owes the route matrix. The two routes share
        // a runtime per engine, so they run in sequence.
        let routed = if subj.spec.is_some() {
            let (ib, jb) = tokio::join!(
                lane_i.drive(&subj, Route::Dispatch, timeout),
                lane_j.drive(&subj, Route::Dispatch, timeout),
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
        // Excluded tier: no value comparison is sound, so neither the twin
        // scan nor the route pair runs; poison stays batch business
        // whatever the tier. A twin violation goes back through the
        // individual path, which confirms it with a rerun.
        let comparable = tier != OracleTier::Excluded;
        let twin = comparable
            && (twin_violation(&interp)
                || twin_violation(&jit)
                || routed_ref
                    .is_some_and(|(a, b)| twin_violation(a) || twin_violation(b)));
        // dispatch-route cycle offsets are not comparable at Exact
        // strength; only settled values are contractual
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
        // The session runs take fresh runtimes; a disagreement goes back
        // through the individual path, which confirms it with a rerun.
        let sessions_agree = !agreed
            || !comparable
            || subj.spec.is_some()
            || !sessions_sampled(i)
            || {
                let (si, sj) = tokio::join!(
                    run_sessions(code, Mode::Interp, timeout),
                    run_sessions(code, Mode::Jit, timeout),
                );
                si.agree(tier) && sj.agree(tier)
            };
        let agreed = agreed && sessions_agree;
        let verdict = if agreed {
            // `ran` is the parent's ring-admission bar and mirrors the
            // individual path: a callable or Excluded subject is never admitted
            let ran = comparable
                && subj.spec.is_none()
                && matches!(&interp, Outcome::Trace(_))
                && matches!(&jit, Outcome::Trace(_));
            BatchVerdict::Agree { ran }
        } else {
            BatchVerdict::Other
        };
        report(i, verdict);
        if poisoned {
            // three in a row: the runtime answers probes but cannot
            // finish real subjects
            consecutive_poison += 1;
            if consecutive_poison >= 3 {
                break;
            }
            if !probe_runtime_health(i, &mut lane_i, &mut lane_j).await {
                break;
            }
        } else {
            consecutive_poison = 0;
        }
    }
    let grace = Duration::from_secs(2);
    let _ = tokio::time::timeout(grace, lane_i.ctx.shutdown()).await;
    let _ = tokio::time::timeout(grace, lane_j.ctx.shutdown()).await;
}

/// One engine's share of a batch: its runtime, its event channel and
/// the resolver swapped to each subject's table in turn.
struct EngineLane {
    ctx: TestCtx,
    rx: mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    swap: std::sync::Arc<SwapResolver>,
}

impl EngineLane {
    async fn drive(
        &mut self,
        subj: &Subject,
        route: Route,
        timeout: Duration,
    ) -> Outcome {
        drive(&self.ctx, &mut self.rx, subj, route, timeout, Entry::Compile).await
    }
}

/// A Timeout/RuntimeErr leaves the shared runtimes suspect but usually
/// recovered: probe both with a trivial subject on a short budget.
/// True when both answered.
async fn probe_runtime_health(
    i: usize,
    lane_i: &mut EngineLane,
    lane_j: &mut EngineLane,
) -> bool {
    // an interrupt breaks the loop cooperatively, so a deep unwind can
    // churn for seconds after the drive returns
    tokio::time::sleep(Duration::from_secs(1)).await;
    let probe = Subject::parse("i64:0", &format!("p{i}")).expect("trivial probe parses");
    lane_i.swap.set(VfsResolver::new(probe.table.clone()));
    lane_j.swap.set(VfsResolver::new(probe.table.clone()));
    let budget = Duration::from_secs(10);
    let (pi, pj) = tokio::join!(
        lane_i.drive(&probe, Route::InLanguage, budget),
        lane_j.drive(&probe, Route::InLanguage, budget),
    );
    matches!(pi, Outcome::Trace(_)) && matches!(pj, Outcome::Trace(_))
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

/// What the parent asks a child to do, instead of what to run, so the
/// parent's cost is per batch and per finding rather than per subject.
/// `ring` is a small sample of the mutation ring for the child to breed
/// from, not a snapshot.
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
    // ring admission is computed here: the child has the parsed program
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

/// Run a batch of eligible programs through one `check-batch` child.
/// A missing or non-Agree verdict, and every subject on a child that
/// died or exited unclean, falls back to the individual
/// [`check_isolated`] path: batches only fast-path agreement.
async fn batch_isolated(
    progs: Vec<String>,
    timeout: Duration,
) -> (Vec<(String, PoolResult)>, Duration) {
    let n = progs.len();
    let mut resolved: Vec<Option<PoolResult>> = (0..n).map(|_| None).collect();
    let mut individual: Vec<usize> = Vec::new();
    let mut remaining: Vec<usize> = (0..n).collect();
    // every child this batch spends is charged to the requesting source
    let mut cpu = Duration::ZERO;
    // Re-batch after a clean abort: the unreported tail rides a fresh
    // child instead of individual re-runs. An unclean exit discards the
    // round's verdicts (the child's memory was suspect for the whole
    // round); a clean round that reports nothing also falls back.
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
    /// Programs the child could not resolve; the parent re-derives them
    /// through the individual path.
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
    // progress-based deadline: a healthy child flushes a line per subject
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
/// per-index verdicts, cpu). Verdicts come back through a file inside
/// the parent-owned sandbox, flushed per subject.
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
        let mut buf = format!("{}\n", progs.len());
        for p in progs {
            buf.push_str(&format!("{}\n", p.len()));
            buf.push_str(p);
        }
        let _ = stdin.write_all(buf.as_bytes()).await;
    }
    // Progress-based deadline: "the verdict file stopped growing" is the
    // wedge signal, so batch size does not scale the wall. The stall
    // budget covers the double stdlib init, the slowest legitimate
    // subject and the post-timeout health probe.
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

/// Coarse "same bug" key: the bisection class, the outcome kinds and
/// the trace-difference class (final-strength for final-tier and route
/// divergences). The minimizer requires a reduction to keep the bucket.
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
    pub probes: Vec<(String, TmVerdict)>,
    pub noparse: usize,
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

/// Run one subject's metamorphic probes against a single warmed
/// runtime (`GXHandle::check_with_resolvers` never executes and
/// restores the env per call). Probes run only when the base accepts:
/// accept→reject is the finding.
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
/// verdict-file text.
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
    // base + 6 kinds × the per-kind cap, one warmed init, plus slack
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

/// One child per program; a subject reporting flips is confirmed by a
/// second fresh child (transforms are deterministic, so the same probe
/// id must flip again). An unconfirmed flip is reported as its own class.
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

/// Run the embedded regression corpus (every finding under `findings/`)
/// through the oracle and return any that now diverge. Empty means the
/// corpus is clean.
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
                // A non-ran agreement from the parallel pass is not
                // trusted: both-Timeout compares equal, so a pin whose
                // budget blew on both modes under load would pass.
                None if !ran => suspect.push(i),
                None => (),
            }
        }
        if next < entries.len() {
            spawn_one(&mut set, next);
            next += 1;
        }
    }
    // retry the untrusted agreements sequentially at 4x: alone on the
    // box, a still-quiet pin passes on its own character
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

/// The checked-in fusion-coverage manifest: one `fused_count<TAB>name`
/// line per corpus program. `fusecheck` diffs live counts against it so
/// a silent de-fusion regression fails loud; `fusecheck --bless`
/// rewrites the file (then rebuild to embed).
pub static FUSECHECK_MANIFEST: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/fusecheck.manifest"));

/// Fused-region count per corpus program, in corpus order. Jit mode,
/// compile only, so the count cannot depend on run-time pacing. A count
/// that could not be measured is `Err` and must be treated as a gate
/// failure, never as 0.
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

/// The oracle-soundness gate: same program, same mode, twice → identical
/// traces, over every corpus seed plus `iters` generated programs.
/// Returns the programs whose traces disagreed with themselves, tagged
/// with the mode that flaked. Must be empty.
pub async fn selfcheck(
    iters: usize,
    seed: u64,
    timeout: Duration,
) -> Vec<(String, &'static str)> {
    use tokio::task::JoinSet;
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
            // budget-limited subjects are counted, never failed on
            let n = bad.len();
            bad.retain(|(_, mode)| *mode != "inconclusive");
            inconclusive += n - bad.len();
            // streamed as they land: a killed run must not take the list
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
                        // crash finding: dedup by `record_crash`'s key
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
        // The mutant is comment data: its newlines are escaped so a
        // multi-line mutant cannot land a bare program line mid-header.
        // The outcome lines are clipped; the traces are reproducible
        // from the program text below.
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
    // a single-source campaign keeps the unnamed log format
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
    // The evolutionary ring: agreeing both-modes-ran mutants with a
    // novel AST shape join a bounded pool of mutation ancestors. Guard
    // rails: the admission bar, a 50/50 base-seed mix, FIFO eviction.
    // Trajectories are not seed-reproducible; findings are, from their text.
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
                let mut rng = mutate::Rng::new(
                    seed.wrapping_add((k as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15)),
                );
                let g: Box<dyn FnMut() -> String + Send> = Box::new(move || {
                    // retry a few times, falling back to a raw seed so the
                    // pool never stalls
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

/// How many checks to keep in flight. The oracle is mostly wait
/// (runtime spin-up, quiescence), so the cores are oversubscribed 8x.
fn parallelism() -> usize {
    // `GRAPHIX_FUZZ_PAR` overrides so concurrent campaigns can share a box
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
    let mut src = generate_source(seed, 1.0, reactive, gen_tasks());
    src.name = "";
    run_pool_multi(corpus, iters, timeout, vec![src])
        .await
        .pop()
        .map(|(_, stats, _)| stats)
        .unwrap_or_default()
}

/// Fresh type-directed programs, plain or scheduled. Neither feeds the
/// mutation ring: admitting them would change what its novelty counter
/// measures.
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
        let limit: u64 = std::env::var("GRAPHIX_FUZZ_MEM_LIMIT")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(48 << 30);
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
        // dispatch-route cycle offsets are not comparable at Exact
        // strength even against themselves; only settled values are
        let route_tier = match route {
            Route::Dispatch if tier == OracleTier::Exact => OracleTier::FinalValues,
            _ => tier,
        };
        let (a, b) = tokio::join!(
            run_program_routed(prog, mode, route, timeout),
            run_program_routed(prog, mode, route, timeout),
        );
        if !a.agrees_with_at(&b, route_tier) {
            // A Timeout is not a value: comparing it against one measures
            // the budget, not determinism. Confirm at 4x.
            let big = timeout * 4;
            let a2 = run_program_routed(prog, mode, route, big).await;
            let b2 = run_program_routed(prog, mode, route, big).await;
            if a2.agrees_with_at(&b2, route_tier) {
                continue;
            }
            if matches!(a2, Outcome::Timeout) || matches!(b2, Outcome::Timeout) {
                // inconclusive at this budget, not flaky
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

/// Run one selfcheck subject in a child process. Isolation is for
/// memory as much as crash containment: every in-process `run_program`
/// keeps its context's JIT pages, so the child pays the leak and exits.
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
        Err(e) => {
            eprintln!("FATAL fuzz harness: child spawn failed: {e}");
            std::process::exit(2)
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(prog.as_bytes()).await;
    }
    // up to 8 in-child runs, each bounded by the per-run timeout
    let deadline = timeout * 10 + Duration::from_secs(30);
    let out = match tokio::time::timeout(deadline, child.wait_with_output()).await {
        Ok(Ok(out)) => out,
        // a dead or wedged child is nondeterminism by definition
        Ok(Err(_)) | Err(_) => return vec!["crash"],
    };
    // verdict in the exit code: 0 = clean, 40+mask flags the flaky modes
    match out.status.code() {
        Some(0) => Vec::new(),
        Some(41) => vec!["interp"],
        Some(42) => vec!["jit"],
        Some(43) => vec!["interp", "jit"],
        // timed out at 4x on the confirm pair: the budget decided
        Some(50) => vec!["inconclusive"],
        _ => vec!["crash"],
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
/// and exit codes. Fusion shape must be a pure function of the program
/// text. A wall-clock timeout on either side (exit 4) skips the pair.
/// `Some(detail)` = flap.
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

/// Run one oracle check in a child process (`graphix-fuzz check-one`,
/// program on stdin, verdict in the exit code). A program that kills the
/// evaluator kills only the child; the campaign records a crash finding
/// and keeps running.
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
        // the pool provides the concurrency; small children keep the
        // total thread count sane
        .env("TOKIO_WORKER_THREADS", "2")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true);
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        // a spawn IO error is a broken harness, not a program crash:
        // recording it would flood the corpus at instant-fail speed
        Err(e) => {
            eprintln!("FATAL fuzz harness: child spawn failed: {e}");
            std::process::exit(2)
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        // a write error means the child died instantly; wait captures it
        let _ = stdin.write_all(prog.as_bytes()).await;
    }
    // The child runs interp+jit with its own per-mode `timeout`; the
    // outer deadline only catches a wedged child and must cover the
    // child's whole legitimate worst case: the concurrent first runs,
    // `check()`'s 60s-floored escalation retry and the nondeterminism re-run.
    let deadline = timeout * 4
        + (timeout * 8).max(Duration::from_secs(60))
        + Duration::from_secs(30);
    let out = match tokio::time::timeout(deadline, child.wait_with_output()).await {
        Ok(Ok(out)) => out,
        Ok(Err(e)) => return PoolResult::Crash(format!("wait: {e}")),
        Err(_) => return PoolResult::Crash("HANG (outer deadline)".into()),
    };
    // the verdict is the exit code (0 = agree, 7 = agree and both ran,
    // 10 = diverge): stdout is corruptible by the program under test
    match out.status.code() {
        Some(0) => PoolResult::Agree { ran: false },
        Some(7) => PoolResult::Agree { ran: true },
        // the child proved the program diverges without dying, so an
        // in-process re-check for the full Divergence is safe
        Some(10) => match check(prog, timeout).await {
            Some(d) => PoolResult::Diverge(d),
            // flaky: drop it rather than record an unreproducible finding
            None => PoolResult::Agree { ran: false },
        },
        _ => {
            // a SIGTERM death is the campaign stop's own kill signal
            // reaching a mid-flight child, not a finding
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                if out.status.signal() == Some(15) {
                    return PoolResult::Agree { ran: false };
                }
            }
            // the child's last stderr lines distinguish a node-walk
            // overflow from a SIGSEGV in JIT'd frames (which prints nothing)
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

/// Oracle-check budget for the campaign's minimizer: a soak pays this
/// per finding, so it buys a legible reproducer, not a minimal one.
pub const CAMPAIGN_MINIMIZE_BUDGET: usize = 80;

/// Minimize a diverging program in a child process. A reduction may
/// itself be a crasher and the minimizer checks candidates in-process.
/// `None` = the child died or wedged; the caller records the
/// unminimized mutant instead.
async fn minimize_isolated(prog: &str, timeout: Duration) -> Option<String> {
    use tokio::io::AsyncWriteExt;
    let mut cmd = tokio::process::Command::new(child_exe());
    let sandbox = sandbox_cwd(&mut cmd);
    // inside the sandbox, so the guard's drop cleans it up
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
    // worst case the whole budget is bottom programs sleeping the
    // per-mode timeout
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

/// One work source in a soak: where its programs come from, what it does
/// with an agreeing result, and the share of the box's CPU it should
/// draw.
pub struct Source<'a> {
    /// Prefix on this source's counter lines; "" for a single-source
    /// campaign.
    pub name: &'static str,
    /// Relative CPU share. Normalized internally, so any positive scale
    /// works.
    pub weight: f64,
    /// Generators, run in their own tasks (generation is real work:
    /// `mutate_wrapper` rewrites an AST per subject). Each owns a
    /// disjoint seed stream, so a subject is reproducible as (source, seed).
    pub gens: Vec<Box<dyn FnMut() -> String + Send + 'a>>,
    /// Ring admission, run in its own task like generation (`shape_stats`
    /// parses the program). `None` = this source has no ring.
    pub on_agree: Option<Box<dyn FnMut(&str, bool) -> bool + Send + 'a>>,
}

/// Per-source accounting. `cpu` is what the source's finished children
/// burned; `inflight` is what it has issued but not been charged for,
/// estimated at the source's own observed mean.
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
    /// Programs the generator task has already produced; the driver only
    /// pops, so filling a batch never blocks dispatch on generation.
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

/// Generator tasks per source. Generation is CPU work, so one task
/// cannot feed a pool; capped because they compete with the workers.
pub fn gen_tasks() -> usize {
    std::thread::available_parallelism().map(|n| n.get() / 4).unwrap_or(2).clamp(2, 8)
}

/// How many programs each source's generator may run ahead. Deep enough
/// that filling a 64-subject batch is always a memory move, small enough
/// that a source cannot hoard memory when the pool is busy elsewhere.
const GEN_BUFFER: usize = 512;

/// Drain whatever the generators have produced into the ready buffers,
/// then answer which source to issue: `want` (the CPU-share choice)
/// whenever it has work, else any source with work.
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

/// The aggregator: issue work orders and aggregate what comes back. The
/// parent never generates, classifies or ships program text; its cost
/// is per batch (issue an order, charge its CPU) and per finding
/// (derive a divergence, admit a ring shape).
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
    // findings are confirmed in detached `derive` tasks, so the tally
    // crosses tasks
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
    // subjects a batch child could not resolve, re-derived one process
    // each; batching everything is only right while this stays small
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
        // keep `par` orders in flight, choosing the source furthest below
        // its target share of measured CPU
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
                // distinct per (source, order) without a shared counter
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
                // Only mutants breed: admitting generated shapes would
                // change what the ring's 50/50 base-seed mix measures.
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
                // a suspect is derived by the individual path, which owns
                // the escalation ladder and the minimizer
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

/// Run several sources through one pool, dividing the box by measured
/// CPU rather than by worker slots (slots are not cores: what a check
/// draws depends on how much of its life the subject spends blocked).
/// Keeps `parallelism()` checks in flight, in isolated child processes
/// unless `GRAPHIX_FUZZ_INPROC=1`. A divergence is minimized, deduped
/// against `corpus` and written without stalling the pool; a crash
/// records immediately. `iters = None` runs forever.
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
    // One generator task per source, bounded; each source's seed stream
    // stays sequential inside its own task.
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
        // Ring admission likewise, try_send'd: dropping an admission
        // costs a shape, stalling the driver costs every source its slots.
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
    // whichever source is furthest below its target share of projected CPU
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
    // Spawn one child's worth of work: subjects accumulate per source
    // into a `check-batch` child (so the child's CPU charges to exactly
    // one account); leftover partial batches flush when `iters` runs out.
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
                // Never generate inline and never spawn a partial batch to
                // fill the gap: report dry and let the caller await
                // generation. Returning without spawning would park the
                // accumulated programs and shrink the pool permanently.
                let si = match ready_source(si, states, gens) {
                    Some(i) => i,
                    None => return false,
                };
                let prog = match states[si].ready.pop_front() {
                    Some(p) => p,
                    None => return false,
                };
                *launched += 1;
                // GRAPHIX_FUZZ_ECHO: print each program as it dispatches
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
                // Every subject batches; the batch child falls back on its
                // own, and the `% individual` counter reports the rate.
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
    // Wait for a generator when nothing is ready: at t=0 every buffer is
    // empty, and "nothing ready" is not "no work left".
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
            // every generator gone means the campaign is out of work
            if closed == gens.len() {
                return false;
            }
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
    }
    // keep asking until the slot is filled
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
                // Refill first: `continue` inside this arm targets the
                // enclosing loop, so a refill at the bottom would leak a
                // slot on every excluded result.
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
                        // per-source counters, each on its own line
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
                            // A HANG in a program touching IO/async modules
                            // is environmental: the child has no resolver.
                            // Signal deaths and panics still record.
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
                            // `check` compares at the program's own tier,
                            // so a Diverge from the child is a real finding
                            states[si].stats.divergences += 1;
                            // bound concurrent minimizations
                            if minims.len() >= par {
                                let _ = minims.join_next().await;
                            }
                            let corpus = corpus.clone();
                            minims.spawn(async move {
                                // isolated: a reduction of a benign
                                // divergence can itself be a crasher
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
    fn throttle_is_excluded_tier() {
        // `throttle` reads the wall clock and must exclude on its own,
        // with no sys:: in the program
        assert_eq!(
            oracle_tier("count(throttle(#rate: duration:0.001s, x))"),
            OracleTier::Excluded
        );
        // the other fire-count-sensitive builtins stay Exact
        assert_eq!(oracle_tier("count(x)"), OracleTier::Exact);
        // a comment naming the builtin must not un-gate the program
        assert_eq!(
            oracle_tier("// a header naming throttle\ncount(x)"),
            OracleTier::Exact
        );
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
        // bare-element body: node-walks
        agree("{ let a = [i64:1, i64:2]; array::flat_map(a, |x| x) }").await;
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
                    d.interp, d.jit
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
    /// both agree in one batch: files do not alias across the warmed
    /// runtime. The differential oracle cannot see aliasing (both
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
