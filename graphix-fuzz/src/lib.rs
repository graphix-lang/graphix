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

pub mod corpus;
pub mod files;
pub mod generate;
pub mod mutate;
pub mod schedule;
pub mod trace;

use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, FusionStats, expr::ModuleResolver};
use graphix_package::Package;
use graphix_package_core::testing::{TestCtx, init_with_flags_and_setup};
use graphix_rt::{GXEvent, NoExt};
use netidx::publisher::Value;
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
    /// cycles count against the budget).
    Timeout,
}

impl Outcome {
    /// The trace of a pure synchronous program producing `v` once at
    /// init — offset 0, single epoch. Test convenience.
    pub fn single(v: Value) -> Outcome {
        Outcome::Trace(trace::Trace {
            epochs: vec![trace::Epoch { events: vec![(0, v)], capped: false }],
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
            (CompileErr(_), CompileErr(_)) => true,
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
    run_program_with_stats(code, mode, timeout).await.0
}

/// Compile `code` (a wrapper, as [`run_program`]) under `mode` WITHOUT
/// driving it — `None` = compiled clean, `Some(error)` = parse/typecheck
/// reject (or the runtime failed to init). This is `gen-check`'s
/// primitive: the generator is type-correct by construction, so the
/// compile-reject RATE is its health metric and each reject message is
/// a tuning signal.
pub async fn compile_program(code: &str, mode: Mode) -> Option<String> {
    let (sched, body) = match schedule::Schedule::parse(code) {
        Ok(x) => x,
        Err(e) => return Some(format!("schedule header: {e}")),
    };
    let (body, files) = match files::split(body) {
        Ok(x) => x,
        Err(e) => return Some(format!("file section: {e}")),
    };
    let (tx, _rx) = mpsc::channel(64);
    let wrapped = format!("let result = {body}");
    let mut tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(ArcStr::from(wrapped)),
    )]);
    for (name, text) in &files {
        tbl.insert(
            Path::from(format!("/{name}")),
            graphix_compiler::expr::VfsEntry::from(ArcStr::from(text.as_str())),
        );
    }
    let resolver = ModuleResolver::VFS(tbl);
    let ctx = match init_with_flags_and_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        |_| {},
    )
    .await
    {
        Ok(c) => c,
        Err(e) => return Some(format!("runtime init failed: {e:?}")),
    };
    let text = format!(
        "{}{}{{ mod test; test::result }}",
        sched.decls(),
        files::mod_decls(&files)
    );
    let res = ctx.rt.compile(ArcStr::from(text)).await;
    ctx.shutdown().await;
    match res {
        Ok(_) => None,
        // Debug format = the multi-line anyhow chain; the last line is
        // the innermost cause, which is what gen-check buckets on.
        Err(e) => Some(format!("{e:?}")),
    }
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
    // A malformed schedule header is a COMPILE-class reject in every
    // mode (agreement) — a generator/minimizer bug surfaces in
    // gen-check, never as a phantom divergence.
    let (sched, body) = match schedule::Schedule::parse(code) {
        Ok(x) => x,
        Err(e) => {
            return (
                Outcome::CompileErr(format!("schedule header: {e}")),
                FusionStats::default(),
            );
        }
    };
    let (body, files) = match files::split(body) {
        Ok(x) => x,
        Err(e) => {
            return (
                Outcome::CompileErr(format!("file section: {e}")),
                FusionStats::default(),
            );
        }
    };
    let (tx, mut rx) = mpsc::channel(64);
    let wrapped = format!("let result = {body}");
    let mut tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(ArcStr::from(wrapped)),
    )]);
    for (name, text) in &files {
        tbl.insert(
            Path::from(format!("/{name}")),
            graphix_compiler::expr::VfsEntry::from(ArcStr::from(text.as_str())),
        );
    }
    let resolver = ModuleResolver::VFS(tbl);
    let ctx = match init_with_flags_and_setup(
        tx,
        REGISTER,
        vec![resolver],
        mode.flags(),
        |_| {},
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
    let base = ctx.fusion_stats().await.unwrap_or_default();
    let outcome = drive(
        &ctx,
        &mut rx,
        &sched,
        &files::mod_decls(&files),
        oracle_tier(code),
        timeout,
    )
    .await;
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
    sched: &schedule::Schedule,
    mods: &str,
    tier: OracleTier,
    timeout: Duration,
) -> Outcome {
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
    macro_rules! bounded {
        ($fut:expr, $on_ok:pat => $ok:expr, $on_err:pat => $err:expr) => {{
            let f = $fut;
            tokio::pin!(f);
            tokio::select! {
                biased;
                r = &mut f => match r { $on_ok => $ok, $on_err => $err },
                _ = &mut drain => unreachable!(),
                _ = &mut deadline => {
                    // A wedged evaluator may be ABORTABLE: runaway call
                    // trees and scaffold loops poll the cooperative
                    // interrupt. Set it and give a grace period — a
                    // bottom-and-answer becomes a normal outcome
                    // instead of a killed child slot.
                    ctx.rt.interrupt();
                    match tokio::time::timeout(
                        Duration::from_millis(750),
                        &mut f,
                    )
                    .await
                    {
                        Ok(r) => {
                            // Re-arm: a completed Sleep must not be
                            // re-polled, and the remaining steps get a
                            // fresh budget.
                            deadline
                                .as_mut()
                                .reset(tokio::time::Instant::now() + timeout);
                            match r { $on_ok => $ok, $on_err => $err }
                        }
                        Err(_) => return Outcome::Timeout,
                    }
                }
            }
        }};
    }
    // The wall-clock-breached twin of `bounded!` for the VERDICT-
    // carrying wait steps: a deadline breach IS `Timeout`, always. The
    // interrupt + grace still run — but only so the runtime unwinds
    // cleanly, never to reclassify. `bounded!`'s reclassification (an
    // interrupted step that then completes reports its normal result)
    // is right for the compile steps — a slow stdlib compile under gate
    // load must not read as a wedged program — but on a wait step it
    // made the verdict a RACE: `interrupt()` is a one-shot flag cleared
    // at cycle end, so for a fast-cycling reactive runaway (the
    // deref-echo class, soak jul05 items 3/20/24) it lands in a poll
    // window only sometimes — the same program flipped between Timeout
    // (interrupt lost, grace expired) and Trace([]) (echo died, idle
    // resolved) run-to-run, in BOTH modes independently.
    macro_rules! wait_bounded {
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
            let mut seg = wait_bounded!(
                ctx.rt.trace_wait_idle(),
                Ok(s) => s,
                Err(e) => return Outcome::RuntimeErr(format!("trace_wait_idle: {e}"))
            );
            if tier == OracleTier::FinalValues {
                for _ in 0..8 {
                    tokio::time::sleep(Duration::from_millis(150)).await;
                    let more = wait_bounded!(
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
    let text = format!("{}{mods}{{ mod test; test::result }}", sched.decls());
    let compiled = bounded!(
        ctx.rt.compile(ArcStr::from(text)),
        Ok(c) => c,
        Err(e) => return Outcome::CompileErr(format!("{e}"))
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
    Outcome::Trace(trace::Trace::from_segments(&segs, eid))
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
    // Value-nondeterministic sources: random values, wall-clock time
    // (timers deliver it, `now()` returns it), generated temp paths,
    // netidx registration timing, and OS-assigned socket addresses
    // (generated programs bind port 0, so the addr getters read back
    // an ephemeral port — the same environmental-value leak as a
    // tempdir path; soak jul08d found `local_addr(s)? <= addr` as a
    // coin flip that passed the interp-self-agreement filter).
    let excluded = [
        "rand::",
        "sys::time",
        "sys::net",
        "tempdir",
        "listener_addr",
        "local_addr",
        "peer_addr",
    ];
    if excluded.iter().any(|m| code.contains(m)) {
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
        if code.contains("<-") {
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
            _ => match self.tier {
                OracleTier::FinalValues => "fusion/JIT bug (final values, interp != jit)",
                _ => "fusion/JIT bug (interp != jit)",
            },
        }
    }
}

/// Run `code` under interp (node-walk) and jit (fusion + cranelift); if
/// they disagree AT THE PROGRAM'S ORACLE TIER, return the `Divergence`.
/// `None` means they agree (or the program is tier-Excluded — it still
/// ran, for shape exercise and crash coverage, but no value comparison
/// is sound for it).
pub async fn check(code: &str, timeout: Duration) -> Option<Divergence> {
    let tier = oracle_tier(code);
    // The two evaluators must agree, or it's a divergence. Each mode
    // spins up its own runtime, so run them concurrently — `join!`
    // overlaps their (mostly I/O-bound) execution on one task.
    let (interp, jit) = tokio::join!(
        run_program(code, Mode::Interp, timeout),
        run_program(code, Mode::Jit, timeout),
    );
    if tier == OracleTier::Excluded {
        return None;
    }
    if interp.agrees_with_at(&jit, tier) {
        return None;
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
        let slow = run_program(code, Mode::Interp, slow_budget).await;
        if slow.agrees_with_at(&jit, tier) {
            return None;
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
        return None;
    }
    Some(Divergence { code: code.to_string(), interp, jit, tier })
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
        (Outcome::Trace(a), Outcome::Trace(b)) => match d.tier {
            OracleTier::FinalValues => a.first_final_difference(b),
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
/// shrinks by subtree hoist / constant replacement, keeping any
/// reduction that still parses and reproduces the SAME divergence
/// bucket. Returns the minimized wrapper and the number of oracle
/// checks spent (capped by `budget`). Accepts partial minima.
pub async fn minimize(code: &str, timeout: Duration, budget: usize) -> (String, usize) {
    let d0 = match check(code, timeout).await {
        Some(d) => d,
        None => return (code.to_string(), 1),
    };
    let target = bucket(&d0);
    let Ok((mut sched, body)) = schedule::Schedule::parse(code) else {
        return (code.to_string(), 1);
    };
    let Ok((body, mut files)) = files::split(body) else {
        return (code.to_string(), 1);
    };
    let mut current = match mutate::parse(body) {
        Some(e) => e,
        None => return (code.to_string(), 1),
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
            if let Some(d) = check(&cand.render(&body_text), timeout).await {
                if bucket(&d) == target {
                    sched = cand;
                    continue 'sched; // restart from the smaller schedule
                }
            }
        }
        break;
    }
    // Phase 1.5 — file-section reductions: drop each module's section
    // pair, then each interface alone (no .gxi = everything public —
    // the divergence often survives the simpler layout).
    'files: while calls < budget && !files.is_empty() {
        let body_text = current.to_string();
        for cand in file_reductions(&files) {
            if calls >= budget {
                break 'files;
            }
            calls += 1;
            let text = sched.render(&files::render(&body_text, &cand));
            if let Some(d) = check(&text, timeout).await {
                if bucket(&d) == target {
                    files = cand;
                    continue 'files; // restart from the smaller file set
                }
            }
        }
        break;
    }
    // Phase 2 — body AST HDD, every candidate re-wrapped in the
    // (now minimal) schedule + file set.
    loop {
        let n = mutate::node_count(&current);
        let cur_text = current.to_string();
        let mut reduced = false;
        'scan: for t in 0..n {
            for repl in mutate::reductions(&current, t) {
                if calls >= budget {
                    break 'scan;
                }
                let cand = mutate::replace(&current, t, &repl).to_string();
                if cand == cur_text || mutate::parse(&cand).is_none() {
                    continue;
                }
                calls += 1;
                let text = sched.render(&files::render(&cand, &files));
                if let Some(d) = check(&text, timeout).await {
                    if bucket(&d) == target {
                        if let Some(e) = mutate::parse(&cand) {
                            current = e;
                            reduced = true;
                            break 'scan; // restart on the smaller program
                        }
                    }
                }
            }
        }
        if !reduced || calls >= budget {
            break;
        }
    }
    (sched.render(&files::render(&current.to_string(), &files)), calls)
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
pub async fn run_regression(timeout: Duration) -> Vec<(String, Divergence)> {
    use tokio::task::JoinSet;
    let par = parallelism();
    let entries = corpus::REGRESSION_CORPUS;
    let mut set: JoinSet<(String, Option<Divergence>)> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one = |set: &mut JoinSet<_>, i: usize| {
        let (name, prog) = (entries[i].0.to_string(), entries[i].1.to_string());
        set.spawn(async move {
            let d = check(&prog, timeout).await;
            (name, d)
        });
    };
    while next < entries.len() && set.len() < par {
        spawn_one(&mut set, next);
        next += 1;
    }
    let mut regressions = Vec::new();
    while let Some(res) = set.join_next().await {
        if let Ok((name, Some(d))) = res {
            regressions.push((name, d));
        }
        if next < entries.len() {
            spawn_one(&mut set, next);
            next += 1;
        }
    }
    regressions
}

/// Number of programs in the embedded regression corpus.
pub fn regression_corpus_len() -> usize {
    corpus::REGRESSION_CORPUS.len()
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
    while let Some(res) = set.join_next().await {
        if let Ok(mut bad) = res {
            // Stream each finding as it lands — a killed or wedged run
            // must not take the collected list with it.
            for (prog, mode) in &bad {
                eprintln!("FLAKY under {mode}: {}", prog.replace('\n', "\\n"));
            }
            flaky.append(&mut bad);
        }
        done += 1;
        if done % 200 == 0 {
            eprintln!("  …{done}/{} selfchecked, {} flaky", progs.len(), flaky.len());
        }
        if next < progs.len() {
            spawn_one(&mut set, progs[next].clone());
            next += 1;
        }
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
        let body = format!(
            "// bisect: {}\n// interp: {}\n// jit:    {}\n\
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
    let seeds = corpus::all_seeds();
    let donors = mutate::donor_pool(&seeds);
    let mut rng = mutate::Rng::new(seed);
    run_pool(corpus, iters, timeout, || {
        // Mutate a random seed; retry a few times if a mutation chain
        // didn't yield a parseable program, falling back to a raw seed
        // (always valid) so the pool never stalls. `mutate_wrapper`
        // preserves (and M3-mutates) schedule headers.
        for _ in 0..8 {
            let s = seeds[rng.below(seeds.len())];
            if let Some(p) = mutate::mutate_wrapper(s, &donors, &mut rng, 5) {
                return p;
            }
        }
        seeds[rng.below(seeds.len())].to_string()
    })
    .await
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
    let mut rng = mutate::Rng::new(seed);
    run_pool(corpus, iters, timeout, || {
        if reactive {
            generate::reactive::gen_reactive_program(&mut rng)
        } else {
            generate::gen_program(&mut rng)
        }
    })
    .await
}

/// What one pool slot concluded about a program.
enum PoolResult {
    Agree,
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
fn sandbox_cwd(cmd: &mut tokio::process::Command) -> tempfile::TempDir {
    match tempfile::tempdir() {
        Ok(d) => {
            cmd.current_dir(d.path()).env("GRAPHIX_FUZZ_SANDBOXED", "1");
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
    for mode in [Mode::Interp, Mode::Jit] {
        let (a, b) = tokio::join!(
            run_program(prog, mode, timeout),
            run_program(prog, mode, timeout),
        );
        if !a.agrees_with_at(&b, tier) {
            let a2 = run_program(prog, mode, timeout).await;
            let b2 = run_program(prog, mode, timeout).await;
            if a2.agrees_with_at(&b2, tier) {
                continue;
            }
            bad.push(match mode {
                Mode::Interp => "interp",
                Mode::Jit => "jit",
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
                for pat in ["ExprId(", "u0:", "kir_", "lambda#"] {
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

async fn check_isolated(prog: &str, timeout: Duration) -> PoolResult {
    use tokio::io::AsyncWriteExt;
    let mut cmd = tokio::process::Command::new(child_exe());
    let _sandbox = sandbox_cwd(&mut cmd);
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
        Some(0) => PoolResult::Agree,
        // The child proved the program diverges WITHOUT dying, so an
        // in-process re-check of the SAME program is safe — re-run it
        // here to get the full Divergence for the record pipeline.
        Some(10) => match check(prog, timeout).await {
            Some(d) => PoolResult::Diverge(d),
            // Flaky (borderline timeout) — drop it rather than record
            // an unreproducible finding.
            None => PoolResult::Agree,
        },
        _ => {
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
    // Worst case the 80-check budget is all bottom programs sleeping the
    // per-mode timeout — bound it generously, the minims pool is
    // concurrent and a kill falls back to the unminimized mutant.
    let deadline = timeout * 2 * 80 + Duration::from_secs(60);
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

/// Worker pool. Keeps `parallelism()` oracle checks in flight over fresh
/// programs from `next_prog`. Checks run in ISOLATED child processes by
/// default (see [`check_isolated`]; `GRAPHIX_FUZZ_INPROC=1` opts back
/// into in-process for debugging). On a divergence it fires a
/// bounded-parallel task that minimizes, dedups against `corpus`, and —
/// if the minimized form is new — writes the `.gx` and prints it
/// immediately, all WITHOUT stalling the check pool (minimization is ≈80
/// serial checks; running it inline drained the cores). A crash records
/// immediately (no minimization — the repro must stay out-of-process).
/// `iters = None` runs forever (until killed), surfacing new divergences
/// live; `Some(n)` stops after `n` programs. `next_prog` runs on the
/// driver task (sequential, deterministic, cheap).
async fn run_pool(
    corpus: &std::sync::Arc<Corpus>,
    iters: Option<usize>,
    timeout: Duration,
    mut next_prog: impl FnMut() -> String,
) -> FuzzStats {
    use tokio::task::JoinSet;
    let par = parallelism();
    let isolate = std::env::var_os("GRAPHIX_FUZZ_INPROC").is_none();
    let mut stats = FuzzStats::default();
    let mut breakage = BreakageWindow::new();
    let mut checks: JoinSet<(String, PoolResult)> = JoinSet::new();
    let mut minims: JoinSet<()> = JoinSet::new();
    let mut launched = 0usize;
    let want = |launched: usize| iters.map_or(true, |n| launched < n);
    let spawn_check = |checks: &mut JoinSet<_>, prog: String| {
        // Crash forensics: with GRAPHIX_FUZZ_ECHO set, print each
        // program as it dispatches. Mostly superseded by isolation
        // (a crasher now records itself), but kept for debugging the
        // DRIVER process itself.
        if std::env::var_os("GRAPHIX_FUZZ_ECHO").is_some() {
            eprintln!("FUZZPROG\t{}", prog.replace('\n', "\\n"));
        }
        checks.spawn(async move {
            let res = if isolate {
                check_isolated(&prog, timeout).await
            } else {
                match check(&prog, timeout).await {
                    Some(d) => PoolResult::Diverge(d),
                    None => PoolResult::Agree,
                }
            };
            (prog, res)
        });
    };
    while want(launched) && checks.len() < par {
        spawn_check(&mut checks, next_prog());
        launched += 1;
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
                if want(launched) {
                    spawn_check(&mut checks, next_prog());
                    launched += 1;
                }
                if let Ok((prog, res)) = res {
                    stats.run += 1;
                    if stats.run % 1000 == 0 {
                        eprintln!(
                            "  …{} run, {} divergences, {} crashes, {} in corpus",
                            stats.run, stats.divergences, stats.crashes,
                            corpus.len()
                        );
                    }
                    let finding = match res {
                        PoolResult::Agree => false,
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
                            stats.crashes += 1;
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
                            stats.divergences += 1;
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
                                    minimize(&prog, timeout, 80).await.0
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
            Some(_) = minims.join_next() => {}
            else => break,
        }
    }
    while minims.join_next().await.is_some() {}
    stats
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
                // wrappers ("node does not emit CLIF"), a
                // function-valued let can never emit by design (the
                // binding node-walks while its call sites fuse), and a
                // lambda call site with fn-typed args dispatches by
                // node-walk while its monomorphic instance body fuses
                // (the `fused > 0` assert above sees that kernel).
                assert!(
                    reason.contains("node does not emit CLIF")
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
        // scalar → Nullable out (select body, Value-shape push)
        agree_fused_clean(
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
        // run, no instrumentation ritual required.
        eprintln!("sweep: {fused} regions fused across 120 programs");
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
            graphix_compiler::expr::VfsEntry::from(ArcStr::from(program.to_string())),
        )]);
        let resolver = ModuleResolver::VFS(tbl);
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
