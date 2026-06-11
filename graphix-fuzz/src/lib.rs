//! Differential model-checking oracle for the graphix fusion/JIT backend.
//!
//! A program is run under several compiler-flag configurations of the
//! *same* front-end:
//!   - **interp** (`CFlag::FusionDisabled`) — the node-walk interpreter,
//!     the simple, more-trusted reference model.
//!   - **jit** (no flags) — the fusion + cranelift-JIT backend, the
//!     system under test.
//!   - **fused** (`CFlag::JitDisabled`) — fusion that runs on the
//!     interpreter; used only to *bisect* a divergence (GIR/emit vs JIT
//!     codegen), not in the hot path.
//!
//! For any deterministic program the configurations must produce the
//! same observable result. A difference proves a bug exists — usually in
//! fusion+JIT (it is far more complex), though the node-walk is the
//! more-trusted model, not infallible. See `design/graphix_fuzz.md`.
//!
//! V1 scope: single-snapshot oracle over pure-synchronous, terminating
//! programs (the first emitted value of `result`). Reactive trace
//! comparison is a later milestone.

pub mod corpus;
pub mod generate;
pub mod mutate;

use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use graphix_compiler::{expr::ModuleResolver, CFlag, FusionStats};
use graphix_package::Package;
use graphix_package_core::testing::{init_with_flags_and_setup, RegisterFn, TestCtx};
use graphix_rt::{GXEvent, NoExt};
use netidx::publisher::Value;
use netidx_core::path::Path;
use std::time::Duration;
use tokio::sync::mpsc;

/// Every stdlib package, so generated programs can use the whole
/// language surface. Mirrors `graphix-tests`'s `TEST_REGISTER` (which is
/// `#[cfg(test)]`-gated and so not importable).
pub const REGISTER: &[RegisterFn] = &[
    <graphix_package_core::P as Package<NoExt>>::register,
    <graphix_package_array::P as Package<NoExt>>::register,
    <graphix_package_map::P as Package<NoExt>>::register,
    <graphix_package_str::P as Package<NoExt>>::register,
    <graphix_package_sys::P as Package<NoExt>>::register,
    <graphix_package_http::P as Package<NoExt>>::register,
    <graphix_package_json::P as Package<NoExt>>::register,
    <graphix_package_toml::P as Package<NoExt>>::register,
    <graphix_package_re::P as Package<NoExt>>::register,
    <graphix_package_rand::P as Package<NoExt>>::register,
    <graphix_package_db::P as Package<NoExt>>::register,
    <graphix_package_xls::P as Package<NoExt>>::register,
    <graphix_package_pack::P as Package<NoExt>>::register,
    <graphix_package_args::P as Package<NoExt>>::register,
    <graphix_package_list::P as Package<NoExt>>::register,
    <graphix_package_sqlite::P as Package<NoExt>>::register,
    <graphix_package_hbs::P as Package<NoExt>>::register,
];

/// The mode a program was run under.
///
/// The GIR interpreter is gone, so there are only two evaluators: the
/// node-walk (the reference) and fusion + cranelift JIT (the system
/// under test). The old middle `Fused` mode (`CFlag::JitDisabled` —
/// fusion on, dispatch via the interpreter) no longer has an
/// interpreter to dispatch into: with `JitDisabled`, fusion builds
/// kernels but can't splice them, so the program node-walks — identical
/// to `Interp`. We drop it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Node-walk interpreter (`CFlag::FusionDisabled`) — the reference.
    Interp,
    /// Fusion + cranelift JIT (no flags) — the system under test.
    Jit,
    /// Fusion + cranelift JIT, but the fused region body is emitted by
    /// walking the region-root Node directly (`CFlag::DirectNodeJit` —
    /// Stage 1 of `design/delete_gir_ir.md`'s `compile_node`). Same
    /// observable semantics as `Jit`; only the body codegen path
    /// differs. A region whose shape `compile_node` doesn't cover yet
    /// simply doesn't fuse and node-walks (so `DirectJit` never produces
    /// a *worse* value than `Interp`).
    DirectJit,
}

impl Mode {
    pub fn flags(self) -> BitFlags<CFlag> {
        match self {
            Mode::Interp => CFlag::FusionDisabled.into(),
            Mode::Jit => BitFlags::empty(),
            Mode::DirectJit => CFlag::DirectNodeJit.into(),
        }
    }
}

/// The result of running one program under one mode.
#[derive(Debug, Clone)]
pub enum Outcome {
    /// Produced a result value.
    Value(Value),
    /// Did not compile (parse / typecheck error).
    CompileErr(String),
    /// Runtime error / the runtime died before producing a result.
    RuntimeErr(String),
    /// Produced no result within the timeout (may be a legitimate
    /// non-terminating program, or a hang).
    Timeout,
}

impl Outcome {
    /// Whether two outcomes are observably equivalent. Equality of
    /// values uses graphix's own `Value` equality (total: `-0.0 == 0.0`,
    /// `NaN == NaN`). Different outcome *kinds* (e.g. Value vs Timeout,
    /// or Value vs RuntimeErr) always disagree — that is the signal for
    /// an asymmetric hang or a fusion-introduced error. Same-kind
    /// non-value outcomes agree without comparing their (mode-dependent)
    /// messages.
    pub fn agrees_with(&self, other: &Outcome) -> bool {
        use Outcome::*;
        match (self, other) {
            (Value(a), Value(b)) => a == b,
            (CompileErr(_), CompileErr(_)) => true,
            (RuntimeErr(_), RuntimeErr(_)) => true,
            (Timeout, Timeout) => true,
            _ => false,
        }
    }

    /// Coarse variant discriminant, for the "same bug" bucket key.
    pub fn kind(&self) -> u8 {
        match self {
            Outcome::Value(_) => 0,
            Outcome::CompileErr(_) => 1,
            Outcome::RuntimeErr(_) => 2,
            Outcome::Timeout => 3,
        }
    }
}

/// Run `code` (a graphix expression) under `mode`, returning its first
/// emitted `result` value or why none came. The program is wrapped as
/// `let result = {code}` and driven to the first update of `result`.
///
/// A fresh `ExecCtx` + in-process resolver is created per call — fusion
/// state and the per-context JIT do not leak between runs (matching the
/// test harness, and avoiding cranelift codegen-context poisoning across
/// programs).
pub async fn run_program(code: &str, mode: Mode, timeout: Duration) -> Outcome {
    run_program_with_stats(code, mode, timeout).await.0
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
    let (tx, mut rx) = mpsc::channel(64);
    let wrapped = format!("let result = {code}");
    let tbl = AHashMap::from_iter([(Path::from("/test.gx"), ArcStr::from(wrapped))]);
    let resolver = ModuleResolver::VFS(tbl);
    let ctx =
        match init_with_flags_and_setup(tx, REGISTER, vec![resolver], mode.flags(), |_| {}).await {
            Ok(c) => c,
            Err(e) => {
                return (
                    Outcome::RuntimeErr(format!("runtime init failed: {e}")),
                    FusionStats::default(),
                )
            }
        };
    let base = ctx.fusion_stats().await.unwrap_or_default();
    let outcome = drive(&ctx, &mut rx, timeout).await;
    let stats = match ctx.fusion_stats().await {
        Ok(mut s) => {
            s.attempted -= base.attempted;
            s.fused -= base.fused;
            s.failed.drain(..base.failed.len());
            s
        }
        Err(_) => FusionStats::default(),
    };
    ctx.shutdown().await;
    (outcome, stats)
}

async fn drive(
    ctx: &TestCtx,
    rx: &mut mpsc::Receiver<poolshark::global::GPooled<Vec<GXEvent>>>,
    timeout: Duration,
) -> Outcome {
    let compiled = match ctx.rt.compile(arcstr::literal!("{ mod test; test::result }")).await {
        Ok(c) => c,
        Err(e) => return Outcome::CompileErr(format!("{e}")),
    };
    let eid = compiled.exprs[0].id;
    // Drain any `Updated(eid)` already in `batch`; `Some(v)` = the result.
    fn take_result(
        batch: &mut poolshark::global::GPooled<Vec<GXEvent>>,
        eid: graphix_compiler::expr::ExprId,
    ) -> Option<Value> {
        let mut found = None;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                if id == eid {
                    found = Some(v);
                }
            }
        }
        found
    }
    // Quiescence-aware wait (see `GXHandle::wait_result_or_idle`):
    //   - `Some(v)`  — `result` emitted `v` while the watch was live.
    //   - `None`     — the runtime went idle with no value for `result`.
    //                  For a synchronous program the value is normally
    //                  emitted during the compile cycle, BEFORE the watch is
    //                  registered, so it lands in `rx` rather than the watch.
    //                  Drain `rx`: the value there is the result, else the
    //                  program is genuinely bottom (div-by-zero, filtered, …).
    // `timeout` is a backstop only — a pure-sync program always settles, but
    // a future async one could loop forever without ever settling or
    // emitting `result`.
    tokio::select! {
        biased;
        r = ctx.rt.wait_result_or_idle(eid) => match r {
            Ok(Some(v)) => Outcome::Value(v),
            Ok(None) => {
                let mut found = None;
                while let Ok(mut batch) = rx.try_recv() {
                    if let Some(v) = take_result(&mut batch, eid) {
                        found = Some(v);
                    }
                }
                match found {
                    Some(v) => Outcome::Value(v),
                    None => Outcome::Timeout,
                }
            }
            Err(e) => Outcome::RuntimeErr(format!("wait_result_or_idle: {e}")),
        },
        _ = tokio::time::sleep(timeout) => Outcome::Timeout,
    }
}

/// A detected disagreement between the reference (interp = node-walk)
/// and the system under test (jit = fusion + cranelift).
#[derive(Debug, Clone)]
pub struct Divergence {
    pub code: String,
    pub interp: Outcome,
    pub jit: Outcome,
}

impl Divergence {
    /// A one-line classification. With only two evaluators left, every
    /// divergence is "node-walk vs fused+JIT" — the fusion path (GIR
    /// emit + cranelift codegen, which can't be told apart now that
    /// there's no interpreter-only mode to bisect against) produced a
    /// different result from the canonical node-walk.
    pub fn bisect(&self) -> &'static str {
        "fusion/JIT bug (interp != jit)"
    }
}

/// Run `code` under interp (node-walk) and jit (fusion + cranelift); if
/// they disagree, return the `Divergence`. `None` means they agree.
pub async fn check(code: &str, timeout: Duration) -> Option<Divergence> {
    // The two evaluators must agree, or it's a divergence. Each mode
    // spins up its own runtime, so run them concurrently — `join!`
    // overlaps their (mostly I/O-bound) execution on one task.
    let (interp, jit) = tokio::join!(
        run_program(code, Mode::Interp, timeout),
        run_program(code, Mode::Jit, timeout),
    );
    if interp.agrees_with(&jit) {
        return None;
    }
    // Suspected divergence — but first rule out nondeterminism: a value
    // whose identity/Display isn't deterministic (a lambda or abstract
    // value's id, leaked rand/time/etc.) would diverge between any two
    // runs, not just across backends. Re-run interp; if it disagrees with
    // itself, the program is nondeterministic, not a backend bug.
    let interp2 = run_program(code, Mode::Interp, timeout).await;
    if !interp.agrees_with(&interp2) {
        return None;
    }
    Some(Divergence { code: code.to_string(), interp, jit })
}

/// Coarse "same bug" key: the bisection class + the interp/jit outcome
/// kinds. Two divergences with the same bucket are treated as the same
/// bug — used by the minimizer to ensure a reduction preserves the bug
/// rather than reducing bug A into a different bug B.
fn bucket(d: &Divergence) -> (&'static str, u8, u8) {
    (d.bisect(), d.interp.kind(), d.jit.kind())
}

/// Hierarchical delta-debugging on the typed AST: repeatedly try to
/// replace a subtree with one of its children (hoist) or a minimal
/// constant, keeping any reduction that still parses, still typechecks,
/// and reproduces the SAME divergence bucket. Returns the minimized
/// program and the number of oracle checks spent (capped by `budget`).
/// Accepts partial minima — a smaller-but-not-minimal repro still beats
/// the raw mutant.
pub async fn minimize(code: &str, timeout: Duration, budget: usize) -> (String, usize) {
    let d0 = match check(code, timeout).await {
        Some(d) => d,
        None => return (code.to_string(), 1),
    };
    let target = bucket(&d0);
    let mut current = match mutate::parse(code) {
        Some(e) => e,
        None => return (code.to_string(), 1),
    };
    let mut calls = 1;
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
                if let Some(d) = check(&cand, timeout).await {
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
    (current.to_string(), calls)
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

/// What a fuzz campaign found.
#[derive(Debug, Default, Clone)]
pub struct FuzzStats {
    /// Mutants that were generated and run through the oracle.
    pub run: usize,
    /// Confirmed divergences (including duplicates of already-saved bugs).
    pub divergences: usize,
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
                    }
                }
                if let Some(n) = path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .and_then(|s| s.strip_prefix("divergence_"))
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
        let n = self
            .counter
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let body = format!(
            "// bisect: {}\n// interp: {:?}\n// jit:    {:?}\n\
             // mutant: {}\n// minimized:\n{}\n",
            d.bisect(),
            d.interp,
            d.jit,
            mutant,
            minimized,
        );
        let _ = std::fs::write(self.dir.join(format!("divergence_{n:06}.gx")), body);
        true
    }
}

/// Extract the minimized program (the text after the `// minimized:`
/// marker) from a recorded divergence file, trimmed — the dedup key.
fn extract_minimized(body: &str) -> Option<String> {
    body.split_once("// minimized:\n")
        .map(|(_, m)| m.trim().to_string())
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
        // (always valid) so the pool never stalls.
        for _ in 0..8 {
            let s = seeds[rng.below(seeds.len())];
            if let Some(p) = mutate::mutate_program(s, &donors, &mut rng, 5) {
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
) -> FuzzStats {
    let mut rng = mutate::Rng::new(seed);
    run_pool(corpus, iters, timeout, || generate::gen_program(&mut rng)).await
}

/// Worker pool. Keeps `parallelism()` oracle checks in flight over fresh
/// programs from `next_prog`. On a divergence it fires a bounded-parallel
/// task that minimizes, dedups against `corpus`, and — if the minimized
/// form is new — writes the `.gx` and prints it immediately, all WITHOUT
/// stalling the check pool (minimization is ≈80 serial checks; running it
/// inline drained the cores). `iters = None` runs forever (until killed),
/// surfacing new divergences live; `Some(n)` stops after `n` programs.
/// `next_prog` runs on the driver task (sequential, deterministic, cheap).
async fn run_pool(
    corpus: &std::sync::Arc<Corpus>,
    iters: Option<usize>,
    timeout: Duration,
    mut next_prog: impl FnMut() -> String,
) -> FuzzStats {
    use tokio::task::JoinSet;
    let par = parallelism();
    let mut stats = FuzzStats::default();
    let mut checks: JoinSet<(String, Option<Divergence>)> = JoinSet::new();
    let mut minims: JoinSet<()> = JoinSet::new();
    let mut launched = 0usize;
    let want = |launched: usize| iters.map_or(true, |n| launched < n);
    let spawn_check = |checks: &mut JoinSet<_>, prog: String| {
        checks.spawn(async move {
            let d = check(&prog, timeout).await;
            (prog, d)
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
                if let Ok((prog, div)) = res {
                    stats.run += 1;
                    if stats.run % 1000 == 0 {
                        eprintln!(
                            "  …{} run, {} divergences, {} in corpus",
                            stats.run, stats.divergences, corpus.len()
                        );
                    }
                    if let Some(d) = div {
                        stats.divergences += 1;
                        // Bound concurrent minimizations so a regressed
                        // (everything-diverges) run can't pile up unboundedly.
                        if minims.len() >= par {
                            let _ = minims.join_next().await;
                        }
                        let corpus = corpus.clone();
                        minims.spawn(async move {
                            let (min, _) = minimize(&prog, timeout, 80).await;
                            if corpus.record(&d, &prog, &min) {
                                println!("DIVERGENCE — {}", d.bisect());
                                println!("    minimized: {min}");
                                println!(
                                    "    interp={:?} jit={:?}",
                                    d.interp, d.jit
                                );
                            }
                        });
                    }
                }
                if want(launched) {
                    spawn_check(&mut checks, next_prog());
                    launched += 1;
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

    /// Stage 1 of `design/delete_gir_ir.md`. These probes pin the new
    /// `CFlag::DirectNodeJit` body-codegen path (`gir_jit::compile_node`)
    /// against both the node-walk reference (`Interp`) and the production
    /// GIR JIT (`Jit`). All three must agree on every program; a program
    /// the direct path can't compile must still produce the right value
    /// under `DirectJit` by not fusing → node-walking. When `must_fuse`
    /// is set, additionally assert (via the per-program [`FusionStats`])
    /// that at least one region actually compiled + spliced under
    /// `DirectJit` — value agreement alone can't distinguish "fused
    /// correctly" from "silently never fused" (the C5 freeze gap: no
    /// select-rooted region was ever attempted and every value test
    /// passed).
    /// How much fusion a probe demands under `DirectJit`, beyond
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

    async fn check_three(code: &str, fuse: Fuse) {
        let t = Duration::from_secs(10);
        let (interp, jit, (direct, stats)) = tokio::join!(
            run_program(code, Mode::Interp, t),
            run_program(code, Mode::Jit, t),
            run_program_with_stats(code, Mode::DirectJit, t),
        );
        assert!(
            interp.agrees_with(&jit),
            "Interp vs Jit disagree for `{code}`: {interp:?} vs {jit:?}"
        );
        assert!(
            interp.agrees_with(&direct),
            "Interp vs DirectJit disagree for `{code}`: \
             {interp:?} vs {direct:?}"
        );
        if fuse != Fuse::No {
            let why: String = stats
                .failed
                .iter()
                .map(|(id, why)| format!("\n  {id:?}: {why}"))
                .collect();
            assert!(
                stats.fused > 0,
                "expected `{code}` to fuse under DirectJit but no region \
                 compiled (attempted={}); failures:{why}",
                stats.attempted,
            );
        }
        if fuse == Fuse::Clean {
            for (id, reason) in &stats.failed {
                assert!(
                    reason.contains("node does not emit CLIF"),
                    "expected `{code}` to fuse cleanly under DirectJit \
                     but {id:?} hit a real blocker: {reason}"
                );
            }
        }
    }

    async fn all_three_agree(code: &str) {
        check_three(code, Fuse::No).await
    }

    /// [`all_three_agree`] + "it really fused": the probe is KNOWN to
    /// compile under the direct path, so a `fused == 0` run is a
    /// coverage regression even though every value still agrees.
    async fn all_three_agree_fused(code: &str) {
        check_three(code, Fuse::Some).await
    }

    /// [`all_three_agree_fused`] + "and NOTHING legitimately refused":
    /// the whole program is expected to compile into kernels, so any
    /// non-ancestor-noise blocker is a regression. Prefer this for new
    /// probes; audit a probe's full blocker profile before using it
    /// (e.g. a bare-Null `let` legitimately refuses → use `_fused`).
    async fn all_three_agree_fused_clean(code: &str) {
        check_three(code, Fuse::Clean).await
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn direct_node_jit_scalar_probes() {
        // const + bin region
        all_three_agree_fused("{ let x = i64:5; x * i64:3 }").await;
        // multiple scalar lets + nested arithmetic with a Ref read twice
        all_three_agree_fused(
            "{ let x = i64:5; let y = i64:2; (x + y) * (x - y) }",
        )
        .await;
        // div-by-zero → value-bottom (Timeout in all three via the
        // taint/guard → boundary pending → no result emitted). Fuses —
        // the bottom is a RUNTIME outcome of the compiled kernel.
        all_three_agree_fused("i64:10 / i64:0").await;
        // comparison + strict bool — `a > 3 && a < 10`
        all_three_agree_fused("{ let a = i64:7; a > i64:3 && a < i64:10 }")
            .await;
        // float arithmetic
        all_three_agree_fused("f64:3.0 + f64:1.0").await;
        // cast then float add. The original probe (`cast<f64>(7) +
        // 1.0`, no `$`) never compiled at all — `cast` returns
        // `[f64, Error]`, so it was a typecheck error in EVERY mode and
        // CompileErr == CompileErr passed silently for the test's whole
        // life (the exact bug class FusionStats exists to catch).
        // Repaired with `$`; it still doesn't fuse — the `cast`
        // CallSite doesn't emit CLIF on the direct path yet ("node
        // does not emit CLIF"), so it node-walks: deliberate fallback.
        all_three_agree("cast<f64>(i64:7)$ + f64:1.0").await;
        // Nested block: the INNER block references `outer`, which is
        // external to the inner region — so under DirectJit `outer`
        // becomes a scalar KERNEL PARAM (exercising `compile_node`'s Ref
        // arm against `env`-bound params, not just block-lets). The
        // original probe's inner block had ONE expression — a parse
        // error in every mode that `all_three_agree` accepted silently
        // (same hollow-CompileErr class as the cast probe above).
        all_three_agree_fused(
            "{ let outer = i64:100; { let t = outer - i64:1; t * i64:2 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let a = i64:9; { let b = a * i64:2; b + a } }",
        )
        .await;
    }

    /// Stage C4 probes: `?`/`$` unwrap and builtin DynCall emission on
    /// the direct path. The generated sweep can't produce these
    /// constructs (`gen_program` emits neither qop nor builtin calls),
    /// so without explicit probes a C4 regression would be invisible
    /// to the gates.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn direct_node_jit_qop_dyncall_probes() {
        // Scalar-success `$` — branchless Scalar2 unwrap of the
        // bounds-checked ArrayRef's Nullable<i64>.
        all_three_agree_fused(
            "{ let a = [i64:1, i64:2, i64:3]; a[0]$ + a[1]$ }",
        )
        .await;
        // Out-of-bounds → error → bottom in every mode (the unwrap's
        // pending path). Fuses — the bottom is a runtime outcome.
        all_three_agree_fused("{ let a = [i64:1]; a[5]$ }").await;
        // MapRef result through `$` — map access + scalar unwrap.
        all_three_agree_fused(r#"{ let m = {"a" => i64:7}; m{"a"}$ + i64:1 }"#)
            .await;
        // Value-shape success `$` (duration element) — the Value
        // unwrap arm + a Value-shape kernel return.
        all_three_agree_fused("{ let a = [duration:1.s]; a[0]$ }").await;
        // Builtin DynCall, scalar return, string arg.
        all_three_agree_fused(r#"{ let s = "hello"; str::len(s) }"#).await;
        // Builtin DynCall inside arithmetic (scalar return feeds Bin).
        all_three_agree_fused(r#"{ let s = "hello"; str::len(s) + i64:1 }"#)
            .await;
        // Builtin DynCall with String return (ret_kind 4) + owned
        // string-return kernel boundary.
        all_three_agree_fused(r#"{ let s = "abc"; str::to_upper(s) }"#).await;
        // Composite-success `$` (#199): the unwrap must re-box the
        // Value's inline ValArray bits into the composite ABI's
        // `*mut ValArray` — owned-producer and borrowed-Local inners.
        all_three_agree_fused("{ let a = [i64:1, i64:2, i64:3]; a[1..]$ }")
            .await;
        all_three_agree_fused(
            "{ let a = [i64:1, i64:2, i64:3]; let x = a[1..]; x$ }",
        )
        .await;
        all_three_agree_fused("{ let t = [(i64:1, i64:2)]; t[0]$ }").await;
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
    async fn direct_node_jit_select_probes() {
        // Literal arms + wildcard.
        all_three_agree_fused(
            "{ let x = i64:7; select x { i64:0 => i64:100, \
             i64:7 => i64:200, _ => i64:1 } }",
        )
        .await;
        // Arm bind with body arithmetic.
        all_three_agree_fused(
            "{ let x = i64:5; select x { i64:0 => i64:100, n => n * i64:2 } }",
        )
        .await;
        // Guard that fails at runtime, then one that passes.
        all_three_agree_fused(
            "{ let x = i64:3; select x { n if n > i64:10 => n, \
             n => n + i64:1 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let x = i64:42; select x { n if n > i64:10 => n * i64:2, \
             n => n } }",
        )
        .await;
        // A guard that BOTTOMS at runtime (div-by-zero) — the arm does
        // not match; the next arm wins.
        all_three_agree_fused(
            "{ let x = i64:9; select x { n if n / i64:0 == i64:1 => i64:1, \
             m => m } }",
        )
        .await;
        // Nullable scrutinee, both arm orders, both runtime values —
        // the trivially-true-first-arm order trap.
        all_three_agree_fused(
            "{ let v: [i64, null] = null; select v { i64 as _ => i64:1, \
             null as _ => i64:0 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let v: [i64, null] = null; select v { null as _ => i64:0, \
             i64 as _ => i64:1 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let v: [i64, null] = i64:42; select v { i64 as _ => i64:1, \
             null as _ => i64:0 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let v: [i64, null] = i64:42; select v { null as _ => i64:0, \
             i64 as _ => i64:1 } }",
        )
        .await;
        // Nullable RESULT (Value merge): scalar arm widens, null arm
        // packs (NULL, 0).
        all_three_agree_fused(
            "{ let v: [i64, null] = i64:42; select v { i64 as _ => i64:1, \
             null as _ => null } }",
        )
        .await;
        // Variant tag-eq + scalar payload bind.
        all_three_agree_fused(
            "{ let v: [`Add(i64), `Neg] = `Add(i64:3); \
             select v { `Add(n) => n + i64:1, `Neg => i64:0 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let v: [`Add(i64), `Neg] = `Neg; \
             select v { `Add(n) => n + i64:1, `Neg => i64:0 } }",
        )
        .await;
        // Computed scrutinee — must be evaluated exactly once and
        // reused by every arm condition.
        all_three_agree_fused(
            "{ let x = i64:5; select (x * i64:2) { i64:10 => i64:1, \
             _ => i64:0 } }",
        )
        .await;
        // Bottom scrutinee with an irrefutable final arm — no value in
        // any mode.
        all_three_agree_fused(
            "{ let x = i64:0; select (i64:10 / x) { n => n + i64:1 } }",
        )
        .await;
        // Bool-literal pair (the only typecheckable conditional final
        // arm) — exercises the unreachable miss trap.
        all_three_agree_fused(
            "{ let b = true; select b { true => i64:1, false => i64:0 } }",
        )
        .await;
        all_three_agree_fused(
            "{ let b = false; select b { true => i64:1, false => i64:0 } }",
        )
        .await;
        // String result merge.
        all_three_agree_fused(
            r#"{ let x = i64:1; select x { i64:0 => "zero", _ => "other" } }"#,
        )
        .await;
        // Nested select.
        all_three_agree_fused(
            "{ let x = i64:5; select (select x { i64:0 => i64:1, \
             n => n + i64:1 }) { i64:6 => i64:100, m => m } }",
        )
        .await;
    }

    /// Stage C6 probes: string interpolation (`emit_string_interpolate_node`,
    /// the Node twin of the GIR Concat arm) and checked arithmetic
    /// (`emit_checked_arith_node` — NEW coverage, the GIR path never
    /// lowered `+?` and friends). The generated sweep produces neither
    /// construct, so without probes a regression would be invisible.
    /// Checked-arith semantics under test: overflow / div-by-zero is a
    /// catchable error VALUE (flows through `is_err` / `$`), never
    /// bottom — the node-walk's `wrap_arith_error` core.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn direct_node_jit_string_checked_probes() {
        // Interpolation: scalar part rendered via Display.
        all_three_agree_fused(r#"{ let x = i64:7; "x is [x]" }"#).await;
        // Mixed string + scalar parts.
        all_three_agree_fused(r#"{ let a = "foo"; let b = i64:2; "[a]-[b]!" }"#)
            .await;
        // Pure string concat through interpolation.
        all_three_agree_fused(r#"{ let a = "foo"; let b = "bar"; "[a][b]" }"#)
            .await;
        // Float / bool parts (per-prim push helpers).
        all_three_agree_fused(
            r#"{ let f = f64:1.5; let b = true; "f=[f] b=[b]" }"#,
        )
        .await;
        // Interpolated literal scalar (const part).
        all_three_agree_fused(r#""n=[i64:42]""#).await;
        // A non-scalar part (Nullable from a[i]) — the restriction: the
        // INTERPOLATION doesn't fuse, node-walks to the right value in
        // every mode. Deliberate fallback, so no fused>0 assertion —
        // a SUB-region (the `a[0]` access) still fuses via the
        // attempt-then-recurse protocol, so fused>0 here would pass
        // without testing what this probe is about.
        all_three_agree(r#"{ let a = [i64:1, i64:2]; "e=[a[0]]" }"#).await;
        // Checked add/sub/mul/mod, no overflow — success unwrapped by `$`.
        all_three_agree_fused("{ let x = i64:5; (x +? i64:3)$ }").await;
        all_three_agree_fused(
            "{ let x = i64:10; (x -? i64:3)$ * (i64:2 *? i64:3)$ }",
        )
        .await;
        all_three_agree_fused("{ let x = i64:10; (x %? i64:3)$ }").await;
        // Overflow → the ArithError error VALUE (catchable, not bottom).
        all_three_agree_fused("i64:9223372036854775807 +? i64:1").await;
        all_three_agree_fused("is_err(i64:9223372036854775807 +? i64:1)")
            .await;
        // `0 /? 0` → error value through is_err — node-walk semantics:
        // checked div0 FLOWS (unlike unchecked div0, which is bottom).
        all_three_agree_fused("is_err(i64:0 /? i64:0)").await;
        // Overflow through `$` — the error drops, bottom in every mode.
        all_three_agree_fused("(i64:9223372036854775807 +? i64:1)$").await;
        // Checked arith inside a larger expression (select consumes the
        // [T, Error] union).
        all_three_agree_fused(
            "{ let x = i64:6; select (x +? i64:1) { i64 as n => n * i64:2, \
             _ => i64:0 } }",
        )
        .await;
        // Checked result interpolated after unwrap — the unwrapped part
        // is a possibly-bottom Scalar2, which the interpolate relay's
        // `.single()` refuses (the GIR `compile_scalar` parity): the
        // INTERPOLATION doesn't fuse, node-walks to the right value in
        // every mode. Deliberate fallback (same sub-region caveat as
        // the Nullable-part probe above).
        all_three_agree(r#"{ let x = i64:5; "v=[(x +? i64:1)$]" }"#).await;
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
    async fn direct_node_jit_composite_probes() {
        // A tuple-let + tuple accessors.
        all_three_agree_fused("{ let t = (i64:1, i64:2); t.0 + t.1 }").await;
        // A struct-let + field accessors.
        all_three_agree_fused("{ let s = { a: i64:4, b: i64:5 }; s.a + s.b }")
            .await;
    }

    /// Stage D2 probes: inline `array::map` emission on the direct
    /// path (`Apply::emit_clif` on MapQ → `MapFn::emit_clif` on the
    /// array package's MapImpl → `scaffold::emit_map_loop`). V1 scope:
    /// BORROWED input arrays + single-name callbacks; the last two
    /// probes pin the deliberate V1 fallbacks (owned input array,
    /// destructured callback) as value-agreeing node-walks — flip them
    /// to `all_three_agree_fused` when the owned-arg stage / D3 land.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn direct_node_jit_map_probes() {
        // scalar → scalar
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; array::map(a, |x| x * i64:2) }",
        )
        .await;
        // scalar → tuple (composite out, owned push)
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; array::map(a, |x| (x, x * i64:2)) }",
        )
        .await;
        // composite (tuple) element + accessors in the body
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |p| p.0 + p.1) }",
        )
        .await;
        // scalar → Nullable out (select body, Value-shape push)
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             array::map(a, |x| select x { i64:1 => i64:10, _ => null }) }",
        )
        .await;
        // capture: the body reads an outer scalar (a kernel param
        // under DirectJit — BindId-first resolution next to the
        // BindId-bound loop element)
        all_three_agree_fused_clean(
            "{ let k = i64:10; let a = [i64:1, i64:2]; \
             array::map(a, |x| x * k) }",
        )
        .await;
        // Nested map-in-map: does NOT inline on either path — the
        // inner CallSite lives in the callback's lambda body, which
        // `resolve_static_calls` never descends into, so the inner
        // MapQ has no analysis_pred (and no bound function) at
        // emission time. Classic has the identical gap (its emit_gir
        // path hits the same unresolved inner site); the runtime
        // per-slot machinery carries correctness. Flip to
        // `all_three_agree_fused` when static resolution descends
        // into lambda bodies (Stage E callee-prepass territory).
        all_three_agree(
            "{ let a = [[i64:1, i64:2], [i64:3]]; \
             array::map(a, |row| array::map(row, |x| x + i64:1)) }",
        )
        .await;
        // string out (push_string)
        all_three_agree_fused_clean(
            r#"{ let a = [i64:1, i64:2]; array::map(a, |x| "v[x]") }"#,
        )
        .await;
        // qop in the body — a may-bottom (Scalar2) field, push_field's
        // RUNTIME bottom-abort seam (no overflow here, so values flow)
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; array::map(a, |x| (x +? i64:1)$) }",
        )
        .await;
        // OWNED input array (a fresh slice producer) — the scaffold
        // adopts it (owned_input_stack registration: pending exits
        // free it, the normal path drops it after the loop).
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; array::map((a[1..])$, |x| x) }",
        )
        .await;
        // Destructured `|(k, v)|` callback — D3: per-leaf BindId-bound
        // reads off the composite element.
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; array::map(a, |(k, v)| k + v) }",
        )
        .await;
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
    async fn direct_node_jit_filter_probes() {
        // scalar element, comparison predicate
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter(a, |x| x > i64:2) }",
        )
        .await;
        // bool element, bare-ref predicate
        all_three_agree_fused_clean(
            "{ let a = [true, false, true]; array::filter(a, |x| x) }",
        )
        .await;
        // composite (tuple) element + accessors in the predicate —
        // EXCEEDS classic: emit_gir requires a register-scalar element
        // for single-name callbacks, the direct path binds composites
        // (keep MOVES the element, not-keep takes the drop_block)
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::filter(a, |p| p.0 > p.1) }",
        )
        .await;
        // capture: the predicate reads an outer scalar
        all_three_agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter(a, |x| x > k) }",
        )
        .await;
        // select in the predicate
        all_three_agree_fused_clean(
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
        all_three_agree(
            "{ let a = [i64:1, i64:5, i64:20]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // ...and with an actual 0: the pred slot for that element is
        // bottom, so filter's output NEVER fires — every mode times
        // out. Pins the canonical blocking semantics (a runtime
        // keep-vs-drop guess in a fused kernel would produce a value
        // here and diverge).
        all_three_agree(
            "{ let a = [i64:0, i64:1, i64:5]; \
             array::filter(a, |x| i64:10 / x > i64:1) }",
        )
        .await;
        // string element — outside bind_elem's V1 shapes (#150) →
        // Ok(None) → node-walk. Flip when string HOF elements land.
        all_three_agree(
            r#"{ let a = ["aa", "b"]; array::filter(a, |s| s == "aa") }"#,
        )
        .await;
        // OWNED input array (fresh slice producer) — adopted by the
        // scaffold, same as the map probe.
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter((a[1..])$, |x| x > i64:1) }",
        )
        .await;
        // Destructured `|(k, v)|` predicate — D3 (the kept element is
        // still the whole tuple)
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; array::filter(a, |(k, v)| k < v) }",
        )
        .await;
    }

    /// Stage D2 probes: inline `array::fold` emission on the direct
    /// path (FoldQ's `Apply::emit_clif` orchestration →
    /// `FoldImpl::emit_clif` → `scaffold::emit_fold_loop`). The
    /// accumulator threads through the loop as a register Variable
    /// (BindId-bound — the acc and elem resolve BindId-first next to
    /// any same-named outer capture). Fold's contract probes: a
    /// may-bottom INIT or BODY de-fuses at BUILD time — the plan's
    /// explicit parity fixture ("a may-bottom fold body must de-fuse,
    /// not runtime-abort").
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn direct_node_jit_fold_probes() {
        // scalar sum
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::fold(a, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // computed init + capture in the body
        all_three_agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::fold(a, k * i64:10, |acc, x| acc + x * k) }",
        )
        .await;
        // composite (tuple) element + accessors — EXCEEDS classic
        // (emit_gir requires a register-scalar element for single-name
        // callbacks)
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::fold(a, i64:0, |acc, p| acc + p.0 * p.1) }",
        )
        .await;
        // select in the body (acc threading through arms)
        all_three_agree_fused_clean(
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
        all_three_agree_fused_clean(
            "{ let acc = i64:100; let a = [i64:1, i64:2]; \
             array::fold(a, acc, |acc, x| acc + x) }",
        )
        .await;
        // HOF callsite in OPERAND position (under the `+`) — pre-#204
        // neither path statically resolved it (static_resolve only
        // descended the Module/Block/Bind/CallSite spine). Now the
        // full-position traversal resolves it and the whole block
        // fuses as one region.
        all_three_agree_fused_clean(
            "{ let k = i64:100; let a = [i64:1, i64:2]; \
             k + array::fold(a, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // #204 position coverage: HOF in a SELECT ARM...
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; let x = i64:1; \
             select x { \
               i64:1 => array::fold(a, i64:0, |acc, y| acc + y), \
               _ => i64:0 } }",
        )
        .await;
        // ...and as an ARRAY-LITERAL ELEMENT.
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             [array::fold(a, i64:0, |acc, x| acc + x), i64:5] }",
        )
        .await;
        // STATICALLY may-bottom BODY (div by the element, no zero
        // present): de-fuses at build, node-walks to a real value all
        // modes agree on — the plan's explicit fold parity fixture.
        all_three_agree(
            "{ let a = [i64:1, i64:2]; \
             array::fold(a, i64:100, |acc, x| acc / x) }",
        )
        .await;
        // STATICALLY may-bottom INIT (same contract, the init seam)
        all_three_agree(
            "{ let n = i64:2; let a = [i64:1, i64:2]; \
             array::fold(a, i64:10 / n, |acc, x| acc + x) }",
        )
        .await;
        // string accumulator — not a register scalar → Ok(None) →
        // node-walk. Flip if/when value-shape accumulators land.
        all_three_agree(
            r#"{ let a = [i64:1, i64:2]; array::fold(a, "", |acc, x| "[acc][x]") }"#,
        )
        .await;
        // OWNED input array (fresh slice producer) — adopted by the
        // scaffold.
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold((a[1..])$, i64:0, |acc, x| acc + x) }",
        )
        .await;
        // Destructured `|acc, (k, v)|` callback — D3 (acc + leaves
        // all BindId-bound in the loop scope)
        all_three_agree_fused_clean(
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
    async fn direct_node_jit_flat_map_probes() {
        // scalar element → fresh array body
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x, x * i64:10]) }",
        )
        .await;
        // composite (tuple) element flattened to its fields —
        // EXCEEDS classic (register-scalar-element gate there)
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::flat_map(a, |p| [p.0, p.1]) }",
        )
        .await;
        // capture in the body
        all_three_agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| [x * k]) }",
        )
        .await;
        // BORROWED body source: the body is a Ref to an outer array,
        // so the scaffold's extend would consume the env's value —
        // `ensure_owned_composite_src` clones it per iteration
        all_three_agree_fused_clean(
            "{ let b = [i64:9]; let a = [i64:1, i64:2]; \
             array::flat_map(a, |x| b) }",
        )
        .await;
        // bare-element body — the OTHER branch of the callback union;
        // not Array-typed → Ok(None) → node-walk (classic parity)
        all_three_agree(
            "{ let a = [i64:1, i64:2]; array::flat_map(a, |x| x) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::flat_map((a[1..])$, |x| [x]) }",
        )
        .await;
        // Destructured callback — D3
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2)]; \
             array::flat_map(a, |(k, v)| [k, v]) }",
        )
        .await;
    }

    /// D3 probes: destructured `|(k, v)|` callbacks — per-leaf
    /// BindId-bound reads off the owned composite element
    /// (`HofElem::leaves` via `scalar_leaves`).
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn direct_node_jit_destructure_probes() {
        // mixed-prim leaves (i64, f64), f64 result
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, f64:2.5), (i64:3, f64:0.5)]; \
             array::map(a, |(k, v)| v) }",
        )
        .await;
        // sparse leaves: `_` positions get no bind (and no read)
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:4)]; \
             array::map(a, |(k, _)| k * i64:10) }",
        )
        .await;
        // find with a destructured predicate — the result is the
        // whole matched tuple
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |(k, v)| k > v) }",
        )
        .await;
        // 3-leaf tuple through fold
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2, i64:3), (i64:4, i64:5, i64:6)]; \
             array::fold(a, i64:0, |acc, (x, y, z)| acc + x * y + z) }",
        )
        .await;
        // composite leaf — outside the register-scalar V1 →
        // Ok(None) → node-walk (flip when composite leaves land)
        all_three_agree(
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
    async fn direct_node_jit_filter_map_probes() {
        // select-bodied Nullable: keep evens doubled
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::filter_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // capture in the body
        all_three_agree_fused_clean(
            "{ let k = i64:2; let a = [i64:1, i64:2, i64:3]; \
             array::filter_map(a, |x| \
               select x { i64:2 => x * k, _ => null }) }",
        )
        .await;
        // composite element — outside the scalar-only scaffold →
        // Ok(None) → node-walk (classic parity; widen with #150)
        all_three_agree(
            "{ let a = [(i64:1, i64:2)]; \
             array::filter_map(a, |p| \
               select p.0 { i64:1 => p.1, _ => null }) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold
        all_three_agree_fused_clean(
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
    async fn direct_node_jit_find_probes() {
        // scalar element, found
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:5, i64:3]; \
             array::find(a, |x| x > i64:2) }",
        )
        .await;
        // scalar element, NOT found (null result)
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2]; array::find(a, |x| x > i64:9) }",
        )
        .await;
        // composite (tuple) element + accessor predicate — the found
        // element is consumed into the Nullable result, not-matched
        // ones drop per iteration. EXCEEDS classic for single-name
        // callbacks.
        all_three_agree_fused_clean(
            "{ let a = [(i64:1, i64:2), (i64:3, i64:1)]; \
             array::find(a, |p| p.0 > p.1) }",
        )
        .await;
        // may-bottom predicate — build-time de-fuse, runtime-clean
        all_three_agree(
            "{ let a = [i64:1, i64:5]; \
             array::find(a, |x| i64:10 / x > i64:4) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold (the early-exit
        // edges and the not-found edge all route through the shared
        // exit where the input drops)
        all_three_agree_fused_clean(
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
    async fn direct_node_jit_find_map_probes() {
        // found: first even, doubled
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x * i64:10, _ => null }) }",
        )
        .await;
        // not found → null
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:3]; \
             array::find_map(a, |x| \
               select x % i64:2 { i64:0 => x, _ => null }) }",
        )
        .await;
        // OWNED input array — adopted by the scaffold
        all_three_agree_fused_clean(
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
    async fn direct_node_jit_owned_input_probes() {
        // array literal as the DIRECT argument
        all_three_agree_fused_clean(
            "array::map([i64:1, i64:2, i64:3], |x| x * i64:2)",
        )
        .await;
        // PIPELINE: filter over an inlined map — two loops, one kernel
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::filter(array::map(a, |x| x * i64:2), |x| x > i64:2) }",
        )
        .await;
        // PIPELINE: fold over an inlined map
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3]; \
             array::fold(array::map(a, |x| x * x), i64:0, |acc, x| acc + x) }",
        )
        .await;
        // PIPELINE: find over an inlined filter (early exit consumes
        // an adopted intermediate)
        all_three_agree_fused_clean(
            "{ let a = [i64:1, i64:2, i64:3, i64:4]; \
             array::find(array::filter(a, |x| x % i64:2 == i64:0), \
               |x| x > i64:2) }",
        )
        .await;
        // PIPELINE feeding init's output into flat_map
        all_three_agree_fused_clean(
            "array::flat_map(array::init(i64:3, |i| i), |x| [x, x])",
        )
        .await;
        // PENDING path through an adopted input: the outer map's body
        // bottom-aborts mid-loop (i64::MAX overflow via `+?` then `$`)
        // while the inner map's result is adopted — the pending
        // cleanup must free it via `owned_input_stack` (a wrong-
        // destructor or double-free would crash the JIT mode; the
        // canonical outcome is a blocked output, Timeout in every
        // mode).
        all_three_agree(
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
    async fn direct_node_jit_init_probes() {
        // scalar body
        all_three_agree_fused_clean("array::init(i64:4, |i| i * i)").await;
        // composite (tuple) body
        all_three_agree_fused_clean("array::init(i64:3, |i| (i, i + i64:1))")
            .await;
        // capture in the body
        all_three_agree_fused_clean(
            "{ let k = i64:10; array::init(i64:3, |i| i * k) }",
        )
        .await;
        // computed n with a capture
        all_three_agree_fused_clean(
            "{ let n = i64:2; array::init(n + i64:1, |i| i) }",
        )
        .await;
        // negative n clamps to the empty array (the scaffold's
        // node-walk-parity clamp)
        all_three_agree_fused_clean("array::init(i64:0 - i64:2, |i| i)")
            .await;
        // may-bottom n (div by a binding) — build-time de-fuse,
        // runtime-clean
        all_three_agree(
            "{ let d = i64:2; array::init(i64:4 / d, |i| i) }",
        )
        .await;
    }

    /// Broad differential sweep: the type-directed generator produces
    /// scalar / tuple / array / select programs. For EVERY one, `Interp`
    /// (node-walk reference) and `DirectJit` (the new `compile_node`
    /// path, falling back to node-walk on any unsupported shape) must
    /// agree. A scalar program exercises `compile_node`; a non-scalar one
    /// exercises the fallback. Deterministic seed → reproducible.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn direct_node_jit_generated_sweep() {
        use crate::generate::gen_program;
        use crate::mutate::Rng;
        let t = Duration::from_secs(10);
        let mut rng = Rng::new(0xD17EC7);
        let mut fused = 0usize;
        for _ in 0..120 {
            let code = gen_program(&mut rng);
            let (interp, (direct, stats)) = tokio::join!(
                run_program(&code, Mode::Interp, t),
                run_program_with_stats(&code, Mode::DirectJit, t),
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
                    "Interp vs DirectJit diverge for `{code}`: \
                     {interp:?} vs {direct:?}"
                );
            }
        }
        // The live coverage number — visible in every `--nocapture`
        // run, no instrumentation ritual required.
        eprintln!("sweep: {fused} regions fused across 120 programs");
    }
}
