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
use graphix_compiler::{expr::ModuleResolver, CFlag};
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Node-walk interpreter (`CFlag::FusionDisabled`) — the reference.
    Interp,
    /// Fusion on the interpreter (`CFlag::JitDisabled`).
    Fused,
    /// Fusion + cranelift JIT (no flags) — the system under test.
    Jit,
}

impl Mode {
    pub fn flags(self) -> BitFlags<CFlag> {
        match self {
            Mode::Interp => CFlag::FusionDisabled.into(),
            Mode::Fused => CFlag::JitDisabled.into(),
            Mode::Jit => BitFlags::empty(),
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
    let (tx, mut rx) = mpsc::channel(64);
    let wrapped = format!("let result = {code}");
    let tbl = AHashMap::from_iter([(Path::from("/test.gx"), ArcStr::from(wrapped))]);
    let resolver = ModuleResolver::VFS(tbl);
    let ctx =
        match init_with_flags_and_setup(tx, REGISTER, vec![resolver], mode.flags(), |_| {}).await {
            Ok(c) => c,
            Err(e) => return Outcome::RuntimeErr(format!("runtime init failed: {e}")),
        };
    let outcome = drive(&ctx, &mut rx, timeout).await;
    ctx.shutdown().await;
    outcome
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

/// A detected disagreement between the reference (interp) and the system
/// under test (jit), with the bisection (fused) outcome included.
#[derive(Debug, Clone)]
pub struct Divergence {
    pub code: String,
    pub interp: Outcome,
    pub fused: Outcome,
    pub jit: Outcome,
}

impl Divergence {
    /// A one-line classification of where the divergence first appears,
    /// mirroring how the #162–#170 cascade was diagnosed:
    /// `interp == fused != jit` ⇒ a cranelift codegen bug;
    /// `interp != fused` ⇒ a GIR/emit (fusion-lowering) bug.
    pub fn bisect(&self) -> &'static str {
        if !self.interp.agrees_with(&self.fused) {
            "GIR/emit bug (interp != fused)"
        } else if !self.fused.agrees_with(&self.jit) {
            "cranelift codegen bug (interp == fused != jit)"
        } else {
            // interp != jit but interp == fused == jit pairwise: a
            // non-transitive Value comparison or a flaky/nondeterministic
            // program.
            "non-transitive or nondeterministic (re-check determinism)"
        }
    }
}

/// Run `code` under interp and jit; if they disagree, also run fused for
/// bisection and return the `Divergence`. `None` means the modes agree.
pub async fn check(code: &str, timeout: Duration) -> Option<Divergence> {
    // Compare ALL THREE modes — interp (reference), fused (interp
    // kernels), jit. Comparing only interp-vs-jit is blind to the
    // `interp == jit != fused` class (a gir-interp bug the JIT dodges via
    // node-walk fallback) — which is exactly where several real bugs
    // lived. All three must agree, or it's a divergence. Each mode spins
    // up its own runtime, so run all three concurrently — `join!` overlaps
    // their (mostly I/O-bound) execution on one task, tripling the cores a
    // single check keeps busy.
    let (interp, fused, jit) = tokio::join!(
        run_program(code, Mode::Interp, timeout),
        run_program(code, Mode::Fused, timeout),
        run_program(code, Mode::Jit, timeout),
    );
    if interp.agrees_with(&fused) && interp.agrees_with(&jit) {
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
    Some(Divergence { code: code.to_string(), interp, fused, jit })
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
    let mut spawn_one = |set: &mut JoinSet<_>, i: usize| {
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
    /// Confirmed divergences.
    pub divergences: usize,
}

/// Source-A campaign: mutate corpus seeds and run each mutant through the
/// oracle, recording divergences under `out_dir`. Deterministic in
/// `seed` — the same seed replays the same run.
pub async fn fuzz(
    iters: usize,
    seed: u64,
    timeout: Duration,
    out_dir: &std::path::Path,
) -> FuzzStats {
    let seeds = corpus::all_seeds();
    let donors = mutate::donor_pool(&seeds);
    let mut rng = mutate::Rng::new(seed);
    run_pool(iters, timeout, out_dir, || {
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
    iters: usize,
    seed: u64,
    timeout: Duration,
    out_dir: &std::path::Path,
) -> FuzzStats {
    let mut rng = mutate::Rng::new(seed);
    run_pool(iters, timeout, out_dir, || generate::gen_program(&mut rng)).await
}

/// Worker pool, two saturating phases:
///   1. Keep `parallelism()` oracle checks in flight over fresh programs
///      from `next_prog`, collecting raw divergences. Minimization is NOT
///      done inline — it's a long serial section (≈80 sequential checks)
///      that would drain the pool and idle the cores between bursts.
///   2. Minimize the found divergences in a second bounded-parallel pool —
///      each minimize is internally serial, but they "pair off" across
///      divergences, so the cores stay busy through the reduction pass too.
/// `next_prog` runs on the driver task (sequential, deterministic, cheap);
/// only `check`/`minimize` are parallel. Output is sorted by program index
/// so it stays deterministic despite parallel completion order.
async fn run_pool(
    iters: usize,
    timeout: Duration,
    out_dir: &std::path::Path,
    mut next_prog: impl FnMut() -> String,
) -> FuzzStats {
    use tokio::task::JoinSet;
    let par = parallelism();
    let mut stats = FuzzStats::default();

    // Phase 1 — saturating run, collect raw divergences only.
    let mut set: JoinSet<(usize, String, Option<Divergence>)> = JoinSet::new();
    let mut next = 0usize;
    let spawn_one = |set: &mut JoinSet<_>, idx: usize, prog: String| {
        set.spawn(async move {
            let d = check(&prog, timeout).await;
            (idx, prog, d)
        });
    };
    while next < iters && set.len() < par {
        let p = next_prog();
        spawn_one(&mut set, next, p);
        next += 1;
    }
    let mut found: Vec<(usize, String, Divergence)> = Vec::new();
    while let Some(res) = set.join_next().await {
        if let Ok((idx, prog, div)) = res {
            stats.run += 1;
            if let Some(d) = div {
                stats.divergences += 1;
                found.push((idx, prog, d));
            }
        }
        if next < iters {
            let p = next_prog();
            spawn_one(&mut set, next, p);
            next += 1;
        }
    }

    // Phase 2 — minimize in parallel, capped at `par` concurrent.
    let mut mset: JoinSet<(usize, String, Divergence, String, usize)> = JoinSet::new();
    let mut fi = found.into_iter();
    let spawn_min = |mset: &mut JoinSet<_>, idx: usize, prog: String, d: Divergence| {
        mset.spawn(async move {
            let (minimized, mcalls) = minimize(&prog, timeout, 80).await;
            (idx, prog, d, minimized, mcalls)
        });
    };
    for _ in 0..par {
        match fi.next() {
            Some((idx, prog, d)) => spawn_min(&mut mset, idx, prog, d),
            None => break,
        }
    }
    let mut results = Vec::new();
    while let Some(res) = mset.join_next().await {
        if let Ok(r) = res {
            results.push(r);
        }
        if let Some((idx, prog, d)) = fi.next() {
            spawn_min(&mut mset, idx, prog, d);
        }
    }

    // Deterministic output (parallel completion order is nondeterministic).
    results.sort_by_key(|(idx, ..)| *idx);
    for (idx, prog, d, minimized, mcalls) in results {
        record_divergence(out_dir, &d, &prog, &minimized, idx);
        println!("[{idx}] DIVERGENCE — {}", d.bisect());
        println!("    program:   {prog}");
        println!("    minimized: {minimized}  ({mcalls} checks)");
        println!("    interp={:?} fused={:?} jit={:?}", d.interp, d.fused, d.jit);
    }
    stats
}

fn record_divergence(
    out_dir: &std::path::Path,
    d: &Divergence,
    mutant: &str,
    minimized: &str,
    i: usize,
) {
    let _ = std::fs::create_dir_all(out_dir);
    let body = format!(
        "// bisect: {}\n// interp: {:?}\n// fused:  {:?}\n// jit:    {:?}\n\
         // mutant: {}\n// minimized:\n{}\n",
        d.bisect(),
        d.interp,
        d.fused,
        d.jit,
        mutant,
        minimized,
    );
    let _ = std::fs::write(out_dir.join(format!("divergence_{i:06}.gx")), body);
}
