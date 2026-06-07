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
    let sleep = tokio::time::sleep(timeout);
    tokio::pin!(sleep);
    loop {
        tokio::select! {
            _ = &mut sleep => break Outcome::Timeout,
            batch = rx.recv() => match batch {
                None => break Outcome::RuntimeErr("runtime died".into()),
                Some(mut batch) => {
                    let mut found = None;
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            if id == eid {
                                found = Some(v);
                            }
                        }
                    }
                    if let Some(v) = found {
                        break Outcome::Value(v);
                    }
                }
            }
        }
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
    let interp = run_program(code, Mode::Interp, timeout).await;
    let jit = run_program(code, Mode::Jit, timeout).await;
    if interp.agrees_with(&jit) {
        return None;
    }
    let fused = run_program(code, Mode::Fused, timeout).await;
    Some(Divergence { code: code.to_string(), interp, fused, jit })
}
