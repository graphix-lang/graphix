//! Sync/async effect classification for fusion.
//!
//! See `design/whole_graph_fusion.md` for the full design. The TL;DR:
//! every operation in the dataflow graph is classified `Sync` or
//! `Async` based on whether it can produce an output on a cycle later
//! than the trigger that activated it. Sync operations can be fused
//! into a single fused kernel; async operations form fusion boundaries
//! that the runtime mediates.
//!
//! This module owns the `EffectKind` lattice and the rules for joining
//! effects across operations. Builtin effects are declared via
//! `BuiltIn::EFFECT`; user-function effects are inferred (M6).

/// The intrinsic effect of a function or expression with respect to
/// fusion.
///
/// `Sync` means: every output the operation produces appears on the
/// same cycle as the input that triggered it (or it produces no output
/// for that input). `Sync` operations are fusion candidates — multiple
/// `Sync` operations can collapse into a single fused kernel.
///
/// `Async` means: the operation may produce output on a cycle later
/// than the trigger that activated it. Async operations are fusion
/// boundaries — the runtime mediates between the kernel that produced
/// the trigger and the consumer of the async output.
///
/// The lattice is `Sync ⊔ Sync = Sync`, everything else `= Async`.
/// `Async` is the conservative default — code that hasn't been
/// classified is treated as async, which is always correct (just
/// loses fusion opportunity).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectKind {
    /// Same-cycle: input on cycle K → output (or nothing) on cycle K.
    Sync,
    /// Possibly-later-cycle: input on cycle K may produce output on a
    /// later cycle, autonomously, or never.
    Async,
}

impl EffectKind {
    /// Lattice join. `Sync ⊔ Sync = Sync`, anything with an `Async` is
    /// `Async`. Use this to combine the effect of a callee with the
    /// effects of its function-typed arguments at a call site, or to
    /// fold across a body looking for any async edge.
    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Sync, Self::Sync) => Self::Sync,
            _ => Self::Async,
        }
    }

    /// True if this effect represents a fusion candidate.
    pub fn is_sync(self) -> bool {
        matches!(self, Self::Sync)
    }

    /// True if this effect represents a fusion boundary.
    pub fn is_async(self) -> bool {
        matches!(self, Self::Async)
    }
}

impl Default for EffectKind {
    /// Conservative default: `Async`. Anything that hasn't been
    /// explicitly classified must not be fused through.
    fn default() -> Self {
        Self::Async
    }
}

/// The declared facts of a registered builtin, recorded by
/// `ExecCtx::register_builtin` from the `BuiltIn` trait consts and
/// looked up by name (`ExecCtx::builtin_effect` /
/// `ExecCtx::builtin_stateless`). `default()` is the conservative
/// reading for an unregistered name: `Async` + stateful.
#[derive(Debug, Clone, Copy, Default)]
pub struct BuiltinFacts {
    /// `BuiltIn::EFFECT` — see [`EffectKind`].
    pub effect: EffectKind,
    /// `BuiltIn::STATELESS` — delete-and-reinit is unobservable (no
    /// cross-invocation state, no per-invocation external effect).
    pub stateless: bool,
}

/// How a lambda recurses with respect to its own `LambdaId`. A summary
/// computed by the analysis pass (`analysis::analyze`) alongside the
/// per-call-site tail facts. This is a human/diagnostic summary — the
/// OPERATIONAL gate that makes the interpreter loop (and the JIT emit a
/// native loop) is the per-`GXLambda` `tail_loop` bit + the per-call-site
/// `is_self_tail_call` flag, not this enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum RecursionKind {
    /// No call in the body can reach this lambda's own `LambdaId`. Also
    /// the default for a lambda the analysis never reached (dynamic-only
    /// callees) — safe, since the operational gate is independent.
    #[default]
    NotRecursive,
    /// Self-recursive, but the recursive call is not in tail position
    /// (both backends recurse on the native stack).
    Recursive,
    /// Self-recursive in tail position with loop-able formals — the
    /// interpreter loops in place and the JIT emits a native loop.
    TailRecursive,
}
