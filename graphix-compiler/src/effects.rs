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

/// A builtin's one classification (`BuiltIn::EFFECT`), the three
/// questions an author answers in one place: does every output land
/// on the cycle of its trigger; does an invocation's result depend on
/// anything but its arguments; and can the JIT call it directly.
#[derive(Debug, Clone, Copy)]
pub enum Effect {
    /// Input on cycle K may produce output on a later cycle,
    /// autonomously, or never. The conservative default.
    Async,
    /// Same-cycle, and the instance holds cross-invocation state
    /// (`count`/`sum`/`uniq`/`once` accumulate or remember), or its
    /// result depends on WHICH arguments were delivered (the
    /// partial-delivery producers: `opt::or`, `filter_err`, `divide`).
    Sync,
    /// Same-cycle, and an invocation's result depends only on its
    /// arguments — no cross-invocation state, an internal memo or
    /// scratch buffer allowed, effects allowed (`print`/`log`/`exit`
    /// emit once whichever instance runs them). The tail-loop
    /// collapse gate (`analysis::lambda_is_stateless`) reuses ONE
    /// activation across a tail loop's iterations only when every
    /// builtin it reaches is `Stateless`; a wrong `Stateless` is a
    /// semantics bug (iterations would share per-iteration state), a
    /// wrong `Sync` only costs the loop. The payload is the direct-call
    /// entry the JIT uses at every fused site (`FastCall`), or `None`
    /// for a builtin that can never be one: an effect (a kernel is a
    /// pure function of its inputs and may re-evaluate) or a
    /// partial-delivery producer (a fast fn sees every argument
    /// present). Under strict fusion a builtin fuses iff it carries a
    /// fast fn; everything else node-walks (design/strict_fusion.md).
    Stateless(Option<crate::FastCall>),
}

impl Effect {
    /// The sync/async lattice fact effect inference reads.
    pub fn kind(self) -> EffectKind {
        match self {
            Effect::Async => EffectKind::Async,
            Effect::Sync | Effect::Stateless(_) => EffectKind::Sync,
        }
    }

    pub fn is_stateless(self) -> bool {
        matches!(self, Effect::Stateless(_))
    }

    pub fn fastcall(self) -> Option<crate::FastCall> {
        match self {
            Effect::Stateless(f) => f,
            Effect::Async | Effect::Sync => None,
        }
    }
}

/// The declared facts of a registered builtin, recorded by
/// `ExecCtx::register_builtin` from [`Effect`] and looked up by name
/// (`ExecCtx::builtin_effect` / `ExecCtx::builtin_stateless` /
/// `ExecCtx::builtin_fastcall`). `default()` is the conservative
/// reading for an unregistered name: `Async` + stateful.
#[derive(Debug, Clone, Copy, Default)]
pub struct BuiltinFacts {
    pub effect: EffectKind,
    pub stateless: bool,
    pub fastcall: Option<crate::FastCall>,
}

impl From<Effect> for BuiltinFacts {
    fn from(e: Effect) -> Self {
        BuiltinFacts {
            effect: e.kind(),
            stateless: e.is_stateless(),
            fastcall: e.fastcall(),
        }
    }
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
