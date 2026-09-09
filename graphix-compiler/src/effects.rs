//! Sync/async effect classification. An operation is `Sync` when every
//! output lands on the cycle of its trigger and `Async` otherwise;
//! async operations are fusion boundaries. Builtin effects are declared
//! via `BuiltIn::EFFECT`; user-function effects are inferred.

/// The sync/async lattice: `Sync ⊔ Sync = Sync`, everything else is
/// `Async`. `Async` is the conservative default.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectKind {
    /// Same-cycle: input on cycle K → output (or nothing) on cycle K.
    Sync,
    /// Possibly-later-cycle: input on cycle K may produce output on a
    /// later cycle, autonomously, or never.
    Async,
}

impl EffectKind {
    /// Lattice join: `Async` absorbs.
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
    /// `Async`.
    fn default() -> Self {
        Self::Async
    }
}

/// A builtin's classification (`BuiltIn::EFFECT`): does every output
/// land on the cycle of its trigger, does the result depend on anything
/// but the arguments, and can the JIT call it directly.
#[derive(Debug, Clone, Copy)]
pub enum Effect {
    /// Input on cycle K may produce output on a later cycle,
    /// autonomously, or never. The conservative default.
    Async,
    /// Same-cycle, but the instance holds cross-invocation state or its
    /// result depends on which arguments were delivered.
    Sync,
    /// Same-cycle and a pure function of its arguments (memos, scratch
    /// buffers and one-shot effects allowed). A wrong `Stateless` is a
    /// semantics bug: tail-loop iterations would share state. The
    /// payload is the JIT's direct-call entry; `None` for a builtin
    /// that must not be called from a kernel (an effect that may
    /// re-evaluate, or one that needs partial argument delivery).
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

/// The facts of a registered builtin, recorded from [`Effect`] and
/// looked up by name. `default()` is the reading for an unregistered
/// name: `Async` and stateful.
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

/// How a lambda recurses on its own `LambdaId`. Diagnostic only: the
/// operational gate is `GXLambda::tail_loop` plus the per-call-site
/// `is_self_tail_call` flag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum RecursionKind {
    /// No call in the body reaches this lambda; also the default for a
    /// lambda the analysis never reached.
    #[default]
    NotRecursive,
    /// Self-recursive outside tail position.
    Recursive,
    /// Self-recursive in tail position with loop-able formals.
    TailRecursive,
}
