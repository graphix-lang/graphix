//! Sync/async effect classification. An operation is `Sync` when every
//! output lands on the cycle of its trigger and `Async` otherwise.
//! Builtin effects are declared via `BuiltIn::EFFECT`; user-function
//! effects are inferred. Fusion admits only `Effect::Stateless` with a
//! `FastCall` (`design/strict_fusion.md`): a `Sync` builtin keeps state
//! and node-walks like an `Async` one.

/// The two facts the fixpoint infers per lambda from an optimistic
/// start: `effect` (`Sync` degrading to `Async`) and `stateless` (no
/// per-activation state: every builtin reached is `Effect::Stateless`,
/// no `<-` targets an own binding, every callee is stateless).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LambdaFacts {
    pub effect: EffectKind,
    pub stateless: bool,
}

impl LambdaFacts {
    pub const PURE: Self = Self { effect: EffectKind::Sync, stateless: true };
    pub const ASYNC: Self = Self { effect: EffectKind::Async, stateless: false };
    pub const STATEFUL: Self = Self { effect: EffectKind::Sync, stateless: false };

    pub fn join(self, other: Self) -> Self {
        Self {
            effect: self.effect.join(other.effect),
            stateless: self.stateless && other.stateless,
        }
    }

    /// Sync and stateless: what a tail loop's single activation and a
    /// sleep-free arm need.
    pub fn is_pure(self) -> bool {
        self.effect.is_sync() && self.stateless
    }
}

/// The sync/async lattice: `Sync ⊔ Sync = Sync`, everything else is
/// `Async`. `Async` is the conservative default.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, netidx_derive::Pack)]
#[pack(unwrapped)]
pub enum EffectKind {
    /// Same-cycle: input on cycle K → output (or nothing) on cycle K.
    Sync,
    /// Possibly-later-cycle: input on cycle K may produce output on a
    /// later cycle, autonomously, or never.
    #[default]
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

    /// True if every output lands on the cycle of its trigger.
    pub fn is_sync(self) -> bool {
        matches!(self, Self::Sync)
    }

    /// True if an output may land on a later cycle.
    pub fn is_async(self) -> bool {
        matches!(self, Self::Async)
    }
}

/// A builtin's classification (`BuiltIn::EFFECT`): does every output
/// land on the cycle of its trigger, does the result depend on anything
/// but the arguments, and can the JIT call it directly.
#[derive(Debug, Clone, Copy, Default)]
pub enum Effect {
    /// Input on cycle K may produce output on a later cycle,
    /// autonomously, or never. The conservative default, and the
    /// reading of an unregistered builtin name.
    #[default]
    Async,
    /// Same-cycle, but the instance holds cross-invocation state or its
    /// result depends on which arguments were delivered.
    Sync,
    /// Same-cycle and a pure function of its arguments (memos, scratch
    /// buffers and one-shot effects allowed). A wrong `Stateless` is a
    /// semantics bug: tail-loop iterations would share state. The
    /// payload is the JIT's direct-call entry; `None` for a builtin
    /// that must not be called from a kernel (an effect that may
    /// re-evaluate, or a raw `Apply` that answers before all its
    /// arguments arrive: a `CachedArgs` builtin never sees a missing one).
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

/// How a lambda recurses on its own `LambdaId`, the join over its
/// instances in declaration order (a later variant dominates). Read by
/// `#[tail_recursive]` and the arm-sleep rule; the operational gate is
/// `GXLambda::tail_loop`.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, netidx_derive::Pack,
)]
#[pack(unwrapped)]
pub enum RecursionKind {
    /// No call in the body reaches this lambda; also the default for a
    /// lambda the analysis never reached.
    #[default]
    NotRecursive,
    /// Every self-call is in tail position and the body holds no
    /// per-activation state: a fused body runs it as a constant-space
    /// loop (the node-walk still dispatches an activation per call).
    TailRecursive,
    /// Every self-call is in tail position but no loop is built: the
    /// body keeps per-activation state, or a formal is one the loop
    /// cannot rebind (`lowering::structural_tail_loop`).
    TailCalls,
    /// A self-call outside tail position, or mutual recursion.
    Recursive,
}
