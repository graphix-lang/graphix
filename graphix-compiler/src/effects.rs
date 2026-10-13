//! Sync/async effect classification. An operation is `Sync` when every
//! output lands on the cycle of its trigger and `Async` otherwise.
//! Builtin effects are declared via `BuiltIn::EFFECT`; user-function
//! effects are inferred. Fusion admits only `Effect::Stateless` with a
//! `FastCall` (`design/strict_fusion.md`): a `Sync` builtin keeps state
//! and node-walks like an `Async` one.

/// The facts the fixpoint infers per lambda from an optimistic start:
/// `effect` (`Sync` degrading to `Async`), `stateless` (no
/// per-activation state: every builtin reached is `Effect::Stateless`
/// or `Effect::Io`, no `<-` targets an own binding, every callee is
/// stateless) and `io` (it reaches an `Effect::Io` builtin).
// CR claude for claude: [doc-drift/low] The module doc above says fusion admits only
// Effect::Stateless with a FastCall. An Effect::Io builtin with an IoCall fuses too
// (Effect::fastcall). It should read 'Effect::Stateless with a FastCall and Effect::Io
// with an IoCall'. The LambdaDef::facts doc in node/lambda.rs has the same gap: its
// stateless definition should say 'every builtin it reaches is Effect::Stateless or
// Effect::Io', as LambdaFacts here and Effect::facts already do. DefGate::thrown's doc
// has two summary lines; drop the first. (c-lambda-05)
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
pub struct LambdaFacts {
    pub effect: EffectKind,
    pub stateless: bool,
    pub io: bool,
}

/// Pure: facts only degrade, and nothing has yet.
impl Default for LambdaFacts {
    fn default() -> Self {
        Self::PURE
    }
}

impl LambdaFacts {
    pub const PURE: Self = Self { effect: EffectKind::Sync, stateless: true, io: false };
    pub const ASYNC: Self =
        Self { effect: EffectKind::Async, stateless: false, io: false };
    pub const STATEFUL: Self =
        Self { effect: EffectKind::Sync, stateless: false, io: false };
    pub const IO: Self = Self { effect: EffectKind::Sync, stateless: true, io: true };

    pub fn join(self, other: Self) -> Self {
        Self {
            effect: self.effect.join(other.effect),
            stateless: self.stateless && other.stateless,
            io: self.io || other.io,
        }
    }

    /// Sync and stateless: what a tail loop's single activation needs.
    pub fn is_pure(self) -> bool {
        self.effect.is_sync() && self.stateless
    }

    /// Whether code with these facts must sleep with its arm: anything
    /// but pure computation.
    pub fn sleeps(self) -> bool {
        !self.is_pure() || self.io
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
    /// Same-cycle and a pure function of its arguments (memos and
    /// scratch buffers allowed, never an effect). A wrong `Stateless`
    /// is a semantics bug: tail-loop iterations would share state, and
    /// an arm holding it skips sleep. The payload is the JIT's
    /// direct-call entry; `None` for a raw `Apply` that answers before
    /// all its arguments arrive (a `CachedArgs` builtin never sees a
    /// missing one).
    Stateless(Option<crate::FastCall>),
    /// Same-cycle and stateless, but it acts on the world each time its
    /// trigger argument fires (a print, a log line): an arm holding one
    /// sleeps and a wake does not run it again. The payload is how a
    /// kernel calls it: [`IoCall`].
    Io(Option<IoCall>),
}

/// A fused [`Effect::Io`] call: `result` computes the call's value,
/// gated with its tag on argument `trigger`'s fire, and each time that
/// fires the kernel queues the call for [`crate::BuiltIn::perform`],
/// which its wrapper runs after the kernel returns.
#[derive(Debug, Clone, Copy)]
pub struct IoCall {
    pub result: crate::FastFn,
    pub trigger: usize,
}

impl Effect {
    /// The sync/async lattice fact effect inference reads.
    pub fn kind(self) -> EffectKind {
        match self {
            Effect::Async => EffectKind::Async,
            Effect::Sync | Effect::Stateless(_) | Effect::Io(_) => EffectKind::Sync,
        }
    }

    /// A call's facts, before its function arguments join them.
    pub fn facts(self) -> LambdaFacts {
        match self {
            Effect::Async => LambdaFacts::ASYNC,
            Effect::Sync => LambdaFacts::STATEFUL,
            Effect::Stateless(_) => LambdaFacts::PURE,
            Effect::Io(_) => LambdaFacts::IO,
        }
    }

    /// The direct-call entry a kernel calls: a `Stateless` builtin's,
    /// or an `Io` builtin's result.
    pub fn fastcall(self) -> Option<crate::FastCall> {
        match self {
            Effect::Stateless(f) => f,
            Effect::Io(c) => c.map(|c| crate::FastCall::Plain(c.result)),
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
