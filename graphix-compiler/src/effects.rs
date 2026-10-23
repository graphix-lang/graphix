//! Sync/async effect classification. An operation is `Sync` when every
//! output lands on the cycle of its trigger and `Async` otherwise.
//! Builtin effects are declared via `BuiltIn::EFFECT`; user-function
//! effects are inferred. Fusion admits only `Effect::Stateless` with a
//! `FastCall` and `Effect::Io` with an `IoCall` (`design/strict_fusion.md`):
//! a `Sync` builtin keeps state and node-walks like an `Async` one.

/// The facts the fixpoint infers per lambda from an optimistic start:
/// `effect` (`Sync` degrading to `Async`), `stateless` (no
/// per-activation state: every builtin reached is `Effect::Stateless`
/// or `Effect::Io`, no `<-` targets an own binding, every callee is
/// stateless) and `io` (it reaches an `Effect::Io` builtin).
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
pub struct LambdaFacts {
    pub effect: EffectKind,
    pub stateless: bool,
    pub io: bool,
    /// It reaches an `Effect::Io` builtin whose act is not diagnostic
    /// output ([`IoCall::diagnostic`]): `io` without `dbg`.
    pub world: bool,
    /// It may raise, or write a binding it does not own: an event an arm
    /// holding it delivers once, so the arm sleeps rather than re-run it
    /// fresh at each selection.
    pub event: bool,
}

/// Pure: facts only degrade, and nothing has yet.
impl Default for LambdaFacts {
    fn default() -> Self {
        Self::PURE
    }
}

impl LambdaFacts {
    pub const PURE: Self = Self {
        effect: EffectKind::Sync,
        stateless: true,
        io: false,
        world: false,
        event: false,
    };
    pub const ASYNC: Self =
        Self { effect: EffectKind::Async, stateless: false, ..Self::PURE };
    pub const STATEFUL: Self = Self { stateless: false, ..Self::PURE };
    pub const IO: Self = Self { io: true, world: true, ..Self::PURE };
    pub const DIAGNOSTIC: Self = Self { io: true, ..Self::PURE };
    pub const EVENT: Self = Self { event: true, ..Self::PURE };

    pub fn join(self, other: Self) -> Self {
        Self {
            effect: self.effect.join(other.effect),
            stateless: self.stateless && other.stateless,
            io: self.io || other.io,
            world: self.world || other.world,
            event: self.event || other.event,
        }
    }

    /// Sync and stateless: what a tail loop's single activation needs.
    pub fn is_pure(self) -> bool {
        self.effect.is_sync() && self.stateless
    }

    /// What `#[pure]` asserts: what [`Self::sleeps`] refuses, but
    /// diagnostic output.
    pub fn is_pure_function(self) -> bool {
        self.is_pure() && !self.world && !self.event
    }

    /// Whether code with these facts must sleep with its arm: anything
    /// but pure computation.
    pub fn sleeps(self) -> bool {
        !self.is_pure() || self.io || self.event
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
    /// Its act is diagnostic output (`dbg`), which a `#[pure]` body may
    /// hold, with a warning.
    pub diagnostic: bool,
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
            Effect::Io(Some(IoCall { diagnostic: true, .. })) => LambdaFacts::DIAGNOSTIC,
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
    /// cannot rebind (`lowering::tail_loop_blocker`, the reason in `NoLoop`).
    TailCalls,
    /// A self-call outside tail position, or mutual recursion.
    Recursive,
}

/// Why a pure body whose self-calls are all in tail position builds no
/// loop ([`RecursionKind::TailCalls`]), for `#[tail_recursive]` to say.
#[derive(Debug, Clone)]
pub enum NoLoop {
    /// A labeled, variadic or opaque formal, which a loop cannot rebind.
    Formals,
    /// Per-call memory: a collection operation, a non-tail self-call, a
    /// callee holding one.
    Memory,
    /// An Io call that does not fire with the formals.
    Io(crate::expr::Expr),
    /// A raise to a handler that does not fire with the formals.
    Raise(crate::expr::Expr),
}
