#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
// combine parser types nest deeply enough that monomorphizing the
// expression parser exceeds the default 128.
#![recursion_limit = "256"]
#[macro_use]
extern crate netidx_core;
#[macro_use]
extern crate combine;
#[macro_use]
extern crate serde_derive;

pub mod abstract_value;
pub mod analysis;
pub(crate) mod dbgenv;
pub mod effects;
pub mod env;
pub mod expr;
pub mod fusion;
pub mod ide;
pub mod node;
pub mod node_shape;
pub mod perfdbg;
pub(crate) mod stack;
pub mod tval;
pub mod typ;

use compact_str::CompactString;
// Re-exported so packages implementing `Apply::emit_clif` get the
// cranelift types through the compiler — no direct cranelift dep in a
// package, so version lockstep with the JIT is structural.
pub use cranelift_codegen;
pub use cranelift_frontend;
pub use fusion::FusionStats;
pub use tval::{Tag, TagValue, TagView};

use crate::{
    effects::EffectKind,
    env::Env,
    expr::{ExprId, ModPath},
    fusion::emit::{BodyCx, CompiledExpr},
    node::{
        callsite::CallSite,
        lambda::{GXLambda, LambdaDef},
    },
    typ::{FnType, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
pub use enumflags2::BitFlags;
use enumflags2::bitflags;
use expr::{Attr, Expr};
use futures::channel::mpsc;
use log::info;
use netidx_value::{Abstract, Value, abstract_type::AbstractWrapper};
use node::compiler;
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use std::{
    any::{Any, TypeId},
    cell::Cell,
    collections::hash_map::{self, Entry},
    fmt::Debug,
    mem,
    sync::{
        self, LazyLock,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Duration,
};
use tokio::{task, time::Instant};
use triomphe::Arc;
use uuid::Uuid;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u64)]
pub enum CFlag {
    WarnUnhandled,
    WarnUnused,
    WarningsAreErrors,
    /// Disable fusion entirely. `compile()` runs build + typecheck and
    /// returns the regular Node graph; no kernels are built or spliced.
    /// This is the test harness's
    /// `interp` mode: the program executes PURELY through the
    /// Update-trait node-walk, the canonical model. The single fusion
    /// control knob — fusion is JIT-only (no interpreter), so there is
    /// no meaningful "fuse but don't JIT" state to represent.
    FusionDisabled,
    /// Interactive (REPL) name-collision policy: a `use` that would
    /// collide with an existing import or declaration SHADOWS it
    /// instead of erroring, the way `let` re-binding already does.
    /// File modules keep the strict rules.
    ReplaceImports,
}

/// Runtime control signals shared between a runtime handle and the
/// running `ExecCtx`. `Interrupt` makes in-flight loops abort to bottom
/// (no value this cycle) while the runtime keeps going; `Abort`
/// additionally shuts the runtime down (its run loop returns before the
/// next cycle). Both are set from the handle side and polled lock-free
/// from the loop side — the tail loop and opt-in builtins via
/// [`ExecCtx::interrupted`], and the JIT kernels via `graphix_interrupted`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[bitflags]
#[repr(u32)]
pub enum CtlFlag {
    Interrupt = 1,
    Abort = 2,
}

/// The default [`Control::max_call_depth`]: the deepest chain of nested
/// (non-tail) graphix-lambda dispatches either evaluator will enter
/// before producing bottom instead. Sized from measurement (2026-07-03,
/// dev profile, 2MiB tokio worker stacks): the node-walk's Rust stack
/// dies at ≈1,600–1,800 nested dispatches for a small body and
/// ≈1,300–1,600 for a heavy one (frame cost is dominated by the
/// dispatch machinery, not body size), so 256 keeps ≥4x headroom below
/// the worst measured case. Embedders with bigger stacks can raise it
/// via [`Control::set_max_call_depth`]. Tail self-calls loop in place
/// in both evaluators and are exempt.
pub const DEFAULT_MAX_CALL_DEPTH: u32 = 256;

/// A runtime diagnostic: a failure whose value-level outcome is BOTTOM
/// by design (no value this cycle — nothing for `?`/`try` to catch),
/// surfaced through the runtime's event stream so embedders and the
/// shell can still tell the user WHICH expression produced nothing and
/// why. Produced into [`ExecCtx::diagnostics`] at the trip site,
/// drained by the runtime after each top-level node update.
#[derive(Debug, Clone)]
pub enum RtDiagnostic {
    /// A nested (non-tail) lambda dispatch hit
    /// [`Control::max_call_depth`] and produced bottom instead of
    /// recursing. `spec` is the lambda body that was about to dispatch
    /// (node-walk trip) or the fused region whose kernel aborted (JIT
    /// trip).
    CallDepthLimit { limit: u32, spec: Expr },
}

impl std::fmt::Display for RtDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RtDiagnostic::CallDepthLimit { limit, spec } => {
                write!(
                    f,
                    "call depth limit ({limit}) exceeded — deep non-tail \
                     recursion produced no value (raise via \
                     Control::set_max_call_depth), at line {} column {} {}",
                    spec.pos.line, spec.pos.column, spec.ori
                )
            }
        }
    }
}

/// Lock-free [`CtlFlag`] set over an `AtomicU32`, plus the shared
/// call-depth guard both evaluators count against. A loop polls
/// [`Control::interrupted`] (any flag ⇒ abort); the run loop polls
/// [`Control::aborted`] (Abort ⇒ shut down). See [`CtlFlag`].
#[derive(Debug)]
pub struct Control {
    flags: AtomicU32,
    /// Current nested non-tail lambda-dispatch depth. ONE counter for
    /// both evaluators — the node-walk pushes in `GXLambda::update`,
    /// JIT'd kernels push at lambda-call sites via the
    /// `graphix_depth_push`/`_pop` helpers — so an impure program that
    /// interleaves kernel frames and node-walk frames is bounded by
    /// the same limit as either alone.
    depth: AtomicU32,
    max_depth: AtomicU32,
    /// Set by the JIT's `graphix_depth_push` helper when a kernel's
    /// lambda dispatch hits the limit (native code can't push a
    /// diagnostic itself); `FusedKernel::update` takes it after each
    /// invocation and reports the kernel's spec. Node-walk trips
    /// report directly and never set this.
    depth_trip: AtomicBool,
    /// The WHOLE-DERIVATION trip poison (Eric's ruling 2026-08-14),
    /// shared by both evaluators like the depth counter itself: set at
    /// ANY trip (node-walk or kernel), cleared when the shared depth
    /// pops to zero — the tripped derivation's root. While set, no
    /// ride between the trip and the root may assemble a partial value
    /// out of history; beyond the root the delivered bottom is an
    /// ordinary bottom. One bit for both engines because a derivation
    /// interleaves them: a node-walk-residue trip inside a kernel
    /// invocation must poison the kernel's scrutinee ride (aug18a
    /// class 2, ride-too-narrow face), and a kernel trip that unwinds
    /// to depth 0 within its own region must NOT poison a top-level
    /// select in the same region (the ride-too-wide face).
    trip_poison: AtomicBool,
}

impl Default for Control {
    fn default() -> Self {
        Self::new()
    }
}

impl Control {
    pub fn new() -> Self {
        Control {
            flags: AtomicU32::new(0),
            depth: AtomicU32::new(0),
            max_depth: AtomicU32::new(DEFAULT_MAX_CALL_DEPTH),
            depth_trip: AtomicBool::new(false),
            trip_poison: AtomicBool::new(false),
        }
    }

    /// Record a depth trip on the whole-derivation poison — see the
    /// `trip_poison` field. Both evaluators' trip sites call this;
    /// `depth_pop` clears it when the derivation's root unwinds.
    pub fn set_trip_poison(&self) {
        self.trip_poison.store(true, Ordering::Relaxed);
    }

    /// Is a depth trip unwinding right now (trip seen, root not yet
    /// unwound)? Read by every ride site in both evaluators.
    pub fn trip_poisoned(&self) -> bool {
        self.trip_poison.load(Ordering::Relaxed)
    }

    /// Record that a JIT-side lambda dispatch hit the depth limit —
    /// see the `depth_trip` field.
    pub fn set_depth_trip(&self) {
        self.depth_trip.store(true, Ordering::Relaxed);
    }

    /// Take (and clear) the JIT depth-trip flag.
    pub fn take_depth_trip(&self) -> bool {
        self.depth_trip.swap(false, Ordering::Relaxed)
    }

    /// Read the JIT depth-trip flag WITHOUT clearing it.
    /// `Kernel::update` peeks to decide its production (a trip is a
    /// delivered FreshBottom, not a silent abort —
    /// missing_fire_epoch3_aug08e); the wrapping `FusedKernel` then
    /// takes it for the diagnostic.
    pub fn peek_depth_trip(&self) -> bool {
        self.depth_trip.load(Ordering::Relaxed)
    }

    /// Request that in-flight loops abort this cycle; the runtime keeps
    /// running and `do_cycle` clears this at the end of the cycle.
    pub fn interrupt(&self) {
        self.flags.fetch_or(CtlFlag::Interrupt as u32, Ordering::Release);
    }

    /// Request shutdown: in-flight loops abort AND the run loop returns
    /// before the next cycle. Sticky — never cleared.
    pub fn abort(&self) {
        self.flags.fetch_or(CtlFlag::Abort as u32, Ordering::Release);
    }

    /// True if any control flag is set — the signal a loop polls to
    /// decide whether to abort (both flags break loops).
    pub fn interrupted(&self) -> bool {
        self.flags.load(Ordering::Acquire) != 0
    }

    /// True if `Abort` is set — the signal the run loop polls before each
    /// cycle to decide whether to shut down.
    pub fn aborted(&self) -> bool {
        self.flags.load(Ordering::Acquire) & (CtlFlag::Abort as u32) != 0
    }

    /// Clear the `Interrupt` bit, leaving `Abort` sticky. Called by
    /// `do_cycle` at the end of each cycle so a one-shot `interrupt()`
    /// doesn't persist into the next cycle.
    pub fn clear_interrupt(&self) {
        self.flags.fetch_and(!(CtlFlag::Interrupt as u32), Ordering::Release);
    }

    /// Enter one nested (non-tail) lambda dispatch. `false` = the depth
    /// limit is reached — the caller must NOT dispatch and must produce
    /// bottom instead (the counter is not left incremented). Paired
    /// with [`depth_pop`](Self::depth_pop) after a `true` return. Fused
    /// collection scaffolds enter through this too (one unit per
    /// non-empty scaffold — every element dispatches at the same depth).
    pub fn depth_push(&self) -> bool {
        let d = self.depth.fetch_add(1, Ordering::Relaxed);
        if d >= self.max_depth.load(Ordering::Relaxed) {
            self.depth.fetch_sub(1, Ordering::Relaxed);
            false
        } else {
            true
        }
    }

    pub fn depth_pop(&self) {
        if self.depth.fetch_sub(1, Ordering::Relaxed) == 1 {
            // The derivation's root has unwound: the trip poison ends
            // here — beyond the root a delivered bottom is an ordinary
            // bottom (standing rules apply).
            self.trip_poison.store(false, Ordering::Relaxed);
        }
    }

    /// The current non-tail call depth (0 = no recursive dispatch in
    /// flight on this context).
    pub fn depth(&self) -> u32 {
        self.depth.load(Ordering::Relaxed)
    }

    /// Reset the depth counter to zero. Called by the runtime at the
    /// end of each cycle — pure insurance against a leaked push (a bug,
    /// but one that would otherwise ratchet every later cycle toward a
    /// spurious limit).
    pub fn depth_reset(&self) {
        self.depth.store(0, Ordering::Relaxed);
        self.trip_poison.store(false, Ordering::Relaxed);
    }

    /// The non-tail call-depth limit (see [`DEFAULT_MAX_CALL_DEPTH`]).
    pub fn max_call_depth(&self) -> u32 {
        self.max_depth.load(Ordering::Relaxed)
    }

    /// Set the non-tail call-depth limit. Raising it beyond the default
    /// requires correspondingly larger runtime thread stacks — the
    /// default keeps ≥4x measured headroom on 2MiB stacks.
    pub fn set_max_call_depth(&self, n: u32) {
        self.max_depth.store(n, Ordering::Relaxed);
    }
}

#[allow(dead_code)]
static TRACE: AtomicBool = AtomicBool::new(false);

#[allow(dead_code)]
pub fn set_trace(b: bool) {
    TRACE.store(b, Ordering::Relaxed)
}

#[allow(dead_code)]
pub fn with_trace<F: FnOnce() -> Result<R>, R>(
    enable: bool,
    spec: &Expr,
    f: F,
) -> Result<R> {
    let prev = trace();
    set_trace(enable);
    if !prev && enable {
        eprintln!("trace enabled at {}, spec: {}", spec.pos, spec);
    } else if prev && !enable {
        eprintln!("trace disabled at {}, spec: {}", spec.pos, spec);
    }
    let r = match f() {
        Err(e) => {
            eprintln!("traced at {} failed with {e:?}", spec.pos);
            Err(e)
        }
        r => r,
    };
    if prev && !enable {
        eprintln!("trace reenabled")
    }
    set_trace(prev);
    r
}

#[allow(dead_code)]
pub fn trace() -> bool {
    TRACE.load(Ordering::Relaxed)
}

// ─── Fusion / JIT mode ───────────────────────────────────────────
//
#[macro_export]
macro_rules! tdbg {
    ($e:expr) => {
        if $crate::trace() { dbg!($e) } else { $e }
    };
}

#[macro_export]
macro_rules! err {
    ($tag:expr, $err:literal) => {{
        let e: Value = ($tag.clone(), ::arcstr::literal!($err)).into();
        Value::Error(e.into())
    }};
}

#[macro_export]
macro_rules! errf {
    ($tag:expr, $fmt:expr, $($args:expr),*) => {{
        let msg: ArcStr = ::compact_str::format_compact!($fmt, $($args),*).as_str().into();
        let e: Value = ($tag.clone(), msg).into();
        Value::Error(e.into())
    }};
    ($tag:expr, $fmt:expr) => {{
        let msg: ArcStr = ::compact_str::format_compact!($fmt).as_str().into();
        let e: Value = ($tag.clone(), msg).into();
        Value::Error(e.into())
    }};
}

#[macro_export]
macro_rules! defetyp {
    ($name:ident, $tag_name:ident, $tag:literal, $typ:expr) => {
        static $tag_name: ArcStr = ::arcstr::literal!($tag);
        static $name: ::std::sync::LazyLock<$crate::typ::Type> =
            ::std::sync::LazyLock::new(|| {
                let scope = $crate::expr::ModPath::root();
                $crate::expr::parser::parse_type(&format!($typ, $tag))
                    .expect("failed to parse type")
                    .scope_refs(&scope)
            });
    };
}

defetyp!(CAST_ERR, CAST_ERR_TAG, "InvalidCast", "Error<`{}(string)>");

atomic_id!(LambdaId);

impl From<u64> for LambdaId {
    fn from(v: u64) -> Self {
        LambdaId(v)
    }
}

atomic_id!(LambdaInstanceId);

atomic_id!(BindId);

impl From<u64> for BindId {
    fn from(v: u64) -> Self {
        BindId(v)
    }
}

impl TryFrom<Value> for BindId {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self> {
        match value {
            Value::U64(id) => Ok(BindId(id)),
            v => bail!("invalid bind id {v}"),
        }
    }
}

pub trait UserEvent: Clone + Debug + Any {
    fn clear(&mut self);
}

pub trait CustomBuiltinType: Debug + Any + Send + Sync {}

impl CustomBuiltinType for Value {}
impl CustomBuiltinType for Option<Value> {}

#[derive(Debug, Clone)]
pub struct NoUserEvent;

impl UserEvent for NoUserEvent {
    fn clear(&mut self) {}
}

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u64)]
pub enum PrintFlag {
    /// Dereference type variables and print both the tvar name and the bound
    /// type or "unbound".
    DerefTVars,
    /// Replace common primitives with shorter type names as defined
    /// in core. e.g. Any, instead of the set of every primitive type.
    ReplacePrims,
    /// When formatting an Origin don't print the source, just the location
    NoSource,
    /// When formatting an Origin don't print the origin's parents
    NoParents,
}

thread_local! {
    static PRINT_FLAGS: Cell<BitFlags<PrintFlag>> = Cell::new(PrintFlag::ReplacePrims.into());
}

/// global pool of channel watch batches
pub static CBATCH_POOL: LazyLock<Pool<Vec<(BindId, Box<dyn CustomBuiltinType>)>>> =
    LazyLock::new(|| Pool::new(10000, 1000));

/// For the duration of the closure F change the way type variables
/// are formatted (on this thread only) according to the specified
/// flags.
pub fn format_with_flags<G: Into<BitFlags<PrintFlag>>, R, F: FnOnce() -> R>(
    flags: G,
    f: F,
) -> R {
    let prev = PRINT_FLAGS.replace(flags.into());
    let res = f();
    PRINT_FLAGS.set(prev);
    res
}

/// Event represents all the things that happened simultaneously in a
/// given execution cycle. Event may contain only one update for each
/// variable and netidx subscription in a given cycle, if more updates
/// happen simultaneously they must be queued and deferred to later
/// cycles.
#[derive(Debug)]
pub struct Event<E: UserEvent> {
    pub init: bool,
    /// Set alongside `init` when the init view being forced is a select
    /// arm's WAKE rather than a birth. The wake must look like an init
    /// to everything whose init handling is its own machinery (fused
    /// kernels force their input view, call sites prime their first
    /// dispatch, refs read standing entries as Fired), but a resumed arm
    /// is not a new one: a `<-` target that already holds a value keeps
    /// it instead of being reseeded by its own re-fired initializer,
    /// because sleep is PAUSE (Eric 2026-07-31). Without this a
    /// `{let s = i64:0; s <- …; s}` arm reset to its seed on every
    /// re-selection (fuzz/pending-triage/arm_fnvalue_connect_stale.gx).
    pub wake_init: bool,
    /// The INNERMOST overlay: same-cycle transient deliveries (select
    /// arm binds, QopDeliver, DynCall side channels, call-site formal
    /// publishes) at depth 0, or the current evaluation frame's
    /// private writes inside a framed pass. Under dense delivery
    /// (design/dense_delivery.md R3) this is NOT the value store —
    /// reads fall through the frame stack to [`Rt::store`] via
    /// `node::read_var`.
    pub variables: IntMap<BindId, TagValue>,
    /// The enclosing overlays of the current frame stack (innermost
    /// last). Empty at depth 0. A framed pass reads through to
    /// enclosing frames (an inner dispatch's captures live in its
    /// caller's frame) and then to the store.
    pub(crate) frames: Vec<IntMap<BindId, TagValue>>,
    pub custom: IntMap<BindId, Box<dyn CustomBuiltinType>>,
    pub user: E,
}

impl<E: UserEvent> Event<E> {
    pub fn new(user: E) -> Self {
        Event {
            init: false,
            wake_init: false,
            variables: IntMap::default(),
            frames: Vec::new(),
            custom: IntMap::default(),
            user,
        }
    }

    pub fn clear(&mut self) {
        let Self { init, wake_init, variables, frames, custom, user } = self;
        *init = false;
        *wake_init = false;
        variables.clear();
        debug_assert!(frames.is_empty(), "unbalanced enter_frame at cycle end");
        frames.clear();
        custom.clear();
        user.clear();
    }

    /// Enter an evaluation-frame OVERLAY: the current `variables` map
    /// is pushed onto the frame stack and `frame` (the pass's private
    /// writes — usually empty, or the tail loop's rebound formals)
    /// becomes the innermost overlay. Reads fall through the stack to
    /// the persistent store, so no seeding is required.
    pub fn enter_frame(&mut self, frame: IntMap<BindId, TagValue>) {
        self.frames.push(std::mem::replace(&mut self.variables, frame));
    }

    /// Leave the frame entered by [`Self::enter_frame`]: the enclosing
    /// overlay returns to `variables`; the frame's final map is
    /// handed back (the tail loop reads it as the previous pass's
    /// rebinds; everything else discards it — only
    /// `ExecCtx::frame_outbox` outlives a frame).
    pub fn exit_frame(&mut self) -> IntMap<BindId, TagValue> {
        let outer = self.frames.pop().expect("exit_frame without enter_frame");
        std::mem::replace(&mut self.variables, outer)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Refs {
    refed: LPooled<IntSet<BindId>>,
    bound: LPooled<IntSet<BindId>>,
}

pub use combine::stream::position::SourcePosition;

/// Metadata captured for every `let foo = |...| 'builtin_name`
/// binding. Stored on [`ExecCtx::builtin_bindings`] keyed by the
/// binding's [`BindId`] so the fusion pass can lower `Apply` sites
/// targeting this binding into a [`fusion::kernel_abi::FnSource::Builtin`]
/// slot without round-tripping through the runtime's `LambdaDef`
/// value.
///
/// `name` is the canonical builtin identifier (e.g. `core_once`) —
/// matches the key used by [`ExecCtx::register_builtin`] and
/// [`ExecCtx::builtins`].
///
/// `argspec` is the original source-level argument list (including
/// each labeled arg's default expression, if any), needed to
/// construct the per-formal-arg [`fusion::kernel_abi::BuiltinSlot`]
/// layout at fusion time.
///
/// `typ` is the binding's resolved function type at the binding
/// site. The fusion pass also reads the per-call-site resolved
/// `FnType` off the `Apply.function.typ` cell when generic; this
/// site-level `typ` is the declared binding signature, useful for
/// quick rejections.
#[derive(Debug, Clone)]
pub struct BuiltinBindInfo {
    pub name: ArcStr,
    pub argspec: triomphe::Arc<[expr::Arg]>,
    pub typ: triomphe::Arc<typ::FnType>,
    /// Lambda definition ID for this binding's value (if it was
    /// compiled as a lambda — every binding registered here was).
    /// Used by `Kernel::pre_bind_builtin` to look up the lambda's
    /// env+scope when compiling a `BuiltinSlot::LabeledDefault`
    /// expression — defaults may reference free variables visible
    /// only in the lambda's original definition scope.
    pub lambda_id: Option<LambdaId>,
}

impl Refs {
    pub fn clear(&mut self) {
        self.refed.clear();
        self.bound.clear();
    }

    pub fn with_external_refs(&self, mut f: impl FnMut(BindId)) {
        for id in &*self.refed {
            if !self.bound.contains(id) {
                f(*id);
            }
        }
    }

    /// Mark `id` as bound within the walked subtree so it is never
    /// surfaced as an external ref. A node implementing [`Update::refs`]
    /// uses this for synthetic internal bindings it owns — e.g. a HOF
    /// builtin's analysis-only per-element slot — which must not leak
    /// into fusion's region-input discovery (a leaked input becomes an
    /// orphaned kernel feeder that nothing ever feeds).
    pub fn mark_bound(&mut self, id: BindId) {
        self.bound.insert(id);
    }

    /// Visit every id BOUND within the walked subtree. Used by the
    /// direct emission's dead-statement elimination to learn which
    /// ids a `Bind` introduces.
    pub fn with_bound(&self, mut f: impl FnMut(BindId)) {
        for id in &*self.bound {
            f(*id);
        }
    }

    /// Visit every id READ in the walked subtree, whether it is bound
    /// inside it or outside. Externality answers "who owns this
    /// binding", which is the wrong question for "did anything this
    /// subtree reads carry new information this cycle" — a `<-` target
    /// declared INSIDE the subtree is written across cycles by the
    /// runtime just like a capture is.
    pub fn with_refs(&self, mut f: impl FnMut(BindId)) {
        for id in &*self.refed {
            f(*id);
        }
    }

    /// True if `id` is referenced anywhere in the walked subtree
    /// (whether or not it is also bound there).
    pub fn is_refed(&self, id: BindId) -> bool {
        self.refed.contains(&id)
    }
}

/// A compiled graph node.
///
/// A newtype rather than a bare `Box<dyn Update>` so that every pass
/// that descends the tree has ONE place to claim stack headroom: each
/// recursive `Update` method is shadowed here by an inherent method
/// that runs the vtable call under [`stack::ensure_sufficient`], and a
/// node's children are `Node`s, so a nesting depth an adversarial
/// program controls costs heap segments instead of overflowing
/// whatever thread the compile lands on. Non-recursive methods
/// (`typ`, `spec`, `view`) and anything not shadowed reach the trait
/// through `Deref`.
pub struct Node<R: Rt, E: UserEvent>(std::mem::ManuallyDrop<Box<dyn Update<R, E>>>);

/// Destroying a deep graph re-enters this through each node's
/// children, and drop glue is not a function `ensure_sufficient` can
/// wrap from outside. `ManuallyDrop` moves the teardown INSIDE the
/// guard: field glue would otherwise run after `drop` returns, back on
/// the original stack.
impl<R: Rt, E: UserEvent> Drop for Node<R, E> {
    fn drop(&mut self) {
        stack::ensure_sufficient(|| unsafe { std::mem::ManuallyDrop::drop(&mut self.0) })
    }
}

impl<R: Rt, E: UserEvent> Node<R, E> {
    pub fn new<T: Update<R, E> + 'static>(node: T) -> Self {
        Self(std::mem::ManuallyDrop::new(Box::new(node)))
    }

    pub fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        stack::ensure_sufficient(|| self.0.update(ctx, event))
    }

    pub fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        stack::ensure_sufficient(|| self.0.delete(ctx))
    }

    pub fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0(ctx))
    }

    pub fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck1(ctx))
    }

    pub fn refs(&self, refs: &mut Refs) {
        stack::ensure_sufficient(|| self.0.refs(refs))
    }

    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        stack::ensure_sufficient(|| self.0.sleep(ctx))
    }

    pub fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        stack::ensure_sufficient(|| self.0.reset_replay(ctx))
    }

    pub fn emit_clif(&self, cx: &mut BodyCx) -> Result<fusion::emit::CompiledExpr> {
        stack::ensure_sufficient(|| self.0.emit_clif(cx))
    }

    pub fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        stack::ensure_sufficient(|| self.0.fuse(ctx))
    }
}

impl<R: Rt, E: UserEvent> Debug for Node<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<R: Rt, E: UserEvent> std::ops::Deref for Node<R, E> {
    type Target = dyn Update<R, E>;

    fn deref(&self) -> &Self::Target {
        &**self.0
    }
}

impl<R: Rt, E: UserEvent> std::ops::DerefMut for Node<R, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut **self.0
    }
}

#[derive(Clone, Copy)]
pub enum BindMode<'a> {
    Definition,
    Dynamic(&'a FnType),
    Static { instance: &'a FnType, site: &'a FnType },
}

impl<'a> BindMode<'a> {
    pub fn resolved(self) -> Option<&'a FnType> {
        match self {
            Self::Definition => None,
            Self::Dynamic(ftype) | Self::Static { site: ftype, .. } => Some(ftype),
        }
    }
}

pub type InitFn<R, E> = sync::Arc<
    dyn for<'a, 'b, 'c, 'd> Fn(
            &'a Scope,
            &'b mut ExecCtx<R, E>,
            &'c mut [Node<R, E>],
            BindMode<'d>,
            ExprId,
        ) -> Result<Box<dyn Apply<R, E>>>
        + Send
        + Sync
        + 'static,
>;

/// Apply is a kind of node that represents a function application. It
/// does not hold ownership of it's arguments, instead those are held
/// by a CallSite node. This allows us to change the function called
/// at runtime without recompiling the arguments.
pub trait Apply<R: Rt, E: UserEvent>: Debug + Send + Sync + Any {
    /// Typed view for analysis-layer code (notably fusion). Default
    /// returns `BuiltIn` — opaque, fusion treats this call as a
    /// runtime `DynCall`. `GXLambda` overrides to `Lambda(self)`,
    /// `BuiltInLambda` delegates to `self.apply.view()`.
    ///
    /// The `BuiltIn` default works on `&dyn Apply` (no `Self: Sized`
    /// bound needed) because the variant carries no reference. This
    /// means every existing Apply impl inherits sensible "opaque
    /// builtin" semantics without per-impl edits.
    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::BuiltIn
    }

    /// Mutable counterpart to [`Self::view`]. Same dispatch story;
    /// used by fusion when it needs to splice a sub-kernel into the
    /// graph reachable through this Apply (e.g. into a `GXLambda`
    /// body Node).
    fn view_mut(&mut self) -> ApplyViewMut<'_, R, E> {
        ApplyViewMut::BuiltIn
    }

    /// Same borrowed-production contract as [`Update::update`]: the
    /// returned `&TagValue` is the builtin's RESIDENT (its result slot
    /// — an ordinary builtin returns `self.out.set(TagValue::fired(v))`
    /// and rides the resident on a quiet cycle). The production's
    /// tag rides in the value — there is no side channel: `GXLambda`
    /// returns its body's tag, `CachedArgs` re-surfaces its result
    /// slot retagged STALE on a quiet arg refresh, the fused `Kernel`
    /// returns the JIT out slot's disc tags.
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue;

    /// delete any internally generated nodes, only needed for
    /// builtins that dynamically generate code at runtime
    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {
        ()
    }

    /// First typecheck pass ("Lambda phase"): typecheck the call's
    /// argument nodes against the lambda's own FnType while it is built.
    fn typecheck0(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    /// Second typecheck pass ("CallSite phase").
    fn typecheck1(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        Ok(())
    }

    /// return the lambdas type, builtins do not need to implement
    /// this, it is implemented by the BuiltIn wrapper
    fn typ(&self) -> Arc<FnType> {
        static EMPTY: LazyLock<Arc<FnType>> = LazyLock::new(|| {
            Arc::new(FnType {
                args: Arc::from_iter([]),
                rtype: Type::Bottom,
                throws: Type::Bottom,
                vargs: None,
                explicit_throws: false,
                ..Default::default()
            })
        });
        Arc::clone(&*EMPTY)
    }

    /// Populate the Refs structure with all the ids bound and refed by this
    /// node. It is only necessary for builtins to implement this if they create
    /// nodes, such as call sites.
    fn refs<'a>(&self, _refs: &mut Refs) {}

    /// Put the builtin to sleep — used by constructs like select for
    /// unselected branches. Sleep is PAUSE, not reset (Eric's ruling
    /// 2026-07-31): value-channel state (arg slots, the result
    /// resident) SURVIVES so a re-woken arm rides its history. Only
    /// the documented arm-rewake RESTART builtins clear their
    /// semantic latches here (once/take/skip/hold/uniq/count — such a
    /// builtin must declare `SLEEP_RESTARTS = true` so the fusion
    /// interior-sleep gate keeps kernels honest), and async builtins
    /// may tear down watches/subscriptions to re-arm on wake.
    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>);

    /// Clear REPLAY caches, preserve SEMANTIC state — the `Apply`-side
    /// twin of [`Update::reset_replay`] (see its doc for the
    /// classification rule and the caller contract). For a builtin:
    /// cached ARG values (`CachedArgs`/`EvalCached` memory) are replay
    /// state and clear; accumulators (`count`'s tally, a queue, `once`'s
    /// fired flag) and pure memos (a compiled `Regex`, a typecheck-derived
    /// cast type) are semantic/derived and survive. **Required, no
    /// default impl** — every builtin must classify its own state.
    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>);

    /// Emit this call site into the open JIT kernel as CLIF (the
    /// builtin-owned half of distributed emission — see
    /// [`Update::emit_clif`]). The contract:
    ///
    /// - `Ok(Some(cv))` — emitted; `cv` is the call's result.
    /// - `Ok(None)` — shape not handled; the call site falls back to
    ///   its next strategy (a runtime `DynCall`, or no fusion). The
    ///   impl MUST NOT have emitted any instructions before returning
    ///   `Ok(None)`.
    /// - `Err` — abort the whole kernel build; the subtree node-walks.
    ///   Partial emission before `Err` is fine — the half-built
    ///   function is discarded.
    fn emit_clif(
        &self,
        _callsite: &CallSite<R, E>,
        _cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        Ok(None)
    }

    /// Participate in the compile-time fusion phase. Called by
    /// [`CallSite::fuse`] — i.e. exactly when `try_fuse` on the call
    /// site FAILED (the call did NOT inline into an enclosing kernel).
    /// [`GXLambda`] overrides this to fuse the maximal sync sub-regions
    /// of its per-callsite instance BODY in place (async residue
    /// node-walks), so a lambda whose call site didn't fuse still runs
    /// its body natively. The impl MUST swallow build errors (de-fuse,
    /// never fail the compile — node-walk is canonical). Default: no-op.
    fn fuse(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }
}

/// Typed view of an [`Apply`] for fusion / analysis layer code,
/// symmetric to [`NodeView`].
///
/// Variants:
/// - [`Lambda`](ApplyView::Lambda) — a graphix-language lambda with
///   a walkable [`Node`] body. Fusion descends into the body via
///   [`GXLambda::body`].
/// - [`BuiltIn`](ApplyView::BuiltIn) — opaque builtin. Fusion emits
///   a runtime `DynCall` (or the builtin participates directly via
///   [`Apply::emit_clif`]); no introspection beyond the trait
///   methods. The default `view()` returns this — every Apply impl
///   inherits opaque-builtin semantics unless it overrides.
pub enum ApplyView<'a, R: Rt, E: UserEvent> {
    Lambda(&'a GXLambda<R, E>),
    BuiltIn,
}

/// Mutable counterpart to [`ApplyView`]. Used by fusion for splicing
/// sub-kernels into Nodes reachable through an Apply (the body Node
/// of a [`Lambda`](ApplyViewMut::Lambda)).
pub enum ApplyViewMut<'a, R: Rt, E: UserEvent> {
    Lambda(&'a mut GXLambda<R, E>),
    BuiltIn,
}

/// Exhaustive typed view of the compiled node graph.
///
/// `Node` (`Box<dyn Update>`) is optimized for execution: vtable
/// dispatch is what LLVM does best for the hot update loop. But for
/// compile-time analysis — fusion, linting, dead-code detection,
/// escape analysis, refactoring — a closed enum is what you want.
///
/// `NodeView` is that enum: one variant per concrete `Update` impl
/// in the compiler. The two forms coexist by specializing: `Node`
/// keeps the vtable for runtime, `NodeView<'a>` provides a borrowed,
/// pattern-matchable view for analysis.
///
/// **No `Other` catch-all.** Adding a new node type requires picking
/// a variant; adding a new variant requires reviewing every
/// exhaustive match (across all analysis tools) that consumes
/// `NodeView`. This is the project's "compiler tracks what humans
/// would otherwise have to remember" pattern, applied to the node
/// graph itself.
#[allow(missing_docs)]
pub enum NodeView<'a, R: Rt, E: UserEvent> {
    // Fusion-relevant containers
    Bind(&'a node::bind::Bind<R, E>),
    Lambda(&'a node::lambda::Lambda),
    Block(&'a node::Block<R, E>),
    Module(&'a node::module::Module<R, E>),
    // Child-bearing non-container
    CallSite(&'a CallSite<R, E>),
    MapQ(&'a node::collection::MapQBase<R, E>),
    FoldQ(&'a node::collection::FoldQBase<R, E>),
    Select(&'a node::select::Select<R, E>),
    Catch(&'a node::error::Catch<R, E>),
    Qop(&'a node::error::Qop<R, E>),
    OrNever(&'a node::error::OrNever<R, E>),
    ExplicitParens(&'a node::ExplicitParens<R, E>),
    TypeCast(&'a node::TypeCast<R, E>),
    Connect(&'a node::Connect<R, E>),
    ConnectDeref(&'a node::ConnectDeref<R, E>),
    StringInterpolate(&'a node::StringInterpolate<R, E>),
    Any(&'a node::Any<R, E>),
    Sample(&'a node::Sample<R, E>),
    // Producers
    Struct(&'a node::data::Struct<R, E>),
    StructWith(&'a node::data::StructWith<R, E>),
    Tuple(&'a node::data::Tuple<R, E>),
    Variant(&'a node::data::Variant<R, E>),
    Construct(&'a node::data::Construct<R, E>),
    Array(&'a node::array::Array<R, E>),
    Map(&'a node::map::Map<R, E>),
    // Accessors
    StructRef(&'a node::data::StructRef<R, E>),
    TupleRef(&'a node::data::TupleRef<R, E>),
    ArrayRef(&'a node::array::ArrayRef<R, E>),
    ArraySlice(&'a node::array::ArraySlice<R, E>),
    MapRef(&'a node::map::MapRef<R, E>),
    // Binding access
    Ref(&'a node::bind::Ref),
    ByRef(&'a node::bind::ByRef<R, E>),
    Deref(&'a node::bind::Deref<R, E>),
    // Arithmetic — one variant per macro-generated struct in node/op.rs
    Add(&'a node::op::Add<R, E>),
    Sub(&'a node::op::Sub<R, E>),
    Mul(&'a node::op::Mul<R, E>),
    Div(&'a node::op::Div<R, E>),
    Mod(&'a node::op::Mod<R, E>),
    CheckedAdd(&'a node::op::CheckedAdd<R, E>),
    CheckedSub(&'a node::op::CheckedSub<R, E>),
    CheckedMul(&'a node::op::CheckedMul<R, E>),
    CheckedDiv(&'a node::op::CheckedDiv<R, E>),
    CheckedMod(&'a node::op::CheckedMod<R, E>),
    // Comparison + logical
    Eq(&'a node::op::Eq<R, E>),
    Ne(&'a node::op::Ne<R, E>),
    Lt(&'a node::op::Lt<R, E>),
    Gt(&'a node::op::Gt<R, E>),
    Lte(&'a node::op::Lte<R, E>),
    Gte(&'a node::op::Gte<R, E>),
    And(&'a node::op::And<R, E>),
    Or(&'a node::op::Or<R, E>),
    Not(&'a node::op::Not<R, E>),
    Neg(&'a node::op::Neg<R, E>),
    // Leaves and declarations
    Constant(&'a node::Constant),
    TypeDef(&'a node::TypeDef),
    Impl(&'a node::traits::Impl<R, E>),
    Nop(&'a node::Nop),
    // Synthetic — produced by fusion itself.
    FusedKernel(&'a fusion::FusedKernel<R, E>),
}

/// Update represents a regular graph node, as opposed to a function
/// application represented by Apply. Regular graph nodes are used for
/// every built in node except for builtin functions.
pub trait Update<R: Rt, E: UserEvent>: Debug + Send + Sync + Any + 'static {
    /// Update the node with the specified event and return its
    /// production, borrowed from the node's own production slot (the
    /// RESIDENT — a computing node recomputes into it, a delegating
    /// node forwards its child's borrow). DENSE: every awake node
    /// delivers every cycle — Fired(v) / Stale(v) / FreshBottom /
    /// StaleBottom; a quiet cycle rides the resident
    /// (`self.resident.ride()`). See `tval::TagValue`, [`TagView`],
    /// and `design/dense_delivery.md`.
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue;

    /// delete the node and it's children from the specified context
    fn delete(&mut self, ctx: &mut ExecCtx<R, E>);

    /// First typecheck pass: structural type checking. Each node checks
    /// itself and recurses into its children (`wrap!(child,
    /// child.typecheck0(ctx))?`), running over the whole tree before
    /// `typecheck1`.
    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()>;

    /// Second typecheck pass, run over the whole tree AFTER `typecheck0`
    /// completes. By now every `FnType::lambda_ids` closure is final, so
    /// a `CallSite` can read it to decide static dispatch and drive the
    /// resolved-type-dependent finalization that used to be the deferred
    /// check. NO default impl: every `Update` node must recurse into its
    /// children's `typecheck1` (mirroring `typecheck0`'s child walk), so
    /// a new node type is a compile error until it participates.
    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()>;

    /// return the node type
    fn typ(&self) -> &Type;

    /// Populate the Refs structure with all the bind ids either refed or bound
    /// by the node and it's children
    fn refs(&self, refs: &mut Refs);

    /// return the original expression used to compile this node
    fn spec(&self) -> &Expr;

    /// put the node to sleep, called on unselected branches
    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>);

    /// Clear REPLAY caches — the "last value I saw" memory that
    /// combineLatest evaluation keeps so one fresh input can combine
    /// with a quiet other (operand `Cached` wrappers, select's cached
    /// scrutinee, a call site's published arg values) — while
    /// preserving SEMANTIC state (`count`'s tally, `once`'s fired
    /// flag, an accumulated queue). Sequential evaluation calls this
    /// between frames (a `For` body between iterations, a tail loop
    /// between jumps) so iteration i−1's sub-results cannot leak into
    /// iteration i when i's producer bottoms; the caller re-primes
    /// quiet inputs from the runtime cache (captures are load-bearing
    /// across frames — see the arm-wake cached-pull delivery).
    /// **Required, no default impl**: the replay-vs-semantic
    /// classification is a per-node decision the compiler must force —
    /// a defaulted no-op on a node with an operand cache is a silent
    /// wrong-answer bug. Impls recurse into children like `sleep`.
    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>);

    /// Return a typed view of this node for compile-time analysis.
    /// **Required, no default impl** — every `Update` impl picks a
    /// `NodeView` variant. Adding a new node type can't compile
    /// without choosing a variant; adding a new variant forces
    /// every exhaustive match consuming `NodeView` to be reviewed.
    fn view(&self) -> NodeView<'_, R, E>;

    /// Emit this node's computation into the open JIT kernel as CLIF
    /// and return its SSA result. Emission recursion is distributed —
    /// an impl compiles its children via `child.emit_clif(cx)`; the
    /// raw cranelift builder is `cx.b` and the graphix-specific
    /// surface (env binds, helper FuncRefs, taint/pending) lives on
    /// [`BodyCx`].
    ///
    /// The default is `Err`: this node doesn't emit, so any kernel
    /// attempt whose subtree contains it fails to compile and the
    /// subtree node-walks — the universal fallback. Correctness is
    /// structural: a missing (or not-yet-written) impl can lose
    /// fusion, never produce a wrong answer.
    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<fusion::emit::CompiledExpr> {
        anyhow::bail!(
            "node does not emit CLIF (spec id {:?}, `{}`) — subtree \
             node-walks",
            self.spec().id,
            self.spec()
        )
    }

    /// Fuse this subtree. The contract:
    ///
    /// - `Ok(Some(replacement))` — "I fused myself: delete me and swap
    ///   this in." The caller (the parent node, or the compile-time
    ///   driver for roots) calls `old.delete(ctx)` after the swap.
    /// - `Ok(None)` — no replacement at this level. The impl already
    ///   recursed `fuse` into its own children via `&mut self` (using
    ///   [`fusion::fuse`], which also attempts
    ///   [`fusion::try_fuse`] on each child) and swapped any
    ///   that returned a replacement.
    ///
    /// Policy is per-node: containers choose where to recurse, and the
    /// maximal-region property falls out of top-down order (the highest
    /// subtree whose `try_fuse` succeeds is spliced and nothing below it
    /// is attempted). The default is
    /// `Ok(None)` with no recursion. Only Module, Block, Bind,
    /// CallSite, and TryCatch (plus `Apply` on Lambda) override it to
    /// descend; the other containers (Sample, Connect, Select, Any,
    /// the operators, producers) deliberately keep the no-recursion
    /// default — a sync subtree under them fuses only as part of an
    /// enclosing block/bind region that fuses as a whole.
    fn fuse(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        Ok(None)
    }
}

pub type BuiltInInitFn<R, E> = for<'a, 'b, 'c, 'd> fn(
    &'a mut ExecCtx<R, E>,
    &'a FnType,
    Option<&'d FnType>,
    &'b Scope,
    &'c [Node<R, E>],
    ExprId,
) -> Result<Box<dyn Apply<R, E>>>;

/// Trait implemented by graphix built-in functions implemented in rust
pub trait BuiltIn<R: Rt, E: UserEvent> {
    /// The name of the builtin, this must be package::unique_name for
    /// builtins in a package
    const NAME: &str;
    /// Sync/async classification for fusion. Conservative default is
    /// `Async`; override to `Sync` when the builtin produces all of its
    /// output on the same cycle as the input that triggered it (it may
    /// produce no output at all, but never on a future cycle relative
    /// to a current trigger). See `effects::EffectKind` and
    /// `design/whole_graph_fusion.md` for the rules and examples.
    const EFFECT: EffectKind = EffectKind::Async;
    /// Whether deleting this builtin's `Apply` and re-initializing it
    /// fresh is unobservable: the instance holds no cross-invocation
    /// state (`count`/`sum`/`min` accumulate — NOT stateless) and each
    /// invocation performs no external effect (`print`/`log` emit — NOT
    /// stateless; an internal same-input memo cache is fine). Only
    /// meaningful for `EFFECT = Sync` builtins (async builtins are
    /// excluded upstream). Consulted by the transient-recursion gate
    /// (`node::callsite::transient_body_ok`,
    /// `design/transient_recursion.md`): a recursive callee instance
    /// may be deleted when its call returns only if every builtin its
    /// body calls is stateless. Conservative default: `false`.
    const STATELESS: bool = false;
    /// Whether this builtin's `sleep` CLEARS semantic state — the
    /// documented arm-rewake RESTART builtins
    /// (`once`/`take`/`skip`/`hold`/`uniq`/`count`): an arm
    /// deselection sleeps the instance and re-arms it for the next
    /// selection. Only meaningful for `EFFECT = Sync` builtins.
    /// Consulted by the fusion interior-sleep gate: a kernel has no
    /// per-arm sleep initiator, so a sleep-restarting builtin's
    /// DynCall refuses to emit inside a fused select arm and the
    /// region de-fuses (the node-walk's arm sleep then applies). A
    /// wrong `false` is a semantics bug (the kernel would keep burned
    /// state the interp re-arms); a wrong `true` only costs fusion
    /// coverage. Default: `false` (sleep-inert).
    const SLEEP_RESTARTS: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved_type: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>>;
}

/// A compile-time check for a `#[..]` attribute. Dispatched by the FUSION
/// walk (`fusion::fuse`) at each visited decorated node, after that node's
/// fusion attempt resolved — so `node` is the final (possibly fused) node of
/// the expression the attribute sits above, and the check sees exactly the
/// fusion outcome. There is no separate attribute walk; under `--no-fusion`
/// the walk never runs and registry attributes are vacuously satisfied (the
/// documented `#[native]` contract). A node absorbed into a strictly-larger
/// ancestor kernel is never visited — also correct (it IS native).
/// Returning `Err` turns the attribute into a compile error. Registered on the
/// `ExecCtx` via [`ExecCtx::register_attribute`], mirroring builtin
/// registration, so packages can add their own attributes. (The
/// definition-asserting attributes — `#[tail_recursive]`, `#[sync]`,
/// `#[async]` — are NOT registry attributes: they are compiler-reserved
/// names stamped into `ExecCtx::def_assertions` at compile and verified at
/// the tail of `analysis::analyze`, mode-independently.)
pub type AttributeCheckFn<R, E> = fn(&ExecCtx<R, E>, &Attr, &Node<R, E>) -> Result<()>;

/// A definition assertion stamped by a compiler-reserved attribute
/// (`#[tail_recursive]` / `#[sync]` / `#[async]`) on a function definition.
/// Stamped by `node::compiler::compile` when the decorated statement is
/// built; verified (and removed) at the tail of `analysis::analyze` once the
/// definition has been reached by the analysis — an assertion on a
/// not-yet-called definition stays pending until a later compile uses it
/// (REPL: define now, call later, verified then; dead code is never
/// verified, like dead code anywhere).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefAssertionKind {
    Sync,
    Async,
    TailRecursive,
}

impl DefAssertionKind {
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        match name {
            "sync" => Some(Self::Sync),
            "async" => Some(Self::Async),
            "tail_recursive" => Some(Self::TailRecursive),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DefAssertion {
    pub(crate) id: LambdaId,
    pub(crate) kind: DefAssertionKind,
    /// The decorated statement, for error positions.
    pub(crate) spec: Expr,
}

/// Trait implemented by graphix attributes (`#[name]` / `#[name(args)]`).
pub trait Attribute<R: Rt, E: UserEvent> {
    /// The bare attribute name as written in source (e.g. `native` for
    /// `#[native]`). Attribute names are a flat global namespace — they are
    /// NOT package-prefixed the way builtin names are.
    const NAME: &str;
    fn check(ctx: &ExecCtx<R, E>, attr: &Attr, node: &Node<R, E>) -> Result<()>;
}

/// The `#[native]` attribute: the decorated expression must compile to native
/// code with zero node-walk residue, else it is a compile error. It may only
/// decorate a value-producing computation or a call — never a function
/// definition. A `native` requirement on a function value would be infectious
/// and brittle (such a function could never be stored, dynamically dispatched,
/// or passed to a non-fusing HOF); a performance requirement belongs at the
/// use site, so a function-typed target is rejected outright.
pub struct Native;

impl<R: Rt, E: UserEvent> Attribute<R, E> for Native {
    const NAME: &str = "native";

    fn check(ctx: &ExecCtx<R, E>, _attr: &Attr, node: &Node<R, E>) -> Result<()> {
        if let Type::Fn(_) = node.typ() {
            crate::bailat!(
                node.spec(),
                "#[native] annotates a computation or a call, not a function — \
                 put it on the call site, not the definition"
            );
        }
        // Fully fused: the decorated node is itself a native kernel. (If it was
        // absorbed into a strictly-larger ancestor kernel, `for_each_node`
        // never reaches it and this check doesn't run — also native, correctly.)
        if let NodeView::FusedKernel(_) = node.view() {
            return Ok(());
        }
        // (Under `--no-fusion` this check never dispatches at all — the
        // fusion walk doesn't run — the same vacuous pass the old in-check
        // `fusion.enabled` branch produced: `#[native]` rides in `run!`
        // interp fixtures and `--no-fusion` bench runs.)
        // The node survived fusion as node-walk residue. Surface the REAL
        // blockers, filtering out attempt-then-recurse protocol noise:
        // `try_fuse` records a `failed` entry for EVERY region root it tries,
        // including a `let`/block whose own region attempt fails ("node does
        // not emit CLIF") but whose VALUE fused in a child sub-region. Report
        // only the LEAF-most failures whose subtree contains NO fused region —
        // the genuine residue (the call/op that node-walks), not the
        // structural containers above it. Source identity, rather than the
        // instance-local ExprId, correlates callback instances with the
        // compile attempt that produced the failure.
        let mut report: Vec<&fusion::FusionFailure> = Vec::new();
        fusion::for_each_node(node, &mut |n| {
            let Some(failure) = ctx.fusion.stats.failure_for_source(n.spec()) else {
                return;
            };
            let mut subtree_fused = false;
            let mut has_failed_desc = false;
            let mut root = true;
            fusion::for_each_node(n, &mut |desc| {
                if root {
                    root = false;
                    return;
                }
                subtree_fused |= ctx.fusion.stats.source_fused(desc.spec());
                has_failed_desc |=
                    ctx.fusion.stats.failure_for_source(desc.spec()).is_some();
            });
            if !subtree_fused
                && !has_failed_desc
                && !report.iter().any(|prior| prior.id == failure.id)
            {
                report.push(failure);
            }
        });
        if crate::dbgenv::gxdbg_native_all() {
            for failure in ctx.fusion.stats.failed.iter() {
                eprintln!("NATIVE-ALL {:?}: {}", failure.id, failure.reason);
            }
        }
        let mut reasons = CompactString::new("");
        for failure in &report {
            use std::fmt::Write;
            let _ = write!(reasons, "\n  - {}", failure.reason);
        }
        if reasons.is_empty() {
            crate::bailat!(
                node.spec(),
                "#[native] expression did not fully fuse to native code"
            );
        }
        crate::bailat!(
            node.spec(),
            "#[native] expression did not fully fuse to native code:{reasons}"
        );
    }
}

pub trait Abortable {
    fn abort(&self);
}

impl Abortable for task::AbortHandle {
    fn abort(&self) {
        task::AbortHandle::abort(self)
    }
}

pub trait Rt: Debug + Any {
    type AbortHandle: Abortable;

    fn clear(&mut self);

    /// This will be called by the compiler whenever a bound variable
    /// is referenced. The ref_by is the toplevel expression that
    /// contains the variable reference. When a variable event
    /// happens, you should update all the toplevel expressions that
    /// ref that variable.
    ///
    /// ref_var will also be called when a bound lambda expression is
    /// referenced, in that case the ref_by id will be the toplevel
    /// expression containing the call site.
    fn ref_var(&mut self, id: BindId, ref_by: ExprId);
    fn unref_var(&mut self, id: BindId, ref_by: ExprId);

    /// Called by the ExecCtx when set_var is called on it.
    ///
    /// All expressions that ref the id should be updated when this happens. The
    /// runtime must deliver all set_vars in a single event except that set_vars
    /// for the same variable in the same cycle must be queued and deferred to
    /// the next cycle.
    ///
    /// The runtime MUST NOT change event while a cycle is in
    /// progress. set_var must be queued until the cycle ends and then
    /// presented as a new batch.
    fn set_var(&mut self, id: BindId, value: Value);

    /// The persistent tagged store (design/dense_delivery.md R3): the
    /// (production, cycle-stamp) of every bound variable's last
    /// delivery. THE cross-cycle read under dense delivery: a reader
    /// interprets `stamp == cycle()` as delivered-this-cycle (the
    /// entry's own tag), an older stamp as the standing value channel
    /// (Stale — Fired under an init view, R2), and absence as the
    /// phantom. Maintained AT DELIVERY — when a queued `set_var`
    /// actually lands in `event.variables` — so a primed read can
    /// never observe a value AHEAD of the delivery stream (the old
    /// `ExecCtx`-owned map got this wrong twice — soak jul08l/jul08n).
    fn store(&self) -> &IntMap<BindId, (TagValue, u64)>;

    /// The last delivered VALUE of a bind — the store entry's value
    /// half. A bind whose last delivery was a bottom yields `None`:
    /// per ruled delta 7 there is no pre-bottom value to resurrect
    /// (the sparse `cached` map retained one; that behavior died with
    /// it at P5b′).
    fn store_value(&self, id: &BindId) -> Option<Value> {
        self.store().get(id).and_then(|(tv, _)| {
            if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) }
        })
    }

    /// Insert a full tagged production into the store, stamped with
    /// the current cycle — the same-cycle publishers' seam (`Bind`'s
    /// publish, the runtime's delivery loop, pattern binds).
    fn store_insert(&mut self, id: BindId, tv: TagValue);

    /// Remove a bind from the store — delete/unbind/replay cleanup.
    fn store_remove(&mut self, id: &BindId);

    /// Insert a STANDING entry — value-channel maintenance that is
    /// deliberately NOT a delivery (`ByRef`'s init seed): stamped as
    /// an earlier cycle, so a same-cycle reader sees it Standing
    /// (stale / init-fired), never Delivered.
    fn store_insert_standing(&mut self, id: BindId, tv: TagValue);

    /// The current cycle number — the store's stamp clock.
    fn cycle(&self) -> u64;

    /// Notify the RT that a top level variable has been set internally
    ///
    /// This is called when the compiler has determined that it's safe to set a
    /// variable without waiting a cycle. When the updated variable is a
    /// toplevel node this method is called to notify the runtime that needs to
    /// update any dependent toplevel nodes.
    fn notify_set(&mut self, id: BindId);

    /// arrange to have a Timer event delivered after timeout. When
    /// the timer expires you are expected to deliver a Variable event
    /// for the id, containing the current time.
    fn set_timer(&mut self, id: BindId, timeout: Duration);

    /// Spawn a task
    ///
    /// When the task completes it's output must be delivered as a
    /// custom event using the returned `BindId`
    ///
    /// Calling `abort` must guarantee that if it is called before the
    /// task completes then no update will be delivered.
    fn spawn<F: Future<Output = (BindId, Box<dyn CustomBuiltinType>)> + Send + 'static>(
        &mut self,
        f: F,
    ) -> Self::AbortHandle;

    /// Spawn a task
    ///
    /// When the task completes it's output must be delivered as a
    /// variable event using the returned `BindId`
    ///
    /// Calling `abort` must guarantee that if it is called before the
    /// task completes then no update will be delivered.
    fn spawn_var<F: Future<Output = (BindId, Value)> + Send + 'static>(
        &mut self,
        f: F,
    ) -> Self::AbortHandle;

    /// Ask the runtime to watch a channel
    ///
    /// When event batches arrive via the channel the runtime must
    /// deliver the events as custom updates.
    fn watch(
        &mut self,
        s: mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    );

    /// Ask the runtime to watch a channel
    ///
    /// When event batches arrive via the channel the runtime must
    /// deliver the events variable updates.
    fn watch_var(&mut self, s: mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>);
}

#[derive(Default)]
pub struct LibState(AHashMap<TypeId, Box<dyn Any + Send + Sync>>);

impl LibState {
    /// Look up and return the context global library state of type
    /// `T`.
    ///
    /// If none is registered in this context for `T` then create one
    /// using `T::default`
    pub fn get_or_default<T>(&mut self) -> &mut T
    where
        T: Default + Any + Send + Sync,
    {
        self.0
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(T::default()) as Box<dyn Any + Send + Sync>)
            .downcast_mut::<T>()
            .unwrap()
    }

    /// Look up and return the context global library state of type
    /// `T`.
    ///
    /// If none is registered in this context for `T` then create one
    /// using the provided function.
    pub fn get_or_else<T, F>(&mut self, f: F) -> &mut T
    where
        T: Any + Send + Sync,
        F: FnOnce() -> T,
    {
        self.0
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(f()) as Box<dyn Any + Send + Sync>)
            .downcast_mut::<T>()
            .unwrap()
    }

    pub fn entry<'a, T>(
        &'a mut self,
    ) -> hash_map::Entry<'a, TypeId, Box<dyn Any + Send + Sync>>
    where
        T: Any + Send + Sync,
    {
        self.0.entry(TypeId::of::<T>())
    }

    /// return true if `T` is present
    pub fn contains<T>(&self) -> bool
    where
        T: Any + Send + Sync,
    {
        self.0.contains_key(&TypeId::of::<T>())
    }

    /// Look up and return a reference to the context global library
    /// state of type `T`.
    ///
    /// If none is registered in this context for `T` return `None`
    pub fn get<T>(&mut self) -> Option<&T>
    where
        T: Any + Send + Sync,
    {
        self.0.get(&TypeId::of::<T>()).map(|t| t.downcast_ref::<T>().unwrap())
    }

    /// Look up and return a mutable reference to the context global
    /// library state of type `T`.
    ///
    /// If none is registered return `None`
    pub fn get_mut<T>(&mut self) -> Option<&mut T>
    where
        T: Any + Send + Sync,
    {
        self.0.get_mut(&TypeId::of::<T>()).map(|t| t.downcast_mut::<T>().unwrap())
    }

    /// Set the context global library state of type `T`
    ///
    /// Any existing state will be returned
    pub fn set<T>(&mut self, t: T) -> Option<Box<T>>
    where
        T: Any + Send + Sync,
    {
        self.0
            .insert(TypeId::of::<T>(), Box::new(t) as Box<dyn Any + Send + Sync>)
            .map(|t| t.downcast::<T>().unwrap())
    }

    /// Remove and refurn the context global state library state of type `T`
    pub fn remove<T>(&mut self) -> Option<Box<T>>
    where
        T: Any + Send + Sync,
    {
        self.0.remove(&TypeId::of::<T>()).map(|t| t.downcast::<T>().unwrap())
    }
}

/// A registry of abstract type UUIDs used by graphix and graphix libraries,
/// along with a string tag describing what the type is. We must do this because
/// you can't register different type ids with the same uuid in netidx's
/// abstract type system, and because abstract types often need to be
/// parameterized by the Rt and UserEvent they will have different a different
/// type id for each monomorphization, and thus they must have a different uuid.
///
/// The tag is necessary because non parameterized functions often end up with
/// an abstract netidx type and want to know generally what it is, for example
/// printing functions.
#[derive(Default)]
pub struct AbstractTypeRegistry {
    by_tid: AHashMap<TypeId, Uuid>,
    by_uuid: AHashMap<Uuid, &'static str>,
}

impl AbstractTypeRegistry {
    fn with<V, F: FnMut(&mut AbstractTypeRegistry) -> V>(mut f: F) -> V {
        static REG: LazyLock<Mutex<AbstractTypeRegistry>> =
            LazyLock::new(|| Mutex::new(AbstractTypeRegistry::default()));
        let mut g = REG.lock();
        f(&mut *g)
    }

    /// Get the UUID of abstract type T
    pub(crate) fn uuid<T: Any>(tag: &'static str) -> Uuid {
        Self::with(|rg| {
            *rg.by_tid.entry(TypeId::of::<T>()).or_insert_with(|| {
                let id = Uuid::new_v4();
                rg.by_uuid.insert(id, tag);
                id
            })
        })
    }

    /// return the tag of this abstract type, or None if it isn't registered
    pub fn tag(a: &Abstract) -> Option<&'static str> {
        Self::with(|rg| rg.by_uuid.get(&a.id()).map(|r| *r))
    }

    /// return true if the abstract type has tag
    pub fn is_a(a: &Abstract, tag: &str) -> bool {
        match Self::tag(a) {
            Some(t) => t == tag,
            None => false,
        }
    }
}

/// Side channel for the interpreter's sync-tail-recursion loop. A
/// tail-position self-call (`CallSite::update`, flagged
/// `is_self_tail_call` by `analysis::analyze`) stashes its rebind args
/// here and returns without dispatching; the enclosing `GXLambda::update`
/// loop takes it, rebinds the formals, and re-runs the body — looping in
/// place instead of recursing on the Rust stack. A single slot suffices:
/// a tail call is the last thing evaluated, and the owning lambda
/// consumes it immediately on the way back up.
pub(crate) struct PendingTailCall {
    /// The recursive callee's `LambdaId` — the loop key the owning
    /// `GXLambda::update` matches against `self.id`.
    pub(crate) lambda: LambdaId,
    /// The self-call's argument values, in callee-formal order. `None`
    /// means the arg expression produced nothing this jump (bottomed,
    /// or quiet with no cached value) — the formal RIDES its previous
    /// value, the interp twin of the kernel's taint-gated tail rebind
    /// (`emit_tail_rebind_jump`; Eric's ruling 2026-07-15/16: a
    /// bottomed tail-jump arg rides its previous value on BOTH
    /// backends). Before this, an incomplete arg set fell through to
    /// genuine depth-charged recursion, so the same loop agreed with
    /// the kernel below the depth limit and silently aborted above it
    /// (jul16a fuzz divergence class B).
    ///
    /// Each present arg carries its production TAG (Eric's ruling
    /// 2026-07-18, the tail_jump_fired_plumbing pin): the rebind used
    /// to deliver every jumped formal unconditionally FIRED — bare
    /// `Value`s left it nothing else to go on — which manufactured
    /// freshness for results that depend on nothing that fired (an
    /// `n - 1` chain from a quiet entry read as an event). The tag is
    /// the arg expression's honest production tag, so freshness rides
    /// the dataflow exactly as the kernel's disc carry does.
    pub(crate) args: smallvec::SmallVec<[Option<TagValue>; 4]>,
}

#[derive(Clone)]
pub(crate) struct ResolvingLambda {
    pub instance: LambdaInstanceId,
    pub ftype: FnType,
}

pub struct ExecCtx<R: Rt, E: UserEvent> {
    // used to wrap lambdas into an abstract netidx value type
    lambdawrap: AbstractWrapper<LambdaDef<R, E>>,
    // all registered built-in functions
    builtins: AHashMap<&'static str, BuiltInInitFn<R, E>>,
    // all registered attributes (`#[name]`), keyed by bare name. An attr whose
    // name is absent here is an "unknown attribute" compile error; present
    // names are checked post-fusion via their `AttributeCheckFn`.
    attributes: AHashMap<&'static str, AttributeCheckFn<R, E>>,
    // whether calling built-in functions is allowed in this context, used for
    // sandboxing
    builtins_allowed: bool,
    // hash consed variant tags
    tags: AHashSet<ArcStr>,
    /// context global library state for built-in functions
    pub libstate: LibState,
    /// the language environment, typdefs, binds, lambdas, etc
    pub env: Env,
    /// the runtime
    pub rt: R,
    /// LambdaDefs indexed by LambdaId, used by `CallSite::typecheck1` to
    /// reach each callee/callback's retained check `Apply` (`def.check`).
    pub lambda_defs: IntMap<LambdaId, Value>,
    /// `BindId → LambdaDef Value` for every lambda binding in the current
    /// compile BATCH. Populated during `typecheck0` (each
    /// `Bind::typecheck0` records its own binding, `Module::typecheck0`
    /// adds the signature→impl proxy entries) so it is globally complete
    /// before `typecheck1` runs the static-resolution it feeds: a
    /// `CallSite` whose `fnode` is a `Ref` looks its `BindId` up here to
    /// pre-bind to a known lambda. This is a compile-time ANALYSIS map,
    /// deliberately kept out of `cached` (runtime state) — see
    /// `CallSite::typecheck1`.
    ///
    /// Scoped to the BATCH, not the individual `compile` call: the RT
    /// compiles a program's top-level statements as SEPARATE `compile`
    /// calls, so clearing per-`compile` would hide a lambda defined in
    /// one statement (`let rec f = …`) from a call in a later statement
    /// (e.g. inside an HOF callback — #203). PERSISTENT across batches
    /// since the jul12 shell resolution-flap fix: the old per-batch
    /// clear dropped the stdlib's entries before the user file
    /// compiled, so resolution fell to the `store_value` fallback and
    /// FUSION became a race against the previous batch's init cycle.
    /// Each RT batch entry instead prunes the OUTGOING batch's
    /// `unstable_bindings` (exactly the `<-`-retargeted lambdas the
    /// clear guarded against — shadowing mints fresh BindIds, so a
    /// pruned id can't be re-inserted with a stale value), and
    /// `Bind::delete` removes its ids (bounds growth on long-lived
    /// LSP/REPL runtimes). BindIds are globally unique, so
    /// accumulation is collision-free; the `unstable_bindings` guard
    /// still excludes the current batch's `<-` targets at read time.
    pub bind_to_lambda: IntMap<BindId, Value>,
    /// BindIds of bindings that are the target of a `<-` (Connect)
    /// somewhere in the program. Populated lazily by
    /// [`node::Connect::compile`] — every Connect resolves
    /// its target name to a BindId via `env.lookup_bind`, and that
    /// BindId is recorded here. Fusion's stability checks consult
    /// this set to determine whether a Bind's value can be safely
    /// fused (a `<-` target rebinds at runtime, so a static splice
    /// into native code would dispatch into stale state).
    pub unstable_bindings: IntSet<BindId>,
    /// The same `<-` targets, but DURABLE: `unstable_bindings` is
    /// cleared at the head of every compile batch and repopulated as
    /// that batch's Connects compile, so it only ever names the current
    /// batch. `Bind::update` needs the question answered for the whole
    /// program lifetime — a select arm's wake must not reseed a `<-`
    /// target that already holds a value (sleep is PAUSE) — and a REPL
    /// or dynamic-module batch compiled afterwards would otherwise wipe
    /// the answer for every earlier arm. Populated beside
    /// `unstable_bindings` by [`node::Connect::compile`]; `Bind::delete`
    /// removes its ids, so growth is bounded exactly as
    /// `bind_to_lambda`'s is.
    pub connect_targets: IntSet<BindId>,
    /// `(scope, name)` → builtin metadata for bindings whose value
    /// is a builtin lambda (i.e. `let foo = |...| 'builtin_name`).
    /// Keyed by name+scope rather than `BindId` because sig and
    /// impl share the same `(scope, name)` but get distinct
    /// `BindId`s — external Refs resolve to the SIG id while
    /// `Bind::compile` registers under the IMPL id, so a `BindId`-
    /// keyed map would miss external lookups. Name+scope is the
    /// single canonical key that both sides see consistently.
    ///
    /// The info captures everything fusion needs to lower an
    /// `Apply` site whose `function` resolves to this binding into
    /// a DynCall against a
    /// [`fusion::kernel_abi::FnSource::Builtin`] slot: the canonical
    /// builtin `name` (matches `ctx.builtins`), the source-level
    /// `argspec` (with default expressions for labeled args), and
    /// the resolved `FnType`.
    ///
    /// Populated by [`node::bind::Bind::compile`] when the
    /// value being bound is an `ExprKind::Lambda` with
    /// `body == Either::Right(name)`. User-lambda bindings leave
    /// no entry here; only builtin lambdas appear.
    pub builtin_bindings: AHashMap<(ModPath, CompactString), BuiltinBindInfo>,
    /// `LambdaId`s whose def-time body typecheck is IN PROGRESS
    /// (`Lambda::typecheck0`'s faux instantiation). A `CallSite` whose
    /// callee ftype carries one of these ids is a recursive self-call
    /// inside the very body being checked: it must unify against the
    /// def's OWN ftype cells instead of a `reset_tvars` freshening —
    /// monomorphic recursion. The knot makes the μ-equation collapse
    /// (`'r ⊇ [T, 'r]` binds `'r := T` through the same-cell rule) and
    /// makes a self-call arg mismatch a def-time error, exactly as it
    /// is for the non-recursive twin. A freshened self-call instead
    /// leaves an orphan cell in the arm union that `constrain_known`
    /// widens to `Any` — the recursive lambda's checked signature
    /// silently degrades (soak jul05 items 11/17).
    pub(crate) rec_defs: nohash::IntSet<LambdaId>,
    /// Param-call knot for the definition gate: the
    /// fn-typed arg-pattern BindIds of the lambda def(s) currently
    /// being body-checked. A callsite whose fnode is a Ref to one of
    /// these unifies against the param's OWN declared FnType cells
    /// (a shallow clone — the same knot `rec_defs` gives self-calls)
    /// instead of a freshened instantiation, so `f(v)` inside
    /// `|f: fn(x: 'a) -> 'b, …|` types as the def's rigid 'b and the
    /// declared-rtype acceptance check can see the body delivers it.
    pub(crate) def_gate_params: nohash::IntSet<BindId>,
    /// Def-gate nesting depth. `constrain_known` (the obs4 def-time
    /// fact conjuncts) runs only at depth 1: a lambda gated INSIDE an
    /// enclosing def's gate (an inline HOF callback) has inferred
    /// cells still entangled with the enclosing inference, and
    /// recording them as CLOSED facts (`bind_as(Any)` snapshots) can
    /// bind a shared cell to `Array<Any>` and make the enclosing rigid
    /// return-type check reject its own body.
    pub(crate) def_gate_depth: usize,
    pub(crate) resolving_lambdas:
        Arc<parking_lot::Mutex<nohash::IntMap<LambdaId, ResolvingLambda>>>,
    pub(crate) resolving_sites: Arc<parking_lot::Mutex<nohash::IntSet<u64>>>,
    /// All state owned by the fusion subsystem — the JIT module,
    /// kernel caches, abstract-type registry, builtin effects, and the
    /// compile-time fusion flags/counters. Grouped into one struct so
    /// `ExecCtx` isn't cluttered with loose fusion fields; reached as
    /// `ctx.fusion.<x>`. See [`fusion::FusionCtx`].
    pub fusion: fusion::FusionCtx,
    /// Runtime side channel for the interpreter's tail-call loop. See
    /// [`PendingTailCall`]. `None` except for the instant between a tail
    /// self-call stashing its args and the owning lambda consuming them.
    pub(crate) pending_tail_call: Option<PendingTailCall>,
    /// Imports whose terminal name did not exist when the `use`
    /// compiled (a `use self::sub::x` may legitimately precede
    /// `mod sub;` in body order). Drained and re-checked at the end
    /// of [`compile_stmt`] — an entry still naming nothing is a
    /// compile error there, so a typo'd import cannot ride along
    /// silently.
    pub(crate) pending_imports: Vec<PendingImport>,
    /// Module names pre-registered by a block's headers scan (one
    /// AST pass over each block's direct children before they
    /// compile), making `mod` declaration order irrelevant to name
    /// resolution. The `Module` compile arm removes its own entry
    /// instead of tripping the duplicate-module guard on it.
    pub(crate) predeclared_mods: AHashSet<ModPath>,
    /// `LambdaId`s whose `GXLambda::update` is currently ON the Rust
    /// call stack, with activation counts (a multiset — recursion
    /// activates the same id many times). `CallSite::bind` consults it:
    /// binding a callee whose def is already active is a recursive
    /// unfold, and a qualifying body (see
    /// `callsite::transient_body_ok`) binds TRANSIENT — the instance is
    /// deleted when its dispatch returns, so non-tail recursion holds
    /// O(depth) instances instead of one per dynamic call (the full
    /// call TREE — fib(28) retained 1M instances / 9.6GB).
    pub(crate) active_lambdas: nohash::IntMap<LambdaId, u32>,
    /// Interrupt/abort control, shared (cloned `Arc`) with the runtime
    /// handle. Loops poll it via [`Self::interrupted`] to abort a wedge;
    /// the runtime polls `control.aborted()` to shut down. See [`Control`].
    pub control: Arc<Control>,
    /// Runtime diagnostics produced during the current cycle — failures
    /// whose value-level outcome is BOTTOM by design (no value, nothing
    /// to catch) but that users still need to hear about. Trip sites
    /// push here; the runtime drains after each node update and
    /// forwards to embedders through its event stream (the shell prints
    /// them). See [`RtDiagnostic`].
    pub diagnostics: Vec<RtDiagnostic>,
    /// Non-zero while executing a tail-loop re-entry against a private,
    /// per-frame variables map (see `reset_replay`). Frame-only behaviors
    /// (a call site's
    /// stale-channel arg delivery; error sites producing silent tainted
    /// placeholders instead of the logged None) gate on this so
    /// reactive-land semantics are untouched. A counter, not a bool:
    /// frames nest. The per-value fired/taint channels themselves ride
    /// [`TagValue`] — see `design/replay_frames.md` v2.
    pub(crate) frame_depth: u32,
    /// The REAL `event.init` of the dispatch whose evaluation frames
    /// are currently running (frames FORCE `event.init` for
    /// re-derivation, so literal nodes can't read the flag directly).
    /// The interp twin of the kernel's `init_flag` wire slot, which is
    /// uniform across all of an invocation's loop iterations: a
    /// constant inside a frame produces FIRED iff the dispatch itself
    /// was a genuine init (`const_stale_gate`). Only meaningful when
    /// `frame_depth > 0`.
    pub(crate) frame_init: bool,
    /// Accumulates the tail-spine selects' scrutinee firing across one
    /// tail-loop dispatch — the interp twin of the kernel's
    /// `tail_scrut_stale` loop accumulator, which every
    /// `emit_kernel_return` folds into the result disc: a dispatch's
    /// result fires if its value chain fired OR any tail-select
    /// scrutinee on the executed path did (control-dependence firing).
    /// Tail-spine selects OR into it (`node/select.rs`); the tail-loop
    /// dispatch saves/resets it on entry and applies it in the
    /// result-tag derivation (`node/lambda.rs`).
    pub(crate) tail_scrut_fired: bool,
    /// Pending definition assertions (see [`DefAssertion`]): stamped at
    /// compile, verified and removed at `analysis::analyze`'s tail once the
    /// asserted definition is reached by the analysis. Persistent across
    /// compiles (a REPL definition is verified when a later statement calls
    /// it); deduped on push.
    pub(crate) def_assertions: Mutex<Vec<DefAssertion>>,
    /// Registry-attribute honesty census (reset per `compile_stmt`): every
    /// registry attribute (`#[native]`, package attrs) recorded at compile
    /// must, by the end of the fusion walk, have been DISPATCHED
    /// (`fusion::check_node_attributes`) or ABSORBED into a successfully
    /// fused region (collected at splice) — anything left over sits in a
    /// position the fusion walk cannot reach, and lying silently is not an
    /// option: `compile_stmt` errors on it. Empty census ⇒ zero work.
    pub(crate) attr_census: Mutex<Vec<Expr>>,
    pub(crate) attr_dispatched: Mutex<IntSet<ExprId>>,
    pub(crate) attr_absorbed: Mutex<IntSet<ExprId>>,
    /// Variable deliveries raised inside an evaluation frame that must
    /// ESCAPE it. A frame runs the body against a PRIVATE
    /// `event.variables` map, which is the point — an interior publish
    /// dies with the pass. But an error delivery to a `catch` handler
    /// is outward-bound, like a `<-` to a binding outside the lambda,
    /// and the private map swallowed it, so the handler never ran
    /// (`findings/qop-tailloop-frame-swallow-aug2026`; the kernel has
    /// no frame, so its QopDeliver wrote the real variable and the two
    /// engines disagreed). `Qop::update` parks the delivery here
    /// instead; `GXLambda::update` drains it into the real map once the
    /// frames unwind. Nested frames bubble: the drain only runs at
    /// `frame_depth == 0`.
    pub(crate) frame_outbox: Vec<(BindId, Value)>,
}

impl<R: Rt, E: UserEvent> ExecCtx<R, E> {
    pub fn clear(&mut self) {
        self.env.clear();
        self.rt.clear();
    }

    /// Build a new execution context.
    ///
    /// This is a very low level interface that you can use to build a
    /// custom runtime with deep integration to your code. It is very
    /// difficult to use, and if you don't implement everything
    /// correctly the semantics of the language can be wrong.
    ///
    /// Most likely you want to use the `rt` module instead.
    pub fn new(user: R) -> Result<Self> {
        let id = AbstractTypeRegistry::uuid::<LambdaDef<R, E>>("lambda");
        let mut this = Self {
            lambdawrap: Abstract::register(id)?,
            env: Env::default(),
            builtins: AHashMap::default(),
            attributes: AHashMap::default(),
            builtins_allowed: true,
            libstate: LibState::default(),
            tags: AHashSet::default(),
            rt: user,
            lambda_defs: IntMap::default(),
            bind_to_lambda: IntMap::default(),
            unstable_bindings: nohash::IntSet::default(),
            connect_targets: nohash::IntSet::default(),
            builtin_bindings: ahash::AHashMap::default(),
            rec_defs: nohash::IntSet::default(),
            def_gate_params: nohash::IntSet::default(),
            def_gate_depth: 0,
            resolving_lambdas: Arc::new(parking_lot::Mutex::new(
                nohash::IntMap::default(),
            )),
            resolving_sites: Arc::new(parking_lot::Mutex::new(nohash::IntSet::default())),
            fusion: fusion::FusionCtx::new()?,
            pending_tail_call: None,
            pending_imports: Vec::new(),
            predeclared_mods: AHashSet::default(),
            active_lambdas: nohash::IntMap::default(),
            control: Arc::new(Control::new()),
            diagnostics: Vec::new(),
            frame_depth: 0,
            frame_init: false,
            tail_scrut_fired: false,
            def_assertions: Mutex::new(Vec::new()),
            attr_census: Mutex::new(Vec::new()),
            attr_dispatched: Mutex::new(IntSet::default()),
            attr_absorbed: Mutex::new(IntSet::default()),
            frame_outbox: Vec::new(),
        };
        // `#[native]` is a language-level attribute (its check is
        // compiler-internal), so it is registered here rather than by a
        // package. Other attributes can be registered via `register_attribute`.
        this.register_attribute::<Native>()?;
        Ok(this)
    }

    /// True if an `interrupt()` or `abort()` is pending on this context's
    /// [`Control`]. Loopy builtins (and the interpreter's tail loop)
    /// poll this at their loop head and `return None` to abort a wedge.
    pub fn interrupted(&self) -> bool {
        self.control.interrupted()
    }

    pub fn register_builtin<T: BuiltIn<R, E>>(&mut self) -> Result<()> {
        if node::collection::CollectionIntrinsic::from_name(T::NAME).is_some() {
            bail!("{} is a collection intrinsic reserved by the compiler", T::NAME)
        }
        match self.builtins.entry(T::NAME) {
            Entry::Vacant(e) => {
                e.insert(T::init);
            }
            Entry::Occupied(_) => bail!("builtin {} is already registered", T::NAME),
        }
        self.fusion.builtin_facts.insert(
            T::NAME,
            effects::BuiltinFacts {
                effect: T::EFFECT,
                stateless: T::STATELESS,
                sleep_restarts: T::SLEEP_RESTARTS,
            },
        );
        Ok(())
    }

    pub fn register_attribute<T: Attribute<R, E>>(&mut self) -> Result<()> {
        match self.attributes.entry(T::NAME) {
            Entry::Vacant(e) => {
                e.insert(T::check);
            }
            Entry::Occupied(_) => {
                bail!("attribute {} is already registered", T::NAME)
            }
        }
        Ok(())
    }

    /// The check fn for a registered attribute, or `None` if `name` is not a
    /// known attribute. Used by the compiler to reject unknown attributes and
    /// by the fusion walk to dispatch each known attribute's check.
    pub fn lookup_attribute(&self, name: &str) -> Option<AttributeCheckFn<R, E>> {
        self.attributes.get(name).copied()
    }

    /// Look up the sync/async effect of a registered builtin. Returns
    /// `EffectKind::Async` (the conservative default) for unknown names
    /// so callers don't need to handle the "missing" case specially.
    pub fn builtin_effect(&self, name: &str) -> EffectKind {
        self.fusion.builtin_facts.get(name).map(|f| f.effect).unwrap_or_default()
    }

    /// Look up a registered builtin's `STATELESS` declaration (see
    /// [`BuiltIn::STATELESS`]). Returns `false` (the conservative
    /// default) for unknown names.
    pub fn builtin_stateless(&self, name: &str) -> bool {
        self.fusion.builtin_facts.get(name).map(|f| f.stateless).unwrap_or(false)
    }

    /// Look up a registered builtin's `SLEEP_RESTARTS` declaration
    /// (see [`BuiltIn::SLEEP_RESTARTS`]). Returns `true` (the
    /// conservative reading for the interior-sleep gate) for unknown
    /// names.
    pub fn builtin_sleep_restarts(&self, name: &str) -> bool {
        self.fusion.builtin_facts.get(name).map(|f| f.sleep_restarts).unwrap_or(true)
    }

    /// Wrap a `LambdaDef` into a `Value` that can be returned from a builtin
    /// as a first-class function value. The runtime handles the resulting
    /// Value as a callable lambda — call sites resolve against the LambdaDef's
    /// `init` to construct an Apply impl. Also registers the LambdaDef in
    /// `lambda_defs` so deferred typechecking can find it.
    pub fn wrap_lambda(&mut self, def: LambdaDef<R, E>) -> Value {
        let id = def.id;
        let v = self.lambdawrap.wrap(def);
        self.lambda_defs.insert(id, v.clone());
        v
    }

    fn tag(&mut self, s: &ArcStr) -> ArcStr {
        match self.tags.get(s) {
            Some(s) => s.clone(),
            None => {
                self.tags.insert(s.clone());
                s.clone()
            }
        }
    }

    /// Restore the lexical environment to the snapshot `env` for the duration
    /// of `f` restoring it to it's original value afterwords. `by_id` and
    /// `lambdas` defined by the closure will be retained.
    pub fn with_restored<T, F: FnOnce(&mut Self) -> T>(&mut self, env: Env, f: F) -> T {
        let snap = self.env.restore_lexical_env(env);
        let orig = mem::replace(&mut self.env, snap);
        let r = f(self);
        self.env = self.env.restore_lexical_env(orig);
        r
    }

    /// Restore the lexical environment to the snapshot `env` for the duration
    /// of `f` restoring it to it's original value afterwords. `by_id` and
    /// `lambdas` defined by the closure will be retained. `env` will be mutated
    /// instead of requiring a clone, this allows maintaining continuity in two
    /// different envs across multiple invocations
    pub fn with_restored_mut<T, F: FnOnce(&mut Self) -> T>(
        &mut self,
        env: &mut Env,
        f: F,
    ) -> T {
        let snap = self.env.restore_lexical_env_mut(env);
        let orig = mem::replace(&mut self.env, snap);
        let r = f(self);
        *env = self.env.clone();
        self.env = self.env.restore_lexical_env(orig);
        r
    }
}

/// A deferred import-existence check: see
/// [`ExecCtx::pending_imports`].
#[derive(Debug)]
pub(crate) struct PendingImport {
    pub(crate) scope: ModPath,
    pub(crate) key: compact_str::CompactString,
    pub(crate) pos: combine::stream::position::SourcePosition,
    pub(crate) ori: triomphe::Arc<expr::Origin>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub lexical: ModPath,
    pub dynamic: ModPath,
}

impl Scope {
    pub fn append<S: AsRef<str> + ?Sized>(&self, s: &S) -> Self {
        Self {
            lexical: ModPath(self.lexical.append(s)),
            dynamic: ModPath(self.dynamic.append(s)),
        }
    }

    /// Append a generated block-scope component (do/fn/sel/ca level).
    pub fn append_block(&self, kind: &str, id: u64) -> Self {
        self.append(block_component(kind, id).as_str())
    }

    pub fn root() -> Self {
        Self { lexical: ModPath::root(), dynamic: ModPath::root() }
    }
}

/// Format a generated block-scope component. The `#` prefix marks it
/// as a non-module level: identifiers cannot start with `#`, so a
/// scope path structurally records where the enclosing module ends —
/// [`mod_root`] strips trailing marked components.
pub fn block_component(kind: &str, id: u64) -> CompactString {
    compact_str::format_compact!("#{kind}{id}")
}

/// True iff `part` is a generated block-scope component rather than
/// a module name.
pub fn is_block_component(part: &str) -> bool {
    part.starts_with('#')
}

/// The module root of a lexical scope path: the path minus trailing
/// generated block components.
pub fn mod_root(mut scope: &str) -> &str {
    use netidx_core::path::Path;
    while let Some(base) = Path::basename(scope) {
        if !is_block_component(base) {
            break;
        }
        match Path::dirname(scope) {
            Some(d) => scope = d,
            None => return "/",
        }
    }
    scope
}

/// compile the expression into a node graph in the specified context
/// and scope, return the root node or an error if compilation failed.
pub fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<Node<R, E>> {
    compile_stmt(ctx, flags, scope, spec).map(|(n, _)| n)
}

/// [`compile`] for statement-list drivers that compile top-level
/// expressions ONE AT A TIME (the shell REPL, `compile_root`, the
/// checker) instead of through a file's synthetic `Do` wrap: a
/// top-level `catch(e) expr` is legal here — it is statement position
/// — and ADVANCES the scope for the statements that follow, exactly
/// like a catch child inside a block. Thread the returned scope into
/// the next statement's compile (and, for a live session, persist it)
/// or later statements silently escape the catch's coverage. For
/// every other expression kind the scope is returned unchanged.
pub fn compile_stmt<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<(Node<R, E>, Scope)> {
    // Fusion runs whenever the program typechecks — including in check/lsp
    // runtimes, which it makes safe two ways: (1) fusion's emit path never
    // panics; on any malformed input it returns Err and de-fuses to the
    // node-walk (the universal correct fallback), so an ill-typed expr that
    // slips past lsp's keep-going typecheck can at worst lose fusion, never
    // crash; and (2) `#[native]` needs fusion to actually run during a check
    // to verify its contract. The one real precondition — that the program
    // typechecked — is already enforced structurally: typecheck0/1 + analyze
    // (below) early-return on any error before the fusion call, so fusion is
    // reached iff this expr is well-typed.
    ctx.fusion.enabled = !flags.contains(CFlag::FusionDisabled);
    ctx.attr_census.lock().clear();
    ctx.attr_dispatched.lock().clear();
    ctx.attr_absorbed.lock().clear();
    ctx.pending_imports.clear();
    ctx.predeclared_mods.clear();
    let top_id = spec.id;
    ctx.fusion.top_id = Some(top_id);
    let env = ctx.env.clone();
    let st = Instant::now();
    let compiled = match &spec.kind {
        expr::ExprKind::Catch(c) => {
            let c = c.clone();
            node::error::Catch::compile(ctx, flags, spec, scope, top_id, &c)
        }
        _ => {
            compiler::compile(ctx, flags, spec, scope, top_id).map(|n| (n, scope.clone()))
        }
    };
    let (mut node, out_scope) = match compiled {
        Ok(n) => n,
        Err(e) => {
            ctx.env = env;
            return Err(e);
        }
    };
    info!("compile time {:?}", st.elapsed());
    // Deferred import-existence checks: a `use` whose terminal name
    // didn't exist at its compile position (e.g. `use self::sub::x`
    // ahead of `mod sub;`) must name something by the end of the
    // statement, so a typo'd import cannot ride along silently.
    for p in mem::take(&mut ctx.pending_imports) {
        let Some(e) = ctx.env.names.get(&p.scope).and_then(|sn| sn.imports.get(&p.key))
        else {
            continue;
        };
        if !ctx.env.import_target_exists(e) {
            let err = ::anyhow::anyhow!(
                "use: no `{}` in `{}` (checked again after the enclosing \
                 statement finished compiling)",
                e.name,
                e.scope
            )
            .context(expr::ParserContext { ori: p.ori.clone(), pos: p.pos });
            ctx.env = env;
            return Err(err);
        }
    }
    let st = Instant::now();
    if let Err(e) = node.typecheck0(ctx) {
        ctx.env = env;
        return Err(e);
    }
    if let Err(e) = node.typecheck1(ctx) {
        ctx.env = env;
        return Err(e);
    }
    info!("typecheck time {:?}", st.elapsed());
    // Function-property analysis (effect + recursion). Runs ALWAYS — the
    // node-walk interpreter reads its results (the tail-loop facts) too,
    // not just fusion. Must come after typecheck1 (it needs resolved call
    // sites + a complete `bind_to_lambda`) and before fusion (which reads
    // the shared `tail_loop` fact).
    if let Err(e) = analysis::analyze(&node, ctx) {
        ctx.env = env;
        return Err(e);
    }
    if ctx.fusion.enabled {
        let st = Instant::now();
        ctx.env.seed_typedef_refs();
        if let Err(e) = fusion::fuse(&mut node, ctx) {
            ctx.env = env;
            return Err(e);
        }
        info!("fusion time {:?}", st.elapsed());
    }
    // Registry-attribute honesty reconciliation (see `attr_census`): a
    // decorated expression the fusion walk neither dispatched nor absorbed
    // sits where no check can reach it — error loudly instead of letting
    // the annotation silently assert nothing. Skipped under `--no-fusion`
    // (the documented vacuity) and free when no attributes exist.
    if ctx.fusion.enabled {
        let census = ctx.attr_census.lock();
        if !census.is_empty() {
            let dispatched = ctx.attr_dispatched.lock();
            let absorbed = ctx.attr_absorbed.lock();
            for spec in census.iter() {
                if !dispatched.contains(&spec.id) && !absorbed.contains(&spec.id) {
                    let e = ::anyhow::anyhow!(
                        "attribute in a position the fusion pass cannot check — \
                         put the decorated expression in its own statement \
                         (or a select arm)"
                    )
                    .context(expr::ErrorContext(spec.clone()));
                    drop(census);
                    drop(dispatched);
                    drop(absorbed);
                    ctx.env = env;
                    return Err(e);
                }
            }
        }
    }
    Ok((node, out_scope))
}
