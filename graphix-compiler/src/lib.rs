#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
#[macro_use]
extern crate netidx_core;
#[macro_use]
extern crate combine;
#[macro_use]
extern crate serde_derive;

pub mod analysis;
pub mod effects;
pub mod env;
pub mod expr;
pub mod fusion;
pub mod ide;
pub mod node;
pub mod node_shape;
pub mod typ;

use compact_str::CompactString;
// Re-exported so packages implementing `Apply::emit_clif` get the
// cranelift types through the compiler — no direct cranelift dep in a
// package, so version lockstep with the JIT is structural.
pub use cranelift_codegen;
pub use cranelift_frontend;
pub use fusion::FusionStats;

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
use netidx::{
    path::Path,
    publisher::{Id, Val, WriteRequest},
    subscriber::{self, Dval, SubId, UpdatesFlags, Value},
};
use netidx_protocols::rpc::server::{ArgSpec, RpcCall};
use netidx_value::{Abstract, abstract_type::AbstractWrapper};
use node::compiler;
use nohash::{IntMap, IntSet};
use parking_lot::{Mutex, RwLock};
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
    /// Disable fusion entirely — both the compile-time whole-graph
    /// phase AND the runtime per-slot HOF-callback fusion (which gates
    /// on `ctx.fusion.enabled`, set from this flag). `compile()`
    /// runs build + typecheck and returns the regular Node graph; no
    /// kernels are built or spliced anywhere. This is the test harness's
    /// `interp` mode: the program executes PURELY through the
    /// Update-trait node-walk, the canonical model. The single fusion
    /// control knob — fusion is JIT-only (no interpreter), so there is
    /// no meaningful "fuse but don't JIT" state to represent.
    FusionDisabled,
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
        }
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
    /// with [`depth_pop`](Self::depth_pop) after a `true` return.
    pub fn depth_push(&self) -> bool {
        let d = self.depth.fetch_add(1, Ordering::Relaxed);
        if d >= self.max_depth.load(Ordering::Relaxed) {
            self.depth.fetch_sub(1, Ordering::Relaxed);
            false
        } else {
            true
        }
    }

    /// Enter the callback-dispatch level a fused HOF loop's inlined
    /// body runs at — the kernel twin of the node-walk's per-element
    /// `depth_push` in `GXLambda::update`. One unit covers the whole
    /// loop: sequential element dispatches all sit at the same level,
    /// so the per-element push/pop pairs collapse to one. Increments
    /// UNCONDITIONALLY (pair with an unconditional
    /// [`depth_pop`](Self::depth_pop) — no branchy cleanup); `false` =
    /// the limit is reached and the loop must not run, its result
    /// tainted (the node-walk's per-element trip observable).
    pub fn depth_enter(&self) -> bool {
        let d = self.depth.fetch_add(1, Ordering::Relaxed);
        d < self.max_depth.load(Ordering::Relaxed)
    }

    pub fn depth_pop(&self) {
        self.depth.fetch_sub(1, Ordering::Relaxed);
    }

    /// Reset the depth counter to zero. Called by the runtime at the
    /// end of each cycle — pure insurance against a leaked push (a bug,
    /// but one that would otherwise ratchet every later cycle toward a
    /// spurious limit).
    pub fn depth_reset(&self) {
        self.depth.store(0, Ordering::Relaxed);
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
        Value::Error(::triomphe::Arc::new(e))
    }};
}

#[macro_export]
macro_rules! errf {
    ($tag:expr, $fmt:expr, $($args:expr),*) => {{
        let msg: ArcStr = ::compact_str::format_compact!($fmt, $($args),*).as_str().into();
        let e: Value = ($tag.clone(), msg).into();
        Value::Error(::triomphe::Arc::new(e))
    }};
    ($tag:expr, $fmt:expr) => {{
        let msg: ArcStr = ::compact_str::format_compact!($fmt).as_str().into();
        let e: Value = ($tag.clone(), msg).into();
        Value::Error(::triomphe::Arc::new(e))
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

atomic_id!(BindId);

/// The old->new `BindId` table a `clone_rebind` walk threads through
/// every node: re-minted bindings record themselves, `Ref`s resolve
/// through it (an id not in the table is a capture and passes through
/// unchanged). See [`Update::clone_rebind`].
pub type RebindMap = nohash::IntMap<BindId, BindId>;

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
    pub variables: IntMap<BindId, Value>,
    pub netidx: IntMap<SubId, subscriber::Event>,
    pub writes: IntMap<Id, WriteRequest>,
    pub rpc_calls: IntMap<BindId, RpcCall>,
    pub custom: IntMap<BindId, Box<dyn CustomBuiltinType>>,
    pub user: E,
}

impl<E: UserEvent> Event<E> {
    pub fn new(user: E) -> Self {
        Event {
            init: false,
            variables: IntMap::default(),
            netidx: IntMap::default(),
            writes: IntMap::default(),
            rpc_calls: IntMap::default(),
            custom: IntMap::default(),
            user,
        }
    }

    pub fn clear(&mut self) {
        let Self { init, variables, netidx, rpc_calls, writes, custom, user } = self;
        *init = false;
        variables.clear();
        netidx.clear();
        rpc_calls.clear();
        custom.clear();
        writes.clear();
        user.clear();
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

    /// True if `id` is referenced anywhere in the walked subtree
    /// (whether or not it is also bound there).
    pub fn is_refed(&self, id: BindId) -> bool {
        self.refed.contains(&id)
    }
}

pub type Node<R, E> = Box<dyn Update<R, E>>;

pub type InitFn<R, E> = sync::Arc<
    dyn for<'a, 'b, 'c, 'd> Fn(
            &'a Scope,
            &'b mut ExecCtx<R, E>,
            &'c mut [Node<R, E>],
            Option<&'d FnType>,
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

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value>;

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

    /// Second typecheck pass ("CallSite phase"). Fires on TWO distinct
    /// `Apply` instances, distinguished by `fn_args`:
    ///
    /// - On the LambdaDef's retained `check` SCRATCH apply, via
    ///   [`node::callsite::finalize_lambda`], with `fn_args = &[]`.
    ///   A builtin extracts call-site-directed types here (e.g.
    ///   `str::parse` reads its target type from `resolved`); a
    ///   `GXLambda` recurses its body.
    /// - HOF call sites ONLY: on the per-call-site BOUND apply
    ///   (`cs.callee`), via `CallSite::try_static_resolve`, with
    ///   `fn_args` listing the positional indices and `LambdaDef`s of the
    ///   fn-typed args the compiler proved resolve statically to a single
    ///   known lambda. A HOF builtin (`MapQ`/`FoldQ`/`Init`) uses these to
    ///   pre-materialize its callback `analysis_pred` — which fusion's
    ///   `emit_clif` later inlines. This MUST be built on the bound
    ///   instance (the one runtime clones per slot), so gate it on
    ///   `fn_args` (an empty slice means "no statically-resolved
    ///   callbacks", which auto-no-ops the scratch firing).
    ///
    /// Default no-op — a builtin with no call-site work ignores both
    /// `resolved` and `fn_args`. (Formerly two methods, `typecheck1` +
    /// `static_resolve_fn_args`; folded into one — the HOF hook is just
    /// the bound-instance firing of this same phase.)
    fn typecheck1(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
        _fn_args: &[StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        Ok(())
    }

    /// return the lambdas type, builtins do not need to implement
    /// this, it is implemented by the BuiltIn wrapper
    fn typ(&self) -> Arc<FnType> {
        static EMPTY: LazyLock<Arc<FnType>> = LazyLock::new(|| {
            Arc::new(FnType {
                args: Arc::from_iter([]),
                constraints: Arc::new(RwLock::new(LPooled::take())),
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

    /// Walk this Apply's INLINE-EMITTED HOF callback bodies for fusion
    /// discovery. A HOF builtin (`map`/`fold`/`filter`/…) inline-emits
    /// its callback into the enclosing kernel (the loop scaffold), but
    /// the callback body is NOT in the node graph reachable by
    /// `fusion::for_each_node` (it skips lambda bodies) — so a
    /// builtin-call / cast / qop inside the callback would never be
    /// discovered and the HOF would de-fuse. A HOF overrides this to hand
    /// each callback body Node to `f`, reached the SAME way `emit_clif`
    /// reaches it (the `analysis_pred` chain) so the ExprIds discovery
    /// registers match what emit looks up. Default: no callbacks. The
    /// `'a` ties each body's borrow to `self`, so the discovery can
    /// collect the bodies and walk them after the main pass.
    fn for_each_hof_callback_body<'a>(&'a self, _f: &mut dyn FnMut(&'a Node<R, E>)) {}

    /// Walk this Apply's FUSED HOF callback bodies for post-fusion
    /// attribute checking. The fused twin of
    /// [`Apply::for_each_hof_callback_body`]: that one exposes the
    /// pristine (unfused) `analysis_pred` body for fusion DISCOVERY;
    /// this one exposes the body of the FUSED template built by
    /// [`Apply::fuse`] (its sync sub-regions replaced by `FusedKernel`s),
    /// so `check_attributes` can descend into it and give each decorated
    /// callback node the right native/residue verdict. Default: no
    /// callbacks — also correct when the HOF batch-loop-inlined (its
    /// callback was absorbed into the enclosing kernel and there is no
    /// separate fused template).
    fn for_each_hof_fused_body<'a>(&'a self, _f: &mut dyn FnMut(&'a Node<R, E>)) {}

    /// put the node to sleep, used in conditions like select for branches that
    /// are not selected. Any cached values should be cleared on sleep.
    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>);

    /// See [`Update::clone_rebind`]. For a builtin this is the
    /// builtin's own responsibility — it clones its argument nodes
    /// through `clone_rebind` (so their internal `Ref`s/binds rebind)
    /// and constructs fresh initial state: a cloned `subscribe` comes
    /// back un-subscribed and opens its own subscription on first
    /// update, a cloned counter resets, a cloned timer arms its own.
    /// `BuiltInLambda` delegates to the inner `apply`. The default
    /// panics so a missed builtin is surfaced loudly.
    fn clone_rebind(
        &self,
        _ctx: &mut ExecCtx<R, E>,
        // The scope is where re-minted bindings register in the env
        // (and where the recompile default resolves); ref RESOLUTION
        // goes through `remap` (see `Update::clone_rebind`).
        _scope: &Scope,
        _remap: &mut RebindMap,
    ) -> Box<dyn Apply<R, E>> {
        panic!("clone_rebind not implemented for Apply {self:?}")
    }

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
    /// site FAILED (the call did NOT batch-loop inline via
    /// [`Apply::emit_clif`]). A HOF builtin (`map`/`fold`/`init`/…)
    /// overrides this to build its per-element fused callback template
    /// NOW, at compile time, via the normal `fuse_callsite` /
    /// [`fusion::fuse`] machinery — rather than lazily at runtime in
    /// `update`. Doing it here records the callback's fusion in
    /// [`fusion::FusionStats`] and makes it visible to the post-fusion
    /// graph walk (the `#[native]` checker descends into the result via
    /// [`Apply::for_each_hof_fused_body`]). The impl MUST swallow build
    /// errors (de-fuse, never fail the compile — node-walk is
    /// canonical). Default: no-op.
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

/// One entry in the `fn_args` slice passed to [`Apply::typecheck1`] (the
/// bound-instance, HOF-callback firing). Records that the call site's
/// positional arg at `arg_idx` is fn-typed AND the compiler proved
/// it resolves statically to a single known `LambdaDef`.
///
/// The `LambdaDef` borrow is tied to the `CallSite::try_static_resolve`
/// call's lifetime; the builtin should consume what it needs from
/// `lambda` synchronously (e.g. clone its `Arc<FnType>`,
/// instantiate an internal Apply via `genn::apply` referencing
/// `lambda.id`) and not store the reference.
pub struct StaticFnArg<'a, R: Rt, E: UserEvent> {
    pub arg_idx: usize,
    pub lambda: &'a LambdaDef<R, E>,
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
    Select(&'a node::select::Select<R, E>),
    TryCatch(&'a node::error::TryCatch<R, E>),
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
    Use(&'a node::Use),
    TypeDef(&'a node::TypeDef),
    Nop(&'a node::Nop),
    // Synthetic — produced by fusion itself.
    FusedKernel(&'a fusion::FusedKernel<R, E>),
}

/// Update represents a regular graph node, as opposed to a function
/// application represented by Apply. Regular graph nodes are used for
/// every built in node except for builtin functions.
pub trait Update<R: Rt, E: UserEvent>: Debug + Send + Sync + Any + 'static {
    /// update the node with the specified event and return any output
    /// it might generate
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value>;

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

    /// Return a typed view of this node for compile-time analysis.
    /// **Required, no default impl** — every `Update` impl picks a
    /// `NodeView` variant. Adding a new node type can't compile
    /// without choosing a variant; adding a new variant forces
    /// every exhaustive match consuming `NodeView` to be reviewed.
    fn view(&self) -> NodeView<'_, R, E>;

    /// Produce an independent deep copy of this node positioned as if
    /// it were compiled fresh in the same lexical scope. Semantics:
    /// every binding the subtree *introduces* is re-minted to a fresh
    /// env-registered `BindId`; every `Ref` to an internal binding
    /// re-resolves to this copy's fresh one; every capture (a `Ref` to
    /// an external binding) keeps pointing at the original outer
    /// binding; immutable shared artifacts (fused-kernel `Arc`s) are
    /// shared, not copied; and stateful builtins get fresh independent
    /// state via their own `clone_rebind`.
    ///
    /// The re-binding rides `remap`, an explicit old→new `BindId` table
    /// threaded through the whole clone walk: each re-minted binding
    /// records itself (`pattern.rs remint_bind_id`), and `Ref` resolves
    /// through the table — an id not in the table is a capture and
    /// passes through unchanged. Resolution is NEVER by name: the walk
    /// threads one flat scope, so name lookup conflates lexically
    /// distinct same-named bindings (a local named like a callee's
    /// parameter aliased to the parameter — audit-jul2026/03). The
    /// initiator (a HOF's per-slot instantiation) seeds the table with
    /// its feeder rebindings (e.g. the analysis element id → this
    /// slot's fresh element id).
    ///
    /// Used by per-slot HOF dispatch (`MapQ`) to instantiate one
    /// independent graph per array element from a single fused
    /// template.
    ///
    /// The default RE-INITS from the node's `spec`: a fresh `compile`
    /// in `scope`. This is the *correct* clone for env-managing nodes
    /// (`TryCatch`, `Module`, `Use`/`TypeDef`, `Lambda`) — `compile`
    /// re-runs the catch-scope / env-snapshot / proxy setup that no
    /// hand-written structural clone could safely duplicate. Value-
    /// computing residue (arithmetic, producers, accessors, `Select`,
    /// …) and the fusion spine override with structural clones (lighter,
    /// and the spine must preserve spliced `FusedKernel` descendants).
    /// NOTE the recompile default resolves refs BY NAME against `scope`
    /// (compile has no remap channel) — it keeps the flat-scope name
    /// hazard the structural spine no longer has; acceptable because
    /// env-managing nodes are rare inside per-slot templates.
    ///
    /// **Capture-safe.** `compile` re-resolves every `Ref` by name, but a
    /// CAPTURE was resolved in an *outer* scope not lexically visible from
    /// `scope`, so a naive recompile would fail ("not defined"). So first
    /// alias EVERY external ref's name — resolved through `remap` — into
    /// the fresh child scope below. Unconditional and deepest-wins:
    /// resolution by recorded IDENTITY, immune to same-named env
    /// pollution. The remap consultation is load-bearing: a per-slot HOF
    /// initiator seeds `remap` with its feeder rebindings (analysis
    /// element id → this slot's fresh id), so a feeder ref aliases to
    /// THIS slot's binding, while a true capture (absent from the table)
    /// aliases to its original outer binding — the same contract the
    /// structural clone spine honors. (The old conditional — alias into
    /// the SHARED `scope`, only when `lookup_bind` found nothing — was
    /// the #5 soak-jul04 bug: a sibling slot clone's re-minted bind
    /// under the same name made the lookup succeed, the alias was
    /// skipped, and the recompiled body read the sibling's binding. It
    /// also resolved slot feeders by name-lookup luck instead of the
    /// remap.) Internal refs still resolve to their re-minted binds: the
    /// replayed compile registers each internal binder in the child
    /// scope as it goes, shadowing the alias for everything lexically
    /// after it — exactly the original scoping. Known residual: two
    /// same-named external captures with distinct ids (only reachable
    /// via a `use` re-pointing the name mid-subtree) collapse to the
    /// later alias; that limitation predates this fix.
    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        remap: &mut RebindMap,
    ) -> Node<R, E> {
        // Recompile into a fresh child scope: compile MUTATES the env, and
        // this spec was already compiled once into `scope` — a replay there
        // collides on registrations that reject redefinition (`deftype`:
        // a typedef in an HOF callback body panicked every per-slot clone)
        // and stacks shadow binds otherwise. The child scope makes the
        // replay hermetic: internal registrations (and the capture aliases
        // above) land in a scope this clone alone owns (and its delete
        // alone cleans). Internal names are lexically invisible to template
        // siblings, so nothing outside the subtree resolves into the child
        // scope.
        static REBIND_SCOPE: AtomicU32 = AtomicU32::new(0);
        let fresh = Scope {
            lexical: ModPath(
                scope.lexical.append(
                    compact_str::format_compact!(
                        "rb{}",
                        REBIND_SCOPE.fetch_add(1, Ordering::Relaxed)
                    )
                    .as_str(),
                ),
            ),
            dynamic: scope.dynamic.clone(),
        };
        let mut refs = Refs::default();
        self.refs(&mut refs);
        for id in refs.refed.iter() {
            if refs.bound.contains(id) {
                continue;
            }
            let bid = remap.get(id).copied().unwrap_or(*id);
            let name = match ctx.env.by_id.get(&bid) {
                Some(b) => b.name.clone(),
                None => continue,
            };
            ctx.env.alias_variable(&fresh.lexical, &name, bid);
        }
        compiler::compile(
            ctx,
            enumflags2::BitFlags::empty(),
            self.spec().clone(),
            &fresh,
            self.spec().id,
        )
        .expect("clone_rebind: recompile from spec failed")
    }

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
    /// Policy is per-node — that's the point of the design: a
    /// container recurses, `Bind` fuses its value, MapQ fuses its
    /// callback template, and the maximal-region property falls out of
    /// top-down order (the highest subtree whose `try_fuse` succeeds
    /// is spliced and nothing below it is attempted). The default is
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

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved_type: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>>;
}

/// A compile-time check for a `#[..]` attribute. Run once per decorated node
/// after fusion, with `node` the compiled (possibly fused) node of the
/// expression the attribute sits above and `attr` the specific attribute.
/// Returning `Err` turns the attribute into a compile error. Registered on the
/// `ExecCtx` via [`ExecCtx::register_attribute`], mirroring builtin
/// registration, so packages can add their own attributes.
pub type AttributeCheckFn<R, E> = fn(&ExecCtx<R, E>, &Attr, &Node<R, E>) -> Result<()>;

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
        if !ctx.fusion.enabled {
            // Nothing to verify when fusion is off — a `#[native]` requirement
            // is vacuously satisfied (there is no fusion to check against). This
            // lets `#[native]` ride in `run!` fixtures (which run an interp/
            // node-walk mode) and in bench programs executed under `--no-fusion`,
            // rather than being a hard error outside a fusion-on run.
            return Ok(());
        }
        // The node survived fusion as node-walk residue. Surface the REAL
        // blockers, filtering out attempt-then-recurse protocol noise:
        // `try_fuse` records a `failed` entry for EVERY region root it tries,
        // including a `let`/block whose own region attempt fails ("node does
        // not emit CLIF") but whose VALUE fused in a child sub-region. Report
        // only the LEAF-most failures whose subtree contains NO fused region —
        // the genuine residue (the call/op that node-walks), not the
        // structural containers above it. Program ExprIds are unique, so this
        // also isolates this expr's blockers from the stdlib baseline.
        let fused: AHashSet<ExprId> =
            ctx.fusion.stats.fused_ids.iter().copied().collect();
        let mut failed: AHashMap<ExprId, CompactString> = AHashMap::default();
        for (id, reason) in ctx.fusion.stats.failed.iter() {
            failed.entry(*id).or_insert_with(|| reason.clone());
        }
        let mut report: Vec<ExprId> = Vec::new();
        node.spec().fold((), &mut |(), e| {
            if failed.contains_key(&e.id) {
                let subtree_fused =
                    e.fold(false, &mut |acc, x| acc || fused.contains(&x.id));
                let has_failed_desc = e.fold(false, &mut |acc, x| {
                    acc || (x.id != e.id && failed.contains_key(&x.id))
                });
                if !subtree_fused && !has_failed_desc {
                    report.push(e.id);
                }
            }
        });
        let mut reasons = CompactString::new("");
        for id in &report {
            use std::fmt::Write;
            let _ = write!(reasons, "\n  - {}", failed[id]);
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

    /// Subscribe to the specified netidx path
    ///
    /// When the subscription updates you are expected to deliver
    /// Netidx events to the expression specified by ref_by.
    fn subscribe(&mut self, flags: UpdatesFlags, path: Path, ref_by: ExprId) -> Dval;

    /// Called when a subscription is no longer needed
    fn unsubscribe(&mut self, path: Path, dv: Dval, ref_by: ExprId);

    /// List the netidx path, return Value::Null if the path did not
    /// change. When the path did update you should send the output
    /// back as a properly formatted struct with two fields, rows and
    /// columns both containing string arrays.
    fn list(&mut self, id: BindId, path: Path);

    /// List the table at path, return Value::Null if the path did not
    /// change
    fn list_table(&mut self, id: BindId, path: Path);

    /// list or table will no longer be called on this BindId, and
    /// related resources can be cleaned up.
    fn stop_list(&mut self, id: BindId);

    /// Publish the specified value, returning it's Id, which must be
    /// used to update the value and unpublish it. If the path is
    /// already published, return an error.
    fn publish(&mut self, path: Path, value: Value, ref_by: ExprId) -> Result<Val>;

    /// Update the specified value
    fn update(&mut self, id: &Val, value: Value);

    /// Stop publishing the specified id
    fn unpublish(&mut self, id: Val, ref_by: ExprId);

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

    /// Notify the RT that a top level variable has been set internally
    ///
    /// This is called when the compiler has determined that it's safe to set a
    /// variable without waiting a cycle. When the updated variable is a
    /// toplevel node this method is called to notify the runtime that needs to
    /// update any dependent toplevel nodes.
    fn notify_set(&mut self, id: BindId);

    /// This must return results from the same path in the call order.
    ///
    /// when the rpc returns you are expected to deliver a Variable
    /// event with the specified id to the expression specified by
    /// ref_by.
    fn call_rpc(&mut self, name: Path, args: Vec<(ArcStr, Value)>, id: BindId);

    /// Publish an rpc at the specified path with the specified
    /// procedure level doc and arg spec.
    ///
    /// When the RPC is called the rpc table in event will be
    /// populated under the specified bind id.
    ///
    /// If the procedure is already published an error will be
    /// returned
    fn publish_rpc(
        &mut self,
        name: Path,
        doc: Value,
        spec: Vec<ArgSpec>,
        id: BindId,
    ) -> Result<()>;

    /// unpublish the rpc identified by the bind id.
    fn unpublish_rpc(&mut self, name: Path);

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
    /// The self-call's argument values, in callee-formal order.
    pub(crate) args: smallvec::SmallVec<[Value; 4]>,
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
    /// the last value of every bound variable
    pub cached: IntMap<BindId, Value>,
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
    /// (e.g. inside an HOF callback — #203). Cleared once per batch
    /// alongside `unstable_bindings` (its sibling batch-scoped resolution
    /// state) at each RT batch entry point. BindIds are globally unique,
    /// so accumulating across a batch's statements is collision-free; the
    /// `unstable_bindings` guard still excludes `<-` targets, and the
    /// per-batch clear keeps a `<-`-reassigned lambda in one batch from
    /// resolving stalely in the next.
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
            cached: IntMap::default(),
            rt: user,
            lambda_defs: IntMap::default(),
            bind_to_lambda: IntMap::default(),
            unstable_bindings: nohash::IntSet::default(),
            builtin_bindings: ahash::AHashMap::default(),
            rec_defs: nohash::IntSet::default(),
            fusion: fusion::FusionCtx::new()?,
            pending_tail_call: None,
            control: Arc::new(Control::new()),
            diagnostics: Vec::new(),
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
        match self.builtins.entry(T::NAME) {
            Entry::Vacant(e) => {
                e.insert(T::init);
            }
            Entry::Occupied(_) => bail!("builtin {} is already registered", T::NAME),
        }
        self.fusion.builtin_effects.insert(T::NAME, T::EFFECT);
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
    /// to run each known attribute's post-fusion check.
    pub fn lookup_attribute(&self, name: &str) -> Option<AttributeCheckFn<R, E>> {
        self.attributes.get(name).copied()
    }

    /// Look up the sync/async effect of a registered builtin. Returns
    /// `EffectKind::Async` (the conservative default) for unknown names
    /// so callers don't need to handle the "missing" case specially.
    pub fn builtin_effect(&self, name: &str) -> EffectKind {
        self.fusion.builtin_effects.get(name).copied().unwrap_or_default()
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

    /// Built in functions should call this when variables are set
    /// unless they are sure the variable does not need to be
    /// cached. This will also call the user ctx set_var.
    pub fn set_var(&mut self, id: BindId, v: Value) {
        self.cached.insert(id, v.clone());
        self.rt.set_var(id, v)
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

    pub fn root() -> Self {
        Self { lexical: ModPath::root(), dynamic: ModPath::root() }
    }
}

/// compile the expression into a node graph in the specified context
/// and scope, return the root node or an error if compilation failed.
pub fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<Node<R, E>> {
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
    let top_id = spec.id;
    ctx.fusion.top_id = Some(top_id);
    let env = ctx.env.clone();
    let st = Instant::now();
    let mut node = match compiler::compile(ctx, flags, spec, scope, top_id) {
        Ok(n) => n,
        Err(e) => {
            ctx.env = env;
            return Err(e);
        }
    };
    info!("compile time {:?}", st.elapsed());
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
        if let Err(e) = fusion::fuse(&mut node, ctx) {
            ctx.env = env;
            return Err(e);
        }
        info!("fusion time {:?}", st.elapsed());
    }
    // Run each registered attribute's post-fusion check over the final graph.
    // Runs unconditionally (not gated on `fusion.enabled`) so a `#[native]`
    // program under `--no-fusion` errors clearly rather than silently passing.
    if let Err(e) = check_attributes(&node, ctx) {
        ctx.env = env;
        return Err(e);
    }
    Ok(node)
}

/// Walk the post-fusion graph and run each registered attribute's check on the
/// node of every decorated expression. `for_each_node` visits every node-walk
/// survivor and every top-level `FusedKernel`, but stops at kernel interiors
/// and lambda bodies — so a decorated node ABSORBED into a larger fused region
/// is (correctly) never re-checked (it's native). HOF callback bodies are NOT
/// absorbed into the visible graph (they fuse into a per-element template held
/// by the HOF builtin), so [`check_decorated_node`] explicitly descends into
/// them — otherwise `#[native]` inside `array::map(a, |x| ...)` would pass
/// vacuously. Returns the first error.
fn check_attributes<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    let mut err: Option<anyhow::Error> = None;
    fusion::for_each_node(node, &mut |n| {
        if err.is_none() {
            if let Err(e) = check_decorated_node(n, ctx) {
                err = Some(e);
            }
        }
    });
    err.map_or(Ok(()), Err)
}

/// Run every registered attribute's check on `n`'s own decorations, then
/// DESCEND into any HOF callback bodies the node's callee fused (a HOF
/// builtin exposes its fused per-element template body via
/// [`Apply::for_each_hof_fused_body`]). The main `for_each_node` walk stops at
/// lambda bodies, so a decorated expression inside an HOF callback is
/// otherwise never visited. The fused template's body carries `FusedKernel`s
/// for the sub-regions that fused and node-walk residue (recorded in
/// `FusionStats`) for those that didn't — exactly what each attribute's
/// per-node verdict needs. Recurses through nested HOFs.
fn check_decorated_node<R: Rt, E: UserEvent>(
    n: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Result<()> {
    if let Some(dec) = &n.spec().dec {
        for attr in dec.attrs.iter() {
            if let Some(check) = ctx.lookup_attribute(&attr.name) {
                check(ctx, attr, n)?;
            }
        }
    }
    if let NodeView::CallSite(cs) = n.view() {
        if let Some(apply) = cs.callee_apply() {
            let mut bodies: Vec<&Node<R, E>> = Vec::new();
            apply.for_each_hof_fused_body(&mut |body| bodies.push(body));
            for body in bodies {
                let mut err: Option<anyhow::Error> = None;
                fusion::for_each_node(body, &mut |bn| {
                    if err.is_none() {
                        if let Err(e) = check_decorated_node(bn, ctx) {
                            err = Some(e);
                        }
                    }
                });
                if let Some(e) = err {
                    return Err(e);
                }
            }
        }
    }
    Ok(())
}
