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
use anyhow::{bail, Result};
use arcstr::ArcStr;
use enumflags2::bitflags;
pub use enumflags2::BitFlags;
use expr::Expr;
use futures::channel::mpsc;
use log::info;
use netidx::{
    path::Path,
    publisher::{Id, Val, WriteRequest},
    subscriber::{self, Dval, SubId, UpdatesFlags, Value},
};
use netidx_protocols::rpc::server::{ArgSpec, RpcCall};
use netidx_value::{abstract_type::AbstractWrapper, Abstract};
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
        self,
        atomic::{AtomicBool, AtomicU32, Ordering},
        LazyLock,
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

/// Lock-free [`CtlFlag`] set over an `AtomicU32`. A loop polls
/// [`Control::interrupted`] (any flag ⇒ abort); the run loop polls
/// [`Control::aborted`] (Abort ⇒ shut down). See [`CtlFlag`].
#[derive(Debug)]
pub struct Control(AtomicU32);

impl Default for Control {
    fn default() -> Self {
        Self::new()
    }
}

impl Control {
    pub fn new() -> Self {
        Control(AtomicU32::new(0))
    }

    /// Request that in-flight loops abort this cycle; the runtime keeps
    /// running and `do_cycle` clears this at the end of the cycle.
    pub fn interrupt(&self) {
        self.0.fetch_or(CtlFlag::Interrupt as u32, Ordering::Release);
    }

    /// Request shutdown: in-flight loops abort AND the run loop returns
    /// before the next cycle. Sticky — never cleared.
    pub fn abort(&self) {
        self.0.fetch_or(CtlFlag::Abort as u32, Ordering::Release);
    }

    /// True if any control flag is set — the signal a loop polls to
    /// decide whether to abort (both flags break loops).
    pub fn interrupted(&self) -> bool {
        self.0.load(Ordering::Acquire) != 0
    }

    /// True if `Abort` is set — the signal the run loop polls before each
    /// cycle to decide whether to shut down.
    pub fn aborted(&self) -> bool {
        self.0.load(Ordering::Acquire) & (CtlFlag::Abort as u32) != 0
    }

    /// Clear the `Interrupt` bit, leaving `Abort` sticky. Called by
    /// `do_cycle` at the end of each cycle so a one-shot `interrupt()`
    /// doesn't persist into the next cycle.
    pub fn clear_interrupt(&self) {
        self.0.fetch_and(!(CtlFlag::Interrupt as u32), Ordering::Release);
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
        if $crate::trace() {
            dbg!($e)
        } else {
            $e
        }
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
    fn for_each_hof_callback_body<'a>(
        &'a self,
        _f: &mut dyn FnMut(&'a Node<R, E>),
    ) {
    }

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
        // CR estokes: Why are we passing this scope? We shouldn't be
        // reconstructing scope multiple times
        _scope: &Scope,
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
    /// state via their own `clone_rebind`. The re-binding rides the
    /// env's scoped name map exactly as `compile` does (binds register
    /// fresh names, refs resolve by name) — no side remap table.
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
    ///
    /// **Capture-safe.** `compile` re-resolves every `Ref` by name, but a
    /// CAPTURE was resolved in an *outer* scope not lexically visible from
    /// `scope`, so a naive recompile would fail ("not defined"). So first
    /// alias each *unresolved* external ref's name → its original id into
    /// `scope`; the recompile then resolves it back to the same outer
    /// binding. Internal refs re-minted into `scope` already resolve and
    /// are left alone (the alias would wrongly shadow the fresh id).
    fn clone_rebind(&self, ctx: &mut ExecCtx<R, E>, scope: &Scope) -> Node<R, E> {
        let mut refs = Refs::default();
        self.refs(&mut refs);
        for id in refs.refed.iter() {
            if refs.bound.contains(id) {
                continue;
            }
            let bid = *id;
            let name = match ctx.env.by_id.get(&bid) {
                Some(b) => b.name.clone(),
                None => continue,
            };
            let mp = ModPath::from_iter([name.as_str()]);
            if ctx.env.lookup_bind(&scope.lexical, &mp).is_none() {
                ctx.env.alias_variable(&scope.lexical, &name, bid);
            }
        }
        compiler::compile(
            ctx,
            enumflags2::BitFlags::empty(),
            self.spec().clone(),
            scope,
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
    /// `Ok(None)` with no recursion — correct for leaves; container
    /// nodes override.
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
    /// `BindId → LambdaDef Value` for every lambda binding in the program
    /// currently compiling. Populated during `typecheck0` (each
    /// `Bind::typecheck0` records its own binding, `Module::typecheck0`
    /// adds the signature→impl proxy entries) so it is globally complete
    /// before `typecheck1` runs the static-resolution it feeds: a
    /// `CallSite` whose `fnode` is a `Ref` looks its `BindId` up here to
    /// pre-bind to a known lambda. This is a compile-time ANALYSIS map,
    /// deliberately kept out of `cached` (runtime state) — see
    /// `CallSite::typecheck1`. Cleared at the top of every `compile`.
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
        Ok(Self {
            lambdawrap: Abstract::register(id)?,
            env: Env::default(),
            builtins: AHashMap::default(),
            builtins_allowed: true,
            libstate: LibState::default(),
            tags: AHashSet::default(),
            cached: IntMap::default(),
            rt: user,
            lambda_defs: IntMap::default(),
            bind_to_lambda: IntMap::default(),
            unstable_bindings: nohash::IntSet::default(),
            builtin_bindings: ahash::AHashMap::default(),
            fusion: fusion::FusionCtx::new()?,
            pending_tail_call: None,
            control: Arc::new(Control::new()),
        })
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
    ctx.bind_to_lambda.clear();
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
    Ok(node)
}
