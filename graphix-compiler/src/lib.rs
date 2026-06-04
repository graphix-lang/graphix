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

pub mod effects;
pub mod env;
pub mod expr;
pub mod fusion;
pub mod gir;
pub mod gir_interp;
pub mod gir_jit;
pub mod gir_jit_helpers;
pub mod gir_jit_intern;
pub mod node;
pub mod node_shape;
pub mod static_resolve;
pub mod typ;

use crate::{
    effects::EffectKind,
    env::Env,
    expr::{ExprId, ModPath},
    node::lambda::LambdaDef,
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
        atomic::{AtomicBool, Ordering},
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
    /// Skip the fusion phase entirely. `compile()` runs build +
    /// typecheck and returns the regular Node graph; no kernels
    /// are built, no splicing happens. Used by the test harness's
    /// `interp` mode as ground truth — the program executes
    /// purely through the Update-trait node-graph interpreter.
    FusionDisabled,
    /// Run the fusion phase but skip JIT-compilation. Kernels are
    /// still built (GIR emission, splicing) and `FusedKernel`
    /// dispatches via [`crate::gir_interp`] instead of native
    /// code. Used by the test harness's `fused` mode to validate
    /// GIR translation independently of the JIT backend.
    JitDisabled,
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

/// A textual occurrence of a name at a specific source position that
/// the compiler resolved to a particular `BindId`. Populated as a side
/// effect of compilation so IDE tooling can answer
/// `textDocument/references` and `textDocument/definition` without
/// re-implementing name resolution.
///
/// `def_pos` and `def_ori` mirror the bind's declaration site at
/// resolution time. They're captured here because some bindings
/// (notably lambda parameters) are unbound from the env when the
/// callsite that created them is dropped — but their declaration
/// site is still meaningful to the user.
#[derive(Debug, Clone)]
pub struct ReferenceSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub name: expr::ModPath,
    pub bind_id: BindId,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<expr::Origin>,
}

/// A textual occurrence of a module reference (either `use foo;` or
/// `mod foo;`). For the `mod foo;` case `def_ori` points at the file
/// the module's body was loaded from — that's the natural target for
/// go-to-definition on a module name.
#[derive(Debug, Clone)]
pub struct ModuleRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    /// Module name as the user wrote it (might be relative).
    pub name: expr::ModPath,
    /// Absolute module path the compiler resolved this reference to.
    pub canonical: expr::ModPath,
    /// Origin of the module's body (the file it was loaded from)
    /// when this site is itself a declaration that pulled the
    /// module in. `None` for plain `use` sites.
    pub def_ori: Option<Arc<expr::Origin>>,
}

/// One entry in the per-compile scope map: the compiler descended
/// into an `Expr` at this `(pos, ori)` while in this `scope`. IDE
/// tooling answers `cursor → scope` by finding the entry with the
/// greatest `pos` ≤ the cursor in the same file.
#[derive(Debug, Clone)]
pub struct ScopeMapEntry {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub scope: Scope,
}

/// Metadata captured for every `let foo = |...| 'builtin_name`
/// binding. Stored on [`ExecCtx::builtin_bindings`] keyed by the
/// binding's [`BindId`] so the fusion pass can lower `Apply` sites
/// targeting this binding into a [`crate::gir::FnSource::Builtin`]
/// slot without round-tripping through the runtime's `LambdaDef`
/// value.
///
/// `name` is the canonical builtin identifier (e.g. `core_once`) —
/// matches the key used by [`ExecCtx::register_builtin`] and
/// [`ExecCtx::builtins`].
///
/// `argspec` is the original source-level argument list (including
/// each labeled arg's default expression, if any), needed to
/// construct the per-formal-arg [`crate::gir::BuiltinSlot`]
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
    /// Used by `GirNode::pre_bind_builtin` to look up the lambda's
    /// env+scope when compiling a `BuiltinSlot::LabeledDefault`
    /// expression — defaults may reference free variables visible
    /// only in the lambda's original definition scope.
    pub lambda_id: Option<LambdaId>,
}

/// A textual occurrence of a type reference (e.g. `Foo` in `let x: Foo`).
/// Captured by the compiler when a `Type::Ref` carrying parse-time
/// position info gets dereferenced. `def_pos`/`def_ori` point at the
/// `type Foo = …` declaration site so go-to-def on a type name lands
/// on the typedef.
#[derive(Debug, Clone)]
pub struct TypeRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    /// The name as written in source (e.g. `Result`, `array::Foo`).
    pub name: expr::ModPath,
    /// Canonical scope of the typedef the reference resolved to.
    pub canonical_scope: expr::ModPath,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<expr::Origin>,
}

/// Maps a `val foo: T` declaration in a `.gxi` interface to its
/// `let foo = …` implementation site in the paired `.gx`. Populated by
/// `check_sig` whenever it matches a sig proxy bind to its impl bind.
/// Used by IDE tooling to (a) goto-def from a sig val site to the impl,
/// and (b) union find-references results across both `BindId`s.
/// Only populated when `env.lsp_mode` is set.
#[derive(Debug, Clone)]
pub struct SigImplLink {
    pub scope: expr::ModPath,
    pub name: compact_str::CompactString,
    pub sig_id: BindId,
    pub impl_id: BindId,
}

/// Per-module snapshot of the *internal* env (the impl's view, where
/// implementation bindings shadow sig proxies). The CheckResult's
/// top-level `env` is the *external* view across the project; this
/// per-module entry lets IDE queries on names inside a module reach
/// the impl bind metadata. Only populated when `env.lsp_mode` is set.
#[derive(Debug, Clone)]
pub struct ModuleInternalView {
    pub scope: expr::ModPath,
    pub env: env::Env,
}

/// Pools backing the IDE side-channel collections. `GPooled` so the
/// buffers can return to the same pool after crossing the
/// runtime-task → LSP-thread boundary as part of `CheckResult`. Sized
/// generously since the LSP recompiles on every keystroke and these
/// can grow into the tens of thousands of entries on large modules.
pub static REFERENCE_SITE_POOL: LazyLock<Pool<Vec<ReferenceSite>>> =
    LazyLock::new(|| Pool::new(64, 65536));
pub static MODULE_REF_SITE_POOL: LazyLock<Pool<Vec<ModuleRefSite>>> =
    LazyLock::new(|| Pool::new(64, 65536));
pub static SCOPE_MAP_ENTRY_POOL: LazyLock<Pool<Vec<ScopeMapEntry>>> =
    LazyLock::new(|| Pool::new(64, 65536));
// `TYPE_REF_SITE_POOL`, `SIG_LINK_POOL`, and `MODULE_INTERNAL_VIEW_POOL`
// back the per-check `Lsp` sinks; they live in `env` next to the
// `Lsp` struct that consumes them.

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
}

pub type Node<R, E> = Box<dyn Update<R, E>>;

/// Phase indicator for Apply::typecheck
#[derive(Debug)]
pub enum TypecheckPhase<'a> {
    /// During Lambda::typecheck — faux args, building FnType
    Lambda,
    /// During deferred check or bind — resolved FnType available
    CallSite(&'a FnType),
}

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
    /// fusible builtins override to `FusedBuiltin(self)`,
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

    /// apply custom typechecking. Phase indicates context:
    /// - Lambda: during lambda body checking (faux args). Return NeedsCallSite
    ///   to opt in to deferred call-site type checking.
    /// - CallSite: during deferred check or bind (resolved FnType available)
    fn typecheck(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        Ok(())
    }

    /// Compile-time hook for HOF builtins. Called by
    /// [`crate::static_resolve::resolve_static_calls`] after this
    /// builtin has been constructed by its `BuiltIn::init`, with
    /// `fn_args` listing the positional indices and `LambdaDef`s of
    /// any fn-typed args at the call site that the compiler proved
    /// resolve statically to a single known lambda. The builtin can
    /// use these to pre-materialize internal `Apply` Nodes (e.g.
    /// `MapQ` synthesizes its callback `Apply` via `genn::apply`)
    /// so fusion's walker can later descend into the callback body
    /// through normal `CallSite`-resolved machinery — no LambdaDef
    /// poking needed at fusion time.
    ///
    /// Default no-op. Builtins that don't take fn-typed args, or
    /// don't fuse their callbacks, can ignore this entirely.
    ///
    /// **Important — analysis only.** Anything constructed here must
    /// not change the builtin's runtime behavior. For example,
    /// `MapQ` keeps `update()` as it was (per-array-element fresh
    /// `Apply` Nodes — preserving per-slot state for async
    /// callbacks); the pre-materialized Apply is consulted only by
    /// `emit_gir`, and only when the callback is fully sync.
    fn static_resolve_fn_args(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
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

    /// put the node to sleep, used in conditions like select for branches that
    /// are not selected. Any cached values should be cleared on sleep.
    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>);
}

/// Typed view of an [`Apply`] for fusion / analysis layer code,
/// symmetric to [`NodeView`].
///
/// Variants:
/// - [`Lambda`](ApplyView::Lambda) — a graphix-language lambda with
///   a walkable [`Node`] body. Fusion's walker descends into the
///   body via [`crate::node::lambda::GXLambda::body`].
/// - [`FusedBuiltin`](ApplyView::FusedBuiltin) — a builtin that
///   knows how to lower itself to GIR via the [`GirEmitter`] trait.
///   The package owning the builtin implements both `Apply` and
///   `GirEmitter` on the same type and overrides `view()` to return
///   this variant.
/// - [`BuiltIn`](ApplyView::BuiltIn) — opaque builtin. Fusion emits
///   a runtime `DynCall`; no introspection beyond the trait
///   methods. The default `view()` returns this — every Apply impl
///   inherits opaque-builtin semantics unless it overrides.
pub enum ApplyView<'a, R: Rt, E: UserEvent> {
    Lambda(&'a crate::node::lambda::GXLambda<R, E>),
    FusedBuiltin(&'a dyn GirEmitter<R, E>),
    BuiltIn,
}

/// Mutable counterpart to [`ApplyView`]. Used by fusion for splicing
/// sub-kernels into Nodes reachable through an Apply (the body Node
/// of a [`Lambda`](ApplyViewMut::Lambda)).
pub enum ApplyViewMut<'a, R: Rt, E: UserEvent> {
    Lambda(&'a mut crate::node::lambda::GXLambda<R, E>),
    FusedBuiltin(&'a mut dyn GirEmitter<R, E>),
    BuiltIn,
}

/// Opt-in fusion trait. An Apply implementor that knows how to lower
/// itself to GIR implements this trait and returns
/// [`ApplyView::FusedBuiltin(self)`](ApplyView::FusedBuiltin) from
/// `Apply::view()`.
///
/// `emit_gir` returns `None` if the call-site shape doesn't match
/// what the emitter expects (e.g. a HOF callback isn't an inline
/// lambda) — fusion falls back to `DynCall` in that case, never an
/// error.
pub trait GirEmitter<R: Rt, E: UserEvent>: Send + Sync {
    fn emit_gir(
        &self,
        callsite: &crate::node::callsite::CallSite<R, E>,
        args: &[(Option<ArcStr>, &Node<R, E>)],
        arg_refs: &[Node<R, E>],
        ctx: &mut crate::fusion::lowering::FusionCtx,
        ec: &mut ExecCtx<R, E>,
    ) -> Option<crate::gir::GirExpr>;
}

/// One entry in the `fn_args` slice passed to
/// [`Apply::static_resolve_fn_args`]. Records that the call site's
/// positional arg at `arg_idx` is fn-typed AND the compiler proved
/// it resolves statically to a single known `LambdaDef`.
///
/// The `LambdaDef` borrow is tied to the `static_resolve_calls`
/// pass's lifetime; the builtin should consume what it needs from
/// `lambda` synchronously (e.g. clone its `Arc<FnType>`,
/// instantiate an internal Apply via `genn::apply` referencing
/// `lambda.id`) and not store the reference.
pub struct StaticFnArg<'a, R: Rt, E: UserEvent> {
    pub arg_idx: usize,
    pub lambda: &'a crate::node::lambda::LambdaDef<R, E>,
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
    Bind(&'a crate::node::bind::Bind<R, E>),
    Lambda(&'a crate::node::lambda::Lambda),
    Block(&'a crate::node::Block<R, E>),
    Module(&'a crate::node::module::Module<R, E>),
    // Child-bearing non-container
    CallSite(&'a crate::node::callsite::CallSite<R, E>),
    Select(&'a crate::node::select::Select<R, E>),
    TryCatch(&'a crate::node::error::TryCatch<R, E>),
    Qop(&'a crate::node::error::Qop<R, E>),
    OrNever(&'a crate::node::error::OrNever<R, E>),
    ExplicitParens(&'a crate::node::ExplicitParens<R, E>),
    TypeCast(&'a crate::node::TypeCast<R, E>),
    Connect(&'a crate::node::Connect<R, E>),
    ConnectDeref(&'a crate::node::ConnectDeref<R, E>),
    StringInterpolate(&'a crate::node::StringInterpolate<R, E>),
    Any(&'a crate::node::Any<R, E>),
    Sample(&'a crate::node::Sample<R, E>),
    // Producers
    Struct(&'a crate::node::data::Struct<R, E>),
    StructWith(&'a crate::node::data::StructWith<R, E>),
    Tuple(&'a crate::node::data::Tuple<R, E>),
    Variant(&'a crate::node::data::Variant<R, E>),
    Array(&'a crate::node::array::Array<R, E>),
    Map(&'a crate::node::map::Map<R, E>),
    // Accessors
    StructRef(&'a crate::node::data::StructRef<R, E>),
    TupleRef(&'a crate::node::data::TupleRef<R, E>),
    ArrayRef(&'a crate::node::array::ArrayRef<R, E>),
    ArraySlice(&'a crate::node::array::ArraySlice<R, E>),
    MapRef(&'a crate::node::map::MapRef<R, E>),
    // Binding access
    Ref(&'a crate::node::bind::Ref),
    ByRef(&'a crate::node::bind::ByRef<R, E>),
    Deref(&'a crate::node::bind::Deref<R, E>),
    // Arithmetic — one variant per macro-generated struct in node/op.rs
    Add(&'a crate::node::op::Add<R, E>),
    Sub(&'a crate::node::op::Sub<R, E>),
    Mul(&'a crate::node::op::Mul<R, E>),
    Div(&'a crate::node::op::Div<R, E>),
    Mod(&'a crate::node::op::Mod<R, E>),
    CheckedAdd(&'a crate::node::op::CheckedAdd<R, E>),
    CheckedSub(&'a crate::node::op::CheckedSub<R, E>),
    CheckedMul(&'a crate::node::op::CheckedMul<R, E>),
    CheckedDiv(&'a crate::node::op::CheckedDiv<R, E>),
    CheckedMod(&'a crate::node::op::CheckedMod<R, E>),
    // Comparison + logical
    Eq(&'a crate::node::op::Eq<R, E>),
    Ne(&'a crate::node::op::Ne<R, E>),
    Lt(&'a crate::node::op::Lt<R, E>),
    Gt(&'a crate::node::op::Gt<R, E>),
    Lte(&'a crate::node::op::Lte<R, E>),
    Gte(&'a crate::node::op::Gte<R, E>),
    And(&'a crate::node::op::And<R, E>),
    Or(&'a crate::node::op::Or<R, E>),
    Not(&'a crate::node::op::Not<R, E>),
    // Leaves and declarations
    Constant(&'a crate::node::Constant),
    Use(&'a crate::node::Use),
    TypeDef(&'a crate::node::TypeDef),
    Nop(&'a crate::node::Nop),
    // Synthetic — produced by fusion itself.
    FusedKernel(&'a crate::fusion::FusedKernel<R, E>),
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

    /// type check the node and it's children. The default impl runs
    /// the node-specific `typecheck_inner` and, on success, propagates
    /// the resolved `typ()` into the source `Expr`'s `typ` cell — so
    /// after a successful typecheck pass every Expr in the program
    /// reports its resolved Type via `Expr::typ.get()`. Don't override
    /// unless you're sure you want to skip propagation (which is
    /// almost certainly wrong — fusion and other AST consumers depend
    /// on this).
    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.typecheck_inner(ctx)?;
        let _ = self.spec().typ.set(self.typ().clone());
        Ok(())
    }

    /// node-specific typecheck logic — see [`Self::typecheck`].
    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()>;

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

    /// Find the descendant Node whose `spec().id == target` and
    /// replace it with `replacement`. Used by the fusion splice
    /// phase to swap a subtree for its kernel-backed equivalent.
    ///
    /// - `Ok(old_node)` — match found and replaced. The caller is
    ///   responsible for deleting `old_node` from the `ExecCtx`
    ///   (calling `.delete(ctx)`) before dropping.
    /// - `Err(replacement)` — no match in this subtree. The
    ///   `replacement` is returned unchanged so the caller can try
    ///   elsewhere.
    ///
    /// The default impl treats `self` as a leaf (no children to
    /// descend) and always returns `Err(replacement)`. Container
    /// nodes (`Block`, `Module`, `Bind`) override to iterate
    /// children and forward the search.
    ///
    /// `self.spec().id == target` is **not** checked here — that's
    /// the caller's responsibility (handled at the top of
    /// `fusion::builder::splice_into`). This method only descends
    /// into children.
    fn splice_child(
        &mut self,
        _target: expr::ExprId,
        replacement: Node<R, E>,
    ) -> std::result::Result<Node<R, E>, Node<R, E>> {
        Err(replacement)
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
    /// Does this builtin need a 2nd typecheck pass? If yes then typecheck will
    /// be called a second time after all types are resolved and may examine
    /// them and have a second chance to reject the program. For example
    /// - type directed deserialization functions need the final deserialization
    ///   type to decide if it's valid, and to build their schema.
    /// - type requirements not expressable in the grammar. For example this
    ///   argument must be some kind of struct, and the fields and types must
    ///   correspond to some other struct (e.g. publish rpc)
    const NEEDS_CALLSITE: bool;
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

pub struct ExecCtx<R: Rt, E: UserEvent> {
    // used to wrap lambdas into an abstract netidx value type
    lambdawrap: AbstractWrapper<LambdaDef<R, E>>,
    // all registered built-in functions
    builtins: AHashMap<&'static str, (BuiltInInitFn<R, E>, bool)>,
    /// Sync/async effect of each registered builtin, keyed by name.
    /// Populated by `register_builtin` from `T::EFFECT`. Read by
    /// fusion's effect inference (M6) to decide whether a builtin
    /// call site can be absorbed into a sync kernel. Builtins absent
    /// from this map are treated as `Async` (the conservative
    /// default), which is always correct.
    pub builtin_effects: AHashMap<&'static str, EffectKind>,
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
    /// LambdaDefs indexed by LambdaId, for deferred type checking
    pub lambda_defs: IntMap<LambdaId, Value>,
    /// deferred type check closures, evaluated after all primary type checking
    pub deferred_checks:
        Vec<Box<dyn FnOnce(&mut ExecCtx<R, E>) -> Result<()> + Send + Sync>>,
    /// Reference sites accumulated during compilation. Each is a
    /// textual occurrence of a name and the `BindId` it resolved to.
    /// At compile boundaries, swap with a fresh `REFERENCE_SITE_POOL`
    /// container via `mem::replace` (not `mem::take` — `Default`
    /// routes to the unsized thread-local registry, not our named
    /// pool). Only populated when `env.lsp_mode` is set.
    pub references: GPooled<Vec<ReferenceSite>>,
    /// Module reference sites — `use foo;` and `mod foo;` mentions.
    /// Same scoping rules as `references`.
    pub module_references: GPooled<Vec<ModuleRefSite>>,
    /// Per-compile scope map. `compile()` pushes one entry every
    /// time it's invoked, recording the scope it was called with.
    /// IDE tooling reads this to answer `cursor → scope` queries.
    pub scope_map: GPooled<Vec<ScopeMapEntry>>,
    /// BindIds of bindings that are the target of a `<-` (Connect)
    /// somewhere in the program. Populated lazily by
    /// [`crate::node::Connect::compile`] — every Connect resolves
    /// its target name to a BindId via `env.lookup_bind`, and that
    /// BindId is recorded here. Fusion's stability checks consult
    /// this set to determine whether a Bind's value can be safely
    /// fused (a `<-` target rebinds at runtime, so a static splice
    /// into native code would dispatch into stale state).
    pub unstable_bindings: nohash::IntSet<BindId>,
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
    /// a `GirOp::DynCall` against a
    /// [`crate::gir::FnSource::Builtin`] slot: the canonical
    /// builtin `name` (matches `ctx.builtins`), the source-level
    /// `argspec` (with default expressions for labeled args), and
    /// the resolved `FnType`.
    ///
    /// Populated by [`crate::node::bind::Bind::compile`] when the
    /// value being bound is an `ExprKind::Lambda` with
    /// `body == Either::Right(name)`. User-lambda bindings leave
    /// no entry here; only builtin lambdas appear.
    pub builtin_bindings:
        ahash::AHashMap<(expr::ModPath, compact_str::CompactString), BuiltinBindInfo>,
    /// Per-context JIT state — cranelift module + cross-kernel-call
    /// cache. Lives here so each `ExecCtx` instance has its own
    /// isolated JIT compile target (no shared global module, no
    /// races across concurrent in-process runtimes).
    ///
    /// Wrapped in a `parking_lot::Mutex` solely to satisfy the
    /// `Sync` bound the graphix-rt async machinery puts on
    /// `ExecCtx` (the underlying cranelift `JITModule` uses
    /// `RefCell` internally so isn't auto-`Sync`). Not an
    /// `Arc<Mutex>` — there's no shared ownership; the Mutex is
    /// just interior mutability. Access is exclusively through
    /// `ctx.jit.lock()` followed by the usual `&mut Jit` API.
    /// JIT operations are compile-time only (rare, never on hot
    /// paths), so the lock cost is negligible.
    ///
    /// Kernels with `GirOp::Call` compile into this module via
    /// [`crate::gir_jit::compile_kernel_with_callees`]; kernels
    /// without any calls use the single-kernel path
    /// [`crate::gir_jit::compile_kernel_with_wrapper`] which creates
    /// its own private `JitCtx` per call (no interaction with this
    /// field). The async-JIT worker thread (`JIT_WORKER`) likewise
    /// uses the single-kernel path and doesn't touch this field.
    pub jit: parking_lot::Mutex<gir_jit::Jit>,
    /// On-demand monomorphized lambda-kernel cache, keyed by
    /// `(LambdaId, Arc<FnType>)`. Populated by fusion's emit_node
    /// when it encounters an `ApplyView::Lambda` call site: build the
    /// kernel once, reuse it for every subsequent call to the same
    /// (lambda definition, monomorphization). Cross-kernel
    /// `GirOp::Call` sites resolve against this map at splice time
    /// (interp) and JIT-compile time (Phase D).
    pub fusion_kernels:
        parking_lot::Mutex<std::collections::BTreeMap<
            (LambdaId, std::sync::Arc<typ::FnType>),
            fusion::lowering::CachedKernel,
        >>,
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
            builtin_effects: AHashMap::default(),
            builtins_allowed: true,
            libstate: LibState::default(),
            tags: AHashSet::default(),
            cached: IntMap::default(),
            rt: user,
            lambda_defs: IntMap::default(),
            deferred_checks: Vec::new(),
            references: REFERENCE_SITE_POOL.take(),
            module_references: MODULE_REF_SITE_POOL.take(),
            scope_map: SCOPE_MAP_ENTRY_POOL.take(),
            unstable_bindings: nohash::IntSet::default(),
            builtin_bindings: ahash::AHashMap::default(),
            jit: parking_lot::Mutex::new(gir_jit::Jit::new()?,),
            fusion_kernels: parking_lot::Mutex::new(std::collections::BTreeMap::new()),
        })
    }

    pub fn register_builtin<T: BuiltIn<R, E>>(&mut self) -> Result<()> {
        match self.builtins.entry(T::NAME) {
            Entry::Vacant(e) => {
                e.insert((T::init, T::NEEDS_CALLSITE));
            }
            Entry::Occupied(_) => bail!("builtin {} is already registered", T::NAME),
        }
        self.builtin_effects.insert(T::NAME, T::EFFECT);
        Ok(())
    }

    /// Look up the sync/async effect of a registered builtin. Returns
    /// `EffectKind::Async` (the conservative default) for unknown names
    /// so callers don't need to handle the "missing" case specially.
    pub fn builtin_effect(&self, name: &str) -> EffectKind {
        self.builtin_effects.get(name).copied().unwrap_or_default()
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
    let top_id = spec.id;
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
    if let Err(e) = node.typecheck(ctx) {
        ctx.env = env;
        return Err(e);
    }
    // run deferred builtin type checks after all primary type checking completes
    while let Some(check) = ctx.deferred_checks.pop() {
        if let Err(e) = check(ctx) {
            ctx.env = env;
            return Err(e);
        }
    }
    info!("typecheck time {:?}", st.elapsed());
    // Static call resolution: pre-bind every CallSite whose function
    // expression resolves to a single known LambdaDef. Eliminates
    // the "bind on first use" indirection on every subsequent
    // update, and exposes the lambda body Node directly for
    // fusion's walker to descend into.
    let st = Instant::now();
    if let Err(e) = crate::static_resolve::resolve_static_calls(ctx, &mut node) {
        ctx.env = env;
        return Err(e);
    }
    info!("static_resolve time {:?}", st.elapsed());
    // Fusion phase: walk the typed node graph, build kernels, splice
    // in place. Currently a no-op stub — see fusion/mod.rs::fuse and
    // the implementation plan for the iteration sequence.
    let st = Instant::now();
    if let Err(e) = crate::fusion::fuse(&mut node, ctx, flags) {
        ctx.env = env;
        return Err(e);
    }
    info!("fusion time {:?}", st.elapsed());
    Ok(node)
}
