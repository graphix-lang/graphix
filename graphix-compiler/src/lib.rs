#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
#![recursion_limit = "256"]
#[macro_use]
extern crate combine;
#[macro_use]
extern crate serde_derive;
#[macro_use]
mod ids;

pub mod abstract_value;
pub mod analysis;
pub(crate) mod dbgenv;
pub mod effects;
pub use effects::Effect;
pub mod env;
pub mod expr;
pub mod fusion;
pub mod ide;
pub mod image;
pub mod node;
pub mod node_shape;
pub(crate) mod perfdbg;
pub(crate) mod profile;
pub mod shared_map;
pub(crate) mod stack;

pub use stack::set_stack_budget;
pub use stack::{Control, CtlFlag, InterruptScope};
pub mod tracked;
pub mod tval;
pub mod typ;

use compact_str::CompactString;
use profile::Phase;
// Packages implementing `Apply::emit_clif` take cranelift through the
// compiler, so they stay in version lockstep with the JIT.
pub use cranelift_codegen;
pub use cranelift_frontend;
pub use fusion::FusionStats;
pub use tval::{Tag, TagValue, TagView};

use crate::{
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
use log::{info, warn};
use netidx_value::{Abstract, Value, abstract_type::AbstractWrapper};
use node::compiler;
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::SmallVec;
use std::{
    any::{Any, TypeId},
    cell::Cell,
    collections::hash_map::{self, Entry},
    fmt::Debug,
    mem,
    sync::{
        self, LazyLock,
        atomic::{AtomicU64, Ordering},
    },
    thread::LocalKey,
    time::Duration,
};
use tokio::{task, time::Instant};
use tracked::{TrackedMap, TrackedSet};
use triomphe::Arc;
use uuid::Uuid;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u64)]
pub enum CFlag {
    WarnUnhandled,
    WarnUnused,
    WarningsAreErrors,
    /// Disable fusion: no kernels are built or spliced and the program
    /// runs purely through the node-walk.
    FusionDisabled,
    /// REPL policy: a colliding `use` shadows instead of erroring.
    ReplaceImports,
    /// Print each `seq`'s lowered machine to stdout as it is compiled
    /// (`graphix --expand`): the source position, then the program.
    ExpandSeq,
    /// Stop after the check: typecheck0 and the settle it records, no
    /// elaboration, analysis or fusion. The nodes compiled this way are
    /// only for inspection, never for running.
    CheckOnly,
}

/// Sets a thread-local `Cell` for a scope and puts the previous value
/// back when dropped, by an unwind too.
struct Restore<T: Copy + 'static> {
    key: &'static LocalKey<Cell<T>>,
    prev: T,
}

impl<T: Copy + 'static> Restore<T> {
    fn replace(key: &'static LocalKey<Cell<T>>, v: T) -> Self {
        Self { key, prev: key.replace(v) }
    }
}

impl<T: Copy + 'static> Drop for Restore<T> {
    fn drop(&mut self) {
        self.key.set(self.prev)
    }
}

thread_local! {
    static TRACE: Cell<bool> = const { Cell::new(false) };
}

/// Turn compiler tracing ([`tdbg!`]) on or off on this thread.
pub fn set_trace(b: bool) {
    TRACE.set(b)
}

/// Run `f` with compiler tracing on this thread set to `enable`.
pub fn with_trace<F: FnOnce() -> Result<R>, R>(
    enable: bool,
    spec: &Expr,
    f: F,
) -> Result<R> {
    let restore = Restore::replace(&TRACE, enable);
    if !restore.prev && enable {
        eprintln!("trace enabled at {}, spec: {}", spec.pos, spec);
    } else if restore.prev && !enable {
        eprintln!("trace disabled at {}, spec: {}", spec.pos, spec);
    }
    let r = f();
    if let Err(e) = &r {
        eprintln!("traced at {} failed with {e:?}", spec.pos);
    }
    if restore.prev && !enable {
        eprintln!("trace reenabled")
    }
    r
}

pub fn trace() -> bool {
    TRACE.get()
}

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

image_id!(LambdaId);

image_id!(LambdaInstanceId);

image_id!(BindId);

impl From<u64> for BindId {
    fn from(v: u64) -> Self {
        BindId(v)
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
    /// Print each type variable with its binding or "unbound".
    DerefTVars,
    /// Print core's short names for primitive sets (`Any`, `Number`).
    ReplacePrims,
    /// Print an Origin's location without its source.
    NoSource,
    /// Print an Origin without its parents.
    NoParents,
    /// Print what the author chose where the canonical form differs:
    /// fields and variants in written order, strings between their
    /// delimiters. The formatter's flag. Without it printed text is a
    /// function of the syntax alone, which program-visible text (a cast
    /// error, a null error) has to be: `WrittenAt` and `Expr::str_form`
    /// are not part of a session image, and a type is shared by content,
    /// so whose written order it carries is incidental.
    AsWritten,
}

thread_local! {
    static PRINT_FLAGS: Cell<BitFlags<PrintFlag>> = Cell::new(PrintFlag::ReplacePrims.into());
}

/// Global pool of channel watch batches.
pub static CBATCH_POOL: LazyLock<Pool<Vec<(BindId, Box<dyn CustomBuiltinType>)>>> =
    LazyLock::new(|| Pool::new(10000, 1000));

pub(crate) fn print_as_written() -> bool {
    PRINT_FLAGS.get().contains(PrintFlag::AsWritten)
}

/// Run `f` with the given type-formatting flags on this thread.
pub fn format_with_flags<G: Into<BitFlags<PrintFlag>>, R, F: FnOnce() -> R>(
    flags: G,
    f: F,
) -> R {
    let _restore = Restore::replace(&PRINT_FLAGS, flags.into());
    f()
}

/// Everything that happened simultaneously in one execution cycle. At
/// most one update per variable per cycle; further updates are queued
/// for later cycles.
#[derive(Debug)]
pub struct Event<E: UserEvent> {
    pub init: bool,
    /// Set alongside `init` when the forced init view is a select arm's
    /// wake rather than a birth: a `<-` target that already holds a
    /// value keeps it instead of being reseeded.
    pub wake_init: bool,
    /// The binds a wake republished fired only because the woken arm's
    /// constants fired, not an input.
    pub wake_phantoms: IntSet<BindId>,
    /// The overlay: this cycle's transient deliveries. Not the value
    /// store: reads fall through to [`Rt::store`].
    pub variables: IntMap<BindId, TagValue>,
    pub custom: IntMap<BindId, Box<dyn CustomBuiltinType>>,
    pub user: E,
}

impl<E: UserEvent> Event<E> {
    pub fn new(user: E) -> Self {
        Event {
            init: false,
            wake_init: false,
            wake_phantoms: IntSet::default(),
            variables: IntMap::default(),
            custom: IntMap::default(),
            user,
        }
    }

    pub fn clear(&mut self) {
        let Self { init, wake_init, wake_phantoms, variables, custom, user } = self;
        *init = false;
        *wake_init = false;
        wake_phantoms.clear();
        variables.clear();
        custom.clear();
        user.clear();
    }
}

#[derive(Debug, Clone, Default)]
pub struct Refs {
    refed: LPooled<IntSet<BindId>>,
    /// The refs a fire can reach the collected node through: `refed`
    /// minus those read only under a sample's right side.
    triggering: LPooled<IntSet<BindId>>,
    bound: LPooled<IntSet<BindId>>,
    banked: usize,
    /// Leave callee instance bodies out of the walk.
    skip_callees: bool,
}

impl Refs {
    /// A walk of one body alone: callee instance bodies are left out.
    pub(crate) fn without_callees() -> Self {
        Self { skip_callees: true, ..Self::default() }
    }

    /// Record a read of `id`, triggering unless under a sample's right
    /// side.
    pub(crate) fn read(&mut self, id: BindId) {
        self.refed.insert(id);
        if self.banked == 0 {
            self.triggering.insert(id);
        }
    }
}

pub use combine::stream::position::SourcePosition;

/// Metadata for a `let foo = |...| 'builtin_name` binding
/// ([`ExecCtx::builtin_bindings`]): the canonical builtin `name`, the
/// source-level `argspec` (with labeled defaults), and the binding's
/// declared `typ`.
#[derive(Debug, Clone, netidx_derive::Pack)]
#[pack(unwrapped)]
pub struct BuiltinBindInfo {
    pub name: ArcStr,
    pub argspec: triomphe::Arc<[expr::Arg]>,
    pub typ: triomphe::Arc<typ::FnType>,
    /// The binding's lambda definition; labeled defaults compile in its
    /// env and scope.
    pub lambda_id: Option<LambdaId>,
}

impl Refs {
    pub fn with_external_refs(&self, mut f: impl FnMut(BindId)) {
        for id in &*self.refed {
            if !self.bound.contains(id) {
                f(*id);
            }
        }
    }

    /// Mark `id` as bound within the walked subtree so it is never
    /// surfaced as an external ref (synthetic internal bindings).
    pub fn mark_bound(&mut self, id: BindId) {
        self.bound.insert(id);
    }

    /// Visit every id bound within the walked subtree.
    pub fn with_bound(&self, mut f: impl FnMut(BindId)) {
        for id in &*self.bound {
            f(*id);
        }
    }

    /// Visit every id read in the walked subtree, bound inside it or
    /// not (a `<-` target declared inside is still written across
    /// cycles).
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

/// A compiled graph node. Each recursive `Update` method is shadowed
/// by an inherent method that runs the vtable call under
/// [`stack::ensure_sufficient`]; the rest reach the trait through
/// `Deref`.
pub struct Node<R: Rt, E: UserEvent>(std::mem::ManuallyDrop<Box<dyn Update<R, E>>>);

/// `ManuallyDrop` moves the recursive teardown inside the stack guard;
/// field glue would run after `drop` returns.
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

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0(ctx))
    }

    pub fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck1(ctx))
    }

    pub fn refs(&self, refs: &mut Refs) {
        stack::ensure_sufficient(|| self.0.refs(refs))
    }

    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        stack::ensure_sufficient(|| self.0.sleep(ctx))
    }

    pub fn emit_clif(&self, cx: &mut BodyCx) -> Result<fusion::emit::CompiledExpr> {
        stack::ensure_sufficient(|| self.0.emit_clif(cx))
    }

    pub fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        stack::ensure_sufficient(|| self.0.fuse(ctx))
    }

    pub(crate) fn downcast_mut<T: Update<R, E>>(&mut self) -> Option<&mut T> {
        let node: &mut dyn Update<R, E> = &mut **self.0;
        (node as &mut dyn Any).downcast_mut::<T>()
    }

    pub fn image_encode(
        &self,
        buf: &mut image::ImageBuf,
    ) -> std::result::Result<(), netidx_core::pack::PackError> {
        stack::ensure_sufficient(|| self.0.image_encode(buf))
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
            &'b mut CompileCtx<R, E>,
            &'c mut [Node<R, E>],
            BindMode<'d>,
            ExprId,
        ) -> Result<Box<dyn Apply<R, E>>>
        + Send
        + Sync
        + 'static,
>;

/// A function application. Its arguments are owned by the `CallSite`,
/// so the function can change at runtime without recompiling them.
pub trait Apply<R: Rt, E: UserEvent>: Debug + Send + Sync + Any {
    /// Typed view for analysis code. The default is `BuiltIn` (opaque).
    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::BuiltIn
    }

    /// Same borrowed-production contract as [`Update::update`]: the
    /// returned `&TagValue` is the builtin's resident result slot.
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue;

    /// Delete any internally generated nodes.
    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {
        ()
    }

    /// First typecheck pass: the call's arguments against the
    /// lambda's own FnType.
    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    /// Second typecheck pass.
    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        Ok(())
    }

    fn image_encode(
        &self,
        buf: &mut image::ImageBuf,
    ) -> std::result::Result<(), netidx_core::pack::PackError>;

    /// The lambda's type; the BuiltIn wrapper implements it for builtins.
    fn typ(&self) -> Arc<FnType> {
        Arc::new(FnType {
            args: Arc::from_iter([]),
            rtype: Type::Bottom,
            throws: Type::Bottom,
            vargs: None,
            explicit_throws: false,
            ..Default::default()
        })
    }

    /// Record every id bound and referenced by this node. Only needed
    /// by builtins that create nodes.
    fn refs<'a>(&self, _refs: &mut Refs) {}

    /// Pause the builtin (an unselected arm). Values and semantic state
    /// are retained; a builtin that discards pending work or detaches an
    /// event source must clear its output to phantom, and the restart
    /// builtins clear their latches.
    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>);

    /// Emit this call site into the open JIT kernel as CLIF.
    /// `Ok(Some(cv))`: emitted. `Ok(None)`: shape not handled, and no
    /// instructions may have been emitted. `Err`: abort the kernel
    /// build (partial emission is fine). A builtin's site reaches this
    /// only when its [`Effect::Stateless`] carries a [`FastCall`];
    /// discovery de-fuses the region before emission otherwise.
    fn emit_clif(
        &self,
        _callsite: &CallSite<R, E>,
        _cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        Ok(None)
    }

    /// Fuse inside this apply when its call site did not fuse
    /// (`GXLambda` fuses its instance body's sync regions in place).
    /// Build errors must be swallowed, never fail the compile.
    fn fuse(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }
}

/// Typed view of an [`Apply`], symmetric to [`NodeView`]: a Graphix
/// lambda with a walkable body, or an opaque builtin.
pub enum ApplyView<'a, R: Rt, E: UserEvent> {
    Lambda(&'a GXLambda<R, E>),
    BuiltIn,
}

/// Exhaustive typed view of the compiled node graph, one variant per
/// concrete `Update` impl, for compile-time analysis. No catch-all:
/// a new node type must pick a variant, and a new variant must be
/// handled by every exhaustive match.
#[allow(missing_docs)]
pub enum NodeView<'a, R: Rt, E: UserEvent> {
    Bind(&'a node::bind::Bind<R, E>),
    Lambda(&'a node::lambda::Lambda),
    Block(&'a node::Block<R, E>),
    Module(&'a node::module::Module<R, E>),
    CallSite(&'a CallSite<R, E>),
    MapQ(&'a node::collection::MapQBase<R, E>),
    FoldQ(&'a node::collection::FoldQBase<R, E>),
    Select(&'a node::select::Select<R, E>),
    Catch(&'a node::error::Catch<R, E>),
    SeqGuard(&'a node::error::SeqGuard<R, E>),
    SeqAbort(&'a node::error::SeqAbortEvent<R, E>),
    SeqMachine(&'a node::seq_machine::SeqMachine<R, E>),
    SeqCapture(&'a node::seq_machine::SeqCapture<R, E>),
    Qop(&'a node::error::Qop<R, E>),
    OrNever(&'a node::error::OrNever<R, E>),
    ExplicitParens(&'a node::ExplicitParens<R, E>),
    TypeCast(&'a node::TypeCast<R, E>),
    Connect(&'a node::Connect<R, E>),
    ConnectDeref(&'a node::ConnectDeref<R, E>),
    StringInterpolate(&'a node::StringInterpolate<R, E>),
    Any(&'a node::Any<R, E>),
    Never(&'a node::Never<R, E>),
    Sample(&'a node::Sample<R, E>),
    Struct(&'a node::data::Struct<R, E>),
    StructWith(&'a node::data::StructWith<R, E>),
    Tuple(&'a node::data::Tuple<R, E>),
    Variant(&'a node::data::Variant<R, E>),
    Construct(&'a node::data::Construct<R, E>),
    Array(&'a node::array::Array<R, E>),
    ListLit(&'a node::array::ListLit<R, E>),
    Map(&'a node::map::Map<R, E>),
    StructRef(&'a node::data::StructRef<R, E>),
    TupleRef(&'a node::data::TupleRef<R, E>),
    ArrayRef(&'a node::array::ArrayRef<R, E>),
    ArraySlice(&'a node::array::ArraySlice<R, E>),
    MapRef(&'a node::map::MapRef<R, E>),
    Ref(&'a node::bind::Ref),
    ByRef(&'a node::bind::ByRef<R, E>),
    Deref(&'a node::bind::Deref<R, E>),
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
    Constant(&'a node::Constant),
    TypeDef(&'a node::TypeDef),
    Impl(&'a node::traits::Impl<R, E>),
    Nop(&'a node::Nop),
    FusedKernel(&'a fusion::FusedKernel<R, E>),
}

/// A regular graph node, as opposed to a function application (Apply).
pub trait Update<R: Rt, E: UserEvent>: Debug + Send + Sync + Any + 'static {
    /// Update the node with the event and return its production,
    /// borrowed from the node's own resident slot. Every awake node
    /// delivers every cycle; a quiet cycle rides the resident. See
    /// [`TagView`].
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue;

    /// Delete the node and its children from the context.
    fn delete(&mut self, ctx: &mut ExecCtx<R, E>);

    /// First typecheck pass: structural checking. Each node checks
    /// itself and recurses into its children.
    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()>;

    /// `typecheck0` for a node of an instance, whose definition's check
    /// settled its types ([`node::lambda::InstanceTypes`]): an impl takes
    /// its types from there and does only the part of `typecheck0` that
    /// is state. The default checks.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _types: &mut node::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0(ctx)
    }

    /// Second typecheck pass, after `typecheck0` finished the whole
    /// tree: `lambda_ids` are final, so call sites can resolve
    /// statically. No default: every node must recurse into its children.
    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()>;

    /// The node's type.
    fn typ(&self) -> &Type;

    /// Record every bind id referenced or bound by the node and its
    /// children.
    fn refs(&self, refs: &mut Refs);

    /// The expression this node was compiled from.
    fn spec(&self) -> &Expr;

    /// Pause the node (an unselected arm).
    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>);

    /// The node's typed view for compile-time analysis.
    fn view(&self) -> NodeView<'_, R, E>;

    /// Write this node into an image: its `NodeTag` and the data its
    /// `image_decode` rebuilds it from. State is never written; an
    /// image is taken before any cycle. The default is the refusal a
    /// kind without a codec reports.
    fn image_encode(
        &self,
        _buf: &mut image::ImageBuf,
    ) -> std::result::Result<(), netidx_core::pack::PackError> {
        warn!("no image codec for the node at {}", self.spec());
        Err(netidx_core::pack::PackError::Application(image::NOT_IMAGED))
    }

    /// Emit this node into the open JIT kernel as CLIF and return its
    /// SSA result; an impl emits its children via `child.emit_clif(cx)`.
    /// The default is `Err`: the subtree node-walks.
    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<fusion::emit::CompiledExpr> {
        anyhow::bail!(
            "node does not emit CLIF (spec id {:?}, `{}`) — subtree \
             node-walks",
            self.spec().id,
            self.spec()
        )
    }

    /// Fuse this subtree. `Ok(Some(replacement))`: the caller swaps it
    /// in and deletes the old node. `Ok(None)`: the impl already
    /// recursed into its children via [`fusion::fuse`]. The default is
    /// `Ok(None)` with no recursion; only the containers fusion
    /// descends through override it.
    fn fuse(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        Ok(None)
    }
}

/// Decode a builtin's application from its image over the restored
/// argument references.
pub type BuiltInDecodeFn<R, E> =
    fn(
        &mut ExecCtx<R, E>,
        &[Node<R, E>],
        &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R, E>>, netidx_core::pack::PackError>;

pub type BuiltInInitFn<R, E> = for<'a, 'b, 'c, 'd> fn(
    &'a mut CompileCtx<R, E>,
    &'a FnType,
    Option<&'d FnType>,
    &'b Scope,
    &'c [Node<R, E>],
    ExprId,
) -> Result<Box<dyn Apply<R, E>>>;

/// A builtin's direct-call entry (see [`FastCall`]): a pure function
/// over present argument values; `None` is bottom this cycle.
pub type FastFn = fn(&[Value]) -> Option<Value>;

/// [`FastFn`] plus the call site's resolved return `Type` and its env,
/// for a builtin whose result is directed by its return type.
pub type TypedFastFn = fn(&env::Env, &Type, &[Value]) -> Option<Value>;

/// The direct-call entry of a [`Effect::Stateless`] builtin: the JIT
/// calls it at every fused site with the present argument values (a
/// tainted argument bottoms the call without invoking it). It may
/// re-evaluate whenever any region input fires, so keep it cheap.
/// `eval` should delegate to the same fn (`graphix_package_core::
/// fast_eval`). `Typed` also receives the site's resolved return type.
#[derive(Debug, Clone, Copy)]
pub enum FastCall {
    Plain(FastFn),
    Typed(TypedFastFn),
}

/// A Graphix builtin implemented in Rust.
pub trait BuiltIn<R: Rt, E: UserEvent> {
    /// The builtin's name, `package::unique_name` for a package builtin.
    const NAME: &str;
    /// The builtin's classification; see [`Effect`]. Default `Async`.
    const EFFECT: Effect = Effect::Async;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved_type: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>>;

    /// Restore an application `image_encode` wrote, over the restored
    /// argument references (`from`, as `init` saw them).
    fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R, E>>, netidx_core::pack::PackError>;
}

/// A compile-time check for a `#[..]` attribute, dispatched by the
/// fusion walk on the final (possibly fused) node of the decorated
/// expression; under `--no-fusion` it never runs. `Err` is a compile
/// error. Registered via [`ExecCtx::register_attribute`]. The
/// definition assertions (`#[tail_recursive]`/`#[sync]`/`#[async]`)
/// are compiler-reserved, not registry attributes.
pub type AttributeCheckFn<R, E> = fn(&CompileCtx<R, E>, &Attr, &Node<R, E>) -> Result<()>;

/// What an attribute demands of its target whatever fusion made of it:
/// it also runs on a node absorbed into a larger kernel, where
/// [`AttributeCheckFn`] never does.
pub type AttributeTargetFn<R, E> = fn(&Attr, &Node<R, E>) -> Result<()>;

/// A definition assertion (`#[tail_recursive]` / `#[sync]` /
/// `#[async]`), verified at the tail of `analysis::analyze` once the
/// definition is reached; until then it stays pending.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefAssertionKind {
    Sync,
    Async,
    TailRecursive,
}

impl DefAssertionKind {
    const ALL: [Self; 3] = [Self::Sync, Self::Async, Self::TailRecursive];

    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Sync => "sync",
            Self::Async => "async",
            Self::TailRecursive => "tail_recursive",
        }
    }

    pub(crate) fn from_name(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|k| k.name() == name)
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
    /// The bare attribute name (`native` for `#[native]`); a flat
    /// global namespace.
    const NAME: &str;
    fn check(ctx: &CompileCtx<R, E>, attr: &Attr, node: &Node<R, E>) -> Result<()>;
    /// See [`AttributeTargetFn`].
    fn check_target(_attr: &Attr, _node: &Node<R, E>) -> Result<()> {
        Ok(())
    }
}

/// `#[native]`: the decorated expression must compile to native code
/// with zero node-walk residue. A function-typed target is rejected;
/// the requirement belongs at the use site.
pub struct Native;

impl<R: Rt, E: UserEvent> Attribute<R, E> for Native {
    const NAME: &str = "native";

    fn check_target(_attr: &Attr, node: &Node<R, E>) -> Result<()> {
        if let Type::Fn(_) = node.typ() {
            crate::bailat!(
                node.spec(),
                "#[native] annotates a computation or a call, not a function — \
                 put it on the call site, not the definition"
            );
        }
        if !matches!(node.view(), NodeView::FusedKernel(_))
            && !fusion::region_is_candidate(node)
        {
            crate::bailat!(
                node.spec(),
                "#[native] annotates a computation; a declaration or a bare \
                 variable read has nothing to fuse — put it on the initializer"
            );
        }
        Ok(())
    }

    fn check(ctx: &CompileCtx<R, E>, attr: &Attr, node: &Node<R, E>) -> Result<()> {
        <Self as Attribute<R, E>>::check_target(attr, node)?;
        if let NodeView::FusedKernel(_) = node.view() {
            return Ok(());
        }
        // Report only the leaf-most failures whose subtree contains no
        // fused region: `try_fuse` records a failure for every region
        // root it tries, containers included.
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

    /// Called whenever a bound variable (or lambda) is referenced;
    /// `ref_by` is the toplevel expression containing the reference,
    /// which must be updated when the variable changes.
    fn ref_var(&mut self, id: BindId, ref_by: ExprId);
    fn unref_var(&mut self, id: BindId, ref_by: ExprId);

    /// Queue a variable write for the next cycle. Writes to distinct
    /// variables are delivered in one event; a second write to the same
    /// variable waits a cycle. The event must not change mid-cycle.
    fn set_var(&mut self, id: BindId, value: Value);
    /// Queue a write through a path: at delivery the variable's value as
    /// it then stands is rebuilt along `path` with `value` at the end.
    /// Deferred exactly like `set_var`.
    fn patch_var(&mut self, id: BindId, path: node::place::Path, value: Value);
    /// Register the place a reference cell stands for (`&root[i].f`), so
    /// `*r` reads through the root and `*r <- v` patches it.
    fn set_ref_path(&mut self, cell: BindId, root: BindId, path: node::place::Path);
    fn ref_path(&self, cell: &BindId) -> Option<&(BindId, node::place::Path)>;
    fn clear_ref_path(&mut self, cell: &BindId);

    /// The persistent store: the (production, cycle stamp) of every
    /// bound variable's last delivery. `stamp == cycle()` reads as
    /// delivered this cycle, an older stamp as standing (Stale; Fired
    /// under an init view), absence as the phantom. Maintained at
    /// delivery, never ahead of it.
    fn store(&self) -> &IntMap<BindId, (TagValue, u64)>;

    /// The last delivered value of a bind; `None` if the last delivery
    /// was a bottom.
    fn store_value(&self, id: &BindId) -> Option<Value> {
        self.store().get(id).and_then(|(tv, _)| {
            if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) }
        })
    }

    /// Insert a production into the store, stamped with the current
    /// cycle.
    fn store_insert(&mut self, id: BindId, tv: TagValue);

    /// Remove a bind from the store.
    fn store_remove(&mut self, id: &BindId);

    /// Insert a standing entry, stamped as an earlier cycle so a
    /// same-cycle reader never sees it as delivered.
    fn store_insert_standing(&mut self, id: BindId, tv: TagValue);

    /// The current cycle number.
    fn cycle(&self) -> u64;

    /// A variable was set within the current cycle; dependent toplevel
    /// nodes must be updated.
    fn notify_set(&mut self, id: BindId);

    /// Deliver a variable event for `id` carrying the current time after
    /// `timeout`.
    fn set_timer(&mut self, id: BindId, timeout: Duration);

    /// Spawn a task whose output is delivered as a custom event for the
    /// returned `BindId`. `abort` before completion guarantees no
    /// delivery.
    fn spawn<F: Future<Output = (BindId, Box<dyn CustomBuiltinType>)> + Send + 'static>(
        &mut self,
        f: F,
    ) -> Self::AbortHandle;

    /// Spawn a task whose output is delivered as a variable event for
    /// the returned `BindId`. `abort` before completion guarantees no
    /// delivery.
    fn spawn_var<F: Future<Output = (BindId, Value)> + Send + 'static>(
        &mut self,
        f: F,
    ) -> Self::AbortHandle;

    /// Deliver batches arriving on the channel as custom updates.
    fn watch(
        &mut self,
        s: mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    );

    /// Deliver batches arriving on the channel as variable updates.
    fn watch_var(&mut self, s: mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>);
}

#[derive(Default)]
pub struct LibState(AHashMap<TypeId, Box<dyn Any + Send + Sync>>);

impl LibState {
    /// The library state of type `T`, created with `T::default` if absent.
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

    /// The library state of type `T`, created with `f` if absent.
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

    /// True if `T` is present.
    pub fn contains<T>(&self) -> bool
    where
        T: Any + Send + Sync,
    {
        self.0.contains_key(&TypeId::of::<T>())
    }

    /// The library state of type `T`, if registered.
    pub fn get<T>(&self) -> Option<&T>
    where
        T: Any + Send + Sync,
    {
        self.0.get(&TypeId::of::<T>()).map(|t| t.downcast_ref::<T>().unwrap())
    }

    /// The library state of type `T`, mutably, if registered.
    pub fn get_mut<T>(&mut self) -> Option<&mut T>
    where
        T: Any + Send + Sync,
    {
        self.0.get_mut(&TypeId::of::<T>()).map(|t| t.downcast_mut::<T>().unwrap())
    }

    /// Set the library state of type `T`, returning any existing state.
    pub fn set<T>(&mut self, t: T) -> Option<Box<T>>
    where
        T: Any + Send + Sync,
    {
        self.0
            .insert(TypeId::of::<T>(), Box::new(t) as Box<dyn Any + Send + Sync>)
            .map(|t| t.downcast::<T>().unwrap())
    }

    /// Remove and return the library state of type `T`.
    pub fn remove<T>(&mut self) -> Option<Box<T>>
    where
        T: Any + Send + Sync,
    {
        self.0.remove(&TypeId::of::<T>()).map(|t| t.downcast::<T>().unwrap())
    }
}

/// A registry of abstract type UUIDs with a string tag per type. Each
/// monomorphization over Rt/UserEvent is a distinct type id and needs
/// its own UUID; the tag lets non-parameterized code (printers) know
/// what a value generally is.
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

    /// The UUID of abstract type T.
    pub(crate) fn uuid<T: Any>(tag: &'static str) -> Uuid {
        Self::with(|rg| {
            *rg.by_tid.entry(TypeId::of::<T>()).or_insert_with(|| {
                let id = Uuid::new_v4();
                rg.by_uuid.insert(id, tag);
                id
            })
        })
    }

    /// The tag of this abstract type, if registered.
    pub fn tag(a: &Abstract) -> Option<&'static str> {
        Self::with(|rg| rg.by_uuid.get(&a.id()).map(|r| *r))
    }

    /// True if the abstract type has `tag`.
    pub fn is_a(a: &Abstract, tag: &str) -> bool {
        match Self::tag(a) {
            Some(t) => t == tag,
            None => false,
        }
    }
}

/// A call site's instantiation identity: per argument, sorted by its
/// key, the source lambda ([`node::lambda::LambdaDef::source`]) it
/// statically resolves to, or `None`. Two sites reaching one def with the same identity are
/// one instantiation (a self-call); different identities are distinct
/// even while the def is resolving. Source identity, not `LambdaId`,
/// because a literal in an instance body is re-minted per compile.
pub(crate) type FnArgIdentity = SmallVec<[(node::callsite::ArgKey, Option<ExprId>); 4]>;

#[derive(Clone)]
pub(crate) struct ResolvingLambda {
    pub instance: LambdaInstanceId,
    pub ftype: FnType,
    pub identity: FnArgIdentity,
}

/// The active instantiations of one def, innermost last. A stack: a
/// site inside `h(k)` may reach a still-resolving `h(g)`.
pub(crate) type ResolvingStack = SmallVec<[ResolvingLambda; 2]>;

impl<R: Rt, E: UserEvent> CompileCtx<R, E> {
    /// The active instantiation of `def` with exactly this identity.
    pub(crate) fn resolving(
        &self,
        def: LambdaId,
        identity: &FnArgIdentity,
    ) -> Option<ResolvingLambda> {
        self.resolving_lambdas
            .lock()
            .get(&def)?
            .iter()
            .rev()
            .find(|r| r.identity == *identity)
            .cloned()
    }

    /// The innermost active instantiation of `def`, whatever its
    /// identity: what a bare value reference inside a resolving body
    /// refers to.
    pub(crate) fn resolving_innermost(&self, def: LambdaId) -> Option<ResolvingLambda> {
        self.resolving_lambdas.lock().get(&def)?.last().cloned()
    }

    pub(crate) fn push_resolving(&self, def: LambdaId, r: ResolvingLambda) {
        self.resolving_lambdas.lock().entry(def).or_default().push(r)
    }

    /// Retire the innermost entry `push_resolving` made for `instance`.
    pub(crate) fn pop_resolving(&self, def: LambdaId, instance: LambdaInstanceId) {
        let mut map = self.resolving_lambdas.lock();
        if let Some(stack) = map.get_mut(&def) {
            if let Some(i) = stack.iter().rposition(|r| r.instance == instance) {
                stack.remove(i);
            }
            if stack.is_empty() {
                map.remove(&def);
            }
        }
    }
}

/// A registered builtin: how its application is built, how an image
/// restores it, and its classification.
struct BuiltinEntry<R: Rt, E: UserEvent> {
    init: BuiltInInitFn<R, E>,
    decode: BuiltInDecodeFn<R, E>,
    effect: Effect,
}

macro_rules! env_restore_methods {
    () => {
        /// Run `f` with the lexical environment restored to `env`, then put
        /// the current one back. Bindings `f` creates are retained.
        pub fn with_restored<T, F: FnOnce(&mut Self) -> T>(
            &mut self,
            env: Env,
            f: F,
        ) -> T {
            let snap = self.env.restore_lexical_env(env);
            let orig = mem::replace(&mut self.env, snap);
            let r = f(self);
            self.env = self.env.restore_lexical_env(orig);
            r
        }

        /// [`Self::with_restored`] mutating `env` in place, so two envs
        /// keep continuity across invocations.
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
    };
}

/// What registration fills and compiling only reads, shared by every
/// compile task.
struct Registry<R: Rt, E: UserEvent> {
    lambdawrap: AbstractWrapper<LambdaDef<R, E>>,
    builtins: AHashMap<&'static str, BuiltinEntry<R, E>>,
    attributes: AHashMap<&'static str, (AttributeCheckFn<R, E>, AttributeTargetFn<R, E>)>,
}

/// Everything compiling and typechecking read and write: the builtin
/// and attribute registry, the program's state, and the scratch of the
/// compile in progress. The runtime half is [`ExecCtx`].
pub struct CompileCtx<R: Rt, E: UserEvent> {
    registry: Arc<Registry<R, E>>,
    // Sandboxing.
    builtins_allowed: bool,
    tags: TrackedSet<ArcStr>,
    /// The language environment: typedefs, binds, lambdas.
    pub env: Env,
    /// LambdaDefs by LambdaId.
    pub lambda_defs: TrackedMap<LambdaId, Value>,
    /// `BindId → LambdaDef Value` for every lambda binding, filled in
    /// `typecheck0` so `typecheck1`'s static resolution sees it
    /// complete. Persistent across batches (`Bind::delete` removes
    /// its ids); the `batch_connect_targets` guard excludes `<-` targets
    /// at read time.
    pub bind_to_lambda: TrackedMap<BindId, Value>,
    /// The `<-` targets of the current compile batch, recorded by
    /// [`node::Connect::compile`]; a `<-` target rebinds at runtime and
    /// must not be statically resolved.
    pub batch_connect_targets: TrackedSet<BindId>,
    /// Every `<-` target for the program's lifetime (`batch_connect_targets`
    /// is per batch): `Bind::update` must not reseed a woken target that
    /// holds a value. `Bind::delete` removes its ids.
    pub connect_targets: TrackedSet<BindId>,
    /// Builtin metadata for `let foo = |...| 'builtin_name` bindings.
    /// Keyed by `(scope, name)` because a sig and its impl share the
    /// name but have distinct `BindId`s.
    pub builtin_bindings: TrackedMap<(ModPath, CompactString), BuiltinBindInfo>,
    /// `LambdaId`s whose def-time body typecheck is in progress: a
    /// self-call inside such a body unifies against the def's own
    /// ftype cells (monomorphic recursion), not a freshening.
    pub(crate) rec_defs: nohash::IntSet<LambdaId>,
    /// The fn-typed parameter BindIds of the defs being body-checked: a
    /// call through one unifies against the param's own declared cells,
    /// so `f(v)` types as the def's rigid 'b.
    pub(crate) def_gate_params: nohash::IntSet<BindId>,
    /// Def-gate nesting depth; a nested gate's cells are still
    /// entangled with the enclosing inference.
    pub(crate) def_gate_depth: usize,
    pub(crate) resolving_lambdas: Mutex<IntMap<LambdaId, ResolvingStack>>,
    /// Per-instance fn-formal BindId → the `LambdaId` forwarded to it:
    /// the persistent record the kernel cache fingerprint reads after
    /// the re-drive's `bind_to_lambda` entry is gone.
    pub fn_forward_resolutions: TrackedMap<BindId, LambdaId>,
    /// Each seq block's lowering, by its expression and lexical scope: a
    /// definition's body lowers once, so every compile of it has the same
    /// expression ids.
    pub(crate) lowered_seqs: TrackedMap<(ExprId, ModPath), Expr>,
    /// Deferred terminal settles, one frame per resolution scope. A
    /// call site pushes its resolved signature into the current frame;
    /// statement boundaries drain it, so a settle runs only after every
    /// writer in its scope. A re-drive's leftovers merge up to the
    /// parent frame.
    pub(crate) pending_settles: Vec<Vec<PendingSettle>>,
    /// Imports whose terminal name did not exist when the `use`
    /// compiled (`use self::sub::x` may precede `mod sub;`); re-checked
    /// at the end of [`compile_stmt`].
    pub(crate) pending_imports: Vec<PendingImport>,
    /// Type names a definition's written types hold that did not resolve
    /// at its check (a `use` the interface defers may name them later);
    /// each must name something once the check ends
    /// ([`check_pending_names`]).
    pub(crate) pending_names: Vec<(typ::TypeRef, Expr)>,
    /// Pending definition assertions; see [`DefAssertion`].
    pub(crate) def_assertions: Mutex<Vec<DefAssertion>>,
    /// Registry attributes recorded this `compile_stmt`; each must be
    /// dispatched or absorbed by the fusion walk, or the statement errors.
    pub(crate) attr_census: Mutex<Vec<Expr>>,
    pub(crate) attr_dispatched: Mutex<IntSet<ExprId>>,
    pub(crate) attr_absorbed: Mutex<IntSet<ExprId>>,
    /// References compiled but not yet registered with the runtime, by
    /// variable and top expression: compiling reaches no runtime, and
    /// [`ExecCtx::apply_deferred`] registers them.
    pub(crate) pending_refs: AHashMap<(BindId, ExprId), usize>,
    /// What compiling abandoned, for the runtime to delete.
    discarded: Vec<Discarded<R, E>>,
    /// The id of the concurrent compile task this one runs in
    /// ([`typ::tvar::InTask`]); 0 at the root.
    pub(crate) task: u32,
    /// The fusion subsystem's state; see [`fusion::FusionCtx`].
    pub fusion: fusion::FusionCtx,
}

enum Discarded<R: Rt, E: UserEvent> {
    Node(Node<R, E>),
    Apply(Box<dyn Apply<R, E>>),
    Stored(BindId),
}

/// The compile context and the runtime: what a node's update reads.
pub struct ExecCtx<R: Rt, E: UserEvent> {
    pub cx: CompileCtx<R, E>,
    /// The tables of the image this session was restored from, for
    /// anything decoded later.
    pub(crate) image_decoder: Option<image::SharedDecoder>,
    /// Library state for builtins.
    pub libstate: LibState,
    /// The runtime.
    pub rt: R,
    /// The call sites through which `Value` comparison and printing
    /// reach core-trait implementations, built on first use.
    pub(crate) core_hook_sites: node::coretraits::CoreHookSites<R, E>,
    /// Interrupt/abort control, shared with the runtime handle. See
    /// [`Control`].
    pub control: Arc<Control>,
}

impl<R: Rt, E: UserEvent> std::ops::Deref for ExecCtx<R, E> {
    type Target = CompileCtx<R, E>;

    fn deref(&self) -> &CompileCtx<R, E> {
        &self.cx
    }
}

impl<R: Rt, E: UserEvent> std::ops::DerefMut for ExecCtx<R, E> {
    fn deref_mut(&mut self) -> &mut CompileCtx<R, E> {
        &mut self.cx
    }
}

impl<R: Rt, E: UserEvent> CompileCtx<R, E> {
    /// A context for a compile task: it shares the registry, starts from
    /// this program's state, recording what it writes, and inherits the
    /// resolution in progress; its scratch outputs start empty.
    pub(crate) fn fork(&self) -> Self {
        Self {
            registry: self.registry.clone(),
            builtins_allowed: self.builtins_allowed,
            tags: self.tags.fork(),
            env: self.env.fork(),
            lambda_defs: self.lambda_defs.fork(),
            bind_to_lambda: self.bind_to_lambda.fork(),
            batch_connect_targets: self.batch_connect_targets.fork(),
            connect_targets: self.connect_targets.fork(),
            builtin_bindings: self.builtin_bindings.fork(),
            rec_defs: self.rec_defs.clone(),
            def_gate_params: self.def_gate_params.clone(),
            def_gate_depth: self.def_gate_depth,
            resolving_lambdas: Mutex::new(self.resolving_lambdas.lock().clone()),
            fn_forward_resolutions: self.fn_forward_resolutions.fork(),
            lowered_seqs: self.lowered_seqs.fork(),
            pending_settles: vec![Vec::new()],
            pending_imports: Vec::new(),
            pending_names: Vec::new(),
            def_assertions: Mutex::new(Vec::new()),
            attr_census: Mutex::new(Vec::new()),
            attr_dispatched: Mutex::new(IntSet::default()),
            attr_absorbed: Mutex::new(IntSet::default()),
            pending_refs: AHashMap::default(),
            discarded: Vec::new(),
            task: self.task,
            fusion: self.fusion.fork(),
        }
    }

    /// Take back what the task `fork` produced: its writes to the
    /// program's state and everything it deferred. The resolution scratch
    /// it inherited is balanced by the time it ends and stays this one's.
    pub(crate) fn join(&mut self, fork: Self) {
        let Self {
            registry: _,
            builtins_allowed: _,
            tags,
            env,
            lambda_defs,
            bind_to_lambda,
            batch_connect_targets,
            connect_targets,
            builtin_bindings,
            rec_defs: _,
            def_gate_params: _,
            def_gate_depth: _,
            resolving_lambdas: _,
            fn_forward_resolutions,
            lowered_seqs,
            pending_settles,
            pending_imports,
            pending_names,
            def_assertions,
            attr_census,
            attr_dispatched,
            attr_absorbed,
            pending_refs,
            discarded,
            task: _,
            fusion,
        } = fork;
        self.tags.join(tags);
        self.env.join(env);
        self.lambda_defs.join(lambda_defs);
        self.bind_to_lambda.join(bind_to_lambda);
        self.batch_connect_targets.join(batch_connect_targets);
        self.connect_targets.join(connect_targets);
        self.builtin_bindings.join(builtin_bindings);
        self.fn_forward_resolutions.join(fn_forward_resolutions);
        self.lowered_seqs.join(lowered_seqs);
        let frame = self.pending_settles.last_mut().expect("root settle frame");
        frame.extend(pending_settles.into_iter().flatten());
        self.pending_imports.extend(pending_imports);
        self.pending_names.extend(pending_names);
        self.def_assertions.lock().extend(def_assertions.into_inner());
        self.attr_census.lock().extend(attr_census.into_inner());
        self.attr_dispatched.lock().extend(attr_dispatched.into_inner());
        self.attr_absorbed.lock().extend(attr_absorbed.into_inner());
        for (k, n) in pending_refs {
            *self.pending_refs.entry(k).or_default() += n;
        }
        self.discarded.extend(discarded);
        self.fusion.join(fusion);
    }

    /// Record that `top_id` reads `id`: the runtime is told at the next
    /// [`ExecCtx::apply_deferred`].
    pub fn record_ref(&mut self, id: BindId, top_id: ExprId) {
        *self.pending_refs.entry((id, top_id)).or_default() += 1;
    }

    /// Abandon a node compiling built: it is deleted, with the runtime,
    /// at the next [`ExecCtx::apply_deferred`].
    pub fn discard(&mut self, node: Node<R, E>) {
        self.discarded.push(Discarded::Node(node));
    }

    /// [`Self::discard`] for an application.
    pub fn discard_apply(&mut self, apply: Box<dyn Apply<R, E>>) {
        self.discarded.push(Discarded::Apply(apply));
    }

    /// Forget the value the runtime stores for `id`, at the next
    /// [`ExecCtx::apply_deferred`].
    pub fn discard_stored(&mut self, id: BindId) {
        self.discarded.push(Discarded::Stored(id));
    }

    env_restore_methods!();

    /// Record `id` as a `<-` target, of this batch and for good.
    pub(crate) fn mark_connect_target(&mut self, id: BindId) {
        self.batch_connect_targets.insert(id);
        self.connect_targets.insert(id);
    }

    pub fn register_builtin<T: BuiltIn<R, E>>(&mut self) -> Result<()> {
        if node::collection::CollectionIntrinsic::from_name(T::NAME).is_some() {
            bail!("{} is a collection intrinsic reserved by the compiler", T::NAME)
        }
        let registry =
            Arc::get_mut(&mut self.registry).expect("registration precedes every fork");
        match registry.builtins.entry(T::NAME) {
            Entry::Vacant(e) => {
                e.insert(BuiltinEntry {
                    init: T::init,
                    decode: T::image_decode,
                    effect: T::EFFECT,
                });
            }
            Entry::Occupied(_) => bail!("builtin {} is already registered", T::NAME),
        }
        Ok(())
    }

    /// The image decoder of a registered builtin.
    pub fn builtin_decoder(&self, name: &str) -> Option<BuiltInDecodeFn<R, E>> {
        self.registry.builtins.get(name).map(|b| b.decode)
    }

    pub fn register_attribute<T: Attribute<R, E>>(&mut self) -> Result<()> {
        let registry =
            Arc::get_mut(&mut self.registry).expect("registration precedes every fork");
        match registry.attributes.entry(T::NAME) {
            Entry::Vacant(e) => {
                e.insert((T::check, T::check_target));
            }
            Entry::Occupied(_) => {
                bail!("attribute {} is already registered", T::NAME)
            }
        }
        Ok(())
    }

    /// The check fn for a registered attribute.
    pub fn lookup_attribute(&self, name: &str) -> Option<AttributeCheckFn<R, E>> {
        self.registry.attributes.get(name).map(|(check, _)| *check)
    }

    /// The target check for a registered attribute.
    pub fn lookup_attribute_target(&self, name: &str) -> Option<AttributeTargetFn<R, E>> {
        self.registry.attributes.get(name).map(|(_, target)| *target)
    }

    /// A registered builtin's [`Effect`]; `Async` for unknown names.
    pub fn builtin_effect(&self, name: &str) -> Effect {
        self.registry.builtins.get(name).map(|b| b.effect).unwrap_or_default()
    }

    /// Wrap a `LambdaDef` into a first-class function `Value` and
    /// register it in `lambda_defs`.
    pub fn wrap_lambda(&mut self, def: LambdaDef<R, E>) -> Value {
        let id = def.id;
        let v = self.registry.lambdawrap.wrap(def);
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
}

impl<R: Rt, E: UserEvent> ExecCtx<R, E> {
    /// Build a new execution context. A low-level interface for custom
    /// runtimes; most embedders want `graphix-rt`.
    pub fn new(user: R) -> Result<Self> {
        let id = AbstractTypeRegistry::uuid::<LambdaDef<R, E>>("lambda");
        let mut this = Self {
            cx: CompileCtx {
                registry: Arc::new(Registry {
                    lambdawrap: Abstract::register(id)?,
                    builtins: AHashMap::default(),
                    attributes: AHashMap::default(),
                }),
                builtins_allowed: true,
                tags: TrackedSet::default(),
                env: Env::default(),
                lambda_defs: TrackedMap::default(),
                bind_to_lambda: TrackedMap::default(),
                batch_connect_targets: TrackedSet::default(),
                connect_targets: TrackedSet::default(),
                builtin_bindings: TrackedMap::default(),
                rec_defs: nohash::IntSet::default(),
                def_gate_params: nohash::IntSet::default(),
                def_gate_depth: 0,
                resolving_lambdas: Mutex::new(IntMap::default()),
                fn_forward_resolutions: TrackedMap::default(),
                lowered_seqs: TrackedMap::default(),
                pending_settles: vec![Vec::new()],
                pending_imports: Vec::new(),
                pending_names: Vec::new(),
                def_assertions: Mutex::new(Vec::new()),
                attr_census: Mutex::new(Vec::new()),
                attr_dispatched: Mutex::new(IntSet::default()),
                attr_absorbed: Mutex::new(IntSet::default()),
                pending_refs: AHashMap::default(),
                discarded: Vec::new(),
                task: 0,
                fusion: fusion::FusionCtx::new()?,
            },
            image_decoder: None,
            libstate: LibState::default(),
            rt: user,
            core_hook_sites: node::coretraits::CoreHookSites::default(),
            control: Arc::new(Control::new()),
        };
        this.register_attribute::<Native>()?;
        Ok(this)
    }

    /// Drop a reference `top_id` holds to `id`: one not replayed yet is
    /// cancelled, any other unregistered.
    pub fn unref_var(&mut self, id: BindId, top_id: ExprId) {
        match self.cx.pending_refs.get_mut(&(id, top_id)) {
            Some(n) => {
                *n -= 1;
                if *n == 0 {
                    self.cx.pending_refs.remove(&(id, top_id));
                }
            }
            None => self.rt.unref_var(id, top_id),
        }
    }

    /// Apply to the runtime what compiling deferred: delete what it
    /// discarded, whose unreplayed references cancel, then register
    /// every reference it recorded.
    pub fn apply_deferred(&mut self) {
        self.delete_discarded();
        for ((id, top_id), n) in self.cx.pending_refs.drain() {
            for _ in 0..n {
                self.rt.ref_var(id, top_id);
            }
        }
    }

    /// Undo what a failed compile deferred: delete what it discarded and
    /// forget the references it recorded.
    pub fn drop_deferred(&mut self) {
        self.delete_discarded();
        self.cx.pending_refs.clear();
    }

    fn delete_discarded(&mut self) {
        for d in mem::take(&mut self.cx.discarded) {
            match d {
                Discarded::Node(mut n) => n.delete(self),
                Discarded::Apply(mut a) => a.delete(self),
                Discarded::Stored(id) => self.rt.store_remove(&id),
            }
        }
    }

    /// Whether a compile deferred something no
    /// [`Self::apply_deferred`] applied.
    pub fn deferred_pending(&self) -> bool {
        !self.cx.pending_refs.is_empty() || !self.cx.discarded.is_empty()
    }

    /// True if an `interrupt()` or `abort()` is pending; loops poll this
    /// at their head.
    pub fn interrupted(&self) -> bool {
        self.control.interrupted()
    }

    /// Open a compile frame for a node built at runtime outside any
    /// statement, as `compile_stmt` does before [`check_and_fuse`].
    pub fn begin_runtime_node(&mut self, top_id: ExprId) {
        self.fusion.top_id = Some(top_id);
        self.pending_settles.clear();
        self.pending_settles.push(Vec::new());
    }
    env_restore_methods!();
}

/// What the check defers to its statement's settle; see
/// [`ExecCtx::pending_settles`].
pub(crate) enum PendingSettle {
    /// A call site's terminal settle.
    Site {
        /// The site's resolved signature.
        ftype: FnType,
        /// The site's return-type cell.
        rtype: Option<typ::TVar>,
        /// Cells (by address) exempt from settling: an omitted defaulted
        /// argument's.
        exempt: AHashSet<usize>,
        /// The signatures of the enclosing definitions: what they reach
        /// is generalized, settled by each call.
        sigs: SmallVec<[triomphe::Arc<FnType>; 1]>,
        spec: Arc<Expr>,
    },
    /// An operator's operand cell, settled once the frame's sites have.
    Operand { tv: typ::TVar, spec: Arc<Expr> },
    /// A `let`'s cell over the initializer type `init`: ⊥ when `init` is
    /// ⊥ and no writer or reader decided the cell.
    LetOverBottom { tv: typ::TVar, init: Type, spec: Arc<Expr> },
    /// A definition's body, settled once its sites have: a cell its gate
    /// created that nothing bounded binds ⊥, the signature's `exempt`.
    Body {
        table: std::sync::Arc<node::lambda::DefTable>,
        owner: LambdaId,
        exempt: AHashSet<usize>,
        spec: Arc<Expr>,
    },
    /// `outer ⊇ inner`, judged once the frame's cells have settled.
    Contains { outer: Type, inner: Type, spec: Arc<Expr> },
    /// An arithmetic operator's rule, judged again once its operands
    /// settled ([`node::op::arith_rule`]).
    Arith {
        op: node::op::BinOp,
        checked: bool,
        lhs: Type,
        rhs: Type,
        out: Type,
        spec: Arc<Expr>,
    },
}

impl PendingSettle {
    /// Run every site settle of `frame`, then every cell settle, then
    /// every rule judged over the settled cells.
    pub(crate) fn drain(
        frame: &[PendingSettle],
        env: &Env,
        mut on_err: impl FnMut(&Arc<Expr>, anyhow::Error) -> Result<()>,
    ) -> Result<()> {
        let mut reached: LPooled<AHashMap<usize, AHashSet<usize>>> = LPooled::take();
        for s in frame.iter() {
            if let PendingSettle::Site { ftype, rtype, exempt, sigs, spec } = s {
                let mut kept: SmallVec<[&AHashSet<usize>; 2]> = SmallVec::new();
                for sig in sigs.iter() {
                    reached.entry(triomphe::Arc::as_ptr(sig).addr()).or_insert_with(
                        || {
                            let mut cells: LPooled<AHashMap<usize, typ::TVar>> =
                                LPooled::take();
                            sig.reached_cells(&mut cells);
                            cells.keys().copied().collect()
                        },
                    );
                }
                for sig in sigs.iter() {
                    kept.push(&reached[&triomphe::Arc::as_ptr(sig).addr()]);
                }
                if let Err(e) = ftype.settle_terminal(env, rtype.as_ref(), exempt, &kept)
                {
                    on_err(spec, e)?
                }
            }
        }
        for s in frame.iter() {
            let res = match s {
                PendingSettle::Operand { tv, spec } => (tv.settle(env), spec),
                PendingSettle::LetOverBottom { tv, init, spec } => {
                    let bottom = init.with_deref(|t| matches!(t, Some(Type::Bottom)));
                    (if bottom { tv.settle_or_bottom(env) } else { Ok(()) }, spec)
                }
                PendingSettle::Body { table, owner, exempt, spec } => {
                    (table.settle_open(env, *owner, exempt), spec)
                }
                _ => continue,
            };
            if let (Err(e), spec) = res {
                on_err(spec, e)?
            }
        }
        for s in frame.iter() {
            let res = match s {
                PendingSettle::Contains { outer, inner, spec } => {
                    (outer.check_contains(env, inner), spec)
                }
                PendingSettle::Arith { op, checked, lhs, rhs, out, spec } => {
                    (node::op::arith_rule(env, *op, *checked, lhs, rhs, out), spec)
                }
                _ => continue,
            };
            if let (Err(e), spec) = res {
                on_err(spec, e)?
            }
        }
        Ok(())
    }
}

/// A deferred import-existence check; see [`ExecCtx::pending_imports`].
#[derive(Debug)]
pub(crate) struct PendingImport {
    pub(crate) scope: ModPath,
    pub(crate) key: compact_str::CompactString,
    pub(crate) pos: combine::stream::position::SourcePosition,
    pub(crate) ori: triomphe::Arc<expr::Origin>,
}

/// The lexical scope (module and block nesting path) and dynamic scope
/// (the error handlers visible to a `?`, following the call chain) of
/// a point in a program.
#[derive(Debug, Clone)]
pub struct Scope {
    pub lexical: ModPath,
    pub dynamic: DynScope,
}

impl Scope {
    pub fn append<S: AsRef<str> + ?Sized>(&self, s: &S) -> Self {
        Self { lexical: ModPath(self.lexical.append(s)), dynamic: self.dynamic.clone() }
    }

    /// Append a generated block-scope component (do/fn/sel/ca level).
    pub fn append_block(&self, kind: &str, id: u64) -> Self {
        self.append(block_component(kind, id).as_str())
    }

    /// The scope covered by a handler installed here; `machine` marks a
    /// seq machine's own handler.
    pub fn with_catch(&self, catch: (BindId, ExprId), machine: bool) -> Self {
        Self {
            lexical: self.lexical.clone(),
            dynamic: self.dynamic.with_catch(catch, machine),
        }
    }

    pub fn root() -> Self {
        Self { lexical: ModPath::root(), dynamic: DynScope::root() }
    }
}

/// The chain of installed error handlers visible at a point, innermost
/// first, one node per install. Each node names the handler's
/// error-variable bind and the top it lives under.
#[derive(Clone, Default)]
pub struct DynScope(Option<ErrorHandler>);

struct DynNode {
    catch: (BindId, ExprId),
    machine: bool,
    /// One past the cycle of the last `abort(..)`; zero before any.
    aborted: AtomicU64,
    raised: AtomicU64,
    /// Raised to descendant handlers and not yet processed by them.
    nested: AtomicU64,
    parent: DynScope,
}

#[derive(Clone)]
pub(crate) struct ErrorHandler(Arc<DynNode>);

impl ErrorHandler {
    pub(crate) fn id(&self) -> (BindId, ExprId) {
        self.0.catch
    }

    pub(crate) fn same(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }

    pub(crate) fn raise(&self) {
        self.0.raised.fetch_add(1, Ordering::Relaxed);
        let mut parent = self.0.parent.0.as_ref();
        while let Some(handler) = parent {
            handler.0.nested.fetch_add(1, Ordering::Relaxed);
            parent = handler.0.parent.0.as_ref();
        }
    }

    pub(crate) fn handled(&self) {
        let mut parent = self.0.parent.0.as_ref();
        while let Some(handler) = parent {
            let pending = handler.0.nested.fetch_sub(1, Ordering::Relaxed);
            debug_assert!(pending > 0, "unraised nested error");
            parent = handler.0.parent.0.as_ref();
        }
    }

    /// A seq's `abort(..)` fired: the run fails with no error in flight.
    pub(crate) fn abort(&self, cycle: u64) {
        self.0.raised.fetch_add(1, Ordering::Relaxed);
        self.0.aborted.store(cycle.wrapping_add(1), Ordering::Relaxed);
    }

    /// Whether an `abort(..)` fired in `cycle`: a step entered in that
    /// cycle is entered into a failed run.
    pub(crate) fn aborted_in(&self, cycle: u64) -> bool {
        self.0.aborted.load(Ordering::Relaxed) == cycle.wrapping_add(1)
    }

    pub(crate) fn generation(&self) -> u64 {
        self.0.raised.load(Ordering::Relaxed)
    }

    pub(crate) fn is_machine(&self) -> bool {
        self.0.machine
    }

    /// The handler of the innermost seq machine covering this one,
    /// itself included.
    pub(crate) fn machine(&self) -> Option<ErrorHandler> {
        let mut cur = Some(self);
        while let Some(h) = cur {
            if h.0.machine {
                return Some(h.clone());
            }
            cur = h.0.parent.0.as_ref();
        }
        None
    }

    /// The handler's allocation address: equal for every scope under
    /// one catch.
    pub(crate) fn identity(&self) -> usize {
        Arc::as_ptr(&self.0) as *const () as usize
    }

    pub(crate) fn parent(&self) -> DynScope {
        self.0.parent.clone()
    }

    pub(crate) fn has_nested_errors(&self) -> bool {
        self.0.nested.load(Ordering::Relaxed) != 0
    }
}

impl Debug for ErrorHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id().fmt(f)
    }
}

impl DynScope {
    pub fn root() -> Self {
        Self(None)
    }

    pub(crate) fn from_handler(h: ErrorHandler) -> Self {
        Self(Some(h))
    }

    pub fn with_catch(&self, catch: (BindId, ExprId), machine: bool) -> Self {
        Self(Some(ErrorHandler(Arc::new(DynNode {
            catch,
            machine,
            aborted: AtomicU64::new(0),
            raised: AtomicU64::new(0),
            nested: AtomicU64::new(0),
            parent: self.clone(),
        }))))
    }

    /// The innermost visible handler, if any.
    pub fn catch(&self) -> Option<(BindId, ExprId)> {
        self.0.as_ref().map(ErrorHandler::id)
    }

    pub(crate) fn handler(&self) -> Option<ErrorHandler> {
        self.0.clone()
    }

    /// The number of handlers visible from here.
    pub fn depth(&self) -> usize {
        let mut n = 0;
        let mut cur = self.0.as_ref();
        while let Some(node) = cur {
            n += 1;
            cur = node.0.parent.0.as_ref();
        }
        n
    }
}

impl std::fmt::Debug for DynScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DynScope(depth: {}, catch: {:?})", self.depth(), self.catch())
    }
}

/// The chain can be as deep as a recursion, so the drop is a loop, not
/// glue recursing into `parent`.
impl Drop for DynNode {
    fn drop(&mut self) {
        let mut next = self.parent.0.take();
        while let Some(handler) = next {
            match Arc::try_unwrap(handler.0) {
                Ok(mut node) => next = node.parent.0.take(),
                Err(_) => break,
            }
        }
    }
}

/// Format a generated block-scope component. The `#` prefix marks a
/// non-module level (identifiers cannot start with `#`); [`mod_root`]
/// strips them.
pub fn block_component(kind: &str, id: u64) -> CompactString {
    compact_str::format_compact!("#{kind}{id}")
}

/// True iff `part` is a generated block-scope component rather than
/// a module name.
pub fn is_block_component(part: &str) -> bool {
    part.starts_with('#')
}

/// True iff `part` is a `do` block's scope component: a loaded
/// script's top level is one.
pub fn is_do_block(part: &str) -> bool {
    part.strip_prefix("#do").is_some_and(|id| id.bytes().all(|b| b.is_ascii_digit()))
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

/// Compile the expression into a node graph in the given context and
/// scope, returning the root node.
pub fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<Node<R, E>> {
    compile_stmt(ctx, flags, scope, spec).map(|(n, _)| n)
}

/// The passes every node runs after it is built and before it updates:
/// the check (typecheck0 and its settle), elaboration (typecheck1 and
/// its settle), function-property analysis, typedef resolution-cell
/// seeding (in both modes) and fusion; only the check under
/// [`CFlag::CheckOnly`]. The caller restores its env on `Err`.
pub fn check_and_fuse<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    node: &mut Node<R, E>,
) -> Result<()> {
    let r = check_and_fuse_inner(ctx, flags, node);
    match &r {
        Ok(()) => ctx.apply_deferred(),
        Err(_) => ctx.drop_deferred(),
    }
    r
}

fn check_and_fuse_inner<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    node: &mut Node<R, E>,
) -> Result<()> {
    let st = Instant::now();
    let _level = typ::tvar::AtLevel::enter(typ::tvar::Level::TOP);
    let p = profile::phase(Phase::Typecheck0);
    if let Err(e) = node
        .typecheck0(ctx)
        .and_then(|()| drain_pending_settles(ctx))
        .and_then(|()| check_pending_names(ctx))
    {
        ctx.pending_settles.clear();
        ctx.pending_settles.push(Vec::new());
        ctx.pending_names.clear();
        return Err(e);
    }
    drop(p);
    if flags.contains(CFlag::CheckOnly) {
        return Ok(());
    }
    let p = profile::phase(Phase::Typecheck1);
    if let Err(e) = node.typecheck1(ctx) {
        ctx.pending_settles.clear();
        ctx.pending_settles.push(Vec::new());
        return Err(e);
    }
    drop(p);
    let p = profile::phase(Phase::Settle);
    drain_pending_settles(ctx)?;
    drop(p);
    info!("typecheck time {:?}", st.elapsed());
    analysis::analyze(node, ctx)?;
    ctx.env.seed_typedef_refs();
    if ctx.fusion.enabled {
        let st = Instant::now();
        let p = profile::phase(Phase::Fusion);
        let fused = fusion::TypeMemo::scope(|| fusion::fuse(node, ctx));
        ctx.fusion.link();
        drop(p);
        fused?;
        info!("fusion time {:?}", st.elapsed());
    }
    Ok(())
}

/// Drain the deferred terminal settles ([`ExecCtx::pending_settles`])
/// after a top-level statement, once every writer for the drained
/// sites has run.
pub(crate) fn drain_pending_settles<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
) -> Result<()> {
    use expr::At;
    let pending = mem::take(ctx.pending_settles.last_mut().expect("root settle frame"));
    PendingSettle::drain(&pending, &ctx.env, |spec, e| Err(e.at(&**spec)))
}

/// A written type name that did not resolve at its definition's check
/// must name something once the check ends.
pub(crate) fn check_pending_names<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
) -> Result<()> {
    use expr::At;
    for (tr, spec) in mem::take(&mut ctx.pending_names) {
        if !tr.names_something(&ctx.env) {
            let e = anyhow::Error::new(typ::UnresolvableRef {
                name: tr.name,
                scope: tr.scope,
            });
            return Err(e.at(&spec));
        }
    }
    Ok(())
}

/// Record the names `t`, written at `spec`, holds that do not resolve
/// yet ([`ExecCtx::pending_names`]).
pub(crate) fn defer_unresolved_names<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    t: &Type,
    spec: &Expr,
) {
    let mut names = Vec::new();
    t.unresolved_names(&ctx.env, &mut names);
    ctx.pending_names.extend(names.into_iter().map(|tr| (tr, spec.clone())));
}

/// A `use` whose name did not exist at its compile position must name
/// something by the end of the compile that deferred it.
pub(crate) fn check_pending_imports<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
) -> Result<()> {
    for p in mem::take(&mut ctx.pending_imports) {
        let Some(e) = ctx.env.names.get(&p.scope).and_then(|sn| sn.imports.get(&p.key))
        else {
            continue;
        };
        if !ctx.env.import_target_exists(e) {
            return Err(::anyhow::anyhow!(
                "use: no `{}` in `{}` (checked again after the enclosing \
                 statement finished compiling)",
                e.name,
                e.scope
            )
            .context(expr::ParserContext { ori: p.ori.clone(), pos: p.pos }));
        }
    }
    Ok(())
}

/// [`compile`] for drivers that compile top-level statements one at a
/// time (REPL, checker). A top-level `catch(e) expr` advances the scope
/// for the statements that follow: thread the returned scope into the
/// next compile, or later statements escape the catch's coverage.
pub fn compile_stmt<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<(Node<R, E>, Scope)> {
    compile_top(ctx, flags, spec, |ctx, spec, top_id| {
        let (n, scope) = node::compile_statement(
            ctx,
            flags,
            spec,
            scope,
            top_id,
            node::StmtAt::TopLevel,
        )?;
        node::defer_typedef_names(ctx, &n);
        Ok((n, scope))
    })
}

/// Compile a script's statements `exprs` as the one block it runs as,
/// with its names at `scope` itself: every statement is built before any
/// is checked, so a `let` takes its type from the writers below it.
/// `spec` is the block's expression.
pub fn compile_script<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
    exprs: &Arc<[Expr]>,
) -> Result<Node<R, E>> {
    compile_top(ctx, flags, spec, |ctx, spec, top_id| {
        let n =
            node::Block::compile(ctx, flags, spec.clone(), scope, top_id, false, exprs)?;
        Ok((n, scope.clone()))
    })
    .map(|(n, _)| n)
}

/// Every node's written span and type snapshot under the checked `nodes`,
/// pre-order, for tooling that asks for types (`Ide::expr_types`). Lambda
/// bodies are not visited (a body compiles per call site, so a site in it
/// has no one type) and a fused region is opaque: record from a check
/// with fusion off.
pub fn record_expr_types<R: Rt, E: UserEvent>(
    nodes: &[Node<R, E>],
    out: &mut Vec<ide::ExprTypeSite>,
) {
    for n in nodes {
        fusion::for_each_node(n, &mut |n| {
            let spec = n.spec();
            out.push(ide::ExprTypeSite {
                ori: spec.ori.clone(),
                pos: spec.pos,
                end: spec.end.get(),
                typ: n.typ().snapshot(),
                cell: matches!(n.typ(), typ::Type::TVar(_)),
            })
        });
    }
}

/// The registries a compile or a registration read writes, as they were
/// before it, so a failure puts them back.
pub(crate) struct Saved {
    env: Env,
    lambda_defs: TrackedMap<LambdaId, Value>,
    bind_to_lambda: TrackedMap<BindId, Value>,
    builtin_bindings: TrackedMap<(ModPath, CompactString), BuiltinBindInfo>,
    fn_forward_resolutions: TrackedMap<BindId, LambdaId>,
    connect_targets: TrackedSet<BindId>,
    batch_connect_targets: TrackedSet<BindId>,
    tags: TrackedSet<ArcStr>,
}

impl Saved {
    pub(crate) fn take<R: Rt, E: UserEvent>(ctx: &ExecCtx<R, E>) -> Self {
        Saved {
            env: ctx.env.clone(),
            lambda_defs: ctx.lambda_defs.clone(),
            bind_to_lambda: ctx.bind_to_lambda.clone(),
            builtin_bindings: ctx.builtin_bindings.clone(),
            fn_forward_resolutions: ctx.fn_forward_resolutions.clone(),
            connect_targets: ctx.connect_targets.clone(),
            batch_connect_targets: ctx.batch_connect_targets.clone(),
            tags: ctx.tags.clone(),
        }
    }

    pub(crate) fn restore<R: Rt, E: UserEvent>(self, ctx: &mut ExecCtx<R, E>) {
        let Saved {
            env,
            lambda_defs,
            bind_to_lambda,
            builtin_bindings,
            fn_forward_resolutions,
            connect_targets,
            batch_connect_targets,
            tags,
        } = self;
        ctx.env = env;
        ctx.lambda_defs = lambda_defs;
        ctx.bind_to_lambda = bind_to_lambda;
        ctx.builtin_bindings = builtin_bindings;
        ctx.fn_forward_resolutions = fn_forward_resolutions;
        ctx.connect_targets = connect_targets;
        ctx.batch_connect_targets = batch_connect_targets;
        ctx.tags = tags;
    }
}

/// Build the top-level node `spec` with `build`, then check and fuse it,
/// unwinding what it registered on failure.
fn compile_top<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    build: impl FnOnce(&mut ExecCtx<R, E>, &Expr, ExprId) -> Result<(Node<R, E>, Scope)>,
) -> Result<(Node<R, E>, Scope)> {
    let _profile = profile::phase(Phase::Compile);
    let _level = typ::tvar::AtLevel::enter(typ::tvar::Level::TOP);
    // A malformed input only de-fuses. The JIT helpers' wire ABI is
    // System V (a 16-byte `TagValue` is two
    // registers); Win64 passes it by hidden pointer, so on Windows the
    // graph is interpreted.
    ctx.fusion.enabled = !flags.contains(CFlag::FusionDisabled) && cfg!(not(windows));
    ctx.attr_census.lock().clear();
    ctx.attr_dispatched.lock().clear();
    ctx.attr_absorbed.lock().clear();
    ctx.pending_imports.clear();
    ctx.pending_names.clear();
    ctx.pending_settles.clear();
    ctx.pending_settles.push(Vec::new());
    let top_id = spec.id;
    ctx.fusion.top_id = Some(top_id);
    let saved = Saved::take(ctx);
    let st = Instant::now();
    let build_profile = profile::phase(Phase::BuildGraph);
    let compiled = build(ctx, &spec, top_id);
    drop(build_profile);
    let (mut node, out_scope) = match compiled {
        Ok(n) => n,
        Err(e) => {
            ctx.drop_deferred();
            saved.restore(ctx);
            return Err(e);
        }
    };
    info!("compile time {:?}", st.elapsed());
    if let Err(err) = check_pending_imports(ctx) {
        return Err(abandon_stmt(ctx, node, saved, err));
    }
    if let Err(e) = check_and_fuse(ctx, flags, &mut node) {
        return Err(abandon_stmt(ctx, node, saved, e));
    }
    // An attribute the fusion walk neither dispatched nor absorbed
    // would silently assert nothing; a check runs no fusion walk and
    // leaves the attributes to a build.
    if ctx.fusion.enabled && !flags.contains(CFlag::CheckOnly) {
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
                    );
                    let e = expr::At::at(e, spec);
                    drop(census);
                    drop(dispatched);
                    drop(absorbed);
                    return Err(abandon_stmt(ctx, node, saved, e));
                }
            }
        }
    }
    Ok((node, out_scope))
}

/// Unwind a statement that was built but failed its checks: delete what
/// it registered (runtime refs, lambda and `<-` target entries), then
/// restore the environment it started from.
fn abandon_stmt<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    mut node: Node<R, E>,
    saved: Saved,
    e: anyhow::Error,
) -> anyhow::Error {
    ctx.drop_deferred();
    node.delete(ctx);
    saved.restore(ctx);
    e
}
