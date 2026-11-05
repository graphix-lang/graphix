#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
#![recursion_limit = "256"]

// Both engines read a narrow scalar from a value word's low bytes.
#[cfg(target_endian = "big")]
compile_error!("graphix supports little-endian targets only");
pub use graphix_types::{
    AbstractTypeRegistry, BindId, CFlag, LambdaId, LambdaInstanceId, LibState, PrintFlag,
    SourcePosition, abstract_value, block_component, dbg_flag, defetyp, env, err, errf,
    expr, format_with_flags, ide, is_block_component, is_do_block, is_fn_block,
    is_lowered, is_method_block, list, mod_root, place_component, reference, rep,
    shared_map, tracked, typ, valarray_of,
};
pub(crate) use graphix_types::{
    AddrMap, AddrSet, CAST_ERR, CAST_ERR_TAG, Restore, profile, stack,
};

pub mod analysis;
pub mod branch;
pub mod cost;
pub(crate) mod dbgenv;
pub mod effects;
pub use effects::Effect;
pub mod fusion;
pub mod image;
pub mod node;
pub mod node_shape;
pub(crate) mod perfdbg;
pub mod prune;

pub use node::{WriteTarget, error::diagnostic_site, module::check_script_sig};
pub use stack::{
    Control, CtlFlag, ParMode, PruneMode, StopCause, WORKER_STACK, with_control,
};
pub use stack::{set_activation_budget, set_stack_budget};
pub mod tval;

use compact_str::{CompactString, format_compact};
use profile::Phase;
// Packages implementing `Apply::emit_clif` take cranelift through the
// compiler, so they stay in version lockstep with the JIT.
pub use cranelift_codegen;
pub use cranelift_frontend;
pub use fusion::FusionStats;
pub use tval::{Tag, TagValue, TagView};

use crate::{
    env::{Env, Lexical},
    expr::{ExprId, ModPath},
    fusion::emit::{BodyCx, CompiledExpr},
    node::{
        callsite::CallSite,
        lambda::{GXLambda, LambdaDef},
    },
    typ::{Bound, FnType, Indiscernible, Open, ReadCopies, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
pub use enumflags2::BitFlags;
use expr::{Attr, Expr};
use futures::channel::mpsc;
use log::{info, warn};
use netidx_value::{Abstract, ValArray, Value, abstract_type::AbstractWrapper};
use node::compiler;
use nohash::{IntMap, IntSet};
use parking_lot::Mutex;
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use smallvec::SmallVec;
use std::{
    any::Any,
    cell::Cell,
    collections::{VecDeque, hash_map::Entry},
    fmt::{Debug, Write as _},
    mem,
    sync::{
        self, LazyLock,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Duration,
};
use tokio::time::Instant;
use tracked::{TrackedMap, TrackedSet};
use triomphe::Arc;

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
    if !restore.prev() && enable {
        eprintln!("trace enabled at {}, spec: {}", spec.pos, spec);
    } else if restore.prev() && !enable {
        eprintln!("trace disabled at {}, spec: {}", spec.pos, spec);
    }
    let r = f();
    if let Err(e) = &r {
        eprintln!("traced at {} failed with {e:?}", spec.pos);
    }
    if restore.prev() && !enable {
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

pub trait CustomBuiltinType: Debug + Any + Send + Sync {}

impl CustomBuiltinType for Value {}
impl CustomBuiltinType for Option<Value> {}

/// Global pool of channel watch batches.
pub static CBATCH_POOL: LazyLock<Pool<Vec<(BindId, Box<dyn CustomBuiltinType>)>>> =
    LazyLock::new(|| Pool::new(10000, 1000));

/// How an update reads what stands. Nested views join ([`View::join`]).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum View {
    /// An ordinary cycle: only what was delivered fired.
    #[default]
    Cycle,
    /// A birth: standing values read as delivered and constants fire.
    Birth,
    /// A select arm's resume: a pause ends, so standing values read
    /// stale, constants stand, and a `<-` target keeps its value; only
    /// the fires the arm missed are delivered.
    Wake,
    /// Nodes born into a world that stands: constants fire, standing
    /// values read stale. An arm's first entry into an existing world,
    /// a seq step entered for a run, a birth inside a resume.
    Step,
}

impl View {
    /// Anything but an ordinary cycle: standing values are re-read.
    pub fn init(self) -> bool {
        self != View::Cycle
    }

    /// Standing values read stale, and a `<-` target keeps its value.
    pub fn wake(self) -> bool {
        matches!(self, View::Wake | View::Step)
    }

    /// Constants fire: something here is born.
    pub fn born(self) -> bool {
        matches!(self, View::Birth | View::Step)
    }

    /// `inner` nested in `self`: each fact holds if either holds it.
    pub fn join(self, inner: View) -> View {
        match (self.born() || inner.born(), self.wake() || inner.wake()) {
            (false, false) => View::Cycle,
            (true, false) => View::Birth,
            (false, true) => View::Wake,
            (true, true) => View::Step,
        }
    }
}

/// Whom a binding's fire schedules besides the readers the walk in
/// progress reaches ([`ExecCtx::deliver`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Notify {
    /// The walk's readers alone: a binding its own root reads.
    Walk,
    /// Also the roots that read the binding, which update this cycle.
    Roots,
}

/// Everything that happened simultaneously in one execution cycle. At
/// most one update per variable per cycle; further updates are queued
/// for later cycles.
#[derive(Debug)]
pub struct Event {
    pub view: View,
    /// The overlay: this cycle's transient deliveries. Not the value
    /// store: reads fall through to [`Rt::store_get`].
    pub variables: branch::Layered<TagValue>,
    /// The cycle's custom deliveries, each taken by its consumer; one
    /// map for every branch of the cycle.
    pub custom: Arc<Mutex<IntMap<BindId, Box<dyn CustomBuiltinType>>>>,
    /// The root the runtime is updating.
    pub root: Option<ExprId>,
    /// Last cycle's fires each delivered now to the readers the walk had
    /// passed when it was made ([`Rt::late`]).
    pub late: Option<Arc<Late>>,
}

/// Fires delivered a cycle late, each to its readers alone.
pub type Late = IntMap<BindId, (TagValue, SmallVec<[prune::Reader; 1]>)>;

impl Event {
    pub fn new() -> Self {
        Event {
            view: View::Cycle,
            variables: branch::Layered::default(),
            custom: Arc::new(Mutex::new(IntMap::default())),
            root: None,
            late: None,
        }
    }

    /// Anything but an ordinary cycle: standing values are re-read,
    /// as delivered at a birth and stale at a wake or a step.
    pub fn init(&self) -> bool {
        self.view.init()
    }

    pub fn wake(&self) -> bool {
        self.view.wake()
    }

    /// Constants fire: something here is born.
    pub fn born(&self) -> bool {
        self.view.born()
    }

    /// Take the cycle's custom delivery for `id`.
    pub fn take_custom(&self, id: &BindId) -> Option<Box<dyn CustomBuiltinType>> {
        self.custom.lock().remove(id)
    }

    /// Run `f` on the cycle's custom delivery for `id`, left in place.
    pub fn with_custom<T>(
        &self,
        id: &BindId,
        f: impl FnOnce(&dyn CustomBuiltinType) -> Option<T>,
    ) -> Option<T> {
        self.custom.lock().get(id).and_then(|c| f(&**c))
    }

    /// The event a branch forked from this one runs over.
    ///
    /// # Safety
    ///
    /// The parent is read through a pointer whose lifetime is erased: it
    /// must outlive this branch and must not be used, by its owner or
    /// another branch's merge, until this branch has merged back.
    pub(crate) unsafe fn fork(&self) -> Self {
        Event {
            view: self.view,
            // SAFETY: the caller's contract
            variables: unsafe { self.variables.fork() },
            custom: self.custom.clone(),
            root: self.root,
            late: self.late.clone(),
        }
    }

    /// Apply what the forked branch's event `child` delivered.
    pub(crate) fn merge(&mut self, child: Self) {
        let Self { view: _, variables, custom: _, root: _, late: _ } = child;
        self.variables.merge(variables);
    }

    pub fn clear(&mut self) {
        let Self { view, variables, custom, root, late } = self;
        *view = View::Cycle;
        variables.clear();
        custom.lock().clear();
        *root = None;
        *late = None;
    }
}

#[derive(Debug, Clone, Default)]
pub struct Refs {
    refed: LPooled<IntSet<BindId>>,
    /// The refs a fire can reach the collected node through: `refed`
    /// minus those read only under a sample's right side. Only a walk of
    /// freshly compiled nodes reads it, so an imaged site adds none.
    triggering: LPooled<IntSet<BindId>>,
    bound: LPooled<IntSet<BindId>>,
    banked: usize,
    /// Leave callee instance bodies out of the walk.
    skip_callees: bool,
    /// Stop at each lambda callee, recording its instance, instead of
    /// walking its body.
    callees: Option<Vec<LambdaInstanceId>>,
    /// Collect the static holds of the nodes walked ([`StaticHold`]).
    static_holds: Option<Vec<(BindId, Arc<expr::Expr>)>>,
    /// Collect each static hold of the nodes walked once, by its address
    /// ([`Refs::holders`]).
    holders: Option<AHashMap<usize, (BindId, ExprId)>>,
}

impl Refs {
    /// A walk of one body alone: callee instance bodies are left out.
    pub(crate) fn without_callees() -> Self {
        Self { skip_callees: true, ..Self::default() }
    }

    /// A walk of one body that stops at its lambda callees
    /// ([`Refs::take_callees`]), collecting its static holds
    /// ([`Refs::take_static_holds`]).
    pub(crate) fn stopping_at_callees() -> Self {
        Self {
            callees: Some(Vec::new()),
            static_holds: Some(Vec::new()),
            ..Self::default()
        }
    }

    /// A walk of a whole tree, callee bodies included, that collects its
    /// static holds ([`Refs::take_static_holds`]).
    pub(crate) fn holding() -> Self {
        Self { static_holds: Some(Vec::new()), ..Self::default() }
    }

    pub(crate) fn take_callees(&mut self) -> Vec<LambdaInstanceId> {
        self.callees.take().unwrap_or_default()
    }

    pub(crate) fn take_static_holds(&mut self) -> Vec<(BindId, Arc<expr::Expr>)> {
        self.static_holds.take().unwrap_or_default()
    }

    /// A walk of a whole tree, callee bodies included, that collects each
    /// static hold it holds once ([`Refs::take_holders`]).
    pub(crate) fn holders() -> Self {
        Self { holders: Some(AHashMap::default()), ..Self::default() }
    }

    /// The binding and the call of each hold [`Refs::holders`] collected.
    pub(crate) fn take_holders(&mut self) -> Vec<(BindId, ExprId)> {
        self.holders.take().map(|h| h.into_values().collect()).unwrap_or_default()
    }

    /// Record the static holds of a walked node.
    pub(crate) fn static_holds<'a>(
        &mut self,
        holds: impl IntoIterator<Item = &'a StaticHold>,
    ) {
        match (&mut self.static_holds, &mut self.holders) {
            (Some(out), _) => {
                out.extend(holds.into_iter().map(|h| (h.id, h.call.clone())))
            }
            (None, Some(out)) => out.extend(
                holds
                    .into_iter()
                    .map(|h| (h as *const StaticHold as usize, (h.id, h.call.id))),
            ),
            (None, None) => (),
        }
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
    /// The lambda definition that wrote the entry: its deletion forgets it.
    pub lambda_id: Option<LambdaId>,
}

/// One source binding's entry: every instance of it describes it alike,
/// its types told apart only by instance identity.
impl graphix_types::tracked::SameValue for BuiltinBindInfo {
    fn same_value(&self, other: &Self) -> bool {
        self.name == other.name && triomphe::Arc::ptr_eq(&self.argspec, &other.argspec)
    }
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
pub struct Node<R: Rt>(std::mem::ManuallyDrop<Box<dyn Update<R>>>);

/// `ManuallyDrop` moves the recursive teardown inside the stack guard;
/// field glue would run after `drop` returns.
impl<R: Rt> Drop for Node<R> {
    fn drop(&mut self) {
        stack::ensure_sufficient(|| unsafe { std::mem::ManuallyDrop::drop(&mut self.0) })
    }
}

impl<R: Rt> Node<R> {
    pub fn new<T: Update<R> + 'static>(node: T) -> Self {
        Self(std::mem::ManuallyDrop::new(Box::new(node)))
    }

    #[inline]
    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        stack::ensure_sufficient(|| self.0.update(ctx))
    }

    #[inline]
    pub fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        stack::ensure_sufficient(|| self.0.delete(ctx))
    }

    #[inline]
    pub fn unregister(&mut self, ctx: &mut ExecCtx<'_, R>) {
        stack::ensure_sufficient(|| self.0.unregister(ctx))
    }

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0(ctx))
    }

    pub fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut node::lambda::InstanceTypes,
    ) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0_instance(ctx, types))
    }

    pub fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck1(ctx))
    }

    pub fn refs(&self, refs: &mut Refs) {
        stack::ensure_sufficient(|| self.0.refs(refs))
    }

    #[inline]
    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        stack::ensure_sufficient(|| self.0.sleep(ctx))
    }

    pub fn emit_clif(&self, cx: &mut BodyCx) -> Result<fusion::emit::CompiledExpr> {
        stack::ensure_sufficient(|| self.0.emit_clif(cx))
    }

    pub fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        stack::ensure_sufficient(|| self.0.fuse(ctx))
    }

    pub(crate) fn as_prune(&self) -> Option<&node::prune::Prune<R>> {
        (&**self.0 as &dyn Any).downcast_ref::<node::prune::Prune<R>>()
    }

    /// The node, if it is a pruning boundary.
    pub(crate) fn as_prune_mut(&mut self) -> Option<&mut node::prune::Prune<R>> {
        (&mut **self.0 as &mut dyn Any).downcast_mut::<node::prune::Prune<R>>()
    }

    /// The node as a `T`, seeing through a pruning boundary.
    pub(crate) fn downcast_ref<T: Update<R>>(&self) -> Option<&T> {
        let node: &dyn Any = &**self.0;
        match node.downcast_ref::<node::prune::Prune<R>>() {
            Some(p) => p.child().downcast_ref::<T>(),
            None => node.downcast_ref::<T>(),
        }
    }

    /// The node as a `T`, seeing through a pruning boundary.
    pub(crate) fn downcast_mut<T: Update<R>>(&mut self) -> Option<&mut T> {
        let node: &mut dyn Any = &mut **self.0;
        if node.is::<node::prune::Prune<R>>() {
            let p = node.downcast_mut::<node::prune::Prune<R>>().expect("checked");
            return p.child_mut().downcast_mut::<T>();
        }
        node.downcast_mut::<T>()
    }

    pub fn image_encode(
        &self,
        buf: &mut image::ImageBuf,
    ) -> std::result::Result<(), netidx_core::pack::PackError> {
        stack::ensure_sufficient(|| self.0.image_encode(buf))
    }
}

impl<R: Rt> Debug for Node<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<R: Rt> std::ops::Deref for Node<R> {
    type Target = dyn Update<R>;

    fn deref(&self) -> &Self::Target {
        &**self.0
    }
}

impl<R: Rt> std::ops::DerefMut for Node<R> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut **self.0
    }
}

#[derive(Clone, Copy)]
pub enum BindMode<'a> {
    Definition,
    Dynamic(&'a FnType),
    Static { instance: &'a FnType },
}

pub type InitFn<R> = sync::Arc<
    dyn for<'a, 'b, 'c, 'd> Fn(
            &'a Scope,
            &'b mut CompileCtx<R>,
            &'c mut [Node<R>],
            BindMode<'d>,
            ExprId,
        ) -> Result<Box<dyn Apply<R>>>
        + Send
        + Sync
        + 'static,
>;

/// A function application. Its arguments are owned by the `CallSite`,
/// so the function can change at runtime without recompiling them.
pub trait Apply<R: Rt>: Debug + Send + Sync + Any {
    /// Typed view for analysis code. The default is `Opaque`.
    fn view(&self) -> ApplyView<'_, R> {
        ApplyView::Opaque
    }

    /// Same borrowed-production contract as [`Update::update`]: the
    /// returned `&TagValue` is the builtin's resident result slot.
    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue;

    /// Delete any internally generated nodes.
    /// Each node of the body, when the application has one.
    fn for_each_body_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        let _ = f;
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        ()
    }

    /// First typecheck pass: the call's arguments against the
    /// lambda's own FnType.
    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        Ok(())
    }

    /// Second typecheck pass.
    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
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

    /// Pause the builtin (an unselected arm): it keeps its state and its
    /// output, and aborts any operation in flight, which it does not
    /// restart unless an argument fires.
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>);

    /// Start afresh: the update after a sleep is a birth (a seq step
    /// entered for its next run), so the builtin forgets what its last
    /// run left, as a new one would hold nothing.
    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    /// Emit this call site into the open JIT kernel as CLIF.
    /// `Ok(Some(cv))`: emitted. `Ok(None)`: shape not handled, and no
    /// instructions may have been emitted. `Err`: abort the kernel
    /// build (partial emission is fine). A builtin's site reaches this
    /// only when its [`Effect::Stateless`] carries a [`FastCall`];
    /// discovery de-fuses the region before emission otherwise.
    fn emit_clif(
        &self,
        _callsite: &CallSite<R>,
        _cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        Ok(None)
    }

    /// Fuse inside this apply when its call site did not fuse
    /// (`GXLambda` fuses its instance body's sync regions in place).
    /// Build errors must be swallowed, never fail the compile.
    fn fuse(&mut self, _ctx: &mut CompileCtx<R>) -> Result<()> {
        Ok(())
    }
}

/// Typed view of an [`Apply`], symmetric to [`NodeView`]: a Graphix
/// lambda with a walkable body, or an opaque builtin.
pub enum ApplyView<'a, R: Rt> {
    Lambda(&'a GXLambda<R>),
    /// A builtin, by its registered name.
    BuiltIn(&'a str),
    /// An application nothing names (a builtin below the wrapper that
    /// names it, a runtime-made apply): analysis assumes it may do
    /// anything, an ordered builtin's call included.
    Opaque,
}

/// Exhaustive typed view of the compiled node graph, one variant per
/// concrete `Update` impl, for compile-time analysis. No catch-all:
/// a new node type must pick a variant, and a new variant must be
/// handled by every exhaustive match.
#[allow(missing_docs)]
pub enum NodeView<'a, R: Rt> {
    Bind(&'a node::bind::Bind<R>),
    Lambda(&'a node::lambda::Lambda),
    Block(&'a node::Block<R>),
    Module(&'a node::module::Module<R>),
    CallSite(&'a CallSite<R>),
    MapQ(&'a node::collection::MapQBase<R>),
    FoldQ(&'a node::collection::FoldQBase<R>),
    Select(&'a node::select::Select<R>),
    Catch(&'a node::error::Catch<R>),
    SeqGuard(&'a node::error::SeqGuard<R>),
    SeqAbort(&'a node::error::SeqAbort<R>),
    SeqMachine(&'a node::seq_machine::SeqMachine<R>),
    SeqCapture(&'a node::seq_machine::SeqCapture<R>),
    Qop(&'a node::error::Qop<R>),
    ExplicitParens(&'a node::ExplicitParens<R>),
    ForkControl(&'a node::fork_control::ForkControl<R>),
    TypeCast(&'a node::TypeCast<R>),
    Connect(&'a node::Connect<R>),
    ConnectDeref(&'a node::ConnectDeref<R>),
    StringInterpolate(&'a node::StringInterpolate<R>),
    Any(&'a node::Any<R>),
    Never(&'a node::Never<R>),
    Sample(&'a node::Sample<R>),
    Struct(&'a node::data::Struct<R>),
    StructWith(&'a node::data::StructWith<R>),
    Tuple(&'a node::data::Tuple<R>),
    Variant(&'a node::data::Variant<R>),
    Construct(&'a node::data::Construct<R>),
    Array(&'a node::array::Array<R>),
    ListLit(&'a node::array::ListLit<R>),
    Map(&'a node::map::Map<R>),
    StructRef(&'a node::data::StructRef<R>),
    TupleRef(&'a node::data::TupleRef<R>),
    ArrayRef(&'a node::array::ArrayRef<R>),
    ArraySlice(&'a node::array::ArraySlice<R>),
    MapRef(&'a node::map::MapRef<R>),
    Ref(&'a node::bind::Ref),
    ByRef(&'a node::bind::ByRef<R>),
    Deref(&'a node::bind::Deref<R>),
    Add(&'a node::op::Add<R>),
    Sub(&'a node::op::Sub<R>),
    Mul(&'a node::op::Mul<R>),
    Div(&'a node::op::Div<R>),
    Mod(&'a node::op::Mod<R>),
    CheckedAdd(&'a node::op::CheckedAdd<R>),
    CheckedSub(&'a node::op::CheckedSub<R>),
    CheckedMul(&'a node::op::CheckedMul<R>),
    CheckedDiv(&'a node::op::CheckedDiv<R>),
    CheckedMod(&'a node::op::CheckedMod<R>),
    Eq(&'a node::op::Eq<R>),
    Ne(&'a node::op::Ne<R>),
    Lt(&'a node::op::Lt<R>),
    Gt(&'a node::op::Gt<R>),
    Lte(&'a node::op::Lte<R>),
    Gte(&'a node::op::Gte<R>),
    And(&'a node::op::And<R>),
    Or(&'a node::op::Or<R>),
    Not(&'a node::op::Unary<R>),
    Neg(&'a node::op::Unary<R>),
    Constant(&'a node::Constant),
    TypeDef(&'a node::TypeDef),
    Impl(&'a node::traits::Impl<R>),
    Nop(&'a node::Nop),
    FusedKernel(&'a fusion::FusedKernel<R>),
}

/// A regular graph node, as opposed to a function application (Apply).
pub trait Update<R: Rt>: Debug + Send + Sync + Any + 'static {
    /// Update the node with the event and return its production,
    /// borrowed from the node's own resident slot. Every awake node
    /// delivers every cycle; a quiet cycle rides the resident. See
    /// [`TagView`].
    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue;

    /// Every child node, the one child enumeration: a callee's instance
    /// and a lambda's body are not children (each is per call site).
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>));

    /// [`Self::for_each_child`], mutably.
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>));

    /// Delete the node and its children from the context.
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.for_each_child_mut(&mut |c| c.delete(ctx))
    }

    /// Undo what this node registered in the global registries, leaving
    /// the runtime as it is. A module's reload unregisters the body it
    /// replaces so the new source registers afresh; what a node
    /// registered is its own to give back, so its later `delete` finds
    /// nothing of its own left and leaves the replacement alone.
    fn unregister(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.for_each_child_mut(&mut |c| c.unregister(ctx))
    }

    /// First typecheck pass: structural checking. Each node checks
    /// itself and recurses into its children.
    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let mut res = Ok(());
        self.for_each_child_mut(&mut |c| {
            if res.is_ok() {
                res = wrap!(c, c.typecheck0(ctx))
            }
        });
        res
    }

    /// `typecheck0` for a node of an instance, whose definition's check
    /// settled its types ([`node::lambda::InstanceTypes`]): an impl takes
    /// its types from there and does only the part of `typecheck0` that
    /// is state.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut node::lambda::InstanceTypes,
    ) -> Result<()> {
        let mut res = Ok(());
        self.for_each_child_mut(&mut |c| {
            if res.is_ok() {
                res = wrap!(c, c.typecheck0_instance(ctx, types))
            }
        });
        res
    }

    /// Second typecheck pass, after `typecheck0` finished the whole
    /// tree: `lambda_ids` are final, so call sites can resolve
    /// statically.
    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let mut res = Ok(());
        self.for_each_child_mut(&mut |c| {
            if res.is_ok() {
                res = wrap!(c, c.typecheck1(ctx))
            }
        });
        res
    }

    /// The node's type.
    fn typ(&self) -> &Type;

    /// Record every bind id referenced or bound by the node and its
    /// children.
    fn refs(&self, refs: &mut Refs) {
        self.for_each_child(&mut |c| c.refs(refs))
    }

    /// The expression this node was compiled from.
    fn spec(&self) -> &Expr;

    /// Pause the node (an unselected arm).
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.for_each_child_mut(&mut |c| c.sleep(ctx))
    }

    /// The node's typed view for compile-time analysis.
    fn view(&self) -> NodeView<'_, R>;

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
        Err(fusion::blocker(
            self.spec(),
            format_compact!("the node-walk computes `{}`", self.spec()),
        ))
    }

    /// Fuse this subtree. `Ok(Some(replacement))`: the caller swaps it
    /// in and deletes the old node. `Ok(None)`: the impl already
    /// recursed into its children via [`fusion::fuse`]. The default is
    /// `Ok(None)` with no recursion; only the containers fusion
    /// descends through override it.
    fn fuse(&mut self, _ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        Ok(None)
    }
}

/// Decode a builtin's application from its image over the restored
/// argument references.
pub type BuiltInDecodeFn<R> =
    fn(
        &mut ExecCtx<'_, R>,
        &[Node<R>],
        &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R>>, netidx_core::pack::PackError>;

pub type BuiltInInitFn<R> = for<'a, 'b, 'c, 'd> fn(
    &'a mut CompileCtx<R>,
    &'a FnType,
    Option<&'d FnType>,
    &'b Scope,
    &'c [Node<R>],
    ExprId,
) -> Result<Box<dyn Apply<R>>>;

/// A builtin's direct-call entry (see [`FastCall`]): a pure function
/// over present argument values; `None` is bottom this cycle.
pub type FastFn = fn(&[Value]) -> Option<Value>;

/// [`FastFn`] plus the call site's resolved function type and its env,
/// for a builtin directed by its argument or return types.
pub type TypedFastFn = fn(&env::Env, &FnType, &[Value]) -> Option<Value>;

/// The direct-call entry of a [`Effect::Stateless`] builtin: the JIT
/// calls it at every fused site with the present argument values (a
/// tainted argument bottoms the call without invoking it). It may
/// re-evaluate whenever any region input fires, so keep it cheap.
/// `eval` should delegate to the same fn (`graphix_package_core::
/// fast_eval`). `Typed` also receives the site's resolved function type.
#[derive(Debug, Clone, Copy)]
pub enum FastCall {
    Plain(FastFn),
    Typed(TypedFastFn),
}

/// A fused [`Effect::Io`] call site: what [`BuiltIn::perform`] reads
/// besides the call's arguments.
pub struct IoSite {
    /// The builtin's registered name.
    pub builtin: ArcStr,
    /// The trigger argument as written.
    pub arg: Expr,
    typ: fusion::emit::KernelType,
}

impl IoSite {
    pub(crate) fn new(builtin: ArcStr, arg: Expr, typ: Type) -> Self {
        Self { builtin, arg, typ: fusion::emit::KernelType::new(typ) }
    }

    /// The trigger argument's type.
    pub fn typ(&self) -> &Type {
        &self.typ.typ
    }
}

pub type BuiltInPerformFn<R> = fn(&mut ExecCtx<'_, R>, &IoSite, &[Value]);

/// A Graphix builtin implemented in Rust.
pub trait BuiltIn<R: Rt> {
    /// The builtin's name, `package::unique_name` for a package builtin.
    const NAME: &str;
    /// The builtin's classification; see [`Effect`]. Default `Async`.
    const EFFECT: Effect = Effect::Async;
    /// Whether the builtin shares state with other nodes whose order of
    /// access decides values (`queuefn`'s queue): two subtrees that both
    /// call an ordered builtin never run beside each other
    /// (`design/parallel_eval.md` §6). External effects are not ordered.
    const ORDERED: bool = false;
    /// Argument whose ordering needs a value walk when its type can hold references.
    const ORDER_ARG: Option<usize> = None;
    /// An event source whose standing interest re-attaches or resumes
    /// at its arm's wake (a subscription, a timer, a watch, a server):
    /// over constant arguments it still acts at every entry.
    const RESUMES: bool = false;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a FnType,
        resolved_type: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>>;

    /// Restore an application `image_encode` wrote, over the restored
    /// argument references (`from`, as `init` saw them).
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        from: &[Node<R>],
        buf: &mut &[u8],
    ) -> std::result::Result<Box<dyn Apply<R>>, netidx_core::pack::PackError>;

    /// An [`Effect::Io`] builtin's effect at a fused call: the kernel
    /// queued the arguments when the trigger fired, and its wrapper
    /// performs the call after the kernel returns, as the node-walk's
    /// application would have.
    fn perform(_ctx: &mut ExecCtx<'_, R>, _site: &IoSite, _args: &[Value]) {
        unreachable!("{} is not an Effect::Io builtin", Self::NAME)
    }
}

/// A compile-time check for a `#[..]` attribute, dispatched by the
/// fusion walk on the final (possibly fused) node of the decorated
/// expression; under `--no-fusion` it never runs. `Err` is a compile
/// error. Registered via [`ExecCtx::register_attribute`]. The
/// definition assertions (`#[tail_recursive]`/`#[sync]`/`#[async]`)
/// are compiler-reserved, not registry attributes.
pub type AttributeCheckFn<R> = fn(&CompileCtx<R>, &Attr, &Node<R>) -> Result<()>;

/// What an attribute demands of its target whatever fusion made of it:
/// it also runs on a node absorbed into a larger kernel, where
/// [`AttributeCheckFn`] never does.
pub type AttributeTargetFn<R> = fn(&Attr, &Node<R>) -> Result<()>;

/// A definition assertion (`#[tail_recursive]` / `#[sync]` /
/// `#[async]` / `#[pure]`), verified at the tail of `analysis::analyze` once the
/// definition is reached; until then it stays pending.
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
pub enum DefAssertionKind {
    Sync,
    Async,
    TailRecursive,
    Pure,
    /// A `#[tail_recursive]` whose definition passed its check: each
    /// instance built after it still owes its loop.
    TailLoop,
}

impl DefAssertionKind {
    const ALL: [Self; 4] = [Self::Sync, Self::Async, Self::TailRecursive, Self::Pure];

    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Sync => "sync",
            Self::Async => "async",
            Self::TailRecursive | Self::TailLoop => "tail_recursive",
            Self::Pure => "pure",
        }
    }

    pub(crate) fn from_name(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|k| k.name() == name)
    }
}

#[derive(Debug, Clone, netidx_derive::Pack)]
#[pack(unwrapped)]
pub(crate) struct DefAssertion {
    pub(crate) id: LambdaId,
    pub(crate) kind: DefAssertionKind,
    /// The decorated statement, for error positions.
    pub(crate) spec: Expr,
    /// The compile's flags, for a warning the check makes.
    pub(crate) flags: BitFlags<CFlag>,
    /// It gave the diagnostic it gives once: a `#[pure]` that holds only
    /// diagnostic output warned of it, a `TailLoop` reported an instance a
    /// run-time bind built without its loop.
    pub(crate) said: bool,
}

/// Trait implemented by graphix attributes (`#[name]`; arguments are refused).
pub trait Attribute<R: Rt> {
    /// The bare attribute name (`native` for `#[native]`); a flat
    /// global namespace.
    const NAME: &str;
    fn check(ctx: &CompileCtx<R>, attr: &Attr, node: &Node<R>) -> Result<()>;
    /// See [`AttributeTargetFn`].
    fn check_target(_attr: &Attr, _node: &Node<R>) -> Result<()> {
        Ok(())
    }
}

/// `#[native]`: the decorated expression must compile to native code
/// with zero node-walk residue. A function-typed target is rejected;
/// the requirement belongs at the use site.
pub struct Native;

impl<R: Rt> Attribute<R> for Native {
    const NAME: &str = "native";

    fn check_target(_attr: &Attr, node: &Node<R>) -> Result<()> {
        if let Type::Fn(_) = node.typ() {
            crate::bailat!(
                node.spec(),
                "#[native] annotates a computation or a call, not a function — \
                 put it on the call site, not the definition"
            );
        }
        if !matches!(node.view(), NodeView::FusedKernel(_))
            && !fusion::region_is_candidate(node)
            && !fusion::is_constant(node)
        {
            crate::bailat!(
                node.spec(),
                "#[native] annotates a computation; a declaration or a bare \
                 variable read has nothing to fuse — put it on the initializer"
            );
        }
        Ok(())
    }

    fn check(ctx: &CompileCtx<R>, attr: &Attr, node: &Node<R>) -> Result<()> {
        <Self as Attribute<R>>::check_target(attr, node)?;
        // a literal is its value: nothing to fuse and nothing node-walks
        if fusion::is_constant(node) {
            return Ok(());
        }
        let mut report: Vec<&fusion::FusionFailure> = Vec::new();
        let mut walked: Vec<&Expr> = Vec::new();
        native_residue(ctx, node, &mut report, &mut walked);
        if crate::dbgenv::gxdbg_native_all() {
            for failure in ctx.fusion.stats.failed.iter() {
                eprintln!("NATIVE-ALL {:?}: {}", failure.id, failure.reason);
            }
        }
        if report.is_empty() && walked.is_empty() {
            return Ok(());
        }
        let mut reasons = CompactString::new("");
        {
            use std::fmt::Write;
            for failure in &report {
                let _ = write!(reasons, "\n  - {}", failure.reason);
            }
            for spec in &walked {
                let _ = write!(reasons, "\n  - the node-walk computes `{spec}`");
            }
        }
        crate::bailat!(
            node.spec(),
            "#[native] expression did not fully fuse to native code:{reasons}"
        );
    }
}

/// The failures behind the node-walk residue under `node`, into `report`;
/// the residue that has none recorded, into `walked`. A kernel's feeder that
/// is more than a variable read is an argument the node-walk computes for it.
fn native_residue<'a, 'n, R: Rt>(
    ctx: &'a CompileCtx<R>,
    node: &'n Node<R>,
    report: &mut Vec<&'a fusion::FusionFailure>,
    walked: &mut Vec<&'n Expr>,
) {
    if let NodeView::FusedKernel(k) = node.view() {
        for feeder in k.feeders() {
            if !matches!(feeder.view(), NodeView::Ref(_)) {
                native_residue(ctx, feeder, report, walked)
            }
        }
        return;
    }
    // Report only the leaf-most failures: `try_fuse` records a failure
    // for every region root it tries, containers included. One whose
    // parts fused is residue for its own reason.
    let before = report.len();
    fusion::for_each_node(node, &mut |n| {
        let mut failures = ctx.fusion.stats.failures_for_source(n.spec()).peekable();
        if failures.peek().is_none() {
            return;
        }
        let mut has_failed_desc = false;
        let mut root = true;
        fusion::for_each_node(n, &mut |desc| {
            if root {
                root = false;
                return;
            }
            has_failed_desc |=
                ctx.fusion.stats.failures_for_source(desc.spec()).next().is_some();
        });
        if has_failed_desc {
            return;
        }
        for failure in failures {
            if !report.iter().any(|p| p.id == failure.id && p.reason == failure.reason) {
                report.push(failure);
            }
        }
    });
    if report.len() == before {
        walked.push(node.spec())
    }
}

/// The log target of a running program's failures: an error nothing
/// handles, a hot operator's failure, a definition assertion a run-time
/// bind breaks. An embedder shows or routes them through its logger.
pub const FAILURE_TARGET: &str = "graphix::failure";

pub trait Rt: Debug + Any + Send + Sync {
    /// Called whenever a bound variable (or lambda) is referenced; `by`
    /// names the toplevel expression containing the reference, which must
    /// be updated when the variable changes, and the boundary around it,
    /// which a delivery marks (`design/walk_pruning.md`).
    fn ref_var(&mut self, id: BindId, by: prune::Reader);
    fn unref_var(&mut self, id: BindId, by: prune::Reader);

    /// Each reader `id` has.
    fn readers(&self, id: &BindId, f: &mut dyn FnMut(&prune::Reader));

    /// Queue a variable write for the next cycle. Writes to distinct
    /// variables are delivered in one event; a second write to the same
    /// variable waits a cycle. The event must not change mid-cycle.
    fn set_var(&mut self, id: BindId, value: Value);
    /// The writes to `id` queued and not delivered yet.
    fn waiting_writes(&self, id: &BindId) -> usize;
    /// [`Self::set_var`] for a level whose intermediate values nobody
    /// needs: it replaces the variable's last waiting level write, so the
    /// changes a cycle makes cost one delivery.
    fn set_level(&mut self, id: BindId, value: Value);
    /// Queue a write through a path: at delivery the variable's value as
    /// it then stands is rebuilt along `path` with `value` at the end.
    /// Deferred exactly like `set_var`.
    fn patch_var(
        &mut self,
        id: BindId,
        path: node::place::Path,
        value: Value,
        writer: Option<Arc<Expr>>,
    );
    /// [`Self::patch_var`] for a level kept at a place: it replaces the
    /// variable's last waiting level write when that patches the same
    /// path, as [`Self::set_level`] does for the whole variable.
    fn patch_level(&mut self, id: BindId, path: node::place::Path, value: Value);
    /// Register the place a reference cell stands for (`&root[i].f`), so
    /// `*r` reads through the root and `*r <- v` patches it.
    fn set_ref_path(&mut self, cell: BindId, to: node::place::RefPath);
    fn ref_path(&self, cell: &BindId) -> Option<&node::place::RefPath>;
    fn clear_ref_path(&mut self, cell: &BindId);

    /// The persistent store: the (production, cycle stamp) of a bound
    /// variable's last delivery. `stamp == cycle()` reads as delivered
    /// this cycle, an older stamp as standing (Stale; Fired under an
    /// init view), absence as the phantom. Maintained at delivery, never
    /// ahead of it.
    fn store_get(&self, id: &BindId) -> Option<&(TagValue, u64)>;

    /// The last delivered value of a bind; `None` if the last delivery
    /// was a bottom.
    fn store_value(&self, id: &BindId) -> Option<Value> {
        branch::stored_value(self.store_get(id))
    }

    /// Insert a production into the store, stamped with the current
    /// cycle.
    fn store_insert(&mut self, id: BindId, tv: TagValue);

    /// Remove a bind from the store.
    fn store_remove(&mut self, id: &BindId);

    /// Drop the writes waiting for `id`: its binding is gone, and a write
    /// that landed now would leave a store entry nothing removes.
    fn forget_writes(&mut self, id: BindId);

    /// Insert a standing entry: no delivery, so it keeps the stamp of
    /// the last one, and a same-cycle reader never sees it as delivered
    /// ([`branch::standing_stamp`]).
    fn store_insert_standing(&mut self, id: BindId, tv: TagValue) {
        let stamp = branch::standing_stamp(self.store_get(&id), self.cycle());
        self.store_put(id, Some((tv, stamp)))
    }

    /// Put back an entry [`Self::store_get`] read, stamp and all, or
    /// remove it when it was absent.
    fn store_put(&mut self, id: BindId, entry: Option<(TagValue, u64)>);

    /// The current cycle number.
    fn cycle(&self) -> u64;

    /// A variable was set within the current cycle; dependent toplevel
    /// nodes must be updated.
    fn notify_set(&mut self, id: BindId);

    /// `reader` meets this cycle's fire `tv` of `id` after the walk passed
    /// it: deliver the fire to it alone next cycle ([`Event::late`]).
    fn late(&mut self, id: BindId, tv: TagValue, reader: prune::Reader);

    /// The root being updated fired `id`, which a reader in another root
    /// reads: one in a root the cycle passed reads it next cycle
    /// ([`Self::late`]), one in a later root this cycle.
    fn produced(&mut self, id: BindId);

    /// Deliver a variable event for `id` carrying the current time after
    /// `timeout`.
    fn set_timer(&mut self, id: BindId, timeout: Duration);

    /// Stop the timer `set_timer` started for `id`, if it has not fired.
    fn cancel_timer(&mut self, id: BindId);

    /// Spawn a task whose output is delivered as a custom event for the
    /// returned `BindId`.
    fn spawn<F: Future<Output = (BindId, Box<dyn CustomBuiltinType>)> + Send + 'static>(
        &mut self,
        f: F,
    );

    /// Spawn a task whose output is delivered as a variable event for
    /// the returned `BindId`.
    fn spawn_var<F: Future<Output = (BindId, Value)> + Send + 'static>(&mut self, f: F);

    /// Deliver batches arriving on the channel as custom updates.
    fn watch(
        &mut self,
        s: mpsc::Receiver<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
    );

    /// Deliver batches arriving on the channel as variable updates.
    fn watch_var(&mut self, s: mpsc::Receiver<GPooled<Vec<(BindId, Value)>>>);
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

/// The active instantiations of every def, innermost first: a site
/// inside `h(k)` may reach a still-resolving `h(g)`. Persistent, so a
/// compile task's fork shares its parent's in O(1).
#[derive(Clone, Default)]
pub(crate) struct Resolving(Option<Arc<ResolvingFrame>>);

struct ResolvingFrame {
    def: LambdaId,
    r: ResolvingLambda,
    next: Resolving,
}

impl Resolving {
    fn iter(&self) -> impl Iterator<Item = &ResolvingFrame> {
        std::iter::successors(self.0.as_deref(), |f| f.next.0.as_deref())
    }

    fn push(&mut self, def: LambdaId, r: ResolvingLambda) {
        let next = mem::take(self);
        self.0 = Some(Arc::new(ResolvingFrame { def, r, next }));
    }

    /// Remove the innermost frame of `def` for `instance`.
    fn remove(&mut self, def: LambdaId, instance: LambdaInstanceId) {
        let mut above: SmallVec<[(LambdaId, ResolvingLambda); 4]> = SmallVec::new();
        let mut at = self.clone();
        while let Some(f) = at.0.clone() {
            if f.def == def && f.r.instance == instance {
                at = f.next.clone();
                for (d, r) in above.drain(..).rev() {
                    at.push(d, r);
                }
                *self = at;
                return;
            }
            above.push((f.def, f.r.clone()));
            at = f.next.clone();
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_none()
    }
}

impl<R: Rt> CompileCtx<R> {
    pub(crate) fn instance_node_type(&mut self, id: ExprId) -> Option<Type> {
        self.body.as_mut()?.types.as_mut()?.typ(id)
    }

    /// The active instantiation of `def` with exactly this identity.
    pub(crate) fn resolving(
        &self,
        def: LambdaId,
        identity: &FnArgIdentity,
    ) -> Option<ResolvingLambda> {
        let stack = self.resolving_lambdas.lock();
        stack
            .iter()
            .find(|f| f.def == def && f.r.identity == *identity)
            .map(|f| f.r.clone())
    }

    /// The innermost active instantiation of `def`, whatever its
    /// identity: what a bare value reference inside a resolving body
    /// refers to.
    pub(crate) fn resolving_innermost(&self, def: LambdaId) -> Option<ResolvingLambda> {
        let stack = self.resolving_lambdas.lock();
        stack.iter().find(|f| f.def == def).map(|f| f.r.clone())
    }

    /// Enter the check of `id`'s default for `label`; false when it is
    /// already in progress.
    pub(crate) fn enter_default(&mut self, id: LambdaId, label: &ArcStr) -> bool {
        if self.defaults_in_check.iter().any(|(i, l)| *i == id && l == label) {
            return false;
        }
        self.defaults_in_check.push((id, label.clone()));
        true
    }

    /// Whether the check of `id`'s default for `label` is in progress.
    pub(crate) fn default_in_check(&self, id: LambdaId, label: &ArcStr) -> bool {
        self.defaults_in_check.iter().any(|(i, l)| *i == id && l == label)
    }

    /// Leave the check [`Self::enter_default`] entered.
    pub(crate) fn leave_default(&mut self, id: LambdaId, label: &ArcStr) {
        if let Some(i) =
            self.defaults_in_check.iter().rposition(|(i, l)| *i == id && l == label)
        {
            self.defaults_in_check.remove(i);
        }
    }

    pub(crate) fn push_resolving(&self, def: LambdaId, r: ResolvingLambda) {
        self.resolving_lambdas.lock().push(def, r)
    }

    /// Retire the innermost entry `push_resolving` made for `instance`.
    pub(crate) fn pop_resolving(&self, def: LambdaId, instance: LambdaInstanceId) {
        self.resolving_lambdas.lock().remove(def, instance)
    }
}

/// A registered builtin: how its application is built, how an image
/// restores it, and its classification.
struct BuiltinEntry<R: Rt> {
    init: BuiltInInitFn<R>,
    decode: BuiltInDecodeFn<R>,
    perform: BuiltInPerformFn<R>,
    effect: Effect,
    ordered: bool,
    order_arg: Option<usize>,
    resumes: bool,
}

macro_rules! env_restore_methods {
    () => {
        /// Run `f` with the lexical environment restored to `env`, then put
        /// the current one back. Bindings `f` creates are retained.
        pub fn with_restored<T, F: FnOnce(&mut Self) -> T>(
            &mut self,
            mut env: Lexical,
            f: F,
        ) -> T {
            self.with_restored_mut(&mut env, f)
        }

        /// [`Self::with_restored`] leaving the lexical environment `f`
        /// built in `env`, so two envs keep continuity across invocations.
        pub fn with_restored_mut<T, F: FnOnce(&mut Self) -> T>(
            &mut self,
            env: &mut Lexical,
            f: F,
        ) -> T {
            self.env.swap_lexical(env);
            let r = f(self);
            self.env.swap_lexical(env);
            r
        }
    };
}

/// What registration fills and compiling only reads, shared by every
/// compile task.
struct Registry<R: Rt> {
    lambdawrap: AbstractWrapper<LambdaDef<R>>,
    builtins: AHashMap<&'static str, BuiltinEntry<R>>,
    attributes: AHashMap<&'static str, (AttributeCheckFn<R>, AttributeTargetFn<R>)>,
}

/// Everything compiling and typechecking read and write: the builtin
/// and attribute registry, the program's state, and the scratch of the
/// compile in progress. The runtime half is [`ExecCtx`].
pub struct CompileCtx<R: Rt> {
    registry: Arc<Registry<R>>,
    /// Stop control, shared with the runtime handle, and the
    /// runtime's evaluation pool, which its compile tasks fork onto too.
    /// See [`Control`].
    pub control: Arc<Control>,
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
    /// holds a value. Both bindings of an interface pair are one where
    /// either is ([`Self::join_interface_pair`]). `Bind::delete` removes
    /// its ids.
    pub connect_targets: TrackedSet<BindId>,
    /// The bindings a live call bound statically: that call keeps the
    /// definition it bound, so a later compile's `<-` to the binding is
    /// refused ([`Self::mark_connect_target`]). Its holders keep their
    /// holds ([`StaticHold`]); `Bind::delete` forgets its ids.
    pub(crate) static_callees: Arc<StaticCallees>,
    /// The lets each block being compiled writes, by statement and name.
    pub(crate) written: node::BlockWrites,
    /// Per block being compiled that declares types, innermost last: its
    /// scope and the names its type definitions declare
    /// ([`node::later_typedef`]).
    pub(crate) block_typedefs: Vec<(ModPath, SmallVec<[ArcStr; 4]>)>,
    /// Builtin metadata for `let foo = |...| 'builtin_name` bindings.
    /// Keyed by `(scope, name)` because a sig and its impl share the
    /// name but have distinct `BindId`s.
    pub builtin_bindings: TrackedMap<(ModPath, CompactString), BuiltinBindInfo>,
    /// `LambdaId`s whose def-time body typecheck is in progress: a
    /// self-call inside such a body unifies against the def's own
    /// ftype cells (monomorphic recursion), not a freshening.
    pub(crate) rec_defs: nohash::IntSet<LambdaId>,
    /// The fn-typed parameter BindIds of the defs being body-checked, with
    /// their definition and position: a call through one unifies against
    /// the param's own declared cells, so `f(v)` types as the def's rigid 'b.
    pub(crate) def_gate_params: nohash::IntMap<BindId, (LambdaId, u32)>,
    /// The fn-typed formals of the instances whose bodies are being
    /// checked, by definition and position: what an instance-defined
    /// literal's recorded parameter calls name (`DefTable`).
    pub(crate) instance_fn_params: AHashMap<(LambdaId, u32), BindId>,
    /// The argument each of those formals was bound to at its call, by
    /// formal: where a collection callback's slots say they are.
    pub(crate) fn_param_args: nohash::IntMap<BindId, triomphe::Arc<Expr>>,
    /// Def-gate nesting depth; a nested gate's cells are still
    /// entangled with the enclosing inference.
    pub(crate) def_gate_depth: usize,
    pub(crate) resolving_lambdas: Mutex<Resolving>,
    /// The (definition, label) defaults being checked or elaborated,
    /// innermost last: a site its own omitted default's elaboration
    /// reaches stays dynamic.
    pub(crate) defaults_in_check: SmallVec<[(LambdaId, ArcStr); 4]>,
    /// The copies the reads in the holder's value being checked share
    /// (`node::bind::ValueReads`).
    pub(crate) holder_reads: Option<ReadCopies>,
    /// Per-instance fn-formal BindId → the `LambdaId` forwarded to it:
    /// the persistent record the kernel cache fingerprint reads after
    /// the re-drive's `bind_to_lambda` entry is gone.
    pub fn_forward_resolutions: TrackedMap<BindId, LambdaId>,
    /// What the definition whose body is compiling shares
    /// (`LambdaDef::shared`): each expression it rewrites lowers once, so
    /// every compile of the body has its check's expression ids. A seq
    /// outside every definition compiles once and is lowered where it
    /// stands.
    pub(crate) body: Option<node::lambda::BodyCompile>,
    /// Deferred terminal settles, one frame per resolution scope. A
    /// call site pushes its resolved signature into the current frame;
    /// statement boundaries drain it, so a settle runs only after every
    /// writer in its scope. A re-drive's leftovers merge up to the
    /// parent frame.
    pub(crate) pending_settles: Vec<SettleFrame>,
    /// Imports whose terminal name did not exist when the `use`
    /// compiled (`use self::sub::x` may precede `mod sub;`); re-checked
    /// by `check_pending_imports`, from `compile_top` and a module's load.
    pub(crate) pending_imports: Vec<PendingImport>,
    /// What a check could not decide until it ends
    /// ([`check_pending_names`]).
    pub(crate) pending_names: Vec<Pending>,
    /// Pending definition assertions; see [`DefAssertion`].
    pub(crate) def_assertions: Arc<Mutex<Vec<DefAssertion>>>,
    /// Registry attributes recorded this `compile_stmt`; each must be
    /// dispatched or absorbed by the fusion walk, or the statement errors.
    pub(crate) attr_census: Mutex<Vec<Expr>>,
    pub(crate) attr_dispatched: Mutex<IntSet<ExprId>>,
    pub(crate) attr_absorbed: Mutex<IntSet<ExprId>>,
    /// References compiled but not yet registered with the runtime, by
    /// variable and top expression: compiling reaches no runtime, and
    /// [`ExecCtx::apply_deferred`] registers them.
    pub(crate) pending_refs: AHashMap<(BindId, prune::Reader), usize>,
    /// What compiling abandoned, for the runtime to delete, each with the
    /// boundary it was compiled in.
    discarded: Vec<(Discarded<R>, Option<prune::BoundaryRef>)>,
    /// The boundary the compile is inside (`design/walk_pruning.md`).
    pub(crate) boundary: Option<prune::BoundaryRef>,
    /// This task's share of the compile's profile ([`profile::Task`]).
    profile: profile::Task,
    /// The fusion subsystem's state; see [`fusion::FusionCtx`].
    pub fusion: fusion::FusionCtx,
}

enum Discarded<R: Rt> {
    Node(Node<R>),
    Apply(Box<dyn Apply<R>>),
    Stored(BindId),
}

/// The compile context, the runtime and the cycle's event: what an
/// embedder owns. Nodes see it through an [`ExecCtx`].
pub struct ExecState<R: Rt> {
    pub cx: CompileCtx<R>,
    /// The tables of the image this session was restored from, for
    /// anything decoded later.
    pub(crate) image_decoder: std::sync::OnceLock<image::SharedDecoder>,
    /// Library state for builtins.
    pub libstate: LibState,
    /// The runtime.
    pub rt: R,
    /// The call sites through which `Value` comparison and printing
    /// reach core-trait implementations, built on first use.
    pub(crate) core_hook_sites: Mutex<node::coretraits::CoreHookSites<R>>,
    /// The cycle's event.
    pub event: Event,
}

impl<R: Rt> std::ops::Deref for ExecState<R> {
    type Target = CompileCtx<R>;

    fn deref(&self) -> &CompileCtx<R> {
        &self.cx
    }
}

impl<R: Rt> std::ops::DerefMut for ExecState<R> {
    fn deref_mut(&mut self) -> &mut CompileCtx<R> {
        &mut self.cx
    }
}

/// What a node's update, delete and sleep read and write: a view of an
/// [`ExecState`].
pub struct ExecCtx<'a, R: Rt> {
    pub cx: branch::CxView<'a, R>,
    pub(crate) image_decoder: &'a std::sync::OnceLock<image::SharedDecoder>,
    pub libstate: &'a LibState,
    pub rt: branch::RtView<'a, R>,
    pub(crate) core_hook_sites: &'a Mutex<node::coretraits::CoreHookSites<R>>,
    pub event: &'a mut Event,
    /// How many forks this branch is below the cycle's root.
    pub(crate) fork_depth: u8,
    /// The runtime's parallel mode when the view was made.
    pub(crate) par: graphix_types::stack::ParMode,
    /// What the code around the node being updated says about forking.
    pub(crate) fork: branch::ForkFlags,
    /// The boundary the walk is inside (`design/walk_pruning.md`); the
    /// compile view takes it whenever the walk compiles.
    pub(crate) boundary: Option<prune::BoundaryRef>,
    /// The runtime's prune mode when the view was made.
    pub(crate) prune: PruneMode,
    /// What the arm being caught up owes the inputs it learns as it runs
    /// (`node::wake::CatchUp`).
    pub(crate) catch_up: Option<triomphe::Arc<node::wake::CatchUp>>,
}

impl<'a, R: Rt> std::ops::Deref for ExecCtx<'a, R> {
    type Target = CompileCtx<R>;

    #[inline]
    fn deref(&self) -> &CompileCtx<R> {
        &self.cx
    }
}

impl<'a, R: Rt> std::ops::DerefMut for ExecCtx<'a, R> {
    #[inline]
    fn deref_mut(&mut self) -> &mut CompileCtx<R> {
        let cx = &mut *self.cx;
        if cx.boundary != self.boundary {
            cx.boundary = self.boundary.clone();
        }
        cx
    }
}

impl<R: Rt> CompileCtx<R> {
    /// `spec` in an `Arc` a node holds: an instance's node takes the one
    /// its definition's check made of the same expression, so instances
    /// share their specs; the check records each it makes.
    pub(crate) fn shared_spec(&self, spec: Expr) -> Arc<Expr> {
        let Some(body) = &self.body else { return Arc::new(spec) };
        let mut share = body.share.lock();
        match share.specs.get(&spec.id) {
            Some(held) if **held == spec && held.dec == spec.dec => held.clone(),
            _ => {
                let spec = Arc::new(spec);
                if body.checking {
                    share.specs.insert(spec.id, spec.clone());
                }
                spec
            }
        }
    }

    /// The lowerings the live definitions keep.
    pub fn lowerings_len(&self) -> usize {
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        let mut n = 0;
        for (_, v) in self.lambda_defs.iter() {
            if let Some(d) = v.downcast_ref::<LambdaDef<R>>() {
                node::lambda::BodyShared::visit(&d.shared, &mut seen, &mut |l| {
                    n += l.lowerings.len()
                })
            }
        }
        n
    }

    /// A context for a compile task: it shares the registry, starts from
    /// this program's state, recording what it writes, and inherits the
    /// resolution in progress; its scratch outputs start empty.
    pub(crate) fn fork(&self) -> Self {
        Self {
            registry: self.registry.clone(),
            control: self.control.clone(),
            tags: self.tags.fork(),
            env: self.env.fork(),
            lambda_defs: self.lambda_defs.fork(),
            bind_to_lambda: self.bind_to_lambda.fork(),
            batch_connect_targets: self.batch_connect_targets.fork(),
            connect_targets: self.connect_targets.fork(),
            static_callees: self.static_callees.clone(),
            written: self.written.fork(),
            block_typedefs: self.block_typedefs.clone(),
            builtin_bindings: self.builtin_bindings.fork(),
            rec_defs: self.rec_defs.clone(),
            def_gate_params: self.def_gate_params.clone(),
            instance_fn_params: self.instance_fn_params.clone(),
            fn_param_args: self.fn_param_args.clone(),
            def_gate_depth: self.def_gate_depth,
            resolving_lambdas: Mutex::new(self.resolving_lambdas.lock().clone()),
            defaults_in_check: self.defaults_in_check.clone(),
            holder_reads: None,
            fn_forward_resolutions: self.fn_forward_resolutions.fork(),
            body: self.body.clone(),
            pending_settles: vec![SettleFrame::default()],
            pending_imports: Vec::new(),
            pending_names: Vec::new(),
            def_assertions: Arc::new(Mutex::new(Vec::new())),
            attr_census: Mutex::new(Vec::new()),
            attr_dispatched: Mutex::new(IntSet::default()),
            attr_absorbed: Mutex::new(IntSet::default()),
            pending_refs: AHashMap::default(),
            discarded: Vec::new(),
            boundary: self.boundary.clone(),
            profile: profile::Task::fork(),
            fusion: self.fusion.fork(),
        }
    }

    /// Run `f`, a forked task's work, on this thread under the profile
    /// it forked with.
    pub(crate) fn run_task<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let mut profile = mem::take(&mut self.profile);
        let r = profile.run(|| f(self));
        self.profile = profile;
        r
    }

    /// Take back what the task `fork` produced: its writes to the
    /// program's state and everything it deferred. The resolution scratch
    /// it inherited is balanced by the time it ends and stays this one's.
    pub(crate) fn join(&mut self, fork: Self) {
        let Self {
            registry: _,
            control: _,
            tags,
            env,
            lambda_defs,
            bind_to_lambda,
            batch_connect_targets,
            connect_targets,
            static_callees: _,
            written: _,
            block_typedefs: _,
            builtin_bindings,
            rec_defs: _,
            def_gate_params: _,
            instance_fn_params: _,
            fn_param_args: _,
            def_gate_depth: _,
            resolving_lambdas: _,
            defaults_in_check: _,
            holder_reads: _,
            fn_forward_resolutions,
            body: _,
            pending_settles,
            pending_imports,
            pending_names,
            def_assertions,
            attr_census,
            attr_dispatched,
            attr_absorbed,
            pending_refs,
            discarded,
            boundary: _,
            profile,
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
        let frame = self.pending_settles.last_mut().expect("root settle frame");
        for later in pending_settles {
            frame.append(later)
        }
        self.pending_imports.extend(pending_imports);
        self.pending_names.extend(pending_names);
        if !Arc::ptr_eq(&self.def_assertions, &def_assertions) {
            let joined = mem::take(&mut *def_assertions.lock());
            self.def_assertions.lock().extend(joined);
        }
        self.attr_census.lock().extend(attr_census.into_inner());
        self.attr_dispatched.lock().extend(attr_dispatched.into_inner());
        self.attr_absorbed.lock().extend(attr_absorbed.into_inner());
        // neither order matters: the larger side is kept and the smaller
        // merged in, so nested forks copy each entry a bounded number of times
        let (mut pending_refs, mut discarded) = (pending_refs, discarded);
        if pending_refs.len() > self.pending_refs.len() {
            mem::swap(&mut pending_refs, &mut self.pending_refs);
        }
        for (k, n) in pending_refs {
            *self.pending_refs.entry(k).or_default() += n;
        }
        if discarded.len() > self.discarded.len() {
            mem::swap(&mut discarded, &mut self.discarded);
        }
        self.discarded.extend(discarded);
        profile.join();
        self.fusion.join(fusion);
    }

    /// Record that `top_id` reads `id`: the runtime is told at the next
    /// [`ExecCtx::apply_deferred`].
    pub fn record_ref(&mut self, id: BindId, top_id: ExprId) {
        let reader = self.reader(top_id);
        self.record_reader(id, reader)
    }

    /// [`Self::record_ref`] for a reader taken earlier.
    pub(crate) fn record_reader(&mut self, id: BindId, reader: prune::Reader) {
        *self.pending_refs.entry((id, reader)).or_default() += 1;
    }

    /// The reader a node at `top_id` inside the current boundary is.
    pub fn reader(&self, top_id: ExprId) -> prune::Reader {
        prune::Reader { root: top_id, boundary: self.boundary.clone(), late: false }
    }

    /// [`Self::reader`] for a node that reads [`Event::late`].
    pub fn late_reader(&self, top_id: ExprId) -> prune::Reader {
        prune::Reader { late: true, ..self.reader(top_id) }
    }

    /// `f` compiling inside `boundary`.
    pub fn in_boundary<T>(
        &mut self,
        boundary: Option<&prune::BoundaryRef>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = mem::replace(&mut self.boundary, boundary.cloned());
        let r = f(self);
        self.boundary = outer;
        r
    }

    /// Abandon a node compiling built: it is deleted, with the runtime,
    /// at the next [`ExecCtx::apply_deferred`].
    pub fn discard(&mut self, node: Node<R>) {
        self.discarded.push((Discarded::Node(node), self.boundary.clone()));
    }

    /// [`Self::discard`] for an application.
    pub fn discard_apply(&mut self, apply: Box<dyn Apply<R>>) {
        self.discarded.push((Discarded::Apply(apply), self.boundary.clone()));
    }

    /// Forget the value the runtime stores for `id`, at the next
    /// [`ExecCtx::apply_deferred`].
    pub fn discard_stored(&mut self, id: BindId) {
        self.discarded.push((Discarded::Stored(id), self.boundary.clone()));
    }

    env_restore_methods!();

    /// Record `id` as a `<-` target, of this batch and for good. A
    /// binding something writes is never generalized: its uses share one
    /// type, which every value written takes (the value restriction). One
    /// an earlier compile generalized stays so, its uses having relied on
    /// it, and a write is held to its type (`Connect::typecheck0_with`).
    /// The bindings `id` is one variable with ([`StaticCallees::join`]),
    /// each a target with it.
    pub(crate) fn mark_connect_target(&mut self, id: BindId) -> Result<()> {
        // CR claude for claude: [bug/medium] A sibling module's compile task sees another's
        // interface pair joined or not by schedule (StaticCallees is shared by every task;
        // check_sig joins in the module's own task), so a write from a sibling's lambda
        // through a val marks the implementing binding a `<-` target only where the join
        // came first, and the implementation's static calls bind or not by schedule: in
        // nw6t (a: `val f: fn(x: i64) -> i64` over `let f: fn(x: i64) -> i64 = |x| x`, `let g
        // = |t| f(t ~ 1)`; w: `let set = |v| { super::a::f <- v; null }; let z = set(t ~
        // |x: i64| -> i64 x + 100)`) base 968fa9bc0 in 20 runs printed a::g's last value 101
        // (9), 1 (5: the write never reaches g's static call), or refused the build (6: "f
        // cannot be written: an earlier compile calls it as a fixed function"); cr49-A the
        // same (6/8/6). The narrow-write judgment skips a sibling's pair for this reason
        // (Module::check_written_vals judges it after). D18 (a)'s one variable by
        // construction (iface-pair-one-var-01) should take it; else the pair's join must be
        // seen by a task only as its fork saw it. probe: ~/ptmp/cr29/scratch/r14-A/fix3/p/nw6t
        // (iface-sibling-join-01)
        // 2026-11-04 claude: Review r14 A-r4-1 found a second shape, main's too: sx14, val
        // fn(x: 'a) -> 'a over let f = |x| x with f(1) and f("s") in a, sibling w writing
        // super::a::f <- t ~ k from a lambda with a rank-2 parameter (no narrow write):
        // --check passes 16/16 on base and lane, the build refuses "f cannot be written: an
        // earlier compile calls it as a fixed function at f(1)" in 7/16 runs on base, 5/16 on
        // the lane. probe ~/ptmp/cr29/scratch/review-r14/A-r4-1/.
        let ids = self.static_callees.variable(id);
        if let Some(call) = ids.iter().find_map(|id| self.static_callees.held(*id)) {
            let name =
                self.env.by_id.get(&id).map(|b| b.name.clone()).unwrap_or_default();
            bail!(
                "{name} cannot be written: an earlier compile calls it as a fixed \
                 function at {}: {call}",
                call.pos
            )
        }
        let mut pairs: LPooled<Vec<(BindId, BindId)>> = LPooled::take();
        self.static_callees.pairs_of(std::iter::once(id), &mut pairs);
        for (inner, outer) in pairs.iter().filter(|(_, outer)| *outer == id) {
            // a sibling module's val is judged once the siblings' checks
            // end (`Module::check_written_vals`)
            if let (Some(i), Some(o)) =
                (self.env.by_id.get(inner), self.env.by_id.get(outer))
                && !self.env.in_sibling(&o.scope)
            {
                self.check_val_written(&o.name, &o.typ, &i.typ)?
            }
        }
        for id in ids {
            self.batch_connect_targets.insert(id);
            self.connect_targets.insert(id);
            if !self.generalized_earlier(id) {
                self.env.poly_binds.remove(&id);
            }
        }
        Ok(())
    }

    /// Refuse writes through the `val` `name`, declared `val`, that the
    /// binding implementing it, of type `imp`, would not take: what is
    /// written through the val is of the val's type, which must fit the
    /// implementation's with each variable held rigid, every use having
    /// taken those at its own types.
    pub(crate) fn check_val_written(
        &self,
        name: &str,
        val: &Type,
        imp: &Type,
    ) -> Result<()> {
        // CR claude for claude: [bug/low] `val f: fn(x: 'a) -> B<'a>` over `let f = |x|
        // B(x)` (`type B<'p> = Abstract<'p>`), written `m::f <- t ~ |x| m::B(x)`, is still
        // refused ("annotate the implementation's let with the val's type", which it
        // already matches), though the write is as general as the implementation (`'a` :=
        // `'c`, which holds `'x` by its lower bound). The containment decides positions in
        // order, arguments first: the val's open `'a` meets the held `'x` at the argument
        // and merges into it, and then the result asks the rigid `'x` to hold the rigid
        // `'c`, which it does not. held-lower-bound-01's fix (a rigid cell holds what its
        // lower bounds hold) closed the REPL shape, where the written value's `'c2` carries
        // its own lower bound into the held `'c`. Fix: the open cell takes the rigid one as
        // a lower bound in a contravariant position instead of merging, here at least, or
        // the check solves the result's invariant positions first. The annotation `let f:
        // fn(x: 'a) -> B<'a> = |x| B(x)` passes. probes: ~/ptmp/cr29/scratch/r17-hk/{vw1,vw2}
        // (val-write-arg-merge-01)
        let held = self.monomorphic(imp);
        let mut cells: LPooled<AHashMap<usize, typ::TVar>> = LPooled::take();
        held.reached_cells(&mut cells);
        let rigid: LPooled<Vec<(typ::TVar, SmallVec<[Type; 1]>)>> = cells
            .values()
            .filter(|tv| !tv.is_bound())
            .map(|tv| (tv.clone(), tv.cell_constraints()))
            .collect();
        let _gates: LPooled<Vec<_>> =
            rigid.iter().map(|(tv, _)| tv.open_rigid()).collect();
        // a rigid cell takes on the bounds of the open cells merged into
        // it: a bound its own do not imply narrows it
        let fits = held.check_contains(&self.env, &self.monomorphic(val)).is_ok()
            && rigid.iter().all(|(tv, own)| {
                tv.cell_constraints().iter().all(|c| {
                    *c == Type::Any
                        || own.iter().any(|q| q.implies(&self.env, c).unwrap_or(false))
                })
            });
        match fits {
            true => Ok(()),
            false => format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "val {name} is written, and what is written lands on its \
                     implementation, of type {}, which the val's {val} does not fit: \
                     annotate the implementation's let with the val's type",
                    typ::Brief(imp)
                )
            }),
        }
    }

    /// Where `a` or `b`, one variable, is a `<-` target, a static call
    /// through neither binds in this batch. Only static resolution: the
    /// binding a write names alone takes its typing.
    pub(crate) fn carry_static_mark(&mut self, a: BindId, b: BindId) {
        if self.connect_targets.contains(&a) || self.connect_targets.contains(&b) {
            self.batch_connect_targets.insert(a);
            self.batch_connect_targets.insert(b);
        }
    }

    // CR claude for claude: [perf/low] The `<-`-target mark this puts on the outer outlives
    // the body whose write caused it: Bind::delete removes only the old inner at a reload,
    // so a reloaded body that writes nothing has its calls through the val resolved
    // dynamically (GXDBG_RESOLVE: unstable=true; base 432e34ac6 static), and fusion may go
    // with them. No answer changes. The same holds for a write a body function compiles
    // after the join (mark_connect_target through StaticCallees::variable). Branch cr37-W
    // 2681714fa dropped this mark for a wake hold that reads the pair (written_target) and
    // fixed the first shape only. probes: ~/ptmp/cr29/scratch/review-r6/W2-r2-1/{n1,n1ctl},
    // W2-r3-1/cr37/{d5b,n2} (iface-reload-mark-01)
    // 2026-11-02 claude: Waits on Eric's ruling of the next-discuss item
    // "iface-pair-one-var-01" (~/ptmp/cr29/next-discuss.md): with the pair one run-time
    // variable the mark has no outer to outlive; with the relays kept it needs cr37-W's
    // wake hold. Readdressed to eric until then.
    // 2026-11-03 claude: Eric ruled (2026-11-03): D18 (a) in ~/ptmp/cr-discuss.md: one
    // variable by construction: from the join the interface pair is the val's one id at run
    // time (one queue, one stored value, the body's let publishing to it) and the relays
    // go, iface-reload-mark-01 with them; on a reload keep both: a replaced body's pending
    // writes drop with its binding, a root write queued across the reload still lands; the
    // shapes cr37-W broke (d6, lv) are its tests.
    // 2026-11-04 claude: Since cr49-A's check_val_written (iface-narrow-write-01) the mark
    // also holds a reloaded body to the val: a dynamic module whose first body writes its
    // own let (let f = |x: i64| -> i64 x; f <- ..) leaves the outer a <- target, so a
    // reload whose body is generic under a narrower val (val f: fn(x: i64) -> i64 over let
    // f = |x| x) is refused as written through the val, where nothing writes it any more;
    // main reloads it. probe ~/ptmp/cr29/scratch/r14-A/q4. D18 (a)'s one-variable rebuild,
    // keeping a replaced body's writes with its binding, should take the mark with it.
    /// Join `inner`, a body's binding implementing an interface `val`, with
    /// `outer`, the `val`'s ([`StaticCallees::join`]). From here on they are
    /// one `<-` target where either is, so a woken `let` keeps what was
    /// written through either. The typing stays the written binding's: the
    /// body's check, before this, took the value restriction from the
    /// targets as the writes named them.
    pub(crate) fn join_interface_pair(&mut self, inner: BindId, outer: BindId) {
        self.static_callees.join(inner, outer);
        if self.connect_targets.contains(&inner) || self.connect_targets.contains(&outer)
        {
            self.connect_targets.insert(inner);
            self.connect_targets.insert(outer);
        }
    }

    /// Hold `id`, a binding the call `call` binds statically.
    pub(crate) fn hold_static(&self, id: BindId, call: &Arc<expr::Expr>) -> StaticHold {
        StaticCallees::hold(&self.static_callees, id, call)
    }

    /// Whether `id` is a binding an earlier compile generalized: its
    /// check already made variables of a function it holds generic.
    pub(crate) fn generalized_earlier(&self, id: BindId) -> bool {
        self.env.by_id.get(&id).is_some_and(|b| holds_generic(&b.typ))
    }

    // CR claude for claude: [perf/medium] A chain of lets holding functions, where each
    // link binds a written name or carries an annotation, checks in time exponential in
    // its length: each occurrence of a shared node holding a function takes its own
    // copy here, so a DAG of holders becomes a tree. `let (a, w) = (x, 0); w <- 1; let
    // x = (a, a)` repeated: 2.7/3.2/12.0G instructions at N=10/12/14 (debug --check,
    // main bbb06c0c6), past an 8G RSS cap at 16; o-wtup 6.4/61.9G at 14/18, o-annot
    // 2.7/33.2G, o-apart2 40.8G at 14; the controls without a lambda stay flat, and
    // u-wsingle (no holder) grows too, so not all of it is the holder rule's. Copies
    // keyed by the shared node and the instantiation they came from would keep the
    // DAG. Found by the r8 re-review of self-arg-instance-01. probes
    // ~/ptmp/cr29/scratch/review-r8/xcr-self-arg/c/ (t-ownone-N, u-wsingle, o-wtup,
    // o-annot, o-apart2) (holder-mono-chain-01)
    // 2026-11-03 claude: monomorphic now keeps the value's DAG: every shared node
    // rebuilds once per call and each of its occurrences holds that one instance of
    // each function beneath it (lib.rs fn monomorphic, the memo no longer skipped below
    // an instantiated function). JUDGED: 'the instantiation they came from' is the
    // monomorphic call, since the walk never enters a function type
    // (FnType::instantiate copies one whole). Holder pins hold: lang::types, modules,
    // functions 3519 ok; under GRAPHIX_NO_SUBST=1 lang::types fails only
    // cast_of_a_reference_in_an_instance,
    // cast_of_a_reference_beside_a_filled_application and
    // union_bound_open_cell_accepts, before the change too and on main's binary alike
    // (an instance recheck refuses at the build). The shared-instance probes share1-8
    // (r9-N/mono) print as main. --check instructions:u (GRAPHIX_PAR=off, one eval
    // thread; the empty program 2.0-2.5G), main ec53d4846 / lane at N=10/12/14/16/18:
    // t-ownone 1.61/2.83/14.63G/killed/killed vs 1.51/1.59/1.66/2.57/2.62G; u-wsingle
    // 2.34/2.56/6.60G/killed/killed vs 1.78/2.44/2.49/2.27/2.41G; o-apart2
    // 3.14/3.31/21.08G/killed/killed vs 2.69/2.73/2.42/2.14/2.39G (killed: 300 s or
    // 8G). o-wtup and o-annot stay exponential: their cost is the Own reads' copy at
    // each read, which doubles the type at each link by rule (new CR
    // holder-own-reads-01 for eric). Pin graphix-compiler
    // monomorphic_tests::a_shared_node_is_one_instance (16 doubling pairs over a
    // generic function: 34 nodes at most, the two parts one instance; the old walk
    // 196607 nodes: fails).
    // 2026-11-03 claude: Review r9 of lane N (both reviewers, high/medium): the fix
    // changes verdicts. A write whose value's type holds one node twice now holds one
    // instance there, so p <- mk(g) (let g = |x| x; let mk = |f| (f, f); p = (|x: i64|
    // x, |x: string| x)), p <- (gg, gg) with let gg = id(g), *r <- mk(g), and a holder
    // let hh = (h, h) written into q: ({f: fn(x: i64) -> i64}, {f: fn(x: string) ->
    // string}) (q <- hh, or an annotated let that something writes) are refused here and
    // run on main ec53d4846, while p <- (g, g) and q <- (h, h) (two reads, two copies)
    // pass on both. Main's verdicts and a linear check do not go together under a local
    // rule: a node whose functions' instances copy no cell can be one instance with
    // main's verdicts, but every chain this CR names copies a cell at each link (the
    // lambda's generic argument cell; the binding's own cell is Top), so that variant
    // measured exactly as main (--check instructions:u, --no-scale, GRAPHIX_PAR=off, one
    // eval thread; main / that variant at N=10 and 14: t-ownone 1.38/1.38G, 5.23/5.23G;
    // u-wsingle 1.37/1.37G, 5.24/5.24G; o-apart2 1.44/1.44G, 9.14/9.13G; both killed at
    // 150 s at N=18), where this commit stays at 1.31-1.33G to 18. Main already ties
    // such places where the shared cell is the binding's own: let a2 = x1; a2 <- never()
    // over x1 = (a1, a1), a1 a written copy of x0 = (|x| x, 1), cannot be annotated
    // ((fn(x: i64) -> i64, i64), (fn(x: string) -> string, i64)) on main. Ruling needed:
    // (a) one instance per node the value's type shares (this commit; ML's rule: OCaml
    // types mk g as ('_w -> '_w) * ('_w -> '_w), one weak variable; <- then agrees with
    // let, whose forms d7, e8, e9 main already refuses); it needs CLAUDE.md's
    // value-restriction line and pins that refuse d3, e1, e7, b1, w3 and accept b3, w2
    // (two reads are two copies, as p <- (g, g) is); or (b) one instance per place
    // (main): drop c30722796 and the chains stay exponential; the per-place variant with
    // its pins (lang::types::written_value_sharing_a_node_holds_an_instance_per_place,
    // monomorphic_tests::a_shared_node_copying_nothing_is_one_instance and
    // a_shared_node_copying_a_cell_is_an_instance_per_occurrence, each failing under the
    // other walk) is ~/ptmp/cr29/scratch/r9-N/item5-perplace.patch. The counts in the
    // note above are scaled perf counts (no --no-scale), 1.5-3x high: unscaled, main /
    // this commit at N=14: t-ownone 5.23/1.32G, u-wsingle 5.24/1.31G, o-apart2
    // 9.14/1.32G; at 18 main killed at 150 s, this commit 1.32-1.33G; the empty program
    // 1.30G. probes ~/ptmp/cr29/scratch/review-r9/N-r1-1/m5/,
    // ~/ptmp/cr29/scratch/review-r9/N-r1-2/mono/, ~/ptmp/cr29/scratch/r9-N/m5/gen2.gx,
    // chains ~/ptmp/cr29/scratch/r9-N/chains/
    // 2026-11-03 claude: Review r9 round 2 (both reviewers, high): with the CR waiting
    // on Eric, c30722796's walk is taken off cr46-N so the branch lands with main's
    // verdicts: monomorphic is main's again (a shared node rebuilds once only while no
    // function beneath it instantiated), its pin
    // monomorphic_tests::a_shared_node_is_one_instance and the tvar_constraints.md
    // sentence are gone, and the chains cost as main's column above (exponential). The
    // notes above that describe the walk as built describe c30722796 only. Both options
    // are saved for the ruling: option (a), this CR's DAG walk with its pin and doc
    // sentence, as ~/ptmp/cr29/scratch/r9-N/item5-dag.patch (a diff of c30722796, apply
    // with git apply); option (b), main's per-place rule with a linear case and its
    // pins, as item5-perplace.patch. If Eric rules (a) it comes back with CLAUDE.md's
    // value-restriction line and lang::types pins that refuse d3, e1, e7, b1, w3 and
    // accept b3, w2 (probes ~/ptmp/cr29/scratch/review-r9/N-r1-1/m5/, N-r1-2/mono/).
    // 2026-11-03 claude: item5-perplace.patch is a diff against c30722796, not against
    // the branch as landed: it applies only on top of item5-dag.patch (git apply
    // item5-dag.patch, then item5-perplace.patch). Option (b) without its pins is the
    // tree as landed. The question is in ~/ptmp/cr29/next-discuss-addendum.md, Round 8
    // (found by the final review of lane N).
    // 2026-11-03 claude: Eric ruled (2026-11-03): D12 a (filled by claude at his word)
    // in ~/ptmp/cr-discuss.md, under his rule for type holes (reject and require an
    // annotation, then a --check census: it comes back to him only if the stdlib, the
    // book's examples or netidx-admin would need a new annotation): one instance per
    // node a written binding's value type shares (ML's weak variable): `p <- mk(g)` is
    // refused as its `let` form is; start from ~/ptmp/cr29/scratch/r9-N/item5-dag.patch,
    // with a CLAUDE.md line and pins.
    // 2026-11-04 claude: Round 15 lane M built D11 (b) and D12 (a) on branch
    // cr50-M-parked (fb0072ecd..f90e93c94) and was parked. D12 held up (a written chain
    // of 18 holders checks in 1.34G where main is killed at 150 s). D11 failed review
    // three times in new ways, each the same cause at a new site: a call through a value
    // no generic binding names shares its definition's cells, but wherever a generic
    // copy meets those cells (a holder's read, then a struct update, then an array, map,
    // list or select of composites) the merge keeps the copy's generic cell, so a later
    // read copies what the shared call decided and the run refuses what the check passed
    // (main runs those). The lane also refuses some well-typed arrays of composites with
    // "expected fn not [fn, fn]". Back to Eric as the D11/D12 follow-up in
    // ~/ptmp/cr29/next-discuss.md; probes ~/ptmp/cr29/scratch/review-r15/M-r*/.
    // 2026-11-05 claude: Eric ruled (2026-11-05) on the D11/D12 follow-up: (a). Split
    // D12 (one instance per shared node) out of cr50-M-parked (fb0072ecd), re-review and
    // land it now.
    /// A value's type as a written binding holds it: each function's
    /// generalized signature it holds, as one instance of it.
    pub(crate) fn monomorphic(&self, t: &Type) -> Type {
        /// A shared node rebuilds once and stays shared, unless a function
        /// type beneath it instantiated: each occurrence of one takes its
        /// own variables.
        struct Memo<'a> {
            open: &'a nohash::IntSet<LambdaId>,
            done: LPooled<AHashMap<usize, (Type, Option<Type>)>>,
            fresh: bool,
            env: &'a Env,
            /// the typedefs being expanded, and the lowest of them a
            /// nested application met: an expansion holding its own
            /// application is copied whole instead
            expanding: LPooled<Vec<(ModPath, SourcePosition)>>,
            cycle: usize,
        }
        fn instance(t: &Type, m: &mut Memo) -> Option<Type> {
            crate::stack::ensure_sufficient(|| {
                let key = t.shared_identity();
                if let Some(k) = key
                    && let Some((_, r)) = m.done.get(&k)
                {
                    return r.clone();
                }
                let outer = mem::replace(&mut m.fresh, false);
                let r = match t {
                    Type::TVar(tv) => tv.binding().map(|b| instance(&b, m).unwrap_or(b)),
                    Type::Fn(ft) => {
                        m.fresh = true;
                        Some(Type::Fn(Arc::new(ft.instantiate(m.open))))
                    }
                    // a typedef's arguments stand for what its body holds:
                    // instantiated as one, or, where the body holds two
                    // places a copy takes apart, as its expansion would be
                    // XCR claude for claude: [bug/low] A typedef application is copied as
                    // one here, where its expansion would be copied per function type, so a
                    // val typed by a typedef whose body names its parameter in two function
                    // types is narrower in check_val_written (and the Connect hold) than the
                    // same type written out: `type S<'a> = {f: fn(x: 'a) -> 'a, g: fn(x:
                    // 'a) -> 'a}; val c: S<'a>` over `let c = {f: |x| x, g: |x| x}`, written
                    // `m::c <- t ~ {f: |x| x, g: |x| x}`, is refused "val c is written, and
                    // what is written lands on its implementation, of type { f: .., g: .. },
                    // which the val's S<'a> does not fit" (sw8; sw6 over `{f: F<'a>, g:
                    // F<'a>}` alike), where `val c: {f: fn(x: 'a) -> 'a, g: fn(x: 'a) -> 'a}`
                    // (sw1) and `{f: F<'a>, g: F<'a>}` (sw7) run "1 2", as main 968fa9bc0
                    // runs all four (cr49-A since 5d12f43ae). It contradicts the book's
                    // interface page ("each of its function types on its own") and
                    // CLAUDE.md ("each function type's apart whatever their names").
                    // Over-refusal only: `let c: S<'a> = ..` passes (sw6b). Copying the
                    // expansion needs a guard for a recursive typedef, whose expansion holds
                    // its own application, and prints the Connect hold's copy expanded
                    // (lang::interfaces::write_through_a_typedef_val_refused names F<'a>),
                    // unless only an application whose body names a parameter in two
                    // function types expands; a read (Type::scheme_read) copies through one
                    // cell map, written out or not, and is not concerned. probes:
                    // ~/ptmp/cr29/scratch/r14-A/fix3/p/sw-{sw1,sw6,sw6b,sw7,sw8}
                    // (iface-typedef-copy-01)
                    // 2026-11-05 claude: Fixed: CompileCtx::monomorphic copies a typedef
                    // application whose body holds two places a copy takes apart
                    // (Type::copied_in_two_places: function types and applications holding a
                    // copied cell) as its expansion, so each function type is copied on its
                    // own; other applications stay copied as one (the Connect hold still
                    // prints F<'a>), and an expansion that meets its own typedef (by
                    // declaration site, Type::lookup_ref_declared) is copied whole. Pins that
                    // fail without it:
                    // lang::interfaces::write_through_a_typedef_of_two_functions_val_as_general
                    // (sw8) and write_through_a_typedef_of_two_typedefs_val_as_general (sw6);
                    // write_through_a_recursive_typedef_of_two_functions_val_as_general pins
                    // the recursion guard. Needs a soak.
                    Type::Ref(_) if t.copied_outside_functions(m.open) => {
                        let apart =
                            t.lookup_ref_declared(m.env).and_then(|(key, body)| match m
                                .expanding
                                .iter()
                                .position(|k| *k == key)
                            {
                                Some(i) => {
                                    m.cycle = m.cycle.min(i);
                                    None
                                }
                                None => body
                                    .copied_in_two_places(m.open)
                                    .then_some((key, body)),
                            });
                        m.fresh = true;
                        let as_one =
                            |open| t.instantiate_with(&mut LPooled::take(), open);
                        match apart {
                            None => Some(as_one(m.open)),
                            Some((key, body)) => {
                                let depth = m.expanding.len();
                                m.expanding.push(key);
                                let r = instance(&body, m);
                                m.expanding.pop();
                                let cyclic = m.cycle <= depth;
                                if m.cycle == depth {
                                    m.cycle = usize::MAX;
                                }
                                match cyclic {
                                    true => Some(as_one(m.open)),
                                    false => Some(r.unwrap_or(body)),
                                }
                            }
                        }
                    }
                    t => t.cow_children(&mut |c| instance(c, m)),
                };
                if let Some(k) = key
                    && !m.fresh
                {
                    m.done.insert(k, (t.clone(), r.clone()));
                }
                m.fresh |= outer;
                r
            })
        }
        let mut m = Memo {
            open: &self.rec_defs,
            done: LPooled::take(),
            fresh: false,
            env: &self.env,
            expanding: LPooled::take(),
            cycle: usize::MAX,
        };
        instance(t, &mut m).unwrap_or_else(|| t.clone())
    }

    pub fn register_builtin<T: BuiltIn<R>>(&mut self) -> Result<()> {
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
                    perform: T::perform,
                    effect: T::EFFECT,
                    ordered: T::ORDERED,
                    order_arg: T::ORDER_ARG,
                    resumes: T::RESUMES,
                });
            }
            Entry::Occupied(_) => bail!("builtin {} is already registered", T::NAME),
        }
        Ok(())
    }

    /// The image decoder of a registered builtin.
    pub fn builtin_decoder(&self, name: &str) -> Option<BuiltInDecodeFn<R>> {
        self.registry.builtins.get(name).map(|b| b.decode)
    }

    pub fn register_attribute<T: Attribute<R>>(&mut self) -> Result<()> {
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
    pub fn lookup_attribute(&self, name: &str) -> Option<AttributeCheckFn<R>> {
        self.registry.attributes.get(name).map(|(check, _)| *check)
    }

    /// The target check for a registered attribute.
    pub fn lookup_attribute_target(&self, name: &str) -> Option<AttributeTargetFn<R>> {
        self.registry.attributes.get(name).map(|(_, target)| *target)
    }

    /// A registered builtin's [`Effect`]; `Async` for unknown names.
    pub fn builtin_effect(&self, name: &str) -> Effect {
        self.registry.builtins.get(name).map(|b| b.effect).unwrap_or_default()
    }

    /// Whether a registered builtin is [`BuiltIn::RESUMES`].
    pub fn builtin_resumes(&self, name: &str) -> bool {
        self.registry.builtins.get(name).is_some_and(|b| b.resumes)
    }

    /// A registered [`Effect::Io`] builtin's [`BuiltIn::perform`].
    pub fn builtin_perform(&self, name: &str) -> Option<BuiltInPerformFn<R>> {
        self.registry
            .builtins
            .get(name)
            .filter(|b| matches!(b.effect, Effect::Io(_)))
            .map(|b| b.perform)
    }

    /// Whether a registered builtin is [`BuiltIn::ORDERED`].
    pub fn builtin_ordered(&self, name: &str) -> bool {
        self.registry.builtins.get(name).is_some_and(|b| b.ordered)
    }

    pub(crate) fn builtin_order_arg(&self, name: &str) -> Option<usize> {
        self.registry.builtins.get(name).and_then(|b| b.order_arg)
    }

    /// Wrap a `LambdaDef` into a first-class function `Value` and
    /// register it in `lambda_defs`.
    pub fn wrap_lambda(&mut self, def: LambdaDef<R>) -> Value {
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

impl<R: Rt> ExecState<R> {
    /// Build a new execution state over the runtime `rt`. A low-level
    /// interface for custom runtimes; most embedders want `graphix-rt`.
    pub fn new(rt: R) -> Result<Self> {
        graphix_types::abstract_value::register();
        reference::register();
        crate::rep::register();
        let id = AbstractTypeRegistry::uuid::<LambdaDef<R>>("lambda");
        let mut this = Self {
            cx: CompileCtx {
                registry: Arc::new(Registry {
                    lambdawrap: Abstract::register(id)?,
                    builtins: AHashMap::default(),
                    attributes: AHashMap::default(),
                }),
                control: Arc::new(Control::new()),
                tags: TrackedSet::default(),
                env: Env::default(),
                lambda_defs: TrackedMap::default(),
                bind_to_lambda: TrackedMap::default(),
                batch_connect_targets: TrackedSet::default(),
                connect_targets: TrackedSet::default(),
                static_callees: Arc::new(StaticCallees::default()),
                written: node::BlockWrites::default(),
                block_typedefs: Vec::new(),
                builtin_bindings: TrackedMap::default(),
                rec_defs: nohash::IntSet::default(),
                def_gate_params: nohash::IntMap::default(),
                instance_fn_params: AHashMap::default(),
                fn_param_args: nohash::IntMap::default(),
                def_gate_depth: 0,
                resolving_lambdas: Mutex::new(Resolving::default()),
                defaults_in_check: SmallVec::new(),
                holder_reads: None,
                fn_forward_resolutions: TrackedMap::default(),
                body: None,
                pending_settles: vec![SettleFrame::default()],
                pending_imports: Vec::new(),
                pending_names: Vec::new(),
                def_assertions: Arc::new(Mutex::new(Vec::new())),
                attr_census: Mutex::new(Vec::new()),
                attr_dispatched: Mutex::new(IntSet::default()),
                attr_absorbed: Mutex::new(IntSet::default()),
                pending_refs: AHashMap::default(),
                discarded: Vec::new(),
                boundary: None,
                profile: profile::Task::default(),
                fusion: fusion::FusionCtx::new(),
            },
            image_decoder: std::sync::OnceLock::new(),
            libstate: LibState::default(),
            rt,
            core_hook_sites: Mutex::new(node::coretraits::CoreHookSites::default()),
            event: Event::new(),
        };
        this.register_attribute::<Native>()?;
        Ok(this)
    }

    /// The view nodes run under.
    pub fn view(&mut self) -> ExecCtx<'_, R> {
        let Self { cx, image_decoder, libstate, rt, core_hook_sites, event } = self;
        let (par, prune) = (cx.control.par_mode(), cx.control.prune_mode());
        let rt = branch::RtView::Root(rt);
        let cx = branch::CxView::Root(cx);
        ExecCtx {
            cx,
            image_decoder,
            libstate,
            rt,
            core_hook_sites,
            event,
            fork_depth: 0,
            par,
            fork: branch::ForkFlags::default(),
            boundary: None,
            prune,
            catch_up: None,
        }
    }
}

impl<'a, R: Rt> ExecCtx<'a, R> {
    /// `f` under `view` joined with the view already in force
    /// ([`View::join`]): a birth inside a wake is a step.
    #[inline]
    pub fn under<T>(&mut self, view: View, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.event.view;
        self.event.view = prev.join(view);
        let r = f(self);
        self.event.view = prev;
        r
    }

    /// What the reference `cell` names: its registered place (a root and
    /// a path), else the binding at the end of its byref chain, else the
    /// cell itself; the path is empty but for a place.
    pub fn ref_target(&self, cell: BindId) -> (BindId, &node::place::Path) {
        node::bind::ref_target(self, cell)
    }

    /// Where a write through the reference cell `cell` lands: its
    /// registered place, else what [`Self::ref_target`] names; nowhere
    /// while it is a place whose address is bottom.
    pub fn write_target(&self, cell: BindId) -> Option<WriteTarget> {
        match self.rt.ref_path(&cell) {
            Some(node::place::RefPath::At(root, path)) => {
                Some(WriteTarget::Place(*root, path.clone()))
            }
            Some(node::place::RefPath::Nowhere) => None,
            None => Some(WriteTarget::Bind(self.ref_target(cell).0)),
        }
    }

    /// `v`, a value of `t`, with each reference a comparison meets
    /// replaced by what it names, `[&root, step..]` (a reference's value
    /// is its own cell): equal results mean equal values, references
    /// equal when they name one place.
    pub fn ref_targets(&self, t: &Type, v: &Value) -> Value {
        use node::place::Step;
        t.map_refs(&self.env, v, &mut |r| match node::bind::ref_cell(r) {
            Some(cell) => {
                let (root, path) = node::bind::ref_target(self, cell);
                let steps = path.iter().map(|s| match s {
                    Step::Index(i) => Value::I64(*i),
                    Step::Field(f) => Value::String(f.clone()),
                    Step::Key(k, _) => k.clone(),
                });
                // the root as a reference, which no data carries: the form
                // equals no array of numbers
                let target =
                    std::iter::once(reference::reference(root, None)).chain(steps);
                Value::Array(ValArray::from_iter(target))
            }
            None => r.clone(),
        })
    }

    /// How this view may fork: `Off` past the depth limit, inside a seq
    /// machine or under `#[serial]`; `Force` under `#[parallel]`.
    #[inline]
    pub(crate) fn fork_mode(&self) -> ParMode {
        let f = self.fork;
        if f.seq || f.inhibit || self.fork_depth >= branch::MAX_FORK_DEPTH {
            ParMode::Off
        } else if f.forced.is_some() && self.par != ParMode::Off {
            ParMode::Force
        } else {
            self.par
        }
    }

    /// Run `f` under the fork flags `flags`.
    #[inline]
    pub(crate) fn with_fork_flags<T>(
        &mut self,
        flags: branch::ForkFlags,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let saved = std::mem::replace(&mut self.fork, flags);
        let r = f(self);
        self.fork = saved;
        r
    }

    /// Deliver a write to either binding of an interface pair to both, as
    /// the cycle starts (`node::module::deliver_pair_writes`).
    pub fn deliver_pair_writes(&mut self) {
        node::module::deliver_pair_writes(self)
    }

    /// Let go of what the references released since the last cycle kept:
    /// a cell's place, chain link and value, and a binding deleted while a
    /// reference named it.
    pub fn release_references(&mut self) {
        let lives = self.control.lives().clone();
        let rt = &self.rt;
        lives.collect(
            |id, f| {
                if let Some((tv, _)) = rt.store_get(&id)
                    && !tv.tag().is_bottom()
                {
                    tv.with_value(|v| f(v))
                }
            },
            |a| a.downcast_ref::<LambdaDef<R>>().and_then(|d| d.captures.get()),
        );
        lives.released(|id| {
            self.cx.env.byref_chain.remove(&id);
            self.cx.env.by_id.remove(&id);
            self.cx.env.poly_binds.remove(&id);
            self.rt.clear_ref_path(&id);
            self.rt.store_release(&id);
        });
    }

    /// The same view, borrowed for a shorter time, over `event`.
    pub fn with_event<'b>(&'b mut self, event: &'b mut Event) -> ExecCtx<'b, R> {
        ExecCtx {
            cx: self.cx.reborrow(),
            image_decoder: self.image_decoder,
            libstate: self.libstate,
            rt: self.rt.reborrow(),
            core_hook_sites: self.core_hook_sites,
            event,
            fork_depth: self.fork_depth,
            par: self.par,
            fork: self.fork,
            boundary: self.boundary.clone(),
            prune: self.prune,
            catch_up: self.catch_up.clone(),
        }
    }

    /// Release an id this node minted for its own deliveries: unregister
    /// it, stop a timer set on it, and forget the value the store holds
    /// for it and the writes waiting for it, which nothing will read.
    pub fn release_var(&mut self, id: BindId, top_id: ExprId) {
        self.rt.cancel_timer(id);
        self.unref_var(id, top_id);
        self.rt.store_release(&id);
    }

    /// Deliver `tv` to the binding `id` this cycle, into the event. A fire
    /// is stored as the event holds it, stamped this cycle, and schedules
    /// what `notify` says; a quiet production stands in the store with the
    /// stamp of the binding's last fire.
    pub fn deliver(&mut self, id: BindId, tv: TagValue, notify: Notify) {
        let tag = tv.tag();
        let stored = match tag.is_bottom() {
            true => TagValue::tagged(Value::Null, tag),
            false => tv.clone(),
        };
        if tag.triggers() {
            match self.event.root {
                None => self.rt.store_insert(id, stored),
                Some(root) => {
                    let mut passed = None;
                    let elsewhere = self.rt.store_fire(
                        id,
                        stored,
                        root,
                        self.boundary.as_ref(),
                        &mut passed,
                    );
                    if let Some(passed) = passed {
                        for r in passed {
                            self.rt.late(id, tv.clone(), r)
                        }
                    }
                    if elsewhere {
                        self.rt.produced(id)
                    }
                }
            }
            if notify == Notify::Roots {
                self.rt.notify_marked(id)
            }
        } else {
            self.rt.store_insert_standing(id, stored)
        }
        self.event.variables.insert(id, tv);
    }

    /// Drop a reference `top_id` holds to `id`: one not replayed yet is
    /// cancelled, any other unregistered.
    pub fn unref_var(&mut self, id: BindId, top_id: ExprId) {
        let reader = self.reader(top_id);
        self.unref_reader(id, reader)
    }

    /// [`Self::unref_var`] for a reader taken earlier.
    pub(crate) fn unref_reader(&mut self, id: BindId, reader: prune::Reader) {
        let key = (id, reader);
        // read through the view first: a branch that compiled nothing
        // must not fork its compile view to find nothing pending
        if !self.cx.pending_refs.contains_key(&key) {
            return self.rt.unref_var(id, key.1);
        }
        let n = self.cx.pending_refs.get_mut(&key).expect("checked above");
        *n -= 1;
        if *n == 0 {
            self.cx.pending_refs.remove(&key);
        }
    }

    /// Register now that the node at `top_id` reads `id`: a read the walk
    /// learns at run time.
    pub fn ref_var(&mut self, id: BindId, top_id: ExprId) {
        let reader = self.reader(top_id);
        self.rt.ref_var(id, reader)
    }

    /// The reader a node at `top_id` inside the walk's boundary is.
    pub fn reader(&self, top_id: ExprId) -> prune::Reader {
        prune::Reader { root: top_id, boundary: self.boundary.clone(), late: false }
    }

    /// [`Self::reader`] for a node that reads [`Event::late`].
    pub fn late_reader(&self, top_id: ExprId) -> prune::Reader {
        prune::Reader { late: true, ..self.reader(top_id) }
    }

    /// `f` walking inside `boundary`.
    pub fn in_boundary<T>(
        &mut self,
        boundary: Option<&prune::BoundaryRef>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = mem::replace(&mut self.boundary, boundary.cloned());
        let r = f(self);
        self.boundary = outer;
        r
    }

    /// Deliver `tv` to `id` this cycle in the event alone (its store entry
    /// stands as it is), marking its readers' boundaries.
    pub fn publish(&mut self, id: BindId, tv: TagValue) {
        self.event.variables.insert(id, tv);
        self.rt.mark_readers(id)
    }

    /// [`Self::publish`] unless `id` was delivered this cycle already, when
    /// `tv` comes back.
    pub fn try_publish(&mut self, id: BindId, tv: TagValue) -> Result<(), TagValue> {
        self.event.variables.try_insert(id, tv)?;
        self.rt.mark_readers(id);
        Ok(())
    }

    /// Apply to the runtime what compiling deferred: register every
    /// reference it recorded, then delete what it discarded, so each
    /// discard's unrefs reach the runtime.
    pub fn apply_deferred(&mut self) {
        // through `Deref`: a branch view forks no compile for nothing
        if !self.deferred_pending() {
            return;
        }
        self.control.rebuilt();
        // registered first, so each discard's delete reaches the runtime,
        // whether it was registered before this batch or in it
        for ((id, reader), n) in self.cx.pending_refs.drain() {
            for _ in 0..n {
                self.rt.ref_var(id, reader.clone());
            }
        }
        self.shrink_pending_refs();
        self.delete_discarded();
    }

    /// A drain or clear walks the table's whole capacity, so one large
    /// batch (a collection's first slots) must not make every later
    /// small one pay for it.
    fn shrink_pending_refs(&mut self) {
        const KEPT: usize = 64;
        if self.cx.pending_refs.capacity() > KEPT {
            self.cx.pending_refs.shrink_to(KEPT)
        }
    }

    /// Undo what a failed compile deferred: delete what it discarded and
    /// forget the references it recorded.
    pub fn drop_deferred(&mut self) {
        if !self.deferred_pending() {
            return;
        }
        self.delete_discarded();
        self.cx.pending_refs.clear();
        self.shrink_pending_refs();
    }

    fn delete_discarded(&mut self) {
        for (d, boundary) in mem::take(&mut self.cx.discarded) {
            self.in_boundary(boundary.as_ref(), |ctx| match d {
                Discarded::Node(mut n) => n.delete(ctx),
                Discarded::Apply(mut a) => a.delete(ctx),
                Discarded::Stored(id) => ctx.rt.store_release(&id),
            })
        }
    }

    /// Whether a compile deferred something no
    /// [`Self::apply_deferred`] applied.
    pub fn deferred_pending(&self) -> bool {
        !self.cx.pending_refs.is_empty() || !self.cx.discarded.is_empty()
    }

    /// True once the runtime is stopping (an interrupt, a budget or a
    /// shutdown); loops poll this at their head.
    pub fn interrupted(&self) -> bool {
        self.control.interrupted()
    }

    /// Open the compile frame of a top-level node, a statement's or one
    /// built at runtime outside any statement, before [`check_and_fuse`]:
    /// the previous compile's scratch is dropped.
    pub fn open_compile_frame(&mut self, top_id: ExprId) {
        debug_assert!(
            self.resolution_scratch_is_empty(),
            "a top-level compile opened over another's resolution scratch"
        );
        self.attr_census.lock().clear();
        self.attr_dispatched.lock().clear();
        self.attr_absorbed.lock().clear();
        self.pending_imports.clear();
        self.pending_names.clear();
        self.pending_settles.clear();
        self.pending_settles.push(SettleFrame::default());
        self.fusion.top_id = Some(top_id);
    }

    /// The scratch a definition's check pushes and pops: empty between
    /// top-level compiles.
    fn resolution_scratch_is_empty(&self) -> bool {
        self.def_gate_depth == 0
            && self.rec_defs.is_empty()
            && self.def_gate_params.is_empty()
            && self.instance_fn_params.is_empty()
            && self.fn_param_args.is_empty()
            && self.written.is_empty()
            && self.block_typedefs.is_empty()
            && self.resolving_lambdas.lock().0.is_none()
            && self.defaults_in_check.is_empty()
            && self.holder_reads.is_none()
            && self.body.is_none()
    }

    /// Empty what a compile a panic unwound left in the resolution
    /// scratch.
    pub fn clear_resolution_scratch(&mut self) {
        self.def_gate_depth = 0;
        self.rec_defs.clear();
        self.def_gate_params.clear();
        self.instance_fn_params.clear();
        self.fn_param_args.clear();
        self.written.clear();
        self.block_typedefs.clear();
        *self.resolving_lambdas.lock() = Resolving::default();
        self.defaults_in_check.clear();
        self.holder_reads = None;
        self.body = None;
    }
    env_restore_methods!();
}

/// What the check defers to its statement's settle; see
/// [`CompileCtx::pending_settles`].
pub(crate) enum PendingSettle {
    /// A call site's terminal settle.
    Site {
        /// The site's resolved signature.
        ftype: FnType,
        /// The site's return-type cell.
        rtype: Option<typ::TVar>,
        unread_callbacks: SmallVec<[typ::TVar; 1]>,
        /// Cells (by address) exempt from settling: an omitted defaulted
        /// argument's.
        exempt: AHashSet<usize>,
        /// The signatures of the enclosing definitions: what they reach
        /// is generalized, settled by each call.
        sigs: SmallVec<[triomphe::Arc<FnType>; 1]>,
        /// What its JSON and TOML bounds judge: `Instance` in an instance's
        /// body checked again (`node::instance_rechecked`).
        text: typ::TextSite,
        spec: Arc<Expr>,
    },
    /// A cell an operation leaves for its context to decide (an
    /// operator's operand, a constructed value's parameter), settled once
    /// the frame's sites have.
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
    /// A compared type, or a map's type, that must hold no union of two
    /// members with one runtime form (only map keys for `Keys`), open
    /// cells read as any type they may yet bind.
    Discernible { typ: Type, what: Discerned, spec: Arc<Expr> },
    /// A select arm's type test against what else reaches it: no two
    /// members with one runtime form, open cells read as any type they
    /// may yet bind.
    Told { test: Type, rest: Type, spec: Arc<Expr> },
}

/// What a [`PendingSettle::Discernible`] judges.
pub(crate) enum Discerned {
    /// A compared type: ordered by `<` and the like, else by `==`/`!=`.
    Compared {
        ordered: bool,
    },
    Keys,
    /// A call's cell with the bound (`Discernible` or `Ordered`).
    Bound(typ::TVar, Type),
}

/// The settles a check deferred, in the order they settle, held as
/// chunks so a join moves the shorter side's chunks, never the settles.
#[derive(Default)]
pub(crate) struct SettleFrame(VecDeque<Vec<PendingSettle>>);

impl SettleFrame {
    pub(crate) fn push(&mut self, settle: PendingSettle) {
        match self.0.back_mut() {
            Some(chunk) => chunk.push(settle),
            None => self.0.push_back(vec![settle]),
        }
    }

    /// `later`'s settles after this frame's.
    pub(crate) fn append(&mut self, mut later: SettleFrame) {
        if self.0.len() < later.0.len() {
            mem::swap(self, &mut later);
            while let Some(chunk) = later.0.pop_back() {
                self.0.push_front(chunk)
            }
        } else {
            self.0.extend(later.0)
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.iter().all(|chunk| chunk.is_empty())
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &PendingSettle> {
        self.0.iter().flatten()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut PendingSettle> {
        self.0.iter_mut().flatten()
    }
}

impl PendingSettle {
    /// Run every site settle of `frame`, then every cell settle, then
    /// every rule judged over the settled cells.
    pub(crate) fn drain(
        frame: &SettleFrame,
        env: &Env,
        mut on_err: impl FnMut(&Arc<Expr>, anyhow::Error) -> Result<()>,
    ) -> Result<()> {
        let mut reached: LPooled<AHashMap<usize, AHashSet<usize>>> = LPooled::take();
        for s in frame.iter() {
            if let PendingSettle::Site {
                ftype,
                rtype,
                exempt,
                sigs,
                spec,
                unread_callbacks,
                text: _,
            } = s
            {
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
                    let e = match e.downcast_ref::<typ::settle::ConcreteRequired>() {
                        Some(concrete)
                            if unread_callbacks
                                .iter()
                                .any(|tv| tv.cell_addr() == concrete.0.cell_addr()) =>
                        {
                            concrete.unread_callback()
                        }
                        _ => e,
                    };
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
                PendingSettle::Discernible { typ, what, spec } => {
                    (discernible(env, typ, what), spec)
                }
                PendingSettle::Told { test, rest, spec } => (told(env, test, rest), spec),
                PendingSettle::Site { ftype, text, spec, .. } => {
                    (site_bounds(env, ftype, *text), spec)
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

/// The members of the union `union` a refusal names: those but the open
/// cells a program did not name, or "another" where there are none.
fn known_members(union: &Type) -> CompactString {
    let mut out = CompactString::default();
    let members = match union {
        Type::Set(ms) => &ms[..],
        t => std::slice::from_ref(t),
    };
    for m in members {
        if !matches!(m, Type::TVar(tv) if tv.binding().is_none() && tv.name.starts_with('_'))
        {
            let sep = if out.is_empty() { "" } else { ", " };
            let _ = write!(out, "{sep}{m}");
        }
    }
    if out.is_empty() {
        out.push_str("another")
    }
    out
}

/// The bounds a call's settled cells must meet beyond their check: a
/// `Discernible` cell's binding, and the map keys a `Concrete` one's (a
/// map read from data).
fn site_bounds(env: &Env, ftype: &FnType, text: typ::TextSite) -> Result<()> {
    let mut tvs: LPooled<AHashMap<ArcStr, typ::TVar>> = LPooled::take();
    ftype.collect_tvars(&mut tvs);
    for tv in tvs.values() {
        let Some(t) = tv.binding() else { continue };
        for c in tv.cell_constraints().iter() {
            match c {
                Type::Bound(Bound::Discernible) | Type::Bound(Bound::Ordered) => {
                    discernible(env, &t, &Discerned::Bound(tv.clone(), c.clone()))?
                }
                Type::Bound(Bound::Concrete) => discernible(env, &t, &Discerned::Keys)?,
                Type::Bound(b @ (Bound::JsonData | Bound::TomlData)) => {
                    let name = match b {
                        Bound::JsonData => "JSON",
                        _ => "TOML",
                    };
                    if let Some(why) = t.text_refusal(env, text) {
                        let r = |t: &Type| t.resolve_tvars();
                        let what = || format_compact!("{}", r(&t));
                        let (what, why) = match &why {
                            typ::TextRefusal::Untagged { union, member, .. }
                                if let Type::TVar(cell) = member
                                    && cell.binding().is_none() =>
                            {
                                match cell.name.starts_with('_') {
                                    true => (
                                        format_compact!(
                                            "a type not known yet beside {}",
                                            known_members(&r(union))
                                        ),
                                        format_compact!(
                                            "beside another member a member needs a tag \
                                             (a variant's, or an abstract type's name): \
                                             annotate the value's type"
                                        ),
                                    ),
                                    false => (
                                        what(),
                                        format_compact!(
                                            "'{} may be any type, and beside another \
                                             member in {} it would need a tag (a \
                                             variant's, or an abstract type's name)",
                                            cell.name,
                                            r(union)
                                        ),
                                    ),
                                }
                            }
                            typ::TextRefusal::Untagged {
                                union,
                                member,
                                read_by: None,
                            } => (
                                what(),
                                format_compact!(
                                    "{} in {} has no tag (a variant's, or an abstract \
                                     type's name), so what another member writes as \
                                     {name} data may read back as it",
                                    r(member),
                                    r(union)
                                ),
                            ),
                            typ::TextRefusal::Untagged {
                                union,
                                member,
                                read_by: Some(a),
                            } => (
                                what(),
                                format_compact!(
                                    "{} in {} has no tag: in the data {a}'s from_value \
                                     reads, abstract values are written bare, so what \
                                     another member writes there may read back as it",
                                    r(member),
                                    r(union)
                                ),
                            ),
                            typ::TextRefusal::SharedTag { union, tag, a, b } => (
                                what(),
                                format_compact!(
                                    "{} and {} in {} share the tag {tag}, so what one \
                                     writes as {name} data may read back as the other",
                                    r(a),
                                    r(b),
                                    r(union)
                                ),
                            ),
                            typ::TextRefusal::Transparent(t) => (
                                what(),
                                format_compact!(
                                    "{} holds a type that is not abstract, which {name} \
                                     writes bare already",
                                    r(t)
                                ),
                            ),
                            typ::TextRefusal::Grows(def) => (
                                what(),
                                format_compact!(
                                    "{def} grows its parameters as it recurses, so \
                                     whether what it writes as {name} data reads back \
                                     can't be decided"
                                ),
                            ),
                        };
                        match tv.name.starts_with('_') {
                            true => bail!("{c} refuses {what}: {why}"),
                            false => bail!("'{}: {c} refuses {what}: {why}", tv.name),
                        }
                    }
                    let written = tv
                        .cell_constraints()
                        .iter()
                        .any(|c| matches!(c, Type::Bound(Bound::Data)));
                    if *b == Bound::TomlData
                        && written
                        && let Some(m) = t.toml_root_pairs(env)
                    {
                        let t = t.resolve_tvars();
                        let why = typ::toml_root_refusal(Some(&m.resolve_tvars()));
                        match tv.name.starts_with('_') {
                            true => bail!("{c} refuses {t}: {why}"),
                            false => bail!("'{}: {c} refuses {t}: {why}", tv.name),
                        }
                    }
                }
                Type::Bound(Bound::OneNumber) => {
                    if t.one_number_open() {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!(
                                "'{}: OneNumber holds {t}, an open member of which may \
                                 yet be another numeric type: annotate it",
                                tv.name
                            )
                        })?
                    }
                }
                _ => (),
            }
        }
    }
    Ok(())
}

/// Whether a select arm testing for `test` tells it from `rest`, what
/// else reaches the arm, open cells read as any type they may yet bind.
fn told(env: &Env, test: &Type, rest: &Type) -> Result<()> {
    let Some(why) = test.rep_collision_tested(env, rest) else { return Ok(()) };
    let open = |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
    format_with_flags(PrintFlag::DerefTVars, || {
        if let Indiscernible::Pair(a, b) = &why
            && (open(a) || open(b))
        {
            let (test, rest) = (test.resolve_tvars(), rest.resolve_tvars());
            let known = if open(a) { b.resolve_tvars() } else { a.resolve_tvars() };
            bail!(
                "this pattern tests for {test} among {rest}, but a type variable there \
                 isn't known here and may share {known}'s runtime form: annotate it"
            )
        }
        Err(why.untold("this pattern", test, rest))
    })
}

fn discernible(env: &Env, typ: &Type, what: &Discerned) -> Result<()> {
    let found = match what {
        Discerned::Compared { ordered } => {
            typ.indiscernible(env, Open::Unknown, *ordered)
        }
        Discerned::Bound(_, bound) => {
            typ.indiscernible(env, Open::Unknown, *bound == Type::Bound(Bound::Ordered))
        }
        Discerned::Keys => typ.map_key_failure(env, Open::Unknown),
    };
    let Some(why) = found else { return Ok(()) };
    let open = |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
    if let Indiscernible::Pair(a, b) = &why
        && (open(a) || open(b))
    {
        let known = if open(a) { b } else { a };
        let typ = typ.resolve_tvars();
        bail!(
            "the members of {typ} must have distinct runtime forms, but a type variable \
             in it isn't known here and may share {known}'s: annotate it"
        )
    }
    if let Discerned::Bound(tv, bound) = what {
        return Err(typ.not_discernible(bound, &tv.name, &why));
    }
    format_with_flags(PrintFlag::DerefTVars, || {
        let typ = typ.resolve_tvars();
        match (what, why) {
            (Discerned::Compared { .. }, Indiscernible::Abstract(a, kind)) => bail!(
                "can't compare values of {typ}: {}",
                Indiscernible::by_representation(&a, kind)
            ),
            (_, Indiscernible::Abstract(a, kind)) => bail!(
                "a map key type can't hold {}",
                Indiscernible::by_representation(&a, kind)
            ),
            (Discerned::Compared { .. }, Indiscernible::Pair(a, b)) => bail!(
                "can't compare values of {typ}: {} and {} have the same runtime \
                 form; wrap them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            (Discerned::Compared { .. }, Indiscernible::Unordered(r))
                if r.resolve_tvars() == typ =>
            {
                let (kind, how) = Indiscernible::unordered_kind(&r);
                bail!(
                    "can't order {kind}s: they have no order (== and != compare them {how})"
                )
            }
            (Discerned::Compared { .. }, Indiscernible::Unordered(r)) => {
                let (kind, how) = Indiscernible::unordered_kind(&r);
                bail!(
                    "can't order values of {typ}: it holds the {kind} {}, and {kind}s have \
                     no order (== and != compare them {how})",
                    r.resolve_tvars()
                )
            }
            (Discerned::Compared { .. }, Indiscernible::Grows(name)) => bail!(
                "can't compare values of {typ}: {name} grows its parameters as it \
                 recurses, so whether its members have distinct runtime forms can't be \
                 decided"
            ),
            (_, Indiscernible::Grows(name)) => bail!(
                "a map key type can't hold {name}: it grows its parameters as it \
                 recurses, so whether its members have distinct runtime forms can't be \
                 decided"
            ),
            (_, Indiscernible::Pair(a, b)) => bail!(
                "{} and {} can't share a map key type: they have the same runtime \
                 form; wrap them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            (Discerned::Compared { .. }, Indiscernible::Numbers(n)) => bail!(
                "can't order values of {typ}: it holds {}, {}",
                n.resolve_tvars(),
                Indiscernible::NUMBERS
            ),
            (_, Indiscernible::Numbers(n)) => bail!(
                "a map key type can't hold {}, {}",
                n.resolve_tvars(),
                Indiscernible::NUMBERS
            ),
            (Discerned::Compared { .. }, Indiscernible::OpenNumber(n)) => bail!(
                "can't order values of {typ}: it holds {}, {}",
                n.resolve_tvars(),
                Indiscernible::OPEN_NUMBER
            ),
            (_, Indiscernible::OpenNumber(n)) => bail!(
                "a map key type can't hold {}, {}",
                n.resolve_tvars(),
                Indiscernible::OPEN_NUMBER
            ),
            (_, Indiscernible::Unordered(r)) => {
                let (kind, _) = Indiscernible::unordered_kind(&r);
                bail!(
                    "a map key type can't hold the {kind} {}: {kind}s have no order",
                    r.resolve_tvars()
                )
            }
        }
    })
}

/// A deferred import-existence check; see [`CompileCtx::pending_imports`].
#[derive(Debug)]
pub(crate) struct PendingImport {
    pub(crate) scope: ModPath,
    pub(crate) key: compact_str::CompactString,
    pub(crate) pos: crate::SourcePosition,
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

    /// Append a generated block-scope component a scope Rust builds at
    /// run time names by a fresh id.
    pub fn append_block(&self, kind: &str, id: u64) -> Self {
        self.append(block_component(kind, id).as_str())
    }

    /// Append the component of the scope `spec` opens (do/fn/sel/ca
    /// level), named by where it is written; `ordinal` tells apart the
    /// scopes one expression opens.
    pub fn append_place(&self, kind: &str, spec: &Expr, ordinal: Option<usize>) -> Self {
        self.append(place_component(kind, &spec.ori, spec.pos, ordinal).as_str())
    }

    /// The scope covered by a handler of `kind` installed here.
    pub fn with_catch(&self, catch: (BindId, ExprId), kind: HandlerKind) -> Self {
        Self {
            lexical: self.lexical.clone(),
            dynamic: self.dynamic.with_catch(catch, kind),
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

/// What installed a handler. A seq machine's and a `try`'s count the
/// errors raised to the handlers under them, which their steps wait on.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HandlerKind {
    Catch,
    Try,
    Machine,
}

struct DynNode {
    catch: (BindId, ExprId),
    kind: HandlerKind,
    /// The innermost seq machine's handler above this one.
    machine: Option<ErrorHandler>,
    /// The innermost `Try` or `Machine` handler above this one.
    counter: Option<ErrorHandler>,
    /// One past the cycle of the last `abort(..)`; zero before any.
    aborted: AtomicU64,
    /// Raises to this handler.
    raised: AtomicU64,
    /// `abort(..)`s of this (seq machine's) handler.
    aborts: AtomicU64,
    /// Raised to descendant handlers and not yet processed by them.
    nested: AtomicU64,
    /// The boundaries of the nodes that poll this handler's state, which a
    /// raise, a handling or an abort marks (`design/walk_pruning.md`).
    pollers: Mutex<SmallVec<[prune::BoundaryRef; 2]>>,
    /// The handler that reads the error was checked: a raise compiled
    /// later must fit the type it was checked at.
    sealed: AtomicBool,
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

    pub(crate) fn raise(&self, cycle: u64) {
        self.0.raised.fetch_add(1, Ordering::Relaxed);
        self.mark_pollers(cycle);
        let mut counter = self.0.counter.as_ref();
        while let Some(handler) = counter {
            handler.0.nested.fetch_add(1, Ordering::Relaxed);
            handler.mark_pollers(cycle);
            counter = handler.0.counter.as_ref();
        }
    }

    pub(crate) fn handled(&self, cycle: u64) {
        let mut counter = self.0.counter.as_ref();
        while let Some(handler) = counter {
            let pending = handler.0.nested.fetch_sub(1, Ordering::Relaxed);
            debug_assert!(pending > 0, "unraised nested error");
            handler.mark_pollers(cycle);
            counter = handler.0.counter.as_ref();
        }
    }

    /// A seq's `abort(..)` fired: the run fails with no error in flight.
    pub(crate) fn abort(&self, cycle: u64) {
        self.0.aborts.fetch_add(1, Ordering::Relaxed);
        self.0.aborted.store(cycle.wrapping_add(1), Ordering::Relaxed);
        self.mark_pollers(cycle);
    }

    /// A node inside `boundary` polls this handler's state.
    pub(crate) fn poll_from(&self, boundary: &Option<prune::BoundaryRef>) {
        if let Some(b) = boundary {
            self.0.pollers.lock().push(b.clone())
        }
    }

    /// Undo one [`Self::poll_from`].
    pub(crate) fn unpoll(&self, boundary: &Option<prune::BoundaryRef>) {
        if let Some(b) = boundary {
            let mut pollers = self.0.pollers.lock();
            if let Some(i) = pollers.iter().position(|p| p == b) {
                pollers.swap_remove(i);
            }
        }
    }

    /// A node inside `boundary` polls this handler and the machine
    /// covering it.
    pub(crate) fn polled_by(&self, boundary: &Option<prune::BoundaryRef>) {
        self.poll_from(boundary);
        if let Some(m) = self.machine().filter(|m| !m.same(self)) {
            m.poll_from(boundary)
        }
    }

    /// Undo one [`Self::polled_by`].
    pub(crate) fn unpolled_by(&self, boundary: &Option<prune::BoundaryRef>) {
        self.unpoll(boundary);
        if let Some(m) = self.machine().filter(|m| !m.same(self)) {
            m.unpoll(boundary)
        }
    }

    fn mark_pollers(&self, cycle: u64) {
        self.0.pollers.lock().iter().for_each(|b| b.mark(cycle))
    }

    /// Whether an `abort(..)` fired in `cycle`: a step entered in that
    /// cycle is entered into a failed run.
    pub(crate) fn aborted_in(&self, cycle: u64) -> bool {
        self.0.aborted.load(Ordering::Relaxed) == cycle.wrapping_add(1)
    }

    /// Changes at every `abort(..)`: a run that began under one stamp
    /// and sees another was aborted.
    pub(crate) fn abort_stamp(&self) -> u64 {
        self.0.aborted.load(Ordering::Relaxed)
    }

    /// Changes at every raise and every `abort(..)`: a run that began
    /// under one generation and sees another failed.
    pub(crate) fn generation(&self) -> u64 {
        self.raised().wrapping_add(self.0.aborts.load(Ordering::Relaxed))
    }

    /// The raises to this handler.
    pub(crate) fn raised(&self) -> u64 {
        self.0.raised.load(Ordering::Relaxed)
    }

    pub(crate) fn kind(&self) -> HandlerKind {
        self.0.kind
    }

    /// The handler of the innermost seq machine covering this one,
    /// itself included.
    pub(crate) fn machine(&self) -> Option<ErrorHandler> {
        match self.0.kind {
            HandlerKind::Machine => Some(self.clone()),
            HandlerKind::Catch | HandlerKind::Try => self.0.machine.clone(),
        }
    }

    /// The handler's allocation address: equal for every scope under
    /// one catch.
    pub(crate) fn identity(&self) -> usize {
        Arc::as_ptr(&self.0) as *const () as usize
    }

    pub(crate) fn parent(&self) -> DynScope {
        self.0.parent.clone()
    }

    pub(crate) fn seal(&self) {
        self.0.sealed.store(true, Ordering::Relaxed)
    }

    pub(crate) fn sealed(&self) -> bool {
        self.0.sealed.load(Ordering::Relaxed)
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

    pub fn with_catch(&self, catch: (BindId, ExprId), kind: HandlerKind) -> Self {
        let parent = self.0.as_ref();
        Self(Some(ErrorHandler(Arc::new(DynNode {
            catch,
            kind,
            machine: parent.and_then(ErrorHandler::machine),
            counter: parent.and_then(|p| match p.0.kind {
                HandlerKind::Catch => p.0.counter.clone(),
                HandlerKind::Try | HandlerKind::Machine => Some(p.clone()),
            }),
            aborted: AtomicU64::new(0),
            raised: AtomicU64::new(0),
            aborts: AtomicU64::new(0),
            nested: AtomicU64::new(0),
            pollers: Mutex::new(SmallVec::new()),
            sealed: AtomicBool::new(false),
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

/// Compile the expression into a node graph in the given context and
/// scope, returning the root node.
pub fn compile<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<Node<R>> {
    compile_stmt(ctx, flags, scope, spec).map(|(n, _)| n)
}

/// The passes every node runs after it is built and before it updates:
/// the check (typecheck0 and its settle), elaboration (typecheck1 and
/// its settle), function-property analysis, typedef resolution-cell
/// seeding (in both modes) and fusion; only the check under
/// [`CFlag::CheckOnly`]. On `Err` the caller unwinds: it deletes the
/// node, whose references cancel what the compile deferred, then
/// [`ExecCtx::drop_deferred`], then restores its env.
pub fn check_and_fuse<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    flags: BitFlags<CFlag>,
    node: &mut Node<R>,
) -> Result<()> {
    check_and_fuse_inner(ctx, flags, node, &|_, _| Ok(()))?;
    ctx.apply_deferred();
    Ok(())
}

/// What `verify` asks of the checked program, before it elaborates.
type Verify<'a, R> = &'a dyn Fn(&CompileCtx<R>, &Node<R>) -> Result<()>;

fn check_and_fuse_inner<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    flags: BitFlags<CFlag>,
    node: &mut Node<R>,
    verify: Verify<R>,
) -> Result<()> {
    let st = Instant::now();
    let _level = typ::tvar::AtLevel::enter(typ::tvar::Level::TOP);
    let p = profile::phase(Phase::Typecheck0);
    if let Err(e) = node
        .typecheck0(ctx)
        .and_then(|()| drain_pending_settles(ctx))
        .and_then(|()| check_pending_names(ctx))
        .and_then(|()| verify(ctx, node))
    {
        let e = resite_pending_name(ctx, e);
        ctx.pending_settles.clear();
        ctx.pending_settles.push(SettleFrame::default());
        ctx.pending_names.clear();
        return Err(e);
    }
    drop(p);
    if flags.contains(CFlag::CheckOnly) {
        return Ok(());
    }
    // an instance elaborates under its definition's env, where a type
    // its signature carries in from the caller's scope does not resolve
    ctx.env.seed_typedef_refs();
    let p = profile::phase(Phase::Typecheck1);
    if let Err(e) = node.typecheck1(ctx) {
        ctx.pending_settles.clear();
        ctx.pending_settles.push(SettleFrame::default());
        return Err(e);
    }
    drop(p);
    let p = profile::phase(Phase::Settle);
    drain_pending_settles(ctx)?;
    drop(p);
    info!("typecheck time {:?}", st.elapsed());
    let tail_loops = analysis::analyze(node, ctx)?;
    ctx.env.seed_typedef_refs();
    seed_node_types(&ctx.env, node, &mut LPooled::take());
    if fusion_on(flags) && ctx.fusion.available() {
        let st = Instant::now();
        let p = profile::phase(Phase::Fusion);
        let fused = fusion::TypeMemo::scope(|| fusion::fuse(node, ctx));
        ctx.fusion.link();
        drop(p);
        fused?;
        info!("fusion time {:?}", st.elapsed());
        analysis::check_tail_loops(tail_loops, node)?;
    }
    Ok(())
}

/// Fill the resolution cell of every name the types of `node` and the
/// nodes under it hold, once the check has ended, as a definition's table
/// seeds its body's rows (`DefTable::record`): a value carries the types
/// it was built at (`typ::regular`). A module's statements are seeded
/// under the module's own env (`Module::typecheck1_nodes`).
pub(crate) fn seed_node_types<R: Rt>(env: &Env, node: &Node<R>, seen: &mut AddrSet) {
    stack::ensure_sufficient(|| {
        if let NodeView::Module(_) = node.view() {
            return;
        }
        node.typ().seed_names_seen(env, seen);
        fusion::for_each_child(node, &mut |c| seed_node_types(env, c, seen))
    })
}

/// Whether a compile under `flags` asks for fusion.
pub fn fusion_on(flags: BitFlags<CFlag>) -> bool {
    !flags.contains(CFlag::FusionDisabled)
}

/// Drain the deferred terminal settles ([`CompileCtx::pending_settles`])
/// after a top-level statement, once every writer for the drained
/// sites has run.
pub(crate) fn drain_pending_settles<R: Rt>(ctx: &mut CompileCtx<R>) -> Result<()> {
    use expr::At;
    let pending = mem::take(ctx.pending_settles.last_mut().expect("root settle frame"));
    PendingSettle::drain(&pending, &ctx.env, |spec, e| Err(e.at(&**spec)))
}

/// A question a check leaves open until it ends.
pub(crate) enum Pending {
    /// A type name a definition's written types hold that did not
    /// resolve at its check (a `use` the interface defers may name it
    /// later); it must name something once the check ends.
    Name(typ::TypeRef, Expr),
    /// A typedef whose body named something not defined yet: it must be
    /// contractive once everything is.
    Contractive(ModPath, ArcStr, Expr),
}

/// A written type name that did not resolve at its definition's check
/// must name something once the check ends, applied rightly.
pub(crate) fn check_pending_names<R: Rt>(ctx: &mut CompileCtx<R>) -> Result<()> {
    use expr::At;
    for p in mem::take(&mut ctx.pending_names) {
        match p {
            Pending::Name(tr, spec) => {
                if !tr.names_something(&ctx.env) {
                    return Err(typ::UnresolvableRef::error(&tr, &ctx.env).at(&spec));
                }
                tr.check_application(&ctx.env).at(&spec)?;
            }
            Pending::Contractive(scope, name, spec) => {
                ctx.env.check_contractive(&scope, &name).at(&spec)?;
            }
        }
    }
    Ok(())
}

/// A check that failed on a type name a definition deferred (a later
/// call expanded its signature first) is placed at the definition.
pub(crate) fn resite_pending_name<R: Rt>(
    ctx: &CompileCtx<R>,
    e: anyhow::Error,
) -> anyhow::Error {
    use expr::At;
    let Some(u) = e.downcast_ref::<typ::UnresolvableRef>() else { return e };
    let deferred = ctx.pending_names.iter().find_map(|p| match p {
        Pending::Name(tr, spec)
            if tr.name == u.name && tr.scope == u.scope && tr.pos == u.pos =>
        {
            Some((tr, spec))
        }
        _ => None,
    });
    match deferred {
        Some((tr, spec)) => typ::UnresolvableRef::error(tr, &ctx.env).at(spec),
        None => e,
    }
}

/// Refuse now each deferred typedef a `use` naming a type just
/// completed, before anything later in the run expands it, and forget
/// those it settled.
pub(crate) fn recheck_pending_contractive<R: Rt>(ctx: &mut CompileCtx<R>) -> Result<()> {
    use expr::At;
    let mut res = Ok(());
    let CompileCtx { pending_names, env, .. } = ctx;
    pending_names.retain(|p| match p {
        Pending::Contractive(_, _, _) if res.is_err() => true,
        Pending::Contractive(scope, name, spec) => {
            match env.check_contractive(scope, name).at(spec) {
                Ok(u) => u == typ::Unguarded::Unknown,
                Err(e) => {
                    res = Err(e);
                    true
                }
            }
        }
        Pending::Name(..) => true,
    });
    res
}

/// Record the names `t`, written at `spec`, holds that do not resolve
/// yet ([`CompileCtx::pending_names`]); refuse one that resolves and is
/// applied wrongly.
pub(crate) fn defer_unresolved_names<R: Rt>(
    ctx: &mut CompileCtx<R>,
    t: &Type,
    spec: &Expr,
) -> Result<()> {
    use expr::At;
    let mut names = Vec::new();
    let res = t.unresolved_names(&ctx.env, &mut names).at(spec);
    ctx.pending_names.extend(names.into_iter().map(|tr| Pending::Name(tr, spec.clone())));
    res
}

/// Warn at each name the written type `t` holds that means a type its
/// block declares below it while another of the name is in view there
/// ([`env::Env::later_block_type_note`]): in a definition's check, never
/// again in an instance's body.
pub(crate) fn warn_later_block_names<R: Rt>(
    ctx: &CompileCtx<R>,
    flags: BitFlags<CFlag>,
    t: &Type,
) -> Result<()> {
    if ctx.body.as_ref().is_some_and(|b| !b.checking) {
        return Ok(());
    }
    let mut res = Ok(());
    t.for_each_written_name(&mut |tr| {
        if res.is_ok()
            && let (Some(pos), Some(ori)) = (tr.pos, &tr.ori)
            && let Some(note) = ctx.env.later_block_type_note(tr)
        {
            let width = tr.name.chars().count() as i32;
            let end = SourcePosition { column: pos.column + width, ..pos };
            res = ctx.env.warn_at(flags, ori, pos, end, note)
        }
    });
    res
}

/// A `use` whose name did not exist at its compile position must name
/// something by the end of the compile that deferred it.
pub(crate) fn check_pending_imports<R: Rt>(ctx: &mut ExecCtx<'_, R>) -> Result<()> {
    for p in mem::take(&mut ctx.pending_imports) {
        let Some(e) = ctx.env.names.get(&p.scope).and_then(|sn| sn.imports.get(&p.key))
        else {
            continue;
        };
        if !ctx.env.import_target_exists(e) {
            return Err(::anyhow::anyhow!("use: no `{}` in `{}`", e.name, e.scope)
                .context(expr::ParserContext { ori: p.ori.clone(), pos: p.pos }));
        }
    }
    Ok(())
}

/// [`compile`] for drivers that compile top-level statements one at a
/// time (REPL, checker). A top-level `catch(e) expr` advances the scope
/// for the statements that follow: thread the returned scope into the
/// next compile, or later statements escape the catch's coverage.
pub fn compile_stmt<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
) -> Result<(Node<R>, Scope)> {
    compile_top(ctx, flags, spec, &|_, _| Ok(()), |ctx, spec, top_id| {
        let (n, scope) = node::compile_statement(
            ctx,
            flags,
            spec,
            scope,
            top_id,
            node::StmtAt::TopLevel,
        )?;
        node::defer_typedef_names(ctx, &n)?;
        Ok((n, scope))
    })
}

/// Compile a script's statements `exprs` as the one block it runs as,
/// with its names at `scope` itself: every statement is built before any
/// is checked, so a `let` takes its type from the writers below it.
/// `spec` is the block's expression; `sig`, its interface, is checked
/// over its names once the script is.
pub fn compile_script<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: Expr,
    exprs: &Arc<[Expr]>,
    sig: Option<&expr::Sig>,
) -> Result<Node<R>> {
    let verify = |cx: &CompileCtx<R>, block: &Node<R>| match (sig, block.view()) {
        (Some(sig), NodeView::Block(b)) => {
            check_script_sig(cx, sig, &scope.lexical, &b.children)
        }
        (Some(_), _) => bail!("BUG: a script compiled to no block"),
        (None, _) => Ok(()),
    };
    compile_top(ctx, flags, spec, &verify, |ctx, spec, top_id| {
        let n = node::Block::compile(
            ctx,
            flags,
            Arc::new(spec.clone()),
            scope,
            top_id,
            false,
            exprs,
        )?;
        Ok((n, scope.clone()))
    })
    .map(|(n, _)| n)
}

/// Every node's written span and type snapshot under the checked `nodes`,
/// pre-order, for tooling that asks for types (`Ide::expr_types`). Lambda
/// bodies are not visited (a body compiles per call site, so a site in it
/// has no one type) and a fused region is opaque: record from a check
/// with fusion off.
pub fn record_expr_types<R: Rt>(nodes: &[Node<R>], out: &mut Vec<ide::ExprTypeSite>) {
    for n in nodes {
        fusion::for_each_node(n, &mut |n| {
            let spec = n.spec();
            out.push(ide::ExprTypeSite {
                ori: spec.ori.clone(),
                pos: spec.pos,
                end: spec.end.get(),
                typ: n.typ().snapshot(),
                cell: matches!(n.typ(), typ::Type::TVar(_)),
                bind: match n.view() {
                    NodeView::Ref(r) => Some(r.id),
                    _ => None,
                },
            })
        });
    }
}

/// The live holds on bindings whose definitions calls bound statically
/// ([`CompileCtx::static_callees`]): per binding, each holding call with
/// the number of holders that hold it, and the bindings that are one
/// variable. One index serves a context and every compile task it forks.
#[derive(Debug, Default)]
pub(crate) struct StaticCallees(Mutex<Holds>);

#[derive(Debug, Default)]
struct Holds {
    calls: AHashMap<BindId, SmallVec<[(Arc<expr::Expr>, u32); 1]>>,
    joined: AHashMap<BindId, Side>,
    /// The holds of a body being replaced, which a write its replacement
    /// makes does not see ([`StaticCallees::set_aside`]).
    aside: AHashMap<(BindId, ExprId), u32>,
}

/// A binding's side of the interface pairs it is in
/// ([`StaticCallees::join`]).
#[derive(Debug)]
enum Side {
    /// It implements the `val` whose binding this is.
    Inner(BindId),
    /// It is a `val`'s, implemented by these: a reload's body joins its
    /// own before the body it replaces retires.
    Outer(SmallVec<[BindId; 1]>),
}

impl Side {
    /// The bindings on the other side.
    fn with(&self) -> &[BindId] {
        match self {
            Side::Inner(outer) => std::slice::from_ref(outer),
            Side::Outer(inners) => inners,
        }
    }
}

impl StaticCallees {
    /// Hold `id` for the call `call`, until the hold drops.
    fn hold(index: &Arc<Self>, id: BindId, call: &Arc<expr::Expr>) -> StaticHold {
        let mut held = index.0.lock();
        let calls = held.calls.entry(id).or_default();
        match calls.iter_mut().find(|(c, _)| c.id == call.id) {
            Some((_, n)) => *n += 1,
            None => calls.push((call.clone(), 1)),
        }
        StaticHold { index: index.clone(), id, call: call.clone() }
    }

    /// A call holding `id`, if one is live and not set aside.
    fn held(&self, id: BindId) -> Option<Arc<expr::Expr>> {
        let held = self.0.lock();
        let aside = |call: ExprId| held.aside.get(&(id, call)).copied().unwrap_or(0);
        let calls = held.calls.get(&id)?;
        calls.iter().find(|(c, n)| *n > aside(c.id)).map(|(c, _)| c.clone())
    }

    /// Leave `holds`, a replaced body's, out of [`Self::held`] until the
    /// guard drops: the body goes when its replacement compiles.
    pub(crate) fn set_aside(
        index: &Arc<Self>,
        holds: Vec<(BindId, ExprId)>,
    ) -> AsideHolds {
        let mut held = index.0.lock();
        for key in holds.iter() {
            *held.aside.entry(*key).or_default() += 1;
        }
        AsideHolds { index: index.clone(), holds }
    }

    /// `inner` and `outer` are one variable, a write to either reaching
    /// the other: the binding implementing an interface's `val`, and the
    /// `val`'s.
    pub(crate) fn join(&self, inner: BindId, outer: BindId) {
        let mut held = self.0.lock();
        held.joined.insert(inner, Side::Inner(outer));
        let side =
            held.joined.entry(outer).or_insert_with(|| Side::Outer(SmallVec::new()));
        if let Side::Outer(inners) = side
            && !inners.contains(&inner)
        {
            inners.push(inner)
        }
    }

    /// `inner` and `outer` are no longer one variable.
    pub(crate) fn unjoin(&self, inner: BindId, outer: BindId) {
        let mut held = self.0.lock();
        if matches!(held.joined.get(&inner), Some(Side::Inner(o)) if *o == outer) {
            held.joined.remove(&inner);
        }
        if let Some(Side::Outer(inners)) = held.joined.get_mut(&outer) {
            inners.retain(|id| *id != inner);
            if inners.is_empty() {
                held.joined.remove(&outer);
            }
        }
    }

    /// `id` and every binding it is one variable with.
    fn variable(&self, id: BindId) -> SmallVec<[BindId; 2]> {
        let held = self.0.lock();
        let mut ids: SmallVec<[BindId; 2]> = smallvec::smallvec![id];
        held.close_over_joins(&mut ids);
        ids
    }

    /// Add to `ids` every binding one of them is one variable with.
    pub(crate) fn add_joined(&self, ids: &mut IntSet<BindId>) {
        let held = self.0.lock();
        if held.joined.is_empty() || ids.is_empty() {
            return;
        }
        let mut all: SmallVec<[BindId; 8]> = ids.iter().copied().collect();
        held.close_over_joins(&mut all);
        ids.extend(all);
    }

    /// The interface pairs, `(inner, outer)`, with a side among
    /// `delivered`.
    pub(crate) fn pairs_of(
        &self,
        delivered: impl Iterator<Item = BindId>,
        pairs: &mut Vec<(BindId, BindId)>,
    ) {
        let held = self.0.lock();
        if held.joined.is_empty() {
            return;
        }
        for id in delivered {
            match held.joined.get(&id) {
                None => (),
                Some(Side::Inner(outer)) => pairs.push((id, *outer)),
                Some(Side::Outer(inners)) => {
                    pairs.extend(inners.iter().map(|inner| (*inner, id)))
                }
            }
        }
        pairs.sort_unstable();
        pairs.dedup();
    }

    /// Forget `id`, a deleted binding: nothing can write it again.
    pub(crate) fn forget(&self, id: BindId) {
        let mut held = self.0.lock();
        held.calls.remove(&id);
        let Some(side) = held.joined.remove(&id) else { return };
        for with in side.with() {
            match held.joined.get_mut(with) {
                Some(Side::Outer(inners)) => {
                    inners.retain(|i| *i != id);
                    if inners.is_empty() {
                        held.joined.remove(with);
                    }
                }
                Some(Side::Inner(_)) => {
                    held.joined.remove(with);
                }
                None => (),
            }
        }
    }

    fn release(&self, id: BindId, call: ExprId) {
        let mut held = self.0.lock();
        let held = &mut held.calls;
        let Some(calls) = held.get_mut(&id) else { return };
        if let Some(i) = calls.iter().position(|(c, _)| c.id == call) {
            calls[i].1 -= 1;
            if calls[i].1 == 0 {
                calls.swap_remove(i);
            }
        }
        if calls.is_empty() {
            held.remove(&id);
        }
    }
}

/// Holds [`StaticCallees::set_aside`] left out, back when it drops.
pub(crate) struct AsideHolds {
    index: Arc<StaticCallees>,
    holds: Vec<(BindId, ExprId)>,
}

impl Drop for AsideHolds {
    fn drop(&mut self) {
        let mut held = self.index.0.lock();
        for key in self.holds.iter() {
            if let Some(n) = held.aside.get_mut(key) {
                *n -= 1;
                if *n == 0 {
                    held.aside.remove(key);
                }
            }
        }
    }
}

/// A node's hold on a binding its call bound statically: while it lives
/// a `<-` to the binding is refused ([`CompileCtx::mark_connect_target`]).
/// The holders are the call that bound it (a call site, an alias `let`)
/// and the kernel that replaced them; each carries its holds through an
/// image.
#[derive(Debug)]
pub(crate) struct StaticHold {
    index: Arc<StaticCallees>,
    pub(crate) id: BindId,
    pub(crate) call: Arc<expr::Expr>,
}

impl Holds {
    /// Extend `ids` with every binding one of them is one variable with.
    fn close_over_joins<A: smallvec::Array<Item = BindId>>(&self, ids: &mut SmallVec<A>) {
        let mut i = 0;
        while let Some(id) = ids.get(i).copied() {
            for with in self.joined.get(&id).map(Side::with).unwrap_or_default() {
                if !ids.contains(with) {
                    ids.push(*with)
                }
            }
            i += 1;
        }
    }
}

impl Drop for StaticHold {
    fn drop(&mut self) {
        self.index.release(self.id, self.call.id)
    }
}

/// The registries a compile or a registration read writes, as they were
/// before it, so a failure, or a check that runs nothing, puts them back.
pub struct Saved {
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
    /// Takes the mutable view: a branch's snapshot must be its own, the
    /// one [`Self::restore`] writes, never the parent's it reads through.
    pub fn take<R: Rt>(ctx: &mut CompileCtx<R>) -> Self {
        // every field is named, so a new one is decided here: program
        // state a failed compile rolls back, or scratch
        let CompileCtx {
            registry: _,
            control: _,
            tags,
            env,
            lambda_defs,
            bind_to_lambda,
            batch_connect_targets,
            connect_targets,
            static_callees: _,
            written: _,
            block_typedefs: _,
            builtin_bindings,
            rec_defs: _,
            def_gate_params: _,
            instance_fn_params: _,
            fn_param_args: _,
            def_gate_depth: _,
            resolving_lambdas: _,
            defaults_in_check: _,
            holder_reads: _,
            fn_forward_resolutions,
            body: _,
            pending_settles: _,
            pending_imports: _,
            pending_names: _,
            def_assertions: _,
            attr_census: _,
            attr_dispatched: _,
            attr_absorbed: _,
            pending_refs: _,
            discarded: _,
            boundary: _,
            profile: _,
            fusion: _,
        } = ctx;
        Saved {
            env: env.clone(),
            lambda_defs: lambda_defs.clone(),
            bind_to_lambda: bind_to_lambda.clone(),
            builtin_bindings: builtin_bindings.clone(),
            fn_forward_resolutions: fn_forward_resolutions.clone(),
            connect_targets: connect_targets.clone(),
            batch_connect_targets: batch_connect_targets.clone(),
            tags: tags.clone(),
        }
    }

    pub fn restore<R: Rt>(self, ctx: &mut CompileCtx<R>) {
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
fn compile_top<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    verify: Verify<R>,
    build: impl FnOnce(&mut ExecCtx<'_, R>, &Expr, ExprId) -> Result<(Node<R>, Scope)>,
) -> Result<(Node<R>, Scope)> {
    let _profile = profile::phase(Phase::Compile);
    let input = Input::enter();
    let top_id = spec.id;
    ctx.open_compile_frame(top_id);
    let saved = Saved::take(ctx);
    let st = Instant::now();
    let build_profile = profile::phase(Phase::BuildGraph);
    let compiled = build(ctx, &spec, top_id);
    drop(build_profile);
    let (mut node, out_scope) = match compiled {
        Ok(n) => n,
        Err(e) => {
            let e = input.refused(ctx, e);
            ctx.drop_deferred();
            saved.restore(ctx);
            return Err(e);
        }
    };
    info!("compile time {:?}", st.elapsed());
    if let Err(err) = check_pending_imports(ctx) {
        let err = input.refused(ctx, err);
        return Err(abandon_stmt(ctx, node, saved, err));
    }
    if let Err(e) = check_and_fuse_inner(ctx, flags, &mut node, verify) {
        let e = input.refused(ctx, e);
        return Err(abandon_stmt(ctx, node, saved, e));
    }
    ctx.apply_deferred();
    if fusion_on(flags)
        && !flags.contains(CFlag::CheckOnly)
        && let Err(e) = check_attribute_census(ctx)
    {
        return Err(abandon_stmt(ctx, node, saved, e));
    }
    if let Err(e) = input.end(ctx, &ctx.env.lexical, Some(node.typ())) {
        return Err(abandon_stmt(ctx, node, saved, expr::At::at(e, &spec)));
    }
    Ok((node, out_scope))
}

// CR claude for claude: [readability/low] An input's refusal over an earlier input's cell
// names bindings but never the type to declare ("this input would decide a type of an
// earlier input: annotate `j` (line 1, column 5 in `let j = s.g`)"): OwnWrites records only
// the cell that would be decided, not what it would have become. It also names bindings
// whose annotation cannot lift the refusal (`r` from `let r = &j`, g1; `w`, ld0 and n2), and
// a refused load names only the first binding it tried to decide (rl1 names j; its check
// then fails on fs too). Fix: record the would-be type beside the cell in OwnWrites and print
// it as the declaration, and name only bindings whose written type holds the cell. Review
// r14 D-r4-1. probe: ~/ptmp/cr29/scratch/review-r14/D-r4-1/{all/review-r14_D-r2-1__g1.txt,
// new/rl1.txt} (./r.sh lane FILE) (input-refusal-type-01)
/// A top-level input while it compiles: a REPL input, a script or a
/// dynamic module's load. It is a compile task of its own: what an
/// earlier input made reads as decided, and a decision about it is not
/// made.
pub(crate) struct Input {
    since: BindId,
    writes: typ::tvar::OwnWrites,
    _task: typ::tvar::InTask,
    _level: typ::tvar::AtLevel,
}

impl Input {
    pub(crate) fn enter() -> Self {
        use typ::tvar::{AtLevel, InTask, Level, OwnWrites, new_task};
        let _level = AtLevel::enter(Level::TOP);
        let task = new_task();
        let _task = InTask::enter(task);
        let writes = OwnWrites::enter_input(task);
        Self { since: BindId::new(), writes, _task, _level }
    }

    /// The end of the input, once its checks pass: what it leaves open
    /// in the types of the bindings `named` names that it made, and of its
    /// value `typ`, settles ([`settle_input`]); a decision it would have
    /// made about an earlier input's type refuses it, naming the bindings
    /// that hold the type.
    pub(crate) fn end<R: Rt>(
        &self,
        ctx: &CompileCtx<R>,
        named: &env::Lexical,
        typ: Option<&Type>,
    ) -> Result<()> {
        settle_input(ctx, named, self.since, typ)?;
        match self.earlier_decision(ctx) {
            None => Ok(()),
            Some(m) => bail!("{m}"),
        }
    }

    /// `e`, a refusal of the input, first told as the decision about an
    /// earlier input's type it would have made, where it tried one: not
    /// made, it may be what the check then missed.
    pub(crate) fn refused<R: Rt>(
        &self,
        ctx: &CompileCtx<R>,
        e: anyhow::Error,
    ) -> anyhow::Error {
        match self.earlier_decision(ctx) {
            None => e,
            Some(m) => e.context(m),
        }
    }

    fn earlier_decision<R: Rt>(&self, ctx: &CompileCtx<R>) -> Option<CompactString> {
        let w = self.writes.foreign()?;
        Some(format_compact!(
            "this input would decide a type of an earlier input: {}",
            earlier_binds(ctx, self.since, &w)
        ))
    }
}

/// The bindings an earlier input made (ids below `since`) whose types
/// hold what `w` wrote, as the declarations to annotate.
fn earlier_binds<R: Rt>(
    ctx: &CompileCtx<R>,
    since: BindId,
    w: &typ::tvar::ForeignWrite,
) -> CompactString {
    use std::fmt::Write;
    let mut held: LPooled<Vec<&Arc<env::Bind>>> = ctx
        .env
        .by_id
        .iter()
        .map(|(_, b)| b)
        .filter(|b| b.id < since && annotatable(b) && w.held_by(&b.typ))
        .collect();
    held.sort_unstable_by_key(|b| b.id);
    let mut out = CompactString::new("");
    for (i, b) in held.iter().take(3).enumerate() {
        let sep = if i == 0 { "annotate " } else { ", " };
        let _ = write!(out, "{sep}`{}` ({})", b.name, typ::tvar::BoundAt(b, None));
    }
    if out.is_empty() {
        out.push_str("annotate the binding it decides");
    }
    out
}

/// Whether a refusal may ask for `b` to be annotated: a lowering's binding
/// may not, nor the cell a reference to an unnamed expression binds,
/// which is named by its text (`&mut never()`).
fn annotatable(b: &env::Bind) -> bool {
    !is_lowered(&b.name) && !b.name.starts_with('&')
}

/// Settle what an input leaves open in the types of the bindings it made
/// (ids from `since`) that `named` names, where a later input can reach
/// them, and of its value `typ`, so a later input reads it as decided
/// ([`typ::settle::settle_input`]). A binding inside a definition's body
/// or an instance, and one a lowering made, is reached only through what
/// names it.
fn settle_input<R: Rt>(
    ctx: &CompileCtx<R>,
    named: &env::Lexical,
    since: BindId,
    typ: Option<&Type>,
) -> Result<()> {
    let mut made: LPooled<Vec<&Arc<env::Bind>>> = named
        .binds
        .into_iter()
        .flat_map(|(_, names)| names.into_iter().map(|(_, id)| *id))
        .filter(|id| *id >= since)
        .filter_map(|id| ctx.env.by_id.get(&id))
        .filter(|b| !is_lowered(&b.name))
        .collect();
    made.sort_unstable_by_key(|b| b.id);
    made.dedup_by_key(|b| b.id);
    let mut types: LPooled<Vec<&Type>> = made.iter().map(|b| &b.typ).collect();
    types.extend(typ);
    typ::settle::settle_input(&ctx.env, &types).map_err(|(i, e)| match made.get(i) {
        Some(b) => e.context(format_compact!(
            "settling the type of `{}` ({})",
            b.name,
            typ::tvar::BoundAt(b, None)
        )),
        None => e,
    })
}

pub(crate) fn check_attribute_census<R: Rt>(ctx: &CompileCtx<R>) -> Result<()> {
    let census = ctx.attr_census.lock();
    // with no JIT no walk ran: `#[native]` cannot hold, the others
    // assert nothing
    if !census.is_empty()
        && let Err(e) = ctx.fusion.jit().map(|_| ())
    {
        let native = <Native as Attribute<R>>::NAME;
        let spec = census.iter().find(|spec| {
            let attrs = spec.dec.as_ref().map(|d| &*d.attrs).unwrap_or(&[]);
            attrs.iter().any(|a| a.name == native)
        });
        if let Some(spec) = spec {
            let e = expr::At::at(e.context("#[native] cannot hold"), spec);
            drop(census);
            return Err(e);
        }
    } else if !census.is_empty() {
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
                return Err(e);
            }
        }
    }
    Ok(())
}

pub(crate) fn with_attribute_frame<R: Rt, T>(
    ctx: &mut CompileCtx<R>,
    f: impl FnOnce(&mut CompileCtx<R>) -> T,
) -> T {
    let census = mem::take(ctx.attr_census.get_mut());
    let dispatched = mem::take(ctx.attr_dispatched.get_mut());
    let absorbed = mem::take(ctx.attr_absorbed.get_mut());
    let result = f(ctx);
    *ctx.attr_census.get_mut() = census;
    *ctx.attr_dispatched.get_mut() = dispatched;
    *ctx.attr_absorbed.get_mut() = absorbed;
    result
}

/// Unwind a statement that was built but failed its checks: delete what
/// it registered (runtime refs, lambda and `<-` target entries), then
/// restore the environment it started from.
fn abandon_stmt<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    mut node: Node<R>,
    saved: Saved,
    e: anyhow::Error,
) -> anyhow::Error {
    node.delete(ctx);
    ctx.drop_deferred();
    saved.restore(ctx);
    e
}

/// Whether `t` holds a generic cell, a variable of a function it holds that
/// a read copies: not a rank-2 type's quantifier, generic whatever compiled
/// it, but a declared signature's own, its scheme's.
pub(crate) fn holds_generic(t: &Type) -> bool {
    let mut cells: LPooled<AHashMap<usize, typ::TVar>> = LPooled::take();
    t.reached_cells(&mut cells);
    let mut generic = cells
        .iter()
        .filter(|(_, tv)| tv.level().is_generic() && !tv.is_quantifier_rigid())
        .peekable();
    if generic.peek().is_none() {
        return false;
    }
    let mut quantified: LPooled<AHashSet<usize>> = LPooled::take();
    t.each_quantifier(&mut |_, tv| {
        if tv.is_formal_quantifier() {
            quantified.insert(tv.cell_addr());
        }
    });
    generic.any(|(addr, _)| !quantified.contains(addr))
}

#[cfg(test)]
mod handler_tests {
    use super::*;

    fn under(scope: &DynScope, kind: HandlerKind) -> DynScope {
        scope.with_catch((BindId::new(), ExprId::new()), kind)
    }

    #[test]
    fn nested_errors_count_only_where_a_step_waits() {
        let machine = under(&DynScope::root(), HandlerKind::Machine);
        let outer = under(&machine, HandlerKind::Catch);
        let try_ = under(&outer, HandlerKind::Try);
        let inner = under(&under(&try_, HandlerKind::Catch), HandlerKind::Catch);
        let [machine, outer, try_, inner] =
            [machine, outer, try_, inner].map(|s| s.handler().unwrap());
        assert!(inner.machine().unwrap().same(&machine));
        assert!(try_.machine().unwrap().same(&machine));
        inner.raise(0);
        assert!(try_.has_nested_errors() && machine.has_nested_errors());
        assert!(!outer.has_nested_errors());
        inner.handled(0);
        assert!(!try_.has_nested_errors() && !machine.has_nested_errors());
    }
}
