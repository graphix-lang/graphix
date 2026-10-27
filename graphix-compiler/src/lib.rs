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
    is_lowered, list, mod_root, reference, rep, shared_map, tracked, typ, valarray_of,
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
pub use stack::{Budget, Control, CtlFlag, ParMode, PruneMode, with_control};
pub use stack::{set_activation_budget, set_stack_budget};
pub mod tval;

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
    typ::{FnType, Indiscernible, Open, Type},
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
    fmt::Debug,
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
    /// values read stale. An arm's first entry, a seq step entered for a
    /// run, a birth inside a resume.
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
}

impl Event {
    pub fn new() -> Self {
        Event {
            view: View::Cycle,
            variables: branch::Layered::default(),
            custom: Arc::new(Mutex::new(IntMap::default())),
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
        }
    }

    /// Apply what the forked branch's event `child` delivered.
    pub(crate) fn merge(&mut self, child: Self) {
        let Self { view: _, variables, custom: _ } = child;
        self.variables.merge(variables);
    }

    pub fn clear(&mut self) {
        let Self { view, variables, custom } = self;
        *view = View::Cycle;
        variables.clear();
        custom.lock().clear();
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
}

impl Refs {
    /// A walk of one body alone: callee instance bodies are left out.
    pub(crate) fn without_callees() -> Self {
        Self { skip_callees: true, ..Self::default() }
    }

    /// A walk of one body that stops at its lambda callees
    /// ([`Refs::take_callees`]).
    pub(crate) fn stopping_at_callees() -> Self {
        Self { callees: Some(Vec::new()), ..Self::default() }
    }

    pub(crate) fn take_callees(&mut self) -> Vec<LambdaInstanceId> {
        self.callees.take().unwrap_or_default()
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
    /// The binding's lambda definition; labeled defaults compile in its
    /// env and scope.
    pub lambda_id: Option<LambdaId>,
}

impl graphix_types::tracked::SameValue for BuiltinBindInfo {
    fn same_value(&self, other: &Self) -> bool {
        self.name == other.name
            && triomphe::Arc::ptr_eq(&self.argspec, &other.argspec)
            && triomphe::Arc::ptr_eq(&self.typ, &other.typ)
            && self.lambda_id == other.lambda_id
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
// CR claude for eric: [perf/high] fast_dispatch calls the fn even when every argument
// is stale, so a fused fast call reruns at each kernel invocation, whichever input
// fired. The node-walk's CachedArgs evaluates it only when one of its own arguments
// fires. The stdlib registers fast fns whose cost scales with their data (str::parse
// runs the Graphix parser, hbs::render, the json/toml/pack writers, sort/dedup, re::*),
// against the 'keep it cheap' contract above. A lambda that parses a constant
// 20000-element literal and takes a ticking i64 runs 23x slower fused (17 s vs 0.7 s).
// Decide between standing a stale call's last production (per-site memory, which strict
// fusion forbids), taking the FastCall off the builtins whose cost scales with their
// data, or documenting the cliff in the #[native] chapter. probe:
// design/review-2026-10-26/repro/x-builtin-effects-fastcall-recompute.gx
// (x-builtin-effects-02)
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
}

impl DefAssertionKind {
    const ALL: [Self; 4] = [Self::Sync, Self::Async, Self::TailRecursive, Self::Pure];

    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::Sync => "sync",
            Self::Async => "async",
            Self::TailRecursive => "tail_recursive",
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
    /// A `#[pure]` that holds only diagnostic output has warned of it.
    pub(crate) warned: bool,
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
    resumes: bool,
}

macro_rules! env_restore_methods {
    () => {
        /// Run `f` with the lexical environment restored to `env`, then put
        /// the current one back. Bindings `f` creates are retained.
        pub fn with_restored<T, F: FnOnce(&mut Self) -> T>(
            &mut self,
            mut env: Env,
            f: F,
        ) -> T {
            self.with_restored_mut(&mut env, f)
        }

        /// [`Self::with_restored`] leaving the lexical environment `f`
        /// built in `env`, so two envs keep continuity across invocations.
        pub fn with_restored_mut<T, F: FnOnce(&mut Self) -> T>(
            &mut self,
            env: &mut Env,
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
    /// Interrupt/abort control, shared with the runtime handle, and the
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
    /// holds a value. `Bind::delete` removes its ids.
    pub connect_targets: TrackedSet<BindId>,
    /// Every binding a call site resolved statically, with the first such
    /// call: that call keeps the definition it bound, so a later compile's
    /// `<-` to the binding is refused ([`Self::mark_connect_target`]).
    /// `Bind::delete` removes its ids.
    pub static_callees: TrackedMap<BindId, StaticCall>,
    /// Per block being compiled, innermost last: the lets something in
    /// it writes, by statement and name ([`node::written_lets`]).
    pub(crate) written_lets: Vec<AHashSet<(ExprId, CompactString)>>,
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
            static_callees: self.static_callees.fork(),
            written_lets: self.written_lets.clone(),
            block_typedefs: self.block_typedefs.clone(),
            builtin_bindings: self.builtin_bindings.fork(),
            rec_defs: self.rec_defs.clone(),
            def_gate_params: self.def_gate_params.clone(),
            instance_fn_params: self.instance_fn_params.clone(),
            fn_param_args: self.fn_param_args.clone(),
            def_gate_depth: self.def_gate_depth,
            resolving_lambdas: Mutex::new(self.resolving_lambdas.lock().clone()),
            defaults_in_check: self.defaults_in_check.clone(),
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
            static_callees,
            written_lets: _,
            block_typedefs: _,
            builtin_bindings,
            rec_defs: _,
            def_gate_params: _,
            instance_fn_params: _,
            fn_param_args: _,
            def_gate_depth: _,
            resolving_lambdas: _,
            defaults_in_check: _,
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
        self.static_callees.join(static_callees);
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
        *self.pending_refs.entry((id, reader)).or_default() += 1;
    }

    /// The reader a node at `top_id` inside the current boundary is.
    pub fn reader(&self, top_id: ExprId) -> prune::Reader {
        prune::Reader { root: top_id, boundary: self.boundary.clone() }
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
    pub(crate) fn mark_connect_target(&mut self, id: BindId) -> Result<()> {
        if let Some(StaticCall(call)) = self.static_callees.get(&id) {
            let name =
                self.env.by_id.get(&id).map(|b| b.name.clone()).unwrap_or_default();
            bail!(
                "{name} cannot be written: an earlier compile calls it as a fixed \
                 function at {}: {call}",
                call.pos
            )
        }
        self.batch_connect_targets.insert(id);
        self.connect_targets.insert(id);
        if !self.generalized_earlier(id) {
            self.env.poly_binds.remove(&id);
        }
        Ok(())
    }

    /// Record that the call `spec` binds `id`'s definition statically.
    // CR claude for claude: [bug/medium] A record here is never removed when the call
    // that made it is deleted; Bind::delete removes only the callee's entry. After a
    // dynamic module reloads a body that called `super::f(1)` statically with one that
    // does not, a REPL `f <- |x: i64| x + 100` is still refused: "an earlier compile
    // calls it as a fixed function ... super::f(1)". The same happens for any deleted
    // calling site, and the stale entry is also written into the image. Fix: key the
    // record by the recording sites (BindId -> site ExprIds), drop a site's entry when
    // its CallSite or alias Bind is deleted, and refuse only while a live site remains.
    // Pin it beside written_after_a_static_call_is_refused. probe:
    // design/review-2026-10-26/repro/tests-modules-image-stale-static-callee.sh
    // (tests-modules-image-01)
    // 2026-10-26 claude: Not fixed (L3-b05). Still reproduces. The suggested per-site
    // key is not enough: a definition's check builds its body's CallSites, which record
    // here, and then discards them (lambda.rs: ctx.discard_apply(f) after the def
    // check), so dropping a site's record at its delete would forget every static call
    // inside a definition, and 'let g = |x| f(x); f <- ..' would be accepted while g's
    // later instances are typed from a check that bound f statically. A record must
    // belong to what keeps the static binding alive: a live CallSite or alias Bind, and
    // a live definition whose checked body holds the call (released when the definition
    // is forgotten). Per-site counts also need TrackedMap::join to merge two compile
    // tasks' writes to one key (a three-way merge; today the fork's value wins) and the
    // recording nodes to carry what they recorded through the image (a lazily decoded
    // instance's sites are counted in the image but never decoded). Needs that design
    // before code.
    pub(crate) fn record_static_callee(&mut self, id: BindId, spec: &Arc<expr::Expr>) {
        if !self.static_callees.contains_key(&id) {
            self.static_callees.insert(id, StaticCall(spec.clone()));
        }
    }

    /// Whether `id` is a binding an earlier compile generalized: its
    /// check already made variables of a function it holds generic.
    pub(crate) fn generalized_earlier(&self, id: BindId) -> bool {
        self.env.by_id.get(&id).is_some_and(|b| {
            let mut cells: LPooled<AHashMap<usize, typ::TVar>> = LPooled::take();
            b.typ.reached_cells(&mut cells);
            let mut generic = cells
                .iter()
                .filter(|(_, tv)| {
                    tv.level() == typ::tvar::Level::GENERIC && !tv.is_quantifier_rigid()
                })
                .peekable();
            if generic.peek().is_none() {
                return false;
            }
            // a function type's own quantifiers are generic whatever compiled it
            let quantified = node::callsite::bound_quantifiers(&b.typ);
            generic.any(|(addr, _)| !quantified.contains(addr))
        })
    }

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
        let mut m = Memo { open: &self.rec_defs, done: LPooled::take(), fresh: false };
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
                static_callees: TrackedMap::default(),
                written_lets: Vec::new(),
                block_typedefs: Vec::new(),
                builtin_bindings: TrackedMap::default(),
                rec_defs: nohash::IntSet::default(),
                def_gate_params: nohash::IntMap::default(),
                instance_fn_params: AHashMap::default(),
                fn_param_args: nohash::IntMap::default(),
                def_gate_depth: 0,
                resolving_lambdas: Mutex::new(Resolving::default()),
                defaults_in_check: SmallVec::new(),
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
    pub fn ref_target(&self, cell: BindId) -> (BindId, &[node::place::Step]) {
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
                    Step::Key(k) => k.clone(),
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

    /// Let go of what the references released since the last cycle kept:
    /// a cell's place, chain link and value, and a binding deleted while a
    /// reference named it.
    pub fn release_references(&mut self) {
        let lives = self.control.lives().clone();
        lives.released(|id| {
            self.cx.env.byref_chain.remove(&id);
            self.cx.env.by_id.remove(&id);
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

    /// Deliver `tv` to the binding `id` this cycle: into the event, and
    /// into the store, where a bottom stands as a stale one.
    // CR claude for claude: [risk/medium] Six other sites deliver to a binding by hand:
    // Bind::update's publish, ByRef::publish, module::store_production with its
    // inserts, PatternNode::bind_event, the seq machine's pc entry, and GX::deliver.
    // They store a bottom three ways: STALE_BOTTOM here, FRESH_BOTTOM in
    // Bind/ByRef/module, the raw tag in bind_event. A quiet production is stored with
    // its tag here and as fired, with a fresh stamp, in Bind. node::read_var hands a
    // this-cycle store entry to readers as Delivered with that tag, and the wake
    // catch-up reads any later stamp as a write, so the conventions are observable.
    // Route every site through this function with one stored form and a notify flag; a
    // quiet republication takes store_insert_standing. Found by reading; not run.
    // (x-dup-02)
    // 2026-10-26 claude: Bind's quiet publication now takes store_insert_standing,
    // which keeps the replaced entry's stamp (c-prune-wake-02); the other hand-rolled
    // sites are unchanged.
    // 2026-10-26 claude: Not done (L3-b05): unifying the stored form is observable and
    // wants its own batch and a soak. node::read_var hands a this-cycle store entry to
    // a reader with no event entry as Delivered with its stored tag, and a standing one
    // through standing_view, which reads fresh under View::Birth. So moving
    // PatternNode::bind_event's stale leaves (stored stale, stamped now) to
    // store_insert_standing (old stamp) makes a Birth-view reader of a stale-present
    // scrutinee's bind see it fresh; storing them fired would fire the arm. Likewise
    // deliver's STALE_BOTTOM vs the others' FRESH_BOTTOM differs for a same-cycle store
    // read. deliver also never calls notify_set, which Bind, ByRef and the seq pc do. A
    // unification must pick, per caller, the event tag, the stored tag, stamped vs
    // standing and notify, and pin each choice.
    pub fn deliver(&mut self, id: BindId, tv: TagValue) {
        let standing = if tv.tag().is_bottom() {
            TagValue::tagged(Value::Null, Tag::STALE_BOTTOM)
        } else {
            tv.clone()
        };
        self.rt.store_insert(id, standing);
        self.event.variables.insert(id, tv);
    }

    /// Drop a reference `top_id` holds to `id`: one not replayed yet is
    /// cancelled, any other unregistered.
    pub fn unref_var(&mut self, id: BindId, top_id: ExprId) {
        let key = (id, self.reader(top_id));
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
        prune::Reader { root: top_id, boundary: self.boundary.clone() }
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

    /// True if an `interrupt()` or `abort()` is pending; loops poll this
    /// at their head.
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
            && self.written_lets.is_empty()
            && self.block_typedefs.is_empty()
            && self.resolving_lambdas.lock().0.is_none()
            && self.defaults_in_check.is_empty()
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
        self.written_lets.clear();
        self.block_typedefs.clear();
        *self.resolving_lambdas.lock() = Resolving::default();
        self.defaults_in_check.clear();
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
                PendingSettle::Discernible { typ, what, spec } => {
                    (discernible(env, typ, what), spec)
                }
                PendingSettle::Told { test, rest, spec } => (told(env, test, rest), spec),
                PendingSettle::Site { ftype, spec, .. } => {
                    (site_bounds(env, ftype), spec)
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

/// The bounds a call's settled cells must meet beyond their check: a
/// `Discernible` cell's binding, and the map keys a `Concrete` one's (a
/// map read from data).
fn site_bounds(env: &Env, ftype: &FnType) -> Result<()> {
    let mut tvs: LPooled<AHashMap<ArcStr, typ::TVar>> = LPooled::take();
    ftype.collect_tvars(&mut tvs);
    for tv in tvs.values() {
        let Some(t) = tv.binding() else { continue };
        for c in tv.cell_constraints().iter() {
            match c {
                Type::Discernible | Type::Ordered => {
                    discernible(env, &t, &Discerned::Bound(tv.clone(), c.clone()))?
                }
                Type::Concrete => discernible(env, &t, &Discerned::Keys)?,
                Type::OneNumber => {
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
    let Some(why) = test.rep_collision_open(env, rest) else { return Ok(()) };
    let open = |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
    // a cell the arm's own predicate holds (a slice pattern's element) is
    // the arm's to narrow, not a type it must tell apart
    fn cells(t: &Type, own: &mut AHashSet<usize>) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if own.insert(tv.cell_addr())
                    && let Some(b) = tv.binding()
                {
                    cells(&b, own)
                }
            }
            t => t.for_each_child(&mut |c| cells(c, own)),
        })
    }
    let mut own: LPooled<AHashSet<usize>> = LPooled::take();
    // CR claude for eric: [bug/high] This exemption waives every cell the test merely
    // mentions, not only a slice pattern's element cells. So `|y: 'b, x: ['b,
    // `Keep('b)]| select x { `Keep(r) => r, r => r }` passes, although an instance may
    // bind 'b to a `Keep variant. `g(`Keep(6), `Keep(5))` then returns 5 typed
    // `Keep(i64), and a later select over it matches no arm. With `Keep(i64) as the
    // test, the same select is refused ("may share `Keep(i64)'s runtime form"). Fix:
    // exempt only cells the pattern itself introduces, never a cell that `rest` also
    // holds, such as the definition's quantifier. The core Map Collection impl's
    // filter_map select is the same shape and will need the keep-marker fix. probe:
    // design/review-2026-10-26/repro/x-engine-collections-own-cell-told.gx
    // (x-engine-collections-02)
    cells(test, &mut own);
    let owned = |t: &Type| matches!(t, Type::TVar(tv) if own.contains(&tv.cell_addr()));
    if let Indiscernible::Pair(a, b) = &why
        && ((open(a) && owned(a)) || (open(b) && owned(b)))
    {
        return Ok(());
    }
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
            typ.indiscernible(env, Open::Unknown, *bound == Type::Ordered)
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
            (Discerned::Compared { .. }, Indiscernible::Abstract(a)) => bail!(
                "can't compare values of {typ}: {}",
                Indiscernible::by_representation(&a)
            ),
            (_, Indiscernible::Abstract(a)) => bail!(
                "a map key type can't hold {}",
                Indiscernible::by_representation(&a)
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

    /// Append a generated block-scope component (do/fn/sel/ca level).
    pub fn append_block(&self, kind: &str, id: u64) -> Self {
        self.append(block_component(kind, id).as_str())
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
    if fusion_on(flags) && ctx.fusion.available() {
        let st = Instant::now();
        let p = profile::phase(Phase::Fusion);
        let fused = fusion::TypeMemo::scope(|| fusion::fuse(node, ctx));
        ctx.fusion.link();
        drop(p);
        fused?;
        info!("fusion time {:?}", st.elapsed());
        analysis::check_tail_loops(tail_loops, ctx)?;
    }
    Ok(())
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
            })
        });
    }
}

/// A call that binds a definition statically ([`CompileCtx::static_callees`]):
/// any one names the binding's refusal, so compile tasks recording different
/// calls of one binding agree.
#[derive(Debug, Clone)]
pub struct StaticCall(pub Arc<expr::Expr>);

impl graphix_types::tracked::SameValue for StaticCall {
    fn same_value(&self, _: &Self) -> bool {
        true
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
    static_callees: TrackedMap<BindId, StaticCall>,
    tags: TrackedSet<ArcStr>,
}

impl Saved {
    pub fn take<R: Rt>(ctx: &CompileCtx<R>) -> Self {
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
            static_callees,
            written_lets: _,
            block_typedefs: _,
            builtin_bindings,
            rec_defs: _,
            def_gate_params: _,
            instance_fn_params: _,
            fn_param_args: _,
            def_gate_depth: _,
            resolving_lambdas: _,
            defaults_in_check: _,
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
            static_callees: static_callees.clone(),
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
            static_callees,
            tags,
        } = self;
        ctx.env = env;
        ctx.lambda_defs = lambda_defs;
        ctx.bind_to_lambda = bind_to_lambda;
        ctx.builtin_bindings = builtin_bindings;
        ctx.fn_forward_resolutions = fn_forward_resolutions;
        ctx.connect_targets = connect_targets;
        ctx.batch_connect_targets = batch_connect_targets;
        ctx.static_callees = static_callees;
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
    let _level = typ::tvar::AtLevel::enter(typ::tvar::Level::TOP);
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
            ctx.drop_deferred();
            saved.restore(ctx);
            return Err(e);
        }
    };
    info!("compile time {:?}", st.elapsed());
    if let Err(err) = check_pending_imports(ctx) {
        return Err(abandon_stmt(ctx, node, saved, err));
    }
    if let Err(e) = check_and_fuse_inner(ctx, flags, &mut node, verify) {
        return Err(abandon_stmt(ctx, node, saved, e));
    }
    ctx.apply_deferred();
    // An attribute the fusion walk neither dispatched nor absorbed
    // would silently assert nothing; a check runs no fusion walk and
    // leaves the attributes to a build.
    if fusion_on(flags) && !flags.contains(CFlag::CheckOnly) {
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
                return Err(abandon_stmt(ctx, node, saved, e));
            }
        } else if !census.is_empty() {
            let dispatched = ctx.attr_dispatched.lock();
            let absorbed = ctx.attr_absorbed.lock();
            for spec in census.iter() {
                // CR claude for eric: [bug/medium] [bug/medium] An attribute inside a
                // function body is censused when the definition compiles, but only a
                // static instance fused in this same compile dispatches it. A function
                // that is never called, is called only dynamically, or is called in a
                // later REPL input is therefore refused here. The refusal tells the
                // user to put the decorated expression in its own statement, which it
                // already is. `--check` accepts the program, and the REPL can never
                // define such a function. Decide the rule. One option: treat a registry
                // attribute in a definition body like a DefAssertion, pending until an
                // instance's fusion dispatches it. The other: keep the refusal, but say
                // the enclosing function was not instantiated in this compile, and add
                // the message to analysis::BUILD_ONLY_REFUSALS. probe:
                // design/review-2026-10-26/repro/c-lib-native-uncalled-body.gx
                // (c-lib-01)
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
