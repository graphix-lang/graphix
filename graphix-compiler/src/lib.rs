#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
#![recursion_limit = "256"]
pub use graphix_types::{
    AbstractTypeRegistry, BindId, CFlag, LambdaId, LambdaInstanceId, LibState, PrintFlag,
    SourcePosition, abstract_value, block_component, dbg_flag, defetyp, env, err, errf,
    expr, format_with_flags, ide, is_block_component, is_do_block, is_fn_block, mod_root,
    shared_map, tracked, typ,
};
pub(crate) use graphix_types::{CAST_ERR, CAST_ERR_TAG, Restore, profile, stack};

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

pub use stack::set_stack_budget;
pub use stack::{Control, CtlFlag, ParMode, with_control};
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
    collections::hash_map::Entry,
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

pub trait UserEvent: Clone + Debug + Any + Send + Sync {
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

/// Global pool of channel watch batches.
pub static CBATCH_POOL: LazyLock<Pool<Vec<(BindId, Box<dyn CustomBuiltinType>)>>> =
    LazyLock::new(|| Pool::new(10000, 1000));

/// How an update reads what stands: in order, a stronger view includes
/// the weaker.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum View {
    /// An ordinary cycle: only what was delivered fired.
    #[default]
    Cycle,
    /// A birth: standing values read as delivered.
    Birth,
    /// A select arm's or a seq step's wake: a birth, except that a `<-`
    /// target that already holds a value keeps it.
    Wake,
}

impl View {
    /// A birth or a wake.
    pub fn init(self) -> bool {
        self != View::Cycle
    }

    pub fn wake(self) -> bool {
        self == View::Wake
    }
}

/// Everything that happened simultaneously in one execution cycle. At
/// most one update per variable per cycle; further updates are queued
/// for later cycles.
#[derive(Debug)]
pub struct Event<E: UserEvent> {
    pub view: View,
    /// The overlay: this cycle's transient deliveries. Not the value
    /// store: reads fall through to [`Rt::store_get`].
    pub variables: branch::Layered<TagValue>,
    /// The cycle's custom deliveries, each taken by its consumer; one
    /// map for every branch of the cycle.
    pub custom: Arc<Mutex<IntMap<BindId, Box<dyn CustomBuiltinType>>>>,
    pub user: E,
}

impl<E: UserEvent> Event<E> {
    pub fn new(user: E) -> Self {
        Event {
            view: View::Cycle,
            variables: branch::Layered::default(),
            custom: Arc::new(Mutex::new(IntMap::default())),
            user,
        }
    }

    /// Whether standing values read as delivered: a birth or a wake.
    pub fn init(&self) -> bool {
        self.view.init()
    }

    pub fn wake(&self) -> bool {
        self.view.wake()
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
    pub(crate) fn fork(&self) -> Self {
        Event {
            view: self.view,
            variables: self.variables.fork(),
            custom: self.custom.clone(),
            user: self.user.clone(),
        }
    }

    /// Apply what the forked branch's event `child` delivered.
    pub(crate) fn merge(&mut self, child: Self) {
        let Self { view: _, variables, custom: _, user: _ } = child;
        self.variables.merge(variables);
    }

    pub fn clear(&mut self) {
        let Self { view, variables, custom, user } = self;
        *view = View::Cycle;
        variables.clear();
        custom.lock().clear();
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

    #[inline]
    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        stack::ensure_sufficient(|| self.0.update(ctx))
    }

    #[inline]
    pub fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        stack::ensure_sufficient(|| self.0.delete(ctx))
    }

    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0(ctx))
    }

    pub fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut node::lambda::InstanceTypes,
    ) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0_instance(ctx, types))
    }

    pub fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck1(ctx))
    }

    pub fn refs(&self, refs: &mut Refs) {
        stack::ensure_sufficient(|| self.0.refs(refs))
    }

    #[inline]
    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
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
    Static { instance: &'a FnType },
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
        ApplyView::BuiltIn("")
    }

    /// Same borrowed-production contract as [`Update::update`]: the
    /// returned `&TagValue` is the builtin's resident result slot.
    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue;

    /// Delete any internally generated nodes.
    fn delete(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
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
    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>);

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
    /// A builtin, by its registered name (empty below the wrapper that
    /// knows it).
    BuiltIn(&'a str),
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
    SeqAbort(&'a node::error::SeqAbort<R, E>),
    SeqMachine(&'a node::seq_machine::SeqMachine<R, E>),
    SeqCapture(&'a node::seq_machine::SeqCapture<R, E>),
    Qop(&'a node::error::Qop<R, E>),
    OrNever(&'a node::error::OrNever<R, E>),
    ExplicitParens(&'a node::ExplicitParens<R, E>),
    ForkControl(&'a node::fork_control::ForkControl<R, E>),
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
// XCR claude for claude: [structure] Node has no child enumeration, unlike Expr's
// for_each_child/map_children. Every Update impl lists its children again by hand in
// refs, delete, sleep, fuse, typecheck0/1 and the image codecs (Not and Neg in
// node/op.rs, and Qop and OrNever in node/error.rs, are line-for-line copies), so a
// child added to refs but missed in sleep compiles without complaint. Two NodeView
// walkers list them a third time and disagree. fusion::for_each_node_inner visits
// select guards, a static module's nodes and impl prototype sites;
// node_shape::node_children skips all three, and it descends FusedKernel feeders that
// fusion treats as opaque. So a NodeShape::contains(..) pin cannot see a kernel inside
// a guard, a module with an interface or an impl prototype. One
// for_each_child/for_each_child_mut on Update would drive both walkers and could be the
// default body of the forwarding methods. (x-dup-03)
// 2026-10-08 claude: re-addressed, a scope call. Fixed: the walkers agree:
// node_shape::node_children rides fusion::for_each_child, the one child step, except that
// it opens a kernel's feeders. Left: a for_each_child_mut on Update as the default body
// of the forwarding methods (refs, delete, sleep, typecheck0/1). That touches all ~80
// node impls, and most do more than forward (sleep sets bits, delete unbinds), so the
// default would serve only the pure forwarders (Not, Neg, Qop, OrNever, ...). Worth a
// pass of its own, or close as accepted?
// 2026-10-09 claude: Eric ruled 10-09: Update::for_each_child/for_each_child_mut,
// required, are the one child enumeration; refs, delete, sleep, typecheck0,
// typecheck0_instance and typecheck1 default to walking them, and a node overrides only
// what does more (19 files, about 220 lines fewer). fusion::for_each_child and
// node_shape's walker delegate to it (a kernel stays opaque to fusion's). The five nodes
// that relied on typecheck0_instance defaulting to the check (FusedKernel, Ref, Module,
// Trait, Impl) say so explicitly. Pins: the whole gate (every node's refs, sleep, delete
// and checks now ride the walk).
pub trait Update<R: Rt, E: UserEvent>: Debug + Send + Sync + Any + 'static {
    /// Update the node with the event and return its production,
    /// borrowed from the node's own resident slot. Every awake node
    /// delivers every cycle; a quiet cycle rides the resident. See
    /// [`TagView`].
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue;

    /// Every child node, the one child enumeration: a callee's instance
    /// and a lambda's body are not children (each is per call site).
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>));

    /// [`Self::for_each_child`], mutably.
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>));

    /// Delete the node and its children from the context.
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.for_each_child_mut(&mut |c| c.delete(ctx))
    }

    /// First typecheck pass: structural checking. Each node checks
    /// itself and recurses into its children.
    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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
        ctx: &mut CompileCtx<R, E>,
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
    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.for_each_child_mut(&mut |c| c.sleep(ctx))
    }

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
        &mut ExecCtx<'_, R, E>,
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
    /// Whether the builtin shares state with other nodes whose order of
    /// access decides values (`queuefn`'s queue): two subtrees that both
    /// call an ordered builtin never run beside each other
    /// (`design/parallel_eval.md` §6). External effects are not ordered.
    const ORDERED: bool = false;

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
        ctx: &mut ExecCtx<'_, R, E>,
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
        // CR claude for eric: [bug] Any FusedKernel passes here, including one that
        // try_fuse_feeding_args built around arguments left on the node-walk. So
        // `#[native] f(throttle(i64:5))` compiles, while `#[native] throttle(i64:5)`
        // and `#[native] { let a = throttle(i64:5); f(a) }` are refused. Feeding only
        // takes an argument that fails builtin discovery, so the verdict depends on an
        // unrelated builtin: with `h` a loop that cannot fuse, `#[native] f(h(1000, 0,
        // &one))` is refused, but `#[native] f(h(once(1000), 0, &one))` passes while
        // h's recursion runs on the node-walk. CLAUDE.md says #[native] asserts zero
        // node-walk residue, but lang::fusion::call_fed_by_node_walked_args relies on
        // this pass, so this needs a ruling. One option: report the recorded blockers
        // of every feeder that is not a plain variable read, and take #[native] off
        // that pin's fed calls. The other: document that a call's arguments are inputs,
        // and feed every argument that does not fuse. probe:
        // design/review-2026-10-05/repro/f-mod-lowering-04.sh (f-mod-lowering-04)
        // 2026-10-07 claude: re-addressed: it asks for a ruling on whether #[native]
        // admits a call fed by node-walked arguments.
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

/// The log target of a running program's failures: an error nothing
/// handles, a hot operator's failure, a definition assertion a run-time
/// bind breaks. An embedder shows or routes them through its logger.
pub const FAILURE_TARGET: &str = "graphix::failure";

pub trait Rt: Debug + Any + Send + Sync {
    /// Called whenever a bound variable (or lambda) is referenced;
    /// `ref_by` is the toplevel expression containing the reference,
    /// which must be updated when the variable changes.
    fn ref_var(&mut self, id: BindId, ref_by: ExprId);
    fn unref_var(&mut self, id: BindId, ref_by: ExprId);

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
    fn patch_var(&mut self, id: BindId, path: node::place::Path, value: Value);
    /// Register the place a reference cell stands for (`&root[i].f`), so
    /// `*r` reads through the root and `*r <- v` patches it.
    fn set_ref_path(&mut self, cell: BindId, root: BindId, path: node::place::Path);
    fn ref_path(&self, cell: &BindId) -> Option<&(BindId, node::place::Path)>;
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

impl<R: Rt, E: UserEvent> CompileCtx<R, E> {
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
struct BuiltinEntry<R: Rt, E: UserEvent> {
    init: BuiltInInitFn<R, E>,
    decode: BuiltInDecodeFn<R, E>,
    effect: Effect,
    ordered: bool,
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
    pub(crate) resolving_lambdas: Mutex<Resolving>,
    /// Per-instance fn-formal BindId → the `LambdaId` forwarded to it:
    /// the persistent record the kernel cache fingerprint reads after
    /// the re-drive's `bind_to_lambda` entry is gone.
    pub fn_forward_resolutions: TrackedMap<BindId, LambdaId>,
    /// Each seq block's lowering, by its expression and lexical scope: a
    /// definition's body lowers once, so every compile of it has the same
    /// expression ids.
    // XCR claude for claude: [bug] Nothing removes an entry from lowered_seqs, and the
    // key's scope is minted fresh on many compiles: a try/with body scope is named by
    // ExprId::new() (node/seq_machine.rs:195), and a lambda literal's body scope by a
    // new LambdaId (node/lambda.rs:1326). So a seq inside a try/with body, or inside a
    // lambda literal in a function, lowers again at every instance under fresh
    // expression ids, and every lowering is kept for good. The DefTable has no rows for
    // those ids, so those nodes check again instead of substituting, which breaks the
    // comment above. Each re-parse also adds entries that keep that parse's whole
    // source text alive: an LSP check of a 512 KB script with one seq grows RSS by
    // about 0.5 MB per check (it levels off without the seq), and REPL lines and
    // dynamic-module reloads do the same. probe:
    // design/review-2026-10-05/repro/c-lib-01.gx (RSS grows ~34 MB per 350 instances;
    // it levels off when the inner seq is moved above the try). (c-lib-01)
    // 2026-10-07 claude: a failed compile now drops what it lowered (Saved, c-lib-07).
    // The minted scopes remain: desugar reads the lexical scope, so keying by
    // the expression alone needs the lowering to stop depending on it.
    // 2026-10-08 claude: a definition's body scope is named by its expression id
    // (node/lambda.rs, lambda_init), which every compile of a literal shares, so a seq in
    // a literal inside a function lowers once: the probe's literal variant levels off at
    // ~90 MB (was +18 MB per 400 steps). A check (GXRt::check, the LSP) now rolls back
    // every registry Saved holds, lowered_seqs included. Left: a dynamic module's reload
    // keeps its old lowerings. No pin: RSS by probe only.
    // 2026-10-09 reviewer: the probe levels off now (debug build, HEAD 32f22367: 87.6 MB
    // at k 400, 91.6 MB at k 2000), and SeqMachine::compile names its nested scopes by
    // the machine's id. Back to CR: the note leaves a dynamic module's reload keeping its
    // old lowerings, which the CR names, and nothing pins the rest. A test that builds
    // many instances of a function holding a seq in a try/with body and in a lambda
    // literal and asserts lowered_seqs.len() stays put would.
    // 2026-10-09 claude: a dynamic module's reload now drops the lowerings under its
    // scope (Module::clear_compiled), and EnvStats reports lowered_seqs_len. Pins:
    // lang::modules::reloads_keep_no_old_lowerings (3 vs 31 without the drop) and
    // instances_share_their_lowerings (seqs in a try body and a lambda literal across 30
    // runtime instances; 5 vs 33 with a fresh body scope per instance). A REPL line's
    // lowering stays, as its bindings in the env do: it grows with what is typed, not
    // with what runs.
    // 2026-10-09 reviewer: the reload drop holds and its pin fails without it (3 vs 31,
    // retain disabled in a scratch worktree), and the lambda-literal half is pinned (5 vs
    // 33 with def_scope named by ExprId::new() in lambda_init). Back to CR: the try/with
    // half is not pinned. instances_share_their_lowerings has no seq INSIDE its try body
    // (one wraps the try, the other is in g), so naming SeqMachine::compile's nested
    // scopes by ExprId::new() again leaves both pins green, while
    // `|x: i64| seq { try { let b = seq { let c = [x][0]?; c + 1 }; b } with(e) { 0 } }`
    // over 20 instances then lowers 21 times (2 with the fix): put a seq in the pin's try
    // body. Also: the scope-prefix drop removes a lowering a definition still live after
    // the reload uses: an old `f` held by `once(foo::f)` and called from new array slots
    // after a reload lowers its seq again under fresh ids (seen with a SEQMISS print in
    // the compiler's Seq arm), so those nodes find no DefTable row and derive their own
    // types; values stay right, and the entry is kept until the next reload. Tying a
    // lowering's life to its definition (its DefTables) would close that. The REPL
    // judgement is fair for the shell; an embedder calling GXHandle::compile in a loop
    // grows the same way, and lowered_seqs_len can be `lowered_seqs.len()`.
    // 2026-10-09 claude: the try/with half is pinned now: instances_share_their_lowerings
    // puts a seq inside the try body (6 vs 34 with SeqMachine's nested scopes minted
    // fresh). A definition from a dynamic module held across its reload (once(foo::f))
    // lowers its seq once more under fresh ids when a new instance binds; its instances
    // then check instead of substituting, values unchanged, and that one entry lasts
    // until the next reload: bounded, performance only. Tying a lowering to its
    // definition would close it, at the cost of carrying the lowering in LambdaDef.
    // lowered_seqs_len uses len().
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
    pub(crate) pending_refs: AHashMap<(BindId, ExprId), usize>,
    /// What compiling abandoned, for the runtime to delete.
    discarded: Vec<Discarded<R, E>>,
    /// The id of the concurrent compile task this one runs in
    /// ([`typ::tvar::InTask`]); 0 at the root.
    pub(crate) task: u32,
    /// This task's share of the compile's profile ([`profile::Task`]).
    profile: profile::Task,
    /// The fusion subsystem's state; see [`fusion::FusionCtx`].
    pub fusion: fusion::FusionCtx,
}

enum Discarded<R: Rt, E: UserEvent> {
    Node(Node<R, E>),
    Apply(Box<dyn Apply<R, E>>),
    Stored(BindId),
}

/// The compile context, the runtime and the cycle's event: what an
/// embedder owns. Nodes see it through an [`ExecCtx`].
pub struct ExecState<R: Rt, E: UserEvent> {
    pub cx: CompileCtx<R, E>,
    /// The tables of the image this session was restored from, for
    /// anything decoded later.
    pub(crate) image_decoder: std::sync::OnceLock<image::SharedDecoder>,
    /// Library state for builtins.
    pub libstate: LibState,
    /// The runtime.
    pub rt: R,
    /// The call sites through which `Value` comparison and printing
    /// reach core-trait implementations, built on first use.
    pub(crate) core_hook_sites: Mutex<node::coretraits::CoreHookSites<R, E>>,
    /// Interrupt/abort control, shared with the runtime handle. See
    /// [`Control`].
    pub control: Arc<Control>,
    /// The cycle's event.
    pub event: Event<E>,
}

impl<R: Rt, E: UserEvent> std::ops::Deref for ExecState<R, E> {
    type Target = CompileCtx<R, E>;

    fn deref(&self) -> &CompileCtx<R, E> {
        &self.cx
    }
}

impl<R: Rt, E: UserEvent> std::ops::DerefMut for ExecState<R, E> {
    fn deref_mut(&mut self) -> &mut CompileCtx<R, E> {
        &mut self.cx
    }
}

/// What a node's update, delete and sleep read and write: a view of an
/// [`ExecState`].
pub struct ExecCtx<'a, R: Rt, E: UserEvent> {
    pub cx: branch::CxView<'a, R, E>,
    pub(crate) image_decoder: &'a std::sync::OnceLock<image::SharedDecoder>,
    pub libstate: &'a LibState,
    pub rt: branch::RtView<'a, R>,
    pub(crate) core_hook_sites: &'a Mutex<node::coretraits::CoreHookSites<R, E>>,
    pub control: &'a Arc<Control>,
    pub event: &'a mut Event<E>,
    /// How many forks this branch is below the cycle's root.
    pub(crate) fork_depth: u8,
    /// The runtime's parallel mode when the view was made.
    pub(crate) par: graphix_types::stack::ParMode,
    /// What the code around the node being updated says about forking.
    pub(crate) fork: branch::ForkFlags,
}

impl<'a, R: Rt, E: UserEvent> std::ops::Deref for ExecCtx<'a, R, E> {
    type Target = CompileCtx<R, E>;

    #[inline]
    fn deref(&self) -> &CompileCtx<R, E> {
        &self.cx
    }
}

impl<'a, R: Rt, E: UserEvent> std::ops::DerefMut for ExecCtx<'a, R, E> {
    #[inline]
    fn deref_mut(&mut self) -> &mut CompileCtx<R, E> {
        &mut self.cx
    }
}

impl<R: Rt, E: UserEvent> CompileCtx<R, E> {
    /// The seq lowerings the context keeps.
    pub fn lowered_seqs_len(&self) -> usize {
        self.lowered_seqs.len()
    }

    /// A context for a compile task: it shares the registry, starts from
    /// this program's state, recording what it writes, and inherits the
    /// resolution in progress; its scratch outputs start empty.
    pub(crate) fn fork(&self) -> Self {
        Self {
            registry: self.registry.clone(),
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
            def_assertions: Arc::new(Mutex::new(Vec::new())),
            attr_census: Mutex::new(Vec::new()),
            attr_dispatched: Mutex::new(IntSet::default()),
            attr_absorbed: Mutex::new(IntSet::default()),
            pending_refs: AHashMap::default(),
            discarded: Vec::new(),
            task: self.task,
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
        self.lowered_seqs.join(lowered_seqs);
        let frame = self.pending_settles.last_mut().expect("root settle frame");
        frame.extend(pending_settles.into_iter().flatten());
        self.pending_imports.extend(pending_imports);
        self.pending_names.extend(pending_names);
        if !Arc::ptr_eq(&self.def_assertions, &def_assertions) {
            let joined = mem::take(&mut *def_assertions.lock());
            self.def_assertions.lock().extend(joined);
        }
        self.attr_census.lock().extend(attr_census.into_inner());
        self.attr_dispatched.lock().extend(attr_dispatched.into_inner());
        self.attr_absorbed.lock().extend(attr_absorbed.into_inner());
        for (k, n) in pending_refs {
            *self.pending_refs.entry(k).or_default() += n;
        }
        self.discarded.extend(discarded);
        profile.join();
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
                    ordered: T::ORDERED,
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

    /// Whether a registered builtin is [`BuiltIn::ORDERED`].
    pub fn builtin_ordered(&self, name: &str) -> bool {
        self.registry.builtins.get(name).is_some_and(|b| b.ordered)
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

impl<R: Rt, E: UserEvent> ExecState<R, E> {
    /// Build a new execution state over the runtime `rt`, with `user`
    /// as the event's user part. A low-level interface for custom
    /// runtimes; most embedders want `graphix-rt`.
    pub fn new(rt: R, user: E) -> Result<Self> {
        let id = AbstractTypeRegistry::uuid::<LambdaDef<R, E>>("lambda");
        let mut this = Self {
            cx: CompileCtx {
                registry: Arc::new(Registry {
                    lambdawrap: Abstract::register(id)?,
                    builtins: AHashMap::default(),
                    attributes: AHashMap::default(),
                }),
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
                resolving_lambdas: Mutex::new(Resolving::default()),
                fn_forward_resolutions: TrackedMap::default(),
                lowered_seqs: TrackedMap::default(),
                pending_settles: vec![Vec::new()],
                pending_imports: Vec::new(),
                pending_names: Vec::new(),
                def_assertions: Arc::new(Mutex::new(Vec::new())),
                attr_census: Mutex::new(Vec::new()),
                attr_dispatched: Mutex::new(IntSet::default()),
                attr_absorbed: Mutex::new(IntSet::default()),
                pending_refs: AHashMap::default(),
                discarded: Vec::new(),
                task: 0,
                profile: profile::Task::default(),
                fusion: fusion::FusionCtx::new()?,
            },
            image_decoder: std::sync::OnceLock::new(),
            libstate: LibState::default(),
            rt,
            core_hook_sites: Mutex::new(node::coretraits::CoreHookSites::default()),
            control: Arc::new(Control::new()),
            event: Event::new(user),
        };
        this.register_attribute::<Native>()?;
        Ok(this)
    }

    /// The view nodes run under.
    pub fn view(&mut self) -> ExecCtx<'_, R, E> {
        let Self { cx, image_decoder, libstate, rt, core_hook_sites, control, event } =
            self;
        let rt = branch::RtView::Root(rt);
        let cx = branch::CxView::Root(cx);
        ExecCtx {
            cx,
            image_decoder,
            libstate,
            rt,
            core_hook_sites,
            control,
            event,
            fork_depth: 0,
            par: control.par_mode(),
            fork: branch::ForkFlags::default(),
        }
    }
}

impl<'a, R: Rt, E: UserEvent> ExecCtx<'a, R, E> {
    /// `f` under `view`, or under the stronger view already in force: a
    /// birth inside a wake stays a wake.
    #[inline]
    pub fn under<T>(&mut self, view: View, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.event.view;
        self.event.view = prev.max(view);
        let r = f(self);
        self.event.view = prev;
        r
    }

    /// `v`, a value of `t`, with each reference a comparison meets
    /// replaced by what it names, `[root, step..]` (a reference's value
    /// is its own cell): equal results mean equal values, references
    /// equal when they name one place.
    pub fn ref_targets(&self, t: &Type, v: &Value) -> Value {
        use node::place::Step;
        t.map_refs(&self.env, v, &mut |r| match r {
            Value::U64(cell) | Value::V64(cell) => {
                let (root, path) = node::bind::ref_target(self, BindId::from(*cell));
                let steps = path.iter().map(|s| match s {
                    Step::Index(i) => Value::I64(*i),
                    Step::Field(f) => Value::String(f.clone()),
                    Step::Key(k) => k.clone(),
                });
                let target = std::iter::once(Value::U64(root.inner())).chain(steps);
                Value::Array(ValArray::from_iter(target))
            }
            r => r.clone(),
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

    /// The same view, borrowed for a shorter time, over `event`.
    pub fn with_event<'b>(&'b mut self, event: &'b mut Event<E>) -> ExecCtx<'b, R, E> {
        ExecCtx {
            cx: self.cx.reborrow(),
            image_decoder: self.image_decoder,
            libstate: self.libstate,
            rt: self.rt.reborrow(),
            core_hook_sites: self.core_hook_sites,
            control: self.control,
            event,
            fork_depth: self.fork_depth,
            par: self.par,
            fork: self.fork,
        }
    }

    /// Release an id this node minted for its own deliveries: unregister
    /// it and forget the value the store holds for it, which nothing will
    /// read again.
    pub fn release_var(&mut self, id: BindId, top_id: ExprId) {
        self.unref_var(id, top_id);
        self.rt.store_remove(&id);
    }

    /// Drop a reference `top_id` holds to `id`: one not replayed yet is
    /// cancelled, any other unregistered.
    pub fn unref_var(&mut self, id: BindId, top_id: ExprId) {
        // read through the view first: a branch that compiled nothing
        // must not fork its compile view to find nothing pending
        if !self.cx.pending_refs.contains_key(&(id, top_id)) {
            return self.rt.unref_var(id, top_id);
        }
        let n = self.cx.pending_refs.get_mut(&(id, top_id)).expect("checked above");
        *n -= 1;
        if *n == 0 {
            self.cx.pending_refs.remove(&(id, top_id));
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

    /// Open the compile frame of a top-level node, a statement's or one
    /// built at runtime outside any statement, before [`check_and_fuse`]:
    /// the previous compile's scratch is dropped.
    pub fn open_compile_frame(&mut self, top_id: ExprId) {
        self.attr_census.lock().clear();
        self.attr_dispatched.lock().clear();
        self.attr_absorbed.lock().clear();
        self.pending_imports.clear();
        self.pending_names.clear();
        self.pending_settles.clear();
        self.pending_settles.push(Vec::new());
        self.fusion.top_id = Some(top_id);
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
                PendingSettle::Discernible { typ, what, spec } => {
                    (discernible(env, typ, what), spec)
                }
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
                _ => (),
            }
        }
    }
    Ok(())
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
            (Discerned::Compared { .. }, Indiscernible::Pair(a, b)) => bail!(
                "can't compare values of {typ}: {} and {} have the same runtime \
                 form; wrap them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            (Discerned::Compared { .. }, Indiscernible::Ref(r))
                if r.resolve_tvars() == typ =>
            {
                bail!(
                    "can't order references: they have no order (== and != compare them by \
                 what they point to)"
                )
            }
            (Discerned::Compared { .. }, Indiscernible::Ref(r)) => bail!(
                "can't order values of {typ}: it holds the reference {}, and references \
                 have no order (== and != compare them by what they point to)",
                r.resolve_tvars()
            ),
            (_, Indiscernible::Pair(a, b)) => bail!(
                "{} and {} can't share a map key type: they have the same runtime \
                 form; wrap them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            (_, Indiscernible::Ref(r)) => bail!(
                "a map key type can't hold the reference {}: references have no order",
                r.resolve_tvars()
            ),
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

    /// Changes at every `abort(..)`: a run that began under one stamp
    /// and sees another was aborted.
    pub(crate) fn abort_stamp(&self) -> u64 {
        self.0.aborted.load(Ordering::Relaxed)
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

    pub fn with_catch(&self, catch: (BindId, ExprId), machine: bool) -> Self {
        Self(Some(ErrorHandler(Arc::new(DynNode {
            catch,
            machine,
            aborted: AtomicU64::new(0),
            raised: AtomicU64::new(0),
            nested: AtomicU64::new(0),
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
pub fn compile<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
/// [`CFlag::CheckOnly`]. On `Err` the caller unwinds: it deletes the
/// node, whose references cancel what the compile deferred, then
/// [`ExecCtx::drop_deferred`], then restores its env.
pub fn check_and_fuse<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    flags: BitFlags<CFlag>,
    node: &mut Node<R, E>,
) -> Result<()> {
    check_and_fuse_inner(ctx, flags, node)?;
    ctx.apply_deferred();
    Ok(())
}

fn check_and_fuse_inner<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
    if fusion_on(flags) && ctx.fusion.available() {
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

/// Whether a compile under `flags` fuses.
fn fusion_on(flags: BitFlags<CFlag>) -> bool {
    // A malformed input only de-fuses. The JIT helpers' wire ABI is
    // System V (a 16-byte `TagValue` is two
    // registers); Win64 passes it by hidden pointer, so on Windows the
    // graph is interpreted.
    // CR claude for eric: [risk] `not(windows)` turns fusion on for every other
    // cranelift host. The helper seam passes and returns the 16-byte TagValue by value,
    // which matches the two-I64 CLIF signatures only where the C ABI uses two registers
    // for it: SysV x86_64 and AAPCS64 (design/helper_abi_portability.md). On s390x
    // Linux, which passes such a struct by reference and returns it through a hidden
    // pointer, the first helper call that takes or returns a TagValue reads a word as a
    // pointer. Until the out-pointer rule lands, gate on the hosts known to match:
    // `all(any(target_arch = "x86_64", target_arch = "aarch64"), not(windows))`. The
    // same doc's last paragraph says a registration image that fails to decode is
    // fatal, but graphix-rt/src/gx.rs:300 warns and compiles cold. (f-helpers-08)
    !flags.contains(CFlag::FusionDisabled) && cfg!(not(windows))
}

/// Drain the deferred terminal settles ([`CompileCtx::pending_settles`])
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

pub(crate) fn check_pending_names<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
) -> Result<()> {
    use expr::At;
    for p in mem::take(&mut ctx.pending_names) {
        match p {
            Pending::Name(tr, spec) => {
                if !tr.names_something(&ctx.env) {
                    return Err(typ::UnresolvableRef::error(&tr, &ctx.env).at(&spec));
                }
            }
            Pending::Contractive(scope, name, spec) => {
                ctx.env.check_contractive(&scope, &name).at(&spec)?
            }
        }
    }
    Ok(())
}

/// Record the names `t`, written at `spec`, holds that do not resolve
/// yet ([`CompileCtx::pending_names`]).
pub(crate) fn defer_unresolved_names<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    t: &Type,
    spec: &Expr,
) {
    let mut names = Vec::new();
    t.unresolved_names(&ctx.env, &mut names);
    ctx.pending_names.extend(names.into_iter().map(|tr| Pending::Name(tr, spec.clone())));
}

/// A `use` whose name did not exist at its compile position must name
/// something by the end of the compile that deferred it.
pub(crate) fn check_pending_imports<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
) -> Result<()> {
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
pub fn compile_stmt<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
    ctx: &mut ExecCtx<'_, R, E>,
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
    lowered_seqs: TrackedMap<(ExprId, ModPath), Expr>,
}

impl Saved {
    pub fn take<R: Rt, E: UserEvent>(ctx: &CompileCtx<R, E>) -> Self {
        // every field is named, so a new one is decided here: program
        // state a failed compile rolls back, or scratch
        let CompileCtx {
            registry: _,
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
            pending_settles: _,
            pending_imports: _,
            pending_names: _,
            def_assertions: _,
            attr_census: _,
            attr_dispatched: _,
            attr_absorbed: _,
            pending_refs: _,
            discarded: _,
            task: _,
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
            lowered_seqs: lowered_seqs.clone(),
        }
    }

    pub fn restore<R: Rt, E: UserEvent>(self, ctx: &mut CompileCtx<R, E>) {
        let Saved {
            env,
            lambda_defs,
            bind_to_lambda,
            builtin_bindings,
            fn_forward_resolutions,
            connect_targets,
            batch_connect_targets,
            tags,
            lowered_seqs,
        } = self;
        ctx.env = env;
        ctx.lambda_defs = lambda_defs;
        ctx.bind_to_lambda = bind_to_lambda;
        ctx.builtin_bindings = builtin_bindings;
        ctx.fn_forward_resolutions = fn_forward_resolutions;
        ctx.connect_targets = connect_targets;
        ctx.batch_connect_targets = batch_connect_targets;
        ctx.tags = tags;
        ctx.lowered_seqs = lowered_seqs;
    }
}

/// Build the top-level node `spec` with `build`, then check and fuse it,
/// unwinding what it registered on failure.
fn compile_top<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    build: impl FnOnce(&mut ExecCtx<'_, R, E>, &Expr, ExprId) -> Result<(Node<R, E>, Scope)>,
) -> Result<(Node<R, E>, Scope)> {
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
    if let Err(e) = check_and_fuse(ctx, flags, &mut node) {
        return Err(abandon_stmt(ctx, node, saved, e));
    }
    // An attribute the fusion walk neither dispatched nor absorbed
    // would silently assert nothing; a check runs no fusion walk and
    // leaves the attributes to a build.
    if fusion_on(flags) && !flags.contains(CFlag::CheckOnly) {
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
    ctx: &mut ExecCtx<'_, R, E>,
    mut node: Node<R, E>,
    saved: Saved,
    e: anyhow::Error,
) -> anyhow::Error {
    node.delete(ctx);
    ctx.drop_deferred();
    saved.restore(ctx);
    e
}
