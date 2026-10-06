#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
#![recursion_limit = "256"]
pub use graphix_types::{
    AbstractTypeRegistry, BindId, CFlag, LambdaId, LambdaInstanceId, LibState, PrintFlag,
    SourcePosition, abstract_value, block_component, defetyp, env, err, errf, expr,
    format_with_flags, ide, is_block_component, is_do_block, mod_root, shared_map,
    tracked, typ,
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
pub use stack::{Control, CtlFlag, InterruptScope, ParMode};
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
    typ::{FnType, Type},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
pub use enumflags2::BitFlags;
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
    any::Any,
    cell::Cell,
    collections::hash_map::Entry,
    fmt::Debug,
    mem,
    sync::{
        self, LazyLock,
        atomic::{AtomicU64, Ordering},
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

/// Everything that happened simultaneously in one execution cycle. At
/// most one update per variable per cycle; further updates are queued
/// for later cycles.
#[derive(Debug)]
pub struct Event<E: UserEvent> {
    // CR claude for eric: [structure] `init` and `wake_init` are two bools for three
    // views (ordinary cycle, birth, wake); `wake_init` without `init` means nothing and
    // is never built. Every view change saves and restores them by hand, eight times
    // (select.rs:1010, seq_machine.rs:299, callsite.rs:1242 and 1667, collection.rs:975
    // with 1073, collection.rs:1477, error.rs:344, module.rs:992), and a birth inside a
    // wake keeps the wake view only because those sites leave `wake_init` alone. One
    // `enum View { Cycle, Birth, Wake }` with a scoped setter makes the meaningless
    // pair unrepresentable and states the birth-inside-wake rule once.
    // (x-invalid-states-07)
    pub init: bool,
    /// Set alongside `init` when the forced init view is a select arm's
    /// wake rather than a birth: a `<-` target that already holds a
    /// value keeps it instead of being reseeded.
    pub wake_init: bool,
    /// The binds a wake republished fired only because the woken arm's
    /// constants fired, not an input.
    pub wake_phantoms: branch::Layered<()>,
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
            init: false,
            wake_init: false,
            wake_phantoms: branch::Layered::default(),
            variables: branch::Layered::default(),
            custom: Arc::new(Mutex::new(IntMap::default())),
            user,
        }
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
            init: self.init,
            wake_init: self.wake_init,
            wake_phantoms: self.wake_phantoms.fork(),
            variables: self.variables.fork(),
            custom: self.custom.clone(),
            user: self.user.clone(),
        }
    }

    /// Apply what the forked branch's event `child` delivered.
    pub(crate) fn merge(&mut self, child: Self) {
        let Self { init: _, wake_init: _, wake_phantoms, variables, custom: _, user: _ } =
            child;
        self.wake_phantoms.merge(wake_phantoms);
        self.variables.merge(variables);
    }

    pub fn clear(&mut self) {
        let Self { init, wake_init, wake_phantoms, variables, custom, user } = self;
        *init = false;
        *wake_init = false;
        wake_phantoms.clear();
        // CR claude for eric: [perf] The overlay keeps the capacity of the cycle that
        // delivered the most. A hashbrown clear of a non-empty table memsets every
        // control byte and scans every bucket, so after one big fire every later cycle
        // that delivers anything pays O(peak). Probe, --no-fusion, beside a 100us timer
        // counter: `let size = select n { 0 => 100000, _ => 0 }; let xs =
        // array::init(size, |i| i)`. Once the slots are gone, each cycle costs 0.22 ms
        // of CPU against 0.16 ms with size 0 (0.19 ms after 25000 slots, 0.24 ms after
        // 200000). Layered::clear (branch.rs:442) could shrink a map whose capacity far
        // exceeds what the cycle used. (x-alloc-13)
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

    // CR claude for eric: [bug] Node has no inherent typecheck0_instance, so every
    // child.typecheck0_instance call (op.rs:283, callsite.rs:2169, node/mod.rs:319, and
    // lambda.rs:782 for the body) reaches the trait through DerefMut with no
    // stack::ensure_sufficient. Checking an instance body is therefore a recursion as
    // deep as the body, and the doc comment on Node above no longer holds. The parser
    // does not bound that depth: every paren level may hold a 1000-operator chain, and
    // --check accepts 300 such levels. A function whose body is 30 nested parenthesized
    // 1000-term `+` chains passes --check, then aborts with a stack overflow when
    // called or under --expand, while GRAPHIX_NO_SUBST=1 runs it. Add the guarded
    // shadow here beside typecheck0, plus a deep_nesting case that builds instances
    // (those cases run Mode::Check, which never elaborates). probe:
    // design/review-2026-10-05/repro/x-stack-03.sh (x-stack-03)
    pub fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        stack::ensure_sufficient(|| self.0.typecheck0(ctx))
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
    SeqAbort(&'a node::error::SeqAbortEvent<R, E>),
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
// CR claude for eric: [structure] Node has no child enumeration, unlike Expr's
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
pub trait Update<R: Rt, E: UserEvent>: Debug + Send + Sync + Any + 'static {
    /// Update the node with the event and return its production,
    /// borrowed from the node's own resident slot. Every awake node
    /// delivers every cycle; a quiet cycle rides the resident. See
    /// [`TagView`].
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue;

    /// Delete the node and its children from the context.
    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>);

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
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>);

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

pub trait Rt: Debug + Any + Send + Sync {
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
    // CR claude for eric: [bug] Nothing removes an entry from lowered_seqs, and the
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
            // CR claude for eric: [perf] Every statically resolved call site forks the
            // context (callsite.rs:1287), and each fork copies resolving_lambdas whole:
            // an IntMap holding an FnType by value for every instantiation still
            // resolving. The forks nest along a static call chain and each lives until
            // its child returns, so elaborating a chain of depth N holds O(N^2) copies;
            // TrackedMap::join (graphix-types/src/tracked.rs:150) also re-appends the
            // whole subtree's touched keys at every level. For `f_i = |a| f_{i-1}(a) +
            // 1`, peak RSS (debug, --no-fusion) is 169 MB at N=500, 478 MB at 1000 and
            // 967 MB at 1500, against 73 MB for a flat program of 1000 lambdas and 75
            // MB under --check; N=4000 is killed at a 6 GB cap. A resolution stack
            // shared by forks (a persistent list, each task pushing its own entry)
            // makes a fork O(1). probe: design/review-2026-10-05/repro/f-jit-04.sh
            // (f-jit-04)
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
        // CR claude for eric: [bug] At run time this set only grows. Every instance
        // built after the batch (a collection slot, an activation, a seq machine's pc
        // and result) inserts its fresh `<-` target ids here. Bind::delete prunes
        // connect_targets and bind_to_lambda but not this set, and only the embedder's
        // next compile or load clears it, so a script keeps one entry for every `<-` of
        // every instance it ever built. Churning 60 instances, each with 20
        // never-firing `<-`, every 10 ms grows a script from ~100 to ~310 MB in 60 s;
        // the same `<-` aimed at one outer variable stays flat, and the first REPL line
        // after the churn takes 0.41 s to walk the set. Remove the ids here in
        // Bind::delete too, or drop this set and guard static resolution with
        // connect_targets. probe: design/review-2026-10-05/repro/c-lib-02.py (c-lib-02)
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
                def_assertions: Arc::new(Mutex::new(Vec::new())),
                attr_census: Mutex::new(Vec::new()),
                attr_dispatched: Mutex::new(IntSet::default()),
                attr_absorbed: Mutex::new(IntSet::default()),
                pending_refs: AHashMap::default(),
                discarded: Vec::new(),
                task: 0,
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

    /// Drop a reference `top_id` holds to `id`: one not replayed yet is
    /// cancelled, any other unregistered.
    pub fn unref_var(&mut self, id: BindId, top_id: ExprId) {
        // CR claude for eric: [perf] In a forked branch whose compile view has not
        // forked yet, `get_mut` here goes through CxView::deref_mut. That boxes a whole
        // CompileCtx::fork() (env, tracked maps, fusion), which the merge then joins
        // back, only to find pending_refs empty: fork_each and fork_join assert that
        // nothing is pending at a fork. ctx.unref_var runs on the update and sleep
        // paths: the sleep of the array/list/map iter builtins, throttle's update, and
        // Deref::release on a moving reference. Each such branch therefore pays an
        // allocation and a fork/join per cycle where the serial walk pays one hash
        // lookup, against parallel_eval.md's rule that a branch that compiles nothing
        // pays nothing. Look the pair up through Deref first and call self.rt.unref_var
        // directly when it is absent. (c-lib-08)
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
/// [`CFlag::CheckOnly`]. The caller restores its env on `Err`.
pub fn check_and_fuse<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
    // CR claude for eric: [risk] check_and_fuse takes `flags` but decides fusion from
    // ctx.fusion.enabled, which only compile_top sets from its own flags.
    // compile_callable (begin_runtime_node, then check_and_fuse) inherits whatever the
    // last compile_top left. After a warm start no compile_top has run, so the field is
    // still FusionCtx::new's `true`, and every GUI widget callback's compile runs the
    // fusion pass under --no-fusion and on Windows. Nothing fuses there today only
    // because the callable's call site has a constant function node and binds
    // dynamically. Decide from `!flags.contains(CFlag::FusionDisabled) &&
    // cfg!(not(windows))` here and in compile_top's attribute check, and delete
    // FusionCtx::enabled (only lib.rs reads it). Open the compile frame for compile_top
    // and begin_runtime_node through one function, since today they reset different
    // scratch. (c-lib-09)
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
    ctx: &mut ExecCtx<'_, R, E>,
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
    ctx: &mut ExecCtx<'_, R, E>,
) -> Result<()> {
    for p in mem::take(&mut ctx.pending_imports) {
        let Some(e) = ctx.env.names.get(&p.scope).and_then(|sn| sn.imports.get(&p.key))
        else {
            continue;
        };
        if !ctx.env.import_target_exists(e) {
            // CR claude for eric: [readability] This error and bind_sig's
            // (graphix-compiler/src/node/module.rs:177) use ParserContext only to carry
            // a position, and its Display prints "parse error at …". So `let x =
            // 1;\nuse array::nosuch;\n1` reports "parse error at line: 2, column: 1 …
            // use: no `nosuch` in `array` (checked again after the enclosing statement
            // finished compiling)", and a second `type T` in a .gxi reports "parse
            // error at line: 3 … T is already defined in scope m". A real parse error
            // already says "Parse error at …" in its own message, so ParserContext's
            // Display (graphix-types/src/expr/context.rs:124) can print just the
            // position. The parenthetical describes the compiler, not the user's
            // mistake: the message is "use: no `nosuch` in `array`". (x-errors-15)
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
/// before it, so a failure puts them back.
// CR claude for eric: [structure] Saved is the only list of the tracked registries that
// the compiler does not check. fork, join and ExecState::new are struct literals or
// destructures, but Saved is written by hand, and it has drifted: it omits
// lowered_seqs, which fork and join treat as program state. A failed compile (a REPL
// line, a dynamic module's source) therefore keeps every seq it lowered, each entry
// pinning its expression and source text, and the doc above no longer says what Saved
// holds. Group the forked-and-joined registries in one struct with fork, join and
// Clone. Saved then becomes a clone of it and cannot drift, and fork, join and new each
// handle it in one line. (c-lib-07)
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
    pub(crate) fn take<R: Rt, E: UserEvent>(ctx: &ExecCtx<'_, R, E>) -> Self {
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

    pub(crate) fn restore<R: Rt, E: UserEvent>(self, ctx: &mut ExecCtx<'_, R, E>) {
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
    ctx: &mut ExecCtx<'_, R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    build: impl FnOnce(&mut ExecCtx<'_, R, E>, &Expr, ExprId) -> Result<(Node<R, E>, Scope)>,
) -> Result<(Node<R, E>, Scope)> {
    let _profile = profile::phase(Phase::Compile);
    let _level = typ::tvar::AtLevel::enter(typ::tvar::Level::TOP);
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
    ctx: &mut ExecCtx<'_, R, E>,
    mut node: Node<R, E>,
    saved: Saved,
    e: anyhow::Error,
) -> anyhow::Error {
    // CR claude for eric: [risk] This drops the deferred references before deleting the
    // node, and check_and_fuse has already dropped them on Err (line 1934). The failed
    // statement's references therefore reach rt.unref_var as pairs the runtime never
    // registered, instead of cancelling in pending_refs. Module::compile_source and
    // read_registration delete first and then call drop_deferred, which is the order
    // that is right when the top id is live. GXRt ignores an unknown pair and a
    // statement's top id is fresh, so nothing shows today; an Rt that counts strictly,
    // or a reused top id, would lose a live registration. Leave the Err cleanup to
    // check_and_fuse's callers and unwind here as delete, drop_deferred, restore;
    // compile_callable (gx.rs:946) should then delete `n` instead of dropping it with
    // `?`. probe: design/review-2026-10-05/repro/c-lib-06.gx (with GRAPHIX_DBG_VARS=1
    // it prints two UNREF_VAR lines and no matching REF_VAR). (c-lib-06)
    ctx.drop_deferred();
    node.delete(ctx);
    saved.restore(ctx);
    e
}
