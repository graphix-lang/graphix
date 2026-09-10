#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
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
pub use effects::Effect;
pub mod env;
pub mod expr;
pub mod fusion;
pub mod ide;
pub mod node;
pub mod node_shape;
pub mod perfdbg;
pub(crate) mod profile;
pub(crate) mod stack;
pub use stack::set_stack_budget;
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
        atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering},
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
    /// Disable fusion: no kernels are built or spliced and the program
    /// runs purely through the node-walk.
    FusionDisabled,
    /// REPL policy: a colliding `use` shadows instead of erroring.
    ReplaceImports,
    /// Print each `seq`'s lowered machine to stdout as it is compiled
    /// (`graphix --expand`): the source position, then the program.
    ExpandSeq,
}

/// Runtime control signals shared between a runtime handle and the
/// running `ExecCtx`. `Interrupt` makes in-flight loops abort to bottom
/// while the runtime keeps going; `Abort` also shuts the runtime down.
/// Polled lock-free via [`ExecCtx::interrupted`] and `graphix_interrupted`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[bitflags]
#[repr(u32)]
pub enum CtlFlag {
    Interrupt = 1,
    Abort = 2,
    /// Set beside `Abort` when the stack budget stopped the runtime.
    Budget = 4,
}

/// A runtime diagnostic: a failure whose value-level outcome is bottom
/// (nothing for `?` to catch), surfaced through the runtime's event
/// stream. Pushed to [`ExecCtx::diagnostics`], drained every cycle.
/// Currently no producer exists.
#[derive(Debug, Clone)]
pub enum RtDiagnostic {}

impl std::fmt::Display for RtDiagnostic {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}

/// Lock-free [`CtlFlag`] set. A loop polls [`Control::interrupted`];
/// the run loop polls [`Control::aborted`].
#[derive(Debug)]
pub struct Control {
    flags: AtomicU32,
}

impl Default for Control {
    fn default() -> Self {
        Self::new()
    }
}

impl Control {
    pub fn new() -> Self {
        Control { flags: AtomicU32::new(0) }
    }

    /// Request that in-flight loops abort this cycle; cleared at the
    /// end of the cycle.
    pub fn interrupt(&self) {
        self.flags.fetch_or(CtlFlag::Interrupt as u32, Ordering::Release);
    }

    /// Request shutdown: in-flight loops abort and the run loop returns
    /// before the next cycle. Sticky.
    pub fn abort(&self) {
        self.flags.fetch_or(CtlFlag::Abort as u32, Ordering::Release);
    }

    /// [`Self::abort`], marked as the stack budget's doing.
    pub fn abort_budget(&self) {
        self.flags
            .fetch_or(CtlFlag::Abort as u32 | CtlFlag::Budget as u32, Ordering::Release);
    }

    /// True if the stack budget aborted this runtime.
    pub fn budget_aborted(&self) -> bool {
        self.flags.load(Ordering::Acquire) & (CtlFlag::Budget as u32) != 0
    }

    /// True if any control flag is set: a loop should abort.
    pub fn interrupted(&self) -> bool {
        self.flags.load(Ordering::Acquire) != 0
    }

    /// True if `Abort` is set.
    pub fn aborted(&self) -> bool {
        self.flags.load(Ordering::Acquire) & (CtlFlag::Abort as u32) != 0
    }

    /// Clear the `Interrupt` bit, leaving `Abort` sticky.
    pub fn clear_interrupt(&self) {
        self.flags.fetch_and(!(CtlFlag::Interrupt as u32), Ordering::Release);
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
    /// Print each type variable with its binding or "unbound".
    DerefTVars,
    /// Print core's short names for primitive sets (`Any`, `Number`).
    ReplacePrims,
    /// Print an Origin's location without its source.
    NoSource,
    /// Print an Origin without its parents.
    NoParents,
}

thread_local! {
    static PRINT_FLAGS: Cell<BitFlags<PrintFlag>> = Cell::new(PrintFlag::ReplacePrims.into());
}

/// Global pool of channel watch batches.
pub static CBATCH_POOL: LazyLock<Pool<Vec<(BindId, Box<dyn CustomBuiltinType>)>>> =
    LazyLock::new(|| Pool::new(10000, 1000));

/// Run `f` with the given type-formatting flags on this thread.
pub fn format_with_flags<G: Into<BitFlags<PrintFlag>>, R, F: FnOnce() -> R>(
    flags: G,
    f: F,
) -> R {
    let prev = PRINT_FLAGS.replace(flags.into());
    let res = f();
    PRINT_FLAGS.set(prev);
    res
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
    /// The innermost overlay: same-cycle transient deliveries at depth
    /// 0, or the current evaluation frame's private writes. Not the
    /// value store: reads fall through the frame stack to [`Rt::store`].
    pub variables: IntMap<BindId, TagValue>,
    /// The enclosing overlays of the frame stack, innermost last.
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

    /// Push the current `variables` onto the frame stack and make
    /// `frame` the innermost overlay.
    pub fn enter_frame(&mut self, frame: IntMap<BindId, TagValue>) {
        self.frames.push(std::mem::replace(&mut self.variables, frame));
    }

    /// Leave the frame entered by [`Self::enter_frame`], handing back
    /// its final map.
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

/// Metadata for a `let foo = |...| 'builtin_name` binding
/// ([`ExecCtx::builtin_bindings`]): the canonical builtin `name`, the
/// source-level `argspec` (with labeled defaults), and the binding's
/// declared `typ`.
#[derive(Debug, Clone)]
pub struct BuiltinBindInfo {
    pub name: ArcStr,
    pub argspec: triomphe::Arc<[expr::Arg]>,
    pub typ: triomphe::Arc<typ::FnType>,
    /// The binding's lambda definition; labeled defaults compile in its
    /// env and scope.
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

/// A function application. Its arguments are owned by the `CallSite`,
/// so the function can change at runtime without recompiling them.
pub trait Apply<R: Rt, E: UserEvent>: Debug + Send + Sync + Any {
    /// Typed view for analysis code. The default is `BuiltIn` (opaque).
    fn view(&self) -> ApplyView<'_, R, E> {
        ApplyView::BuiltIn
    }

    /// Mutable counterpart to [`Self::view`].
    fn view_mut(&mut self) -> ApplyViewMut<'_, R, E> {
        ApplyViewMut::BuiltIn
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
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    /// Second typecheck pass.
    fn typecheck1(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        Ok(())
    }

    /// The lambda's type; the BuiltIn wrapper implements it for builtins.
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

    /// Record every id bound and referenced by this node. Only needed
    /// by builtins that create nodes.
    fn refs<'a>(&self, _refs: &mut Refs) {}

    /// Pause the builtin (an unselected arm). Values and semantic state
    /// are retained; a builtin that discards pending work or detaches an
    /// event source must clear its output to phantom, and the restart
    /// builtins clear their latches.
    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>);

    /// Clear replay caches (cached argument values), preserve semantic
    /// state (accumulators, memos); see [`Update::reset_replay`]. No
    /// default: every builtin classifies its own state.
    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>);

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
    fn fuse(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }
}

/// Typed view of an [`Apply`], symmetric to [`NodeView`]: a Graphix
/// lambda with a walkable body, or an opaque builtin.
pub enum ApplyView<'a, R: Rt, E: UserEvent> {
    Lambda(&'a GXLambda<R, E>),
    BuiltIn,
}

/// Mutable counterpart to [`ApplyView`].
pub enum ApplyViewMut<'a, R: Rt, E: UserEvent> {
    Lambda(&'a mut GXLambda<R, E>),
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
    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()>;

    /// Second typecheck pass, after `typecheck0` finished the whole
    /// tree: `lambda_ids` are final, so call sites can resolve
    /// statically. No default: every node must recurse into its children.
    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()>;

    /// The node's type.
    fn typ(&self) -> &Type;

    /// Record every bind id referenced or bound by the node and its
    /// children.
    fn refs(&self, refs: &mut Refs);

    /// The expression this node was compiled from.
    fn spec(&self) -> &Expr;

    /// Pause the node (an unselected arm).
    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>);

    /// Clear replay caches (the last-seen input values a node combines
    /// a fresh input with) while preserving semantic state (a tally, a
    /// queue, a fired flag). Called between tail-loop frames. No
    /// default: the classification is per node. Recurses like `sleep`.
    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>);

    /// The node's typed view for compile-time analysis.
    fn view(&self) -> NodeView<'_, R, E>;

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
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved_type: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>>;
}

/// A compile-time check for a `#[..]` attribute, dispatched by the
/// fusion walk on the final (possibly fused) node of the decorated
/// expression; under `--no-fusion` it never runs. `Err` is a compile
/// error. Registered via [`ExecCtx::register_attribute`]. The
/// definition assertions (`#[tail_recursive]`/`#[sync]`/`#[async]`)
/// are compiler-reserved, not registry attributes.
pub type AttributeCheckFn<R, E> = fn(&ExecCtx<R, E>, &Attr, &Node<R, E>) -> Result<()>;

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
    /// The bare attribute name (`native` for `#[native]`); a flat
    /// global namespace.
    const NAME: &str;
    fn check(ctx: &ExecCtx<R, E>, attr: &Attr, node: &Node<R, E>) -> Result<()>;
}

/// `#[native]`: the decorated expression must compile to native code
/// with zero node-walk residue. A function-typed target is rejected;
/// the requirement belongs at the use site.
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
        if let NodeView::FusedKernel(_) = node.view() {
            return Ok(());
        }
        if !fusion::region_is_candidate(node) {
            crate::bailat!(
                node.spec(),
                "#[native] annotates a computation; a declaration or a bare \
                 variable read has nothing to fuse — put it on the initializer"
            );
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
    pub fn get<T>(&mut self) -> Option<&T>
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

/// Side channel for the interpreter's tail-recursion loop: a
/// tail-position self-call stashes its rebind args here and returns;
/// the enclosing `GXLambda::update` rebinds the formals and re-runs the
/// body. One slot suffices: a tail call is the last thing evaluated.
pub(crate) struct PendingTailCall {
    /// The recursive callee's `LambdaId`.
    pub(crate) lambda: LambdaId,
    /// The self-call's argument productions in callee-formal order.
    /// `None`: the arg produced nothing this jump and the formal rides
    /// its previous value. Each present arg keeps its own tag, so
    /// freshness rides the dataflow as the kernel's disc carry does.
    pub(crate) args: smallvec::SmallVec<[Option<TagValue>; 4]>,
}

/// A call site's instantiation identity: per argument, the source
/// lambda ([`node::lambda::LambdaDef::source`]) it statically resolves
/// to, or `None`. Two sites reaching one def with the same identity are
/// one instantiation (a self-call); different identities are distinct
/// even while the def is resolving. Source identity, not `LambdaId`,
/// because a literal in an instance body is re-minted per compile.
pub(crate) type FnArgIdentity = smallvec::SmallVec<[Option<ExprId>; 4]>;

#[derive(Clone)]
pub(crate) struct ResolvingLambda {
    pub instance: LambdaInstanceId,
    pub ftype: FnType,
    pub identity: FnArgIdentity,
}

/// The active instantiations of one def, innermost last. A stack: a
/// site inside `h(k)` may reach a still-resolving `h(g)`.
pub(crate) type ResolvingStack = smallvec::SmallVec<[ResolvingLambda; 2]>;

impl<R: Rt, E: UserEvent> ExecCtx<R, E> {
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

pub struct ExecCtx<R: Rt, E: UserEvent> {
    lambdawrap: AbstractWrapper<LambdaDef<R, E>>,
    builtins: AHashMap<&'static str, BuiltInInitFn<R, E>>,
    attributes: AHashMap<&'static str, AttributeCheckFn<R, E>>,
    // Sandboxing.
    builtins_allowed: bool,
    tags: AHashSet<ArcStr>,
    /// Library state for builtins.
    pub libstate: LibState,
    /// The language environment: typedefs, binds, lambdas.
    pub env: Env,
    /// The runtime.
    pub rt: R,
    /// LambdaDefs by LambdaId.
    pub lambda_defs: IntMap<LambdaId, Value>,
    /// The call sites through which `Value` comparison and printing
    /// reach core-trait implementations, built on first use.
    pub(crate) core_hook_sites: node::coretraits::CoreHookSites<R, E>,
    /// `BindId → LambdaDef Value` for every lambda binding, filled in
    /// `typecheck0` so `typecheck1`'s static resolution sees it
    /// complete. Persistent across batches (`Bind::delete` removes
    /// its ids); the `batch_connect_targets` guard excludes `<-` targets
    /// at read time.
    pub bind_to_lambda: IntMap<BindId, Value>,
    /// The `<-` targets of the current compile batch, recorded by
    /// [`node::Connect::compile`]; a `<-` target rebinds at runtime and
    /// must not be statically resolved.
    pub batch_connect_targets: IntSet<BindId>,
    /// Every `<-` target for the program's lifetime (`batch_connect_targets`
    /// is per batch): `Bind::update` must not reseed a woken target that
    /// holds a value. `Bind::delete` removes its ids.
    pub connect_targets: IntSet<BindId>,
    /// Builtin metadata for `let foo = |...| 'builtin_name` bindings.
    /// Keyed by `(scope, name)` because a sig and its impl share the
    /// name but have distinct `BindId`s.
    pub builtin_bindings: AHashMap<(ModPath, CompactString), BuiltinBindInfo>,
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
    pub(crate) resolving_lambdas:
        Arc<parking_lot::Mutex<nohash::IntMap<LambdaId, ResolvingStack>>>,
    /// Per-instance fn-formal BindId → the `LambdaId` forwarded to it:
    /// the persistent record the kernel cache fingerprint reads after
    /// the re-drive's `bind_to_lambda` entry is gone.
    pub(crate) fn_forward_resolutions: IntMap<BindId, LambdaId>,
    /// Deferred terminal settles, one frame per resolution scope. A
    /// call site pushes its resolved signature into the current frame;
    /// statement boundaries drain it, so a settle runs only after every
    /// writer in its scope. A re-drive's leftovers merge up to the
    /// parent frame. Entries: (resolved sig, the site's rtype cell,
    /// defaulted-arg cells exempt from settling, the site spec).
    pub(crate) pending_settles: Vec<
        Vec<(
            typ::FnType,
            Option<typ::TVar>,
            ahash::AHashSet<usize>,
            triomphe::Arc<expr::Expr>,
        )>,
    >,
    /// The fusion subsystem's state; see [`fusion::FusionCtx`].
    pub fusion: fusion::FusionCtx,
    /// See [`PendingTailCall`].
    pub(crate) pending_tail_call: Option<PendingTailCall>,
    /// Imports whose terminal name did not exist when the `use`
    /// compiled (`use self::sub::x` may precede `mod sub;`); re-checked
    /// at the end of [`compile_stmt`].
    pub(crate) pending_imports: Vec<PendingImport>,
    /// Module names pre-registered by a block's header scan, so `mod`
    /// declaration order does not matter.
    pub(crate) predeclared_mods: AHashSet<ModPath>,
    /// `LambdaId`s whose `GXLambda::update` is on the Rust stack, with
    /// activation counts. `CallSite::bind` binds a recursive unfold
    /// transient (`callsite::transient_body_ok`), so non-tail recursion
    /// holds O(depth) instances.
    pub(crate) active_lambdas: nohash::IntMap<LambdaId, u32>,
    /// Interrupt/abort control, shared with the runtime handle. See
    /// [`Control`].
    pub control: Arc<Control>,
    /// Runtime diagnostics produced during the current cycle; see
    /// [`RtDiagnostic`].
    pub diagnostics: Vec<RtDiagnostic>,
    /// Non-zero while a tail-loop re-entry runs against a private
    /// per-frame variables map; frame-only behaviors gate on it. A
    /// counter: frames nest.
    pub(crate) frame_depth: u32,
    /// The real `event.init` of the dispatch whose frames are running
    /// (frames force `event.init` for re-derivation). Only meaningful
    /// when `frame_depth > 0`.
    pub(crate) dispatch_init: bool,
    /// Set only while a `Select::update` sleeps an arm it is
    /// deselecting: a recursive-edge `CallSite::sleep` under it deletes
    /// its callee (shrink = delete). Cleared crossing into any callee
    /// body, so a whole-recursion pause retains.
    pub(crate) deselecting_arm: bool,
    /// Whether any tail-spine select's scrutinee fired during the
    /// current tail-loop dispatch: the dispatch's result fires if its
    /// value chain did or any such scrutinee did.
    pub(crate) tail_scrut_fired: bool,
    /// Pending definition assertions; see [`DefAssertion`].
    pub(crate) def_assertions: Mutex<Vec<DefAssertion>>,
    /// Registry attributes recorded this `compile_stmt`; each must be
    /// dispatched or absorbed by the fusion walk, or the statement errors.
    pub(crate) attr_census: Mutex<Vec<Expr>>,
    pub(crate) attr_dispatched: Mutex<IntSet<ExprId>>,
    pub(crate) attr_absorbed: Mutex<IntSet<ExprId>>,
    /// Variable deliveries raised inside an evaluation frame that must
    /// escape it (an error delivery to a `catch` handler); drained into
    /// the real map at `frame_depth == 0`.
    pub(crate) frame_outbox: Vec<(BindId, Value)>,
}

impl<R: Rt, E: UserEvent> ExecCtx<R, E> {
    pub fn clear(&mut self) {
        self.env.clear();
        self.rt.clear();
    }

    /// True while an evaluation frame (a tail-loop pass) is running.
    pub fn in_frame(&self) -> bool {
        self.frame_depth > 0
    }

    /// Build a new execution context. A low-level interface for custom
    /// runtimes; most embedders want `graphix-rt`.
    pub(crate) fn mark_connect_target(&mut self, id: BindId) {
        self.batch_connect_targets.insert(id);
        self.connect_targets.insert(id);
    }

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
            core_hook_sites: node::coretraits::CoreHookSites::default(),
            bind_to_lambda: IntMap::default(),
            batch_connect_targets: nohash::IntSet::default(),
            connect_targets: nohash::IntSet::default(),
            builtin_bindings: ahash::AHashMap::default(),
            rec_defs: nohash::IntSet::default(),
            def_gate_params: nohash::IntSet::default(),
            def_gate_depth: 0,
            resolving_lambdas: Arc::new(parking_lot::Mutex::new(
                nohash::IntMap::default(),
            )),
            fn_forward_resolutions: IntMap::default(),
            pending_settles: vec![Vec::new()],
            fusion: fusion::FusionCtx::new()?,
            pending_tail_call: None,
            pending_imports: Vec::new(),
            predeclared_mods: AHashSet::default(),
            active_lambdas: nohash::IntMap::default(),
            control: Arc::new(Control::new()),
            diagnostics: Vec::new(),
            frame_depth: 0,
            dispatch_init: false,
            deselecting_arm: false,
            tail_scrut_fired: false,
            def_assertions: Mutex::new(Vec::new()),
            attr_census: Mutex::new(Vec::new()),
            attr_dispatched: Mutex::new(IntSet::default()),
            attr_absorbed: Mutex::new(IntSet::default()),
            frame_outbox: Vec::new(),
        };
        this.register_attribute::<Native>()?;
        Ok(this)
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
        self.fusion.builtin_facts.insert(T::NAME, effects::BuiltinFacts::from(T::EFFECT));
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

    /// The check fn for a registered attribute.
    pub fn lookup_attribute(&self, name: &str) -> Option<AttributeCheckFn<R, E>> {
        self.attributes.get(name).copied()
    }

    /// A registered builtin's effect; `Async` for unknown names.
    pub fn builtin_effect(&self, name: &str) -> EffectKind {
        self.fusion.builtin_facts.get(name).map(|f| f.effect).unwrap_or_default()
    }

    /// Whether a registered builtin is [`Effect::Stateless`]; `false`
    /// for unknown names.
    pub fn builtin_stateless(&self, name: &str) -> bool {
        self.fusion.builtin_facts.get(name).map(|f| f.stateless).unwrap_or(false)
    }

    /// A registered builtin's direct-call entry, if its
    /// [`Effect::Stateless`] carries one.
    pub fn builtin_fastcall(&self, name: &str) -> Option<FastCall> {
        self.fusion.builtin_facts.get(name).and_then(|f| f.fastcall)
    }

    /// Wrap a `LambdaDef` into a first-class function `Value` and
    /// register it in `lambda_defs`.
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

    /// Run `f` with the lexical environment restored to `env`, then put
    /// the current one back. Bindings `f` creates are retained.
    pub fn with_restored<T, F: FnOnce(&mut Self) -> T>(&mut self, env: Env, f: F) -> T {
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

    /// The scope covered by a handler installed here.
    pub fn with_catch(&self, catch: (BindId, ExprId)) -> Self {
        Self { lexical: self.lexical.clone(), dynamic: self.dynamic.with_catch(catch) }
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

    pub(crate) fn generation(&self) -> u64 {
        self.0.raised.load(Ordering::Relaxed)
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

    pub fn with_catch(&self, catch: (BindId, ExprId)) -> Self {
        Self(Some(ErrorHandler(Arc::new(DynNode {
            catch,
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
/// both typecheck passes, the deferred settles, function-property
/// analysis, typedef resolution-cell seeding (in both modes) and
/// fusion. The caller restores its env on `Err`.
pub fn check_and_fuse<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    node: &mut Node<R, E>,
) -> Result<()> {
    let st = Instant::now();
    let p = profile::phase(Phase::Typecheck0);
    node.typecheck0(ctx)?;
    drop(p);
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
        fusion::fuse(node, ctx)?;
        drop(p);
        info!("fusion time {:?}", st.elapsed());
    }
    Ok(())
}

/// Drain the deferred terminal settles ([`ExecCtx::pending_settles`])
/// after a top-level statement, once every writer for the drained
/// sites has run.
pub(crate) fn drain_pending_settles<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
) -> Result<()> {
    use ::anyhow::Context as _;
    let pending = mem::take(ctx.pending_settles.last_mut().expect("root settle frame"));
    for (ft, rtc, defaulted, spec) in pending.iter() {
        ft.settle_terminal(&ctx.env, rtc.as_ref(), defaulted)
            .with_context(|| expr::ErrorContext((**spec).clone()))?;
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
    let _profile = profile::phase(Phase::Compile);
    // Fusion also runs in check/lsp runtimes: `#[native]` needs it to
    // verify its contract, and a malformed input only de-fuses.
    ctx.fusion.enabled = !flags.contains(CFlag::FusionDisabled);
    ctx.attr_census.lock().clear();
    ctx.attr_dispatched.lock().clear();
    ctx.attr_absorbed.lock().clear();
    ctx.pending_imports.clear();
    ctx.predeclared_mods.clear();
    ctx.pending_settles.clear();
    ctx.pending_settles.push(Vec::new());
    let top_id = spec.id;
    ctx.fusion.top_id = Some(top_id);
    let env = ctx.env.clone();
    let st = Instant::now();
    let build_profile = profile::phase(Phase::BuildGraph);
    let compiled = match &spec.kind {
        expr::ExprKind::Catch(c) => {
            let c = c.clone();
            node::error::Catch::compile(ctx, flags, spec, scope, top_id, &c)
        }
        // Declarations are legal only in statement position; `compile`
        // rejects them.
        expr::ExprKind::Use { reexport, names } => {
            let (reexport, names) = (*reexport, names.clone());
            node::compile_use(ctx, flags, spec, scope, reexport, &names)
                .map(|n| (n, scope.clone()))
        }
        expr::ExprKind::Module { name, value } => {
            let (name, value) = (name.clone(), value.clone());
            compiler::compile_module(ctx, flags, spec, scope, top_id, &name, &value)
                .map(|n| (n, scope.clone()))
        }
        expr::ExprKind::TypeDef(td) => {
            let td = td.clone();
            node::TypeDef::compile(ctx, spec, scope, &td.name, &td.params, &td.body)
                .map(|n| (n, scope.clone()))
        }
        expr::ExprKind::Trait(t) => {
            let t = t.clone();
            node::traits::Trait::compile(ctx, flags, spec, scope, &t, top_id)
                .map(|n| (n, scope.clone()))
        }
        expr::ExprKind::Impl(im) => {
            let im = im.clone();
            node::traits::Impl::compile(ctx, flags, spec, scope, &im, top_id)
                .map(|n| (n, scope.clone()))
        }
        _ => {
            compiler::compile(ctx, flags, spec, scope, top_id).map(|n| (n, scope.clone()))
        }
    };
    drop(build_profile);
    let (mut node, out_scope) = match compiled {
        Ok(n) => n,
        Err(e) => {
            ctx.env = env;
            return Err(e);
        }
    };
    info!("compile time {:?}", st.elapsed());
    // A `use` whose name did not exist at its compile position must
    // name something by the end of the statement.
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
    if let Err(e) = check_and_fuse(ctx, &mut node) {
        ctx.env = env;
        return Err(e);
    }
    // An attribute the fusion walk neither dispatched nor absorbed
    // would silently assert nothing.
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
