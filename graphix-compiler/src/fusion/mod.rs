//! Fusion: JIT-compile pure subtrees of the compiled node graph to
//! native kernels.
//!
//! Code generation is distributed: each node's `Update::emit_clif` /
//! `Apply::emit_clif` emits its own CLIF and [`fuse`] drives the
//! `Update::fuse` recursion. This module supplies the shared mechanics:
//! [`try_fuse`] (early effect rejection and whole-subtree compilation),
//! [`fuse`] (the child-visit protocol),
//! [`lowering`] (discovery and signature derivation) and [`kernel`]
//! (the runtime [`FusedKernel`] node).

pub mod emit;
pub mod emit_helpers;
pub mod kernel;
pub mod kernel_abi;
pub mod lowering;
pub(crate) mod par_loop;
pub(crate) mod share;

pub use kernel::FusedKernel;

use crate::{
    ApplyView, BindId, CompileCtx, LambdaId, Node, NodeView, PrintFlag, Refs, Rt, Update,
    UserEvent,
    env::Env,
    expr::{Expr, ExprId, ExprKind, ModPath, Origin},
    format_with_flags,
    fusion::{
        kernel_abi::{
            AbiKind, FreezeError, KernelParam, KernelSig, ParamKind,
            freeze_for_abi_normalized, try_freeze_for_abi_normalized,
        },
        lowering::expand_refs,
    },
    node::{self, callsite::CallNode, genn},
    profile::{self, Phase},
    typ::{FnType, Type},
};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use dashmap::DashMap;
use parking_lot::{MappedMutexGuard, MutexGuard};
use poolshark::local::LPooled;
use rayon::prelude::*;
use smallvec::SmallVec;
use std::{cell::RefCell, collections::BTreeMap, sync::LazyLock};
use triomphe::Arc;

#[derive(Debug, Clone)]
struct FusionSource {
    origin: triomphe::Arc<Origin>,
    pos: crate::SourcePosition,
    kind: std::mem::Discriminant<ExprKind>,
}

impl FusionSource {
    fn new(spec: &Expr) -> Self {
        Self {
            origin: spec.ori.clone(),
            pos: spec.pos,
            kind: std::mem::discriminant(&spec.kind),
        }
    }

    fn matches(&self, spec: &Expr) -> bool {
        self.origin.as_ref() == spec.ori.as_ref()
            && self.pos == spec.pos
            && self.kind == std::mem::discriminant(&spec.kind)
    }
}

#[derive(Debug, Clone)]
pub struct FusionFailure {
    pub id: ExprId,
    pub reason: ArcStr,
    source: FusionSource,
}

impl FusionFailure {
    pub(crate) fn matches(&self, spec: &Expr) -> bool {
        self.source.matches(spec)
    }
}

#[derive(Debug)]
pub(crate) struct FusionBlocker {
    spec: Expr,
    reason: CompactString,
}

impl std::fmt::Display for FusionBlocker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.reason.fmt(f)
    }
}

impl std::error::Error for FusionBlocker {}

pub(crate) fn blocker(spec: &Expr, reason: CompactString) -> anyhow::Error {
    FusionBlocker { spec: spec.clone(), reason }.into()
}

/// Compile-time fusion outcome counters, accumulated on
/// [`FusionCtx::stats`] by every `compile()` the context runs. A
/// region that fails to compile node-walks and produces the correct
/// value, so these counters are the only way to ask "did it fuse, and
/// if not why".
#[derive(Debug, Clone, Default)]
pub struct FusionStats {
    /// `try_fuse` attempts that passed the root-shape and return-type gates.
    pub attempted: usize,
    /// Regions that compiled and were spliced in.
    pub fused: usize,
    /// Attempts rejected during discovery, before input collection or emission.
    pub rejected_before_emit: usize,
    /// Per-failure source identity and compile error, across the
    /// context's compiles.
    pub failed: Vec<FusionFailure>,
    /// JIT module rotations: an exhausted module is replaced by a fresh
    /// one, and its ~256MB arena is freed when the last kernel compiled
    /// into it drops.
    pub jit_generations: usize,
    /// Region roots that fused. Distinguishes a structural `failed`
    /// entry (a block whose value fused in a sub-region) from a real
    /// blocker with nothing fused beneath it.
    fused_sources: Vec<FusionSource>,
}

impl FusionStats {
    /// Add a compile task's counts after this one's.
    fn join(&mut self, other: Self) {
        let Self {
            attempted,
            fused,
            rejected_before_emit,
            failed,
            jit_generations,
            fused_sources,
        } = other;
        self.attempted += attempted;
        self.fused += fused;
        self.rejected_before_emit += rejected_before_emit;
        self.failed.extend(failed);
        self.jit_generations += jit_generations;
        self.fused_sources.extend(fused_sources);
    }

    fn record_failure(&mut self, spec: &Expr, reason: &str) {
        self.failed.push(FusionFailure {
            id: spec.id,
            reason: ArcStr::from(reason),
            source: FusionSource::new(spec),
        });
    }

    fn record_fused(&mut self, spec: &Expr) {
        self.fused += 1;
        self.fused_sources.push(FusionSource::new(spec));
    }

    pub(crate) fn failure_for_source(&self, spec: &Expr) -> Option<&FusionFailure> {
        self.failed.iter().rev().find(|failure| failure.matches(spec))
    }

    pub(crate) fn source_fused(&self, spec: &Expr) -> bool {
        self.fused_sources.iter().any(|source| source.matches(spec))
    }
}

/// A fusion pass's memo of the env-dependent type work its regions
/// repeat: expansions and freezes. Installed for one pass by
/// [`TypeMemo::scope`] and shared by its compile tasks
/// ([`TypeMemo::current`], [`TypeMemo::enter`]); outside a pass the work
/// is done every time.
#[derive(Default)]
pub(crate) struct TypeMemo {
    expanded: Table<Type>,
    frozen: Table<Result<Type, kernel_abi::FreezeError>>,
}

/// Results by the allocations a type is made of (see [`identity`]),
/// and, for types with no open cell, by content: equal content is the
/// same type once the definitions its named types resolve to agree,
/// which its equality leaves out. Resolution cells fill during a pass,
/// so only a result computed with every named type already resolved
/// is kept. A result is the same whichever task computes it.
struct Table<V> {
    by_id: DashMap<Identity, (Type, V)>,
    by_content: DashMap<(Type, u64), V>,
}

impl<V> Default for Table<V> {
    fn default() -> Self {
        Self { by_id: DashMap::default(), by_content: DashMap::default() }
    }
}

type Identity = (std::mem::Discriminant<Type>, usize, usize);

/// A key naming `t`'s content by its allocations, and the type that
/// owns them. Within a pass no type cell is bound and the env is fixed,
/// so one allocation expands and freezes the same way.
fn identity(t: &Type) -> Option<(Identity, Type)> {
    fn a<T: ?Sized>(p: &triomphe::Arc<T>) -> usize {
        triomphe::Arc::as_ptr(p) as *const () as usize
    }
    let (x, y) = match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => return identity(&b),
            None => (tv.cell_addr(), 0),
        },
        Type::Ref(tr) => (tr.cell_addr(), a(&tr.params)),
        Type::Fn(f) => (a(f), 0),
        Type::Set(ts) | Type::Tuple(ts) => (a(ts), 0),
        Type::Struct(fs) => (a(fs), 0),
        Type::Error(t) | Type::Array(t) | Type::List(t) | Type::ByRef(_, t) => (a(t), 0),
        Type::Variant(tag, ts, _) => (tag.as_ptr() as usize, a(ts)),
        Type::Map { key, value } => (a(key), a(value)),
        Type::App(f, x) => (a(f), a(x)),
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::Abstract { .. }
        | Type::Hole
        | Type::Concrete
        | Type::Singleton
        | Type::OneNumber
        | Type::Discernible
        | Type::Ordered
        | Type::Function => return None,
    };
    Some(((std::mem::discriminant(t), x, y), t.clone()))
}

/// The definitions the named types in `t` resolve to; `None` when one
/// is not resolved yet, so its expansion reads the env it is given.
fn resolutions(t: &Type) -> Option<u64> {
    use std::hash::{Hash, Hasher};
    fn walk(t: &Type, h: &mut ahash::AHasher) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::Ref(tr) => match tr.def_key() {
                None => false,
                Some(k) => {
                    k.hash(h);
                    tr.params.iter().all(|p| walk(p, h))
                }
            },
            Type::TVar(tv) => tv.binding().is_none_or(|b| walk(&b, h)),
            Type::Fn(ft) => {
                let mut ok = true;
                ft.for_each_part(&mut |t, _| ok = ok && walk(t, h));
                ok
            }
            t => {
                let mut ok = true;
                t.for_each_child(&mut |c| ok = ok && walk(c, h));
                ok
            }
        })
    }
    let mut h = ahash::AHasher::default();
    walk(t, &mut h).then(|| h.finish())
}

thread_local! {
    static TYPE_MEMO: RefCell<Option<Arc<TypeMemo>>> = const { RefCell::new(None) };
}

impl TypeMemo {
    /// Run `f` with a fresh memo for its fusion pass.
    pub(crate) fn scope<T>(f: impl FnOnce() -> T) -> T {
        Self::enter(Some(Arc::new(TypeMemo::default())), f)
    }

    /// The memo of the pass running on this thread, for its tasks.
    pub(crate) fn current() -> Option<Arc<TypeMemo>> {
        TYPE_MEMO.with(|m| m.borrow().clone())
    }

    /// Run `f` with `memo` installed on this thread.
    pub(crate) fn enter<T>(memo: Option<Arc<TypeMemo>>, f: impl FnOnce() -> T) -> T {
        let prev = TYPE_MEMO.with(|m| m.replace(memo));
        let r = f();
        TYPE_MEMO.with(|m| *m.borrow_mut() = prev);
        r
    }

    fn cached<V: Clone>(
        t: &Type,
        table: fn(&TypeMemo) -> &Table<V>,
        too_big: impl FnOnce() -> V,
        compute: impl FnOnce() -> V,
    ) -> V {
        // the walks below treat a type as a tree: one that shares its
        // parts may unfold exponentially, past what any kernel encodes
        if lowering::unfolds_past(t, lowering::FUSION_SIZE_CAP) {
            return too_big();
        }
        let Some((id, owner)) = identity(t) else { return compute() };
        let Some(memo) = Self::current() else { return compute() };
        let tb = table(&memo);
        if let Some(v) = tb.by_id.get(&id).map(|e| e.1.clone()) {
            return v;
        }
        let Some(resolved) = resolutions(t) else { return compute() };
        let key = (!t.has_unbound()).then(|| (t.clone(), resolved));
        let hit = key.as_ref().and_then(|k| tb.by_content.get(k).map(|v| v.clone()));
        let v = hit.unwrap_or_else(compute);
        if let Some(k) = key {
            tb.by_content.insert(k, v.clone());
        }
        tb.by_id.insert(id, (owner, v.clone()));
        v
    }

    pub(crate) fn expanded(t: &Type, compute: impl FnOnce() -> Type) -> Type {
        Self::cached(t, |m| &m.expanded, || t.clone(), compute)
    }

    pub(crate) fn frozen(
        t: &Type,
        compute: impl FnOnce() -> Result<Type, kernel_abi::FreezeError>,
    ) -> Result<Type, kernel_abi::FreezeError> {
        Self::cached(
            t,
            |m| &m.frozen,
            || Err(kernel_abi::FreezeError::Unsupported),
            compute,
        )
    }
}

/// Regions linked together: enough work for every core, few enough
/// functions held uncompiled.
const LINK_BATCH: usize = 256;

pub(crate) type KernelCacheKey =
    (LambdaId, Arc<FnType>, lowering::QopCoverage, lowering::FnResolutions);

enum JitSlot {
    Unbuilt,
    Built(emit::Jit),
    /// The build failed: the context runs unfused.
    Unavailable,
}

/// Per-[`ExecCtx`] state owned by the fusion subsystem, reached as
/// `ctx.fusion.<x>`.
pub struct FusionCtx {
    /// Per-context cranelift module, built on first use ([`Self::jit`]),
    /// shared by the context's compile tasks for installing. The mutex is
    /// interior mutability for `ExecCtx`'s `Sync` bound; JIT ops are
    /// compile-time only.
    jit: Arc<parking_lot::Mutex<JitSlot>>,
    /// What this context, or this compile task, emits before a link: the
    /// names, the kernel caches (lambda kernel signatures and bodies; a
    /// later compile may call an earlier one's lambda) and the functions
    /// waiting. Built with the JIT; a fusion task's is forked from its
    /// parent's ([`fuse_each`]).
    emission: parking_lot::Mutex<Option<emit::Emission>>,
    /// Compile-time fusion outcome counters, accumulated across every
    /// `compile()` this context runs. See [`FusionStats`].
    pub stats: FusionStats,
    /// The top expression id of the running compile. Feeder Refs must
    /// register under it: `Rt::ref_var` is keyed `(BindId, top_id)`, and
    /// a region's interior id would strand the top expression at count 0.
    pub(crate) top_id: Option<ExprId>,
    /// The collection prototype or slot walk in progress, if any.
    pub(crate) share: Option<share::Share>,
}

impl FusionCtx {
    /// A compile task's fusion state: the module shared, its own outcome
    /// counters, which [`Self::join`] adds back. A fusion task gets an
    /// emission of its own ([`fuse_each`]); any other emits nothing.
    pub(crate) fn fork(&self) -> Self {
        Self {
            jit: self.jit.clone(),
            emission: parking_lot::Mutex::new(None),
            stats: FusionStats::default(),
            top_id: self.top_id,
            share: None,
        }
    }

    pub(crate) fn join(&mut self, fork: Self) {
        self.stats.join(fork.stats);
        if let Some(task) = fork.emission.into_inner() {
            self.emission
                .get_mut()
                .as_mut()
                .expect("a fusion task's parent emits")
                .join(task)
        }
    }

    /// The context's JIT module, built on first use.
    pub(crate) fn jit(&self) -> anyhow::Result<MappedMutexGuard<'_, emit::Jit>> {
        let mut jit = self.jit.lock();
        if let JitSlot::Unbuilt = *jit {
            *jit = match emit::Jit::new() {
                Ok(j) => JitSlot::Built(j),
                Err(e) => {
                    log::warn!("fusion is unavailable, the program runs unfused: {e:#}");
                    JitSlot::Unavailable
                }
            }
        }
        MutexGuard::try_map(jit, |jit| match jit {
            JitSlot::Built(j) => Some(j),
            JitSlot::Unbuilt | JitSlot::Unavailable => None,
        })
        .map_err(|_| anyhow::anyhow!("fusion is unavailable"))
    }

    /// Install the kernels an image decode session restored; the session
    /// ends with this, before anything runs.
    pub(crate) fn install_restored(&self) -> anyhow::Result<()> {
        match &mut *self.jit.lock() {
            JitSlot::Built(jit) => jit.install_restored(),
            JitSlot::Unbuilt | JitSlot::Unavailable => Ok(()),
        }
    }

    /// Whether this context can fuse: its JIT builds.
    pub(crate) fn available(&self) -> bool {
        self.jit().is_ok()
    }

    /// What this context or task emits into, built with the JIT.
    pub(crate) fn emission(
        &self,
    ) -> anyhow::Result<MappedMutexGuard<'_, emit::Emission>> {
        let mut em = self.emission.lock();
        if em.is_none() {
            *em = Some(self.jit()?.emission());
        }
        Ok(MutexGuard::map(em, |em| em.as_mut().expect("built above")))
    }

    /// A lambda kernel this task has built, or its parents had.
    pub(crate) fn kernel(&self, key: &KernelCacheKey) -> Option<LambdaCallInfo> {
        self.emission.lock().as_ref()?.kernel(key)
    }

    pub(crate) fn cache_kernel(&self, key: KernelCacheKey, info: LambdaCallInfo) {
        if let Ok(mut em) = self.emission() {
            em.cache_kernel(key, info)
        }
    }

    /// How many regions this context emitted wait for the next link; a
    /// fusion task's count for nothing, it does not link.
    pub(crate) fn unlinked(&self) -> usize {
        match self.emission.lock().as_ref() {
            Some(em) if em.is_root() => em.unlinked(),
            _ => 0,
        }
    }

    /// Compile and install every region fused since the last link; a
    /// region's kernel has its entry from here on.
    pub(crate) fn link(&mut self) {
        self.with_jit(emit::Jit::link)
    }

    /// Start compiling every region fused since the last link while
    /// fusion goes on; they install at the next link or batch.
    pub(crate) fn link_batch(&mut self) {
        self.with_jit(emit::Jit::link_batch)
    }

    fn with_jit(&mut self, f: impl FnOnce(&mut emit::Jit, Vec<emit::Pending>)) {
        let pending = match self.emission.get_mut() {
            Some(em) => {
                debug_assert!(em.is_root(), "only the context links");
                em.take_pending()
            }
            None => Vec::new(),
        };
        let mut jit = self.jit.lock();
        let JitSlot::Built(jit) = &mut *jit else { return };
        let retired = jit.retired();
        f(jit, pending);
        self.stats.jit_generations += jit.retired() - retired;
    }

    /// A slot walk: its source was checked when the prototype fused.
    pub(crate) fn reusing(&self) -> bool {
        matches!(self.share, Some(share::Share::Reuse { .. }))
    }

    pub fn new() -> anyhow::Result<Self> {
        Ok(Self {
            jit: Arc::new(parking_lot::Mutex::new(JitSlot::Unbuilt)),
            emission: parking_lot::Mutex::new(None),
            stats: FusionStats::default(),
            top_id: None,
            share: None,
        })
    }
}

/// One free-var input slot resolved during walker analysis.
#[derive(Debug, Clone)]
pub(crate) struct FreeVarInput {
    pub(crate) bind_id: BindId,
    pub(crate) name: ArcStr,
    /// Kernel-input classification, computed once from the binding's type.
    pub(crate) kind: ParamKind,
    /// Full graphix type, needed by the runtime feeder Node.
    pub(crate) typ: Type,
}

/// Collect every external Ref of the subtree (referenced but not bound
/// inside it) and resolve each to a [`FreeVarInput`]. A statically
/// resolved lambda's captures surface here through `CallSite::refs`,
/// which is what makes capture forwarding automatic. Slots with no
/// kernel-input representation are skipped; emitting such a Ref later
/// fails the build.
pub(crate) fn collect_region_inputs<R: Rt, E: UserEvent>(
    subtree: &dyn Update<R, E>,
    ctx: &CompileCtx<R, E>,
) -> LPooled<Vec<FreeVarInput>> {
    let mut refs = Refs::default();
    subtree.refs(&mut refs);
    let mut out: LPooled<Vec<FreeVarInput>> = LPooled::take();
    let mut seen: LPooled<nohash::IntSet<BindId>> = LPooled::take();
    refs.with_external_refs(|id| {
        if !seen.insert(id) {
            return;
        }
        if let Some(fv) = free_var_input(id, ctx) {
            out.push(fv);
        }
    });
    // `Refs` iterates in set order, which varies with absolute BindId
    // values across processes; BindIds allocate in compile order, so
    // sorting makes the signature source-order-stable.
    out.sort_by_key(|fv| fv.bind_id);
    out
}

/// Resolve binding `id` to its [`FreeVarInput`], or `None` if its type
/// has no kernel-input representation.
pub(crate) fn free_var_input<R: Rt, E: UserEvent>(
    id: BindId,
    ctx: &CompileCtx<R, E>,
) -> Option<FreeVarInput> {
    let b = ctx.env.by_id.get(&id)?;
    // `freeze_for_abi` is env-free and rejects `Type::Ref`, so refs expand
    // first. The feeder keeps the unresolved type; only the slot
    // classification needs the concrete rep.
    let resolved = expand_refs(&b.typ, &ctx.env);
    // Normalized: a select with a never() arm leaves a Set polluted by
    // the arm's late-bound TVar.
    let frozen = freeze_for_abi_normalized(&resolved)?;
    let kind = lowering::param_kind(&frozen)?;
    Some(FreeVarInput {
        bind_id: id,
        name: ArcStr::from(b.name.as_str()),
        kind,
        typ: b.typ.clone(),
    })
}

/// The single definition of "tail position". A body root is a tail
/// position; tailness propagates through a `Block`'s last child (unless
/// a `catch` covers it), an `ExplicitParens`' inner node and every
/// `Select` arm body, and stops at a `Leaf`. The analysis walks and the kernel emitter must agree on
/// this set, so all of them match on this enum.
pub(crate) enum TailPosition<'a, R: Rt, E: UserEvent> {
    Block(&'a node::Block<R, E>),
    Parens(&'a node::ExplicitParens<R, E>),
    Select(&'a node::select::Select<R, E>),
    Leaf(&'a Node<R, E>),
}

pub(crate) fn tail_position<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
) -> TailPosition<'a, R, E> {
    match node.view() {
        NodeView::Block(b) if !b.value_is_caught() => TailPosition::Block(b),
        NodeView::ExplicitParens(ep) => TailPosition::Parens(ep),
        NodeView::Select(s) => TailPosition::Select(s),
        _ => TailPosition::Leaf(node),
    }
}

/// Call `f` on each tail-position leaf of `node`; returns whether any
/// call returned true. Every Select arm is visited (no short-circuit),
/// and `on_select` fires for each Select on the tail spine with a true leaf.
pub(crate) fn for_each_tail_leaf<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut impl FnMut(&'a Node<R, E>) -> bool,
    on_select: &mut impl FnMut(&'a node::select::Select<R, E>),
) -> bool {
    match tail_position(node) {
        TailPosition::Block(b) => {
            b.children.last().is_some_and(|c| for_each_tail_leaf(c, f, on_select))
        }
        TailPosition::Parens(ep) => for_each_tail_leaf(&ep.n, f, on_select),
        TailPosition::Select(s) => {
            let mut any = false;
            for (_, body) in s.arms.iter() {
                any |= for_each_tail_leaf(body, f, on_select);
            }
            if any {
                on_select(s);
            }
            any
        }
        TailPosition::Leaf(n) => f(n),
    }
}

/// Visit `node` and every reachable descendant, pre-order. The
/// `NodeView` match is exhaustive on purpose. Lambda bodies are not
/// descended (a body compiles per call site) and `FusedKernel` is opaque.
pub(crate) fn for_each_node<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    crate::stack::ensure_sufficient(|| for_each_node_inner(node, f))
}

fn for_each_node_inner<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    f(node);
    for_each_child(node, &mut |c| for_each_node::<R, E>(c, f));
}

/// A binary operator's operands.
pub(crate) fn binary_operands<'a, R: Rt, E: UserEvent>(
    view: &NodeView<'a, R, E>,
) -> Option<(&'a Node<R, E>, &'a Node<R, E>)> {
    use NodeView as V;
    Some(match view {
        V::Add(n) => (&n.lhs, &n.rhs),
        V::Sub(n) => (&n.lhs, &n.rhs),
        V::Mul(n) => (&n.lhs, &n.rhs),
        V::Div(n) => (&n.lhs, &n.rhs),
        V::Mod(n) => (&n.lhs, &n.rhs),
        V::CheckedAdd(n) => (&n.lhs, &n.rhs),
        V::CheckedSub(n) => (&n.lhs, &n.rhs),
        V::CheckedMul(n) => (&n.lhs, &n.rhs),
        V::CheckedDiv(n) => (&n.lhs, &n.rhs),
        V::CheckedMod(n) => (&n.lhs, &n.rhs),
        V::Eq(n) => (&n.lhs, &n.rhs),
        V::Ne(n) => (&n.lhs, &n.rhs),
        V::Lt(n) => (&n.lhs, &n.rhs),
        V::Gt(n) => (&n.lhs, &n.rhs),
        V::Lte(n) => (&n.lhs, &n.rhs),
        V::Gte(n) => (&n.lhs, &n.rhs),
        V::And(n) => (&n.lhs, &n.rhs),
        V::Or(n) => (&n.lhs, &n.rhs),
        _ => return None,
    })
}

/// The children of `node`, the one child step every walk takes, in a
/// deterministic order. A kernel has none: its feeders are outside its
/// region.
pub(crate) fn for_each_child<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    use NodeView as V;
    let view = node.view();
    if let Some((l, r)) = binary_operands(&view) {
        f(l);
        return f(r);
    }
    match view {
        V::Bind(b) => f(&b.node),
        V::MapQ(m) => [&m.source, &m.prototype].into_iter().for_each(f),
        V::FoldQ(m) => [&m.source, &m.init, &m.prototype].into_iter().for_each(f),
        V::Module(m) => m.source().into_iter().chain(m.nodes.iter()).for_each(f),
        V::Block(b) => b.children.iter().for_each(f),
        V::CallSite(cs) => {
            cs.args.values().filter_map(|a| a.node.as_ref()).for_each(&mut *f);
            f(&cs.fnode)
        }
        V::Select(s) => {
            f(&s.arg.node);
            for (pat, body) in s.arms.iter() {
                if let Some(g) = &pat.guard {
                    f(&g.node)
                }
                f(body)
            }
        }
        V::Catch(c) => {
            f(&c.handler);
            if let Some(abort) = &c.action {
                f(&abort.node);
                abort.manual().into_iter().for_each(f);
            }
        }
        V::Qop(q) => f(&q.n),
        V::SeqGuard(g) => f(&g.n),
        V::SeqAbort(a) => f(&a.n),
        V::SeqCapture(c) => [&c.snapshot, &c.live].into_iter().for_each(f),
        V::SeqMachine(m) => {
            f(&m.pc);
            m.steps.iter().flat_map(|s| s.nodes.iter()).for_each(f)
        }
        V::OrNever(o) => f(&o.n),
        V::ExplicitParens(p) => f(&p.n),
        V::ForkControl(p) => f(&p.n),
        V::TypeCast(t) => f(&t.n),
        V::Not(n) => f(&n.n),
        V::Neg(n) => f(&n.n),
        V::Connect(c) => f(&c.node),
        V::ConnectDeref(c) => f(&c.rhs),
        V::StringInterpolate(s) => s.args.iter().for_each(f),
        V::Any(a) => a.n.iter().for_each(f),
        V::Never(a) => a.n.iter().for_each(f),
        V::Sample(s) => [&s.trigger, &s.arg.node].into_iter().for_each(f),
        V::Struct(s) => s.n.iter().for_each(f),
        V::StructWith(s) => {
            f(&s.source);
            s.replace.iter().for_each(|r| f(&r.n))
        }
        V::Tuple(t) => t.n.iter().for_each(f),
        V::Variant(v) => v.n.iter().for_each(f),
        V::Construct(c) => f(&c.arg),
        V::Array(a) => a.n.iter().for_each(f),
        V::ListLit(a) => a.n.iter().for_each(f),
        V::Map(m) => m.n.iter().for_each(f),
        V::StructRef(s) => f(&s.source),
        V::TupleRef(t) => f(&t.source),
        V::ArrayRef(a) => [&a.source, &a.i].into_iter().for_each(f),
        V::ArraySlice(a) => {
            f(&a.source);
            a.start.iter().chain(a.end.iter()).for_each(f)
        }
        V::MapRef(m) => [&m.source, &m.key].into_iter().for_each(f),
        V::ByRef(b) => b.for_each_child(f),
        V::Deref(d) => f(&d.child),
        V::Impl(i) => {
            f(&i.body);
            i.prototypes.iter().for_each(|p| f(&p.site))
        }
        V::Lambda(_)
        | V::Ref(_)
        | V::Constant(_)
        | V::TypeDef(_)
        | V::Nop(_)
        | V::FusedKernel(_) => (),
        V::Add(_)
        | V::Sub(_)
        | V::Mul(_)
        | V::Div(_)
        | V::Mod(_)
        | V::CheckedAdd(_)
        | V::CheckedSub(_)
        | V::CheckedMul(_)
        | V::CheckedDiv(_)
        | V::CheckedMod(_)
        | V::Eq(_)
        | V::Ne(_)
        | V::Lt(_)
        | V::Gt(_)
        | V::Lte(_)
        | V::Gte(_)
        | V::And(_)
        | V::Or(_) => unreachable!("binary operands"),
    }
}

pub(crate) fn for_each_emitted_node<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    let mut stack: LPooled<Vec<&'a Node<R, E>>> = LPooled::take();
    stack.push(node);
    while let Some(node) = stack.pop() {
        let mut inline: LPooled<Vec<&'a Node<R, E>>> = LPooled::take();
        for_each_node(node, &mut |node| {
            f(node);
            let NodeView::CallSite(callsite) = node.view() else { return };
            let Some(ApplyView::Lambda(lambda)) = callsite.resolved_apply() else {
                return;
            };
            if let Some(body) = lambda.inline_callback_body() {
                inline.push(body);
            }
        });
        stack.extend(inline.drain(..));
    }
}

/// Every node a kernel built from `node` would run: the emitted nodes
/// and, through each statically-resolved lambda call, the callee's
/// body, each body once.
pub(crate) fn for_each_reachable_node<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    let mut seen: LPooled<nohash::IntSet<usize>> = LPooled::take();
    let mut stack: LPooled<Vec<&'a Node<R, E>>> = LPooled::take();
    stack.push(node);
    while let Some(body) = stack.pop() {
        if !seen.insert(body as *const Node<R, E> as usize) {
            continue;
        }
        let mut callees: LPooled<Vec<&'a Node<R, E>>> = LPooled::take();
        for_each_emitted_node(body, &mut |n| {
            f(n);
            let NodeView::CallSite(cs) = n.view() else { return };
            if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
                callees.push(g.body());
            }
        });
        stack.extend(callees.drain(..));
    }
}

/// The callee of one statically-resolved lambda call site in a region
/// being compiled, recorded by [`discover_lambda_calls`] and consumed by
/// `CallSite::emit_clif` to emit a CLIF `call` against it: the cached
/// kernel every site reaching it shares.
pub type LambdaCallInfo = triomphe::Arc<lowering::CachedKernel>;

/// A discovered callee's body Node + self-call info. The body reference
/// is live through this region's resolved `GXLambda` for the duration
/// of `try_fuse`.
pub struct CalleeBody<'n, R: Rt, E: UserEvent> {
    pub body: &'n Node<R, E>,
    /// `Some((self_bind, info))` for a self-recursive callee: non-tail
    /// self-calls emit through the lambda-call path against the
    /// kernel's own FuncRef; tail-position ones become the rebind loop.
    pub self_call: Option<(BindId, LambdaCallInfo)>,
    /// This callee body's own statically-resolved lambda call sites;
    /// empty for a leaf callee.
    pub sites: LPooled<nohash::IntMap<ExprId, LambdaCallInfo>>,
    /// This callee body's own builtin/cast/qop Apply sites.
    pub apply_sites: nohash::IntMap<ExprId, lowering::BuiltinCallSiteInfo>,
    /// This body's formal ids for the kernel's, where the kernel was
    /// cached from another instance of the definition: old to new.
    pub params: SmallVec<[(BindId, BindId); 4]>,
}

/// What [`discover_lambda_calls`] found in a region.
pub(crate) struct Discovery<'n, R: Rt, E: UserEvent> {
    /// The root's call sites.
    pub(crate) sites: LPooled<nohash::IntMap<ExprId, LambdaCallInfo>>,
    /// Every callee in the closure, in discovery order, by kernel
    /// identity: fn indices and the region layout are stable across
    /// processes.
    pub(crate) callees: LPooled<Vec<(kernel_abi::KernelKey, Arc<KernelSig>)>>,
    /// Each callee's body, self-call info and own call sites, by kernel
    /// identity.
    pub(crate) bodies: BTreeMap<kernel_abi::KernelKey, CalleeBody<'n, R, E>>,
    /// The decorated nodes a successful build absorbs.
    pub(crate) decorated: LPooled<Vec<&'n Node<R, E>>>,
    /// Call sites whose lambda has no kernel, with the reason.
    pub(crate) refused: LPooled<Vec<(&'n Expr, CompactString)>>,
}

/// Walk the region collecting every statically-resolved lambda call
/// site, building (or cache-hitting) each callee's kernel signature
/// transitively: a built callee's own body is scanned in turn. A lambda
/// that fails to build is not recorded; its call site bails at emission
/// and the region de-fuses (never a partial kernel). A callee already
/// in `bodies` is not re-scanned, which closes self- and mutual
/// recursion.
pub(crate) fn discover_lambda_calls<'n, R: Rt, E: UserEvent>(
    root: &'n Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> Discovery<'n, R, E> {
    let collect_decorated = !ctx.attr_census.lock().is_empty();
    let mut d = Discovery {
        sites: LPooled::take(),
        callees: LPooled::take(),
        bodies: BTreeMap::new(),
        decorated: LPooled::take(),
        refused: LPooled::take(),
    };
    // The second field says where a body's discovered sites land:
    // `None` = the root, `Some(ptr)` = that callee's `CalleeBody.sites`.
    let mut worklist: LPooled<Vec<(&'n Node<R, E>, Option<kernel_abi::KernelKey>)>> =
        LPooled::take();
    worklist.push((root, None));
    while let Some((body, target)) = worklist.pop() {
        let mut local_sites: LPooled<nohash::IntMap<ExprId, LambdaCallInfo>> =
            LPooled::take();
        let mut enqueue: LPooled<Vec<(&'n Node<R, E>, kernel_abi::KernelKey)>> =
            LPooled::take();
        for_each_emitted_node(body, &mut |n| {
            if collect_decorated
                && n.spec().dec.as_ref().is_some_and(|dec| !dec.attrs.is_empty())
            {
                d.decorated.push(n);
            }
            let NodeView::CallSite(cs) = n.view() else {
                return;
            };
            let Some(ApplyView::Lambda(g)) = cs.resolved_apply() else {
                return;
            };
            // A collection-bodied lambda is emitted inline at its call site.
            if matches!(g.body().view(), NodeView::MapQ(_) | NodeView::FoldQ(_)) {
                return;
            }
            // The source name labels the emitted symbol only; resolution
            // is by kernel identity.
            let name: ArcStr = match &cs.fnode.spec().kind {
                ExprKind::Ref { name } => match lowering::ident_of(name) {
                    Some(ident) => ArcStr::from(ident),
                    None => ArcStr::from(AsRef::<str>::as_ref(&name.0)),
                },
                _ => literal!("lambda"),
            };
            // The site's resolved FnType keys the kernel cache.
            let Some(site_ftype) = cs.resolved_ftype() else {
                return;
            };
            let cached = match lowering::build_lambda_kernel(g, site_ftype, &name, ctx) {
                Ok(cached) => cached,
                Err(why) => {
                    let reason = format_compact!("lambda `{name}` has no kernel: {why}");
                    d.refused.push((n.spec(), reason));
                    return;
                }
            };
            let ptr = kernel_abi::kernel_key(&cached.kernel);
            // A repeat reach (a self-call or mutual back-edge) records
            // the site but does not re-enqueue the body.
            if !d.bodies.contains_key(&ptr) {
                d.callees.push((ptr, cached.kernel.clone()));
                d.bodies.insert(
                    ptr,
                    CalleeBody {
                        body: g.body(),
                        self_call: cached.self_call.map(|sb| (sb, cached.clone())),
                        sites: LPooled::take(),
                        apply_sites: cached.apply_sites.clone(),
                        params: lowering::rebound_formals(g, &cached.kernel),
                    },
                );
                enqueue.push((g.body(), ptr));
            }
            local_sites.insert(n.spec().id, cached);
        });
        match target {
            None => d.sites = local_sites,
            Some(ptr) => {
                if let Some(cb) = d.bodies.get_mut(&ptr) {
                    cb.sites = local_sites;
                }
            }
        }
        worklist.extend(enqueue.drain(..).map(|(body, ptr)| (body, Some(ptr))));
    }
    d
}

/// The fusion visit protocol for one `Node`: try to fuse the whole
/// subtree via [`try_fuse`]; otherwise recurse into the node's own
/// `fuse`. The replacement is swapped in here because a node cannot
/// replace itself behind `&mut self`. Top-down order gives maximality:
/// the highest subtree that fuses is spliced and nothing below it is
/// attempted. `FusionDisabled` is checked once in [`crate::compile`].
pub fn fuse<R: Rt, E: UserEvent>(
    child: &mut Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
) -> anyhow::Result<()> {
    if let Some(new) = try_fuse(child, ctx)? {
        ctx.discard(std::mem::replace(child, new));
        check_node_attributes(child, ctx)?;
        return Ok(());
    }
    if let Some(new) = try_fuse_feeding_args(child, ctx)? {
        ctx.discard(std::mem::replace(child, new));
        check_node_attributes(child, ctx)?;
        return Ok(());
    }
    descend(child, ctx)
}

/// The scope of the bindings that name a fed argument: no source can
/// name it.
static FED_ARGS: LazyLock<ModPath> = LazyLock::new(|| ModPath::from(["#fed"]));

/// A lambda call whose arguments do not all fuse (an effect, a stateful
/// builtin) fuses with each such argument as a feeder: the node-walk
/// runs it and the kernel reads its production as an input, as it would
/// read a `let` bound to the argument.
fn try_fuse_feeding_args<R: Rt, E: UserEvent>(
    child: &mut Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
) -> anyhow::Result<Option<Node<R, E>>> {
    let top_id = ctx.fusion.top_id.unwrap_or(child.spec().id);
    let Some(cs) = child.downcast_mut::<CallNode<R, E>>().and_then(CallNode::call_mut)
    else {
        return Ok(None);
    };
    if !matches!(cs.resolved_apply(), Some(ApplyView::Lambda(_))) {
        return Ok(None);
    }
    // A feeder runs before the kernel, so a fed argument's raise would
    // reach its handler ahead of a fused earlier argument's: then nothing
    // is fed.
    let raises = |n: &Node<R, E>| {
        let mut any = false;
        for_each_reachable_node(n, &mut |n| {
            any |= matches!(n.view(), NodeView::Qop(q) if q.handler.is_some())
        });
        any
    };
    let mut feeds: SmallVec<[bool; 8]> = SmallVec::new();
    let mut fused_raise = false;
    for arg in cs.args.values_mut() {
        let Some(node) = arg.node.as_mut() else {
            feeds.push(false);
            continue;
        };
        let mut discovery = lowering::BuiltinCallDiscovery::default();
        let feed =
            lowering::walk_node_for_builtin_calls(node, ctx, &mut discovery).is_err();
        match (feed, raises(node)) {
            (true, true) if fused_raise => return Ok(None),
            (false, true) => fused_raise = true,
            _ => (),
        }
        feeds.push(feed);
    }
    let mut fed: LPooled<Vec<(BindId, Node<R, E>)>> = LPooled::take();
    for (arg, feed) in cs.args.values_mut().zip(feeds) {
        let Some(node) = arg.node.as_mut().filter(|_| feed) else { continue };
        let typ = node.typ().clone();
        let (id, read) = genn::bind(ctx, &FED_ARGS, "arg", typ, top_id);
        if free_var_input(id, ctx).is_none() {
            ctx.env.unbind_variable(id);
            ctx.discard(read);
            continue;
        }
        fed.push((id, std::mem::replace(node, read)));
    }
    if fed.is_empty() {
        return Ok(None);
    }
    // the bindings name the inputs for the attempt alone: a kernel's
    // feeder replaces its read
    let ids: SmallVec<[BindId; 8]> = fed.iter().map(|(id, _)| *id).collect();
    let r = fuse_fed(child, ctx, &mut fed);
    for id in ids {
        ctx.env.unbind_variable(id);
    }
    r
}

fn fuse_fed<R: Rt, E: UserEvent>(
    child: &mut Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
    fed: &mut Vec<(BindId, Node<R, E>)>,
) -> anyhow::Result<Option<Node<R, E>>> {
    // Every fed argument must be an input: an argument the kernel skips
    // would drop its effect.
    let kernel = match try_fuse(child, ctx)? {
        Some(mut new) => {
            let k = new
                .downcast_mut::<FusedKernel<R, E>>()
                .expect("try_fuse builds a kernel");
            if fed.iter().all(|(id, _)| k.has_input(*id)) {
                Some(new)
            } else {
                ctx.discard(new);
                None
            }
        }
        None => None,
    };
    match kernel {
        Some(mut new) => {
            let kernel = new.downcast_mut::<FusedKernel<R, E>>().expect("checked above");
            for (id, mut node) in fed.drain(..) {
                fuse_parts([&mut node], ctx)?;
                kernel.feed(ctx, id, node);
            }
            Ok(Some(new))
        }
        None => {
            let cs = child
                .downcast_mut::<CallNode<R, E>>()
                .and_then(CallNode::call_mut)
                .expect("still the call");
            for arg in cs.args.values_mut() {
                let Some(node) = arg.node.as_mut() else { continue };
                let NodeView::Ref(r) = node.view() else { continue };
                let id = r.id;
                if let Some(i) = fed.iter().position(|(fid, _)| *fid == id) {
                    let (_, orig) = fed.swap_remove(i);
                    ctx.discard(std::mem::replace(node, orig));
                }
            }
            Ok(None)
        }
    }
}

fn descend<R: Rt, E: UserEvent>(
    child: &mut Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
) -> anyhow::Result<()> {
    if let Some(new) = child.fuse(ctx)? {
        ctx.discard(std::mem::replace(child, new));
    }
    check_node_attributes(child, ctx)
}

/// Fuse the parts of a node that did not fuse as a whole: a part that
/// calls a function is tried as a region of its own ([`fuse`]), any
/// other part only descends (arithmetic over a leaf is not worth a
/// kernel). The `fuse` of every node with children that is not a
/// container ends here.
pub(crate) fn fuse_parts<'a, R: Rt + 'a, E: UserEvent + 'a>(
    parts: impl IntoIterator<Item = &'a mut Node<R, E>>,
    ctx: &mut CompileCtx<R, E>,
) -> anyhow::Result<Option<Node<R, E>>> {
    fuse_each(ctx, parts, |part, ctx| {
        if calls_a_function(part) { fuse(part, ctx) } else { descend(part, ctx) }
    })?;
    Ok(None)
}

/// Fuse each of `parts` through `visit`, in compile tasks when there are
/// several: the parts are disjoint subtrees, each fused against its own
/// fork of the context, the forks joined in order and the first error
/// in order the result. A collection's prototype or slot walk matches
/// its kernels by attempt order, so it runs in order.
pub(crate) fn fuse_each<'a, R: Rt + 'a, E: UserEvent + 'a>(
    ctx: &mut CompileCtx<R, E>,
    parts: impl IntoIterator<Item = &'a mut Node<R, E>>,
    visit: fn(&mut Node<R, E>, &mut CompileCtx<R, E>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    let mut parts: LPooled<Vec<&'a mut Node<R, E>>> = parts.into_iter().collect();
    if parts.len() < 2
        || ctx.fusion.share.is_some()
        || crate::dbgenv::graphix_fuse_serial()
    {
        return parts.drain(..).try_for_each(|p| visit(p, ctx));
    }
    // a part that calls no function is not worth a task; the error is the
    // serial walk's, the first in order
    let (mut heavy, mut light): (LPooled<Vec<_>>, LPooled<Vec<_>>) =
        parts.drain(..).enumerate().partition(|(_, p)| calls_a_function(p));
    let light_err =
        light.drain(..).find_map(|(i, p)| visit(p, ctx).err().map(|e| (i, e)));
    if let Some((i, _)) = &light_err {
        heavy.retain(|(h, _)| h < i);
    }
    let first = |heavy_err: Option<(usize, anyhow::Error)>| match [light_err, heavy_err]
        .into_iter()
        .flatten()
        .min_by_key(|(i, _)| *i)
    {
        Some((_, e)) => Err(e),
        None => Ok(()),
    };
    if heavy.len() < 2 {
        return first(
            heavy.drain(..).find_map(|(i, p)| visit(p, ctx).err().map(|e| (i, e))),
        );
    }
    let mut parts = heavy;
    // fusion records no attribute, and whether there are any decides
    // what discovery collects
    let census = ctx.attr_census.lock().clone();
    let memo = TypeMemo::current();
    let mut emission = ctx.fusion.emission()?;
    let mut work: LPooled<Vec<(usize, &mut Node<R, E>, CompileCtx<R, E>)>> = parts
        .drain(..)
        .map(|(i, p)| {
            let task = ctx.fork();
            *task.attr_census.lock() = census.clone();
            *task.fusion.emission.lock() = Some(emission.fork());
            (i, p, task)
        })
        .collect();
    drop(emission);
    let mut results: Vec<(usize, anyhow::Result<()>)> = work
        .par_iter_mut()
        .map(|(i, n, task)| {
            (*i, task.run_task(|task| TypeMemo::enter(memo.clone(), || visit(n, task))))
        })
        .collect();
    for (_, _, task) in work.drain(..) {
        task.attr_census.lock().clear();
        ctx.join(task);
    }
    ctx.fusion.emission()?.thaw();
    if ctx.fusion.unlinked() >= LINK_BATCH {
        ctx.fusion.link_batch();
    }
    first(results.drain(..).find_map(|(i, r)| r.err().map(|e| (i, e))))
}

/// Does the subtree call a lambda or run a collection operation, the
/// computations that can loop?
fn calls_a_function<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut calls = false;
    for_each_node(node, &mut |n| {
        calls |= match n.view() {
            NodeView::CallSite(cs) => {
                matches!(cs.resolved_apply(), Some(ApplyView::Lambda(_)))
            }
            NodeView::MapQ(_) | NodeView::FoldQ(_) => true,
            _ => false,
        }
    });
    calls
}

/// Dispatch each registered attribute's check ([`crate::AttributeCheckFn`])
/// on a node the fusion walk just resolved. A fused node was replaced by
/// its [`FusedKernel`], which carries the region root's spec, so
/// `#[native]` passes; a node absorbed into a larger kernel is never
/// visited, which is also a pass. Definition assertions
/// (`#[tail_recursive]`/`#[sync]`/`#[async]`) verify in `analysis::analyze`.
fn check_node_attributes<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> anyhow::Result<()> {
    if ctx.fusion.reusing() {
        return Ok(());
    }
    if let Some(dec) = &node.spec().dec {
        let mut any = false;
        for attr in dec.attrs.iter() {
            if let Some(check) = ctx.lookup_attribute(&attr.name) {
                any = true;
                check(ctx, attr, node)?;
            }
        }
        if any {
            ctx.attr_dispatched.lock().insert(node.spec().id);
        }
    }
    Ok(())
}

/// The target half of the attribute checks on a node absorbed into a
/// kernel ([`crate::AttributeTargetFn`]).
fn check_attribute_targets<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
) -> anyhow::Result<()> {
    if let Some(dec) = &node.spec().dec {
        for attr in dec.attrs.iter() {
            if let Some(check) = ctx.lookup_attribute_target(&attr.name) {
                check(attr, node)?;
            }
        }
    }
    Ok(())
}

/// Try to fuse the whole subtree rooted at `node` into one JIT kernel.
/// Mechanics only; policy lives in each node's [`Update::fuse`].
///
/// `Ok(Some(replacement))`: a [`FusedKernel`] node the caller swaps in.
/// `Ok(None)`: the root type has no kernel representation, the subtree
/// is an identity passthrough, or some node does not emit CLIF.
/// Discovery rejects known effects; emission validates the remaining shapes.
// CR claude for eric: [perf] A pure subtree of any size becomes one CLIF function.
// Cranelift's backtracking register allocator (regalloc2, reached from Jit::link) is
// superlinear in function size, so cold-start compile time grows roughly quadratically
// with region size while the node-walk stays linear. Debug build, fused vs --no-fusion:
// a block of 5000 chained lets, fused as one region, takes 11.3 s vs 1.0 s (2500: 2.9 s
// vs 0.7 s); a 5000-arm select takes 5.3 s vs 1.0 s; a lambda whose body is a 9000-deep
// `+` chain takes 44 s vs 0.7 s. Nothing bounds or splits a region. Splitting an
// oversized region at its parts would keep it native. (x-stack-10)
// 2026-10-09 claude: Needs a ruling: splitting trades against #[native]. The simple fix
// refuses a region past a node budget, so fuse() descends to its parts, which fuse
// separately and keep compile time linear; but the glue between the parts node-walks, so
// #[native] over an expression that large fails, and fusion stops being predictable from
// the source alone. Options: (a) split past a budget, and let #[native] mean 'fuses,
// possibly as several kernels'; (b) split only where no #[native] covers the region; (c)
// keep one region and bound regalloc instead (cranelift's single-pass allocator for
// oversized functions), which keeps the semantics and costs code quality there. I lean
// (c), then (b).
pub fn try_fuse<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
) -> anyhow::Result<Option<Node<R, E>>> {
    if !region_is_candidate(node) {
        return Ok(None);
    }
    let phase = profile::phase(Phase::ReturnType);
    let Some(return_type) = freeze_region_return(node.typ(), &ctx.env) else {
        if crate::dbgenv::gxdbg_freeze_ret() {
            format_with_flags(PrintFlag::DerefTVars, || {
                eprintln!("FREEZE-RET-MISS {:?} typ={}", node.spec().id, node.typ());
                Ok::<_, std::fmt::Error>(())
            })
            .ok();
        }
        return Ok(None);
    };
    drop(phase);
    if ctx.fusion.reusing() {
        return Ok(share::reuse(ctx, node, &return_type));
    }
    let mut fused = build_region(node, ctx, return_type)?;
    share::record(ctx, node, fused.as_mut());
    Ok(fused)
}

fn build_region<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
    return_type: Type,
) -> anyhow::Result<Option<Node<R, E>>> {
    ctx.fusion.stats.attempted += 1;
    let phase = profile::phase(Phase::Builtins);
    // `apply_sites` lets `CallSite::emit_clif` lower a registered site
    // to a direct call.
    let mut discovery = lowering::BuiltinCallDiscovery::default();
    if let Err(blocker) = lowering::walk_node_for_builtin_calls(node, ctx, &mut discovery)
    {
        ctx.fusion.stats.rejected_before_emit += 1;
        return refuse(ctx, &blocker.spec, &blocker.reason);
    }
    drop(phase);
    let phase = profile::phase(Phase::Inputs);
    let inputs = collect_region_inputs(&**node, ctx);
    drop(phase);
    let phase = profile::phase(Phase::Callees);
    ctx.fusion.emission()?.attempt();
    let lambdas = discover_lambda_calls(node, ctx);
    drop(phase);
    let source_id = node.spec().id;
    let kernel = Arc::new(sig_from_params(
        ArcStr::from(format_compact!("region_{:?}", source_id).as_str()),
        inputs.iter().map(|fv| KernelParam {
            name: fv.name.clone(),
            kind: fv.kind.clone(),
            bind_id: Some(fv.bind_id),
        }),
        return_type,
    ));
    let phase = profile::phase(Phase::Emit);
    let result = ctx.fusion.emission().and_then(|mut em| {
        emit::compile_kernel_with_callees_direct(
            &mut em,
            &kernel,
            &lambdas.callees,
            node,
            &discovery.apply_sites,
            &lambdas.sites,
            &lambdas.bodies,
            &ctx.env,
        )
    });
    drop(phase);
    let wrapped = match result {
        Ok(w) => w,
        Err(e) => {
            ctx.fusion.emission()?.forget_attempt();
            log::trace!("fusion::try_fuse: region {source_id:?} doesn't fuse: {e:#}");
            for (spec, why) in lambdas.refused.iter() {
                ctx.fusion.stats.record_failure(spec, why);
            }
            // the region's own record carries the causes: a refused callee
            // or a blocker may sit in a callee's body, outside the region
            let mut why = format_compact!("{e:#}");
            for (_, refused) in lambdas.refused.iter() {
                why.push_str("; ");
                why.push_str(refused);
            }
            let blocker = e
                .chain()
                .find_map(|cause| cause.downcast_ref::<FusionBlocker>())
                .map(|blocker| &blocker.spec)
                .filter(|spec| {
                    let mut inside = false;
                    for_each_node(node, &mut |n| inside |= n.spec().id == spec.id);
                    inside
                });
            return refuse(ctx, blocker.unwrap_or_else(|| node.spec()), &why);
        }
    };
    // Feeders register under the real top id: `Rt::ref_var` is keyed
    // `(BindId, top_id)`, and the region's interior id would strand the
    // top expression at ref count zero.
    let feeder_top = ctx.fusion.top_id.unwrap_or(source_id);
    let feeders: Box<[Node<R, E>]> = inputs
        .iter()
        .map(|fv| genn::reference::<R, E>(ctx, fv.bind_id, fv.typ.clone(), feeder_top))
        .collect();
    let n = FusedKernel::new(
        node.spec().clone(),
        node.typ().clone(),
        crate::analysis::region_facts(node, ctx),
        kernel,
        wrapped,
        feeders,
    );
    log::debug!(
        "fusion::try_fuse: fused region {source_id:?} with {} input(s)",
        inputs.len()
    );
    if crate::dbgenv::graphix_dbg_region() {
        for (i, fv) in inputs.iter().enumerate() {
            let deref = format_with_flags(PrintFlag::DerefTVars, || {
                format_compact!("{}", fv.typ)
            });
            let cons = match &fv.typ {
                crate::typ::Type::TVar(tv) => {
                    let cs = tv.cell_constraints();
                    format_compact!("{cs:?}")
                }
                _ => format_compact!("-"),
            };
            eprintln!(
                "DBGREGION {source_id:?} input[{i}] name={} bind={:?} \
                 typ={} deref={deref} cons={cons} kind={:?}",
                fv.name, fv.bind_id, fv.typ, fv.kind
            );
        }
    }
    ctx.fusion.stats.record_fused(node.spec());
    if ctx.fusion.unlinked() >= LINK_BATCH {
        ctx.fusion.link_batch();
    }
    for n in lambdas.decorated.iter() {
        check_attribute_targets(n, ctx)?;
    }
    ctx.attr_absorbed.lock().extend(lambdas.decorated.iter().map(|n| n.spec().id));
    Ok(Some(n))
}

/// De-fuse the region, recording the reason so `attempted` and
/// `failed` agree.
fn refuse<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    spec: &Expr,
    reason: &str,
) -> anyhow::Result<Option<Node<R, E>>> {
    ctx.fusion.stats.record_failure(spec, reason);
    Ok(None)
}

/// Freeze a region root's type into the kernel's return ABI type, or
/// `None` if the region can't fuse: bare `Null` and `Unit` have no
/// kernel representation. Normalized because a select-rooted region's
/// type is the raw arm union; on failure, retry with refs expanded,
/// since an abstract-typed return carries Refs the env-free freeze rejects.
pub(crate) fn freeze_region_return(typ: &Type, env: &Env) -> Option<Type> {
    if crate::dbgenv::graphix_dbg_freeze() {
        let d = format_with_flags(PrintFlag::DerefTVars, || format_compact!("{typ}"));
        eprintln!(
            "DBGFREEZE typ={typ} deref={d} resolved={:?}",
            typ.resolve_tvars().normalize()
        );
    }
    let return_type = match try_freeze_for_abi_normalized(typ) {
        Ok(t) => t,
        Err(FreezeError::Unsupported) => return None,
        Err(FreezeError::NonCanonical | FreezeError::Unresolved) => {
            let resolved = expand_refs(typ, env);
            freeze_for_abi_normalized(&resolved)?
        }
    };
    match kernel_abi::abi_kind(&return_type) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::String,
        ) => Some(return_type),
        Some(AbiKind::Unit | AbiKind::Null) | None => None,
    }
}

/// Why a node can never be inside a kernel: an effect or a declaration.
/// Block emission keeps every statement containing one, and discovery
/// rejects a region containing one before collecting inputs.
pub(crate) fn effect_blocker<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
) -> Option<&'static str> {
    match node.view() {
        NodeView::Connect(_) | NodeView::ConnectDeref(_) => Some("connect is an effect"),
        NodeView::Catch(_) => Some("catch installs an error handler"),
        NodeView::SeqGuard(_) => Some("sequence guard keeps cross-cycle state"),
        NodeView::SeqAbort(_) => Some("sequence abort fails a run"),
        NodeView::SeqMachine(_) => Some("a seq machine sequences across cycles"),
        NodeView::SeqCapture(_) => Some("a seqq capture is chosen by the analysis"),
        NodeView::ForkControl(_) => {
            Some("fork control runs its child under flags of its own")
        }
        NodeView::Module(_) => Some("module statement is structure, not computation"),
        NodeView::Block(b) if b.module => {
            Some("module statement is structure, not computation")
        }
        NodeView::Impl(_) => Some("impl statement is structure, not computation"),
        _ => None,
    }
}

/// A region root must emit a value. Declarations stay in the graph;
/// a bare binding read forwards an input without computation.
pub(crate) fn region_is_candidate<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_)
            | NodeView::Bind(_)
            | NodeView::Lambda(_)
            | NodeView::Module(_)
            | NodeView::Impl(_)
            | NodeView::TypeDef(_)
            | NodeView::Nop(_)
            | NodeView::FusedKernel(_) => return false,
            NodeView::Block(b) => return !b.module,
            NodeView::ExplicitParens(p) => n = &*p.n,
            _ => return true,
        }
    }
}

/// A [`KernelSig`] over `params` in source order, which is the ABI
/// order.
pub(crate) fn sig_from_params(
    fn_name: ArcStr,
    params: impl IntoIterator<Item = KernelParam>,
    return_type: Type,
) -> KernelSig {
    KernelSig {
        fn_name,
        params: params.into_iter().collect(),
        return_type,
        has_tail_loop: false,
        skipped_args: Vec::new(),
        tail_invariant: Vec::new(),
        site_block_words: std::sync::atomic::AtomicU64::new(0),
    }
}
