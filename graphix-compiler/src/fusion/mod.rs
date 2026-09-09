//! Fusion: JIT-compile pure subtrees of the compiled node graph to
//! native kernels.
//!
//! Code generation is distributed: each node's `Update::emit_clif` /
//! `Apply::emit_clif` emits its own CLIF and [`fuse`] drives the
//! `Update::fuse` recursion. This module supplies the shared mechanics:
//! [`try_fuse`] (early effect rejection and whole-subtree compilation),
//! [`fuse`] (the child-visit protocol),
//! [`lowering`] (discovery and signature derivation) and [`builder`]
//! (the runtime [`builder::FusedKernel`] carrier).

pub mod builder;
pub mod emit;
pub mod emit_helpers;
pub mod intern;
pub mod kernel;
pub mod kernel_abi;
pub mod lowering;

pub use builder::FusedKernel;

use crate::{
    ApplyView, BindId, ExecCtx, LambdaId, Node, NodeView, Refs, Rt, Update, UserEvent,
    env::Env,
    expr::{Expr, ExprId, ExprKind, Origin},
    fusion::{
        kernel_abi::{KernelSig, freeze_for_abi_normalized},
        lowering::{RegionInputKind, expand_refs},
    },
    node,
    node::genn,
    perfdbg,
    typ::{FnType, Type},
};
use poolshark::local::LPooled;

#[derive(Debug, Clone)]
struct FusionSource {
    origin: triomphe::Arc<Origin>,
    pos: combine::stream::position::SourcePosition,
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
    pub reason: compact_str::CompactString,
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
    reason: compact_str::CompactString,
}

impl std::fmt::Display for FusionBlocker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.reason.fmt(f)
    }
}

impl std::error::Error for FusionBlocker {}

pub(crate) fn blocker(spec: &Expr, reason: compact_str::CompactString) -> anyhow::Error {
    FusionBlocker { spec: spec.clone(), reason }.into()
}

/// Compile-time fusion outcome counters, accumulated on
/// [`FusionCtx::stats`] by every `compile()` the context runs. A
/// region that fails to compile node-walks and produces the correct
/// value, so these counters are the only way to ask "did it fuse, and
/// if not why".
#[derive(Debug, Clone, Default)]
pub struct FusionStats {
    /// `try_fuse` attempts that passed the identity and return-type gates.
    pub attempted: usize,
    /// Regions that compiled and were spliced in.
    pub fused: usize,
    /// Attempts rejected during discovery, before input collection or emission.
    pub rejected_before_emit: usize,
    /// Per-failure source identity and compile error. Compile-time only;
    /// bounded by program size.
    pub failed: Vec<FusionFailure>,
    /// JIT module rotations (see `FusionCtx::retired_jits`). Each leaves
    /// one ~256MB arena resident until the ExecCtx is dropped; embedders
    /// that recompile heavily can poll this and recycle the ExecCtx.
    pub jit_generations: usize,
    /// Region roots that fused. Distinguishes a structural `failed`
    /// entry (a block whose value fused in a sub-region) from a real
    /// blocker with nothing fused beneath it.
    fused_sources: Vec<FusionSource>,
}

impl FusionStats {
    fn record_failure(&mut self, spec: &Expr, reason: compact_str::CompactString) {
        self.failed.push(FusionFailure {
            id: spec.id,
            reason,
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

/// Per-[`ExecCtx`] state owned by the fusion subsystem, reached as
/// `ctx.fusion.<x>`.
pub struct FusionCtx {
    /// Per-context cranelift module + cross-kernel-call cache. The
    /// mutex is interior mutability for `ExecCtx`'s `Sync` bound; JIT
    /// ops are compile-time only.
    pub jit: parking_lot::Mutex<emit::Jit>,
    /// JIT generations retired when the active arena exhausted. Their
    /// kernels stay mapped and executing; generations never link (a
    /// region builds atomically within one). Freed by ExecCtx drop or
    /// [`Self::reset_jit_for_check`].
    pub retired_jits: parking_lot::Mutex<Vec<emit::Jit>>,
    /// Monomorphized lambda-kernel cache. Catch coverage and fn
    /// resolutions are part of the key because the kernel bakes them.
    /// The cached `Arc<KernelSig>` is the callable handle: the JIT's
    /// `by_kernel` cache keys on its pointer identity.
    pub kernels: parking_lot::Mutex<
        std::collections::BTreeMap<
            (
                LambdaId,
                std::sync::Arc<FnType>,
                lowering::QopCoverage,
                lowering::FnResolutions,
            ),
            lowering::CachedKernel,
        >,
    >,
    /// Lambdas whose kernel build is currently on the stack. A
    /// re-entrant build (mutual recursion) is refused so the chain
    /// de-fuses instead of recursing forever. `Arc` so a drop-guard can
    /// hold the set without borrowing the `ExecCtx`.
    pub(crate) building: triomphe::Arc<parking_lot::Mutex<nohash::IntSet<u64>>>,
    /// Whether fusion is enabled for the current compile; set by
    /// [`crate::compile`].
    pub enabled: bool,
    /// Compile-time fusion outcome counters, accumulated across every
    /// `compile()` this context runs. See [`FusionStats`].
    pub stats: FusionStats,
    /// The top expression id of the running compile. Feeder Refs must
    /// register under it: `Rt::ref_var` is keyed `(BindId, top_id)`, and
    /// a region's interior id would strand the top expression at count 0.
    pub(crate) top_id: Option<ExprId>,
    /// Declared facts of each registered builtin, keyed by name (from
    /// `T::EFFECT`). An absent builtin is treated as `Async` + stateful.
    pub builtin_facts: ahash::AHashMap<&'static str, crate::effects::BuiltinFacts>,
}

impl FusionCtx {
    pub fn new() -> anyhow::Result<Self> {
        Ok(Self {
            jit: parking_lot::Mutex::new(emit::Jit::new()?),
            retired_jits: parking_lot::Mutex::new(Vec::new()),
            kernels: parking_lot::Mutex::new(std::collections::BTreeMap::new()),
            building: triomphe::Arc::new(parking_lot::Mutex::new(
                nohash::IntSet::default(),
            )),
            enabled: true,
            stats: FusionStats::default(),
            top_id: None,
            builtin_facts: ahash::AHashMap::default(),
        })
    }

    /// Reset the JIT to an empty module, discarding every compiled
    /// kernel. The lambda-kernel signature cache survives: it holds
    /// module-independent descriptors that re-declare into the fresh
    /// module on next use.
    ///
    /// For the check/LSP path only, which never executes a kernel and
    /// would otherwise accumulate every checked file's kernels in one
    /// module. Must not be called on a runtime with live kernels: it
    /// frees their code.
    pub fn reset_jit_for_check(&self) -> anyhow::Result<()> {
        *self.jit.lock() = emit::Jit::new()?;
        self.retired_jits.lock().clear();
        Ok(())
    }
}

/// One free-var input slot resolved during walker analysis.
#[derive(Debug, Clone)]
pub(crate) struct FreeVarInput {
    pub(crate) bind_id: BindId,
    pub(crate) name: arcstr::ArcStr,
    /// Kernel-input classification, computed once from the binding's type.
    pub(crate) kind: RegionInputKind,
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
    ctx: &ExecCtx<R, E>,
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
    ctx: &ExecCtx<R, E>,
) -> Option<FreeVarInput> {
    let b = ctx.env.by_id.get(&id)?;
    // `freeze_for_abi` is env-free and rejects `Type::Ref`, so refs expand
    // first. The feeder keeps the unresolved type; only the slot
    // classification needs the concrete rep.
    let resolved = expand_refs(&b.typ, &ctx.env);
    // Normalized: a select with a never() arm leaves a Set polluted by
    // the arm's late-bound TVar.
    let frozen = kernel_abi::freeze_for_abi_normalized(&resolved)?;
    let kind = lowering::type_to_region_input_kind(frozen)?;
    Some(FreeVarInput {
        bind_id: id,
        name: arcstr::ArcStr::from(b.name.as_str()),
        kind,
        typ: b.typ.clone(),
    })
}

/// The single definition of "tail position". A body root is a tail
/// position; tailness propagates through a `Block`'s last child, an
/// `ExplicitParens`' inner node and every `Select` arm body, and stops
/// at a `Leaf`. The analysis walks and the kernel emitter must agree on
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
        NodeView::Block(b) => TailPosition::Block(b),
        NodeView::ExplicitParens(ep) => TailPosition::Parens(ep),
        NodeView::Select(s) => TailPosition::Select(s),
        _ => TailPosition::Leaf(node),
    }
}

/// Call `f` on each tail-position leaf of `node`; returns whether any
/// call returned true. Every Select arm is visited (no short-circuit),
/// and `on_select` fires for each Select on the tail spine with a true leaf.
pub(crate) fn for_each_tail_leaf<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    f: &mut impl FnMut(&Node<R, E>) -> bool,
    on_select: &mut impl FnMut(&node::select::Select<R, E>),
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
    macro_rules! rec {
        ($($n:expr),*) => {{ $(for_each_node::<R, E>($n, f);)* }};
    }
    match node.view() {
        NodeView::Bind(b) => rec!(&b.node),
        NodeView::MapQ(m) => rec!(&m.source, &m.prototype),
        NodeView::FoldQ(m) => rec!(&m.source, &m.init, &m.prototype),
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                rec!(child)
            }
        }
        NodeView::Block(blk) => {
            for child in blk.children.iter() {
                rec!(child)
            }
        }
        NodeView::CallSite(cs) => {
            // The args map is hash-ordered; walk in ArgKey order so the
            // downstream discovery order is deterministic.
            let mut args: LPooled<Vec<(&crate::node::callsite::ArgKey, &Node<R, E>)>> =
                cs.args
                    .iter()
                    .filter_map(|(k, a)| a.node.as_ref().map(|n| (k, n)))
                    .collect();
            args.sort_by(|(a, _), (b, _)| a.cmp(b));
            for (_, n) in args.drain(..) {
                rec!(n)
            }
            rec!(&cs.fnode)
        }
        NodeView::Select(s) => {
            rec!(&s.arg.node);
            for (pat, body) in s.arms.iter() {
                if let Some(g) = &pat.guard {
                    rec!(&g.node)
                }
                rec!(body)
            }
        }
        NodeView::Catch(c) => {
            rec!(&c.handler);
            if let Some(abort) = &c.seq_abort {
                rec!(&abort.node);
            }
        }
        NodeView::Qop(q) => rec!(&q.n),
        NodeView::SeqGuard(g) => rec!(&g.n),
        NodeView::OrNever(o) => rec!(&o.n),
        NodeView::ExplicitParens(p) => rec!(&p.n),
        NodeView::TypeCast(t) => rec!(&t.n),
        NodeView::Not(n) => rec!(&n.n),
        NodeView::Neg(n) => rec!(&n.n),
        NodeView::Connect(c) => rec!(&c.node),
        NodeView::ConnectDeref(c) => rec!(&c.rhs),
        NodeView::StringInterpolate(s) => {
            for a in s.args.iter() {
                rec!(a)
            }
        }
        NodeView::Any(a) => {
            for n in a.n.iter() {
                rec!(n)
            }
        }
        NodeView::Never(a) => {
            for n in a.n.iter() {
                rec!(n)
            }
        }
        NodeView::Sample(s) => rec!(&s.trigger, &s.arg.node),
        NodeView::Struct(s) => {
            for c in s.n.iter() {
                rec!(c)
            }
        }
        NodeView::StructWith(s) => {
            rec!(&s.source);
            for r in s.replace.iter() {
                rec!(&r.n)
            }
        }
        NodeView::Tuple(t) => {
            for c in t.n.iter() {
                rec!(c)
            }
        }
        NodeView::Variant(v) => {
            for c in v.n.iter() {
                rec!(c)
            }
        }
        NodeView::Construct(c) => rec!(&c.arg),
        NodeView::Array(a) => {
            for c in a.n.iter() {
                rec!(c)
            }
        }
        NodeView::ListLit(a) => {
            for c in a.n.iter() {
                rec!(c)
            }
        }
        NodeView::Map(m) => {
            for c in m.keys.iter() {
                rec!(c)
            }
            for c in m.vals.iter() {
                rec!(c)
            }
        }
        NodeView::StructRef(s) => rec!(&s.source),
        NodeView::TupleRef(t) => rec!(&t.source),
        NodeView::ArrayRef(a) => rec!(&a.source, &a.i),
        NodeView::ArraySlice(a) => {
            rec!(&a.source);
            if let Some(s) = &a.start {
                rec!(s)
            }
            if let Some(e) = &a.end {
                rec!(e)
            }
        }
        NodeView::MapRef(m) => rec!(&m.source, &m.key),
        NodeView::ByRef(b) => rec!(&b.child),
        NodeView::Deref(d) => rec!(&d.child),
        NodeView::Add(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Sub(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Mul(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Div(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Mod(o) => rec!(&o.lhs, &o.rhs),
        NodeView::CheckedAdd(o) => rec!(&o.lhs, &o.rhs),
        NodeView::CheckedSub(o) => rec!(&o.lhs, &o.rhs),
        NodeView::CheckedMul(o) => rec!(&o.lhs, &o.rhs),
        NodeView::CheckedDiv(o) => rec!(&o.lhs, &o.rhs),
        NodeView::CheckedMod(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Eq(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Ne(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Lt(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Gt(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Lte(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Gte(o) => rec!(&o.lhs, &o.rhs),
        NodeView::And(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Or(o) => rec!(&o.lhs, &o.rhs),
        NodeView::Lambda(_) => {}
        NodeView::Impl(i) => {
            rec!(&i.body);
            for p in i.prototypes.iter() {
                rec!(p)
            }
        }
        NodeView::Ref(_)
        | NodeView::Constant(_)
        | NodeView::TypeDef(_)
        | NodeView::Nop(_) => {}
        NodeView::FusedKernel(_) => {}
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

/// One statically-resolved lambda call site in a region being compiled,
/// recorded by [`discover_lambda_calls`] and consumed by
/// `CallSite::emit_clif` to emit a CLIF `call` against the callee.
#[derive(Debug, Clone)]
pub struct LambdaCallInfo {
    /// The callee kernel's name in the `funcids`/`callee_refs` maps —
    /// the cached kernel's name, never this call site's source name.
    pub fn_name: arcstr::ArcStr,
    /// The same `Arc` the `by_kernel` entry keys on.
    pub kernel: std::sync::Arc<KernelSig>,
    /// The callee's input types in signature order, formals then
    /// captures, resolved and frozen at build time — the caller's type
    /// authority for arg classification (env is unavailable at emit time).
    pub arg_types: Vec<Type>,
    /// Closure-converted captures, appended after the formal args; the
    /// caller marshals each from its own env, BindId-first.
    pub captures: Vec<lowering::CaptureSlot>,
}

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
}

/// Walk the region collecting every statically-resolved lambda call
/// site, building (or cache-hitting) each callee's kernel signature
/// transitively: a built callee's own body is scanned in turn. A lambda
/// that fails to build is not recorded; its call site bails at emission
/// and the region de-fuses (never a partial kernel).
///
/// Returns the root's call sites, every callee in the closure in
/// discovery order, each callee's body + self-call info + own call
/// sites keyed by kernel identity, and the decorated nodes the region
/// would absorb. A callee already in `bodies` is not re-scanned, which
/// closes self- and mutual recursion.
pub(crate) fn discover_lambda_calls<'n, R: Rt, E: UserEvent>(
    root: &'n Node<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> (
    LPooled<nohash::IntMap<ExprId, LambdaCallInfo>>,
    LPooled<Vec<(usize, std::sync::Arc<KernelSig>)>>,
    std::collections::BTreeMap<usize, CalleeBody<'n, R, E>>,
    LPooled<nohash::IntSet<ExprId>>,
) {
    // Decorated nodes seen by this walk are exactly the nodes a
    // successful build absorbs; `try_fuse` commits them to `attr_absorbed`.
    let collect_decorated = !ctx.attr_census.lock().is_empty();
    let mut decorated: LPooled<nohash::IntSet<ExprId>> = LPooled::take();
    // Keyed by kernel identity (names shadow, monomorphizations share a
    // name); a Vec in discovery order so fn indices and the region
    // layout are stable across processes.
    let mut callees: LPooled<Vec<(usize, std::sync::Arc<KernelSig>)>> = LPooled::take();
    let mut bodies: std::collections::BTreeMap<usize, CalleeBody<'n, R, E>> =
        std::collections::BTreeMap::new();
    // The second field says where a body's discovered sites land:
    // `None` = the root, `Some(ptr)` = that callee's `CalleeBody.sites`.
    let mut worklist: LPooled<Vec<(&'n Node<R, E>, Option<usize>)>> = LPooled::take();
    worklist.push((root, None));
    let mut root_sites: LPooled<nohash::IntMap<ExprId, LambdaCallInfo>> = LPooled::take();
    while let Some((body, target)) = worklist.pop() {
        let mut local_sites: LPooled<nohash::IntMap<ExprId, LambdaCallInfo>> =
            LPooled::take();
        let mut enqueue: LPooled<Vec<(&'n Node<R, E>, usize)>> = LPooled::take();
        for_each_emitted_node(body, &mut |n| {
            if collect_decorated
                && n.spec().dec.as_ref().is_some_and(|d| !d.attrs.is_empty())
            {
                decorated.insert(n.spec().id);
            }
            let NodeView::CallSite(cs) = n.view() else {
                return;
            };
            let Some(ApplyView::Lambda(g)) = cs.resolved_apply() else {
                return;
            };
            // The source name labels the emitted symbol only; resolution
            // is by kernel identity. A lambda-literal call has no name
            // and stays on the node-walk.
            let ExprKind::Ref { name } = &cs.fnode.spec().kind else {
                return;
            };
            let name: arcstr::ArcStr = match lowering::ident_of(name) {
                Some(ident) => arcstr::ArcStr::from(ident),
                None => {
                    let s: &str = name.0.as_ref();
                    arcstr::ArcStr::from(s)
                }
            };
            // The site's resolved FnType keys the kernel cache.
            let Some(site_ftype) = cs.resolved_ftype() else {
                return;
            };
            let Some(cached) = lowering::build_lambda_kernel(g, site_ftype, &name, ctx)
            else {
                return;
            };
            let ptr = kernel_abi::kernel_key(&cached.kernel);
            // A repeat reach (a self-call or mutual back-edge) records
            // the site but does not re-enqueue the body.
            if !bodies.contains_key(&ptr) {
                callees.push((ptr, cached.kernel.clone()));
                let self_call = cached.is_rec.then(|| {
                    (
                        cached.self_bind.expect(
                            "is_rec without self_bind — \
                             build_lambda_kernel derives is_rec FROM \
                             self_bind",
                        ),
                        LambdaCallInfo {
                            fn_name: cached.fn_name.clone(),
                            kernel: cached.kernel.clone(),
                            arg_types: cached.signature.arg_types.clone(),
                            captures: cached.captures.clone(),
                        },
                    )
                });
                bodies.insert(
                    ptr,
                    CalleeBody {
                        body: g.body(),
                        self_call,
                        sites: LPooled::take(),
                        apply_sites: cached.apply_sites.clone(),
                    },
                );
                enqueue.push((g.body(), ptr));
            }
            local_sites.insert(
                n.spec().id,
                LambdaCallInfo {
                    fn_name: cached.fn_name,
                    kernel: cached.kernel,
                    arg_types: cached.signature.arg_types.clone(),
                    captures: cached.captures,
                },
            );
        });
        match target {
            None => root_sites = local_sites,
            Some(ptr) => {
                if let Some(cb) = bodies.get_mut(&ptr) {
                    cb.sites = local_sites;
                }
            }
        }
        worklist.extend(enqueue.drain(..).map(|(body, ptr)| (body, Some(ptr))));
    }
    (root_sites, callees, bodies, decorated)
}

/// The fusion visit protocol for one `Node`: try to fuse the whole
/// subtree via [`try_fuse`]; otherwise recurse into the node's own
/// `fuse`. The replacement is swapped in here because a node cannot
/// replace itself behind `&mut self`. Top-down order gives maximality:
/// the highest subtree that fuses is spliced and nothing below it is
/// attempted. `FusionDisabled` is checked once in [`crate::compile`].
pub fn fuse<R: Rt, E: UserEvent>(
    child: &mut Node<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> anyhow::Result<()> {
    if let Some(new) = try_fuse(child, ctx)? {
        let mut old = std::mem::replace(child, new);
        old.delete(ctx);
        check_node_attributes(child, ctx)?;
        return Ok(());
    }
    if let Some(new) = child.fuse(ctx)? {
        let mut old = std::mem::replace(child, new);
        old.delete(ctx);
    }
    check_node_attributes(child, ctx)?;
    Ok(())
}

/// Dispatch each registered attribute's check ([`crate::AttributeCheckFn`])
/// on a node the fusion walk just resolved. A fused node was replaced by
/// its [`FusedKernel`], which carries the region root's spec, so
/// `#[native]` passes; a node absorbed into a larger kernel is never
/// visited, which is also a pass. Definition assertions
/// (`#[tail_recursive]`/`#[sync]`/`#[async]`) verify in `analysis::analyze`.
fn check_node_attributes<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> anyhow::Result<()> {
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

/// Attribute sweep over collection-intrinsic callback bodies, which the
/// fuse driver never descends (their fusion is the inline emission at
/// the enclosing call). Descends through resolved lambda call sites.
pub(crate) fn check_attributes_subtree<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> anyhow::Result<()> {
    let mut err: Option<anyhow::Error> = None;
    let mut stack: poolshark::local::LPooled<Vec<&Node<R, E>>> =
        poolshark::local::LPooled::take();
    stack.push(root);
    while let Some(node) = stack.pop() {
        let mut descend: poolshark::local::LPooled<Vec<&Node<R, E>>> =
            poolshark::local::LPooled::take();
        for_each_node(node, &mut |n| {
            if err.is_none() {
                if let Err(e) = check_node_attributes(n, ctx) {
                    err = Some(e);
                    return;
                }
            }
            if let NodeView::CallSite(cs) = n.view() {
                if let Some(crate::ApplyView::Lambda(g)) = cs.resolved_apply() {
                    descend.push(g.body());
                }
            }
        });
        if let Some(e) = err {
            return Err(e);
        }
        stack.extend(descend.drain(..));
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
pub fn try_fuse<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> anyhow::Result<Option<Node<R, E>>> {
    // A bare binding read forwards one input unchanged; fusing it wraps
    // zero compute in dispatch overhead.
    if region_is_identity(node) {
        return Ok(None);
    }
    let phase = perfdbg::span(&perfdbg::FUSION_RETURN_NS);
    let Some(return_type) = freeze_region_return(node.typ(), &ctx.env) else {
        if crate::dbgenv::gxdbg_freeze_ret() {
            crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                eprintln!("FREEZE-RET-MISS {:?} typ={}", node.spec().id, node.typ());
                Ok::<_, std::fmt::Error>(())
            })
            .ok();
        }
        return Ok(None);
    };
    drop(phase);
    ctx.fusion.stats.attempted += 1;
    let phase = perfdbg::span(&perfdbg::FUSION_BUILTINS_NS);
    // `apply_sites` lets `CallSite::emit_clif` lower a registered site
    // to a direct call.
    let mut discovery = lowering::BuiltinCallDiscovery::default();
    if let Err(blocker) = lowering::walk_node_for_builtin_calls(node, ctx, &mut discovery)
    {
        ctx.fusion.stats.rejected_before_emit += 1;
        return refuse(ctx, &blocker.spec, blocker.reason);
    }
    drop(phase);
    let phase = perfdbg::span(&perfdbg::FUSION_INPUTS_NS);
    let inputs = collect_region_inputs(&**node, ctx);
    if let Some(name) = non_scalar_basename_collision(&inputs) {
        return refuse(
            ctx,
            node.spec(),
            compact_str::format_compact!(
                "non-scalar region inputs share basename `{name}` — \
                 refuse to fuse"
            ),
        );
    }
    drop(phase);
    let phase = perfdbg::span(&perfdbg::FUSION_CALLEES_NS);
    // Callee kernels build before the jit lock is taken:
    // `build_lambda_kernel` needs `&mut ExecCtx`.
    let (lambda_sites, lambda_callees, callee_bodies, region_decorated) =
        discover_lambda_calls(node, ctx);
    drop(phase);
    let source_id = node.spec().id;
    let (sig, _arg_types) = match sig_from_inputs(
        arcstr::ArcStr::from(
            compact_str::format_compact!("region_{:?}", source_id).as_str(),
        ),
        inputs.iter().map(|fv| (fv.name.clone(), &fv.kind, Some(fv.bind_id))),
        return_type,
    ) {
        Ok(v) => v,
        Err(e) => {
            // A freeze invariant violation; de-fuse rather than panic.
            return refuse(
                ctx,
                node.spec(),
                compact_str::format_compact!("sig_from_inputs: {e:#}"),
            );
        }
    };
    let kernel = std::sync::Arc::new(sig);
    let build = |ctx: &mut ExecCtx<R, E>| {
        emit::compile_kernel_with_callees_direct(
            &mut ctx.fusion.jit.lock(),
            &kernel,
            &lambda_callees,
            node,
            &discovery.apply_sites,
            &lambda_sites,
            &callee_bodies,
            None,
            &ctx.env,
        )
    };
    let phase = perfdbg::span(&perfdbg::FUSION_EMIT_NS);
    let mut result = build(ctx);
    // An exhausted arena retires the whole active `Jit` (its kernels
    // stay mapped) and the build retries once in a fresh module; the
    // retry recompiles the whole callee set, so generations never link.
    if let Err(e) = &result {
        if format!("{e:#}").contains("memory region exhausted") {
            match emit::Jit::new() {
                Ok(fresh) => {
                    let old = std::mem::replace(&mut *ctx.fusion.jit.lock(), fresh);
                    ctx.fusion.retired_jits.lock().push(old);
                    ctx.fusion.stats.jit_generations += 1;
                    log::warn!(
                        "JIT code arena exhausted: retired generation {} (its \
                         kernels stay resident and running) and retrying this \
                         region in a fresh module. Recompile-heavy sessions \
                         (hot-reloading dynamic modules, long REPL/plugin \
                         sessions) accumulate one resident ~256MB arena per \
                         rotation — recycle the runtime/ExecCtx to reclaim \
                         them all.",
                        ctx.fusion.stats.jit_generations
                    );
                    result = build(ctx);
                }
                Err(e2) => {
                    log::warn!(
                        "JIT code arena exhausted and a fresh module could \
                         not be created ({e2:#}) — the region will run \
                         interpreted"
                    );
                }
            }
        }
    }
    drop(phase);
    let wrapped = match result {
        Ok(w) => std::sync::Arc::new(w),
        Err(e) => {
            log::trace!("fusion::try_fuse: region {source_id:?} doesn't fuse: {e:#}");
            let spec = e
                .chain()
                .find_map(|cause| cause.downcast_ref::<FusionBlocker>())
                .map(|blocker| &blocker.spec)
                .unwrap_or_else(|| node.spec());
            return refuse(ctx, spec, compact_str::format_compact!("{e:#}"));
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
    match builder::FusedKernel::<R, E>::new(
        node.spec().clone(),
        node.typ().clone(),
        kernel,
        Some(wrapped),
        feeders,
    ) {
        Ok(n) => {
            log::debug!(
                "fusion::try_fuse: fused region {source_id:?} with {} input(s)",
                inputs.len()
            );
            if crate::dbgenv::graphix_dbg_region() {
                for (i, fv) in inputs.iter().enumerate() {
                    let deref =
                        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                            compact_str::format_compact!("{}", fv.typ)
                        });
                    let cons = match &fv.typ {
                        crate::typ::Type::TVar(tv) => {
                            let cs = tv.cell_constraints();
                            compact_str::format_compact!("{cs:?}")
                        }
                        _ => compact_str::format_compact!("-"),
                    };
                    eprintln!(
                        "DBGREGION {source_id:?} input[{i}] name={} bind={:?} \
                         typ={} deref={deref} cons={cons} kind={:?}",
                        fv.name, fv.bind_id, fv.typ, fv.kind
                    );
                }
            }
            ctx.fusion.stats.record_fused(node.spec());
            if !region_decorated.is_empty() {
                ctx.attr_absorbed.lock().extend(region_decorated.iter().copied());
            }
            Ok(Some(n))
        }
        Err(e) => refuse(
            ctx,
            node.spec(),
            compact_str::format_compact!("FusedKernel::new: {e:#}"),
        ),
    }
}

/// De-fuse the region, recording the reason so `attempted` and
/// `failed` agree.
fn refuse<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    spec: &Expr,
    reason: compact_str::CompactString,
) -> anyhow::Result<Option<Node<R, E>>> {
    ctx.fusion.stats.record_failure(spec, reason);
    Ok(None)
}

/// Scalar env slots resolve by BindId, but the other per-kind tables
/// are name-keyed, so two non-scalar inputs sharing a basename would
/// alias one slot. Returns the first colliding name.
pub(crate) fn non_scalar_basename_collision(
    inputs: &[FreeVarInput],
) -> Option<&arcstr::ArcStr> {
    let mut names: LPooled<ahash::AHashSet<&str>> = LPooled::take();
    for fv in inputs {
        if matches!(fv.kind, RegionInputKind::Prim(_)) {
            continue;
        }
        if !names.insert(fv.name.as_str()) {
            return Some(&fv.name);
        }
    }
    None
}

/// Freeze a region root's type into the kernel's return ABI type, or
/// `None` if the region can't fuse: bare `Null` and `Unit` have no
/// kernel representation. Normalized because a select-rooted region's
/// type is the raw arm union; on failure, retry with refs expanded,
/// since an abstract-typed return carries Refs the env-free freeze rejects.
pub(crate) fn freeze_region_return(typ: &Type, env: &Env) -> Option<Type> {
    use kernel_abi::AbiKind;
    if typ.with_deref(|t| matches!(t, Some(Type::Fn(_) | Type::ByRef(_)))) {
        return None;
    }
    if crate::dbgenv::graphix_dbg_freeze() {
        let d = crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            compact_str::format_compact!("{typ}")
        });
        eprintln!(
            "DBGFREEZE typ={typ} deref={d} resolved={:?}",
            typ.resolve_tvars().normalize()
        );
    }
    let return_type = match freeze_for_abi_normalized(typ) {
        Some(t) => t,
        None => {
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

/// True iff the subtree is a bare binding read (through any number of
/// grouping parens) — an identity passthrough that forwards one input
/// unchanged, so there's no compute to fuse.
fn region_is_identity<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_) => return true,
            NodeView::ExplicitParens(p) => n = &*p.n,
            _ => return false,
        }
    }
}

/// Build a [`KernelSig`] from a typed input list — signature only. One
/// param per input in source order; vec order is ABI order. The second
/// return is the per-input graphix type list in the same order, the
/// caller-side type authority for cross-kernel call marshalling.
pub(crate) fn sig_from_inputs<'k>(
    fn_name: arcstr::ArcStr,
    inputs: impl IntoIterator<Item = (arcstr::ArcStr, &'k RegionInputKind, Option<BindId>)>,
    return_type: Type,
) -> anyhow::Result<(KernelSig, Vec<Type>)> {
    use kernel_abi::{KernelParam, ParamKind};
    let mut params: Vec<KernelParam> = Vec::new();
    let mut arg_types: Vec<Type> = Vec::new();
    for (name, kind, bind_id) in inputs.into_iter() {
        let (kind, typ) = match kind {
            RegionInputKind::Prim(prim) => {
                (ParamKind::Scalar(*prim), kernel_abi::prim_type(*prim))
            }
            RegionInputKind::Array(elem) => (
                ParamKind::Array { elem: elem.clone() },
                kernel_abi::array_type(elem.clone()),
            ),
            RegionInputKind::Tuple(t) => {
                let elems = kernel_abi::tuple_slots(t).map(<[Type]>::to_vec).ok_or_else(
                    || {
                        anyhow::anyhow!(
                            "RegionInputKind::Tuple must carry a frozen \
                             Type::Tuple (freeze invariant)"
                        )
                    },
                )?;
                (ParamKind::Tuple { elems }, t.clone())
            }
            RegionInputKind::Struct(t) => {
                let fields = kernel_abi::struct_fields(t)
                    .map(<[(arcstr::ArcStr, Type)]>::to_vec)
                    .ok_or_else(|| {
                        anyhow::anyhow!(
                            "RegionInputKind::Struct must carry a frozen \
                             Type::Struct (freeze invariant)"
                        )
                    })?;
                (ParamKind::Struct { fields }, t.clone())
            }
            RegionInputKind::Variant(t) => {
                let cases = kernel_abi::variant_cases(t).ok_or_else(|| {
                    anyhow::anyhow!(
                        "RegionInputKind::Variant must carry a frozen variant \
                         Type (freeze invariant)"
                    )
                })?;
                (ParamKind::Variant { cases }, t.clone())
            }
            RegionInputKind::Nullable(elem) => (
                ParamKind::Nullable { elem: elem.clone() },
                kernel_abi::nullable_type(elem.clone()),
            ),
            RegionInputKind::String => (ParamKind::String, kernel_abi::string_type()),
            RegionInputKind::Value(t) => (ParamKind::Value { typ: t.clone() }, t.clone()),
        };
        params.push(KernelParam { name, kind, bind_id });
        arg_types.push(typ);
    }
    let sig = KernelSig {
        fn_name,
        params,
        return_type,
        has_tail_loop: false,
        skipped_args: Vec::new(),
        tail_invariant: Vec::new(),
        defined: std::sync::atomic::AtomicBool::new(false),
        site_block_words: std::sync::atomic::AtomicU64::new(0),
    };
    Ok((sig, arg_types))
}
