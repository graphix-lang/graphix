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

pub use kernel::FusedKernel;

use crate::{
    ApplyView, BindId, ExecCtx, LambdaId, Node, NodeView, PrintFlag, Refs, Rt, Update,
    UserEvent,
    env::Env,
    expr::{Expr, ExprId, ExprKind, Origin},
    format_with_flags,
    fusion::{
        kernel_abi::{
            AbiKind, FreezeError, KernelParam, KernelSig, ParamKind,
            freeze_for_abi_normalized, try_freeze_for_abi_normalized,
        },
        lowering::expand_refs,
    },
    node,
    node::genn,
    profile::{self, Phase},
    typ::{FnType, Type},
};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use parking_lot::{MappedMutexGuard, MutexGuard};
use poolshark::local::LPooled;
use std::collections::BTreeMap;
use triomphe::Arc;

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

/// Per-[`ExecCtx`] state owned by the fusion subsystem, reached as
/// `ctx.fusion.<x>`.
pub struct FusionCtx {
    /// Per-context cranelift module + cross-kernel-call cache, built on
    /// first use ([`Self::jit`]). The mutex is interior mutability for
    /// `ExecCtx`'s `Sync` bound; JIT ops are compile-time only.
    jit: parking_lot::Mutex<Option<emit::Jit>>,
    /// Monomorphized lambda-kernel cache. Catch coverage and fn
    /// resolutions are part of the key because the kernel bakes them.
    /// The cached `Arc<KernelSig>` is the callable handle: the JIT's
    /// `by_kernel` cache keys on its pointer identity. It lives as long
    /// as the context's compiled code (a later compile may call an
    /// earlier one's lambda); [`Self::reset_jit_for_check`] clears it.
    // XCR claude for eric: cleared per check (the LSP's growth), not per compile:
    // a REPL or dynamic-module compile calls an earlier compile's lambdas, whose
    // cached sig is also the key of their compiled bodies in `by_kernel`, and
    // FusionStats is documented to accumulate across compiles.
    pub kernels: parking_lot::Mutex<
        BTreeMap<
            (LambdaId, Arc<FnType>, lowering::QopCoverage, lowering::FnResolutions),
            LambdaCallInfo,
        >,
    >,
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
}

impl FusionCtx {
    /// The context's JIT module, built on first use.
    pub(crate) fn jit(&self) -> anyhow::Result<MappedMutexGuard<'_, emit::Jit>> {
        let mut jit = self.jit.lock();
        if jit.is_none() {
            *jit = Some(emit::Jit::new()?);
        }
        Ok(MutexGuard::map(jit, |jit| jit.as_mut().expect("built above")))
    }

    pub fn new() -> anyhow::Result<Self> {
        Ok(Self {
            jit: parking_lot::Mutex::new(None),
            kernels: parking_lot::Mutex::new(BTreeMap::new()),
            enabled: true,
            stats: FusionStats::default(),
            top_id: None,
        })
    }

    /// Drop the JIT module (the next fusion builds a fresh one) and
    /// forget the previous check's kernel signatures and fusion outcomes:
    /// every lambda id they key on died with it. The old module's code is
    /// freed once no kernel compiled into it is left.
    ///
    /// For the check/LSP path, which would otherwise accumulate every
    /// checked file's kernels in one module.
    pub fn reset_jit_for_check(&mut self) -> anyhow::Result<()> {
        *self.jit.lock() = None;
        self.kernels.lock().clear();
        self.stats.failed.clear();
        self.stats.fused_sources.clear();
        Ok(())
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
    macro_rules! rec {
        ($($n:expr),*) => {{ $(for_each_node::<R, E>($n, f);)* }};
    }
    match node.view() {
        NodeView::Bind(b) => rec!(&b.node),
        NodeView::MapQ(m) => rec!(&m.source, &m.prototype),
        NodeView::FoldQ(m) => rec!(&m.source, &m.init, &m.prototype),
        NodeView::Module(m) => {
            if let Some(s) = m.source() {
                rec!(s);
            }
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
                if let Some(manual) = abort.manual() {
                    rec!(manual);
                }
            }
        }
        NodeView::Qop(q) => rec!(&q.n),
        NodeView::SeqGuard(g) => rec!(&g.n),
        NodeView::SeqAbort(a) => rec!(&a.n),
        NodeView::SeqCapture(c) => rec!(&c.snapshot, &c.live),
        NodeView::SeqMachine(m) => {
            rec!(&m.pc);
            for s in m.steps.iter() {
                for n in s.nodes.iter() {
                    rec!(n)
                }
            }
        }
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
            for (k, _) in m.entries.iter() {
                rec!(k)
            }
            for (_, v) in m.entries.iter() {
                rec!(v)
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
        NodeView::ByRef(b) => b.for_each_child(&mut |c| rec!(c)),
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
                rec!(&p.site)
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
    pub(crate) decorated: LPooled<nohash::IntSet<ExprId>>,
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
    ctx: &ExecCtx<R, E>,
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
                d.decorated.insert(n.spec().id);
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
    let mut stack: LPooled<Vec<&Node<R, E>>> = LPooled::take();
    stack.push(root);
    while let Some(node) = stack.pop() {
        let mut descend: LPooled<Vec<&Node<R, E>>> = LPooled::take();
        for_each_node(node, &mut |n| {
            if err.is_none() {
                if let Err(e) = check_node_attributes(n, ctx) {
                    err = Some(e);
                    return;
                }
            }
            if let NodeView::CallSite(cs) = n.view() {
                if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
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
    let build = |ctx: &mut ExecCtx<R, E>| {
        emit::compile_kernel_with_callees_direct(
            &mut *ctx.fusion.jit()?,
            &kernel,
            &lambdas.callees,
            node,
            &discovery.apply_sites,
            &lambdas.sites,
            &lambdas.bodies,
            &ctx.env,
        )
    };
    let phase = profile::phase(Phase::Emit);
    let mut result = build(ctx);
    // An exhausted arena retires the whole active `Jit` (its kernels
    // keep its code alive) and the build retries once in a fresh module;
    // the retry recompiles the whole callee set, so generations never link.
    if let Err(e) = &result
        && e.chain().any(|c| c.is::<emit::ArenaExhausted>())
    {
        match emit::Jit::new() {
            Ok(fresh) => {
                *ctx.fusion.jit.lock() = Some(fresh);
                ctx.fusion.stats.jit_generations += 1;
                log::warn!(
                    "JIT code arena exhausted: retired generation {} (freed when its \
                     last kernel drops) and retrying this region in a fresh module",
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
    drop(phase);
    let wrapped = match result {
        Ok(w) => w,
        Err(e) => {
            log::trace!("fusion::try_fuse: region {source_id:?} doesn't fuse: {e:#}");
            for (spec, why) in lambdas.refused.iter() {
                ctx.fusion.stats.record_failure(spec, why);
            }
            let spec = e
                .chain()
                .find_map(|cause| cause.downcast_ref::<FusionBlocker>())
                .map(|blocker| &blocker.spec)
                .unwrap_or_else(|| node.spec());
            return refuse(ctx, spec, &format!("{e:#}"));
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
    if !lambdas.decorated.is_empty() {
        ctx.attr_absorbed.lock().extend(lambdas.decorated.iter().copied());
    }
    Ok(Some(n))
}

/// Whether a build failed for want of JIT code memory: the arena
/// refuses the define with an allocation error.

/// De-fuse the region, recording the reason so `attempted` and
/// `failed` agree.
fn refuse<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
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
