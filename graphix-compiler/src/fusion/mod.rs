//! Fusion: identify sync (pure) subtrees of the compiled node graph
//! and JIT-compile them to native kernels.
//!
//! The architecture is distributed (`design/distributed_jit.md`):
//! body code generation is `Update::emit_clif` / `Apply::emit_clif`
//! trait methods living with each node, and the fusion phase is
//! `Update::jit` recursion driven by [`fuse`]. This module supplies
//! the shared mechanics those impls call:
//!
//! - [`try_fuse`] — the whole-subtree compile attempt: region-input
//!   collection → [`sig_from_inputs`] → builtin/lambda call-site
//!   discovery → compile under the jit lock (`emit_clif` recursion
//!   from the root) → [`builder::FusedKernel`] + feeders. "Is it
//!   fusable" IS the compile attempt.
//! - [`jit_node`] — the uniform child-visit protocol (try_fuse,
//!   else recurse, swap in any replacement).
//! - [`lowering`] — discovery, signature derivation (lambda callees,
//!   per-slot HOF callbacks, body splits), abstract-type resolution.
//! - [`builder`] — the runtime [`builder::FusedKernel`] carrier.

pub mod builder;
pub mod lowering;

// Re-export public surface so callers see a flat `fusion::*` API.
pub use builder::FusedKernel;
pub use lowering::*;

/// Compile-time fusion outcome counters, accumulated on
/// [`crate::ExecCtx::fusion_stats`] by every `compile()` the context
/// runs. Per-context (the compiler supports many instances per
/// process and tests run in parallel) — unlike the runtime-side
/// per-thread `gir_jit_helpers` invocation counters, which can't
/// give per-region failure reasons and are wrong for a per-program
/// assertion on a multi-thread runtime.
///
/// Exists because the direct JIT path degrades silently: a region
/// that fails to compile just node-walks and produces the correct
/// value, so value-agreement tests cannot distinguish "fused
/// correctly" from "never fused". These counters make "did it
/// actually fuse, and if not why" a queryable fact.
#[derive(Debug, Clone, Default)]
pub struct FusionStats {
    /// `try_fuse` attempts that passed the cheap gates (identity,
    /// return-type) and reached the compile attempt.
    pub attempted: usize,
    /// Regions that compiled + spliced (direct path), or classic-path
    /// splices (for old-vs-new coverage comparison at the flip).
    pub fused: usize,
    /// Per-failure (region root ExprId, compile error) — the blocker
    /// profile. Compile-time only; bounded by program size.
    pub failed: Vec<(crate::expr::ExprId, compact_str::CompactString)>,
}

/// Run the fusion phase on a compiled Node tree: [`jit_node`] on the
/// root — `Update::jit` recursion in which each node fuses its own
/// maximal sync subtree via [`try_fuse`].
///
/// Called by [`crate::compile`] between typecheck phase 2 and
/// return. The function takes `&mut Node` rather than `&mut [Node]`
/// because — with `load()`'s Module wrapping in place — every
/// compile produces one Node (a module containing the file's
/// graph, or a single inline expression). Cross-Bind dependencies
/// are visible because they're inside the single Node tree.
pub fn fuse<R: crate::Rt, E: crate::UserEvent>(
    node: &mut crate::Node<R, E>,
    ctx: &mut crate::ExecCtx<R, E>,
    flags: enumflags2::BitFlags<crate::CFlag>,
) -> anyhow::Result<()> {
    // `FusionDisabled`: skip the whole phase — programs execute via
    // the node-walk. Used by the test harness's `interp` mode as
    // ground truth. `JitDisabled`: fusion could build kernels but
    // nothing can splice (a fused node REQUIRES a JIT artifact), so
    // skip entirely.
    let off = crate::CFlag::FusionDisabled | crate::CFlag::JitDisabled;
    if flags.intersects(off) {
        return Ok(());
    }
    jit_node(node, ctx)
}

/// One free-var input slot resolved during walker analysis.
#[derive(Debug, Clone)]
pub(crate) struct FreeVarInput {
    pub(crate) bind_id: crate::BindId,
    pub(crate) name: arcstr::ArcStr,
    /// Kernel-input classification, computed once from the binding's
    /// GIR type. Drives the `RegionInput.kind` at build time.
    pub(crate) kind: crate::fusion::lowering::RegionInputKind,
    /// Full graphix type — used to construct the runtime feeder Node
    /// (`genn::reference`), which needs the complete `Type`.
    pub(crate) typ: crate::typ::Type,
}

/// Walk the candidate subtree, collect every external Ref's
/// BindId (referenced but NOT bound inside the body), and resolve
/// each to its name + GIR-input classification via `ctx.env.by_id`.
///
/// Because `CallSite::refs` recurses into a statically-resolved
/// lambda's body (via `GXLambda::refs`), a lambda's captures
/// transitively surface here as external Refs of the enclosing
/// region — so this pre-pass registers them as parent kernel inputs,
/// and `emit_lambda_call`'s capture forwarding finds them via
/// `lookup_local_by_bind_id`. This is the mechanism that makes
/// closure conversion's capture cascade automatic.
///
/// Slots whose type has no kernel-input representation (function
/// types, bare `String`/`Null`/`Unit`) are skipped — the builder
/// fails those Refs when emitting them and the candidate stays
/// unfused (correct fall-back to the interpreter).
pub(crate) fn collect_region_inputs<R: crate::Rt, E: crate::UserEvent>(
    subtree: &dyn crate::Update<R, E>,
    ctx: &crate::ExecCtx<R, E>,
) -> Vec<FreeVarInput> {
    let mut refs = crate::Refs::default();
    subtree.refs(&mut refs);
    let mut out: Vec<FreeVarInput> = Vec::new();
    let mut seen: ahash::AHashSet<crate::BindId> = ahash::AHashSet::default();
    refs.with_external_refs(|id| {
        if !seen.insert(id) {
            return;
        }
        let Some(b) = ctx.env.by_id.get(&id) else { return };
        // Resolve named/abstract type refs to their concrete rep
        // BEFORE freezing — `freeze_concrete` is deliberately Env-free
        // and rejects `Type::Ref`, so an abstract-typed input (a Ref
        // to a hidden-rep type name, e.g. an interface's `type Elem`)
        // needs the same `resolve_abstract` pre-pass the classic
        // kernel-signature derivation applies (#218). The feeder type
        // below stays UNRESOLVED — the runtime Ref wants the type
        // system's view; only the kernel-slot classification (`kind`,
        // which carries the frozen types) needs the concrete rep.
        let resolved =
            crate::fusion::lowering::resolve_abstract(&b.typ, &ctx.env, 0);
        let Some(frozen) = crate::gir::freeze_concrete(&resolved) else {
            return;
        };
        let Some(kind) =
            crate::fusion::lowering::type_to_region_input_kind(frozen)
        else {
            return;
        };
        out.push(FreeVarInput {
            bind_id: id,
            name: arcstr::ArcStr::from(b.name.as_str()),
            kind,
            typ: b.typ.clone(),
        });
    });
    out
}

/// Visit `node` and every reachable descendant (pre-order: `f` sees a
/// node before its children) — the canonical full-coverage immutable
/// walker. The `NodeView` match is EXHAUSTIVE on purpose: a new node
/// variant is a compile error here, not a silently-untraversed
/// container (same discipline as `static_resolve::collect_lambda_binds`,
/// which should migrate onto this walker at Stage F). Lambda BODIES are
/// not descended (a body compiles per call site — its call sites belong
/// to the callee kernel's own discovery), and `FusedKernel` is opaque
/// (post-fusion synthetic).
pub(crate) fn for_each_node<'a, R: crate::Rt, E: crate::UserEvent>(
    node: &'a crate::Node<R, E>,
    f: &mut dyn FnMut(&'a crate::Node<R, E>),
) {
    f(node);
    use crate::NodeView;
    macro_rules! rec {
        ($($n:expr),*) => {{ $(for_each_node::<R, E>($n, f);)* }};
    }
    match node.view() {
        NodeView::Bind(b) => rec!(&b.node),
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
            for arg in cs.args.values() {
                if let Some(n) = &arg.node {
                    rec!(n)
                }
            }
            rec!(&cs.fnode)
        }
        NodeView::Select(s) => {
            rec!(&s.arg.node);
            for (pat, body) in s.arms.iter() {
                if let Some(g) = &pat.guard {
                    rec!(&g.node)
                }
                rec!(&body.node)
            }
        }
        NodeView::TryCatch(t) => {
            for n in t.nodes.iter() {
                rec!(n)
            }
            rec!(&t.handler)
        }
        NodeView::Qop(q) => rec!(&q.n),
        NodeView::OrNever(o) => rec!(&o.n),
        NodeView::ExplicitParens(p) => rec!(&p.n),
        NodeView::TypeCast(t) => rec!(&t.n),
        NodeView::Not(n) => rec!(&n.n),
        NodeView::Connect(c) => rec!(&c.node),
        NodeView::ConnectDeref(c) => rec!(&c.rhs.node),
        NodeView::StringInterpolate(s) => {
            for a in s.args.iter() {
                rec!(&a.node)
            }
        }
        NodeView::Any(a) => {
            for n in a.n.iter() {
                rec!(n)
            }
        }
        NodeView::Sample(s) => rec!(&s.trigger, &s.arg.node),
        NodeView::Struct(s) => {
            for c in s.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::StructWith(s) => {
            rec!(&s.source);
            for r in s.replace.iter() {
                rec!(&r.n.node)
            }
        }
        NodeView::Tuple(t) => {
            for c in t.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::Variant(v) => {
            for c in v.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::Array(a) => {
            for c in a.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::Map(m) => {
            for c in m.keys.iter() {
                rec!(&c.node)
            }
            for c in m.vals.iter() {
                rec!(&c.node)
            }
        }
        NodeView::StructRef(s) => rec!(&s.source),
        NodeView::TupleRef(t) => rec!(&t.source),
        NodeView::ArrayRef(a) => rec!(&a.source.node, &a.i.node),
        NodeView::ArraySlice(a) => {
            rec!(&a.source.node);
            if let Some(s) = &a.start {
                rec!(&s.node)
            }
            if let Some(e) = &a.end {
                rec!(&e.node)
            }
        }
        NodeView::MapRef(m) => rec!(&m.source.node, &m.key.node),
        NodeView::ByRef(b) => rec!(&b.child),
        NodeView::Deref(d) => rec!(&d.child),
        NodeView::Add(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Sub(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Mul(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Div(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Mod(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedAdd(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedSub(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedMul(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedDiv(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedMod(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Eq(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Ne(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Lt(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Gt(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Lte(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Gte(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::And(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Or(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Lambda(_) => {}
        NodeView::Ref(_)
        | NodeView::Constant(_)
        | NodeView::Use(_)
        | NodeView::TypeDef(_)
        | NodeView::Nop(_) => {}
        NodeView::FusedKernel(_) => {}
    }
}

/// One discovered statically-resolved lambda call site in a region
/// being directly compiled — the lambda-call analogue of
/// [`crate::fusion::lowering::BuiltinCallSiteInfo`]. Recorded by
/// [`discover_lambda_calls`] during `try_fuse`'s ANALYSIS phase (before
/// the jit lock — `build_lambda_kernel` needs `&mut ExecCtx`); consumed
/// by `CallSite::emit_clif` via `BodyCx::lambda_site` to emit a CLIF
/// `call` against the declared callee.
#[derive(Debug, Clone)]
pub struct LambdaCallInfo {
    /// The callee kernel's name in the `funcids`/`callee_refs` maps.
    /// Always the CACHED kernel's name (a cache hit returns the first
    /// builder's name — possibly a `__hof_*` name from the per-slot
    /// HOF path), never this call site's source name.
    pub fn_name: arcstr::ArcStr,
    /// Kept alive so the return-ABI read outlives the build and the
    /// `by_kernel` entry pins the same `Arc`.
    pub kernel: std::sync::Arc<crate::gir::KernelSig>,
    /// The callee's flat input types in signature order — formals
    /// first, captures appended (`KnownFusedFn::arg_types`, cloned
    /// from the `CachedKernel` at discovery). These were resolved
    /// (`resolve_abstract`) and frozen at BUILD time, so they're the
    /// caller's type authority for arg classification — freezing the
    /// caller-side node type instead re-rejects abstract Refs (#218),
    /// and env isn't available at emit time to resolve them. The
    /// classic caller (`emit_lambda_call`) types args the same way.
    pub arg_types: Vec<crate::typ::Type>,
    /// Closure-converted captures, appended after the formal args in
    /// the callee's input list (the caller marshals each from its own
    /// env, BindId-first).
    pub captures: Vec<crate::fusion::lowering::CaptureSlot>,
}

/// A discovered callee's body Node + self-call info, recorded by
/// [`discover_lambda_calls`] for the direct path's per-callee body
/// emission (F0). The body reference is live through this region's
/// resolved `GXLambda` for the duration of `try_fuse`.
pub struct CalleeBody<'n, R: crate::Rt, E: crate::UserEvent> {
    pub body: &'n crate::Node<R, E>,
    /// `Some((self_bind, info))` for a self-recursive callee: the
    /// binding its body's self-references carry, and the kernel's own
    /// call descriptor — non-tail self-calls emit through the regular
    /// lambda-call path against the kernel's own FuncRef; tail-position
    /// self-calls become the rebind-and-jump loop.
    pub self_call: Option<(crate::BindId, LambdaCallInfo)>,
}

/// Walk the region collecting every statically-resolved lambda call
/// site, building (or cache-hitting) each callee's [`CachedKernel`]
/// signature. A lambda that fails to build (unsupported arg/return
/// shape) is simply NOT recorded — its call site bails at emission and
/// the subtree node-walks. There is no transitive closure: a callee
/// body's own call sites are #203-unresolved, so a callee's only
/// cross-kernel reference is itself.
///
/// The third return maps each callee kernel (by `Arc::as_ptr`
/// identity — the define loop's cache key) to its body `Node`, reached
/// live through this site's resolved `GXLambda`, plus the self-call
/// emission info for recursive callees. Callee bodies compile by
/// `emit_clif` over that Node.
fn discover_lambda_calls<'n, R: crate::Rt, E: crate::UserEvent>(
    root: &'n crate::Node<R, E>,
    ctx: &mut crate::ExecCtx<R, E>,
) -> (
    nohash::IntMap<crate::expr::ExprId, LambdaCallInfo>,
    std::collections::BTreeMap<arcstr::ArcStr, std::sync::Arc<crate::gir::KernelSig>>,
    std::collections::BTreeMap<usize, CalleeBody<'n, R, E>>,
) {
    let mut sites: nohash::IntMap<crate::expr::ExprId, LambdaCallInfo> =
        nohash::IntMap::default();
    let mut callees: std::collections::BTreeMap<
        arcstr::ArcStr,
        std::sync::Arc<crate::gir::KernelSig>,
    > = std::collections::BTreeMap::new();
    let mut bodies: std::collections::BTreeMap<usize, CalleeBody<'n, R, E>> =
        std::collections::BTreeMap::new();
    for_each_node(root, &mut |n| {
        let crate::NodeView::CallSite(cs) = n.view() else {
            return;
        };
        let Some(crate::ApplyView::Lambda(g)) = cs.resolved_apply() else {
            return;
        };
        // Kernel name: the call site's SOURCE name, derived exactly
        // like the classic region path (lowering.rs `emit_expr_node`'s
        // CallSite arm) — a self-recursive body call resolves to the
        // same key the build registered before emitting its body, so a
        // recursive lambda builds ONCE with a coherent cache entry.
        // (Shadowed same-name lambdas can't collide within one region:
        // the second `let f` would put a Lambda-valued Bind inside the
        // region, which doesn't emit — the region splits first.) A
        // lambda-literal call (`(|x| x)(1)` — fnode isn't a Ref) has no
        // name and stays on the node-walk, classic parity.
        let crate::expr::ExprKind::Ref { name } = &cs.fnode.spec().kind
        else {
            return;
        };
        let name: arcstr::ArcStr =
            match crate::fusion::lowering::ident_of(name) {
                Some(ident) => arcstr::ArcStr::from(ident),
                None => {
                    let s: &str = name.0.as_ref();
                    arcstr::ArcStr::from(s)
                }
            };
        // The fnode Ref's BindId identifies the binding being called —
        // build_lambda_kernel uses it to recognise self-recursion (and
        // emit_known_fused_call to refuse shadowed same-name targets,
        // #206).
        let call_bind = match cs.fnode.view() {
            crate::NodeView::Ref(r) => Some(r.id),
            _ => None,
        };
        let Some(cached) = crate::fusion::lowering::build_lambda_kernel(
            g, &name, call_bind, ctx,
        ) else {
            return;
        };
        callees
            .entry(cached.fn_name.clone())
            .or_insert_with(|| cached.kernel.clone());
        bodies
            .entry(std::sync::Arc::as_ptr(&cached.kernel) as usize)
            .or_insert_with(|| CalleeBody {
                body: g.body(),
                // Self-call emission info for a recursive callee: the
                // binding the body's self-references carry, plus the
                // kernel's own call descriptor (name in callee_refs /
                // ABI Arc / capture slots — captures are params of
                // the kernel itself, so a self-call forwards them
                // from its own env).
                self_call: cached.is_rec.then(|| {
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
                }),
            });
        sites.insert(
            n.spec().id,
            LambdaCallInfo {
                fn_name: cached.fn_name,
                kernel: cached.kernel,
                arg_types: cached.signature.arg_types.clone(),
                captures: cached.captures,
            },
        );
    });
    (sites, callees, bodies)
}

/// Drive fusion for one child `Node` (the distributed path's uniform
/// child-visit protocol, used by every [`crate::Update::jit`] impl and
/// the compile-time driver): first try to fuse the WHOLE child subtree
/// via [`try_fuse`]; if it doesn't fuse, recurse into the child's own
/// `jit` (which fuses ITS children's maximal subtrees in turn). Either
/// way a produced replacement is swapped in and the original deleted —
/// the parent owns the swap because a node can't replace itself behind
/// `&mut self`.
///
/// Maximality falls out of the top-down order: the highest subtree
/// whose `try_fuse` succeeds is spliced and nothing below it is ever
/// attempted; a failed attempt falls through to finer-grained fusion
/// inside.
pub fn jit_node<R: crate::Rt, E: crate::UserEvent>(
    child: &mut crate::Node<R, E>,
    ctx: &mut crate::ExecCtx<R, E>,
) -> anyhow::Result<()> {
    if let Some(new) = try_fuse(child, ctx)? {
        let mut old = std::mem::replace(child, new);
        old.delete(ctx);
        return Ok(());
    }
    if let Some(new) = child.jit(ctx)? {
        let mut old = std::mem::replace(child, new);
        old.delete(ctx);
    }
    Ok(())
}

/// Try to fuse the whole subtree rooted at `node` into one JIT kernel.
/// Mechanics only, NO policy — policy (where to attempt, what to
/// recurse) lives in each node's [`crate::Update::jit`] impl.
///
/// `Ok(Some(replacement))` — the subtree compiled; the replacement is
/// a [`FusedKernel`] node (feeders + JIT dispatch) the caller swaps in
/// (deleting the original). `Ok(None)` — not fusable: the root type
/// isn't representable at the kernel boundary, the subtree is an
/// identity passthrough (zero compute — the runtime `Ref` feeder
/// already produces the value), or some node in the subtree doesn't
/// emit CLIF (the `emit_clif` default `Err`) — async ops, unsupported
/// shapes. "Is it fusable" IS the compile attempt; there is no
/// separate analysis to drift out of sync with the emitter.
pub fn try_fuse<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
    ctx: &mut crate::ExecCtx<R, E>,
) -> anyhow::Result<Option<crate::Node<R, E>>> {
    // Identity suppression: a bare binding read (possibly through
    // grouping parens) forwards one input unchanged — fusing it wraps
    // zero compute in dispatch overhead (and the `run!` harness's
    // `let result = {code}` wrapper would otherwise register as
    // "fused" without any real body fusion — see CLAUDE.md #139).
    if region_is_identity(node) {
        return Ok(None);
    }
    let Some(return_type) = freeze_region_return(node.typ(), &ctx.env) else {
        return Ok(None);
    };
    ctx.fusion_stats.attempted += 1;
    let inputs = collect_region_inputs(&**node, ctx);
    if let Some(name) = non_scalar_basename_collision(&inputs) {
        // A real blocker, not protocol noise — log it (a silent
        // Ok(None) after `attempted += 1` makes the stats disagree
        // with the failure list).
        ctx.fusion_stats.failed.push((
            node.spec().id,
            compact_str::format_compact!(
                "non-scalar region inputs share basename `{name}` — \
                 refuse to fuse"
            ),
        ));
        return Ok(None);
    }
    // Discover sync-builtin Apply sites BEFORE the kernel build (the
    // same Node-based prepass the classic path runs in `fuse`):
    // `fn_params` installs the `FnSource::Builtin` slots on the sig —
    // the runtime (`GirNode::pre_init_builtin_slots`) constructs each
    // site's Apply from them — and `apply_sites` lets
    // `CallSite::emit_clif` recognise a registered site and lower it
    // to a DynCall.
    let mut discovery = crate::fusion::BuiltinCallDiscovery::default();
    crate::fusion::walk_node_for_builtin_calls::<R, E>(
        node,
        ctx,
        &crate::expr::ModPath::root(),
        &mut discovery,
    );
    // Statically-resolved lambda call sites: build (or cache-hit) each
    // callee's kernel NOW — `build_lambda_kernel` needs `&mut ExecCtx`,
    // which emission (under the jit lock) can't have. The callees
    // compile from their GIR bodies during the parallel period; the
    // parent's call sites emit CLIF `call`s against them.
    let (lambda_sites, lambda_callees, callee_bodies) =
        discover_lambda_calls(node, ctx);
    let source_id = node.spec().id;
    let (mut sig, _arg_types) = sig_from_inputs(
        arcstr::ArcStr::from(
            compact_str::format_compact!("region_{:?}", source_id).as_str(),
        ),
        inputs.iter().map(|fv| (fv.name.clone(), &fv.kind, Some(fv.bind_id))),
        return_type,
    );
    sig.fn_params = discovery.fn_params;
    let kernel = std::sync::Arc::new(sig);
    // The compile attempt: entry binds the declared params, then the
    // body is emitted by `emit_clif` recursion from the root. Any Err
    // (a node that doesn't emit) discards the half-built function —
    // the subtree node-walks.
    let wrapped = match crate::gir_jit::compile_kernel_with_callees_direct(
        &mut ctx.jit.lock(),
        &kernel,
        &lambda_callees,
        node,
        &discovery.apply_sites,
        &lambda_sites,
        &callee_bodies,
        None,
        &ctx.env,
    ) {
        Ok(w) => std::sync::Arc::new(w),
        Err(e) => {
            log::trace!(
                "fusion::try_fuse: region {source_id:?} doesn't fuse: {e:#}"
            );
            ctx.fusion_stats
                .failed
                .push((source_id, compact_str::format_compact!("{e:#}")));
            return Ok(None);
        }
    };
    // Feeders register `Rt::ref_var(bind_id, TOP_ID)` — the runtime
    // wakes a top expression on `set_var` only while its (id, top_id)
    // ref count is nonzero. The REAL top id (from `ExecCtx::
    // fuse_top_id`, set per `compile()`), NOT the region's interior
    // `source_id`: registering under an id no installed expression
    // matches would strand the top expression at count zero once the
    // spliced original's Refs unref on delete — a region fed by a
    // `<-`-written variable would never see updates past cycle one.
    let feeder_top = ctx.fuse_top_id.unwrap_or(source_id);
    let feeders: Box<[crate::Node<R, E>]> = inputs
        .iter()
        .map(|fv| {
            crate::node::genn::reference::<R, E>(
                ctx,
                fv.bind_id,
                fv.typ.clone(),
                feeder_top,
            )
        })
        .collect();
    match builder::FusedKernel::<R, E>::new(
        ctx,
        node.spec().clone(),
        node.typ().clone(),
        kernel,
        Some(wrapped),
        feeders,
        crate::Scope::root(),
        source_id,
    ) {
        Ok(n) => {
            log::debug!(
                "fusion::try_fuse: fused region {source_id:?} with {} input(s)",
                inputs.len()
            );
            ctx.fusion_stats.fused += 1;
            Ok(Some(n))
        }
        Err(e) => {
            // The kernel COMPILED but the runtime carrier refused it —
            // log like any other blocker (a silent Ok(None) here made
            // `attempted` and `failed` disagree, which is exactly the
            // drift FusionStats exists to expose).
            ctx.fusion_stats.failed.push((
                source_id,
                compact_str::format_compact!("FusedKernel::new: {e:#}"),
            ));
            Ok(None)
        }
    }
}

/// Scalar env slots resolve BindId-first (C2) — duplicate basenames
/// are fine there. The OTHER per-kind tables are still name-keyed
/// (BindId-keying lands per-table as each shape gains emission):
/// two distinct non-scalar inputs sharing a basename would silently
/// alias one slot — callers refuse to fuse instead. Returns the
/// first colliding name.
pub(crate) fn non_scalar_basename_collision(
    inputs: &[FreeVarInput],
) -> Option<&arcstr::ArcStr> {
    use crate::fusion::lowering::RegionInputKind;
    let mut names: ahash::AHashSet<&str> = ahash::AHashSet::default();
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

/// Freeze a region root's graphix type into the kernel's return ABI
/// type, or `None` if the region can't fuse. The kernel boundary
/// can't represent bare `Null` (fusion must widen to Nullable first)
/// or `Unit` (a side-effect-only marker), and a type that doesn't
/// freeze to a concrete shape can't have an ABI at all.
/// `freeze_normalized` because a select-rooted region's type is
/// typecheck's raw arm union (`Set([i64, TVar→i64])`), which only
/// freezes once flattened. On plain-freeze failure, retry through
/// `resolve_abstract` (#218): a region returning an abstract-typed
/// value (e.g. `Array<Elem>` with `Elem` an interface type) carries
/// Refs the env-free freeze rejects; the resolved concrete rep IS
/// the return ABI.
pub(crate) fn freeze_region_return(
    typ: &crate::typ::Type,
    env: &crate::env::Env,
) -> Option<crate::typ::Type> {
    use crate::gir::AbiKind;
    let return_type = match crate::gir::freeze_normalized(typ) {
        Some(t) => t,
        None => {
            let resolved =
                crate::fusion::lowering::resolve_abstract(typ, env, 0);
            crate::gir::freeze_normalized(&resolved)?
        }
    };
    match crate::gir::abi_kind(&return_type) {
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
/// grouping parens) — the Node analog of the GIR
/// `is_identity_passthrough` (`Return(Local(_))`) check.
fn region_is_identity<R: crate::Rt, E: crate::UserEvent>(
    node: &crate::Node<R, E>,
) -> bool {
    use crate::NodeView;
    let mut n: &dyn crate::Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_) => return true,
            NodeView::ExplicitParens(p) => n = &*p.n,
            _ => return false,
        }
    }
}

/// Build a [`crate::gir::KernelSig`] straight from a typed input list
/// — no GIR body. The single source of truth for per-kind slot routing
/// (one slot per input, one [`crate::gir::TailCallSlot`] per input in
/// source order — the runtime's `build_arg_layout` reads the slot list
/// for per-position routing even in non-tail kernels). Used by every
/// kernel-build path: `try_fuse` regions, lambda kernels
/// (`build_lambda_kernel` — formals carry `bind_id: None`, captures
/// their binding), and body-split sub-regions.
///
/// The second return is the flat per-input graphix type list in slot
/// order — [`crate::gir::KnownFusedFn::arg_types`], the caller-side
/// type authority for cross-kernel call marshalling.
pub(crate) fn sig_from_inputs<'k>(
    fn_name: arcstr::ArcStr,
    inputs: impl IntoIterator<
        Item = (
            arcstr::ArcStr,
            &'k crate::fusion::lowering::RegionInputKind,
            Option<crate::BindId>,
        ),
    >,
    return_type: crate::typ::Type,
) -> (crate::gir::KernelSig, Vec<crate::typ::Type>) {
    use crate::fusion::lowering::RegionInputKind;
    use crate::gir::{
        ArrayInput, Input, NullableInput, StringInput, StructInput,
        TailCallSlot, TailCallSlotKind, TupleInput, ValueInput, VariantInput,
    };
    let mut sig = crate::gir::KernelSig {
        fn_name,
        params: Vec::new(),
        fn_params: Vec::new(),
        array_params: Vec::new(),
        tuple_params: Vec::new(),
        struct_params: Vec::new(),
        variant_params: Vec::new(),
        nullable_params: Vec::new(),
        string_params: Vec::new(),
        value_params: Vec::new(),
        tail_call_slots: Vec::new(),
        return_type,
        has_tail_loop: false,
    };
    let mut arg_types: Vec<crate::typ::Type> = Vec::new();
    for (name, kind, bind_id) in inputs {
        let slot_kind = match kind {
            RegionInputKind::Prim(prim) => {
                sig.params.push(Input { name: name.clone(), prim: *prim, bind_id });
                arg_types.push(crate::gir::prim_type(*prim));
                TailCallSlotKind::Scalar(*prim)
            }
            RegionInputKind::Array(elem) => {
                sig.array_params.push(ArrayInput {
                    name: name.clone(),
                    elem: elem.clone(),
                    bind_id,
                });
                arg_types.push(crate::gir::array_type(elem.clone()));
                TailCallSlotKind::ValArray
            }
            RegionInputKind::Tuple(t) => {
                let elems = crate::gir::tuple_slots(t)
                    .map(<[crate::typ::Type]>::to_vec)
                    .expect(
                        "RegionInputKind::Tuple must carry a frozen \
                         Type::Tuple (freeze invariant)",
                    );
                sig.tuple_params.push(TupleInput {
                    name: name.clone(),
                    elems,
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::ValArray
            }
            RegionInputKind::Struct(t) => {
                let fields = crate::gir::struct_fields(t)
                    .map(<[(arcstr::ArcStr, crate::typ::Type)]>::to_vec)
                    .expect(
                        "RegionInputKind::Struct must carry a frozen \
                         Type::Struct (freeze invariant)",
                    );
                sig.struct_params.push(StructInput {
                    name: name.clone(),
                    fields,
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::ValArray
            }
            RegionInputKind::Variant(t) => {
                let cases = crate::gir::variant_cases(t).expect(
                    "RegionInputKind::Variant must carry a frozen variant \
                     Type (freeze invariant)",
                );
                sig.variant_params.push(VariantInput {
                    name: name.clone(),
                    cases,
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::Variant
            }
            RegionInputKind::Nullable(elem) => {
                sig.nullable_params.push(NullableInput {
                    name: name.clone(),
                    elem: elem.clone(),
                    bind_id,
                });
                arg_types.push(crate::gir::nullable_type(elem.clone()));
                TailCallSlotKind::Nullable
            }
            RegionInputKind::String => {
                sig.string_params
                    .push(StringInput { name: name.clone(), bind_id });
                arg_types.push(crate::gir::string_type());
                TailCallSlotKind::String
            }
            RegionInputKind::Value(t) => {
                sig.value_params.push(ValueInput {
                    name: name.clone(),
                    typ: t.clone(),
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::Value
            }
        };
        sig.tail_call_slots.push(TailCallSlot { name, kind: slot_kind });
    }
    (sig, arg_types)
}

/// Find a Node anywhere in the tree whose `spec().id == target`.
/// Returns the owning `&Node<R, E>` reference if found — callers
/// can call any `Update` trait method (including `view()` for
/// further NodeView dispatch).
pub(crate) fn find_node_by_id<'a, R: crate::Rt, E: crate::UserEvent>(
    node: &'a crate::Node<R, E>,
    target: crate::expr::ExprId,
) -> Option<&'a crate::Node<R, E>> {
    if node.spec().id == target {
        return Some(node);
    }
    use crate::NodeView;
    match node.view() {
        NodeView::Block(blk) => {
            for child in blk.children.iter() {
                if let Some(found) = find_node_by_id::<R, E>(child, target) {
                    return Some(found);
                }
            }
            None
        }
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                if let Some(found) = find_node_by_id::<R, E>(child, target) {
                    return Some(found);
                }
            }
            None
        }
        NodeView::Bind(b) => find_node_by_id::<R, E>(&b.node, target),
        // Other container nodes (callsite args, select arms, etc.)
        // can't host Region candidates today — the walker doesn't
        // descend into them.
        _ => None,
    }
}

/// Find the descendant Node whose `spec().id == target` (or
/// `node` itself if it matches) and replace it with
/// `replacement`. On match: returns `Ok(old_node)`. On miss:
/// returns `Err(replacement)` unchanged so the caller can drop or
/// retry.
pub fn splice_into<R: crate::Rt, E: crate::UserEvent>(
    node: &mut crate::Node<R, E>,
    target: crate::expr::ExprId,
    replacement: crate::Node<R, E>,
) -> std::result::Result<crate::Node<R, E>, crate::Node<R, E>> {
    if node.spec().id == target {
        Ok(std::mem::replace(node, replacement))
    } else {
        node.splice_child(target, replacement)
    }
}
