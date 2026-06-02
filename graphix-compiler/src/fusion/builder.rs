//! Unified fusion: clean-slate prototype.
//!
//! This is the "go for broke" rewrite of the fusion pipeline as
//! described in `design/unified_fusion.md`. The pipeline:
//!
//! ```text
//! parse → compile (node graph) → walk → build → splice → native
//!                                ─ fusion_v2 ─
//! ```
//!
//! Each phase is a small module:
//!
//! - **Walk** (`fusion_unified::walk`): node graph → `Vec<KernelCandidate>`.
//! - **Build** (this module, `build_region`): candidate → `GirKernel`.
//! - **JIT** (`gir_jit::compile_kernel_with_wrapper`): `GirKernel` →
//!   native code. **Fully reused** — GIR → native is orthogonal to
//!   the discovery and splice layers.
//! - **Splice** (this module, `splice_region` — Phase 3.5): replace
//!   the node in the runtime's `nodes` map with a kernel-backed node.
//!
//! **Phase 3 scope** (current): build phase for `Region` candidates.
//! The smoke test compiles a small Sync program, walks it, builds a
//! kernel for the resulting Region candidate, and confirms the
//! kernel has the expected shape. Splice + execute lands in Phase
//! 3.5; LambdaBind / ValueBind / AnonymousLambda candidates land
//! later.
//!
//! **Reused machinery** (from legacy fusion):
//! - `fusion::emit_expr` — Expr → GirExpr translation. This is pure
//!   syntactic lowering, orthogonal to discovery. Reusing it is the
//!   right call.
//! - `fusion::build_kir_kernel_from_region` — wraps emit_expr into
//!   a GirKernel. Reusing it for now; may swap to a direct
//!   emit_expr + manual GirKernel assembly if the v2 needs differ.
//! - `gir`, `gir_jit`, `gir_interp` — the GIR → native and
//!   GIR → interp layers. Fully reused.

use crate::{
    expr::Expr,
    fusion::walker::{CandidateKind, KernelCandidate},
    gir::GirKernel,
    gir_jit::WrappedKernel,
    Event, ExecCtx, Node, Refs, Rt, UserEvent, Update,
};
use anyhow::{anyhow, Result};
use netidx_value::Value;
use std::sync::Arc as StdArc;

/// A kernel built from one candidate, ready to splice into the
/// runtime's node graph.
pub struct BuiltKernel {
    pub fn_name: arcstr::ArcStr,
    pub kernel: StdArc<GirKernel>,
    pub source_id: crate::expr::ExprId,
    /// Splice target — drives how `splice_one` plugs the kernel
    /// back into the runtime's nodes map.
    pub splice: SpliceTarget,
    /// Transitively-reachable sub-kernels referenced from this
    /// kernel's body via [`crate::gir::GirOp::Call`]. The splicer
    /// populates the runtime
    /// [`crate::gir_interp::KernelRegistry`] from this map so Call
    /// ops dispatch correctly.
    pub called_kernels: std::collections::BTreeMap<
        arcstr::ArcStr,
        crate::fusion::lowering::CachedKernel,
    >,
}

#[derive(Debug, Clone, Copy)]
pub enum SpliceTarget {
    /// Replace `runtime.nodes[source_id]` with a kernel-backed
    /// node. Used for `Region` candidates and `ValueBind` candidates
    /// (the bind's compiled subtree gets replaced).
    NodeReplaceById,
    /// Populate `ctx.fusion_lambdas[name].cache` with the kernel
    /// so `Lambda::compile`'s InitFn picks it up. Used for
    /// `LambdaBind` and `AnonymousLambda` candidates.
    InitFnCache,
}

/// Build a Region candidate into a kernel. **Phase 3 prototype**:
/// the caller supplies `inputs` directly (free-var discovery moves
/// to a separate pass in a later iteration). For zero-input
/// regions, pass `&[]`.
///
/// Returns `Err` if GIR emission fails (unsupported expression
/// shape) — same fail-soft contract today's `build_region_kernel`
/// uses, but lifted to Result for cleaner upstream handling.
/// Build a Region kernel from an already-resolved body Node ref. The
/// `Node<R, E>` ref is used to call `body.spec()` for the GIR-build
/// path (lowering today still walks the Expr tree; the Node ref will
/// be used directly once the lowering side is fully Node-based).
///
/// `source_id` identifies the Region in the runtime's node graph so
/// the splice phase knows where to install the kernel-backed Node.
pub fn build_region<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    source_id: crate::expr::ExprId,
    fn_name: &str,
    inputs: &[crate::fusion::RegionInput],
    discovery: crate::fusion::BuiltinCallDiscovery,
    ec: &mut ExecCtx<R, E>,
) -> Result<BuiltKernel> {
    let known = std::collections::BTreeMap::new();
    let consts = std::collections::BTreeMap::new();
    let (kernel, _signature, called_kernels) = crate::fusion::build_kir_kernel_from_region(
        fn_name,
        body,
        inputs,
        &discovery.fn_params,
        None,
        &known,
        &consts,
        discovery.apply_sites,
        ec,
    )
    .ok_or_else(|| anyhow!("GIR emission failed for region {fn_name}"))?;
    Ok(BuiltKernel {
        fn_name: arcstr::ArcStr::from(fn_name),
        kernel: StdArc::new(kernel),
        source_id,
        splice: SpliceTarget::NodeReplaceById,
        called_kernels,
    })
}

/// Build a Region kernel from a `KernelCandidate`. Thin wrapper around
/// [`build_region`] that extracts the body Node ref + source id from
/// the candidate. Kept around for code paths still operating in terms
/// of candidates; new sites should prefer `build_region` directly.
pub fn build_region_from_candidate<'a, R: Rt, E: UserEvent>(
    candidate: &KernelCandidate<'a, R, E>,
    fn_name: &str,
    inputs: &[crate::fusion::RegionInput],
    discovery: crate::fusion::BuiltinCallDiscovery,
    ec: &mut ExecCtx<R, E>,
) -> Result<BuiltKernel> {
    let body = match &candidate.kind {
        CandidateKind::Region { body, .. } => *body,
        _ => return Err(anyhow!("build_region called on non-Region candidate")),
    };
    build_region(body, candidate.source_id, fn_name, inputs, discovery, ec)
}

// ──────────────────────────────────────────────────────────────────
// Splice phase: BuiltKernel → Node<R, E>
// ──────────────────────────────────────────────────────────────────

/// Clean-slate v2 wrapper that turns a `BuiltKernel` into an
/// `Update`-implementing Node. Holds the compiled kernel + JIT
/// artifact and dispatches at `update()` time.
///
/// **Current scope**: kernels with zero or more scalar (primitive)
/// inputs and a scalar return. Composite inputs / returns are
/// follow-up work — `fuse()` filters them out so the kernel never
/// reaches this struct.
pub struct FusedKernel<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: crate::typ::Type,
    /// One feeder Node per kernel input slot. Driven by
    /// `GirNode::update` via the `from` slice.
    feeders: Box<[Node<R, E>]>,
    /// The actual kernel executor — handles JIT/interp dispatch,
    /// `DYN_DISPATCH_HANDLE` setup, builtin slot pre-binding,
    /// pending-flag propagation, composite-return marshalling.
    /// Routing through `GirNode` (instead of re-implementing the
    /// dispatch surface) keeps FusedKernel minimal.
    inner: crate::gir_interp::GirNode<R, E>,
    _phantom: std::marker::PhantomData<fn() -> (R, E)>,
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for FusedKernel<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedKernel")
            .field("inputs", &self.feeders.len())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> FusedKernel<R, E> {
    pub fn new(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        typ: crate::typ::Type,
        kernel: StdArc<GirKernel>,
        wrapped: Option<StdArc<WrappedKernel>>,
        feeders: Box<[Node<R, E>]>,
        scope: crate::Scope,
        top_id: crate::expr::ExprId,
        called_kernels: std::collections::BTreeMap<
            arcstr::ArcStr,
            crate::fusion::lowering::CachedKernel,
        >,
    ) -> Result<Node<R, E>> {
        let n_args = feeders.len();
        let mut registry = crate::gir_interp::KernelRegistry::default();
        for (name, c) in called_kernels {
            registry.kernels.insert(name, c.kernel);
        }
        let registry = StdArc::new(registry);
        let inner = match wrapped {
            Some(w) => crate::gir_interp::GirNode::with_jit(
                ctx, kernel, n_args, w, registry, scope, top_id,
            )?,
            None => crate::gir_interp::GirNode::new(
                ctx, kernel, n_args, registry, scope, top_id,
            )?,
        };
        Ok(Box::new(Self {
            spec,
            typ,
            feeders,
            inner,
            _phantom: std::marker::PhantomData,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for FusedKernel<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        // Delegate to GirNode (Apply) — drives the feeders, sets up
        // the DynCall dispatch handle, invokes JIT (or interp), and
        // decodes the return value.
        use crate::Apply;
        self.inner.update(ctx, &mut self.feeders, event)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        use crate::Apply;
        self.inner.delete(ctx);
        for feeder in self.feeders.iter_mut() {
            feeder.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for feeder in self.feeders.iter_mut() {
            feeder.sleep(ctx);
        }
    }

    fn typecheck_inner(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typ(&self) -> &crate::typ::Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        for feeder in self.feeders.iter() {
            feeder.refs(refs);
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::FusedKernel(self)
    }
}

/// Splice a `BuiltKernel` into a runtime Node. For Region targets,
/// produces a `FusedKernel<R, E>` Update node that the caller
/// installs in the runtime's `nodes` map at `built.source_id`.
///
/// `wrapped` is the JIT artifact when fusion JIT-compiled the
/// kernel, or `None` when the `JitDisabled` compile flag was set —
/// the resulting `FusedKernel` dispatches via `gir_interp` in that
/// case.
pub fn splice<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    built: &BuiltKernel,
    wrapped: Option<StdArc<WrappedKernel>>,
    spec: Expr,
    typ: crate::typ::Type,
    feeders: Box<[Node<R, E>]>,
    scope: crate::Scope,
    top_id: crate::expr::ExprId,
) -> Result<Node<R, E>> {
    match built.splice {
        SpliceTarget::NodeReplaceById => FusedKernel::<R, E>::new(
            ctx,
            spec,
            typ,
            built.kernel.clone(),
            wrapped,
            feeders,
            scope,
            top_id,
            built.called_kernels.clone(),
        ),
        SpliceTarget::InitFnCache => Err(anyhow!(
            "splice: InitFnCache target not yet supported (LambdaBind path)"
        )),
    }
}

