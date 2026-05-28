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
) -> Result<BuiltKernel> {
    let body_spec = body.spec();
    let known = std::collections::BTreeMap::new();
    let consts = std::collections::BTreeMap::new();
    let (kernel, _signature) = crate::fusion::build_kir_kernel_from_region(
        fn_name,
        body_spec,
        inputs,
        &discovery.fn_params,
        None,
        &known,
        &consts,
        discovery.apply_sites,
    )
    .ok_or_else(|| anyhow!("GIR emission failed for region {fn_name}"))?;
    Ok(BuiltKernel {
        fn_name: arcstr::ArcStr::from(fn_name),
        kernel: StdArc::new(kernel),
        source_id,
        splice: SpliceTarget::NodeReplaceById,
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
) -> Result<BuiltKernel> {
    let body = match &candidate.kind {
        CandidateKind::Region { body, .. } => *body,
        _ => return Err(anyhow!("build_region called on non-Region candidate")),
    };
    build_region(body, candidate.source_id, fn_name, inputs, discovery)
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
    ) -> Result<Node<R, E>> {
        let n_args = feeders.len();
        let registry =
            StdArc::new(crate::gir_interp::KernelRegistry::default());
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
        ),
        SpliceTarget::InitFnCache => Err(anyhow!(
            "splice: InitFnCache target not yet supported (LambdaBind path)"
        )),
    }
}

/// Core build helper: body Expr + free-var inputs → GirKernel.
/// Wraps the legacy `build_kir_kernel_from_region` chokepoint with
/// empty `extras` / `known` / `consts` — those slots are for
/// HOF args / cross-kernel calls / inlined constants which the v2
/// prototype doesn't exercise yet.
#[cfg(test)]
fn build_region_kernel(
    fn_name: &str,
    body: &Expr,
    inputs: &[crate::fusion::RegionInput],
) -> Option<(GirKernel, crate::gir::KnownFusedFn)> {
    let extras: Vec<crate::gir::FnParam> = Vec::new();
    let known = std::collections::BTreeMap::new();
    let consts = std::collections::BTreeMap::new();
    crate::fusion::build_kir_kernel_from_region(
        fn_name,
        body,
        inputs,
        &extras,
        None,
        &known,
        &consts,
        nohash::IntMap::default(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{ExprKind, ModPath};
    use crate::gir::PrimType;
    use crate::typ::Type;
    use netidx_value::{Typ, Value};
    use triomphe::Arc;

    /// Construct `1i64 + 2i64` as an Expr tree by hand (no parser
    /// needed). This is the simplest possible fusable program.
    fn one_plus_two() -> Expr {
        let one = ExprKind::Constant(Value::I64(1)).to_expr_nopos();
        let two = ExprKind::Constant(Value::I64(2)).to_expr_nopos();
        let add = ExprKind::Add {
            lhs: Arc::new(one),
            rhs: Arc::new(two),
        }
        .to_expr_nopos();
        // emit_expr's typed-AST fast path reads `body.typ.get()`.
        // For our hand-constructed expr, the type cells are empty,
        // so emit falls back to its emit-expr walk which infers
        // from the children. Constants carry their concrete types
        // directly via Value, so this should work without
        // typecheck.
        let _ = add.typ.set(Type::Primitive(Typ::I64.into()));
        add
    }

    #[test]
    fn build_region_one_plus_two() {
        // Smoke test: hand-constructed `1 + 2`, no walker needed.
        // Direct call into the build phase to verify GIR emission
        // works end-to-end on a minimal expr.
        let body = one_plus_two();
        let result = build_region_kernel("region_smoke", &body, &[]);
        let (kernel, sig) =
            result.expect("GIR emission should succeed for 1 + 2");
        assert_eq!(&*kernel.fn_name, "region_smoke");
        assert!(kernel.params.is_empty(), "zero scalar params");
        assert!(kernel.array_params.is_empty(), "zero array params");
        assert!(kernel.tuple_params.is_empty(), "zero tuple params");
        assert!(kernel.struct_params.is_empty(), "zero struct params");
        assert!(kernel.fn_params.is_empty(), "zero fn params");
        // Return type should be i64 (Prim).
        assert!(
            matches!(
                kernel.return_type,
                crate::gir::GirType::Prim(PrimType::I64)
            ),
            "expected i64 return, got {:?}",
            kernel.return_type
        );
        // Signature should mirror return type.
        assert!(matches!(
            sig.return_type,
            crate::gir::GirType::Prim(PrimType::I64)
        ));
    }

    #[test]
    fn build_region_jit_compiles() {
        // Verify the built kernel can be JIT-compiled via the
        // existing gir_jit path. End-to-end GIR → native check.
        let body = one_plus_two();
        let (kernel, _) =
            build_region_kernel("region_jit", &body, &[]).expect("GIR emit");
        let wrapped = crate::gir_jit::compile_kernel_with_wrapper(&kernel)
            .expect("JIT compile");
        // Invoke the wrapper directly — it expects (args_ptr,
        // out_ptr). Zero-input kernel takes a dummy args slice and
        // returns into out[0] as the i64 result bits.
        let args: [u64; 0] = [];
        let mut out: [u64; 2] = [0; 2];
        let f = unsafe { wrapped.fn_ptr() };
        unsafe {
            f(args.as_ptr(), out.as_mut_ptr());
        }
        assert_eq!(out[0] as i64, 3, "1 + 2 should produce 3");
    }

    #[test]
    fn build_region_with_one_input() {
        // Hand-built body: `Ref(x) + 2`. One free-var input `x: i64`.
        // Build the kernel, JIT-compile, invoke with x=5, expect 7.
        use crate::expr::ExprKind;
        use crate::fusion::{RegionInput, RegionInputKind, RegionInputSource};

        // Body: Ref(x) + 2
        let x_ref = ExprKind::Ref {
            name: ModPath::from_iter(["x"]),
        }
        .to_expr_nopos();
        let _ = x_ref.typ.set(Type::Primitive(Typ::I64.into()));
        let two = ExprKind::Constant(Value::I64(2)).to_expr_nopos();
        let _ = two.typ.set(Type::Primitive(Typ::I64.into()));
        let add = ExprKind::Add {
            lhs: Arc::new(x_ref.clone()),
            rhs: Arc::new(two),
        }
        .to_expr_nopos();
        let _ = add.typ.set(Type::Primitive(Typ::I64.into()));

        // One scalar input for x.
        let inputs = vec![RegionInput {
            expr_id: x_ref.id,
            name: arcstr::literal!("x"),
            kind: RegionInputKind::Prim(PrimType::I64),
            source: RegionInputSource::Binding,
        }];

        let (kernel, _sig) =
            build_region_kernel("region_with_input", &add, &inputs)
                .expect("GIR emission with one input");
        assert_eq!(kernel.params.len(), 1, "one scalar param");
        assert_eq!(&*kernel.params[0].name, "x");
        assert_eq!(kernel.params[0].prim, PrimType::I64);

        // JIT-compile and invoke with x = 5.
        let wrapped = crate::gir_jit::compile_kernel_with_wrapper(&kernel)
            .expect("JIT compile");
        let args: [u64; 1] = [5_i64 as u64];
        let mut out: [u64; 2] = [0; 2];
        let f = unsafe { wrapped.fn_ptr() };
        unsafe {
            f(args.as_ptr(), out.as_mut_ptr());
        }
        assert_eq!(out[0] as i64, 7, "x + 2 with x=5 should produce 7");
    }

    #[test]
    fn build_region_unknown_scope() {
        // Sanity: smoke test that ModPath::root() is accessible
        // from this crate (used by the walker entry point).
        let _ = ModPath::root();
    }

    // Update-time test for `splice()` + `FusedKernel::update`
    // lives in `stdlib/graphix-tests` where `GXRt` is available
    // — `graphix-compiler` doesn't have a concrete `Rt` impl to
    // monomorphize against. The compile-time structural shape
    // (splice returns Result<Node<R, E>>, FusedKernel impls
    // Update, NodeView::FusedV2 is a valid variant) is enforced
    // by the trait system and verified by `cargo build` of this
    // crate.
}
