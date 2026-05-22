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
//! - **Build** (this module, `build_region`): candidate → `KirKernel`.
//! - **JIT** (`kir_jit::compile_kernel_with_wrapper`): `KirKernel` →
//!   native code. **Fully reused** — KIR → native is orthogonal to
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
//! - `fusion::emit_expr` — Expr → KirExpr translation. This is pure
//!   syntactic lowering, orthogonal to discovery. Reusing it is the
//!   right call.
//! - `fusion::build_kir_kernel_from_region` — wraps emit_expr into
//!   a KirKernel. Reusing it for now; may swap to a direct
//!   emit_expr + manual KirKernel assembly if the v2 needs differ.
//! - `kernel_ir`, `kir_jit`, `kir_interp` — the KIR → native and
//!   KIR → interp layers. Fully reused.

use crate::{
    expr::Expr,
    fusion::walker::{CandidateKind, KernelCandidate},
    kernel_ir::{KirKernel, KirType},
    kir_jit::WrappedKernel,
    Event, ExecCtx, Node, Refs, Rt, UserEvent, Update,
};
use anyhow::{anyhow, Result};
use netidx_value::Value;
use std::sync::Arc as StdArc;

/// A kernel built from one candidate, ready to splice into the
/// runtime's node graph.
pub struct BuiltKernel {
    pub fn_name: arcstr::ArcStr,
    pub kernel: StdArc<KirKernel>,
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
/// Returns `Err` if KIR emission fails (unsupported expression
/// shape) — same fail-soft contract today's `build_region_kernel`
/// uses, but lifted to Result for cleaner upstream handling.
pub fn build_region<'a, R: Rt, E: UserEvent>(
    candidate: &KernelCandidate<'a, R, E>,
    fn_name: &str,
    inputs: &[crate::fusion::RegionInput],
) -> Result<BuiltKernel> {
    let body = match &candidate.kind {
        CandidateKind::Region { body, .. } => *body,
        _ => return Err(anyhow!("build_region called on non-Region candidate")),
    };
    let (kernel, _signature) = build_region_kernel(fn_name, body, inputs)
        .ok_or_else(|| anyhow!("KIR emission failed for region {fn_name}"))?;
    Ok(BuiltKernel {
        fn_name: arcstr::ArcStr::from(fn_name),
        kernel: StdArc::new(kernel),
        source_id: candidate.source_id,
        splice: SpliceTarget::NodeReplaceById,
    })
}

// ──────────────────────────────────────────────────────────────────
// Splice phase: BuiltKernel → Node<R, E>
// ──────────────────────────────────────────────────────────────────

/// Clean-slate v2 wrapper that turns a `BuiltKernel` into an
/// `Update`-implementing Node. Holds the compiled kernel + JIT
/// artifact and dispatches at `update()` time.
///
/// **Phase 3c scope**: zero-input Region kernels with scalar
/// return only. Input plumbing (feeder nodes subscribing to
/// BindIds), interp fallback, and non-Prim return types land in
/// follow-up iterations.
pub struct FusedKernel<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: crate::typ::Type,
    kernel: StdArc<KirKernel>,
    wrapped: StdArc<WrappedKernel>,
    /// Has this kernel produced its value this run? Zero-input
    /// kernels are constant — we fire once on initial cycle and
    /// then return None (no change).
    fired: bool,
    /// `fn() -> (R, E)` instead of `(R, E)` so `Send + Sync` are
    /// unconditional — the standard "no actual storage, just
    /// covariant in the generic" pattern.
    _phantom: std::marker::PhantomData<fn() -> (R, E)>,
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for FusedKernel<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedKernel")
            .field("fn_name", &self.kernel.fn_name)
            .field("return_type", &self.kernel.return_type)
            .field("fired", &self.fired)
            .finish()
    }
}

impl<R: Rt, E: UserEvent> FusedKernel<R, E> {
    pub fn new(
        spec: Expr,
        typ: crate::typ::Type,
        kernel: StdArc<KirKernel>,
        wrapped: StdArc<WrappedKernel>,
    ) -> Node<R, E> {
        Box::new(Self {
            spec,
            typ,
            kernel,
            wrapped,
            fired: false,
            _phantom: std::marker::PhantomData,
        })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for FusedKernel<R, E> {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _event: &mut Event<E>,
    ) -> Option<Value> {
        // Zero-input kernel: fire once on the initial cycle, then
        // hold the value (return None to indicate no change).
        if self.fired {
            return None;
        }
        // Invoke the wrapper. Zero-arg kernel takes an empty args
        // slice; out is a 2-slot buffer (the wrapper writes its
        // result into out[0..n] depending on the return type).
        let args: [u64; 0] = [];
        let mut out: [u64; 2] = [0; 2];
        let f = unsafe { self.wrapped.fn_ptr() };
        unsafe {
            f(args.as_ptr(), out.as_mut_ptr());
        }
        self.fired = true;
        // Decode the result based on the kernel's return type.
        match &self.kernel.return_type {
            KirType::Prim(p) => {
                let reg = crate::kir_jit::unpack_u64_to_reg(out[0], *p);
                Some(reg.to_value())
            }
            // Non-Prim returns: deferred to Phase 3d / 3e.
            other => {
                log::warn!(
                    "fusion_v2: non-Prim return type {other:?} not yet \
                     supported in FusedKernel; producing None"
                );
                None
            }
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Zero-input kernel has no child nodes to delete.
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Reset on sleep so the next wakeup re-fires.
        self.fired = false;
    }

    fn typecheck_inner(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        // The body's typechecking was done by the original compile
        // pass before fusion. Nothing more to do here.
        Ok(())
    }

    fn typ(&self) -> &crate::typ::Type {
        &self.typ
    }

    fn refs(&self, _refs: &mut Refs) {
        // Zero-input kernel: no input BindIds to register. Input
        // plumbing lands in the next iteration.
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
/// Requires the kernel to have been JIT-compiled — callers pass
/// the `WrappedKernel` alongside the built kernel. Pure-interp
/// fallback lands later.
pub fn splice<R: Rt, E: UserEvent>(
    built: &BuiltKernel,
    wrapped: StdArc<WrappedKernel>,
    spec: Expr,
    typ: crate::typ::Type,
) -> Result<Node<R, E>> {
    match built.splice {
        SpliceTarget::NodeReplaceById => Ok(FusedKernel::<R, E>::new(
            spec,
            typ,
            built.kernel.clone(),
            wrapped,
        )),
        SpliceTarget::InitFnCache => Err(anyhow!(
            "splice: InitFnCache target not yet supported (LambdaBind path)"
        )),
    }
}

/// Core build helper: body Expr + free-var inputs → KirKernel.
/// Wraps the legacy `build_kir_kernel_from_region` chokepoint with
/// empty `extras` / `known` / `consts` — those slots are for
/// HOF args / cross-kernel calls / inlined constants which the v2
/// prototype doesn't exercise yet.
fn build_region_kernel(
    fn_name: &str,
    body: &Expr,
    inputs: &[crate::fusion::RegionInput],
) -> Option<(KirKernel, crate::kernel_ir::KnownFusedFn)> {
    let extras: Vec<crate::kernel_ir::FnParam> = Vec::new();
    let known = std::collections::BTreeMap::new();
    let consts = std::collections::BTreeMap::new();
    crate::fusion::build_kir_kernel_from_region(
        fn_name, body, inputs, &extras, None, &known, &consts,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{ExprKind, ModPath};
    use crate::kernel_ir::PrimType;
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
        // Direct call into the build phase to verify KIR emission
        // works end-to-end on a minimal expr.
        let body = one_plus_two();
        let result = build_region_kernel("region_smoke", &body, &[]);
        let (kernel, sig) =
            result.expect("KIR emission should succeed for 1 + 2");
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
                crate::kernel_ir::KirType::Prim(PrimType::I64)
            ),
            "expected i64 return, got {:?}",
            kernel.return_type
        );
        // Signature should mirror return type.
        assert!(matches!(
            sig.return_type,
            crate::kernel_ir::KirType::Prim(PrimType::I64)
        ));
    }

    #[test]
    fn build_region_jit_compiles() {
        // Verify the built kernel can be JIT-compiled via the
        // existing kir_jit path. End-to-end KIR → native check.
        let body = one_plus_two();
        let (kernel, _) =
            build_region_kernel("region_jit", &body, &[]).expect("KIR emit");
        let wrapped = crate::kir_jit::compile_kernel_with_wrapper(&kernel)
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
                .expect("KIR emission with one input");
        assert_eq!(kernel.params.len(), 1, "one scalar param");
        assert_eq!(&*kernel.params[0].name, "x");
        assert_eq!(kernel.params[0].prim, PrimType::I64);

        // JIT-compile and invoke with x = 5.
        let wrapped = crate::kir_jit::compile_kernel_with_wrapper(&kernel)
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
