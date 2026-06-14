//! The runtime carrier for a fused region: [`FusedKernel`] wraps the
//! JIT artifact + its input feeder Nodes as an ordinary `Update`
//! node. Built by `fusion::try_fuse` (regions) and
//! `FusedCallback::build_slot` (per-slot HOF dispatch); the actual
//! kernel executor is [`crate::gir_interp::GirNode`].

use crate::{
    expr::Expr,
    gir::KernelSig,
    gir_jit::WrappedKernel,
    Event, ExecCtx, Node, Refs, Rt, UserEvent, Update,
};
use anyhow::{anyhow, Result};
use netidx_value::Value;
use std::sync::Arc as StdArc;

/// Wrapper that turns a compiled kernel into an
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
        kernel: StdArc<KernelSig>,
        wrapped: Option<StdArc<WrappedKernel>>,
        feeders: Box<[Node<R, E>]>,
        scope: crate::Scope,
        top_id: crate::expr::ExprId,
    ) -> Result<Node<R, E>> {
        let n_args = feeders.len();
        // A fused node REQUIRES a JIT. The GIR interpreter is gone —
        // if JIT compilation failed (or `JitDisabled` was set), refuse
        // to construct. Every splice site treats this `Err` as "don't
        // splice — leave the original nodes to node-walk", which is
        // the universal fallback.
        let wrapped = match wrapped {
            Some(w) => w,
            None => {
                return Err(anyhow!(
                    "no JIT for kernel `{}` — fused node must node-walk",
                    kernel.fn_name
                ))
            }
        };
        let inner = crate::gir_interp::GirNode::new(
            ctx, kernel, n_args, wrapped, scope, top_id,
        )?;
        Ok(Box::new(Self {
            spec,
            typ,
            feeders,
            inner,
            _phantom: std::marker::PhantomData,
        }))
    }

    /// The compiled kernel IR this region fused into. Used by graph
    /// introspection (`crate::node_shape`) to assert what fused.
    pub fn kernel(&self) -> &StdArc<KernelSig> {
        self.inner.kernel()
    }

    /// The per-input feeder nodes — the kernel's children in the
    /// graph. Each drives one kernel input slot.
    pub fn feeders(&self) -> &[Node<R, E>] {
        &self.feeders
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

    fn typecheck0(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
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

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &crate::Scope,
    ) -> Node<R, E> {
        // An ordinary Node: recurse its feeder deps (Refs that re-resolve
        // to the slot's fresh element / captures by name) and clone the
        // GirNode by SHARING the immutable kernel/JIT/registry Arcs while
        // re-initing per-cycle scratch + dyn_slots. The kernel is
        // incidental — the precompiled update logic, shared across slots.
        let feeders: Box<[Node<R, E>]> =
            self.feeders.iter().map(|f| f.clone_rebind(ctx, scope)).collect();
        let n_args = feeders.len();
        let inner = self
            .inner
            .clone_shared(ctx, n_args, scope.clone(), self.spec.id)
            .expect("FusedKernel GirNode clone_shared failed");
        Box::new(Self {
            spec: self.spec.clone(),
            typ: self.typ.clone(),
            feeders,
            inner,
            _phantom: std::marker::PhantomData,
        })
    }
}


