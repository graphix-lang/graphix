//! The runtime carrier for a fused region: [`FusedKernel`] wraps the
//! JIT artifact + its input feeder Nodes as an ordinary `Update`
//! node. Built by `fusion::try_fuse` (regions) and
//! `FusedCallback::build_slot` (per-slot HOF dispatch); the actual
//! kernel executor is [`Kernel`].

use crate::{
    Apply, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Update, UserEvent,
    expr::{Expr, ExprId},
    fusion::{emit::WrappedKernel, kernel::Kernel, kernel_abi::KernelSig},
    typ::Type,
};
use anyhow::{Result, anyhow};
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
    typ: Type,
    /// One feeder Node per kernel input slot. Driven by
    /// `Kernel::update` via the `from` slice.
    feeders: Box<[Node<R, E>]>,
    /// The actual kernel executor — handles JIT/interp dispatch,
    /// `DYN_DISPATCH_HANDLE` setup, builtin slot pre-binding,
    /// pending-flag propagation, composite-return marshalling.
    /// Routing through `Kernel` (instead of re-implementing the
    /// dispatch surface) keeps FusedKernel minimal.
    inner: Kernel<R, E>,
    /// THIS instance's lifted connect-target ids, parallel to
    /// `kernel().lifted` (the sig's originals for the first instance;
    /// freshly minted per `clone_rebind`). The state buffer's reserved
    /// head words mirror these; clones mint from HERE, not the sig, so
    /// a clone-of-a-clone remaps the ids its feeders actually carry.
    lifted: Box<[crate::BindId]>,
    _phantom: std::marker::PhantomData<fn() -> (R, E)>,
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for FusedKernel<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedKernel").field("inputs", &self.feeders.len()).finish()
    }
}

impl<R: Rt, E: UserEvent> FusedKernel<R, E> {
    pub fn new(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        typ: Type,
        kernel: StdArc<KernelSig>,
        wrapped: Option<StdArc<WrappedKernel>>,
        feeders: Box<[Node<R, E>]>,
        scope: Scope,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let n_args = feeders.len();
        // A fused node REQUIRES a JIT. There is no interpreter fallback —
        // if JIT compilation failed, refuse
        // to construct. Every splice site treats this `Err` as "don't
        // splice — leave the original nodes to node-walk", which is
        // the universal fallback.
        let wrapped = match wrapped {
            Some(w) => w,
            None => {
                return Err(anyhow!(
                    "no JIT for kernel `{}` — fused node must node-walk",
                    kernel.fn_name
                ));
            }
        };
        let lifted: Box<[crate::BindId]> = kernel.lifted.iter().copied().collect();
        let inner = Kernel::new(ctx, kernel, n_args, wrapped, scope, top_id)?;
        Ok(Box::new(Self {
            spec,
            typ,
            feeders,
            inner,
            lifted,
            _phantom: std::marker::PhantomData,
        }))
    }

    /// The compiled kernel IR this region fused into. Used by graph
    /// introspection (`node_shape`) to assert what fused.
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
    ) -> Option<crate::TagValue> {
        // Delegate to Kernel (Apply) — drives the feeders, sets up
        // the DynCall dispatch handle, invokes JIT (or interp), and
        // decodes the return value. A RUN only surfaces FIRED outputs
        // (the return gate forces stale/taint to None inside the JIT);
        // a stale-fed poll re-surfaces the kernel's result slot tagged
        // STALE (`Kernel::last_result` — the value channel inside an
        // evaluation frame), so propagate `out_tag`.
        let res = self.inner.update(ctx, &mut self.feeders, event);
        // A lambda dispatch inside the kernel hit the call-depth limit
        // (the `graphix_depth_push` helper flagged it — native code
        // can't push a diagnostic). Report the fused region's spec so
        // the runtime's event stream can tell the user WHICH
        // expression bottomed. Node-walk trips (including DynCall'd
        // node-walk residue) report themselves and don't set the flag.
        if ctx.control.take_depth_trip() {
            ctx.diagnostics.push(crate::RtDiagnostic::CallDepthLimit {
                limit: ctx.control.max_call_depth(),
                spec: self.spec.clone(),
            });
        }
        res.map(|v| crate::TagValue::tagged(v, self.inner.out_tag()))
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // Unlike sleep (which leaves the kernel's input slots for the
        // arm-wake cached replay), a frame reset clears them: a
        // partially-fused loop body's kernel must not fire iteration i
        // from iteration i−1's marshalled inputs.
        self.inner.reset_replay(ctx);
        for feeder in self.feeders.iter_mut() {
            feeder.reset_replay(ctx);
        }
    }

    fn typecheck0(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typ(&self) -> &Type {
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

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::FusedKernel(self)
    }
}
