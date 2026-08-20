//! The runtime carrier for a fused region: [`FusedKernel`] wraps the
//! JIT artifact + its input feeder Nodes as an ordinary `Update`
//! node. Built by `fusion::try_fuse`; the actual kernel executor is
//! [`Kernel`].

use crate::{
    Apply, BindId, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Update, UserEvent,
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
        lifted_ids: &[BindId],
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
        let inner = Kernel::new(ctx, kernel, n_args, wrapped, lifted_ids, scope, top_id)?;
        Ok(Node::new(Self { spec, typ, feeders, inner }))
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
    ) -> &crate::TagValue {
        // Delegate to Kernel (Apply) — drives the feeders, sets up
        // the DynCall dispatch handle, invokes JIT (or interp), and
        // decodes the return value. The production is HONEST (the 5c
        // output flip): Fired/Stale results carry their in-band tag, a
        // bottomed result is the shared FreshBottom/StaleBottom, a
        // quiet poll rides the resident. Forward it — Kernel's
        // resident IS this node's return slot, and the old depth-0
        // fired-only filter (replay_frames Ruling A.2) is REPEALED:
        // dense consumers read staleness and bottomness off the tag.
        let res = self.inner.update(ctx, &mut self.feeders, event);
        // A lambda dispatch inside the kernel hit the call-depth limit
        // (the `graphix_depth_push` helper flagged it — native code
        // can't push a diagnostic). Report the fused region's spec so
        // the runtime's event stream can tell the user WHICH
        // expression bottomed. Node-walk trips (including DynCall'd
        // node-walk residue) report themselves and don't set the flag.
        // TAKE (Kernel::update only peeked — its production decision
        // left the flag for this diagnostic).
        if ctx.control.take_depth_trip() {
            log::error!(
                "call depth limit ({}) exceeded in fused region {} — the                  derivation produced a settled bottom (raise via                  Control::set_max_call_depth)",
                ctx.control.max_call_depth(),
                self.spec
            );
            ctx.diagnostics.push(crate::RtDiagnostic::CallDepthLimit {
                limit: ctx.control.max_call_depth(),
                spec: self.spec.clone(),
            });
        }
        res
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.inner.delete(ctx);
        for feeder in self.feeders.iter_mut() {
            feeder.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if crate::dbgenv::gxdbg_kernel_sleep() {
            eprintln!("FUSED-KERNEL-SLEEP {:?}", self.spec.id);
        }
        // Delegates: `Kernel::sleep` KEEPS both the input slots (the
        // arm-wake cached replay — a re-selected arm's kernel must
        // fire from its retained inputs) and the interior-bottom ride
        // caches (sleep is PAUSE), and sleeps the dyn slots' bound
        // applies (the `CallSite::sleep` twin). Contrast
        // `reset_replay` below, which clears both.
        self.inner.sleep(ctx);
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
