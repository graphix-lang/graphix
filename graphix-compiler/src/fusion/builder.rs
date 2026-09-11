//! [`FusedKernel`]: the `Update` node wrapping a JIT artifact and its
//! input feeder Nodes. Built by `fusion::try_fuse`; [`Kernel`] executes it.

use crate::{
    Apply, Event, ExecCtx, Node, NodeView, Refs, Rt, Update, UserEvent,
    expr::Expr,
    fusion::{
        emit::{WrappedKernel, record_decode, record_encode, record_len},
        kernel::Kernel,
        kernel_abi::KernelSig,
    },
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_nodes, encode_nodes, nodes_len, put_tag, tag_len},
    },
    typ::Type,
};
use anyhow::{Result, anyhow};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use std::sync::Arc as StdArc;

/// An `Update` node over a compiled kernel.
pub struct FusedKernel<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    /// One feeder Node per kernel input slot.
    feeders: Box<[Node<R, E>]>,
    inner: Kernel,
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for FusedKernel<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedKernel").field("inputs", &self.feeders.len()).finish()
    }
}

impl<R: Rt, E: UserEvent> FusedKernel<R, E> {
    pub fn new(
        spec: Expr,
        typ: Type,
        kernel: StdArc<KernelSig>,
        wrapped: Option<StdArc<WrappedKernel>>,
        feeders: Box<[Node<R, E>]>,
    ) -> Result<Node<R, E>> {
        let n_args = feeders.len();
        // No interpreter fallback: without a JIT the caller leaves the
        // original nodes to node-walk.
        let wrapped = match wrapped {
            Some(w) => w,
            None => {
                return Err(anyhow!(
                    "no JIT for kernel `{}` — fused node must node-walk",
                    kernel.fn_name
                ));
            }
        };
        let inner = Kernel::new(kernel, n_args, wrapped)?;
        Ok(Node::new(Self { spec, typ, feeders, inner }))
    }

    /// The kernel signature this region fused into.
    pub fn kernel(&self) -> &StdArc<KernelSig> {
        self.inner.kernel()
    }

    /// The feeder nodes, one per kernel input slot.
    pub fn feeders(&self) -> &[Node<R, E>] {
        &self.feeders
    }

    /// Rebuild a region from an image: the wrapper record installs into
    /// the context's module and the node allocates fresh state.
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let feeders = decode_nodes(ctx, buf)?.into_boxed_slice();
        let state_words = decode_varint(buf)? as usize;
        let slot_table_words = Pack::decode(buf)?;
        let own_site = Pack::decode(buf)?;
        let state_self_blocks = Pack::decode(buf)?;
        let wrapper = record_decode(buf)?;
        let wrapped = ctx
            .fusion
            .jit
            .lock()
            .load_wrapped(
                &wrapper,
                state_words,
                slot_table_words,
                own_site,
                state_self_blocks,
            )
            .map_err(|e| {
                log::warn!(
                    "loading the kernel `{}` from the image: {e:#}",
                    wrapper.label
                );
                PackError::InvalidFormat
            })?;
        let kernel = wrapper.kernel.clone();
        let inner = Kernel::new(kernel, feeders.len(), StdArc::new(wrapped))
            .map_err(|_| PackError::InvalidFormat)?;
        Ok(Node::new(Self { spec, typ, feeders, inner }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for FusedKernel<R, E> {
    fn image_len(&self) -> usize {
        let w = self.inner.wrapped();
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + nodes_len(&self.feeders)
            + varint_len(w.state_words as u64)
            + w.slot_table_words.encoded_len()
            + w.own_site.encoded_len()
            + w.state_self_blocks.encoded_len()
            + record_len(&w.wrapper)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let w = self.inner.wrapped();
        put_tag(NodeTag::Fused, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.feeders, buf)?;
        encode_varint(w.state_words as u64, buf);
        w.slot_table_words.encode(buf)?;
        w.own_site.encode(buf)?;
        w.state_self_blocks.encode(buf)?;
        record_encode(&w.wrapper, buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> &crate::TagValue {
        let res = self.inner.update(ctx, &mut self.feeders, event);
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
        self.inner.sleep(ctx);
        for feeder in self.feeders.iter_mut() {
            feeder.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
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
