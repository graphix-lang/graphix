//! `#[parallel]` and `#[serial]` (`design/parallel_eval.md` §7): a node
//! that runs its child under fork flags of its own.

use crate::{
    CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, TagValue, Update, UserEvent,
    branch::ForkFlags,
    expr::Expr,
    fusion::{
        emit::{BodyCx, CompiledExpr},
        fuse,
    },
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    node::lambda,
    typ::Type,
    wrap,
};
use anyhow::{Result, bail};
use netidx_core::pack::{Pack, PackError};

/// What the attribute asks of the code it decorates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForkKind {
    /// `#[serial]`: nothing forks, callees included.
    Serial,
    /// `#[parallel]` or `#[parallel(grain)]`: every fork point within
    /// forks, callees excluded; a collection in ranges of `grain` slots
    /// (0: one range per worker).
    Parallel(u32),
}

impl ForkKind {
    /// The attribute `name` with its grain, if it is one of these.
    pub(crate) fn of(name: &str, grain: Option<u32>) -> Option<Self> {
        match name {
            "serial" => Some(Self::Serial),
            "parallel" => Some(Self::Parallel(grain.unwrap_or(0))),
            _ => None,
        }
    }

    fn packed(self) -> Option<u32> {
        match self {
            Self::Serial => None,
            Self::Parallel(g) => Some(g),
        }
    }
}

#[derive(Debug)]
pub struct ForkControl<R: Rt, E: UserEvent> {
    spec: Expr,
    pub(crate) kind: ForkKind,
    pub(crate) n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> ForkControl<R, E> {
    pub(crate) fn new(spec: Expr, kind: ForkKind, n: Node<R, E>) -> Node<R, E> {
        Node::new(Self { spec, kind, n })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let kind = match Option::<u32>::decode(buf)? {
            None => ForkKind::Serial,
            Some(g) => ForkKind::Parallel(g),
        };
        let n = decode_node(ctx, buf)?;
        Ok(Self::new(spec, kind, n))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ForkControl<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ForkControl, buf);
        self.spec.encode(buf)?;
        self.kind.packed().encode(buf)?;
        self.n.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // a boundary: the interior gets its own region pass
        fuse(&mut self.n, ctx)?;
        Ok(None)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let flags = match self.kind {
            ForkKind::Serial => ForkFlags { inhibit: true, ..ctx.fork },
            ForkKind::Parallel(grain) => ForkFlags { forced: Some(grain), ..ctx.fork },
        };
        ctx.with_fork_flags(flags, |ctx| self.n.update(ctx))
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.sleep(ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.n, self.n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        // CR claude for eric: [bug] check_parallel only looks for static fork points.
        // Inside a seq body, outside lambda literals, the machine sets ForkFlags::seq,
        // so fork_mode() is Off, and a #[parallel] map there builds and never forks, in
        // the default mode and under GRAPHIX_PAR=force. CLAUDE.md makes #[parallel] a
        // compile error where nothing forks. Refuse it in a seq body outside lambda
        // literals, the way seq.rs refuse_catch refuses catch. Checking the body before
        // lowering also catches `#[parallel] let`, whose attribute the lowering drops
        // today. probe: design/review-2026-10-05/repro/x-parallel-10.gx (x-parallel-10)
        if let ForkKind::Parallel(_) = self.kind {
            crate::analysis::check_parallel(&self.spec, &self.n, ctx)?;
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        self.n.typ()
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ForkControl(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        bail!("fork control runs its child under flags of its own")
    }
}
