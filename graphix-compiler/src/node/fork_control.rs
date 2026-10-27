//! `#[parallel]` and `#[serial]` (`design/parallel_eval.md` §7): a node
//! that runs its child under fork flags of its own.

use crate::{
    CompileCtx, ExecCtx, Node, NodeView, Rt, TagValue, Update,
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
    typ::Type,
    wrap,
};
use anyhow::{Result, bail};
use netidx_core::pack::{Pack, PackError};
use triomphe::Arc;

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
pub struct ForkControl<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub(crate) kind: ForkKind,
    pub(crate) n: Node<R>,
}

impl<R: Rt> ForkControl<R> {
    pub(crate) fn new(spec: Arc<Expr>, kind: ForkKind, n: Node<R>) -> Node<R> {
        Node::new(Self { spec, kind, n })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let kind = match Option::<u32>::decode(buf)? {
            None => ForkKind::Serial,
            Some(g) => ForkKind::Parallel(g),
        };
        let n = decode_node(ctx, buf)?;
        Ok(Self::new(spec, kind, n))
    }
}

impl<R: Rt> Update<R> for ForkControl<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ForkControl, buf);
        self.spec.encode(buf)?;
        self.kind.packed().encode(buf)?;
        self.n.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        // a boundary: the interior gets its own region pass
        fuse(&mut self.n, ctx)?;
        Ok(None)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let flags = match self.kind {
            ForkKind::Serial => ForkFlags { inhibit: true, ..ctx.fork },
            ForkKind::Parallel(grain) => ForkFlags { forced: Some(grain), ..ctx.fork },
        };
        ctx.with_fork_flags(flags, |ctx| self.n.update(ctx))
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        match self.kind {
            // checked by the analysis, once the blocks under it are planned
            ForkKind::Parallel(_) => (),
            // nothing under a `#[serial]` forks, callees included
            ForkKind::Serial => {
                let mut inner = None;
                crate::fusion::for_each_node(&self.n, &mut |n| {
                    if let NodeView::ForkControl(f) = n.view()
                        && let ForkKind::Parallel(_) = f.kind
                    {
                        inner.get_or_insert_with(|| f.spec.clone());
                    }
                });
                if let Some(spec) = inner {
                    crate::bailat!(spec, "#[parallel] under #[serial] never forks")
                }
            }
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        self.n.typ()
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::ForkControl(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        bail!("fork control runs its child under flags of its own")
    }
}
