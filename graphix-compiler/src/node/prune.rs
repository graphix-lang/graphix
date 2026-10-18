//! A boundary of walk pruning (`design/walk_pruning.md`): a statement,
//! an instance body or a select arm, which rides its resident in a cycle
//! that delivered nothing its subtree reads. Transparent to every
//! analysis: its view, children, spec and type are its child's.

use crate::{
    CompileCtx, ExecCtx, Node, NodeView, PruneMode, Refs, Rt, TagValue, Update,
    UserEvent, View,
    expr::Expr,
    fusion::emit::{BodyCx, CompiledExpr},
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    node::lambda::InstanceTypes,
    prune::BoundaryRef,
    typ::Type,
};
use anyhow::Result;
use graphix_types::BindId;
use netidx_core::pack::PackError;
use nohash::IntSet;

#[derive(Debug)]
pub struct Prune<R: Rt, E: UserEvent> {
    boundary: Option<BoundaryRef>,
    child: Node<R, E>,
    resident: TagValue,
    /// Walked at least once: until then nothing stands to ride.
    walked: bool,
}

impl<R: Rt, E: UserEvent> Prune<R, E> {
    /// `child`, compiled by `f` inside a new boundary within the current one.
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        f: impl FnOnce(&mut CompileCtx<R, E>) -> Result<Node<R, E>>,
    ) -> Result<Node<R, E>> {
        let boundary = Some(BoundaryRef::new(ctx.boundary.clone()));
        let child = ctx.in_boundary(&boundary, f)?;
        Ok(Self::wrap(boundary, child))
    }

    /// A new boundary inside the current one.
    pub(crate) fn boundary(ctx: &CompileCtx<R, E>) -> Option<BoundaryRef> {
        Some(BoundaryRef::new(ctx.boundary.clone()))
    }

    /// `child`, compiled inside `boundary`.
    pub(crate) fn wrap(boundary: Option<BoundaryRef>, child: Node<R, E>) -> Node<R, E> {
        Node::new(Self { boundary, child, resident: TagValue::phantom(), walked: false })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let boundary = Some(BoundaryRef::new(ctx.boundary.clone()));
        let child = ctx.in_boundary(&boundary, |ctx| decode_node(ctx, buf))?;
        Ok(Self::wrap(boundary, child))
    }

    /// Fuse the child, as a region or by its parts, inside the boundary.
    pub(crate) fn fuse_child(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        ctx.in_boundary(&self.boundary, |ctx| crate::fusion::fuse(&mut self.child, ctx))
    }

    pub(crate) fn child(&self) -> &Node<R, E> {
        &self.child
    }

    pub(crate) fn child_mut(&mut self) -> &mut Node<R, E> {
        &mut self.child
    }

    /// Whether this cycle may skip the boundary: walked before, a plain
    /// cycle, and nothing it reads delivered.
    fn clean(&self, ctx: &ExecCtx<'_, R, E>) -> bool {
        self.walked
            && ctx.event.view == View::Cycle
            && !self.boundary.as_ref().is_some_and(|b| ctx.rt.dirty(b))
    }

    /// The walk of a boundary pruning would have skipped: it must
    /// deliver the ridden resident and publish nothing.
    fn audit(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let mut expected = self.resident.clone();
        expected.ride();
        let before: IntSet<BindId> = ctx.event.variables.own_ids().collect();
        let tv = ctx.in_boundary(&self.boundary, |ctx| self.child.update(ctx).clone());
        let quiet = !tv.tag().triggers()
            && tv.tag().is_bottom() == expected.tag().is_bottom()
            && (tv.tag().is_bottom()
                || tv.with_value(|a| expected.with_value(|b| a == b)));
        // a stale publication of what the store holds reads as no
        // publication in a plain cycle
        let standing = |id: &BindId, tv: &TagValue| {
            !tv.tag().triggers()
                && ctx.rt.store_get(id).is_some_and(|(s, _)| {
                    s.tag().is_bottom() == tv.tag().is_bottom()
                        && (tv.tag().is_bottom()
                            || s.with_value(|a| tv.with_value(|b| a == b)))
                })
        };
        let published: Vec<(BindId, TagValue)> = ctx
            .event
            .variables
            .own_ids()
            .filter(|id| !before.contains(id))
            .map(|id| (id, ctx.event.variables.get(&id).cloned().expect("own")))
            .filter(|(id, tv)| !standing(id, tv))
            .collect();
        if !quiet || !published.is_empty() {
            panic!(
                "GRAPHIX_PRUNE=audit: a boundary nothing marked changed in a cycle: \
                 {:?} -> {:?}, published {:?}, at {} `{}`",
                expected,
                tv,
                published,
                self.child.spec().pos,
                self.child.spec()
            )
        }
        self.resident.set(tv)
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Prune<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        if ctx.prune != PruneMode::Off && self.clean(ctx) {
            if ctx.prune == PruneMode::Audit {
                return self.audit(ctx);
            }
            ctx.control.note_boundary(false);
            return self.resident.ride();
        }
        self.walked = true;
        ctx.control.note_boundary(true);
        let tv = ctx.in_boundary(&self.boundary, |ctx| self.child.update(ctx).clone());
        self.resident.set(tv)
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.child.for_each_child(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.child.for_each_child_mut(f)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.in_boundary(&self.boundary, |ctx| self.child.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.walked = false;
        ctx.in_boundary(&self.boundary, |ctx| self.child.sleep(ctx))
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        ctx.in_boundary(&self.boundary, |ctx| self.child.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        ctx.in_boundary(&self.boundary, |ctx| self.child.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        ctx.in_boundary(&self.boundary, |ctx| self.child.typecheck1(ctx))
    }

    fn typ(&self) -> &Type {
        self.child.typ()
    }

    fn refs(&self, refs: &mut Refs) {
        self.child.refs(refs)
    }

    fn spec(&self) -> &Expr {
        self.child.spec()
    }

    fn view(&self) -> NodeView<'_, R, E> {
        self.child.view()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Prune, buf);
        self.child.image_encode(buf)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        self.child.emit_clif(cx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // reached through `descend`, which tries no region here
        ctx.in_boundary(&self.boundary, |ctx| {
            crate::fusion::descend(&mut self.child, ctx)
        })?;
        Ok(None)
    }
}
