//! A boundary of walk pruning (`design/walk_pruning.md`): a statement,
//! an instance body or a select arm, which rides its resident in a cycle
//! that delivered nothing its subtree reads, or a fence no cycle skips
//! (a collection slot's call). Transparent to every analysis: its view,
//! children, spec and type are its child's.

use crate::{
    CompileCtx, ExecCtx, Node, NodeView, PruneMode, Refs, Rt, TagValue, Update,
    UserEvent, View, branch,
    expr::Expr,
    fusion::emit::{BodyCx, CompiledExpr},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    node::lambda::InstanceTypes,
    prune::BoundaryRef,
    typ::Type,
};
use anyhow::Result;
use graphix_types::BindId;
use netidx_core::pack::PackError;

#[derive(Debug)]
pub struct Prune<R: Rt, E: UserEvent> {
    boundary: BoundaryRef,
    child: Node<R, E>,
    /// What a cycle that skips the boundary rides; `None` for a boundary
    /// no cycle skips ([`Self::fence`]).
    ride: Option<Ride>,
}

#[derive(Debug)]
struct Ride {
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
        let boundary = Self::boundary(ctx);
        let child = ctx.in_boundary(Some(&boundary), f)?;
        Ok(Self::wrap(boundary, child))
    }

    /// A new boundary inside the current one.
    pub(crate) fn boundary(ctx: &CompileCtx<R, E>) -> BoundaryRef {
        BoundaryRef::new(ctx.boundary.clone())
    }

    /// `child`, compiled inside `boundary`.
    pub(crate) fn wrap(boundary: BoundaryRef, child: Node<R, E>) -> Node<R, E> {
        let ride = Some(Ride { resident: TagValue::phantom(), walked: false });
        Node::new(Self { boundary, child, ride })
    }

    /// `child`, compiled by `f` inside a new boundary within the current
    /// one that no cycle skips: the walk reaching it is what its visit
    /// records, so a reply whose reader sits in a part an interrupt cut
    /// short is delivered again (a collection slot's call).
    pub(crate) fn fence(
        ctx: &mut CompileCtx<R, E>,
        f: impl FnOnce(&mut CompileCtx<R, E>) -> Node<R, E>,
    ) -> Node<R, E> {
        let boundary = Self::boundary(ctx);
        let child = ctx.in_boundary(Some(&boundary), f);
        Node::new(Self { boundary, child, ride: None })
    }

    /// Run `f` over the child inside the boundary.
    pub(crate) fn within<T>(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        f: impl FnOnce(&mut CompileCtx<R, E>, &mut Node<R, E>) -> T,
    ) -> T {
        ctx.in_boundary(Some(&self.boundary), |ctx| f(ctx, &mut self.child))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let boundary = BoundaryRef::new(ctx.boundary.clone());
        let child = ctx.in_boundary(Some(&boundary), |ctx| decode_node(ctx, buf))?;
        Ok(Self::wrap(boundary, child))
    }

    /// Fuse the child, as a region or by its parts, inside the boundary.
    pub(crate) fn fuse_child(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.within(ctx, |ctx, child| crate::fusion::fuse(child, ctx))
    }

    pub(crate) fn child(&self) -> &Node<R, E> {
        &self.child
    }

    pub(crate) fn child_mut(&mut self) -> &mut Node<R, E> {
        &mut self.child
    }

    /// The walk of `child` in `boundary`, which pruning would have
    /// skipped: it must deliver the ridden `resident`, publish nothing and
    /// queue no effect.
    fn audit(
        boundary: &BoundaryRef,
        child: &mut Node<R, E>,
        ctx: &mut ExecCtx<'_, R, E>,
        resident: &TagValue,
    ) -> TagValue {
        let mut expected = resident.clone();
        expected.ride();
        let ((tv, published), effects) = branch::alone(ctx, |ctx| {
            let tv = ctx.in_boundary(Some(boundary), |ctx| child.update(ctx).clone());
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
                .map(|id| (id, ctx.event.variables.get(&id).cloned().expect("own")))
                .filter(|(id, tv)| !standing(id, tv))
                .collect();
            (tv, published)
        });
        let quiet = !tv.tag().triggers()
            && tv.tag().is_bottom() == expected.tag().is_bottom()
            && (tv.tag().is_bottom()
                || tv.with_value(|a| expected.with_value(|b| a == b)));
        if !quiet || !published.is_empty() || !effects.is_empty() {
            panic!(
                "GRAPHIX_PRUNE=audit: a boundary nothing marked changed in a cycle: \
                 {:?} -> {:?}, published {:?}, queued {:?}, at {} `{}`",
                expected,
                tv,
                published,
                effects,
                child.spec().pos,
                child.spec()
            )
        }
        tv
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Prune<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        self.boundary.visit(ctx.rt.cycle());
        let Self { boundary, child, ride } = self;
        let Some(ride) = ride else {
            return ctx.in_boundary(Some(boundary), |ctx| child.update(ctx));
        };
        // skipped when walked before, in a plain cycle, with nothing it
        // reads delivered
        if ctx.prune != PruneMode::Off
            && ride.walked
            && ctx.event.view == View::Cycle
            && !ctx.rt.dirty(boundary)
        {
            if ctx.prune == PruneMode::Audit {
                let tv = Self::audit(boundary, child, ctx, &ride.resident);
                return ride.resident.set(tv);
            }
            ctx.control.note_boundary(false);
            return ride.resident.ride();
        }
        ride.walked = true;
        ctx.control.note_boundary(true);
        let tv = ctx.in_boundary(Some(boundary), |ctx| child.update(ctx).clone());
        ride.resident.set(tv)
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.child.for_each_child(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.child.for_each_child_mut(f)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.in_boundary(Some(&self.boundary), |ctx| self.child.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(ride) = &mut self.ride {
            ride.walked = false;
        }
        ctx.in_boundary(Some(&self.boundary), |ctx| self.child.sleep(ctx))
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        ctx.in_boundary(Some(&self.boundary), |ctx| self.child.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        ctx.in_boundary(Some(&self.boundary), |ctx| {
            self.child.typecheck0_instance(ctx, types)
        })
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        ctx.in_boundary(Some(&self.boundary), |ctx| self.child.typecheck1(ctx))
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

    /// A fence stands only around a collection slot, which exists only
    /// once a cycle ran.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.ride.is_none() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::Prune, buf);
        self.child.image_encode(buf)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        self.child.emit_clif(cx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // reached through `descend`, which tries no region here
        ctx.in_boundary(Some(&self.boundary), |ctx| {
            crate::fusion::descend(&mut self.child, ctx)
        })?;
        Ok(None)
    }
}
