//! The `For` node — the sync subset's loop (design/sync_subset.md,
//! "P4 final scope"). Compiled from the desugar-internal
//! `ExprKind::ForFold`: a fold whose accumulator is the tuple of the
//! mut locals the loop body assigns.
//!
//! Emission contract: NEVER-UNTIL-COMPLETE — the loop emits only when
//! every element produced a value this evaluation; a bottomed body (or,
//! for an async body, a not-yet-arrived value) means no emission this
//! cycle. This matches the old fold contract and the JIT's #219
//! loop-carried taint model.
//!
//! Sequential semantics ARE the semantics: the sync body is ONE node
//! tree re-run in place per element (the `GXLambda` tail-loop
//! precedent) — a stateful Sync builtin in the body (`count`) is shared
//! across iterations, exactly what a loop means in a sequential
//! language. The per-slot-counter behavior of the old HOF machinery
//! dies with it.
//!
//! Async bodies (per-index instantiation + re-evaluation under taint)
//! are the second phase of this construction — see `design/
//! sync_subset.md`. Until that lands, an async call in a loop body has
//! ONE shared instance and its cross-iteration semantics are wrong;
//! the differential fixtures gate the phase.

use super::{Cached, compiler::compile, pattern::StructPatternNode};
use crate::{
    BindId, CFlag, Event, ExecCtx, ExprId, Node, NodeView, PrintFlag, RebindMap, Refs,
    Rt, Scope, Update, UserEvent,
    expr::{Expr, StructurePattern},
    fusion::emit::{BodyCx, CompiledExpr},
    typ::Type,
    wrap,
};
use anyhow::Result;
use enumflags2::BitFlags;
use netidx::publisher::Value;

#[derive(Debug)]
pub struct For<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub iter: Cached<R, E>,
    pub init: Cached<R, E>,
    pub acc_pattern: StructPatternNode,
    pub elem_pattern: StructPatternNode,
    pub body: Node<R, E>,
    /// The element type cell the elem pattern was compiled against —
    /// the iter's Array element unifies into it at typecheck.
    elem_t: Type,
    /// The body's EXTERNAL references (its refs minus the ids the
    /// acc/elem patterns bind) — the loop re-runs when any of these
    /// fire, in addition to iter/init updates and the init view.
    ext_refs: Vec<BindId>,
    scope: Scope,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> For<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        iter: &Expr,
        init: &Expr,
        acc_pattern: &StructurePattern,
        elem_pattern: &StructurePattern,
        body: &Expr,
    ) -> Result<Node<R, E>> {
        let iter = Cached::new(compile(ctx, flags, iter.clone(), scope, top_id)?);
        let init = Cached::new(compile(ctx, flags, init.clone(), scope, top_id)?);
        // The acc's type is the loop-carried unification knot: init
        // seeds it, the body result must stay within it.
        let acc_t = Type::empty_tvar();
        let elem_t = Type::empty_tvar();
        let acc_pattern = StructPatternNode::compile(
            ctx,
            &acc_t,
            acc_pattern,
            scope,
            spec.pos,
            spec.ori.clone(),
        )?;
        let elem_pattern = StructPatternNode::compile(
            ctx,
            &elem_t,
            elem_pattern,
            scope,
            spec.pos,
            spec.ori.clone(),
        )?;
        let body = compile(ctx, flags, body.clone(), scope, top_id)?;
        let mut refs = Refs::default();
        body.refs(&mut refs);
        acc_pattern.ids(&mut |id| refs.mark_bound(id));
        elem_pattern.ids(&mut |id| refs.mark_bound(id));
        let mut ext_refs: Vec<BindId> = Vec::new();
        refs.with_external_refs(|id| {
            if !ext_refs.contains(&id) {
                ext_refs.push(id);
            }
        });
        Ok(Box::new(Self {
            spec,
            typ: acc_t,
            iter,
            init,
            acc_pattern,
            elem_pattern,
            body,
            elem_t,
            ext_refs,
            scope: scope.clone(),
            top_id,
        }))
    }
}

impl<R: Rt, E: UserEvent> For<R, E> {
    /// The element type cell (the iter's Array element unified into
    /// it at typecheck) — the emitter's element-shape authority.
    pub fn elem_typ(&self) -> &Type {
        &self.elem_t
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for For<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let iter_up = self.iter.update(ctx, event);
        let init_up = self.init.update(ctx, event);
        let arr = match self.iter.cached.as_ref() {
            Some(Value::Array(a)) => a.clone(),
            Some(_) | None => return None,
        };
        let init = self.init.cached.as_ref()?.clone();
        let ext_fired = self.ext_refs.iter().any(|id| event.variables.contains_key(id));
        if !(iter_up || init_up || ext_fired || event.init) {
            return None;
        }
        // One call-depth unit covers the whole loop (the JIT scaffold's
        // `emit_depth_unit` twin): sequential element dispatches all
        // sit at the same level. A trip means the loop must not run —
        // bottom (never-until-complete).
        if !ctx.control.depth_enter() {
            ctx.control.depth_pop();
            return None;
        }
        let mut acc = init;
        let mut out = Some(());
        for v in arr.iter() {
            // Cooperative interrupt poll — a wedged huge loop aborts to
            // bottom, mirroring the scaffold loop head's poll.
            if ctx.control.interrupted() {
                out = None;
                break;
            }
            self.acc_pattern.bind(&acc, &mut |id, v| {
                ctx.rt.cached_mut().insert(id, v.clone());
                event.variables.insert(id, v);
            });
            self.elem_pattern.bind(v, &mut |id, v| {
                ctx.rt.cached_mut().insert(id, v.clone());
                event.variables.insert(id, v);
            });
            match self.body.update(ctx, event) {
                Some(v) => acc = v,
                // NEVER-UNTIL-COMPLETE: a bottomed element (or a
                // not-yet-arrived async value) suppresses the whole
                // loop's emission this cycle.
                None => {
                    out = None;
                    break;
                }
            }
        }
        ctx.control.depth_pop();
        out.map(|()| acc)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.iter.node.refs(refs);
        self.init.node.refs(refs);
        self.acc_pattern.ids(&mut |id| refs.mark_bound(id));
        self.elem_pattern.ids(&mut |id| refs.mark_bound(id));
        self.body.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.iter.node.delete(ctx);
        self.init.node.delete(ctx);
        self.body.delete(ctx);
        self.acc_pattern.ids(&mut |id| ctx.env.unbind_variable(id));
        self.elem_pattern.ids(&mut |id| ctx.env.unbind_variable(id));
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.iter.sleep(ctx);
        self.init.sleep(ctx);
        self.body.sleep(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.iter.node, self.iter.node.typecheck0(ctx))?;
        wrap!(self.init.node, self.init.node.typecheck0(ctx))?;
        // iter must be an Array; its element type feeds the elem
        // pattern's cells (bound at pattern compile).
        let elem_t = crate::deref_typ!("array", ctx, self.iter.node.typ(),
            Some(Type::Array(et)) => Ok((**et).clone())
        );
        let elem_t = wrap!(self.iter.node, elem_t)?;
        wrap!(self.iter.node, self.elem_t.check_contains(&ctx.env, &elem_t))?;
        // init seeds the acc knot; the body result must stay within it.
        wrap!(self.init.node, self.typ.check_contains(&ctx.env, &self.init.node.typ()))?;
        wrap!(self.body, self.body.typecheck0(ctx))?;
        // The loop-carry check runs against a SNAPSHOT of the body's
        // type (`reset_tvars` — bindings carried, cells fresh): the
        // body type is a select-arm UNION whose members' interiors can
        // reference FOREIGN live cells (a stdlib callee's def-signature
        // tvar reached through an arm's callsite type), and a live
        // containment walk alias/copy-contaminated `array::push`'s def
        // 'a. Snapshot cells are throwaway, so the walk can only bind
        // the acc's OWN cells.
        let body_t = self.body.typ().reset_tvars();
        wrap!(self.body, self.typ.check_contains(&ctx.env, &body_t))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.iter.node, self.iter.node.typecheck1(ctx))?;
        wrap!(self.init.node, self.init.node.typecheck1(ctx))?;
        wrap!(self.body, self.body.typecheck1(ctx))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::For(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        crate::fusion::emit::emit_for_node(cx, self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Statement spine: descend (the loop itself fuses only as part
        // of a parent region via emit_clif above).
        crate::fusion::fuse(&mut self.iter.node, ctx)?;
        crate::fusion::fuse(&mut self.init.node, ctx)?;
        crate::fusion::fuse(&mut self.body, ctx)?;
        Ok(None)
    }

    fn clone_rebind(
        &self,
        _ctx: &mut ExecCtx<R, E>,
        _scope: &Scope,
        _remap: &mut RebindMap,
    ) -> Node<R, E> {
        // clone_rebind is being DELETED this pass (design/sync_subset
        // "P4 final scope"); nothing may clone a For before that
        // deletion completes.
        unreachable!("For nodes are never clone_rebind'd")
    }
}
