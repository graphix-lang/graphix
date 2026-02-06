use super::pattern::StructPatternNode;
use crate::{
    compiler::compile,
    expr::{self, Expr, ExprId, ExprKind, ModPath},
    format_with_flags,
    typ::Type,
    wrap, BindId, CFlag, Event, ExecCtx, Node, PrintFlag, Refs, Rt, Scope, Update,
    UserEvent,
};
use anyhow::{bail, Context, Result};
use enumflags2::BitFlags;
use netidx_value::Value;
use triomphe::Arc;

#[derive(Debug)]
pub(crate) struct Bind<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    pattern: StructPatternNode,
    node: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        b: &expr::BindExpr,
    ) -> Result<Node<R, E>> {
        let expr::BindExpr { rec, pattern, typ, value } = b;
        let (node, pattern, typ) = if *rec {
            if !pattern.single_bind().is_some() {
                bail!("at {} can't use rec on a complex pattern", spec.pos)
            }
            match value {
                Expr { kind: ExprKind::Lambda(_), .. } => (),
                _ => bail!("let rec may only be used for lambdas"),
            }
            let typ = match typ {
                Some(typ) => typ.scope_refs(&scope.lexical),
                None => Type::empty_tvar(),
            };
            let pattern =
                StructPatternNode::compile(ctx, &typ, pattern, scope, spec.pos, spec.ori.clone())
                    .with_context(|| format!("at {}", spec.pos))?;
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let ntyp = node.typ();
            if !typ.contains(&ctx.env, ntyp)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!("at {} error {} can't be matched by {typ}", ntyp, spec.pos)
                })?
            }
            (node, pattern, typ)
        } else {
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let typ = match typ {
                Some(typ) => typ.scope_refs(&scope.lexical),
                None => {
                    let typ = node.typ().clone();
                    let ptyp = pattern.infer_type_predicate(&ctx.env)?;
                    if !ptyp.contains(&ctx.env, &typ)? {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!(
                                "at {} match error {typ} can't be matched by {ptyp}",
                                spec.pos
                            )
                        })?
                    }
                    typ
                }
            };
            let pattern =
                StructPatternNode::compile(ctx, &typ, pattern, scope, spec.pos, spec.ori.clone())
                    .with_context(|| format!("at {}", spec.pos))?;
            (node, pattern, typ)
        };
        if pattern.is_refutable() {
            bail!("at {} refutable patterns are not allowed in let", spec.pos);
        }
        Ok(Box::new(Self { spec, typ, pattern, node }))
    }

    /// Return the id if this bind has only a single binding, otherwise return None
    pub(crate) fn single_id(&self) -> Option<BindId> {
        let mut id = None;
        let mut n = 0;
        self.pattern.ids(&mut |i| {
            if n == 0 {
                id = Some(i)
            }
            n += 1
        });
        if n == 1 {
            id
        } else {
            None
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Bind<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if let Some(v) = self.node.update(ctx, event) {
            self.pattern.bind(&v, &mut |id, v| {
                event.variables.insert(id, v.clone());
                ctx.cached.insert(id, v);
                ctx.rt.notify_set(id);
            })
        }
        None
    }

    fn refs(&self, refs: &mut Refs) {
        self.pattern.ids(&mut |id| {
            refs.bound.insert(id);
        });
        self.node.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.delete(ctx);
        self.pattern.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx);
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck(ctx))?;
        wrap!(self.node, self.typ.check_contains(&ctx.env, self.node.typ()))?;
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Ref {
    pub(super) spec: Arc<Expr>,
    pub(super) typ: Type,
    pub(super) id: BindId,
    pub(super) top_id: ExprId,
}

impl Ref {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        match ctx.env.lookup_bind(&scope.lexical, name) {
            None => bail!("at {} {name} not defined", spec.pos),
            Some((_, bind)) => {
                ctx.rt.ref_var(bind.id, top_id);
                let typ = bind.typ.clone();
                let spec = Arc::new(spec);
                Ok(Box::new(Self { spec, typ, id: bind.id, top_id }))
            }
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Ref {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        event.variables.get(&self.id).map(|v| v.clone())
    }

    fn refs(&self, refs: &mut Refs) {
        refs.refed.insert(self.id);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct ByRef<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    child: Node<R, E>,
    id: BindId,
}

impl<R: Rt, E: UserEvent> ByRef<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let child = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let id = BindId::new();
        if let Some(c) = (&*child as &dyn std::any::Any).downcast_ref::<Ref>() {
            ctx.env.byref_chain.insert_cow(id, c.id);
        }
        let typ = Type::ByRef(Arc::new(child.typ().clone()));
        Ok(Box::new(Self { spec, typ, child, id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ByRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if let Some(v) = self.child.update(ctx, event) {
            ctx.set_var(self.id, v);
        }
        if event.init {
            Some(Value::U64(self.id.inner()))
        } else {
            None
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.env.byref_chain.remove_cow(&self.id);
        self.child.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.child.sleep(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.child.refs(refs)
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck(ctx))?;
        let t = Type::ByRef(Arc::new(self.child.typ().clone()));
        wrap!(self, self.typ.check_contains(&ctx.env, &t))
    }
}

#[derive(Debug)]
pub(crate) struct Deref<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    child: Node<R, E>,
    id: Option<BindId>,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> Deref<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let child = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let typ = Type::empty_tvar();
        Ok(Box::new(Self { spec, typ, child, id: None, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Deref<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if let Some(v) = self.child.update(ctx, event) {
            if let Value::U64(i) | Value::V64(i) = v {
                let new_id = BindId::from(i);
                if self.id != Some(new_id) {
                    if let Some(old) = self.id {
                        ctx.rt.unref_var(old, self.top_id);
                    }
                    ctx.rt.ref_var(new_id, self.top_id);
                    self.id = Some(new_id);
                }
            }
        }
        self.id.and_then(|id| match event.variables.get(&id).cloned() {
            None if event.init => ctx.cached.get(&id).cloned(),
            v => v,
        })
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.top_id);
        }
        self.child.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.child.sleep(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.child.refs(refs);
        if let Some(id) = self.id {
            refs.refed.insert(id);
        }
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck(ctx))?;
        let typ = match self.child.typ() {
            Type::ByRef(t) => (**t).clone(),
            _ => bail!("expected reference"),
        };
        wrap!(self, self.typ.check_contains(&ctx.env, &typ))?;
        Ok(())
    }
}
