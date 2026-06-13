use super::pattern::StructPatternNode;
use crate::{
    bailat,
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
pub struct Bind<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub(crate) typ: Type,
    pub(crate) pattern: StructPatternNode,
    pub(crate) node: Node<R, E>,
    /// Module scope at the binding's declaration point. Recorded so
    /// post-compile analysis tools (e.g. fusion's NodeView walk) can
    /// determine whether the binding is publishable at module level
    /// or block-scoped.
    pub(crate) scope: Scope,
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
    /// The single `BindId` this binding introduces, when the pattern
    /// binds exactly one name (`let x = …`). `None` for destructuring
    /// patterns. Used by the fusion walker (ValueBind candidates) and
    /// the JIT block-let binder (BindId-keyed env slots).
    pub(crate) fn single_bind_id(&self) -> Option<BindId> {
        let mut id: Option<BindId> = None;
        let mut count = 0usize;
        self.pattern.ids(&mut |i| {
            count += 1;
            if id.is_none() {
                id = Some(i);
            }
        });
        if count == 1 { id } else { None }
    }

    /// Build a `Bind` node from an already-compiled RHS node and an
    /// already-compiled destructuring pattern. AOT codegen has
    /// already resolved the type and generated BindIds for the
    /// pattern's names.
    #[allow(dead_code)]
    pub fn new(
        pattern: StructPatternNode,
        node: Node<R, E>,
        typ: Type,
        spec: Expr,
        scope: Scope,
    ) -> Node<R, E> {
        Box::new(Self { spec, typ, pattern, node, scope })
    }

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
                bailat!(spec, "can't use rec on a complex pattern")
            }
            match value {
                Expr { kind: ExprKind::Lambda(_), .. } => (),
                _ => bail!("let rec may only be used for lambdas"),
            }
            let typ = match typ {
                Some(typ) => typ.scope_refs(&scope.lexical),
                None => Type::empty_tvar(),
            };
            let pattern = StructPatternNode::compile(
                ctx,
                &typ,
                pattern,
                scope,
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| expr::ErrorContext(spec.clone()))?;
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let ntyp = node.typ();
            if !typ.contains(&ctx.env, ntyp)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bailat!(spec, "error {} can't be matched by {typ}", ntyp)
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
                            bailat!(
                                spec,
                                "match error {typ} can't be matched by {ptyp}"
                            )
                        })?
                    }
                    typ
                }
            };
            let pattern = StructPatternNode::compile(
                ctx,
                &typ,
                pattern,
                scope,
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| expr::ErrorContext(spec.clone()))?;
            (node, pattern, typ)
        };
        if pattern.is_refutable() {
            bailat!(spec, "refutable patterns are not allowed in let");
        }
        // If the bind's value is a builtin lambda (`let foo = |...| 'name`),
        // stash the metadata on `ctx.builtin_bindings` so the fusion
        // pass can recognise `Apply { function: Ref(foo) }` sites as
        // direct calls to a builtin and lower them via
        // `FnSource::Builtin`. Only fires for single-bind patterns
        // (multi-bind destructure of a lambda doesn't happen in
        // practice) and when the lambda body is the `'name` form.
        // If the bind's value is a builtin lambda (`let foo = |...| 'name`)
        // and the pattern is a simple `let <name> = ...`, register
        // the builtin metadata on `ctx.builtin_bindings` keyed by
        // (scope, name). Fusion's discovery pass looks up by
        // (scope, name) at every `Apply` site, so it doesn't
        // matter that sig and impl get different `BindId`s.
        if let ExprKind::Bind(be) = &spec.kind {
            if let crate::expr::StructurePattern::Bind(bind_name) =
                &be.pattern
            {
                if let ExprKind::Lambda(lam) = &value.kind {
                    if let netidx::utils::Either::Right(builtin_name) =
                        &lam.body
                    {
                        if let crate::typ::Type::Fn(fn_type) = node.typ() {
                            // Lambda Node's def field holds the
                            // LambdaDef; downcast through NodeView::Lambda
                            // to pull its id. Used at fusion time to
                            // look up the lambda's env+scope when
                            // compiling labeled-default arg expressions.
                            let lambda_id = match node.view() {
                                crate::NodeView::Lambda(l) => {
                                    l.lambda_id::<R, E>()
                                }
                                _ => None,
                            };
                            ctx.builtin_bindings.insert(
                                (
                                    scope.lexical.clone(),
                                    compact_str::CompactString::from(
                                        bind_name.as_str(),
                                    ),
                                ),
                                crate::BuiltinBindInfo {
                                    name: builtin_name.clone(),
                                    argspec: lam.args.clone(),
                                    typ: fn_type.clone(),
                                    lambda_id,
                                },
                            );
                        }
                    }
                }
            }
        }
        Ok(Box::new(Self { spec, typ, pattern, node, scope: scope.clone() }))
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

    fn typecheck0_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck0(ctx))?;
        wrap!(self.node, self.typ.check_contains(&ctx.env, self.node.typ()))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Bind(self)
    }

    fn jit(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
    ) -> Result<Option<Node<R, E>>> {
        // Fuse the bound VALUE, never the Bind itself: the Bind must
        // stay live to drive the publish of the result to its BindId
        // (the ValueBind splice shape). A whole-Bind fusion can't
        // happen anyway — Bind has no emit_clif, so any try_fuse
        // rooted here fails structurally.
        crate::fusion::jit_node(&mut self.node, ctx)?;
        Ok(None)
    }

    fn splice_child(
        &mut self,
        target: ExprId,
        replacement: crate::Node<R, E>,
    ) -> std::result::Result<crate::Node<R, E>, crate::Node<R, E>> {
        if self.node.spec().id == target {
            Ok(std::mem::replace(&mut self.node, replacement))
        } else {
            self.node.splice_child(target, replacement)
        }
    }

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
    ) -> Node<R, E> {
        // `rec`: pattern bound before value (so the value can reference
        // itself); non-rec: value cloned before the binding is re-minted
        // (the RHS can't see the new binding). Mirrors `Bind::compile`.
        let rec = matches!(&self.spec.kind, ExprKind::Bind(b) if b.rec);
        let (pattern, node) = if rec {
            let pattern = self.pattern.clone_rebind(ctx, scope);
            let node = self.node.clone_rebind(ctx, scope);
            (pattern, node)
        } else {
            let node = self.node.clone_rebind(ctx, scope);
            let pattern = self.pattern.clone_rebind(ctx, scope);
            (pattern, node)
        };
        Box::new(Self {
            spec: self.spec.clone(),
            typ: self.typ.clone(),
            pattern,
            node,
            scope: self.scope.clone(),
        })
    }
}

#[derive(Debug)]
pub struct Ref {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub id: BindId,
    pub(super) top_id: ExprId,
}

impl Ref {
    /// Construct a `Ref` node from its already-resolved components.
    /// AOT codegen uses this after name resolution has already
    /// assigned a BindId and a Type.
    ///
    /// Callers must ensure the runtime is told about the reference
    /// (via `ctx.rt.ref_var(id, top_id)`) separately — this
    /// constructor is a pure builder and does not touch ExecCtx.
    #[allow(dead_code)]
    pub fn new<R: Rt, E: UserEvent>(
        id: BindId,
        typ: Type,
        top_id: ExprId,
        spec: Expr,
    ) -> Node<R, E> {
        Box::new(Self { spec: Arc::new(spec), typ, id, top_id })
    }

    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        match ctx.env.lookup_bind(&scope.lexical, name) {
            None => bailat!(spec, "{name} not defined"),
            Some((_, bind)) => {
                let bind_id = bind.id;
                let typ = bind.typ.clone();
                let def_pos = bind.pos;
                let def_ori = bind.ori.clone();
                if ctx.env.lsp_mode {
                    ctx.references.push(crate::ReferenceSite {
                        pos: spec.pos,
                        ori: spec.ori.clone(),
                        name: name.clone(),
                        bind_id,
                        def_pos,
                        def_ori,
                    });
                }
                ctx.rt.ref_var(bind_id, top_id);
                let spec = Arc::new(spec);
                Ok(Box::new(Self { spec, typ, id: bind_id, top_id }))
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

    fn typecheck0_inner(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Ref(self)
    }

    fn emit_clif(
        &self,
        cx: &mut crate::gir_jit::BodyCx,
    ) -> Result<crate::gir_jit::CompiledExpr> {
        crate::gir_jit::emit_ref_node(
            cx,
            self.spec.as_ref(),
            &self.typ,
            self.id,
        )
    }

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
    ) -> Node<R, E> {
        // Resolve a fresh id by NAME in `scope`. The name comes from the
        // source spec (`ExprKind::Ref`), or — for a synthesized (NOP-spec)
        // feeder ref — from the binding record. An internal binding
        // (re-minted by an enclosing clone) resolves to its fresh id; a
        // capture (external, not re-bound here) resolves to the unchanged
        // outer binding; failing to resolve keeps the original id.
        let name: Option<ModPath> = match &self.spec.kind {
            ExprKind::Ref { name } => Some(name.clone()),
            _ => ctx
                .env
                .by_id
                .get(&self.id)
                .map(|b| ModPath::from_iter([b.name.as_str()])),
        };
        let new_id = name
            .and_then(|n| {
                ctx.env.lookup_bind(&scope.lexical, &n).map(|(_, b)| b.id)
            })
            .unwrap_or(self.id);
        ctx.rt.ref_var(new_id, self.top_id);
        Box::new(Self {
            spec: self.spec.clone(),
            typ: self.typ.clone(),
            id: new_id,
            top_id: self.top_id,
        })
    }
}

#[derive(Debug)]
pub struct ByRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub child: Node<R, E>,
    pub id: BindId,
}

impl<R: Rt, E: UserEvent> ByRef<R, E> {
    /// Construct a `ByRef` node from an already-compiled child.
    /// AOT codegen supplies the `BindId` (allocated at codegen time,
    /// reused when the generated tree is built) and the resolved
    /// `Type`. Interpreter `compile` still handles the additional
    /// byref-chain plumbing it needs — generated code that wants
    /// ref-to-ref chaining must mirror that separately via
    /// `ctx.env.byref_chain.insert_cow(...)` before building the
    /// node.
    #[allow(dead_code)]
    pub fn new(id: BindId, typ: Type, child: Node<R, E>, spec: Expr) -> Node<R, E> {
        Box::new(Self { spec, typ, child, id })
    }

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

    fn typecheck0_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck0(ctx))?;
        let t = Type::ByRef(Arc::new(self.child.typ().clone()));
        wrap!(self, self.typ.check_contains(&ctx.env, &t))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::ByRef(self)
    }
}

#[derive(Debug)]
pub struct Deref<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub child: Node<R, E>,
    pub id: Option<BindId>,
    pub(super) top_id: ExprId,
}

impl<R: Rt, E: UserEvent> Deref<R, E> {
    /// Build a `Deref` node from an already-compiled child that
    /// evaluates to a `Value::U64` / `Value::V64` holding a BindId.
    /// AOT codegen passes the resolved type rather than leaving an
    /// empty type variable for the interpreter to pin down later.
    #[allow(dead_code)]
    pub fn new(typ: Type, child: Node<R, E>, top_id: ExprId, spec: Expr) -> Node<R, E> {
        Box::new(Self { spec, typ, child, id: None, top_id })
    }

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

    fn typecheck0_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck0(ctx))?;
        let typ = match self.child.typ() {
            Type::ByRef(t) => (**t).clone(),
            _ => bail!("expected reference"),
        };
        wrap!(self, self.typ.check_contains(&ctx.env, &typ))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Deref(self)
    }
}
