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
    pub(super) spec: Expr,
    pub(super) typ: Type,
    pub(super) pattern: StructPatternNode,
    pub(super) node: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
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
    ) -> Node<R, E> {
        Box::new(Self { spec, typ, pattern, node })
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
        // Side-channel for fusion: when value is a Lambda (directly,
        // not wrapped), publish the binding name on ctx so
        // `Lambda::compile` can capture it and use it as the
        // synthetic `fn_name` during fusion. Required for self-
        // recursion detection (`let rec X = |...| X(...)`). Restored
        // at the end of the bind so sibling/outer Binds aren't
        // affected.
        let saved_binding = if matches!(&value.kind, ExprKind::Lambda(_)) {
            Some(std::mem::replace(
                &mut ctx.current_binding_name,
                pattern.single_bind().cloned(),
            ))
        } else {
            None
        };
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
        if let Some(prior) = saved_binding {
            ctx.current_binding_name = prior;
        }
        // Record outer-scope const bindings so subsequent inner-
        // lambda fusion attempts (e.g. unannotated callbacks) can
        // inline references to this name as a literal. No-op for
        // non-const-foldable values like subscriptions or runtime
        // computations.
        if let Some(name) = b.pattern.single_bind() {
            crate::fusion::record_const_binding(
                name,
                &b.value,
                &mut ctx.fusion_known_consts,
            );
        }
        // Lazy-fusion registry: when value is a Lambda, register it
        // in `ctx.fusion_lambdas` keyed by binding name. The lazy
        // path (`fusion::lazy_resolve_kernel`) looks here when an
        // inner kernel's body calls this name. Build is on demand;
        // we just record the lambda + a NotAttempted cache slot
        // here.
        //
        // Skip registration when the binding is in
        // `ctx.unstable_bindings` (i.e. the name is the target of a
        // `<-` somewhere in the program). Cross-kernel `KirOp::Call`
        // bakes the callee in at fusion time; if the binding gets
        // rebound at runtime, fused callers would silently dispatch
        // into stale native code. With the binding skipped, callers
        // fail to lower the call as `KirOp::Call` and fall back to
        // GXLambda — correct, just slower. (When `KirOp::DynCall`
        // lands these become late-bound calls instead of fall-back.)
        if let (Some(name), ExprKind::Lambda(la)) =
            (b.pattern.single_bind(), &b.value.kind)
        {
            if !ctx.unstable_bindings.contains(name) {
                let entry = std::sync::Arc::new(crate::FusionLazyEntry {
                    fn_name: name.clone(),
                    spec_id: b.value.id,
                    lambda: la.clone(),
                    cache: parking_lot::Mutex::new(
                        crate::FusionLazyCache::NotAttempted,
                    ),
                    effect: parking_lot::Mutex::new(crate::effects::EffectKind::Sync),
                });
                ctx.fusion_lambdas.insert(name.clone(), entry);
            }
        }
        if pattern.is_refutable() {
            bailat!(spec, "refutable patterns are not allowed in let");
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
pub struct Ref {
    pub(super) spec: Arc<Expr>,
    pub(super) typ: Type,
    pub(super) id: BindId,
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

    fn typecheck(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct ByRef<R: Rt, E: UserEvent> {
    pub(super) spec: Expr,
    pub(super) typ: Type,
    pub(super) child: Node<R, E>,
    pub(super) id: BindId,
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

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck(ctx))?;
        let t = Type::ByRef(Arc::new(self.child.typ().clone()));
        wrap!(self, self.typ.check_contains(&ctx.env, &t))
    }
}

#[derive(Debug)]
pub struct Deref<R: Rt, E: UserEvent> {
    pub(super) spec: Expr,
    pub(super) typ: Type,
    pub(super) child: Node<R, E>,
    pub(super) id: Option<BindId>,
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
