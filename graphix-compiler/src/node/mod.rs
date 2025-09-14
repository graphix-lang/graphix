use crate::{
    env::{self, Env},
    expr::{self, Expr, ExprId, ExprKind, ModPath},
    format_with_flags,
    typ::{TVal, TVar, Type},
    wrap, BindId, Event, ExecCtx, Node, PrintFlag, Refs, Rt, Update, UserEvent,
};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use compiler::compile;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use pattern::StructPatternNode;
use std::{cell::RefCell, sync::LazyLock};
use triomphe::Arc;

pub(crate) mod array;
pub(crate) mod callsite;
pub(crate) mod compiler;
pub(crate) mod data;
pub(crate) mod dynamic;
pub mod genn;
pub(crate) mod lambda;
pub(crate) mod op;
pub(crate) mod pattern;
pub(crate) mod select;

#[macro_export]
macro_rules! wrap {
    ($n:expr, $e:expr) => {
        match $e {
            Ok(x) => Ok(x),
            e => {
                anyhow::Context::context(e, $crate::expr::ErrorContext($n.spec().clone()))
            }
        }
    };
}

#[macro_export]
macro_rules! update_args {
    ($args:expr, $ctx:expr, $event:expr) => {{
        let mut updated = false;
        let mut determined = true;
        for n in $args.iter_mut() {
            updated |= n.update($ctx, $event);
            determined &= n.cached.is_some();
        }
        (updated, determined)
    }};
}

static NOP: LazyLock<Arc<Expr>> = LazyLock::new(|| {
    Arc::new(
        ExprKind::Constant(Value::String(literal!("nop"))).to_expr(Default::default()),
    )
});

#[derive(Debug)]
pub(crate) struct Nop {
    pub typ: Type,
}

impl Nop {
    pub(crate) fn new<R: Rt, E: UserEvent>(typ: Type) -> Node<R, E> {
        Box::new(Nop { typ })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Nop {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _event: &mut Event<E>,
    ) -> Option<Value> {
        None
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typecheck(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &NOP
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, _refs: &mut Refs) {}
}

#[derive(Debug)]
struct Cached<R: Rt, E: UserEvent> {
    cached: Option<Value>,
    node: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Cached<R, E> {
    fn new(node: Node<R, E>) -> Self {
        Self { cached: None, node }
    }

    /// update the node, return whether the node updated. If it did,
    /// the updated value will be stored in the cached field, if not,
    /// the previous value will remain there.
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> bool {
        match self.node.update(ctx, event) {
            None => false,
            Some(v) => {
                self.cached = Some(v);
                true
            }
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.cached = None;
        self.node.sleep(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct Use {
    spec: Expr,
    scope: ModPath,
    name: ModPath,
}

impl Use {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        match ctx.env.canonical_modpath(scope, name) {
            None => bail!("at {} no such module {name}", spec.pos),
            Some(_) => {
                let used = ctx.env.used.get_or_default_cow(scope.clone());
                Arc::make_mut(used).push(name.clone());
                Ok(Box::new(Self { spec, scope: scope.clone(), name: name.clone() }))
            }
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Use {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _event: &mut Event<E>,
    ) -> Option<Value> {
        None
    }

    fn typecheck(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(used) = ctx.env.used.get_mut_cow(&self.scope) {
            Arc::make_mut(used).retain(|n| n != &self.name);
            if used.is_empty() {
                ctx.env.used.remove_cow(&self.scope);
            }
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &Type::Bottom
    }
}

#[derive(Debug)]
pub(crate) struct TypeDef {
    spec: Expr,
    scope: ModPath,
    name: ArcStr,
}

impl TypeDef {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        name: &ArcStr,
        params: &Arc<[(TVar, Option<Type>)]>,
        typ: &Type,
    ) -> Result<Node<R, E>> {
        let typ = typ.scope_refs(scope);
        ctx.env
            .deftype(scope, name, params.clone(), typ)
            .with_context(|| format!("in typedef at {}", spec.pos))?;
        let name = name.clone();
        let scope = scope.clone();
        Ok(Box::new(Self { spec, scope, name }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeDef {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _event: &mut Event<E>,
    ) -> Option<Value> {
        None
    }

    fn typecheck(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.env.undeftype(&self.scope, &self.name)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &Type::Bottom
    }
}

#[derive(Debug)]
pub(crate) struct Constant {
    spec: Arc<Expr>,
    value: Value,
    typ: Type,
}

impl Constant {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        spec: Expr,
        value: &Value,
    ) -> Result<Node<R, E>> {
        let spec = Arc::new(spec);
        let value = value.clone();
        let typ = Type::Primitive(Typ::get(&value).into());
        Ok(Box::new(Self { spec, value, typ }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Constant {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        if event.init {
            Some(self.value.clone())
        } else {
            None
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn refs(&self, _refs: &mut Refs) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }
}

// used for both mod and do
#[derive(Debug)]
pub(crate) struct Block<R: Rt, E: UserEvent> {
    module: bool,
    spec: Expr,
    children: Box<[Node<R, E>]>,
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        module: bool,
        exprs: &Arc<[Expr]>,
    ) -> Result<Node<R, E>> {
        let children = exprs
            .iter()
            .map(|e| compile(ctx, e.clone(), scope, top_id))
            .collect::<Result<Box<[Node<R, E>]>>>()?;
        Ok(Box::new(Self { module, spec, children }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Block<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        self.children.iter_mut().fold(None, |_, n| n.update(ctx, event))
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.children {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.children {
            n.sleep(ctx)
        }
    }

    fn refs(&self, refs: &mut Refs) {
        for n in &self.children {
            n.refs(refs)
        }
    }

    fn typ(&self) -> &Type {
        &self.children.last().map(|n| n.typ()).unwrap_or(&Type::Bottom)
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in &mut self.children {
            if self.module {
                wrap!(n, n.typecheck(ctx)).with_context(|| self.spec.ori.clone())?
            } else {
                wrap!(n, n.typecheck(ctx))?
            }
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }
}

#[derive(Debug)]
pub(crate) struct Bind<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    pattern: StructPatternNode,
    node: Node<R, E>,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        b: &expr::Bind,
    ) -> Result<Node<R, E>> {
        let expr::Bind { rec, doc, pattern, typ, export: _, value } = b;
        let (node, pattern, typ) = if *rec {
            if !pattern.single_bind().is_some() {
                bail!("at {} can't use rec on a complex pattern", spec.pos)
            }
            match value {
                Expr { kind: ExprKind::Lambda(_), .. } => (),
                _ => bail!("let rec may only be used for lambdas"),
            }
            let typ = match typ {
                Some(typ) => typ.scope_refs(scope),
                None => Type::empty_tvar(),
            };
            let pattern = StructPatternNode::compile(ctx, &typ, pattern, scope)
                .with_context(|| format!("at {}", spec.pos))?;
            let node = compile(ctx, value.clone(), &scope, top_id)?;
            let ntyp = node.typ();
            if !typ.contains(&ctx.env, ntyp)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!("at {} error {} can't be matched by {typ}", ntyp, spec.pos)
                })?
            }
            (node, pattern, typ)
        } else {
            let node = compile(ctx, value.clone(), &scope, top_id)?;
            let typ = match typ {
                Some(typ) => typ.scope_refs(scope),
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
            let pattern = StructPatternNode::compile(ctx, &typ, pattern, scope)
                .with_context(|| format!("at {}", spec.pos))?;
            (node, pattern, typ)
        };
        if pattern.is_refutable() {
            bail!("at {} refutable patterns are not allowed in let", spec.pos);
        }
        if let Some(doc) = doc {
            pattern.ids(&mut |id| {
                if let Some(b) = ctx.env.by_id.get_mut_cow(&id) {
                    b.doc = Some(doc.clone());
                }
            });
        }
        Ok(Box::new(Self { spec, typ, pattern, node, top_id }))
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
                if self.spec.id == self.top_id {
                    ctx.rt.notify_set(id);
                }
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
    spec: Arc<Expr>,
    typ: Type,
    id: BindId,
    top_id: ExprId,
}

impl Ref {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        match ctx.env.lookup_bind(scope, name) {
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
pub(crate) struct StringInterpolate<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    typs: Box<[Type]>,
    args: Box<[Cached<R, E>]>,
}

impl<R: Rt, E: UserEvent> StringInterpolate<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let args: Box<[Cached<R, E>]> = args
            .iter()
            .map(|e| Ok(Cached::new(compile(ctx, e.clone(), scope, top_id)?)))
            .collect::<Result<_>>()?;
        let typs = args.iter().map(|c| c.node.typ().clone()).collect();
        let typ = Type::Primitive(Typ::String.into());
        Ok(Box::new(Self { spec, typ, typs, args }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StringInterpolate<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        use std::fmt::Write;
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }
        let (updated, determined) = update_args!(self.args, ctx, event);
        if updated && determined {
            BUF.with_borrow_mut(|buf| {
                buf.clear();
                for (typ, c) in self.typs.iter().zip(self.args.iter()) {
                    match c.cached.as_ref().unwrap() {
                        Value::String(s) => write!(buf, "{s}"),
                        v => write!(buf, "{}", TVal { env: &ctx.env, typ, v }),
                    }
                    .unwrap()
                }
                Some(Value::String(buf.as_str().into()))
            })
        } else {
            None
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        for a in &self.args {
            a.node.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.args {
            n.node.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.args {
            n.sleep(ctx);
        }
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for (i, a) in self.args.iter_mut().enumerate() {
            wrap!(a.node, a.node.typecheck(ctx))?;
            self.typs[i] = a.node.typ().with_deref(|t| match t {
                None => Type::Any,
                Some(t) => t.clone(),
            });
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Connect<R: Rt, E: UserEvent> {
    spec: Expr,
    node: Node<R, E>,
    id: BindId,
}

impl<R: Rt, E: UserEvent> Connect<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let id = match ctx.env.lookup_bind(scope, name) {
            None => bail!("at {} {name} is undefined", spec.pos),
            Some((_, env::Bind { id, .. })) => *id,
        };
        let node = compile(ctx, value.clone(), scope, top_id)?;
        Ok(Box::new(Self { spec, node, id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Connect<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if let Some(v) = self.node.update(ctx, event) {
            ctx.set_var(self.id, v)
        }
        None
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn refs(&self, refs: &mut Refs) {
        self.node.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck(ctx))?;
        let bind = match ctx.env.by_id.get(&self.id) {
            None => bail!("BUG missing bind {:?}", self.id),
            Some(bind) => bind,
        };
        wrap!(self, bind.typ.check_contains(&ctx.env, self.node.typ()))
    }
}

#[derive(Debug)]
pub(crate) struct ConnectDeref<R: Rt, E: UserEvent> {
    spec: Expr,
    rhs: Cached<R, E>,
    src_id: BindId,
    target_id: Option<BindId>,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let src_id = match ctx.env.lookup_bind(scope, name) {
            None => bail!("at {} {name} is undefined", spec.pos),
            Some((_, env::Bind { id, .. })) => *id,
        };
        ctx.rt.ref_var(src_id, top_id);
        let rhs = Cached::new(compile(ctx, value.clone(), scope, top_id)?);
        Ok(Box::new(Self { spec, rhs, src_id, target_id: None, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ConnectDeref<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let mut up = self.rhs.update(ctx, event);
        if let Some(Value::U64(id)) = event.variables.get(&self.src_id) {
            if let Some(target_id) = ctx.env.byref_chain.get(&BindId::from(*id)) {
                self.target_id = Some(*target_id);
                up = true;
            }
        }
        if up {
            if let Some(v) = &self.rhs.cached {
                if let Some(id) = self.target_id {
                    ctx.set_var(id, v.clone())
                }
            }
        }
        None
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn refs(&self, refs: &mut Refs) {
        refs.refed.insert(self.src_id);
        self.rhs.node.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.src_id, self.top_id);
        self.rhs.node.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.rhs.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.rhs.node, self.rhs.node.typecheck(ctx))?;
        let bind = match ctx.env.by_id.get(&self.src_id) {
            None => bail!("BUG missing bind {:?}", self.src_id),
            Some(bind) => bind,
        };
        let typ = Type::ByRef(Arc::new(self.rhs.node.typ().clone()));
        wrap!(self, bind.typ.check_contains(&ctx.env, &typ))
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
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let child = compile(ctx, expr.clone(), scope, top_id)?;
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
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let child = compile(ctx, expr.clone(), scope, top_id)?;
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
        self.id.and_then(|id| event.variables.get(&id).cloned())
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

fn typ_echain(param: Type) -> Type {
    Type::Ref {
        scope: ModPath::root(),
        name: ModPath::from(["ErrChain"]),
        params: Arc::from_iter([param]),
    }
}

fn wrap_error<R: Rt, E: UserEvent>(env: &Env<R, E>, spec: &Expr, e: Value) -> Value {
    static ERRCHAIN: LazyLock<Type> = LazyLock::new(|| typ_echain(Type::empty_tvar()));
    let pos: Value =
        [(literal!("column"), spec.pos.column), (literal!("line"), spec.pos.line)].into();
    if ERRCHAIN.is_a(env, &e) {
        let error = e.clone().cast_to::<[(ArcStr, Value); 4]>().unwrap();
        let error = error[1].1.clone();
        [
            (literal!("cause"), e.clone()),
            (literal!("error"), error),
            (literal!("ori"), spec.ori.to_value()),
            (literal!("pos"), pos),
        ]
        .into()
    } else {
        [
            (literal!("cause"), Value::Null),
            (literal!("error"), e.clone()),
            (literal!("ori"), spec.ori.to_value()),
            (literal!("pos"), pos),
        ]
        .into()
    }
}

#[derive(Debug)]
pub(crate) struct Qop<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    id: BindId,
    n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Qop<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, e.clone(), scope, top_id)?;
        let id = ctx.env.lookup_catch(scope)?;
        let typ = Type::empty_tvar();
        Ok(Box::new(Self { spec, typ, id, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Qop<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        match self.n.update(ctx, event) {
            None => None,
            Some(Value::Error(e)) => {
                let e = wrap_error(&ctx.env, &self.spec, (*e).clone());
                ctx.set_var(self.id, Value::Error(Arc::new(e)));
                None
            }
            Some(v) => Some(v),
        }
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck(ctx))?;
        let err = Type::Error(Arc::new(Type::empty_tvar()));
        if !self.n.typ().contains(&ctx.env, &err)? {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!("cannot use the ? operator on non error type {}", self.n.typ())
            })?
        }
        let err = Type::Primitive(Typ::Error.into());
        let rtyp = self.n.typ().diff(&ctx.env, &err)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        let bind = ctx
            .env
            .by_id
            .get(&self.id)
            .ok_or_else(|| anyhow!("BUG: missing catch id"))?;
        let etyp = self.n.typ().diff(&ctx.env, &rtyp)?;
        let etyp = typ_echain(wrap!(
            self.n,
            etyp.strip_error(&ctx.env)
                .ok_or_else(|| anyhow!("expected an error got {etyp}"))
        )?);
        wrap!(self, bind.typ.check_contains(&ctx.env, &etyp))?;
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct TypeCast<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    target: Type,
    n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> TypeCast<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        expr: &Expr,
        typ: &Type,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, expr.clone(), scope, top_id)?;
        let target = typ.scope_refs(scope);
        if let Err(e) = target.check_cast(&ctx.env) {
            bail!("in cast at {} {e}", spec.pos);
        }
        let typ = target.union(&ctx.env, &Type::Primitive(Typ::Error.into()))?;
        Ok(Box::new(Self { spec, typ, target, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeCast<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        self.n.update(ctx, event).map(|v| self.target.cast_value(&ctx.env, v))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(wrap!(self.n, self.n.typecheck(ctx))?)
    }
}

#[derive(Debug)]
pub(crate) struct Any<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    n: Box<[Node<R, E>]>,
}

impl<R: Rt, E: UserEvent> Any<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        let typ =
            Type::Set(Arc::from_iter(n.iter().map(|n| n.typ().clone()))).normalize();
        Ok(Box::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Any<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        self.n
            .iter_mut()
            .filter_map(|s| s.update(ctx, event))
            .fold(None, |r, v| r.or(Some(v)))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck(ctx))?
        }
        let rtyp = Type::Primitive(BitFlags::empty());
        let rtyp = wrap!(
            self,
            self.n.iter().fold(Ok(rtyp), |rtype, n| n.typ().union(&ctx.env, &rtype?))
        )?;
        Ok(self.typ.check_contains(&ctx.env, &rtyp)?)
    }
}

#[derive(Debug)]
struct Sample<R: Rt, E: UserEvent> {
    spec: Expr,
    triggered: usize,
    typ: Type,
    id: BindId,
    top_id: ExprId,
    trigger: Node<R, E>,
    arg: Cached<R, E>,
}

impl<R: Rt, E: UserEvent> Sample<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
    ) -> Result<Node<R, E>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let trigger = compile(ctx, (**lhs).clone(), scope, top_id)?;
        let arg = Cached::new(compile(ctx, (**rhs).clone(), scope, top_id)?);
        let typ = arg.node.typ().clone();
        Ok(Box::new(Self { triggered: 0, id, top_id, spec, typ, trigger, arg }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Sample<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if let Some(_) = self.trigger.update(ctx, event) {
            self.triggered += 1;
        }
        self.arg.update(ctx, event);
        let var = event.variables.get(&self.id).cloned();
        let res = if self.triggered > 0 && self.arg.cached.is_some() && var.is_none() {
            self.triggered -= 1;
            self.arg.cached.clone()
        } else {
            var
        };
        if self.arg.cached.is_some() {
            while self.triggered > 0 {
                self.triggered -= 1;
                ctx.rt.set_var(self.id, self.arg.cached.clone().unwrap());
            }
        }
        res
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.arg.node.delete(ctx);
        self.trigger.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.arg.sleep(ctx);
        self.trigger.sleep(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        refs.refed.insert(self.id);
        self.arg.node.refs(refs);
        self.trigger.refs(refs);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.trigger, self.trigger.typecheck(ctx))?;
        wrap!(self.arg.node, self.arg.node.typecheck(ctx))
    }
}

#[derive(Debug)]
pub(crate) struct Catch<R: Rt, E: UserEvent> {
    spec: Expr,
    handler: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Catch<R, E> {
    pub(crate) fn new(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        top_id: ExprId,
        bind: &ArcStr,
        constraint: &Option<Type>,
        handler: &Arc<Expr>,
    ) -> Result<Node<R, E>> {
        let name = format_compact!("ca{}", BindId::new().inner());
        let inner_scope = ModPath(scope.append(name.as_str()));
        let typ = match constraint {
            Some(t) => t.clone(),
            None => Type::empty_tvar(),
        };
        let id = ctx.env.bind_variable(&inner_scope, bind, typ).id;
        let handler = compile(ctx, (**handler).clone(), &inner_scope, top_id)?;
        ctx.env.catch.insert_cow(scope.clone(), id);
        Ok(Box::new(Self { spec, handler }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Catch<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let _ = self.handler.update(ctx, event);
        None
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.handler.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.handler, self.handler.typecheck(ctx))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn refs(&self, refs: &mut Refs) {
        self.handler.refs(refs);
    }
}
