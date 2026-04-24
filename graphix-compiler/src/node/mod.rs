use crate::{
    expr::{Expr, ExprId, ExprKind, ModPath},
    typ::{TVal, TVar, Type},
    BindId, CFlag, Event, ExecCtx, Node, Refs, Rt, Scope, Update, UserEvent, CAST_ERR,
};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::{literal, ArcStr};
use compiler::compile;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use std::{cell::RefCell, sync::LazyLock};
use triomphe::Arc;

pub(crate) mod array;
pub(crate) mod bind;
pub(crate) mod callsite;
pub(crate) mod compiler;
pub(crate) mod data;
pub(crate) mod error;
pub mod genn;
pub mod lambda;
pub(crate) mod map;
pub(crate) mod module;
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

#[macro_export]
macro_rules! deref_typ {
    ($name:literal, $ctx:expr, $typ:expr, $($pat:pat => $body:expr),+) => {
        $typ.with_deref(|typ| {
            let mut typ = typ.cloned();
            let mut hist: poolshark::local::LPooled<fxhash::FxHashSet<usize>> = poolshark::local::LPooled::take();
            loop {
                #[allow(unreachable_patterns)]
                match &typ {
                    $($pat => break $body),+,
                    Some(rt @ $crate::typ::Type::Ref($crate::typ::TypeRef { .. })) => {
                        let rt = rt.lookup_ref(&$ctx.env)?;
                        if hist.insert(&rt as *const _ as usize) {
                            typ = Some(rt);
                        } else {
                            $crate::format_with_flags(PrintFlag::DerefTVars, || {
                                anyhow::bail!("expected {} not {rt}", $name)
                            })?
                        }
                    }
                    Some(t) => $crate::format_with_flags(PrintFlag::DerefTVars, || {
                        anyhow::bail!("expected {} not {t}", $name)
                    })?,
                    None => anyhow::bail!("type must be known, annotations needed")
                }
            }
        })
    };
}

pub(crate) static NOP: LazyLock<Arc<Expr>> = LazyLock::new(|| {
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
pub(crate) struct ExplicitParens<R: Rt, E: UserEvent> {
    spec: Expr,
    n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> ExplicitParens<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, spec.clone(), scope, top_id)?;
        Ok(Box::new(ExplicitParens { spec, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ExplicitParens<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        self.n.update(ctx, event)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck(ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.n.typ()
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
    }
}

/// Wraps a child `Node` with its last-produced value cached for the
/// current cycle. Many op nodes use this so that if one operand
/// updates and another doesn't, they can still produce output using
/// the previous cached value for the unchanged operand.
///
/// Public so that AOT-generated code (see `graphix compile`, item 3
/// in the plan) can construct op-node trees directly. Don't rely on
/// the exact shape of this type — it's public for codegen reuse, not
/// as a stable user-facing API.
#[derive(Debug)]
pub struct Cached<R: Rt, E: UserEvent> {
    pub cached: Option<Value>,
    pub node: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Cached<R, E> {
    pub fn new(node: Node<R, E>) -> Self {
        Self { cached: None, node }
    }

    /// update the node, return whether the node updated. If it did,
    /// the updated value will be stored in the cached field, if not,
    /// the previous value will remain there.
    pub fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> bool {
        match self.node.update(ctx, event) {
            None => false,
            Some(v) => {
                self.cached = Some(v);
                true
            }
        }
    }

    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.cached = None;
        self.node.sleep(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct Use {
    spec: Expr,
    scope: Scope,
    name: ModPath,
}

impl Use {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        ctx.env
            .use_in_scope(scope, name)
            .map_err(|e| anyhow!("at {} {e:?}", spec.pos))?;
        // Record the `use foo;` site for IDE tooling so find-references
        // and go-to-definition on a module name return all known import
        // sites and the file the module lives in.
        let canonical = ctx
            .env
            .canonical_modpath(&scope.lexical, name)
            .unwrap_or_else(|| name.clone());
        ctx.module_references.push(crate::ModuleRefSite {
            pos: spec.pos,
            ori: spec.ori.clone(),
            name: name.clone(),
            canonical,
            def_ori: None,
        });
        Ok(Box::new(Self { spec, scope: scope.clone(), name: name.clone() }))
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
        ctx.env.stop_use_in_scope(&self.scope, &self.name);
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
        scope: &Scope,
        name: &ArcStr,
        params: &Arc<[(TVar, Option<Type>)]>,
        typ: &Type,
    ) -> Result<Node<R, E>> {
        let typ = typ.scope_refs(&scope.lexical);
        ctx.env
            .deftype(
                &scope.lexical,
                name,
                params.clone(),
                typ,
                None,
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| format!("in typedef at {}", spec.pos))?;
        let name = name.clone();
        Ok(Box::new(Self { spec, scope: scope.lexical.clone(), name }))
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
pub struct Constant {
    pub(super) spec: Arc<Expr>,
    pub(super) value: Value,
    pub(super) typ: Type,
}

impl Constant {
    /// Construct a `Constant` node from its final components. AOT-
    /// generated code uses this after it has already chosen the
    /// value, type, and spec at code-generation time.
    pub fn new<R: Rt, E: UserEvent>(value: Value, typ: Type, spec: Expr) -> Node<R, E> {
        Box::new(Self { spec: Arc::new(spec), value, typ })
    }

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
pub struct Block<R: Rt, E: UserEvent> {
    pub(super) module: bool,
    pub(super) spec: Expr,
    pub(super) children: Box<[Node<R, E>]>,
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// Build a `Block` / `do` node from an already-compiled list of
    /// child expressions. `module` selects "module" semantics (no
    /// returned value) vs "do" semantics (the last child's value is
    /// the block's value).
    pub fn new(
        module: bool,
        children: Box<[Node<R, E>]>,
        spec: Expr,
    ) -> Node<R, E> {
        Box::new(Self { module, spec, children })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        module: bool,
        exprs: &Arc<[Expr]>,
    ) -> Result<Node<R, E>> {
        let children = exprs
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[Node<R, E>]>>>()?;
        Ok(Box::new(Self { module, spec, children }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Block<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let res = self.children.iter_mut().fold(None, |_, n| n.update(ctx, event));
        if self.module {
            None
        } else {
            res
        }
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
pub(crate) struct StringInterpolate<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    typs: Box<[Type]>,
    args: Box<[Cached<R, E>]>,
}

impl<R: Rt, E: UserEvent> StringInterpolate<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let args: Box<[Cached<R, E>]> = args
            .iter()
            .map(|e| Ok(Cached::new(compile(ctx, flags, e.clone(), scope, top_id)?)))
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
pub struct Connect<R: Rt, E: UserEvent> {
    pub(super) spec: Expr,
    pub(super) node: Node<R, E>,
    pub(super) id: BindId,
}

impl<R: Rt, E: UserEvent> Connect<R, E> {
    /// Build a `Connect` node from an already-compiled RHS expression
    /// and the BindId of the variable to be updated on each cycle.
    pub fn new(id: BindId, rhs: Node<R, E>, spec: Expr) -> Node<R, E> {
        Box::new(Self { spec, node: rhs, id })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let (id, def_pos, def_ori) = match ctx.env.lookup_bind(&scope.lexical, name)
        {
            None => bail!("at {} {name} is undefined", spec.pos),
            Some((_, b)) => (b.id, b.pos, b.ori.clone()),
        };
        ctx.references.push(crate::ReferenceSite {
            pos: spec.pos,
            ori: spec.ori.clone(),
            name: name.clone(),
            bind_id: id,
            def_pos,
            def_ori,
        });
        let node = compile(ctx, flags, value.clone(), scope, top_id)?;
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
pub struct ConnectDeref<R: Rt, E: UserEvent> {
    pub(super) spec: Expr,
    pub(super) rhs: Cached<R, E>,
    pub(super) src_id: BindId,
    pub(super) target_id: Option<BindId>,
    pub(super) top_id: ExprId,
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
    /// Build a `ConnectDeref` from an already-compiled RHS node and
    /// the source reference's BindId. The caller is responsible for
    /// registering the reference with the runtime (via
    /// `ctx.rt.ref_var(src_id, top_id)`).
    pub fn new(
        src_id: BindId,
        rhs: Node<R, E>,
        top_id: ExprId,
        spec: Expr,
    ) -> Node<R, E> {
        Box::new(Self {
            spec,
            rhs: Cached::new(rhs),
            src_id,
            target_id: None,
            top_id,
        })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let (src_id, def_pos, def_ori) =
            match ctx.env.lookup_bind(&scope.lexical, name) {
                None => bail!("at {} {name} is undefined", spec.pos),
                Some((_, b)) => (b.id, b.pos, b.ori.clone()),
            };
        ctx.references.push(crate::ReferenceSite {
            pos: spec.pos,
            ori: spec.ori.clone(),
            name: name.clone(),
            bind_id: src_id,
            def_pos,
            def_ori,
        });
        ctx.rt.ref_var(src_id, top_id);
        let rhs = Cached::new(compile(ctx, flags, value.clone(), scope, top_id)?);
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
                    ctx.set_var(id, v.clone());
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
pub(crate) struct TypeCast<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    target: Type,
    n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> TypeCast<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
        typ: &Type,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let target = typ.scope_refs(&scope.lexical);
        if let Err(e) = target.check_cast(&ctx.env) {
            bail!("in cast at {} {e}", spec.pos);
        }
        let typ = target.union(&ctx.env, &CAST_ERR)?;
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
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        Ok(Box::new(Self { spec, typ: Type::empty_tvar(), n }))
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
        let rtyp = Type::Bottom;
        let rtyp = wrap!(
            self,
            self.n.iter().fold(Ok(rtyp), |rtype, n| rtype?.union(&ctx.env, n.typ()))
        )?;
        let rtyp = if rtyp == Type::Bottom { Type::empty_tvar() } else { rtyp };
        self.typ.check_contains(&ctx.env, &rtyp)?;
        Ok(())
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
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
    ) -> Result<Node<R, E>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let trigger = compile(ctx, flags, (**lhs).clone(), scope, top_id)?;
        let arg = Cached::new(compile(ctx, flags, (**rhs).clone(), scope, top_id)?);
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
