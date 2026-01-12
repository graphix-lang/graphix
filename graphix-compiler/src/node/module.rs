use crate::{
    compiler::compile,
    env::Env,
    errf,
    expr::{
        parser, BindSig, Expr, ExprId, ExprKind, ModPath, Origin, Sandbox, Sig, SigKind,
        Source, StructurePattern, TypeDefExpr,
    },
    node::{bind::Bind, Nop},
    typ::Type,
    wrap, BindId, CFlag, Event, ExecCtx, Node, Refs, Rt, Scope, Update, UserEvent,
};
use anyhow::{bail, Context, Result};
use arcstr::{literal, ArcStr};
use compact_str::CompactString;
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{any::Any, mem, sync::LazyLock};
use triomphe::Arc;

fn bind_sig<R: Rt, E: UserEvent>(
    env: &mut Env<R, E>,
    mod_env: &mut Env<R, E>,
    scope: &Scope,
    sig: &Sig,
) -> Result<()> {
    env.modules.insert_cow(scope.lexical.clone());
    for si in sig.items.iter() {
        match &si.kind {
            SigKind::Module(name) => {
                let scope = scope.append(name);
                env.modules.insert_cow(scope.lexical.clone());
            }
            SigKind::Use(name) => {
                env.use_in_scope(scope, name)?;
                mod_env.use_in_scope(scope, name)?;
            }
            SigKind::Bind(BindSig { name, typ }) => {
                let typ = typ.scope_refs(&scope.lexical);
                typ.alias_tvars(&mut LPooled::take());
                env.bind_variable(&scope.lexical, name, typ);
            }
            SigKind::TypeDef(td) => {
                let typ = td.typ.scope_refs(&scope.lexical);
                env.deftype(&scope.lexical, &td.name, td.params.clone(), typ.clone())?;
                mod_env.deftype(&scope.lexical, &td.name, td.params.clone(), typ)?
            }
        }
    }
    Ok(())
}

// copy the exported signature of all the exported inner modules in this sig to
// the global env
fn export_sig<R: Rt, E: UserEvent>(
    env: &mut Env<R, E>,
    inner_env: &Env<R, E>,
    scope: &Scope,
    sig: &Sig,
) {
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            use std::fmt::Write;
            let mut buf: LPooled<String> = LPooled::take();
            let scope = scope.append(name);
            env.modules.insert_cow(scope.lexical.clone());
            macro_rules! copy_sig {
                ($kind:ident) => {
                    let iter = inner_env.$kind.range::<ModPath, _>(&scope.lexical..);
                    for (path, inner) in iter {
                        buf.clear();
                        write!(buf, "{}/", scope.lexical).unwrap();
                        if path == &scope.lexical || path.starts_with(&*buf) {
                            env.$kind.insert_cow(path.clone(), inner.clone());
                        }
                    }
                };
            }
            copy_sig!(binds);
            copy_sig!(typedefs);
        }
    }
}

fn check_sig<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    top_id: ExprId,
    proxy: &mut FxHashMap<BindId, BindId>,
    scope: &Scope,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let mut has_bind: LPooled<FxHashSet<ArcStr>> = LPooled::take();
    for n in nodes {
        if let Some(binds) = ctx.env.binds.get(&scope.lexical)
            && let Some(bind) = (&**n as &dyn Any).downcast_ref::<Bind<R, E>>()
            && let Expr { kind: ExprKind::Bind(bexp), .. } = bind.spec()
            && let StructurePattern::Bind(name) = &bexp.pattern
            && let Some(id) = bind.single_id()
            && let Some(proxy_id) = binds.get(&CompactString::from(name.as_str()))
            && let Some(proxy_bind) = ctx.env.by_id.get(&proxy_id)
        {
            proxy_bind.typ.unbind_tvars();
            if !(proxy_bind.typ.contains(&ctx.env, bind.typ())?
                && bind.typ().contains(&ctx.env, &proxy_bind.typ)?)
            {
                bail!(
                    "signature mismatch \"val {name}: ...\", signature has type {}, implementation has type {}",
                    proxy_bind.typ,
                    bind.typ()
                )
            }
            proxy.insert(id, *proxy_id);
            ctx.rt.ref_var(id, top_id);
            ctx.rt.ref_var(*proxy_id, top_id);
            has_bind.insert(name.clone());
        }
        if let Expr { kind: ExprKind::TypeDef(td), .. } = n.spec()
            && let Some(defs) = ctx.env.typedefs.get(&scope.lexical)
            && let Some(sig_td) = defs.get(&CompactString::from(td.name.as_str()))
        {
            let sig_td = TypeDefExpr {
                name: td.name.clone(),
                params: sig_td.params.clone(),
                typ: sig_td.typ.clone(),
            };
            if td != &sig_td {
                bail!(
                    "signature mismatch in {}, expected {}, found {}",
                    td.name,
                    sig_td,
                    td
                )
            }
        }
    }
    for si in sig.items.iter() {
        let missing = match &si.kind {
            SigKind::Bind(BindSig { name, .. }) => !has_bind.contains(name),
            SigKind::Module(_)
            | SigKind::Use(_)
            | SigKind::TypeDef(TypeDefExpr { .. }) => false,
        };
        if missing {
            bail!("missing required sig item {si}")
        }
    }
    Ok(())
}

static ERR_TAG: ArcStr = literal!("DynamicLoadError");
static TYP: LazyLock<Type> = LazyLock::new(|| {
    let t = Arc::from_iter([Type::Primitive(Typ::String.into())]);
    let err = Type::Error(Arc::new(Type::Variant(ERR_TAG.clone(), t)));
    Type::Set(Arc::from_iter([err, Type::Primitive(Typ::Null.into())]))
});

#[derive(Debug)]
pub(super) struct Module<R: Rt, E: UserEvent> {
    spec: Expr,
    flags: BitFlags<CFlag>,
    source: Node<R, E>,
    env: Env<R, E>,
    sig: Sig,
    scope: Scope,
    proxy: FxHashMap<BindId, BindId>,
    nodes: Box<[Node<R, E>]>,
    is_static: bool,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> Module<R, E> {
    pub(super) fn compile_dynamic(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        sandbox: Sandbox,
        sig: Sig,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, (*source).clone(), scope, top_id)?;
        let mut env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        bind_sig(&mut ctx.env, &mut env, &scope, &sig)
            .context("binding module signature")?;
        Ok(Box::new(Self {
            spec,
            flags,
            env,
            sig,
            source,
            scope: scope.clone(),
            proxy: FxHashMap::default(),
            nodes: Box::new([]),
            is_static: false,
            top_id,
        }))
    }

    pub(super) fn compile_static(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        sig: Sig,
        exprs: Arc<[Expr]>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let source = Nop::new(Type::Primitive(Typ::String | Typ::Error));
        let mut env = ctx.env.clone();
        bind_sig(&mut ctx.env, &mut env, &scope, &sig)
            .with_context(|| format!("binding signature for module {}", scope.lexical))?;
        let mut t = Self {
            spec,
            flags,
            env,
            sig,
            source,
            scope: scope.clone(),
            proxy: FxHashMap::default(),
            nodes: Box::new([]),
            is_static: true,
            top_id,
        };
        t.compile_inner(ctx, &exprs)
            .with_context(|| format!("compiling module {}", scope.lexical))?;
        Ok(Box::new(t))
    }

    fn compile_source(&mut self, ctx: &mut ExecCtx<R, E>, text: ArcStr) -> Result<()> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = parser::parse(ori)?;
        self.compile_inner(ctx, &exprs)
    }

    fn compile_inner(&mut self, ctx: &mut ExecCtx<R, E>, exprs: &[Expr]) -> Result<()> {
        ctx.builtins_allowed = self.is_static;
        let nodes = ctx.with_restored_mut(&mut self.env, |ctx| -> Result<_> {
            let mut nodes = exprs
                .iter()
                .map(|e| compile(ctx, self.flags, e.clone(), &self.scope, self.top_id))
                .collect::<Result<Vec<_>>>()?;
            for n in &mut nodes {
                n.typecheck(ctx)?
            }
            Ok(nodes)
        });
        ctx.builtins_allowed = true;
        let nodes = nodes?;
        check_sig(ctx, self.top_id, &mut self.proxy, &self.scope, &self.sig, &nodes)?;
        self.nodes = Box::from(nodes);
        export_sig(&mut ctx.env, &self.env, &self.scope, &self.sig);
        Ok(())
    }

    fn clear_compiled(&mut self, ctx: &mut ExecCtx<R, E>) {
        for (id, proxy_id) in self.proxy.drain() {
            ctx.rt.unref_var(id, self.top_id);
            ctx.rt.unref_var(proxy_id, self.top_id);
        }
        ctx.with_restored(self.env.clone(), |ctx| {
            for mut n in mem::take(&mut self.nodes) {
                n.delete(ctx)
            }
        })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Module<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<netidx_value::Value> {
        let mut compiled = false;
        if !self.is_static
            && let Some(v) = self.source.update(ctx, event)
        {
            self.clear_compiled(ctx);
            match v {
                Value::String(s) => {
                    if let Err(e) = self.compile_source(ctx, s) {
                        return Some(errf!(ERR_TAG, "compile error {e:?}"));
                    }
                }
                v => return Some(errf!(ERR_TAG, "unexpected {v}")),
            }
            compiled = true;
        }
        let init = event.init;
        if compiled {
            event.init = true;
        }
        for (inner_id, proxy_id) in &self.proxy {
            if let Some(v) = event.variables.get(proxy_id) {
                let v = v.clone();
                event.variables.insert(*inner_id, v.clone());
                ctx.cached.insert(*inner_id, v);
            }
        }
        let res = self.nodes.iter_mut().fold(None, |_, n| n.update(ctx, event));
        event.init = init;
        for (inner_id, proxy_id) in &self.proxy {
            if let Some(v) = event.variables.remove(inner_id) {
                event.variables.insert(*proxy_id, v.clone());
                ctx.cached.insert(*proxy_id, v);
            }
        }
        if self.is_static {
            res
        } else {
            compiled.then(|| Value::Null)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if self.is_static {
            ctx.with_restored(self.env.clone(), |ctx| {
                for n in &mut self.nodes {
                    n.delete(ctx);
                }
            });
        } else {
            self.source.delete(ctx);
            self.clear_compiled(ctx);
        }
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        for n in &self.nodes {
            n.refs(refs)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if self.is_static {
            ctx.with_restored(self.env.clone(), |ctx| {
                for n in &mut self.nodes {
                    n.sleep(ctx);
                }
            });
        } else {
            self.source.sleep(ctx);
            self.clear_compiled(ctx);
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        if self.is_static {
            self.nodes.last().map(|n| n.typ()).unwrap_or(&Type::Bottom)
        } else {
            &TYP
        }
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck(ctx))?;
        let t = Type::Primitive(Typ::String | Typ::Error);
        wrap!(self.source, t.check_contains(&self.env, self.source.typ()))
    }
}
