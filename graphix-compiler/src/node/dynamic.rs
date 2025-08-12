use crate::{
    compiler::compile,
    env::Env,
    expr::{
        parser, Expr, ExprId, ExprKind, ModPath, Origin, Sandbox, Sig, SigItem, Source,
        StructurePattern, TypeDef,
    },
    node::{Bind, Block},
    typ::Type,
    wrap, BindId, Event, ExecCtx, Node, Refs, Rt, Update, UserEvent, KNOWN,
};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use compact_str::CompactString;
use fxhash::{FxHashMap, FxHashSet};
use netidx_value::{Typ, Value};
use std::{any::Any, mem};
use triomphe::Arc;

fn bind_sig<R: Rt, E: UserEvent>(
    env: &mut Env<R, E>,
    scope: &ModPath,
    sig: &Sig,
) -> Result<()> {
    env.modules.insert_cow(scope.clone());
    for si in sig.iter() {
        match si {
            SigItem::Bind(name, typ) => {
                KNOWN.with_borrow_mut(|known| typ.alias_tvars(known));
                env.bind_variable(&scope, name, typ.clone());
            }
            SigItem::TypeDef(td) => {
                env.deftype(&scope, &td.name, td.params.clone(), td.typ.clone())?
            }
            SigItem::Module(name, sig) => {
                let scope = ModPath(scope.append(&name));
                bind_sig(env, &scope, sig)?
            }
        }
    }
    Ok(())
}

fn check_sig<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    top_id: ExprId,
    proxy: &mut FxHashMap<BindId, BindId>,
    scope: &ModPath,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let mut has_bind: FxHashSet<ArcStr> = FxHashSet::default();
    let mut has_mod: FxHashSet<ArcStr> = FxHashSet::default();
    let mut has_def: FxHashSet<ArcStr> = FxHashSet::default();
    for n in nodes {
        if let Some(binds) = ctx.env.binds.get(scope)
            && let Some(bind) = (&**n as &dyn Any).downcast_ref::<Bind<R, E>>()
            && let Expr { kind: ExprKind::Bind(bexp), .. } = &bind.spec
            && let StructurePattern::Bind(name) = &bexp.pattern
            && let Some(id) = bind.single_id()
            && let Some(proxy_id) = binds.get(&CompactString::from(name.as_str()))
            && let Some(proxy_bind) = ctx.env.by_id.get(&proxy_id)
        {
            proxy_bind.typ.unbind_tvars();
            if !proxy_bind.typ.contains(&ctx.env, &bind.typ)? {
                bail!(
                    "signature mismatch in bind {name}, expected type {}, found type {}",
                    proxy_bind.typ,
                    bind.typ
                )
            }
            proxy.insert(id, *proxy_id);
            ctx.rt.ref_var(id, top_id);
            ctx.rt.ref_var(*proxy_id, top_id);
            has_bind.insert(name.clone());
        }
        if let Expr { kind: ExprKind::Module { name, .. }, .. } = n.spec()
            && let Some(block) = (&**n as &dyn Any).downcast_ref::<Block<R, E>>()
            && let scope = ModPath(scope.append(name.as_str()))
            && ctx.env.modules.contains(&scope)
            && let Some(sig) = sig.find_module(name)
        {
            check_sig(ctx, top_id, proxy, &scope, sig, &block.children)?;
            has_mod.insert(name.clone());
        }
        if let Expr { kind: ExprKind::Module { name, .. }, .. } = n.spec()
            && let Some(dynmod) = (&**n as &dyn Any).downcast_ref::<DynamicModule<R, E>>()
            && let Some(sub_sig) = sig.find_module(name)
        {
            if &dynmod.sig != sub_sig {
                bail!(
                    "signature mismatch in mod {name}, expected {}, found {}",
                    sub_sig,
                    dynmod.sig
                )
            }
            has_mod.insert(name.clone());
        }
        if let Expr { kind: ExprKind::TypeDef(td), .. } = n.spec()
            && let Some(defs) = ctx.env.typedefs.get(scope)
            && let Some(sig_td) = defs.get(&CompactString::from(td.name.as_str()))
        {
            let sig_td = TypeDef {
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
            has_def.insert(td.name.clone());
        }
    }
    for si in sig.iter() {
        let missing = match si {
            SigItem::Bind(name, _) => !has_bind.contains(name),
            SigItem::Module(name, _) => !has_mod.contains(name),
            SigItem::TypeDef(td) => !has_def.contains(&td.name),
        };
        if missing {
            bail!("missing required sig item {si}")
        }
    }
    Ok(())
}

#[derive(Debug)]
pub(super) struct DynamicModule<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    source: Node<R, E>,
    env: Env<R, E>,
    sig: Sig,
    scope: ModPath,
    proxy: FxHashMap<BindId, BindId>,
    nodes: Box<[Node<R, E>]>,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> DynamicModule<R, E> {
    pub(super) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        sandbox: Sandbox,
        sig: Sig,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, (*source).clone(), scope, top_id)?;
        let env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        bind_sig(&mut ctx.env, &scope, &sig).context("binding module signature")?;
        Ok(Box::new(Self {
            spec,
            typ: Type::Primitive(Typ::Error | Typ::Null),
            env,
            sig,
            source,
            scope: scope.clone(),
            proxy: FxHashMap::default(),
            nodes: Box::new([]),
            top_id,
        }))
    }

    fn compile_inner(&mut self, ctx: &mut ExecCtx<R, E>, text: ArcStr) -> Result<()> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = parser::parse(ori)?;
        ctx.builtins_allowed = false;
        let nodes = ctx.with_restored(self.env.clone(), |ctx| -> Result<_> {
            let mut nodes = exprs
                .iter()
                .map(|e| compile(ctx, e.clone(), &self.scope, self.top_id))
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

impl<R: Rt, E: UserEvent> Update<R, E> for DynamicModule<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<netidx_value::Value> {
        let mut compiled = false;
        if let Some(v) = self.source.update(ctx, event) {
            self.clear_compiled(ctx);
            match v {
                Value::Error(_) => return Some(v),
                Value::String(s) => {
                    if let Err(e) = self.compile_inner(ctx, s) {
                        let m = format!("invalid dynamic module, compile error {e:?}");
                        return Some(Value::Error(m.into()));
                    }
                }
                v => return Some(Value::Error(format!("unexpected {v}").into())),
            }
            compiled = true;
        }
        let init = event.init;
        if compiled {
            event.init = true;
        }
        for (inner_id, proxy_id) in &self.proxy {
            if let Some(v) = event.variables.get(proxy_id) {
                event.variables.insert(*inner_id, v.clone());
            }
        }
        for n in &mut self.nodes {
            n.update(ctx, event);
        }
        event.init = init;
        for (inner_id, proxy_id) in &self.proxy {
            if let Some(v) = event.variables.remove(inner_id) {
                event.variables.insert(*proxy_id, v);
            }
        }
        compiled.then(|| Value::Null)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx);
        self.clear_compiled(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        for n in &self.nodes {
            n.refs(refs)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.sleep(ctx);
        self.clear_compiled(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck(ctx))?;
        let t = Type::Primitive(Typ::String | Typ::Error);
        wrap!(self.source, t.check_contains(&self.env, self.source.typ()))
    }
}
