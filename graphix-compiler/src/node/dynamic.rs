use std::any::Any;

use crate::{
    compiler::compile,
    env::Env,
    expr::{
        Expr, ExprId, ExprKind, ModPath, Sandbox, Sig, SigItem, StructurePattern, TypeDef,
    },
    node::{Bind, Block},
    typ::Type,
    BindId, ExecCtx, Node, Rt, Update, UserEvent,
};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use compact_str::CompactString;
use fxhash::{FxHashMap, FxHashSet};
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
                if &**name == "dynload_status" {
                    bail!("cannot use reserved name dynload_status in a dynamic module")
                }
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
    env: &Env<R, E>,
    proxy: &mut FxHashMap<BindId, BindId>,
    scope: &ModPath,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let binds = env.binds.get(scope);
    let mut has_bind: FxHashSet<ArcStr> = FxHashSet::default();
    let mut has_mod: FxHashSet<ArcStr> = FxHashSet::default();
    let mut has_def: FxHashSet<ArcStr> = FxHashSet::default();
    for n in nodes {
        if let Some(binds) = binds
            && let Some(bind) = (&**n as &dyn Any).downcast_ref::<Bind<R, E>>()
            && let Expr { kind: ExprKind::Bind(bexp), .. } = &bind.spec
            && let StructurePattern::Bind(name) = &bexp.pattern
            && let Some(id) = bind.single_id()
            && let Some(proxy_id) = binds.get(&CompactString::from(name.as_str()))
            && let Some(proxy_bind) = env.by_id.get(&proxy_id)
        {
            if bind.typ != proxy_bind.typ {
                bail!(
                    "signature mismatch in bind {name}, expected type {}, found type {}",
                    bind.typ,
                    proxy_bind.typ
                )
            }
            proxy.insert(id, *proxy_id);
            has_bind.insert(name.clone());
        }
        if let Expr { kind: ExprKind::Module { name, .. }, .. } = n.spec()
            && let Some(block) = (&**n as &dyn Any).downcast_ref::<Block<R, E>>()
            && let scope = ModPath(scope.append(name.as_str()))
            && env.modules.contains(&scope)
            && let Some(sig) = sig.find_module(name)
        {
            check_sig(env, proxy, &scope, sig, &block.children)?;
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
            && let Some(defs) = env.typedefs.get(scope)
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
struct DynamicModule<R: Rt, E: UserEvent> {
    spec: Expr,
    source: Node<R, E>,
    env: Env<R, E>,
    sig: Sig,
    scope: ModPath,
    proxy: FxHashMap<BindId, BindId>,
    nodes: Box<[Node<R, E>]>,
    status: BindId,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> DynamicModule<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &ModPath,
        sandbox: Sandbox,
        sig: Sig,
        source: Arc<Expr>,
        name: ArcStr,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, (*source).clone(), scope, top_id)?;
        let env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        let scope = ModPath(scope.append(&name));
        bind_sig(&mut ctx.env, &scope, &sig).context("binding module signature")?;
        let status = ctx
            .env
            .bind_variable(
                &scope,
                "dynload_status",
                Type::Ref {
                    scope: ModPath::from(["core"]),
                    name: ModPath::from(["DynLoadStatus"]),
                    params: Arc::from_iter([]),
                },
            )
            .id;
        Ok(Box::new(Self {
            spec,
            env,
            sig,
            source,
            scope,
            proxy: FxHashMap::default(),
            nodes: Box::new([]),
            status,
            top_id,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for DynamicModule<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut crate::Event<E>,
    ) -> Option<netidx_value::Value> {
        todo!()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        todo!()
    }

    fn refs(&self, refs: &mut crate::Refs) {
        todo!()
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        todo!()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        todo!()
    }
}
