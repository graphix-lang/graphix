use crate::{
    defetyp, err, errf,
    expr::{Expr, ExprId},
    node::{compiler::compile, Cached},
    typ::Type,
    update_args, wrap, CFlag, Event, ExecCtx, Node, Refs, Rt, Scope, Update, UserEvent,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use immutable_chunkmap::map::Map as CMap;
use netidx_value::Value;
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "MapKeyError", "Error<`{}(string)>");

#[derive(Debug)]
pub struct Map<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub keys: Box<[Cached<R, E>]>,
    pub vals: Box<[Cached<R, E>]>,
}

impl<R: Rt, E: UserEvent> Map<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[(Expr, Expr)]>,
    ) -> Result<Node<R, E>> {
        let keys = args
            .iter()
            .map(|(k, _)| Ok(Cached::new(compile(ctx, flags, k.clone(), scope, top_id)?)))
            .collect::<Result<_>>()?;
        let vals = args
            .iter()
            .map(|(_, v)| Ok(Cached::new(compile(ctx, flags, v.clone(), scope, top_id)?)))
            .collect::<Result<_>>()?;
        let typ = Type::Map {
            key: Arc::new(Type::empty_tvar()),
            value: Arc::new(Type::empty_tvar()),
        };
        Ok(Box::new(Self { spec, typ, keys, vals }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Map<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if self.keys.is_empty() && event.init {
            return Some(Value::Map(CMap::new()));
        }
        let (kupdated, kdetermined) = update_args!(self.keys, ctx, event);
        let (vupdated, vdetermined) = update_args!(self.vals, ctx, event);
        let (updated, determined) = (kupdated || vupdated, kdetermined && vdetermined);
        if updated && determined {
            let mut m = CMap::new();
            for (k, v) in self.keys.iter().zip(self.vals.iter()) {
                m.insert_cow(
                    k.cached.as_ref().cloned().unwrap(),
                    v.cached.as_ref().cloned().unwrap(),
                );
            }
            Some(Value::Map(m))
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.keys.iter_mut().for_each(|n| n.node.delete(ctx));
        self.vals.iter_mut().for_each(|n| n.node.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.keys.iter_mut().for_each(|n| n.sleep(ctx));
        self.vals.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.keys.iter().for_each(|n| n.node.refs(refs));
        self.vals.iter().for_each(|n| n.node.refs(refs))
    }

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.keys.iter_mut().chain(self.vals.iter_mut()) {
            wrap!(n.node, n.node.typecheck(ctx))?
        }
        let ktype = self
            .keys
            .iter()
            .fold(Ok(Type::Bottom), |acc, n| n.node.typ().union(&ctx.env, &acc?));
        let ktype = wrap!(self, ktype)?;
        let vtype = self
            .vals
            .iter()
            .fold(Ok(Type::Bottom), |acc, n| n.node.typ().union(&ctx.env, &acc?));
        let vtype = wrap!(self, vtype)?;
        let rtype = Type::Map { key: Arc::new(ktype), value: Arc::new(vtype) };
        Ok(self.typ.check_contains(&ctx.env, &rtype)?)
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Map(self)
    }

    fn emit_clif(
        &self,
        cx: &mut crate::gir_jit::BodyCx,
    ) -> Result<crate::gir_jit::CompiledExpr> {
        crate::gir_jit::emit_map_new_node(cx, &self.keys, &self.vals, &self.typ)
    }

    fn clone_rebind(&self, ctx: &mut ExecCtx<R, E>, scope: &Scope) -> Node<R, E> {
        Box::new(Self {
            spec: self.spec.clone(),
            typ: self.typ.clone(),
            keys: self
                .keys
                .iter()
                .map(|c| Cached::new(c.node.clone_rebind(ctx, scope)))
                .collect(),
            vals: self
                .vals
                .iter()
                .map(|c| Cached::new(c.node.clone_rebind(ctx, scope)))
                .collect(),
        })
    }
}

#[derive(Debug)]
pub struct MapRef<R: Rt, E: UserEvent> {
    pub source: Cached<R, E>,
    pub key: Cached<R, E>,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub vtyp: Type,
}

/// Look up `key` in a `Value::Map`, returning the value or the
/// `map key not found` error. Shared by the node-walk `MapRef`, the
/// fusion interpreter, and the JIT (`graphix_map_ref`) so all three
/// agree bit-for-bit. `src` must be a `Value::Map`.
pub(crate) fn map_get(src: &Value, key: &Value) -> Value {
    match src {
        Value::Map(map) => match map.get(key) {
            Some(value) => value.clone(),
            None => errf!(ERR_TAG, "map key {key} not found"),
        },
        _ => err!(ERR_TAG, "COMPILER BUG! expected a map"),
    }
}

impl<R: Rt, E: UserEvent> MapRef<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        key: &Expr,
    ) -> Result<Node<R, E>> {
        let source = Cached::new(compile(ctx, flags, source.clone(), scope, top_id)?);
        let key = Cached::new(compile(ctx, flags, key.clone(), scope, top_id)?);
        let vtyp = match &source.node.typ() {
            Type::Map { value, .. } => (**value).clone(),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([vtyp.clone(), ERR.clone()]));
        Ok(Box::new(Self { source, key, spec, typ, vtyp }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for MapRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let up = self.source.update(ctx, event);
        let up = self.key.update(ctx, event) || up;
        if !up {
            return None;
        }
        let key = match &self.key.cached {
            Some(key) => key,
            None => return None,
        };
        match &self.source.cached {
            Some(src) => Some(map_get(src, key)),
            None => None,
        }
    }

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck(ctx))?;
        wrap!(self.key.node, self.key.node.typecheck(ctx))?;
        let mt = Type::Map {
            key: Arc::new(self.key.node.typ().clone()),
            value: Arc::new(self.vtyp.clone()),
        };
        wrap!(self, mt.check_contains(&ctx.env, self.source.node.typ()))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.node.refs(refs);
        self.key.node.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.node.delete(ctx);
        self.key.node.delete(ctx);
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.sleep(ctx);
        self.key.sleep(ctx);
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::MapRef(self)
    }

    fn emit_clif(
        &self,
        cx: &mut crate::gir_jit::BodyCx,
    ) -> Result<crate::gir_jit::CompiledExpr> {
        crate::gir_jit::emit_map_ref_node(cx, &self.source.node, &self.key.node)
    }

    fn clone_rebind(&self, ctx: &mut ExecCtx<R, E>, scope: &Scope) -> Node<R, E> {
        Box::new(Self {
            source: Cached::new(self.source.node.clone_rebind(ctx, scope)),
            key: Cached::new(self.key.node.clone_rebind(ctx, scope)),
            spec: self.spec.clone(),
            typ: self.typ.clone(),
            vtyp: self.vtyp.clone(),
        })
    }
}
