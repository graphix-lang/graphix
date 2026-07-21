use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent, defetyp, err, errf,
    expr::{Expr, ExprId},
    fusion::emit::{BodyCx, CompiledExpr, emit_map_new_node, emit_map_ref_node},
    node::{Cached, compiler::compile},
    typ::Type,
    wrap,
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
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        if self.keys.is_empty() {
            // Empty producer = a constant: FIRED at init, the STALE
            // value channel inside frames (the Constant frame rule —
            // a per-site instance's `let res = []` seed died after
            // frame resets and its For bottomed on the missing init,
            // firing-jul2026/03).
            // Frame depth first — frames force init (see Constant).
            if ctx.frame_depth > 0 {
                return Some(if ctx.frame_init {
                    TagValue::fired(Value::Map(CMap::new()))
                } else {
                    TagValue::stale(Value::Map(CMap::new()))
                });
            } else if event.init {
                return Some(TagValue::fired(Value::Map(CMap::new())));
            }
            return None;
        }
        let mut produced = false;
        let mut fired = false;
        let mut determined = true;
        for c in self.keys.iter_mut().chain(self.vals.iter_mut()) {
            if let Some(t) = c.update(ctx, event) {
                produced = true;
                fired |= t.is_fired();
            }
            determined &= c.cached.is_some();
        }
        if produced && determined {
            if self.keys.iter().chain(self.vals.iter()).any(|c| c.tag.is_tainted()) {
                return Some(TagValue::tainted(Value::Null));
            }
            let tag = if fired { Tag::FIRED } else { Tag::STALE };
            let mut m = CMap::new();
            for (k, v) in self.keys.iter().zip(self.vals.iter()) {
                m.insert_cow(
                    k.cached.as_ref().cloned().unwrap(),
                    v.cached.as_ref().cloned().unwrap(),
                );
            }
            Some(TagValue::tagged(Value::Map(m), tag))
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.keys.iter_mut().for_each(|n| n.reset_replay(ctx));
        self.vals.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.keys.iter().for_each(|n| n.node.refs(refs));
        self.vals.iter().for_each(|n| n.node.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.keys.iter_mut().chain(self.vals.iter_mut()) {
            wrap!(n.node, n.node.typecheck0(ctx))?
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

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.keys.iter_mut().chain(self.vals.iter_mut()) {
            wrap!(n.node, n.node.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Map(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map_new_node(cx, &self.keys, &self.vals, &self.typ)
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
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let s = self.source.update(ctx, event);
        let k = self.key.update(ctx, event);
        if s.is_none() && k.is_none() {
            return None;
        }
        if self.source.tag.is_tainted() || self.key.tag.is_tainted() {
            return Some(TagValue::tainted(Value::Null));
        }
        let fired = s.is_some_and(|t| t.is_fired()) || k.is_some_and(|t| t.is_fired());
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let key = match &self.key.cached {
            Some(key) => key,
            None => return None,
        };
        match &self.source.cached {
            Some(src) => Some(TagValue::tagged(map_get(src, key), tag)),
            None => None,
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck0(ctx))?;
        wrap!(self.key.node, self.key.node.typecheck0(ctx))?;
        let mt = Type::Map {
            key: Arc::new(self.key.node.typ().clone()),
            value: Arc::new(self.vtyp.clone()),
        };
        wrap!(self, mt.check_contains(&ctx.env, self.source.node.typ()))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck1(ctx))?;
        wrap!(self.key.node, self.key.node.typecheck1(ctx))?;
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.reset_replay(ctx);
        self.key.reset_replay(ctx);
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::MapRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map_ref_node(cx, &self.source.node, &self.key.node)
    }
}
