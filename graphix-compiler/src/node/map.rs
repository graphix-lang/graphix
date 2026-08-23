use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent, defetyp, err, errf,
    expr::{Expr, ExprId},
    fusion::emit::{BodyCx, CompiledExpr, emit_map_new_node, emit_map_ref_node},
    node::{compiler::compile, dense_gate, gather, read_prod},
    typ::Type,
    wrap,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use immutable_chunkmap::map::Map as CMap;
use netidx_value::Value;
use poolshark::local::LPooled;
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "MapKeyError", "Error<`{}(string)>");

#[derive(Debug)]
pub struct Map<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub keys: Box<[Node<R, E>]>,
    pub vals: Box<[Node<R, E>]>,
    resident: TagValue,
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
            .map(|(k, _)| compile(ctx, flags, k.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let vals = args
            .iter()
            .map(|(_, v)| compile(ctx, flags, v.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let typ = Type::Map {
            key: Arc::new(Type::empty_tvar()),
            value: Arc::new(Type::empty_tvar()),
        };
        Ok(Node::new(Self { spec, typ, keys, vals, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Map<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.keys.is_empty() {
            // Empty producer = a constant: FIRED at init, the STALE
            // value channel inside frames (the Constant frame rule —
            // a per-site instance's `let res = []` seed died after
            // frame resets and its For bottomed on the missing init,
            // firing-jul2026/03).
            // Frame depth first — frames force init (see Constant).
            if ctx.frame_depth > 0 {
                return self.resident.set(if ctx.frame_init {
                    TagValue::fired(Value::Map(CMap::new()))
                } else {
                    TagValue::stale(Value::Map(CMap::new()))
                });
            } else if event.init {
                return self.resident.set(TagValue::fired(Value::Map(CMap::new())));
            }
            return self.resident.ride();
        }
        let mut kvals: LPooled<Vec<Value>> = LPooled::take();
        let mut vvals: LPooled<Vec<Value>> = LPooled::take();
        let (kt, kf, kb) = gather(ctx, event, &mut self.keys, &mut kvals);
        let (vt, vf, vb) = gather(ctx, event, &mut self.vals, &mut vvals);
        let (trig, fired, bottom) = (kt || vt, kf || vf, kb || vb);
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        // construction compares keys — armed so a core `Ord`
        // implementation on the key type orders the map (the value seam)
        let m = super::coretraits::with_value_hooks(ctx, event, |_, _| {
            let mut m = CMap::new();
            for (k, v) in kvals.drain(..).zip(vvals.drain(..)) {
                m.insert_cow(k, v);
            }
            m
        });
        self.resident.set(TagValue::tagged(Value::Map(m), tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.keys.iter_mut().for_each(|n| n.delete(ctx));
        self.vals.iter_mut().for_each(|n| n.delete(ctx))
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
        self.keys.iter().for_each(|n| n.refs(refs));
        self.vals.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.keys.iter_mut().chain(self.vals.iter_mut()) {
            wrap!(n, n.typecheck0(ctx))?
        }
        let ktype = self
            .keys
            .iter()
            .fold(Ok(Type::Bottom), |acc, n| n.typ().union(&ctx.env, &acc?));
        let ktype = wrap!(self, ktype)?;
        let vtype = self
            .vals
            .iter()
            .fold(Ok(Type::Bottom), |acc, n| n.typ().union(&ctx.env, &acc?));
        let vtype = wrap!(self, vtype)?;
        let rtype = Type::Map { key: Arc::new(ktype), value: Arc::new(vtype) };
        Ok(self.typ.check_contains(&ctx.env, &rtype)?)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.keys.iter_mut().chain(self.vals.iter_mut()) {
            wrap!(n, n.typecheck1(ctx))?
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
    pub source: Node<R, E>,
    pub key: Node<R, E>,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub vtyp: Type,
    resident: TagValue,
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
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let key = compile(ctx, flags, key.clone(), scope, top_id)?;
        let vtyp = match &source.typ() {
            Type::Map { value, .. } => (**value).clone(),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([vtyp.clone(), ERR.clone()]));
        Ok(Node::new(Self {
            source,
            key,
            spec,
            typ,
            vtyp,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for MapRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let mut trig = false;
        let mut fired = false;
        let mut bottom = false;
        let sval = read_prod!(self.source, ctx, event, trig, fired, bottom);
        let kval = read_prod!(self.key, ctx, event, trig, fired, bottom);
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        // the lookup compares keys — armed so a core `Ord`
        // implementation on the key type is honored (the value seam)
        let v = super::coretraits::with_value_hooks(ctx, event, |_, _| {
            map_get(&sval.unwrap(), &kval.unwrap())
        });
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        wrap!(self.key, self.key.typecheck0(ctx))?;
        let mt = Type::Map {
            key: Arc::new(self.key.typ().clone()),
            value: Arc::new(self.vtyp.clone()),
        };
        wrap!(self, mt.check_contains(&ctx.env, self.source.typ()))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        wrap!(self.key, self.key.typecheck1(ctx))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.key.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx);
        self.key.delete(ctx);
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
        emit_map_ref_node(cx, &self.source, &self.key)
    }
}
