use super::{
    WakeBit,
    compiler::compile,
    coretraits::with_hooks,
    data::{composite_plumbing, gathered},
    dense_gate,
};
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update,
    cost::ForkSite,
    defetyp, err, errf,
    expr::{Expr, ExprId},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr, emit_map_new_node, emit_map_ref_node},
    },
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    typ::{Bound, NakedPrefix, TVar, Type, tvar::require_bounds},
    wrap,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use immutable_chunkmap::map::Map as CMap;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use poolshark::local::LPooled;
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "MapKeyError", "Error<`{}(string)>");

#[derive(Debug)]
pub struct Map<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    /// Each entry's key then its value, in written order: the order
    /// they update in.
    pub n: Box<[Node<R>]>,
    resident: TagValue,
    check_keys: bool,
    pub(crate) fork: ForkSite,
}

impl<R: Rt> Map<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        n: Box<[Node<R>]>,
        fork: ForkSite,
        check_keys: bool,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            fork,
            check_keys,
        })
    }

    /// Each entry's key and value, in written order.
    pub(crate) fn entries(&self) -> impl Iterator<Item = (&Node<R>, &Node<R>)> {
        self.n.chunks_exact(2).map(|kv| (&kv[0], &kv[1]))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = crate::image::count_decode(buf)?;
        let entries = (0..2 * n)
            .map(|_| decode_node(ctx, buf))
            .collect::<Result<Box<[_]>, PackError>>()?;
        Ok(Self::with(
            spec,
            typ,
            entries,
            ForkSite::image_decode(buf)?,
            bool::decode(buf)?,
        ))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[(Expr, Expr)]>,
    ) -> Result<Node<R>> {
        let n = args
            .iter()
            .flat_map(|(k, v)| [k, v])
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let typ = Type::Map {
            key: Arc::new(Type::empty_tvar()),
            value: Arc::new(Type::empty_tvar()),
        };
        Ok(Self::with(spec, typ, n, ForkSite::default(), true))
    }
}

impl<R: Rt> Update<R> for Map<R> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Map, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_varint(self.n.len() as u64 / 2, buf);
        for (k, v) in self.entries() {
            k.image_encode(buf)?;
            v.image_encode(buf)?;
        }
        self.fork.image_encode(buf)?;
        self.check_keys.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let (mut vals, tag) = gathered!(self, ctx, Value::Map(CMap::new()));
        let m = with_hooks(ctx, || {
            let mut m = CMap::new();
            while let (Some(k), Some(v)) = (vals.next(), vals.next()) {
                if self.check_keys && !super::coretraits::ordered(&k) {
                    return None;
                }
                m.insert_cow(k, v);
            }
            Some(Value::Map(m))
        });
        match m {
            Some(m) => self.resident.set(TagValue::tagged(m, tag)),
            None => self.resident.set_bottom_as(tag),
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        for n in &mut self.n {
            wrap!(n, n.typecheck1(ctx))?;
        }
        let check_keys = self.entries().any(|(k, _)| k.typ().orders_refs(&ctx.env));
        self.check_keys = check_keys;
        Ok(())
    }

    composite_plumbing!(Map);

    super::typed_by_row!();

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map_new_node(cx, &self.n, &self.typ)
    }
}

impl<R: Rt> Map<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, child(n, ctx))?
        }
        if !check {
            return Ok(());
        }
        let bottom = Type::Bottom;
        let mut kts: LPooled<Vec<&Type>> = LPooled::take();
        let mut vts: LPooled<Vec<&Type>> = LPooled::take();
        kts.push(&bottom);
        vts.push(&bottom);
        for (k, v) in self.entries() {
            kts.push(k.typ());
            vts.push(v.typ());
        }
        let ktype = wrap!(self, Type::union(&ctx.env, &kts))?;
        require_bounds(&[Type::Bound(Bound::Ordered)], &ktype);
        let vtype = wrap!(self, super::array::element_union(&ctx.env, &vts))?;
        let bound = match &self.typ {
            Type::Map { key, value } => {
                super::array::bind_element(key, &ktype)
                    && super::array::bind_element(value, &vtype)
            }
            _ => false,
        };
        if !bound {
            let rtype = Type::Map { key: Arc::new(ktype), value: Arc::new(vtype) };
            self.typ.check_contains(&ctx.env, &rtype).map_err(|e| {
                match (&self.typ, &rtype) {
                    (Type::Map { key, value }, Type::Map { key: kt, value: vt }) => {
                        TVar::refusal_over(value, vt, TVar::refusal_over(key, kt, e))
                    }
                    _ => e,
                }
            })?;
        }
        let judgment = crate::PendingSettle::Discernible {
            typ: self.typ.clone(),
            what: crate::Discerned::Keys,
            spec: self.spec.clone(),
        };
        super::defer_judgment(ctx, judgment);
        Ok(())
    }
}

#[derive(Debug)]
pub struct MapRef<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R>,
    pub key: Node<R>,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub vtyp: Type,
    resident: TagValue,
    check_key: bool,
}

/// Look up `key` in a `Value::Map`, returning the value or the
/// `map key not found` error. Shared by the node-walk and the JIT so
/// both agree bit-for-bit. `src` must be a `Value::Map`.
pub(crate) fn map_get(src: &Value, key: &Value, check_key: bool) -> Option<Value> {
    if check_key && !super::coretraits::ordered(key) {
        return None;
    }
    Some(match src {
        Value::Map(map) => match map.get(key) {
            Some(value) => value.clone(),
            // the printer a message uses: an abstract key by its Display
            None => errf!(ERR_TAG, "map key {} not found", NakedPrefix(key)),
        },
        _ => err!(ERR_TAG, "COMPILER BUG! expected a map"),
    })
}

impl<R: Rt> MapRef<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        vtyp: Type,
        source: Node<R>,
        key: Node<R>,
        check_key: bool,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            source,
            key,
            spec,
            typ,
            vtyp,
            resident: TagValue::phantom(),
            check_key,
        })
    }

    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        wrap!(self.key, child(&mut self.key, ctx))?;
        // the map's key type holds the key, not the other way around
        let kt = Type::empty_tvar();
        let mt =
            Type::Map { key: Arc::new(kt.clone()), value: Arc::new(self.vtyp.clone()) };
        wrap!(self, mt.check_contains(&ctx.env, self.source.typ()))?;
        match check {
            true => wrap!(self.key, kt.check_contains(&ctx.env, self.key.typ())),
            false => Ok(()),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let source = decode_node(ctx, buf)?;
        let key = decode_node(ctx, buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let vtyp = Type::decode(buf)?;
        Ok(Self::with(spec, typ, vtyp, source, key, bool::decode(buf)?))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        key: &Expr,
    ) -> Result<Node<R>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let key = compile(ctx, flags, key.clone(), scope, top_id)?;
        let vtyp = match &source.typ() {
            Type::Map { value, .. } => (**value).clone(),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([vtyp.clone(), ERR.clone()]));
        Ok(Self::with(spec, typ, vtyp, source, key, true))
    }
}

impl<R: Rt> Update<R> for MapRef<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.source);
        f(&self.key)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.source);
        f(&mut self.key)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::MapRef, buf);
        self.source.image_encode(buf)?;
        self.key.image_encode(buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.vtyp.encode(buf)?;
        self.check_key.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let s = self.source.update(ctx);
        let k = self.key.update(ctx);
        let tag = s.tag().join(k.tag());
        dense_gate!(self, tag, tag.is_bottom());
        let v = with_hooks(ctx, || {
            s.with_value(|s| k.with_value(|k| map_get(s, k, self.check_key)))
        });
        match v {
            Some(v) => self.resident.set(TagValue::tagged(v, tag)),
            None => self.resident.set_bottom_as(tag),
        }
    }

    // The element cell binds from the source as in the check; the
    // index needs no judging.
    super::typed_by_row!(source);

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        wrap!(self.key, self.key.typecheck1(ctx))?;
        self.check_key = self.key.typ().orders_refs(&ctx.env);
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.source.sleep(ctx);
        self.key.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.source, &mut self.key], ctx)
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::MapRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map_ref_node(cx, &self.source, &self.key)
    }
}
