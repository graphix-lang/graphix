use super::{
    WakeBit,
    compiler::compile,
    coretraits::with_hooks,
    data::{composite_plumbing, gathered},
    dense_gate,
};
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, UserEvent,
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
    typ::Type,
    wrap,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use immutable_chunkmap::map::Map as CMap;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "MapKeyError", "Error<`{}(string)>");

#[derive(Debug)]
pub struct Map<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    /// Every key in written order, then every value: the order they
    /// update in.
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    fork: ForkSite,
}

impl<R: Rt, E: UserEvent> Map<R, E> {
    fn with(spec: Expr, typ: Type, n: Box<[Node<R, E>]>) -> Node<R, E> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            fork: ForkSite::default(),
        })
    }

    /// The entries' keys and values, each in written order.
    pub(crate) fn entries(&self) -> (&[Node<R, E>], &[Node<R, E>]) {
        self.n.split_at(self.n.len() / 2)
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = crate::image::count_decode(buf)?;
        let (mut keys, mut values) = (Vec::with_capacity(n), Vec::with_capacity(n));
        for _ in 0..n {
            keys.push(decode_node(ctx, buf)?);
            values.push(decode_node(ctx, buf)?);
        }
        keys.extend(values);
        Ok(Self::with(spec, typ, keys.into_boxed_slice()))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[(Expr, Expr)]>,
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|(k, _)| k)
            .chain(args.iter().map(|(_, v)| v))
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let typ = Type::Map {
            key: Arc::new(Type::empty_tvar()),
            value: Arc::new(Type::empty_tvar()),
        };
        Ok(Self::with(spec, typ, n))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Map<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Map, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        let (keys, values) = self.entries();
        encode_varint(keys.len() as u64, buf);
        for (k, v) in keys.iter().zip(values.iter()) {
            k.image_encode(buf)?;
            v.image_encode(buf)?;
        }
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let (vals, tag) = gathered!(self, ctx, Value::Map(CMap::new()));
        let vals: SmallVec<[Value; 32]> = vals.collect();
        let (keys, values) = vals.split_at(vals.len() / 2);
        let m = with_hooks(ctx, || {
            let mut m = CMap::new();
            for (k, v) in keys.iter().zip(values.iter()) {
                m.insert_cow(k.clone(), v.clone());
            }
            m
        });
        self.resident.set(TagValue::tagged(Value::Map(m), tag))
    }

    composite_plumbing!(Map);

    super::typed_by_row!();

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        let (keys, values) = self.entries();
        emit_map_new_node(cx, keys, values, &self.typ)
    }
}

impl<R: Rt, E: UserEvent> Map<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
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
        let (keys, values) = self.entries();
        kts.extend(keys.iter().map(|k| k.typ()));
        vts.extend(values.iter().map(|v| v.typ()));
        let ktype = wrap!(self, Type::union(&ctx.env, &kts))?;
        ktype.require_compared(&Type::Ordered);
        let vtype = wrap!(self, Type::union(&ctx.env, &vts))?;
        let rtype = Type::Map { key: Arc::new(ktype), value: Arc::new(vtype) };
        self.typ.check_contains(&ctx.env, &rtype)?;
        let judgment = crate::PendingSettle::Discernible {
            typ: self.typ.clone(),
            what: crate::Discerned::Keys,
            spec: Arc::new(self.spec.clone()),
        };
        super::defer_judgment(ctx, judgment);
        Ok(())
    }
}

#[derive(Debug)]
pub struct MapRef<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R, E>,
    pub key: Node<R, E>,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub vtyp: Type,
    resident: TagValue,
}

/// Look up `key` in a `Value::Map`, returning the value or the
/// `map key not found` error. Shared by the node-walk and the JIT so
/// both agree bit-for-bit. `src` must be a `Value::Map`.
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
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        _check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        wrap!(self.key, child(&mut self.key, ctx))?;
        // the map's key type holds the key, not the other way around
        let kt = Type::empty_tvar();
        let mt =
            Type::Map { key: Arc::new(kt.clone()), value: Arc::new(self.vtyp.clone()) };
        wrap!(self, mt.check_contains(&ctx.env, self.source.typ()))?;
        wrap!(self.key, kt.check_contains(&ctx.env, self.key.typ()))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let source = decode_node(ctx, buf)?;
        let key = decode_node(ctx, buf)?;
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let vtyp = Type::decode(buf)?;
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            source,
            key,
            spec,
            typ,
            vtyp,
            resident: TagValue::phantom(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
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
            slept: WakeBit::default(),
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.source);
        f(&self.key)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.source);
        f(&mut self.key)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::MapRef, buf);
        self.source.image_encode(buf)?;
        self.key.image_encode(buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.vtyp.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let s = self.source.update(ctx);
        let k = self.key.update(ctx);
        let tag = s.tag().join(k.tag());
        dense_gate!(self, tag, tag.is_bottom());
        let v = with_hooks(ctx, || s.with_value(|s| k.with_value(|k| map_get(s, k))));
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
    }

    /// The element cell binds from the source as in the check; the
    /// index needs no judging.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types), false)
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.source.sleep(ctx);
        self.key.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.source, &mut self.key], ctx)
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::MapRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map_ref_node(cx, &self.source, &self.key)
    }
}
