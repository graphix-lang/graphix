use super::{WakeBit, compiler::compile, coretraits::with_hooks, dense_gate};
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent, defetyp, err, errf,
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
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
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
    /// The `key => value` entries, in written order.
    pub entries: Box<[(Node<R, E>, Node<R, E>)]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Map<R, E> {
    fn with(
        spec: Expr,
        typ: Type,
        entries: Box<[(Node<R, E>, Node<R, E>)]>,
    ) -> Node<R, E> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            entries,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_varint(buf)? as usize;
        if n > buf.len() {
            return Err(PackError::TooBig);
        }
        let entries = (0..n)
            .map(|_| Ok((decode_node(ctx, buf)?, decode_node(ctx, buf)?)))
            .collect::<Result<_, PackError>>()?;
        Ok(Self::with(spec, typ, entries))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[(Expr, Expr)]>,
    ) -> Result<Node<R, E>> {
        let entries = args
            .iter()
            .map(|(k, v)| {
                let k = compile(ctx, flags, k.clone(), scope, top_id)?;
                Ok((k, compile(ctx, flags, v.clone(), scope, top_id)?))
            })
            .collect::<Result<_>>()?;
        let typ = Type::Map {
            key: Arc::new(Type::empty_tvar()),
            value: Arc::new(Type::empty_tvar()),
        };
        Ok(Self::with(spec, typ, entries))
    }

    /// Every key, then every value: the order the entries update in.
    fn each(&mut self, mut f: impl FnMut(&mut Node<R, E>) -> Result<()>) -> Result<()> {
        for (k, _) in self.entries.iter_mut() {
            f(k)?
        }
        for (_, v) in self.entries.iter_mut() {
            f(v)?
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Map<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Map, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_varint(self.entries.len() as u64, buf);
        for (k, v) in self.entries.iter() {
            k.image_encode(buf)?;
            v.image_encode(buf)?;
        }
        Ok(())
    }

    // CR claude for eric: [structure] This re-implements by hand what gather and
    // gathered! do for Struct, Tuple and Variant: update every child, join the tags,
    // apply the dense gate. The keys-then-values order is walked in four more places
    // (each, refs, node_shape.rs:337, fusion/mod.rs:728). Unlike the other literals it
    // has no ForkSite, so a map literal never forks: `#[parallel] {"x" => f(a), "y" =>
    // f(a + 1)}` is refused ("has nothing to run in parallel") where `#[parallel]
    // (f(a), f(a + 1))` runs. Store the entries flat, keys then values, with a
    // ForkSite, and use gathered! and composite_plumbing! as the other constructors do.
    // (c-data-map-08)
    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        if self.entries.is_empty() {
            return super::produce_constant(ctx.event, &mut self.resident, || {
                Value::Map(CMap::new())
            });
        }
        let (mut keys, mut vals): (SmallVec<[&mut Node<R, E>; 8]>, SmallVec<[_; 8]>) =
            self.entries.iter_mut().map(|(k, v)| (k, v)).unzip();
        let keys: SmallVec<[&TagValue; 8]> =
            keys.iter_mut().map(|k| k.update(ctx)).collect();
        let vals: SmallVec<[&TagValue; 8]> =
            vals.iter_mut().map(|v| v.update(ctx)).collect();
        let tag = keys.iter().chain(vals.iter()).fold(Tag::STALE, |t, p| t.join(p.tag()));
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let m = with_hooks(ctx, || {
            let mut m = CMap::new();
            for (k, v) in keys.iter().zip(vals.iter()) {
                m.insert_cow(k.value_cloned(), v.value_cloned());
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let _ = self.each(|n| Ok(n.delete(ctx)));
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        let _ = self.each(|n| Ok(n.sleep(ctx)));
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.entries.iter_mut().flat_map(|(k, v)| [k, v]), ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.entries.iter().for_each(|(k, _)| k.refs(refs));
        self.entries.iter().for_each(|(_, v)| v.refs(refs))
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.each(|n| wrap!(n, n.typecheck1(ctx)))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Map(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map_new_node(cx, &self.entries, &self.typ)
    }
}

impl<R: Rt, E: UserEvent> Map<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        self.each(|n| wrap!(n, child(n, ctx)))?;
        if !check {
            return Ok(());
        }
        let bottom = Type::Bottom;
        let mut kts: LPooled<Vec<&Type>> = LPooled::take();
        let mut vts: LPooled<Vec<&Type>> = LPooled::take();
        kts.push(&bottom);
        vts.push(&bottom);
        for (k, v) in self.entries.iter() {
            kts.push(k.typ());
            vts.push(v.typ());
        }
        let ktype = wrap!(self, Type::union(&ctx.env, &kts))?;
        let vtype = wrap!(self, Type::union(&ctx.env, &vts))?;
        let rtype = Type::Map { key: Arc::new(ktype), value: Arc::new(vtype) };
        Ok(self.typ.check_contains(&ctx.env, &rtype)?)
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
        // CR claude for eric: [bug] Map containment is covariant in the key, so this
        // check requires the key's type to contain the map's key type. As a result a
        // key narrower than the map's key is refused, and a wider one is accepted. `let
        // m = {`Red => "r", `Green => "g"}; m{`Red}` is refused (Map<`Red, ..> does not
        // contain Map<[`Green, `Red], ..>), and so are `m{1}` over Map<[i64, string],
        // i64> and `m{"a"}` over Map<[string, null], i64>. `map::get(m, `Red)` checks
        // and returns "r". Place::elem_type (bind.rs:1058) makes the same check, so
        // `&m{`Red}` is refused too; the fix is to check the key against the source's
        // key cell (map key contains key). probe:
        // design/review-2026-10-05/repro/c-data-map-01.gx (c-data-map-01)
        let mt = Type::Map {
            key: Arc::new(self.key.typ().clone()),
            value: Arc::new(self.vtyp.clone()),
        };
        wrap!(self, mt.check_contains(&ctx.env, self.source.typ()))?;
        Ok(())
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
        dense_gate!(self, tag.triggers(), tag.is_bottom());
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

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        wrap!(self.key, self.key.typecheck1(ctx))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.key.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.delete(ctx);
        self.key.delete(ctx);
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
