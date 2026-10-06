use super::{WakeBit, compiler::compile, dense_gate, gather};
use crate::cost::ForkSite;
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, TagValue, Update,
    UserEvent, abstract_value, bailat, deref_typ,
    expr::{At, Expr, ExprId, ExprKind, ModPath, WrittenAt},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, emit_abstract_ref_node, emit_construct_node,
            emit_struct_new_node, emit_struct_ref_node, emit_struct_with_node,
            emit_tuple_new_node, emit_tuple_ref_node, emit_variant_new_node,
        },
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    typ::{AbstractId, Type},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::iter;
use triomphe::Arc;

/// The `Update` methods Struct, Tuple and Variant share: children `n`
/// gathered into one value, a resident, a wake bit.
macro_rules! composite_plumbing {
    ($name:ident) => {
        fn spec(&self) -> &Expr {
            &self.spec
        }

        fn typ(&self) -> &Type {
            &self.typ
        }

        fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
            self.n.iter_mut().for_each(|n| n.delete(ctx))
        }

        fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
            self.slept.set();
            self.n.iter_mut().for_each(|n| n.sleep(ctx))
        }

        fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
            fusion::fuse_parts(self.n.iter_mut(), ctx)
        }

        fn refs(&self, refs: &mut Refs) {
            self.n.iter().for_each(|n| n.refs(refs))
        }

        fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
            for n in self.n.iter_mut() {
                wrap!(n, n.typecheck1(ctx))?
            }
            Ok(())
        }

        fn view(&self) -> NodeView<'_, R, E> {
            NodeView::$name(self)
        }
    };
}

/// A composite's children gathered and gated: returns from the caller
/// with `$empty` for a childless one, a bottom, or a ride; otherwise
/// the children's values and the tag of the result.
macro_rules! gathered {
    ($self:ident, $ctx:ident, $empty:expr) => {{
        if $self.n.is_empty() {
            return super::produce_constant($ctx.event, &mut $self.resident, || $empty);
        }
        let (tag, prods) = gather($ctx, &mut $self.n, &mut $self.fork);
        dense_gate!($self, tag.triggers(), tag.is_bottom());
        (prods.into_iter().map(|tv| tv.value_cloned()), tag)
    }};
}

#[derive(Debug)]
pub struct Struct<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub names: Box<[ArcStr]>,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    fork: ForkSite,
}

impl<R: Rt, E: UserEvent> Struct<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &[(ArcStr, Expr)],
    ) -> Result<Node<R, E>> {
        let names: Box<[ArcStr]> = args.iter().map(|(n, _)| ctx.tag(n)).collect();
        let n = args
            .iter()
            .map(|(_, e)| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        let typs = names
            .iter()
            .zip(n.iter())
            .map(|(n, a)| (n.clone(), a.typ().clone(), WrittenAt::NOWHERE));
        let typ = Type::Struct(Arc::from_iter(typs));
        Ok(Node::new(Self {
            spec,
            typ,
            names,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Struct<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let names: Box<[ArcStr]> =
            Vec::<ArcStr>::decode(buf)?.iter().map(|n| ctx.tag(n)).collect();
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self {
            spec,
            typ,
            names,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Struct<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Struct, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        image::slice_encode(&self.names, buf)?;
        encode_nodes(&self.n, buf)
    }

    composite_plumbing!(Struct);

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let (vals, tag) = gathered!(self, ctx, Value::Array(ValArray::from([])));
        let iter = self.names.iter().zip(vals).map(|(name, v)| {
            let name = Value::String(name.clone());
            Value::Array(ValArray::from_iter_exact([name, v].into_iter()))
        });
        let v = Value::Array(ValArray::from_iter_exact(iter));
        self.resident.set(TagValue::tagged(v, tag))
    }

    super::typed_by_row!();

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_struct_new_node(cx, &self.names, &self.n)
    }
}

#[derive(Debug)]
pub struct Replace<R: Rt, E: UserEvent> {
    /// The field's position in the struct's sorted layout, resolved by
    /// typecheck0.
    pub(crate) index: Option<usize>,
    pub name: ArcStr,
    pub n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Replace<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.index.encode(buf)?;
        self.name.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let index = Option::<usize>::decode(buf)?;
        let name = ArcStr::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        Ok(Replace { index, name, n })
    }
}

#[derive(Debug)]
pub struct StructWith<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub source: Node<R, E>,
    pub replace: Box<[Replace<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> StructWith<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let source = decode_node(ctx, buf)?;
        let n = decode_varint(buf)? as usize;
        // CR claude for claude: [risk] n comes straight from the image. A corrupt count
        // of 2^63-1 panics with capacity overflow, and one of 2^40 aborts on the failed
        // allocation, so the process dies on every start while the entry stays instead
        // of failing the read and starting cold. callsite.rs:1868 (also reached when an
        // instance is decoded lazily at its first dispatch), traits.rs:594, bind.rs:908
        // and stdlib/graphix-package-map/src/lib.rs:218 size their allocations from an
        // unchecked count the same way. Every other image count that sizes an
        // allocation is guarded, in several spellings: pattern.rs:1379 decode_count,
        // lambda.rs:263 len, map.rs:62, and n.min(buf.len()) in image/nodes.rs,
        // select.rs and seq_machine.rs. One count reader in image, used at every count,
        // fixes these and replaces the copies. probe:
        // design/review-2026-10-05/repro/c-image-08.gx (c-image-08)
        let mut replace = Vec::with_capacity(n);
        for _ in 0..n {
            replace.push(Replace::image_decode(ctx, buf)?);
        }
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            source,
            replace: replace.into_boxed_slice(),
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
        replace: &[(ArcStr, Expr)],
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let replace = replace
            .iter()
            .map(|(name, e)| {
                Ok(Replace {
                    index: None,
                    name: name.clone(),
                    n: compile(ctx, flags, e.clone(), scope, top_id)?,
                })
            })
            .collect::<Result<Box<[_]>>>()?;
        let typ = source.typ().clone();
        Ok(Node::new(Self {
            spec,
            typ,
            source,
            replace,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StructWith<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::StructWith, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.source.image_encode(buf)?;
        encode_varint(self.replace.len() as u64, buf);
        for r in self.replace.iter() {
            r.image_encode(buf)?;
        }
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let index: SmallVec<[Option<usize>; 8]> =
            self.replace.iter().map(|r| r.index).collect();
        let src = self.source.update(ctx);
        let vals: SmallVec<[&TagValue; 8]> =
            self.replace.iter_mut().map(|r| r.n.update(ctx)).collect();
        let tag = vals.iter().fold(src.tag(), |t, v| t.join(v.tag()));
        // an unshaped (non-struct-rep) source is bottom
        let shaped = src.with_value(|v| matches!(v, Value::Array(_)));
        dense_gate!(self, tag.triggers(), tag.is_bottom() || !shaped);
        let v = src.with_value(|src| {
            let Value::Array(src) = src else { unreachable!("gated on the shape") };
            let mut fields: LPooled<Vec<Value>> = src.iter().cloned().collect();
            for (i, v) in index.iter().zip(vals.iter()) {
                if let Some(Value::Array(kv)) = i.and_then(|i| fields.get_mut(i))
                    && kv.len() == 2
                {
                    *kv = ValArray::from_iter_exact(
                        [kv[0].clone(), v.value_cloned()].into_iter(),
                    );
                }
            }
            Value::Array(ValArray::from_iter_exact(fields.drain(..)))
        });
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.delete(ctx);
        self.replace.iter_mut().for_each(|r| r.n.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.source.sleep(ctx);
        self.replace.iter_mut().for_each(|r| r.n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(
            iter::once(&mut self.source).chain(self.replace.iter_mut().map(|r| &mut r.n)),
            ctx,
        )
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.replace.iter().for_each(|r| r.n.refs(refs))
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        for rep in self.replace.iter_mut() {
            wrap!(rep.n, rep.n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::StructWith(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_struct_with_node(cx, &self.source, &self.replace)
    }
}

#[derive(Debug)]
pub struct StructRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub source: Node<R, E>,
    pub sorted_field_idx: Option<usize>,
    pub field_name: ArcStr,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> StructRef<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let source = decode_node(ctx, buf)?;
        let sorted_field_idx = Option::<usize>::decode(buf)?;
        let field_name = ArcStr::decode(buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            source,
            sorted_field_idx,
            field_name,
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
        field_name: &ArcStr,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let (typ, sorted_field_idx) = match &source.typ() {
            Type::Struct(flds) => {
                flds.iter()
                    .enumerate()
                    .find_map(|(i, (n, t, _))| {
                        if field_name == n { Some((t.clone(), Some(i))) } else { None }
                    })
                    .unwrap_or_else(|| (Type::empty_tvar(), None))
            }
            _ => (Type::empty_tvar(), None),
        };
        let field_name = field_name.clone();
        Ok(Node::new(Self {
            spec,
            typ,
            source,
            sorted_field_idx,
            field_name,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StructRef<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::StructRef, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.source.image_encode(buf)?;
        self.sorted_field_idx.encode(buf)?;
        self.field_name.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.source.update(ctx);
        let tag = tv.tag();
        if tag.is_bottom() {
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
        let res = tv.with_value(|v| match (v, self.sorted_field_idx) {
            (Value::Array(a), Some(i)) => a.get(i).and_then(|v| match v {
                Value::Array(a) if a.len() == 2 => Some(a[1].clone()),
                _ => None,
            }),
            _ => None,
        });
        match res {
            Some(v) => self.resident.set(TagValue::tagged(v, tag)),
            None => self.resident.ride(),
        }
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.sleep(ctx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.source], ctx)
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::StructRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        let sorted_idx = self
            .sorted_field_idx
            .ok_or_else(|| anyhow!("emit_clif: struct field index unresolved"))?;
        emit_struct_ref_node(cx, &self.source, sorted_idx, &self.typ)
    }
}

#[derive(Debug)]
pub struct Tuple<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    fork: ForkSite,
}

impl<R: Rt, E: UserEvent> Tuple<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
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
        let typ = Type::Tuple(Arc::from_iter(n.iter().map(|n| n.typ().clone())));
        Ok(Node::new(Self {
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Tuple<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self {
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Tuple<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Tuple, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }
    composite_plumbing!(Tuple);

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let (vals, tag) = gathered!(self, ctx, Value::Array(ValArray::from([])));
        let v = Value::Array(ValArray::from_iter_exact(vals));
        self.resident.set(TagValue::tagged(v, tag))
    }

    super::typed_by_row!();

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_tuple_new_node(cx, &self.n)
    }
}

#[derive(Debug)]
pub struct Variant<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub tag: ArcStr,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    fork: ForkSite,
}

impl<R: Rt, E: UserEvent> Variant<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        tag: &ArcStr,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        let typs = Arc::from_iter(n.iter().map(|n| n.typ().clone()));
        let typ = Type::Variant(tag.clone(), typs, WrittenAt::NOWHERE);
        let tag = ctx.tag(tag);
        Ok(Node::new(Self {
            spec,
            typ,
            tag,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Variant<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let tag = ctx.tag(&ArcStr::decode(buf)?);
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self {
            spec,
            typ,
            tag,
            n,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Variant<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Variant, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.tag.encode(buf)?;
        encode_nodes(&self.n, buf)
    }
    composite_plumbing!(Variant);

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let (vals, tag) = gathered!(self, ctx, Value::String(self.tag.clone()));
        let a = iter::once(Value::String(self.tag.clone())).chain(vals);
        let v = Value::Array(ValArray::from_iter(a));
        self.resident.set(TagValue::tagged(v, tag))
    }

    super::typed_by_row!();

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_variant_new_node(cx, &self.tag, &self.n)
    }
}

/// `T(v)`: the constructor of an abstract type — boxes its argument with
/// the type's tag. Compiles only where the definition is visible.
#[derive(Debug)]
pub struct Construct<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub id: AbstractId,
    pub name: ArcStr,
    /// The representation at this instance's parameters — what `arg`
    /// must be contained by.
    pub rep: Type,
    pub arg: Node<R, E>,
    /// `typ`'s parameters as the checker left them, taken at the first
    /// construction: what every value minted here carries.
    params: Option<Arc<[Type]>>,
    resident: TagValue,
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
}

impl<R: Rt, E: UserEvent> Construct<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let id = AbstractId::decode(buf)?;
        let name = ArcStr::decode(buf)?;
        let rep = Type::decode(buf)?;
        let arg = decode_node(ctx, buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            id,
            name,
            rep,
            arg,
            params: None,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        arg: &Expr,
    ) -> Result<Node<R, E>> {
        let arg = compile(ctx, flags, arg.clone(), scope, top_id)?;
        let td = ctx
            .env
            .lookup_typedef(&scope.lexical, name)
            .at(&spec)?
            .ok_or_else(|| anyhow!("unknown type {name}").at(&spec))?;
        let Type::Abstract { id, .. } = td.typ() else {
            bailat!(spec, "{name} is not an abstract type, so it has no constructor")
        };
        let id = *id;
        let Some(r) = ctx.env.abstract_rep(id, &scope.lexical) else {
            bailat!(
                spec,
                "the definition of {name} is not visible here, so it cannot be constructed"
            )
        };
        let (typ, rep) = r.instantiate(id);
        let name = r.name.clone();
        Ok(Node::new(Self {
            spec,
            typ,
            id,
            name,
            rep,
            arg,
            params: None,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Construct<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Construct, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.id.encode(buf)?;
        self.name.encode(buf)?;
        self.rep.encode(buf)?;
        self.arg.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.arg.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let params = self.params.get_or_insert_with(|| match &self.typ.resolve_tvars() {
            Type::Abstract { params, .. } => params.clone(),
            _ => Arc::from_iter([]),
        });
        let v = abstract_value::wrap(
            self.id,
            self.name.clone(),
            params.clone(),
            tv.value_cloned(),
        );
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.arg.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.arg.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.arg.sleep(ctx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.arg], ctx)
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.arg, self.arg.typecheck1(ctx))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Construct(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_construct_node(cx, &self.typ, &self.name, &self.arg)
    }
}

/// The type of `.field` over `source`: a tuple's field, an error's
/// payload, or an abstract type's payload where its definition is
/// visible from `scope`.
pub(crate) fn tuple_field_type<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    scope: &ModPath,
    source: &Type,
    field: usize,
) -> Result<Type> {
    deref_typ!("tuple", ctx, source,
        Some(Type::Tuple(flds)) => flds
            .get(field)
            .map(|t| t.clone())
            .ok_or_else(|| anyhow!("in tuple, no such field {}", field)),
        Some(Type::Error(t)) => {
            if field != 0 {
                bail!("no such field {}", field);
            }
            Ok((**t).clone())
        },
        Some(Type::Abstract { id, params }) => {
            if field != 0 {
                bail!("no such field {}: an abstract type has only its payload .0", field);
            }
            match ctx.env.abstract_rep(*id, scope) {
                Some(r) => Ok(r.instantiate_with(params)),
                None => bail!(
                    "the definition of this abstract type is not visible here, so \
                     its payload cannot be read"
                ),
            }
        }
    )
}

/// The sorted position and type of the field `name` over `source`, a
/// struct.
pub(crate) fn struct_field_type<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    source: &Type,
    name: &ArcStr,
) -> Result<(usize, Type)> {
    deref_typ!("struct", ctx, source,
        Some(Type::Struct(flds)) => {
            match flds.iter().enumerate().find(|(_, (n, _, _))| n == name) {
                Some((i, (_, t, _))) => Ok((i, t.clone())),
                None => bail!("in struct, unknown field {name}"),
            }
        }
    )
}

#[derive(Debug)]
pub struct TupleRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub source: Node<R, E>,
    pub field: usize,
    scope: ModPath,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> TupleRef<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let source = decode_node(ctx, buf)?;
        let field = usize::decode(buf)?;
        let scope = ModPath::decode(buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            source,
            field,
            scope,
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
        field: &usize,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let field = *field;
        let typ = match &source.typ() {
            Type::Tuple(ts) => {
                ts.get(field).map(|t| t.clone()).unwrap_or_else(Type::empty_tvar)
            }
            Type::Error(t) => (**t).clone(),
            Type::Abstract { id, params } if field == 0 => ctx
                .env
                .abstract_rep(*id, &scope.lexical)
                .map(|r| r.instantiate_with(params))
                .unwrap_or_else(Type::empty_tvar),
            _ => Type::empty_tvar(),
        };
        let scope = scope.lexical.clone();
        Ok(Node::new(Self {
            spec,
            typ,
            source,
            field,
            scope,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TupleRef<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TupleRef, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.source.image_encode(buf)?;
        self.field.encode(buf)?;
        self.scope.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.source.update(ctx);
        let tag = tv.tag();
        if tag.is_bottom() {
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
        let v = tv.value_cloned();
        let res = match v {
            Value::Array(a) => a.get(self.field).map(|v| v.clone()),
            Value::Error(v) => Some((*v).clone()),
            Value::Abstract(_) if self.field == 0 => {
                abstract_value::get(&v).map(|g| g.payload.clone())
            }
            _ => None,
        };
        match res {
            Some(v) => self.resident.set(TagValue::tagged(v, tag)),
            None => self.resident.ride(),
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.source], ctx)
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TupleRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // CR claude for claude: [perf] with_deref does not expand a Type::Ref. So an
        // abstract source typed by its typedef name (a field declared `c: Counter`,
        // read as `w.c.0`) takes the tuple read, emit_accessor_source_node refuses it,
        // and the whole region node-walks, although the node-walk and tuple_field_type
        // both handle this source. `|w: W| -> i64 #[native] (w.c.0 + w.n)` with `type W
        // = {c: Counter, n: i64}` is refused. The refusal prints the TypeRef with {:?},
        // including the program's whole source text (fusion/emit/nodes.rs:915). Decide
        // between the two reads on the expanded type (expand_refs, as
        // emit_tuple_ref_node does for its element type), and print that reason's type
        // with Display. probe: design/review-2026-10-05/repro/c-data-map-05.gx
        // (c-data-map-05)
        let abstract_source =
            self.source.typ().with_deref(|t| matches!(t, Some(Type::Abstract { .. })));
        if abstract_source {
            emit_abstract_ref_node(cx, &self.source, &self.typ)
        } else {
            emit_tuple_ref_node(cx, &self.source, self.field, &self.typ)
        }
    }
}

impl<R: Rt, E: UserEvent> Struct<R, E> {
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
        match &self.typ {
            Type::Struct(typs) => {
                if self.n.len() != typs.len() {
                    bail!(
                        "struct length mismatch {} fields expected vs {}",
                        typs.len(),
                        self.n.len()
                    )
                }
                for ((_, t, _), n) in typs.iter().zip(self.n.iter()) {
                    wrap!(n, t.check_contains(&ctx.env, &n.typ()))?
                }
            }
            _ => bail!("BUG: expected a struct rtype"),
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> StructWith<R, E> {
    /// Each replacement's field index is state: an instance finds it too.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        // Clone the type out of `with_deref` before unifying: the closure
        // holds TVar read guards that the writes below would deadlock on.
        // CR claude for claude: [bug] deref_cloned looks through bound cells only. A
        // source typed by a typedef name (a field or tuple element declared `p: Point`
        // is Type::Ref) or by a union that collapses only when normalized therefore
        // hits `expected a struct`, while `(w.p).x` on the same source passes through
        // struct_field_type's deref_typ!. `{(w.p) with x: 3.0}` and the nested update
        // `{s with cursor: {(s.cursor) with row: ..}}` are refused. A let-bound copy
        // works only because a pattern bind expands a top-level ref. Look each
        // replacement up with struct_field_type, which returns clones so no guard is
        // held across the child check, and give emit_struct_with_node
        // (fusion/emit/nodes.rs:788) the same expansion so the newly accepted sources
        // still fuse. probe: design/review-2026-10-05/repro/c-data-map-02.gx
        // (c-data-map-02)
        let styp = self.source.typ().deref_cloned();
        let mut fields = || -> Result<()> {
            match &styp {
                Some(Type::Struct(flds)) => {
                    for rep in self.replace.iter_mut() {
                        let r =
                            flds.iter().enumerate().find_map(|(i, (field, typ, _))| {
                                if field == &rep.name { Some((i, typ)) } else { None }
                            });
                        match r {
                            None => bail!("struct has no field named {}", rep.name),
                            Some((i, typ)) => {
                                wrap!(rep.n, child(&mut rep.n, ctx))?;
                                if check {
                                    wrap!(
                                        rep.n,
                                        typ.check_contains(&ctx.env, &rep.n.typ())
                                    )?;
                                }
                                rep.index = Some(i);
                            }
                        }
                    }
                    Ok(())
                }
                None => bail!("type must be known, annotations needed"),
                _ => bail!("expected a struct"),
            }
        };
        wrap!(self, fields())?;
        match check {
            true => wrap!(self, self.typ.check_contains(&ctx.env, self.source.typ())),
            false => Ok(()),
        }
    }
}

impl<R: Rt, E: UserEvent> StructRef<R, E> {
    /// The field's index is state: an instance finds it too.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        let etyp = struct_field_type(ctx, self.source.typ(), &self.field_name);
        let (idx, typ) = wrap!(self, etyp)?;
        self.sorted_field_idx = Some(idx);
        if let ExprKind::StructRef { field, .. } = &self.spec.kind
            && ctx.env.ide.is_lsp()
        {
            ctx.env.push_field_ref(crate::ide::FieldRefSite {
                pos: field.pos_or(self.spec.pos),
                ori: self.spec.ori.clone(),
                name: field.name.clone(),
                typ: typ.clone(),
            });
        }
        match check {
            true => wrap!(self, self.typ.check_contains(&ctx.env, &typ)),
            false => Ok(()),
        }
    }
}

impl<R: Rt, E: UserEvent> Tuple<R, E> {
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
        match &self.typ {
            Type::Tuple(typs) => {
                if self.n.len() != typs.len() {
                    bail!("tuple arity mismatch {} vs {}", self.n.len(), typs.len())
                }
                for (t, n) in typs.iter().zip(self.n.iter()) {
                    wrap!(n, t.check_contains(&ctx.env, &n.typ()))?
                }
            }
            _ => bail!("BUG: unexpected tuple rtype"),
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Variant<R, E> {
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
        match &self.typ {
            Type::Variant(ttag, typs, _) => {
                if ttag != &self.tag {
                    bail!("expected {ttag} not {}", self.tag)
                }
                if self.n.len() != typs.len() {
                    bail!("arity mismatch {} vs {}", self.n.len(), typs.len())
                }
                for (t, n) in typs.iter().zip(self.n.iter()) {
                    wrap!(n, t.check_contains(&ctx.env, &n.typ()))?
                }
            }
            _ => bail!("BUG: unexpected variant rtype"),
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Construct<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.arg, child(&mut self.arg, ctx))?;
        match check {
            true => wrap!(self.arg, self.rep.check_contains(&ctx.env, &self.arg.typ())),
            false => Ok(()),
        }
    }
}

impl<R: Rt, E: UserEvent> TupleRef<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        if !check {
            return Ok(());
        }
        let etyp = tuple_field_type(ctx, &self.scope, self.source.typ(), self.field);
        let etyp = wrap!(self, etyp)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &etyp))
    }
}
