use super::{WakeBit, compiler::compile, dense_gate};
use crate::cost::ForkSite;
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, UserEvent,
    abstract_value, bailat, deref_typ,
    expr::{At, Expr, ExprId, ExprKind, ModPath, WrittenAt},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, FieldOf, emit_abstract_ref_node, emit_construct_node,
            emit_field_ref_node, emit_seq_new_node, emit_struct_new_node,
            emit_struct_with_node, emit_variant_new_node,
        },
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    rep,
    typ::{AbstractId, Type},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
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

        fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
            self.n.iter().for_each(f)
        }

        fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
            self.n.iter_mut().for_each(f)
        }

        fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
            self.slept.set();
            self.n.iter_mut().for_each(|n| n.sleep(ctx))
        }

        fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
            fusion::fuse_parts(self.n.iter_mut(), ctx)
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
            return $crate::node::produce_constant(
                $ctx.event,
                &mut $self.resident,
                || $empty,
            );
        }
        let (tag, prods) = $crate::node::gather($ctx, &mut $self.n, &mut $self.fork);
        dense_gate!($self, tag, tag.is_bottom());
        (prods.into_iter().map(|tv| tv.value_cloned()), tag)
    }};
}
pub(super) use {composite_plumbing, gathered};

#[derive(Debug)]
pub struct Struct<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
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
        spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
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
        if self.n.is_empty() {
            return super::produce_constant(ctx.event, &mut self.resident, || {
                rep::structure_of_fields([])
            });
        }
        let fields = self.n.len();
        let (tag, prods) = super::gather(ctx, &mut self.n, &mut self.fork);
        let woke = self.slept.take();
        dense_gate!(self.resident, tag, tag.is_bottom(), woke);
        // a field that did not fire holds what its pair in the resident
        // holds, unless a wake recomputed it
        let held = match self.resident.is_bottom() || woke {
            true => None,
            false => self.resident.with_value(|v| {
                rep::struct_fields(v)
                    .is_some_and(|f| f.len() == fields)
                    .then(|| v.clone())
            }),
        };
        let held = held.as_ref().and_then(rep::struct_fields);
        let iter =
            self.names.iter().zip(prods).enumerate().map(|(i, (name, tv))| match held {
                Some(held) if !tv.tag().triggers() => held[i].clone(),
                _ => rep::make_field(name.clone(), tv.value_cloned()),
            });
        let v = rep::structure_of_fields(iter);
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
    pub(crate) spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let source = decode_node(ctx, buf)?;
        let n = crate::image::count_decode(buf)?;
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
        spec: Arc<Expr>,
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.source);
        self.replace.iter().for_each(|r| f(&r.n))
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.source);
        self.replace.iter_mut().for_each(|r| f(&mut r.n))
    }

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
        let shaped = src.with_value(|v| rep::struct_fields(v).is_some());
        dense_gate!(self, tag, tag.is_bottom() || !shaped);
        let v = src.with_value(|src| {
            let src = rep::struct_fields(src).expect("gated on the shape");
            let mut fields: LPooled<Vec<Value>> = src.iter().cloned().collect();
            for (i, v) in index.iter().zip(vals.iter()) {
                if let Some(i) = *i
                    && let Some((name, _)) = rep::field_at(src, i)
                {
                    fields[i] = rep::make_field(name.clone(), v.value_cloned());
                }
            }
            rep::structure_of_fields(fields.drain(..))
        });
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
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

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::StructWith(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_struct_with_node(cx, &self.source, &self.replace)
    }
}

#[derive(Debug)]
pub struct StructRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
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
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        field_name: &ArcStr,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        // what the source's type says now; the check decides the rest
        let (typ, sorted_field_idx) =
            match struct_field_type(ctx, source.typ(), field_name) {
                Ok((i, t)) => (t, Some(i)),
                Err(_) => (Type::empty_tvar(), None),
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.source)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.source)
    }

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
        let res = tv.with_value(|v| {
            let fields = rep::struct_fields(v)?;
            rep::field_at(fields, self.sorted_field_idx?).map(|(_, v)| v.clone())
        });
        match res {
            Some(v) => self.resident.set(TagValue::tagged(v, tag)),
            None => self.resident.ride(),
        }
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

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::StructRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        let sorted_idx = self
            .sorted_field_idx
            .ok_or_else(|| anyhow!("emit_clif: struct field index unresolved"))?;
        emit_field_ref_node(cx, &self.source, sorted_idx, &self.typ, FieldOf::Struct)
    }
}

#[derive(Debug)]
pub struct Tuple<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    fork: ForkSite,
}

impl<R: Rt, E: UserEvent> Tuple<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
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
        let (vals, tag) = gathered!(self, ctx, rep::tuple([]));
        let v = rep::tuple(vals);
        self.resident.set(TagValue::tagged(v, tag))
    }

    super::typed_by_row!();

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_seq_new_node(cx, Some(rep::Kind::Tuple), &self.n)
    }
}

#[derive(Debug)]
pub struct Variant<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
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
        spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
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
        let (vals, tag) = gathered!(self, ctx, rep::make_variant(self.tag.clone(), []));
        let v = rep::make_variant(self.tag.clone(), vals);
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
    pub(crate) spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
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
        spec: Arc<Expr>,
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.arg)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.arg)
    }

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
        dense_gate!(self, tag, tag.is_bottom());
        // CR claude for eric: [bug/medium] When the check never decides a type
        // parameter of the constructed type (top-level `Bx((1, []))` with `type Bx<'a>
        // = Abstract<(i64, Array<'a>)>`), the params resolve to an unbound cell, and
        // every value minted here carries that live checker cell. The core-trait lookup
        // (coretraits `impl_for`) refuses any type with an unbound cell. So `impl<'a>
        // Eq`/`Display`/`Ord for Bx<'a>` are silently skipped: `==` compares
        // structurally and printing shows the raw form. Inside a function, or after an
        // unrelated statement such as a `map::insert` of the value settles 'a, the same
        // impls apply. emit_construct_node mints the same params, so both engines agree
        // on the wrong answer. Needs a rule: either freeze the params at construction
        // (an undecided cell becomes ⊥ or its bound, through one helper shared with
        // emit_construct_node), or let `impl_for` treat an open param in a value's type
        // as matching any generic impl head. The rule also decides whether a concrete
        // `impl Eq for Bx<i64>` covers such a value. Pin Eq, Ord (map key) and Display
        // either way. probe: design/review-2026-10-12/repro/c-data-map-01.sh
        // (c-data-map-01)
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.arg.sleep(ctx)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.arg], ctx)
    }

    super::typed_by_row!();

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
    // tuples of one arity: the field's type is each one's
    if let Some(members) = union_of(ctx, source, |t| match t {
        Type::Tuple(flds) => Some(flds.clone()),
        _ => None,
    })? && members.windows(2).all(|w| w[0].len() == w[1].len())
    {
        let ts = members
            .iter()
            .map(|flds| flds.get(field).cloned())
            .collect::<Option<LPooled<Vec<Type>>>>()
            .ok_or_else(|| anyhow!("in tuple, no such field {}", field))?;
        let ts: LPooled<Vec<&Type>> = ts.iter().collect();
        return Type::union(&ctx.env, &ts);
    }
    // errors: the field is each one's payload
    if let Some(members) = union_of(ctx, source, |t| match t {
        Type::Error(t) => Some((**t).clone()),
        _ => None,
    })? {
        if field != 0 {
            bail!("no such field {}", field);
        }
        let ts: LPooled<Vec<&Type>> = members.iter().collect();
        return Type::union(&ctx.env, &ts);
    }
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
    let field = |flds: &[(ArcStr, Type, WrittenAt)]| match flds
        .iter()
        .enumerate()
        .find(|(_, (n, _, _))| n == name)
    {
        Some((i, (_, t, _))) => Ok((i, t.clone())),
        None => bail!("in struct, unknown field {name}"),
    };
    // structs of one shape: the field sits at one index in each
    if let Some(members) = union_of(ctx, source, |t| match t {
        Type::Struct(flds) => Some(flds.clone()),
        _ => None,
    })? && members.windows(2).all(|w| same_names(&w[0], &w[1]))
    {
        let mut index = 0;
        let mut ts: LPooled<Vec<Type>> = LPooled::take();
        for flds in members.iter() {
            let (i, t) = field(flds)?;
            index = i;
            ts.push(t);
        }
        let ts: LPooled<Vec<&Type>> = ts.iter().collect();
        return Ok((index, Type::union(&ctx.env, &ts)?));
    }
    deref_typ!("struct", ctx, source,
        Some(Type::Struct(flds)) => field(flds)
    )
}

/// The members of `t`, a union once expanded, each expanded and taken
/// apart by `part`, when every one of them is that.
fn union_of<R: Rt, E: UserEvent, T>(
    ctx: &CompileCtx<R, E>,
    t: &Type,
    part: impl Fn(&Type) -> Option<T>,
) -> Result<Option<LPooled<Vec<T>>>> {
    let expand = |t: &Type| -> Result<Option<Type>> {
        let mut t = t.with_deref(|t| t.cloned());
        while let Some(r @ Type::Ref(_)) = &t {
            t = Some(r.lookup_ref(&ctx.env)?);
        }
        Ok(t)
    };
    let set = expand(t)?;
    let Some(Type::Set(members)) = &set else { return Ok(None) };
    let mut out: LPooled<Vec<T>> = LPooled::take();
    for m in members.iter() {
        match expand(m)?.as_ref().and_then(&part) {
            Some(p) => out.push(p),
            None => return Ok(None),
        }
    }
    Ok(Some(out))
}

fn same_names(a: &[(ArcStr, Type, WrittenAt)], b: &[(ArcStr, Type, WrittenAt)]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|((n0, _, _), (n1, _, _))| n0 == n1)
}

#[derive(Debug)]
pub struct TupleRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
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
        let spec = Arc::new(Expr::decode(buf)?);
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
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        field: &usize,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let field = *field;
        // what the source's type says now; the check decides the rest
        let typ = tuple_field_type(ctx, &scope.lexical, source.typ(), field)
            .unwrap_or_else(|_| Type::empty_tvar());
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.source)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.source)
    }

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
        let res = match &v {
            Value::Array(_) => {
                rep::tuple_elems(&v).and_then(|e| e.get(self.field)).cloned()
            }
            Value::Error(v) => Some((**v).clone()),
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.source], ctx)
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TupleRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // an abstract's payload and an error's read alike
        let payload_source =
            crate::fusion::emit::resolve_node_typ(cx.ctx, self.source.typ()).with_deref(
                |t| matches!(t, Some(Type::Abstract { .. } | Type::Error(_))),
            );
        if payload_source {
            emit_abstract_ref_node(cx, &self.source, &self.typ)
        } else {
            emit_field_ref_node(cx, &self.source, self.field, &self.typ, FieldOf::Tuple)
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
        let styp = self.source.typ().clone();
        let top_id = self.spec.id;
        let mut fields = || -> Result<()> {
            for rep in self.replace.iter_mut() {
                let (i, typ) = struct_field_type(ctx, &styp, &rep.name)?;
                wrap!(rep.n, child(&mut rep.n, ctx))?;
                if check {
                    wrap!(rep.n, typ.check_contains(&ctx.env, &rep.n.typ()))?;
                    wrap!(
                        rep.n,
                        super::callsite::check_hidden_defaults(
                            ctx,
                            BitFlags::empty(),
                            &Scope::root(),
                            top_id,
                            &typ,
                            &rep.n.typ(),
                            super::callsite::Narrowing::Copy
                        )
                    )?;
                }
                rep.index = Some(i);
            }
            Ok(())
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
