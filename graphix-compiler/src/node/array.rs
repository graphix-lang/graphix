use super::{WakeBit, compiler::compile, dense_gate, gather, list, produce_constant};
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, TagValue, Update,
    UserEvent, defetyp,
    env::Env,
    err,
    expr::{Expr, ExprId},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, emit_array_ref_node, emit_array_slice_node,
            emit_list_new_node, emit_tuple_new_node,
        },
    },
    image::{
        ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, opt_node_decode,
            opt_node_encode, put_tag,
        },
    },
    typ::Type,
    wrap,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{PBytes, Typ, ValArray, Value};
use poolshark::local::LPooled;
use std::{fmt::Debug, iter, marker::PhantomData, ops::Range};
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "ArrayIndexError", "Error<`{}(string)>");

/// An array index or slice bound: any integer. An open cell is narrowed
/// to the integers, as an arithmetic operand is to the numbers.
pub(super) fn check_index<R: Rt, E: UserEvent>(env: &Env, i: &Node<R, E>) -> Result<()> {
    wrap!(i, super::op::constrain_operand(env, &Type::Primitive(Typ::integer()), i.typ()))
}

#[derive(Debug)]
pub struct ArrayRef<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R, E>,
    pub i: Node<R, E>,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub etyp: Type,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> ArrayRef<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let source = decode_node(ctx, buf)?;
        let i = decode_node(ctx, buf)?;
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let etyp = Type::decode(buf)?;
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            source,
            i,
            spec,
            typ,
            etyp,
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
        i: &Expr,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let i = compile(ctx, flags, i.clone(), scope, top_id)?;
        let etyp = match &source.typ() {
            Type::Array(et) => (**et).clone(),
            Type::Primitive(p) if *p == Typ::Bytes => Type::Primitive(Typ::U8.into()),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([etyp.clone(), ERR.clone()]));
        Ok(Node::new(Self {
            source,
            i,
            spec,
            typ,
            etyp,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        }))
    }
}

/// An integer index or slice bound as an `i64`, the form both engines
/// index with. An unsigned value above `i64::MAX` saturates: it is out
/// of bounds for any length and never counts from the end.
pub(crate) fn index_i64(v: &Value) -> Option<i64> {
    match v {
        Value::I64(i) | Value::Z64(i) => Some(*i),
        Value::U64(u) | Value::V64(u) => Some(i64::try_from(*u).unwrap_or(i64::MAX)),
        v => v.clone().cast_to::<i64>().ok(),
    }
}

/// The offset `i` names in a sequence of `len` elements: non-negative
/// counts from the start, negative from the end. Unchecked against `len`.
fn offset(len: usize, i: i64) -> Option<usize> {
    usize::try_from(if i < 0 { len as i64 + i } else { i }).ok()
}

/// The position index `i` names in a sequence of `len` elements.
/// An indexed or sliced source is bytes only when its type says so; a
/// source not known yet (an open cell) is assumed to be an array.
fn known_bytes(env: &Env, source: &Type) -> Result<bool> {
    let bytes = Type::Primitive(Typ::Bytes.into());
    Ok(!source.with_deref(|t| t.is_none())
        && bytes.contains_with_flags(BitFlags::empty(), env, source)?)
}

// CR claude for claude: [readability] index has no doc; its line, 'The position index `i`
// names in a sequence of `len` elements.', sits at 121 as the first line of
// known_bytes' doc, so known_bytes is summarized by a sentence about index. Move it
// here. (c-collection-09)
pub(crate) fn index(len: usize, i: i64) -> Option<usize> {
    offset(len, i).filter(|j| *j < len)
}

/// The range `[start..end]` names in a sequence of `len` elements; an
/// absent bound is the start or the end.
fn slice_range(len: usize, start: Option<i64>, end: Option<i64>) -> Option<Range<usize>> {
    let bound = |b: Option<i64>, absent| match b {
        None => Some(absent),
        Some(i) => offset(len, i).filter(|j| *j <= len),
    };
    match (bound(start, 0), bound(end, len)) {
        (Some(i), Some(j)) if i <= j => Some(i..j),
        _ => None,
    }
}

/// `array[i]`, shared by the node-walk and the JIT: the bare element,
/// or the `ArrayIndexError` value when out of bounds.
pub(crate) fn array_index(elts: &ValArray, i: i64) -> Value {
    match index(elts.len(), i) {
        Some(i) => elts[i].clone(),
        None => err!(ERR_TAG, "array index out of bounds"),
    }
}

/// `bytes[i]`, with the rules of [`array_index`]: the `Value::U8` or
/// the out-of-bounds error.
pub(crate) fn bytes_index(b: &PBytes, i: i64) -> Value {
    match index(b.len(), i) {
        Some(i) => Value::U8(b[i]),
        None => err!(ERR_TAG, "array index out of bounds"),
    }
}

/// `a[i..j]` / `a[i..]` / `a[..j]` / `a[..]` over an array or bytes,
/// shared by the node-walk and the JIT: the sub-array / sub-bytes, or
/// the `ArrayIndexError` value for a bound out of range.
pub(crate) fn array_slice(src: &Value, start: Option<i64>, end: Option<i64>) -> Value {
    let v = match src {
        Value::Array(elts) => slice_range(elts.len(), start, end)
            .and_then(|r| elts.subslice(r).ok())
            .map(Value::Array),
        Value::Bytes(b) => slice_range(b.len(), start, end)
            .map(|r| Value::Bytes(PBytes::new(b.slice(r)))),
        _ => return err!(ERR_TAG, "expected array"),
    };
    v.unwrap_or_else(|| err!(ERR_TAG, "slice out of bounds"))
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArrayRef<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArrayRef, buf);
        self.source.image_encode(buf)?;
        self.i.image_encode(buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.etyp.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let s = self.source.update(ctx);
        let i = self.i.update(ctx);
        let tag = s.tag().join(i.tag());
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let v = s.with_value(|s| match (s, i.with_value(index_i64)) {
            (_, None) => err!(ERR_TAG, "expected an integer"),
            (Value::Array(elts), Some(i)) => array_index(elts, i),
            (Value::Bytes(b), Some(i)) => bytes_index(b, i),
            (_, Some(_)) => err!(ERR_TAG, "expected an array"),
        });
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
        wrap!(self.i, self.i.typecheck1(ctx))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.i.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.delete(ctx);
        self.i.delete(ctx);
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
        self.i.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.source, &mut self.i], ctx)
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ArrayRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_array_ref_node(cx, &self.source, &self.i)
    }
}

#[derive(Debug)]
pub struct ArraySlice<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R, E>,
    pub start: Option<Node<R, E>>,
    pub end: Option<Node<R, E>>,
    pub(crate) spec: Expr,
    pub typ: Type,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> ArraySlice<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let source = decode_node(ctx, buf)?;
        let start = opt_node_decode(ctx, buf)?;
        let end = opt_node_decode(ctx, buf)?;
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            source,
            start,
            end,
            spec,
            typ,
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
        start: &Option<Arc<Expr>>,
        end: &Option<Arc<Expr>>,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let start = start
            .as_ref()
            .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
            .transpose()?;
        let end = end
            .as_ref()
            .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
            .transpose()?;
        let typ = Type::Set(Arc::from_iter([source.typ().clone(), ERR.clone()]));
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            source,
            start,
            end,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArraySlice<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArraySlice, buf);
        self.source.image_encode(buf)?;
        opt_node_encode(self.start.as_ref(), buf)?;
        opt_node_encode(self.end.as_ref(), buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let s = self.source.update(ctx);
        let start = self.start.as_mut().map(|n| n.update(ctx));
        let end = self.end.as_mut().map(|n| n.update(ctx));
        let tag = [start, end].iter().flatten().fold(s.tag(), |t, b| t.join(b.tag()));
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let bound = |b: Option<&TagValue>| {
            b.map(|b| b.with_value(index_i64).ok_or(())).transpose()
        };
        let v = match (bound(start), bound(end)) {
            (Ok(start), Ok(end)) => s.with_value(|s| array_slice(s, start, end)),
            _ => err!(ERR_TAG, "expected an integer"),
        };
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
        if let Some(start) = self.start.as_mut() {
            wrap!(start, start.typecheck1(ctx))?;
        }
        if let Some(end) = self.end.as_mut() {
            wrap!(end, end.typecheck1(ctx))?;
        }
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        if let Some(start) = &self.start {
            start.refs(refs)
        }
        if let Some(end) = &self.end {
            end.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.source.delete(ctx);
        if let Some(start) = &mut self.start {
            start.delete(ctx);
        }
        if let Some(end) = &mut self.end {
            end.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.source.sleep(ctx);
        if let Some(start) = &mut self.start {
            start.sleep(ctx);
        }
        if let Some(end) = &mut self.end {
            end.sleep(ctx);
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(
            iter::once(&mut self.source)
                .chain(self.start.as_mut())
                .chain(self.end.as_mut()),
            ctx,
        )
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ArraySlice(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_array_slice_node(cx, &self.source, self.start.as_ref(), self.end.as_ref())
    }
}

/// What a [`SeqLit`] builds: an array or a native list.
pub trait SeqKind: Debug + Send + Sync + 'static {
    /// A native list, else an array.
    const LIST: bool;

    /// The literal's type over its element type.
    fn typ(elem: Type) -> Type;

    /// The literal's value over its elements, in order.
    fn build(elems: impl ExactSizeIterator<Item = Value>) -> Value;

    fn empty() -> Value;

    fn view<R: Rt, E: UserEvent>(n: &SeqLit<R, E, Self>) -> NodeView<'_, R, E>
    where
        Self: Sized;

    fn emit<R: Rt, E: UserEvent>(
        cx: &mut BodyCx,
        n: &[Node<R, E>],
    ) -> Result<CompiledExpr>;
}

#[derive(Debug)]
pub struct ArrayKind;

impl SeqKind for ArrayKind {
    const LIST: bool = false;

    fn typ(elem: Type) -> Type {
        Type::Array(Arc::new(elem))
    }

    fn build(elems: impl ExactSizeIterator<Item = Value>) -> Value {
        Value::Array(ValArray::from_iter_exact(elems))
    }

    fn empty() -> Value {
        Value::Array(ValArray::from([]))
    }

    fn view<R: Rt, E: UserEvent>(n: &Array<R, E>) -> NodeView<'_, R, E> {
        NodeView::Array(n)
    }

    fn emit<R: Rt, E: UserEvent>(
        cx: &mut BodyCx,
        n: &[Node<R, E>],
    ) -> Result<CompiledExpr> {
        // the runtime shape is a tuple literal's
        emit_tuple_new_node(cx, n)
    }
}

#[derive(Debug)]
pub struct ListKind;

impl SeqKind for ListKind {
    const LIST: bool = true;

    fn typ(elem: Type) -> Type {
        Type::List(Arc::new(elem))
    }

    fn build(elems: impl ExactSizeIterator<Item = Value>) -> Value {
        list::from_iter(elems)
    }

    fn empty() -> Value {
        list::nil()
    }

    fn view<R: Rt, E: UserEvent>(n: &ListLit<R, E>) -> NodeView<'_, R, E> {
        NodeView::ListLit(n)
    }

    fn emit<R: Rt, E: UserEvent>(
        cx: &mut BodyCx,
        n: &[Node<R, E>],
    ) -> Result<CompiledExpr> {
        emit_list_new_node(cx, n)
    }
}

pub type Array<R, E> = SeqLit<R, E, ArrayKind>;
pub type ListLit<R, E> = SeqLit<R, E, ListKind>;

/// An array or list literal, `[a, b]` or `[<a, b>]`.
#[derive(Debug)]
pub struct SeqLit<R: Rt, E: UserEvent, K: SeqKind> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    kind: PhantomData<K>,
    fork: crate::cost::ForkSite,
}

impl<R: Rt, E: UserEvent, K: SeqKind> SeqLit<R, E, K> {
    fn with(spec: Expr, typ: Type, n: Box<[Node<R, E>]>) -> Node<R, E> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            kind: PhantomData,
            fork: Default::default(),
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[Expr]>,
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        Ok(Self::with(spec, K::typ(Type::empty_tvar()), n))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Self::with(spec, typ, n))
    }
}

impl<R: Rt, E: UserEvent, K: SeqKind> Update<R, E> for SeqLit<R, E, K> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(if K::LIST { NodeTag::ListLit } else { NodeTag::Array }, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        if self.n.is_empty() {
            return produce_constant(ctx.event, &mut self.resident, K::empty);
        }
        let (tag, prods) = gather(ctx, &mut self.n, &mut self.fork);
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let v = K::build(prods.into_iter().map(|tv| tv.value_cloned()));
        self.resident.set(TagValue::tagged(v, tag))
    }

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

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        for n in &mut self.n {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        K::view(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        K::emit(cx, &self.n)
    }
}

impl<R: Rt, E: UserEvent> ArrayRef<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        wrap!(self.i, child(&mut self.i, ctx))?;
        let source_typ = self.source.typ();
        if known_bytes(&ctx.env, source_typ)? {
            let byte = Type::Primitive(Typ::U8.into());
            wrap!(self, self.etyp.check_contains(&ctx.env, &byte))?;
        } else {
            let at = Type::Array(Arc::new(self.etyp.clone()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        match check {
            true => check_index(&ctx.env, &self.i),
            false => Ok(()),
        }
    }
}

impl<R: Rt, E: UserEvent> ArraySlice<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        let source_typ = self.source.typ();
        if check && !known_bytes(&ctx.env, source_typ)? {
            let at = Type::Array(Arc::new(Type::empty_tvar()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        // `typ` copied the source's type at compile; a source whose type
        // is decided in its typecheck0 (a select) is related here
        let Type::Set(members) = &self.typ else { unreachable!() };
        wrap!(self, members[0].check_contains(&ctx.env, source_typ))?;
        for i in [self.start.as_mut(), self.end.as_mut()].into_iter().flatten() {
            wrap!(i, child(i, ctx))?;
            if check {
                check_index(&ctx.env, i)?;
            }
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent, K: SeqKind> SeqLit<R, E, K> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        for n in &mut self.n {
            wrap!(n, child(n, ctx))?
        }
        if !check {
            return Ok(());
        }
        let bottom = Type::Bottom;
        let mut ts: LPooled<Vec<&Type>> = LPooled::take();
        ts.push(&bottom);
        ts.extend(self.n.iter().map(|n| n.typ()));
        let rtype = match wrap!(self, Type::union(&ctx.env, &ts))? {
            Type::Bottom => K::typ(Type::empty_tvar()),
            t => K::typ(t),
        };
        Ok(self.typ.check_contains(&ctx.env, &rtype)?)
    }
}
