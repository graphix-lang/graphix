use super::{WakeBit, compiler::compile, dense_gate, gather, list, produce_constant};
use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, TagValue, Update, UserEvent,
    defetyp,
    env::Env,
    err, errf,
    expr::{Expr, ExprId},
    fusion::emit::{
        BodyCx, CompiledExpr, emit_array_ref_node, emit_array_slice_node,
        emit_list_new_node, emit_tuple_new_node,
    },
    image::{
        ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, nodes_len, opt_node_decode,
            opt_node_encode, opt_node_len, put_tag, tag_len,
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
use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::Bound::{Excluded, Included, Unbounded},
};
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "ArrayIndexError", "Error<`{}(string)>");

/// An array index or slice bound: any integer.
pub(super) fn check_index<R: Rt, E: UserEvent>(env: &Env, i: &Node<R, E>) -> Result<()> {
    wrap!(i, Type::Primitive(Typ::integer()).check_contains(env, i.typ()))
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
        ctx: &mut ExecCtx<R, E>,
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
        ctx: &mut ExecCtx<R, E>,
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

/// The position index `i` names in a sequence of `len` elements:
/// non-negative counts from the start, negative from the end.
pub(crate) fn index(len: usize, i: i64) -> Option<usize> {
    let j = if i < 0 { len as i64 + i } else { i };
    usize::try_from(j).ok().filter(|j| *j < len)
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
/// the `ArrayIndexError` value for a negative or out-of-range bound.
pub(crate) fn array_slice(src: &Value, start: Option<i64>, end: Option<i64>) -> Value {
    let bound = |b: Option<i64>| b.map(usize::try_from).transpose();
    let (Ok(start), Ok(end)) = (bound(start), bound(end)) else {
        return err!(ERR_TAG, "a slice bound must not be negative");
    };
    match src {
        Value::Array(elts) => {
            let range =
                (start.map_or(Unbounded, Included), end.map_or(Unbounded, Excluded));
            match elts.subslice(range) {
                Ok(a) => Value::Array(a),
                Err(e) => errf!(ERR_TAG, "{e}"),
            }
        }
        Value::Bytes(b) => {
            let (i, j) = (start.unwrap_or(0), end.unwrap_or(b.len()));
            if i <= j && j <= b.len() {
                Value::Bytes(PBytes::new(b.slice(i..j)))
            } else {
                err!(ERR_TAG, "slice out of bounds")
            }
        }
        _ => err!(ERR_TAG, "expected array"),
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArrayRef<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.source.image_len()
            + self.i.image_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.etyp.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArrayRef, buf);
        self.source.image_encode(buf)?;
        self.i.image_encode(buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.etyp.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let s = self.source.update(ctx, event);
        let i = self.i.update(ctx, event);
        let tag = s.tag().join(i.tag());
        dense_gate!(self, ctx, tag.triggers(), tag.is_bottom());
        let v = s.with_value(|s| match (s, i.with_value(index_i64)) {
            (_, None) => err!(ERR_TAG, "expected an integer"),
            (Value::Array(elts), Some(i)) => array_index(elts, i),
            (Value::Bytes(b), Some(i)) => bytes_index(b, i),
            (_, Some(_)) => err!(ERR_TAG, "expected an array"),
        });
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        wrap!(self.i, self.i.typecheck0(ctx))?;
        let bytes_typ = Type::Primitive(Typ::Bytes.into());
        let source_typ = self.source.typ();
        if bytes_typ.contains_with_flags(BitFlags::empty(), &ctx.env, source_typ)? {
            let byte = Type::Primitive(Typ::U8.into());
            wrap!(self, self.etyp.check_contains(&ctx.env, &byte))?;
        } else {
            // if we don't already know it's a bytes, assume it will be an array
            let at = Type::Array(Arc::new(self.etyp.clone()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        check_index(&ctx.env, &self.i)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        wrap!(self.i, self.i.typecheck1(ctx))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.i.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx);
        self.i.delete(ctx);
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        self.source.sleep(ctx);
        self.i.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.reset_replay(ctx);
        self.i.reset_replay(ctx);
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
        ctx: &mut ExecCtx<R, E>,
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
        ctx: &mut ExecCtx<R, E>,
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
    fn image_len(&self) -> usize {
        tag_len()
            + self.source.image_len()
            + opt_node_len(self.start.as_ref())
            + opt_node_len(self.end.as_ref())
            + self.spec.encoded_len()
            + self.typ.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArraySlice, buf);
        self.source.image_encode(buf)?;
        opt_node_encode(self.start.as_ref(), buf)?;
        opt_node_encode(self.end.as_ref(), buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let s = self.source.update(ctx, event);
        let start = self.start.as_mut().map(|n| n.update(ctx, event));
        let end = self.end.as_mut().map(|n| n.update(ctx, event));
        let tag = [start, end].iter().flatten().fold(s.tag(), |t, b| t.join(b.tag()));
        dense_gate!(self, ctx, tag.triggers(), tag.is_bottom());
        let bound = |b: Option<&TagValue>| {
            b.map(|b| b.with_value(index_i64).ok_or(())).transpose()
        };
        let v = match (bound(start), bound(end)) {
            (Ok(start), Ok(end)) => s.with_value(|s| array_slice(s, start, end)),
            _ => err!(ERR_TAG, "expected an integer"),
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        let bytes_typ = Type::Primitive(Typ::Bytes.into());
        let source_typ = self.source.typ();
        if !bytes_typ.contains_with_flags(BitFlags::empty(), &ctx.env, source_typ)? {
            // if we don't already know it's bytes, assume it will be an array
            let at = Type::Array(Arc::new(Type::empty_tvar()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        if let Some(start) = self.start.as_mut() {
            wrap!(start, start.typecheck0(ctx))?;
            check_index(&ctx.env, start)?;
        }
        if let Some(end) = self.end.as_mut() {
            wrap!(end, end.typecheck0(ctx))?;
            check_index(&ctx.env, end)?;
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx);
        if let Some(start) = &mut self.start {
            start.delete(ctx);
        }
        if let Some(end) = &mut self.end {
            end.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        self.source.sleep(ctx);
        if let Some(start) = &mut self.start {
            start.sleep(ctx);
        }
        if let Some(end) = &mut self.end {
            end.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.reset_replay(ctx);
        if let Some(start) = &mut self.start {
            start.reset_replay(ctx);
        }
        if let Some(end) = &mut self.end {
            end.reset_replay(ctx);
        }
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
        })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Self::with(spec, typ, n))
    }
}

impl<R: Rt, E: UserEvent, K: SeqKind> Update<R, E> for SeqLit<R, E, K> {
    fn image_len(&self) -> usize {
        tag_len() + self.spec.encoded_len() + self.typ.encoded_len() + nodes_len(&self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(if K::LIST { NodeTag::ListLit } else { NodeTag::Array }, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.n.is_empty() {
            return produce_constant(ctx, event, &mut self.resident, K::empty);
        }
        let (tag, prods) = gather(ctx, event, &mut self.n);
        dense_gate!(self, ctx, tag.triggers(), tag.is_bottom());
        let v = K::build(prods.into_iter().map(|tv| tv.value_cloned()));
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in &mut self.n {
            wrap!(n, n.typecheck0(ctx))?
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

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
