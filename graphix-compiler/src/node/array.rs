use super::{WakeBit, compiler::compile, dense_gate, gather, list, produce_constant};
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, defetyp,
    env::Env,
    err,
    expr::{Expr, ExprId},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, emit_array_ref_node, emit_array_slice_node,
            emit_list_new_node, emit_seq_new_node,
        },
    },
    image::{
        ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, opt_node_decode,
            opt_node_encode, put_tag,
        },
    },
    typ::{
        Type,
        bounds::{self, Use},
    },
    wrap,
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{PBytes, Typ, ValArray, Value};
use std::{fmt::Debug, iter, marker::PhantomData, ops::Range};
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "ArrayIndexError", "Error<`{}(string)>");

/// An array index or slice bound: any integer. An open cell is narrowed
/// to the integers, as an arithmetic operand is to the numbers.
pub(super) fn check_index<R: Rt>(env: &Env, i: &Node<R>) -> Result<()> {
    wrap!(i, super::op::constrain_operand(env, &Type::Primitive(Typ::integer()), i.typ()))
}

#[derive(Debug)]
pub struct ArrayRef<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    // CR claude for claude: [readability/low] The doc above names a mechanism the tree
    // no longer has: wake catch-up went with bind-driven bodies, and what this bit does
    // is make the first update after a sleep recompute instead of riding the resident
    // (`dense_gate!`). Thirteen `slept` fields in node/array.rs, data.rs, map.rs, op.rs
    // and lambda.rs carry the same line. Delete the comments, since the type and the
    // macro say it, or write what the bit does. Found by reading; not run.
    // (a-compiler-10)
    slept: WakeBit,
    pub source: Node<R>,
    pub i: Node<R>,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub etyp: Type,
    resident: TagValue,
}

impl<R: Rt> ArrayRef<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        etyp: Type,
        source: Node<R>,
        i: Node<R>,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            source,
            i,
            spec,
            typ,
            etyp,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let source = decode_node(ctx, buf)?;
        let i = decode_node(ctx, buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let etyp = Type::decode(buf)?;
        Ok(Self::with(spec, typ, etyp, source, i))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        i: &Expr,
    ) -> Result<Node<R>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let i = compile(ctx, flags, i.clone(), scope, top_id)?;
        let etyp = match &source.typ() {
            Type::Array(et) => (**et).clone(),
            Type::Primitive(p) if *p == Typ::Bytes => Type::Primitive(Typ::U8.into()),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([etyp.clone(), ERR.clone()]));
        Ok(Self::with(spec, typ, etyp, source, i))
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

/// An indexed or sliced source is bytes only when its type says so; a
/// source not known yet (an open cell) is assumed to be an array.
fn known_bytes(env: &Env, source: &Type) -> Result<bool> {
    let bytes = Type::Primitive(Typ::Bytes.into());
    Ok(!source.with_deref(|t| t.is_none())
        && bytes.contains_with_flags(BitFlags::empty(), env, source)?)
}

/// The position index `i` names in a sequence of `len` elements.
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

/// `src[i]`, shared by the node-walk and the JIT: [`array_index`] or
/// [`bytes_index`] by the source's form, else the `ArrayIndexError`.
pub(crate) fn value_index(src: &Value, i: i64) -> Value {
    match src {
        Value::Array(elts) => array_index(elts, i),
        Value::Bytes(b) => bytes_index(b, i),
        _ => err!(ERR_TAG, "expected an array"),
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

impl<R: Rt> Update<R> for ArrayRef<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.source);
        f(&self.i)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.source);
        f(&mut self.i)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArrayRef, buf);
        self.source.image_encode(buf)?;
        self.i.image_encode(buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.etyp.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let s = self.source.update(ctx);
        let i = self.i.update(ctx);
        let tag = s.tag().join(i.tag());
        dense_gate!(self, tag, tag.is_bottom());
        let v = s.with_value(|s| match i.with_value(index_i64) {
            None => err!(ERR_TAG, "expected an integer"),
            Some(i) => value_index(s, i),
        });
        self.resident.set(TagValue::tagged(v, tag))
    }

    super::typed_by_row!();

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.source.sleep(ctx);
        self.i.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.source, &mut self.i], ctx)
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::ArrayRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_array_ref_node(cx, &self.source, &self.i)
    }
}

#[derive(Debug)]
pub struct ArraySlice<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R>,
    pub start: Option<Node<R>>,
    pub end: Option<Node<R>>,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    resident: TagValue,
}

impl<R: Rt> ArraySlice<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        source: Node<R>,
        start: Option<Node<R>>,
        end: Option<Node<R>>,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            source,
            start,
            end,
            spec,
            typ,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let source = decode_node(ctx, buf)?;
        let start = opt_node_decode(ctx, buf)?;
        let end = opt_node_decode(ctx, buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        Ok(Self::with(spec, typ, source, start, end))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        start: &Option<Arc<Expr>>,
        end: &Option<Arc<Expr>>,
    ) -> Result<Node<R>> {
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
        Ok(Self::with(spec, typ, source, start, end))
    }
}

impl<R: Rt> Update<R> for ArraySlice<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.source);
        self.start.iter().chain(self.end.iter()).for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.source);
        self.start.iter_mut().chain(self.end.iter_mut()).for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArraySlice, buf);
        self.source.image_encode(buf)?;
        opt_node_encode(self.start.as_ref(), buf)?;
        opt_node_encode(self.end.as_ref(), buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let s = self.source.update(ctx);
        let start = self.start.as_mut().map(|n| n.update(ctx));
        let end = self.end.as_mut().map(|n| n.update(ctx));
        let tag = [start, end].iter().flatten().fold(s.tag(), |t, b| t.join(b.tag()));
        dense_gate!(self, tag, tag.is_bottom());
        let bound = |b: Option<&TagValue>| {
            b.map(|b| b.with_value(index_i64).ok_or(())).transpose()
        };
        let v = match (bound(start), bound(end)) {
            (Ok(start), Ok(end)) => s.with_value(|s| array_slice(s, start, end)),
            _ => err!(ERR_TAG, "expected an integer"),
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    super::typed_by_row!();

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.source.sleep(ctx);
        if let Some(start) = &mut self.start {
            start.sleep(ctx);
        }
        if let Some(end) = &mut self.end {
            end.sleep(ctx);
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
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

    fn view(&self) -> NodeView<'_, R> {
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

    fn view<R: Rt>(n: &SeqLit<R, Self>) -> NodeView<'_, R>
    where
        Self: Sized;

    fn emit<R: Rt>(cx: &mut BodyCx, n: &[Node<R>]) -> Result<CompiledExpr>;
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

    fn view<R: Rt>(n: &Array<R>) -> NodeView<'_, R> {
        NodeView::Array(n)
    }

    fn emit<R: Rt>(cx: &mut BodyCx, n: &[Node<R>]) -> Result<CompiledExpr> {
        emit_seq_new_node(cx, None, n)
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

    fn view<R: Rt>(n: &ListLit<R>) -> NodeView<'_, R> {
        NodeView::ListLit(n)
    }

    fn emit<R: Rt>(cx: &mut BodyCx, n: &[Node<R>]) -> Result<CompiledExpr> {
        emit_list_new_node(cx, n)
    }
}

pub type Array<R> = SeqLit<R, ArrayKind>;
pub type ListLit<R> = SeqLit<R, ListKind>;

/// An array or list literal, `[a, b]` or `[<a, b>]`.
#[derive(Debug)]
pub struct SeqLit<R: Rt, K: SeqKind> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R>]>,
    resident: TagValue,
    kind: PhantomData<K>,
    pub(crate) fork: crate::cost::ForkSite,
}

impl<R: Rt, K: SeqKind> SeqLit<R, K> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        n: Box<[Node<R>]>,
        fork: crate::cost::ForkSite,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            kind: PhantomData,
            fork,
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[Expr]>,
    ) -> Result<Node<R>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        Ok(Self::with(spec, K::typ(Type::empty_tvar()), n, Default::default()))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Self::with(spec, typ, n, crate::cost::ForkSite::image_decode(buf)?))
    }
}

impl<R: Rt, K: SeqKind> Update<R> for SeqLit<R, K> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.n.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        self.n.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(if K::LIST { NodeTag::ListLit } else { NodeTag::Array }, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)?;
        self.fork.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        if self.n.is_empty() {
            return produce_constant(ctx.event, &mut self.resident, K::empty);
        }
        let (tag, prods) = gather(ctx, &mut self.n, &mut self.fork);
        dense_gate!(self, tag, tag.is_bottom());
        let v = K::build(prods.into_iter().map(|tv| tv.value_cloned()));
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R> {
        K::view(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        K::emit(cx, &self.n)
    }
}

impl<R: Rt> ArrayRef<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        wrap!(self.i, child(&mut self.i, ctx))?;
        // an instance's type is its row
        if !check {
            return Ok(());
        }
        let source_typ = self.source.typ();
        if bounds::active() {
            // whatever the source turns out to be gives the element: an
            // array's, or a byte
            if check {
                let bytes = Type::Primitive(Typ::Bytes.into());
                let indexed =
                    bounds::union_of([Type::Array(Arc::new(self.etyp.clone())), bytes]);
                wrap!(self, indexed.check_contains(&ctx.env, source_typ))?;
                let read = Use::Element { out: self.etyp.clone() };
                wrap!(self, super::solving(|s| s.watch(&ctx.env, source_typ, read)))?;
                check_index(&ctx.env, &self.i)?
            }
            return Ok(());
        }
        if known_bytes(&ctx.env, source_typ)? {
            let byte = Type::Primitive(Typ::U8.into());
            wrap!(self, self.etyp.check_contains(&ctx.env, &byte))?;
        } else {
            let at = Type::Array(Arc::new(self.etyp.clone()));
            // CR claude for eric: [bug/low] An index over a source typed as a union of
            // two array types is refused here with "Array<i64> does not contain
            // [Array<i64>, Array<string>]": the element variable takes the first member
            // and the second fails, so the message states a requirement nobody wrote
            // and names nothing to annotate. ArraySlice refuses the same source, and
            // array::len and array::map refuse it with the same shape of message. A
            // tuple union's `.0` reads the union of its members and a slice pattern
            // over this same value checks, so the value can be taken apart but not
            // indexed or measured. Either relate each array member separately and union
            // the element types into etyp, or refuse in words that say to select on the
            // value or give it one array type. probe:
            // design/review-2026-11-16/repro/x-engine-collections-index-union-of-arrays.gx
            // (x-engine-collections-03)
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        match check {
            true => check_index(&ctx.env, &self.i),
            false => Ok(()),
        }
    }
}

impl<R: Rt> ArraySlice<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        for i in [self.start.as_mut(), self.end.as_mut()].into_iter().flatten() {
            wrap!(i, child(i, ctx))?;
        }
        // an instance's type is its row
        if !check {
            return Ok(());
        }
        let source_typ = self.source.typ();
        if check {
            let sliced = bounds::union_of([
                Type::Array(Arc::new(Type::empty_tvar())),
                Type::Primitive(Typ::Bytes.into()),
            ]);
            wrap!(self, sliced.check_contains(&ctx.env, source_typ))?
        }
        // `typ` copied the source's type at compile; a source whose type
        // is decided in its typecheck0 (a select) is related here
        let Type::Set(members) = &self.typ else { unreachable!() };
        wrap!(self, members[0].check_contains(&ctx.env, source_typ))?;
        match check {
            true => [self.start.as_ref(), self.end.as_ref()]
                .into_iter()
                .flatten()
                .try_for_each(|i| check_index(&ctx.env, i)),
            false => Ok(()),
        }
    }
}

impl<R: Rt, K: SeqKind> SeqLit<R, K> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        for n in &mut self.n {
            wrap!(n, child(n, ctx))?
        }
        if !check {
            return Ok(());
        }
        // CR claude for claude: [perf/low] A tuple, struct, array or map is keyed here by
        // its Arc address. The element types of separately written literals (`[{name: "a",
        // v: 1}, {name: "b", v: 2}, ..]`) therefore never dedupe, and every one reaches
        // Type::union_of_alternatives. A literal of 5000/10000/20000 struct rows checks in
        // 162/369/1128 ms, against 89/97/114 ms for the same count of integers. The
        // x-perf-compile-08 fix made only primitives linear. Fix: key a composite by its
        // discriminant and a hash of its parts' SameType keys. Drop the third u64, which is
        // 0 in every arm. Map literals stay superlinear too (80000 entries 2.3 s, whether
        // the keys or the values are uniform), so Map::typecheck0_with's path has a
        // per-entry cost of its own to find. probe:
        // design/review-2026-11-16/repro/x-rerun-1-literal-composite-union-quadratic.sh
        // (x-rerun-1-02)
        // 2026-11-18 claude: On bounds-spike the literal's old union path (element_union,
        // SameType) is deleted with the old check: the elements flow into one solver cell.
        // The cost this measured is not re-measured under the solver: run the probe again,
        // literals and map literals both.

        // the elements flow into one cell, which may join them
        let (Type::Array(e) | Type::List(e)) = &self.typ else {
            bail!("BUG: a literal that is no array and no list")
        };
        for n in self.n.iter() {
            wrap!(n, e.check_contains(&ctx.env, n.typ()))?
        }
        Ok(())
    }
}
