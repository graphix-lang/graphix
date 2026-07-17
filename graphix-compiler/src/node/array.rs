use super::{Cached, compiler::compile};
use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent, defetyp, err, errf,
    expr::{Expr, ExprId},
    fusion::emit::{
        BodyCx, CompiledExpr, emit_array_ref_node, emit_array_slice_node,
        emit_tuple_new_node,
    },
    typ::Type,
    wrap,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::{PBytes, Typ, ValArray, Value};
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "ArrayIndexError", "Error<`{}(string)>");

#[derive(Debug)]
pub struct ArrayRef<R: Rt, E: UserEvent> {
    pub source: Cached<R, E>,
    pub i: Cached<R, E>,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub etyp: Type,
}

impl<R: Rt, E: UserEvent> ArrayRef<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        i: &Expr,
    ) -> Result<Node<R, E>> {
        let source = Cached::new(compile(ctx, flags, source.clone(), scope, top_id)?);
        let i = Cached::new(compile(ctx, flags, i.clone(), scope, top_id)?);
        let etyp = match &source.node.typ() {
            Type::Array(et) => (**et).clone(),
            Type::Primitive(p) if *p == Typ::Bytes => Type::Primitive(Typ::U8.into()),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([etyp.clone(), ERR.clone()]));
        Ok(Box::new(Self { source, i, spec, typ, etyp }))
    }
}

/// The runtime semantics of `array[i]` — the single source of truth
/// shared by the node-walk ([`ArrayRef::update`]) and the JIT (via
/// `graphix_valarray_index`). On success returns the bare element; on
/// The largest `n` `array::init(n, f)` will build — 16M elements
/// (256MB of `Value`s). Beyond it BOTH evaluators log and produce
/// bottom, like the other hot-path failures: `init(i64:MAX, …)` used
/// to capacity-overflow-panic the node-walk's slot Vec (ABORTING the
/// process — the fuzzer's crash corpus) and would panic the JIT's
/// result-buffer reserve the same way. One shared constant so the two
/// backends bottom at the same length.
pub const MAX_ARRAY_INIT_LEN: i64 = 16 * 1024 * 1024;

/// an out-of-bounds index returns the `ArrayIndexError` value (the
/// access type is `[elem, Error<…>]`, so callers wrap the result in
/// `Nullable<elem>`).
///
/// Negative indices count from the end: `a[-1]` is the last element and
/// `a[-len]` is the first (element 0). The shared single source of
/// truth, so node-walk / JIT agree bit-for-bit.
pub(crate) fn array_index(elts: &ValArray, i: i64) -> Value {
    if i >= 0 {
        let i = i as usize;
        if i < elts.len() {
            elts[i].clone()
        } else {
            err!(ERR_TAG, "array index out of bounds")
        }
    } else {
        let i = elts.len() as i64 + i;
        if i >= 0 {
            elts[i as usize].clone()
        } else {
            err!(ERR_TAG, "array index out of bounds")
        }
    }
}

/// Shared `bytes[i]` indexing — the same bounds-check / negative-from-end
/// rule as [`array_index`], returning `Value::U8` or the out-of-bounds
/// error (the access type is `[u8, Error<…>]` → `Nullable<u8>`). Single
/// source of truth for node-walk / JIT.
pub(crate) fn bytes_index(b: &PBytes, i: i64) -> Value {
    let idx = if i >= 0 { i } else { b.len() as i64 + i };
    if idx >= 0 && (idx as usize) < b.len() {
        Value::U8(b[idx as usize])
    } else {
        err!(ERR_TAG, "index out of bounds")
    }
}

/// Shared `a[i..j]` / `a[i..]` / `a[..j]` / `a[..]` slicing for arrays
/// and bytes, given pre-parsed `usize` bounds. Returns the sub-array /
/// sub-bytes or an error (`[T, Error<…>]` → `Nullable<T>`). Single source
/// of truth for node-walk / JIT.
pub(crate) fn array_slice(
    src: &Value,
    start: Option<usize>,
    end: Option<usize>,
) -> Value {
    match src {
        Value::Array(elts) => match (start, end) {
            (None, None) => Value::Array(elts.clone()),
            (Some(i), Some(j)) => match elts.subslice(i..j) {
                Ok(a) => Value::Array(a),
                Err(e) => errf!(ERR_TAG, "{e:?}"),
            },
            (Some(i), None) => match elts.subslice(i..) {
                Ok(a) => Value::Array(a),
                Err(e) => errf!(ERR_TAG, "{e:?}"),
            },
            (None, Some(j)) => match elts.subslice(..j) {
                Ok(a) => Value::Array(a),
                Err(e) => errf!(ERR_TAG, "{e:?}"),
            },
        },
        Value::Bytes(b) => match (start, end) {
            (None, None) => Value::Bytes(b.clone()),
            (Some(i), Some(j)) if i <= j && j <= b.len() => {
                Value::Bytes(PBytes::new(b.slice(i..j)))
            }
            (Some(i), None) if i <= b.len() => Value::Bytes(PBytes::new(b.slice(i..))),
            (None, Some(j)) if j <= b.len() => Value::Bytes(PBytes::new(b.slice(..j))),
            _ => err!(ERR_TAG, "slice out of bounds"),
        },
        _ => err!(ERR_TAG, "expected array"),
    }
}

/// `array_slice` with `i64` bounds (the fused-kernel representation).
/// A negative bound wraps to `usize::MAX` via `i as usize`, exactly
/// matching the node-walk's `cast_to::<usize>()` (whose `FromValue for
/// usize` does `Value::I64(v) => Ok(v as usize)`). So an out-of-range
/// (incl. negative) bound surfaces the SAME "out of bounds" error in
/// both backends — this is just `array_slice` (the node-walk's own
/// function) with the i64→usize wrap applied. Shared by the JIT
/// slice path.
pub(crate) fn array_slice_i64(
    src: &Value,
    start: Option<i64>,
    end: Option<i64>,
) -> Value {
    array_slice(src, start.map(|i| i as usize), end.map(|i| i as usize))
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArrayRef<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let s = self.source.update(ctx, event);
        let ip = self.i.update(ctx, event);
        if s.is_none() && ip.is_none() {
            return None;
        }
        if self.source.tag.is_tainted() || self.i.tag.is_tainted() {
            return Some(TagValue::tainted(Value::Null));
        }
        let fired = s.is_some_and(|t| t.is_fired()) || ip.is_some_and(|t| t.is_fired());
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let i = match &self.i.cached {
            Some(Value::I64(i)) => *i,
            Some(v) => match v.clone().cast_to::<i64>() {
                Ok(i) => i,
                Err(_) => {
                    return Some(TagValue::tagged(
                        err!(ERR_TAG, "expected an integer"),
                        tag,
                    ));
                }
            },
            None => return None,
        };
        match &self.source.cached {
            Some(Value::Array(elts)) => Some(TagValue::tagged(array_index(elts, i), tag)),
            Some(Value::Bytes(b)) => Some(TagValue::tagged(bytes_index(b, i), tag)),
            None => None,
            _ => Some(TagValue::tagged(err!(ERR_TAG, "expected an array"), tag)),
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck0(ctx))?;
        wrap!(self.i.node, self.i.node.typecheck0(ctx))?;
        let int = Type::Primitive(Typ::integer());
        let bytes_typ = Type::Primitive(Typ::Bytes.into());
        let source_typ = self.source.node.typ();
        if bytes_typ.contains_with_flags(BitFlags::empty(), &ctx.env, source_typ)? {
            let byte = Type::Primitive(Typ::U8.into());
            wrap!(self, self.etyp.check_contains(&ctx.env, &byte))?;
        } else {
            // if we don't already know it's a bytes, assume it will be an array
            let at = Type::Array(Arc::new(self.etyp.clone()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        wrap!(self.i.node, int.check_contains(&ctx.env, self.i.node.typ()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck1(ctx))?;
        wrap!(self.i.node, self.i.node.typecheck1(ctx))?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.node.refs(refs);
        self.i.node.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.node.delete(ctx);
        self.i.node.delete(ctx);
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
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
        emit_array_ref_node(cx, &self.source.node, &self.i.node)
    }
}

#[derive(Debug)]
pub struct ArraySlice<R: Rt, E: UserEvent> {
    pub source: Cached<R, E>,
    pub start: Option<Cached<R, E>>,
    pub end: Option<Cached<R, E>>,
    pub(crate) spec: Expr,
    pub typ: Type,
}

impl<R: Rt, E: UserEvent> ArraySlice<R, E> {
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
        let source = Cached::new(compile(ctx, flags, source.clone(), scope, top_id)?);
        let start = start
            .as_ref()
            .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id).map(Cached::new))
            .transpose()?;
        let end = end
            .as_ref()
            .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id).map(Cached::new))
            .transpose()?;
        let typ = Type::Set(Arc::from_iter([source.node.typ().clone(), ERR.clone()]));
        Ok(Box::new(Self { spec, typ, source, start, end }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArraySlice<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let s = self.source.update(ctx, event);
        let st = self.start.as_mut().and_then(|c| c.update(ctx, event));
        let en = self.end.as_mut().and_then(|c| c.update(ctx, event));
        if s.is_none() && st.is_none() && en.is_none() {
            return None;
        }
        if self.source.tag.is_tainted()
            || self.start.as_ref().is_some_and(|c| c.tag.is_tainted())
            || self.end.as_ref().is_some_and(|c| c.tag.is_tainted())
        {
            return Some(TagValue::tainted(Value::Null));
        }
        let fired = s.is_some_and(|t| t.is_fired())
            || st.is_some_and(|t| t.is_fired())
            || en.is_some_and(|t| t.is_fired());
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        macro_rules! number {
            ($e:expr) => {
                match $e.clone().cast_to::<usize>() {
                    Ok(i) => i,
                    Err(_) => {
                        return Some(TagValue::tagged(
                            err!(ERR_TAG, "expected a non negative number"),
                            tag,
                        ));
                    }
                }
            };
        }
        macro_rules! bound {
            ($bound:expr) => {{
                match $bound.cached.as_ref() {
                    None => return None,
                    Some(Value::U64(i) | Value::V64(i)) => Some(*i as usize),
                    Some(v) => Some(number!(v)),
                }
            }};
        }
        let (start, end) = match (&self.start, &self.end) {
            (None, None) => (None, None),
            (Some(c), None) => (bound!(c), None),
            (None, Some(c)) => (None, bound!(c)),
            (Some(c0), Some(c1)) => (bound!(c0), bound!(c1)),
        };
        match &self.source.cached {
            Some(src) => Some(TagValue::tagged(array_slice(src, start, end), tag)),
            None => None,
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck0(ctx))?;
        let it = Type::Primitive(Typ::integer());
        let bytes_typ = Type::Primitive(Typ::Bytes.into());
        let source_typ = self.source.node.typ();
        if !bytes_typ.contains_with_flags(BitFlags::empty(), &ctx.env, source_typ)? {
            // if we don't already know it's bytes, assume it will be an array
            let at = Type::Array(Arc::new(Type::empty_tvar()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        if let Some(start) = self.start.as_mut() {
            wrap!(start.node, start.node.typecheck0(ctx))?;
            wrap!(start.node, it.check_contains(&ctx.env, &start.node.typ()))?;
        }
        if let Some(end) = self.end.as_mut() {
            wrap!(end.node, end.node.typecheck0(ctx))?;
            wrap!(end.node, it.check_contains(&ctx.env, &end.node.typ()))?;
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck1(ctx))?;
        if let Some(start) = self.start.as_mut() {
            wrap!(start.node, start.node.typecheck1(ctx))?;
        }
        if let Some(end) = self.end.as_mut() {
            wrap!(end.node, end.node.typecheck1(ctx))?;
        }
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.node.refs(refs);
        if let Some(start) = &self.start {
            start.node.refs(refs)
        }
        if let Some(end) = &self.end {
            end.node.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.node.delete(ctx);
        if let Some(start) = &mut self.start {
            start.node.delete(ctx);
        }
        if let Some(end) = &mut self.end {
            end.node.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
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
        emit_array_slice_node(
            cx,
            &self.source.node,
            self.start.as_ref().map(|c| &c.node),
            self.end.as_ref().map(|c| &c.node),
        )
    }
}

#[derive(Debug)]
pub struct Array<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Cached<R, E>]>,
}

impl<R: Rt, E: UserEvent> Array<R, E> {
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
            .map(|e| Ok(Cached::new(compile(ctx, flags, e.clone(), scope, top_id)?)))
            .collect::<Result<_>>()?;
        let typ = Type::Array(Arc::new(Type::empty_tvar()));
        Ok(Box::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Array<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        if self.n.is_empty() {
            // Empty producer = a constant: FIRED at init, the STALE
            // value channel inside frames (the Constant frame rule —
            // a per-site instance's `let res = []` seed died after
            // frame resets and its For bottomed on the missing init,
            // firing-jul2026/03).
            // Frame depth first — frames force init (see Constant).
            if ctx.frame_depth > 0 {
                return Some(if ctx.frame_init {
                    TagValue::fired(Value::Array(ValArray::from([])))
                } else {
                    TagValue::stale(Value::Array(ValArray::from([])))
                });
            } else if event.init {
                return Some(TagValue::fired(Value::Array(ValArray::from([]))));
            }
            return None;
        }
        let mut produced = false;
        let mut fired = false;
        let mut determined = true;
        for c in self.n.iter_mut() {
            if let Some(t) = c.update(ctx, event) {
                produced = true;
                fired |= t.is_fired();
            }
            determined &= c.cached.is_some();
        }
        if produced && determined {
            if self.n.iter().any(|c| c.tag.is_tainted()) {
                return Some(TagValue::tainted(Value::Null));
            }
            let tag = if fired { Tag::FIRED } else { Tag::STALE };
            let iter = self.n.iter().map(|n| n.cached.clone().unwrap());
            Some(TagValue::tagged(Value::Array(ValArray::from_iter_exact(iter)), tag))
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
        self.n.iter_mut().for_each(|n| n.node.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.node.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in &mut self.n {
            wrap!(n.node, n.node.typecheck0(ctx))?
        }
        let rtype = Type::Bottom;
        let rtype = wrap!(
            self,
            self.n
                .iter()
                .fold(Ok(rtype), |rtype, n| n.node.typ().union(&ctx.env, &rtype?))
        )?;
        let rtype = match rtype {
            Type::Bottom => Type::Array(Arc::new(Type::empty_tvar())),
            t => Type::Array(Arc::new(t)),
        };
        Ok(self.typ.check_contains(&ctx.env, &rtype)?)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in &mut self.n {
            wrap!(n.node, n.node.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Array(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // `[a, b, c]` — the runtime shape (a flat ValArray) is
        // identical to a tuple literal's; share the producer relay.
        emit_tuple_new_node(cx, &self.n)
    }
}
