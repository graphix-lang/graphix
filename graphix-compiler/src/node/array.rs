use super::{compiler::compile, Cached};
use crate::{
    defetyp, err, errf,
    expr::{Expr, ExprId},
    typ::Type,
    update_args, wrap, CFlag, Event, ExecCtx, Node, Refs, Rt, Scope, Update, UserEvent,
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
/// shared by the node-walk ([`ArrayRef::update`]), the GIR interpreter
/// ([`crate::gir_interp`]), and the JIT (via
/// `graphix_valarray_index`). On success returns the bare element; on
/// an out-of-bounds index returns the `ArrayIndexError` value (the
/// access type is `[elem, Error<…>]`, so callers wrap the result in
/// `Nullable<elem>`).
///
/// Negative indices count from the end: `a[-1]` is the last element and
/// `a[-len]` is the first (element 0). The shared single source of
/// truth, so node-walk / gir-interp / JIT agree bit-for-bit.
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
/// source of truth for node-walk / gir-interp / JIT.
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
/// of truth for node-walk / gir-interp / JIT.
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
/// (incl. negative) bound surfaces the SAME "out of bounds" error in all
/// three backends — this is just `array_slice` (the node-walk's own
/// function) with the i64→usize wrap applied. Shared by the gir-interp
/// and JIT slice paths.
pub(crate) fn array_slice_i64(
    src: &Value,
    start: Option<i64>,
    end: Option<i64>,
) -> Value {
    array_slice(src, start.map(|i| i as usize), end.map(|i| i as usize))
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArrayRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let up = self.source.update(ctx, event);
        let up = self.i.update(ctx, event) || up;
        if !up {
            return None;
        }
        let i = match &self.i.cached {
            Some(Value::I64(i)) => *i,
            Some(v) => match v.clone().cast_to::<i64>() {
                Ok(i) => i,
                Err(_) => return Some(err!(ERR_TAG, "expected an integer")),
            },
            None => return None,
        };
        match &self.source.cached {
            Some(Value::Array(elts)) => Some(array_index(elts, i)),
            Some(Value::Bytes(b)) => Some(bytes_index(b, i)),
            None => None,
            _ => Some(err!(ERR_TAG, "expected an array")),
        }
    }

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck(ctx))?;
        wrap!(self.i.node, self.i.node.typecheck(ctx))?;
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

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::ArrayRef(self)
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        macro_rules! number {
            ($e:expr) => {
                match $e.clone().cast_to::<usize>() {
                    Ok(i) => i,
                    Err(_) => return Some(err!(ERR_TAG, "expected a non negative number")),
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
        let up = self.source.update(ctx, event);
        let up = self.start.as_mut().map(|c| c.update(ctx, event)).unwrap_or(false) || up;
        let up = self.end.as_mut().map(|c| c.update(ctx, event)).unwrap_or(false) || up;
        if !up {
            return None;
        }
        let (start, end) = match (&self.start, &self.end) {
            (None, None) => (None, None),
            (Some(c), None) => (bound!(c), None),
            (None, Some(c)) => (None, bound!(c)),
            (Some(c0), Some(c1)) => (bound!(c0), bound!(c1)),
        };
        match &self.source.cached {
            Some(src) => Some(array_slice(src, start, end)),
            None => None,
        }
    }

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source.node, self.source.node.typecheck(ctx))?;
        let it = Type::Primitive(Typ::integer());
        let bytes_typ = Type::Primitive(Typ::Bytes.into());
        let source_typ = self.source.node.typ();
        if !bytes_typ.contains_with_flags(BitFlags::empty(), &ctx.env, source_typ)? {
            // if we don't already know it's bytes, assume it will be an array
            let at = Type::Array(Arc::new(Type::empty_tvar()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        if let Some(start) = self.start.as_mut() {
            wrap!(start.node, start.node.typecheck(ctx))?;
            wrap!(start.node, it.check_contains(&ctx.env, &start.node.typ()))?;
        }
        if let Some(end) = self.end.as_mut() {
            wrap!(end.node, end.node.typecheck(ctx))?;
            wrap!(end.node, it.check_contains(&ctx.env, &end.node.typ()))?;
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

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::ArraySlice(self)
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if self.n.is_empty() && event.init {
            return Some(Value::Array(ValArray::from([])));
        }
        let (updated, determined) = update_args!(self.n, ctx, event);
        if updated && determined {
            let iter = self.n.iter().map(|n| n.cached.clone().unwrap());
            Some(Value::Array(ValArray::from_iter_exact(iter)))
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

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.node.refs(refs))
    }

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in &mut self.n {
            wrap!(n.node, n.node.typecheck(ctx))?
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

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Array(self)
    }
}
