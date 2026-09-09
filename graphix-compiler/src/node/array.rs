use super::{compiler::compile, dense_gate, gather, read_prod};
use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent, defetyp, err, errf,
    expr::{Expr, ExprId},
    fusion::emit::{
        BodyCx, CompiledExpr, emit_array_ref_node, emit_array_slice_node,
        emit_list_new_node, emit_tuple_new_node,
    },
    typ::Type,
    wrap,
};
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::{PBytes, Typ, ValArray, Value};
use poolshark::local::LPooled;
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "ArrayIndexError", "Error<`{}(string)>");

#[derive(Debug)]
pub struct ArrayRef<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: bool,
    pub source: Node<R, E>,
    pub i: Node<R, E>,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub etyp: Type,
    resident: TagValue,
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
            slept: false,
        }))
    }
}

/// The largest `n` `array::init(n, f)` will build; beyond it both
/// evaluators log and produce bottom.
pub const MAX_ARRAY_INIT_LEN: i64 = 16 * 1024 * 1024;

/// `array[i]`, shared by the node-walk and the JIT. Returns the bare
/// element, or the `ArrayIndexError` value when out of bounds. Negative
/// indices count from the end.
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

/// `bytes[i]`, with the same rules as [`array_index`]; returns
/// `Value::U8` or the out-of-bounds error.
pub(crate) fn bytes_index(b: &PBytes, i: i64) -> Value {
    let idx = if i >= 0 { i } else { b.len() as i64 + i };
    if idx >= 0 && (idx as usize) < b.len() {
        Value::U8(b[idx as usize])
    } else {
        err!(ERR_TAG, "index out of bounds")
    }
}

/// `a[i..j]` / `a[i..]` / `a[..j]` / `a[..]` for arrays and bytes, given
/// `usize` bounds. Returns the sub-array / sub-bytes or an error.
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

/// [`array_slice`] with `i64` bounds, for the JIT. A negative bound
/// wraps via `as usize`, matching the node-walk's `cast_to::<usize>()`,
/// so it surfaces the same out-of-bounds error.
pub(crate) fn array_slice_i64(
    src: &Value,
    start: Option<i64>,
    end: Option<i64>,
) -> Value {
    array_slice(src, start.map(|i| i as usize), end.map(|i| i as usize))
}

impl<R: Rt, E: UserEvent> Update<R, E> for ArrayRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let mut trig = false;
        let mut fired = false;
        let mut bottom = false;
        let sval = read_prod!(self.source, ctx, event, trig, fired, bottom);
        let ival = read_prod!(self.i, ctx, event, trig, fired, bottom);
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let i = match ival.unwrap() {
            Value::I64(i) => i,
            v => match v.cast_to::<i64>() {
                Ok(i) => i,
                Err(_) => {
                    return self.resident.set(TagValue::tagged(
                        err!(ERR_TAG, "expected an integer"),
                        tag,
                    ));
                }
            },
        };
        let v = match sval.unwrap() {
            Value::Array(elts) => array_index(&elts, i),
            Value::Bytes(b) => bytes_index(&b, i),
            _ => err!(ERR_TAG, "expected an array"),
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        wrap!(self.i, self.i.typecheck0(ctx))?;
        let int = Type::Primitive(Typ::integer());
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
        wrap!(self.i, int.check_contains(&ctx.env, self.i.typ()))
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
        self.slept = true;
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
    slept: bool,
    pub source: Node<R, E>,
    pub start: Option<Node<R, E>>,
    pub end: Option<Node<R, E>>,
    pub(crate) spec: Expr,
    pub typ: Type,
    resident: TagValue,
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
            slept: false,
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let mut trig = false;
        let mut fired = false;
        let mut bottom = false;
        let sval = read_prod!(self.source, ctx, event, trig, fired, bottom);
        let stval = match self.start.as_mut() {
            None => None,
            Some(n) => read_prod!(n, ctx, event, trig, fired, bottom),
        };
        let etval = match self.end.as_mut() {
            None => None,
            Some(n) => read_prod!(n, ctx, event, trig, fired, bottom),
        };
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        macro_rules! number {
            ($e:expr) => {
                match $e.clone().cast_to::<usize>() {
                    Ok(i) => i,
                    Err(_) => {
                        return self.resident.set(TagValue::tagged(
                            err!(ERR_TAG, "expected a non negative number"),
                            tag,
                        ));
                    }
                }
            };
        }
        macro_rules! bound {
            ($bound:expr) => {{
                match $bound {
                    Value::U64(i) | Value::V64(i) => Some(*i as usize),
                    v => Some(number!(v)),
                }
            }};
        }
        let start = match &stval {
            None => None,
            Some(v) => bound!(v),
        };
        let end = match &etval {
            None => None,
            Some(v) => bound!(v),
        };
        let v = array_slice(&sval.unwrap(), start, end);
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        let it = Type::Primitive(Typ::integer());
        let bytes_typ = Type::Primitive(Typ::Bytes.into());
        let source_typ = self.source.typ();
        if !bytes_typ.contains_with_flags(BitFlags::empty(), &ctx.env, source_typ)? {
            // if we don't already know it's bytes, assume it will be an array
            let at = Type::Array(Arc::new(Type::empty_tvar()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        if let Some(start) = self.start.as_mut() {
            wrap!(start, start.typecheck0(ctx))?;
            wrap!(start, it.check_contains(&ctx.env, &start.typ()))?;
        }
        if let Some(end) = self.end.as_mut() {
            wrap!(end, end.typecheck0(ctx))?;
            wrap!(end, it.check_contains(&ctx.env, &end.typ()))?;
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
        self.slept = true;
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

#[derive(Debug)]
pub struct Array<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: bool,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
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
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let typ = Type::Array(Arc::new(Type::empty_tvar()));
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom(), slept: false }))
    }
}

#[derive(Debug)]
pub struct ListLit<R: Rt, E: UserEvent> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: bool,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> ListLit<R, E> {
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
        let typ = Type::List(Arc::new(Type::empty_tvar()));
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom(), slept: false }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ListLit<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        use crate::node::collection::list;
        if self.n.is_empty() {
            // an empty producer is a constant (see Array)
            if ctx.frame_depth > 0 {
                return self.resident.set(if ctx.frame_init {
                    TagValue::fired(list::nil())
                } else {
                    TagValue::stale(list::nil())
                });
            } else if event.init {
                return self.resident.set(TagValue::fired(list::nil()));
            }
            return self.resident.ride();
        }
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        let (trig, fired, bottom) = gather(ctx, event, &mut self.n, &mut vals);
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let v = list::from_iter(vals.drain(..));
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
        self.slept = true;
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
        let rtype = wrap!(self, Type::union(&ctx.env, &ts))?;
        let rtype = match rtype {
            Type::Bottom => Type::List(Arc::new(Type::empty_tvar())),
            t => Type::List(Arc::new(t)),
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
        NodeView::ListLit(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_list_new_node(cx, &self.n)
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Array<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.n.is_empty() {
            // an empty producer is a constant: fired at init, stale
            // inside frames (see Constant)
            if ctx.frame_depth > 0 {
                return self.resident.set(if ctx.frame_init {
                    TagValue::fired(Value::Array(ValArray::from([])))
                } else {
                    TagValue::stale(Value::Array(ValArray::from([])))
                });
            } else if event.init {
                return self
                    .resident
                    .set(TagValue::fired(Value::Array(ValArray::from([]))));
            }
            return self.resident.ride();
        }
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        let (trig, fired, bottom) = gather(ctx, event, &mut self.n, &mut vals);
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let v = Value::Array(ValArray::from_iter_exact(vals.drain(..)));
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
        self.slept = true;
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
        let rtype = wrap!(self, Type::union(&ctx.env, &ts))?;
        let rtype = match rtype {
            Type::Bottom => Type::Array(Arc::new(Type::empty_tvar())),
            t => Type::Array(Arc::new(t)),
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
        NodeView::Array(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // the runtime shape is a tuple literal's
        emit_tuple_new_node(cx, &self.n)
    }
}
