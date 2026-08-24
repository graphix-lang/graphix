use super::{compiler::compile, dense_gate, gather};
use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag, TagValue,
    Update, UserEvent, abstract_value, deref_typ,
    expr::{Expr, ExprId, ExprKind, ModPath, StructWithExpr},
    fusion::emit::{
        BodyCx, CompiledExpr, emit_abstract_ref_node, emit_construct_node,
        emit_struct_new_node, emit_struct_ref_node, emit_struct_with_node,
        emit_tuple_new_node, emit_tuple_ref_node, emit_variant_new_node,
    },
    typ::{AbstractId, Type},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::iter;
use triomphe::Arc;

#[derive(Debug)]
pub struct Struct<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub names: Box<[ArcStr]>,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Struct<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
        let typs = names.iter().zip(n.iter()).map(|(n, a)| (n.clone(), a.typ().clone()));
        let typ = Type::Struct(Arc::from_iter(typs));
        Ok(Node::new(Self { spec, typ, names, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Struct<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.n.is_empty() {
            // Empty producer = a constant: FIRED at init, the STALE
            // value channel inside frames (the Constant frame rule —
            // a per-site instance's `let res = []` seed died after
            // frame resets and its For bottomed on the missing init,
            // firing-jul2026/03).
            // Frame depth first — frames force init (see Constant).
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
        let iter = self.names.iter().zip(vals.drain(..)).map(|(name, v)| {
            let name = Value::String(name.clone());
            Value::Array(ValArray::from_iter_exact([name, v].into_iter()))
        });
        let v = Value::Array(ValArray::from_iter_exact(iter));
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
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck0(ctx))?
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
                for ((_, t), n) in typs.iter().zip(self.n.iter()) {
                    t.check_contains(&ctx.env, &n.typ())?
                }
            }
            _ => bail!("BUG: expected a struct rtype"),
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Struct(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_struct_new_node(cx, &self.names, &self.n)
    }
}

#[derive(Debug)]
pub struct Replace<R: Rt, E: UserEvent> {
    pub(crate) index: Option<usize>,
    pub name: Value,
    pub n: Node<R, E>,
}

#[derive(Debug)]
pub struct StructWith<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub source: Node<R, E>,
    pub replace: Box<[Replace<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> StructWith<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
                    name: Value::String(name.clone()),
                    n: compile(ctx, flags, e.clone(), scope, top_id)?,
                })
            })
            .collect::<Result<Box<[_]>>>()?;
        let typ = source.typ().clone();
        Ok(Node::new(Self { spec, typ, source, replace, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StructWith<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let mut trig = false;
        let mut fired = false;
        let mut bottom = false;
        let src = {
            let tv = self.source.update(ctx, event);
            let t = tv.tag();
            trig |= t.triggers();
            fired |= t.is_fired();
            if t.is_bottom() {
                bottom = true;
                None
            } else {
                match tv.value_cloned() {
                    Value::Array(a) => Some(a),
                    // an unshaped (non-struct-rep) source is bottom
                    _ => {
                        bottom = true;
                        None
                    }
                }
            }
        };
        let mut rvals: SmallVec<[Value; 8]> = SmallVec::new();
        for r in self.replace.iter_mut() {
            let tv = r.n.update(ctx, event);
            let t = tv.tag();
            trig |= t.triggers();
            fired |= t.is_fired();
            if t.is_bottom() {
                bottom = true
            } else if !bottom {
                rvals.push(tv.value_cloned())
            }
        }
        dense_gate!(self, ctx, trig, bottom);
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let src = src.unwrap();
        let mut si = 0;
        let iter = src.iter().enumerate().map(|(i, v)| match v {
            Value::Array(v) if v.len() == 2 => {
                if let Some(r) = self.replace.get_mut(si) {
                    match r.index {
                        Some(index) if i == index => {
                            let rep = rvals[si].clone();
                            si += 1;
                            Value::Array(ValArray::from_iter_exact(
                                [v[0].clone(), rep].into_iter(),
                            ))
                        }
                        None if &r.name == &v[0] => {
                            r.index = Some(i);
                            let rep = rvals[si].clone();
                            si += 1;
                            Value::Array(ValArray::from_iter_exact(
                                [v[0].clone(), rep].into_iter(),
                            ))
                        }
                        _ => Value::Array(v.clone()),
                    }
                } else {
                    Value::Array(v.clone())
                }
            }
            _ => v.clone(),
        });
        let v = Value::Array(ValArray::from_iter_exact(iter));
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx);
        self.replace.iter_mut().for_each(|r| r.n.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.sleep(ctx);
        self.replace.iter_mut().for_each(|r| r.n.sleep(ctx))
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.reset_replay(ctx);
        self.replace.iter_mut().for_each(|r| r.n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.replace.iter().for_each(|r| r.n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        let fields = match &self.spec.kind {
            ExprKind::StructWith(StructWithExpr { source: _, replace }) => {
                replace.iter().map(|(n, _)| n.clone()).collect::<SmallVec<[ArcStr; 8]>>()
            }
            _ => bail!("BUG: miscompiled structwith"),
        };
        // clone the deref'd type out BEFORE recursing — the typecheck0 and
        // unification calls below take TVar write locks, and with_deref
        // holds read guards on the source type's whole deref chain for the
        // closure's duration (a same-thread deadlock, not just a race)
        let styp = self.source.typ().with_deref(|typ| typ.cloned());
        let check = || -> Result<()> {
            match styp {
                Some(Type::Struct(flds)) => {
                    for (rep, n) in self.replace.iter_mut().zip(fields.iter()) {
                        let r = flds.iter().enumerate().find_map(|(i, (field, typ))| {
                            if field == n { Some((i, typ)) } else { None }
                        });
                        match r {
                            None => bail!("struct has no field named {n}"),
                            Some((i, typ)) => {
                                wrap!(rep.n, rep.n.typecheck0(ctx))?;
                                wrap!(rep.n, typ.check_contains(&ctx.env, &rep.n.typ()))?;
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
        wrap!(self, check())?;
        wrap!(self, self.typ.check_contains(&ctx.env, self.source.typ()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
    pub field: Option<usize>,
    pub field_name: ArcStr,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> StructRef<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        field_name: &ArcStr,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let (typ, field) = match &source.typ() {
            Type::Struct(flds) => {
                flds.iter()
                    .enumerate()
                    .find_map(|(i, (n, t))| {
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
            field,
            field_name,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StructRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.source.update(ctx, event);
        let tag = tv.tag();
        if tag.is_bottom() {
            return if tag.triggers() {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            };
        }
        let v = tv.value_cloned();
        let res = match v {
            Value::Array(a) => match self.field {
                Some(i) => a.get(i).and_then(|v| match v {
                    Value::Array(a) if a.len() == 2 => Some(a[1].clone()),
                    _ => None,
                }),
                None => {
                    let res = a.iter().enumerate().find_map(|(i, kv)| match kv {
                        Value::Array(kv) => match &kv[..] {
                            [Value::String(f), v] if f == &self.field_name => {
                                Some((i, v.clone()))
                            }
                            _ => None,
                        },
                        _ => None,
                    });
                    match res {
                        Some((i, v)) => {
                            self.field = Some(i);
                            Some(v)
                        }
                        None => None,
                    }
                }
            },
            _ => None,
        };
        match res {
            Some(v) => self.resident.set(TagValue::tagged(v, tag)),
            None => self.resident.ride(),
        }
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.sleep(ctx)
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.reset_replay(ctx)
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        let etyp = deref_typ!("struct", ctx, self.source.typ(),
            Some(Type::Struct(flds)) => {
                let typ = flds.iter().enumerate().find_map(|(i, (n, t))| {
                    if &self.field_name == n {
                        Some((i, t.clone()))
                    } else {
                        None
                    }
                });
                match typ {
                    Some((i, t)) => Ok((i, t)),
                    None => bail!("in struct, unknown field {}", self.field_name),
                }
        });
        let (idx, typ) = wrap!(self, etyp)?;
        self.field = Some(idx);
        wrap!(self, self.typ.check_contains(&ctx.env, &typ))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::StructRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // `field` is the position in the struct type's canonical
        // (sorted) layout, resolved by typecheck; unresolved → the
        // subtree node-walks.
        let sorted_idx = self
            .field
            .ok_or_else(|| anyhow::anyhow!("emit_clif: struct field index unresolved"))?;
        emit_struct_ref_node(cx, &self.source, sorted_idx, &self.typ)
    }
}

#[derive(Debug)]
pub struct Tuple<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Tuple<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Tuple<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.n.is_empty() {
            // Empty producer = a constant: FIRED at init, the STALE
            // value channel inside frames (the Constant frame rule —
            // a per-site instance's `let res = []` seed died after
            // frame resets and its For bottomed on the missing init,
            // firing-jul2026/03).
            // Frame depth first — frames force init (see Constant).
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
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck0(ctx))?
        }
        match &self.typ {
            Type::Tuple(typs) => {
                if self.n.len() != typs.len() {
                    bail!("tuple arity mismatch {} vs {}", self.n.len(), typs.len())
                }
                for (t, n) in typs.iter().zip(self.n.iter()) {
                    t.check_contains(&ctx.env, &n.typ())?
                }
            }
            _ => bail!("BUG: unexpected tuple rtype"),
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Tuple(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_tuple_new_node(cx, &self.n)
    }
}

#[derive(Debug)]
pub struct Variant<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub tag: ArcStr,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Variant<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
        let typ = Type::Variant(tag.clone(), typs);
        let tag = ctx.tag(tag);
        Ok(Node::new(Self { spec, typ, tag, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Variant<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.n.len() == 0 {
            if event.init {
                self.resident.set(TagValue::fired(Value::String(self.tag.clone())))
            } else {
                self.resident.ride()
            }
        } else {
            let mut vals: LPooled<Vec<Value>> = LPooled::take();
            let (trig, fired, bottom) = gather(ctx, event, &mut self.n, &mut vals);
            dense_gate!(self, ctx, trig, bottom);
            let tag = if fired { Tag::FIRED } else { Tag::STALE };
            let a = iter::once(Value::String(self.tag.clone())).chain(vals.drain(..));
            let v = Value::Array(ValArray::from_iter(a));
            self.resident.set(TagValue::tagged(v, tag))
        }
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
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck0(ctx))?
        }
        match &self.typ {
            Type::Variant(ttag, typs) => {
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

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Variant(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_variant_new_node(cx, &self.tag, &self.n)
    }
}

/// `T(v)`: the constructor of a Graphix-minted abstract type
/// (`design/nominal_abstract_types.md`) — boxes its argument with the
/// type's tag. Compiles only where the definition is visible.
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
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Construct<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
            .lookup_typedef(&scope.lexical, name)?
            .ok_or_else(|| anyhow!("unknown type {name}"))?;
        let Type::Abstract { id, .. } = &td.typ else {
            bail!("{name} is not an abstract type, so it has no constructor")
        };
        let id = *id;
        let Some(r) = ctx.env.abstract_rep(id, &scope.lexical) else {
            bail!(
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
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Construct<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.arg.update(ctx, event);
        let tag = tv.tag();
        if tag.is_bottom() {
            return if tag.triggers() {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            };
        }
        let v = abstract_value::wrap(self.id, self.name.clone(), tv.value_cloned());
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.arg.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.arg.sleep(ctx)
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.arg.reset_replay(ctx)
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.arg, self.arg.typecheck0(ctx))?;
        wrap!(self.arg, self.rep.check_contains(&ctx.env, &self.arg.typ()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.arg, self.arg.typecheck1(ctx))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Construct(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_construct_node(cx, self.id, &self.name, &self.arg)
    }
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
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.source.update(ctx, event);
        let tag = tv.tag();
        if tag.is_bottom() {
            return if tag.triggers() {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            };
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        let etyp = deref_typ!("tuple", ctx, self.source.typ(),
            Some(Type::Tuple(flds)) => flds
                .get(self.field)
                .map(|t| t.clone())
                .ok_or_else(|| anyhow!("in tuple, no such field {}", self.field)),
            Some(Type::Error(t)) => {
                if self.field != 0 {
                    bail!("no such field {}", self.field);
                }
                Ok((**t).clone())
            },
            Some(Type::Abstract { id, params }) => {
                if self.field != 0 {
                    bail!("no such field {}: an abstract type has only its payload .0", self.field);
                }
                match ctx.env.abstract_rep(*id, &self.scope) {
                    Some(r) => Ok(r.instantiate_with(params)),
                    None => bail!(
                        "the definition of this abstract type is not visible here, so \
                         its payload cannot be read"
                    ),
                }
            }
        );
        let etyp = wrap!(self, etyp)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &etyp))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TupleRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        let abstract_source =
            self.source.typ().with_deref(|t| matches!(t, Some(Type::Abstract { .. })));
        if abstract_source {
            emit_abstract_ref_node(cx, &self.source, &self.typ)
        } else {
            emit_tuple_ref_node(cx, &self.source, self.field, &self.typ)
        }
    }
}
