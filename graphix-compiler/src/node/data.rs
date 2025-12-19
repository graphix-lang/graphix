use super::{compiler::compile, Cached};
use crate::{
    deref_typ,
    expr::{Expr, ExprId, ExprKind, StructWithExpr},
    typ::Type,
    update_args, wrap, CFlag, Event, ExecCtx, Node, PrintFlag, Refs, Rt, Scope, Update,
    UserEvent,
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::{ValArray, Value};
use smallvec::SmallVec;
use std::iter;
use triomphe::Arc;

#[derive(Debug)]
pub(crate) struct Struct<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    names: Box<[ArcStr]>,
    n: Box<[Cached<R, E>]>,
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
            .map(|(_, e)| Ok(Cached::new(compile(ctx, flags, e.clone(), scope, top_id)?)))
            .collect::<Result<Box<[_]>>>()?;
        let typs =
            names.iter().zip(n.iter()).map(|(n, a)| (n.clone(), a.node.typ().clone()));
        let typ = Type::Struct(Arc::from_iter(typs));
        Ok(Box::new(Self { spec, typ, names, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Struct<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if self.n.is_empty() && event.init {
            return Some(Value::Array(ValArray::from([])));
        }
        let (updated, determined) = update_args!(self.n, ctx, event);
        if updated && determined {
            let iter = self.names.iter().zip(self.n.iter()).map(|(name, n)| {
                let name = Value::String(name.clone());
                let v = n.cached.clone().unwrap();
                Value::Array(ValArray::from_iter_exact([name, v].into_iter()))
            });
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

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n.node, n.node.typecheck(ctx))?
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
                    t.check_contains(&ctx.env, &n.node.typ())?
                }
            }
            _ => bail!("BUG: expected a struct rtype"),
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Replace<R: Rt, E: UserEvent> {
    index: Option<usize>,
    name: Value,
    n: Cached<R, E>,
}

#[derive(Debug)]
pub(crate) struct StructWith<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    source: Node<R, E>,
    current: Option<ValArray>,
    replace: Box<[Replace<R, E>]>,
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
                    n: Cached::new(compile(ctx, flags, e.clone(), scope, top_id)?),
                })
            })
            .collect::<Result<Box<[_]>>>()?;
        let typ = source.typ().clone();
        Ok(Box::new(Self { spec, typ, source, current: None, replace }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StructWith<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let mut updated = self
            .source
            .update(ctx, event)
            .map(|v| match v {
                Value::Array(a) => {
                    self.current = Some(a);
                    true
                }
                _ => false,
            })
            .unwrap_or(false);
        let mut determined = self.current.is_some();
        for r in self.replace.iter_mut() {
            updated |= r.n.update(ctx, event);
            determined &= r.n.cached.is_some();
        }
        if updated && determined {
            let mut si = 0;
            let iter =
                self.current.as_ref().unwrap().iter().enumerate().map(|(i, v)| match v {
                    Value::Array(v) if v.len() == 2 => {
                        if let Some(r) = self.replace.get_mut(si) {
                            match r.index {
                                Some(index) if i == index => {
                                    si += 1;
                                    let rep = r.n.cached.clone().unwrap();
                                    Value::Array(ValArray::from_iter_exact(
                                        [v[0].clone(), rep].into_iter(),
                                    ))
                                }
                                None if &r.name == &v[0] => {
                                    si += 1;
                                    r.index = Some(i);
                                    let rep = r.n.cached.clone().unwrap();
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
        self.source.delete(ctx);
        self.replace.iter_mut().for_each(|r| r.n.node.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.source.sleep(ctx);
        self.replace.iter_mut().for_each(|r| r.n.sleep(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        self.replace.iter().for_each(|r| r.n.node.refs(refs))
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck(ctx))?;
        let fields = match &self.spec.kind {
            ExprKind::StructWith(StructWithExpr { source: _, replace }) => {
                replace.iter().map(|(n, _)| n.clone()).collect::<SmallVec<[ArcStr; 8]>>()
            }
            _ => bail!("BUG: miscompiled structwith"),
        };
        wrap!(
            self,
            self.source.typ().with_deref(|typ| match typ {
                Some(Type::Struct(flds)) => {
                    for (rep, n) in self.replace.iter_mut().zip(fields.iter()) {
                        let r = flds.iter().enumerate().find_map(|(i, (field, typ))| {
                            if field == n {
                                Some((i, typ))
                            } else {
                                None
                            }
                        });
                        match r {
                            None => bail!("struct has no field named {n}"),
                            Some((i, typ)) => {
                                wrap!(rep.n.node, rep.n.node.typecheck(ctx))?;
                                wrap!(
                                    rep.n.node,
                                    typ.check_contains(&ctx.env, &rep.n.node.typ())
                                )?;
                                rep.index = Some(i);
                            }
                        }
                    }
                    Ok(())
                }
                None => bail!("type must be known, annotations needed"),
                _ => bail!("expected a struct"),
            })
        )?;
        wrap!(self, self.typ.check_contains(&ctx.env, self.source.typ()))
    }
}

#[derive(Debug)]
pub(crate) struct StructRef<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    source: Node<R, E>,
    field: Option<usize>,
    field_name: ArcStr,
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
                        if field_name == n {
                            Some((t.clone(), Some(i)))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| (Type::empty_tvar(), None))
            }
            _ => (Type::empty_tvar(), None),
        };
        let field_name = field_name.clone();
        Ok(Box::new(Self { spec, typ, source, field, field_name }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StructRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        match self.source.update(ctx, event) {
            Some(Value::Array(a)) => match self.field {
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
            Some(_) | None => None,
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

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck(ctx))?;
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
}

#[derive(Debug)]
pub(crate) struct Tuple<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    n: Box<[Cached<R, E>]>,
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
            .map(|e| Ok(Cached::new(compile(ctx, flags, e.clone(), scope, top_id)?)))
            .collect::<Result<Box<[_]>>>()?;
        let typ = Type::Tuple(Arc::from_iter(n.iter().map(|n| n.node.typ().clone())));
        Ok(Box::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Tuple<R, E> {
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

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n.node, n.node.typecheck(ctx))?
        }
        match &self.typ {
            Type::Tuple(typs) => {
                if self.n.len() != typs.len() {
                    bail!("tuple arity mismatch {} vs {}", self.n.len(), typs.len())
                }
                for (t, n) in typs.iter().zip(self.n.iter()) {
                    t.check_contains(&ctx.env, &n.node.typ())?
                }
            }
            _ => bail!("BUG: unexpected tuple rtype"),
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Variant<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    tag: ArcStr,
    n: Box<[Cached<R, E>]>,
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
            .map(|e| Ok(Cached::new(compile(ctx, flags, e.clone(), scope, top_id)?)))
            .collect::<Result<Box<[_]>>>()?;
        let typs = Arc::from_iter(n.iter().map(|n| n.node.typ().clone()));
        let typ = Type::Variant(tag.clone(), typs);
        let tag = ctx.tag(tag);
        Ok(Box::new(Self { spec, typ, tag, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Variant<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        if self.n.len() == 0 {
            if event.init {
                Some(Value::String(self.tag.clone()))
            } else {
                None
            }
        } else {
            let (updated, determined) = update_args!(self.n, ctx, event);
            if updated && determined {
                let a = iter::once(Value::String(self.tag.clone()))
                    .chain(self.n.iter().map(|n| n.cached.clone().unwrap()));
                Some(Value::Array(ValArray::from_iter(a)))
            } else {
                None
            }
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

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n.node, n.node.typecheck(ctx))?
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
                    wrap!(n.node, t.check_contains(&ctx.env, &n.node.typ()))?
                }
            }
            _ => bail!("BUG: unexpected variant rtype"),
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct TupleRef<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    source: Node<R, E>,
    field: usize,
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
            _ => Type::empty_tvar(),
        };
        Ok(Box::new(Self { spec, typ, source, field }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TupleRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        self.source.update(ctx, event).and_then(|v| match v {
            Value::Array(a) => a.get(self.field).map(|v| v.clone()),
            Value::Error(v) => Some((*v).clone()),
            _ => None,
        })
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

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck(ctx))?;
        let etyp = deref_typ!("tuple", ctx, self.source.typ(),
            Some(Type::Tuple(flds)) => Ok(flds[self.field].clone()),
            Some(Type::Error(t)) => {
                if self.field != 0 {
                    bail!("no such field {}", self.field);
                }
                Ok((**t).clone())
            }
        );
        let etyp = wrap!(self, etyp)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &etyp))
    }
}
