use crate::{
    compiler::compile,
    deref_typ,
    env::Env,
    expr::{self, Expr, ExprId, ModPath},
    format_with_flags,
    typ::Type,
    wrap, BindId, CFlag, Event, ExecCtx, Node, PrintFlag, Refs, Rt, Scope, Update,
    UserEvent,
};
use anyhow::{anyhow, bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, sync::LazyLock};
use triomphe::Arc;

pub(super) static ECHAIN: LazyLock<ModPath> =
    LazyLock::new(|| ModPath::from(["ErrChain"]));

fn typ_echain(param: Type) -> Type {
    Type::Ref {
        scope: ModPath::root(),
        name: ECHAIN.clone(),
        params: Arc::from_iter([param]),
    }
}

pub(super) fn wrap_error(env: &Env, spec: &Expr, e: Value) -> Value {
    static ERRCHAIN: LazyLock<Type> = LazyLock::new(|| typ_echain(Type::empty_tvar()));
    let pos: Value =
        [(literal!("column"), spec.pos.column), (literal!("line"), spec.pos.line)].into();
    if ERRCHAIN.is_a(env, &e) {
        let error = e.clone().cast_to::<[(ArcStr, Value); 4]>().unwrap();
        let error = error[1].1.clone();
        [
            (literal!("cause"), e.clone()),
            (literal!("error"), error),
            (literal!("ori"), spec.ori.to_value()),
            (literal!("pos"), pos),
        ]
        .into()
    } else {
        [
            (literal!("cause"), Value::Null),
            (literal!("error"), e.clone()),
            (literal!("ori"), spec.ori.to_value()),
            (literal!("pos"), pos),
        ]
        .into()
    }
}

#[derive(Debug)]
pub(crate) struct TryCatch<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    nodes: LPooled<Vec<Node<R, E>>>,
    handler: Node<R, E>,
}

impl<R: Rt, E: UserEvent> TryCatch<R, E> {
    pub(crate) fn new(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        tc: &Arc<expr::TryCatchExpr>,
    ) -> Result<Node<R, E>> {
        let inner_name = format_compact!("tc{}", BindId::new().inner());
        let inner_scope = scope.append(inner_name.as_str());
        let catch_name = format_compact!("ca{}", BindId::new().inner());
        let catch_scope = scope.append(catch_name.as_str());
        let typ = Type::empty_tvar();
        match &typ {
            Type::TVar(tv) => {
                let mut tv = tv.write();
                tv.frozen = true;
                *tv.typ.write() = Some(Type::Bottom)
            }
            _ => unreachable!(),
        }
        let id = ctx
            .env
            .bind_variable(&catch_scope.lexical, &tc.bind, typ, spec.pos, spec.ori.clone())
            .id;
        let handler = compile(ctx, flags, (*tc.handler).clone(), &catch_scope, top_id)?;
        ctx.env.catch.insert_cow(inner_scope.dynamic.clone(), id);
        let nodes = tc
            .exprs
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), &inner_scope, top_id))
            .collect::<Result<LPooled<Vec<_>>>>()?;
        let typ =
            nodes.last().ok_or_else(|| anyhow!("empty try catch block"))?.typ().clone();
        Ok(Box::new(Self { spec, typ, nodes, handler }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TryCatch<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let res = self.nodes.iter_mut().fold(None, |_, n| n.update(ctx, event));
        let _ = self.handler.update(ctx, event);
        res
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in self.nodes.iter_mut() {
            n.delete(ctx);
        }
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in self.nodes.iter_mut() {
            n.sleep(ctx)
        }
        self.handler.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.nodes.iter_mut() {
            wrap!(n, n.typecheck(ctx))?
        }
        wrap!(self.handler, self.handler.typecheck(ctx))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        for n in self.nodes.iter() {
            n.refs(refs);
        }
        self.handler.refs(refs);
    }
}

#[derive(Debug)]
pub(crate) struct Qop<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    id: Option<BindId>,
    n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Qop<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let id = match ctx.env.lookup_catch(&scope.dynamic).ok() {
            None => {
                if flags.contains(CFlag::WarnUnhandled | CFlag::WarningsAreErrors) {
                    bail!(
                        "ERROR: {} at {} error raised by ? will not be caught",
                        spec.ori,
                        spec.pos
                    )
                }
                if flags.contains(CFlag::WarnUnhandled) {
                    eprintln!(
                        "WARNING: {} at {} error raised by ? will not be caught",
                        spec.ori, spec.pos
                    );
                }
                None
            }
            o => o,
        };
        let typ = Type::empty_tvar();
        Ok(Box::new(Self { spec, typ, id, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Qop<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        match self.n.update(ctx, event) {
            None => None,
            Some(Value::Error(e)) => match self.id {
                Some(id) => {
                    let e = wrap_error(&ctx.env, &self.spec, (*e).clone());
                    let v = Value::Error(Arc::new(e));
                    match event.variables.entry(id) {
                        Entry::Vacant(e) => {
                            e.insert(v);
                        }
                        Entry::Occupied(_) => ctx.set_var(id, v),
                    }
                    None
                }
                None => {
                    log::error!(
                        "unhandled error in {} at {} {e}",
                        self.spec.ori,
                        self.spec.pos
                    );
                    eprintln!(
                        "unhandled error in {} at {} {e}",
                        self.spec.ori, self.spec.pos
                    );
                    None
                }
            },
            Some(v) => Some(v),
        }
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        fn fix_echain_typ<R: Rt, E: UserEvent>(
            ctx: &ExecCtx<R, E>,
            etyp: &Type,
        ) -> Result<Type> {
            deref_typ!("error", ctx, etyp,
                Some(Type::Primitive(p)) => {
                    if !p.contains(Typ::Error) {
                        bail!("expected error not {}", Type::Primitive(*p))
                    }
                    if *p == BitFlags::from(Typ::Error) {
                        Ok(Type::Error(Arc::new(typ_echain(Type::Any))))
                    } else {
                        let mut p = *p;
                        p.remove(Typ::Error);
                        Ok(Type::Set(Arc::from_iter([
                            Type::Error(Arc::new(typ_echain(Type::Any))),
                            Type::Primitive(p)
                        ])))
                    }
                },
                Some(Type::Error(et)) => et.with_deref(|et| match et {
                    None => bail!("type must be known"),
                    Some(Type::Ref { scope, name, .. })
                        if scope == &ModPath::root() && name == &*ECHAIN =>
                    {
                        Ok(etyp.clone())
                    }
                    Some(et) => {
                        Ok(Type::Error(Arc::new(typ_echain(et.clone()))))
                    }
                }),
                Some(Type::Set(elts)) => {
                    let mut res = elts
                        .iter()
                        .map(|et| fix_echain_typ(ctx, et))
                        .collect::<Result<LPooled<Vec<Type>>>>()?;
                    Ok(Type::Set(Arc::from_iter(res.drain(..))))
                }
            )
        }
        wrap!(self.n, self.n.typecheck(ctx))?;
        let err = Type::Error(Arc::new(Type::empty_tvar()));
        if !self.n.typ().contains_with_flags(BitFlags::empty(), &ctx.env, &err)? {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!("cannot use the ? operator on non error type {}", self.n.typ())
            })?
        }
        let err = Type::Primitive(Typ::Error.into());
        let rtyp = self.n.typ().diff(&ctx.env, &err)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        if let Some(id) = self.id {
            let etyp = self.n.typ().diff(&ctx.env, &rtyp)?;
            let etyp = wrap!(self, fix_echain_typ(&ctx, &etyp))?;
            let bind = ctx.env.by_id.get(&id).ok_or_else(|| anyhow!("BUG: catch"))?;
            match &bind.typ {
                Type::TVar(tv) => {
                    let tv = tv.read();
                    let mut typ = tv.typ.write();
                    match &mut *typ {
                        None => *typ = Some(etyp.clone()),
                        Some(t) => *typ = Some(t.union(&ctx.env, &etyp)?),
                    }
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct OrNever<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> OrNever<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let typ = Type::empty_tvar();
        Ok(Box::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for OrNever<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        match self.n.update(ctx, event) {
            None => None,
            Some(Value::Error(e)) => {
                log::warn!("ignored error in {} at {} {e}", self.spec.ori, self.spec.pos);
                None
            }
            Some(v) => Some(v),
        }
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck(ctx))?;
        let err = Type::Error(Arc::new(Type::empty_tvar()));
        if !self.n.typ().contains_with_flags(BitFlags::empty(), &ctx.env, &err)? {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!("cannot use the $ operator on non error type {}", self.n.typ())
            })?
        }
        let err = Type::Primitive(Typ::Error.into());
        let rtyp = self.n.typ().diff(&ctx.env, &err)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        Ok(())
    }
}
