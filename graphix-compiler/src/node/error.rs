use crate::{
    Apply, BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope,
    TagValue, Update, UserEvent,
    compiler::compile,
    deref_typ,
    env::Env,
    expr::{self, Expr, ExprId, ModPath},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_qop_node},
    typ::{Type, TypeRef},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, sync::LazyLock};
use triomphe::Arc;

pub(super) static ECHAIN: LazyLock<ModPath> =
    LazyLock::new(|| ModPath::from(["ErrChain"]));

fn typ_echain(param: Type) -> Type {
    Type::Ref(TypeRef::synthetic(
        ModPath::root(),
        ECHAIN.clone(),
        Arc::from_iter([param]),
    ))
}

pub(crate) fn wrap_error(env: &Env, spec: &Expr, e: Value) -> Value {
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

/// The error-delivery side of a handler-ful `?` as an `Apply`, so a fused
/// kernel can perform it through the DynCall machinery (see
/// [`crate::fusion::kernel_abi::FnSource::QopDeliver`]). `update` receives
/// the operator's value in `from[0]`; on an `Error` it replicates
/// `Qop::update`'s handler path EXACTLY — `wrap_error` with this `?`'s
/// position/origin, then write the catch handler's variable (vacant →
/// insert into `event.variables`, occupied → `set_var`). Returns
/// `Value::Null` (a completed side effect); the kernel's error branch
/// separately aborts the cycle to bottom, matching `Qop::update`'s `None`.
#[derive(Debug)]
pub(crate) struct QopDeliverApply {
    pub(crate) handler_id: BindId,
    pub(crate) spec: Expr,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for QopDeliverApply {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let v = from.get_mut(0)?.update(ctx, event)?.value();
        if let Value::Error(e) = v {
            let e = wrap_error(&ctx.env, &self.spec, (*e).clone());
            let v = Value::Error(Arc::new(e));
            match event.variables.entry(self.handler_id) {
                Entry::Vacant(slot) => {
                    slot.insert(TagValue::fired(v));
                }
                Entry::Occupied(_) => ctx.rt.set_var(self.handler_id, v),
            }
        }
        Some(Value::Null)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
pub struct TryCatch<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub nodes: LPooled<Vec<Node<R, E>>>,
    pub handler: Node<R, E>,
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
                tv.typ.write().typ = Some(Type::Bottom)
            }
            _ => unreachable!(),
        }
        let id = ctx
            .env
            .bind_variable(
                &catch_scope.lexical,
                &tc.bind,
                typ,
                spec.pos,
                spec.ori.clone(),
            )
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
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in self.nodes.iter_mut() {
            n.reset_replay(ctx)
        }
        self.handler.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.nodes.iter_mut() {
            wrap!(n, n.typecheck0(ctx))?
        }
        wrap!(self.handler, self.handler.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.nodes.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        wrap!(self.handler, self.handler.typecheck1(ctx))?;
        Ok(())
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

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TryCatch(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // The TryCatch itself is a fusion boundary (no `emit_clif`): the
        // catch handler reads the error variable a handler-ful `?` writes,
        // and that read is necessarily a separate kernel (next cycle). But
        // its CHILDREN fuse — each try-body statement and the catch handler
        // fuse their own maximal subtrees. Without this recursion the whole
        // try body node-walks, so wrapping a hot loop in `try`/`catch`
        // would kill its fusion (the `?` inside fuses via the QopDeliver
        // site once the body is reached).
        for n in self.nodes.iter_mut() {
            crate::fusion::fuse(n, ctx)?;
        }
        crate::fusion::fuse(&mut self.handler, ctx)?;
        Ok(None)
    }
}

#[derive(Debug)]
pub struct Qop<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub id: Option<BindId>,
    pub n: Node<R, E>,
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
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let tv = self.n.update(ctx, event)?;
        if tv.is_tainted() {
            // a taint placeholder is not an error VALUE — pass it on
            return Some(tv);
        }
        let (v, tag) = tv.into_parts();
        match v {
            Value::Error(e) => match self.id {
                Some(id) => {
                    let e = wrap_error(&ctx.env, &self.spec, (*e).clone());
                    let v = Value::Error(Arc::new(e));
                    match event.variables.entry(id) {
                        Entry::Vacant(slot) => {
                            slot.insert(TagValue::fired(v));
                        }
                        Entry::Occupied(_) => ctx.rt.set_var(id, v),
                    }
                    None
                }
                None => {
                    if ctx.frame_depth > 0 {
                        // in-frame swallowed error: the taint channel,
                        // silent (the log is a reactive debugging aid)
                        return Some(TagValue::tainted(Value::Null));
                    }
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
            v => Some(TagValue::tagged(v, tag)),
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
                    Some(Type::Ref (TypeRef { scope, name, .. }))
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
        wrap!(self.n, self.n.typecheck0(ctx))?;
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
                    let mut cell = tv.typ.write();
                    cell.typ = match &cell.typ {
                        None => Some(etyp.clone()),
                        Some(t) => Some(t.union(&ctx.env, &etyp)?),
                    };
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Qop(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // A handler-ful `?` (`id: Some` — caught by an enclosing `try`)
        // delivers its error by WRITING the handler's variable. The fused
        // kernel now performs that write in-kernel via the discovered
        // `QopDeliver` site (scalar success type only — non-scalar
        // handler-ful `?` de-fuses inside `emit_qop_node`). The catch
        // handler that READS the variable is always a separate kernel
        // (next cycle), so there's no read-after-write hazard. A
        // handler-LESS `?` (`id: None`) passes `None` — its error path is
        // the bottom the kernel produces with no delivery.
        emit_qop_node(cx, &self.n, &self.typ, self.id.map(|_| self.spec.id))
    }
}

#[derive(Debug)]
pub struct OrNever<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Node<R, E>,
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
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let tv = self.n.update(ctx, event)?;
        if tv.is_tainted() {
            return Some(tv);
        }
        let (v, tag) = tv.into_parts();
        match v {
            Value::Error(e) => {
                if ctx.frame_depth > 0 {
                    // in-frame swallowed error: silent taint
                    return Some(TagValue::tainted(Value::Null));
                }
                log::warn!("ignored error in {} at {} {e}", self.spec.ori, self.spec.pos);
                None
            }
            v => Some(TagValue::tagged(v, tag)),
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
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

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::OrNever(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // `$` never has a catch handler (log + drop on error) — no delivery.
        emit_qop_node(cx, &self.n, &self.typ, None)
    }
}
