use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt, Scope, Tag,
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

#[derive(Debug)]
pub struct Catch<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub handler: Node<R, E>,
    constraint: Option<Type>,
    /// Throws unioned into the bind before `catch(e: T)` ascribes `T`
    /// onto it. Coverage is this snapshot, not the bind after ascription.
    thrown: Option<Type>,
    bind_id: BindId,
    top_id: ExprId,
    typ: Type,
}

impl<R: Rt, E: UserEvent> Catch<R, E> {
    /// Compile a `catch(e) expr` statement. Only reachable from the
    /// statement-position dispatch in [`super::compile_block_children`]
    /// (blocks/module bodies) and the toplevel entry — the generic
    /// `compiler.rs` arm rejects other positions. Returns the node and
    /// the ADVANCED scope: subsequent siblings compile with the
    /// dynamic path extended by this catch's `c<id>` segment, so all
    /// three lookup clocks (Qop compile, CallSite typecheck, lambda
    /// instance bind — including LATE runtime binds) resolve coverage
    /// by path depth exactly like nested `try` blocks did. The lexical
    /// path is NOT extended: exports/typedefs/mods after a catch stay
    /// visible to the outside world (external resolution walks up,
    /// never down). Segment names derive from the spec's ExprId (the
    /// `do{id}` precedent), so a transient re-bind of a callee with an
    /// interior catch overwrites its registration instead of leaking a
    /// fresh key per rebind.
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        c: &Arc<expr::CatchExpr>,
    ) -> Result<(Node<R, E>, Scope)> {
        let catch_scope = scope.append_block("ca", spec.id.inner());
        let typ = Type::empty_tvar();
        match &typ {
            Type::TVar(tv) => {
                let mut tv = tv.write();
                tv.frozen = true;
                tv.typ.write().typ = Some(Type::Bottom)
            }
            _ => unreachable!(),
        }
        let bind_id = ctx
            .env
            .bind_variable(&catch_scope.lexical, &c.bind, typ, spec.pos, spec.ori.clone())
            .id;
        // The handler compiles BEFORE this catch registers, so a
        // rethrowing `?` inside it resolves to a predecessor catch in
        // the same block or an outer one — never to itself.
        let handler = compile(ctx, flags, (*c.handler).clone(), &catch_scope, top_id)?;
        let covered = scope.with_catch((bind_id, top_id));
        ctx.rt.ref_var(bind_id, top_id);
        let node = Node::new(Self {
            spec,
            handler,
            constraint: c.constraint.clone(),
            thrown: None,
            bind_id,
            top_id,
            typ: Type::Bottom,
        });
        Ok((node, covered))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Catch<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // The handler is a live reactive expression; its value channel
        // is discarded — a catch installation never produces. The
        // owning block updates catches AFTER its other children
        // (innermost first), so a same-cycle Vacant-insert delivery
        // from a covered `?` — or from an inner handler's rethrow — is
        // seen this cycle (the try-era handler-after-body order).
        let _ = self.handler.update(ctx, event);
        TagValue::phantom_ref()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.bind_id, self.top_id);
        self.handler.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.handler.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.handler.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        // Siblings typecheck first, so Qop/CallSite throws are already
        // unioned into the bind. Snapshot them, then ascribe `T` so the
        // handler sees `e: T` rather than the inferred union.
        if let Some(t) = self.constraint.clone() {
            let tv = {
                let bind = ctx
                    .env
                    .by_id
                    .get(&self.bind_id)
                    .ok_or_else(|| anyhow!("BUG: catch bind vanished"))?;
                match &bind.typ {
                    Type::TVar(tv) => tv.clone(),
                    _ => unreachable!(),
                }
            };
            if self.thrown.is_none() {
                let contents = tv.read().typ.read().typ.clone().unwrap_or(Type::Bottom);
                self.thrown = Some(contents);
            }
            tv.read().typ.write().typ = Some(t);
        }
        wrap!(self.handler, self.handler.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.handler, self.handler.typecheck1(ctx))?;
        // `catch(e: T)`: T is the type of `e`. It must still cover every
        // error the region throws (the snapshot from typecheck0). A call
        // site's compile-time `ftype.throws` supersets later instance
        // interiors, so this stays sound for runtime-bound callees.
        if let Some(t) = &self.constraint {
            let accumulated = self
                .thrown
                .as_ref()
                .ok_or_else(|| anyhow!("BUG: catch ascription snapshot missing"))?;
            wrap!(self, t.check_contains(&ctx.env, accumulated))?;
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        refs.bound.insert(self.bind_id);
        self.handler.refs(refs);
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Catch(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // A catch is a fusion boundary (no `emit_clif`): the handler
        // reads the error variable a handler-ful `?` writes, and that
        // read is necessarily a separate kernel. The handler's own
        // subtrees fuse.
        crate::fusion::fuse(&mut self.handler, ctx)?;
        Ok(None)
    }
}

/// Deliver a `?`'s error `e` (the raw error payload) to the catch
/// handler `(handler_id, handler_top)` on behalf of the `?` at
/// `spec` under `own_top` — the ONE handler path, shared by
/// `Qop::update` and the fused kernel's delivery drain
/// (`Kernel::update`): `wrap_error` against the env, then a same-top
/// delivery uses the same-cycle Vacant-insert (or `set_var` when the
/// handler's variable already holds this cycle's value), a CROSS-top
/// delivery (REPL: a catch installed by an earlier input) goes through
/// `rt.set_var`, and inside an evaluation frame the delivery is parked
/// (`ExecCtx::frame_outbox`) — the frame's private `event.variables`
/// is discarded when the pass ends, and a handler delivery is
/// outward-bound.
pub(crate) fn deliver_error<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    (id, handler_top): (BindId, ExprId),
    own_top: ExprId,
    spec: &Expr,
    e: Value,
) {
    let e = wrap_error(&ctx.env, spec, e);
    let v = Value::Error(e.into());
    if handler_top != own_top {
        ctx.rt.set_var(id, v)
    } else if ctx.frame_depth > 0 {
        ctx.frame_outbox.push((id, v));
    } else {
        match event.variables.entry(id) {
            Entry::Vacant(slot) => {
                slot.insert(TagValue::fired(v));
            }
            Entry::Occupied(_) => ctx.rt.set_var(id, v),
        }
    }
}

/// A fused handler-ful `?` site: what the kernel's delivery drain
/// needs to run [`deliver_error`] for an error the emitted code
/// raised there. Interned per site (`BodyCx::interned_qop_site`) and
/// kept alive by the kernel's `KernelValues`.
#[derive(Debug)]
pub struct QopSite {
    pub handler: (BindId, ExprId),
    pub own_top: ExprId,
    pub spec: Expr,
}

#[derive(Debug)]
pub struct Qop<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    /// The resolved handler: its error-variable bind and the TOP the
    /// handler node lives under. A same-top delivery uses the
    /// same-cycle Vacant-insert; a CROSS-top delivery (REPL: catch
    /// installed by an earlier input) must go through `rt.set_var` —
    /// the insert only reaches nodes that update later in the same
    /// cycle, and cross-top ordering is not ours to assume.
    pub id: Option<(BindId, ExprId)>,
    pub(crate) top_id: ExprId,
    pub n: Node<R, E>,
    resident: TagValue,
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
        let id = match scope.dynamic.catch() {
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
        Ok(Node::new(Self { spec, typ, id, top_id, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Qop<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        if tv.tag().is_bottom() {
            // a bottom (incl. the phantom) is not an error VALUE —
            // pass it on
            return tv;
        }
        let err = tv.with_value(|v| match v {
            Value::Error(e) => Some(e.clone()),
            _ => None,
        });
        // Handler dispatch and logging key on the error arriving as an
        // EVENT: a stale error re-delivery is the value channel and
        // rides (no re-dispatch, no log spam — Q2's standing-bottoms-
        // never-log rule).
        let fired = tv.tag().is_fired();
        match err {
            None => tv,
            Some(_) if !fired => self.resident.ride(),
            Some(e) => match self.id {
                Some(handler) => {
                    deliver_error(
                        ctx,
                        event,
                        handler,
                        self.top_id,
                        &self.spec,
                        (*e).clone(),
                    );
                    // the consumed error event produces an event with
                    // no value
                    self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                }
                None => {
                    // LOG EVERYWHERE (Q2): a fresh unhandled error
                    // logs at every depth; standing bottoms never log.
                    log::error!(
                        "unhandled error in {} at {} {e}",
                        self.spec.ori,
                        self.spec.pos
                    );
                    eprintln!(
                        "unhandled error in {} at {} {e}",
                        self.spec.ori, self.spec.pos
                    );
                    self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                }
            },
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
        if let Some((id, _)) = self.id {
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
        // A handler-ful `?` (`id: Some` — caught by an enclosing catch)
        // raises its error onto the invocation's delivery queue; the
        // kernel node delivers it after the run through
        // [`deliver_error`], the interp's exact path. A handler-LESS
        // `?` just bottoms.
        let handler = self.id.map(|handler| {
            cx.interned_qop_site(QopSite {
                handler,
                own_top: self.top_id,
                spec: self.spec.clone(),
            })
        });
        emit_qop_node(cx, self.spec.id, &self.n, &self.typ, handler)
    }
}

#[derive(Debug)]
pub struct OrNever<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Node<R, E>,
    resident: TagValue,
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
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for OrNever<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        if tv.tag().is_bottom() {
            return tv;
        }
        let err = tv.with_value(|v| match v {
            Value::Error(e) => Some(e.clone()),
            _ => None,
        });
        match err {
            None => tv,
            Some(e) => {
                // LOG EVERYWHERE (Q2): a fresh ignored error logs at
                // every depth; a stale error re-delivery rides.
                if tv.tag().is_fired() {
                    log::warn!(
                        "ignored error in {} at {} {e}",
                        self.spec.ori,
                        self.spec.pos
                    );
                    self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                } else {
                    self.resident.ride()
                }
            }
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
        emit_qop_node(cx, self.spec.id, &self.n, &self.typ, None)
    }
}
