// CR claude for eric: [style] `crate::image` is imported twice outside the
// `crate::{..}` group; `crate::fusion::fuse` (4 uses), `super::read_var` and
// `super::VarRead` are spelled out at their uses. Merge and import.
use crate::image::ImageBuf;
use crate::image::{
    self,
    nodes::{
        NodeTag, decode_node, opt_node_decode, opt_node_encode, opt_node_len, put_tag,
        tag_len,
    },
};
use crate::{
    BindId, CFlag, ErrorHandler, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt,
    Scope, Tag, TagValue, Update, UserEvent,
    compiler::compile,
    defetyp, deref_typ,
    env::Env,
    expr::{self, Expr, ExprId, ExprKind, ModPath, WrittenAt},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, QopSink, emit_qop_node},
    typ::{Type, TypeRef},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use cranelift_codegen::ir::Value as ClifValue;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, ValArray, Value};
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

/// The fields of `ErrChain<'a>` in a structurally typed world: a struct
/// with exactly these names IS the chain.
fn is_echain_shape(fields: &[(ArcStr, Type, WrittenAt)]) -> bool {
    const NAMES: [&str; 4] = ["cause", "error", "ori", "pos"];
    fields.len() == NAMES.len()
        && NAMES.iter().all(|n| fields.iter().any(|(f, _, _)| f.as_str() == *n))
}

pub(crate) fn wrap_error(env: &Env, spec: &Expr, e: Value) -> Value {
    // CR claude for eric: [risk] a process-wide static `TypeRef` carries a
    // write-once resolution cell: the first ExecCtx whose `is_a` resolves
    // `ErrChain` fills it with that env's typedef, and every other context in the
    // process reuses it (the rule: statics never cache what can differ between
    // contexts). Build the ref per call or keep it in the context. Below,
    // `error[1]` relies on the sorted field order without saying so.
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

// CR claude for eric: [structure] `seq_abort`, `capture` and `received` are one
// seq-only concern spread over three independent fields: `capture` is only read
// when `seq_abort` is `Some` (`first`), so `(capture: Some, seq_abort: None)` is
// representable and inert. Fold them into one `Option<SeqCatch>`. `typ` is
// always `Type::Bottom` yet stored and imaged; return `&Type::Bottom` as
// `Connect` does.
#[derive(Debug)]
pub struct Catch<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub handler: Node<R, E>,
    pub(crate) seq_abort: Option<SeqAbort<R, E>>,
    /// A seq `try`'s capture cell (§7.9): receives the first error of
    /// each failure and the union of this handler's inferred throws.
    capture: Option<BindId>,
    own_handler: ErrorHandler,
    received: u64,
    last_cycle: Option<u64>,
    constraint: Option<Type>,
    /// Throws unioned into the bind before `catch(e: T)` ascribes `T`
    /// onto it. Coverage is this snapshot, not the bind after ascription.
    thrown: Option<Type>,
    bind_id: BindId,
    top_id: ExprId,
    typ: Type,
}

#[derive(Debug)]
pub(crate) struct SeqAbort<R: Rt, E: UserEvent> {
    pub(crate) node: Node<R, E>,
    /// A machine's `abort(..)` event, which a [`SeqAbortEvent`] counted
    /// into the handler's generation before the machine updated: a fired
    /// production runs `node` with no error in flight.
    pub(crate) manual: Option<Node<R, E>>,
    /// A machine's step variable, written idle when the machine sleeps.
    pc: Option<BindId>,
    pending: bool,
}

impl<R: Rt, E: UserEvent> Catch<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let handler = decode_node(ctx, buf)?;
        let seq_abort = match opt_node_decode(ctx, buf)? {
            None => None,
            Some(node) => Some(SeqAbort {
                node,
                manual: opt_node_decode(ctx, buf)?,
                pc: Option::<BindId>::decode(buf)?,
                pending: false,
            }),
        };
        let capture = Option::<BindId>::decode(buf)?;
        let own_handler = image::handler_decode(buf)?;
        let constraint = Option::<Type>::decode(buf)?;
        let thrown = Option::<Type>::decode(buf)?;
        let bind_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let typ = Type::decode(buf)?;
        ctx.rt.ref_var(bind_id, top_id);
        Ok(Node::new(Self {
            spec,
            handler,
            seq_abort,
            capture,
            own_handler,
            received: 0,
            last_cycle: None,
            constraint,
            thrown,
            bind_id,
            top_id,
            typ,
        }))
    }

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
            .bind_variable(
                &catch_scope.lexical,
                &c.bind,
                typ,
                c.bind.pos_or(spec.pos),
                spec.ori.clone(),
            )
            .id;
        // the handler compiles before this catch registers, so a
        // rethrowing `?` inside it never resolves to itself
        let handler = compile(ctx, flags, (*c.handler).clone(), &catch_scope, top_id)?;
        let covered = scope.with_catch((bind_id, top_id), c.seq_pc.is_some());
        let lookup = |ctx: &ExecCtx<R, E>, name: &ArcStr| {
            let path = ModPath::from([name.as_str()]);
            match ctx.env.lookup_bind(&scope.lexical, &path)? {
                Some((_, b)) => Ok(b.id),
                None => bail!("BUG: seq cell {name} is not bound"),
            }
        };
        let seq_abort = match &c.seq_abort {
            None if c.seq_manual.is_some() || c.seq_pc.is_some() => {
                bail!("BUG: a seq abort event without an abort action")
            }
            None => None,
            Some(e) => Some(SeqAbort {
                node: compile(ctx, flags, (**e).clone(), &catch_scope, top_id)?,
                manual: c
                    .seq_manual
                    .as_ref()
                    .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
                    .transpose()?,
                pc: c.seq_pc.as_ref().map(|n| lookup(ctx, n)).transpose()?,
                pending: false,
            }),
        };
        let capture = c.seq_capture.as_ref().map(|n| lookup(ctx, n)).transpose()?;
        ctx.rt.ref_var(bind_id, top_id);
        let node = Node::new(Self {
            spec,
            handler,
            seq_abort,
            capture,
            own_handler: covered.dynamic.handler().unwrap(),
            received: 0,
            last_cycle: None,
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
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.handler.image_len()
            + opt_node_len(self.seq_abort.as_ref().map(|a| &a.node))
            + self
                .seq_abort
                .as_ref()
                .map_or(0, |a| opt_node_len(a.manual.as_ref()) + a.pc.encoded_len())
            + self.capture.encoded_len()
            + image::handler_len(&self.own_handler)
            + self.constraint.encoded_len()
            + self.thrown.encoded_len()
            + self.bind_id.encoded_len()
            + self.top_id.encoded_len()
            + self.typ.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Catch, buf);
        self.spec.encode(buf)?;
        self.handler.image_encode(buf)?;
        opt_node_encode(self.seq_abort.as_ref().map(|a| &a.node), buf)?;
        if let Some(a) = &self.seq_abort {
            opt_node_encode(a.manual.as_ref(), buf)?;
            a.pc.encode(buf)?;
        }
        self.capture.encode(buf)?;
        image::handler_encode(&self.own_handler, buf)?;
        self.constraint.encode(buf)?;
        self.thrown.encode(buf)?;
        self.bind_id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let _ = self.handler.update(ctx, event);
        let cycle = ctx.rt.cycle();
        let first = self.seq_abort.as_ref().is_some_and(|a| !a.pending);
        let delivered = match super::read_var(ctx, event, &self.bind_id) {
            Some(super::VarRead::Delivered(tv))
                if self.last_cycle != Some(cycle) && tv.tag().is_fired() =>
            {
                Some((first && self.capture.is_some()).then(|| tv.value_cloned()))
            }
            _ => None,
        };
        if let Some(captured) = delivered {
            self.last_cycle = Some(cycle);
            self.received = self.received.wrapping_add(1);
            self.own_handler.handled();
            if let (Some(cap), Some(v)) = (self.capture, captured) {
                ctx.rt.set_var(cap, v);
            }
            if let Some(abort) = &mut self.seq_abort {
                abort.pending = true;
            }
        }
        if let Some(abort) = &mut self.seq_abort {
            if let Some(manual) = &mut abort.manual
                && manual.update(ctx, event).is_fired()
            {
                self.received = self.received.wrapping_add(1);
                abort.pending = true;
            }
            if abort.pending
                && self.received == self.own_handler.generation()
                && !self.own_handler.has_nested_errors()
            {
                abort.pending = false;
                let init = std::mem::replace(&mut event.init, true);
                let dispatch_init = std::mem::replace(&mut ctx.dispatch_init, true);
                let _ = abort.node.update(ctx, event);
                ctx.dispatch_init = dispatch_init;
                event.init = init;
            }
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.bind_id, self.top_id);
        self.handler.delete(ctx);
        if let Some(abort) = &mut self.seq_abort {
            abort.node.delete(ctx);
            abort.manual.iter_mut().for_each(|n| n.delete(ctx));
        }
    }

    // CR claude for eric: [risk] suspected: `handled()` runs only when an awake
    // catch sees a delivery. A delivery still in flight when the catch sleeps or
    // is deleted (the second same-cycle error, queued by `deliver_error`'s
    // `Occupied` arm, or a cross-top one) is never acknowledged, so every
    // ancestor's `nested` count stays raised for its lifetime and an enclosing
    // seq guard answers bottom from then on. Reconcile outstanding raises
    // (`generation` vs handled) here and in `delete`.
    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.handler.sleep(ctx);
        if let Some(abort) = &mut self.seq_abort {
            abort.node.sleep(ctx);
            abort.manual.iter_mut().for_each(|n| n.sleep(ctx));
            abort.pending = false;
            if let Some(pc) = abort.pc {
                ctx.rt.set_var(pc, Value::String(literal!("Idle")));
            }
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.handler.reset_replay(ctx);
        if let Some(abort) = &mut self.seq_abort {
            abort.node.reset_replay(ctx);
            abort.manual.iter_mut().for_each(|n| n.reset_replay(ctx));
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        // siblings typecheck first, so the region's throws are already
        // unioned into the bind: snapshot them, then ascribe `T`
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
        wrap!(self.handler, self.handler.typecheck0(ctx))?;
        if let Some(abort) = &mut self.seq_abort {
            wrap!(abort.node, abort.node.typecheck0(ctx))?;
            if let Some(manual) = &mut abort.manual {
                wrap!(manual, manual.typecheck0(ctx))?;
            }
        }
        // the capture cell's type is the union of every covering
        // handler's throws
        if let Some(cap) = self.capture {
            let etyp = ctx
                .env
                .by_id
                .get(&self.bind_id)
                .map(|b| b.typ.clone())
                .ok_or_else(|| anyhow!("BUG: catch bind vanished"))?;
            let bind = ctx
                .env
                .by_id
                .get(&cap)
                .ok_or_else(|| anyhow!("BUG: seq capture cell vanished"))?;
            let Type::TVar(tv) = &bind.typ else {
                bail!("BUG: seq capture cell is not a cell")
            };
            // union the cell's content, not the cell
            let etyp = match &etyp {
                Type::TVar(b) => b.read().typ.read().typ.clone().unwrap_or(Type::Bottom),
                t => t.clone(),
            };
            let joined = {
                let tv = tv.read();
                let cell = tv.typ.read();
                match &cell.typ {
                    None => etyp.clone(),
                    Some(t) => Type::union(&ctx.env, &[t, &etyp])?,
                }
            };
            tv.read().typ.write().typ = Some(joined);
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.handler, self.handler.typecheck1(ctx))?;
        if let Some(abort) = &mut self.seq_abort {
            wrap!(abort.node, abort.node.typecheck1(ctx))?;
            if let Some(manual) = &mut abort.manual {
                wrap!(manual, manual.typecheck1(ctx))?;
            }
        }
        // `T` must cover every error the region throws (the typecheck0
        // snapshot); a call site's `ftype.throws` supersets later
        // instance interiors, so this holds for runtime-bound callees
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
        if let Some(abort) = &self.seq_abort {
            abort.node.refs(refs);
            abort.manual.iter().for_each(|n| n.refs(refs));
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Catch(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // a catch is a fusion boundary; the handler's own subtrees fuse
        crate::fusion::fuse(&mut self.handler, ctx)?;
        if let Some(abort) = &mut self.seq_abort {
            crate::fusion::fuse(&mut abort.node, ctx)?;
            if let Some(manual) = &mut abort.manual {
                crate::fusion::fuse(manual, ctx)?;
            }
        }
        Ok(None)
    }
}

defetyp!(NULL_ERR, NULL_ERR_TAG, "NullError", "Error<`{}(string)>");

/// What a `?`/`$` takes out of its operand: the errors when the operand
/// has any, otherwise the null.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Strip {
    Error,
    Null,
}

impl Strip {
    fn of<R: Rt, E: UserEvent>(
        ctx: &ExecCtx<R, E>,
        op: char,
        typ: &Type,
    ) -> Result<Self> {
        let err = Type::Error(Arc::new(Type::empty_tvar()));
        let null = Type::Primitive(Typ::Null.into());
        if typ.contains_with_flags(BitFlags::empty(), &ctx.env, &err)? {
            Ok(Self::Error)
        } else if typ.contains_with_flags(BitFlags::empty(), &ctx.env, &null)? {
            Ok(Self::Null)
        } else {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "cannot use the {op} operator on {typ}, it has no error and no null"
                )
            })
        }
    }

    fn removed(self) -> Type {
        match self {
            Self::Error => Type::Primitive(Typ::Error.into()),
            Self::Null => Type::Primitive(Typ::Null.into()),
        }
    }

    fn strips(self, v: &Value) -> bool {
        match self {
            Self::Error => matches!(v, Value::Error(_)),
            Self::Null => matches!(v, Value::Null),
        }
    }

    fn decode(buf: &mut &[u8]) -> Result<Self, PackError> {
        Ok(if bool::decode(buf)? { Self::Null } else { Self::Error })
    }

    fn encode(self, buf: &mut ImageBuf) -> Result<(), PackError> {
        (self == Self::Null).encode(buf)
    }
}

/// The payload a `?` raises for a null operand: `NullError` naming the
/// operand.
pub(crate) fn null_error(spec: &Expr) -> Value {
    let operand = match &spec.kind {
        ExprKind::Qop(e) => format_compact!("{e}"),
        _ => format_compact!("{spec}"),
    };
    let tag = Value::String(NULL_ERR_TAG.clone());
    Value::Array(ValArray::from_iter([tag, Value::from(operand)]))
}

// CR claude for eric: [readability] `spec.ori` renders as "in file ...", so the
// message reads "unhandled error in in file ..." (probed, both engines; the JIT
// helper `graphix_swallowed_error` re-spells the same format, as does
// `OrNever`'s "ignored error in {} at {}"). Drop the literal "in" and keep one
// formatter for both engines.
/// What a handler-less `?` reports for the error payload `e` it raised.
pub(crate) fn unhandled_msg(spec: &Expr, e: &Value) -> ArcStr {
    format_compact!("unhandled error in {} at {} {e}", spec.ori, spec.pos).as_str().into()
}

/// Deliver a `?`'s raw error payload `e` to `handler` on behalf of the
/// `?` at `spec` under `own_top`. The one handler path, shared by
/// `Qop::update` and the fused kernel's delivery drain: same-top
/// deliveries land in this cycle's event, cross-top ones go through
/// `rt.set_var`, and inside an evaluation frame the delivery is parked
/// in `ExecCtx::frame_outbox`.
pub(crate) fn deliver_error<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    handler: &ErrorHandler,
    own_top: ExprId,
    spec: &Expr,
    e: Value,
) {
    let (id, handler_top) = handler.id();
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

/// The interned "origin at position" a fused `$` or handler-less `?`
/// names when it logs a swallowed error.
fn diagnostic_site(cx: &mut BodyCx, spec: &Expr) -> Result<ClifValue> {
    cx.interned_str(&format_compact!("{} at {}", spec.ori, spec.pos).as_str().into())
}

/// A fused handler-ful `?` site: what the kernel's delivery drain needs
/// to run [`deliver_error`] for an error raised there.
#[derive(Debug)]
pub struct QopSite {
    pub(crate) handler: ErrorHandler,
    pub own_top: ExprId,
    pub spec: Expr,
}

#[derive(Debug)]
pub struct Qop<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    /// The resolved handler: its error-variable bind and the top the
    /// handler node lives under.
    pub(crate) handler: Option<ErrorHandler>,
    pub(crate) top_id: ExprId,
    pub n: Node<R, E>,
    pub(crate) strip: Strip,
    resident: TagValue,
    flags: BitFlags<CFlag>,
}

impl<R: Rt, E: UserEvent> Qop<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let handler = match bool::decode(buf)? {
            true => Some(image::handler_decode(buf)?),
            false => None,
        };
        let top_id = ExprId::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let strip = Strip::decode(buf)?;
        let flags = image::flags_decode(buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            handler,
            top_id,
            n,
            strip,
            resident: TagValue::phantom(),
            flags,
        }))
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let handler = scope.dynamic.handler();
        if handler.is_none() && !matches!(spec.kind, ExprKind::Rethrow(_)) {
            Self::check_unhandled(&ctx.env, flags, &spec)?;
        }
        let typ = Type::empty_tvar();
        Ok(Node::new(Self {
            spec,
            typ,
            handler,
            top_id,
            n,
            strip: Strip::Error,
            resident: TagValue::phantom(),
            flags,
        }))
    }

    // CR claude for eric: [readability] the error is a string with the origin
    // and position printed into it (and an "ERROR:" prefix) rather than a
    // `bailat!(spec, ..)`, so it carries no `ErrorSite` and the LSP cannot place
    // it on the `?`.
    fn check_unhandled(env: &Env, flags: BitFlags<CFlag>, spec: &Expr) -> Result<()> {
        if flags.contains(CFlag::WarnUnhandled | CFlag::WarningsAreErrors) {
            bail!(
                "ERROR: {} at {} error raised by ? will not be caught",
                spec.ori,
                spec.pos
            )
        }
        if flags.contains(CFlag::WarnUnhandled) {
            let msg = "error raised by ? will not be caught";
            env.warn(&spec.ori, spec.pos, spec.end.0, msg);
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Qop<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + 1
            + self.handler.as_ref().map_or(0, image::handler_len)
            + self.top_id.encoded_len()
            + self.n.image_len()
            + 1
            + image::flags_len(self.flags)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Qop, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.handler.is_some().encode(buf)?;
        if let Some(h) = &self.handler {
            image::handler_encode(h, buf)?;
        }
        self.top_id.encode(buf)?;
        self.n.image_encode(buf)?;
        self.strip.encode(buf)?;
        image::flags_encode(self.flags, buf)
    }

    // CR claude for eric: [structure] `Qop` and `OrNever` repeat one skeleton
    // (bottom passes, strip test, stale strip rides, fired strip bottoms) and one
    // typecheck (`Strip::of` + `diff`), differing only in the sink. The copies
    // have already drifted: `?` turns an uninhabited result into `Bottom`, `$`
    // leaves the empty union (probe: `let x: Error<`E> = error(`E); select x$ {
    // i64 as n => n, _ => 0 }` reports "pattern i64 will never match '_: []",
    // the `?` form "unreachable arm"). Share the skeleton and the typing.
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        if tv.tag().is_bottom() {
            // a bottom is not an error value
            return tv;
        }
        let strip = self.strip;
        let err = tv.with_value(|v| match v {
            Value::Error(e) if strip == Strip::Error => Some((**e).clone()),
            Value::Null if strip == Strip::Null => Some(null_error(&self.spec)),
            _ => None,
        });
        let fired = tv.tag().is_fired();
        match err {
            None => tv,
            Some(_) if !fired => self.resident.ride(),
            Some(e) => match &self.handler {
                Some(handler) => {
                    handler.raise();
                    deliver_error(ctx, event, handler, self.top_id, &self.spec, e);
                    self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                }
                None => {
                    let msg = unhandled_msg(&self.spec, &e);
                    log::error!("{msg}");
                    eprintln!("{msg}");
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

    // CR claude for eric: [structure] a 55-line recursive helper nested inside
    // `typecheck0`: `fix_echain_typ` is the ErrChain typing rule and belongs
    // beside `wrap_error`/`typ_echain` at module level, where its pairing with
    // `wrap_error`'s chaining is visible.
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
                    // a chain arriving expanded through a call site's
                    // throws is the same chain: `wrap_error` chains the
                    // value rather than nesting it, so the type must not nest
                    Some(Type::Struct(fields)) if is_echain_shape(fields) => {
                        Ok(etyp.clone())
                    }
                    Some(et) => {
                        // the chain may arrive as a Ref from another scope
                        let expanded = match et {
                            Type::Ref(_) => Some(et.lookup_ref(&ctx.env)?),
                            _ => None,
                        };
                        let chain = matches!(
                            expanded.as_ref().unwrap_or(et),
                            Type::Struct(fields) if is_echain_shape(fields)
                        );
                        if chain {
                            Ok(etyp.clone())
                        } else {
                            Ok(Type::Error(Arc::new(typ_echain(et.clone()))))
                        }
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
        if matches!(self.spec.kind, ExprKind::Rethrow(_)) {
            if self.n.typ().with_deref(|t| matches!(t, Some(Type::Bottom))) {
                return self.typ.check_contains(&ctx.env, &Type::Bottom);
            }
            if self.handler.is_none() {
                Self::check_unhandled(&ctx.env, self.flags, &self.spec)?;
            }
        }
        self.strip = wrap!(self, Strip::of(ctx, '?', self.n.typ()))?;
        let rtyp = self.n.typ().diff(&ctx.env, &self.strip.removed())?;
        // a `?` with no member left never produces: bottom, which a
        // select absorbs, not an empty union no pattern could match
        let rtyp = if rtyp.is_uninhabited() { Type::Bottom } else { rtyp };
        wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        if let Some(handler) = &self.handler {
            let (id, _) = handler.id();
            let etyp = match self.strip {
                Strip::Error => self.n.typ().diff(&ctx.env, &rtyp)?,
                Strip::Null => NULL_ERR.clone(),
            };
            let etyp = if matches!(self.spec.kind, ExprKind::Rethrow(_)) {
                etyp
            } else {
                wrap!(self, fix_echain_typ(&ctx, &etyp))?
            };
            let bind = ctx.env.by_id.get(&id).ok_or_else(|| anyhow!("BUG: catch"))?;
            match &bind.typ {
                Type::TVar(tv) => {
                    let tv = tv.read();
                    let mut cell = tv.typ.write();
                    cell.typ = match &cell.typ {
                        None => Some(etyp.clone()),
                        Some(t) => Some(Type::union(&ctx.env, &[t, &etyp])?),
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
        let sink = match (self.handler.as_ref(), self.strip) {
            (None, Strip::Error) => {
                QopSink::Log { site: diagnostic_site(cx, &self.spec)?, unhandled: true }
            }
            (None, Strip::Null) => QopSink::UnhandledNull(
                cx.interned_str(&unhandled_msg(&self.spec, &null_error(&self.spec)))?,
            ),
            (Some(handler), strip) => {
                let site = cx.interned_qop_site(QopSite {
                    handler: handler.clone(),
                    own_top: self.top_id,
                    spec: self.spec.clone(),
                })?;
                match strip {
                    Strip::Error => QopSink::Deliver(site),
                    Strip::Null => QopSink::DeliverNull(site),
                }
            }
        };
        emit_qop_node(cx, self.spec.id, &self.n, &self.typ, sink)
    }
}

#[derive(Debug)]
enum GuardState {
    Sleeping,
    /// Entered under this handler generation; before a fired production
    /// has passed, a standing value is the previous run's answer.
    Running {
        generation: (u64, u64),
        fired_since_entry: bool,
    },
    Failed,
}

#[derive(Debug)]
pub struct SeqGuard<R: Rt, E: UserEvent> {
    spec: Expr,
    pub(crate) n: Node<R, E>,
    handler: ErrorHandler,
    /// The machine's handler: an `abort(..)` fails the run there, and a
    /// try-body arm's nearest handler is its jump.
    machine: ErrorHandler,
    state: GuardState,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> SeqGuard<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let handler = image::handler_decode(buf)?;
        let machine = image::handler_decode(buf)?;
        Ok(Node::new(Self {
            spec,
            n,
            handler,
            machine,
            state: GuardState::Sleeping,
            resident: TagValue::phantom(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let handler =
            scope.dynamic.handler().ok_or_else(|| anyhow!("BUG: seq handler"))?;
        let machine = handler.machine().ok_or_else(|| anyhow!("BUG: seq machine"))?;
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        Ok(Node::new(Self {
            spec,
            n,
            handler,
            machine,
            state: GuardState::Sleeping,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for SeqGuard<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.n.image_len()
            + image::handler_len(&self.handler)
            + image::handler_len(&self.machine)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqGuard, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)?;
        image::handler_encode(&self.handler, buf)?;
        image::handler_encode(&self.machine, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let current = (self.handler.generation(), self.machine.generation());
        let (generation, fired_since_entry) = match self.state {
            GuardState::Sleeping if self.machine.aborted_in(ctx.rt.cycle()) => {
                self.state = GuardState::Failed;
                return self
                    .resident
                    .set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
            }
            GuardState::Sleeping => {
                let generation = current;
                self.state = GuardState::Running { generation, fired_since_entry: false };
                (generation, false)
            }
            GuardState::Running { generation, fired_since_entry } => {
                (generation, fired_since_entry)
            }
            GuardState::Failed => {
                if self.handler.has_nested_errors() {
                    let _ = self.n.update(ctx, event);
                }
                return self.resident.ride();
            }
        };
        // CR claude for eric: [bug] a FIRED production that arrives while a nested
        // catch still has an error in flight falls through to FRESH_BOTTOM and is
        // forgotten (`fired_since_entry` stays false); once the nested count
        // drains the value is stale and the guard answers bottom forever, so the
        // run never completes. Probe: a callee with `catch(e) ..` and two `?`
        // raising in one cycle (the second queues via `set_var`) called as `seq
        // let k = n { f(k) }` wedges the seq for good; with one `?` it proceeds
        // (both engines). design/seq_blocks.md §7.4 says a nested catch may
        // consume its errors without aborting. Remember the suppressed fire and
        // pass the value when the count drains.
        if generation == current || self.handler.has_nested_errors() {
            let value = self.n.update(ctx, event);
            let current = (self.handler.generation(), self.machine.generation());
            if generation == current && !self.handler.has_nested_errors() {
                if fired_since_entry || value.tag().is_bottom() {
                    return value;
                }
                if value.is_fired() {
                    self.state =
                        GuardState::Running { generation, fired_since_entry: true };
                    return value;
                }
                return TagValue::bottom_null(false);
            }
        }
        if generation != (self.handler.generation(), self.machine.generation()) {
            self.state = GuardState::Failed;
        }
        self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.state = GuardState::Sleeping;
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.state = GuardState::Sleeping;
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck0(ctx)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck1(ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        self.n.typ()
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::SeqGuard(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        crate::fusion::fuse(&mut self.n, ctx)?;
        Ok(None)
    }
}

/// A seq's `abort(..)` event, placed before the machine's select: a
/// fired production fails the run in the cycle it fires, so no guard
/// under the machine passes a completion that cycle. A block updates its
/// catches after their covered children, which is too late for that.
#[derive(Debug)]
pub struct SeqAbortEvent<R: Rt, E: UserEvent> {
    spec: Expr,
    pub(crate) n: Node<R, E>,
    machine: ErrorHandler,
}

impl<R: Rt, E: UserEvent> SeqAbortEvent<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let machine = image::handler_decode(buf)?;
        Ok(Node::new(Self { spec, n, machine }))
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let machine = scope
            .dynamic
            .handler()
            .and_then(|h| h.machine())
            .ok_or_else(|| anyhow!("BUG: seq machine"))?;
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        Ok(Node::new(Self { spec, n, machine }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for SeqAbortEvent<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.n.image_len()
            + image::handler_len(&self.machine)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqAbort, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)?;
        image::handler_encode(&self.machine, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.n.update(ctx, event).is_fired() {
            self.machine.abort(ctx.rt.cycle());
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck0(ctx)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck1(ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::SeqAbort(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        crate::fusion::fuse(&mut self.n, ctx)?;
        Ok(None)
    }
}

#[derive(Debug)]
pub struct OrNever<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Node<R, E>,
    pub(crate) strip: Strip,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> OrNever<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let strip = Strip::decode(buf)?;
        Ok(Node::new(Self { spec, typ, n, strip, resident: TagValue::phantom() }))
    }

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
        let strip = Strip::Error;
        Ok(Node::new(Self { spec, typ, n, strip, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for OrNever<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.n.image_len()
            + 1
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::OrNever, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)?;
        self.strip.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        if tv.tag().is_bottom() {
            return tv;
        }
        let strip = self.strip;
        if !tv.with_value(|v| strip.strips(v)) {
            return tv;
        }
        if !tv.tag().is_fired() {
            return self.resident.ride();
        }
        // only a fresh error logs; a null is not a failure
        tv.with_value(|v| {
            if let Value::Error(e) = v {
                log::warn!("ignored error in {} at {} {e}", self.spec.ori, self.spec.pos)
            }
        });
        self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
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
        self.strip = wrap!(self, Strip::of(ctx, '$', self.n.typ()))?;
        let rtyp = self.n.typ().diff(&ctx.env, &self.strip.removed())?;
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
        let sink = match self.strip {
            Strip::Error => {
                QopSink::Log { site: diagnostic_site(cx, &self.spec)?, unhandled: false }
            }
            Strip::Null => QopSink::DropNull,
        };
        emit_qop_node(cx, self.spec.id, &self.n, &self.typ, sink)
    }
}
