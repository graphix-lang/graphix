use super::{VarRead, read_var};
use crate::{
    BindId, CFlag, ErrorHandler, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt,
    Scope, Tag, TagValue, Update, UserEvent,
    compiler::compile,
    defetyp, deref_typ,
    env::Env,
    expr::{self, Expr, ExprId, ExprKind, ModPath, WrittenAt},
    format_with_flags,
    fusion::{
        emit::{BodyCx, CompiledExpr, QopSink, emit_qop_node},
        fuse,
    },
    image::{
        self, ImageBuf,
        nodes::{
            NodeTag, decode_node, opt_node_decode, opt_node_encode, opt_node_len,
            put_tag, tag_len,
        },
    },
    typ::{Type, TypeRef},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, ValArray, Value};
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, fmt, sync::LazyLock};
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
    let pos: Value =
        [(literal!("column"), spec.pos.column), (literal!("line"), spec.pos.line)].into();
    let (cause, error) = if typ_echain(Type::empty_tvar()).is_a(env, &e) {
        let fields = e.clone().cast_to::<[(ArcStr, Value); 4]>().unwrap();
        let error = fields.into_iter().find(|(n, _)| n == "error").unwrap().1;
        (e, error)
    } else {
        (Value::Null, e)
    };
    [
        (literal!("cause"), cause),
        (literal!("error"), error),
        (literal!("ori"), spec.ori.to_value()),
        (literal!("pos"), pos),
    ]
    .into()
}

#[derive(Debug)]
pub struct Catch<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub handler: Node<R, E>,
    pub(crate) seq_abort: Option<SeqAbort<R, E>>,
    own_handler: ErrorHandler,
    /// Raises of `own_handler` acknowledged: delivered, a seq's abort
    /// event, or given up when the catch sleeps or goes away.
    received: u64,
    last_cycle: Option<u64>,
    constraint: Option<Type>,
    /// Throws unioned into the bind before `catch(e: T)` ascribes `T`
    /// onto it. Coverage is this snapshot, not the bind after ascription.
    thrown: Option<Type>,
    bind_id: BindId,
    top_id: ExprId,
}

/// A seq machine's handler, or a `try` body arm's jump: the action run
/// once every error of a failure has arrived.
#[derive(Debug)]
pub(crate) struct SeqAbort<R: Rt, E: UserEvent> {
    pub(crate) node: Node<R, E>,
    /// A machine's `abort(..)` event, which a [`SeqAbortEvent`] counted
    /// into the handler's generation before the machine updated: a fired
    /// production runs `node` with no error in flight.
    pub(crate) manual: Option<Node<R, E>>,
    /// A machine's step variable, written idle when the machine sleeps.
    pc: Option<BindId>,
    /// A `try`'s capture cell (§7.3): receives the first error of each
    /// failure and the union of this handler's inferred throws.
    capture: Option<BindId>,
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
                capture: Option::<BindId>::decode(buf)?,
                pending: false,
            }),
        };
        let own_handler = image::handler_decode(buf)?;
        let constraint = Option::<Type>::decode(buf)?;
        let thrown = Option::<Type>::decode(buf)?;
        let bind_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.rt.ref_var(bind_id, top_id);
        Ok(Node::new(Self {
            spec,
            handler,
            seq_abort,
            own_handler,
            received: 0,
            last_cycle: None,
            constraint,
            thrown,
            bind_id,
            top_id,
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
                tv.freeze();
                tv.bind(Type::Bottom)
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
            None if c.seq_manual.is_some()
                || c.seq_pc.is_some()
                || c.seq_capture.is_some() =>
            {
                bail!("BUG: a seq catch without an abort action")
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
                capture: c.seq_capture.as_ref().map(|n| lookup(ctx, n)).transpose()?,
                pending: false,
            }),
        };
        ctx.rt.ref_var(bind_id, top_id);
        let node = Node::new(Self {
            spec,
            handler,
            seq_abort,
            own_handler: covered.dynamic.handler().unwrap(),
            received: 0,
            last_cycle: None,
            constraint: c.constraint.clone(),
            thrown: None,
            bind_id,
            top_id,
        });
        Ok((node, covered))
    }

    /// Acknowledge the raises whose deliveries this catch will not see,
    /// so no enclosing handler waits on them.
    fn give_up_in_flight(&mut self) {
        while self.received != self.own_handler.generation() {
            self.received = self.received.wrapping_add(1);
            self.own_handler.handled();
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Catch<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.handler.image_len()
            + opt_node_len(self.seq_abort.as_ref().map(|a| &a.node))
            + self.seq_abort.as_ref().map_or(0, |a| {
                opt_node_len(a.manual.as_ref())
                    + a.pc.encoded_len()
                    + a.capture.encoded_len()
            })
            + image::handler_len(&self.own_handler)
            + self.constraint.encoded_len()
            + self.thrown.encoded_len()
            + self.bind_id.encoded_len()
            + self.top_id.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Catch, buf);
        self.spec.encode(buf)?;
        self.handler.image_encode(buf)?;
        opt_node_encode(self.seq_abort.as_ref().map(|a| &a.node), buf)?;
        if let Some(a) = &self.seq_abort {
            opt_node_encode(a.manual.as_ref(), buf)?;
            a.pc.encode(buf)?;
            a.capture.encode(buf)?;
        }
        image::handler_encode(&self.own_handler, buf)?;
        self.constraint.encode(buf)?;
        self.thrown.encode(buf)?;
        self.bind_id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let _ = self.handler.update(ctx, event);
        let cycle = ctx.rt.cycle();
        let capture =
            self.seq_abort.as_ref().and_then(|a| a.capture.filter(|_| !a.pending));
        // a delivery whose raise was given up while asleep is not counted again
        let delivered = match read_var(ctx, event, &self.bind_id) {
            Some(VarRead::Delivered(tv))
                if self.last_cycle != Some(cycle)
                    && tv.tag().is_fired()
                    && self.received != self.own_handler.generation() =>
            {
                Some(capture.map(|cap| (cap, tv.value_cloned())))
            }
            _ => None,
        };
        if let Some(captured) = delivered {
            self.last_cycle = Some(cycle);
            self.received = self.received.wrapping_add(1);
            self.own_handler.handled();
            if let Some((cap, v)) = captured {
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
        self.give_up_in_flight();
        ctx.rt.unref_var(self.bind_id, self.top_id);
        self.handler.delete(ctx);
        if let Some(abort) = &mut self.seq_abort {
            abort.node.delete(ctx);
            abort.manual.iter_mut().for_each(|n| n.delete(ctx));
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.give_up_in_flight();
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
                let contents = tv.binding().unwrap_or(Type::Bottom);
                self.thrown = Some(contents);
            }
            tv.bind(t);
        }
        wrap!(self.handler, self.handler.typecheck0(ctx))?;
        let Some(abort) = &mut self.seq_abort else { return Ok(()) };
        wrap!(abort.node, abort.node.typecheck0(ctx))?;
        if let Some(manual) = &mut abort.manual {
            wrap!(manual, manual.typecheck0(ctx))?;
        }
        // the capture cell's type is the union of every covering
        // handler's throws
        if let Some(cap) = abort.capture {
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
                Type::TVar(b) => b.binding().unwrap_or(Type::Bottom),
                t => t.clone(),
            };
            let joined = match tv.binding() {
                None => etyp.clone(),
                Some(t) => Type::union(&ctx.env, &[&t, &etyp])?,
            };
            tv.bind(joined);
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
        Type::BOTTOM
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
        fuse(&mut self.handler, ctx)?;
        if let Some(abort) = &mut self.seq_abort {
            fuse(&mut abort.node, ctx)?;
            if let Some(manual) = &mut abort.manual {
                fuse(manual, ctx)?;
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

/// The typing `?` and `$` share: what `op` strips from `operand`, and
/// the result type, `operand` less it. With no member left the result
/// never produces: bottom, which a select absorbs, not an empty union
/// no pattern could match.
fn strip_typ<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    op: char,
    operand: &Type,
) -> Result<(Strip, Type)> {
    let strip = Strip::of(ctx, op, operand)?;
    let rtyp = operand.diff(&ctx.env, &strip.removed())?;
    Ok((strip, if rtyp.is_uninhabited() { Type::Bottom } else { rtyp }))
}

/// The production `?` and `$` share: a bottom or a kept value passes
/// through; a stripped one is a bottom, and a fresh one goes to `sink`.
fn strip_production<'a>(
    strip: Strip,
    tv: &'a TagValue,
    resident: &'a mut TagValue,
    sink: impl FnOnce(&Value),
) -> &'a TagValue {
    if tv.tag().is_bottom() || !tv.with_value(|v| strip.strips(v)) {
        return tv;
    }
    if !tv.tag().is_fired() {
        return resident.ride();
    }
    tv.with_value(sink);
    resident.set_bottom(true)
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

/// Where a swallowed error or a failed operator reports itself, in both
/// engines: the origin (`in file ..`), then the position.
pub(crate) fn diagnostic_site(spec: &Expr) -> ArcStr {
    format_compact!("{} at {}", spec.ori, spec.pos).as_str().into()
}

/// What a handler-less `?` at `site` reports for the error payload `e`.
pub(crate) fn unhandled_msg(site: &str, e: &dyn fmt::Display) -> ArcStr {
    format_compact!("unhandled error {site} {e}").as_str().into()
}

/// An error nothing handles, or a hot operator's failure: logged from
/// the calling module, so the log tells the engines apart, and written
/// to stderr, which a shell without a log still shows.
macro_rules! report_failure {
    ($msg:expr) => {{
        let msg: &str = $msg;
        log::error!("{msg}");
        eprintln!("{msg}");
    }};
}
pub(crate) use report_failure;

/// A `$` at `site` dropping the error payload `e`, logged from the
/// calling module.
macro_rules! report_ignored {
    ($site:expr, $e:expr) => {
        log::warn!("ignored error {} {}", $site, $e)
    };
}
pub(crate) use report_ignored;

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

/// The error type a `?` delivers for `etyp`: the payload wrapped in an
/// `ErrChain`, once. `wrap_error` chains a caught error rather than
/// nesting it, so a chain arriving from a callee's throws keeps its type.
fn fix_echain_typ<R: Rt, E: UserEvent>(ctx: &ExecCtx<R, E>, etyp: &Type) -> Result<Type> {
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
            Some(Type::Struct(fields)) if is_echain_shape(fields) => Ok(etyp.clone()),
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

    fn check_unhandled(env: &Env, flags: BitFlags<CFlag>, spec: &Expr) -> Result<()> {
        if flags.contains(CFlag::WarnUnhandled) {
            let msg = "error raised by ? will not be caught";
            env.warn(flags, spec, spec.pos, spec.end.0, msg)?;
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

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        strip_production(self.strip, tv, &mut self.resident, |v| {
            let e = match v {
                Value::Error(e) => (**e).clone(),
                _ => null_error(&self.spec),
            };
            match &self.handler {
                Some(handler) => {
                    handler.raise();
                    deliver_error(ctx, event, handler, self.top_id, &self.spec, e);
                }
                None => report_failure!(&unhandled_msg(&diagnostic_site(&self.spec), &e)),
            }
        })
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
        let rethrow = matches!(self.spec.kind, ExprKind::Rethrow(_));
        if rethrow {
            if self.n.typ().with_deref(|t| matches!(t, Some(Type::Bottom))) {
                return self.typ.check_contains(&ctx.env, &Type::Bottom);
            }
            if self.handler.is_none() {
                Self::check_unhandled(&ctx.env, self.flags, &self.spec)?;
            }
        }
        let (strip, rtyp) = wrap!(self, strip_typ(ctx, '?', self.n.typ()))?;
        self.strip = strip;
        wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        if let Some(handler) = &self.handler {
            let (id, _) = handler.id();
            let etyp = match self.strip {
                Strip::Error => self.n.typ().diff(&ctx.env, &rtyp)?,
                Strip::Null => NULL_ERR.clone(),
            };
            let etyp =
                if rethrow { etyp } else { wrap!(self, fix_echain_typ(ctx, &etyp))? };
            let bind = ctx.env.by_id.get(&id).ok_or_else(|| anyhow!("BUG: catch"))?;
            match &bind.typ {
                Type::TVar(tv) => {
                    let cell = tv.cell();
                    let mut cell = cell.write();
                    cell.binding = match &cell.binding {
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
            (None, Strip::Error) => QopSink::Log {
                site: cx.interned_str(&diagnostic_site(&self.spec))?,
                unhandled: true,
            },
            (None, Strip::Null) => {
                let e = null_error(&self.spec);
                let msg = unhandled_msg(&diagnostic_site(&self.spec), &e);
                QopSink::UnhandledNull(cx.interned_str(&msg)?)
            }
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
    /// has passed, a standing value is the previous run's answer. `owed`:
    /// a fire arrived while nested catches drained, passed once they have.
    Running {
        generation: (u64, u64),
        passed: bool,
        owed: bool,
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
        let (generation, passed, owed) = match self.state {
            GuardState::Sleeping if self.machine.aborted_in(ctx.rt.cycle()) => {
                self.state = GuardState::Failed;
                return self.resident.set_bottom(true);
            }
            GuardState::Sleeping => {
                self.state = GuardState::Running {
                    generation: current,
                    passed: false,
                    owed: false,
                };
                (current, false, false)
            }
            GuardState::Running { generation, passed, owed } => {
                (generation, passed, owed)
            }
            GuardState::Failed => {
                if self.handler.has_nested_errors() {
                    let _ = self.n.update(ctx, event);
                }
                return self.resident.ride();
            }
        };
        if generation == current || self.handler.has_nested_errors() {
            let value = self.n.update(ctx, event);
            if generation == (self.handler.generation(), self.machine.generation()) {
                if self.handler.has_nested_errors() {
                    if value.is_fired() {
                        self.state =
                            GuardState::Running { generation, passed, owed: true };
                    }
                } else if value.tag().is_bottom() {
                    self.state = GuardState::Running { generation, passed, owed: false };
                    return value;
                } else if value.is_fired() || owed {
                    self.state =
                        GuardState::Running { generation, passed: true, owed: false };
                    if value.is_fired() {
                        return value;
                    }
                    return self
                        .resident
                        .set(TagValue::tagged(value.value_cloned(), Tag::FIRED));
                } else if passed {
                    return value;
                } else {
                    return TagValue::bottom_null(false);
                }
            }
        }
        if generation != (self.handler.generation(), self.machine.generation()) {
            self.state = GuardState::Failed;
        }
        self.resident.set_bottom(true)
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
        fuse(&mut self.n, ctx)?;
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
        Type::BOTTOM
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::SeqAbort(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse(&mut self.n, ctx)?;
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
        strip_production(self.strip, tv, &mut self.resident, |v| {
            // a null is not a failure
            if let Value::Error(e) = v {
                report_ignored!(&diagnostic_site(&self.spec), e)
            }
        })
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
        let (strip, rtyp) = wrap!(self, strip_typ(ctx, '$', self.n.typ()))?;
        self.strip = strip;
        wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))
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
            Strip::Error => QopSink::Log {
                site: cx.interned_str(&diagnostic_site(&self.spec))?,
                unhandled: false,
            },
            Strip::Null => QopSink::DropNull,
        };
        emit_qop_node(cx, self.spec.id, &self.n, &self.typ, sink)
    }
}
