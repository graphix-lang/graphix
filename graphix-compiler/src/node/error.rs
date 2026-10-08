use super::{VarRead, read_var};
use crate::{
    BindId, CFlag, CompileCtx, ErrorHandler, ExecCtx, Node, NodeView, PrintFlag, Refs,
    Rt, Scope, Tag, TagValue, Update, UserEvent, View,
    compiler::compile,
    defetyp, deref_typ,
    env::Env,
    expr::{self, CatchRole, Expr, ExprId, ExprKind, ModPath, WrittenAt},
    format_with_flags,
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr, QopSink, emit_qop_node},
        fuse,
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, opt_node_decode, opt_node_encode, put_tag},
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
use std::{fmt, sync::LazyLock};
use triomphe::Arc;

pub(super) static ECHAIN: LazyLock<ModPath> =
    LazyLock::new(|| ModPath::from(["ErrChain"]));

fn typ_echain(param: Type) -> Type {
    Type::Ref(Arc::new(TypeRef::synthetic(
        ModPath::root(),
        ECHAIN.clone(),
        Arc::from_iter([param]),
    )))
}

/// The fields of `ErrChain<'a>` in a structurally typed world: a struct
/// with exactly these names IS the chain.
fn is_echain_shape(fields: &[(ArcStr, Type, WrittenAt)]) -> bool {
    const NAMES: [&str; 4] = ["cause", "error", "ori", "pos"];
    fields.len() == NAMES.len()
        && NAMES.iter().all(|n| fields.iter().any(|(f, _, _)| f.as_str() == *n))
}

/// A raised value that is a chain already, by its top-level shape alone
/// (the check decides by the same shape, [`fix_echain_typ`]): its
/// `error` field.
fn chain_error(e: &Value) -> Option<Value> {
    const NAMES: [&str; 4] = ["cause", "error", "ori", "pos"];
    let Value::Array(fields) = e else { return None };
    if fields.len() != NAMES.len() {
        return None;
    }
    let mut error = None;
    for (f, name) in fields.iter().zip(NAMES) {
        match f {
            Value::Array(kv)
                if kv.len() == 2 && matches!(&kv[0], Value::String(n) if n == name) =>
            {
                if name == "error" {
                    error = Some(kv[1].clone())
                }
            }
            _ => return None,
        }
    }
    error
}

pub(crate) fn wrap_error(spec: &Expr, e: Value) -> Value {
    let pos: Value =
        [(literal!("column"), spec.pos.column), (literal!("line"), spec.pos.line)].into();
    let (cause, error) = match chain_error(&e) {
        Some(error) => (e, error),
        None => (Value::Null, e),
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
    pub(crate) action: Option<CatchAction<R, E>>,
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
pub(crate) struct CatchAction<R: Rt, E: UserEvent> {
    pub(crate) node: Node<R, E>,
    role: AbortRole<R, E>,
    /// A failure is being collected: the abort stamp of the covering
    /// seq machine when it began ([`ErrorHandler::abort_stamp`]).
    pending: Option<u64>,
}

#[derive(Debug)]
enum AbortRole<R: Rt, E: UserEvent> {
    Machine {
        /// The `abort(..)` event, which a [`SeqAbort`] counted into
        /// the handler's generation before the machine updated: a fired
        /// production runs `node` with no error in flight.
        manual: Option<Node<R, E>>,
        /// The step variable, written idle when the machine sleeps.
        pc: BindId,
    },
    /// A `try`'s capture cell (§7.3): receives the first error of each
    /// failure and the union of this handler's inferred throws.
    Try { capture: BindId },
}

impl<R: Rt, E: UserEvent> CatchAction<R, E> {
    pub(crate) fn manual(&self) -> Option<&Node<R, E>> {
        match &self.role {
            AbortRole::Machine { manual, .. } => manual.as_ref(),
            AbortRole::Try { .. } => None,
        }
    }

    fn manual_mut(&mut self) -> Option<&mut Node<R, E>> {
        match &mut self.role {
            AbortRole::Machine { manual, .. } => manual.as_mut(),
            AbortRole::Try { .. } => None,
        }
    }

    fn capture(&self) -> Option<BindId> {
        match self.role {
            AbortRole::Try { capture } => Some(capture),
            AbortRole::Machine { .. } => None,
        }
    }
}

/// Join `etyp`, an error type raised to `handler`'s catch, into the type
/// its bind infers. A sealed catch takes only what its bind holds.
pub(crate) fn join_raised(env: &Env, handler: &ErrorHandler, etyp: &Type) -> Result<()> {
    let (catch, _) = handler.id();
    let Some(Type::TVar(tv)) = env.by_id.get(&catch).map(|b| &b.typ) else {
        bail!("BUG: catch {catch:?} has no inferred bind")
    };
    let joined = match tv.binding() {
        None => etyp.clone(),
        Some(t)
            if !etyp.has_unbound()
                && t.contains_with_flags(BitFlags::empty(), env, etyp)? =>
        {
            return Ok(());
        }
        Some(t) if handler.sealed() && !etyp.has_unbound() => bail!(
            "this error reaches a catch whose handler was checked to take {t}, \
             which does not contain {etyp}"
        ),
        Some(t) => Type::union(env, &[&t, etyp])?,
    };
    tv.bind(joined);
    Ok(())
}

/// Join an open raised type (a callback's `throws 'e`) into `catch`: two
/// open cells are one (a gate's `'e` and a call's instantiation of it),
/// anything else joins as a union member.
pub(crate) fn join_open_raised(
    env: &Env,
    handler: &ErrorHandler,
    etyp: &Type,
) -> Result<()> {
    let (catch, _) = handler.id();
    let Some(Type::TVar(tv)) = env.by_id.get(&catch).map(|b| &b.typ) else {
        bail!("BUG: catch {catch:?} has no inferred bind")
    };
    match tv.binding() {
        Some(t @ Type::TVar(_))
            if t.deref_cloned().is_none() && t.contains(env, etyp)? =>
        {
            Ok(())
        }
        _ => join_raised(env, handler, etyp),
    }
}

impl<R: Rt, E: UserEvent> Catch<R, E> {
    /// The covering seq machine's abort stamp, 0 outside one.
    fn run_stamp(&self) -> u64 {
        self.own_handler.machine().map_or(0, |m| m.abort_stamp())
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let handler = decode_node(ctx, buf)?;
        let action = match opt_node_decode(ctx, buf)? {
            None => None,
            Some(node) => {
                let role = if bool::decode(buf)? {
                    AbortRole::Machine {
                        manual: opt_node_decode(ctx, buf)?,
                        pc: BindId::decode(buf)?,
                    }
                } else {
                    AbortRole::Try { capture: BindId::decode(buf)? }
                };
                Some(CatchAction { node, role, pending: None })
            }
        };
        let own_handler = image::handler_decode(buf)?;
        let constraint = Option::<Type>::decode(buf)?;
        let thrown = Option::<Type>::decode(buf)?;
        let bind_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(bind_id, top_id);
        Ok(Node::new(Self {
            spec,
            handler,
            action,
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
        ctx: &mut CompileCtx<R, E>,
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
        let covered = scope
            .with_catch((bind_id, top_id), matches!(c.role, CatchRole::Machine { .. }));
        let lookup = |ctx: &CompileCtx<R, E>, name: &ArcStr| {
            let path = ModPath::from([name.as_str()]);
            match ctx.env.lookup_bind(&scope.lexical, &path)? {
                Some((_, b)) => Ok(b.id),
                None => bail!("BUG: seq cell {name} is not bound"),
            }
        };
        let action = match &c.role {
            CatchRole::User => None,
            CatchRole::Machine { action, manual, pc } => Some(CatchAction {
                node: compile(ctx, flags, (**action).clone(), &catch_scope, top_id)?,
                role: AbortRole::Machine {
                    manual: manual
                        .as_ref()
                        .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
                        .transpose()?,
                    pc: lookup(ctx, pc)?,
                },
                pending: None,
            }),
            CatchRole::Try { action, capture } => Some(CatchAction {
                node: compile(ctx, flags, (**action).clone(), &catch_scope, top_id)?,
                role: AbortRole::Try { capture: lookup(ctx, capture)? },
                pending: None,
            }),
        };
        ctx.record_ref(bind_id, top_id);
        let node = Node::new(Self {
            spec,
            handler,
            action,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Catch, buf);
        self.spec.encode(buf)?;
        self.handler.image_encode(buf)?;
        opt_node_encode(self.action.as_ref().map(|a| &a.node), buf)?;
        match self.action.as_ref().map(|a| &a.role) {
            None => (),
            Some(AbortRole::Machine { manual, pc }) => {
                true.encode(buf)?;
                opt_node_encode(manual.as_ref(), buf)?;
                pc.encode(buf)?;
            }
            Some(AbortRole::Try { capture }) => {
                false.encode(buf)?;
                capture.encode(buf)?;
            }
        }
        image::handler_encode(&self.own_handler, buf)?;
        self.constraint.encode(buf)?;
        self.thrown.encode(buf)?;
        self.bind_id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let _ = self.handler.update(ctx);
        let cycle = ctx.rt.cycle();
        let capture = self
            .action
            .as_ref()
            .and_then(|a| a.capture().filter(|_| a.pending.is_none()));
        // a delivery whose raise was given up while asleep is not counted again
        let delivered = match read_var(ctx, &self.bind_id) {
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
            let stamp = self.run_stamp();
            if let Some(abort) = &mut self.action {
                abort.pending.get_or_insert(stamp);
            }
        }
        if let Some(abort) = &mut self.action {
            if let Some(manual) = abort.manual_mut()
                && manual.update(ctx).is_fired()
            {
                self.received = self.received.wrapping_add(1);
                abort.pending.get_or_insert(0);
            }
            if let Some(stamp) = abort.pending
                && self.received == self.own_handler.generation()
                && !self.own_handler.has_nested_errors()
            {
                abort.pending = None;
                // a try's run aborted while its errors drained takes no jump
                let aborted = matches!(abort.role, AbortRole::Try { .. })
                    && self
                        .own_handler
                        .machine()
                        .is_some_and(|m| m.abort_stamp() != stamp);
                if !aborted {
                    ctx.under(View::Birth, |ctx| {
                        let _ = abort.node.update(ctx);
                    });
                }
            }
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.give_up_in_flight();
        ctx.release_var(self.bind_id, self.top_id);
        ctx.env.unbind_variable(self.bind_id);
        self.handler.delete(ctx);
        if let Some(abort) = &mut self.action {
            abort.node.delete(ctx);
            abort.manual_mut().into_iter().for_each(|n| n.delete(ctx));
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.give_up_in_flight();
        self.handler.sleep(ctx);
        if let Some(abort) = &mut self.action {
            abort.node.sleep(ctx);
            abort.manual_mut().into_iter().for_each(|n| n.sleep(ctx));
            abort.pending = None;
            if let AbortRole::Machine { pc, .. } = abort.role {
                ctx.rt.set_var(pc, Value::String(crate::expr::seq::IDLE.clone()));
            }
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
    }

    /// The error bind and the capture take the types their definition's
    /// check left them ([`Catch::aux_types`]); whether `T` covers the
    /// region's throws was judged there.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        match types.aux(self.spec.id) {
            None => self.typecheck0_with(
                ctx,
                &mut |n, ctx| n.typecheck0_instance(ctx, types),
                false,
            ),
            Some(rows) => {
                for (id, row) in self.bind_ids().zip(rows.iter()) {
                    if let Some(Type::TVar(tv)) = ctx.env.by_id.get(&id).map(|b| &b.typ) {
                        tv.bind(row.clone());
                    }
                }
                wrap!(self.handler, self.handler.typecheck0_instance(ctx, types))?;
                let Some(abort) = &mut self.action else { return Ok(()) };
                wrap!(abort.node, abort.node.typecheck0_instance(ctx, types))?;
                match abort.manual_mut() {
                    Some(manual) => wrap!(manual, manual.typecheck0_instance(ctx, types)),
                    None => Ok(()),
                }
            }
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.handler, self.handler.typecheck1(ctx))?;
        if let Some(abort) = &mut self.action {
            wrap!(abort.node, abort.node.typecheck1(ctx))?;
            if let Some(manual) = abort.manual_mut() {
                wrap!(manual, manual.typecheck1(ctx))?;
            }
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
        if let Some(abort) = &self.action {
            abort.node.refs(refs);
            abort.manual().into_iter().for_each(|n| n.refs(refs));
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Catch(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // a catch is a fusion boundary; the handler's own subtrees fuse
        fuse(&mut self.handler, ctx)?;
        if let Some(abort) = &mut self.action {
            fuse(&mut abort.node, ctx)?;
            if let Some(manual) = abort.manual_mut() {
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
        ctx: &CompileCtx<R, E>,
        op: char,
        typ: &Type,
    ) -> Result<Self> {
        // an open operand would take the error form here and fail an
        // instance whose type has neither: its type must be known
        if typ.with_deref(|t| t.is_none()) {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "the operand of {op} has type {typ}, not known here: annotate it \
                     with a type that holds an error or null"
                )
            })?
        }
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
    ctx: &CompileCtx<R, E>,
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
        ExprKind::Qop { arg, written } => {
            format_compact!("{}", written.as_ref().unwrap_or(arg))
        }
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
/// the calling module, so the log tells the engines apart, under
/// [`crate::FAILURE_TARGET`], which an embedder routes (the shell shows
/// it on stderr).
macro_rules! report_failure {
    ($msg:expr) => {{
        let msg: &str = $msg;
        log::error!(target: $crate::FAILURE_TARGET, "{msg}");
    }};
}
pub(crate) use report_failure;

/// A select no arm matched, which exhaustiveness makes a hole in the
/// check: reported as a failure, and under GRAPHIX_ABORT_ON_NO_MATCH (the
/// fuzzer's children) the process aborts, so the hole is a finding.
pub(crate) fn report_coverage_hole(what: &dyn fmt::Display) {
    report_failure!(&format_compact!("{what}: a coverage hole in the check"));
    if crate::dbgenv::graphix_abort_on_no_match() {
        std::process::abort()
    }
}

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
/// `rt.set_var`.
pub(crate) fn deliver_error<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    handler: &ErrorHandler,
    own_top: ExprId,
    spec: &Expr,
    e: Value,
) {
    let (id, handler_top) = handler.id();
    let e = wrap_error(spec, e);
    let v = Value::Error(e.into());
    if handler_top != own_top {
        ctx.rt.set_var(id, v)
    } else {
        if let Err(tv) = ctx.event.variables.try_insert(id, TagValue::fired(v)) {
            ctx.rt.set_var(id, tv.value())
        }
    }
}

/// The error type a `?` delivers for `etyp`: the payload wrapped in an
/// `ErrChain`, once. `wrap_error` chains a caught error rather than
/// nesting it, so a chain arriving from a callee's throws keeps its type.
fn fix_echain_typ<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
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
            // CR claude for eric: [bug] `?` refuses any operand whose error payload is
            // a type variable. `|r: Result<i64, 'e>| -> i64 r?` stops here with `type
            // must be known`, although the type is written out and `r$` checks. A bound
            // that rules out a chain (`'e: [`A, `B]`) and `|e| error(e)?` are refused
            // the same way, so a function that is generic in its error type cannot
            // raise that error. Typing the raise as plain `ErrChain<'e>` would be
            // unsound, because `wrap_error` chains a payload that is itself an ErrChain
            // instead of wrapping it. So either decide the wrap statically for the
            // site, or over-approximate as the primitive arm above does
            // (`ErrChain<Any>`), or refuse with a message that names the generic
            // payload and asks for a concrete error type. Line 619's `expected error
            // not []` is false for the operands that reach it (`x: Any`, `[Any, null]`,
            // a `let r = never()` read by `r?` before its writer) and names nothing to
            // annotate. probe: design/review-2026-10-05/repro/c-error-op-05.gx
            // (c-error-op-05)
            // 2026-10-08 claude: refused with a message that names the payload; the
            // generic raise (deciding the wrap for a variable payload) remains.
            // 2026-10-08 claude: re-addressed, a typing rule: what a `?` over `Result<T,
            // 'e>` raises when 'e is generic. wrap_error chains by the value's shape, so
            // the raise is ErrChain<'e> unless 'e binds to a chain, which is that chain.
            // Options: (a) a bound that keeps chains out of 'e (the raise is then
            // ErrChain<'e>, exact); (b) type the raise ErrChain<Any> (sound, a typed
            // catch loses the payload's type); (c) keep the refusal, which now names the
            // payload. I lean to (a): a generic error type that is itself a chain is the
            // rare case.
            None => format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "? raises {etyp}, whose payload is a type variable: whether it is a \
                     chain already is not known; give the error a concrete type"
                )
            }),
            Some(Type::Ref(tr)) if tr.scope == ModPath::root() && tr.name == *ECHAIN =>
            {
                Ok(etyp.clone())
            }
            Some(Type::Struct(fields)) if is_echain_shape(fields) => Ok(etyp.clone()),
            // a payload chains by the shape of each value's own type, as
            // delivery decides (`chain_error`): the chain members of a
            // union stay, the others wrap together
            Some(et) => {
                let members: LPooled<Vec<Type>> = match expand(&ctx.env, et)? {
                    Type::Set(ref ms) => ms.iter().cloned().collect(),
                    t => [t].into_iter().collect(),
                };
                let mut chains: LPooled<Vec<Type>> = LPooled::take();
                let mut wrapped: LPooled<Vec<Type>> = LPooled::take();
                for m in members.iter() {
                    match is_chain_type(&ctx.env, m)? {
                        true => chains.push(Type::Error(Arc::new(m.clone()))),
                        false => wrapped.push(m.clone()),
                    }
                }
                if wrapped.is_empty() {
                    return Ok(etyp.clone());
                }
                let rest = match &wrapped[..] {
                    [one] => one.clone(),
                    _ => Type::Set(Arc::from_iter(wrapped.drain(..))),
                };
                let wrap = Type::Error(Arc::new(typ_echain(rest)));
                match chains.is_empty() {
                    true => Ok(wrap),
                    false => Ok(Type::Set(Arc::from_iter(chains.drain(..).chain([wrap])))),
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

/// `t` with every alias expanded, through chains of them.
fn expand(env: &Env, t: &Type) -> Result<Type> {
    let mut t = t.clone();
    while let Type::Ref(_) = &t {
        t = t.lookup_ref(env)?;
    }
    Ok(t)
}

/// Whether values of `t` are chains by their shape: the `ErrChain`
/// definition, or a struct with exactly its field names.
fn is_chain_type(env: &Env, t: &Type) -> Result<bool> {
    if let Type::Ref(tr) = t
        && tr.scope == ModPath::root()
        && tr.name == *ECHAIN
    {
        return Ok(true);
    }
    Ok(matches!(expand(env, t)?, Type::Struct(ref fields) if is_echain_shape(fields)))
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
        ctx: &mut ExecCtx<'_, R, E>,
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
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let handler = scope.dynamic.handler();
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

    pub(crate) fn check_unhandled(
        env: &Env,
        flags: BitFlags<CFlag>,
        spec: &Expr,
        raised: impl fmt::Display,
    ) -> Result<()> {
        if flags.contains(CFlag::WarnUnhandled) {
            env.warn(
                flags,
                spec,
                spec.pos,
                spec.end.0,
                format_args!("{raised} will not be caught"),
            )?;
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Qop<R, E> {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.n.update(ctx);
        strip_production(self.strip, tv, &mut self.resident, |v| {
            let e = match v {
                Value::Error(e) => (**e).clone(),
                _ => null_error(&self.spec),
            };
            match &self.handler {
                Some(handler) => {
                    handler.raise();
                    deliver_error(ctx, handler, self.top_id, &self.spec, e);
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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
        emit_qop_node(cx, &self.n, &self.typ, sink)
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
        ctx: &mut ExecCtx<'_, R, E>,
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
        ctx: &mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqGuard, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)?;
        image::handler_encode(&self.handler, buf)?;
        image::handler_encode(&self.machine, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
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
                    let _ = self.n.update(ctx);
                }
                return self.resident.ride();
            }
        };
        if generation == current || self.handler.has_nested_errors() {
            let value = self.n.update(ctx);
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.state = GuardState::Sleeping;
        self.n.sleep(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse(&mut self.n, ctx)?;
        Ok(None)
    }
}

/// A seq's `abort(..)` event, placed before the machine's select: a
/// fired production fails the run in the cycle it fires, so no guard
/// under the machine passes a completion that cycle. A block updates its
/// catches after their covered children, which is too late for that.
#[derive(Debug)]
pub struct SeqAbort<R: Rt, E: UserEvent> {
    spec: Expr,
    pub(crate) n: Node<R, E>,
    machine: ErrorHandler,
}

impl<R: Rt, E: UserEvent> SeqAbort<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let machine = image::handler_decode(buf)?;
        Ok(Node::new(Self { spec, n, machine }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
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

impl<R: Rt, E: UserEvent> Update<R, E> for SeqAbort<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqAbort, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)?;
        image::handler_encode(&self.machine, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        if self.n.update(ctx).is_fired() {
            self.machine.abort(ctx.rt.cycle());
        }
        TagValue::phantom_ref()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.sleep(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
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
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let strip = Strip::decode(buf)?;
        Ok(Node::new(Self { spec, typ, n, strip, resident: TagValue::phantom() }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::OrNever, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)?;
        self.strip.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.n.update(ctx);
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    super::typed_by_row!();

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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
        emit_qop_node(cx, &self.n, &self.typ, sink)
    }
}

impl<R: Rt, E: UserEvent> Catch<R, E> {
    /// The error bind, then a `try` arm's capture cell.
    fn bind_ids(&self) -> impl Iterator<Item = BindId> + use<R, E> {
        let capture = self.action.as_ref().and_then(|a| a.capture());
        std::iter::once(self.bind_id).chain(capture)
    }

    /// What the check left the error binds ([`super::lambda::DefTable`]),
    /// in [`Self::bind_ids`] order.
    pub(crate) fn aux_types(&self, env: &Env) -> Box<[Type]> {
        self.bind_ids()
            .map(|id| env.by_id.get(&id).map_or(Type::Bottom, |b| b.typ.clone()))
            .collect()
    }

    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
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
            tv.bind(t.clone());
            // `T` must cover every error the region throws, judged with
            // the check's settle
            if check {
                let inner = self.thrown.clone().unwrap_or(Type::Bottom);
                let spec = Arc::new(self.spec.clone());
                ctx.pending_settles
                    .last_mut()
                    .expect("settle frame")
                    .push(crate::PendingSettle::Contains { outer: t, inner, spec });
            }
        }
        wrap!(self.handler, child(&mut self.handler, ctx))?;
        // a handler that reads the error was checked at the bind's type; a
        // handler that ignores it (the REPL's tail catch) takes any raise
        let mut refs = Refs::default();
        self.handler.refs(&mut refs);
        if refs.is_refed(self.bind_id) {
            self.own_handler.seal();
        }
        let Some(abort) = &mut self.action else { return Ok(()) };
        wrap!(abort.node, child(&mut abort.node, ctx))?;
        if let Some(manual) = abort.manual_mut() {
            wrap!(manual, child(manual, ctx))?;
        }
        // the capture cell's type is the union of every covering
        // handler's throws
        if let Some(cap) = abort.capture() {
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
}

impl<R: Rt, E: UserEvent> Qop<R, E> {
    /// The strip and the raise joined into the handler's bind are state:
    /// an instance computes them too.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.n, child(&mut self.n, ctx))?;
        let rethrow = matches!(self.spec.kind, ExprKind::Rethrow(_));
        if rethrow {
            if self.n.typ().with_deref(|t| matches!(t, Some(Type::Bottom))) {
                return match check {
                    true => self.typ.check_contains(&ctx.env, &Type::Bottom),
                    false => Ok(()),
                };
            }
        }
        // warned once, at the definition's check: an instance compiles
        // under its call site's handlers, which raise_throws judges
        if check && self.handler.is_none() {
            Self::check_unhandled(&ctx.env, self.flags, &self.spec, "error raised by ?")?;
        }
        let (strip, rtyp) = wrap!(self, strip_typ(ctx, '?', self.n.typ()))?;
        self.strip = strip;
        if check {
            wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        }
        if let Some(handler) = &self.handler {
            let etyp = match self.strip {
                Strip::Error => self.n.typ().diff(&ctx.env, &rtyp)?,
                Strip::Null => NULL_ERR.clone(),
            };
            let etyp =
                if rethrow { etyp } else { wrap!(self, fix_echain_typ(ctx, &etyp))? };
            join_raised(&ctx.env, handler, &etyp)?;
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> SeqGuard<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
    ) -> Result<()> {
        child(&mut self.n, ctx)
    }
}

impl<R: Rt, E: UserEvent> SeqAbort<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
    ) -> Result<()> {
        child(&mut self.n, ctx)
    }
}

impl<R: Rt, E: UserEvent> OrNever<R, E> {
    /// The strip is state: an instance computes it too.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.n, child(&mut self.n, ctx))?;
        let (strip, rtyp) = wrap!(self, strip_typ(ctx, '$', self.n.typ()))?;
        self.strip = strip;
        match check {
            true => wrap!(self, self.typ.check_contains(&ctx.env, &rtyp)),
            false => Ok(()),
        }
    }
}
