use super::{VarRead, read_var};
use crate::{
    BindId, CFlag, CompileCtx, ErrorHandler, ExecCtx, Node, NodeView, PrintFlag, Refs,
    Rt, Scope, Tag, TagValue, Update, UserEvent, View,
    compiler::compile,
    defetyp,
    env::Env,
    expr::{self, CatchRole, Expr, ExprId, ExprKind, ModPath},
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
    prune::BoundaryRef,
    rep,
    typ::Type,
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{fmt, sync::OnceLock};
use triomphe::Arc;

/// The `error` field of a raised chain link.
fn chain_error(e: &Value) -> Option<Value> {
    const NAMES: [&str; 4] = ["cause", "error", "ori", "pos"];
    let fields = rep::struct_fields(e)?;
    if fields.len() != NAMES.len() {
        return None;
    }
    let mut error = None;
    for (f, name) in fields.iter().zip(NAMES) {
        match rep::field(f) {
            Some((n, v)) if n == name => {
                if name == "error" {
                    error = Some(v.clone())
                }
            }
            _ => return None,
        }
    }
    error
}

/// What every raise at one `?` site writes alike, built at its first
/// raise: the chain link's `ori` and `pos`, the site's `NullError`, and
/// whether what it raises is a chain the raise extends.
#[derive(Debug, Default)]
pub(crate) struct SiteValues {
    values: OnceLock<[Value; 3]>,
    extends: OnceLock<bool>,
}

impl SiteValues {
    fn get(&self, spec: &Expr) -> &[Value; 3] {
        self.values.get_or_init(|| {
            let pos = rep::structure([
                (literal!("column"), Value::from(spec.pos.column)),
                (literal!("line"), Value::from(spec.pos.line)),
            ]);
            [spec.ori.to_value(), pos, null_error(spec)]
        })
    }

    pub(crate) fn null_error(&self, spec: &Expr) -> Value {
        self.get(spec)[2].clone()
    }

    /// `e`, a value of `raised`, raised at `spec`: the next link of its
    /// chain when `raised` is chains, else the first link of a new one.
    fn wrap_error(&self, env: &Env, raised: &Type, spec: &Expr, e: Value) -> Value {
        let [ori, pos, _] = self.get(spec);
        let extends = *self.extends.get_or_init(|| match raised_chain(env, raised) {
            Ok(chain) => chain.is_some(),
            Err(err) => {
                report_failure!(&format_compact!("{}: {err}", diagnostic_site(spec)));
                false
            }
        });
        let (cause, error) = match extends.then(|| chain_error(&e)).flatten() {
            Some(error) => (e, error),
            None => (Value::Null, e),
        };
        rep::structure([
            (literal!("cause"), cause),
            (literal!("error"), error),
            (literal!("ori"), ori.clone()),
            (literal!("pos"), pos.clone()),
        ])
    }
}

#[derive(Debug)]
pub struct Catch<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub handler: Node<R, E>,
    pub(crate) action: Option<CatchAction<R, E>>,
    own_handler: ErrorHandler,
    receipts: Receipts,
    constraint: Option<Type>,
    /// Throws unioned into the bind before `catch(e: T)` ascribes `T`
    /// onto it. Coverage is this snapshot, not the bind after ascription.
    thrown: Option<Type>,
    bind_id: BindId,
    top_id: ExprId,
    /// The boundary this catch polls its handler from.
    polled: Option<BoundaryRef>,
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
        /// A `seqq`'s queue clock, and whether a sleep aborted a run
        /// whose credit it owes at the wake.
        clock: Option<(BindId, bool)>,
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
        Some(t)
            if handler.sealed()
                && !etyp.has_unbound()
                && t.with_deref(|t| matches!(t, None | Some(Type::Bottom))) =>
        {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "this error reaches a catch that saw no error when it was checked: \
                     annotate the catch with the errors it takes, `catch(e: {etyp})`"
                )
            })?
        }
        Some(t) if handler.sealed() && !etyp.has_unbound() => bail!(
            "this error reaches a catch whose handler was checked to take {t}: \
             {t} does not contain {etyp}"
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
        let spec = Arc::new(Expr::decode(buf)?);
        let handler = decode_node(ctx, buf)?;
        let action = match opt_node_decode(ctx, buf)? {
            None => None,
            Some(node) => {
                let role = if bool::decode(buf)? {
                    AbortRole::Machine {
                        manual: opt_node_decode(ctx, buf)?,
                        pc: BindId::decode(buf)?,
                        clock: Option::<BindId>::decode(buf)?.map(|c| (c, false)),
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
        let polled = ctx.boundary.clone();
        own_handler.polled_by(&polled);
        Ok(Node::new(Self {
            spec,
            handler,
            action,
            own_handler,
            receipts: Receipts::default(),
            constraint,
            thrown,
            bind_id,
            top_id,
            polled,
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        c: &Arc<expr::CatchExpr>,
    ) -> Result<(Node<R, E>, Scope)> {
        let catch_scope = scope.append_block("ca", spec.id.inner());
        // an ascribed catch takes its type now, so a raise it covers writes
        // nothing, from whatever task checks the raise
        let typ = Type::empty_tvar();
        match &typ {
            Type::TVar(tv) => {
                tv.freeze();
                tv.bind(c.constraint.clone().unwrap_or(Type::Bottom))
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
            CatchRole::Machine { action, manual, pc, clock } => Some(CatchAction {
                node: compile(ctx, flags, (**action).clone(), &catch_scope, top_id)?,
                role: AbortRole::Machine {
                    manual: manual
                        .as_ref()
                        .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
                        .transpose()?,
                    pc: lookup(ctx, pc)?,
                    clock: clock
                        .as_ref()
                        .map(|c| lookup(ctx, c))
                        .transpose()?
                        .map(|c| (c, false)),
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
        let own_handler = covered.dynamic.handler().unwrap();
        if c.constraint.is_some() {
            own_handler.seal();
        }
        let polled = ctx.boundary.clone();
        own_handler.polled_by(&polled);
        let node = Node::new(Self {
            spec,
            handler,
            action,
            own_handler,
            receipts: Receipts::default(),
            constraint: c.constraint.clone(),
            thrown: None,
            bind_id,
            top_id,
            polled,
        });
        Ok((node, covered))
    }

    fn give_up_in_flight(&mut self, cycle: u64) {
        self.receipts.give_up(&self.own_handler, cycle)
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Catch<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.handler);
        if let Some(abort) = &self.action {
            f(&abort.node);
            abort.manual().into_iter().for_each(f)
        }
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.handler);
        if let Some(abort) = &mut self.action {
            f(&mut abort.node);
            abort.manual_mut().into_iter().for_each(f)
        }
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Catch, buf);
        self.spec.encode(buf)?;
        self.handler.image_encode(buf)?;
        opt_node_encode(self.action.as_ref().map(|a| &a.node), buf)?;
        match self.action.as_ref().map(|a| &a.role) {
            None => (),
            Some(AbortRole::Machine { manual, pc, clock }) => {
                true.encode(buf)?;
                opt_node_encode(manual.as_ref(), buf)?;
                pc.encode(buf)?;
                clock.map(|(c, _)| c).encode(buf)?;
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
        let capture = self
            .action
            .as_ref()
            .and_then(|a| a.capture().filter(|_| a.pending.is_none()));
        if let Some(v) = self.receipts.take(ctx, &self.own_handler) {
            if let Some(cap) = capture {
                ctx.rt.set_var(cap, v);
            }
            let stamp = self.run_stamp();
            if let Some(abort) = &mut self.action {
                abort.pending.get_or_insert(stamp);
            }
        }
        if let Some(abort) = &mut self.action {
            if let AbortRole::Machine { clock: Some((clock, owed)), .. } = &mut abort.role
                && std::mem::take(owed)
                && !ctx.event.born()
            {
                ctx.rt.set_var(*clock, Value::Bool(true));
            }
            if let Some(manual) = abort.manual_mut()
                && manual.update(ctx).is_fired()
            {
                abort.pending.get_or_insert(0);
            }
            if let Some(stamp) = abort.pending
                && self.receipts.settled(&self.own_handler)
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
        self.give_up_in_flight(ctx.rt.cycle());
        self.own_handler.unpolled_by(&self.polled);
        ctx.release_var(self.bind_id, self.top_id);
        ctx.env.unbind_variable(self.bind_id);
        self.handler.delete(ctx);
        if let Some(abort) = &mut self.action {
            abort.node.delete(ctx);
            abort.manual_mut().into_iter().for_each(|n| n.delete(ctx));
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.give_up_in_flight(ctx.rt.cycle());
        self.handler.sleep(ctx);
        if let Some(abort) = &mut self.action {
            abort.node.sleep(ctx);
            abort.manual_mut().into_iter().for_each(|n| n.sleep(ctx));
            abort.pending = None;
            if let AbortRole::Machine { pc, clock, .. } = &mut abort.role {
                let idle = crate::rep::make_variant(crate::expr::seq::IDLE.clone(), []);
                let running = ctx
                    .rt
                    .store_get(pc)
                    .is_some_and(|(tv, _)| tv.with_value(|v| *v != idle));
                if let Some((_, owed)) = clock {
                    *owed |= running;
                }
                ctx.rt.set_var(*pc, idle);
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
        // what `?` raises is the operand's error part, which Any does not say
        let any = |t: &Type| matches!(t, Type::Any);
        // CR claude for claude: [readability/low] with_deref does not expand a
        // Type::Ref, so `x?` over `[A, Error<`E>]` with `type A = Any` misses this
        // test. It is then refused by fix_echain_typ as "expected error not []" (the
        // diff through A left no error), where `[Any, Error<`E>]` written out gets the
        // cast-or-use-$ message. Test through refs (the Type::holds walk holds_abstract
        // uses) so both reach this refusal. probe:
        // design/review-2026-10-26/repro/c-error-op-any-typedef-qop-msg.gx
        // (c-error-op-04)
        let holds_any = typ.with_deref(|t| match t {
            Some(Type::Set(ts)) => ts.iter().any(any),
            Some(t) => any(t),
            None => false,
        });
        if op == '?' && holds_any {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "the operand of ? has type {typ}, so the error it raises is not \
                     known: cast it to a type whose error is (a Result), or use $"
                )
            })?
        }
        let err = Type::Error(Arc::new(Type::empty_tvar()));
        let null = Type::Primitive(Typ::Null.into());
        let holds = |t: &Type| typ.contains_with_flags(BitFlags::empty(), &ctx.env, t);
        // an open member holds both: the strip is the one that takes a
        // member out, the error form when neither does (`Any`)
        let takes = |strip: Self| -> Result<bool> {
            let rest = typ.diff(&ctx.env, &strip.removed())?;
            Ok(!typ.diff(&ctx.env, &rest)?.is_uninhabited())
        };
        if holds(&err)? && holds(&null)? && !takes(Self::Error)? && takes(Self::Null)? {
            Ok(Self::Null)
        } else if holds(&err)? {
            Ok(Self::Error)
        } else if holds(&null)? {
            Ok(Self::Null)
        } else {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "cannot use the {op} operator on {typ}, it has no error and no null"
                )
            })
        }
    }

    /// The strip a definition's check recorded as its removed type.
    fn of_removed(t: &Type) -> Option<Self> {
        [Self::Error, Self::Null].into_iter().find(|s| s.removed() == *t)
    }

    pub(crate) fn removed(self) -> Type {
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
/// no pattern could match. An instance strips what its definition
/// decided, whatever its operand became.
fn strip_typ<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    op: char,
    operand: &Type,
    decided: Option<Strip>,
) -> Result<(Strip, Type)> {
    let strip = match decided {
        Some(strip) => strip,
        None => Strip::of(ctx, op, operand)?,
    };
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
    // the bottom keeps the input's tag
    resident.set_bottom_as(tv.tag())
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
    rep::make_variant(NULL_ERR_TAG.clone(), [Value::from(operand)])
}

/// Where a swallowed error or a failed operator reports itself, in both
/// engines: the origin (`in file ..`), then the position.
pub fn diagnostic_site(spec: &Expr) -> ArcStr {
    format_compact!("{} at {}", spec.ori, spec.pos).as_str().into()
}

/// What a handler-less `?` at `site` reports for the error payload `e`.
pub(crate) fn unhandled_msg(site: &str, e: &Value) -> ArcStr {
    format_compact!("unhandled error {site} {}", crate::typ::Naked(e)).as_str().into()
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
        log::warn!("ignored error {} {}", $site, $crate::typ::Naked($e))
    };
}
pub(crate) use report_ignored;

/// Deliver a `?`'s raw error payload `e`, a value of `raised`, to
/// `handler` on behalf of the `?` at `spec` under `own_top`. The one handler path, shared by
/// `Qop::update` and the fused kernel's delivery drain: same-top
/// deliveries land in this cycle's event, cross-top ones go through
/// `rt.set_var`.
pub(crate) fn deliver_error<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    handler: &ErrorHandler,
    own_top: ExprId,
    spec: &Expr,
    raised: &Type,
    values: &SiteValues,
    e: Value,
) {
    let (id, handler_top) = handler.id();
    let e = values.wrap_error(&ctx.env, raised, spec, e);
    let v = Value::Error(e.into());
    if handler_top != own_top {
        ctx.rt.set_var(id, v)
    } else {
        if let Err(tv) = ctx.try_publish(id, TagValue::fired(v)) {
            ctx.rt.set_var(id, tv.value())
        }
    }
}

/// The error type a `?` delivers for `etyp`: what it raises wrapped in
/// an `ErrChain`, once. A raise of chains extends them rather than
/// nesting them (`wrap_error`), so a chain arriving from a callee's
/// throws keeps its type.
fn fix_echain_typ<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    etyp: &Type,
) -> Result<Type> {
    let Some(payload) = raised_payload(&ctx.env, etyp)? else {
        return format_with_flags(PrintFlag::DerefTVars, || {
            bail!("expected error not {etyp}")
        });
    };
    let chain = match payload.chain_error_type(&ctx.env)? {
        Some(error) => Type::err_chain(error),
        None => Type::err_chain(payload),
    };
    Ok(Type::Error(Arc::new(chain)))
}

/// What the errors of `t` carry, the union of their payloads, or `None`
/// when `t` holds no error.
fn raised_payload(env: &Env, t: &Type) -> Result<Option<Type>> {
    fn walk(env: &Env, t: &Type, payloads: &mut Vec<Type>) -> Result<()> {
        t.with_deref(|t| match t {
            None => Ok(()),
            Some(t @ Type::Ref(_)) => walk(env, &t.lookup_ref_peek(env)?, payloads),
            Some(Type::Set(ts)) => ts.iter().try_for_each(|t| walk(env, t, payloads)),
            Some(Type::Error(payload)) => Ok(payloads.push(
                payload.with_deref(|t| t.cloned()).unwrap_or_else(|| (**payload).clone()),
            )),
            Some(Type::Primitive(p)) if p.contains(Typ::Error) => {
                Ok(payloads.push(Type::Any))
            }
            Some(_) => Ok(()),
        })
    }
    let mut payloads: LPooled<Vec<Type>> = LPooled::take();
    walk(env, t, &mut payloads)?;
    Ok(match payloads.len() {
        0 => None,
        1 => payloads.pop(),
        _ => Some(Type::Set(Arc::from_iter(payloads.drain(..)))),
    })
}

/// The error type of the chains `raised` raises when it raises nothing
/// else.
fn raised_chain(env: &Env, raised: &Type) -> Result<Option<Type>> {
    match raised_payload(env, raised)? {
        Some(payload) => payload.chain_error_type(env),
        None => Ok(None),
    }
}

/// A fused handler-ful `?` site: what the kernel's delivery drain needs
/// to run [`deliver_error`] for an error raised there.
#[derive(Debug)]
pub struct QopSite {
    pub(crate) handler: ErrorHandler,
    pub own_top: ExprId,
    pub spec: Arc<Expr>,
    /// What the site raises: the operand's type, or `NullError`'s.
    pub raised: Type,
    pub(crate) values: SiteValues,
}

/// Where a `?` or `$` sends what it strips.
#[derive(Debug)]
pub(crate) enum QopMode {
    /// `?`: to the handler covering it, else logged as unhandled.
    Raise {
        handler: Option<ErrorHandler>,
        top_id: ExprId,
        flags: BitFlags<CFlag>,
        values: SiteValues,
    },
    /// `$`: an error logged as ignored, a null dropped.
    Ignore,
}

impl QopMode {
    fn decode(buf: &mut &[u8]) -> Result<Self, PackError> {
        if !bool::decode(buf)? {
            return Ok(Self::Ignore);
        }
        let handler = match bool::decode(buf)? {
            true => Some(image::handler_decode(buf)?),
            false => None,
        };
        let top_id = ExprId::decode(buf)?;
        let flags = image::flags_decode(buf)?;
        Ok(Self::Raise { handler, top_id, flags, values: SiteValues::default() })
    }

    fn encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let Self::Raise { handler, top_id, flags, .. } = self else {
            return false.encode(buf);
        };
        true.encode(buf)?;
        handler.is_some().encode(buf)?;
        if let Some(h) = handler {
            image::handler_encode(h, buf)?;
        }
        top_id.encode(buf)?;
        image::flags_encode(*flags, buf)
    }
}

/// `?` and `$`: the operand less what `strip` takes, which goes where
/// `mode` says.
#[derive(Debug)]
pub struct Qop<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Node<R, E>,
    pub(crate) strip: Strip,
    pub(crate) mode: QopMode,
    /// What a raise carries ([`Self::raised_typ`]).
    raised: Type,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Qop<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let strip = Strip::decode(buf)?;
        let mode = QopMode::decode(buf)?;
        let raised = Self::raised_typ(strip, &n);
        Ok(Node::new(Self {
            spec,
            typ,
            n,
            strip,
            mode,
            raised,
            resident: TagValue::phantom(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let mode = match spec.kind {
            ExprKind::OrNever(_) => QopMode::Ignore,
            _ => QopMode::Raise {
                handler: scope.dynamic.handler(),
                top_id,
                flags,
                values: SiteValues::default(),
            },
        };
        let typ = Type::empty_tvar();
        let strip = Strip::Error;
        let raised = Self::raised_typ(strip, &n);
        Ok(Node::new(Self {
            spec,
            typ,
            n,
            strip,
            mode,
            raised,
            resident: TagValue::phantom(),
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

    /// Whether a stripped value is raised (`?`), not ignored (`$`).
    pub(crate) fn raises(&self) -> bool {
        matches!(self.mode, QopMode::Raise { .. })
    }

    /// The handler a raise here goes to, an event, not the log.
    pub(crate) fn handler(&self) -> Option<&ErrorHandler> {
        match &self.mode {
            QopMode::Raise { handler, .. } => handler.as_ref(),
            QopMode::Ignore => None,
        }
    }

    /// The type of what a raise here carries.
    fn raised_typ(strip: Strip, n: &Node<R, E>) -> Type {
        match strip {
            Strip::Error => n.typ().clone(),
            Strip::Null => NULL_ERR.clone(),
        }
    }

    /// The handler a raise here goes to and the top this node lives under.
    pub(crate) fn raise_site(&self) -> Option<(&ErrorHandler, ExprId)> {
        match &self.mode {
            QopMode::Raise { handler: Some(h), top_id, .. } => Some((h, *top_id)),
            _ => None,
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Qop<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Qop, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)?;
        self.strip.encode(buf)?;
        self.mode.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.n.update(ctx);
        let (spec, mode, raised) = (&self.spec, &self.mode, &self.raised);
        strip_production(self.strip, tv, &mut self.resident, |v| match mode {
            QopMode::Raise { handler, top_id, values, .. } => {
                let e = match v {
                    Value::Error(e) => (**e).clone(),
                    _ => values.null_error(spec),
                };
                match handler {
                    Some(handler) => {
                        handler.raise(ctx.rt.cycle());
                        deliver_error(ctx, handler, *top_id, spec, raised, values, e);
                    }
                    None => report_failure!(&unhandled_msg(&diagnostic_site(spec), &e)),
                }
            }
            QopMode::Ignore => {
                // a null is not a failure
                if let Value::Error(e) = v {
                    report_ignored!(&diagnostic_site(spec), e)
                }
            }
        })
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true, None)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        let row = types.settle(self.spec.id, &self.typ);
        let decided = types.aux(self.spec.id).and_then(|r| Strip::of_removed(&r[0]));
        self.typecheck0_with(
            ctx,
            &mut |n, ctx| n.typecheck0_instance(ctx, types),
            !row,
            decided,
        )
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Qop(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        let sink = match (&self.mode, self.strip) {
            (QopMode::Ignore, Strip::Error) => QopSink::Log {
                site: cx.interned_str(&diagnostic_site(&self.spec))?,
                unhandled: false,
            },
            (QopMode::Ignore, Strip::Null) => QopSink::DropNull,
            (QopMode::Raise { handler: None, .. }, Strip::Error) => QopSink::Log {
                site: cx.interned_str(&diagnostic_site(&self.spec))?,
                unhandled: true,
            },
            (QopMode::Raise { handler: None, .. }, Strip::Null) => {
                let e = null_error(&self.spec);
                let msg = unhandled_msg(&diagnostic_site(&self.spec), &e);
                QopSink::UnhandledNull(cx.interned_str(&msg)?)
            }
            (QopMode::Raise { handler: Some(handler), top_id, .. }, strip) => {
                let site = cx.interned_qop_site(QopSite {
                    handler: handler.clone(),
                    own_top: *top_id,
                    spec: self.spec.clone(),
                    raised: self.raised.clone(),
                    values: SiteValues::default(),
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
    spec: Arc<Expr>,
    pub(crate) n: Node<R, E>,
    handler: ErrorHandler,
    /// The machine's handler: an `abort(..)` fails the run there, and a
    /// try-body arm's nearest handler is its jump.
    machine: ErrorHandler,
    state: GuardState,
    resident: TagValue,
    /// The boundary this guard polls its handlers from.
    polled: Option<BoundaryRef>,
}

impl<R: Rt, E: UserEvent> SeqGuard<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let n = decode_node(ctx, buf)?;
        let handler = image::handler_decode(buf)?;
        let machine = image::handler_decode(buf)?;
        let polled = ctx.boundary.clone();
        handler.polled_by(&polled);
        Ok(Node::new(Self {
            spec,
            n,
            handler,
            machine,
            state: GuardState::Sleeping,
            resident: TagValue::phantom(),
            polled,
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R, E>> {
        let handler =
            scope.dynamic.handler().ok_or_else(|| anyhow!("BUG: seq handler"))?;
        let machine = handler.machine().ok_or_else(|| anyhow!("BUG: seq machine"))?;
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let polled = ctx.boundary.clone();
        handler.polled_by(&polled);
        Ok(Node::new(Self {
            spec,
            n,
            handler,
            machine,
            state: GuardState::Sleeping,
            resident: TagValue::phantom(),
            polled,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for SeqGuard<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.handler.unpolled_by(&self.polled);
        self.n.delete(ctx)
    }

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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.state = GuardState::Sleeping;
        self.n.sleep(ctx);
    }

    super::typed_by_row!(children);

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        self.n.typ()
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
    spec: Arc<Expr>,
    pub(crate) n: Node<R, E>,
    machine: ErrorHandler,
}

impl<R: Rt, E: UserEvent> SeqAbort<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let n = decode_node(ctx, buf)?;
        let machine = image::handler_decode(buf)?;
        Ok(Node::new(Self { spec, n, machine }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

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

    super::typed_by_row!(children);

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::SeqAbort(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse(&mut self.n, ctx)?;
        Ok(None)
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
        // CR claude for eric: [bug/medium] An open raise (r? over Result<i64, 'e>, or a
        // callback's throws 'e) into an ascribed catch is judged only by the Contains
        // pushed below. That push ignores def_gate_depth, unlike defer_settle, so it
        // drains at the statement's typecheck1 and narrows the definition's 'e only
        // after a later statement's call has been checked. `let run = |r: Result<i64,
        // 'e>| { catch(e: Error<ErrChain<`A>>) ..; r? + 1 }; run(error(`B))` passes
        // --check, and elaboration refuses the instance (GRAPHIX_ELAB_AUDIT reports
        // it). Judge open raises into a sealed catch where they join (T.check_contains
        // in join_raised, which also lets `thrown` and this Contains go), or inside the
        // definition's own settle ahead of PendingSettle::Body. Whether a rigid
        // declared 'e is then refused ("declare the bound") or narrowed is your call.
        // The `thrown` doc is stale either way: Catch::compile binds T, so the snapshot
        // is T plus open raises. probe:
        // design/review-2026-10-26/repro/c-error-op-ascribed-catch-open-throws.gx
        // (c-error-op-01)
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
                let spec = self.spec.clone();
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
    /// The raise joined into the handler's bind is state: an instance
    /// computes it too, over the strip its definition `decided`.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
        decided: Option<Strip>,
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
        if check && let QopMode::Raise { handler: None, flags, .. } = &self.mode {
            Self::check_unhandled(&ctx.env, *flags, &self.spec, "error raised by ?")?;
        }
        let op = if self.raises() { '?' } else { '$' };
        let (strip, rtyp) = wrap!(self, strip_typ(ctx, op, self.n.typ(), decided))?;
        self.strip = strip;
        self.raised = Self::raised_typ(strip, &self.n);
        if check {
            wrap!(self, self.typ.check_contains(&ctx.env, &rtyp))?;
        }
        if let Some(handler) = self.handler() {
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

/// A catch a builtin puts over a function it calls (a request handler):
/// it tells the builtin when the function raised, and passes each error
/// on to the handler covering the builtin, or logs it where none does.
#[derive(Debug)]
pub struct ErrorRelay {
    handler: ErrorHandler,
    receipts: Receipts,
}

/// The receipt of a handler's raises by the catch that reads them: each
/// raise's error arrives in raise order, one a cycle, and is taken once;
/// one whose raise was given up (the catch asleep or gone) is passed over
/// when it arrives. Aborts are not raises.
#[derive(Debug, Default)]
struct Receipts {
    /// Deliveries arrived, taken or passed over.
    arrived: u64,
    /// Raises acknowledged to the enclosing handlers: taken, or given up.
    acked: u64,
    /// The cycle a delivery arrived in: a later raise that cycle arrives
    /// the next.
    last_cycle: Option<u64>,
}

impl Receipts {
    /// This cycle's error, when one of `handler`'s raises arrived and was
    /// not given up.
    fn take<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &ExecCtx<'_, R, E>,
        handler: &ErrorHandler,
    ) -> Option<Value> {
        let (bind_id, _) = handler.id();
        let cycle = ctx.rt.cycle();
        // a catch updated twice in one cycle would read its delivery twice
        let v = match read_var(ctx, &bind_id) {
            Some(VarRead::Delivered(tv))
                if self.last_cycle != Some(cycle)
                    && tv.tag().is_fired()
                    && self.arrived < handler.raised() =>
            {
                tv.value_cloned()
            }
            _ => return None,
        };
        self.last_cycle = Some(cycle);
        self.arrived += 1;
        if self.arrived <= self.acked {
            return None;
        }
        self.acked += 1;
        handler.handled(cycle);
        Some(v)
    }

    /// Acknowledge every raise not taken yet, so no enclosing handler
    /// waits on them; their deliveries are passed over.
    // CR claude for eric: [bug/high] give_up acks raises whose deliveries have not
    // landed yet. A delivery that then lands while the catch is asleep is never seen by
    // take, so `arrived` lags `acked` after the wake. The next real raises are then
    // passed over as given up, and handled() is never called for them. The ancestors'
    // nested count stays above zero, and a seq whose step called into such a catch
    // never passes its step (SeqGuard sees has_nested_errors() forever). The user
    // handler still runs on those errors through wake catch-up, so the handler and
    // Receipts disagree about what was given up. Count deliveries where they land (for
    // example, have raise() return an ordinal that deliver_error records on the
    // DynNode, and let take compare it with acked). Also decide whether a catch hears
    // at its wake the errors delivered while it slept, and make the handler and
    // Receipts agree. probe:
    // design/review-2026-10-19/repro/c-error-op-receipts-sleep-stall.gx (c-error-op-01)
    // 2026-10-23 claude: Readdressed to claude in the 2026-10-23 triage: no ruling
    // needed. Take the counting fix; the wake question follows wake catch-up: a catch
    // hears, once, the deliveries that landed while it slept, and Receipts agree.
    // 2026-10-25 claude: Tried 2026-10-25, needs a decision before code. A delivery
    // reaches the catch through its error variable, one a cycle, and lands in the
    // runtime with no hook that knows the handler; a sleeping catch reads none of them,
    // and at its wake catch-up re-raises them as one fire at the latest value. So
    // Receipts cannot count landings by observing them, and an ordinal recorded at
    // raise or at deliver_error is issue order, not landing. The triage's 'hears, once,
    // at its wake, and Receipts agree' leaves a choice: (a) a sleeping catch no longer
    // gives its in-flight raises up: the handler hears them at the wake and Receipts
    // ack them then, but an enclosing seq step waits on them for as long as the catch
    // sleeps, forever if its arm never wakes; or (b) give_up stays at sleep (ancestors
    // never wait) and the one catch-up fire at the wake counts as every given-up
    // delivery that landed (arrived catches up to acked), the handler's run on it
    // passed over; Receipts then agree, but the handler does not hear those errors.
    // Readdressed to eric for the choice.
    fn give_up(&mut self, handler: &ErrorHandler, cycle: u64) {
        while self.acked < handler.raised() {
            self.acked += 1;
            handler.handled(cycle);
        }
    }

    /// Every raise to `handler` is acknowledged.
    fn settled(&self, handler: &ErrorHandler) -> bool {
        self.acked == handler.raised()
    }
}

impl ErrorRelay {
    /// The relay, and the scope the function it covers compiles under.
    pub fn new<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        top_id: ExprId,
    ) -> (Self, Scope) {
        let typ = Type::empty_tvar();
        if let Type::TVar(tv) = &typ {
            tv.freeze();
            tv.bind(Type::Bottom);
        }
        let lexical = scope.append_block("ca", top_id.inner()).lexical;
        let bind_id = ctx
            .env
            .bind_variable(
                &lexical,
                "e",
                typ,
                Default::default(),
                crate::node::genn::SYNTHETIC.clone(),
            )
            .id;
        ctx.record_ref(bind_id, top_id);
        let covered = scope.with_catch((bind_id, top_id), false);
        let handler = covered.dynamic.handler().unwrap();
        (Self { handler, receipts: Receipts::default() }, covered)
    }

    /// The error the function raised this cycle, which also goes on.
    pub fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
    ) -> Option<Value> {
        let e = self.receipts.take(ctx, &self.handler)?;
        match self.handler.parent().handler() {
            Some(parent) => {
                parent.raise(ctx.rt.cycle());
                let (id, _) = parent.id();
                ctx.rt.set_var(id, e.clone())
            }
            None => report_failure!(&format_compact!(
                "unhandled error {}",
                crate::typ::Naked(&e)
            )),
        }
        Some(e)
    }

    /// Raises in flight when the function sleeps or is deleted are
    /// acknowledged, so no enclosing handler waits on them.
    pub fn give_up_in_flight(&mut self, cycle: u64) {
        self.receipts.give_up(&self.handler, cycle)
    }

    pub fn delete<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.give_up_in_flight(ctx.rt.cycle());
        let (bind_id, top_id) = self.handler.id();
        ctx.release_var(bind_id, top_id);
        ctx.env.unbind_variable(bind_id);
    }

    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        image::handler_encode(&self.handler, buf)
    }

    pub fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let handler = image::handler_decode(buf)?;
        let (bind_id, top_id) = handler.id();
        ctx.record_ref(bind_id, top_id);
        Ok(Self { handler, receipts: Receipts::default() })
    }
}
