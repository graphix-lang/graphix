use super::{VarRead, read_var, wake::FreshFrame};
use crate::{
    BindId, CFlag, CompileCtx, ErrorHandler, ExecCtx, HandlerKind, Node, NodeView,
    Notify, PrintFlag, Refs, Rt, Scope, SourcePosition, Tag, TagValue, Update, View,
    bailat,
    compiler::compile,
    defetyp,
    env::{Bind, Env},
    expr::{self, At, CatchRole, Expr, ExprId, ExprKind, ModPath, Origin},
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
    typ::{Type, bounds},
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
pub struct Catch<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub handler: Node<R>,
    pub(crate) action: Option<CatchAction<R>>,
    own_handler: ErrorHandler,
    receipts: Receipts,
    unused_warning: Option<BitFlags<CFlag>>,
    bind_id: BindId,
    top_id: ExprId,
    /// The boundary this catch polls its handler from.
    polled: Option<BoundaryRef>,
}

/// A seq machine's handler, or a `try` body arm's jump: the action run
/// once every error of a failure has arrived.
#[derive(Debug)]
pub(crate) struct CatchAction<R: Rt> {
    pub(crate) node: Node<R>,
    role: AbortRole<R>,
    /// A failure is being collected: the abort stamp of the covering
    /// seq machine when it began ([`ErrorHandler::abort_stamp`]).
    pending: Option<u64>,
}

#[derive(Debug)]
enum AbortRole<R: Rt> {
    Machine {
        /// The `abort(..)` event, which a [`SeqAbort`] counted into
        /// the handler's generation before the machine updated: a fired
        /// production runs `node` with no error in flight.
        manual: Option<Node<R>>,
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

impl<R: Rt> CatchAction<R> {
    fn with(node: Node<R>, role: AbortRole<R>) -> Self {
        Self { node, role, pending: None }
    }

    pub(crate) fn manual(&self) -> Option<&Node<R>> {
        match &self.role {
            AbortRole::Machine { manual, .. } => manual.as_ref(),
            AbortRole::Try { .. } => None,
        }
    }

    fn manual_mut(&mut self) -> Option<&mut Node<R>> {
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

impl<R: Rt> AbortRole<R> {
    fn machine(manual: Option<Node<R>>, pc: BindId, clock: Option<BindId>) -> Self {
        AbortRole::Machine { manual, pc, clock: clock.map(|c| (c, false)) }
    }
}

/// A catch nothing reads, installed over a compile to collect what it
/// raises: each raise joins the type its bind infers.
pub(crate) struct RaiseProbe(BindId);

impl RaiseProbe {
    /// The probe, and `scope` with it installed as the innermost handler.
    pub(crate) fn open<R: Rt>(ctx: &mut CompileCtx<R>, scope: &Scope) -> (Self, Scope) {
        let id = BindId::new();
        ctx.env.by_id.insert(
            id,
            Arc::new(Bind {
                doc: None,
                id,
                name: "#faux".into(),
                scope: scope.lexical.clone(),
                typ: Type::empty_tvar(),
                pos: SourcePosition::default(),
                ori: Arc::new(Origin::default()),
                binder: None,
            }),
        );
        (Self(id), scope.with_catch((id, ExprId::new()), HandlerKind::Catch))
    }

    /// What was raised: the cell the raises joined into.
    pub(crate) fn raised<R: Rt>(&self, ctx: &CompileCtx<R>) -> Type {
        ctx.env.by_id.get(&self.0).map_or(Type::Bottom, |b| b.typ.clone())
    }

    pub(crate) fn close<R: Rt>(&self, ctx: &mut CompileCtx<R>) {
        ctx.env.by_id.remove(&self.0);
    }
}

/// `etyp`, an error type raised to `handler`'s catch, flows into the
/// type its bind infers.
pub(crate) fn join_raised(env: &Env, handler: &ErrorHandler, etyp: &Type) -> Result<()> {
    // a raise of nothing joins nothing
    if etyp.with_deref(|t| matches!(t, Some(Type::Bottom))) {
        return Ok(());
    }
    let (catch, _) = handler.id();
    let Some(Type::TVar(tv)) = env.by_id.get(&catch).map(|b| &b.typ) else {
        bail!("BUG: catch {catch:?} has no inferred bind")
    };
    Type::TVar(tv.clone()).check_contains(env, etyp)
}

impl<R: Rt> Catch<R> {
    fn with(
        spec: Arc<Expr>,
        handler: Node<R>,
        action: Option<CatchAction<R>>,
        own_handler: ErrorHandler,
        unused_warning: Option<BitFlags<CFlag>>,
        bind_id: BindId,
        top_id: ExprId,
        polled: Option<BoundaryRef>,
    ) -> Node<R> {
        Node::new(Self {
            spec,
            handler,
            action,
            own_handler,
            receipts: Receipts::default(),
            unused_warning,
            bind_id,
            top_id,
            polled,
        })
    }

    /// The covering seq machine's abort stamp, 0 outside one.
    fn run_stamp(&self) -> u64 {
        self.own_handler.machine().map_or(0, |m| m.abort_stamp())
    }

    /// The errors raised under the catch flow into its binding, which
    /// holds at most what is written of it. A block holds this before the
    /// region the catch covers, which is checked ahead of the catch, so its
    /// raises meet what is written; the catch's own check holds it where
    /// no block did (an input that is one catch).
    pub(crate) fn hold_written(&self, ctx: &CompileCtx<R>) -> Result<()> {
        match self.written() {
            Some(written) => written.check_contains(&ctx.env, &self.binder(ctx)?),
            None => Ok(()),
        }
    }

    /// The type the catch's binding is written with.
    fn written(&self) -> Option<&Type> {
        match &self.spec.kind {
            ExprKind::Catch(c) => c.constraint.as_ref(),
            _ => None,
        }
    }

    /// The type of the catch's binding.
    fn binder(&self, ctx: &CompileCtx<R>) -> Result<Type> {
        match ctx.env.by_id.get(&self.bind_id) {
            Some(b) => Ok(b.typ.clone()),
            None => bail!("BUG: catch bind vanished"),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let handler = decode_node(ctx, buf)?;
        let action = match opt_node_decode(ctx, buf)? {
            None => None,
            Some(node) => {
                let role = if bool::decode(buf)? {
                    AbortRole::machine(
                        opt_node_decode(ctx, buf)?,
                        BindId::decode(buf)?,
                        Option::<BindId>::decode(buf)?,
                    )
                } else {
                    AbortRole::Try { capture: BindId::decode(buf)? }
                };
                Some(CatchAction::with(node, role))
            }
        };
        let own_handler = image::handler_decode(buf)?;
        let bind_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(bind_id, top_id);
        let polled = ctx.boundary.clone();
        own_handler.polled_by(&polled);
        Ok(Self::with(spec, handler, action, own_handler, None, bind_id, top_id, polled))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        c: &Arc<expr::CatchExpr>,
    ) -> Result<(Node<R>, Scope)> {
        if let Some(t) = &c.constraint {
            t.refuse_quantified_fns(&ctx.env).at(&spec)?;
        }
        let catch_scope = scope.append_place("ca", &spec, None);
        let typ = Type::empty_tvar();
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
        let covered = scope.with_catch(
            (bind_id, top_id),
            match &c.role {
                CatchRole::User => HandlerKind::Catch,
                CatchRole::Try { .. } => HandlerKind::Try,
                CatchRole::Machine { .. } => HandlerKind::Machine,
            },
        );
        let lookup = |ctx: &CompileCtx<R>, name: &ArcStr| {
            let path = ModPath::from([name.as_str()]);
            match ctx.env.lookup_bind(&scope.lexical, &path)? {
                Some((_, b)) => Ok(b.id),
                None => bail!("BUG: seq cell {name} is not bound"),
            }
        };
        let action = match &c.role {
            CatchRole::User => None,
            CatchRole::Machine { action, manual, pc, clock } => {
                let node = compile(ctx, flags, (**action).clone(), &catch_scope, top_id)?;
                let manual = manual
                    .as_ref()
                    .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
                    .transpose()?;
                let pc = lookup(ctx, pc)?;
                let clock = clock.as_ref().map(|c| lookup(ctx, c)).transpose()?;
                Some(CatchAction::with(node, AbortRole::machine(manual, pc, clock)))
            }
            CatchRole::Try { action, capture } => {
                let node = compile(ctx, flags, (**action).clone(), &catch_scope, top_id)?;
                let role = AbortRole::Try { capture: lookup(ctx, capture)? };
                Some(CatchAction::with(node, role))
            }
        };
        ctx.record_ref(bind_id, top_id);
        let own_handler = covered.dynamic.handler().unwrap();
        if c.constraint.is_some() {
            own_handler.seal();
        }
        let polled = ctx.boundary.clone();
        own_handler.polled_by(&polled);
        let node = Self::with(
            spec,
            handler,
            action,
            own_handler,
            matches!(c.role, CatchRole::User).then_some(flags),
            bind_id,
            top_id,
            polled,
        );
        Ok((node, covered))
    }

    fn give_up_in_flight(&mut self, cycle: u64) {
        self.receipts.give_up(&self.own_handler, cycle)
    }
}

impl<R: Rt> Update<R> for Catch<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.handler);
        if let Some(abort) = &self.action {
            f(&abort.node);
            abort.manual().into_iter().for_each(f)
        }
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
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
        self.bind_id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let taken = match self.receipts.take(ctx, &self.own_handler) {
            Receipt::Passed(standing) => {
                let prior = ctx.event.variables.put(self.bind_id, standing);
                let _ = self.handler.update(ctx);
                ctx.event.variables.put_back(self.bind_id, prior);
                None
            }
            Receipt::Nothing => {
                let _ = self.handler.update(ctx);
                None
            }
            Receipt::Taken(v) => {
                let _ = self.handler.update(ctx);
                Some(v)
            }
        };
        let capture = self
            .action
            .as_ref()
            .and_then(|a| a.capture().filter(|_| a.pending.is_none()));
        if let Some(v) = taken {
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.give_up_in_flight(ctx.rt.cycle());
        self.handler.sleep(ctx);
        if let Some(abort) = &mut self.action {
            abort.node.sleep(ctx);
            abort.manual_mut().into_iter().for_each(|n| n.sleep(ctx));
            abort.pending = None;
            if let AbortRole::Machine { pc, clock, .. } = &mut abort.role {
                let idle = Value::I64(crate::expr::seq::IDLE);
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self, self.hold_written(ctx))?;
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
    }

    /// The error bind and the capture take the types their definition's
    /// check left them ([`Catch::aux_types`]); whether `T` covers the
    /// region's throws was judged there. With no row the binder holds what
    /// is written of it, or nothing.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        self.unused_warning = None;
        match types.aux(self.spec.id) {
            None => {
                if let Type::TVar(tv) = &self.binder(ctx)? {
                    tv.bind(self.written().cloned().unwrap_or(Type::Bottom))
                }
                self.typecheck0_with(
                    ctx,
                    &mut |n, ctx| n.typecheck0_instance(ctx, types),
                    false,
                )
            }
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

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Catch(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
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
    fn of<R: Rt>(ctx: &CompileCtx<R>, op: char, typ: &Type) -> Result<Self> {
        // an open operand would take the strip here before what it holds
        // is decided: its type must be known. An open member beside one
        // that holds an error leaves the strip decided (errors come off
        // first); beside none it would pick the null form or the error form
        // on what it later holds
        let err = Type::Error(Arc::new(Type::empty_tvar()));
        let members = Type::dispatch_members(&ctx.env, typ)?;
        let open_member = |m: &Type| m.with_deref(|t| t.is_none()) && m.has_free();
        let open = match typ.with_deref(|t| t.is_none()) {
            true => Some(typ),
            false => match members.iter().find(|m| open_member(m)) {
                None => None,
                Some(open) => {
                    let mut held = false;
                    for m in members.iter().filter(|m| !open_member(m)) {
                        held |= m.decided_holds(&ctx.env, &err)?
                    }
                    (!held).then_some(open)
                }
            },
        };
        if let Some(open) = open {
            let below = open.open_lower();
            format_with_flags(PrintFlag::DerefTVars, || match &below[..] {
                [] => bail!(
                    "the operand of {op} has type {typ}, not known here: annotate it \
                     with a type that holds an error or null"
                ),
                [l, ls @ ..] => {
                    use std::fmt::Write;
                    let mut at_least = format_compact!("{l}");
                    for l in ls {
                        write!(at_least, " | {l}")?
                    }
                    bail!(
                        "the operand of {op} has a type not decided here (at least \
                         {at_least}): annotate it"
                    )
                }
            })?
        }
        // what `?` raises is the operand's error part, which Any does not say
        if op == '?'
            && Type::dispatch_members(&ctx.env, typ)?
                .iter()
                .any(|t| matches!(t, Type::Any))
        {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "the operand of ? has type {typ}, so the error it raises is not \
                     known: cast it to a type whose error is (a Result), or use $"
                )
            })?
        }
        let null = Type::Primitive(Typ::Null.into());
        let holds = |t: &Type| typ.decided_holds(&ctx.env, t);
        // an open member holds both: the strip is the one that takes a
        // member out, the error form when neither does (`Any`)
        let takes = |strip: Self| -> Result<bool> {
            Ok(!strip.taken(&ctx.env, typ)?.is_uninhabited())
        };
        if holds(&err)? && holds(&null)? && !takes(Self::Error)? && takes(Self::Null)? {
            Ok(Self::Null)
        } else if holds(&err)? {
            if op == '?' {
                Self::open_member_raising_nothing(&ctx.env, typ)?;
            }
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

    /// `?` raises the errors its operand's decided members hold, and the
    /// handler it joins them into is fixed there: an open member without
    /// `NoError` may later hold an error that is raised and joined into
    /// nothing, so it is refused, naming the bound to declare.
    fn open_member_raising_nothing(env: &Env, typ: &Type) -> Result<()> {
        for m in Type::dispatch_members(env, typ)? {
            let Type::TVar(tv) = &m else { continue };
            if tv.binding().is_some() || !tv.may_hold_error(env)? {
                continue;
            }
            let name = format_compact!("{tv}");
            format_with_flags(PrintFlag::DerefTVars, || match tv.is_rigid() {
                true => bail!(
                    "the operand of ? has type {typ}, where {name} is any type its \
                     bounds admit, an error among them that ? would raise past \
                     its handler: declare {name}: NoError"
                ),
                false => bail!(
                    "the operand of ? has type {typ}, where {name} is not decided \
                     here and may hold an error that ? would raise past its \
                     handler: annotate it with a type that holds no error"
                ),
            })?
        }
        Ok(())
    }

    /// What a check by bounds says goes from an operand of the type `t`,
    /// where `t` is decided enough to say.
    fn by_bounds(env: &Env, t: &Type) -> Result<Option<Self>> {
        Ok(match (bounds::strippable(env, t)?, bounds::strips_errors(env, t)?) {
            (false, _) => None,
            (true, true) => Some(Self::Error),
            (true, false) => Some(Self::Null),
        })
    }

    pub(crate) fn removed(self) -> Type {
        match self {
            Self::Error => Type::Primitive(Typ::Error.into()),
            Self::Null => Type::Primitive(Typ::Null.into()),
        }
    }

    /// What this strip takes out of `typ`.
    // CR claude for claude: [perf/medium] Checking `?` costs time quadratic in the
    // operand union's member count. taken computes typ.diff(typ - removed), a
    // difference of an n-member union by an (n-1)-member one. Qop::typecheck0_with
    // computes it at every `?` site, and Strip::of computes it up to twice more when
    // the operand also holds null. Strip::of and open_member_raising_nothing also walk
    // Type::dispatch_members three times. Debug build, 100 sites over a 400-member
    // union: plain reads 0.05 s, `$` 0.18 s, `?` 1.2 s; at 800 members `?` takes 4.7 s.
    // Fix: take the members a strip matches directly (Strip::members(env, typ, true),
    // linear), compute them once per check, and share one dispatch_members list across
    // Strip::of and open_member_raising_nothing. probe:
    // design/review-2026-11-16/repro/c-error-op-strip-taken-quadratic.sh
    // (c-error-op-02)
    fn taken(self, env: &Env, typ: &Type) -> Result<Type> {
        match typ.diff_exact(env, &self.removed())? {
            Some(rest) => typ.diff(env, &rest),
            None => self.members(env, typ, true),
        }
    }

    /// What this strip leaves of `typ`.
    fn left(self, env: &Env, typ: &Type) -> Result<Type> {
        match typ.diff_exact(env, &self.removed())? {
            Some(rest) => Ok(rest),
            None => self.members(env, typ, false),
        }
    }

    /// The members of `typ`, read through names and cells, that this strip
    /// takes (or leaves): where the pair budget left a difference wider,
    /// which says neither.
    fn members(self, env: &Env, typ: &Type, taken: bool) -> Result<Type> {
        let removed = self.removed();
        let takes = |m: &Type| match self {
            Self::Error => matches!(m, Type::Error(_)) || *m == removed,
            Self::Null => *m == removed,
        };
        let ms =
            Type::dispatch_members(env, typ)?.into_iter().filter(|m| takes(m) == taken);
        Ok(Type::Set(Arc::from_iter(ms)).normalize())
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
fn strip_typ<R: Rt>(
    ctx: &CompileCtx<R>,
    op: char,
    operand: &Type,
    decided: Option<Strip>,
) -> Result<(Strip, Type)> {
    let strip = match decided {
        Some(strip) => strip,
        None => Strip::of(ctx, op, operand)?,
    };
    let rtyp = strip.left(&ctx.env, operand)?;
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
    if tv.tag().is_fired() {
        tv.with_value(sink)
    }
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
/// `Qop::update` and the fused kernel's delivery drain. The error is
/// stored as any write is: a same-top delivery lands in this cycle's
/// event, a cross-top one or a second one this cycle goes through
/// `rt.set_var`.
pub(crate) fn deliver_error<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
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
    if handler_top != own_top || ctx.event.variables.contains_key(&id) {
        ctx.rt.set_var(id, v)
    } else {
        ctx.deliver(id, TagValue::fired(v), Notify::Walk)
    }
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
    fn raise(
        handler: Option<ErrorHandler>,
        top_id: ExprId,
        flags: BitFlags<CFlag>,
    ) -> Self {
        Self::Raise { handler, top_id, flags, values: SiteValues::default() }
    }

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
        Ok(Self::raise(handler, top_id, flags))
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
pub struct Qop<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Node<R>,
    pub(crate) strip: Strip,
    pub(crate) mode: QopMode,
    /// What a raise carries ([`Self::raised_typ`]).
    raised: Type,
    /// An instance whose strip its definition decided.
    of_definition: bool,
    resident: TagValue,
}

impl<R: Rt> Qop<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        n: Node<R>,
        strip: Strip,
        mode: QopMode,
    ) -> Node<R> {
        let raised = Self::raised_typ(strip, &n);
        Node::new(Self {
            spec,
            typ,
            n,
            strip,
            mode,
            raised,
            of_definition: false,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let strip = Strip::decode(buf)?;
        let mode = QopMode::decode(buf)?;
        Ok(Self::with(spec, typ, n, strip, mode))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R>> {
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let mode = match spec.kind {
            ExprKind::OrNever(_) => QopMode::Ignore,
            _ => QopMode::raise(scope.dynamic.handler(), top_id, flags),
        };
        let typ = Type::empty_tvar();
        let strip = Strip::Error;
        Ok(Self::with(spec, typ, n, strip, mode))
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

    /// Take off what a check by bounds says goes from an operand of the
    /// type `t`, where `t` is decided enough to say.
    fn strip_of(&mut self, env: &Env, t: &Type) -> Result<()> {
        if let Some(strip) = Strip::by_bounds(env, t)? {
            self.strip = strip;
            self.raised = Self::raised_typ(self.strip, &self.n)
        }
        Ok(())
    }

    /// The type of what a raise here carries.
    fn raised_typ(strip: Strip, n: &Node<R>) -> Type {
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

impl<R: Rt> Update<R> for Qop<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        // an operand the check decided after this node was checked says
        // what goes only now
        if !self.of_definition && !matches!(self.spec.kind, ExprKind::Rethrow(_)) {
            let t = self.n.typ().resolve_tvars();
            wrap!(self, self.strip_of(&ctx.env, &t))?
        }
        Ok(())
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Qop, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.n.image_encode(buf)?;
        self.strip.encode(buf)?;
        self.mode.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.n, self.n.typecheck0(ctx))?;
        // warned once, at the definition's check: an instance compiles
        // under its call site's handlers, which raise_throws judges
        if let QopMode::Raise { handler: None, flags, .. } = &self.mode {
            // CR claude for claude: [bug/medium] A seq whose run raises with no catch
            // above it is warned here as "error raised by ? will not be caught" at the
            // `seq` keyword, where there is no `?`: the machine's handler is a
            // compiler-made Rethrow whose spec is the seq's. At run time the same
            // Rethrow's handler-less branch in Qop::update logs "unhandled error <seq
            // site> {cause: null, error: `A(1), ori: {..}, pos: {column: i32:3, line:
            // i32:16}}". That is the whole chain with typed integers and the real raise
            // site buried in it, where a handler-less `?` logs only its payload at its
            // own site. Fix: for ExprKind::Rethrow, word the warning for the seq. When
            // the payload is a chain, log its `error` (chain_error) at the site of the
            // link that raised it. probe:
            // design/review-2026-11-16/repro/c-error-op-seq-rethrow-diagnostic.gx
            // (c-error-op-03)
            match self.spec.kind {
                // a seq's rethrow raises only what its run raised, which
                // is known once the check ends
                ExprKind::Rethrow(_) => ctx.pending_names.push(crate::Pending::Raise {
                    raised: self.n.typ().clone(),
                    at: (*self.spec).clone(),
                    callee: None,
                    flags: *flags,
                }),
                _ => Self::check_unhandled(
                    &ctx.env,
                    *flags,
                    &self.spec,
                    "error raised by ?",
                )?,
            }
        }
        wrap!(self, self.check_by_bounds(ctx))
    }

    /// The strip is the definition's, over the operand as the definition
    /// sees it: an instance that gives a variable an error takes off what
    /// every instance does. The raise joined into the handler's bind is
    /// state: an instance computes it too.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        let row = types.settle(ctx, &self.spec, &self.typ)?;
        let seen = types.definitions(self.n.spec().id).map(Type::resolve_tvars);
        let decided = match &seen {
            Some(t) => Strip::by_bounds(&ctx.env, t)?,
            None => None,
        };
        self.of_definition = decided.is_some();
        wrap!(self.n, self.n.typecheck0_instance(ctx, types))?;
        if matches!(self.spec.kind, ExprKind::Rethrow(_))
            && self.n.typ().with_deref(|t| matches!(t, Some(Type::Bottom)))
        {
            return Ok(());
        }
        let op = if self.raises() { '?' } else { '$' };
        let (strip, left) = wrap!(self, strip_typ(ctx, op, self.n.typ(), decided))?;
        self.strip = strip;
        self.raised = Self::raised_typ(strip, &self.n);
        // where the definition's check left this node no row, its type is
        // what the strip leaves of the operand
        if !row && !bounds::take(&self.typ, &left) {
            bailat!(self.spec, "BUG: a strip's result took no row")
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R> {
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
pub struct SeqGuard<R: Rt> {
    spec: Arc<Expr>,
    pub(crate) n: Node<R>,
    handler: ErrorHandler,
    /// The machine's handler: an `abort(..)` fails the run there, and a
    /// try-body arm's nearest handler is its jump.
    machine: ErrorHandler,
    state: GuardState,
    /// It guards a call, not a nested seq: the call is issued as a fresh
    /// frame, everything it reads delivered.
    call: bool,
    resident: TagValue,
    /// The boundary this guard polls its handlers from.
    polled: Option<BoundaryRef>,
}

impl<R: Rt> SeqGuard<R> {
    fn with(
        spec: Arc<Expr>,
        n: Node<R>,
        handler: ErrorHandler,
        machine: ErrorHandler,
        polled: Option<BoundaryRef>,
    ) -> Node<R> {
        handler.polled_by(&polled);
        let call = matches!(&spec.kind, ExprKind::SeqGuard(e)
            if matches!(e.kind, ExprKind::Apply(_)));
        Node::new(Self {
            spec,
            n,
            handler,
            machine,
            state: GuardState::Sleeping,
            call,
            resident: TagValue::phantom(),
            polled,
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let n = decode_node(ctx, buf)?;
        let handler = image::handler_decode(buf)?;
        let machine = image::handler_decode(buf)?;
        let polled = ctx.boundary.clone();
        Ok(Self::with(spec, n, handler, machine, polled))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R>> {
        let handler =
            scope.dynamic.handler().ok_or_else(|| anyhow!("BUG: seq handler"))?;
        let machine = handler.machine().ok_or_else(|| anyhow!("BUG: seq machine"))?;
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        let polled = ctx.boundary.clone();
        Ok(Self::with(spec, n, handler, machine, polled))
    }
}

impl<R: Rt> Update<R> for SeqGuard<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let current = (self.handler.generation(), self.machine.generation());
        let (generation, passed, owed, issue) = match self.state {
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
                (current, false, false, self.call)
            }
            GuardState::Running { generation, passed, owed } => {
                (generation, passed, owed, false)
            }
            GuardState::Failed => {
                if self.handler.has_nested_errors() {
                    let _ = self.n.update(ctx);
                }
                return self.resident.ride();
            }
        };
        if generation == current || self.handler.has_nested_errors() {
            let n = &mut self.n;
            let frame = issue.then(|| FreshFrame::open(ctx, |r| n.refs(r)));
            let value = n.update(ctx);
            if let Some(frame) = frame {
                frame.close(ctx)
            }
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn view(&self) -> NodeView<'_, R> {
        NodeView::SeqGuard(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fuse(&mut self.n, ctx)?;
        Ok(None)
    }
}

/// A seq's `abort(..)` event, placed before the machine's select: a
/// fired production fails the run in the cycle it fires, so no guard
/// under the machine passes a completion that cycle. A block updates its
/// catches after their covered children, which is too late for that.
#[derive(Debug)]
pub struct SeqAbort<R: Rt> {
    spec: Arc<Expr>,
    pub(crate) n: Node<R>,
    machine: ErrorHandler,
}

impl<R: Rt> SeqAbort<R> {
    fn with(spec: Arc<Expr>, n: Node<R>, machine: ErrorHandler) -> Node<R> {
        Node::new(Self { spec, n, machine })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let n = decode_node(ctx, buf)?;
        let machine = image::handler_decode(buf)?;
        Ok(Self::with(spec, n, machine))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        e: &Expr,
    ) -> Result<Node<R>> {
        let machine = scope
            .dynamic
            .handler()
            .and_then(|h| h.machine())
            .ok_or_else(|| anyhow!("BUG: seq machine"))?;
        let n = compile(ctx, flags, e.clone(), scope, top_id)?;
        Ok(Self::with(spec, n, machine))
    }
}

impl<R: Rt> Update<R> for SeqAbort<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::SeqAbort, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)?;
        image::handler_encode(&self.machine, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn view(&self) -> NodeView<'_, R> {
        NodeView::SeqAbort(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fuse(&mut self.n, ctx)?;
        Ok(None)
    }
}

impl<R: Rt> Catch<R> {
    /// The error bind, then a `try` arm's capture cell.
    fn bind_ids(&self) -> impl Iterator<Item = BindId> + use<R> {
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
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        // CR claude for claude: [bug/low] A handler that reads its binder's parts over
        // a region no raise reaches is refused with "expected tuple not _" at `e.0`,
        // which names neither the catch nor the cause; a seq's `with(e)` does the same.
        // `{ catch(e) msg <- "[e.0.error]"; a[1]$ }` is the smallest case, and the same
        // handler written `"[e]"` checks with no warning though it can never run. Fix:
        // when the binder's raised type is still bottom once the covered region is
        // checked, say so over the handler's error (`Told::over`: no `?` under this
        // catch raises, so `e` has no type) and warn that the catch hears nothing.
        // probe:
        // design/review-2026-11-16/repro/x-engine-seq-errors-catch-no-raise-diagnostic.gx
        // (x-engine-seq-errors-06)
        wrap!(self.handler, child(&mut self.handler, ctx))?;
        // a handler that reads the error was checked at the bind's type; a
        // handler that ignores it (the REPL's tail catch) takes any raise
        let mut refs = Refs::default();
        self.handler.refs(&mut refs);
        if refs.is_refed(self.bind_id) {
            self.own_handler.seal();
        }
        if check
            && let Some(flags) = self.unused_warning.take()
            && !refs.is_refed(self.bind_id)
        {
            // One registration is the catch's delivery slot. Further registrations
            // read its binder, including those in checked definition bodies.
            let mut registrations = 0;
            let reads = ctx.pending_refs.iter().any(|((id, _), count)| {
                if *id == self.bind_id {
                    registrations += count;
                }
                registrations > 1
            });
            if !reads {
                let ExprKind::Catch(c) = &self.spec.kind else { unreachable!() };
                ctx.env.warn(
                    flags,
                    &self.spec,
                    c.bind.pos_or(self.spec.pos),
                    self.spec.end.0,
                    format_args!(
                        "catch handler never reads its error; sample the error binder (`e ~ value`)"
                    ),
                )?;
            }
        }
        let Some(abort) = &mut self.action else { return Ok(()) };
        wrap!(abort.node, child(&mut abort.node, ctx))?;
        if let Some(manual) = abort.manual_mut() {
            wrap!(manual, child(manual, ctx))?;
        }
        // the capture cell's type is the union of every covering
        // handler's throws
        if let Some(cap) = abort.capture() {
            let etyp = self.binder(ctx)?;
            let bind = ctx
                .env
                .by_id
                .get(&cap)
                .ok_or_else(|| anyhow!("BUG: seq capture cell vanished"))?;
            if !matches!(&bind.typ, Type::TVar(_)) {
                bail!("BUG: seq capture cell is not a cell")
            }
            return bind.typ.check_contains(&ctx.env, &etyp);
        }
        Ok(())
    }
}

impl<R: Rt> Qop<R> {
    /// The refusal of a raise that a sealed catch, whose handler was
    /// checked before it, cannot take: one naming the annotation where the
    /// catch saw no error.
    fn unfit(&self, env: &Env, handler: &Type, e: anyhow::Error) -> anyhow::Error {
        if !self.handler().is_some_and(|h| h.sealed()) {
            return e;
        }
        let raised = || {
            let t = self.n.typ().with_deref(|t| t.cloned())?;
            if matches!(self.spec.kind, ExprKind::Rethrow(_)) {
                return Some(t);
            }
            let (_, payloads) = bounds::strip(env, &t, true).ok()?;
            let chain = Type::err_chain(bounds::union_of(payloads));
            Some(Type::Error(Arc::new(chain)))
        };
        let nothing = handler.with_deref(|t| matches!(t, Some(Type::Bottom)));
        format_with_flags(PrintFlag::DerefTVars, || match raised() {
            Some(etyp) if nothing => anyhow!(
                "this error reaches a catch that saw no error when it was checked: \
                 annotate the catch with the errors it takes, `catch(e: {etyp})`"
            ),
            _ => anyhow!(
                "this error reaches a catch whose handler was checked to take {handler}: {e}"
            ),
        })
    }

    // CR claude for claude: [bug/medium] When the operand is only an error,
    // strip_typ returns Type::Bottom, but this site only checks that the Qop's
    // fresh cell (Qop::compile's empty_tvar) contains it, so the cell stays open.
    // never() is typed Type::Bottom and a select absorbs it; this open cell is not
    // absorbed. As a result, `select x { 2 => error(`E(x))?, _ => x }` keeps an
    // undecided member: a later `select v { 0 => 1, n => n }` is refused asking to
    // annotate v (accepted with never()), and at the top level the select cannot
    // fuse ('a region of type '_N: unbound has no kernel return').
    // lang::errors::arm_input_raise_delivers pins that as FuseExpect::None, and its
    // #[native] twin fuses only through `let v: i64`. Bind the cell to Type::Bottom
    // when the strip leaves nothing, as strip_typ's doc promises, then flip that
    // fixture to FuseExpect::Jit. probe:
    // design/review-2026-11-16/repro/tests-lang-rest-d-raise-arm-leaves-select-open.gx
    // (tests-lang-rest-d-02)
    // 2026-11-19 claude: On bounds-spike Qop's old path (typecheck0_with, the strip's
    // check_contains) is deleted with the old check: a definition's Qop is checked by
    // check_by_bounds, an instance's takes its row. Moved here unchanged. The probe
    // now checks (--check exit 0). Not cleared: flip
    // lang::errors::arm_input_raise_delivers as the text says and pin the probe first.

    // CR claude for claude: [perf/medium] When the operand is always what the
    // op strips (`error(`E(x))?`, `error(..)$`), `rtyp` is ⊥ and self.typ is
    // left an unbound cell. Fusion cannot classify that cell: a select arm
    // holding the raise refuses its region ("a region of type '_N: unbound has
    // no kernel return"), and a `never(error(..)?)` arm's discarded argument
    // refuses emission ("doesn't classify — can't drop it"). strict_fusion.md
    // says both fuse (emit_qop_always_bad). `#[native] select x { 2 =>
    // error(`E(x))$, _ => x }` at the top level is refused, while with `let w:
    // i64 =` it fuses and both engines agree. Settle the cell to ⊥ where the
    // strip leaves nothing, or freeze such a cell as ⊥ in fusion. Then
    // lang::fusion's never_arm_args_raise and arm_raise_standing_input fuse.
    // probe: design/review-2026-11-16/repro/tests-fusion-arm-raise-defuses.sh
    // (tests-fusion-02)
    // 2026-11-19 claude: Moved here unchanged with Qop's old path (deleted on
    // bounds-spike). The probe now: d and b fuse and print 1, 3; c as before; a
    // (`never(error(`E(x))$)`) is still refused 'a region of type _ has no
    // kernel return'.

    /// The check by bounds: the operand less its errors, or its null,
    /// where its type is decided enough to say which; else the strip
    /// waits for it. What a `?` raises reaches the handler as a chain over
    /// the payloads of the errors taken off.
    fn check_by_bounds(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let env = &ctx.env;
        let handler =
            self.handler().and_then(|h| env.by_id.get(&h.id().0)).map(|b| b.typ.clone());
        if matches!(self.spec.kind, ExprKind::Rethrow(_)) {
            return match handler {
                Some(h) => h
                    .check_contains(env, self.n.typ())
                    .map_err(|e| self.unfit(env, &h, e)),
                None => Ok(()),
            };
        }
        let payload = match self.raises() {
            false => None,
            true => {
                let payload = Type::empty_tvar();
                if let Some(h) = &handler {
                    let chain = Type::Error(Arc::new(Type::err_chain(payload.clone())));
                    h.check_contains(env, &chain).map_err(|e| self.unfit(env, h, e))?
                }
                Some(payload)
            }
        };
        match self.n.typ().with_deref(|t| t.cloned()) {
            Some(t) if bounds::strippable(env, &t)? => {
                self.strip_of(env, &t)?;
                let (left, payloads) = bounds::strip(env, &t, payload.is_some())?;
                for (p, to) in
                    payloads.iter().flat_map(|p| payload.iter().map(move |to| (p, to)))
                {
                    to.check_contains(env, p).map_err(|e| match &handler {
                        Some(h) => self.unfit(env, h, e),
                        None => e,
                    })?
                }
                self.typ.check_contains(env, &left)
            }
            _ => {
                let (operand, out) = (self.n.typ().clone(), self.typ.clone());
                super::solving(|s| s.hold_strip(operand, out, payload));
                Ok(())
            }
        }
    }
}

impl<R: Rt> SeqGuard<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
    ) -> Result<()> {
        child(&mut self.n, ctx)
    }
}

impl<R: Rt> SeqAbort<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
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
    /// Raises were given up at a sleep and the catch has not woken since.
    gave_up: bool,
    /// Raises acknowledged to the enclosing handlers: taken, or given up.
    acked: u64,
    /// The cycle a delivery arrived in: a later raise that cycle arrives
    /// the next.
    last_cycle: Option<u64>,
}

/// What a catch makes of the delivery to its variable this cycle.
enum Receipt {
    Nothing,
    /// The error of a raise not given up.
    Taken(Value),
    /// A delivery no raise left to take (its raise was given up), as the
    /// handler sees it: standing, moved ([`Tag::moved`]).
    Passed(TagValue),
}

impl Receipts {
    /// This cycle's delivery of `handler`'s raises.
    fn take<R: Rt>(&mut self, ctx: &ExecCtx<'_, R>, handler: &ErrorHandler) -> Receipt {
        let (bind_id, _) = handler.id();
        let cycle = ctx.rt.cycle();
        if std::mem::take(&mut self.gave_up) {
            self.landed_asleep(ctx, handler, &bind_id);
        }
        // a catch updated twice in one cycle would read its delivery twice
        let tv = match read_var(ctx, &bind_id) {
            Some(VarRead::Delivered(tv))
                if self.last_cycle != Some(cycle) && tv.tag().is_fired() =>
            {
                tv
            }
            _ => return Receipt::Nothing,
        };
        let passed = || {
            let mut standing = tv.clone();
            standing.retag(Tag::MOVED);
            Receipt::Passed(standing)
        };
        if self.arrived >= handler.raised() {
            return passed();
        }
        self.last_cycle = Some(cycle);
        self.arrived += 1;
        if self.arrived <= self.acked {
            return passed();
        }
        self.acked += 1;
        handler.handled(cycle);
        Receipt::Taken(tv.value_cloned())
    }

    /// At the wake, count as arrived the given-up deliveries that landed
    /// while the catch slept, unread: those owed less those still on their
    /// way (queued, or delivered this cycle), ahead of the deliveries of the
    /// raises made since.
    fn landed_asleep<R: Rt>(
        &mut self,
        ctx: &ExecCtx<'_, R>,
        handler: &ErrorHandler,
        id: &BindId,
    ) {
        let owed = self.acked - self.arrived;
        let now = matches!(read_var(ctx, id), Some(VarRead::Delivered(tv)) if tv.tag().is_fired());
        let coming = ctx.rt.waiting_writes(id) as u64 + now as u64;
        let since = handler.raised() - self.acked;
        self.arrived += owed - owed.min(coming.saturating_sub(since));
    }

    /// Acknowledge every raise not taken yet, so no enclosing handler
    /// waits on them; their deliveries are passed over.
    fn give_up(&mut self, handler: &ErrorHandler, cycle: u64) {
        self.gave_up = true;
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
    fn with(handler: ErrorHandler) -> Self {
        Self { handler, receipts: Receipts::default() }
    }

    /// The relay, and the scope the function it covers compiles under.
    pub fn new<R: Rt>(
        ctx: &mut CompileCtx<R>,
        scope: &Scope,
        top_id: ExprId,
    ) -> (Self, Scope) {
        // the check gives the cell what the function raises as it flows in
        let typ = Type::empty_tvar();
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
        let covered = scope.with_catch((bind_id, top_id), HandlerKind::Catch);
        let handler = covered.dynamic.handler().unwrap();
        (Self::with(handler), covered)
    }

    /// The error the function raised this cycle, which also goes on.
    pub fn update<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) -> Option<Value> {
        let Receipt::Taken(e) = self.receipts.take(ctx, &self.handler) else {
            return None;
        };
        match self.handler.parent().handler() {
            Some(parent) => {
                parent.raise(ctx.rt.cycle());
                let (id, _) = parent.id();
                ctx.rt.set_var(id, e.clone())
            }
            // CR claude for claude: [readability/low] An http or rpc handler's raise
            // that no catch covers is logged twice per request. Here it is logged as
            // "unhandled error error:{cause: null, error: `Empty("/a"), ...}": no site,
            // `error` doubled, a format no other unhandled report uses. The builtin's
            // Reply also logs it as "an http handler raised: ...". Fix: drop this
            // report, since every Handler user logs the raise. Or log it through
            // unhandled_msg with the chain's own site and its `error` field. probe:
            // design/review-2026-11-16/repro/c-error-op-relay-unhandled-log.gx
            // (c-error-op-04)
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

    pub fn delete<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.give_up_in_flight(ctx.rt.cycle());
        let (bind_id, top_id) = self.handler.id();
        ctx.release_var(bind_id, top_id);
        ctx.env.unbind_variable(bind_id);
    }

    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        image::handler_encode(&self.handler, buf)
    }

    pub fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let handler = image::handler_decode(buf)?;
        let (bind_id, top_id) = handler.id();
        ctx.record_ref(bind_id, top_id);
        Ok(Self::with(handler))
    }
}
