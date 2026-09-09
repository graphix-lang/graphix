use anyhow::{Result, bail};
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, FastCall, Node, Refs, Rt, Scope, Tag,
    TagValue, UserEvent,
    effects::Effect,
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
};
use netidx_value::{ValArray, Value};

use crate::{CachedArgs, CachedVals, EvalCached, seam_tick, seam_value};

fn fc_is_some(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Bool(false)),
        _ => Some(Value::Bool(true)),
    }
}

#[derive(Debug, Default)]
pub(crate) struct IsSomeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsSomeEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_is_some)));
    const NAME: &str = "core_opt_is_some";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        crate::fast_eval(fc_is_some, from)
    }
}

pub(crate) type IsSome = CachedArgs<IsSomeEv>;

fn fc_is_none(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Bool(true)),
        _ => Some(Value::Bool(false)),
    }
}

#[derive(Debug, Default)]
pub(crate) struct IsNoneEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsNoneEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_is_none)));
    const NAME: &str = "core_opt_is_none";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        crate::fast_eval(fc_is_none, from)
    }
}

pub(crate) type IsNone = CachedArgs<IsNoneEv>;

#[derive(Debug, Default)]
pub(crate) struct ContainsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ContainsEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_opt_contains";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), _) => Some(Value::Bool(false)),
            (Some(v), Some(x)) => Some(Value::Bool(v == x)),
            _ => None,
        }
    }
}

pub(crate) type Contains = CachedArgs<ContainsEv>;

fn fc_or_never(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => None,
        v => Some(v.clone()),
    }
}

#[derive(Debug, Default)]
pub(crate) struct OrNeverEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrNeverEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_or_never)));
    const NAME: &str = "core_opt_or_never";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        crate::fast_eval(fc_or_never, from)
    }
}

pub(crate) type OrNever = CachedArgs<OrNeverEv>;

#[derive(Debug, Default)]
pub(crate) struct OrDefaultEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrDefaultEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_opt_or_default";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), Some(d)) => Some(d.clone()),
            (Some(v), _) => Some(v.clone()),
            _ => None,
        }
    }
}

pub(crate) type OrDefault = CachedArgs<OrDefaultEv>;

#[derive(Debug, Default)]
pub(crate) struct OrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_opt_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), Some(b)) => Some(b.clone()),
            (Some(a), _) => Some(a.clone()),
            _ => None,
        }
    }
}

pub(crate) type Or = CachedArgs<OrEv>;

#[derive(Debug, Default)]
pub(crate) struct AndEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for AndEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_opt_and";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), _) => Some(Value::Null),
            (Some(_), Some(b)) => Some(b.clone()),
            _ => None,
        }
    }
}

pub(crate) type And = CachedArgs<AndEv>;

fn fc_xor(args: &[Value]) -> Option<Value> {
    let (a, b) = (&args[0], &args[1]);
    let a_some = !matches!(a, Value::Null);
    let b_some = !matches!(b, Value::Null);
    Some(if a_some && !b_some {
        a.clone()
    } else if !a_some && b_some {
        b.clone()
    } else {
        Value::Null
    })
}

#[derive(Debug, Default)]
pub(crate) struct XorEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for XorEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_xor)));
    const NAME: &str = "core_opt_xor";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        crate::fast_eval(fc_xor, from)
    }
}

pub(crate) type Xor = CachedArgs<XorEv>;

#[derive(Debug, Default)]
pub(crate) struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_opt_zip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), _) | (_, Some(Value::Null)) => Some(Value::Null),
            (Some(a), Some(b)) => Some(Value::Array(ValArray::from_iter_exact(
                [a.clone(), b.clone()].into_iter(),
            ))),
            _ => None,
        }
    }
}

pub(crate) type Zip = CachedArgs<ZipEv>;

fn fc_unzip(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Array(ValArray::from_iter_exact(
            [Value::Null, Value::Null].into_iter(),
        ))),
        Value::Array(pair) if pair.len() == 2 => Some(Value::Array(
            ValArray::from_iter_exact([pair[0].clone(), pair[1].clone()].into_iter()),
        )),
        _ => None,
    }
}

#[derive(Debug, Default)]
pub(crate) struct UnzipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnzipEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_unzip)));
    const NAME: &str = "core_opt_unzip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        crate::fast_eval(fc_unzip, from)
    }
}

pub(crate) type Unzip = CachedArgs<UnzipEv>;

#[derive(Debug, Default)]
pub(crate) struct OkOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OkOrEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_opt_ok_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), Some(e)) => Some(Value::Error(e.clone().into())),
            (Some(v), _) => Some(v.clone()),
            _ => None,
        }
    }
}

pub(crate) type OkOr = CachedArgs<OkOrEv>;

/// Shared state for HOFs that feed the option's inner value into a
/// unary callback. Latest-wins: a new input while the callback is in
/// flight overwrites `x`; wrap with `queue` for ordered delivery.
#[derive(Debug)]
struct HofState<R: Rt, E: UserEvent> {
    inner: Node<R, E>,
    fid: BindId,
    x: BindId,
}

impl<R: Rt, E: UserEvent> HofState<R, E> {
    /// Build the bindings and callsite for `f(x)` where the option's
    /// inner type is `typ.args[1].typ`'s argument type.
    fn unary(
        ctx: &mut ExecCtx<R, E>,
        typ: &FnType,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Self> {
        let ptyp = match &typ.args[1].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("expected a function not {t}"),
        };
        if ptyp.args.is_empty() {
            bail!("expected unary callback");
        }
        let x_typ = ptyp.args[0].typ.clone();
        let (x, xn) = genn::bind(ctx, &scope.lexical, "x", x_typ, top_id);
        let fid = BindId::new();
        let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
        let inner =
            genn::apply(fnode, scope.clone(), smallvec::smallvec![xn], &ptyp, top_id);
        Ok(Self { inner, fid, x })
    }

    fn feed_callable(
        &self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) {
        if let Some(tv) = seam_value(from[1].update(ctx, event)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            ctx.rt.store_insert(self.fid, TagValue::fired(v.clone()));
            event.variables.insert(self.fid, TagValue::tagged(v, tag));
        }
    }

    fn feed_x(&self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>, v: Value, tag: Tag) {
        ctx.rt.store_insert(self.x, TagValue::fired(v.clone()));
        event.variables.insert(self.x, TagValue::tagged(v, tag));
    }

    /// A null input emits `on_null` without invoking the callback; a
    /// non-null input is fed into `x` and the callback's output becomes
    /// the result. `direct` wins over `inner` when both produce in one
    /// cycle: it is always for the input just consumed.
    fn tick_unary(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
        on_null: Value,
    ) -> Option<Value> {
        self.feed_callable(ctx, from, event);
        let direct = match seam_value(from[0].update(ctx, event)) {
            Some(tv) => {
                let tag = tv.tag();
                // Only a fired null emits; a stale null is quiet.
                let drives = tv.is_fired();
                match tv.value_cloned() {
                    Value::Null if drives => Some(on_null),
                    Value::Null => None,
                    v => {
                        self.feed_x(ctx, event, v, tag);
                        None
                    }
                }
            }
            None => None,
        };
        let inner_out =
            seam_tick(self.inner.update(ctx, event)).map(|tv| tv.value_cloned());
        direct.or(inner_out)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.inner.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.inner.reset_replay(ctx);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.store_remove(&self.fid);
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.inner.delete(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        self.inner.refs(refs);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.inner.typecheck0(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct OptMap<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptMap<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_map";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: HofState::unary(ctx, typ, scope, top_id)?,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptMap<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match self.s.tick_unary(ctx, from, event, Value::Null) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct OptFlatMap<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptFlatMap<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_flat_map";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: HofState::unary(ctx, typ, scope, top_id)?,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptFlatMap<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match self.s.tick_unary(ctx, from, event, Value::Null) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct OptFilter<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
    pending: Option<Value>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptFilter<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_filter";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: HofState::unary(ctx, typ, scope, top_id)?,
            pending: None,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptFilter<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        self.s.feed_callable(ctx, from, event);
        let direct = match seam_value(from[0].update(ctx, event)) {
            Some(tv) => {
                let tag = tv.tag();
                // A stale null neither emits nor clears the pending latch.
                let drives = tv.is_fired();
                match tv.value_cloned() {
                    Value::Null if drives => {
                        self.pending = None;
                        Some(Value::Null)
                    }
                    Value::Null => None,
                    v => {
                        self.pending = Some(v.clone());
                        self.s.feed_x(ctx, event, v, tag);
                        None
                    }
                }
            }
            None => None,
        };
        let inner_out =
            seam_tick(self.s.inner.update(ctx, event)).map(|b| match b.value_cloned() {
                Value::Bool(true) => self.pending.clone().unwrap_or(Value::Null),
                _ => Value::Null,
            });
        match direct.or(inner_out) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pending = None;
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pending = None;
        self.s.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct OptIsSomeAnd<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptIsSomeAnd<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_is_some_and";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: HofState::unary(ctx, typ, scope, top_id)?,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptIsSomeAnd<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match self.s.tick_unary(ctx, from, event, Value::Bool(false)) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct OptIsNoneOr<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptIsNoneOr<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_is_none_or";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: HofState::unary(ctx, typ, scope, top_id)?,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptIsNoneOr<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match self.s.tick_unary(ctx, from, event, Value::Bool(true)) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.reset_replay(ctx);
    }
}

#[derive(Debug)]
struct OrElseShared<R: Rt, E: UserEvent> {
    inner: Node<R, E>,
    fid: BindId,
    last_a: Option<Value>,
    last_f: Option<Value>,
}

impl<R: Rt, E: UserEvent> OrElseShared<R, E> {
    fn init(
        ctx: &mut ExecCtx<R, E>,
        typ: &FnType,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Self> {
        let ptyp = match &typ.args[1].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("expected a function not {t}"),
        };
        let fid = BindId::new();
        let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
        let inner =
            genn::apply(fnode, scope.clone(), smallvec::smallvec![], &ptyp, top_id);
        Ok(Self { inner, fid, last_a: None, last_f: None })
    }

    /// Returns `(a_updated, f_updated)`: whether `a` or `f()` fired
    /// this cycle. `f` is always driven so its latest result is cached
    /// for the next null `a`.
    fn tick(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> (bool, bool) {
        if let Some(tv) = seam_value(from[1].update(ctx, event)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            ctx.rt.store_insert(self.fid, TagValue::fired(v.clone()));
            event.variables.insert(self.fid, TagValue::tagged(v, tag));
        }
        // A stale delivery refreshes the latches but does not drive
        // an emission.
        let a_updated = if let Some(a) = seam_value(from[0].update(ctx, event)) {
            self.last_a = Some(a.value_cloned());
            a.is_fired()
        } else {
            false
        };
        let f_updated = if let Some(v) = seam_value(self.inner.update(ctx, event)) {
            self.last_f = Some(v.value_cloned());
            v.is_fired()
        } else {
            false
        };
        (a_updated, f_updated)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.last_a = None;
        self.last_f = None;
        self.inner.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.last_a = None;
        self.last_f = None;
        self.inner.reset_replay(ctx);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.store_remove(&self.fid);
        self.inner.delete(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        self.inner.refs(refs);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.inner.typecheck0(ctx)
    }
}

#[derive(Debug)]
pub(crate) struct OptOrElse<R: Rt, E: UserEvent> {
    s: OrElseShared<R, E>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptOrElse<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_or_else";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: OrElseShared::init(ctx, typ, scope, top_id)?,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptOrElse<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let (a_up, f_up) = self.s.tick(ctx, from, event);
        let res = if a_up {
            match &self.s.last_a {
                Some(Value::Null) => self.s.last_f.clone(),
                Some(v) => Some(v.clone()),
                None => None,
            }
        } else if f_up && matches!(self.s.last_a, Some(Value::Null)) {
            self.s.last_f.clone()
        } else {
            None
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.reset_replay(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct OptOkOrElse<R: Rt, E: UserEvent> {
    s: OrElseShared<R, E>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptOkOrElse<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_opt_ok_or_else";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        if from.len() != 2 {
            bail!("expected two arguments");
        }
        let typ = resolved.unwrap_or(typ);
        Ok(Box::new(Self {
            s: OrElseShared::init(ctx, typ, scope, top_id)?,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptOkOrElse<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let (a_up, f_up) = self.s.tick(ctx, from, event);
        let wrap_err = |e: Value| Value::Error(e.into());
        let res = if a_up {
            match &self.s.last_a {
                Some(Value::Null) => self.s.last_f.clone().map(wrap_err),
                Some(v) => Some(v.clone()),
                None => None,
            }
        } else if f_up && matches!(self.s.last_a, Some(Value::Null)) {
            self.s.last_f.clone().map(wrap_err)
        } else {
            None
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.s.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.s.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.s.reset_replay(ctx);
    }
}
