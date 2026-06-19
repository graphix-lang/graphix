use anyhow::{bail, Result};
use graphix_compiler::{
    effects::EffectKind,
    expr::ExprId,
    node::genn,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, Event, ExecCtx, Node, Refs, Rt, Scope, UserEvent,
};
use netidx_value::{ValArray, Value};

use crate::{CachedArgs, CachedVals, EvalCached};

// ── Predicates ─────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct IsSomeEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsSomeEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_opt_is_some";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            None => None,
            Some(Value::Null) => Some(Value::Bool(false)),
            Some(_) => Some(Value::Bool(true)),
        }
    }
}

pub(crate) type IsSome = CachedArgs<IsSomeEv>;

#[derive(Debug, Default)]
pub(crate) struct IsNoneEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsNoneEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_opt_is_none";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            None => None,
            Some(Value::Null) => Some(Value::Bool(true)),
            Some(_) => Some(Value::Bool(false)),
        }
    }
}

pub(crate) type IsNone = CachedArgs<IsNoneEv>;

#[derive(Debug, Default)]
pub(crate) struct ContainsEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ContainsEv {
    const EFFECT: EffectKind = EffectKind::Sync;
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

// ── Unwrapping / defaults ──────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct OrNeverEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrNeverEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_opt_or_never";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            None | Some(Value::Null) => None,
            Some(v) => Some(v.clone()),
        }
    }
}

pub(crate) type OrNever = CachedArgs<OrNeverEv>;

#[derive(Debug, Default)]
pub(crate) struct OrDefaultEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrDefaultEv {
    const EFFECT: EffectKind = EffectKind::Sync;
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

// ── Binary combinators ─────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct OrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
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
    const EFFECT: EffectKind = EffectKind::Sync;
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

#[derive(Debug, Default)]
pub(crate) struct XorEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for XorEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_opt_xor";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let (a, b) = match (&from.0[0], &from.0[1]) {
            (Some(a), Some(b)) => (a, b),
            _ => return None,
        };
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
}

pub(crate) type Xor = CachedArgs<XorEv>;

// ── Structural ─────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct ZipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ZipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
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

#[derive(Debug, Default)]
pub(crate) struct UnzipEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for UnzipEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_opt_unzip";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            None => None,
            Some(Value::Null) => Some(Value::Array(ValArray::from_iter_exact(
                [Value::Null, Value::Null].into_iter(),
            ))),
            Some(Value::Array(pair)) if pair.len() == 2 => Some(Value::Array(
                ValArray::from_iter_exact([pair[0].clone(), pair[1].clone()].into_iter()),
            )),
            Some(_) => None,
        }
    }
}

pub(crate) type Unzip = CachedArgs<UnzipEv>;

// ── Conversion to Result ───────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct OkOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OkOrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_opt_ok_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match (&from.0[0], &from.0[1]) {
            (Some(Value::Null), Some(e)) => {
                Some(Value::Error(triomphe::Arc::new(e.clone())))
            }
            (Some(v), _) => Some(v.clone()),
            _ => None,
        }
    }
}

pub(crate) type OkOr = CachedArgs<OkOrEv>;

// ── Higher-order combinators ───────────────────────────────────────
//
// These follow the same shape as `core::filter` (see `lib.rs` Filter)
// but without its `VecDeque` queue. Each input update is fed into the
// callback's argument binding `x` and `self.inner` (the callsite node
// for `f(x)`) is driven to produce an output. When a new input arrives
// while an earlier callback is still in flight, the new value simply
// overwrites `x`, superseding the pending computation (latest-wins).
// This avoids unbounded memory growth when a user callback never
// returns — the explicit `core::queue` / `core::hold` operators are
// available for users who want ordered async behavior.

/// Shared state for HOFs that feed the option's inner value into a
/// unary callback (`map`, `flat_map`, `filter`, `is_some_and`,
/// `is_none_or`).
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
        let inner = genn::apply(fnode, scope.clone(), vec![xn], &ptyp, top_id);
        Ok(Self { inner, fid, x })
    }

    fn feed_callable(
        &self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) {
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
    }

    fn feed_x(&self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>, v: Value) {
        ctx.cached.insert(self.x, v.clone());
        event.variables.insert(self.x, v);
    }

    /// Standard fire-and-forget tick used by map/flat_map/is_some_and/
    /// is_none_or: a null input emits `on_null` directly without
    /// invoking the callback; a non-null input is fed into `x` and the
    /// callback's output (whenever it arrives) becomes the result.
    /// `direct.or(inner)` is `direct` first because in the same cycle
    /// both can produce, and the direct branch is always for the input
    /// we just consumed.
    fn tick_unary(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
        on_null: Value,
    ) -> Option<Value> {
        self.feed_callable(ctx, from, event);
        let direct = match from[0].update(ctx, event) {
            Some(Value::Null) => Some(on_null),
            Some(v) => {
                self.feed_x(ctx, event, v);
                None
            }
            None => None,
        };
        let inner_out = self.inner.update(ctx, event);
        direct.or(inner_out)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.inner.sleep(ctx);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.fid);
        ctx.cached.remove(&self.x);
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

// ── map ────────────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct OptMap<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptMap<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: HofState::unary(ctx, typ, scope, top_id)? }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptMap<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.s.tick_unary(ctx, from, event, Value::Null)
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
}

// ── flat_map ───────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct OptFlatMap<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptFlatMap<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: HofState::unary(ctx, typ, scope, top_id)? }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptFlatMap<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.s.tick_unary(ctx, from, event, Value::Null)
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
}

// ── filter ─────────────────────────────────────────────────────────
//
// We need to remember the value fed into `x` so that when `pred`
// eventually produces its bool, we can emit the original `x` on
// `true`. Latest-wins: a newer non-null input overwrites the cached
// value the same way it overwrites `x` itself.

#[derive(Debug)]
pub(crate) struct OptFilter<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
    pending: Option<Value>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptFilter<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: HofState::unary(ctx, typ, scope, top_id)?, pending: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptFilter<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.s.feed_callable(ctx, from, event);
        let direct = match from[0].update(ctx, event) {
            Some(Value::Null) => {
                self.pending = None;
                Some(Value::Null)
            }
            Some(v) => {
                self.pending = Some(v.clone());
                self.s.feed_x(ctx, event, v);
                None
            }
            None => None,
        };
        let inner_out = self.s.inner.update(ctx, event).map(|b| match b {
            Value::Bool(true) => self.pending.clone().unwrap_or(Value::Null),
            _ => Value::Null,
        });
        direct.or(inner_out)
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
}

// ── is_some_and ────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct OptIsSomeAnd<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptIsSomeAnd<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: HofState::unary(ctx, typ, scope, top_id)? }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptIsSomeAnd<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.s.tick_unary(ctx, from, event, Value::Bool(false))
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
}

// ── is_none_or ─────────────────────────────────────────────────────

#[derive(Debug)]
pub(crate) struct OptIsNoneOr<R: Rt, E: UserEvent> {
    s: HofState<R, E>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptIsNoneOr<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: HofState::unary(ctx, typ, scope, top_id)? }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptIsNoneOr<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.s.tick_unary(ctx, from, event, Value::Bool(true))
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
}

// ── or_else / ok_or_else ───────────────────────────────────────────
//
// These take a nullary callback `f`. We always drive `f` so its latest
// result is cached and available whenever a null input arrives. For
// pure `f`s this means one initial evaluation; for reactive `f`s the
// cache tracks the latest value from `f`. We also cache `a`'s latest
// value so a new emission from `f` can be resolved against the current
// `a`.

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
        let inner = genn::apply(fnode, scope.clone(), vec![], &ptyp, top_id);
        Ok(Self { inner, fid, last_a: None, last_f: None })
    }

    /// Run the update plumbing shared by or_else / ok_or_else. Returns
    /// `(a_updated, f_updated)`: whether `a` or `f()` produced a new
    /// value on this cycle.
    fn tick(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> (bool, bool) {
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        let a_updated = if let Some(a) = from[0].update(ctx, event) {
            self.last_a = Some(a);
            true
        } else {
            false
        };
        let f_updated = if let Some(v) = self.inner.update(ctx, event) {
            self.last_f = Some(v);
            true
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.fid);
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
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptOrElse<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: OrElseShared::init(ctx, typ, scope, top_id)? }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptOrElse<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let (a_up, f_up) = self.s.tick(ctx, from, event);
        // a non-null always emits a
        // a null with cached f emits that f
        // f update while a is null emits the new f
        // a null without cached f stays silent — we have nothing to emit
        // until f produces, at which point the f_up arm below fires
        if a_up {
            match &self.s.last_a {
                Some(Value::Null) => self.s.last_f.clone(),
                Some(v) => Some(v.clone()),
                None => None,
            }
        } else if f_up && matches!(self.s.last_a, Some(Value::Null)) {
            self.s.last_f.clone()
        } else {
            None
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
}

#[derive(Debug)]
pub(crate) struct OptOkOrElse<R: Rt, E: UserEvent> {
    s: OrElseShared<R, E>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for OptOkOrElse<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        Ok(Box::new(Self { s: OrElseShared::init(ctx, typ, scope, top_id)? }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for OptOkOrElse<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let (a_up, f_up) = self.s.tick(ctx, from, event);
        let wrap_err = |e: Value| Value::Error(triomphe::Arc::new(e));
        if a_up {
            match &self.s.last_a {
                // a null without cached f stays silent until f produces.
                Some(Value::Null) => self.s.last_f.clone().map(wrap_err),
                Some(v) => Some(v.clone()),
                None => None,
            }
        } else if f_up && matches!(self.s.last_a, Some(Value::Null)) {
            self.s.last_f.clone().map(wrap_err)
        } else {
            None
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
}
