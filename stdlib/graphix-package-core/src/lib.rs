#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, FastCall, FastFn, Node, Refs, Rt, Scope, Tag,
    TagValue, TagView, TypedFastFn, UserEvent,
    effects::Effect,
    env::Env,
    err, errf,
    expr::{Expr, ExprId},
    node::{coretraits, genn},
    typ::{FnType, TVal, Type, TypeRef},
};
use graphix_rt::GXRt;
use netidx::{path::Path, publisher::Typ, subscriber::Value};
use netidx_core::utils::Either;
use netidx_value::{FromValue, ValArray};
use poolshark::local::LPooled;
use std::{
    any::Any,
    collections::VecDeque,
    fmt::{Debug, Write},
    iter,
    time::Duration,
};
use tokio::time::Instant;

pub(crate) mod buffer;
pub(crate) mod math;
pub(crate) mod opt;
pub(crate) mod queuefn;

/// The success member `T` of a `Result<T, E>` return type, in either the
/// named form or the expanded `[T, Error<E>]` form. Shape only; the
/// typecheck-time validation is [`extract_cast_type`].
pub fn cast_target(rtype: &Type) -> Option<Type> {
    rtype.with_deref(|t| match t? {
        Type::Ref(TypeRef { name, params, .. })
            if Path::basename(&**name) == Some("Result") && params.len() == 2 =>
        {
            Some(params[0].clone())
        }
        Type::Set(elements) if elements.len() == 2 => {
            elements.iter().find(|elem| !matches!(elem, Type::Error(_))).cloned()
        }
        _ => None,
    })
}

pub fn extract_cast_type(resolved_typ: Option<&FnType>) -> Option<Type> {
    let typ = cast_target(&resolved_typ?.rtype)?;
    if typ.has_unbound() {
        return None;
    }
    // ⊥ has no surface syntax, so a ⊥ anywhere in the target is an
    // unconstrained cell, as unusable as an unbound one.
    fn contains_bottom(t: &Type, depth: u32) -> bool {
        if depth > 64 {
            return false;
        }
        let t = t.with_deref(|d| d.cloned()).unwrap_or_else(|| t.clone());
        match t {
            Type::Bottom => true,
            Type::Set(els) | Type::Tuple(els) | Type::Variant(_, els) => {
                els.iter().any(|e| contains_bottom(e, depth + 1))
            }
            Type::Array(e) | Type::Error(e) | Type::ByRef(e) => {
                contains_bottom(&e, depth + 1)
            }
            Type::Struct(fields) => {
                fields.iter().any(|(_, e)| contains_bottom(e, depth + 1))
            }
            Type::Map { key, value } => {
                contains_bottom(&key, depth + 1) || contains_bottom(&value, depth + 1)
            }
            _ => false,
        }
    }
    if contains_bottom(&typ, 0) {
        return None;
    }
    Some(typ)
}

/// Program arguments stored in LibState. Index 0 is the script filename.
#[derive(Default)]
pub struct ProgramArgs(pub Vec<ArcStr>);

/// Print-capture sink, seeded into `ctx.libstate` by harnesses. When
/// present, `print`/`println`/`dbg` Stdout and Stderr output appends
/// here (exactly the bytes the stream would receive) instead of the
/// process streams. Log destinations are unaffected.
#[derive(Debug, Default, Clone)]
pub struct PrintSink(pub triomphe::Arc<parking_lot::Mutex<String>>);

impl PrintSink {
    /// Take the captured text, leaving the sink empty.
    pub fn take(&self) -> String {
        std::mem::take(&mut *self.0.lock())
    }
}

/// Implement `netidx_core::pack::Pack` as a non-serializable stub.
/// Use this for abstract wrapper types that should never be encoded/decoded.
#[macro_export]
macro_rules! impl_no_pack {
    ($t:ty) => {
        impl ::netidx_core::pack::Pack for $t {
            fn encoded_len(&self) -> usize {
                0
            }

            fn encode(
                &self,
                _buf: &mut impl ::bytes::BufMut,
            ) -> Result<(), ::netidx_core::pack::PackError> {
                Err(::netidx_core::pack::PackError::Application(0))
            }

            fn decode(
                _buf: &mut impl ::bytes::Buf,
            ) -> Result<Self, ::netidx_core::pack::PackError> {
                Err(::netidx_core::pack::PackError::Application(0))
            }
        }
    };
}

/// Generates `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`, `impl_no_pack!`,
/// and the `LazyLock<AbstractWrapper<T>>` static for an abstract value type
/// whose identity is determined by `Arc::as_ptr(&self.inner)`.
#[macro_export]
macro_rules! impl_abstract_arc {
    ($name:ident, $wrapper_vis:vis static $wrapper:ident = [$($uuid:expr),* $(,)?]) => {
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                std::sync::Arc::ptr_eq(&self.inner, &other.inner)
            }
        }
        impl Eq for $name {}
        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                std::sync::Arc::as_ptr(&self.inner).addr().cmp(&std::sync::Arc::as_ptr(&other.inner).addr())
            }
        }
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::sync::Arc::as_ptr(&self.inner).hash(state)
            }
        }
        $crate::impl_no_pack!($name);
        $wrapper_vis static $wrapper: std::sync::LazyLock<
            netidx_value::abstract_type::AbstractWrapper<$name>,
        > = std::sync::LazyLock::new(|| {
            let id = uuid::Uuid::from_bytes([$($uuid),*]);
            netidx_value::Abstract::register::<$name>(id)
                .expect(concat!("failed to register ", stringify!($name)))
        });
    };
    ($name:ident, $wrapper_vis:vis static $wrapper:ident = $path:literal) => {
        $crate::impl_abstract_arc!(@identity $name);
        $crate::abstract_wrapper!($name, $wrapper_vis static $wrapper = $path);
    };
    (@identity $name:ident) => {
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                std::sync::Arc::ptr_eq(&self.inner, &other.inner)
            }
        }
        impl Eq for $name {}
        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                std::sync::Arc::as_ptr(&self.inner).addr().cmp(&std::sync::Arc::as_ptr(&other.inner).addr())
            }
        }
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                std::sync::Arc::as_ptr(&self.inner).hash(state)
            }
        }
        $crate::impl_no_pack!($name);
    };
}

/// The `LazyLock<AbstractWrapper<T>>` static for a Rust-backed abstract
/// type, registered under the UUID derived from its Graphix path
/// (`graphix_compiler::typ::abstract_uuid`), which is what makes a
/// runtime type test (`File as f`) exact.
#[macro_export]
macro_rules! abstract_wrapper {
    ($name:ty, $wrapper_vis:vis static $wrapper:ident = $path:literal) => {
        $wrapper_vis static $wrapper: std::sync::LazyLock<
            netidx_value::abstract_type::AbstractWrapper<$name>,
        > = std::sync::LazyLock::new(|| {
            netidx_value::Abstract::register::<$name>(
                ::graphix_compiler::typ::abstract_uuid($path),
            )
            .expect(concat!("failed to register ", $path))
        });
    };
}

pub mod memo;
pub mod testing;

pub use memo::FastMemo;

/// Check if a Value is a struct-shaped array: non-empty, every element is
/// a 2-element array with a string first element, keys sorted ascending.
pub fn is_struct(arr: &ValArray) -> bool {
    if arr.is_empty() {
        return false;
    }
    let mut prev: Option<&ArcStr> = None;
    for v in arr.iter() {
        match v {
            Value::Array(pair) if pair.len() == 2 => match &pair[0] {
                Value::String(k) => {
                    if let Some(p) = prev {
                        if k <= p {
                            return false;
                        }
                    }
                    prev = Some(k);
                }
                _ => return false,
            },
            _ => return false,
        }
    }
    true
}

/// The TICK view of a production at a builtin's arg seam — `Some` iff
/// this delivery is an event that advances the builtin's state. Only
/// `Fired` ticks; stale deliveries and bottoms do not.
pub fn seam_tick<'a>(tv: &'a TagValue) -> Option<&'a TagValue> {
    match tv.view() {
        TagView::Fired(tv) => Some(tv),
        TagView::Stale(_) | TagView::FreshBottom | TagView::StaleBottom => None,
    }
}

/// The VALUE view of a production at a builtin's arg seam — `Some` for
/// any value-bearing delivery (fired or stale), `None` for bottoms.
/// For config/label args whose consumption is not event counting.
pub fn seam_value<'a>(tv: &'a TagValue) -> Option<&'a TagValue> {
    match tv.view() {
        TagView::Fired(tv) | TagView::Stale(tv) => Some(tv),
        TagView::FreshBottom | TagView::StaleBottom => None,
    }
}

/// The per-arg read for raw-Apply builtins: update the arg node and
/// return `(value, fired)` — `None` for bottoms, and whether this
/// delivery is an event. Every arg must be read every cycle, so call
/// this for each of `from` unconditionally before any early return.
pub fn seam_arg<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    node: &mut Node<R, E>,
    event: &mut Event<E>,
) -> (Option<Value>, bool) {
    match seam_value(node.update(ctx, event)) {
        Some(tv) => {
            let fired = tv.is_fired();
            (Some(tv.value_cloned()), fired)
        }
        None => (None, false),
    }
}

#[derive(Debug)]
pub struct CachedVals(pub Box<[Option<Value>]>, pub Box<[Tag]>);

impl CachedVals {
    pub fn new<R: Rt, E: UserEvent>(from: &[Node<R, E>]) -> CachedVals {
        CachedVals(
            from.into_iter().map(|_| None).collect(),
            from.into_iter().map(|_| Tag::FIRED).collect(),
        )
    }

    pub fn clear(&mut self) {
        for v in &mut self.0 {
            *v = None
        }
        for t in &mut self.1 {
            *t = Tag::FIRED
        }
    }

    /// True if any arg slot holds a taint no clean production has
    /// overwritten since.
    pub fn any_tainted(&self) -> bool {
        self.1.iter().any(|t| t.is_bottom())
    }

    /// True if any arg slot is bottom — tainted or never delivered. The
    /// wrapper bottoms the invocation on this instead of calling `eval`,
    /// so builtin authors never see a bottomed or missing arg.
    pub fn any_bottom(&self) -> bool {
        self.0.iter().any(|v| v.is_none()) || self.any_tainted()
    }

    /// Update the slots from the arg nodes; `true` iff any production
    /// fired or tainted (a stale production refreshes its slot silently).
    /// A tainted production marks the slot's tag but keeps the value.
    pub fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> bool {
        self.update_full(ctx, from, event).is_some_and(|t| t.triggers())
    }

    /// [`Self::update`] with the full production summary: `None` = no
    /// production; `Some(tag)` = TAINT if any tainted, else FIRED if any
    /// fired, else STALE.
    pub fn update_full<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Tag> {
        let mut prod: Option<Tag> = None;
        for (i, src) in from.iter_mut().enumerate() {
            let tv = src.update(ctx, event);
            let tag = tv.tag();
            if tag.is_bottom() {
                self.1[i] = Tag::STALE_BOTTOM;
            } else {
                self.0[i] = Some(tv.value_cloned());
                self.1[i] = tag;
            }
            prod = Some(match prod {
                None => tag,
                Some(p) => p.join(tag),
            });
        }
        prod
    }

    pub fn flat_iter<'a>(&'a self) -> impl Iterator<Item = Option<Value>> + 'a {
        self.0.iter().flat_map(|v| match v {
            None => Either::Left(iter::once(None)),
            Some(v) => Either::Right(v.clone().flatten().map(Some)),
        })
    }

    pub fn get<T: FromValue>(&self, i: usize) -> Option<T> {
        self.0.get(i).and_then(|v| v.as_ref()).and_then(|v| v.clone().cast_to::<T>().ok())
    }
}

/// A once-per-instance latch: `take` is true the first time it is called
/// after construction or `reset`.
#[derive(Debug, Default, Clone, Copy)]
pub struct FireOnce(bool);

impl FireOnce {
    pub fn take(&mut self) -> bool {
        !std::mem::replace(&mut self.0, true)
    }

    pub fn reset(&mut self) {
        self.0 = false
    }
}

pub type ByRefChain = graphix_compiler::env::Map<BindId, BindId>;

/// Typed argument read for a fast fn (clone + cast).
pub fn fast_get<T: FromValue>(args: &[Value], i: usize) -> Option<T> {
    args.get(i).and_then(|v| v.clone().cast_to::<T>().ok())
}

/// The cached argument slots as a fast fn's `&[Value]` view; `None`
/// if any slot has never been delivered.
fn fast_args(from: &CachedVals) -> Option<LPooled<Vec<Value>>> {
    let mut args: LPooled<Vec<Value>> = LPooled::take();
    for v in from.0.iter() {
        args.push(v.as_ref()?.clone());
    }
    Some(args)
}

/// Run a builtin's fast fn over the cached argument slots —
/// the node-walk half of a fastcall builtin, so `eval` and the JIT share
/// one implementation.
pub fn fast_eval(f: FastFn, from: &CachedVals) -> Option<Value> {
    f(&fast_args(from)?)
}

/// [`fast_eval`] for a `FastCall::Typed` fn: `typ` is the call site's
/// resolved return type (`resolved.rtype` from `typecheck1`).
pub fn fast_eval_typed(
    f: TypedFastFn,
    env: &Env,
    typ: &Type,
    from: &CachedVals,
) -> Option<Value> {
    f(env, typ, &fast_args(from)?)
}

/// The sort every collection's `sort(#dir, #numeric, c)` runs: `dir`
/// is the `Direction` tag (`Ascending`/`Descending`, anything else is
/// no value), `numeric` compares values cast to f64.
pub fn sort_values(
    dir: &str,
    numeric: bool,
    vals: impl Iterator<Item = Value>,
) -> Option<LPooled<Vec<Value>>> {
    fn cn(v: &Value) -> Value {
        v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone())
    }
    let mut buf: LPooled<Vec<Value>> = vals.collect();
    match (dir, numeric) {
        ("Ascending", true) => buf.sort_by(|a, b| cn(a).cmp(&cn(b))),
        ("Ascending", false) => buf.sort(),
        ("Descending", true) => buf.sort_by(|a, b| cn(b).cmp(&cn(a))),
        ("Descending", false) => buf.sort_by(|a, b| b.cmp(a)),
        _ => return None,
    }
    Some(buf)
}

pub trait EvalCached<R: Rt, E: UserEvent>:
    Debug + Default + Send + Sync + 'static
{
    const NAME: &str;
    /// The builtin's classification — see `graphix_compiler::Effect`.
    const EFFECT: Effect = Effect::Async;

    fn init(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self::default()
    }

    fn eval(&mut self, ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value>;

    fn typecheck0(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct CachedArgs<T> {
    /// Set by `sleep()`, taken by the next update.
    woke_pending: bool,
    cached: CachedVals,
    /// The last value `eval` produced; a stale arg refresh re-surfaces
    /// it retagged STALE instead of re-running `eval`.
    last_result: TagValue,
    t: T,
}

impl<R: Rt, E: UserEvent, T: EvalCached<R, E>> BuiltIn<R, E> for CachedArgs<T> {
    const EFFECT: Effect = T::EFFECT;
    const NAME: &str = T::NAME;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let t = CachedArgs::<T> {
            woke_pending: false,
            cached: CachedVals::new(from),
            last_result: TagValue::phantom(),
            t: T::init(ctx, typ, resolved, scope, from, top_id),
        };
        Ok(Box::new(t))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCached<R, E>> Apply<R, E> for CachedArgs<T> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.woke_pending) && !ctx.in_frame();
        let (ev, cached, last_result) =
            (&mut self.t, &mut self.cached, &mut self.last_result);
        coretraits::with_value_hooks(ctx, event, move |ctx, event| {
            Self::update_inner(ev, cached, last_result, woke, ctx, from, event)
        })
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.t.typecheck0(ctx, from)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.t.typecheck1(ctx, from, resolved)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.woke_pending = true;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The arg slots are the value channel and survive replay resets.
    }
}

pub trait EvalCachedAsync: Debug + Default + Send + Sync + 'static {
    const NAME: &str;

    type Args: Debug + Any + Send + Sync;

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self::default()
    }

    /// map the final value with access to self and ctx
    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        v: Value,
    ) -> Option<Value> {
        Some(v)
    }

    fn typecheck0<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
    ) -> Result<()> {
        Ok(())
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args>;
    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send;
}

impl<T> CachedArgs<T> {
    fn update_inner<'a, R: Rt, E: UserEvent>(
        ev: &mut T,
        cached: &mut CachedVals,
        last_result: &'a mut TagValue,
        woke: bool,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &'a TagValue
    where
        T: EvalCached<R, E>,
    {
        match cached.update_full(ctx, from, event) {
            None => last_result.ride(),
            Some(t) if cached.any_bottom() => {
                // A bottom arg bottoms the invocation without calling eval.
                TagValue::bottom_null(t.triggers())
            }
            Some(t) if t.is_fired() => match ev.eval(ctx, cached) {
                Some(v) => last_result.set(TagValue::fired(v)),
                None => last_result.ride(),
            },
            Some(_) if !last_result.tag().is_bottom() => {
                // Wake catch-up: args may have drifted while asleep. A
                // stateless eval re-runs from the present slots; a
                // stateful one must not (its last result is its state).
                if T::EFFECT.is_stateless() && woke {
                    match ev.eval(ctx, cached) {
                        Some(v) => last_result.set(TagValue::stale(v)),
                        None => last_result.retag(Tag::STALE),
                    }
                } else {
                    last_result.retag(Tag::STALE)
                }
            }
            Some(_) => {
                // Nothing to re-surface yet: run eval once to establish
                // the value channel, STALE.
                match ev.eval(ctx, cached) {
                    Some(v) => last_result.set(TagValue::stale(v)),
                    None => last_result.ride(),
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct CachedArgsAsync<T: EvalCachedAsync> {
    cached: CachedVals,
    id: BindId,
    top_id: ExprId,
    queued: VecDeque<T::Args>,
    running: bool,
    out: TagValue,
    t: T,
}

impl<R: Rt, E: UserEvent, T: EvalCachedAsync> BuiltIn<R, E> for CachedArgsAsync<T> {
    const NAME: &str = T::NAME;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let t = CachedArgsAsync::<T> {
            id,
            top_id,
            cached: CachedVals::new(from),
            queued: VecDeque::new(),
            running: false,
            out: TagValue::phantom(),
            t: T::init(ctx, typ, resolved, scope, from, top_id),
        };
        Ok(Box::new(t))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCachedAsync> Apply<R, E> for CachedArgsAsync<T> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let mut bottomed = false;
        if self.cached.update(ctx, from, event) {
            if self.cached.any_bottom() {
                // A completed reply from a prior invocation still
                // wins the cycle's output.
                bottomed = true;
            } else if let Some(args) = self.t.prepare_args(&self.cached) {
                self.queued.push_back(args);
            }
        }
        let res = event.variables.remove(&self.id).and_then(|tv| {
            self.running = false;
            self.t.map_value(ctx, tv.value())
        });
        if !self.running
            && let Some(args) = self.queued.pop_front()
        {
            self.running = true;
            let id = self.id;
            ctx.rt.spawn_var(async move { (id, T::eval(args).await) });
        }
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None if bottomed => TagValue::bottom_null(true),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.t.typecheck0(ctx, from)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.t.typecheck1(ctx, from, resolved)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.queued.clear();
        self.cached.clear();
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.delete(ctx);
        self.running = false;
        self.out = TagValue::phantom();
        let id = BindId::new();
        ctx.rt.ref_var(id, self.top_id);
        self.id = id;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

fn fc_is_err(args: &[Value]) -> Option<Value> {
    match args {
        [v] => Some(Value::Bool(matches!(v, Value::Error(_)))),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct IsErrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for IsErrEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_is_err)));
    const NAME: &str = "core_is_err";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_is_err, from)
    }
}

type IsErr = CachedArgs<IsErrEv>;

#[derive(Debug, Default)]
struct FilterErr {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for FilterErr {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_filter_err";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(FilterErr::default()))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for FilterErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match seam_tick(from[0].update(ctx, event)).and_then(|tv| {
            match tv.value_cloned() {
                v @ Value::Error(_) => Some(v),
                _ => None,
            }
        }) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

fn fc_error(args: &[Value]) -> Option<Value> {
    Some(Value::Error(args[0].clone().into()))
}

#[derive(Debug, Default)]
struct ToErrorEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ToErrorEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_error)));
    const NAME: &str = "core_error";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_error, from)
    }
}

type ToError = CachedArgs<ToErrorEv>;

#[derive(Debug)]
struct Once {
    val: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_once";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Once { val: false, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Once {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let res = match from {
            [s] => seam_tick(s.update(ctx, event)).and_then(|tv| {
                if self.val {
                    None
                } else {
                    self.val = true;
                    Some(tv.value_cloned())
                }
            }),
            _ => None,
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.val = false
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Once per lifetime, not once per frame; only sleep restarts it.
    }
}

#[derive(Debug)]
struct Take {
    n: Option<usize>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Take {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_take";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Take { n: None, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Take {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        // Seed the countdown on a tick only: a stale ride of #n must
        // not clobber the running count.
        if let Some(n) = seam_tick(from[0].update(ctx, event))
            .and_then(|tv| tv.value_cloned().cast_to::<usize>().ok())
        {
            self.n = Some(n)
        }
        let res =
            seam_tick(from[1].update(ctx, event)).and_then(|tv| match &mut self.n {
                None => None,
                Some(n) if *n > 0 => {
                    *n -= 1;
                    Some(tv.value_cloned())
                }
                Some(_) => None,
            });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.n = None
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The countdown spans the node's lifetime; only sleep restarts it.
    }
}

#[derive(Debug)]
struct Skip {
    n: Option<usize>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Skip {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_skip";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Skip { n: None, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Skip {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        // Seed the countdown on a tick only: a stale ride of #n must
        // not clobber the running count.
        if let Some(n) = seam_tick(from[0].update(ctx, event))
            .and_then(|tv| tv.value_cloned().cast_to::<usize>().ok())
        {
            self.n = Some(n)
        }
        let res =
            seam_tick(from[1].update(ctx, event)).and_then(|tv| match &mut self.n {
                None => Some(tv.value_cloned()),
                Some(n) if *n > 0 => {
                    *n -= 1;
                    None
                }
                Some(_) => Some(tv.value_cloned()),
            });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.n = None
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The countdown spans the node's lifetime; only sleep restarts it.
    }
}

fn fc_all(args: &[Value]) -> Option<Value> {
    match args {
        [] => None,
        [hd, tl @ ..] => {
            if tl.iter().all(|v1| v1 == hd) {
                Some(hd.clone())
            } else {
                None
            }
        }
    }
}

#[derive(Debug, Default)]
struct AllEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for AllEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_all)));
    const NAME: &str = "core_all";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_all, from)
    }
}

type All = CachedArgs<AllEv>;

fn add_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l + r),
    }
}

#[derive(Debug, Default)]
struct SumEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for SumEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_sum";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => add_vals(res, v.clone()),
        })
    }
}

type Sum = CachedArgs<SumEv>;

#[derive(Debug, Default)]
struct ProductEv;

fn prod_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l * r),
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ProductEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_product";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => prod_vals(res, v.clone()),
        })
    }
}

type Product = CachedArgs<ProductEv>;

#[derive(Debug, Default)]
struct DivideEv;

fn div_vals(lhs: Option<Value>, rhs: Option<Value>) -> Option<Value> {
    match (lhs, rhs) {
        (None, None) | (Some(_), None) => None,
        (None, r @ Some(_)) => r,
        (Some(l), Some(r)) => Some(l / r),
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for DivideEv {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_divide";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        from.flat_iter().fold(None, |res, v| match res {
            res @ Some(Value::Error(_)) => res,
            res => div_vals(res, v.clone()),
        })
    }
}

type Divide = CachedArgs<DivideEv>;

#[derive(Debug, Default)]
struct MinEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MinEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_min";

    // Each argument is compared as a whole value; no flattening, as
    // the declared type `fn(a: 'a, @args: 'a) -> 'a` promises.
    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res: Option<&Value> = None;
        for v in from.0.iter() {
            match (res, v) {
                (_, None) => return None,
                (None, Some(v)) => res = Some(v),
                (Some(v0), Some(v)) => {
                    if v < v0 {
                        res = Some(v)
                    }
                }
            }
        }
        res.cloned()
    }
}

type Min = CachedArgs<MinEv>;

#[derive(Debug, Default)]
struct MaxEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MaxEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_max";

    // Whole-value comparison, no flattening — see `MinEv`.
    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res: Option<&Value> = None;
        for v in from.0.iter() {
            match (res, v) {
                (_, None) => return None,
                (None, Some(v)) => res = Some(v),
                (Some(v0), Some(v)) => {
                    if v > v0 {
                        res = Some(v)
                    }
                }
            }
        }
        res.cloned()
    }
}

type Max = CachedArgs<MaxEv>;

#[derive(Debug, Default)]
struct AndEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for AndEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_and";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(true));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => (),
                Some(_) => {
                    res = Some(Value::Bool(false));
                }
            }
        }
        res
    }
}

type And = CachedArgs<AndEv>;

#[derive(Debug, Default)]
struct OrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for OrEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res = Some(Value::Bool(false));
        for v in from.flat_iter() {
            match v {
                None => return None,
                Some(Value::Bool(true)) => {
                    res = Some(Value::Bool(true));
                }
                Some(_) => (),
            }
        }
        res
    }
}

type Or = CachedArgs<OrEv>;

macro_rules! int_binop {
    ($l:expr, $r:expr, $op:tt) => {
        match ($l, $r) {
            (Value::U8(l), Value::U8(r)) => Some(Value::U8(l $op r)),
            (Value::I8(l), Value::I8(r)) => Some(Value::I8(l $op r)),
            (Value::U16(l), Value::U16(r)) => Some(Value::U16(l $op r)),
            (Value::I16(l), Value::I16(r)) => Some(Value::I16(l $op r)),
            (Value::U32(l), Value::U32(r)) => Some(Value::U32(l $op r)),
            (Value::V32(l), Value::V32(r)) => Some(Value::V32(l $op r)),
            (Value::I32(l), Value::I32(r)) => Some(Value::I32(l $op r)),
            (Value::Z32(l), Value::Z32(r)) => Some(Value::Z32(l $op r)),
            (Value::U64(l), Value::U64(r)) => Some(Value::U64(l $op r)),
            (Value::V64(l), Value::V64(r)) => Some(Value::V64(l $op r)),
            (Value::I64(l), Value::I64(r)) => Some(Value::I64(l $op r)),
            (Value::Z64(l), Value::Z64(r)) => Some(Value::Z64(l $op r)),
            _ => None,
        }
    };
}

macro_rules! int_shift {
    ($l:expr, $r:expr, $method:ident) => {
        match ($l, $r) {
            (Value::U8(l), Value::U8(r)) => Some(Value::U8(l.$method(*r as u32))),
            (Value::I8(l), Value::I8(r)) => Some(Value::I8(l.$method(*r as u32))),
            (Value::U16(l), Value::U16(r)) => Some(Value::U16(l.$method(*r as u32))),
            (Value::I16(l), Value::I16(r)) => Some(Value::I16(l.$method(*r as u32))),
            (Value::U32(l), Value::U32(r)) => Some(Value::U32(l.$method(*r as u32))),
            (Value::V32(l), Value::V32(r)) => Some(Value::V32(l.$method(*r as u32))),
            (Value::I32(l), Value::I32(r)) => Some(Value::I32(l.$method(*r as u32))),
            (Value::Z32(l), Value::Z32(r)) => Some(Value::Z32(l.$method(*r as u32))),
            (Value::U64(l), Value::U64(r)) => Some(Value::U64(l.$method(*r as u32))),
            (Value::V64(l), Value::V64(r)) => Some(Value::V64(l.$method(*r as u32))),
            (Value::I64(l), Value::I64(r)) => Some(Value::I64(l.$method(*r as u32))),
            (Value::Z64(l), Value::Z64(r)) => Some(Value::Z64(l.$method(*r as u32))),
            _ => None,
        }
    };
}

fn fc_bit_and(args: &[Value]) -> Option<Value> {
    int_binop!(&args[0], &args[1], &)
}

fn fc_bit_or(args: &[Value]) -> Option<Value> {
    int_binop!(&args[0], &args[1], |)
}

fn fc_bit_xor(args: &[Value]) -> Option<Value> {
    int_binop!(&args[0], &args[1], ^)
}

fn fc_shl(args: &[Value]) -> Option<Value> {
    int_shift!(&args[0], &args[1], wrapping_shl)
}

fn fc_shr(args: &[Value]) -> Option<Value> {
    int_shift!(&args[0], &args[1], wrapping_shr)
}

#[derive(Debug, Default)]
struct BitAndEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitAndEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bit_and)));
    const NAME: &str = "core_bit_and";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_and, from)
    }
}

type BitAnd = CachedArgs<BitAndEv>;

#[derive(Debug, Default)]
struct BitOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitOrEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bit_or)));
    const NAME: &str = "core_bit_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_or, from)
    }
}

type BitOr = CachedArgs<BitOrEv>;

#[derive(Debug, Default)]
struct BitXorEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitXorEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bit_xor)));
    const NAME: &str = "core_bit_xor";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_xor, from)
    }
}

type BitXor = CachedArgs<BitXorEv>;

fn fc_bit_not(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::U8(v) => Some(Value::U8(!v)),
        Value::I8(v) => Some(Value::I8(!v)),
        Value::U16(v) => Some(Value::U16(!v)),
        Value::I16(v) => Some(Value::I16(!v)),
        Value::U32(v) => Some(Value::U32(!v)),
        Value::V32(v) => Some(Value::V32(!v)),
        Value::I32(v) => Some(Value::I32(!v)),
        Value::Z32(v) => Some(Value::Z32(!v)),
        Value::U64(v) => Some(Value::U64(!v)),
        Value::V64(v) => Some(Value::V64(!v)),
        Value::I64(v) => Some(Value::I64(!v)),
        Value::Z64(v) => Some(Value::Z64(!v)),
        _ => None,
    }
}

#[derive(Debug, Default)]
struct BitNotEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitNotEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_bit_not)));
    const NAME: &str = "core_bit_not";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_not, from)
    }
}

type BitNot = CachedArgs<BitNotEv>;

#[derive(Debug, Default)]
struct ShlEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ShlEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_shl)));
    const NAME: &str = "core_shl";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_shl, from)
    }
}

type Shl = CachedArgs<ShlEv>;

#[derive(Debug, Default)]
struct ShrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ShrEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_shr)));
    const NAME: &str = "core_shr";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_shr, from)
    }
}

type Shr = CachedArgs<ShrEv>;

/// Feeds each input value to `pred` and emits it when `pred` returns
/// `true`. A new input arriving while `pred` is still working replaces
/// the pending value; wrap with `queue` for strict pairing.
#[derive(Debug)]
struct Filter<R: Rt, E: UserEvent> {
    pred: Node<R, E>,
    pending: Option<Value>,
    fid: BindId,
    x: BindId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Filter<R, E> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_filter";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                let (x, xn) =
                    genn::bind(ctx, &scope.lexical, "x", typ.args[0].typ.clone(), top_id);
                let fid = BindId::new();
                let ptyp = match &typ.args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                };
                let fnode = genn::reference(ctx, fid, Type::Fn(ptyp.clone()), top_id);
                let pred = genn::apply(
                    fnode,
                    scope.clone(),
                    smallvec::smallvec![xn],
                    &ptyp,
                    top_id,
                );
                Ok(Box::new(Self {
                    pred,
                    pending: None,
                    fid,
                    x,
                    out: TagValue::phantom(),
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Filter<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if let Some(tv) = seam_value(from[1].update(ctx, event)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            ctx.rt.store_insert(self.fid, TagValue::fired(v.clone()));
            event.variables.insert(self.fid, TagValue::tagged(v, tag));
        }
        if let Some(tv) = seam_value(from[0].update(ctx, event)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            self.pending = Some(v.clone());
            ctx.rt.store_insert(self.x, TagValue::fired(v.clone()));
            event.variables.insert(self.x, TagValue::tagged(v, tag));
        }
        let res = seam_tick(self.pred.update(ctx, event)).and_then(|b| {
            match b.value_cloned() {
                Value::Bool(true) => self.pending.clone(),
                _ => None,
            }
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.store_remove(&self.fid);
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pending = None;
        self.pred.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pending = None;
        self.pred.reset_replay(ctx);
    }
}

#[derive(Debug)]
struct Queue {
    triggered: usize,
    queue: VecDeque<Value>,
    id: BindId,
    top_id: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Queue {
    const NAME: &str = "core_queue";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let id = BindId::new();
                ctx.rt.ref_var(id, top_id);
                Ok(Box::new(Self {
                    triggered: 0,
                    queue: VecDeque::new(),
                    id,
                    top_id,
                    out: TagValue::phantom(),
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Queue {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx, event)).is_some() {
            self.triggered += 1;
        }
        if let Some(tv) = seam_tick(from[1].update(ctx, event)) {
            self.queue.push_back(tv.value_cloned());
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            self.triggered -= 1;
            ctx.rt.set_var(self.id, self.queue.pop_front().unwrap());
        }
        match event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.triggered = 0;
        self.queue.clear();
        self.out = TagValue::phantom();
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Hold {
    triggered: usize,
    current: Option<Value>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Hold {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_hold";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => Ok(Box::new(Self {
                triggered: 0,
                current: None,
                out: TagValue::phantom(),
            })),
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Hold {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx, event)).is_some() {
            self.triggered += 1;
        }
        if let Some(tv) = seam_tick(from[1].update(ctx, event)) {
            self.current = Some(tv.value_cloned());
        }
        if self.triggered > 0
            && let Some(v) = self.current.take()
        {
            self.triggered -= 1;
            self.out.set(TagValue::fired(v))
        } else {
            self.out.ride()
        }
    }

    fn delete(&mut self, _: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _: &mut ExecCtx<R, E>) {
        self.triggered = 0;
        self.current = None;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Seq {
    id: BindId,
    top_id: ExprId,
    args: CachedVals,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Seq {
    const NAME: &str = "core_seq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let args = CachedVals::new(from);
        Ok(Box::new(Self { id, top_id, args, out: TagValue::phantom() }))
    }
}

/// Each element of a range is one queued set_var, so the range is
/// capped at `MAX_ARRAY_INIT_LEN` elements.
fn range_len_exceeds_cap(i: i64, j: i64) -> bool {
    j as i128 - i as i128 > graphix_compiler::node::MAX_ARRAY_INIT_LEN as i128
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Seq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if self.args.update(ctx, from, event) {
            let err = match &self.args.0[..] {
                [Some(Value::I64(i)), Some(Value::I64(j))] if i <= j => {
                    let e = literal!("RangeError");
                    if range_len_exceeds_cap(*i, *j) {
                        Some(errf!(
                            e,
                            "seq range {i}..{j} exceeds the {} element limit",
                            graphix_compiler::node::MAX_ARRAY_INIT_LEN
                        ))
                    } else {
                        for v in *i..*j {
                            ctx.rt.set_var(self.id, Value::I64(v));
                        }
                        None
                    }
                }
                _ => {
                    let e = literal!("RangeError");
                    Some(err!(e, "invalid args i must be <= j"))
                }
            };
            if let Some(e) = err {
                return self.out.set(TagValue::fired(e));
            }
        }
        match event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.out = TagValue::phantom();
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    last: Option<Instant>,
    tid: Option<BindId>,
    top_id: ExprId,
    /// The latest value of the throttled arg, emitted when the timer
    /// fires.
    last_v: Option<Value>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Throttle {
    const NAME: &str = "core_throttle";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self {
            wait: Duration::ZERO,
            last: None,
            tid: None,
            top_id,
            last_v: None,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Throttle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        macro_rules! emit_cached {
            () => {{
                match self.last_v.clone() {
                    Some(v) => return self.out.set(TagValue::fired(v)),
                    None => return self.out.ride(),
                }
            }};
        }
        macro_rules! maybe_schedule {
            ($last:expr) => {{
                let now = Instant::now();
                if now - *$last >= self.wait {
                    *$last = now;
                    emit_cached!()
                } else {
                    let id = BindId::new();
                    ctx.rt.ref_var(id, self.top_id);
                    ctx.rt.set_timer(id, self.wait - (now - *$last));
                    self.tid = Some(id);
                    return self.out.ride();
                }
            }};
        }
        // A fired duration retunes the wait; any value-bearing delivery
        // of the throttled arg lands in `last_v`, but only a fired one
        // is an event to throttle.
        let new_wait = match seam_value(from[0].update(ctx, event)) {
            Some(tv) if tv.is_fired() => tv.with_value(|v| match v {
                Value::Duration(d) => Some(**d),
                _ => None,
            }),
            _ => None,
        };
        let mut up1 = false;
        if let Some(tv) = seam_value(from[1].update(ctx, event)) {
            up1 = tv.is_fired();
            self.last_v = Some(tv.value_cloned());
        }
        if let Some(d) = new_wait {
            self.wait = d;
            if let Some(id) = self.tid.take()
                && let Some(last) = &mut self.last
            {
                ctx.rt.unref_var(id, self.top_id);
                maybe_schedule!(last)
            }
        }
        if up1 && self.tid.is_none() {
            match &mut self.last {
                Some(last) => maybe_schedule!(last),
                None => {
                    self.last = Some(Instant::now());
                    emit_cached!()
                }
            }
        }
        if let Some(id) = self.tid
            && let Some(_) = event.variables.get(&id)
        {
            ctx.rt.unref_var(id, self.top_id);
            self.tid = None;
            self.last = Some(Instant::now());
            emit_cached!()
        }
        self.out.ride()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.tid.take() {
            ctx.rt.unref_var(id, self.top_id);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.delete(ctx);
        self.last = None;
        self.wait = Duration::ZERO;
        self.last_v = None;
        self.out = TagValue::phantom();
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Count {
    count: i64,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Count {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_count";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Count { count: 0, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Count {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if from
            .into_iter()
            .fold(false, |u, n| u || seam_tick(n.update(ctx, event)).is_some())
        {
            self.count += 1;
            self.out.set(TagValue::fired(Value::I64(self.count)))
        } else {
            self.out.ride()
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.count = 0
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Default)]
struct MeanEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MeanEv {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_mean";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        static TAG: ArcStr = literal!("MeanError");
        let mut total = 0.;
        let mut samples = 0;
        let mut error = None;
        for v in from.flat_iter() {
            if let Some(v) = v {
                match v.cast_to::<f64>() {
                    Err(e) => error = Some(errf!(TAG, "{e:?}")),
                    Ok(v) => {
                        total += v;
                        samples += 1;
                    }
                }
            }
        }
        if let Some(e) = error {
            Some(e)
        } else if samples == 0 {
            Some(err!(TAG, "mean requires at least one argument"))
        } else {
            Some(Value::F64(total / samples as f64))
        }
    }
}

type Mean = CachedArgs<MeanEv>;

#[derive(Debug)]
struct Uniq(Option<Value>, TagValue);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Uniq {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_uniq";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Uniq(None, TagValue::phantom())))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Uniq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let (last, out) = (&mut self.0, &mut self.1);
        coretraits::with_value_hooks(ctx, event, |ctx, event| {
            let res = seam_tick(from[0].update(ctx, event)).and_then(|tv| {
                let v = tv.value_cloned();
                if Some(&v) != last.as_ref() {
                    *last = Some(v.clone());
                    Some(v)
                } else {
                    None
                }
            });
            match res {
                Some(v) => out.set(TagValue::fired(v)),
                None => out.ride(),
            }
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.0 = None
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Clone, Copy)]
enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl FromValue for Level {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Trace" => Ok(Self::Trace),
            "Debug" => Ok(Self::Debug),
            "Info" => Ok(Self::Info),
            "Warn" => Ok(Self::Warn),
            "Error" => Ok(Self::Error),
            v => bail!("invalid log level {v}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum LogDest {
    Stdout,
    Stderr,
    Log(Level),
}

impl FromValue for LogDest {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.clone().cast_to::<ArcStr>()? {
            "Stdout" => Ok(Self::Stdout),
            "Stderr" => Ok(Self::Stderr),
            _ => Ok(Self::Log(v.cast_to()?)),
        }
    }
}

#[derive(Debug)]
struct Dbg {
    spec: Expr,
    dest: LogDest,
    typ: Type,
    buf: String,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Dbg {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_dbg";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Dbg {
            spec: from[1].spec().clone(),
            dest: LogDest::Stderr,
            typ: Type::Bottom,
            buf: String::new(),
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Dbg {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if let Some(v) =
            seam_value(from[0].update(ctx, event)).map(|tv| tv.value_cloned())
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        let Some(v) = seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
        else {
            return self.out.ride();
        };
        self.buf.clear();
        write!(self.buf, "{} dbg({}): ", self.spec.pos, self.spec).unwrap();
        let (buf, typ) = (&mut self.buf, &self.typ);
        coretraits::with_value_hooks(ctx, event, |ctx, _| {
            write!(buf, "{}", TVal { env: &ctx.env, typ, v: &v }).unwrap()
        });
        emit_line(ctx, self.dest, &self.buf, "\n");
        self.out.set(TagValue::fired(v))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typecheck0(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.typ = from[1].typ().clone();
        Ok(())
    }
}

/// Where a print builtin's output goes this cycle, and the line it
/// writes there.
fn emit_line<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    dest: LogDest,
    line: &str,
    suffix: &str,
) {
    let sink = match dest {
        LogDest::Stdout | LogDest::Stderr => ctx.libstate.get::<PrintSink>().cloned(),
        LogDest::Log(_) => None,
    };
    match (dest, sink) {
        (LogDest::Stdout | LogDest::Stderr, Some(sink)) => {
            let mut out = sink.0.lock();
            out.push_str(line);
            out.push_str(suffix);
        }
        (LogDest::Stdout, None) => print!("{line}{suffix}"),
        (LogDest::Stderr, None) => eprint!("{line}{suffix}"),
        (LogDest::Log(lvl), _) => match lvl {
            Level::Trace => log::trace!("{line}"),
            Level::Debug => log::debug!("{line}"),
            Level::Info => log::info!("{line}"),
            Level::Warn => log::warn!("{line}"),
            Level::Error => log::error!("{line}"),
        },
    }
}

#[derive(Debug)]
struct Log {
    scope: Scope,
    dest: LogDest,
    buf: String,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Log {
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_log";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _resolved: Option<&'d FnType>,
        scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self {
            scope: scope.clone(),
            dest: LogDest::Stdout,
            buf: String::new(),
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Log {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        if let Some(v) =
            seam_value(from[0].update(ctx, event)).map(|tv| tv.value_cloned())
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        let Some(v) = seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
        else {
            return self.out.ride();
        };
        self.buf.clear();
        write!(self.buf, "{}: ", self.scope.lexical).unwrap();
        let typ = from[1].typ().clone();
        let buf = &mut self.buf;
        coretraits::with_value_hooks(ctx, event, |ctx, _| {
            write!(buf, "{}", TVal { env: &ctx.env, typ: &typ, v: &v }).unwrap()
        });
        emit_line(ctx, self.dest, &self.buf, "\n");
        self.out.set(TagValue::fired(Value::Null))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

macro_rules! printfn {
    ($type:ident, $name:literal, $suffix:literal) => {
        #[derive(Debug)]
        struct $type {
            dest: LogDest,
            buf: String,
            out: TagValue,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $type {
            const EFFECT: Effect = Effect::Sync;
            const NAME: &str = $name;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a graphix_compiler::typ::FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                _from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new(Self {
                    dest: LogDest::Stdout,
                    buf: String::new(),
                    out: TagValue::phantom(),
                }))
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $type {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> &TagValue {
                if let Some(v) =
                    seam_value(from[0].update(ctx, event)).map(|tv| tv.value_cloned())
                    && let Ok(d) = v.cast_to::<LogDest>()
                {
                    self.dest = d;
                }
                let Some(v) =
                    seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
                else {
                    return self.out.ride();
                };
                self.buf.clear();
                let typ = from[1].typ().clone();
                let buf = &mut self.buf;
                coretraits::with_value_hooks(ctx, event, |ctx, _| {
                    match &v {
                        Value::String(s) => write!(buf, "{s}"),
                        v => write!(buf, "{}", TVal { env: &ctx.env, typ: &typ, v }),
                    }
                    .unwrap()
                });
                emit_line(ctx, self.dest, &self.buf, $suffix);
                self.out.set(TagValue::fired(Value::Null))
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

            fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
        }
    };
}

printfn!(Print, "core_print", "");
printfn!(Println, "core_println", "\n");

/// `array::len` — registered here (the array package binds the name)
/// because core's `Collection` implementation for `Array` needs it.
#[derive(Debug, Default)]
struct ArrayLenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ArrayLenEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(array_len)));
    const NAME: &str = "core_array_len";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(array_len, from)
    }
}

fn array_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a)] => Some(Value::I64(a.len() as i64)),
        _ => None,
    }
}

type ArrayLen = CachedArgs<ArrayLenEv>;

/// `map::len` — registered here for the `Collection` implementation
/// for `Map`; the map package binds the name.
#[derive(Debug, Default)]
struct MapLenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MapLenEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(map_len)));
    const NAME: &str = "core_map_len";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(map_len, from)
    }
}

fn map_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Map(m)] => Some(Value::I64(m.len() as i64)),
        _ => None,
    }
}

type MapLen = CachedArgs<MapLenEv>;

/// `map::union` — the union of two maps, the second's value on a key in
/// both. In core for `Collection::flat_map` over `Map`.
fn fc_map_union(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(a), Value::Map(b)) => {
            Some(Value::Map(a.union(b, |_, _, v| Some(v.clone()))))
        }
        _ => None,
    }
}

#[derive(Debug, Default)]
struct MapUnionEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MapUnionEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_map_union)));
    const NAME: &str = "core_map_union";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_map_union, from)
    }
}

type MapUnion = CachedArgs<MapUnionEv>;

graphix_derive::defpackage! {
    builtins => [
        ArrayLen,
        MapLen,
        MapUnion,
        IsErr,
        FilterErr,
        ToError,
        Once,
        Take,
        Skip,
        All,
        Sum,
        Product,
        Divide,
        Min,
        Max,
        And,
        Or,
        BitAnd,
        BitOr,
        BitXor,
        BitNot,
        Shl,
        Shr,
        Filter as Filter<GXRt<X>, X::UserEvent>,
        Queue,
        queuefn::QueueFn as queuefn::QueueFn<GXRt<X>, X::UserEvent>,
        Hold,
        Seq,
        Throttle,
        Count,
        Mean,
        Uniq,
        Dbg,
        Log,
        Print,
        Println,
        buffer::BytesToString,
        buffer::BytesToStringLossy,
        buffer::BytesFromString,
        buffer::BytesConcat,
        buffer::BytesToArray,
        buffer::BytesFromArray,
        buffer::BytesLen,
        buffer::BufferEncode,
        buffer::BufferDecode,
        math::MathSin,
        math::MathCos,
        math::MathTan,
        math::MathAsin,
        math::MathAcos,
        math::MathAtan,
        math::MathAtan2,
        math::MathSinh,
        math::MathCosh,
        math::MathTanh,
        math::MathAsinh,
        math::MathAcosh,
        math::MathAtanh,
        math::MathExp,
        math::MathExp2,
        math::MathExpM1,
        math::MathLn,
        math::MathLn1p,
        math::MathLog2,
        math::MathLog10,
        math::MathLog,
        math::MathPow,
        math::MathSqrt,
        math::MathCbrt,
        math::MathHypot,
        math::MathFloor,
        math::MathCeil,
        math::MathRound,
        math::MathTrunc,
        math::MathFract,
        math::MathAbs,
        math::MathSignum,
        math::MathCopysign,
        math::MathMin,
        math::MathMax,
        math::MathClamp,
        math::MathIsNan,
        math::MathIsFinite,
        math::MathIsInfinite,
        math::MathToDegrees,
        math::MathToRadians,
        opt::IsSome,
        opt::IsNone,
        opt::Contains,
        opt::OrNever,
        opt::OrDefault,
        opt::Or,
        opt::And,
        opt::Xor,
        opt::OkOr,
        opt::Zip,
        opt::Unzip,
        opt::OptMap as opt::OptMap<GXRt<X>, X::UserEvent>,
        opt::OptFlatMap as opt::OptFlatMap<GXRt<X>, X::UserEvent>,
        opt::OptFilter as opt::OptFilter<GXRt<X>, X::UserEvent>,
        opt::OptOrElse as opt::OptOrElse<GXRt<X>, X::UserEvent>,
        opt::OptOkOrElse as opt::OptOkOrElse<GXRt<X>, X::UserEvent>,
        opt::OptIsSomeAnd as opt::OptIsSomeAnd<GXRt<X>, X::UserEvent>,
        opt::OptIsNoneOr as opt::OptIsNoneOr<GXRt<X>, X::UserEvent>,
    ],
}

/// Embedder-provided netidx configuration for `sys::net` (and any
/// other library that wants netidx), seeded into `ctx.libstate` before
/// package registration. Absent → `Internal`.
#[derive(Debug, Clone)]
pub enum NetConfig {
    /// Use these pre-built handles.
    Ready {
        publisher: netidx::publisher::Publisher,
        subscriber: netidx::subscriber::Subscriber,
    },
    /// Build from a netidx config + auth on first use.
    Config {
        config: netidx::config::Config,
        auth: netidx::publisher::DesiredAuth,
        bind: Option<netidx::publisher::BindCfg>,
    },
    /// Process-internal netidx (resolver + pub/sub) built on demand.
    Internal,
}

/// Optional embedder-seeded netidx tuning. `publish` bounds the publish
/// flusher's batch commit: a subscriber that doesn't consume updates
/// within the timeout is dropped; None (the default) waits.
#[derive(Debug, Clone)]
pub struct NetTimeouts {
    pub publish: Option<std::time::Duration>,
}
