#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    Apply, BindId, BuiltIn, Event, ExecCtx, FastFn, Node, Refs, Rt, Scope, Tag, TagValue,
    TagView, UserEvent,
    effects::EffectKind,
    err, errf,
    expr::{Expr, ExprId},
    node::{coretraits, genn},
    typ::{FnType, TVal, Type, TypeRef},
};
use graphix_rt::GXRt;
use netidx::{path::Path, subscriber::Value};
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

// ── Cast context for typed deserialization ────────────────────────

/// Extract the success type from a resolved `Result<T, E>` return type.
/// Returns `None` if `resolved_typ` is absent or `T` contains free tvars.
pub fn extract_cast_type(resolved_typ: Option<&FnType>) -> Option<Type> {
    let ft = resolved_typ?;
    let typ = match &ft.rtype {
        Type::Ref(TypeRef { name, params, .. })
            if Path::basename(&**name) == Some("Result") && params.len() == 2 =>
        {
            params[0].clone()
        }
        // Handle the expanded form [T, Error<E>] — this occurs when the
        // Result type alias was expanded during TVar binding in contains().
        Type::Set(elements) if elements.len() == 2 => {
            let mut success = None;
            for elem in elements.iter() {
                if !matches!(elem, Type::Error(_)) {
                    success = Some(elem.clone());
                }
            }
            success?
        }
        _ => return None,
    };
    if typ.has_unbound() {
        return None;
    }
    // A ⊥-settled target is just as unusable as an unbound one: ⊥
    // means "nothing ever constrained this cell" (never-as-Bottom's
    // terminal settle), so there is no type to DIRECT the
    // deserialization — and the value WOULD flow at runtime, laundering
    // it under the never-arrives type into positions that trust the
    // type system completely. Reject → the builtin's "type must be
    // known, annotations needed" error, exactly as for unbound. The
    // artifact also arrives as a ⊥ MEMBER of a set: a collection
    // callback's cell aliasing left `str::parse`'s target as the whole
    // `[⊥, Error<ParseError>]` union, which a top-level-only check
    // missed — Bottom has no surface syntax, so a ⊥ member is always a
    // settle artifact, never an annotated target (soak-jul14b 000003).
    // The walk is RECURSIVE through set members (jul16g divergence
    // 000000: a nested-map callback's artifact arrived as
    // `[[⊥, Error<ParseError>], Error<ParseError>]` — the ⊥ one level
    // inside a set MEMBER, which the one-level check accepted; the
    // fused parse then cast through the garbage union while the
    // interp's runtime slot instance erred). The artifact also nests
    // inside COMPOSITE constructors: an unconstrained parse return
    // settled as `Array<⊥>` under one unification order and stayed
    // open under another, turning compile acceptance into a
    // per-process coin flip (aug04d2 divergence_000000 — the Set-only
    // walk accepted the Array form; Eric's ruling: compile-reject, no
    // question). ⊥ anywhere in a cast target is a settle artifact for
    // the same reason a ⊥ member is — no surface syntax can name it.
    // Depth-capped against pathological recursive shapes.
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

// ── Program arguments ─────────────────────────────────────────────

/// Program arguments stored in LibState. Index 0 is the script filename.
#[derive(Default)]
pub struct ProgramArgs(pub Vec<ArcStr>);

/// Print-capture sink, seeded into `ctx.libstate` by harnesses (the
/// differential fuzzer's stdout oracle). When present, the print
/// family's (`print`/`println`/`dbg`) Stdout AND Stderr destinations
/// append here instead of the process streams — per-runtime capture
/// that stays correct when two modes run concurrently in one process.
/// Log destinations are unaffected. Each emission appends exactly the
/// bytes the process stream would have received.
#[derive(Debug, Default, Clone)]
pub struct PrintSink(pub triomphe::Arc<parking_lot::Mutex<String>>);

impl PrintSink {
    /// Take the captured text, leaving the sink empty.
    pub fn take(&self) -> String {
        std::mem::take(&mut *self.0.lock())
    }
}

// ── Shared macros ──────────────────────────────────────────────────

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
/// type, registered under the UUID DERIVED FROM ITS GRAPHIX PATH
/// (`graphix_compiler::typ::abstract_uuid`). That derivation is what
/// makes a runtime type test (`File as f`) exact for a type whose
/// values Rust mints: the compiler knows the type's identity from its
/// path alone, so it can recognize the value without the package
/// telling it anything (`design/nominal_abstract_types.md`).
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

// ── Testing infrastructure ─────────────────────────────────────────

pub mod testing;

// ── Shared helpers ────────────────────────────────────────────────

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

// ── Shared traits and structs ──────────────────────────────────────

/// The TICK view of a production at a builtin's arg seam — `Some` iff
/// this delivery is a consumable EVENT (one that advances the
/// builtin's state machine: burns `once`'s shot, counts in `count`,
/// consumes a `take`, emits a print). Only `Fired` ticks — a stale
/// delivery is the value channel, not an event, and bottoms never
/// tick (a bottom is no event and no value at a builtin seam, per the
/// Q1 ruling).
pub fn seam_tick<'a>(tv: &'a TagValue) -> Option<&'a TagValue> {
    match tv.view() {
        TagView::Fired(tv) => Some(tv),
        TagView::Stale(_) | TagView::FreshBottom | TagView::StaleBottom => None,
    }
}

/// The VALUE view of a production at a builtin's arg seam — `Some` for
/// any value-bearing delivery (fired or stale), `None` for bottoms.
/// For config/label args (`throttle`'s duration, `take`'s `#n`, a
/// print destination) whose consumption is value-plane tracking rather
/// than event counting: dense and sparse agree, so it takes no gate.
pub fn seam_value<'a>(tv: &'a TagValue) -> Option<&'a TagValue> {
    match tv.view() {
        TagView::Fired(tv) | TagView::Stale(tv) => Some(tv),
        TagView::FreshBottom | TagView::StaleBottom => None,
    }
}

/// The per-arg dense read for raw-Apply builtins tracking their own
/// designated state (subscriptions, queues, listeners): update the arg
/// node and return `(value, fired)` — the production's value channel
/// (`None` for bottoms: a bottom is no event and no value at a builtin
/// seam, per the Q1 ruling) and whether this delivery is an EVENT
/// (fired; bottoms never tick). Every arg must be read every cycle, so
/// call this for each of `from` unconditionally before any early
/// return.
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

    /// True if any arg slot currently holds a taint (a poisoned value
    /// event arrived and no clean production has overwritten it since
    /// — the kernel's per-slot taint bit).
    pub fn any_tainted(&self) -> bool {
        self.1.iter().any(|t| t.is_tainted())
    }

    /// The Q1 wrapper-seam test (design/dense_delivery.md, BOTTOM
    /// PROPAGATES): true if any arg slot is currently BOTTOM — either
    /// poisoned at rest (the taint mark) or never delivered at all
    /// (the phantom). The wrapper bottoms the invocation on this
    /// instead of calling `eval`, so builtin authors never see a
    /// bottomed or missing arg.
    pub fn any_bottom(&self) -> bool {
        self.0.iter().any(|v| v.is_none()) || self.any_tainted()
    }

    /// Update the slots from the arg nodes; `true` iff any production
    /// TRIGGERED (fired or tainted — a merely-stale production
    /// refreshes its slot silently). A tainted production marks the
    /// slot's tag but keeps the previous (helper-safe) value.
    pub fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> bool {
        self.update_full(ctx, from, event).is_some_and(|t| t.triggers())
    }

    /// [`Self::update`] with the full production summary: `None` = no
    /// production at all; `Some(tag)` = productions arrived — TAINT if
    /// any tainted, else FIRED if any fired, else STALE (value-channel
    /// refresh only).
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
            if tag.is_tainted() {
                self.1[i] = Tag::TAINT;
            } else {
                self.0[i] = Some(tv.value_cloned());
                self.1[i] = tag;
            }
            // the orthogonal OR-join (taint ORs, stale ANDs)
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

pub type ByRefChain = graphix_compiler::env::Map<BindId, BindId>;

/// Typed argument read for a FASTCALL fn — the `&[Value]` twin of
/// [`CachedVals::get`] (clone + cast).
pub fn fast_get<T: FromValue>(args: &[Value], i: usize) -> Option<T> {
    args.get(i).and_then(|v| v.clone().cast_to::<T>().ok())
}

/// Run an `EvalCached::FASTCALL` fn over the cached argument slots —
/// the node-walk half of a fastcall builtin, so `eval` and the JIT share
/// one implementation. A slot that has never been delivered means the
/// call has no value yet (bottoms never reach here — Q1).
pub fn fast_eval(f: FastFn, from: &CachedVals) -> Option<Value> {
    let mut args: LPooled<Vec<Value>> = LPooled::take();
    for v in from.0.iter() {
        args.push(v.as_ref()?.clone());
    }
    f(&args)
}

pub trait EvalCached<R: Rt, E: UserEvent>:
    Debug + Default + Send + Sync + 'static
{
    const NAME: &str;
    /// Sync/async classification for fusion. Same semantics as
    /// `BuiltIn::EFFECT`: defaults to `Async` (conservative); override
    /// to `Sync` when the cached operation produces all of its output
    /// on the same cycle as the most recent input that triggered it.
    /// `CachedArgs<T>`'s `BuiltIn` impl pulls this through to the
    /// builtin registry.
    const EFFECT: EffectKind = EffectKind::Async;
    /// Same semantics as `BuiltIn::STATELESS`: `eval` is a
    /// deterministic function of the current args with no external
    /// effect and no cross-invocation state in `Self` (an internal
    /// memo/scratch that never changes an output is fine) — so
    /// deleting the instance and re-initializing it fresh is
    /// unobservable. Conservative default: `false`. Pulled through to
    /// the builtin registry by `CachedArgs<T>`'s `BuiltIn` impl.
    const STATELESS: bool = false;
    /// Same semantics as `BuiltIn::SLEEP_RESTARTS`: `sleep()` clears
    /// semantic state (the arm-rewake RESTART builtins). Consulted by
    /// the fusion interior-sleep gate. Default: `false` (sleep-inert
    /// — the EvalCached wrapper's own sleep clears nothing semantic).
    const SLEEP_RESTARTS: bool = false;
    /// See `BuiltIn::FASTCALL`. Declare with `eval` delegating to the
    /// same fn through [`fast_eval`], so the node-walk and the JIT run
    /// one implementation.
    const FASTCALL: Option<FastFn> = None;

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
    cached: CachedVals,
    /// The last value `eval` produced — the builtin's RESULT slot on
    /// the value channel (absent until the first result): a
    /// merely-stale arg refresh re-surfaces it retagged STALE instead
    /// of re-running `eval`, exactly the kernel's DynCall result temp.
    resident: TagValue,
    t: T,
}

impl<R: Rt, E: UserEvent, T: EvalCached<R, E>> BuiltIn<R, E> for CachedArgs<T> {
    const EFFECT: EffectKind = T::EFFECT;
    const NAME: &str = T::NAME;
    const STATELESS: bool = T::STATELESS;
    const SLEEP_RESTARTS: bool = T::SLEEP_RESTARTS;
    const FASTCALL: Option<FastFn> = T::FASTCALL;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let t = CachedArgs::<T> {
            cached: CachedVals::new(from),
            resident: TagValue::phantom(),
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
        // The whole EvalCached family runs under the value-hook loan
        // (`coretraits::with_value_hooks`): a builtin whose eval
        // compares or sorts Values — min/max, all, array::sort, the
        // map:: operations — honors core Eq/Ord implementations at
        // the value seam.
        let (ev, cached, resident) = (&mut self.t, &mut self.cached, &mut self.resident);
        coretraits::with_value_hooks(ctx, event, move |ctx, event| {
            Self::update_inner(ev, cached, resident, ctx, from, event)
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
        // The arg slots survive sleep exactly as they survive replay
        // resets (below): sleep is PAUSE, and the kernel twin — the
        // DynCall site instance's cached slots — persists across arm
        // deselection, riding on the next dispatch (Eric's ruling
        // 2026-07-31, select_reselect_interior_bottom; witnessed via
        // `max(in0 * 10, 1 / v0)` in a re-woken arm).
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The arg slots PERSIST: they are the interpreter's VALUE
        // channel — the kernel twin of a computed value held in an SSA
        // temp while the FIRING channel (the slots-word) stays quiet. A
        // const-result feeder (`f(v)` with a constant body) fires once
        // ever; its slot value is what lets `push(res, f(v))` keep
        // emitting per fired `res`, exactly like the kernel (the
        // hof_const_body_prev_len pin). `t`'s own state (a tally, a
        // memo) is the builtin's semantics and also survives.
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
        resident: &'a mut TagValue,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &'a TagValue
    where
        T: EvalCached<R, E>,
    {
        match cached.update_full(ctx, from, event) {
            None => resident.ride(),
            Some(t) if cached.any_bottom() => {
                // Q1 BOTTOM PROPAGATES (the dense wrapper seam): an
                // arg is bottom — standing poison or the
                // never-delivered phantom — so the invocation bottoms
                // WITHOUT calling eval; authors never see bottoms.
                // FreshBottom iff a delivery triggered this cycle
                // (`triggers()` becomes the dense fired-bit rule at
                // the 5b flip). No resident clobber: the value channel
                // may re-surface the last genuine result on recovery.
                TagValue::bottom_null(t.triggers())
            }
            Some(_) if cached.any_tainted() => {
                // DEFENSE-IN-DEPTH: unreachable when the seams hold —
                // the CallSite gates every builtin's tainted arg
                // productions to silence and the fused DynCall
                // delivers taint-masked slots as absence (Eric's
                // rulings 2026-07-19/20), so no poisoned delivery can
                // reach these slots. If a new channel leaks one, emit
                // the tainted placeholder (loud downstream) rather
                // than replaying stale state — the SHARED placeholder,
                // so the resident keeps the last genuine result.
                TagValue::tainted_null()
            }
            Some(t) if t.is_fired() => match ev.eval(ctx, cached) {
                Some(v) => resident.set(TagValue::fired(v)),
                // eval produced nothing: ride the resident — the
                // previous result re-surfaces stale, a never-set
                // resident stays the phantom.
                None => resident.ride(),
            },
            Some(_) if !resident.tag().is_bottom() => {
                // WAKE CATCH-UP (design/wake_catchup.md): the first
                // update after this site's sleep may deliver all-stale
                // args whose VALUES drifted while it slept (the slots
                // above are already refreshed to the present values).
                // A STATELESS eval is a pure function of the slots:
                // re-run it, result STALE — the phantom arm's "value
                // rule, not a firing one" extended from
                // first-production to wake. A stateful eval must NOT
                // re-run (its resident IS its state — an accumulator
                // re-run on stale slots would double-count; its edge
                // catch-up arrives separately as a genuine fired
                // delivery from the select's tracked fire bits).
                if T::STATELESS && ctx.wake_recompute() {
                    match ev.eval(ctx, cached) {
                        Some(v) => resident.set(TagValue::stale(v)),
                        None => resident.retag(Tag::STALE),
                    }
                } else {
                    // stale refresh: surface the result slot on the
                    // value channel — eval does not re-run
                    resident.retag(Tag::STALE)
                }
            }
            Some(_) => {
                // ...unless there is NOTHING to surface. A result slot
                // still holding its phantom has never been filled, and
                // "re-surface the last result" is vacuous: the call
                // produces no value at all, so a caller that needs one
                // (a select arm whose body is `math::to_radians(f64:45.)`
                // — every argument a constant, hence never a triggering
                // delivery inside a frame) computes nothing at all,
                // while the kernel recomputes per invocation and has
                // the value. Establish the value channel by running
                // `eval` ONCE; the result is STALE, so this is a value
                // rule and not a firing one
                // (`findings/arm-local-bind-aug2026/03`).
                match ev.eval(ctx, cached) {
                    Some(v) => resident.set(TagValue::stale(v)),
                    None => resident.ride(),
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
                // Q1 BOTTOM PROPAGATES: an arg is bottom, so this
                // invocation bottoms and eval is never queued (a
                // completed reply from a PRIOR invocation below still
                // wins the cycle's output).
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
        let id = BindId::new();
        ctx.rt.ref_var(id, self.top_id);
        self.id = id;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Async wrapper: queued results and the running flag are
        // in-flight semantics; the arg cache feeds re-evaluation on
        // completion. Async builtins never sit inside a sync frame.
    }
}

// ── Core builtins ──────────────────────────────────────────────────

#[derive(Debug, Default)]
struct IsErr {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IsErr {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_is_err";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(IsErr::default()))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IsErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match seam_tick(from[0].update(ctx, event)).map(|tv| {
            tv.with_value(|v| match v {
                Value::Error(_) => Value::Bool(true),
                _ => Value::Bool(false),
            })
        }) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug, Default)]
struct FilterErr {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for FilterErr {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
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

#[derive(Debug, Default)]
struct ToError {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ToError {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_error";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(ToError::default()))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ToError {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        match seam_tick(from[0].update(ctx, event))
            .map(|e| Value::Error(e.value_cloned().into()))
        {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Once {
    val: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    // Sync since P7 (the F2 Async flip reverted): every output
    // appears on the same cycle as the event that triggered it, and
    // the fused DynCall delivers per-arg truth — a non-fired slot
    // arrives `TagValue::stale` and the seam ticks on Fired only
    // (dyncall-stale-arg-fired-aug2026) — so the update-history-
    // sensitive state machine sees the same per-arg events in a
    // kernel as in the node-walk.
    const EFFECT: EffectKind = EffectKind::Sync;
    const SLEEP_RESTARTS: bool = true;
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
        // The fired flag is SEMANTIC (once per subscription lifetime,
        // not once per frame) — sleep's reset is the arm-rewake
        // restart semantics, which a frame reset must not replicate.
    }
}

#[derive(Debug)]
struct Take {
    n: Option<usize>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Take {
    // Sync since P7 (the F2 Async flip reverted): every output
    // appears on the same cycle as the event that triggered it, and
    // the fused DynCall delivers per-arg truth — a non-fired slot
    // arrives `TagValue::stale` and the seam ticks on Fired only
    // (dyncall-stale-arg-fired-aug2026) — so the update-history-
    // sensitive state machine sees the same per-arg events in a
    // kernel as in the node-walk.
    const EFFECT: EffectKind = EffectKind::Sync;
    const SLEEP_RESTARTS: bool = true;
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
        // seed the countdown on a TICK only: a stale ride of #n is
        // the value channel and must not clobber the running count (a
        // fired re-delivery is a genuine re-seed)
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
        // The countdown is semantic (take/skip across the node's
        // lifetime); only sleep's arm-rewake restarts it.
    }
}

#[derive(Debug)]
struct Skip {
    n: Option<usize>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Skip {
    // Sync since P7 (the F2 Async flip reverted): every output
    // appears on the same cycle as the event that triggered it, and
    // the fused DynCall delivers per-arg truth — a non-fired slot
    // arrives `TagValue::stale` and the seam ticks on Fired only
    // (dyncall-stale-arg-fired-aug2026) — so the update-history-
    // sensitive state machine sees the same per-arg events in a
    // kernel as in the node-walk.
    const EFFECT: EffectKind = EffectKind::Sync;
    const SLEEP_RESTARTS: bool = true;
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
        // seed the countdown on a TICK only: a stale ride of #n is
        // the value channel and must not clobber the running count (a
        // fired re-delivery is a genuine re-seed)
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
        // The countdown is semantic (take/skip across the node's
        // lifetime); only sleep's arm-rewake restarts it.
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_all";
    const FASTCALL: Option<FastFn> = Some(fc_all);

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
    const EFFECT: EffectKind = EffectKind::Sync;
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
    const EFFECT: EffectKind = EffectKind::Sync;
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_min";

    // VALUE-LEVEL: each argument is compared as a whole value under
    // graphix's total order — no recursive flattening. The flatten was
    // a bscript holdover that contradicted the declared type
    // (`fn(a: 'a, @args: 'a) -> 'a` resolves 'a := Array<i64> for
    // `min([1,2], [3])` and promises an array back; the flattened
    // scalar broke the JIT's return ABI — soak jul07b). Eric's ruling
    // 2026-07-08: the impl does what the type says.
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
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_max";

    // VALUE-LEVEL, no flattening — see `MinEv`.
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
    const EFFECT: EffectKind = EffectKind::Sync;
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
    const EFFECT: EffectKind = EffectKind::Sync;
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

// ── Bitwise operations ──────────────────────────────────────────

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_bit_and";
    const FASTCALL: Option<FastFn> = Some(fc_bit_and);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_and, from)
    }
}

type BitAnd = CachedArgs<BitAndEv>;

#[derive(Debug, Default)]
struct BitOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitOrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_bit_or";
    const FASTCALL: Option<FastFn> = Some(fc_bit_or);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_or, from)
    }
}

type BitOr = CachedArgs<BitOrEv>;

#[derive(Debug, Default)]
struct BitXorEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitXorEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_bit_xor";
    const FASTCALL: Option<FastFn> = Some(fc_bit_xor);

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_bit_not";
    const FASTCALL: Option<FastFn> = Some(fc_bit_not);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_bit_not, from)
    }
}

type BitNot = CachedArgs<BitNotEv>;

#[derive(Debug, Default)]
struct ShlEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ShlEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_shl";
    const FASTCALL: Option<FastFn> = Some(fc_shl);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_shl, from)
    }
}

type Shl = CachedArgs<ShlEv>;

#[derive(Debug, Default)]
struct ShrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ShrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_shr";
    const FASTCALL: Option<FastFn> = Some(fc_shr);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        fast_eval(fc_shr, from)
    }
}

type Shr = CachedArgs<ShrEv>;

/// Fire-and-forget filter: when the input produces a value we feed it
/// into `pred`, and emit the value whenever `pred` returns `true`. If a
/// new input arrives while `pred` is still working on the last one, the
/// new input replaces the pending value — the caller should wrap this
/// with `queue` if they need strict pairing between inputs and verdicts.
#[derive(Debug)]
struct Filter<R: Rt, E: UserEvent> {
    pred: Node<R, E>,
    pending: Option<Value>,
    fid: BindId,
    x: BindId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Filter<R, E> {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        // `pending` (the held candidate value) and the published
        // pred-fn/element values are all per-invocation replay memory.
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
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The queue and trigger debt are semantic buffering; delivery
        // rides set_var (async, so never inside a sync frame anyway).
    }
}

#[derive(Debug)]
struct Hold {
    triggered: usize,
    current: Option<Value>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Hold {
    // Sync since P7 (the F2 Async flip reverted, same as Uniq below):
    // hold's `current` latch re-arms only when `v` ACTUALLY fires,
    // and the fused DynCall now delivers per-arg truth — a non-fired
    // slot arrives `TagValue::stale` and the seam ticks on Fired only
    // (dyncall-stale-arg-fired-aug2026) — so the jul07c re-latch
    // divergence class is structurally closed.
    const EFFECT: EffectKind = EffectKind::Sync;
    const SLEEP_RESTARTS: bool = true;
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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // hold's held value and trigger debt ARE its contract (sample
        // semantics) — not replay memory.
    }
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
                    // Range guard (the array::init precedent, same
                    // shared cap): each element is one queued set_var —
                    // an unbounded range is a synchronous,
                    // uninterruptible loop and a memory bomb
                    // (seq(i64::MIN, 4) wedged its evaluator past every
                    // deadline — soak jul06g). i128: j - i overflows
                    // i64 for exactly the ranges being rejected.
                    let e = literal!("SeqError");
                    if *j as i128 - *i as i128
                        > graphix_compiler::node::MAX_ARRAY_INIT_LEN as i128
                    {
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
                    let e = literal!("SeqError");
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
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    last: Option<Instant>,
    tid: Option<BindId>,
    top_id: ExprId,
    /// The latest value of the throttled arg — the emission source
    /// when the timer fires (async, after the arg's delivery is long
    /// gone). An explicit OWN field, not an arg-cache slot: the value
    /// a throttle emits is its designated semantic memory
    /// (design/dense_delivery.md, the throttle/timer P4 item).
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
        // both args update up front: a fired duration retunes the
        // wait; the throttled arg's value lands in `last_v` on ANY
        // value-bearing delivery (the value channel), while only a
        // FIRED delivery counts as an event to throttle.
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
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // Timing state is semantic, and `last_v` feeds the in-flight
        // timer's emission (async — never inside a sync frame).
    }
}

#[derive(Debug)]
struct Count {
    count: i64,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Count {
    // Sync since P7 (the F2 Async flip reverted): every output
    // appears on the same cycle as the event that triggered it, and
    // the fused DynCall delivers per-arg truth — a non-fired slot
    // arrives `TagValue::stale` and the seam ticks on Fired only
    // (dyncall-stale-arg-fired-aug2026) — so the update-history-
    // sensitive state machine sees the same per-arg events in a
    // kernel as in the node-walk.
    const EFFECT: EffectKind = EffectKind::Sync;
    const SLEEP_RESTARTS: bool = true;
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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The tally is the canonical semantic-state example — it
        // accumulates across frames in both backends.
    }
}

#[derive(Debug, Default)]
struct MeanEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MeanEv {
    const EFFECT: EffectKind = EffectKind::Sync;
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
    // Sync since P7 (the F2 Async flip reverted): every output
    // appears on the same cycle as the event that triggered it, and
    // the fused DynCall delivers per-arg truth — a non-fired slot
    // arrives `TagValue::stale` and the seam ticks on Fired only
    // (dyncall-stale-arg-fired-aug2026) — so the update-history-
    // sensitive state machine sees the same per-arg events in a
    // kernel as in the node-walk.
    const EFFECT: EffectKind = EffectKind::Sync;
    const SLEEP_RESTARTS: bool = true;
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
        // the dedup comparison runs armed: a core Eq implementation
        // decides what "the same value" means (the value seam)
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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // The held value is uniq's CONTRACT (dedup across time), not
        // replay memory.
    }
}

#[derive(Debug)]
struct Never;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Never {
    // Async, deliberately: `Async` means "output may appear on a later
    // cycle, autonomously, or never" — never() is the limiting case of
    // that contract. Marking it Sync let it fuse as a DynCall that
    // pended on EVERY kernel run: wasted work in used positions, and
    // in dead positions the whole-kernel pending bottomed results the
    // node-walk still produces (the dead-pend divergence). As a fusion
    // boundary the node-walk handles it — zero work, exact semantics.
    // This is also what exempts `never()` from the dead-variadic-call
    // compile error (callsite.rs `reject_dead_variadic_call`): never()
    // is the sanctioned way to write a value that never arrives.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "core_never";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Never))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Never {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        for n in from {
            n.update(ctx, event);
        }
        TagValue::phantom_ref()
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
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
        // rendered under the value-hook loan: an abstract with a core
        // Display implementation prints through it at the seam
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
        // Captured (the harness stdout oracle) — the sink receives
        // exactly the bytes the process stream would have.
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
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Log {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
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
        if let Some(v) = seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
        {
            self.buf.clear();
            write!(self.buf, "{}: ", self.scope.lexical).unwrap();
            let typ = from[1].typ().clone();
            let buf = &mut self.buf;
            coretraits::with_value_hooks(ctx, event, |ctx, _| {
                write!(buf, "{}", TVal { env: &ctx.env, typ: &typ, v: &v }).unwrap()
            });
            emit_line(ctx, self.dest, &self.buf, "\n");
        }
        TagValue::phantom_ref()
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
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $type {
            const EFFECT: EffectKind = EffectKind::Sync;
            const NAME: &str = $name;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut ExecCtx<R, E>,
                _typ: &'a graphix_compiler::typ::FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                _from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                Ok(Box::new(Self { dest: LogDest::Stdout, buf: String::new() }))
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
                if let Some(v) =
                    seam_tick(from[1].update(ctx, event)).map(|tv| tv.value_cloned())
                {
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
                }
                TagValue::phantom_ref()
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

            fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}
        }
    };
}

printfn!(Print, "core_print", "");
printfn!(Println, "core_println", "\n");

// ── Package registration ───────────────────────────────────────────

/// `array::len` — registered here (the array package binds the name)
/// because core's `Collection` implementation for `Array` needs it.
#[derive(Debug, Default)]
struct ArrayLenEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ArrayLenEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_array_len";
    const FASTCALL: Option<FastFn> = Some(array_len);

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_map_len";
    const FASTCALL: Option<FastFn> = Some(map_len);

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
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "core_map_union";
    const FASTCALL: Option<FastFn> = Some(fc_map_union);

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
        Never,
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

/// Embedder-provided netidx configuration for the `sys::net` package
/// (and any other library that wants netidx), seeded into
/// `ctx.libstate` BEFORE package registration. Absent → `Internal`.
/// Lives in package-core so the test harness and embedders can seed
/// it without depending on package-sys.
#[derive(Debug, Clone)]
pub enum NetConfig {
    /// Use these pre-built handles (a real config, or a shared
    /// InternalOnly).
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
    /// Process-internal netidx (resolver + pub/sub) on demand — the
    /// test/fuzz/`--no-netidx` default.
    Internal,
}

/// Optional embedder-seeded netidx tuning (the shell's
/// --publish-timeout). `publish` bounds the publish flusher's batch
/// commit: a subscriber that doesn't consume updates within the
/// timeout is dropped; None (the default) waits.
#[derive(Debug, Clone)]
pub struct NetTimeouts {
    pub publish: Option<std::time::Duration>,
}
