#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use graphix_compiler::{
    effects::EffectKind,
    err, errf,
    expr::{Expr, ExprId},
    node::{callsite::CallSite, genn},
    typ::{FnType, TVal, Type, TypeRef},
    Apply, BindId, BuiltIn, Event, ExecCtx, LambdaId, Node, Refs, Rt, Scope, StaticFnArg,
    UserEvent,
};
use graphix_rt::GXRt;
use immutable_chunkmap::map::Map as CMap;
use netidx::{path::Path, subscriber::Value};
use netidx_core::utils::Either;
use netidx_value::{FromValue, ValArray};
use poolshark::local::LPooled;
use std::{
    any::Any,
    collections::{hash_map::Entry, VecDeque},
    fmt::Debug,
    iter,
    marker::PhantomData,
    time::Duration,
};
use tokio::time::Instant;
use triomphe::Arc as TArc;

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
    Some(typ)
}

// ── Program arguments ─────────────────────────────────────────────

/// Program arguments stored in LibState. Index 0 is the script filename.
#[derive(Default)]
pub struct ProgramArgs(pub Vec<ArcStr>);

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
}

#[macro_export]
macro_rules! arity1 {
    ($from:expr, $updates:expr) => {
        match (&*$from, &*$updates) {
            ([arg], [arg_up]) => (arg, arg_up),
            (_, _) => unreachable!(),
        }
    };
}

#[macro_export]
macro_rules! arity2 {
    ($from:expr, $updates:expr) => {
        match (&*$from, &*$updates) {
            ([arg0, arg1], [arg0_up, arg1_up]) => ((arg0, arg1), (arg0_up, arg1_up)),
            (_, _) => unreachable!(),
        }
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

#[derive(Debug)]
pub struct CachedVals(pub Box<[Option<Value>]>);

impl CachedVals {
    pub fn new<R: Rt, E: UserEvent>(from: &[Node<R, E>]) -> CachedVals {
        CachedVals(from.into_iter().map(|_| None).collect())
    }

    pub fn clear(&mut self) {
        for v in &mut self.0 {
            *v = None
        }
    }

    pub fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> bool {
        from.into_iter().enumerate().fold(false, |res, (i, src)| {
            match src.update(ctx, event) {
                None => res,
                v @ Some(_) => {
                    self.0[i] = v;
                    true
                }
            }
        })
    }

    /// Like update, but return the indexes of the nodes that updated
    /// instead of a consolidated bool
    pub fn update_diff<R: Rt, E: UserEvent>(
        &mut self,
        up: &mut [bool],
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) {
        for (i, n) in from.iter_mut().enumerate() {
            match n.update(ctx, event) {
                None => (),
                v => {
                    self.0[i] = v;
                    up[i] = true
                }
            }
        }
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

pub type ByRefChain = immutable_chunkmap::map::MapS<BindId, BindId>;

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
    t: T,
}

impl<R: Rt, E: UserEvent, T: EvalCached<R, E>> BuiltIn<R, E> for CachedArgs<T> {
    const EFFECT: EffectKind = T::EFFECT;
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
            cached: CachedVals::new(from),
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
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event) {
            self.t.eval(ctx, &self.cached)
        } else {
            None
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
        // Absorbed, not forwarded: the inner `EvalCached` impl is never a
        // HOF (HOFs are direct `Apply` impls), so it never needs the
        // statically-resolved callbacks. Keeps `EvalCached::typecheck1`
        // at its current signature.
        _fn_args: &[StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        self.t.typecheck1(ctx, from, resolved)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.cached.clear()
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

#[derive(Debug)]
pub struct CachedArgsAsync<T: EvalCachedAsync> {
    cached: CachedVals,
    id: BindId,
    top_id: ExprId,
    queued: VecDeque<T::Args>,
    running: bool,
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
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event)
            && let Some(args) = self.t.prepare_args(&self.cached)
        {
            self.queued.push_back(args);
        }
        let res = event.variables.remove(&self.id).and_then(|v| {
            self.running = false;
            self.t.map_value(ctx, v)
        });
        if !self.running
            && let Some(args) = self.queued.pop_front()
        {
            self.running = true;
            let id = self.id;
            ctx.rt.spawn_var(async move { (id, T::eval(args).await) });
        }
        res
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
        // Absorbed (see CachedArgs): inner EvalCachedAsync is never a HOF.
        _fn_args: &[StaticFnArg<'_, R, E>],
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
}

pub trait MapCollection: Debug + Clone + Default + Send + Sync + 'static {
    /// return the length of the collection
    fn len(&self) -> usize;

    /// iterate the collection elements as values
    fn iter_values(&self) -> impl Iterator<Item = Value>;

    /// given a value, return Some if the value is the collection type
    /// we are mapping.
    fn select(v: Value) -> Option<Self>;

    /// given a collection wrap it in a value
    fn project(self) -> Value;

    /// return the element type given the function type
    fn etyp(ft: &FnType) -> Result<Type>;
}

impl MapCollection for ValArray {
    fn iter_values(&self) -> impl Iterator<Item = Value> {
        (**self).iter().cloned()
    }

    fn len(&self) -> usize {
        (**self).len()
    }

    fn select(v: Value) -> Option<Self> {
        match v {
            Value::Array(a) => Some(a.clone()),
            _ => None,
        }
    }

    fn project(self) -> Value {
        Value::Array(self)
    }

    fn etyp(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Array(et) => Ok((**et).clone()),
            _ => bail!("expected array"),
        }
    }
}

impl MapCollection for CMap<Value, Value, 32> {
    fn iter_values(&self) -> impl Iterator<Item = Value> {
        self.into_iter().map(|(k, v)| {
            Value::Array(ValArray::from_iter_exact([k.clone(), v.clone()].into_iter()))
        })
    }

    fn len(&self) -> usize {
        CMap::len(self)
    }

    fn select(v: Value) -> Option<Self> {
        match v {
            Value::Map(m) => Some(m.clone()),
            _ => None,
        }
    }

    fn project(self) -> Value {
        Value::Map(self)
    }

    fn etyp(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Map { key, value } => {
                Ok(Type::Tuple(TArc::from_iter([(**key).clone(), (**value).clone()])))
            }
            _ => bail!("expected Map, got {:?}", ft.args[0].typ),
        }
    }
}

pub trait MapFn<R: Rt, E: UserEvent>: Debug + Default + Send + Sync + 'static {
    type Collection: MapCollection;

    const NAME: &str;

    /// finish will be called when every lambda instance has produced
    /// a value for the updated array. Out contains the output of the
    /// predicate lambda for each index i, and a is the array. out and
    /// a are guaranteed to have the same length. out\[i\].cur is
    /// guaranteed to be Some.
    fn finish(&mut self, slots: &[Slot<R, E>], a: &Self::Collection) -> Option<Value>;

    /// Codegen hook for the node-graph JIT. Implementations emit the
    /// HOF loop into the open kernel via the `emit::scaffold` fns,
    /// compiling the callback body with `body.emit_clif(cx)`.
    /// Returning `Ok(None)` (the default) falls back to the runtime
    /// HOF dispatch (per-element [`Apply`] Nodes via
    /// [`Self::finish`]).
    ///
    /// Implementors receive:
    /// - `array_arg`: the outer HOF call site's input array argument
    ///   Node.
    /// - `body`: the callback lambda's body Node, provided by `MapQ`'s
    ///   analysis-time setup (the callback must be statically
    ///   resolvable — MapQ's `Apply::emit_clif` orchestration
    ///   short-circuits otherwise).
    /// - `elem_name` / `elem_id`: the callback's parameter name and
    ///   BindId (`x` in `|x| body`) — the element binding the body's
    ///   `Ref`s resolve against.
    /// - `in_elem`: the array's element type.
    /// - `elem_binds`: tuple-destructure callback leaves
    ///   `(BindId, position)` when the arg is `|(k, v)|`; empty for a
    ///   single-name `|x|` callback.
    ///
    /// Same contract as [`graphix_compiler::Apply::emit_clif`]:
    /// `Ok(None)` = shape not handled, and it MUST be decided BEFORE
    /// the first instruction is emitted (run every gate up front);
    /// `Err` aborts the kernel build (partial emission fine — the
    /// function is discarded). No `ExecCtx`: emission runs under the
    /// jit lock (see design/distributed_jit.md §1). Implementations
    /// live with each `MapFn` impl (the package that defines the
    /// runtime semantics) — the compiler doesn't know builtin names.
    fn emit_clif(
        _cx: &mut graphix_compiler::fusion::emit::BodyCx,
        _array_arg: &Node<R, E>,
        _body: &Node<R, E>,
        _elem_name: &ArcStr,
        _elem_id: Option<graphix_compiler::BindId>,
        _in_elem: &graphix_compiler::typ::Type,
        _elem_binds: &[(graphix_compiler::BindId, usize)],
    ) -> Result<Option<graphix_compiler::fusion::emit::CompiledExpr>> {
        Ok(None)
    }
}

#[derive(Debug)]
pub struct Slot<R: Rt, E: UserEvent> {
    pub id: BindId,
    pub pred: Node<R, E>,
    pub cur: Option<Value>,
}

impl<R: Rt, E: UserEvent> Slot<R, E> {
    pub fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.delete(ctx);
        ctx.cached.remove(&self.id);
        ctx.env.unbind_variable(self.id);
    }
}

#[derive(Debug)]
pub struct MapQ<R: Rt, E: UserEvent, T: MapFn<R, E>> {
    scope: Scope,
    predid: BindId,
    top_id: ExprId,
    mftyp: TArc<FnType>,
    etyp: Type,
    slots: Vec<Slot<R, E>>,
    cur: T::Collection,
    t: T,
    /// Analysis-only Slot pre-materialized by the bound-instance firing
    /// of [`Apply::typecheck1`] (with `fn_args`) when the callback is
    /// statically resolvable. Mirrors what `update()` builds per
    /// element at runtime, but with the inner CallSite already
    /// resolved against the callback's `LambdaDef` so fusion's
    /// walker can descend into the callback body via standard
    /// `cs.resolved_apply() -> ApplyView::Lambda(&g) -> g.body()`.
    ///
    /// `None` when the callback is dynamic (passed via a binding
    /// the compiler can't statically resolve); fusion falls back
    /// to DynCall for those cases.
    ///
    /// The bound (resolved) representative callback CallSite — the
    /// PRISTINE prototype. `emit_clif` reads this body at compile time to
    /// inline the callback into a surrounding region (full fusion); it is
    /// never mutated. See `design/impure_hof_fusion.md` (superseding).
    pub analysis_pred: Option<Slot<R, E>>,
    /// The per-slot template: a `clone_rebind`'d COPY of `analysis_pred`
    /// with its sync sub-regions fused. Built lazily once on the first
    /// slot-growing `update()` — a SEPARATE copy, so the pristine
    /// `analysis_pred` (which `emit_clif` reads) is never mutated. `update`
    /// `clone_rebind`s THIS per slot; each clone shares the template's
    /// compiled kernel `Arc`s with freshly re-bound async residue (fresh
    /// BindIds → independent per-slot state). `None` until first built, or
    /// when the callback wasn't statically resolvable (→ interp fallback).
    pub fused_template: Option<Node<R, E>>,
}

impl<R: Rt, E: UserEvent, T: MapFn<R, E>> BuiltIn<R, E> for MapQ<R, E, T> {
    // Intrinsically sync: the body iterates and dispatches per-element
    // predicate calls. Slot/cached plumbing is sync scaffolding, not
    // an async-effect operation. The call-site effect joins MapQ's
    // intrinsic Sync with the predicate's effect (M6 HOF inference).
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = T::NAME;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _] => {
                let typ = resolved.unwrap_or(typ);
                Ok(Box::new(Self {
                    scope: scope
                        .append(&format_compact!("fn{}", LambdaId::new().inner())),
                    predid: BindId::new(),
                    top_id,
                    etyp: T::Collection::etyp(typ)?,
                    mftyp: match &typ.args[1].typ {
                        Type::Fn(ft) => ft.clone(),
                        t => bail!("expected a function not {t}"),
                    },
                    slots: vec![],
                    cur: Default::default(),
                    t: T::default(),
                    analysis_pred: None,
                    fused_template: None,
                }))
            }
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent, T: MapFn<R, E>> Apply<R, E> for MapQ<R, E, T> {
    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
        fn_args: &[StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        // HOF callback pre-materialization (the bound-instance firing —
        // `fn_args` non-empty). MapQ takes the callback at positional arg
        // index 1 (index 0 is the input array). Empty `fn_args` (the
        // scratch `def.check` firing) → `find` returns None → no-op.
        let Some(cb) = fn_args.iter().find(|a| a.arg_idx == 1) else {
            return Ok(());
        };
        // Synthesize a representative predicate Apply Node — mirrors
        // exactly what `update()` builds per array element at runtime
        // (see the `while self.slots.len() < a.len()` loop below).
        // This is analysis-only: the runtime path continues to build
        // its own fresh Slots per element so async callbacks
        // (e.g. `array::map(a, |p| net::publish(p, ...))`) keep
        // independent per-slot state.
        let (id, x_node) =
            genn::bind(ctx, &self.scope.lexical, "x", self.etyp.clone(), self.top_id);
        let fnode =
            genn::reference(ctx, self.predid, Type::Fn(self.mftyp.clone()), self.top_id);
        let mut pred = genn::apply(
            fnode,
            self.scope.clone(),
            vec![x_node],
            &self.mftyp,
            self.top_id,
        );
        // Look up the callback's wrapped `Value` (the form
        // `LambdaDef::init` -> Lambda Node emit), then statically
        // resolve the synthetic CallSite directly against
        // `cb.lambda`. We resolve it here rather than relying on the
        // call site's own `try_static_resolve` because we already know
        // the resolution target — no need to consult `bind_to_lambda`.
        let fv = match ctx.lambda_defs.get(&cb.lambda.id).cloned() {
            Some(v) => v,
            None => return Ok(()),
        };
        let any: &mut dyn Any = &mut *pred;
        let Some(cs) = any.downcast_mut::<CallSite<R, E>>() else {
            return Ok(());
        };
        cs.resolve_static(ctx, cb.lambda, fv)?;
        // The analysis_pred is resolved (bound) but NOT fused here. The
        // fuse + splice is deferred to the first `update()` (the lazy-fuse
        // block in `update`) — because `emit_clif` (the array-HOF region
        // inliner) reads THIS SAME `analysis_pred` body at compile time to
        // inline the callback into the surrounding region. Mutating it
        // here (splicing in FusedKernels) corrupts what `emit_clif` reads,
        // so the HOF can't region-fuse and falls back to MapQ. Deferring
        // to runtime orders the mutation strictly after `emit_clif` has
        // read the pristine prototype. See design/impure_hof_fusion.md.
        self.analysis_pred = Some(Slot { id, pred, cur: None });
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let slen = self.slots.len();
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.predid, v.clone());
            event.variables.insert(self.predid, v);
        }
        let (up, resized) = match from[0]
            .update(ctx, event)
            .and_then(|v| T::Collection::select(v))
        {
            Some(a) if a.len() == slen => (Some(a), false),
            Some(a) if a.len() < slen => {
                while self.slots.len() > a.len() {
                    if let Some(mut s) = self.slots.pop() {
                        s.delete(ctx)
                    }
                }
                (Some(a), true)
            }
            Some(a) => {
                // Build the per-slot template ONCE, lazily at first use:
                // a `clone_rebind`'d COPY of the pristine `analysis_pred`
                // with its sync sub-regions fused. We fuse the CLONE so
                // the prototype `emit_clif` reads stays untouched — the
                // two readers never share a mutable object. Lazy keeps a
                // HOF that region-fuses from building this unused
                // template. Split (impure): splice each sync sub-region;
                // whole-body (pure): replace the body with one FusedKernel.
                if self.fused_template.is_none() && self.analysis_pred.is_some() {
                    let scope = self.scope.clone();
                    let ap = self.analysis_pred.as_ref().unwrap();
                    let element_id = ap.id;
                    let mut t = ap.pred.clone_rebind(ctx, &scope);
                    {
                        let any: &mut dyn Any = &mut *t;
                        if let Some(cs) = any.downcast_mut::<CallSite<R, E>>() {
                            let fc = graphix_compiler::fusion::lowering::fuse_callsite(
                                cs, ctx,
                            );
                            if let Some(fc) = &fc {
                                // Whole body fused to one kernel: replace
                                // the body with that FusedKernel.
                                let element_ref = genn::reference(
                                    ctx,
                                    element_id,
                                    self.etyp.clone(),
                                    self.top_id,
                                );
                                if let Ok(fk) = fc.build_slot(
                                    ctx,
                                    vec![element_ref],
                                    self.scope.clone(),
                                    self.top_id,
                                ) {
                                    if let Some(graphix_compiler::ApplyViewMut::Lambda(
                                        g,
                                    )) = cs.resolved_apply_mut()
                                    {
                                        let mut old = std::mem::replace(g.body_mut(), fk);
                                        old.delete(ctx);
                                    }
                                }
                            } else if ctx.fusion_enabled {
                                // Impure callback (async ops in the body):
                                // no whole-body kernel. Fuse the body's
                                // maximal sync sub-regions IN PLACE via the
                                // canonical walk — the async residue stays
                                // node-walked, and each per-slot clone_rebind
                                // shares the fused kernels' Arcs. Gated on
                                // `fusion_enabled` so `FusionDisabled` (interp
                                // mode) node-walks the callback too: `try_fuse`
                                // doesn't self-gate, so without this the HOF
                                // callback would fuse even with fusion off.
                                // (The whole-body branch above is gated the
                                // same way — `fuse_callsite` returns None when
                                // fusion is disabled.) Formerly
                                // build_body_split + splice_into_body.
                                if let Some(graphix_compiler::ApplyViewMut::Lambda(g)) =
                                    cs.resolved_apply_mut()
                                {
                                    let _ =
                                        graphix_compiler::fusion::fuse(g.body_mut(), ctx);
                                }
                            }
                        }
                    }
                    self.fused_template = Some(t);
                }
                while self.slots.len() < a.len() {
                    // Mint this slot's fresh element binding "x" in scope
                    // (fresh id, no ref_var). The cloned template's arg[0]
                    // element ref + the FusedKernel's element feeder
                    // resolve "x" to THIS slot's id via the env name map.
                    let id = ctx
                        .env
                        .bind_variable(
                            &self.scope.lexical,
                            "x",
                            self.etyp.clone(),
                            Default::default(),
                            TArc::new(graphix_compiler::expr::Origin::default()),
                        )
                        .id;
                    // Clone the fused template for this slot: a fresh
                    // independent graph whose sync sub-regions SHARE the
                    // compiled kernel `Arc` and whose async residue is
                    // freshly re-bound (fresh BindIds → independent
                    // per-slot state). Fall back to a fresh interpreted
                    // CallSite when no template was built (callback never
                    // analysis-resolved). See design/impure_hof_fusion.md.
                    let pred = match &self.fused_template {
                        Some(t) => {
                            let scope = self.scope.clone();
                            t.clone_rebind(ctx, &scope)
                        }
                        None => {
                            let node =
                                genn::reference(ctx, id, self.etyp.clone(), self.top_id);
                            let fnode = genn::reference(
                                ctx,
                                self.predid,
                                Type::Fn(self.mftyp.clone()),
                                self.top_id,
                            );
                            genn::apply(
                                fnode,
                                self.scope.clone(),
                                vec![node],
                                &self.mftyp,
                                self.top_id,
                            )
                        }
                    };
                    self.slots.push(Slot { id, pred, cur: None });
                }
                (Some(a), true)
            }
            None => (None, false),
        };
        if let Some(a) = up {
            for (s, v) in self.slots.iter().zip(a.iter_values()) {
                ctx.cached.insert(s.id, v.clone());
                event.variables.insert(s.id, v);
            }
            self.cur = a.clone();
            if a.len() == 0 {
                return Some(T::Collection::project(a));
            }
        }
        let init = event.init;
        let mut up = resized;
        for (i, s) in self.slots.iter_mut().enumerate() {
            if i == slen {
                // new nodes were added starting here
                event.init = true;
                if let Entry::Vacant(e) = event.variables.entry(self.predid)
                    && let Some(v) = ctx.cached.get(&self.predid)
                {
                    e.insert(v.clone());
                }
            }
            if let Some(v) = s.pred.update(ctx, event) {
                s.cur = Some(v);
                up = true;
            }
        }
        event.init = init;
        if up && self.slots.iter().all(|s| s.cur.is_some()) {
            self.t.finish(&mut &self.slots, &self.cur)
        } else {
            None
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        let mftyp = match &from[1].typ() {
            Type::Fn(ft) => ft.clone(),
            t => bail!("expected a function not {t}"),
        };
        let (_, node) =
            genn::bind(ctx, &self.scope.lexical, "x", self.etyp.clone(), self.top_id);
        let fargs = vec![node];
        let ft = mftyp.clone();
        let fnode = genn::reference(ctx, self.predid, Type::Fn(ft.clone()), self.top_id);
        let mut node = genn::apply(fnode, self.scope.clone(), fargs, &ft, self.top_id);
        node.typecheck0(ctx)?;
        node.delete(ctx);
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        for s in &self.slots {
            s.pred.refs(refs)
        }
        if let Some(s) = &self.analysis_pred {
            // `s.id` (the synthetic per-element binding) and `predid`
            // (the callback-function handle) are internal to MapQ's
            // analysis-only prototype. Mask them so fusion's
            // region-input discovery surfaces only the callback's *real*
            // captures — not these synthetic bindings, which nothing
            // feeds at runtime (a leaked kernel input becomes an
            // orphaned feeder → the kernel never fires → hang).
            refs.mark_bound(s.id);
            refs.mark_bound(self.predid);
            s.pred.refs(refs);
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.cached.remove(&self.predid);
        for sl in &mut self.slots {
            sl.delete(ctx)
        }
        if let Some(mut s) = self.analysis_pred.take() {
            s.delete(ctx);
        }
        if let Some(mut t) = self.fused_template.take() {
            t.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.cur = Default::default();
        for sl in &mut self.slots {
            sl.cur = None;
            sl.pred.sleep(ctx);
        }
        // analysis_pred is analysis-only — never run at runtime, no
        // need to put to sleep.
    }

    /// Direct-path codegen (Stage D2) — the [`Apply::emit_clif`] twin
    /// of the `Apply::emit_clif` orchestration below: locate the
    /// callback body through the analysis_pred's resolved CallSite,
    /// extract the element name / BindId / destructure leaves, then
    /// delegate to the per-`MapFn` [`MapFn::emit_clif`]. Every miss is
    /// `Ok(None)` — nothing has been emitted yet, so the call site
    /// falls back to its next strategy (DynCall, or no fusion).
    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut graphix_compiler::fusion::emit::BodyCx,
    ) -> Result<Option<graphix_compiler::fusion::emit::CompiledExpr>> {
        // No analysis_pred → callback wasn't statically resolvable.
        let Some(slot) = self.analysis_pred.as_ref() else {
            return Ok(None);
        };
        // Outer call site's positional args: array, callback.
        let Some(array_arg) = callsite.arg_positional(0) else {
            return Ok(None);
        };
        // Locate the callback's body Node through the synthesized
        // inner CallSite's resolved Apply.
        let inner_cs = match slot.pred.view() {
            graphix_compiler::NodeView::CallSite(cs) => cs,
            _ => return Ok(None),
        };
        let Some(inner_apply) = inner_cs.resolved_apply() else {
            return Ok(None);
        };
        let g = match inner_apply {
            graphix_compiler::ApplyView::Lambda(g) => g,
            _ => return Ok(None),
        };
        let body = g.body();
        // A `|(k, v)|` tuple-destructure arg has no single name —
        // synthesize an element name and hand the per-leaf
        // `(BindId, position)` list to the impl. A single-name `|x|`
        // arg pulls its name from the callback `FnType.args[0].kind`
        // and its BindId from the arg pattern (the body's `Ref`s carry
        // that id — the direct path resolves BindId-first).
        let (elem_name, elem_id, elem_binds) =
            match g.args().first().and_then(|p| p.tuple_leaves()) {
                Some(binds) => (arcstr::literal!("__elem"), None, binds),
                None => {
                    let n = match g.typ().args.first().map(|a| &a.kind) {
                        Some(graphix_compiler::typ::FnArgKind::Positional {
                            name: Some(n),
                        }) => n.clone(),
                        Some(graphix_compiler::typ::FnArgKind::Labeled {
                            name, ..
                        }) => name.clone(),
                        _ => return Ok(None),
                    };
                    let id = g.args().first().and_then(|p| p.single_bind_id());
                    (n, id, Vec::new())
                }
            };
        // Delegate to the per-MapFn codegen.
        T::emit_clif(cx, array_arg, body, &elem_name, elem_id, &self.etyp, &elem_binds)
    }
}

pub trait FoldFn<R: Rt, E: UserEvent>: Debug + Send + Sync + 'static {
    type Collection: MapCollection;

    const NAME: &str;

    /// Direct-path codegen hook (Stage D2) — the [`MapFn::emit_clif`]
    /// analog for the 2-arg `(acc, elem)` callback. `Ok(None)` gates
    /// must all be decided before the first emitted instruction; after
    /// emission starts a mismatch must `Err` (the kernel build aborts
    /// and the subtree node-walks). The acc/elem BindIds come from the
    /// callback's arg patterns — the direct path resolves Refs
    /// BindId-first.
    fn emit_clif(
        _cx: &mut graphix_compiler::fusion::emit::BodyCx,
        _array_arg: &Node<R, E>,
        _init_arg: &Node<R, E>,
        _body: &Node<R, E>,
        _acc_name: &ArcStr,
        _acc_id: Option<graphix_compiler::BindId>,
        _elem_name: &ArcStr,
        _elem_id: Option<graphix_compiler::BindId>,
        _in_elem: &graphix_compiler::typ::Type,
        _elem_binds: &[(graphix_compiler::BindId, usize)],
    ) -> Result<Option<graphix_compiler::fusion::emit::CompiledExpr>> {
        Ok(None)
    }
}

/// Pre-materialized analysis predicate for [`FoldQ`]. See
/// [`MapQ::analysis_pred`] for the rationale — same shape adapted
/// to the 2-arg `(acc, elem)` callback signature.
#[derive(Debug)]
pub struct FoldAnalysisPred<R: Rt, E: UserEvent> {
    /// BindId the accumulator Ref reads from. Synthetic — runtime
    /// uses the parallel `binds`/`initids` arrays in [`FoldQ`].
    pub acc_id: BindId,
    /// BindId the element Ref reads from. Synthetic, same.
    pub elem_id: BindId,
    /// The synthesized callback [`Apply`] Node. Statically resolved
    /// against the callback's `LambdaDef`, so its `function` field
    /// holds a `GXLambda` whose body is walkable.
    pub pred: Node<R, E>,
}

impl<R: Rt, E: UserEvent> FoldAnalysisPred<R, E> {
    pub fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pred.delete(ctx);
        ctx.cached.remove(&self.acc_id);
        ctx.cached.remove(&self.elem_id);
        ctx.env.unbind_variable(self.acc_id);
        ctx.env.unbind_variable(self.elem_id);
    }
}

#[derive(Debug)]
pub struct FoldQ<R: Rt, E: UserEvent, T: FoldFn<R, E>> {
    top_id: ExprId,
    fid: BindId,
    scope: Scope,
    binds: Vec<BindId>,
    nodes: Vec<Node<R, E>>,
    inits: Vec<Option<Value>>,
    initids: Vec<BindId>,
    initid: BindId,
    mftype: TArc<FnType>,
    etyp: Type,
    ityp: Type,
    init: Option<Value>,
    /// Analysis-only pre-materialized callback Apply. See
    /// [`MapQ::analysis_pred`] for semantics.
    pub analysis_pred: Option<FoldAnalysisPred<R, E>>,
    t: PhantomData<T>,
}

impl<R: Rt, E: UserEvent, T: FoldFn<R, E>> BuiltIn<R, E> for FoldQ<R, E, T> {
    // Intrinsically sync. See MapQ above for the rationale.
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = T::NAME;

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _] => {
                let typ = resolved.unwrap_or(typ);
                Ok(Box::new(Self {
                    top_id,
                    scope: scope.clone(),
                    binds: vec![],
                    nodes: vec![],
                    inits: vec![],
                    initids: vec![],
                    initid: BindId::new(),
                    fid: BindId::new(),
                    etyp: T::Collection::etyp(typ)?,
                    ityp: typ.args[1].typ.clone(),
                    mftype: match &typ.args[2].typ {
                        Type::Fn(ft) => ft.clone(),
                        t => bail!("expected a function not {t}"),
                    },
                    init: None,
                    analysis_pred: None,
                    t: PhantomData,
                }))
            }
            _ => bail!("expected three arguments"),
        }
    }
}

impl<R: Rt, E: UserEvent, T: FoldFn<R, E>> Apply<R, E> for FoldQ<R, E, T> {
    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
        fn_args: &[StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        // HOF callback pre-materialization (bound-instance firing). FoldQ:
        // positional args are (input, init, callback); callback at index 2.
        // Empty `fn_args` (scratch firing) → `find` None → no-op.
        let Some(cb) = fn_args.iter().find(|a| a.arg_idx == 2) else {
            return Ok(());
        };
        // Synthesize a representative (acc, elem) -> ... predicate
        // Apply Node — mirrors what `update()` builds per array
        // element. Analysis-only; runtime continues per-element
        // synthesis for independent state.
        let (acc_id, acc_node) =
            genn::bind(ctx, &self.scope.lexical, "acc", self.ityp.clone(), self.top_id);
        let (elem_id, elem_node) =
            genn::bind(ctx, &self.scope.lexical, "x", self.etyp.clone(), self.top_id);
        let fnode =
            genn::reference(ctx, self.fid, Type::Fn(self.mftype.clone()), self.top_id);
        let mut pred = genn::apply(
            fnode,
            self.scope.clone(),
            vec![acc_node, elem_node],
            &self.mftype,
            self.top_id,
        );
        let fv = match ctx.lambda_defs.get(&cb.lambda.id).cloned() {
            Some(v) => v,
            None => return Ok(()),
        };
        let any: &mut dyn Any = &mut *pred;
        let Some(cs) = any.downcast_mut::<CallSite<R, E>>() else {
            return Ok(());
        };
        cs.resolve_static(ctx, cb.lambda, fv)?;
        self.analysis_pred = Some(FoldAnalysisPred { acc_id, elem_id, pred });
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let init = match from[0].update(ctx, event).and_then(|v| T::Collection::select(v))
        {
            None => self.nodes.len(),
            Some(a) if a.len() == self.binds.len() => {
                for (id, v) in self.binds.iter().zip(a.iter_values()) {
                    ctx.cached.insert(*id, v.clone());
                    event.variables.insert(*id, v.clone());
                }
                self.nodes.len()
            }
            Some(a) => {
                let vals = a.iter_values().collect::<LPooled<Vec<Value>>>();
                while self.binds.len() < a.len() {
                    self.binds.push(BindId::new());
                    self.inits.push(None);
                    self.initids.push(BindId::new());
                }
                while a.len() < self.binds.len() {
                    if let Some(id) = self.binds.pop() {
                        ctx.cached.remove(&id);
                    }
                    if let Some(id) = self.initids.pop() {
                        ctx.cached.remove(&id);
                    }
                    self.inits.pop();
                    if let Some(mut n) = self.nodes.pop() {
                        n.delete(ctx);
                    }
                }
                let init = self.nodes.len();
                for i in 0..self.binds.len() {
                    ctx.cached.insert(self.binds[i], vals[i].clone());
                    event.variables.insert(self.binds[i], vals[i].clone());
                    if i >= self.nodes.len() {
                        let n = genn::reference(
                            ctx,
                            if i == 0 { self.initid } else { self.initids[i - 1] },
                            self.ityp.clone(),
                            self.top_id,
                        );
                        let x = genn::reference(
                            ctx,
                            self.binds[i],
                            self.etyp.clone(),
                            self.top_id,
                        );
                        let fnode = genn::reference(
                            ctx,
                            self.fid,
                            Type::Fn(self.mftype.clone()),
                            self.top_id,
                        );
                        let node = genn::apply(
                            fnode,
                            self.scope.clone(),
                            vec![n, x],
                            &self.mftype,
                            self.top_id,
                        );
                        self.nodes.push(node);
                    }
                }
                init
            }
        };
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.initid, v.clone());
            event.variables.insert(self.initid, v.clone());
            self.init = Some(v);
        }
        if let Some(v) = from[2].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        let old_init = event.init;
        for i in 0..self.nodes.len() {
            if i == init {
                event.init = true;
                if let Some(v) = ctx.cached.get(&self.fid)
                    && let Entry::Vacant(e) = event.variables.entry(self.fid)
                {
                    e.insert(v.clone());
                }
                if i == 0 {
                    if let Some(v) = self.init.as_ref()
                        && let Entry::Vacant(e) = event.variables.entry(self.initid)
                    {
                        e.insert(v.clone());
                    }
                } else {
                    if let Some(v) = self.inits[i - 1].clone() {
                        event.variables.insert(self.initids[i - 1], v);
                    }
                }
            }
            match self.nodes[i].update(ctx, event) {
                Some(v) => {
                    ctx.cached.insert(self.initids[i], v.clone());
                    event.variables.insert(self.initids[i], v.clone());
                    self.inits[i] = Some(v);
                }
                None => {
                    ctx.cached.remove(&self.initids[i]);
                    event.variables.remove(&self.initids[i]);
                    self.inits[i] = None;
                }
            }
        }
        event.init = old_init;
        self.inits.last().and_then(|v| v.clone())
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        let mut n = genn::reference(ctx, self.initid, self.ityp.clone(), self.top_id);
        let x = genn::reference(ctx, BindId::new(), self.etyp.clone(), self.top_id);
        let fnode =
            genn::reference(ctx, self.fid, Type::Fn(self.mftype.clone()), self.top_id);
        n = genn::apply(fnode, self.scope.clone(), vec![n, x], &self.mftype, self.top_id);
        n.typecheck0(ctx)?;
        n.delete(ctx);
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        for n in &self.nodes {
            n.refs(refs)
        }
        if let Some(p) = &self.analysis_pred {
            // Mask the analysis-only synthetic bindings (the per-element
            // `acc`/`x` accumulator+element and the callback-function
            // handle `fid`) so fusion's region-input discovery sees only
            // the callback's real captures — not these internal
            // bindings (which nothing feeds at runtime → orphaned kernel
            // feeder → hang). See `MapQ::refs`.
            refs.mark_bound(p.acc_id);
            refs.mark_bound(p.elem_id);
            refs.mark_bound(self.fid);
            p.pred.refs(refs);
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let i =
            iter::once(&self.initid).chain(self.binds.iter()).chain(self.initids.iter());
        for id in i {
            ctx.cached.remove(id);
        }
        for n in &mut self.nodes {
            n.delete(ctx);
        }
        if let Some(mut p) = self.analysis_pred.take() {
            p.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.init = None;
        for v in &mut self.inits {
            *v = None
        }
        for n in &mut self.nodes {
            n.sleep(ctx)
        }
        // analysis_pred is analysis-only — no runtime sleep needed.
    }

    /// Direct-path codegen (Stage D2) — the [`Apply::emit_clif`] twin
    /// of the `Apply::emit_clif` orchestration below, adapted to
    /// the 2-arg `(acc, elem)` callback. See `MapQ::emit_clif` for the
    /// shared structure; every miss is `Ok(None)` (nothing emitted
    /// yet — the call site falls back to DynCall or no fusion).
    fn emit_clif(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut graphix_compiler::fusion::emit::BodyCx,
    ) -> Result<Option<graphix_compiler::fusion::emit::CompiledExpr>> {
        let Some(slot) = self.analysis_pred.as_ref() else {
            return Ok(None);
        };
        // Outer call site's positional args: array, init, callback.
        let Some(array_arg) = callsite.arg_positional(0) else {
            return Ok(None);
        };
        let Some(init_arg) = callsite.arg_positional(1) else {
            return Ok(None);
        };
        let inner_cs = match slot.pred.view() {
            graphix_compiler::NodeView::CallSite(cs) => cs,
            _ => return Ok(None),
        };
        let Some(inner_apply) = inner_cs.resolved_apply() else {
            return Ok(None);
        };
        let g = match inner_apply {
            graphix_compiler::ApplyView::Lambda(g) => g,
            _ => return Ok(None),
        };
        let body = g.body();
        let mut params = g.typ().args.iter();
        // The accumulator (1st) param is always a single name.
        let acc_name = match params.next().map(|a| &a.kind) {
            Some(graphix_compiler::typ::FnArgKind::Positional { name: Some(n) }) => {
                n.clone()
            }
            Some(graphix_compiler::typ::FnArgKind::Labeled { name, .. }) => name.clone(),
            _ => return Ok(None),
        };
        let acc_id = g.args().first().and_then(|p| p.single_bind_id());
        // The element (2nd) param may be an `|acc, (k, v)|`
        // destructure — same handling as `MapQ::emit_clif`.
        let (elem_name, elem_id, elem_binds) =
            match g.args().get(1).and_then(|p| p.tuple_leaves()) {
                Some(binds) => (arcstr::literal!("__elem"), None, binds),
                None => {
                    let n = match params.next().map(|a| &a.kind) {
                        Some(graphix_compiler::typ::FnArgKind::Positional {
                            name: Some(n),
                        }) => n.clone(),
                        Some(graphix_compiler::typ::FnArgKind::Labeled {
                            name, ..
                        }) => name.clone(),
                        _ => return Ok(None),
                    };
                    let id = g.args().get(1).and_then(|p| p.single_bind_id());
                    (n, id, Vec::new())
                }
            };
        T::emit_clif(
            cx,
            array_arg,
            init_arg,
            body,
            &acc_name,
            acc_id,
            &elem_name,
            elem_id,
            &self.etyp,
            &elem_binds,
        )
    }
}

// ── Core builtins ──────────────────────────────────────────────────

#[derive(Debug)]
struct IsErr;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for IsErr {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_is_err";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(IsErr))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for IsErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|v| match v {
            Value::Error(_) => Value::Bool(true),
            _ => Value::Bool(false),
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct FilterErr;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for FilterErr {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_filter_err";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(FilterErr))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for FilterErr {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|v| match v {
            v @ Value::Error(_) => Some(v),
            _ => None,
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct ToError;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for ToError {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_error";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(ToError))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for ToError {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).map(|e| Value::Error(triomphe::Arc::new(e)))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

#[derive(Debug)]
struct Once {
    val: bool,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    // Async, deliberately (F2 flip): this builtin's semantics are
    // UPDATE-HISTORY-SENSITIVE — its Apply keeps state keyed to which
    // arg updated on which cycle. The fused DynCall dispatch protocol
    // re-delivers EVERY arg as a fresh update on every dispatch (the
    // kernel can't reproduce per-arg update granularity), so eager
    // dispatch mis-counts (e.g. fused `skip(#n:1, e)` saw `n` "update"
    // every cycle and never passed an event). Async = fusion boundary
    // = the node-walk runs it with exact update semantics.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "core_once";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Once { val: false }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Once {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        match from {
            [s] => s.update(ctx, event).and_then(|v| {
                if self.val {
                    None
                } else {
                    self.val = true;
                    Some(v)
                }
            }),
            _ => None,
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.val = false
    }
}

#[derive(Debug)]
struct Take {
    n: Option<usize>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Take {
    // Async, deliberately (F2 flip): this builtin's semantics are
    // UPDATE-HISTORY-SENSITIVE — its Apply keeps state keyed to which
    // arg updated on which cycle. The fused DynCall dispatch protocol
    // re-delivers EVERY arg as a fresh update on every dispatch (the
    // kernel can't reproduce per-arg update granularity), so eager
    // dispatch mis-counts (e.g. fused `skip(#n:1, e)` saw `n` "update"
    // every cycle and never passed an event). Async = fusion boundary
    // = the node-walk runs it with exact update semantics.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "core_take";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Take { n: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Take {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(n) =
            from[0].update(ctx, event).and_then(|v| v.cast_to::<usize>().ok())
        {
            self.n = Some(n)
        }
        match from[1].update(ctx, event) {
            None => None,
            Some(v) => match &mut self.n {
                None => None,
                Some(n) if *n > 0 => {
                    *n -= 1;
                    return Some(v);
                }
                Some(_) => None,
            },
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.n = None
    }
}

#[derive(Debug)]
struct Skip {
    n: Option<usize>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Skip {
    // Async, deliberately (F2 flip): this builtin's semantics are
    // UPDATE-HISTORY-SENSITIVE — its Apply keeps state keyed to which
    // arg updated on which cycle. The fused DynCall dispatch protocol
    // re-delivers EVERY arg as a fresh update on every dispatch (the
    // kernel can't reproduce per-arg update granularity), so eager
    // dispatch mis-counts (e.g. fused `skip(#n:1, e)` saw `n` "update"
    // every cycle and never passed an event). Async = fusion boundary
    // = the node-walk runs it with exact update semantics.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "core_skip";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Skip { n: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Skip {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(n) =
            from[0].update(ctx, event).and_then(|v| v.cast_to::<usize>().ok())
        {
            self.n = Some(n)
        }
        match from[1].update(ctx, event) {
            None => None,
            Some(v) => match &mut self.n {
                None => Some(v),
                Some(n) if *n > 0 => {
                    *n -= 1;
                    None
                }
                Some(_) => Some(v),
            },
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.n = None
    }
}

#[derive(Debug, Default)]
struct AllEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for AllEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_all";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &*from.0 {
            [] => None,
            [hd, tl @ ..] => match hd {
                None => None,
                v @ Some(_) => {
                    if tl.into_iter().all(|v1| v1 == v) {
                        v.clone()
                    } else {
                        None
                    }
                }
            },
        }
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

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => {
                    res = Some(v);
                }
                (Some(v0), Some(v)) => {
                    res = if v < v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
    }
}

type Min = CachedArgs<MinEv>;

#[derive(Debug, Default)]
struct MaxEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for MaxEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_max";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        let mut res = None;
        for v in from.flat_iter() {
            match (res, v) {
                (None, None) | (Some(_), None) => return None,
                (None, Some(v)) => {
                    res = Some(v);
                }
                (Some(v0), Some(v)) => {
                    res = if v > v0 { Some(v) } else { Some(v0) };
                }
            }
        }
        res
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
    ($from:expr, $op:tt) => {
        match (&$from.0[0], &$from.0[1]) {
            (Some(Value::U8(l)), Some(Value::U8(r))) => Some(Value::U8(l $op r)),
            (Some(Value::I8(l)), Some(Value::I8(r))) => Some(Value::I8(l $op r)),
            (Some(Value::U16(l)), Some(Value::U16(r))) => Some(Value::U16(l $op r)),
            (Some(Value::I16(l)), Some(Value::I16(r))) => Some(Value::I16(l $op r)),
            (Some(Value::U32(l)), Some(Value::U32(r))) => Some(Value::U32(l $op r)),
            (Some(Value::V32(l)), Some(Value::V32(r))) => Some(Value::V32(l $op r)),
            (Some(Value::I32(l)), Some(Value::I32(r))) => Some(Value::I32(l $op r)),
            (Some(Value::Z32(l)), Some(Value::Z32(r))) => Some(Value::Z32(l $op r)),
            (Some(Value::U64(l)), Some(Value::U64(r))) => Some(Value::U64(l $op r)),
            (Some(Value::V64(l)), Some(Value::V64(r))) => Some(Value::V64(l $op r)),
            (Some(Value::I64(l)), Some(Value::I64(r))) => Some(Value::I64(l $op r)),
            (Some(Value::Z64(l)), Some(Value::Z64(r))) => Some(Value::Z64(l $op r)),
            _ => None,
        }
    };
}

macro_rules! int_shift {
    ($from:expr, $method:ident) => {
        match (&$from.0[0], &$from.0[1]) {
            (Some(Value::U8(l)), Some(Value::U8(r))) => {
                Some(Value::U8(l.$method(*r as u32)))
            }
            (Some(Value::I8(l)), Some(Value::I8(r))) => {
                Some(Value::I8(l.$method(*r as u32)))
            }
            (Some(Value::U16(l)), Some(Value::U16(r))) => {
                Some(Value::U16(l.$method(*r as u32)))
            }
            (Some(Value::I16(l)), Some(Value::I16(r))) => {
                Some(Value::I16(l.$method(*r as u32)))
            }
            (Some(Value::U32(l)), Some(Value::U32(r))) => {
                Some(Value::U32(l.$method(*r as u32)))
            }
            (Some(Value::V32(l)), Some(Value::V32(r))) => {
                Some(Value::V32(l.$method(*r as u32)))
            }
            (Some(Value::I32(l)), Some(Value::I32(r))) => {
                Some(Value::I32(l.$method(*r as u32)))
            }
            (Some(Value::Z32(l)), Some(Value::Z32(r))) => {
                Some(Value::Z32(l.$method(*r as u32)))
            }
            (Some(Value::U64(l)), Some(Value::U64(r))) => {
                Some(Value::U64(l.$method(*r as u32)))
            }
            (Some(Value::V64(l)), Some(Value::V64(r))) => {
                Some(Value::V64(l.$method(*r as u32)))
            }
            (Some(Value::I64(l)), Some(Value::I64(r))) => {
                Some(Value::I64(l.$method(*r as u32)))
            }
            (Some(Value::Z64(l)), Some(Value::Z64(r))) => {
                Some(Value::Z64(l.$method(*r as u32)))
            }
            _ => None,
        }
    };
}

#[derive(Debug, Default)]
struct BitAndEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitAndEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_bit_and";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        int_binop!(from, &)
    }
}

type BitAnd = CachedArgs<BitAndEv>;

#[derive(Debug, Default)]
struct BitOrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitOrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_bit_or";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        int_binop!(from, |)
    }
}

type BitOr = CachedArgs<BitOrEv>;

#[derive(Debug, Default)]
struct BitXorEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitXorEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_bit_xor";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        int_binop!(from, ^)
    }
}

type BitXor = CachedArgs<BitXorEv>;

#[derive(Debug, Default)]
struct BitNotEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for BitNotEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_bit_not";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        match &from.0[0] {
            Some(Value::U8(v)) => Some(Value::U8(!v)),
            Some(Value::I8(v)) => Some(Value::I8(!v)),
            Some(Value::U16(v)) => Some(Value::U16(!v)),
            Some(Value::I16(v)) => Some(Value::I16(!v)),
            Some(Value::U32(v)) => Some(Value::U32(!v)),
            Some(Value::V32(v)) => Some(Value::V32(!v)),
            Some(Value::I32(v)) => Some(Value::I32(!v)),
            Some(Value::Z32(v)) => Some(Value::Z32(!v)),
            Some(Value::U64(v)) => Some(Value::U64(!v)),
            Some(Value::V64(v)) => Some(Value::V64(!v)),
            Some(Value::I64(v)) => Some(Value::I64(!v)),
            Some(Value::Z64(v)) => Some(Value::Z64(!v)),
            _ => None,
        }
    }
}

type BitNot = CachedArgs<BitNotEv>;

#[derive(Debug, Default)]
struct ShlEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ShlEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_shl";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        int_shift!(from, wrapping_shl)
    }
}

type Shl = CachedArgs<ShlEv>;

#[derive(Debug, Default)]
struct ShrEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for ShrEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_shr";

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        int_shift!(from, wrapping_shr)
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
                let pred = genn::apply(fnode, scope.clone(), vec![xn], &ptyp, top_id);
                Ok(Box::new(Self { pred, pending: None, fid, x }))
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
    ) -> Option<Value> {
        if let Some(v) = from[1].update(ctx, event) {
            ctx.cached.insert(self.fid, v.clone());
            event.variables.insert(self.fid, v);
        }
        if let Some(v) = from[0].update(ctx, event) {
            self.pending = Some(v.clone());
            ctx.cached.insert(self.x, v.clone());
            event.variables.insert(self.x, v);
        }
        self.pred.update(ctx, event).and_then(|b| match b {
            Value::Bool(true) => self.pending.clone(),
            _ => None,
        })
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
        ctx.cached.remove(&self.fid);
        ctx.cached.remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.pending = None;
        self.pred.sleep(ctx);
    }
}

#[derive(Debug)]
struct Queue {
    triggered: usize,
    queue: VecDeque<Value>,
    id: BindId,
    top_id: ExprId,
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
                Ok(Box::new(Self { triggered: 0, queue: VecDeque::new(), id, top_id }))
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
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(v) = from[1].update(ctx, event) {
            self.queue.push_back(v);
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            self.triggered -= 1;
            ctx.rt.set_var(self.id, self.queue.pop_front().unwrap());
        }
        event.variables.get(&self.id).cloned()
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
}

#[derive(Debug)]
struct Hold {
    triggered: usize,
    current: Option<Value>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Hold {
    // Hold takes (trigger, value); it caches state across cycles
    // (triggered count, latest value) but each emission lands on the
    // same cycle as the most recent input that completed the
    // (trigger-arrived, value-arrived) pairing. State becomes a
    // kernel-local register; no cycle-shifted output.
    const EFFECT: EffectKind = EffectKind::Sync;
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
            [_, _] => Ok(Box::new(Self { triggered: 0, current: None })),
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
    ) -> Option<Value> {
        if from[0].update(ctx, event).is_some() {
            self.triggered += 1;
        }
        if let Some(v) = from[1].update(ctx, event) {
            self.current = Some(v);
        }
        if self.triggered > 0
            && let Some(v) = self.current.take()
        {
            self.triggered -= 1;
            Some(v)
        } else {
            None
        }
    }

    fn delete(&mut self, _: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _: &mut ExecCtx<R, E>) {
        self.triggered = 0;
        self.current = None;
    }
}

#[derive(Debug)]
struct Seq {
    id: BindId,
    top_id: ExprId,
    args: CachedVals,
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
        Ok(Box::new(Self { id, top_id, args }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Seq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.args.update(ctx, from, event) {
            match &self.args.0[..] {
                [Some(Value::I64(i)), Some(Value::I64(j))] if i <= j => {
                    for v in *i..*j {
                        ctx.rt.set_var(self.id, Value::I64(v));
                    }
                }
                _ => {
                    let e = literal!("SeqError");
                    return Some(err!(e, "invalid args i must be <= j"));
                }
            }
        }
        event.variables.get(&self.id).cloned()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    last: Option<Instant>,
    tid: Option<BindId>,
    top_id: ExprId,
    args: CachedVals,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Throttle {
    const NAME: &str = "core_throttle";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let args = CachedVals::new(from);
        Ok(Box::new(Self { wait: Duration::ZERO, last: None, tid: None, top_id, args }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Throttle {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        macro_rules! maybe_schedule {
            ($last:expr) => {{
                let now = Instant::now();
                if now - *$last >= self.wait {
                    *$last = now;
                    return self.args.0[1].clone();
                } else {
                    let id = BindId::new();
                    ctx.rt.ref_var(id, self.top_id);
                    ctx.rt.set_timer(id, self.wait - (now - *$last));
                    self.tid = Some(id);
                    return None;
                }
            }};
        }
        let mut up = [false; 2];
        self.args.update_diff(&mut up, ctx, from, event);
        if up[0]
            && let Some(Value::Duration(d)) = &self.args.0[0]
        {
            self.wait = **d;
            if let Some(id) = self.tid.take()
                && let Some(last) = &mut self.last
            {
                ctx.rt.unref_var(id, self.top_id);
                maybe_schedule!(last)
            }
        }
        if up[1] && self.tid.is_none() {
            match &mut self.last {
                Some(last) => maybe_schedule!(last),
                None => {
                    self.last = Some(Instant::now());
                    return self.args.0[1].clone();
                }
            }
        }
        if let Some(id) = self.tid
            && let Some(_) = event.variables.get(&id)
        {
            ctx.rt.unref_var(id, self.top_id);
            self.tid = None;
            self.last = Some(Instant::now());
            return self.args.0[1].clone();
        }
        None
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
        self.args.clear();
    }
}

#[derive(Debug)]
struct Count {
    count: i64,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Count {
    // Async, deliberately (F2 flip): this builtin's semantics are
    // UPDATE-HISTORY-SENSITIVE — its Apply keeps state keyed to which
    // arg updated on which cycle. The fused DynCall dispatch protocol
    // re-delivers EVERY arg as a fresh update on every dispatch (the
    // kernel can't reproduce per-arg update granularity), so eager
    // dispatch mis-counts (e.g. fused `skip(#n:1, e)` saw `n` "update"
    // every cycle and never passed an event). Async = fusion boundary
    // = the node-walk runs it with exact update semantics.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "core_count";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Count { count: 0 }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Count {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if from.into_iter().fold(false, |u, n| u || n.update(ctx, event).is_some()) {
            self.count += 1;
            Some(Value::I64(self.count))
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.count = 0
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
struct Uniq(Option<Value>);

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Uniq {
    // Async, deliberately (F2 flip): this builtin's semantics are
    // UPDATE-HISTORY-SENSITIVE — its Apply keeps state keyed to which
    // arg updated on which cycle. The fused DynCall dispatch protocol
    // re-delivers EVERY arg as a fresh update on every dispatch (the
    // kernel can't reproduce per-arg update granularity), so eager
    // dispatch mis-counts (e.g. fused `skip(#n:1, e)` saw `n` "update"
    // every cycle and never passed an event). Async = fusion boundary
    // = the node-walk runs it with exact update semantics.
    const EFFECT: EffectKind = EffectKind::Async;
    const NAME: &str = "core_uniq";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Uniq(None)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Uniq {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        from[0].update(ctx, event).and_then(|v| {
            if Some(&v) != self.0.as_ref() {
                self.0 = Some(v.clone());
                Some(v)
            } else {
                None
            }
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.0 = None
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
    ) -> Option<Value> {
        for n in from {
            n.update(ctx, event);
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
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
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Dbg {
    const EFFECT: EffectKind = EffectKind::Sync;
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
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Dbg {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[0].update(ctx, event)
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        from[1].update(ctx, event).map(|v| {
            let tv = TVal { env: &ctx.env, typ: &self.typ, v: &v };
            match self.dest {
                LogDest::Stderr => {
                    eprintln!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                }
                LogDest::Stdout => {
                    println!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                }
                LogDest::Log(level) => match level {
                    Level::Trace => {
                        log::trace!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Debug => {
                        log::debug!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Info => {
                        log::info!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Warn => {
                        log::warn!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                    Level::Error => {
                        log::error!("{} dbg({}): {}", self.spec.pos, self.spec, tv)
                    }
                },
            };
            v
        })
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typecheck0(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.typ = from[1].typ().clone();
        Ok(())
    }
}

#[derive(Debug)]
struct Log {
    scope: Scope,
    dest: LogDest,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Log {
    const EFFECT: EffectKind = EffectKind::Sync;
    const NAME: &str = "core_log";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _resolved: Option<&'d FnType>,
        scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { scope: scope.clone(), dest: LogDest::Stdout }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Log {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if let Some(v) = from[0].update(ctx, event)
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        if let Some(v) = from[1].update(ctx, event) {
            let tv = TVal { env: &ctx.env, typ: from[1].typ(), v: &v };
            match self.dest {
                LogDest::Stdout => println!("{}: {}", self.scope.lexical, tv),
                LogDest::Stderr => eprintln!("{}: {}", self.scope.lexical, tv),
                LogDest::Log(lvl) => match lvl {
                    Level::Trace => log::trace!("{}: {}", self.scope.lexical, tv),
                    Level::Debug => log::debug!("{}: {}", self.scope.lexical, tv),
                    Level::Info => log::info!("{}: {}", self.scope.lexical, tv),
                    Level::Warn => log::warn!("{}: {}", self.scope.lexical, tv),
                    Level::Error => log::error!("{}: {}", self.scope.lexical, tv),
                },
            }
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

macro_rules! printfn {
    ($type:ident, $name:literal, $print:ident, $eprint:ident) => {
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
            ) -> Option<Value> {
                use std::fmt::Write;
                if let Some(v) = from[0].update(ctx, event)
                    && let Ok(d) = v.cast_to::<LogDest>()
                {
                    self.dest = d;
                }
                if let Some(v) = from[1].update(ctx, event) {
                    self.buf.clear();
                    match v {
                        Value::String(s) => write!(self.buf, "{s}"),
                        v => write!(
                            self.buf,
                            "{}",
                            TVal { env: &ctx.env, typ: &from[1].typ(), v: &v }
                        ),
                    }
                    .unwrap();
                    match self.dest {
                        LogDest::Stdout => $print!("{}", self.buf),
                        LogDest::Stderr => $eprint!("{}", self.buf),
                        LogDest::Log(lvl) => match lvl {
                            Level::Trace => log::trace!("{}", self.buf),
                            Level::Debug => log::debug!("{}", self.buf),
                            Level::Info => log::info!("{}", self.buf),
                            Level::Warn => log::warn!("{}", self.buf),
                            Level::Error => log::error!("{}", self.buf),
                        },
                    }
                }
                None
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
        }
    };
}

printfn!(Print, "core_print", print, eprint);
printfn!(Println, "core_println", println, eprintln);

// ── Package registration ───────────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
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
