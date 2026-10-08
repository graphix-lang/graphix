#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use bytes::{Buf, BufMut};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, FastFn, Node, Refs, Rt, Scope, Tag,
    TagValue, TagView, TypedFastFn, UserEvent,
    effects::Effect,
    env::Env,
    err, errf,
    expr::{Expr, ExprId},
    image::{self, ImageBuf},
    node::{coretraits, genn},
    typ::{FnType, TVal, Type},
};
use graphix_rt::GXRt;
use netidx::{path::Path, publisher::Typ, subscriber::Value};
use netidx_core::{
    pack::{Pack, PackError, decode_varint, encode_varint},
    utils::Either,
};
use netidx_value::{FromValue, ValArray};
use poolshark::local::LPooled;
use std::{
    any::Any,
    collections::VecDeque,
    fmt::{Debug, Write},
    iter,
    marker::PhantomData,
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
    let target = rtype.with_deref(|t| match t? {
        Type::Ref(tr)
            if Path::basename(&*tr.name) == Some("Result") && tr.params.len() == 2 =>
        {
            Some(tr.params[0].clone())
        }
        Type::Set(elements) if elements.len() == 2 => {
            elements.iter().find(|elem| !matches!(elem, Type::Error(_))).cloned()
        }
        _ => None,
    })?;
    without_errors(&target)
}

/// `t` with its error members gone: data is never cast into an error (a
/// checker may bind the success type to a union holding one), and a type
/// that is only errors is no target.
fn without_errors(t: &Type) -> Option<Type> {
    fn member(t: &Type) -> Option<Type> {
        t.with_deref(|d| match d? {
            Type::Error(_) => None,
            Type::Primitive(p) if p.contains(Typ::Error) => {
                let mut p = *p;
                p.remove(Typ::Error);
                (!p.is_empty()).then(|| Type::Primitive(p))
            }
            _ => Some(t.clone()),
        })
    }
    t.with_deref(|d| match d? {
        Type::Set(els) => {
            let mut kept = els.iter().filter_map(member);
            match (kept.next(), kept.next()) {
                (None, _) => None,
                (Some(one), None) => Some(one),
                (Some(a), Some(b)) => Some(Type::Set(triomphe::Arc::from_iter(
                    [a, b].into_iter().chain(kept),
                ))),
            }
        }
        _ => member(t),
    })
}

pub fn extract_cast_type(resolved_typ: Option<&FnType>) -> Option<Type> {
    cast_target(&resolved_typ?.rtype).filter(castable)
}

/// Whether a read can cast to `t`: no unbound cell and no ⊥ (which has no
/// surface syntax, so a ⊥ anywhere in it is an unconstrained cell).
pub fn castable(t: &Type) -> bool {
    fn contains_bottom(t: &Type, depth: u32) -> bool {
        if depth > 64 {
            return false;
        }
        let t = t.with_deref(|d| d.cloned()).unwrap_or_else(|| t.clone());
        match &t {
            Type::Bottom => true,
            Type::Set(els) | Type::Tuple(els) | Type::Variant(_, els, _) => {
                els.iter().any(|e| contains_bottom(e, depth + 1))
            }
            Type::Array(e) | Type::Error(e) | Type::ByRef(_, e) => {
                contains_bottom(e, depth + 1)
            }
            Type::Struct(fields) => {
                fields.iter().any(|(_, e, _)| contains_bottom(e, depth + 1))
            }
            Type::Map { key, value } => {
                contains_bottom(key, depth + 1) || contains_bottom(value, depth + 1)
            }
            _ => false,
        }
    }
    !t.has_unbound() && !contains_bottom(t, 0)
}

/// A type-directed builtin's target: the success type of its resolved
/// return type, set at init and again at `typecheck1`, imaged with it.
#[derive(Debug, Default, Clone, netidx_derive::Pack)]
pub struct CastTarget(Option<Type>);

impl CastTarget {
    pub fn of(resolved: Option<&FnType>) -> Self {
        Self(extract_cast_type(resolved))
    }

    pub fn refresh(&mut self, resolved: &FnType) {
        self.0 = extract_cast_type(Some(resolved))
    }

    /// `v` cast to the target, or as it is when there is none.
    pub fn cast(&self, env: &Env, v: Value) -> Value {
        match &self.0 {
            Some(t) => t.cast_value(env, v),
            None => v,
        }
    }

    /// What a reader whose errors are tagged `tag` hands back: its own
    /// error as it is, and data as [`Self::read_data`] does.
    pub fn read(&self, env: &Env, tag: &ArcStr, v: Value) -> Value {
        match v {
            v @ Value::Error(_) => v,
            v => self.read_data(env, tag, v),
        }
    }

    /// Data cast to the target (an error in it refused like any value the
    /// target does not admit), or with no target an error saying so.
    pub fn read_data(&self, env: &Env, tag: &ArcStr, v: Value) -> Value {
        match &self.0 {
            Some(t) => t.cast_value(env, v),
            None => errf!(tag, "no concrete return type found"),
        }
    }
}

/// A text format's input: a string, or bytes holding one.
#[derive(Debug)]
pub enum ReadInput {
    Str(ArcStr),
    Bytes(bytes::Bytes),
}

impl ReadInput {
    /// The first argument, when it is a string or bytes.
    pub fn of(cached: &CachedVals) -> Option<Self> {
        match cached.0.first()?.as_ref()? {
            Value::String(s) => Some(Self::Str(s.clone())),
            Value::Bytes(b) => Some(Self::Bytes((**b).clone())),
            _ => None,
        }
    }
}

/// A format a typed reader parses: what it reads its input from, how it
/// parses it, and the tag of its own errors.
pub trait ReadFormat: Debug + Send + Sync + 'static {
    const NAME: &str;
    const TAG: ArcStr;
    type Args: Debug + Any + Send + Sync;

    fn prepare_args(cached: &CachedVals) -> Option<Self::Args>;

    /// The parsed value, or an error tagged `TAG`.
    fn parse(args: Self::Args) -> impl Future<Output = Value> + Send;

    /// What the reader hands back for what `parse` made.
    fn read(target: &CastTarget, env: &Env, v: Value) -> Value {
        target.read(env, &Self::TAG, v)
    }
}

/// A builtin that parses its input with `F` and casts what it parsed to
/// its call site's type.
#[derive(Debug)]
pub struct TypedRead<F> {
    target: CastTarget,
    format: PhantomData<fn() -> F>,
}

impl<F> Default for TypedRead<F> {
    fn default() -> Self {
        Self { target: CastTarget::default(), format: PhantomData }
    }
}

impl<F> ImageState for TypedRead<F> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.target.encode(buf)
    }

    fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self { target: CastTarget::decode(buf)?, format: PhantomData })
    }
}

impl<F: ReadFormat> EvalCachedAsync for TypedRead<F> {
    const NAME: &str = F::NAME;
    type Args = F::Args;

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self { target: CastTarget::of(resolved), format: PhantomData }
    }

    fn typecheck1<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.target.refresh(resolved);
        Ok(())
    }

    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        v: Value,
    ) -> Option<Value> {
        Some(F::read(&self.target, &ctx.env, v))
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        F::prepare_args(cached)
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        F::parse(args)
    }
}

/// Program arguments stored in LibState. Index 0 is the script filename.
#[derive(Default, Clone)]
pub struct ProgramArgs(pub Vec<ArcStr>);

/// How `sys::exit` ends a program, seeded into `ctx.libstate` by an
/// embedder that ends in order (a display cleared, its runtime stopped)
/// and then exits with the code. Without one the process exits on the
/// spot.
#[derive(Clone)]
pub struct ExitRequest(
    triomphe::Arc<parking_lot::Mutex<Option<tokio::sync::oneshot::Sender<i32>>>>,
);

impl ExitRequest {
    pub fn new() -> (Self, tokio::sync::oneshot::Receiver<i32>) {
        let (tx, rx) = tokio::sync::oneshot::channel();
        (Self(triomphe::Arc::new(parking_lot::Mutex::new(Some(tx)))), rx)
    }

    /// Ask to exit with `code`; false when the embedder no longer listens.
    /// A program already exiting keeps its first code.
    pub fn request(&self, code: i32) -> bool {
        match self.0.lock().take() {
            Some(tx) => tx.send(code).is_ok(),
            None => true,
        }
    }
}

/// Print-capture sink, seeded into `ctx.libstate` by harnesses. When
/// present, `print`/`println`/`dbg` Stdout and Stderr output appends
/// here (exactly the bytes the stream would receive) instead of the
/// process streams. Log destinations are unaffected.
#[derive(Debug, Default, Clone)]
pub struct PrintSink(triomphe::Arc<parking_lot::Mutex<SinkBuf>>);

/// The captured text with the end offset of every cycle that wrote.
#[derive(Debug, Default)]
struct SinkBuf {
    text: String,
    marks: Vec<(u64, usize)>,
}

impl PrintSink {
    fn push(&self, cycle: u64, line: &str, suffix: &str) {
        let mut b = self.0.lock();
        b.text.push_str(line);
        b.text.push_str(suffix);
        let end = b.text.len();
        match b.marks.last_mut() {
            Some((c, at)) if *c == cycle => *at = end,
            _ => b.marks.push((cycle, end)),
        }
    }

    /// Take the captured text, leaving the sink empty.
    pub fn take(&self) -> String {
        let mut b = self.0.lock();
        b.marks.clear();
        std::mem::take(&mut b.text)
    }

    /// Take the text written in cycles up to and including `cycle`, each
    /// cycle's apart; later writes stay in the sink.
    pub fn take_through(&self, cycle: u64) -> Vec<(u64, String)> {
        let mut b = self.0.lock();
        let keep = b.marks.iter().position(|(c, _)| *c > cycle).unwrap_or(b.marks.len());
        let at = keep.checked_sub(1).map_or(0, |i| b.marks[i].1);
        let rest = b.text.split_off(at);
        let taken = std::mem::replace(&mut b.text, rest);
        let mut start = 0;
        let cycles = b
            .marks
            .drain(..keep)
            .map(|(c, end)| {
                let text = taken[start..end].to_string();
                start = end;
                (c, text)
            })
            .collect();
        for (_, end) in b.marks.iter_mut() {
            *end -= at;
        }
        cycles
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

pub mod handler;
pub mod memo;
pub mod testing;

pub use handler::{Handler, Reply};
pub use memo::FastMemo;

/// A map key as a text format (JSON, TOML, a template's data) writes it:
/// a string bare, any other value in its naked form.
pub fn map_key(k: &Value) -> String {
    match k {
        Value::String(s) => s.to_string(),
        k => k.to_string_naked(),
    }
}

/// Check if a Value is a struct-shaped array: non-empty, every element is
/// a 2-element array with a string first element, keys sorted ascending.
// CR claude for claude: [bug] is_struct decides from the value alone, but a struct, an
// Array<(string, T)> and an array of one-payload variants share one encoding. So json
// and toml write_str/write_bytes and hbs::render write one type as an object or as an
// array depending on its contents. `[("a", 1), ("b", 2)]` writes `{"a":1,"b":2}` and
// `[("b", 1), ("a", 2)]` writes `[["b",1],["a",2]]`; toml::write_str accepts the first
// and refuses the second; and the hbs template `{{#each
// p}}{{this.[0]}}={{this.[1]}};{{/each}}` renders `=;=;` for the first. The writers
// take `value: Any` and a FastCall sees no argument type, so they need the site's
// argument type to serialize by type, as the readers use the return type; that would
// also write a List as a flat array instead of its cons cells. probe:
// design/review-2026-10-05/repro/small-pkgs-11.gx (small-pkgs-11)
// 2026-10-07 claude: needs the site's argument type at the fast call: TypedFastFn
// carries only the resolved return type, so this wants a fast-call form carrying the
// argument types, in the emitter as well as the node-walk. Deferred to the fusion
// batch.
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
    ctx: &mut ExecCtx<'_, R, E>,
    node: &mut Node<R, E>,
) -> (Option<Value>, bool) {
    match seam_value(node.update(ctx)) {
        Some(tv) => {
            let fired = tv.is_fired();
            (Some(tv.value_cloned()), fired)
        }
        None => (None, false),
    }
}

/// What a builtin's argument update means for its invocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Invocation {
    /// nothing fired
    Quiet,
    /// an argument fired and every argument holds a value
    Fired,
    /// an argument is bottom, so the invocation is: freshly when an
    /// argument fired or tainted this cycle (design/dense_delivery.md R3)
    Bottom { fresh: bool },
}

#[derive(Debug)]
pub struct CachedVals(pub Box<[Option<Value>]>, pub Box<[Tag]>);

/// A builtin payload's image: its state after `init` and the typecheck
/// passes, restored in the context it was built in. `unit_image_state!`
/// implements it for a payload with no state, `pack_image_state!` for
/// one whose state is its `Pack` encoding.
pub trait ImageState: Sized {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError>;
    fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError>;
}

/// Write `v` through the reference `r`: a place reference (`&s.f`,
/// `&a[i]`) patches its root at its path, any other writes the binding
/// its reference chain names.
pub(crate) fn write_through<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    r: BindId,
    v: Value,
) {
    match ctx.rt.ref_path(&r).cloned() {
        Some((root, path)) => ctx.rt.patch_var(root, path, v),
        None => {
            let target = ctx.env.byref_chain.get(&r).copied().unwrap_or(r);
            ctx.rt.set_var(target, v)
        }
    }
}

/// How `iter` and `iterq` walk a collection: a cursor over a value's
/// elements, one at a time.
pub trait Elements: Debug + Send + Sync + 'static {
    const ITER: &str;
    const ITERQ: &str;
    type Cursor: Debug + Send + Sync + 'static;

    /// A cursor at the first element, or `None` when `v` holds none.
    fn cursor(v: Value) -> Option<Self::Cursor>;

    /// The cursor's element, the cursor moved past it; `None` at the end.
    fn next(c: &mut Self::Cursor) -> Option<Value>;
}

/// `iter`: every element of each collection that fires, in one cycle.
#[derive(Debug)]
pub struct Iter<C> {
    id: BindId,
    top_id: ExprId,
    out: TagValue,
    elems: PhantomData<fn() -> C>,
}

impl<R: Rt, E: UserEvent, C: Elements> BuiltIn<R, E> for Iter<C> {
    const NAME: &str = C::ITER;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, out: TagValue::phantom(), elems: PhantomData }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, out: TagValue::phantom(), elems: PhantomData }))
    }
}

impl<R: Rt, E: UserEvent, C: Elements> Apply<R, E> for Iter<C> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if let Some(v) = seam_tick(from[0].update(ctx)).map(|tv| tv.value_cloned())
            && let Some(mut c) = C::cursor(v)
        {
            while let Some(e) = C::next(&mut c) {
                if ctx.interrupted() {
                    return self.out.ride();
                }
                ctx.rt.set_var(self.id, e);
            }
        }
        match ctx.event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.out = TagValue::phantom();
    }
}

/// `iterq`: one element per `#clock` fire, the collections queued in the
/// order they fired.
#[derive(Debug)]
pub struct IterQ<C: Elements> {
    triggered: usize,
    queue: VecDeque<C::Cursor>,
    id: BindId,
    top_id: ExprId,
    out: TagValue,
}

impl<R: Rt, E: UserEvent, C: Elements> BuiltIn<R, E> for IterQ<C> {
    const NAME: &str = C::ITERQ;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        let queue = VecDeque::new();
        Ok(Box::new(Self { triggered: 0, queue, id, top_id, out: TagValue::phantom() }))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        let queue = VecDeque::new();
        Ok(Box::new(Self { triggered: 0, queue, id, top_id, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent, C: Elements> Apply<R, E> for IterQ<C> {
    /// The queue and the banked fires exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.triggered > 0 || !self.queue.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx)).is_some() {
            self.triggered += 1;
        }
        if let Some(v) = seam_tick(from[1].update(ctx)).map(|tv| tv.value_cloned())
            && let Some(c) = C::cursor(v)
        {
            self.queue.push_back(c);
        }
        while self.triggered > 0
            && let Some(c) = self.queue.front_mut()
        {
            if ctx.interrupted() {
                return self.out.ride();
            }
            match C::next(c) {
                Some(e) => {
                    ctx.rt.set_var(self.id, e);
                    self.triggered -= 1;
                }
                None => {
                    self.queue.pop_front();
                }
            }
        }
        match ctx.event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.queue.clear();
        self.triggered = 0;
        self.out = TagValue::phantom();
    }
}

/// A pure builtin whose eval is its fast fn: the unit evaluator `$ev`,
/// its `CachedArgs` alias `$alias`, its name and its fast fn.
#[macro_export]
macro_rules! fast_builtin {
    ($(#[$meta:meta])* $vis:vis $alias:ident, $ev:ident, $name:literal, $fc:path) => {
        $(#[$meta])*
        #[derive(Debug, Default)]
        $vis struct $ev;
        $crate::unit_image_state!($ev);

        impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>
            $crate::EvalCached<R, E> for $ev
        {
            const EFFECT: ::graphix_compiler::effects::Effect =
                ::graphix_compiler::effects::Effect::Stateless(Some(
                    ::graphix_compiler::FastCall::Plain($fc),
                ));
            const NAME: &str = $name;

            fn eval(
                &mut self,
                ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                from: &$crate::CachedVals,
            ) -> Option<::netidx_value::Value> {
                $crate::fast_eval(ctx, $fc, from)
            }
        }

        $vis type $alias = $crate::CachedArgs<$ev>;
    };
}

/// [`ImageState`] for a unit struct; the destructuring fails to
/// compile for a payload that has fields.
#[macro_export]
macro_rules! unit_image_state {
    ($($t:ident),* $(,)?) => {$(
        impl $crate::ImageState for $t {
            fn image_encode(
                &self,
                _buf: &mut ::graphix_compiler::image::ImageBuf,
            ) -> ::std::result::Result<(), ::netidx_core::pack::PackError> {
                let $t = self;
                Ok(())
            }

            fn image_decode<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                _buf: &mut &[u8],
            ) -> ::std::result::Result<Self, ::netidx_core::pack::PackError> {
                Ok($t)
            }
        }
    )*};
}

/// [`ImageState`] for a payload whose state is its `Pack` encoding
/// (`#[derive(netidx_derive::Pack)]` on a struct with fields).
#[macro_export]
macro_rules! pack_image_state {
    ($($t:ident),* $(,)?) => {$(
        impl $crate::ImageState for $t {
            fn image_encode(
                &self,
                buf: &mut ::graphix_compiler::image::ImageBuf,
            ) -> ::std::result::Result<(), ::netidx_core::pack::PackError> {
                ::netidx_core::pack::Pack::encode(self, buf)
            }

            fn image_decode<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                buf: &mut &[u8],
            ) -> ::std::result::Result<Self, ::netidx_core::pack::PackError> {
                ::netidx_core::pack::Pack::decode(buf)
            }
        }
    )*};
}

impl CachedVals {
    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.0.len() as u64, buf);
        for v in self.0.iter() {
            v.encode(buf)?;
        }
        for t in self.1.iter() {
            t.bits().encode(buf)?;
        }
        Ok(())
    }

    pub fn image_decode(buf: &mut &[u8]) -> Result<Self, PackError> {
        let n = decode_varint(buf)? as usize;
        let vals = (0..n)
            .map(|_| <Option<Value> as Pack>::decode(buf))
            .collect::<Result<_, _>>()?;
        let tags = (0..n)
            .map(|_| u8::decode(buf).map(Tag::from_raw))
            .collect::<Result<_, _>>()?;
        Ok(CachedVals(vals, tags))
    }

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

    /// Update the slots from the arg nodes (a stale production refreshes
    /// its slot silently; a tainted one marks the slot's tag but keeps the
    /// value) and say what that means for the invocation.
    pub fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> Invocation {
        let triggered = self.update_full(ctx, from).is_some_and(|t| t.triggers());
        if self.any_bottom() {
            Invocation::Bottom { fresh: triggered }
        } else if triggered {
            Invocation::Fired
        } else {
            Invocation::Quiet
        }
    }

    /// [`Self::update`] with the full production summary: `None` = no
    /// production; `Some(tag)` = TAINT if any tainted, else FIRED if any
    /// fired, else STALE.
    pub fn update_full<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> Option<Tag> {
        let mut prod: Option<Tag> = None;
        for (i, src) in from.iter_mut().enumerate() {
            let tv = src.update(ctx);
            let tag = tv.tag();
            if tag.is_bottom() {
                self.1[i] = Tag::STALE_BOTTOM;
            } else {
                // CR claude for claude: [perf] Every update stores a fresh clone of each
                // argument that is not bottom, stale ones included. So in a quiet cycle
                // every builtin call bumps and drops the refcount of each string, bytes
                // or array argument just to store the value its slot already holds. A
                // stale production whose value_words (graphix-compiler/src/tval.rs:104)
                // equal the slot's can leave the slot alone. fast_eval then clones
                // every slot once more into an LPooled<Vec<Value>> on each eval
                // (fast_args, line 524), because the slots are Option<Value> and a fast
                // fn takes &[Value]. (x-alloc-10)
                // 2026-10-07 claude: a slot whose value has the same words keeps
                // its value, so a quiet cycle clones nothing here. fast_eval's
                // second clone stands: it needs slots a fast fn can borrow.
                let slot = &mut self.0[i];
                tv.with_value(|v| {
                    let same = |old: &Value| {
                        graphix_compiler::tval::value_words(old)
                            == graphix_compiler::tval::value_words(v)
                    };
                    if !slot.as_ref().is_some_and(same) {
                        *slot = Some(v.clone());
                    }
                });
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

impl Pack for FireOnce {
    fn encoded_len(&self) -> usize {
        1
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.0.encode(buf)
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        Ok(Self(bool::decode(buf)?))
    }
}

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
pub fn fast_eval<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    f: FastFn,
    from: &CachedVals,
) -> Option<Value> {
    let args = fast_args(from)?;
    coretraits::with_hooks(ctx, || f(&args))
}

/// [`fast_eval`] for a `FastCall::Typed` fn: `typ` is the call site's
/// resolved return type (`resolved.rtype` from `typecheck1`).
pub fn fast_eval_typed<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    f: TypedFastFn,
    typ: &Type,
    from: &CachedVals,
) -> Option<Value> {
    let args = fast_args(from)?;
    coretraits::with_display_hooks(ctx, |env| f(env, typ, &args))
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
    let sort = |buf: &mut LPooled<Vec<Value>>| match (dir, numeric) {
        ("Ascending", true) => Some(buf.sort_by(|a, b| cn(a).cmp(&cn(b)))),
        ("Ascending", false) => Some(buf.sort()),
        ("Descending", true) => Some(buf.sort_by(|a, b| cn(b).cmp(&cn(a)))),
        ("Descending", false) => Some(buf.sort_by(|a, b| b.cmp(a))),
        _ => None,
    };
    // an Ord that is no total order (a user impl) may panic the sort,
    // which leaves every element in the buffer: its order is wrong, and
    // the runtime goes on
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| sort(&mut buf))) {
        Ok(None) => None,
        Ok(Some(())) | Err(_) => Some(buf),
    }
}

/// A builtin over cached arguments. `eval` runs unarmed: a fast fn is
/// armed by [`fast_eval`] (it sees only its arguments), and an `eval`
/// that compares or prints values itself takes the loan
/// (`coretraits::with_hooks`, `with_display_hooks`) around
/// that operation, with nothing of the context inside, so a core-trait
/// implementation on an abstract value applies.
pub trait EvalCached<R: Rt, E: UserEvent>:
    Debug + Default + Send + Sync + ImageState + 'static
{
    const NAME: &str;
    /// The builtin's classification — see `graphix_compiler::Effect`.
    const EFFECT: Effect = Effect::Async;
    /// See `graphix_compiler::BuiltIn::ORDERED`.
    const ORDERED: bool = false;

    fn init(
        _ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self::default()
    }

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value>;

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
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
    const ORDERED: bool = T::ORDERED;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
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

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let woke_pending = bool::decode(buf)?;
        let cached = CachedVals::image_decode(buf)?;
        let t = T::image_decode(ctx, buf)?;
        Ok(Box::new(Self { woke_pending, cached, last_result: TagValue::phantom(), t }))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCached<R, E>> Apply<R, E> for CachedArgs<T> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.woke_pending.encode(buf)?;
        self.cached.image_encode(buf)?;
        self.t.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.woke_pending);
        let (ev, cached, last_result) =
            (&mut self.t, &mut self.cached, &mut self.last_result);
        Self::update_inner(ev, cached, last_result, woke, ctx, from)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.t.typecheck0(ctx, from)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.t.typecheck1(ctx, from, resolved)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.woke_pending = true;
    }
}

pub trait EvalCachedAsync: Debug + Default + Send + Sync + ImageState + 'static {
    const NAME: &str;

    type Args: Debug + Any + Send + Sync;

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self::default()
    }

    /// Take runtime state `init` could not reach (it compiles, the
    /// runtime is not there); runs at each update, before `prepare_args`.
    fn attach<R: Rt, E: UserEvent>(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    /// map the final value with access to self and ctx
    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<'_, R, E>,
        v: Value,
    ) -> Option<Value> {
        Some(v)
    }

    fn typecheck0<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
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
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &'a TagValue
    where
        T: EvalCached<R, E>,
    {
        match cached.update_full(ctx, from) {
            None => last_result.ride(),
            // a bottom arg bottoms the invocation without calling eval
            Some(t) if cached.any_bottom() => last_result.set_bottom(t.triggers()),
            Some(t) if t.is_fired() => match ev.eval(ctx, cached) {
                Some(v) => last_result.set(TagValue::fired(v)),
                // no value this cycle, as a kernel's fast call reads it
                None => last_result.set_bottom(true),
            },
            Some(_) if !last_result.tag().is_bottom() => {
                // Wake catch-up: args may have drifted while asleep. A
                // stateless eval re-runs from the present slots; a
                // stateful one must not (its last result is its state).
                if T::EFFECT.is_stateless() && woke {
                    match ev.eval(ctx, cached) {
                        Some(v) => last_result.set(TagValue::stale(v)),
                        None => last_result.set_bottom(false),
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
    /// Set by `sleep()`, taken by the next update: the operation starts
    /// again over its present arguments.
    slept: bool,
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
        ctx: &'a mut CompileCtx<R, E>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        let t = CachedArgsAsync::<T> {
            slept: false,
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

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let slept = bool::decode(buf)?;
        let cached = CachedVals::image_decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let t = T::image_decode(ctx, buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self {
            slept,
            cached,
            id,
            top_id,
            queued: VecDeque::new(),
            running: false,
            out: TagValue::phantom(),
            t,
        }))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCachedAsync> Apply<R, E> for CachedArgsAsync<T> {
    /// The queue holds arguments already prepared for `eval`, and a
    /// running eval is a task, which exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.queued.is_empty() || self.running {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.slept.encode(buf)?;
        self.cached.image_encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.t.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        self.t.attach(ctx);
        let woke = std::mem::take(&mut self.slept);
        let invocation = self.cached.update(ctx, from);
        let start = match invocation {
            Invocation::Fired => true,
            Invocation::Quiet => woke,
            Invocation::Bottom { .. } => false,
        };
        if start && let Some(args) = self.t.prepare_args(&self.cached) {
            self.queued.push_back(args);
        }
        let res = ctx.event.variables.remove(&self.id).and_then(|tv| {
            self.running = false;
            self.t.map_value(ctx, tv.value())
        });
        // CR claude for eric: [bug] A call site runs one request at a time, FIFO, and
        // nothing cancels or replaces the one in flight. spawn_var keeps no abort
        // handle, and sleep and delete only drop the reply id. So a request whose args
        // are already stale blocks every later one for as long as it waits on a peer or
        // a resource. wait on a replaced Proc reports nothing until the old child
        // exits, then reports the old child's status while c names the new one. accept
        // on a replaced listener keeps the old port bound and never accepts on the new
        // listener until something connects to the old port. One client that never
        // sends a ClientHello stalls every later tls::accept at the call site, and
        // there is no handshake timeout. probe:
        // design/review-2026-10-05/repro/sys-io-03.gx (sys-io-03)
        if !self.running
            && let Some(args) = self.queued.pop_front()
        {
            self.running = true;
            let id = self.id;
            // a panicking eval still answers, so the site runs its next call
            let eval = futures::FutureExt::catch_unwind(std::panic::AssertUnwindSafe(
                T::eval(args),
            ));
            ctx.rt.spawn_var(async move {
                let v = eval.await.unwrap_or_else(|_| {
                    errf!(arcstr::literal!("Panicked"), "{} panicked", T::NAME)
                });
                (id, v)
            });
        }
        match res {
            // a completed reply from a prior invocation wins the cycle
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match invocation {
                Invocation::Bottom { fresh } => self.out.set_bottom(fresh),
                Invocation::Quiet | Invocation::Fired => self.out.ride(),
            },
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.t.typecheck0(ctx, from)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.t.typecheck1(ctx, from, resolved)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // the reply id is this call's alone, and so is what was stored for it
        ctx.release_var(self.id, self.top_id);
        ctx.rt.store_remove(&self.id);
        self.queued.clear();
        self.cached.clear();
    }

    // XCR claude for claude: [bug] Sleep drops the timer's timeout and repeat, and update
    // rebuilds them only from a fired timeout. At an arm's wake, a binding or parameter
    // argument arrives stale, so `timer(interval, true)` in a re-selected arm never
    // fires again, while `timer(duration:3.ms, true)` restarts because constants fire
    // at the wake. AfterIdle::sleep (line 144) and CachedArgsAsync::sleep
    // (graphix-package-core/src/lib.rs:936) have the same hole: they reset their output
    // on the premise that the operation restarts on wake, nothing restarts it over
    // level arguments, and the arm stays bottom for good (json::read(doc),
    // sys::fs::read_all(p), after_idle(d, v)). Subscribe and Publish handle this with a
    // slept bit that makes the first update after sleep act on the present arguments;
    // these three need the same. Both engines agree, so the fuzzer cannot see it.
    // Probe: design/review-2026-10-05/repro/x-engine-firing-04.gx. (x-engine-firing-04)
    // 2026-10-07 claude: a slept bit in all three makes the first update after a wake
    // act on the present arguments: Timer starts a run, AfterIdle starts its wait, and
    // CachedArgsAsync runs the operation again (an effect too: a write in a reselected
    // arm writes again). design/async_sleep_outputs.md and CLAUDE.md say so. The repro
    // prints every arm again after Paused -> Live; the wake tests in lang/async_restart
    // pass unchanged, so a pin for the level-argument case is still owed.
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.delete(ctx);
        self.slept = true;
        self.running = false;
        self.out = TagValue::phantom();
        let id = BindId::new();
        ctx.rt.ref_var(id, self.top_id);
        self.id = id;
    }
}

fn fc_is_err(args: &[Value]) -> Option<Value> {
    match args {
        [v] => Some(Value::Bool(matches!(v, Value::Error(_)))),
        _ => None,
    }
}

crate::fast_builtin!(IsErr, IsErrEv, "core_is_err", fc_is_err);

#[derive(Debug, Default)]
struct FilterErr {
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for FilterErr {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = (ctx, buf);
        Ok(Box::new(FilterErr::default()))
    }

    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_filter_err";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let _ = buf;
        Ok(())
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        match seam_tick(from[0].update(ctx)).and_then(|tv| match tv.value_cloned() {
            v @ Value::Error(_) => Some(v),
            _ => None,
        }) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
}

fn fc_error(args: &[Value]) -> Option<Value> {
    Some(Value::Error(args[0].clone().into()))
}

crate::fast_builtin!(ToError, ToErrorEv, "core_error", fc_error);

#[derive(Debug)]
struct Once {
    val: bool,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Once {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = ctx;
        Ok(Box::new(Once { val: bool::decode(buf)?, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_once";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.val.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let res = match from {
            [s] => seam_tick(s.update(ctx)).and_then(|tv| {
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

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.val = false;
        self.out = TagValue::phantom();
    }
}

// XCR claude for claude: [bug] sleep() clears the configured #n together with the
// running count, and update reloads n only from a FIRED #n (line 1147). A level #n
// (a let, a parameter, a callee's argument) is therefore lost for good once the arm
// sleeps: take passes nothing ever again, and Skip (1215, 1234) passes everything.
// A literal #n restarts only because a constant re-fires at the wake. The
// fired-only seed also leaves take dead from birth when a sibling arm consumed #n's
// fire before this arm's first selection. Keep the last #n read through seam_value
// across sleep and have sleep() reset only the remaining count; Throttle::sleep
// zeroes its #rate the same way. probe:
// design/review-2026-10-05/repro/x-engine-firing-05.gx (x-engine-firing-05)
// 2026-10-07 claude: seed_count takes a fired #n as a restart and a standing one
// when no count runs, so sleep() forgets only the count; Throttle keeps its rate
// across sleep and takes a standing rate it has not seen. The repro's level and
// literal #n now agree in every epoch, under x-engine-firing-07's rule (a restarted
// skip shows nothing until it passes): ... [] [] [7, 7, 8, 8]. No pin checks the
// values (fuzz pins check engine agreement); wants a soak.
/// Set the count `#n` gives take and skip: a fired `#n` restarts it, and
/// a standing one seeds a count not running (after a sleep, or at a birth
/// whose fire a sibling consumed), so a level `#n` survives a sleep.
fn seed_count(n: &TagValue, left: &mut Option<usize>) {
    if let Some(tv) = seam_value(n)
        && (tv.is_fired() || left.is_none())
        && let Ok(n) = tv.value_cloned().cast_to::<i64>()
    {
        // a negative count is none
        *left = Some(n.max(0) as usize)
    }
}

/// `take` (`TAKE`) passes `#n` updates and drops the rest; `skip` drops
/// `#n` updates and passes the rest.
#[derive(Debug)]
struct Counted<const TAKE: bool> {
    n: Option<usize>,
    out: TagValue,
}

type Take = Counted<true>;
type Skip = Counted<false>;

impl<R: Rt, E: UserEvent, const TAKE: bool> BuiltIn<R, E> for Counted<TAKE> {
    fn image_decode(
        _ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let n = Option::<u64>::decode(buf)?.map(|n| n as usize);
        Ok(Box::new(Self { n, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = if TAKE { "core_take" } else { "core_skip" };

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Self { n: None, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent, const TAKE: bool> Apply<R, E> for Counted<TAKE> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.n.map(|n| n as u64).encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        seed_count(from[0].update(ctx), &mut self.n);
        let res = seam_tick(from[1].update(ctx)).and_then(|tv| {
            let counting = matches!(self.n, Some(n) if n > 0);
            if let Some(n) = &mut self.n
                && *n > 0
            {
                *n -= 1;
            }
            // take passes while its count runs; skip once it has run out,
            // or when it has none
            (counting == TAKE).then(|| tv.value_cloned())
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.n = None;
        self.out = TagValue::phantom();
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

crate::fast_builtin!(All, AllEv, "core_all", fc_all);

/// The arguments with every array spread into its elements.
fn flat(args: &[Value]) -> impl Iterator<Item = Value> + '_ {
    args.iter().flat_map(|v| v.clone().flatten())
}

/// `op` over the flattened arguments, left to right. A failed step (a
/// division by zero, an overflow) is logged and the result is nothing,
/// as the `/` operator does, never an error the type does not admit.
fn arith(name: &str, args: &[Value], op: fn(Value, Value) -> Value) -> Option<Value> {
    let mut acc: Option<Value> = None;
    for v in flat(args) {
        let next = match acc {
            None => v,
            Some(l) => op(l, v),
        };
        if let Value::Error(e) = &next {
            log::error!("{name}: {e}");
            return None;
        }
        acc = Some(next);
    }
    acc
}

fn fc_sum(args: &[Value]) -> Option<Value> {
    arith("sum", args, |l, r| l + r)
}

fn fc_product(args: &[Value]) -> Option<Value> {
    arith("product", args, |l, r| l * r)
}

fn fc_divide(args: &[Value]) -> Option<Value> {
    arith("divide", args, |l, r| l / r)
}

/// The least (`keep` = Less) or greatest argument, each compared as a
/// whole value, as `fn(a: 'a, @args: 'a) -> 'a` promises.
fn extremum(args: &[Value], keep: std::cmp::Ordering) -> Option<Value> {
    let mut res = args.first()?;
    for v in &args[1..] {
        if v.partial_cmp(res) == Some(keep) {
            res = v
        }
    }
    Some(res.clone())
}

fn fc_min(args: &[Value]) -> Option<Value> {
    extremum(args, std::cmp::Ordering::Less)
}

fn fc_max(args: &[Value]) -> Option<Value> {
    extremum(args, std::cmp::Ordering::Greater)
}

fn fc_and(args: &[Value]) -> Option<Value> {
    Some(Value::Bool(flat(args).all(|v| v == Value::Bool(true))))
}

fn fc_or(args: &[Value]) -> Option<Value> {
    Some(Value::Bool(flat(args).any(|v| v == Value::Bool(true))))
}

crate::fast_builtin!(Sum, SumEv, "core_sum", fc_sum);
crate::fast_builtin!(Product, ProductEv, "core_product", fc_product);
crate::fast_builtin!(Divide, DivideEv, "core_divide", fc_divide);
crate::fast_builtin!(Min, MinEv, "core_min", fc_min);
crate::fast_builtin!(Max, MaxEv, "core_max", fc_max);
crate::fast_builtin!(And, AndEv, "core_and", fc_and);
crate::fast_builtin!(Or, OrEv, "core_or", fc_or);

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

crate::fast_builtin!(BitAnd, BitAndEv, "core_bit_and", fc_bit_and);

crate::fast_builtin!(BitOr, BitOrEv, "core_bit_or", fc_bit_or);

crate::fast_builtin!(BitXor, BitXorEv, "core_bit_xor", fc_bit_xor);

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

crate::fast_builtin!(BitNot, BitNotEv, "core_bit_not", fc_bit_not);

crate::fast_builtin!(Shl, ShlEv, "core_shl", fc_shl);

crate::fast_builtin!(Shr, ShrEv, "core_shr", fc_shr);

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
        ctx: &'a mut CompileCtx<R, E>,
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

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let pred = image::decode_node(ctx, buf)?;
        let pending = Pack::decode(buf)?;
        let fid = BindId::decode(buf)?;
        let x = BindId::decode(buf)?;
        Ok(Box::new(Self { pred, pending, fid, x, out: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Filter<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.pred.image_encode(buf)?;
        self.pending.encode(buf)?;
        self.fid.encode(buf)?;
        self.x.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            ctx.rt.store_insert(self.fid, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.fid, TagValue::tagged(v, tag));
        }
        if let Some(tv) = seam_value(from[0].update(ctx)) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            self.pending = Some(v.clone());
            ctx.rt.store_insert(self.x, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.x, TagValue::tagged(v, tag));
        }
        let res = seam_tick(self.pred.update(ctx)).and_then(|b| match b.value_cloned() {
            Value::Bool(true) => self.pending.clone(),
            _ => None,
        });
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)?;
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_remove(&self.fid);
        ctx.rt.store_remove(&self.x);
        ctx.env.unbind_variable(self.x);
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
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
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Queue {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let triggered = decode_varint(buf)? as usize;
        let queue = Vec::<Value>::decode(buf)?.into();
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { triggered, queue, id, top_id, out: TagValue::phantom() }))
    }

    const NAME: &str = "core_queue";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        match from {
            [_, _, _] => {
                let id = BindId::new();
                ctx.record_ref(id, top_id);
                Ok(Box::new(Self {
                    triggered: 0,
                    queue: VecDeque::new(),
                    id,
                    top_id,
                    out: TagValue::phantom(),
                }))
            }
            _ => bail!("queue: expected three arguments (#clock, #flush, v)"),
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Queue {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.triggered as u64, buf);
        encode_varint(self.queue.len() as u64, buf);
        for v in &self.queue {
            v.encode(buf)?;
        }
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx)).is_some() {
            self.triggered += 1;
        }
        if seam_tick(from[1].update(ctx)).is_some() {
            self.queue.clear();
        }
        if let Some(tv) = seam_tick(from[2].update(ctx)) {
            self.queue.push_back(tv.value_cloned());
        }
        while self.triggered > 0 && self.queue.len() > 0 {
            self.triggered -= 1;
            ctx.rt.set_var(self.id, self.queue.pop_front().unwrap());
        }
        match ctx.event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.triggered = 0;
        self.queue.clear();
        self.out = TagValue::phantom();
    }
}

#[derive(Debug)]
struct Hold {
    triggered: usize,
    current: Option<Value>,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Hold {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = ctx;
        let triggered = decode_varint(buf)? as usize;
        let current = Pack::decode(buf)?;
        Ok(Box::new(Self { triggered, current, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_hold";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.triggered as u64, buf);
        self.current.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if seam_tick(from[0].update(ctx)).is_some() {
            self.triggered += 1;
        }
        if let Some(tv) = seam_tick(from[1].update(ctx)) {
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

    fn delete(&mut self, _: &mut ExecCtx<'_, R, E>) {}

    fn sleep(&mut self, _: &mut ExecCtx<'_, R, E>) {
        self.triggered = 0;
        self.current = None;
        self.out = TagValue::phantom();
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
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let args = CachedVals::image_decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self { id, top_id, args, out: TagValue::phantom() }))
    }

    const NAME: &str = "core_seq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.args.image_encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let invocation = self.args.update(ctx, from);
        // a bottomed argument bottoms the invocation and issues nothing
        if let Invocation::Bottom { fresh } = invocation {
            return self.out.set_bottom(fresh);
        }
        if invocation == Invocation::Fired {
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
        match ctx.event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => self.out.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
        self.out = TagValue::phantom();
    }
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
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = ctx;
        let wait = Duration::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let last_v = Pack::decode(buf)?;
        Ok(Box::new(Self {
            wait,
            last: None,
            tid: None,
            top_id,
            last_v,
            out: TagValue::phantom(),
        }))
    }

    const NAME: &str = "core_throttle";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // a running timer and its wall-clock mark exist only once a cycle ran
        if self.last.is_some() || self.tid.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.wait.encode(buf)?;
        self.top_id.encode(buf)?;
        self.last_v.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
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
        // a standing rate this throttle has not taken (a sibling consumed
        // its fire) is taken as a fired one is
        let new_wait = seam_value(from[0].update(ctx))
            .and_then(|tv| {
                tv.with_value(|v| match v {
                    Value::Duration(d) => Some(**d),
                    _ => None,
                })
            })
            .filter(|d| *d != self.wait);
        let mut up1 = false;
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            up1 = tv.is_fired();
            self.last_v = Some(tv.value_cloned());
        }
        if let Some(d) = new_wait {
            self.wait = d;
            if let Some(id) = self.tid.take()
                && let Some(last) = &mut self.last
            {
                ctx.release_var(id, self.top_id);
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
            && let Some(_) = ctx.event.variables.get(&id)
        {
            ctx.release_var(id, self.top_id);
            self.tid = None;
            self.last = Some(Instant::now());
            emit_cached!()
        }
        self.out.ride()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(id) = self.tid.take() {
            ctx.release_var(id, self.top_id);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.delete(ctx);
        self.last = None;
        self.last_v = None;
        self.out = TagValue::phantom();
    }
}

#[derive(Debug)]
struct Count {
    count: i64,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Count {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = ctx;
        Ok(Box::new(Count { count: i64::decode(buf)?, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_count";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.count.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if from.into_iter().fold(false, |u, n| u || seam_tick(n.update(ctx)).is_some()) {
            self.count += 1;
            self.out.set(TagValue::fired(Value::I64(self.count)))
        } else {
            self.out.ride()
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        // XCR claude for claude: [bug] sleep() restarts the count but keeps `out`. When a
        // re-selected arm's input does not fire at the wake, the arm emits the previous
        // activation's count, and the next fire counts 1, so the arm shows 2 and then
        // 1. Once, Take, Skip, Uniq and Hold do the same in their sleep: a woken
        // once(x) emits the old value and then the next x, and a woken uniq(x) emits a
        // duplicate. A fresh count in the same position shows nothing, so a restarted
        // builtin is not a fresh one; this goes against wake_catchup.md (at a wake,
        // residents are refreshed, not surfaced) and against the reasoning of
        // async_sleep_outputs.md. Either clear `out` to TagValue::phantom() in each
        // restart builtin's sleep (check the seq lowering's once, uniq and hold first),
        // or rule the surfacing intended in CLAUDE.md. probe:
        // design/review-2026-10-05/repro/x-engine-firing-07.gx (x-engine-firing-07)
        // 2026-10-07 claude: count, once, take, skip, uniq and hold set `out` to the
        // phantom in sleep(), so a woken arm shows what a fresh one would (the repro:
        // 1 at the first fire after the wake, never the old 2). The seq lowering's
        // once, uniq and hold sit inside a machine that resets on its arm's sleep.
        // No pin checks the values; a semantics change: wants review and a soak.
        self.count = 0;
        self.out = TagValue::phantom();
    }
}

fn fc_mean(args: &[Value]) -> Option<Value> {
    static TAG: ArcStr = literal!("MeanError");
    let mut total = 0.;
    let mut samples = 0;
    for v in flat(args) {
        match v.cast_to::<f64>() {
            Err(e) => return Some(errf!(TAG, "{e:?}")),
            Ok(v) => {
                total += v;
                samples += 1;
            }
        }
    }
    Some(match samples {
        0 => err!(TAG, "mean requires at least one argument"),
        n => Value::F64(total / n as f64),
    })
}

crate::fast_builtin!(Mean, MeanEv, "core_mean", fc_mean);

/// The last value, as compared; the output; the argument type when it
/// holds a reference, which compares by what it names.
#[derive(Debug)]
struct Uniq(Option<Value>, TagValue, Option<Type>);

impl Uniq {
    fn new<R: Rt, E: UserEvent>(
        ctx: &CompileCtx<R, E>,
        last: Option<Value>,
        from: &[Node<R, E>],
    ) -> Self {
        let refs = from.first().map(|n| n.typ()).filter(|t| t.compares_refs(&ctx.env));
        Self(last, TagValue::phantom(), refs.cloned())
    }
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Uniq {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        Ok(Box::new(Uniq::new(ctx, Pack::decode(buf)?, from)))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_uniq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R, E>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(Uniq::new(ctx, None, from)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Uniq {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.0.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let (last, out) = (&mut self.0, &mut self.1);
        let Some(v) = seam_tick(from[0].update(ctx)).map(|tv| tv.value_cloned()) else {
            return out.ride();
        };
        let cmp = match &self.2 {
            Some(t) => ctx.ref_targets(t, &v),
            None => v.clone(),
        };
        let changed = coretraits::with_hooks(ctx, || Some(&cmp) != last.as_ref());
        if changed {
            *last = Some(cmp);
            out.set(TagValue::fired(v))
        } else {
            out.ride()
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.0 = None;
        self.1 = TagValue::phantom();
    }
}

#[derive(Debug, Clone, Copy, netidx_derive::FromValue)]
enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

#[derive(Debug, Clone, Copy)]
enum LogDest {
    Stdout,
    Stderr,
    Log(Level),
}

impl Pack for LogDest {
    fn encoded_len(&self) -> usize {
        1
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        buf.put_u8(match self {
            Self::Stdout => 0,
            Self::Stderr => 1,
            Self::Log(Level::Trace) => 2,
            Self::Log(Level::Debug) => 3,
            Self::Log(Level::Info) => 4,
            Self::Log(Level::Warn) => 5,
            Self::Log(Level::Error) => 6,
        });
        Ok(())
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        Ok(match buf.get_u8() {
            0 => Self::Stdout,
            1 => Self::Stderr,
            2 => Self::Log(Level::Trace),
            3 => Self::Log(Level::Debug),
            4 => Self::Log(Level::Info),
            5 => Self::Log(Level::Warn),
            6 => Self::Log(Level::Error),
            _ => return Err(PackError::UnknownTag),
        })
    }
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
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = ctx;
        let spec = Expr::decode(buf)?;
        let dest = LogDest::decode(buf)?;
        let typ = Type::decode(buf)?;
        let buf_ = String::decode(buf)?;
        Ok(Box::new(Dbg { spec, dest, typ, buf: buf_, out: TagValue::phantom() }))
    }

    // CR claude for eric: [bug] dbg is an effect but is declared Stateless(None), as
    // are core_log (line 2522) and sys_time_now (sys/src/time.rs:394).
    // arm_sleeps_on_deselect counts Stateless as pure, so an arm holding only such a
    // call skips sleep, and each re-selection enters it as a birth where the standing
    // argument reads FIRED: dbg and log print again and now resamples although the
    // argument never fired, while println in the same arm shape prints once. Whether
    // the arm sleeps also depends on fusion, so the engines disagree: `{ dbg(#dest:
    // `Stdout, z); 100 }` prints once under the JIT (the fused `100` is a FusedKernel,
    // counted ASYNC) and at every entry under --no-fusion. Declaring them Sync, as
    // print/println are, makes both engines print once; the Effect::Stateless doc and
    // CLAUDE.md's "`None` for effects" say the opposite and need the same correction.
    // probe: design/review-2026-10-05/repro/x-builtin-effects-05.gx
    // (x-builtin-effects-05)
    // 2026-10-07 claude: re-addressed with core-lib-09, which asks the opposite (print
    // and println Stateless(None)). One Effect variant serves two questions: is_pure
    // (analysis.rs) decides both that an arm skips sleep and that a tail loop runs as one
    // activation. An effect without a fast call wants "may skip a tail loop's
    // activations" but "must sleep with its arm" (dbg, log, print, buffer::decode's
    // writes, x-builtin-effects-02). A fourth class, or a second predicate for arm sleep,
    // is the decision.
    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_dbg";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.spec.encode(buf)?;
        self.dest.encode(buf)?;
        self.typ.encode(buf)?;
        self.buf.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if let Some(v) = seam_value(from[0].update(ctx)).map(|tv| tv.value_cloned())
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        let Some(v) = seam_tick(from[1].update(ctx)).map(|tv| tv.value_cloned()) else {
            return self.out.ride();
        };
        self.buf.clear();
        write!(self.buf, "{} dbg({}): ", self.spec.pos, self.spec).unwrap();
        let (buf, typ) = (&mut self.buf, &self.typ);
        coretraits::with_display_hooks(ctx, |env| {
            write!(buf, "{}", TVal { env, typ, v: &v }).unwrap()
        });
        emit_line(ctx, self.dest, &self.buf, "\n");
        self.out.set(TagValue::fired(v))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R, E>,
        from: &mut [Node<R, E>],
    ) -> Result<()> {
        self.typ = from[1].typ().clone();
        Ok(())
    }
}

/// Where a print builtin's output goes this cycle, and the line it
/// writes there.
fn emit_line<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    dest: LogDest,
    line: &str,
    suffix: &str,
) {
    use std::io::Write;
    let sink = match dest {
        LogDest::Stdout | LogDest::Stderr => ctx.libstate.get::<PrintSink>(),
        LogDest::Log(_) => None,
    };
    match (dest, sink) {
        (LogDest::Stdout | LogDest::Stderr, Some(sink)) => {
            sink.push(ctx.rt.cycle(), line, suffix)
        }
        // a reader that has gone takes nothing, and the program goes on
        (LogDest::Stdout, None) => {
            let _ = write!(std::io::stdout().lock(), "{line}{suffix}");
        }
        (LogDest::Stderr, None) => {
            let _ = write!(std::io::stderr().lock(), "{line}{suffix}");
        }
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
    dest: LogDest,
    buf: String,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for Log {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        _from: &[Node<R, E>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R, E>>, PackError> {
        let _ = ctx;
        let dest = LogDest::decode(buf)?;
        let buf_ = String::decode(buf)?;
        Ok(Box::new(Self { dest, buf: buf_, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Stateless(None);
    const NAME: &str = "core_log";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R, E>,
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

impl<R: Rt, E: UserEvent> Apply<R, E> for Log {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.dest.encode(buf)?;
        self.buf.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        if let Some(v) = seam_value(from[0].update(ctx)).map(|tv| tv.value_cloned())
            && let Ok(d) = v.cast_to::<LogDest>()
        {
            self.dest = d;
        }
        let Some(v) = seam_tick(from[1].update(ctx)).map(|tv| tv.value_cloned()) else {
            return self.out.ride();
        };
        self.buf.clear();
        // the logged argument's position: where the call is
        write!(self.buf, "{}: ", from[1].spec().pos).unwrap();
        let typ = from[1].typ().clone();
        let buf = &mut self.buf;
        coretraits::with_display_hooks(ctx, |env| {
            write!(buf, "{}", TVal { env, typ: &typ, v: &v }).unwrap()
        });
        emit_line(ctx, self.dest, &self.buf, "\n");
        self.out.set(TagValue::fired(Value::Null))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
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
            // CR claude for eric: [perf] print and println are declared Sync, while dbg
            // and log, which have the same shape (a #dest config, a scratch buffer,
            // emit_line), are Stateless(None), and design/strict_fusion.md lists print
            // among the effects kept Stateless(None) so a tail loop that prints stays
            // one activation. As Sync, a tail recursion that calls println keeps an
            // activation per iteration, and #[tail_recursive] on it is refused as
            // stateful, while the same loop with dbg or log is accepted. Declare both
            // Stateless(None). Dbg, Log and printfn repeat one update body (dest from
            // arg 0, seam_tick on arg 1, format into buf, emit_line), which is how
            // their effects drifted apart; one shared body would also stop all four
            // imaging their scratch buffer. probe:
            // design/review-2026-10-05/repro/core-lib-09.gx (core-lib-09)
            // 2026-10-07 claude: re-addressed with x-builtin-effects-05 (the same choice from the
            // other side).
            const EFFECT: Effect = Effect::Sync;
            const NAME: &str = $name;

            fn image_decode(
                _ctx: &mut ExecCtx<'_, R, E>,
                _from: &[Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R, E>>, PackError> {
                let dest = LogDest::decode(buf)?;
                let buf = String::decode(buf)?;
                Ok(Box::new(Self { dest, buf, out: TagValue::phantom() }))
            }

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut CompileCtx<R, E>,
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
            fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
                self.dest.encode(buf)?;
                self.buf.encode(buf)
            }

            fn update(
                &mut self,
                ctx: &mut ExecCtx<'_, R, E>,
                from: &mut [Node<R, E>],
            ) -> &TagValue {
                if let Some(v) =
                    seam_value(from[0].update(ctx)).map(|tv| tv.value_cloned())
                    && let Ok(d) = v.cast_to::<LogDest>()
                {
                    self.dest = d;
                }
                let Some(v) = seam_tick(from[1].update(ctx)).map(|tv| tv.value_cloned())
                else {
                    return self.out.ride();
                };
                self.buf.clear();
                let typ = from[1].typ().clone();
                let buf = &mut self.buf;
                coretraits::with_display_hooks(ctx, |env| {
                    match &v {
                        Value::String(s) => write!(buf, "{s}"),
                        v => write!(buf, "{}", TVal { env, typ: &typ, v }),
                    }
                    .unwrap()
                });
                emit_line(ctx, self.dest, &self.buf, $suffix);
                self.out.set(TagValue::fired(Value::Null))
            }

            fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
        }
    };
}

printfn!(Print, "core_print", "");
printfn!(Println, "core_println", "\n");

crate::fast_builtin!(
    /// `array::len` — registered here (the array package binds the name)
    /// because core's `Collection` implementation for `Array` needs it.
    ArrayLen, ArrayLenEv, "core_array_len", array_len
);

fn array_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Array(a)] => Some(Value::I64(a.len() as i64)),
        _ => None,
    }
}

crate::fast_builtin!(
    /// `map::len` — registered here for the `Collection` implementation
    /// for `Map`; the map package binds the name.
    MapLen, MapLenEv, "core_map_len", map_len
);

fn map_len(args: &[Value]) -> Option<Value> {
    match args {
        [Value::Map(m)] => Some(Value::I64(m.len() as i64)),
        _ => None,
    }
}

/// `map::union` — the union of two maps, the second's value on a key in
/// both. In core for `Collection::flat_map` over `Map`.
fn fc_map_union(args: &[Value]) -> Option<Value> {
    match (&args[0], &args[1]) {
        (Value::Map(a), Value::Map(b)) => {
            // chunkmap's union may hand f its values in either order
            Some(Value::Map(
                a.insert_many(b.into_iter().map(|(k, v)| (k.clone(), v.clone()))),
            ))
        }
        _ => None,
    }
}

crate::fast_builtin!(MapUnion, MapUnionEv, "core_map_union", fc_map_union);

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
#[derive(Clone)]
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
    /// Load a config + auth on first use, then build as `Config` does. A
    /// load that fails fails that use; the next one loads again.
    Load {
        load: std::sync::Arc<
            dyn Fn() -> anyhow::Result<(
                    netidx::config::Config,
                    netidx::publisher::DesiredAuth,
                )> + Send
                + Sync,
        >,
        bind: Option<netidx::publisher::BindCfg>,
    },
    /// Process-internal netidx (resolver + pub/sub) built on demand.
    Internal,
}

impl std::fmt::Debug for NetConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ready { .. } => write!(f, "Ready"),
            Self::Config { config, auth, bind } => f
                .debug_struct("Config")
                .field("config", config)
                .field("auth", auth)
                .field("bind", bind)
                .finish(),
            Self::Load { bind, .. } => {
                f.debug_struct("Load").field("bind", bind).finish()
            }
            Self::Internal => write!(f, "Internal"),
        }
    }
}

/// Optional embedder-seeded netidx tuning. `publish` bounds the publish
/// flusher's batch commit: a subscriber that doesn't consume updates
/// within the timeout is dropped; None (the default) waits.
#[derive(Debug, Clone)]
pub struct NetTimeouts {
    pub publish: Option<std::time::Duration>,
}
