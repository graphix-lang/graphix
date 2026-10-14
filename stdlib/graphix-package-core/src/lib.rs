#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use bytes::{Buf, BufMut};
use futures::{
    channel::oneshot,
    future::{self, FutureExt, Shared},
};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, FastFn, IoSite, Node, Refs, Rt, Scope,
    Tag, TagValue, TagView, TypedFastFn, UserEvent, WriteTarget,
    effects::{Effect, IoCall},
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

/// How long a TLS handshake may take before the stream is dropped: the
/// standard TCP timeout. rustls has no clock of its own, so a peer that
/// goes quiet mid-handshake would otherwise hold its socket and task until
/// it hung up.
pub const TLS_HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(120);
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

/// Whether a read can cast to `t`: the checker's Concrete rule.
pub fn castable(t: &Type) -> bool {
    t.is_concrete()
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
    pub fn read(&self, env: &Env, tag: &ArcStr, flat: bool, v: Value) -> Value {
        match v {
            v @ Value::Error(_) => v,
            v => self.read_data(env, tag, flat, v),
        }
    }

    /// Data cast to the target (an error in it refused like any value the
    /// target does not admit), or with no target an error saying so. A
    /// `flat` format writes a list as a flat array, so its arrays are
    /// never read as a list's cells.
    pub fn read_data(&self, env: &Env, tag: &ArcStr, flat: bool, v: Value) -> Value {
        match &self.0 {
            Some(t) if flat => t.cast_data(env, v),
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
    /// The format writes a list as a flat array, and reads it back so.
    const FLAT_LISTS: bool;
    type Args: Debug + Any + Send + Sync;

    fn prepare_args(cached: &CachedVals) -> Option<Self::Args>;

    /// The parsed value, or an error tagged `TAG`.
    fn parse(args: Self::Args) -> impl Future<Output = Value> + Send;

    /// What the reader hands back for what `parse` made.
    fn read(target: &CastTarget, env: &Env, v: Value) -> Value {
        target.read(env, &Self::TAG, Self::FLAT_LISTS, v)
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

/// Every captured write, with the cycle that made it, in the order made.
#[derive(Debug, Default)]
struct SinkBuf(Vec<(u64, String)>);

impl PrintSink {
    fn push(&self, cycle: u64, line: &str, suffix: &str) {
        let mut text = String::with_capacity(line.len() + suffix.len());
        text.push_str(line);
        text.push_str(suffix);
        self.0.lock().0.push((cycle, text));
    }

    /// Take the captured text, leaving the sink empty.
    pub fn take(&self) -> String {
        self.0.lock().0.drain(..).map(|(_, t)| t).collect()
    }

    /// Take the writes made in cycles up to and including `cycle`, one
    /// entry a print call; later writes stay in the sink.
    pub fn take_through(&self, cycle: u64) -> Vec<(u64, String)> {
        let mut b = self.0.lock();
        let keep = b.0.iter().position(|(c, _)| *c > cycle).unwrap_or(b.0.len());
        b.0.drain(..keep).collect()
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

pub use handler::{Handler, Inbox, Reply};
pub use memo::FastMemo;

/// A map key as a text format (JSON, TOML, a template's data) writes it:
/// a string bare, any other value in its naked form.
pub fn map_key(k: &Value) -> String {
    match k {
        Value::String(s) => s.to_string(),
        k => graphix_compiler::typ::Naked(k).to_string(),
    }
}

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

/// How a writer serializes a value: an object's entries or a sequence's
/// elements, each with the type that directs it, or a leaf; or a
/// reference, which is not data and no writer writes.
pub enum Shape {
    Object(LPooled<Vec<(ArcStr, Value, Option<Type>)>>, Keys),
    Seq(LPooled<Vec<(Value, Option<Type>)>>),
    Leaf,
    Ref,
}

/// Whose keys an object's are: a struct's fields, which a cast reads
/// back as null when one is left out, or a map's, which are data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keys {
    Fields,
    Data,
}

/// The f64 a text writer gives an f32: the one its shortest decimal
/// names, which is the number the program wrote and reads back to it.
pub fn f32_as_written(n: f32) -> f64 {
    compact_str::format_compact!("{n}").parse().unwrap_or(n as f64)
}

/// `v`'s shape under `t`, the type its call site gave it: a struct or a
/// map is an object; an array, a list, a tuple or a variant's tag and
/// payload a sequence; a union reads as the first member `v` inhabits.
/// Where the type says nothing (`None`, `Any`, an open variable), a
/// non-empty array of `(string, _)` pairs in key order reads as an object.
pub fn shape(env: &Env, t: Option<&Type>, v: &Value) -> Shape {
    let typed = t.and_then(|t| constructor(env, t));
    match (typed.as_ref(), v) {
        (Some(Type::ByRef(..)), _) => Shape::Ref,
        // CR claude for claude: [perf/low] A union picks its member with a full is_a
        // walk of the value. Writers recurse through shape() level by level, so a value
        // of a recursive union type (`type J = [.., Array<J>, Map<string, J>]`)
        // re-walks its whole subtree at every level: O(nodes × depth). Fix: choose the
        // member by a shallow head test, falling back to is_a only when two members
        // share a head. Separately, the json package doubles each document.
        // json_to_value parses into a serde_json::Value and copies every string and key
        // again, and value_to_json builds a serde_json::Value tree before serializing,
        // so peak memory and allocations are about twice what they need to be. A serde
        // Visitor straight into Value, and a Serialize wrapper over (env, type, &Value)
        // that streams to the writer, would remove the intermediate tree. (x-alloc-07)
        // 2026-10-13 claude: the member is now chosen by its shallow form
        // (graphix_types::typ::member_of), is_a only where two members share
        // one. The json double-copy part stands.
        (Some(Type::Set(ts)), v) => {
            shape(env, graphix_compiler::typ::member_of(env, ts, v).map(|i| &ts[i]), v)
        }
        (Some(Type::Struct(fs)), Value::Array(a)) if fs.len() == a.len() => {
            let mut entries: LPooled<Vec<_>> = LPooled::take();
            for (f, (_, t, _)) in a.iter().zip(fs.iter()) {
                if let Value::Array(p) = f
                    && let [Value::String(k), v] = &p[..]
                {
                    entries.push((k.clone(), v.clone(), Some(t.clone())));
                }
            }
            Shape::Object(entries, Keys::Fields)
        }
        (Some(Type::Map { value, .. }), Value::Map(m)) => Shape::Object(
            m.into_iter()
                .map(|(k, v)| {
                    (ArcStr::from(map_key(k)), v.clone(), Some(Type::clone(value)))
                })
                .collect(),
            Keys::Data,
        ),
        (Some(Type::Array(e)), Value::Array(a)) => {
            Shape::Seq(a.iter().map(|x| (x.clone(), Some(Type::clone(e)))).collect())
        }
        (Some(Type::List(e)), v) if graphix_compiler::list::is_list(v) => Shape::Seq(
            graphix_compiler::list::Iter::new(v.clone())
                .map(|x| (x, Some(Type::clone(e))))
                .collect(),
        ),
        (Some(Type::Tuple(ts)), Value::Array(a)) if ts.len() == a.len() => Shape::Seq(
            a.iter().zip(ts.iter()).map(|(x, t)| (x.clone(), Some(t.clone()))).collect(),
        ),
        (Some(Type::Variant(_, ps, _)), Value::Array(a)) if a.len() == ps.len() + 1 => {
            let payload =
                a[1..].iter().zip(ps.iter()).map(|(x, t)| (x.clone(), Some(t.clone())));
            Shape::Seq(std::iter::once((a[0].clone(), None)).chain(payload).collect())
        }
        (_, v) => untyped_shape(v),
    }
}

/// `t` through its variables and names to its constructor; `None` for
/// `Any` or an open variable.
fn constructor(env: &Env, t: &Type) -> Option<Type> {
    let mut t = t.deref_cloned()?;
    while let Type::Ref(_) = &t {
        t = t.lookup_ref_peek(env).ok()?.deref_cloned()?;
    }
    match t {
        Type::Any => None,
        t => Some(t),
    }
}

fn untyped_shape(v: &Value) -> Shape {
    match v {
        Value::Array(a) if is_struct(a) => Shape::Object(
            a.iter()
                .filter_map(|f| match f {
                    Value::Array(p) => match &p[..] {
                        [Value::String(k), v] => Some((k.clone(), v.clone(), None)),
                        _ => None,
                    },
                    _ => None,
                })
                .collect(),
            Keys::Fields,
        ),
        Value::Array(a) => Shape::Seq(a.iter().map(|x| (x.clone(), None)).collect()),
        Value::Map(m) => Shape::Object(
            m.into_iter()
                .map(|(k, v)| (ArcStr::from(map_key(k)), v.clone(), None))
                .collect(),
            Keys::Data,
        ),
        _ => Shape::Leaf,
    }
}

/// What a builtin emits for `v` because of the production tagged
/// `cause`: a fire, and a wake's own when only a wake caused it.
pub fn caused(v: Value, cause: Tag) -> TagValue {
    TagValue::tagged(v, cause.fresh_or_wake())
}

/// The production of a raw builtin whose consumed values are `tags`
/// (a trigger's fire is not a value), when any of them is bottom: a
/// bottom (a fresh one if any bottom fired), which the builtin returns
/// where it would ride. An emission owed to an earlier input still
/// delivers, and the resident re-surfaces once the values return.
pub fn bottom_of(tags: impl IntoIterator<Item = Tag>) -> Option<&'static TagValue> {
    let mut bottom = None;
    for t in tags {
        if t.is_bottom() {
            bottom = Some(bottom.unwrap_or(false) || t.triggers());
        }
    }
    bottom.map(TagValue::bottom_null)
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
// CR claude for claude: [structure/low] A value slice and a tag slice must stay the
// same length, but the type does not say so, and both fields are public. `update_full`
// indexes `self.1[i]` assuming the lengths match. Store one `Box<[(Option<Value>,
// Tag)]>`, which is also one allocation per call site, and make the field private with
// a `get(i) -> Option<&Value>` accessor. That accessor replaces the
// `cached.0.get(i)?.as_ref()?` repeated across the db, gui, http, sqlite and sys
// packages. (core-lib-09)
pub struct CachedVals(pub Box<[Option<Value>]>, pub Box<[Tag]>, Evaluated);

/// Whether `eval` (or `quiet`) has run over the slots as they stand.
#[derive(Debug, Default, Clone, Copy)]
struct Evaluated(bool);

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
    match ctx.write_target(r) {
        WriteTarget::Place(root, path) => ctx.rt.patch_var(root, path, v, None),
        WriteTarget::Bind(target) => ctx.rt.set_var(target, v),
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

/// `iter`: each element of every collection that fires, one per cycle,
/// in order.
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
        // an element owed to a collection that fired earlier delivers;
        // a bottom collection stands in place of a ride
        let x = from[0].update(ctx);
        let bottom = bottom_of([x.tag()]);
        if let Some(v) = seam_tick(x).map(|tv| tv.value_cloned())
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
            None => match bottom {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.release_var(self.id, self.top_id)
    }

    // CR claude for claude: [structure/medium] About fifteen builtins (core, sys
    // net/io/watch/time, http, db, queuefn) repeat the life of a runtime variable by
    // hand. They record_ref in init and again in image_decode, re-mint on sleep
    // (release_var, BindId::new, ref_var; sys::net names it both `renew` and `remint`),
    // release at delete, and encode id and top_id. A new async builtin that copies init
    // but forgets the record_ref in decode is deaf after a warm start, and nothing in
    // the types catches it. Add a `RtVar { id, top_id }` to graphix-package-core with
    // new, image_encode, image_decode (decode plus record_ref), remint and release.
    // Hold it in place of the two fields so the re-registration rule lives in one
    // place. (x-dup-05)
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
        let fired = seam_tick(from[0].update(ctx)).is_some();
        let x = from[1].update(ctx);
        let x_tag = x.tag();
        if fired {
            self.triggered += 1;
        }
        if let Some(v) = seam_tick(x).map(|tv| tv.value_cloned())
            && let Some(c) = C::cursor(v)
        {
            self.queue.push_back(c);
        }
        let bottom = bottom_of([x_tag]);
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
            None => match bottom {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
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

/// [`fast_builtin`] for a `FastCall::Typed` fn, directed by the call
/// site's resolved function type.
#[macro_export]
macro_rules! typed_fast_builtin {
    ($(#[$meta:meta])* $vis:vis $alias:ident, $ev:ident, $name:literal, $fc:path) => {
        $(#[$meta])*
        #[derive(Debug, Default)]
        $vis struct $ev {
            resolved: Option<::graphix_compiler::typ::FnType>,
        }

        impl $crate::ImageState for $ev {
            fn image_encode(
                &self,
                buf: &mut ::graphix_compiler::image::ImageBuf,
            ) -> ::std::result::Result<(), ::netidx_core::pack::PackError> {
                ::netidx_core::pack::Pack::encode(&self.resolved, buf)
            }

            fn image_decode<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                buf: &mut &[u8],
            ) -> ::std::result::Result<Self, ::netidx_core::pack::PackError> {
                Ok(Self { resolved: ::netidx_core::pack::Pack::decode(buf)? })
            }
        }

        impl<R: ::graphix_compiler::Rt, E: ::graphix_compiler::UserEvent>
            $crate::EvalCached<R, E> for $ev
        {
            const EFFECT: ::graphix_compiler::effects::Effect =
                ::graphix_compiler::effects::Effect::Stateless(Some(
                    ::graphix_compiler::FastCall::Typed($fc),
                ));
            const NAME: &str = $name;

            fn init(
                _ctx: &mut ::graphix_compiler::CompileCtx<R, E>,
                _typ: &::graphix_compiler::typ::FnType,
                resolved: Option<&::graphix_compiler::typ::FnType>,
                _scope: &::graphix_compiler::Scope,
                _from: &[::graphix_compiler::Node<R, E>],
                _top_id: ::graphix_compiler::expr::ExprId,
            ) -> Self {
                Self { resolved: resolved.cloned() }
            }

            fn typecheck1(
                &mut self,
                _ctx: &mut ::graphix_compiler::CompileCtx<R, E>,
                _from: &mut [::graphix_compiler::Node<R, E>],
                resolved: &::graphix_compiler::typ::FnType,
            ) -> ::anyhow::Result<()> {
                self.resolved = Some(resolved.clone());
                Ok(())
            }

            fn eval(
                &mut self,
                ctx: &mut ::graphix_compiler::ExecCtx<'_, R, E>,
                from: &$crate::CachedVals,
            ) -> Option<::netidx_value::Value> {
                $crate::fast_eval_typed(ctx, $fc, self.resolved.as_ref()?, from)
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
        Ok(CachedVals(vals, tags, Evaluated(false)))
    }

    pub fn new<R: Rt, E: UserEvent>(from: &[Node<R, E>]) -> CachedVals {
        CachedVals(
            from.into_iter().map(|_| None).collect(),
            from.into_iter().map(|_| Tag::FIRED).collect(),
            Evaluated(false),
        )
    }

    pub fn clear(&mut self) {
        for v in &mut self.0 {
            *v = None
        }
        for t in &mut self.1 {
            *t = Tag::FIRED
        }
        self.2 = Evaluated(false);
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
                if !self.1[i].is_bottom() {
                    self.2 = Evaluated(false);
                }
                self.1[i] = Tag::STALE_BOTTOM;
            } else {
                let slot = &mut self.0[i];
                let evaluated = &mut self.2;
                tv.with_value(|v| {
                    let same = |old: &Value| {
                        graphix_compiler::tval::value_words(old)
                            == graphix_compiler::tval::value_words(v)
                    };
                    if !slot.as_ref().is_some_and(same) {
                        *slot = Some(v.clone());
                        *evaluated = Evaluated(false);
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

    /// Argument `i` as the abstract type `T` it holds.
    pub fn abstract_arg<T: Any + Send + Sync>(&self, i: usize) -> Option<&T> {
        match self.0.get(i)?.as_ref()? {
            Value::Abstract(a) => a.downcast_ref::<T>(),
            _ => None,
        }
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
/// resolved function type (from `typecheck1`).
pub fn fast_eval_typed<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    f: TypedFastFn,
    typ: &FnType,
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
    let descending = match dir {
        "Ascending" => false,
        "Descending" => true,
        _ => return None,
    };
    let mut buf: LPooled<Vec<Value>> = vals.collect();
    // an Ord that is no total order (a user impl) may panic the sort,
    // which leaves every element in the buffer: its order is wrong, and
    // the runtime goes on
    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if numeric {
            // each element cast once, beside it
            let mut keyed: LPooled<Vec<(Value, Value)>> = buf
                .iter()
                .map(|v| {
                    (v.clone().cast(Typ::F64).unwrap_or_else(|| v.clone()), v.clone())
                })
                .collect();
            if descending {
                keyed.sort_by(|a, b| b.0.cmp(&a.0))
            } else {
                keyed.sort_by(|a, b| a.0.cmp(&b.0))
            }
            buf.clear();
            buf.extend(keyed.drain(..).map(|(_, v)| v));
        } else if descending {
            buf.sort_by(|a, b| b.cmp(a))
        } else {
            buf.sort()
        }
    }));
    Some(buf)
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
    const EFFECT: Effect;
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

    /// The call's value over arguments that did not fire, without the
    /// effect an `Effect::Io` builtin's `eval` performs.
    fn quiet(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        self.eval(ctx, from)
    }

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
        self.cached.2 = Evaluated(false);
    }
}

pub trait EvalCachedAsync: Debug + Default + Send + Sync + ImageState + 'static {
    const NAME: &str;

    const CALLS: Calls = Calls::Fifo;

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
        // a wake's own fire when every fired arg was, as a kernel's fast
        // call folds it; under a genuine init every arg is born
        let genuine = ctx.event.init() && !ctx.event.wake();
        let prod =
            cached.update_full(ctx, from).map(|t| if genuine { t.real() } else { t });
        match prod {
            None => last_result.ride(),
            // a bottom arg bottoms the invocation without calling eval
            Some(t) if cached.any_bottom() => last_result.set_bottom_as(t),
            Some(t) if t.is_fired() => {
                cached.2 = Evaluated(true);
                match ev.eval(ctx, cached) {
                    Some(v) => last_result.set(TagValue::tagged(v, t)),
                    // no value this cycle, as a kernel's fast call reads it
                    None => last_result.set_bottom_as(t),
                }
            }
            Some(_) if !last_result.tag().is_bottom() => {
                // Wake catch-up: args may have drifted while asleep. A
                // stateless value is recomputed from the present slots; a
                // stateful one must not be (its last result is its state).
                if matches!(T::EFFECT, Effect::Stateless(_) | Effect::Io(_)) && woke {
                    cached.2 = Evaluated(true);
                    match ev.quiet(ctx, cached) {
                        Some(v) => last_result.set(TagValue::stale(v)),
                        None => last_result.set_bottom(false),
                    }
                } else {
                    last_result.retag(Tag::STALE)
                }
            }
            // a bottom eval already gave over these slots stands
            Some(_) if cached.2.0 => last_result.ride(),
            Some(_) => {
                // Nothing to re-surface yet: compute the value once to
                // establish the value channel, STALE.
                cached.2 = Evaluated(true);
                match ev.quiet(ctx, cached) {
                    Some(v) => last_result.set(TagValue::stale(v)),
                    None => last_result.ride(),
                }
            }
        }
    }
}

/// How an async call site runs its invocations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Calls {
    /// One at a time, in order.
    Fifo,
    /// A new invocation aborts the calls in flight and drops those
    /// queued, so the call answers only its latest arguments. Only for
    /// an `eval` that loses nothing when dropped.
    Latest,
    /// Each at once; the replies come in the order the calls complete.
    Concurrent,
}

/// The abort for every call a site has in flight: dropping it resolves
/// each call's receiver.
#[derive(Debug)]
struct Abort {
    _tx: oneshot::Sender<()>,
    rx: Shared<oneshot::Receiver<()>>,
}

impl Abort {
    fn new() -> Self {
        let (tx, rx) = oneshot::channel();
        Self { _tx: tx, rx: rx.shared() }
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
    /// calls spawned under `id` and not answered yet
    inflight: usize,
    abort: Option<Abort>,
    /// the error tag a panicking eval answers under, from the call's
    /// declared return type; none when it declares no such error
    panic_tag: Option<ArcStr>,
    out: TagValue,
    t: T,
}

/// The tag a caught panic answers under: the first variant with one
/// string payload in an error `t` declares.
fn panic_tag(env: &Env, t: &Type) -> Option<ArcStr> {
    fn error_tag(env: &Env, t: &Type) -> Option<ArcStr> {
        match &constructor(env, t)? {
            Type::Set(ts) => ts.iter().find_map(|t| error_tag(env, t)),
            Type::Variant(tag, ps, _) => match &ps[..] {
                [Type::Primitive(p)] if p.exactly_one() == Some(Typ::String) => {
                    Some(tag.clone())
                }
                _ => None,
            },
            _ => None,
        }
    }
    match &constructor(env, t)? {
        Type::Set(ts) => ts.iter().find_map(|t| panic_tag(env, t)),
        Type::Error(e) => error_tag(env, e),
        _ => None,
    }
}

/// The reply of an eval that panicked where the call declares no error
/// to answer with: the call answers nothing.
static PANICKED: std::sync::LazyLock<Value> = std::sync::LazyLock::new(|| {
    errf!(arcstr::literal!("Panicked"), "an async builtin panicked")
});

impl<T: EvalCachedAsync> CachedArgsAsync<T> {
    /// Abort the calls in flight, drop those queued and release the reply
    /// id, which is this call's alone, as is what was stored for it.
    fn release<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.abort = None;
        self.inflight = 0;
        self.queued.clear();
        ctx.release_var(self.id, self.top_id);
    }

    /// `release`, then answer under a fresh reply id.
    fn renew<R: Rt, E: UserEvent>(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.release(ctx);
        self.id = BindId::new();
        ctx.rt.ref_var(self.id, self.top_id);
    }
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
        let panic_tag = panic_tag(&ctx.env, &resolved.unwrap_or(typ).rtype);
        let t = CachedArgsAsync::<T> {
            slept: false,
            id,
            top_id,
            cached: CachedVals::new(from),
            queued: VecDeque::new(),
            inflight: 0,
            abort: None,
            panic_tag,
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
        let panic_tag = Pack::decode(buf)?;
        let t = T::image_decode(ctx, buf)?;
        ctx.record_ref(id, top_id);
        Ok(Box::new(Self {
            slept,
            cached,
            id,
            top_id,
            queued: VecDeque::new(),
            inflight: 0,
            abort: None,
            panic_tag,
            out: TagValue::phantom(),
            t,
        }))
    }
}

impl<R: Rt, E: UserEvent, T: EvalCachedAsync> Apply<R, E> for CachedArgsAsync<T> {
    /// The queue holds arguments already prepared for `eval`, and a
    /// running eval is a task, which exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.queued.is_empty() || self.inflight > 0 {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.slept.encode(buf)?;
        self.cached.image_encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.panic_tag.encode(buf)?;
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
            // CR claude for eric: [bug/high] A wake restarts every async builtin over
            // its standing arguments, including the ones that act on the world. A woken
            // arm therefore performs the effect again even though its trigger never
            // fired: sqlite::exec/exec_batch/begin/commit/rollback, non-GET http
            // requests, the db writers, the cursor reads that advance a shared
            // iterator, and sys::fs::write_all. In the probe, one trigger fire plus two
            // wakes inserts three rows, while a sync println under the same trigger
            // prints once. This follows design/async_sleep_outputs.md, but it
            // contradicts the wake catch-up rule (a wake re-raises only fires no
            // selected reader saw) and the sync Io rule (EvalCached::quiet computes
            // without the effect). Proposed fix: give EvalCachedAsync an effect
            // classification parallel to Effect::Io (for example an `ACTS` constant).
            // An acting builtin would restart at a wake only when an argument fired
            // during the sleep, never on `woke` alone; queries would keep restarting.
            // Then mark the acting builtins, update async_sleep_outputs.md and
            // CLAUDE.md, and add a pin per family. probe:
            // design/review-2026-10-12/repro/http-sqlite-db1-02.gx (http-sqlite-db1-02)
            // CR claude for eric: [bug/high] A wake reissues the call over its standing
            // arguments even when the last call already completed before the arm slept.
            // For a builtin that acts on the world, that repeats the action with no new
            // trigger. Probe: design/review-2026-10-12/repro/db2-02.gx. There, one fire
            // of the trigger pops two entries: db::pop_min runs again at the wake. The
            // same holds for every effectful user of this wrapper: db
            // insert/remove/batch/compare_and_swap/generate_id/cursor read/txn commit,
            // sqlite exec, http requests, and stream writes. The rule is the documented
            // one (CLAUDE.md's async-sleep sentence, design/async_sleep_outputs.md, pin
            // wake_reissues_standing_async). It conflicts with wake catch-up, where a
            // wake re-raises only fires no reader saw, and with the Io rule. Options:
            // restart at the wake only a call that was in flight or queued when the arm
            // slept, recorded in sleep(), with a completed call staying completed and
            // its output phantom. Or add an EvalCachedAsync flag so only idempotent
            // builtins restart. Either way, update the doc and the pin to match.
            // (db2-02)
            Invocation::Quiet => woke,
            Invocation::Bottom { .. } => false,
        };
        let res = ctx.event.variables.remove(&self.id).and_then(|tv| {
            self.inflight -= 1;
            match tv.value() {
                v if self.panic_tag.is_none() && v == *PANICKED => None,
                v => self.t.map_value(ctx, v),
            }
        });
        if start && let Some(args) = self.t.prepare_args(&self.cached) {
            if T::CALLS == Calls::Latest && self.inflight > 0 {
                self.renew(ctx);
            }
            self.queued.push_back(args);
        }
        while (T::CALLS == Calls::Concurrent || self.inflight == 0)
            && let Some(args) = self.queued.pop_front()
        {
            self.inflight += 1;
            let (id, panic_tag) = (self.id, self.panic_tag.clone());
            let aborted = self.abort.get_or_insert_with(Abort::new).rx.clone();
            let eval = std::panic::AssertUnwindSafe(T::eval(args)).catch_unwind();
            ctx.rt.spawn_var(async move {
                let v = match future::select(std::pin::pin!(eval), aborted).await {
                    future::Either::Left((Ok(v), _)) => v,
                    // a panicking eval still answers, so the site runs its next call
                    future::Either::Left((Err(_), _)) => {
                        // XCR claude for claude: [bug/medium] x-panics-05 fixed at the
                        // symptom: a caught panic replies an error outside the declared
                        // type. The fix catches the async builtin's panic and replies
                        // `errf!("Panicked", ..)` (lib.rs). The root cause,
                        // netidx-core's Duration::decode calling
                        // Duration::new(u64::MAX, 1e9), still panics and still prints a
                        // panic message to stderr. `Panicked` is not in pack::read's
                        // declared error type ([`PackErr, `InvalidCast]), so a handler
                        // that is exhaustive over the declared errors matches nothing.
                        // The checker considers such a select complete. Failure: With
                        // GRAPHIX_ABORT_ON_NO_MATCH=1 the program below aborts: 'select
                        // e.0.error {..}: no arm matches ["Panicked", "pack_read
                        // panicked"]: a coverage hole in the check' (exit 134). Without
                        // it the select is silently bottom and r is never printed.
                        // stderr also shows 'panicked .. overflow in Duration::new',
                        // which corrupts a TUI. Fix: Fix the decode in netidx-core: use
                        // Duration::checked_add or validate ns < 1e9 and return a
                        // PackError. In the catch, map a panic to the builtin's own
                        // declared error tag (`PackErr` here), or declare `Panicked` in
                        // every async builtin's error type, so the static types stay
                        // honest. Unverified: the reviewer's report, not checked by a
                        // verifier. (x-rerun-8-08)
                        // 2026-10-13 claude: netidx-core's Duration::decode refuses
                        // the overflow (netidx dc8540eb), pinned by
                        // lib_tests::pack::pack_read_oversize_duration_nanos. A caught
                        // panic now answers the first string-payload variant of the
                        // error the call declares (panic_tag), or nothing, logged as a
                        // failure, when it declares none. No program reaches that path
                        // now, so it has no pin.
                        log::error!(target: graphix_compiler::FAILURE_TARGET, "{} panicked", T::NAME);
                        match &panic_tag {
                            Some(tag) => errf!(tag.clone(), "{} panicked", T::NAME),
                            None => PANICKED.clone(),
                        }
                    }
                    // an aborted eval's id was released, which drops this reply
                    future::Either::Right(_) => Value::Null,
                };
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
        self.release(ctx);
        self.cached.clear();
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // CR claude for eric: [bug/medium] Sleep aborts the call in flight, and the
        // wake re-issues it from the start. That is only correct for an eval that loses
        // nothing when dropped. The stream overrides do lose something. IoReadExactEv
        // and IoReadAllEv drop the bytes they already read: a read_exact of 6 asleep
        // across "abc" | "def" | "ghi" returns "defghi". IoWriteExactEv leaves a
        // partial write_all and then writes the whole buffer again: 192000 bytes sent
        // arrive as 257536. DbCursorReadEv and DbCursorReadManyEv run on a blocking
        // task that keeps advancing the cursor after the abort, so those entries are
        // lost. design/async_sleep_outputs.md prescribes this abort for stream I/O, yet
        // keeps IoLines' reader across sleep for exactly this reason. The rule is
        // decided per wrapper but needs to be per eval. Options: EvalCachedAsync
        // declares whether an eval is cancel-safe, and a sleep lets an unsafe one
        // finish and banks its reply for the wake; or the stream halves get a pushback
        // buffer that an aborted read refills, while writes stay non-abortable. Either
        // way, write the rule into design/async_sleep_outputs.md. Probe:
        // design/review-2026-10-12/repro/x-builtin-effects-03.sh (x-builtin-effects-03)
        self.renew(ctx);
        self.cached.clear();
        self.slept = true;
        self.out = TagValue::phantom();
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

    const EFFECT: Effect = Effect::Sync;
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
        let x = from[0].update(ctx);
        if let Some(bottom) = bottom_of([x.tag()]) {
            return bottom;
        }
        match seam_tick(x).and_then(|tv| match tv.value_cloned() {
            v @ Value::Error(_) => Some(caused(v, tv.tag())),
            _ => None,
        }) {
            Some(tv) => self.out.set(tv),
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
            [s] => {
                let x = s.update(ctx);
                if let Some(bottom) = bottom_of([x.tag()]) {
                    return bottom;
                }
                seam_tick(x).and_then(|tv| {
                    if self.val {
                        None
                    } else {
                        self.val = true;
                        Some(caused(tv.value_cloned(), tv.tag()))
                    }
                })
            }
            _ => None,
        };
        match res {
            Some(tv) => self.out.set(tv),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
        self.val = false;
        self.out = TagValue::phantom();
    }
}

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
        let n = from[0].update(ctx);
        let n_tag = n.tag();
        seed_count(n, &mut self.n);
        let x = from[1].update(ctx);
        if let Some(bottom) = bottom_of([n_tag, x.tag()]) {
            return bottom;
        }
        let res = seam_tick(x).and_then(|tv| {
            let counting = matches!(self.n, Some(n) if n > 0);
            if let Some(n) = &mut self.n
                && *n > 0
            {
                *n -= 1;
            }
            // take passes while its count runs; skip once it has run out,
            // or when it has none
            (counting == TAKE).then(|| caused(tv.value_cloned(), tv.tag()))
        });
        match res {
            Some(tv) => self.out.set(tv),
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

/// `all`: its first argument when every argument equals it, references
/// compared by what they name, as `==` compares them.
#[derive(Debug, Default)]
pub(crate) struct AllEv {
    /// the argument type, when it holds references
    refs: Option<Type>,
}

impl ImageState for AllEv {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.refs.encode(buf)
    }

    fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self { refs: Pack::decode(buf)? })
    }
}

impl<R: Rt, E: UserEvent> EvalCached<R, E> for AllEv {
    const EFFECT: Effect =
        Effect::Stateless(Some(graphix_compiler::FastCall::Plain(fc_all)));
    const NAME: &str = "core_all";

    fn init(
        ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        let refs = from.first().map(|n| n.typ()).filter(|t| t.compares_refs(&ctx.env));
        Self { refs: refs.cloned() }
    }

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R, E>, from: &CachedVals) -> Option<Value> {
        let Some(t) = &self.refs else { return fast_eval(ctx, fc_all, from) };
        let [hd, tl @ ..] = &from.0[..] else { return None };
        let hd = hd.as_ref()?;
        let named = ctx.ref_targets(t, hd);
        for v in tl {
            if ctx.ref_targets(t, v.as_ref()?) != named {
                return None;
            }
        }
        Some(hd.clone())
    }
}

pub(crate) type All = CachedArgs<AllEv>;

/// The arguments with every array spread into its elements.
fn flat(args: &[Value]) -> impl Iterator<Item = Value> + '_ {
    args.iter().flat_map(|v| v.clone().flatten())
}

/// `op` over the flattened arguments, left to right. Integer steps wrap
/// as `+` and `*` do; a step the value arithmetic refuses (a division by
/// zero, a decimal or duration overflow) is logged as a failure and the
/// result is nothing, as the `/` operator does, never an error the type
/// does not admit.
fn arith(name: &str, args: &[Value], op: fn(Value, Value) -> Value) -> Option<Value> {
    let mut acc: Option<Value> = None;
    for v in flat(args) {
        let next = match acc {
            None => v,
            Some(l) => op(l, v),
        };
        if let Value::Error(e) = &next {
            log::error!(target: graphix_compiler::FAILURE_TARGET, "{name}: {e}");
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
// CR claude for claude: [structure/low] `Filter` repeats `opt::OptHof`'s callback
// scaffolding: the genn::bind / genn::reference / genn::apply init, and a `delete`,
// `refs` and `typecheck0` that match line for line. Its `update` also writes
// `opt::feed` inline twice. The same feed is inlined in array's group, queuefn's
// `WrapperApply`, sys::net's publish and core's handler.rs. Dropping a private bind is
// spelled three ways: `release_var`; `unref_var` + `store_remove` in `Sample::delete`,
// which is `release_var` written out; and `release_var` + `store_remove` in
// `QueueFn::delete`, which removes twice. A change to how a private bind is fed or
// released has to find every copy. Put `feed` beside `genn::bind` in
// graphix_compiler::node::genn, use it at each of these sites, and build `Filter` on
// the same callback struct `OptHof` uses. (x-node-contract-06)
// 2026-10-13 claude: OptHof is gone (opt's unary HOFs are now selects in opt.gx);
// Filter's callback scaffolding and the inlined feeds remain.
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
        let x = from[0].update(ctx);
        let x_tag = x.tag();
        if let Some(tv) = seam_value(x) {
            let tag = tv.tag();
            let v = tv.value_cloned();
            self.pending = Some(v.clone());
            ctx.rt.store_insert(self.x, TagValue::fired(v.clone()));
            ctx.event.variables.insert(self.x, TagValue::tagged(v, tag));
        }
        let pred = self.pred.update(ctx);
        if let Some(bottom) = bottom_of([x_tag, pred.tag()]) {
            return bottom;
        }
        let res = seam_tick(pred).and_then(|b| match b.value_cloned() {
            Value::Bool(true) => self.pending.clone().map(|v| caused(v, b.tag())),
            _ => None,
        });
        match res {
            Some(tv) => self.out.set(tv),
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
        let fired = seam_tick(from[0].update(ctx)).is_some();
        let cleared = seam_tick(from[1].update(ctx)).is_some();
        let x = from[2].update(ctx);
        let x_tag = x.tag();
        if fired {
            self.triggered += 1;
        }
        if cleared {
            self.queue.clear();
        }
        if let Some(tv) = seam_tick(x) {
            self.queue.push_back(tv.value_cloned());
        }
        let bottom = bottom_of([x_tag]);
        while self.triggered > 0 && self.queue.len() > 0 {
            self.triggered -= 1;
            ctx.rt.set_var(self.id, self.queue.pop_front().unwrap());
        }
        match ctx.event.variables.get(&self.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match bottom {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
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
        // CR claude for eric: [doc-drift/low] Queue::sleep discards every queued value.
        // A seqq whose select arm sleeps therefore loses not only the run in progress
        // (the reset seq_blocks.md §6.5 documents) but every request queued behind it,
        // silently. Probe: design/review-2026-10-12/repro/x-engine-seq-errors-06.gx,
        // where request 3 is never run after the wake and both engines agree. Needs a
        // rule. If dropping is intended, add queue to CLAUDE.md's restart-builtin list
        // and say in §6.5 and in core::queue's interface doc that a sleeping seqq
        // discards its queue. If sleep should be a pause, keep the queue here, re-arm
        // one credit at the wake, and pin it. (x-engine-seq-errors-06)
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
        let mut cause = None::<Tag>;
        if let Some(tv) = seam_tick(from[0].update(ctx)) {
            self.triggered += 1;
            cause = Some(tv.tag());
        }
        if let Some(tv) = seam_tick(from[1].update(ctx)) {
            self.current = Some(tv.value_cloned());
            cause = Some(cause.map_or(tv.tag(), |c| c.join(tv.tag())));
        }
        if self.triggered > 0
            && let Some(cause) = cause
            && let Some(v) = self.current.take()
        {
            self.triggered -= 1;
            self.out.set(caused(v, cause))
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

/// Where a throttle stands between emissions.
#[derive(Debug, Clone, Copy)]
enum Run {
    /// nothing emitted since it started or slept
    Idle,
    /// emitted at `last`, nothing held
    Open { last: Instant },
    /// emitted at `last`; `tid`'s timer releases the value held
    Pending { last: Instant, tid: BindId },
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    run: Run,
    top_id: ExprId,
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
            run: Run::Idle,
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
            run: Run::Idle,
            top_id,
            last_v: None,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Throttle {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // a running timer and its wall-clock mark exist only once a cycle ran
        if !matches!(self.run, Run::Idle) {
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
        // A fired duration retunes the wait; any value-bearing delivery
        // of the throttled arg lands in `last_v`, but only a fired one
        // is an event to throttle.
        // a standing rate this throttle has not taken (a sibling consumed
        // its fire) is taken as a fired one is
        let rate = from[0].update(ctx);
        let rate_tag = rate.tag();
        let new_wait = seam_value(rate)
            .and_then(|tv| {
                tv.with_value(|v| match v {
                    Value::Duration(d) => Some((**d, tv.tag())),
                    _ => None,
                })
            })
            .filter(|(d, _)| *d != self.wait);
        let x = from[1].update(ctx);
        let x_tag = x.tag();
        let mut up1 = None;
        if let Some(tv) = seam_value(x) {
            up1 = tv.is_fired().then(|| tv.tag());
            self.last_v = Some(tv.value_cloned());
        }
        // a value held for a timer delivers; a bottom stands in place of
        // a ride
        let bottom = bottom_of([rate_tag, x_tag]);
        macro_rules! ride {
            () => {
                return match bottom {
                    Some(bottom) => bottom,
                    None => self.out.ride(),
                }
            };
        }
        macro_rules! emit {
            ($cause:expr) => {{
                match self.last_v.clone() {
                    Some(v) => return self.out.set(caused(v, $cause)),
                    None => ride!(),
                }
            }};
        }
        // emit now if the wait since `last` has passed, else hold the
        // value for a timer
        macro_rules! maybe_schedule {
            ($last:expr, $cause:expr) => {{
                let (last, now) = ($last, Instant::now());
                if now - last >= self.wait {
                    self.run = Run::Open { last: now };
                    emit!($cause)
                } else {
                    let tid = BindId::new();
                    ctx.rt.ref_var(tid, self.top_id);
                    ctx.rt.set_timer(tid, self.wait - (now - last));
                    self.run = Run::Pending { last, tid };
                    ride!()
                }
            }};
        }
        if let Some((d, cause)) = new_wait {
            self.wait = d;
            if let Run::Pending { last, tid } = self.run {
                ctx.release_var(tid, self.top_id);
                maybe_schedule!(last, cause)
            }
        }
        if let Some(cause) = up1 {
            match self.run {
                Run::Idle => {
                    self.run = Run::Open { last: Instant::now() };
                    emit!(cause)
                }
                Run::Open { last } => maybe_schedule!(last, cause),
                Run::Pending { .. } => (),
            }
        }
        if let Run::Pending { tid, .. } = self.run
            && ctx.event.variables.contains_key(&tid)
        {
            ctx.release_var(tid, self.top_id);
            self.run = Run::Open { last: Instant::now() };
            emit!(Tag::FIRED)
        }
        ride!()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Run::Pending { tid, .. } = self.run {
            ctx.release_var(tid, self.top_id);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.delete(ctx);
        self.run = Run::Idle;
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
        let x = from[0].update(ctx);
        if let Some(bottom) = bottom_of([x.tag()]) {
            return bottom;
        }
        match seam_tick(x) {
            Some(tv) => {
                self.count += 1;
                self.out.set(caused(Value::I64(self.count), tv.tag()))
            }
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {
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
        let tv = from[0].update(ctx);
        // a bottom input is a bottom output, and what comes back from it
        // is new
        if tv.tag().is_bottom() {
            *last = None;
            return out.set_bottom(tv.tag().triggers());
        }
        let Some((v, tag)) = seam_tick(tv).map(|tv| (tv.value_cloned(), tv.tag())) else {
            return out.ride();
        };
        let cmp = match &self.2 {
            Some(t) => ctx.ref_targets(t, &v),
            None => v.clone(),
        };
        let changed = coretraits::with_hooks(ctx, || Some(&cmp) != last.as_ref());
        if changed {
            *last = Some(cmp);
            out.set(caused(v, tag))
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

/// The four printers. Each writes a line to `#dest` (argument 0) at
/// every fire of its message (argument 1), and fused, through the same
/// [`Printer::emit`] after its kernel's run.
#[derive(Debug, Clone, Copy)]
enum Printer {
    Dbg,
    Log,
    Print,
    Println,
}

impl Printer {
    fn default_dest(self) -> LogDest {
        match self {
            Self::Dbg => LogDest::Stderr,
            Self::Log | Self::Print | Self::Println => LogDest::Stdout,
        }
    }

    /// The call's value: dbg's is its message, the others' null.
    fn result(self, v: &Value) -> Value {
        match self {
            Self::Dbg => v.clone(),
            Self::Log | Self::Print | Self::Println => Value::Null,
        }
    }

    fn emit<R: Rt, E: UserEvent>(
        self,
        ctx: &mut ExecCtx<'_, R, E>,
        dest: LogDest,
        arg: &Expr,
        typ: &Type,
        v: &Value,
    ) {
        let mut line: LPooled<String> = LPooled::take();
        match self {
            Self::Dbg => write!(line, "{} dbg({arg}): ", arg.pos),
            Self::Log => write!(line, "{}: ", arg.pos),
            Self::Print | Self::Println => Ok(()),
        }
        .unwrap();
        coretraits::with_display_hooks(ctx, |env| {
            match (self, v) {
                // CR claude for eric: [bug/low] This arm picks raw output by the value,
                // not by the type. A bare variant shares its runtime form with a
                // string, so `println(`A)` prints `A`, while `println(`B(1))`,
                // `println([`A])` and `log(`A)` all print the backtick: a value of one
                // type `[`A, `B(i64)]` is printed in two notations. `typ` is in hand
                // here, so print raw only when the site's type says string, and send
                // everything else through TVal. `perform` reaches the same code, so the
                // node-walk and fused output stay the same. One decision is needed:
                // what a string under `Any`, or under a union that also holds a bare
                // variant, should print. probe:
                // design/review-2026-10-12/repro/book-stdlib-11.gx (book-stdlib-11)
                (Self::Print | Self::Println, Value::String(s)) => write!(line, "{s}"),
                (_, v) => write!(line, "{}", TVal { env, typ, v }),
            }
            .unwrap()
        });
        let suffix = match self {
            Self::Print => "",
            Self::Dbg | Self::Log | Self::Println => "\n",
        };
        emit_line(ctx, dest, &line, suffix)
    }

    /// A fused call's effect: the arguments its kernel queued.
    fn perform<R: Rt, E: UserEvent>(
        self,
        ctx: &mut ExecCtx<'_, R, E>,
        site: &IoSite,
        args: &[Value],
    ) {
        let dest = args[0].clone().cast_to::<LogDest>().unwrap_or(self.default_dest());
        self.emit(ctx, dest, &site.arg, site.typ(), &args[1])
    }
}

fn dbg_result(args: &[Value]) -> Option<Value> {
    Some(args[1].clone())
}

fn null_result(_: &[Value]) -> Option<Value> {
    Some(Value::Null)
}

/// A printer's application. A bottom argument bottoms the call, and its
/// tag is the message's: the kernel's fast call reads `#dest` as stale.
#[derive(Debug)]
struct PrintCall {
    printer: Printer,
    dest: LogDest,
    out: TagValue,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for PrintCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.dest.encode(buf)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        from: &mut [Node<R, E>],
    ) -> &TagValue {
        let dest = from[0].update(ctx);
        let dest_bottom = dest.is_bottom();
        if let Some(d) =
            seam_value(dest).and_then(|tv| tv.value_cloned().cast_to::<LogDest>().ok())
        {
            self.dest = d;
        }
        let msg = from[1].update(ctx);
        let tag = msg.tag();
        let v = match seam_value(msg) {
            Some(tv) if !dest_bottom => tv.value_cloned(),
            _ => return self.out.set_bottom_as(tag),
        };
        if tag.is_fired() {
            self.printer.emit(ctx, self.dest, from[1].spec(), from[1].typ(), &v);
        }
        self.out.set(TagValue::tagged(self.printer.result(&v), tag))
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R, E>) {}
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

macro_rules! printer {
    ($type:ident, $name:literal, $printer:expr, $result:ident) => {
        struct $type;

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $type {
            const EFFECT: Effect =
                Effect::Io(Some(IoCall { result: $result, trigger: 1 }));
            const NAME: &str = $name;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut CompileCtx<R, E>,
                _typ: &'a graphix_compiler::typ::FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                _from: &'c [Node<R, E>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R, E>>> {
                let printer = $printer;
                let dest = printer.default_dest();
                Ok(Box::new(PrintCall { printer, dest, out: TagValue::phantom() }))
            }

            fn image_decode(
                _ctx: &mut ExecCtx<'_, R, E>,
                _from: &[Node<R, E>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R, E>>, PackError> {
                let dest = LogDest::decode(buf)?;
                Ok(Box::new(PrintCall {
                    printer: $printer,
                    dest,
                    out: TagValue::phantom(),
                }))
            }

            fn perform(ctx: &mut ExecCtx<'_, R, E>, site: &IoSite, args: &[Value]) {
                $printer.perform(ctx, site, args)
            }
        }
    };
}

printer!(Dbg, "core_dbg", Printer::Dbg, dbg_result);
printer!(Log, "core_log", Printer::Log, null_result);
printer!(Print, "core_print", Printer::Print, null_result);
printer!(Println, "core_println", Printer::Println, null_result);

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
    /// `list::len` — registered here because core's `Collection`
    /// implementation for `List` needs it; the list package binds the name.
    ListLen, ListLenEv, "core_list_len", list_len
);

fn list_len(args: &[Value]) -> Option<Value> {
    Some(Value::I64(graphix_compiler::node::list::len(&args[0])? as i64))
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
        ListLen,
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
        opt::OrDefault,
        opt::Or,
        opt::And,
        opt::Xor,
        opt::OkOr,
        opt::Zip,
        opt::Unzip,
        opt::OptOrElse as opt::OptOrElse<GXRt<X>, X::UserEvent>,
        opt::OptOkOrElse as opt::OptOkOrElse<GXRt<X>, X::UserEvent>,
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

/// A value whose drop runs on a blocking thread, when there is a runtime:
/// dropping the last handle to a database flushes and syncs (sled's log,
/// a WAL checkpoint), which must never stall a cycle.
pub struct Reaped<T: Send + 'static>(std::mem::ManuallyDrop<T>);

impl<T: Send + 'static> Reaped<T> {
    pub fn new(t: T) -> Self {
        Self(std::mem::ManuallyDrop::new(t))
    }
}

impl<T: Send + 'static> std::ops::Deref for Reaped<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

thread_local! {
    /// Set while this thread reaps, or spawns a reap: a reap the value
    /// holds drops in place. A spawn on a pool shutting down drops its
    /// closure under the pool's lock, where a second spawn would deadlock.
    static REAPING: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Runs `f` with [`REAPING`] set, restoring it after.
fn reaping<U>(f: impl FnOnce() -> U) -> U {
    let outer = REAPING.replace(true);
    let r = f();
    REAPING.set(outer);
    r
}

impl<T: Send + 'static> Drop for Reaped<T> {
    fn drop(&mut self) {
        // SAFETY: the value is not touched again
        let t = unsafe { std::mem::ManuallyDrop::take(&mut self.0) };
        match tokio::runtime::Handle::try_current() {
            Ok(rt) if !REAPING.get() => {
                reaping(|| drop(rt.spawn_blocking(move || reaping(|| drop(t)))))
            }
            _ => drop(t),
        }
    }
}

impl<T: Send + 'static> Debug for Reaped<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Reaped<{}>", std::any::type_name::<T>())
    }
}

#[cfg(test)]
mod reaped_test {
    use super::Reaped;
    use std::{sync::mpsc, thread, time::Duration};

    /// A shut-down runtime cancels a blocking spawn inline, under its pool's
    /// lock: a nested reap dropped by that cancel must not spawn again.
    #[test]
    fn nested_reap_at_shutdown() {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let handle = rt.handle().clone();
        rt.shutdown_background();
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let _rt = handle.enter();
            drop(Reaped::new(Reaped::new(Reaped::new(()))));
            tx.send(()).unwrap();
        });
        rx.recv_timeout(Duration::from_secs(5)).expect("a nested reap deadlocked");
    }
}
