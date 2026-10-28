#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use futures::{
    channel::oneshot,
    future::{self, FutureExt, Shared},
};
use graphix_compiler::{
    Apply, BindId, BuiltIn, CompileCtx, ExecCtx, FastFn, IoSite, Node, Refs, Rt, Scope,
    Tag, TagValue, TagView, TypedFastFn, WriteTarget,
    effects::{Effect, IoCall},
    env::Env,
    err, errf,
    expr::{Expr, ExprId},
    image::{self, ImageBuf},
    node::{coretraits, genn},
    rep,
    typ::{FnType, TVal, Type},
};
use graphix_rt::GXRt;
use netidx::{path::Path, publisher::Typ, subscriber::Value};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use netidx_value::FromValue;
use poolshark::local::LPooled;
use rust_decimal::{Decimal, prelude::ToPrimitive};
use smallvec::SmallVec;
use std::{
    any::Any,
    borrow::Cow,
    cmp::Ordering,
    collections::VecDeque,
    fmt::{Debug, Write},
    marker::PhantomData,
    time::Duration,
};
use tokio::time::Instant;

pub(crate) mod buffer;
pub(crate) mod math;
pub(crate) mod opt;
pub(crate) mod queuefn;

/// How long a TLS handshake may take before the stream is dropped: the
/// standard TCP timeout. rustls has no clock of its own, so a peer that
/// goes quiet mid-handshake would otherwise hold its socket and task until
/// it hung up.
pub const TLS_HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(120);

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

pub fn extract_cast_type(env: &Env, resolved_typ: Option<&FnType>) -> Option<Type> {
    cast_target(&resolved_typ?.rtype).filter(|t| castable(env, t))
}

/// Whether a read can cast to `t`: the checker's Concrete rule.
pub fn castable(env: &Env, t: &Type) -> bool {
    t.is_concrete(env)
}

/// A type-directed builtin's target: the success type of its resolved
/// return type, set at init and again at `typecheck1`, imaged with it.
#[derive(Debug, Default, Clone, netidx_derive::Pack)]
pub struct CastTarget(Option<Type>);

impl CastTarget {
    pub fn of(env: &Env, resolved: Option<&FnType>) -> Self {
        Self(extract_cast_type(env, resolved))
    }

    pub fn refresh(&mut self, env: &Env, resolved: &FnType) {
        self.0 = extract_cast_type(env, Some(resolved))
    }

    /// Whether a part of the target reads a number exactly
    /// (`Type::holds_exact_number`).
    pub fn holds_exact_number(&self, env: &Env) -> bool {
        self.0.as_ref().is_some_and(|t| t.holds_exact_number(env))
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

    /// [`Self::read`] of data whose decimals stand for floats read exactly
    /// ([`Type::cast_exact_floats`]).
    pub fn read_exact_floats(&self, env: &Env, tag: &ArcStr, v: Value) -> Value {
        match (&self.0, v) {
            (Some(t), v) if !matches!(v, Value::Error(_)) => t.cast_exact_floats(env, v),
            (_, v) => self.read(env, tag, v),
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
        match cached.value(0)? {
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

    /// The arguments `parse` takes; `exact` when the target holds a number
    /// read exactly ([`CastTarget::holds_exact_number`]), which a number
    /// the format names past a float's precision should reach exactly.
    fn prepare_args(cached: &CachedVals, exact: bool) -> Option<Self::Args>;

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
    exact: bool,
    format: PhantomData<fn() -> F>,
}

impl<F> TypedRead<F> {
    fn new(env: &Env, target: CastTarget) -> Self {
        Self { exact: target.holds_exact_number(env), target, format: PhantomData }
    }
}

impl<F> Default for TypedRead<F> {
    fn default() -> Self {
        Self { target: CastTarget::default(), exact: false, format: PhantomData }
    }
}

impl<F> ImageState for TypedRead<F> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.target.encode(buf)
    }

    fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self::new(&ctx.env, CastTarget::decode(buf)?))
    }
}

impl<F: ReadFormat> EvalCachedAsync for TypedRead<F> {
    const NAME: &str = F::NAME;
    type Args = F::Args;

    fn init<R: Rt>(
        ctx: &mut CompileCtx<R>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R>],
        _top_id: ExprId,
    ) -> Self {
        Self::new(&ctx.env, CastTarget::of(&ctx.env, resolved))
    }

    fn typecheck1<R: Rt>(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.target.refresh(&ctx.env, resolved);
        self.exact = self.target.holds_exact_number(&ctx.env);
        Ok(())
    }

    fn map_value<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>, v: Value) -> Option<Value> {
        Some(coretraits::with_display_hooks(ctx, |env| F::read(&self.target, env, v)))
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        F::prepare_args(cached, self.exact)
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

    /// Ask to exit with `code`. A program already exiting keeps its first
    /// code, and one whose embedder no longer listens has been dropped by
    /// it: the process is the embedder's to end.
    pub fn request(&self, code: i32) {
        if let Some(tx) = self.0.lock().take() {
            let _ = tx.send(code);
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
/// whose identity is the address its `inner` Arc points at (`@identity
/// T.field` names another field): equal only to itself, ordered and hashed
/// by that address, so its order holds within a run and not between runs.
#[macro_export]
macro_rules! impl_abstract_arc {
    ($name:ident, $wrapper_vis:vis static $wrapper:ident = $path:literal) => {
        $crate::impl_abstract_arc!(@identity $name);
        $crate::abstract_wrapper!($name, $wrapper_vis static $wrapper = $path);
    };
    (@identity $name:ident) => {
        $crate::impl_abstract_arc!(@identity $name.inner);
    };
    (@identity $name:ident . $field:ident) => {
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                std::ptr::eq(&*self.$field, &*other.$field)
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
                (&raw const *self.$field).addr().cmp(&(&raw const *other.$field).addr())
            }
        }
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                (&raw const *self.$field).addr().hash(state)
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

/// A map key as a text format (JSON, TOML, a template's data) writes it,
/// by its data (`abstract_value::to_data`): a string bare, a datetime in
/// RFC3339, a duration in seconds, any other value in its naked form. A
/// reader's cast reads back a key whose data is a string, a number, a
/// bool, a datetime or a duration; the text of any other key's does not.
// CR claude for claude: [bug/medium] The doc says a reader's cast reads this text back,
// but it does not for a bare variant or a composite key. Map<`A, i64> writes
// {"\"A\"":2} (the data string printed with its quotes) and Map<(i64, i64), i64> writes
// {"[1, 2]":3`}; json::read into the same types gives InvalidCast for both, so the json
// and toml writers' output does not read back into its type. Write a string-data key
// bare, and decide whether composite keys parse on read (as str::parse does) or are
// refused by the writers. probe:
// design/review-2026-10-26/repro/x-rerun-5-json-map-keys.gx (x-rerun-5-04)
// 2026-10-26 claude: The string-data half is done (t-rep-02, x-kind-markers-01): a
// bare-variant key is written as its tag and reads back, pinned by
// lib_tests::json::string_data_map_keys_read_back. The composite-key decision still
// stands.
// 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q41, Map keys that
// are not strings or scalars, in json and toml): yes, the pairs array (the
// recommendation: the pairs array.)
// CR claude for claude: [bug/medium] The text this writes for a tuple, struct, variant or
// list key, for a null key ("null") and for a bytes key (base64) does not read back.
// The reader casts the object key string to the key type, and a cast parses a string
// only into a scalar. So json/toml write {(1, 2) => "q"} as {"[1, 2]":"q"}, and reading
// it back as Map<(i64, i64), string> gives InvalidCast "not a tuple". That breaks this
// doc's promise and CLAUDE.md's "what the json and toml writers emit reads back into
// its type". Decide between two fixes: write such a map as the array of its (key,
// value) pairs, which the cast already reads as a map, or refuse it at the writer.
// probe: design/review-2026-10-26/repro/core-lib-composite-map-key-no-roundtrip.gx
// (core-lib-02)
// 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q41, Map keys that
// are not strings or scalars, in json and toml): yes, the pairs array (the
// recommendation: the pairs array.)
// CR claude for claude: [bug/high] A bare-variant or composite map key is written as
// the quoted Naked text of its data: `A becomes the JSON key "\"A\"" (TOML '"A"', hbs
// &quot;A&quot;), and (1, "a") becomes "[1, \"a\"]". No reader casts those back, so
// json::read/toml::read into the same Map type fail with InvalidCast, and an enum-keyed
// map writes unreadable JSON. The doc above says the opposite. Fix: write a bare
// variant as its tag (refuse two keys that write alike, as to_data does), and have the
// json/toml readers parse an object key as a Graphix literal (Expr::literal_value, as
// str::parse does) when the key type is neither a string nor a scalar. Pin enum, tuple
// and struct keys beside time_map_keys_read_back. probe:
// design/review-2026-10-26/repro/x-kind-markers-composite-map-keys.gx
// (x-kind-markers-01)
// 2026-10-26 claude: The bare-variant half is fixed with t-rep-02: map_key writes a
// key's data, so `A is the JSON key "A" (TOML A) and reads back; a bare variant beside
// the string it writes as is refused by the writers' duplicate-key checks. Pins:
// lib_tests::json::string_data_map_keys_read_back,
// lib_tests::toml::toml_variant_map_key_reads_back. What remains is the composite keys,
// (1, "a") written as "[1, \"a\"]": parse an object key as a Graphix literal on read
// (this CR's suggestion), write such a map as its (key, value) pairs, or refuse it at
// the writer. That is the choice core-lib-02 and x-rerun-5-04 already ask Eric to make,
// so this is readdressed rather than decided here.
// 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q41, Map keys that
// are not strings or scalars, in json and toml): yes, the pairs array (the
// recommendation: the pairs array.)
pub fn map_key(k: &Value) -> Result<ArcStr> {
    Ok(match graphix_compiler::abstract_value::to_data(k)? {
        Value::String(s) => s,
        Value::DateTime(dt) => dt.to_rfc3339().into(),
        Value::Duration(d) => format_compact!("{}", d.as_secs_f64()).as_str().into(),
        k => format_compact!("{}", graphix_compiler::typ::Naked(&k)).as_str().into(),
    })
}

/// How a writer serializes a value: an object's entries or a sequence's
/// elements, each with the type that directs it, or a leaf.
pub enum Shape {
    Object(LPooled<Vec<(ArcStr, Value, Option<Type>)>>, Keys),
    Seq(LPooled<Vec<(Value, Option<Type>)>>),
    Leaf(Value),
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
    format_compact!("{n}").parse().unwrap_or(n as f64)
}

/// `v` as a writer writes it, with the type that directs it: an abstract
/// value is the data its `Cast` implementation makes of it, under its
/// representation's type when that is the data, else under none. What
/// `abstract_value::to_data` refuses is refused.
fn written<'a>(
    env: &Env,
    mut t: Option<Type>,
    v: &'a Value,
) -> Result<(Option<Type>, Cow<'a, Value>)> {
    use graphix_compiler::abstract_value::{Written, written_as};
    let mut v = Cow::Borrowed(v);
    for depth in 0.. {
        if !matches!(&*v, Value::Abstract(_)) {
            break;
        }
        v = Cow::Owned(match written_as(&v, depth)? {
            Written::Representation(g) => {
                t = env.abstract_reps.get(&g.id).map(|r| r.instantiate_with(&g.params));
                g.payload.clone()
            }
            Written::Ran(d) => {
                t = None;
                d
            }
        });
    }
    Ok((t, v))
}

/// `v`'s shape under `t`, the type its call site gave it: a struct or a
/// map is an object; an array, a list, a tuple or a variant's tag and
/// payload a sequence; a bare variant its tag; a union reads as the
/// member `v` inhabits. `v` is the program's value: its kind markers
/// decide members, lists and composites, and an abstract value in it is
/// the data its `Cast` implementation writes ([`written`]).
/// Where the type says nothing (`None`, `Any`, an open variable), an
/// unmarked non-empty array of `(string, _)` pairs in key order (data a
/// read left untyped) reads as an object.
pub fn shape(env: &Env, t: Option<&Type>, v: &Value) -> Result<Shape> {
    let (t, v) = written(env, t.and_then(|t| constructor(env, t)), v)?;
    let v = &*v;
    match (t.as_ref(), v) {
        (Some(Type::Set(ts)), v) => {
            shape(env, graphix_compiler::typ::member_of(env, ts, v).map(|i| &ts[i]), v)
        }
        (Some(Type::Struct(fs)), v) => match rep::read_struct(v).map(rep::Parts::elems) {
            Some(a) if fs.len() == a.len() => {
                let mut entries: LPooled<Vec<_>> = LPooled::take();
                for (f, (_, t, _)) in a.iter().zip(fs.iter()) {
                    if let Some((k, v)) = rep::field(f) {
                        entries.push((k.clone(), v.clone(), Some(t.clone())));
                    }
                }
                Ok(Shape::Object(entries, Keys::Fields))
            }
            _ => untyped_shape(v),
        },
        (Some(Type::Map { value, .. }), Value::Map(m)) => Ok(Shape::Object(
            m.into_iter()
                .map(|(k, v)| Ok((map_key(k)?, v.clone(), Some(Type::clone(value)))))
                .collect::<Result<_>>()?,
            Keys::Data,
        )),
        (Some(Type::Array(e)), v) if let Some(a) = rep::array_elems(v) => {
            Ok(Shape::Seq(a.iter().map(|x| (x.clone(), Some(Type::clone(e)))).collect()))
        }
        (Some(Type::List(e)), v) if graphix_compiler::list::is_list(v) => Ok(Shape::Seq(
            graphix_compiler::list::Iter::new(v.clone())
                .map(|x| (x, Some(Type::clone(e))))
                .collect(),
        )),
        (Some(Type::Tuple(ts)), v) => match rep::read_tuple(v).map(rep::Parts::elems) {
            Some(a) if ts.len() == a.len() => Ok(Shape::Seq(
                a.iter()
                    .zip(ts.iter())
                    .map(|(x, t)| (x.clone(), Some(t.clone())))
                    .collect(),
            )),
            _ => untyped_shape(v),
        },
        (Some(Type::Variant(_, ps, _)), v) => match rep::read_variant(v) {
            Some((tag, p)) if !ps.is_empty() && p.elems().len() == ps.len() => {
                let payload = p
                    .elems()
                    .iter()
                    .zip(ps.iter())
                    .map(|(x, t)| (x.clone(), Some(t.clone())));
                let tag = (Value::String(tag.clone()), None);
                Ok(Shape::Seq(std::iter::once(tag).chain(payload).collect()))
            }
            _ => untyped_shape(v),
        },
        (_, v) => untyped_shape(v),
    }
}

/// `v` written as data: every tuple, struct, variant and list in it in
/// its data form, markers gone, and every abstract value in it the data
/// its `Cast` implementation writes it as (`abstract_value::to_data`),
/// under the hooks the implementation runs through.
pub fn write_data<R: Rt>(ctx: &mut ExecCtx<'_, R>, v: &Value) -> Result<Value> {
    coretraits::with_hooks(ctx, || graphix_compiler::abstract_value::to_data(v))
}

/// [`write_data`] with its tuples, structs, variants and lists kept as
/// values ([`graphix_compiler::abstract_value::to_marked_data`]).
pub fn write_marked_data<R: Rt>(ctx: &mut ExecCtx<'_, R>, v: &Value) -> Result<Value> {
    coretraits::with_hooks(ctx, || graphix_compiler::abstract_value::to_marked_data(v))
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

/// `v`'s shape by its kind marker, or as data where it has none.
fn untyped_shape(v: &Value) -> Result<Shape> {
    let seq = |vs: &[Value]| Shape::Seq(vs.iter().map(|x| (x.clone(), None)).collect());
    let object = |fs: &[Value]| {
        Shape::Object(
            fs.iter()
                .filter_map(|f| rep::field(f).map(|(k, v)| (k.clone(), v.clone(), None)))
                .collect(),
            Keys::Fields,
        )
    };
    Ok(match rep::kind(v) {
        Some(rep::Kind::Struct) => object(rep::struct_fields(v).unwrap_or(&[])),
        Some(rep::Kind::Tuple) => seq(rep::tuple_elems(v).unwrap_or(&[])),
        Some(rep::Kind::Variant) => match rep::variant(v) {
            Some((tag, [])) => Shape::Leaf(Value::String(tag.clone())),
            Some((tag, payload)) => Shape::Seq(
                std::iter::once((Value::String(tag.clone()), None))
                    .chain(payload.iter().map(|x| (x.clone(), None)))
                    .collect(),
            ),
            None => seq(&[]),
        },
        Some(rep::Kind::List) => Shape::Seq(
            graphix_compiler::list::Iter::new(v.clone()).map(|x| (x, None)).collect(),
        ),
        None => match v {
            Value::Array(a) if rep::is_struct(a) => object(a),
            Value::Array(a) => seq(a),
            Value::Map(m) => Shape::Object(
                m.into_iter()
                    .map(|(k, v)| Ok((map_key(k)?, v.clone(), None)))
                    .collect::<Result<_>>()?,
                Keys::Data,
            ),
            v => Shape::Leaf(v.clone()),
        },
    })
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
pub fn seam_arg<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    node: &mut Node<R>,
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

/// A runtime variable a builtin answers through, and the expression that
/// refers to it: recorded at the build, recorded again at an image's
/// decode, re-minted where an answer under the old id must land nowhere,
/// released at delete.
#[derive(Debug)]
pub struct RtVar {
    pub id: BindId,
    top_id: ExprId,
}

impl RtVar {
    pub fn new<R: Rt>(ctx: &mut CompileCtx<R>, top_id: ExprId) -> Self {
        let id = BindId::new();
        ctx.record_ref(id, top_id);
        Self { id, top_id }
    }

    pub fn top_id(&self) -> ExprId {
        self.top_id
    }

    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    pub fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(id, top_id);
        Ok(Self { id, top_id })
    }

    /// Answer under a fresh id: what was sent under the old one lands
    /// nowhere.
    pub fn remint<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.release(ctx);
        self.fresh(ctx);
    }

    /// Answer under a fresh id and return the old one, still referenced:
    /// the caller releases it once what is owed under it has come.
    pub fn fresh<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) -> BindId {
        let old = std::mem::replace(&mut self.id, BindId::new());
        ctx.ref_var(self.id, self.top_id);
        old
    }

    pub fn release<R: Rt>(&self, ctx: &mut ExecCtx<'_, R>) {
        ctx.release_var(self.id, self.top_id)
    }
}

#[derive(Debug)]
pub struct CachedVals {
    /// Each argument's last value and the tag it arrived with.
    slots: Box<[(Option<Value>, Tag)]>,
    evaluated: Evaluated,
}

impl CachedVals {
    /// The slots with those at `idx` as data leaving the program, their
    /// forms kept ([`write_marked_data`]).
    fn as_data<R: Rt>(
        &self,
        ctx: &mut ExecCtx<'_, R>,
        idx: &[usize],
    ) -> Result<CachedVals> {
        let mut slots = self.slots.clone();
        for i in idx {
            if let Some((Some(v), _)) = slots.get_mut(*i) {
                *v = write_marked_data(ctx, v)?;
            }
        }
        Ok(CachedVals { slots, evaluated: self.evaluated })
    }
}

/// Whether `eval` (or `quiet`) has run over the slots as they stand.
#[derive(Debug, Default, Clone, Copy)]
struct Evaluated(bool);

/// A builtin payload's image: its state after `init` and the typecheck
/// passes, restored in the context it was built in. `unit_image_state!`
/// implements it for a payload with no state, `pack_image_state!` for
/// one whose state is its `Pack` encoding.
pub trait ImageState: Sized {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError>;
    fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError>;
}

/// Write `v` through the reference `r`: a place reference (`&s.f`,
/// `&a[i]`) patches its root at its path, any other writes the binding
/// its reference chain names.
pub(crate) fn write_through<R: Rt>(ctx: &mut ExecCtx<'_, R>, r: BindId, v: Value) {
    match ctx.write_target(r) {
        Some(WriteTarget::Place(root, path)) => ctx.rt.patch_var(root, path, v, None),
        Some(WriteTarget::Bind(target)) => ctx.rt.set_var(target, v),
        None => (),
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
    var: RtVar,
    out: TagValue,
    elems: PhantomData<fn() -> C>,
}

impl<C> Iter<C> {
    fn new(var: RtVar) -> Self {
        Self { var, out: TagValue::phantom(), elems: PhantomData }
    }
}

impl<R: Rt, C: Elements> BuiltIn<R> for Iter<C> {
    const NAME: &str = C::ITER;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self::new(RtVar::new(ctx, top_id))))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self::new(RtVar::image_decode(ctx, buf)?)))
    }
}

impl<R: Rt, C: Elements> Apply<R> for Iter<C> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.var.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
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
                ctx.rt.set_var(self.var.id, e);
            }
        }
        match ctx.event.variables.get(&self.var.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match bottom {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.release(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.remint(ctx);
        self.out = TagValue::phantom();
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.out = TagValue::phantom();
    }
}

/// What a builtin sent through its own reply variable and has not yet
/// received. A sleep takes it back, so a pause loses no delivery: the
/// reply id is reminted and the items are sent again from the wake.
#[derive(Debug)]
struct InFlight<T>(VecDeque<T>);

impl<T> Default for InFlight<T> {
    fn default() -> Self {
        Self(VecDeque::new())
    }
}

impl<T> InFlight<T> {
    fn send<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>, var: &RtVar, item: T, v: Value) {
        ctx.rt.set_var(var.id, v);
        self.0.push_back(item);
    }

    /// The delivery this cycle, if one arrived.
    fn receive<R: Rt>(&mut self, ctx: &ExecCtx<'_, R>, var: &RtVar) -> Option<Value> {
        let v = ctx.event.variables.get(&var.id).map(|tv| tv.value_cloned())?;
        self.0.pop_front();
        Some(v)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The items not yet received, oldest first, for a remint.
    fn take_back(&mut self) -> std::collections::vec_deque::Drain<'_, T> {
        self.0.drain(..)
    }
}

/// `iterq`: one element per `#clock` fire, the collections queued in the
/// order they fired.
#[derive(Debug)]
pub struct IterQ<C: Elements> {
    triggered: usize,
    queue: VecDeque<C::Cursor>,
    /// Elements a sleep took back, sent again before the cursors.
    returned: VecDeque<Value>,
    sent: InFlight<Value>,
    var: RtVar,
    out: TagValue,
}

impl<C: Elements> IterQ<C> {
    fn new(var: RtVar) -> Self {
        Self {
            triggered: 0,
            queue: VecDeque::new(),
            returned: VecDeque::new(),
            sent: InFlight::default(),
            var,
            out: TagValue::phantom(),
        }
    }
}

impl<R: Rt, C: Elements> BuiltIn<R> for IterQ<C> {
    const NAME: &str = C::ITERQ;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self::new(RtVar::new(ctx, top_id))))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self::new(RtVar::image_decode(ctx, buf)?)))
    }
}

impl<R: Rt, C: Elements> Apply<R> for IterQ<C> {
    /// The queue and the banked fires exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.triggered > 0
            || !self.queue.is_empty()
            || !self.returned.is_empty()
            || !self.sent.is_empty()
        {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.var.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
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
            && let Some(e) = self.returned.pop_front()
        {
            self.sent.send(ctx, &self.var, e.clone(), e);
            self.triggered -= 1;
        }
        while self.triggered > 0
            && let Some(c) = self.queue.front_mut()
        {
            if ctx.interrupted() {
                return self.out.ride();
            }
            match C::next(c) {
                Some(e) => {
                    self.sent.send(ctx, &self.var, e.clone(), e);
                    self.triggered -= 1;
                }
                None => {
                    self.queue.pop_front();
                }
            }
        }
        match self.sent.receive(ctx, &self.var) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match bottom {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.release(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.remint(ctx);
        let back: SmallVec<[Value; 4]> = self.sent.take_back().collect();
        self.triggered += back.len();
        for e in back.into_iter().rev() {
            self.returned.push_front(e)
        }
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.queue.clear();
        self.returned.clear();
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

        impl<R: ::graphix_compiler::Rt>
            $crate::EvalCached<R> for $ev
        {
            const EFFECT: ::graphix_compiler::effects::Effect =
                ::graphix_compiler::effects::Effect::Stateless(Some(
                    ::graphix_compiler::FastCall::Plain($fc),
                ));
            const NAME: &str = $name;

            fn eval(
                &mut self,
                ctx: &mut ::graphix_compiler::ExecCtx<'_, R>,
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

            fn image_decode<R: ::graphix_compiler::Rt>(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R>,
                buf: &mut &[u8],
            ) -> ::std::result::Result<Self, ::netidx_core::pack::PackError> {
                Ok(Self { resolved: ::netidx_core::pack::Pack::decode(buf)? })
            }
        }

        impl<R: ::graphix_compiler::Rt>
            $crate::EvalCached<R> for $ev
        {
            const EFFECT: ::graphix_compiler::effects::Effect =
                ::graphix_compiler::effects::Effect::Stateless(Some(
                    ::graphix_compiler::FastCall::Typed($fc),
                ));
            const NAME: &str = $name;

            fn init(
                _ctx: &mut ::graphix_compiler::CompileCtx<R>,
                _typ: &::graphix_compiler::typ::FnType,
                resolved: Option<&::graphix_compiler::typ::FnType>,
                _scope: &::graphix_compiler::Scope,
                _from: &[::graphix_compiler::Node<R>],
                _top_id: ::graphix_compiler::expr::ExprId,
            ) -> Self {
                Self { resolved: resolved.cloned() }
            }

            fn typecheck1(
                &mut self,
                _ctx: &mut ::graphix_compiler::CompileCtx<R>,
                _from: &mut [::graphix_compiler::Node<R>],
                resolved: &::graphix_compiler::typ::FnType,
            ) -> ::anyhow::Result<()> {
                self.resolved = Some(resolved.clone());
                Ok(())
            }

            fn eval(
                &mut self,
                ctx: &mut ::graphix_compiler::ExecCtx<'_, R>,
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

            fn image_decode<R: ::graphix_compiler::Rt>(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R>,
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

            fn image_decode<R: ::graphix_compiler::Rt>(
                _ctx: &mut ::graphix_compiler::ExecCtx<'_, R>,
                buf: &mut &[u8],
            ) -> ::std::result::Result<Self, ::netidx_core::pack::PackError> {
                ::netidx_core::pack::Pack::decode(buf)
            }
        }
    )*};
}

impl CachedVals {
    pub fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.slots.len() as u64, buf);
        for (v, _) in self.slots.iter() {
            v.encode(buf)?;
        }
        for (_, t) in self.slots.iter() {
            t.bits().encode(buf)?;
        }
        Ok(())
    }

    pub fn image_decode(buf: &mut &[u8]) -> Result<Self, PackError> {
        let n = decode_varint(buf)? as usize;
        let mut vals: LPooled<Vec<Option<Value>>> = LPooled::take();
        for _ in 0..n {
            vals.push(<Option<Value> as Pack>::decode(buf)?);
        }
        let mut slots = Vec::with_capacity(n);
        for v in vals.drain(..) {
            slots.push((v, Tag::from_raw(u8::decode(buf)?)));
        }
        Ok(CachedVals { slots: slots.into(), evaluated: Evaluated(false) })
    }

    pub fn new<R: Rt>(from: &[Node<R>]) -> CachedVals {
        Self::from_values(from.iter().map(|_| None))
    }

    /// Slots holding `values`, each as if it just fired.
    pub fn from_values(values: impl IntoIterator<Item = Option<Value>>) -> CachedVals {
        CachedVals {
            slots: values.into_iter().map(|v| (v, Tag::FIRED)).collect(),
            evaluated: Evaluated(false),
        }
    }

    pub fn len(&self) -> usize {
        self.slots.len()
    }

    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Argument `i`'s value, when it has one.
    pub fn value(&self, i: usize) -> Option<&Value> {
        self.slots.get(i)?.0.as_ref()
    }

    /// Each argument's value, `None` where it has none.
    pub fn values(&self) -> impl Iterator<Item = Option<&Value>> {
        self.slots.iter().map(|(v, _)| v.as_ref())
    }

    /// The tag argument `i` last arrived with.
    pub fn tag(&self, i: usize) -> Option<Tag> {
        Some(self.slots.get(i)?.1)
    }

    pub fn clear(&mut self) {
        for (v, t) in self.slots.iter_mut() {
            *v = None;
            *t = Tag::FIRED;
        }
        self.evaluated = Evaluated(false);
    }

    /// True if any arg slot is bottom — tainted or never delivered. The
    /// wrapper bottoms the invocation on this instead of calling `eval`,
    /// so builtin authors never see a bottomed or missing arg.
    pub fn any_bottom(&self) -> bool {
        self.slots.iter().any(|(v, t)| v.is_none() || t.is_bottom())
    }

    /// Update the slots from the arg nodes (a stale production refreshes
    /// its slot silently; a tainted one marks the slot's tag but keeps the
    /// value) and say what that means for the invocation.
    pub fn update<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        from: &mut [Node<R>],
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
    pub fn update_full<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        from: &mut [Node<R>],
    ) -> Option<Tag> {
        let mut prod: Option<Tag> = None;
        for (src, (slot, slot_tag)) in from.iter_mut().zip(self.slots.iter_mut()) {
            let tv = src.update(ctx);
            let tag = tv.tag();
            if tag.is_bottom() {
                if !slot_tag.is_bottom() {
                    self.evaluated = Evaluated(false);
                }
                *slot_tag = Tag::STALE_BOTTOM;
            } else {
                let evaluated = &mut self.evaluated;
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
                *slot_tag = tag;
            }
            prod = Some(match prod {
                None => tag,
                Some(p) => p.join(tag),
            });
        }
        prod
    }

    /// Argument `i` as the abstract type `T` it holds.
    pub fn abstract_arg<T: Any + Send + Sync>(&self, i: usize) -> Option<&T> {
        match self.value(i)? {
            Value::Abstract(a) => a.downcast_ref::<T>(),
            _ => None,
        }
    }

    pub fn get<T: FromValue>(&self, i: usize) -> Option<T> {
        self.value(i).and_then(|v| v.clone().cast_to::<T>().ok())
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
    for v in from.values() {
        args.push(v?.clone());
    }
    Some(args)
}

/// Run a builtin's fast fn over the cached argument slots —
/// the node-walk half of a fastcall builtin, so `eval` and the JIT share
/// one implementation.
pub fn fast_eval<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    f: FastFn,
    from: &CachedVals,
) -> Option<Value> {
    let args = fast_args(from)?;
    coretraits::with_hooks(ctx, || f(&args))
}

/// [`fast_eval`] for a `FastCall::Typed` fn: `typ` is the call site's
/// resolved function type (from `typecheck1`).
pub fn fast_eval_typed<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    f: TypedFastFn,
    typ: &FnType,
    from: &CachedVals,
) -> Option<Value> {
    let args = fast_args(from)?;
    coretraits::with_display_hooks(ctx, |env| f(env, typ, &args))
}

/// A number ordered exactly against any other, whatever their kinds:
/// an integer, a decimal or a float, a NaN below every number.
#[derive(Debug, Clone, Copy)]
pub enum ExactNum {
    Int(i128),
    Dec(Decimal),
    Float(f64),
}

impl ExactNum {
    /// `v` as a number when it is one.
    pub fn of(v: &Value) -> Option<Self> {
        Some(match v {
            Value::I8(n) => Self::Int(*n as i128),
            Value::I16(n) => Self::Int(*n as i128),
            Value::I32(n) | Value::Z32(n) => Self::Int(*n as i128),
            Value::I64(n) | Value::Z64(n) => Self::Int(*n as i128),
            Value::U8(n) => Self::Int(*n as i128),
            Value::U16(n) => Self::Int(*n as i128),
            Value::U32(n) | Value::V32(n) => Self::Int(*n as i128),
            Value::U64(n) | Value::V64(n) => Self::Int(*n as i128),
            Value::Decimal(d) => Self::Dec(**d),
            Value::F32(f) => Self::Float(*f as f64),
            Value::F64(f) => Self::Float(*f),
            _ => return None,
        })
    }

    /// The integer or decimal `s` writes, exactly.
    pub fn parse(s: &str) -> Option<Self> {
        match s.parse::<i128>() {
            Ok(n) => Some(Self::Int(n)),
            Err(_) => s.parse::<Decimal>().ok().map(Self::Dec),
        }
    }
}

impl Ord for ExactNum {
    fn cmp(&self, other: &Self) -> Ordering {
        use ExactNum::*;
        match (self, other) {
            (Int(a), Int(b)) => a.cmp(b),
            (Dec(a), Dec(b)) => a.cmp(b),
            (Float(a), Float(b)) => float_cmp(*a, *b),
            (Int(i), Dec(d)) => int_dec_cmp(*i, d),
            (Dec(d), Int(i)) => int_dec_cmp(*i, d).reverse(),
            (Int(i), Float(f)) => int_float_cmp(*i, *f),
            (Float(f), Int(i)) => int_float_cmp(*i, *f).reverse(),
            (Dec(d), Float(f)) => dec_float_cmp(d, *f),
            (Float(f), Dec(d)) => dec_float_cmp(d, *f).reverse(),
        }
    }
}

impl PartialOrd for ExactNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for ExactNum {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for ExactNum {}

/// Floats in the order `Value` keeps: NaN equal to NaN and below every
/// number.
fn float_cmp(a: f64, b: f64) -> Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true, true) => Ordering::Equal,
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        (false, false) => a.partial_cmp(&b).expect("no NaN"),
    }
}

/// A decimal's whole part, which always fits an i128.
fn dec_whole(d: &Decimal) -> i128 {
    d.trunc().to_i128().expect("a decimal's whole part fits an i128")
}

fn int_dec_cmp(i: i128, d: &Decimal) -> Ordering {
    i.cmp(&dec_whole(d)).then_with(|| Decimal::ZERO.cmp(&d.fract()))
}

fn int_float_cmp(i: i128, f: f64) -> Ordering {
    if f.is_nan() || f < i128::MIN as f64 {
        Ordering::Greater
    } else if f >= i128::MAX as f64 {
        Ordering::Less
    } else {
        let whole = f.trunc();
        i.cmp(&(whole as i128)).then_with(|| 0f64.total_cmp(&(f - whole)))
    }
}

fn dec_float_cmp(d: &Decimal, f: f64) -> Ordering {
    // 2^96: no decimal reaches it
    const PAST_EVERY_DECIMAL: f64 = 79228162514264337593543950336.0;
    if f.is_nan() || f <= -PAST_EVERY_DECIMAL {
        Ordering::Greater
    } else if f >= PAST_EVERY_DECIMAL {
        Ordering::Less
    } else {
        let whole = f.trunc();
        dec_whole(d).cmp(&(whole as i128)).then_with(|| fract_cmp(d.fract(), f - whole))
    }
}

/// Two fractions, each under 1 in magnitude, compared exactly: `d` is
/// `n / 10^s` and `f` is `m / 2^e`, so `d` against `f` is `n * 2^e`
/// against `m * 5^s * 2^s`.
fn fract_cmp(d: Decimal, f: f64) -> Ordering {
    let sign = d.cmp(&Decimal::ZERO).cmp(&f.partial_cmp(&0.0).expect("no NaN"));
    if sign != Ordering::Equal || d.is_zero() {
        return sign;
    }
    let bits = f.abs().to_bits();
    let (biased, fraction) = ((bits >> 52) as u32, bits & ((1 << 52) - 1));
    let (m, e) = match biased {
        0 => (fraction, 1074),
        b => (fraction | 1 << 52, 1075 - b),
    };
    let (n, s) = (d.mantissa().unsigned_abs(), d.scale());
    let rhs = m as u128 * 5u128.pow(s);
    let magnitude =
        if e >= s { shl_cmp(n, e - s, rhs) } else { shl_cmp(rhs, s - e, n).reverse() };
    if d.is_sign_negative() { magnitude.reverse() } else { magnitude }
}

/// `a * 2^k` against `b`.
fn shl_cmp(a: u128, k: u32, b: u128) -> Ordering {
    if a != 0 && (k >= 128 || a.leading_zeros() < k) {
        Ordering::Greater
    } else {
        a.checked_shl(k).unwrap_or(0).cmp(&b)
    }
}

/// A value as `#numeric` compares it: a number, or a string writing an
/// integer or a decimal, exactly ([`ExactNum`]); any other value cast to
/// f64 where it casts, and in `Value`'s order where it does not.
enum NumKey {
    Num(ExactNum),
    Other(Value),
}

impl NumKey {
    fn of(v: &Value) -> Self {
        match v {
            v if let Some(n) = ExactNum::of(v) => Self::Num(n),
            Value::String(s) if let Some(n) = ExactNum::parse(s.trim()) => Self::Num(n),
            v => match v.clone().cast(Typ::F64) {
                Some(Value::F64(f)) => Self::Num(ExactNum::Float(f)),
                _ => Self::Other(v.clone()),
            },
        }
    }

    fn cmp(&self, other: &Self) -> Ordering {
        // a number orders against any other value as an f64 does
        let number = Value::F64(0.);
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a.cmp(b),
            (Self::Num(_), Self::Other(b)) => number.cmp(b),
            (Self::Other(a), Self::Num(_)) => a.cmp(&number),
            (Self::Other(a), Self::Other(b)) => a.cmp(b),
        }
    }
}

/// The sort every collection's `sort(#dir, #numeric, c)` runs: `dir`
/// is the `Direction` tag (`Ascending`/`Descending`, anything else is
/// no value), `numeric` compares values as numbers ([`NumKey`]).
pub fn sort_values(
    dir: &Value,
    numeric: bool,
    vals: impl Iterator<Item = Value>,
) -> Option<LPooled<Vec<Value>>> {
    let Some((dir, [])) = rep::variant(dir) else { return None };
    let descending = match dir.as_str() {
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
            // each element keyed once, beside it
            let mut keyed: LPooled<Vec<(NumKey, Value)>> =
                buf.iter().map(|v| (NumKey::of(v), v.clone())).collect();
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
pub trait EvalCached<R: Rt>:
    Debug + Default + Send + Sync + ImageState + 'static
{
    const NAME: &str;
    /// The builtin's classification — see `graphix_compiler::Effect`.
    const EFFECT: Effect;
    /// See `graphix_compiler::BuiltIn::ORDERED`.
    const ORDERED: bool = false;

    fn init(
        _ctx: &mut CompileCtx<R>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R>],
        _top_id: ExprId,
    ) -> Self {
        Self::default()
    }

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R>, from: &CachedVals) -> Option<Value>;

    /// The call's value over arguments that did not fire, without the
    /// effect an `Effect::Io` builtin's `eval` performs.
    fn quiet(&mut self, ctx: &mut ExecCtx<'_, R>, from: &CachedVals) -> Option<Value> {
        self.eval(ctx, from)
    }

    fn typecheck0(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
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

impl<R: Rt, T: EvalCached<R>> BuiltIn<R> for CachedArgs<T> {
    const EFFECT: Effect = T::EFFECT;
    const NAME: &str = T::NAME;
    const ORDERED: bool = T::ORDERED;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let t = CachedArgs::<T> {
            woke_pending: false,
            cached: CachedVals::new(from),
            last_result: TagValue::phantom(),
            t: T::init(ctx, typ, resolved, scope, from, top_id),
        };
        Ok(Box::new(t))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let woke_pending = bool::decode(buf)?;
        let cached = CachedVals::image_decode(buf)?;
        let t = T::image_decode(ctx, buf)?;
        Ok(Box::new(Self { woke_pending, cached, last_result: TagValue::phantom(), t }))
    }
}

impl<R: Rt, T: EvalCached<R>> Apply<R> for CachedArgs<T> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.woke_pending.encode(buf)?;
        self.cached.image_encode(buf)?;
        self.t.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let woke = std::mem::take(&mut self.woke_pending);
        let (ev, cached, last_result) =
            (&mut self.t, &mut self.cached, &mut self.last_result);
        Self::update_inner(ev, cached, last_result, woke, ctx, from)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R>,
        from: &mut [Node<R>],
    ) -> Result<()> {
        self.t.typecheck0(ctx, from)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R>,
        from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.t.typecheck1(ctx, from, resolved)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.woke_pending = true;
        self.cached.evaluated = Evaluated(false);
    }
}

pub trait EvalCachedAsync: Debug + Default + Send + Sync + ImageState + 'static {
    const NAME: &str;

    const CALLS: Calls = Calls::Fifo;

    /// Whether an `eval` may be dropped before it completes. One that may
    /// not (a write cut off mid-way, a read that accumulates, a handshake,
    /// work on a blocking thread, a spawned child) is let finish when its
    /// site sleeps. Either way nothing answers after a sleep: a reply to a
    /// call made before it is dropped, or handed to [`Self::reclaim`]'s
    /// closure. Never `false` under [`Calls::Latest`], which aborts.
    // XCR claude for claude: [bug/high] The default makes a sleep throw away the reply of
    // evals whose work no abort stops. The db and sqlite ops run under spawn_blocking,
    // process spawn completes on its first poll, and a tcp accept may have completed
    // before the sleep. Their reply is the only copy of what they did: a popped or
    // removed entry, a commit's outcome, a child's handle and pipes, an accepted
    // connection. This breaks this constant's own contract. A db::pop_min in an arm
    // deselected the next cycle loses the entry. A spawn there leaves an orphan process
    // the program never sees. Fix: keep every completed reply across a sleep (route all
    // replies through `kept`) and let a sleep abort only the cancel-safe calls still
    // pending. Failing that, mark those evals not cancel-safe. Accept is Latest, so
    // only the first fix covers it. probe:
    // design/review-2026-10-26/repro/x-async-io-db-pop-reply-lost.gx,
    // design/review-2026-10-26/repro/x-async-io-spawn-reply-lost.gx (x-async-io-01)
    // 2026-10-26 claude: The 'failing that' branch is done for the evals named in
    // db2-01, x-builtin-effects-01, http-sqlite-db1-01 and sys-io-01 (db and sqlite
    // ops, spawn, open, tcp connect/listen are CANCEL_SAFE = false, pinned by
    // lib_tests::*::*answer*_at_the_wake). The root fix (keep every completed reply
    // across a sleep) and tcp accept under Latest remain for this decision.
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q16/q18,
    // discussed): nothing delivers after a sleep. A call site that sleeps opens a fresh
    // reply slot and drops any reply to the old one; work is cancelled where it can be,
    // and the book documents what cannot be (a write still happens, its result is not
    // seen). A stream read whose reply is dropped returns its bytes to the stream's
    // buffer, so no data is lost. This replaces the CANCEL_SAFE split (a call not
    // cancel-safe finishing and answering at the wake).
    // 2026-10-27 claude: Built Eric's q16/q18 ruling: nothing answers a call after its
    // site's sleep. A sleep counts a generation in the site's KeptReplies, and a reply
    // of an older generation, kept or arriving later, is settled by the new
    // EvalCachedAsync::reclaim (stream reads give their bytes back to the stream through
    // a ReadTicket; later reads of that stream wait until every earlier read settles) or
    // dropped. Calls not cancel-safe still finish their work (a pop, a commit, a spawn,
    // a connect happen) and are not answered; cancel-safe ones abort as before. Docs:
    // CLAUDE.md, design/async_sleep_outputs.md, book core/select.md and
    // embedding/builtins.md, the graphix-lang skill. Pins:
    // lib_tests::{db::db_pop_is_not_answered_after_a_sleep,
    // db::db_txn_commit_is_not_answered_after_a_sleep,
    // fs::file_handle::test_open_is_not_answered_after_a_sleep,
    // sqlite::sqlite_exec_is_not_answered_after_a_sleep,
    // process::spawn_is_not_answered_after_a_sleep,
    // tcp::tcp_connect_is_not_answered_after_a_sleep, tcp::tcp_read_across_sleep (bytes
    // returned in order), db::db_not_data_answers_after_a_sleep}, sys
    // io::test::a_dropped_read_lets_the_next_read. The repro now prints only len
    // after=1, the ruled behaviour. Semantics change: needs a soak.
    const CANCEL_SAFE: bool = true;

    /// What settles the reply of a call `args` made, for an eval not
    /// cancel-safe: called once, with the reply when its site slept before
    /// answering it (a stream read returns its bytes to the stream), or
    /// with `None` when the site answers it. `None` here: nothing to settle.
    fn reclaim(_args: &Self::Args) -> Option<Reclaim> {
        None
    }

    /// The arguments that leave as data (a store's keys and values):
    /// each abstract value in them is written as its `Cast`
    /// implementation says before `prepare_args`, their tuples, structs,
    /// variants and lists kept as values ([`write_marked_data`]), so keys
    /// differ and order as the program's values do; what the builtin
    /// stores as data it strips (`rep::strip`).
    const DATA_ARGS: &[usize] = &[];

    /// Whether the answer is data read back as the call's resolved success
    /// type: cast to it, which reads an abstract type through its `Cast`
    /// implementation.
    const DATA_RESULT: bool = false;

    type Args: Debug + Any + Send + Sync;

    fn init<R: Rt>(
        _ctx: &mut CompileCtx<R>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R>],
        _top_id: ExprId,
    ) -> Self {
        Self::default()
    }

    /// Take runtime state `init` could not reach (it compiles, the
    /// runtime is not there); runs at each update, before `prepare_args`.
    fn attach<R: Rt>(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    /// map the final value with access to self and ctx
    fn map_value<R: Rt>(&mut self, _ctx: &mut ExecCtx<'_, R>, v: Value) -> Option<Value> {
        Some(v)
    }

    fn typecheck0<R: Rt>(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1<R: Rt>(
        &mut self,
        _ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
        _resolved: &FnType,
    ) -> Result<()> {
        Ok(())
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args>;
    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send;
}

impl<T> CachedArgs<T> {
    fn update_inner<'a, R: Rt>(
        ev: &mut T,
        cached: &mut CachedVals,
        last_result: &'a mut TagValue,
        woke: bool,
        ctx: &mut ExecCtx<'_, R>,
        from: &mut [Node<R>],
    ) -> &'a TagValue
    where
        T: EvalCached<R>,
    {
        let prod = cached.update_full(ctx, from);
        match prod {
            None => last_result.ride(),
            // a bottom arg bottoms the invocation without calling eval
            Some(t) if cached.any_bottom() => last_result.set_bottom_as(t),
            Some(t) if t.is_fired() => {
                cached.evaluated = Evaluated(true);
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
                    cached.evaluated = Evaluated(true);
                    match ev.quiet(ctx, cached) {
                        Some(v) => last_result.set(TagValue::stale(v)),
                        None => last_result.set_bottom(false),
                    }
                } else {
                    last_result.retag(Tag::STALE)
                }
            }
            // a bottom eval already gave over these slots stands
            Some(_) if cached.evaluated.0 => last_result.ride(),
            Some(_) => {
                // Nothing to re-surface yet: compute the value once to
                // establish the value channel, STALE.
                cached.evaluated = Evaluated(true);
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
    /// queued, so the call answers only its latest arguments; a call that
    /// completed before the abort reached it still answers. Only for an
    /// `eval` that loses nothing when dropped before it completes.
    Latest,
    /// Each at once; the replies come in the order the calls complete.
    Concurrent,
}

/// Settles a reply: takes it back when it will not be answered
/// ([`EvalCachedAsync::reclaim`]).
pub type Reclaim = Box<dyn FnOnce(Option<Value>) + Send>;

/// The replies of a site whose evals finish across a sleep, oldest first,
/// and the sleeps it has had: a reply to a call made before the latest
/// sleep is reclaimed, never answered.
#[derive(Default)]
struct KeptReplies {
    sleeps: u64,
    replies: VecDeque<(Value, Option<Reclaim>)>,
}

impl KeptReplies {
    /// Take back every reply waiting, for a sleep.
    fn reclaim_all(&mut self) {
        self.sleeps += 1;
        for (v, reclaim) in self.replies.drain(..) {
            if let Some(reclaim) = reclaim {
                reclaim(Some(v))
            }
        }
    }
}

impl std::fmt::Debug for KeptReplies {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KeptReplies").field("replies", &self.replies.len()).finish()
    }
}

type Kept = triomphe::Arc<parking_lot::Mutex<KeptReplies>>;

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
    cached: CachedVals,
    var: RtVar,
    queued: VecDeque<T::Args>,
    /// calls spawned under `id` and not answered yet
    inflight: usize,
    /// the reply ids of calls a later one replaced, held until each
    /// answers: one that completed before it was replaced still answers
    replaced: SmallVec<[BindId; 1]>,
    abort: Option<Abort>,
    /// the replies of an eval not [`EvalCachedAsync::CANCEL_SAFE`], oldest
    /// first: they wait here, and the reply variable only wakes the site,
    /// so a sleep can take back those it will not answer
    kept: Option<Kept>,
    /// the error tag a panicking eval answers under, from the call's
    /// declared return type; none when it declares no such error
    panic_tag: Option<ArcStr>,
    /// the type a `DATA_RESULT` answer is read back as, where it holds an
    /// abstract type
    data_result: Option<Type>,
    out: TagValue,
    t: T,
}

/// The type a builtin answering with stored data reads it back as: its
/// success type. Stored data carries no markers and no abstract value,
/// so the read makes its tuples, structs, variants and lists and casts
/// its abstract types.
pub fn data_target(env: &Env, resolved: Option<&FnType>) -> Option<Type> {
    extract_cast_type(env, resolved)
}

/// Stored data `v` read back as `t` ([`data_target`]); data it can't be is
/// an error tagged `tag`.
pub fn read_back<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    t: &Type,
    tag: &ArcStr,
    v: Value,
) -> Value {
    match coretraits::with_display_hooks(ctx, |env| t.cast_value(env, v)) {
        Value::Error(e) => errf!(tag.clone(), "{}", ErrorText(&e)),
        v => v,
    }
}

/// The message an error's payload carries: a variant's one string
/// (`InvalidCast(msg)`), else the payload's naked form.
pub struct ErrorText<'a>(pub &'a Value);

impl std::fmt::Display for ErrorText<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match rep::variant(self.0) {
            Some((_, [Value::String(m)])) => f.write_str(m),
            _ => write!(f, "{}", graphix_compiler::typ::Naked(self.0)),
        }
    }
}

fn data_result<T: EvalCachedAsync>(env: &Env, resolved: Option<&FnType>) -> Option<Type> {
    T::DATA_RESULT.then(|| data_target(env, resolved)).flatten()
}

impl<T: EvalCachedAsync> CachedArgsAsync<T> {
    /// Data that does not cross, answered as the call's declared error.
    fn not_data(&self, why: &impl std::fmt::Display) -> Value {
        match &self.panic_tag {
            Some(tag) => errf!(tag.clone(), "{why}"),
            None => errf!(literal!("InvalidCast"), "{why}"),
        }
    }
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

/// The reply of an eval aborted before it completed: it answers nothing.
static ABORTED: std::sync::LazyLock<Value> = std::sync::LazyLock::new(|| {
    errf!(arcstr::literal!("Aborted"), "an async builtin was aborted")
});

impl<T: EvalCachedAsync> CachedArgsAsync<T> {
    /// Abort the calls in flight, drop those queued and release the reply
    /// ids, which are this call's alone, as is what was stored for them.
    fn release<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.drop_calls(ctx);
        self.var.release(ctx);
    }

    /// `release`, then answer under a fresh reply id.
    fn renew<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.drop_calls(ctx);
        self.var.remint(ctx);
    }

    /// Abort the calls in flight, drop those queued and release the ids
    /// of the calls replaced.
    fn drop_calls<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.abort = None;
        self.inflight = 0;
        self.queued.clear();
        if let Some(kept) = &self.kept {
            kept.lock().reclaim_all()
        }
        for id in self.replaced.drain(..) {
            ctx.release_var(id, self.var.top_id());
        }
    }

    /// Abort the calls in flight for a newer one, answering under a fresh
    /// reply id; the old id stays until its reply comes, since a call that
    /// completed before the abort reached it has already done its work.
    fn replace<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.abort = None;
        self.inflight = 0;
        self.queued.clear();
        let old = self.var.fresh(ctx);
        self.replaced.push(old);
    }

    /// Answer `v` as a call in flight would, in a later cycle.
    fn answer_next_cycle<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>, v: Value) {
        self.inflight += 1;
        match &self.kept {
            None => ctx.rt.set_var(self.var.id, v),
            Some(kept) => {
                kept.lock().replies.push_front((v, None));
                ctx.rt.set_var(self.var.id, Value::Null)
            }
        }
    }

    /// The reply of the first replaced call that completed, its id
    /// released; another's waits for a later cycle under its own id.
    fn replaced_reply<R: Rt>(&mut self, ctx: &mut ExecCtx<'_, R>) -> Option<Value> {
        let mut reply = None;
        let top_id = self.var.top_id();
        self.replaced.retain(|id| match ctx.event.variables.remove(id) {
            None => true,
            Some(tv) => match tv.value() {
                v if v != *ABORTED && reply.is_some() => {
                    ctx.rt.set_var(*id, v);
                    true
                }
                v => {
                    ctx.release_var(*id, top_id);
                    if v != *ABORTED {
                        reply = Some(v);
                    }
                    false
                }
            },
        });
        reply
    }
}

impl<R: Rt, T: EvalCachedAsync> BuiltIn<R> for CachedArgsAsync<T> {
    const NAME: &str = T::NAME;

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        const {
            assert!(
                T::CANCEL_SAFE || !matches!(T::CALLS, Calls::Latest),
                "a Latest call aborts what it replaces, so its eval must be cancel-safe"
            )
        };
        let var = RtVar::new(ctx, top_id);
        let panic_tag = panic_tag(&ctx.env, &resolved.unwrap_or(typ).rtype);
        let data_result = data_result::<T>(&ctx.env, resolved);
        let t = CachedArgsAsync::<T> {
            var,
            cached: CachedVals::new(from),
            queued: VecDeque::new(),
            inflight: 0,
            replaced: SmallVec::new(),
            abort: None,
            kept: (!T::CANCEL_SAFE).then(Kept::default),
            panic_tag,
            data_result,
            out: TagValue::phantom(),
            t: T::init(ctx, typ, resolved, scope, from, top_id),
        };
        Ok(Box::new(t))
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let cached = CachedVals::image_decode(buf)?;
        let var = RtVar::image_decode(ctx, buf)?;
        let panic_tag = Pack::decode(buf)?;
        let data_result = Pack::decode(buf)?;
        let t = T::image_decode(ctx, buf)?;
        Ok(Box::new(Self {
            cached,
            var,
            queued: VecDeque::new(),
            inflight: 0,
            replaced: SmallVec::new(),
            abort: None,
            kept: (!T::CANCEL_SAFE).then(Kept::default),
            panic_tag,
            data_result,
            out: TagValue::phantom(),
            t,
        }))
    }
}

impl<R: Rt, T: EvalCachedAsync> Apply<R> for CachedArgsAsync<T> {
    /// The queue holds arguments already prepared for `eval`, and a
    /// running eval is a task, which exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.queued.is_empty() || self.inflight > 0 || !self.replaced.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.cached.image_encode(buf)?;
        self.var.image_encode(buf)?;
        self.panic_tag.encode(buf)?;
        self.data_result.encode(buf)?;
        self.t.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        self.t.attach(ctx);
        let invocation = self.cached.update(ctx, from);
        let start = invocation == Invocation::Fired;
        let event = ctx.event.variables.remove(&self.var.id);
        let mut reply = match &self.kept {
            None => event.map(|tv| tv.value()),
            Some(kept) if event.is_some() => {
                let mut kept = kept.lock();
                let v = kept.replies.pop_front().map(|(v, reclaim)| {
                    if let Some(reclaim) = reclaim {
                        reclaim(None)
                    }
                    v
                });
                // one answer a cycle: the next wakes the site again
                if !kept.replies.is_empty() {
                    ctx.rt.set_var(self.var.id, Value::Null);
                }
                v
            }
            Some(_) => None,
        };
        if reply.is_some() {
            self.inflight -= 1;
        }
        if let Some(earlier) = self.replaced_reply(ctx) {
            // one answer a cycle: the replaced call's first, this one's next
            if let Some(v) = reply.replace(earlier) {
                self.answer_next_cycle(ctx, v);
            }
        }
        let mut res = reply.and_then(|v| match v {
            v if self.panic_tag.is_none() && v == *PANICKED => None,
            v => self.t.map_value(ctx, v),
        });
        if let (Some(t), Some(v)) = (&self.data_result, &res)
            && !matches!(v, Value::Error(_))
        {
            let tag = self.panic_tag.clone().unwrap_or(literal!("InvalidCast"));
            res = Some(read_back(ctx, t, &tag, res.take().expect("a value")));
        }
        let data;
        let mut not_data = None;
        let cached = match T::DATA_ARGS {
            _ if !start => None,
            [] => Some(&self.cached),
            idx => match self.cached.as_data(ctx, idx) {
                Ok(c) => {
                    data = c;
                    Some(&data)
                }
                Err(e) => {
                    not_data = Some(self.not_data(&format_compact!("{e:#}")));
                    None
                }
            },
        };
        if let Some(cached) = cached
            && let Some(args) = self.t.prepare_args(cached)
        {
            if T::CALLS == Calls::Latest && self.inflight > 0 {
                self.replace(ctx);
            }
            self.queued.push_back(args);
        }
        while (T::CALLS == Calls::Concurrent || self.inflight == 0)
            && let Some(args) = self.queued.pop_front()
        {
            self.inflight += 1;
            let (id, panic_tag) = (self.var.id, self.panic_tag.clone());
            let kept = self.kept.clone().map(|k| {
                let sleeps = k.lock().sleeps;
                (k, sleeps, T::reclaim(&args))
            });
            let aborted = self.abort.get_or_insert_with(Abort::new).rx.clone();
            let eval = std::panic::AssertUnwindSafe(T::eval(args)).catch_unwind();
            ctx.rt.spawn_var(async move {
                let v = match future::select(std::pin::pin!(eval), aborted).await {
                    future::Either::Left((Ok(v), _)) => v,
                    // a panicking eval still answers, so the site runs its next call
                    future::Either::Left((Err(_), _)) => {
                        log::error!(target: graphix_compiler::FAILURE_TARGET, "{} panicked", T::NAME);
                        match &panic_tag {
                            Some(tag) => errf!(tag.clone(), "{} panicked", T::NAME),
                            None => PANICKED.clone(),
                        }
                    }
                    future::Either::Right(_) => ABORTED.clone(),
                };
                match kept {
                    None => (id, v),
                    Some((kept, sleeps, reclaim)) => {
                        let mut kept = kept.lock();
                        if kept.sleeps == sleeps {
                            kept.replies.push_back((v, reclaim));
                        } else if let Some(reclaim) = reclaim {
                            reclaim(Some(v))
                        }
                        (id, Value::Null)
                    }
                }
            });
        }
        let res = match (res, not_data) {
            // a completed reply from a prior invocation wins the cycle
            (Some(v), Some(e)) => {
                self.answer_next_cycle(ctx, e);
                Some(v)
            }
            (res, e) => res.or(e),
        };
        match res {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match invocation {
                Invocation::Bottom { fresh } => self.out.set_bottom(fresh),
                Invocation::Quiet | Invocation::Fired => self.out.ride(),
            },
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R>,
        from: &mut [Node<R>],
    ) -> Result<()> {
        self.t.typecheck0(ctx, from)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R>,
        from: &mut [Node<R>],
        resolved: &FnType,
    ) -> Result<()> {
        self.data_result = data_result::<T>(&ctx.env, Some(resolved));
        self.t.typecheck1(ctx, from, resolved)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.release(ctx);
        self.cached.clear();
    }

    /// Nothing made before a sleep answers after it: the calls in flight
    /// are aborted and those queued dropped, under a fresh reply id. An
    /// eval not cancel-safe is let finish instead, its replies taken back
    /// ([`EvalCachedAsync::reclaim`]).
    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        // XCR claude for claude: [bug/high] CachedArgsAsync has no `reborn`, so a seq step
        // entered for a new run inherits the calls of the run before. A call that is
        // not CANCEL_SAFE and was in flight when the run was aborted (by abort(e) or
        // its arm's sleep) finishes asleep. At the new run's first update its kept
        // reply is popped as this run's answer, and the new run's own call is spawned
        // behind it, so every later run gets the previous run's reply. The repro's run
        // 2 reads stream B but answers child A's "AAAA", and run 3 answers run 2's
        // "BBBB". Fix: a `reborn` that gives in-flight calls a fresh `Kept`, remints
        // the reply id, and resets inflight, slept and queued. Rule whether the bytes
        // those orphaned calls consumed are dropped or whether a rebirth waits for
        // them. probe:
        // design/review-2026-10-26/repro/core-lib-kept-reply-crosses-rebirth.gx Also
        // reported as (x-async-io-02). (core-lib-01)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q16/q18,
        // discussed): nothing delivers after a sleep. A call site that sleeps opens a
        // fresh reply slot and drops any reply to the old one; work is cancelled where
        // it can be, and the book documents what cannot be (a write still happens, its
        // result is not seen). A stream read whose reply is dropped returns its bytes
        // to the stream's buffer, so no data is lost. This replaces the CANCEL_SAFE
        // split (a call not cancel-safe finishing and answering at the wake).
        // 2026-10-27 claude: Fixed by the ruling built under x-async-io-01: a sleep
        // (which a seq abort also brings the step) advances the site's reply generation,
        // so the aborted run's read is never answered and its bytes go back to the
        // stream; the next run reads its own stream. The repro now prints "run 2: BBBB"
        // and run 3 stalls, as expected. Pins: the *_is_not_answered_after_a_sleep
        // fixtures and lib_tests::tcp::tcp_read_across_sleep; no seq-specific pin yet.
        match &self.kept {
            None => self.renew(ctx),
            Some(kept) => {
                kept.lock().reclaim_all();
                self.queued.clear();
                self.inflight = 0;
                self.var.remint(ctx);
            }
        }
    }
}

fn fc_is_err(args: &[Value]) -> Option<Value> {
    match args {
        [v] => Some(Value::Bool(matches!(v, Value::Error(_)))),
        _ => None,
    }
}

crate::fast_builtin!(IsErr, IsErrEv, "core_is_err", fc_is_err);

fn fc_error(args: &[Value]) -> Option<Value> {
    Some(Value::Error(args[0].clone().into()))
}

crate::fast_builtin!(ToError, ToErrorEv, "core_error", fc_error);

#[derive(Debug)]
struct Once {
    val: bool,
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Once {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let _ = ctx;
        Ok(Box::new(Once { val: bool::decode(buf)?, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_once";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Once { val: false, out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for Once {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.val.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
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
                        Some(TagValue::fired(tv.value_cloned()))
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

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.val = false;
        self.out = TagValue::phantom();
    }
}

/// Set the count `#n` gives take and skip: a fired `#n` restarts it, and
/// a standing one seeds a count not running (after a rebirth, or at a
/// birth whose fire a sibling consumed).
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

impl<R: Rt, const TAKE: bool> BuiltIn<R> for Counted<TAKE> {
    fn image_decode(
        _ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let n = Option::<u64>::decode(buf)?.map(|n| n as usize);
        Ok(Box::new(Self { n, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = if TAKE { "core_take" } else { "core_skip" };

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self { n: None, out: TagValue::phantom() }))
    }
}

impl<R: Rt, const TAKE: bool> Apply<R> for Counted<TAKE> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.n.map(|n| n as u64).encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
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
            (counting == TAKE).then(|| TagValue::fired(tv.value_cloned()))
        });
        match res {
            Some(tv) => self.out.set(tv),
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
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

    fn image_decode<R: Rt>(
        _ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self { refs: Pack::decode(buf)? })
    }
}

impl<R: Rt> EvalCached<R> for AllEv {
    const EFFECT: Effect =
        Effect::Stateless(Some(graphix_compiler::FastCall::Plain(fc_all)));
    const NAME: &str = "core_all";

    fn init(
        ctx: &mut CompileCtx<R>,
        _typ: &FnType,
        _resolved: Option<&FnType>,
        _scope: &Scope,
        from: &[Node<R>],
        _top_id: ExprId,
    ) -> Self {
        let refs = from.first().map(|n| n.typ()).filter(|t| t.compares_refs(&ctx.env));
        Self { refs: refs.cloned() }
    }

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R>, from: &CachedVals) -> Option<Value> {
        let Some(t) = &self.refs else { return fast_eval(ctx, fc_all, from) };
        let mut vs = from.values();
        let hd = vs.next()??;
        let named = ctx.ref_targets(t, hd);
        for v in vs {
            let v = ctx.ref_targets(t, v?);
            if !coretraits::with_hooks(ctx, || v == named) {
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
            if !graphix_compiler::fusion::emit_helpers::kernel_running() {
                log::error!(target: graphix_compiler::FAILURE_TARGET, "{name}: {e}");
            }
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
struct Filter<R: Rt> {
    pred: genn::Callback<R>,
    pending: Option<Value>,
    out: TagValue,
}

impl<R: Rt> Filter<R> {
    fn with(pred: genn::Callback<R>) -> Self {
        Self { pred, pending: None, out: TagValue::phantom() }
    }
}

impl<R: Rt> BuiltIn<R> for Filter<R> {
    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_filter";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        typ: &'a graphix_compiler::typ::FnType,
        resolved: Option<&'d FnType>,
        scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        match from {
            [_, _] => {
                let ptyp = match &resolved.unwrap_or(typ).args[1].typ {
                    Type::Fn(ft) => ft.clone(),
                    t => bail!("expected a function not {t}"),
                };
                let pred = genn::Callback::new(ctx, scope, &ptyp, &["x"], top_id)?;
                Ok(Box::new(Self::with(pred)))
            }
            _ => bail!("expected two arguments"),
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let pred = genn::Callback::image_decode(ctx, buf)?;
        Ok(Box::new(Self::with(pred)))
    }
}

impl<R: Rt> Apply<R> for Filter<R> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.pending.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.pred.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        if let Some(tv) = seam_value(from[1].update(ctx)) {
            self.pred.feed_fn(ctx, tv.clone());
        }
        let x = from[0].update(ctx);
        let x_tag = x.tag();
        if let Some(tv) = seam_value(x) {
            self.pending = Some(tv.value_cloned());
            self.pred.feed_arg(ctx, 0, tv.clone());
        }
        let pred = self.pred.update(ctx);
        if let Some(bottom) = bottom_of([x_tag, pred.tag()]) {
            return bottom;
        }
        let res = seam_tick(pred).and_then(|b| match b.value_cloned() {
            Value::Bool(true) => self.pending.clone().map(|v| TagValue::fired(v)),
            _ => None,
        });
        match res {
            Some(tv) => self.out.set(tv),
            None => self.out.ride(),
        }
    }

    fn typecheck0(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _from: &mut [Node<R>],
    ) -> anyhow::Result<()> {
        self.pred.typecheck0(ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.pred.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.pred.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.pred.sleep(ctx);
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.pending = None;
    }
}

#[derive(Debug)]
struct Queue {
    triggered: usize,
    /// Each waiting value with its arrival number.
    queue: VecDeque<(u64, Value)>,
    /// How many values have arrived.
    pushed: u64,
    sent: InFlight<(u64, Value)>,
    var: RtVar,
    out: TagValue,
}

impl Queue {
    fn new(var: RtVar) -> Self {
        Self {
            triggered: 0,
            queue: VecDeque::new(),
            pushed: 0,
            sent: InFlight::default(),
            var,
            out: TagValue::phantom(),
        }
    }
}

impl<R: Rt> BuiltIn<R> for Queue {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Self::new(RtVar::image_decode(ctx, buf)?)))
    }

    const NAME: &str = "core_queue";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        match from {
            [_, _, _, _] => Ok(Box::new(Self::new(RtVar::new(ctx, top_id)))),
            _ => {
                bail!("queue: expected four arguments (#clock, #flush, #flush_before, v)")
            }
        }
    }
}

impl<R: Rt> Apply<R> for Queue {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // banked fires and arrivals exist only once a cycle ran
        if self.triggered > 0 || self.pushed > 0 || !self.sent.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.var.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let fired = seam_tick(from[0].update(ctx)).is_some();
        let cleared = seam_tick(from[1].update(ctx)).is_some();
        let before =
            seam_tick(from[2].update(ctx)).and_then(|tv| match tv.value_cloned() {
                Value::I64(n) => Some(n),
                _ => None,
            });
        let x = from[3].update(ctx);
        let x_tag = x.tag();
        if fired {
            self.triggered += 1;
        }
        if cleared {
            self.queue.clear();
        }
        if let Some(n) = before {
            self.queue.retain(|(i, _)| *i as i64 >= n);
        }
        if let Some(tv) = seam_tick(x) {
            self.queue.push_back((self.pushed, tv.value_cloned()));
            self.pushed += 1;
        }
        let bottom = bottom_of([x_tag]);
        while self.triggered > 0
            && let Some((i, v)) = self.queue.pop_front()
        {
            self.triggered -= 1;
            self.sent.send(ctx, &self.var, (i, v.clone()), v);
        }
        match self.sent.receive(ctx, &self.var) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match bottom {
                Some(bottom) => bottom,
                None => self.out.ride(),
            },
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.release(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.remint(ctx);
        let back: SmallVec<[(u64, Value); 4]> = self.sent.take_back().collect();
        self.triggered += back.len();
        for item in back.into_iter().rev() {
            self.queue.push_front(item)
        }
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.queue.clear();
        self.triggered = 0;
        self.out = TagValue::phantom();
    }
}

#[derive(Debug)]
struct Hold {
    triggered: usize,
    current: Option<Value>,
    out: TagValue,
}

impl Hold {
    fn with(triggered: usize, current: Option<Value>) -> Self {
        Self { triggered, current, out: TagValue::phantom() }
    }
}

impl<R: Rt> BuiltIn<R> for Hold {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let _ = ctx;
        let triggered = decode_varint(buf)? as usize;
        let current = Pack::decode(buf)?;
        Ok(Box::new(Self::with(triggered, current)))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_hold";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        match from {
            [_, _] => Ok(Box::new(Self::with(0, None))),
            _ => bail!("expected two arguments"),
        }
    }
}

impl<R: Rt> Apply<R> for Hold {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.triggered as u64, buf);
        self.current.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let mut ticked = false;
        if seam_tick(from[0].update(ctx)).is_some() {
            self.triggered += 1;
            ticked = true;
        }
        if let Some(tv) = seam_tick(from[1].update(ctx)) {
            self.current = Some(tv.value_cloned());
            ticked = true;
        }
        if self.triggered > 0
            && ticked
            && let Some(v) = self.current.take()
        {
            self.triggered -= 1;
            self.out.set(TagValue::fired(v))
        } else {
            self.out.ride()
        }
    }

    fn delete(&mut self, _: &mut ExecCtx<'_, R>) {}

    fn sleep(&mut self, _: &mut ExecCtx<'_, R>) {}

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.triggered = 0;
        self.current = None;
        self.out = TagValue::phantom();
    }
}

#[derive(Debug)]
struct Range {
    var: RtVar,
    args: CachedVals,
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Range {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let var = RtVar::image_decode(ctx, buf)?;
        let args = CachedVals::image_decode(buf)?;
        Ok(Box::new(Self { var, args, out: TagValue::phantom() }))
    }

    const NAME: &str = "core_range";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        let var = RtVar::new(ctx, top_id);
        let args = CachedVals::new(from);
        Ok(Box::new(Self { var, args, out: TagValue::phantom() }))
    }
}

/// Each element of a range is one queued set_var, so the range is
/// capped at `MAX_ARRAY_INIT_LEN` elements.
fn range_len_exceeds_cap(i: i64, j: i64) -> bool {
    j as i128 - i as i128 > graphix_compiler::node::MAX_ARRAY_INIT_LEN as i128
}

impl<R: Rt> Apply<R> for Range {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.var.image_encode(buf)?;
        self.args.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let invocation = self.args.update(ctx, from);
        if invocation == Invocation::Fired {
            let err = match (self.args.value(0), self.args.value(1)) {
                (Some(Value::I64(i)), Some(Value::I64(j))) if i <= j => {
                    let e = literal!("RangeError");
                    if range_len_exceeds_cap(*i, *j) {
                        Some(errf!(
                            e,
                            "range {i}..{j} exceeds the {} element limit",
                            graphix_compiler::node::MAX_ARRAY_INIT_LEN
                        ))
                    } else {
                        for v in *i..*j {
                            ctx.rt.set_var(self.var.id, Value::I64(v));
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
        match ctx.event.variables.get(&self.var.id).map(|tv| tv.value_cloned()) {
            Some(v) => self.out.set(TagValue::fired(v)),
            None => match invocation {
                Invocation::Bottom { fresh } => self.out.set_bottom(fresh),
                _ => self.out.ride(),
            },
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.release(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.var.remint(ctx);
        self.out = TagValue::phantom();
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.out = TagValue::phantom();
    }
}

/// Where a throttle stands between emissions.
#[derive(Debug, Clone, Copy)]
enum Run {
    /// nothing emitted since it started or was reborn
    Idle,
    /// emitted at `last`, nothing held
    Open { last: Instant },
    /// emitted at `last`; `tid`'s timer releases the value held
    Pending { last: Instant, tid: BindId },
    /// emitted at `last`, a value held across a sleep: its timer starts
    /// again at the next update
    Held { last: Instant },
}

#[derive(Debug)]
struct Throttle {
    wait: Duration,
    run: Run,
    top_id: ExprId,
    last_v: Option<Value>,
    out: TagValue,
}

impl<R: Rt> BuiltIn<R> for Throttle {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
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
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Self {
            wait: Duration::ZERO,
            run: Run::Idle,
            top_id,
            last_v: None,
            out: TagValue::phantom(),
        }))
    }
}

impl<R: Rt> Apply<R> for Throttle {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // a running timer and its wall-clock mark exist only once a cycle ran
        if !matches!(self.run, Run::Idle) {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        self.wait.encode(buf)?;
        self.top_id.encode(buf)?;
        self.last_v.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
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
                    Value::Duration(d) => Some(**d),
                    _ => None,
                })
            })
            .filter(|d| *d != self.wait);
        let x = from[1].update(ctx);
        let x_tag = x.tag();
        let mut up1 = false;
        if let Some(tv) = seam_value(x) {
            up1 = tv.is_fired();
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
            () => {{
                match self.last_v.clone() {
                    Some(v) => return self.out.set(TagValue::fired(v)),
                    None => ride!(),
                }
            }};
        }
        // emit now if the wait since `last` has passed, else hold the
        // value for a timer
        macro_rules! maybe_schedule {
            ($last:expr) => {{
                let (last, now) = ($last, Instant::now());
                if now - last >= self.wait {
                    self.run = Run::Open { last: now };
                    emit!()
                } else {
                    let tid = BindId::new();
                    ctx.ref_var(tid, self.top_id);
                    ctx.rt.set_timer(tid, self.wait - (now - last));
                    self.run = Run::Pending { last, tid };
                    ride!()
                }
            }};
        }
        if let Some(d) = new_wait {
            self.wait = d;
            if let Run::Pending { last, tid } = self.run {
                ctx.release_var(tid, self.top_id);
                maybe_schedule!(last)
            }
        }
        if let Run::Held { last } = self.run {
            maybe_schedule!(last)
        }
        if up1 {
            match self.run {
                Run::Idle => {
                    self.run = Run::Open { last: Instant::now() };
                    emit!()
                }
                Run::Open { last } => maybe_schedule!(last),
                Run::Pending { .. } | Run::Held { .. } => (),
            }
        }
        if let Run::Pending { tid, .. } = self.run
            && ctx.event.variables.contains_key(&tid)
        {
            ctx.release_var(tid, self.top_id);
            self.run = Run::Open { last: Instant::now() };
            emit!()
        }
        ride!()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Run::Pending { tid, .. } = self.run {
            ctx.release_var(tid, self.top_id);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.delete(ctx);
        if let Run::Pending { last, .. } = self.run {
            self.run = Run::Held { last };
        }
        self.out = TagValue::phantom();
    }

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
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

impl<R: Rt> BuiltIn<R> for Count {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        _from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        let _ = ctx;
        Ok(Box::new(Count { count: i64::decode(buf)?, out: TagValue::phantom() }))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_count";

    fn init<'a, 'b, 'c, 'd>(
        _ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Count { count: 0, out: TagValue::phantom() }))
    }
}

impl<R: Rt> Apply<R> for Count {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.count.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let x = from[0].update(ctx);
        if let Some(bottom) = bottom_of([x.tag()]) {
            return bottom;
        }
        match seam_tick(x) {
            Some(_) => {
                self.count += 1;
                self.out.set(TagValue::fired(Value::I64(self.count)))
            }
            None => self.out.ride(),
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
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
            Err(e) => return Some(errf!(TAG, "{e:#}")),
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
    fn new<R: Rt>(ctx: &CompileCtx<R>, last: Option<Value>, from: &[Node<R>]) -> Self {
        let refs = from.first().map(|n| n.typ()).filter(|t| t.compares_refs(&ctx.env));
        Self(last, TagValue::phantom(), refs.cloned())
    }
}

impl<R: Rt> BuiltIn<R> for Uniq {
    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        from: &[Node<R>],
        buf: &mut &[u8],
    ) -> Result<Box<dyn Apply<R>>, PackError> {
        Ok(Box::new(Uniq::new(ctx, Pack::decode(buf)?, from)))
    }

    const EFFECT: Effect = Effect::Sync;
    const NAME: &str = "core_uniq";

    fn init<'a, 'b, 'c, 'd>(
        ctx: &'a mut CompileCtx<R>,
        _typ: &'a FnType,
        _resolved: Option<&'d FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R>>> {
        Ok(Box::new(Uniq::new(ctx, None, from)))
    }
}

impl<R: Rt> Apply<R> for Uniq {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.0.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
        let (last, out) = (&mut self.0, &mut self.1);
        let tv = from[0].update(ctx);
        // a bottom input is a bottom output, and what comes back from it
        // is new
        if tv.tag().is_bottom() {
            *last = None;
            return out.set_bottom(tv.tag().triggers());
        }
        let Some(v) = seam_tick(tv).map(|tv| tv.value_cloned()) else {
            return out.ride();
        };
        let cmp = match &self.2 {
            Some(t) => ctx.ref_targets(t, &v),
            None => v.clone(),
        };
        // one allocation is one value when every equality is structural:
        // a core Eq implementation need not be reflexive
        let changed = match last.as_ref() {
            Some(l) if !coretraits::hooks_live(&ctx.env) => !structural_eq(l, &cmp),
            _ => coretraits::with_hooks(ctx, || Some(&cmp) != last.as_ref()),
        };
        if changed {
            *last = Some(cmp);
            out.set(TagValue::fired(v))
        } else {
            out.ride()
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}

    fn reborn(&mut self, _ctx: &mut ExecCtx<'_, R>) {
        self.0 = None;
        self.1 = TagValue::phantom();
    }
}

/// `a == b` where every equality is structural, an array or a map both
/// share equal without a walk.
fn structural_eq(a: &Value, b: &Value) -> bool {
    let mut pairs: LPooled<Vec<(&Value, &Value)>> = LPooled::take();
    pairs.push((a, b));
    while let Some(pair) = pairs.pop() {
        match pair {
            (Value::Array(l), Value::Array(r)) => {
                if l.len() != r.len() {
                    return false;
                }
                if !std::ptr::eq(l[..].as_ptr(), r[..].as_ptr()) {
                    pairs.extend(l.iter().zip(r.iter()));
                }
            }
            (Value::Map(l), Value::Map(r)) => {
                if l.len() != r.len() {
                    return false;
                }
                if l.root().map(|n| n.identity()) != r.root().map(|n| n.identity()) {
                    for ((kl, vl), (kr, vr)) in l.into_iter().zip(r.into_iter()) {
                        pairs.push((kl, kr));
                        pairs.push((vl, vr));
                    }
                }
            }
            (Value::Error(l), Value::Error(r)) => pairs.push((&**l, &**r)),
            (l, r) if l != r => return false,
            _ => (),
        }
    }
    true
}

#[derive(Debug, Clone, Copy, netidx_derive::FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        match rep::read_variant(&v).map(|(tag, p)| (tag.as_str(), p.elems().len())) {
            Some(("Stdout", 0)) => Ok(Self::Stdout),
            Some(("Stderr", 0)) => Ok(Self::Stderr),
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

    fn emit<R: Rt>(
        self,
        ctx: &mut ExecCtx<'_, R>,
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
    fn perform<R: Rt>(self, ctx: &mut ExecCtx<'_, R>, site: &IoSite, args: &[Value]) {
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

impl<R: Rt> Apply<R> for PrintCall {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.dest.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>, from: &mut [Node<R>]) -> &TagValue {
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

    fn sleep(&mut self, _ctx: &mut ExecCtx<'_, R>) {}
}

/// Where a print builtin's output goes this cycle, and the line it
/// writes there.
fn emit_line<R: Rt>(ctx: &mut ExecCtx<'_, R>, dest: LogDest, line: &str, suffix: &str) {
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
    ($type:ident, $name:literal, $printer:expr, $result:ident, $diagnostic:literal) => {
        struct $type;

        impl<R: Rt> BuiltIn<R> for $type {
            const EFFECT: Effect = Effect::Io(Some(IoCall {
                result: $result,
                trigger: 1,
                diagnostic: $diagnostic,
            }));
            const NAME: &str = $name;

            fn init<'a, 'b, 'c, 'd>(
                _ctx: &'a mut CompileCtx<R>,
                _typ: &'a graphix_compiler::typ::FnType,
                _resolved: Option<&'d FnType>,
                _scope: &'b Scope,
                _from: &'c [Node<R>],
                _top_id: ExprId,
            ) -> Result<Box<dyn Apply<R>>> {
                let printer = $printer;
                let dest = printer.default_dest();
                Ok(Box::new(PrintCall { printer, dest, out: TagValue::phantom() }))
            }

            fn image_decode(
                _ctx: &mut ExecCtx<'_, R>,
                _from: &[Node<R>],
                buf: &mut &[u8],
            ) -> Result<Box<dyn Apply<R>>, PackError> {
                let dest = LogDest::decode(buf)?;
                Ok(Box::new(PrintCall {
                    printer: $printer,
                    dest,
                    out: TagValue::phantom(),
                }))
            }

            fn perform(ctx: &mut ExecCtx<'_, R>, site: &IoSite, args: &[Value]) {
                $printer.perform(ctx, site, args)
            }
        }
    };
}

printer!(Dbg, "core_dbg", Printer::Dbg, dbg_result, true);
printer!(Log, "core_log", Printer::Log, null_result, false);
printer!(Print, "core_print", Printer::Print, null_result, false);
printer!(Println, "core_println", Printer::Println, null_result, false);

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
        Filter as Filter<GXRt<X>>,
        Queue,
        queuefn::QueueFn as queuefn::QueueFn<GXRt<X>>,
        Hold,
        Range,
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

#[cfg(test)]
mod exact_num_test {
    use super::{ExactNum, ExactNum::*};
    use rust_decimal::Decimal;

    fn dec(s: &str) -> ExactNum {
        Dec(s.parse::<Decimal>().unwrap())
    }

    #[test]
    fn mixed_numbers_order_exactly() {
        // ascending, each strictly below the next
        let ascending = [
            Float(f64::NAN),
            Float(f64::NEG_INFINITY),
            Float(-1e30),
            dec("-79228162514264337593543950335"),
            Int(-3),
            Float(-0.1),
            dec("-0.1000000000000000055511151231"),
            dec("-0.1"),
            Int(0),
            Float(5e-324),
            dec("0.0000000000000000000000000001"),
            dec("0.1"),
            dec("0.10000000000000000001"),
            dec("0.1000000000000000055511151231"),
            Float(0.1),
            dec("0.1000000000000000055511151232"),
            dec("0.5000000000000000000000000001"),
            Int(9007199254740993),
            dec("9007199254740993.25"),
            Float(9007199254740994.0),
            dec("79228162514264337593543950335"),
            Float(1e30),
            Int(i128::MAX),
            Float(f64::INFINITY),
        ];
        for (i, a) in ascending.iter().enumerate() {
            for (j, b) in ascending.iter().enumerate() {
                assert_eq!(a.cmp(b), i.cmp(&j), "{a:?} against {b:?}");
            }
        }
        assert_eq!(Int(1), dec("1.000"));
        assert_eq!(Int(1), Float(1.0));
        assert_eq!(dec("0.5"), Float(0.5));
        assert_eq!(dec("-0.5"), Float(-0.5));
        assert_eq!(Float(f64::NAN), Float(f64::NAN));
    }
}

#[cfg(test)]
mod structural_eq_test {
    use super::structural_eq;
    use netidx::{protocol::valarray::ValArray, subscriber::Value};

    fn arr(vs: impl IntoIterator<Item = Value>) -> Value {
        Value::Array(ValArray::from_iter(vs))
    }

    #[test]
    fn agrees_with_value_equality() {
        let big = arr((0..100).map(Value::I64));
        let map = |vs: &[(i64, f64)]| -> Value {
            Value::Map(vs.iter().map(|(k, v)| (Value::I64(*k), Value::F64(*v))).collect())
        };
        let vals = [
            Value::F64(f64::NAN),
            Value::F64(0.0),
            big.clone(),
            arr((0..100).map(Value::I64)),
            arr((0..99).map(Value::I64)),
            arr([Value::I64(1), big.clone()]),
            arr([Value::I64(1), arr((0..100).map(Value::I64))]),
            arr([Value::I64(2), big.clone()]),
            arr([Value::F64(f64::NAN), big.clone()]),
            map(&[(1, 1.0), (2, f64::NAN)]),
            map(&[(1, 1.0), (2, f64::NAN)]),
            map(&[(1, 1.0), (2, 2.0)]),
            map(&[(1, 1.0)]),
            Value::Error(big.clone().into()),
            Value::Error(arr((0..100).map(Value::I64)).into()),
        ];
        for a in &vals {
            for b in &vals {
                assert_eq!(structural_eq(a, b), a == b, "{a} against {b}");
            }
        }
    }
}
