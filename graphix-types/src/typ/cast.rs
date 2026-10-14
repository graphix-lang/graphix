use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG, PrintFlag,
    env::Env,
    errf,
    expr::{ModPath, WrittenAt},
    format_with_flags, list,
    stack::ensure_sufficient,
    typ::{Type, TypeRef, params_size, setops::union_identical, tval::NakedPrefix},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::{BitFlags, bitflags, make_bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::{Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{borrow::Cow, fmt, mem};
use triomphe::Arc;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum IsAFlags {
    /// A `Type::Abstract` test also accepts a Rust-backed abstract
    /// value whose wrapper UUID is not the type's path-derived one
    /// (packages registering ad-hoc UUIDs). A Graphix-minted box
    /// always answers by its tag. An explicit `T as t` is strict.
    MatchAbstract,
    /// The type-blind leaves (`Any`, `⊥`, an unbound tvar) match
    /// nothing instead of everything: "does this type describe v"
    /// rather than "could v inhabit it".
    Strict,
}

/// A failed cast. Formatting is deferred to the report: a union tries
/// its members and discards every failure but the last.
#[derive(Debug)]
struct CastFail {
    why: &'static str,
    to: Type,
    v: Value,
}

impl fmt::Display for CastFail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "can't cast {} to {}: {}", NakedPrefix(&self.v), self.to, self.why)
    }
}

/// A cast level's result: `None` when the value already has the type.
type Cast = std::result::Result<Option<Value>, CastFail>;

/// The static source type of `v` at a cast position, with names, cells
/// and applications looked through; `None` when unknown. Of a union,
/// the member whose outer form `v` has, or the kind of collection all
/// such members are (a list, an array, a tuple and a struct share
/// `Value::Array`); `None` when they differ.
fn src_head(src: Option<&Type>, env: &Env, v: &Value) -> Option<Type> {
    let mut cur = src?.clone();
    loop {
        cur = match &cur {
            Type::TVar(_) => cur.deref_cloned()?,
            Type::App(c, a) => Type::app_filled(c, a)?,
            Type::Ref(_) => cur.lookup_ref(env).ok()?,
            Type::Set(ms) => {
                let mut heads: SmallVec<[Type; 4]> = SmallVec::new();
                for m in ms.iter() {
                    let h = src_head(Some(m), env, v)?;
                    if has_form(&h, v) {
                        heads.push(h)
                    }
                }
                return match &heads[..] {
                    [one] => Some(one.clone()),
                    [_, ..] if heads.iter().all(|h| matches!(h, Type::Array(_))) => {
                        Some(Type::Array(Arc::new(Type::Any)))
                    }
                    [_, ..] if heads.iter().all(|h| matches!(h, Type::List(_))) => {
                        Some(Type::List(Arc::new(Type::Any)))
                    }
                    _ => None,
                };
            }
            Type::Any | Type::Bottom | Type::Hole => return None,
            _ => return Some(cur),
        };
    }
}

/// Whether `v` has the outer runtime form of the head `t`; true where
/// the form says nothing.
fn has_form(t: &Type, v: &Value) -> bool {
    match (t, v) {
        (Type::Array(_), Value::Array(_)) => true,
        (Type::List(_), v) => list::is_list(v),
        (Type::Tuple(ts), Value::Array(a)) => a.len() == ts.len(),
        (Type::Struct(fs), Value::Array(a)) => a.len() == fs.len(),
        (Type::Variant(tag, ps, _), Value::String(s)) => ps.is_empty() && s == tag,
        (Type::Variant(tag, ps, _), Value::Array(a)) => {
            a.len() == ps.len() + 1 && matches!(&a[0], Value::String(s) if s == tag)
        }
        (Type::Map { .. }, Value::Map(_))
        | (Type::Error(_), Value::Error(_))
        | (Type::Abstract { .. }, Value::Abstract(_)) => true,
        (Type::Primitive(p), v) => p.contains(Typ::get(v)),
        (
            Type::Array(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(..)
            | Type::Map { .. }
            | Type::Error(_)
            | Type::Abstract { .. },
            _,
        ) => false,
        _ => true,
    }
}

/// `f` over the values of `elts`, rebuilt only if some value changed.
fn cast_elts<T>(
    elts: &[T],
    get: impl Fn(&T) -> &Value,
    mut f: impl FnMut(usize, &Value) -> Cast,
) -> std::result::Result<Option<LPooled<Vec<Value>>>, CastFail> {
    let mut out: Option<LPooled<Vec<Value>>> = None;
    for (i, e) in elts.iter().enumerate() {
        let v = get(e);
        match f(i, v)? {
            Some(c) => out
                .get_or_insert_with(|| elts[..i].iter().map(|e| get(e).clone()).collect())
                .push(c),
            None => {
                if let Some(o) = out.as_mut() {
                    o.push(v.clone())
                }
            }
        }
    }
    Ok(out)
}

/// A type reference as a path walk keys it: its definition and the
/// parameters it is applied to, so `Id<i64>` and `Id<&i64>` are two.
type RefKey = (usize, Arc<[Type]>);

/// A cast in progress: the refs expanded on the way to each value, and
/// whether the value is data from a format that writes a list as a flat
/// array, whose arrays are never a list's cells.
struct CastCx {
    hist: LPooled<AHashSet<(RefKey, usize)>>,
    data: bool,
}

/// An `is_a` walk's state: the references on its path with the value
/// each met, and each reference's expansion, made once a walk. Within a
/// walk a reference is its definition and its params' allocation, and
/// the expansions held keep those allocations alive.
#[derive(Default)]
struct IsAHist {
    path: AHashSet<((usize, usize), usize)>,
    expanded: AHashMap<(usize, usize), (Arc<[Type]>, Option<Type>)>,
}

fn ref_key(tr: &TypeRef) -> Option<RefKey> {
    tr.def_key().map(|k| (k, tr.params.clone()))
}

/// A type reference as a walk deciding a property that composes over a
/// type's parts keys it: its definition and whether each parameter has
/// the property. Exact, and finite where the parameters grow as the
/// definition recurses (`type N<'a> = [null, ('a, N<Array<'a>>)]`).
type ShapeKey = (usize, SmallVec<[bool; 4]>);

/// What a walk knows of one application: it is being decided, at its
/// depth in the walk, or what was decided.
enum Verdict<T> {
    Deciding(usize),
    Decided(T),
}

struct Verdicts<T> {
    known: LPooled<AHashMap<ShapeKey, Verdict<T>>>,
    /// The number of applications being decided.
    depth: usize,
    /// The shallowest application being decided that the decision in
    /// progress read.
    read: usize,
}

impl<T> Verdicts<T> {
    fn new() -> Self {
        Self { known: LPooled::take(), depth: 0, read: usize::MAX }
    }
}

/// `decide` the application `k` once per walk. A repeat while it is being
/// decided answers `cycle`, an assumption that holds coinductively for
/// `k` itself; a decision that read the assumption of an application
/// further out answers `cycle` only provisionally, since that application
/// may yet fail it, so it is not kept.
fn decide_once<T: Clone + PartialEq>(
    seen: &mut Verdicts<T>,
    k: ShapeKey,
    cycle: T,
    decide: impl FnOnce(&mut Verdicts<T>) -> T,
) -> T {
    match seen.known.get(&k) {
        Some(Verdict::Deciding(depth)) => {
            seen.read = seen.read.min(*depth);
            cycle
        }
        Some(Verdict::Decided(t)) => t.clone(),
        None => {
            let depth = seen.depth;
            seen.depth += 1;
            seen.known.insert(k.clone(), Verdict::Deciding(depth));
            let outer = mem::replace(&mut seen.read, usize::MAX);
            let t = decide(seen);
            seen.depth -= 1;
            // CR claude for claude: [perf/medium] A `cycle` verdict that read an outer
            // application's assumption is removed here, so every later occurrence of
            // the same application decides again. For a cycle of typedefs where each
            // one names the next twice, the work doubles per typedef. With 22 typedefs,
            // a program that only prints `null` takes about 12 s, because check_cast,
            // holds_ref, reaches_out and holds_abstract all go through decide_once.
            // Keep these provisional verdicts in a pending list tagged with the
            // shallowest depth they read. When that application finishes with `cycle`,
            // commit them as Decided. When it finishes otherwise, drop them. probe:
            // design/review-2026-10-12/repro/t-cast-setops-16-decide-once-exponential.gx
            // (t-cast-setops-09)
            if seen.read < depth && t == cycle {
                seen.known.remove(&k);
            } else {
                seen.known.insert(k, Verdict::Decided(t.clone()));
            }
            seen.read = seen.read.min(outer);
            t
        }
    }
}

impl Type {
    fn check_cast_int(
        &self,
        env: &Env,
        seen: &mut Verdicts<Option<ArcStr>>,
    ) -> Result<()> {
        ensure_sufficient(|| self.check_cast_inner(env, seen))
    }

    fn check_cast_inner(
        &self,
        env: &Env,
        seen: &mut Verdicts<Option<ArcStr>>,
    ) -> Result<()> {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.check_cast_int(env, seen),
                None => bail!("can't cast a value to a type constructor"),
            },
            Type::Hole => bail!("can't cast a value to a type constructor"),
            Type::Primitive(_) | Type::Any => Ok(()),
            Type::Fn(_) => bail!("can't cast a value to a function"),
            Type::Bottom => bail!("can't cast a value to bottom"),
            Type::Abstract { .. } => {
                bail!("can't cast a value to an abstract type; use its constructor")
            }
            Type::TVar(_) => match self.deref_cloned() {
                Some(t) => t.check_cast_int(env, seen),
                None => bail!("can't cast a value to a free type variable"),
            },
            Type::ByRef(..) => bail!("can't cast a reference"),
            Type::Ref(tr) => {
                let t = self.lookup_ref(env)?;
                let shape = tr
                    .params
                    .iter()
                    .map(|p| p.check_cast_int(env, seen).is_ok())
                    .collect();
                let Some(k) = tr.def_key().map(|k| (k, shape)) else {
                    return t.check_cast_int(env, seen);
                };
                let refused = decide_once(seen, k, None, |seen| {
                    t.check_cast_int(env, seen)
                        .err()
                        .map(|e| ArcStr::from(format_compact!("{e:#}").as_str()))
                });
                match refused {
                    None => Ok(()),
                    Some(why) => bail!("{why}"),
                }
            }
            t => {
                let mut r = Ok(());
                t.for_each_child(&mut |c| {
                    if r.is_ok() {
                        r = c.check_cast_int(env, seen)
                    }
                });
                r
            }
        }
    }

    pub fn check_cast(&self, env: &Env) -> Result<()> {
        self.check_cast_int(env, &mut Verdicts::new())
    }

    /// Whether a value of this type can hold a reference, outside a
    /// function or an abstract type, which a cast never takes apart. A
    /// cast refuses such a source: a reference is not a number.
    #[doc(hidden)]
    pub fn holds_ref(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) => Some(true),
            Type::Fn(_) | Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a function outside an
    /// abstract type: a runtime type test tells a function from a value
    /// but not one signature from another.
    #[doc(hidden)]
    pub fn holds_fn(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Fn(_) => Some(true),
            Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a reference, a function or
    /// `Any` anywhere: what a builtin given one may reach through.
    #[doc(hidden)]
    pub fn reaches_out(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) | Type::Fn(_) | Type::Any => Some(true),
            _ => None,
        })
    }

    /// Whether a value of this type can hold an abstract value, whose
    /// core-trait impls (`Eq`, `Ord`, `Display`) a comparison or a print
    /// runs.
    #[doc(hidden)]
    pub fn holds_abstract(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Abstract { .. } | Type::Any => Some(true),
            _ => None,
        })
    }

    /// Whether some part of this type is one `leaf` says yes to; `leaf`
    /// stops the walk at a part it decides.
    fn holds(
        &self,
        env: &Env,
        seen: &mut Verdicts<bool>,
        leaf: &dyn Fn(&Type) -> Option<bool>,
    ) -> bool {
        ensure_sufficient(|| {
            if let Some(v) = leaf(self) {
                return v;
            }
            match self {
                Type::TVar(_) => {
                    self.deref_cloned().is_some_and(|t| t.holds(env, seen, leaf))
                }
                // an open constructor is unknown like an open cell: its
                // argument decides
                Type::App(c, a) => match Type::app_filled(c, a) {
                    Some(f) => f.holds(env, seen, leaf),
                    None => a.holds(env, seen, leaf),
                },
                Type::Ref(tr) => {
                    let Ok(t) = self.lookup_ref(env) else { return false };
                    let shape =
                        tr.params.iter().map(|p| p.holds(env, seen, leaf)).collect();
                    match tr.def_key() {
                        None => t.holds(env, seen, leaf),
                        Some(k) => decide_once(seen, (k, shape), false, |seen| {
                            t.holds(env, seen, leaf)
                        }),
                    }
                }
                t => {
                    let mut r = false;
                    t.for_each_child(&mut |c| r = r || c.holds(env, seen, leaf));
                    r
                }
            }
        })
    }

    fn cast_int(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        ensure_sufficient(|| self.cast_inner(env, cx, src, v))
    }

    fn cast_fail(&self, why: &'static str, v: &Value) -> CastFail {
        CastFail { why, to: self.clone(), v: v.clone() }
    }

    fn cast_inner(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.cast_int(env, cx, src, v),
                None => Ok(None),
            },
            Type::Hole => Err(self.cast_fail("a type constructor", v)),
            Type::Concrete
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::Ordered
            | Type::Discernible => Err(self.cast_fail("a constraint", v)),
            Type::Bottom | Type::Any => Ok(None),
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => Ok(None),
                _ => Err(self.cast_fail("not a function", v)),
            },
            // CR claude for eric: [bug/high] A cast to a Graphix abstract type accepts
            // any decoded GxAbstract whose id and params match (`is_a`), and never
            // checks the payload against the definition's representation. AbstractIds
            // are path-derived and stable across processes, so data from pack::read, a
            // file or netidx can carry a payload of any type. Example: a Box(string)
            // read as `Box = Abstract<i64>` makes the JIT compute `b.0 + 1` as 1 while
            // the node-walk logs a parse error and produces nothing. Worse, `holds_ref`
            // stops at `Type::Abstract`, so `Abstract<&mut i64>` passes the `Concrete`
            // read bound, and bytes naming any bind id become a writable reference to a
            // variable the program never shared. Decide the rule: either cast the
            // payload against the representation (substituted by the params) and refuse
            // a representation holding a reference or function as data, or refuse
            // abstract read targets entirely. Pin both cases. probe:
            // design/review-2026-10-12/repro/tests-types-02.sh (tests-types-02)
            Type::Abstract { .. } => {
                if self.is_a(env, v) {
                    Ok(None)
                } else {
                    Err(self.cast_fail("not this abstract type", v))
                }
            }
            Type::ByRef(..) => {
                Err(self.cast_fail("a reference can't be read from data", v))
            }
            Type::Primitive(s) => {
                if s.contains(Typ::get(v)) {
                    return Ok(None);
                }
                if let Value::Error(_) = v {
                    return Err(self.cast_fail("an error is not data", v));
                }
                if let Value::Abstract(_) = v {
                    return Err(
                        self.cast_fail("an abstract value has no primitive form", v)
                    );
                }
                // CR claude for eric: [bug/high] When the integer member of a
                // primitive-set target refuses a number, this falls through to netidx's
                // lossy `Value::cast` for a sibling member. A number (or a datetime,
                // duration or bool) casts to `null` unconditionally, to `bool` by
                // truthiness, and to `string` in the typed form "i64:300". So
                // `cast<[i8, null]>(300)` is null, `cast<[i8, bool]>(300)` is true and
                // `cast<[i8, string]>(300)` is "i64:300". In practice an optional
                // small-int field holding 300 in `json::read` or `str::parse` reads as
                // absent with no `InvalidCast`. Both engines agree, so the fuzzer
                // cannot see it. This breaks the integer-cast rule ("holds the value
                // exactly or is an InvalidCast") and json round-tripping. A string
                // source is already refused here, so the rule is uneven too. Proposed
                // rule: in `convert`, `Null` accepts only `Value::Null` (the
                // omitted-field path still works), `Bool` only a bool or
                // "true"/"false", and a number reaches `String` only by its plain
                // display or not at all inside a union with a numeric member. Then pin
                // `json::read` and `str::parse` into `[i8, null]`. Probe:
                // design/review-2026-10-12/repro/x-engine-collections-01.gx
                // (x-engine-collections-01)
                // 2026-10-13 claude: the null part is done: convert takes nothing to
                // null or to an error (x-security-02), so `cast<[i8, null]>(300)` is an
                // InvalidCast. The bool and string parts stand.
                match s.iter().find_map(|t| convert(v.clone(), t)) {
                    Some(v) => Ok(Some(v)),
                    None => Err(self.cast_fail("no primitive conversion", v)),
                }
            }
            Type::TVar(tv) => match tv.binding() {
                Some(t) => t.cast_int(env, cx, src, v),
                None => Ok(None),
            },
            Type::Error(e) => {
                let src = src_head(src, env, v);
                let src = match &src {
                    Some(Type::Error(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    Value::Error(inner) => Ok(e
                        .cast_int(env, cx, src, inner)?
                        .map(|c| Value::Error(c.into()))),
                    // data is never read into an error
                    v => Err(self.cast_fail("not an error", v)),
                }
            }
            // An array and a list share `Value::Array`: the source's
            // static type says which one the value is, the shape only
            // when the source is unknown.
            Type::Array(et) => {
                let src = src_head(src, env, v);
                let from_list = matches!(&src, Some(Type::List(_)));
                if from_list && !list::is_list(v) {
                    return Err(self.cast_fail("not a list", v));
                }
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    _ if from_list => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in list::Iter::new(v.clone()) {
                            let c = et.cast_int(env, cx, src, &el)?;
                            out.push(c.unwrap_or(el));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    Value::Array(elts) => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |_, el| et.cast_int(env, cx, src, el),
                    )?
                    .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..))))),
                    v => {
                        let c = et.cast_int(env, cx, src, v)?;
                        Ok(Some(Value::Array([c.unwrap_or_else(|| v.clone())].into())))
                    }
                }
            }
            Type::List(et) => {
                let src = src_head(src, env, v);
                let spine = match &src {
                    Some(Type::List(_)) if !list::is_list(v) => {
                        return Err(self.cast_fail("not a list", v));
                    }
                    Some(Type::List(_)) => true,
                    Some(Type::Array(_)) => false,
                    // CR claude for eric: [bug/medium] With no source type, this
                    // guesses that any array that parses as a cons spine is already a
                    // list. An ordinary array whose last element is [] (or a 2-array
                    // chain ending in one) is then walked as a spine and loses
                    // elements. `cast<List<Array<i64>>>` of an Any `[[1], []]` gives a
                    // 1-element list. The json writer flattens a List to a plain array
                    // (core's Shape::Seq), so `json::write_str([<[1, 2], []>])` gives
                    // "[[1,2],[]]", and `json::read` of that into List<Array<i64>>
                    // gives `[<[1, 2]>]`. That breaks the rule that what the json and
                    // toml writers emit reads back into its type. The guess exists so
                    // that a List widened to Any can be cast back, so fixing it needs a
                    // decision. Options: a cons rep a flat array cannot be mistaken
                    // for; readers (json, toml, netidx data) that cast with an Array
                    // source so they never sniff; or drop the sniff and convert untyped
                    // arrays element by element. Pin `[[1], []]` and the json round
                    // trip either way. probe:
                    // design/review-2026-10-12/repro/t-expr-core-01.gx (t-expr-core-01)
                    // 2026-10-13 claude: the data readers (json, toml, sqlite) now cast
                    // through cast_data, so their arrays never reach this guess; what
                    // stands is the in-language cast of an Any. Pins:
                    // lib_tests::json::flat_list_round_trips (json and toml).
                    _ => !cx.data && list::len(v).is_some(),
                };
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    _ if spine => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        let mut changed = false;
                        for el in list::Iter::new(v.clone()) {
                            match et.cast_int(env, cx, src, &el)? {
                                Some(c) => {
                                    changed = true;
                                    out.push(c)
                                }
                                None => out.push(el),
                            }
                        }
                        Ok(changed.then(|| list::from_iter(out.drain(..))))
                    }
                    Value::Array(elts) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in elts.iter() {
                            let c = et.cast_int(env, cx, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(list::from_iter(out.drain(..))))
                    }
                    v => {
                        let c = et.cast_int(env, cx, src, v)?;
                        Ok(Some(list::from_iter([c.unwrap_or_else(|| v.clone())])))
                    }
                }
            }
            Type::Map { key, value } => {
                let src = src_head(src, env, v);
                let (ks, vs) = match &src {
                    Some(Type::Map { key, value }) => (Some(&**key), Some(&**value)),
                    _ => (None, None),
                };
                let mut entry = |k: &Value,
                                 v: &Value|
                 -> std::result::Result<_, CastFail> {
                    let ck = key.cast_int(env, cx, ks, k)?;
                    let cv = value.cast_int(env, cx, vs, v)?;
                    Ok((ck.is_some() || cv.is_some()).then(|| {
                        (ck.unwrap_or_else(|| k.clone()), cv.unwrap_or_else(|| v.clone()))
                    }))
                };
                match v {
                    Value::Map(m) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        let mut changed = false;
                        for (k, v) in m.into_iter() {
                            match entry(k, v)? {
                                Some(kv) => {
                                    changed = true;
                                    out.push(kv)
                                }
                                None => out.push((k.clone(), v.clone())),
                            }
                        }
                        Ok(changed.then(|| Value::Map(Map::from_iter(out.drain(..)))))
                    }
                    Value::Array(a) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        for p in a.iter() {
                            match p {
                                Value::Array(p) if p.len() == 2 => {
                                    let kv = entry(&p[0], &p[1])?;
                                    out.push(
                                        kv.unwrap_or_else(|| {
                                            (p[0].clone(), p[1].clone())
                                        }),
                                    )
                                }
                                _ => {
                                    return Err(
                                        self.cast_fail("expected an array of pairs", v)
                                    );
                                }
                            }
                        }
                        Ok(Some(Value::Map(Map::from_iter(out.drain(..)))))
                    }
                    _ => Err(self.cast_fail("not a map", v)),
                }
            }
            Type::Tuple(ts) => {
                let src = src_head(src, env, v);
                let ss = match &src {
                    Some(Type::Tuple(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                match v {
                    Value::Array(elts) if elts.len() == ts.len() => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |i, el| ts[i].cast_int(env, cx, ss.map(|ss| &ss[i]), el),
                    )?
                    .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..))))),
                    Value::Array(_) => Err(self.cast_fail("tuple size mismatch", v)),
                    _ => Err(self.cast_fail("not a tuple", v)),
                }
            }
            Type::Struct(ts) => {
                let Value::Array(elts) = v else {
                    return Err(self.cast_fail("not a struct", v));
                };
                if elts.len() > ts.len() {
                    return Err(self.cast_fail("struct size mismatch", v));
                }
                let mut fields: SmallVec<[(&ArcStr, &Value); 8]> =
                    match elts.iter().map(struct_field).collect::<Option<_>>() {
                        Some(f) => f,
                        None => {
                            return Err(self.cast_fail("expected an array of pairs", v));
                        }
                    };
                let mut sorted = fields.is_sorted_by_key(|(n, _)| *n);
                if !sorted {
                    fields.sort_by_key(|(n, _)| *n);
                }
                // a field the data omits reads as null where its type admits
                // null (an optional field, a key a document leaves out)
                if fields.len() < ts.len() {
                    static NULL: Value = Value::Null;
                    let mut filled: SmallVec<[(&ArcStr, &Value); 8]> = SmallVec::new();
                    let mut have = fields.iter().copied().peekable();
                    for (fname, ftyp, _) in ts.iter() {
                        match have.peek() {
                            Some((n, _)) if *n == fname => filled.extend(have.next()),
                            _ if matches!(
                                ftyp.cast_int(env, cx, None, &NULL),
                                Ok(None)
                            ) =>
                            {
                                filled.push((fname, &NULL))
                            }
                            _ => return Err(self.cast_fail("struct size mismatch", v)),
                        }
                    }
                    if have.next().is_some() {
                        return Err(self.cast_fail("struct fields mismatch", v));
                    }
                    fields = filled;
                    sorted = false;
                }
                if ts.iter().zip(fields.iter()).any(|((fname, _, _), (n, _))| n != &fname)
                {
                    return Err(self.cast_fail("struct fields mismatch", v));
                }
                let src = src_head(src, env, v);
                let ss = match &src {
                    Some(Type::Struct(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                let cast = cast_elts(
                    &fields[..],
                    |(_, fv)| *fv,
                    |i, fv| ts[i].1.cast_int(env, cx, ss.map(|ss| &ss[i].1), fv),
                )?;
                if sorted && cast.is_none() {
                    return Ok(None);
                }
                let mut out: LPooled<Vec<Value>> = LPooled::take();
                for (i, (n, fv)) in fields.iter().enumerate() {
                    let fv = match &cast {
                        Some(c) => c[i].clone(),
                        None => (*fv).clone(),
                    };
                    let pair = [Value::String((*n).clone()), fv];
                    out.push(Value::Array(ValArray::from_iter_exact(pair.into_iter())));
                }
                Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
            }
            Type::Variant(tag, ts, _) if ts.is_empty() => match v {
                Value::String(s) if s == tag => Ok(None),
                _ => Err(self.cast_fail("variant tag mismatch", v)),
            },
            Type::Variant(tag, ts, _) => {
                let src = src_head(src, env, v);
                let ss = match &src {
                    Some(Type::Variant(stag, ss, _))
                        if stag == tag && ss.len() == ts.len() =>
                    {
                        Some(ss)
                    }
                    _ => None,
                };
                let payload = |i: usize| ss.map(|ss| &ss[i]);
                match v {
                    Value::Array(elts) if elts.len() == ts.len() + 1 => {
                        if !matches!(&elts[0], Value::String(s) if s == tag) {
                            return Err(self.cast_fail("variant tag mismatch", v));
                        }
                        Ok(cast_elts(
                            &elts[1..],
                            |v| v,
                            |i, el| ts[i].cast_int(env, cx, payload(i), el),
                        )?
                        .map(|mut a| {
                            a.insert(0, Value::String(tag.clone()));
                            Value::Array(ValArray::from_iter_exact(a.drain(..)))
                        }))
                    }
                    Value::Array(elts) if elts.len() == ts.len() => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        out.push(Value::String(tag.clone()));
                        for (i, el) in elts.iter().enumerate() {
                            let c = ts[i].cast_int(env, cx, payload(i), el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    Value::Array(_) => Err(self.cast_fail("variant length mismatch", v)),
                    _ => Err(self.cast_fail("not a variant", v)),
                }
            }
            // `cx` is the current path, not a visited set: a name met
            // again on the same value expands without consuming it.
            Type::Ref(tr) => {
                let t = match self.lookup_ref(env) {
                    Ok(t) => t,
                    Err(_) => return Err(self.cast_fail("undefined type", v)),
                };
                let Some(key) = ref_key(tr).map(|k| (k, (v as *const Value).addr()))
                else {
                    return Err(self.cast_fail("undefined type", v));
                };
                // the same definition over the same value with larger params
                // grows at every level, the value never consumed
                let size = params_size(&key.0.1);
                // CR claude for claude: [perf/medium] This scans every (definition,
                // value) pair on the current path at each Ref expansion. A recursive
                // value n levels deep puts about n entries on the path, so a cast or
                // type-directed read (cast, pack/json/toml reads, sys::net subscribe
                // and call) costs O(n^2): a 40000-deep `type L = [`Cons(i64, L), `Nil]`
                // adds about 2s of cast (probe:
                // design/review-2026-10-12/repro/x-stack-cast-quadratic.gx). lookup_ref
                // also re-expands the typedef at every level. Keep the path in a map
                // keyed by (definition, value address) that holds that pair's params
                // size, so the grows check is one lookup. Cache the expansion per
                // (definition, params) as is_a's IsAHist does. (x-stack-04)
                let grows = cx.hist.iter().any(|((d, ps), at)| {
                    *d == key.0.0 && *at == key.1 && params_size(ps) < size
                });
                if grows || !cx.hist.insert(key.clone()) {
                    return Err(
                        self.cast_fail("the type recurses without consuming it", v)
                    );
                }
                let r = t.cast_int(env, cx, src, v);
                cx.hist.remove(&key);
                r
            }
            // A member the value already inhabits wins; else the first
            // member it converts to.
            Type::Set(ts) => {
                // a member whose tag the value carries is its reading,
                // whatever a tagless reading of an earlier member says
                let mut tagged = ts.iter().filter(|t| carries_tag(env, t, v)).peekable();
                if tagged.peek().is_some() {
                    let mut refused = None;
                    for t in tagged {
                        match t.cast_int(env, cx, src, v) {
                            Ok(r) => return Ok(r),
                            Err(e) => refused = Some(e),
                        }
                    }
                    return Err(refused.expect("a tagged member was tried"));
                }
                // CR claude for eric: [bug/high] When no member holds the value, this
                // takes the first member that converts, and primitive members sort
                // first while `convert` falls back to netidx `Value::cast`, which turns
                // an array into its first element, null, a bool or its Display string.
                // So cast<[Array<f64>, null]>([1, 2]) is null, cast<[bool,
                // Array<f64>]>([1, 2]) is true, cast<[string, Array<f64>]>([1, 2]) is
                // "[i64:1, i64:2]", and json::read of {"weights": [1, 2, 3]} into
                // {weights: [Array<f64>, null]} silently yields weights: null. A plain
                // cast<i64>([5, 6, 7]) is 5, against the rule that an integer cast
                // holds the value exactly or is an InvalidCast. Proposed fix: `convert`
                // refuses an array or map source for every scalar target, and this pass
                // tries the members of the value's own runtime class first, falling
                // back to cross-class conversions only when none of them converts;
                // which cross-class conversions remain (number to null, anything to
                // string) is a cast rule to decide. probe:
                // design/review-2026-10-12/repro/t-cast-setops-03.gx (t-cast-setops-03)
                let mut converted = None;
                for t in ts.iter() {
                    match t.cast_int(env, cx, src, v) {
                        Ok(None) => return Ok(None),
                        Ok(Some(c)) if converted.is_none() => converted = Some(c),
                        Ok(Some(_)) | Err(_) => (),
                    }
                }
                converted
                    .map(Some)
                    .ok_or_else(|| self.cast_fail("no member admits it", v))
            }
        }
    }

    /// Cast `v` of unknown static type to this type; a failure is the
    /// `InvalidCast` error value. An array and a list are told apart by
    /// shape.
    pub fn cast_value(&self, env: &Env, v: Value) -> Value {
        self.cast_value_int(env, None, false, v)
    }

    /// [`Self::cast_value`] of data decoded from a format that writes a
    /// list as a flat array (json, toml): every array in it is an array,
    /// never a list's cells.
    pub fn cast_data(&self, env: &Env, v: Value) -> Value {
        self.cast_value_int(env, None, true, v)
    }

    /// [`Self::cast_value`] of a `v` statically of type `src`, which
    /// decides whether a value is an array or a list.
    pub fn cast_from(&self, env: &Env, src: &Type, v: Value) -> Value {
        self.cast_value_int(env, Some(src), false, v)
    }

    fn cast_value_int(
        &self,
        env: &Env,
        src: Option<&Type>,
        data: bool,
        v: Value,
    ) -> Value {
        let mut cx = CastCx { hist: LPooled::take(), data };
        match self.cast_int(env, &mut cx, src, &v) {
            Ok(None) => v,
            Ok(Some(v)) => v,
            Err(e) => errf!(CAST_ERR_TAG, "{e}"),
        }
    }

    fn is_a_int(
        &self,
        env: &Env,
        hist: &mut IsAHist,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        ensure_sufficient(|| self.is_a_int_inner(env, hist, flags, v))
    }

    fn is_a_int_inner(
        &self,
        env: &Env,
        hist: &mut IsAHist,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.is_a_int(env, hist, flags, v),
                None => !flags.contains(IsAFlags::Strict),
            },
            Type::Hole
            | Type::Concrete
            | Type::Function
            | Type::Singleton
            | Type::Discernible
            | Type::Ordered
            | Type::OneNumber => false,
            // `hist` is the current path, not a visited set: a repeat
            // on the path is a name expanding without consuming value
            // structure; a repeat off the path is union backtracking.
            // a test commits nothing, and expands each ref once a walk
            Type::Ref(tr) => {
                let def =
                    tr.def_key().or_else(|| tr.resolve_in(env).map(|r| r.def_key()));
                let Some(def) = def else { return false };
                let rk = (def, (*tr.params).as_ptr().addr());
                let t = match hist.expanded.get(&rk) {
                    Some((_, t)) => t.clone(),
                    None => {
                        let t = self.lookup_ref_with(env, false).ok().flatten();
                        hist.expanded.insert(rk, (tr.params.clone(), t.clone()));
                        t
                    }
                };
                let Some(t) = t else { return false };
                let key = (rk, (v as *const Value).addr());
                hist.path.insert(key.clone()) && {
                    let r = t.is_a_int(env, hist, flags, v);
                    hist.path.remove(&key);
                    r
                }
            }
            Type::Primitive(t) => t.contains(Typ::get(&v)),
            Type::Abstract { id, params } => match v {
                Value::Abstract(a) => {
                    match a.downcast_ref::<crate::abstract_value::GxAbstract>() {
                        Some(g) => {
                            g.id == *id
                                && g.params.len() == params.len()
                                // parameters are invariant: each one equal
                                && params.iter().zip(g.params.iter()).all(|(p, gp)| {
                                    let holds = |a: &Type, b: &Type| {
                                        a.contains_with_flags(BitFlags::empty(), env, b)
                                            .unwrap_or(false)
                                    };
                                    holds(p, gp) && holds(gp, p)
                                })
                        }
                        None => {
                            a.id().as_u64_pair().1 == id.inner()
                                || flags.contains(IsAFlags::MatchAbstract)
                        }
                    }
                }
                _ => false,
            },
            Type::Any => !flags.contains(IsAFlags::Strict),
            // `Any` elements: test the runtime class without walking.
            Type::Array(et)
                if matches!(&**et, Type::Any) && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Array(_))
            }
            Type::Array(et) => match v {
                Value::Array(a) => a.iter().all(|v| et.is_a_int(env, hist, flags, v)),
                _ => false,
            },
            // Walk the spine iteratively (heads recurse).
            Type::List(et) => {
                let mut cur = v;
                loop {
                    if list::is_nil(cur) {
                        break true;
                    }
                    match list::split(cur) {
                        Some((h, t)) => {
                            if !et.is_a_int(env, hist, flags, h) {
                                break false;
                            }
                            cur = t;
                        }
                        None => break false,
                    }
                }
            }
            Type::Map { key, value }
                if matches!(&**key, Type::Any)
                    && matches!(&**value, Type::Any)
                    && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Map(_))
            }
            Type::Map { key, value } => match v {
                Value::Map(m) => m.into_iter().all(|(k, v)| {
                    key.is_a_int(env, hist, flags, k)
                        && value.is_a_int(env, hist, flags, v)
                }),
                _ => false,
            },
            Type::Error(e) => match v {
                Value::Error(v) => e.is_a_int(env, hist, flags, v),
                _ => false,
            },
            Type::ByRef(..) => matches!(v, Value::U64(_) | Value::V64(_)),
            Type::Tuple(ts) => match v {
                Value::Array(elts) => {
                    elts.len() == ts.len()
                        && ts
                            .iter()
                            .zip(elts.iter())
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                _ => false,
            },
            Type::Struct(ts) => match v {
                Value::Array(elts) => {
                    elts.len() == ts.len()
                        && ts.iter().zip(elts.iter()).all(|((n, t, _), v)| match v {
                            Value::Array(a) if a.len() == 2 => match &a[..] {
                                [Value::String(key), v] => {
                                    n == key && t.is_a_int(env, hist, flags, v)
                                }
                                _ => false,
                            },
                            _ => false,
                        })
                }
                _ => false,
            },
            Type::Variant(tag, ts, _) if ts.len() == 0 => match &v {
                Value::String(s) => s == tag,
                _ => false,
            },
            Type::Variant(tag, ts, _) => match &v {
                Value::Array(elts) => {
                    ts.len() + 1 == elts.len()
                        && match &elts[0] {
                            Value::String(s) => s == tag,
                            _ => false,
                        }
                        && ts
                            .iter()
                            .zip(elts[1..].iter())
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                _ => false,
            },
            Type::TVar(tv) => match tv.binding() {
                None => !flags.contains(IsAFlags::Strict),
                Some(t) => t.is_a_int(env, hist, flags, v),
            },
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => true,
                _ => false,
            },
            Type::Bottom => !flags.contains(IsAFlags::Strict),
            Type::Set(ts) => ts.iter().any(|t| t.is_a_int(env, hist, flags, v)),
        }
    }

    /// True if v is structurally compatible with the type.
    pub fn is_a(&self, env: &Env, v: &Value) -> bool {
        self.is_a_int(env, &mut IsAHist::default(), BitFlags::empty(), v)
    }

    /// [`Self::is_a`] with flags.
    pub fn is_a_with(&self, env: &Env, flags: BitFlags<IsAFlags>, v: &Value) -> bool {
        self.is_a_int(env, &mut IsAHist::default(), flags, v)
    }

    /// The shallow discriminator for a select arm's INFERRED type
    /// predicate: `self` with every payload position replaced by `Any`,
    /// when the outermost shape alone tells `self`'s values apart from
    /// the other members of `scrutinee`. `None` keeps the full walk
    /// (members not enumerable, two share a shape, or no payload).
    /// Sound only for inferred predicates, which typecheck unified
    /// against their member; explicit `x as T` stays strict.
    pub fn shallow_discriminant(&self, env: &Env, scrutinee: &Type) -> Option<Type> {
        let mut scrut: LPooled<Vec<Type>> = LPooled::take();
        let mut seen: LPooled<Vec<RefKey>> = LPooled::take();
        flatten_union_members(scrutinee, env, &mut scrut, &mut seen)?;
        let mut sfacts: LPooled<Vec<MemberFacts>> = LPooled::take();
        for m in scrut.iter() {
            sfacts.push(member_facts(m));
        }
        let mut preds: LPooled<Vec<Type>> = LPooled::take();
        seen.clear();
        flatten_union_members(self, env, &mut preds, &mut seen)?;
        let mut out: LPooled<Vec<Type>> = LPooled::take();
        let mut changed = false;
        for p in preds.iter() {
            let pf = member_facts(p);
            if pf.exact {
                out.push(p.clone());
                continue;
            }
            if let Some(pa) = &pf.arr {
                let n = sfacts
                    .iter()
                    .filter(|mf| mf.arr.as_ref().is_some_and(|ma| arr_overlap(pa, ma)))
                    .count();
                if n != 1 {
                    return None;
                }
            }
            if pf.map && sfacts.iter().filter(|mf| mf.map).count() != 1 {
                return None;
            }
            if pf.error && sfacts.iter().filter(|mf| mf.error).count() != 1 {
                return None;
            }
            out.push(shallowify(p));
            changed = true;
        }
        if !changed {
            return None;
        }
        Some(if out.len() == 1 {
            out.pop().unwrap()
        } else {
            Type::Set(Arc::from_iter(out.drain(..)))
        })
    }
}

/// A struct value's `[name, value]` pair.
fn struct_field(v: &Value) -> Option<(&ArcStr, &Value)> {
    match v {
        Value::Array(a) if a.len() == 2 => match &a[0] {
            Value::String(n) => Some((n, &a[1])),
            _ => None,
        },
        _ => None,
    }
}

/// A flattened union member's runtime footprint. Variants, tuples,
/// structs and arrays all inhabit `Value::Array`; `arr` is the
/// member's constraint within that class. `exact`: the full `is_a` is
/// already O(1).
struct MemberFacts {
    arr: Option<(Option<ArcStr>, ArrCon)>,
    map: bool,
    error: bool,
    exact: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ArrCon {
    Len(usize),
    AnyLen,
}

impl MemberFacts {
    const EXACT: Self = MemberFacts { arr: None, map: false, error: false, exact: true };

    fn arr(tag: Option<ArcStr>, con: ArrCon) -> Self {
        MemberFacts { arr: Some((tag, con)), map: false, error: false, exact: false }
    }
}

fn member_facts(t: &Type) -> MemberFacts {
    match t {
        Type::Primitive(bits) => MemberFacts {
            arr: bits.contains(Typ::Array).then_some((None, ArrCon::AnyLen)),
            map: bits.contains(Typ::Map),
            error: bits.contains(Typ::Error),
            exact: true,
        },
        Type::Variant(_, ps, _) if ps.is_empty() => MemberFacts::EXACT,
        Type::Variant(tag, ps, _) => {
            MemberFacts::arr(Some(tag.clone()), ArrCon::Len(ps.len() + 1))
        }
        Type::Tuple(ts) => MemberFacts::arr(None, ArrCon::Len(ts.len())),
        Type::Struct(fs) => MemberFacts::arr(None, ArrCon::Len(fs.len())),
        // A list shapes as an array at runtime, so beside an Array
        // member the deep walk is forced.
        Type::Array(_) | Type::List(_) => MemberFacts::arr(None, ArrCon::AnyLen),
        Type::Map { .. } => MemberFacts { map: true, exact: false, ..MemberFacts::EXACT },
        Type::Error(_) => MemberFacts { error: true, exact: false, ..MemberFacts::EXACT },
        Type::Abstract { .. } | Type::Fn(_) | Type::ByRef(..) | Type::Bottom => {
            MemberFacts::EXACT
        }
        // `flatten_union_members` never yields these.
        Type::Any
        | Type::Set(_)
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::App(..)
        | Type::Hole
        | Type::Concrete
        | Type::Singleton
        | Type::OneNumber
        | Type::Discernible
        | Type::Ordered
        | Type::Function => MemberFacts::EXACT,
    }
}

fn arr_overlap(
    (ptag, pcon): &(Option<ArcStr>, ArrCon),
    (mtag, mcon): &(Option<ArcStr>, ArrCon),
) -> bool {
    match (pcon, mcon) {
        (ArrCon::AnyLen, _) | (_, ArrCon::AnyLen) => true,
        (ArrCon::Len(a), ArrCon::Len(b)) => {
            a == b
                && match (ptag, mtag) {
                    (Some(pt), Some(mt)) => pt == mt,
                    // A tuple/struct of the right length can shape
                    // like a variant.
                    _ => true,
                }
        }
    }
}

fn shallowify(t: &Type) -> Type {
    match t {
        Type::Variant(tag, ps, at) => {
            Type::Variant(tag.clone(), Arc::from_iter(ps.iter().map(|_| Type::Any)), *at)
        }
        Type::Tuple(ts) => Type::Tuple(Arc::from_iter(ts.iter().map(|_| Type::Any))),
        Type::Struct(fs) => Type::Struct(Arc::from_iter(
            fs.iter().map(|(n, _, at)| (n.clone(), Type::Any, *at)),
        )),
        Type::Array(_) => Type::Array(Arc::new(Type::Any)),
        Type::List(_) => Type::List(Arc::new(Type::Any)),
        Type::Map { .. } => {
            Type::Map { key: Arc::new(Type::Any), value: Arc::new(Type::Any) }
        }
        Type::Error(_) => Type::Error(Arc::new(Type::Any)),
        t => t.clone(),
    }
}

/// `seen` is the path of definitions being expanded.
fn flatten_union_members(
    t: &Type,
    env: &Env,
    out: &mut LPooled<Vec<Type>>,
    seen: &mut LPooled<Vec<RefKey>>,
) -> Option<()> {
    ensure_sufficient(|| match t {
        Type::Set(ts) => {
            for t in ts.iter() {
                flatten_union_members(t, env, out, seen)?;
            }
            Some(())
        }
        Type::Ref(tr) => {
            let t = t.lookup_ref(env).ok()?;
            let key = ref_key(tr)?;
            if seen.contains(&key) {
                return None;
            }
            seen.push(key);
            let res = flatten_union_members(&t, env, out, seen);
            seen.pop();
            res
        }
        Type::TVar(tv) => match tv.binding() {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::App(c, a) => match Type::app_filled(c, a) {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::Any | Type::Hole => None,
        t => {
            out.push(t.clone());
            Some(())
        }
    })
}

/// What a runtime test sees of `t` at its head: cells, filled
/// applications and typedefs expanded; `None` for what it can't see into
/// (an open cell, `Any`, a constructor), which takes any value.
fn rep_head<'a>(env: &Env, t: &'a Type) -> Option<Cow<'a, Type>> {
    let owned = |t: Type| rep_head(env, &t).map(|h| Cow::Owned(h.into_owned()));
    ensure_sufficient(|| match t {
        Type::TVar(_) => t.deref_cloned().and_then(owned),
        Type::App(c, a) => Type::app_filled(c, a).and_then(owned),
        Type::Ref(_) => t.lookup_ref(env).ok().and_then(owned),
        Type::Any
        | Type::Hole
        | Type::Concrete
        | Type::Function
        | Type::Singleton
        | Type::Discernible
        | Type::Ordered
        | Type::OneNumber => None,
        t => Some(Cow::Borrowed(t)),
    })
}

/// The primitive tags a value of another type may share.
const SHARED_PRIMS: BitFlags<Typ> =
    make_bitflags!(Typ::{String | U64 | V64 | Map | Error | Array});

/// Whether a value of `t` may also be a value of a type `t` doesn't hold.
fn shares_form(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None | Some(Type::Bottom | Type::Abstract { .. }) => false,
        Some(Type::Primitive(p)) => p.intersects(SHARED_PRIMS),
        Some(Type::Set(ts)) => ts.iter().any(|t| shares_form(env, t)),
        Some(_) => true,
    })
}

/// How a runtime-form judgment reads an open cell.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Open {
    /// As no type yet: nothing collides with it. Inference may still
    /// fill it.
    Benign,
    /// As any type it may yet bind: it collides with another open cell
    /// and with every type whose form another type shares.
    Unknown,
}

/// The pair `(a, b)` when one is an open cell [`Open::Unknown`] reads as
/// colliding with the other.
fn open_collision(env: &Env, a: &Type, b: &Type) -> Option<(Type, Type)> {
    let open = |t: &Type| match t {
        Type::TVar(tv) => tv.open_cell(),
        _ => None,
    };
    let collides = match (open(a), open(b)) {
        (Some(x), Some(y)) => x.cell_addr() != y.cell_addr(),
        (Some(_), None) => shares_form(env, b),
        (None, Some(_)) => shares_form(env, a),
        (None, None) => false,
    };
    collides.then(|| (a.clone(), b.clone()))
}

/// The primitive tags a value of the (non-primitive) head `t` has.
fn rep_prims(t: &Type) -> BitFlags<Typ> {
    match t {
        Type::Variant(_, a, _) if a.is_empty() => Typ::String.into(),
        Type::ByRef(..) => Typ::U64 | Typ::V64,
        Type::Map { .. } => Typ::Map.into(),
        Type::Error(_) => Typ::Error.into(),
        t if ArrView::of(t).is_some() => Typ::Array.into(),
        _ => BitFlags::empty(),
    }
}

/// The array a value of `t` is, when it is one.
enum ArrView {
    Exact(LPooled<Vec<Type>>),
    Of(Type),
    List(Type),
}

impl ArrView {
    fn of(t: &Type) -> Option<Self> {
        let name =
            |n: &ArcStr| Type::Variant(n.clone(), Arc::from_iter([]), WrittenAt::NOWHERE);
        Some(match t {
            Type::Tuple(ts) => ArrView::Exact(ts.iter().cloned().collect()),
            Type::Struct(fs) => ArrView::Exact(
                fs.iter()
                    .map(|(n, t, _)| Type::Tuple(Arc::from_iter([name(n), t.clone()])))
                    .collect(),
            ),
            Type::Variant(tag, args, _) if !args.is_empty() => ArrView::Exact(
                std::iter::once(name(tag)).chain(args.iter().cloned()).collect(),
            ),
            Type::Array(e) => ArrView::Of((**e).clone()),
            Type::List(e) => ArrView::List((**e).clone()),
            _ => return None,
        })
    }
}

/// The applications a walk has expanded, each once, and the path of
/// applications it is inside, each with its parameters' size.
struct Expanded<K: std::hash::Hash + Eq + 'static> {
    seen: LPooled<AHashSet<K>>,
    path: LPooled<Vec<((usize, usize), usize)>>,
}

impl<K: std::hash::Hash + Eq + 'static> Default for Expanded<K> {
    fn default() -> Self {
        Self { seen: LPooled::take(), path: LPooled::take() }
    }
}

/// An application a walk is about to expand: its key, its definition (or
/// pair of definitions) and its parameters' size.
type At<K> = (K, (usize, usize), usize);

fn def_of(t: &Type) -> usize {
    match t {
        Type::Ref(tr) => tr.def_key().unwrap_or(0),
        _ => usize::MAX,
    }
}

fn size_of(t: &Type) -> usize {
    match t {
        Type::Ref(tr) => params_size(&tr.params),
        _ => 0,
    }
}

/// How many applications of one definition with smaller parameters a
/// path may hold before the walk takes them for growing at every level:
/// a finite type may grow a few times (`type A<'a> = [null, A<Array<i64>>]`
/// from `A<i64>`), one that grows at every level never stops.
const MAX_GROWTH: usize = 4;

/// `body` inside the application `at`: `again` when the walk expanded it
/// already, `grows` when the path holds [`MAX_GROWTH`] applications of
/// its definition with smaller parameters, as one whose parameters grow
/// at every level does (`type N<'a> = [null, ('a, N<Array<'a>>)]`), which
/// no walk finishes. Parameters that stop growing on a path come from
/// finitely many types, so a walk ends.
fn walk<K: std::hash::Hash + Eq + 'static, R>(
    seen: &mut Expanded<K>,
    at: Option<At<K>>,
    again: R,
    grows: impl FnOnce() -> R,
    body: impl FnOnce(&mut Expanded<K>) -> R,
) -> R {
    let Some((k, def, size)) = at else { return body(seen) };
    if !seen.seen.insert(k) {
        return again;
    }
    if seen.path.iter().filter(|(d, s)| *d == def && *s < size).count() >= MAX_GROWTH {
        return grows();
    }
    seen.path.push((def, size));
    let r = body(seen);
    seen.path.pop();
    r
}

/// Pairs already on the walk; a finite witness never revisits one.
type RepSeen = Expanded<(Type, Type)>;

fn pair_at(a: &Type, b: &Type) -> Option<At<(Type, Type)>> {
    (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_))).then(|| {
        ((a.clone(), b.clone()), (def_of(a), def_of(b)), size_of(a) + size_of(b))
    })
}

fn ref_at(t: &Type) -> Option<At<Type>> {
    matches!(t, Type::Ref(_)).then(|| (t.clone(), (def_of(t), 0), size_of(t)))
}

fn rep_overlaps(env: &Env, seen: &mut RepSeen, a: &Type, b: &Type) -> bool {
    ensure_sufficient(|| {
        walk(
            seen,
            pair_at(a, b),
            false,
            || true,
            |seen| {
                let (Some(a), Some(b)) = (rep_head(env, a), rep_head(env, b)) else {
                    return true;
                };
                let (a, b) = (&*a, &*b);
                match (a, b) {
                    (Type::Bottom, _) | (_, Type::Bottom) => false,
                    (Type::Set(ts), _) => {
                        ts.iter().any(|t| rep_overlaps(env, seen, t, &b))
                    }
                    (_, Type::Set(ts)) => {
                        ts.iter().any(|t| rep_overlaps(env, seen, &a, t))
                    }
                    (Type::Primitive(p), Type::Primitive(q)) => p.intersects(*q),
                    (Type::Primitive(p), t) | (t, Type::Primitive(p)) => {
                        p.intersects(rep_prims(t))
                    }
                    (Type::Variant(x, xa, _), Type::Variant(y, ya, _))
                        if xa.is_empty() && ya.is_empty() =>
                    {
                        x == y
                    }
                    (Type::ByRef(..), Type::ByRef(..))
                    | (Type::Fn(_), Type::Fn(_))
                    | (Type::Map { .. }, Type::Map { .. }) => true,
                    (Type::Error(x), Type::Error(y)) => rep_overlaps(env, seen, x, y),
                    (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. }) => {
                        x == y
                    }
                    (a, b) => match (ArrView::of(a), ArrView::of(b)) {
                        (Some(x), Some(y)) => arr_overlaps(env, seen, x, y),
                        _ => false,
                    },
                }
            },
        )
    })
}

fn arr_overlaps(env: &Env, seen: &mut RepSeen, a: ArrView, b: ArrView) -> bool {
    use ArrView::*;
    let list = |seen: &mut RepSeen, x: &[Type], e: &Type| {
        x.is_empty()
            || (x.len() == 2
                && rep_overlaps(env, seen, &x[0], e)
                && rep_overlaps(env, seen, &x[1], &Type::List(Arc::new(e.clone()))))
    };
    match (a, b) {
        (Exact(x), Exact(y)) => {
            x.len() == y.len()
                && x.iter().zip(y.iter()).all(|(x, y)| rep_overlaps(env, seen, x, y))
        }
        (Exact(x), Of(e)) | (Of(e), Exact(x)) => {
            x.iter().all(|t| rep_overlaps(env, seen, t, &e))
        }
        (Exact(x), List(e)) | (List(e), Exact(x)) => list(seen, &x, &e),
        (Of(_) | List(_), Of(_) | List(_)) => true,
    }
}

fn rep_collision(
    env: &Env,
    seen: &mut RepSeen,
    open: Open,
    a: &Type,
    b: &Type,
) -> Option<(Type, Type)> {
    ensure_sufficient(|| {
        walk(
            seen,
            pair_at(a, b),
            None,
            || Some((a.clone(), b.clone())),
            |seen| {
                let (Some(ha), Some(hb)) = (rep_head(env, a), rep_head(env, b)) else {
                    return match open {
                        Open::Benign => None,
                        Open::Unknown => open_collision(env, a, b),
                    };
                };
                let (a, b) = (&*ha, &*hb);
                let mut pairs = |xs: &mut dyn Iterator<Item = (&Type, &Type)>| {
                    xs.map(|(x, y)| rep_collision(env, seen, open, x, y))
                        .find(Option::is_some)
                        .flatten()
                };
                match (a, b) {
                    (Type::Bottom, _) | (_, Type::Bottom) => None,
                    (Type::Set(ts), _) => pairs(&mut ts.iter().map(|t| (t, b))),
                    (_, Type::Set(ts)) => pairs(&mut ts.iter().map(|t| (a, t))),
                    (Type::Primitive(_), Type::Primitive(_)) => None,
                    (Type::Array(x), Type::Array(y))
                    | (Type::List(x), Type::List(y))
                    | (Type::Error(x), Type::Error(y)) => {
                        pairs(&mut std::iter::once((&**x, &**y)))
                    }
                    (
                        Type::Map { key: k0, value: v0 },
                        Type::Map { key: k1, value: v1 },
                    ) => pairs(&mut [(&**k0, &**k1), (&**v0, &**v1)].into_iter()),
                    (Type::Tuple(x), Type::Tuple(y)) if x.len() == y.len() => {
                        pairs(&mut x.iter().zip(y.iter()))
                    }
                    (Type::Struct(x), Type::Struct(y))
                        if x.len() == y.len()
                            && x.iter().zip(y.iter()).all(|(x, y)| x.0 == y.0) =>
                    {
                        pairs(&mut x.iter().zip(y.iter()).map(|(x, y)| (&x.1, &y.1)))
                    }
                    (Type::Variant(x, xa, _), Type::Variant(y, ya, _))
                        if x == y && xa.len() == ya.len() =>
                    {
                        pairs(&mut xa.iter().zip(ya.iter()))
                    }
                    // the tag tells them apart
                    (Type::Variant(x, _, _), Type::Variant(y, _, _)) if x != y => None,
                    // a Graphix-minted value carries its params and the test compares
                    // them; a Rust-backed one carries only its id, so two of its
                    // instantiations share one runtime form
                    (
                        a @ Type::Abstract { id: x, params: px },
                        b @ Type::Abstract { id: y, params: py },
                    ) if x == y => {
                        let rust_backed = env.abstract_reps.get(x).is_none();
                        let same = px.len() == py.len()
                            && px
                                .iter()
                                .zip(py.iter())
                                .all(|(p, q)| union_identical(p, q));
                        (rust_backed && !same).then(|| (a.clone(), b.clone()))
                    }
                    // a runtime test can't tell a reference's mutability
                    (Type::ByRef(m0, x), Type::ByRef(m1, y)) if m0 == m1 && x == y => {
                        None
                    }
                    (Type::Fn(x), Type::Fn(y)) if x == y => None,
                    (a, b) => rep_overlaps(env, &mut RepSeen::default(), a, b)
                        .then(|| (a.clone(), b.clone())),
                }
            },
        )
    })
}

impl Type {
    /// Two parts, one of `self` and one of `t`, that are different types
    /// with one runtime form: a value of either passes a runtime test for
    /// the other (a tuple, struct, list or payload variant and an array; a
    /// bare variant and a string; a reference and a number or another
    /// reference; two function types). A test that tells them apart can't
    /// tell such a value's type.
    pub fn rep_collision(&self, env: &Env, t: &Type) -> Option<(Type, Type)> {
        rep_collision(env, &mut RepSeen::default(), Open::Benign, self, t)
    }
}

/// Why a type is not `Discernible` or not `Ordered`.
pub enum Indiscernible {
    /// Two members of one union with one runtime form.
    Pair(Type, Type),
    /// A reference or a function, under `Ordered`: neither has an order.
    Unordered(Type),
    /// The name of a definition whose parameters grow as it recurses, so
    /// no walk decides it.
    Grows(ModPath),
}

impl Indiscernible {
    /// What an unordered type is, and how `==` and `!=` compare it.
    pub fn unordered_kind(t: &Type) -> (&'static str, &'static str) {
        match t {
            Type::Fn(_) => ("function", "by identity"),
            _ => ("reference", "by what they point to"),
        }
    }
}

// CR claude for eric: [bug/medium] Abstract types are opaque to every comparison rule:
// `rep_ambiguity` and `compared_ref` return nothing for `Type::Abstract`, and
// `require_compared` skips it. At run time, though, `GxAbstract` with no core-trait
// impl compares and orders by its payload. So `array::sort` and map keys over
// `Abstract<fn..>` or `Abstract<&T>` are accepted and order by LambdaId or bind id (the
// kind of order x-diff-par-02 ruled out for bare functions). `T(r1) == T(r2)` is false
// where `r1 == r2` and `[r1] == [r2]` are true. `U(`A) == U("A")` over `Abstract<[`A,
// string]>` is accepted and true, though `==` over `[`A, string]` is refused. Proposed
// fix: for an abstract type with no Eq/Ord impl, all three walks descend into its
// representation, with params substituted, and `map_refs` rewrites references in the
// payload. One with a user impl stays opaque. Needs your ruling on two points: a hidden
// representation would then show up in bound errors outside its module, and a
// Rust-backed abstract has no representation to descend into. probe:
// design/review-2026-10-12/repro/x-fix-types-05.gx (x-fix-types-05)
// CR claude for eric: [bug/medium] rep_ambiguity treats Type::Abstract as opaque (`_ =>
// None`), and so does Type::require_compared. But when no core Eq/Ord hook applies,
// GxAbstract's eq, cmp and hash are the payload's. So `type T = Abstract<[`A, string]>`
// makes T(`A) == T("A") true, and a map keyed by both keeps one entry. Writing `a.0 ==
// b.0` is refused, so the abstract wrapper lets an indiscernible comparison through.
// The same happens for `Box<'a> = Abstract<'a>` at Box<[`A, string]>, where the use
// site can see the argument. One of three rules is needed: (1) look through an abstract
// with no core Eq/Ord impl to its rep in env.abstract_reps, substituted by its params;
// (2) refuse comparing or keying such an abstract; or (3) refuse `Abstract<rep>` over a
// non-Discernible rep unless Eq/Ord is implemented. With a hidden rep, (1) would show
// the rep in the error message. Whichever is chosen, update design/tvar_constraints.md
// (the Discernible section) to match. probe:
// design/review-2026-10-12/repro/x-expr-walks-03.gx (x-expr-walks-03)
fn rep_ambiguity(
    env: &Env,
    seen: &mut Expanded<Type>,
    open: Open,
    keys_only: bool,
    refs: bool,
    t: &Type,
) -> Option<Indiscernible> {
    ensure_sufficient(|| {
        let grows = || match t {
            Type::Ref(tr) => Some(Indiscernible::Grows(tr.name.clone())),
            _ => unreachable!("only a reference expands"),
        };
        walk(seen, ref_at(t), None, grows, |seen| {
            let t = rep_head(env, t)?;
            let t = &*t;
            let mut parts = |ts: &mut dyn Iterator<Item = &Type>| {
                ts.map(|t| rep_ambiguity(env, seen, open, keys_only, refs, t))
                    .find(Option::is_some)
                    .flatten()
            };
            match &t {
                Type::Set(ts) => {
                    if !keys_only {
                        for (i, a) in ts.iter().enumerate() {
                            for b in &ts[i + 1..] {
                                let c = rep_collision(
                                    env,
                                    &mut RepSeen::default(),
                                    open,
                                    a,
                                    b,
                                );
                                if let Some((a, b)) = c {
                                    return Some(Indiscernible::Pair(a, b));
                                }
                            }
                        }
                    }
                    parts(&mut ts.iter())
                }
                Type::Map { key, value } => {
                    let keys = match keys_only {
                        true => rep_ambiguity(
                            env,
                            &mut Expanded::default(),
                            open,
                            false,
                            true,
                            key,
                        ),
                        false => None,
                    };
                    keys.or_else(|| parts(&mut [&**key, &**value].into_iter()))
                }
                Type::Array(e) | Type::List(e) | Type::Error(e) => {
                    parts(&mut std::iter::once(&**e))
                }
                Type::Tuple(ts) | Type::Variant(_, ts, _) => parts(&mut ts.iter()),
                Type::Struct(fs) => parts(&mut fs.iter().map(|(_, t, _)| t)),
                Type::ByRef(..) | Type::Fn(_) if refs && !keys_only => {
                    Some(Indiscernible::Unordered(t.clone()))
                }
                _ => None,
            }
        })
    })
}

impl Type {
    /// Why this type is not `Discernible`: two members of one union
    /// anywhere in it (not under a reference, a function or an abstract
    /// type) with one runtime form, which comparing its values can't tell
    /// apart; with `ordered`, why it is not `Ordered`: that, or a
    /// reference in the same places.
    pub fn indiscernible(
        &self,
        env: &Env,
        open: Open,
        ordered: bool,
    ) -> Option<Indiscernible> {
        rep_ambiguity(env, &mut Expanded::default(), open, false, ordered, self)
    }

    /// Why a map key type this type holds is not `Ordered`.
    pub fn map_key_failure(&self, env: &Env, open: Open) -> Option<Indiscernible> {
        rep_ambiguity(env, &mut Expanded::default(), open, true, false, self)
    }

    /// The refusal of `self` as a binding of `'name: bound` (`Discernible`
    /// or `Ordered`) for the reason `why`.
    pub fn not_discernible(
        &self,
        bound: &Type,
        name: &str,
        why: &Indiscernible,
    ) -> anyhow::Error {
        format_with_flags(PrintFlag::DerefTVars, || {
            let t = self.resolve_tvars();
            let what = match name.starts_with('_') {
                true => format_compact!("{t} must be {bound} here, but it"),
                false => format_compact!("'{name} must be {bound}, but {t}"),
            };
            match why {
                Indiscernible::Pair(a, b) => anyhow::anyhow!(
                    "{what} holds {} and {}, which have the same runtime form; wrap \
                     them in distinct variants",
                    a.resolve_tvars(),
                    b.resolve_tvars()
                ),
                Indiscernible::Grows(name) => anyhow::anyhow!(
                    "{what} holds {name}, whose parameters grow as it recurses, so \
                     whether its members have distinct runtime forms can't be decided"
                ),
                Indiscernible::Unordered(r) => {
                    let (kind, how) = Indiscernible::unordered_kind(r);
                    let r = r.resolve_tvars();
                    let holds = match r == t {
                        true => format_compact!("is a {kind}"),
                        false => format_compact!("holds the {kind} {r}"),
                    };
                    anyhow::anyhow!(
                        "{what} {holds}: {kind}s have no order, and only == and != \
                         compare them, {how}"
                    )
                }
            }
        })
    }

    /// Hand `bound` (`Discernible` or `Ordered`) to every open cell a
    /// comparison of this type reaches.
    pub fn require_compared(&self, bound: &Type) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.require_compared(bound),
                None => tv.add_cell_constraint(bound.clone()),
            },
            Type::ByRef(..) | Type::Fn(_) | Type::Abstract { .. } => (),
            t => t.for_each_child(&mut |c| c.require_compared(bound)),
        })
    }
}

/// Whether a comparison of values of `t` meets a reference: one where
/// [`Type::indiscernible`] looks.
// CR claude for eric: [bug/medium] compared_ref, map_refs and rep_ambiguity all stop at
// Type::Abstract. GxAbstract's eq and ord still compare the payload when no Eq/Ord impl
// rides the value, so a reference inside an abstract (in its rep or in a type
// parameter) is compared by its raw session cell id. Example: `type R =
// Abstract<&i64>`. Here `R(&x) == R(&x)` is false while `&x == &x` is true. `R(&x) <
// R(&y)` orders by cell id, which a warm start relocates. `array::sort` and the map
// literal `{R(&x) => 1, R(&y) => 2}` pass Ordered, although a bare `&i64` is refused in
// both places. One fix: when an abstract has no Eq/Ord impl, walk its rep
// (env.abstract_reps with the params substituted) in all three functions. The other:
// refuse `==` and Ordered over an abstract whose rep or params hold a reference unless
// it has an impl. The choice also decides how much of a hidden rep a refusal may reveal
// outside the defining module, and whether tvar_constraints.md's rule that the checks
// do not look under an abstract still holds. probe:
// design/review-2026-10-12/repro/x-expr-walks-04.gx (x-expr-walks-04)
fn compared_ref(env: &Env, seen: &mut Expanded<Type>, t: &Type) -> bool {
    ensure_sufficient(|| {
        walk(
            seen,
            ref_at(t),
            false,
            || true,
            |seen| {
                let Some(t) = rep_head(env, t) else { return false };
                let t = &*t;
                let mut any = |ts: &mut dyn Iterator<Item = &Type>| {
                    Iterator::any(&mut &mut *ts, |t| compared_ref(env, seen, t))
                };
                match t {
                    Type::ByRef(..) => true,
                    Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                        any(&mut ts.iter())
                    }
                    Type::Array(e) | Type::List(e) | Type::Error(e) => {
                        any(&mut std::iter::once(&**e))
                    }
                    Type::Struct(fs) => any(&mut fs.iter().map(|(_, t, _)| t)),
                    Type::Map { key, value } => any(&mut [&**key, &**value].into_iter()),
                    _ => false,
                }
            },
        )
    })
}

/// Whether `v` is the variant `t` names in its own form: the tag of a
/// bare variant, or an array of a payload variant's tag and payload.
fn carries_tag(env: &Env, t: &Type, v: &Value) -> bool {
    let Some(head) = rep_head(env, t) else { return false };
    let Type::Variant(tag, args, _) = &*head else { return false };
    match v {
        Value::String(s) => args.is_empty() && s == tag,
        Value::Array(a) => {
            !args.is_empty()
                && a.len() == args.len() + 1
                && matches!(&a[0], Value::String(s) if s == tag)
        }
        _ => false,
    }
}

/// `v` as the primitive `t`. An integer target holds the value exactly
/// or refuses it, whatever the source: a number, a float (its fraction
/// dropped) or a string; every other conversion is netidx's.
/// `v` as the primitive `t`, exactly or not at all. Nothing converts to
/// null or to an error, and an array or a map converts only to bytes (an
/// array of numbers, as json carries them), a map or an array, or its
/// printed string.
fn convert(v: Value, t: Typ) -> Option<Value> {
    match (&v, t) {
        (_, Typ::Null | Typ::Error) => return None,
        (Value::Array(a), Typ::Bytes) => {
            let mut bytes: LPooled<Vec<u8>> = LPooled::take();
            for el in a.iter() {
                match convert(el.clone(), Typ::U8)? {
                    Value::U8(b) => bytes.push(b),
                    _ => return None,
                }
            }
            return Some(Value::Bytes(bytes::Bytes::copy_from_slice(&bytes).into()));
        }
        (Value::Array(_) | Value::Map(_), Typ::Map | Typ::Array | Typ::String) => {
            return v.cast(t);
        }
        (Value::Array(_) | Value::Map(_), _) => return None,
        (Value::String(s), t) if t != Typ::String && parses_to_collection(s) => {
            return None;
        }
        _ => (),
    }
    if !is_integer(t) {
        return v.cast(t);
    }
    let n = match &v {
        Value::I8(n) => *n as i128,
        Value::I16(n) => *n as i128,
        Value::I32(n) | Value::Z32(n) => *n as i128,
        Value::I64(n) | Value::Z64(n) => *n as i128,
        Value::U8(n) => *n as i128,
        Value::U16(n) => *n as i128,
        Value::U32(n) | Value::V32(n) => *n as i128,
        Value::U64(n) | Value::V64(n) => *n as i128,
        Value::F32(f) => float_int(*f as f64)?,
        Value::F64(f) => float_int(*f)?,
        Value::String(s) => match s.trim().parse::<i128>() {
            Ok(n) => n,
            Err(_) => match s.parse::<Value>() {
                Ok(Value::String(_)) | Err(_) => return None,
                Ok(parsed) => return convert(parsed, t),
            },
        },
        Value::Bool(_) | Value::Decimal(_) | Value::DateTime(_) | Value::Duration(_) => {
            return v.cast(t);
        }
        _ => return None,
    };
    match t {
        Typ::I8 => i8::try_from(n).ok().map(Value::I8),
        Typ::I16 => i16::try_from(n).ok().map(Value::I16),
        Typ::I32 => i32::try_from(n).ok().map(Value::I32),
        Typ::Z32 => i32::try_from(n).ok().map(Value::Z32),
        Typ::I64 => i64::try_from(n).ok().map(Value::I64),
        Typ::Z64 => i64::try_from(n).ok().map(Value::Z64),
        Typ::U8 => u8::try_from(n).ok().map(Value::U8),
        Typ::U16 => u16::try_from(n).ok().map(Value::U16),
        Typ::U32 => u32::try_from(n).ok().map(Value::U32),
        Typ::V32 => u32::try_from(n).ok().map(Value::V32),
        Typ::U64 => u64::try_from(n).ok().map(Value::U64),
        Typ::V64 => u64::try_from(n).ok().map(Value::V64),
        _ => unreachable!("an integer type"),
    }
}

/// Whether `s` reads as an array or a map, which no primitive but a
/// string holds.
fn parses_to_collection(s: &str) -> bool {
    matches!(s.trim_start().as_bytes().first(), Some(b'[' | b'{'))
        && matches!(s.parse::<Value>(), Ok(Value::Array(_) | Value::Map(_)))
}

fn is_integer(t: Typ) -> bool {
    matches!(
        t,
        Typ::I8
            | Typ::I16
            | Typ::I32
            | Typ::Z32
            | Typ::I64
            | Typ::Z64
            | Typ::U8
            | Typ::U16
            | Typ::U32
            | Typ::V32
            | Typ::U64
            | Typ::V64
    )
}

/// A float's integer part, `None` for NaN or an infinity.
fn float_int(f: f64) -> Option<i128> {
    f.is_finite().then(|| f.trunc() as i128)
}

fn map_refs(env: &Env, t: &Type, v: &Value, f: &mut dyn FnMut(&Value) -> Value) -> Value {
    ensure_sufficient(|| {
        if !compared_ref(env, &mut Expanded::default(), t) {
            return v.clone();
        }
        let Some(t) = rep_head(env, t) else { return v.clone() };
        let t = &*t;
        let mut each = |ts: &mut dyn Iterator<Item = (&Type, &Value)>| {
            let mut vs: LPooled<Vec<Value>> =
                Iterator::map(&mut &mut *ts, |(t, v)| map_refs(env, t, v, f)).collect();
            Value::Array(ValArray::from_iter_exact(vs.drain(..)))
        };
        match (t, v) {
            (Type::ByRef(..), v) => f(v),
            // CR claude for claude: [perf/low] is_a here checks the member against the
            // whole remaining value, and map_refs then recurses one level down and does
            // it again. Comparing two n-deep values of `type L = [`Cons(&i64, L),
            // `Nil]` is therefore O(n^2): 3.6s at n=4000 against 0.5s to build them
            // (probe: design/review-2026-10-12/repro/x-stack-map-refs-quadratic.gx).
            // tval's member_of already chooses a union member cheaply, using
            // shape_excludes before falling back to is_a. Share it here and in
            // graphix-package-core's shape(), which has the same per-level is_a. Also
            // stop allocating a fresh set for compared_ref at every level. (x-stack-05)
            // CR claude for claude: [perf/medium] To choose a member, this runs a full
            // is_a over the rest of the value. For a recursive type such as `[`Nil,
            // `Cons(&i64, L)]`, each list cell walks the whole tail, so == on two
            // 2000-element lists takes about 4 s and 4000 elements take tens of
            // seconds. map_refs also calls compared_ref with a freshly allocated
            // AHashSet at every value node. Choose the member by its shallow shape (tag
            // and arity, as tval::member_of does) and fall back to is_a only when
            // several members admit that shape. Decide compared_ref once per type, not
            // once per value node. probe:
            // design/review-2026-10-12/repro/t-cast-setops-01-map-refs-quadratic.gx
            // (t-cast-setops-12)
            (Type::Set(ts), v) => match ts.iter().find(|m| m.is_a(env, v)) {
                Some(m) => map_refs(env, m, v, f),
                None => v.clone(),
            },
            (Type::Error(e), Value::Error(x)) => {
                Value::Error(map_refs(env, e, x, f).into())
            }
            (Type::Array(e), Value::Array(x)) => each(&mut x.iter().map(|v| (&**e, v))),
            (Type::List(e), v) => {
                let vs: LPooled<Vec<Value>> =
                    list::Iter::new(v.clone()).map(|v| map_refs(env, e, &v, f)).collect();
                list::from_iter(vs.iter().cloned())
            }
            (Type::Tuple(ts), Value::Array(x)) if x.len() == ts.len() => {
                each(&mut ts.iter().zip(x.iter()))
            }
            (Type::Variant(tag, ts, _), Value::Array(x)) if x.len() == ts.len() + 1 => {
                let name =
                    Type::Variant(tag.clone(), Arc::from_iter([]), WrittenAt::NOWHERE);
                each(&mut std::iter::once(&name).chain(ts.iter()).zip(x.iter()))
            }
            (Type::Struct(fs), Value::Array(x)) if x.len() == fs.len() => {
                let mut vs: LPooled<Vec<Value>> = fs
                    .iter()
                    .zip(x.iter())
                    .map(|((_, t, _), field)| match field {
                        Value::Array(nv) if nv.len() == 2 => {
                            Value::Array(ValArray::from_iter_exact(
                                [nv[0].clone(), map_refs(env, t, &nv[1], f)].into_iter(),
                            ))
                        }
                        v => v.clone(),
                    })
                    .collect();
                Value::Array(ValArray::from_iter_exact(vs.drain(..)))
            }
            (Type::Map { value, .. }, Value::Map(m)) => Value::Map(
                m.into_iter()
                    .map(|(k, v)| (k.clone(), map_refs(env, value, v, f)))
                    .collect(),
            ),
            (_, v) => v.clone(),
        }
    })
}

impl Type {
    /// Whether comparing values of this type meets a reference: `==`
    /// compares such values by [`Self::map_refs`].
    pub fn compares_refs(&self, env: &Env) -> bool {
        compared_ref(env, &mut Expanded::default(), self)
    }

    /// `v`, a value of this type, with each reference a comparison meets
    /// replaced by `f` of it.
    pub fn map_refs(
        &self,
        env: &Env,
        v: &Value,
        f: &mut dyn FnMut(&Value) -> Value,
    ) -> Value {
        map_refs(env, self, v, f)
    }
}
