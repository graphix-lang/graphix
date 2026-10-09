use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG, PrintFlag,
    env::Env,
    errf,
    expr::WrittenAt,
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

/// The static source type at a cast position, with names, cells and
/// applications looked through; `None` when unknown. A union decides
/// only the kind of collection its array-shaped members all are (a list
/// and an array share `Value::Array`): the member when it is one, else
/// that kind over `Any`; a union holding both cannot be told apart.
fn src_head(src: Option<&Type>, env: &Env) -> Option<Type> {
    let mut cur = src?.clone();
    loop {
        cur = match &cur {
            Type::TVar(_) => cur.deref_cloned()?,
            Type::App(c, a) => Type::app_filled(c, a)?,
            Type::Ref(_) => cur.lookup_ref(env).ok()?,
            Type::Set(ms) => {
                let heads: SmallVec<[Type; 4]> = ms
                    .iter()
                    .filter_map(|m| src_head(Some(m), env))
                    .filter(|h| matches!(h, Type::Array(_) | Type::List(_)))
                    .collect();
                return match &heads[..] {
                    [one] => Some(one.clone()),
                    [first, rest @ ..] => {
                        let list = matches!(first, Type::List(_));
                        rest.iter().all(|h| matches!(h, Type::List(_)) == list).then(
                            || match list {
                                true => Type::List(Arc::new(Type::Any)),
                                false => Type::Array(Arc::new(Type::Any)),
                            },
                        )
                    }
                    [] => None,
                };
            }
            Type::Any | Type::Bottom | Type::Hole => return None,
            _ => return Some(cur),
        };
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
        hist: &mut AHashSet<(RefKey, usize)>,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        ensure_sufficient(|| self.cast_inner(env, hist, src, v))
    }

    fn cast_fail(&self, why: &'static str, v: &Value) -> CastFail {
        CastFail { why, to: self.clone(), v: v.clone() }
    }

    fn cast_inner(
        &self,
        env: &Env,
        hist: &mut AHashSet<(RefKey, usize)>,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.cast_int(env, hist, src, v),
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
                // CR claude for eric: [bug] Every type-directed read (str::parse,
                // json/toml/pack::read, sys::net subscribe/call, sqlite::query) and
                // every cast from a string reaches this line, and netidx's Value::cast
                // parses a string as an i64 literal and converts with Rust `as`, so
                // out-of-range input silently becomes another number:
                // str::parse("70000") into u16 is 4464, json::read("300") into u8 is
                // 44, cast<u32>("4294967296") is 0, yet
                // cast<u64>("18446744073709551615") is refused. The reads promise an
                // error when the value does not fit (str.gxi "an error on failure",
                // sys::net "InvalidCast if the conversion fails"), netidx's own parser
                // refuses "u16:70000", and netidx-admin's parse_id
                // (tui/services.gx:146) relies on cast<u32> refusing bad input, so a
                // typed uid 4294967296 becomes uid 0. A union target takes the first
                // member in Typ order that converts, so cast<[u8, i64]>("300") is u8:44
                // although i64 holds 300. Number-to-number cast<T> saturation is pinned
                // (types::cast_narrow_saturates), so an exact conversion for reads and
                // strings needs a ruling on that pin, and str::parse's signature must
                // then admit the InvalidCast it can already return. probe:
                // design/review-2026-10-05/repro/gx-stdlib-02.gx (gx-stdlib-02)
                // netidx casts every error as false: an error is no data
                if let Value::Error(_) = v {
                    return Err(self.cast_fail("an error is not data", v));
                }
                match s.iter().find_map(|t| v.clone().cast(t)) {
                    Some(v) => Ok(Some(v)),
                    None => Err(self.cast_fail("no primitive conversion", v)),
                }
            }
            Type::TVar(tv) => match tv.binding() {
                Some(t) => t.cast_int(env, hist, src, v),
                None => Ok(None),
            },
            Type::Error(e) => {
                let src = src_head(src, env);
                let src = match &src {
                    Some(Type::Error(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    Value::Error(inner) => Ok(e
                        .cast_int(env, hist, src, inner)?
                        .map(|c| Value::Error(c.into()))),
                    v => {
                        let c = e.cast_int(env, hist, src, v)?;
                        Ok(Some(Value::Error(c.unwrap_or_else(|| v.clone()).into())))
                    }
                }
            }
            // An array and a list share `Value::Array`: the source's
            // static type says which one the value is, the shape only
            // when the source is unknown.
            Type::Array(et) => {
                let src = src_head(src, env);
                let from_list = matches!(&src, Some(Type::List(_)));
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    _ if from_list => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in list::Iter::new(v.clone()) {
                            let c = et.cast_int(env, hist, src, &el)?;
                            out.push(c.unwrap_or(el));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    Value::Array(elts) => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |_, el| et.cast_int(env, hist, src, el),
                    )?
                    .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..))))),
                    v => {
                        let c = et.cast_int(env, hist, src, v)?;
                        Ok(Some(Value::Array([c.unwrap_or_else(|| v.clone())].into())))
                    }
                }
            }
            Type::List(et) => {
                let src = src_head(src, env);
                let spine = match &src {
                    Some(Type::List(_)) => true,
                    Some(Type::Array(_)) => false,
                    // CR claude for eric: [bug] With no single static source type, a
                    // 2-element array whose last element is list-shaped is taken for a
                    // list spine. That covers json/pack/toml/str::parse/netidx reads
                    // and an Any or union source such as [Array<Array<i64>>, null]. So
                    // [[1, 2], []] cast to List<Array<i64>> silently becomes [<[1, 2]>]
                    // and the empty row is lost. The nullable source breaks the rule
                    // pinned by types::cast_array_to_list (the source type, not the
                    // shape, picks the conversion), because src_head gives up on a
                    // union whose only collection member is an Array. The read cases
                    // cannot be fixed here: a list serializes as its raw rep, so
                    // json::write_str([<[1, 2]>]) and json::write_str of the array [[1,
                    // 2], []] are the same document. probe:
                    // design/review-2026-10-05/repro/t-cast-setops-08.gx
                    // (t-cast-setops-08)
                    _ => list::len(v).is_some(),
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
                            match et.cast_int(env, hist, src, &el)? {
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
                            let c = et.cast_int(env, hist, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(list::from_iter(out.drain(..))))
                    }
                    v => {
                        let c = et.cast_int(env, hist, src, v)?;
                        Ok(Some(list::from_iter([c.unwrap_or_else(|| v.clone())])))
                    }
                }
            }
            Type::Map { key, value } => {
                let src = src_head(src, env);
                let (ks, vs) = match &src {
                    Some(Type::Map { key, value }) => (Some(&**key), Some(&**value)),
                    _ => (None, None),
                };
                let mut entry = |k: &Value,
                                 v: &Value|
                 -> std::result::Result<_, CastFail> {
                    let ck = key.cast_int(env, hist, ks, k)?;
                    let cv = value.cast_int(env, hist, vs, v)?;
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
                let src = src_head(src, env);
                let ss = match &src {
                    Some(Type::Tuple(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                match v {
                    Value::Array(elts) if elts.len() == ts.len() => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |i, el| ts[i].cast_int(env, hist, ss.map(|ss| &ss[i]), el),
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
                // a field the data omits reads as null where null casts to its
                // type (an optional field, a key a document leaves out)
                if fields.len() < ts.len() {
                    static NULL: Value = Value::Null;
                    let mut filled: SmallVec<[(&ArcStr, &Value); 8]> = SmallVec::new();
                    let mut have = fields.iter().copied().peekable();
                    for (fname, ftyp, _) in ts.iter() {
                        match have.peek() {
                            Some((n, _)) if *n == fname => filled.extend(have.next()),
                            _ if ftyp.cast_int(env, hist, None, &NULL).is_ok() => {
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
                let src = src_head(src, env);
                let ss = match &src {
                    Some(Type::Struct(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                let cast = cast_elts(
                    &fields[..],
                    |(_, fv)| *fv,
                    |i, fv| ts[i].1.cast_int(env, hist, ss.map(|ss| &ss[i].1), fv),
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
                let src = src_head(src, env);
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
                            |i, el| ts[i].cast_int(env, hist, payload(i), el),
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
                            let c = ts[i].cast_int(env, hist, payload(i), el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    Value::Array(_) => Err(self.cast_fail("variant length mismatch", v)),
                    _ => Err(self.cast_fail("not a variant", v)),
                }
            }
            // `hist` is the current path, not a visited set: a name met
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
                let grows = hist.iter().any(|((d, ps), at)| {
                    *d == key.0.0 && *at == key.1 && params_size(ps) < size
                });
                if grows || !hist.insert(key.clone()) {
                    return Err(
                        self.cast_fail("the type recurses without consuming it", v)
                    );
                }
                let r = t.cast_int(env, hist, src, v);
                hist.remove(&key);
                r
            }
            // A member the value already inhabits wins; else the first
            // member it converts to.
            // CR claude for eric: [bug] The first member a value converts to wins, and
            // a payload variant also converts an array of exactly its payload's length
            // as a tagless payload (555-563). So a value carrying one member's tag is
            // claimed by any member that sorts before it: cast<[`Abc(string, i64),
            // `Zed(i64)]> of ["Zed", "5"] or ["Zed", i32:5] gives the variant
            // Abc("Zed", 5), while with the tag order mirrored the tagged member wins.
            // The type-directed reads (json, toml, pack, sqlite, str::parse, sys::net)
            // cast external data this way. A member whose tag the value carries should
            // beat every tagless reading. probe:
            // design/review-2026-10-05/repro/t-cast-setops-14.gx is the coverage one;
            // this one is design/review-2026-10-05/repro/t-cast-setops-13.gx
            // (t-cast-setops-13)
            Type::Set(ts) => {
                let mut converted = None;
                for t in ts.iter() {
                    match t.cast_int(env, hist, src, v) {
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
        self.cast_value_int(env, None, v)
    }

    /// [`Self::cast_value`] of a `v` statically of type `src`, which
    /// decides whether a value is an array or a list.
    pub fn cast_from(&self, env: &Env, src: &Type, v: Value) -> Value {
        self.cast_value_int(env, Some(src), v)
    }

    fn cast_value_int(&self, env: &Env, src: Option<&Type>, v: Value) -> Value {
        match self.cast_int(env, &mut LPooled::take(), src, &v) {
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
                                && params.iter().zip(g.params.iter()).all(|(p, gp)| {
                                    p.contains_with_flags(BitFlags::empty(), env, gp)
                                        .unwrap_or(false)
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
            // XCR claude for eric: [bug] A reference type test matches any u64 or v64
            // and never sees the referent, yet the select narrows the arm's bind to
            // `&T` and subtracts `&T` from the later arms (setops.rs:574;
            // pattern.rs:1215 admits the predicate). Over `r: [&string, &i64]`,
            // `&string as s` takes a reference to an i64: `*s` hands an i64 to string
            // code (the JIT panics at kernel.rs:243 and the runtime dies; the node-walk
            // loses the value), and `*s <- "x"` writes a string into an i64 variable. A
            // `u64 as n` arm before a reference arm reads the session's bind id as a
            // number (cold and warm images differ), and the reverse turns a u64 into a
            // reference to any variable. A reference type test can only answer "is a
            // reference": refuse a predicate that would narrow a referent, and a
            // scrutinee that mixes references with u64/v64. probe:
            // design/review-2026-10-05/repro/x-typecheck-patterns-01.gx
            // (x-typecheck-patterns-01)
            // 2026-10-06 claude: refused now by the one-runtime-form rule
            // (Type::rep_collision, checked per arm in Select::typecheck0_with): a
            // reference collides with u64/v64 and with a reference to another type, so
            // `&string as s` over [&string, &i64] and `u64 as n` over [&i64, u64] are
            // refused. A reference test that can't be mistaken, like `&T as r` over [&T,
            // null], stays legal. Pinned by lang::select::same_form_references_refused
            // and must-reject family 10.
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

/// Pairs already on the walk; a finite witness never revisits one.
type RepSeen = AHashSet<(Type, Type)>;

fn revisits(seen: &mut RepSeen, a: &Type, b: &Type) -> bool {
    (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_)))
        && !seen.insert((a.clone(), b.clone()))
}

fn rep_overlaps(env: &Env, seen: &mut RepSeen, a: &Type, b: &Type) -> bool {
    ensure_sufficient(|| {
        if revisits(seen, a, b) {
            return false;
        }
        let (Some(a), Some(b)) = (rep_head(env, a), rep_head(env, b)) else {
            return true;
        };
        let (a, b) = (&*a, &*b);
        match (a, b) {
            (Type::Bottom, _) | (_, Type::Bottom) => false,
            (Type::Set(ts), _) => ts.iter().any(|t| rep_overlaps(env, seen, t, &b)),
            (_, Type::Set(ts)) => ts.iter().any(|t| rep_overlaps(env, seen, &a, t)),
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
            (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. }) => x == y,
            (a, b) => match (ArrView::of(a), ArrView::of(b)) {
                (Some(x), Some(y)) => arr_overlaps(env, seen, x, y),
                _ => false,
            },
        }
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
        if revisits(seen, a, b) {
            return None;
        }
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
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                pairs(&mut [(&**k0, &**k1), (&**v0, &**v1)].into_iter())
            }
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
                    && px.iter().zip(py.iter()).all(|(p, q)| union_identical(p, q));
                (rust_backed && !same).then(|| (a.clone(), b.clone()))
            }
            (Type::ByRef(_, x), Type::ByRef(_, y)) if x == y => None,
            (Type::Fn(x), Type::Fn(y)) if x == y => None,
            (a, b) => rep_overlaps(env, &mut RepSeen::default(), a, b)
                .then(|| (a.clone(), b.clone())),
        }
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
    /// A reference, under `Ordered`.
    Ref(Type),
}

fn rep_ambiguity(
    env: &Env,
    seen: &mut AHashSet<Type>,
    open: Open,
    keys_only: bool,
    refs: bool,
    t: &Type,
) -> Option<Indiscernible> {
    ensure_sufficient(|| {
        if matches!(t, Type::Ref(_)) && !seen.insert(t.clone()) {
            return None;
        }
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
                            let c =
                                rep_collision(env, &mut RepSeen::default(), open, a, b);
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
                        &mut AHashSet::default(),
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
            Type::ByRef(..) if refs && !keys_only => Some(Indiscernible::Ref(t.clone())),
            _ => None,
        }
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
        rep_ambiguity(env, &mut AHashSet::default(), open, false, ordered, self)
    }

    /// Why a map key type this type holds is not `Ordered`.
    pub fn map_key_failure(&self, env: &Env, open: Open) -> Option<Indiscernible> {
        rep_ambiguity(env, &mut AHashSet::default(), open, true, false, self)
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
                Indiscernible::Ref(r) => {
                    let r = r.resolve_tvars();
                    let holds = match r == t {
                        true => format_compact!("is a reference"),
                        false => format_compact!("holds the reference {r}"),
                    };
                    anyhow::anyhow!(
                        "{what} {holds}: references have no order, and only == and != \
                         compare them, by what they point to"
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
fn compared_ref(env: &Env, seen: &mut AHashSet<Type>, t: &Type) -> bool {
    ensure_sufficient(|| {
        if matches!(t, Type::Ref(_)) && !seen.insert(t.clone()) {
            return false;
        }
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
    })
}

fn map_refs(env: &Env, t: &Type, v: &Value, f: &mut dyn FnMut(&Value) -> Value) -> Value {
    ensure_sufficient(|| {
        if !compared_ref(env, &mut AHashSet::default(), t) {
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
        compared_ref(env, &mut AHashSet::default(), self)
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
