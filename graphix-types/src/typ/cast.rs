use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG, PrintFlag,
    abstract_value::{self, CastHook},
    env::Env,
    errf,
    expr::ModPath,
    format_with_flags, list, rep,
    stack::ensure_sufficient,
    typ::{
        AbstractId, TVar, Type, TypeRef, params_size,
        setops::union_identical,
        tval::{NakedPrefix, has_form, member_of},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::{BitFlags, bitflags, make_bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::{NakedValue, Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{borrow::Cow, fmt, mem, ops::ControlFlow};
use triomphe::Arc;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum IsAFlags {
    /// A `Type::Abstract` test also accepts a Rust-backed abstract
    /// value whose wrapper UUID is not the type's path-derived one
    /// (packages registering ad-hoc UUIDs), and a Graphix-minted box
    /// answers by its tag alone, its payload pattern testing the rest.
    /// An explicit `T as t` is strict.
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
    why: Cow<'static, str>,
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

/// A cast in progress: the refs expanded on the way to each value.
struct CastCx {
    hist: LPooled<AHashSet<(RefKey, usize)>>,
    /// The params sizes on the path of each (definition, value address).
    sizes: LPooled<AHashMap<(usize, usize), SmallVec<[usize; 2]>>>,
    /// Each reference's expansion, by definition and params allocation,
    /// with the params held so the allocation stays theirs.
    expanded: LPooled<AHashMap<(usize, usize), (Arc<[Type]>, Type)>>,
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
/// depth in the walk, what was decided, or a verdict that holds if the
/// application being decided at depth `read` turns out `cycle` too.
enum Verdict<T> {
    Deciding(usize),
    Decided(T),
    Provisional { read: usize, t: T },
}

struct Verdicts<T> {
    known: LPooled<AHashMap<ShapeKey, Verdict<T>>>,
    /// The provisional verdicts, each with the depth it was decided at.
    pending: LPooled<Vec<(ShapeKey, usize)>>,
    /// The number of applications being decided.
    depth: usize,
    /// The shallowest application being decided that the decision in
    /// progress read.
    read: usize,
}

impl<T> Verdicts<T> {
    fn new() -> Self {
        Self {
            known: LPooled::take(),
            pending: LPooled::take(),
            depth: 0,
            read: usize::MAX,
        }
    }

    /// Settle the provisional verdicts that read the application decided
    /// at `depth` as `t`: refuted, every one decided inside it may have
    /// assumed it and is dropped; confirmed outright, those that read it
    /// first are decided; confirmed provisionally, they read what it read.
    fn settle(&mut self, depth: usize, refuted: bool, read: usize) {
        let mut i = 0;
        while i < self.pending.len() {
            let (k, at) = &self.pending[i];
            let Some(Verdict::Provisional { read: r, .. }) = self.known.get_mut(k) else {
                self.pending.swap_remove(i);
                continue;
            };
            if refuted && *at > depth {
                self.known.remove(k);
                self.pending.swap_remove(i);
            } else if *r < depth || refuted {
                i += 1;
            } else if read < depth {
                *r = read;
                i += 1;
            } else {
                let (k, _) = self.pending.swap_remove(i);
                if let Some(v) = self.known.get_mut(&k)
                    && let Verdict::Provisional { t, .. } =
                        mem::replace(v, Verdict::Deciding(0))
                {
                    *v = Verdict::Decided(t)
                }
            }
        }
    }
}

/// `decide` the application `k` once per walk. A repeat while it is being
/// decided answers `cycle`, an assumption that holds coinductively for
/// `k` itself; a decision that read the assumption of an application
/// further out answers `cycle` provisionally, since that application may
/// yet fail it, and is settled when that application is.
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
        Some(Verdict::Provisional { read, t }) => {
            seen.read = seen.read.min(*read);
            t.clone()
        }
        Some(Verdict::Decided(t)) => t.clone(),
        None => {
            let depth = seen.depth;
            seen.depth += 1;
            seen.known.insert(k.clone(), Verdict::Deciding(depth));
            let outer = mem::replace(&mut seen.read, usize::MAX);
            let t = decide(seen);
            seen.depth -= 1;
            let refuted = t != cycle;
            seen.settle(depth, refuted, seen.read);
            if seen.read < depth && !refuted {
                seen.known.insert(
                    k.clone(),
                    Verdict::Provisional { read: seen.read, t: t.clone() },
                );
                seen.pending.push((k, depth));
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
            Type::Abstract { params, .. } => {
                params.iter().try_for_each(|p| p.check_cast_int(env, seen))
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

    /// Whether a map type is written in this type, through the
    /// definitions it names (an abstract's hidden one excepted).
    pub fn holds_map(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Map { .. } => Some(true),
            Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a reference, outside a
    /// function or an abstract type, which a cast never takes apart. A
    /// cast refuses such a source: a reference names a cell, it is no
    /// data to read.
    #[doc(hidden)]
    pub fn holds_ref(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) => Some(true),
            Type::Fn(_) | Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    /// Whether a reference this type test holds sits where `reaching`
    /// admits `Any`: there the test would take a reference to anything
    /// for one to its type.
    pub fn ref_over_any(&self, env: &Env, reaching: &Type) -> bool {
        fn go(env: &Env, seen: &mut RepSeen, p: &Type, r: &Type) -> bool {
            ensure_sufficient(|| {
                let (Some(p), Some(r)) = (p.deref_cloned(), r.deref_cloned()) else {
                    return false;
                };
                walk(
                    seen,
                    pair_at(&p, &r),
                    false,
                    || true,
                    |seen| {
                        let expand = |t: &Type| match t {
                            Type::Ref(_) => t.lookup_ref(env).ok(),
                            t => Some(t.clone()),
                        };
                        let (Some(p), Some(r)) = (expand(&p), expand(&r)) else {
                            return false;
                        };
                        match (&p, &r) {
                            (_, Type::Any) => p.holds_ref(env),
                            (Type::Set(ps), _) => ps.iter().any(|p| go(env, seen, p, &r)),
                            (_, Type::Set(rs)) => rs.iter().any(|r| go(env, seen, &p, r)),
                            (Type::Tuple(ps), Type::Tuple(rs))
                                if ps.len() == rs.len() =>
                            {
                                ps.iter().zip(rs.iter()).any(|(p, r)| go(env, seen, p, r))
                            }
                            (Type::Array(p), Type::Array(r))
                            | (Type::List(p), Type::List(r))
                            | (Type::Error(p), Type::Error(r))
                            | (Type::ByRef(_, p), Type::ByRef(_, r)) => {
                                go(env, seen, p, r)
                            }
                            (Type::Struct(ps), Type::Struct(rs)) => {
                                ps.iter().any(|(n, p, _)| {
                                    rs.iter()
                                        .any(|(m, r, _)| n == m && go(env, seen, p, r))
                                })
                            }
                            (Type::Variant(pt, ps, _), Type::Variant(rt, rs, _))
                                if pt == rt && ps.len() == rs.len() =>
                            {
                                ps.iter().zip(rs.iter()).any(|(p, r)| go(env, seen, p, r))
                            }
                            (
                                Type::Map { key: pk, value: pv },
                                Type::Map { key: rk, value: rv },
                            ) => go(env, seen, pk, rk) || go(env, seen, pv, rv),
                            _ => false,
                        }
                    },
                )
            })
        }
        self.holds_ref(env) && go(env, &mut RepSeen::default(), self, reaching)
    }

    /// Whether the type holds `Any` or an open cell anywhere: a part a
    /// value-level predicate cannot be decided of.
    #[doc(hidden)]
    pub fn holds_unknown(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Any => Some(true),
            Type::TVar(tv) if tv.binding().is_none() => Some(true),
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

    /// Whether a value of this type can hold an abstract value that
    /// crosses no data: a Rust-backed one, or a Graphix one with no `Cast`
    /// implementation. No cast or read reaches one and no writer writes
    /// it.
    pub fn holds_uncastable(&self, env: &Env) -> bool {
        self.uncastable(env).is_some()
    }

    /// Whether `Data ⊇ self` holds as the type stands: no reference, no
    /// function and no abstract type [`Self::holds_uncastable`] refuses;
    /// open cells hold (a bind hands them the conjunct).
    pub(crate) fn data_holds(&self, env: &Env) -> bool {
        !self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) | Type::Fn(_) => Some(true),
            Type::Abstract { id, .. } if !env.casts(*id) => Some(true),
            _ => None,
        })
    }

    /// The first abstract type [`Self::holds_uncastable`] finds.
    pub fn uncastable(&self, env: &Env) -> Option<Type> {
        self.uncastable_but(env, &|_| false)
    }

    /// Every abstract type this type names, through its definitions.
    pub(crate) fn abstract_ids(&self, env: &Env, out: &mut Vec<AbstractId>) {
        let out = std::cell::RefCell::new(out);
        self.holds(env, &mut Verdicts::new(), &|t| {
            if let Type::Abstract { id, .. } = t {
                out.borrow_mut().push(*id);
            }
            None
        });
    }

    /// [`Self::uncastable`], passing over the abstract types `skip` says.
    pub(crate) fn uncastable_but(
        &self,
        env: &Env,
        skip: &dyn Fn(AbstractId) -> bool,
    ) -> Option<Type> {
        let found = std::cell::RefCell::new(None);
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Abstract { id, .. } if !env.casts(*id) && !skip(*id) => {
                *found.borrow_mut() = Some(t.clone());
                Some(true)
            }
            _ => None,
        });
        found.into_inner()
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

    fn cast_fail(&self, why: impl Into<Cow<'static, str>>, v: &Value) -> CastFail {
        CastFail { why: why.into(), to: self.clone(), v: v.clone() }
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
            | Type::Data
            | Type::Function
            | Type::Singleton
            | Type::OneNumber
            | Type::NoError
            | Type::Ordered
            | Type::Discernible => Err(self.cast_fail("a constraint", v)),
            Type::Bottom | Type::Any => Ok(None),
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => Ok(None),
                _ => Err(self.cast_fail("not a function", v)),
            },
            Type::Abstract { id, params } => {
                let Some(rep) = env.abstract_reps.get(id) else {
                    // a Rust-backed abstract is Rust's: only its own values
                    return match self.is_a(env, v) {
                        true => Ok(None),
                        false => Err(self.cast_fail("not this abstract type", v)),
                    };
                };
                let held = abstract_value::get(v).filter(|g| g.id == *id);
                if held.is_some_and(|g| g.params == *params) {
                    return Ok(None);
                }
                match abstract_value::from_value(self, v) {
                    None => Err(self.cast_fail("it has no Cast implementation", v)),
                    Some(CastHook::Ran(None)) => {
                        Err(self.cast_fail("its Cast implementation produced nothing", v))
                    }
                    Some(CastHook::Ran(Some(Value::Error(e)))) => {
                        Err(self.cast_fail(refusal(&e), v))
                    }
                    Some(CastHook::Ran(Some(r))) => Ok(Some(r)),
                    Some(CastHook::Representation) => {
                        let data = held.map_or(v, |g| &g.payload);
                        let payload =
                            rep.instantiate_with(params).cast_int(env, cx, None, data)?;
                        Ok(Some(abstract_value::wrap(
                            *id,
                            rep.name.clone(),
                            params.clone(),
                            payload.unwrap_or_else(|| data.clone()),
                        )))
                    }
                }
            }
            Type::ByRef(..) => {
                Err(self.cast_fail("a reference can't be read from data", v))
            }
            Type::Primitive(s) => {
                // a tuple, struct, variant or list is no primitive; a bare
                // variant reads as a string by its tag
                if rep::kind(v).is_some() {
                    return match rep::variant(v) {
                        Some((tag, [])) if s.contains(Typ::String) => {
                            Ok(Some(Value::String(tag.clone())))
                        }
                        _ => Err(self.cast_fail("no primitive holds it", v)),
                    };
                }
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
                let own = own_class(v);
                match (*s & own)
                    .iter()
                    .chain((*s & !own).iter())
                    .find_map(|t| convert(v.clone(), t))
                {
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
            // a list reads as the array of its elements
            Type::Array(et) => {
                let src = src_head(src, env, v);
                if matches!(&src, Some(Type::List(_))) && !list::is_list(v) {
                    return Err(self.cast_fail("not a list", v));
                }
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    v if list::is_list(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in list::Iter::new(v.clone()) {
                            let c = et.cast_int(env, cx, src, &el)?;
                            out.push(c.unwrap_or(el));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    v if let Some(elts) = rep::array_elems(v) => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |_, el| et.cast_int(env, cx, src, el),
                    )?
                    .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..))))),
                    v => Err(self.cast_fail("not an array", v)),
                }
            }
            Type::List(et) => {
                let src = src_head(src, env, v);
                if matches!(&src, Some(Type::List(_))) && !list::is_list(v) {
                    return Err(self.cast_fail("not a list", v));
                }
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    v if list::is_list(v) => {
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
                    v if let Some(elts) = rep::array_elems(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in elts.iter() {
                            let c = et.cast_int(env, cx, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(list::from_iter(out.drain(..))))
                    }
                    v => Err(self.cast_fail("not a list", v)),
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
                        // CR claude for eric: [bug/low] Keys that convert to the same
                        // key collapse here, the last one winning, with no error.
                        // cast<Map<i64, string>>({"1" => "a", "01" => "b", "1.5" =>
                        // "c"}) is {1 => "b"}, and the array-of-pairs path below does
                        // the same. Every other lossy cast is an InvalidCast. Proposed:
                        // refuse when the rebuilt map has fewer entries than were cast.
                        // probe:
                        // design/review-2026-10-19/repro/t-cast-setops-map-key-merge.gx
                        // (t-cast-setops-09)
                        Ok(changed.then(|| Value::Map(Map::from_iter(out.drain(..)))))
                    }
                    v if let Some(a) = rep::array_elems(v) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        for p in a.iter() {
                            match rep::read_tuple(p).map(|p| p.elems()) {
                                Some(p @ [_, _]) => {
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
                match rep::read_tuple(v) {
                    Some(parts) if parts.elems().len() == ts.len() => {
                        let elts = parts.elems();
                        let cast = cast_elts(
                            elts,
                            |v| v,
                            |i, el| ts[i].cast_int(env, cx, ss.map(|ss| &ss[i]), el),
                        )?;
                        Ok(match (cast, parts.held()) {
                            (None, true) => None,
                            (None, false) => Some(rep::tuple(elts.iter().cloned())),
                            (Some(mut a), _) => Some(rep::tuple(a.drain(..))),
                        })
                    }
                    Some(_) => Err(self.cast_fail("tuple size mismatch", v)),
                    None => Err(self.cast_fail("not a tuple", v)),
                }
            }
            Type::Struct(ts) => {
                let Some(parts) = rep::read_struct(v) else {
                    return Err(self.cast_fail("not a struct", v));
                };
                let elts = parts.elems();
                if elts.len() > ts.len() {
                    return Err(self.cast_fail("struct size mismatch", v));
                }
                let mut fields: SmallVec<[(&ArcStr, &Value); 8]> =
                    match elts.iter().map(rep::field).collect::<Option<_>>() {
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
                static NULL: Value = Value::Null;
                let mut omitted: SmallVec<[(usize, Option<Value>); 2]> = SmallVec::new();
                if fields.len() < ts.len() {
                    let mut filled: SmallVec<[(&ArcStr, &Value); 8]> = SmallVec::new();
                    let mut have = fields.iter().copied().peekable();
                    for (i, (fname, ftyp, _)) in ts.iter().enumerate() {
                        match have.peek() {
                            Some((n, _)) if *n == fname => filled.extend(have.next()),
                            _ => match ftyp.cast_int(env, cx, None, &NULL) {
                                Ok(read) => {
                                    omitted.push((i, read));
                                    filled.push((fname, &NULL))
                                }
                                Err(_) => {
                                    return Err(self.cast_fail("struct size mismatch", v));
                                }
                            },
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
                    |i, fv| match omitted.iter_mut().find(|(j, _)| *j == i) {
                        Some((_, read)) => Ok(read.take()),
                        None => ts[i].1.cast_int(env, cx, ss.map(|ss| &ss[i].1), fv),
                    },
                )?;
                if sorted && cast.is_none() && parts.held() {
                    return Ok(None);
                }
                let mut out: LPooled<Vec<Value>> = LPooled::take();
                for (i, (n, fv)) in fields.iter().enumerate() {
                    let fv = match &cast {
                        Some(c) => c[i].clone(),
                        None => (*fv).clone(),
                    };
                    out.push(rep::make_field((*n).clone(), fv));
                }
                Ok(Some(rep::structure_of_fields(out.drain(..))))
            }
            Type::Variant(tag, ts, _) if ts.is_empty() => match rep::read_variant(v) {
                Some((t, parts)) if t == tag && parts.elems().is_empty() => {
                    Ok((!parts.held()).then(|| rep::make_variant(tag.clone(), [])))
                }
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
                let read = rep::read_variant(v);
                match (read, rep::array_elems(v)) {
                    (Some((t, parts)), _) if parts.elems().len() == ts.len() => {
                        if t != tag {
                            return Err(self.cast_fail("variant tag mismatch", v));
                        }
                        let elts = parts.elems();
                        let cast = cast_elts(
                            elts,
                            |v| v,
                            |i, el| ts[i].cast_int(env, cx, payload(i), el),
                        )?;
                        Ok(match (cast, parts.held()) {
                            (None, true) => None,
                            (None, false) => {
                                Some(rep::make_variant(tag.clone(), elts.iter().cloned()))
                            }
                            (Some(mut a), _) => {
                                Some(rep::make_variant(tag.clone(), a.drain(..)))
                            }
                        })
                    }
                    // a payload written without its tag, as json may carry it
                    (_, Some(elts)) if elts.len() == ts.len() => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for (i, el) in elts.iter().enumerate() {
                            let c = ts[i].cast_int(env, cx, payload(i), el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(rep::make_variant(tag.clone(), out.drain(..))))
                    }
                    (_, Some(_)) => Err(self.cast_fail("variant length mismatch", v)),
                    _ => Err(self.cast_fail("not a variant", v)),
                }
            }
            // `cx` is the current path, not a visited set: a name met
            // again on the same value expands without consuming it.
            Type::Ref(tr) => {
                let Some(key) = ref_key(tr).map(|k| (k, (v as *const Value).addr()))
                else {
                    return Err(self.cast_fail("undefined type", v));
                };
                let ek = (key.0.0, tr.params.as_ptr().addr());
                let t = match cx.expanded.get(&ek) {
                    Some((_, t)) => t.clone(),
                    None => match self.lookup_ref(env) {
                        Ok(t) => {
                            cx.expanded.insert(ek, (tr.params.clone(), t.clone()));
                            t
                        }
                        Err(_) => return Err(self.cast_fail("undefined type", v)),
                    },
                };
                // the same definition over the same value with larger params
                // grows at every level, the value never consumed
                let size = params_size(&key.0.1);
                let at = (key.0.0, key.1);
                let grows =
                    cx.sizes.get(&at).is_some_and(|ss| ss.iter().any(|s| *s < size));
                if grows || !cx.hist.insert(key.clone()) {
                    return Err(
                        self.cast_fail("the type recurses without consuming it", v)
                    );
                }
                cx.sizes.entry(at).or_default().push(size);
                let r = t.cast_int(env, cx, src, v);
                if let Some(ss) = cx.sizes.get_mut(&at) {
                    ss.pop();
                    if ss.is_empty() {
                        cx.sizes.remove(&at);
                    }
                }
                cx.hist.remove(&key);
                r
            }
            // A member the value already inhabits wins; else the first
            // member of its own runtime class it converts to; else the
            // first member it converts to across classes.
            // CR claude for eric: [bug/high] A union takes the first member that
            // converts, and integer() drops a fraction, so an integer member always
            // wins over a float member that holds the value exactly. cast<[i64,
            // f64]>("3.5") is i64 3 and cast<Number>("3.5") is i8 3. A json or toml
            // string field "3.5" read into [i64, f64] or Number loses the fraction the
            // same way. str::parse("3.5") into [i64, f64] gives f64 3.5, so a read and
            // a cast disagree. Proposed fix: prefer a member that holds the value
            // exactly, and fall back to the fraction-dropping conversion only when none
            // does. Also decide whether a fractional string converts to an integer at
            // all (parse sends it through f64). CAST_TO_AN_INTEGER_IS_EXACT pins only
            // integral inputs. probe:
            // design/review-2026-10-19/repro/tests-types-union-cast-drops-fraction.gx
            // (tests-types-03)
            Type::Set(ts) => {
                // a member whose tag the value carries is its reading,
                // whatever a tagless reading of an earlier member says
                let mut tagged = ts.iter().filter(|t| carries_tag(env, t, v)).peekable();
                if tagged.peek().is_some() {
                    let mut read = Err(None);
                    for t in tagged {
                        match t.cast_int(env, cx, src, v) {
                            Ok(None) => return Ok(None),
                            Ok(Some(c)) if read.is_err() => read = Ok(c),
                            Ok(Some(_)) => (),
                            Err(e) if read.is_err() => read = Err(Some(e)),
                            Err(_) => (),
                        }
                    }
                    return match read {
                        Ok(c) => Ok(Some(c)),
                        Err(e) => Err(e.expect("a tagged member was tried")),
                    };
                }
                // CR claude for eric: [bug/medium] A data composite never holds a
                // member as is: it is always rebuilt, so it never answers Ok(None). The
                // first member whose element casts succeed therefore wins, truncating
                // conversions included. json [1.5, 2.5] read as [(i64, i64), (f64,
                // f64)] is (1, 2), and ["A", 1.5, 2.5] read as [`A(i64, i64), `A(f64,
                // f64)] is `A(1, 2), while the scalar 1.5 read as [i64, f64] stays 1.5.
                // Needs a ruling on whether a member "holds" data it truncates. One
                // fix: before these passes, take the first member whose every leaf
                // holds its value unconverted (a strict is_a over the data form).
                // probe:
                // design/review-2026-10-19/repro/t-cast-setops-union-cast-converts-held.gx
                // (t-cast-setops-03)
                let own: LPooled<Vec<bool>> =
                    ts.iter().map(|t| in_own_class(env, t, v)).collect();
                let mut converted = None;
                for pass in [true, false] {
                    for (t, _) in ts.iter().zip(own.iter()).filter(|(_, o)| **o == pass) {
                        match t.cast_int(env, cx, src, v) {
                            Ok(None) => return Ok(None),
                            Ok(Some(c)) if converted.is_none() => converted = Some(c),
                            Ok(Some(_)) | Err(_) => (),
                        }
                    }
                    if let Some(c) = converted {
                        return Ok(Some(c));
                    }
                }
                Err(self.cast_fail("no member admits it", v))
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
        let mut cx = CastCx {
            hist: LPooled::take(),
            sizes: LPooled::take(),
            expanded: LPooled::take(),
        };
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
            | Type::Data
            | Type::Function
            | Type::Singleton
            | Type::Discernible
            | Type::Ordered
            | Type::OneNumber
            | Type::NoError => false,
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
            Type::Primitive(t) => rep::kind(v).is_none() && t.contains(Typ::get(&v)),
            Type::Abstract { id, params } => match v {
                Value::Abstract(a) => {
                    match a.downcast_ref::<crate::abstract_value::GxAbstract>() {
                        Some(g) => {
                            g.id == *id
                                && (flags.contains(IsAFlags::MatchAbstract)
                                    || g.params.len() == params.len()
                                    // parameters are invariant: each one equal
                                    && params.iter().zip(g.params.iter()).all(|(p, gp)| {
                                    let holds = |a: &Type, b: &Type| {
                                        a.contains_with_flags(BitFlags::empty(), env, b)
                                            .unwrap_or(false)
                                    };
                                    holds(p, gp) && holds(gp, p)
                                }))
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
                rep::array_elems(v).is_some()
            }
            Type::Array(et) => match rep::array_elems(v) {
                Some(a) => a.iter().all(|v| et.is_a_int(env, hist, flags, v)),
                None => false,
            },
            Type::List(et)
                if matches!(&**et, Type::Any) && !flags.contains(IsAFlags::Strict) =>
            {
                list::is_list(v)
            }
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
            Type::ByRef(..) => crate::reference::cell(v).is_some(),
            Type::Tuple(ts) => match rep::tuple_elems(v) {
                Some(elts) => {
                    elts.len() == ts.len()
                        && ts
                            .iter()
                            .zip(elts.iter())
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                _ => false,
            },
            Type::Struct(ts) => match rep::struct_fields(v) {
                Some(elts) => {
                    elts.len() == ts.len()
                        && ts.iter().zip(elts.iter()).all(|((n, t, _), v)| {
                            match rep::field(v) {
                                Some((key, v)) => {
                                    n == key && t.is_a_int(env, hist, flags, v)
                                }
                                None => false,
                            }
                        })
                }
                _ => false,
            },
            Type::Variant(tag, ts, _) => match rep::variant(v) {
                Some((t, payload)) => {
                    t == tag
                        && payload.len() == ts.len()
                        && ts
                            .iter()
                            .zip(payload)
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                None => false,
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
    /// (members not enumerable, two share a shape, a union below the top
    /// whose members share one, which a nested structure test cannot tell
    /// apart, or no payload). Sound only for inferred predicates, which
    /// typecheck unified against their member; explicit `x as T` stays
    /// strict.
    pub fn shallow_discriminant(&self, env: &Env, scrutinee: &Type) -> Option<Type> {
        let mut scrut: LPooled<Vec<Type>> = LPooled::take();
        let mut seen: LPooled<Vec<RefKey>> = LPooled::take();
        flatten_union_members(scrutinee, env, &mut scrut, &mut seen)?;
        let below = |m: &Type| {
            m.try_for_each_child(&mut |c| match shapes_collide(
                c,
                env,
                &mut LPooled::take(),
            ) {
                true => ControlFlow::Break(()),
                false => ControlFlow::Continue(()),
            })
            .is_break()
        };
        if scrut.iter().any(below) {
            return None;
        }
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

/// Whether `t` or a part of it is a union two of whose members share a
/// shape (`arr_overlap`). A typedef is expanded once, whatever its
/// parameters: a non-regular one's grow at every level, and what they
/// hold is walked at the first.
fn shapes_collide(t: &Type, env: &Env, seen: &mut LPooled<Vec<usize>>) -> bool {
    ensure_sufficient(|| match t {
        Type::Ref(tr) => {
            let Some(key) = tr.def_key() else { return false };
            let Ok(x) = t.lookup_ref(env) else { return false };
            if seen.contains(&key) {
                return false;
            }
            seen.push(key);
            let r = shapes_collide(&x, env, seen);
            seen.pop();
            r
        }
        Type::TVar(tv) => tv.binding().is_some_and(|b| shapes_collide(&b, env, seen)),
        Type::Set(ts) => {
            let shapes: LPooled<Vec<_>> = ts
                .iter()
                .filter_map(|m| m.with_deref(|m| m.and_then(|m| member_facts(m).arr)))
                .collect();
            let overlap = shapes
                .iter()
                .enumerate()
                .any(|(i, a)| shapes[i + 1..].iter().any(|b| arr_overlap(a, b)));
            overlap || ts.iter().any(|m| shapes_collide(m, env, seen))
        }
        t => t
            .try_for_each_child(&mut |c| match shapes_collide(c, env, seen) {
                true => ControlFlow::Break(()),
                false => ControlFlow::Continue(()),
            })
            .is_break(),
    })
}

/// A flattened union member's runtime footprint. Arrays, tuples,
/// structs, variants with a payload and lists all inhabit
/// `Value::Array`, each kind under its own marker; `arr` is the member's
/// constraint within that class. `exact`: the full `is_a` is already
/// O(1).
struct MemberFacts {
    arr: Option<(Form, ArrCon)>,
    map: bool,
    error: bool,
    exact: bool,
}

/// What a member's array is: its kind, and a variant's tag.
#[derive(Clone, PartialEq, Eq)]
enum Form {
    Array,
    Tuple,
    Struct,
    Variant(ArcStr),
    List,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ArrCon {
    Len(usize),
    AnyLen,
}

impl MemberFacts {
    const EXACT: Self = MemberFacts { arr: None, map: false, error: false, exact: true };

    fn arr(form: Form, con: ArrCon) -> Self {
        MemberFacts { arr: Some((form, con)), map: false, error: false, exact: false }
    }
}

fn member_facts(t: &Type) -> MemberFacts {
    match t {
        Type::Primitive(bits) => MemberFacts {
            arr: bits.contains(Typ::Array).then_some((Form::Array, ArrCon::AnyLen)),
            map: bits.contains(Typ::Map),
            error: bits.contains(Typ::Error),
            exact: true,
        },
        Type::Variant(_, ps, _) if ps.is_empty() => MemberFacts::EXACT,
        Type::Variant(tag, ps, _) => {
            MemberFacts::arr(Form::Variant(tag.clone()), ArrCon::Len(ps.len()))
        }
        Type::Tuple(ts) => MemberFacts::arr(Form::Tuple, ArrCon::Len(ts.len())),
        Type::Struct(fs) => MemberFacts::arr(Form::Struct, ArrCon::Len(fs.len())),
        Type::Array(_) => MemberFacts::arr(Form::Array, ArrCon::AnyLen),
        Type::List(_) => MemberFacts::arr(Form::List, ArrCon::AnyLen),
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
        | Type::Data
        | Type::Singleton
        | Type::OneNumber
        | Type::NoError
        | Type::Discernible
        | Type::Ordered
        | Type::Function => MemberFacts::EXACT,
    }
}

fn arr_overlap((pf, pcon): &(Form, ArrCon), (mf, mcon): &(Form, ArrCon)) -> bool {
    pf == mf
        && match (pcon, mcon) {
            (ArrCon::AnyLen, _) | (_, ArrCon::AnyLen) => true,
            (ArrCon::Len(a), ArrCon::Len(b)) => a == b,
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
        | Type::Data
        | Type::Function
        | Type::Singleton
        | Type::Discernible
        | Type::Ordered
        | Type::OneNumber
        | Type::NoError => None,
        t => Some(Cow::Borrowed(t)),
    })
}

/// The primitive tags a value of another type may share.
const SHARED_PRIMS: BitFlags<Typ> = make_bitflags!(Typ::{Map | Error | Array});

/// Whether a value of `t` may also be a value of a type `t` doesn't hold.
fn shares_form(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None | Some(Type::Bottom | Type::Abstract { .. }) => false,
        Some(Type::Variant(_, ps, _)) if ps.is_empty() => false,
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
    // a `NoError` cell is never an error, so it shares no error's form
    let shares = |cell: &super::TVar, t: &Type| {
        let bounds = cell.cell_constraints();
        shares_form(env, t)
            && !(bounds.contains(&Type::NoError) && error_only(env, t))
            && !bounds
                .iter()
                .any(|b| prims_only(env, b).is_some_and(|p| !holds_prims(env, t, p)))
    };
    let collides = match (open(a), open(b)) {
        (Some(x), Some(y)) => x.cell_addr() != y.cell_addr(),
        (Some(x), None) => shares(&x, b),
        (None, Some(y)) => shares(&y, a),
        (None, None) => false,
    };
    collides.then(|| (a.clone(), b.clone()))
}

/// The primitive tags of `t` when every value of it is a primitive
/// (`Number`, `Int`).
fn prims_only(env: &Env, t: &Type) -> Option<BitFlags<Typ>> {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        Some(Type::Primitive(p)) => Some(*p),
        Some(Type::Set(ts)) => ts
            .iter()
            .try_fold(BitFlags::empty(), |acc, t| prims_only(env, t).map(|p| acc | p)),
        _ => None,
    })
}

/// Whether a value of `t` may carry one of the primitive tags `p`.
fn holds_prims(env: &Env, t: &Type, p: BitFlags<Typ>) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None => true,
        Some(Type::Bottom) => false,
        Some(Type::Primitive(q)) => q.intersects(p),
        Some(Type::Set(ts)) => ts.iter().any(|t| holds_prims(env, t, p)),
        Some(h @ (Type::Map { .. } | Type::Error(_) | Type::Array(_))) => {
            rep_prims(h).intersects(p)
        }
        Some(_) => p.intersects(SHARED_PRIMS | Typ::Abstract),
    })
}

/// Whether every value of `t` is an error.
fn error_only(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        Some(Type::Error(_)) => true,
        Some(Type::Primitive(p)) => *p == BitFlags::from(Typ::Error),
        Some(Type::Set(ts)) => ts.iter().all(|t| error_only(env, t)),
        _ => false,
    })
}

/// The primitive tags a value of the (non-primitive) head `t` has; a
/// tuple, struct, variant or list has none, its marker sets it apart.
fn rep_prims(t: &Type) -> BitFlags<Typ> {
    match t {
        Type::Map { .. } => Typ::Map.into(),
        Type::Error(_) => Typ::Error.into(),
        Type::Array(_) => Typ::Array.into(),
        _ => BitFlags::empty(),
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

/// Whether two heads [`rep_collision`]'s own arms did not decide share a
/// runtime form: a primitive and a head whose form holds it, two
/// references, or two function types.
fn rep_overlaps(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Primitive(p), t) | (t, Type::Primitive(p)) => p.intersects(rep_prims(t)),
        (Type::ByRef(..), Type::ByRef(..)) | (Type::Fn(_), Type::Fn(_)) => true,
        _ => false,
    }
}

fn rep_collision(
    env: &Env,
    seen: &mut RepSeen,
    open: Open,
    a: &Type,
    b: &Type,
) -> Option<Indiscernible> {
    ensure_sufficient(|| {
        let grows = || match (a, b) {
            (Type::Ref(tr), _) | (_, Type::Ref(tr)) => {
                Some(Indiscernible::Grows(tr.name.clone()))
            }
            _ => unreachable!("only a reference expands"),
        };
        walk(seen, pair_at(a, b), None, grows, |seen| {
            let pairs =
                |seen: &mut RepSeen, xs: &mut dyn Iterator<Item = (&Type, &Type)>| {
                    xs.map(|(x, y)| rep_collision(env, seen, open, x, y))
                        .find(Option::is_some)
                        .flatten()
                };
            let (ha, hb) = (rep_head(env, a), rep_head(env, b));
            let (Some(ha), Some(hb)) = (ha.as_deref(), hb.as_deref()) else {
                // an open side meets each member of a union on the other
                return match (open, ha.as_deref(), hb.as_deref()) {
                    (Open::Benign, _, _) => None,
                    (Open::Unknown, _, Some(Type::Set(ts))) => {
                        pairs(seen, &mut ts.iter().map(|t| (a, t)))
                    }
                    (Open::Unknown, Some(Type::Set(ts)), _) => {
                        pairs(seen, &mut ts.iter().map(|t| (t, b)))
                    }
                    (Open::Unknown, _, _) => {
                        open_collision(env, a, b).map(|(a, b)| Indiscernible::Pair(a, b))
                    }
                };
            };
            let (a, b) = (ha, hb);
            let mut pairs =
                |xs: &mut dyn Iterator<Item = (&Type, &Type)>| pairs(seen, xs);
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
                    (rust_backed && !same)
                        .then(|| Indiscernible::Pair(a.clone(), b.clone()))
                }
                // a runtime test can't tell a reference's mutability
                (Type::ByRef(m0, x), Type::ByRef(m1, y)) if m0 == m1 && x == y => None,
                (Type::Fn(x), Type::Fn(y)) if x == y => None,
                (a, b) => {
                    rep_overlaps(a, b).then(|| Indiscernible::Pair(a.clone(), b.clone()))
                }
            }
        })
    })
}

impl Type {
    /// Two parts, one of `self` and one of `t`, that are different types
    /// with one runtime form: a value of either passes a runtime test for
    /// the other (a tuple, struct, list or payload variant and an array; a
    /// bare variant and a string; two references; two function types). A
    /// test that tells them apart can't tell such a value's type.
    pub fn rep_collision(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), Open::Benign, self, t)
    }

    /// [`Self::rep_collision`] with an open cell read as any type it may
    /// yet bind ([`Open::Unknown`]): what a definition must rule out.
    pub fn rep_collision_open(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), Open::Unknown, self, t)
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
    /// The refusal of `what`, a runtime test for `test` among `rest`,
    /// which a [`rep_collision`] found can't tell this apart.
    pub fn untold(&self, what: &str, test: &Type, rest: &Type) -> anyhow::Error {
        match self {
            Indiscernible::Pair(a, b) => anyhow::anyhow!(
                "{what} can't tell {} from {}: both have the same runtime form; wrap \
                 them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            Indiscernible::Grows(name) => anyhow::anyhow!(
                "{what} tests for {} among {}, which hold {name}, whose parameters grow \
                 as it recurses, so whether their runtime forms differ can't be decided",
                test.resolve_tvars(),
                rest.resolve_tvars()
            ),
            Indiscernible::Unordered(_) => unreachable!("a runtime test is no order"),
        }
    }

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
                                if c.is_some() {
                                    return c;
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
    pub(crate) fn require_compared(
        &self,
        bound: &Type,
        hand: &mut dyn FnMut(&TVar, Type),
    ) {
        ensure_sufficient(|| match self {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => b.require_compared(bound, hand),
                None => hand(tv, bound.clone()),
            },
            Type::ByRef(..) | Type::Fn(_) | Type::Abstract { .. } => (),
            t => t.for_each_child(&mut |c| c.require_compared(bound, hand)),
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

/// Why a `Cast` implementation refused: the message of its
/// `` `InvalidCast(why) ``, else the error as it prints.
fn refusal(e: &Value) -> Cow<'static, str> {
    match rep::variant(e) {
        Some((_, [Value::String(s)])) => Cow::Owned(s.to_string()),
        _ => Cow::Owned(format!("{}", NakedPrefix(e))),
    }
}

/// Whether `v` is the variant `t` names: one held, or a payload
/// variant's data, its tag and then its payload (a data string is a
/// string, never a tag here).
fn carries_tag(env: &Env, t: &Type, v: &Value) -> bool {
    let Some(head) = rep_head(env, t) else { return false };
    let Type::Variant(tag, args, _) = &*head else { return false };
    match rep::read_variant(v) {
        Some((t, rep::Parts::Held(p))) => t == tag && p.len() == args.len(),
        Some((t, rep::Parts::Data(p))) => {
            !args.is_empty() && t == tag && p.len() == args.len()
        }
        None => false,
    }
}

/// The primitives `v` reads as without leaving its runtime class: every
/// number for a number, its own primitive for anything else.
fn own_class(v: &Value) -> BitFlags<Typ> {
    let t = Typ::get(v);
    if t.is_number() { Typ::number() } else { t.into() }
}

/// Whether `t` may read `v` without leaving `v`'s runtime class, which a
/// union tries before any conversion across classes.
// CR claude for eric: [bug/medium] Kind markers made unions like [(i64, i64),
// Array<f64>], [{a: i64}, Array<(string, i64)>] and [string, `A] legal, but their
// members share a data form once rep::strip writes them. This test reads the outer form
// strictly, so an unmarked array is in the Array member's class only, and a string is
// in the string member's class only. A tuple written by pack or json therefore reads
// back as the array ((1, 2) -> [1, 2]), a struct as its pairs, and ("k", 1) of
// [(string, i64), Array<string>] as ["k", "1"]. This breaks CLAUDE.md's rule that what
// the writers emit reads back into its type. design/distinct_composite_forms.md calls
// the string case intended. Choose one: refuse such unions under Data/Concrete, or have
// the union read prefer the member that reads unmarked data exactly (tuple of that
// arity, then struct, then array). Then fix the doc that loses. probe:
// design/review-2026-10-19/repro/t-rep-union-data-roundtrip.gx (t-rep-01)
// 2026-10-20 claude: json::read reproduces it as well: {"a":1} read into [{a: i64},
// Array<(string, i64)>] selects the Array member and prints array [("a", 1)] (found by
// the stdlib fix agent, re-run on main 2026-10-20).
fn in_own_class(env: &Env, t: &Type, v: &Value) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None | Some(Type::Bottom) => true,
        Some(Type::Primitive(s)) => rep::kind(v).is_none() && s.intersects(own_class(v)),
        Some(Type::Set(ts)) => ts.iter().any(|t| in_own_class(env, t, v)),
        Some(Type::Array(_)) => rep::array_elems(v).is_some(),
        Some(Type::List(_)) => list::is_list(v),
        Some(Type::Tuple(_)) => rep::tuple_elems(v).is_some(),
        Some(Type::Struct(_)) => rep::struct_fields(v).is_some(),
        Some(Type::Variant(..)) => rep::variant(v).is_some(),
        Some(Type::Map { .. }) => matches!(v, Value::Map(_)),
        Some(Type::Error(_)) => matches!(v, Value::Error(_)),
        Some(_) => matches!(v, Value::Abstract(_)),
    })
}

/// `v` as the primitive `t`, exactly or not at all. A number, a bool (1 or
/// 0), a datetime (Unix seconds) and a duration (seconds) convert to a
/// number, a number
/// to a datetime or a duration (an integer target holds it exactly, a
/// fraction is dropped), a string to what it parses as, a number, bool,
/// datetime or duration to its plain printed form, and an array of
/// numbers to bytes. Nothing converts to null or to an error.
fn convert(v: Value, t: Typ) -> Option<Value> {
    match (&v, t) {
        (_, Typ::Null | Typ::Error) => None,
        (v, t) if Typ::get(v) == t => Some(v.clone()),
        (Value::Array(a), Typ::Bytes) => {
            let mut bytes: LPooled<Vec<u8>> = LPooled::take();
            for el in a.iter() {
                match convert(el.clone(), Typ::U8)? {
                    Value::U8(b) => bytes.push(b),
                    _ => return None,
                }
            }
            Some(Value::Bytes(bytes::Bytes::copy_from_slice(&bytes).into()))
        }
        // CR claude for eric: [doc-drift/low] The primitive `array` reads a map as its
        // [k, v] pairs here, but Type::Array refuses a map ("not an array"), while
        // Type::Map reads an array of pairs. CLAUDE.md's list of class-leaving casts
        // names none of these, nor list-to-array. cast<array>(m) works and
        // cast<Array<(string, i64)>>(m) fails. Needs a ruling: either a map reads as
        // its pairs for both targets (add the arm to Type::Array) or for neither (drop
        // Value::Map here). Then list the crossings in CLAUDE.md. probe:
        // design/review-2026-10-19/repro/t-cast-setops-map-array-class.gx
        // (t-cast-setops-11)
        (Value::Array(_) | Value::Map(_), Typ::Map | Typ::Array) => v.cast(t),
        (Value::String(s), t) => parse(s, t),
        (_, Typ::String) => printed(&v),
        (_, Typ::DateTime | Typ::Duration) if Typ::get(&v).is_number() => v.cast(t),
        (_, t) if t.is_integer() && counts(&v) => integer(&v, t),
        (_, t) if t.is_number() && counts(&v) => v.cast(t),
        _ => None,
    }
}

/// The string `s` read as the primitive `t`.
fn parse(s: &ArcStr, t: Typ) -> Option<Value> {
    if t == Typ::DateTime
        && let Ok(d) = chrono::DateTime::parse_from_rfc3339(s)
    {
        return Some(Value::DateTime(Arc::new(d.with_timezone(&chrono::Utc))));
    }
    if t.is_integer()
        && let Ok(n) = s.trim().parse::<i128>()
    {
        return of_int(n, t);
    }
    let plain = match t {
        Typ::Decimal => {
            let s = s.trim();
            rust_decimal::Decimal::from_str_exact(s)
                .or_else(|_| rust_decimal::Decimal::from_scientific(s))
                .ok()
                .map(|d| Value::Decimal(d.into()))
        }
        Typ::F64 => s.trim().parse::<f64>().ok().map(Value::F64),
        Typ::F32 => s.trim().parse::<f32>().ok().map(Value::F32),
        _ => None,
    };
    if plain.is_some() {
        return plain;
    }
    if parses_to_collection(s) {
        return None;
    }
    match s.parse::<Value>() {
        Ok(Value::String(_)) | Err(_) => None,
        Ok(parsed) => convert(parsed, t),
    }
}

/// A number, bool, datetime or duration printed without its type: a
/// datetime in RFC3339 and a duration in seconds, as json writes them.
fn printed(v: &Value) -> Option<Value> {
    let s = match v {
        Value::Bool(b) => format_compact!("{b}"),
        Value::DateTime(d) => format_compact!("{}", d.to_rfc3339()),
        Value::Duration(d) => format_compact!("{}", d.as_secs_f64()),
        v if Typ::get(v).is_number() => format_compact!("{}", NakedValue(v)),
        _ => return None,
    };
    Some(Value::String(s.as_str().into()))
}

/// `v`, a number, bool, datetime or duration, as the integer type `t`, exactly
/// or not at all; a fraction, of a float or of a second, is dropped
/// first.
fn integer(v: &Value, t: Typ) -> Option<Value> {
    let n = match v {
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
        Value::Bool(b) => *b as i128,
        Value::DateTime(d) => {
            let secs = d.timestamp() as i128;
            secs + (secs < 0 && d.timestamp_subsec_nanos() > 0) as i128
        }
        Value::Duration(d) => d.as_secs() as i128,
        v => return v.clone().cast(t),
    };
    of_int(n, t)
}

/// Whether `v` reads as a number: a number, a bool (1 or 0), a datetime
/// (its Unix seconds) or a duration (its seconds).
fn counts(v: &Value) -> bool {
    matches!(v, Value::Bool(_) | Value::DateTime(_) | Value::Duration(_))
        || Typ::get(v).is_number()
}

fn of_int(n: i128, t: Typ) -> Option<Value> {
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

/// A float's integer part, `None` for NaN or an infinity.
fn float_int(f: f64) -> Option<i128> {
    f.is_finite().then(|| f.trunc() as i128)
}

/// Whether a part of each type a [`map_refs`] walk meets can hold a
/// reference, decided once per type.
type HoldsRef = AHashMap<Type, bool>;

fn map_refs(
    env: &Env,
    holds: &mut HoldsRef,
    t: &Type,
    v: &Value,
    f: &mut dyn FnMut(&Value) -> Value,
) -> Value {
    ensure_sufficient(|| {
        let refs = match holds.get(t) {
            Some(r) => *r,
            None => {
                let r = compared_ref(env, &mut Expanded::default(), t);
                holds.insert(t.clone(), r);
                r
            }
        };
        if !refs {
            return v.clone();
        }
        let Some(t) = rep_head(env, t) else { return v.clone() };
        let t = &*t;
        let mut each = |ts: &mut dyn Iterator<Item = (&Type, &Value)>| {
            let vs: LPooled<Vec<Value>> =
                Iterator::map(&mut &mut *ts, |(t, v)| map_refs(env, holds, t, v, f))
                    .collect();
            vs
        };
        match (t, v) {
            (Type::ByRef(..), v) => f(v),
            (Type::Set(ts), v) => match member_of(env, ts, v) {
                Some(i) => map_refs(env, holds, &ts[i], v, f),
                None => v.clone(),
            },
            (Type::Error(e), Value::Error(x)) => {
                Value::Error(map_refs(env, holds, e, x, f).into())
            }
            (Type::Array(e), v) if let Some(x) = rep::array_elems(v) => {
                let mut vs = each(&mut x.iter().map(|v| (&**e, v)));
                Value::Array(ValArray::from_iter_exact(vs.drain(..)))
            }
            (Type::List(e), v) => {
                let vs: LPooled<Vec<Value>> = list::Iter::new(v.clone())
                    .map(|v| map_refs(env, holds, e, &v, f))
                    .collect();
                list::from_iter(vs.iter().cloned())
            }
            (Type::Tuple(ts), v)
                if let Some(x) = rep::tuple_elems(v)
                    && x.len() == ts.len() =>
            {
                let mut vs = each(&mut ts.iter().zip(x.iter()));
                rep::tuple(vs.drain(..))
            }
            (Type::Variant(tag, ts, _), v)
                if let Some((_, x)) = rep::variant(v)
                    && !x.is_empty()
                    && x.len() == ts.len() =>
            {
                let mut vs = each(&mut ts.iter().zip(x.iter()));
                rep::make_variant(tag.clone(), vs.drain(..))
            }
            (Type::Struct(fs), v)
                if let Some(x) = rep::struct_fields(v)
                    && x.len() == fs.len() =>
            {
                let mut vs: LPooled<Vec<Value>> = fs
                    .iter()
                    .zip(x.iter())
                    .map(|((_, t, _), field)| match rep::field(field) {
                        Some((n, v)) => {
                            rep::make_field(n.clone(), map_refs(env, holds, t, v, f))
                        }
                        None => field.clone(),
                    })
                    .collect();
                rep::structure_of_fields(vs.drain(..))
            }
            (Type::Map { value, .. }, Value::Map(m)) => Value::Map(
                m.into_iter()
                    .map(|(k, v)| (k.clone(), map_refs(env, holds, value, v, f)))
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
        map_refs(env, &mut HoldsRef::default(), self, v, f)
    }
}
