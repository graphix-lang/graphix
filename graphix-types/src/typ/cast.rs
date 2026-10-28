use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG, PrintFlag,
    abstract_value::{self, CastHook},
    env::Env,
    errf,
    expr::{ModPath, union_members},
    format_with_flags, list, rep,
    stack::ensure_sufficient,
    typ::{
        AbstractId, Bound, CoreTrait, TVar, Type, TypeRef, params_size,
        setops::union_identical,
        tval::{NakedPrefix, has_form, member_of},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::{BitFlags, bitflags, make_bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::{NakedValue, Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{borrow::Cow, convert::Infallible, fmt, mem, ops::ControlFlow, sync::OnceLock};
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
#[derive(Debug, Clone)]
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
/// such members are; `None` when they differ.
fn src_head(cx: &mut CastCx, src: Option<&Type>, env: &Env, v: &Value) -> Option<Type> {
    let mut cur = src?.clone();
    loop {
        cur = match &cur {
            Type::TVar(_) => cur.deref_cloned()?,
            Type::App(c, a) => Type::app_filled(c, a)?,
            Type::Ref(tr) => cx.expand(env, &cur, tr)?,
            Type::Set(ms) => {
                let mut heads: SmallVec<[Type; 4]> = SmallVec::new();
                for m in ms.iter() {
                    let h = src_head(cx, Some(m), env, v)?;
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

/// How many values deep a cast reads: data nested deeper is refused, so
/// foreign data bounds neither the walk's stack nor its memory.
pub const MAX_DATA_DEPTH: usize = 1024;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CastInput {
    Data,
    ExactFloats,
    Marked,
}

/// A cast in progress: the refs expanded on the way to each value.
struct CastCx {
    hist: LPooled<AHashSet<(RefKey, usize)>>,
    /// The params sizes on the path of each (definition, value address).
    sizes: LPooled<AHashMap<(usize, usize), SmallVec<[usize; 2]>>>,
    /// Each reference's expansion, by definition and params allocation,
    /// with the params held so the allocation stays theirs.
    expanded: LPooled<AHashMap<(usize, usize), (Arc<[Type]>, Type)>>,
    /// Each union's reading of a value inside another union's, by the
    /// union's members, the value's address and `exact`, with the union
    /// held so the members stay its: a union tries several members over
    /// one value, so without it a failing read is exponential in the
    /// data's depth.
    read: LPooled<AHashMap<(usize, usize, bool), (Type, Cast)>>,
    /// Whether the walk is inside a union's reading.
    in_union: bool,
    /// How many values on the path are built by the walk, not held by
    /// the cast's value: their addresses are reused, so no reading of a
    /// value under one is remembered.
    built: usize,
    /// How many times the walk refused a type that recursed without
    /// consuming the value, which depends on the path: no reading that
    /// met one is remembered.
    cuts: usize,
    /// The address of the value being cast and how many values deep it is.
    at: usize,
    depth: usize,
    /// Only a conversion that loses nothing counts: a union tries its
    /// members so first.
    exact: bool,
    input: CastInput,
    check_keys: bool,
}

impl CastCx {
    fn new(input: CastInput, check_keys: bool) -> Self {
        CastCx {
            hist: LPooled::take(),
            sizes: LPooled::take(),
            expanded: LPooled::take(),
            read: LPooled::take(),
            in_union: false,
            built: 0,
            cuts: 0,
            at: 0,
            depth: 0,
            exact: false,
            input,
            check_keys,
        }
    }

    /// The expansion of the reference `t`, made once a cast.
    fn expand(&mut self, env: &Env, t: &Type, tr: &TypeRef) -> Option<Type> {
        let Some(def) = tr.def_key() else { return t.lookup_ref(env).ok() };
        let ek = (def, tr.params.as_ptr().addr());
        if let Some((_, t)) = self.expanded.get(&ek) {
            return Some(t.clone());
        }
        let x = t.lookup_ref(env).ok()?;
        self.expanded.insert(ek, (tr.params.clone(), x.clone()));
        Some(x)
    }

    /// `f` over a value the walk built.
    fn over_built<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.built += 1;
        let r = f(self);
        self.built -= 1;
        r
    }

    /// `v` as the f64 a float position reads, when it is a decimal that
    /// stands for a float.
    fn float_of(&self, v: &Value) -> Option<Value> {
        match v {
            Value::Decimal(_) if self.input == CastInput::ExactFloats => {
                convert(v.clone(), Typ::F64)
            }
            _ => None,
        }
    }
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
    /// The shared nodes a search walked and found nothing under, so a
    /// type that shares them (a DAG) is searched once; only outside any
    /// application being decided, whose provisional verdicts a node's may
    /// rest on.
    walked: LPooled<AHashSet<usize>>,
    /// The types the walk built outside any application being decided,
    /// alive until it ends: a node `walked` holds keeps its address.
    built: LPooled<Vec<Type>>,
}

impl<T> Verdicts<T> {
    fn new() -> Self {
        Self {
            known: LPooled::take(),
            pending: LPooled::take(),
            depth: 0,
            read: usize::MAX,
            walked: LPooled::take(),
            built: LPooled::take(),
        }
    }

    /// `f` over `t`, a type built for this walk.
    fn over_built<R>(&mut self, t: Type, f: impl FnOnce(&mut Self, &Type) -> R) -> R {
        let r = f(self, &t);
        if self.depth == 0 {
            self.built.push(t)
        }
        r
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

    /// Whether a part of this type reads a number exactly: an integer or
    /// a decimal, or an abstract type, whose `Cast` reads what it likes.
    pub fn holds_exact_number(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Primitive(p) => Some(p.intersects(Typ::integer() | Typ::Decimal)),
            Type::Abstract { .. } => Some(true),
            _ => None,
        })
    }

    /// Whether a value of this type can hold a reference, outside a
    /// function or an abstract type with no `Cast` implementation, which
    /// a cast never takes apart (one with an implementation may read its
    /// payload at its parameters). A cast refuses such a source: a
    /// reference names a cell, it is no data to read.
    #[doc(hidden)]
    pub fn holds_ref(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) => Some(true),
            Type::Fn(_) => Some(false),
            Type::Abstract { id, .. } if !env.casts(*id) => Some(false),
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
        !self.holds_beyond_data(env, false)
    }

    /// Whether `Concrete ⊇ self` holds as the type stands: [`Self::data_holds`]
    /// and no ⊥ (a value read from data can't be any of them), judged
    /// through type references; bound cells by their bindings, open cells
    /// admitted.
    pub(crate) fn concrete_holds(&self, env: &Env) -> bool {
        !self.holds_beyond_data(env, true)
    }

    /// Whether some part of this type is no data: a reference, a function,
    /// an abstract type with no cast, or, with `bottom`, ⊥.
    fn holds_beyond_data(&self, env: &Env, bottom: bool) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::ByRef(..) | Type::Fn(_) => Some(true),
            Type::Bottom => bottom.then_some(true),
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
                // an open cell will hold what bounds it from below
                Type::TVar(_) => match self.deref_cloned() {
                    Some(t) => seen.over_built(t, |seen, t| t.holds(env, seen, leaf)),
                    None => self.open_lower().iter().any(|l| l.holds(env, seen, leaf)),
                },
                // an open constructor is unknown like an open cell: its
                // argument decides
                Type::App(c, a) => match Type::app_filled(c, a) {
                    Some(f) => seen.over_built(f, |seen, f| f.holds(env, seen, leaf)),
                    None => a.holds(env, seen, leaf),
                },
                Type::Ref(tr) => {
                    let Ok(t) = self.lookup_ref(env) else { return false };
                    let shape =
                        tr.params.iter().map(|p| p.holds(env, seen, leaf)).collect();
                    match tr.def_key() {
                        None => seen.over_built(t, |seen, t| t.holds(env, seen, leaf)),
                        Some(k) => decide_once(seen, (k, shape), false, |seen| {
                            t.holds(env, seen, leaf)
                        }),
                    }
                }
                t => {
                    let node = super::node_addr(t).filter(|_| seen.depth == 0);
                    if node.is_some_and(|n| seen.walked.contains(&n)) {
                        return false;
                    }
                    let mut r = false;
                    t.for_each_child(&mut |c| r = r || c.holds(env, seen, leaf));
                    if !r && let Some(n) = node {
                        seen.walked.insert(n);
                    }
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
        let at = (v as *const Value).addr();
        if at == cx.at {
            return ensure_sufficient(|| self.cast_inner(env, cx, src, v));
        }
        if cx.depth == MAX_DATA_DEPTH {
            return Err(self.cast_fail("the data is nested too deep", v));
        }
        let outer = mem::replace(&mut cx.at, at);
        cx.depth += 1;
        let r = ensure_sufficient(|| self.cast_inner(env, cx, src, v));
        cx.depth -= 1;
        cx.at = outer;
        r
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
            Type::Bound(_) => Err(self.cast_fail("a constraint", v)),
            Type::Bottom | Type::Any => Ok(cx.float_of(v)),
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
                if !s.intersects(Typ::integer() | Typ::Decimal)
                    && let Some(f) = cx.float_of(v)
                {
                    let c = self.cast_inner(env, cx, src, &f)?;
                    return Ok(Some(c.unwrap_or(f)));
                }
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
                let mut members = (*s & own).iter().chain((*s & !own).iter());
                let exact = members
                    .clone()
                    .find_map(|t| convert(v.clone(), t).filter(|c| holds_exactly(v, c)));
                match exact.or_else(|| match cx.exact {
                    true => None,
                    false => members.find_map(|t| convert(v.clone(), t)),
                }) {
                    Some(v) => Ok(Some(v)),
                    None => Err(self.cast_fail("no primitive conversion", v)),
                }
            }
            Type::TVar(tv) => match tv.binding() {
                Some(t) => t.cast_int(env, cx, src, v),
                None => Ok(cx.float_of(v)),
            },
            Type::Error(e) => {
                let src = src_head(cx, src, env, v);
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
            // a list reads as the array of its elements, a map as the
            // array of its (key, value) pairs
            Type::Array(et) => {
                let src = src_head(cx, src, env, v);
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    v if list::is_list(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in list::heads(v) {
                            let c = et.cast_int(env, cx, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    v if let Some(elts) = rep::array_elems(v) => Ok(cast_elts(
                        &elts[..],
                        |v| v,
                        |_, el| et.cast_int(env, cx, src, el),
                    )?
                    .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..))))),
                    Value::Map(m) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for (k, val) in m.into_iter() {
                            let pair = rep::tuple([k.clone(), val.clone()]);
                            let c =
                                cx.over_built(|cx| et.cast_int(env, cx, None, &pair))?;
                            out.push(c.unwrap_or(pair));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    v => Err(self.cast_fail("not an array", v)),
                }
            }
            Type::List(et) => {
                let src = src_head(cx, src, env, v);
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    v if list::is_list(v) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        let mut changed = false;
                        for el in list::heads(v) {
                            match et.cast_int(env, cx, src, el)? {
                                Some(c) => {
                                    changed = true;
                                    out.push(c)
                                }
                                None => out.push(el.clone()),
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
                let src = src_head(cx, src, env, v);
                let (ks, vs) = match &src {
                    Some(Type::Map { key, value }) => (Some(&**key), Some(&**value)),
                    _ => (None, None),
                };
                let check_keys = cx.check_keys && key.orders_refs(env);
                let mut entry = |k: &Value,
                                 v: &Value|
                 -> std::result::Result<_, CastFail> {
                    let ck = key.cast_int(env, cx, ks, k)?;
                    if check_keys
                        && !crate::reference::orderable(ck.as_ref().unwrap_or(k))
                    {
                        return Err(self.cast_fail(
                            "references have no order, including under Any",
                            k,
                        ));
                    }
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
                        if !changed {
                            return Ok(None);
                        }
                        let n = out.len();
                        let map = Map::from_iter(out.drain(..));
                        match map.len() == n {
                            true => Ok(Some(Value::Map(map))),
                            false => Err(self.cast_fail("two keys cast to one key", v)),
                        }
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
                        let n = out.len();
                        let map = Map::from_iter(out.drain(..));
                        match map.len() == n {
                            true => Ok(Some(Value::Map(map))),
                            false => Err(self.cast_fail("two keys cast to one key", v)),
                        }
                    }
                    _ => Err(self.cast_fail("not a map", v)),
                }
            }
            Type::Tuple(ts) => {
                let src = src_head(cx, src, env, v);
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
                let src = src_head(cx, src, env, v);
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
                let src = src_head(cx, src, env, v);
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
                    (Some((t, _)), _) if t != tag => {
                        Err(self.cast_fail("variant tag mismatch", v))
                    }
                    (Some(_), _) => Err(self.cast_fail("variant length mismatch", v)),
                    // XCR claude for claude: [bug/medium] This tagless read takes any
                    // array of the payload's length, including another variant's
                    // written form. json::write_str(`B(2)) is ["B",2], and reading it
                    // into `A(string, i64) gives `A("B", 2) instead of InvalidCast. An
                    // untagged [1, 2] read into [`A(i64, i64), `B(i64, i64)] becomes
                    // `A(1, 2), with nothing to choose the member. No writer here emits
                    // a tagless payload (rep::strip and core's shape always write the
                    // tag), so the arm only turns a mismatch into a wrong value. Delete
                    // it, or admit it only for a lone variant target whose data does
                    // not open with a string. probe:
                    // design/review-2026-10-26/repro/x-kind-markers-tagless-variant-read.gx
                    // (x-kind-markers-03)
                    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q42,
                    // Union members that share a data form): yes a for json/toml, b for
                    // pack and db (the recommendation: (a) for json/toml targets; (b) for
                    // pack and db.)
                    // 2026-10-27 codex: Partial rework of q42. Pack and db now preserve and
                    // validate composite kind markers; their casts defer a converting array
                    // member until every array member has had a chance to hold the value
                    // unchanged. Reused Restore for the decoder's scoped marker permission.
                    // Pins pack_preserves_union_member_kinds failed all four modes on
                    // current main; pack_array_union_prefers_unchanged_elements and
                    // db_array_union_prefers_unchanged_elements failed all eight modes with
                    // the old patch, then passed with the revised member selection. The
                    // temporary cr_rework harness (exact retained fixtures) passes 20
                    // tests, including db_preserves_union_member_kind and the recursive
                    // abstract-alias guard. Binary rule, book, interface and skill docs
                    // updated. The JSON/TOML target restriction remains unimplemented:
                    // putting a format-specific bound on json::read would change a public
                    // signature used by
                    // ../netidx/graphix-package-netidx-admin/src/graphix/tui/landing.gx,
                    // contrary to this brief's explicit restriction. Applying it to
                    // Concrete generally would incorrectly reject the pack/db targets this
                    // ruling requires. No such signature change was made. This CR stays
                    // open; the text-data constraint needs coordination or an approved
                    // checker design preserving that API.
                    // 2026-10-27 codex: Rechecked finding 10 in this continuation. The
                    // q42(a) target restriction is still blocked by the brief's ban on
                    // changing public signatures used by ../netidx: json::read is used in
                    // graphix-package-netidx-admin/src/graphix/tui/landing.gx. Concrete is
                    // shared with pack/db, which must accept these unions. A
                    // format-specific builtin refusal in typecheck1 would make build reject
                    // what check accepts, contrary to the project rule. No such workaround
                    // or signature change was made; q42 remains partial and needs
                    // coordinator authorization for a format-specific target bound.
                    // 2026-10-28 claude: q42(a) built, but it does not cover this: the
                    // target here is one variant, not a union, and reading ["B", 2] into
                    // `A(string, i64) still gives `A("B", 2) (probe run 2026-10-28). The
                    // tagless arm is still to delete or narrow as the CR proposes.
                    // 2026-10-28 claude: Fixed as the CR proposed: the tagless read is
                    // deleted, since no writer emits a payload without its tag and the arm
                    // only turned a mismatch into a wrong value. A variant reads only data
                    // that names its tag; another variant's written form is a tag mismatch
                    // (now named so), an untagged array not a variant. Pin:
                    // lang::types::a_variant_reads_only_its_own_tag.
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
                let Some(t) = cx.expand(env, self, tr) else {
                    return Err(self.cast_fail("undefined type", v));
                };
                // the same definition over the same value with larger params
                // grows at every level, the value never consumed
                let size = params_size(&key.0.1);
                let at = (key.0.0, key.1);
                let grows =
                    cx.sizes.get(&at).is_some_and(|ss| ss.iter().any(|s| *s < size));
                if grows || !cx.hist.insert(key.clone()) {
                    cx.cuts += 1;
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
            // first member it converts to across classes. Each way, a member
            // that holds the value exactly comes before one that would lose
            // part of it (a fraction, a sub-second part, a rounding).
            Type::Set(ts) => {
                let outer = cx.exact;
                let key = (cx.in_union && cx.built == 0)
                    .then(|| (ts.as_ptr().addr(), (v as *const Value).addr(), outer));
                if let Some(k) = &key
                    && let Some((_, r)) = cx.read.get(k)
                {
                    return r.clone();
                }
                let (in_union, cuts) = (mem::replace(&mut cx.in_union, true), cx.cuts);
                let array = rep::array_elems(v).is_some()
                    && (cx.input == CastInput::Marked
                        || matches!(src_head(cx, src, env, v), Some(Type::Array(_))));
                let rounds: &[bool] = if outer { &[true] } else { &[true, false] };
                let mut r = Err(None);
                for exact in rounds {
                    cx.exact = *exact;
                    r = self.cast_member(env, cx, src, v, ts, array).map_err(Some);
                    cx.exact = outer;
                    if r.is_ok() {
                        break;
                    }
                }
                cx.in_union = in_union;
                let r = r.map_err(|e| e.expect("a round was tried"));
                if let Some(k) = key
                    && cx.cuts == cuts
                {
                    cx.read.insert(k, (self.clone(), r.clone()));
                }
                r
            }
        }
    }

    /// The union `ts`'s reading of `v`, as `cast_int` takes one: when `v`
    /// is statically an array (`array`), an array member it inhabits; else
    /// a member whose tag it carries, else, for unmarked data, a tuple of
    /// its length and then a struct, else one the value inhabits, else the
    /// first member of its own runtime class it converts to, else the first
    /// it converts to at all.
    fn cast_member(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
        ts: &[Type],
        array: bool,
    ) -> Cast {
        if array {
            let arrays = ts.iter().filter(|t| match rep_head(env, t).as_deref() {
                Some(Type::Array(_)) => true,
                Some(Type::Primitive(p)) => p.contains(Typ::Array),
                _ => false,
            });
            let mut converted = None;
            for t in arrays {
                match t.cast_int(env, cx, src, v) {
                    Ok(None) => return Ok(None),
                    Ok(Some(v))
                        if cx.input == CastInput::Marked && converted.is_none() =>
                    {
                        converted = Some(v)
                    }
                    _ => (),
                }
            }
            if converted.is_some() {
                return Ok(converted);
            }
        }
        // a member whose tag the value carries is its reading, whatever a
        // tagless reading of an earlier member says
        // XCR claude for claude: [bug/medium] The data a writer emits does not say which
        // union member it came from, and this order reads it back as the wrong one. A
        // tuple ("B", 7) of [(string, i64), `B(i64)] comes back from pack, json and db
        // as `B(7), against the stated rule that unmarked data reads as a tuple of its
        // length first. A List of [List<i64>, (i64, i64)] comes back from pack as a
        // tuple. A Map of [Map<string, i64>, Array<(string, i64)>] comes back from json
        // as the pairs. The checker accepts these unions, so a reloaded state takes the
        // wrong select arm. Decide one rule: refuse, under Data/Concrete, unions whose
        // members share a data form (as Discernible refuses shared runtime forms), or
        // have pack and db keep kinds, since only Graphix reads them back. probe:
        // design/review-2026-10-26/repro/x-kind-markers-tuple-reads-back-as-variant.gx
        // (x-kind-markers-02)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q42, Union
        // members that share a data form): yes a for json/toml, b for pack and db (the
        // recommendation: (a) for json/toml targets; (b) for pack and db.)
        // 2026-10-27 codex: Partial rework of q42. Pack and db now preserve and
        // validate composite kind markers; their casts defer a converting array member
        // until every array member has had a chance to hold the value unchanged. Reused
        // Restore for the decoder's scoped marker permission. Pins
        // pack_preserves_union_member_kinds failed all four modes on current main;
        // pack_array_union_prefers_unchanged_elements and
        // db_array_union_prefers_unchanged_elements failed all eight modes with the old
        // patch, then passed with the revised member selection. The temporary cr_rework
        // harness (exact retained fixtures) passes 20 tests, including
        // db_preserves_union_member_kind and the recursive abstract-alias guard. Binary
        // rule, book, interface and skill docs updated. The JSON/TOML target
        // restriction remains unimplemented: putting a format-specific bound on
        // json::read would change a public signature used by
        // ../netidx/graphix-package-netidx-admin/src/graphix/tui/landing.gx, contrary
        // to this brief's explicit restriction. Applying it to Concrete generally would
        // incorrectly reject the pack/db targets this ruling requires. No such
        // signature change was made. This CR stays open; the text-data constraint needs
        // coordination or an approved checker design preserving that API.
        // 2026-10-27 codex: Rechecked finding 10 in this continuation. The q42(a)
        // target restriction is still blocked by the brief's ban on changing public
        // signatures used by ../netidx: json::read is used in
        // graphix-package-netidx-admin/src/graphix/tui/landing.gx. Concrete is shared
        // with pack/db, which must accept these unions. A format-specific builtin
        // refusal in typecheck1 would make build reject what check accepts, contrary to
        // the project rule. No such workaround or signature change was made; q42
        // remains partial and needs coordinator authorization for a format-specific
        // target bound.
        // 2026-10-28 claude: q42(a) built for json/toml: json::read's target is 'b:
        // Concrete + JsonData and toml::read's 'b: Concrete + TomlData (new bounds,
        // typ::textdata): no union anywhere in the target holds two members one value
        // of the format could be written by (any-length arrays share the empty array,
        // an object reads as its sorted key/value pairs, a struct takes only its fields
        // and leaves out only nullable ones, ints and floats write apart, JSON writes a
        // datetime and a tag as strings and a duration as a float, TOML has no null and
        // a native datetime). Judged of the call's settled binding in site_bounds,
        // never at the bind, where a refusal would narrow a union target to one member.
        // Pins: lang::types::{json_target_union_its_data_cannot_tell_apart_refused,
        // toml_target_union_its_data_cannot_tell_apart_refused,
        // json_target_members_written_apart_read_back},
        // typ::textdata::test::unions_whose_members_write_alike;
        // data_reads_back_as_written keeps the pack half.
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
        // a writer wrote the tuple or the struct the data reads back as
        // CR claude for claude: [bug/medium] The rank pass takes the first same-length
        // tuple (then struct) that casts at all, including by printing a number into a
        // string, ahead of a member the data reads without leaving its class. With T =
        // [List<i64>, (i64, string), Array<[i64, f64]>], json::write_str of [[<1, 2>],
        // [1, 2.5]] reads back as [(1, "2"), (1, "2.5")], and an Array<i64> value cast
        // to [Array<i64>, (i64, i64)] becomes a tuple. The Set arm's 'a member the
        // value already inhabits wins' and this function's doc both claim otherwise.
        // Should the tuple and struct rank only win when no element leaves its class,
        // with the comments updated to the order chosen? probe:
        // design/review-2026-10-26/repro/t-cast-setops-tuple-rank-cross-class.gx
        // (t-cast-setops-06)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q42, Union
        // members that share a data form): yes a for json/toml, b for pack and db (the
        // recommendation: (a) for json/toml targets; (b) for pack and db.)
        // 2026-10-27 codex: Partial rework of q42. Pack and db now preserve and
        // validate composite kind markers; their casts defer a converting array member
        // until every array member has had a chance to hold the value unchanged.
        // Reused Restore for the decoder's scoped marker permission. Pins
        // pack_preserves_union_member_kinds failed all four modes on current main;
        // pack_array_union_prefers_unchanged_elements and
        // db_array_union_prefers_unchanged_elements failed all eight modes with the
        // old patch, then passed with the revised member selection. The temporary
        // cr_rework harness (exact retained fixtures) passes 20 tests, including
        // db_preserves_union_member_kind and the recursive abstract-alias guard.
        // Binary rule, book, interface and skill docs updated. The JSON/TOML target
        // restriction remains unimplemented: putting a format-specific bound on
        // json::read would change a public signature used by
        // ../netidx/graphix-package-netidx-admin/src/graphix/tui/landing.gx, contrary
        // to this brief's explicit restriction. Applying it to Concrete generally
        // would incorrectly reject the pack/db targets this ruling requires. No such
        // signature change was made. This CR stays open; the text-data constraint
        // needs coordination or an approved checker design preserving that API.
        // 2026-10-27 codex: Rechecked finding 10 in this continuation. The q42(a)
        // target restriction is still blocked by the brief's ban on changing public
        // signatures used by ../netidx: json::read is used in
        // graphix-package-netidx-admin/src/graphix/tui/landing.gx. Concrete is shared
        // with pack/db, which must accept these unions. A format-specific builtin
        // refusal in typecheck1 would make build reject what check accepts, contrary
        // to the project rule. No such workaround or signature change was made; q42
        // remains partial and needs coordinator authorization for a format-specific
        // target bound.
        // 2026-10-28 claude: q42(a) built (JsonData/TomlData): the probe's json read
        // into Array<[List<i64>, (i64, string), Array<[i64, f64]>]> is now refused
        // (List<i64> and Array<[i64, f64]> write alike). Left: an Array<i64> value
        // cast to [Array<i64>, (i64, i64)] becoming the tuple, an in-program cast the
        // reader bounds do not decide.
        let mut ranked: SmallVec<[bool; 8]> = SmallVec::from_elem(false, ts.len());
        if let Value::Array(a) = v
            && rep::kind(v).is_none()
        {
            let rank = |t: &Type| match rep_head(env, t).as_deref() {
                Some(Type::Tuple(es)) if es.len() == a.len() => Some(0),
                Some(Type::Struct(_)) => Some(1),
                _ => None,
            };
            for r in 0..2 {
                for (i, t) in ts.iter().enumerate().filter(|(_, t)| rank(t) == Some(r)) {
                    match t.cast_int(env, cx, src, v) {
                        Ok(c) => return Ok(c),
                        Err(_) => ranked[i] = true,
                    }
                }
            }
        }
        let own: LPooled<Vec<bool>> =
            ts.iter().map(|t| in_own_class(env, t, v)).collect();
        let mut converted = None;
        for pass in [true, false] {
            let members = ts.iter().zip(own.iter()).zip(ranked.iter());
            for ((t, _), _) in members.filter(|((_, o), r)| **o == pass && !**r) {
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

    /// Cast `v` of unknown static type to this type; a failure is the
    /// `InvalidCast` error value. An unmarked array reads as a writer's
    /// data: a union reads it as a tagged variant, a tuple or a struct
    /// before an array.
    pub fn cast_value(&self, env: &Env, v: Value) -> Value {
        self.cast_with(env, None, v, CastInput::Data, true).unwrap_or_else(|e| e)
    }

    /// Read binary data whose composite kinds are preserved: an unmarked
    /// array is an array, including at nested union positions.
    pub fn cast_marked_value(&self, env: &Env, v: Value) -> Value {
        self.cast_with(env, None, v, CastInput::Marked, true).unwrap_or_else(|e| e)
    }

    /// [`Self::cast_value`] with its refusal apart from a cast value that
    /// is an error: `Err` is the `InvalidCast` error value.
    pub fn try_cast_value(&self, env: &Env, v: Value) -> Result<Value, Value> {
        self.cast_with(env, None, v, CastInput::Data, true)
    }

    /// [`Self::cast_value`] of a `v` statically of type `src`, which
    /// decides whether an unmarked array is an array or a writer's data:
    /// a union reads an array as an array member it inhabits first.
    pub fn cast_from(&self, env: &Env, src: &Type, v: Value) -> Value {
        self.cast_with(env, Some(src), v, CastInput::Data, true).unwrap_or_else(|e| e)
    }

    /// [`Self::cast_value`] of data whose decimals stand for floats read
    /// exactly (a json number past a float's precision): a decimal stays
    /// one only where its position holds an integer or a decimal, and is
    /// elsewhere the f64 its text reads as.
    pub fn cast_exact_floats(&self, env: &Env, v: Value) -> Value {
        self.cast_with(env, None, v, CastInput::ExactFloats, true).unwrap_or_else(|e| e)
    }

    /// A cast with its input format and static map-key check decision.
    /// `check_keys` may be false only when the target contains neither Any
    /// nor an abstract type whose representation a cast can enter.
    pub fn cast_with(
        &self,
        env: &Env,
        src: Option<&Type>,
        v: Value,
        input: CastInput,
        check_keys: bool,
    ) -> Result<Value, Value> {
        match self.cast_int(env, &mut CastCx::new(input, check_keys), src, &v) {
            Ok(None) => Ok(v),
            Ok(Some(v)) => Ok(v),
            Err(e) => Err(errf!(CAST_ERR_TAG, "{e}")),
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
            Type::Hole | Type::Bound(_) => false,
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
        ShallowScrutinee::new(scrutinee.clone()).discriminant(env, self)
    }
}

/// The scrutinee and its referenced definitions must stay fixed while these facts are used.
#[derive(Debug)]
pub struct ShallowScrutinee {
    typ: Type,
    members: OnceLock<Option<SmallVec<[Type; 8]>>>,
    facts: OnceLock<Option<Box<[MemberFacts]>>>,
}

impl ShallowScrutinee {
    pub fn new(typ: Type) -> Self {
        Self { typ, members: OnceLock::new(), facts: OnceLock::new() }
    }

    pub fn members(&self, env: &Env) -> Option<&[Type]> {
        self.members
            .get_or_init(|| {
                let mut members = SmallVec::new();
                union_members(env, &self.typ, &mut members).ok()?;
                Some(members)
            })
            .as_deref()
    }

    fn facts(&self, env: &Env) -> Option<&[MemberFacts]> {
        self.facts
            .get_or_init(|| {
                let mut scrut: LPooled<Vec<Type>> = LPooled::take();
                let mut seen: LPooled<Vec<RefKey>> = LPooled::take();
                flatten_union_members(&self.typ, env, &mut scrut, &mut seen)?;
                let mut walked = ShapeSeen::default();
                let mut below = |m: &Type| {
                    m.try_for_each_child(&mut |c| match shapes_collide(
                        c,
                        env,
                        &mut walked,
                    ) {
                        true => ControlFlow::Break(()),
                        false => ControlFlow::Continue(()),
                    })
                    .is_break()
                };
                if scrut.iter().any(|m| below(m)) {
                    return None;
                }
                Some(scrut.iter().map(member_facts).collect())
            })
            .as_deref()
    }

    pub fn discriminant(&self, env: &Env, predicate: &Type) -> Option<Type> {
        let mut preds: LPooled<Vec<Type>> = LPooled::take();
        let mut seen: LPooled<Vec<RefKey>> = LPooled::take();
        flatten_union_members(predicate, env, &mut preds, &mut seen)?;
        if preds.iter().all(|p| member_facts(p).exact) {
            return None;
        }
        let sfacts = self.facts(env)?;
        let mut out: LPooled<Vec<Type>> = LPooled::take();
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
        }
        Some(if out.len() == 1 {
            out.pop().unwrap()
        } else {
            Type::Set(Arc::from_iter(out.drain(..)))
        })
    }
}

/// Whether `t` or a part of it is a union two of whose members share a
/// shape (`arr_overlap`). An application is walked once a search, and a
/// typedef once a path, whatever its parameters: a non-regular one's grow
/// at every level, and what they hold is walked at the first.
/// What a [`shapes_collide`] search has walked: the applications, and
/// the typedefs on the path.
#[derive(Default)]
struct ShapeSeen {
    walked: LPooled<AHashSet<RefKey>>,
    path: SmallVec<[usize; 8]>,
}

fn shapes_collide(t: &Type, env: &Env, seen: &mut ShapeSeen) -> bool {
    ensure_sufficient(|| match t {
        Type::Ref(tr) => {
            let Some(key) = ref_key(tr) else { return false };
            if seen.path.contains(&key.0) || !seen.walked.insert(key.clone()) {
                return false;
            }
            let Ok(x) = t.lookup_ref(env) else { return false };
            seen.path.push(key.0);
            let r = shapes_collide(&x, env, seen);
            seen.path.pop();
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
#[derive(Debug)]
struct MemberFacts {
    arr: Option<(Form, ArrCon)>,
    map: bool,
    error: bool,
    exact: bool,
}

/// What a member's array is: its kind, and a variant's tag.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Form {
    Array,
    Tuple,
    Struct,
    Variant(ArcStr),
    List,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
        | Type::Bound(_) => MemberFacts::EXACT,
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
    resolved_head(env, t).filter(|h| !matches!(&**h, Type::Any))
}

fn resolved_head<'a>(env: &Env, t: &'a Type) -> Option<Cow<'a, Type>> {
    let owned = |t: Type| resolved_head(env, &t).map(|h| Cow::Owned(h.into_owned()));
    ensure_sufficient(|| match t {
        Type::TVar(_) => t.deref_cloned().and_then(owned),
        Type::App(c, a) => Type::app_filled(c, a).and_then(owned),
        Type::Ref(_) => t.lookup_ref(env).ok().and_then(owned),
        Type::Hole | Type::Bound(_) => None,
        t => Some(Cow::Borrowed(t)),
    })
}

/// The primitive tags a value of another type may share.
const SHARED_PRIMS: BitFlags<Typ> = make_bitflags!(Typ::{Map | Error | Array});

/// Whether a value of `t` may also be a value of a type `t` doesn't hold:
/// a Rust-backed abstract's values carry only its id, so one with
/// parameters shares its form with its other instantiations.
fn shares_form(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        Some(Type::Abstract { id, params }) => {
            !params.is_empty() && !env.abstract_reps.contains_key(id)
        }
        None | Some(Type::Bottom) => false,
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

/// Why one of `a` and `b`, an open cell [`Open::Unknown`] reads as any
/// type it may yet bind, collides with the other: it may share the
/// other's form, or, `ordered`, be another numeric type. A bound that is
/// a type holds every value of the cell, so a cell with such bounds
/// collides only where each of them does.
fn open_collision(
    env: &Env,
    seen: &mut RepSeen,
    ordered: bool,
    a: &Type,
    b: &Type,
) -> Option<Indiscernible> {
    let open = |t: &Type| match t {
        Type::TVar(tv) => tv.open_cell(),
        _ => None,
    };
    let pair = || Some(Indiscernible::Pair(a.clone(), b.clone()));
    let mut beside = |cell: &super::TVar, t: &Type| {
        // a `NoError` cell is never an error, so it shares no error's form
        if cell.cell_constraints().contains(&Type::Bound(Bound::NoError))
            && error_only(env, t)
        {
            return None;
        }
        let typed = cell.typed_bounds(env);
        if typed.is_empty() {
            return match shares_form(env, t) {
                true => pair(),
                false => (ordered && holds_number(env, t))
                    .then(|| Indiscernible::OpenNumber(t.clone())),
            };
        }
        let mut why = None;
        for b in typed.iter() {
            why = Some(rep_collision(env, seen, Open::Unknown, ordered, t, b)?);
        }
        match why? {
            Indiscernible::Numbers(_) => Some(Indiscernible::OpenNumber(t.clone())),
            _ => pair(),
        }
    };
    match (open(a), open(b)) {
        (Some(x), Some(y)) => (x.cell_addr() != y.cell_addr()).then(pair).flatten(),
        (Some(x), None) => beside(&x, b),
        (None, Some(y)) => beside(&y, a),
        (None, None) => None,
    }
}

/// Whether a value of `t` may be a number.
fn holds_number(env: &Env, t: &Type) -> bool {
    match rep_head(env, t).as_deref() {
        Some(Type::Primitive(p)) => p.intersects(Typ::number()),
        _ => false,
    }
}

impl TVar {
    /// The bounds of this cell that are types, each holding every value
    /// of it: its constraints less the marker bounds (`Concrete`, a
    /// trait) a runtime test can't see.
    pub fn typed_bounds(&self, env: &Env) -> SmallVec<[Type; 1]> {
        let mut bounds = self.cell_constraints();
        bounds.retain(|b| rep_head(env, b).is_some());
        bounds
    }
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
struct Expanded<
    K: std::hash::Hash + Eq + 'static,
    N: std::hash::Hash + Eq + 'static = usize,
> {
    seen: LPooled<AHashSet<K>>,
    path: LPooled<Vec<((usize, usize), usize)>>,
    /// The shared nodes (or pairs of them) the walk found nothing under,
    /// so a type that shares them (a DAG) is walked once.
    clear: LPooled<AHashSet<N>>,
    /// The heads the walk built, alive until it ends: a node `clear`
    /// holds keeps its address.
    built: LPooled<Vec<Type>>,
}

impl<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static> Default
    for Expanded<K, N>
{
    fn default() -> Self {
        Self {
            seen: LPooled::take(),
            path: LPooled::take(),
            clear: LPooled::take(),
            built: LPooled::take(),
        }
    }
}

impl<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static>
    Expanded<K, N>
{
    /// `body` over the node `node`, unless the walk found nothing under
    /// it already. Whatever a node first found nothing under, it finds
    /// nothing under again: every application in it was expanded then,
    /// and is not again.
    fn once<R>(
        &mut self,
        node: Option<N>,
        body: impl FnOnce(&mut Self) -> Option<R>,
    ) -> Option<R> {
        if node.as_ref().is_some_and(|n| self.clear.contains(n)) {
            return None;
        }
        let r = body(self);
        if r.is_none()
            && let Some(n) = node
        {
            self.clear.insert(n);
        }
        r
    }

    /// Keep a head the walk built alive until it ends.
    fn keep(&mut self, head: Option<Cow<'_, Type>>) {
        if let Some(Cow::Owned(t)) = head {
            self.built.push(t)
        }
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
fn walk<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static, R>(
    seen: &mut Expanded<K, N>,
    at: Option<At<K>>,
    again: R,
    grows: impl FnOnce() -> R,
    body: impl FnOnce(&mut Expanded<K, N>) -> R,
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
type RepSeen = Expanded<(Type, Type), (usize, usize)>;

fn pair_at(a: &Type, b: &Type) -> Option<At<(Type, Type)>> {
    (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_))).then(|| {
        ((a.clone(), b.clone()), (def_of(a), def_of(b)), size_of(a) + size_of(b))
    })
}

fn ref_at(t: &Type) -> Option<At<Type>> {
    matches!(t, Type::Ref(_)).then(|| (t.clone(), (def_of(t), 0), size_of(t)))
}

/// An abstract type a walk is about to look through, keyed like a named
/// type: a representation may name its own type.
fn abstract_at(t: &Type) -> Option<At<Type>> {
    match t {
        Type::Abstract { id, params } => {
            Some((t.clone(), (id.inner() as usize, 1), params_size(params)))
        }
        _ => None,
    }
}

/// What an abstract type's values compare by without an implementation
/// of the corresponding comparison trait: its representation, its
/// parameters substituted (`design/tvar_constraints.md`). A Rust-backed
/// abstract has none to look into, and one with an implementation is
/// what the implementation says.
fn compared_rep(
    env: &Env,
    id: AbstractId,
    params: &[Type],
    kind: CoreTrait,
) -> Option<Type> {
    // XCR claude for claude: [bug/medium] Either impl excuses the representation from both
    // judgments, but an Eq impl says nothing about order. type B = Abstract<[i64, f64]>
    // with only `impl Eq for B` passes Ordered, and array::sort([B(2), B(1.5), B(1)])
    // returns [B(1), B(2), B(1.5)], the mixed-numeric order Ordered refuses when the
    // impl is absent. An Ord-only abstract likewise escapes the Discernible check that
    // == relies on. Should the representation be skipped per trait instead (Ordered
    // when Ord is implemented, Discernible and compared_ref when Eq is)? The CLAUDE.md
    // sentence 'no Eq or Ord implementation' would need the same change. probe:
    // design/review-2026-10-26/repro/t-cast-setops-eq-only-ordered.gx
    // (t-cast-setops-05)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q32, An Eq-only
    // abstract and Ordered): yes, per trait (the recommendation: per trait.)
    // 2026-10-27 codex: Implemented the per-trait ruling: Ord alone bypasses the Ordered
    // representation check; Eq alone bypasses Discernible and equality-reference
    // traversal. Diagnostics name the missing trait. Pins
    // lang::traits::{eq_only_abstract_does_not_supply_order,ord_only_abstract_does_not_supply_equality}
    // failed in all four modes before the fix; the corresponding positive tests cover
    // each implemented trait. The equality refusal uses differing function signatures,
    // which share a runtime form. Rules, book and language skill updated.
    if env.implements(kind, id) {
        return None;
    }
    Some(env.abstract_reps.get(&id)?.instantiate_with(params))
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

/// Two parts of `a` and `b` a runtime test can't tell apart, or,
/// `ordered`, two numeric types at one position, whose values have no
/// order between them.
fn rep_collision(
    env: &Env,
    seen: &mut RepSeen,
    open: Open,
    ordered: bool,
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
                    xs.map(|(x, y)| rep_collision(env, seen, open, ordered, x, y))
                        .find(Option::is_some)
                        .flatten()
                };
            let (ha, hb) = (rep_head(env, a), rep_head(env, b));
            let node = ha
                .as_deref()
                .and_then(super::node_addr)
                .zip(hb.as_deref().and_then(super::node_addr));
            let r = seen.once(node, |seen| {
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
                        (Open::Unknown, _, _) => open_collision(env, seen, ordered, a, b),
                    };
                };
                let (a, b) = (ha, hb);
                let mut pairs =
                    |xs: &mut dyn Iterator<Item = (&Type, &Type)>| pairs(seen, xs);
                match (a, b) {
                    (Type::Bottom, _) | (_, Type::Bottom) => None,
                    (Type::Set(ts), _) => pairs(&mut ts.iter().map(|t| (t, b))),
                    (_, Type::Set(ts)) => pairs(&mut ts.iter().map(|t| (a, t))),
                    (Type::Primitive(p), Type::Primitive(q)) => {
                        let numbers = (*p | *q) & Typ::number();
                        (ordered && numbers.len() > 1)
                            .then(|| Indiscernible::Numbers(Type::Primitive(numbers)))
                    }
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
                        (rust_backed && !same)
                            .then(|| Indiscernible::Pair(a.clone(), b.clone()))
                    }
                    // a runtime test can't tell a reference's mutability
                    (Type::ByRef(m0, x), Type::ByRef(m1, y)) if m0 == m1 && x == y => {
                        None
                    }
                    (Type::Fn(x), Type::Fn(y)) if x == y => None,
                    (a, b) => rep_overlaps(a, b)
                        .then(|| Indiscernible::Pair(a.clone(), b.clone())),
                }
            });
            seen.keep(ha);
            seen.keep(hb);
            r
        })
    })
}

impl Type {
    /// Two parts, one of `self` and one of `t`, that are different types
    /// with one runtime form: a value of either passes a runtime test for
    /// the other (two references; two function types; a Rust-backed
    /// abstract at two instantiations; a primitive and a head whose values
    /// carry it, `array` and `Array<i64>`). A test that tells them apart
    /// can't tell such a value's type.
    pub fn rep_collision(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), Open::Benign, false, self, t)
    }

    /// [`Self::rep_collision`] with an open cell read as any type it may
    /// yet bind ([`Open::Unknown`]): what a definition must rule out.
    pub fn rep_collision_open(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        rep_collision(env, &mut RepSeen::default(), Open::Unknown, false, self, t)
    }
}

/// Why a type is not `Discernible` or not `Ordered`.
pub enum Indiscernible {
    /// Two members of one union with one runtime form.
    Pair(Type, Type),
    /// A reference or a function, under `Ordered`: neither has an order.
    Unordered(Type),
    /// A type holding two numeric types, under `Ordered`: their values
    /// have no order between them.
    Numbers(Type),
    /// A numeric type beside an open cell, under `Ordered`: the cell may
    /// yet be another numeric type.
    OpenNumber(Type),
    /// The name of a definition whose parameters grow as it recurses, so
    /// no walk decides it.
    Grows(ModPath),
    /// An abstract type without the requested comparison implementation, whose values
    /// compare by a representation that fails one of the above; its
    /// representation is its module's, so only its name is told.
    Abstract(Type, CoreTrait),
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
            Indiscernible::Unordered(_)
            | Indiscernible::Numbers(_)
            | Indiscernible::OpenNumber(_) => {
                unreachable!("a runtime test is no order")
            }
            Indiscernible::Abstract(..) => {
                unreachable!("a runtime test does not look into an abstract type")
            }
        }
    }

    /// Why an abstract type's values fail a comparison: they compare by
    /// a representation outside its module's sight.
    pub fn by_representation(a: &Type, kind: CoreTrait) -> CompactString {
        format_compact!(
            "{} has no {kind:?} implementation, so its values compare by its \
             representation, which can't tell them apart or order them here; \
             implement {kind:?} for it",
            a.resolve_tvars()
        )
    }

    /// Why a type holding two numeric types has no order.
    pub const NUMBERS: &str = "more than one numeric type: values of two numeric \
                               types have no order between them; cast them to one";

    /// Why a type holding a numeric type beside an open cell has no order.
    pub const OPEN_NUMBER: &str = "beside a type variable that isn't known here and \
                                   may be another numeric type, whose values have no \
                                   order with these: annotate it";

    /// What an unordered type is, and how `==` and `!=` compare it.
    pub fn unordered_kind(t: &Type) -> (&'static str, &'static str) {
        match t {
            Type::Fn(_) => ("function", "by identity"),
            _ => ("reference", "by what they point to"),
        }
    }
}

// XCR claude for claude: [perf/medium] [perf/medium] This walk has no memo of shared
// nodes: `Expanded` covers only Type::Ref expansion, so a DAG type is walked once per
// path to each node. A let chain of pairs makes `xN == xN`, `uniq(xN)` or `{xN => 1}`
// cost 2^N in the check: N=26 takes 3.4 s, while `[xN, xN]` stays at 0.03 s. Every
// Discernible/Ordered judgment comes through here (lib.rs discernible and site_bounds,
// contains.rs). Remember the shared nodes a walk found nothing under, outside a
// recursive decision, as Type::holds now does, and give rep_collision's RepSeen the
// same treatment. probe: design/review-2026-10-26/repro/c-lib-discernible-dag.sh
// (c-lib-02)
// 2026-10-27 claude: rep_ambiguity and rep_collision remember the shared nodes (a pair
// of them for rep_collision) a walk found nothing under (Expanded::once; sound because
// a node walked to nothing has every application in it expanded, and an expanded one
// is not walked again), and keep each head they build alive until the walk ends
// (Expanded::keep). x60 == x60, x60 < x60, uniq(x60), {x60 => 1} and
// array::sort([x60]) check in about 1.2 s, as the bare value does. Pin: graphix-shell
// deep_nesting comparisons_over_a_shared_type_dag_check_in_linear_time (fails past 15
// s with the memo undone). Not fixed: select x60 { v => 1 } still does not check in 30
// s, a walk in the select's check the repro's header names.
// 2026-10-27 claude: reviewed: the Discernible/Ordered half holds
// (Expanded::once/keep, deep_nesting comparisons pin), but the repro's select x60 { v
// => 1 } still does not check in 30 s, which this CR's probe covers. Back to CR for
// that walk.
// 2026-10-27 claude: The select's walk was Type::could_match (a select arm's form
// test), which walked the pair DAG as a tree. could_match_int now remembers the pairs
// of shared nodes it found could match (MatchHist::found, a NodePairs, as contains'
// Link::equal and setops do; each pair is held for the walk). select x60 { v => 1 }
// now checks in 0.02 s; it did not finish in 30 s before. Pin: deep_nesting
// comparisons_over_a_shared_type_dag_check_in_linear_time now also checks the select.
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
            let head = rep_head(env, t)?;
            let r = seen.once(super::node_addr(&head), |seen| {
            let t = &*head;
            let mut parts = |ts: &mut dyn Iterator<Item = &Type>| {
                ts.map(|t| rep_ambiguity(env, seen, open, keys_only, refs, t))
                    .find(Option::is_some)
                    .flatten()
            };
            let ordered = refs && !keys_only;
            match &t {
                // CR claude for eric: [bug/medium] This check refuses an order over two
                // numeric types only where the type shows them. Any reaches the `_ =>
                // None` arm and passes Ordered, and the run-time order then puts every
                // i64 below every f64. json::read(r"[3, 2.5, 1, 0.5]") read as
                // Array<Any> sorts to [1, 3, 0.5, 2.5], and under Any `i64:3 < f64:2.5`
                // is true. The other ways around this check found this round (a union
                // the normalizer did not merge, an abstract with Eq but no Ord, an open
                // member beside a number) share the cause: the run-time numeric order
                // is by class. Decide between ordering numbers by value in the run-time
                // comparison (ties broken by class so it stays consistent with ==),
                // which closes all of these at once, and documenting Any's class order.
                // probe:
                // design/review-2026-10-26/repro/x-rerun-6-any-mixed-numeric-order.gx
                // (x-rerun-6-01)
                // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q33,
                // Mixed numerics under Any order by class): yes, by value. implement in
                // netidx-value? (the recommendation: by value; it closes every route at
                // once.)
                // 2026-10-27 codex: Reproduced q33 with
                // design/review-2026-10-26/repro/x-rerun-6-any-mixed-numeric-order.gx:
                // the result is [1, 3, 0.5, 2.5]. The common comparison is
                // netidx-value/src/op.rs, Value::partial_cmp, which compares Typ first at
                // each pair in its iterative walk; Value::Ord delegates to it. Array
                // sorting, map ordering, nested values and Graphix operators all reach
                // that contract. Fixing only a Graphix operator would leave these routes
                // inconsistent. No upstream file was changed because ../netidx is
                // read-only. Proposed upstream change: before the Typ comparison,
                // recognize a pair of numeric values and compare mathematical values
                // exactly, then break equal-value ties by Typ so cmp == Equal remains
                // consistent with the existing class-sensitive Eq/Hash.
                // Integer/float/decimal comparison must not cast both sides to f64 (that
                // loses large integers and decimal distinctions); compare sign, magnitude
                // and exact mantissa/exponent, using bounded integer arithmetic for the
                // numeric domain. Preserve the existing same-type NaN and signed-zero
                // rules, define a total cross-type NaN/infinity placement, and keep the
                // numeric family in one interval relative to nonnumeric kinds so
                // transitivity holds. Keep this in the iterative pair walk so composite
                // comparison uses it too. Upstream pins should cover 2^53 +/- 1, i64/u64
                // limits, negative fractions, decimal versus binary floats, infinities,
                // NaNs, signed zero, class ties, and transitivity/antisymmetry over all
                // numeric kinds; also ordered-map lookup and nested arrays. Once that is
                // available, Graphix should remove the mixed-numeric Ordered refusals in
                // Type::rep_collision and pin sorting/comparison/map keys under both Any
                // and explicit unions across interpreter and native modes. This CR
                // remains open; the Graphix relaxation is unsafe before the upstream
                // contract changes.
                // 2026-10-28 claude: Built on branches, not merged: netidx
                // q33-numeric-order (04460470, netidx-value Value::partial_cmp: numbers
                // of different types by exact value, ties by type, NaN below every
                // number, every number before every other kind, so == and the hash still
                // agree) and graphix q33-numeric-order (4a9fb1009, pin
                // lang::types::any_mixed_numbers_order_by_value, CLAUDE.md). The probe
                // sorts [0.5, 1, 2.5, 3]. The gate fails one test, graphix-package-db
                // encoding::test::key_bytes_sort_as_values: a db key typed Any is stored
                // by a byte encoding that orders numbers by class, so the db's iteration
                // order would stop matching Value::cmp. Question for eric: (a) a new
                // order-preserving numeric key encoding (exact across i128, f64 and
                // decimal; changes the on-disk order of Any keys, so an existing db with
                // mixed numeric Any keys must be rewritten), (b) refuse Any (and any type
                // that may hold two numeric types) as a db key, or (c) keep db's class
                // order for Any keys and document that it differs from Value's. Recommend
                // (b): Ordered already refuses mixed numeric key types, Any is the one
                // hole, and it changes no file.
                Type::Primitive(p) if ordered && (*p & Typ::number()).len() > 1 => {
                    Some(Indiscernible::Numbers(t.clone()))
                }
                Type::Set(ts) => {
                    if !keys_only {
                        for (i, a) in ts.iter().enumerate() {
                            for b in &ts[i + 1..] {
                                let c = rep_collision(
                                    env,
                                    &mut RepSeen::default(),
                                    open,
                                    ordered,
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
                Type::ByRef(..) | Type::Fn(_) if ordered => {
                    Some(Indiscernible::Unordered(t.clone()))
                }
                Type::Abstract { id, params } => {
                    let kind = if ordered { CoreTrait::Ord } else { CoreTrait::Eq };
                    let rep = compared_rep(env, *id, params, kind)?;
                    let inside = walk(
                        seen,
                        abstract_at(t),
                        None,
                        || None,
                        |seen| rep_ambiguity(env, seen, open, keys_only, refs, &rep),
                    );
                    seen.keep(Some(Cow::Owned(rep)));
                    let inside = inside?;
                    let open = |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
                    // an unknown part is judged as it would be outside
                    Some(match inside {
                        Indiscernible::Pair(a, b) if open(&a) || open(&b) => {
                            Indiscernible::Pair(a, b)
                        }
                        Indiscernible::OpenNumber(n) => Indiscernible::OpenNumber(n),
                        Indiscernible::Grows(n) => Indiscernible::Grows(n),
                        _ => Indiscernible::Abstract(t.clone(), kind),
                    })
                }
                _ => None,
            }
            });
            seen.keep(Some(head));
            r
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
                Indiscernible::Numbers(n) => anyhow::anyhow!(
                    "{what} holds {}, {}",
                    n.resolve_tvars(),
                    Indiscernible::NUMBERS
                ),
                Indiscernible::OpenNumber(n) => anyhow::anyhow!(
                    "{what} holds {}, {}",
                    n.resolve_tvars(),
                    Indiscernible::OPEN_NUMBER
                ),
                Indiscernible::Abstract(a, kind) => {
                    anyhow::anyhow!(
                        "{what} holds {}",
                        Indiscernible::by_representation(a, *kind)
                    )
                }
            }
        })
    }
}

fn reference_rep(
    env: &Env,
    id: AbstractId,
    params: &Arc<[Type]>,
    kind: CoreTrait,
) -> Option<Type> {
    let typ = Type::Abstract { id, params: params.clone() };
    if env.find_impl(kind.id(), &typ).is_ok_and(|im| im.is_some()) {
        return None;
    }
    Some(env.abstract_reps.get(&id)?.instantiate_with(params))
}

/// Whether a comparison of values of `t` meets a reference: one where
/// [`Type::indiscernible`] looks.
fn compared_ref(env: &Env, seen: &mut Expanded<Type>, t: &Type, kind: CoreTrait) -> bool {
    ensure_sufficient(|| {
        walk(
            seen,
            ref_at(t),
            false,
            || true,
            |seen| {
                let Some(t) = resolved_head(env, t) else { return false };
                let t = &*t;
                let mut any = |ts: &mut dyn Iterator<Item = &Type>| {
                    Iterator::any(&mut &mut *ts, |t| compared_ref(env, seen, t, kind))
                };
                match t {
                    Type::Any | Type::ByRef(..) => true,
                    Type::Set(ts) | Type::Tuple(ts) | Type::Variant(_, ts, _) => {
                        any(&mut ts.iter())
                    }
                    Type::Array(e) | Type::List(e) | Type::Error(e) => {
                        any(&mut std::iter::once(&**e))
                    }
                    Type::Struct(fs) => any(&mut fs.iter().map(|(_, t, _)| t)),
                    Type::Map { key, value } => any(&mut [&**key, &**value].into_iter()),
                    Type::Abstract { id, params } => {
                        match reference_rep(env, *id, params, kind) {
                            None => false,
                            Some(rep) => walk(
                                seen,
                                abstract_at(t),
                                false,
                                || true,
                                |seen| compared_ref(env, seen, &rep, kind),
                            ),
                        }
                    }
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
fn in_own_class(env: &Env, t: &Type, v: &Value) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None | Some(Type::Bottom) => true,
        Some(Type::Primitive(s)) => rep::kind(v).is_none() && s.intersects(own_class(v)),
        Some(Type::Set(ts)) => ts.iter().any(|t| in_own_class(env, t, v)),
        Some(Type::Array(_)) => rep::array_elems(v).is_some(),
        // CR claude for claude: [bug/medium] Unmarked array data never counts as a List
        // member's class, while any Array member counts however its elements must
        // convert. Data from a list writer therefore reads back as another member:
        // cast<[List<i64>, Array<string>]> of [1, 2] is ["1", "2"], a pack round trip
        // of [<1, 2>] typed so gives ["1", "2"], and a Map member round-tripped through
        // json reads back as the array-of-pairs member. The t-rep-01 fix ranked tuples
        // and structs only. Decide whether data counts as a List's (and an object's as
        // a Map's) own class, or whether the class test looks at elements. probe:
        // design/review-2026-10-26/repro/x-rerun-5-list-member-cast.gx (x-rerun-5-03)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q42, Union
        // members that share a data form): yes a for json/toml, b for pack and db (the
        // recommendation: (a) for json/toml targets; (b) for pack and db.)
        // 2026-10-27 codex: Partial rework of q42. Pack and db now preserve and
        // validate composite kind markers; their casts defer a converting array member
        // until every array member has had a chance to hold the value unchanged. Reused
        // Restore for the decoder's scoped marker permission. Pins
        // pack_preserves_union_member_kinds failed all four modes on current main;
        // pack_array_union_prefers_unchanged_elements and
        // db_array_union_prefers_unchanged_elements failed all eight modes with the old
        // patch, then passed with the revised member selection. The temporary cr_rework
        // harness (exact retained fixtures) passes 20 tests, including
        // db_preserves_union_member_kind and the recursive abstract-alias guard. Binary
        // rule, book, interface and skill docs updated. The JSON/TOML target
        // restriction remains unimplemented: putting a format-specific bound on
        // json::read would change a public signature used by
        // ../netidx/graphix-package-netidx-admin/src/graphix/tui/landing.gx, contrary
        // to this brief's explicit restriction. Applying it to Concrete generally would
        // incorrectly reject the pack/db targets this ruling requires. No such
        // signature change was made. This CR stays open; the text-data constraint needs
        // coordination or an approved checker design preserving that API.
        // 2026-10-27 codex: Rechecked finding 10 in this continuation. The q42(a)
        // target restriction is still blocked by the brief's ban on changing public
        // signatures used by ../netidx: json::read is used in
        // graphix-package-netidx-admin/src/graphix/tui/landing.gx. Concrete is shared
        // with pack/db, which must accept these unions. A format-specific builtin
        // refusal in typecheck1 would make build reject what check accepts, contrary to
        // the project rule. No such workaround or signature change was made; q42
        // remains partial and needs coordinator authorization for a format-specific
        // target bound.
        // 2026-10-28 claude: q42(a) built (JsonData/TomlData): the probe's json read of
        // a Map member beside an array of pairs is now refused at the read. Left: the
        // probe's in-program cast<[List<i64>, Array<string>]>([1, 2]), which still
        // gives ["1", "2"]; a reader bound does not decide a cast of a program value,
        // so whether a List member counts unmarked array data as its own class is still
        // this CR's question.
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
/// datetime or duration to its plain printed form, a decimal to the float
/// nearest it, and an array of numbers to bytes. Nothing converts to null
/// or to an error.
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
        (Value::Array(_) | Value::Map(_), Typ::Map | Typ::Array) => v.cast(t),
        (Value::String(s), t) => parse(s, t),
        (_, Typ::String) => printed(&v),
        (_, Typ::DateTime | Typ::Duration) if Typ::get(&v).is_number() => v.cast(t),
        (_, t) if t.is_integer() && counts(&v) => integer(&v, t),
        (Value::Decimal(d), Typ::F64) => {
            format_compact!("{d}").parse().ok().map(Value::F64)
        }
        (Value::Decimal(d), Typ::F32) => {
            format_compact!("{d}").parse().ok().map(Value::F32)
        }
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
            s.parse::<rust_decimal::Decimal>().ok().map(|d| Value::Decimal(d.into()))
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
        Value::Decimal(d) => format_compact!("{d}"),
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

/// Whether `c`, `v` converted, holds all of `v`: converted back to `v`'s
/// class it is `v` again. A string is what it parses as, so `"3.50"` is
/// held exactly by 3.5 and not by 3.
fn holds_exactly(v: &Value, c: &Value) -> bool {
    match v {
        Value::String(s) => match s.trim().parse::<Value>() {
            Ok(Value::String(_)) | Err(_) => true,
            Ok(p) => holds_exactly(&p, c),
        },
        v => convert(c.clone(), Typ::get(v)).as_ref() == Some(v),
    }
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
                let r = compared_ref(env, &mut Expanded::default(), t, CoreTrait::Eq);
                holds.insert(t.clone(), r);
                r
            }
        };
        if !refs {
            return v.clone();
        }
        let Some(t) = resolved_head(env, t) else { return v.clone() };
        let t = &*t;
        let mut each = |ts: &mut dyn Iterator<Item = (&Type, &Value)>| {
            let vs: LPooled<Vec<Value>> =
                Iterator::map(&mut &mut *ts, |(t, v)| map_refs(env, holds, t, v, f))
                    .collect();
            vs
        };
        match (t, v) {
            (Type::Any, v) => map_any_refs(env, holds, v, f).unwrap_or_else(|| v.clone()),
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
            (Type::Abstract { id, params }, v)
                if let Some(g) = abstract_value::get(v)
                    && g.id == *id
                    && let Some(rep) = reference_rep(env, *id, params, CoreTrait::Eq) =>
            {
                let payload = map_refs(env, holds, &rep, &g.payload, f);
                abstract_value::wrap(*id, g.name.clone(), g.params.clone(), payload)
            }
            (_, v) => v.clone(),
        }
    })
}

fn map_any_refs(
    env: &Env,
    holds: &mut HoldsRef,
    v: &Value,
    f: &mut dyn FnMut(&Value) -> Value,
) -> Option<Value> {
    ensure_sufficient(|| {
        if crate::reference::cell(v).is_some() {
            return Some(f(v));
        }
        if let Some(g) = abstract_value::get(v) {
            let rep = reference_rep(env, g.id, &g.params, CoreTrait::Eq)?;
            if !compared_ref(env, &mut Expanded::default(), &rep, CoreTrait::Eq) {
                return None;
            }
            let payload = map_refs(env, holds, &rep, &g.payload, f);
            return Some(abstract_value::wrap(
                g.id,
                g.name.clone(),
                g.params.clone(),
                payload,
            ));
        }
        rep::map_parts(v, &mut |v| Ok::<_, Infallible>(map_any_refs(env, holds, v, f)))
            .unwrap_or_else(|never| match never {})
    })
}

impl Type {
    /// Whether comparing values of this type meets a reference: `==`
    /// compares such values by [`Self::map_refs`].
    pub fn compares_refs(&self, env: &Env) -> bool {
        compared_ref(env, &mut Expanded::default(), self, CoreTrait::Eq)
    }

    pub fn orders_refs(&self, env: &Env) -> bool {
        compared_ref(env, &mut Expanded::default(), self, CoreTrait::Ord)
    }

    /// `v`, a value of this type, with each reference a comparison meets
    /// replaced by `f` of it.
    pub fn map_refs(
        &self,
        env: &Env,
        v: &Value,
        f: &mut dyn FnMut(&Value) -> Value,
    ) -> Value {
        map_refs(env, &mut LPooled::take(), self, v, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::Env,
        expr::{ModPath, TypeDefBody, parser::parse_type},
        typ::Type,
    };
    use netidx_value::Value;
    use std::time::{Duration, Instant};
    use triomphe::Arc;

    /// An env holding the typedefs `defs` at the root, and `t` there.
    fn typed(defs: &[(&str, &str)], t: &str) -> (Env, Type) {
        let mut env = Env::default();
        for (name, body) in defs {
            env.deftype(
                &ModPath::root(),
                name,
                Arc::from_iter([]),
                &TypeDefBody::Alias(parse_type(body).unwrap()),
                true,
                None,
                Default::default(),
                Default::default(),
                Arc::new(Default::default()),
            )
            .unwrap();
        }
        env.seed_typedef_refs();
        let t = parse_type(t).unwrap().scope_refs(&ModPath::root());
        t.seed_refs(&env);
        (env, t)
    }

    // A select's shallow test over `type T0 = [null, (T1, T1)]`, .. walks
    // each typedef once, not once per path.
    #[test]
    fn shallow_discriminant_walks_a_typedef_dag_once() {
        let depth = 26;
        let names: Vec<String> = (0..=depth).map(|i| format!("T{i}")).collect();
        let bodies: Vec<String> = (0..=depth)
            .map(|i| match i {
                i if i == depth => "i64".into(),
                i => format!("[null, (T{}, T{})]", i + 1, i + 1),
            })
            .collect();
        let defs: Vec<(&str, &str)> =
            names.iter().zip(bodies.iter()).map(|(n, b)| (&**n, &**b)).collect();
        let (env, scrutinee) = typed(&defs, "T0");
        let pred = parse_type("(T1, T1)").unwrap().scope_refs(&ModPath::root());
        pred.seed_refs(&env);
        let started = Instant::now();
        pred.shallow_discriminant(&env, &scrutinee);
        assert!(started.elapsed() < Duration::from_secs(2), "{:?}", started.elapsed());
    }

    // A failing union read costs what the data's size does: each union
    // reads each value once, however many members try it.
    #[test]
    fn failing_union_read_is_linear() {
        let (env, j) = typed(&[("J", "[i64, (J, i64), Array<J>]")], "J");
        let mut v = Value::Array([Value::from("x"), Value::I64(1)].into_iter().collect());
        for _ in 0..22 {
            v = Value::Array([v, Value::I64(1)].into_iter().collect());
        }
        let started = Instant::now();
        let r = j.cast_value(&env, v);
        assert!(format!("{r}").contains("no member admits it"), "{r}");
        assert!(started.elapsed() < Duration::from_secs(2), "{:?}", started.elapsed());
    }
}
