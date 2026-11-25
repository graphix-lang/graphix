use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG, PrintFlag,
    abstract_value::{self, CastHook},
    env::Env,
    errf,
    expr::union_members,
    format_with_flags, list, rep,
    stack::ensure_sufficient,
    typ::{
        AbstractId, Bound, CoreTrait, MapType, RefHist, RefKey, TVar, Type, TypeRef,
        Unfolded,
        setops::{union_identical, union_identical_in},
        tval::{NakedPrefix, PlainScalar, has_form, member_of},
        tvar::Judgment,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::{BitFlags, bitflags, make_bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::{Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    borrow::Cow, collections::hash_map::Entry, convert::Infallible, fmt, mem,
    ops::ControlFlow, sync::OnceLock,
};
use triomphe::Arc;

// CR claude for claude: [structure/low] This file is four subsystems under one name:
// the cast walk and its conversions; the `Verdicts` searches behind `holds_fn`,
// `holds_ref` and `data_holds`; the run-time type test (`is_a_int`,
// `ShallowScrutinee`); and the runtime-form judgments (`rep_collision`,
// `rep_ambiguity`, `compared_ref`, `map_refs`). They share `rep_head` and `ref_key` and
// little else, and a reader looking for select's test or the `Ordered` rule has no
// reason to open a file named for casts. Move the last three to files of their own
// beside this one (`holds.rs`, `is_a.rs`, `form.rs`) with their tests; no code changes.
// Found by reading; not run. (t-cast-12)
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

/// How many values deep a cast reads: data nested deeper is refused, so
/// foreign data bounds neither the walk's stack nor its memory.
pub const MAX_DATA_DEPTH: usize = 1024;

/// The data a cast reads.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CastInput {
    Data,
    /// JSON or TOML: an abstract value is an object of one entry keyed by
    /// its type's name, except under `Transparent`; with `exact_floats`
    /// a decimal stands for a float read exactly.
    Text {
        exact_floats: bool,
    },
    Marked,
}

/// A cast in progress: the refs expanded on the way to each value.
struct CastCx {
    hist: LPooled<AHashSet<(RefKey, usize)>>,
    /// Each reference's expansion, by definition and params allocation,
    /// with the params held so the allocation stays theirs.
    expanded: LPooled<AHashMap<(usize, usize), (Arc<[Type]>, Type)>>,
    /// Each union's reading of a value inside another union's, by the
    /// union's members, the value's address and `holds`, with the union
    /// held so the members stay its: a union tries several members over
    /// one value, so without it a failing read is exponential in the
    /// data's depth.
    read: LPooled<AHashMap<(usize, usize, Holds), (Type, Cast, NumReads)>>,
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
    /// How much of a value a conversion may lose: a union tries its
    /// members at each in turn, the least first.
    holds: Holds,
    /// Whether the cast turned a conversion down as an overflow
    /// (`overflowed`): only then can the last pass read anything.
    overflow_refused: bool,
    /// How the reading so far took the numbers it met.
    nums: NumReads,
    /// The last text the walk parsed and the scalar it parses as, the
    /// text held so its address names it.
    text: Option<(ArcStr, Option<Value>)>,
    input: CastInput,
    check_keys: bool,
}

/// How much of a value a reading keeps, the most first: all of it, all of
/// it rounded to a real member's precision (an f64 into an f32, a decimal
/// or a long integer into an f64), less (a fraction an integer drops), or
/// none: a finite number read as an infinity, tried after every member
/// failed at the others.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Holds {
    Exactly,
    Rounded,
    Partly,
    Overflowed,
}

impl Holds {
    const KEEPING: [Holds; 3] = [Holds::Exactly, Holds::Rounded, Holds::Partly];
    const ALL: [Holds; 4] =
        [Holds::Exactly, Holds::Rounded, Holds::Partly, Holds::Overflowed];
}

/// Which side of the line between integers and reals a number is on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NumKind {
    Integer,
    Real,
}

impl NumKind {
    fn of(t: Typ) -> Option<NumKind> {
        match (t.is_integer(), t.is_real()) {
            (true, _) => Some(NumKind::Integer),
            (_, true) => Some(NumKind::Real),
            _ => None,
        }
    }
}

/// How a reading took the numbers it met, values or text that reads as
/// one: how many it read across the line between integers and reals, and
/// how many as no number.
#[derive(Debug, Clone, Copy, Default)]
struct NumReads {
    crossed: usize,
    as_other: usize,
}

impl NumReads {
    /// Count a value of the kind `kind` says read as `c`.
    fn read(&mut self, c: &Value, kind: impl FnOnce() -> Option<NumKind>) {
        let to = NumKind::of(Typ::get(c));
        match (kind(), to) {
            (None, _) => (),
            (Some(k), Some(to)) if k == to => (),
            (Some(_), Some(_)) => self.crossed += 1,
            (Some(_), None) => self.as_other += 1,
        }
    }

    fn add(&mut self, d: NumReads) {
        self.crossed += d.crossed;
        self.as_other += d.as_other;
    }

    fn since(self, base: NumReads) -> NumReads {
        NumReads {
            crossed: self.crossed - base.crossed,
            as_other: self.as_other - base.as_other,
        }
    }

    /// Whether every number kept its kind.
    fn kept(self) -> bool {
        self.crossed == 0 && self.as_other == 0
    }
}

/// A union's reading of a value among those its members offer, in its
/// order: the first, except that in a round that drops no part one reading
/// a number across the line between integers and reals gives way to a
/// later one keeping every number its kind.
struct Choice {
    whole: bool,
    got: Option<(Value, NumReads)>,
}

impl Choice {
    fn new(cx: &CastCx) -> Self {
        Choice { whole: cx.holds < Holds::Partly, got: None }
    }

    fn offer(&mut self, c: Value, d: NumReads) {
        let replaces = match &self.got {
            None => true,
            Some((_, g)) => self.whole && g.crossed > 0 && d.kept(),
        };
        if replaces {
            self.got = Some((c, d))
        }
    }

    /// Whether no later reading would replace the one held.
    fn settled(&self) -> bool {
        self.got.as_ref().is_some_and(|(_, g)| !self.whole || g.crossed == 0)
    }

    /// The reading chosen, its count added to `cx`'s.
    fn take(self, cx: &mut CastCx) -> Option<Value> {
        self.got.map(|(c, d)| {
            cx.nums.add(d);
            c
        })
    }
}

impl CastCx {
    fn new(input: CastInput, check_keys: bool) -> Self {
        CastCx {
            hist: LPooled::take(),
            expanded: LPooled::take(),
            read: LPooled::take(),
            in_union: false,
            built: 0,
            cuts: 0,
            at: 0,
            depth: 0,
            holds: Holds::Overflowed,
            overflow_refused: false,
            nums: NumReads::default(),
            text: None,
            input,
            check_keys,
        }
    }

    /// The scalar the text `v` parses as ([`parsed_scalar`]), parsed once
    /// however many members of a union read it; `None` for a value that
    /// is no text.
    fn scalar(&mut self, v: &Value) -> Option<Value> {
        let Value::String(s) = v else { return None };
        if let Some((t, p)) = &self.text
            && ArcStr::ptr_eq(t, s)
        {
            return p.clone();
        }
        let p = parsed_scalar(s);
        self.text = Some((s.clone(), p.clone()));
        p
    }

    /// `t`'s reading of `v` and how it took the numbers it met, the count
    /// left as it was: a union adds only its chosen reading's.
    fn trial(
        &mut self,
        env: &Env,
        t: &Type,
        src: Option<&Type>,
        v: &Value,
    ) -> (Cast, NumReads) {
        let base = self.nums;
        let r = t.cast_int(env, self, src, v);
        let d = self.nums.since(base);
        self.nums = base;
        (r, d)
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
            Value::Decimal(_)
                if self.input == (CastInput::Text { exact_floats: true }) =>
            {
                convert(v.clone(), Typ::F64)
            }
            _ => None,
        }
    }
}

/// An `is_a` walk's state: each reference with each value it met, on
/// the walk's path or decided, once a walk; each reference's expansion,
/// made once a walk; `cuts` counts the path repeats, a verdict reached
/// past one kept for that path alone. Within a walk a reference is its
/// definition and its params' allocation, and the expansions held keep
/// those allocations alive; every value a walk meets is borrowed from its
/// root, so an address is one value.
#[derive(Default)]
struct IsAHist {
    met: AHashMap<((usize, usize), usize), Met>,
    expanded: AHashMap<(usize, usize), (Arc<[Type]>, Option<Type>)>,
    cuts: usize,
}

/// A reference met at a value: being decided on the path, or decided.
#[derive(Clone, Copy)]
enum Met {
    OnPath,
    Decided(bool),
}

fn ref_key(tr: &TypeRef) -> Option<RefKey> {
    tr.def_key().map(|k| (k, tr.params.clone()))
}

/// A type reference as a walk deciding a property that composes over a
/// type's parts keys it: its definition and whether each parameter has
/// the property: exact, and one of finitely many per definition.
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
    /// What a search found under the shared nodes it walked, and whether
    /// it looked a type reference up there, so a type that shares them (a
    /// DAG) is searched once: each node is held, so its address stays its
    /// own while the walk lasts.
    walked: LPooled<AHashMap<usize, (Type, bool, bool)>>,
    /// The judgment a ground cell keeps of this walk's finding beneath it.
    judgment: Option<Judgment>,
    /// Whether the walk looked a type reference up, whose expansion the
    /// env decides.
    looked_up: bool,
    /// The open cells the walk is beneath, by address: one that flows
    /// into itself holds there nothing it does not hold outside.
    beneath: SmallVec<[usize; 4]>,
}

impl<T> Verdicts<T> {
    fn new() -> Self {
        Self {
            known: LPooled::take(),
            pending: LPooled::take(),
            depth: 0,
            read: usize::MAX,
            walked: LPooled::take(),
            judgment: None,
            looked_up: false,
            beneath: SmallVec::new(),
        }
    }

    /// A walk whose finding beneath a ground cell the cell keeps as `j`.
    fn judging(j: Judgment) -> Self {
        Self { judgment: Some(j), ..Self::new() }
    }

    /// `search` beneath `t`, once a walk where it is a shared node. What
    /// it finds is found for good; that it finds nothing is kept only
    /// where it read no application still being decided, whose verdict
    /// it would rest on. Inside an application being decided only an
    /// application is kept: its parameters are all searched at every
    /// meeting, and the composites there are the definition's own.
    fn search_once(&mut self, t: &Type, search: impl FnOnce(&mut Self) -> bool) -> bool {
        let Some(node) = super::node_addr(t) else { return search(self) };
        if let Some((_, found, looked_up)) = self.walked.get(&node) {
            self.looked_up |= *looked_up;
            return *found;
        }
        let outer = mem::replace(&mut self.read, usize::MAX);
        let looked_outer = mem::replace(&mut self.looked_up, false);
        let found = search(self);
        let looked_up = self.looked_up;
        self.looked_up |= looked_outer;
        let kept = self.depth == 0 || matches!(t, Type::Ref(_));
        if kept && (found || self.read >= self.depth) {
            self.walked.insert(node, (t.clone(), found, looked_up));
        }
        self.read = self.read.min(outer);
        found
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
                let at = seen.pair_at(env, &p, &r);
                walk(seen, at, false, |seen| {
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
                        (Type::Tuple(ps), Type::Tuple(rs)) if ps.len() == rs.len() => {
                            ps.iter().zip(rs.iter()).any(|(p, r)| go(env, seen, p, r))
                        }
                        (Type::Array(p), Type::Array(r))
                        | (Type::List(p), Type::List(r))
                        | (Type::Error(p), Type::Error(r))
                        | (Type::ByRef(_, p), Type::ByRef(_, r)) => go(env, seen, p, r),
                        (Type::Struct(ps), Type::Struct(rs)) => {
                            ps.iter().any(|(n, p, _)| {
                                rs.iter().any(|(m, r, _)| n == m && go(env, seen, p, r))
                            })
                        }
                        (Type::Variant(pt, ps, _), Type::Variant(rt, rs, _))
                            if pt == rt && ps.len() == rs.len() =>
                        {
                            ps.iter().zip(rs.iter()).any(|(p, r)| go(env, seen, p, r))
                        }
                        (Type::Map(p), Type::Map(r)) => {
                            go(env, seen, &p.key, &r.key)
                                || go(env, seen, &p.value, &r.value)
                        }
                        _ => false,
                    }
                })
            })
        }
        self.holds_ref(env) && go(env, &mut RepSeen::default(), self, reaching)
    }

    /// Whether `_`, bottom, stands in this type outside a function type,
    /// through the definitions it names.
    pub fn holds_bottom(&self, env: &Env) -> bool {
        self.holds(env, &mut Verdicts::new(), &|t| match t {
            Type::Bottom => Some(true),
            Type::Fn(_) => Some(false),
            _ => None,
        })
    }

    /// Whether the type holds `Any` or an open cell anywhere: a part a
    /// value-level predicate cannot be decided of.
    #[doc(hidden)]
    pub fn holds_unknown(&self, env: &Env) -> bool {
        self.holds_unknown_in(env, &mut Verdicts::judging(Judgment::HoldsUnknown))
    }

    /// Whether a value of this type can hold a function outside an
    /// abstract type: a runtime type test tells a function from a value
    /// but not one signature from another.
    #[doc(hidden)]
    pub fn holds_fn(&self, env: &Env) -> bool {
        self.holds_fn_in(env, &mut Verdicts::judging(Judgment::HoldsFn))
    }

    fn holds_fn_in(&self, env: &Env, seen: &mut Verdicts<bool>) -> bool {
        self.holds(env, seen, &|t| match t {
            Type::Fn(_) => Some(true),
            Type::Abstract { .. } => Some(false),
            _ => None,
        })
    }

    fn holds_unknown_in(&self, env: &Env, seen: &mut Verdicts<bool>) -> bool {
        self.holds(env, seen, &|t| match t {
            Type::Any => Some(true),
            Type::TVar(tv) if tv.binding().is_none() => Some(true),
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
        self.holds(env, &mut Verdicts::judging(Judgment::HoldsAbstract), &|t| match t {
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
            #[cfg(test)]
            HOLDS_STEPS.with(|n| n.set(n.get() + 1));
            if let Some(v) = leaf(self) {
                return v;
            }
            match self {
                // with no application in progress, what a ground cell's
                // binding holds rests on nothing the walk assumed
                Type::TVar(tv)
                    if seen.depth == 0
                        && let Some(j) = seen.judgment
                        && let Some(found) = tv.ground_holds(j, |b| {
                            let outer = mem::replace(&mut seen.looked_up, false);
                            let found = b.holds(env, seen, leaf);
                            let keep = !seen.looked_up;
                            seen.looked_up |= outer;
                            (found, keep)
                        }) =>
                {
                    found
                }
                // an open cell will hold what bounds it from below
                Type::TVar(tv) => match self.deref_cloned() {
                    Some(t) => t.holds(env, seen, leaf),
                    None => tv.open_cell().is_some_and(|c| {
                        let addr = c.cell_addr();
                        if seen.beneath.contains(&addr) {
                            return false;
                        }
                        seen.beneath.push(addr);
                        let found =
                            c.cell_lower().iter().any(|l| l.holds(env, seen, leaf));
                        seen.beneath.pop();
                        found
                    }),
                },
                // an open constructor is unknown like an open cell: its
                // argument decides
                Type::App(c, a) => match Type::app_filled(c, a) {
                    Some(f) => f.holds(env, seen, leaf),
                    None => a.holds(env, seen, leaf),
                },
                Type::Ref(tr) => seen.search_once(self, |seen| {
                    seen.looked_up = true;
                    // CR claude for claude: [perf/medium] The typedef is expanded here
                    // before `decide_once` looks its definition up, so a reference to a
                    // definition already being decided or decided pays a walk of the
                    // whole body for nothing. A union of N structs that each name the
                    // union is expanded N times in one walk: the check of 500 calls
                    // passing such a value takes 0.8 s, 2.5 s and 9.1 s at 150, 300 and
                    // 600 variants, and the samples sit in `replace_tvars` under this
                    // lookup. Compute `shape`, call `decide_once`, and look the
                    // definition up inside its closure. probe:
                    // design/review-2026-11-16/repro/c-callsite-unread-walk-per-call.sh
                    // (c-callsite-06)
                    let Ok(t) = self.lookup_ref(env) else { return false };
                    let shape =
                        tr.params.iter().map(|p| p.holds(env, seen, leaf)).collect();
                    match tr.def_key() {
                        None => t.holds(env, seen, leaf),
                        Some(k) => decide_once(seen, (k, shape), false, |seen| {
                            t.holds(env, seen, leaf)
                        }),
                    }
                }),
                t => seen.search_once(t, |seen| {
                    let mut r = false;
                    t.for_each_child(&mut |c| r = r || c.holds(env, seen, leaf));
                    r
                }),
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
        // CR claude for eric: [bug/medium] This bound guards foreign data, but it also
        // refuses a value the program built that is already of the target: cast<L>(a)
        // over a 1100-deep `type L = [`Nil, `Cons(i64, L)]` answers InvalidCast, while
        // `select a { L as _ => .. }` over the same value takes the arm. A cast and a
        // type test disagree on one value. The refusal also names the leaf where the
        // count ran out ("can't cast 76 to i64"), not the value or the limit. Decide
        // whether an in-language cast is bounded at all. If it is, the type test should
        // share the bound and the message should name the limit. probe:
        // design/review-2026-11-16/repro/t-rep-cast-refuses-deep-own-value.gx
        // (t-rep-03)
        // CR claude for eric: [bug/medium] This bound also refuses a program's own
        // value: `cast<L>(v)` of a `v: L` 1,100 conses deep (`` type L = [`Cons(i64,
        // L), `Nil] ``) is an `InvalidCast`, where a select reads the same value.
        // `pack::write_bytes` writes such a value and `pack::read` then refuses it, so
        // the round trip fails; json's writer has a bound of its own, 128. No doc a
        // program's author reads states either. Decide the rule: the writers refuse
        // past the bound the readers have (as `abstract_value::nested` does for
        // abstract values) and data.md says so, or the bound is for bytes being decoded
        // and not for a value already built. probe:
        // design/review-2026-11-16/repro/t-cast-depth-limit-roundtrip.gx (t-cast-04)
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
            Type::Abstract { .. } => {
                self.cast_abstract(env, cx, v, matches!(cx.input, CastInput::Text { .. }))
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
                // the value's class, then the class of the scalar text parses
                // as, then the rest; a real value, or text that parses as one,
                // tries the reals of each first
                let parsed = match s.intersects(text_scalars()) {
                    true => cx.scalar(v),
                    false => None,
                };
                let like = parsed.as_ref().unwrap_or(v);
                let own = own_class(v);
                let as_parsed =
                    parsed.as_ref().map_or(BitFlags::empty(), own_class) & !own;
                let reals = match Typ::get(like).is_real() {
                    true => Typ::real(),
                    false => BitFlags::empty(),
                };
                let kind = || NumKind::of(Typ::get(like));
                let to = |t| convert_text(v.clone(), t, |_| parsed.clone());
                let mut refused = false;
                let read = [own, as_parsed, !own & !as_parsed]
                    .into_iter()
                    .find_map(|class| {
                        let tier = *s & class;
                        let members = (tier & reals).iter().chain((tier & !reals).iter());
                        let mut exact = members.clone().filter_map(|t| {
                            to(t).filter(|c| holds_exactly(v, parsed.as_ref(), c))
                        });
                        exact
                            .next()
                            .or_else(|| {
                                (cx.holds >= Holds::Rounded)
                                    .then(|| {
                                        members.clone().filter(|t| t.is_real()).find_map(
                                            |t| {
                                                to(t).filter(|c| {
                                                    holds_rounded(v, parsed.as_ref(), c)
                                                })
                                            },
                                        )
                                    })
                                    .flatten()
                            })
                            .or_else(|| {
                                (cx.holds >= Holds::Partly)
                                    .then(|| {
                                        members.clone().find_map(|t| {
                                            to(t).filter(|c| {
                                                let o = overflowed(v, parsed.as_ref(), c);
                                                refused |= o;
                                                !o
                                            })
                                        })
                                    })
                                    .flatten()
                            })
                    })
                    .or_else(|| {
                        (refused && cx.holds == Holds::Overflowed)
                            .then(|| {
                                [own, as_parsed, !own & !as_parsed].into_iter().find_map(
                                    |class| {
                                        let tier = *s & class;
                                        (tier & reals)
                                            .iter()
                                            .chain((tier & !reals).iter())
                                            .find_map(to)
                                    },
                                )
                            })
                            .flatten()
                    });
                cx.overflow_refused |= refused;
                match read {
                    Some(c) => {
                        if cx.in_union {
                            cx.nums.read(&c, kind)
                        }
                        Ok(Some(c))
                    }
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
            Type::Map(m) => {
                let MapType { key, value } = &**m;
                let src = src_head(cx, src, env, v);
                let (ks, vs) = match &src {
                    Some(Type::Map(s)) => (Some(&s.key), Some(&s.value)),
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
                if !cx.hist.insert(key.clone()) {
                    cx.cuts += 1;
                    return Err(
                        self.cast_fail("the type recurses without consuming it", v)
                    );
                }
                let r = t.cast_int(env, cx, src, v);
                cx.hist.remove(&key);
                r
            }
            // The members of the value's class come first: one it already
            // inhabits, a tuple or a struct its unmarked data reads back as,
            // then the rest of the class; then, for text, the members of the
            // class of the scalar it parses as; then the rest. Within each, a
            // member that holds the value exactly comes before one that holds
            // it rounded to a real's precision, and that before one that
            // would drop part of it (a fraction, a sub-second part)
            // (`Holds`); short of a dropped part, one keeping every number an
            // integer or a real as it was before one reading one across
            // (`Choice`). An infinity a finite value overflows to comes after
            // every group.
            Type::Set(ts) => {
                let key = (cx.in_union && cx.built == 0)
                    .then(|| (ts.as_ptr().addr(), (v as *const Value).addr(), cx.holds));
                if let Some(k) = &key
                    && let Some((_, r, d)) = cx.read.get(k)
                {
                    let (r, d) = (r.clone(), *d);
                    cx.nums.add(d);
                    return r;
                }
                let base = cx.nums;
                let (in_union, cuts) = (mem::replace(&mut cx.in_union, true), cx.cuts);
                let array = rep::array_elems(v).is_some()
                    && (cx.input == CastInput::Marked
                        || matches!(src_head(cx, src, env, v), Some(Type::Array(_))));
                let r = self.cast_member(env, cx, src, v, ts, array);
                cx.in_union = in_union;
                if let Some(k) = key
                    && cx.cuts == cuts
                {
                    cx.read.insert(k, (self.clone(), r.clone(), cx.nums.since(base)));
                }
                r
            }
        }
    }

    /// The abstract type `self`'s reading of `v`. Text (`wrapped`) holds
    /// it as an object of one entry keyed by the type's name, the data
    /// inside; `Transparent<T>` holds what `T` holds there without the
    /// object. A written `from_value` reads the data inside as a cast of
    /// data does, nested abstract values bare, as the writer writes them.
    fn cast_abstract(
        &self,
        env: &Env,
        cx: &mut CastCx,
        v: &Value,
        wrapped: bool,
    ) -> Cast {
        let Type::Abstract { id, params } = self else {
            unreachable!("an abstract type")
        };
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
        let wrap = |payload: Option<Value>, data: &Value| {
            Ok(Some(abstract_value::wrap(
                *id,
                rep.name.clone(),
                params.clone(),
                payload.unwrap_or_else(|| data.clone()),
            )))
        };
        let inner = || rep.instantiate_with(params);
        if wrapped && *id == AbstractId::transparent() {
            let inner = inner();
            let payload = match rep_head(env, &inner).as_deref() {
                Some(t @ Type::Abstract { id, .. }) => {
                    t.cast_abstract(env, cx, v, *id == AbstractId::transparent())?
                }
                _ => inner.cast_int(env, cx, None, v)?,
            };
            return wrap(payload, v);
        }
        let v = match wrapped {
            false => v,
            true => match unwrapped(&rep.name, v) {
                Some(inside) => inside,
                None => {
                    return Err(self.cast_fail(
                        format!("expected an object whose one key is {}", rep.name),
                        v,
                    ));
                }
            },
        };
        // CR claude for eric: [risk/medium] `from_value` is handed `v` as its reader
        // holds it: marked by pack and db, stripped data by json, toml, netidx and
        // `cast`. So one `Cast` impl sees `(5, `Q)` under pack and `[5, "Q"]` under
        // JSON. A from_value that tests the shape its own to_value wrote (`select v {
        // (i64, `Q) as (x, _) => .. }`) reads back from pack and fails from JSON.
        // Decide: either hand from_value one form (strip `v` with `rep::strip` before
        // the hook, so every reader agrees), or state on the `Cast` trait in core's
        // mod.gxi that from_value must `cast` its argument and never test its shape.
        // probe: design/review-2026-11-16/repro/x-kind-markers-fromvalue-two-forms.gx
        // (x-kind-markers-05)
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
                wrap(inner().cast_int(env, cx, None, data)?, data)
            }
        }
    }

    /// The union `ts`'s reading of `v`, as `cast_int` takes one, over its
    /// members past every union in them ([`members_by_class`]): when `v`
    /// is statically an array (`array`), an array member it inhabits; else
    /// a member whose tag it carries; else the members of `v`'s class (for
    /// unmarked data a tuple of its length and then a struct first), then
    /// for text the members of the class of the scalar it parses as, then
    /// the rest, each group as [`Self::tier`] reads it.
    fn cast_member(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
        ts: &[Type],
        array: bool,
    ) -> Cast {
        let own = match rep::kind(v) {
            None => own_class(v),
            Some(_) => BitFlags::empty(),
        };
        let mut members: SmallVec<[Cow<Type>; 8]> = SmallVec::new();
        let by_class = |classes, out: &mut SmallVec<_>| {
            let mut named: LPooled<Vec<RefKey>> = LPooled::take();
            for t in ts.iter() {
                members_by_class(env, Cow::Borrowed(t), out, &mut named, 0)
            }
            merge_primitives(classes, out)
        };
        by_class([own, BitFlags::empty()], &mut members);
        // text parses only where a member may read the scalar it parses as
        let parsed = match v {
            Value::String(_) if members.iter().any(|t| takes_scalar(env, t)) => {
                cx.scalar(v)
            }
            _ => None,
        };
        let as_parsed = parsed.as_ref().map_or(BitFlags::empty(), own_class) & !own;
        if !as_parsed.is_empty() {
            members.clear();
            by_class([own, as_parsed], &mut members);
        }
        let ts = || members.iter().map(|t| &**t);
        let converts = cx.input == CastInput::Marked;
        let arrays: SmallVec<[&Type; 4]> = match array {
            false => SmallVec::new(),
            true => ts()
                .filter(|t| match rep_head(env, t).as_deref() {
                    Some(Type::Array(_)) => true,
                    Some(Type::Primitive(p)) => p.contains(Typ::Array),
                    _ => false,
                })
                .collect(),
        };
        // CR claude for eric: [bug/low] A value whose static type is an array is still
        // read as a writer's data when its elements need a conversion: this tier offers
        // none unless the input is `Marked`, so the cast falls to the tuple and struct
        // readings. `cast<[(i64, i64), Array<i64>]>` of an `Array<f64>` is the tuple
        // `(1, 2)` for two elements and the array `[1, 2, 3]` for three, and the array
        // `[1, 2]` for an `Array<i64>`: the kind of the result follows the data. Where
        // the source's element type is known (not `Any`) this tier could convert, as it
        // does for `Marked` input. probe:
        // design/review-2026-11-16/repro/t-cast-static-array-reads-as-tuple.gx
        // (t-cast-06)
        if let Ok(r) = self.tier(env, cx, src, v, &[&arrays], converts, &Holds::KEEPING) {
            return Ok(r);
        }
        // a member whose tag the value carries is its reading, whatever a
        // tagless reading of an earlier member says
        let tagged: SmallVec<[&Type; 4]> =
            ts().filter(|t| carries_tag(env, t, v)).collect();
        // CR claude for claude: [bug/medium] When unmarked data starts with a string
        // that is a member's tag and has that member's arity, the tagged member is the
        // only reading tried: if its payload does not fit, the cast fails without
        // trying a member the value already inhabits. `["A", "x"]` cast to `` [`A(i64),
        // Array<string>] `` from `Any`, by `str::parse` or as a subscriber reads it is
        // an `InvalidCast` though it is an `Array<string>`, and beside `(string,
        // string)` it fails where `["B", "x"]` reads as the tuple. The tag gives its
        // member priority, not the only say: on a refusal fall through to the tiers
        // below and return the tagged refusal only if none reads the value. probe:
        // design/review-2026-11-16/repro/t-cast-tagged-data-no-fallback.gx (t-cast-03)
        if !tagged.is_empty() {
            return self
                .tier(env, cx, src, v, &[&tagged], true, &Holds::ALL)
                .map_err(|e| e.expect("a tagged member was tried"));
        }
        // a writer wrote the tuple or the struct the data reads back as
        let rank = |t: &Type| match (v, rep_head(env, t).as_deref()) {
            (Value::Array(a), Some(Type::Tuple(es)))
                if rep::kind(v).is_none()
                    && es.len() == a.len()
                    && es.iter().zip(a.iter()).all(|(e, x)| in_own_class(env, e, x)) =>
            {
                Some(0)
            }
            (Value::Array(a), Some(Type::Struct(fs)))
                if rep::kind(v).is_none() && fields_in_own_class(env, fs, a) =>
            {
                Some(1)
            }
            _ => None,
        };
        let mut groups: [SmallVec<[&Type; 4]>; 5] = Default::default();
        for t in ts() {
            let g = match rank(t) {
                Some(r) => r,
                None if in_own_class(env, t, v) => 2,
                None if parsed.as_ref().is_some_and(|p| in_own_class(env, t, p)) => 3,
                None => 4,
            };
            groups[g].push(t)
        }
        let [tuples, structs, own, as_parsed, rest] = groups;
        for tier in [&[&tuples, &structs, &own][..], &[&as_parsed], &[&rest]] {
            if let Ok(r) = self.tier(env, cx, src, v, tier, true, &Holds::KEEPING) {
                return Ok(r);
            }
        }
        let none = SmallVec::new();
        let arrays = if converts { &arrays } else { &none };
        let last = [arrays, &tuples, &structs, &own, &as_parsed, &rest];
        if cx.overflow_refused
            && let Ok(r) = self.tier(env, cx, src, v, &last, true, &[Holds::Overflowed])
        {
            return Ok(r);
        }
        // CR claude for claude: [bug/medium] A union that reads no member answers this
        // and drops what its members said: `tier` returns a refusal and every caller
        // but the tagged one discards it. So `cast<[Sorted, null]>` and a json read of
        // an optional `Sorted` field lose the reason its `from_value` gave ("not
        // sorted"), which design/abstract_cast.md says the cast's error repeats, and a
        // bad field deep inside an optional struct is reported as the whole value
        // against the whole union. Give `CastFail` the depth it was raised at and
        // report the deepest refusal tried (the first on a tie), this text only when no
        // member was tried. `CastFail`'s doc says a union keeps its last failure; it
        // keeps none. probe:
        // design/review-2026-11-16/repro/t-cast-union-drops-refusal.gx (t-cast-02)
        Err(self.cast_fail("no member admits it", v))
    }

    /// The reading of `v` by the members `groups` hold, each of `rounds`
    /// up to `cx.holds` in turn: a member that holds `v` as it is, else what the
    /// round's [`Choice`] took, offered each member's conversion (only
    /// where `converts`) in order, a group at a time, the next group only
    /// where the choice may still change. `Err` when no round read it,
    /// with the last refusal when a member was tried.
    fn tier(
        &self,
        env: &Env,
        cx: &mut CastCx,
        src: Option<&Type>,
        v: &Value,
        groups: &[&SmallVec<[&Type; 4]>],
        converts: bool,
        rounds: &[Holds],
    ) -> std::result::Result<Option<Value>, Option<CastFail>> {
        if groups.iter().all(|g| g.is_empty()) {
            return Err(None);
        }
        let outer = cx.holds;
        let mut refused = None;
        for &holds in rounds {
            if holds > outer {
                break;
            }
            cx.holds = holds;
            let mut choice = Choice::new(cx);
            for group in groups.iter() {
                for t in group.iter() {
                    match cx.trial(env, t, src, v) {
                        (Ok(None), d) => {
                            cx.holds = outer;
                            cx.nums.add(d);
                            return Ok(None);
                        }
                        (Ok(Some(c)), d) if converts => choice.offer(c, d),
                        (Ok(Some(_)), _) => (),
                        (Err(e), _) => refused = Some(e),
                    }
                }
                if choice.settled() {
                    break;
                }
            }
            cx.holds = outer;
            if let Some(c) = choice.take(cx) {
                return Ok(Some(c));
            }
        }
        Err(refused)
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

    // CR claude for claude: [dead/low] Nothing calls this in either repository:
    // `TypeCast::update` and the fused cast site call `cast_with`.
    // design/distinct_composite_forms.md still says both engines' casts call it, and
    // design/strict_fusion.md and design/tvar_constraints.md cite it. Delete it, move
    // its sentence on what a static source decides to `cast_with`, and point the three
    // docs there. Found by reading; not run. (t-cast-10)
    // 2026-11-19 claude: On bounds-spike cast_from is deleted (it had no caller) and
    // its sentence on what a static source decides is now in cast_with's doc, where
    // this CR moved. Left: the three design docs that cite cast_from.

    /// A cast with its input format and static map-key check decision.
    /// `check_keys` may be false only when the target contains neither Any
    /// nor an abstract type whose representation a cast can enter. A
    /// static source `src` decides whether an unmarked array is an array or
    /// a writer's data: a union reads an array as an array member it
    /// inhabits first.
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
                match hist.met.entry(key) {
                    Entry::Occupied(e) => match *e.get() {
                        Met::Decided(r) => return r,
                        Met::OnPath => {
                            hist.cuts += 1;
                            return false;
                        }
                    },
                    Entry::Vacant(e) => {
                        e.insert(Met::OnPath);
                    }
                }
                #[cfg(test)]
                IS_A_NAMES.with(|n| n.set(n.get() + 1));
                let cuts = hist.cuts;
                let r = t.is_a_int(env, hist, flags, v);
                match hist.cuts == cuts {
                    true => hist.met.insert(key, Met::Decided(r)),
                    false => hist.met.remove(&key),
                };
                r
            }
            Type::Primitive(t) => rep::kind(v).is_none() && t.contains(Typ::get(&v)),
            Type::Abstract { id, params } => match v {
                Value::Abstract(a) => {
                    match a.downcast_ref::<crate::abstract_value::GxAbstract>() {
                        Some(g) => {
                            g.id == *id
                                && (flags.contains(IsAFlags::MatchAbstract)
                                    || g.params.len() == params.len()
                                    // parameters are invariant: each one equal,
                                    // but a ⊥ the representation only reads is
                                    // below every type, as the check reads it
                                    && params.iter().zip(g.params.iter()).enumerate().all(
                                        |(i, (p, gp))| {
                                            let holds = |a: &Type, b: &Type| {
                                                a.decided_holds(env, b).unwrap_or(false)
                                            };
                                            super::bounds::bottom_below(env, id, i, gp)
                                                || (holds(p, gp) && holds(gp, p))
                                        },
                                    ))
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
            Type::Map(m)
                if matches!(m.key, Type::Any)
                    && matches!(m.value, Type::Any)
                    && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Map(_))
            }
            Type::Map(t) => match v {
                Value::Map(m) => m.into_iter().all(|(k, v)| {
                    t.key.is_a_int(env, hist, flags, k)
                        && t.value.is_a_int(env, hist, flags, v)
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

    /// `self`, a written test, with every ⊥ outside a function type, a
    /// cell bound to it included, as `Any`; `None` where none holds one.
    /// A test passes every value at either, but a union keeps `Any` where
    /// it drops ⊥: `['b, null]`, `'b` the test's own variable left at ⊥,
    /// would read as `null` alone, under a name (`Opt<'b>` over `type
    /// Opt<'x> = ['x, null]`) in the node-walk's test and anywhere in a
    /// fused arm's layout of what it binds.
    pub fn blind_bottoms(&self) -> Option<Type> {
        /// The shared nodes holding nothing to change, those of 16 nodes
        /// or more: a small test takes no table.
        struct Clean {
            nodes: super::Lazy<AHashSet<usize>>,
            steps: u32,
        }
        fn go(t: &Type, clean: &mut Clean) -> Option<Type> {
            clean.steps += 1;
            ensure_sufficient(|| match t {
                Type::Bottom => Some(Type::Any),
                Type::TVar(tv) => go(&tv.binding()?, clean),
                Type::Fn(_) => None,
                t => {
                    let node = super::node_addr(t);
                    if let Some(n) = &node
                        && clean.nodes.get().is_some_and(|c| c.contains(n))
                    {
                        return None;
                    }
                    let from = clean.steps;
                    let blind = t.cow_children(&mut |c| go(c, clean));
                    if blind.is_none()
                        && clean.steps - from >= 16
                        && let Some(n) = node
                    {
                        clean.nodes.get_mut().insert(n);
                    }
                    blind
                }
            })
        }
        go(self, &mut Clean { nodes: super::Lazy::new(), steps: 0 })
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
    /// apart, or no payload, or the one member of a shape is one `self`
    /// cannot match). Sound only for inferred predicates, which typecheck
    /// unified against their member; explicit `x as T` stays strict.
    pub fn shallow_discriminant(&self, env: &Env, scrutinee: &Type) -> Option<Type> {
        ShallowScrutinee::new(scrutinee.clone()).discriminant(env, self)
    }
}

/// The scrutinee and its referenced definitions must stay fixed while these facts are used.
#[derive(Debug)]
pub struct ShallowScrutinee {
    typ: Type,
    members: OnceLock<Option<SmallVec<[Type; 8]>>>,
    facts: OnceLock<Option<ScrutineeFacts>>,
}

/// The scrutinee's flattened members, and whether one holds `Any` below
/// its top where a pattern reads ([`holds_any`]).
#[derive(Debug)]
struct ScrutineeFacts {
    members: Box<[(Type, MemberFacts)]>,
    any: bool,
}

impl ScrutineeFacts {
    /// `p`, of the facts `pf`, shallowed against the member it meets. The
    /// shape decides `p` only where one member has it and `p` can match
    /// that member (an instance's scrutinee may be another member of
    /// `p`'s shape): `None` elsewhere.
    fn shallow(&self, env: &Env, p: &Type, pf: &MemberFacts) -> Option<Type> {
        let mut shaped = self.members.iter().filter(|(_, mf)| match &pf.arr {
            Some(pa) => mf.arr.as_ref().is_some_and(|ma| arr_overlap(pa, ma)),
            None => (pf.map && mf.map) || (pf.error && mf.error),
        });
        match (shaped.next(), shaped.next()) {
            (Some((m, _)), None) if p.could_pass(env, m).unwrap_or(false) => {
                Some(shallowify(env, p, self.any.then_some(m)))
            }
            _ => None,
        }
    }
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

    fn facts(&self, env: &Env) -> Option<&ScrutineeFacts> {
        self.facts
            .get_or_init(|| {
                let mut scrut: LPooled<Vec<Type>> = LPooled::take();
                flatten_union_members(&self.typ, env, &mut scrut)?;
                let below = |part: fn(&Type, &Env, &mut ShapeSeen) -> bool| {
                    let mut walked = ShapeSeen::default();
                    scrut.iter().any(|m| {
                        m.try_for_each_child(&mut |c| match part(c, env, &mut walked) {
                            true => ControlFlow::Break(()),
                            false => ControlFlow::Continue(()),
                        })
                        .is_break()
                    })
                };
                if below(shapes_collide) {
                    return None;
                }
                let any = below(holds_any);
                let members = scrut.iter().map(|m| (m.clone(), member_facts(m)));
                Some(ScrutineeFacts { members: members.collect(), any })
            })
            .as_ref()
    }

    pub fn discriminant(&self, env: &Env, predicate: &Type) -> Option<Type> {
        let mut preds: LPooled<Vec<Type>> = LPooled::take();
        flatten_union_members(predicate, env, &mut preds)?;
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
            out.push(sfacts.shallow(env, p, &pf)?);
        }
        Some(if out.len() == 1 {
            out.pop().unwrap()
        } else {
            Type::Set(Arc::from_iter(out.drain(..)))
        })
    }
}

/// What an [`any_part`] search has walked: the applications, and
/// the shared nodes, each held so its address stays its own.
#[derive(Default)]
struct ShapeSeen {
    walked: LPooled<AHashSet<RefKey>>,
    nodes: LPooled<AHashMap<usize, Type>>,
}

/// Whether `found` holds of `t` or of a part of it below no part it rules
/// out (`Some(false)`). An application is walked once a search, and so is
/// a shared node.
fn any_part(
    t: &Type,
    env: &Env,
    seen: &mut ShapeSeen,
    found: fn(&Type) -> Option<bool>,
) -> bool {
    #[cfg(test)]
    SHAPE_STEPS.with(|n| n.set(n.get() + 1));
    if let Some(addr) = super::node_addr(t) {
        match seen.nodes.entry(addr) {
            Entry::Occupied(_) => return false,
            Entry::Vacant(e) => {
                e.insert(t.clone());
            }
        }
    }
    ensure_sufficient(|| match t {
        Type::Ref(tr) => {
            let Some(key) = ref_key(tr) else { return false };
            if !seen.walked.insert(key) {
                return false;
            }
            let Ok(x) = t.lookup_ref(env) else { return false };
            any_part(&x, env, seen, found)
        }
        Type::TVar(tv) => tv.binding().is_some_and(|b| any_part(&b, env, seen, found)),
        t => found(t).unwrap_or_else(|| {
            t.try_for_each_child(&mut |c| match any_part(c, env, seen, found) {
                true => ControlFlow::Break(()),
                false => ControlFlow::Continue(()),
            })
            .is_break()
        }),
    })
}

/// Whether `t` or a part of it is a union two of whose members share a
/// shape (`arr_overlap`).
fn shapes_collide(t: &Type, env: &Env, seen: &mut ShapeSeen) -> bool {
    any_part(t, env, seen, |t| {
        let Type::Set(ts) = t else { return None };
        let shapes: LPooled<Vec<_>> = ts
            .iter()
            .filter_map(|m| m.with_deref(|m| m.and_then(|m| member_facts(m).arr)))
            .collect();
        shapes
            .iter()
            .enumerate()
            .any(|(i, a)| shapes[i + 1..].iter().any(|b| arr_overlap(a, b)))
            .then_some(true)
    })
}

/// Whether `t` is `Any` or holds it where a pattern reads: outside every
/// function type and reference.
fn holds_any(t: &Type, env: &Env, seen: &mut ShapeSeen) -> bool {
    any_part(t, env, seen, |t| match t {
        Type::Any => Some(true),
        Type::Fn(_) | Type::ByRef(..) => Some(false),
        _ => None,
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

/// `p` with each payload position as `Any`: its shape alone decides it.
/// `m` is the member it meets in a scrutinee holding `Any`: a position
/// keeps `p`'s part where `m`'s holds `Any` and `p`'s does not hold all of
/// `m`'s, since there the part is the arm's own, a test nothing else makes.
/// A part kept is shallowed against `m`'s in turn ([`shallow_part`]).
fn shallowify(env: &Env, p: &Type, m: Option<&Type>) -> Type {
    if !matches!(
        p,
        Type::Variant(..)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Array(_)
            | Type::List(_)
            | Type::Map(_)
            | Type::Error(_)
    ) {
        return p.clone();
    }
    let mut parts: SmallVec<[Type; 8]> = SmallVec::new();
    if let Some(m) = m {
        let aligned = match (p, m) {
            (Type::Variant(pt, ps, _), Type::Variant(mt, ms, _)) => {
                pt == mt && ps.len() == ms.len()
            }
            (Type::Tuple(ps), Type::Tuple(ms)) => ps.len() == ms.len(),
            (Type::Struct(ps), Type::Struct(ms)) => {
                ps.len() == ms.len() && ps.iter().zip(ms.iter()).all(|(p, m)| p.0 == m.0)
            }
            (p, m) => std::mem::discriminant(p) == std::mem::discriminant(m),
        };
        if !aligned {
            return p.clone();
        }
        let _ = m.try_for_each_child(&mut |c| {
            parts.push(c.clone());
            ControlFlow::<()>::Continue(())
        });
    }
    let mut parts = parts.iter();
    let blank = p.cow_children(&mut |pp| {
        let own = |mp: &&Type| {
            holds_any(mp, env, &mut ShapeSeen::default())
                && !(!mp.has_unbound() && pp.decided_holds(env, mp).unwrap_or(false))
        };
        match parts.next().filter(own) {
            Some(mp) => shallow_part(env, pp, mp),
            None => Some(Type::Any),
        }
    });
    blank.unwrap_or_else(|| p.clone())
}

/// `pp`, a part of a predicate that does not hold `mp`, the part of its
/// member it stands over, shallowed against the member of `mp` it meets;
/// `None` where its shape decides nothing there (`mp` is `Any` or open)
/// and the part stays whole. Only cells are looked through: a named
/// type is not expanded, so this ends with `pp`'s own structure.
fn shallow_part(env: &Env, pp: &Type, mp: &Type) -> Option<Type> {
    ensure_sufficient(|| {
        let pp = pp.deref_cloned()?;
        let pf = member_facts(&pp);
        if pf.exact {
            return None;
        }
        ShallowScrutinee::new(mp.clone()).facts(env)?.shallow(env, &pp, &pf)
    })
}

/// `t`'s members, each application expanded once, in the order first
/// met; `None` where one is open, `Any`, or recurses through unions
/// alone (`seen`, the path of definitions being expanded).
fn flatten_union_members(
    t: &Type,
    env: &Env,
    out: &mut LPooled<Vec<Type>>,
) -> Option<()> {
    fn go(
        t: &Type,
        env: &Env,
        out: &mut LPooled<Vec<Type>>,
        seen: &mut LPooled<Vec<RefKey>>,
        unfolded: &mut Unfolded,
    ) -> Option<()> {
        ensure_sufficient(|| match t {
            Type::Set(ts) => {
                for t in ts.iter() {
                    go(t, env, out, seen, unfolded)?;
                }
                Some(())
            }
            Type::Ref(tr) => {
                tr.resolve_in(env)?;
                let key = ref_key(tr)?;
                if seen.contains(&key) {
                    return None;
                }
                if !unfolded.first(t) {
                    return Some(());
                }
                let whole = t.lookup_ref(env).ok()?;
                seen.push(key);
                let res = go(&whole, env, out, seen, unfolded);
                seen.pop();
                res
            }
            Type::TVar(tv) => match tv.binding() {
                Some(t) => go(&t, env, out, seen, unfolded),
                None => None,
            },
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => go(&t, env, out, seen, unfolded),
                None => None,
            },
            Type::Any | Type::Hole => None,
            t => {
                out.push(t.clone());
                Some(())
            }
        })
    }
    go(t, env, out, &mut LPooled::take(), &mut Unfolded::default())
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
pub(super) fn shares_form(env: &Env, t: &Type) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        Some(Type::Abstract { id, params }) => {
            !params.is_empty() && !env.abstract_reps.contains_key(id)
        }
        None | Some(Type::Bottom) => false,
        Some(Type::Variant(_, ps, _)) if ps.is_empty() => false,
        Some(Type::Primitive(p)) => p.intersects(SHARED_PRIMS),
        Some(Type::Set(ts)) => ts.iter().any(|t| shares_form(env, t)),
        // CR claude for eric: [bug/medium] This arm makes every tuple, struct and
        // payload variant share its form, which was true before kind markers. Now a
        // written test of a composite checks each part at run time, so a value of an
        // open member passes it only if it is that type. A composite shares a form only
        // where one of its parts does. Today `|x: ['a, (i64, i64)]| select x { (i64,
        // i64) as (a, b) => .., _ => .. }` is refused "annotate it", with nothing a
        // declared 'a can be annotated with. The inferred `` `Tick(n) `` refusal's
        // advice (write `` `Tick(i64) as `Tick(n) ``) leads straight into this refusal.
        // design/distinct_composite_forms.md lists only references, functions, errors,
        // maps, arrays and Rust-backed abstracts. Fix: for Tuple, Struct and a payload
        // Variant, recurse into the parts; keep List (nil passes any list's test).
        // `crosses` gains the same. probe:
        // design/review-2026-11-16/repro/x-kind-markers-open-member-composite.sh
        // (x-kind-markers-04)
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
/// collides only where each of them does, each judged on a walk of its
/// own: the pairs a bound that collides held clear are not clear for the
/// rest of `seen`'s walk.
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
        if cell.cell_above().contains(&Type::Bound(Bound::NoError)) && error_only(env, t)
        {
            return None;
        }
        let typed = cell.typed_bounds(env);
        if typed.is_empty() {
            return match shares_form(env, t) {
                true => pair(),
                false => (ordered && !NUMBERS_ORDERED.get() && holds_number(env, t))
                    .then(|| Indiscernible::OpenNumber(t.clone())),
            };
        }
        let mut why = None;
        for b in typed.iter() {
            let walk = &mut RepSeen::default();
            let found =
                rep_collision(env, walk, Subject::Beside, Open::Unknown, ordered, t, b);
            seen.env_read |= walk.env_read;
            why = Some(found?);
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
        let mut bounds = self.cell_above();
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

/// The applications a walk has expanded, each once.
struct Expanded<
    K: std::hash::Hash + Eq + 'static,
    N: std::hash::Hash + Eq + 'static = usize,
> {
    seen: RefHist<AHashSet<K>>,
    /// The shared nodes (or pairs of them) the walk found nothing under,
    /// so a type that shares them (a DAG) is walked once.
    clear: LPooled<AHashSet<N>>,
    /// The heads the walk built, alive until it ends: a node `clear`
    /// holds keeps its address.
    built: LPooled<Vec<Type>>,
    /// How many applications the walk is inside.
    depth: usize,
    /// Whether the walk read what the environment says of a type (an
    /// abstract's implementations or representation, a head it could not
    /// resolve): a verdict it reached holds in this environment only.
    env_read: bool,
    /// Whether the walk passes over a product no value of the other side
    /// passes ([`parts_collision`]).
    told: bool,
}

impl<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static> Default
    for Expanded<K, N>
{
    fn default() -> Self {
        Self {
            seen: RefHist::new(),
            clear: LPooled::take(),
            built: LPooled::take(),
            depth: 0,
            env_read: false,
            told: false,
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

/// `body` inside the application `at`: `again` when the walk expanded it
/// already. A typedef is neither expansive nor non-contractive
/// ([`Env::deftype`]), so a type unfolds to finitely many applications
/// and the walk ends.
fn walk<K: std::hash::Hash + Eq + 'static, N: std::hash::Hash + Eq + 'static, R>(
    seen: &mut Expanded<K, N>,
    at: Option<K>,
    again: R,
    body: impl FnOnce(&mut Expanded<K, N>) -> R,
) -> R {
    let Some(k) = at else { return body(seen) };
    if !seen.seen.insert(k) {
        return again;
    }
    seen.depth += 1;
    let r = body(seen);
    seen.depth -= 1;
    r
}

/// Pairs already on the walk, each side by the id of its structure
/// ([`RefHist::param_id`]); a finite witness never revisits one.
type RepSeen = Expanded<(usize, usize), (usize, usize)>;

impl RepSeen {
    /// The pair a walk is at where a side names a type. An id reads each
    /// shared node of its side once, so a side that shares its parts (a
    /// difference of two recursive unions) costs its nodes, not its
    /// unfolding.
    fn pair_at(&mut self, env: &Env, a: &Type, b: &Type) -> Option<(usize, usize)> {
        (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_)))
            .then(|| (self.seen.param_id(a, env), self.seen.param_id(b, env)))
    }
}

fn ref_at(t: &Type) -> Option<Type> {
    matches!(t, Type::Ref(_)).then(|| t.clone())
}

/// An abstract type a walk is about to look through, keyed like a named
/// type: a representation may name its own type.
fn abstract_at(t: &Type) -> Option<Type> {
    matches!(t, Type::Abstract { .. }).then(|| t.clone())
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

/// What `a` is to a runtime-form judgment.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Subject {
    /// A type beside `b`.
    Beside,
    /// A select arm's test whose cells, bound or open, are parts the arm
    /// binds and does not test; a union of the test's types every value
    /// of a member it holds ([`holds`]).
    Binds,
}

/// Whether `t`, a part of a select arm's test, is `b` or a union holding
/// it, `b` a head no union: a value of `b` that passes the test there is
/// typed `b` ([`Type::matching_type`]), whatever else of `t` shares its
/// form. A name the search has expanded (`unfolded`) is not read again.
fn holds(env: &Env, unfolded: &mut Unfolded, t: &Type, b: &Type) -> bool {
    ensure_sufficient(|| {
        #[cfg(test)]
        WRITTEN_HOLDS.with(|n| n.set(n.get() + 1));
        if let Type::Ref(tr) = t
            && tr.resolve_in(env).is_some()
            && !unfolded.first(t)
        {
            return false;
        }
        match rep_head(env, t).as_deref() {
            Some(Type::Set(ts)) => ts.iter().any(|t| holds(env, unfolded, t, b)),
            Some(h) => union_identical_in(env, h, b),
            None => false,
        }
    })
}

/// How many pairs one [`may_pass`] walk judges: past it, a pair may pass.
const MAX_PASS_PAIRS: u32 = 1 << 12;

/// A [`may_pass`] walk: the pairs of names it met, `None` while one is in
/// progress, and how many pairs it has judged.
struct Passes {
    names: RefHist<AHashMap<(usize, usize), Option<bool>>>,
    pairs: u32,
}

/// Whether a value of `b` may pass the run-time test ([`Type::is_a`]) for
/// `a`, a part of a select arm's test, whose cells pass every value where
/// `binds`: true wherever a head is not decided, a pair of names is met
/// inside itself, or the walk is past its budget. An empty array, list or
/// map passes the test for any, and a reference or function the test for
/// any other.
fn may_pass(env: &Env, seen: &mut Passes, binds: bool, a: &Type, b: &Type) -> bool {
    if binds && matches!(a, Type::TVar(_)) {
        return true;
    }
    seen.pairs += 1;
    if seen.pairs > MAX_PASS_PAIRS {
        return true;
    }
    ensure_sufficient(|| {
        let at = (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_)))
            .then(|| (seen.names.param_id(a, env), seen.names.param_id(b, env)));
        if let Some(at) = at {
            match seen.names.get(&at) {
                Some(r) => return r.unwrap_or(true),
                None => seen.names.insert(at, None),
            };
        }
        let r = match (rep_head(env, a), rep_head(env, b)) {
            (Some(a), Some(b)) => heads_may_pass(env, seen, binds, &a, &b),
            _ => true,
        };
        if let Some(at) = at {
            seen.names.insert(at, Some(r));
        }
        r
    })
}

/// [`may_pass`] of two heads.
fn heads_may_pass(env: &Env, seen: &mut Passes, binds: bool, a: &Type, b: &Type) -> bool {
    let opaque =
        |t: &Type| matches!(t, Type::Abstract { .. } | Type::ByRef(..) | Type::Fn(_));
    let mut pass = |x: &Type, y: &Type| may_pass(env, seen, binds, x, y);
    match (a, b) {
        (Type::Bottom, _) | (_, Type::Bottom) => true,
        (Type::Set(ts), _) => ts.iter().any(|t| pass(t, b)),
        (_, Type::Set(ts)) => ts.iter().any(|t| pass(a, t)),
        (Type::Primitive(p), Type::Primitive(q)) => p.intersects(*q),
        (Type::Primitive(p), t) | (t, Type::Primitive(p)) => match t {
            Type::Tuple(_) | Type::Struct(_) | Type::Variant(..) | Type::List(_) => false,
            Type::Array(_) | Type::Map(_) | Type::Error(_) => p.intersects(rep_prims(t)),
            _ => p.contains(Typ::Abstract),
        },
        (Type::Error(x), Type::Error(y)) => pass(x, y),
        (Type::Tuple(x), Type::Tuple(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| pass(x, y))
        }
        (Type::Struct(x), Type::Struct(y)) => {
            x.len() == y.len()
                && x.iter().zip(y.iter()).all(|(x, y)| x.0 == y.0 && pass(&x.1, &y.1))
        }
        (Type::Variant(x, xa, _), Type::Variant(y, ya, _)) => {
            x == y
                && xa.len() == ya.len()
                && xa.iter().zip(ya.iter()).all(|(x, y)| pass(x, y))
        }
        (a, b) => {
            mem::discriminant(a) == mem::discriminant(b) || (opaque(a) && opaque(b))
        }
    }
}

/// The collision of two products part by part: the first one a part
/// holds. A walk that tells ([`test_collision`]) passes over a select
/// arm's test one of whose parts passes no value of its counterpart.
fn parts_collision<'a>(
    env: &Env,
    seen: &mut RepSeen,
    subject: Subject,
    open: Open,
    ordered: bool,
    parts: impl Iterator<Item = (&'a Type, &'a Type)> + Clone,
) -> Option<Indiscernible> {
    let first = |seen: &mut RepSeen| {
        parts
            .clone()
            .find_map(|(x, y)| rep_collision(env, seen, subject, open, ordered, x, y))
    };
    if !seen.told {
        return first(seen);
    }
    let passes = &mut Passes { names: RefHist::new(), pairs: 0 };
    let binds = subject == Subject::Binds;
    match parts.clone().all(|(x, y)| may_pass(env, passes, binds, x, y)) {
        true => first(seen),
        false => None,
    }
}

/// A run-time test `a` against `b`, what else reaches it. A walk holds
/// the pairs it met clear, so it ends at its first collision: a second
/// walk, made where the first found one, passes over what a part tells.
fn test_collision(
    env: &Env,
    subject: Subject,
    open: Open,
    a: &Type,
    b: &Type,
) -> Option<Indiscernible> {
    let judge = |told: bool| {
        let seen = &mut RepSeen { told, ..RepSeen::default() };
        rep_collision(env, seen, subject, open, false, a, b)
    };
    judge(false).and_then(|_| judge(true))
}

/// Two parts of `a` and `b` a runtime test can't tell apart, or,
/// `ordered`, two numeric types at one position, whose values have no
/// order between them.
fn rep_collision(
    env: &Env,
    seen: &mut RepSeen,
    subject: Subject,
    open: Open,
    ordered: bool,
    a: &Type,
    b: &Type,
) -> Option<Indiscernible> {
    if subject == Subject::Binds && matches!(a, Type::TVar(_)) {
        return None;
    }
    ensure_sufficient(|| {
        let at = seen.pair_at(env, a, b);
        walk(seen, at, None, |seen| {
            let pairs =
                |seen: &mut RepSeen, xs: &mut dyn Iterator<Item = (&Type, &Type)>| {
                    xs.map(|(x, y)| {
                        rep_collision(env, seen, subject, open, ordered, x, y)
                    })
                    .find(Option::is_some)
                    .flatten()
                };
            let (ha, hb) = (rep_head(env, a), rep_head(env, b));
            seen.env_read |= ha.is_none() || hb.is_none();
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
                    (Type::Set(_), Type::Set(ts)) if subject != Subject::Beside => {
                        pairs(&mut ts.iter().map(|t| (a, t)))
                    }
                    (Type::Set(_), _)
                        if subject != Subject::Beside
                            && holds(env, &mut Unfolded::default(), a, b) =>
                    {
                        None
                    }
                    (Type::Set(ts), _) => pairs(&mut ts.iter().map(|t| (t, b))),
                    (_, Type::Set(ts)) => pairs(&mut ts.iter().map(|t| (a, t))),
                    (Type::Primitive(p), Type::Primitive(q)) => {
                        let numbers = (*p | *q) & Typ::number();
                        (ordered && !NUMBERS_ORDERED.get() && numbers.len() > 1)
                            .then(|| Indiscernible::Numbers(Type::Primitive(numbers)))
                    }
                    (Type::Array(x), Type::Array(y))
                    | (Type::List(x), Type::List(y))
                    | (Type::Error(x), Type::Error(y)) => {
                        pairs(&mut std::iter::once((&**x, &**y)))
                    }
                    (Type::Map(m0), Type::Map(m1)) => pairs(
                        &mut [(&m0.key, &m1.key), (&m0.value, &m1.value)].into_iter(),
                    ),
                    (Type::Tuple(x), Type::Tuple(y)) if x.len() == y.len() => {
                        let parts = x.iter().zip(y.iter());
                        parts_collision(env, seen, subject, open, ordered, parts)
                    }
                    (Type::Struct(x), Type::Struct(y))
                        if x.len() == y.len()
                            && x.iter().zip(y.iter()).all(|(x, y)| x.0 == y.0) =>
                    {
                        let parts = x.iter().zip(y.iter()).map(|(x, y)| (&x.1, &y.1));
                        parts_collision(env, seen, subject, open, ordered, parts)
                    }
                    (Type::Variant(x, xa, _), Type::Variant(y, ya, _))
                        if x == y && xa.len() == ya.len() =>
                    {
                        let parts = xa.iter().zip(ya.iter());
                        parts_collision(env, seen, subject, open, ordered, parts)
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
                        seen.env_read = true;
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
        let seen = &mut RepSeen::default();
        rep_collision(env, seen, Subject::Beside, Open::Benign, false, self, t)
    }

    /// [`Self::rep_collision`] with an open cell read as any type it may
    /// yet bind ([`Open::Unknown`]), and a product no value of the other
    /// passes, for another part, passed over ([`test_collision`]): what a
    /// definition must rule out.
    pub fn rep_collision_open(&self, env: &Env, t: &Type) -> Option<Indiscernible> {
        test_collision(env, Subject::Beside, Open::Unknown, self, t)
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
    /// An abstract type without the requested comparison implementation, whose values
    /// compare by a representation that fails one of the above; its
    /// representation is its module's, so only its name is told.
    Abstract(Type, CoreTrait),
}

impl Indiscernible {
    /// The refusal of a map key type for this reason.
    pub fn as_map_key(&self) -> anyhow::Error {
        format_with_flags(PrintFlag::DerefTVars, || match self {
            Indiscernible::Abstract(a, kind) => anyhow::anyhow!(
                "a map key type can't hold {}",
                Indiscernible::by_representation(a, *kind)
            ),
            Indiscernible::Pair(a, b) => anyhow::anyhow!(
                "{} and {} can't share a map key type: they have the same runtime \
                     form; wrap them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
            ),
            Indiscernible::Numbers(n) => anyhow::anyhow!(
                "a map key type can't hold {}, {}",
                n.resolve_tvars(),
                Indiscernible::NUMBERS
            ),
            Indiscernible::OpenNumber(n) => anyhow::anyhow!(
                "a map key type can't hold {}, {}",
                n.resolve_tvars(),
                Indiscernible::OPEN_NUMBER
            ),
            Indiscernible::Unordered(r) => {
                let (kind, _) = Indiscernible::unordered_kind(r);
                anyhow::anyhow!(
                    "a map key type can't hold the {kind} {}: {kind}s have no order",
                    r.resolve_tvars()
                )
            }
        })
    }

    /// The refusal of `what`, a runtime test a [`rep_collision`] found
    /// can't tell this apart.
    pub fn untold(&self, what: &str) -> anyhow::Error {
        match self {
            Indiscernible::Pair(a, b) => anyhow::anyhow!(
                "{what} can't tell {} from {}: both have the same runtime form; wrap \
                 them in distinct variants",
                a.resolve_tvars(),
                b.resolve_tvars()
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

thread_local! {
    /// The judgment in progress orders numbers of different types, as the
    /// run-time order does ([`numbers_ordered`]).
    static NUMBERS_ORDERED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// `f`, with an ordering taking a type that holds two numeric types: the
/// check by bounds judges `Ordered` so (D47 follow-up 5). The clause that
/// refuses them goes with the check it belongs to.
pub fn numbers_ordered<R>(f: impl FnOnce() -> R) -> R {
    let was = NUMBERS_ORDERED.replace(true);
    let r = f();
    NUMBERS_ORDERED.set(was);
    r
}

fn rep_ambiguity(
    env: &Env,
    seen: &mut Expanded<Type>,
    open: Open,
    keys_only: bool,
    refs: bool,
    t: &Type,
) -> Option<Indiscernible> {
    ensure_sufficient(|| {
        // with no application in progress, what a ground cell's binding
        // holds rests on nothing the walk assumed
        if !keys_only
            && seen.depth == 0
            && let Type::TVar(tv) = t
            && let judgment = match refs {
                true => Judgment::Ordered,
                false => Judgment::Compared,
            }
            && let Some(r) = tv.ground_verdict(judgment, |b| {
                let r = rep_ambiguity(env, seen, open, keys_only, refs, b);
                (r, !seen.env_read)
            })
        {
            return r;
        }
        walk(seen, ref_at(t), None, |seen| {
            let Some(head) = rep_head(env, t) else {
                seen.env_read = true;
                return None;
            };
            let r = seen.once(super::node_addr(&head), |seen| {
            let t = &*head;
            let mut parts = |ts: &mut dyn Iterator<Item = &Type>| {
                ts.map(|t| rep_ambiguity(env, seen, open, keys_only, refs, t))
                    .find(Option::is_some)
                    .flatten()
            };
            let ordered = refs && !keys_only;
            match &t {
                Type::Primitive(p)
                    if ordered
                        && !NUMBERS_ORDERED.get()
                        && (*p & Typ::number()).len() > 1 =>
                {
                    Some(Indiscernible::Numbers(t.clone()))
                }
                // a union with a member that is `Any`, by whatever name, is
                // `Any`
                Type::Set(ts)
                    if ts.iter().any(|m| {
                        matches!(resolved_head(env, m).as_deref(), Some(Type::Any))
                    }) =>
                {
                    None
                }
                Type::Set(ts) => {
                    let mut pairs_read = false;
                    if !keys_only {
                        for (i, a) in ts.iter().enumerate() {
                            for b in &ts[i + 1..] {
                                let mut pair_seen = RepSeen::default();
                                let c = rep_collision(
                                    env,
                                    &mut pair_seen,
                                    Subject::Beside,
                                    open,
                                    ordered,
                                    a,
                                    b,
                                );
                                if c.is_some() {
                                    return c;
                                }
                                pairs_read |= pair_seen.env_read;
                            }
                        }
                    }
                    let r = parts(&mut ts.iter());
                    seen.env_read |= pairs_read;
                    r
                }
                Type::Map(m) => {
                    let keys = match keys_only {
                        true => rep_ambiguity(
                            env,
                            &mut Expanded::default(),
                            open,
                            false,
                            true,
                            &m.key,
                        ),
                        false => None,
                    };
                    keys.or_else(|| parts(&mut [&m.key, &m.value].into_iter()))
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
                    seen.env_read = true;
                    let kind = if ordered { CoreTrait::Ord } else { CoreTrait::Eq };
                    let rep = compared_rep(env, *id, params, kind)?;
                    let inside = walk(
                        seen,
                        abstract_at(t),
                        None,
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
                // CR claude for claude: [readability/low] This joins "holds" to a
                // sentence that starts with the type and its verb: "Array<Box<[i64,
                // f64]>> must be Ordered here, but it holds Box<[i64, f64]> has no Ord
                // implementation, so its values compare by its representation, ...".
                // Split `by_representation` into the type and the clause after it, so
                // this arm reads "holds T, which has no Ord implementation, so .." and
                // the operator's message keeps "T has no ..". probe:
                // design/review-2026-11-16/repro/t-cast-not-discernible-wording.gx
                // (t-cast-13)
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
        walk(seen, ref_at(t), false, |seen| {
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
                Type::Map(m) => any(&mut [&m.key, &m.value].into_iter()),
                Type::Abstract { id, params } => {
                    match reference_rep(env, *id, params, kind) {
                        None => false,
                        Some(rep) => walk(seen, abstract_at(t), false, |seen| {
                            compared_ref(env, seen, &rep, kind)
                        }),
                    }
                }
                _ => false,
            }
        })
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

/// The data inside `v`, the object of one entry keyed `name` a text
/// format writes an abstract value of the type `name` as (read as its
/// `[key, value]` pairs).
fn unwrapped<'a>(name: &str, v: &'a Value) -> Option<&'a Value> {
    match &rep::array_elems(v)?[..] {
        [entry] => match &rep::array_elems(entry)?[..] {
            [Value::String(k), inside] if k == name => Some(inside),
            _ => None,
        },
        _ => None,
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

/// Push the members of the union member `t` a union tries one at a time:
/// a name or a variable naming a union is that union's members, and one
/// naming a primitive set that set ([`merge_primitives`] splits them by
/// class). A name met again adds nothing: `named` holds those met.
fn members_by_class<'a>(
    env: &Env,
    t: Cow<'a, Type>,
    out: &mut SmallVec<[Cow<'a, Type>; 8]>,
    named: &mut LPooled<Vec<RefKey>>,
    depth: usize,
) {
    if depth > MAX_UNION_NESTING {
        return out.push(t);
    }
    if let Type::Ref(tr) = &*t
        && let Some(k) = ref_key(tr)
    {
        if named.contains(&k) {
            return;
        }
        named.push(k);
    }
    let head = match &t {
        Cow::Borrowed(t) => rep_head(env, t),
        Cow::Owned(t) => rep_head(env, t).map(|h| Cow::Owned(h.into_owned())),
    };
    ensure_sufficient(|| match &head {
        Some(Cow::Borrowed(Type::Set(ms))) => ms
            .iter()
            .for_each(|m| members_by_class(env, Cow::Borrowed(m), out, named, depth + 1)),
        Some(Cow::Owned(Type::Set(ms))) => ms.iter().for_each(|m| {
            members_by_class(env, Cow::Owned(m.clone()), out, named, depth + 1)
        }),
        Some(h) if let Type::Primitive(p) = &**h => {
            out.push(Cow::Owned(Type::Primitive(*p)))
        }
        _ => out.push(t),
    })
}

/// The primitives of `members`, written or named, as one set split by
/// `classes` and the rest, where the first of them stood: a union reads
/// its primitives in their canonical order however it names them, and
/// each class's with the members of that class.
fn merge_primitives<'a>(
    [own, parsed]: [BitFlags<Typ>; 2],
    members: &mut SmallVec<[Cow<'a, Type>; 8]>,
) {
    let mut prims = BitFlags::empty();
    let mut first = None;
    let mut i = 0;
    members.retain(|t| {
        let keep = match &**t {
            Type::Primitive(p) => {
                prims |= *p;
                first.get_or_insert(i);
                false
            }
            _ => true,
        };
        if keep {
            i += 1;
        }
        keep
    });
    if let Some(at) = first {
        let parts = [prims & own, prims & parsed, prims & !own & !parsed];
        for (k, p) in parts.into_iter().filter(|p| !p.is_empty()).enumerate() {
            members.insert(at + k, Cow::Owned(Type::Primitive(p)))
        }
    }
}

/// Whether the union member `t`, past [`members_by_class`], may read the
/// scalar a text parses as ([`in_own_class`] of it) where the text's own
/// class does not hold it.
fn takes_scalar(env: &Env, t: &Type) -> bool {
    match rep_head(env, t).as_deref() {
        Some(Type::Primitive(p)) => p.intersects(text_scalars()),
        Some(Type::Set(_)) => true,
        _ => false,
    }
}

/// The classes of the scalars a text parses as ([`parsed_scalar`]).
fn text_scalars() -> BitFlags<Typ> {
    Typ::number() | Typ::Bool | Typ::DateTime | Typ::Duration
}

/// How many unions deep [`members_by_class`] looks into a union's members;
/// a deeper one is tried whole.
const MAX_UNION_NESTING: usize = 64;

/// Whether `t` may read `v` without leaving `v`'s runtime class, which a
/// union tries before any conversion across classes.
fn in_own_class(env: &Env, t: &Type, v: &Value) -> bool {
    ensure_sufficient(|| match rep_head(env, t).as_deref() {
        None | Some(Type::Bottom) => true,
        Some(Type::Primitive(s)) => rep::kind(v).is_none() && s.intersects(own_class(v)),
        Some(Type::Set(ts)) => ts.iter().any(|t| in_own_class(env, t, v)),
        Some(Type::Array(e)) => {
            rep::array_elems(v).is_some_and(|a| a.iter().all(|x| in_own_class(env, e, x)))
        }
        Some(Type::List(e)) if list::is_list(v) => {
            list::Iter::new(v.clone()).all(|x| in_own_class(env, e, &x))
        }
        Some(Type::List(e)) => {
            rep::kind(v).is_none()
                && rep::array_elems(v)
                    .is_some_and(|a| a.iter().all(|x| in_own_class(env, e, x)))
        }
        Some(Type::Tuple(ts)) => rep::tuple_elems(v).is_some_and(|a| {
            a.len() == ts.len() && ts.iter().zip(a).all(|(t, x)| in_own_class(env, t, x))
        }),
        Some(Type::Struct(fs)) => rep::struct_fields(v).is_some_and(|a| {
            a.len() == fs.len()
                && fs.iter().zip(a).all(|((_, t, _), f)| {
                    rep::field(f).is_some_and(|(_, x)| in_own_class(env, t, x))
                })
        }),
        Some(Type::Variant(tag, ps, _)) => rep::variant(v).is_some_and(|(t, a)| {
            t == tag
                && a.len() == ps.len()
                && ps.iter().zip(a).all(|(p, x)| in_own_class(env, p, x))
        }),
        Some(Type::Map(t)) => match v {
            Value::Map(m) => m.into_iter().all(|(k, x)| {
                in_own_class(env, &t.key, k) && in_own_class(env, &t.value, x)
            }),
            _ => false,
        },
        Some(Type::Error(_)) => matches!(v, Value::Error(_)),
        Some(_) => matches!(v, Value::Abstract(_)),
    })
}

/// Whether unmarked struct data, `[name, value]` pairs, reads into the
/// fields `fs` with each value in its own class.
fn fields_in_own_class(
    env: &Env,
    fs: &[(ArcStr, Type, crate::typ::WrittenAt)],
    a: &ValArray,
) -> bool {
    a.iter().all(|p| match p {
        Value::Array(kv) if kv.len() == 2 => match (&kv[0], &kv[1]) {
            (Value::String(n), x) => fs
                .iter()
                .find(|(f, _, _)| f == n)
                .is_some_and(|(_, t, _)| in_own_class(env, t, x)),
            _ => false,
        },
        _ => false,
    })
}

/// `v` as the primitive `t`, exactly or not at all. A number converts to
/// a number, and a bool (1 or 0), a datetime (Unix seconds) and a
/// duration (seconds) to an integer or a float, never a decimal; a number
/// to a datetime or a duration (an integer target holds it exactly, a
/// fraction is dropped), a string to what it parses as, a number, bool,
/// datetime or duration to its plain printed form, a decimal to the float
/// nearest it, a float to its decimal (a whole one with a point), and an
/// array of numbers to bytes. Nothing converts to null or to an error.
fn convert(v: Value, t: Typ) -> Option<Value> {
    convert_text(v, t, parsed_scalar)
}

/// [`convert`], `scalar` reading a text as the scalar it parses as
/// ([`parsed_scalar`]) where the conversion needs it.
fn convert_text(
    v: Value,
    t: Typ,
    scalar: impl FnOnce(&str) -> Option<Value>,
) -> Option<Value> {
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
        (Value::String(s), t) => parse(s, t, scalar),
        (_, Typ::String) => printed(&v),
        (_, Typ::DateTime | Typ::Duration) if Typ::get(&v).is_number() => v.cast(t),
        (_, t) if t.is_integer() && counts(&v) => integer(&v, t),
        (Value::Decimal(d), Typ::F64) => {
            format_compact!("{d}").parse().ok().map(Value::F64)
        }
        (Value::Decimal(d), Typ::F32) => {
            format_compact!("{d}").parse().ok().map(Value::F32)
        }
        (Value::F32(_) | Value::F64(_), Typ::Decimal) => float_decimal(&v),
        (_, t) if t.is_number() && counts(&v) => v.cast(t),
        _ => None,
    }
}

/// The float `v` as the decimal its shortest text names, the number the
/// program wrote, rounded to the decimal's 28 places; a whole one keeps a
/// point (scale 1) where the decimal can hold it, so it prints as the
/// float does (`3.0`), and a zero, one that rounds to zero included, is
/// `0.0` with the float's sign.
fn float_decimal(v: &Value) -> Option<Value> {
    let text = match v {
        Value::F32(f) => format_compact!("{f}"),
        Value::F64(f) => format_compact!("{f}"),
        _ => return None,
    };
    let d: rust_decimal::Decimal = text.parse().ok()?;
    if d.is_zero() {
        let mut zero = rust_decimal::Decimal::new(0, 1);
        zero.set_sign_negative(text.starts_with('-'));
        return Some(Value::Decimal(Arc::new(zero)));
    }
    if d.scale() > 0 {
        return Some(Value::Decimal(Arc::new(d)));
    }
    let mut pointed = d;
    pointed.rescale(1);
    Some(Value::Decimal(Arc::new(match pointed.scale() == 1 && pointed == d {
        true => pointed,
        false => d,
    })))
}

/// The string `s` read as the primitive `t`, `scalar` reading it as the
/// scalar it parses as.
fn parse(
    s: &ArcStr,
    t: Typ,
    scalar: impl FnOnce(&str) -> Option<Value>,
) -> Option<Value> {
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
    if text_scalars().contains(t) {
        return scalar(s).and_then(|p| convert(p, t));
    }
    if parses_to_collection(s) {
        return None;
    }
    match literal(s) {
        Ok(Value::String(_)) | Err(_) => None,
        Ok(parsed) => convert(parsed, t),
    }
}

/// A number, bool, datetime or duration printed without its type: a
/// datetime in RFC3339 and a duration in seconds, as json writes them,
/// and a whole float with its point.
fn printed(v: &Value) -> Option<Value> {
    let s = match v {
        Value::Bool(b) => format_compact!("{b}"),
        Value::DateTime(d) => format_compact!("{}", d.to_rfc3339()),
        Value::Duration(d) => {
            format_compact!("{}", PlainScalar(&Value::F64(d.as_secs_f64())))
        }
        Value::Decimal(d) => format_compact!("{d}"),
        v if Typ::get(v).is_number() => format_compact!("{}", PlainScalar(v)),
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
/// held exactly by 3.5 and not by 3, and a bool is held by the number 1
/// or 0. `parsed` is the scalar `v` parses as when it is text.
fn holds_exactly(v: &Value, parsed: Option<&Value>, c: &Value) -> bool {
    match (v, c) {
        (Value::String(_), c) => parsed.is_none_or(|p| holds_exactly(p, None, c)),
        (Value::Bool(b), c) if Typ::get(c).is_number() => {
            holds_exactly(&Value::U8(*b as u8), None, c)
        }
        (v, c) => convert(c.clone(), Typ::get(v)).as_ref() == Some(v),
    }
}

/// Whether the real `c`, `v` converted, holds `v` rounded to its
/// precision: any real but an infinity a finite value overflowed to.
/// `parsed` is the scalar `v` parses as when it is text.
fn holds_rounded(v: &Value, parsed: Option<&Value>, c: &Value) -> bool {
    Typ::get(c).is_real() && !overflowed(v, parsed, c)
}

/// Whether `c` is an infinity read from a finite number, or from text
/// naming one.
fn overflowed(v: &Value, parsed: Option<&Value>, c: &Value) -> bool {
    let infinite = |v: &Value| match v {
        Value::F32(f) => f.is_infinite(),
        Value::F64(f) => f.is_infinite(),
        _ => false,
    };
    infinite(c)
        && !match v {
            Value::String(_) => parsed.is_some_and(infinite),
            v => infinite(v),
        }
}

/// The scalar the text `s` parses as: a number, a bool, a datetime or a
/// duration; `None` for anything else.
fn parsed_scalar(s: &str) -> Option<Value> {
    literal(s)
        .ok()
        .filter(|p| text_scalars().contains(Typ::get(p)))
        .or_else(|| long_integer(s))
}

/// Integer text too long for every integer type: the decimal it names
/// where a decimal holds it exactly, else the f64 nearest it.
fn long_integer(s: &str) -> Option<Value> {
    let digits = s.strip_prefix(['-', '+']).unwrap_or(s);
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    match rust_decimal::Decimal::from_str_exact(s) {
        Ok(d) => Some(Value::Decimal(Arc::new(d))),
        Err(_) => s.parse().ok().map(Value::F64),
    }
}

#[cfg(test)]
thread_local! {
    static LITERALS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static IS_A_NAMES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static SHAPE_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static HOLDS_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static WRITTEN_HOLDS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// `s` read as a value literal.
fn literal(s: &str) -> Result<Value> {
    #[cfg(test)]
    LITERALS.with(|n| n.set(n.get() + 1));
    s.parse()
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
        && matches!(literal(s), Ok(Value::Array(_) | Value::Map(_)))
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
            (Type::Map(t), Value::Map(m)) => Value::Map(
                m.into_iter()
                    .map(|(k, v)| (k.clone(), map_refs(env, holds, &t.value, v, f)))
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
pub(crate) mod tests {
    use crate::{
        env::Env,
        expr::{ModPath, parser::parse_type},
        typ::{TVar, Type},
    };
    use ahash::AHashMap;
    use arcstr::literal;
    use netidx_value::Value;
    use std::{
        borrow::Cow,
        time::{Duration, Instant},
    };
    use triomphe::Arc;

    /// An env holding the typedefs `defs` at the root, and `t` there.
    pub(crate) fn typed(defs: &[(&str, &str)], t: &str) -> (Env, Type) {
        Env::typed_at_root(defs, t).unwrap()
    }

    /// The type `t` written at the root of `env`.
    pub(crate) fn parsed(env: &Env, t: &str) -> Type {
        let t = parse_type(t).unwrap().scope_refs(&ModPath::root());
        t.seed_refs(env);
        t
    }

    /// `links` applications over a `leaf`, each of `P` to the two below
    /// it but the top, of `top`: every link one node, shared by the two
    /// above it.
    fn application_dag(env: &Env, links: usize, top: &str, leaf: &str) -> Type {
        let mut below = vec![parsed(env, leaf), parsed(env, "[Array<i64>, null]")];
        for i in 0..links {
            let name = if i + 1 == links { top } else { "P" };
            let [q, p] = below.last_chunk().unwrap().clone();
            let params = AHashMap::from_iter([(literal!("p"), p), (literal!("q"), q)]);
            below.push(parsed(env, &format!("{name}<'p, 'q>")).replace_tvars(&params));
        }
        below.pop().unwrap()
    }

    // A search for a part (`Type::holds`) walks each shared application
    // of a DAG once: where it finds nothing and where every link holds
    // what it looks for, and inside a definition being decided, which the
    // top link of `Q` is when the links below it are met again. As a
    // tree, the links below one are Fibonacci many.
    #[test]
    fn a_search_walks_a_dag_of_applications_once() {
        const LINKS: usize = 40;
        let defs = [
            ("P<'x, 'y>", "[Array<'x>, Array<'y>, null]"),
            ("Q<'x, 'y>", "[List<'x>, List<'y>, null]"),
        ];
        let (env, _) = typed(&defs, "i64");
        for (top, leaf, held) in
            [("P", "i64", false), ("Q", "i64", false), ("Q", "Any", true)]
        {
            let dag = application_dag(&env, LINKS, top, leaf);
            super::HOLDS_STEPS.with(|s| s.set(0));
            assert_eq!(dag.holds_abstract(&env), held, "{top} over {leaf}");
            let steps = super::HOLDS_STEPS.with(|s| s.get());
            assert!(steps <= 32 * LINKS, "{top} over {leaf}: {steps} steps");
        }
    }

    // A search that finds nothing under an application while a definition
    // it names is being decided says nothing of the application: `B`'s
    // `C<i64>` is first met inside `A`, before `A`'s own `_`, and `W`
    // holds what its second parameter does alone.
    #[test]
    fn a_search_keeps_no_verdict_that_rests_on_a_definition_being_decided() {
        let defs = [
            ("A", "[`Go(B), `Yes(_)]"),
            ("B", "(C<i64>, i64)"),
            ("C<'p>", "[Array<A>, 'p]"),
            ("W<'x, 'y>", "(fn(a: 'x) -> i64, 'y)"),
        ];
        let (env, both) = typed(&defs, "W<A, B>");
        assert!(both.holds_bottom(&env));
        assert!(!parsed(&env, "W<B, i64>").holds_bottom(&env));
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
        // CR claude for claude: [test-gap/low] This pin and
        // `failing_union_read_is_linear` assert a wall-clock bound inside the library's
        // shared test binary: under the gate's load two seconds can pass with nothing
        // wrong, and what the bound proves depends on the machine. The pins beside them
        // count work. Assert on `SHAPE_STEPS` here, as
        // `shallow_discriminant_walks_a_shared_dag_once` does, and give the union read
        // a `#[cfg(test)]` counter of `cast_member` calls. Found by reading; not run.
        // (t-cast-11)
        assert!(started.elapsed() < Duration::from_secs(2), "{:?}", started.elapsed());
    }

    /// `type O<'a> = ['a, null]` and, to `n`, `type Oi = Array<O<Array<..
    /// Oi+1>>>`, each `O` applied to a larger argument than the last: a
    /// finite chain, the root `O0`.
    pub(crate) fn opt_chain(o: &str, n: usize) -> Vec<(String, String)> {
        let mut defs = vec![(format!("{o}<'a>"), "['a, null]".to_string())];
        for i in 0..n {
            let next = match i + 1 == n {
                true => "i64".to_string(),
                false => format!("{o}{}", i + 1),
            };
            let arg = (0..=i).fold(next, |t, _| format!("Array<{t}>"));
            defs.push((format!("{o}{i}"), format!("Array<{o}<{arg}>>")));
        }
        defs
    }

    pub(crate) fn typed_chain(o: &str, n: usize) -> (Env, Type) {
        let defs = opt_chain(o, n);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        typed(&defs, &format!("{o}0"))
    }

    // A non-recursive typedef applied to ever larger arguments down a
    // finite chain of typedefs is compared and keyed as its expansion.
    #[test]
    fn a_finite_chain_of_larger_arguments_is_discernible() {
        let (env, t) = typed_chain("O", 8);
        assert!(t.indiscernible(&env, super::Open::Benign, true).is_none());
        assert!(t.map_key_failure(&env, super::Open::Benign).is_none());
        let u = parsed(&env, "[O0, string]");
        assert!(u.indiscernible(&env, super::Open::Benign, true).is_none());
    }

    // A value read through one definition applied twice, the second time
    // to a larger argument, is read by the inner application.
    #[test]
    fn a_definition_applied_twice_over_one_value_reads_it() {
        let v = crate::rep::structure([(arcstr::literal!("x"), Value::I64(1))]);
        for o in ["['a, null]", "'a"] {
            let defs = [("O<'a>", o), ("B", "O<{x: i64}>"), ("A", "O<B>")];
            let (env, a) = typed(&defs, "A");
            assert_eq!(a.try_cast_value(&env, v.clone()), Ok(v.clone()), "{o}");
            assert!(a.is_a(&env, &v), "{o}");
            let b = parsed(&env, "B");
            let printed = |t: &Type| {
                format!("{}", crate::typ::tval::TVal { env: &env, typ: t, v: &v })
            };
            assert_eq!(printed(&a), printed(&b), "{o}");
        }
    }

    // A definition nested in its own argument is walked at each
    // application: a union of one shape inside the inner is found.
    #[test]
    fn a_nested_application_is_searched_for_shapes() {
        let defs = [("O<'a>", "['a, null]"), ("X", "[(i64, i64), (string, string)]")];
        let (env, t) = typed(&defs, "Array<O<O<X>>>");
        assert!(super::shapes_collide(&t, &env, &mut super::ShapeSeen::default()));
    }

    /// A select's shallow test over a union holding a DAG of tuples walks
    /// each shared node once, not once per path.
    #[test]
    fn shallow_discriminant_walks_a_shared_dag_once() {
        let n = 20;
        let mut t = parse_type("i64").unwrap();
        for _ in 0..n {
            t = Type::Tuple(Arc::from_iter([t.clone(), t]));
        }
        let scrutinee =
            Type::Set(Arc::from_iter([parse_type("null").unwrap(), t.clone()]));
        super::SHAPE_STEPS.with(|s| s.set(0));
        assert!(t.shallow_discriminant(&Env::default(), &scrutinee).is_some());
        let steps = super::SHAPE_STEPS.with(|s| s.get());
        assert!(steps <= 4 * n, "{steps} steps over a DAG of depth {n}");
    }

    /// Where the member a predicate meets holds `Any`, the shallow test
    /// keeps the predicate's part that does not hold the member's: the
    /// arm's own types there, which only this test checks. A part behind
    /// a function type or a reference is no test's to read.
    #[test]
    fn shallow_discriminant_keeps_a_part_narrower_than_any() {
        let (env, _) = typed(&[("V", "Any"), ("P", "(V, i64)")], "V");
        let shallow = |pred: &str, scrutinee: &str| {
            let d =
                parsed(&env, pred).shallow_discriminant(&env, &parsed(&env, scrutinee));
            d.map(|t| t.to_string())
        };
        let some = |t: &str| Some(t.to_string());
        assert_eq!(shallow("((i64, i64), i64)", "(Any, i64)"), some("((i64, i64), Any)"));
        assert_eq!(shallow("(Any, i64)", "(Any, i64)"), some("(Any, Any)"));
        assert_eq!(shallow("(Array<i64>, i64)", "[P, null]"), some("(Array<i64>, Any)"));
        assert_eq!(
            shallow("(Array<i64>, Array<i64>)", "(Array<i64>, Array<Any>)"),
            some("(Any, Array<i64>)")
        );
        assert_eq!(
            shallow("`S({a: `K(i64)}, i64)", "[`S({a: Any}, i64), `N]"),
            some("`S({ a: `K(i64) }, Any)")
        );
        assert_eq!(shallow("Array<(i64, i64)>", "Array<Any>"), some("Array<(i64, i64)>"));
        assert_eq!(
            shallow("Map<string, (i64, i64)>", "Map<string, V>"),
            some("Map<Any, (i64, i64)>")
        );
        assert_eq!(
            shallow("(fn() -> Any, &Any, i64)", "(fn() -> Any, &Any, i64)"),
            some("(Any, Any, Any)")
        );
        assert_eq!(shallow("((i64, i64), i64)", "((i64, i64), i64)"), some("(Any, Any)"));
    }

    /// A part kept under a member holding `Any` is shallowed in turn, so
    /// the test of a nested pattern over a recursive type reads the
    /// pattern's depth, never the value's. A named part is not expanded:
    /// a written predicate recursive beside its member ends.
    #[test]
    fn shallow_discriminant_shallows_a_kept_part_in_turn() {
        let defs = [
            ("L", "[`Cons(Any, L), `Nil]"),
            ("N", "[`Cons(i64, N), `Nil]"),
            ("T", "[`Node(Any, T, T), `Leaf]"),
        ];
        let (env, _) = typed(&defs, "L");
        let shallow = |pred: &str, scrutinee: &str| {
            let d =
                parsed(&env, pred).shallow_discriminant(&env, &parsed(&env, scrutinee));
            d.map(|t| t.to_string())
        };
        let some = |t: &str| Some(t.to_string());
        assert_eq!(
            shallow("`Cons(Any, `Cons(Any, L))", "L"),
            some("`Cons(Any, `Cons(Any, Any))")
        );
        assert_eq!(
            shallow("`Cons(Any, `Cons((i64, i64), `Cons(Any, L)))", "L"),
            some("`Cons(Any, `Cons((i64, i64), `Cons(Any, Any)))")
        );
        assert_eq!(
            shallow("`Node(Any, `Node((i64, i64), T, T), `Leaf)", "T"),
            some("`Node(Any, `Node((i64, i64), Any, Any), `Leaf)")
        );
        assert_eq!(
            shallow("(((i64, i64), i64), i64)", "((Any, i64), i64)"),
            some("(((i64, i64), Any), Any)")
        );
        assert_eq!(
            shallow("([(i64, i64), null], i64)", "([(Any, i64), null], i64)"),
            some("([null, (i64, i64)], Any)")
        );
        assert_eq!(shallow("N", "L"), some("[`Cons(i64, N), `Nil]"));
    }

    /// A runtime-form walk keys the pair it is at by its sides' nodes: a
    /// named type against a DAG of tuples 64 deep, 2^64 leaves read as a
    /// tree, ends.
    #[test]
    fn a_form_walk_keys_a_shared_side_by_its_nodes() {
        let (env, r) = typed(&[("R", "[(R, R), `Z]")], "R");
        let mut t = parsed(&env, "`Y");
        for _ in 0..64 {
            t = Type::Tuple(Arc::from_iter([t.clone(), t]));
        }
        assert!(r.rep_collision(&env, &t).is_none());
        assert!(t.rep_collision(&env, &r).is_none());
    }

    /// A cell with two bounds that are types collides where each bound
    /// does, and a bound that collides clears no pair of names for the
    /// parts of the walk after it.
    #[test]
    fn a_bound_that_collides_clears_no_pair_for_the_next_part() {
        let defs = [("X", "{r: &i64}"), ("Y", "{r: &string}")];
        let (env, _) = typed(&defs, "null");
        let cell = TVar::empty_named(literal!("c"));
        cell.add_cell_constraint(parsed(&env, "X"));
        cell.add_cell_constraint(parsed(&env, "{q: i64}"));
        let cell = Type::TVar(cell);
        let (x, y) = (parsed(&env, "X"), parsed(&env, "Y"));
        let tuple =
            |a: &Type, b: &Type| Type::Tuple(Arc::from_iter([a.clone(), b.clone()]));
        assert!(cell.rep_collision_open(&env, &y).is_none());
        assert!(x.rep_collision_open(&env, &y).is_some());
        assert!(y.rep_collision_open(&env, &x).is_some());
        assert!(tuple(&cell, &y).rep_collision_open(&env, &tuple(&y, &x)).is_some());
    }

    /// A dispatch's members collide at a part only where every other part
    /// of one passes a value of the other, as a select arm's test does.
    #[test]
    fn a_dispatch_member_is_told_by_another_part() {
        let (env, _) = typed(&[], "null");
        let judged = |a: &str, b: &str| {
            let (a, b) = (parsed(&env, a), parsed(&env, b));
            a.rep_collision_open(&env, &b).is_some()
        };
        assert!(!judged("(&i64, i64)", "(&string, string)"));
        assert!(!judged("`K(&i64, i64)", "`K(&string, null)"));
        assert!(judged("(&i64, Array<i64>)", "(&string, Array<string>)"));
        assert!(judged("(&i64, i64)", "(&string, 'a)"));
    }

    /// `type L1 = [`A, `B]; type M1 = [`A, `C]` and, to `n`, `type Li =
    /// [Li-1, Mi-1]; type Mi = [Mi-1, Li-1]`: each name holds the next two.
    pub(crate) fn union_dag(n: usize) -> Vec<(String, String)> {
        let mut defs =
            vec![("L1".into(), "[`A, `B]".into()), ("M1".into(), "[`A, `C]".into())];
        for i in 2..=n {
            defs.push((format!("L{i}"), format!("[L{}, M{}]", i - 1, i - 1)));
            defs.push((format!("M{i}"), format!("[M{}, L{}]", i - 1, i - 1)));
        }
        defs
    }

    // A union's members past the unions its names hold expand each name
    // once, however many paths reach it.
    #[test]
    fn union_members_expand_each_name_once() {
        let defs = union_dag(20);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L20");
        let head = super::rep_head(&env, &t).map(|h| h.into_owned());
        let Some(Type::Set(ms)) = &head else { panic!("L20 is a union") };
        let mut out = smallvec::SmallVec::new();
        let mut named = poolshark::local::LPooled::take();
        for m in ms.iter() {
            super::members_by_class(&env, Cow::Borrowed(m), &mut out, &mut named, 0)
        }
        let leaves: Vec<String> = out.iter().map(|m| m.to_string()).collect();
        assert_eq!(leaves, ["`A", "`B", "`A", "`C"]);
        let v = Value::String(arcstr::literal!("C"));
        let flat = parse_type("[`A, `B, `C]").unwrap();
        assert_eq!(t.cast_value(&env, v.clone()), flat.cast_value(&env, v));
    }

    // So do the member lists a select reads (`union_members`, the
    // shallow test's `flatten_union_members`): L1 and M1 once each, not
    // 2^15 times.
    #[test]
    fn select_members_expand_each_application_once() {
        let defs = union_dag(16);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L16");
        let mut members = smallvec::SmallVec::new();
        crate::expr::union_members(&env, &t, &mut members).unwrap();
        let leaves: Vec<String> = members.iter().map(|m| m.to_string()).collect();
        assert_eq!(leaves, ["`A", "`B", "`A", "`C"]);
        let mut flat = poolshark::local::LPooled::take();
        super::flatten_union_members(&t, &env, &mut flat).unwrap();
        assert_eq!(&*flat, &members[..]);
    }

    // An is_a walk over a DAG of named unions decides each reference once
    // at a value, and may_be_error expands each name once: a value no
    // member holds costs each of the 39 names at most once per reference
    // to it (two), and a type no member of which is an error each name
    // once, not each of the 2^19 paths.
    #[test]
    fn walks_over_a_union_dag_take_each_name_once() {
        let defs = union_dag(20);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L20");
        let d = crate::rep::make_variant(arcstr::literal!("D"), [Value::I64(1)]);
        let c = crate::rep::make_variant(arcstr::literal!("C"), []);
        let names = |v: &Value| {
            super::IS_A_NAMES.with(|n| n.set(0));
            let r = t.is_a(&env, v);
            (r, super::IS_A_NAMES.with(|n| n.get()))
        };
        let (holds, n) = names(&d);
        assert!(!holds && n <= 2 * 39, "{n} names");
        assert!(names(&c).0);
        crate::typ::tvar::MAY_BE_ERROR_NAMES.with(|n| n.set(0));
        assert!(!t.may_be_error(&env, false, false).unwrap());
        assert_eq!(crate::typ::tvar::MAY_BE_ERROR_NAMES.with(|n| n.get()), 39);
    }

    // A text cast into a union parses the text as a literal once, and only
    // where a member may read the scalar it parses as.
    #[test]
    fn a_union_parses_its_text_once_and_only_for_a_scalar_member() {
        let parses = |t: &str, v: Value| {
            let (env, t) = typed(&[], t);
            super::LITERALS.with(|n| n.set(0));
            let r = t.cast_value(&env, v);
            (r, super::LITERALS.with(|n| n.get()))
        };
        let s = |s: &str| Value::String(s.into());
        let strs = |ss: &[&str]| Value::Array(ss.iter().map(|x| s(x)).collect());
        let a = crate::rep::make_variant(arcstr::literal!("A"), []);
        assert_eq!(parses("[`A, null]", s("A")), (a.clone(), 0));
        assert_eq!(parses("[`A, `B(i64)]", s("A")), (a.clone(), 0));
        assert_eq!(parses("[string, `Z]", s("w1")), (s("w1"), 0));
        assert_eq!(parses("Array<[`A, null]>", strs(&["A", "A", "A"])).1, 0);
        assert_eq!(parses("[i64, `A]", s("3")), (Value::I64(3), 1));
        assert_eq!(parses("[i64, `A]", s("A")), (a, 1));
        assert_eq!(parses("[f32, bool, `A]", s("true")), (Value::Bool(true), 1));
        let d = "3.0".parse::<rust_decimal::Decimal>().unwrap();
        let d = Value::Decimal(triomphe::Arc::new(d));
        assert_eq!(parses("[duration, decimal]", s("3.0")), (d, 1));
        assert_eq!(parses("Array<[i64, `A]>", strs(&["3", "A", "4"])).1, 3);
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
