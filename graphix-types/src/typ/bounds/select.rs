//! Narrowing. A select is one use of its scrutinee: each decided type that
//! reaches the scrutinee is passed down the arms, each arm keeps the part
//! its pattern admits and binds its names to that part, and what the last
//! arm leaves must be nothing. A pattern tests only what is written in it,
//! and a test admits what the run time cannot tell from it.

use super::{Solving, alike, deep, open, shallow, union_of};
use crate::{
    env::Env,
    expr::WrittenAt,
    stack::ensure_sufficient,
    typ::{TVar, Type, setops::strictly_identical},
};
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use netidx_value::Typ;
use smallvec::SmallVec;
use triomphe::Arc;

#[derive(Clone, Debug)]
pub enum Pat {
    /// Admits everything; binds it where named.
    Any(Option<TVar>),
    /// A run-time type test, then a structure.
    Test(Type, Box<Pat>),
    /// A literal of a primitive type: `whole` where the literal is every
    /// value of the type (`null`).
    Lit(Type, bool),
    Tuple(Option<TVar>, Vec<Pat>),
    Variant(Option<TVar>, ArcStr, Vec<Pat>),
    /// The named fields; `exhaustive` where the pattern names every field.
    Struct(Option<TVar>, Vec<(ArcStr, Pat)>, bool),
    /// An array or list by its elements: the whole, the elements written,
    /// and the name of the run the pattern leaves open, if it leaves one.
    Elems {
        all: Option<TVar>,
        list: bool,
        elems: Vec<Pat>,
        open: Option<Option<TVar>>,
    },
    Or(Vec<Pat>),
    /// A pattern, and a cell that takes the whole of what it admits.
    Whole(TVar, Box<Pat>),
}

impl Pat {
    /// The pattern with each cell it binds through `f`.
    pub(super) fn map_cells(&self, f: &mut dyn FnMut(&TVar) -> TVar) -> Pat {
        let named =
            |v: &Option<TVar>, f: &mut dyn FnMut(&TVar) -> TVar| v.as_ref().map(f);
        let all = |ps: &[Pat], f: &mut dyn FnMut(&TVar) -> TVar| -> Vec<Pat> {
            ps.iter().map(|p| p.map_cells(f)).collect()
        };
        match self {
            Pat::Any(v) => Pat::Any(named(v, f)),
            Pat::Test(t, p) => Pat::Test(t.clone(), Box::new(p.map_cells(f))),
            Pat::Lit(t, whole) => Pat::Lit(t.clone(), *whole),
            Pat::Tuple(v, ps) => {
                let v = named(v, f);
                Pat::Tuple(v, all(ps, f))
            }
            Pat::Variant(v, tag, ps) => {
                let v = named(v, f);
                Pat::Variant(v, tag.clone(), all(ps, f))
            }
            Pat::Struct(v, fs, exhaustive) => {
                let v = named(v, f);
                let fs = fs.iter().map(|(n, p)| (n.clone(), p.map_cells(f))).collect();
                Pat::Struct(v, fs, *exhaustive)
            }
            Pat::Elems { all: v, list, elems, open } => {
                let v = named(v, f);
                let elems = all(elems, f);
                let open = open.as_ref().map(|o| named(o, f));
                Pat::Elems { all: v, list: *list, elems, open }
            }
            Pat::Or(ps) => Pat::Or(all(ps, f)),
            Pat::Whole(v, p) => {
                let v = f(v);
                Pat::Whole(v, Box::new(p.map_cells(f)))
            }
        }
    }

    fn cells(&self, out: &mut SmallVec<[TVar; 4]>) {
        self.map_cells(&mut |v| {
            out.push(v.clone());
            v.clone()
        });
    }
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub pat: Pat,
    pub guarded: bool,
}

/// Inside a select `bool` is the union of two tags, so that `true` and
/// `false` arms each take their half.
pub const TRUE: ArcStr = literal!("#true");
pub const FALSE: ArcStr = literal!("#false");

fn tag(name: &ArcStr, parts: impl IntoIterator<Item = Type>) -> Type {
    Type::Variant(name.clone(), Arc::from_iter(parts), WrittenAt::default())
}

/// `t` with each part `f` answers for replaced, a part's own parts after
/// it; a union is rebuilt as one.
fn map_parts(t: &Type, f: &mut impl FnMut(&Type) -> Option<Type>) -> Type {
    ensure_sufficient(|| match f(t) {
        Some(t) => t,
        None => match t {
            Type::Set(ms) => union_of(ms.iter().map(|m| map_parts(m, f))),
            t => t
                .cow_children(&mut |c| Some(map_parts(c, f)))
                .unwrap_or_else(|| t.clone()),
        },
    })
}

pub fn split(t: &Type) -> Type {
    map_parts(t, &mut |t| match t {
        Type::Primitive(p) if p.contains(Typ::Bool) => {
            let halves = [tag(&TRUE, []), tag(&FALSE, [])];
            Some(union_of([Type::Primitive(*p & !Typ::Bool)].into_iter().chain(halves)))
        }
        _ => None,
    })
}

pub fn unsplit(t: &Type) -> Type {
    map_parts(t, &mut |t| match t {
        Type::Variant(name, _, _) if *name == TRUE || *name == FALSE => {
            Some(Type::Primitive(Typ::Bool.into()))
        }
        t => lengths(t).map(|(_, _, inner)| unsplit(&inner)),
    })
}

/// Inside a select, the arrays of a type that earlier arms left: those
/// whose length is not in `taken` (a bit per length under 64) and is under
/// `from`. Written as a variant around the array type.
const LENGTHS: &str = "#lengths:";

fn lengths(t: &Type) -> Option<(u64, usize, Type)> {
    match t {
        Type::Variant(name, ts, _) if name.starts_with(LENGTHS) && ts.len() == 1 => {
            let (taken, from) = name[LENGTHS.len()..].split_once(':')?;
            Some((taken.parse().ok()?, from.parse().ok()?, ts[0].clone()))
        }
        _ => None,
    }
}

fn left(array: &Type, taken: u64, from: usize) -> Type {
    match (taken, from) {
        (0, usize::MAX) => array.clone(),
        (taken, from) if from <= 64 && (0..from).all(|n| taken >> n & 1 == 1) => {
            Type::Bottom
        }
        (taken, from) => {
            let name = format_compact!("{LENGTHS}{taken}:{from}");
            tag(&ArcStr::from(name.as_str()), [array.clone()])
        }
    }
}

fn plain(t: Type) -> Type {
    lengths(&t).map(|l| l.2).unwrap_or(t)
}

fn has(t: &Type, bit: Typ) -> bool {
    matches!(t, Type::Primitive(p) if p.contains(bit))
}

fn nothing(t: &Type) -> bool {
    matches!(t, Type::Bottom) || matches!(t, Type::Primitive(p) if p.is_empty())
}

pub(super) struct Select {
    pub(super) arms: Vec<Arm>,
    hit: Vec<bool>,
    routed: bool,
    /// A copy made for an instance of a generic definition: an arm
    /// nothing reaches there is no dead arm.
    instance: bool,
    pub(super) waiting: Vec<Type>,
    /// Where the select is written, for a refusal.
    at: ArcStr,
    /// What was passed down the arms.
    seen: Vec<Type>,
}

/// What one pattern makes of one type: the part it admits, what it
/// leaves, and what its names are bound to.
struct Routed {
    part: Type,
    rest: Type,
    binds: Vec<(TVar, Type)>,
}

/// A pattern never looks into a function's type, so that may be open;
/// every other part must be decided.
fn routable(t: &Type) -> bool {
    match t {
        Type::TVar(tv) => tv.is_rigid(),
        Type::Fn(_) => true,
        Type::Array(t) | Type::List(t) | Type::Error(t) | Type::ByRef(_, t) => {
            routable(t)
        }
        Type::Map(m) => routable(&m.key) && routable(&m.value),
        Type::Tuple(ts) | Type::Variant(_, ts, _) | Type::Set(ts) => {
            ts.iter().all(routable)
        }
        Type::Abstract { params, .. } => params.iter().all(routable),
        Type::Ref(r) => r.params.iter().all(routable),
        Type::Struct(fs) => fs.iter().all(|(_, t, _)| routable(t)),
        _ => true,
    }
}

impl Solving {
    /// A select of `arms`, written at `at`: a use to register on its
    /// scrutinee.
    pub fn select(&mut self, arms: Vec<Arm>, at: ArcStr) -> usize {
        let hit = vec![false; arms.len()];
        self.selects.push(Select {
            arms,
            hit,
            routed: false,
            instance: false,
            waiting: vec![],
            at,
            seen: vec![],
        });
        self.selects.len() - 1
    }

    /// A copy of select `id` for an instance of the generic definition it
    /// is in, each cell its patterns bind through `f`.
    pub(super) fn select_copy(
        &mut self,
        id: usize,
        f: &mut dyn FnMut(&mut Self, &TVar) -> TVar,
    ) -> usize {
        let arms = self.selects[id].arms.clone();
        let arms: Vec<Arm> = arms
            .iter()
            .map(|a| Arm {
                pat: a.pat.map_cells(&mut |v| f(self, v)),
                guarded: a.guarded,
            })
            .collect();
        let new = self.select(arms, self.selects[id].at.clone());
        self.selects[new].instance = true;
        new
    }

    /// The cells a select's patterns bind.
    pub(super) fn select_cells(&self, id: usize) -> SmallVec<[TVar; 4]> {
        let mut out = SmallVec::new();
        self.selects[id].arms.iter().for_each(|a| a.pat.cells(&mut out));
        out
    }

    /// No value has the type: a product with such a part, at any depth. A
    /// named type is taken to have values.
    fn empty(&self, t: &Type) -> bool {
        match &shallow(t) {
            t if nothing(t) => true,
            Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.iter().any(|t| self.empty(t)),
            Type::Struct(fs) => fs.iter().any(|(_, t, _)| self.empty(t)),
            Type::Set(ms) => ms.iter().all(|t| self.empty(t)),
            _ => false,
        }
    }

    /// `t` with a typedef at its top unfolded; a type with no value is
    /// nothing.
    fn expand(&self, env: &Env, t: &Type) -> Type {
        let t = shallow(t);
        match &t {
            Type::Ref(_) => match t.lookup_ref(env) {
                Ok(e) => ensure_sufficient(|| self.expand(env, &e)),
                Err(_) => t,
            },
            t if self.empty(t) => Type::Bottom,
            _ => t,
        }
    }

    /// What a declared variable is known to be: its bounds, anything with
    /// none.
    fn bound_of(&self, env: &Env, v: &TVar) -> Type {
        v.cell_constraints()
            .iter()
            .filter(|u| {
                open(&shallow(u)).is_none()
                    && !matches!(u, Type::Bound(_))
                    && !u.is_trait_ref(env)
            })
            .fold(Type::Any, |b, u| self.both(env, true, &b, &split(&deep(u))))
    }

    /// The values in both `a` and `b`; where `b` is a `test`, the values
    /// of `a` that pass it, which is more where the test cannot tell.
    pub(super) fn both(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        ensure_sufficient(|| self.both_in(env, test, a, b))
    }

    fn both_in(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        let (a, b) = (plain(self.expand(env, a)), plain(self.expand(env, b)));
        let each = |xs: &[Type], ys: &[Type]| -> Option<Arc<[Type]>> {
            let ts: SmallVec<[Type; 4]> =
                xs.iter().zip(ys).map(|(x, y)| self.both(env, test, x, y)).collect();
            (!ts.iter().any(nothing)).then(|| Arc::from_iter(ts))
        };
        match (&a, &b) {
            _ if strictly_identical(&a, &b) => a,
            (Type::Any, t) | (t, Type::Any) => t.clone(),
            (Type::TVar(v), t) | (t, Type::TVar(v)) => {
                self.both(env, test, &self.bound_of(env, v), t)
            }
            (Type::Set(ms), t) => union_of(ms.iter().map(|m| self.both(env, test, m, t))),
            (t, Type::Set(ms)) => union_of(ms.iter().map(|m| self.both(env, test, t, m))),
            // a test tells a reference or a function from what is none,
            // and an abstract type made outside Graphix by its name: the
            // value keeps the type it had
            (Type::ByRef(..), Type::ByRef(..)) | (Type::Fn(_), Type::Fn(_)) if test => a,
            (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. })
                if test && x == y && env.abstract_reps.get(x).is_none() =>
            {
                a
            }
            (Type::Primitive(p), Type::Primitive(q)) if p.intersects(*q) => {
                Type::Primitive(*p & *q)
            }
            (Type::Error(_), p) | (p, Type::Error(_)) if has(p, Typ::Error) => match &a {
                Type::Error(_) => a,
                _ => b,
            },
            (Type::Array(_), p) | (p, Type::Array(_)) if has(p, Typ::Array) => match &a {
                Type::Array(_) => a,
                _ => b,
            },
            (Type::Array(x), Type::Array(y)) => {
                Type::Array(Arc::new(self.both(env, test, x, y)))
            }
            (Type::List(x), Type::List(y)) => {
                Type::List(Arc::new(self.both(env, test, x, y)))
            }
            (Type::Error(x), Type::Error(y)) => match self.both(env, test, x, y) {
                t if nothing(&t) => Type::Bottom,
                t => Type::Error(Arc::new(t)),
            },
            (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
                each(xs, ys).map(Type::Tuple).unwrap_or(Type::Bottom)
            }
            (Type::Variant(name, xs, at), Type::Variant(..)) if alike(&a, &b) => {
                let Type::Variant(_, ys, _) = &b else { unreachable!() };
                each(xs, ys)
                    .map(|ts| Type::Variant(name.clone(), ts, *at))
                    .unwrap_or(Type::Bottom)
            }
            (Type::Struct(xs), Type::Struct(ys)) if alike(&a, &b) => {
                let fs: SmallVec<[(ArcStr, Type, WrittenAt); 4]> = xs
                    .iter()
                    .zip(ys.iter())
                    .map(|(x, y)| (x.0.clone(), self.both(env, test, &x.1, &y.1), x.2))
                    .collect();
                match fs.iter().any(|(_, t, _)| nothing(t)) {
                    true => Type::Bottom,
                    false => Type::Struct(Arc::from_iter(fs)),
                }
            }
            _ => Type::Bottom,
        }
    }

    /// The values of `a` not in `b`; where `b` is a `test`, those that do
    /// not pass it.
    pub(super) fn less(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        ensure_sufficient(|| self.less_in(env, test, a, b))
    }

    fn less_in(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        let (a, b) = (plain(self.expand(env, a)), plain(self.expand(env, b)));
        // a product less a product: the products that differ from it in a
        // first component
        let product = |xs: &[Type], ys: &[Type], wrap: &dyn Fn(Arc<[Type]>) -> Type| {
            let mut out: SmallVec<[Type; 4]> = SmallVec::new();
            for i in 0..xs.len() {
                let left = self.less(env, test, &xs[i], &ys[i]);
                if !nothing(&left) {
                    let mut ts: SmallVec<[Type; 4]> = xs[..i]
                        .iter()
                        .zip(ys)
                        .map(|(x, y)| self.both(env, test, x, y))
                        .collect();
                    ts.push(left);
                    ts.extend(xs[i + 1..].iter().cloned());
                    if !ts.iter().any(nothing) {
                        out.push(wrap(Arc::from_iter(ts)))
                    }
                }
            }
            union_of(out)
        };
        match (&a, &b) {
            _ if strictly_identical(&a, &b) => Type::Bottom,
            (_, Type::Any) | (Type::Bottom, _) => Type::Bottom,
            (Type::TVar(v), _) => self.less(env, test, &self.bound_of(env, v), &b),
            (Type::Set(ms), _) => {
                union_of(ms.iter().map(|m| self.less(env, test, m, &b)))
            }
            (_, Type::Set(ms)) => {
                ms.iter().fold(a.clone(), |a, m| self.less(env, test, &a, m))
            }
            (Type::ByRef(..), Type::ByRef(..)) | (Type::Fn(_), Type::Fn(_)) if test => {
                Type::Bottom
            }
            (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. })
                if test && x == y && env.abstract_reps.get(x).is_none() =>
            {
                Type::Bottom
            }
            (Type::Primitive(p), Type::Primitive(q)) => match *p & !*q {
                left if left.is_empty() => Type::Bottom,
                left => Type::Primitive(left),
            },
            (Type::Error(_), p) if has(p, Typ::Error) => Type::Bottom,
            (Type::Array(_), p) if has(p, Typ::Array) => Type::Bottom,
            (Type::Array(x), Type::Array(y))
            | (Type::List(x), Type::List(y))
            | (Type::Error(x), Type::Error(y))
                if nothing(&self.less(env, test, x, y)) =>
            {
                Type::Bottom
            }
            (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
                product(xs, ys, &Type::Tuple)
            }
            (Type::Variant(name, xs, at), Type::Variant(..)) if alike(&a, &b) => {
                let Type::Variant(_, ys, _) = &b else { unreachable!() };
                product(xs, ys, &|ts| Type::Variant(name.clone(), ts, *at))
            }
            (Type::Struct(fs), Type::Struct(gs)) if alike(&a, &b) => {
                let (xs, ys): (SmallVec<[Type; 4]>, SmallVec<[Type; 4]>) = fs
                    .iter()
                    .zip(gs.iter())
                    .map(|(x, y)| (x.1.clone(), y.1.clone()))
                    .unzip();
                product(&xs, &ys, &|ts| {
                    let fields = fs.iter().zip(ts.iter());
                    Type::Struct(Arc::from_iter(
                        fields.map(|(f, t)| (f.0.clone(), t.clone(), f.2)),
                    ))
                })
            }
            _ => a,
        }
    }

    fn route(&self, env: &Env, pat: &Pat, t: &Type) -> Routed {
        ensure_sufficient(|| self.route_in(env, pat, t))
    }

    fn route_in(&self, env: &Env, pat: &Pat, t: &Type) -> Routed {
        let t = self.expand(env, t);
        if let Pat::Whole(v, p) = pat {
            let mut r = self.route(env, p, &t);
            if !nothing(&r.part) {
                r.binds.push((v.clone(), r.part.clone()))
            }
            return r;
        }
        // a pattern that looks into a declared variable sees its bound
        if let (Type::TVar(v), false) = (&t, matches!(pat, Pat::Any(_))) {
            return self.route(env, pat, &self.bound_of(env, v));
        }
        if let Type::Set(ms) = &t {
            let (mut parts, mut rests, mut binds) = (vec![], vec![], vec![]);
            for m in ms.iter() {
                let r = self.route(env, pat, m);
                parts.push(r.part);
                rests.push(r.rest);
                binds.extend(r.binds)
            }
            return Routed { part: union_of(parts), rest: union_of(rests), binds };
        }
        let named = |v: &Option<TVar>, part: &Type, mut binds: Vec<(TVar, Type)>| {
            if let Some(v) = v {
                binds.push((v.clone(), part.clone()))
            }
            binds
        };
        let none = || Routed { part: Type::Bottom, rest: t.clone(), binds: vec![] };
        // each sub-pattern over its component; the part is a product, the
        // rest the products that fail in a first component
        let product = |all: &Option<TVar>,
                       ps: &[&Pat],
                       xs: &[Type],
                       wrap: &dyn Fn(Vec<Type>) -> Type| {
            let rs: Vec<Routed> =
                ps.iter().zip(xs).map(|(p, x)| self.route(env, p, x)).collect();
            let mut rests = vec![];
            for (i, r) in rs.iter().enumerate() {
                if !nothing(&r.rest) {
                    let mut ts: Vec<Type> =
                        rs[..i].iter().map(|r| r.part.clone()).collect();
                    ts.push(r.rest.clone());
                    ts.extend(xs[i + 1..].iter().cloned());
                    if !ts.iter().any(nothing) {
                        rests.push(wrap(ts))
                    }
                }
            }
            match rs.iter().any(|r| nothing(&r.part)) {
                true => none(),
                false => {
                    let part = wrap(rs.iter().map(|r| r.part.clone()).collect());
                    let binds = rs.into_iter().flat_map(|r| r.binds).collect();
                    Routed {
                        binds: named(all, &part, binds),
                        part,
                        rest: union_of(rests),
                    }
                }
            }
        };
        let any = |n: usize| vec![Type::Any; n];
        match (pat, &t) {
            (_, t) if nothing(t) => none(),
            (Pat::Any(v), _) => Routed {
                part: t.clone(),
                rest: Type::Bottom,
                binds: named(v, &t, vec![]),
            },
            (Pat::Test(test, p), _) => {
                let test = split(test);
                let r = self.route(env, p, &self.both(env, true, &t, &test));
                let rest = union_of([self.less(env, true, &t, &test), r.rest.clone()]);
                Routed { rest, ..r }
            }
            (Pat::Lit(lit, whole), _) => {
                let rest = match whole {
                    true => self.less(env, true, &t, lit),
                    false => t.clone(),
                };
                Routed { part: self.both(env, true, &t, lit), rest, binds: vec![] }
            }
            (Pat::Or(ps), _) => {
                let (mut reach, mut parts, mut binds) = (t.clone(), vec![], vec![]);
                for p in ps {
                    let r = self.route(env, p, &reach);
                    parts.push(r.part);
                    binds.extend(r.binds);
                    reach = r.rest
                }
                Routed { part: union_of(parts), rest: reach, binds }
            }
            (Pat::Elems { all, list, elems, open: run }, _)
                if matches!(plain(t.clone()), Type::Array(_) | Type::List(_)) =>
            {
                // a pattern takes the arrays of its length, or of its
                // length and longer where it leaves a run open
                let (taken, from, array) =
                    lengths(&t).unwrap_or((0, usize::MAX, t.clone()));
                let e = match &array {
                    Type::Array(e) if !*list => e.clone(),
                    Type::List(e) if *list => e.clone(),
                    _ => return none(),
                };
                let n = elems.len();
                let gone = n >= from || (run.is_none() && n < 64 && taken >> n & 1 == 1);
                let rs: Vec<Routed> =
                    elems.iter().map(|p| self.route(env, p, &e)).collect();
                if gone || rs.iter().any(|r| nothing(&r.part)) {
                    return none();
                }
                let whole = rs.iter().all(|r| nothing(&r.rest));
                let mut binds: Vec<(TVar, Type)> =
                    rs.into_iter().flat_map(|r| r.binds).collect();
                if let Some(Some(v)) = run {
                    binds.push((v.clone(), array.clone()))
                }
                let rest = match (whole, run) {
                    (false, _) => t.clone(),
                    (true, Some(_)) => left(&array, taken, n.min(from)),
                    (true, None) if n < 64 => left(&array, taken | 1 << n, from),
                    (true, None) => t.clone(),
                };
                Routed { binds: named(all, &array, binds), part: array, rest }
            }
            (Pat::Elems { list, .. }, Type::Any) => {
                let e = Arc::new(Type::Any);
                let r = self.route(
                    env,
                    pat,
                    &if *list { Type::List(e) } else { Type::Array(e) },
                );
                Routed { rest: Type::Any, ..r }
            }
            (_, _) if lengths(&t).is_some() && !matches!(pat, Pat::Or(_)) => {
                self.route(env, pat, &plain(t.clone()))
            }
            (Pat::Tuple(all, ps), Type::Tuple(xs)) if ps.len() == xs.len() => {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| {
                    Type::Tuple(Arc::from(ts))
                })
            }
            (Pat::Tuple(all, ps), Type::Any) => {
                let r =
                    product(all, &ps.iter().collect::<Vec<_>>(), &any(ps.len()), &|ts| {
                        Type::Tuple(Arc::from(ts))
                    });
                Routed { rest: Type::Any, ..r }
            }
            (Pat::Variant(all, name, ps), Type::Variant(have, xs, at))
                if name == have && ps.len() == xs.len() =>
            {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| {
                    Type::Variant(name.clone(), Arc::from(ts), *at)
                })
            }
            (Pat::Variant(all, name, ps), Type::Any) => {
                let r =
                    product(all, &ps.iter().collect::<Vec<_>>(), &any(ps.len()), &|ts| {
                        tag(name, ts)
                    });
                Routed { rest: Type::Any, ..r }
            }
            (Pat::Struct(all, fs, exhaustive), Type::Struct(xs))
                if fs.iter().all(|(n, _)| xs.iter().any(|(m, ..)| m == n))
                    && (!exhaustive || fs.len() == xs.len()) =>
            {
                let ignore = Pat::Any(None);
                let ps: Vec<&Pat> = xs
                    .iter()
                    .map(|(n, ..)| {
                        fs.iter().find(|(m, _)| m == n).map(|(_, p)| p).unwrap_or(&ignore)
                    })
                    .collect();
                let ts: Vec<Type> = xs.iter().map(|(_, t, _)| t.clone()).collect();
                product(all, &ps, &ts, &|ts| {
                    let fields = xs.iter().zip(ts);
                    Type::Struct(Arc::from_iter(
                        fields.map(|(x, t)| (x.0.clone(), t, x.2)),
                    ))
                })
            }
            _ => none(),
        }
    }

    /// Pass one type that reaches a select's scrutinee down its arms.
    pub(super) fn select_route(&mut self, env: &Env, id: usize, t: &Type) -> Result<()> {
        let whole = deep(t);
        if !routable(&whole) {
            self.selects[id].waiting.push(t.clone());
            return Ok(());
        }
        let mut reach = self.expand(env, &split(&whole));
        if nothing(&reach) {
            return Ok(());
        }
        let (mut binds, mut hits) = (vec![], vec![]);
        for (i, arm) in self.selects[id].arms.iter().enumerate() {
            let r = self.route(env, &arm.pat, &reach);
            if !nothing(&r.part) {
                hits.push(i);
                binds.extend(r.binds)
            }
            if !arm.guarded {
                reach = r.rest
            }
        }
        if !nothing(&reach) {
            bail!(
                "missing match cases: nothing matches {} ({})",
                unsplit(&reach),
                self.selects[id].at
            )
        }
        self.selects[id].routed = true;
        self.selects[id].seen.push(unsplit(&whole));
        hits.into_iter().for_each(|i| self.selects[id].hit[i] = true);
        // a name takes every part routed to it, as one type
        let mut names: Vec<(TVar, Vec<Type>)> = vec![];
        for (v, t) in binds {
            match names.iter_mut().find(|(n, _)| n.same_cell(&v)) {
                Some((_, ts)) => ts.push(t),
                None => names.push((v, vec![t])),
            }
        }
        for (v, ts) in names {
            let t = self.absorbed(env, &unsplit(&union_of(ts)));
            self.constrain(env, &t, &Type::TVar(v))?
        }
        Ok(())
    }

    /// A decided union less each member another member holds: the part of
    /// `Array<string>` an `Array<i64>` test takes is the empty array, which
    /// `Array<i64>` holds.
    fn absorbed(&mut self, env: &Env, t: &Type) -> Type {
        let Type::Set(ms) = t else { return t.clone() };
        if t.has_free() {
            return t.clone();
        }
        let mut kept: Vec<Type> = vec![];
        for m in ms.iter() {
            let held =
                |s: &mut Self, m: &Type, by: &Type| alike(m, by) && s.attempt(env, m, by);
            if kept.iter().any(|k| held(self, m, k)) {
                continue;
            }
            kept.retain(|k| !held(self, k, m));
            kept.push(m.clone())
        }
        union_of(kept)
    }

    /// Route what waited for a cell; true if anything was.
    pub(super) fn select_waiting(
        &mut self,
        env: &Env,
        ready: &dyn Fn(&Self, usize, &Type) -> bool,
    ) -> Result<bool> {
        let mut progress = false;
        for id in 0..self.selects.len() {
            for t in std::mem::take(&mut self.selects[id].waiting) {
                if ready(self, id, &t) {
                    self.fix_in(env, &t)?;
                    self.select_route(env, id, &t)?;
                    progress = true
                } else {
                    self.selects[id].waiting.push(t)
                }
            }
        }
        Ok(progress)
    }

    /// An arm of a select that was judged and that nothing reaches.
    pub(super) fn dead_arms(&self) -> Result<()> {
        // a select still waiting is one of a generic definition, whose
        // instances give it its values
        let judged = |s: &&Select| s.routed && !s.instance && s.waiting.is_empty();
        for s in self.selects.iter().filter(judged) {
            if let Some(i) = s.hit.iter().position(|h| !h) {
                let seen: Vec<String> = s.seen.iter().map(|t| t.to_string()).collect();
                bail!(
                    "pattern {} will never match: nothing reaches it ({}; the value is {})",
                    i + 1,
                    s.at,
                    seen.join(" | ")
                )
            }
        }
        Ok(())
    }

    /// Have the select `id` wait on `t`.
    pub(super) fn select_wait(&mut self, id: usize, t: Type) {
        self.selects[id].waiting.push(t)
    }
}
