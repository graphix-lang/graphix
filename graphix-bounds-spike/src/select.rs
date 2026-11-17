//! Narrowing. A select is one use of its scrutinee: each ground lower bound
//! that reaches the scrutinee is passed down the arms, each arm keeps the part
//! its pattern admits and binds its names to that part, and what the last arm
//! leaves must be nothing. The set operations here are over ground types.

use crate::{
    solve::Solver,
    ty::{Ty, Var, union},
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use netidx_value::Typ;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Pat {
    /// Admits everything; binds it where named.
    Any(Option<Var>),
    /// A run-time type test, then a structure.
    Test(Ty, Box<Pat>),
    /// A literal of a primitive type: `whole` where the literal is every
    /// value of the type (`null`).
    Lit(Ty, bool),
    Tuple(Option<Var>, Vec<Pat>),
    Variant(Option<Var>, ArcStr, Vec<Pat>),
    /// The named fields; `exhaustive` where the pattern names every field.
    Struct(Option<Var>, Vec<(ArcStr, Pat)>, bool),
    /// An array or list by its elements: the whole, the elements written,
    /// and the name of the run the pattern leaves open, if it leaves one.
    Elems {
        all: Option<Var>,
        list: bool,
        elems: Vec<Pat>,
        open: Option<Option<Var>>,
    },
    Or(Vec<Pat>),
    /// An abstract type's constructor around what it holds.
    Abs(Option<Var>, ArcStr, Box<Pat>),
    /// A pattern, and a variable that takes the whole of what it admits.
    Whole(Var, Box<Pat>),
}

impl Pat {
    pub fn map_vars(&self, f: &mut impl FnMut(Var) -> Var) -> Pat {
        let mut all = |ps: &[Pat]| ps.iter().map(|p| p.map_vars(f)).collect::<Vec<_>>();
        match self {
            Pat::Any(v) => Pat::Any(v.map(&mut *f)),
            Pat::Test(t, p) => Pat::Test(t.clone(), Box::new(p.map_vars(f))),
            Pat::Lit(t, w) => Pat::Lit(t.clone(), *w),
            Pat::Tuple(v, ps) => {
                let ps = all(ps);
                Pat::Tuple(v.map(&mut *f), ps)
            }
            Pat::Variant(v, tag, ps) => {
                let ps = all(ps);
                Pat::Variant(v.map(&mut *f), tag.clone(), ps)
            }
            Pat::Struct(v, fs, ex) => {
                let fs = fs.iter().map(|(n, p)| (n.clone(), p.map_vars(f))).collect();
                Pat::Struct(v.map(&mut *f), fs, *ex)
            }
            Pat::Elems { all: v, list, elems, open } => {
                let elems = all(elems);
                Pat::Elems {
                    all: v.map(&mut *f),
                    list: *list,
                    elems,
                    open: open.map(|o| o.map(&mut *f)),
                }
            }
            Pat::Or(ps) => Pat::Or(all(ps)),
            Pat::Abs(v, tag, p) => {
                let p = Box::new(p.map_vars(f));
                Pat::Abs(v.map(&mut *f), tag.clone(), p)
            }
            Pat::Whole(v, p) => {
                let v = f(*v);
                Pat::Whole(v, Box::new(p.map_vars(f)))
            }
        }
    }

    /// The widest type the pattern admits, the bound an arm's test puts on
    /// an undeclared scrutinee; `None` where it admits anything.
    pub fn admits(&self) -> Option<Ty> {
        let all = |ps: &[Pat]| -> Rc<[Ty]> {
            ps.iter().map(|p| p.admits().unwrap_or(Ty::Any)).collect()
        };
        match self {
            Pat::Any(_) => None,
            Pat::Test(t, _) | Pat::Lit(t, _) => Some(t.clone()),
            Pat::Tuple(_, ps) => Some(Ty::Tuple(all(ps))),
            Pat::Variant(_, tag, ps) => Some(Ty::Variant(tag.clone(), all(ps))),
            Pat::Struct(_, fs, true) => Some(Ty::Struct(
                fs.iter()
                    .map(|(n, p)| (n.clone(), p.admits().unwrap_or(Ty::Any)))
                    .collect(),
            )),
            Pat::Struct(_, _, false) => None,
            Pat::Elems { list: false, .. } => Some(Ty::Array(Rc::new(Ty::Any))),
            Pat::Elems { list: true, .. } => Some(Ty::List(Rc::new(Ty::Any))),
            Pat::Or(ps) => {
                ps.iter().map(|p| p.admits()).collect::<Option<Vec<Ty>>>().map(union)
            }
            Pat::Abs(..) => None,
            Pat::Whole(_, p) => p.admits(),
        }
    }
}

#[derive(Clone)]
pub struct Arm {
    pub pat: Pat,
    pub guarded: bool,
}

/// Inside a select `bool` is the union of two tags, so that `true` and
/// `false` arms each take their half.
pub const TRUE: &str = "#true";
pub const FALSE: &str = "#false";

pub fn split(t: &Ty) -> Ty {
    t.map(&mut |t| match t {
        Ty::Prim(p) if p.contains(Typ::Bool) => {
            let halves = [TRUE, FALSE].map(|tag| Ty::Variant(tag.into(), [].into()));
            Some(union([Ty::Prim(*p & !Typ::Bool)].into_iter().chain(halves)))
        }
        _ => None,
    })
}

pub fn unsplit(t: &Ty) -> Ty {
    t.map(&mut |t| match t {
        Ty::Variant(tag, _) if tag == TRUE || tag == FALSE => Some(Ty::prim(Typ::Bool)),
        t => lengths(t).map(|(_, _, inner)| unsplit(&inner)),
    })
}

/// Inside a select, the arrays of a type that earlier arms left: those
/// whose length is not in `taken` (a bit per length under 64) and is under
/// `from`. Written as a variant around the array type.
const LENGTHS: &str = "#lengths:";

fn lengths(t: &Ty) -> Option<(u64, usize, Ty)> {
    match t {
        Ty::Variant(tag, ts) if tag.starts_with(LENGTHS) && ts.len() == 1 => {
            let (taken, from) = tag[LENGTHS.len()..].split_once(':')?;
            Some((taken.parse().ok()?, from.parse().ok()?, ts[0].clone()))
        }
        _ => None,
    }
}

fn left(array: &Ty, taken: u64, from: usize) -> Ty {
    match (taken, from) {
        (0, usize::MAX) => array.clone(),
        (taken, from) if from <= 64 && (0..from).all(|n| taken >> n & 1 == 1) => Ty::Bot,
        (taken, from) => {
            Ty::Variant(format!("{LENGTHS}{taken}:{from}").into(), [array.clone()].into())
        }
    }
}

pub struct Select {
    pub(crate) arms: Vec<Arm>,
    hit: Vec<bool>,
    routed: bool,
    /// A copy made for an instance of a generic definition: an arm nothing
    /// reaches there is no dead arm.
    instance: bool,
    waiting: Vec<Ty>,
    /// Where the select is written, for a refusal.
    pub at: String,
    /// What was passed down the arms.
    seen: Vec<Ty>,
}

/// What one pattern makes of one type: the part it admits, what it leaves,
/// and what its names are bound to.
struct Routed {
    part: Ty,
    rest: Ty,
    binds: Vec<(Var, Ty)>,
}

impl Solver {
    pub fn select(&mut self, arms: Vec<Arm>) -> usize {
        let hit = vec![false; arms.len()];
        self.selects.push(Select {
            arms,
            hit,
            routed: false,
            instance: false,
            waiting: vec![],
            at: String::new(),
            seen: vec![],
        });
        self.selects.len() - 1
    }

    /// A copy of select `of` with `arms`, made for an instance of the
    /// generic definition `of` is in.
    pub(crate) fn select_instance(&mut self, of: usize, arms: Vec<Arm>) -> usize {
        let id = self.select(arms);
        self.selects[id].instance = true;
        self.selects[id].at = self.selects[of].at.clone();
        id
    }

    /// What a select still waits to pass down its arms.
    pub(crate) fn select_waits(&self, id: usize) -> Vec<Ty> {
        self.selects[id].waiting.clone()
    }

    pub(crate) fn select_wait(&mut self, id: usize, t: Ty) {
        self.selects[id].waiting.push(t)
    }

    /// The variables a select's patterns bind.
    pub(crate) fn select_vars(&self, id: usize) -> Vec<Var> {
        let mut vs = vec![];
        for a in self.selects[id].arms.iter() {
            a.pat.map_vars(&mut |v| {
                vs.push(v);
                v
            });
        }
        vs
    }

    fn has(t: &Ty, bit: Typ) -> bool {
        matches!(t, Ty::Prim(p) if p.contains(bit))
    }

    /// No value has the type: a product with such a component, at any
    /// depth. A named type is taken to have values.
    fn empty(&self, t: &Ty) -> bool {
        match self.shallow(t) {
            Ty::Bot => true,
            Ty::Tuple(ts) | Ty::Variant(_, ts) => ts.iter().any(|t| self.empty(t)),
            Ty::Struct(fs) => fs.iter().any(|(_, t)| self.empty(t)),
            Ty::Union(ms) => ms.iter().all(|t| self.empty(t)),
            _ => false,
        }
    }

    /// `t` with a typedef at its top unfolded; a type with no value is
    /// nothing.
    fn expand(&self, t: &Ty) -> Ty {
        match self.shallow(t) {
            Ty::Named(id, args) => self.expand(&self.unfold(id, &args)),
            t if self.empty(&t) => Ty::Bot,
            t => t,
        }
    }

    /// The values in both `a` and `b`.
    pub fn both(&self, a: &Ty, b: &Ty) -> Ty {
        let plain = |t: Ty| lengths(&t).map(|l| l.2).unwrap_or(t);
        let (a, b) = (plain(self.expand(a)), plain(self.expand(b)));
        let each = |xs: &[Ty], ys: &[Ty]| -> Option<Rc<[Ty]>> {
            let ts: Rc<[Ty]> = xs.iter().zip(ys).map(|(x, y)| self.both(x, y)).collect();
            (!ts.iter().any(|t| matches!(t, Ty::Bot))).then_some(ts)
        };
        match (&a, &b) {
            _ if a == b => a,
            (Ty::Any, t) | (t, Ty::Any) => t.clone(),
            (Ty::Union(ms), t) | (t, Ty::Union(ms)) => {
                union(ms.iter().map(|m| self.both(m, t)))
            }
            (Ty::Prim(p), Ty::Prim(q)) if !(*p & *q).is_empty() => Ty::Prim(*p & *q),
            (Ty::Error(_), p) | (p, Ty::Error(_)) if Self::has(p, Typ::Error) => match &a
            {
                Ty::Error(_) => a,
                _ => b,
            },
            (Ty::Array(_), p) | (p, Ty::Array(_)) if Self::has(p, Typ::Array) => match &a
            {
                Ty::Array(_) => a,
                _ => b,
            },
            (Ty::Array(x), Ty::Array(y)) => Ty::Array(Rc::new(self.both(x, y))),
            (Ty::List(x), Ty::List(y)) => Ty::List(Rc::new(self.both(x, y))),
            (Ty::Error(x), Ty::Error(y)) => match self.both(x, y) {
                Ty::Bot => Ty::Bot,
                t => Ty::Error(Rc::new(t)),
            },
            (Ty::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => {
                each(xs, ys).map(Ty::Tuple).unwrap_or(Ty::Bot)
            }
            (Ty::Variant(t0, xs), Ty::Variant(t1, ys))
                if t0 == t1 && xs.len() == ys.len() =>
            {
                each(xs, ys).map(|ts| Ty::Variant(t0.clone(), ts)).unwrap_or(Ty::Bot)
            }
            (Ty::Struct(xs), Ty::Struct(ys)) if a.head() == b.head() => {
                let fs: Vec<_> = xs
                    .iter()
                    .zip(ys.iter())
                    .map(|((n, x), (_, y))| (n.clone(), self.both(x, y)))
                    .collect();
                match fs.iter().any(|(_, t)| matches!(t, Ty::Bot)) {
                    true => Ty::Bot,
                    false => Ty::Struct(fs.into()),
                }
            }
            _ => Ty::Bot,
        }
    }

    /// The values of `a` a test for `b` does not take.
    pub fn less(&self, a: &Ty, b: &Ty) -> Ty {
        let plain = |t: Ty| lengths(&t).map(|l| l.2).unwrap_or(t);
        let (a, b) = (plain(self.expand(a)), plain(self.expand(b)));
        // a product less a product: the tuples that differ from it in a first component
        let product = |xs: &[Ty], ys: &[Ty], wrap: &dyn Fn(Rc<[Ty]>) -> Ty| -> Ty {
            let mut out = vec![];
            for i in 0..xs.len() {
                let left = self.less(&xs[i], &ys[i]);
                if !matches!(left, Ty::Bot) {
                    let mut ts: Vec<Ty> =
                        xs[..i].iter().zip(ys).map(|(x, y)| self.both(x, y)).collect();
                    ts.push(left);
                    ts.extend(xs[i + 1..].iter().cloned());
                    if !ts.iter().any(|t| matches!(t, Ty::Bot)) {
                        out.push(wrap(ts.into()))
                    }
                }
            }
            union(out)
        };
        match (&a, &b) {
            _ if a == b => Ty::Bot,
            (_, Ty::Any) | (Ty::Bot, _) => Ty::Bot,
            (Ty::Union(ms), _) => union(ms.iter().map(|m| self.less(m, &b))),
            (_, Ty::Union(ms)) => ms.iter().fold(a.clone(), |a, m| self.less(&a, m)),
            (Ty::Prim(p), Ty::Prim(q)) => match *p & !*q {
                left if left.is_empty() => Ty::Bot,
                left => Ty::Prim(left),
            },
            (Ty::Error(_), p) if Self::has(p, Typ::Error) => Ty::Bot,
            (Ty::Array(_), p) if Self::has(p, Typ::Array) => Ty::Bot,
            (Ty::Array(x), Ty::Array(y))
            | (Ty::List(x), Ty::List(y))
            | (Ty::Error(x), Ty::Error(y))
                if matches!(self.less(x, y), Ty::Bot) =>
            {
                Ty::Bot
            }
            (Ty::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => {
                product(xs, ys, &|ts| Ty::Tuple(ts))
            }
            (Ty::Variant(t0, xs), Ty::Variant(t1, ys))
                if t0 == t1 && xs.len() == ys.len() =>
            {
                product(xs, ys, &|ts| Ty::Variant(t0.clone(), ts))
            }
            (Ty::Struct(xs), Ty::Struct(ys)) if a.head() == b.head() => {
                let names: Vec<ArcStr> = xs.iter().map(|(n, _)| n.clone()).collect();
                let (xs, ys): (Vec<Ty>, Vec<Ty>) = xs
                    .iter()
                    .zip(ys.iter())
                    .map(|((_, x), (_, y))| (x.clone(), y.clone()))
                    .unzip();
                product(&xs, &ys, &|ts| {
                    Ty::Struct(names.iter().cloned().zip(ts.iter().cloned()).collect())
                })
            }
            _ => a,
        }
    }

    fn route(&self, pat: &Pat, t: &Ty) -> Routed {
        let t = self.expand(t);
        // a pattern that looks into a declared variable sees its bound
        if let Pat::Whole(v, p) = pat {
            let mut r = self.route(p, &t);
            if !matches!(r.part, Ty::Bot) {
                r.binds.push((*v, r.part.clone()))
            }
            return r;
        }
        if let (Ty::Rigid(v), false) = (&t, matches!(pat, Pat::Any(_))) {
            let bound = self.vars[v.0 as usize]
                .upper
                .iter()
                .filter(|u| !matches!(u, Ty::Var(_) | Ty::Trait(_)))
                .fold(Ty::Any, |b, u| self.both(&b, &split(&self.deep(u))));
            return self.route(pat, &bound);
        }
        if let Ty::Union(ms) = &t {
            let (mut parts, mut rests, mut binds) = (vec![], vec![], vec![]);
            for m in ms.iter() {
                let r = self.route(pat, m);
                parts.push(r.part);
                rests.push(r.rest);
                binds.extend(r.binds)
            }
            return Routed { part: union(parts), rest: union(rests), binds };
        }
        let named = |v: &Option<Var>, part: &Ty, mut binds: Vec<(Var, Ty)>| {
            if let Some(v) = v {
                binds.push((*v, part.clone()))
            }
            binds
        };
        let nothing = || Routed { part: Ty::Bot, rest: t.clone(), binds: vec![] };
        // each sub-pattern over its component; the part is a product, the
        // rest the products that fail in a first component
        let product = |all: &Option<Var>,
                       ps: &[&Pat],
                       xs: &[Ty],
                       wrap: &dyn Fn(Vec<Ty>) -> Ty| {
            let rs: Vec<Routed> =
                ps.iter().zip(xs).map(|(p, x)| self.route(p, x)).collect();
            let mut rests = vec![];
            for (i, r) in rs.iter().enumerate() {
                if !matches!(r.rest, Ty::Bot) {
                    let mut ts: Vec<Ty> =
                        rs[..i].iter().map(|r| r.part.clone()).collect();
                    ts.push(r.rest.clone());
                    ts.extend(xs[i + 1..].iter().cloned());
                    if !ts.iter().any(|t| matches!(t, Ty::Bot)) {
                        rests.push(wrap(ts))
                    }
                }
            }
            match rs.iter().any(|r| matches!(r.part, Ty::Bot)) {
                true => nothing(),
                false => {
                    let part = wrap(rs.iter().map(|r| r.part.clone()).collect());
                    let binds = rs.into_iter().flat_map(|r| r.binds).collect();
                    Routed { binds: named(all, &part, binds), part, rest: union(rests) }
                }
            }
        };
        match (pat, &t) {
            (_, Ty::Bot) => nothing(),
            (Pat::Any(v), _) => {
                Routed { part: t.clone(), rest: Ty::Bot, binds: named(v, &t, vec![]) }
            }
            (Pat::Test(test, p), _) => {
                let test = split(test);
                let r = self.route(p, &self.both(&t, &test));
                Routed { rest: union([self.less(&t, &test), r.rest]), ..r }
            }
            (Pat::Lit(lit, whole), _) => {
                let rest = match whole {
                    true => self.less(&t, lit),
                    false => t.clone(),
                };
                Routed { part: self.both(&t, lit), rest, binds: vec![] }
            }
            (Pat::Abs(all, tag, p), Ty::Abs(t0, args)) if tag == t0 => {
                match self.rep(tag, args) {
                    None => nothing(),
                    Some(rep) => {
                        let r = self.route(p, &rep);
                        match (&r.part, &r.rest) {
                            (Ty::Bot, _) => nothing(),
                            (_, rest) => Routed {
                                part: t.clone(),
                                rest: if matches!(rest, Ty::Bot) {
                                    Ty::Bot
                                } else {
                                    t.clone()
                                },
                                binds: named(all, &t, r.binds),
                            },
                        }
                    }
                }
            }
            (Pat::Or(ps), _) => {
                let (mut reach, mut parts, mut binds) = (t.clone(), vec![], vec![]);
                for p in ps {
                    let r = self.route(p, &reach);
                    parts.push(r.part);
                    binds.extend(r.binds);
                    reach = r.rest
                }
                Routed { part: union(parts), rest: reach, binds }
            }
            (Pat::Elems { all, list, elems, open: run }, _)
                if matches!(
                    lengths(&t).map(|l| l.2).unwrap_or(t.clone()),
                    Ty::Array(_) | Ty::List(_)
                ) =>
            {
                // a pattern takes the arrays of its length, or of its
                // length and longer where it leaves a run open
                let (taken, from, array) =
                    lengths(&t).unwrap_or((0, usize::MAX, t.clone()));
                let e = match &array {
                    Ty::Array(e) if !*list => e.clone(),
                    Ty::List(e) if *list => e.clone(),
                    _ => return nothing(),
                };
                let n = elems.len();
                let gone = n >= from || (run.is_none() && n < 64 && taken >> n & 1 == 1);
                let rs: Vec<Routed> = elems.iter().map(|p| self.route(p, &e)).collect();
                if gone || rs.iter().any(|r| matches!(r.part, Ty::Bot)) {
                    return nothing();
                }
                let whole = rs.iter().all(|r| matches!(r.rest, Ty::Bot));
                let mut binds: Vec<(Var, Ty)> =
                    rs.into_iter().flat_map(|r| r.binds).collect();
                if let Some(Some(v)) = run {
                    binds.push((*v, array.clone()))
                }
                let rest = match (whole, run) {
                    (false, _) => t.clone(),
                    (true, Some(_)) => left(&array, taken, n.min(from)),
                    (true, None) if n < 64 => left(&array, taken | 1 << n, from),
                    (true, None) => t.clone(),
                };
                Routed { binds: named(all, &array, binds), part: array, rest }
            }
            (Pat::Elems { list, .. }, Ty::Any) => {
                let any = Rc::new(Ty::Any);
                let r =
                    self.route(pat, &if *list { Ty::List(any) } else { Ty::Array(any) });
                Routed { rest: Ty::Any, ..r }
            }
            (_, _) if lengths(&t).is_some() && !matches!(pat, Pat::Or(_)) => {
                let (_, _, array) = lengths(&t).unwrap();
                self.route(pat, &array)
            }
            (Pat::Tuple(all, ps), Ty::Tuple(xs)) if ps.len() == xs.len() => {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| {
                    Ty::Tuple(ts.into())
                })
            }
            (Pat::Tuple(all, ps), Ty::Any) => {
                let xs = vec![Ty::Any; ps.len()];
                let r = product(all, &ps.iter().collect::<Vec<_>>(), &xs, &|ts| {
                    Ty::Tuple(ts.into())
                });
                Routed { rest: Ty::Any, ..r }
            }
            (Pat::Variant(all, tag, ps), Ty::Variant(t0, xs))
                if tag == t0 && ps.len() == xs.len() =>
            {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| {
                    Ty::Variant(tag.clone(), ts.into())
                })
            }
            (Pat::Variant(all, tag, ps), Ty::Any) => {
                let xs = vec![Ty::Any; ps.len()];
                let r = product(all, &ps.iter().collect::<Vec<_>>(), &xs, &|ts| {
                    Ty::Variant(tag.clone(), ts.into())
                });
                Routed { rest: Ty::Any, ..r }
            }
            (Pat::Struct(all, fs, exhaustive), Ty::Struct(xs))
                if fs.iter().all(|(n, _)| xs.iter().any(|(m, _)| m == n))
                    && (!exhaustive || fs.len() == xs.len()) =>
            {
                let ignore = Pat::Any(None);
                let ps: Vec<&Pat> = xs
                    .iter()
                    .map(|(n, _)| {
                        fs.iter().find(|(m, _)| m == n).map(|(_, p)| p).unwrap_or(&ignore)
                    })
                    .collect();
                let ts: Vec<Ty> = xs.iter().map(|(_, t)| t.clone()).collect();
                product(all, &ps, &ts, &|ts| {
                    Ty::Struct(xs.iter().map(|(n, _)| n.clone()).zip(ts).collect())
                })
            }
            _ => nothing(),
        }
    }

    /// Pass one lower bound of a select's scrutinee down its arms.
    pub(crate) fn select_route(&mut self, id: usize, t: &Ty) -> Result<()> {
        if !self.is_ground(t) {
            self.selects[id].waiting.push(t.clone());
            return Ok(());
        }
        let mut reach = self.expand(&split(&self.deep(t)));
        let (mut binds, mut hits) = (vec![], vec![]);
        for (i, arm) in self.selects[id].arms.iter().enumerate() {
            let r = self.route(&arm.pat, &reach);
            if !matches!(r.part, Ty::Bot) {
                hits.push(i);
                binds.extend(r.binds)
            }
            if !arm.guarded {
                reach = r.rest
            }
        }
        if !matches!(reach, Ty::Bot) {
            if std::env::var("SPIKE_PANIC").is_ok() {
                panic!("missing match cases: {reach}")
            }
            bail!(
                "missing match cases: nothing matches {} ({})",
                unsplit(&reach),
                self.selects[id].at
            )
        }
        self.selects[id].routed = true;
        let whole = unsplit(&self.deep(t));
        self.selects[id].seen.push(whole);
        hits.into_iter().for_each(|i| self.selects[id].hit[i] = true);
        // a name takes every part routed to it, as one type
        let mut names: Vec<(Var, Vec<Ty>)> = vec![];
        for (v, t) in binds {
            match names.iter_mut().find(|(n, _)| *n == v) {
                Some((_, ts)) => ts.push(t),
                None => names.push((v, vec![t])),
            }
        }
        for (v, ts) in names {
            let t = self.absorbed(&unsplit(&union(ts)));
            self.constrain(&t, &Ty::Var(v))?
        }
        Ok(())
    }

    /// A ground union less each member another member holds: the part of
    /// `Array<string>` an `Array<i64>` test takes is the empty array, which
    /// `Array<i64>` holds.
    fn absorbed(&mut self, t: &Ty) -> Ty {
        let Ty::Union(ms) = t else { return t.clone() };
        if !self.is_ground(t) {
            return t.clone();
        }
        let mut kept: Vec<Ty> = vec![];
        for m in ms.iter() {
            let held = |s: &mut Self, m: &Ty, by: &Ty| {
                m.head() == by.head() && s.constrain(m, by).is_ok()
            };
            if kept.iter().any(|k| held(self, m, k)) {
                continue;
            }
            kept.retain(|k| !held(self, k, m));
            kept.push(m.clone())
        }
        union(kept)
    }

    /// Route what waited for a variable; true if anything was.
    pub(crate) fn select_waiting(
        &mut self,
        ready: &dyn Fn(&Self, usize, &Ty) -> bool,
    ) -> Result<bool> {
        let mut progress = false;
        for id in 0..self.selects.len() {
            for t in std::mem::take(&mut self.selects[id].waiting) {
                if ready(self, id, &t) {
                    self.fix_in(&t)?;
                    self.select_route(id, &t)?;
                    progress = true
                } else {
                    self.selects[id].waiting.push(t)
                }
            }
        }
        Ok(progress)
    }

    /// An arm of a select that was judged and that nothing reaches.
    pub(crate) fn dead_arms(&self) -> Result<()> {
        for s in self.selects.iter().filter(|s| s.routed && !s.instance) {
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
}
