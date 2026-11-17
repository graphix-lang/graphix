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
                fs.iter().map(|(n, p)| (n.clone(), p.admits().unwrap_or(Ty::Any))).collect(),
            )),
            Pat::Struct(_, _, false) => None,
        }
    }
}

#[derive(Clone)]
pub struct Arm {
    pub pat: Pat,
    pub guarded: bool,
    /// The boolean a literal arm tests, so `true` and `false` together
    /// leave no bool.
    pub boolean: Option<bool>,
}

pub struct Select {
    pub arms: Vec<Arm>,
    hit: Vec<bool>,
    routed: bool,
    /// A copy made for an instance of a generic definition: an arm nothing
    /// reaches there is no dead arm.
    instance: bool,
    waiting: Vec<Ty>,
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
        self.selects.push(Select { arms, hit, routed: false, instance: false, waiting: vec![] });
        self.selects.len() - 1
    }

    pub(crate) fn select_instance(&mut self, arms: Vec<Arm>) -> usize {
        let id = self.select(arms);
        self.selects[id].instance = true;
        id
    }

    pub(crate) fn select_arms(&self, id: usize) -> Vec<Arm> {
        self.selects[id].arms.clone()
    }

    fn has(t: &Ty, bit: Typ) -> bool {
        matches!(t, Ty::Prim(p) if p.contains(bit))
    }

    fn expand(&self, t: &Ty) -> Ty {
        match self.shallow(t) {
            Ty::Named(id, args) => self.expand(&self.unfold(id, &args)),
            t => t,
        }
    }

    /// The values in both `a` and `b`.
    pub fn both(&self, a: &Ty, b: &Ty) -> Ty {
        let (a, b) = (self.expand(a), self.expand(b));
        let each = |xs: &[Ty], ys: &[Ty]| -> Option<Rc<[Ty]>> {
            let ts: Rc<[Ty]> = xs.iter().zip(ys).map(|(x, y)| self.both(x, y)).collect();
            (!ts.iter().any(|t| matches!(t, Ty::Bot))).then_some(ts)
        };
        match (&a, &b) {
            _ if a == b => a,
            (Ty::Any, t) | (t, Ty::Any) => t.clone(),
            (Ty::Union(ms), t) | (t, Ty::Union(ms)) => union(ms.iter().map(|m| self.both(m, t))),
            (Ty::Prim(p), Ty::Prim(q)) if !(*p & *q).is_empty() => Ty::Prim(*p & *q),
            (Ty::Error(_), p) | (p, Ty::Error(_)) if Self::has(p, Typ::Error) => match &a {
                Ty::Error(_) => a,
                _ => b,
            },
            (Ty::Array(_), p) | (p, Ty::Array(_)) if Self::has(p, Typ::Array) => match &a {
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
            (Ty::Variant(t0, xs), Ty::Variant(t1, ys)) if t0 == t1 && xs.len() == ys.len() => {
                each(xs, ys).map(|ts| Ty::Variant(t0.clone(), ts)).unwrap_or(Ty::Bot)
            }
            (Ty::Struct(xs), Ty::Struct(ys)) if a.head() == b.head() => {
                let fs: Vec<_> =
                    xs.iter().zip(ys.iter()).map(|((n, x), (_, y))| (n.clone(), self.both(x, y))).collect();
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
        let (a, b) = (self.expand(a), self.expand(b));
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
            (Ty::Variant(t0, xs), Ty::Variant(t1, ys)) if t0 == t1 && xs.len() == ys.len() => {
                product(xs, ys, &|ts| Ty::Variant(t0.clone(), ts))
            }
            (Ty::Struct(xs), Ty::Struct(ys)) if a.head() == b.head() => {
                let names: Vec<ArcStr> = xs.iter().map(|(n, _)| n.clone()).collect();
                let (xs, ys): (Vec<Ty>, Vec<Ty>) =
                    xs.iter().zip(ys.iter()).map(|((_, x), (_, y))| (x.clone(), y.clone())).unzip();
                product(&xs, &ys, &|ts| {
                    Ty::Struct(names.iter().cloned().zip(ts.iter().cloned()).collect())
                })
            }
            _ => a,
        }
    }

    fn route(&self, pat: &Pat, t: &Ty) -> Routed {
        let t = self.expand(t);
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
        let product = |all: &Option<Var>, ps: &[&Pat], xs: &[Ty], wrap: &dyn Fn(Vec<Ty>) -> Ty| {
            let rs: Vec<Routed> = ps.iter().zip(xs).map(|(p, x)| self.route(p, x)).collect();
            let mut rests = vec![];
            for (i, r) in rs.iter().enumerate() {
                if !matches!(r.rest, Ty::Bot) {
                    let mut ts: Vec<Ty> = rs[..i].iter().map(|r| r.part.clone()).collect();
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
                let r = self.route(p, &self.both(&t, test));
                Routed { rest: union([self.less(&t, test), r.rest]), ..r }
            }
            (Pat::Lit(lit, whole), _) => {
                let rest = match whole {
                    true => self.less(&t, lit),
                    false => t.clone(),
                };
                Routed { part: self.both(&t, lit), rest, binds: vec![] }
            }
            (Pat::Tuple(all, ps), Ty::Tuple(xs)) if ps.len() == xs.len() => {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| Ty::Tuple(ts.into()))
            }
            (Pat::Tuple(all, ps), Ty::Any) => {
                let xs = vec![Ty::Any; ps.len()];
                let r = product(all, &ps.iter().collect::<Vec<_>>(), &xs, &|ts| Ty::Tuple(ts.into()));
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
                    .map(|(n, _)| fs.iter().find(|(m, _)| m == n).map(|(_, p)| p).unwrap_or(&ignore))
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
        let mut reach = self.deep(t);
        let (mut seen, mut binds, mut hits) = ([false, false], vec![], vec![]);
        for (i, arm) in self.selects[id].arms.iter().enumerate() {
            let r = self.route(&arm.pat, &reach);
            if !matches!(r.part, Ty::Bot) {
                hits.push(i);
                binds.extend(r.binds);
                if let (Some(b), false) = (arm.boolean, arm.guarded) {
                    seen[b as usize] = true
                }
            }
            if !arm.guarded {
                reach = r.rest;
                if seen == [true, true] {
                    reach = self.less(&reach, &Ty::prim(Typ::Bool))
                }
            }
        }
        if !matches!(reach, Ty::Bot) {
            bail!("missing match cases: nothing matches {reach}")
        }
        self.selects[id].routed = true;
        hits.into_iter().for_each(|i| self.selects[id].hit[i] = true);
        binds.iter().try_for_each(|(v, t)| self.constrain(t, &Ty::Var(*v)))
    }

    /// Route what waited for a variable; true if anything was.
    pub(crate) fn select_waiting(&mut self, fixable: &dyn Fn(&Self, &Ty) -> bool) -> Result<bool> {
        let mut progress = false;
        for id in 0..self.selects.len() {
            for t in std::mem::take(&mut self.selects[id].waiting) {
                if fixable(self, &t) {
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
                bail!("pattern {} will never match: nothing reaches it", i + 1)
            }
        }
        Ok(())
    }
}
