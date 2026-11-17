use crate::{
    select::{Arm, Select},
    ty::{FnArg, FnTy, Ty, Var, union},
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use netidx_value::Typ;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

/// Whether a variable's lower bounds may form a union nobody wrote. An
/// array literal's element and a select's result join; a parameter, an
/// instantiated quantifier and a binding do not: one of their lower bounds
/// must hold the others.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
    Join,
    Single,
}

/// A use that needs the shape of what flows into a variable. It is applied
/// to each concrete lower bound as it arrives.
/// A call's argument: its label, its type and where it is written.
pub type Arg = (Option<ArcStr>, Ty, Option<Site>);

#[derive(Clone, Debug)]
pub enum Watch {
    Field { field: ArcStr, out: Ty },
    Index { i: usize, out: Ty },
    Call { args: Vec<Arg>, out: Ty },
    Select(usize),
}

impl Watch {
    fn needs(&self) -> &'static str {
        match self {
            Watch::Field { .. } => "reading a struct",
            Watch::Index { .. } => "reading a tuple",
            Watch::Call { .. } => "reading a fn",
            Watch::Select(_) => "a select",
        }
    }
}

pub struct VarInfo {
    pub lower: Vec<Ty>,
    pub upper: Vec<Ty>,
    pub level: u32,
    pub kind: Kind,
    pub singleton: bool,
    pub quantified: bool,
    pub fixed: Option<Ty>,
    /// Fixed with nothing flowing in, to what its uses demand: as a lower
    /// bound of another variable it says nothing.
    demanded: bool,
    /// The lower bounds that reached this variable out of a joining
    /// variable's bounds: those of one source are one type.
    joined: Vec<(Var, Ty)>,
    /// The variables at this one's level that flow into it. What flows
    /// into them is passed on as it arrives, so this is a record for
    /// fixing, never walked when a bound is added.
    feeders: Vec<Var>,
    /// Bound by a pattern over an `Any` part: with no use it is `Any`.
    over_any: bool,
    watches: Vec<(Watch, bool)>,
}

pub struct Def {
    pub params: Vec<Var>,
    pub body: Ty,
}

/// Where in the source a bound was made: line and column.
pub type Site = (u32, u32);

/// Both ends of a refusal: where the value comes from and where it is used.
#[derive(Debug)]
pub struct Ends(pub Option<Site>, pub Option<Site>);

impl std::fmt::Display for Ends {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let at = |s: &Option<Site>| match s {
            Some((l, c)) => format!("line {l}, column {c}"),
            None => "a signature".to_string(),
        };
        write!(f, "the value comes from {}, and is used at {}", at(&self.0), at(&self.1))
    }
}

/// The two values a variable may not hold together.
#[derive(Debug)]
pub struct Two(pub Option<Site>, pub Option<Site>);

impl std::fmt::Display for Two {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let at = |s: &Option<Site>| match s {
            Some((l, c)) => format!("line {l}, column {c}"),
            None => "elsewhere".to_string(),
        };
        write!(f, "one value comes from {}, the other from {}: declare the type that holds both", at(&self.0), at(&self.1))
    }
}

#[derive(Default)]
pub struct Solver {
    /// The expression being checked.
    pub at: Option<Site>,
    /// Where each bound was first made: (variable, is a lower bound, bound).
    sites: HashMap<(Var, bool, Ty), Site>,
    pub vars: Vec<VarInfo>,
    pub defs: Vec<Def>,
    cache: HashSet<(Ty, Ty)>,
    /// A type below a union with more than one member that could take it,
    /// waiting for both to be ground; true where a scheme's variables hold
    /// it, so each instance takes a copy.
    held: Vec<(Ty, Ty, bool)>,
    fixing: HashSet<Var>,
    /// The joining variable whose bounds are being passed on.
    via: Option<Var>,
    pub selects: Vec<Select>,
    /// What each abstract type is made of, where that has been seen.
    pub reps: HashMap<ArcStr, Def>,
    /// The operand and the result of a `?` or `$` whose operand's type is
    /// not known yet.
    /// The third is where the payloads of the errors it raises go, for `?`.
    pub strips: Vec<(Ty, Ty, Option<Ty>)>,
    /// A held choice is being resolved: a decided member that does not fit
    /// is out.
    resolving: bool,
    /// How many relations were taken apart, the prototype's cost measure.
    pub steps: u64,
}

fn mismatch<T>(container: &Ty, t: &Ty) -> Result<T> {
    bail!("type mismatch {container} does not contain {t}")
}

impl Solver {
    pub fn fresh(&mut self, level: u32, kind: Kind) -> Var {
        self.vars.push(VarInfo {
            lower: vec![],
            upper: vec![],
            level,
            kind,
            singleton: false,
            quantified: false,
            fixed: None,
            demanded: false,
            joined: vec![],
            feeders: vec![],
            over_any: false,
            watches: vec![],
        });
        Var(self.vars.len() as u32 - 1)
    }

    pub(crate) fn over_any(&mut self, v: Var) {
        self.info_mut(v).over_any = true
    }

    pub fn fresh_ty(&mut self, level: u32, kind: Kind) -> Ty {
        Ty::Var(self.fresh(level, kind))
    }

    fn info(&self, v: Var) -> &VarInfo {
        &self.vars[v.0 as usize]
    }

    fn info_mut(&mut self, v: Var) -> &mut VarInfo {
        &mut self.vars[v.0 as usize]
    }

    /// `t` with a fixed variable at its top replaced by what it was fixed to.
    pub fn shallow(&self, t: &Ty) -> Ty {
        match t {
            Ty::Var(v) => match &self.info(*v).fixed {
                Some(t) => self.shallow(t),
                None => t.clone(),
            },
            t => t.clone(),
        }
    }

    /// `t` with every fixed variable replaced.
    pub fn deep(&self, t: &Ty) -> Ty {
        t.map(&mut |t| match t {
            Ty::Var(v) => self.info(*v).fixed.as_ref().map(|t| self.deep(t)),
            _ => None,
        })
    }

    fn open_vars(&self, t: &Ty) -> Vec<Var> {
        let mut vs = vec![];
        self.deep(t).each(&mut |t| {
            if let Ty::Var(v) = t
                && !vs.contains(v)
            {
                vs.push(*v)
            }
        });
        vs
    }

    pub(crate) fn is_ground(&self, t: &Ty) -> bool {
        self.open_vars(t).is_empty()
    }

    fn level(&self, t: &Ty) -> u32 {
        let mut l = 0;
        self.deep(t).each(&mut |t| {
            if let Ty::Var(v) | Ty::Rigid(v) = t {
                l = l.max(self.info(*v).level)
            }
        });
        l
    }

    /// What a value of the abstract type `tag` at `args` holds.
    pub fn rep(&self, tag: &ArcStr, args: &[Ty]) -> Option<Ty> {
        let def = self.reps.get(tag)?;
        Some(def.body.map(&mut |t| match t {
            Ty::Var(v) => def.params.iter().position(|p| p == v).map(|i| args[i].clone()),
            _ => None,
        }))
    }

    pub(crate) fn unfold(&self, id: u32, args: &[Ty]) -> Ty {
        let def = &self.defs[id as usize];
        def.body.map(&mut |t| match t {
            Ty::Var(v) => def.params.iter().position(|p| p == v).map(|i| args[i].clone()),
            _ => None,
        })
    }

    /// `a` fits where `b` is wanted.
    pub fn constrain(&mut self, a: &Ty, b: &Ty) -> Result<()> {
        self.steps += 1;
        let (a, b) = (self.shallow(a), self.shallow(b));
        if a == b {
            return Ok(());
        }
        match (&a, &b) {
            (Ty::Bot, _) | (_, Ty::Any) => Ok(()),
            (Ty::Var(v), _) | (_, Ty::Var(v)) => {
                if !self.cache.insert((a.clone(), b.clone())) {
                    return Ok(());
                }
                let lvl = self.info(*v).level;
                match (&a, &b) {
                    (Ty::Var(v), _) if self.level(&b) <= lvl => {
                        self.info_mut(*v).upper.push(b.clone());
                        self.site(*v, false, &b);
                        match &b {
                            Ty::Var(w) if self.info(*w).level == lvl => {
                                self.info_mut(*w).feeders.push(*v)
                            }
                            Ty::Var(_) => (),
                            b => self.demanded(*v, b)?,
                        }
                        let via = self.pass_on(*v);
                        let mine = self.via;
                        for l in self.info(*v).lower.clone() {
                            let from = self.sites.get(&(*v, true, l.clone())).copied();
                            // a bound that came here out of a join keeps its source
                            // (a join passes everything on as its own)
                            let src = self.info(*v).joined.iter().find(|(_, j)| j == &l).map(|(s, _)| *s);
                            self.via = match self.info(*v).kind {
                                Kind::Join => mine,
                                Kind::Single => src.or(mine),
                            };
                            self.constrain(&l, &b).map_err(|e| self.ends(e, from, self.at))?
                        }
                        self.via = via;
                        Ok(())
                    }
                    (Ty::Var(_), Ty::Var(w)) if self.info(*w).level >= lvl => {
                        self.arrive(*w, &a)
                    }
                    (Ty::Var(_), _) => {
                        let b = self.extrude(&b, false, lvl, &mut HashMap::new());
                        self.constrain(&a, &b)
                    }
                    (_, Ty::Var(w)) if self.level(&a) <= lvl => self.arrive(*w, &a),
                    (_, _) => {
                        let a = self.extrude(&a, true, lvl, &mut HashMap::new());
                        self.constrain(&a, &b)
                    }
                }
            }
            (Ty::Union(xs), Ty::Union(ys))
                if ys.iter().filter(|m| matches!(m, Ty::Var(_))).count() == 1 =>
            {
                // what no closed member of `b` takes goes to its open member as one type
                let closed = |x: &Ty| {
                    ys.iter().any(|m| !matches!(m, Ty::Var(_)) && m.head() == x.head())
                };
                let mut rest = vec![];
                for x in xs.iter() {
                    match x {
                        Ty::Prim(p) => {
                            let left = ys.iter().fold(*p, |left, m| match m {
                                Ty::Prim(q) => left & !*q,
                                _ => left,
                            });
                            if !left.is_empty() {
                                rest.push(Ty::Prim(left))
                            }
                        }
                        x if ys.contains(x) => (),
                        x if closed(x) => self.constrain(x, &b)?,
                        x => rest.push(x.clone()),
                    }
                }
                let open = ys.iter().find(|m| matches!(m, Ty::Var(_))).unwrap();
                match rest.is_empty() {
                    true => Ok(()),
                    false => self.constrain(&union(rest), open),
                }
            }
            (Ty::Union(ms), _) => ms.iter().try_for_each(|m| self.constrain(m, &b)),
            (Ty::Named(id, args), _) | (_, Ty::Named(id, args)) => {
                if !self.cache.insert((a.clone(), b.clone())) {
                    return Ok(());
                }
                let t = self.unfold(*id, args);
                match &a {
                    Ty::Named(..) => self.constrain(&t, &b),
                    _ => self.constrain(&a, &t),
                }
            }
            (_, Ty::Union(ms)) => self.below_union(&a, ms, &b),
            (Ty::Rigid(v), _) => {
                let bounds = self.info(*v).upper.clone();
                match bounds.iter().any(|u| self.constrain(u, &b).is_ok()) {
                    true => Ok(()),
                    false => mismatch(&b, &a),
                }
            }
            (Ty::Prim(x), Ty::Prim(y)) if y.contains(*x) => Ok(()),
            (Ty::Error(_), Ty::Prim(y)) if y.contains(Typ::Error) => Ok(()),
            (Ty::Array(_), Ty::Prim(y)) if y.contains(Typ::Array) => Ok(()),
            (Ty::Array(x), Ty::Array(y))
            | (Ty::List(x), Ty::List(y))
            | (Ty::Error(x), Ty::Error(y))
            | (Ty::Ref(_, x), Ty::Ref(false, y)) => self.constrain(x, y),
            (Ty::Ref(true, x), Ty::Ref(true, y)) => {
                self.constrain(x, y)?;
                self.constrain(y, x)
            }
            (Ty::Map(k0, v0), Ty::Map(k1, v1)) => {
                self.constrain(k0, k1)?;
                self.constrain(v0, v1)
            }
            (Ty::Tuple(xs), Ty::Tuple(ys)) if xs.len() == ys.len() => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(x, y))
            }
            (Ty::Abs(t0, xs), Ty::Abs(t1, ys)) if t0 == t1 && xs.len() == ys.len() => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(x, y))
            }
            (Ty::Variant(t0, xs), Ty::Variant(t1, ys)) if t0 == t1 && xs.len() == ys.len() => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(x, y))
            }
            (Ty::Struct(xs), Ty::Struct(ys)) if a.head() == b.head() => xs
                .iter()
                .zip(ys.iter())
                .try_for_each(|((_, x), (_, y))| self.constrain(x, y)),
            (Ty::Fn(f), Ty::Fn(g)) => self.fn_below(f, g),
            _ => mismatch(&b, &a),
        }
    }

    /// `a` is a new lower bound of `w`.
    fn site(&mut self, v: Var, lower: bool, t: &Ty) {
        if let Some(at) = self.at {
            self.sites.entry((v, lower, t.clone())).or_insert(at);
        }
    }

    fn ends(&self, e: anyhow::Error, from: Option<Site>, to: Option<Site>) -> anyhow::Error {
        match e.downcast_ref::<Ends>() {
            Some(_) => e,
            None => e.context(Ends(from, to)),
        }
    }

    fn arrive(&mut self, w: Var, a: &Ty) -> Result<()> {
        self.info_mut(w).lower.push(a.clone());
        self.site(w, true, a);
        if let Some(src) = self.via
            && src != w
        {
            self.info_mut(w).joined.push((src, a.clone()))
        }
        if !matches!(a, Ty::Var(_)) {
            for i in 0..self.info(w).watches.len() {
                let watch = self.info(w).watches[i].0.clone();
                if self.apply(&watch, a)? {
                    self.info_mut(w).watches[i].1 = true
                }
            }
        }
        let via = self.pass_on(w);
        for u in self.info(w).upper.clone() {
            let to = self.sites.get(&(w, false, u.clone())).copied();
            self.constrain(a, &u).map_err(|e| self.ends(e, self.at, to))?
        }
        self.via = via;
        Ok(())
    }

    /// Mark what follows as `v`'s bounds passed on, where `v` joins; the
    /// mark to restore after.
    fn pass_on(&mut self, v: Var) -> Option<Var> {
        match self.info(v).kind {
            Kind::Join => self.via.replace(v),
            Kind::Single => self.via,
        }
    }

    fn decided(&self, v: Var, seen: &mut Vec<Var>) -> bool {
        seen.push(v);
        self.info(v).fixed.is_some()
            || self.info(v).lower.iter().any(|l| match l {
                Ty::Var(u) => !seen.contains(u) && self.decided(*u, seen),
                _ => true,
            })
            || self.info(v).feeders.clone().iter().any(|u| !seen.contains(u) && self.decided(*u, seen))
    }

    /// `b` is a new upper bound of `v`: a use that needs a shape takes it
    /// from a shape demanded of `v`, since nothing wider can flow in.
    fn demanded(&mut self, v: Var, b: &Ty) -> Result<()> {
        for i in 0..self.info(v).watches.len() {
            let watch = self.info(v).watches[i].0.clone();
            let shaped = matches!(
                (&watch, b),
                (Watch::Field { .. }, Ty::Struct(_))
                    | (Watch::Index { .. }, Ty::Tuple(_))
                    | (Watch::Call { .. }, Ty::Fn(_))
            );
            if shaped && self.apply(&watch, b)? {
                self.info_mut(v).watches[i].1 = true
            }
        }
        Ok(())
    }

    /// A copy of `t` no deeper than `lvl`: each deeper variable gets a twin
    /// at `lvl` per polarity, joined to it by a bound.
    fn extrude(&mut self, t: &Ty, pos: bool, lvl: u32, memo: &mut HashMap<(Var, bool), Var>) -> Ty {
        if self.level(t) <= lvl {
            return t.clone();
        }
        match self.shallow(t) {
            Ty::Var(v) => {
                if let Some(n) = memo.get(&(v, pos)) {
                    return Ty::Var(*n);
                }
                let n = self.fresh(lvl, self.info(v).kind);
                self.info_mut(n).singleton = self.info(v).singleton;
                memo.insert((v, pos), n);
                if pos {
                    self.info_mut(v).upper.push(Ty::Var(n));
                    let ls = self.info(v).lower.clone();
                    let ls = ls.iter().map(|l| self.extrude(l, pos, lvl, memo)).collect();
                    self.info_mut(n).lower = ls
                } else {
                    self.info_mut(v).lower.push(Ty::Var(n));
                    let us = self.info(v).upper.clone();
                    let us = us.iter().map(|u| self.extrude(u, pos, lvl, memo)).collect();
                    self.info_mut(n).upper = us
                }
                Ty::Var(n)
            }
            Ty::Fn(ft) => Ty::Fn(Rc::new(FnTy {
                args: ft
                    .args
                    .iter()
                    .map(|a| FnArg { ty: self.extrude(&a.ty, !pos, lvl, memo), ..a.clone() })
                    .collect(),
                vargs: ft.vargs.as_ref().map(|t| self.extrude(t, !pos, lvl, memo)),
                ret: self.extrude(&ft.ret, pos, lvl, memo),
                throws: self.extrude(&ft.throws, pos, lvl, memo),
            })),
            t => {
                let mut first = true;
                t.map(&mut |c| match std::mem::replace(&mut first, false) {
                    true => None,
                    false => Some(self.extrude(c, pos, lvl, memo)),
                })
            }
        }
    }

    /// `f` is called wherever `g` may be.
    fn fn_below(&mut self, f: &FnTy, g: &FnTy) -> Result<()> {
        let positional = |ft: &FnTy| -> Vec<Ty> {
            ft.args.iter().filter(|a| a.label.is_none()).map(|a| a.ty.clone()).collect()
        };
        let (fp, gp) = (positional(f), positional(g));
        let short = || -> Result<()> {
            bail!("type mismatch: a function of {} arguments where one of {} is wanted", fp.len(), gp.len())
        };
        for (i, ga) in gp.iter().enumerate() {
            match (fp.get(i), &f.vargs) {
                (Some(fa), _) => self.constrain(ga, fa)?,
                (None, Some(v)) => self.constrain(ga, v)?,
                (None, None) => short()?,
            }
        }
        if fp.len() > gp.len() {
            short()?
        }
        if let (Some(fv), Some(gv)) = (&f.vargs, &g.vargs) {
            self.constrain(gv, fv)?
        }
        for ga in g.args.iter().filter(|a| a.label.is_some()) {
            match f.args.iter().find(|fa| fa.label == ga.label) {
                Some(fa) if ga.optional && !fa.optional => {
                    bail!("type mismatch: #{} is required", ga.label.as_ref().unwrap())
                }
                Some(fa) => self.constrain(&ga.ty, &fa.ty)?,
                None => bail!("type mismatch: no argument #{}", ga.label.as_ref().unwrap()),
            }
        }
        for fa in f.args.iter().filter(|a| a.label.is_some() && !a.optional) {
            if !g.args.iter().any(|ga| ga.label == fa.label) {
                bail!("type mismatch: #{} is required", fa.label.as_ref().unwrap())
            }
        }
        self.constrain(&f.ret, &g.ret)?;
        self.constrain(&f.throws, &g.throws)
    }

    /// `a`, which is no variable and no union, fits a member of `b`.
    fn below_union(&mut self, a: &Ty, ms: &[Ty], b: &Ty) -> Result<()> {
        let primitive = |bit| ms.iter().any(|m| matches!(m, Ty::Prim(p) if p.contains(bit)));
        if ms.contains(a)
            || (matches!(a, Ty::Error(_)) && primitive(Typ::Error))
            || (matches!(a, Ty::Array(_)) && primitive(Typ::Array))
        {
            return Ok(());
        }
        let open: Vec<&Ty> = ms.iter().filter(|m| matches!(m, Ty::Var(_))).collect();
        if let Ty::Prim(p) = a {
            let rest = ms.iter().fold(*p, |rest, m| match m {
                Ty::Prim(q) => rest & !*q,
                _ => rest,
            });
            return match (rest.is_empty(), open.as_slice()) {
                (true, _) => Ok(()),
                (false, [m]) => self.constrain(&Ty::Prim(rest), m),
                (false, []) => mismatch(b, a),
                (false, _) => Ok(self.held.push((a.clone(), b.clone(), false))),
            };
        }
        let head = a.head();
        let cands: Vec<&Ty> =
            ms.iter().filter(|m| !matches!(m, Ty::Var(_)) && m.head() == head).collect();
        match (cands.as_slice(), open.as_slice()) {
            ([m], []) => self.constrain(a, m),
            ([], [m]) => self.constrain(a, m),
            ([], []) => mismatch(b, a),
            _ if self.resolving && self.is_ground(a) => {
                let (ground, loose): (Vec<&Ty>, Vec<&Ty>) =
                    cands.iter().partition(|m| self.is_ground(m));
                if ground.iter().any(|m| self.constrain(a, m).is_ok()) {
                    return Ok(());
                }
                match (loose.as_slice(), open.as_slice()) {
                    ([m], []) | ([], [m]) => self.constrain(a, m),
                    ([], []) => mismatch(b, a),
                    _ => Ok(self.held.push((a.clone(), b.clone(), false))),
                }
            }
            _ if !self.is_ground(a) || !self.is_ground(b) => {
                Ok(self.held.push((a.clone(), b.clone(), false)))
            }
            _ => match cands.iter().any(|m| self.constrain(a, m).is_ok()) {
                true => Ok(()),
                false => mismatch(b, a),
            },
        }
    }

    fn ambiguous(&self, a: &Ty, b: &Ty) -> Result<()> {
        bail!("{a} could go to more than one member of the union {b}, and nothing has decided which")
    }

    /// Apply a use to one concrete lower bound; false where the bound says
    /// nothing (bottom).
    fn apply(&mut self, w: &Watch, a: &Ty) -> Result<bool> {
        if let Watch::Select(id) = w {
            return self.select_route(*id, a).map(|()| true);
        }
        match (self.shallow(a), w) {
            (Ty::Bot, _) => Ok(false),
            (Ty::Union(ms), _) => {
                let mut any = false;
                for m in ms.iter() {
                    any |= self.apply(w, m)?
                }
                Ok(any)
            }
            (Ty::Named(id, args), _) => {
                let t = self.unfold(id, &args);
                self.apply(w, &t)
            }
            (Ty::Rigid(v), _) => {
                let mut any = false;
                for u in self.info(v).upper.clone() {
                    any |= self.apply(w, &u)?
                }
                match any {
                    true => Ok(true),
                    false => bail!("{} needs its type known here: annotate the binding", w.needs()),
                }
            }
            (Ty::Struct(fs), Watch::Field { field, out }) => {
                match fs.iter().find(|(n, _)| n == field) {
                    Some((_, t)) => self.constrain(t, out).map(|()| true),
                    None => bail!("{a} has no field {field}"),
                }
            }
            (Ty::Abs(tag, args), Watch::Index { i: 0, out }) => match self.rep(&tag, &args) {
                Some(rep) => self.constrain(&rep, out).map(|()| true),
                None => bail!("{tag} is abstract here: its payload cannot be read"),
            },
            (Ty::Error(t), Watch::Index { i: 0, out }) => self.constrain(&t, out).map(|()| true),
            (Ty::Tuple(ts), Watch::Index { i, out }) => match ts.get(*i) {
                Some(t) => self.constrain(t, out).map(|()| true),
                None => bail!("{a} has no element {i}"),
            },
            (Ty::Fn(ft), Watch::Call { args, out }) => {
                self.call(&ft, args, out).map(|()| true)
            }
            (a, Watch::Field { field, .. }) => bail!("{a} has no field {field}"),
            (a, Watch::Index { i, .. }) => bail!("{a} has no element {i}"),
            (a, Watch::Call { .. }) => bail!("{a} is not a function"),
            (_, Watch::Select(_)) => Ok(false),
        }
    }

    /// Register a use of whatever `t` turns out to be.
    pub fn watch(&mut self, t: &Ty, w: Watch) -> Result<()> {
        match self.shallow(t) {
            Ty::Var(v) => {
                let mut fired = false;
                for l in self.info(v).lower.clone() {
                    if !matches!(l, Ty::Var(_)) {
                        let src = self.info(v).joined.iter().find(|(_, j)| j == &l).map(|(s, _)| *s);
                        let via = self.via;
                        self.via = src.or(via);
                        fired |= self.apply(&w, &l)?;
                        self.via = via
                    }
                }
                self.info_mut(v).watches.push((w, fired));
                for u in self.info(v).upper.clone() {
                    if !matches!(u, Ty::Var(_)) {
                        self.demanded(v, &u)?
                    }
                }
                Ok(())
            }
            t => self.apply(&w, &t).map(|_| ()),
        }
    }

    /// A call of a function whose type is known.
    pub fn call(&mut self, ft: &FnTy, args: &[Arg], out: &Ty) -> Result<()> {
        let outer = self.at;
        let r = self.call_at(ft, args);
        self.at = outer;
        r?;
        self.constrain(&ft.ret, out)
    }

    fn call_at(&mut self, ft: &FnTy, args: &[Arg]) -> Result<()> {
        let mut formals = ft.args.iter().filter(|a| a.label.is_none());
        for (_, arg, at) in args.iter().filter(|(l, ..)| l.is_none()) {
            self.at = at.or(self.at);
            match (formals.next(), &ft.vargs) {
                (Some(f), _) => self.constrain(arg, &f.ty)?,
                (None, Some(v)) => self.constrain(arg, v)?,
                (None, None) => bail!("too many arguments"),
            }
        }
        if formals.next().is_some() {
            bail!("missing argument")
        }
        for (label, arg, at) in args.iter().filter(|(l, ..)| l.is_some()) {
            self.at = at.or(self.at);
            match ft.args.iter().find(|f| &f.label == label) {
                Some(f) => self.constrain(arg, &f.ty)?,
                None => bail!("no argument #{}", label.as_ref().unwrap()),
            }
        }
        for f in ft.args.iter().filter(|f| f.label.is_some() && !f.optional) {
            if !args.iter().any(|(l, ..)| l == &f.label) {
                bail!("missing required argument #{}", f.label.as_ref().unwrap())
            }
        }
        Ok(())
    }

    /// A copy of `t` with every variable deeper than `above` replaced by a
    /// fresh one at `at`, bounds included.
    pub fn instantiate(&mut self, t: &Ty, above: u32, at: u32) -> Ty {
        let mut memo = HashMap::new();
        let t = self.copy(t, above, at, &mut memo);
        for (a, b, scheme) in self.held.clone() {
            let mut mine = false;
            for t in [&a, &b] {
                t.each(&mut |t| {
                    if let Ty::Var(v) | Ty::Rigid(v) = t {
                        mine |= memo.contains_key(v)
                    }
                })
            }
            if scheme && mine {
                let (a, b) = (self.copy(&a, above, at, &mut memo), self.copy(&b, above, at, &mut memo));
                self.held.push((a, b, false))
            }
        }
        t
    }

    fn copy(&mut self, t: &Ty, above: u32, at: u32, memo: &mut HashMap<Var, Var>) -> Ty {
        self.deep(t).map(&mut |t| match t {
            Ty::Var(v) | Ty::Rigid(v) if self.info(*v).level > above => {
                if let Some(n) = memo.get(v) {
                    return Some(Ty::Var(*n));
                }
                let kind = match t {
                    Ty::Rigid(_) => Kind::Single,
                    _ => self.info(*v).kind,
                };
                let n = self.fresh(at, kind);
                self.info_mut(n).singleton = self.info(*v).singleton;
                self.info_mut(n).over_any = self.info(*v).over_any;
                memo.insert(*v, n);
                let (ls, us) = (self.info(*v).lower.clone(), self.info(*v).upper.clone());
                let ls = ls.iter().map(|l| self.copy(l, above, at, memo)).collect();
                let us = us.iter().map(|u| self.copy(u, above, at, memo)).collect();
                // what came out of one join stays one bound in the copy
                let joined = self.info(*v).joined.clone();
                let joined = joined
                    .iter()
                    .map(|(src, l)| (*memo.get(src).unwrap_or(src), self.copy(l, above, at, memo)))
                    .collect();
                let info = self.info_mut(n);
                (info.lower, info.upper, info.joined) = (ls, us, joined);
                for (w, _) in self.info(*v).watches.clone() {
                    if let Watch::Select(id) = w {
                        let arms = self.select_arms(id);
                        let arms = arms
                            .into_iter()
                            .map(|a| Arm {
                                pat: a.pat.map_vars(&mut |u| {
                                    match self.copy(&Ty::Var(u), above, at, memo) {
                                        Ty::Var(n) => n,
                                        _ => u,
                                    }
                                }),
                                ..a
                            })
                            .collect();
                        let id = self.select_instance(arms);
                        self.info_mut(n).watches.push((Watch::Select(id), false))
                    }
                }
                Some(Ty::Var(n))
            }
            _ => None,
        })
    }

    /// Every open variable reachable from `t` through bounds.
    fn reach(&self, t: &Ty, seen: &mut Vec<Var>) {
        for v in self.open_vars(t) {
            if !seen.contains(&v) {
                seen.push(v);
                let info = self.info(v);
                for b in info.lower.iter().chain(info.upper.iter()) {
                    self.reach(b, seen)
                }
                for u in info.feeders.iter() {
                    self.reach(&Ty::Var(*u), seen)
                }
                for (w, _) in info.watches.iter() {
                    if let Watch::Select(id) = w {
                        for u in self.select_vars(*id) {
                            self.reach(&Ty::Var(u), seen)
                        }
                    }
                }
            }
        }
    }

    /// Mark the variables of a declared scheme as its own.
    pub fn quantify(&mut self, t: &Ty, lvl: u32) {
        let mut kept = vec![];
        self.reach(t, &mut kept);
        for v in kept {
            if self.info(v).level > lvl {
                self.info_mut(v).quantified = true
            }
        }
    }

    /// Close the definition of a generic binding of type `t`, checked one
    /// level above `lvl`: what `t` reaches stays, as the scheme's variables;
    /// every other variable of the definition is fixed.
    /// `mark` is how many variables existed when the definition began: only
    /// later ones are its own.
    pub fn generalize(&mut self, t: &Ty, lvl: u32, mark: usize) -> Result<()> {
        let mut kept = vec![];
        self.reach(t, &mut kept);
        // a name bound over an `Any` part is decided here, by the
        // definition's own uses: its type is the arm's run-time test, which
        // no instance may change
        for v in kept.clone() {
            if self.info(v).level > lvl && self.info(v).over_any && self.info(v).fixed.is_none() {
                let t = self.demand(v);
                self.testable(&t)?;
                let info = self.info_mut(v);
                (info.fixed, info.demanded) = (Some(t.clone()), true);
                for u in self.info(v).upper.clone() {
                    if matches!(u, Ty::Var(_)) {
                        self.constrain(&t, &u)?
                    }
                }
            }
        }
        for v in kept {
            if self.info(v).level > lvl && self.info(v).fixed.is_none() {
                self.info_mut(v).quantified = true;
                if let Some((w, _)) = self.info(v).watches.iter().find(|(w, fired)| !fired && !matches!(w, Watch::Select(_))) {
                    bail!("{} needs its type known here: annotate the binding", w.needs())
                }
            }
        }
        self.fix_scope(lvl + 1, false, mark)
    }

    /// Fix every variable an input left open.
    pub fn finish(&mut self) -> Result<()> {
        self.fix_scope(0, true, 0)?;
        self.dead_arms()
    }

    /// Fix every variable in `t`, those nothing has decided included.
    pub(crate) fn fix_in(&mut self, t: &Ty) -> Result<()> {
        loop {
            let vs = self.open_vars(t);
            if vs.is_empty() {
                return Ok(());
            }
            vs.into_iter().try_for_each(|v| self.fix(v))?
        }
    }

    /// What `?` and `$` leave of `t`: `t` less its errors, or, where it
    /// holds none, less null.
    /// With it, the payloads of the errors taken off.
    pub fn strip(&self, t: &Ty) -> (Ty, Vec<Ty>) {
        let members = match t {
            Ty::Union(ms) => ms.to_vec(),
            t => vec![t.clone()],
        };
        let errors = members
            .iter()
            .any(|m| matches!(m, Ty::Error(_)) || matches!(m, Ty::Prim(p) if p.contains(Typ::Error)));
        let gone = if errors { Typ::Error } else { Typ::Null };
        let payloads = members
            .iter()
            .filter_map(|m| match m {
                Ty::Error(p) => Some((**p).clone()),
                _ => None,
            })
            .collect();
        let left = union(members.into_iter().filter_map(|m| match m {
            Ty::Error(_) if errors => None,
            Ty::Prim(p) => Some(Ty::Prim(p & !gone)).filter(|_| !(p & !gone).is_empty()),
            m => Some(m),
        }));
        (left, payloads)
    }

    fn fixable(&self, v: Var, min: u32) -> bool {
        let mut all = vec![];
        self.reach(&Ty::Var(v), &mut all);
        all.iter().all(|v| self.info(*v).level >= min && !self.info(*v).quantified)
    }

    fn fix_scope(&mut self, min: u32, last: bool, mark: usize) -> Result<()> {
        let mut progress = true;
        while progress {
            progress = false;
            for i in (0..self.held.len()).rev() {
                let (a, b, scheme) = self.held[i].clone();
                if scheme {
                    continue;
                }
                let (avs, bvs) = (self.open_vars(&a), self.open_vars(&b));
                let mut vs = avs.iter().chain(bvs.iter());
                if vs.clone().all(|v| self.fixable(*v, min)) {
                    // a member something has decided is decided here; one
                    // nothing has stays open, to take what the others do not
                    self.held.swap_remove(i);
                    avs.iter().try_for_each(|v| self.fix(*v))?;
                    for v in bvs.iter() {
                        if self.decided(*v, &mut vec![]) {
                            self.fix(*v)?
                        }
                    }
                    let (a, b) = (self.deep(&a), self.deep(&b));
                    let n = self.held.len();
                    self.resolving = true;
                    let r = self.constrain(&a, &b);
                    self.resolving = false;
                    r?;
                    progress |= self.held.len() == n
                } else if vs.any(|v| self.info(*v).quantified) {
                    self.held[i].2 = true
                }
            }
            let fixable = |s: &Self, t: &Ty| s.open_vars(t).iter().all(|v| s.fixable(*v, min));
            progress |= self.select_waiting(&fixable)?;
            for i in (0..self.strips.len()).rev() {
                let (arg, out, raised) = self.strips[i].clone();
                if fixable(self, &arg) {
                    self.strips.swap_remove(i);
                    self.fix_in(&arg)?;
                    let (left, payloads) = self.strip(&self.deep(&arg));
                    self.constrain(&left, &out)?;
                    for p in raised.iter().flat_map(|r| payloads.iter().map(move |p| (p, r))) {
                        self.constrain(p.0, p.1)?
                    }
                    progress = true
                } else if self.open_vars(&arg).iter().any(|v| self.info(*v).quantified) {
                    bail!("the type of the operand of ? or $ is not known here: annotate it")
                }
            }
        }
        // a variable something flows into first: one nothing flows into may
        // be decided by a variable that holds it
        let order = (mark as u32..self.vars.len() as u32).map(Var);
        let (decided, open): (Vec<Var>, Vec<Var>) =
            order.partition(|v| self.decided(*v, &mut vec![]));
        for v in decided.into_iter().chain(open) {
            if self.info(v).fixed.is_none()
                && self.info(v).level >= min
                && !self.info(v).quantified
                && (last || self.fixable(v, min))
            {
                self.fix(v)?;
                if let Some((w, _)) = self.info(v).watches.iter().find(|(w, fired)| !fired && !matches!(w, Watch::Select(_)))
                    && self.info(v).fixed != Some(Ty::Bot)
                {
                    bail!("{} needs its type known here: annotate the binding", w.needs())
                }
            }
        }
        if last && let Some((a, b, _)) = self.held.iter().find(|h| !h.2) {
            return self.ambiguous(a, b);
        }
        Ok(())
    }

    /// Give `v` one type: the union of what flows into it, or, where nothing
    /// does, the meet of what its uses demand.
    fn fix(&mut self, v: Var) -> Result<()> {
        if self.info(v).fixed.is_some() || self.info(v).quantified || !self.fixing.insert(v) {
            return Ok(());
        }
        let mut cands: Vec<Ty> = vec![];
        let mut from: Vec<Option<Site>> = vec![];
        let mut groups: Vec<(Var, Vec<Ty>)> = vec![];
        let feeders = self.info(v).feeders.iter().filter(|u| !self.info(**u).quantified);
        let feeders: Vec<Ty> = feeders.map(|u| Ty::Var(*u)).collect();
        for l in self.info(v).lower.clone().into_iter().chain(feeders) {
            match self.shallow(&l) {
                _ if matches!(&l, Ty::Var(u) if self.info(*u).demanded && !self.info(*u).over_any) => (),
                // a variable nothing flows into says nothing as a lower
                // bound; it is fixed from above, after this one. A name
                // bound over `Any` does hold a value: it says what it is
                // fixed to
                Ty::Var(u) if !self.decided(u, &mut vec![]) && !self.info(u).over_any => (),
                Ty::Var(u) => {
                    self.fix(u)?;
                    if let Some(t) = self.info(u).fixed.clone()
                        && (!self.info(u).demanded || self.info(u).over_any)
                    {
                        cands.push(t);
                        from.push(None)
                    }
                }
                t => {
                    for u in self.open_vars(&t) {
                        if self.fixing.contains(&u) {
                            bail!("infinite type: a variable holds itself in {t}")
                        }
                        if self.info(v).kind == Kind::Join || self.decided(u, &mut vec![]) {
                            self.fix(u)?
                        }
                    }
                    let src = self.info(v).joined.iter().find(|(_, j)| j == &l).map(|(s, _)| *s);
                    match (src, groups.iter_mut().find(|(s, _)| Some(*s) == src)) {
                        (None, _) => {
                            cands.push(self.deep(&t));
                            from.push(self.sites.get(&(v, true, l.clone())).copied())
                        }
                        (Some(_), Some((_, g))) => g.push(self.deep(&t)),
                        (Some(s), None) => groups.push((s, vec![self.deep(&t)])),
                    }
                }
            }
        }
        cands.extend(groups.into_iter().map(|(_, g)| union(g)));
        from.resize(cands.len(), None);
        let live: Vec<bool> = cands.iter().map(|t| !matches!(t, Ty::Bot)).collect();
        let mut keep = live.iter();
        cands.retain(|_| *keep.next().unwrap());
        let mut keep = live.iter();
        from.retain(|_| *keep.next().unwrap());
        let ty = if !cands.is_empty() {
            match self.info(v).kind {
                Kind::Join => union(cands),
                // a variable nothing has decided may stay open inside the
                // type: a later holder that shares it can still decide it
                Kind::Single => self.holder(&cands, &from).map_err(|e| {
                    match std::env::var("SPIKE_DEBUG") {
                        Ok(_) => e.context(format!(
                            "debug: '_{} level {} lower {:?} feeders {:?} joined {:?}",
                            v.0,
                            self.info(v).level,
                            self.info(v).lower.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
                            self.info(v).feeders,
                            self.info(v).joined.iter().map(|(s, t)| (s.0, t.to_string())).collect::<Vec<_>>()
                        )),
                        Err(_) => e,
                    }
                })?,
            }
        } else if self.info(v).singleton {
            Ty::Bot
        } else {
            let mut uppers = vec![];
            for u in self.info(v).upper.clone() {
                for w in self.open_vars(&u) {
                    self.fix(w)?
                }
                let u = self.deep(&u);
                if !matches!(u, Ty::Var(_)) {
                    uppers.push(u)
                }
            }
            self.info_mut(v).demanded = true;
            let t = match uppers.is_empty() && self.info(v).over_any {
                true => Ty::Any,
                false => self.meet(&uppers)?,
            };
            if self.info(v).over_any {
                self.testable(&t)?
            }
            t
        };
        if self.info(v).singleton
            && !matches!(&ty, Ty::Bot)
            && !matches!(&ty, Ty::Prim(p) if p.exactly_one().is_some())
        {
            bail!("cannot compute with {ty}: both operands must be one numeric type")
        }
        self.info_mut(v).fixed = Some(ty);
        self.fixing.remove(&v);
        Ok(())
    }

    /// The type of a name bound over an `Any` part is the arm's run-time
    /// test: it must be something a test can tell.
    fn testable(&self, t: &Ty) -> Result<()> {
        let mut opaque = false;
        t.each(&mut |t| opaque |= matches!(t, Ty::Fn(_) | Ty::Ref(..)));
        if matches!(t, Ty::Bot) || opaque {
            bail!("a name bound inside an Any part: its uses do not decide a type a test can tell ({t})")
        }
        Ok(())
    }

    /// The one of `ts` that holds the others.
    fn holder(&mut self, ts: &[Ty], from: &[Option<Site>]) -> Result<Ty> {
        // a lower bound with a variable nothing has decided is tried last:
        // that variable takes what the other lower bounds need
        let (ground, open): (Vec<&Ty>, Vec<&Ty>) = ts.iter().partition(|t| self.is_ground(t));
        for t in ground.into_iter().chain(open) {
            if ts.iter().all(|o| self.constrain(o, t).is_ok()) {
                return Ok(t.clone());
            }
        }
        let t = &ts[0];
        let i = ts.iter().position(|o| self.constrain(o, t).is_err()).unwrap_or(0);
        mismatch::<Ty>(t, &ts[i]).map_err(|e| e.context(Two(from[0], from[i])))
    }

    /// The one of `ts` every other holds; nothing demanded is bottom.
    fn meet(&mut self, ts: &[Ty]) -> Result<Ty> {
        if ts.is_empty() {
            return Ok(Ty::Bot);
        }
        for t in ts {
            if ts.iter().all(|o| self.constrain(t, o).is_ok()) {
                return Ok(t.clone());
            }
        }
        let prims = ts.iter().try_fold(None, |acc: Option<_>, t| match (acc, t) {
            (None, Ty::Prim(p)) => Some(Some(*p)),
            (Some(a), Ty::Prim(p)) => Some(Some(a & *p)),
            _ => None,
        });
        match prims {
            Some(Some(p)) if !p.is_empty() => Ok(Ty::Prim(p)),
            _ => bail!(
                "unsatisfiable constraints: {}",
                ts.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" & ")
            ),
        }
    }
}
