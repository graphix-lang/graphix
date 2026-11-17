use crate::ty::{FnArg, FnTy, Ty, Var, union};
use anyhow::{Result, bail};
use arcstr::ArcStr;
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
#[derive(Clone, Debug)]
pub enum Watch {
    Field { field: ArcStr, out: Ty },
    Index { i: usize, out: Ty },
    Call { args: Vec<(Option<ArcStr>, Ty)>, out: Ty },
}

impl Watch {
    fn needs(&self) -> &'static str {
        match self {
            Watch::Field { .. } => "reading a struct",
            Watch::Index { .. } => "reading a tuple",
            Watch::Call { .. } => "reading a fn",
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
    watches: Vec<(Watch, bool)>,
}

pub struct Def {
    pub params: Vec<Var>,
    pub body: Ty,
}

#[derive(Default)]
pub struct Solver {
    pub vars: Vec<VarInfo>,
    pub defs: Vec<Def>,
    cache: HashSet<(Ty, Ty)>,
    /// A type below a union with more than one member that could take it,
    /// waiting for both to be ground; true where a scheme's variables hold
    /// it, so each instance takes a copy.
    held: Vec<(Ty, Ty, bool)>,
    fixing: HashSet<Var>,
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
            watches: vec![],
        });
        Var(self.vars.len() as u32 - 1)
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

    fn is_ground(&self, t: &Ty) -> bool {
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

    fn unfold(&self, id: u32, args: &[Ty]) -> Ty {
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
                        match &b {
                            Ty::Var(w) if self.info(*w).level == lvl => {
                                self.info_mut(*w).lower.push(a.clone())
                            }
                            Ty::Var(_) => (),
                            b => self.demanded(*v, b)?,
                        }
                        for l in self.info(*v).lower.clone() {
                            self.constrain(&l, &b)?
                        }
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
    fn arrive(&mut self, w: Var, a: &Ty) -> Result<()> {
        self.info_mut(w).lower.push(a.clone());
        if !matches!(a, Ty::Var(_)) {
            for i in 0..self.info(w).watches.len() {
                let watch = self.info(w).watches[i].0.clone();
                if self.apply(&watch, a)? {
                    self.info_mut(w).watches[i].1 = true
                }
            }
        }
        for u in self.info(w).upper.clone() {
            self.constrain(a, &u)?
        }
        Ok(())
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
        if ms.contains(a) {
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
        }
    }

    /// Register a use of whatever `t` turns out to be.
    pub fn watch(&mut self, t: &Ty, w: Watch) -> Result<()> {
        match self.shallow(t) {
            Ty::Var(v) => {
                let mut fired = false;
                for l in self.info(v).lower.clone() {
                    if !matches!(l, Ty::Var(_)) {
                        fired |= self.apply(&w, &l)?
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
    pub fn call(&mut self, ft: &FnTy, args: &[(Option<ArcStr>, Ty)], out: &Ty) -> Result<()> {
        let mut formals = ft.args.iter().filter(|a| a.label.is_none());
        for (_, arg) in args.iter().filter(|(l, _)| l.is_none()) {
            match (formals.next(), &ft.vargs) {
                (Some(f), _) => self.constrain(arg, &f.ty)?,
                (None, Some(v)) => self.constrain(arg, v)?,
                (None, None) => bail!("too many arguments"),
            }
        }
        if formals.next().is_some() {
            bail!("missing argument")
        }
        for (label, arg) in args.iter().filter(|(l, _)| l.is_some()) {
            match ft.args.iter().find(|f| &f.label == label) {
                Some(f) => self.constrain(arg, &f.ty)?,
                None => bail!("no argument #{}", label.as_ref().unwrap()),
            }
        }
        for f in ft.args.iter().filter(|f| f.label.is_some() && !f.optional) {
            if !args.iter().any(|(l, _)| l == &f.label) {
                bail!("missing required argument #{}", f.label.as_ref().unwrap())
            }
        }
        self.constrain(&ft.ret, out)
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
                memo.insert(*v, n);
                let (ls, us) = (self.info(*v).lower.clone(), self.info(*v).upper.clone());
                let ls = ls.iter().map(|l| self.copy(l, above, at, memo)).collect();
                let us = us.iter().map(|u| self.copy(u, above, at, memo)).collect();
                let info = self.info_mut(n);
                (info.lower, info.upper) = (ls, us);
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
            }
        }
    }

    /// Close the definition of a generic binding of type `t`, checked one
    /// level above `lvl`: what `t` reaches stays, as the scheme's variables;
    /// every other variable of the definition is fixed.
    pub fn generalize(&mut self, t: &Ty, lvl: u32) -> Result<()> {
        let mut kept = vec![];
        self.reach(t, &mut kept);
        for v in kept {
            if self.info(v).level > lvl {
                self.info_mut(v).quantified = true;
                if let Some((w, _)) = self.info(v).watches.iter().find(|(_, fired)| !fired) {
                    bail!("{} needs its type known here: annotate the binding", w.needs())
                }
            }
        }
        self.fix_scope(lvl + 1, false)
    }

    /// Fix every variable an input left open.
    pub fn finish(&mut self) -> Result<()> {
        self.fix_scope(0, true)
    }

    fn fixable(&self, v: Var, min: u32) -> bool {
        let mut all = vec![];
        self.reach(&Ty::Var(v), &mut all);
        all.iter().all(|v| self.info(*v).level >= min && !self.info(*v).quantified)
    }

    fn fix_scope(&mut self, min: u32, last: bool) -> Result<()> {
        let mut progress = true;
        while progress {
            progress = false;
            for i in (0..self.held.len()).rev() {
                let (a, b, scheme) = self.held[i].clone();
                if scheme {
                    continue;
                }
                let mut vs = self.open_vars(&a);
                vs.extend(self.open_vars(&b));
                if vs.iter().all(|v| self.fixable(*v, min)) {
                    self.held.swap_remove(i);
                    vs.iter().try_for_each(|v| self.fix(*v))?;
                    let (a, b) = (self.deep(&a), self.deep(&b));
                    if self.constrain(&a, &b).is_err() {
                        return self.ambiguous(&a, &b);
                    }
                    progress = true
                } else if vs.iter().any(|v| self.info(*v).quantified) {
                    self.held[i].2 = true
                }
            }
        }
        for i in 0..self.vars.len() as u32 {
            let v = Var(i);
            if self.info(v).fixed.is_none()
                && self.info(v).level >= min
                && !self.info(v).quantified
                && self.fixable(v, min)
            {
                self.fix(v)?;
                if let Some((w, _)) = self.info(v).watches.iter().find(|(_, fired)| !fired)
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
        if self.info(v).fixed.is_some() || !self.fixing.insert(v) {
            return Ok(());
        }
        let mut cands: Vec<Ty> = vec![];
        for l in self.info(v).lower.clone() {
            match self.shallow(&l) {
                _ if matches!(&l, Ty::Var(u) if self.info(*u).demanded) => (),
                Ty::Var(u) => {
                    self.fix(u)?;
                    if let Some(t) = self.info(u).fixed.clone()
                        && !self.info(u).demanded
                    {
                        cands.push(t)
                    }
                }
                t => {
                    for u in self.open_vars(&t) {
                        if self.fixing.contains(&u) {
                            bail!("infinite type: a variable holds itself in {t}")
                        }
                        self.fix(u)?
                    }
                    cands.push(self.deep(&t))
                }
            }
        }
        cands.retain(|t| !matches!(t, Ty::Bot));
        let ty = if !cands.is_empty() {
            match self.info(v).kind {
                Kind::Join => union(cands),
                Kind::Single => self.holder(&cands)?,
            }
        } else if self.info(v).singleton {
            Ty::Bot
        } else {
            let mut uppers = vec![];
            for u in self.info(v).upper.clone() {
                if !matches!(self.shallow(&u), Ty::Var(_)) {
                    for w in self.open_vars(&u) {
                        self.fix(w)?
                    }
                    uppers.push(self.deep(&u))
                }
            }
            self.info_mut(v).demanded = true;
            self.meet(&uppers)?
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

    /// The one of `ts` that holds the others.
    fn holder(&mut self, ts: &[Ty]) -> Result<Ty> {
        for t in ts {
            if ts.iter().all(|o| self.constrain(o, t).is_ok()) {
                return Ok(t.clone());
            }
        }
        let t = &ts[0];
        let o = ts.iter().find(|o| self.constrain(o, t).is_err()).unwrap_or(t);
        mismatch(t, o)
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
