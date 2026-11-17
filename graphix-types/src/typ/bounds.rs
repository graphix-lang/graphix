//! Inference by bounds. While a definition is checked no variable is
//! bound: what flows into a variable is a lower bound of its cell, what a
//! use demands of it an upper bound (a constraint), and each new bound is
//! met with the bounds already on the other side, so the bounds are always
//! consistent with each other.

use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        FnArgKind, FnArgType, FnType, Mutability, TVar, Type, node_addr,
        setops::union_identical,
        tvar::{AtLevel, Level},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::ops::ControlFlow;
use triomphe::Arc;

/// What the solver keeps of an open cell beside its bounds.
#[derive(Debug, Default)]
pub struct Known {
    /// Its lower bounds may form a union nobody wrote: an array literal's
    /// element, a select's result. Any other cell is of one type, so one
    /// of the bounds in `direct` must hold the others.
    pub(crate) join: bool,
    /// What reached the cell other than along an edge from another cell:
    /// an argument, an initial value, a write, a part of something taken
    /// apart, and each cell that feeds it (but not what that cell passes
    /// on).
    pub(crate) direct: SmallVec<[Type; 1]>,
    /// The cells at this one's depth that flow into it. What flows into
    /// them is passed on as it arrives, so this is a record for fixing.
    pub(crate) feeders: SmallVec<[TVar; 1]>,
    /// For a declared variable: the cells outside its definition that its
    /// values flow into. Each instance's type for it flows there.
    pub(crate) sinks: SmallVec<[TVar; 1]>,
}

/// The relation's own state for one check.
#[derive(Default)]
pub struct Solving {
    /// The pairs holding a typedef's name that are being related: a pair
    /// met again inside itself holds.
    unfolding: Vec<(Type, Type)>,
    /// What each cell has had added since the outermost attempt began, in
    /// order, each the last of its list when added: a failed attempt takes
    /// its own back.
    added: Vec<(TVar, Added)>,
    /// How many attempts are in progress.
    attempts: u32,
    /// A type below a union more than one of whose members could take it,
    /// waiting until both are decided.
    held: Vec<(Type, Type)>,
    /// A held choice is being made: a decided member that does not fit is
    /// out.
    resolving: bool,
    /// The cells made to carry what one cell passes to a union's open
    /// member, by address.
    passing: AHashMap<usize, TVar>,
}

#[derive(Clone, Copy)]
enum Added {
    Lower,
    Upper,
    Direct,
    Feeder,
    Sink,
}

/// `t` past the cells that are bound: an open cell, or a type that is no
/// cell.
fn shallow(t: &Type) -> Type {
    match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => ensure_sufficient(|| shallow(&b)),
            None => t.clone(),
        },
        t => t.clone(),
    }
}

/// The cell `t` is, where it is one the solver may bound: open, and no
/// declared variable inside its own definition.
fn open(t: &Type) -> Option<&TVar> {
    match t {
        Type::TVar(tv) if !tv.is_bound() && !tv.is_rigid() => Some(tv),
        _ => None,
    }
}

/// How deep the deepest cell of `t` is, open or declared, not looking
/// through bounds.
fn depth(t: &Type) -> u32 {
    fn go(t: &Type, deepest: &mut u32, seen: &mut AHashSet<usize>) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => go(&b, deepest, seen),
                None => *deepest = (*deepest).max(tv.level().depth()),
            },
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            t => {
                let _ = t.try_for_each_child::<()>(&mut |c| {
                    go(c, deepest, seen);
                    ControlFlow::Continue(())
                });
            }
        })
    }
    let mut deepest = 0;
    go(t, &mut deepest, &mut LPooled::take());
    deepest
}

/// What tells two members of a union apart without looking inside them.
fn alike(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Primitive(p), Type::Primitive(q)) => p.intersects(*q),
        (Type::Array(_), Type::Array(_))
        | (Type::List(_), Type::List(_))
        | (Type::Error(_), Type::Error(_))
        | (Type::Map(_), Type::Map(_))
        | (Type::Fn(_), Type::Fn(_)) => true,
        (Type::ByRef(m, _), Type::ByRef(n, _)) => m == n,
        (Type::Tuple(xs), Type::Tuple(ys)) => xs.len() == ys.len(),
        (Type::Variant(t, xs, _), Type::Variant(u, ys, _)) => {
            t == u && xs.len() == ys.len()
        }
        (Type::Struct(xs), Type::Struct(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| x.0 == y.0)
        }
        (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. }) => x == y,
        _ => false,
    }
}

fn mismatch<T>(wanted: &Type, got: &Type) -> Result<T> {
    bail!("type mismatch {wanted} does not contain {got}")
}

/// Add `t` to `bounds` unless it is there; false if it was.
fn record(bounds: &mut SmallVec<[Type; 1]>, t: &Type) -> bool {
    match bounds.iter().any(|b| union_identical(b, t)) {
        true => false,
        false => {
            bounds.push(t.clone());
            true
        }
    }
}

/// `f` over what the solver keeps of the cell of `v`.
fn known<R>(v: &TVar, f: impl FnOnce(&mut Known) -> R) -> R {
    let cell = v.cell();
    let mut c = cell.write();
    f(c.solving.get_or_insert_default())
}

impl TVar {
    /// Whether the cell's lower bounds may form a union nobody wrote.
    pub fn joins(&self) -> bool {
        self.cell().read().solving.as_ref().is_some_and(|k| k.join)
    }

    /// Let the cell's lower bounds form a union: it is where values of
    /// different types meet by the program's own structure.
    pub fn set_joins(&self) {
        known(self, |k| k.join = true)
    }

    /// What reached the cell other than along an edge from another cell.
    pub fn cell_direct(&self) -> SmallVec<[Type; 1]> {
        let cell = self.cell();
        let c = cell.read();
        c.solving.as_ref().map(|k| k.direct.clone()).unwrap_or_default()
    }
}

impl Solving {
    /// `a` fits where `b` is wanted. An open cell on either side takes the
    /// other as a bound, and the bound meets every bound the cell has on
    /// its other side.
    pub fn constrain(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        self.relate(env, a, b, false)
    }

    /// [`Self::constrain`] as a question: where `a` does not fit, every
    /// bound the try added is taken back and nothing is changed.
    pub fn attempt(&mut self, env: &Env, a: &Type, b: &Type) -> bool {
        let (mark, held) = (self.added.len(), self.held.len());
        self.attempts += 1;
        let fits = self.constrain(env, a, b).is_ok();
        self.attempts -= 1;
        if !fits {
            self.held.truncate(held);
            for (v, what) in self.added.drain(mark..).rev() {
                let cell = v.cell();
                let mut c = cell.write();
                match (what, c.solving.as_mut()) {
                    (Added::Lower, _) => drop(c.lower.pop()),
                    (Added::Upper, _) => drop(c.constraints.pop()),
                    (Added::Direct, Some(k)) => drop(k.direct.pop()),
                    (Added::Feeder, Some(k)) => drop(k.feeders.pop()),
                    (Added::Sink, Some(k)) => drop(k.sinks.pop()),
                    (_, None) => (),
                }
            }
        }
        if self.attempts == 0 {
            self.added.clear()
        }
        fits
    }

    fn note(&mut self, v: &TVar, what: Added) {
        if self.attempts > 0 {
            self.added.push((v.clone(), what))
        }
    }

    /// `edge`: the relation passes a cell's bound on to a cell it feeds,
    /// so what arrives is the feeder's and not new to the cell fed.
    fn relate(&mut self, env: &Env, a: &Type, b: &Type, edge: bool) -> Result<()> {
        ensure_sufficient(|| self.relate_in(env, a, b, edge))
    }

    fn relate_in(&mut self, env: &Env, a: &Type, b: &Type, edge: bool) -> Result<()> {
        let (a, b) = (shallow(a), shallow(b));
        match (&a, &b) {
            (Type::Bottom, _) | (_, Type::Any) => return Ok(()),
            // a predicate on a cell is judged when the cell is decided
            (_, Type::Bound(_)) => return Ok(()),
            (Type::TVar(v), Type::TVar(w)) if v.same_cell(w) => return Ok(()),
            // with no open cell on either side this is the relation over
            // decided types
            _ if !a.has_free() && !b.has_free() => {
                return match b.contains(env, &a)? {
                    true => Ok(()),
                    false => mismatch(&b, &a),
                };
            }
            _ => (),
        }
        if open(&a).is_some() || open(&b).is_some() {
            return self.cells(env, &a, &b, edge);
        }
        match (&a, &b) {
            // what no decided member of `b` takes goes to its open member
            // as one type
            (Type::Set(xs), Type::Set(ys))
                if ys.iter().filter(|m| open(&shallow(m)).is_some()).count() == 1 =>
            {
                let ys: SmallVec<[Type; 4]> = ys.iter().map(shallow).collect();
                let decided =
                    |x: &Type| ys.iter().any(|m| open(m).is_none() && alike(m, x));
                let mut rest: SmallVec<[Type; 4]> = SmallVec::new();
                for x in xs.iter().map(shallow) {
                    match &x {
                        Type::Primitive(p) => {
                            let left = ys.iter().fold(*p, |left, m| match m {
                                Type::Primitive(q) => left & !*q,
                                _ => left,
                            });
                            if !left.is_empty() {
                                rest.push(Type::Primitive(left))
                            }
                        }
                        x if ys.iter().any(|m| union_identical(m, x)) => (),
                        x if decided(x) => self.constrain(env, x, &b)?,
                        x => rest.push(x.clone()),
                    }
                }
                let to = ys.iter().find(|m| open(m).is_some()).unwrap();
                match &rest[..] {
                    [] => Ok(()),
                    [one] => self.constrain(env, one, to),
                    many => {
                        let all: SmallVec<[&Type; 4]> = many.iter().collect();
                        self.constrain(env, &Type::union_exact(env, &all)?, to)
                    }
                }
            }
            (Type::Set(ms), _) => ms.iter().try_for_each(|m| self.constrain(env, m, &b)),
            (Type::Ref(_), _) | (_, Type::Ref(_)) => self.unfolded(env, &a, &b),
            // a declared variable is whatever its bounds say, and one
            // suffices
            (Type::TVar(v), _) => {
                let bounds = v.cell_constraints();
                match (bounds.iter().any(|u| self.attempt(env, u, &b)), &b) {
                    (true, _) => Ok(()),
                    (false, Type::Set(ms)) => self.below_union(env, &a, ms, &b),
                    (false, _) => mismatch(&b, &a),
                }
            }
            (_, Type::Set(ms)) => self.below_union(env, &a, ms, &b),
            (Type::Array(x), Type::Array(y))
            | (Type::List(x), Type::List(y))
            | (Type::Error(x), Type::Error(y))
            | (Type::ByRef(_, x), Type::ByRef(Mutability::Shared, y)) => {
                self.constrain(env, x, y)
            }
            (Type::ByRef(Mutability::Mut, x), Type::ByRef(Mutability::Mut, y)) => {
                self.equal(env, x, y)
            }
            (Type::Map(m), Type::Map(n)) => {
                self.constrain(env, &m.key, &n.key)?;
                self.constrain(env, &m.value, &n.value)
            }
            (Type::Tuple(xs), Type::Tuple(ys)) if alike(&a, &b) => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(env, x, y))
            }
            (Type::Variant(_, xs, _), Type::Variant(_, ys, _)) if alike(&a, &b) => {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.constrain(env, x, y))
            }
            (Type::Struct(xs), Type::Struct(ys)) if alike(&a, &b) => xs
                .iter()
                .zip(ys.iter())
                .try_for_each(|(x, y)| self.constrain(env, &x.1, &y.1)),
            (Type::Abstract { params: xs, .. }, Type::Abstract { params: ys, .. })
                if alike(&a, &b) && xs.len() == ys.len() =>
            {
                xs.iter().zip(ys.iter()).try_for_each(|(x, y)| self.equal(env, x, y))
            }
            (Type::Fn(f), Type::Fn(g)) => self.fn_below(env, f, g),
            _ => mismatch(&b, &a),
        }
    }

    /// Each fits where the other is wanted.
    fn equal(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        self.constrain(env, a, b)?;
        self.constrain(env, b, a)
    }

    /// `a` or `b` is an open cell. A bound is kept by the deeper of the two
    /// sides, so nothing a cell holds is deeper than the cell: where the
    /// other side is deeper, it is first copied out to the cell's depth.
    fn cells(&mut self, env: &Env, a: &Type, b: &Type, edge: bool) -> Result<()> {
        let (va, vb) = (open(a), open(b));
        let at = va.or(vb).unwrap().level();
        match (va, vb) {
            // what one cell passes to a union's open member reaches that
            // member as one feeder, as it would had the two met directly
            (Some(_), _) if let Some((to, rest)) = self.passes(b) => {
                let through = {
                    let _at = AtLevel::enter(to.level());
                    TVar::empty_named(to.name.clone())
                };
                through.set_joins();
                self.passing.insert(through.cell_addr(), through.clone());
                let through = Type::TVar(through);
                self.constrain(env, &through, &Type::TVar(to))?;
                let members: SmallVec<[&Type; 4]> =
                    rest.iter().chain([&through]).collect();
                self.constrain(env, a, &Type::union_exact(env, &members)?)
            }
            (Some(v), _) if depth(b) <= at.depth() => {
                let cell = v.cell();
                let lower = {
                    let mut c = cell.write();
                    match record(&mut c.constraints, b) {
                        true => c.lower.clone(),
                        false => return Ok(()),
                    }
                };
                self.note(v, Added::Upper);
                if let Some(w) = vb {
                    if w.level().depth() == at.depth() {
                        known(w, |k| k.feeders.push(v.clone()));
                        self.note(w, Added::Feeder)
                    }
                    if !edge {
                        known(w, |k| k.direct.push(a.clone()));
                        self.note(w, Added::Direct)
                    }
                }
                lower.iter().try_for_each(|l| self.relate(env, l, b, vb.is_some()))
            }
            (Some(_), Some(w)) => self.arrive(env, w, a, edge),
            (Some(_), None) => {
                let b = self.extrude(b, false, at, &mut LPooled::take());
                self.escapes(&b, at)?;
                self.constrain(env, a, &b)
            }
            (None, Some(w)) if depth(a) <= at.depth() => self.arrive(env, w, a, edge),
            (None, Some(_)) => {
                let a = self.extrude(a, true, at, &mut LPooled::take());
                self.escapes(&a, at)?;
                self.constrain(env, &a, b)
            }
            (None, None) => unreachable!(),
        }
    }

    /// The open member of the union `b` and its other members, where `b`
    /// is a union with one open member that carries nothing yet.
    fn passes(&self, b: &Type) -> Option<(TVar, SmallVec<[Type; 4]>)> {
        let Type::Set(ms) = b else { return None };
        let ms: SmallVec<[Type; 4]> = ms.iter().map(shallow).collect();
        let mut cells = ms.iter().filter_map(open);
        let (to, None) = (cells.next()?.clone(), cells.next()) else { return None };
        if self.passing.contains_key(&to.cell_addr()) {
            return None;
        }
        let rest = ms.iter().filter(|m| open(m).is_none()).cloned().collect();
        Some((to, rest))
    }

    /// `a` is a new lower bound of the open cell `w`: it must fit whatever
    /// is demanded of `w`.
    fn arrive(&mut self, env: &Env, w: &TVar, a: &Type, edge: bool) -> Result<()> {
        let cell = w.cell();
        let upper = {
            let mut c = cell.write();
            match record(&mut c.lower, a) {
                true => c.constraints.clone(),
                false => return Ok(()),
            }
        };
        self.note(w, Added::Lower);
        if !edge {
            known(w, |k| k.direct.push(a.clone()));
            self.note(w, Added::Direct)
        }
        upper.iter().try_for_each(|u| self.relate(env, a, u, open(&shallow(u)).is_some()))
    }

    /// A copy of `t` no deeper than `at`: each deeper cell gets a twin at
    /// `at` per polarity, joined to it by a bound. A declared variable has
    /// a twin only where its values flow out.
    fn extrude(
        &mut self,
        t: &Type,
        pos: bool,
        at: Level,
        twins: &mut AHashMap<(usize, bool), TVar>,
    ) -> Type {
        if depth(t) <= at.depth() {
            return t.clone();
        }
        let twin = |of: &TVar| {
            let _at = AtLevel::enter(at);
            TVar::empty_named(of.name.clone())
        };
        let t = shallow(t);
        match &t {
            Type::TVar(v) => {
                if let Some(n) = twins.get(&(v.cell_addr(), pos)) {
                    return Type::TVar(n.clone());
                }
                match (v.is_rigid(), pos) {
                    (true, false) => t.clone(),
                    (true, true) => {
                        let n = twin(v);
                        n.set_joins();
                        twins.insert((v.cell_addr(), pos), n.clone());
                        known(v, |k| k.sinks.push(n.clone()));
                        self.note(v, Added::Sink);
                        Type::TVar(n)
                    }
                    (false, _) => {
                        let n = twin(v);
                        if v.joins() {
                            n.set_joins()
                        }
                        twins.insert((v.cell_addr(), pos), n.clone());
                        let (lower, upper) = (v.cell_lower(), v.cell_constraints());
                        let cell = v.cell();
                        match pos {
                            true => {
                                cell.write().constraints.push(Type::TVar(n.clone()));
                                self.note(v, Added::Upper);
                                let lower: SmallVec<[Type; 1]> = lower
                                    .iter()
                                    .map(|l| self.extrude(l, pos, at, twins))
                                    .collect();
                                n.cell().write().lower = lower
                            }
                            false => {
                                cell.write().lower.push(Type::TVar(n.clone()));
                                self.note(v, Added::Lower);
                                let upper: SmallVec<[Type; 1]> = upper
                                    .iter()
                                    .map(|u| self.extrude(u, pos, at, twins))
                                    .collect();
                                n.cell().write().constraints = upper
                            }
                        }
                        Type::TVar(n)
                    }
                }
            }
            Type::Fn(ft) => {
                let args = ft.args.iter().map(|a| FnArgType {
                    kind: a.kind.clone(),
                    typ: self.extrude(&a.typ, !pos, at, twins),
                });
                let args = Arc::from_iter(args);
                Type::Fn(Arc::new(FnType {
                    args,
                    vargs: ft.vargs.as_ref().map(|t| self.extrude(t, !pos, at, twins)),
                    rtype: self.extrude(&ft.rtype, pos, at, twins),
                    throws: self.extrude(&ft.throws, pos, at, twins),
                    explicit_throws: ft.explicit_throws,
                    quantifiers: ft.quantifiers.clone(),
                    lambda_ids: ft.lambda_ids.clone(),
                }))
            }
            other => other
                .cow_children(&mut |c| Some(self.extrude(c, pos, at, twins)))
                .unwrap_or_else(|| other.clone()),
        }
    }

    /// What is still deeper than `at` once its cells have twins there is a
    /// declared variable of a definition `at` is outside of.
    fn escapes(&self, t: &Type, at: Level) -> Result<()> {
        if depth(t) > at.depth() {
            bail!(
                "a declared type variable meets a value from outside the definition that declares it ({t})"
            )
        }
        Ok(())
    }

    /// `a`, which is no open cell and no union, fits a member of the union
    /// `b`, whose members are `ms`.
    fn below_union(&mut self, env: &Env, a: &Type, ms: &[Type], b: &Type) -> Result<()> {
        // a member that is a typedef's name is met as what it stands for
        let mut members: SmallVec<[Type; 4]> = SmallVec::new();
        for m in ms.iter().map(shallow) {
            match &m {
                Type::Ref(_) => members.push(m.lookup_ref(env)?),
                _ => members.push(m),
            }
        }
        let primitive = |bit: Typ| {
            members.iter().any(|m| matches!(m, Type::Primitive(p) if p.contains(bit)))
        };
        if members.iter().any(|m| union_identical(m, a))
            || (matches!(a, Type::Error(_)) && primitive(Typ::Error))
            || (matches!(a, Type::Array(_)) && primitive(Typ::Array))
        {
            return Ok(());
        }
        let cells: SmallVec<[&Type; 2]> =
            members.iter().filter(|m| open(m).is_some()).collect();
        if let Type::Primitive(p) = a {
            let rest = members.iter().fold(*p, |rest, m| match m {
                Type::Primitive(q) => rest & !*q,
                _ => rest,
            });
            return match (rest.is_empty(), &cells[..]) {
                (true, _) => Ok(()),
                (false, [m]) => self.constrain(env, &Type::Primitive(rest), m),
                (false, []) => mismatch(b, a),
                (false, _) => Ok(self.held.push((a.clone(), b.clone()))),
            };
        }
        let takers: SmallVec<[&Type; 2]> =
            members.iter().filter(|m| open(m).is_none() && alike(m, a)).collect();
        match (&takers[..], &cells[..]) {
            ([m], []) | ([], [m]) => self.constrain(env, a, m),
            ([], []) => mismatch(b, a),
            // the choice is made once `a` is decided: a decided member
            // takes it or is out, and what is left takes it if it is one
            _ if self.resolving && !a.has_free() => {
                let (decided, loose): (SmallVec<[&Type; 2]>, SmallVec<[&Type; 2]>) =
                    takers.iter().copied().partition(|m| !m.has_free());
                if decided.iter().any(|m| self.attempt(env, a, m)) {
                    return Ok(());
                }
                match (&loose[..], &cells[..]) {
                    ([m], []) | ([], [m]) => self.constrain(env, a, m),
                    ([], []) => mismatch(b, a),
                    _ => Ok(self.held.push((a.clone(), b.clone()))),
                }
            }
            _ => Ok(self.held.push((a.clone(), b.clone()))),
        }
    }

    /// A pair with a typedef's name on a side, by what the name stands for.
    fn unfolded(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        let met = |(x, y): &(Type, Type)| union_identical(x, a) && union_identical(y, b);
        if self.unfolding.iter().any(met) {
            return Ok(());
        }
        let expand = |t: &Type| match t {
            Type::Ref(_) => t.lookup_ref(env),
            t => Ok(t.clone()),
        };
        let (x, y) = (expand(a)?, expand(b)?);
        self.unfolding.push((a.clone(), b.clone()));
        let r = self.constrain(env, &x, &y);
        self.unfolding.pop();
        r
    }

    /// `f` may be called wherever `g` may be.
    fn fn_below(&mut self, env: &Env, f: &FnType, g: &FnType) -> Result<()> {
        let positional = |ft: &FnType| -> SmallVec<[Type; 4]> {
            let args = ft.args.iter();
            args.filter(|a| matches!(a.kind, FnArgKind::Positional { .. }))
                .map(|a| a.typ.clone())
                .collect()
        };
        let labeled = |ft: &FnType| -> SmallVec<[(ArcStr, bool, Type); 4]> {
            let args = ft.args.iter().filter_map(|a| match &a.kind {
                FnArgKind::Labeled { name, has_default } => {
                    Some((name.clone(), *has_default, a.typ.clone()))
                }
                FnArgKind::Positional { .. } => None,
            });
            args.collect()
        };
        let (fp, gp) = (positional(f), positional(g));
        for (i, ga) in gp.iter().enumerate() {
            match (fp.get(i), &f.vargs) {
                (Some(fa), _) => self.constrain(env, ga, fa)?,
                (None, Some(v)) => self.constrain(env, ga, v)?,
                (None, None) => bail!(
                    "type mismatch: a function of {} arguments where one of {} is wanted",
                    fp.len(),
                    gp.len()
                ),
            }
        }
        if fp.len() > gp.len() {
            bail!(
                "type mismatch: a function of {} arguments where one of {} is wanted",
                fp.len(),
                gp.len()
            )
        }
        if let (Some(fv), Some(gv)) = (&f.vargs, &g.vargs) {
            self.constrain(env, gv, fv)?
        }
        let (fl, gl) = (labeled(f), labeled(g));
        for (name, optional, ga) in gl.iter() {
            match fl.iter().find(|(n, ..)| n == name) {
                Some((_, false, _)) if *optional => {
                    bail!("type mismatch: #{name} is required")
                }
                Some((_, _, fa)) => self.constrain(env, ga, fa)?,
                None => bail!("type mismatch: no argument #{name}"),
            }
        }
        if let Some((name, ..)) =
            fl.iter().find(|(n, opt, _)| !opt && !gl.iter().any(|(m, ..)| m == n))
        {
            bail!("type mismatch: #{name} is required")
        }
        self.constrain(env, &f.rtype, &g.rtype)?;
        self.constrain(env, &f.throws, &g.throws)
    }
}

#[cfg(test)]
mod tests {
    use super::Solving;
    use crate::{
        LambdaId,
        env::Env,
        typ::{
            TVar, Type,
            cast::tests::{parsed, typed},
            tvar::{AtLevel, Level},
        },
    };
    use ahash::AHashMap;
    use arcstr::ArcStr;

    /// A variable of the top level.
    fn var(name: &str) -> (TVar, Type) {
        let _at = AtLevel::enter(Level::TOP);
        let tv = TVar::empty_named(ArcStr::from(name));
        (tv.clone(), Type::TVar(tv))
    }

    /// A variable of a definition nested `depth` deep.
    fn var_at(name: &str, depth: u32) -> (TVar, Type) {
        let _at = AtLevel::enter(Level::Def { depth, owner: LambdaId::new() });
        let tv = TVar::empty_named(ArcStr::from(name));
        (tv.clone(), Type::TVar(tv))
    }

    /// `t` written at the root of `env`, each variable it names standing
    /// for the type `vars` gives it.
    fn over(env: &Env, t: &str, vars: &[(&str, &Type)]) -> Type {
        let known = vars.iter().map(|(n, t)| (ArcStr::from(*n), (*t).clone()));
        parsed(env, t).replace_tvars(&AHashMap::from_iter(known))
    }

    fn shown(ts: &[Type]) -> Vec<String> {
        ts.iter().map(|t| t.to_string()).collect()
    }

    #[test]
    fn decided_types_are_related_as_they_are() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let t = |t: &str| parsed(&env, t);
        assert!(s.constrain(&env, &t("i64"), &t("[i64, string]")).is_ok());
        assert!(!s.attempt(&env, &t("string"), &t("i64")));
        assert!(s.constrain(&env, &t("Array<i64>"), &t("Array<[i64, null]>")).is_ok());
    }

    #[test]
    fn a_lower_bound_meets_a_later_upper_bound() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        assert!(s.constrain(&env, &parsed(&env, "Array<i64>"), &ta).is_ok());
        assert!(s.constrain(&env, &ta, &parsed(&env, "Array<[i64, string]>")).is_ok());
        assert!(!s.attempt(&env, &ta, &parsed(&env, "Array<string>")));
        assert!(!a.is_bound());
        assert_eq!(shown(&a.cell_lower()), ["Array<i64>"]);
    }

    #[test]
    fn an_upper_bound_meets_a_later_lower_bound() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (_, ta) = var("a");
        assert!(s.constrain(&env, &ta, &parsed(&env, "i64")).is_ok());
        assert!(!s.attempt(&env, &parsed(&env, "string"), &ta));
        assert!(s.constrain(&env, &parsed(&env, "i64"), &ta).is_ok());
    }

    #[test]
    fn a_bound_passes_along_an_edge_between_two_variables() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((_, ta), (b, tb)) = (var("a"), var("b"));
        assert!(s.constrain(&env, &parsed(&env, "i64"), &ta).is_ok());
        assert!(s.constrain(&env, &ta, &tb).is_ok());
        assert!(shown(&b.cell_lower()).contains(&"i64".to_string()));
        assert!(!s.attempt(&env, &tb, &parsed(&env, "string")));
        assert!(b.cell_constraints().is_empty());
        assert!(s.constrain(&env, &tb, &parsed(&env, "[i64, null]")).is_ok());
        // what arrives later passes too
        assert!(!s.attempt(&env, &parsed(&env, "string"), &ta));
        assert!(s.constrain(&env, &parsed(&env, "null"), &ta).is_ok());
    }

    #[test]
    fn a_function_takes_its_parameters_the_other_way() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        let f = over(&env, "fn(x: 'a) -> 'a", &[("a", &ta)]);
        let g = parsed(&env, "fn(x: i64) -> [i64, string]");
        assert!(s.constrain(&env, &f, &g).is_ok());
        assert_eq!(shown(&a.cell_lower()), ["i64"]);
        assert_eq!(shown(&a.cell_constraints()), ["[i64, string]"]);
        let h = parsed(&env, "fn(x: string) -> string");
        assert!(!s.attempt(&env, &f, &h));
    }

    #[test]
    fn a_recursive_typedef_is_unfolded_to_its_variable() {
        let (env, _) = typed(&[("L<'a>", "[`C('a, L<'a>), `N]")], "null");
        let mut s = Solving::default();
        let (x, tx) = var("x");
        let list = over(&env, "L<'x>", &[("x", &tx)]);
        assert!(s.constrain(&env, &parsed(&env, "`C(i64, `C(i64, `N))"), &list).is_ok());
        assert_eq!(shown(&x.cell_lower()), ["i64"]);
        assert!(!s.attempt(&env, &tx, &parsed(&env, "string")));
        // and a list of the variable is a list of what holds it
        assert!(s.constrain(&env, &list, &parsed(&env, "L<[i64, null]>")).is_ok());
    }

    #[test]
    fn a_writable_reference_holds_one_type() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        let r = over(&env, "&mut 'a", &[("a", &ta)]);
        assert!(s.constrain(&env, &r, &parsed(&env, "&mut i64")).is_ok());
        assert_eq!(shown(&a.cell_lower()), ["i64"]);
        assert_eq!(shown(&a.cell_constraints()), ["i64"]);
        let (_, tb) = var("b");
        let shared = over(&env, "&'b", &[("b", &tb)]);
        assert!(s.constrain(&env, &shared, &parsed(&env, "&[i64, string]")).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "&i64"), &shared).is_ok());
    }

    #[test]
    fn a_deeper_type_is_copied_out_to_the_cell_it_reaches() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((y, ty), (x, tx)) = (var("y"), var_at("x", 1));
        // an inner variable flows out: the outer cell holds a twin of it,
        // never the inner cell
        assert!(s.constrain(&env, &over(&env, "Array<'x>", &[("x", &tx)]), &ty).is_ok());
        let [Type::Array(elem)] = &y.cell_lower()[..] else { panic!() };
        let Type::TVar(twin) = &**elem else { panic!() };
        assert!(!twin.same_cell(&x));
        assert_eq!(twin.level().depth(), 0);
        // and what flows into the inner one reaches the twin
        assert!(s.constrain(&env, &parsed(&env, "i64"), &tx).is_ok());
        assert_eq!(shown(&twin.cell_lower()), ["i64"]);
        assert!(!s.attempt(&env, &ty, &parsed(&env, "Array<string>")));
        assert!(s.constrain(&env, &ty, &parsed(&env, "Array<[i64, null]>")).is_ok());
    }

    #[test]
    fn a_declared_variable_is_its_bounds_and_takes_none() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var_at("a", 1);
        a.cell().write().constraints.push(parsed(&env, "[i64, null]"));
        let _gate = a.open_rigid();
        assert!(s.constrain(&env, &ta, &parsed(&env, "[i64, string, null]")).is_ok());
        assert!(!s.attempt(&env, &ta, &parsed(&env, "i64")));
        assert!(!s.attempt(&env, &parsed(&env, "i64"), &ta));
        assert!(a.cell_lower().is_empty());
        // its own values may flow out to an outer cell; a value from
        // outside may not be taken for one of its
        let (y, ty) = var("y");
        assert!(s.constrain(&env, &ta, &ty).is_ok());
        assert!(
            !y.cell_lower().iter().any(|l| matches!(l, Type::TVar(v) if v.same_cell(&a)))
        );
        let (_, tz) = var("z");
        assert!(s.constrain(&env, &tz, &ta).is_err());
    }

    #[test]
    fn a_union_with_one_open_member_gives_it_what_the_others_do_not_take() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (a, ta) = var("a");
        let opt = over(&env, "['a, null]", &[("a", &ta)]);
        assert!(s.constrain(&env, &parsed(&env, "null"), &opt).is_ok());
        assert!(a.cell_lower().is_empty());
        assert!(s.constrain(&env, &parsed(&env, "[i64, null]"), &opt).is_ok());
        assert_eq!(shown(&a.cell_lower()), ["i64"]);
        assert_eq!(shown(&a.cell_direct()), ["i64"]);
    }

    #[test]
    fn what_one_cell_passes_to_an_open_member_arrives_as_one_feeder() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((_, te), (a, ta)) = (var("e"), var("a"));
        assert!(s.constrain(&env, &parsed(&env, "i64"), &te).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "Array<i64>"), &te).is_ok());
        let opt = over(&env, "['a, null]", &[("a", &ta)]);
        assert!(s.constrain(&env, &te, &opt).is_ok());
        // both of the cell's types reach 'a, but as one thing met directly
        assert_eq!(a.cell_lower().len(), 2);
        assert_eq!(a.cell_direct().len(), 1);
    }

    #[test]
    fn a_choice_between_two_members_is_held() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((a, ta), (_, tx)) = (var("a"), var("x"));
        let both = over(&env, "[Array<'a>, Array<string>]", &[("a", &ta)]);
        let one = over(&env, "Array<'x>", &[("x", &tx)]);
        assert!(s.constrain(&env, &one, &both).is_ok());
        assert!(a.cell_lower().is_empty());
        assert_eq!(s.held.len(), 1);
        // a failed try takes back what it held
        assert!(!s.attempt(
            &env,
            &over(&env, "(Array<'x>, i64)", &[("x", &tx)]),
            &over(&env, "([Array<'a>, Array<string>], string)", &[("a", &ta)])
        ));
        assert_eq!(s.held.len(), 1);
    }
}
