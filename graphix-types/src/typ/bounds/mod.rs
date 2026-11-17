//! Inference by bounds. While a definition is checked no variable is
//! bound: what flows into a variable is a lower bound of its cell, what a
//! use demands of it an upper bound (a constraint), and each new bound is
//! met with the bounds already on the other side, so the bounds are always
//! consistent with each other.

use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        Bound, FnArgKind, FnArgType, FnType, Mutability, TVar, Type, node_addr,
        setops::strictly_identical,
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

mod select;
pub use select::{Arm, Pat};

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
    /// A scheme's variable: each use of the scheme takes a copy, and
    /// nothing fixes it.
    pub(crate) quantified: bool,
    /// Decided by what is demanded of it, nothing having flowed in: as a
    /// lower bound of another cell it says nothing.
    pub(crate) demanded: bool,
    /// The check that bounded it has it in its list.
    pub(crate) listed: bool,
    /// The uses waiting on what flows into the cell, and whether each has
    /// met a value.
    pub(crate) uses: SmallVec<[(Use, bool); 1]>,
}

/// A call's argument: its label, if it is passed by one, and its type.
pub type Arg = (Option<ArcStr>, Type);

/// A use that needs the shape of what it is given: it is applied to each
/// decided type that flows into the cell it waits on, as that type
/// arrives.
#[derive(Debug, Clone)]
pub enum Use {
    /// A read of the field, which goes to `out`.
    Field { field: ArcStr, out: Type },
    /// A read of a tuple's element, or of an error's payload.
    Index { i: usize, out: Type },
    /// A call with `args`, whose result goes to `out`.
    Call { args: SmallVec<[Arg; 4]>, out: Type },
    /// A struct update: `out` takes the struct with each of `fields` at
    /// its new type.
    With { fields: SmallVec<[(ArcStr, Type); 2]>, out: Type },
    /// The scrutinee of a select ([`Solving::select`]).
    Select(usize),
}

impl Use {
    fn needs(&self) -> &'static str {
        match self {
            Use::Field { .. } => "reading a struct",
            Use::Index { .. } => "reading a tuple",
            Use::Call { .. } => "reading a fn",
            Use::With { .. } => "updating a struct",
            Use::Select(_) => "a select",
        }
    }

    /// The types the use holds, each through `f`.
    fn map(&self, f: &mut impl FnMut(&Type) -> Type) -> Use {
        match self {
            Use::Field { field, out } => Use::Field { field: field.clone(), out: f(out) },
            Use::Index { i, out } => Use::Index { i: *i, out: f(out) },
            Use::Call { args, out } => Use::Call {
                args: args.iter().map(|(l, t)| (l.clone(), f(t))).collect(),
                out: f(out),
            },
            Use::With { fields, out } => Use::With {
                fields: fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
                out: f(out),
            },
            Use::Select(id) => Use::Select(*id),
        }
    }

    fn each(&self, f: &mut impl FnMut(&Type)) {
        match self {
            Use::Field { out, .. } | Use::Index { out, .. } => f(out),
            Use::Call { args, out } => {
                f(out);
                args.iter().for_each(|(_, t)| f(t))
            }
            Use::With { fields, out } => {
                f(out);
                fields.iter().for_each(|(_, t)| f(t))
            }
            Use::Select(_) => (),
        }
    }
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
    /// The third is set where a scheme's variables hold the choice, so
    /// each instance takes a copy.
    held: Vec<(Type, Type, bool)>,
    /// Every cell this check has bounded, in the order first met: a
    /// definition's own are those met since it began.
    cells: Vec<TVar>,
    /// The cells being fixed, by address.
    fixing: AHashSet<usize>,
    /// Cells inside a generic definition that take more than one value
    /// and whose one type its variables decide: each instance checks a
    /// copy.
    singles: Vec<TVar>,
    selects: Vec<select::Select>,
    /// A held choice is being made: a decided member that does not fit is
    /// out.
    resolving: bool,
    /// The cells made to carry what one cell passes to a union's open
    /// member, by address.
    passing: AHashMap<usize, TVar>,
}

/// A kind of use a check may still owe when a definition closes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Pending {
    Held,
    Select,
}

#[derive(Clone, Copy)]
enum Added {
    Lower,
    Upper,
    Direct,
    Feeder,
    Sink,
}

/// The union of `ts` as they stand: flat, each member once, the primitives
/// as one member. No two cells are made one, whatever the members share.
pub fn union_of(ts: impl IntoIterator<Item = Type>) -> Type {
    Type::flatten_set(ts)
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
    match bounds.iter().any(|b| strictly_identical(b, t)) {
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
        self.attempting(|s| s.constrain(env, a, b))
    }

    /// Whether `f` succeeds; where it does not, every bound it added is
    /// taken back.
    fn attempting(&mut self, f: impl FnOnce(&mut Self) -> Result<()>) -> bool {
        let (mark, held) = (self.added.len(), self.held.len());
        self.attempts += 1;
        let fits = f(self).is_ok();
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
                        x if ys.iter().any(|m| strictly_identical(m, x)) => (),
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
                        self.constrain(env, &union_of(all.into_iter().cloned()), to)
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
                self.constrain(env, a, &union_of(members.into_iter().cloned()))
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
                self.track(v);
                if let Some(w) = vb {
                    if w.level().depth() == at.depth() {
                        known(w, |k| k.feeders.push(v.clone()));
                        self.note(w, Added::Feeder)
                    }
                    if !edge {
                        known(w, |k| k.direct.push(a.clone()));
                        self.note(w, Added::Direct)
                    }
                } else {
                    self.demanded(env, v, b)?
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
        self.track(w);
        if !edge {
            known(w, |k| k.direct.push(a.clone()));
            self.note(w, Added::Direct)
        }
        // a use of `w` meets the bound; where the bound is a cell, what
        // that cell holds
        let met = match open(a) {
            Some(v) => flowing(v, &mut SmallVec::new()),
            None => SmallVec::from_iter([a.clone()]),
        };
        for a in met.iter() {
            for i in 0..uses(w).len() {
                let u = uses(w)[i].0.clone();
                if self.apply(env, &u, a)? {
                    known(w, |k| k.uses[i].1 = true)
                }
            }
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
        if members.iter().any(|m| strictly_identical(m, a))
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
                (false, _) => Ok(self.held.push((a.clone(), b.clone(), false))),
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
                    _ => Ok(self.held.push((a.clone(), b.clone(), false))),
                }
            }
            _ => Ok(self.held.push((a.clone(), b.clone(), false))),
        }
    }

    /// A pair with a typedef's name on a side, by what the name stands for.
    fn unfolded(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        let met =
            |(x, y): &(Type, Type)| strictly_identical(x, a) && strictly_identical(y, b);
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

    /// Declare `bound` of the open cell `v`, which nothing has flowed into
    /// yet: every value of it will be one of `bound`.
    pub fn declare(&mut self, v: &TVar, bound: Type) {
        v.cell().write().add_constraint(bound);
        self.track(v)
    }

    /// Have `v` among the cells this check fixes.
    pub fn track(&mut self, v: &TVar) {
        if !known(v, |k| std::mem::replace(&mut k.listed, true)) {
            self.cells.push(v.clone())
        }
    }

    /// How many cells the check has met: a definition's own are those met
    /// after its mark.
    pub fn mark(&self) -> usize {
        self.cells.len()
    }
}

/// `t` with every bound cell replaced by what it is bound to.
fn deep(t: &Type) -> Type {
    map_cells(t, &mut |_| None)
}

/// `t` with each open cell `f` answers for replaced, bound cells looked
/// through.
fn map_cells(t: &Type, f: &mut impl FnMut(&TVar) -> Option<Type>) -> Type {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => map_cells(&b, f),
            None => f(tv).unwrap_or_else(|| t.clone()),
        },
        t => t.cow_children(&mut |c| Some(map_cells(c, f))).unwrap_or_else(|| t.clone()),
    })
}

/// The unbound cells of `t`, each once, bound cells looked through;
/// declared variables inside their definition too where `rigid`.
fn cells_of(t: &Type, rigid: bool, out: &mut SmallVec<[TVar; 4]>) {
    fn go(
        t: &Type,
        rigid: bool,
        out: &mut SmallVec<[TVar; 4]>,
        seen: &mut AHashSet<usize>,
    ) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => go(&b, rigid, out, seen),
                None if (rigid || !tv.is_rigid())
                    && !out.iter().any(|o| o.same_cell(tv)) =>
                {
                    out.push(tv.clone())
                }
                None => (),
            },
            t if node_addr(t).is_some_and(|n| !seen.insert(n)) => (),
            t => {
                let _ = t.try_for_each_child::<()>(&mut |c| {
                    go(c, rigid, out, seen);
                    ControlFlow::Continue(())
                });
            }
        })
    }
    go(t, rigid, out, &mut LPooled::take())
}

fn open_cells(t: &Type) -> SmallVec<[TVar; 4]> {
    let mut out = SmallVec::new();
    cells_of(t, false, &mut out);
    out
}

fn quantified(v: &TVar) -> bool {
    v.cell().read().solving.as_ref().is_some_and(|k| k.quantified)
}

fn demanded(v: &TVar) -> bool {
    v.cell().read().solving.as_ref().is_some_and(|k| k.demanded)
}

fn feeders(v: &TVar) -> SmallVec<[TVar; 1]> {
    let cell = v.cell();
    let c = cell.read();
    c.solving.as_ref().map(|k| k.feeders.clone()).unwrap_or_default()
}

fn singleton(v: &TVar) -> bool {
    v.cell_constraints().iter().any(|c| matches!(c, Type::Bound(Bound::Singleton)))
}

/// Whether anything says what `v` is: it is bound, or something that is
/// no undecided cell flows into it.
fn decided(v: &TVar, seen: &mut SmallVec<[usize; 8]>) -> bool {
    seen.push(v.cell_addr());
    let mut through = |u: &TVar| !seen.contains(&u.cell_addr()) && decided(u, seen);
    v.is_bound()
        || v.cell_lower().iter().any(|l| match open(&shallow(l)) {
            Some(u) => through(u),
            None => true,
        })
        || feeders(v).iter().any(|u| through(u))
}

impl Solving {
    /// Every unbound cell reachable from `t` through bounds.
    fn reach(&self, t: &Type, seen: &mut SmallVec<[TVar; 4]>) {
        let mut found = SmallVec::new();
        cells_of(t, true, &mut found);
        for v in found {
            if !seen.iter().any(|s| s.same_cell(&v)) {
                seen.push(v.clone());
                let (lower, upper) = (v.cell_lower(), v.cell_constraints());
                lower.iter().chain(upper.iter()).for_each(|b| self.reach(b, seen));
                feeders(&v).iter().for_each(|u| self.reach(&Type::TVar(u.clone()), seen));
                // and what its uses produce
                for (u, _) in uses(&v).iter() {
                    u.each(&mut |t| self.reach(t, seen));
                    if let Use::Select(id) = u {
                        for b in self.select_cells(*id) {
                            self.reach(&Type::TVar(b), seen)
                        }
                    }
                }
            }
        }
    }

    /// Mark the variables of a declared scheme `t` as its own: those
    /// deeper than `depth`.
    pub fn quantify(&mut self, t: &Type, depth: u32) {
        let mut kept = SmallVec::new();
        self.reach(t, &mut kept);
        for v in kept.iter().filter(|v| v.level().depth() > depth) {
            known(v, |k| k.quantified = true)
        }
    }

    /// A use of `v` that met no value: it needs a shape nothing gave it.
    fn unread(&self, v: &TVar) -> Result<()> {
        match uses(v).iter().find(|(u, met)| !met && !matches!(u, Use::Select(_))) {
            None => Ok(()),
            Some((u, _)) => {
                bail!("{} needs its type known here: annotate the binding", u.needs())
            }
        }
    }

    /// Close the definition of a generic binding of type `t`, checked one
    /// level deeper than `depth`: what `t` reaches stays, as the scheme's
    /// variables; every other cell the check has met since `mark` is
    /// fixed.
    pub fn generalize(
        &mut self,
        env: &Env,
        t: &Type,
        depth: u32,
        mark: usize,
    ) -> Result<()> {
        self.quantify(t, depth);
        let mut kept = SmallVec::new();
        self.reach(t, &mut kept);
        kept.iter()
            .filter(|v| v.level().depth() > depth && !v.is_bound())
            .try_for_each(|v| self.unread(v))?;
        self.fix_scope(env, depth + 1, false, mark)?;
        for v in self.cells[mark..].iter() {
            let pending = v.level().depth() > depth
                && !v.is_bound()
                && !v.is_rigid()
                && !quantified(v)
                && !v.joins()
                && v.cell_direct().len() > 1;
            if pending {
                self.singles.push(v.clone())
            }
        }
        Ok(())
    }

    /// Fix every cell an input left open.
    pub fn finish(&mut self, env: &Env) -> Result<()> {
        self.fix_scope(env, 0, true, 0)?;
        self.dead_arms()
    }

    /// A use of the scheme `t`: a copy of it, at the level `at`, with a
    /// cell of its own for each of the scheme's (those deeper than
    /// `above`), bounded as the scheme's are.
    pub fn instantiate(&mut self, t: &Type, above: u32, at: Level) -> Type {
        let mut copies = LPooled::take();
        let t = self.copy(t, above, at, &mut copies);
        let mine = |t: &Type, copies: &AHashMap<usize, TVar>| {
            let mut all = SmallVec::new();
            cells_of(t, true, &mut all);
            all.iter().any(|v| copies.contains_key(&v.cell_addr()))
        };
        // what the definition left to its instances
        for (a, b, scheme) in self.held.clone() {
            if scheme && (mine(&a, &copies) || mine(&b, &copies)) {
                let pair = (
                    self.copy(&a, above, at, &mut copies),
                    self.copy(&b, above, at, &mut copies),
                );
                self.held.push((pair.0, pair.1, false))
            }
        }
        for v in self.singles.clone() {
            let mut all = SmallVec::new();
            self.reach(&Type::TVar(v.clone()), &mut all);
            if v.level().depth() > above
                && all.iter().any(|u| copies.contains_key(&u.cell_addr()))
            {
                self.copy(&Type::TVar(v), above, at, &mut copies);
            }
        }
        // a select of the definition that waits on one of its variables
        // is the instance's to finish
        for id in 0..self.selects.len() {
            for w in self.selects[id].waiting.clone() {
                if mine(&w, &copies) {
                    let new = self.select_copy(id, &mut |s, v| {
                        s.copy_cell(v, above, at, &mut copies)
                    });
                    let w = self.copy(&w, above, at, &mut copies);
                    self.select_wait(new, w)
                }
            }
        }
        t
    }

    /// The copy of the cell `v`, or `v` where it is no deeper than
    /// `above`.
    fn copy_cell(
        &mut self,
        v: &TVar,
        above: u32,
        at: Level,
        copies: &mut AHashMap<usize, TVar>,
    ) -> TVar {
        match &self.copy(&Type::TVar(v.clone()), above, at, copies) {
            Type::TVar(n) => n.clone(),
            _ => v.clone(),
        }
    }

    fn copy(
        &mut self,
        t: &Type,
        above: u32,
        at: Level,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Type {
        let mut made: SmallVec<[(TVar, TVar); 4]> = SmallVec::new();
        let t = map_cells(t, &mut |v| {
            if v.level().depth() <= above {
                return None;
            }
            if let Some(n) = copies.get(&v.cell_addr()) {
                return Some(Type::TVar(n.clone()));
            }
            let n = {
                let _at = AtLevel::enter(at);
                TVar::empty_named(v.name.clone())
            };
            copies.insert(v.cell_addr(), n.clone());
            made.push((v.clone(), n.clone()));
            Some(Type::TVar(n))
        });
        // the copy is bounded and fed as the original is
        for (v, n) in made {
            let (lower, mut upper) = (v.cell_lower(), v.cell_constraints());
            let (join, direct, fed, sinks, used) = {
                let cell = v.cell();
                let c = cell.read();
                match c.solving.as_ref() {
                    Some(k) => (
                        k.join && c.rigid_gates == 0,
                        k.direct.clone(),
                        k.feeders.clone(),
                        k.sinks.clone(),
                        k.uses.clone(),
                    ),
                    None => Default::default(),
                }
            };
            upper.extend(sinks.into_iter().map(Type::TVar));
            let all = |s: &mut Self, ts: &[Type], copies: &mut AHashMap<usize, TVar>| {
                ts.iter().map(|t| s.copy(t, above, at, copies)).collect()
            };
            let (lower, upper, direct): (
                SmallVec<[Type; 1]>,
                SmallVec<[Type; 1]>,
                SmallVec<[Type; 1]>,
            ) = (
                all(self, &lower, copies),
                all(self, &upper, copies),
                all(self, &direct, copies),
            );
            let mut feeds: SmallVec<[TVar; 1]> = SmallVec::new();
            for u in fed.iter() {
                if let Type::TVar(u) =
                    &self.copy(&Type::TVar(u.clone()), above, at, copies)
                {
                    feeds.push(u.clone())
                }
            }
            {
                let cell = n.cell();
                let mut c = cell.write();
                (c.lower, c.constraints) = (lower, upper);
                let k = c.solving.get_or_insert_default();
                (k.join, k.direct, k.feeders) = (join, direct, feeds)
            }
            // the copy is used as the original is: what flows into it in
            // this instance meets the same reads and calls
            for (u, met) in used.iter() {
                let u = match u {
                    Use::Select(id) => {
                        Use::Select(self.select_copy(*id, &mut |s, v| {
                            s.copy_cell(v, above, at, copies)
                        }))
                    }
                    u => u.map(&mut |t| self.copy(t, above, at, copies)),
                };
                let met = *met && !matches!(u, Use::Select(_));
                known(&n, |k| k.uses.push((u, met)))
            }
            self.track(&n)
        }
        t
    }

    fn fixable(&self, v: &TVar, min: u32) -> bool {
        let mut all = SmallVec::new();
        self.reach(&Type::TVar(v.clone()), &mut all);
        all.iter().all(|u| u.level().depth() >= min && !quantified(u))
    }

    /// Fix the cells met since `mark` that are at `min` or deeper and that
    /// no scheme variable reaches; with `last`, every cell left.
    fn fix_scope(&mut self, env: &Env, min: u32, last: bool, mark: usize) -> Result<()> {
        let mut progress = true;
        // at the input's end, choices that wait on each other are made in
        // whatever order is left
        let mut ordered = true;
        while progress || (last && ordered) {
            if !progress {
                ordered = false
            }
            progress = false;
            // a pending use (a held choice, a select still waiting) waits
            // for any other whose result reaches what it reads: its kind
            // and index, and everything its result reaches
            let mut produced: Vec<((Pending, usize), SmallVec<[TVar; 4]>)> = vec![];
            if ordered {
                let closure = |s: &Self, outs: SmallVec<[TVar; 4]>| {
                    let mut all = SmallVec::new();
                    outs.iter().for_each(|v| s.reach(&Type::TVar(v.clone()), &mut all));
                    all
                };
                for (i, (_, b, _)) in self.held.iter().enumerate() {
                    produced.push(((Pending::Held, i), closure(self, open_cells(b))))
                }
                for id in 0..self.selects.len() {
                    if !self.selects[id].waiting.is_empty() {
                        let binds = self.select_cells(id);
                        produced.push(((Pending::Select, id), closure(self, binds)))
                    }
                }
            }
            let free = |key: (Pending, usize), ins: &[TVar]| {
                !produced.iter().any(|(k, c)| {
                    *k != key && ins.iter().any(|v| c.iter().any(|u| u.same_cell(v)))
                })
            };
            for i in (0..self.held.len()).rev() {
                let (a, b, scheme) = self.held[i].clone();
                if scheme {
                    continue;
                }
                let (avs, bvs) = (open_cells(&a), open_cells(&b));
                let ready = avs.iter().chain(bvs.iter()).all(|v| self.fixable(v, min));
                if ready && free((Pending::Held, i), &avs) {
                    // a member something has decided is decided here; one
                    // nothing has stays open, to take what the others do
                    // not
                    self.held.swap_remove(i);
                    avs.iter().try_for_each(|v| self.fix(env, v))?;
                    for v in bvs.iter() {
                        if decided(v, &mut SmallVec::new()) {
                            self.fix(env, v)?
                        }
                    }
                    let n = self.held.len();
                    self.resolving = true;
                    let r = self.constrain(env, &deep(&a), &deep(&b));
                    self.resolving = false;
                    r?;
                    progress |= self.held.len() == n
                } else if avs.iter().chain(bvs.iter()).any(quantified) {
                    self.held[i].2 = true
                }
            }
            let ready = |s: &Self, id: usize, t: &Type| {
                let ins = open_cells(t);
                ins.iter().all(|v| s.fixable(v, min)) && free((Pending::Select, id), &ins)
            };
            progress |= self.select_waiting(env, &ready)?
        }
        // a cell something flows into first: one nothing flows into may be
        // decided by a cell that holds it
        let (flowing, empty): (Vec<TVar>, Vec<TVar>) = self.cells[mark..]
            .iter()
            .cloned()
            .partition(|v| decided(v, &mut SmallVec::new()));
        // what a choice still held will decide is not known yet: it, and
        // whatever it reaches, waits with the choice
        let mut waiting: SmallVec<[TVar; 4]> = SmallVec::new();
        if !last {
            for (_, b, _) in self.held.iter() {
                for v in open_cells(b) {
                    self.reach(&Type::TVar(v), &mut waiting)
                }
            }
            for id in 0..self.selects.len() {
                if !self.selects[id].waiting.is_empty() {
                    for v in self.select_cells(id) {
                        self.reach(&Type::TVar(v), &mut waiting)
                    }
                }
            }
        }
        for v in flowing.into_iter().chain(empty) {
            let mine = !v.is_bound()
                && !v.is_rigid()
                && !quantified(&v)
                && v.level().depth() >= min;
            let now = last
                || (self.fixable(&v, min) && !waiting.iter().any(|w| w.same_cell(&v)));
            if mine && now {
                self.fix(env, &v)?;
                if !matches!(v.binding(), Some(Type::Bottom)) {
                    self.unread(&v)?
                }
            }
        }
        if last && let Some((a, b, _)) = self.held.iter().find(|h| !h.2) {
            bail!(
                "{a} could go to more than one member of the union {b}, and nothing has decided which"
            )
        }
        Ok(())
    }

    /// Give `v` one type: the union of what flows into it, or, where
    /// nothing does, the narrowest of what its uses demand.
    fn fix(&mut self, env: &Env, v: &TVar) -> Result<()> {
        if v.is_bound()
            || v.is_rigid()
            || quantified(v)
            || !self.fixing.insert(v.cell_addr())
        {
            return Ok(());
        }
        let r = self.fixed(env, v);
        self.fixing.remove(&v.cell_addr());
        v.cell().write().set_binding(Some(r?));
        Ok(())
    }

    fn fixed(&mut self, env: &Env, v: &TVar) -> Result<Type> {
        // a cell nothing has decided may stay open inside a bound: a
        // holder that shares it can still decide it
        let mut flows: SmallVec<[Type; 4]> = SmallVec::new();
        let fed = feeders(v).into_iter().filter(|u| !quantified(u)).map(Type::TVar);
        for l in v.cell_lower().into_iter().chain(fed) {
            if let Some(t) = self.settled(env, &l)?
                && !matches!(t, Type::Bottom)
            {
                flows.push(t)
            }
        }
        if !v.joins() {
            let mut given: SmallVec<[Type; 4]> = SmallVec::new();
            for l in v.cell_direct() {
                if let Some(t) = self.settled(env, &l)?
                    && !matches!(t, Type::Bottom)
                    && !given.iter().any(|g| strictly_identical(g, &t))
                {
                    given.push(t)
                }
            }
            if given.len() > 1 {
                self.holder(env, &given)?
            }
        }
        let single = singleton(v);
        let t = match &flows[..] {
            [] if single => Type::Bottom,
            [] => {
                let mut demands: SmallVec<[Type; 4]> = SmallVec::new();
                for u in v.cell_constraints() {
                    for w in open_cells(&u) {
                        self.fix(env, &w)?
                    }
                    let u = deep(&u);
                    let says = !matches!(u, Type::TVar(_) | Type::Bound(_))
                        && !u.is_trait_ref(env);
                    if says {
                        demands.push(u)
                    }
                }
                known(v, |k| k.demanded = true);
                // a bound that names the cell itself decides nothing alone
                let t = self.meet(env, &demands)?;
                match open_cells(&t).iter().any(|u| u.same_cell(v)) {
                    true => Type::Bottom,
                    false => t,
                }
            }
            [one] => one.clone(),
            many => {
                let all: SmallVec<[&Type; 4]> = many.iter().collect();
                union_of(all.into_iter().cloned())
            }
        };
        let one =
            matches!(&t, Type::Primitive(p) if p.exactly_one().is_some() || p.is_empty());
        if single && !matches!(t, Type::Bottom) && !one {
            // a declared variable is one type, which each call must give
            // it
            match &t {
                Type::TVar(r) if r.is_rigid() => {
                    let cell = r.cell();
                    cell.write().add_constraint(Type::Bound(Bound::Singleton))
                }
                _ => bail!(
                    "cannot compute with {t}: both operands must be one numeric type"
                ),
            }
        }
        Ok(t)
    }

    /// What a lower bound `l` says once the cells in it are fixed: nothing
    /// for a cell nothing flows into (it is fixed from above, later).
    fn settled(&mut self, env: &Env, l: &Type) -> Result<Option<Type>> {
        if let Type::TVar(u) = l
            && demanded(u)
        {
            return Ok(None);
        }
        let l = shallow(l);
        Ok(match open(&l) {
            Some(u) if !decided(u, &mut SmallVec::new()) => None,
            Some(u) => {
                self.fix(env, u)?;
                u.binding().filter(|_| !demanded(u))
            }
            None => {
                for u in open_cells(&l) {
                    if self.fixing.contains(&u.cell_addr()) {
                        bail!("infinite type: a variable holds itself in {l}")
                    }
                    if decided(&u, &mut SmallVec::new()) {
                        self.fix(env, &u)?
                    }
                }
                Some(deep(&l))
            }
        })
    }

    /// One of `ts` holds the others.
    fn holder(&mut self, env: &Env, ts: &[Type]) -> Result<()> {
        // one with a cell nothing has decided is tried last: that cell
        // takes what the others need
        let (decided, loose): (SmallVec<[&Type; 4]>, SmallVec<[&Type; 4]>) =
            ts.iter().partition(|t| !t.has_free());
        for t in decided.into_iter().chain(loose) {
            if self.attempting(|s| ts.iter().try_for_each(|o| s.constrain(env, o, t))) {
                return Ok(());
            }
        }
        let t = &ts[0];
        let other = ts.iter().find(|o| !self.attempt(env, o, t)).unwrap_or(t);
        bail!(
            "type mismatch {t} does not contain {other}: declare the type that holds both"
        )
    }

    /// The one of `ts` every other holds; nothing demanded is bottom.
    fn meet(&mut self, env: &Env, ts: &[Type]) -> Result<Type> {
        if ts.is_empty() {
            return Ok(Type::Bottom);
        }
        for t in ts {
            if self.attempting(|s| ts.iter().try_for_each(|o| s.constrain(env, t, o))) {
                return Ok(t.clone());
            }
        }
        let common = ts.iter().try_fold(None, |acc: Option<_>, t| match (acc, t) {
            (None, Type::Primitive(p)) => Some(Some(*p)),
            (Some(a), Type::Primitive(p)) => Some(Some(a & *p)),
            _ => None,
        });
        match common {
            Some(Some(p)) if !p.is_empty() => Ok(Type::Primitive(p)),
            _ => {
                let all: Vec<String> = ts.iter().map(|t| t.to_string()).collect();
                bail!("unsatisfiable constraints: {}", all.join(" & "))
            }
        }
    }
}

fn uses(v: &TVar) -> SmallVec<[(Use, bool); 1]> {
    let cell = v.cell();
    let c = cell.read();
    c.solving.as_ref().map(|k| k.uses.clone()).unwrap_or_default()
}

/// Every type known to flow into `v`: its own lower bounds and those of
/// the cells among them.
fn flowing(v: &TVar, seen: &mut SmallVec<[usize; 8]>) -> SmallVec<[Type; 4]> {
    seen.push(v.cell_addr());
    let mut out = SmallVec::new();
    for l in v.cell_lower().iter().map(shallow) {
        match open(&l) {
            Some(u) if !seen.contains(&u.cell_addr()) => out.extend(flowing(u, seen)),
            Some(_) => (),
            None => out.push(l),
        }
    }
    out
}

impl Solving {
    /// Register a use of whatever `t` turns out to be.
    pub fn watch(&mut self, env: &Env, t: &Type, u: Use) -> Result<()> {
        let t = shallow(t);
        let Some(v) = open(&t) else { return self.apply(env, &u, &t).map(|_| ()) };
        let mut met = false;
        for l in flowing(v, &mut SmallVec::new()) {
            met |= self.apply(env, &u, &l)?
        }
        known(v, |k| k.uses.push((u, met)));
        self.track(v);
        // a shape demanded of the cell serves, since nothing wider can
        // flow in
        for b in v.cell_constraints() {
            if open(&shallow(&b)).is_none() {
                self.demanded(env, v, &b)?
            }
        }
        Ok(())
    }

    /// `b` is a new upper bound of `v`: a use that needs a shape takes it
    /// from a shape demanded of `v`.
    fn demanded(&mut self, env: &Env, v: &TVar, b: &Type) -> Result<()> {
        let b = shallow(b);
        for i in 0..uses(v).len() {
            let u = uses(v)[i].0.clone();
            let shaped = matches!(
                (&u, &b),
                (Use::Field { .. } | Use::With { .. }, Type::Struct(_))
                    | (Use::Index { .. }, Type::Tuple(_))
                    | (Use::Call { .. }, Type::Fn(_))
            );
            if shaped && self.apply(env, &u, &b)? {
                known(v, |k| k.uses[i].1 = true)
            }
        }
        Ok(())
    }

    /// Apply a use to one type that reaches it; false where the type says
    /// nothing (bottom).
    fn apply(&mut self, env: &Env, u: &Use, a: &Type) -> Result<bool> {
        if let Use::Select(id) = u {
            return self.select_route(env, *id, a).map(|()| true);
        }
        let a = shallow(a);
        ensure_sufficient(|| match (&a, u) {
            (Type::Bottom, _) => Ok(false),
            (Type::Set(ms), _) => {
                let mut any = false;
                for m in ms.iter() {
                    any |= self.apply(env, u, m)?
                }
                Ok(any)
            }
            (Type::Ref(_), _) => self.apply(env, u, &a.lookup_ref(env)?),
            (Type::TVar(v), _) if v.is_rigid() => {
                let mut any = false;
                for b in v.cell_constraints() {
                    if !matches!(b, Type::Bound(_)) && !b.is_trait_ref(env) {
                        any |= self.apply(env, u, &b)?
                    }
                }
                match any {
                    true => Ok(true),
                    false => {
                        bail!(
                            "{} needs its type known here: annotate the binding",
                            u.needs()
                        )
                    }
                }
            }
            (Type::TVar(_), _) => Ok(false),
            (Type::Struct(fs), Use::Field { field, out }) => {
                match fs.iter().find(|(n, ..)| n == field) {
                    Some((_, t, _)) => self.constrain(env, t, out).map(|()| true),
                    None => bail!("{a} has no field {field}"),
                }
            }
            (Type::Struct(fs), Use::With { fields, out }) => {
                if let Some((n, _)) =
                    fields.iter().find(|(n, _)| !fs.iter().any(|(m, ..)| m == n))
                {
                    bail!("{a} has no field {n}")
                }
                let new =
                    |n: &ArcStr| fields.iter().find(|(m, _)| m == n).map(|(_, t)| t);
                let fs = fs
                    .iter()
                    .map(|(n, t, at)| (n.clone(), new(n).unwrap_or(t).clone(), *at));
                let updated = Type::Struct(Arc::from_iter(fs));
                self.constrain(env, &updated, out).map(|()| true)
            }
            (Type::Error(t), Use::Index { i: 0, out }) => {
                self.constrain(env, t, out).map(|()| true)
            }
            (Type::Tuple(ts), Use::Index { i, out }) => match ts.get(*i) {
                Some(t) => self.constrain(env, t, out).map(|()| true),
                None => bail!("{a} has no element {i}"),
            },
            (Type::Fn(ft), Use::Call { args, out }) => {
                self.call(env, ft, args, out).map(|()| true)
            }
            (_, Use::Field { field, .. }) => bail!("{a} has no field {field}"),
            (_, Use::Index { i, .. }) => bail!("{a} has no element {i}"),
            (_, Use::Call { .. }) => bail!("{a} is not a function"),
            (_, Use::With { .. }) => bail!("{a} is not a struct"),
            (_, Use::Select(_)) => Ok(false),
        })
    }

    /// A call of a function whose type is known: each argument fits its
    /// parameter, in the order written, and the result goes to `out`.
    pub fn call(
        &mut self,
        env: &Env,
        ft: &FnType,
        args: &[Arg],
        out: &Type,
    ) -> Result<()> {
        let labeled = |a: &FnArgType| match &a.kind {
            FnArgKind::Labeled { name, has_default } => {
                Some((name.clone(), *has_default))
            }
            FnArgKind::Positional { .. } => None,
        };
        let mut formals = ft.args.iter().filter(|a| labeled(a).is_none());
        for (_, arg) in args.iter().filter(|(l, _)| l.is_none()) {
            match (formals.next(), &ft.vargs) {
                (Some(f), _) => self.constrain(env, arg, &f.typ)?,
                (None, Some(v)) => self.constrain(env, arg, v)?,
                (None, None) => bail!("too many arguments"),
            }
        }
        if formals.next().is_some() {
            bail!("missing argument")
        }
        for (label, arg) in args.iter().filter_map(|(l, t)| l.as_ref().map(|l| (l, t))) {
            let named = |f: &&FnArgType| labeled(f).is_some_and(|(n, _)| &n == label);
            match ft.args.iter().find(named) {
                Some(f) => self.constrain(env, arg, &f.typ)?,
                None => bail!("no argument #{label}"),
            }
        }
        for (name, optional) in ft.args.iter().filter_map(labeled) {
            if !optional && !args.iter().any(|(l, _)| l.as_ref() == Some(&name)) {
                bail!("missing required argument #{name}")
            }
        }
        self.constrain(env, &ft.rtype, out)
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

    fn fixed(v: &TVar) -> String {
        v.binding().map(|t| t.to_string()).unwrap_or_else(|| "open".into())
    }

    #[test]
    fn a_cell_is_fixed_to_what_flows_into_it() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((join, tj), (one, to)) = (var("j"), var("o"));
        join.set_joins();
        for t in ["i64", "string"] {
            assert!(s.constrain(&env, &parsed(&env, t), &tj).is_ok())
        }
        // one of the values a cell of one type takes may hold the others
        for t in ["i64", "[i64, null]"] {
            assert!(s.constrain(&env, &parsed(&env, t), &to).is_ok())
        }
        assert!(s.finish(&env).is_ok());
        assert_eq!(fixed(&join), "[i64, string]");
        assert_eq!(fixed(&one), "[i64, null]");
    }

    #[test]
    fn a_cell_of_one_type_takes_no_two_unrelated_values() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (_, tt) = var("t");
        for t in ["i64", "string"] {
            assert!(s.constrain(&env, &parsed(&env, t), &tt).is_ok())
        }
        assert!(s.finish(&env).is_err());
        // what another cell passes on is that cell's, not a second value
        let mut s = Solving::default();
        let ((e, te), (p, tp)) = (var("e"), var("p"));
        e.set_joins();
        for t in ["i64", "string"] {
            assert!(s.constrain(&env, &parsed(&env, t), &te).is_ok())
        }
        assert!(s.constrain(&env, &te, &tp).is_ok());
        assert!(s.finish(&env).is_ok());
        assert_eq!(fixed(&p), "[i64, string]");
    }

    #[test]
    fn a_cell_nothing_flows_into_is_what_its_uses_demand() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((a, ta), (b, tb), (c, _)) = (var("a"), var("b"), var("c"));
        assert!(s.constrain(&env, &ta, &parsed(&env, "[i64, string]")).is_ok());
        assert!(s.constrain(&env, &ta, &parsed(&env, "[i64, null]")).is_ok());
        assert!(s.constrain(&env, &tb, &parsed(&env, "Array<i64>")).is_ok());
        s.track(&c);
        assert!(s.finish(&env).is_ok());
        assert_eq!(fixed(&a), "i64");
        assert_eq!(fixed(&b), "Array<i64>");
        assert_eq!(fixed(&c), "_");
    }

    #[test]
    fn each_use_of_a_scheme_has_cells_of_its_own() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let mark = s.mark();
        let (a, ta) = var_at("a", 1);
        s.track(&a);
        let id = over(&env, "fn(x: 'a) -> 'a", &[("a", &ta)]);
        assert!(s.generalize(&env, &id, 0, mark).is_ok());
        assert!(!a.is_bound());
        let ((r0, t0), (r1, t1)) = (var("r"), var("r"));
        for (arg, out) in [("i64", &t0), ("string", &t1)] {
            let call = over(&env, &format!("fn(x: {arg}) -> 'r"), &[("r", out)]);
            let f = s.instantiate(&id, 0, Level::TOP);
            assert!(s.constrain(&env, &f, &call).is_ok())
        }
        assert!(s.finish(&env).is_ok());
        assert_eq!((fixed(&r0), fixed(&r1)), ("i64".into(), "string".into()));
        assert!(!a.is_bound());
    }

    #[test]
    fn what_a_definition_leaves_undecided_its_instance_decides() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let mark = s.mark();
        let ((a, ta), (x, tx)) = (var_at("a", 1), var_at("x", 1));
        // a local of the definition takes its parameter and a string
        s.track(&a);
        assert!(s.constrain(&env, &ta, &tx).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "string"), &tx).is_ok());
        let f = over(&env, "fn(p: 'a) -> i64", &[("a", &ta)]);
        assert!(s.generalize(&env, &f, 0, mark).is_ok());
        assert!(!x.is_bound());
        let ok = s.instantiate(&f, 0, Level::TOP);
        assert!(s.constrain(&env, &ok, &parsed(&env, "fn(p: string) -> i64")).is_ok());
        assert!(s.finish(&env).is_ok());
        let no = s.instantiate(&f, 0, Level::TOP);
        assert!(s.constrain(&env, &no, &parsed(&env, "fn(p: i64) -> i64")).is_ok());
        assert!(s.finish(&env).is_err());
    }

    #[test]
    fn a_held_choice_is_made_when_its_type_is_decided() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((a, ta), (x, tx)) = (var("a"), var("x"));
        let both = over(&env, "[Array<'a>, Array<string>]", &[("a", &ta)]);
        let one = over(&env, "Array<'x>", &[("x", &tx)]);
        assert!(s.constrain(&env, &one, &both).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "i64"), &tx).is_ok());
        assert!(s.finish(&env).is_ok());
        assert_eq!((fixed(&x), fixed(&a)), ("i64".into(), "i64".into()));
    }

    #[test]
    fn a_use_meets_each_value_as_it_arrives() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((_, tv), (out, to)) = (var("v"), var("out"));
        out.set_joins();
        let read = super::Use::Field { field: ArcStr::from("a"), out: to.clone() };
        assert!(s.watch(&env, &tv, read).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "{a: i64, b: string}"), &tv).is_ok());
        assert_eq!(shown(&out.cell_lower()), ["i64"]);
        assert!(!s.attempt(&env, &parsed(&env, "{b: string}"), &tv));
        // a call, of a cell that turns out to be a function
        let ((_, tf), (r, tr)) = (var("f"), var("r"));
        let args = [(None, parsed(&env, "i64"))];
        let call = super::Use::Call { args: args.into_iter().collect(), out: tr };
        assert!(s.watch(&env, &tf, call).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "fn(x: i64) -> string"), &tf).is_ok());
        assert_eq!(shown(&r.cell_lower()), ["string"]);
        assert!(!s.attempt(&env, &parsed(&env, "fn(x: string) -> string"), &tf));
    }

    #[test]
    fn a_use_nothing_reaches_needs_an_annotation() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let mark = s.mark();
        let ((_, tp), (_, to)) = (var_at("p", 1), var_at("o", 1));
        let read = super::Use::Field { field: ArcStr::from("a"), out: to.clone() };
        assert!(s.watch(&env, &tp, read).is_ok());
        let f = over(&env, "fn(p: 'p) -> 'o", &[("p", &tp), ("o", &to)]);
        assert!(s.generalize(&env, &f, 0, mark).is_err());
    }

    #[test]
    fn a_select_gives_each_arm_what_its_pattern_admits() {
        use super::{Arm, Pat, Use};
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((n, tn), (r, tr)) = (var("n"), var("r"));
        n.set_joins();
        r.set_joins();
        let arms = vec![
            Arm {
                pat: Pat::Test(parsed(&env, "i64"), Box::new(Pat::Any(Some(n.clone())))),
                guarded: false,
            },
            Arm { pat: Pat::Any(Some(r.clone())), guarded: false },
        ];
        let id = s.select(arms, ArcStr::from("here"));
        let scrutinee = parsed(&env, "[i64, string, null]");
        assert!(s.watch(&env, &scrutinee, Use::Select(id)).is_ok());
        assert_eq!(shown(&n.cell_lower()), ["i64"]);
        assert_eq!(shown(&r.cell_lower()), ["[null, string]"]);
        assert!(s.finish(&env).is_ok());
        let _ = (tn, tr);
    }

    #[test]
    fn a_select_must_take_every_value_and_have_no_dead_arm() {
        use super::{Arm, Pat, Use};
        let (env, _) = typed(&[], "null");
        let test = |t: &str| Arm {
            pat: Pat::Test(parsed(&env, t), Box::new(Pat::Any(None))),
            guarded: false,
        };
        let mut s = Solving::default();
        let id = s.select(vec![test("i64")], ArcStr::from("here"));
        let scrutinee = parsed(&env, "[i64, string]");
        assert!(s.watch(&env, &scrutinee, Use::Select(id)).is_err());
        let mut s = Solving::default();
        let id = s.select(vec![test("i64"), test("string")], ArcStr::from("here"));
        assert!(s.watch(&env, &parsed(&env, "i64"), Use::Select(id)).is_ok());
        assert!(s.finish(&env).is_err());
        // a test admits what the run time cannot tell from it
        let mut s = Solving::default();
        let (v, _) = var("v");
        v.set_joins();
        let arm = Arm {
            pat: Pat::Test(parsed(&env, "&i64"), Box::new(Pat::Any(Some(v.clone())))),
            guarded: false,
        };
        let id = s.select(vec![arm], ArcStr::from("here"));
        let scrutinee = parsed(&env, "[&i64, &string]");
        assert!(s.watch(&env, &scrutinee, Use::Select(id)).is_ok());
        assert_eq!(shown(&v.cell_lower()), ["&[i64, string]"]);
    }
}
