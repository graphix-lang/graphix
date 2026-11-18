//! Inference by bounds. While a definition is checked no variable is
//! bound: what flows into a variable is a lower bound of its cell, what a
//! use demands of it an upper bound (a constraint), and each new bound is
//! met with the bounds already on the other side, so the bounds are always
//! consistent with each other.

use crate::{
    LambdaId,
    env::Env,
    expr::ModPath,
    stack::ensure_sufficient,
    typ::{
        AbstractId, Bound, ContainsFlags, FnArgKind, FnArgType, FnType, Mutability, Open,
        TVar, Type,
        cast::numbers_ordered,
        node_addr,
        toml_root_refusal,
        setops::strictly_identical,
        tvar::{AtLevel, Level, ShortName},
        variance,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{cell::RefCell, ops::ControlFlow};
use triomphe::Arc;

mod select;
pub use select::{Arm, FALSE, Pat, TRUE, Template, unsplit};

thread_local! {
    /// The solver of the check this thread is running, where the check is
    /// by bounds.
    static ACTIVE: RefCell<Option<Solving>> = const { RefCell::new(None) };
    /// How many cells the old relation has bound since [`begin`]: under a
    /// check by bounds each is a node not yet asking the solver.
    static BOUND_BY_OLD: std::cell::Cell<Option<usize>> = const { std::cell::Cell::new(None) };
}

/// The old relation is binding a cell: counted under a check by bounds,
/// and traced where `GRAPHIX_BOUNDS_AUDIT` asks.
pub(crate) fn old_binding(of: &TVar, to: &Type) {
    if let Some(n) = BOUND_BY_OLD.get() {
        BOUND_BY_OLD.set(Some(n + 1));
        if std::env::var_os("GRAPHIX_BOUNDS_AUDIT").is_some_and(|v| v != "count") {
            eprintln!(
                "bounds audit: the old relation binds '{} to {to}\n{}",
                of.name,
                std::backtrace::Backtrace::force_capture()
            )
        }
    }
}

/// Whether `t` is a cell a check by bounds left open: a scheme's variable.
pub(crate) fn made(t: &Type) -> bool {
    match t {
        // a declared variable is read by its bound, which that relation
        // does
        Type::TVar(tv) if tv.is_rigid() => false,
        Type::TVar(tv) => {
            let cell = tv.cell();
            let c = cell.read();
            c.binding.is_none() && c.solving.is_some()
        }
        _ => false,
    }
}

/// The relation as it was is asked about a scheme's variable, outside any
/// check by bounds: a path not yet the solver's. Traced where
/// `GRAPHIX_BOUNDS_AUDIT` asks.
pub(crate) fn met_outside(a: &Type, b: &Type) {
    if std::env::var_os("GRAPHIX_BOUNDS_AUDIT").is_some_and(|v| v != "count") {
        eprintln!(
            "bounds audit: the old relation is asked about a scheme's variable ({a} over {b})\n{}",
            std::backtrace::Backtrace::force_capture()
        )
    }
}

/// Begin a check by bounds on this thread: until [`end`], what a node
/// asks of two types ([`Type::check_contains`], [`Type::contains`]) is
/// asked of the solver.
pub fn begin() {
    ACTIVE.set(Some(Solving::default()));
    BOUND_BY_OLD.set(Some(0))
}

/// A check by bounds from its input's compile on: one [`begin`]s where
/// none is running, and is [`abandon`]ed when this drops unless [`end`]
/// ended it.
pub struct Check(bool);

impl Check {
    pub fn begin(by_bounds: bool) -> Self {
        let begins = by_bounds && !active();
        if begins {
            begin()
        }
        Check(begins)
    }
}

impl Drop for Check {
    fn drop(&mut self) {
        if self.0 {
            abandon()
        }
    }
}

/// Whether a check by bounds is running on this thread.
pub fn active() -> bool {
    BOUND_BY_OLD.get().is_some()
}

/// End the check [`begin`] began: fix every cell it left open.
pub fn end(env: &Env) -> Result<()> {
    let old = BOUND_BY_OLD.take();
    match ACTIVE.take() {
        Some(mut solving) => {
            if std::env::var_os("GRAPHIX_BOUNDS_AUDIT").is_some() {
                eprintln!(
                    "bounds audit: {} cells met, {} bound by the old relation",
                    solving.cells.len(),
                    old.unwrap_or(0)
                )
            }
            solving.finish(env)
        }
        None => Ok(()),
    }
}

/// Drop the check [`begin`] began, a refusal having ended it.
pub fn abandon() {
    ACTIVE.take();
    BOUND_BY_OLD.take();
}

/// `f` over this thread's solver; none where no check by bounds is
/// running, or where the solver itself is asking, of two decided types.
pub fn with<T>(f: impl FnOnce(&mut Solving) -> T) -> Option<T> {
    let mut solving = ACTIVE.take()?;
    let r = f(&mut solving);
    ACTIVE.set(Some(solving));
    Some(r)
}

/// What the solver keeps of an open cell beside its bounds.
#[derive(Debug, Default)]
pub struct Known {
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
    /// A call made gives a value of this type: an operation that reads
    /// the type to know what to make will read it.
    pub(crate) made: bool,
    /// The check that bounded it has it in its list.
    pub(crate) listed: bool,
    /// The uses waiting on what flows into the cell, and whether each has
    /// met a value.
    pub(crate) uses: SmallVec<[(Use, bool); 1]>,
    /// For a scheme's variable: what its definition left to each
    /// instance, which a use of the scheme takes a copy of with the cell.
    pub(crate) left: SmallVec<[Left; 0]>,
    /// For a cell a labeled parameter's type holds: the label, the type
    /// of the default its definition gives it, and the parameter's type.
    /// A call that omits the label passes the default, and no other
    /// call's parameter takes it.
    pub(crate) defaults: SmallVec<[(ArcStr, Type, Type); 0]>,
}

/// A check a generic definition could not finish, because the types its
/// instances give its variables decide it.
#[derive(Debug, Clone)]
pub(crate) enum Left {
    /// A type below a union more than one of whose members could take
    /// it.
    Held(Arc<(Type, Type)>),
    /// A cell inside the definition that takes more than one value, and
    /// must be of one type in each instance.
    Single(TVar),
    /// A select waiting on a type.
    Waiting(Arc<Template>, Type),
    /// The operands of a comparison, one of whose types must hold the
    /// other's, and whether it is an ordering.
    Compared(Arc<(Type, Type, bool)>),
    /// A type one of the definition's cells was fixed to, the predicates
    /// that cell carries and its name: they hold of what the type is in
    /// each instance.
    Asked(Arc<(Type, SmallVec<[Type; 1]>, ArcStr)>),
}

/// A call's argument: its label, if it is passed by one, and its type.
pub type Arg = (Option<ArcStr>, Type);

/// A use that needs the shape of what it is given: it is applied to each
/// decided type that flows into the cell it waits on, as that type
/// arrives.
#[derive(Debug, Clone)]
pub enum Use {
    /// A read of the field, which goes to `out`.
    Field { field: ArcStr, out: Type, write: bool },
    /// A read of a tuple's element, or of a payload: an error's, or an
    /// abstract type's where its definition is visible from `from`.
    Index { i: usize, out: Type, write: bool, from: ModPath },
    /// A read of an element by its position: an array's element, or a
    /// byte of `bytes`.
    Element { out: Type },
    /// A call with `args`, whose result goes to `out`.
    Call { args: SmallVec<[Arg; 4]>, out: Type },
    /// A struct update: `out` takes the struct with each of `fields` at
    /// its new type.
    With { fields: SmallVec<[(ArcStr, Type); 2]>, out: Type },
    /// The scrutinee of a select ([`Solving::select`]).
    Select(usize),
    /// [`Use::Select`] as a cell keeps it: with the select as written,
    /// which a later copy of the cell is routed down a copy of.
    Routed(usize, Arc<Template>),
    /// The receiver of a call of a method whose type the receiver's
    /// decides (a trait whose `self` is a constructor): `typings` is the
    /// method as each implementation types it, a scheme each; the call
    /// takes the first whose `receiver`th positional parameter takes the
    /// receiver, which is the `at`th of `args`. What the method raises
    /// goes to `throws`, and the typing's cells are made at `level`.
    Method {
        name: ArcStr,
        typings: Arc<[Type]>,
        receiver: usize,
        at: usize,
        args: SmallVec<[Arg; 4]>,
        out: Type,
        throws: Type,
        level: Level,
    },
}

impl Use {
    fn needs(&self) -> &'static str {
        match self {
            Use::Field { .. } => "reading a struct",
            Use::Index { .. } => "reading a tuple",
            Use::Element { .. } => "indexing an array",
            Use::Call { .. } => "reading a fn",
            Use::With { .. } => "updating a struct",
            Use::Select(_) | Use::Routed(..) => "a select",
            Use::Method { .. } => "calling a method",
        }
    }

    /// The types the use holds, each through `f`.
    fn map(&self, f: &mut impl FnMut(&Type) -> Type) -> Use {
        match self {
            Use::Field { field, out, write } => {
                Use::Field { field: field.clone(), out: f(out), write: *write }
            }
            Use::Index { i, out, write, from } => {
                Use::Index { i: *i, out: f(out), write: *write, from: from.clone() }
            }
            Use::Element { out } => Use::Element { out: f(out) },
            Use::Call { args, out } => Use::Call {
                args: args.iter().map(|(l, t)| (l.clone(), f(t))).collect(),
                out: f(out),
            },
            Use::With { fields, out } => Use::With {
                fields: fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
                out: f(out),
            },
            Use::Select(id) => Use::Select(*id),
            Use::Routed(id, of) => Use::Routed(*id, of.clone()),
            Use::Method { name, typings, receiver, at, args, out, throws, level } => {
                Use::Method {
                    name: name.clone(),
                    typings: typings.clone(),
                    receiver: *receiver,
                    at: *at,
                    args: args.iter().map(|(l, t)| (l.clone(), f(t))).collect(),
                    out: f(out),
                    throws: f(throws),
                    level: *level,
                }
            }
        }
    }

    fn each(&self, f: &mut impl FnMut(&Type)) {
        match self {
            Use::Field { out, .. } | Use::Index { out, .. } | Use::Element { out } => {
                f(out)
            }
            Use::Call { args, out } => {
                f(out);
                args.iter().for_each(|(_, t)| f(t))
            }
            Use::With { fields, out } => {
                f(out);
                fields.iter().for_each(|(_, t)| f(t))
            }
            Use::Select(_) | Use::Routed(..) => (),
            Use::Method { args, out, throws, .. } => {
                f(out);
                f(throws);
                args.iter().for_each(|(_, t)| f(t))
            }
        }
    }
}

/// The relation's own state for one check.
#[derive(Default)]
pub struct Solving {
    /// The pairs holding a typedef's name that are being related: a pair
    /// met again inside itself holds.
    unfolding: Vec<(Type, Type)>,
    /// The pairs related to their end inside the outermost of those: one
    /// met again has added every bound it adds.
    unfolded: Vec<(Type, Type)>,
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
    /// The operand and the result of each `?` or `$` whose operand is not
    /// decided yet; the third is where the payloads of the errors a `?`
    /// raises go.
    strips: Vec<(Type, Type, Option<Type>)>,
    /// A held choice is being made: a decided member that does not fit is
    /// out.
    resolving: bool,
    /// The choice is tried for a value whose parts are still open.
    placing: bool,
    /// The scheme's cells the use being made has copied, whose
    /// [`Known::left`] it has still to take.
    copied: Vec<TVar>,
    /// The copies being made are a scheme's too ([`specialize`]): each is
    /// a variable of the copy, and keeps what the original left to its
    /// instances.
    scheming: bool,
    /// The pairs of typedef names a select's walk is inside.
    walks: std::cell::RefCell<select::Walks>,
    /// The twin each cell copied out has at a level, per polarity
    /// ([`Self::extrude`]), with the cell, which it keeps alive; and the
    /// ones made since the outermost attempt began, in order.
    twins: AHashMap<(usize, bool, u32), (TVar, Level, TVar)>,
    twinned: Vec<(usize, bool, u32)>,
    /// The declared variables being met are a signature others read
    /// ([`fits`]): nothing it does not say holds of them.
    sealed: bool,
    /// The cells fixed that carry a predicate, which is judged of what
    /// the cell holds once every cell is fixed ([`Self::judge`]).
    judged: Vec<TVar>,
    /// The operands of each comparison, and whether it is an ordering,
    /// judged with them ([`Self::compare`]).
    compared: Vec<(Type, Type, bool)>,
    /// A flow left out of a cell's fixing because its source was being
    /// fixed around it: the source and the cell, checked once both are
    /// fixed ([`Self::fix`]).
    cycles: Vec<(TVar, TVar)>,
    /// What a definition asked of a type its variables decide, which
    /// this instance of it judges: the type, the predicates and the name
    /// of the variable they bound.
    asked: Vec<Arc<(Type, SmallVec<[Type; 1]>, ArcStr)>>,
}

/// A kind of use a check may still owe when a definition closes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Pending {
    Held,
    Select,
    Strip,
}

#[derive(Clone, Copy)]
enum Added {
    Lower,
    Upper,
    Feeder,
    Sink,
}

/// The union of `ts` as they stand: flat, each member once, the primitives
/// as one member. No two cells are made one, whatever the members share.
pub fn union_of(ts: impl IntoIterator<Item = Type>) -> Type {
    Type::flatten_set(ts)
}

/// The union of the decided types `ts`, less each one another holds: a
/// name and what it stands for are one type.
fn join(env: &Env, ts: &[Type]) -> Result<Type> {
    let mut kept: SmallVec<[&Type; 4]> = SmallVec::new();
    for t in ts.iter() {
        let mut held = false;
        for k in kept.iter().filter(|k| !k.has_free() && !t.has_free()) {
            held |= k.contains(env, t)?
        }
        if !held {
            let mut holds: SmallVec<[bool; 4]> = SmallVec::new();
            for k in kept.iter() {
                holds.push(!k.has_free() && !t.has_free() && t.contains(env, k)?)
            }
            let mut holds = holds.into_iter();
            kept.retain(|_| !holds.next().unwrap_or(false));
            kept.push(t)
        }
    }
    Ok(union_of(kept.into_iter().cloned()))
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

/// Add `t` to the lower bounds of the cell of `v`, or to the upper ones,
/// unless it is there; the bounds on the cell's other side where it was
/// added. The bound may hold the cell, so nothing is compared under the
/// cell's lock.
fn record(v: &TVar, t: &Type, lower: bool) -> Option<SmallVec<[Type; 1]>> {
    let cell = v.cell();
    let had = {
        let c = cell.read();
        if lower { c.lower.clone() } else { c.constraints.clone() }
    };
    if had.iter().any(|b| strictly_identical(b, t)) {
        return None;
    }
    let mut c = cell.write();
    Some(match lower {
        true => {
            c.lower.push(t.clone());
            c.constraints.clone()
        }
        false => {
            c.constraints.push(t.clone());
            c.lower.clone()
        }
    })
}

/// `f` over what the solver keeps of the cell of `v`.
fn known<R>(v: &TVar, f: impl FnOnce(&mut Known) -> R) -> R {
    let cell = v.cell();
    let mut c = cell.write();
    f(c.solving.get_or_insert_default())
}

impl Solving {
    /// `a` fits where `b` is wanted. An open cell on either side takes the
    /// other as a bound, and the bound meets every bound the cell has on
    /// its other side.
    pub fn constrain(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        self.relate(env, a, b)
    }

    /// [`Self::constrain`] as a question: where `a` does not fit, every
    /// bound the try added is taken back and nothing is changed.
    pub fn attempt(&mut self, env: &Env, a: &Type, b: &Type) -> bool {
        self.attempting(|s| s.constrain(env, a, b))
    }

    /// Whether `a` would fit where `b` is wanted, asked without leaving a
    /// bound behind either way.
    pub fn probe(&mut self, env: &Env, a: &Type, b: &Type) -> bool {
        self.trying(false, |s| s.constrain(env, a, b))
    }

    /// Whether `f` succeeds; where it does not, every bound it added is
    /// taken back.
    fn attempting(&mut self, f: impl FnOnce(&mut Self) -> Result<()>) -> bool {
        self.trying(true, f)
    }

    /// Whether `f` succeeds. What it added is taken back where it fails,
    /// and where it succeeds too unless `keep`.
    fn trying(&mut self, keep: bool, f: impl FnOnce(&mut Self) -> Result<()>) -> bool {
        let (mark, held) = (self.added.len(), self.held.len());
        let (unfolded, twinned) = (self.unfolded.len(), self.twinned.len());
        self.attempts += 1;
        let fits = f(self).is_ok();
        self.attempts -= 1;
        if !(fits && keep) {
            self.held.truncate(held);
            self.unfolded.truncate(unfolded);
            for key in self.twinned.drain(twinned..) {
                self.twins.remove(&key);
            }
            for (v, what) in self.added.drain(mark..).rev() {
                let cell = v.cell();
                let mut c = cell.write();
                match (what, c.solving.as_mut()) {
                    (Added::Lower, _) => drop(c.lower.pop()),
                    (Added::Upper, _) => drop(c.constraints.pop()),
                    (Added::Feeder, Some(k)) => drop(k.feeders.pop()),
                    (Added::Sink, Some(k)) => drop(k.sinks.pop()),
                    (_, None) => (),
                }
            }
        }
        if self.attempts == 0 {
            self.added.clear();
            self.twinned.clear()
        }
        fits
    }

    fn note(&mut self, v: &TVar, what: Added) {
        if self.attempts > 0 {
            self.added.push((v.clone(), what))
        }
    }

    fn relate(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        ensure_sufficient(|| self.relate_in(env, a, b))
    }

    fn relate_in(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        // an application whose constructor is decided is the type it makes
        let applied = |t: Type| match &t {
            Type::App(c, x) => Type::app_filled(c, x).unwrap_or(t),
            _ => t,
        };
        let (a, b) = (applied(shallow(a)), applied(shallow(b)));
        let (a, b) = (flat(env, a)?, flat(env, b)?);
        if std::env::var_os("GRAPHIX_BOUNDS_DEBUG").is_some() {
            eprintln!("relate {a} <= {b}")
        }
        match (&a, &b) {
            (Type::Bottom, _) | (_, Type::Any) => return Ok(()),
            // a declared variable holds a predicate its signature says it
            // does; one that is one type at each call is all its own
            // definition may leave unsaid, and an interface's may leave
            // none
            (Type::TVar(v), Type::Bound(p))
                if v.is_rigid() && (self.sealed || *p != Bound::Singleton) =>
            {
                for own in v.cell_constraints().iter() {
                    if numbers_ordered(|| own.implies(env, &b))? {
                        return Ok(());
                    }
                }
                let name = v.declared_name().unwrap_or_else(|| v.name.clone());
                let name = ShortName(&name);
                match self.sealed {
                    true => bail!(
                        "the implementation requires '{name}: {b}, which the signature \
                         does not declare"
                    ),
                    false => bail!(
                        "the body needs '{name} within {b}, which the signature does \
                         not declare: add {b} to the bound of '{name}"
                    ),
                }
            }
            // a signature others read says what holds of a type its
            // variables are part of, whatever they stand for: the relation
            // over decided types reads each by its declared bound
            (_, Type::Bound(_))
                if self.sealed && !matches!(a, Type::TVar(_)) && a.has_rigid() =>
            {
                // a comparison reaches the values a variable stands for
                // inside data: each such variable is held to the
                // predicate
                if let Type::Bound(Bound::Ordered | Bound::Discernible) = &b {
                    let mut reached = SmallVec::new();
                    compared(env, &a, &mut reached, &mut SmallVec::new());
                    for v in reached.iter() {
                        if !numbers_ordered(|| v.conjunct_implies(env, &b))? {
                            let name = v.declared_name().unwrap_or_else(|| v.name.clone());
                            bail!(
                                "the implementation requires '{}: {b}, which the \
                                 signature does not declare",
                                ShortName(&name)
                            )
                        }
                    }
                }
                let flags = ContainsFlags::RigidCheck | ContainsFlags::Sealed;
                return match numbers_ordered(|| b.contains_with_flags(flags, env, &a))? {
                    true => Ok(()),
                    false => bail!(
                        "the implementation requires {a} to be {b}, which the \
                         signature's bounds do not say"
                    ),
                };
            }
            // a predicate on a cell is judged when the cell is decided
            (_, Type::Bound(_)) => return Ok(()),
            (Type::TVar(v), Type::TVar(w)) if v.same_cell(w) => return Ok(()),
            // with no cell on either side, open or declared, this is the
            // relation over decided types. A declared variable is not
            // handed over: there it could take a bound nobody declared.
            _ if !a.has_unbound() && !b.has_unbound() => {
                return match b.contains(env, &a)? {
                    true => Ok(()),
                    false => mismatch(&b, &a),
                };
            }
            _ => (),
        }
        if open(&a).is_some() || open(&b).is_some() {
            return self.cells(env, &a, &b);
        }
        match (&a, &b) {
            // what no decided member of `b` takes goes to its open member
            // as one type
            (Type::Set(xs), Type::Set(ys))
                if ys.iter().filter(|m| open(&shallow(m)).is_some()).count() == 1 =>
            {
                let ys: SmallVec<[Type; 4]> = ys.iter().map(shallow).collect();
                // a typedef's name is met as what it stands for
                let stood = |t: &Type| match t {
                    Type::Ref(_) => t.lookup_ref(env).unwrap_or_else(|_| t.clone()),
                    t => t.clone(),
                };
                let shapes: SmallVec<[Type; 4]> =
                    ys.iter().filter(|m| open(m).is_none()).map(stood).collect();
                let decided = |x: &Type| shapes.iter().any(|m| alike(m, &stood(x)));
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
            // whether a type has an implementation is asked of the whole
            // type, once it is decided; a declared variable in it is a
            // constant to the question
            (_, Type::Ref(_)) if b.is_trait_ref(env) => match a.has_free() {
                true => Ok(self.held.push((a.clone(), b.clone(), false))),
                false => match b.contains(env, &a)? {
                    true => Ok(()),
                    false => bail!("{a} does not implement {b}"),
                },
            },
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
            // two applications of one constructor nothing has decided:
            // how it holds its argument is not known, so the arguments
            // are one type
            (Type::App(c, x), Type::App(d, y)) => {
                self.equal(env, c, d)?;
                self.equal(env, x, y)
            }
            // an application of a constructor not decided yet, against a
            // type that is one: the type's constructor, and the argument
            // as that constructor holds it (an array's element may be
            // narrower, a cell written through may not)
            (_, Type::App(c, y)) => match Type::app_split(&a, env)? {
                Some((ctor, arg)) => {
                    self.constrain(env, &ctor, c)?;
                    match ctor.fill_hole(y) {
                        Some(filled) => self.constrain(env, &a, &filled),
                        None => self.equal(env, &arg, y),
                    }
                }
                None => mismatch(&b, &a),
            },
            (Type::App(c, x), _) => match Type::app_split(&b, env)? {
                Some((ctor, arg)) => {
                    self.constrain(env, c, &ctor)?;
                    match ctor.fill_hole(x) {
                        Some(filled) => self.constrain(env, &filled, &b),
                        None => self.equal(env, x, &arg),
                    }
                }
                None => mismatch(&b, &a),
            },
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
    fn cells(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        let (va, vb) = (open(a), open(b));
        let at = va.or(vb).unwrap().level();
        match (va, vb) {
            (Some(v), _) if depth(b) <= at.depth() => {
                let Some(lower) = record(v, b, false) else { return Ok(()) };
                self.note(v, Added::Upper);
                self.track(v);
                if let Some(w) = vb {
                    if w.level().depth() == at.depth() {
                        known(w, |k| k.feeders.push(v.clone()));
                        self.note(w, Added::Feeder)
                    }
                } else {
                    self.demanded(env, v, b)?
                }
                lower.iter().try_for_each(|l| self.relate(env, l, b))
            }
            (Some(_), Some(w)) => self.arrive(env, w, a),
            (Some(_), None) => {
                let b = self.extrude(b, false, at);
                self.escapes(&b, at)?;
                self.constrain(env, a, &b)
            }
            (None, Some(w)) if depth(a) <= at.depth() => self.arrive(env, w, a),
            (None, Some(_)) => {
                let a = self.extrude(a, true, at);
                self.escapes(&a, at)?;
                self.constrain(env, &a, b)
            }
            (None, None) => unreachable!(),
        }
    }

    /// `a` is a new lower bound of the open cell `w`: it must fit whatever
    /// is demanded of `w`.
    fn arrive(&mut self, env: &Env, w: &TVar, a: &Type) -> Result<()> {
        let Some(upper) = record(w, a, true) else { return Ok(()) };
        self.note(w, Added::Lower);
        self.track(w);
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
        upper.iter().try_for_each(|u| self.relate(env, a, u))
    }

    /// A copy of `t` no deeper than `at`: each deeper cell gets a twin at
    /// `at` per polarity, joined to it by a bound, and has that one twin
    /// there however often it is copied out. A declared variable has a
    /// twin only where its values flow out.
    fn extrude(&mut self, t: &Type, pos: bool, at: Level) -> Type {
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
                let key = (v.cell_addr(), pos, at.depth());
                if let Some((_, level, n)) = self.twins.get(&key)
                    && *level == at
                {
                    return Type::TVar(n.clone());
                }
                let keep = |s: &mut Self, n: &TVar| {
                    s.twins.insert(key, (v.clone(), at, n.clone()));
                    s.twinned.push(key)
                };
                match (v.is_rigid(), pos) {
                    (true, false) => t.clone(),
                    (true, true) => {
                        if std::env::var_os("GRAPHIX_BOUNDS_DEBUG").is_some() {
                            eprintln!(
                                "sink of '{} (depth {}) at depth {}\n{}",
                                v.name,
                                v.level().depth(),
                                at.depth(),
                                std::backtrace::Backtrace::force_capture()
                            )
                        }
                        let n = twin(v);
                        keep(self, &n);
                        known(v, |k| k.sinks.push(n.clone()));
                        self.note(v, Added::Sink);
                        Type::TVar(n)
                    }
                    (false, _) => {
                        let n = twin(v);
                        keep(self, &n);
                        let (lower, upper) = (v.cell_lower(), v.cell_constraints());
                        let cell = v.cell();
                        match pos {
                            true => {
                                cell.write().constraints.push(Type::TVar(n.clone()));
                                self.note(v, Added::Upper);
                                let lower: SmallVec<[Type; 1]> = lower
                                    .iter()
                                    .map(|l| self.extrude(l, pos, at))
                                    .collect();
                                n.cell().write().lower = lower
                            }
                            false => {
                                cell.write().lower.push(Type::TVar(n.clone()));
                                self.note(v, Added::Lower);
                                let upper: SmallVec<[Type; 1]> = upper
                                    .iter()
                                    .map(|u| self.extrude(u, pos, at))
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
                    typ: self.extrude(&a.typ, !pos, at),
                });
                let args = Arc::from_iter(args);
                Type::Fn(Arc::new(FnType {
                    args,
                    vargs: ft.vargs.as_ref().map(|t| self.extrude(t, !pos, at)),
                    rtype: self.extrude(&ft.rtype, pos, at),
                    throws: self.extrude(&ft.throws, pos, at),
                    explicit_throws: ft.explicit_throws,
                    quantifiers: ft.quantifiers.clone(),
                    lambda_ids: ft.lambda_ids.clone(),
                }))
            }
            other => other
                .cow_children(&mut |c| Some(self.extrude(c, pos, at)))
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

    /// The decided `a` goes to the one of `cells`, open members of the
    /// union `b`, whose bounds admit it; among two or more it waits.
    fn among(&mut self, env: &Env, a: &Type, b: &Type, cells: &[&Type]) -> Result<()> {
        let admit: SmallVec<[&Type; 2]> =
            cells.iter().copied().filter(|m| self.probe(env, a, m)).collect();
        match &admit[..] {
            [m] => self.constrain(env, a, m),
            [] => mismatch(b, a),
            _ => Ok(self.held.push((a.clone(), b.clone(), false))),
        }
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
                (false, _) if self.resolving => {
                    self.among(env, &Type::Primitive(rest), b, &cells)
                }
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
            _ if self.resolving && (self.placing || !a.has_free()) => {
                let (decided, loose): (SmallVec<[&Type; 2]>, SmallVec<[&Type; 2]>) =
                    takers.iter().copied().partition(|m| !m.has_free());
                // a value with open parts goes to every decided member
                // that takes it, together: no order among them decides it
                if a.has_free() {
                    let fits: SmallVec<[&Type; 2]> =
                        decided.iter().copied().filter(|m| self.probe(env, a, m)).collect();
                    if !fits.is_empty() {
                        return fits.iter().try_for_each(|m| self.constrain(env, a, m));
                    }
                } else if decided.iter().any(|m| self.attempt(env, a, m)) {
                    return Ok(());
                }
                // a member written in the value's shape takes it before a
                // bare variable does
                match &loose[..] {
                    [m] if self.attempt(env, a, m) => Ok(()),
                    [] | [_] => self.among(env, a, b, &cells),
                    _ => Ok(self.held.push((a.clone(), b.clone(), false))),
                }
            }
            _ => Ok(self.held.push((a.clone(), b.clone(), false))),
        }
    }

    /// A pair with a typedef's name on a side, by what the name stands for.
    fn unfolded(&mut self, env: &Env, a: &Type, b: &Type) -> Result<()> {
        // two applications of one definition are related by their
        // arguments, each as the definition's body meets its parameter,
        // where the body leaves that no choice
        if let (Type::Ref(ra), Type::Ref(rb)) = (a, b)
            && ra.same_definition(rb, env)
            && ra.params.len() == rb.params.len()
            && let Some(meets) =
                ra.resolve_peek(env).and_then(|def| variance::meets(env, &def))
        {
            let pairs = || ra.params.iter().zip(rb.params.iter()).zip(meets.iter());
            if pairs().all(|((x, y), m)| !m.walk() || strictly_identical(x, y)) {
                for ((x, y), m) in pairs().filter(|(_, m)| !m.walk()) {
                    if m.holds() || m.holds_swapped() {
                        self.constrain(env, x, y)?
                    }
                    if m.held() || m.held_swapped() {
                        self.constrain(env, y, x)?
                    }
                }
                return Ok(());
            }
        }
        let met =
            |(x, y): &(Type, Type)| strictly_identical(x, a) && strictly_identical(y, b);
        if self.unfolding.iter().any(met) || self.unfolded.iter().any(met) {
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
        match self.unfolding.is_empty() {
            true => self.unfolded.clear(),
            false if r.is_ok() => self.unfolded.push((a.clone(), b.clone())),
            false => (),
        }
        // a refusal names the types as they were written
        r.map_err(|e| match e.to_string().starts_with("type mismatch") {
            true => anyhow::anyhow!("type mismatch {b} does not contain {a}"),
            false => e,
        })
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
        // a label of `f` that `g` hides or leaves optional is left out by
        // a call through `g`. Where `g` declares variables for it, each
        // call picks them, and the call that leaves the label out is held
        // to the default: `g` carries it as `f` does.
        for (name, optional, fa) in fl.iter().filter(|(_, optional, _)| *optional) {
            let _ = optional;
            match gl.iter().find(|(n, ..)| n == name) {
                Some((_, false, _)) => (),
                Some((_, true, ga)) if let Some(v) = declared_in(ga).first() => {
                    carry(name, fa, v, ga)
                }
                Some((_, true, _)) | None => self.omitted(env, name, fa)?,
            }
        }
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

/// What `?` and `$` leave of the decided type `t`: `t` less its errors,
/// or, where it holds none, less null. With it, the payloads of the errors
/// taken off, which a `?` raises (`raises`) and so must know.
pub fn strip(env: &Env, t: &Type, raises: bool) -> Result<(Type, SmallVec<[Type; 2]>)> {
    let mut members: SmallVec<[Type; 4]> = SmallVec::new();
    members_of(env, t, &mut members)?;
    if raises && members.iter().any(|m| matches!(m, Type::Any)) {
        bail!(
            "the operand of ? has type {t}, so the error it raises is not known: cast \
             it to a type whose error is (a Result), or use $"
        )
    }
    Ok(strip_members(members))
}

/// The members of `t`, a name that stands for a union, an error or a
/// primitive read as what it stands for, each name once.
fn members_of(env: &Env, t: &Type, out: &mut SmallVec<[Type; 4]>) -> Result<()> {
    fn go(
        env: &Env,
        t: &Type,
        out: &mut SmallVec<[Type; 4]>,
        seen: &mut AHashSet<Type>,
    ) -> Result<()> {
        ensure_sufficient(|| match &shallow(t) {
            Type::Set(ms) => ms.iter().try_for_each(|m| go(env, m, out, seen)),
            named @ Type::Ref(_) if !seen.insert(named.clone()) => Ok(()),
            named @ Type::Ref(_) => match named.lookup_ref(env)? {
                body @ Type::Set(_) => go(env, &body, out, seen),
                body @ (Type::Error(_) | Type::Primitive(_)) => Ok(out.push(body)),
                _ => Ok(out.push(named.clone())),
            },
            t => Ok(out.push(t.clone())),
        })
    }
    let mut seen: LPooled<AHashSet<Type>> = LPooled::take();
    go(env, t, out, &mut seen)
}

/// The declared variables of `t` that a comparison of two of its values
/// reaches: those not behind a function, a reference or an abstract type,
/// which compare by identity.
fn compared(
    env: &Env,
    t: &Type,
    out: &mut SmallVec<[TVar; 4]>,
    seen: &mut SmallVec<[Type; 4]>,
) {
    ensure_sufficient(|| match &shallow(t) {
        Type::TVar(v) if v.is_rigid() => {
            if !out.iter().any(|u| u.same_cell(v)) {
                out.push(v.clone())
            }
        }
        Type::Fn(_) | Type::ByRef(..) | Type::Abstract { .. } => (),
        named @ Type::Ref(_) => {
            if !seen.iter().any(|s| strictly_identical(s, named)) {
                seen.push(named.clone());
                if let Ok(body) = named.lookup_ref(env) {
                    compared(env, &body, out, seen)
                }
            }
        }
        t => {
            let _ = t.try_for_each_child::<()>(&mut |c| {
                compared(env, c, out, seen);
                ControlFlow::Continue(())
            });
        }
    })
}

/// Whether an operation reads the type `v` stands for to know what to do:
/// it is bounded `Concrete` or `Function`.
fn reads_type(v: &TVar) -> bool {
    v.cell_constraints()
        .iter()
        .any(|c| matches!(c, Type::Bound(Bound::Concrete | Bound::Function)))
}

fn infinite() -> anyhow::Error {
    anyhow::anyhow!(
        "infinite type: a value here would hold a value of its own type (a function \
         passed or returning itself, a collection holding itself); declare a named \
         recursive type and annotate the binding"
    )
}

/// A cell is of a finite type: nothing that flows into it holds the cell
/// inside a type, directly or through the cells that flow in.
fn finite(v: &TVar) -> Result<()> {
    let mut seen: LPooled<AHashSet<(usize, bool)>> = LPooled::take();
    let flows = |u: &TVar, inside: bool| {
        let fed = feeders(u).into_iter().map(Type::TVar);
        u.cell_lower().into_iter().chain(fed).map(move |l| (l, inside))
    };
    let mut todo: SmallVec<[(Type, bool); 8]> = flows(v, false).collect();
    while let Some((t, inside)) = todo.pop() {
        match &shallow(&t) {
            Type::TVar(u) if u.same_cell(v) && inside => return Err(infinite()),
            Type::TVar(u) if u.same_cell(v) => (),
            Type::TVar(u) => {
                if seen.insert((u.cell_addr(), inside)) {
                    todo.extend(flows(u, inside))
                }
            }
            t => {
                let mut cells = SmallVec::new();
                cells_of(t, true, &mut cells);
                todo.extend(cells.into_iter().map(|u| (Type::TVar(u), true)))
            }
        }
    }
    Ok(())
}

/// Record that the labeled parameter `label`, of the type `param`, which
/// holds the cell `v`, has a default of the type `default`: each call
/// that leaves the label out passes it ([`Known::defaults`]).
pub fn defaulted(v: &TVar, label: ArcStr, default: Type, param: Type) {
    known(v, |k| k.defaults.push((label, default, param)))
}

/// The default a function gives the label `name`, whose parameter is
/// `from`, is the default of a function type that keeps the label
/// optional over its own variable `v`, in the parameter `to`.
fn carry(name: &ArcStr, from: &Type, v: &TVar, to: &Type) {
    for c in cells_in(from).iter() {
        for (label, default, _) in known(c, |k| k.defaults.clone()) {
            let carried = known(v, |k| k.defaults.iter().any(|(l, ..)| l == name));
            if &label == name && !carried {
                defaulted(v, label, deep(&default), to.clone())
            }
        }
    }
}

/// The cells `t` holds that nothing has decided, declared variables
/// among them.
pub fn cells_in(t: &Type) -> SmallVec<[TVar; 4]> {
    let mut cells = SmallVec::new();
    cells_of(t, true, &mut cells);
    cells
}

/// The declared variables `t` holds.
fn declared_in(t: &Type) -> SmallVec<[TVar; 4]> {
    let mut cells = cells_in(t);
    cells.retain(|v: &mut TVar| v.is_rigid());
    cells
}

/// Whether `t` is a scheme a check by bounds made: one of its cells is a
/// scheme's variable there.
pub fn scheme(t: &Type) -> bool {
    let mut cells = SmallVec::new();
    cells_of(t, true, &mut cells);
    cells.iter().any(quantified)
}

/// Print `what` to stderr where `GRAPHIX_BOUNDS_DEBUG` is set.
pub fn debug(what: impl FnOnce() -> String) {
    if std::env::var_os("GRAPHIX_BOUNDS_DEBUG").is_some() {
        eprintln!("{}", what())
    }
}

/// The types of the instance of the scheme `def` that is used as `at`, a
/// decided type: a copy of each of the scheme's cells, fixed, under the
/// address of the cell it is the copy of. An instance is built from its
/// definition's check through this, in whatever task builds it.
pub fn instance(env: &Env, def: &Type, at: &Type) -> Result<AHashMap<usize, TVar>> {
    let mut solving = Solving::default();
    let mut copies = AHashMap::default();
    let one = solving.instantiate_into(
        def,
        Level::GENERIC.depth() - 1,
        Level::TOP,
        &mut copies,
    );
    solving.constrain(env, &one, at)?;
    solving.finish(env)?;
    Ok(copies)
}

/// Whether what a binding holds, whose type its check left as `made`, is
/// the `declared` type for whatever each of the declaration's variables
/// stands for: one instance of `made` fits the declaration with its
/// variables held as constants. `outer` is the level the declaration is
/// read at. Asked of the solver the check is running, where there is one,
/// since a cell of `made` the check has not decided takes the bound.
pub fn fits(env: &Env, made: &Type, declared: &Type, outer: Level) -> Result<()> {
    let fits = |s: &mut Solving| {
        let mark = s.mark();
        s.quantify(made, outer.depth());
        let level = Level::Def { depth: outer.depth() + 1, owner: LambdaId::new() };
        let _at = AtLevel::enter(level);
        let held = declared.reset_tvars();
        // the declaration's variables, those only its bounds name among
        // them
        let mut constants: SmallVec<[TVar; 4]> = SmallVec::new();
        cells_of(&held, true, &mut constants);
        let mut next = 0;
        while let Some(v) = constants.get(next).cloned() {
            for bound in v.cell_constraints().iter() {
                let mut named = SmallVec::new();
                cells_of(bound, true, &mut named);
                for u in named {
                    if !constants.iter().any(|k| k.same_cell(&u)) {
                        constants.push(u)
                    }
                }
            }
            next += 1
        }
        constants.iter().for_each(|v| v.claim(level));
        let _held: SmallVec<[_; 4]> = constants.iter().map(|v| v.open_rigid()).collect();
        let one = s.instantiate(made, Level::GENERIC.depth() - 1, level);
        let sealed = std::mem::replace(&mut s.sealed, true);
        let fit = s.constrain(env, &one, &held);
        s.sealed = sealed;
        fit?;
        // a default the declaration keeps optional over its own variables
        // is passed by its clients' calls: the declaration carries it
        let own = cells_in(declared);
        let at: AHashMap<ArcStr, Type> =
            own.iter().map(|v| (v.name.clone(), Type::TVar(v.clone()))).collect();
        for c in constants.iter() {
            let Some(v) = own.iter().find(|v| v.name == c.name) else { continue };
            for (label, default, param) in known(c, |k| k.defaults.clone()) {
                let carried = known(v, |k| k.defaults.iter().any(|(l, ..)| *l == label));
                if !carried {
                    defaulted(v, label, deep(&default), param.replace_tvars(&at))
                }
            }
        }
        // the instance is decided here, where the declaration's
        // variables are still constants
        s.generalize(env, Type::BOTTOM, outer.depth(), mark)
    };
    match with(fits) {
        Some(r) => r,
        None => fits(&mut Solving::default()),
    }
}

/// Whether a value of the type `written`, as general as it is declared,
/// may stand where readers take the scheme `held` at its instances: one
/// instance of `written` is `held` for whatever each of `held`'s
/// variables stands for. A variable of `held` is a cell nothing flows
/// into, a constant here; its other cells are what flows into them.
pub fn written_fits(env: &Env, written: &Type, held: &Type, outer: Level) -> Result<()> {
    let fits = |s: &mut Solving| {
        let mark = s.mark();
        let level = Level::Def { depth: outer.depth() + 1, owner: LambdaId::new() };
        let _at = AtLevel::enter(level);
        let one = s.instantiate(held, Level::GENERIC.depth() - 1, level);
        let mut cells = Cells::new();
        s.reach(&one, &mut cells);
        let (flowed, constants): (SmallVec<[TVar; 4]>, SmallVec<[TVar; 4]>) =
            cells.iter().cloned().partition(|v| decided(v, &mut SmallVec::new()));
        constants.iter().for_each(|v| v.claim(level));
        let _held: SmallVec<[_; 4]> = constants.iter().map(|v| v.open_rigid()).collect();
        flowed.iter().try_for_each(|v| s.fix(env, v))?;
        let sealed = std::mem::replace(&mut s.sealed, true);
        let fit = s.constrain(env, &written.reset_tvars(), &deep(&one));
        s.sealed = sealed;
        fit?;
        s.generalize(env, Type::BOTTOM, outer.depth(), mark)
    };
    match with(fits) {
        Some(r) => r,
        None => fits(&mut Solving::default()),
    }
}

/// A type of a generic definition's check as one instance of the
/// definition has it: each of the definition's cells is the instance's
/// (`known`, by the cell's address), and a scheme the type holds beside
/// (a generic definition inside the first) is copied as a scheme, over the
/// instance's cells, the copies added to `known`.
pub fn specialize(t: &Type, known: &mut AHashMap<usize, TVar>) -> Type {
    let mut solving = Solving { scheming: true, ..Solving::default() };
    let t = solving.copy(t, Level::GENERIC.depth() - 1, Level::GENERIC, known);
    deep(&t)
}

/// Give each open cell of `own`, a type an instance compiled, the part of
/// `row` at its place, where `row` is that type as the definition's check
/// left it and the instance reads it ([`specialize`]). False where an
/// open cell of `own` finds no part.
pub fn take(own: &Type, row: &Type) -> bool {
    let all = |xs: &[Type], ys: &[Type]| {
        xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| take(x, y))
    };
    ensure_sufficient(|| match (own, row) {
        (Type::TVar(v), row) => match v.binding() {
            Some(b) => take(&b, row),
            None => {
                if !matches!(row, Type::TVar(r) if r.same_cell(v)) {
                    v.cell().write().set_binding(Some(row.clone()))
                }
                true
            }
        },
        (own, Type::TVar(r)) => match r.binding() {
            Some(b) => take(own, &b),
            None => !own.has_unbound(),
        },
        (Type::Fn(f), Type::Fn(g)) => {
            f.args.len() == g.args.len()
                && f.args.iter().zip(g.args.iter()).all(|(a, b)| take(&a.typ, &b.typ))
                && match (&f.vargs, &g.vargs) {
                    (Some(a), Some(b)) => take(a, b),
                    (None, None) => true,
                    _ => false,
                }
                && take(&f.rtype, &g.rtype)
                && take(&f.throws, &g.throws)
        }
        (Type::Array(x), Type::Array(y))
        | (Type::List(x), Type::List(y))
        | (Type::Error(x), Type::Error(y))
        | (Type::ByRef(_, x), Type::ByRef(_, y)) => take(x, y),
        (Type::Map(m), Type::Map(n)) => take(&m.key, &n.key) && take(&m.value, &n.value),
        (Type::Tuple(xs), Type::Tuple(ys))
        | (Type::Set(xs), Type::Set(ys))
        | (Type::Variant(_, xs, _), Type::Variant(_, ys, _))
        | (Type::Abstract { params: xs, .. }, Type::Abstract { params: ys, .. }) => {
            all(xs, ys)
        }
        (Type::Ref(a), Type::Ref(b)) => all(&a.params, &b.params),
        (Type::Struct(xs), Type::Struct(ys)) => {
            xs.len() == ys.len()
                && xs.iter().zip(ys.iter()).all(|((_, x, _), (_, y, _))| take(x, y))
        }
        (own, _) => !own.has_unbound(),
    })
}

/// A method of a trait whose `self` is a constructor, as the
/// implementation for `target` (a type with a hole) types it: each
/// `self<X>` of the declared `sig` is `target` with `X` in its hole.
pub fn method_typing(sig: &Type, target: &Type) -> Type {
    fn at(t: &Type, target: &Type) -> Option<Type> {
        ensure_sufficient(|| match t {
            Type::App(head, arg) if matches!(&**head, Type::TVar(v) if &*v.name == "self") => {
                let arg = at(arg, target).unwrap_or_else(|| (**arg).clone());
                target.fill_hole(&arg)
            }
            t => t.cow_children(&mut |c| at(c, target)),
        })
    }
    at(sig, target).unwrap_or_else(|| sig.clone())
}

/// What is demanded of a cell, `t`, less the members a predicate among
/// the cell's `bounds` rules out: a value of `Result<'a, 'e>` demanded as
/// `[i64, Error<'e>]` is an `i64` there, since `'a` holds no error.
fn within(env: &Env, t: Type, bounds: &[Type]) -> Result<Type> {
    let predicates = || bounds.iter().filter(|b| matches!(b, Type::Bound(_)));
    if predicates().next().is_none() {
        return Ok(t);
    }
    let mut members: SmallVec<[Type; 4]> = SmallVec::new();
    members_of(env, &t, &mut members)?;
    // a union of primitives is one member: each of them is judged
    let members: SmallVec<[Type; 4]> = members
        .into_iter()
        .flat_map(|m| match &m {
            Type::Primitive(p) if p.len() > 1 => {
                p.iter().map(|bit| Type::Primitive(bit.into())).collect()
            }
            _ => SmallVec::<[Type; 4]>::from_iter([m]),
        })
        .collect();
    if members.len() < 2 {
        return Ok(t);
    }
    let mut kept: SmallVec<[Type; 4]> = SmallVec::new();
    for m in members.iter() {
        let mut holds = true;
        for b in predicates() {
            holds &= m.has_unbound() || b.contains(env, m)?
        }
        if holds {
            kept.push(m.clone())
        }
    }
    Ok(match kept.len() {
        0 => t,
        n if n == members.len() => t,
        _ => union_of(kept),
    })
}

/// The union `t` with each member that names a union replaced by that
/// union's members: a type is taken by one member of a union, whatever
/// names the union was written with.
fn flat(env: &Env, t: Type) -> Result<Type> {
    fn names_union(env: &Env, m: &Type) -> Result<Option<Type>> {
        Ok(match &shallow(m) {
            named @ Type::Ref(_) if !named.is_trait_ref(env) => {
                // a union of primitives alone is one primitive type
                Some(named.lookup_ref(env)?)
                    .filter(|body| matches!(body, Type::Set(_) | Type::Primitive(_)))
            }
            _ => None,
        })
    }
    fn members(env: &Env, t: &Type, out: &mut SmallVec<[Type; 4]>) -> Result<()> {
        ensure_sufficient(|| match &shallow(t) {
            Type::Set(ms) => ms.iter().try_for_each(|m| members(env, m, out)),
            m => match names_union(env, m)? {
                Some(body) => members(env, &body, out),
                None => Ok(out.push(m.clone())),
            },
        })
    }
    let Type::Set(ms) = &t else { return Ok(t) };
    let mut named = false;
    for m in ms.iter() {
        named |= names_union(env, m)?.is_some()
    }
    if !named {
        return Ok(t);
    }
    let mut out: SmallVec<[Type; 4]> = SmallVec::new();
    members(env, &t, &mut out)?;
    Ok(union_of(out))
}

/// Whether `?` or `$` can tell here what it takes off `t`: `t` holds an
/// error, which is what goes, or nothing in it is undecided.
pub fn strippable(env: &Env, t: &Type) -> Result<bool> {
    let mut members: SmallVec<[Type; 4]> = SmallVec::new();
    members_of(env, t, &mut members)?;
    Ok(match &members[..] {
        [Type::TVar(_)] => false,
        [_] => true,
        ms => ms.iter().any(|m| matches!(m, Type::Error(_))) || !t.has_unbound(),
    })
}

/// Whether `?` and `$` take the errors off `t`, a type they can tell
/// that of ([`strippable`]): it holds one. They take the null otherwise.
pub fn strips_errors(env: &Env, t: &Type) -> Result<bool> {
    let mut members: SmallVec<[Type; 4]> = SmallVec::new();
    members_of(env, t, &mut members)?;
    Ok(holds_errors(&members))
}

fn holds_errors(members: &[Type]) -> bool {
    members.iter().any(|m| {
        matches!(m, Type::Error(_))
            || matches!(m, Type::Primitive(p) if p.contains(Typ::Error))
    })
}

fn strip_members(members: SmallVec<[Type; 4]>) -> (Type, SmallVec<[Type; 2]>) {
    let errors = holds_errors(&members);
    let gone = if errors { Typ::Error } else { Typ::Null };
    let payloads = members.iter().filter_map(|m| match m {
        Type::Error(p) => Some((**p).clone()),
        _ => None,
    });
    let payloads = payloads.collect();
    let left = union_of(members.into_iter().filter_map(|m| match m {
        Type::Error(_) if errors => None,
        Type::Primitive(p) => {
            Some(Type::Primitive(p & !gone)).filter(|_| !(p & !gone).is_empty())
        }
        m => Some(m),
    }));
    (left, payloads)
}

/// What a value of the abstract type `id` at `params` holds, where the
/// type is made in Graphix.
fn rep(env: &Env, id: &AbstractId, params: &[Type]) -> Option<Type> {
    let made = env.abstract_reps.get(id)?;
    let at: AHashMap<ArcStr, Type> = made
        .params
        .iter()
        .zip(params.iter())
        .map(|((formal, _), p)| (formal.name.clone(), p.clone()))
        .collect();
    Some(made.rep.replace_tvars(&at))
}

/// `t` with every bound cell replaced by what it is bound to.
fn deep(t: &Type) -> Type {
    map_cells(t, &mut |_| None)
}

/// `t` with each open cell `f` answers for replaced, bound cells looked
/// through.
fn map_cells(t: &Type, f: &mut impl FnMut(&TVar) -> Option<Type>) -> Type {
    // a part many places share is mapped once, and one nothing changes
    // in stays shared: `f` gives a cell the same answer wherever it is
    fn go(
        t: &Type,
        f: &mut impl FnMut(&TVar) -> Option<Type>,
        mapped: &mut AHashMap<usize, Option<Type>>,
    ) -> Option<Type> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => {
                    let addr = tv.cell_addr();
                    if let Some(r) = mapped.get(&addr) {
                        return r.clone();
                    }
                    let r = Some(go(&b, f, mapped).unwrap_or(b));
                    mapped.insert(addr, r.clone());
                    r
                }
                None => f(tv),
            },
            t => {
                let addr = node_addr(t);
                if let Some(r) = addr.and_then(|a| mapped.get(&a)) {
                    return r.clone();
                }
                let r = t.cow_children(&mut |c| go(c, f, mapped));
                if let Some(a) = addr {
                    mapped.insert(a, r.clone());
                }
                r
            }
        })
    }
    let mut mapped: LPooled<AHashMap<usize, Option<Type>>> = LPooled::take();
    go(t, f, &mut mapped).unwrap_or_else(|| t.clone())
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
                None if (rigid || !tv.is_rigid()) && seen.insert(tv.cell_addr()) => {
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

/// What [`Solving::held_back`] found: the cells it met, and those among
/// them that may not be fixed yet, by address.
struct HeldBack {
    met: LPooled<AHashSet<usize>>,
    back: LPooled<AHashSet<usize>>,
}

/// Cells, each once, in the order met.
struct Cells {
    met: LPooled<Vec<TVar>>,
    addrs: LPooled<AHashSet<usize>>,
}

impl Cells {
    fn new() -> Self {
        Cells { met: LPooled::take(), addrs: LPooled::take() }
    }

    /// Add `v`; false if it was there.
    fn insert(&mut self, v: &TVar) -> bool {
        let new = self.addrs.insert(v.cell_addr());
        if new {
            self.met.push(v.clone())
        }
        new
    }

    fn has(&self, v: &TVar) -> bool {
        self.addrs.contains(&v.cell_addr())
    }

    fn iter(&self) -> std::slice::Iter<'_, TVar> {
        self.met.iter()
    }
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
    /// Every open cell reachable from `t` through bounds. A declared
    /// variable inside its definition is a constant: it is not one, and
    /// nothing is reached through it.
    fn reach(&self, t: &Type, seen: &mut Cells) {
        let mut found = SmallVec::new();
        cells_of(t, false, &mut found);
        for v in found {
            if seen.insert(&v) {
                let mut next = SmallVec::new();
                self.next(&v, &mut next);
                next.iter().for_each(|u| self.reach(&Type::TVar(u.clone()), seen))
            }
        }
    }

    /// What the cell `v` reaches in one step: the open cells of its
    /// bounds, the cells that feed it, and those its uses produce.
    fn next(&self, v: &TVar, out: &mut SmallVec<[TVar; 4]>) {
        let (lower, upper) = (v.cell_lower(), v.cell_constraints());
        lower.iter().chain(upper.iter()).for_each(|b| cells_of(b, false, out));
        out.extend(feeders(v).into_iter().filter(|u| !u.is_bound() && !u.is_rigid()));
        for (u, _) in uses(v).iter() {
            u.each(&mut |t| cells_of(t, false, out));
            if let Use::Routed(_, of) = u {
                let mut binds = SmallVec::new();
                of.cells(&mut binds);
                out.extend(binds.into_iter().filter(|b| !b.is_bound()))
            }
        }
    }

    /// Of the cells reachable from `from`, those a definition closing at
    /// `min` may not fix: each that reaches a cell shallower than `min` or
    /// a scheme's variable.
    fn held_back(&self, from: impl IntoIterator<Item = TVar>, min: u32) -> HeldBack {
        let stops = |v: &TVar| v.level().depth() < min || quantified(v);
        let mut held = HeldBack { met: LPooled::take(), back: LPooled::take() };
        let mut under: LPooled<AHashMap<usize, SmallVec<[usize; 2]>>> = LPooled::take();
        let mut work: LPooled<Vec<TVar>> = LPooled::take();
        for v in from {
            if !v.is_bound() && !v.is_rigid() && held.met.insert(v.cell_addr()) {
                work.push(v)
            }
        }
        while let Some(v) = work.pop() {
            if stops(&v) {
                held.back.insert(v.cell_addr());
                continue;
            }
            let mut next = SmallVec::new();
            self.next(&v, &mut next);
            for u in next {
                under.entry(u.cell_addr()).or_default().push(v.cell_addr());
                if held.met.insert(u.cell_addr()) {
                    work.push(u)
                }
            }
        }
        let mut work: LPooled<Vec<usize>> = held.back.iter().copied().collect();
        while let Some(stop) = work.pop() {
            for v in under.get(&stop).into_iter().flatten() {
                if held.back.insert(*v) {
                    work.push(*v)
                }
            }
        }
        held
    }

    /// Whether `v` may be fixed by a definition closing at `min`: nothing
    /// it reaches is shallower or a scheme's variable.
    fn fixable(&self, held: &HeldBack, v: &TVar, min: u32) -> bool {
        match held.met.contains(&v.cell_addr()) {
            true => !held.back.contains(&v.cell_addr()),
            false => {
                let mut all = Cells::new();
                self.reach(&Type::TVar(v.clone()), &mut all);
                all.iter().all(|u| u.level().depth() >= min && !quantified(u))
            }
        }
    }

    /// Mark the variables of a declared scheme `t` as its own: those
    /// deeper than `depth`. They leave the level of the definition that
    /// made them ([`Level::GENERIC`]), so a use of the scheme copies them
    /// wherever it stands.
    pub fn quantify(&mut self, t: &Type, depth: u32) {
        let mut kept = Cells::new();
        self.reach(t, &mut kept);
        for v in kept.iter().filter(|v| v.level().depth() > depth) {
            known(v, |k| k.quantified = true);
            v.generalize()
        }
    }

    /// The declared variable `v` is its scheme's from here on: its
    /// definition has closed, so it is no constant any more.
    pub fn own(&mut self, v: &TVar) {
        if !v.is_bound() {
            known(v, |k| k.quantified = true);
            v.generalize()
        }
    }

    /// What is asked of the scheme's variable `v` can be met: some value
    /// is of every type it is used as. One that cannot is a parameter no
    /// call can pass.
    fn meetable(&self, env: &Env, v: &TVar) -> Result<()> {
        let asked: SmallVec<[Type; 4]> = v
            .cell_constraints()
            .iter()
            .map(deep)
            // nothing is asked of a cell wanted as nothing but that
            // nothing flows into it, which its flows are held to
            .filter(|u| {
                !matches!(u, Type::TVar(_) | Type::Bound(_) | Type::Bottom)
                    && !u.is_trait_ref(env)
            })
            .collect();
        let mut common = Type::Any;
        for (i, u) in asked.iter().enumerate() {
            // as a test meets them: two function types, two references
            // or two abstract types may share values no walk can list
            common = self.both(env, true, &common, &select::split(u));
            if matches!(common, Type::Bottom) {
                bail!(
                    "a value here is used as {} and as {u}, and no value is both: \
                     nothing can be passed for it",
                    asked[..i]
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<SmallVec<[String; 2]>>()
                        .join(" and as ")
                )
            }
        }
        Ok(())
    }

    /// A use of `v` that met no value: it needs a shape nothing gave it.
    fn unread(&self, v: &TVar) -> Result<()> {
        let waits = |u: &Use| {
            !matches!(
                u,
                Use::Select(_)
                    | Use::Routed(..)
                    | Use::Method { .. }
                    | Use::Element { .. }
            )
        };
        match uses(v).iter().find(|(u, met)| !met && waits(u)) {
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
        // a default is made anew by each call that leaves its label out:
        // what its type leaves open is each instance's
        for v in cells_in(t).iter() {
            for (_, default, _) in known(v, |k| k.defaults.clone()).iter() {
                self.quantify(default, depth)
            }
        }
        let mut kept = Cells::new();
        self.reach(t, &mut kept);
        for v in kept.iter().filter(|v| v.level().depth() > depth && !v.is_bound()) {
            finite(v)?;
            self.meetable(env, v)?
        }
        self.fix_scope(env, depth + 1, false, mark)?;
        for v in self.cells[mark..].iter() {
            let pending = v.level().depth() > depth
                && !v.is_bound()
                && !v.is_rigid()
                && !quantified(v)
                && v.cell_constraints().iter().any(|c| matches!(c, Type::Bound(_)));
            if pending && !v.level().is_generic() {
                self.singles.push(v.clone());
                v.generalize();
                // each instance whose variables it reaches checks a copy
                let mut reached = Cells::new();
                self.reach(&Type::TVar(v.clone()), &mut reached);
                for u in reached.iter().filter(|u| !u.same_cell(v)) {
                    // what stands between it and those variables is the
                    // instance's too: the copy reads no cell of the
                    // definition
                    if u.level().depth() > depth
                        && !u.is_bound()
                        && !u.is_rigid()
                        && !quantified(u)
                        && !u.level().is_generic()
                    {
                        u.generalize()
                    }
                    self.leave(&Type::TVar(u.clone()), |ls| {
                        let same = |l: &Left| matches!(l, Left::Single(w) if w.same_cell(v));
                        (!ls.iter().any(same)).then(|| Left::Single(v.clone()))
                    })
                }
            }
        }
        // a predicate of a type a variable of the scheme is part of is
        // each instance's to judge
        let (left, mine): (Vec<_>, Vec<_>) = std::mem::take(&mut self.judged)
            .into_iter()
            .partition(|v| v.binding().is_some_and(|t| scheme(&t)));
        self.judged = mine;
        for v in left {
            let Some(t) = v.binding() else { continue };
            let predicates: SmallVec<[Type; 1]> = v
                .cell_constraints()
                .into_iter()
                .filter(|c| matches!(c, Type::Bound(_)))
                .collect();
            let asked = Arc::new((t, predicates, v.name.clone()));
            self.leave(&asked.0, |ls| {
                let same = |l: &Left| matches!(l, Left::Asked(a) if Arc::ptr_eq(a, &asked));
                (!ls.iter().any(same)).then(|| Left::Asked(asked.clone()))
            })
        }
        // a comparison whose operands a variable of the scheme decides is
        // each instance's to judge
        let (left, mine): (Vec<_>, Vec<_>) = std::mem::take(&mut self.compared)
            .into_iter()
            .partition(|(a, b, _)| scheme(a) || scheme(b));
        self.compared = mine;
        for pair in left {
            let pair = Arc::new(pair);
            for t in [&pair.0, &pair.1] {
                self.leave(t, |ls| {
                    let same = |l: &Left| matches!(l, Left::Compared(p) if Arc::ptr_eq(p, &pair));
                    (!ls.iter().any(same)).then(|| Left::Compared(pair.clone()))
                })
            }
        }
        // a select still waiting waits for an instance's types
        for id in 0..self.selects.len() {
            let of = self.selects[id].template.clone();
            for w in self.selects[id].waiting.clone() {
                self.leave(&w, |ls| {
                    let same = |l: &Left| match l {
                        Left::Waiting(t, u) => {
                            Arc::ptr_eq(t, &of) && strictly_identical(u, &w)
                        }
                        _ => false,
                    };
                    (!ls.iter().any(same)).then(|| Left::Waiting(of.clone(), w.clone()))
                })
            }
        }
        Ok(())
    }

    /// Leave a check to the instances of the scheme whose variables `t`
    /// holds: each of those variables keeps what `left` gives for it.
    fn leave(&self, t: &Type, left: impl Fn(&[Left]) -> Option<Left>) {
        let mut cells = SmallVec::new();
        cells_of(t, true, &mut cells);
        for v in cells.iter().filter(|v| v.is_rigid() || v.level().is_generic()) {
            known(v, |k| {
                if let Some(l) = left(&k.left) {
                    k.left.push(l)
                }
            })
        }
    }

    /// Fix every cell an input left open.
    pub fn finish(&mut self, env: &Env) -> Result<()> {
        self.fix_scope(env, 0, true, 0)?;
        self.judge(env)?;
        self.dead_arms(env)
    }

    /// A use of the scheme `t`: a copy of it, at the level `at`, with a
    /// cell of its own for each of the scheme's (those deeper than
    /// `above`), bounded as the scheme's are.
    pub fn instantiate(&mut self, t: &Type, above: u32, at: Level) -> Type {
        self.instantiate_into(t, above, at, &mut LPooled::take())
    }

    /// [`Self::instantiate`], with the instance's cells in `copies`, each
    /// under the address of the scheme's cell it is the copy of: what
    /// reads the definition's types at this instance maps them through it.
    pub fn instantiate_into(
        &mut self,
        t: &Type,
        above: u32,
        at: Level,
        copies: &mut AHashMap<usize, TVar>,
    ) -> Type {
        let outer = std::mem::take(&mut self.copied);
        let t = self.copy(t, above, at, copies);
        // what the definition left to its instances, each once
        let mut done: SmallVec<[Left; 4]> = SmallVec::new();
        while let Some(v) = self.copied.pop() {
            let left = v.cell().read().solving.as_ref().map(|k| k.left.clone());
            for l in left.into_iter().flatten() {
                let seen = |d: &Left| match (d, &l) {
                    (Left::Held(p), Left::Held(q)) => Arc::ptr_eq(p, q),
                    (Left::Compared(p), Left::Compared(q)) => Arc::ptr_eq(p, q),
                    (Left::Asked(p), Left::Asked(q)) => Arc::ptr_eq(p, q),
                    (Left::Single(u), Left::Single(w)) => u.same_cell(w),
                    (Left::Waiting(s, u), Left::Waiting(t, w)) => {
                        Arc::ptr_eq(s, t) && strictly_identical(u, w)
                    }
                    _ => false,
                };
                if done.iter().any(seen) {
                    continue;
                }
                match &l {
                    Left::Held(pair) => {
                        let a = self.copy(&pair.0, above, at, copies);
                        let b = self.copy(&pair.1, above, at, copies);
                        self.held.push((a, b, false))
                    }
                    Left::Compared(pair) => {
                        let a = self.copy(&pair.0, above, at, copies);
                        let b = self.copy(&pair.1, above, at, copies);
                        self.compared.push((a, b, pair.2))
                    }
                    Left::Asked(asked) => {
                        let t = self.copy(&asked.0, above, at, copies);
                        self.asked.push(Arc::new((t, asked.1.clone(), asked.2.clone())))
                    }
                    Left::Single(u) => {
                        self.copy(&Type::TVar(u.clone()), above, at, copies);
                    }
                    // a select that waited on one of the scheme's
                    // variables is the instance's to finish
                    Left::Waiting(of, w) => {
                        let new = self
                            .select_copy(of, &mut |s, v| s.copy_cell(v, above, at, copies));
                        let w = self.copy(w, above, at, copies);
                        self.select_wait(new, w)
                    }
                }
                done.push(l)
            }
        }
        self.copied = outer;
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
        ensure_sufficient(|| self.copy_in(t, above, at, copies))
    }

    fn copy_in(
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
            self.copied.push(v.clone());
            Some(Type::TVar(n))
        });
        // the copy is bounded and fed as the original is
        for (v, n) in made {
            let (lower, mut upper) = (v.cell_lower(), v.cell_constraints());
            let (fed, sinks, used, defaults) = {
                let cell = v.cell();
                let c = cell.read();
                match c.solving.as_ref() {
                    Some(k) => (
                        k.feeders.clone(),
                        k.sinks.clone(),
                        k.uses.clone(),
                        k.defaults.clone(),
                    ),
                    None => Default::default(),
                }
            };
            let defaults: SmallVec<[(ArcStr, Type, Type); 0]> = defaults
                .iter()
                .map(|(label, default, param)| {
                    let default = self.copy(default, above, at, copies);
                    (label.clone(), default, self.copy(param, above, at, copies))
                })
                .collect();
            upper.extend(sinks.into_iter().map(Type::TVar));
            let all = |s: &mut Self, ts: &[Type], copies: &mut AHashMap<usize, TVar>| {
                ts.iter().map(|t| s.copy(t, above, at, copies)).collect()
            };
            let (lower, upper): (SmallVec<[Type; 1]>, SmallVec<[Type; 1]>) =
                (all(self, &lower, copies), all(self, &upper, copies));
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
                k.feeders = feeds;
                k.defaults = defaults
            }
            // the copy is used as the original is: what flows into it in
            // this instance meets the same reads and calls
            for (u, met) in used.iter() {
                let u = match u {
                    Use::Routed(_, of) if self.scheming => {
                        let of = self.template_copy(of, &mut |s, v| {
                            s.copy_cell(v, above, at, copies)
                        });
                        Use::Routed(usize::MAX, of)
                    }
                    Use::Routed(_, of) => {
                        let new = self.select_copy(of, &mut |s, v| {
                            s.copy_cell(v, above, at, copies)
                        });
                        Use::Routed(new, self.selects[new].template.clone())
                    }
                    u => u.map(&mut |t| self.copy(t, above, at, copies)),
                };
                let u = match u {
                    Use::Method {
                        name,
                        typings,
                        receiver,
                        at: nth,
                        args,
                        out,
                        throws,
                        ..
                    } => Use::Method {
                        name,
                        typings,
                        receiver,
                        at: nth,
                        args,
                        out,
                        throws,
                        level: at,
                    },
                    u => u,
                };
                let met = *met
                    && !matches!(u, Use::Select(_) | Use::Routed(..) | Use::Method { .. });
                known(&n, |k| k.uses.push((u, met)))
            }
            if !self.scheming {
                self.track(&n);
                continue;
            }
            let left = v.cell().read().solving.as_ref().map(|k| k.left.clone());
            for l in left.into_iter().flatten() {
                let l = match l {
                    Left::Held(pair) => Left::Held(Arc::new((
                        self.copy(&pair.0, above, at, copies),
                        self.copy(&pair.1, above, at, copies),
                    ))),
                    Left::Compared(pair) => Left::Compared(Arc::new((
                        self.copy(&pair.0, above, at, copies),
                        self.copy(&pair.1, above, at, copies),
                        pair.2,
                    ))),
                    Left::Asked(asked) => Left::Asked(Arc::new((
                        self.copy(&asked.0, above, at, copies),
                        asked.1.clone(),
                        asked.2.clone(),
                    ))),
                    Left::Single(u) => Left::Single(self.copy_cell(&u, above, at, copies)),
                    Left::Waiting(of, w) => {
                        let of = self.template_copy(&of, &mut |s, v| {
                            s.copy_cell(v, above, at, copies)
                        });
                        Left::Waiting(of, self.copy(&w, above, at, copies))
                    }
                };
                known(&n, |k| k.left.push(l))
            }
            known(&n, |k| k.quantified = true);
            n.generalize()
        }
        t
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
            let held = {
                let own = self.cells[mark..].iter().cloned();
                let pending = self.held.iter().flat_map(|(a, b, _)| [a, b]);
                let pending =
                    pending.chain(self.selects.iter().flat_map(|s| s.waiting.iter()));
                let pending =
                    pending.chain(self.strips.iter().map(|(operand, ..)| operand));
                let pending: SmallVec<[TVar; 4]> = pending.flat_map(open_cells).collect();
                self.held_back(own.chain(pending), min)
            };
            // a pending use (a held choice, a select still waiting) waits
            // for any other whose result reaches what it reads: its kind
            // and index, and everything its result reaches
            let mut produced: Vec<((Pending, usize), Cells)> = vec![];
            if ordered {
                let closure = |s: &Self, outs: SmallVec<[TVar; 4]>| {
                    let mut all = Cells::new();
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
                for (i, (_, out, raised)) in self.strips.iter().enumerate() {
                    let mut outs = open_cells(out);
                    raised.iter().for_each(|p| outs.extend(open_cells(p)));
                    produced.push(((Pending::Strip, i), closure(self, outs)))
                }
            }
            let free = |key: (Pending, usize), ins: &[TVar]| {
                !produced.iter().any(|(k, c)| *k != key && ins.iter().any(|v| c.has(v)))
            };
            for i in (0..self.held.len()).rev() {
                let (a, b, scheme) = self.held[i].clone();
                if scheme {
                    continue;
                }
                let (avs, bvs) = (open_cells(&a), open_cells(&b));
                let ready =
                    avs.iter().chain(bvs.iter()).all(|v| self.fixable(&held, v, min));
                if ready && free((Pending::Held, i), &avs) {
                    // a member something has decided is decided here; one
                    // nothing has stays open, to take what the others do
                    // not
                    self.held.swap_remove(i);
                    for v in bvs.iter() {
                        if decided(v, &mut SmallVec::new()) {
                            self.fix(env, v)?
                        }
                    }
                    // a part something says the type of, from below or
                    // above, is fixed; a value whose other parts are open
                    // goes to a decided member that takes it as it stands,
                    // and the member decides them; with none they are
                    // fixed too
                    let said = |v: &TVar| {
                        decided(v, &mut SmallVec::new())
                            || v.cell_constraints().iter().any(|u| {
                                let u = deep(u);
                                !matches!(u, Type::TVar(_) | Type::Bound(_))
                                    && !u.is_trait_ref(env)
                            })
                    };
                    for v in avs.iter().filter(|v| said(v)) {
                        self.fix(env, v)?
                    }
                    let (a, b) = (deep(&a), deep(&b));
                    (self.resolving, self.placing) = (true, a.has_free());
                    let parts = open_cells(&a);
                    let placed = self.placing
                        && self.attempting(|s| {
                            let (n, mark) = (s.held.len(), s.added.len());
                            s.constrain(env, &a, &b)?;
                            // members give an open part values; two of
                            // them never decide one from above two ways
                            let from_above = parts.iter().any(|u| {
                                let added = s.added[mark..].iter().filter(|(v, what)| {
                                    matches!(what, Added::Upper) && u.same_cell(v)
                                });
                                let uppers = u.cell_constraints();
                                let uppers = &uppers[uppers.len() - added.count()..];
                                !decided(u, &mut SmallVec::new())
                                    && uppers.iter().any(|t| !strictly_identical(t, &uppers[0]))
                            });
                            match s.held.len() == n && !from_above {
                                true => Ok(()),
                                false => bail!("no member takes it as it stands"),
                            }
                        });
                    (self.resolving, self.placing) = (false, false);
                    avs.iter().try_for_each(|v| self.fix(env, v))?;
                    if placed {
                        progress = true;
                        continue;
                    }
                    let n = self.held.len();
                    self.resolving = true;
                    let r = self.constrain(env, &deep(&a), &b);
                    self.resolving = false;
                    r?;
                    progress |= self.held.len() == n
                } else if avs.iter().chain(bvs.iter()).any(quantified) {
                    self.held[i].2 = true;
                    let pair = Type::Tuple(Arc::from_iter([a.clone(), b.clone()]));
                    self.leave(&pair, |ls| {
                        let same = |l: &Left| match l {
                            Left::Held(p) => {
                                strictly_identical(&p.0, &a) && strictly_identical(&p.1, &b)
                            }
                            _ => false,
                        };
                        (!ls.iter().any(same))
                            .then(|| Left::Held(Arc::new((a.clone(), b.clone()))))
                    })
                }
            }
            let ready = |s: &Self, id: usize, t: &Type| {
                let ins = open_cells(t);
                ins.iter().all(|v| s.fixable(&held, v, min))
                    && free((Pending::Select, id), &ins)
            };
            progress |= self.select_waiting(env, &ready)?;
            for i in (0..self.strips.len()).rev() {
                let (operand, out, raised) = self.strips[i].clone();
                let ins = open_cells(&operand);
                if ins.iter().all(|v| self.fixable(&held, v, min))
                    && free((Pending::Strip, i), &ins)
                {
                    self.strips.swap_remove(i);
                    // the operand's own shape says what goes: what its
                    // parts are is decided only where the shape does not
                    ins.iter().try_for_each(|v| self.fix(env, v))?;
                    if !strippable(env, &deep(&operand))? {
                        self.fix_in(env, &operand)?
                    }
                    let (left, payloads) =
                        strip(env, &deep(&operand), raised.is_some())?;
                    self.constrain(env, &left, &out)?;
                    for to in raised.iter() {
                        payloads.iter().try_for_each(|p| self.constrain(env, p, to))?
                    }
                    progress = true
                } else if ins.iter().any(quantified) {
                    bail!(
                        "the type of the operand of ? or $ is not known here: annotate it"
                    )
                }
            }
        }
        let held = self.held_back(self.cells[mark..].iter().cloned(), min);
        // a cell something flows into first: one nothing flows into may be
        // decided by a cell that holds it
        let (flowing, empty): (Vec<TVar>, Vec<TVar>) = self.cells[mark..]
            .iter()
            .cloned()
            .partition(|v| decided(v, &mut SmallVec::new()));
        // what a choice still held will decide is not known yet: it, and
        // whatever it reaches, waits with the choice
        let mut waiting = Cells::new();
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
            for (_, out, raised) in self.strips.iter() {
                for v in
                    open_cells(out).into_iter().chain(raised.iter().flat_map(open_cells))
                {
                    self.reach(&Type::TVar(v), &mut waiting)
                }
            }
        }
        for v in flowing.into_iter().chain(empty) {
            let mine = !v.is_bound()
                && !v.is_rigid()
                && !quantified(&v)
                && v.level().depth() >= min;
            let now = last || (self.fixable(&held, &v, min) && !waiting.has(&v));
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

    /// Fix every cell of `t`, those nothing has decided included.
    fn fix_in(&mut self, env: &Env, t: &Type) -> Result<()> {
        loop {
            let cells = open_cells(t);
            if cells.is_empty() {
                return Ok(());
            }
            cells.iter().try_for_each(|v| self.fix(env, v))?
        }
    }

    /// Hold a `?` or `$` whose operand is not decided yet: `out` takes
    /// what it leaves of the operand once that is, and `raised` the
    /// payloads of the errors a `?` takes off.
    pub fn hold_strip(&mut self, operand: Type, out: Type, raised: Option<Type>) {
        self.strips.push((operand, out, raised))
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
        // a flow from a cell that was being fixed around this one was
        // left out: what that cell is now fits here, or no finite type
        // holds both
        if self.fixing.is_empty() {
            for (from, to) in std::mem::take(&mut self.cycles) {
                let (from, to) = (Type::TVar(from), Type::TVar(to));
                self.constrain(env, &deep(&from), &deep(&to)).map_err(|_| infinite())?
            }
        }
        if v.cell_constraints().iter().any(|c| matches!(c, Type::Bound(_))) {
            self.judged.push(v.clone())
        }
        Ok(())
    }

    /// Each predicate a fixed cell carries holds of what the cell was
    /// fixed to.
    fn judge(&mut self, env: &Env) -> Result<()> {
        for v in std::mem::take(&mut self.judged) {
            let Some(t) = v.binding().map(|t| deep(&t)) else { continue };
            // an operation that reads a type to know what to make (a parse,
            // a typed read) needs one where its value is made, or where
            // something was asked of the type: nothing decided this
            let asked = |v: &TVar| {
                v.cell_constraints().iter().any(|c| !matches!(c, Type::Bound(_)))
            };
            if matches!(t, Type::Bottom) && (known(&v, |k| k.made) || asked(&v)) {
                for p in v.cell_constraints().iter() {
                    match p {
                        Type::Bound(Bound::Concrete) => bail!(
                            "the type '{} must be known here, an operation reads it to \
                             know what to make: annotate where its result goes",
                            ShortName(&v.name)
                        ),
                        Type::Bound(Bound::Function) => bail!(
                            "the type '{} must be a function here, an operation reads \
                             its signature: annotate it",
                            ShortName(&v.name)
                        ),
                        _ => (),
                    }
                }
            }
            self.hold(env, &v.name, &v.cell_constraints(), &t)?
        }
        for asked in std::mem::take(&mut self.asked) {
            let (t, predicates, name) = &*asked;
            self.hold(env, name, predicates, &deep(t))?
        }
        let number = Type::Primitive(Typ::number());
        for (a, b, ordered) in std::mem::take(&mut self.compared) {
            let (a, b) = (deep(&a), deep(&b));
            // numbers of two types have an order, and are never equal
            if ordered && number.contains(env, &a)? && number.contains(env, &b)? {
                continue;
            }
            if !a.contains(env, &b)? && !b.contains(env, &a)? {
                match ordered {
                    true => bail!(
                        "cannot order {a} against {b}: neither type holds the \
                         other, so the answer never depends on the values"
                    ),
                    false => bail!(
                        "cannot compare {a} with {b}: neither type holds the \
                         other, so the two are never equal"
                    ),
                }
            }
        }
        Ok(())
    }

    /// Each predicate among `bounds` holds of the decided type `t`, which
    /// the variable `name` stands for. Nothing is of every kind, and a
    /// variable is of the kinds its own bound says, which the relation
    /// held it to.
    fn hold(&self, env: &Env, name: &ArcStr, bounds: &[Type], t: &Type) -> Result<()> {
        if matches!(t, Type::Bottom | Type::TVar(_)) {
            return Ok(());
        }
        let named = |what: &Type| match name.starts_with('_') {
            true => format_compact!("{what}"),
            false => format_compact!("'{name}: {what}"),
        };
        for p in bounds.iter() {
            match p {
                Type::Bound(Bound::Ordered | Bound::Discernible) => {
                    let ordered = matches!(p, Type::Bound(Bound::Ordered));
                    let why =
                        numbers_ordered(|| t.indiscernible(env, Open::Benign, ordered));
                    if let Some(why) = why {
                        return Err(t.not_discernible(p, name, &why));
                    }
                }
                Type::Bound(Bound::Data) if !p.contains(env, t)? => {
                    return Err(t.not_data(env, &[], [t, t]).unwrap_or_else(|| t.no_data()));
                }
                Type::Bound(b @ (Bound::JsonData | Bound::TomlData)) => {
                    let format = match b {
                        Bound::JsonData => "JSON",
                        _ => "TOML",
                    };
                    if let Some(why) = t.text_refusal(env) {
                        let (what, why) = why.told(format, t);
                        bail!("{} refuses {what}: {why}", named(p))
                    }
                    let written = bounds.contains(&Type::Bound(Bound::Data));
                    if *b == Bound::TomlData
                        && written
                        && let Some(m) = t.toml_root_pairs(env)
                    {
                        let why = toml_root_refusal(Some(&m));
                        bail!("{} refuses {t}: {why}", named(p))
                    }
                }
                Type::Bound(Bound::Concrete) => {
                    let why = numbers_ordered(|| t.map_key_failure(env, Open::Benign));
                    if let Some(why) = why {
                        return Err(why.as_map_key());
                    }
                }
                Type::Bound(_) if !p.contains(env, t)? => bail!("{t} must be {p} here"),
                _ => (),
            }
        }
        Ok(())
    }

    /// The operands of a comparison: one's type holds the other's, or,
    /// for an ordering, both are numbers; judged once both are decided, by
    /// each instance where a generic definition's variables decide them.
    pub fn compare(&mut self, a: Type, b: Type, ordered: bool) {
        self.compared.push((a, b, ordered))
    }

    fn fixed(&mut self, env: &Env, v: &TVar) -> Result<Type> {
        // a cell nothing has decided may stay open inside a bound: a
        // holder that shares it can still decide it
        let mut flows: SmallVec<[Type; 4]> = SmallVec::new();
        let fed = feeders(v).into_iter().filter(|u| !quantified(u)).map(Type::TVar);
        for l in v.cell_lower().into_iter().chain(fed) {
            match self.settled(env, &l)? {
                Some(Type::Bottom) => (),
                Some(t) => flows.push(t),
                None => {
                    // a cell being fixed around this one flows into it:
                    // the two are one type, and what flows into that
                    // cell flows on into this one
                    if let Type::TVar(u) = &shallow(&l)
                        && !u.same_cell(v)
                        && self.fixing.contains(&u.cell_addr())
                    {
                        let mut seen = SmallVec::from_iter([v.cell_addr()]);
                        for t in flowing(u, &mut seen) {
                            match self.settled(env, &t)? {
                                None | Some(Type::Bottom) => (),
                                Some(t) => flows.push(t),
                            }
                        }
                        self.cycles.push((u.clone(), v.clone()))
                    }
                }
            }
        }
        let single = singleton(v);
        let t = match &flows[..] {
            [] if single => Type::Bottom,
            [] => {
                let mut demands: SmallVec<[Type; 4]> = SmallVec::new();
                // the variables of a definition it is demanded as
                let mut variables: SmallVec<[TVar; 2]> = SmallVec::new();
                for u in v.cell_constraints() {
                    for w in open_cells(&u) {
                        self.fix(env, &w)?
                    }
                    let u = deep(&u);
                    if let Type::TVar(w) = &u
                        && (w.is_rigid() || quantified(w))
                        && !variables.iter().any(|x| x.same_cell(w))
                    {
                        variables.push(w.clone())
                    }
                    let says = !matches!(u, Type::TVar(_) | Type::Bound(_))
                        && !u.is_trait_ref(env);
                    if says {
                        demands.push(u)
                    }
                }
                known(v, |k| k.demanded = true);
                // a bound that names the cell itself decides nothing alone
                let t = self.meet(env, &demands)?;
                let t = within(env, t, &v.cell_constraints())?;
                match open_cells(&t).iter().any(|u| u.same_cell(v)) {
                    true => Type::Bottom,
                    // demanded as one variable of a definition and as
                    // nothing else, it is that variable: each instance
                    // gives it its type
                    false if matches!(t, Type::Bottom) && variables.len() == 1 => {
                        Type::TVar(variables.swap_remove(0))
                    }
                    false => t,
                }
            }
            [one] => one.clone(),
            many => join(env, many)?,
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
        let u = match u {
            Use::Select(id) => Use::Routed(id, self.selects[id].template.clone()),
            u => u,
        };
        let t = shallow(t);
        let Some(v) = open(&t) else { return self.apply(env, &u, &t).map(|_| ()) };
        let mut met = false;
        for l in flowing(v, &mut SmallVec::new()) {
            met |= self.apply(env, &u, &l)?
        }
        known(v, |k| k.uses.push((u, met)));
        self.track(v);
        // where nothing has flowed in, a shape demanded of the cell
        // serves, since nothing wider can
        for b in v.cell_constraints() {
            if open(&shallow(&b)).is_none() {
                self.demanded(env, v, &b)?
            }
        }
        Ok(())
    }

    /// `b` is an upper bound of `v`: a use that needs a shape, and that
    /// nothing reaching `v` has given one, takes it from a shape demanded
    /// of `v`. One that has its shape reads what flows in, which is no
    /// wider than any demand.
    fn demanded(&mut self, env: &Env, v: &TVar, b: &Type) -> Result<()> {
        let b = shallow(b);
        for i in 0..uses(v).len() {
            let (u, met) = uses(v)[i].clone();
            if met {
                continue;
            }
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

    /// The part `t` of a value goes to `out`, the place a read or a
    /// reference stands for. One that may be written through takes the
    /// part's exact type: what is written must fit every value the place
    /// may be a part of.
    fn part(&mut self, env: &Env, t: &Type, out: &Type, write: bool) -> Result<()> {
        match write {
            false => self.constrain(env, t, out),
            true => self.equal(env, t, out).map_err(|e| {
                e.context(
                    "a writable reference to a part of a union needs the part to be \
                     one type in every member",
                )
            }),
        }
    }

    /// Apply a use to one type that reaches it; false where the type says
    /// nothing (bottom).
    fn apply(&mut self, env: &Env, u: &Use, a: &Type) -> Result<bool> {
        if let Use::Select(id) | Use::Routed(id, _) = u {
            return self.select_route(env, *id, a).map(|()| true);
        }
        let a = shallow(a);
        if let Use::Method { typings, receiver, at, args, out, throws, level, .. } = u {
            if matches!(a, Type::Bottom) {
                return Ok(false);
            }
            // the implementation whose receiver takes the whole type; with
            // none, a union's members each find their own below
            for typed in typings.iter() {
                let typed = self.instantiate(typed, 0, *level);
                let Type::Fn(ft) = &typed else { continue };
                let positional = ft
                    .args
                    .iter()
                    .filter(|a| matches!(a.kind, FnArgKind::Positional { .. }));
                let Some(formal) = positional.clone().nth(*receiver) else { continue };
                if self.attempt(env, &a, &formal.typ) {
                    let mut args = args.clone();
                    args[*at].1 = a.clone();
                    self.call(env, ft, &args, out)?;
                    return self.constrain(env, &ft.throws, throws).map(|()| true);
                }
            }
        }
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
            (Type::Struct(fs), Use::Field { field, out, write }) => {
                match fs.iter().find(|(n, ..)| n == field) {
                    Some((_, t, _)) => self.part(env, t, out, *write).map(|()| true),
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
            (Type::Abstract { id, params }, Use::Index { i: 0, out, from, .. }) => {
                match env.abstract_rep(*id, from).and_then(|_| rep(env, id, params)) {
                    Some(rep) => self.constrain(env, &rep, out).map(|()| true),
                    None => bail!(
                        "the definition of {a} is not visible here, so its payload \
                         cannot be read"
                    ),
                }
            }
            (Type::Error(t), Use::Index { i: 0, out, .. }) => {
                self.constrain(env, t, out).map(|()| true)
            }
            (Type::Tuple(ts), Use::Index { i, out, write, .. }) => match ts.get(*i) {
                Some(t) => self.part(env, t, out, *write).map(|()| true),
                None => bail!("{a} has no element {i}"),
            },
            (Type::Array(t), Use::Element { out }) => {
                self.constrain(env, t, out).map(|()| true)
            }
            (Type::Primitive(p), Use::Element { out }) if *p == Typ::Bytes => {
                self.constrain(env, &Type::Primitive(Typ::U8.into()), out).map(|()| true)
            }
            (_, Use::Element { .. }) => bail!("{a} is no array and no bytes"),
            (Type::Fn(ft), Use::Call { args, out }) => {
                self.call(env, ft, args, out).map(|()| true)
            }
            (_, Use::Field { field, .. }) => bail!("{a} has no field {field}"),
            (_, Use::Index { i, .. }) => bail!("{a} has no element {i}"),
            (_, Use::Call { .. }) => bail!("{a} is not a function"),
            (_, Use::With { .. }) => bail!("{a} is not a struct"),
            (_, Use::Select(_) | Use::Routed(..)) => Ok(false),
            (_, Use::Method { name, .. }) => bail!("{a} has no implementation of {name}"),
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
        for a in ft.args.iter() {
            let Some((name, optional)) = labeled(a) else { continue };
            match args.iter().any(|(l, _)| l.as_ref() == Some(&name)) {
                true => (),
                false if optional => self.omitted(env, &name, &a.typ)?,
                false => bail!("missing required argument #{name}"),
            }
        }
        for v in cells_in(&ft.rtype).iter().filter(|v| reads_type(v)) {
            known(v, |k| k.made = true)
        }
        self.constrain(env, &ft.rtype, out)
    }

    /// A function whose parameter for the label `name` is `formal` is
    /// called without it: the default its definition gives the label goes
    /// to the parameter ([`defaulted`]).
    fn omitted(&mut self, env: &Env, name: &ArcStr, formal: &Type) -> Result<()> {
        let mut cells = SmallVec::new();
        cells_of(formal, true, &mut cells);
        for v in cells.iter() {
            let given = known(v, |k| k.defaults.clone());
            for (label, default, param) in given.iter() {
                if label == name {
                    self.constrain(env, default, param).map_err(|e| {
                        e.context(format!(
                            "the default of #{name} is passed here, where the call \
                             leaves #{name} out"
                        ))
                    })?
                }
            }
        }
        Ok(())
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
        assert_eq!(a.cell_lower().len(), 2);
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
    fn a_cell_holds_the_union_of_what_flows_into_it() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let (t, tt) = var("t");
        for t in ["i64", "string"] {
            assert!(s.constrain(&env, &parsed(&env, t), &tt).is_ok())
        }
        assert!(s.finish(&env).is_ok());
        assert_eq!(fixed(&t), "[i64, string]");
        // through another cell too
        let mut s = Solving::default();
        let ((_, te), (p, tp)) = (var("e"), var("p"));
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
        // an operand of arithmetic in the definition takes its parameter
        // and an i64: it is one numeric type in each instance
        s.track(&a);
        s.declare(&x, Type::Bound(crate::typ::Bound::Singleton));
        assert!(s.constrain(&env, &ta, &tx).is_ok());
        assert!(s.constrain(&env, &parsed(&env, "i64"), &tx).is_ok());
        let f = over(&env, "fn(p: 'a) -> i64", &[("a", &ta)]);
        assert!(s.generalize(&env, &f, 0, mark).is_ok());
        assert!(!x.is_bound());
        let ok = s.instantiate(&f, 0, Level::TOP);
        assert!(s.constrain(&env, &ok, &parsed(&env, "fn(p: i64) -> i64")).is_ok());
        assert!(s.finish(&env).is_ok());
        let no = s.instantiate(&f, 0, Level::TOP);
        assert!(s.constrain(&env, &no, &parsed(&env, "fn(p: f64) -> i64")).is_ok());
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
        let read = super::Use::Field { field: ArcStr::from("a"), out: to.clone(), write: false };
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
    fn a_use_nothing_reaches_is_each_instances() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let mark = s.mark();
        let ((p, tp), (o, to)) = (var_at("p", 1), var_at("o", 1));
        s.track(&p);
        s.track(&o);
        let (field, from) = (ArcStr::from("a"), to.clone());
        let read = super::Use::Field { field, out: from, write: false };
        assert!(s.watch(&env, &tp, read).is_ok());
        let f = over(&env, "fn(p: 'p) -> 'o", &[("p", &tp), ("o", &to)]);
        assert!(s.generalize(&env, &f, 0, mark).is_ok());
        let ok = s.instantiate(&f, 0, Level::TOP);
        let has = parsed(&env, "fn(p: {a: i64}) -> i64");
        assert!(s.constrain(&env, &ok, &has).is_ok() && s.finish(&env).is_ok());
        let no = s.instantiate(&f, 0, Level::TOP);
        let lacks = parsed(&env, "fn(p: {b: i64}) -> i64");
        assert!(s.constrain(&env, &no, &lacks).is_err() || s.finish(&env).is_err());
    }

    #[test]
    fn a_select_gives_each_arm_what_its_pattern_admits() {
        use super::{Arm, Pat, Use};
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((n, tn), (r, tr)) = (var("n"), var("r"));
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
        let arm = Arm {
            pat: Pat::Test(parsed(&env, "&i64"), Box::new(Pat::Any(Some(v.clone())))),
            guarded: false,
        };
        let id = s.select(vec![arm], ArcStr::from("here"));
        let scrutinee = parsed(&env, "[&i64, &string]");
        assert!(s.watch(&env, &scrutinee, Use::Select(id)).is_ok());
        assert_eq!(shown(&v.cell_lower()), ["&[i64, string]"]);
    }

    #[test]
    fn arithmetic_over_one_declared_variable() {
        let (env, _) = typed(&[], "null");
        let mut s = Solving::default();
        let ((a, ta), (sum, ts)) = (var_at("a", 1), var_at("s", 1));
        s.declare(&a, parsed(&env, "[i64, f64]"));
        let _gate = a.open_rigid();
        assert!(s.constrain(&env, &ta, &ts).is_ok());
        assert!(s.constrain(&env, &ts, &ta).is_ok());
        s.constrain(&env, &ts, &parsed(&env, "[i64, f64]")).unwrap();
        let _ = sum;
    }
}
