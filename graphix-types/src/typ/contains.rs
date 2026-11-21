use crate::{
    AddrSet, PrintFlag, SourcePosition,
    dbgenv::{graphix_dbg_bind, graphix_dbg_cycle_bt},
    env::{Env, ImplDef},
    expr::{ModPath, Told},
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        AbstractId, Bound, Brief, CoreTrait, FnType, Indiscernible, NodePairs, Open,
        TVar, TraitId, Type,
        cast::shares_form,
        node_addr,
        setops::{renames, strictly_identical, strictly_same_unordered, union_identical},
        tvar::{
            ArenaRef, CellFact, Cycle, Declared, Fresh, HeldQuantifiers, RigidGate,
            Toward, current_arena, cycle_toward, settled_cause_note,
        },
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use netidx_value::Typ;
use nohash::IntMap;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// The infinite-type refusal: what a unification asked for holds
/// itself, so no finite type does.
#[derive(Debug)]
pub struct InfiniteType;

impl std::fmt::Display for InfiniteType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            "cannot infer a finite type here: unification requires a type that \
             contains itself (e.g. a function that returns itself); declare a \
             named recursive type and annotate the binding",
        )
    }
}

impl std::error::Error for InfiniteType {}

/// The named types a member walk expanded: each definition's parameter
/// lists, told apart by [`strictly_identical`], so two open cells are two.
type Expanded = IntMap<usize, SmallVec<[Arc<[Type]>; 1]>>;

/// Whether `params` of `def` is not in `seen` yet; adds it.
fn first_expansion(seen: &mut Expanded, def: usize, params: &Arc<[Type]>) -> bool {
    let applied = seen.entry(def).or_default();
    let same = |ps: &Arc<[Type]>| {
        ps.len() == params.len()
            && ps.iter().zip(params.iter()).all(|(p0, p1)| strictly_identical(p0, p1))
    };
    if applied.iter().any(same) {
        return false;
    }
    applied.push(params.clone());
    true
}

/// The walk of [`Type::dispatch_members`]: a named type already
/// expanded is skipped, and a member already in `out` is not added.
fn flatten_members(
    env: &Env,
    t: &Type,
    seen: &mut Expanded,
    out: &mut SmallVec<[Type; 8]>,
) -> Result<()> {
    fn add(out: &mut SmallVec<[Type; 8]>, m: Type) {
        if !out.iter().any(|o| strictly_identical(o, &m)) {
            out.push(m)
        }
    }
    ensure_sufficient(|| match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => flatten_members(env, &b, seen, out),
            None => Ok(add(out, t.clone())),
        },
        Type::Set(ts) => ts.iter().try_for_each(|t| flatten_members(env, t, seen, out)),
        Type::Ref(tr) => {
            if let Some(r) = tr.resolve_in(env)
                && !first_expansion(seen, r.def_key(), &tr.params)
            {
                return Ok(());
            }
            flatten_members(env, &t.lookup_ref(env)?, seen, out)
        }
        Type::Primitive(p) => {
            Ok(p.iter().for_each(|b| add(out, Type::Primitive(b.into()))))
        }
        t => Ok(add(out, t.clone())),
    })
}

/// Is `a` an open cell that `b` reaches (`'r ⊇ fn(..) -> 'r`)?
/// A rigid cell never unifies, so it makes no cycle.
fn open_cell_reaches(a: &Type, b: &Type) -> bool {
    matches!(a, Type::TVar(tv) if !tv.is_bound() && !tv.is_rigid() && tv.would_cycle(b))
}

/// Does `t` reach a cell that is open, unconstrained and
/// `cycle_refused`? Pure read through bindings and constraints.
fn type_has_refused_open_cell(t: &Type) -> bool {
    fn walk(t: &Type, visited: &mut AddrSet) -> bool {
        ensure_sufficient(|| {
            if let Some(node) = node_addr(t)
                && !visited.insert(node)
            {
                return false;
            }
            match t {
                Type::TVar(tv) => {
                    if !visited.insert(tv.cell_addr()) {
                        return false;
                    }
                    let (bound, cons, refused) = {
                        let cell = tv.cell();
                        let cell = cell.read();
                        (
                            cell.binding.clone(),
                            cell.constraints.clone(),
                            cell.facts.contains(CellFact::CycleRefused),
                        )
                    };
                    if bound.is_none() && cons.is_empty() && refused {
                        return true;
                    }
                    bound.iter().chain(cons.iter()).any(|c| walk(c, visited))
                }
                t => {
                    let mut found = false;
                    t.for_each_child(&mut |c| found |= walk(c, visited));
                    found
                }
            }
        })
    }
    walk(t, &mut LPooled::take())
}

/// True iff every conjunct of the cell's constraints contains `t`; one
/// that holds the cell itself ([`holds_cell`]) contains whatever the cell
/// takes. A pure probe.
fn conjuncts_admit(tv: &TVar, env: &Env, t: &Type) -> Result<bool> {
    for c in tv.cell_constraints().iter() {
        if !holds_cell(env, c, tv.cell_addr()) && !c.decided_holds(env, t)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Whether every conjunct of the open cell's constraints contains `t`
/// ([`conjuncts_admit`]).
pub(super) fn cell_admits(tv: &TVar, env: &Env, t: &Type) -> Result<bool> {
    conjuncts_admit(tv, env, t)
}

impl Type {
    /// Whether the declared bound `self` implies the bound `c`: it is `c`,
    /// or holds it of every type it admits (a predicate only where none of
    /// those has an unknown part), `Ordered` implies `Discernible` and
    /// `Singleton` implies `OneNumber`.
    pub fn implies(&self, env: &Env, c: &Type) -> Result<bool> {
        if strictly_identical(self, c)
            || matches!(
                (self, c),
                (Type::Bound(Bound::Ordered), Type::Bound(Bound::Discernible))
                    | (Type::Bound(Bound::Singleton), Type::Bound(Bound::OneNumber))
            )
        {
            return Ok(true);
        }
        if self.is_predicate() || (c.is_predicate() && self.holds_unknown(env)) {
            return Ok(false);
        }
        // the bound holds whatever types the declaring side's variables take
        let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
        hold_rigid_through_bounds(self, &mut LPooled::take(), &mut gates);
        c.decided_holds(env, self)
    }
}

/// [`hold_rigid`], and every open cell the conjuncts of a held cell reach,
/// to any depth: a declaring side's variables are each its callers' to
/// pick, one named only in a bound too. `held` is the cells held so far.
pub(super) fn hold_rigid_through_bounds(
    t: &Type,
    held: &mut AddrSet,
    gates: &mut Vec<RigidGate>,
) {
    each_cell_through_bounds(t, false, held, &mut |tv| {
        gates.push(tv.open_rigid());
        true
    });
}

/// Whether `f` holds of each open cell `t` reaches, and of each one the
/// conjuncts of such a cell reach, and its lower bounds where `lower`, to
/// any depth, each once: `seen` holds the cells met. Stops at the first
/// that `f` refuses.
fn each_cell_through_bounds(
    t: &Type,
    lower: bool,
    seen: &mut AddrSet,
    f: &mut impl FnMut(&TVar) -> bool,
) -> bool {
    let mut todo: LPooled<Vec<TVar>> = LPooled::take();
    let reach = |t: &Type, todo: &mut Vec<TVar>| {
        find_open_cell(t, true, &mut |tv| {
            todo.push(tv.clone());
            false
        });
    };
    reach(t, &mut todo);
    while let Some(tv) = todo.pop() {
        if !seen.insert(tv.cell_addr()) {
            continue;
        }
        if !f(&tv) {
            return false;
        }
        let lowers = match lower {
            true => tv.cell_lower(),
            false => SmallVec::new(),
        };
        for c in tv.cell_constraints().iter().chain(lowers.iter()) {
            reach(c, &mut todo)
        }
    }
    true
}

/// `f` over copies of `ts` that share one cell map, each rigid cell's
/// copy held rigid: what `f` decides, it decides in the copies alone.
pub(super) fn on_copies<const N: usize, R>(
    ts: [&Type; N],
    f: impl FnOnce([Type; N]) -> R,
) -> R {
    let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    let copies = ts
        .map(|t| t.reset_tvars_int(&mut known, Fresh::Copy).unwrap_or_else(|| t.clone()));
    let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
    for t in ts {
        find_open_cell(t, true, &mut |tv| {
            if tv.is_rigid()
                && let Some(c) = known.get(&tv.cell_addr())
            {
                gates.push(c.open_rigid());
            }
            false
        });
    }
    f(copies)
}

/// How two distinct open cells unify.
#[derive(Debug, Clone, Copy)]
enum OpenPair {
    /// Two declared vars of the def under check (rigid cells exist only
    /// inside its gate): the body must not require them equal.
    Distinct,
    /// Both names are settled (`frozen`): merge the cells, since
    /// `frozen` gates name-aliasing, not unification.
    Merge,
    /// `t0` keeps its name: `t1` aliases it.
    AliasRight,
    /// `t0` aliases `t1`.
    AliasLeft,
}

impl OpenPair {
    fn of(t0: &TVar, t1: &TVar) -> Self {
        if t0.is_rigid() && t1.is_rigid() {
            return OpenPair::Distinct;
        }
        // a rigid cell survives only a merge
        if t0.is_rigid() || t1.is_rigid() {
            return OpenPair::Merge;
        }
        match (t0.read().frozen, t1.read().frozen) {
            (true, true) => OpenPair::Merge,
            (true, false) => OpenPair::AliasRight,
            (false, _) => OpenPair::AliasLeft,
        }
    }

    /// Perform the unification; `false` for [`OpenPair::Distinct`].
    fn apply(self, t0: &TVar, t1: &TVar) -> bool {
        let dbg = |what: &str, a: &TVar, b: &TVar| {
            if graphix_dbg_bind() {
                eprintln!(
                    "{what} '{}({:x}) -> '{}({:x})",
                    a.name,
                    a.cell_addr(),
                    b.name,
                    b.cell_addr()
                );
            }
        };
        match self {
            OpenPair::Distinct => return false,
            OpenPair::Merge => t0.alias_cells(t1),
            OpenPair::AliasRight => {
                dbg("RALIAS", t1, t0);
                t1.alias(t0)
            }
            OpenPair::AliasLeft => {
                dbg("LALIAS", t0, t1);
                t0.alias(t1)
            }
        }
        true
    }
}

/// Weld the tvar cells of two loosely-equal types by position (the
/// loose part is `Fn` equality, which does not tell distinct open cells
/// apart): two open cells that compared equal must share fate, or the
/// discarded side's later binding never reaches the survivor. Two
/// function types paired are one, their cells welded or shared already,
/// so each names the other's definitions. `false` when two distinct
/// rigid cells meet; only `commit` links.
struct Link {
    commit: bool,
    found: NodePairs,
}

impl Link {
    fn equal(&mut self, t0: &Type, t1: &Type) -> bool {
        if t0.node_key().is_some_and(|k| t1.node_key() == Some(k)) {
            return true;
        }
        let pair = self.found.key(t0, t1);
        if pair.is_some_and(|pair| self.found.contains(pair)) {
            return true;
        }
        let equal = ensure_sufficient(|| self.equal_inner(t0, t1));
        if let Some(pair) = pair.filter(|_| equal) {
            self.found.insert(pair, t0, t1)
        }
        equal
    }

    fn all(&mut self, a: &[Type], b: &[Type]) -> bool {
        a.iter().zip(b.iter()).all(|(x, y)| self.equal(x, y))
    }

    fn equal_inner(&mut self, t0: &Type, t1: &Type) -> bool {
        match (t0, t1) {
            (Type::TVar(a), Type::TVar(b)) => {
                if a.same_cell(b) {
                    return true;
                }
                match (a.binding(), b.binding()) {
                    (None, None) => match OpenPair::of(a, b) {
                        OpenPair::Distinct => false,
                        act => !self.commit || act.apply(a, b),
                    },
                    (Some(x), Some(y)) => self.equal(&x, &y),
                    // a var a merge left forwarding is the cell it forwards to
                    (Some(x), None) => self.equal(&x, t1),
                    (None, Some(y)) => self.equal(t0, &y),
                }
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                let equal = f0
                    .args
                    .iter()
                    .zip(f1.args.iter())
                    .all(|(a, b)| self.equal(&a.typ, &b.typ))
                    && match (&f0.vargs, &f1.vargs) {
                        (Some(a), Some(b)) => self.equal(a, b),
                        _ => true,
                    }
                    && self.equal(&f0.rtype, &f1.rtype)
                    && self.equal(&f0.throws, &f1.throws);
                if equal && self.commit {
                    f0.lambda_ids.link(&f1.lambda_ids)
                }
                equal
            }
            (Type::Ref(r0), Type::Ref(r1)) => self.all(&r0.params, &r1.params),
            (Type::Set(a), Type::Set(b)) => {
                (a.len() > 1
                    && !a.iter().zip(b.iter()).all(|(x, y)| strictly_identical(x, y))
                    && strictly_same_unordered(a, b))
                    || self.all(a, b)
            }
            (Type::Tuple(a), Type::Tuple(b))
            | (Type::Variant(_, a, _), Type::Variant(_, b, _))
            | (Type::Abstract { params: a, .. }, Type::Abstract { params: b, .. }) => {
                self.all(a, b)
            }
            (Type::Struct(a), Type::Struct(b)) => {
                a.iter().zip(b.iter()).all(|((_, x, _), (_, y, _))| self.equal(x, y))
            }
            (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b))
            | (Type::Error(a), Type::Error(b)) => self.equal(a, b),
            (Type::ByRef(m0, a), Type::ByRef(m1, b)) => m0 == m1 && self.equal(a, b),
            (Type::Map(m0), Type::Map(m1)) => {
                self.equal(&m0.key, &m1.key) && self.equal(&m0.value, &m1.value)
            }
            (Type::App(c0, a0), Type::App(c1, a1)) => {
                self.equal(c0, c1) && self.equal(a0, a1)
            }
            // a bound variable compares through its binding, as union_identical
            // does, so the cells under it weld
            (Type::TVar(a), t) => match a.binding() {
                Some(x) => self.equal(&x, t),
                None => true,
            },
            (t, Type::TVar(b)) => match b.binding() {
                Some(y) => self.equal(t, &y),
                None => true,
            },
            _ => true,
        }
    }
}

fn link_equal(t0: &Type, t1: &Type, commit: bool) -> bool {
    Link { commit, found: NodePairs::default() }.equal(t0, t1)
}

/// Weld two open cells a union collapse paired, as [`Link`] welds those a
/// containment compared equal.
pub(super) fn weld(a: &TVar, b: &TVar) {
    if !a.same_cell(b) && a.binding().is_none() && b.binding().is_none() {
        OpenPair::of(a, b).apply(a, b);
    }
}

/// How many positional arguments a function type takes, when it takes a
/// fixed number.
fn positional_arity(t: &Type) -> Option<usize> {
    t.with_deref(|t| match t {
        Some(Type::Fn(ft)) if ft.vargs.is_none() => {
            Some(ft.args.iter().filter(|a| a.is_positional()).count())
        }
        _ => None,
    })
}

/// `t` with its bindings and named types followed to its head; `None`
/// where a named type on the way is one `seen` holds, which takes each
/// one followed.
fn named_head(env: &Env, t: &Type, seen: &mut Expanded) -> Option<Type> {
    let mut t = t.with_deref(|d| d.cloned()).unwrap_or_else(|| t.clone());
    while let Type::Ref(tr) = &t
        && let Some((def, x)) = t.lookup_ref_probe(env)
    {
        #[cfg(test)]
        HEAD_STEPS.with(|n| n.set(n.get() + 1));
        if !first_expansion(seen, def, &tr.params) {
            return None;
        }
        t = x.with_deref(|d| d.cloned()).unwrap_or(x);
    }
    Some(t)
}

/// Whether `c` holds the open cell at `addr` as it stands, so that every
/// type the cell takes is in `c`: `c` is the cell, a union one of whose
/// members holds it, through names and bound cells, or an open cell above
/// a lower bound that holds it.
pub(super) fn holds_cell(env: &Env, c: &Type, addr: usize) -> bool {
    // a name holds no cell but its parameters'
    let may_hold = |t: &Type| match t {
        Type::TVar(_) | Type::Set(_) | Type::App(..) => true,
        Type::Ref(tr) => !tr.params.is_empty(),
        _ => false,
    };
    if !may_hold(c) {
        return false;
    }
    let mut seen: LPooled<Expanded> = LPooled::take();
    let mut cells: LPooled<AddrSet> = LPooled::take();
    let mut todo: SmallVec<[Type; 4]> = SmallVec::new();
    todo.push(c.clone());
    while let Some(t) = todo.pop() {
        match &named_head(env, &t, &mut seen) {
            Some(Type::Set(ms)) => {
                todo.extend(ms.iter().filter(|m| may_hold(m)).cloned())
            }
            Some(Type::TVar(tv)) => {
                let Some(open) = tv.open_cell() else { continue };
                if open.cell_addr() == addr {
                    return true;
                }
                if cells.insert(open.cell_addr()) {
                    todo.extend(open.cell_lower().into_iter().filter(may_hold))
                }
            }
            _ => (),
        }
    }
    false
}

/// Whether `expected` is, or returns, `` [`Keep(T), null] `` and `actual`
/// holds, or returns, a value that is neither: one kept bare.
fn kept_bare(env: &Env, expected: &Type, actual: &Type) -> bool {
    let members = |t: &Type| {
        let mut found: SmallVec<[Type; 4]> = SmallVec::new();
        let mut todo: SmallVec<[Type; 4]> = SmallVec::new();
        let mut seen: LPooled<Expanded> = LPooled::take();
        todo.extend(match named_head(env, t, &mut seen) {
            Some(Type::Fn(ref ft)) => named_head(env, &ft.rtype, &mut seen),
            head => head,
        });
        while let Some(t) = todo.pop() {
            match &t {
                Type::Set(ms) => {
                    todo.extend(ms.iter().filter_map(|m| named_head(env, m, &mut seen)))
                }
                _ => found.push(t),
            }
        }
        found
    };
    let keep = |t: &Type| matches!(t, Type::Variant(tag, ts, _) if &**tag == "Keep" && ts.len() == 1);
    let null = |t: &Type| matches!(t, Type::Primitive(p) if *p == Typ::Null);
    let expected = members(expected);
    expected.iter().any(keep)
        && expected.iter().all(|m| keep(m) || null(m))
        && members(actual).iter().any(|m| !keep(m) && !null(m))
}

/// Two function types that differ by a renaming of their open cells alone
/// ([`renames`]) and whose cells can be welded; under `commit` they are
/// welded and the function types linked as one.
pub(super) fn renaming_linked(env: &Env, t0: &Type, t1: &Type, commit: bool) -> bool {
    renames(env, t0, t1)
        && link_equal(t0, t1, false)
        && (!commit || link_equal(t0, t1, true))
}

/// A non-containment report that formats lazily: callers probe these
/// errors, and rendering a large type eagerly is tree-cost.
// CR claude for eric: [structure/low] About 1,300 lines of this file decide no
// containment: this type and its notes, AmbiguousMember's and UndecidedTaker's
// messages, DataMeet, same_name_apart, kept_bare, contains_mismatch and the trait,
// bound and uncastable refusals, and the dispatch planning (dispatch_arms, crosses,
// dispatch_crossing) that only the trait nodes call. They sit between the walk's memos
// and contains_dispatch, so the relation's rules are read across a thousand lines of
// message building. Move the refusals to a `typ/refusal.rs` and the dispatch planning
// to a `typ/dispatch.rs`: plain moves, no new layer. Found by reading; not run.
// (t-contains-a-07)
pub struct TypeMismatch {
    expected: Type,
    actual: Type,
    /// The arena of the epoch that refused: a refusal outlives a failed
    /// input, and its types' cells must too.
    _arena: Option<ArenaRef>,
    /// A name the two sides write at one place for two definitions, and
    /// where each is written: taken while the definitions are in scope.
    apart: Option<(ModPath, SourcePosition, SourcePosition)>,
    /// [`kept_bare`], taken while the named types are in scope.
    kept_bare: bool,
    /// A cycle under a constructor refused a pair of cells on the way: no
    /// finite type holds both.
    infinite: bool,
    /// [`settled_cause_note`] of the two sides.
    settled: Option<CompactString>,
    /// [`unmet_bound_note`] of the two sides.
    unmet: Option<CompactString>,
    /// [`undeclared_bound_note`] of the two sides.
    undeclared: Option<CompactString>,
    held: HeldQuantifiers,
}

impl std::fmt::Display for TypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.infinite {
            return write!(f, "{InfiniteType}");
        }
        format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
            if let Some((passed, takes)) =
                positional_arity(&self.expected).zip(positional_arity(&self.actual))
                && passed != takes
            {
                let s = |n: usize| if n == 1 { "" } else { "s" };
                write!(
                    f,
                    "a function taking {takes} argument{} where {passed} argument{} are \
                     passed: ",
                    s(takes),
                    s(passed)
                )?;
            }
            let (expected, actual) = self.held.printing(|| {
                (
                    format_compact!("{}", Brief(&self.expected)),
                    format_compact!("{}", Brief(&self.actual)),
                )
            });
            write!(f, "type mismatch {expected} does not contain {actual}")?;
            if self.kept_bare {
                write!(f, ": a [`Keep(T), null] keeps v as `Keep(v)")?;
            }
            // sides that print alike name two definitions of one name
            if expected == actual
                && let Some((name, p0, p1)) = &self.apart
            {
                write!(
                    f,
                    ": the two {name} are different definitions, at {p0} and {p1}"
                )?;
            }
            if let Some(note) = &self.settled {
                write!(f, ": {note}")?;
            }
            if let Some(note) = &self.unmet {
                write!(f, ": {note}")?;
            }
            if let Some(note) = &self.undeclared {
                write!(f, ": {note}")?;
            }
            Ok(())
        })
    }
}

/// The note a refusal ends with where a generated cell it reaches, which
/// prints as what it is above, stands within a bound that none of those
/// lower bounds meets: the bound, and the type it refuses.
fn unmet_bound_note(env: &Env, types: &[&Type]) -> Option<CompactString> {
    let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    for t in types {
        t.reached_cells(&mut cells);
    }
    let mut found: SmallVec<[&TVar; 2]> = cells
        .values()
        .filter(|tv| {
            tv.generated() && tv.binding().is_none() && !tv.cell_constraints().is_empty()
        })
        .collect();
    found.sort_by_key(|tv| tv.cell_addr());
    found.into_iter().find_map(|tv| {
        let lower = tv
            .cell_lower()
            .into_iter()
            .find(|l| !conjuncts_admit(tv, env, l).unwrap_or(true))?;
        let within = format_with_flags(PrintFlag::ReplacePrims, || {
            let mut s = CompactString::default();
            for (i, c) in tv.cell_constraints().iter().enumerate() {
                s.push_str(if i == 0 { "" } else { " & " });
                s.push_str(&format_compact!("{c}"));
            }
            s
        });
        let lower =
            format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                format_compact!("{lower}")
            });
        Some(format_compact!(
            "it is within the bound {within}, which {lower} does not meet: a variable \
             the bound names is one type"
        ))
    })
}

/// The note a refusal ends with where a variable of a definition being
/// checked meets an open cell whose bound the variable's own does not imply:
/// the body needs more of it than its signature declares.
fn undeclared_bound_note(env: &Env, s: &Type, t: &Type) -> Option<CompactString> {
    type Seen = AHashSet<(usize, usize)>;
    fn unmet(
        env: &Env,
        declared: &TVar,
        open: &TVar,
        seen: &mut Seen,
    ) -> Option<CompactString> {
        if !declared.is_rigid() || open.is_rigid() {
            return None;
        }
        open.cell_constraints()
            .iter()
            .find_map(|c| {
                declared.cell_constraints().iter().find_map(|own| {
                    let mut found = None;
                    go(env, own, c, seen, &mut found);
                    found
                })
            })
            .or_else(|| {
                let cons = open.cell_constraints();
                let needs: SmallVec<[&Type; 2]> = cons
                    .iter()
                    .filter(|c| {
                        !(holds_cell(env, c, open.cell_addr())
                            || declared.conjunct_implies(env, c).unwrap_or(true))
                    })
                    .collect();
                (!needs.is_empty()).then(|| declared.undeclared_bound(env, &needs))
            })
    }
    fn go(
        env: &Env,
        s: &Type,
        t: &Type,
        seen: &mut Seen,
        found: &mut Option<CompactString>,
    ) -> bool {
        ensure_sufficient(|| {
            if let (Some(a), Some(b)) = (node_addr(s), node_addr(t))
                && !seen.insert((a, b))
            {
                return false;
            }
            match (s, t) {
                (Type::TVar(a), Type::TVar(b))
                    if a.binding().is_none() && b.binding().is_none() =>
                {
                    *found = unmet(env, a, b, seen).or_else(|| unmet(env, b, a, seen));
                    found.is_some()
                }
                (Type::TVar(tv), t) if tv.is_bound() => match tv.binding() {
                    Some(b) => go(env, &b, t, seen, found),
                    None => false,
                },
                (s, Type::TVar(tv)) if tv.is_bound() => match tv.binding() {
                    Some(b) => go(env, s, &b, seen, found),
                    None => false,
                },
                (Type::Ref(r0), Type::Ref(r1))
                    if r0.name == r1.name && r0.params.len() == r1.params.len() =>
                {
                    r0.params
                        .iter()
                        .zip(r1.params.iter())
                        .any(|(a, b)| go(env, a, b, seen, found))
                }
                (s, t) => any_part_pair(s, t, &mut |a, b| go(env, a, b, seen, found)),
            }
        })
    }
    let mut found = None;
    if TVar::checking_a_body() {
        go(env, s, t, &mut LPooled::take(), &mut found);
    }
    found
}

/// A type that two or more undecided members of a union could take. Which
/// takes it would follow the order the check relates things in, so the
/// check takes neither: the union is decided where it comes from.
#[derive(Debug)]
pub struct AmbiguousMember {
    union: Type,
    typ: Type,
    members: SmallVec<[Type; 2]>,
    /// The definitions being checked where the refusal was made, whose
    /// variables it names after their gates close.
    declared: Declared,
}

impl AmbiguousMember {
    pub(super) fn new(s: &[Type], t: &Type, members: &[&Type]) -> anyhow::Error {
        if graphix_dbg_bind() {
            eprintln!(
                "AMBIGUOUS {} <- {t}",
                Type::Set(Arc::from_iter(s.iter().cloned()))
            );
        }
        if graphix_dbg_cycle_bt() {
            eprintln!("AMBIGUOUS\n{}", std::backtrace::Backtrace::force_capture());
        }
        anyhow::Error::new(AmbiguousMember {
            union: Type::Set(Arc::from_iter(s.iter().cloned())),
            typ: t.clone(),
            members: members.iter().map(|m| (*m).clone()).collect(),
            declared: Declared::current(),
        })
    }

    /// The refusal without what to annotate, for a site that names it.
    pub fn fact(&self) -> impl std::fmt::Display + '_ {
        struct Fact<'a>(&'a AmbiguousMember);
        impl std::fmt::Display for Fact<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let Fact(a) = self;
                let flags = PrintFlag::DerefTVars | PrintFlag::ReplacePrims;
                a.declared.printing(|| {
                    format_with_flags(flags, || {
                        write!(f, "{} could go to ", Brief(&a.typ))?;
                        write_members(f, &a.members)?;
                        write!(
                            f,
                            " of the union {}, which nothing has decided, and the check \
                             picks none by order",
                            Brief(&a.union)
                        )
                    })
                })
            }
        }
        Fact(self)
    }
}

impl std::fmt::Display for AmbiguousMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: write the type of what holds that union (a let, a parameter), so its \
             members are decided",
            self.fact()
        )
    }
}

impl std::error::Error for AmbiguousMember {}

/// `members` as a refusal lists them: copies of one variable print alike.
fn write_members(f: &mut std::fmt::Formatter<'_>, members: &[Type]) -> std::fmt::Result {
    let shown: SmallVec<[CompactString; 2]> =
        members.iter().map(|m| format_compact!("{}", Brief(m))).collect();
    match shown.iter().all(|m| *m == shown[0]) {
        true => write!(f, "any of {} members", shown.len()),
        false => {
            for (i, m) in shown.iter().enumerate() {
                let sep = if i == 0 { "" } else { " or " };
                write!(f, "{sep}{m}")?;
            }
            Ok(())
        }
    }
}

/// Where the union of an [`UndecidedTaker`] stands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TakerUnion {
    /// A variable's bound.
    Bound,
    /// The type the value is held in: a binding written, a parameter
    /// passed to.
    Holder,
    /// The value's own type, under a function's parameter: the undecided
    /// type is its holder's.
    Value,
}

/// A type nothing has decided, at a union two or more decided members of
/// which could take it: a variable's bound, or a union met in a
/// containment. Which member holds it follows what decides that type,
/// later, so the check takes neither: the undecided side is annotated.
#[derive(Debug)]
pub struct UndecidedTaker {
    union: Type,
    typ: Type,
    members: SmallVec<[Type; 2]>,
    at: TakerUnion,
    throws_only: bool,
}

impl UndecidedTaker {
    pub fn at(&self) -> TakerUnion {
        self.at
    }

    /// The refusal without what to annotate, for a site that names it.
    pub fn fact(&self) -> impl std::fmt::Display + '_ {
        struct Fact<'a>(&'a UndecidedTaker);
        impl std::fmt::Display for Fact<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let Fact(u) = self;
                format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                    write!(f, "{} could go to ", Brief(&u.typ))?;
                    write_members(f, &u.members)?;
                    let held = match u.at {
                        TakerUnion::Bound => "bound",
                        TakerUnion::Holder | TakerUnion::Value => "union",
                    };
                    let decides = match u.throws_only {
                        true => {
                            "what it throws decides which, and nothing has decided that"
                        }
                        false => "nothing has decided which",
                    };
                    write!(f, " of the {held} {}, and {decides}", Brief(&u.union))
                })
            }
        }
        Fact(self)
    }
}

impl std::fmt::Display for UndecidedTaker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let annotate = match self.at {
            TakerUnion::Bound | TakerUnion::Holder => {
                "annotate the value (bind it to a `let` with its type written)"
            }
            TakerUnion::Value => {
                "write the type of what holds the value (a let, a parameter), or give \
                 the value's own parameter one member's type"
            }
        };
        write!(f, "{}: {annotate}", self.fact())
    }
}

impl std::error::Error for UndecidedTaker {}

/// The walk that finds where an open cell bounded `Data` meets a function
/// or a reference, pairing the parts of two types as `contains` does.
struct DataMeet<'a> {
    env: &'a Env,
    /// Each side's enclosing function types' own quantifiers: such a cell
    /// judges what its function is called with, never the function.
    own: [SmallVec<[ArcStr; 2]>; 2],
    /// The node pairs walked, each held so an expansion's node keeps its
    /// address for the walk.
    seen: LPooled<AHashMap<(usize, usize), (Type, Type)>>,
}

impl<'a> DataMeet<'a> {
    fn new(env: &'a Env) -> Self {
        Self { env, own: Default::default(), seen: LPooled::take() }
    }

    /// The part of one side that an open `Data` cell of the other meets
    /// and that holds a function or a reference.
    fn walk(&mut self, t: [&Type; 2]) -> Option<Type> {
        ensure_sufficient(|| self.walk_inner(t))
    }

    fn walk_inner(&mut self, t: [&Type; 2]) -> Option<Type> {
        for i in 0..2 {
            let Type::TVar(tv) = t[i] else { continue };
            let other = t[1 - i];
            return match tv.binding() {
                Some(b) => {
                    let mut t = t;
                    t[i] = &b;
                    self.walk(t)
                }
                None => (tv.requires_data()
                    && !self.own[i].contains(&tv.name)
                    && (other.holds_fn(self.env) || other.holds_ref(self.env)))
                .then(|| other.clone()),
            };
        }
        if let (Some(a), Some(b)) = (node_addr(t[0]), node_addr(t[1]))
            && self.seen.insert((a, b), (t[0].clone(), t[1].clone())).is_some()
        {
            return None;
        }
        let expand = |i: usize| t[i].lookup_ref_probe(self.env).map(|(_, x)| x);
        match t {
            [Type::Ref(r0), Type::Ref(r1)] if r0.same_definition(r1, self.env) => {
                self.zip(&r0.params, &r1.params)
            }
            [Type::Ref(_), _] => {
                let x = expand(0)?;
                self.walk([&x, t[1]])
            }
            [_, Type::Ref(_)] => {
                let x = expand(1)?;
                self.walk([t[0], &x])
            }
            [Type::Set(s), o] => s.iter().find_map(|m| self.walk([m, o])),
            [o, Type::Set(s)] => s.iter().find_map(|m| self.walk([o, m])),
            [Type::Fn(f0), Type::Fn(f1)] => {
                let lens = [self.own[0].len(), self.own[1].len()];
                self.own[0].extend(f0.quantifiers.iter().cloned());
                self.own[1].extend(f1.quantifiers.iter().cloned());
                let found = self.fn_parts(f0, f1);
                self.own[0].truncate(lens[0]);
                self.own[1].truncate(lens[1]);
                found
            }
            // CR claude for claude: [bug/medium] `DataMeet` pairs the parameters of any
            // two abstract types, where `contains` (and `any_part_pair`) pair them only
            // under one id. So passing `B<fn() -> i64>` where `A<'d: Data>` is wanted
            // is refused as "fn() -> i64 is not data", and no change to the function
            // fixes that; with `B(1)` the same call reports the real type mismatch.
            // This walk is one of three in this file that pair two types' parts, each
            // by its own rules. `any_part_pair` checks ids, pairs refs by
            // `same_filled_definition`, pairs function args by position with no throws
            // or vargs, and has no App. `DataMeet` has no id check, expands refs, pairs
            // labeled args by label, and walks throws, vargs and App. `same_name_apart`
            // is a positional `for_each_child` zip whatever the two heads are. Fix: one
            // pairing function with `contains`'s rules, which `meets_itself`,
            // `decided_part`, `nests`, `DataMeet` and `same_name_apart` call with their
            // own leaf test; at least require one abstract id here. probe:
            // design/review-2026-11-16/repro/x-dup-datameet-abstract-ids.gx (x-dup-02)
            [Type::Tuple(a), Type::Tuple(b)]
            | [Type::Abstract { params: a, .. }, Type::Abstract { params: b, .. }] => {
                self.zip(a, b)
            }
            [Type::Variant(n0, a, _), Type::Variant(n1, b, _)] if n0 == n1 => {
                self.zip(a, b)
            }
            [Type::Struct(a), Type::Struct(b)] => a.iter().find_map(|(n, x, _)| {
                let (_, y, _) = b.iter().find(|(m, _, _)| m == n)?;
                self.walk([x, y])
            }),
            [Type::Array(a), Type::Array(b)]
            | [Type::List(a), Type::List(b)]
            | [Type::Error(a), Type::Error(b)]
            | [Type::ByRef(_, a), Type::ByRef(_, b)] => self.walk([a, b]),
            [Type::Map(m0), Type::Map(m1)] => self
                .walk([&m0.key, &m1.key])
                .or_else(|| self.walk([&m0.value, &m1.value])),
            [Type::App(c0, a0), Type::App(c1, a1)] => {
                self.walk([c0, c1]).or_else(|| self.walk([a0, a1]))
            }
            _ => None,
        }
    }

    fn zip(&mut self, a: &[Type], b: &[Type]) -> Option<Type> {
        a.iter().zip(b.iter()).find_map(|(x, y)| self.walk([x, y]))
    }

    /// A function's arguments, positional ones by position and labeled
    /// ones by label, then its variadic argument, result and throws.
    fn fn_parts(&mut self, f0: &FnType, f1: &FnType) -> Option<Type> {
        f0.args
            .iter()
            .filter(|a| a.is_positional())
            .zip(f1.args.iter().filter(|a| a.is_positional()))
            .find_map(|(x, y)| self.walk([&x.typ, &y.typ]))
            .or_else(|| {
                f0.args.iter().find_map(|x| {
                    let l = x.label()?;
                    let y = f1.args.iter().find(|y| y.label() == Some(l))?;
                    self.walk([&x.typ, &y.typ])
                })
            })
            .or_else(|| match (&f0.vargs, &f1.vargs) {
                (Some(a), Some(b)) => self.walk([a, b]),
                _ => None,
            })
            .or_else(|| self.walk([&f0.rtype, &f1.rtype]))
            .or_else(|| self.walk([&f0.throws, &f1.throws]))
    }
}

/// The first two references, at one place in `t0` and `t1`, that write one
/// name and resolve to different definitions: the name and where each
/// definition is written.
fn same_name_apart(
    t0: &Type,
    t1: &Type,
) -> Option<(ModPath, SourcePosition, SourcePosition)> {
    fn walk(
        t0: &Type,
        t1: &Type,
        seen: &mut AHashSet<(usize, usize)>,
    ) -> Option<(ModPath, SourcePosition, SourcePosition)> {
        ensure_sufficient(|| match (t0, t1) {
            (Type::TVar(tv), _) | (_, Type::TVar(tv)) => {
                let b = tv.binding()?;
                match t0 {
                    Type::TVar(_) => walk(&b, t1, seen),
                    _ => walk(t0, &b, seen),
                }
            }
            (Type::Ref(r0), Type::Ref(r1)) if r0.name == r1.name => {
                match (r0.resolved(), r1.resolved()) {
                    (Some(d0), Some(d1)) if r0.def_key() != r1.def_key() => {
                        Some((r0.name.clone(), d0.pos(), d1.pos()))
                    }
                    _ => r0
                        .params
                        .iter()
                        .zip(r1.params.iter())
                        .find_map(|(a, b)| walk(a, b, seen)),
                }
            }
            _ => {
                if let (Some(a), Some(b)) = (node_addr(t0), node_addr(t1))
                    && !seen.insert((a, b))
                {
                    return None;
                }
                let mut c0: LPooled<Vec<Type>> = LPooled::take();
                let mut c1: LPooled<Vec<Type>> = LPooled::take();
                t0.for_each_child(&mut |c| c0.push(c.clone()));
                t1.for_each_child(&mut |c| c1.push(c.clone()));
                c0.iter().zip(c1.iter()).find_map(|(a, b)| walk(a, b, seen))
            }
        })
    }
    walk(t0, t1, &mut LPooled::take())
}

impl TypeMismatch {
    /// Whether this refuses `actual` to `expected` themselves, not one part
    /// of them to another.
    pub fn refuses(&self, expected: &Type, actual: &Type) -> bool {
        super::setops::strictly_identical(&self.expected, expected)
            && super::setops::strictly_identical(&self.actual, actual)
    }

    /// `expected` refusing `actual` with no note: a check by bounds refuses
    /// so, and wording a refusal decides nothing of either side.
    /// `e` with the note naming the binding an earlier input settled at ⊥
    /// in `cause`, where `e` is a mismatch that carries none yet.
    pub(crate) fn noting_settled(
        env: &Env,
        cause: &Type,
        e: anyhow::Error,
    ) -> anyhow::Error {
        match e.downcast::<TypeMismatch>() {
            Ok(mut m) => {
                if m.settled.is_none() {
                    m.settled = super::tvar::settled_note(env, &[cause])
                }
                anyhow::Error::new(m)
            }
            Err(e) => e,
        }
    }

    pub(crate) fn plain(expected: &Type, actual: &Type) -> anyhow::Error {
        anyhow::Error::new(TypeMismatch {
            expected: expected.clone(),
            actual: actual.clone(),
            apart: None,
            kept_bare: false,
            infinite: false,
            settled: None,
            unmet: None,
            undeclared: None,
            held: HeldQuantifiers::of([expected, actual]),
            _arena: current_arena(),
        })
    }
}

/// A refusal is read as its message, never as the cells of its types.
impl std::fmt::Debug for TypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for TypeMismatch {}

/// The first open cell in `t` that `pick` takes, each shared node walked
/// once; a function type's parts only `into_fns`.
pub(super) fn find_open_cell(
    t: &Type,
    into_fns: bool,
    pick: &mut dyn FnMut(&TVar) -> bool,
) -> Option<TVar> {
    fn walk(
        t: &Type,
        into_fns: bool,
        pick: &mut dyn FnMut(&TVar) -> bool,
        seen: &mut AddrSet,
    ) -> Option<TVar> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => walk(&b, into_fns, pick, seen),
                None => pick(tv).then(|| tv.clone()),
            },
            Type::Fn(_) if !into_fns => None,
            t => {
                if let Some(a) = node_addr(t)
                    && !seen.insert(a)
                {
                    return None;
                }
                let mut found = None;
                t.for_each_child(&mut |c| {
                    if found.is_none() {
                        found = walk(c, into_fns, pick, seen)
                    }
                });
                found
            }
        })
    }
    walk(t, into_fns, pick, &mut LPooled::take())
}

/// An open cell in `t` with the `Discernible` or `Ordered` conjunct, and
/// the stronger of the two it holds.
fn discernible_cell(t: &Type) -> Option<(TVar, Type)> {
    let bound = |tv: &TVar| {
        let cons = tv.cell_constraints();
        [Type::Bound(Bound::Ordered), Type::Bound(Bound::Discernible)]
            .into_iter()
            .find(|b| cons.contains(b))
    };
    let tv = find_open_cell(t, false, &mut |tv| bound(tv).is_some())?;
    let b = bound(&tv)?;
    Some((tv, b))
}

/// Whether `f` holds of a pair of parts `s` and `t` place alike: a
/// function's result and arguments, each member of a union beside the
/// other side, the parts of one constructor on both sides.
// CR claude for claude: [structure/medium] This file pairs the parts of two types in
// five walks, and each pairs a function's arguments by its own rule: here by index when
// the counts are equal, without vargs or throws; DataMeet::fn_parts by position and by
// label, with vargs and throws; commit_parts by index when counts and names are equal;
// Link::equal_inner by a blind zip; same_name_apart by zipping for_each_child.
// commit_parts' rule is why a bound's parts miss a callback whose parameter is named
// differently. Give FnType one argument pairing beside its contains_int and make this
// function the one enumeration of paired parts, so the other four keep only their own
// TVar, Ref and Set arms. Found by reading; not run. (t-contains-a-04)
pub(super) fn any_part_pair(
    s: &Type,
    t: &Type,
    f: &mut impl FnMut(&Type, &Type) -> bool,
) -> bool {
    fn zip(a: &[Type], b: &[Type], f: &mut impl FnMut(&Type, &Type) -> bool) -> bool {
        a.len() == b.len() && a.iter().zip(b.iter()).any(|(a, b)| f(a, b))
    }
    match (s, t) {
        (Type::Fn(f0), Type::Fn(f1)) => {
            f(&f0.rtype, &f1.rtype)
                || (f0.args.len() == f1.args.len()
                    && f0.args.iter().zip(f1.args.iter()).any(|(a, b)| f(&a.typ, &b.typ)))
        }
        (Type::Set(ms), t) => ms.iter().any(|m| f(m, t)),
        (s, Type::Set(ms)) => ms.iter().any(|m| f(s, m)),
        (Type::Array(a), Type::Array(b))
        | (Type::List(a), Type::List(b))
        | (Type::Error(a), Type::Error(b))
        | (Type::ByRef(_, a), Type::ByRef(_, b)) => f(a, b),
        (Type::Map(m0), Type::Map(m1)) => f(&m0.key, &m1.key) || f(&m0.value, &m1.value),
        (Type::Tuple(a), Type::Tuple(b)) => zip(a, b, f),
        (Type::Variant(n0, a, _), Type::Variant(n1, b, _)) if n0 == n1 => zip(a, b, f),
        (Type::Abstract { id: i0, params: a }, Type::Abstract { id: i1, params: b })
            if i0 == i1 =>
        {
            zip(a, b, f)
        }
        (Type::Ref(r0), Type::Ref(r1)) if r0.same_filled_definition(r1) => {
            zip(&r0.params, &r1.params, f)
        }
        (Type::Struct(a), Type::Struct(b)) => a.iter().any(|(n, x, _)| {
            b.iter().find(|(m, _, _)| m == n).is_some_and(|(_, y, _)| f(x, y))
        }),
        _ => false,
    }
}

impl Type {
    /// `self` holds `t`: in a check by bounds the solver relates the two;
    /// outside one, the relation over decided types ([`super::holds`]).
    pub fn check_contains(&self, env: &Env, t: &Self) -> Result<()> {
        if let Some(r) = super::bounds::with(|s| s.constrain(env, t, self)) {
            return r;
        }
        let ok = super::holds::holds(env, self, t)?;
        if graphix_dbg_bind() {
            eprintln!("CHK-CONTAINS {self} >= {t} -> {ok}");
        }
        match ok {
            true => Ok(()),
            false => Err(self.contains_mismatch(env, t)),
        }
    }

    /// Whether an open cell of `self` meets, at its place in `t`, a part
    /// holding the cell under a constructor: `self` cannot hold `t` as no
    /// finite type holds itself, and no union of the two would.
    pub fn meets_itself(&self, t: &Self) -> bool {
        fn go(s: &Type, t: &Type, seen: &mut AHashSet<(usize, usize)>) -> bool {
            ensure_sufficient(|| {
                if let (Some(a), Some(b)) = (node_addr(s), node_addr(t))
                    && !seen.insert((a, b))
                {
                    return false;
                }
                // an open cell stands for its lower bounds too
                match (s, t) {
                    (Type::TVar(tv), t) => match tv.binding() {
                        Some(b) => go(&b, t, seen),
                        None => {
                            (!matches!(t, Type::TVar(u) if u.same_cell(tv))
                                && cycle_toward(&tv.cell(), t, Toward::Any)
                                    == Some(Cycle::Constructor))
                                || tv.cell_lower().iter().any(|l| go(l, t, seen))
                        }
                    },
                    (s, Type::TVar(tv)) => match tv.binding() {
                        Some(b) => go(s, &b, seen),
                        None => tv.cell_lower().iter().any(|l| go(s, l, seen)),
                    },
                    (s, t) => any_part_pair(s, t, &mut |a, b| go(a, b, seen)),
                }
            })
        }
        go(self, t, &mut LPooled::take())
    }

    /// Whether a part of `self` under a constructor is `t`, or holds `t`'s
    /// part at its place in `t`: a value of `t`, or of its part, written
    /// inside one of `self`. An open cell stands for its lower bounds.
    pub fn nests(&self, t: &Self) -> bool {
        fn addr(t: &Type) -> Option<usize> {
            match t {
                Type::TVar(tv) => Some(tv.cell_addr()),
                t => node_addr(t),
            }
        }
        fn each(tv: &TVar, f: &mut impl FnMut(&Type) -> bool) -> bool {
            match tv.binding() {
                Some(b) => f(&b),
                None => tv.cell_lower().iter().any(f),
            }
        }
        fn under(s: &Type, t: &Type, inside: bool, seen: &mut AHashSet<usize>) -> bool {
            ensure_sufficient(|| {
                if addr(s).is_some_and(|a| !seen.insert(a)) {
                    return false;
                }
                match s {
                    Type::TVar(tv) => each(tv, &mut |s| {
                        (inside && union_identical(s, t)) || under(s, t, inside, seen)
                    }),
                    Type::Set(ms) => ms.iter().any(|m| {
                        (inside && union_identical(m, t)) || under(m, t, inside, seen)
                    }),
                    s => {
                        let mut found = false;
                        s.for_each_child(&mut |c| {
                            found =
                                found || union_identical(c, t) || under(c, t, true, seen)
                        });
                        found
                    }
                }
            })
        }
        fn paired(w: &Type, h: &Type, seen: &mut AHashSet<(usize, usize)>) -> bool {
            ensure_sufficient(|| {
                if let (Some(a), Some(b)) = (addr(w), addr(h))
                    && !seen.insert((a, b))
                {
                    return false;
                }
                match (w, h) {
                    (Type::TVar(tv), h) => each(tv, &mut |w| paired(w, h, seen)),
                    (w, Type::TVar(tv)) => each(tv, &mut |h| paired(w, h, seen)),
                    (w, h) => {
                        under(w, h, false, &mut LPooled::take())
                            || any_part_pair(h, w, &mut |h, w| paired(w, h, seen))
                    }
                }
            })
        }
        paired(self, t, &mut LPooled::take())
    }

    /// A bound cell of `self` that holds lower bounds, a type a use
    /// decided after values were built into it, whose place in `t` does
    /// not reach the cell: the cell, and that part of `t`. The union of
    /// the bounds and the part holds both, where the decided type cannot.
    pub fn decided_part(&self, t: &Self) -> Option<(TVar, Type)> {
        fn go(
            s: &Type,
            t: &Type,
            seen: &mut AHashSet<(usize, usize)>,
            found: &mut Option<(TVar, Type)>,
        ) -> bool {
            ensure_sufficient(|| {
                if let (Some(a), Some(b)) = (node_addr(s), node_addr(t))
                    && !seen.insert((a, b))
                {
                    return false;
                }
                match (s, t) {
                    (Type::TVar(tv), t) => match tv.binding() {
                        Some(_) if !tv.cell_lower().is_empty() && !tv.would_cycle(t) => {
                            *found = Some((tv.clone(), t.clone()));
                            true
                        }
                        Some(b) => go(&b, t, seen, found),
                        None => false,
                    },
                    (s, Type::TVar(tv)) => match tv.binding() {
                        Some(b) => go(s, &b, seen, found),
                        None => false,
                    },
                    (s, t) => any_part_pair(s, t, &mut |a, b| go(a, b, seen, found)),
                }
            })
        }
        let mut found = None;
        go(self, t, &mut LPooled::take(), &mut found);
        found
    }

    fn contains_mismatch(&self, env: &Env, t: &Self) -> anyhow::Error {
        // A refused open cell on either side is the infinite type,
        // surfacing at a consumer; report it as the settle path does.
        if type_has_refused_open_cell(self)
            || type_has_refused_open_cell(t)
            || open_cell_reaches(self, t)
            || open_cell_reaches(t, self)
        {
            return anyhow::Error::new(InfiniteType);
        }
        if let Some(e) = self.discernible_refusal(env, t) {
            return e;
        }
        if let Some(e) = self.trait_refusal(env, t) {
            return e;
        }
        if let Some(e) = self.uncastable_refusal(env, t) {
            return e;
        }
        anyhow::Error::new(TypeMismatch {
            expected: self.clone(),
            actual: t.clone(),
            apart: same_name_apart(self, t),
            kept_bare: kept_bare(env, self, t),
            infinite: false,
            settled: settled_cause_note(env, self, t, false),
            unmet: unmet_bound_note(env, &[self, t]),
            undeclared: undeclared_bound_note(env, self, t),
            held: HeldQuantifiers::of([self, t]),
            _arena: current_arena(),
        })
    }

    /// A type-directed read's target or a writer's value, an open cell
    /// bounded by `Concrete` or `Data`, refuses a type holding an abstract
    /// type that crosses no data: name it.
    fn uncastable_refusal(&self, env: &Env, t: &Self) -> Option<anyhow::Error> {
        let bounded = |t: &Type, by: &dyn Fn(&TVar) -> bool| {
            find_open_cell(t, true, &mut |tv| by(tv)).is_some()
        };
        let either = |by: &dyn Fn(&TVar) -> bool| bounded(self, by) || bounded(t, by);
        if !either(&|tv| tv.requires_concrete() || tv.requires_data()) {
            return None;
        }
        // the open side's own abstract types are the operation's (a tree
        // handle), not the data's: the data's is the other side's
        let open =
            |t: &Type| bounded(t, &|tv| tv.requires_concrete() || tv.requires_data());
        let (bound, data) = if open(t) { (t, self) } else { (self, t) };
        let mut own: LPooled<Vec<AbstractId>> = LPooled::take();
        bound.abstract_ids(env, &mut *own);
        data.not_data(env, &own, [self, t])
    }

    /// Why this type, a writer's value or a read's target, is not data:
    /// an abstract type it holds crosses none (those of `own` are the
    /// operation's), or a function or a reference is part of `meet`.
    pub(crate) fn not_data(
        &self,
        env: &Env,
        own: &[AbstractId],
        meet: [&Type; 2],
    ) -> Option<anyhow::Error> {
        if let Some(a) = self.uncastable_but(env, &|id| own.contains(&id)) {
            let minted = matches!(&a, Self::Abstract { id, .. }
                if env.abstract_reps.get(id).is_some());
            let advice = match minted {
                // CR claude for claude: [readability/low] This advice is also given
                // when the impl exists and is only hidden from a sibling's check
                // because its module's interface does not declare it: the sibling's
                // `json::write_str(m0::make(5))` is told to implement Cast for a type
                // whose module already does. A user trait's refusal in the same place
                // names the interface and the line to add. Where `impls_all` holds a
                // Cast impl for the id that `impl_hidden` filters, say the impl is not
                // in its module's interface and name the declaration. probe:
                // design/review-2026-11-16/repro/x-traits-abstract-sibling-cast-undeclared-message.gx
                // (x-traits-abstract-10)
                true => "implement Cast for it where it is defined",
                false => {
                    "it is a Rust type, which crosses data only where its package \
                          implements Cast for it"
                }
            };
            return Some(anyhow::anyhow!(
                "{a} has no Cast implementation, so no read, cast or writer reaches \
                 it: {advice}"
            ));
        }
        DataMeet::new(env).walk(meet).map(|part| part.no_data())
    }

    /// The refusal of this type, which is or holds a function or a
    /// reference, as data.
    pub(crate) fn no_data(&self) -> anyhow::Error {
        anyhow::anyhow!(
            "{} is not data: data holds no function and no reference",
            Brief(self)
        )
    }

    /// An open cell bounded by a trait refuses a type with a member that
    /// has no impl of it: name the member, and an impl a sibling module
    /// holds but leaves out of its interface.
    fn trait_refusal(&self, env: &Env, t: &Self) -> Option<anyhow::Error> {
        let Self::TVar(tv) = self else { return None };
        if tv.binding().is_some() {
            return None;
        }
        let mut asking: LPooled<Vec<(TraitId, Self)>> = LPooled::take();
        tv.cell_constraints()
            .iter()
            .find_map(|c| Self::bound_refusal(env, c, t, &mut asking))
    }

    /// Why `t` fails the trait bound `c`, when `c` is a trait reference.
    pub(super) fn bound_refusal(
        env: &Env,
        c: &Self,
        t: &Self,
        asking: &mut Vec<(TraitId, Self)>,
    ) -> Option<anyhow::Error> {
        let Self::Ref(r) = c else { return None };
        let tid = env.trait_of_ref(r)?;
        let def = env.trait_def(tid)?;
        let members = Self::dispatch_members(env, t).ok()?;
        let seen = env.impls_of(tid).unwrap_or_default();
        let name = &def.name;
        for m in members.iter() {
            if Self::trait_holds(tid, env, m).ok()? {
                continue;
            }
            if asking.iter().any(|(i, a)| *i == tid && a == m) {
                continue;
            }
            let hidden = env.impls.get(&tid).and_then(|l| {
                l.iter()
                    .find(|im| {
                        !seen.iter().any(|s| Arc::ptr_eq(s, im))
                            && env.heads_overlap(&im.target, m).unwrap_or(false)
                    })
                    .cloned()
            });
            return Some(match hidden {
                None => match Self::asked_impl_bound_refusal(env, tid, m, asking) {
                    Some(why) => Told::over(
                        why,
                        &format_compact!(
                            "{m} does not implement {name}: a bound of its impl is \
                             not met"
                        ),
                    ),
                    None => anyhow::anyhow!("{m} does not implement {name}"),
                },
                Some(im) => anyhow::anyhow!(
                    "{m} does not implement {name} here: the impl in module {} is \
                     not in its interface (declare `impl {name} for {m};` there)",
                    crate::written_scope(&im.scope)
                ),
            });
        }
        if members.len() > 1
            && let Some(m) = def.self_beyond_receiver(env)
        {
            return Some(anyhow::anyhow!(
                "a {name} call over {} narrows only its self argument, and \
                 {name}::{} takes `self` beyond it, in another parameter, a \
                 variable's bound, or under a function parameter or a writable \
                 reference in its result: select on the value and make the call for \
                 each member",
                Brief(t),
                m.name
            ));
        }
        if let Some(why) = Self::dispatch_collision(env, &members) {
            let open = |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
            if let Indiscernible::Pair(a, b) = &why
                && (open(a) || open(b))
            {
                let known = if open(a) { b } else { a };
                return Some(anyhow::anyhow!(
                    "a {} call over {t} tests each member, but a type variable in it \
                     isn't known here and may share {known}'s runtime form: annotate it",
                    def.name
                ));
            }
            let what = format_compact!("a {} call over {t}", def.name);
            return Some(why.untold(&what));
        }
        if let Some((a, b)) = Self::dispatch_crossing(env, &members).ok()? {
            return Some(Self::crossing_refusal(&def.name, t, a, b));
        }
        None
    }

    /// [`Self::impl_bound_refusal`] with `m` recorded as being asked about,
    /// so a type that reaches itself through the bound is not asked again.
    fn asked_impl_bound_refusal(
        env: &Env,
        tid: TraitId,
        m: &Self,
        asking: &mut Vec<(TraitId, Self)>,
    ) -> Option<anyhow::Error> {
        asking.push((tid, m.clone()));
        let why = Self::impl_bound_refusal(env, tid, m, asking);
        asking.pop();
        why
    }

    /// Why a bound of an impl of `tid` whose head holds `m` is not met by
    /// what its parameter took.
    // CR claude for claude: [bug/medium] When a head parameter met a declared variable
    // whose bounds lack the impl's bound, this reports nothing, so bound_refusal says
    // only "Array<'a> does not implement Show". Example: `'a: Number |x: Array<'a>|
    // Show::show(x)` under `impl<'b: Show> Show for Array<'b>`, an impl that visibly
    // matches. A body's call of a bounded function in the same spot names the fix
    // ("write 'a: Ordered + Number"). Report the unmet bound and the annotation ("a
    // bound of its impl is not met: write 'a: Number + Show"), and pin it in
    // lang::traits; nothing pins this refusal today. probe:
    // design/review-2026-11-16/repro/tests-traits-a-rigid-impl-bound-no-remedy.sh
    // (tests-traits-a-04)
    fn impl_bound_refusal(
        env: &Env,
        tid: TraitId,
        m: &Self,
        asking: &mut Vec<(TraitId, Self)>,
    ) -> Option<anyhow::Error> {
        for im in env.impls_of(tid)?.iter().filter(|im| !im.params.is_empty()) {
            let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            let Some(head) = im.target.reset_tvars_int(&mut known, Fresh::Copy) else {
                continue;
            };
            head.drop_bounds();
            if !head.contains(env, m).ok()? {
                continue;
            }
            for p in im.params.iter() {
                let taken = known.get(&p.cell_addr()).and_then(TVar::binding);
                let Some(taken) = taken else { continue };
                for c in p.cell_constraints().iter() {
                    if let Some(why) = Self::bound_refusal(env, c, &taken, asking) {
                        return Some(why);
                    }
                }
            }
        }
        None
    }

    /// The members a dispatch over `t` tests, each once, in the order
    /// first met: a union flattened through its type references (each
    /// named type expanded once, however many of the names it reaches
    /// name it), a union of primitives split into its primitives and an
    /// open cell kept as one member.
    pub fn dispatch_members(env: &Env, t: &Self) -> Result<SmallVec<[Type; 8]>> {
        let mut seen: LPooled<Expanded> = LPooled::take();
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        flatten_members(env, t, &mut seen, &mut members)?;
        Ok(members)
    }

    /// The arms of a dispatch over `t`, a type with no open cell and no
    /// two members that cross ([`Self::dispatch_crossing`]): its
    /// [`Self::dispatch_members`], one per type, none after a member
    /// that holds it and none the arms before it leave no value. Of two
    /// that each hold the other the first met stays, and a member
    /// another holds stands before the first that does.
    pub fn dispatch_arms(env: &Env, t: &Self) -> Result<SmallVec<[Type; 8]>> {
        let holds = |a: &Type, b: &Type| a.decided_holds(env, b);
        let mut arms: SmallVec<[Type; 8]> = SmallVec::new();
        let mut shared = false;
        for m in Self::dispatch_members(env, t)? {
            let mut holder = None;
            for (i, o) in arms.iter().enumerate() {
                if !o.shares_constructor(&m) {
                    continue;
                }
                shared = true;
                if holds(o, &m)? {
                    holder = Some((i, holds(&m, o)?));
                    break;
                }
            }
            match holder {
                Some((_, true)) => (),
                Some((i, false)) => arms.insert(i, m),
                None => arms.push(m),
            }
        }
        if !shared {
            return Ok(arms);
        }
        let mut rest = t.clone();
        let mut i = 0;
        while i < arms.len() {
            if arms[i].could_match(env, &rest)? {
                rest = rest.diff(env, &arms[i])?;
                i += 1
            } else {
                arms.remove(i);
            }
        }
        Ok(arms)
    }

    /// Two members a dispatch's runtime test can't tell apart.
    fn dispatch_collision(env: &Env, members: &[Type]) -> Option<Indiscernible> {
        members.iter().enumerate().find_map(|(i, a)| {
            members[i + 1..].iter().find_map(|b| a.rep_collision_open(env, b))
        })
    }

    /// Whether `self` and `t`, two members of a dispatch, cross: one value
    /// may be a value of both and neither holds the other, so no rule says
    /// which member the value is. A member holding an open cell may cross
    /// whatever it may share a value with. A member that is an open cell
    /// is judged by its form ([`Self::dispatch_collision`]), and by its
    /// typed bounds where it has any: it crosses a type whose form another
    /// shares where every one of them may share a value with it.
    fn crosses(&self, env: &Env, t: &Self) -> Result<bool> {
        let open = |t: &Type| match t {
            Type::TVar(tv) => tv.open_cell(),
            _ => None,
        };
        let cell = match (open(self), open(t)) {
            (Some(c), None) => Some((c, t)),
            (None, Some(c)) => Some((c, self)),
            (Some(_), Some(_)) | (None, None) => None,
        };
        if let Some((cell, t)) = cell {
            let bounds = cell.typed_bounds(env);
            if bounds.is_empty() || !shares_form(env, t) {
                return Ok(false);
            }
            for b in bounds.iter() {
                if !b.shares_value(env, t)? {
                    return Ok(false);
                }
            }
            return Ok(true);
        }
        // of one abstract type made in Graphix they are one type or share
        // no value, whatever a cell in their parameters becomes
        let made = |t: &Type| match t {
            Type::Abstract { id, .. } => env.abstract_reps.contains_key(id),
            _ => false,
        };
        if (made(self) && made(t)) || !self.shares_value(env, t)? {
            return Ok(false);
        }
        if self.has_unbound() || t.has_unbound() {
            return Ok(true);
        }
        let holds = |a: &Type, b: &Type| a.decided_holds(env, b);
        Ok(!(holds(self, t)? || holds(t, self)?))
    }

    /// Two members of a dispatch that cross ([`Self::crosses`]): its arms
    /// would give a value of both to whichever stands first in the union.
    pub fn dispatch_crossing<'a>(
        env: &Env,
        members: &'a [Type],
    ) -> Result<Option<(&'a Type, &'a Type)>> {
        for (i, a) in members.iter().enumerate() {
            for b in &members[i + 1..] {
                if a.crosses(env, b)? {
                    return Ok(Some((a, b)));
                }
            }
        }
        Ok(None)
    }

    /// The refusal of a `name` call over `t`, whose members `a` and `b`
    /// cross.
    pub fn crossing_refusal(name: &str, t: &Type, a: &Type, b: &Type) -> anyhow::Error {
        anyhow::anyhow!(
            "a {name} call over {} tests each member, and a value may be both {} and \
             {}, neither of which holds the other: give the value one member's type",
            t.resolve_tvars(),
            a.resolve_tvars(),
            b.resolve_tvars()
        )
    }

    /// Why `self ⊇ t` fails when `self` holds an open `Discernible` or
    /// `Ordered` cell and `t` is not that.
    pub fn discernible_refusal(&self, env: &Env, t: &Self) -> Option<anyhow::Error> {
        let (tv, bound) = discernible_cell(self)?;
        let why =
            t.indiscernible(env, Open::Benign, bound == Type::Bound(Bound::Ordered))?;
        Some(t.not_discernible(&bound, &tv.name, &why))
    }

    /// Does `t` implement the trait `tid`? ⊥ implements everything,
    /// `Any` nothing, a union iff every member does, an open cell iff it
    /// could still become an implementor (a declared one only through its
    /// own bounds), a typedef by its expansion, anything structural by the
    /// impl table. A question met again inside its own answer holds if the
    /// rest does.
    pub(crate) fn trait_holds(tid: TraitId, env: &Env, t: &Self) -> Result<bool> {
        ensure_sufficient(|| Self::trait_holds_inner(tid, env, t))
    }

    fn trait_holds_inner(tid: TraitId, env: &Env, t: &Self) -> Result<bool> {
        // Eq, Ord and Display have a structural default for every type
        if CoreTrait::of_id(tid).is_some_and(CoreTrait::structural) {
            return Ok(true);
        }
        // a union implements a trait when every member does, unless a method
        // takes `self` beyond the receiver, which dispatch never narrows
        let union = match t {
            Self::Set(_) => true,
            Self::Primitive(p) => p.len() > 1,
            _ => false,
        };
        if union
            && env.trait_def(tid).is_some_and(|d| d.self_beyond_receiver(env).is_some())
        {
            return Ok(false);
        }
        // nor when its dispatch, which tests each member, can't tell two
        // apart or gives a value of two to the one the union puts first; each
        // member is asked once, however many names reach it
        if union && env.trait_def(tid).is_some_and(|d| !d.hole) {
            let members = Self::dispatch_members(env, t)?;
            if Self::dispatch_collision(env, &members).is_some()
                || Self::dispatch_crossing(env, &members)?.is_some()
            {
                return Ok(false);
            }
            for m in members.iter() {
                if !Self::trait_holds(tid, env, m)? {
                    return Ok(false);
                }
            }
            return Ok(true);
        }
        let asked = |t: &Type, f: &dyn Fn() -> Result<bool>| {
            let key = (tid, super::holds::identity(env, t));
            if ASKED.with_borrow(|a| a.contains(&key)) {
                return Ok(true);
            }
            let _asking = Asking::push(&ASKED, key);
            f()
        };
        match t {
            Self::Bottom => Ok(true),
            Self::Any => Ok(false),
            Self::TVar(tv) => match tv.binding() {
                Some(b) => Self::trait_holds(tid, env, &b),
                None if tv.is_rigid() => Ok(tv.cell_constraints().iter().any(
                    |c| matches!(c, Self::Ref(r) if env.trait_of_ref(r) == Some(tid)),
                )),
                None => Ok(true),
            },
            Self::Set(ts) => {
                for m in ts.iter() {
                    if !Self::trait_holds(tid, env, m)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            Self::Primitive(p) if p.len() > 1 => {
                for m in p.iter() {
                    if env.find_impl(tid, &Self::Primitive(m.into()))?.is_none() {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            Self::Ref(tr) => match env.trait_of_ref(tr) {
                Some(o) => Ok(o == tid),
                // A constructor trait's reference is matched by name,
                // never expanded.
                None if env.trait_def(tid).is_some_and(|d| d.hole) => {
                    Ok(env.find_impl(tid, t)?.is_some())
                }
                None => {
                    let e = t.lookup_ref(env)?;
                    asked(t, &|| Self::trait_holds(tid, env, &e))
                }
            },
            t => asked(t, &|| Ok(find_impl(env, tid, t)?.is_some())),
        }
    }

    /// Is this a reference to a trait (in `env`)?
    pub fn is_trait_ref(&self, env: &Env) -> bool {
        matches!(self, Self::Ref(tr) if env.trait_of_ref(tr).is_some())
    }

    /// The one typed bound of an open cell (`'a: {a: i64, b: string}`,
    /// `'a: [i64, {a: i64}]`): every value the cell may hold is a value
    /// of it.
    pub fn open_bound(&self, env: &Env) -> Option<Self> {
        let Self::TVar(tv) = self else { return None };
        match &tv.open_cell()?.typed_bounds(env)[..] {
            [b] => Some(b.clone()),
            _ => None,
        }
    }

    /// Whether `self` holds `t`: in a check by bounds as the solver would
    /// relate them, binding nothing; outside one, the relation over decided
    /// types ([`super::holds`]).
    pub fn contains(&self, env: &Env, t: &Self) -> Result<bool> {
        if let Some(fits) = super::bounds::with(|s| s.attempt(env, t, self)) {
            return Ok(fits);
        }
        let r = super::holds::holds(env, self, t);
        if graphix_dbg_bind() {
            eprintln!("CONTAINS {self} >= {t} -> {r:?}");
        }
        r
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typ::{RefHist, RefPair, tvar::CellRef};
    use arcstr::literal;
    use enumflags2::BitFlags;

    // A walk's memo of node pairs keys a variant by its payload and its
    // tag: a walk past its eager nodes that met `A(p) against `A(p) has
    // not met `A(p) against `B(p) over the same payload.
    #[test]
    fn a_walks_node_pairs_tell_apart_variants_over_one_payload() {
        let ts: Arc<[Type]> = Arc::from_iter([Type::Primitive(Typ::I64.into())]);
        let v = |tag| Type::Variant(tag, ts.clone(), Default::default());
        let (a, b) = (v(literal!("A")), v(literal!("B")));
        let past_eager = |last: &Type| {
            let filler = (0..2 * NodePairs::EAGER as usize).map(|_| a.clone());
            Type::Tuple(Arc::from_iter(filler.chain([a.clone(), last.clone()])))
        };
        let (aa, ab) = (past_eager(&a), past_eager(&b));
        let env = Env::default();
        assert!(!aa.could_match(&env, &ab).unwrap());
        assert!(!strictly_identical(&aa, &ab));
        assert!(!union_identical(&aa, &ab));
        assert!(!strictly_identical(&a, &b));
        assert!(!union_identical(&a, &b));
    }

    /// `type T<'x> = Array<'x>` and an application of it to copy.
    fn applicable() -> (Env, Type) {
        Env::typed_at_root(&[("T<'x>", "Array<'x>")], "T<i64>").unwrap()
    }

    /// `T<..T<leaf>..>`, `n` applications deep, outermost first: each
    /// level's parameter is the next level's node.
    fn applied(t: &Type, n: usize, leaf: Type) -> Vec<Type> {
        let Type::Ref(tr) = t else { unreachable!() };
        nested(n, leaf, |p| Type::Ref(Arc::new(tr.with_params(Arc::from_iter([p])))))
    }

    /// `leaf` wrapped `n` times, outermost first.
    fn nested(n: usize, leaf: Type, wrap: impl Fn(Type) -> Type) -> Vec<Type> {
        let mut levels: Vec<Type> = vec![];
        for _ in 0..n {
            let inner = levels.last().cloned().unwrap_or_else(|| leaf.clone());
            levels.push(wrap(inner));
        }
        levels.reverse();
        levels
    }

    fn tagged(n: usize, leaf: Type) -> Vec<Type> {
        nested(n, leaf, |p| {
            Type::Variant(literal!("V"), Arc::from_iter([p]), Default::default())
        })
    }

    fn part(t: &Type) -> Type {
        let mut part = None;
        t.for_each_child(&mut |c| part = Some(c.clone()));
        part.unwrap()
    }

    fn levels_of(tuple: &Type) -> Vec<Type> {
        let mut levels = vec![];
        tuple.for_each_child(&mut |c| levels.push(c.clone()));
        levels
    }

    /// Whether each of `levels` holds the next one's node.
    fn nests_shared(levels: &[Type]) -> bool {
        levels.windows(2).all(|w| part(&w[0]).node_key() == w[1].node_key())
    }

    // A named type applied to parameters is a node a walk visits once; a
    // bare name is a leaf.
    #[test]
    fn a_walk_visits_an_application_once() {
        let (_env, t) = applicable();
        let (_bare_env, bare) = Env::typed_at_root(&[("N", "i64")], "N").unwrap();
        assert!(node_addr(&t).is_some() && node_addr(&bare).is_none());
        let steps = |n: usize| {
            let levels = applied(&t, n, Type::TVar(var("a")));
            let all = Type::Tuple(Arc::from_iter(levels));
            crate::typ::fntyp::TVAR_WALK_STEPS.with(|c| c.set(0));
            let mut known = AHashMap::default();
            all.collect_tvars(&mut known);
            assert_eq!(known.len(), 1);
            crate::typ::fntyp::TVAR_WALK_STEPS.with(|c| c.get())
        };
        for n in [100, 200] {
            assert!(steps(n) <= 4 * n, "{n}: {} steps", steps(n));
        }
    }

    // A copy and a normal form of a chain of applications, or of tags,
    // each level a node the level above shares, rebuild each level once:
    // the result shares its levels as the source does.
    #[test]
    fn a_rewrite_of_a_chain_keeps_its_levels_shared() {
        let (_env, t) = applicable();
        let prim = |t: Typ| Type::Primitive(t.into());
        let unflat = || {
            let inner = Type::Set(Arc::from_iter([prim(Typ::String), prim(Typ::I64)]));
            Type::Set(Arc::from_iter([prim(Typ::I64), inner]))
        };
        for chain in [applied(&t, 50, unflat()), tagged(50, unflat())] {
            let all = Type::Tuple(Arc::from_iter(chain));
            let normal = levels_of(&all.normalize());
            assert_ne!(part(&normal[49]), unflat());
            assert!(nests_shared(&normal));
        }
        for chain in
            [applied(&t, 50, Type::TVar(var("a"))), tagged(50, Type::TVar(var("a")))]
        {
            let all = Type::Tuple(Arc::from_iter(chain.iter().cloned()));
            let copy = levels_of(&all.resolve_tvars());
            assert_ne!(copy[0].node_key(), chain[0].node_key());
            assert!(nests_shared(&copy));
        }
    }

    // The expansions of a chain of applications, each built anew over a
    // level the chain shares, are hashed in a step or two each: a node
    // that holds no cell is hashed once a walk.
    #[test]
    fn the_expansions_of_a_chain_hash_each_level_once() {
        use crate::typ::SHAPE_HASH_STEPS;
        let (env, t) = applicable();
        for n in [100, 200] {
            let levels = applied(&t, n, Type::Primitive(Typ::I64.into()));
            let mut hist: RefHist<AHashMap<RefPair, usize>> = RefHist::new();
            SHAPE_HASH_STEPS.with(|c| c.set(0));
            let ids: AHashSet<_> = levels
                .iter()
                .map(|l| hist.ref_id(&l.lookup_ref(&env).unwrap(), &env).unwrap())
                .collect();
            let steps = SHAPE_HASH_STEPS.with(|c| c.get());
            assert_eq!(ids.len(), n);
            assert!(steps <= 4 * n, "{n}: {steps} steps");
        }
    }

    // Function types apart in their parts are keyed apart by their shape
    // hash, so keying many compares none with another; two whose cells
    // differ only by name share a key, two that pair their cells otherwise
    // do not.
    #[test]
    fn function_types_are_keyed_by_their_parts() {
        use crate::typ::setops::IDENTITY_STEPS;
        const N: usize = 200;
        let env = Env::default();
        let mut hist: RefHist<AHashMap<RefPair, usize>> = RefHist::new();
        IDENTITY_STEPS.with(|c| c.set(0));
        let ids: AHashSet<_> = (0..N)
            .map(|k| {
                let arg = format!("{}i64{}", "Array<".repeat(k), ">".repeat(k));
                hist.ref_id(&parsed(&format!("fn(x: {arg}) -> i64")), &env).unwrap()
            })
            .collect();
        let steps = IDENTITY_STEPS.with(|c| c.get());
        assert_eq!(ids.len(), N);
        assert!(steps <= N, "{steps} identity steps");
        let mut id = |s: &str| hist.ref_id(&parsed(s), &env).unwrap();
        assert_eq!(id("fn(x: 'a) -> 'a"), id("fn(x: 'b) -> 'b"));
        assert_ne!(id("fn(x: 'a) -> 'a"), id("fn(x: 'a) -> 'b"));
    }

    // One cell standing outside a function type and inside it is hashed
    // by its cell outside and alike any open cell inside, so the type is
    // keyed with its twin whose function holds another cell there.
    #[test]
    fn a_cell_outside_and_inside_a_function_type_is_hashed_for_each() {
        let env = Env::default();
        let t = parsed("('a, fn(x: 'a) -> i64)");
        let Type::Tuple(ts) = &t else { unreachable!("a tuple") };
        let Type::Fn(f) = &ts[1] else { unreachable!("a function type") };
        match (&ts[0], &f.args[0].typ) {
            (Type::TVar(a), Type::TVar(b)) => assert!(a.same_cell(b)),
            _ => unreachable!("two vars"),
        }
        let twin =
            Type::Tuple(Arc::from_iter([ts[0].clone(), parsed("fn(x: 'b) -> i64")]));
        assert!(union_identical(&t, &twin));
        let mut hist: RefHist<AHashMap<RefPair, usize>> = RefHist::new();
        assert_eq!(hist.ref_id(&t, &env), hist.ref_id(&twin, &env));
    }

    // A hash that read a cell is taken again: once the cell is bound the
    // type is its binding's.
    #[test]
    fn a_shape_hash_that_read_a_cell_is_not_kept() {
        let env = Env::default();
        let tv = var("a");
        let open = Type::Array(Arc::new(Type::TVar(tv.clone())));
        let closed = parsed("Array<i64>");
        let mut hist: RefHist<AHashMap<RefPair, usize>> = RefHist::new();
        assert_ne!(hist.ref_id(&open, &env), hist.ref_id(&closed, &env));
        tv.bind(Type::Primitive(Typ::I64.into()));
        assert_eq!(hist.ref_id(&open, &env), hist.ref_id(&closed, &env));
    }

    fn at_root(env: &Env, t: &str) -> Type {
        let t = parsed(t).scope_refs(&ModPath::root());
        t.seed_refs(env);
        t
    }

    // The trial gives the first array to `Array<'a>`, deciding 'a, and
    // the placement to the member that holds it as it stands, deciding
    // nothing: the union after it asks a trial of its own, since as
    // written 'a would be `Array<i64>` and the `T` member refused.
    #[test]
    fn a_placement_leaves_its_trial_at_a_choice_between_members() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let held = at_root(
            &env,
            "[`X([Array<'a>, Array<Array<['c, i64, Array<i64>]>>], [Array<'a>, `T('a)]), \
             `Y('d)]",
        );
        let t = at_root(
            &env,
            "[`X(Array<Array<[i64, Array<i64>]>>, \
             [Array<Array<i64>>, `T(Array<[i64, Array<i64>]>)]), \
             `Y(Array<Array<Array<Array<Array<Array<Array<i64>>>>>>>)]",
        );
        held.check_contains(&env, &t).unwrap();
    }

    /// `c` and `d`, two open vars, merged after `met` saw each: the cell
    /// the merge left no var reading.
    fn absorbed_after(mut met: impl FnMut(&Type)) -> CellRef {
        let [c, d] = ["c", "d"].map(|n| TVar::empty_named(n.into()));
        let before = [c.cell(), d.cell()];
        met(&Type::TVar(c.clone()));
        met(&Type::TVar(d.clone()));
        Type::TVar(c.clone()).check_contains(&Env::default(), &Type::TVar(d)).unwrap();
        let merged = c.cell();
        let [x, y] = before;
        if CellRef::ptr_eq(&x, &merged) { y } else { x }
    }

    // And so an open var's id, and an abstract type's, which reads its
    // parameters' address: a type the walk met and its caller dropped
    // lends its id to none made after it.
    #[test]
    fn a_ref_id_holds_what_its_key_reads() {
        let env = Env::default();
        let mut hist: RefHist<AHashMap<RefPair, usize>> = RefHist::new();
        let absorbed = absorbed_after(|t| {
            hist.ref_id(t, &env).unwrap();
        });
        let CellRef::Own(absorbed) = absorbed else { panic!("a test's cell is its own") };
        assert_eq!(
            std::sync::Arc::strong_count(&absorbed),
            2,
            "held here and by the walk"
        );
        let abstract_over = |p: Typ| Type::Abstract {
            id: AbstractId::of(&ModPath::root(), "A"),
            params: Arc::from_iter([Type::Primitive(p.into())]),
        };
        let first = hist.ref_id(&abstract_over(Typ::I64), &env);
        let later: Vec<Type> = (0..64).map(|_| abstract_over(Typ::String)).collect();
        assert!(later.iter().all(|t| hist.ref_id(t, &env) != first));
    }

    fn parsed(s: &str) -> Type {
        let t = crate::expr::parser::parse_type(s).unwrap();
        t.alias_tvars(&mut LPooled::take());
        t
    }

    /// A type that shares its parts: 2^60 nodes as a tree.
    fn shared(tuple: bool) -> Type {
        let mut t = Type::Primitive(Typ::I64.into());
        for _ in 0..60 {
            t = match tuple {
                true => Type::Tuple(Arc::from_iter([t.clone(), t])),
                false => Type::Variant(
                    literal!("A"),
                    Arc::from_iter([t.clone(), t]),
                    Default::default(),
                ),
            };
        }
        t
    }

    /// A diagnostic renders a type that shares its parts in time bounded
    /// by its prefix, not 2^depth, a union's members included.
    #[test]
    fn brief_is_bounded() {
        let [pair, tags, alike] = shared_unions(60);
        let twin = Type::Set(Arc::from_iter([shared(true), shared(true)]));
        for t in [shared(true), shared(false), pair, tags, alike, twin] {
            let s = format_compact!("{}", Brief(&t));
            assert!(s.len() <= Brief::BYTES + '…'.len_utf8() && s.ends_with('…'), "{s}");
        }
        let small = Type::Primitive(Typ::I64.into());
        assert_eq!(format_compact!("{}", Brief(&small)), "i64");
    }

    /// A print where cells print as they stand stops at [`Brief::MOST`]: a
    /// message that prints a type whole, the REPL's type of an input, a
    /// hover. Over types without cells it prints as far as the plain print
    /// does, but for the members a standing print prints once.
    #[test]
    fn a_standing_print_is_bounded() {
        let [pair, _, _] = shared_unions(16);
        let [_, tags, alike] = shared_unions(10);
        let deref: BitFlags<PrintFlag> = PrintFlag::DerefTVars.into();
        let hover = PrintFlag::ReplacePrims | PrintFlag::StandingHeaders;
        for (t, merges) in [(pair, false), (tags, false), (alike, true)] {
            let plain = format!("{t}");
            assert!(plain.len() > Brief::MOST, "{}", plain.len());
            for flags in [deref, hover] {
                let s = format_with_flags(flags, || format!("{t}"));
                assert!(
                    s.len() <= Brief::MOST + '…'.len_utf8() && s.ends_with('…'),
                    "{}",
                    s.len()
                );
                if !merges {
                    assert_eq!(Some(&plain[..Brief::MOST]), s.strip_suffix('…'));
                }
            }
        }
    }

    /// A capped union prints as far as the uncapped print does.
    #[test]
    fn brief_of_a_union_is_its_print_cut() {
        for depth in [2, 7] {
            for t in shared_unions(depth) {
                let flags = PrintFlag::DerefTVars | PrintFlag::ReplacePrims;
                let whole = format_with_flags(flags, || format!("{t}"));
                let brief = format_compact!("{}", Brief(&t));
                match whole.len() > Brief::BYTES {
                    true => {
                        assert_eq!(Some(&whole[..Brief::BYTES]), brief.strip_suffix('…'))
                    }
                    false => assert_eq!(whole, brief),
                }
            }
        }
    }

    /// Two members that print alike over two cells, each printing as the
    /// one type it settles to, print once, capped or not, where the two
    /// together pass what the cap leaves them and one does not: a union of
    /// them, and a union holding it second.
    #[test]
    fn brief_of_a_union_prints_alike_members_once() {
        let i64s = |n| {
            Type::Tuple(Arc::from_iter((0..n).map(|_| Type::Primitive(Typ::I64.into()))))
        };
        let alike = |n| {
            let member = || {
                let cell = TVar::empty_named(literal!("_7"));
                cell.add_lower(Type::Primitive(Typ::I64.into()));
                Type::Tuple(Arc::from_iter([i64s(n), Type::TVar(cell)]))
            };
            Type::Set(Arc::from_iter([member(), member()]))
        };
        let behind =
            Type::Set(Arc::from_iter([i64s(40), Type::Array(Arc::new(alike(50)))]));
        let flags = PrintFlag::DerefTVars | PrintFlag::ReplacePrims;
        for t in [alike(60), behind] {
            let whole = format_with_flags(flags, || format!("{t}"));
            assert!(whole.len() <= Brief::BYTES, "{whole}");
            assert_eq!(format_compact!("{}", Brief(&t)), whole);
        }
    }

    /// Two open cells of one generated name, as two instances of one
    /// inferred generic hold, print under two names, so a union over them
    /// prints both members; a minted name numbers apart in its short form.
    #[test]
    fn two_open_cells_of_one_generated_name_print_apart() {
        let minted = format_compact!("_{}", crate::ids::MINT_BASE + 7);
        for name in [literal!("_7"), ArcStr::from(minted.as_str())] {
            let member = || {
                let cell = Type::TVar(TVar::empty_named(name.clone()));
                Type::Tuple(Arc::from_iter([Type::Primitive(Typ::I64.into()), cell]))
            };
            let t = Type::Set(Arc::from_iter([member(), member()]));
            let flags = PrintFlag::DerefTVars | PrintFlag::ReplacePrims;
            let want = "[(i64, '_7: unbound), (i64, '_7_1: unbound)]";
            assert_eq!(format_with_flags(flags, || format!("{t}")), want);
            assert_eq!(format_compact!("{}", Brief(&t)), want);
        }
    }

    /// An open cell that prints as its one lower bound, `string`.
    fn lowered() -> Type {
        let cell = TVar::empty_named(literal!("_7"));
        cell.add_lower(prim(Typ::String));
        Type::TVar(cell)
    }

    /// `[(t, c1), (t, c2)]` nested `depth` deep, `t` the level below and
    /// `c1`, `c2` two cells `cell` makes.
    fn alike_over_cells(depth: usize, cell: impl Fn() -> Type) -> Type {
        (0..depth).fold(prim(Typ::I64), |t, _| {
            let member = || Type::Tuple(Arc::from_iter([t.clone(), cell()]));
            Type::Set(Arc::from_iter([member(), member()]))
        })
    }

    /// The members printed to print `t` whole and under `Brief`, which
    /// agree, and the text.
    fn member_prints(t: &Type) -> (usize, usize, String) {
        let flags = PrintFlag::DerefTVars | PrintFlag::ReplacePrims;
        let counted = |f: &dyn Fn() -> String| {
            crate::typ::print::MEMBER_PRINTS.with(|n| n.set(0));
            let s = f();
            (crate::typ::print::MEMBER_PRINTS.with(|n| n.get()), s)
        };
        let (whole_n, whole) = counted(&|| format_with_flags(flags, || format!("{t}")));
        let (brief_n, brief) = counted(&|| format!("{}", Brief(t)));
        assert_eq!(whole, brief);
        (whole_n, brief_n, whole)
    }

    /// Members alike but for two cells that print alike, nested: the part
    /// they share prints once a level, not 2^depth times, whether the
    /// cells print as their binding or as their one lower bound.
    #[test]
    fn members_alike_but_for_their_cells_print_a_shared_part_once() {
        let depth = 18;
        let bound = || {
            let cell = TVar::empty_named(literal!("_7"));
            cell.bind(prim(Typ::Bool));
            Type::TVar(cell)
        };
        for (cell, shown) in [(&bound as &dyn Fn() -> Type, "bool"), (&lowered, "string")]
        {
            let t = alike_over_cells(depth, cell);
            let expected =
                (0..depth).fold(String::from("i64"), |s, _| format!("({s}, {shown})"));
            assert_eq!(member_prints(&t), (depth, depth, expected));
        }
    }

    /// The parts decide only what the text would: cells that print apart
    /// keep both members, and a cell prints as the type it is bound to.
    #[test]
    fn members_told_by_their_parts_print_as_their_text_would() {
        let named =
            |n: &'static str| move || Type::TVar(TVar::empty_named(ArcStr::from(n)));
        let pair = |a: Type, b: Type| {
            let shared = alike_over_cells(2, lowered);
            let member = |c| Type::Tuple(Arc::from_iter([shared.clone(), c]));
            Type::Set(Arc::from_iter([member(a), member(b)]))
        };
        let inner = "((i64, string), string)";
        let apart = pair(named("_7")(), named("_8")());
        assert_eq!(
            member_prints(&apart).2,
            format!("[({inner}, '_7: unbound), ({inner}, '_8: unbound)]")
        );
        let bound = TVar::empty_named(literal!("_9"));
        bound.bind(prim(Typ::I64));
        let to_binding = pair(Type::TVar(bound), prim(Typ::I64));
        assert_eq!(member_prints(&to_binding), (3, 3, format!("({inner}, i64)")));
    }

    /// Members alike but for cells each holding a copy of the level below,
    /// nested over cells that print as their lower bound: a cell holding a
    /// union is told by the members' text, each member printed as before
    /// (2^(depth+1) - 2), not by their parts, which would print the level
    /// below three times. Only the lowest level's are told by parts.
    #[test]
    fn members_alike_but_for_cells_holding_a_union_print_as_before() {
        fn level(depth: usize) -> Type {
            match depth {
                0 => lowered(),
                _ => {
                    let member = || {
                        let cell = TVar::empty_named(literal!("_9"));
                        cell.bind(level(depth - 1));
                        Type::Tuple(Arc::from_iter([prim(Typ::I64), Type::TVar(cell)]))
                    };
                    Type::Set(Arc::from_iter([member(), member()]))
                }
            }
        }
        let depth = 10;
        let expected =
            (0..depth).fold(String::from("string"), |s, _| format!("(i64, {s})"));
        let prints = (1 << (depth + 1)) - 2 - (1 << (depth - 1));
        assert_eq!(member_prints(&level(depth)), (prints, prints, expected));
    }

    /// Two names of one cell, one generated, print in one form through a
    /// message, so members alike but for them and for cells that print
    /// alike are one member, as their text would be.
    #[test]
    fn members_over_one_cell_under_two_names_print_as_their_text_would() {
        let generated = TVar::empty_named(literal!("_5"));
        generated.bind(prim(Typ::String));
        let written = generated.renamed(literal!("a"));
        let member =
            |v: &TVar| Type::Tuple(Arc::from_iter([Type::TVar(v.clone()), lowered()]));
        let t = Type::Set(Arc::from_iter([member(&generated), member(&written)]));
        assert_eq!(member_prints(&t), (1, 1, String::from("(string, string)")));
    }

    /// A cell keeps the name it first printed under through a message,
    /// whatever handle a later member reaches it by, so a kept member's
    /// part reprints as it first did and a member whose text differs from
    /// every kept one is kept.
    #[test]
    fn members_over_a_cell_named_by_a_later_member_print_as_their_text_would() {
        let c = || Type::TVar(TVar::empty_named(literal!("_7")));
        let x = TVar::empty_named(literal!("b"));
        let pair = |a: TVar, b: Type| Type::Tuple(Arc::from_iter([Type::TVar(a), b]));
        let k = pair(x.renamed(literal!("_6")), c());
        let t = pair(x.clone(), prim(Typ::I64));
        let u = pair(x.renamed(literal!("z")), c());
        let s = Type::Set(Arc::from_iter([k, t, u]));
        let want = "[('_6: unbound, '_7: unbound), ('_6: unbound, i64), \
                    ('_6: unbound, '_7_1: unbound)]";
        assert_eq!(member_prints(&s), (3, 3, String::from(want)));
    }

    fn var(name: &'static str) -> TVar {
        TVar::empty_named(ArcStr::from(name))
    }

    fn prim(t: Typ) -> Type {
        Type::Primitive(t.into())
    }

    /// A declared var `e` whose bound is a cell `z` bound to `i64`: neither
    /// binds where `e` is rigid, so the relation is no cycle. `z ⊇ e` holds
    /// by the bound, `e ⊇ z` does not (`e` holds only itself), and `e` keeps
    /// its bound.
    #[test]
    fn a_declared_var_and_the_bound_cell_its_bound_names() {
        let env = Env::default();
        for (holder, holds) in [(true, true), (false, false)] {
            let (z, e) = (var("z"), var("e"));
            z.bind(prim(Typ::I64));
            e.add_cell_constraint(Type::TVar(z.clone()));
            let _gate = e.open_rigid();
            let (tz, te) = (Type::TVar(z), Type::TVar(e.clone()));
            let (t0, t1) = if holder { (&tz, &te) } else { (&te, &tz) };
            assert_eq!(t0.decided_holds(&env, t1).unwrap(), holds, "{t0} ⊇ {t1}");
            assert!(!e.is_bound());
            assert_eq!(e.cell_constraints().len(), 1);
        }
    }

    #[test]
    fn mismatch_message_is_bounded() {
        let env = Env::default();
        let small = Type::Primitive(Typ::I64.into());
        for (expected, actual) in [(&small, &shared(false)), (&shared(true), &small)] {
            let e = expected.check_contains(&env, actual).unwrap_err();
            let s = format_compact!("{e}");
            assert!(s.len() < 3 * Brief::BYTES, "{s}");
            assert!(s.starts_with("type mismatch "), "{s}");
        }
    }

    fn binding(t: &Type, i: usize) -> Option<Type> {
        match t {
            Type::Set(s) => s[i].deref_cloned(),
            _ => panic!("not a set: {t}"),
        }
    }

    // a reference can't be read from data: the run-time cast refuses one
    #[test]
    fn parse_into_a_reference_is_refused_at_run_time() {
        let env = Env::default();
        for t in ["&i64", "&mut i64", "{a: &string}", "(&i64, i64)"] {
            let v = parsed(t).cast_value(&env, netidx_value::Value::U64(1));
            assert!(matches!(v, netidx_value::Value::Error(_)), "{t}: {v}");
        }
    }

    // a member bound to an open cell is as free as that cell: the other
    // side's free member aliases it, never captures a sibling
    #[test]
    fn a_member_linked_to_an_open_cell_is_free() {
        let env = Env::default();
        let c = TVar::empty_named(literal!("c"));
        let b = TVar::empty_named(literal!("b"));
        b.bind(Type::TVar(c.clone()));
        let linked = Type::Set(Arc::from_iter([
            Type::TVar(b.clone()),
            Type::Array(Arc::new(Type::TVar(b))),
        ]));
        let free = parsed("['b, Array<'b>]");
        assert!(linked.contains(&env, &free).unwrap());
        assert!(binding(&free, 0).is_none(), "captured: {free}");
    }

    #[test]
    fn a_bound_cell_the_other_side_mentions_answers_by_its_binding() {
        let env = Env::default();
        let a = TVar::empty_named(literal!("a"));
        a.bind(Type::Any);
        let array = Type::Array(Arc::new(Type::TVar(a.clone())));
        assert!(Type::TVar(a).contains(&env, &array).unwrap());
    }

    /// Unions `depth` deep whose members share their parts, a tree of
    /// 2^depth nodes each: `[(t, t), null]`, `` [`A(t, t), `B(t)] `` and
    /// `` [`A(t, t), `A(t, i64)] ``, whose members print alike past the cap.
    fn shared_unions(depth: usize) -> [Type; 3] {
        let leaf = || Type::Primitive(Typ::I64.into());
        let variant = |tag, ts: &[Type]| {
            Type::Variant(tag, Arc::from_iter(ts.iter().cloned()), Default::default())
        };
        let mut ts = [leaf(), leaf(), leaf()];
        for _ in 0..depth {
            let [pair, tags, alike] = &ts;
            ts = [
                [
                    Type::Tuple(Arc::from_iter([pair.clone(), pair.clone()])),
                    Type::Primitive(Typ::Null.into()),
                ],
                [
                    variant(literal!("A"), &[tags.clone(), tags.clone()]),
                    variant(literal!("B"), &[tags.clone()]),
                ],
                [
                    variant(literal!("A"), &[alike.clone(), alike.clone()]),
                    variant(literal!("A"), &[alike.clone(), leaf()]),
                ],
            ]
            .map(|members| Type::Set(Arc::from_iter(members)));
        }
        ts
    }
}

/// A question this thread is asking, popped when the answer is in.
struct Asking<T: 'static>(&'static std::thread::LocalKey<std::cell::RefCell<Vec<T>>>);

impl<T: 'static> Asking<T> {
    fn push(
        key: &'static std::thread::LocalKey<std::cell::RefCell<Vec<T>>>,
        q: T,
    ) -> Self {
        key.with_borrow_mut(|s| s.push(q));
        Asking(key)
    }
}

impl<T: 'static> Drop for Asking<T> {
    fn drop(&mut self) {
        self.0.with_borrow_mut(|s| s.pop());
    }
}

thread_local! {
    /// The impls being tried, each for the trait and the identity of the
    /// type it is tried for ([`find_impl`]).
    static TRYING: std::cell::RefCell<Vec<(TraitId, u128, Arc<ImplDef>)>> =
        const { std::cell::RefCell::new(Vec::new()) };
    /// Whether a type implements a trait, being asked ([`Type::trait_holds`]).
    static ASKED: std::cell::RefCell<Vec<(TraitId, u128)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// The impl of `trait_id` for `t`: the impl whose head can be made `t`. An
/// impl's bound can ask the question again of the type it is being tried
/// for (a recursive typedef under a generic impl): met again, it is
/// answered by the impl being tried.
pub(crate) fn find_impl(
    env: &Env,
    trait_id: TraitId,
    t: &Type,
) -> Result<Option<Arc<ImplDef>>> {
    let Some(list) = env.impls_all(trait_id) else { return Ok(None) };
    // An open cell inside `t` could still become anything, and two heads
    // that fit it would each commit a guess; a declared (rigid) cell is a
    // constant, which takes the impl's bounds as a body's call hands a
    // declared variable a bound it needs, so each call checks them.
    if t.has_free() {
        return match t.lowered_cells() {
            Some(_) => find_impl(env, trait_id, &t.frozen(env)),
            None => Ok(None),
        };
    }
    let key = super::holds::identity(env, t);
    let trying = TRYING.with_borrow(|s| {
        s.iter()
            .rev()
            .find(|(id, k, _)| *id == trait_id && *k == key)
            .map(|(.., im)| im.clone())
    });
    if trying.is_some() {
        return Ok(trying);
    }
    let wanted = t.with_deref(|t| t.and_then(Type::resolved_abstract));
    for im in list.iter().filter(|im| !env.impl_hidden(im)) {
        if let (Some(a), Some(b)) = (im.target.resolved_abstract(), wanted)
            && a != b
        {
            continue;
        }
        let head = if im.params.is_empty() {
            im.target.clone()
        } else {
            im.target.reset_tvars()
        };
        let fits = {
            let _trying = Asking::push(&TRYING, (trait_id, key, im.clone()));
            super::bounds::unifies(env, &[(&head, t)])
        };
        if graphix_dbg_bind() {
            eprintln!("FIND-IMPL head={head:?} t={t:?} fits={fits}");
        }
        if fits {
            return Ok(Some(im.clone()));
        }
    }
    if graphix_dbg_bind() {
        eprintln!("FIND-IMPL none for {t:?}");
    }
    Ok(None)
}

#[cfg(test)]
thread_local! {
    static CONTAINS_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NESTING_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static HEAD_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static VERDICTS_ANSWERED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}
