use crate::{
    PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_cycle_bt},
    env::Env,
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        AndAc, CoreTrait, Lazy, Mutability, NormKey, Open, RefHist, RefPair, TVar,
        TraitId, Type, node_addr, params_size, probe_key, setops::union_identical,
        tvar::would_cycle_inner,
    },
};
use ahash::AHashMap;
use anyhow::{Result, bail};
use enumflags2::{BitFlags, bitflags};
use netidx_value::Typ;
use nohash::{IntMap, IntSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    mem,
    ops::{Deref, DerefMut},
};
use triomphe::Arc;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum ContainsFlags {
    /// Bind and alias cells to make the containment hold; without it
    /// the walk is a probe that binds nothing.
    Commit,
    /// Enforce rigid (declared) tvar semantics in a probe too; see
    /// `TCell::rigid_gates`. A committing check always enforces them: a
    /// rigid cell never binds, so it holds only itself and Bottom.
    RigidCheck,
}

/// The infinite-type rejection wording, shared by
/// [`TVar::settle_or_bottom`] and [`Type::contains_mismatch`].
pub(crate) const INFINITE_TYPE_MSG: &str = "cannot infer a finite type here: unification requires a type that \
     contains itself (e.g. a function that returns itself); declare a \
     named recursive type and annotate the binding";

/// contains' walk state: its cycle memo (each pair in progress with the
/// memo depth it was assumed at), and caches no other relation uses.
pub(super) struct ContainsHist {
    hist: RefHist<AHashMap<RefPair, usize>>,
    /// Per-call ref-expansion cache (ref_id → raw `lookup_ref` result).
    /// Committing consumers take `reset_tvars()` copies; the concrete
    /// mass stays Arc-shared so repeated pairs are pruned by identity.
    expansions: Lazy<IntMap<usize, Type>>,
    /// Pure-probe pair memo: `contains_int` verdicts for empty-flag
    /// calls, keyed by both sides' content-Arc identities. Each entry
    /// pins both types so an address cannot be recycled under its key,
    /// and carries the `epoch` at insert: a committing call may bind a
    /// cell a verdict read, so the epoch bumps there.
    probe_pairs: Lazy<AHashMap<(NormKey, NormKey), (u64, bool)>>,
    probe_pins: Lazy<Vec<Type>>,
    /// A probe that depends on its own verdict claims nothing.
    distribution_probes_in_progress: SmallVec<[usize; 4]>,
    /// The typedefs a trait question is in progress for.
    traits_in_progress: SmallVec<[(usize, TraitId); 4]>,
    /// The shallowest in-progress pair the current verdict assumed.
    low_water: usize,
    epoch: u64,
    /// Per definition, on this path: the size of the params its latest
    /// expansion took, and how many expansions in a row grew them.
    unfolding: Lazy<IntMap<usize, (usize, u32)>>,
}

/// How many expansions of one typedef in a row may take larger params. A
/// pair met again ends a walk, so params that keep growing are a walk
/// that never ends (`type N<'a> = [null, ('a, N<Array<'a>>)]`); nested
/// applications of one alias shrink theirs.
const MAX_GROWTH: u32 = 64;

impl Deref for ContainsHist {
    type Target = RefHist<AHashMap<RefPair, usize>>;

    fn deref(&self) -> &Self::Target {
        &self.hist
    }
}

impl DerefMut for ContainsHist {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.hist
    }
}

impl ContainsHist {
    pub(super) fn new() -> Self {
        ContainsHist {
            hist: RefHist::new(),
            expansions: Lazy::new(),
            probe_pairs: Lazy::new(),
            probe_pins: Lazy::new(),
            distribution_probes_in_progress: SmallVec::new(),
            traits_in_progress: SmallVec::new(),
            low_water: usize::MAX,
            epoch: 0,
            unfolding: Lazy::new(),
        }
    }

    /// Cached pure-probe verdict for `(t0, t1)`, if current.
    fn probe_get(&self, t0: &Type, t1: &Type) -> Option<bool> {
        let k = (probe_key(t0)?, probe_key(t1)?);
        let (epoch, r) = self.probe_pairs.get()?.get(&k).copied()?;
        (epoch == self.epoch).then_some(r)
    }

    fn probe_put(&mut self, t0: &Type, t1: &Type, r: bool) {
        if let (Some(k0), Some(k1)) = (probe_key(t0), probe_key(t1))
            && self.probe_pairs.get_mut().insert((k0, k1), (self.epoch, r)).is_none()
        {
            let pins = self.probe_pins.get_mut();
            pins.push(t0.clone());
            pins.push(t1.clone());
        }
    }

    /// Decide `key` by `f`, assuming it holds meanwhile: a pair met
    /// again inside its own proof is `true` (coinduction), and the depth
    /// of that assumption lowers `low_water`.
    fn assuming(
        &mut self,
        key: RefPair,
        f: impl FnOnce(&mut Self) -> Result<bool>,
    ) -> Result<bool> {
        if let Some(&depth) = self.hist.get(&key) {
            self.low_water = self.low_water.min(depth);
            return Ok(true);
        }
        let depth = self.hist.len();
        self.hist.insert(key, depth);
        let r = f(self);
        self.hist.remove(&key);
        r
    }

    /// [`Type::lookup_ref_with`] through the expansion cache. A non-Ref,
    /// an unresolvable ref, or a ref with TVar params (its expansion
    /// embeds the caller's live cells) goes uncached. A probe (`!commit`)
    /// hands back the cached expansion itself; committing calls take
    /// `reset_tvars()` copies. `None` is a violated parameter bound
    /// under a probe.
    fn expand_ref(
        &mut self,
        t: &Type,
        id: Option<usize>,
        env: &Env,
        commit: bool,
    ) -> Result<Option<Type>> {
        let (Type::Ref(tr), Some(id)) = (t, id) else {
            return t.lookup_ref_with(env, commit);
        };
        if !tr.params.iter().all(|p| p.tvar_free()) {
            return t.lookup_ref_with(env, commit);
        }
        let fresh = |e: &Type| if commit { e.reset_tvars() } else { e.clone() };
        if let Some(e) = self.expansions.get().and_then(|m| m.get(&id)) {
            return Ok(Some(fresh(e)));
        }
        let Some(e) = t.lookup_ref_with(env, commit)? else { return Ok(None) };
        let r = fresh(&e);
        self.expansions.get_mut().insert(id, e);
        Ok(Some(r))
    }
}

/// A cell whose chain of bindings ends open is as free as the cell it
/// ends in.
fn is_unbound_tvar(t: &Type) -> bool {
    matches!(t, Type::TVar(_)) && t.with_deref(|d| d.is_none())
}

/// Is `a` an open cell that `b` reaches (`'r ⊇ fn(..) -> 'r`)?
/// A rigid cell never unifies, so it makes no cycle.
fn open_cell_reaches(a: &Type, b: &Type) -> bool {
    matches!(a, Type::TVar(tv) if !tv.is_bound() && !tv.is_rigid() && tv.would_cycle(b))
}

/// Does `t` reach a cell that is open, unconstrained and
/// `cycle_refused`? Pure read through bindings and constraints.
fn type_has_refused_open_cell(t: &Type) -> bool {
    fn walk(t: &Type, visited: &mut IntSet<usize>) -> bool {
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
                            cell.cycle_refused,
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

/// Whether every part of `t` is one type, so an open cell bound to a part
/// loses nothing (a union part would bind the cell to the whole union,
/// where it only had to stay within it). A variable is exact: binding
/// aliases the cell to it, constraints and all.
fn exact(t: &Type) -> bool {
    ensure_sufficient(|| match t {
        Type::Set(_) | Type::Any | Type::Ref(_) => false,
        Type::Primitive(p) => p.len() == 1,
        Type::TVar(tv) => tv.binding().is_none_or(|b| exact(&b)),
        t => {
            let mut all = true;
            t.for_each_child(&mut |c| all &= exact(c));
            all
        }
    })
}

/// Give the open cells inside `t`, about to be bound into the cell, their
/// part of each exact structural conjunct (`Array<i64> ⊇ Array<'y>` binds
/// `'y`): a probe admits them free, and the bind passes down only the
/// predicates.
fn commit_cell_constraints(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    for c in tv.cell_constraints().iter() {
        let predicate = c.is_predicate();
        if !predicate
            && !c.is_trait_ref(env)
            && exact(c)
            && !c.contains_int(flags, env, hist, t)?
        {
            return Ok(false);
        }
    }
    Ok(true)
}

/// True iff binding `t` into the cell would satisfy every conjunct of
/// the cell's constraints. A pure probe.
fn cell_constraints_ok(
    tv: &TVar,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    for c in tv.cell_constraints().iter() {
        if !c.contains_int(BitFlags::empty(), env, hist, t)? {
            return Ok(false);
        }
    }
    Ok(true)
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
/// discarded side's later binding never reaches the survivor. `false`
/// when two distinct rigid cells meet; only `commit` links.
fn link_equal(t0: &Type, t1: &Type, commit: bool) -> bool {
    ensure_sufficient(|| link_equal_inner(t0, t1, commit))
}

fn link_equal_inner(t0: &Type, t1: &Type, commit: bool) -> bool {
    let all = |a: &[Type], b: &[Type]| {
        a.iter().zip(b.iter()).all(|(x, y)| link_equal(x, y, commit))
    };
    match (t0, t1) {
        (Type::TVar(a), Type::TVar(b)) => {
            if a.same_cell(b) {
                return true;
            }
            match (a.binding(), b.binding()) {
                (None, None) => match OpenPair::of(a, b) {
                    OpenPair::Distinct => false,
                    act => !commit || act.apply(a, b),
                },
                (Some(x), Some(y)) => link_equal(&x, &y, commit),
                // Unreachable under an eq-true verdict.
                _ => true,
            }
        }
        (Type::Fn(f0), Type::Fn(f1)) => {
            f0.args
                .iter()
                .zip(f1.args.iter())
                .all(|(a, b)| link_equal(&a.typ, &b.typ, commit))
                && match (&f0.vargs, &f1.vargs) {
                    (Some(a), Some(b)) => link_equal(a, b, commit),
                    _ => true,
                }
                && link_equal(&f0.rtype, &f1.rtype, commit)
                && link_equal(&f0.throws, &f1.throws, commit)
        }
        (Type::Ref(r0), Type::Ref(r1)) => all(&r0.params, &r1.params),
        (Type::Set(a), Type::Set(b))
        | (Type::Tuple(a), Type::Tuple(b))
        | (Type::Variant(_, a, _), Type::Variant(_, b, _))
        | (Type::Abstract { params: a, .. }, Type::Abstract { params: b, .. }) => {
            all(a, b)
        }
        (Type::Struct(a), Type::Struct(b)) => {
            a.iter().zip(b.iter()).all(|((_, x, _), (_, y, _))| link_equal(x, y, commit))
        }
        (Type::Array(a), Type::Array(b))
        | (Type::List(a), Type::List(b))
        | (Type::Error(a), Type::Error(b)) => link_equal(a, b, commit),
        (Type::ByRef(m0, a), Type::ByRef(m1, b)) => m0 == m1 && link_equal(a, b, commit),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            link_equal(k0, k1, commit) && link_equal(v0, v1, commit)
        }
        (Type::App(c0, a0), Type::App(c1, a1)) => {
            link_equal(c0, c1, commit) && link_equal(a0, a1, commit)
        }
        // a bound variable compares through its binding, as union_identical
        // does, so the cells under it weld
        (Type::TVar(a), t) => match a.binding() {
            Some(x) => link_equal(&x, t, commit),
            None => true,
        },
        (t, Type::TVar(b)) => match b.binding() {
            Some(y) => link_equal(t, &y, commit),
            None => true,
        },
        _ => true,
    }
}

/// Two types identical up to `Fn` equality whose cells can be welded.
fn identical_linked(t0: &Type, t1: &Type, commit: bool) -> bool {
    union_identical(t0, t1)
        && link_equal(t0, t1, false)
        && (!commit || link_equal(t0, t1, true))
}

/// A non-containment report that formats lazily: callers probe these
/// errors, and rendering a large type eagerly is tree-cost.
#[derive(Debug)]
pub struct TypeMismatch {
    expected: Type,
    actual: Type,
}

impl std::fmt::Display for TypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
            write!(f, "type mismatch {} does not contain {}", self.expected, self.actual)
        })
    }
}

impl std::error::Error for TypeMismatch {}

/// Both nodes wrap the same content allocation, so containment holds
/// reflexively. `TVar`/`Ref` keep their full arms: cheap and delicate.
fn same_content(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Set(x), Type::Set(y)) | (Type::Tuple(x), Type::Tuple(y)) => {
            (**x).as_ptr() == (**y).as_ptr()
        }
        (Type::Struct(x), Type::Struct(y)) => (**x).as_ptr() == (**y).as_ptr(),
        (Type::Variant(t0, x, _), Type::Variant(t1, y, _)) => {
            t0 == t1 && (**x).as_ptr() == (**y).as_ptr()
        }
        (Type::Fn(x), Type::Fn(y)) => Arc::ptr_eq(x, y),
        (Type::Array(x), Type::Array(y))
        | (Type::List(x), Type::List(y))
        | (Type::Error(x), Type::Error(y)) => Arc::ptr_eq(x, y),
        (Type::ByRef(m0, x), Type::ByRef(m1, y)) => m0 == m1 && Arc::ptr_eq(x, y),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            Arc::ptr_eq(k0, k1) && Arc::ptr_eq(v0, v1)
        }
        _ => false,
    }
}

/// An open cell in `t` with the `Discernible` or `Ordered` conjunct, and
/// the stronger of the two it holds.
fn discernible_cell(t: &Type) -> Option<(TVar, Type)> {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => match tv.binding() {
            Some(b) => discernible_cell(&b),
            None => {
                let cons = tv.cell_constraints();
                let has = |b: Type| cons.contains(&b).then_some(b);
                has(Type::Ordered)
                    .or_else(|| has(Type::Discernible))
                    .map(|b| (tv.clone(), b))
            }
        },
        Type::Fn(_) => None,
        t => {
            let mut found = None;
            t.for_each_child(&mut |c| {
                if found.is_none() {
                    found = discernible_cell(c)
                }
            });
            found
        }
    })
}

/// The deref/expansion steps [`Type::set_covers_by_distribution`] takes
/// to reach a head constructor before giving up on a chain of aliases.
const HEAD_CHAIN_LIMIT: usize = 64;

impl Type {
    pub fn check_contains(&self, env: &Env, t: &Self) -> Result<()> {
        let ok = self.contains_int(
            ContainsFlags::Commit.into(),
            env,
            &mut ContainsHist::new(),
            t,
        )?;
        if graphix_dbg_bind() {
            eprintln!("CHK-CONTAINS {self} >= {t} -> {ok}");
        }
        if ok { Ok(()) } else { Err(self.contains_mismatch(env, t)) }
    }

    // CR claude for claude: [readability] When a trait bound refuses a type, the error is
    // a bare mismatch against the bounded cell. With no `impl Show for i64`,
    // `Show::show(2)` gives "type mismatch 'self: unbound within Show does not contain
    // i64", which never says that i64 lacks an impl. When the impl exists in a sibling
    // module whose .gxi does not declare it (Env::impls_of hides it from the other
    // modules' checks), the message is the same, and nothing hints that `impl t::Show
    // for i64;` in that .gxi fixes it. When the refusing side is a cell with trait
    // conjuncts, say "i64 does not implement Show", naming the member without an impl
    // for a union self and the module of a hidden impl that would match. In run mode
    // the module context also shows the script's synthetic block scope ("compiling
    // module #do4611686018427394750::b", node/module.rs:771), where --check prints
    // "compiling module b". (x-typecheck-patterns-14)
    // 2026-10-07 claude: trait_refusal names the member without an impl and a hidden
    // impl's module (pinned in lang::traits). The run-mode module name ("#do..::b")
    // still differs from --check's; that is c-analysis-branch-11's naming.
    fn contains_mismatch(&self, env: &Env, t: &Self) -> anyhow::Error {
        // A refused open cell on either side is the infinite type,
        // surfacing at a consumer; report it as the settle path does.
        if type_has_refused_open_cell(self)
            || type_has_refused_open_cell(t)
            || open_cell_reaches(self, t)
            || open_cell_reaches(t, self)
        {
            return anyhow::anyhow!("{INFINITE_TYPE_MSG}");
        }
        if let Some(e) = self.discernible_refusal(env, t) {
            return e;
        }
        if let Some(e) = self.trait_refusal(env, t) {
            return e;
        }
        anyhow::Error::new(TypeMismatch { expected: self.clone(), actual: t.clone() })
    }

    /// An open cell bounded by a trait refuses a type with a member that
    /// has no impl of it: name the member, and an impl a sibling module
    /// holds but leaves out of its interface.
    fn trait_refusal(&self, env: &Env, t: &Self) -> Option<anyhow::Error> {
        let Self::TVar(tv) = self else { return None };
        if tv.binding().is_some() {
            return None;
        }
        let mut hist = ContainsHist::new();
        for c in tv.cell_constraints().iter() {
            let Self::Ref(r) = c else { continue };
            let Some(tid) = env.trait_of_ref(r) else { continue };
            let Some(def) = env.trait_def(tid) else { continue };
            let mut found: SmallVec<[Type; 8]> = SmallVec::new();
            crate::expr::union_members(env, t, &mut found).ok()?;
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            for m in found.drain(..) {
                match m {
                    Self::Primitive(p) => {
                        members.extend(p.iter().map(|b| Self::Primitive(b.into())))
                    }
                    m => members.push(m),
                }
            }
            let seen = env.impls_of(tid).unwrap_or_default();
            for m in members.iter() {
                if Self::trait_contains(tid, BitFlags::empty(), env, &mut hist, m).ok()? {
                    continue;
                }
                let name = &def.name;
                let hidden = env.impls.get(&tid).and_then(|l| {
                    l.iter()
                        .find(|im| {
                            !seen.iter().any(|s| Arc::ptr_eq(s, im))
                                && env.heads_overlap(&im.target, m).unwrap_or(false)
                        })
                        .cloned()
                });
                return Some(match hidden {
                    None => anyhow::anyhow!("{m} does not implement {name}"),
                    Some(im) => anyhow::anyhow!(
                        "{m} does not implement {name} here: the impl in module {} is \
                         not in its interface (declare `impl {name} for {m};` there)",
                        im.scope
                    ),
                });
            }
        }
        None
    }

    /// Why `self ⊇ t` fails when `self` holds an open `Discernible` or
    /// `Ordered` cell and `t` is not that.
    pub fn discernible_refusal(&self, env: &Env, t: &Self) -> Option<anyhow::Error> {
        let (tv, bound) = discernible_cell(self)?;
        let why = t.indiscernible(env, Open::Benign, bound == Type::Ordered)?;
        Some(t.not_discernible(&bound, &tv.name, &why))
    }

    /// [`Self::check_contains`] with rigid enforcement; the def gate's
    /// acceptance checks only.
    pub fn check_contains_rigid(&self, env: &Env, t: &Self) -> Result<()> {
        let flags = ContainsFlags::Commit | ContainsFlags::RigidCheck;
        let ok = self.contains_int(flags, env, &mut ContainsHist::new(), t)?;
        if ok { Ok(()) } else { Err(self.contains_mismatch(env, t)) }
    }

    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        ensure_sufficient(|| self.contains_int_inner(flags, env, hist, t))
    }

    fn contains_int_inner(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        if (self as *const Type) == (t as *const Type) || same_content(self, t) {
            return Ok(true);
        }
        if flags.contains(ContainsFlags::Commit) {
            // A committing call may bind a cell a cached verdict read.
            hist.epoch += 1;
            return self.contains_dispatch(flags, env, hist, t);
        }
        if !flags.is_empty() {
            return self.contains_dispatch(flags, env, hist, t);
        }
        if let Some(r) = hist.probe_get(self, t) {
            return Ok(r);
        }
        // Only a verdict that assumed no pair from further out holds
        // outside this call.
        let height = hist.len();
        let outer = mem::replace(&mut hist.low_water, usize::MAX);
        let r = self.contains_dispatch(flags, env, hist, t);
        let assumed = hist.low_water;
        hist.low_water = outer.min(assumed);
        let r = r?;
        if assumed >= height {
            hist.probe_put(self, t, r);
        }
        Ok(r)
    }

    fn contains_dispatch(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        let commit = flags.contains(ContainsFlags::Commit);
        let rigid = flags.contains(ContainsFlags::RigidCheck);
        // A trait in type position is a predicate, reached only as a
        // cell conjunct.
        if let Self::Ref(tr) = self
            && let Some(tid) = env.trait_of_ref(tr)
        {
            return Self::trait_contains(tid, flags, env, hist, t);
        }
        if let Self::Ref(tr) = t
            && let Some(tid) = env.trait_of_ref(tr)
        {
            return Ok(match self {
                Self::Any => true,
                Self::Ref(tr0) => env.trait_of_ref(tr0) == Some(tid),
                _ => false,
            });
        }
        // A constructor application decomposes a reference by name,
        // ahead of the expansion arm.
        if matches!(
            (self, t),
            (Self::App(..), Self::Ref(_)) | (Self::Ref(_), Self::App(..))
        ) {
            return self.app_contains(flags, env, hist, t);
        }
        // A cell bound to a reference meets a reference by name before
        // either expands.
        if let (Self::Ref(_), Self::TVar(_)) = (self, t)
            && let Some(behind) = t.ref_behind()
        {
            return self.contains_int(flags, env, hist, &behind);
        }
        if let (Self::TVar(_), Self::Ref(_)) = (self, t)
            && let Some(behind) = self.ref_behind()
        {
            return behind.contains_int(flags, env, hist, t);
        }
        match (self, t) {
            (Self::Concrete, t) => Ok(t.concrete_holds()),
            (_, Self::Concrete) => Ok(false),
            (Self::Function, t) => t.function_holds(env, commit),
            (_, Self::Function) => Ok(false),
            (Self::Singleton, t) => t.singleton_holds(env, commit),
            (_, Self::Singleton) => Ok(false),
            (Self::OneNumber, t) => t.one_number_holds(env, commit),
            (_, Self::OneNumber) => Ok(false),
            (Self::Discernible, t) => {
                Ok(t.indiscernible(env, Open::Benign, false).is_none())
            }
            (_, Self::Discernible) => Ok(false),
            (Self::Ordered, t) => Ok(t.indiscernible(env, Open::Benign, true).is_none()),
            (_, Self::Ordered) => Ok(false),
            (Self::Hole, Self::Hole) => Ok(true),
            (Self::Hole, Self::TVar(tv)) => match tv.binding() {
                Some(b) => Self::Hole.contains_int(flags, env, hist, &b),
                None => Ok(false),
            },
            (Self::TVar(tv), Self::Hole) => match tv.binding() {
                Some(b) => b.contains_int(flags, env, hist, &Self::Hole),
                None => Ok(false),
            },
            (Self::Hole, _) | (_, Self::Hole) => Ok(false),
            // A reference is contained by itself with identical params
            // (whatever each param's variance). Two filled cells can
            // hold different defs for one name; disagreement, like any
            // other pair, takes the expansion arm.
            (Self::Ref(tr0), Self::Ref(tr1))
                if tr0.scope == tr1.scope
                    && tr0.name == tr1.name
                    && tr0.cells_agree(tr1)
                    && tr0.params.len() == tr1.params.len()
                    && tr0
                        .params
                        .iter()
                        .zip(tr1.params.iter())
                        .all(|(a, b)| identical_linked(a, b, commit)) =>
            {
                Ok(true)
            }
            (t0 @ Self::Ref(_), t1) | (t0, t1 @ Self::Ref(_)) => {
                // CR claude for claude: [bug] This memo keys a reference on its
                // definition and its params. A typedef whose params grow as it recurses
                // never meets a repeated pair, so comparing two different
                // instantiations of it unfolds forever. Example: `type N<'a> = [null,
                // ('a, N<Array<'a>>)]`, which the cast_to_a_growing_definition pin
                // treats as legal. `let n1: N<i64> = null; let n2: N<[i64, string]> =
                // n1` is a valid widening, and it hangs `--check`; so does the
                // `N<string>` form, which should be refused. The language server checks
                // inside its message loop, so such a file freezes it. Contractiveness
                // in Env::deftype makes this memo sound but does not bound it, and
                // nothing here plays the role of fusion's MAX_FREEZE_EXPANSIONS; probe:
                // design/review-2026-10-05/repro/t-contains-09.gx (t-contains-09)
                // 2026-10-07 claude: no longer hangs: past MAX_GROWTH growing expansions of
                // one definition on a path the comparison is refused ("cannot compare
                // two instances of N"). The valid widening N<[i64, string]> := N<i64>
                // is refused that way too; deciding it needs the parameters' variance.
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                // one entry per definition, at its larger params
                let defs = match [t0, t1].map(|t| match t {
                    Self::Ref(tr) => tr.def_key().map(|d| (d, params_size(&tr.params))),
                    _ => None,
                }) {
                    [Some((d0, s0)), Some((d1, s1))] if d0 == d1 => {
                        [Some((d0, s0.max(s1))), None]
                    }
                    defs => defs,
                };
                let mut saved: SmallVec<[(usize, Option<(usize, u32)>); 2]> =
                    SmallVec::new();
                let restore =
                    |hist: &mut ContainsHist, saved: &[(usize, Option<(usize, u32)>)]| {
                        for (d, old) in saved.iter().rev() {
                            match old {
                                Some(old) => {
                                    hist.unfolding.get_mut().insert(*d, *old);
                                }
                                None => {
                                    hist.unfolding.get_mut().remove(d);
                                }
                            }
                        }
                    };
                for (d, size) in defs.iter().flatten() {
                    let old = hist.unfolding.get().and_then(|m| m.get(d)).copied();
                    let growth = match old {
                        Some((last, g)) if *size > last => g + 1,
                        _ => 0,
                    };
                    saved.push((*d, old));
                    hist.unfolding.get_mut().insert(*d, (*size, growth));
                    if growth > MAX_GROWTH {
                        restore(hist, &saved);
                        let name = match (t0, t1) {
                            (Self::Ref(tr), _) | (_, Self::Ref(tr)) => tr.name.clone(),
                            _ => unreachable!("a definition key is a reference's"),
                        };
                        bail!(
                            "cannot compare two instances of {name}: its parameters \
                             grow at every level of its recursion, so no comparison ends"
                        )
                    }
                }
                let r = hist.assuming(key, |hist| {
                    let Some(e0) = hist.expand_ref(t0, key.0, env, commit)? else {
                        return Ok(false);
                    };
                    let Some(e1) = hist.expand_ref(t1, key.1, env, commit)? else {
                        return Ok(false);
                    };
                    e0.contains_int(flags, env, hist, &e1)
                });
                restore(hist, &saved);
                r
            }
            // ⊥ fits whatever the cell becomes; binding would only
            // foreclose its writers. The cell remembers it was fed ⊥.
            (Self::TVar(t0), Self::Bottom) => {
                if commit && t0.binding().is_none() {
                    t0.mark_bottom_fed();
                }
                Ok(true)
            }
            // ⊥ ⊇ 'r has one solution, so an open cell commits; a bound
            // cell answers for its binding.
            (Self::Bottom, Self::TVar(t0)) => match t0.binding() {
                Some(b) => Self::Bottom.contains_int(flags, env, hist, &b),
                None => {
                    if commit {
                        t0.bind_bottom_required();
                    }
                    Ok(true)
                }
            },
            (Self::Bottom, Self::Bottom) => Ok(true),
            // a constructor applied over open cells: each was fed ⊥
            (t0 @ Self::App(..), Self::Bottom) => {
                if commit {
                    let mut tvs: LPooled<AHashMap<arcstr::ArcStr, TVar>> =
                        LPooled::take();
                    t0.collect_tvars(&mut tvs);
                    for tv in tvs.values().filter(|tv| tv.binding().is_none()) {
                        tv.mark_bottom_fed();
                    }
                }
                Ok(true)
            }
            (Self::Bottom, _) => Ok(false),
            (_, Self::Bottom) => Ok(true),
            (Self::TVar(t0), Self::Any) => {
                // Clone the binding out before recursing, here and in
                // every deref arm: the walk can revisit this cell and
                // write-lock it, and the locks are non-reentrant.
                if let Some(t0) = t0.binding() {
                    return t0.contains_int(flags, env, hist, t);
                }
                // A rigid cell contains only itself and Bottom.
                if (rigid || commit) && t0.is_rigid() {
                    return Ok(false);
                }
                if !cell_constraints_ok(t0, env, hist, &Self::Any)? {
                    return Ok(false);
                }
                // A rigid cell is never written outside the acceptance
                // judgment.
                if commit && !t0.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!("BIND lhs '{}({:x}) := Any", t0.name, t0.cell_addr());
                    }
                    t0.bind(Self::Any);
                }
                Ok(true)
            }
            (Self::Any, _) => Ok(true),
            (
                Self::Abstract { id: id0, params: p0 },
                Self::Abstract { id: id1, params: p1 },
            ) => {
                if id0 != id1 {
                    return Ok(false);
                }
                // CR claude for eric: [bug] Abstract parameters are checked
                // covariantly, whatever the representation (or a Rust-backed handle)
                // does with them. With `type Sink<'a> = Abstract<fn(x: 'a) -> i64>`, a
                // `Sink<i64>` is accepted as a `Sink<[i64, string]>`, so a string
                // reaches `|x: i64|`. Likewise a helper taking `db::Tree<i64, [i64,
                // string]>` accepts a `db::Tree<i64, i64>` and stores a string in the
                // i64 tree. The JIT then reads the string's pointer as an i64 or panics
                // at fusion/kernel.rs:243, while the transparent `type Sink<'a> = fn(x:
                // 'a) -> i64` is refused. Make abstract parameters invariant (check
                // both directions), or record a variance per parameter at Env::deftype
                // with Rust-backed types invariant. The open-constructor case in
                // app_contains (`'c<a0> ⊇ 'c<a1>` by a0 ⊇ a1) is the same rule and
                // needs the same fix. probe:
                // design/review-2026-10-05/repro/t-contains-07.gx (t-contains-07)
                Ok(p0.len() == p1.len()
                    && p0
                        .iter()
                        .zip(p1.iter())
                        .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            (Self::Primitive(p0), Self::Primitive(p1)) => Ok(p0.contains(*p1)),
            (Self::Primitive(p), Self::Array(_) | Self::Tuple(_) | Self::Struct(_)) => {
                Ok(p.contains(Typ::Array))
            }
            // a bare variant is a string at run time
            (Self::Primitive(p), Self::Variant(_, args, _)) => {
                Ok(!args.is_empty() && p.contains(Typ::Array))
            }
            (Self::Array(t0), Self::Array(t1)) => t0.contains_int(flags, env, hist, t1),
            (Self::List(t0), Self::List(t1)) => t0.contains_int(flags, env, hist, t1),
            (
                Self::List(_),
                Self::Primitive(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Struct(_)
                | Self::Variant(_, _, _)
                | Self::Error(_)
                | Self::Map { .. },
            )
            | (
                Self::Primitive(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Struct(_)
                | Self::Variant(_, _, _)
                | Self::Error(_)
                | Self::Map { .. },
                Self::List(_),
            ) => Ok(false),
            (Self::Array(t0), Self::Primitive(p)) if *p == BitFlags::from(Typ::Array) => {
                t0.contains_int(flags, env, hist, &Type::Any)
            }
            (Self::Map { key: k0, value: v0 }, Self::Map { key: k1, value: v1 }) => {
                Ok(k0.contains_int(flags, env, hist, k1)?
                    && v0.contains_int(flags, env, hist, v1)?)
            }
            (Self::Primitive(p), Self::Map { .. }) => Ok(p.contains(Typ::Map)),
            (Self::Map { key, value }, Self::Primitive(p))
                if *p == BitFlags::from(Typ::Map) =>
            {
                Ok(key.contains_int(flags, env, hist, &Type::Any)?
                    && value.contains_int(flags, env, hist, &Type::Any)?)
            }
            (Self::Primitive(p0), Self::Error(_)) => Ok(p0.contains(Typ::Error)),
            (Self::Error(e), Self::Primitive(p)) if *p == BitFlags::from(Typ::Error) => {
                e.contains_int(flags, env, hist, &Type::Any)
            }
            (Self::Error(e0), Self::Error(e1)) => e0.contains_int(flags, env, hist, e1),
            (Self::Tuple(t0), Self::Tuple(t1)) => Ok(t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::Struct(t0), Self::Struct(t1)) => {
                Ok(t0.len() == t1.len() && {
                    // Struct fields are sorted by name.
                    t0.iter()
                        .zip(t1.iter())
                        .map(|((n0, t0, _), (n1, t1, _))| {
                            Ok(n0 == n1 && t0.contains_int(flags, env, hist, t1)?)
                        })
                        .collect::<Result<AndAc>>()?
                        .0
                })
            }
            (Self::Variant(tg0, t0, _), Self::Variant(tg1, t1, _)) => Ok(tg0 == tg1
                && t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            // XCR claude for eric: [bug] This arm makes references covariant (`&[i64,
            // string]` holds `&i64`). A reference is writable, and
            // ConnectDeref::typecheck0_with (node/mod.rs:2361) checks `*r <- v` only
            // against r's own type. So a program that passes the check writes a string
            // or null into an `i64` binding: the JIT then panics at
            // fusion/kernel.rs:243 and the runtime dies, while the node-walk computes
            // on the wrong type. No annotation is needed: `let set = |v: 'a, r: &'a| *r
            // <- v` called as `set(n, &x)` with `n: [i64, null]` and `x = 1` passes.
            // The same call with the reference first, `set(&x, n)`, is refused, so this
            // arm undoes callsite.rs::Widening's rule that a reference keeps the first
            // argument's type. Plain invariance would also refuse the read-only
            // widenings the stdlib relies on (`#title: &"Chart"` into `&[string,
            // null]`, tui browser.gx:156), so the fix needs a design choice; probe:
            // design/review-2026-10-05/repro/c-node-mod-01.gx (c-node-mod-01)
            // 2026-10-06 claude: references split into `&T` (read-only, covariant) and
            // `&mut T` (writable, invariant); `*r <- v` needs every reference r may hold
            // to be `&mut` (ConnectDeref::typecheck0_with). The probe is refused.
            // design/place_references.md has the rules.
            // `&T` only reads, so it is covariant, and a `&mut` is one; `&mut T`
            // also writes, so it is invariant.
            (Self::ByRef(Mutability::Shared, t0), Self::ByRef(_, t1)) => {
                t0.contains_int(flags, env, hist, t1)
            }
            (Self::ByRef(Mutability::Mut, t0), Self::ByRef(Mutability::Mut, t1)) => {
                Ok(t0.contains_int(flags, env, hist, t1)?
                    && t1.contains_int(flags, env, hist, t0)?)
            }
            (Self::ByRef(Mutability::Mut, _), Self::ByRef(Mutability::Shared, _)) => {
                Ok(false)
            }
            // Two vars sharing one cell are already unified; the cycle
            // guard below would otherwise poison both.
            (Self::TVar(t0), Self::TVar(t1))
                if t0.wrapper_addr() == t1.wrapper_addr()
                    || t0.read().id == t1.read().id
                    || t0.same_cell(t1) =>
            {
                Ok(true)
            }
            (tt0 @ Self::TVar(t0), tt1 @ Self::TVar(t1)) => {
                Self::contains_tvars(flags, env, hist, (tt0, t0), (tt1, t1))
            }
            // Deref first: a bound cell answers for its binding. The
            // occurs check guards the open cell's bind; an open cell
            // `t1` reaches (the μ-shape `'r ⊇ [T, 'r]`) takes the arms
            // below, where the union collapses.
            (Self::TVar(t0), t1) if t0.is_bound() || !t0.would_cycle(t1) => {
                if let Some(t0) = t0.binding() {
                    return t0.contains_int(flags, env, hist, t1);
                }
                // A rigid tvar contains only itself and Bottom.
                if (rigid || commit) && t0.is_rigid() {
                    return Ok(false);
                }
                // A constraint violation fails here, at the site that
                // tried it.
                if !cell_constraints_ok(t0, env, hist, t1)? {
                    return Ok(false);
                }
                if commit && !t0.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "BIND lhs '{}({:x}) := {t1:?}",
                            t0.name,
                            t0.cell_addr()
                        );
                    }
                    if !commit_cell_constraints(t0, flags, env, hist, t1)? {
                        return Ok(false);
                    }
                    t0.bind(t1.clone());
                }
                Ok(true)
            }
            (t0, Self::TVar(t1)) if t1.is_bound() || !t1.would_cycle(t0) => {
                if let Some(t1) = t1.binding() {
                    return t0.contains_int(flags, env, hist, &t1);
                }
                // t0 contains an arbitrary 'a only when it contains one
                // of the cell's conjuncts ('a ⊆ C ⊆ t0) or, a union, one
                // member holds it; a commit cannot bind the cell, so it
                // takes this verdict too.
                if (rigid || commit) && t1.is_rigid() {
                    if let Self::Set(s) = t0
                        && Self::set_commit(s, flags, env, hist, t)?.unwrap_or(false)
                    {
                        return Ok(true);
                    }
                    for c in t1.cell_constraints().iter() {
                        // probed first: a failed flagged check would leave
                        // bindings behind; the one that holds commits, so
                        // t0's open cells take their part of the conjunct
                        if t0.contains_int(BitFlags::empty(), env, hist, c)? {
                            return match commit {
                                true => t0.contains_int(flags, env, hist, c),
                                false => Ok(true),
                            };
                        }
                    }
                    return Ok(false);
                }
                if !cell_constraints_ok(t1, env, hist, t0)? {
                    // a union the cell's constraints refuse whole may
                    // still hold it in one member: `['b, null] ⊇ 'r`
                    // with `'r` within `Array<_>`, by `'b`
                    if let Self::Set(s) = t0 {
                        return Ok(
                            Self::set_commit(s, flags, env, hist, t)?.unwrap_or(false)
                        );
                    }
                    // a t0 wider than the cell's witness holds it
                    // ('a ⊆ W ⊆ t0): the cell settles to W
                    let Ok(w) = t1.witness(env)? else { return Ok(false) };
                    if !t0.contains_int(BitFlags::empty(), env, hist, &w)? {
                        return Ok(false);
                    }
                    if !commit || t1.is_rigid() {
                        return Ok(true);
                    }
                    t1.bind(w.clone());
                    return t0.contains_int(flags, env, hist, &w);
                }
                if commit && !t1.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "BIND rhs '{}({:x}) := {t0:?}",
                            t1.name,
                            t1.cell_addr()
                        );
                    }
                    if !commit_cell_constraints(t1, flags, env, hist, t0)? {
                        return Ok(false);
                    }
                    t1.bind(t0.clone());
                }
                Ok(true)
            }
            (t0 @ Self::Set(_), t1 @ Self::Set(_))
                if identical_linked(t0, t1, commit) =>
            {
                Ok(true)
            }
            // A set with a bare unbound tvar member binds the tvar to
            // the residue (the rhs members no concrete lhs member
            // covers) in one act; per-member it would capture greedily.
            (t0 @ Self::Set(s0), Self::Set(s1)) if s0.iter().any(is_unbound_tvar) => {
                // the members free on entry: covering an rhs member may
                // bind one (`Array<'b> ⊇ Array<i64>`) before the residue
                // reaches it
                let free: SmallVec<[bool; 8]> = s0.iter().map(is_unbound_tvar).collect();
                let probe = BitFlags::empty();
                let mut residue: LPooled<Vec<Type>> = LPooled::take();
                for m in s1.iter() {
                    // An rhs member equal to the whole lhs set is
                    // covered reflexively; as residue it would close a
                    // cycle.
                    let reflexive = m
                        .deref_cloned()
                        .is_some_and(|md| identical_linked(t0, &md, commit));
                    if reflexive {
                        continue;
                    }
                    // An rhs member that is one of the lhs's own cells
                    // is covered reflexively too.
                    let own_cell = match m {
                        Self::TVar(mtv) => s0.iter().any(|c| match c {
                            Self::TVar(ctv) => ctv.same_cell(mtv),
                            _ => false,
                        }),
                        _ => false,
                    };
                    if own_cell {
                        continue;
                    }
                    // A free rhs member is residue too: the coverage
                    // loop would bind it greedily; in the residue it
                    // aliases with the bare lhs member.
                    if is_unbound_tvar(m) {
                        residue.push(m.clone());
                        continue;
                    }
                    let mut covered = false;
                    // CR claude for claude: [bug] This loop commits the first
                    // non-variable member that covers an rhs member before the residue
                    // reaches the bare variable. So `['b, Array<'b>] ⊇ [Array<i64>,
                    // Array<Array<i64>>]` binds 'b := i64 through `Array<'b> ⊇
                    // Array<i64>` and then cannot place `Array<Array<i64>>`, though 'b
                    // := Array<i64> covers both. InstanceTypes::new
                    // (node/lambda.rs:402) meets exactly this union for every
                    // array::flat_map or list::flat_map instance whose callback returns
                    // an array of arrays. So `array::flat_map([1, 2], |x| [[x]])`,
                    // `array::flat_map(groups, |g| g)`, a generic wrapper over flat_map
                    // and the book's Bag impl pass --check and the LSP but fail to
                    // build with no reason given; GRAPHIX_NO_SUBST=1 runs them. Making
                    // this containment succeed also admits a callback whose return is
                    // that union itself (`|x| select x { 1 => [x], n => [[n]] }`,
                    // refused today), and the runtime splices its bare-array member, so
                    // decide the fix together with flat_map's signature
                    // (x-engine-collections-03). probe:
                    // design/review-2026-10-05/repro/x-engine-collections-02.gx
                    // (x-engine-collections-02)
                    // 2026-10-07 claude: flat_map is parametric now (x-engine-collections-03), so no
                    // instance meets this union and the probe runs ([[1], [2]]). The greedy commit
                    // stands for a union a program writes with a bare variable beside a constructor of
                    // it; fixing it needs the commit to try the bare member before the constructor
                    // when the constructor's binding cannot place the rest.
                    for (c, _) in s0.iter().zip(&free).filter(|(_, free)| !**free) {
                        if c.contains_int(probe, env, hist, m)? {
                            if !c.contains_int(flags, env, hist, m)? {
                                return Ok(false);
                            }
                            covered = true;
                            break;
                        }
                    }
                    if !covered {
                        residue.push(m.clone());
                    }
                }
                if residue.is_empty() {
                    return Ok(true);
                }
                let target = if residue.len() == 1 {
                    residue[0].clone()
                } else {
                    Type::Set(Arc::from_iter(residue.drain(..)))
                };
                let target = target.normalize();
                let bare = s0.iter().zip(&free).find(|(_, free)| **free);
                match bare {
                    Some((tv_m, _)) => tv_m.contains_int(flags, env, hist, &target),
                    None => Ok(false),
                }
            }
            // Member-wise identity pre-pass: only the residue takes the
            // general per-member walk (which is O(|s0|·|s1|) per level).
            (t0 @ Self::Set(s0), Self::Set(s1)) => {
                for m in s1.iter() {
                    if !s0.iter().any(|c| identical_linked(c, m, commit))
                        && !t0.contains_int(flags, env, hist, m)?
                    {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (t0, Self::Set(s)) => Ok(s
                .iter()
                .map(|t1| t0.contains_int(flags, env, hist, t1))
                .collect::<Result<AndAc>>()?
                .0),
            (Self::Set(s), t) => {
                if graphix_dbg_bind() {
                    eprintln!("SET-T {} >= {t}", Self::Set(s.clone()));
                }
                // a member that is `t` covers it and binds nothing
                if s.iter().any(|m| identical_linked(m, t, commit)) {
                    return Ok(true);
                }
                match t {
                    // Prims first: the narrowest TVar bindings. The concrete
                    // members take the bits they cover one at a time; the
                    // rest go to a bare open member as one residue.
                    Self::Primitive(p) if p.len() > 1 => {
                        let concrete: SmallVec<[Type; 8]> =
                            s.iter().filter(|m| !is_unbound_tvar(m)).cloned().collect();
                        let bare = s.iter().find(|m| is_unbound_tvar(m));
                        let mut uncovered = BitFlags::empty();
                        for b in p.iter() {
                            let bit = Self::Primitive(b.into());
                            if !Self::set_admits(&concrete, env, hist, &bit)? {
                                uncovered |= b
                            }
                        }
                        if uncovered.is_empty() || bare.is_some() {
                            for b in (*p & !uncovered).iter() {
                                let bit = Self::Primitive(b.into());
                                if Self::set_commit(&concrete, flags, env, hist, &bit)?
                                    != Some(true)
                                {
                                    return Ok(false);
                                }
                            }
                            return match bare {
                                Some(m) if !uncovered.is_empty() => m.contains_int(
                                    flags,
                                    env,
                                    hist,
                                    &Self::Primitive(uncovered),
                                ),
                                _ => Ok(true),
                            };
                        }
                    }
                    _ => (),
                }
                match Self::set_commit(s, flags, env, hist, t)? {
                    Some(r) => Ok(r),
                    None => Self::set_covers_by_distribution(flags, env, hist, s, t),
                }
            }
            (Self::Fn(f0), Self::Fn(f1)) => {
                let r = f0.contains_int(flags, env, hist, f1)?;
                if r && commit {
                    f0.lambda_ids.link(&f1.lambda_ids);
                }
                Ok(r)
            }
            (Self::App(..), _) | (_, Self::App(..)) => {
                self.app_contains(flags, env, hist, t)
            }
            (Self::Abstract { .. }, _) | (_, Self::Abstract { .. }) => Ok(false),
            (_, Self::Any)
            | (_, Self::TVar(_))
            | (Self::TVar(_), _)
            | (Self::Fn(_), _)
            | (Self::ByRef(..), _)
            | (_, Self::ByRef(..))
            | (_, Self::Fn(_))
            | (Self::Tuple(_), Self::Array(_))
            | (Self::Tuple(_), Self::Primitive(_))
            | (Self::Tuple(_), Self::Struct(_))
            | (Self::Tuple(_), Self::Variant(_, _, _))
            | (Self::Tuple(_), Self::Error(_))
            | (Self::Tuple(_), Self::Map { .. })
            | (Self::Array(_), Self::Primitive(_))
            | (Self::Array(_), Self::Tuple(_))
            | (Self::Array(_), Self::Struct(_))
            | (Self::Array(_), Self::Variant(_, _, _))
            | (Self::Array(_), Self::Error(_))
            | (Self::Array(_), Self::Map { .. })
            | (Self::Struct(_), Self::Array(_))
            | (Self::Struct(_), Self::Primitive(_))
            | (Self::Struct(_), Self::Tuple(_))
            | (Self::Struct(_), Self::Variant(_, _, _))
            | (Self::Struct(_), Self::Error(_))
            | (Self::Struct(_), Self::Map { .. })
            | (Self::Variant(_, _, _), Self::Array(_))
            | (Self::Variant(_, _, _), Self::Struct(_))
            | (Self::Variant(_, _, _), Self::Primitive(_))
            | (Self::Variant(_, _, _), Self::Tuple(_))
            | (Self::Variant(_, _, _), Self::Error(_))
            | (Self::Variant(_, _, _), Self::Map { .. })
            | (Self::Error(_), Self::Array(_))
            | (Self::Error(_), Self::Primitive(_))
            | (Self::Error(_), Self::Struct(_))
            | (Self::Error(_), Self::Variant(_, _, _))
            | (Self::Error(_), Self::Tuple(_))
            | (Self::Error(_), Self::Map { .. })
            | (Self::Map { .. }, Self::Array(_))
            | (Self::Map { .. }, Self::Primitive(_))
            | (Self::Map { .. }, Self::Struct(_))
            | (Self::Map { .. }, Self::Variant(_, _, _))
            | (Self::Map { .. }, Self::Tuple(_))
            | (Self::Map { .. }, Self::Error(_)) => Ok(false),
        }
    }

    /// Two distinct cells. Occurs checks run only where a cell would
    /// bind or alias: an open cell meeting a bound cell whose binding
    /// reaches it is the μ-shape through a binding (a copy would bind
    /// the infinite type, so the binding is walked, where `'r ⊇ [T,
    /// 'r]` collapses); any other cycle is refused.
    fn contains_tvars(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        (tt0, t0): (&Type, &TVar),
        (tt1, t1): (&Type, &TVar),
    ) -> Result<bool> {
        let commit = flags.contains(ContainsFlags::Commit);
        let rigid = flags.contains(ContainsFlags::RigidCheck);
        let (addr0, addr1) = (t0.cell_addr(), t1.cell_addr());
        let cyc0 = || would_cycle_inner(addr0, tt1);
        let cyc1 = || would_cycle_inner(addr1, tt0);
        let refuse = || {
            if graphix_dbg_cycle_bt() {
                eprintln!(
                    "CYCLE-REFUSED-PAIR ({addr0:x},{addr1:x})\n{}",
                    std::backtrace::Backtrace::force_capture()
                );
            }
            t0.mark_cycle_refused();
            t1.mark_cycle_refused();
            Ok(true)
        };
        match (t0.binding(), t1.binding()) {
            // Two bound cells decide by walking the bindings; every bind
            // is occurs-checked, and the pair memo bounds a walk that
            // meets the pair again.
            (Some(b0), Some(b1)) => hist.assuming((Some(addr0), Some(addr1)), |hist| {
                b0.contains_int(flags, env, hist, &b1)
            }),
            (None, Some(b1)) => {
                if cyc0() {
                    return tt0.contains_int(flags, env, hist, &b1);
                }
                if cyc1() {
                    return refuse();
                }
                // A rigid receiver must not bind; re-verdict against
                // the binding.
                if (rigid || commit) && t0.is_rigid() {
                    return tt0.contains_int(flags, env, hist, &b1);
                }
                if commit && !t0.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "TT-LEFTCOPY '{} <= '{}",
                            t0.read().id.inner(),
                            t1.read().id.inner()
                        );
                    }
                    if !cell_constraints_ok(t0, env, hist, &b1)?
                        || !commit_cell_constraints(t0, flags, env, hist, &b1)?
                    {
                        return Ok(false);
                    }
                    t0.copy(t1, b1);
                }
                Ok(true)
            }
            (Some(b0), None) => {
                if cyc1() {
                    return b0.contains_int(flags, env, hist, tt1);
                }
                if cyc0() {
                    return refuse();
                }
                if (rigid || commit) && t1.is_rigid() {
                    return b0.contains_int(flags, env, hist, tt1);
                }
                if commit && !t1.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "TT-RIGHTCOPY '{} <= '{}",
                            t1.read().id.inner(),
                            t0.read().id.inner()
                        );
                    }
                    if !cell_constraints_ok(t1, env, hist, &b0)?
                        || !commit_cell_constraints(t1, flags, env, hist, &b0)?
                    {
                        return Ok(false);
                    }
                    t1.copy(t0, b0);
                }
                Ok(true)
            }
            // two quantifiers: t0 holds t1 only through one of t1's
            // conjuncts ('t1 ⊆ C ⊆ t0), whatever an occurs check would say
            (None, None) if t0.is_rigid() && t1.is_rigid() => {
                let cons = t1.cell_constraints();
                hist.assuming((Some(addr0), Some(addr1)), |hist| {
                    for c in cons.iter() {
                        if tt0.contains_int(flags, env, hist, c)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                })
            }
            (None, None) => {
                if cyc0() || cyc1() {
                    return refuse();
                }
                match OpenPair::of(t0, t1) {
                    OpenPair::Distinct => Ok(false),
                    act => {
                        if commit {
                            act.apply(t0, t1);
                        }
                        Ok(true)
                    }
                }
            }
        }
    }

    /// Does some member of `s` admit `t` (a probe)?
    fn set_admits(
        s: &[Type],
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<bool> {
        for m in s.iter() {
            if m.contains_int(BitFlags::empty(), env, hist, t)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Decide `s ⊇ t` by the first member (structural members before
    /// bare unbound tvars, which admit anything: `['b, Array<'b>]` must
    /// bind an array through `Array<'b>`) that a probe admits `t` into,
    /// deciding with `flags` only there: a member that would bind a cell
    /// and then fail is never tried. `None` when no member admits `t`.
    fn set_commit(
        s: &[Type],
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<Option<bool>> {
        let members = s
            .iter()
            .filter(|m| !is_unbound_tvar(m))
            .chain(s.iter().filter(|m| is_unbound_tvar(m)));
        let mut admitted = false;
        for m in members {
            if m.contains_int(BitFlags::empty(), env, hist, t)? {
                admitted = true;
                if flags.is_empty() || m.contains_int(flags, env, hist, t)? {
                    return Ok(Some(true));
                }
            }
        }
        Ok(admitted.then_some(false))
    }

    /// The distribution law for product heads: a set whose members
    /// split one argument position of a constructor across same-shaped
    /// alternatives covers the pooled constructor —
    /// `` [`T(A), `T(B)] ⊇ `T([A, B]) `` — provided every candidate
    /// covers every other position in full. Decided by probes over a
    /// cell-free scrutinee; a committing walk then commits every
    /// candidate's covering positions and the pooled one.
    fn set_covers_by_distribution(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        s: &Arc<[Type]>,
        t: &Self,
    ) -> Result<bool> {
        let t_id = hist.ref_id(t, env);
        if let Some(id) = t_id {
            if hist.distribution_probes_in_progress.contains(&id) {
                return Ok(false);
            }
            hist.distribution_probes_in_progress.push(id);
        }
        let r = Self::set_covers_by_distribution_inner(flags, env, hist, s, t);
        if t_id.is_some() {
            hist.distribution_probes_in_progress.pop();
        }
        r
    }

    fn set_covers_by_distribution_inner(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        s: &Arc<[Type]>,
        t: &Self,
    ) -> Result<bool> {
        fn head(env: &Env, t: &Type) -> Result<Type> {
            let mut cur = t.clone();
            for _ in 0..HEAD_CHAIN_LIMIT {
                cur = match &cur {
                    Type::TVar(_) => match cur.deref_cloned() {
                        Some(next) => next,
                        None => break,
                    },
                    // An unresolvable ref does not distribute; not an error.
                    Type::Ref(_) => match cur.lookup_ref_with(env, false) {
                        Ok(Some(next)) => next,
                        Ok(None) | Err(_) => break,
                    },
                    _ => break,
                }
            }
            Ok(cur)
        }
        let t = head(env, t)?;
        if t.has_unbound() {
            return Ok(false);
        }
        let mut targs: LPooled<Vec<Type>> = LPooled::take();
        match &t {
            Type::Variant(_, args, _) => targs.extend(args.iter().cloned()),
            Type::Tuple(args) => targs.extend(args.iter().cloned()),
            Type::Struct(flds) => targs.extend(flds.iter().map(|(_, t, _)| t.clone())),
            _ => return Ok(false),
        }
        let mut cands: LPooled<Vec<LPooled<Vec<Type>>>> = LPooled::take();
        for m in s.iter() {
            let m = head(env, m)?;
            let args: Option<LPooled<Vec<Type>>> = match (&t, &m) {
                (Type::Variant(tt, ta, _), Type::Variant(mt, ma, _))
                    if tt == mt && ta.len() == ma.len() =>
                {
                    Some(ma.iter().cloned().collect())
                }
                (Type::Tuple(ta), Type::Tuple(ma)) if ta.len() == ma.len() => {
                    Some(ma.iter().cloned().collect())
                }
                (Type::Struct(tf), Type::Struct(mf))
                    if tf.len() == mf.len()
                        && tf
                            .iter()
                            .zip(mf.iter())
                            .all(|((a, _, _), (b, _, _))| a == b) =>
                {
                    Some(mf.iter().map(|(_, t, _)| t.clone()).collect())
                }
                _ => None,
            };
            // Open cells in a candidate are fine (the probe accepts
            // through them without binding); only the scrutinee side
            // must be cell-free.
            if let Some(args) = args {
                cands.push(args);
            }
        }
        if cands.is_empty() {
            return Ok(false);
        }
        let probe = BitFlags::empty();
        let mut distributing: Option<usize> = None;
        for j in 0..targs.len() {
            let mut full = true;
            for c in cands.iter() {
                full &= c[j].contains_int(probe, env, hist, &targs[j])?;
                if !full {
                    break;
                }
            }
            if !full {
                // a second position differs: split position by position,
                // a verdict only where nothing could bind
                if distributing.is_some() {
                    if !cands.iter().all(|c| c.iter().all(|t| !t.has_unbound())) {
                        return Ok(false);
                    }
                    let cands: LPooled<Vec<&[Type]>> =
                        cands.iter().map(|c| &c[..]).collect();
                    return Self::covers_by_parts(env, hist, &cands, &targs);
                }
                distributing = Some(j);
            }
        }
        let pool = distributing.map(|j| {
            Type::Set(Arc::from_iter(cands.iter().map(|c| c[j].clone()))).normalize()
        });
        if let (Some(j), Some(pool)) = (distributing, &pool)
            && !pool.contains_int(probe, env, hist, &targs[j])?
        {
            return Ok(false);
        }
        if flags.is_empty() {
            return Ok(true);
        }
        // With no distributing position the first candidate covers `t`.
        let covering = match distributing {
            None => &cands[..1],
            Some(_) => &cands[..],
        };
        for c in covering.iter() {
            for (j, (cj, tj)) in c.iter().zip(targs.iter()).enumerate() {
                if Some(j) != distributing && !cj.contains_int(flags, env, hist, tj)? {
                    return Ok(false);
                }
            }
        }
        match (distributing, pool) {
            (Some(j), Some(pool)) => pool.contains_int(flags, env, hist, &targs[j]),
            _ => Ok(true),
        }
    }

    /// Do the candidates' argument lists cover `targs` together? The first
    /// position splits into its members, and each member's candidates
    /// must cover the positions after it. A probe.
    fn covers_by_parts(
        env: &Env,
        hist: &mut ContainsHist,
        cands: &[&[Type]],
        targs: &[Type],
    ) -> Result<bool> {
        ensure_sufficient(|| {
            let Some((first, rest)) = targs.split_first() else {
                return Ok(!cands.is_empty());
            };
            let mut found: SmallVec<[Type; 8]> = SmallVec::new();
            crate::expr::union_members(env, first, &mut found)?;
            let mut members: SmallVec<[Type; 8]> = SmallVec::new();
            for m in found.drain(..) {
                match m {
                    Type::Primitive(p) => {
                        members.extend(p.iter().map(|b| Type::Primitive(b.into())))
                    }
                    m => members.push(m),
                }
            }
            if members.is_empty() {
                members.push(first.clone())
            }
            for m in members.iter() {
                let mut sub: SmallVec<[&[Type]; 8]> = SmallVec::new();
                for c in cands.iter() {
                    if c[0].contains_int(BitFlags::empty(), env, hist, m)? {
                        sub.push(&c[1..])
                    }
                }
                if !Self::covers_by_parts(env, hist, &sub, rest)? {
                    return Ok(false);
                }
            }
            Ok(true)
        })
    }

    /// A constructor application (`self<'a>`, `'c<'b>`) against another
    /// type, either way round. A bound constructor is its filled type;
    /// an open one meets the other side decomposed on its outermost
    /// form and binds the constructor variable by name, discharging its
    /// bound through `find_impl`. A type with no last parameter is not
    /// a constructor.
    fn app_contains(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        match (self, t) {
            (Self::App(c0, a0), Self::App(c1, a1)) => {
                match (Self::app_filled(c0, a0), Self::app_filled(c1, a1)) {
                    (Some(f0), Some(f1)) => f0.contains_int(flags, env, hist, &f1),
                    (Some(f0), None) => f0.contains_int(flags, env, hist, t),
                    (None, Some(f1)) => self.contains_int(flags, env, hist, &f1),
                    (None, None) => Ok(c0.contains_int(flags, env, hist, c1)?
                        && a0.contains_int(flags, env, hist, a1)?),
                }
            }
            (Self::App(c, a), t1) => match Self::app_filled(c, a) {
                Some(filled) => filled.contains_int(flags, env, hist, t1),
                None => match Self::app_split_for(
                    c,
                    t1,
                    env,
                    flags.contains(ContainsFlags::Commit),
                )? {
                    Some((ctor, last)) => {
                        Ok(Self::bind_ctor(c, &ctor, flags, env, hist)?
                            && a.contains_int(flags, env, hist, &last)?)
                    }
                    None => Ok(false),
                },
            },
            (t0, Self::App(c, a)) => match Self::app_filled(c, a) {
                Some(filled) => t0.contains_int(flags, env, hist, &filled),
                None => match Self::app_split_for(
                    c,
                    t0,
                    env,
                    flags.contains(ContainsFlags::Commit),
                )? {
                    Some((ctor, last)) => {
                        Ok(Self::bind_ctor(c, &ctor, flags, env, hist)?
                            && last.contains_int(flags, env, hist, a)?)
                    }
                    None => Ok(false),
                },
            },
            _ => unreachable!("app_contains without an application"),
        }
    }

    /// Bind an open constructor variable to a constructor by name (the
    /// general walk would expand the reference and lose the name every
    /// later lookup keys on). Anything but an open, non-rigid variable
    /// takes the general walk.
    fn bind_ctor(
        c: &Self,
        ctor: &Self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
    ) -> Result<bool> {
        if let Self::TVar(cv) = c
            && !cv.is_bound()
            && !cv.is_rigid()
        {
            if !cell_constraints_ok(cv, env, hist, ctor)? {
                return Ok(false);
            }
            if flags.contains(ContainsFlags::Commit) {
                if graphix_dbg_bind() {
                    eprintln!("BIND ctor '{}({:x}) := {ctor:?}", cv.name, cv.cell_addr());
                }
                cv.bind(ctor.clone());
            }
            return Ok(true);
        }
        c.contains_int(flags, env, hist, ctor)
    }

    /// Does `t` implement the trait `tid`? ⊥ implements everything,
    /// `Any` nothing, a union iff every member does, an open cell iff
    /// it could still become an implementor (a rigid cell only through
    /// its own conjuncts), a typedef by its expansion, anything
    /// structural by the impl table.
    fn trait_contains(
        tid: TraitId,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        ensure_sufficient(|| Self::trait_contains_inner(tid, flags, env, hist, t))
    }

    fn trait_contains_inner(
        tid: TraitId,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        // The core traits have a structural default for every type.
        if CoreTrait::of_id(tid).is_some() {
            return Ok(true);
        }
        // a union implements a trait when every member does, unless a method
        // takes `self` beyond the receiver, which dispatch never narrows
        let union = match t {
            Self::Set(_) => true,
            Self::Primitive(p) => p.len() > 1,
            _ => false,
        };
        if union && env.trait_def(tid).is_some_and(|d| d.takes_self_beyond_receiver()) {
            return Ok(false);
        }
        match t {
            Self::Bottom => Ok(true),
            Self::Any => Ok(false),
            Self::TVar(tv) => {
                if let Some(b) = tv.binding() {
                    return Self::trait_contains(tid, flags, env, hist, &b);
                }
                if flags.contains(ContainsFlags::RigidCheck) && tv.is_rigid() {
                    return Ok(tv.cell_constraints().iter().any(
                        |c| matches!(c, Self::Ref(r) if env.trait_of_ref(r) == Some(tid)),
                    ));
                }
                Ok(true)
            }
            Self::Set(ts) => {
                for m in ts.iter() {
                    if !Self::trait_contains(tid, flags, env, hist, m)? {
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
                // A typedef met again inside its own question implements
                // the trait if the rest of it does.
                None => {
                    let e = t.lookup_ref(env)?;
                    let key = (hist.ref_id(t, env).unwrap_or(usize::MAX), tid);
                    if hist.traits_in_progress.contains(&key) {
                        return Ok(true);
                    }
                    hist.traits_in_progress.push(key);
                    let r = Self::trait_contains(tid, flags, env, hist, &e);
                    hist.traits_in_progress.pop();
                    r
                }
            },
            t => Ok(env.find_impl(tid, t)?.is_some()),
        }
    }

    /// Is this a reference to a trait (in `env`)?
    pub fn is_trait_ref(&self, env: &Env) -> bool {
        matches!(self, Self::Ref(tr) if env.trait_of_ref(tr).is_some())
    }

    pub fn contains(&self, env: &Env, t: &Self) -> Result<bool> {
        let r = self.contains_int(
            ContainsFlags::Commit.into(),
            env,
            &mut ContainsHist::new(),
            t,
        );
        if graphix_dbg_bind() {
            eprintln!("CONTAINS {self} >= {t} -> {r:?}");
        }
        r
    }

    pub fn contains_with_flags(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        t: &Self,
    ) -> Result<bool> {
        self.contains_int(flags, env, &mut ContainsHist::new(), t)
    }
}

// CR claude for claude: [test-gap] These three tests are the only direct pins of
// Type::contains. No graphix-tests pin covers the rules broken by the accepted
// ill-typed repros in design/review-2026-10-05/repro: the ByRef arm's covariance
// (c-node-mod-01; reference_variable_does_not_widen pins only callsite.rs::Widening
// with the reference first), abstract parameters (t-contains-07), a rigid cell decided
// by a conjunct probe (t-contains-03), a conjunct never reaching a binding's open cells
// (t-contains-08), and ⊥ ⊇ 'x, occurs refusals and open rigid constructors (t-tvar-02,
// t-tvar-08, t-tvar-01). Each passes --check at HEAD, and --check on t-contains-09
// (contains over a nested typedef) never returns; cast_to_a_growing_definition covers
// only cast's walk. No family in graphix-fuzz/src/mustreject.rs targets a variance
// rule, and the generators build no parameterized abstract type. So the fleet meets
// such a hole only when a generated program happens to run the lie, and then reports it
// as a JIT divergence (graphix-fuzz check on t-contains-07.gx). Pin each repro with its
// fix, and once each variance rule is stated, give it a must-reject family: a write
// through a widened reference, a widened abstract parameter. (t-contains-12)
// 2026-10-07 claude: t-contains-03, t-contains-08 and t-tvar-08 are fixed and pinned in
// lang::types (rigid_by_conjunct_commits, conjunct_reaches_open_cells,
// rigid_pair_by_conjuncts), with t-tvar-01, x-typecheck-generics-F13 and the union
// trait rule beside them. c-node-mod-01, t-contains-07, t-tvar-02 and t-contains-09
// still stand, and so do the variance must-reject families.
// 2026-10-06 claude: the reference part is done: `&T`/`&mut T` rules are pinned in
// lang::byref and by must-reject family 9 (ref-write, ref-widen). The rest stands.
#[cfg(test)]
mod tests {
    use super::*;
    use arcstr::literal;

    fn parsed(s: &str) -> Type {
        let t = crate::expr::parser::parse_type(s).unwrap();
        t.alias_tvars(&mut LPooled::take());
        t
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

    // covering `Array<i64>` binds the bare member first; the residue
    // `i64` must still reach it
    #[test]
    fn a_free_union_member_takes_the_residue_after_a_sibling_binds_it() {
        let env = Env::default();
        let free = parsed("['b, Array<'b>]");
        assert!(free.contains(&env, &parsed("[i64, Array<i64>]")).unwrap());
        assert_eq!(binding(&free, 0), Some(parsed("i64")));
        let free = parsed("['b, Array<'b>]");
        assert!(!free.contains(&env, &parsed("[string, Array<i64>]")).unwrap());
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
}
