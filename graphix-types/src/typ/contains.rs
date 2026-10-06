use crate::{
    PrintFlag,
    dbgenv::{graphix_dbg_bind, graphix_dbg_cycle_bt},
    env::Env,
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        AndAc, CoreTrait, Lazy, NormKey, RefHist, RefPair, TVar, TraitId, Type, TypeRef,
        node_addr, probe_key, setops::union_identical, tvar::would_cycle_inner,
    },
};
use ahash::AHashMap;
use anyhow::Result;
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
}

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
fn open_cell_reaches(a: &Type, b: &Type) -> bool {
    matches!(a, Type::TVar(tv) if !tv.is_bound() && tv.would_cycle(b))
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

/// True iff binding `t` into the cell would satisfy every conjunct of
/// the cell's constraints. A pure probe.
fn cell_constraints_ok(
    tv: &TVar,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    for c in tv.cell_constraints().iter() {
        // CR claude for eric: [bug] This probe admits an open cell inside `t`
        // (`Array<i64> ⊇ Array<'y>` holds with `'y` free). The bind that follows
        // installs `t` without giving `'y` its part of the conjunct, because
        // `TVar::bind` passes down only Concrete, Singleton and OneNumber, so `'y`
        // generalizes unbounded. With `f = 'a: Array<i64> |x: 'a| -> i64 x[0]$`, `let g
        // = |y| f([y])` is typed `fn(y: 'y) -> i64`. `--check` accepts `g("hello")` and
        // the build refuses it, and through `let h: fn(y: string) -> i64 = g` the run
        // panics at fusion/kernel.rs:243 (a String in a compiled Scalar(I64) slot). An
        // inferred conjunct has the same hole: `let f = |i| a[i]$; let g = |b, k|
        // f(select b { true => 1, false => k })` passes the check at `g(false, "s")`.
        // probe: design/review-2026-10-05/repro/t-contains-08.gx (t-contains-08)
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
        // CR claude for eric: [bug] With exactly one rigid side this picks a name alias
        // by `frozen` alone. A declared variable written once in its signature is
        // unfrozen, so AliasLeft (t0 rigid) or AliasRight (t1 rigid) points it at the
        // other cell through merge_into's Merge::Name path, which has no rigid-survivor
        // rule; the variable then reads as free and the body binds it. `let eq = |a:
        // 'x, b: 'x| a == b; let f = |x: 'a| eq(x, 1)` checks as fn(x: i64) and `|x:
        // 'a, y: 'b| eq(x, y)` as fn(x: 'a, y: 'a), while `x == 1`, `x == y` and `eq(1,
        // x)` are refused. In a trait impl the check passes, a static call is refused
        // only at elaboration, and a dynamic call runs the impl at a type it was never
        // checked at, writing an f64 into an i64 and panicking the JIT (probe:
        // design/review-2026-10-05/repro/x-typecheck-generics-F13.gx). With one rigid
        // side the cells should merge (OpenPair::Merge), which keeps the rigid cell.
        // (x-typecheck-generics-F13)
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
        | (Type::Error(a), Type::Error(b))
        | (Type::ByRef(a), Type::ByRef(b)) => link_equal(a, b, commit),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            link_equal(k0, k1, commit) && link_equal(v0, v1, commit)
        }
        (Type::App(c0, a0), Type::App(c1, a1)) => {
            link_equal(c0, c1, commit) && link_equal(a0, a1, commit)
        }
        // CR claude for eric: [bug] A bound variable against a non-variable lands in
        // this arm, but union_identical accepts that pair through the binding
        // (setops.rs:80) and Fn equality counts distinct open cells as equal, so
        // identical_linked answers identical and welds nothing. A union member that is
        // a variable bound to a function type then covers a same-shaped function type
        // through the identity shortcuts (lines 558, 817, 837, 893) with their open
        // cells left apart: the checker accepts a program whose string-typed binding
        // holds an i64, the node-walk prints it, and the JIT panics the runtime at
        // fusion/kernel.rs:243. Add the arm union_identical has, recursing into the
        // binding with the sides kept in order. probe:
        // design/review-2026-10-05/repro/x-expr-walks-06.gx (x-expr-walks-06)
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
        | (Type::Error(x), Type::Error(y))
        | (Type::ByRef(x), Type::ByRef(y)) => Arc::ptr_eq(x, y),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            Arc::ptr_eq(k0, k1) && Arc::ptr_eq(v0, v1)
        }
        _ => false,
    }
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
        if ok { Ok(()) } else { Err(self.contains_mismatch(t)) }
    }

    // CR claude for eric: [readability] When a trait bound refuses a type, the error is
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
    fn contains_mismatch(&self, t: &Self) -> anyhow::Error {
        // A refused open cell on either side is the infinite type,
        // surfacing at a consumer; report it as the settle path does.
        if type_has_refused_open_cell(self)
            || type_has_refused_open_cell(t)
            || open_cell_reaches(self, t)
            || open_cell_reaches(t, self)
        {
            return anyhow::anyhow!("{INFINITE_TYPE_MSG}");
        }
        anyhow::Error::new(TypeMismatch { expected: self.clone(), actual: t.clone() })
    }

    /// [`Self::check_contains`] with rigid enforcement; the def gate's
    /// acceptance checks only.
    pub fn check_contains_rigid(&self, env: &Env, t: &Self) -> Result<()> {
        let flags = ContainsFlags::Commit | ContainsFlags::RigidCheck;
        let ok = self.contains_int(flags, env, &mut ContainsHist::new(), t)?;
        if ok { Ok(()) } else { Err(self.contains_mismatch(t)) }
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
            (t0 @ Self::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Self::Ref(TypeRef { .. })) => {
                // CR claude for eric: [bug] This memo keys a reference on its
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
                // CR claude for eric: [bug] This memo never ends a walk through a
                // recursive typedef whose self-reference is a union member (`[T<'a>,
                // null]`, ErrChain's `cause`) once the refs' params hold type
                // variables. `[T<P>, null] ⊇ T<Q>` and `T<P> ⊇ {..}` are keyed by the
                // non-Ref side's allocation, and expand_ref (line 148) rebuilds such a
                // ref's expansion on every visit; set_covers_by_distribution's head()
                // rebuilds every ref it expands. So no pair ever repeats, and the walk
                // recurses until memory runs out. Both `let h = |s| { catch(e) null;
                // error(`A(s))?; error(`B(s))?; null }` (the catch's union probes
                // ErrChain<[`A('s), `B('s)]> ⊇ ErrChain<`A('s)>) and `type T<'a> = {n:
                // [T<'a>, null], v: 'a}; let g = |t: T<'b>| -> T<['b, i64]> t;` take
                // `graphix --check`, and every run, past 6 GB in about 6 s. The memo
                // has to recognize a pair met again by its content, not by its
                // allocation. probe: design/review-2026-10-05/repro/x-parallel-02.gx
                // (x-parallel-02)
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                hist.assuming(key, |hist| {
                    let Some(e0) = hist.expand_ref(t0, key.0, env, commit)? else {
                        return Ok(false);
                    };
                    let Some(e1) = hist.expand_ref(t1, key.1, env, commit)? else {
                        return Ok(false);
                    };
                    e0.contains_int(flags, env, hist, &e1)
                })
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
                        t0.bind(Self::Bottom);
                    }
                    Ok(true)
                }
            },
            (Self::Bottom, Self::Bottom) => Ok(true),
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
            (
                Self::Primitive(p),
                Self::Array(_) | Self::Tuple(_) | Self::Struct(_) | Self::Variant(..),
            ) => Ok(p.contains(Typ::Array)),
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
            // CR claude for eric: [dead] This arm and the pointer-equality arms at 691
            // (Struct), 704 (Variant) and 815 (Set) never fire. contains_dispatch is
            // reached only from contains_int_inner, after same_content (453) has
            // already returned true for every pair whose Tuple, Struct, Variant, Set or
            // Fn content is one allocation. For the same reason, `same` in the Fn arm
            // (936) is always false. Delete the four arms and reduce the Fn arm to `let
            // r = f0.contains_int(flags, env, hist, f1)?; if r && commit {
            // f0.lambda_ids.link(&f1.lambda_ids) } Ok(r)`. (t-contains-14)
            (Self::Tuple(t0), Self::Tuple(t1)) if Arc::ptr_eq(t0, t1) => Ok(true),
            (Self::Tuple(t0), Self::Tuple(t1)) => Ok(t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::Struct(t0), Self::Struct(t1)) if Arc::ptr_eq(t0, t1) => Ok(true),
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
            (Self::Variant(tg0, t0, _), Self::Variant(tg1, t1, _))
                if tg0.as_ptr() == tg1.as_ptr() && Arc::ptr_eq(t0, t1) =>
            {
                Ok(true)
            }
            (Self::Variant(tg0, t0, _), Self::Variant(tg1, t1, _)) => Ok(tg0 == tg1
                && t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            // CR claude for eric: [bug] This arm makes references covariant (`&[i64,
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
            (Self::ByRef(t0), Self::ByRef(t1)) => t0.contains_int(flags, env, hist, t1),
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
                    // CR claude for eric: [bug] Under Commit this admits `t0 ⊇ 'r`
                    // (rigid, open 'r) on a probe of one conjunct and records nothing,
                    // so t0's open cells stay free and a later check binds them to
                    // anything. Take `id = |a: Array<'e>| -> Array<'e> a`. Then `'r:
                    // Array<i64> |x: 'r| -> Array<string> id(x)` leaves 'e open, the
                    // return check binds 'e := string, and the def is accepted even
                    // though it returns its Array<i64> argument; without the annotation
                    // its signature returns a free `Array<'e>`. The instance's own
                    // check refuses it (GRAPHIX_NO_SUBST=1 GRAPHIX_ELAB_AUDIT=1), but
                    // elaboration by substitution trusts the def, so the JIT reads the
                    // i64 as an ArcStr and aborts. A rank-2 formal with a structural
                    // quantifier bound (`fn<'b: Array<i64>>(x: 'b) -> Array<string>`)
                    // reaches the same route. probe:
                    // design/review-2026-10-05/repro/t-contains-03.gx (t-contains-03)
                    for c in t1.cell_constraints().iter() {
                        // A probe: a flagged check would alias live
                        // cells into the constraint store.
                        if t0.contains_int(BitFlags::empty(), env, hist, c)? {
                            return Ok(true);
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
                    t1.bind(t0.clone());
                }
                Ok(true)
            }
            (Self::Set(s0), Self::Set(s1)) if Arc::ptr_eq(s0, s1) => Ok(true),
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
                    // CR claude for eric: [bug] This loop commits the first
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
            // CR claude for eric: [bug] `[i64, 'y] ⊇ 'y` with 'y open binds 'y := i64.
            // The TVar arms skip it because 'y occurs in the set, this arm has no
            // identity pre-pass (the Set ⊇ Set arms have one), and set_commit tries the
            // structural member i64 before the free 'y. An identical struct member
            // loses the same way: `[{v: i64}, {v: 'y}] ⊇ {v: 'y}` binds 'y := i64. As a
            // result `let f = |y| array::push([y, 1], y); f("s")` is refused at "s"
            // with "i64 does not contain string". Also, `let y = str::parse("42")$; let
            // x = select c { 0 => y, _ => null }; x <- y` is accepted with parse's
            // target bound to null, when an open target must be refused. Cover t
            // without binding when a member is identical_linked to it, before
            // set_commit. probe: design/review-2026-10-05/repro/t-contains-10.gx
            // (t-contains-10)
            (Self::Set(s), t) => {
                if graphix_dbg_bind() {
                    eprintln!("SET-T {} >= {t}", Self::Set(s.clone()));
                }
                match t {
                    // Prims first: the narrowest TVar bindings.
                    // CR claude for eric: [bug] When a union with a bare open member is
                    // checked against a multi-bit primitive, this arm commits the bits
                    // one at a time. The first bit that no concrete member covers binds
                    // the open member ('a := i64), the next bit is admitted by nothing,
                    // and the arm returns false with that binding left behind. So
                    // `opt::is_some(x)` with x: [i64, string, null] is refused ("[null,
                    // 'a: i64] does not contain [i64, null, string]"), while the same
                    // type written as `type N = [i64, string]; [N, null]`, or `[i64,
                    // `A, null]`, is accepted. A probe of the same pair answers true,
                    // so set_commit can pick a member whose commit then fails. Bind the
                    // open member once to the bits no concrete member covers, as the
                    // Set ⊇ Set residue arm does. probe:
                    // design/review-2026-10-05/repro/x-diff-types-04.gx
                    // (x-diff-types-04)
                    Self::Primitive(p) if p.len() > 1 => {
                        let mut all = true;
                        for p in t.iter_prims() {
                            all &= Self::set_admits(s, env, hist, &p)?;
                        }
                        // CR claude for eric: [bug] This pre-pass commits the
                        // primitives one at a time. The first primitive no concrete
                        // member covers binds the bare free member to itself. The next
                        // one is not admitted by that binding, and the arm returns
                        // false without trying the whole set at 930 and without undoing
                        // the binding. A probe of the same pair says true, so
                        // set_commit's probe-then-commit keeps the binding and moves on
                        // to the next member. Valid calls are refused:
                        // `opt::is_some(v)` with `v: [i64, string, null]` fails with
                        // "[null, 'a: i64] does not contain [i64, null, string]" while
                        // `[i64, Array<i64>, null]` (a Set, the residue arm at 824) is
                        // accepted, and the leaked binding refuses an unrelated
                        // argument (`|x: [Array<['a, null]>, Array<'c>], y: 'a|` called
                        // with `Array<[bool, null, string]>` and "t"). Hand the
                        // uncovered primitives to the bare member as one residue, as
                        // the Set ⊇ Set arm does; probe:
                        // design/review-2026-10-05/repro/t-contains-11.gx
                        // (t-contains-11)
                        if all {
                            for p in t.iter_prims() {
                                if Self::set_commit(s, flags, env, hist, &p)?
                                    != Some(true)
                                {
                                    return Ok(false);
                                }
                            }
                            return Ok(true);
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
                let same = Arc::ptr_eq(f0, f1);
                let r = same || f0.contains_int(flags, env, hist, f1)?;
                if r && !same && commit {
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
            | (Self::ByRef(_), _)
            | (_, Self::ByRef(_))
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
                    if !cell_constraints_ok(t0, env, hist, &b1)? {
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
                    if !cell_constraints_ok(t1, env, hist, &b0)? {
                        return Ok(false);
                    }
                    t1.copy(t0, b0);
                }
                Ok(true)
            }
            (None, None) => {
                // CR claude for eric: [bug] When two open rigid cells' bounds reach
                // each other (`'b: Array<'a>`, or `'b: 'a`), this occurs check fires
                // before OpenPair::Distinct is consulted. refuse() then answers true,
                // so the def gate accepts a value of either quantifier as the other.
                // The cycle_refused marks matter only for a cell still open at a
                // terminal settle, and a call binds both copies, so nothing reports it.
                // The instance substitutes that verdict: `'a: Any, 'b: Array<'a> |x:
                // 'b, y: 'a| -> 'b y` called as `f([1], 12345)` builds a kernel that
                // returns the i64 as Array<i64> and segfaults, while the node-walk
                // hands an Array-typed binding an i64. Decide a rigid pair by its
                // conjuncts, as the `(t0, TVar(t1))` rigid arm does, and not by the
                // refusal: a blanket Ok(false) would also refuse `'b: 'a |x: 'a, y: 'b|
                // -> 'a y`, which passes today only through this path. probe:
                // design/review-2026-10-05/repro/t-tvar-08.gx (t-tvar-08)
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
                // CR claude for eric: [bug] Distribution allows only one position to
                // differ, so a set listing every combination of two unions, [(`L, `L),
                // (`L, `N), (`N, `L), (`N, `N)], is held not to contain ([`L, `N], [`L,
                // `N]). Select::check_coverage runs this check before the literal pool,
                // so a select that lists every combination is refused with "missing
                // match cases". check_dead_arms trusts the pool and refuses a `_` added
                // after those arms as unreachable, so the exhaustive form cannot be
                // written at all; tuples, multi-payload variants, structs and payload
                // binds all hit this. The same gap refuses passing such a tuple to a
                // parameter typed as the four-member union. Distributing recursively
                // (split on one position, then require each group to cover the
                // remaining positions) would close it. probe:
                // design/review-2026-10-05/repro/x-engine-seq-errors-08.gx
                // (x-engine-seq-errors-08)
                if distributing.is_some() {
                    return Ok(false);
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
                None => match Self::app_split_for(c, t1, env)? {
                    Some((ctor, last)) => {
                        Ok(Self::bind_ctor(c, &ctor, flags, env, hist)?
                            && a.contains_int(flags, env, hist, &last)?)
                    }
                    None => Ok(false),
                },
            },
            (t0, Self::App(c, a)) => match Self::app_filled(c, a) {
                Some(filled) => t0.contains_int(flags, env, hist, &filled),
                None => match Self::app_split_for(c, t0, env)? {
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
        // CR claude for eric: [bug] Under a plain Commit (no RigidCheck) a rigid
        // constructor variable takes this path: cell_constraints_ok passes, the bind is
        // skipped, and Ok(true) comes back. So inside a `'c: Collection` body,
        // `Array<i64> ⊇ 'c<i64>` holds without binding 'c, and so do `List<i64> ⊇
        // 'c<i64>` and `array::len(xs)`. That breaks the rule every TVar arm keeps (a
        // rigid cell is refused under `rigid || commit`) and the doc above. The check
        // then accepts bodies that read a Map or List as an Array: the JIT reaches
        // unreachable_unchecked in Value::clone (UB in release), the node-walk returns
        // garbage, and the array::len form passes --check only for elaboration to
        // refuse it. Take this path only when `!cv.is_rigid()`, so a rigid one falls to
        // the general walk and is refused under commit. probe:
        // design/review-2026-10-05/repro/t-tvar-01.gx (t-tvar-01)
        if let Self::TVar(cv) = c
            && !cv.is_bound()
            && !(flags.contains(ContainsFlags::RigidCheck) && cv.is_rigid())
        {
            if !cell_constraints_ok(cv, env, hist, ctor)? {
                return Ok(false);
            }
            if flags.contains(ContainsFlags::Commit) && !cv.is_rigid() {
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
            // CR claude for eric: [bug] This arm lets a union satisfy a trait whenever
            // every member has an impl, whatever the trait's methods take. So
            // `Comb::comb(x, y)` with `comb: fn(self, other: self)` and x, y: [A, B]
            // passes `--check`. The build then lowers the call through
            // lower_trait_union (graphix-compiler/src/node/traits.rs:877), which
            // narrows only the receiver, and refuses `#bind::N(#t, #a1)` because `#a1`
            // is still [A, B]; the message names `#bind`, `#a1` and `'_N`, and
            // graphix-fuzz reports a check/build divergence. A generic `'a: Comb |x:
            // 'a, y: 'a| Comb::comb(x, y)` called with [A, B] fails the same way in its
            // instance, and only this discharge sees that route, so the refusal belongs
            // here and in the multi-flag Primitive arm below: a union cannot satisfy a
            // trait that has a method taking `self` in a parameter other than the
            // receiver (a `self` return is fine). probe:
            // design/review-2026-10-05/repro/x-engine-seq-errors-05.gx
            // (x-engine-seq-errors-05)
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

// CR claude for eric: [test-gap] These three tests are the only direct pins of
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
