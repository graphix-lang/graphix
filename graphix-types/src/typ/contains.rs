use crate::{
    AddrMap, AddrSet, PrintFlag, SourcePosition,
    dbgenv::{graphix_dbg_bind, graphix_dbg_cycle_bt},
    env::{Env, ImplDef},
    expr::ModPath,
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        AbstractId, AndAc, Bound, Brief, CoreTrait, FnType, Indiscernible, Lazy,
        Mutability, NodePairs, NormKey, Open, RefHist, RefPair, TVar, TraitId, Type,
        node_addr, params_size, probe_key,
        setops::{strictly_identical, union_identical},
        tvar::{CellFact, Fresh, RigidGate, would_cycle_inner},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use arcstr::ArcStr;
use compact_str::format_compact;
use enumflags2::{BitFlags, bitflags};
use netidx_value::Typ;
use nohash::IntMap;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::RefCell,
    mem,
    ops::{ControlFlow, Deref, DerefMut},
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
    /// The walk is under an odd number of function arguments, where the
    /// sides of the judgment have swapped.
    Contra,
    /// `self` is the written form of the very function `t` types, which
    /// adopts `t`'s quantifier bounds: its quantifiers absorb them rather
    /// than hold `t` to their declared bounds.
    Adopt,
}

/// The infinite-type rejection wording, shared by
/// [`TVar::settle_or_bottom`] and [`Type::contains_mismatch`].
pub(crate) const INFINITE_TYPE_MSG: &str = "cannot infer a finite type here: unification requires a type that \
     contains itself (e.g. a function that returns itself); declare a \
     named recursive type and annotate the binding";

/// A question a walk may meet again inside its own answer.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Question {
    /// The distribution law over the type with this id: met again, it
    /// claims nothing.
    Distribution(usize),
    /// The type with this id implements the trait: met again, it does if
    /// the rest of it does.
    Implements(usize, TraitId),
}

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
    probe_pairs: Lazy<AHashMap<(NormKey, NormKey, bool), (u64, bool)>>,
    probe_pins: Lazy<Vec<Type>>,
    /// Committing pairs proven to hold, by both sides' identities and the
    /// flags: what the proof bound stays bound for the rest of the call,
    /// so the same pair met again holds without walking it (a DAG's
    /// shared nodes are met once per path otherwise). Pinned with
    /// `probe_pins`.
    commit_pairs: Lazy<AHashSet<(NormKey, NormKey, BitFlags<ContainsFlags>)>>,
    /// The questions in progress on this path, answered by assumption
    /// when met again.
    in_progress: SmallVec<[Question; 4]>,
    /// The shallowest entry of `in_progress` a verdict was answered by
    /// assumption from: what depends on it is the walk's answer, not the
    /// pair's, until that question returns.
    tainted: Option<usize>,
    /// The shallowest in-progress pair the current verdict assumed.
    low_water: usize,
    epoch: u64,
    /// Per definition and side (`true` for the right), on this path: the
    /// size of the params its latest expansion took, and how many
    /// expansions in a row grew them. The sides expand on alternate steps
    /// (`T ⊇ {..}`, then `[T, null] ⊇ T`), each growing on its own.
    unfolding: Lazy<AHashMap<(usize, bool), (usize, u32)>>,
    /// A definition whose parameters grew past [`MAX_GROWTH`] under a
    /// probe: the reason a refusal gives when the verdict needed it.
    grew: Option<ModPath>,
    /// [`Self::closed`]'s verdicts by node and cell identity, each node
    /// pinned so its address is not recycled under its key.
    closed: Lazy<AddrMap<bool>>,
    closed_pins: Lazy<Vec<Type>>,
    /// How many function types hold quantifier cells rigid on this path
    /// (`FnType::contains_int`): a verdict reached under one depends on
    /// the gate, so the probe memo is neither read nor filled.
    pub(super) gated: u32,
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
            commit_pairs: Lazy::new(),
            in_progress: SmallVec::new(),
            tainted: None,
            low_water: usize::MAX,
            epoch: 0,
            unfolding: Lazy::new(),
            grew: None,
            closed: Lazy::new(),
            closed_pins: Lazy::new(),
            gated: 0,
        }
    }

    /// Whether `t` reaches no open cell, through bindings: a commit over
    /// two closed sides binds nothing, so it is the probe. Within a walk
    /// a closed node stays closed (cells only bind), and one found open
    /// only costs the commit's walk.
    fn closed(&mut self, t: &Type) -> bool {
        ensure_sufficient(|| {
            let key = t.shared_identity();
            if let Some(k) = key {
                if let Some(c) = self.closed.get().and_then(|m| m.get(&k)) {
                    return *c;
                }
                // a binding that reaches its own cell is not closed
                self.closed.get_mut().insert(k, false);
                self.closed_pins.get_mut().push(t.clone());
            }
            let c = match t {
                Type::TVar(tv) => tv.binding().is_some_and(|b| self.closed(&b)),
                t => t
                    .try_for_each_child(&mut |c| match self.closed(c) {
                        true => ControlFlow::Continue(()),
                        false => ControlFlow::Break(()),
                    })
                    .is_continue(),
            };
            if let Some(k) = key {
                self.closed.get_mut().insert(k, c);
            }
            c
        })
    }

    /// Cached pure-probe verdict for `(t0, t1)`, rigid cells held rigid
    /// or not, if current.
    fn probe_get(&self, rigid: bool, t0: &Type, t1: &Type) -> Option<bool> {
        let k = (probe_key(t0)?, probe_key(t1)?, rigid);
        let (epoch, r) = self.probe_pairs.get()?.get(&k).copied()?;
        (epoch == self.epoch).then_some(r)
    }

    fn probe_put(&mut self, rigid: bool, t0: &Type, t1: &Type, r: bool) {
        if let (Some(k0), Some(k1)) = (probe_key(t0), probe_key(t1))
            && self
                .probe_pairs
                .get_mut()
                .insert((k0, k1, rigid), (self.epoch, r))
                .is_none()
        {
            let pins = self.probe_pins.get_mut();
            pins.push(t0.clone());
            pins.push(t1.clone());
        }
    }

    /// Start a verdict that may be memoized: its height, and the outer
    /// verdict's assumption state, set aside.
    fn enter_verdict(&mut self) -> (usize, usize, Option<usize>) {
        let height = self.len();
        let outer = mem::replace(&mut self.low_water, usize::MAX);
        (height, outer, self.tainted.take())
    }

    /// End it: whether the verdict assumed no pair from further out, so
    /// it holds outside this call; the outer state takes its assumptions.
    fn leave_verdict(
        &mut self,
        height: usize,
        outer: usize,
        outer_taint: Option<usize>,
    ) -> bool {
        let assumed = self.low_water;
        self.low_water = outer.min(assumed);
        let tainted = self.tainted;
        self.tainted = match (outer_taint, tainted) {
            (Some(a), Some(b)) => Some(a.min(b)),
            (a, b) => a.or(b),
        };
        assumed >= height && tainted.is_none()
    }

    fn commit_held(&self, flags: BitFlags<ContainsFlags>, t0: &Type, t1: &Type) -> bool {
        let (Some(k0), Some(k1)) = (probe_key(t0), probe_key(t1)) else { return false };
        self.commit_pairs.get().is_some_and(|m| m.contains(&(k0, k1, flags)))
    }

    fn commit_put(&mut self, flags: BitFlags<ContainsFlags>, t0: &Type, t1: &Type) {
        if let (Some(k0), Some(k1)) = (probe_key(t0), probe_key(t1))
            && self.commit_pairs.get_mut().insert((k0, k1, flags))
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

    /// Decide `q` by `f` with it in progress; met again inside, it is
    /// `assumed`, and the verdicts that met it are memoized no further
    /// out than it.
    fn question(
        &mut self,
        q: Question,
        assumed: bool,
        f: impl FnOnce(&mut Self) -> Result<bool>,
    ) -> Result<bool> {
        if let Some(i) = self.in_progress.iter().position(|p| *p == q) {
            self.tainted = Some(self.tainted.map_or(i, |t| t.min(i)));
            return Ok(assumed);
        }
        let i = self.in_progress.len();
        self.in_progress.push(q);
        let r = f(self);
        self.in_progress.pop();
        if self.tainted.is_some_and(|t| t >= i) {
            self.tainted = None;
        }
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

/// A union whose open member's cell another member mentions, as
/// `['b, Array<'b>]`: where a member goes decides that cell.
fn shares_bare_cell(s0: &[Type]) -> bool {
    let open = |tv: &TVar| tv.open_cell().map(|c| c.cell_addr());
    s0.iter()
        .filter_map(|m| match m {
            Type::TVar(tv) => open(tv),
            _ => None,
        })
        .any(|cell| {
            s0.iter().filter(|m| !is_unbound_tvar(m)).any(|m| {
                let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                m.collect_tvars(&mut tvs);
                tvs.values().any(|tv| open(tv) == Some(cell))
            })
        })
}

/// How deep `t` nests constructors.
fn nesting(t: &Type) -> usize {
    ensure_sufficient(|| {
        let mut depth = 0;
        t.for_each_child(&mut |c| depth = depth.max(nesting(c)));
        depth + 1
    })
}

/// The flags a probe guiding a decision under `flags` runs with: it holds
/// a rigid cell rigid wherever the decision will, so a member it admits
/// does not fail on that cell.
fn guide(flags: BitFlags<ContainsFlags>) -> BitFlags<ContainsFlags> {
    match flags.intersects(ContainsFlags::Commit | ContainsFlags::RigidCheck) {
        true => BitFlags::from(ContainsFlags::RigidCheck),
        false => BitFlags::empty(),
    }
}

/// A variable whose chain of bindings ends open.
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
        if predicate || c.is_trait_ref(env) {
            continue;
        }
        let held = match exact(c) {
            true => c.contains_int(flags, env, hist, t)?,
            false => commit_parts(tv, flags, env, hist, c, t)?,
        };
        if !held {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Pass the parts of a conjunct `c` that is not exact down to the open
/// cells of `t` they meet: a cell in a covariant position is narrowed by
/// its part (`Array<[i64, null]> ⊇ Array<'y>` gives `'y: [i64, null]`, `Int
/// ⊇ [i64, 'k]` gives `'k: Int`), an exact part is committed as an exact
/// conjunct is, and a function's argument takes the conjunct's argument,
/// the least it must accept. A probe accepted `t` already; what the walk
/// can't line up it leaves.
fn commit_parts(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    c: &Type,
    t: &Type,
) -> Result<bool> {
    ensure_sufficient(|| {
        if exact(c) {
            return c.contains_int(flags, env, hist, t);
        }
        let all = |pairs: &mut dyn Iterator<Item = (&Type, &Type)>,
                   hist: &mut ContainsHist|
         -> Result<bool> {
            for (c, t) in pairs {
                if !commit_parts(tv, flags, env, hist, c, t)? {
                    return Ok(false);
                }
            }
            Ok(true)
        };
        match (c, t) {
            (Type::Any, _) => Ok(true),
            (_, Type::TVar(t)) => match t.open_cell() {
                Some(cell) if cell.cell_addr() != tv.cell_addr() => {
                    cell.narrow_cell(env, c.clone())?;
                    Ok(true)
                }
                Some(_) => Ok(true),
                None => match t.binding() {
                    Some(b) => commit_parts(tv, flags, env, hist, c, &b),
                    None => Ok(true),
                },
            },
            (Type::Ref(_), _) => match c.lookup_ref(env) {
                Ok(c) => commit_parts(tv, flags, env, hist, &c, t),
                Err(_) => Ok(true),
            },
            (_, Type::Set(ts)) => all(&mut ts.iter().map(|t| (c, t)), hist),
            (Type::Set(cs), t) if structured(t) && cs.iter().all(structured_or_prim) => {
                let mut same = cs.iter().filter(|c| same_head(c, t));
                match (same.next(), same.next()) {
                    (Some(c), None) => commit_parts(tv, flags, env, hist, c, t),
                    _ => Ok(true),
                }
            }
            (Type::Array(c), Type::Array(t))
            | (Type::List(c), Type::List(t))
            | (Type::Error(c), Type::Error(t))
            | (Type::ByRef(Mutability::Shared, c), Type::ByRef(Mutability::Shared, t)) => {
                commit_parts(tv, flags, env, hist, c, t)
            }
            (Type::Map { value: c, .. }, Type::Map { value: t, .. }) => {
                commit_parts(tv, flags, env, hist, c, t)
            }
            (Type::Tuple(cs), Type::Tuple(ts)) if cs.len() == ts.len() => {
                all(&mut cs.iter().zip(ts.iter()), hist)
            }
            (Type::Variant(x, cs, _), Type::Variant(y, ts, _))
                if x == y && cs.len() == ts.len() =>
            {
                all(&mut cs.iter().zip(ts.iter()), hist)
            }
            (Type::Struct(cs), Type::Struct(ts))
                if cs.len() == ts.len()
                    && cs.iter().zip(ts.iter()).all(|(c, t)| c.0 == t.0) =>
            {
                all(&mut cs.iter().zip(ts.iter()).map(|(c, t)| (&c.1, &t.1)), hist)
            }
            (Type::Fn(cf), Type::Fn(tf))
                if cf.args.len() == tf.args.len()
                    && cf
                        .args
                        .iter()
                        .zip(tf.args.iter())
                        .all(|(c, t)| c.name() == t.name()) =>
            {
                for (ca, ta) in cf.args.iter().zip(tf.args.iter()) {
                    // a bound that names its own cell (`'a: fn(x: 'a)`) would
                    // make the cell's binding hold itself: no finite type
                    if tv.would_cycle(&ca.typ)
                        || !ta.typ.contains_int(flags, env, hist, &ca.typ)?
                    {
                        return Ok(false);
                    }
                }
                commit_parts(tv, flags, env, hist, &cf.rtype, &tf.rtype)
            }
            _ => Ok(true),
        }
    })
}

/// Whether `t` is built by a constructor with parts.
fn structured(t: &Type) -> bool {
    match t {
        Type::Variant(_, ps, _) => !ps.is_empty(),
        Type::Array(_)
        | Type::List(_)
        | Type::Tuple(_)
        | Type::Struct(_)
        | Type::Map { .. }
        | Type::Error(_)
        | Type::ByRef(..)
        | Type::Fn(_) => true,
        _ => false,
    }
}

/// Whether a union member's head is known without expanding it, and
/// holds no structured value unless it is structured.
fn structured_or_prim(t: &Type) -> bool {
    match t {
        Type::Primitive(p) => {
            !p.intersects(Typ::Array | Typ::Map | Typ::Error | Typ::Abstract)
        }
        t => structured(t) || matches!(t, Type::Variant(..)),
    }
}

/// Whether `c` and `t` are applications of one constructor.
fn same_head(c: &Type, t: &Type) -> bool {
    match (c, t) {
        (Type::Variant(x, xs, _), Type::Variant(y, ys, _)) => {
            x == y && xs.len() == ys.len()
        }
        (Type::Tuple(xs), Type::Tuple(ys)) => xs.len() == ys.len(),
        (Type::ByRef(m, _), Type::ByRef(n, _)) => m == n,
        _ => mem::discriminant(c) == mem::discriminant(t),
    }
}

/// True iff binding `t` into the cell would satisfy every conjunct of
/// the cell's constraints. A pure probe.
fn cell_constraints_ok(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    let probe = guide(flags);
    for c in tv.cell_constraints().iter() {
        if !c.contains_int(probe, env, hist, t)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Whether `q`, when a rank-2 formal's quantifier holds it rigid at a
/// call, admits unifying with the open cell `o`: the callee was checked
/// against `q`'s declared bounds alone, so each of `o`'s conjuncts must
/// follow from them. A quantifier with no bound is not judged.
fn quantifier_admits(env: &Env, q: &TVar, o: &TVar) -> bool {
    if !q.is_quantifier_rigid() || o.is_rigid() {
        return true;
    }
    let own = q.cell_constraints();
    if own.is_empty() {
        return true;
    }
    // bounds that name each other (`'a: [i64, ('a, 'b)]`) reach this pair
    // again while judging it: coinductively, it holds there
    let pair = (Arc::as_ptr(&q.cell()) as usize, Arc::as_ptr(&o.cell()) as usize);
    let Some(_judging) = Judging::enter(pair) else { return true };
    let trait_of = |t: &Type| match t {
        Type::Ref(tr) => env.trait_of_ref(tr),
        _ => None,
    };
    o.cell_constraints().iter().all(|c| {
        own.iter().any(|q| match (trait_of(q), trait_of(c)) {
            (Some(a), Some(b)) => a == b,
            (None, None) => q.implies(env, c).unwrap_or(false),
            _ => false,
        })
    })
}

thread_local! {
    static JUDGING: RefCell<Vec<(usize, usize)>> = const { RefCell::new(Vec::new()) };
}

/// A quantifier/cell pair [`quantifier_admits`] is judging on this thread.
struct Judging((usize, usize));

impl Judging {
    /// `None` when the pair is already being judged.
    fn enter(pair: (usize, usize)) -> Option<Self> {
        JUDGING.with_borrow_mut(|j| {
            (!j.contains(&pair)).then(|| {
                j.push(pair);
                Self(pair)
            })
        })
    }
}

impl Drop for Judging {
    fn drop(&mut self) {
        JUDGING.with_borrow_mut(|j| {
            if let Some(i) = j.iter().rposition(|p| *p == self.0) {
                j.remove(i);
            }
        })
    }
}

impl Type {
    /// Whether the declared bound `self` implies the bound `c`: it is `c`,
    /// or holds it of every type it admits (a predicate only where none of
    /// those has an unknown part), and `Ordered` implies `Discernible`.
    pub fn implies(&self, env: &Env, c: &Type) -> Result<bool> {
        if strictly_identical(self, c)
            || matches!(
                (self, c),
                (Type::Bound(Bound::Ordered), Type::Bound(Bound::Discernible))
            )
        {
            return Ok(true);
        }
        if self.is_predicate() || (c.is_predicate() && self.holds_unknown(env)) {
            return Ok(false);
        }
        // the bound holds whatever types the declaring side's variables take
        let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
        hold_rigid(self, &mut gates);
        c.contains_with_flags(ContainsFlags::RigidCheck.into(), env, self)
    }
}

/// Hold every open cell of `t` rigid while `gates` lives.
pub(super) fn hold_rigid(t: &Type, gates: &mut Vec<RigidGate>) {
    find_open_cell(t, true, &mut |tv| {
        gates.push(tv.open_rigid());
        false
    });
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
        // XCR claude for claude: [bug/medium] When an open cell meets a rigid one this
        // returns Merge, and `TVar::merge_into` then adds the other cell's conjuncts to
        // the rigid cell without checking that the rigid cell's declared bound implies
        // them. That absorption is right for a definition's own declared variables
        // during its own check. It is wrong for the rigid gates `FnType::contains_int`
        // opens on a rank-2 formal's quantifiers, because the callee's body was already
        // checked against the declared bound alone. So `app(add)` passes the check,
        // where `app` takes `f: fn<'b: Number>(..)` and `add` declares `'a: Int`. The
        // same holds for an implied Singleton, and for Ordered or Discernible under
        // `'b: Any`. The build then refuses the callee's instance of `f` with the
        // internal "an instance at .. of a definition typed .." error, while --check
        // and the LSP report the program clean. A fix: let a gate opened by
        // `open_quantifier_gates` refuse a merge that would add a conjunct its cell's
        // own conjuncts do not imply, so the argument is refused at the passing site
        // with a message naming the missing bound. probe:
        // design/review-2026-10-12/repro/x-typecheck-generics-02.gx
        // (x-typecheck-generics-02)
        // 2026-10-13 claude: partly fixed. A call's rank-2 gate (callsite.rs
        // quantified_formal) now refuses an argument whose type conjuncts its bound does
        // not imply (quantifier_admits): app(add) with 'a: Int under 'b: Number is
        // refused at the passing site (lang::types::rank2_refuses_narrower_bound).
        // Re-addressed for the predicates: refusing an implied Singleton would refuse the
        // pinned NESTED_QUANTIFIER_TWO_TYPES (apply(sq), sq = |x| x * x under 'b:
        // Number), which the build accepts since apply calls f at concrete types. Is a
        // rank-2 'b: Number argument held to Singleton, Ordered and Discernible? Also
        // open: a quantifier copy that reaches a later comparison without its bound
        // (the instance re-check of a variadic builtin's fn(x: Eq), and of a collection
        // element's fn<'a: Number>) is not judged, so 'b with no type conjunct is
        // skipped; FnType::contains_int's own quantifier gates are not judged either.
        // 2026-10-24 claude: Eric ruled (2026-10-24): a rank-2 quantifier keeps exactly
        // its declared bounds (fn<'b: Number> must say 'b: Number + Singleton to take |x|
        // x * x); the NESTED_QUANTIFIER pins change.
        // 2026-10-24 claude: Ruling built: callsite::added_bound now holds a rank-2
        // quantifier to exactly its declared bounds, predicates included, through the
        // shared Type::implies (the rule sig_matches uses), so fn<'b: Number> refuses |x|
        // x * x ('b: Singleton undeclared) and Number + Singleton takes it;
        // quantifier_admits judges predicates too. Pins: lang::types
        // NESTED_QUANTIFIER_SQUARE (now Number + Singleton, 9),
        // NESTED_QUANTIFIER_SQUARE_UNDECLARED (refused), NESTED_QUANTIFIER_TWO_TYPES
        // (Number + Singleton, 15), rank2_refuses_narrower_bound. Still open from the
        // 10-13 note: a quantifier copy that reaches a later comparison without its bound
        // (a variadic builtin's fn(x: Eq), a collection element's fn<'a: Number>) has no
        // conjunct and is not judged, and FnType::contains_int's own quantifier gates are
        // not judged.
        // 2026-10-24 claude: Later the same day: the bound-less quantifier copy was
        // Type::resolve_tvars, which CallSite uses for the instance's type (site_ftype)
        // and which gave every open cell a fresh copy without its bounds, so fn<'a:
        // Function> reached an instance as fn(..) and added_bound refused filter(queuefn,
        // ..) (lang::types::function_bounded_builtin_passed_as_a_value). A function
        // type's open quantifiers now keep their bounds through resolve_tvars
        // (ResolveTvarsCx::quantifiers). Still open: FnType::contains_int's own
        // quantifier gates are not judged.
        // 2026-10-27 claude: Still open, and it needs a ruling. FnType::contains_int
        // holds the quantifiers of a written fn type rigid wherever it stands, but its
        // gates are plain rigid ones, so quantifier_admits does not judge them and a
        // merge absorbs the argument's bound into them. Witness:
        // design/review-2026-10-12/repro/x-typecheck-generics-02c-typedef-field.gx (type
        // S = {f: fn<'b: Number>(..) -> 'b}; let s: S = {f: add} with add at 'a: Int;
        // s.f(1.5, 2.5)): --check passes, the run fails 'binding the callee failed .. an
        // instance at fn(x: f64, y: f64) -> f64 of a definition typed fn<'a: Int +
        // Singleton>'. A plain let annotation (let f: fn<'b: Number>(x: 'b, y: 'b) -> 'b
        // = add) absorbs instead: 'b becomes Number & Int & Singleton, f(1, 2) checks and
        // runs, f(1.5, 2.5) is refused at the call. The question: is a quantified fn type
        // written outside a formal (a let annotation, a struct field, a typedef, a return
        // type) rank-2, held to exactly its declared bounds as a formal's is? (A) Yes:
        // open_quantifier_gates opens quantifier gates (the one-line change, tried
        // 2026-10-27): the witness and the let are refused at the binding ('S does not
        // contain {f: fn<'a: Int + Singleton>..}'), but
        // lang::collection::collection_returns_a_generic_function then passes the check
        // and fails elaboration, because the callback instance's recorded return type is
        // fn<'a: Number> while the lambda's own type gained Singleton from its body
        // (array::map([1, 2], |r| 'a: Number |#start: 'a = 0, x: 'a| -> 'a start + x));
        // that needs fixing alongside (the lambda's published type must be its checked
        // one), and a let over a narrower function stops checking. (B) No, the
        // annotation's quantifier absorbs: then the typedef case must keep the absorbed
        // cell per binding instead of re-expanding S at each s.f, which the Ref form
        // cannot hold today. (A) matches the comment in FnType::contains_int ('wherever
        // the type stands') and the formal rule; (B) is what a let does today.
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q22,
        // discussed): (A). A written fn<'b: C>(..) means the same wherever it is written:
        // a let annotation, a struct field, a typedef and a return type hold a value to
        // every 'b the bound admits, as a formal does.
        // 2026-10-27 claude: Ruling (A) built: FnType::contains_int opens quantifier
        // gates (open_quantifier_rigid) on the containing side's open quantifiers, so a
        // written fn<'b: C> holds what it is given to exactly its declared bounds
        // wherever it stands. The witness and the let are refused at the binding ('S does
        // not contain { f: fn<'a: Int + Singleton>..'). Two follow-ons: quantifier_admits
        // judges a pair coinductively (thread-local Judging set), since bounds that name
        // each other re-entered it without end and exhausted memory
        // (lang::image::a_definition_entered_three_times_restores); an instance's lambda
        // checks its written type over its definition's recorded one with
        // ContainsFlags::Adopt (Type::check_adopts), adopting a bound the body added
        // (lang::collection::collection_returns_a_generic_function).
        // trait_any_bound_does_not_imply_ordered is now refused by the impl's annotation,
        // message updated. Pins: lang::types::{rank2_struct_field_refuses_narrower_bound,
        // rank2_let_annotation_refuses_narrower_bound,
        // rank2_let_annotation_holds_each_instantiation}. Docs: CLAUDE.md,
        // design/tvar_constraints.md. GUI suite 3.30 s for 338 tests at load 41.
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
struct Link {
    commit: bool,
    found: NodePairs,
}

impl Link {
    fn equal(&mut self, t0: &Type, t1: &Type) -> bool {
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
                    // Unreachable under an eq-true verdict.
                    _ => true,
                }
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                f0.args
                    .iter()
                    .zip(f1.args.iter())
                    .all(|(a, b)| self.equal(&a.typ, &b.typ))
                    && match (&f0.vargs, &f1.vargs) {
                        (Some(a), Some(b)) => self.equal(a, b),
                        _ => true,
                    }
                    && self.equal(&f0.rtype, &f1.rtype)
                    && self.equal(&f0.throws, &f1.throws)
            }
            (Type::Ref(r0), Type::Ref(r1)) => self.all(&r0.params, &r1.params),
            (Type::Set(a), Type::Set(b))
            | (Type::Tuple(a), Type::Tuple(b))
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
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                self.equal(k0, k1) && self.equal(v0, v1)
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
    /// A name the two sides write at one place for two definitions, and
    /// where each is written: taken while the definitions are in scope.
    apart: Option<(ModPath, SourcePosition, SourcePosition)>,
}

impl std::fmt::Display for TypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            let (expected, actual) = (
                format_compact!("{}", Brief(&self.expected)),
                format_compact!("{}", Brief(&self.actual)),
            );
            write!(f, "type mismatch {expected} does not contain {actual}")?;
            // sides that print alike name two definitions of one name
            if expected == actual
                && let Some((name, p0, p1)) = &self.apart
            {
                write!(
                    f,
                    ": the two {name} are different definitions, at {p0} and {p1}"
                )?;
            }
            Ok(())
        })
    }
}

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
            [Type::Ref(r0), Type::Ref(r1)] if r0.same_definition(r1) => {
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
            [Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }] => {
                self.walk([k0, k1]).or_else(|| self.walk([v0, v1]))
            }
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

impl std::error::Error for TypeMismatch {}

impl Type {
    /// Both nodes wrap the same content allocation, so containment holds
    /// reflexively. `TVar`/`Ref` keep their full arms: cheap and delicate.
    pub fn shares_content(&self, t: &Type) -> bool {
        same_content(self, t)
    }
}

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

/// The refusal of a comparison of two instances of `name` whose
/// parameters grow at every level.
fn growth_error(name: &ModPath) -> anyhow::Error {
    anyhow::anyhow!(
        "cannot compare two instances of {name}: its parameters grow at every \
         level of its recursion, so no comparison ends"
    )
}

/// The deref/expansion steps [`Type::set_covers_by_distribution`] takes
/// to reach a head constructor before giving up on a chain of aliases.
const HEAD_CHAIN_LIMIT: usize = 64;

impl Type {
    pub fn check_contains(&self, env: &Env, t: &Self) -> Result<()> {
        let mut hist = ContainsHist::new();
        let ok = self.contains_int(ContainsFlags::Commit.into(), env, &mut hist, t)?;
        if graphix_dbg_bind() {
            eprintln!("CHK-CONTAINS {self} >= {t} -> {ok}");
        }
        match (ok, hist.grew) {
            (true, _) => Ok(()),
            (false, Some(name)) => Err(growth_error(&name)),
            (false, None) => Err(self.contains_mismatch(env, t)),
        }
    }

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
        if let Some(e) = self.uncastable_refusal(env, t) {
            return e;
        }
        anyhow::Error::new(TypeMismatch {
            expected: self.clone(),
            actual: t.clone(),
            apart: same_name_apart(self, t),
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
        if let Some(a) = data.uncastable_but(env, &|id| own.contains(&id)) {
            return Some(anyhow::anyhow!(
                "{a} has no Cast implementation, so no read, cast or writer reaches \
                 it: implement Cast for it where it is defined"
            ));
        }
        let part = DataMeet::new(env).walk([self, t])?;
        Some(anyhow::anyhow!(
            "{} is not data: data holds no function and no reference",
            Brief(&part)
        ))
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
            let members = Self::dispatch_members(env, t).ok()?;
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
                        crate::written_scope(&im.scope)
                    ),
                });
            }
            if let Some(why) = Self::dispatch_collision(env, &members) {
                let open =
                    |t: &Type| matches!(t, Type::TVar(tv) if tv.open_cell().is_some());
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
                return Some(why.untold(&what, t, t));
            }
        }
        None
    }

    /// The members a dispatch over `t` tests: a union flattened through
    /// its type references, a union of primitives split into its
    /// primitives and an open cell kept as one member.
    pub fn dispatch_members(env: &Env, t: &Self) -> Result<SmallVec<[Type; 8]>> {
        fn go(env: &Env, t: &Type, out: &mut SmallVec<[Type; 8]>) -> Result<()> {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => match tv.binding() {
                    Some(b) => go(env, &b, out),
                    None => Ok(out.push(t.clone())),
                },
                Type::Set(ts) => ts.iter().try_for_each(|t| go(env, t, out)),
                Type::Ref(_) => go(env, &t.lookup_ref(env)?, out),
                Type::Primitive(p) => {
                    Ok(out.extend(p.iter().map(|b| Type::Primitive(b.into()))))
                }
                t => Ok(out.push(t.clone())),
            })
        }
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        go(env, t, &mut members)?;
        Ok(members)
    }

    /// Two members a dispatch's runtime test can't tell apart.
    fn dispatch_collision(env: &Env, members: &[Type]) -> Option<Indiscernible> {
        members.iter().enumerate().find_map(|(i, a)| {
            members[i + 1..].iter().find_map(|b| a.rep_collision_open(env, b))
        })
    }

    /// Why `self ⊇ t` fails when `self` holds an open `Discernible` or
    /// `Ordered` cell and `t` is not that.
    pub fn discernible_refusal(&self, env: &Env, t: &Self) -> Option<anyhow::Error> {
        let (tv, bound) = discernible_cell(self)?;
        let why =
            t.indiscernible(env, Open::Benign, bound == Type::Bound(Bound::Ordered))?;
        Some(t.not_discernible(&bound, &tv.name, &why))
    }

    /// [`Self::check_contains`] with rigid enforcement; the def gate's
    /// acceptance checks only.
    /// [`Self::check_contains`] of a function's written type over its
    /// checked type `t`, adopting `t`'s quantifier bounds
    /// ([`ContainsFlags::Adopt`]).
    pub fn check_adopts(&self, env: &Env, t: &Self) -> Result<()> {
        let flags = ContainsFlags::Commit | ContainsFlags::Adopt;
        let ok = self.contains_int(flags, env, &mut ContainsHist::new(), t)?;
        if ok { Ok(()) } else { Err(self.contains_mismatch(env, t)) }
    }

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
        // a commit over two closed sides binds nothing: it is the probe
        let flags = match flags.contains(ContainsFlags::Commit) {
            true if hist.closed(self) && hist.closed(t) => BitFlags::empty(),
            _ => flags,
        };
        if flags.contains(ContainsFlags::Commit) {
            if hist.gated == 0 && hist.commit_held(flags, self, t) {
                return Ok(true);
            }
            // A committing call may bind a cell a cached verdict read.
            hist.epoch += 1;
            let (height, outer, outer_taint) = hist.enter_verdict();
            let r = self.contains_dispatch(flags, env, hist, t);
            let own = hist.leave_verdict(height, outer, outer_taint);
            let r = r?;
            if r && own && hist.gated == 0 {
                hist.commit_put(flags, self, t);
            }
            return Ok(r);
        }
        // a probe binds nothing: its verdict, rigid or not and in either
        // polarity, is memoized
        let probe = flags & !ContainsFlags::Contra;
        let rigid = probe == ContainsFlags::RigidCheck;
        if !probe.is_empty() && !rigid {
            return self.contains_dispatch(flags, env, hist, t);
        }
        if hist.gated == 0
            && let Some(r) = hist.probe_get(rigid, self, t)
        {
            return Ok(r);
        }
        // Only a verdict that assumed no pair from further out holds
        // outside this call.
        let (height, outer, outer_taint) = hist.enter_verdict();
        let r = self.contains_dispatch(flags, env, hist, t);
        let own = hist.leave_verdict(height, outer, outer_taint);
        let r = r?;
        if own && hist.gated == 0 {
            hist.probe_put(rigid, self, t, r);
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
            (Self::Bound(Bound::Concrete), t) => Ok(t.concrete_holds(env)),
            (_, Self::Bound(Bound::Concrete)) => Ok(false),
            (Self::Bound(Bound::Data), t) => Ok(t.data_holds(env)),
            (_, Self::Bound(Bound::Data)) => Ok(false),
            (Self::Bound(Bound::Function), t) => t.function_holds(env, commit),
            (_, Self::Bound(Bound::Function)) => Ok(false),
            (Self::Bound(Bound::Singleton), t) => t.singleton_holds(env, commit),
            (_, Self::Bound(Bound::Singleton)) => Ok(false),
            (Self::Bound(Bound::OneNumber), t) => t.one_number_holds(env, commit),
            (_, Self::Bound(Bound::OneNumber)) => Ok(false),
            (Self::Bound(Bound::NoError), t) => Ok(!t.may_be_error(env, commit)?),
            (_, Self::Bound(Bound::NoError)) => Ok(false),
            (Self::Bound(Bound::Discernible), t) => {
                Ok(t.indiscernible(env, Open::Benign, false).is_none())
            }
            (_, Self::Bound(Bound::Discernible)) => Ok(false),
            (Self::Bound(Bound::Ordered), t) => {
                Ok(t.indiscernible(env, Open::Benign, true).is_none())
            }
            (_, Self::Bound(Bound::Ordered)) => Ok(false),
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
                    && tr0.same_definition(tr1)
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
                // two instances of one definition whose parameters contain
                // each other are one type: no expansion, which a parameter
                // growing at each level would never end
                if let (Self::Ref(tr0), Self::Ref(tr1)) = (t0, t1)
                    && tr0.same_definition(tr1)
                    && tr0.params.len() == tr1.params.len()
                {
                    let pairs = || tr0.params.iter().zip(tr1.params.iter());
                    let equal = |f: BitFlags<ContainsFlags>, hist: &mut ContainsHist| {
                        for (a, b) in pairs() {
                            if !a.contains_int(f, env, hist, b)?
                                || !b.contains_int(f, env, hist, a)?
                            {
                                return Ok::<_, anyhow::Error>(false);
                            }
                        }
                        Ok(true)
                    };
                    let probe = flags & !BitFlags::from(ContainsFlags::Commit);
                    if equal(probe, hist)? && (!commit || equal(flags, hist)?) {
                        return Ok(true);
                    }
                }
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                let defs = [(t0, false), (t1, true)].map(|(t, right)| match t {
                    Self::Ref(tr) => {
                        tr.def_key().map(|d| ((d, right), params_size(&tr.params)))
                    }
                    _ => None,
                });
                let mut saved: SmallVec<[((usize, bool), Option<(usize, u32)>); 2]> =
                    SmallVec::new();
                let restore =
                    |hist: &mut ContainsHist,
                     saved: &[((usize, bool), Option<(usize, u32)>)]| {
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
                        // a probe answers; a commit that grows past the limit
                        // is an error the program sees
                        let name = match (t0, t1) {
                            (Self::Ref(tr), _) | (_, Self::Ref(tr)) => tr.name.clone(),
                            _ => unreachable!("a definition key is a reference's"),
                        };
                        if !commit {
                            hist.grew = Some(name);
                            return Ok(false);
                        }
                        return Err(growth_error(&name));
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
            // cell answers for its binding; a rigid one may be anything.
            (Self::Bottom, Self::TVar(t0)) => match t0.binding() {
                Some(b) => Self::Bottom.contains_int(flags, env, hist, &b),
                None if (rigid || commit) && t0.is_rigid() => Ok(false),
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
                if !cell_constraints_ok(t0, flags, env, hist, &Self::Any)? {
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
            // in a contravariant position Any binds an open cell (a callback
            // that takes any value fixes its formal's type), never under a
            // covariant Any, where `let x: Any = y` decides nothing of `y`
            (Self::Any, Self::TVar(t1)) => {
                if commit
                    && flags.contains(ContainsFlags::Contra)
                    && let Some(cell) = t1.open_cell()
                    && !cell.is_rigid()
                    && cell_constraints_ok(&cell, flags, env, hist, &Self::Any)?
                    && commit_cell_constraints(&cell, flags, env, hist, &Self::Any)?
                {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "BIND rhs '{}({:x}) := Any",
                            cell.name,
                            cell.cell_addr()
                        );
                    }
                    cell.bind(Self::Any);
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
                // the representation is hidden, so its parameters are invariant
                Ok(p0.len() == p1.len()
                    && p0
                        .iter()
                        .zip(p1.iter())
                        .map(|(t0, t1)| t0.equal_int(flags, env, hist, t1))
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            (Self::Primitive(p0), Self::Primitive(p1)) => Ok(p0.contains(*p1)),
            (Self::Primitive(p), Self::Array(_)) => Ok(p.contains(Typ::Array)),
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
            (Self::ByRef(Mutability::Shared, t0), Self::ByRef(_, t1)) => {
                t0.contains_int(flags, env, hist, t1)
            }
            (Self::ByRef(Mutability::Mut, t0), Self::ByRef(Mutability::Mut, t1)) => {
                t0.equal_int(flags, env, hist, t1)
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
                if !cell_constraints_ok(t0, flags, env, hist, t1)? {
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
                        if t0.contains_int(guide(flags), env, hist, c)? {
                            return match commit {
                                true => t0.contains_int(flags, env, hist, c),
                                false => Ok(true),
                            };
                        }
                    }
                    return Ok(false);
                }
                if !cell_constraints_ok(t1, flags, env, hist, t0)? {
                    // a `NoError` cell below a union holding errors takes
                    // the union without them: `Primitive ⊇ 'r` gives `'r`
                    // every primitive but `error`
                    if t1.cell_constraints().contains(&Self::Bound(Bound::NoError))
                        && let Some(w) = t0.without_errors(env)?
                        && cell_constraints_ok(t1, flags, env, hist, &w)?
                    {
                        if commit && !t1.is_rigid() {
                            t1.bind(w);
                        }
                        return Ok(true);
                    }
                    // a union the cell's constraints refuse whole may
                    // still hold it in one member: `['b, null] ⊇ 'r`
                    // with `'r` within `Array<_>`, by `'b`; a union of
                    // primitives in one of its bits
                    match t0 {
                        Self::Set(s) => {
                            return Ok(Self::set_commit(s, flags, env, hist, t)?
                                .unwrap_or(false));
                        }
                        Self::Primitive(p) if p.len() > 1 => {
                            let bits: SmallVec<[Type; 4]> =
                                p.iter().map(|b| Self::Primitive(b.into())).collect();
                            return Ok(Self::set_commit(&bits, flags, env, hist, t)?
                                .unwrap_or(false));
                        }
                        _ => (),
                    }
                    // a t0 wider than the cell's witness holds it
                    // ('a ⊆ W ⊆ t0): the cell settles to W
                    let Ok(w) = t1.witness(env)? else { return Ok(false) };
                    if !t0.contains_int(guide(flags), env, hist, &w)? {
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
            // CR claude for claude: [bug/medium] filter_map's ['b, null, `Keep('b)]
            // refuses a callback returning [i64, `Keep(null)]. Both placement orders
            // here bind the bare 'b from one member and then refuse the other:
            // `Keep(null) fixes 'b to null and i64 no longer fits, or the reverse.
            // Nothing widens 'b to [i64, null], although 'b sits only in covariant
            // positions. So `select x { i64 as v => v + 1, null as _ => `Keep(null) }`
            // is refused, even with an explicit return annotation, while the all-`Keep
            // spelling of the same callback runs. Either let a bare cell that shares a
            // constructor sibling take the union of what both need, or document that
            // keeping a null forces `Keep on every kept value. probe:
            // design/review-2026-10-26/repro/gx-stdlib-keep-null-mixed.gx Also reported
            // as (x-engine-collections-03). (gx-stdlib-01)
            // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q30, The
            // `Keep marker is in-band):
            (t0 @ Self::Set(s0), Self::Set(s1)) if s0.iter().any(is_unbound_tvar) => {
                if !commit || !shares_bare_cell(s0) {
                    return Self::place_members(flags, env, hist, t0, s0, s1);
                }
                // the members as written, else deepest first: the deepest a
                // constructor member fits decides the variable its bare
                // sibling shares; each order is tried on copies first
                let mut deepest: SmallVec<[Type; 8]> = s1.iter().cloned().collect();
                deepest.sort_by_key(|m| std::cmp::Reverse(nesting(m)));
                for order in [&s1[..], &deepest[..]] {
                    let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                    let copy0 = t0.reset_tvars_int(&mut known, Fresh::Copy);
                    let copy0 = copy0.unwrap_or_else(|| t0.clone());
                    let copy1: SmallVec<[Type; 8]> = order
                        .iter()
                        .map(|m| {
                            m.reset_tvars_int(&mut known, Fresh::Copy)
                                .unwrap_or_else(|| m.clone())
                        })
                        .collect();
                    let Self::Set(c0) = &copy0 else {
                        unreachable!("a copy keeps its shape")
                    };
                    let trial = &mut ContainsHist::new();
                    if Self::place_members(flags, env, trial, &copy0, c0, &copy1)? {
                        return Self::place_members(flags, env, hist, t0, s0, order);
                    }
                }
                Self::place_members(flags, env, hist, t0, s0, s1)
            }
            // Member-wise identity pre-pass: only the residue takes the
            // general per-member walk (which is O(|s0|·|s1|) per level).
            // An open rhs member takes from the lhs members its known
            // siblings leave: in `[A, E] ⊇ ['b, E]`, 'b is A, never E; where
            // they leave none it can take, from the whole lhs.
            (t0 @ Self::Set(s0), Self::Set(s1)) => {
                let mut used: SmallVec<[bool; 8]> = smallvec::smallvec![false; s0.len()];
                let mut open: SmallVec<[&Type; 2]> = SmallVec::new();
                for m in s1.iter() {
                    if is_unbound_tvar(m) {
                        open.push(m);
                        continue;
                    }
                    if let Some(i) =
                        s0.iter().position(|c| identical_linked(c, m, commit))
                    {
                        used[i] = true;
                        continue;
                    }
                    if !t0.contains_int(flags, env, hist, m)? {
                        return Ok(false);
                    }
                    for (i, c) in s0.iter().enumerate() {
                        if c.contains_int(guide(flags), env, hist, m)? {
                            used[i] = true;
                            break;
                        }
                    }
                }
                if open.is_empty() {
                    return Ok(true);
                }
                let mut rest: SmallVec<[Type; 8]> = s0
                    .iter()
                    .zip(&used)
                    .filter(|(_, used)| !**used)
                    .map(|(c, _)| c.clone())
                    .collect();
                let rest = match rest.len() {
                    0 => t0.clone(),
                    1 => rest.pop().unwrap(),
                    _ => Self::Set(Arc::from_iter(rest.drain(..))),
                };
                for m in open {
                    let from = match rest.contains_int(guide(flags), env, hist, m)? {
                        true => &rest,
                        false => t0,
                    };
                    if !from.contains_int(flags, env, hist, m)? {
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
                        let mut uncovered = BitFlags::empty();
                        for b in p.iter() {
                            let bit = Self::Primitive(b.into());
                            if !Self::set_admits(&concrete, env, hist, &bit)? {
                                uncovered |= b
                            }
                        }
                        let bare = Self::residue_member(
                            s.iter().filter(|m| is_unbound_tvar(m)),
                            flags,
                            env,
                            hist,
                            &Self::Primitive(uncovered),
                        )?;
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
            | (Self::Map { .. }, Self::Error(_))
            | (Self::Primitive(_), Self::Tuple(_))
            | (Self::Primitive(_), Self::Struct(_))
            | (Self::Primitive(_), Self::Variant(_, _, _)) => Ok(false),
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
        // a probe answers the verdict alone; a commit records the refusal
        let refuse = || {
            if graphix_dbg_cycle_bt() {
                eprintln!(
                    "CYCLE-REFUSED-PAIR ({addr0:x},{addr1:x})\n{}",
                    std::backtrace::Backtrace::force_capture()
                );
            }
            if commit {
                t0.mark_cycle_refused();
                t1.mark_cycle_refused();
            }
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
                // a probe answers as the commit would: the cell's own
                // conjuncts must admit the binding
                if !cell_constraints_ok(t0, flags, env, hist, &b1)? {
                    return Ok(false);
                }
                if commit && !t0.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "TT-LEFTCOPY '{} <= '{}",
                            t0.read().id.inner(),
                            t1.read().id.inner()
                        );
                    }
                    if !commit_cell_constraints(t0, flags, env, hist, &b1)? {
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
                if !cell_constraints_ok(t1, flags, env, hist, &b0)? {
                    return Ok(false);
                }
                if commit && !t1.is_rigid() {
                    if graphix_dbg_bind() {
                        eprintln!(
                            "TT-RIGHTCOPY '{} <= '{}",
                            t1.read().id.inner(),
                            t0.read().id.inner()
                        );
                    }
                    if !commit_cell_constraints(t1, flags, env, hist, &b0)? {
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
                if !quantifier_admits(env, t0, t1) || !quantifier_admits(env, t1, t0) {
                    return Ok(false);
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
        let probe = guide(flags);
        let mut admitted = false;
        for m in members {
            if m.contains_int(probe, env, hist, t)? {
                admitted = true;
                if !flags.contains(ContainsFlags::Commit)
                    || m.contains_int(flags, env, hist, t)?
                {
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
        match hist.ref_id(t, env) {
            Some(id) => hist.question(Question::Distribution(id), false, |hist| {
                Self::set_covers_by_distribution_inner(flags, env, hist, s, t)
            }),
            None => Self::set_covers_by_distribution_inner(flags, env, hist, s, t),
        }
    }

    fn set_covers_by_distribution_inner(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        s: &Arc<[Type]>,
        t: &Self,
    ) -> Result<bool> {
        // a reference's head comes from the expansion cache, so the
        // questions about its parts are the memo's
        fn head(env: &Env, hist: &mut ContainsHist, t: &Type) -> Result<Type> {
            let mut cur = t.clone();
            for _ in 0..HEAD_CHAIN_LIMIT {
                cur = match &cur {
                    Type::TVar(_) => match cur.deref_cloned() {
                        Some(next) => next,
                        None => break,
                    },
                    // An unresolvable ref does not distribute; not an error.
                    Type::Ref(_) => {
                        let id = hist.ref_id(&cur, env);
                        match hist.expand_ref(&cur, id, env, false) {
                            Ok(Some(next)) => next,
                            Ok(None) | Err(_) => break,
                        }
                    }
                    _ => break,
                }
            }
            Ok(cur)
        }
        // a rigid cell never binds: a constant to the law
        let t = head(env, hist, t)?;
        if t.has_free() {
            return Ok(false);
        }
        let mut targs: LPooled<Vec<Type>> = LPooled::take();
        match &t {
            Type::Variant(_, args, _) => targs.extend(args.iter().cloned()),
            Type::Tuple(args) => targs.extend(args.iter().cloned()),
            Type::Struct(flds) => targs.extend(flds.iter().map(|(_, t, _)| t.clone())),
            Type::Error(arg) => targs.push((**arg).clone()),
            _ => return Ok(false),
        }
        let mut cands: LPooled<Vec<LPooled<Vec<Type>>>> = LPooled::take();
        for m in s.iter() {
            let m = head(env, hist, m)?;
            let args: Option<LPooled<Vec<Type>>> = match (&t, &m) {
                (Type::Variant(tt, ta, _), Type::Variant(mt, ma, _))
                    if tt == mt && ta.len() == ma.len() =>
                {
                    Some(ma.iter().cloned().collect())
                }
                (Type::Tuple(ta), Type::Tuple(ma)) if ta.len() == ma.len() => {
                    Some(ma.iter().cloned().collect())
                }
                (Type::Error(_), Type::Error(ma)) => {
                    let mut args: LPooled<Vec<Type>> = LPooled::take();
                    args.push((**ma).clone());
                    Some(args)
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
        if cands.len() < 2 {
            return Ok(false);
        }
        let probe = BitFlags::from(ContainsFlags::RigidCheck);
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
                    if !cands.iter().all(|c| c.iter().all(|t| !t.has_free())) {
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
        if !flags.contains(ContainsFlags::Commit) {
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
            // members that select the same candidates ask the same question
            // of the rest: it is asked once
            let mut asked: SmallVec<[SmallVec<[usize; 8]>; 4]> = SmallVec::new();
            for m in members.iter() {
                let mut picked: SmallVec<[usize; 8]> = SmallVec::new();
                for (i, c) in cands.iter().enumerate() {
                    if c[0].contains_int(
                        ContainsFlags::RigidCheck.into(),
                        env,
                        hist,
                        m,
                    )? {
                        picked.push(i)
                    }
                }
                if asked.contains(&picked) {
                    continue;
                }
                let sub: SmallVec<[&[Type]; 8]> =
                    picked.iter().map(|i| &cands[*i][1..]).collect();
                if !Self::covers_by_parts(env, hist, &sub, rest)? {
                    return Ok(false);
                }
                asked.push(picked);
            }
            Ok(true)
        })
    }

    /// `t0`, a union `s0` holding an open member, contains the members
    /// `s1` in that order: each goes to the first constructor member that
    /// fits it, and what none takes to the open member.
    fn place_members(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t0: &Self,
        s0: &[Type],
        s1: &[Type],
    ) -> Result<bool> {
        let commit = flags.contains(ContainsFlags::Commit);
        // the members free on entry: covering an rhs member may
        // bind one (`Array<'b> ⊇ Array<i64>`) before the residue
        // reaches it
        let free: SmallVec<[bool; 8]> = s0.iter().map(is_unbound_tvar).collect();
        let probe = guide(flags);
        let mut residue: LPooled<Vec<Type>> = LPooled::take();
        for m in s1.iter() {
            // An rhs member equal to the whole lhs set is
            // covered reflexively; as residue it would close a
            // cycle.
            let reflexive =
                m.deref_cloned().is_some_and(|md| identical_linked(t0, &md, commit));
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
            for (c, _) in s0.iter().zip(&free).filter(|(_, free)| !**free) {
                if c.contains_int(probe, env, hist, m)? {
                    if commit && !c.contains_int(flags, env, hist, m)? {
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
        let free = s0.iter().zip(&free).filter_map(|(m, free)| free.then_some(m));
        match Self::residue_member(free, flags, env, hist, &target)? {
            Some(m) => m.contains_int(flags, env, hist, &target),
            None => Ok(false),
        }
    }

    /// The member of a union's open `members` that takes the `residue` no
    /// other member covers: the first a probe admits it into (a rigid
    /// member admits only itself), else the first.
    fn residue_member<'a>(
        mut members: impl Iterator<Item = &'a Type> + Clone,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        residue: &Type,
    ) -> Result<Option<&'a Type>> {
        for m in members.clone() {
            if m.contains_int(guide(flags), env, hist, residue)? {
                return Ok(Some(m));
            }
        }
        Ok(members.next())
    }

    /// Containment both ways: an invariant position.
    fn equal_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        Ok(self.contains_int(flags, env, hist, t)?
            && t.contains_int(flags, env, hist, self)?)
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
                    // an unknown constructor's argument is invariant
                    (None, None) => Ok(c0.contains_int(flags, env, hist, c1)?
                        && a0.equal_int(flags, env, hist, a1)?),
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
                    // the filled type decides, by its constructor's variance
                    Some((ctor, last)) => {
                        Ok(Self::bind_ctor(c, &ctor, flags, env, hist)?
                            && match ctor.fill_hole(a) {
                                Some(filled) => {
                                    filled.contains_int(flags, env, hist, t1)?
                                }
                                None => a.equal_int(flags, env, hist, &last)?,
                            })
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
                            && match ctor.fill_hole(a) {
                                Some(filled) => {
                                    t0.contains_int(flags, env, hist, &filled)?
                                }
                                None => last.equal_int(flags, env, hist, a)?,
                            })
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
            if !cell_constraints_ok(cv, flags, env, hist, ctor)? {
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
        if union && env.trait_def(tid).is_some_and(|d| d.takes_self_beyond_receiver()) {
            return Ok(false);
        }
        // nor when its dispatch, which tests each member, can't tell two apart
        if union
            && env.trait_def(tid).is_some_and(|d| !d.hole)
            && Self::dispatch_collision(env, &Self::dispatch_members(env, t)?).is_some()
        {
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
                    let id = hist.ref_id(t, env).unwrap_or(usize::MAX);
                    hist.question(Question::Implements(id, tid), true, |hist| {
                        Self::trait_contains(tid, flags, env, hist, &e)
                    })
                }
            },
            // An impl's bound can ask the question again of the expanded
            // type: met again, it holds if the rest does.
            t => {
                let id = hist.ref_id(t, env).unwrap_or(usize::MAX);
                hist.question(Question::Implements(id, tid), true, |hist| {
                    Ok(find_impl_with(env, tid, t, hist)?.is_some())
                })
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use arcstr::literal;

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
    /// by its prefix, not 2^depth.
    #[test]
    fn brief_is_bounded() {
        for t in [shared(true), shared(false)] {
            let s = format_compact!("{}", Brief(&t));
            assert!(s.len() <= Brief::BYTES + '…'.len_utf8() && s.ends_with('…'), "{s}");
        }
        let small = Type::Primitive(Typ::I64.into());
        assert_eq!(format_compact!("{}", Brief(&small)), "i64");
    }

    /// `&T` and `&mut T` over one inner allocation are two types to every
    /// memo and every walk that visits a shared node once.
    #[test]
    fn reference_mutability_is_part_of_a_shared_node() {
        let env = Env::default();
        let inner = Arc::new(Type::Primitive(Typ::I64.into()));
        let shared = Type::ByRef(Mutability::Shared, inner.clone());
        let writable = Type::ByRef(Mutability::Mut, inner);
        assert!(shared.contains(&env, &writable).unwrap());
        assert!(!writable.contains(&env, &shared).unwrap());
        assert!(!union_identical(&shared, &writable));
        assert_ne!(crate::typ::norm_key(&shared), crate::typ::norm_key(&writable));
        assert_ne!(probe_key(&shared), probe_key(&writable));
        assert_ne!(node_addr(&shared), node_addr(&writable));
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

/// The impl of `trait_id` for `t`, its head matched in the caller's walk
/// `hist`, so a trait question met again inside an impl's bound (a
/// recursive typedef under a generic impl) reads as the one in progress.
pub(super) fn find_impl_with(
    env: &Env,
    trait_id: TraitId,
    t: &Type,
    hist: &mut ContainsHist,
) -> Result<Option<Arc<ImplDef>>> {
    let Some(list) = env.impls_all(trait_id) else { return Ok(None) };
    // An open cell inside `t` could still become anything, and two heads
    // that fit it would each commit a guess; a declared (rigid) cell is a
    // constant, which takes the impl's bounds as a body's call hands a
    // declared variable a bound it needs, so each call checks them.
    if t.has_free() {
        return Ok(None);
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
        let (hc, tc) = (
            head.contains_int(ContainsFlags::Commit.into(), env, hist, t)?,
            t.contains_int(ContainsFlags::Commit.into(), env, hist, &head)?,
        );
        if graphix_dbg_bind() {
            eprintln!("FIND-IMPL head={head:?} t={t:?} head>=t={hc} t>=head={tc}");
        }
        if hc && tc {
            return Ok(Some(im.clone()));
        }
    }
    if graphix_dbg_bind() {
        eprintln!("FIND-IMPL none for {t:?}");
    }
    Ok(None)
}

/// [`Env::find_impl`]: [`find_impl_with`] in a walk of its own.
pub(crate) fn find_impl(
    env: &Env,
    trait_id: TraitId,
    t: &Type,
) -> Result<Option<Arc<ImplDef>>> {
    find_impl_with(env, trait_id, t, &mut ContainsHist::new())
}
