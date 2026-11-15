use crate::{
    AddrMap, AddrSet, PrintFlag, SourcePosition,
    dbgenv::{graphix_dbg_bind, graphix_dbg_cycle_bt},
    env::{Env, ImplDef},
    expr::{ModPath, Told},
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        AbstractId, AndAc, AppKey, Bound, Brief, CoreTrait, FnType, Held, Indiscernible,
        Lazy, MapType, Mutability, NodePairs, NormKey, Open, RefHist, RefPair, TVar,
        TraitId, Type, TypeRef, Unfolded,
        cast::shares_form,
        node_addr,
        normalize::NormCx,
        probe_key,
        setops::{
            Identical, IdenticalMemo, one_type, renames, strictly_identical,
            union_identical,
        },
        tvar::{
            CellFact, CellLock, Cycle, Declared, Fresh, HeldQuantifiers, RigidGate,
            Standing, Toward, bare_path, cell_writes, cycle_toward, in_input,
            settled_cause_note, would_cycle_inner,
        },
        variance,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::{BitFlags, bitflags};
use netidx_value::Typ;
use nohash::IntMap;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    collections::hash_map::Entry,
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
    /// An open cell `self` reaches outside a function's arguments takes
    /// `t`'s part as a lower bound instead of binding to it: a constructed
    /// value's payload bounds its parameters, the context decides them.
    Lower,
    /// Under `Lower`, an open cell of the bound below a union holding an
    /// open cell is taken by that member, as a bare cell would take it,
    /// rather than bound to the whole union: where no binding holds every
    /// bound of a cell ([`commit_lower`]).
    IntoMember,
    /// Under `Contra`, an open cell `self` reaches takes `t`'s part as a
    /// lower bound instead of binding to it, a rigid cell too: a solve for
    /// an instance that a held type must contain decides each cell by every
    /// position that meets it, where a bind at an argument fixes it below
    /// what a later position needs.
    LowerInArgs,
    /// The rigid cells are a fixed signature's variables, whose bounds its
    /// callers read: an open cell merges into one only where its own
    /// conjuncts admit it, since the rigid cell may take on none, and one
    /// holds a predicate only where its own conjuncts imply it.
    Sealed,
    /// The walk is the probe of a cell's conjunct: an open cell it reaches
    /// that stands above a lower bound holding `t` holds `t` as it stands
    /// ([`held_by_lower`]), since it is not the cell being decided.
    Conjunct,
}

/// What a trial on copies takes from the walk it is made in
/// ([`ContainsHist::seed`]).
#[derive(Clone, Copy)]
enum Inherit {
    /// The pairs that walk was itself seeded with.
    Assumed,
    /// Those, and the pairs it is expanding: for the trial of a union
    /// with a bare open member, which any member of the other side may
    /// go to, so a definition's own recursion is placed on the copies.
    Expanding,
}

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
    /// Probe pair memo: `contains_int` verdicts for calls that do not
    /// commit, keyed by both sides' identities ([`Self::probe_side`]) and
    /// the probe's flags ([`probe_flags`]). Each entry pins both types so
    /// an address cannot be recycled under its key, and carries the
    /// [`cell_writes`] count it was reached at: a write to a cell may
    /// change a verdict that read it.
    probe_pairs: Lazy<AHashMap<MemoKey, (u64, bool)>>,
    /// Per verdict of `probe_pairs` over an open var and a type that
    /// holds no cell, once the pair is probed again: the var's cell,
    /// while nothing it reaches is written. Such a verdict read only the
    /// cells the var reaches and copies of impl heads ([`find_impl_with`]),
    /// which admit the same types throughout: only an impl's own check
    /// writes its cells, a conjunct its declared bounds imply or a fact no
    /// verdict reads. So it stands through a write to any other cell, and a
    /// chain of open cells is probed once however many of its links a call
    /// binds.
    probe_standing: Lazy<AHashMap<MemoKey, Standing>>,
    probe_pins: Lazy<Vec<Type>>,
    /// Verdicts that rest on assumptions still in progress, held while
    /// those stand: one is dropped when an assumption it may rest on is
    /// answered otherwise, and joins `probe_pairs` (a probe's) or
    /// `commit_pairs` (a commit's) once every one was answered as assumed.
    /// Pinned with `probe_pins`.
    provisional: Lazy<AHashMap<MemoKey, Provisional>>,
    /// The keys of `provisional` in the order they were reached: an
    /// assumption's verdicts are those reached after it was made.
    provisional_order: Lazy<Vec<MemoKey>>,
    /// Per pair [`Self::assuming`] took in progress, in the order taken:
    /// its own place while it is in progress, then the place of the pair
    /// its answer rests on (`usize::MAX` for none). A verdict held in
    /// `provisional` reads the pair it rests on through these
    /// ([`resting`]), so an answer that rests on a pair further out
    /// rewrites none of them.
    assumed: Lazy<Vec<usize>>,
    /// Committing pairs proven to hold, by both sides' identities and the
    /// flags: what the proof bound stays bound for the rest of the call,
    /// so the same pair met again holds without walking it (a DAG's
    /// shared nodes are met once per path otherwise). Pinned with
    /// `probe_pins`.
    commit_pairs: Lazy<AHashSet<MemoKey>>,
    /// An id for each application [`Self::pair_key`] met, by its
    /// definition and its parameters' keys, the application pinned with
    /// `probe_pins`.
    apps: Lazy<AHashMap<AppKey, usize>>,
    /// The cells those keys hold by address ([`TypeRef::key_cells`]).
    app_cells: Lazy<Vec<Arc<CellLock>>>,
    /// The id of each application node [`Self::pair_key`] met: a node's
    /// key never changes. Pinned with `probe_pins`.
    app_nodes: Lazy<AddrMap<usize>>,
    /// The questions in progress on this path, answered by assumption
    /// when met again.
    in_progress: SmallVec<[Question; 4]>,
    /// What the current verdict assumed of the walk in progress.
    rests: Rests,
    /// A cycle under a constructor refused a pair of cells: the reason a
    /// refusal gives ([`TypeMismatch`]).
    cycled: bool,
    /// [`Self::closure`]'s verdicts by node and cell identity, each with
    /// the [`cell_writes`] count it read the cells at, `None` for an open
    /// one [`Self::closed`] reached, which may rest on verdicts kept from
    /// an earlier count; each node and cell held so its address is not
    /// recycled under its key.
    closed: Lazy<AddrMap<(Closure, Option<u64>)>>,
    closed_pins: Lazy<Vec<Held>>,
    /// The one normalize pass of the unions this walk builds from members
    /// holding no cell and no function type ([`Self::normal_union`]):
    /// what it keeps of a node read nothing a walk can write.
    norm: Option<NormCx>,
    /// The unions [`Self::pool`] built, by the ids of the parts they pool.
    pools: Lazy<AHashMap<SmallVec<[usize; 8]>, Type>>,
    /// The walk is a trial on copies ([`on_copies`]): a member a union
    /// admits commits directly, whatever it leaves behind in the copies.
    trial: bool,
    /// The walk is the commit that follows a trial which held a cell's
    /// lower bounds ([`commit_lower`]): the cells below take their choice
    /// of how to hold theirs from a probe, as that trial did, and run no
    /// trial of their own.
    proved: bool,
    /// The walk places a union's members in an order a trial placed them
    /// all in ([`Type::place_in_order`]), and has met no choice since
    /// ([`Self::strays`]): that trial took each union below with two
    /// members holding open cells as written, and so does this placement,
    /// with no trial of its own.
    as_tried: bool,
    /// A union's first member that holds a type takes it (a walk that
    /// binds copies alone, a select's patterns), where any other walk
    /// refuses a choice between two ([`AmbiguousMember`]).
    ordered: bool,
    /// The walk pre-unifies an argument that is not checked yet: a decided
    /// member takes an undecided type only where no other member could.
    early: bool,
    /// [`identical_linked`]'s pairs found identical.
    identical: IdenticalMemo,
    /// The pairs holding an open cell that this walk is expanding a
    /// name of ([`Self::assuming`]), innermost last.
    unfolding: SmallVec<[(Type, Type); 2]>,
    /// In a trial: the pairs the walk it was made in was expanding and
    /// assuming, on the trial's copies ([`Self::seed`]).
    assumed_above: SmallVec<[(Type, Type); 2]>,
}

/// The verdicts a walk held before it took cells rigid
/// ([`ContainsHist::gate`]).
pub(super) struct Ungated {
    probe_pairs: Lazy<AHashMap<MemoKey, (u64, bool)>>,
    probe_standing: Lazy<AHashMap<MemoKey, Standing>>,
    provisional: Lazy<AHashMap<MemoKey, Provisional>>,
    provisional_order: Lazy<Vec<MemoKey>>,
    commit_pairs: Lazy<AHashSet<MemoKey>>,
}

/// A verdict memo's key: both sides' identities ([`ContainsHist::pair_key`],
/// a probe's [`ContainsHist::probe_side`]) and the flags the verdict was
/// reached under, a probe's as [`probe_flags`] keeps them.
type MemoKey = (NormKey, NormKey, BitFlags<ContainsFlags>);

/// The flags a probe's verdict is kept under: a plain or a rigid probe,
/// sealed or not, asks the same in either polarity, any other reads its
/// polarity ([`takes_lower`]).
fn probe_flags(flags: BitFlags<ContainsFlags>) -> BitFlags<ContainsFlags> {
    let plain = flags & !ContainsFlags::Contra;
    let rigid = plain & !ContainsFlags::Sealed;
    match plain.is_empty() || rigid == ContainsFlags::RigidCheck {
        true => plain,
        false => flags,
    }
}

/// What a verdict assumed of the walk in progress: the place in
/// [`ContainsHist::assumed`] of the shallowest pair in progress it took
/// to hold, the shallowest entries of `in_progress` it took to hold and
/// to fail (`usize::MAX` for none).
#[derive(Clone, Copy)]
struct Rests {
    pairs: usize,
    held: usize,
    failed: usize,
}

impl Rests {
    const NOTHING: Self =
        Rests { pairs: usize::MAX, held: usize::MAX, failed: usize::MAX };

    /// The questions taken to hold, or to fail.
    fn taken(&mut self, held: bool) -> &mut usize {
        match held {
            true => &mut self.held,
            false => &mut self.failed,
        }
    }

    #[inline]
    fn join(self, other: Self) -> Self {
        Rests {
            pairs: self.pairs.min(other.pairs),
            held: self.held.min(other.held),
            failed: self.failed.min(other.failed),
        }
    }

    /// Whether verdict `r`, reached by a verdict at `height`, is the
    /// pair's wherever it is met. contains is monotone in what it
    /// assumes, so a false that took only holds stays false had they been
    /// walked, and a true that took only failures stays true.
    fn settled(self, r: bool, height: usize) -> bool {
        match r {
            true => self.pairs >= height && self.held == usize::MAX,
            false => self.failed == usize::MAX,
        }
    }

    /// Whether a verdict at `height` assumed nothing from further out: what
    /// a commit bound it bound under no assumption.
    fn own(self, height: usize) -> bool {
        self.pairs >= height && self.held == usize::MAX && self.failed == usize::MAX
    }
}

/// A verdict that rests on assumptions in progress, and the
/// [`cell_writes`] count it was reached at.
#[derive(Clone, Copy)]
struct Provisional {
    verdict: bool,
    epoch: u64,
    rests: Rests,
}

/// What [`ContainsHist::closure`] knows of a type, least first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Closure {
    /// It reaches an open cell through bindings, or may.
    Open,
    /// It reaches no open cell through bindings; a bound cell's conjuncts
    /// and lower bounds may.
    Closed,
    /// It holds no cell.
    NoCell,
    /// It holds no cell and no function type: a copy of it is itself.
    Plain,
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
            probe_standing: Lazy::new(),
            probe_pins: Lazy::new(),
            provisional: Lazy::new(),
            provisional_order: Lazy::new(),
            assumed: Lazy::new(),
            commit_pairs: Lazy::new(),
            apps: Lazy::new(),
            app_cells: Lazy::new(),
            app_nodes: Lazy::new(),
            in_progress: SmallVec::new(),
            rests: Rests::NOTHING,
            cycled: false,
            closed: Lazy::new(),
            closed_pins: Lazy::new(),
            norm: None,
            pools: Lazy::new(),
            trial: false,
            proved: false,
            as_tried: false,
            ordered: false,
            early: false,
            identical: IdenticalMemo::default(),
            unfolding: SmallVec::new(),
            assumed_above: SmallVec::new(),
        }
    }

    pub(super) fn ordered() -> Self {
        ContainsHist { ordered: true, ..Self::new() }
    }

    /// The walk of a pre-unification ([`Type::pre_unify_arg`]).
    pub(super) fn early() -> Self {
        ContainsHist { early: true, ..Self::new() }
    }

    /// Put the pairs this walk assumes of the walk it is a trial of, and
    /// with [`Inherit::Expanding`] those it is expanding, in progress in
    /// `trial`, a walk over copies made with `known`: the trial meets the
    /// recursion of a name it is inside as this walk will, by assumption.
    /// Expanded there, a pair over the copies' cells is a question no
    /// walk has in progress, and a recursive definition with a bare
    /// parameter would place its members on copies again at every level,
    /// without end.
    fn seed(
        &self,
        trial: &mut Self,
        known: &mut AHashMap<usize, TVar>,
        env: &Env,
        inherit: Inherit,
    ) {
        let mut copy =
            |t: &Type| t.reset_tvars_int(known, Fresh::Copy).unwrap_or_else(|| t.clone());
        let expanding = match inherit {
            Inherit::Assumed => &[][..],
            Inherit::Expanding => &self.unfolding[..],
        };
        for (t0, t1) in self.assumed_above.iter().chain(expanding) {
            let (c0, c1) = (copy(t0), copy(t1));
            let key = (trial.ref_id(&c0, env), trial.ref_id(&c1, env));
            if let Entry::Vacant(e) = trial.hist.entry(key) {
                let assumed = trial.assumed.get_mut();
                e.insert(assumed.len());
                assumed.push(assumed.len())
            }
            trial.assumed_above.push((c0, c1))
        }
    }

    /// A trial and the walk it is a trial of part here: at a choice
    /// between a union's members, which the trial makes by their order
    /// and the walk judges.
    fn strays(&mut self) {
        self.as_tried = false
    }

    fn trial() -> Self {
        ContainsHist { trial: true, ..Self::ordered() }
    }

    /// The walk goes on with cells held rigid (a function type's open
    /// quantifiers, a union member taking a type as it stands): a verdict
    /// reached so depends on the hold, so until [`Self::ungate`] the walk
    /// keeps its verdicts in memos of their own, and reads none of those
    /// it held before, which are returned.
    pub(super) fn gate(&mut self) -> Ungated {
        Ungated {
            probe_pairs: mem::replace(&mut self.probe_pairs, Lazy::new()),
            probe_standing: mem::replace(&mut self.probe_standing, Lazy::new()),
            provisional: mem::replace(&mut self.provisional, Lazy::new()),
            provisional_order: mem::replace(&mut self.provisional_order, Lazy::new()),
            commit_pairs: mem::replace(&mut self.commit_pairs, Lazy::new()),
        }
    }

    /// The hold [`Self::gate`] began is over: its verdicts are dropped,
    /// and those held before it stand again.
    pub(super) fn ungate(&mut self, outer: Ungated) {
        let Ungated {
            probe_pairs,
            probe_standing,
            provisional,
            provisional_order,
            commit_pairs,
        } = outer;
        self.probe_pairs = probe_pairs;
        self.probe_standing = probe_standing;
        self.provisional = provisional;
        self.provisional_order = provisional_order;
        self.commit_pairs = commit_pairs;
    }

    /// A walk several relations share begins the next. The pairs a commit
    /// proved are dropped, as a walk of the relation's own would hold none:
    /// its caller writes cells between two relations. What carries over is
    /// a probe's verdicts, each stamped with [`cell_writes`], and what no
    /// cell changes.
    fn next_relation(&mut self) {
        debug_assert!(self.in_progress.is_empty() && self.unfolding.is_empty());
        if let Some(held) = &mut self.commit_pairs.0 {
            held.clear()
        }
        if let Some(held) = &mut self.provisional.0 {
            held.clear()
        }
        if let Some(order) = &mut self.provisional_order.0 {
            order.clear()
        }
    }

    /// Whether the walk's verdict is the check's own: neither a trial nor
    /// ordered, and over types their own checks have made.
    fn decides(&self) -> bool {
        !(self.trial || self.ordered || self.early)
    }

    /// Whether `t` reaches no open cell, through bindings: a commit over
    /// two closed sides binds nothing, so it is the probe. Within a walk a
    /// closed node stays closed (cells only bind), and one found open
    /// only costs the commit's walk.
    pub(super) fn closed(&mut self, t: &Type) -> bool {
        self.closure(t, false) != Closure::Open
    }

    /// [`Self::closed`] as the cells stand now: a verdict reached before
    /// a cell was written is reached again.
    fn closed_now(&mut self, t: &Type) -> bool {
        self.closure(t, true) != Closure::Open
    }

    /// Whether a member `m` that holds `t` takes it and binds nothing:
    /// neither reaches an open cell as the cells stand now.
    fn binds_nothing(&mut self, m: &Type, t: &Type) -> bool {
        self.closed_now(m) && self.closed_now(t)
    }

    /// Whether `t`, a node, holds no cell, so it reaches none: no occurs
    /// check finds a cycle through it.
    fn holds_no_cell(&mut self, t: &Type) -> bool {
        t.shared_identity().is_some() && self.closure(t, false) >= Closure::NoCell
    }

    /// [`names_members_by`], each parameter's verdict kept by node
    /// ([`Self::closure`]).
    fn names_members(&mut self, env: &Env, m: &Type) -> bool {
        names_members_by(env, m, &mut |p| self.closure(p, false) < Closure::NoCell)
    }

    /// Whether [`Self::closure`] has found `t` [`Closure::Plain`], without
    /// a walk.
    fn known_plain(&self, t: &Type) -> bool {
        let closed = |k| self.closed.get().and_then(|m| m.get(&k)).map(|(c, _)| *c);
        t.shared_identity().is_some_and(|k| closed(k) == Some(Closure::Plain))
    }

    fn closure(&mut self, t: &Type, now: bool) -> Closure {
        ensure_sufficient(|| {
            let key = t.shared_identity();
            let writes = cell_writes();
            if let Some(k) = key {
                match self.closed.get().and_then(|m| m.get(&k)).copied() {
                    Some((c, at))
                        if !now || c >= Closure::NoCell || at == Some(writes) =>
                    {
                        return c;
                    }
                    Some(_) => (),
                    None => self.closed_pins.get_mut().push(Held::of(t)),
                }
                // a binding that reaches its own cell is not closed
                self.closed.get_mut().insert(k, (Closure::Open, Some(writes)));
            }
            let c = match t {
                Type::TVar(tv) if tv.ground_now() => Closure::Closed,
                Type::TVar(tv) => match tv.binding() {
                    Some(b) => self.closure(&b, now).min(Closure::Closed),
                    None => Closure::Open,
                },
                t => {
                    let mut c = match t {
                        Type::Fn(_) => Closure::NoCell,
                        _ => Closure::Plain,
                    };
                    let _ = t.try_for_each_child(&mut |p| {
                        c = c.min(self.closure(p, now));
                        match c {
                            Closure::Open => ControlFlow::Break(()),
                            Closure::Closed | Closure::NoCell | Closure::Plain => {
                                ControlFlow::Continue(())
                            }
                        }
                    });
                    c
                }
            };
            if let Some(k) = key {
                let at = (now || c != Closure::Open).then_some(writes);
                self.closed.get_mut().insert(k, (c, at));
            }
            c
        })
    }

    /// The union of `members` in normal form, each node they share
    /// normalized once a walk where none holds a cell or a function type.
    fn normal_union<'a>(
        &mut self,
        members: impl Iterator<Item = &'a Type> + Clone,
    ) -> Type {
        let plain = |hist: &mut Self, m: &Type| hist.closure(m, false) == Closure::Plain;
        if members.clone().all(|m| plain(self, m)) {
            Type::normal_union(members, self.norm.get_or_insert_with(NormCx::take))
        } else {
            Type::normal_union(members, &mut NormCx::take())
        }
    }

    /// [`Self::normal_union`] of `parts`, the one node for every union of
    /// parts of the same ids ([`RefHist::ref_id`]) holding no cell and no
    /// function type: its verdicts are memoized by its node.
    fn pool<'a>(
        &mut self,
        env: &Env,
        parts: impl Iterator<Item = &'a Type> + Clone,
    ) -> Type {
        let plain = parts.clone().all(|p| self.closure(p, false) == Closure::Plain);
        let key = match plain {
            true => parts.clone().map(|p| self.ref_id(p, env)).collect(),
            false => None,
        };
        let Some(key) = key else { return self.normal_union(parts) };
        if let Some(pool) = self.pools.get().and_then(|m| m.get(&key)) {
            return pool.clone();
        }
        let pool = self.normal_union(parts);
        self.pools.get_mut().insert(key, pool.clone());
        pool
    }

    /// Cached probe verdict for `(t0, t1)` under `flags`
    /// ([`probe_flags`]), if current: no cell was written since, neither
    /// side holds one, or none its var reaches was (`probe_standing`). A
    /// provisional one passes the assumptions it rests on to the verdict
    /// reading it.
    fn probe_get(
        &mut self,
        flags: BitFlags<ContainsFlags>,
        t0: &Type,
        t1: &Type,
    ) -> Option<bool> {
        let k = (self.probe_side(t0)?, self.probe_side(t1)?, flags);
        let writes = cell_writes();
        if let Some((epoch, r)) = self.probe_pairs.get().and_then(|m| m.get(&k).copied())
        {
            let stands = |hist: &Self| {
                let standing = hist.probe_standing.get().and_then(|m| m.get(&k));
                match (standing, t0, t1) {
                    (Some(s), Type::TVar(tv), _) | (Some(s), _, Type::TVar(tv)) => {
                        s.holds(tv)
                    }
                    _ => false,
                }
            };
            let no_cell =
                |hist: &mut Self, t: &Type| hist.closure(t, false) >= Closure::NoCell;
            let current = epoch == writes
                || (no_cell(self, t0) && no_cell(self, t1))
                || stands(self);
            if current {
                return Some(r);
            }
        }
        let p = self.provisional.get()?.get(&k).copied()?;
        if p.epoch != writes {
            return None;
        }
        self.rests = self.rests.join(self.rests_now(p.rests));
        Some(p.verdict)
    }

    /// Memoize a probe's verdict, reached by a verdict at `height`
    /// assuming `rests`: for good where it is settled, else while what it
    /// rests on stands.
    fn probe_put(
        &mut self,
        (flags, t0, t1): (BitFlags<ContainsFlags>, &Type, &Type),
        r: bool,
        height: usize,
        rests: Rests,
    ) {
        let (Some(k0), Some(k1)) = (self.probe_side(t0), self.probe_side(t1)) else {
            return;
        };
        let k = (k0, k1, flags);
        let epoch = cell_writes();
        let new = if rests.settled(r, height) {
            let again = self.probe_pairs.get_mut().insert(k, (epoch, r)).is_some();
            if again {
                let standing = match (t0, t1) {
                    (Type::TVar(tv), t) | (t, Type::TVar(tv))
                        if !tv.is_bound()
                            && self.closure(t, false) >= Closure::NoCell =>
                    {
                        Standing::of(tv)
                    }
                    _ => None,
                };
                match (standing, &mut self.probe_standing.0) {
                    (Some(s), _) => drop(self.probe_standing.get_mut().insert(k, s)),
                    (None, Some(held)) => drop(held.remove(&k)),
                    (None, None) => (),
                }
            }
            !again
        } else {
            let p = Provisional { verdict: r, epoch, rests };
            self.provisional_order.get_mut().push(k);
            self.provisional.get_mut().insert(k, p).is_none()
        };
        if new {
            let pins = self.probe_pins.get_mut();
            pins.push(t0.clone());
            pins.push(t1.clone());
        }
    }

    /// An assumption that its question is `assumed`, made when `mark`
    /// provisional verdicts were held, is answered `r`. Those reached
    /// since may rest on it: one that agrees with a wrong assumption is
    /// dropped (a disagreeing probe stands, by monotonicity; a commit
    /// bound what it bound under the assumption, and is dropped), the rest
    /// take what the answer rested on, a pair's from `assumed` and a
    /// question's from `update`, and each then settled is held for good.
    fn answer_assumption(
        &mut self,
        mark: usize,
        assumed: bool,
        r: &Result<bool>,
        update: impl Fn(&mut Rests),
    ) {
        let wrong = match r {
            Ok(r) if *r == assumed => None,
            Ok(_) => Some(assumed),
            Err(_) => {
                self.drop_provisional(mark);
                return;
            }
        };
        let Some(order) = self.provisional_order.get() else { return };
        if order.len() <= mark {
            return;
        }
        let mut order = mem::take(self.provisional_order.get_mut());
        let map = self.provisional.get_mut();
        let assumed = self.assumed.get_mut();
        let mut kept = mark;
        for i in mark..order.len() {
            let k = order[i];
            let Some(p) = map.get_mut(&k) else { continue };
            #[cfg(test)]
            VERDICTS_ANSWERED.with(|n| n.set(n.get() + 1));
            let commit = k.2.contains(ContainsFlags::Commit);
            if wrong == Some(p.verdict) || (commit && wrong.is_some()) {
                map.remove(&k);
                continue;
            }
            p.rests.pairs = resting(assumed, p.rests.pairs);
            update(&mut p.rests);
            match commit {
                true if p.rests.own(usize::MAX) => {
                    map.remove(&k);
                    self.commit_pairs.get_mut().insert(k);
                }
                false if p.rests.settled(p.verdict, usize::MAX) => {
                    let p = map.remove(&k).unwrap();
                    self.probe_pairs.get_mut().insert(k, (p.epoch, p.verdict));
                    if let Some(held) = &mut self.probe_standing.0 {
                        held.remove(&k);
                    }
                }
                _ => {
                    order[kept] = k;
                    kept += 1;
                }
            }
        }
        order.truncate(kept);
        *self.provisional_order.get_mut() = order;
    }

    fn drop_provisional(&mut self, mark: usize) {
        let Some(order) = self.provisional_order.get() else { return };
        if order.len() <= mark {
            return;
        }
        let order = self.provisional_order.get_mut();
        let map = self.provisional.get_mut();
        for k in order.drain(mark..) {
            map.remove(&k);
        }
    }

    /// Start a verdict that may be memoized: its height, the place the
    /// next pair it assumes takes, and the outer verdict's assumptions,
    /// set aside.
    fn enter_verdict(&mut self) -> (usize, Rests) {
        let height = self.assumed.get().map_or(0, |a| a.len());
        (height, mem::replace(&mut self.rests, Rests::NOTHING))
    }

    /// `rests`, a held verdict's, as the pairs answered since leave it.
    fn rests_now(&mut self, rests: Rests) -> Rests {
        match &mut self.assumed.0 {
            Some(assumed) => Rests { pairs: resting(assumed, rests.pairs), ..rests },
            None => rests,
        }
    }

    /// End it: what the verdict assumed, which the outer verdict takes
    /// too.
    fn leave_verdict(&mut self, outer: Rests) -> Rests {
        let rests = self.rests;
        self.rests = outer.join(rests);
        rests
    }

    /// The key a committed pair is held under: a node's identity, or an
    /// application's definition and its id ([`Self::app_id`]), since an
    /// expansion builds its applications anew and two of one definition
    /// over the same parameters are one type.
    fn pair_key(&mut self, t: &Type) -> Option<NormKey> {
        let Type::Ref(_) = t else { return probe_key(t) };
        let [def, id] = self.app_id(t)?;
        Some((mem::discriminant(t), def, id))
    }

    /// The definition of `t`, a resolved application, and the id of its
    /// parameters' keys ([`Type::param_key_with`]), an application under
    /// them keyed by its own id: a chain of applications nested `n` deep
    /// is keyed in `n` words, not `n` squared.
    fn app_id(&mut self, t: &Type) -> Option<[usize; 2]> {
        let Type::Ref(tr) = t else { return None };
        let def = tr.def_key()?;
        let node = Arc::as_ptr(tr).addr();
        if let Some(id) = self.app_nodes.get().and_then(|m| m.get(&node)) {
            return Some([def, *id]);
        }
        let key = tr.app_key_with(def, &mut |p| self.app_id(p));
        let next = self.apps.get().map_or(0, |m| m.len());
        let id = match self.apps.get_mut().entry(key) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let nodes = self.app_nodes.get();
                let by_id = |p: &Type| {
                    let node = p.shared_identity();
                    node.is_some_and(|a| nodes.is_some_and(|m| m.contains_key(&a)))
                };
                tr.key_cells(&by_id, self.app_cells.get_mut());
                *e.insert(next)
            }
        };
        self.probe_pins.get_mut().push(t.clone());
        self.app_nodes.get_mut().insert(node, id);
        Some([def, id])
    }

    /// Whether `t` is a name applied over a cell: its expansion holds the
    /// cell, so it is built anew, of new nodes, wherever the name is met.
    #[inline]
    fn open_app(&mut self, t: &Type) -> bool {
        matches!(t, Type::Ref(tr) if !tr.params.is_empty()) && !self.holds_no_cell(t)
    }

    /// The key a probe's side is held under: its node's identity; an
    /// [`Self::open_app`]'s [`Self::pair_key`], once its name is resolved;
    /// a var's wrapper, which the entry's pin holds. Of a var a verdict
    /// reads its name and what its link and cells hold, each changed only
    /// by a counted write ([`cell_writes`]).
    #[inline]
    fn probe_side(&mut self, t: &Type) -> Option<NormKey> {
        match t {
            Type::TVar(tv) => Some((mem::discriminant(t), tv.wrapper_addr(), 0)),
            t if self.open_app(t) => self.pair_key(t).or_else(|| probe_key(t)),
            t => probe_key(t),
        }
    }

    /// Whether the committing pair was proven to hold. One proven under
    /// assumptions still in progress passes them to the verdict reading it.
    fn commit_held(
        &mut self,
        flags: BitFlags<ContainsFlags>,
        t0: &Type,
        t1: &Type,
    ) -> bool {
        let (Some(k0), Some(k1)) = (self.pair_key(t0), self.pair_key(t1)) else {
            return false;
        };
        let k = (k0, k1, flags);
        if self.commit_pairs.get().is_some_and(|m| m.contains(&k)) {
            return true;
        }
        let Some(p) = self.provisional.get().and_then(|m| m.get(&k)).copied() else {
            return false;
        };
        self.rests = self.rests.join(self.rests_now(p.rests));
        true
    }

    /// Keep a committing pair proven to hold by a verdict at `height`
    /// assuming `rests`: for good where it assumed nothing further out; an
    /// [`Self::open_app`]'s, whose walk a sibling would repeat, while what
    /// it assumed stands.
    fn commit_put(
        &mut self,
        (flags, t0, t1): (BitFlags<ContainsFlags>, &Type, &Type),
        height: usize,
        rests: Rests,
    ) {
        let own = rests.own(height);
        if !own && !self.open_app(t0) && !self.open_app(t1) {
            return;
        }
        let (Some(k0), Some(k1)) = (self.pair_key(t0), self.pair_key(t1)) else {
            return;
        };
        let k = (k0, k1, flags);
        let new = match own {
            true => self.commit_pairs.get_mut().insert(k),
            false => {
                let p = Provisional { verdict: true, epoch: cell_writes(), rests };
                self.provisional_order.get_mut().push(k);
                self.provisional.get_mut().insert(k, p).is_none()
            }
        };
        if new {
            let pins = self.probe_pins.get_mut();
            pins.push(t0.clone());
            pins.push(t1.clone());
        }
    }

    /// Decide `key` by `f`, assuming it holds meanwhile: a pair met
    /// again inside its own proof is `true` (coinduction), and that
    /// assumption is what the verdict meeting it rests on. An answer that
    /// holds resting on a pair further out settles no verdict reached
    /// since: each reads through `assumed` what it rests on now, and is
    /// looked at once, when the outermost pair it rests on is answered.
    fn assuming(
        &mut self,
        key: RefPair,
        f: impl FnOnce(&mut Self) -> Result<bool>,
    ) -> Result<bool> {
        if let Some(&at) = self.hist.get(&key) {
            self.rests.pairs = self.rests.pairs.min(at);
            return Ok(true);
        }
        let assumed = self.assumed.get_mut();
        let at = assumed.len();
        assumed.push(at);
        self.hist.insert(key, at);
        let outer = mem::replace(&mut self.rests.pairs, usize::MAX);
        let mark = self.provisional_order.get().map_or(0, |o| o.len());
        let r = f(self);
        self.hist.remove(&key);
        let rest = beyond(&mut self.rests.pairs, at, outer);
        self.assumed.get_mut()[at] = rest;
        if rest == usize::MAX || !matches!(r, Ok(true)) {
            self.answer_assumption(mark, true, &r, |_| ());
        }
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
            let at = self.rests.taken(assumed);
            *at = (*at).min(i);
            return Ok(assumed);
        }
        let i = self.in_progress.len();
        self.in_progress.push(q);
        let outer = mem::replace(self.rests.taken(assumed), usize::MAX);
        let mark = self.provisional_order.get().map_or(0, |o| o.len());
        let r = f(self);
        self.in_progress.pop();
        let rest = beyond(self.rests.taken(assumed), i, outer);
        self.answer_assumption(mark, assumed, &r, |p| {
            answered(p.taken(assumed), i, rest)
        });
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
        let (Type::Ref(_), Some(id)) = (t, id) else {
            return t.lookup_ref_with(env, commit);
        };
        if self.closure(t, false) < Closure::NoCell {
            return t.lookup_ref_with(env, commit);
        }
        let e = match self.expansions.get().and_then(|m| m.get(&id)) {
            Some(e) => e.clone(),
            None => {
                let Some(e) = t.lookup_ref_with(env, commit)? else { return Ok(None) };
                self.expansions.get_mut().insert(id, e.clone());
                e
            }
        };
        Ok(Some(match commit && self.closure(&e, false) < Closure::Plain {
            true => e.reset_tvars_beside(&|p| self.known_plain(p)),
            false => e,
        }))
    }
}

/// The assumption at `depth` is answered: `at`, the shallowest one the
/// verdict in progress took (`usize::MAX` for none), gives up those from
/// `depth` on and joins `outer`'s. What the answer itself took.
fn beyond(at: &mut usize, depth: usize, outer: usize) -> usize {
    let rest = match *at {
        a if a >= depth => usize::MAX,
        a => a,
    };
    *at = rest.min(outer);
    rest
}

/// [`beyond`] for a provisional verdict: one that took the assumption at
/// `depth` (or one past it) takes what its answer took, `rest`, instead.
fn answered(at: &mut usize, depth: usize, rest: usize) {
    if (depth..usize::MAX).contains(at) {
        *at = rest
    }
}

/// The pair in progress that a verdict which took the pair at `at` rests
/// on now ([`ContainsHist::assumed`]), `usize::MAX` for none; each place
/// passed is left one step nearer its answer.
fn resting(assumed: &mut [usize], mut at: usize) -> usize {
    while at != usize::MAX {
        let next = assumed[at];
        if next == at {
            break;
        }
        if next != usize::MAX {
            assumed[at] = assumed[next]
        }
        at = next
    }
    at
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

/// How deep `t` nests constructors, each node `depths` holds read from it.
fn nesting(t: &Type, depths: &mut AddrMap<usize>) -> usize {
    ensure_sufficient(|| {
        let node = node_addr(t);
        if let Some(depth) = node.and_then(|n| depths.get(&n)) {
            return *depth;
        }
        #[cfg(test)]
        NESTING_STEPS.with(|n| n.set(n.get() + 1));
        let mut depth = 0;
        t.for_each_child(&mut |c| depth = depth.max(nesting(c, depths)));
        if let Some(n) = node {
            depths.insert(n, depth + 1);
        }
        depth + 1
    })
}

/// The flags a probe guiding a decision under `flags` runs with: it holds
/// a rigid cell rigid wherever the decision will, so a member it admits
/// does not fail on that cell.
fn guide(flags: BitFlags<ContainsFlags>) -> BitFlags<ContainsFlags> {
    let kept = flags
        & (ContainsFlags::Lower
            | ContainsFlags::IntoMember
            | ContainsFlags::LowerInArgs
            | ContainsFlags::Sealed
            | ContainsFlags::Conjunct);
    match flags.intersects(ContainsFlags::Commit | ContainsFlags::RigidCheck) {
        true => kept | ContainsFlags::RigidCheck,
        false => kept,
    }
}

// CR claude for claude: [bug/low] A well-typed call over a shorter chain of bounds is
// refused, as it is on base-r25 (the same refusal before 3157516bb): `let f = 'a3:
// ['a4, Array<'a4>], 'a4: ['a5, Array<'b>], 'a5: Array<i64>, 'b: Number |x: ('a3, 'a4,
// 'a5)| x; f(([1], [], [1]))` is refused with `type mismatch Array<Array<['_N: unbound
// within Number, ..]>> does not contain Array<i64>` at the first `[1]`, which `'a3 =
// Array<i64>` (its member `'a4`) types; `([], [], [1])` is accepted. The member of
// `'a3`'s bound that takes `Array<_>` is `Array<'a4>`, which decides 'a4 an array
// where the member `'a4` holds it. probe: ~/ptmp/cr29/scratch/r26/26e/p3.gx
// (bound-chain-array-member-01)

/// Whether the open cell `tv` takes what it must hold as a lower bound
/// rather than binding to it ([`ContainsFlags::Lower`] outside a
/// function's arguments and [`ContainsFlags::LowerInArgs`] inside them).
fn takes_lower(flags: BitFlags<ContainsFlags>, tv: &TVar) -> bool {
    !tv.is_rigid()
        && match flags.contains(ContainsFlags::Contra) {
            false => flags.contains(ContainsFlags::Lower),
            true => flags.contains(ContainsFlags::LowerInArgs),
        }
}

/// Whether `m`, a union's member, is a name or a bound cell that stands
/// for an open variable or for a union holding one as a member: its
/// members are the union's. `holds_var` answers whether a name's
/// parameter holds a variable.
fn names_members_by(
    env: &Env,
    m: &Type,
    holds_var: &mut dyn FnMut(&Type) -> bool,
) -> bool {
    fn named(m: &Type, holds_var: &mut dyn FnMut(&Type) -> bool) -> bool {
        match m {
            Type::Ref(tr) => tr.params.iter().any(|p| holds_var(p)),
            Type::TVar(tv) => tv.is_bound() && !is_unbound_tvar(m),
            _ => false,
        }
    }
    fn go(
        env: &Env,
        m: &Type,
        seen: &mut Expanded,
        holds_var: &mut dyn FnMut(&Type) -> bool,
    ) -> bool {
        named(m, holds_var)
            && ensure_sufficient(|| match &named_head(env, m, seen) {
                Some(Type::Set(ms)) => {
                    ms.iter().any(|m| is_unbound_tvar(m) || go(env, m, seen, holds_var))
                }
                Some(h) => is_unbound_tvar(h),
                None => false,
            })
    }
    named(m, holds_var) && go(env, m, &mut Expanded::default(), holds_var)
}

/// [`names_members_by`], each parameter walked ([`Type::tvar_free`]).
fn names_members(env: &Env, m: &Type) -> bool {
    names_members_by(env, m, &mut |p| !p.tvar_free())
}

/// Whether `m`, a union's member, is a name or a bound cell that stands
/// for a union or for primitives (a union of them is one member), or
/// [`names_members`].
fn names_union(env: &Env, m: &Type) -> bool {
    names_members(env, m)
        || (matches!(m, Type::Ref(_) | Type::TVar(_))
            && matches!(
                named_head(env, m, &mut Expanded::default()),
                Some(Type::Set(_) | Type::Primitive(_))
            ))
}

/// Two vars sharing one cell, or one id, are already unified.
fn one_variable(t0: &TVar, t1: &TVar) -> bool {
    t0.wrapper_addr() == t1.wrapper_addr()
        || t0.read().id == t1.read().id
        || t0.same_cell(t1)
}

/// Whether two members of a union are one member: the same variable, or
/// one type with every cell by identity ([`one_type`]), which either
/// decides alike.
fn one_member(m0: &Type, m1: &Type) -> bool {
    match (m0, m1) {
        (Type::TVar(t0), Type::TVar(t1)) if one_variable(t0, t1) => true,
        _ => one_type(m0, m1),
    }
}

/// Whether `t` reaches a cell nothing has decided: open, and not declared.
fn undecided(t: &Type) -> bool {
    find_open_cell(t, true, &mut |tv| !tv.is_rigid()).is_some()
}

/// Whether `t` reaches an undecided cell other than a function's throws:
/// without it, only what a function throws is open.
fn undecided_outside_throws(t: &Type) -> bool {
    fn walk(t: &Type, seen: &mut AddrSet) -> bool {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => walk(&b, seen),
                None => !tv.is_rigid(),
            },
            t => {
                if let Some(a) = node_addr(t)
                    && !seen.insert(a)
                {
                    return false;
                }
                match t {
                    Type::Fn(f) => {
                        f.args.iter().any(|a| walk(&a.typ, seen))
                            || f.vargs.iter().any(|v| walk(v, seen))
                            || walk(&f.rtype, seen)
                    }
                    t => {
                        let mut found = false;
                        t.for_each_child(&mut |c| found = found || walk(c, seen));
                        found
                    }
                }
            }
        })
    }
    walk(t, &mut LPooled::take())
}

/// Whether a commit can decide no cell `t` reaches: each open cell is held
/// rigid, and so is each one their conjuncts and lower bounds reach, and
/// none is one an earlier task left open outside a top-level input: a
/// function type quantifying such a cell takes it fresh for each walk
/// (`FnType::contains_int`), and a commit decides the copy.
fn decides_no_cell(t: &Type) -> bool {
    let fresh = !in_input();
    each_cell_through_bounds(t, true, &mut LPooled::take(), &mut |tv| {
        tv.is_rigid() && !(fresh && tv.earlier_task())
    })
}

/// How many parameters an `Array`, `List`, `Error`, tuple, struct, variant or
/// map holds.
fn ctor_arity(t: &Type) -> Option<usize> {
    match t {
        Type::Array(_) | Type::List(_) | Type::Error(_) => Some(1),
        Type::Map(_) => Some(2),
        Type::Tuple(ps) | Type::Variant(_, ps, _) => Some(ps.len()),
        Type::Struct(fs) => Some(fs.len()),
        _ => None,
    }
}

/// The `i`th parameter of a type with [`ctor_arity`] more than `i`.
fn ctor_param(t: &Type, i: usize) -> Option<&Type> {
    match t {
        Type::Array(p) | Type::List(p) | Type::Error(p) => (i == 0).then_some(&**p),
        Type::Map(m) => [&m.key, &m.value].get(i).copied(),
        Type::Tuple(ps) | Type::Variant(_, ps, _) => ps.get(i),
        Type::Struct(fs) => fs.get(i).map(|(_, t, _)| t),
        _ => None,
    }
}

/// A variable whose chain of bindings ends open.
fn is_unbound_tvar(t: &Type) -> bool {
    matches!(t, Type::TVar(_)) && t.with_deref(|d| d.is_none())
}

/// Whether `m` stands for the open cell `tv`: its var, or a chain of
/// bindings to vars ending there.
fn is_own_cell(m: &Type, tv: &TVar) -> bool {
    matches!(m, Type::TVar(m) if m.open_cell().is_some_and(|c| c.same_cell(tv)))
}

/// Whether `m` is a var whose chain ends at an open cell that is not
/// rigid.
fn is_flexible_var(m: &Type) -> bool {
    matches!(m, Type::TVar(m) if m.open_cell().is_some_and(|c| !c.is_rigid()))
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
/// aliases the cell to it, constraints and all. The walk stops at the
/// first part that is not exact, so a shared node or cell met again was.
fn exact(t: &Type) -> bool {
    fn walk(t: &Type, seen: &mut AddrSet) -> bool {
        ensure_sufficient(|| {
            if let Some(node) = node_addr(t)
                && !seen.insert(node)
            {
                return true;
            }
            match t {
                Type::Set(_) | Type::Any | Type::Ref(_) => false,
                Type::Primitive(p) => p.len() == 1,
                Type::TVar(tv) => match tv.binding() {
                    None => true,
                    Some(_) if !seen.insert(tv.cell_addr()) => true,
                    Some(b) => walk(&b, seen),
                },
                t => t
                    .try_for_each_child(&mut |c| match walk(c, seen) {
                        true => ControlFlow::Continue(()),
                        false => ControlFlow::Break(()),
                    })
                    .is_continue(),
            }
        })
    }
    walk(t, &mut LPooled::take())
}

/// Give the open cells inside `t`, about to be bound into the cell, their
/// part of each exact structural conjunct (`Array<i64> ⊇ Array<'y>` binds
/// `'y`), each trait conjunct (every member of a union implements the
/// trait) and each lower bound (`'p` above `Array<string>` bound to
/// `Array<'y>` puts `'y` above `string`): a probe admits them free, and
/// the bind passes down only the predicates.
fn commit_cell_bounds(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    Ok(commit_lower(tv, flags, env, hist, t)?
        && commit_conjuncts(tv, flags, env, hist, t)?)
}

/// The conjuncts' half of [`commit_cell_bounds`]: the open cells of `t`
/// take their part of each conjunct, and the open cells a conjunct holds
/// stand above their part of `t`.
fn commit_conjuncts(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    for c in tv.cell_constraints().iter() {
        if c.is_trait_ref(env) {
            for m in Type::dispatch_members(env, t)?.iter() {
                if let Type::TVar(m) = m
                    && !m.is_rigid()
                {
                    m.add_cell_constraint(c.clone())
                }
            }
            continue;
        }
        if c.is_predicate() {
            continue;
        }
        // CR claude for eric: [bug/low] An argument's undecided part that meets a union
        // of a declared bound is bound to that union at the pre-unification, before the
        // argument's own expression is checked: `let f = 'x: (['a, null], i64) |y: 'x| y;
        // let g = |u| f((u, u + 1)); g(1)` is refused, "cannot compute [null, 'a: unbound] +
        // i64", where the same union written in the parameter, `|y: (['a, null], i64)|`,
        // and `|u: i64|` print (1, 2). base-r23 does not return (bound-holds-own-cell-01).
        // The commit of the conjunct below binds u's cell to `[null, 'a]` (`BIND rhs` under
        // GRAPHIX_DBG_BIND); the union written in the parameter leaves u open until `u + 1`
        // is checked. probes: ~/ptmp/cr29/scratch/r23/23c/early_bind{,_ok_unbounded}.gx
        // (bound-union-binds-argument-early-01)
        // 2026-11-12 claude: Review of bound-holds-own-cell-01: two more witnesses, with no
        // arithmetic. Over `let f = 'x: Array<['a, null]> |y: 'x| y`, `let g = |u| f([u,
        // [u]]); g(1)` and `let u = never(); let r = f([u, (u, 1)]); u <- 1` are refused,
        // "type mismatch 'x: unbound within Array<[null, 'a: unbound above '_N: unbound
        // within [null, 'a: …]]> does not contain Array<[null, 'a: ['_N: .., Array<'_N:
        // ..>]]>", a message that names nothing to annotate, where the union written in the
        // parameter prints [1, [1]] and [1, (1, 1)]. GRAPHIX_DBG_BIND shows the same `BIND
        // rhs` of the array's element to `[null, 'a]` before its elements are checked.
        // base-r23 does not return. probes: ~/ptmp/cr29/scratch/r23/23c/rev/{p5,p4}.gx
        // (refused), {q5,q4}.gx (the written twins)
        // 2026-11-13 claude: Learned fixing never-under-union-bound-01 (lane 24c): a literal
        // array met by a parameter over a variable that a bound's holder put above a union
        // is refused the same way, with no wrapper: `let f = 'a1: ['a2, duration] |x0: 'a1,
        // x2: Array<'a2>| x0; let w0: [null, string] = null; f(w0, ["s"])` fails "type
        // mismatch Array<[null, string]> does not contain Array<string>" on base-r24 and on
        // the lane (s2.gx); the array annotated `Array<[null, string]>` (s4.gx) and the
        // union written in the parameter (s5.gx) print null. Since
        // bound-chain-lower-settle-01 a chain's settle reaches it where base-r24 ran: `'a0:
        // ['a1, bool], 'a1: ['a2, duration], 'a2: ['a3, null], 'a3: [i64, string] |x0: 'a0,
        // x1: 'a1, x2: Array<'a2>|` given `(w0, "s", ["s"])` printed null on base-r24 and is
        // refused on the lane (1 of 7200 generated programs, gen/w61-p0310.gx). probes:
        // ~/ptmp/cr29/scratch/r24/24c/nuub/{s1,s2,s3,s4,s5}.gx, gen/w61-p0310.gx
        // 2026-11-14 claude: Lane 24c fixer: two of the mechanisms recorded here are fixed,
        // and the one in the CR's own text is not. Fixed: (1) the 11-12 note's p5 and p4,
        // and the same arrays bound to a `let` first (`let v = [u, [u]]; f(v)`, refused on
        // base-r24 with no pre-unification in play): contains::commit_parts narrowed a cell
        // of the value by a part holding an open cell (`['a, null]`), the bound's own
        // containment then put 'a above that cell, and the same cell under a constructor
        // (`[u]`, `(u, 1)`) could not go below 'a, the two bounds reading as a cycle; a part
        // holding an open cell narrows no cell now. (2) the 11-13 note's s1, s2 and
        // gen/w61-p0310: the open copy of a formal kept the lower bounds of the variables it
        // opens, a literal's element that merged with the copy took them, and the element's
        // own check refused a "s" below `[null, string]`; Widening::opening drops them
        // (TVar::drop_lower) and typecheck_arg hands TVar::rejoin each copy's bounds as it
        // was made. Pins:
        // lang::types::undecided_{value,binding,array}_*_under_a_union_bound* and
        // literal_{narrower_than,outside}_what_* (ten),
        // contains::tests::a_bounds_part_holding_an_open_cell_narrows_no_cell. NOT fixed:
        // the CR's own witness, `f((u, u + 1))` at `'x: (['a, null], i64)`: the
        // pre-unification binds the copy of 'x to the argument, and the bound's containment
        // (commit_conjuncts, its last step) binds u to `[null, 'a]`. Two fixes were built
        // and dropped (tried.txt has both): (a) Type::pre_unify_seen pre-unifies the
        // argument with the bound's type as if written and leaves the variable open; (b)
        // under ContainsHist::early a cell that a union with an undecided member meets is
        // narrowed by it, not bound. Each makes the witness print (1, 2), and each breaks
        // programs pinned as valid, because that same bind is what gives a literal's element
        // the bound's union:
        // lang::types::declared_variables_passed_on_through_a_chain_of_union_bounds and
        // _beside_callbacks (`h(`A([`B(null)]), [`A("s")], null)`: the first literal typed
        // on its own decides 'b2 as Array<`B(null)>, and the second, neither wider nor
        // narrower, is the mismatch),
        // undecided_element_takes_the_union_of_a_bound_over_another_variable (`f([1])[0]$`
        // is `[i64, null]` today and i64 without the bind),
        // two_undecided_elements_of_a_bound_widen_its_variable; six pins with (b), nine with
        // (a), which also lets 3 of 1000 generated programs through the check that the build
        // refuses (base-r24 does with the argument in a `let`: noted on
        // lower-bound-refuses-narrower-01). So a literal under a bound takes its expected
        // type by this bind, and an undecided value used twice in one argument pays for it;
        // which of the two stands, or whether a variable only bounds hold widens
        // (bound-var-no-widen-01) so that both can, is a rule this CR does not decide.
        // Recommendation: (b), once bound-var-no-widen-01 is decided for widening; until
        // then the refusal could name the value to annotate (`|u: i64|` runs). The written
        // form has the same early bind for a union of primitives
        // (written-primitive-union-binds-early-01). probes ~/ptmp/cr29/scratch/r24/24c/buba/
        // (README, tried.txt)
        // 2026-11-14 claude: Lane 24c reviewer: the two fixed mechanisms hold (the ten pins
        // and the unit test fail with either rule undone; 1400 generated calls beside their
        // written twins, the lane before the commit against it: 5 newly accepted, each valid
        // and the same fused, with --no-fusion, under GRAPHIX_PAR=force and
        // GRAPHIX_NO_SUBST=1, none newly refused; census 2967 files, no verdict or message
        // moved). The dropped fix (b) was built again: it breaks 7 pins of lang::types, the
        // six named above and
        // partial_wrapper_bound_reaches_the_callees_instance_under_a_recursive_typedef.
        // Mechanism (2) is fixed only where the call opens a copy, so where the variable
        // above the bound's lower bound is widenable. Where it is not, a literal narrower
        // than the lower bound is still refused, and the union written in the parameter
        // runs: a function type holds the variable (`let f = 'a1: ['a2, duration] |x0: 'a1,
        // x2: Array<'a2>, k: fn(a: 'a2) -> i64| x2; let w0: [null, string] = null; f(w0,
        // ["s"], |a: [null, string]| 0)`, "Array<[null, string]> does not contain
        // Array<string>"), an undecided argument pinned it first (`|x0: 'a1, x2: Array<'a2>,
        // x3: Array<'a2>|` given `(w0, [u], ["s"])` in a wrapper), or the literal goes to a
        // second bounded variable (`'a3: Array<'a2> |x0: 'a1, x2: 'a3, ..|` given `["s"]`).
        // There the pre-unification is with the formal as it stands, the element takes the
        // lower bound, and a copy would not help alone: the meeting with the formal refuses
        // a type the lower bound holds (lower-bound-refuses-narrower-01; `x2: fn() ->
        // Array<'a2>` given `|| ["s"]` is refused there with no pre-unification). base-r24
        // alike. Pins added:
        // lang::types::undecided_value_under_a_union_bound_takes_what_a_later_callback_decides{,_refused},
        // literal_narrower_than_what_a_bounds_holder_put_{its_variable_above_nested,a_one_instance_variable_above},
        // undecided_value_after_a_literal_narrower_than_what_*. probes
        // ~/ptmp/cr29/scratch/r24/24c/bubarev/{j1,k12,k18,j18}.gx (refused),
        // {j1t,k12t,k18t,j18t}.gx (the written twins, which run), gen2.py
        // 2026-11-13 claude: Deciding union-bound-open-member-order-01 (closed by D36): its
        // literal forms are this CR's. Over `let f = 'a: [Array<i64>, 'b] |v: 'a, y: 'b| v`,
        // `f(["x"], ["s"])`, `f([z], ["s"])` over `let z: string = never()` and over `let z:
        // [i64, string] = never()` are refused, "Array<i64> does not contain Array<string>"
        // ("Array<[i64, string]>" for the last): the pre-unification gives the literal's
        // element to the structural member before "x" or z is checked. The same arrays bound
        // to a `let` first (`let x = ["x"]; f(x, ["s"])`) go to 'b and print, and so does
        // the union written in the parameter, `|v: [Array<i64>, 'b], y: 'b|`, with the
        // literals. A lower bound alike: `let x: Array<[i64, string]> = ["z"]; f(x, ["s"])`
        // is refused, "Array<[i64, string]> does not contain Array<string>", a mismatch that
        // is false as written: GRAPHIX_DBG_BIND shows the copy of 'b bound to the literal's
        // Array<'e> and `LOWER 'e above [i64, string]` before "s" is checked; `let w =
        // ["s"]; f(x, w)` and the parameter form print ["z"]. base-r24 alike. probes:
        // ~/ptmp/cr29/scratch/r24/24d/ubo/{r9e,r9c,r9b,d1,p1,e3,e13,e4,e14}.gx
        // 2026-11-13 claude: Fixing method-union-member-order-01: a trait call's literal
        // array is this CR's too, with the variable member decided first. Over 'trait T {
        // val m: fn(self, x: [self, Array<string>]) -> i64 }; impl T for Array<i64> { let m
        // = |s, x| 0 }', 'T::m([1], [2])' is refused, "Array<string> does not contain
        // Array<i64>", after the receiver made self Array<i64>; 'let v = [2]; T::m([1], v)'
        // prints 0, and so do the literal under the union written out ('x: [Array<i64>,
        // Array<string>]') and the plain function '|s: Array<i64>, x: [Array<i64>,
        // Array<string>]|'. Under 'impl<'t> T for Array<'t>' alike. base-r24 alike. probes:
        // ~/ptmp/cr29/scratch/r24/24d/mum/{k3,k12,g1}.gx (refused), {k10,k1,k9,k8}.gx (run)
        // 2026-11-14 claude: The witness left is item D39 in ~/ptmp/cr-discuss.md (Open).
        let held = match exact(c) {
            true => c.contains_int(flags | ContainsFlags::Conjunct, env, hist, t)?,
            false => commit_parts(
                tv,
                flags | ContainsFlags::Conjunct,
                env,
                hist,
                &mut Lazy::new(),
                c,
                t,
            )?,
        };
        if !held {
            return Ok(false);
        }

        // the conjunct's own open cells must hold what the cell takes:
        // `'d: [null, 'c]` bound to i64 puts i64 below 'c, so 'c bound
        // later to a type without it is refused
        if !hist.closed_now(c)
            && !exact(c)
            && c.has_unbound()
            && !c.contains_int(flags | ContainsFlags::Lower, env, hist, t)?
        {
            return Ok(false);
        }
    }
    Ok(true)
}

impl TVar {
    /// Commit the cell's conjuncts over `t` as the containment that binds
    /// the cell to `t` does ([`commit_conjuncts`]), for a bind made outside
    /// one: a settle at the cell's lower bounds, a call's widening. So no
    /// bind leaves a cell a conjunct holds free of what the cell took.
    /// False where a conjunct does not hold `t`.
    pub fn commit_conjuncts(&self, env: &Env, t: &Type) -> Result<bool> {
        self.commit_conjuncts_in(env, &mut ContainsHist::new(), t)
    }

    /// [`Self::commit_conjuncts`] in `hist`, a walk that may have decided
    /// other relations before ([`ContainsHist::next_relation`]).
    pub(super) fn commit_conjuncts_in(
        &self,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<bool> {
        let flags = ContainsFlags::Commit.into();
        hist.next_relation();
        Ok(conjuncts_admit(self, flags, env, hist, t)?
            && commit_conjuncts(self, flags, env, hist, t)?)
    }
}

impl TVar {
    /// Whether one of the open cell's union bounds has two members that
    /// take `t`, none holding the others ([`member_with_parts`]): the
    /// cell binding to `t` now would leave the bound not asking which.
    pub fn bound_takers_ambiguous(&self, env: &Env, t: &Type) -> Result<bool> {
        let flags = BitFlags::empty();
        let mut hist = ContainsHist::early();
        for c in self.cell_constraints() {
            let members = Type::dispatch_members(env, &c)?;
            let mut admit: SmallVec<[&Type; 2]> = SmallVec::new();
            for m in members.iter().filter(|m| m.takes_constructor_of(t)) {
                if m.contains_int(guide(flags), env, &mut hist, t)? {
                    admit.push(m)
                }
            }
            if admit.len() >= 2 && widest(&admit, flags, env, &mut hist)?.is_none() {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

/// Pass the parts of a conjunct `c` that is not exact down to the open
/// cells of `t` they meet: a cell in a covariant position is narrowed by
/// its part (`Array<[i64, null]> ⊇ Array<'y>` gives `'y: [i64, null]`, `Int
/// ⊇ [i64, 'k]` gives `'k: Int`), an exact part is committed as an exact
/// conjunct is, and a function's argument takes the conjunct's argument,
/// the least it must accept. A rigid cell takes no part: the probe held
/// it against its part, and the conjunct's open cells take it as what
/// they must hold ([`commit_conjuncts`]). Neither does a cell whose part
/// holds an open cell: the conjunct's own containment relates the two
/// there, and a cell within a part that stands above it would reach
/// itself. A writable reference's referent
/// is its part's exactly, and so is an abstract type's parameter. Of a union's
/// members, each name and bound variable among them followed to what it
/// stands for, one has the parts ([`member_with_parts`]). A decided
/// variable of the conjunct is met as its binding, and a name in `t`
/// applied over an open cell as what it stands for, each pair of a part
/// and such a name once (`expanded`). A probe accepted `t` already; what
/// the walk can't line up it leaves.
fn commit_parts(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    expanded: &mut Lazy<AHashSet<RefPair>>,
    c: &Type,
    t: &Type,
) -> Result<bool> {
    ensure_sufficient(|| {
        if exact(c) {
            return c.contains_int(flags, env, hist, t);
        }
        let all = |pairs: &mut dyn Iterator<Item = (&Type, &Type)>,
                   hist: &mut ContainsHist,
                   expanded: &mut Lazy<AHashSet<RefPair>>|
         -> Result<bool> {
            for (c, t) in pairs {
                if !commit_parts(tv, flags, env, hist, expanded, c, t)? {
                    return Ok(false);
                }
            }
            Ok(true)
        };
        match (c, t) {
            (Type::Any, _) => Ok(true),
            (_, Type::TVar(t)) => match t.open_cell() {
                Some(cell)
                    if cell.cell_addr() != tv.cell_addr()
                        && !cell.is_rigid()
                        && !c.has_unbound() =>
                {
                    cell.narrow_cell(env, c.clone())?;
                    Ok(true)
                }
                Some(_) => Ok(true),
                None => match t.binding() {
                    Some(b) => commit_parts(tv, flags, env, hist, expanded, c, &b),
                    None => Ok(true),
                },
            },
            (Type::TVar(c), _) => match c.binding() {
                Some(c) => commit_parts(tv, flags, env, hist, expanded, &c, t),
                None => Ok(true),
            },
            (_, Type::Ref(_)) if !hist.closed_now(t) => {
                let pair = (hist.ref_id(c, env), hist.ref_id(t, env));
                match t.lookup_ref(env) {
                    Ok(t) if expanded.get_mut().insert(pair) => {
                        commit_parts(tv, flags, env, hist, expanded, c, &t)
                    }
                    Ok(_) | Err(_) => Ok(true),
                }
            }
            (Type::Ref(_), _) => match c.lookup_ref(env) {
                Ok(c) => commit_parts(tv, flags, env, hist, expanded, &c, t),
                Err(_) => Ok(true),
            },
            (_, Type::Set(ts)) => all(&mut ts.iter().map(|t| (c, t)), hist, expanded),
            (Type::Set(cs), t) if structured(t) => {
                let named: SmallVec<[Type; 8]>;
                let cs: &[Type] = match cs.iter().all(structured_or_prim) {
                    true => cs,
                    false => match Type::dispatch_members(env, c) {
                        Ok(ms) if ms.iter().all(structured_or_prim) => {
                            named = ms;
                            &named
                        }
                        Ok(_) | Err(_) => return Ok(true),
                    },
                };
                match member_with_parts(c, cs, flags, env, hist, t)? {
                    Some(c) => commit_parts(tv, flags, env, hist, expanded, c, t),
                    None => Ok(true),
                }
            }
            (Type::Array(c), Type::Array(t))
            | (Type::List(c), Type::List(t))
            | (Type::Error(c), Type::Error(t))
            | (Type::ByRef(Mutability::Shared, c), Type::ByRef(_, t)) => {
                commit_parts(tv, flags, env, hist, expanded, c, t)
            }
            (Type::ByRef(Mutability::Mut, c), Type::ByRef(Mutability::Mut, t)) => {
                c.equal_int(flags, env, hist, t)
            }
            (Type::Abstract { .. }, Type::Abstract { .. }) => {
                c.contains_int(flags, env, hist, t)
            }
            (Type::Map(c), Type::Map(t)) => all(
                &mut [(&c.key, &t.key), (&c.value, &t.value)].into_iter(),
                hist,
                expanded,
            ),
            (Type::Tuple(cs), Type::Tuple(ts)) if cs.len() == ts.len() => {
                all(&mut cs.iter().zip(ts.iter()), hist, expanded)
            }
            (Type::Variant(x, cs, _), Type::Variant(y, ts, _))
                if x == y && cs.len() == ts.len() =>
            {
                all(&mut cs.iter().zip(ts.iter()), hist, expanded)
            }
            (Type::Struct(cs), Type::Struct(ts))
                if cs.len() == ts.len()
                    && cs.iter().zip(ts.iter()).all(|(c, t)| c.0 == t.0) =>
            {
                all(
                    &mut cs.iter().zip(ts.iter()).map(|(c, t)| (&c.1, &t.1)),
                    hist,
                    expanded,
                )
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
                        || !ta.typ.contains_int(
                            flags & !ContainsFlags::Conjunct,
                            env,
                            hist,
                            &ca.typ,
                        )?
                    {
                        return Ok(false);
                    }
                }
                commit_parts(tv, flags, env, hist, expanded, &cf.rtype, &tf.rtype)
            }
            _ => Ok(true),
        }
    })
}

/// The member of `cs`, the members of the union conjunct `bound`, that
/// has the parts of a value of type `t` ([`commit_parts`]): the one that
/// takes its constructor
/// ([`Type::takes_constructor_of`]); of several, where `t` is a function
/// or holds a cell to decide, the one a probe admits `t` into, or the one
/// of those that holds the others. `None` where no member is told apart
/// and none need be: `t` has no cell to decide, some member holds it
/// whatever decides its cells, or the walk is not the check's own
/// ([`ContainsHist::decides`]). Else which member holds `t` follows what
/// decides its cells: [`UndecidedTaker`].
fn member_with_parts<'a>(
    bound: &Type,
    cs: &'a [Type],
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<Option<&'a Type>> {
    let mut takers = cs.iter().filter(|c| c.takes_constructor_of(t));
    let Some(c0) = takers.next() else { return Ok(None) };
    let Some(c1) = takers.next() else { return Ok(Some(c0)) };
    let open = undecided(t);
    if !open && !matches!(t, Type::Fn(_)) {
        return Ok(None);
    }
    let mut admit: SmallVec<[&Type; 2]> = SmallVec::new();
    for c in [c0, c1].into_iter().chain(takers) {
        if c.contains_int(guide(flags), env, hist, t)? {
            admit.push(c)
        }
    }
    if admit.len() < 2 {
        return Ok(admit.first().copied());
    }
    if let Some(c) = widest(&admit, flags, env, hist)? {
        return Ok(Some(c));
    }
    if !open || !hist.decides() {
        return Ok(None);
    }
    for c in admit.iter().copied() {
        if holds_as_it_stands(c, flags, env, hist, t)? {
            return Ok(None);
        }
    }
    Err(UndecidedTaker::new(TakerUnion::Bound, bound, t, &admit))
}

/// The member of `ms` that holds each of the others as it stands
/// ([`holds_as_it_stands`]): what any of them takes it takes, whatever
/// decides their cells.
fn widest<'a>(
    ms: &[&'a Type],
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
) -> Result<Option<&'a Type>> {
    'widest: for m in ms.iter().copied() {
        for o in ms.iter().copied() {
            if !std::ptr::eq(m, o) && !holds_as_it_stands(m, flags, env, hist, o)? {
                continue 'widest;
            }
        }
        return Ok(Some(m));
    }
    Ok(None)
}

/// Whether `m` taking `t`, on copies, leaves each other member of `ms`
/// holding `t` as it stands: `m` decides every cell another of them would.
fn decides_each(
    m: &Type,
    ms: &[&Type],
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &ContainsHist,
    t: &Type,
) -> Result<bool> {
    for o in ms.iter().copied().filter(|o| !std::ptr::eq(*o, m)) {
        let decided = on_copies(Some((hist, env)), [m, o, t], |[m, o, t], trial| {
            Ok(m.contains_int(flags, env, trial, &t)?
                && holds_as_it_stands(&o, flags, env, trial, &t)?)
        })?;
        if !decided {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Whether `m` holds `t` with every open cell of both held as it stands,
/// so it holds `t` whatever decides them.
fn holds_as_it_stands(
    m: &Type,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    if hist.closed(m) && hist.closed(t) {
        return m.contains_int(guide(flags), env, hist, t);
    }
    let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
    hold_rigid(m, &mut gates);
    hold_rigid(t, &mut gates);
    let outer = hist.gate();
    let holds = m.contains_int(guide(flags), env, hist, t);
    hist.ungate(outer);
    holds
}

/// Whether `tier[at]` takes `t` on copies, and, when `after` is asked,
/// which members of the tier hold `t` as it stands once it has: bit `i`
/// for `tier[i]`, none where a probe failed. Each is what [`decide_alike`]
/// asks of the pair, taken from the same walk.
fn takes_leaving(
    tier: &[&Type],
    at: usize,
    after: bool,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &ContainsHist,
    t: &Type,
) -> Result<(bool, Option<u64>)> {
    let others: SmallVec<[(usize, &Type); 4]> =
        tier.iter().copied().enumerate().filter(|(i, _)| after && *i != at).collect();
    let mut ts: SmallVec<[&Type; 6]> = smallvec::smallvec![tier[at], t];
    ts.extend(others.iter().map(|(_, o)| *o));
    on_copies_of(Some((hist, env)), &ts, |copies, trial| {
        let [m, t, rest @ ..] = &copies[..] else { unreachable!() };
        if !m.contains_int(flags, env, trial, t)? {
            return Ok((false, None));
        }
        let mut bits = after.then_some(0u64);
        for ((i, _), o) in others.iter().zip(rest) {
            bits = match (bits, holds_as_it_stands(o, flags, env, trial, t)) {
                (Some(bits), Ok(true)) => Some(bits | 1 << i),
                (bits, Ok(true) | Ok(false)) => bits,
                (_, Err(_)) => None,
            }
        }
        Ok((true, bits))
    })
}

/// Whether `m0` and `m1`, decided members of a union that each hold `t`,
/// decide its cells alike: each holds `t` as the other decides it, so
/// which of them takes it decides nothing.
fn decide_alike(
    m0: &Type,
    m1: &Type,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &ContainsHist,
    t: &Type,
) -> Result<bool> {
    let held = |by: &Type, other: &Type| {
        on_copies(Some((hist, env)), [by, other, t], |[by, other, t], trial| {
            Ok(by.contains_int(flags, env, trial, &t)?
                && holds_as_it_stands(&other, flags, env, trial, &t)?)
        })
    };
    Ok(held(m0, m1)? && held(m1, m0)?)
}

/// Whether `t` is built by a constructor with parts.
fn structured(t: &Type) -> bool {
    match t {
        Type::Variant(_, ps, _) => !ps.is_empty(),
        Type::Abstract { params, .. } => !params.is_empty(),
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
        t => structured(t) || matches!(t, Type::Variant(..) | Type::Abstract { .. }),
    }
}

/// True iff binding `t` into the cell would satisfy every conjunct of
/// the cell's constraints and contain each of its lower bounds. A pure
/// probe.
fn cell_constraints_ok(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    Ok(conjuncts_admit(tv, flags, env, hist, t)?
        && holds_lower(tv, below(flags), env, hist, t)?)
}

/// Whether `t` contains each lower bound of the cell under `flags`.
fn holds_lower(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    for l in tv.cell_lower().iter() {
        if !t.contains_int(flags, env, hist, l)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Whether `t` contains the union of `ls` under `flags`: a union `t`
/// takes a bound that is an open cell after the others, from the members
/// they left, so `[i64, Array<i64>]` above `'x` and `Array<'x>` makes
/// `'x` `i64`.
fn holds_union(
    ls: &[Type],
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    match ls {
        [] => Ok(true),
        [l] => t.contains_int(flags, env, hist, l),
        ls => t.contains_int(
            flags,
            env,
            hist,
            &Type::Set(Arc::from_iter(ls.iter().cloned())),
        ),
    }
}

// CR claude for claude: [perf/low] A call passing an array, or a tuple, for every
// link of a chain of bounds checks in time quadratic in the chain's length, where the
// twin passing integers is linear (deep_nesting chainboundsvalued). `python3 gen2.py
// arrnotrait N` / `gen3.py tupnotrait N` (--no-cache --check, debug, empty 0.75 G): arrays
// 8.1 / 27.9 G at 500 / 1000 links, tuples (i64, i64) 5.0 / 17.0 G at 1000 / 2000, a
// trait on every link `impl<'e> Tr for Array<'e>` 16.6 G at 500. The profile is flat
// (contains_dispatch, TVar::read, cell_lower, probe_get, ContainsHist::closure): each
// link's commit probes and commits the whole chain below it once. A fix makes a link's
// commit read the links below by a verdict that stands while nothing they reach is
// written, as the integers' probes do (probe_standing). probes:
// ~/ptmp/cr29/scratch/r24/24h/impls-read/, ~/ptmp/cr29/scratch/r25/25e/ (chainbounds-quadratic-01)

/// Commit `t ⊇ l` for each lower bound `l` of a cell bound to `t`, on
/// copies first, so bounds a probe admits apart but no commit holds
/// together (`(i64, string)` above `('x, 'x)`) refuse the bind and bind
/// no cell. An open cell of a bound below a union is bound to the union
/// where that holds every bound, else taken by the union's open member
/// ([`ContainsFlags::IntoMember`]): `['h, 'r]` above `'x` and
/// `Array<'x>`.
fn commit_lower(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    let lower = tv.cell_lower();
    let mut flags = below(flags) | ContainsFlags::Commit;
    if lower.is_empty() || flags.contains(ContainsFlags::IntoMember) {
        return holds_union(&lower, flags, env, hist, t);
    }
    if hist.trial || hist.proved {
        if !holds_union(&lower, guide(flags), env, hist, t)? {
            flags |= ContainsFlags::IntoMember;
            if !holds_union(&lower, guide(flags), env, hist, t)? {
                return Ok(false);
            }
        }
        return holds_union(&lower, flags, env, hist, t);
    }
    let ls = Type::Tuple(Arc::from_iter(lower.iter().cloned()));
    let holds = |flags| {
        on_copies(Some((&*hist, env)), [t, &ls], |[t, ls], trial| match &ls {
            Type::Tuple(ls) => holds_union(ls, flags, env, trial, &t),
            _ => Ok(false),
        })
    };
    if !holds(flags)? {
        flags |= ContainsFlags::IntoMember;
        if !holds(flags)? {
            return Ok(false);
        }
    }
    let proved = mem::replace(&mut hist.proved, true);
    let held = holds_union(&lower, flags, env, hist, t);
    hist.proved = proved;
    held
}

/// Whether the open cell `tv`, above lower bounds that hold `t` and that
/// `t` is strictly narrower than, holds `t` as it stands (`t ⊆ L ⊆ tv`):
/// binding it to `t` would not hold its bounds. The bound that holds
/// commits, so `t`'s open cells take their part of it. `None` where no
/// bound holds `t`, or `t` holds them all and the cell binds to it.
fn held_by_lower(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<Option<bool>> {
    let lower = tv.cell_lower();
    if lower.is_empty()
        || !flags.contains(ContainsFlags::Conjunct)
        || flags.contains(ContainsFlags::Contra)
    {
        return Ok(None);
    }
    for l in lower.iter() {
        if l.contains_int(guide(flags), env, hist, t)?
            && !holds_union(&lower, guide(flags), env, hist, t)?
        {
            return match flags.contains(ContainsFlags::Commit) {
                true => l.contains_int(flags, env, hist, t).map(Some),
                false => Ok(Some(true)),
            };
        }
    }
    Ok(None)
}

/// The flags `t ⊇ l` runs with for a lower bound `l` of a cell bound to
/// `t`: the open cells of `t` take their parts of `l` as lower bounds in
/// turn, so a cell above two bounds holds both whatever their order.
fn below(flags: BitFlags<ContainsFlags>) -> BitFlags<ContainsFlags> {
    guide(flags) | ContainsFlags::Lower
}

/// True iff every conjunct of the cell's constraints contains `t`; one
/// that holds the cell itself ([`holds_cell`]) contains whatever the cell
/// takes. A pure probe.
fn conjuncts_admit(
    tv: &TVar,
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    let probe = (guide(flags) & !ContainsFlags::Lower) | ContainsFlags::Conjunct;
    for c in tv.cell_constraints().iter() {
        if !holds_cell(env, c, tv.cell_addr()) && !c.contains_int(probe, env, hist, t)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Whether every conjunct of the open cell's constraints contains `t`
/// ([`conjuncts_admit`]).
pub(super) fn cell_admits(tv: &TVar, env: &Env, t: &Type) -> Result<bool> {
    conjuncts_admit(tv, BitFlags::empty(), env, &mut ContainsHist::new(), t)
}

/// Whether the rigid open cell `tv` (as `tt`) holds `t`: a rigid cell
/// holds only itself, ⊥, what one of its lower bounds holds (`t ⊆ L ⊆
/// tv`), since whatever it stands for holds each, and a union of such.
/// The bound that holds commits, and so does each member of a union
/// held whole.
fn rigid_holds_by_lower(
    (tt, tv): (&Type, &TVar),
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    t: &Type,
) -> Result<bool> {
    let commit = flags.contains(ContainsFlags::Commit);
    for l in tv.cell_lower().iter() {
        if l.contains_int(guide(flags), env, hist, t)? {
            return match commit {
                true => l.contains_int(flags, env, hist, t),
                false => Ok(true),
            };
        }
    }
    match t {
        Type::Set(s) if tt.holds_each(guide(flags), env, hist, s)? => match commit {
            true => tt.holds_each(flags, env, hist, s),
            false => Ok(true),
        },
        _ => Ok(false),
    }
}

/// Whether the open cell `to` (as `tt`) admits the lower bounds of the
/// open cell `from` its unification would merge in: its conjuncts
/// contain each, and a rigid cell, which holds only itself, holds each.
fn admits_lower(
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    (tt, to): (&Type, &TVar),
    from: &TVar,
) -> Result<bool> {
    let probe = (guide(flags) & !ContainsFlags::Lower) | ContainsFlags::RigidCheck;
    for l in from.cell_lower().iter() {
        let held = match to.is_rigid() {
            true => tt.contains_int(probe, env, hist, l)?,
            false => conjuncts_admit(to, flags, env, hist, l)?,
        };
        if !held {
            return Ok(false);
        }
    }
    Ok(true)
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
        c.contains_with_flags(
            ContainsFlags::RigidCheck | ContainsFlags::Sealed,
            env,
            self,
        )
    }
}

/// Hold every open cell of `t` rigid while `gates` lives.
pub(super) fn hold_rigid(t: &Type, gates: &mut Vec<RigidGate>) {
    find_open_cell(t, true, &mut |tv| {
        gates.push(tv.open_rigid());
        false
    });
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

/// A trial: `f` over copies of `ts` that share one cell map, each rigid
/// cell's copy held rigid, and a walk state of its own, in which the
/// pairs the walk `within` it is made in is expanding are in progress
/// ([`ContainsHist::seed`]); what `f` binds, it binds in the copies alone.
pub(super) fn on_copies<const N: usize, R>(
    within: Option<(&ContainsHist, &Env)>,
    ts: [&Type; N],
    f: impl FnOnce([Type; N], &mut ContainsHist) -> Result<R>,
) -> Result<R> {
    on_copies_of(within, &ts, |copies, trial| {
        let mut copies = copies.drain(..);
        f([(); N].map(|()| copies.next().expect("one copy of each type")), trial)
    })
}

/// [`on_copies`] over any number of types.
pub(super) fn on_copies_of<R>(
    within: Option<(&ContainsHist, &Env)>,
    ts: &[&Type],
    f: impl FnOnce(&mut SmallVec<[Type; 4]>, &mut ContainsHist) -> Result<R>,
) -> Result<R> {
    let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    let mut copies: SmallVec<[Type; 4]> = ts
        .iter()
        .map(|t| {
            t.reset_tvars_int(&mut known, Fresh::Copy).unwrap_or_else(|| (*t).clone())
        })
        .collect();
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
    let mut trial = ContainsHist::trial();
    if let Some((walk, env)) = within {
        walk.seed(&mut trial, &mut known, env, Inherit::Assumed)
    }
    f(&mut copies, &mut trial)
}

/// A cell, a type that may reach it and the edges the walk takes.
type Reach<'a> = (&'a TVar, &'a Type, Toward);

/// The cells on a cycle through cells and unions alone, each type's bare
/// paths to its cell and the cell, are one type: the open ones merge, each
/// first dropping its bounds that are cells of the cycle, and each bound
/// one is the merged cell, which the names of the dropped bounds then
/// hold to their parameters' bounds. Two declared vars of the def under
/// check are not one type.
fn merge_bare_cycle(
    flags: BitFlags<ContainsFlags>,
    env: &Env,
    hist: &mut ContainsHist,
    paths: &[Reach],
) -> Result<bool> {
    let commit = flags.contains(ContainsFlags::Commit);
    let mut on: LPooled<Vec<TVar>> = LPooled::take();
    for (to, from, toward) in paths {
        let mut path = bare_path(to.cell_addr(), from, *toward);
        if !path.is_empty() {
            on.extend(path.drain(..));
            on.push((*to).clone());
        }
    }
    let mut cells: LPooled<AddrSet> = LPooled::take();
    let mut open: LPooled<Vec<TVar>> = LPooled::take();
    let mut bound: LPooled<Vec<TVar>> = LPooled::take();
    for tv in on.iter() {
        cells.insert(tv.cell_addr());
        match tv.open_cell() {
            Some(o) if !open.iter().any(|m| m.same_cell(&o)) => {
                cells.insert(o.cell_addr());
                open.push(o)
            }
            Some(_) => (),
            None if matches!(tv.binding(), Some(Type::TVar(_)))
                || bound.iter().any(|b| b.same_cell(tv)) => {}
            None => bound.push(tv.clone()),
        }
    }
    if graphix_dbg_bind() {
        eprintln!("BARE-CYCLE {} open, {} bound", open.len(), bound.len());
    }
    let mut rigid = open.iter().filter(|o| o.is_rigid());
    let survivor = match (rigid.next(), rigid.next()) {
        (Some(_), Some(_)) => return Ok(false),
        (Some(r), None) => r.clone(),
        (None, _) => match open.first() {
            Some(o) => o.clone(),
            None => return Ok(true),
        },
    };
    let mut dropped: LPooled<Vec<Type>> = LPooled::take();
    if commit {
        for o in open.iter() {
            o.drop_bounds_on(&cells, &|b| bound_members(env, b, false), &mut dropped)?;
        }
    }
    for o in open.iter() {
        if o.same_cell(&survivor) {
            continue;
        }
        let (ts, to) = (Type::TVar(survivor.clone()), Type::TVar(o.clone()));
        if !admits_lower(flags, env, hist, (&ts, &survivor), o)?
            || !admits_lower(flags, env, hist, (&to, o), &survivor)?
        {
            return Ok(false);
        }
        if commit {
            OpenPair::of(&survivor, o).apply(&survivor, o);
        }
    }
    if commit {
        // the merged cell holds a bound cell's binding from below, where
        // `'r ⊇ [T, 'r]` takes `T` beside the bounds it has
        let merged = Type::TVar(survivor);
        let below = (flags | ContainsFlags::Lower) & !ContainsFlags::Contra;
        for b in bound.iter() {
            let b = Type::TVar(b.clone());
            if !merged.contains_int(below, env, hist, &b)?
                || !b.contains_int(flags, env, hist, &merged)?
            {
                return Ok(false);
            }
        }
        // only once the cells are one: a parameter's bound may decide the
        // cell it is held to
        for d in dropped.iter() {
            bound_members(env, d, true)?;
        }
    }
    Ok(true)
}

/// The members of the bound `b`, a union's or `b` alone, each name over
/// cells as the members of what it names, its parameters held to their
/// bounds where `commit`.
fn bound_members(env: &Env, b: &Type, commit: bool) -> Result<SmallVec<[Type; 8]>> {
    fn names(_: &Env, m: &Type) -> bool {
        matches!(m, Type::Ref(tr) if !tr.params.iter().all(|p| p.tvar_free()))
    }
    let members = match b {
        Type::Set(ms) => &ms[..],
        b => std::slice::from_ref(b),
    };
    Type::unfolded_members(env, members, commit, names)
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

/// A member of a position [`Type::covers_by_parts`] splits, with the
/// rigid cells whose values it stands for.
type Split = (Type, SmallVec<[TVar; 2]>);

/// The members of `t`, each with `holders`: a rigid cell with one typed
/// bound is that bound's members, holding the cell too, one with none a
/// member of its own; a cell not held rigid is left out, except in a rigid
/// cell's bound, where it is a variable of the same definition and is held
/// rigid while `gates` live.
fn split_members(
    env: &Env,
    t: &Type,
    holders: &SmallVec<[TVar; 2]>,
    gates: &mut Vec<RigidGate>,
    out: &mut SmallVec<[Split; 8]>,
) -> Result<()> {
    ensure_sufficient(|| {
        let mut found: SmallVec<[Type; 8]> = SmallVec::new();
        let mut rigid: SmallVec<[TVar; 2]> = SmallVec::new();
        crate::expr::union_members_with(env, t, &mut found, &mut |tv| {
            if let Some(c) = tv.open_cell()
                && (c.is_rigid() || !holders.is_empty())
            {
                rigid.push(c)
            }
        })?;
        for c in rigid.iter().filter(|c| !c.is_rigid()) {
            gates.push(c.open_rigid())
        }
        for m in found {
            match m {
                Type::Primitive(p) => out.extend(
                    p.iter().map(|b| (Type::Primitive(b.into()), holders.clone())),
                ),
                m => out.push((m, holders.clone())),
            }
        }
        for cell in rigid {
            if holders.iter().any(|h| h.same_cell(&cell)) {
                continue;
            }
            match &cell.typed_bounds(env)[..] {
                [b] => {
                    let mut inner = holders.clone();
                    inner.push(cell.clone());
                    split_members(env, b, &inner, gates, out)?
                }
                _ => out.push((Type::TVar(cell), holders.clone())),
            }
        }
        Ok(())
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

/// Two types identical up to `Fn` equality whose cells can be welded;
/// under `commit` their function types are linked as one.
fn identical_linked(hist: &mut ContainsHist, t0: &Type, t1: &Type, commit: bool) -> bool {
    match hist.identical.identical(t0, t1) {
        Identical::Apart => false,
        Identical::Same => !commit || link_equal(t0, t1, true),
        Identical::Renamed => {
            link_equal(t0, t1, false) && (!commit || link_equal(t0, t1, true))
        }
    }
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
#[derive(Debug)]
pub struct TypeMismatch {
    expected: Type,
    actual: Type,
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
        let lower = tv.cell_lower().into_iter().find(|l| {
            let mut hist = ContainsHist::new();
            !conjuncts_admit(tv, BitFlags::empty(), env, &mut hist, l).unwrap_or(true)
        })?;
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

/// A type that two or more undecided members of a union could take. Which
/// takes it would follow the order the check relates things in, so the
/// check takes neither: the union is decided where it comes from.
#[derive(Debug)]
pub struct AmbiguousMember {
    union: Type,
    typ: Type,
    members: SmallVec<[Type; 2]>,
    contra: bool,
    /// The definitions being checked where the refusal was made, whose
    /// variables it names after their gates close.
    declared: Declared,
}

impl AmbiguousMember {
    fn new(
        flags: BitFlags<ContainsFlags>,
        s: &[Type],
        t: &Type,
        members: &[&Type],
    ) -> anyhow::Error {
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
            contra: flags.contains(ContainsFlags::Contra),
            declared: Declared::current(),
        })
    }

    /// Whether the union stands in the type the relation holds a value
    /// in (a binding written, a parameter passed to), not under a
    /// function's parameter, where it is the value's own.
    pub fn in_holder(&self) -> bool {
        !self.contra
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
    fn new(at: TakerUnion, union: &Type, t: &Type, members: &[&Type]) -> anyhow::Error {
        if graphix_dbg_bind() {
            eprintln!("UNDECIDED-TAKER {union} <- {t}");
        }
        if graphix_dbg_cycle_bt() {
            eprintln!("UNDECIDED-TAKER\n{}", std::backtrace::Backtrace::force_capture());
        }
        anyhow::Error::new(UndecidedTaker {
            union: union.clone(),
            typ: t.clone(),
            members: members.iter().map(|m| (*m).clone()).collect(),
            at,
            throws_only: undecided(t) && !undecided_outside_throws(t),
        })
    }

    pub fn at(&self) -> TakerUnion {
        self.at
    }

    /// Whether what the type throws is the only part of it nothing has
    /// decided.
    pub fn throws_only(&self) -> bool {
        self.throws_only
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
        (Type::Map(x), Type::Map(y)) => Arc::ptr_eq(x, y),
        _ => false,
    }
}

/// Whether `a` and `b`, a cell proved ground and a node, are one type
/// ([`resolves_to`]).
#[inline]
fn same_resolution(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::TVar(tv), t) | (t, Type::TVar(tv)) if node_addr(t).is_some() => {
            resolves_to(tv, t)
        }
        _ => false,
    }
}

/// Whether `tv`, proved ground, is `t`: the cell stands for its resolution
/// ([`TVar::ground_resolution`]), and two nodes over one content, or of one
/// kind over the same parts, hold each other.
fn resolves_to(tv: &TVar, t: &Type) -> bool {
    let Some(r) = tv.ground_resolution() else { return false };
    let same = |x: &Type, y: &Type| match (x, y) {
        (Type::Primitive(p0), Type::Primitive(p1)) => p0 == p1,
        (x, y) => same_content(x, y),
    };
    let parts = |x: &[Type], y: &[Type]| {
        x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| same(x, y))
    };
    match (&r, t) {
        (Type::Tuple(x), Type::Tuple(y)) => parts(x, y),
        (Type::Variant(t0, x, _), Type::Variant(t1, y, _)) => t0 == t1 && parts(x, y),
        (r, t) => same_content(r, t),
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

/// Whether `f` holds of a pair of parts `s` and `t` place alike: a
/// function's result and arguments, each member of a union beside the
/// other side, the parts of one constructor on both sides.
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
        match ok {
            true => Ok(()),
            false => {
                let mut e = self.contains_mismatch(env, t);
                if let Some(m) = e.downcast_mut::<TypeMismatch>() {
                    m.infinite = hist.cycled;
                }
                Err(e)
            }
        }
    }

    /// [`Self::check_contains`] where the open cells `self` reaches take
    /// `t`'s parts as lower bounds ([`ContainsFlags::Lower`]).
    pub fn check_contains_lower(&self, env: &Env, t: &Self) -> Result<()> {
        let mut hist = ContainsHist::new();
        let flags = ContainsFlags::Commit | ContainsFlags::Lower;
        match self.contains_int(flags, env, &mut hist, t)? {
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
            held: HeldQuantifiers::of([self, t]),
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
            let minted = matches!(&a, Self::Abstract { id, .. }
                if env.abstract_reps.get(id).is_some());
            let advice = match minted {
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
        let mut asking: LPooled<Vec<(TraitId, Self)>> = LPooled::take();
        tv.cell_constraints()
            .iter()
            .find_map(|c| Self::bound_refusal(env, c, t, &mut asking))
    }

    /// Why `t` fails the trait bound `c`, when `c` is a trait reference.
    fn bound_refusal(
        env: &Env,
        c: &Self,
        t: &Self,
        asking: &mut Vec<(TraitId, Self)>,
    ) -> Option<anyhow::Error> {
        let Self::Ref(r) = c else { return None };
        let tid = env.trait_of_ref(r)?;
        let def = env.trait_def(tid)?;
        let mut hist = ContainsHist::new();
        let members = Self::dispatch_members(env, t).ok()?;
        let seen = env.impls_of(tid).unwrap_or_default();
        let name = &def.name;
        for m in members.iter() {
            if Self::trait_contains(tid, BitFlags::empty(), env, &mut hist, m).ok()? {
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
        let holds = |a: &Type, b: &Type| a.contains_with_flags(BitFlags::empty(), env, b);
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
        let holds = |a: &Type, b: &Type| a.contains_with_flags(BitFlags::empty(), env, b);
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

    /// [`Self::check_contains`] with rigid enforcement; the def gate's
    /// acceptance checks only.
    pub fn check_contains_rigid(&self, env: &Env, t: &Self) -> Result<()> {
        let flags = ContainsFlags::Commit | ContainsFlags::RigidCheck;
        let ok = self.contains_int(flags, env, &mut ContainsHist::new(), t)?;
        if ok { Ok(()) } else { Err(self.contains_mismatch(env, t)) }
    }

    /// Whether `self` holds every member of a union.
    fn holds_each(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        members: &[Self],
    ) -> Result<bool> {
        Ok(members
            .iter()
            .map(|t| self.contains_int(flags, env, hist, t))
            .collect::<Result<AndAc>>()?
            .0)
    }

    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        #[cfg(test)]
        CONTAINS_STEPS.with(|n| n.set(n.get() + 1));
        ensure_sufficient(|| self.contains_int_inner(flags, env, hist, t))
    }

    fn contains_int_inner(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        if (self as *const Type) == (t as *const Type)
            || same_content(self, t)
            || same_resolution(self, t)
        {
            return Ok(true);
        }
        // a commit over two closed sides binds nothing: it is the probe
        let flags = match flags.contains(ContainsFlags::Commit) {
            true if hist.closed(self) && hist.closed(t) => BitFlags::empty(),
            _ => flags,
        };
        if flags.contains(ContainsFlags::Commit) {
            if hist.commit_held(flags, self, t) {
                return Ok(true);
            }
            let (height, outer) = hist.enter_verdict();
            let r = self.contains_dispatch(flags, env, hist, t);
            let rests = hist.leave_verdict(outer);
            let r = r?;
            if r {
                hist.commit_put((flags, self, t), height, rests);
            }
            return Ok(r);
        }
        // a probe binds nothing: its verdict under its flags is memoized
        let probe = probe_flags(flags);
        if let Some(r) = hist.probe_get(probe, self, t) {
            return Ok(r);
        }
        let (height, outer) = hist.enter_verdict();
        let r = match self.contains_dispatch(flags, env, hist, t) {
            Ok(false) if flags.contains(ContainsFlags::RigidCheck) => {
                self.contains_unfolded(flags, env, hist, t)
            }
            r => r,
        };
        let rests = hist.leave_verdict(outer);
        let r = r?;
        hist.probe_put((probe, self, t), r, height, rests);
        Ok(r)
    }

    /// A rigid check that failed, asked again with each union member that
    /// names its members ([`names_members`]) as those members: a commit
    /// reads them so first, and a rigid check never commits. `Opt<'a> ⊇
    /// [Opt<'a>, null]` over `type Opt<'x> = ['x, null]` holds for a
    /// declared `'a`. False where neither side holds such a member.
    fn contains_unfolded(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        let mut unfolded = |t: &Self| match t {
            Self::Set(s)
                if s.iter().any(|m| hist.names_members(env, m)) && !hist.closed(t) =>
            {
                Self::unfolded(env, s, false).ok()
            }
            _ => None,
        };
        match (unfolded(self), unfolded(t)) {
            (None, None) => Ok(false),
            (t0, t1) => t0.as_ref().unwrap_or(self).contains_int(
                flags,
                env,
                hist,
                t1.as_ref().unwrap_or(t),
            ),
        }
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
        // a fixed signature's variable is any type its conjuncts admit: it
        // holds a predicate only where one of them implies it
        let sealed = flags.contains(ContainsFlags::Sealed);
        if sealed
            && let (Self::Bound(_), Self::TVar(tv)) = (self, t)
            && tv.binding().is_none()
            && tv.is_rigid()
        {
            return tv.conjunct_implies(env, self);
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
            (Self::Bound(Bound::OneNumber), t) => t.one_number_holds(env, commit, sealed),
            (_, Self::Bound(Bound::OneNumber)) => Ok(false),
            (Self::Bound(Bound::NoError), t) => Ok(!t.may_be_error(env, commit, sealed)?),
            (_, Self::Bound(Bound::NoError)) => Ok(false),
            (Self::Bound(Bound::Discernible), t) => {
                Ok(t.indiscernible(env, Open::Benign, false).is_none())
            }
            (_, Self::Bound(Bound::Discernible)) => Ok(false),
            (Self::Bound(Bound::Ordered), t) => {
                Ok(t.indiscernible(env, Open::Benign, true).is_none())
            }
            (_, Self::Bound(Bound::Ordered)) => Ok(false),
            // judged of the call's settled binding (`site_bounds`): refused
            // here, a union target would narrow to one member instead
            (Self::Bound(Bound::JsonData | Bound::TomlData), _) => Ok(true),
            (_, Self::Bound(Bound::JsonData | Bound::TomlData)) => Ok(false),
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
                    && tr0.same_definition(tr1, env)
                    && tr0.params.len() == tr1.params.len()
                    && tr0
                        .params
                        .iter()
                        .zip(tr1.params.iter())
                        .all(|(a, b)| identical_linked(hist, a, b, commit)) =>
            {
                tr0.hold_bounds_with(tr1, env, commit)
            }
            (t0 @ Self::Ref(_), t1) | (t0, t1 @ Self::Ref(_)) => {
                if let (Self::Ref(tr0), Self::Ref(tr1)) = (t0, t1)
                    && tr0.same_definition(tr1, env)
                    && tr0.params.len() == tr1.params.len()
                    && let Some(held) =
                        Self::applications_hold(flags, env, hist, (t0, tr0), (t1, tr1))?
                {
                    return Ok(held);
                }
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                let open = commit && !(hist.closed_now(t0) && hist.closed_now(t1));
                if open {
                    hist.unfolding.push((t0.clone(), t1.clone()))
                }
                let held = hist.assuming(key, |hist| {
                    let Some(e0) = hist.expand_ref(t0, key.0, env, commit)? else {
                        return Ok(false);
                    };
                    let Some(e1) = hist.expand_ref(t1, key.1, env, commit)? else {
                        return Ok(false);
                    };
                    e0.contains_int(flags, env, hist, &e1)
                });
                if open {
                    hist.unfolding.pop();
                }
                held
            }
            // ⊥ fits whatever the cell becomes; binding would only
            // foreclose its writers. The cell remembers it was fed ⊥.
            (Self::TVar(t0), Self::Bottom) => {
                if commit && t0.binding().is_none() {
                    t0.mark_bottom_fed();
                }
                Ok(true)
            }
            // ⊥ ⊇ 'r has one solution, so an open cell commits unless it is
            // bounded from below; a bound cell answers for its binding; a
            // rigid one may be anything.
            (Self::Bottom, Self::TVar(t0)) => match t0.binding() {
                Some(b) => Self::Bottom.contains_int(flags, env, hist, &b),
                None if (rigid || commit) && t0.is_rigid() => Ok(false),
                None => {
                    if !holds_lower(t0, below(flags), env, hist, &Self::Bottom)? {
                        return Ok(false);
                    }
                    if commit {
                        if !commit_lower(t0, flags, env, hist, &Self::Bottom)? {
                            return Ok(false);
                        }
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
            (t0 @ Self::Bottom, Self::Set(s)) => t0.holds_each(flags, env, hist, s),
            (Self::Bottom, _) => Ok(false),
            (_, Self::Bottom) => Ok(true),
            (Self::TVar(t0), Self::Any) => {
                // Clone the binding out before recursing, here and in
                // every deref arm: the walk can revisit this cell and
                // write-lock it, and the locks are non-reentrant.
                if let Some(t0) = t0.binding() {
                    return t0.contains_int(flags, env, hist, t);
                }
                if (rigid || commit) && t0.is_rigid() {
                    return rigid_holds_by_lower((self, t0), flags, env, hist, t);
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
                    && commit_cell_bounds(&cell, flags, env, hist, &Self::Any)?
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
            (Self::Map(m0), Self::Map(m1)) => {
                Ok(m0.key.contains_int(flags, env, hist, &m1.key)?
                    && m0.value.contains_int(flags, env, hist, &m1.value)?)
            }
            (Self::Primitive(p), Self::Map { .. }) => Ok(p.contains(Typ::Map)),
            (Self::Map(m), Self::Primitive(p)) if *p == BitFlags::from(Typ::Map) => {
                Ok(m.key.contains_int(flags, env, hist, &Type::Any)?
                    && m.value.contains_int(flags, env, hist, &Type::Any)?)
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
            (Self::TVar(t0), Self::TVar(t1)) if one_variable(t0, t1) => Ok(true),
            (tt0 @ Self::TVar(t0), tt1 @ Self::TVar(t1)) => {
                Self::contains_tvars(flags, env, hist, (tt0, t0), (tt1, t1))
            }
            // Deref first: a bound cell answers for its binding. The
            // occurs check guards the open cell's bind; an open cell
            // `t1` reaches (the μ-shape `'r ⊇ [T, 'r]`) takes the arms
            // below, where the union collapses.
            (Self::TVar(t0), t1)
                if t0.is_bound() || hist.holds_no_cell(t1) || !t0.would_cycle(t1) =>
            {
                if let Some(t0) = t0.binding() {
                    return t0.contains_int(flags, env, hist, t1);
                }
                if (rigid || commit) && t0.is_rigid() {
                    return rigid_holds_by_lower((self, t0), flags, env, hist, t1);
                }
                if takes_lower(flags, t0) {
                    if !conjuncts_admit(t0, flags, env, hist, t1)? {
                        return Ok(false);
                    }
                    if commit {
                        if graphix_dbg_bind() {
                            eprintln!(
                                "LOWER '{}({:x}) above {t1:?}",
                                t0.name,
                                t0.cell_addr()
                            );
                        }
                        t0.add_lower(t1.clone());
                    }
                    return Ok(true);
                }
                if let Some(held) = held_by_lower(t0, flags, env, hist, t1)? {
                    return Ok(held);
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
                    if !commit_cell_bounds(t0, flags, env, hist, t1)? {
                        return Ok(false);
                    }
                    t0.bind(t1.clone());
                }
                Ok(true)
            }
            (t0, Self::TVar(t1))
                if t1.is_bound() || hist.holds_no_cell(t0) || !t1.would_cycle(t0) =>
            {
                if let Some(t1) = t1.binding() {
                    return t0.contains_int(flags, env, hist, &t1);
                }
                if flags.contains(ContainsFlags::Lower | ContainsFlags::IntoMember)
                    && !flags.contains(ContainsFlags::Contra)
                    && !t1.is_rigid()
                    && let Self::Set(s) = t0
                    && let Some(m) = s.iter().find(|m| is_unbound_tvar(m))
                {
                    return m.contains_int(flags, env, hist, t);
                }
                // t0 contains an arbitrary 'a only when it contains one
                // of the cell's conjuncts ('a ⊆ C ⊆ t0) or, a union, one
                // member holds it; a commit cannot bind the cell, so it
                // takes this verdict too.
                if (rigid || commit) && t1.is_rigid() {
                    if let Self::Set(s) = t0
                        && Self::set_commit_flat(s, flags, env, hist, t)?.unwrap_or(false)
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
                            if !commit_cell_bounds(t1, flags, env, hist, &w)? {
                                return Ok(false);
                            }
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
                            return Ok(Self::set_commit_flat(s, flags, env, hist, t)?
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
                    t1.weld_lower();
                    let Ok(w) = t1.witness(env)? else { return Ok(false) };
                    if !t1.cell_lower().is_empty()
                        && !commit_conjuncts(t1, flags, env, hist, &w)?
                    {
                        return Ok(false);
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
                    if !commit_cell_bounds(t1, flags, env, hist, t0)? {
                        return Ok(false);
                    }
                    t1.bind(t0.clone());
                }
                Ok(true)
            }
            (t0 @ Self::Set(_), t1 @ Self::Set(_))
                if identical_linked(hist, t0, t1, commit) =>
            {
                Ok(true)
            }
            // a member that names a variable or a union stands as what it
            // names: `['a, Id<'b>]` over `type Id<'x> = 'x` is `['a, 'b]`
            (Self::Set(s0), _)
                if commit
                    && !hist.closed(self)
                    && s0.iter().any(|m| hist.names_members(env, m)) =>
            {
                let flat = Self::unfolded(env, s0, true)?;
                let held = t.with_deref(|t| match t {
                    Some(Self::Set(s1)) => Self::unfolded_held(env, s1).map(Some),
                    _ => Ok(None),
                })?;
                flat.contains_int(flags, env, hist, held.as_ref().unwrap_or(t))
            }
            (_, Self::Set(s1))
                if commit
                    && !hist.closed(t)
                    && s1.iter().any(|m| hist.names_members(env, m)) =>
            {
                self.contains_int(flags, env, hist, &Self::unfolded(env, s1, true)?)
            }
            // A set with a bare unbound tvar member binds the tvar to
            // the residue (the rhs members no concrete lhs member
            // covers) in one act; per-member it would capture greedily.
            (t0 @ Self::Set(s0), Self::Set(s1)) if s0.iter().any(is_unbound_tvar) => {
                if !commit || !shares_bare_cell(s0) {
                    return Self::place_members(flags, env, hist, t0, s0, s1);
                }
                // the deepest a constructor member fits decides the variable
                // its bare sibling shares
                Self::place_in_order(
                    flags,
                    env,
                    hist,
                    (t0, s0, s1),
                    (ContainsHist::ordered, Inherit::Expanding),
                    Self::place_members,
                )
            }
            // Where two lhs members hold open cells, the first that fits an
            // rhs member alone may decide them so that a later member fits
            // none.
            (t0 @ Self::Set(s0), Self::Set(s1)) => {
                let holding = |m: &&Type| !is_unbound_tvar(m) && m.has_unbound();
                let placed = hist.trial || hist.as_tried;
                if !commit || placed || s0.iter().filter(holding).nth(1).is_none() {
                    return Self::place_each(flags, env, hist, t0, s0, s1);
                }
                Self::place_in_order(
                    flags,
                    env,
                    hist,
                    (t0, s0, s1),
                    (ContainsHist::trial, Inherit::Assumed),
                    Self::place_each,
                )
            }
            // `'r ⊇ [T.., 'r]` is `'r ⊇ [T..]`: the cell's own member asks
            // nothing of it, an open member that is not rigid is one type
            // with it, and the rest are held together, as a union without
            // it would be
            (Self::TVar(t0), Self::Set(s))
                if !t0.is_rigid() && s.iter().any(|m| is_own_cell(m, t0)) =>
            {
                let mut held: SmallVec<[Type; 4]> = SmallVec::new();
                for m in s.iter() {
                    if is_own_cell(m, t0) {
                        continue;
                    }
                    if is_flexible_var(m) {
                        if !self.contains_int(flags, env, hist, m)? {
                            return Ok(false);
                        }
                    } else if !held.iter().any(|h| one_member(h, m)) {
                        held.push(m.clone());
                    }
                }
                match held.len() {
                    0 => Ok(true),
                    1 => self.contains_int(flags, env, hist, &held[0]),
                    _ => {
                        let held = Self::Set(Arc::from_iter(held.drain(..)));
                        self.contains_int(flags, env, hist, &held)
                    }
                }
            }
            (t0, Self::Set(s)) => t0.holds_each(flags, env, hist, s),
            (Self::Set(s), t) => {
                if graphix_dbg_bind() {
                    eprintln!("SET-T {} >= {t}", Self::Set(s.clone()));
                }
                // a member that is `t` covers it and binds nothing
                if s.iter().any(|m| identical_linked(hist, m, t, commit)) {
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
                        let bare = match uncovered.is_empty() {
                            true => None,
                            false => Self::residue_member(
                                s,
                                s.iter().filter(|m| is_unbound_tvar(m)),
                                flags,
                                env,
                                hist,
                                &Self::Primitive(uncovered),
                            )?,
                        };
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

    /// Whether `t0` holds `t1`, two applications of one definition, by
    /// their arguments: each pair as the definition's body meets its
    /// parameter ([`variance::meets`]), which is what their expansions
    /// ask, in a step a parameter. `None` where only the expansions tell:
    /// a parameter the walk must judge whose arguments differ, or a commit
    /// that does not hold in parameter order (a cell two arguments share
    /// is bound by the first position the walk meets, which may be
    /// another). A name that means nothing here, and a definition a name
    /// it reaches is not yet defined for, are one type where their
    /// arguments are equal and hold no open cell.
    fn applications_hold(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        (t0, tr0): (&Type, &TypeRef),
        (t1, tr1): (&Type, &TypeRef),
    ) -> Result<Option<bool>> {
        let commit = flags.contains(ContainsFlags::Commit);
        let probe = flags & !BitFlags::from(ContainsFlags::Commit);
        let same = |a: &Type, b: &Type| {
            std::ptr::eq(a, b)
                || same_content(a, b)
                || matches!((a, b), (Type::TVar(a), Type::TVar(b)) if one_variable(a, b))
        };
        let pairs = || tr0.params.iter().zip(tr1.params.iter());
        let meets = tr0.resolve_peek(env).and_then(|def| variance::meets(env, &def));
        let Some(meets) = meets.filter(|meets| {
            pairs().zip(meets.iter()).all(|((a, b), m)| !m.walk() || same(a, b))
        }) else {
            let open =
                |p: &Type, hist: &mut ContainsHist| !hist.closed_now(p) && p.has_free();
            if tr0.resolve_peek(env).is_some()
                && tr0.params.iter().chain(tr1.params.iter()).any(|p| open(p, hist))
            {
                return Ok(None);
            }
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
            return match equal(probe, hist)? && (!commit || equal(flags, hist)?) {
                true => Ok(Some(tr0.hold_bounds_with(tr1, env, commit)?)),
                false => Ok(None),
            };
        };
        if !Self::arguments_hold(&meets, probe, env, hist, &tr0.params, &tr1.params)? {
            return Ok(Some(false));
        }
        if commit {
            let trial =
                hist.trial
                    || on_copies(Some((&*hist, env)), [t0, t1], |copies, trial| {
                        match &copies {
                            [Type::Ref(c0), Type::Ref(c1)] => Self::arguments_hold(
                                &meets, flags, env, trial, &c0.params, &c1.params,
                            ),
                            _ => Ok(false),
                        }
                    })?;
            if !trial
                || !Self::arguments_hold(
                    &meets,
                    flags,
                    env,
                    hist,
                    &tr0.params,
                    &tr1.params,
                )?
            {
                hist.strays();
                return Ok(None);
            }
        }
        Ok(Some(tr0.hold_bounds_with(tr1, env, commit)?))
    }

    /// The questions `meets` asks of each pair of `args0` and `args1`,
    /// under `flags`: one asked with the sides swapped is asked as under
    /// a function's argument, and a pair held both ways as an invariant
    /// position holds it.
    fn arguments_hold(
        meets: &[variance::Meets],
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        args0: &[Type],
        args1: &[Type],
    ) -> Result<bool> {
        let swapped = flags ^ ContainsFlags::Contra;
        for ((a, b), m) in args0.iter().zip(args1.iter()).zip(meets.iter()) {
            if m.walk() {
                continue;
            }
            let as_written = match (m.holds(), m.held()) {
                (true, true) => a.equal_int(flags, env, hist, b)?,
                (true, false) => a.contains_int(flags, env, hist, b)?,
                (false, true) => b.contains_int(flags, env, hist, a)?,
                (false, false) => true,
            };
            let holds = as_written
                && match (m.held_swapped(), m.holds_swapped()) {
                    (true, true) => b.equal_int(swapped, env, hist, a)?,
                    (true, false) => b.contains_int(swapped, env, hist, a)?,
                    (false, true) => a.contains_int(swapped, env, hist, b)?,
                    (false, false) => true,
                };
            if !holds {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Two distinct cells. Occurs checks run only where a cell would
    /// bind, alias or take a lower bound, the last along the relation's
    /// direction ([`Toward`]): an open cell meeting a bound cell whose
    /// binding reaches it is the μ-shape through a binding (a copy would
    /// bind the infinite type, so the binding is walked, where `'r ⊇ [T,
    /// 'r]` collapses); any other cycle under a constructor is refused,
    /// and the cells on one through cells and unions alone merge
    /// ([`merge_bare_cycle`]).
    fn contains_tvars(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        (tt0, t0): (&Type, &TVar),
        (tt1, t1): (&Type, &TVar),
    ) -> Result<bool> {
        let commit = flags.contains(ContainsFlags::Commit);
        let rigid = flags.contains(ContainsFlags::RigidCheck);
        let (cell0, cell1) = (t0.cell(), t1.cell());
        let (addr0, addr1) = (cell0.data_ptr().addr(), cell1.data_ptr().addr());
        // `t0` takes `t1` as a lower bound, rather than a merge or a bind
        let lower = takes_lower(flags, t0);
        // A cycle under a constructor has no finite solution; the cells on
        // a bare one are one type.
        let refuse = |cycle: Cycle, paths: &[Reach], hist: &mut ContainsHist| {
            if graphix_dbg_cycle_bt() {
                eprintln!(
                    "CYCLE-REFUSED-PAIR {cycle:?} ({addr0:x},{addr1:x})\n{}",
                    std::backtrace::Backtrace::force_capture()
                );
            }
            match cycle {
                Cycle::Constructor => {
                    hist.cycled = true;
                    Ok(false)
                }
                Cycle::Bare => merge_bare_cycle(flags, env, hist, paths),
            }
        };
        match (t0.binding(), t1.binding()) {
            // Two bound cells decide by walking the bindings; every bind
            // is occurs-checked, and the pair memo bounds a walk that
            // meets the pair again.
            (Some(b0), Some(b1)) => hist.assuming((Some(addr0), Some(addr1)), |hist| {
                b0.contains_int(flags, env, hist, &b1)
            }),
            (None, Some(b1)) => {
                // A rigid receiver must not bind; re-verdict against
                // the binding.
                if would_cycle_inner(&cell0, tt1) || ((rigid || commit) && t0.is_rigid())
                {
                    return tt0.contains_int(flags, env, hist, &b1);
                }
                let up = if lower { Toward::Above } else { Toward::Any };
                if let Some(cycle) = cycle_toward(&cell1, tt0, up) {
                    return refuse(cycle, &[(t1, tt0, up)], hist);
                }
                if lower {
                    if !conjuncts_admit(t0, flags, env, hist, &b1)? {
                        return Ok(false);
                    }
                    if commit {
                        t0.add_lower(b1);
                    }
                    return Ok(true);
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
                    if !commit_cell_bounds(t0, flags, env, hist, &b1)? {
                        return Ok(false);
                    }
                    t0.copy(t1, b1);
                }
                Ok(true)
            }
            (Some(b0), None) => {
                if would_cycle_inner(&cell1, tt0) || ((rigid || commit) && t1.is_rigid())
                {
                    return b0.contains_int(flags, env, hist, tt1);
                }
                if let Some(cycle) = cycle_toward(&cell0, tt1, Toward::Any) {
                    return refuse(cycle, &[(t0, tt1, Toward::Any)], hist);
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
                    if !commit_cell_bounds(t1, flags, env, hist, &b0)? {
                        return Ok(false);
                    }
                    // below a ⊥ its input settled the cell can be nothing
                    // else: linked, so a refusal over it finds the binding
                    // that holds the settled cell
                    match t0.settled_bottom() {
                        true => t1.copy(t0, tt0.clone()),
                        false => t1.copy(t0, b0),
                    }
                }
                Ok(true)
            }
            // two quantifiers: t0 holds t1 only through one of t0's lower
            // bounds ('t1 ⊆ L ⊆ t0) or one of t1's conjuncts ('t1 ⊆ C ⊆
            // t0), whatever an occurs check would say. A lower bound that
            // holds the cell adds none; a conjunct a lower bound holds is
            // put above that bound's open cells, so it is tried last
            (None, None) if t0.is_rigid() && t1.is_rigid() => {
                let cons = t1.cell_constraints();
                hist.assuming((Some(addr0), Some(addr1)), |hist| {
                    if rigid_holds_by_lower((tt0, t0), flags, env, hist, tt1)? {
                        return Ok(true);
                    }
                    for c in cons.iter() {
                        if tt0.contains_int(flags, env, hist, c)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                })
            }
            (None, None) => {
                let (down, up) = match lower {
                    true => (Toward::Below, Toward::Above),
                    false => (Toward::Any, Toward::Any),
                };
                if let Some(cycle) =
                    cycle_toward(&cell0, tt1, down).max(cycle_toward(&cell1, tt0, up))
                {
                    return refuse(cycle, &[(t0, tt1, down), (t1, tt0, up)], hist);
                }
                if flags.contains(ContainsFlags::Sealed) {
                    let held = match (t0.is_rigid(), t1.is_rigid()) {
                        (false, true) => conjuncts_admit(t0, flags, env, hist, tt1)?,
                        (true, false) => conjuncts_admit(t1, flags, env, hist, tt0)?,
                        _ => true,
                    };
                    if !held {
                        return Ok(false);
                    }
                }
                // an open cell below is bounded by what bounds `t0`, as a
                // merge would have it
                if lower {
                    if commit {
                        for c in t0.cell_constraints() {
                            t1.add_cell_constraint(c)
                        }
                        t0.add_lower(tt1.clone());
                    }
                    return Ok(true);
                }
                if !admits_lower(flags, env, hist, (tt0, t0), t1)?
                    || !admits_lower(flags, env, hist, (tt1, t1), t0)?
                {
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

    /// The members of the union `s`, each that `names` holds
    /// ([`names_members`], [`names_union`]) replaced by what it names, a
    /// union's members in its place, until none does; a named type's
    /// parameter bounds are committed where `commit`. A cell, or a named
    /// type applied to the same parameters, met again adds nothing.
    fn unfolded_members(
        env: &Env,
        s: &[Type],
        commit: bool,
        names: fn(&Env, &Type) -> bool,
    ) -> Result<SmallVec<[Type; 8]>> {
        fn unfold(
            env: &Env,
            m: &Type,
            (commit, names): (bool, fn(&Env, &Type) -> bool),
            (cells, applied): (&mut AddrSet, &mut Expanded),
            depth: usize,
            out: &mut SmallVec<[Type; 8]>,
        ) -> Result<()> {
            if !names(env, m) {
                return Ok(out.push(m.clone()));
            }
            if depth > HEAD_CHAIN_LIMIT {
                bail!("a union's member names itself")
            }
            match m {
                Type::TVar(tv) if !cells.insert(tv.cell_addr()) => return Ok(()),
                Type::Ref(tr)
                    if let Some(def) = tr.probe_def_key(env)
                        && !first_expansion(applied, def, &tr.params) =>
                {
                    return Ok(());
                }
                _ => (),
            }
            let mut head = m.clone();
            for _ in 0..HEAD_CHAIN_LIMIT {
                head = match head.with_deref(|d| d.cloned()) {
                    Some(d @ Type::Ref(_)) if commit => d.lookup_ref(env)?,
                    Some(d @ Type::Ref(_)) => match d.lookup_ref_probe(env) {
                        Some((_, x)) => x,
                        None => break,
                    },
                    Some(d) => d,
                    None => break,
                };
                if !matches!(head, Type::Ref(_)) {
                    break;
                }
            }
            ensure_sufficient(|| match &head {
                Type::Set(ms) => ms.iter().try_for_each(|m| {
                    unfold(env, m, (commit, names), (cells, applied), depth + 1, out)
                }),
                _ => Ok(out.push(head.clone())),
            })
        }
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        let mut cells: LPooled<AddrSet> = LPooled::take();
        let mut applied: LPooled<Expanded> = LPooled::take();
        for m in s.iter() {
            unfold(env, m, (commit, names), (&mut cells, &mut applied), 0, &mut members)?
        }
        Ok(members)
    }

    /// The union `s` with each member that [`names_members`] as what it
    /// names ([`Self::unfolded_members`]).
    fn unfolded(env: &Env, s: &[Type], commit: bool) -> Result<Type> {
        let members = Self::unfolded_members(env, s, commit, names_members)?;
        Ok(Self::Set(Arc::from_iter(members)))
    }

    /// The union `s` as an unfolded one must contain it: every member
    /// that names a union as what it names ([`names_union`]) and its
    /// primitives a type apiece, so each goes to the holder's member that
    /// takes it and only what none takes to an open one.
    fn unfolded_held(env: &Env, s: &[Type]) -> Result<Type> {
        let members = Self::unfolded_members(env, s, true, names_union)?;
        Ok(Self::Set(Arc::from_iter(members.iter().flat_map(|m| {
            let (bits, whole) = match m {
                Self::Primitive(p) => (Some(p.iter()), None),
                m => (None, Some(m.clone())),
            };
            bits.into_iter().flatten().map(|b| Self::Primitive(b.into())).chain(whole)
        }))))
    }

    /// Decide `s ⊇ t` by the member that takes `t` ([`Self::take`]),
    /// structural members before bare unbound tvars, which admit anything:
    /// `['b, Array<'b>]` must bind an array through `Array<'b>`. `None`
    /// when no member admits `t`.
    fn set_commit(
        s: &[Type],
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<Option<bool>> {
        let probe = guide(flags);
        let commit = flags.contains(ContainsFlags::Commit);
        let mut admit: SmallVec<[&Type; 4]> = SmallVec::new();
        let mut structural = 0;
        for bare in [false, true] {
            for m in s.iter().filter(|m| is_unbound_tvar(m) == bare) {
                if !m.contains_int(probe, env, hist, t)? {
                    continue;
                }
                if !commit || hist.binds_nothing(m, t) {
                    return Ok(Some(true));
                }
                admit.push(m)
            }
            if !bare {
                structural = admit.len()
            }
        }
        let (structural, bare) = admit.split_at(structural);
        Self::take(s, [structural, bare], flags, env, hist, t)
    }

    /// [`Self::set_commit`] over `s` flat ([`Self::unfolded`]): for a union
    /// met whole, a cell going into it, where no arm has unfolded it.
    fn set_commit_flat(
        s: &[Type],
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<Option<bool>> {
        let named = flags.contains(ContainsFlags::Commit)
            && s.iter().any(|m| hist.names_members(env, m));
        match named {
            false => Self::set_commit(s, flags, env, hist, t),
            true => match &Self::unfolded(env, s, true)? {
                Self::Set(s) => Self::set_commit(s, flags, env, hist, t),
                _ => Ok(None),
            },
        }
    }

    /// Commit `t` into `m`, an undecided member of a union, where it holds
    /// `t` with every cell of both held as it stands, so it decides none
    /// of them; `None` where it does not, or holds no undecided cell.
    fn takes_as_it_stands(
        m: &Type,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<Option<bool>> {
        if !undecided(m) {
            return Ok(None);
        }
        let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
        hold_rigid(m, &mut gates);
        hold_rigid(t, &mut gates);
        let outer = hist.gate();
        let r = on_copies(Some((&*hist, env)), [m, t], |[m, t], trial| {
            m.contains_int(flags, env, trial, &t)
        })
        .and_then(|holds| match holds {
            true => m.contains_int(flags, env, hist, t).map(Some),
            false => Ok(None),
        });
        hist.ungate(outer);
        r
    }

    /// Commit `t` into the member of the union `s` that takes it, of those
    /// a probe admits it into, a tier at a time; `None` with no member. A
    /// probe judges each part apart, so it admits `fn(i: 'c) -> 'c` into
    /// `fn(path: string) -> i64`, which a commit refuses after binding 'c:
    /// where another member admits `t` too, a member takes it only once it
    /// holds on copies, unless a commit can decide no cell of either
    /// ([`decides_no_cell`]): that commit is the probe that admitted it. A
    /// member that holds it as it stands, deciding no cell of the union
    /// or of `t`, takes it first; else, in the first tier where a member
    /// holds it, the one decided member that does, else the one undecided
    /// member. Of several undecided ones that could, the one that holds
    /// the others ([`widest`]) takes it where that leaves each of them
    /// holding `t` as it stands ([`decides_each`], then the same of the
    /// walk's own take): no cell another would decide is left open. Any
    /// other two that could are a choice only the order of the check's
    /// relations would make ([`AmbiguousMember`]), and so is a decided
    /// member beside another for a type a pre-unification meets
    /// undecided. Two decided ones that could take a type holding a cell
    /// to decide, and would decide it differently ([`decide_alike`]),
    /// would decide it by their place in the union ([`UndecidedTaker`]); the
    /// walk on copies that admits a decided member reports which of the
    /// others hold `t` as it stands after it ([`takes_leaving`]).
    /// A walk on copies takes the first, and a trial commits its members
    /// directly: a trial nested in each would double the walk at every
    /// level of a union typedef chain. Where none holds it, the last
    /// admitted answers.
    fn take(
        s: &[Type],
        tiers: [&[&Type]; 2],
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Type,
    ) -> Result<Option<bool>> {
        let Some(last) = tiers.iter().rev().find_map(|tier| tier.last()) else {
            return Ok(None);
        };
        if tiers[0].len() + tiers[1].len() == 1 {
            return Ok(Some(last.contains_int(flags, env, hist, t)?));
        }
        hist.strays();
        if !hist.ordered {
            for m in tiers.iter().flat_map(|tier| tier.iter().copied()) {
                if let Some(r) = Self::takes_as_it_stands(m, flags, env, hist, t)? {
                    return Ok(Some(r));
                }
            }
        }
        let mut fixed_t = None;
        let mut t_open = None;
        for tier in tiers {
            let mut open: SmallVec<[&Type; 2]> = SmallVec::new();
            let mut decided: SmallVec<[&Type; 2]> = SmallVec::new();
            let mut decided_after: SmallVec<[(usize, Option<u64>); 2]> = SmallVec::new();
            for (at, m) in tier.iter().copied().enumerate() {
                if hist.trial {
                    if m.contains_int(flags, env, hist, t)? {
                        return Ok(Some(true));
                    }
                    continue;
                }
                if open.is_empty() && decided.is_empty() && std::ptr::eq(m, *last) {
                    break;
                }
                // CR claude for claude: [perf/low] A choice between the members of a
                // union asks a walk on copies for each member that admits the type, at
                // every such union the walk meets, and each of those walks what is below
                // it: over a recursive definition with two members of one head (`type
                // D0<'a0, 'a1, 'a2> = [Error<D0<'a0, 'a1, null>>, Error<D0<'a1, 'a0, 'a2>>,
                // `Y(D0<'a1, 'a2, 'a0>)]`, its textual twin F0, and `fn(z: Any, k: F0<Any,
                // i64, Any>) -> Any` holding `|z: 'q0, k: D0<'q0, 'q1, 'q2>| z`) the commit
                // takes 5.3M contains steps: 1516 walks on copies from here take 3.6M of
                // them, the walk itself 9k commit steps and 1.7M probe steps, since a probe
                // verdict over cells stands only until the next cell is written.
                // --no-cache --check takes 21.4G instructions for it (an empty program
                // 0.74G), 22.8G for s2311_0224_two.gx and 4.3G for s2311_0289_two.gx. The
                // placement a trial allows (ContainsHist::as_tried) cannot take this
                // choice as its trial did: two undecided members that could take the type
                // are refused (AmbiguousMember), which only the walk itself judges.
                // Untried: a walk on copies' verdict kept by its pair while no cell
                // either side reaches is written, so a union met again asks none; one walk
                // on copies a union that reports every member's verdict. probes:
                // ~/ptmp/cr29/scratch/r23/23c/oact/{s2311_0140_two,s2311_0224_two,
                // s2311_0289_two}.gx, generator ~/ptmp/cr29/scratch/r20/20c/x1/gen2.py
                // (MODE=perm, seed 2311) (take-member-trials-01)
                // 2026-11-13 claude: Partial. A choice whose commit can decide no cell of
                // the member or the type (contains::decides_no_cell: every open cell held
                // rigid, and every one their conjuncts and lower bounds reach) is given to
                // the first member the probe admits, with no walk on copies: all 1516 walks
                // of the lead probe were such, under a member taking a type as it stands.
                // Pin typ::contains::tests::a_choice_no_commit_can_decide_walks_no_copies
                // (5.30M -> 224k and 832k -> 76k steps; fails with the rule undone).
                // --no-cache --check instructions, base-r24 -> lane: s2311_0140_two 22.25G
                // -> 1.65G, s2311_0289_two 4.45G -> 1.08G, s2311_0224_two 23.56G -> 17.84G.
                // What stands is 0224's: decided members (F0's two Error members) taking an
                // undecided type in the walk itself, 568 walks on copies taking 2.9M of the
                // commit's 4.0M steps; those commits decide a cell, so each needs its
                // trial. The untried memo above would still ask 364 of the 568 even keyed
                // by the pair's text with no validity check, and one walk reporting every
                // member's verdict at most halves them: each walk takes the applications
                // below on fresh copies. What would end it: a commit tried in place and
                // undone (a trail of writes) instead of on copies, or the placement a trial
                // allows taking a choice between decided members as its trial did. Probes
                // and generators: ~/ptmp/cr29/scratch/r24/24f/tmt/ (README).
                // 2026-11-13 claude: Review (lane 24f): the partial fix holds for the
                // probes, with a correction. decides_no_cell saw every cell the member and
                // the type reach held rigid, but a function type whose quantifier an
                // earlier task left open takes it fresh for each walk
                // (FnType::contains_int), and a commit decides the copy: under a later
                // task, with 'c held rigid, [fn(x: string) -> i64, fn(x: string) -> string]
                // ⊇ fn<'c: Any>(x: 'c) -> 'c was refused, since the probe admits the
                // identity into the first member part by part and that commit refuses it,
                // where base-r24 walks each member and the second takes it. decides_no_cell
                // now also refuses a cell an earlier task left open outside a top-level
                // input (Type-level only: no program found that reaches it). Pin
                // typ::contains::tests::a_commit_decides_a_quantifier_an_earlier_task_left_open
                // fails with the correction undone; the fixer's pin still fails with the
                // rule undone (5.30M steps). --no-cache --check after the correction: 0140
                // 1.66G, 0289 1.08G, 0224 17.87G; guicheck 39.50G base, 39.58G lane; 600
                // genrigid programs (seed 11) identical to base-r24.
                // 2026-11-14 claude: Lane 25e: partial. On base-r25 s2311_0224_two.gx took
                // 55.0G instructions (17.9G at 24f's correction): decide_alike
                // (undecided-into-two-decided-01) asks a second walk on copies per pair of
                // decided takers, 1136 walks and 6.6M of the commit's 12.76M contains steps,
                // its first half repeating the walk take had just run for that member. take's
                // walk on copies now also reports, for each other member of the tier, whether
                // it holds the type as it stands after the commit (takes_leaving, bits in one
                // u64, none where a probe failed or the tier holds over 64 members, which fall
                // back to decide_alike), so the pair's verdict is read from the two walks the
                // choice already made: 12.76M -> 7.72M steps, 55.0G -> 34.2G instructions (0140
                // 1.62G and 0289 1.07G unchanged). Pin
                // typ::contains::tests::a_choice_between_decided_members_walks_each_member_once
                // (12.76M steps without it, bound 9M). With an assert that each pair read
                // equals decide_alike's: graphix-types and graphix-tests lib, census (2967
                // files, identical to base-r25), gen2 perm/amb/all seed 2511 and genrigid seed
                // 21 (2000 programs) identical. guicheck 39.68G base, 39.80G lane; deep_nesting
                // passes. What stands: 568 choices x 2 members, each a walk on copies of the
                // member's commit (about 4.4k steps) plus the other's probe (about 1.4k); the
                // memo or trail ideas above are untouched. probe:
                // ~/ptmp/cr29/scratch/r25/25e/s2311_0224_two.gx
                let fixed = decides_no_cell(m)
                    && *fixed_t.get_or_insert_with(|| decides_no_cell(t));
                let wants_after = !hist.ordered
                    && !hist.early
                    && tier.len() <= u64::BITS as usize
                    && !undecided(m)
                    && *t_open.get_or_insert_with(|| undecided(t));
                let (holds, after) = match fixed {
                    true => (true, None),
                    false => takes_leaving(tier, at, wants_after, flags, env, hist, t)?,
                };
                if !holds {
                    continue;
                }
                let member_open = undecided(m);
                if hist.ordered || !(member_open || undecided(t)) {
                    return Ok(Some(m.contains_int(flags, env, hist, t)?));
                }
                let takers = match member_open || hist.early {
                    true => &mut open,
                    false => &mut decided,
                };
                if !takers.iter().any(|o| one_member(o, m)) {
                    takers.push(m);
                    if !member_open && !hist.early {
                        decided_after.push((at, after));
                    }
                }
            }
            if let [m, rest @ ..] = &decided[..] {
                let mut alike = true;
                for (o, (o_at, o_after)) in rest.iter().zip(&decided_after[1..]) {
                    let (m_at, m_after) = decided_after[0];
                    alike = alike
                        && match (m_after, *o_after) {
                            (Some(m_after), Some(o_after)) => {
                                m_after >> o_at & 1 == 1 && o_after >> m_at & 1 == 1
                            }
                            _ => decide_alike(m, o, flags, env, hist, t)?,
                        }
                }
                if alike {
                    return Ok(Some(m.contains_int(flags, env, hist, t)?));
                }
                let at = match flags.contains(ContainsFlags::Contra) {
                    true => TakerUnion::Value,
                    false => TakerUnion::Holder,
                };
                let union = Type::Set(Arc::from_iter(s.iter().cloned()));
                return Err(UndecidedTaker::new(at, &union, t, &decided));
            }
            let taker = match open[..] {
                [] => continue,
                [m] => m,
                _ if hist.early => return Err(AmbiguousMember::new(flags, s, t, &open)),
                _ => match widest(&open, flags, env, hist)? {
                    Some(m) if decides_each(m, &open, flags, env, hist, t)? => m,
                    _ => return Err(AmbiguousMember::new(flags, s, t, &open)),
                },
            };
            let took = taker.contains_int(flags, env, hist, t)?;
            for o in open.iter().copied().filter(|o| took && !std::ptr::eq(*o, taker)) {
                if !holds_as_it_stands(o, flags, env, hist, t)? {
                    return Err(AmbiguousMember::new(flags, s, t, &open));
                }
            }
            return Ok(Some(took));
        }
        Ok(Some(last.contains_int(flags, env, hist, t)?))
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
        if !hist.closed_now(&t) && t.has_free() {
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
        // the members' heads, those of a member naming a union included,
        // each name once
        let mut heads: LPooled<Vec<Type>> = LPooled::take();
        let mut unfolded = Unfolded::default();
        let mut todo: LPooled<Vec<Type>> = s.iter().rev().cloned().collect();
        while let Some(m) = todo.pop() {
            if !unfolded.first(&m) {
                continue;
            }
            match head(env, hist, &m)? {
                Type::Set(ref ms) => todo.extend(ms.iter().rev().cloned()),
                h => heads.push(h),
            }
        }
        let mut cands: LPooled<Vec<LPooled<Vec<Type>>>> = LPooled::take();
        for m in heads.iter() {
            let args: Option<LPooled<Vec<Type>>> = match (&t, m) {
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
        let probe = (flags & ContainsFlags::Sealed) | ContainsFlags::RigidCheck;
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
        let pool = distributing.map(|j| hist.pool(env, cands.iter().map(|c| &c[j])));
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
    /// must cover the positions after it. A rigid cell there is its typed
    /// bound's members, each also picked by a candidate holding the cell,
    /// or, with no one bound, a member of its own; a rigid cell its bound
    /// holds splits the same way. A probe.
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
            let mut members: SmallVec<[Split; 8]> = SmallVec::new();
            let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
            split_members(env, first, &SmallVec::new(), &mut gates, &mut members)?;
            if members.is_empty() {
                members.push((first.clone(), SmallVec::new()))
            }
            // members that select the same candidates ask the same question
            // of the rest: it is asked once
            let mut asked: SmallVec<[SmallVec<[usize; 8]>; 4]> = SmallVec::new();
            let flags = ContainsFlags::RigidCheck.into();
            for (m, cells) in members.iter() {
                let mut picked: SmallVec<[usize; 8]> = SmallVec::new();
                for (i, c) in cands.iter().enumerate() {
                    let mut holds = c[0].contains_int(flags, env, hist, m)?;
                    for cell in cells.iter() {
                        if holds {
                            break;
                        }
                        let cell = Type::TVar(cell.clone());
                        holds = c[0].contains_int(flags, env, hist, &cell)?;
                    }
                    if holds {
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

    /// `place` over the members `s1` in the first of two orders that
    /// places them all on copies, each walk on copies starting from
    /// `trial`: as written, then deepest first; as written where neither
    /// does. A [`ContainsHist::trial`] that places them took every union
    /// below with two members holding open cells as written, so the
    /// placement it allows does too (`as_tried`), asking no trial of its
    /// own for them until it meets a choice ([`ContainsHist::strays`]).
    fn place_in_order(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        (t0, s0, s1): (&Self, &[Type], &[Type]),
        (trial, inherit): (fn() -> ContainsHist, Inherit),
        place: fn(
            BitFlags<ContainsFlags>,
            &Env,
            &mut ContainsHist,
            &Self,
            &[Type],
            &[Type],
        ) -> Result<bool>,
    ) -> Result<bool> {
        let mut deepest: SmallVec<[Type; 8]> = s1.iter().cloned().collect();
        let mut depths: LPooled<AddrMap<usize>> = LPooled::take();
        deepest.sort_by_key(|m| std::cmp::Reverse(nesting(m, &mut depths)));
        if deepest[..] == s1[..] {
            return place(flags, env, hist, t0, s0, s1);
        }
        for order in [s1, &deepest[..]] {
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
            let Self::Set(c0) = &copy0 else { unreachable!("a copy keeps its shape") };
            let mut on_copies = trial();
            hist.seed(&mut on_copies, &mut known, env, inherit);
            if place(flags, env, &mut on_copies, &copy0, c0, &copy1)? {
                let tried = hist.as_tried || on_copies.trial;
                let outer = mem::replace(&mut hist.as_tried, tried);
                let r = place(flags, env, hist, t0, s0, order);
                hist.as_tried &= outer;
                return r;
            }
        }
        place(flags, env, hist, t0, s0, s1)
    }

    /// `t0`, a union `s0` holding no bare open member, contains the
    /// members `s1` in that order. Member-wise identity pre-pass: only the
    /// residue takes the general per-member walk (which is O(|s0|·|s1|)
    /// per level). An open rhs member takes from the lhs members its known
    /// siblings leave: in `[A, E] ⊇ ['b, E]`, 'b is A, never E; where they
    /// leave none it can take, from the whole lhs. A primitive member a
    /// known primitive only partly fills is left whole: in `[i64, string,
    /// A] ⊇ [i64, 'b]`, 'b is `[i64, string, A]`.
    fn place_each(
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t0: &Self,
        s0: &[Type],
        s1: &[Type],
    ) -> Result<bool> {
        let commit = flags.contains(ContainsFlags::Commit);
        let bits = |t: &Type| {
            t.with_deref(|d| match d {
                Some(Self::Primitive(p)) => Some(*p),
                _ => None,
            })
        };
        let fills = |c: &Type, m: &Type| match (bits(c), bits(m)) {
            (Some(c), Some(m)) => (c & !m).is_empty(),
            _ => true,
        };
        let mut used: SmallVec<[bool; 8]> = smallvec::smallvec![false; s0.len()];
        let mut open: SmallVec<[&Type; 2]> = SmallVec::new();
        for m in s1.iter() {
            if is_unbound_tvar(m) {
                open.push(m);
                continue;
            }
            if let Some(i) = s0.iter().position(|c| identical_linked(hist, c, m, commit))
            {
                used[i] = true;
                continue;
            }
            if !t0.contains_int(flags, env, hist, m)? {
                return Ok(false);
            }
            for (i, c) in s0.iter().enumerate() {
                if c.contains_int(guide(flags), env, hist, m)? {
                    used[i] = fills(c, m);
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

    /// `s1` with the members that only one constructor member of `s0`
    /// admits, and that bind a cell of it, replaced by one member that
    /// holds them together (`Array<'b> ⊇ [Array<i64>, Array<string>]` is
    /// `'b ⊇ [i64, string]`), so that the order they stand in decides
    /// nothing. `None` where no two members share a taker.
    fn join_cell_sharing(
        s0: &[Type],
        free: &[bool],
        s1: &[Type],
        env: &Env,
        hist: &mut ContainsHist,
    ) -> Result<Option<LPooled<Vec<Type>>>> {
        let probe = BitFlags::empty();
        let mut takers: LPooled<Vec<Option<usize>>> = LPooled::take();
        let mut shapes: LPooled<Vec<Type>> = LPooled::take();
        let mut shared = false;
        for m in s1.iter() {
            let mut taker = None;
            let shape = match m {
                Type::Ref(_) => m.lookup_ref_with(env, false)?,
                _ => None,
            };
            let shape = shape.as_ref().unwrap_or(m);
            shapes.push(shape.clone());
            if ctor_arity(shape).is_some_and(|n| n > 0) {
                let mut found = None;
                let mut many = false;
                for (i, c) in s0.iter().enumerate().filter(|(i, _)| !free[*i]) {
                    if c.contains_int(probe, env, hist, shape)? {
                        many |= found.is_some();
                        found = Some(i);
                    }
                }
                if let Some(i) = found
                    && !many
                    && !hist.binds_nothing(&s0[i], shape)
                {
                    shared |= takers.contains(&Some(i));
                    taker = Some(i);
                }
            }
            takers.push(taker);
        }
        if !shared {
            return Ok(None);
        }
        let mut out: LPooled<Vec<Type>> = LPooled::take();
        let mut done: SmallVec<[usize; 4]> = SmallVec::new();
        for (m, taker) in s1.iter().zip(takers.iter()) {
            let Some(i) = *taker else {
                out.push(m.clone());
                continue;
            };
            if done.contains(&i) {
                continue;
            }
            let group = shapes
                .iter()
                .zip(takers.iter())
                .filter_map(|(m, t)| (*t == Some(i)).then_some(m));
            done.push(i);
            match Self::join_ctor(group, hist) {
                Some(joined) => out.push(joined),
                None => out.extend(
                    s1.iter()
                        .zip(takers.iter())
                        .filter_map(|(m, t)| (*t == Some(i)).then(|| m.clone())),
                ),
            }
        }
        Ok(Some(out))
    }

    /// The one type that holds every member of `group`, applications of
    /// one constructor to as many parameters, each parameter the union of
    /// the members' there.
    fn join_ctor<'a>(
        group: impl Iterator<Item = &'a Type> + Clone,
        hist: &mut ContainsHist,
    ) -> Option<Type> {
        let first = group.clone().next()?;
        let arity = ctor_arity(first)?;
        let same = |m: &Type| {
            ctor_arity(m) == Some(arity)
                && match (first, m) {
                    (Self::Array(_), Self::Array(_))
                    | (Self::List(_), Self::List(_))
                    | (Self::Error(_), Self::Error(_))
                    | (Self::Map(_), Self::Map(_))
                    | (Self::Tuple(_), Self::Tuple(_)) => true,
                    (Self::Variant(a, ..), Self::Variant(b, ..)) => a == b,
                    (Self::Struct(a), Self::Struct(b)) => {
                        a.iter().zip(b.iter()).all(|(a, b)| a.0 == b.0)
                    }
                    _ => false,
                }
        };
        if !group.clone().all(same) {
            return None;
        }
        let mut joined: SmallVec<[Type; 4]> = SmallVec::new();
        for i in 0..arity {
            joined
                .push(hist.normal_union(group.clone().filter_map(|m| ctor_param(m, i))));
        }
        let mut joined = joined.into_iter();
        let mut next = || joined.next();
        match first {
            Self::Array(_) => Some(Self::Array(Arc::new(next()?))),
            Self::List(_) => Some(Self::List(Arc::new(next()?))),
            Self::Error(_) => Some(Self::Error(Arc::new(next()?))),
            Self::Map(_) => {
                let key = next()?;
                let value = next()?;
                Some(Self::Map(Arc::new(MapType { key, value })))
            }
            Self::Tuple(_) => Some(Self::Tuple(Arc::from_iter(joined))),
            Self::Variant(tag, _, at) => {
                Some(Self::Variant(tag.clone(), Arc::from_iter(joined), at.clone()))
            }
            Self::Struct(fs) => Some(Self::Struct(Arc::from_iter(
                fs.iter().zip(joined).map(|((n, _, at), t)| (n.clone(), t, at.clone())),
            ))),
            _ => None,
        }
    }

    /// Give `m` to the member of the union `s0` that takes it
    /// ([`Self::take`]), of those not `free` on entry; `None` where it is
    /// left to the open member: none admits it.
    fn place_covered(
        s0: &[Type],
        free: &[bool],
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        m: &Type,
    ) -> Result<Option<bool>> {
        let commit = flags.contains(ContainsFlags::Commit);
        let mut admit: SmallVec<[&Type; 4]> = SmallVec::new();
        for (c, _) in s0.iter().zip(free).filter(|(_, free)| !**free) {
            if c.contains_int(guide(flags), env, hist, m)? {
                admit.push(c);
                if !commit || hist.binds_nothing(c, m) {
                    admit.truncate(0);
                    admit.push(c);
                    break;
                }
            }
        }
        match admit.is_empty() {
            true => Ok(None),
            false if !commit => Ok(Some(true)),
            false => Self::take(s0, [&admit, &[]], flags, env, hist, m),
        }
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
        let joined = match commit {
            true => Self::join_cell_sharing(s0, &free, s1, env, hist)?,
            false => None,
        };
        let s1: &[Type] = joined.as_deref().map_or(s1, |j| &j[..]);
        let mut residue: LPooled<Vec<Type>> = LPooled::take();
        for m in s1.iter() {
            // An rhs member equal to the whole lhs set is
            // covered reflexively; as residue it would close a
            // cycle.
            let reflexive = m
                .deref_cloned()
                .is_some_and(|md| identical_linked(hist, t0, &md, commit));
            if reflexive {
                continue;
            }
            // An rhs member that is one of the lhs's own cells
            // is covered reflexively too.
            let own_cell = match m {
                Self::TVar(mtv) => s0.iter().any(|c| match c {
                    Self::TVar(ctv) => one_variable(ctv, mtv),
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
            match Self::place_covered(s0, &free, flags, env, hist, m)? {
                Some(true) => continue,
                Some(false) => return Ok(false),
                None => (),
            }
            let bits = m.with_deref(|d| match d {
                Some(Self::Primitive(p)) if p.len() > 1 => Some(*p),
                _ => None,
            });
            let Some(bits) = bits else {
                residue.push(m.clone());
                continue;
            };
            let mut left = BitFlags::empty();
            for b in bits.iter() {
                let part = Self::Primitive(b.into());
                match Self::place_covered(s0, &free, flags, env, hist, &part)? {
                    Some(true) => (),
                    Some(false) => return Ok(false),
                    None => left |= b,
                }
            }
            if left == bits {
                residue.push(m.clone());
            } else if !left.is_empty() {
                residue.push(Self::Primitive(left));
            }
        }
        // CR claude for eric: [risk/low] A definition's check and its instance's
        // second check (GRAPHIX_NO_SUBST=1) still meet one union of two undecided
        // members in two states. `let mk = |c, a, b| (select c { true => a, false => b
        // }, [a], [b]); let g = |p, q| { let t = mk(true, never(), never()); let u =
        // never(); u <- t.0; u <- p; u <- q; let r: (Array<string>, Array<i64>) =
        // (t.1, t.2); r }; g(1, "s")`: the definition's check writes `p`, an open
        // parameter, into the union whole (the rule below) and the annotation then
        // decides the members, so `--check` and the default build accept and run it;
        // the instance checked again holds `p` at i64 when it meets `u <- p`, before
        // the annotation, and is refused (AmbiguousMember). An instance takes its
        // definition's types by default, so only the debug A/B splits; base-r20
        // splits there too, with a mismatch at the annotation. probes:
        // ~/ptmp/cr29/scratch/r19/19g/{s3,s4}.gx (union-nosubst-order-01)
        // an undecided rhs member takes a union of undecided members whole,
        // as it would alone: no one member is picked for it
        // 2026-11-11 claude: Another split of the same kind, where a variable of a
        // typedef union stays undecided at the call: `type S2<'p, 'q> = [{v: 'p}, {v:
        // 'q}]; let f = |y1: 'b, x0: S2<'b, 'c>, y2: 'c| (x0, y1, y2); let g = |t|
        // f(2, {v: 2}, never()); g(1)` checks and runs, and under GRAPHIX_NO_SUBST=1
        // the instance of g is refused, "type mismatch ([{ v: i64 }, { v: _ }], i64,
        // _) does not contain ({ v: 'b: i64 }, 'b: i64, 'c: i64)": the instance holds
        // 'c at `_`, and its own check relates the call's result to that signature
        // after the arguments, where the callee's open {v: 'c} goes into the decided
        // {v: i64} and 'c is then i64 against `_`. base-r22 alike. Since
        // decided-arg-waits-01 the call with the union's parameter first is accepted
        // too and splits the same way; in that CR's differential (5484 generated
        // programs) 32 programs differ in verdict under NO_SUBST, 22 of them accepted
        // by base-r22 and split there too, the other 10 newly accepted orders of the
        // same shapes (this one, and choice-widen-order-01's). probes:
        // ~/ptmp/cr29/scratch/r21/21t/d32/nsb/{b1,b2,b3}.gx, nscmp.out
        // 2026-11-12 claude: Two more splits of this kind, which the default build
        // takes since open-instance-signature-01 (a cell only a union's members hold
        // is placed member for member): 'type P<'p, 'q> = [Array<'p>, Array<'q>]; type
        // O<'t> = ['t, null]; let f = |x: P<'a, string>, y: O<'a>| (x, y); let g = |u,
        // v| f(u, v); g(["s"], null)' and the recursive G<'b>/H<'b> wrapper at never()
        // check and run, and under GRAPHIX_NO_SUBST=1 the instance of g is refused,
        // "([Array<string>, Array<_>], [null, _]) does not contain (Array<string>,
        // [null, 'a: string])": its own check gives the callee's Array<'a> to the
        // decided Array<string>. Before and after that fix alike. probes:
        // ~/ptmp/cr29/scratch/r23/23b/ois/{o5,u5min}.gx, nest/{n1,n4,n5,n6}.gx
        // 2026-11-13 claude: Re-run on base-r24 and lane 24h (bafcd4bc0), alike: s3,
        // s4, b1, b3, n1, n4, n5, n6, o5 and u5min still check and run by default and
        // are refused under GRAPHIX_NO_SUBST=1 as quoted above; b2 is refused at the
        // check in both modes (the ordered call rule). Not fixed, since neither kind
        // is local to the second check, which meets decided types where its definition
        // met open cells. (A) s3, s4 and most generated splits: an instance's formal
        // holds its site's decided type, so its second check makes the call its
        // definition made with an open argument as the call written out with a decided
        // one, and D36 refuses that on purpose. The wrapper 'let g = |u0| f(u0,
        // (["s"], "s")); let k = |z| g(z); k(&["s"])' over 'f = |x0: &P<'b, 'a>, x1:
        // (P<'a, 'b>, 'a)|' runs by default and its instance is refused under
        // NO_SUBST, 'Array<string> could go to Array<'b> or Array<'a>', the message
        // the default check gives 'f(&["s"], (["s"], "s"))' written out (w1, w2); s3
        // written out with 'let p: [i64, string] = 1' is refused with s3's message
        // where 'u <- p' comes before the annotation and runs after it (d5, d6). (B)
        // b1, b3, n*, o5, u5min: an instance is built at its site's signature after
        // resolve_tvars (CallSite::setup_static_bind), where a cell its input settled
        // is a literal ⊥, and its second check relates the body's type to that return
        // after the body, where the definition's return took the body's type whole;
        // the callee's open {v: 'c} or Array<'a> then meets two decided members, {v:
        // i64} and {v: _}, and goes to the first (lane 24d, since 093fa4399, refuses
        // it there instead). Written out, the default check does the same by the order
        // of the tuple's positions: 'let r: ([{v: i64}, {v: _}], i64, _) = f(2, {v:
        // 2}, never())' over b3's f is refused with b3's message, and with the bare
        // position first it runs (d1, d2; o5's types the other way round, d3 runs, d4
        // is refused). Tried and reverted: reopening the instance's vacuous ⊥ cells
        // for the second check in lambda::instantiate changed nothing (the ⊥ is
        // literal by then); reopened before the resolve, base's Type::take would still
        // give the open member to the decided {v: i64} before the undecided one and
        // the unify-back would refuse it; on top of lane 24d's
        // undecided-into-decided-member-01 rule it might agree, untried. Measured with
        // lane 24d's genu.py, seed 9301, 1500 families, 6715 programs: base-r24 and
        // lane 24h each accept 2286 by default, of which 324 differ under NO_SUBST,
        // the same 324 on both: 270 member-choice refusals (A, all in wrapper or
        // nested shapes) and 54 mismatches at an instance's return (B); lane 24d's
        // reviewer measured 1866 of 6302 on 24d. Recommendation: since D36 a verdict
        // follows the order its relations are met in, so a second check's refusal is
        // no oracle for substitution; keep NO_SUBST as the A/B of what both modes
        // accept (types, elaboration, values) and stop reading its refusals as splits,
        // or let an instance whose second check refuses take its definition's types
        // with the refusal logged. Whether NO_SUBST should keep refusing is a choice
        // about the debug tool, not about what the language accepts. probes:
        // ~/ptmp/cr29/scratch/r24/24h/uno/ (README, nsdiff.py, ns9301.txt)
        // 2026-11-13 claude: Review of never-under-union-bound-01 (lane 24c): two more
        // programs the default build runs and GRAPHIX_NO_SUBST=1 refuses, base-r24
        // alike and with that CR's wait on or off (2 of 8600 generated programs). A
        // wrapper passes a never() part to a chain of union bounds: `let f = 'a0:
        // ('a1, 'a4), 'a1: [Array<'a2>, bool], 'a2: ['a3, bool], 'a3: ['a4, string],
        // 'a4: [Array<'a5>, null], 'a5: Number |x0: 'a0, x1: 'a1| (x0, x1); let g =
        // |p0, p1| f(p0, p1); g((never(), [1]), [never()])`. The definition's check
        // settles the never()'s cell to the chain's own union, and the instance
        // checked again meets that union whole against the chain, "'a1: unbound within
        // [bool, Array<'a2: unbound within [bool, 'a3: unbound within [string, 'a4:
        // Array<i64>]]>] does not contain Array<[bool, string, Array<i64>]>". Not
        // diagnosed further. probes:
        // ~/ptmp/cr29/scratch/r24/24c/nuubrev/nosubst_{p01498,p01204}.gx
        // 2026-11-14 claude: Fixing rigid-narrowed-by-callee-bound-01 (lane 24c): one
        // more split, with a declared variable: `let g = |x: Array<['c, i64]>, s: 'c|
        // x; let f = |v: 'a, w: Array<['a, null]>| g(w, v); f(1, [2])` checks and
        // prints [2], and under GRAPHIX_NO_SUBST=1 the instance at i64 is refused,
        // "'c: null does not contain i64": its own check gives the i64 of `[i64,
        // null]` to the union's i64 member and only the null to 'c, where the
        // definition's check put 'c above `['a, null]` whole. base-r24 alike; 2 of 480
        // generated twins. probes: ~/ptmp/cr29/scratch/r24/24c/rnb/l/l1t.gx, gen/w.out
        // (w05-an-v-xs-{b,c}-t)
        // 2026-11-14 claude: Fixing bound-union-binds-argument-early-01 (lane 24c):
        // one more split, the bound form and its written twin alike. `let f = 'x:
        // Array<['c, null]> |y: 'x, k: fn(a: 'x) -> i64, z: 'c| z; let g = |u| { let v
        // = [[u], u]; f(v, |a| 0, null) }; g(null)` prints null fused, with
        // --no-fusion and under GRAPHIX_PAR=force, and GRAPHIX_NO_SUBST=1 refuses the
        // instance, "type mismatch 'c: Array<null> does not contain null"; with `y:
        // Array<['c, null]>` written in both places the same, on the lane before that
        // fix too (the bound form was refused at the check then). probes:
        // ~/ptmp/cr29/scratch/r24/24c/buba/g3/p0563-{b,t}.gx
        // 2026-11-13 claude: Since undecided-into-two-decided-01 (093fa4399) an
        // instance checked again is refused far more often: a variable its call left
        // undecided is ⊥ in the instance, a decided member beside the other one, so
        // the callee's own open cell could go to either (UndecidedTaker). 'let f = |a,
        // b, c: bool| select c { true => [a], false => [b] }; let h = |t, u| f(u,
        // never(), t); h(true, 1.5)' and 'type P<'p, 'q> = [Array<'p>, Array<'q>]; let
        // f = |x: 'a, y: 'b| -> P<'a, 'b> [x]; let g = |t| f(t, never()); g(1.5)' run
        // by default and on base-r24 under GRAPHIX_NO_SUBST=1, where the lane refuses
        // them, 'Array<'b: unbound> could go to Array<f64> or Array<_> of the union
        // [Array<f64>, Array<_>], and nothing has decided which'. Of the 6302 programs
        // of genu.py (seeds 9301, 9302 wide, 9303) the lane accepts, 1866 differ under
        // NO_SUBST, before and after undecided-into-decided-member-01's fix alike;
        // against base-r24 (4001 accepted of seeds 9301 and 9302 wide) 545 differ on
        // the lane alone and 618 on both. The default build, where an instance
        // takes its definition's types, runs every one of them. probes:
        // ~/ptmp/cr29/scratch/r24/24d/udm-rev/{u3,p10,p11}.gx, diff-pre/sound.txt,
        // diff-base/sound-summary.txt
        // 2026-11-14 claude: Round 25 lane 25e, re-run on base-r25 (3fcf32fc0): s3 and
        // s4 (r19/19g), b1 and b3 (r21/21t/d32/nsb), o5 (r23/23b/ois), l1t
        // (r24/24c/rnb/l) and p0563-b (r24/24c/buba/g3) all still check and run by
        // default and are refused under GRAPHIX_NO_SUBST=1 with the messages quoted
        // above (AmbiguousMember or UndecidedTaker on an open member, or 'c: null does
        // not contain i64). Not fixed, and left a CR (PARTIAL): every one is a case
        // where the instance's second check meets decided types (its site's) at the
        // point its definition's check met open cells, so making it agree means either
        // putting the open cells back for the second check (tried in lane 24h, the
        // cells are literal bottom by then) or reordering the relations of the second
        // check (order chasing, dropped under D36), and neither is local or small. The
        // shipped mode is sound in all of them (identical fused, --no-fusion and
        // PAR=force). The one local change left is the debug tool's, not the
        // checker's: an instance whose second check refuses takes its definition's
        // types with the refusal logged. That changes only what NO_SUBST refuses, so
        // it is Eric's choice about the A/B and not made here. Recommendation
        // unchanged from the 2026-11-13 note: read NO_SUBST refusals as the A/B of
        // types, elaboration and values over what both modes accept, not as a split;
        // if that is ruled, delete this CR and the same-kind ones at select.rs (the
        // two NO_SUBST notes near the select arm walk).
        // 2026-11-14 claude: Coordinator, round 25: whether a refusal only
        // GRAPHIX_NO_SUBST=1 makes is a defect is D42 in ~/ptmp/cr-discuss.md (Open);
        // three rounds tried a fix and undid it. Readdressed to eric.
        let mut undecided_free = s0
            .iter()
            .zip(&free)
            .filter_map(|(m, free)| (*free && undecided(m)).then_some(m));
        let first = undecided_free.next();
        if first.is_some_and(|first| undecided_free.any(|m| !one_member(first, m))) {
            let mut i = 0;
            while i < residue.len() {
                if !is_unbound_tvar(&residue[i]) || !undecided(&residue[i]) {
                    i += 1;
                    continue;
                }
                let m = residue.remove(i);
                if !t0.contains_int(flags, env, hist, &m)? {
                    return Ok(false);
                }
            }
        }
        if residue.is_empty() {
            return Ok(true);
        }
        let target = hist.normal_union(residue.iter());
        let free = s0.iter().zip(&free).filter_map(|(m, free)| free.then_some(m));
        match Self::residue_member(s0, free, flags, env, hist, &target)? {
            Some(m) => m.contains_int(flags, env, hist, &target),
            None => Ok(false),
        }
    }

    /// The member of the union `s`'s open `members` that takes the
    /// `residue` no other member covers: one a probe admits it into (a
    /// rigid member admits what [`rigid_holds_by_lower`] holds, before an
    /// undecided one), else the first. Two undecided
    /// ones that admit it are a choice only the order of the check's
    /// relations would make ([`AmbiguousMember`]); a walk on copies takes
    /// the first.
    fn residue_member<'a>(
        s: &[Type],
        mut members: impl Iterator<Item = &'a Type> + Clone,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        residue: &Type,
    ) -> Result<Option<&'a Type>> {
        let commit = flags.contains(ContainsFlags::Commit);
        let judged = commit && !hist.ordered;
        let mut open: SmallVec<[&Type; 2]> = SmallVec::new();
        for m in members.clone() {
            if !m.contains_int(guide(flags), env, hist, residue)? {
                continue;
            }
            if !judged || !undecided(m) {
                if commit && (undecided(m) || !open.is_empty()) {
                    hist.strays()
                }
                return Ok(Some(m));
            }
            if !open.iter().any(|o| one_member(o, m)) {
                open.push(m)
            }
        }
        match open[..] {
            [] => Ok(members.next()),
            [m] => Ok(Some(m)),
            _ => Err(AmbiguousMember::new(flags, s, residue, &open)),
        }
    }

    /// Containment both ways: an invariant position.
    fn equal_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        // a union of undecided members is decided by what must hold it
        match !hist.closed(self) && self.holds_open_union() && !t.holds_open_union() {
            true => Ok(t.contains_int(flags, env, hist, self)?
                && self.contains_int(flags, env, hist, t)?),
            false => Ok(self.contains_int(flags, env, hist, t)?
                && t.contains_int(flags, env, hist, self)?),
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
                if !Self::trait_contains(tid, flags, env, hist, m)? {
                    return Ok(false);
                }
            }
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
                if flags.contains(ContainsFlags::Commit)
                    && let Some(mut cells) = t.lowered_cells()
                {
                    super::settle::settle_order(&mut cells);
                    for tv in cells.iter() {
                        tv.settle_witness(env)?
                    }
                }
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

    /// Whether a type going into `self` may have to choose among a union's
    /// undecided members ([`Self::take`], [`Self::residue_member`]): `self`
    /// holds, through names, a union two of whose members that hold an
    /// open cell are bare variables, or are built by one constructor.
    pub fn may_choose_member(&self, env: &Env) -> bool {
        // whether one type could be a value of both: a head no walk
        // resolved could be anything
        fn one_constructor(h0: &Type, h1: &Type) -> bool {
            match (h0, h1) {
                (Type::Ref(_) | Type::App(..) | Type::Set(_), _)
                | (_, Type::Ref(_) | Type::App(..) | Type::Set(_))
                | (Type::Primitive(_), Type::Primitive(_)) => true,
                (h0, h1) => h0.shares_constructor(h1),
            }
        }
        fn chooses(env: &Env, ms: &[Type]) -> bool {
            let heads: SmallVec<[Type; 4]> = ms
                .iter()
                .filter(|m| m.has_unbound())
                .map(|m| {
                    named_head(env, m, &mut LPooled::take()).unwrap_or_else(|| m.clone())
                })
                .collect();
            heads.iter().enumerate().any(|(i, h0)| {
                heads[i + 1..].iter().any(|h1| {
                    match (is_unbound_tvar(h0), is_unbound_tvar(h1)) {
                        (true, true) => true,
                        (false, false) => one_constructor(h0, h1),
                        (_, _) => false,
                    }
                })
            })
        }
        fn go(env: &Env, t: &Type, names: &mut Expanded, seen: &mut AddrSet) -> bool {
            ensure_sufficient(|| match t {
                Type::TVar(tv) => tv.binding().is_some_and(|b| go(env, &b, names, seen)),
                Type::Ref(tr) if !tr.params.iter().all(|p| p.tvar_free()) => {
                    match named_head(env, t, names) {
                        Some(Type::Ref(_)) => true,
                        Some(head) => go(env, &head, names, seen),
                        None => false,
                    }
                }
                Type::Set(ms) => {
                    let chooses = match ms.iter().any(|m| names_members(env, m)) {
                        false => chooses(env, ms),
                        true => match &Type::unfolded(env, ms, true) {
                            Ok(Type::Set(ms)) => chooses(env, ms),
                            Ok(_) | Err(_) => true,
                        },
                    };
                    chooses || ms.iter().any(|m| go(env, m, names, seen))
                }
                t if node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
                t => {
                    let mut found = false;
                    t.for_each_child(&mut |c| found = found || go(env, c, names, seen));
                    found
                }
            })
        }
        go(env, self, &mut LPooled::take(), &mut LPooled::take())
    }

    /// [`Self::contains`] for a select's patterns, whose arms are consulted
    /// top down, and for a relation over copies: the first member of a
    /// union that holds a type takes it, where [`Self::contains`] refuses
    /// a choice between two undecided ones ([`AmbiguousMember`]).
    pub fn contains_ordered(&self, env: &Env, t: &Self) -> Result<bool> {
        self.contains_ordered_in(env, &mut ContainsHist::ordered(), t)
    }

    /// [`Self::contains_ordered`] in `hist`, an ordered walk that may have
    /// decided other relations before ([`ContainsHist::next_relation`]).
    pub(super) fn contains_ordered_in(
        &self,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        debug_assert!(hist.ordered);
        hist.next_relation();
        let r = self.contains_int(ContainsFlags::Commit.into(), env, hist, t);
        if graphix_dbg_bind() {
            eprintln!("CONTAINS-ORDERED {self} >= {t} -> {r:?}");
        }
        r
    }

    /// [`Self::check_contains`] as [`Self::contains_ordered`] decides.
    pub fn check_contains_ordered(&self, env: &Env, t: &Self) -> Result<()> {
        let mut hist = ContainsHist::ordered();
        match self.contains_int(ContainsFlags::Commit.into(), env, &mut hist, t)? {
            true => Ok(()),
            false => {
                let mut e = self.contains_mismatch(env, t);
                if let Some(m) = e.downcast_mut::<TypeMismatch>() {
                    m.infinite = hist.cycled;
                }
                Err(e)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typ::{
        normalize::{
            NORMALIZED,
            tests::{ground_pair_chain, while_ground},
        },
        setops::tests::past_eager,
        tvar::{InTask, new_task},
    };
    use arcstr::literal;

    /// A ground cell and a pair of its resolution's parts hold each other
    /// in one step, where a walk of the two takes a step a link.
    #[test]
    fn a_ground_cell_holds_its_resolutions_parts_at_once() {
        let depth = 64;
        let cells = ground_pair_chain(depth);
        let top = Type::TVar(cells[depth].clone());
        let env = Env::default();
        let steps = while_ground(&cells[depth], || {
            let below = Type::TVar(cells[depth - 1].clone()).resolve_tvars();
            let pair = Type::Tuple(Arc::from_iter([below.clone(), below]));
            [(&top, &pair), (&pair, &top)].map(|(t0, t1)| {
                CONTAINS_STEPS.with(|n| n.set(0));
                t0.check_contains(&env, t1).unwrap();
                CONTAINS_STEPS.with(|n| n.get())
            })
        });
        assert_eq!(steps, [1, 1]);
    }

    /// A pair whose parts are not the resolution's own is walked: an equal
    /// copy holds, and a pair of one of its parts and another type does not.
    #[test]
    fn a_ground_cell_takes_only_its_resolutions_own_parts_at_once() {
        let depth = 8;
        let cells = ground_pair_chain(depth);
        let top = Type::TVar(cells[depth].clone());
        let copy =
            Type::TVar(ground_pair_chain(depth - 1)[depth - 1].clone()).resolve_tvars();
        let equal = Type::Tuple(Arc::from_iter([copy.clone(), copy]));
        let env = Env::default();
        let (steps, refused) = while_ground(&cells[depth], || {
            let below = Type::TVar(cells[depth - 1].clone()).resolve_tvars();
            let string = Type::Primitive(Typ::String.into());
            let unequal = Type::Tuple(Arc::from_iter([below, string]));
            let steps = [(&top, &equal), (&equal, &top)].map(|(t0, t1)| {
                CONTAINS_STEPS.with(|n| n.set(0));
                t0.check_contains(&env, t1).unwrap();
                CONTAINS_STEPS.with(|n| n.get())
            });
            let refused = [(&top, &unequal), (&unequal, &top)]
                .map(|(t0, t1)| t0.check_contains(&env, t1).is_err());
            (steps, refused)
        });
        assert!(steps.iter().all(|n| *n > 1), "{steps:?}");
        assert_eq!(refused, [true, true]);
    }

    // A type expanded through a chain of parametric typedefs, a DAG that
    // shares each level, contains the application it expanded with each
    // pair of a node and an application met once: every expansion builds
    // its applications anew, `P11<'a>` twice under each `P12<'a>`.
    #[test]
    fn an_expanded_chain_contains_its_application_once_per_level() {
        let steps = expanded_chain_steps(&["'a"], "('a, i64)", "");
        assert!(steps <= 64 * CHAIN_DEPTH, "{steps} steps");
    }

    // And so where a parameter has no node identity: a primitive, `Any`,
    // a map, each keyed by its content.
    #[test]
    fn an_expanded_chain_over_a_leaf_contains_its_application_once_per_level() {
        for leaf in ["i64", "Any", "Map<string, i64>"] {
            let steps = expanded_chain_steps(&["'a", "'c"], "('a, 'c)", leaf);
            assert!(steps <= 64 * CHAIN_DEPTH, "{leaf}: {steps} steps");
        }
    }

    // And so where each level passes its parameter rebuilt, a new node at
    // every expansion: `P<i><'a> = (P<i-1><('a, i64)>, P<i-1><('a, i64)>)`,
    // its parameters keyed by their content.
    #[test]
    fn an_expanded_chain_over_a_rebuilt_parameter_contains_its_application_once_per_level()
     {
        let steps = chain_steps(&["'a"], "('a, i64)", "('a, i64)", "");
        assert!(steps <= 64 * CHAIN_DEPTH, "{steps} steps");
    }

    // And where that parameter is a function type with no quantifiers of
    // its own, or a struct of 26 fields: both by content, names by text.
    #[test]
    fn an_expanded_chain_over_a_rebuilt_function_or_wide_parameter_contains_its_application_once_per_level()
     {
        let wide: String =
            (1..=25).map(|k| format!("a{k:02}: i64, ")).chain(["z: 'a".into()]).collect();
        for pass in ["fn(x: 'a) -> i64", &format!("{{{wide}}}")] {
            let steps = chain_steps(&["'a"], "('a, i64)", pass, "");
            assert!(steps <= 64 * CHAIN_DEPTH, "{pass}: {steps} steps");
        }
    }

    // A key names a node it met already by where its words start, by the
    // node's identity and a variant's tag: two variants over one payload
    // allocation are two types.
    #[test]
    fn a_param_key_tells_apart_variants_over_one_payload() {
        let ts: Arc<[Type]> = Arc::from_iter([Type::Primitive(Typ::I64.into())]);
        let v = |tag| Type::Variant(tag, ts.clone(), Default::default());
        let ab = Type::Tuple(Arc::from_iter([v(literal!("A")), v(literal!("B"))]));
        let aa = Type::Tuple(Arc::from_iter([v(literal!("A")), v(literal!("A"))]));
        assert_ne!(ab.param_key(), aa.param_key());
    }

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

    // Two function types identical only by pairing their own cells, probed
    // and then committed in one walk: the commit links the cells, so a
    // binding of one reaches the other.
    #[test]
    fn a_commit_after_a_probe_links_cells_paired_by_renaming() {
        let f = |v: &str| {
            crate::expr::parser::parse_type(&format!("fn(x: '{v}) -> '{v}")).unwrap()
        };
        let cell = |t: &Type, v: &str| {
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells[v].clone()
        };
        let (f0, f1) = (f("a"), f("b"));
        let (x, y) = (past_eager(&f0), past_eager(&f1));
        let mut hist = ContainsHist::new();
        assert!(identical_linked(&mut hist, &x, &y, false));
        assert!(identical_linked(&mut hist, &x, &y, true));
        cell(&f0, "a").bind(Type::Primitive(Typ::I64.into()));
        assert_eq!(cell(&f1, "b").binding(), Some(Type::Primitive(Typ::I64.into())));
    }

    // A cell a merge left forwarding to a rigid one is that cell: paired
    // inside a function type with another rigid cell the two functions are
    // two, and with an open one they are one, that cell welded to it.
    #[test]
    fn a_cell_forwarding_to_a_rigid_cell_pairs_as_that_cell() {
        let f = |v: &str| {
            crate::expr::parser::parse_type(&format!("fn(x: '{v}) -> i64")).unwrap()
        };
        let cell = |t: &Type, v: &str| {
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells[v].clone()
        };
        let (f0, f1, f2) = (f("a"), f("b"), f("c"));
        let (a, b, c) = (cell(&f0, "a"), cell(&f1, "b"), cell(&f2, "c"));
        let rigid = b.open_rigid();
        let sharing = TVar::empty_named(literal!("s"));
        sharing.alias(&a);
        sharing.alias_cells(&b);
        assert_eq!(a.binding(), Some(Type::TVar(b.clone())));
        let apart = c.open_rigid();
        let mut hist = ContainsHist::new();
        assert!(!identical_linked(&mut hist, &f0, &f2, false));
        assert!(!identical_linked(&mut hist, &f0, &f2, true));
        drop(apart);
        let mut hist = ContainsHist::new();
        assert!(identical_linked(&mut hist, &f0, &f2, true));
        assert!(c.same_cell(&b));
        drop(rigid);
    }

    // A union's member that is the function it takes, by pairing their
    // cells, names that function's definitions and is named by it, as a
    // member that only contains it does; a probe links nothing.
    #[test]
    fn a_union_member_welded_to_a_function_names_its_definitions() {
        let f = |v: &str| {
            let t = crate::expr::parser::parse_type(&format!("fn(x: '{v}) -> '{v}"));
            let t = t.unwrap();
            let Type::Fn(ft) = &t else { unreachable!() };
            let id = crate::LambdaId::new();
            ft.lambda_ids.set_id(id);
            (t, id)
        };
        let ids = |t: &Type| match t {
            Type::Fn(ft) => ft.lambda_ids.ids(),
            _ => unreachable!(),
        };
        let ((f0, id0), (f1, id1)) = (f("a"), f("b"));
        let held =
            Type::Set(Arc::from_iter([f0.clone(), Type::Primitive(Typ::Null.into())]));
        let env = Env::default();
        assert!(held.contains_with_flags(BitFlags::empty(), &env, &f1).unwrap());
        assert!(!ids(&f0).contains(&id1) && !ids(&f1).contains(&id0));
        assert!(held.contains(&env, &f1).unwrap());
        assert!(ids(&f0).contains(&id1) && ids(&f1).contains(&id0));
    }

    // A member that is the function's type as it stands, their cells
    // shared already, names its definitions too.
    #[test]
    fn a_union_member_that_is_a_function_names_its_definitions() {
        let t = crate::expr::parser::parse_type("fn(x: 'a) -> 'a").unwrap();
        let Type::Fn(ft) = &t else { unreachable!() };
        let node = |id: crate::LambdaId| {
            let lambda_ids = crate::typ::fntyp::LambdaIds::default();
            lambda_ids.set_id(id);
            Type::Fn(Arc::new(FnType { lambda_ids, ..(**ft).clone() }))
        };
        let ids = |t: &Type| match t {
            Type::Fn(ft) => ft.lambda_ids.ids(),
            _ => unreachable!(),
        };
        let (id0, id1) = (crate::LambdaId::new(), crate::LambdaId::new());
        let (f0, f1) = (node(id0), node(id1));
        let held =
            Type::Set(Arc::from_iter([f0.clone(), Type::Primitive(Typ::Null.into())]));
        let env = Env::default();
        assert!(held.contains_with_flags(BitFlags::empty(), &env, &f1).unwrap());
        assert!(!ids(&f0).contains(&id1) && !ids(&f1).contains(&id0));
        assert!(held.contains(&env, &f1).unwrap());
        assert!(ids(&f0).contains(&id1) && ids(&f1).contains(&id0));
    }

    const CHAIN_DEPTH: usize = 12;

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

    // A chain of applications is keyed in one step a level: an
    // application under a parameter by its own id, which a chain built
    // anew over the same leaf shares and one over another leaf does not.
    #[test]
    fn a_chain_of_applications_is_keyed_once_a_level() {
        use crate::typ::KEY_STEPS;
        let (_env, t) = applicable();
        let prim = |t: Typ| Type::Primitive(t.into());
        for n in [100, 200] {
            let mut hist = ContainsHist::new();
            let mut keys = |leaf: Typ| {
                let levels = applied(&t, n, prim(leaf));
                KEY_STEPS.with(|c| c.set(0));
                let keys: Vec<_> =
                    levels.iter().map(|l| hist.pair_key(l).unwrap()).collect();
                (keys, KEY_STEPS.with(|c| c.get()))
            };
            let (first, steps) = keys(Typ::I64);
            assert!(steps <= 4 * n, "{n}: {steps} steps");
            let distinct: AHashSet<_> = first.iter().collect();
            assert_eq!(distinct.len(), n);
            let (again, _) = keys(Typ::I64);
            assert_eq!(first, again);
            let (other, _) = keys(Typ::String);
            assert!(first.iter().zip(other.iter()).all(|(a, b)| a != b));
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
            let mut hist = ContainsHist::new();
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
        let mut hist = ContainsHist::new();
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
        let mut hist = ContainsHist::new();
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
        let mut hist = ContainsHist::new();
        assert_ne!(hist.ref_id(&open, &env), hist.ref_id(&closed, &env));
        tv.bind(Type::Primitive(Typ::I64.into()));
        assert_eq!(hist.ref_id(&open, &env), hist.ref_id(&closed, &env));
    }

    // A committing walk takes the expansion of an application that holds
    // no cell as it is where a copy would change nothing, and a copy of
    // its own where it holds a function type, whose lambdas are a copy's.
    #[test]
    fn a_committed_expansion_is_copied_only_where_a_copy_differs() {
        let (env, t) =
            Env::typed_at_root(&[("T<'x>", "Array<'x>")], "T<fn(x: i64) -> i64>")
                .unwrap();
        let plain = applied(&t, 2, Type::Primitive(Typ::I64.into())).remove(0);
        let mut hist = ContainsHist::new();
        let mut expand = |t: &Type| {
            let id = hist.ref_id(t, &env);
            part(&hist.expand_ref(t, id, &env, true).unwrap().unwrap())
        };
        assert_eq!(expand(&plain).node_key(), expand(&plain).node_key());
        assert_ne!(expand(&t).node_key(), expand(&t).node_key());
    }

    fn expanded_chain_steps(params: &[&str], base: &str, leaf: &str) -> usize {
        chain_steps(params, base, &params.join(", "), leaf)
    }

    /// The containment steps of `P0<params> = base; P1<params> =
    /// (P0<pass>, P0<pass>); ..` to [`CHAIN_DEPTH`], expanded by hand over a
    /// quantified function and `leaf` (when `params` has two), against its
    /// application to them. `pass` is `params` or, over one parameter,
    /// a type of `'a`.
    fn chain_steps(params: &[&str], base: &str, pass: &str, leaf: &str) -> usize {
        let rebuilt = pass != params.join(", ");
        let params = params.join(", ");
        let mut names = vec![format!("P0<{params}>")];
        let mut bodies = vec![String::from(base)];
        for i in 1..=CHAIN_DEPTH {
            names.push(format!("P{i}<{params}>"));
            bodies.push(format!("(P{}<{pass}>, P{}<{pass}>)", i - 1, i - 1));
        }
        let defs: Vec<(&str, &str)> =
            names.iter().zip(bodies.iter()).map(|(n, b)| (&**n, &**b)).collect();
        let (second, last) = match leaf {
            "" => (String::new(), Type::Primitive(Typ::I64.into())),
            leaf => (format!(", {leaf}"), parsed(leaf)),
        };
        let app = format!("P{CHAIN_DEPTH}<fn<'b: Number>(x: 'b) -> 'b{second}>");
        let (env, app) = Env::typed_at_root(&defs, &app).unwrap();
        let mut f = String::from("fn<'c: Number>(x: 'c) -> 'c");
        if rebuilt {
            for _ in 0..CHAIN_DEPTH {
                f = pass.replace("'a", &f);
            }
        }
        let mut expanded = Type::Tuple(Arc::from_iter([parsed(&f), last]));
        for _ in 0..CHAIN_DEPTH {
            expanded = Type::Tuple(Arc::from_iter([expanded.clone(), expanded.clone()]));
        }
        CONTAINS_STEPS.with(|n| n.set(0));
        expanded.check_contains(&env, &app).unwrap();
        CONTAINS_STEPS.with(|n| n.get())
    }

    /// Two families of names recurring through error payloads, each name
    /// three: `X<i> = [Error<X<i+1>>, Error<X<7i+3>>, Error<Error<X<i+2>>>,
    /// `P]` (mod `n`), a `Y` twin with `Q.
    fn error_families(n: usize) -> Vec<(String, String)> {
        let mut defs = vec![];
        for (p, tag, k) in [("X", "`P", 7), ("Y", "`Q", 5)] {
            for i in 0..n {
                let (a, b, c) = ((i + 1) % n, (i * k + 3) % n, (i + 2) % n);
                let body = format!(
                    "[Error<{p}{a}>, Error<{p}{b}>, Error<Error<{p}{c}>>, {tag}]"
                );
                defs.push((format!("{p}{i}"), body));
            }
        }
        defs
    }

    fn at_root(env: &Env, t: &str) -> Type {
        let t = parsed(t).scope_refs(&ModPath::root());
        t.seed_refs(env);
        t
    }

    // A probe's verdict that assumed pairs or questions in progress is
    // kept: a false that took only holds and a true that took only
    // failures for good, any other while what it took stands, so each
    // pair of the families is walked a bounded number of times.
    #[test]
    fn names_recurring_through_errors_contain_in_polynomial_steps() {
        for (n, bound) in [(4, 4_000), (8, 16_000), (16, 64_000)] {
            let defs = error_families(n);
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(a, b)| (&**a, &**b)).collect();
            let (env, _) = Env::typed_at_root(&defs, "X0").unwrap();
            for (a, b, holds) in [
                ("Y0", "X0", false),
                ("X0", "Y0", false),
                ("[X0, Y0]", "Y0", true),
                ("Y0", "[X0, Y0]", false),
            ] {
                let (a, b) = (at_root(&env, a), at_root(&env, b));
                CONTAINS_STEPS.with(|n| n.set(0));
                assert_eq!(a.contains(&env, &b).unwrap(), holds, "n={n}: {a} ⊇ {b}");
                let steps = CONTAINS_STEPS.with(|n| n.get());
                assert!(steps <= bound, "n={n}: {a} ⊇ {b} took {steps} steps");
            }
        }
    }

    // A narrowing over the families asks the probes of all its pairs
    // through one walk, so a pair of names its pairs share is probed once.
    #[test]
    fn a_narrowing_over_names_recurring_through_errors_probes_each_pair_once() {
        for n in [4, 8, 16] {
            let defs = error_families(n);
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(a, b)| (&**a, &**b)).collect();
            let (env, _) = Env::typed_at_root(&defs, "X0").unwrap();
            for (test, reaching) in
                [("Y0", "[X0, Y0]"), ("X0", "Y0"), ("[X1, Y1]", "[X0, Y0]")]
            {
                let (t, r) = (at_root(&env, test), at_root(&env, reaching));
                CONTAINS_STEPS.with(|n| n.set(0));
                assert!(!t.matching_type(&env, &r).unwrap().is_uninhabited());
                let steps = CONTAINS_STEPS.with(|n| n.get());
                let bound = 400 * n * n;
                assert!(
                    steps <= bound,
                    "n={n}: {test} over {reaching} took {steps} steps"
                );
            }
        }
    }

    /// Two families of six names each, unions of errors and arrays of
    /// their own family's names, each union tagged by its family: no
    /// value of one family's names is the other's.
    fn tagged_families() -> [(&'static str, &'static str); 12] {
        [
            ("T0", "[Error<T4>, Array<T2>, Error<T0>, Error<[Error<T3>, T2]>, `A0]"),
            ("T1", "[Error<[T3, T5]>, Error<T5>, Error<Error<T5>>, `A1]"),
            (
                "T2",
                "[Error<T5>, Error<[T5, T3]>, Error<[Error<T1>, T0]>, Error<[T4, T3]>, `A2]",
            ),
            (
                "T3",
                "[Error<[T1, T2]>, Error<T4>, Error<[T5, T0]>, Error<Error<Error<T2>>>, `A3]",
            ),
            ("T4", "[Error<T0>, Error<Error<Error<T1>>>, Error<T4>, `A4]"),
            ("T5", "[Error<Array<T4>>, Array<T3>, `P0(T5), `A5]"),
            ("U0", "[Error<U4>, Array<U2>, Error<U0>, Error<[Error<U3>, U2]>, `B0]"),
            ("U1", "[Error<[U3, U5]>, Error<U5>, Error<Error<U5>>, `B1, Error<U2>]"),
            (
                "U2",
                "[Error<U5>, Error<[U5, U3]>, Error<[Error<U1>, U0]>, Error<[U4, U3]>, `B2]",
            ),
            ("U3", "[Error<U4>, Error<[U5, U0]>, Error<Error<Error<U2>>>, `B3]"),
            ("U4", "[Error<U0>, Error<Error<Error<U1>>>, Error<U4>, `B4, `Z1]"),
            ("U5", "[Error<Array<U4>>, Array<U3>, Array<U2>, `B5]"),
        ]
    }

    // The distribution law pools the parts of a union's candidates, and a
    // pool of parts of the same ids is one node: a question about it met
    // again is its memoized verdict, not a walk over a new union.
    #[test]
    fn a_distribution_pools_the_same_parts_once() {
        let (env, _) = Env::typed_at_root(&tagged_families(), "T0").unwrap();
        for (a, b, bound) in [
            ("[T3, T5]", "Error<U2>", 10_000),
            ("T2", "U2", 9_000),
            ("T1", "Error<U4>", 10_000),
        ] {
            let (a, b) = (at_root(&env, a), at_root(&env, b));
            CONTAINS_STEPS.with(|n| n.set(0));
            assert!(!a.contains(&env, &b).unwrap(), "{a} ⊇ {b}");
            let steps = CONTAINS_STEPS.with(|n| n.get());
            assert!(steps <= bound, "{a} ⊇ {b} took {steps} steps");
        }
    }

    // A select's narrowing contains each member and alternative it pairs
    // through one walk, so a pair of names they share is contained once,
    // whichever of them meets it.
    #[test]
    fn a_narrowing_contains_its_pairs_through_one_walk() {
        let (env, _) = Env::typed_at_root(&tagged_families(), "T0").unwrap();
        let t0 = at_root(&env, "T0");
        for test in ["U1", "Array<U2>", "Error<U1>", "Error<T1>"] {
            let narrowed = at_root(&env, test).matching_type(&env, &t0).unwrap();
            CONTAINS_STEPS.with(|n| n.set(0));
            assert!(t0.narrow_pattern(&env, &narrowed).unwrap(), "{test}");
            let steps = CONTAINS_STEPS.with(|n| n.get());
            assert!(steps <= 15_000, "{test} over T0 took {steps} steps");
        }
        for n in [4, 8, 16] {
            let defs = error_families(n);
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(a, b)| (&**a, &**b)).collect();
            let (env, _) = Env::typed_at_root(&defs, "X0").unwrap();
            for (test, reaching) in [("X0", "Y0"), ("[X1, Y1]", "[X0, Y0]")] {
                let (t, r) = (at_root(&env, test), at_root(&env, reaching));
                let narrowed = t.matching_type(&env, &r).unwrap();
                CONTAINS_STEPS.with(|n| n.set(0));
                assert!(r.narrow_pattern(&env, &narrowed).unwrap());
                let steps = CONTAINS_STEPS.with(|n| n.get());
                let bound = 400 * n * n;
                assert!(
                    steps <= bound,
                    "n={n}: {test} over {reaching} took {steps} steps"
                );
            }
        }
    }

    /// A DAG of named unions, two names a level over both names below:
    /// `L<i> = [L<i-1>, M<i-1>]`, `M<i>` the same, to `L<n>`, over `L0`
    /// and `M0`.
    fn union_dag(n: usize, l0: &str, m0: &str) -> Vec<(String, String)> {
        let mut defs = vec![("L0".into(), l0.into()), ("M0".into(), m0.into())];
        for i in 1..=n {
            let body = format!("[L{}, M{}]", i - 1, i - 1);
            defs.push((format!("L{i}"), body.clone()));
            defs.push((format!("M{i}"), body));
        }
        defs
    }

    // A value refused against a DAG of named unions is refused, and its
    // refusal written, with each name walked a bounded number of times:
    // walked as a tree, `L<n>` is 2^n paths. Over `` [`Keep(i64), null] ``
    // the refusal still finds the value kept bare.
    #[test]
    fn a_refusal_against_a_dag_of_named_unions_walks_each_name_once() {
        let error = ("[null, Error<`E>, `A]", "[null, `B]");
        let keep = ("[`Keep(i64), null]", "[null]");
        for n in [4, 8, 16, 32] {
            for (leaves, leaf, refused) in [
                (error, "string", Some("L{n} does not contain string")),
                (error, "Any", Some("L{n} does not contain Any")),
                (error, "`C", Some("L{n} does not contain `C")),
                (error, "i64", Some("L{n} does not contain i64")),
                (error, "W", Some("L{n} does not contain W")),
                (error, "`B", None),
                (error, "Error<`E>", None),
                (keep, "i64", Some("i64: a [`Keep(T), null] keeps v as `Keep(v)")),
                (keep, "`Keep(i64)", None),
            ] {
                let mut defs = union_dag(n, leaves.0, leaves.1);
                defs.push(("W".into(), "Abstract<i64>".into()));
                let defs: Vec<(&str, &str)> =
                    defs.iter().map(|(a, b)| (&**a, &**b)).collect();
                let (env, l) = Env::typed_at_root(&defs, &format!("L{n}")).unwrap();
                let leaf = at_root(&env, leaf);
                CONTAINS_STEPS.with(|n| n.set(0));
                HEAD_STEPS.with(|n| n.set(0));
                let r = l.check_contains(&env, &leaf);
                match (r, refused) {
                    (Ok(()), None) => (),
                    (Err(e), Some(m)) => {
                        let m = m.replace("{n}", &n.to_string());
                        assert!(e.to_string().ends_with(&m), "n={n}: {e}")
                    }
                    (r, _) => panic!("n={n}: L{n} ⊇ {leaf}: {r:?}"),
                }
                let steps = CONTAINS_STEPS.with(|n| n.get());
                assert!(steps <= 16 * n, "n={n}: L{n} ⊇ {leaf} took {steps} steps");
                let heads = HEAD_STEPS.with(|n| n.get());
                assert!(heads <= 8 * n, "n={n}: L{n} ⊇ {leaf} named {heads} heads");
            }
        }
    }

    // A declared var against a DAG of named unions, 2^n paths deep, is
    // refused in steps linear in n, and held through a conjunct a name
    // below reaches in steps quadratic in n.
    #[test]
    fn a_rigid_var_against_a_dag_of_named_unions_takes_polynomial_steps() {
        for n in [4, 8, 16, 32] {
            let defs = union_dag(n, "[null, Error<`E>, `A]", "[null, `B]");
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(a, b)| (&**a, &**b)).collect();
            let (env, l) = Env::typed_at_root(&defs, &format!("L{n}")).unwrap();
            for (conjunct, bound) in [(None, 16 * n), (Some("`B"), 16 * n * n)] {
                let a = var("a");
                if let Some(c) = conjunct {
                    a.add_cell_constraint(at_root(&env, c));
                }
                let _gate = a.open_rigid();
                CONTAINS_STEPS.with(|n| n.set(0));
                let r = l.check_contains(&env, &Type::TVar(a.clone()));
                match (r, conjunct) {
                    (Ok(()), Some(_)) => (),
                    (Err(e), None) => {
                        let m = format!("L{n} does not contain 'a");
                        assert!(e.to_string().contains(&m), "n={n}: {e}")
                    }
                    (r, _) => panic!("n={n}: L{n} ⊇ {a}: {r:?}"),
                }
                let steps = CONTAINS_STEPS.with(|n| n.get());
                assert!(steps <= bound, "n={n}: L{n} ⊇ {a} took {steps} steps");
            }
        }
    }

    // A union implements a trait when each member does, and each member
    // of a DAG of named unions is asked once: walked as a tree, `L<n>` is
    // 2^n paths, each asking the impl table again.
    #[test]
    fn a_trait_over_a_dag_of_named_unions_asks_each_member_once() {
        for (m0, holds) in [("[string, i64]", true), ("[string, bool]", false)] {
            for n in [4, 8, 16, 32] {
                let defs = union_dag(n, "[i64, string]", m0);
                let defs: Vec<(&str, &str)> =
                    defs.iter().map(|(a, b)| (&**a, &**b)).collect();
                let (mut env, l) = Env::typed_at_root(&defs, &format!("L{n}")).unwrap();
                let (root, pos, ori) = (
                    ModPath::root(),
                    SourcePosition::default(),
                    Arc::new(Default::default()),
                );
                let show = env
                    .deftrait(
                        &root,
                        &literal!("Show"),
                        std::iter::empty(),
                        None,
                        pos,
                        ori,
                    )
                    .unwrap();
                for t in [Typ::I64, Typ::String] {
                    let im = ImplDef {
                        trait_id: show.id,
                        target: prim(t),
                        params: Arc::from_iter([]),
                        scope: root.clone(),
                        methods: Default::default(),
                        declared: false,
                        pos,
                        ori: Arc::new(Default::default()),
                    };
                    env.register_impl(Arc::new(im)).unwrap();
                }
                let mut hist = ContainsHist::new();
                CONTAINS_STEPS.with(|n| n.set(0));
                let r =
                    Type::trait_contains(show.id, BitFlags::empty(), &env, &mut hist, &l);
                assert_eq!(r.unwrap(), holds, "n={n}: L{n} over {m0}");
                let steps = CONTAINS_STEPS.with(|n| n.get());
                assert!(steps <= 16, "n={n}: Show for L{n} took {steps} steps");
            }
        }
    }

    // The probe memo keys a var by the var itself and holds it: a var
    // dropped after its verdict lends its address to no later var, so a
    // later var bound otherwise is judged by its own binding.
    #[test]
    fn a_probe_verdict_on_a_var_is_not_a_later_vars() {
        let defs = union_dag(2, "[null, `A]", "[null, `B]");
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, l) = Env::typed_at_root(&defs, "L2").unwrap();
        let mut hist = ContainsHist::new();
        let probe = BitFlags::from(ContainsFlags::RigidCheck);
        for _ in 0..64 {
            let open = Type::TVar(var("v"));
            assert!(l.contains_int(probe, &env, &mut hist, &open).unwrap());
            drop(open);
            let s = Type::TVar(TVar::named(literal!("w"), prim(Typ::String)));
            assert!(!l.contains_int(probe, &env, &mut hist, &s).unwrap());
        }
    }

    // A verdict on a var is the var's as it stands: a rigid gate opened
    // after an open var's verdict refuses what the open var held, and so
    // does a merge into a cell bound outside the union.
    #[test]
    fn a_probe_verdict_on_a_var_follows_its_cell() {
        let defs = union_dag(2, "[null, `A]", "[null, `B]");
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, l) = Env::typed_at_root(&defs, "L2").unwrap();
        let mut hist = ContainsHist::new();
        let probe = BitFlags::from(ContainsFlags::RigidCheck);
        let a = var("a");
        let t = Type::TVar(a.clone());
        assert!(l.contains_int(probe, &env, &mut hist, &t).unwrap());
        let gate = a.open_rigid();
        assert!(!l.contains_int(probe, &env, &mut hist, &t).unwrap());
        drop(gate);
        assert!(l.contains_int(probe, &env, &mut hist, &t).unwrap());
        a.alias(&TVar::named(literal!("s"), prim(Typ::String)));
        assert!(!l.contains_int(probe, &env, &mut hist, &t).unwrap());
    }

    // What one walk keeps is each pair's verdict wherever it is met: the
    // pairs asked in one walk, in either order, are answered as each is
    // asked alone. `U ⊇ V` holds inside `S ⊇ T`, which assumes itself,
    // and fails alone, as `S ⊇ T` does.
    #[test]
    fn a_kept_probe_verdict_is_the_pairs_own() {
        let mut defs = error_families(3);
        defs.extend(
            [
                ("S", "[Error<U>, `A]"),
                ("T", "[Error<V>, Error<`B>]"),
                ("U", "[Error<S>, `C]"),
                ("V", "[Error<T>, `C]"),
            ]
            .map(|(n, b)| (n.to_string(), b.to_string())),
        );
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, _) = Env::typed_at_root(&defs, "X0").unwrap();
        let mut names: Vec<String> = defs.iter().map(|(n, _)| n.to_string()).collect();
        names.extend(defs.iter().map(|(n, _)| format!("Error<{n}>")));
        names.extend(["[X0, Y1]".into(), "Error<[X2, Y0]>".into(), "`P".into()]);
        let types: Vec<Type> = names.iter().map(|n| at_root(&env, n)).collect();
        let pairs: Vec<(&Type, &Type)> =
            types.iter().flat_map(|a| types.iter().map(move |b| (a, b))).collect();
        let probe = |hist: &mut ContainsHist, (a, b): (&Type, &Type)| {
            a.contains_int(BitFlags::empty(), &env, hist, b).unwrap()
        };
        let alone: Vec<bool> =
            pairs.iter().map(|p| probe(&mut ContainsHist::new(), *p)).collect();
        let mut hist = ContainsHist::new();
        let forward: Vec<bool> = pairs.iter().map(|p| probe(&mut hist, *p)).collect();
        let mut hist = ContainsHist::new();
        let mut backward: Vec<bool> =
            pairs.iter().rev().map(|p| probe(&mut hist, *p)).collect();
        backward.reverse();
        for (i, (a, b)) in pairs.iter().enumerate() {
            assert_eq!(alone[i], forward[i], "{a} ⊇ {b}");
            assert_eq!(alone[i], backward[i], "{a} ⊇ {b}");
        }
    }

    /// `D<'a0, ..> = [`N, `X(fn(x: 'a0) -> i64, D<'a1, 'a0, ..>), `Y(D<'a1, ..,
    /// 'a0>)]` over `n` parameters: a swap and a rotation, so its
    /// applications over `n` cells are their `n!` orders, each reaching
    /// every other.
    fn permuted(name: &str, n: usize) -> (String, String) {
        let ps = |order: &mut dyn Iterator<Item = usize>| {
            order.map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ")
        };
        let swap = ps(&mut [1, 0].into_iter().chain(2..n));
        let rotate = ps(&mut (1..n).chain([0]));
        let head = format!("{name}<{}>", ps(&mut (0..n)));
        let body =
            format!("[`N, `X(fn(x: 'a0) -> i64, {name}<{swap}>), `Y({name}<{rotate}>)]");
        (head, body)
    }

    /// The steps `fn(k: H<Any, ..>, z: i64) -> i64` takes to hold `fn(k:
    /// D<'q0, ..>, z: 'q0) -> 'q0` over [`permuted`] definitions of `n`
    /// parameters, `H` being `held` (`D` itself or its twin `F`), and of a
    /// probe of `D<'q0, ..>` holding `H<Any, ..>`: the cells stay open, so
    /// every order is an application of its own.
    fn permuted_steps(held: &str, n: usize) -> (usize, usize) {
        let defs = [permuted("D", n), permuted("F", n)];
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, _) = Env::typed_at_root(&defs, "i64").unwrap();
        let cells: Vec<String> = (0..n).map(|i| format!("'q{i}")).collect();
        let (cells, anys) = (cells.join(", "), vec!["Any"; n].join(", "));
        let open = at_root(&env, &format!("fn(k: D<{cells}>, z: 'q0) -> 'q0"));
        let held = at_root(&env, &format!("fn(k: {held}<{anys}>, z: i64) -> i64"));
        let (Type::Fn(o), Type::Fn(h)) = (&open, &held) else { unreachable!() };
        let (d, f) = (&o.args[0].typ, &h.args[0].typ);
        CONTAINS_STEPS.with(|n| n.set(0));
        let mut hist = ContainsHist::new();
        assert!(d.contains_int(BitFlags::empty(), &env, &mut hist, f).unwrap());
        let probe = CONTAINS_STEPS.with(|n| n.replace(0));
        held.check_contains(&env, &open).unwrap();
        (CONTAINS_STEPS.with(|n| n.get()), probe)
    }

    // An application answered inside a pair still in progress is kept
    // while that pair stands, so a sibling meeting it does not walk it
    // again: a probe walks each of the `n!` pairs once, a commit twice,
    // in the trial of the outermost union with two open members and in
    // the placement that trial allows. A walk that forgets an answered
    // pair takes every path through them, and a trial at every such
    // union walks the pairs once per pair.
    #[test]
    fn open_applications_of_a_recursive_definition_are_walked_once() {
        for n in [3, 4, 5] {
            let apps: usize = (1..=n).product();
            let (commit, probe) = permuted_steps("F", n);
            assert!(probe <= 64 * apps, "n={n}: a probe took {probe} steps");
            assert!(commit <= 128 * apps, "n={n}: a commit took {commit} steps");
        }
    }

    // A verdict reached inside pairs in progress is looked at when the
    // outermost pair it rests on is answered, not at the answer of each
    // pair between: the applications of a permuting definition reach
    // each other, so every verdict of the walk rests on the first pair
    // until the walk's end.
    #[test]
    fn a_held_verdict_is_answered_once() {
        for n in [4, 5] {
            let apps: usize = (1..=n).product();
            VERDICTS_ANSWERED.with(|n| n.set(0));
            let (commit, _) = permuted_steps("F", n);
            let answered = VERDICTS_ANSWERED.with(|n| n.get());
            assert!(answered <= commit, "n={n}: {answered} looks at held verdicts");
            assert!(answered <= 64 * apps, "n={n}: {answered} looks at held verdicts");
        }
    }

    // A union member that takes a type as it stands is walked with every
    // cell held rigid, and the verdicts reached under that hold are kept
    // while it lasts: two members of one head over a recursive definition
    // are a choice at every application, and a walk that keeps nothing
    // there takes every path through the applications.
    #[test]
    fn a_walk_over_held_cells_keeps_its_verdicts() {
        let body = "[`Y(D<'a0, 'a1, null>), `Y(D<'a1, 'a2, 'a0>), \
                    `U(fn(x0: 'a0, x1: 'a1, x2: 'a2) -> i64)]";
        let (env, _) = Env::typed_at_root(&[("D<'a0, 'a1, 'a2>", body)], "i64").unwrap();
        let open = at_root(&env, "fn(k: D<'q0, 'q1, 'q2>) -> i64");
        let held = at_root(&env, "fn(k: D<Any, Any, Any>) -> i64");
        CONTAINS_STEPS.with(|n| n.set(0));
        held.check_contains(&env, &open).unwrap();
        let steps = CONTAINS_STEPS.with(|n| n.get());
        assert!(steps <= 40_000, "a commit took {steps} steps");
    }

    // A member takes a type as it stands in a walk with every cell held
    // rigid, where two members of one head over a recursive definition
    // are a choice at every application: a commit there can decide no
    // cell, so the probe that admitted a member answers, and a walk on
    // copies of each member walks the applications below at every choice.
    #[test]
    fn a_choice_no_commit_can_decide_walks_no_copies() {
        let cases = [
            (
                "[Error<D<'a0, 'a1, null>>, Error<D<'a1, 'a0, 'a2>>, `Y(D<'a1, 'a2, 'a0>)]",
                "fn(z: 'q0, k: D<'q0, 'q1, 'q2>) -> 'q0",
                "fn(z: Any, k: F<Any, i64, Any>) -> Any",
                400_000,
            ),
            (
                "[`Y(D<null, 'a1, 'a2>), Error<D<'a1, 'a0, 'a2>>, `Y(D<'a1, 'a2, 'a0>)]",
                "fn(k: (D<'q0, 'q1, 'q2>, D<'q1, 'q1, 'q2>), z: 'q0) -> 'q0",
                "fn(k: (F<i64, i64, i64>, F<Any, i64, Any>), z: string) -> string",
                150_000,
            ),
        ];
        for (body, open, held, most) in cases {
            let twin = body.replace("D<", "F<");
            let defs = [("D<'a0, 'a1, 'a2>", body), ("F<'a0, 'a1, 'a2>", &*twin)];
            let (env, _) = Env::typed_at_root(&defs, "i64").unwrap();
            let open = at_root(&env, open);
            let held = at_root(&env, held);
            CONTAINS_STEPS.with(|n| n.set(0));
            held.check_contains(&env, &open).unwrap();
            let steps = CONTAINS_STEPS.with(|n| n.get());
            assert!(steps <= most, "{body}: a commit took {steps} steps");
        }
    }

    // Two decided members of one head over a recursive definition that both
    // take a type with a cell to decide are a choice at every application:
    // the walk on copies that admits each also reports whether the other
    // still holds the type, so no second walk per pair asks it again.
    #[test]
    fn a_choice_between_decided_members_walks_each_member_once() {
        let body =
            "[Error<D<'a0, string, 'a2>>, Error<D<'a1, 'a0, 'a2>>, `Y(D<'a1, 'a2, 'a0>)]";
        let twin = body.replace("D<", "F<");
        let defs = [("D<'a0, 'a1, 'a2>", body), ("F<'a0, 'a1, 'a2>", &*twin)];
        let (env, _) = Env::typed_at_root(&defs, "i64").unwrap();
        let open = at_root(&env, "fn(k: D<'q0, 'q0, 'q1>) -> D<'q0, 'q1, 'q2>");
        let held = at_root(&env, "fn(k: F<Any, i64, i64>) -> F<i64, i64, Any>");
        CONTAINS_STEPS.with(|n| n.set(0));
        held.check_contains(&env, &open).unwrap();
        let steps = CONTAINS_STEPS.with(|n| n.get());
        assert!(steps <= 9_000_000, "a commit took {steps} steps");
    }

    // A function type whose quantifier an earlier task left open takes it
    // fresh for each walk, so a commit decides the copy though the cell is
    // held rigid: the probe admits the identity into `fn(x: string) ->
    // i64` part by part, and the commit refuses it there, so the choice
    // walks each member and the identity goes to the one that holds it.
    #[test]
    fn a_commit_decides_a_quantifier_an_earlier_task_left_open() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        for (s, t) in [
            (
                "[fn(x: string) -> i64, fn(x: string) -> string]",
                "fn<'c: Any>(x: 'c) -> 'c",
            ),
            (
                "[`A(fn(x: string) -> i64), `A(fn(x: string) -> string)]",
                "`A(fn<'c: Any>(x: 'c) -> 'c)",
            ),
        ] {
            let (s, t) = (at_root(&env, s), at_root(&env, t));
            let mut held = vec![];
            hold_rigid(&t, &mut held);
            let _later = InTask::enter(new_task());
            assert!(s.contains(&env, &t).unwrap(), "{s} ⊇ {t}");
        }
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

    // A member whose cell an earlier part of the walk bound is decided when
    // its union is met: it takes a decided type and binds nothing, wherever
    // it stands among the members, in a walk on copies as in the check. A
    // walk on copies that gave `Array<i64>` to the first member admitted
    // would decide 'a there, and `Array<'a>` would hold no `Array<string>`:
    // the tag would have one taker in one order of the members, and two in
    // the other.
    #[test]
    fn a_member_decided_since_the_walk_met_it_takes_a_decided_type() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let cases = [
            ("[Array<'a>, Array<'b>]", "Array<i64>"),
            ("[Array<'b>, Array<'a>]", "Array<i64>"),
            ("[Array<'a>, Array<'b>, 'c]", "[Array<i64>, string]"),
            ("[Array<'b>, Array<'a>, 'c]", "[Array<i64>, string]"),
        ];
        for (union, part) in cases {
            let held = at_root(&env, &format!("('b, {union}, Array<'a>)"));
            let t = at_root(&env, &format!("([i64, null], {part}, Array<string>)"));
            let mut trial = ContainsHist::trial();
            let flags = ContainsFlags::Commit.into();
            assert!(held.contains_int(flags, &env, &mut trial, &t).unwrap(), "{union}");
            let held =
                at_root(&env, &format!("[`A('b, {union}, Array<'a>), `A('d, 'e, 'f)]"));
            let t = at_root(&env, &format!("`A([i64, null], {part}, Array<string>)"));
            let e = held.check_contains(&env, &t).unwrap_err();
            assert!(e.is::<AmbiguousMember>(), "{union}: {e:#}");
        }
    }

    // Which member of a union an undecided type goes to, on copies, follows
    // the members' place: the union written one way meets a pair with a
    // type mismatch where the other holds. The hold is sound (it stands
    // when asked again, and `'z` took the one member that held its type).
    #[test]
    fn an_undecided_type_met_by_a_trial_goes_to_the_first_member_that_holds_it() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let held = |union: &str| {
            at_root(
                &env,
                &format!(
                    "[`X('b, 'c, {union}, Array<'a>, [i64, null]), `Y(Array<Array<Array<'c>>>)]"
                ),
            )
        };
        let t = at_root(
            &env,
            "[`X([i64, null], i64, Array<'z>, Array<string>, 'z), `Y(Array<Array<Array<[i64, string]>>>)]",
        );
        let e = held("[Array<'a>, Array<'b>]").check_contains(&env, &t).unwrap_err();
        assert!(
            !e.is::<AmbiguousMember>() && format!("{e:#}").contains("type mismatch"),
            "{e:#}"
        );
        let held = held("[Array<'b>, Array<'a>]");
        held.check_contains(&env, &t).unwrap();
        held.check_contains(&env, &t).unwrap();
        let Type::Set(ms) = &held else { panic!("no set") };
        let Type::Variant(_, xs, _) = &ms[0] else { panic!("no variant") };
        assert_eq!(
            crate::format_with_flags(crate::PrintFlag::DerefTVars, || xs[2].to_string()),
            "[Array<'b: [i64, null]>, Array<'a: string>]"
        );
    }

    // The unions a trial walks before it meets a choice between members
    // are taken as written by the placement it allows too, which asks no
    // trial of its own for them: each union ahead of the choice adds the
    // steps of one walk, not of a walk and a trial.
    #[test]
    fn a_union_ahead_of_a_choice_between_members_asks_no_trial_of_its_own() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let steps = |n: usize| {
            let ahead = |part: &str| -> String {
                (0..n).map(|k| part.replace('k', &k.to_string()) + ", ").collect()
            };
            let held = at_root(
                &env,
                &format!(
                    "[`X({}[Array<'a>, Array<Array<['c, i64, Array<i64>]>>], \
                     [Array<'a>, `T('a)]), `Y('d)]",
                    ahead("[i64, `T('pk), Array<Array<'pk>>]")
                ),
            );
            let t = at_root(
                &env,
                &format!(
                    "[`X({}Array<Array<[i64, Array<i64>]>>, \
                     [Array<Array<i64>>, `T(Array<[i64, Array<i64>]>)]), \
                     `Y(Array<Array<Array<Array<Array<Array<Array<i64>>>>>>>)]",
                    ahead("[i64, Array<Array<i64>>]")
                ),
            );
            CONTAINS_STEPS.with(|n| n.set(0));
            held.check_contains(&env, &t).unwrap();
            CONTAINS_STEPS.with(|n| n.get())
        };
        let none = steps(0);
        for n in 1..=4 {
            let steps = steps(n);
            assert!(
                steps <= none + 52 * n,
                "n={n}: {steps} steps against {none} with none"
            );
        }
    }

    // `closed_now` reads the cells: a verdict `closed` reached at the same
    // write count through one it kept from before the cell was bound is
    // not its answer.
    #[test]
    fn closed_now_reads_the_cells_after_closed_answered_from_a_kept_verdict() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let held = at_root(&env, "('a, Array<'a>)");
        let Type::Tuple(parts) = &held else { unreachable!("a tuple") };
        let mut hist = ContainsHist::new();
        assert!(!hist.closed(&parts[0]));
        parts[0].check_contains(&env, &at_root(&env, "i64")).unwrap();
        assert!(!hist.closed(&parts[1]));
        assert!(hist.closed_now(&parts[1]));
        assert!(hist.binds_nothing(&parts[1], &at_root(&env, "Array<i64>")));
    }

    // Two applications of one definition are compared by their arguments,
    // each as the body meets its parameter, in steps by the parameters:
    // the `n!` applications their expansions reach are not walked. `'q0`
    // is left for `z` to decide, as the twin definition leaves it.
    #[test]
    fn applications_of_one_definition_compare_by_their_arguments() {
        for n in [3, 4, 6, 12] {
            let (commit, probe) = permuted_steps("D", n);
            assert!(probe <= 2 * n, "n={n}: a probe took {probe} steps");
            assert!(commit <= 6 * n, "n={n}: a commit took {commit} steps");
        }
    }

    // A recursive definition with a bare parameter places its expansion's
    // members on copies, where its own recursion comes back over fresh
    // cells: the trial starts with the pairs its walk is expanding in
    // progress, so it meets them by assumption, where expanding each
    // would place its members on copies again without end. One
    // definition, or two with one body.
    #[test]
    fn a_trial_meets_the_recursion_it_is_inside_by_assumption() {
        let tree = "['a, `Node(Array<Tree<'a>>)]";
        let twin = "['a, `Node(Array<Twin<'a>>)]";
        let cases: [(&[(&str, &str)], &str, &str); 4] = [
            (&[("Tree<'a>", tree)], "Tree<'q>", "Tree<i64>"),
            (&[("Tree<'a>", tree), ("Twin<'a>", twin)], "Tree<'q>", "Twin<i64>"),
            (
                &[("D<'a, 'b>", "[`U(D<'b, 'a>), 'b, `V(Array<'a>)]")],
                "D<'q, 'r>",
                "D<i64, string>",
            ),
            (
                &[("D<'a>", "[`U(Array<D<'a>>), `V(i64), 'a, null]")],
                "D<['q, null]>",
                "D<string>",
            ),
        ];
        for (defs, open, held) in cases {
            let (env, _) = Env::typed_at_root(defs, "i64").unwrap();
            let open = at_root(&env, &format!("fn(x: {open}) -> i64"));
            let held = at_root(&env, &format!("fn(x: {held}) -> i64"));
            CONTAINS_STEPS.with(|n| n.set(0));
            held.check_contains(&env, &open).unwrap();
            let steps = CONTAINS_STEPS.with(|n| n.get());
            assert!(steps <= 200, "{open}: a commit took {steps} steps");
        }
    }

    // A commit that writes a cell outdates the probes settled before it. A
    // pair settled then and answered since, under an assumption still in
    // progress, is read from the verdicts kept while that stands: two
    // definitions reaching each other, 12 applications over three cells
    // against 2, take steps by their pairs, not by the paths through them.
    #[test]
    fn a_kept_verdict_is_read_past_an_outdated_one() {
        let (swap, rotate) = ("'a1, 'a0, 'a2", "'a1, 'a2, 'a0");
        let defs: Vec<(String, String)> = ["D", "F"]
            .iter()
            .flat_map(|n| {
                [
                    format!(
                        "[Array<{n}0<{swap}>>, `X({n}0<{swap}>, 'a0), `Y({n}1<{rotate}>)]"
                    ),
                    format!(
                        "[Error<{n}0<{swap}>>, `X(fn(x: {n}0<{swap}>) -> 'a0), \
                         `Y({n}1<{rotate}>)]"
                    ),
                ]
                .into_iter()
                .enumerate()
                .map(move |(i, body)| (format!("{n}{i}<'a0, 'a1, 'a2>"), body))
            })
            .collect();
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, _) = Env::typed_at_root(&defs, "i64").unwrap();
        let open = at_root(&env, "fn(k: D0<'q0, 'q1, 'q2>) -> i64");
        let held = at_root(&env, "fn(k: F0<Any, Any, Any>) -> i64");
        CONTAINS_STEPS.with(|n| n.set(0));
        let writes = cell_writes();
        held.check_contains(&env, &open).unwrap();
        let (steps, writes) = (CONTAINS_STEPS.with(|n| n.get()), cell_writes() - writes);
        assert!(writes > 1, "the walk wrote {writes} cells");
        assert!(steps <= 1000 * 12 * 2, "the walk took {steps} steps");
    }

    // What one walk keeps of applications over open cells is each pair's
    // verdict wherever it is met, probed or committed: the pairs asked in
    // one walk, in either order, are answered as each is asked alone. The
    // cells are declared, so no commit binds one. `U<'c> ⊇ V<'c>` holds
    // inside `S<'c> ⊇ T<'c>`, which assumes itself, and fails alone, as
    // `S<'c> ⊇ T<'c>` does.
    #[test]
    fn a_kept_verdict_over_open_applications_is_the_pairs_own() {
        let mut defs: Vec<(String, String)> = vec![];
        for (p, tag) in [("X", "`P"), ("Y", "`Q")] {
            for i in 0..3 {
                let (a, b, c) = ((i + 1) % 3, (i * 2) % 3, (i + 2) % 3);
                let body = format!(
                    "[Error<{p}{a}<'a>>, Error<{p}{b}<'a>>, Error<Error<{p}{c}<'a>>>, \
                     {tag}('a)]"
                );
                defs.push((format!("{p}{i}<'a>"), body));
            }
        }
        defs.extend(
            [
                ("S<'a>", "[Error<U<'a>>, `A('a)]"),
                ("T<'a>", "[Error<V<'a>>, Error<`B>]"),
                ("U<'a>", "[Error<S<'a>>, `C]"),
                ("V<'a>", "[Error<T<'a>>, `C]"),
                ("G<'a, 'b>", "[Error<G<'b, 'a>>, Error<H<'a, 'b>>, `P('a)]"),
                ("H<'a, 'b>", "[Error<H<'b, 'a>>, `P('b)]"),
            ]
            .map(|(n, b)| (n.to_string(), b.to_string())),
        );
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, _) = Env::typed_at_root(&defs, "i64").unwrap();
        let mut names: Vec<String> = vec![];
        for (n, _) in defs.iter() {
            let n = n.split_once('<').unwrap().0;
            match n {
                "G" | "H" => names.extend(
                    ["'c, 'd", "'d, 'c", "'c, Any", "Any, 'c", "Any, Any"]
                        .map(|ps| format!("{n}<{ps}>")),
                ),
                n => names.extend(["'c", "'d", "Any"].map(|p| format!("{n}<{p}>"))),
            }
        }
        names.extend(["Error<[X2<'c>, Y0<Any>]>".into(), "`P('c)".into()]);
        asked_together_as_alone(&env, &names);
    }

    /// Every pair of `names` asked of one walk, in either order, is
    /// answered as it is asked alone, probed, and committed over declared
    /// cells, which no commit binds; some hold and some do not.
    fn asked_together_as_alone(env: &Env, names: &[String]) {
        let all = at_root(env, &format!("({})", names.join(", ")));
        let Type::Tuple(types) = &all else { unreachable!() };
        let mut cells = AHashMap::default();
        all.collect_tvars(&mut cells);
        let pairs: Vec<(&Type, &Type)> =
            types.iter().flat_map(|a| types.iter().map(move |b| (a, b))).collect();
        for (flags, declared) in [
            (BitFlags::empty(), false),
            (ContainsFlags::RigidCheck.into(), false),
            (ContainsFlags::RigidCheck.into(), true),
            (ContainsFlags::Commit.into(), true),
        ] {
            let _gates: Vec<RigidGate> = match declared {
                true => cells.values().map(|c| c.open_rigid()).collect(),
                false => vec![],
            };
            let ask = |hist: &mut ContainsHist, (a, b): (&Type, &Type)| {
                a.contains_int(flags, env, hist, b).unwrap()
            };
            let alone: Vec<bool> =
                pairs.iter().map(|p| ask(&mut ContainsHist::new(), *p)).collect();
            let mut hist = ContainsHist::new();
            let forward: Vec<bool> = pairs.iter().map(|p| ask(&mut hist, *p)).collect();
            let mut hist = ContainsHist::new();
            let mut backward: Vec<bool> =
                pairs.iter().rev().map(|p| ask(&mut hist, *p)).collect();
            backward.reverse();
            for (i, (a, b)) in pairs.iter().enumerate() {
                assert_eq!(alone[i], forward[i], "{flags:?}: {a} ⊇ {b}");
                assert_eq!(alone[i], backward[i], "{flags:?}: {a} ⊇ {b}");
            }
            assert!(alone.iter().any(|r| *r) && alone.iter().any(|r| !*r));
            assert!(cells.values().all(|c| !c.is_bound()));
        }
    }

    // A verdict that read a kept one rests on what that one rests on, and a
    // kept one whose own assumption is answered still rests on those
    // further out. `S ⊇ T` fails at its last member (`ZZ) or its first
    // (`AA), whichever order a union's members are walked in. In J, `U ⊇ V`
    // holds assuming it, and `W0 ⊇ Z0` and `W2 ⊇ Z2`, on either side of
    // it, hold by `U ⊇ V`: the later one reads it kept, and no union
    // between them probes it first. In L, `W ⊇ Z` holds assuming `S ⊇ T`
    // and is kept inside `U ⊇ V`, answered as assumed before `S ⊇ T`
    // fails. None holds alone.
    #[test]
    fn a_kept_verdict_rests_on_what_it_read() {
        let mut defs: Vec<(String, String)> = vec![];
        for (f, fail) in [("A", "`AA"), ("Z", "`ZZ")] {
            let d = |name: &str, body: String| (format!("{name}{f}<'a>"), body);
            let side = |s: &str, u: &str, w: &str, fail: &str| {
                [
                    d(
                        &format!("J{s}"),
                        format!(
                            "[`K0(J{w}0{f}<'a>), `K1(J{u}{f}<'a>), `K2(J{w}2{f}<'a>), \
                             `P('a){fail}]"
                        ),
                    ),
                    d(&format!("J{u}"), format!("[`E(J{s}{f}<'a>), `C]")),
                    d(&format!("J{w}0"), format!("`R(J{u}{f}<'a>)")),
                    d(&format!("J{w}2"), format!("`R(J{u}{f}<'a>)")),
                    d(&format!("L{s}"), format!("[`K(L{u}{f}<'a>), `P('a){fail}]")),
                    d(&format!("L{u}"), format!("[`E(L{w}{f}<'a>), `C]")),
                    d(&format!("L{w}"), format!("[`R(L{s}{f}<'a>), `C]")),
                ]
            };
            defs.extend(side("S", "U", "W", ""));
            defs.extend(side("T", "V", "Z", &format!(", {fail}")));
        }
        let names: Vec<String> = defs
            .iter()
            .flat_map(|(n, _)| ["'c", "Any"].map(|p| n.replace("'a", p)))
            .chain(["`P('c)".to_string()])
            .collect();
        let defs: Vec<(&str, &str)> = defs.iter().map(|(a, b)| (&**a, &**b)).collect();
        let (env, _) = Env::typed_at_root(&defs, "i64").unwrap();
        asked_together_as_alone(&env, &names);
    }

    // An application's key reads its cells' addresses, so the walk holds
    // the cells: one a merge leaves no var reading keeps its address.
    #[test]
    fn an_application_key_holds_its_cells() {
        let defs = [("L<'a>", "['a, null]")];
        let (env, apps) = Env::typed_at_root(&defs, "(L<'c>, L<'d>)").unwrap();
        let mut cells = AHashMap::default();
        apps.collect_tvars(&mut cells);
        let (c, d): (TVar, TVar) = (cells["c"].clone(), cells["d"].clone());
        let before = [c.cell(), d.cell()];
        let Type::Tuple(apps) = &apps else { unreachable!() };
        let mut hist = ContainsHist::new();
        assert_ne!(hist.pair_key(&apps[0]).unwrap(), hist.pair_key(&apps[1]).unwrap());
        Type::TVar(c.clone()).check_contains(&env, &Type::TVar(d.clone())).unwrap();
        assert!(c.same_cell(&d));
        let merged = c.cell();
        let absorbed = before.iter().find(|b| !Arc::ptr_eq(b, &merged)).unwrap();
        assert_eq!(Arc::count(absorbed), 2, "held here and by the walk");
    }

    // An application keyed under another by its id holds its own key's
    // cells: a chain nested `n` deep holds its cell once, not `n` times.
    #[test]
    fn a_nested_application_key_holds_its_cells_once() {
        let defs = [("L<'a>", "['a, null]")];
        let nest = (0..40).fold(String::from("'c"), |t, _| format!("L<{t}>"));
        let (_env, app) = Env::typed_at_root(&defs, &nest).unwrap();
        let mut hist = ContainsHist::new();
        hist.pair_key(&app).unwrap();
        assert_eq!(hist.app_cells.get().map(|c| c.len()), Some(1));
    }

    /// `c` and `d`, two open vars, merged after `met` saw each: the cell
    /// the merge left no var reading.
    fn absorbed_after(mut met: impl FnMut(&Type)) -> Arc<CellLock> {
        let [c, d] = ["c", "d"].map(|n| TVar::empty_named(n.into()));
        let before = [c.cell(), d.cell()];
        met(&Type::TVar(c.clone()));
        met(&Type::TVar(d.clone()));
        Type::TVar(c.clone()).check_contains(&Env::default(), &Type::TVar(d)).unwrap();
        let merged = c.cell();
        let [x, y] = before;
        if Arc::ptr_eq(&x, &merged) { y } else { x }
    }

    // A closure verdict's key reads its var's cell's address, so the walk
    // holds the cell: one a merge leaves no var reading keeps its address.
    #[test]
    fn a_closure_key_holds_its_cell() {
        let mut hist = ContainsHist::new();
        let absorbed = absorbed_after(|t| assert!(!hist.closed(t)));
        assert_eq!(Arc::count(&absorbed), 2, "held here and by the walk");
    }

    // And so an open var's id, and an abstract type's, which reads its
    // parameters' address: a type the walk met and its caller dropped
    // lends its id to none made after it.
    #[test]
    fn a_ref_id_holds_what_its_key_reads() {
        let env = Env::default();
        let mut hist = ContainsHist::new();
        let absorbed = absorbed_after(|t| {
            hist.ref_id(t, &env).unwrap();
        });
        assert_eq!(Arc::count(&absorbed), 2, "held here and by the walk");
        let abstract_over = |p: Typ| Type::Abstract {
            id: AbstractId::of(&ModPath::root(), "A"),
            params: Arc::from_iter([Type::Primitive(p.into())]),
        };
        let first = hist.ref_id(&abstract_over(Typ::I64), &env);
        let later: Vec<Type> = (0..64).map(|_| abstract_over(Typ::String)).collect();
        assert!(later.iter().all(|t| hist.ref_id(t, &env) != first));
    }

    /// `T0..T3` and `U0..U3`, each `[Error<next>, Error<next but one>, tag]`
    /// over its own four names: a DAG of named error unions.
    fn error_rings() -> (Env, Vec<Type>, Vec<Type>) {
        let mut defs = vec![];
        for (name, tag) in [("T", "A"), ("U", "B")] {
            for i in 0..4 {
                let (next, after) = ((i + 1) % 4, (i + 2) % 4);
                defs.push((
                    format!("{name}{i}"),
                    format!("[Error<{name}{next}>, Error<{name}{after}>, `{tag}{i}]"),
                ));
            }
        }
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, _) = Env::typed_at_root(&defs, "null").unwrap();
        let named = |name: &str| -> Vec<Type> {
            let root = ModPath::root();
            (0..4).map(|i| parsed(&format!("{name}{i}")).scope_refs(&root)).collect()
        };
        (env, named("T"), named("U"))
    }

    // What `T0` leaves after a test of `U0` is a DAG of unions over the
    // names, and whether it holds `U1` asks a distribution at each of its
    // error members: the walk normalizes each node the pools share once,
    // not once a question.
    #[test]
    fn a_walk_normalizes_the_nodes_its_pools_share_once() {
        let (env, ts, us) = error_rings();
        let left = ts[0].diff(&env, &us[0]).unwrap();
        NORMALIZED.with(|n| n.set(0));
        CONTAINS_STEPS.with(|n| n.set(0));
        assert!(!left.contains_with_flags(BitFlags::empty(), &env, &us[1]).unwrap());
        let (normalized, steps) =
            (NORMALIZED.with(|n| n.get()), CONTAINS_STEPS.with(|n| n.get()));
        assert!(normalized <= steps, "{normalized} nodes normalized in {steps} steps");
    }

    // A member that holds a cell is normalized as the cell stands at each
    // question: the walk's pass keeps nothing of it.
    #[test]
    fn a_union_over_a_cell_is_normalized_at_each_question() {
        let a = TVar::empty_named(literal!("a"));
        let over_cell =
            Type::Set(Arc::from_iter([Type::TVar(a.clone()), parsed("string")]));
        let members = [Type::Error(Arc::new(over_cell)), parsed("Error<[i64, string]>")];
        let mut hist = ContainsHist::new();
        let open = hist.normal_union(members.iter());
        assert!(matches!(&open, Type::Set(s) if s.len() == 2), "{open}");
        a.bind(Type::Primitive(Typ::I64.into()));
        let bound = hist.normal_union(members.iter());
        assert!(matches!(&bound, Type::Error(_)), "{bound}");
    }

    /// `fn<'a0: P<'a1, 'a2>, ..>(x: ('a0, 'a1)) -> i64` over `P<'x, 'y> =
    /// [Array<'x>, Array<'y>, null]`, `n` links to `'an: Number`: an
    /// instance's own copy of the definition, and the instance its bounds
    /// decide, each link's type one node.
    fn bound_dag(n: usize) -> (Env, FnType, FnType) {
        let bound = |i: usize, var: &str| match n - i {
            0 => String::from("Number"),
            1 => format!("[Array<'{var}{n}>, null]"),
            _ => format!("P<'{var}{}, '{var}{}>", i + 1, i + 2),
        };
        let quantifiers: Vec<String> =
            (0..=n).map(|i| format!("'a{i}: {}", bound(i, "a"))).collect();
        let def = format!("fn<{}>(x: ('a0, 'a1)) -> i64", quantifiers.join(", "));
        let defs =
            [("Number", "[i64, f64]"), ("P<'x, 'y>", "[Array<'x>, Array<'y>, null]")];
        let (env, def) = Env::typed_at_root(&defs, &def).unwrap();
        def.alias_tvars(&mut LPooled::take());
        let Type::Fn(def) = &def else { unreachable!() };
        let def = def.instantiate_owned(&mut AHashMap::default());
        let mut decided: AHashMap<ArcStr, Type> = AHashMap::default();
        for i in (0..=n).rev() {
            let w = at_root(&env, &bound(i, "w")).replace_tvars(&decided);
            decided.insert(ArcStr::from(format!("w{i}")), w);
        }
        let Type::Fn(instance) = &at_root(&env, "fn(x: ('w0, 'w1)) -> i64") else {
            unreachable!()
        };
        (env, def, instance.replace_tvars(&decided))
    }

    // An instance's signature holds its definition's over a DAG of bounds
    // through a union typedef in polynomial steps: a probe guiding a
    // commit under `Lower` is kept as any other is (walked at every
    // meeting, the steps triple a link), and a verdict over two types
    // that hold no cell stands through the binds between.
    #[test]
    fn an_instance_over_a_dag_of_bounds_through_a_typedef_takes_polynomial_steps() {
        for n in [4, 12, 32] {
            let (env, def, instance) = bound_dag(n);
            CONTAINS_STEPS.with(|n| n.set(0));
            instance.check_contains(&env, &def).unwrap();
            let steps = CONTAINS_STEPS.with(|n| n.get());
            assert!(steps <= 320 * n * n, "n={n}: {steps} steps");
        }
    }

    // The depth a union's members are ordered by measures each shared
    // node of a member once: as a tree, the links below one are Fibonacci
    // many.
    #[test]
    fn a_members_depth_measures_each_shared_node_once() {
        let n = 32;
        let (_, _, instance) = bound_dag(n);
        NESTING_STEPS.with(|n| n.set(0));
        let depth = nesting(&instance.args[0].typ, &mut LPooled::take());
        let measured = NESTING_STEPS.with(|n| n.get());
        assert!(depth > n, "depth {depth}");
        assert!(measured <= 8 * n, "{measured} nodes measured");
    }

    // A verdict under `Lower` is the pair's under `Lower` alone, in the
    // polarity it was reached in: an open cell above a type its lower
    // bound refuses takes it as a bound (`Lower`), and refuses the bind.
    #[test]
    fn a_probe_verdict_is_kept_under_its_flags() {
        let (env, _) = Env::typed_at_root(&[], "i64").unwrap();
        let a = Type::TVar(var("a"));
        let Type::TVar(tv) = &a else { unreachable!() };
        tv.add_lower(prim(Typ::String));
        let int = Type::Array(Arc::new(prim(Typ::I64)));
        let mut hist = ContainsHist::new();
        let lower = ContainsFlags::Lower | ContainsFlags::RigidCheck;
        let plain = BitFlags::from(ContainsFlags::RigidCheck);
        for _ in 0..2 {
            assert!(a.contains_int(lower, &env, &mut hist, &int).unwrap());
            assert!(!a.contains_int(plain, &env, &mut hist, &int).unwrap());
            assert!(
                !a.contains_int(lower | ContainsFlags::Contra, &env, &mut hist, &int)
                    .unwrap()
            );
        }
    }

    // A verdict under `Lower` over an open var and a type that holds no
    // cell stands by the var's place as a plain one does: through a
    // write to a cell the var does not reach, and no further than a
    // write to one it reaches. `'c0: Array<'c1>` and on, links enough to
    // be numbered, above as many arrays of `i64`.
    #[test]
    fn a_flagged_verdict_stands_while_no_cell_its_var_reaches_is_written() {
        const LINKS: usize = 40;
        let env = Env::default();
        let array = |t: Type| Type::Array(Arc::new(t));
        let cells: Vec<TVar> = (0..=LINKS).map(|_| var("c")).collect();
        for link in cells.windows(2) {
            link[0].add_cell_constraint(array(Type::TVar(link[1].clone())));
        }
        let top = Type::TVar(cells[0].clone());
        let arrays = (0..LINKS).fold(prim(Typ::I64), |t, _| array(t));
        let lower = ContainsFlags::Lower | ContainsFlags::RigidCheck;
        let mut hist = ContainsHist::new();
        let mut probe = || {
            CONTAINS_STEPS.with(|n| n.set(0));
            let held = top.contains_int(lower, &env, &mut hist, &arrays).unwrap();
            (held, CONTAINS_STEPS.with(|n| n.get()))
        };
        for _ in 0..2 {
            let (held, steps) = probe();
            assert!(held && steps > 1, "{steps} steps");
            var("other").bind(prim(Typ::String));
        }
        assert_eq!(probe(), (true, 1));
        cells[LINKS].bind(prim(Typ::String));
        assert!(!probe().0);
    }

    // A trait question an impl with parameters answers reads a copy of
    // the impl's head, whose cells no walk writes, so the verdict on a var
    // stands through it, in the walk that asked it.
    #[test]
    fn a_verdict_stands_through_a_trait_an_impl_with_parameters_answers() {
        const LINKS: usize = 40;
        let mut env = Env::default();
        let (root, pos) = (ModPath::root(), SourcePosition::default());
        let show = env
            .deftrait(
                &root,
                &literal!("Show"),
                std::iter::empty(),
                None,
                pos,
                Arc::new(Default::default()),
            )
            .unwrap();
        let array = |t: Type| Type::Array(Arc::new(t));
        let e = TVar::empty_generic(literal!("e"));
        let im = ImplDef {
            trait_id: show.id,
            target: array(Type::TVar(e.clone())),
            params: Arc::from_iter([e]),
            scope: root,
            methods: Default::default(),
            declared: false,
            pos,
            ori: Arc::new(Default::default()),
        };
        env.register_impl(Arc::new(im)).unwrap();
        let cells: Vec<TVar> = (0..=LINKS).map(|_| var("c")).collect();
        for link in cells.windows(2) {
            link[0].add_cell_constraint(array(Type::TVar(link[1].clone())));
        }
        let top = Type::TVar(cells[0].clone());
        let arrays = (0..LINKS).fold(prim(Typ::I64), |t, _| array(t));
        let lower = ContainsFlags::Lower | ContainsFlags::RigidCheck;
        let probe = |hist: &mut ContainsHist| {
            CONTAINS_STEPS.with(|n| n.set(0));
            let held = top.contains_int(lower, &env, hist, &arrays).unwrap();
            (held, CONTAINS_STEPS.with(|n| n.get()))
        };
        let mut hist = ContainsHist::new();
        for _ in 0..2 {
            let (held, steps) = probe(&mut hist);
            assert!(held && steps > 1, "{steps} steps");
            var("other").bind(prim(Typ::String));
        }
        let shown = array(prim(Typ::I64));
        let empty = BitFlags::empty();
        assert!(Type::trait_contains(show.id, empty, &env, &mut hist, &shown).unwrap());
        var("other").bind(prim(Typ::String));
        assert_eq!(probe(&mut hist), (true, 1));
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

    fn var(name: &'static str) -> TVar {
        TVar::empty_named(ArcStr::from(name))
    }

    fn prim(t: Typ) -> Type {
        Type::Primitive(t.into())
    }

    /// `p ⊇ q`, `q` above `w`, `w` bound to `[p, null]`: the three cells
    /// are one type, above both `i64` and `null`.
    #[test]
    fn bare_cycle_through_a_bound_cell_merges() {
        let env = Env::default();
        let (p, q, w) = (var("p"), var("q"), var("w"));
        p.add_lower(prim(Typ::I64));
        w.bind(Type::Set(Arc::from_iter([Type::TVar(p.clone()), prim(Typ::Null)])));
        q.add_lower(Type::TVar(w.clone()));
        Type::TVar(p.clone()).check_contains(&env, &Type::TVar(q.clone())).unwrap();
        assert!(p.same_cell(&q));
        assert!(p.cell_lower().iter().all(|l| !p.would_cycle(l)));
        p.settle(&env).unwrap();
        let both = Type::Primitive(Typ::I64 | Typ::Null);
        let settled = p.binding().unwrap();
        assert!(both.contains(&env, &settled).unwrap(), "{settled}");
        assert!(settled.contains(&env, &both).unwrap(), "{settled}");
    }

    /// `x` over the union `O` names, `b` its parameter there.
    fn named_union_over(b: &TVar) -> (Env, Type) {
        let (env, o) = Env::typed_at_root(&[("O<'t>", "['t, null]")], "O<'b>").unwrap();
        let known = AHashMap::from_iter([(b.name.clone(), Type::TVar(b.clone()))]);
        let o = o.replace_tvars(&known);
        o.seed_refs(&env);
        (env, o)
    }

    /// `x: O<'b>` over `type O<'t> = ['t, null]`, `b` bound, and `x ⊇ b`:
    /// `x` reaches `b` through the name's parameter, so the bound is
    /// dropped as the union written out is and `x` takes `b`'s binding; so
    /// does a var aliased onto `b`'s cell.
    #[test]
    fn bare_cycle_through_a_typedefs_parameter_takes_the_bound_cells_binding() {
        for (bound, aliased) in
            [(Typ::I64, false), (Typ::Null, false), (Typ::I64, true), (Typ::Null, true)]
        {
            let (x, b, held) = (var("x"), var("b"), var("b"));
            b.bind(prim(bound));
            let (env, o) = named_union_over(if aliased { &held } else { &b });
            x.add_cell_constraint(o);
            if aliased {
                held.alias(&b);
                assert!(held.same_cell(&b));
            }
            let below = Type::TVar(if aliased { held } else { b });
            Type::TVar(x.clone()).check_contains(&env, &below).unwrap();
            assert_eq!(x.binding(), Some(prim(bound)));
        }
    }

    /// The same cycle through a lower bound: `x` above `O<'b>` is `b` only
    /// where `b` holds the name's other member.
    #[test]
    fn bare_cycle_through_a_typedefs_parameter_below_holds_the_other_members() {
        let both = Type::Primitive(Typ::I64 | Typ::Null);
        for (bound, holds) in [(prim(Typ::I64), false), (both, true)] {
            let (x, b) = (var("x"), var("b"));
            b.bind(bound.clone());
            let (env, o) = named_union_over(&b);
            x.add_lower(o);
            let r = Type::TVar(x.clone()).check_contains(&env, &Type::TVar(b));
            assert_eq!(r.is_ok(), holds, "{r:?}");
            assert_eq!(x.binding(), holds.then_some(bound));
        }
    }

    /// Two open cells on that cycle are one cell, without the bound that
    /// named the union.
    #[test]
    fn bare_cycle_through_a_typedefs_parameter_merges_the_open_cells() {
        let (x, b) = (var("x"), var("b"));
        let (env, o) = named_union_over(&b);
        x.add_cell_constraint(o);
        Type::TVar(x.clone()).check_contains(&env, &Type::TVar(b.clone())).unwrap();
        assert!(x.same_cell(&b));
        assert!(x.cell_constraints().is_empty(), "{:?}", x.cell_constraints());
    }

    /// Through a lower bound the merged cell stays above the name's other
    /// member, beside a written member of the same union.
    #[test]
    fn bare_cycle_through_a_typedefs_parameter_below_keeps_the_other_members() {
        let (x, b) = (var("x"), var("b"));
        let (env, o) = named_union_over(&b);
        x.add_lower(Type::Set(Arc::from_iter([o, prim(Typ::String)])));
        Type::TVar(x.clone()).check_contains(&env, &Type::TVar(b.clone())).unwrap();
        assert!(x.same_cell(&b));
        let lower = x.cell_lower();
        assert!(lower.iter().all(|l| !x.would_cycle(l)), "{lower:?}");
        x.settle(&env).unwrap();
        assert_eq!(x.binding(), Some(Type::Primitive(Typ::Null | Typ::String)));
    }

    /// A declared var on a cycle of open cells is every one of them.
    #[test]
    fn bare_cycle_through_a_declared_var_merges_into_it() {
        let env = Env::default();
        let (a, p, q) = (var("a"), var("p"), var("q"));
        let _gate = a.open_rigid();
        p.add_lower(Type::TVar(a.clone()));
        q.add_lower(Type::TVar(p.clone()));
        Type::TVar(a.clone()).check_contains(&env, &Type::TVar(q.clone())).unwrap();
        assert!(a.same_cell(&p) && a.same_cell(&q));
        assert!(a.is_rigid() && !a.is_bound());
    }

    /// A declared var `e` whose bound is a cell `z` bound to `i64`: neither
    /// binds where `e` is rigid, so the relation is no cycle. `z ⊇ e` holds
    /// by the bound, `e ⊇ z` does not (`e` holds only itself), a probe
    /// answers as the commit does, and `e` keeps its bound.
    #[test]
    fn a_declared_var_and_the_bound_cell_its_bound_names() {
        let env = Env::default();
        let checks = [
            ContainsFlags::Commit | ContainsFlags::RigidCheck,
            ContainsFlags::Commit.into(),
            ContainsFlags::RigidCheck.into(),
        ];
        for flags in checks {
            for (holder, holds) in [(true, true), (false, false)] {
                let (z, e) = (var("z"), var("e"));
                z.bind(prim(Typ::I64));
                e.add_cell_constraint(Type::TVar(z.clone()));
                let _gate = e.open_rigid();
                let (tz, te) = (Type::TVar(z), Type::TVar(e.clone()));
                let (t0, t1) = if holder { (&tz, &te) } else { (&te, &tz) };
                let held = t0.contains_with_flags(flags, &env, t1).unwrap();
                assert_eq!(held, holds, "{t0} ⊇ {t1} under {flags:?}");
                assert!(!e.is_bound(), "{flags:?}");
                assert_eq!(e.cell_constraints().len(), 1, "{flags:?}");
            }
        }
    }

    /// A declared var holds a union of open cells as each of them, and no
    /// union with a member it does not hold, which decides none of them.
    #[test]
    fn a_declared_var_holds_a_union_of_open_cells_as_each() {
        let env = Env::default();
        let union = |ts: [Type; 2]| Type::Set(Arc::from_iter(ts));
        let (a, p, q) = (var("a"), var("p"), var("q"));
        let _gate = a.open_rigid();
        let both = union([Type::TVar(p.clone()), Type::TVar(q.clone())]);
        assert!(Type::TVar(a.clone()).contains_ordered(&env, &both).unwrap());
        assert!(a.same_cell(&p) && a.same_cell(&q));
        assert!(a.is_rigid() && !a.is_bound());
        let (b, r) = (var("b"), var("r"));
        let _gate = b.open_rigid();
        let mixed = union([Type::TVar(r.clone()), prim(Typ::I64)]);
        assert!(!Type::TVar(b.clone()).contains_ordered(&env, &mixed).unwrap());
        assert!(!b.same_cell(&r) && !r.is_bound());
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

    // a type either of two undecided members could take goes to neither;
    // one that only one can take, or that a member holds as it stands,
    // goes there, and an ordered walk takes the first
    #[test]
    fn two_undecided_members_refuse_a_type_either_could_take() {
        let env = Env::default();
        for (union, t) in [
            ("['a, 'b]", "i64"),
            ("['a, 'b, null]", "[i64, null]"),
            ("[Array<'a>, Array<'b>]", "Array<i64>"),
            ("[(i64, 'a), ('b, string)]", "(i64, string)"),
        ] {
            let e = parsed(union).contains(&env, &parsed(t)).unwrap_err();
            assert!(e.downcast_ref::<AmbiguousMember>().is_some(), "{union}: {e}");
            let ordered = parsed(union);
            assert!(ordered.contains_ordered(&env, &parsed(t)).unwrap(), "{union}");
        }
        for (union, t, taker) in [
            ("['a, Array<'b>]", "Array<i64>", "b"),
            ("['a, Error<'e>]", "Error<`X>", "e"),
            ("[Array<'a>, List<'b>]", "List<i64>", "b"),
            ("['a, null]", "i64", "a"),
        ] {
            let u = parsed(union);
            assert!(u.contains(&env, &parsed(t)).unwrap(), "{union}");
            let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            u.collect_tvars(&mut tvs);
            for (name, tv) in tvs.iter() {
                assert_eq!(tv.is_bound(), &**name == taker, "{union}: '{name}");
            }
        }
        let held = parsed("[[Array<'a>, null], [List<'b>, null]]");
        assert!(held.contains(&env, &parsed("null")).unwrap());
        assert!(held.has_unbound());
    }

    // a member of several primitive types that no constructor member
    // takes whole goes a type at a time: the open member takes only the
    // types none does, and the member as it stands where none takes any
    #[test]
    fn an_open_member_takes_only_the_primitives_no_member_does() {
        let env = Env::default();
        for (union, t, open) in [
            ("['a, null]", "[i64, null, Array<i64>]", "[i64, Array<i64>]"),
            ("['a, i64]", "[i64, null, Array<i64>]", "[null, Array<i64>]"),
            ("['a, string]", "[i64, null, Array<i64>]", "[i64, null, Array<i64>]"),
            ("['a, null, Array<'b>]", "[i64, string, null, Array<i64>]", "[i64, string]"),
        ] {
            let union = parsed(union);
            assert!(union.contains(&env, &parsed(t)).unwrap(), "{union}");
            let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            union.collect_tvars(&mut tvs);
            let a = Type::TVar(tvs["a"].clone()).deref_cloned().unwrap();
            let open = parsed(open);
            assert!(open.contains(&env, &a).unwrap(), "{union}: {a}");
            assert!(a.contains(&env, &open).unwrap(), "{union}: {a}");
        }
    }

    // a type holding a cell to decide that two decided members could
    // take goes to neither, under a function's parameter too; one that
    // only one can take, or that they decide alike, goes there, and an
    // ordered walk and a pre-unification take or pass by
    #[test]
    fn two_decided_members_refuse_an_undecided_type_either_could_take() {
        let env = Env::default();
        for (union, t, at) in [
            ("[Array<i64>, Array<string>]", "Array<'e>", TakerUnion::Holder),
            ("[Array<string>, Array<i64>, null]", "Array<'e>", TakerUnion::Holder),
            (
                "[Map<string, i64>, Map<string, bool>]",
                "Map<string, 'e>",
                TakerUnion::Holder,
            ),
            (
                "[fn(x: i64) -> i64, fn(x: string) -> i64]",
                "fn(x: 'e) -> i64",
                TakerUnion::Holder,
            ),
            (
                "fn(q: Array<'e>) -> i64",
                "fn(q: [Array<i64>, Array<bool>]) -> i64",
                TakerUnion::Value,
            ),
        ] {
            let e = match parsed(union).contains(&env, &parsed(t)) {
                Err(e) => e,
                Ok(held) => panic!("{union} ⊇ {t}: {held}"),
            };
            let taker = e.downcast_ref::<UndecidedTaker>();
            assert_eq!(taker.map(|u| u.at()), Some(at), "{union}: {e}");
            assert!(parsed(union).contains_ordered(&env, &parsed(t)).unwrap(), "{union}");
        }
        for (union, t, decided) in [
            ("[Array<i64>, List<string>]", "List<'e>", Some("string")),
            ("[(i64, i64), (string, string)]", "('e, i64)", Some("i64")),
            ("[[Array<bool>, `X], [Array<bool>, `Y]]", "Array<'e>", Some("bool")),
            ("[(Any, [i64, string]), (Any, [i64, bool])]", "('e, i64)", None),
        ] {
            let t = parsed(t);
            assert!(parsed(union).contains(&env, &t).unwrap(), "{union}");
            let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            t.collect_tvars(&mut tvs);
            let e = Type::TVar(tvs["e"].clone());
            assert_eq!(e.deref_cloned(), decided.map(parsed), "{union}");
        }
        let t = parsed("Array<'e>");
        Type::pre_unify_arg(&env, &parsed("[Array<i64>, Array<string>]"), &t).unwrap();
        assert!(t.has_unbound());
    }

    // members that are one type, cell for cell, are one member at the
    // pick, which takes the type: a union's repeats and order and a cell
    // bound to another tell no two apart; two open cells do
    #[test]
    fn members_that_are_one_type_take_a_type_as_one() {
        let env = Env::default();
        let set = |ts: &[&Type]| Type::Set(ts.iter().map(|t| (*t).clone()).collect());
        let array = |t: &Type| Type::Array(Arc::new(t.clone()));
        let fresh = || {
            let (a, b, c) = (var("a"), var("b"), var("c"));
            c.bind(Type::TVar(a.clone()));
            (Type::TVar(a), Type::TVar(b), Type::TVar(c))
        };
        let (a, b, c) = fresh();
        let null = prim(Typ::Null);
        for (m0, m1) in [
            (set(&[&a, &a]), a.clone()),
            (array(&set(&[&a, &a])), array(&a)),
            (c.clone(), a.clone()),
            (array(&set(&[&c, &a])), array(&a)),
            (set(&[&a, &null]), set(&[&null, &a, &a])),
            (set(&[&set(&[&a, &b]), &null]), set(&[&b, &null, &a])),
        ] {
            assert!(one_type(&m0, &m1) && one_type(&m1, &m0), "{m0} is {m1}");
        }
        assert!(!strictly_identical(&array(&set(&[&a, &a])), &array(&a)));
        for (m0, m1) in [
            (a.clone(), b.clone()),
            (set(&[&a, &b]), a.clone()),
            (array(&set(&[&a, &b])), array(&a)),
            (array(&set(&[&a, &null])), array(&a)),
            (set(&[&a, &null]), set(&[&a, &b, &null])),
            (Type::Set(Arc::from_iter([])), a.clone()),
        ] {
            assert!(!one_type(&m0, &m1) && !one_type(&m1, &m0), "{m0} is not {m1}");
        }
        let i64s = parsed("Array<i64>");
        for elements in [0, 1] {
            let (a, _, c) = fresh();
            let element = [set(&[&a, &a]), set(&[&c, &a])][elements].clone();
            let union = set(&[&array(&element), &array(&a)]);
            assert!(union.contains(&env, &i64s).unwrap(), "{union}");
            assert_eq!(a.deref_cloned(), Some(prim(Typ::I64)), "{union}");
        }
        let (a, _, c) = fresh();
        let union = set(&[&c, &a]);
        assert!(union.contains(&env, &prim(Typ::I64)).unwrap(), "{union}");
        assert_eq!(a.deref_cloned(), Some(prim(Typ::I64)), "{union}");
        let (a, b, _) = fresh();
        let union = set(&[&array(&set(&[&a, &b])), &array(&a)]);
        let e = union.contains(&env, &i64s).unwrap_err();
        assert!(e.downcast_ref::<AmbiguousMember>().is_some(), "{union}: {e}");
        assert!(a.has_unbound() && b.has_unbound(), "{union}");
    }

    // of two undecided members that could take a type, one that holds the
    // others as it stands takes it where that decides every cell another
    // would; where it leaves one of them open, or the walk is a
    // pre-unification, none takes it and nothing is decided
    #[test]
    fn a_member_holding_the_others_takes_a_type_where_it_decides_their_cells() {
        let env = Env::default();
        // a tuple of the members, so one name is one cell and no two merge
        let union = |members: &str| match &parsed(members) {
            Type::Tuple(ms) => Type::Set(ms.clone()),
            t => panic!("{t} is no tuple"),
        };
        let bound = |u: &Type| {
            let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            u.collect_tvars(&mut tvs);
            let mut bound: SmallVec<[ArcStr; 4]> = tvs
                .iter()
                .filter(|(_, tv)| tv.is_bound())
                .map(|(n, _)| n.clone())
                .collect();
            bound.sort();
            bound
        };
        for (members, t, decided) in [
            (
                "((i64, [`A('a), `B(string)]), (i64, `A('a)), (i64, `B(string)))",
                "(i64, `A(i64))",
                &["a"][..],
            ),
            (
                "((i64, [`A('a), `B('b)], 'z), (i64, `A('a), 'z), (i64, `B('b), 'z))",
                "(i64, `B(string), null)",
                &["b", "z"],
            ),
            (
                "((i64, [Array<'a>, {k: 'b}]), (i64, Array<'a>), (i64, {k: 'b}))",
                "(i64, Array<i64>)",
                &["a"],
            ),
        ] {
            let u = union(members);
            assert!(u.contains(&env, &parsed(t)).unwrap(), "{u}");
            assert_eq!(&bound(&u)[..], decided, "{u}");
            let early = union(members);
            let e = early
                .contains_int(
                    ContainsFlags::Commit.into(),
                    &env,
                    &mut ContainsHist::early(),
                    &parsed(t),
                )
                .unwrap_err();
            assert!(e.downcast_ref::<AmbiguousMember>().is_some(), "{early}: {e}");
            assert!(bound(&early).is_empty(), "{early}");
        }
        for (members, t) in [
            ("((i64, Any, 'z), (i64, ('c, Any), 'z))", "(i64, (string, i64), i64)"),
            ("(`S(Any, 'z), `S(Array<'c>, 'z))", "`S(Array<string>, i64)"),
            ("((i64, [`A('a), `B('b)]), (i64, `A('a)), (i64, `B('b)))", "(i64, 'q)"),
        ] {
            let u = union(members);
            let e = u.contains(&env, &parsed(t)).unwrap_err();
            assert!(e.downcast_ref::<AmbiguousMember>().is_some(), "{u}: {e}");
            assert!(bound(&u).is_empty(), "{u}");
        }
        // the copies give `K(i64) to `K('c), the first that holds it, and the
        // walk to `K(['c, i64]), which holds it as it stands
        let Type::Tuple(ps) = &parsed("(i64, (`K('c), `K(['c, i64])), 'z, `K('c))")
        else {
            panic!("no tuple")
        };
        let [n, Type::Tuple(ks), z, k] = &ps[..] else { panic!("no tuple") };
        let tuple = |ts: [&Type; 3]| Type::Tuple(ts.into_iter().cloned().collect());
        let u = Type::Set(Arc::from_iter([
            tuple([n, &Type::Set(ks.clone()), z]),
            tuple([n, k, z]),
        ]));
        let e = u.contains(&env, &parsed("(i64, `K(i64), i64)")).unwrap_err();
        assert!(e.downcast_ref::<AmbiguousMember>().is_some(), "{u}: {e}");
        assert_eq!(&bound(&u)[..], ["z"], "{u}");
    }

    // one cell standing twice is no union of two undecided members: an
    // undecided union goes to the cell whole, its members left as they
    // are, and a signature's position holding it does not wait
    #[test]
    fn a_cell_standing_twice_is_one_undecided_member() {
        let env = Env::default();
        let set = |ts: &[&Type]| Type::Set(ts.iter().map(|t| (*t).clone()).collect());
        let (a, x, y) = (var("a"), var("x"), var("y"));
        let c = var("c");
        c.bind(Type::TVar(a.clone()));
        let (ta, tc) = (Type::TVar(a.clone()), Type::TVar(c));
        let (tx, ty) = (Type::TVar(x.clone()), Type::TVar(y.clone()));
        assert!(!set(&[&ta, &ta]).holds_open_union());
        assert!(!set(&[&tc, &ta]).holds_open_union());
        assert!(set(&[&ta, &tx]).holds_open_union());
        assert!(set(&[&ta, &ta]).contains(&env, &set(&[&tx, &ty])).unwrap());
        assert!(!x.is_bound() && !y.is_bound() && !x.same_cell(&y), "{tx} {ty}");
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

    fn formal(t: &Type) -> Option<Type> {
        match t {
            Type::Fn(f) => f.args[0].typ.deref_cloned(),
            Type::Set(s) => s.iter().find_map(|m| match m {
                Type::Fn(f) if f.args[0].name().is_some_and(|n| n == "i") => {
                    f.args[0].typ.deref_cloned()
                }
                _ => None,
            }),
            _ => panic!("not a function: {t}"),
        }
    }

    // the first member admits `fn(i: 'c) -> 'c` part by part but not whole
    // ('c would be string and i64): the second takes it, at ⊥
    #[test]
    fn a_member_admitted_by_parts_alone_binds_nothing() {
        let env = Env::default();
        let written = |ms: [&str; 2]| Type::Set(Arc::from_iter(ms.map(parsed)));
        let union = written(["fn(path: string) -> i64", "fn(i: _) -> _"]);
        let one = parsed("fn(i: 'c) -> 'c");
        assert!(union.contains(&env, &one).unwrap(), "{union} ⊇ {one}");
        assert_eq!(formal(&one), Some(Type::Bottom), "{one}");
        let both = written(["fn(path: string) -> i64", "fn(i: 'c) -> 'c"]);
        assert!(union.contains(&env, &both).unwrap(), "{union} ⊇ {both}");
        assert_eq!(formal(&both), Some(Type::Bottom), "{both}");
    }

    #[test]
    fn a_bound_cell_the_other_side_mentions_answers_by_its_binding() {
        let env = Env::default();
        let a = TVar::empty_named(literal!("a"));
        a.bind(Type::Any);
        let array = Type::Array(Arc::new(Type::TVar(a.clone())));
        assert!(Type::TVar(a).contains(&env, &array).unwrap());
    }

    // a bound that holds the cell it would narrow says nothing of it: the
    // cell, a union over it, over a cell bound to it, over a cell above
    // it, a name for such a union; so a probe of the cell returns
    #[test]
    fn a_bound_that_holds_its_own_cell_narrows_nothing() {
        let (env, named) =
            Env::typed_at_root(&[("E<'p, 'q>", "['p, 'q]")], "E<i64, null>").unwrap();
        let Type::Ref(named) = &named else { unreachable!() };
        let set = |ts: &[&Type]| Type::Set(ts.iter().map(|t| (*t).clone()).collect());
        let null = prim(Typ::Null);
        let (a, b, link, above) = (var("a"), var("b"), var("link"), var("above"));
        let (ta, tb) = (Type::TVar(a.clone()), Type::TVar(b.clone()));
        link.bind(ta.clone());
        above.add_lower(set(&[&ta, &prim(Typ::String)]));
        let name = named.with_params(Arc::from_iter([null.clone(), ta.clone()]));
        for own in [
            ta.clone(),
            set(&[&null, &ta]),
            set(&[&null, &Type::TVar(link)]),
            set(&[&null, &Type::TVar(above)]),
            set(&[&null, &set(&[&tb, &ta])]),
            Type::Ref(Arc::new(name)),
        ] {
            a.narrow_cell(&env, own.clone()).unwrap();
            assert!(a.cell_constraints().is_empty(), "{own}");
        }
        assert!(ta.contains(&env, &prim(Typ::I64)).unwrap());
        for other in [set(&[&null, &tb]), Type::Array(Arc::new(ta.clone()))] {
            let a = var("a");
            a.narrow_cell(&env, other.clone()).unwrap();
            assert_eq!(a.cell_constraints().len(), 1, "{other}");
        }
    }

    // a conjunct that comes to hold its own cell, by a lower bound a
    // member takes after it, admits what the cell takes
    #[test]
    fn a_conjunct_that_comes_to_hold_its_own_cell_admits_every_type() {
        let env = Env::default();
        let set = |ts: &[&Type]| Type::Set(ts.iter().map(|t| (*t).clone()).collect());
        let (int, null) = (prim(Typ::I64), prim(Typ::Null));
        let (u, e) = (var("u"), var("e"));
        let (tu, te) = (Type::TVar(u.clone()), Type::TVar(e.clone()));
        u.narrow_cell(&env, set(&[&te, &null])).unwrap();
        te.check_contains_lower(&env, &tu).unwrap();
        assert_eq!(u.cell_constraints().len(), 1);
        assert!(!e.same_cell(&u) && !e.cell_lower().is_empty(), "{tu} {te}");
        assert!(tu.contains_with_flags(BitFlags::empty(), &env, &int).unwrap());
        assert!(tu.contains(&env, &int).unwrap());
        assert!(te.contains(&env, &prim(Typ::String)).is_ok_and(|held| !held), "{te}");
    }

    // a bound's parts reach the open cells a name in the value is applied
    // over, however deep the name's definition holds them (an exact part
    // binds its cell, another narrows it), and a decided variable of the
    // bound stands as its binding; two recursive names met against each
    // other are walked once
    #[test]
    fn a_bounds_parts_reach_the_cells_under_a_name() {
        let defs = [
            ("G<'a, 'b>", "[`X('a), `Y(H<'b, 'a>)]"),
            ("H<'a, 'b>", "[`X('b), `Y(G<'b, 'a>), `Z('a)]"),
            ("T<'x>", "[`Leaf('x), `Node(Array<T<'x>>)]"),
            ("U<'x>", "[`Leaf('x), `Node(Array<U<'x>>)]"),
        ];
        for (bound_and_value, decided, part) in [
            ("(G<i64, string>, G<'p, 'q>)", None, "string"),
            ("(Array<T<string>>, Array<U<'q>>)", None, "string"),
            (
                "(('d, i64), (Array<'q>, i64))",
                Some("Array<[string, null]>"),
                "[string, null]",
            ),
        ] {
            let (env, t) = Env::typed_at_root(&defs, bound_and_value).unwrap();
            t.alias_tvars(&mut LPooled::take());
            let Type::Tuple(sides) = &t else { panic!("{t} is no pair") };
            let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            t.collect_tvars(&mut cells);
            if let Some(d) = decided {
                cells["d"].bind(parsed(d))
            }
            let holder = var("holder");
            holder.add_cell_constraint(sides[0].clone());
            assert!(holder.commit_conjuncts(&env, &sides[1]).unwrap(), "{t}");
            let part = parsed(part);
            let q = &cells["q"];
            let within: SmallVec<[Type; 2]> =
                q.binding().into_iter().chain(q.cell_constraints()).collect();
            assert!(
                matches!(&within[..], [c] if part.contains(&env, c).unwrap()
                    && c.contains(&env, &part).unwrap()),
                "{t}: 'q within {within:?}"
            );
        }
    }

    // a bound's part narrows no rigid cell of the value: the bound's own
    // open cell stands above the rigid cell, which keeps the bounds it had
    #[test]
    fn a_bounds_part_narrows_no_rigid_cell() {
        let (env, t) = Env::typed_at_root(&[], "(Array<['c, null]>, Array<'a>)").unwrap();
        t.alias_tvars(&mut LPooled::take());
        let Type::Tuple(sides) = &t else { panic!("{t} is no pair") };
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        t.collect_tvars(&mut cells);
        let (a, c) = (&cells["a"], &cells["c"]);
        let _held = a.open_rigid();
        let holder = var("holder");
        holder.add_cell_constraint(sides[0].clone());
        assert!(holder.commit_conjuncts(&env, &sides[1]).unwrap(), "{t}");
        assert!(a.cell_constraints().is_empty(), "{t}");
        assert!(c.open_cell().is_some_and(|o| !o.same_cell(a)), "{t}");
        assert!(matches!(&c.cell_lower()[..], [Type::TVar(l)] if l.same_cell(a)), "{t}");
    }

    // a bound's part that holds an open cell narrows no cell of the value:
    // the bound's own containment puts its cell above the value's parts,
    // one of them that cell under a constructor
    #[test]
    fn a_bounds_part_holding_an_open_cell_narrows_no_cell() {
        let (env, t) =
            Env::typed_at_root(&[], "(Array<['c, null]>, Array<['u, Array<'u>]>)")
                .unwrap();
        t.alias_tvars(&mut LPooled::take());
        let Type::Tuple(sides) = &t else { panic!("{t} is no pair") };
        let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        t.collect_tvars(&mut cells);
        let (c, u) = (&cells["c"], &cells["u"]);
        let holder = var("holder");
        holder.add_cell_constraint(sides[0].clone());
        assert!(holder.commit_conjuncts(&env, &sides[1]).unwrap(), "{t}");
        assert!(u.cell_constraints().is_empty(), "{t}");
        assert_eq!(c.cell_lower().len(), 1, "{t}");
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
        return match t.lowered_cells() {
            Some(_) => find_impl_with(env, trait_id, &t.frozen(env), hist),
            None => Ok(None),
        };
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
        let hc = head.contains_int(ContainsFlags::Commit.into(), env, hist, t)?;
        let tc = hc && t.contains_int(ContainsFlags::Commit.into(), env, hist, &head)?;
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

#[cfg(test)]
thread_local! {
    static CONTAINS_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NESTING_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static HEAD_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static VERDICTS_ANSWERED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}
