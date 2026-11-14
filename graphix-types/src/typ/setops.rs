use crate::{
    AddrSet, PrintFlag,
    env::Env,
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        Bound, Brief, ContainsFlags, FnType, Kept, Lazy, NodeKey, NodePairs, RefHist,
        RefPair, TVar, Type, TypeRef,
        contains::{ContainsHist, find_open_cell, on_copies, renaming_linked},
        fntyp::LambdaIds,
        node_addr,
        tvar::{Fresh, cell_writes},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow};
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{iter, mem, ops::ControlFlow, slice};
use triomphe::Arc;

struct MatchingTypes {
    memo: RefHist<AHashMap<RefPair, Kept<Type>>>,
    refs: LPooled<AHashSet<(Option<usize>, Option<usize>)>>,
    /// One containment walk for every probe the narrowing asks: a pair of
    /// shared parts is probed once, whichever pair of the narrowing meets it.
    contains: ContainsHist,
    /// Type a part holding a rigid cell by the written test's part
    /// ([`Type::matching_parts`]).
    inside: bool,
}

impl MatchingTypes {
    fn new(inside: bool) -> Self {
        MatchingTypes {
            memo: RefHist::new(),
            refs: LPooled::take(),
            contains: ContainsHist::new(),
            inside,
        }
    }
}

/// diff's walk state: each pair in progress with what it subtracts when
/// met again inside its own difference (nothing), and the difference of
/// each finished pair whose walk was cut only at itself or below it and
/// whose minuend, of which a difference is made, [`RefHist::ref_id`] keys
/// exactly. A subtrahend keyed alike is read alike: diff reads every part
/// of it through. A finished pair's difference is kept with what its walk
/// reached, and is read again only where the path holds none of it: a
/// fresh walk there would be cut nowhere it was not, so the memo changes
/// no answer.
struct DiffHist {
    /// Each pair in progress: its minuend and its depth on the path, from 1.
    hist: RefHist<AHashMap<RefPair, (Type, usize)>>,
    done: Lazy<AHashMap<RefPair, Kept<Done>>>,
    ids: Lazy<AHashMap<Reach, usize>>,
    unfoldings: Unfoldings,
    pairs: Pairs,
    /// The difference of each pair of shared nodes, by their identities,
    /// met past [`NodePairs::EAGER`] steps, with the [`cell_writes`] it
    /// was reached at: a residual a walk built is a DAG, and its parts are
    /// subtracted once while no cell they may read is written.
    parts: Lazy<AHashMap<(NodeKey, NodeKey), Kept<(Done, u64)>>>,
    steps: u32,
    /// The depth of the shallowest pair in progress the current pair's
    /// walk was cut at, `usize::MAX` for none; 0 where a budget stopped it,
    /// so a walk a budget widened keeps nothing.
    cut: usize,
    /// What the current pair's walk reached.
    reached: Reached,
    /// A budget made the walk subtract less than it would have.
    widened: bool,
    /// What the difference of the pair being walked holds.
    over_cut: OverCut,
}

/// A finished pair's difference, what its walk reached and what the
/// difference holds.
struct Done {
    answer: Type,
    reached: Reached,
    over_cut: OverCut,
}

/// The Errors a walk built, nested, around a minuend it answered whole
/// (for a pair met again, past the budget, or a collection or an Error
/// kept whole over one); `None` for a difference that holds no such
/// minuend.
type OverCut = Option<usize>;

/// A minuend answered whole, no Error built around it.
const CUT: OverCut = Some(0);

/// How many Errors, nested, a difference builds around a minuend answered
/// whole: an Error whose payload's difference holds that many keeps its
/// payload whole, so no residual grows with the recursion it was cut in
/// (`design/tvar_constraints.md`, Unfolding a pair of definitions).
pub(super) const ERRORS_OVER_CUT: usize = 3;

/// What a diff walk reached that a path may hold: a pair by its key, a
/// pair of definitions ([`Unfoldings`] cuts a walk at one met again).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Reach {
    Pair(RefPair),
    Defs(DefPair),
}

/// A set of [`Reach`]es by the ids [`DiffHist`] gives them.
#[derive(Default, Clone)]
struct Reached(SmallVec<[u64; 4]>);

impl Reached {
    fn insert(&mut self, id: usize) {
        let (w, b) = (id / 64, id % 64);
        if self.0.len() <= w {
            self.0.resize(w + 1, 0)
        }
        self.0[w] |= 1 << b
    }

    fn contains(&self, id: usize) -> bool {
        self.0.get(id / 64).is_some_and(|w| w & (1 << (id % 64)) != 0)
    }

    fn union(&mut self, other: &Reached) {
        if self.0.len() < other.0.len() {
            self.0.resize(other.0.len(), 0)
        }
        self.0.iter_mut().zip(other.0.iter()).for_each(|(a, b)| *a |= b)
    }
}

impl DiffHist {
    fn new() -> Self {
        DiffHist {
            hist: RefHist::new(),
            done: Lazy::new(),
            ids: Lazy::new(),
            parts: Lazy::new(),
            steps: 0,
            unfoldings: Unfoldings::default(),
            pairs: Pairs::default(),
            cut: usize::MAX,
            reached: Reached::default(),
            widened: false,
            over_cut: None,
        }
    }

    fn id(&mut self, r: Reach) -> usize {
        let ids = self.ids.get_mut();
        let next = ids.len();
        *ids.entry(r).or_insert(next)
    }

    /// Note that the current walk entered `r`, or was cut at it. Only
    /// what a walk enters is ever on a path, so the ids stay within the
    /// pairs the walk may unfold.
    fn reach(&mut self, r: Reach) {
        let id = self.id(r);
        self.reached.insert(id);
    }

    /// The key [`DiffHist::parts`] holds `t0`, `t1`'s difference by: a
    /// pair diff walks into, a union on a side or two of one constructor.
    #[inline]
    fn parts_key(&mut self, t0: &Type, t1: &Type) -> Option<(NodeKey, NodeKey)> {
        if self.steps < NodePairs::EAGER {
            self.steps += 1;
            return None;
        }
        match (t0, t1) {
            (
                Type::Set(_),
                Type::Set(_) | Type::Error(_) | Type::Array(_) | Type::List(_),
            )
            | (Type::Error(_) | Type::Array(_) | Type::List(_), Type::Set(_))
            | (Type::Error(_), Type::Error(_))
            | (Type::Array(_), Type::Array(_))
            | (Type::List(_), Type::List(_)) => (),
            _ => return None,
        }
        Some((t0.node_key()?, t1.node_key()?))
    }

    fn holds(&mut self, c: OverCut) {
        self.over_cut = self.over_cut.max(c)
    }

    /// `f`'s difference, apart from the pair being walked: the pair holds
    /// what it holds unless nothing is left of it.
    fn part(&mut self, f: impl FnOnce(&mut Self) -> Result<Type>) -> Result<Type> {
        let outer = mem::take(&mut self.over_cut);
        let r = f(self);
        let part = mem::replace(&mut self.over_cut, outer);
        if r.as_ref().is_ok_and(|d| !is_empty_ty(d)) {
            self.holds(part)
        }
        r
    }

    /// Whether the path holds nothing of `r`.
    fn path_clear_of(&self, r: &Reached) -> bool {
        let Some(ids) = self.ids.get() else { return true };
        let held = |k: Reach| ids.get(&k).is_some_and(|id| r.contains(*id));
        !self.hist.keys().any(|k| held(Reach::Pair(*k)))
            && !self.unfoldings.held().any(|d| held(Reach::Defs(d)))
    }
}

/// How many pairs one walk (a top-level `diff` or `could_match`) unfolds,
/// a pair any side of which is a named type. Past it diff subtracts
/// nothing and could_match answers an overlap, wherever a pair is met: the
/// walk's work is bounded whatever the types, branching recursive unions
/// whose paths multiply with their depth included
/// (`design/tvar_constraints.md`, Unfolding a pair of definitions).
pub(super) const WALK_PAIRS: usize = 1000;

/// The pairs a walk has unfolded, a side of each a named type, counted
/// against [`WALK_PAIRS`].
#[derive(Default)]
pub(super) struct Pairs(usize);

impl Pairs {
    /// Count one more unfolding; false once the walk has used them all.
    pub(super) fn unfold(&mut self) -> bool {
        let more = self.0 < WALK_PAIRS;
        self.0 += more as usize;
        more
    }
}

/// The definitions of a pair of named types a walk unfolds, their
/// arguments ignored.
pub(super) type DefPair = (usize, usize);

/// The pair [`Unfoldings`] counts: none unless both sides are named types.
/// A side that is not walks into its own parts, smaller at every step, until
/// it meets a name, so a pattern written as a type, a name's alias
/// included, is unfolded against the other side as deep as it goes.
pub(super) fn def_pair(env: &Env, t0: &Type, t1: &Type) -> Option<DefPair> {
    let def = |t: &Type| match t {
        Type::Ref(tr) => tr.resolve_in(env).map(|r| r.def_key()),
        _ => None,
    };
    Some((def(t0)?, def(t1)?))
}

/// How many unfoldings of one pair of definitions a walk's path may
/// hold; the next is the pair met again. Types a walk builds as it goes (a
/// union less each member in turn, parameters that grow) are new keys at
/// every level, and an exact difference may repeat only after the lcm of
/// the cycles it walks. Each more unfolding multiplies a walk's cost
/// (`design/tvar_constraints.md`, Unfolding a pair of definitions).
pub(super) const UNFOLDINGS: usize = 1;

/// The pairs of definitions a walk is inside, each with its unfoldings'
/// pairs in progress, innermost last.
#[derive(Default)]
pub(super) struct Unfoldings(LPooled<AHashMap<DefPair, SmallVec<[RefPair; 1]>>>);

impl Unfoldings {
    /// The innermost pair in progress of `defs` when the walk has unfolded
    /// it [`UNFOLDINGS`] times.
    pub(super) fn again(&self, defs: Option<DefPair>) -> Option<RefPair> {
        self.0
            .get(&defs?)
            .filter(|ks| ks.len() >= UNFOLDINGS)
            .and_then(|ks| ks.last().copied())
    }

    pub(super) fn enter(&mut self, defs: Option<DefPair>, key: RefPair) {
        if let Some(defs) = defs {
            self.0.entry(defs).or_default().push(key)
        }
    }

    pub(super) fn leave(&mut self, defs: Option<DefPair>) {
        if let Some(ks) = defs.and_then(|defs| self.0.get_mut(&defs)) {
            ks.pop();
        }
    }

    /// The pairs of definitions the path holds.
    fn held(&self) -> impl Iterator<Item = DefPair> + '_ {
        self.0.iter().filter(|(_, ks)| !ks.is_empty()).map(|(d, _)| *d)
    }
}

/// Whether [`RefHist::ref_id`] may key `t` alike with a type that prints
/// apart: `t` holds a bound cell, keyed as its binding, or a cell in a
/// function type, paired by position.
fn keyed_loosely(t: &Type) -> bool {
    cell_beneath(t, |tv, in_fn| in_fn || tv.binding().is_some())
}

/// Whether `t` holds a cell `pick` takes (told whether the cell is in a
/// function type), each shared node walked once. A reference's parameters
/// are read, its expansion is not.
fn cell_beneath(t: &Type, pick: impl Fn(&TVar, bool) -> bool) -> bool {
    fn go(
        t: &Type,
        in_fn: bool,
        pick: &impl Fn(&TVar, bool) -> bool,
        seen: &mut AHashSet<(usize, bool)>,
    ) -> ControlFlow<()> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if pick(tv, in_fn) => ControlFlow::Break(()),
            Type::TVar(_) => ControlFlow::Continue(()),
            t if node_addr(t).is_some_and(|n| !seen.insert((n, in_fn))) => {
                ControlFlow::Continue(())
            }
            Type::Fn(_) => t.try_for_each_child(&mut |c| go(c, true, pick, seen)),
            t => t.try_for_each_child(&mut |c| go(c, in_fn, pick, seen)),
        })
    }
    go(t, false, &pick, &mut LPooled::take()).is_break()
}

fn is_empty_ty(t: &Type) -> bool {
    match t {
        Type::Bottom => true,
        Type::Primitive(p) => p.is_empty(),
        _ => false,
    }
}

impl Type {
    /// No value inhabits this type: bottom, the empty primitive set, or
    /// an empty union (a `diff` residual with nothing left).
    #[doc(hidden)]
    pub fn is_uninhabited(&self) -> bool {
        match self {
            Type::Set(ts) => ts.is_empty(),
            t => is_empty_ty(t),
        }
    }

    /// Whether a walk of `self` is one of `members`' ([`walks_alike`]).
    pub fn walks_as_one_of(&self, members: &[Type]) -> bool {
        members.iter().any(|m| walks_alike(m, self))
    }

    /// Drop from `members` each walked as a member before it.
    pub fn drop_repeated(members: &mut SmallVec<[Type; 8]>) {
        let mut i = 1;
        while i < members.len() {
            match members[i].walks_as_one_of(&members[..i]) {
                true => drop(members.remove(i)),
                false => i += 1,
            }
        }
    }
}

fn diff_already_normal(before: &Type, after: &Type) -> bool {
    match (before, after) {
        (_, Type::Primitive(p)) if p.is_empty() => true,
        (Type::Set(s), Type::Set(out)) => {
            let mut i = 0;
            for o in out.iter() {
                loop {
                    if i >= s.len() {
                        return false;
                    }
                    let j = i;
                    i += 1;
                    if s[j] == *o {
                        break;
                    }
                }
            }
            true
        }
        (Type::Set(s), t) => s.iter().any(|m| m == t),
        _ => false,
    }
}

/// Structural identity for union-collapse decisions: two unbound
/// cells are identical only when they are one cell (a collapse must not
/// discard a cell whose future binding may diverge), a bound cell is its
/// binding, and inside a function type the cells pair consistently
/// (`fn('a) -> 'a` is `fn('b) -> 'b`, not `fn('a) -> 'b`).
pub(super) fn union_identical(t0: &Type, t1: &Type) -> bool {
    Identity::new(None, &mut NodePairs::default()).same(t0, t1)
}

/// [`union_identical`] with a name read as the definition it means in
/// `env` ([`TypeRef::same_definition`]), filled or not: what a walk has
/// filled so far tells no two types apart.
pub(super) fn union_identical_in(env: &Env, t0: &Type, t1: &Type) -> bool {
    Identity { env: Some(env), ..Identity::new(None, &mut NodePairs::default()) }
        .same(t0, t1)
}

/// [`union_identical`] with every unbound cell by identity, inside a
/// function type too: one signature's variables are its own.
pub(crate) fn strictly_identical(t0: &Type, t1: &Type) -> bool {
    Identity { strict: true, ..Identity::new(None, &mut NodePairs::default()) }
        .same(t0, t1)
}

/// [`strictly_identical`] with a union read as the set of its members,
/// through its bindings and nested unions: neither their order nor a
/// member that stands twice tells two types apart, so `Array<['a, 'a]>`
/// is `Array<'a>`. Two open cells are still two.
pub(super) fn one_type(t0: &Type, t1: &Type) -> bool {
    Identity {
        strict: true,
        sets: true,
        ..Identity::new(None, &mut NodePairs::default())
    }
    .same(t0, t1)
}

/// [`one_type`] with a union's members but its open cells read in order:
/// a narrowing walks two such types alike, where a union of two members
/// one type with each other walks apart from one of them.
pub(super) fn walks_alike(t0: &Type, t1: &Type) -> bool {
    Identity {
        strict: true,
        sets: true,
        in_order: true,
        ..Identity::new(None, &mut NodePairs::default())
    }
    .same(t0, t1)
}

/// Whether the function types `t0` and `t1` differ by a renaming of their
/// open cells alone ([`union_identical_in`]), a scheme's cell beside a live
/// one too: each part of one stands beside its own image in the other.
pub(super) fn renames(env: &Env, t0: &Type, t1: &Type) -> bool {
    Identity {
        env: Some(env),
        across: true,
        ..Identity::new(None, &mut NodePairs::default())
    }
    .same(t0, t1)
}

/// The pairs of live cells union collapses have met: each pair is one
/// cell once [`apply_welds`] runs.
#[derive(Default)]
pub(super) struct Welds {
    pairs: SmallVec<[(TVar, TVar); 2]>,
    /// The alternatives of the join the pairs are for: two members one of
    /// them holds stay two ([`apart`]).
    alts: SmallVec<[Type; 4]>,
}

impl Welds {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn joining(alts: SmallVec<[Type; 4]>) -> Self {
        Self { pairs: SmallVec::new(), alts }
    }

    pub(super) fn alternatives(&self) -> &[Type] {
        &self.alts
    }
}

impl std::ops::Deref for Welds {
    type Target = SmallVec<[(TVar, TVar); 2]>;

    fn deref(&self) -> &Self::Target {
        &self.pairs
    }
}

impl std::ops::DerefMut for Welds {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pairs
    }
}

/// Whether one of `alts` holds both `t0` and `t1`: two members of one
/// alternative are two, however alike.
pub(super) fn apart(alts: &[Type], t0: &Type, t1: &Type) -> bool {
    let (Some(k0), Some(k1)) = (t0.node_key(), t1.node_key()) else {
        return false;
    };
    alts.iter().any(|a| holds_node(a, &[k0, k1]))
}

/// [`union_identical`], for a collapse that keeps `t0` in place of `t1`:
/// the live cells it pairs go on `welds`, so the dropped member's cells
/// learn what the kept one learns. Two members of one alternative of the
/// join `welds` is for collapse only where they are identical as they stand.
pub(super) fn union_collapses(t0: &Type, t1: &Type, welds: &mut Welds) -> bool {
    let start = welds.len();
    let mut same = Identity::new(Some(welds), &mut NodePairs::default()).same(t0, t1);
    if same && apart(&welds.alts, t0, t1) && !strictly_identical(t0, t1) {
        same = false;
    }
    if !same {
        welds.truncate(start)
    }
    same
}

/// [`union_collapses`] for two lower bounds of one cell: outside a function
/// type too it pairs two open cells nothing bounds ([`TVar::unbounded`]),
/// so bounds that differ only in such cells are one bound once the pairs
/// weld. Two cells earlier compile tasks left open are decided open, and
/// stay two.
pub(super) fn lower_collapses(t0: &Type, t1: &Type, welds: &mut Welds) -> bool {
    let start = welds.len();
    let same = Identity {
        unbounded: true,
        ..Identity::new(Some(welds), &mut NodePairs::default())
    }
    .same(t0, t1);
    if !same {
        welds.truncate(start)
    }
    same
}

/// [`union_identical`] of two members of a union as the source writes it,
/// where no two occurrences of a variable share a cell yet and its name
/// says which one it is: two open cells are identical when they have one
/// name and the same bounds.
pub(super) fn written_identical(t0: &Type, t1: &Type) -> bool {
    Identity {
        by_name: true,
        pairs: Some(SmallVec::new()),
        ..Identity::new(None, &mut NodePairs::default())
    }
    .same(t0, t1)
}

/// Weld each pair `welds` holds.
pub(super) fn apply_welds(welds: &mut Welds) {
    for (a, b) in welds.drain(..) {
        super::contains::weld(&a, &b)
    }
}

/// What [`union_identical`] finds of two types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Identical {
    Apart,
    /// Identical, each cell met only beside itself.
    Same,
    /// Identical by pairing two distinct open cells inside a function
    /// type.
    Renamed,
}

/// [`union_identical`] over the questions of one walk: the verdict of a
/// node pair whose walk read no cell stands for good, and the node pairs
/// found identical, with no two distinct cells paired, stand while this
/// thread writes no cell, so a chain asked about at every link is walked
/// once. A question keys its node pairs past `NodePairs::EAGER` steps,
/// and its pairs of applications from its first: an application is
/// what a DAG through typedefs shares, which the questions meet at
/// every link.
#[derive(Default)]
pub(super) struct IdenticalMemo {
    writes: u64,
    found: NodePairs,
    settled: Settled,
}

impl IdenticalMemo {
    pub(super) fn identical(&mut self, t0: &Type, t1: &Type) -> Identical {
        let writes = cell_writes();
        if self.writes != writes || !memo_on() {
            self.found.clear();
            self.writes = writes;
        }
        if !memo_on() {
            self.settled.clear()
        }
        self.found.restart();
        let mut id = Identity {
            settled: Some(&mut self.settled),
            ..Identity::new(None, &mut self.found)
        };
        let same = id.same(t0, t1);
        let renamed = id.renamed;
        if renamed {
            self.found.clear()
        }
        match (same, renamed) {
            (false, _) => Identical::Apart,
            (true, false) => Identical::Same,
            (true, true) => Identical::Renamed,
        }
    }
}

/// The verdicts of node pairs whose walk read no cell: no variable, and
/// no name whose resolution cell is open. Neither side can change, so a
/// verdict stands through any write. Each pair is held, so an address
/// stays its node's.
#[derive(Default)]
struct Settled(Option<LPooled<AHashMap<(NodeKey, NodeKey), (Type, Type, bool)>>>);

impl Settled {
    fn get(&self, key: (NodeKey, NodeKey)) -> Option<bool> {
        self.0.as_ref().and_then(|m| m.get(&key)).map(|(_, _, same)| *same)
    }

    fn insert(&mut self, key: (NodeKey, NodeKey), t0: &Type, t1: &Type, same: bool) {
        self.0
            .get_or_insert_with(LPooled::take)
            .insert(key, (t0.clone(), t1.clone(), same));
    }

    fn clear(&mut self) {
        if let Some(m) = &mut self.0 {
            m.clear()
        }
    }
}

/// Identity of `t0` and `t1`.
struct Identity<'a> {
    strict: bool,
    /// A union is the set of its members ([`one_type`]); with `strict`
    /// alone, since a failed search for a member must pair no cells.
    sets: bool,
    /// With `sets`, a union's members but its open cells in order
    /// ([`walks_alike`]).
    in_order: bool,
    /// Two open cells of one name are identical, whatever holds them
    /// (`pairs` holds those met, never `None`).
    by_name: bool,
    /// Two open cells nothing bounds are identical outside a function
    /// type too, and go on `live` ([`lower_collapses`]).
    unbounded: bool,
    /// A scheme's cell pairs with a live one too ([`renames`]).
    across: bool,
    /// Where the live cells paired go.
    live: Option<&'a mut Welds>,
    /// Names are compared by what they mean here, else by their cells.
    env: Option<&'a Env>,
    /// Inside a function type: the cells paired so far, by address.
    pairs: Option<SmallVec<[(usize, usize); 4]>>,
    /// The nodes found identical outside a function type.
    found: &'a mut NodePairs,
    /// Those found inside the function type being compared, which hold
    /// only under its pairing of cells.
    found_in_fn: NodePairs,
    /// Two distinct cells were paired.
    renamed: bool,
    /// Where the verdicts of pairs whose walk read no cell go: a plain
    /// walk's alone ([`IdenticalMemo`]), whose every cell read is a
    /// [`Self::same_inner`] of a variable or of a name.
    settled: Option<&'a mut Settled>,
    /// The pair being walked has read a cell so far.
    read_cell: bool,
}

impl<'a> Identity<'a> {
    fn new(live: Option<&'a mut Welds>, found: &'a mut NodePairs) -> Self {
        Self {
            strict: false,
            sets: false,
            in_order: false,
            by_name: false,
            unbounded: false,
            across: false,
            live,
            env: None,
            pairs: None,
            found,
            found_in_fn: NodePairs::default(),
            renamed: false,
            settled: None,
            read_cell: false,
        }
    }

    fn same(&mut self, t0: &Type, t1: &Type) -> bool {
        #[cfg(test)]
        IDENTITY_STEPS.with(|n| n.set(n.get() + 1));
        // one shared node is identical to itself, however large
        if t0.node_key().is_some_and(|a| t1.node_key() == Some(a)) {
            return true;
        }
        let pair = match (t0, t1) {
            (Type::Ref(r0), Type::Ref(r1))
                if self.settled.is_some()
                    && !r0.params.is_empty()
                    && !r1.params.is_empty() =>
            {
                t0.node_key().zip(t1.node_key())
            }
            _ => self.found.key(t0, t1),
        };
        if let Some(pair) = pair {
            if let Some(same) = self.settled.as_ref().and_then(|s| s.get(pair)) {
                return same;
            }
            if self.found.contains(pair)
                || (self.pairs.is_some() && self.found_in_fn.contains(pair))
            {
                self.read_cell = true;
                return true;
            }
        }
        let above = mem::replace(&mut self.read_cell, false);
        let same = ensure_sufficient(|| self.same_inner(t0, t1));
        let read_cell = self.read_cell;
        self.read_cell |= above;
        if let Some(pair) = pair {
            match (&mut self.settled, self.pairs.is_some()) {
                (Some(settled), _) if !read_cell => settled.insert(pair, t0, t1, same),
                _ if !same => (),
                (_, false) => self.found.insert(pair, t0, t1),
                (_, true) => self.found_in_fn.insert(pair, t0, t1),
            }
        }
        same
    }

    fn all(&mut self, a: &[Type], b: &[Type]) -> bool {
        a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| self.same(a, b))
    }

    /// Whether the unions `a` and `b` hold the same members ([`Leaves`]):
    /// an open cell is one only with itself, any other member is one with
    /// another only by a walk of the two.
    fn same_members(&mut self, a: &[Type], b: &[Type]) -> bool {
        let (a, b) = (Leaves::of(a), Leaves::of(b));
        a.cells[..] == b.cells[..]
            && match self.in_order {
                true => self.all(&a.rest, &b.rest),
                false => self.within(&a.rest, &b.rest) && self.within(&b.rest, &a.rest),
            }
    }

    /// Whether each of `a` is one of `b`.
    fn within(&mut self, a: &[Type], b: &[Type]) -> bool {
        a.iter().all(|m| b.iter().any(|n| self.same(m, n)))
    }

    fn cells(&mut self, a: &TVar, b: &TVar) -> bool {
        if a.same_cell(b) {
            return true;
        }
        let (x, y) = (a.cell_addr(), b.cell_addr());
        let Some(pairs) = &mut self.pairs else {
            return match &mut self.live {
                Some(live)
                    if self.unbounded
                        && a.unbounded()
                        && b.unbounded()
                        && !(a.earlier_task() && b.earlier_task()) =>
                {
                    live.push((a.clone(), b.clone()));
                    self.renamed = true;
                    true
                }
                _ => false,
            };
        };
        if self.by_name {
            if a.name != b.name {
                return false;
            }
            if !pairs.contains(&(x, y)) {
                pairs.push((x, y));
                let (ca, cb) = (a.cell_constraints(), b.cell_constraints());
                return self.all(&ca, &cb);
            }
            return true;
        }
        if !self.across && a.level().is_generic() != b.level().is_generic() {
            return false;
        }
        match pairs.iter().find(|(p, q)| *p == x || *q == y) {
            Some(&(p, q)) => p == x && q == y,
            None => {
                pairs.push((x, y));
                if let Some(live) = &mut self.live
                    && !a.level().is_generic()
                {
                    if a.is_rigid() && b.is_rigid() {
                        return false;
                    }
                    live.push((a.clone(), b.clone()))
                }
                self.renamed = true;
                let (ca, cb) = (a.cell_constraints(), b.cell_constraints());
                if !self.across {
                    return self.all(&ca, &cb);
                }
                let kept = self.pairs.as_ref().map_or(0, |p| p.len());
                if !self.all(&ca, &cb)
                    && let Some(p) = &mut self.pairs
                {
                    p.truncate(kept)
                }
                true
            }
        }
    }

    #[inline(never)]
    fn same_inner(&mut self, t0: &Type, t1: &Type) -> bool {
        self.read_cell |= matches!(t0, Type::TVar(_)) || matches!(t1, Type::TVar(_));
        if self.sets {
            match (members_of(t0), members_of(t1)) {
                (None, None) => (),
                (s0, s1) => {
                    let s0 = s0.as_deref().unwrap_or(slice::from_ref(t0));
                    let s1 = s1.as_deref().unwrap_or(slice::from_ref(t1));
                    return self.same_members(s0, s1);
                }
            }
        }
        match (t0, t1) {
            (Type::TVar(a), Type::TVar(b)) => match (a.binding(), b.binding()) {
                (Some(x), Some(y)) => a.same_cell(b) || self.same(&x, &y),
                (None, None) => self.cells(a, b),
                (None, Some(y)) => matches!(y, Type::TVar(_)) && self.same(t0, &y),
                (Some(x), None) => matches!(x, Type::TVar(_)) && self.same(&x, t1),
            },
            (Type::TVar(a), t) | (t, Type::TVar(a)) => {
                a.binding().is_some_and(|x| self.same(&x, t))
            }
            (Type::Bottom, Type::Bottom)
            | (Type::Any, Type::Any)
            | (Type::Hole, Type::Hole)
            | (Type::Bound(Bound::Concrete), Type::Bound(Bound::Concrete))
            | (Type::Bound(Bound::Data), Type::Bound(Bound::Data))
            | (Type::Bound(Bound::Function), Type::Bound(Bound::Function))
            | (Type::Bound(Bound::OneNumber), Type::Bound(Bound::OneNumber))
            | (Type::Bound(Bound::NoError), Type::Bound(Bound::NoError))
            | (Type::Bound(Bound::Discernible), Type::Bound(Bound::Discernible))
            | (Type::Bound(Bound::Ordered), Type::Bound(Bound::Ordered))
            | (Type::Bound(Bound::JsonData), Type::Bound(Bound::JsonData))
            | (Type::Bound(Bound::TomlData), Type::Bound(Bound::TomlData))
            | (Type::Bound(Bound::Singleton), Type::Bound(Bound::Singleton)) => true,
            (Type::App(c0, a0), Type::App(c1, a1)) => {
                self.same(c0, c1) && self.same(a0, a1)
            }
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (
                Type::Abstract { id: i0, params: p0 },
                Type::Abstract { id: i1, params: p1 },
            ) => i0 == i1 && self.all(p0, p1),
            (Type::Ref(r0), Type::Ref(r1)) => {
                let same = match self.env {
                    Some(env) => r0.same_definition(r1, env),
                    None => r0.same_settled_definition(r1).unwrap_or_else(|| {
                        self.read_cell = true;
                        r0.same_filled_definition(r1)
                    }),
                };
                same && self.all(&r0.params, &r1.params)
            }
            (Type::Set(s0), Type::Set(s1)) => self.all(s0, s1),
            (Type::Error(a), Type::Error(b))
            | (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b)) => self.same(a, b),
            (Type::ByRef(m0, a), Type::ByRef(m1, b)) => m0 == m1 && self.same(a, b),
            (Type::Map(m0), Type::Map(m1)) => {
                self.same(&m0.key, &m1.key) && self.same(&m0.value, &m1.value)
            }
            (Type::Tuple(a), Type::Tuple(b)) => self.all(a, b),
            (Type::Struct(a), Type::Struct(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|((n0, x, _), (n1, y, _))| n0 == n1 && self.same(x, y))
            }
            (Type::Variant(tg0, a, _), Type::Variant(tg1, b, _)) => {
                tg0 == tg1 && self.all(a, b)
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                let outer = !self.strict && self.pairs.is_none();
                if outer {
                    self.pairs = Some(SmallVec::new());
                }
                let r = f0.args.len() == f1.args.len()
                    && f0.quantifiers.len() == f1.quantifiers.len()
                    && (!self.by_name || f0.quantifiers == f1.quantifiers)
                    && f0
                        .args
                        .iter()
                        .zip(f1.args.iter())
                        .all(|(a, b)| a.kind == b.kind && self.same(&a.typ, &b.typ))
                    && match (&f0.vargs, &f1.vargs) {
                        (None, None) => true,
                        (Some(a), Some(b)) => self.same(a, b),
                        _ => false,
                    }
                    && self.same(&f0.rtype, &f1.rtype)
                    && self.same(&f0.throws, &f1.throws);
                if outer {
                    self.pairs = None;
                    self.found_in_fn.clear();
                }
                r
            }
            _ => false,
        }
    }
}

/// Two same-named refs to one definition with identical params.
fn same_ref(env: &Env, r0: &TypeRef, r1: &TypeRef) -> bool {
    r0.same_definition(r1, env)
        && r0.params.len() == r1.params.len()
        && r0.params.iter().zip(r1.params.iter()).all(|(a, b)| union_identical(a, b))
}

/// How many parts a union of alternatives meets within a member: an
/// array's, a list's or a reference's element, a map's key and value, a
/// tuple's, a struct's or a variant's positions, in [`Type::cow_children`]
/// order.
fn part_count(t: &Type) -> usize {
    match t {
        Type::Array(_) | Type::List(_) | Type::ByRef(..) => 1,
        Type::Map(_) => 2,
        Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.len(),
        Type::Struct(fs) => fs.len(),
        _ => 0,
    }
}

/// Part `p` of `t`, `p < part_count(t)`.
fn part(t: &Type, p: usize) -> &Type {
    match t {
        Type::Array(e) | Type::List(e) | Type::ByRef(_, e) => e,
        Type::Map(m) if p == 0 => &m.key,
        Type::Map(m) => &m.value,
        Type::Tuple(ts) | Type::Variant(_, ts, _) => &ts[p],
        Type::Struct(fs) => &fs[p].1,
        _ => unreachable!("a type with no parts"),
    }
}

/// The members of the union `t` stands for through its bindings.
fn members_of(t: &Type) -> Option<Arc<[Type]>> {
    t.with_deref(|t| match t {
        Some(Type::Set(ms)) => Some(ms.clone()),
        _ => None,
    })
}

/// The members of a union ([`members_of`]), nested unions flattened:
/// its open cells by address, sorted, each once, and the rest.
struct Leaves {
    cells: LPooled<Vec<usize>>,
    rest: LPooled<Vec<Type>>,
}

impl Leaves {
    fn of(ms: &[Type]) -> Self {
        let mut l = Self { cells: LPooled::take(), rest: LPooled::take() };
        l.add(ms);
        l.cells.sort_unstable();
        l.cells.dedup();
        l
    }

    fn add(&mut self, ms: &[Type]) {
        for m in ms {
            #[cfg(test)]
            IDENTITY_STEPS.with(|n| n.set(n.get() + 1));
            match (members_of(m), m) {
                (Some(ms), _) => ensure_sufficient(|| self.add(&ms)),
                (None, Type::TVar(tv)) if let Some(c) = tv.open_cell() => {
                    self.cells.push(c.cell_addr())
                }
                (None, _) => self.rest.push(m.clone()),
            }
        }
    }
}

/// The members of `t` ([`members_of`]), nested unions flattened, or `t`
/// where it is no union. `shape` takes the tree in preorder: a union's
/// member count, `None` for a member.
fn union_members(
    t: &Type,
    out: &mut LPooled<Vec<Type>>,
    shape: &mut LPooled<Vec<Option<usize>>>,
) {
    ensure_sufficient(|| match members_of(t) {
        Some(ms) => {
            shape.push(Some(ms.len()));
            ms.iter().for_each(|m| union_members(m, out, shape))
        }
        None => {
            shape.push(None);
            out.push(t.clone())
        }
    })
}

/// The type [`union_members`] took apart into `shape`, each member taken
/// from `met`, in order.
fn rebuilt(
    shape: &mut impl Iterator<Item = Option<usize>>,
    met: &mut impl Iterator<Item = Type>,
) -> Type {
    ensure_sufficient(|| match shape.next().expect("a shape for each alternative") {
        Some(n) => Type::Set(Arc::from_iter((0..n).map(|_| rebuilt(shape, met)))),
        None => met.next().expect("a met member for each member"),
    })
}

// CR claude for claude: [perf/medium] A cell written between two visits of one pair of
// shared nodes makes every Meets entry stale, so alternatives whose shared parts sit on
// both sides of a function part still meet once per path. `let x{i} = (x{i-1}, (|q| q,
// x), x{i-1}); let y{i} = (y{i-1}, (|q| q, y), y{i-1});` over `x0 = (|q| q, x)`, `y0 =
// (|q| q, y)` (`x: i64`, `y: Any`), then `let r = select b { true => x{n}, false =>
// y{n} }`: --check (--no-cache, instructions:u, an empty program 0.75G) 1.19/4.43/48.59G
// at 10/12/14 links on r24-f at 922a10ba6 (base-r24 4.38G at 12). A struct link `{a:
// x{i-1}, b: |q| q, c: x{i-1}}`, whose fields sort the function between, 1.17/4.34G at
// 10/12. The function part last, `(x{i-1}, x{i-1}, (|q| q, x))`, takes 0.79G at 12, and
// a middle part with no function, `(1, x)`, 0.76G. Meets::get reads an entry only while
// cell_writes() is what it was when the entry was made, and the meet of the middle
// part's functions writes cells (meet_open_fns binds a fresh instance's cells and the
// occurrences' own), so the entry the first part made for (x{i-1}, y{i-1}) is stale at
// the third. Unit twin: a triple DAG with a fresh `fn(q: 'c) -> 'c` pair between the
// shared parts takes 5118 meets and 24564 cell writes at 10 links; a tuple of leaves
// there, 130 meets. A fix needs a test finer than the thread's write count: an entry
// stands while no cell its alternatives reach was written (as Closure and
// probe_standing let a contains probe verdict stand). probes:
// ~/ptmp/cr29/scratch/r24/24f/review/{m10,m12,m14,s10,s12,t12,mn12}.gx, gen3.py
// (meet-memo-writes-01)
/// The meets of one [`Type::meet_alternatives`] walk past
/// [`NodePairs::EAGER`] steps, by the nodes the alternatives hold (a bound
/// cell as the node it holds), each with the [`cell_writes`] it was made
/// at: alternatives over shared nodes meet once while no cell is written,
/// and the members rebuilt of them share what they met.
struct Meets {
    steps: u32,
    found: Lazy<AHashMap<MeetKey, Meet>>,
}

/// The nodes alternatives hold and the alternative each member is of.
type MeetKey = (SmallVec<[NodeKey; 4]>, SmallVec<[u32; 4]>);

/// A meet [`Meets`] keeps: each alternative's met type, `None` where the
/// meet left it as it stands, kept with the nodes its key reads.
struct Meet {
    met: Option<SmallVec<[Option<Type>; 4]>>,
    writes: u64,
    _nodes: SmallVec<[Type; 4]>,
}

impl Meets {
    fn new() -> Self {
        Meets { steps: 0, found: Lazy::new() }
    }

    /// The key of the alternatives `ts` and the nodes it reads, once the
    /// walk has grown past [`NodePairs::EAGER`]; `None` where one holds
    /// no node.
    fn key(
        &mut self,
        ts: &[&Type],
        tags: &[u32],
    ) -> Option<(MeetKey, SmallVec<[Type; 4]>)> {
        if self.steps < NodePairs::EAGER {
            self.steps += 1;
            return None;
        }
        if !memo_on() {
            return None;
        }
        let mut key = SmallVec::new();
        let mut nodes = SmallVec::new();
        for t in ts {
            let (k, n) =
                t.with_deref(|t| t.and_then(|t| Some((t.node_key()?, t.clone()))))?;
            key.push(k);
            nodes.push(n);
        }
        Some(((key, tags.iter().copied().collect()), nodes))
    }

    /// The meet of `ts` found under `key` while no cell has been written
    /// since: an alternative it left as it stands is the one in `ts`.
    fn get(&self, key: &MeetKey, ts: &[&Type]) -> Option<Option<LPooled<Vec<Type>>>> {
        let m = self.found.get()?.get(key).filter(|m| m.writes == cell_writes())?;
        Some(m.met.as_ref().map(|met| {
            ts.iter()
                .zip(met.iter())
                .map(|(t, m)| m.as_ref().unwrap_or(*t).clone())
                .collect()
        }))
    }

    fn insert(
        &mut self,
        key: MeetKey,
        nodes: SmallVec<[Type; 4]>,
        ts: &[&Type],
        met: Option<&LPooled<Vec<Type>>>,
    ) {
        let met = met.map(|met| {
            ts.iter()
                .zip(met.iter())
                .map(|(t, m)| {
                    (m.shared_identity() != t.shared_identity()).then(|| m.clone())
                })
                .collect()
        });
        let m = Meet { met, writes: cell_writes(), _nodes: nodes };
        self.found.get_mut().insert(key, m);
    }
}

/// The one alternative `ts` are, `⊥` aside: the same node, however many
/// times it stands. It joins nothing, so a union it holds keeps its
/// members as they stand.
fn sole<'a>(ts: &[&'a Type]) -> Option<&'a Type> {
    let mut held = ts.iter().copied().filter(|t| !matches!(t, Type::Bottom));
    let one = held.next()?;
    let node = one.node_key();
    held.all(|t| node.is_some() && t.node_key() == node).then_some(one)
}

/// Whether every node `keys` names stands somewhere in `t`, through its
/// bindings.
pub(super) fn holds_node(t: &Type, keys: &[NodeKey]) -> bool {
    fn walk(t: &Type, keys: &[NodeKey], found: &mut u64, seen: &mut AddrSet) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if let Some(b) = tv.binding() {
                    walk(&b, keys, found, seen)
                }
            }
            t => {
                if let Some(a) = node_addr(t)
                    && !seen.insert(a)
                {
                    return;
                }
                if let Some(k) = t.node_key()
                    && let Some(i) = keys.iter().position(|n| *n == k)
                {
                    *found |= 1 << i
                }
                t.for_each_child(&mut |c| walk(c, keys, found, seen));
            }
        })
    }
    assert!(keys.len() <= 64);
    let mut found = 0;
    walk(t, keys, &mut found, &mut LPooled::take());
    found == (u64::MAX >> (64 - keys.len()))
}

/// The alternatives `ts` are members of, `tags[i]` the one `ts[i]` is of,
/// each of two or more members a union of them.
fn alternatives(ts: &[&Type], tags: &[u32]) -> SmallVec<[Type; 4]> {
    let mut by_tag: SmallVec<[(u32, SmallVec<[Type; 2]>); 4]> = SmallVec::new();
    for (t, tag) in ts.iter().zip(tags) {
        match by_tag.iter_mut().find(|(g, _)| g == tag) {
            Some((_, ms)) => ms.push((*t).clone()),
            None => by_tag.push((*tag, smallvec![(*t).clone()])),
        }
    }
    by_tag
        .into_iter()
        .map(|(_, mut ms)| match ms.len() {
            1 => ms.remove(0),
            _ => Type::Set(Arc::from_iter(ms)),
        })
        .collect()
}

/// The two-member union of `a` and `b`.
fn pair(a: &Type, b: &Type) -> Type {
    Type::Set(Arc::from_iter([a.clone(), b.clone()]))
}

/// Whether `f` holds an open cell a definition or the top level owns,
/// and one of a scheme, which no definition owns.
fn open_cells(f: &Type) -> (bool, bool) {
    let (mut live, mut generic) = (false, false);
    find_open_cell(f, true, &mut |tv| {
        if tv.level().is_generic() {
            generic = true
        } else {
            live = true
        }
        live && generic
    });
    (live, generic)
}

/// Make the function types `a` and `b` one in `hist`'s walk: a renaming's
/// cells weld pair by pair ([`renaming_linked`]), which decides no member
/// of a union the signature holds over its own variables, and any other
/// two hold each other.
fn fns_join(env: &Env, hist: &mut ContainsHist, a: &Type, b: &Type) -> Result<bool> {
    if renaming_linked(env, a, b, true) {
        return Ok(true);
    }
    let commit = ContainsFlags::Commit.into();
    Ok(a.contains_int(commit, env, hist, b)? && b.contains_int(commit, env, hist, a)?)
}

/// Whether the function types `a` and `b` meet: one is a renaming of the
/// other, or each holds the other, by a probe and then by a commit on
/// copies, since a probe judges each part apart.
fn fns_meet(env: &Env, a: &Type, b: &Type) -> Result<bool> {
    if renaming_linked(env, a, b, false) {
        return Ok(true);
    }
    let probe = BitFlags::empty();
    Ok(a.contains_with_flags(probe, env, b)?
        && b.contains_with_flags(probe, env, a)?
        && on_copies(None, [a, b], |[a, b], trial| fns_join(env, trial, &a, &b))?)
}

/// Whether `a` meets `b` and `c` meets `d` at once, on copies that share
/// their cells ([`fns_meet`]).
fn fns_meet_both(env: &Env, [a, b]: [&Type; 2], [c, d]: [&Type; 2]) -> Result<bool> {
    on_copies(None, [a, b, c, d], |[a, b, c, d], trial| {
        Ok(fns_join(env, trial, &a, &b)? && fns_join(env, trial, &c, &d)?)
    })
}

/// The refusal of a function `f` among alternatives that meets `a` and
/// meets `b`, and is not one type with both at once.
fn two_meets(f: &Type, a: &Type, b: &Type) -> anyhow::Error {
    let mut known = LPooled::take();
    let [f, a, b] = [f, a, b]
        .map(|t| t.reset_tvars_int(&mut known, Fresh::Copy).unwrap_or_else(|| t.clone()));
    format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
        anyhow!(
            "the function {} could be {} or {}, two functions it stands beside, but \
             not both, and the check picks neither by order: write the part of its \
             type that tells them apart (`|x: T| -> R throws E ..`), so it is one of \
             them or neither",
            Brief(&f),
            Brief(&a),
            Brief(&b)
        )
    })
}

/// How a union treats two applications of one definition that differ
/// in a parameter the body does not hold linearly.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Merge {
    Exact,
    Supertype,
}

impl Type {
    /// Same-named refs `self` (`t0`) and `t` (`t1`): identical params are
    /// one member; one differing param merges param-wise when the merge
    /// holds both (a probe, so only a covariant param merges) and, for an
    /// exact union, the param stands once in the body outside any
    /// collection, so the merge holds no mixed value; more than one would
    /// invent pairs, so they stay two members.
    fn union_ref_params(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        welds: &mut Welds,
        t0: &TypeRef,
        t1: &TypeRef,
        t: &Self,
    ) -> Result<Self> {
        let mut kept = Welds::new();
        let mut differ = t0
            .params
            .iter()
            .zip(t1.params.iter())
            .enumerate()
            .filter(|(_, (a, b))| !union_collapses(a, b, &mut kept));
        let (i, (p0, p1)) = match (differ.next(), differ.next()) {
            (None, _) => {
                welds.extend(kept.drain(..));
                return Ok(self.clone());
            }
            (Some(d), None) => d,
            (Some(_), Some(_)) => return Ok(pair(self, t)),
        };
        if merge == Merge::Exact
            && !t0.resolve_in(env).is_some_and(|d| d.param_is_linear(i))
        {
            return Ok(pair(self, t));
        }
        let mut params: LPooled<Vec<Type>> = t0.params.iter().cloned().collect();
        params[i] = p0.union_int(env, hist, merge, &mut kept, p1)?;
        let merged =
            Type::Ref(Arc::new(t0.with_params(Arc::from_iter(params.drain(..)))));
        let probe = BitFlags::empty();
        if merged.contains_with_flags(probe, env, self)?
            && merged.contains_with_flags(probe, env, t)?
        {
            welds.extend(kept.drain(..));
            Ok(merged)
        } else {
            Ok(pair(self, t))
        }
    }

    fn union_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        welds: &mut Welds,
        t: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| self.union_inner(env, hist, merge, welds, t))
    }

    fn union_inner(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        welds: &mut Welds,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (Type::Bottom, t) | (t, Type::Bottom) => Ok(t.clone()),
            (Type::Primitive(p), t) | (t, Type::Primitive(p)) if p.is_empty() => {
                Ok(t.clone())
            }
            (Type::Ref(t0), Type::Ref(t1))
                if t0.same_definition(t1, env) && t0.params.len() == t1.params.len() =>
            {
                t0.hold_bounds_with(t1, env, true)?;
                self.union_ref_params(env, hist, merge, welds, t0, t1, t)
            }
            (tr @ Type::Ref(_), t) | (t, tr @ Type::Ref(_)) => {
                let key = if matches!(self, Type::Ref(_)) {
                    (hist.ref_id(tr, env), hist.ref_id(t, env))
                } else {
                    (hist.ref_id(t, env), hist.ref_id(tr, env))
                };
                if let Some(u) = hist.get(&key) {
                    return Ok(u.clone());
                }
                let e = tr.lookup_ref(env)?;
                hist.insert(key, tr.clone());
                let r = if matches!(self, Type::Ref(_)) {
                    e.union_int(env, hist, merge, welds, t)
                } else {
                    t.union_int(env, hist, merge, welds, &e)
                };
                hist.remove(&key);
                r
            }
            (t0 @ Type::Abstract { .. }, t1 @ Type::Abstract { .. })
                if union_collapses(t0, t1, welds) =>
            {
                Ok(self.clone())
            }
            (t0 @ Type::Abstract { .. }, t1) | (t0, t1 @ Type::Abstract { .. }) => {
                Ok(pair(t0, t1))
            }
            (Type::Any, _) | (_, Type::Any) => Ok(Type::Any),
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Bound(_), _)
            | (_, Type::Bound(_)) => {
                if self == t {
                    Ok(self.clone())
                } else {
                    Ok(pair(self, t))
                }
            }
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                let mut s = *s0;
                s.insert(*s1);
                Ok(Type::Primitive(s))
            }
            (Type::Primitive(p), Type::Array(_))
            | (Type::Array(_), Type::Primitive(p))
                if p.contains(Typ::Array) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Primitive(p), Type::Map { .. })
            | (Type::Map { .. }, Type::Primitive(p))
                if p.contains(Typ::Map) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Primitive(p), Type::Error(_))
            | (Type::Error(_), Type::Primitive(p))
                if p.contains(Typ::Error) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Error(e0), Type::Error(e1)) => {
                Ok(Type::Error(Arc::new(e0.union_int(env, hist, merge, welds, e1)?)))
            }
            (Type::Set(s0), Type::Set(s1)) => Ok(Type::Set(Arc::from_iter(
                s0.iter().cloned().chain(s1.iter().cloned()),
            ))),
            (Type::Set(s), t) | (t, Type::Set(s)) => Ok(Type::Set(Arc::from_iter(
                s.iter().cloned().chain(iter::once(t.clone())),
            ))),
            (u @ Type::Variant(tg0, t0, at), t @ Type::Variant(tg1, t1, _)) => {
                // Component-wise union is exact only when at most one
                // position differs: `P(A, X) ∪ `P(B, Y) is not
                // `P([A, B], [X, Y]) (that invents `P(A, Y)).
                let differing = || {
                    t0.iter()
                        .zip(t1.iter())
                        .filter(|(a, b)| !union_collapses(a, b, &mut Welds::new()))
                        .count()
                };
                if tg0 == tg1 && t0.len() == t1.len() && differing() <= 1 {
                    let mut typs = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.union_int(env, hist, merge, welds, t1))
                        .collect::<Result<LPooled<Vec<_>>>>()?;
                    Ok(Type::Variant(tg0.clone(), Arc::from_iter(typs.drain(..)), *at))
                } else {
                    Ok(pair(u, t))
                }
            }
            (t0 @ Type::TVar(_), t1 @ Type::TVar(_))
                if union_collapses(t0, t1, welds) =>
            {
                Ok(t0.clone())
            }
            // A bound cell unions as its binding; an unbound cell
            // stays its own member.
            (Type::TVar(tv), t1) => match tv.binding() {
                Some(b) => b.union_int(env, hist, merge, welds, t1),
                None => Ok(pair(self, t1)),
            },
            (t0, Type::TVar(tv)) => match tv.binding() {
                Some(b) => t0.union_int(env, hist, merge, welds, &b),
                None => Ok(pair(t0, t)),
            },
            // Everything left of one variant is kept once if identical,
            // else as two members.
            (t0, t1)
                if std::mem::discriminant(t0) == std::mem::discriminant(t1)
                    && union_collapses(t0, t1, welds) =>
            {
                Ok(t0.clone())
            }
            (t0, t1) => Ok(pair(t0, t1)),
        }
    }

    /// The union of `ts`, which may widen an application's parameter
    /// that recurses or sits under a collection (`L<i64> ∪ L<string>` is
    /// `L<[i64, string]>`, which holds mixed lists): a type for the values
    /// of `ts`.
    pub fn union(env: &Env, ts: &[&Type]) -> Result<Self> {
        Self::union_with(env, Merge::Supertype, ts)
    }

    /// The union of `ts`, admitting nothing none of them admits: what
    /// a set of patterns covers.
    pub fn union_exact(env: &Env, ts: &[&Type]) -> Result<Self> {
        Self::union_with(env, Merge::Exact, ts)
    }

    /// [`Self::union_exact`] of `ts`, the members of one union: two of them
    /// that differ by a renaming of their cells are two.
    pub fn union_exact_of_members(env: &Env, ts: &[&Type]) -> Result<Self> {
        let tags: LPooled<Vec<u32>> = ts.iter().map(|_| 0).collect();
        Self::union_tagged(env, Merge::Exact, ts, &tags)
    }

    /// [`Self::union_exact`] of the parts `ts` the alternatives of a
    /// pattern take from the members that reach them, `tags[i]` the
    /// alternative `ts[i]` is of: the parts of one alternative are members
    /// of one union, and two of them that differ by a renaming of their
    /// cells are two.
    pub fn union_exact_of_parts(env: &Env, ts: &[&Type], tags: &[u32]) -> Result<Self> {
        Self::union_tagged(env, Merge::Exact, ts, tags)
    }

    /// The union of a select's alternatives, which a call through the
    /// select must accept alike: function members meet first, and so do
    /// those in the members' parts ([`Self::meet_alternatives`]).
    pub fn union_of_alternatives(env: &Env, ts: &[&Type]) -> Result<Self> {
        Self::union_meeting(env, Merge::Supertype, ts)
    }

    /// The union of the parts `ts` one name of a pattern takes from the
    /// members and alternatives that bind it, admitting nothing none of
    /// them admits: a call through the name must accept each alike, so
    /// they meet as a select's alternatives do
    /// ([`Self::union_of_alternatives`]).
    pub fn union_exact_of_alternatives(env: &Env, ts: &[&Type]) -> Result<Self> {
        Self::union_meeting(env, Merge::Exact, ts)
    }

    fn union_meeting(env: &Env, merge: Merge, ts: &[&Type]) -> Result<Self> {
        let tags: LPooled<Vec<u32>> = (0..ts.len() as u32).collect();
        match Self::meet_alternatives(env, &mut Meets::new(), ts, &tags)? {
            None => Self::union_with(env, merge, ts),
            Some(met) => {
                let met: LPooled<Vec<&Type>> = met.iter().collect();
                Self::union_with(env, merge, &met)
            }
        }
    }

    // XCR claude for claude: [bug/low] A union that one of several alternatives already
    // holds has its own members paired where the alternatives join: two function types
    // that differ by a renaming of two undecided variables become one member and the
    // variables one cell. `type F2<'p, 'q> = [fn(q: 'p) -> i64, fn(q: 'q) -> i64]; let f
    // = |y: 'a, z: 'b, x: F2<'a, 'b>| x; let h = |u, v, w| { let r = f(u, v, w); let g =
    // select c { true => r, false => null }; (u, v) }; h(1, "s", |q: i64| q + 1)` is
    // refused, "type mismatch 'b: i64 does not contain string" at "s", naming no
    // annotation (m1.gx), where `h` without the select runs (m0.gx; one alternative is
    // its type as it stands, `sole`). Alike: `select c { true => f(u, v, w), false =>
    // f(u, v, w) }`, two nodes of one type (m2.gx); a field or a tuple position read
    // over two shapes that each hold the union (m3.gx, m4.gx); a slice pattern's
    // element (m5.gx); two arms' captures of one part (m6.gx); a bare name over the
    // whole value, `select r { k => k }` (m7.gx). Nothing wrong runs: the type left is
    // narrower than the value's. Two mechanisms, each enough alone (the unit test
    // typ::normalize::tests::a_union_of_one_alternative_keeps_its_union_of_renamings
    // fails with either's `sole` test removed): meet_alternatives flattens every
    // alternative's members into one list, where meet_open_fns and meet_parts pair two
    // members of one alternative as they pair two alternatives; and the welding
    // normalize below (normalize::Members::Welding) collapses renamings in every
    // union the result reaches, at any depth, those an alternative held before the
    // join too. What would fix it: a member meets, and collapses with, only a member
    // of another alternative, and a member two alternatives hold cell for cell is one
    // member that meets neither's neighbours: the meet carries each member's
    // alternative through meet_members and meet_parts, and the union collapses across
    // alternatives itself, its last normalize as the members stand. Not looked into:
    // which callers of the plain Type::union need the welding merge of products with
    // one differing part (normalize's Type::merge under Members::Welding), which that
    // last step would lose; census them. base-r24 alike. Found fixing
    // read-welds-data-union-01. probes: ~/ptmp/cr29/scratch/r24/24d/rwd/m{0..7}.gx
    // (alternative-union-members-pair-01)
    // 2026-11-14 claude: Review of read-welds-data-union-01: a seq step that reads an
    // outer binding holding such a union pairs it too, with no select written: 'let h =
    // |u, v, w| { let x = f(u, v, w); seq { let g = x; (u, v) } }; h(1, "s", |q: i64| q
    // + 1)' is refused, 'type mismatch i64 does not contain string', on base-r24 and the
    // lane, where the same step over a value it makes itself ('seq { let x = f(u, v, w);
    // (u, v) }') runs since that fix. The step's lowering reads its snapshot through
    // 'any(#seqpc ~! snap, snap)', two nodes of one type, m2.gx's kind. probes:
    // ~/ptmp/cr29/scratch/r24/24d/rwd-rev/seq/{s3,s4}.gx (refused), s1.gx (runs)
    // 2026-11-14 claude: Fixed: a join pairs and collapses members of different
    // alternatives only. meet_open_fns/meet_parts carry each member's alternative (tags
    // through meet_alternatives; a member two members of one other alternative could
    // meet meets neither), union_tagged gives Welds the join's alternatives so
    // union_collapses and the welding normalize keep two members of one alternative
    // apart unless identical as they stand (setops::apart), narrow_per_member tags the
    // parts by the pattern alternative (union_exact_of_parts) and a slice name over
    // members of one element union joins them as members (union_exact_of_members; an
    // or-pattern child still meets). m0..m7 and the seq step (rwd-rev s3/s4) run; a call
    // through such a union is refused naming the variables, as a call of the binding is
    // (c1/c2 ran on base-r25 by the conflation). Pins that fail without it:
    // lang::types::a_select_of_a_union_of_functions_beside_null_keeps_both_members and
    // its 7 neighbours, call_through_a_select_of_calls_each_a_union_of_functions_refused
    // (36 runs),
    // typ::normalize::tests::a_join_of_alternatives_keeps_the_union_one_of_them_holds.
    // Not done: which member of the other alternative a member pairs with still follows
    // the sort order (alternative-members-pair-order-01). Census 2967 files 0 changes;
    // guicheck 39.599G -> 39.789G; deep_nesting ok. Needs a soak.
    fn union_with(env: &Env, merge: Merge, ts: &[&Type]) -> Result<Self> {
        let tags: LPooled<Vec<u32>> = (0..ts.len() as u32).collect();
        Self::union_tagged(env, merge, ts, &tags)
    }

    /// [`Self::union_with`] of `ts`, `tags[i]` the alternative `ts[i]` is a
    /// member of: members of one alternative collapse only where
    /// identical as they stand.
    fn union_tagged(env: &Env, merge: Merge, ts: &[&Type], tags: &[u32]) -> Result<Self> {
        if let Some(one) = sole(ts) {
            return Ok(one.normalize());
        }
        let mut iter = ts.iter().copied();
        let Some(first) = iter.next() else {
            return Ok(Type::Primitive(BitFlags::empty()));
        };
        let mut hist = RefHist::new();
        let mut welds = Welds::joining(alternatives(ts, tags));
        let mut acc = first.clone();
        for t in iter {
            acc = acc.union_int(env, &mut hist, merge, &mut welds, t)?;
        }
        let acc = acc.normalize_welding(&mut welds);
        apply_welds(&mut welds);
        Ok(acc)
    }

    /// [`Self::meet_members`] over the members of `ts`, an alternative
    /// that is a union standing for its members ([`union_members`]), each
    /// union rebuilt of its met members: a call through the union may be
    /// any member's; alternatives over nodes `meets` has met take that
    /// meet ([`Meets`]). One alternative ([`sole`]) meets nothing. `None`
    /// when no member changes.
    fn meet_alternatives(
        env: &Env,
        meets: &mut Meets,
        ts: &[&Type],
        tags: &[u32],
    ) -> Result<Option<LPooled<Vec<Type>>>> {
        if sole(ts).is_some() {
            return Ok(None);
        }
        #[cfg(test)]
        MEET_STEPS.with(|n| n.set(n.get() + 1));
        let Some((key, nodes)) = meets.key(ts, tags) else {
            return Self::meet_unions(env, meets, ts, tags);
        };
        if let Some(met) = meets.get(&key, ts) {
            return Ok(met);
        }
        let met = Self::meet_unions(env, meets, ts, tags)?;
        meets.insert(key, nodes, ts, met.as_ref());
        Ok(met)
    }

    fn meet_unions(
        env: &Env,
        meets: &mut Meets,
        ts: &[&Type],
        tags: &[u32],
    ) -> Result<Option<LPooled<Vec<Type>>>> {
        ensure_sufficient(|| {
            if !ts.iter().any(|t| members_of(t).is_some()) {
                return Self::meet_members(env, meets, ts, tags);
            }
            let mut flat: LPooled<Vec<Type>> = LPooled::take();
            let mut shape: LPooled<Vec<Option<usize>>> = LPooled::take();
            let mut flat_tags: LPooled<Vec<u32>> = LPooled::take();
            for (t, tag) in ts.iter().zip(tags) {
                union_members(t, &mut flat, &mut shape);
                flat_tags.resize(flat.len(), *tag);
            }
            let flat: LPooled<Vec<&Type>> = flat.iter().collect();
            let Some(mut met) = Self::meet_members(env, meets, &flat, &flat_tags)? else {
                return Ok(None);
            };
            let (mut shape, mut met) = (shape.drain(..), met.drain(..));
            Ok(Some(ts.iter().map(|_| rebuilt(&mut shape, &mut met)).collect()))
        })
    }

    /// [`Self::meet_open_fns`] over `ts`, then over the parts of each set
    /// of members one value may be ([`Type::shares_constructor`]), whose
    /// parts ([`part`]) pair by position. `None` when no member changes.
    fn meet_members(
        env: &Env,
        meets: &mut Meets,
        ts: &[&Type],
        tags: &[u32],
    ) -> Result<Option<LPooled<Vec<Type>>>> {
        let fns = Self::meet_open_fns(env, ts, tags)?;
        let parts = match &fns {
            Some(met) => {
                let met: LPooled<Vec<&Type>> = met.iter().collect();
                Self::meet_parts(env, meets, &met, tags)?
            }
            None => Self::meet_parts(env, meets, ts, tags)?,
        };
        Ok(parts.or(fns))
    }

    /// The members of `ts` of one shape meet part by part
    /// ([`Self::meet_alternatives`]). `None` when no member changes.
    fn meet_parts(
        env: &Env,
        meets: &mut Meets,
        ts: &[&Type],
        tags: &[u32],
    ) -> Result<Option<LPooled<Vec<Type>>>> {
        let members: SmallVec<[Option<Type>; 4]> = ts
            .iter()
            .map(|t| t.with_deref(|t| t.filter(|t| part_count(t) > 0).cloned()))
            .collect();
        let mut grouped: SmallVec<[bool; 4]> = smallvec![false; members.len()];
        let mut out: Option<LPooled<Vec<Type>>> = None;
        for (i, head) in members.iter().enumerate() {
            let Some(head) = head else { continue };
            if grouped[i] {
                continue;
            }
            let group: SmallVec<[usize; 4]> = (i..members.len())
                .filter(|j| {
                    members[*j].as_ref().is_some_and(|m| head.shares_constructor(m))
                })
                .collect();
            for j in group.iter() {
                grouped[*j] = true;
            }
            let group_tags: SmallVec<[u32; 4]> = group.iter().map(|j| tags[*j]).collect();
            if group_tags.iter().all(|t| *t == group_tags[0]) {
                continue;
            }
            let mut rebuilt: SmallVec<[Type; 4]> =
                group.iter().filter_map(|j| members[*j].clone()).collect();
            let mut changed = false;
            for p in 0..part_count(head) {
                let met = {
                    let at: SmallVec<[&Type; 4]> =
                        rebuilt.iter().map(|m| part(m, p)).collect();
                    Self::meet_alternatives(env, meets, &at, &group_tags)?
                };
                let Some(mut met) = met else { continue };
                for (m, t) in rebuilt.iter_mut().zip(met.drain(..)) {
                    let mut k = 0;
                    let mut t = Some(t);
                    *m = m
                        .cow_children(&mut |_| {
                            k += 1;
                            if k - 1 == p { t.take() } else { None }
                        })
                        .unwrap_or_else(|| m.clone());
                }
                changed = true;
            }
            if changed {
                let out =
                    out.get_or_insert_with(|| ts.iter().map(|t| (*t).clone()).collect());
                for (j, m) in group.iter().zip(rebuilt.drain(..)) {
                    out[*j] = m;
                }
            }
        }
        Ok(out)
    }

    /// Function members that meet both ways, one with open cells, are one
    /// member: calling either takes the same arguments, and the union's
    /// call needs one signature. A member with a scheme's cells is an
    /// instance of the first member it meets; any other is one type with
    /// every member it meets, since the meet binds its own cells as the
    /// call would bind them. A member whose own cells two members it
    /// cannot both be could decide is refused ([`two_meets`]), the member
    /// that looks for its meets and the one it finds alike: which decided
    /// them would follow the members' order. A scheme's instance that
    /// joins a scheme leaves it that instance's cells, which are no
    /// definition's, so it is a scheme's member still; one a member with
    /// cells of its own joins holds those from there on. `None` when no
    /// member changes.
    fn meet_open_fns(
        env: &Env,
        ts: &[&Type],
        tags: &[u32],
    ) -> Result<Option<LPooled<Vec<Type>>>> {
        let fns: SmallVec<[(usize, Arc<FnType>); 4]> = ts
            .iter()
            .enumerate()
            .filter_map(|(i, t)| {
                t.with_deref(|t| match t {
                    Some(Type::Fn(f)) => Some((i, f.clone())),
                    _ => None,
                })
            })
            .collect();
        if fns.len() < 2 {
            return Ok(None);
        }
        let member = |j: usize| Type::Fn(fns[j].1.clone());
        let instance = |j: usize| {
            Type::Fn(Arc::new(
                fns[j].1.reset_tvars_int(&mut LPooled::take(), Fresh::Generic),
            ))
        };
        // `group[k]` names the member each of `fns` met into, and `base`
        // holds that group's last met type
        let mut group: SmallVec<[usize; 4]> = (0..fns.len()).collect();
        let mut base: SmallVec<[Option<Arc<FnType>>; 4]> = smallvec![None; fns.len()];
        let mut met: SmallVec<[bool; 4]> = smallvec![false; fns.len()];
        let mut scheme: SmallVec<[bool; 4]> =
            (0..fns.len()).map(|j| open_cells(&member(j)) == (false, true)).collect();
        let tag = |j: usize| tags[fns[j].0];
        let repeated = (1..fns.len()).any(|j| (0..j).any(|i| tag(i) == tag(j)));
        let clash = |group: &[usize], ga: usize, gb: usize| {
            repeated
                && (0..fns.len()).any(|x| {
                    group[x] == ga
                        && (0..fns.len()).any(|y| group[y] == gb && tag(x) == tag(y))
                })
        };
        for k in 0..fns.len() {
            let own = member(k);
            if own.tvar_free() {
                continue;
            }
            let (live, generic) = match scheme[k] {
                true => (false, true),
                false => open_cells(&own),
            };
            let inst = instance(k);
            let mut meets: SmallVec<[usize; 4]> = smallvec![];
            for j in 0..fns.len() {
                if j == k
                    || met[j]
                    || group[j] == group[k]
                    || clash(&group, group[k], group[j])
                {
                    continue;
                }
                if fns_meet(env, &inst, &member(j))? {
                    meets.push(j);
                    if generic && !live {
                        break;
                    }
                }
            }
            if repeated {
                let kept: SmallVec<[usize; 4]> = meets
                    .iter()
                    .copied()
                    .filter(|j| meets.iter().all(|i| i == j || tag(*i) != tag(*j)))
                    .collect();
                let mut unambiguous = SmallVec::<[usize; 4]>::new();
                for j in kept {
                    let mut twin = false;
                    for x in 0..fns.len() {
                        if x != k
                            && x != j
                            && tag(x) == tag(k)
                            && !member(x).tvar_free()
                            && fns_meet(env, &instance(x), &member(j))?
                        {
                            twin = true;
                            break;
                        }
                    }
                    if !twin {
                        unambiguous.push(j)
                    }
                }
                meets = unambiguous;
            }
            let Some(first) = meets.first().map(|j| member(*j)) else { continue };
            let before = (!generic && meets.len() > 1).then(|| {
                let mut known = LPooled::take();
                let mut copy = |t: &Type| {
                    t.reset_tvars_int(&mut known, Fresh::Copy)
                        .unwrap_or_else(|| t.clone())
                };
                let was = copy(&inst);
                let stood: SmallVec<[Type; 4]> =
                    meets.iter().map(|j| copy(&member(*j))).collect();
                (was, stood)
            });
            if generic {
                let again = instance(k);
                for j in &meets[1..] {
                    let g = member(*j);
                    if !fns_meet_both(env, [&inst, &first], [&again, &g])? {
                        return Err(two_meets(&inst, &first, &g));
                    }
                }
                meets.truncate(1);
            }
            for (n, j) in meets.iter().copied().enumerate() {
                let g = member(j);
                if let Some((was, stood)) = &before
                    && n > 0
                    && !fns_meet(env, &inst, &g)?
                {
                    let mut other = &stood[0];
                    for earlier in &stood[..n] {
                        if !fns_meet_both(env, [was, earlier], [was, &stood[n]])? {
                            other = earlier;
                            break;
                        }
                    }
                    return Err(two_meets(was, other, &stood[n]));
                }
                if !scheme[j] && open_cells(&g).0 {
                    let again = instance(j);
                    for m in 0..fns.len() {
                        if m == k
                            || met[m]
                            || meets.contains(&m)
                            || group[m] == group[k]
                            || group[m] == group[j]
                            || tag(m) == tag(k)
                            || tag(m) == tag(j)
                        {
                            continue;
                        }
                        let h = member(m);
                        if fns_meet(env, &again, &h)?
                            && !fns_meet_both(env, [&inst, &g], [&again, &h])?
                        {
                            return Err(two_meets(&g, &inst, &h));
                        }
                    }
                }
                if renaming_linked(env, &inst, &g, true)
                    || (inst.contains(env, &g)? && g.contains(env, &inst)?)
                {
                    let (from, to) = (group[k], group[j]);
                    for m in group.iter_mut().filter(|m| **m == from) {
                        *m = to;
                    }
                    base[from] = None;
                    base[to] = Some(fns[j].1.clone());
                    met[k] = true;
                    scheme[j] &= scheme[k];
                }
            }
        }
        if !met.iter().any(|m| *m) {
            return Ok(None);
        }
        // one member stands for each group, and a call through it may be any
        // of its definitions; the definitions' own types are untouched
        let mut out: LPooled<Vec<Type>> = ts.iter().map(|t| (*t).clone()).collect();
        for (to, g) in base.iter().enumerate() {
            let Some(g) = g else { continue };
            let ids: SmallVec<[&LambdaIds; 4]> = fns
                .iter()
                .zip(group.iter())
                .filter(|(_, m)| **m == to)
                .map(|((_, f), _)| &f.lambda_ids)
                .collect();
            let one = Type::Fn(Arc::new(FnType {
                lambda_ids: LambdaIds::joining(&ids),
                ..(**g).clone()
            }));
            for ((i, _), _) in fns.iter().zip(group.iter()).filter(|(_, m)| **m == to) {
                out[*i] = one.clone();
            }
        }
        Ok(Some(out))
    }

    fn diff_int(&self, env: &Env, hist: &mut DiffHist, t: &Self) -> Result<Self> {
        #[cfg(test)]
        DIFF_STEPS.with(|n| n.set(n.get() + 1));
        hist.part(|hist| self.diff_part(env, hist, t))
    }

    /// [`Self::diff_int`], [`DiffHist::over_cut`] this pair's alone.
    fn diff_part(&self, env: &Env, hist: &mut DiffHist, t: &Self) -> Result<Self> {
        let Some(key) = hist.parts_key(self, t) else {
            return ensure_sufficient(|| self.diff_inner(env, hist, t));
        };
        let writes = cell_writes();
        if memo_on()
            && let Some(Kept { answer: (k, at), .. }) =
                hist.parts.get().and_then(|m| m.get(&key))
            && *at == writes
            && hist.path_clear_of(&k.reached)
        {
            let (answer, reached, over_cut) =
                (k.answer.clone(), k.reached.clone(), k.over_cut);
            hist.reached.union(&reached);
            hist.holds(over_cut);
            return Ok(answer);
        }
        let depth = hist.hist.len() + 1;
        let outer_cut = mem::replace(&mut hist.cut, usize::MAX);
        let outer_reached = mem::take(&mut hist.reached);
        let r = ensure_sufficient(|| self.diff_inner(env, hist, t));
        let cut = mem::replace(&mut hist.cut, outer_cut);
        hist.cut = hist.cut.min(cut);
        let reached = mem::replace(&mut hist.reached, outer_reached);
        hist.reached.union(&reached);
        if cut >= depth
            && cell_writes() == writes
            && let Ok(d) = &r
        {
            let done =
                (Done { answer: d.clone(), reached, over_cut: hist.over_cut }, writes);
            hist.parts.get_mut().insert(key, Kept::new(done, self, t));
        }
        r
    }

    fn diff_inner(&self, env: &Env, hist: &mut DiffHist, t: &Self) -> Result<Self> {
        match (self, t) {
            (Type::Ref(tr0), Type::Ref(tr1)) if same_ref(env, tr0, tr1) => {
                tr0.hold_bounds_with(tr1, env, true)?;
                Ok(Type::Primitive(BitFlags::empty()))
            }
            (t0 @ Type::Ref(_), t1) | (t0, t1 @ Type::Ref(_)) => {
                let key = (hist.hist.ref_id(t0, env), hist.hist.ref_id(t1, env));
                let kept = key.0.is_some() && key.1.is_some() && !keyed_loosely(self);
                let defs = def_pair(env, t0, t1);
                if kept
                    && memo_on()
                    && let Some(k) = hist.done.get().and_then(|m| m.get(&key))
                    && hist.path_clear_of(&k.answer.reached)
                {
                    let k = &k.answer;
                    let (answer, reached, over_cut) =
                        (k.answer.clone(), k.reached.clone(), k.over_cut);
                    hist.reached.union(&reached);
                    hist.holds(over_cut);
                    return Ok(answer);
                }
                if let Some((r, depth)) = hist.hist.get(&key) {
                    let r = r.clone();
                    hist.cut = hist.cut.min(*depth);
                    hist.reach(Reach::Pair(key));
                    hist.holds(CUT);
                    return Ok(r);
                }
                // a pair met again inside its own difference subtracts
                // nothing: assuming it covered would drop what it did not
                if let (Some(k), Some(defs)) = (hist.unfoldings.again(defs), defs) {
                    let depth = hist.hist.get(&k).map_or(0, |(_, d)| *d);
                    hist.cut = hist.cut.min(depth);
                    hist.reach(Reach::Defs(defs));
                    hist.holds(CUT);
                    return Ok(self.clone());
                }
                if !hist.pairs.unfold() {
                    hist.cut = 0;
                    hist.widened = true;
                    hist.holds(CUT);
                    return Ok(self.clone());
                }
                #[cfg(test)]
                DIFF_PAIRS.with(|n| n.set(n.get() + 1));
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                let depth = hist.hist.len() + 1;
                hist.hist.insert(key, (self.clone(), depth));
                hist.unfoldings.enter(defs, key);
                let outer_cut = mem::replace(&mut hist.cut, usize::MAX);
                let outer_reached = mem::take(&mut hist.reached);
                hist.reach(Reach::Pair(key));
                if let Some(defs) = defs {
                    hist.reach(Reach::Defs(defs));
                }
                // a residual keeps the name of a union or an error it took
                // nothing from (the two a difference builds anew): a later
                // pair is keyed by it, and met again
                let r = t0.diff_int(env, hist, &t1).map(|d| match (self, &t0) {
                    (Type::Ref(_), Type::Set(_) | Type::Error(_))
                        if union_identical(&d, &t0) =>
                    {
                        self.clone()
                    }
                    _ => d,
                });
                hist.unfoldings.leave(defs);
                hist.hist.remove(&key);
                let cut = mem::replace(&mut hist.cut, outer_cut);
                hist.cut = hist.cut.min(cut);
                let reached = mem::replace(&mut hist.reached, outer_reached);
                hist.reached.union(&reached);
                if kept
                    && cut >= depth
                    && let Ok(d) = &r
                {
                    let done =
                        Done { answer: d.clone(), reached, over_cut: hist.over_cut };
                    hist.done.get_mut().insert(key, Kept::new(done, self, t));
                }
                r
            }
            (Type::App(c, a), t) if let Some(filled) = Type::app_filled(c, a) => {
                filled.diff_int(env, hist, t)
            }
            (t0, Type::App(c, a)) if let Some(filled) = Type::app_filled(c, a) => {
                t0.diff_int(env, hist, &filled)
            }
            (Type::App(..), Type::Any) => Ok(Type::Primitive(BitFlags::empty())),
            (Type::TVar(tv), t @ Type::App(..)) | (t @ Type::App(..), Type::TVar(tv))
                if let Some(b) = tv.binding() =>
            {
                match self {
                    Type::TVar(_) => b.diff_int(env, hist, t),
                    _ => t.diff_int(env, hist, &b),
                }
            }
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Bound(_), _)
            | (_, Type::Bound(_)) => Ok(if same_type(self, t) {
                Type::Primitive(BitFlags::empty())
            } else {
                self.clone()
            }),
            (Type::Set(s0), Type::Set(s1)) => {
                let mut s: LPooled<Vec<Type>> = LPooled::take();
                for m in s0.iter() {
                    let less = |hist: &mut DiffHist| {
                        s1.iter().try_fold(m.clone(), |d, t| d.diff_int(env, hist, t))
                    };
                    s.push(hist.part(less)?);
                }
                Ok(Self::flatten_set(s.drain(..).filter(|d| !is_empty_ty(d))))
            }
            (Type::Set(s), t) => {
                let mut diffs: LPooled<Vec<Type>> = LPooled::take();
                let mut partial = false;
                for m in s.iter() {
                    let d = m.diff_int(env, hist, t)?;
                    if !is_empty_ty(&d) && d != *m {
                        partial = true;
                    }
                    diffs.push(d);
                }
                if partial {
                    Ok(Self::flatten_set(diffs.drain(..).filter(|d| !is_empty_ty(d))))
                } else {
                    diffs.retain(|d| !is_empty_ty(d));
                    Ok(match diffs.len() {
                        0 => Type::Primitive(BitFlags::empty()),
                        1 => diffs.pop().unwrap(),
                        _ => Type::Set(Arc::from_iter(diffs.drain(..))),
                    })
                }
            }
            (t, Type::Set(s)) => {
                let mut t = t.clone();
                for st in s.iter() {
                    t = t.diff_int(env, hist, st)?;
                }
                Ok(t)
            }
            (Type::Tuple(_), Type::Tuple(_))
            | (Type::Struct(_), Type::Struct(_))
            | (Type::Variant(..), Type::Variant(..)) => {
                if held_by_shape(env, hist, self, t) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Map { .. }, Type::Map { .. }) => {
                if same_type(self, t) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Map { .. }, Type::Primitive(p)) => {
                if p.contains(Typ::Map) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Primitive(p), Type::Map(m)) => {
                if is_any(&m.key) && is_any(&m.value) {
                    let mut p = *p;
                    p.remove(Typ::Map);
                    Ok(Type::Primitive(p))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Fn(_), Type::Fn(_)) => {
                if same_type(self, t) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            // Bindings are cloned out: the recursion may lock these cells.
            (Type::TVar(tv0), t1 @ Type::TVar(tv1)) => {
                if tv0.same_cell(tv1) {
                    return Ok(Type::Primitive(BitFlags::empty()));
                }
                Ok(match (tv0.binding(), tv1.binding()) {
                    (None, _) => Type::TVar(tv0.clone()),
                    (Some(t0), None) => t0.diff_int(env, hist, t1)?,
                    (Some(t0), Some(t1)) => t0.diff_int(env, hist, &t1)?,
                })
            }
            (Type::TVar(tv), t) => Ok(match tv.binding() {
                Some(tv) => tv.diff_int(env, hist, t)?,
                None => self.clone(),
            }),
            (t, Type::TVar(tv)) => Ok(match tv.binding() {
                Some(tv) => t.diff_int(env, hist, &tv)?,
                None => self.clone(),
            }),
            // A collection that fails an element type test may still hold
            // elements that pass it: one that is not all `t1` is any
            // collection of `t0`, so only a `t1` covering every element
            // subtracts anything.
            (Type::List(t0), Type::List(t1)) | (Type::Array(t0), Type::Array(t1)) => {
                let covered = union_identical(t0, t1)
                    || matches!(
                        t0.diff_int(env, hist, t1)?,
                        Type::Primitive(p) if p.is_empty()
                    );
                match covered {
                    true => Ok(Type::Primitive(BitFlags::empty())),
                    false => {
                        hist.over_cut = hist.over_cut.min(CUT);
                        Ok(self.clone())
                    }
                }
            }
            (Type::Primitive(p), Type::Array(t)) => {
                if is_any(t) {
                    let mut s = *p;
                    s.remove(Typ::Array);
                    Ok(Type::Primitive(s))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Array(_), Type::Primitive(p)) => {
                if p.contains(Typ::Array) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (_, Type::Any) => Ok(Type::Primitive(BitFlags::empty())),
            (Type::Any, _) => Ok(Type::Any),
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                let mut s = *s0;
                s.remove(*s1);
                Ok(Type::Primitive(s))
            }
            (Type::Primitive(p), Type::Error(e)) => {
                if is_any(e) {
                    let mut s = *p;
                    s.remove(Typ::Error);
                    Ok(Type::Primitive(s))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Error(_), Type::Primitive(p)) => {
                if p.contains(Typ::Error) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Error(e0), Type::Error(e1)) => {
                if union_identical(e0, e1) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    match e0.diff_int(env, hist, e1)? {
                        Type::Primitive(p) if p.is_empty() => {
                            Ok(Type::Primitive(BitFlags::empty()))
                        }
                        d if hist.over_cut >= Some(ERRORS_OVER_CUT)
                            || union_identical(&d, e0) =>
                        {
                            hist.over_cut = hist.over_cut.min(CUT);
                            Ok(self.clone())
                        }
                        d => {
                            hist.over_cut = hist.over_cut.map(|n| n + 1);
                            Ok(Type::Error(Arc::new(d)))
                        }
                    }
                }
            }
            (Type::ByRef(m0, t0), Type::ByRef(m1, t1)) => {
                if (m0 == m1 || *m1 == super::Mutability::Shared)
                    && union_identical(t0, t1)
                {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Abstract { .. }, Type::Abstract { .. }) if same_type(self, t) => {
                Ok(Type::Primitive(BitFlags::empty()))
            }
            (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Fn(_), _)
            | (_, Type::Fn(_))
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
            | (Type::ByRef(..), _)
            | (_, Type::ByRef(..))
            | (Type::Error(_), _)
            | (_, Type::Error(_))
            | (Type::Primitive(_), _)
            | (_, Type::Primitive(_))
            | (Type::Bottom, _)
            | (Type::Map { .. }, _) => Ok(self.clone()),
        }
    }

    pub fn matching_type(&self, env: &Env, reaching: &Self) -> Result<Self> {
        self.matching_type_int(env, &mut MatchingTypes::new(false), reaching)
    }

    /// [`Self::matching_type`], where a part of `reaching` holding a rigid
    /// cell, whose values no pattern takes apart, is this test's part: the
    /// types of the binds inside it.
    pub fn matching_parts(&self, env: &Env, reaching: &Self) -> Result<Self> {
        self.matching_type_int(env, &mut MatchingTypes::new(true), reaching)
    }

    fn matching_type_int(
        &self,
        env: &Env,
        walk: &mut MatchingTypes,
        reaching: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| {
            let key = (walk.memo.ref_id(self, env), walk.memo.ref_id(reaching, env));
            if let Some(m) = walk.memo.get(&key) {
                return Ok(m.answer.clone());
            }
            let typ = self.matching_type_inner(env, walk, reaching)?;
            walk.memo.insert(key, Kept::new(typ.clone(), self, reaching));
            Ok(typ)
        })
    }

    fn matching_type_inner(
        &self,
        env: &Env,
        walk: &mut MatchingTypes,
        reaching: &Self,
    ) -> Result<Self> {
        if strictly_identical(self, reaching) {
            return Ok(self.clone());
        }
        match (self, reaching) {
            (Self::TVar(tv), t) => match tv.binding() {
                Some(p) => p.matching_type_int(env, walk, t),
                None => Ok(t.clone()),
            },
            (p, Self::TVar(tv)) if own_over_rigid(p, tv) => Ok(reaching.clone()),
            (p, Self::TVar(tv)) => match tv.binding() {
                Some(t) => p.matching_type_int(env, walk, &t),
                None if tv.cell_constraints().contains(&Self::Bound(Bound::NoError)) => {
                    p.diff(env, &Self::Primitive(Typ::Error.into()))
                }
                None => Ok(p.clone()),
            },
            (p @ Self::Array(_), Self::App(c, a)) if Self::app_filled(c, a).is_none() => {
                p.matching_type_int(env, walk, &Self::Array(a.clone()))
            }
            (p @ Self::List(_), Self::App(c, a)) if Self::app_filled(c, a).is_none() => {
                p.matching_type_int(env, walk, &Self::List(a.clone()))
            }
            (p @ Self::Ref(_), t) | (p, t @ Self::Ref(_)) => {
                let definition = |t: &Type| match t {
                    Self::Ref(tr) => tr.resolve_in(env).map(|r| r.def_key()),
                    _ => None,
                };
                let key = (definition(p), definition(t));
                // a part holding the test's own variable passes every value
                // there: what reaches it types it
                if !walk.refs.insert(key) {
                    return Ok(if p.has_free() { t.clone() } else { p.clone() });
                }
                let r =
                    p.lookup_ref(env)?.matching_type_int(env, walk, &t.lookup_ref(env)?);
                walk.refs.remove(&key);
                r
            }

            (Self::Any, t) | (t, Self::Any) => Ok(t.clone()),
            (Self::Primitive(p), Self::Primitive(t)) => Ok(Self::Primitive(*p & *t)),
            (Self::Set(ps), t) | (t, Self::Set(ps)) => {
                let mut matched: LPooled<Vec<Type>> = LPooled::take();
                for p in ps.iter() {
                    let m = if matches!(self, Self::Set(_)) {
                        p.matching_type_int(env, walk, t)?
                    } else {
                        t.matching_type_int(env, walk, p)?
                    };
                    if !m.is_uninhabited() {
                        matched.push(m);
                    }
                }
                Ok(Self::flatten_set(matched.drain(..)))
            }
            (p, t) => {
                let same_shape = matches!(
                    p,
                    Self::Array(_)
                        | Self::List(_)
                        | Self::Error(_)
                        | Self::Map(_)
                        | Self::Tuple(_)
                        | Self::Variant(..)
                        | Self::Struct(_)
                ) && p.shares_constructor(t);
                let rigid_inside = walk.inside && same_shape && t.has_rigid();
                let probe = BitFlags::empty();
                if !rigid_inside
                    && !p.has_unbound()
                    && p.contains_int(probe, env, &mut walk.contains, t)?
                {
                    return Ok(t.clone());
                }
                if !t.has_unbound()
                    && t.contains_int(probe, env, &mut walk.contains, p)?
                {
                    return Ok(p.clone());
                }
                if !p.could_match(env, t)? {
                    return Ok(Self::Bottom);
                }
                if !same_shape {
                    return Ok(p.clone());
                }
                let mut parts: LPooled<Vec<Type>> = LPooled::take();
                t.for_each_child(&mut |c| parts.push(c.clone()));
                let mut parts = parts.drain(..);
                let mut error = None;
                let narrowed = p.cow_children(&mut |c| {
                    let t = parts.next().expect("matching shape");
                    match c.matching_type_int(env, walk, &t) {
                        Ok(n) => (!strictly_identical(c, &n)).then_some(n),
                        Err(e) => {
                            error = Some(e);
                            None
                        }
                    }
                });
                match error {
                    Some(e) => Err(e),
                    None => Ok(narrowed.unwrap_or_else(|| p.clone())),
                }
            }
        }
    }

    /// The values of `self` that are not values of `t`, or more where the
    /// walk's pair budget stopped it subtracting.
    pub fn diff(&self, env: &Env, t: &Self) -> Result<Self> {
        Ok(self.diff_walk(env, t)?.0)
    }

    /// The values of `self` that are not values of `t`, `None` where a
    /// budget left [`Self::diff`] wider: a caller that reads what a
    /// difference left out needs this one.
    pub fn diff_exact(&self, env: &Env, t: &Self) -> Result<Option<Self>> {
        let (d, widened) = self.diff_walk(env, t)?;
        Ok((!widened).then_some(d))
    }

    fn diff_walk(&self, env: &Env, t: &Self) -> Result<(Self, bool)> {
        let mut hist = DiffHist::new();
        let r = self.diff_int(env, &mut hist, t)?;
        let r = if diff_already_normal(self, &r) { r } else { r.normalize() };
        Ok((r, hist.widened))
    }
}

/// Whether `p`, a written test's part, holds a variable of the test's own
/// over `tv`, an open cell held rigid: a run-time test passes every value
/// at that variable, so `p` does not type the cell's values that pass it,
/// and the cell does.
fn own_over_rigid(p: &Type, tv: &TVar) -> bool {
    tv.open_cell().is_some_and(|c| c.is_rigid()) && p.has_free()
}

/// `Any`, read through bound type variables.
fn is_any(t: &Type) -> bool {
    t.with_deref(|t| matches!(t, Some(Type::Any)))
}

/// Equality read through bound type variables, where two open cells are
/// one type only when they are one cell.
fn same_type(t0: &Type, t1: &Type) -> bool {
    let bound = |t: &Type| cell_beneath(t, |tv, _| tv.binding().is_some());
    union_identical(t0, t1) || (bound(t0) || bound(t1)) && identical_normal(t0, t1)
}

/// Both sides read through bound type variables and normalized, where two
/// open cells are one type only when they are one cell: two declared
/// variables may differ in every instance.
fn identical_normal(t0: &Type, t1: &Type) -> bool {
    union_identical(&t0.resolve_tvars().normalize(), &t1.resolve_tvars().normalize())
}

/// Whether every value of `t0` is one of `t1` by their shapes alone
/// ([`Held::of`]), against the pair budget of `hist`'s walk.
fn held_by_shape(env: &Env, hist: &mut DiffHist, t0: &Type, t1: &Type) -> bool {
    Held { env, hist, read: SmallVec::new(), minuend_reads: 0 }.of(t0, t1)
}

/// The walk of [`held_by_shape`].
struct Held<'a> {
    env: &'a Env,
    hist: &'a mut DiffHist,
    /// The definition of each name of `t1` the path is reading, with the
    /// count of `t0`'s names the path was reading when it began.
    read: SmallVec<[(usize, usize); 4]>,
    /// How many names of `t0` the path is reading.
    minuend_reads: usize,
}

impl Held<'_> {
    /// Whether every value of `t0` is one of `t1` by their shapes alone:
    /// `t1` is `Any` or `t0` itself, read through bound cells, primitives
    /// `t1`'s flags hold, or the two are tuples, structs or variants of one
    /// shape whose parts are held so. A union `t0` is held where each of its
    /// members is, and another `t0` by a union `t1` where one of its members
    /// holds it, a primitive flag by flag. Names are read through, one of
    /// `t1` against the pair budget, past which nothing more is held, and
    /// never again on a path once `t0` has read a name since it began: the
    /// walk stops where both sides recur.
    fn of(&mut self, t0: &Type, t1: &Type) -> bool {
        if is_any(t1) || union_identical(t0, t1) {
            return true;
        }
        let env = self.env;
        let product =
            |t: &Type| matches!(t, Type::Tuple(_) | Type::Struct(_) | Type::Variant(..));
        ensure_sufficient(|| match (t0.deref_cloned(), t1.deref_cloned()) {
            (Some(t0 @ Type::Ref(_)), Some(t1)) => {
                if !product(&t1) && identical_normal(&t0, &t1) {
                    return true;
                }
                let Ok(Some(t)) = t0.lookup_ref_with(env, false) else { return false };
                self.minuend_reads += 1;
                let held = self.of(&t, &t1);
                self.minuend_reads -= 1;
                held
            }
            (Some(t0), Some(ref t1 @ Type::Ref(ref tr))) => {
                self.read_through(&t0, t1, tr)
            }
            (Some(Type::Set(ref ms)), Some(ref t1))
                if product(t1) || matches!(t1, Type::Set(_) | Type::Primitive(_)) =>
            {
                ms.iter().all(|m| self.of(m, t1))
            }
            (Some(Type::Primitive(p0)), Some(Type::Set(ref ms))) if p0.len() > 1 => {
                p0.iter().all(|p| {
                    let p = Type::Primitive(p.into());
                    ms.iter().any(|m| self.of(&p, m))
                })
            }
            (_, Some(Type::Set(ref ms))) => ms.iter().any(|m| self.of(t0, m)),
            (Some(Type::Primitive(p0)), Some(Type::Primitive(p1))) => p1.contains(p0),
            (Some(Type::Tuple(ref p0)), Some(Type::Tuple(ref p1))) => {
                self.at_each(p0.iter(), p1.iter())
            }
            (
                Some(Type::Variant(ref tag0, ref p0, _)),
                Some(Type::Variant(ref tag1, ref p1, _)),
            ) => tag0 == tag1 && self.at_each(p0.iter(), p1.iter()),
            (Some(Type::Struct(ref f0)), Some(Type::Struct(ref f1))) => {
                f0.len() == f1.len()
                    && f0.iter().zip(f1.iter()).all(|((n0, _, _), (n1, _, _))| n0 == n1)
                    && self.at_each(
                        f0.iter().map(|(_, t, _)| t),
                        f1.iter().map(|(_, t, _)| t),
                    )
            }
            (_, Some(ref t1)) if product(t1) => false,
            _ => identical_normal(t0, t1),
        })
    }

    /// [`Self::of`] with `t1`, the name `tr`, read through: not held where
    /// the path reads it already from before `t0` read a name, or the pair
    /// budget is spent.
    fn read_through(&mut self, t0: &Type, t1: &Type, tr: &TypeRef) -> bool {
        let Some(def) = tr.resolve_in(self.env).map(|r| r.def_key()) else {
            return false;
        };
        if self.read.iter().any(|(d, at)| *d == def && *at < self.minuend_reads) {
            return false;
        }
        if !self.hist.pairs.unfold() {
            self.hist.cut = 0;
            self.hist.widened = true;
            self.hist.holds(CUT);
            return false;
        }
        #[cfg(test)]
        DIFF_PAIRS.with(|n| n.set(n.get() + 1));
        self.read.push((def, self.minuend_reads));
        let held = matches!(
            t1.lookup_ref_with(self.env, false),
            Ok(Some(t)) if self.of(t0, &t)
        );
        self.read.pop();
        held
    }

    /// Positional [`Self::of`].
    fn at_each<'a, 'b>(
        &mut self,
        t0: impl IntoIterator<Item = &'a Type>,
        t1: impl IntoIterator<Item = &'b Type>,
    ) -> bool {
        let mut t0 = t0.into_iter();
        let mut t1 = t1.into_iter();
        loop {
            match (t0.next(), t1.next()) {
                (None, None) => return true,
                (Some(a), Some(b)) if self.of(a, b) => (),
                _ => return false,
            }
        }
    }
}

#[cfg(test)]
thread_local! {
    pub(super) static DIFF_PAIRS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static DIFF_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static MEET_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    pub(super) static IDENTITY_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NO_MEMO: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Whether diff, could_match, [`IdenticalMemo`] and [`Meets`] read their
/// memos: a test compares their answers with the walks' without them.
#[inline(always)]
pub(super) fn memo_on() -> bool {
    #[cfg(test)]
    return !NO_MEMO.with(|m| m.get());
    #[cfg(not(test))]
    true
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        PrintFlag,
        env::Env,
        expr::{ModPath, TypeDefBody, parser::parse_type},
        format_with_flags,
        typ::{
            Bound, NodeKey, NodePairs, TVar, Type,
            cast::tests::{parsed, typed, union_dag},
            matches::MATCH_PAIRS,
            node_addr,
            setops::{
                ERRORS_OVER_CUT, Identical, IdenticalMemo, WALK_PAIRS, one_type,
                union_identical,
            },
        },
    };
    use ahash::{AHashMap, AHashSet};
    use arcstr::literal;
    use netidx_value::Typ;
    use triomphe::Arc;

    /// `t` at the root of `env`, its variable 'c bound to i64.
    fn bound(env: &Env, t: &str) -> Type {
        let t = parsed(env, t);
        let mut cells = AHashMap::default();
        t.collect_tvars(&mut cells);
        cells["c"].bind(Type::Primitive(Typ::I64.into()));
        t
    }

    // Each instantiation of W expands to an abstract type built anew, so
    // the walk meets Box<i64> and Box<string> at fresh addresses.
    #[test]
    fn matching_type_tells_fresh_instantiations_apart() {
        let (env, _) =
            typed(&[("Box<'a>", "Abstract<'a>"), ("W<'a>", "[Box<'a>, `X]")], "null");
        let test = parsed(&env, "[W<i64>, W<string>]");
        let reaching = parsed(&env, "[Box<i64>, Box<string>, `X]");
        for _ in 0..16 {
            let m = test.matching_type(&env, &reaching).unwrap().normalize();
            assert_eq!(m.to_string(), "[`X, Box<i64>, Box<string>]");
        }
    }

    // A test's part holding a variable of the test's own, over a cell held
    // rigid, types none of the cell's values: a run-time test passes every
    // value at the variable, so what passes the part is the cell's. A
    // closed part types what passes it.
    #[test]
    fn matching_type_keeps_a_rigid_cell_under_a_part_holding_the_tests_own_variable() {
        let (env, _) = typed(&[("Opt<'b>", "['b, null]")], "null");
        let tests = [
            "Array<['b, null]>",
            "Array<Opt<'b>>",
            "Array<Opt<Opt<'b>>>",
            "Array<['b, i64]>",
            "Array<'b>",
        ];
        for test in tests {
            let test = parsed(&env, test);
            let reaching = parsed(&env, "[Array<'a>, i64]");
            let mut cells = AHashMap::default();
            reaching.collect_tvars(&mut cells);
            let _gate = cells["a"].open_rigid();
            let m = test.matching_type(&env, &reaching).unwrap().normalize();
            assert_eq!(m.to_string(), "Array<'a>", "{test}");
            let m = test.matching_parts(&env, &reaching).unwrap().normalize();
            assert_eq!(m.to_string(), "Array<'a>", "{test}");
        }
        let closed = parsed(&env, "Array<[i64, null]>");
        let reaching = parsed(&env, "Array<'a>");
        let mut cells = AHashMap::default();
        reaching.collect_tvars(&mut cells);
        let _gate = cells["a"].open_rigid();
        let m = closed.matching_parts(&env, &reaching).unwrap().normalize();
        assert_eq!(m.to_string(), "Array<[i64, null]>");
    }

    // A reference whose cell is open names the definition its name has
    // in the env, as a filled one does: the union of the two is the one
    // reference, whichever cells a walk filled before.
    #[test]
    fn an_open_reference_is_its_filled_twin() {
        let (env, filled) = typed(&[("R<'a>", "[`Nil, `Cons('a, R<'a>)]")], "R<i64>");
        let open = || parse_type("R<i64>").unwrap().scope_refs(&ModPath::root());
        for (t0, t1) in [(filled.clone(), open()), (open(), filled.clone())] {
            let u = Type::union(&env, &[&t0, &t1]).unwrap();
            assert_eq!(u.to_string(), "R<i64>");
            assert!(t0.diff(&env, &t1).unwrap().is_uninhabited());
        }
    }

    // And a walk keys a type holding the name as one, whichever of its
    // copies a walk filled first: a select's narrowing reads its memo by
    // these keys, so its answer does not depend on what was asked before.
    #[test]
    fn a_name_inside_a_type_keys_alike_filled_or_open() {
        use crate::typ::{RefHist, RefPair};
        let (env, _) = typed(&[("U", "[`K(U), `N]")], "null");
        let open = || parse_type("`K(U)").unwrap().scope_refs(&ModPath::root());
        let filled = || parsed(&env, "`K(U)");
        for (t0, t1) in [(filled(), open()), (open(), filled())] {
            let mut hist = RefHist::<AHashMap<RefPair, usize>>::new();
            let (k0, k1) = (hist.ref_id(&t0, &env), hist.ref_id(&t1, &env));
            assert!(k0.is_some() && k0 == k1, "{t0} {k0:?}, {t1} {k1:?}");
        }
    }

    // Two open references a walk takes as one by their arguments stay
    // open: the walk may run under an env that does not see their scope,
    // where their name means an outer shadow, which a fill would keep for
    // every later walk.
    #[test]
    fn a_shortcut_fills_no_open_pair() {
        let (env, _) = typed(&[("S<'a>", "['a, `X]"), ("I", "i64")], "null");
        let open = |t: &str| parse_type(t).unwrap().scope_refs(&ModPath::root());
        let (t0, t1) = (open("S<i64>"), open("S<I>"));
        t0.check_contains(&env, &t1).unwrap();
        for t in [&t0, &t1] {
            let Type::Ref(tr) = t else { panic!("{t} is a reference") };
            assert!(tr.is_open(), "{t} was filled")
        }
    }

    // Two applications of one definition taken as one by their arguments
    // hold those arguments to the parameters' bounds, as their expansions
    // do.
    #[test]
    fn one_definition_by_its_arguments_holds_their_bounds() {
        let (mut env, _) = typed(&[], "null");
        let r = (TVar::empty_named(literal!("r")), Some(Type::Bound(Bound::NoError)));
        let body = TypeDefBody::Alias(parse_type("['r, `X]").unwrap());
        let site = (Default::default(), Default::default(), Arc::new(Default::default()));
        let root = ModPath::root();
        env.deftype(
            &root,
            "R",
            Arc::from_iter([r]),
            &body,
            true,
            None,
            site.0,
            site.1,
            site.2,
        )
        .unwrap();
        env.seed_typedef_refs();
        let t = parsed(&env, "R<'c>");
        let mut cells = AHashMap::default();
        t.collect_tvars(&mut cells);
        t.check_contains(&env, &t.clone()).unwrap();
        let bounds = cells["c"].cell_constraints();
        assert!(bounds.contains(&Type::Bound(Bound::NoError)), "{bounds:?}");
    }

    // diff reads a bound cell as its binding wherever it compares parts:
    // a map's, a function's, an abstract type's parameters and an open
    // constructor's argument, under one constructor cell.
    #[test]
    fn diff_reads_bound_cells_through() {
        let (env, _) =
            typed(&[("S", "Map<i64, i64>"), ("Box<'a>", "Abstract<'a>")], "null");
        let bound = |t: &str| bound(&env, t);
        let expanded = |t: Type| t.lookup_ref(&env).unwrap();
        let app = bound("'k<'c>");
        let Type::App(k, _) = &app else { unreachable!() };
        let app_i64 = Type::App(k.clone(), Arc::new(Type::Primitive(Typ::I64.into())));
        for (t0, t1) in [
            (bound("Map<'c, i64>"), parsed(&env, "Map<i64, i64>")),
            (parsed(&env, "Map<i64, i64>"), bound("Map<'c, i64>")),
            (
                bound("[Error<Map<'c, i64>>, Array<Map<i64, i64>>]"),
                parsed(&env, "[Error<S>, Array<S>]"),
            ),
            (bound("fn(x: 'c) -> i64"), parsed(&env, "fn(x: i64) -> i64")),
            (expanded(bound("Box<'c>")), expanded(parsed(&env, "Box<i64>"))),
            (app, app_i64),
        ] {
            let d = t0.diff(&env, &t1).unwrap();
            assert!(d.is_uninhabited(), "{t0:?} - {t1:?} = {d:?}");
        }
    }

    // Two open cells are one type only when they are one cell, wherever
    // diff compares parts: an array's or a list's element, a map's key, an
    // error's payload, an abstract type's parameter and a constructor.
    #[test]
    fn diff_tells_two_open_cells_apart() {
        let (env, _) = typed(&[("Box<'a>", "Abstract<'a>")], "null");
        let expanded = |t: &str| parsed(&env, t).lookup_ref(&env).unwrap();
        for (t0, t1) in [
            (parsed(&env, "Array<'a>"), parsed(&env, "Array<'b>")),
            (parsed(&env, "List<'a>"), parsed(&env, "List<'b>")),
            (parsed(&env, "Array<Array<'a>>"), parsed(&env, "Array<Array<'b>>")),
            (parsed(&env, "Map<'a, i64>"), parsed(&env, "Map<'b, i64>")),
            (parsed(&env, "Error<Array<'a>>"), parsed(&env, "Error<Array<'b>>")),
            (expanded("Box<'a>"), expanded("Box<'b>")),
            (parsed(&env, "'k<i64>"), parsed(&env, "'j<i64>")),
        ] {
            let d = t0.diff(&env, &t1).unwrap();
            assert!(union_identical(&d, &t0), "{t0} - {t1} = {d}");
        }
    }

    // A cell bound to Any is Any where diff asks whether a part covers
    // anything: at a tuple's position and as a map's, an array's or an
    // error's every value.
    #[test]
    fn diff_reads_a_cell_bound_to_any_as_any() {
        let (env, _) = typed(&[], "null");
        let any = |t: &str| {
            let t = parsed(&env, t);
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells.values().for_each(|c| c.bind(Type::Any));
            t
        };
        let d = parsed(&env, "(i64, string)").diff(&env, &any("('c, string)")).unwrap();
        assert_eq!(d.to_string(), "[]");
        for (p, t) in [
            (Typ::Array, "Array<'c>"),
            (Typ::Map, "Map<'c, 'd>"),
            (Typ::Error, "Error<'c>"),
        ] {
            let d = Type::Primitive(p | Typ::I64).diff(&env, &any(t)).unwrap();
            assert_eq!(d.to_string(), "i64", "{p:?} - {t}");
        }
    }

    // A tuple, struct or variant goes whole where its subtrahend holds each of
    // its positions, through a name and at any depth, and stays whole where a
    // position keeps a value, as one where two open cells stand.
    #[test]
    fn diff_takes_a_product_held_at_every_position() {
        let (env, _) = typed(&[("P", "(i64, i64)")], "null");
        for (t0, t1, left) in [
            ("`A((i64, i64))", "`A((i64, Any))", "[]"),
            ("[`A(P), `B]", "`A((Any, i64))", "`B"),
            ("(`A((i64, i64)), i64)", "(`A((Any, i64)), Any)", "[]"),
            ("{a: i64, b: (i64, i64)}", "{a: i64, b: (Any, i64)}", "[]"),
            ("`A([(i64, i64), (i64, string)])", "`A((i64, Any))", "[]"),
            ("`A((i64, [i64, null]))", "`A((i64, i64))", "`A((i64, [i64, null]))"),
            ("(i64, i64)", "(i64, string)", "(i64, i64)"),
            ("`A(('a, i64))", "`A(('b, Any))", "`A(('a, i64))"),
            ("`A('a)", "`A('b)", "`A('a)"),
        ] {
            let d = parsed(&env, t0).diff(&env, &parsed(&env, t1)).unwrap();
            assert_eq!(d.to_string(), left, "{t0} - {t1}");
        }
    }

    // A position the subtrahend writes as a union or a name holds a minuend's
    // position one of its members holds, member by member where the minuend's
    // is a union, and a name of the subtrahend is read through.
    #[test]
    fn diff_takes_a_product_a_union_or_name_holds_at_a_position() {
        let (env, _) = typed(
            &[
                ("U", "[i64, string]"),
                ("N", "[I, R]"),
                ("I", "[i64, i32]"),
                ("R", "f64"),
                ("L", "[`Cons(i64, L), `Nil]"),
                ("M", "[`Cons([i64, string], M), `Nil]"),
            ],
            "null",
        );
        for (t0, t1, left) in [
            ("[(i64, i64), null]", "([i64, string], i64)", "null"),
            ("[`A(i64, `X), null]", "`A(i64, [`X, `Y])", "null"),
            (
                "[{a: i64, b: (null, i64)}, null]",
                "{a: i64, b: ([i64, null], i64)}",
                "null",
            ),
            (
                "[(i64, [i64, null]), null]",
                "([i64, string], [i64, string, null])",
                "null",
            ),
            ("[(i64, [`X, i64]), null]", "(i64, [`X, `Y, i64])", "null"),
            ("[(i64, i64), null]", "(U, i64)", "null"),
            ("[(i32, i64), null]", "(N, i64)", "null"),
            ("[([i32, f64], i64), null]", "(N, i64)", "null"),
            ("[`A(`Nil), null]", "`A(L)", "null"),
            ("[`A(`Cons(i64, `Nil)), null]", "`A(L)", "null"),
            (
                "[(i64, [i64, f64]), null]",
                "(i64, [i64, string])",
                "[null, (i64, [i64, f64])]",
            ),
            ("[(f64, i64), null]", "(U, i64)", "[null, (f64, i64)]"),
            ("[(i64, i64), null]", "(['a, string], i64)", "[null, (i64, i64)]"),
            ("[`A(L), null]", "`A(M)", "[null, `A(L)]"),
        ] {
            let d = parsed(&env, t0).diff(&env, &parsed(&env, t1)).unwrap();
            assert_eq!(d.to_string(), left, "{t0} - {t1}");
        }
    }

    // A name the subtrahend writes is read again on a path only while the
    // minuend reads none: a pair of recursive names stops at their first
    // turn, and a finite minuend goes as deep as it is.
    #[test]
    fn held_reads_a_subtrahends_name_until_both_sides_recur() {
        let (env, _) = typed(
            &[("L", "[`Cons(i64, L), `Nil]"), ("M", "[`Cons([i64, string], M), `Nil]")],
            "null",
        );
        for (t0, t1, left, pairs) in [
            ("[`A(L), null]", "`A(M)", "[null, `A(L)]", 1),
            ("[`A(`Cons(i64, `Cons(i64, `Cons(i64, `Nil)))), null]", "`A(M)", "null", 4),
        ] {
            super::DIFF_PAIRS.with(|n| n.set(0));
            let d = parsed(&env, t0).diff(&env, &parsed(&env, t1)).unwrap();
            assert_eq!(d.to_string(), left, "{t0} - {t1}");
            assert_eq!(super::DIFF_PAIRS.with(|n| n.get()), pairs, "{t0} - {t1}");
        }
    }

    // A bind's cell holds a name's expansion, never the name: the name in
    // the minuend is read through to meet it, a union, a recursive union
    // and an abstract type alike, at a payload's top and inside a tuple.
    #[test]
    fn diff_reads_a_minuend_name_through_to_a_bound_expansion() {
        let (env, _) = typed(
            &[
                ("U", "[i64, string]"),
                ("L", "[`Cons(i64, L), `Nil]"),
                ("W", "Abstract<(i64, i64)>"),
            ],
            "null",
        );
        for (t0, t1, cell, left) in [
            ("[`A((U, i64)), `B]", "`A(('c, Any))", "U", "`B"),
            ("[`A(L), `B]", "`A('c)", "L", "`B"),
            ("[`A((L, i64)), `B]", "`A(('c, i64))", "L", "`B"),
            ("[`A(W), `B]", "`A('c)", "W", "`B"),
            ("[`A((U, i64)), `B]", "`A(('c, Any))", "L", "[`A((U, i64)), `B]"),
            ("[`A(W), `B]", "`A('c)", "U", "[`A(W), `B]"),
        ] {
            let t1 = parsed(&env, t1);
            let mut cells = AHashMap::default();
            t1.collect_tvars(&mut cells);
            cells["c"].bind(parsed(&env, cell).lookup_ref(&env).unwrap());
            let d = parsed(&env, t0).diff(&env, &t1).unwrap();
            assert_eq!(d.to_string(), left, "{t0} - {t1}, 'c = {cell}");
        }
    }

    /// Every difference of two of `pool` and could_match verdict of two,
    /// and could_match of each of `pool` against each difference (the
    /// question a select's next arm asks), bound cells printed.
    fn memo_answers(env: &Env, pool: &[Type], memo: bool) -> Vec<String> {
        super::NO_MEMO.with(|m| m.set(!memo));
        let mut out = vec![];
        format_with_flags(PrintFlag::DerefTVars, || {
            for s in pool {
                for t in pool {
                    let d = s.diff(env, t).unwrap();
                    out.push(format!(
                        "{s} - {t} = {d}, {}",
                        s.could_match(env, t).unwrap()
                    ));
                    for a in pool {
                        out.push(format!(
                            "{a} ~ {d}: {}",
                            a.could_match(env, &d).unwrap()
                        ));
                    }
                }
            }
        });
        super::NO_MEMO.with(|m| m.set(false));
        out
    }

    fn memos_agree(env: &Env, pool: &[Type]) {
        let without = memo_answers(env, pool, false);
        let with = memo_answers(env, pool, true);
        for (a, b) in without.iter().zip(with.iter()) {
            assert_eq!(a, b);
        }
    }

    fn written_memos_agree(defs: &[(&str, &str)], pool: &[&str]) {
        let (env, _) = typed(defs, "null");
        let pool: Vec<Type> = pool.iter().map(|t| parsed(&env, t)).collect();
        memos_agree(&env, &pool)
    }

    /// `` (`A, .. `A, last) ``, `n` `A.
    fn long_tuple(n: usize, last: &str) -> String {
        format!("({}{last})", "`A, ".repeat(n))
    }

    // Over names that recur through payloads (a pair met in progress
    // subtracts nothing there, so where a cycle is cut decides what is
    // left), over instantiations of an abstract type (built anew at each
    // expansion), over a walk that grows past `NodePairs::EAGER` while a
    // pair is in progress (K0, K1), and over cells (a key reads a bound
    // one as its binding, and pairs a function type's by position), bound
    // cells included as a program binds them, to a recursive name's
    // expansion, the memos change no answer.
    #[test]
    fn diff_and_could_match_memos_change_no_answer() {
        written_memos_agree(
            &[
                ("At", "[Error<Bt>, Error<Ct>, `R]"),
                ("Bt", "[Error<At>, `P]"),
                ("Ct", "[Array<Ct>, Error<Bt>]"),
            ],
            &["At", "Bt", "Ct", "[At, Bt]", "[Bt, Ct]", "Error<Array<Ct>>", "`R"],
        );
        written_memos_agree(
            &[
                ("L", "[Error<M>, `A]"),
                ("M", "[Error<L>, `B, `C]"),
                ("X", "[Error<Y>, `A]"),
                ("Y", "[Error<X>, `B]"),
                ("Z", "[X, Y]"),
            ],
            &["L", "M", "X", "Y", "Z", "[L, M]"],
        );
        written_memos_agree(
            &[("H", "[Error<Error<K>>, `Q]"), ("K", "[Error<H>, Error<K>, `P]")],
            &["H", "K", "[H, K]", "Error<K>", "`Q"],
        );
        written_memos_agree(
            &[("Box<'a>", "Abstract<'a>"), ("W<'a>", "[Box<'a>, `X]")],
            &[
                "Box<i64>",
                "[Box<i64>, Box<string>]",
                "W<i64>",
                "W<string>",
                "[W<i64>, W<f64>]",
            ],
        );
        for n in [16, 24] {
            let (k0, k1) = (long_tuple(n, "`X"), long_tuple(n, "`Y"));
            written_memos_agree(
                &[
                    ("K0", &k0),
                    ("K1", &k1),
                    ("L0", "(N0, `P)"),
                    ("L1", "(N1, `Q)"),
                    ("N0", "[Error<K0>, Error<L0>, `Y]"),
                    ("N1", "[Error<K1>, Error<L1>, `W]"),
                ],
                &[
                    "[Error<K0>, Error<L0>, Error<N0>]",
                    "[Error<K1>, Error<L1>, Error<N1>]",
                    "Error<N0>",
                    "Error<Error<L1>>",
                    "[Error<K1>, Error<L1>, `Z]",
                ],
            );
        }
        let (env, _) = typed(
            &[
                ("R<'p>", "Map<'p, i64>"),
                ("S", "Map<i64, i64>"),
                ("Box<'a>", "Abstract<'a>"),
                ("W<'a>", "[Box<'a>, `X]"),
                ("E<'p>", "[Error<R<'p>>, Array<R<'p>>]"),
                ("P", "[i64, (i64, `X)]"),
            ],
            "null",
        );
        let pool = [
            bound(&env, "R<'c>"),
            parsed(&env, "R<i64>"),
            bound(&env, "[Error<R<'c>>, Array<R<i64>>]"),
            bound(&env, "[Error<R<i64>>, Array<R<'c>>]"),
            bound(&env, "[Error<R<'c>>, Error<R<i64>>]"),
            bound(&env, "[R<'c>, R<i64>]"),
            parsed(&env, "[Error<S>, Array<S>]"),
            bound(&env, "W<'c>"),
            parsed(&env, "W<i64>"),
            bound(&env, "E<'c>"),
            parsed(&env, "E<i64>"),
            parsed(&env, "Error<R<i64>>"),
            parsed(&env, "[Error<R<'x>>, Error<R<'y>>, `D('x)]"),
            parsed(&env, "`D('z)"),
            parsed(&env, "[Error<fn(x: 'a) -> 'a>, Error<fn(x: 'b) -> 'b>]"),
            parsed(&env, "Error<fn(x: 'q) -> 'q>"),
        ];
        memos_agree(&env, &pool);
        let any = |t: &str| {
            let t = parsed(&env, t);
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells.values().for_each(|c| c.bind(Type::Any));
            t
        };
        let pool = [
            parsed(&env, "[Error<R<i64>>, Array<i64>, (i64, `X)]"),
            any("[Error<R<'c>>, Array<'d>, ('e, `X)]"),
            any("[Error<R<'c>>, Error<R<Any>>]"),
            parsed(&env, "[Error<R<Any>>, Array<Any>, (Any, `X)]"),
            any("[Array<'c>, Array<Any>, ('d, `X), (Any, `X)]"),
            parsed(&env, "[Error<S>, Array<i64>]"),
            Type::Set(Arc::from_iter([
                parsed(&env, "Error<P>"),
                parsed(&env, "Error<Error<P>>"),
            ])),
            Type::Set(Arc::from_iter([
                any("Error<('d, `X)>"),
                parsed(&env, "Error<Error<(Any, `X)>>"),
            ])),
        ];
        memos_agree(&env, &pool);
        let (env, _) = typed(
            &[
                ("Box<'a>", "Abstract<'a>"),
                ("W<'a>", "[Box<'a>, `X]"),
                ("N0", "[(N0, `U1), Error<N5>, `E]"),
                ("N1", "[Error<N5>, Map<string, N0>]"),
                ("N2", "[W<N0>, W<N1>]"),
                ("N4", "[Box<i64>, Error<Error<N5>>, N1, `T0(N5)]"),
                ("N5", "[(N4, `U0), N4]"),
            ],
            "null",
        );
        let expanded = |t: &str, name: &str| {
            let t = parsed(&env, t);
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells["c"].bind(parsed(&env, name).lookup_ref(&env).unwrap());
            t
        };
        let pool = [
            expanded("[Error<'c>, Error<N5>]", "N5"),
            expanded("Error<'c>", "N1"),
            expanded("`T0('c)", "N4"),
            expanded("[Error<'c>, `T0(N5)]", "N5"),
            expanded("W<'c>", "N0"),
            parsed(&env, "[N1, N2]"),
            parsed(&env, "Error<N5>"),
            parsed(&env, "N4"),
            parsed(&env, "N5"),
        ];
        memos_agree(&env, &pool);
    }

    // Generated names over the K0/K1 shape, Error towers over them and
    // unions of towers: a select's coverage composes could_match over
    // diff's output, and the memos change none of it.
    #[test]
    fn memos_change_no_composed_answer() {
        struct Rng(u64);
        impl Rng {
            fn below(&mut self, n: usize) -> usize {
                self.0 ^= self.0 << 13;
                self.0 ^= self.0 >> 7;
                self.0 ^= self.0 << 17;
                (self.0 % n as u64) as usize
            }
        }
        let err =
            |t: &str, k: usize| (0..k).fold(t.to_string(), |t, _| format!("Error<{t}>"));
        let tags = ["`P", "`Q", "`W", "`Y", "`Z"];
        for seed in 1..=40u64 {
            let mut r = Rng(seed.wrapping_mul(0x9E3779B97F4A7C15) | 1);
            let n = [2, 8, 16, 24, 40][r.below(5)];
            let mut defs: Vec<(String, String)> = vec![
                ("K0".into(), long_tuple(n, "`X")),
                ("K1".into(), long_tuple(n, "`Y")),
                ("L0".into(), format!("(N0, {})", tags[r.below(5)])),
                ("L1".into(), format!("(N1, {})", tags[r.below(5)])),
                ("N0".into(), format!("[Error<K0>, Error<L0>, {}]", tags[r.below(5)])),
                ("N1".into(), format!("[Error<K1>, Error<L1>, {}]", tags[r.below(5)])),
            ];
            let mut names: Vec<String> = defs.iter().map(|(n, _)| n.clone()).collect();
            for i in 0..1 + r.below(3) {
                let mut ms = std::collections::BTreeSet::from(["`Z".to_string()]);
                for _ in 0..2 + r.below(3) {
                    let m = match r.below(10) {
                        0..5 => err(&names[r.below(names.len())], 1 + r.below(2)),
                        5..7 => tags[r.below(5)].to_string(),
                        _ => names[r.below(names.len())].clone(),
                    };
                    ms.insert(m);
                }
                let ms: Vec<String> = ms.into_iter().collect();
                defs.push((format!("D{i}"), format!("[{}]", ms.join(", "))));
                names.push(format!("D{i}"));
            }
            let mut pool: Vec<String> = vec![
                "[Error<K0>, Error<L0>, Error<N0>]".into(),
                "[Error<K1>, Error<L1>, Error<N1>]".into(),
            ];
            for _ in 0..6 {
                pool.push(err(&names[r.below(names.len())], 1 + r.below(3)));
            }
            for _ in 0..4 {
                let pivot = names[r.below(names.len())].clone();
                let mut ms = std::collections::BTreeSet::from([
                    err(&pivot, 1),
                    err(&pivot, 2 + r.below(2)),
                ]);
                if r.below(2) == 0 {
                    ms.insert(err(&names[r.below(names.len())], 1 + r.below(3)));
                }
                let ms: Vec<String> = ms.into_iter().collect();
                pool.push(format!("[{}]", ms.join(", ")));
            }
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(n, b)| (&**n, &**b)).collect();
            let pool: Vec<&str> = pool.iter().map(|t| &**t).collect();
            written_memos_agree(&defs, &pool);
        }
    }

    /// `Ti = (Ti+1, Ti+1)` down to `Tn = [Error<T0>, leaf]`, and its
    /// twin `Si` with another leaf.
    fn tuple_dag(n: usize) -> Vec<(String, String)> {
        let mut defs = vec![];
        for (p, leaf) in [("T", "`Z"), ("S", "`Y")] {
            for i in 0..n {
                defs.push((format!("{p}{i}"), format!("({p}{}, {p}{})", i + 1, i + 1)));
            }
            defs.push((format!("{p}{n}"), format!("[Error<{p}0>, {leaf}]")));
        }
        defs
    }

    // Narrowing S0 to what a test of T0 takes of it, over a tuple DAG
    // whose only way back is through Error, asks each pair of shared
    // nodes once whether a union stands under a constructor.
    #[test]
    fn narrowing_over_a_tuple_dag_takes_each_pair_once() {
        use crate::typ::tvar::SPLIT_STEPS;
        let defs = tuple_dag(12);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "T0");
        let s = parsed(&env, "S0");
        let m = t.matching_type(&env, &s).unwrap();
        SPLIT_STEPS.with(|n| n.set(0));
        assert!(!s.narrow_pattern(&env, &m).unwrap());
        assert_eq!(SPLIT_STEPS.with(|n| n.get()), 174);
    }

    /// `T<i> = [Array<T<i+1>>, Array<T<i+2>>, `A<i>]`, indices mod `n`.
    fn array_family(n: usize) -> Vec<(String, String)> {
        (0..n)
            .map(|i| {
                let body =
                    format!("[Array<T{}>, Array<T{}>, `A{i}]", (i + 1) % n, (i + 2) % n);
                (format!("T{i}"), body)
            })
            .collect()
    }

    // Narrowing T0 to what a test of T1 takes of it, a predicate built of
    // parts shared along many paths, narrows each pair of shared nodes once.
    #[test]
    fn narrowing_over_shared_array_parts_takes_each_pair_once() {
        use crate::typ::tvar::NARROW_STEPS;
        let defs = array_family(5);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "T1");
        let s = parsed(&env, "T0");
        let m = t.matching_type(&env, &s).unwrap();
        NARROW_STEPS.with(|n| n.set(0));
        assert!(s.narrow_pattern(&env, &m).unwrap());
        assert_eq!(NARROW_STEPS.with(|n| n.get()), 310);
    }

    // Narrowing T0 to what a test of U0 takes of it, each a union that
    // returns to itself through an array, meets the pair it started from
    // inside itself and ends there.
    #[test]
    fn narrowing_over_two_recursive_unions_ends() {
        use crate::typ::tvar::NARROW_STEPS;
        let defs = [
            ("T0", "[Array<[T0, `A1]>, Array<T0>, `A0]"),
            ("U0", "[Array<[U0, U1]>, Array<U1>, `B0]"),
            ("U1", "[Array<U1>, `B1]"),
        ];
        let (env, t) = typed(&defs, "U0");
        let s = parsed(&env, "T0");
        let m = t.matching_type(&env, &s).unwrap();
        NARROW_STEPS.with(|n| n.set(0));
        assert!(s.narrow_pattern(&env, &m).unwrap());
        assert_eq!(NARROW_STEPS.with(|n| n.get()), 79);
    }

    /// Error cycles `C<l>_j = [Error<C<l>_j+1 mod l>, tag]` of the lengths
    /// `ls`, position 0 tagged `` `Mm `` where `shared`, `S` the union of
    /// their heads, and `M0 = [Error<M0>, `Mm]`.
    fn error_cycles(ls: &[usize], shared: bool) -> Vec<(String, String)> {
        let mut defs = vec![];
        for &l in ls {
            for j in 0..l {
                let tag = match j == 0 && shared {
                    true => "`Mm".to_string(),
                    false => format!("`K{l}_{j}"),
                };
                defs.push((
                    format!("C{l}_{j}"),
                    format!("[Error<C{l}_{}>, {tag}]", (j + 1) % l),
                ));
            }
        }
        let heads: Vec<String> = ls.iter().map(|l| format!("C{l}_0")).collect();
        defs.push(("S".into(), format!("[{}]", heads.join(", "))));
        defs.push(("M0".into(), "[Error<M0>, `Mm]".into()));
        defs
    }

    fn diff_pairs(defs: &[(String, String)], t: &str, less: &str) -> (String, usize) {
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, t);
        super::DIFF_PAIRS.with(|n| n.set(0));
        let rest = t.diff(&env, &parsed(&env, less)).unwrap();
        (rest.to_string(), super::DIFF_PAIRS.with(|n| n.get()))
    }

    // Over Error cycles of coprime lengths, whose exact residual repeats
    // only after their lcm, the pairs diff walks grow with the lengths, not
    // with their lcm.
    #[test]
    fn diff_over_error_cycles_walks_pairs_by_their_lengths() {
        let pairs =
            |ls: &[usize], shared| diff_pairs(&error_cycles(ls, shared), "M0", "S").1;
        let sets: [&[usize]; 4] =
            [&[2, 3], &[2, 3, 5], &[2, 3, 5, 7], &[2, 3, 5, 7, 11, 13]];
        let apart: Vec<usize> = sets.iter().map(|ls| pairs(ls, false)).collect();
        let shared: Vec<usize> = sets.iter().map(|ls| pairs(ls, true)).collect();
        assert_eq!((apart, shared), (vec![10, 17, 26, 54], vec![12, 22, 34, 62]));
    }

    // A name a subtrahend takes nothing from stays the name, at every
    // depth, and a member it takes whole leaves nothing beside it: the
    // residual of two recursive unions is built of their names, so a pair
    // the walk left is the pair it comes back to.
    #[test]
    fn diff_keeps_a_name_it_takes_nothing_from() {
        let defs = [
            ("T0", "[Error<[T0, `A1]>, Error<T0>, `A0]"),
            ("T3", "[Error<T3>, null, `A3]"),
            ("U0", "[Error<[U0, U1]>, Error<U1>, `B0]"),
            ("U1", "[Error<U1>, `B1]"),
        ];
        let (env, t) = typed(&defs, "T0");
        let less = |t: &Type, less: &str| t.diff(&env, &parsed(&env, less)).unwrap();
        super::DIFF_PAIRS.with(|n| n.set(0));
        assert_eq!(less(&t, "U0").to_string(), "T0");
        assert_eq!(super::DIFF_PAIRS.with(|n| n.get()), 17);
        assert_eq!(less(&t, "i64").to_string(), "T0");
        assert_eq!(less(&parsed(&env, "[T0, T3]"), "T3").to_string(), "T0");
        assert_eq!(
            less(&parsed(&env, "Error<[T0, T3]>"), "Error<T3>").to_string(),
            "Error<T0>"
        );
    }

    // Names recurring through Error payloads leave a residual built of
    // them, well inside the walk's pair budget.
    #[test]
    fn diff_over_names_recurring_through_errors_ends() {
        let defs: Vec<(String, String)> = [
            ("At", "[Error<Bt>, Error<Ct>, `R]"),
            ("Bt", "[Error<At>, `P]"),
            ("Ct", "[Array<Ct>, Error<Bt>]"),
            ("H", "[Error<Error<K>>, `Q]"),
            ("K", "[Error<H>, Error<K>, `P]"),
        ]
        .iter()
        .map(|(n, b)| (n.to_string(), b.to_string()))
        .collect();
        let rest = "[Error<Ct>, Error<Error<At>>, `R]".to_string();
        assert_eq!(diff_pairs(&defs, "At", "K"), (rest, 58));
    }

    /// How deep `t` nests Errors, each shared node walked once.
    fn errors_nested(t: &Type) -> usize {
        fn go(t: &Type, seen: &mut AHashMap<usize, usize>) -> usize {
            let node = node_addr(t);
            if let Some(d) = node.and_then(|n| seen.get(&n)) {
                return *d;
            }
            let mut below = 0;
            t.for_each_child(&mut |c| below = below.max(go(c, seen)));
            let d = below + matches!(t, Type::Error(_)) as usize;
            if let Some(n) = node {
                seen.insert(n, d);
            }
            d
        }
        go(t, &mut AHashMap::default())
    }

    /// The pairs each of `arms` unfolds, taken in turn off `x`, and how
    /// deep what it leaves nests Errors.
    fn arms_cost(defs: &[(&str, &str)], x: &str, arms: &[&str]) -> Vec<(usize, usize)> {
        let (env, mut t) = typed(defs, x);
        let mut costs = vec![];
        for arm in arms {
            super::DIFF_PAIRS.with(|n| n.set(0));
            t = t.diff(&env, &parsed(&env, arm)).unwrap();
            costs.push((super::DIFF_PAIRS.with(|n| n.get()), errors_nested(&t)));
        }
        costs
    }

    // An Error whose payload's difference holds a minuend answered for a
    // pair met again is rebuilt around it ERRORS_OVER_CUT deep and kept
    // whole beyond.
    #[test]
    fn a_residual_nests_few_errors_over_a_pair_met_again() {
        let defs = [("T", "[Error<T>, `A, `C]"), ("U", "[Error<U>, `B, `C]")];
        let (env, _) = typed(&defs, "T");
        let nested = |name: &str, d: usize| {
            parsed(&env, &format!("{}{name}{}", "Error<".repeat(d), ">".repeat(d)))
        };
        let less = |d: usize| nested("T", d).diff(&env, &nested("U", d)).unwrap();
        assert_eq!(less(0).to_string(), "[Error<T>, `A]");
        assert_eq!(
            less(ERRORS_OVER_CUT).to_string(),
            nested("[Error<T>, `A]", ERRORS_OVER_CUT).to_string()
        );
        let whole = nested("T", ERRORS_OVER_CUT + 1);
        assert_eq!(less(ERRORS_OVER_CUT + 1).to_string(), whole.to_string());
    }

    // Arms testing recursive unions over a value of others, each taking a
    // part of what reaches it through Errors: every walk ends inside its
    // pair budget, on a residual a few Errors deep.
    #[test]
    fn residuals_over_two_families_of_recursive_unions_stay_shallow() {
        let through_arrays = [
            ("T0", "[Array<T1>, Error<T1>, Error<T0>, `A0]"),
            ("T1", "[Array<T1>, (T1, T0), Error<T1>, `A1]"),
            ("T2", "[Error<T2>, Error<[T1, T2]>, Array<T0>, `A2]"),
            ("U0", "[Array<U1>, Error<U1>, Error<U0>, `A0]"),
            ("U1", "[(U1, U0), Error<U1>, `A1]"),
            ("U2", "[Error<[U1, U2]>, Array<U0>, `A2]"),
        ];
        let costs = arms_cost(&through_arrays, "T2", &["U2", "Error<U1>"]);
        assert_eq!(costs, vec![(60, 1), (13, 4)]);
        let one_family = [
            ("T0", "[Error<T2>, Error<T3>, Error<T0>, `A0]"),
            ("T2", "[Error<[T2, T2]>, Array<T2>, `P2(T0), `A2]"),
            ("T3", "[Error<T0>, Error<[T0, T2]>, `A3]"),
        ];
        assert_eq!(arms_cost(&one_family, "T0", &["T3"]), vec![(479, 1)]);
        let four_a_side = [
            ("T0", "[Error<T2>, i64, Error<[T3, T0]>, `A0]"),
            ("T1", "[string, Error<T0>, `A1]"),
            ("T2", "[string, Error<T3>, Error<T2>, Array<T3>, `A2]"),
            ("T3", "[`P0(T1), Error<T2>, (T3, T1), Error<Error<T3>>, `A3]"),
            ("U0", "[Error<U2>, i64, Error<[U3, U0]>, `B0, null]"),
            ("U1", "[string, Error<U0>, `B1]"),
            ("U2", "[string, Error<U3>, Error<U2>, Array<U3>, `B2]"),
            ("U3", "[`P0(U1), (U3, U1), Error<Error<U3>>, `B3]"),
        ];
        let arms = ["U1", "Error<U2>", "U3", "U1", "Error<U2>"];
        let costs = arms_cost(&four_a_side, "T0", &arms);
        assert_eq!(costs, vec![(113, 4), (70, 4), (45, 4), (113, 4), (70, 4)]);
    }

    /// `T0 .. Tn-1`, each `[Error<Ti+1>, Error<Ti+2>, `Ai]` (indices mod
    /// n), and `U0 .. Un-1` alike with `` `Bi ``: unions whose paths
    /// branch at every level.
    fn branching(n: usize) -> Vec<(String, String)> {
        let mut defs = vec![];
        for (name, tag) in [("T", "A"), ("U", "B")] {
            for i in 0..n {
                let (a, b) = ((i + 1) % n, (i + 2) % n);
                let body = format!("[Error<{name}{a}>, Error<{name}{b}>, `{tag}{i}]");
                defs.push((format!("{name}{i}"), body));
            }
        }
        defs
    }

    /// The steps diff takes (every part pair it walks) for `t` less `less`.
    fn diff_steps(defs: &[(String, String)], t: &str, less: &str) -> usize {
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, t);
        super::DIFF_STEPS.with(|n| n.set(0));
        t.diff(&env, &parsed(&env, less)).unwrap();
        super::DIFF_STEPS.with(|n| n.get())
    }

    // Over branching recursive unions, an Error nesting `d` deep takes steps
    // linear in `d`, and a family of `n` definitions stays within the pairs
    // the walk may unfold.
    #[test]
    fn diff_over_branching_unions_is_bounded() {
        let nested = |d: usize| format!("{}`C{}", "Error<".repeat(d), ">".repeat(d));
        let deep: Vec<usize> = [8, 16]
            .iter()
            .map(|&d| diff_steps(&branching(2), "T0", &nested(d)))
            .collect();
        let wide: Vec<usize> =
            [8, 16].iter().map(|&n| diff_steps(&branching(n), "T0", "U0")).collect();
        assert_eq!((deep, wide), (vec![99, 195], vec![14001, 14001]));
    }

    // could_match over a branching family answers an overlap once it has
    // unfolded its pairs.
    #[test]
    fn could_match_over_branching_unions_is_bounded() {
        let defs: Vec<(String, String)> = branching(16);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "T0");
        MATCH_PAIRS.with(|n| n.set(0));
        let could = t.could_match(&env, &parsed(&env, "U0")).unwrap();
        let pairs = MATCH_PAIRS.with(|n| n.get());
        assert_eq!((could, pairs), (true, 14));
    }

    // A select over a DAG of named unions (Li = [Li-1, Mi-1], Mi = [Mi-1,
    // Li-1]) takes an arm's test off each name once, and refutes a test
    // nothing the names hold could match once per name, not once per path.
    #[test]
    fn diff_and_could_match_take_each_name_of_a_union_dag_once() {
        let defs = union_dag(20);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L20");
        super::DIFF_PAIRS.with(|n| n.set(0));
        let rest = t.diff(&env, &parsed(&env, "`A")).unwrap();
        assert_eq!(rest.to_string(), "[`B, `C]");
        assert_eq!(super::DIFF_PAIRS.with(|n| n.get()), 39);
        MATCH_PAIRS.with(|n| n.set(0));
        assert!(!parsed(&env, "`D(i64)").could_match(&env, &t).unwrap());
        assert_eq!(MATCH_PAIRS.with(|n| n.get()), 39);
    }

    // Past the walk's budget a difference is wider than exact, and what it
    // left out is no measure of what the subtrahend holds (the difference
    // from it loses the error behind a name): diff_exact says so.
    #[test]
    fn diff_exact_past_the_budget_is_none() {
        let names: Vec<(String, String)> = (0..WALK_PAIRS + 100)
            .map(|i| (format!("N{i}"), format!("`N{i}")))
            .chain([("Zerr".to_string(), "Error<`E>".to_string())])
            .collect();
        let defs: Vec<(&str, &str)> = names.iter().map(|(n, b)| (&**n, &**b)).collect();
        let all = names.iter().map(|(n, _)| &**n).collect::<Vec<_>>().join(", ");
        let (env, t) = typed(&defs, &format!("[{all}, null]"));
        let error = Type::Primitive(Typ::Error.into());
        assert!(t.diff_exact(&env, &error).unwrap().is_none());
        let twice = t.diff(&env, &t.diff(&env, &error).unwrap()).unwrap();
        assert!(!twice.to_string().contains("Zerr"));
    }

    // Over names that each name the two before them, the paths to the
    // first double at every level: each name is expanded once, and a
    // member two names hold is one member, where it was first met.
    #[test]
    fn dispatch_members_expand_each_name_once() {
        let mut names = vec![
            ("L0".to_string(), "[`A, null, Error<`E>]".to_string()),
            ("M0".to_string(), "[`B, null]".to_string()),
        ];
        for i in 1..=40 {
            names.push((format!("L{i}"), format!("[L{}, M{}]", i - 1, i - 1)));
            names.push((format!("M{i}"), format!("[M{}, L{}]", i - 1, i - 1)));
        }
        let defs: Vec<(&str, &str)> = names.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L40");
        let members = Type::dispatch_members(&env, &t).unwrap();
        let printed = format_with_flags(PrintFlag::DerefTVars, || {
            members.iter().map(|m| m.to_string()).collect::<Vec<_>>().join(", ")
        });
        assert_eq!(printed, "null, Error<`E>, `A, `B");
    }

    // Two spellings of one type are one member, and a member another
    // holds stands before it, whichever was met first.
    #[test]
    fn dispatch_arms_are_one_per_type_narrower_first() {
        let defs = [
            ("X", "[i64, string]"),
            ("Wide", "[Array<[i64, string]>, null]"),
            ("Narrow", "[Array<i64>, Array<X>, string]"),
            ("Zed", "[Array<i64>, Array<X>, string]"),
        ];
        let printed = |t: &str| {
            let (env, t) = typed(&defs, t);
            let members = Type::dispatch_arms(&env, &t).unwrap();
            format_with_flags(PrintFlag::DerefTVars, || {
                members.iter().map(|m| m.to_string()).collect::<Vec<_>>().join(", ")
            })
        };
        assert_eq!(printed("[Narrow, Wide]"), "string, Array<i64>, Array<X>, null");
        assert_eq!(
            printed("[Wide, Zed]"),
            "null, Array<i64>, Array<[i64, string]>, string"
        );
        assert_eq!(
            printed("[Wide, Array<[null, i64, string]>, Narrow]"),
            "string, Array<i64>, Array<X>, null, Array<[i64, null, string]>"
        );
    }

    // A member whose every value the arms before it take is no arm, as
    // the select over the arms would find it dead.
    #[test]
    fn dispatch_arms_leave_out_a_member_the_arms_before_cover() {
        let defs = [
            ("P", "[Error<`A>, null]"),
            ("Q", "[Error<`B>, string]"),
            ("R", "[Error<[`A, `B]>, i64]"),
            ("AB", "Error<[`A, `B]>"),
        ];
        let printed = |t: &str| {
            let (env, t) = typed(&defs, t);
            let members = Type::dispatch_arms(&env, &t).unwrap();
            format_with_flags(PrintFlag::DerefTVars, || {
                members.iter().map(|m| m.to_string()).collect::<Vec<_>>().join(", ")
            })
        };
        assert_eq!(printed("[R, Q, P]"), "null, Error<`A>, string, Error<`B>, i64");
        assert_eq!(printed("[AB, Error<`A>, Error<`B>]"), "Error<`A>, Error<`B>");
    }

    // Two members cross where one value may be of both and neither holds
    // the other; the empty array is a value of every array type.
    #[test]
    fn dispatch_crossing_is_a_shared_value_neither_member_holds() {
        let defs = [
            ("S", "Array<[i64, string]>"),
            ("N", "Array<[i64, null]>"),
            ("Arr<'a>", "Array<'a>"),
            ("Rec<'a>", "{v: Array<'a>}"),
            ("L<'a>", "[`Cons('a, L<'a>), `Nil]"),
            ("P<'a>", "('a, i64)"),
        ];
        let crossing = |t: &str| {
            let (env, t) = typed(&defs, t);
            let members = Type::dispatch_members(&env, &t).unwrap();
            Type::dispatch_crossing(&env, &members).unwrap().is_some()
        };
        for t in [
            "[S, N]",
            "[Array<i64>, Array<string>]",
            "[List<i64>, List<string>]",
            "[Map<string, i64>, Map<string, string>]",
            "[(i64, [i64, string]), ([i64, null], i64)]",
            "[Error<[`A, `B]>, Error<[`B, `C]>]",
            "[`T(Array<i64>), `T(Array<string>)]",
            "[{a: Array<i64>}, {a: Array<string>}]",
            "[(i64, Array<i64>), (i64, Array<string>)]",
            "[(Arr<i64>, i64), (Arr<string>, i64)]",
            "[(Rec<i64>, i64), (Rec<string>, i64)]",
            "[(L<i64>, i64), (L<string>, i64)]",
            "[Error<Arr<i64>>, Error<Arr<string>>]",
        ] {
            assert!(crossing(t), "{t}")
        }
        for t in [
            "[Array<i64>, S]",
            "[Array<i64>, null, string]",
            "[(i64, i64), (string, string)]",
            "[(i64, i64), (i64, [i64, string])]",
            "[Error<`A>, Error<`B>, Error<[`A, `B]>]",
            "[`T(Array<i64>), `U(Array<string>)]",
            "[{a: Array<i64>}, {b: Array<string>}]",
            "[Array<i64>, Map<string, i64>, List<i64>]",
            "[(i64, Array<i64>), (string, Array<string>)]",
            "[(Arr<i64>, i64), (Arr<string>, string)]",
            "[(L<i64>, i64), (L<string>, string)]",
            "[(P<i64>, i64), (P<string>, i64)]",
            "[L<i64>, L<string>]",
        ] {
            assert!(!crossing(t), "{t}")
        }
    }

    // One definition applied to two open cells is two types, each
    // expanded; applied to one cell twice, it is expanded once.
    #[test]
    fn dispatch_members_expand_a_name_per_open_cell() {
        let (env, t) = typed(&[("F<'t>", "['t, null]")], "(F<'a>, F<'b>, F<'a>)");
        t.alias_tvars(&mut AHashMap::default());
        let Type::Tuple(members) = &t else { unreachable!() };
        let t = Type::Set(members.clone());
        let members = Type::dispatch_members(&env, &t).unwrap();
        let printed = format_with_flags(PrintFlag::DerefTVars, || {
            members.iter().map(|m| m.to_string()).collect::<Vec<_>>().join(", ")
        });
        assert_eq!(printed, "null, 'a: unbound, 'b: unbound");
    }

    /// `T` and `U`, each `[Error<T>, Error<U>, tag]`, `T` less an Error
    /// tower `d` deep of `` `A ``: a residual whose paths double at every
    /// level, its parts shared.
    fn residual(d: usize) -> (Env, Type) {
        let defs = [("T", "[Error<T>, Error<U>, `A]"), ("U", "[Error<T>, Error<U>, `B]")];
        let (env, t) = typed(&defs, "T");
        let tower = format!("{}`A{}", "Error<".repeat(d), ">".repeat(d));
        let r = t.diff(&env, &parsed(&env, &tower)).unwrap();
        (env, r)
    }

    /// The pairs and the steps diff takes for `t` less `less`, and the
    /// difference, `None` where a budget widened it.
    fn diff_cost(env: &Env, t: &Type, less: &str) -> (usize, usize, Option<Type>) {
        super::DIFF_PAIRS.with(|n| n.set(0));
        super::DIFF_STEPS.with(|n| n.set(0));
        let rest = t.diff_exact(env, &parsed(env, less)).unwrap();
        let pairs = super::DIFF_PAIRS.with(|n| n.get());
        (pairs, super::DIFF_STEPS.with(|n| n.get()), rest)
    }

    // A select's next arm subtracts from the residual the arms before it
    // left, a DAG whose paths double at every level: each pair of its
    // shared parts is walked once, through a named pair in progress below
    // it, so the cost grows with the depth and the difference stays exact.
    #[test]
    fn diff_of_a_shared_residual_walks_each_part_once() {
        let costs: Vec<_> = [8, 12, 16]
            .iter()
            .map(|&d| {
                let (env, r) = residual(d);
                let (pairs, _, rest) = diff_cost(&env, &r, "Error<U>");
                let tower = format!("{}`B{}", "Error<".repeat(d + 1), ">".repeat(d + 1));
                let (_, steps, deeper) = diff_cost(&env, &r, &tower);
                let rest = rest.map(|r| r.to_string());
                (pairs, rest, steps, deeper.is_some())
            })
            .collect();
        let rest = Some("[Error<`A>, `A]".to_string());
        assert_eq!(
            costs,
            vec![
                (17, rest.clone(), 148, true),
                (25, rest.clone(), 198, true),
                (33, rest, 245, true)
            ]
        );
    }

    // Over residuals of the doubling family, where a pair's difference is
    // kept only while the path holds nothing its walk reached, the memos
    // change no answer.
    #[test]
    fn memos_change_no_answer_over_shared_residuals() {
        let (env, _) = residual(0);
        let mut pool: Vec<Type> = [2, 3, 5]
            .iter()
            .map(|&d| {
                let tower = format!("{}`A{}", "Error<".repeat(d), ">".repeat(d));
                parsed(&env, "T").diff(&env, &parsed(&env, &tower)).unwrap()
            })
            .collect();
        for t in ["T", "U", "Error<U>", "Error<Error<`B>>", "[Error<T>, `A]"] {
            pool.push(parsed(&env, t));
        }
        memos_agree(&env, &pool);
    }

    // A difference kept for a pair of shared unions is read again only
    // where the path holds nothing its walk reached: V's rest less U's,
    // each a residual of shared parts, meets a part first off the path of
    // a pair its walk enters and again beneath that pair.
    #[test]
    fn memos_change_no_answer_over_shared_parts() {
        let (env, _) = typed(
            &[
                ("T", "[`C, Array<V>]"),
                ("U", "[Error<Error<U>>, Error<V>, Error<T>, `A]"),
                ("V", "[Array<W>, Error<Error<U>>, `B]"),
                ("W", "[Error<Error<T>>, Error<U>, List<T>, List<W>, `C]"),
            ],
            "null",
        );
        let less = |t: &str, arms: &[&str]| {
            arms.iter()
                .fold(parsed(&env, t), |r, a| r.diff(&env, &parsed(&env, a)).unwrap())
        };
        let pool = [
            less(
                "U",
                &[
                    "`A",
                    "[Error<List<`B>>, Error<T>]",
                    "List<U>",
                    "Error<`B>",
                    "Error<Error<Error<List<Error<Error<Error<`B>>>>>>>",
                ],
            ),
            less("V", &["Error<Array<Array<Error<Error<Error<`B>>>>>>"]),
        ];
        memos_agree(&env, &pool);
    }

    /// A chain of `n` links ending in i64, each link `[null, Array<next>]`,
    /// the links from the top; `cells` reaches each next link through a
    /// cell bound to it.
    fn identity_chain(n: usize, cells: bool) -> Vec<Type> {
        let mut links = vec![Type::Primitive(Typ::I64.into())];
        for i in 0..n {
            let next = links.last().unwrap().clone();
            let next = match cells {
                false => next,
                true => {
                    let c = TVar::empty_named(arcstr::format!("c{i}"));
                    c.bind(next);
                    Type::TVar(c)
                }
            };
            links.push(Type::Set(Arc::from_iter([
                Type::Primitive(Typ::Null.into()),
                Type::Array(Arc::new(next)),
            ])));
        }
        links.reverse();
        links
    }

    /// The steps [`IdenticalMemo`] takes to find each link of a chain of
    /// `n` identical to its twin's, top first, as an elaboration asks of
    /// a signature copy's cells.
    fn identical_chain_steps(n: usize, memo: bool) -> usize {
        let (l, r) = (identity_chain(n, true), identity_chain(n, false));
        super::NO_MEMO.with(|m| m.set(!memo));
        super::IDENTITY_STEPS.with(|s| s.set(0));
        let mut m = IdenticalMemo::default();
        for (a, b) in l.iter().zip(r.iter()) {
            assert_eq!(m.identical(a, b), Identical::Same);
        }
        super::NO_MEMO.with(|m| m.set(false));
        super::IDENTITY_STEPS.with(|s| s.get())
    }

    // Each question walks the rest of the chain; the memo keeps the pairs
    // an earlier one found, so the chain is walked once.
    #[test]
    fn identical_links_of_a_chain_walk_it_once() {
        let steps = [100, 200, 400].map(|n| identical_chain_steps(n, true));
        assert_eq!(steps, [6357, 13257, 27057]);
        let without = [100, 200, 400].map(|n| identical_chain_steps(n, false));
        assert_eq!(without, [20301, 80601, 321201]);
    }

    /// `(i64, .., i64, f)`, enough leaves first that the walk keys `f`.
    pub(crate) fn past_eager(f: &Type) -> Type {
        let leaves = (0..NodePairs::EAGER).map(|_| Type::Primitive(Typ::I64.into()));
        Type::Tuple(Arc::from_iter(leaves.chain([f.clone()])))
    }

    // Two function types identical only by pairing their own cells: a
    // later question meeting them again finds them renamed too, so the
    // caller links their cells.
    #[test]
    fn identical_memo_keeps_no_pair_found_by_renaming() {
        let f = |v: &str| parse_type(&format!("fn(x: '{v}) -> '{v}")).unwrap();
        let (f0, f1) = (f("a"), f("b"));
        let mut m = IdenticalMemo::default();
        for _ in 0..2 {
            assert_eq!(
                m.identical(&past_eager(&f0), &past_eager(&f1)),
                Identical::Renamed
            );
        }
    }

    /// `(fn(q: 'a) -> 'a, leaf)` under `n` links, each a pair of one node.
    fn fn_pair_dag(n: usize, leaf: Type) -> Type {
        let f = parse_type("fn(q: 'a) -> 'a").unwrap();
        f.alias_tvars(&mut AHashMap::default());
        let x0 = Type::Tuple(Arc::from_iter([f, leaf]));
        (0..n).fold(x0, |x, _| Type::Tuple(Arc::from_iter([x.clone(), x])))
    }

    /// The meets the union of alternatives takes over two pair DAGs of
    /// `n` links ([`fn_pair_dag`], over `i64` and over `Any`), and its
    /// two members.
    fn pair_dag_meet(n: usize, memo: bool) -> (usize, [Type; 2]) {
        let x = fn_pair_dag(n, Type::Primitive(Typ::I64.into()));
        let y = fn_pair_dag(n, Type::Any);
        super::NO_MEMO.with(|m| m.set(!memo));
        super::MEET_STEPS.with(|s| s.set(0));
        let u = Type::union_of_alternatives(&Env::default(), &[&x, &y]).unwrap();
        super::NO_MEMO.with(|m| m.set(false));
        let Type::Set(ms) = &u else { panic!("two members: {u}") };
        let [m0, m1] = &ms[..] else { panic!("two members: {u}") };
        (super::MEET_STEPS.with(|s| s.get()), [m0.clone(), m1.clone()])
    }

    /// The nodes `t` holds, each once.
    fn nodes(t: &Type, seen: &mut AHashSet<NodeKey>) {
        if t.node_key().is_some_and(|k| seen.insert(k))
            && let Type::Tuple(ps) = t
        {
            ps.iter().for_each(|p| nodes(p, seen))
        }
    }

    /// The paths of `m0` and `m1` that end in one function node, the
    /// meet of their alternatives' functions.
    fn met_paths(m0: &Type, m1: &Type) -> usize {
        match (m0, m1) {
            (Type::Tuple(p0), Type::Tuple(p1)) => {
                p0.iter().zip(p1.iter()).map(|(a, b)| met_paths(a, b)).sum()
            }
            (Type::Fn(_), Type::Fn(_)) => (m0.node_key() == m1.node_key()) as usize,
            _ => 0,
        }
    }

    // A select over two pair DAGs that each share their parts meets each
    // pair of nodes once, with the answer the walk of every path gives,
    // and each met member is a DAG as its alternative is.
    #[test]
    fn the_meet_of_two_pair_dags_takes_each_pair_once() {
        for (n, steps) in [(10, 93), (20, 121), (40, 183)] {
            assert_eq!(pair_dag_meet(n, true).0, steps, "{n} links");
        }
        let without = [4, 6, 8].map(|n| pair_dag_meet(n, false).0);
        assert_eq!(without, [63, 255, 1023]);
        let print = |m: &Type| format_with_flags(PrintFlag::DerefTVars, || m.to_string());
        let ([w0, w1], [o0, o1]) = (pair_dag_meet(7, true).1, pair_dag_meet(7, false).1);
        assert_eq!((print(&w0), print(&w1)), (print(&o0), print(&o1)));
        assert_eq!((met_paths(&w0, &w1), met_paths(&o0, &o1)), (128, 128));
        let [m0, m1] = pair_dag_meet(40, true).1;
        let sizes = [m0, m1].map(|m| {
            let mut seen = AHashSet::default();
            nodes(&m, &mut seen);
            seen.len()
        });
        assert_eq!(sizes, [99, 99]);
    }

    // A weld moves one function's cell into the other's; a var that shared
    // the moved cell reads on through the forward link the weld left.
    #[test]
    fn identical_reads_a_welded_cell_through_its_forward_link() {
        let f = |arg: &str, ret: &str| {
            let t = parse_type(&format!("fn(x: '{arg}) -> '{ret}")).unwrap();
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells[ret].alias(&cells[arg]);
            (t, cells[arg].clone(), cells[ret].clone())
        };
        let ((f0, a, r), (f1, b, _)) = (f("a", "r"), f("b", "s"));
        assert!(union_identical(&f0, &f1));
        crate::typ::contains::weld(&a, &b);
        assert!(a.same_cell(&b));
        assert!(!r.same_cell(&b) && r.stands_for() == b.stands_for());
        assert!(union_identical(&f0, &f1));
        assert!(union_identical(&f1, &f0));
    }

    // A pair found identical is not kept past a write to a cell it read.
    #[test]
    fn identical_memo_forgets_at_a_cell_write() {
        let (a, b) = (TVar::empty_named(literal!("a")), TVar::empty_named(literal!("b")));
        a.bind(Type::Primitive(Typ::I64.into()));
        b.bind(Type::Primitive(Typ::I64.into()));
        let (x, y) = (
            Type::Array(Arc::new(Type::TVar(a))),
            Type::Array(Arc::new(Type::TVar(b.clone()))),
        );
        let mut m = IdenticalMemo::default();
        assert_eq!(m.identical(&past_eager(&x), &past_eager(&y)), Identical::Same);
        b.bind(Type::Primitive(Typ::String.into()));
        assert_eq!(m.identical(&past_eager(&x), &past_eager(&y)), Identical::Apart);
    }

    // A walk that takes a kept agreement over cells reads those cells: its
    // verdict is not kept past a write either.
    #[test]
    fn identical_memo_keeps_nothing_built_on_an_agreement_over_cells() {
        let (a, b) = (TVar::empty_named(literal!("a")), TVar::empty_named(literal!("b")));
        a.bind(Type::Primitive(Typ::I64.into()));
        b.bind(Type::Primitive(Typ::I64.into()));
        let (x, y) = (
            Type::Array(Arc::new(Type::TVar(a))),
            Type::Array(Arc::new(Type::TVar(b.clone()))),
        );
        let list = |t: &Type| past_eager(&Type::List(Arc::new(t.clone())));
        let (xs, ys) = (list(&x), list(&y));
        let mut m = IdenticalMemo::default();
        assert_eq!(m.identical(&past_eager(&x), &past_eager(&y)), Identical::Same);
        assert_eq!(m.identical(&xs, &ys), Identical::Same);
        b.bind(Type::Primitive(Typ::String.into()));
        assert_eq!(m.identical(&xs, &ys), Identical::Apart);
    }

    /// An env defining `P<'x, 'y>` as `[Array<'x>, Array<'y>, null]`.
    fn links_env() -> Env {
        typed(&[("P<'x, 'y>", "[Array<'x>, Array<'y>, null]")], "null").0
    }

    /// `n` links over i64, the links from the top, each but the last two
    /// `P` applied to the two below it: a DAG that holds no cell, as an
    /// instance's signature over a chain of bounds through `P` is.
    fn application_links(env: &Env, n: usize) -> Vec<Type> {
        let mut links = vec![parsed(env, "i64"), parsed(env, "[Array<i64>, null]")];
        while links.len() < n {
            let [y, x] = links.last_chunk().unwrap().clone();
            let params = AHashMap::from_iter([(literal!("x"), x), (literal!("y"), y)]);
            links.push(parsed(env, "P<'x, 'y>").replace_tvars(&params));
        }
        links.reverse();
        links
    }

    /// The steps [`IdenticalMemo`] takes to ask every link of one of two
    /// twin DAGs of `n` links whether it is each link of the other, a cell
    /// written before each question, as a walk placing a union's members
    /// asks of them.
    fn identical_links_steps(n: usize, memo: bool) -> usize {
        let env = links_env();
        let (l, r) = (application_links(&env, n), application_links(&env, n));
        super::NO_MEMO.with(|m| m.set(!memo));
        super::IDENTITY_STEPS.with(|s| s.set(0));
        let mut m = IdenticalMemo::default();
        for (i, a) in l.iter().enumerate() {
            for (j, b) in r.iter().enumerate() {
                let writes = super::cell_writes();
                TVar::empty_named(literal!("w")).bind(Type::Primitive(Typ::I64.into()));
                assert!(super::cell_writes() > writes);
                let found = if i == j { Identical::Same } else { Identical::Apart };
                assert_eq!(m.identical(a, b), found);
            }
        }
        super::NO_MEMO.with(|m| m.set(false));
        super::IDENTITY_STEPS.with(|s| s.get())
    }

    // A verdict over two nodes that hold no cell stands through cell
    // writes, apart or identical, so each pair is walked once.
    #[test]
    fn identical_memo_walks_each_pair_of_cell_free_links_once() {
        let steps = [16, 32, 64].map(|n| identical_links_steps(n, true));
        assert_eq!(steps, [475, 1963, 8011]);
        let without = [16, 32, 64].map(|n| identical_links_steps(n, false));
        assert_eq!(without, [1460, 11124, 87796]);
    }

    // A difference found at a name whose resolution cell is open is not
    // kept: filled, the cell names the other side's definition.
    #[test]
    fn identical_memo_keeps_no_difference_read_at_an_open_name() {
        let env = links_env();
        let filled = parsed(&env, "P<i64, i64>");
        let open = parse_type("P<i64, i64>").unwrap().scope_refs(&ModPath::root());
        let mut m = IdenticalMemo::default();
        assert_eq!(
            m.identical(&past_eager(&filled), &past_eager(&open)),
            Identical::Apart
        );
        open.seed_refs(&env);
        assert_eq!(
            m.identical(&past_eager(&filled), &past_eager(&open)),
            Identical::Same
        );
    }

    /// The steps [`one_type`] takes to find each of `ms` one type with
    /// the first, as the pick asks of a union's members.
    fn one_type_steps(ms: &[Type]) -> usize {
        super::IDENTITY_STEPS.with(|s| s.set(0));
        for m in ms {
            assert!(one_type(&ms[0], m) && one_type(m, &ms[0]), "{} is {m}", ms[0]);
        }
        super::IDENTITY_STEPS.with(|s| s.get())
    }

    /// `n` arrays of eight nested unions of the one cell `a`, no two
    /// nested alike.
    fn arrays_of_one_cell(n: usize, a: &Type) -> Vec<Type> {
        let set = |ts: Vec<Type>| Type::Set(Arc::from_iter(ts));
        let nested = |i: usize| match i % 3 {
            0 => a.clone(),
            1 => set(vec![a.clone(), a.clone()]),
            _ => set(vec![set(vec![a.clone()]), a.clone()]),
        };
        (0..n)
            .map(|k| Type::Array(Arc::new(set((0..8).map(|i| nested(i + k)).collect()))))
            .collect()
    }

    // Members that are one type are found one in a walk of their size,
    // however many the union holds.
    #[test]
    fn one_type_walks_members_of_one_cell_in_their_size() {
        let a = Type::TVar(TVar::empty_named(literal!("a")));
        let steps = [100, 200, 400].map(|n| one_type_steps(&arrays_of_one_cell(n, &a)));
        assert_eq!(steps, [8582, 17252, 34582]);
    }

    /// The steps [`one_type`] takes, both ways, over two arrays of a union
    /// of `n` open cells: the second's in the reverse order (one type), or
    /// each lacking a cell the other holds (two).
    fn one_type_cell_steps(n: usize) -> [usize; 2] {
        let cells: Vec<Type> = (0..=n)
            .map(|i| Type::TVar(TVar::empty_named(arcstr::format!("c{i}"))))
            .collect();
        let array =
            |ts: &[Type]| Type::Array(Arc::new(Type::Set(ts.iter().cloned().collect())));
        let reversed: Vec<Type> = cells[..n].iter().rev().cloned().collect();
        [
            (array(&cells[..n]), array(&reversed), true),
            (array(&cells[1..]), array(&cells[..n]), false),
        ]
        .map(|(m0, m1, one)| {
            super::IDENTITY_STEPS.with(|s| s.set(0));
            assert_eq!(one_type(&m0, &m1) && one_type(&m1, &m0), one, "{m0}, {m1}");
            super::IDENTITY_STEPS.with(|s| s.get())
        })
    }

    // Two unions of open cells are compared in a walk of their width,
    // whatever order each holds its cells in and wherever they differ.
    #[test]
    fn one_type_walks_unions_of_cells_in_their_width() {
        let steps = [100, 200, 400].map(one_type_cell_steps);
        assert_eq!(steps, [[404, 202], [804, 402], [1604, 802]]);
    }

    // Cells bound to one cell are that cell, in a function type too, and a
    // cell bound to a type is that type; two open cells are two wherever
    // they stand, and so are two types that differ under a union.
    #[test]
    fn one_type_holds_every_cell_by_identity() {
        let set = |ts: &[&Type]| Type::Set(ts.iter().map(|t| (*t).clone()).collect());
        let array = |t: &Type| Type::Array(Arc::new(t.clone()));
        let var = |n: &'static str| TVar::empty_named(arcstr::ArcStr::from(n));
        let (a, b, x, y, z) = (var("a"), var("b"), var("x"), var("y"), var("z"));
        x.bind(Type::TVar(a.clone()));
        y.bind(Type::TVar(a.clone()));
        z.bind(Type::Primitive(Typ::I64.into()));
        let [a, b, x, y, z] = [a, b, x, y, z].map(Type::TVar);
        let f = |arg: &Type, ret: &Type| {
            let t = parse_type("fn(v: i64) -> i64").unwrap();
            let Type::Fn(f) = &t else { unreachable!() };
            let mut f = (**f).clone();
            let mut args = f.args.to_vec();
            args[0].typ = arg.clone();
            f.args = Arc::from(args);
            f.rtype = ret.clone();
            Type::Fn(Arc::new(f))
        };
        let (i64s, strs) =
            (Type::Primitive(Typ::I64.into()), Type::Primitive(Typ::String.into()));
        for (m0, m1) in [
            (x.clone(), y.clone()),
            (array(&set(&[&x, &y])), array(&a)),
            (f(&set(&[&a, &x]), &a), f(&a, &set(&[&y, &y]))),
            (set(&[&array(&x), &array(&set(&[&a, &a]))]), array(&y)),
            (
                Type::Tuple(Arc::from_iter([set(&[&a, &b]), a.clone()])),
                Type::Tuple(Arc::from_iter([set(&[&b, &x, &b]), y.clone()])),
            ),
            (set(&[&x, &i64s, &set(&[&b, &a])]), set(&[&i64s, &b, &y, &i64s])),
            (set(&[&z, &a]), set(&[&i64s, &x])),
            (set(&[&a, &array(&x)]), set(&[&array(&y), &y])),
            (set(&[&x, &set(&[&y, &a])]), a.clone()),
        ] {
            assert!(one_type(&m0, &m1) && one_type(&m1, &m0), "{m0} is {m1}");
        }
        for (m0, m1) in [
            (x.clone(), b.clone()),
            (f(&a, &a), f(&b, &b)),
            (f(&a, &b), f(&b, &a)),
            (f(&set(&[&a, &b]), &a), f(&a, &a)),
            (array(&set(&[&x, &i64s])), array(&a)),
            (set(&[&array(&a), &array(&b)]), array(&a)),
            (
                Type::Tuple(Arc::from_iter([set(&[&a, &b]), a.clone()])),
                Type::Tuple(Arc::from_iter([set(&[&b, &x, &b]), b.clone()])),
            ),
            (set(&[&a, &i64s]), set(&[&a, &strs])),
            (set(&[&a, &array(&a)]), set(&[&a, &array(&b)])),
            (set(&[&z, &a]), set(&[&z, &b])),
            (set(&[&a, &b]), a.clone()),
            (set(&[&a, &i64s]), set(&[&a])),
        ] {
            assert!(!one_type(&m0, &m1) && !one_type(&m1, &m0), "{m0} is not {m1}");
        }
    }
}
