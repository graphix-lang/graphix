use crate::{
    AddrSet,
    env::Env,
    stack::ensure_sufficient,
    typ::{
        Bound, Kept, Lazy, NodeKey, NodePairs, RefHist, RefPair, TVar, Type, TypeRef,
        contains::AmbiguousMember,
        node_addr,
        tvar::{StandingEnds, cell_writes},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{iter, mem, ops::ControlFlow, slice};
use triomphe::Arc;

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
pub(super) struct Unfoldings(Lazy<AHashMap<DefPair, SmallVec<[RefPair; 1]>>>);

impl Unfoldings {
    /// The innermost pair in progress of `defs` when the walk has unfolded
    /// it [`UNFOLDINGS`] times.
    pub(super) fn again(&self, defs: Option<DefPair>) -> Option<RefPair> {
        self.0
            .get()?
            .get(&defs?)
            .filter(|ks| ks.len() >= UNFOLDINGS)
            .and_then(|ks| ks.last().copied())
    }

    pub(super) fn enter(&mut self, defs: Option<DefPair>, key: RefPair) {
        if let Some(defs) = defs {
            self.0.get_mut().entry(defs).or_default().push(key)
        }
    }

    pub(super) fn leave(&mut self, defs: Option<DefPair>) {
        if let Some(ks) = defs.and_then(|defs| self.0.0.as_mut()?.get_mut(&defs)) {
            ks.pop();
        }
    }

    /// The pairs of definitions the path holds.
    fn held(&self) -> impl Iterator<Item = DefPair> + '_ {
        self.0
            .get()
            .into_iter()
            .flatten()
            .filter(|(_, ks)| !ks.is_empty())
            .map(|(d, _)| *d)
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

    /// Whether `self` and `other` are one constraint: they differ by a
    /// renaming of their open cells alone ([`renames`]), and each cell
    /// stands at its image's level, rigidity and lower bounds. A walk
    /// over either reads the same.
    pub fn one_constraint(&self, env: &Env, other: &Type) -> bool {
        if !renames(env, self, other) {
            return false;
        }
        let cells = |t: &Type| {
            let cells = std::cell::RefCell::new(SmallVec::<[TVar; 4]>::new());
            t.first_open(&|tv| {
                cells.borrow_mut().push(tv.clone());
                false
            });
            cells.into_inner()
        };
        let (a, b) = (cells(self), cells(other));
        a.len() == b.len()
            && a.iter().zip(b.iter()).all(|(x, y)| {
                let (lx, ly) = (x.cell_lower(), y.cell_lower());
                x.is_rigid() == y.is_rigid()
                    && x.level().is_generic() == y.level().is_generic()
                    && lx.len() == ly.len()
                    && lx.iter().zip(ly.iter()).all(|(p, q)| strictly_identical(p, q))
            })
    }

    /// Whether a walk of `self` is one of `members`' ([`walks_alike`]).
    pub fn walks_as_one_of(&self, members: &[Type], ends: &mut StandingEnds) -> bool {
        members.iter().any(|m| walks_alike(m, self, ends))
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

/// Whether each of `a` is [`strictly_identical`] to its own member of `b`,
/// whatever the order the members stand in.
pub(super) fn strictly_same_unordered(a: &[Type], b: &[Type]) -> bool {
    if a.len() < 2 || a.len() != b.len() {
        return false;
    }
    let mut taken: SmallVec<[bool; 8]> = smallvec![false; b.len()];
    a.iter().all(|m| {
        let found = b
            .iter()
            .enumerate()
            .find(|(j, n)| !taken[*j] && strictly_identical(m, n))
            .map(|(j, _)| j);
        found.is_some_and(|j| {
            taken[j] = true;
            true
        })
    })
}

/// [`union_identical`] with every unbound cell by identity, inside a
/// function type too: one signature's variables are its own.
pub(crate) fn strictly_identical(t0: &Type, t1: &Type) -> bool {
    Identity { strict: true, ..Identity::new(None, &mut NodePairs::default()) }
        .same(t0, t1)
}

/// [`one_type`] with a union's members but its open cells read in order:
/// a narrowing walks two such types alike, where a union of two members
/// one type with each other walks apart from one of them.
pub(super) fn walks_alike(t0: &Type, t1: &Type, ends: &mut StandingEnds) -> bool {
    Identity {
        strict: true,
        sets: Some(ends),
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

/// What a union's collapse of its members carries: the alternatives of the
/// join it is for, and the first ambiguity it met.
#[derive(Default)]
pub(super) struct Collapsing {
    /// The alternatives of the join the collapse is for.
    pub(super) alts: Alternatives,
    /// The first pairing of unions by position a collapse met that another
    /// pairing would have made as cheaply.
    pub(super) ambiguity: Option<Box<Ambiguity>>,
}

impl Collapsing {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn joining(alts: SmallVec<[Type; 4]>) -> Self {
        Self { alts: Alternatives::new(alts), ambiguity: None }
    }

    /// What a collapse that may yet fail starts from ([`Self::rewind`]):
    /// whether an ambiguity was met before it.
    pub(super) fn mark(&self) -> bool {
        self.ambiguity.is_some()
    }

    /// Forget the ambiguity a failed collapse met.
    pub(super) fn rewind(&mut self, ambiguous: bool) {
        if !ambiguous {
            self.ambiguity = None
        }
    }

    /// Take the ambiguity `other` met.
    pub(super) fn absorb(&mut self, other: &mut Collapsing) {
        if self.ambiguity.is_none() {
            self.ambiguity = other.ambiguity.take()
        }
    }
}

/// The alternatives of a join, with the nodes each holds read once while
/// this thread writes no cell.
#[derive(Default)]
pub(super) struct Alternatives {
    alts: SmallVec<[Type; 4]>,
    held: Option<(u64, SmallVec<[LPooled<AHashSet<NodeKey>>; 4]>)>,
}

impl Alternatives {
    pub(super) fn new(alts: SmallVec<[Type; 4]>) -> Self {
        Self { alts, held: None }
    }

    /// Whether one alternative holds both `t0` and `t1`: two members of one
    /// alternative are two, however alike.
    // CR claude for claude: [perf/medium] apart scans the held-node set of every
    // alternative of the join, and union_tagged collapses each member into the
    // accumulator through it. A union of N same-shaped composite alternatives therefore
    // costs N² probes; alternatives() also groups members by tag with a linear find. An
    // array literal of 8000 structs (or tuples, or one-element arrays) takes 146 ms of
    // Typecheck0, 4x per doubling, against 0.2 ms for 8000 integers, and `any` and
    // select arms over composites pay alike. element_union's dedupe catches only
    // primitives and shared Arcs, never separately written literals, and
    // a_literals_element_union_is_linear pins only i64. Index the held nodes by key
    // when `held` is built (key to the alternatives holding it), so apart is two
    // lookups; group alternatives() by tag in a map; pin a struct-literal array. probe:
    // design/review-2026-11-16/repro/x-perf-compile-composite-literal-union.py
    // (x-perf-compile-04)
    pub(super) fn apart(&mut self, t0: &Type, t1: &Type) -> bool {
        let (Some(k0), Some(k1)) = (t0.node_key(), t1.node_key()) else {
            return false;
        };
        if self.alts.is_empty() {
            return false;
        }
        let writes = cell_writes();
        if self.held.as_ref().is_none_or(|(at, _)| *at != writes) {
            #[cfg(test)]
            HELD_READS.with(|n| n.set(n.get() + 1));
            let nodes = self.alts.iter().map(|a| held_nodes(a)).collect();
            self.held = Some((writes, nodes));
        }
        let (_, nodes) = self.held.as_ref().unwrap();
        nodes.iter().any(|n| n.contains(&k0) && n.contains(&k1))
    }
}

/// [`union_identical`], for a collapse that keeps `t0` in place of `t1`:
/// two live cells are identical only where they are one, and two members
/// of one alternative of the join `collapsing` is for collapse only where
/// they are identical as they stand.
pub(super) fn union_collapses(t0: &Type, t1: &Type, collapsing: &mut Collapsing) -> bool {
    let mark = collapsing.mark();
    let mut same =
        Identity::new(Some(collapsing), &mut NodePairs::default()).same(t0, t1);
    if same && collapsing.alts.apart(t0, t1) && !strictly_identical(t0, t1) {
        same = false;
    }
    if !same {
        collapsing.rewind(mark)
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
}

/// A union whose members a walk paired with another union's by position,
/// where `typ`, one of the other's members, could as well have gone to
/// either of `members`.
pub(super) struct Ambiguity {
    pub(super) union: SmallVec<[Type; 4]>,
    pub(super) typ: Type,
    pub(super) members: [Type; 2],
}

impl Ambiguity {
    pub(super) fn refusal(&self) -> anyhow::Error {
        AmbiguousMember::new(
            &self.union,
            &self.typ,
            &[&self.members[0], &self.members[1]],
        )
    }
}

/// Identity of `t0` and `t1`.
struct Identity<'a> {
    // CR claude for claude: [structure/low] Six flags and options (`strict`, `sets`,
    // `in_order`, `by_name`, `unbounded`, `across`) spell the walks the constructors
    // below build, and most of their other combinations mean nothing: `sets` needs
    // `strict` (its doc), `in_order` needs `sets`, `by_name` needs `pairs` to be `Some`
    // from the start, `unbounded` acts only with `live`. Nothing keeps a new caller
    // from building one of those, and `another_pairing` copies all six by hand. One
    // enum of how two open cells pair (by identity; by identity over sets, in order or
    // not; by position; by position across a scheme; by name; welded, unbounded or not)
    // in place of the six fields makes the others unrepresentable and turns the tests
    // in `cells` and at the head of `same_inner` into one match. Found by reading; not
    // run. (t-setops-11)
    strict: bool,
    /// A union is the set of its members ([`one_type`]), its open cells
    /// read through where their forwarding chains end; with `strict`
    /// alone, since a failed search for a member must pair no cells.
    sets: Option<&'a mut StandingEnds>,
    /// With `sets`, a union's members but its open cells in order
    /// ([`walks_alike`]).
    in_order: bool,
    /// Two open cells of one name are identical, whatever holds them
    /// (`pairs` holds those met, never `None`).
    by_name: bool,
    /// A scheme's cell pairs with a live one too ([`renames`]).
    across: bool,
    /// The union collapse this walk is for, where an ambiguity goes; two
    /// live cells are identical there only where they are one.
    collapsing: Option<&'a mut Collapsing>,
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
    /// How many pairs of distinct cells were paired.
    paired: usize,
    /// The first pairing of unions the walk took by position that another
    /// pairing would have made as cheaply.
    ambiguity: Option<Box<Ambiguity>>,
    /// Where the verdicts of pairs whose walk read no cell go: a plain
    /// walk's alone ([`IdenticalMemo`]), whose every cell read is a
    /// [`Self::same_inner`] of a variable or of a name.
    settled: Option<&'a mut Settled>,
    /// The pair being walked has read a cell so far.
    read_cell: bool,
}

impl<'a> Identity<'a> {
    fn new(collapsing: Option<&'a mut Collapsing>, found: &'a mut NodePairs) -> Self {
        Self {
            strict: false,
            sets: None,
            in_order: false,
            by_name: false,
            across: false,
            collapsing,
            env: None,
            pairs: None,
            found,
            found_in_fn: NodePairs::default(),
            renamed: false,
            paired: 0,
            ambiguity: None,
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

    /// Whether the unions `a` and `b` are one, member by member in their
    /// order, unless the members are the same types in another order: a
    /// pairing by position that renames cells yields to the pairing of
    /// members that are cell for cell the same ([`strictly_same_unordered`]),
    /// which renames none. A pairing by position that renames cells where
    /// the members pair some other way at no more renamed cells is the
    /// walk's [`Ambiguity`]: which members are the same is for the program
    /// to say.
    fn same_unions(&mut self, a: &[Type], b: &[Type]) -> bool {
        let pairs = self.pairs.as_ref().map(|p| p.len());
        let renamed = mem::replace(&mut self.renamed, false);
        let same = self.all(a, b);
        if same && self.renamed {
            if strictly_same_unordered(a, b) {
                if let (Some(pairs), Some(n)) = (&mut self.pairs, pairs) {
                    pairs.truncate(n)
                }
                self.renamed = renamed;
                return true;
            }
            if self.ambiguity.is_none()
                && self.collapsing.as_ref().is_none_or(|l| l.ambiguity.is_none())
                && let Some((i, j)) = self.another_pairing(a, b, pairs.unwrap_or(0))
            {
                let found = Box::new(Ambiguity {
                    union: a.iter().cloned().collect(),
                    typ: b[j].clone(),
                    members: [a[i].clone(), a[j].clone()],
                });
                match &mut self.collapsing {
                    Some(collapsing) => collapsing.ambiguity = Some(found),
                    None => self.ambiguity = Some(found),
                }
            }
        }
        self.renamed |= renamed;
        same
    }

    /// The first member `i` of `a` and the member `j` of `b` it pairs with,
    /// where the members pair some way other than by position at no more
    /// renamed cells than by position takes. `base` is how many cell pairs
    /// the walk held on entering the unions.
    fn another_pairing(
        &mut self,
        a: &[Type],
        b: &[Type],
        base: usize,
    ) -> Option<(usize, usize)> {
        let n = a.len();
        if n < 2 || n != b.len() {
            return None;
        }
        let mut costs: SmallVec<[Option<usize>; 16]> = SmallVec::new();
        for x in a {
            for y in b {
                let mut found = NodePairs::default();
                let mut scratch = Collapsing::new();
                let mut probe = Identity {
                    strict: self.strict,
                    sets: self.sets.as_deref_mut(),
                    in_order: self.in_order,
                    by_name: self.by_name,
                    across: self.across,
                    collapsing: self.collapsing.as_ref().map(|_| &mut scratch),
                    env: self.env,
                    pairs: self.pairs.as_ref().map(|p| SmallVec::from_slice(&p[..base])),
                    ..Identity::new(None, &mut found)
                };
                costs.push(probe.same(x, y).then_some(probe.paired));
            }
        }
        let cost = |i: usize, j: usize| costs[i * n + j];
        let limit = (0..n).try_fold(0, |s, i| cost(i, i).map(|c| s + c))?;
        fn search(
            cost: &dyn Fn(usize, usize) -> Option<usize>,
            i: usize,
            spent: usize,
            limit: usize,
            taken: &mut SmallVec<[bool; 8]>,
            moved: Option<(usize, usize)>,
        ) -> Option<(usize, usize)> {
            if i == taken.len() {
                return moved;
            }
            (0..taken.len()).find_map(|j| {
                let c = cost(i, j).filter(|c| !taken[j] && spent + c <= limit)?;
                taken[j] = true;
                let moved = moved.or((i != j).then_some((i, j)));
                let found = search(cost, i + 1, spent + c, limit, taken, moved);
                taken[j] = false;
                found
            })
        }
        search(&cost, 0, 0, limit, &mut smallvec![false; n], None)
    }

    /// Whether the unions `a` and `b` hold the same members ([`Leaves`]):
    /// an open cell is one only with itself, any other member is one with
    /// another only by a walk of the two.
    fn same_members(&mut self, a: &[Type], b: &[Type]) -> bool {
        let ends = self.sets.as_deref_mut().expect("a walk of sets");
        let (a, b) = (Leaves::of(a, ends), Leaves::of(b, ends));
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
        // CR claude for claude: [bug/medium] A cell that stands beside itself inside a
        // function type records no pair: this return, and the same-node shortcut at the
        // top of `Identity::same`, answer true before `pairs` is read, so the cell is
        // then free to pair with another one. `union_identical` finds `fn(x: 'a) -> 'a`
        // identical to `fn(x: 'a) -> 'b`, which its own doc says it is not;
        // `RefHist::shape_id` gives the two one id, and a walk keyed by `ref_id` reads
        // one for the other (the check accepts `q(v)` over `(f, g, n)` with `g: fn(x:
        // 'a) -> 'b` typed as `f: fn(x: 'a) -> 'a`). While `pairs` is `Some`, record
        // the pair (x, x) here through the lookup the distinct cells take, and skip the
        // shortcut in `same` for a `Type::TVar`. probe:
        // design/review-2026-11-16/repro/t-setops-identity-shared-cell.gx and
        // `a_cell_met_beside_itself_pairs_with_no_other` in
        // t-setops-identity-and-alternatives.rs (t-setops-02)
        if a.same_cell(b) {
            return true;
        }
        let (x, y) = (a.cell_addr(), b.cell_addr());
        let Some(pairs) = &mut self.pairs else { return false };
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
                // two live cells are one only where they are; making them
                // one would decide what neither has decided
                if self.collapsing.is_some() && !a.level().is_generic() {
                    return false;
                }
                pairs.push((x, y));
                self.renamed = true;
                self.paired += 1;
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
        if self.sets.is_some() {
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
            // CR claude for claude: [bug/low] Both alternatives call `self.same(&x,
            // t)`: with the bound cell on the right the two sides are swapped, so
            // inside a function type the cells beneath are recorded as (right, left),
            // and the consistency check in `cells`, which reads each pair as (left,
            // right), misses a left cell paired twice. `union_identical` finds `fn(x:
            // Array<'a>, y: 'a)` identical to `fn(x: 'c, y: 'e)` with `'c` bound to
            // `Array<'b>`, and not the other way round, nor with `Array<'b>` written in
            // place of `'c`. Split the arm: `(t, Type::TVar(a)) =>
            // a.binding().is_some_and(|x| self.same(t, &x))`. probe:
            // `a_bound_cell_on_the_right_keeps_the_sides` in
            // design/review-2026-11-16/repro/t-setops-identity-and-alternatives.rs
            // (t-setops-04)
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
            (Type::Set(s0), Type::Set(s1)) => self.same_unions(s0, s1),
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
    fn of(ms: &[Type], ends: &mut StandingEnds) -> Self {
        let mut l = Self { cells: LPooled::take(), rest: LPooled::take() };
        l.add(ms, ends);
        l.cells.sort_unstable();
        l.cells.dedup();
        l
    }

    fn add(&mut self, ms: &[Type], ends: &mut StandingEnds) {
        for m in ms {
            #[cfg(test)]
            IDENTITY_STEPS.with(|n| n.set(n.get() + 1));
            if let Type::TVar(tv) = m
                && let Some(c) = ends.open_cell(tv)
            {
                self.cells.push(c);
                continue;
            }
            match members_of(m) {
                Some(ms) => ensure_sufficient(|| self.add(&ms, ends)),
                None => self.rest.push(m.clone()),
            }
        }
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

/// The nodes that stand somewhere in `t`, through its bindings.
fn held_nodes(t: &Type) -> LPooled<AHashSet<NodeKey>> {
    fn walk(t: &Type, found: &mut AHashSet<NodeKey>, seen: &mut AddrSet) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if let Some(b) = tv.binding() {
                    walk(&b, found, seen)
                }
            }
            t => {
                if let Some(a) = node_addr(t)
                    && !seen.insert(a)
                {
                    return;
                }
                if let Some(k) = t.node_key() {
                    found.insert(k);
                }
                t.for_each_child(&mut |c| walk(c, found, seen));
            }
        })
    }
    let mut found = LPooled::take();
    walk(t, &mut found, &mut LPooled::take());
    found
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
        collapsing: &mut Collapsing,
        t0: &TypeRef,
        t1: &TypeRef,
        t: &Self,
    ) -> Result<Self> {
        // CR claude for claude: [bug/low] `kept` holds no alternatives, so the rule
        // `union_tagged` states (members of one alternative collapse only where
        // identical as they stand, `Alternatives::apart`) is not applied to the
        // parameters of two applications: `union_exact_of_members` of `R<fn(q: 'c) ->
        // i64>` and `R<fn(q: 'd) -> i64>` is the one member `R<fn(q: 'c) -> i64>`,
        // where the bare functions stay two. The variant arm of `union_inner` counts
        // its differing positions with a `Welds::new()` too, so `` `P(fn(q: 'c) -> i64,
        // i64) `` with `` `P(fn(q: 'd) -> i64, string) `` is one variant with two
        // positions unioned at once, which its own comment says invents pairs. Give
        // both the alternatives of `welds`: ask `welds.alts.apart` beside each
        // `union_collapses`, as `union_collapses` itself does. probe:
        // `members_of_one_alternative_collapse_only_as_they_stand` in
        // design/review-2026-11-16/repro/t-setops-identity-and-alternatives.rs
        // (t-setops-09)
        let mut kept = Collapsing::new();
        let mut differ = t0
            .params
            .iter()
            .zip(t1.params.iter())
            .enumerate()
            .filter(|(_, (a, b))| !union_collapses(a, b, &mut kept));
        let (i, (p0, p1)) = match (differ.next(), differ.next()) {
            (None, _) => {
                collapsing.absorb(&mut kept);
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
        if merged.decided_holds(env, self)? && merged.decided_holds(env, t)? {
            collapsing.absorb(&mut kept);
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
        collapsing: &mut Collapsing,
        t: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| self.union_inner(env, hist, merge, collapsing, t))
    }

    fn union_inner(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        collapsing: &mut Collapsing,
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
                self.union_ref_params(env, hist, merge, collapsing, t0, t1, t)
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
                    e.union_int(env, hist, merge, collapsing, t)
                } else {
                    t.union_int(env, hist, merge, collapsing, &e)
                };
                hist.remove(&key);
                r
            }
            (t0 @ Type::Abstract { .. }, t1 @ Type::Abstract { .. })
                if union_collapses(t0, t1, collapsing) =>
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
                Ok(Type::Error(Arc::new(e0.union_int(env, hist, merge, collapsing, e1)?)))
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
                        .filter(|(a, b)| !union_collapses(a, b, &mut Collapsing::new()))
                        .count()
                };
                if tg0 == tg1 && t0.len() == t1.len() && differing() <= 1 {
                    let mut typs = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.union_int(env, hist, merge, collapsing, t1))
                        .collect::<Result<LPooled<Vec<_>>>>()?;
                    Ok(Type::Variant(tg0.clone(), Arc::from_iter(typs.drain(..)), *at))
                } else {
                    Ok(pair(u, t))
                }
            }
            (t0 @ Type::TVar(_), t1 @ Type::TVar(_))
                if union_collapses(t0, t1, collapsing) =>
            {
                Ok(t0.clone())
            }
            // A bound cell unions as its binding; an unbound cell
            // stays its own member.
            (Type::TVar(tv), t1) => match tv.binding() {
                Some(b) => b.union_int(env, hist, merge, collapsing, t1),
                None => Ok(pair(self, t1)),
            },
            (t0, Type::TVar(tv)) => match tv.binding() {
                Some(b) => t0.union_int(env, hist, merge, collapsing, &b),
                None => Ok(pair(t0, t)),
            },
            // Everything left of one variant is kept once if identical,
            // else as two members.
            (t0, t1)
                if std::mem::discriminant(t0) == std::mem::discriminant(t1)
                    && union_collapses(t0, t1, collapsing) =>
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

    /// [`Self::union_exact`] of the parts `ts` the alternatives of a
    /// pattern take from the members that reach them, `tags[i]` the
    /// alternative `ts[i]` is of: the parts of one alternative are members
    /// of one union, and two of them that differ by a renaming of their
    /// cells are two.
    pub fn union_exact_of_parts(env: &Env, ts: &[&Type], tags: &[u32]) -> Result<Self> {
        Self::union_tagged(env, Merge::Exact, ts, tags)
    }

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
        let mut collapsing = Collapsing::joining(alternatives(ts, tags));
        let mut acc = first.clone();
        for t in iter {
            acc = acc.union_int(env, &mut hist, merge, &mut collapsing, t)?;
        }
        let acc = acc.normalize_collapsing(&mut collapsing);
        if let Some(a) = collapsing.ambiguity.take() {
            return Err(a.refusal());
        }
        Ok(acc)
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
        // CR claude for claude: [structure/low] The walk of a pair is written twice,
        // here and in the `Type::Ref` arm of `diff_inner`: both save `hist.cut` and
        // `hist.reached`, run the walk, fold them back and keep a `Done` where `cut >=
        // depth`, and both replay a kept `Done` with the same lines (`reached.union`,
        // `holds(over_cut)`, the answer). A change to what a kept difference carries,
        // or to when one may be kept, has to be made in both. Make them two methods of
        // `DiffHist`: `replay(&Done) -> Type`, and one that runs a closure between the
        // save and the fold and returns the walk's cut and what it reached. Found by
        // reading; not run. (t-setops-10)
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
                    Ok(self.diff_one_part(env, hist, t)?.unwrap_or_else(|| self.clone()))
                }
            }
            // CR claude for claude: [bug/low] A map loses only an identical map, where
            // an array, a list and an Error lose themselves to a test whose element
            // type covers theirs. `Map<Any, Any> as m` over `[Map<string, i64>, null]`
            // leaves the map reaching the arms after it, so `Map<Any, Any> as m => ..,
            // null as _ => .., _ => ..` is accepted with a dead third arm; the same
            // select over `[Array<i64>, null]` with `Array<Any>` is refused. Subtract
            // the map where the key's and the value's differences are both empty, as
            // the `(Type::List(t0), Type::List(t1)) | (Type::Array(t0),
            // Type::Array(t1))` arm does for its element. probe:
            // design/review-2026-11-16/repro/t-setops-diff-wider-test-not-subtracted.gx
            // (t-setops-07)
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
            // CR claude for claude: [bug/low] This arm, and the cell-against-cell arm
            // above it, stand above `(_, Type::Any)`, so an open cell less `Any` is the
            // cell. `Array<'a>` less `Array<Any>` is then not covered (the element's
            // difference is `'a`, not empty), and `Array<Any> as a => .., null as _ =>
            // .., _ => ..` over `[Array<'a>, null]` is accepted with a dead third arm,
            // where `Array<i64>` in place of `Array<'a>` refuses it. Every value is a
            // value of `Any`: move `(_, Type::Any)` above the `Type::TVar` arms, or
            // answer empty in them where `is_any(t)`. probe:
            // design/review-2026-11-16/repro/t-setops-diff-wider-test-not-subtracted.gx
            // (t-setops-08)
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

    /// The tuple, struct or variant `self` less `t`, a product of its
    /// shape, where every position but one is held by `t`'s
    /// ([`held_by_shape`]) and a union there loses whole members to `t`'s
    /// part: `self` with that position's difference. `None` where there is
    /// no such position, or its difference is not a union of the members
    /// it had.
    fn diff_one_part(
        &self,
        env: &Env,
        hist: &mut DiffHist,
        t: &Self,
    ) -> Result<Option<Self>> {
        let (p0, p1): (SmallVec<[&Type; 4]>, SmallVec<[&Type; 4]>) = match (self, t) {
            (Type::Tuple(a), Type::Tuple(b)) => (a.iter().collect(), b.iter().collect()),
            (Type::Variant(tag0, a, _), Type::Variant(tag1, b, _)) if tag0 == tag1 => {
                (a.iter().collect(), b.iter().collect())
            }
            (Type::Struct(a), Type::Struct(b))
                if a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(x, y)| x.0 == y.0) =>
            {
                (
                    a.iter().map(|(_, t, _)| t).collect(),
                    b.iter().map(|(_, t, _)| t).collect(),
                )
            }
            _ => return Ok(None),
        };
        if p0.len() != p1.len() {
            return Ok(None);
        }
        // CR claude for claude: [bug/low] `union_parts` reads a position through bound
        // cells but not through a typedef name, so a product whose one differing
        // position is a named union loses nothing. Over `(O, i64)` with `type O = [i64,
        // null]`, the arms `(null, _)` and `(i64, i64) as (a, b)` leave `(O, i64)`
        // reaching a third arm `_`, which is accepted, where the same select over
        // `([i64, null], i64)` refuses it as unreachable (`` `K(O) `` against ``
        // `K([i64, null]) `` alike). `held_by_shape` reads a minuend's name through:
        // read it here too (`lookup_ref_with(env, false)` before `parts_of`), so
        // `whole` is the named union's members. probe:
        // design/review-2026-11-16/repro/t-setops-diff-one-part-named-union.gx
        // (t-setops-06)
        if !p0.iter().any(|a| union_parts(a).is_some()) {
            return Ok(None);
        }
        let mut apart = None;
        for (i, (a, b)) in p0.iter().zip(p1.iter()).enumerate() {
            if !held_by_shape(env, hist, a, b) {
                if apart.is_some() {
                    return Ok(None);
                }
                apart = Some(i);
            }
        }
        let Some(i) = apart else { return Ok(None) };
        let Some(whole) = union_parts(p0[i]) else { return Ok(None) };
        let d = p0[i].diff_int(env, hist, p1[i])?;
        // CR claude for claude: [bug/medium] Where the difference at the one part is a
        // lone open cell (['b, null] less null is the declared 'b), parts_of returns
        // None, since deref_cloned is None for an open cell. diff_one_part then gives
        // up, and what reaches the next arm keeps the whole part, null included. The
        // next arm's bind holds null: over `` `B(['b, null]) `` with 'b: [i64, string],
        // `` `B(null) => 1, `B(v) => select v { i64 as n => n, string as s =>
        // str::len(s) } `` is refused for a missing null, and `` `B(v) => v `` returns
        // [i64, null, 'b]. The concrete `` `B([i64, string, null]) `` and `` `B(['b,
        // i64, null]) `` both check. Read a lone open cell as one member here. Then
        // update lang::select::distribution_variable_beside_null_is_its_bound and
        // distribution_variable_beside_a_variant_is_its_bound, which pin residuals
        // naming the covered null and `C(bool), and pin the bind. probe:
        // design/review-2026-11-16/repro/tests-select-c-null-arm-leaves-null-beside-a-variable.sh
        // (tests-select-c-01)
        let Some((flags, rest)) = parts_of(&d) else { return Ok(None) };
        if !whole.0.contains(flags) || !each_member_of(&rest, &whole.1) {
            return Ok(None);
        }
        if is_empty_ty(&d) {
            return Ok(Some(Type::Primitive(BitFlags::empty())));
        }
        let part = |j: usize, t: &Type| if j == i { d.clone() } else { t.clone() };
        Ok(Some(match self {
            Type::Tuple(a) => {
                Type::Tuple(a.iter().enumerate().map(|(j, t)| part(j, t)).collect())
            }
            Type::Variant(tag, a, at) => Type::Variant(
                tag.clone(),
                a.iter().enumerate().map(|(j, t)| part(j, t)).collect(),
                at.clone(),
            ),
            Type::Struct(a) => Type::Struct(
                a.iter()
                    .enumerate()
                    .map(|(j, (n, t, at))| (n.clone(), part(j, t), at.clone()))
                    .collect(),
            ),
            _ => return Ok(None),
        }))
    }

    fn diff_walk(&self, env: &Env, t: &Self) -> Result<(Self, bool)> {
        let mut hist = DiffHist::new();
        let r = self.diff_int(env, &mut hist, t)?;
        let r = if diff_already_normal(self, &r) { r } else { r.normalize() };
        Ok((r, hist.widened))
    }
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

/// Whether each of `ms` is identical ([`union_identical`]) to one of `of`;
/// an open cell is identical only to its own cell, so those are found by
/// address.
fn each_member_of(ms: &[Type], of: &[Type]) -> bool {
    let mut cells: LPooled<Vec<usize>> = LPooled::take();
    let mut others: LPooled<Vec<&Type>> = LPooled::take();
    for n in of {
        match n {
            Type::TVar(tv) if let Some(c) = tv.open_cell() => cells.push(c.cell_addr()),
            n => others.push(n),
        }
    }
    cells.sort_unstable();
    ms.iter().all(|m| match m {
        Type::TVar(tv) if let Some(c) = tv.open_cell() => {
            cells.binary_search(&c.cell_addr()).is_ok()
        }
        m => others.iter().any(|n| union_identical(m, n)),
    })
}

/// The primitive flags and the other members of `t`.
fn parts_of(t: &Type) -> Option<(BitFlags<Typ>, SmallVec<[Type; 4]>)> {
    let mut flags = BitFlags::empty();
    let mut rest: SmallVec<[Type; 4]> = SmallVec::new();
    let mut add = |m: &Type| match m {
        Type::Primitive(p) => flags |= *p,
        m => rest.push(m.clone()),
    };
    match t.deref_cloned()? {
        Type::Set(ref ms) => ms.iter().for_each(&mut add),
        m => add(&m),
    }
    Some((flags, rest))
}

/// [`parts_of`] where `t` is a union of two or more.
fn union_parts(t: &Type) -> Option<(BitFlags<Typ>, SmallVec<[Type; 4]>)> {
    parts_of(t).filter(|(flags, rest)| flags.len() + rest.len() > 1)
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
        // CR claude for claude: [perf/medium] `Held::of` keeps no answer for a pair of
        // shared nodes, so where `t1` holds `t0` by shape without being identical to it
        // (the `union_identical` test above fails) a tuple, struct or variant whose
        // positions share their parts is walked once per path: `Type::diff` of two
        // tuple types nested d deep costs 2^d. A two-arm select over `select c { true
        // => (a26, 0), false => (b26, null) }`, each of `a26` and `b26` built by 26
        // lets of `(x, x)`, takes 22 s to check where the lets alone take 0.1 s, and
        // the time doubles per level. Every other walk of this file takes a DAG once
        // per pair of nodes (`NodePairs`, `DiffHist::parts`): keep here the pairs of
        // shared nodes found held whose walk read no name of `t1` (the answer of the
        // others depends on `read`). probe:
        // design/review-2026-11-16/repro/t-setops-held-by-shape-dag.gx,
        // t-setops-held-by-shape-dag.rs (t-setops-03)
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
    pub(super) static IDENTITY_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NO_MEMO: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static HELD_READS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
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
        expr::{ModPath, parser::parse_type},
        format_with_flags,
        typ::{
            TVar, Type,
            cast::tests::{parsed, typed, union_dag},
            matches::MATCH_PAIRS,
            node_addr,
            setops::{Alternatives, ERRORS_OVER_CUT, WALK_PAIRS, union_identical},
            tvar::{CHAIN_STEPS, Level, StandingEnds},
        },
    };
    use ahash::AHashMap;
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

    /// The members of one alternative are apart however alike; those of two
    /// are not. The alternatives' nodes are read once while no cell is
    /// written, and again after a write.
    #[test]
    fn alternatives_read_their_nodes_once_while_no_cell_is_written() {
        let pair =
            |a: &Type, b: &Type| Type::Tuple(Arc::from_iter([a.clone(), b.clone()]));
        let (x, y, z) = (
            Type::Array(Arc::new(Type::Primitive(Typ::I64.into()))),
            Type::Array(Arc::new(Type::Primitive(Typ::I64.into()))),
            Type::Array(Arc::new(Type::Primitive(Typ::String.into()))),
        );
        let cell = TVar::empty_named(literal!("c"));
        let held = Type::Array(Arc::new(Type::TVar(cell.clone())));
        let beside = Type::Array(Arc::new(Type::Primitive(Typ::Bool.into())));
        cell.bind(pair(&x, &held));
        let mut alts = Alternatives::new(
            [pair(&x, &y), pair(&z, &Type::TVar(cell.clone()))].into_iter().collect(),
        );
        super::HELD_READS.with(|n| n.set(0));
        for _ in 0..50 {
            assert!(alts.apart(&x, &y));
            assert!(alts.apart(&x, &held));
            assert!(alts.apart(&z, &x));
            assert!(!alts.apart(&y, &z));
            assert!(!alts.apart(&y, &beside));
        }
        assert_eq!(super::HELD_READS.with(|n| n.get()), 1);
        let other = TVar::empty_named(literal!("d"));
        other.bind(Type::Primitive(Typ::I64.into()));
        assert!(alts.apart(&x, &y));
        assert_eq!(super::HELD_READS.with(|n| n.get()), 2);
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

    // A product the subtrahend holds at every position but one, where a
    // union of the minuend loses whole members, keeps the rest of the union
    // there, and is whole where more than one position differs or the
    // difference is not a part of the union.
    #[test]
    fn diff_keeps_the_rest_of_a_union_at_the_one_differing_position() {
        let (env, _) = typed(&[("L", "[`Cons(i64, L), `Nil]")], "null");
        for (t0, t1, left) in [
            ("`A([i64, string])", "`A(i64)", "`A(string)"),
            ("`A([i64, string])", "`A([i64, string])", "[]"),
            ("`A([i64, string, null])", "`A(i64)", "`A([null, string])"),
            ("`A([Array<i64>, null])", "`A(Array<i64>)", "`A(null)"),
            ("`A([Array<i64>, null])", "`A(Array<string>)", "`A([null, Array<i64>])"),
            ("([i64, null], i64)", "(null, Any)", "(i64, i64)"),
            ("([i64, null], i64)", "(null, i64)", "(i64, i64)"),
            ("([i64, null], [i64, null])", "(i64, i64)", "([i64, null], [i64, null])"),
            ("{a: [i64, null], b: i64}", "{a: i64, b: Any}", "{ a: null, b: i64 }"),
            ("`A([`B, `C])", "`A(`B)", "`A(`C)"),
            ("`A(L)", "`A(`Nil)", "`A(L)"),
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
            ("[(i64, [i64, f64]), null]", "(i64, [i64, string])", "[null, (i64, f64)]"),
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

    /// Walks asking whether a union is one of several, each holding the
    /// cell at the head of one long chain of forwarding links, read the
    /// chain's end once while no cell is written.
    #[test]
    fn walks_as_one_of_reads_a_long_chain_once() {
        let n = 2000;
        let cells: Vec<TVar> =
            (0..n).map(|i| TVar::empty_named(arcstr::format!("a{i}"))).collect();
        for c in &cells {
            c.set_level(Level::TOP)
        }
        for w in cells.windows(2) {
            w[0].bind(Type::TVar(w[1].clone()))
        }
        let union = |t: Typ| {
            Type::Set(Arc::from_iter([
                Type::TVar(cells[0].clone()),
                Type::Primitive(t.into()),
            ]))
        };
        let members = [Typ::I64, Typ::F64, Typ::String, Typ::Bool].map(union);
        CHAIN_STEPS.with(|s| s.set(0));
        let mut ends = StandingEnds::default();
        assert!(!union(Typ::U8).walks_as_one_of(&members, &mut ends));
        assert!(!union(Typ::U16).walks_as_one_of(&members, &mut ends));
        let steps = CHAIN_STEPS.with(|s| s.get());
        assert!(steps <= n + 8, "{steps} links walked for {} members", members.len());
        assert!(union(Typ::Bool).walks_as_one_of(&members, &mut ends));
    }
}
