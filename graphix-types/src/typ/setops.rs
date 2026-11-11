use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        Bound, ContainsFlags, FnType, Kept, Lazy, NodeKey, NodePairs, RefHist, RefPair,
        TVar, Type, TypeRef,
        contains::on_copies,
        fntyp::LambdaIds,
        node_addr,
        tvar::{Fresh, cell_writes},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{iter, mem, ops::ControlFlow};
use triomphe::Arc;

struct MatchingTypes {
    memo: RefHist<AHashMap<RefPair, Kept<Type>>>,
    refs: LPooled<AHashSet<(Option<usize>, Option<usize>)>>,
    /// Type a part holding a rigid cell by the written test's part
    /// ([`Type::matching_parts`]).
    inside: bool,
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
}

/// A finished pair's difference and what its walk reached.
struct Done {
    answer: Type,
    reached: Reached,
}

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

/// [`union_identical`] with every unbound cell by identity, inside a
/// function type too: one signature's variables are its own.
pub(crate) fn strictly_identical(t0: &Type, t1: &Type) -> bool {
    Identity { strict: true, ..Identity::new(None, &mut NodePairs::default()) }
        .same(t0, t1)
}

/// The pairs of live cells union collapses have met: each pair is one
/// cell once [`apply_welds`] runs.
pub(super) type Welds = SmallVec<[(TVar, TVar); 2]>;

/// [`union_identical`], for a collapse that keeps `t0` in place of `t1`:
/// the live cells it pairs go on `welds`, so the dropped member's cells
/// learn what the kept one learns.
pub(super) fn union_collapses(t0: &Type, t1: &Type, welds: &mut Welds) -> bool {
    let start = welds.len();
    let same = Identity::new(Some(welds), &mut NodePairs::default()).same(t0, t1);
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

/// [`union_identical`] over the questions of one walk: the node pairs
/// found identical, with no two distinct cells paired, stand while this
/// thread writes no cell, so a chain asked about at every link is walked
/// once.
#[derive(Default)]
pub(super) struct IdenticalMemo {
    writes: u64,
    found: NodePairs,
}

impl IdenticalMemo {
    pub(super) fn identical(&mut self, t0: &Type, t1: &Type) -> Identical {
        let writes = cell_writes();
        if self.writes != writes || !memo_on() {
            self.found.clear();
            self.writes = writes;
        }
        self.found.restart();
        let mut id = Identity::new(None, &mut self.found);
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

/// Identity of `t0` and `t1`.
struct Identity<'a> {
    strict: bool,
    /// Two open cells of one name are identical, whatever holds them
    /// (`pairs` holds those met, never `None`).
    by_name: bool,
    /// Where the live cells paired inside a function type go.
    live: Option<&'a mut Welds>,
    /// Inside a function type: the cells paired so far, by address.
    pairs: Option<SmallVec<[(usize, usize); 4]>>,
    /// The nodes found identical outside a function type.
    found: &'a mut NodePairs,
    /// Those found inside the function type being compared, which hold
    /// only under its pairing of cells.
    found_in_fn: NodePairs,
    /// Two distinct cells were paired.
    renamed: bool,
}

impl<'a> Identity<'a> {
    fn new(live: Option<&'a mut Welds>, found: &'a mut NodePairs) -> Self {
        Self {
            strict: false,
            by_name: false,
            live,
            pairs: None,
            found,
            found_in_fn: NodePairs::default(),
            renamed: false,
        }
    }

    fn same(&mut self, t0: &Type, t1: &Type) -> bool {
        #[cfg(test)]
        IDENTITY_STEPS.with(|n| n.set(n.get() + 1));
        // one shared node is identical to itself, however large
        if t0.node_key().is_some_and(|a| t1.node_key() == Some(a)) {
            return true;
        }
        let pair = self.found.key(t0, t1);
        if let Some(pair) = pair
            && (self.found.contains(pair)
                || (self.pairs.is_some() && self.found_in_fn.contains(pair)))
        {
            return true;
        }
        let same = ensure_sufficient(|| self.same_inner(t0, t1));
        if let Some(pair) = pair.filter(|_| same) {
            match self.pairs {
                None => self.found.insert(pair, t0, t1),
                Some(_) => self.found_in_fn.insert(pair, t0, t1),
            }
        }
        same
    }

    fn all(&mut self, a: &[Type], b: &[Type]) -> bool {
        a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| self.same(a, b))
    }

    fn cells(&mut self, a: &TVar, b: &TVar) -> bool {
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
        if a.level().is_generic() != b.level().is_generic() {
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
                self.all(&ca, &cb)
            }
        }
    }

    #[inline(never)]
    fn same_inner(&mut self, t0: &Type, t1: &Type) -> bool {
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
                r0.same_filled_definition(r1) && self.all(&r0.params, &r1.params)
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

/// Whether `a` and `b` are one constructor whose parts ([`part`]) pair by
/// position.
fn same_shape(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Array(_), Type::Array(_))
        | (Type::List(_), Type::List(_))
        | (Type::Map(_), Type::Map(_)) => true,
        (Type::ByRef(m0, _), Type::ByRef(m1, _)) => m0 == m1,
        (Type::Tuple(a), Type::Tuple(b)) => a.len() == b.len(),
        (Type::Variant(t0, a, _), Type::Variant(t1, b, _)) => {
            t0 == t1 && a.len() == b.len()
        }
        (Type::Struct(a), Type::Struct(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.0 == y.0)
        }
        _ => false,
    }
}

/// The members of the union `t` stands for through its bindings.
fn members_of(t: &Type) -> Option<Arc<[Type]>> {
    t.with_deref(|t| match t {
        Some(Type::Set(ms)) => Some(ms.clone()),
        _ => None,
    })
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

    /// The union of a select's alternatives, which a call through the
    /// select must accept alike: function members meet first, and so do
    /// those in the members' parts ([`Self::meet_alternatives`]).
    pub fn union_of_alternatives(env: &Env, ts: &[&Type]) -> Result<Self> {
        match Self::meet_alternatives(env, ts)? {
            None => Self::union(env, ts),
            Some(met) => {
                let met: LPooled<Vec<&Type>> = met.iter().collect();
                Self::union(env, &met)
            }
        }
    }

    fn union_with(env: &Env, merge: Merge, ts: &[&Type]) -> Result<Self> {
        let mut iter = ts.iter().copied();
        let Some(first) = iter.next() else {
            return Ok(Type::Primitive(BitFlags::empty()));
        };
        let mut hist = RefHist::new();
        let mut welds = Welds::new();
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
    /// any member's. `None` when no member changes.
    fn meet_alternatives(env: &Env, ts: &[&Type]) -> Result<Option<LPooled<Vec<Type>>>> {
        ensure_sufficient(|| {
            if !ts.iter().any(|t| members_of(t).is_some()) {
                return Self::meet_members(env, ts);
            }
            let mut flat: LPooled<Vec<Type>> = LPooled::take();
            let mut shape: LPooled<Vec<Option<usize>>> = LPooled::take();
            ts.iter().for_each(|t| union_members(t, &mut flat, &mut shape));
            let flat: LPooled<Vec<&Type>> = flat.iter().collect();
            let Some(mut met) = Self::meet_members(env, &flat)? else { return Ok(None) };
            let (mut shape, mut met) = (shape.drain(..), met.drain(..));
            Ok(Some(ts.iter().map(|_| rebuilt(&mut shape, &mut met)).collect()))
        })
    }

    /// [`Self::meet_open_fns`] over `ts`, then over the parts of each set
    /// of members of one shape ([`same_shape`]). `None` when no member
    /// changes.
    fn meet_members(env: &Env, ts: &[&Type]) -> Result<Option<LPooled<Vec<Type>>>> {
        let fns = Self::meet_open_fns(env, ts)?;
        let parts = match &fns {
            Some(met) => {
                let met: LPooled<Vec<&Type>> = met.iter().collect();
                Self::meet_parts(env, &met)?
            }
            None => Self::meet_parts(env, ts)?,
        };
        Ok(parts.or(fns))
    }

    /// The members of `ts` of one shape meet part by part
    /// ([`Self::meet_alternatives`]); a part that is one node in every
    /// member meets nothing. `None` when no member changes.
    fn meet_parts(env: &Env, ts: &[&Type]) -> Result<Option<LPooled<Vec<Type>>>> {
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
                .filter(|j| members[*j].as_ref().is_some_and(|m| same_shape(head, m)))
                .collect();
            for j in group.iter() {
                grouped[*j] = true;
            }
            if group.len() < 2 {
                continue;
            }
            let mut rebuilt: SmallVec<[Type; 4]> =
                group.iter().filter_map(|j| members[*j].clone()).collect();
            let mut changed = false;
            for p in 0..part_count(head) {
                let met = {
                    let at: SmallVec<[&Type; 4]> =
                        rebuilt.iter().map(|m| part(m, p)).collect();
                    let one = at[0].node_key();
                    if one.is_some() && at.iter().all(|t| t.node_key() == one) {
                        continue;
                    }
                    Self::meet_alternatives(env, &at)?
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
    /// call needs one signature. A generic member meets through a fresh
    /// instance; an occurrence's own cells bind, as the call would bind
    /// them, once the meet holds on copies (a probe judges each part apart,
    /// and a refused commit would leave its bindings behind). `None` when
    /// no member changes.
    fn meet_open_fns(env: &Env, ts: &[&Type]) -> Result<Option<LPooled<Vec<Type>>>> {
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
        // `group[k]` names the member each of `fns` met into, and `base`
        // holds that group's last met type
        let mut group: SmallVec<[usize; 4]> = (0..fns.len()).collect();
        let mut base: SmallVec<[Option<Arc<FnType>>; 4]> = smallvec![None; fns.len()];
        let mut met: SmallVec<[bool; 4]> = smallvec![false; fns.len()];
        let probe = BitFlags::empty();
        for (k, (_, f)) in fns.iter().enumerate() {
            if Type::Fn(f.clone()).tvar_free() {
                continue;
            }
            let inst = Type::Fn(Arc::new(
                f.reset_tvars_int(&mut LPooled::take(), Fresh::Generic),
            ));
            for (j, (_, g)) in fns.iter().enumerate() {
                if j == k || met[j] {
                    continue;
                }
                let g = Type::Fn(g.clone());
                if inst.contains_with_flags(probe, env, &g)?
                    && g.contains_with_flags(probe, env, &inst)?
                    && on_copies([&inst, &g], |[i, g], trial| {
                        let commit = ContainsFlags::Commit.into();
                        Ok(i.contains_int(commit, env, trial, &g)?
                            && g.contains_int(commit, env, trial, &i)?)
                    })?
                    && inst.contains(env, &g)?
                    && g.contains(env, &inst)?
                {
                    let Type::Fn(g) = &g else { unreachable!("a function member") };
                    let (from, to) = (group[k], group[j]);
                    for m in group.iter_mut().filter(|m| **m == from) {
                        *m = to;
                    }
                    base[from] = None;
                    base[to] = Some(g.clone());
                    met[k] = true;
                    break;
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
            let (answer, reached) = (k.answer.clone(), k.reached.clone());
            hist.reached.union(&reached);
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
            let done = (Done { answer: d.clone(), reached }, writes);
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
                    let (answer, reached) =
                        (k.answer.answer.clone(), k.answer.reached.clone());
                    hist.reached.union(&reached);
                    return Ok(answer);
                }
                if let Some((r, depth)) = hist.hist.get(&key) {
                    let r = r.clone();
                    hist.cut = hist.cut.min(*depth);
                    hist.reach(Reach::Pair(key));
                    return Ok(r);
                }
                // a pair met again inside its own difference subtracts
                // nothing: assuming it covered would drop what it did not
                if let (Some(k), Some(defs)) = (hist.unfoldings.again(defs), defs) {
                    let depth = hist.hist.get(&k).map_or(0, |(_, d)| *d);
                    hist.cut = hist.cut.min(depth);
                    hist.reach(Reach::Defs(defs));
                    return Ok(self.clone());
                }
                if !hist.pairs.unfold() {
                    hist.cut = 0;
                    hist.widened = true;
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
                let r = t0.diff_int(env, hist, &t1);
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
                    let done = Done { answer: d.clone(), reached };
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
                for i in 0..s0.len() {
                    s.push(s0[i].clone());
                    for j in 0..s1.len() {
                        s[i] = s[i].diff_int(env, hist, &s1[j])?
                    }
                }
                Ok(Self::flatten_set(s.drain(..)))
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
                    Ok(Self::flatten_set(diffs.drain(..)))
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
            (Type::Tuple(t0), Type::Tuple(t1)) => {
                if same_resolved(t0.iter(), t1.iter()) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Struct(t0), Type::Struct(t1)) => {
                let same = t0.len() == t1.len()
                    && t0.iter().zip(t1.iter()).all(|((n0, _, _), (n1, _, _))| n0 == n1)
                    && same_resolved(
                        t0.iter().map(|(_, t, _)| t),
                        t1.iter().map(|(_, t, _)| t),
                    );
                if same {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Variant(tg0, t0, _), Type::Variant(tg1, t1, _)) => {
                if tg0 == tg1 && same_resolved(t0.iter(), t1.iter()) {
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
                let covered = t0 == t1
                    || matches!(
                        t0.diff_int(env, hist, t1)?,
                        Type::Primitive(p) if p.is_empty()
                    );
                match covered {
                    true => Ok(Type::Primitive(BitFlags::empty())),
                    false => Ok(self.clone()),
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
                if e0 == e1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    match e0.diff_int(env, hist, e1)? {
                        Type::Primitive(p) if p.is_empty() => {
                            Ok(Type::Primitive(BitFlags::empty()))
                        }
                        d => Ok(Type::Error(Arc::new(d))),
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
        let refs = LPooled::take();
        let mut walk = MatchingTypes { memo: RefHist::new(), refs, inside: false };
        self.matching_type_int(env, &mut walk, reaching)
    }

    /// [`Self::matching_type`], where a part of `reaching` holding a rigid
    /// cell, whose values no pattern takes apart, is this test's part: the
    /// types of the binds inside it.
    pub fn matching_parts(&self, env: &Env, reaching: &Self) -> Result<Self> {
        let refs = LPooled::take();
        let mut walk = MatchingTypes { memo: RefHist::new(), refs, inside: true };
        self.matching_type_int(env, &mut walk, reaching)
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
                if !walk.refs.insert(key) {
                    return Ok(p.clone());
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
                let same_shape = match (p, t) {
                    (Self::Array(_), Self::Array(_))
                    | (Self::List(_), Self::List(_))
                    | (Self::Error(_), Self::Error(_))
                    | (Self::Map { .. }, Self::Map { .. }) => true,
                    (Self::Tuple(a), Self::Tuple(b)) => a.len() == b.len(),
                    (Self::Variant(a, ats, _), Self::Variant(b, bts, _)) => {
                        a == b && ats.len() == bts.len()
                    }
                    (Self::Struct(a), Self::Struct(b)) => {
                        a.len() == b.len()
                            && a.iter().zip(b.iter()).all(|((a, _, _), (b, _, _))| a == b)
                    }
                    _ => false,
                };
                let rigid_inside = walk.inside && same_shape && t.has_rigid();
                // CR claude for claude: [perf/low] Each of the two containment probes
                // below starts a walk of its own (contains_with_flags), so nothing a
                // probe learned (its probe memo, its expansions, its normalize pass)
                // serves the next pair, and a matching_type over a DAG of named error
                // unions asks the same distributions once per pair: cm4.gx makes about
                // 1400 such walks asking 44000 distributions. One ContainsHist kept in
                // MatchingTypes and passed to contains_int, as MatchHist keeps one for
                // could_match, takes cm4 --no-cache --check from 5.83G to 3.22G
                // instructions on lane 20g after contains-normalize-repeat-01 (an empty
                // program 0.76G); the GUI examples' check is unchanged (40.7G). Not
                // done there: a walk kept across top-level questions needs its own
                // review (what an Err leaves in it, the probe memo across the pairs).
                // probe: ~/ptmp/cr29/scratch/r19/19b/mtm/cm4.gx
                // (matching-type-probe-walks-01)
                if !rigid_inside
                    && !p.has_unbound()
                    && p.contains_with_flags(BitFlags::empty(), env, t)?
                {
                    return Ok(t.clone());
                }
                if !t.has_unbound() && t.contains_with_flags(BitFlags::empty(), env, p)? {
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

/// `Any`, read through bound type variables.
fn is_any(t: &Type) -> bool {
    t.with_deref(|t| matches!(t, Some(Type::Any)))
}

/// Equality read through bound type variables.
fn same_type(t0: &Type, t1: &Type) -> bool {
    let bound = |t: &Type| cell_beneath(t, |tv, _| tv.binding().is_some());
    t0 == t1 || (bound(t0) || bound(t1)) && same_normal(t0, t1)
}

/// Equality of both sides read through bound type variables, normalized.
fn same_normal(t0: &Type, t1: &Type) -> bool {
    t0.resolve_tvars().normalize() == t1.resolve_tvars().normalize()
}

/// Positional [`same_normal`]. `Any` in the subtrahend covers anything.
fn same_resolved<'a, 'b>(
    t0: impl IntoIterator<Item = &'a Type>,
    t1: impl IntoIterator<Item = &'b Type>,
) -> bool {
    let mut t0 = t0.into_iter();
    let mut t1 = t1.into_iter();
    loop {
        match (t0.next(), t1.next()) {
            (None, None) => return true,
            (Some(a), Some(b)) => {
                if !is_any(b) && a != b && !same_normal(a, b) {
                    return false;
                }
            }
            _ => return false,
        }
    }
}

#[cfg(test)]
thread_local! {
    pub(super) static DIFF_PAIRS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static DIFF_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static IDENTITY_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NO_MEMO: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Whether diff, could_match and [`IdenticalMemo`] read their memos of
/// pairs: a test compares their answers with the walks' without them.
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
            Bound, NodePairs, TVar, Type,
            cast::tests::{parsed, typed, union_dag},
            matches::MATCH_PAIRS,
            setops::{Identical, IdenticalMemo, WALK_PAIRS, union_identical},
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
    // constructor's argument.
    #[test]
    fn diff_reads_bound_cells_through() {
        let (env, _) =
            typed(&[("S", "Map<i64, i64>"), ("Box<'a>", "Abstract<'a>")], "null");
        let bound = |t: &str| bound(&env, t);
        let expanded = |t: Type| t.lookup_ref(&env).unwrap();
        for (t0, t1) in [
            (bound("Map<'c, i64>"), parsed(&env, "Map<i64, i64>")),
            (parsed(&env, "Map<i64, i64>"), bound("Map<'c, i64>")),
            (
                bound("[Error<Map<'c, i64>>, Array<Map<i64, i64>>]"),
                parsed(&env, "[Error<S>, Array<S>]"),
            ),
            (bound("fn(x: 'c) -> i64"), parsed(&env, "fn(x: i64) -> i64")),
            (expanded(bound("Box<'c>")), expanded(parsed(&env, "Box<i64>"))),
            (bound("'k<'c>"), parsed(&env, "'k<i64>")),
        ] {
            let d = t0.diff(&env, &t1).unwrap();
            assert!(d.is_uninhabited(), "{t0:?} - {t1:?} = {d:?}");
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
        assert_eq!((apart, shared), (vec![12, 24, 43, 178], vec![12, 24, 43, 178]));
    }

    // Names recurring through Error payloads end within the walk's pair
    // budget.
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
        assert_eq!(diff_pairs(&defs, "At", "K").1, WALK_PAIRS);
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
                (18, rest.clone(), 155, true),
                (26, rest.clone(), 202, true),
                (34, rest, 253, true)
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
}
