use crate::{
    AddrMap, AddrSet, LambdaId, PRINT_FLAGS, PrintFlag, SourcePosition,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    format_with_flags, image,
    typ::{
        Bound, Mutability, TVar, TVarsSeen, Type,
        contains::{ContainsFlags, ContainsHist, any_part_pair},
        key_text,
        matches::{MatchHist, SigMatch, SigPairs},
        normalize::NormCx,
        settle::{DataPositions, Held},
        tvar::{self, Fresh, Level, RigidGate},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_derive::Pack;
use nohash::IntSet;
use parking_lot::RwLock;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::{Cell, RefCell},
    cmp::Ordering,
    collections::{VecDeque, hash_map::Entry},
    fmt::{self, Debug, Write},
    hash::{Hash, Hasher},
    ops::{ControlFlow, Range},
    sync::{Arc as SArc, Weak},
    thread::LocalKey,
};
use triomphe::Arc;

/// Positional or labeled function argument. A positional name is
/// documentation only (not part of type identity); a label is the
/// call-site key, and `has_default` is part of the type's shape.
#[derive(Debug, Clone, Pack)]
#[pack(unwrapped)]
pub enum FnArgKind {
    Positional { name: Option<ArcStr> },
    Labeled { name: ArcStr, has_default: bool },
}

impl FnArgKind {
    pub fn name(&self) -> Option<&ArcStr> {
        match self {
            FnArgKind::Positional { name } => name.as_ref(),
            FnArgKind::Labeled { name, .. } => Some(name),
        }
    }

    pub fn label(&self) -> Option<&ArcStr> {
        match self {
            FnArgKind::Labeled { name, .. } => Some(name),
            FnArgKind::Positional { .. } => None,
        }
    }

    pub fn is_labeled(&self) -> bool {
        matches!(self, FnArgKind::Labeled { .. })
    }

    pub fn is_positional(&self) -> bool {
        matches!(self, FnArgKind::Positional { .. })
    }

    pub fn has_default(&self) -> bool {
        matches!(self, FnArgKind::Labeled { has_default: true, .. })
    }
}

impl PartialEq for FnArgKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FnArgKind::Positional { .. }, FnArgKind::Positional { .. }) => true,
            (
                FnArgKind::Labeled { name: n0, has_default: d0 },
                FnArgKind::Labeled { name: n1, has_default: d1 },
            ) => n0 == n1 && d0 == d1,
            _ => false,
        }
    }
}

impl Eq for FnArgKind {}

impl PartialOrd for FnArgKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnArgKind {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (FnArgKind::Positional { .. }, FnArgKind::Positional { .. }) => {
                Ordering::Equal
            }
            (FnArgKind::Positional { .. }, FnArgKind::Labeled { .. }) => Ordering::Less,
            (FnArgKind::Labeled { .. }, FnArgKind::Positional { .. }) => {
                Ordering::Greater
            }
            (
                FnArgKind::Labeled { name: n0, has_default: d0 },
                FnArgKind::Labeled { name: n1, has_default: d1 },
            ) => n0.cmp(n1).then_with(|| d0.cmp(d1)),
        }
    }
}

impl Hash for FnArgKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            FnArgKind::Positional { .. } => 0u8.hash(state),
            FnArgKind::Labeled { name, has_default } => {
                1u8.hash(state);
                name.hash(state);
                has_default.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, Pack, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pack(unwrapped)]
pub struct FnArgType {
    pub kind: FnArgKind,
    pub typ: Type,
}

impl FnArgType {
    pub fn name(&self) -> Option<&ArcStr> {
        self.kind.name()
    }

    pub fn label(&self) -> Option<&ArcStr> {
        self.kind.label()
    }

    pub fn is_labeled(&self) -> bool {
        self.kind.is_labeled()
    }

    pub fn is_positional(&self) -> bool {
        self.kind.is_positional()
    }

    pub fn has_default(&self) -> bool {
        self.kind.has_default()
    }
}

#[derive(Debug, Clone)]
struct Link(Weak<RwLock<LambdaIdsInner>>);

impl Hash for Link {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(crate::ids::mix(self.addr() as u64))
    }
}

impl nohash::IsEnabled for Link {}

impl PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Link {}

impl Link {
    fn upgrade(&self) -> Option<LambdaIds> {
        Weak::upgrade(&self.0).map(LambdaIds)
    }

    fn addr(&self) -> usize {
        Weak::as_ptr(&self.0) as usize
    }
}

#[derive(Debug, Default)]
struct LambdaIdsInner {
    own: Option<LambdaId>,
    links: IntSet<Link>,
}

#[derive(Debug, Clone)]
pub struct LambdaIds(SArc<RwLock<LambdaIdsInner>>);

impl Default for LambdaIds {
    fn default() -> Self {
        Self(SArc::new(RwLock::new(LambdaIdsInner::default())))
    }
}

impl LambdaIds {
    pub(crate) fn addr(&self) -> usize {
        SArc::as_ptr(&self.0) as *const () as usize
    }

    pub fn set_id(&self, id: LambdaId) {
        self.0.write().own = Some(id)
    }

    /// The node of a fresh instantiation: carries this node's `own` id
    /// and a one-way snapshot of its links, so what later unifies with
    /// the instance lands on the copy and never reaches the def's node.
    pub(crate) fn instantiate(&self) -> LambdaIds {
        let inner = self.0.read();
        Self(SArc::new(RwLock::new(LambdaIdsInner {
            own: inner.own,
            links: inner.links.clone(),
        })))
    }

    /// Every live linked id; dead links are pruned as the walk meets
    /// them. Locks one node at a time. O(live linked nodes).
    pub fn ids(&self) -> LPooled<IntSet<LambdaId>> {
        let mut visited: LPooled<AddrSet> = LPooled::take();
        let mut ids: LPooled<IntSet<LambdaId>> = LPooled::take();
        let mut work: LPooled<Vec<LambdaIds>> = LPooled::take();
        visited.insert(SArc::as_ptr(&self.0) as usize);
        work.push(self.clone());
        while let Some(node) = work.pop() {
            let mut inner = node.0.write();
            if let Some(id) = inner.own {
                ids.insert(id);
            }
            inner.links.retain(|link| match link.upgrade() {
                Some(next) => {
                    if visited.insert(link.addr()) {
                        work.push(next);
                    }
                    true
                }
                None => false,
            });
        }
        ids
    }

    #[doc(hidden)]
    pub fn own(&self) -> Option<LambdaId> {
        self.0.read().own
    }

    pub fn link(&self, other: &LambdaIds) {
        self.0.write().links.insert(other.as_link());
        other.0.write().links.insert(self.as_link());
    }

    /// A node that reaches every id `nodes` reach, which none of them
    /// reaches back.
    pub fn joining(nodes: &[&LambdaIds]) -> LambdaIds {
        let joined = LambdaIds::default();
        {
            let mut inner = joined.0.write();
            for n in nodes {
                inner.links.insert(n.as_link());
            }
        }
        joined
    }

    fn as_link(&self) -> Link {
        Link(SArc::downgrade(&self.0))
    }
}

/// A function signature. Constraints live only in the tvar cells: a
/// quantifier like `fn<'a: Number>` seeds `'a`'s cell conjunction, and
/// [`FnType::constraint_view`] derives the listing from reachable cells.
#[derive(Debug, Clone)]
pub struct FnType {
    pub args: Arc<[FnArgType]>,
    pub vargs: Option<Type>,
    pub rtype: Type,
    pub throws: Type,
    pub explicit_throws: bool,
    /// The quantifier names the `fn<...>` header declared, in source
    /// order (the constraint types live in the cells). They decide which
    /// cells a call freshens or holds rigid, and which conjuncts
    /// `constraint_view`, and so Eq and Ord, see; the image keys them.
    pub quantifiers: Arc<[ArcStr]>,
    /// Every LambdaId this type might represent.
    pub lambda_ids: LambdaIds,
}

/// Signature vars in their deterministic order, by (name, TVarId):
/// recording and settle order land in constraint lists and diagnostics.
pub(super) fn sorted_tvars(
    tvs: impl IntoIterator<Item = (ArcStr, TVar)>,
) -> LPooled<Vec<(ArcStr, TVar)>> {
    let mut v: LPooled<Vec<(ArcStr, TVar)>> = tvs.into_iter().collect();
    v.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.read().id.cmp(&b.1.read().id)));
    v
}

thread_local! {
    static VIEWING: RefCell<AddrSet> = RefCell::new(AddrSet::default());
    static CONSTRAINING: RefCell<AddrSet> = RefCell::new(AddrSet::default());
    /// The cells whose conjuncts a walk is visiting: a normalized copy of a
    /// signature is a new FnType but holds the same cells.
    static CONJUNCTS: RefCell<AddrSet> = RefCell::new(AddrSet::default());
    /// How deep `for_each_part` calls nest on this thread.
    static PARTS_DEPTH: Cell<usize> = const { Cell::new(0) };
    /// For the outermost `for_each_part` call, or a signature's whole
    /// parse: each nested signature's position cells, keyed by the
    /// signature it pins, so a walk collects each signature's once rather
    /// than once per signature above it. A cell's handle follows its
    /// aliases, so an entry stands while the parse aliases.
    static POSITIONS: RefCell<AddrMap<(Arc<FnType>, Vec<(ArcStr, TVar)>)>> =
        RefCell::new(AddrMap::default());
    /// For the outermost `for_each_part` call: what each nested quantified
    /// signature names from outside itself, as `collect_tvars` finds it
    /// under no guard, while this thread writes no cell.
    static FREE: RefCell<FreeCells> = RefCell::new(FreeCells::default());
    /// How many times a walk's guard has refused a signature or a cell.
    static CUTS: Cell<u64> = const { Cell::new(0) };
    /// How deep signature parses nest on this thread.
    static PARSE_DEPTH: Cell<usize> = const { Cell::new(0) };
    /// For the outermost signature's parse: each signature declared inside
    /// it, with the cells it names from outside itself as its declaration
    /// left them, which the parse's walks read rather than walk it again.
    static DECLARED: RefCell<AddrMap<(Arc<FnType>, Vec<(ArcStr, TVar)>)>> =
        RefCell::new(AddrMap::default());
}

#[derive(Default)]
struct FreeCells {
    /// [`tvar::cell_writes`] when the entries were read from the cells.
    writes: u64,
    of: AddrMap<(Arc<FnType>, Vec<(ArcStr, TVar)>)>,
}

impl FreeCells {
    /// The entries that still stand: none after a cell write.
    fn current(&mut self) -> &mut AddrMap<(Arc<FnType>, Vec<(ArcStr, TVar)>)> {
        let writes = tvar::cell_writes();
        if self.writes != writes {
            self.writes = writes;
            self.of.clear();
        }
        &mut self.of
    }
}

#[cfg(test)]
thread_local! {
    pub(super) static TVAR_WALK_STEPS: Cell<usize> = const { Cell::new(0) };
}

/// One `for_each_part` call's depth; the outermost forgets the positions
/// memo on its way out, unwind too.
struct PartsDepth;

impl PartsDepth {
    fn enter() -> Self {
        PARTS_DEPTH.with(|d| d.set(d.get() + 1));
        PartsDepth
    }
}

impl Drop for PartsDepth {
    fn drop(&mut self) {
        let outermost = PARTS_DEPTH.with(|d| {
            d.set(d.get() - 1);
            d.get() == 0
        });
        if outermost {
            // a signature's parse keeps the positions memo to its end
            if PARSE_DEPTH.with(|d| d.get()) == 0 {
                POSITIONS.with_borrow_mut(|m| {
                    if !m.is_empty() {
                        m.clear()
                    }
                });
            }
            FREE.with_borrow_mut(|m| {
                if !m.of.is_empty() {
                    m.of.clear()
                }
            });
        }
    }
}

/// One signature's parse, which the signatures it holds are parsed
/// inside; the outermost forgets the declared signatures and the
/// positions memo on its way out, unwind too.
pub struct ParseDepth;

impl ParseDepth {
    pub fn enter() -> Self {
        PARSE_DEPTH.with(|d| d.set(d.get() + 1));
        ParseDepth
    }
}

impl Drop for ParseDepth {
    fn drop(&mut self) {
        let outermost = PARSE_DEPTH.with(|d| {
            d.set(d.get() - 1);
            d.get() == 0
        });
        if outermost {
            DECLARED.with_borrow_mut(|m| m.clear());
            if PARTS_DEPTH.with(|d| d.get()) == 0 {
                POSITIONS.with_borrow_mut(|m| m.clear());
            }
        }
    }
}

/// Which names an alias walk aliases: every name it meets, or only those
/// its map holds as it starts. A declaration inside another signature's
/// parse aliases its own quantifiers alone; the enclosing declarations
/// alias the rest, the outermost every name left.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Aliasing {
    Every,
    Known,
}

/// `f` of the cells `ft` names from outside itself, where it is a
/// signature declared inside the parse in progress; `None` for any other.
fn declared<R>(ft: &Arc<FnType>, f: impl FnOnce(&[(ArcStr, TVar)]) -> R) -> Option<R> {
    if PARSE_DEPTH.with(|d| d.get()) == 0 {
        return None;
    }
    DECLARED.with_borrow(|m| m.get(&(Arc::as_ptr(ft) as usize)).map(|(_, free)| f(free)))
}

/// Whether `ft` is a signature the parse in progress declared naming no
/// variable from outside itself: a walk for an enclosing signature's
/// variables finds none in it.
pub(super) fn closed(ft: &Arc<FnType>) -> bool {
    declared(ft, <[_]>::is_empty).unwrap_or(false)
}

/// Where `ft` is a signature the parse in progress declared, the cells it
/// names from outside itself into `known`, and true.
pub(super) fn declared_free(
    ft: &Arc<FnType>,
    known: &mut AHashMap<ArcStr, TVar>,
) -> bool {
    declared(ft, |free| {
        for (name, tv) in free {
            known.entry(name.clone()).or_insert_with(|| tv.clone());
        }
    })
    .is_some()
}

/// Whether an alias walk over `known` aliasing `how` enters `ft`: a
/// signature the parse in progress declared only where it names, from
/// outside itself, a name the walk aliases.
pub(super) fn aliases_into(
    ft: &Arc<FnType>,
    known: &AHashMap<ArcStr, TVar>,
    how: Aliasing,
) -> bool {
    declared(ft, |free| match how {
        Aliasing::Every => !free.is_empty(),
        Aliasing::Known => free.iter().any(|(name, _)| known.contains_key(name)),
    })
    .unwrap_or(true)
}

/// The cells the quantified signature `ft` names from outside itself, its
/// own quantifiers' left out, into `known`: memoized while a
/// `for_each_part` walk is in progress. A walk no guard cut is the same
/// under every guard, and one that found nothing finds nothing under any.
pub(super) fn free_cells(ft: &Arc<FnType>, known: &mut AHashMap<ArcStr, TVar>) {
    let key = Arc::as_ptr(ft) as usize;
    let unguarded = || {
        CONSTRAINING.with_borrow(|s| s.is_empty())
            && CONJUNCTS.with_borrow(|s| s.is_empty())
    };
    let hit = FREE.with_borrow_mut(|m| match m.current().get(&key) {
        Some((_, cells)) if cells.is_empty() || unguarded() => {
            for (name, tv) in cells {
                known.entry(name.clone()).or_insert_with(|| tv.clone());
            }
            true
        }
        Some(_) | None => false,
    });
    if hit {
        return;
    }
    let cuts = CUTS.with(|c| c.get());
    let mut inner: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut inner);
    inner.retain(|name, _| !ft.quantifiers.contains(name));
    if PARTS_DEPTH.with(|d| d.get()) > 0 && CUTS.with(|c| c.get()) == cuts {
        let cells = inner.iter().map(|(n, tv)| (n.clone(), tv.clone())).collect();
        FREE.with_borrow_mut(|m| m.current().insert(key, (ft.clone(), cells)));
    }
    for (name, tv) in inner.drain() {
        known.entry(name).or_insert(tv);
    }
}

/// The cells `ft`'s positions name, its own quantifiers' included, into
/// `out`: memoized while a `for_each_part` walk is in progress.
pub(super) fn position_cells(ft: &Arc<FnType>, out: &mut AHashMap<ArcStr, TVar>) {
    let key = Arc::as_ptr(ft) as usize;
    let hit = POSITIONS.with_borrow(|m| {
        m.get(&key).map(|(_, cells)| {
            for (name, tv) in cells {
                out.entry(name.clone()).or_insert_with(|| tv.clone());
            }
        })
    });
    if hit.is_some() {
        return;
    }
    let mut inner: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
    ft.for_each_type(&mut |t| t.collect_position_tvars(&mut inner, &mut seen));
    if PARTS_DEPTH.with(|d| d.get()) > 0 {
        let cells = inner.iter().map(|(n, tv)| (n.clone(), tv.clone())).collect();
        POSITIONS.with_borrow_mut(|m| m.insert(key, (ft.clone(), cells)));
    }
    for (name, tv) in inner.drain() {
        out.entry(name).or_insert(tv);
    }
}

/// One walk's reentrancy guard on a signature: a conjunct can reach the
/// signature it constrains. The key is released on drop, unwind too.
struct Walking {
    set: &'static LocalKey<RefCell<AddrSet>>,
    key: usize,
}

impl Walking {
    fn enter(set: &'static LocalKey<RefCell<AddrSet>>, ft: &FnType) -> Option<Self> {
        Self::enter_key(set, ft as *const FnType as usize)
    }

    fn enter_key(set: &'static LocalKey<RefCell<AddrSet>>, key: usize) -> Option<Self> {
        let entered = set.with_borrow_mut(|s| s.insert(key));
        if !entered {
            CUTS.with(|c| c.set(c.get() + 1));
        }
        entered.then_some(Walking { set, key })
    }
}

impl Drop for Walking {
    fn drop(&mut self) {
        self.set.with_borrow_mut(|s| s.remove(&self.key));
    }
}

/// Refuse a bound that reaches its own variable: directly (`'a: 'a`), under
/// a constructor (`'a: Array<'a>`) or through another declared variable's
/// bound (`'a: 'b, 'b: 'a`). A recursive bound is a recursive type.
pub fn check_bounds_acyclic(constraints: &[(TVar, Type)]) -> Result<()> {
    // each declared variable's node, numbered in the order first declared
    let mut node: LPooled<AHashMap<&ArcStr, usize>> = LPooled::take();
    let mut names: LPooled<Vec<&ArcStr>> = LPooled::take();
    for (tv, _) in constraints {
        node.entry(&tv.name).or_insert_with(|| {
            names.push(&tv.name);
            names.len() - 1
        });
    }
    // (variable, a declared variable one of its bounds names): each bound's
    // names sorted, the bounds in the order written, grouped by variable
    let mut edges: LPooled<Vec<(usize, usize)>> = LPooled::take();
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    let mut to: LPooled<Vec<ArcStr>> = LPooled::take();
    for (tv, tc) in constraints {
        tc.collect_tvars(&mut named);
        to.extend(named.drain().map(|(n, _)| n).filter(|n| node.contains_key(n)));
        to.sort_unstable();
        let from = node[&tv.name];
        edges.extend(to.drain(..).map(|n| (from, node[&n])));
    }
    edges.sort_by_key(|(from, _)| *from);
    let mut start: LPooled<Vec<usize>> = LPooled::take();
    start.extend((0..=names.len()).map(|v| edges.partition_point(|(f, _)| *f < v)));
    let named_by = |v: usize| edges[start[v]..start[v + 1]].iter().map(|(_, to)| *to);
    let cyclic = on_cycles(&start, &edges);
    let Some(q) = constraints.iter().map(|(tv, _)| node[&tv.name]).find(|q| cyclic[*q])
    else {
        return Ok(());
    };
    let mut seen: LPooled<Vec<bool>> = LPooled::take();
    seen.resize(names.len(), false);
    // (variable reached, the variable q's own bound named on the way),
    // breadth first: the first path back is the shortest
    let mut todo: LPooled<VecDeque<(usize, usize)>> = LPooled::take();
    todo.extend(named_by(q).map(|n| (n, n)));
    while let Some((n, via)) = todo.pop_front() {
        if n == q {
            let (q, via) = (names[q], names[via]);
            let path = match via == q {
                true => format_compact!("'{q}'s bound names '{q}"),
                false => format_compact!("'{q}'s bound reaches '{q} through '{via}"),
            };
            bail!(
                "{path}: a bound may not reach its own variable; a recursive \
                 bound is a recursive type (`type T = [i64, Array<T>]`, `'{q}: T`)"
            )
        }
        if !std::mem::replace(&mut seen[n], true) {
            todo.extend(named_by(n).map(|m| (m, via)));
        }
    }
    unreachable!("'{} lies on a cycle of bounds", names[q])
}

/// Which nodes lie on a cycle of `edges`, (source, target) pairs grouped
/// by source, the edges out of node `v` at `start[v]..start[v + 1]`: the
/// members of a strongly connected component of two or more, and a node
/// with an edge to itself (Tarjan's algorithm, iterative).
fn on_cycles(start: &[usize], edges: &[(usize, usize)]) -> LPooled<Vec<bool>> {
    const UNSEEN: usize = usize::MAX;
    let n = start.len() - 1;
    let mut cyclic: LPooled<Vec<bool>> = LPooled::take();
    cyclic.resize(n, false);
    // (visit number, lowest visit number reachable) per node
    let mut num: LPooled<Vec<(usize, usize)>> = LPooled::take();
    num.resize(n, (UNSEEN, UNSEEN));
    let mut on_stack: LPooled<Vec<bool>> = LPooled::take();
    on_stack.resize(n, false);
    let mut stack: LPooled<Vec<usize>> = LPooled::take();
    // (node, its next edge to follow)
    let mut calls: LPooled<Vec<(usize, usize)>> = LPooled::take();
    let mut count = 0;
    for root in 0..n {
        if num[root].0 != UNSEEN {
            continue;
        }
        let mut enter = Some(root);
        loop {
            if let Some(v) = enter.take() {
                num[v] = (count, count);
                count += 1;
                on_stack[v] = true;
                stack.push(v);
                calls.push((v, start[v]));
            }
            let Some((v, e)) = calls.last_mut() else { break };
            let v = *v;
            if *e < start[v + 1] {
                let w = edges[*e].1;
                *e += 1;
                cyclic[v] |= w == v;
                if num[w].0 == UNSEEN {
                    enter = Some(w)
                } else if on_stack[w] {
                    num[v].1 = num[v].1.min(num[w].0)
                }
                continue;
            }
            calls.pop();
            if let Some((u, _)) = calls.last() {
                num[*u].1 = num[*u].1.min(num[v].1)
            }
            if num[v].0 == num[v].1 {
                let at = stack.iter().rposition(|w| *w == v).unwrap();
                let component = &stack[at..];
                for w in component {
                    on_stack[*w] = false;
                    cyclic[*w] |= component.len() > 1;
                }
                stack.truncate(at);
            }
        }
    }
    cyclic
}

/// Seed each conjunct on its quantifier's cell in `known`, aliasing every
/// conjunct's own variables onto the quantifiers before any cell holds a
/// conjunct, so no alias's occurs check walks a chain of bounds.
pub fn seed_conjuncts<'a>(
    known: &mut AHashMap<ArcStr, TVar>,
    constraints: impl IntoIterator<Item = (&'a TVar, Type)>,
) {
    seed_conjuncts_aliasing(known, &[], constraints, Aliasing::Every)
}

/// [`seed_conjuncts`] in the scope of the signature quantifying `own`, the
/// conjuncts' variables aliased `how`.
fn seed_conjuncts_aliasing<'a>(
    known: &mut AHashMap<ArcStr, TVar>,
    own: &[ArcStr],
    constraints: impl IntoIterator<Item = (&'a TVar, Type)>,
    how: Aliasing,
) {
    let constraints: LPooled<Vec<(&TVar, Type)>> = constraints.into_iter().collect();
    let passed = tvar::ConjunctsPassed::within(own);
    for (_, tc) in constraints.iter() {
        tc.alias_tvars_in(known, &passed, how);
    }
    for (tv, tc) in constraints.iter() {
        known[&tv.name].add_cell_constraint(tc.normalize());
    }
}

/// `known` and, until nothing new arrives, the cells the bounds of its
/// cells name.
fn close_through_bounds(known: &mut AHashMap<ArcStr, TVar>, seen: &mut TVarsSeen) {
    let mut todo: LPooled<Vec<TVar>> = LPooled::take();
    todo.extend(known.values().cloned());
    let mut found: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    while let Some(tv) = todo.pop() {
        for tc in tv.cell_constraints() {
            tc.collect_tvars_seen(&mut found, seen);
        }
        for (name, tv) in found.drain() {
            if !known.contains_key(&name) {
                known.insert(name, tv.clone());
                todo.push(tv);
            }
        }
    }
}

/// The variables `t` names, in order, a signature's conjuncts included.
fn var_names(t: &Type) -> LPooled<Vec<ArcStr>> {
    fn walk(t: &Type, out: &mut Vec<ArcStr>) {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => out.push(tv.name.clone()),
            Type::Fn(ft) => ft.for_each_part(&mut |t, _| walk(t, out)),
            t => t.for_each_child(&mut |c| walk(c, out)),
        })
    }
    let mut out: LPooled<Vec<ArcStr>> = LPooled::take();
    walk(t, &mut out);
    out
}

impl FnType {
    /// `self` declaring `constraints`: its quantifiers are their names
    /// in source order (a `+`-bound variable is one quantifier), and every
    /// same-named tvar of the signature and of a conjunct is the
    /// quantifier's, whose cell holds the conjuncts. Inside another
    /// signature's parse ([`ParseDepth`]) only its own quantifiers are
    /// aliased, every other name by the enclosing declarations
    /// ([`Aliasing`]), and the cells it names from outside itself are
    /// recorded for the parse's walks.
    pub fn declaring(mut self, constraints: &[(TVar, Type)]) -> Arc<Self> {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut names: LPooled<Vec<ArcStr>> = LPooled::take();
        for (tv, _) in constraints.iter() {
            if !known.contains_key(&tv.name) {
                names.push(tv.name.clone());
                known.insert(tv.name.clone(), tv.clone());
            }
        }
        self.quantifiers = Arc::from_iter(names.drain(..));
        let ft = Arc::new(self);
        let nested = PARSE_DEPTH.with(|d| d.get()) > 1;
        let how = if nested { Aliasing::Known } else { Aliasing::Every };
        ft.alias_tvars_in(
            &mut known,
            &tvar::ConjunctsPassed::within(&ft.quantifiers),
            how,
        );
        let constraints = constraints.iter().map(|(tv, tc)| (tv, tc.clone()));
        seed_conjuncts_aliasing(&mut known, &ft.quantifiers, constraints, how);
        if nested {
            known.clear();
            ft.collect_tvars(&mut known);
            let free = known.drain().filter(|(name, _)| !ft.quantifiers.contains(name));
            let free = (ft.clone(), free.collect());
            DECLARED.with_borrow_mut(|m| m.insert(Arc::as_ptr(&ft) as usize, free));
        }
        ft
    }

    /// The tvar cells reachable from args / vargs / rtype / throws,
    /// by name.
    pub(crate) fn sig_tvars(&self) -> LPooled<AHashMap<ArcStr, TVar>> {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.for_each_type(&mut |t| t.collect_tvars(&mut known));
        known
    }

    /// [`Self::collect_tvars`] and then, until nothing new arrives, the
    /// cells the bounds of the cells found name: a quantifier only the
    /// bound of a bound names is reached too.
    pub fn collect_tvars_through_bounds(&self, known: &mut AHashMap<ArcStr, TVar>) {
        let mut seen = TVarsSeen::take();
        self.collect_tvars_seen(known, &mut seen);
        close_through_bounds(known, &mut seen)
    }

    /// Whether a trait method's `self` occurs in a bound of a variable the
    /// signature holds, through its bounds.
    pub fn bound_mentions_self(&self) -> bool {
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars_through_bounds(&mut held);
        held.values().any(|tv| tv.cell_constraints().iter().any(Type::mentions_self))
    }

    /// `(tvar, conjunct)` for every conjunct of every declared quantifier,
    /// normalized (so equality and display agree on the canonical form),
    /// sorted by name then conjunct: the part of the constraints that is
    /// the signature's identity.
    pub fn constraint_view(&self) -> LPooled<Vec<(TVar, Type)>> {
        let mut view: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        if self.quantifiers.is_empty() {
            return view;
        }
        let Some(_guard) = Walking::enter(&VIEWING, self) else { return view };
        // every cell the signature reaches, through its conjuncts too: a
        // quantifier may occur only inside another's bound
        let mut reached: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut reached);
        if !self.quantifiers.iter().all(|q| reached.contains_key(q)) {
            close_through_bounds(&mut reached, &mut TVarsSeen::take());
        }
        self.view_of(&mut reached, &mut view);
        view
    }

    /// The part of [`Self::constraint_view`] after the cells are found:
    /// the conjuncts of the quantifiers in `reached`, into `view`.
    fn view_of(
        &self,
        reached: &mut AHashMap<ArcStr, TVar>,
        view: &mut Vec<(TVar, Type)>,
    ) {
        let mut cx = NormCx::take();
        for q in self.quantifiers.iter() {
            let Some(tv) = reached.remove(q) else { continue };
            for tc in tv.cell_constraints() {
                let n = tc.normalize_int(&mut cx).unwrap_or_else(|| tc.clone());
                view.push((tv.clone(), n));
            }
        }
        // equality is blind to an open variable's name: `'a: 'b + 'c` holds two
        view.sort_by(|(a, x), (b, y)| {
            a.name
                .cmp(&b.name)
                .then_with(|| x.cmp(y))
                .then_with(|| var_names(x).as_slice().cmp(var_names(y).as_slice()))
        });
        view.dedup_by(|(a, x), (b, y)| {
            a.name == b.name
                && x == y
                && var_names(x).as_slice() == var_names(y).as_slice()
        });
    }

    /// [`Self::constraint_view`] for printing, which asks it of every
    /// signature of a nest: the quantifiers are looked up among the
    /// cells the positions name (a nested signature's, once a print), and
    /// only a quantifier no position names takes the walk through bounds.
    fn printed_view(&self) -> LPooled<Vec<(TVar, Type)>> {
        let mut view: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        if self.quantifiers.is_empty() {
            return view;
        }
        let Some(_guard) = Walking::enter(&VIEWING, self) else { return view };
        let mut reached: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        self.for_each_type(&mut |t| t.collect_position_tvars(&mut reached, &mut seen));
        if !self.quantifiers.iter().all(|q| reached.contains_key(q)) {
            reached.clear();
            self.collect_tvars_through_bounds(&mut reached);
        }
        self.view_of(&mut reached, &mut view);
        view
    }

    /// The canonical bytes the image keys this type by: the shape with
    /// every variable by identity, and the lambda ids cell by identity.
    pub(crate) fn content_key(&self, out: &mut Vec<u8>) {
        let Self { args, vargs, rtype, throws, explicit_throws, quantifiers, lambda_ids } =
            self;
        encode_varint(quantifiers.len() as u64, out);
        for q in quantifiers.iter() {
            key_text(q, out);
        }
        encode_varint(args.len() as u64, out);
        for a in args.iter() {
            match &a.kind {
                FnArgKind::Positional { name } => {
                    out.put_u8(0);
                    match name {
                        None => out.put_u8(0),
                        Some(n) => {
                            out.put_u8(1);
                            key_text(n, out);
                        }
                    }
                }
                FnArgKind::Labeled { name, has_default } => {
                    out.put_u8(1);
                    key_text(name, out);
                    out.put_u8(*has_default as u8);
                }
            }
            a.typ.content_key(out);
        }
        match vargs {
            None => out.put_u8(0),
            Some(t) => {
                out.put_u8(1);
                t.content_key(out);
            }
        }
        rtype.content_key(out);
        throws.content_key(out);
        out.put_u8(*explicit_throws as u8);
        out.put_u64_le(lambda_ids.addr() as u64);
    }
}

/// Structural: `lambda_ids` is provenance, `explicit_throws` does not
/// survive a print round-trip when throws is Bottom, and the declared
/// constraints compare only once the shape is equal.
impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        let _scope = PartsDepth::enter();
        self.args == other.args
            && self.vargs == other.vargs
            && self.rtype == other.rtype
            && self.throws == other.throws
            && *self.constraint_view() == *other.constraint_view()
    }
}

impl Eq for FnType {}

impl PartialOrd for FnType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnType {
    fn cmp(&self, other: &Self) -> Ordering {
        let _scope = PartsDepth::enter();
        self.args
            .cmp(&other.args)
            .then_with(|| self.vargs.cmp(&other.vargs))
            .then_with(|| self.rtype.cmp(&other.rtype))
            .then_with(|| (*self.constraint_view()).cmp(&*other.constraint_view()))
            .then_with(|| self.throws.cmp(&other.throws))
    }
}

/// The shape only: equal signatures have equal shapes.
impl Hash for FnType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.args.hash(state);
        self.vargs.hash(state);
        self.rtype.hash(state);
        self.throws.hash(state);
    }
}

impl Default for FnType {
    fn default() -> Self {
        Self {
            args: Arc::from_iter([]),
            vargs: None,
            rtype: Default::default(),
            throws: Default::default(),
            explicit_throws: false,
            quantifiers: Arc::from_iter([]),
            lambda_ids: LambdaIds::default(),
        }
    }
}

impl FnType {
    /// `None` when already normal.
    pub(super) fn normalize_int(
        &self,
        cx: &mut super::normalize::NormCx,
    ) -> Option<Self> {
        self.cow_walk(|t| t.normalize_int(cx))
    }

    /// See [`Type::resolve_tvars`].
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen_int(&mut super::normalize::ResolveTvarsCx::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn resolve_tvars_seen_int(
        &self,
        cx: &mut super::normalize::ResolveTvarsCx,
    ) -> Option<Self> {
        self.open_quantifiers(|_, open| {
            cx.quantifiers.insert(open.cell_addr());
        });
        self.cow_walk(|t| t.resolve_tvars_seen_int(cx)).map(|f| {
            tvar::keep_quantifiers(self, f, |tv| {
                Type::TVar(tv.clone()).resolve_tvars_seen_int(cx)
            })
        })
    }

    /// Rewrite every type position through `f` (`None` means
    /// unchanged), rebuilding only if something changed.
    pub(crate) fn cow_walk(
        &self,
        mut f: impl FnMut(&Type) -> Option<Type>,
    ) -> Option<Self> {
        let Self { args, vargs, rtype, throws, explicit_throws, quantifiers, lambda_ids } =
            self;
        let new_args = Type::cow_slice(args, |a| {
            f(&a.typ).map(|typ| FnArgType { kind: a.kind.clone(), typ })
        });
        let new_vargs = vargs.as_ref().and_then(&mut f);
        let new_rtype = f(rtype);
        let new_throws = f(throws);
        if new_args.is_none()
            && new_vargs.is_none()
            && new_rtype.is_none()
            && new_throws.is_none()
        {
            return None;
        }
        Some(FnType {
            args: new_args.unwrap_or_else(|| args.clone()),
            vargs: match new_vargs {
                Some(t) => Some(t),
                None => vargs.clone(),
            },
            rtype: new_rtype.unwrap_or_else(|| rtype.clone()),
            throws: new_throws.unwrap_or_else(|| throws.clone()),
            explicit_throws: *explicit_throws,
            quantifiers: quantifiers.clone(),
            lambda_ids: lambda_ids.clone(),
        })
    }

    /// Read-only walk over args, vargs, rtype, throws in that order.
    /// Cell constraints are not visited; [`Self::for_each_part`] visits
    /// the signature cells' conjuncts too, and which of the two a walk
    /// takes decides whether it sees quantifier bounds.
    pub(crate) fn try_for_each_type<B>(
        &self,
        f: &mut impl FnMut(&Type) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        let FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        for arg in args.iter() {
            f(&arg.typ)?;
        }
        if let Some(t) = vargs {
            f(t)?;
        }
        f(rtype)?;
        f(throws)
    }

    /// [`Self::try_for_each_type`] without early exit.
    #[doc(hidden)]
    pub fn for_each_type(&self, f: &mut impl FnMut(&Type)) {
        let _ = self.try_for_each_type::<()>(&mut |t| {
            f(t);
            ControlFlow::Continue(())
        });
    }

    /// Every type position of the signature in its one order: args,
    /// vargs, rtype, each signature cell's conjuncts (flagged `true`),
    /// throws. [`Self::alias_tvars`] makes the first-seen occurrence of a
    /// name the surviving cell, so the order is observable. The
    /// conjuncts are guarded per `FnType` address: one can reach back.
    #[doc(hidden)]
    pub fn for_each_part(&self, f: &mut impl FnMut(&Type, bool)) {
        self.for_each_part_passing(&mut |_| true, f)
    }

    /// [`Self::for_each_part`], passing the conjuncts only of the cells
    /// `pass` admits.
    fn for_each_part_passing(
        &self,
        pass: &mut impl FnMut(&TVar) -> bool,
        f: &mut impl FnMut(&Type, bool),
    ) {
        let _depth = PartsDepth::enter();
        let FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        for arg in args.iter() {
            f(&arg.typ, false)
        }
        if let Some(vargs) = vargs {
            f(vargs, false)
        }
        f(rtype, false);
        if let Some(_guard) = Walking::enter(&CONSTRAINING, self) {
            // the cells the positions name, each nested signature's own
            // conjunct pass left to that signature's walk
            let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
            self.for_each_type(&mut |t| t.collect_position_tvars(&mut cells, &mut seen));
            for (_, tv) in sorted_tvars(cells.drain()).drain(..) {
                if let Some(_cell) = Walking::enter_key(&CONJUNCTS, tv.cell_addr())
                    && pass(&tv)
                {
                    for tc in tv.cell_constraints() {
                        f(&tc, true);
                    }
                }
            }
        }
        f(throws, false);
    }

    pub fn unbind_tvars(&self) {
        self.for_each_type(&mut |t| t.unbind_tvars())
    }

    /// [`Type::unbind_vacuous_tvars`] over the signature.
    pub fn unbind_vacuous_tvars(&self) {
        self.for_each_type(&mut |t| t.unbind_vacuous_tvars())
    }

    pub fn reset_tvars(&self) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Copy)
    }

    /// [`Type::instantiate_with`] over the signature.
    #[doc(hidden)]
    pub fn instantiate_with(
        &self,
        known: &mut AHashMap<usize, TVar>,
        open: &IntSet<LambdaId>,
    ) -> Self {
        self.instantiate_how(known, Fresh::Instantiate(open))
    }

    /// [`Type::rename_with`] over the signature.
    #[doc(hidden)]
    pub fn rename_with(&self, known: &AHashMap<usize, TVar>) -> Self {
        match self.cow_walk(|t| t.rename_int(known)) {
            None => self.clone(),
            Some(f) => tvar::keep_quantifiers(self, f, |tv| {
                Type::TVar(tv.clone()).rename_int(known)
            }),
        }
    }

    /// [`Self::instantiate_with`] copying every open cell, whoever owns
    /// it: an instance's own copy of its definition's signature, which its
    /// check then binds without writing a cell outside the instance.
    #[doc(hidden)]
    pub fn instantiate_owned(&self, known: &mut AHashMap<usize, TVar>) -> Self {
        self.instantiate_how(known, Fresh::Copy)
    }

    pub(super) fn instantiate_how(
        &self,
        known: &mut AHashMap<usize, TVar>,
        how: Fresh<'_>,
    ) -> Self {
        let fresh = self
            .cow_walk(|t| t.instantiate_int(known, how))
            .unwrap_or_else(|| self.clone());
        let mut fresh = tvar::keep_quantifiers(self, fresh, |tv| {
            Type::TVar(tv.clone()).instantiate_int(known, how)
        });
        fresh.lambda_ids = self.lambda_ids.instantiate();
        fresh
    }

    /// One cell-identity freshening map across the whole signature
    /// (see [`Type::reset_tvars_int`]). Always a fresh signature: an
    /// instantiation's `lambda_ids` is its own.
    pub(super) fn reset_tvars_int(
        &self,
        known: &mut AHashMap<usize, TVar>,
        how: Fresh<'_>,
    ) -> Self {
        let mut fresh = self
            .cow_walk(|t| t.reset_tvars_int(known, how))
            .unwrap_or_else(|| self.clone());
        fresh.lambda_ids = self.lambda_ids.instantiate();
        fresh
    }

    /// [`Type::replace_tvars`] over the signature: its own quantifiers are
    /// names like any other here, a function type it holds keeps its own.
    pub fn replace_tvars(&self, known: &AHashMap<ArcStr, Type>) -> Self {
        self.replace_tvars_int(&mut tvar::Subst::new(known), &mut LPooled::take())
            .unwrap_or_else(|| self.clone())
    }

    /// `None` when no TVar is beneath.
    pub(super) fn replace_tvars_int(
        &self,
        s: &mut tvar::Subst,
        fresh: &mut AHashMap<usize, TVar>,
    ) -> Option<Self> {
        self.cow_walk(|t| t.replace_tvars_int(s, fresh))
    }

    // CR claude for claude: [bug/medium] A hover shows a decided variable as its
    // bound: `let h = 'a: Number |x: 'a| -> 'a x; let n = |q: i64| h(q)` hovers (and
    // outlines) `n: fn(q: i64) -> Number`, where the check says `fn(q: i64) -> i64`
    // (`let z: string = n` prints it). This replaces every `'_N` the signature holds
    // that carries one conjunct by that conjunct, a cell already bound to `i64`
    // included: with `'a: Number + Singleton` (two conjuncts) the hover reads `->
    // i64`. Base-r23 alike. The shell's REPL completion (`graphix-shell/src/
    // completion.rs`) folds through this too, and prints canonically rather than
    // through `graphix_lsp::query::display_type`, so by reading it also lists one
    // instance's header (hover-instance-header-01). probe:
    // ~/ptmp/cr29/scratch/r23/23h/hover-review/probe_test.rs (`nested`, `nested3`,
    // `nested4`; outputs in lane-final.txt beside it) (display-auto-bound-01)
    /// Replace auto type variables (`'_23`) that carry one constraint
    /// with that constraint, for display. Call before
    /// `Type::resolve_tvars`, which discards the cells.
    pub fn replace_auto_constrained(&self) -> Self {
        let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
        // Read the cells directly: auto names are never declared, so
        // `constraint_view` cannot see them.
        for (name, tv) in self.sig_tvars().drain() {
            if name.starts_with('_')
                && let [tc] = &tv.cell_constraints()[..]
            {
                known.insert(name, tc.clone());
            }
        }
        let mut all_tvars: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut all_tvars);
        for (name, tv) in all_tvars.drain() {
            known.entry(name).or_insert(Type::TVar(tv));
        }
        self.replace_tvars(&known)
    }

    pub fn has_unbound(&self) -> bool {
        self.try_for_each_type(&mut |t| {
            if t.has_unbound() {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            }
        })
        .is_break()
    }

    pub fn alias_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        self.alias_tvars_in(
            known,
            &tvar::ConjunctsPassed::within(&self.quantifiers),
            Aliasing::Every,
        )
    }

    pub(super) fn alias_tvars_in(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        passed: &tvar::ConjunctsPassed,
        how: Aliasing,
    ) {
        self.for_each_part_passing(&mut |tv| passed.first(tv), &mut |t, _| {
            t.alias_tvars_in(known, passed, how)
        })
    }

    /// A reference's copy of the scheme (`Fresh::Scheme`) under the open
    /// gates `open`.
    #[doc(hidden)]
    pub fn scheme(&self, open: &IntSet<LambdaId>) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Scheme(open))
    }

    /// One instance of the scheme under the open gates `open`: what a
    /// call copies is a fresh cell at the current level, which every use of
    /// the copy shares (`Fresh::Instantiate`).
    #[doc(hidden)]
    pub fn one_instance(&self, open: &IntSet<LambdaId>) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Instantiate(open))
    }

    /// A quantifier an earlier compile task left open: the check decided
    /// it open, so a later task compares the type as any instance of
    /// itself and never writes the cell (`tvar::earlier_task`), except an
    /// input's own check, to which an earlier input's is one type
    /// (`tvar::in_input`).
    fn has_earlier_quantifier(&self) -> bool {
        if super::tvar::in_input() {
            return false;
        }
        let mut any = false;
        self.open_quantifiers(|_, open| any |= open.earlier_task());
        any
    }

    /// Each of the signature's own quantifiers, as the signature names it.
    pub fn each_quantifier(&self, mut f: impl FnMut(&ArcStr, &TVar)) {
        if self.quantifiers.is_empty() {
            return;
        }
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut named);
        for q in self.quantifiers.iter() {
            if let Some(tv) = named.get(q) {
                f(q, tv)
            }
        }
    }

    /// Refuse a declared quantifier the signature does not hold: no
    /// position names it, nor a bound of a variable the signature holds.
    /// No call can choose one, so its declaration would decide nothing.
    pub fn check_quantifiers_held(&self) -> Result<()> {
        if self.quantifiers.is_empty() {
            return Ok(());
        }
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut seen = TVarsSeen::take();
        self.collect_tvars_seen(&mut held, &mut seen);
        let mut todo: LPooled<Vec<TVar>> = held.values().cloned().collect();
        let mut found: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        while let Some(tv) = todo.pop() {
            for tc in tv.cell_constraints() {
                match tc.normalize_int(&mut seen.norm) {
                    Some(n) => n.collect_tvars_seen(&mut found, &mut seen),
                    None => tc.collect_tvars_seen(&mut found, &mut seen),
                }
            }
            for (name, tv) in found.drain() {
                if !held.contains_key(&name) {
                    todo.push(tv.clone());
                    held.insert(name, tv);
                }
            }
        }
        match self.quantifiers.iter().find(|q| !held.contains_key(*q)) {
            None => Ok(()),
            Some(q) => bail!(
                "'{q} is declared but the signature does not hold it: write it on \
                 a parameter, the return type, the throws or the bound of a \
                 quantifier the signature holds, or remove it"
            ),
        }
    }

    /// Each of the signature's own quantifiers whose cell is open, as the
    /// signature names it and as its open cell.
    fn open_quantifiers(&self, mut f: impl FnMut(&TVar, TVar)) {
        self.each_quantifier(|_, tv| {
            if let Some(open) = tv.open_cell() {
                f(tv, open)
            }
        })
    }

    /// Map each of the signature's own open quantifiers to a fresh cell
    /// in `known`, its conjuncts copied through `walk`: every call picks
    /// them anew, whatever gate owns them.
    fn fresh_quantifiers(
        &self,
        known: &mut AHashMap<usize, TVar>,
        walk: impl Fn(&Type, &mut AHashMap<usize, TVar>) -> Type,
    ) {
        let mut opens: LPooled<Vec<TVar>> = LPooled::take();
        self.open_quantifiers(|_, open| opens.push(open));
        for tv in opens.iter() {
            known
                .entry(tv.cell_addr())
                .or_insert_with(|| TVar::empty_named(tv.name.clone()));
        }
        for tv in opens.iter() {
            let f = known[&tv.cell_addr()].clone();
            for c in tv.cell_constraints() {
                f.add_cell_constraint(walk(&c, known));
            }
        }
    }

    /// The open quantifiers of every function type the signature holds, by
    /// cell: a generic function's a scheme holds.
    pub fn inner_quantifiers(&self, out: &mut AHashMap<usize, TVar>) {
        let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
        self.for_each_part(&mut |t, _| {
            t.quantifiers_seen(true, &mut seen, &mut |_, tv| {
                if let Some(open) = tv.open_cell() {
                    out.insert(open.cell_addr(), open);
                }
            })
        });
    }

    /// A copy whose own open quantifiers are fresh cells; every other cell
    /// is shared.
    fn fresh_own_quantifiers(&self) -> Self {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let image = |known: &AHashMap<usize, TVar>, a| {
            known.get(&a).map(|f| Type::TVar(f.clone()))
        };
        let walk = |c: &Type, known: &mut AHashMap<usize, TVar>| {
            c.swap_cells(&|a| image(known, a))
        };
        self.fresh_quantifiers(&mut known, walk);
        if known.is_empty() {
            return self.clone();
        }
        self.cow_walk(|t| Some(t.swap_cells(&|a| image(&known, a))))
            .unwrap_or_else(|| self.clone())
    }

    /// A call's copy of the signature under the open gates `open`
    /// (`Fresh::Instantiate`), each fresh cell the copy holds more than
    /// once frozen, so it keeps its cell when it unifies with an unfrozen
    /// one.
    pub fn instantiate(&self, open: &IntSet<LambdaId>) -> Self {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        let fresh = self.reset_tvars_int(&mut known, Fresh::Instantiate(open));
        let copies: LPooled<AHashSet<usize>> =
            known.values().map(|tv| tv.cell_addr()).collect();
        let mut occurrences = super::tvar::Occurrences::default();
        fresh.for_each_part(&mut |t, _| t.tvar_occurrences(&mut occurrences));
        for tv in occurrences.vars.iter() {
            let addr = tv.cell_addr();
            if occurrences.many(addr) && copies.contains(&addr) {
                tv.freeze()
            }
        }
        fresh
    }

    /// Mark generic every cell the signature reaches at `depth` or
    /// deeper: the definition at `depth` closed over them. A cell a
    /// binding lowered above it is its environment's.
    #[doc(hidden)]
    pub fn generalize(&self, depth: u32) {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reached_cells(&mut cells);
        for tv in cells.values() {
            if tv.level().depth() >= depth {
                tv.generalize()
            }
        }
    }

    /// Claim every cell the signature reaches for `level`
    /// ([`TVar::claim`]).
    #[doc(hidden)]
    pub fn claim(&self, level: Level) {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reached_cells(&mut cells);
        for tv in cells.values() {
            tv.claim(level)
        }
    }

    /// Conjuncts are visited in their canonical form: a tvar in a
    /// portion that normalizes away carries no constraint force, and
    /// identity must not depend on the stored form.
    pub fn collect_tvars(&self, known: &mut AHashMap<ArcStr, TVar>) {
        self.collect_tvars_seen(known, &mut TVarsSeen::take())
    }

    // CR claude for claude: [bug/low] A name walk over a quantified signature
    // reads a bound of a cell declared outside it under the signature's own
    // names. This walk passes the conjuncts of every position cell
    // (for_each_part) and close_through_bounds those of every cell found, both
    // keyed by name, so where an enclosing definition's cell has a bound naming
    // a variable the signature also declares, the enclosing variable lands
    // under that name, and constraint_view, printed_view, each_quantifier and
    // check_quantifiers_held read it as the signature's own. In `let f = 'a:
    // Number, 'z: Array<'a> |w: 'z| { let g = 'a: Any |x: 'z| -> i64 0; g(w)
    // }; f([1])` g declares `'a`, which nothing of g holds, and the check
    // accepts it (h1.gx; h2.gx, where no outside bound names `'a`, is refused,
    // "'a is declared but the signature does not hold it"). `graphix fmt
    // --stdout` rewrites 5 of 12000 generated nests (gen2.py q mode, seeds 101
    // 202 303 404: fmt/g404_s02086.gx prints its inner signature's own `'z:
    // i64`, held only through a bound, as `'z: Any`) and refuses
    // fmt/g303_s00747.gx as a formatter bug, on base-r23 and after
    // (nested-quantifier-capture-01), which gave the alias walk the scope rule
    // alone. Where one round finds two cells of one name, close_through_bounds
    // keeps one or the other by hash order: collect_tvars_through_bounds of a
    // nested signature of bounds_order.gx disagrees between calls. A fix would
    // give the name walks the alias walk's rule (tvar::ConjunctsPassed): a
    // signature's walk reads the bounds of its own quantifiers alone. probes:
    // ~/ptmp/cr29/scratch/r23/23f/nested-quantifier-capture/ (README)
    // (name-walk-outer-bound-01)
    pub(super) fn collect_tvars_seen(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        seen: &mut TVarsSeen,
    ) {
        self.for_each_part(&mut |t, constraint| match constraint {
            true => match t.normalize_int(&mut seen.norm) {
                Some(n) => n.collect_tvars_seen(known, seen),
                None => t.collect_tvars_seen(known, seen),
            },
            false => t.collect_tvars_seen(known, seen),
        })
    }

    /// Index of the first positional parameter (`args.len()` when all
    /// are labeled); `args[..first_positional()]` is the labeled prefix.
    pub fn first_positional(&self) -> usize {
        self.args.iter().position(|a| a.is_positional()).unwrap_or(self.args.len())
    }

    /// `self`'s arguments paired with `t`'s: labeled by name, positional
    /// by index. The pairs cover what aligns; the flag says whether the
    /// shapes align in full: no label of `self` absent from `t`, no
    /// required label of `t` absent from `self`, as many positionals.
    pub(super) fn align<'a>(
        &'a self,
        t: &'a Self,
    ) -> (SmallVec<[(&'a FnArgType, &'a FnArgType); 8]>, bool) {
        let (sul, tul) = (self.first_positional(), t.first_positional());
        let mut pairs: SmallVec<[(&FnArgType, &FnArgType); 8]> = SmallVec::new();
        let mut ok = self.args.len() - sul == t.args.len() - tul;
        for a in &self.args[..sul] {
            match t.args[..tul].iter().find(|b| b.label() == a.label()) {
                Some(b) => pairs.push((a, b)),
                None => ok = false,
            }
        }
        ok &= t.args[..tul].iter().all(|b| {
            b.has_default() || self.args[..sul].iter().any(|a| a.label() == b.label())
        });
        pairs.extend(self.args[sul..].iter().zip(t.args[tul..].iter()));
        (pairs, ok)
    }

    /// Whether a value of type `t` could match a pattern typed `self`:
    /// same arity and labels, every component could match; nothing is
    /// unified.
    pub(super) fn could_match_int(
        &self,
        env: &Env,
        hist: &mut MatchHist,
        t: &Self,
    ) -> Result<bool> {
        let (pairs, ok) = self.align(t);
        if !ok {
            return Ok(false);
        }
        for (a, b) in pairs {
            if !a.typ.could_match_int(env, hist, &b.typ)? {
                return Ok(false);
            }
        }
        match (&self.vargs, &t.vargs) {
            (None, None) => (),
            (Some(a), Some(b)) => {
                if !a.could_match_int(env, hist, b)? {
                    return Ok(false);
                }
            }
            _ => return Ok(false),
        }
        Ok(self.rtype.could_match_int(env, hist, &t.rtype)?
            && self.throws.could_match_int(env, hist, &t.throws)?)
    }

    pub fn contains(&self, env: &Env, t: &Self) -> Result<bool> {
        self.contains_int(ContainsFlags::Commit.into(), env, &mut ContainsHist::new(), t)
    }

    /// Arguments are contravariant: each of `t`'s parameters must admit
    /// what `self`'s callers pass, and a label `self` lets a caller omit
    /// must be one `t` defaults.
    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        // A quantifier self has not picked stands for every type its bound
        // admits, since each call through the value picks it anew: t must
        // fit it held rigid, wherever the type stands; one an earlier task
        // left open is held rigid in a copy.
        let copy;
        let this = match self.has_earlier_quantifier() {
            true => {
                copy = self.fresh_own_quantifiers();
                &copy
            }
            false => self,
        };
        let gates = this.open_quantifier_gates();
        let gated = u32::from(!gates.is_empty());
        hist.gated += gated;
        let r = this.contains_gated(flags, env, hist, t);
        hist.gated -= gated;
        drop(gates);
        r
    }

    /// [`Self::contains_int`] with self's open quantifiers held rigid.
    fn contains_gated(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
        t: &Self,
    ) -> Result<bool> {
        // a type whose quantifier an earlier task left open is any instance
        // of itself: its quantifiers are fresh for this comparison, so
        // self's bind them
        let fresh;
        let t = match t.has_earlier_quantifier() {
            true => {
                fresh = t.fresh_own_quantifiers();
                &fresh
            }
            false => t,
        };
        let (pairs, ok) = self.align(t);
        if !ok || pairs.iter().any(|(s, t)| s.has_default() && !t.has_default()) {
            return Ok(false);
        }
        let vargs = match (&t.vargs, &self.vargs) {
            (Some(tv), Some(sv)) => Some((tv, sv)),
            (None, None) => None,
            (_, _) => return Ok(false),
        };
        let args = flags ^ ContainsFlags::Contra;
        // each position as (container, contained, flags), in order, the
        // bounds between the result and the throws
        let mut positions: SmallVec<[Option<(&Type, &Type, _)>; 8]> =
            pairs.iter().map(|(s, t)| Some((&t.typ, &s.typ, args))).collect();
        positions.extend(vargs.map(|(tv, sv)| Some((tv, sv, args))));
        let arguments = positions.len();
        positions.push(Some((&self.rtype, &t.rtype, flags)));
        positions.push(None);
        positions.push(Some((&self.throws, &t.throws, flags)));
        // a union of open cells is decided by the positions that decide
        // its members alone, wherever they are written: each position's
        // round is fixed before any position binds a cell
        let later: SmallVec<[bool; 8]> = positions
            .iter()
            .map(|p| {
                p.is_some_and(|(c, t, _)| {
                    (!hist.closed(c) && holds_open_union(c))
                        || (!hist.closed(t) && holds_open_union(t))
                })
            })
            .collect();
        let hold = |hist: &mut ContainsHist, of: Range<usize>, round: bool| {
            for p in of.filter(|i| later[*i] == round).map(|i| &positions[i]) {
                let holds = match p {
                    Some((c, t, flags)) => c.contains_int(*flags, env, hist, t)?,
                    None => {
                        self.bounds_hold(flags, env, hist)?
                            && t.bounds_hold(flags, env, hist)?
                    }
                };
                if !holds {
                    return Ok(false);
                }
            }
            Ok(true)
        };
        // a variable of `t` no function type holds takes the widest of the
        // closed types the arguments give it, whatever their order
        let mark = match flags.contains(ContainsFlags::Commit) {
            true => {
                let cells = t.order_free_cells(env, hist);
                hist.defer(cells)
            }
            false => None,
        };
        let held = hold(hist, 0..arguments, false);
        Ok(hist.settle_deferred(mark, env, held)?
            && hold(hist, arguments..positions.len(), false)?
            && hold(hist, 0..positions.len(), true)?)
    }

    /// The open cells an argument holds that no function type of the
    /// signature holds, nor a union beside another undecided member, in
    /// the order the signature writes them: a value of a wider type is
    /// well typed wherever the signature holds one as data, and a
    /// position that holds one exactly decides it whenever it is met.
    fn order_free_cells(
        &self,
        env: &Env,
        hist: &mut ContainsHist,
    ) -> SmallVec<[TVar; 2]> {
        let mut cells = SmallVec::new();
        let mut open = |t: &Type| !hist.closed(t) && t.has_free_in_data();
        if !self.args.iter().any(|a| open(&a.typ))
            && !self.vargs.as_ref().is_some_and(&mut open)
        {
            return cells;
        }
        let mut found: LPooled<AHashMap<usize, bool>> = LPooled::take();
        let mut positions = DataPositions::new(env).telling_members();
        self.for_each_type(&mut |t| {
            positions.walk(t, Held::default(), &mut |tv, held| {
                if tv.is_bound() || tv.is_rigid() || tv.earlier_task() {
                    return;
                }
                let waits = !held.function && !held.member;
                match found.entry(tv.cell_addr()) {
                    Entry::Occupied(mut e) => *e.get_mut() &= waits,
                    Entry::Vacant(e) => {
                        e.insert(waits);
                        cells.push(tv.clone())
                    }
                }
            })
        });
        cells.retain(|tv| found[&tv.cell_addr()]);
        cells
    }

    /// Rigid gates on a scheme's quantifiers no call has picked: what each
    /// meets must hold for every type its bound admits.
    fn open_quantifier_gates(&self) -> LPooled<Vec<super::tvar::RigidGate>> {
        let mut gates: LPooled<Vec<super::tvar::RigidGate>> = LPooled::take();
        self.open_quantifiers(|tv, open| {
            if open.level().is_generic() {
                gates.push(tv.open_rigid())
            }
        });
        gates
    }

    /// Every declared bound holds for its bound variable. An open one
    /// (its cell open at the end of any forward link) stays open: it
    /// stands for one type the bound admits, and its cell enforces the
    /// bound at every binding.
    fn bounds_hold(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut ContainsHist,
    ) -> Result<bool> {
        for (tv, tc) in self.constraint_view().iter() {
            if tv.open_cell().is_none()
                && !tc.contains_int(flags, env, hist, &Type::TVar(tv.clone()))?
            {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub fn check_contains(&self, env: &Env, other: &Self) -> Result<()> {
        if !self.contains(env, other)? {
            bail!("Fn type mismatch {self} does not contain {other}")
        }
        Ok(())
    }

    /// The parameter positions of [`Self::contains`] alone: pushes a
    /// declared signature's parameter types into an argument function
    /// before its body is typechecked. Return and throws are what the
    /// body determines and are left open.
    pub fn pre_unify_params(&self, env: &Env, t: &Self) -> Result<()> {
        let mut hist = ContainsHist::new();
        let cells = t.order_free_cells(env, &mut hist);
        let mark = hist.defer(cells);
        let held = self.align(t).0.into_iter().try_for_each(|(s, t)| {
            let flags = ContainsFlags::Commit.into();
            t.typ.contains_int(flags, env, &mut hist, &s.typ).map(drop)
        });
        hist.settle_deferred(mark, env, held.map(|()| true)).map(drop)
    }

    pub fn sig_matches(&self, env: &Env, impl_fn: &Self) -> Result<SigMatch> {
        let (mut pairs, mut hist) = (SigPairs::default(), super::RefHist::new());
        self.sig_matches_int(env, impl_fn, &mut pairs, &mut hist)?;
        pairs.finished(env, &mut hist)
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_fn: &Self,
        tvar_map: &mut SigPairs,
        hist: &mut super::RefHist<AHashSet<super::RefPair>>,
    ) -> Result<()> {
        let Self {
            args: sig_args,
            vargs: sig_vargs,
            rtype: sig_rtype,
            throws: sig_throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = self;
        let Self {
            args: impl_args,
            vargs: impl_vargs,
            rtype: impl_rtype,
            throws: impl_throws,
            explicit_throws: _,
            quantifiers: _,
            lambda_ids: _,
        } = impl_fn;
        if sig_args.len() != impl_args.len() {
            bail!(
                "argument count mismatch: signature has {}, implementation has {}",
                sig_args.len(),
                impl_args.len()
            );
        }
        // labeled arguments pair by label, whatever order each side writes
        let (pairs, aligned) = self.align(impl_fn);
        if !aligned {
            bail!("the signature's and the implementation's labeled arguments differ")
        }
        let args = (|| -> Result<()> {
            let first_positional = self.first_positional();
            for (i, (sig_arg, impl_arg)) in pairs.into_iter().enumerate() {
                use FnArgKind::Labeled;
                if let (
                    Labeled { name, has_default: d0 },
                    Labeled { has_default: d1, .. },
                ) = (&sig_arg.kind, &impl_arg.kind)
                    && d0 != d1
                {
                    let (s, i) = if *d0 { ("a", "no") } else { ("no", "a") };
                    bail!(
                        "argument #{name}: the signature gives it {s} default, the \
                         implementation {i} default"
                    )
                }
                sig_arg
                    .typ
                    .sig_matches_int(env, &impl_arg.typ, tvar_map, hist)
                    .with_context(|| match sig_arg.kind.label() {
                        Some(l) => format!("in argument #{l}"),
                        None => format!("in argument {}", i - first_positional),
                    })?;
            }
            match (sig_vargs, impl_vargs) {
                (None, None) => (),
                (Some(sig_va), Some(impl_va)) => {
                    sig_va
                        .sig_matches_int(env, impl_va, tvar_map, hist)
                        .context("in variadic argument")?;
                }
                (None, Some(_)) => {
                    bail!("signature has no variadic args but implementation does")
                }
                (Some(_), None) => {
                    bail!("signature has variadic args but implementation does not")
                }
            }
            Ok(())
        })();
        args?;
        sig_rtype
            .sig_matches_int(env, impl_rtype, tvar_map, hist)
            .context("in return type")?;
        let nothing = |t: &Type| matches!(t.deref_cloned(), Some(Type::Bottom));
        sig_throws
            .sig_matches_int(env, impl_throws, tvar_map, hist)
            .map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    match (nothing(sig_throws), nothing(impl_throws)) {
                        (true, false) => anyhow::anyhow!(
                            "the signature throws nothing, the implementation throws \
                             {impl_throws}"
                        ),
                        (false, true) => anyhow::anyhow!(
                            "the signature throws {sig_throws}, the implementation \
                             throws nothing"
                        ),
                        _ => e,
                    }
                })
            })
            .context("in throws clause")?;
        tvar_map.fns.push((self.clone(), impl_fn.clone()));
        Ok(())
    }

    /// The bounds of a signature against its implementation's, once the
    /// whole signature has paired its variables ([`SigPairs::finish`]).
    pub(super) fn sig_bounds(
        &self,
        env: &Env,
        impl_fn: &Self,
        tvar_map: &mut SigPairs,
        hist: &mut super::RefHist<AHashSet<super::RefPair>>,
    ) -> Result<()> {
        // every cell the walk may have mapped: an unannotated parameter's
        // is bound, and the cells its binding holds carry the conjuncts
        let impl_cells = {
            let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            impl_fn
                .for_each_part(&mut |t, _| super::settle::position_cells(t, &mut cells));
            // every cell, a variable named only inside the bound of a bound
            // too, to any depth
            let mut todo: LPooled<Vec<TVar>> = cells.values().cloned().collect();
            let mut found: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            while let Some(tv) = todo.pop() {
                for tc in tv.cell_constraints() {
                    super::settle::position_cells(&tc, &mut found);
                }
                for (addr, tv) in found.drain() {
                    if !cells.contains_key(&addr) {
                        cells.insert(addr, tv.clone());
                        todo.push(tv);
                    }
                }
            }
            sorted_tvars(cells.drain().map(|(_, tv)| (tv.name.clone(), tv)))
        };
        // An implementation may be more general than its signature: a bound
        // the signature declares need not be the implementation's. One the
        // implementation does carry pairs the variables inside it with the
        // signature's, as positions do.
        // each signature cell's paired impl cells, by their place in
        // `impl_cells`, as the pairing grows
        let place: LPooled<AHashMap<usize, usize>> = impl_cells
            .iter()
            .enumerate()
            .map(|(i, (_, tv))| (tv.cell_addr(), i))
            .collect();
        let mut paired_to: LPooled<AHashMap<usize, SmallVec<[usize; 1]>>> =
            LPooled::take();
        let mut indexed = 0;
        // the impl cells the signature's variable is paired with
        let mut matched = |tvar_map: &SigPairs, sig_tv: &TVar| {
            for (addr, t) in tvar_map.paired_since(indexed) {
                if let Type::TVar(s) = t
                    && let Some(i) = place.get(&addr)
                {
                    paired_to.entry(s.cell_addr()).or_default().push(*i);
                }
            }
            indexed = tvar_map.paired_len();
            match paired_to.get_mut(&sig_tv.cell_addr()) {
                None => LPooled::<Vec<&TVar>>::take(),
                Some(places) => {
                    places.sort_unstable();
                    places.iter().map(|i| &impl_cells[*i].1).collect()
                }
            }
        };
        let view = self.constraint_view();
        let bounds = || view.iter().enumerate().filter(|(_, (_, tc))| *tc != Type::Any);
        // each (bound, impl cell) pairing tried: a pairing only grows, so
        // one that failed fails again
        let mut tried: LPooled<AHashSet<(usize, usize)>> = LPooled::take();
        // each impl conjunct, by its cell and place, a bound has paired
        // with, and each (bound, impl conjunct) tried after the first
        let mut met: LPooled<AHashSet<(usize, usize)>> = LPooled::take();
        let mut tried_rest: LPooled<AHashSet<(usize, usize, usize)>> = LPooled::take();
        // passes until one pairs nothing: a variable met only in a bound
        // later in the view, or in a union the settle pairs, has its own
        // bound paired by the next
        loop {
            let start = tvar_map.paired_len();
            for (k, (sig_tv, sig_tc)) in bounds() {
                for impl_tv in matched(tvar_map, sig_tv).iter() {
                    let addr = impl_tv.cell_addr();
                    if !tried.insert((k, addr)) {
                        continue;
                    }
                    for (j, c) in impl_tv.cell_constraints().iter().enumerate() {
                        if tvar_map
                            .trial(|m| sig_tc.sig_matches_int(env, c, m, hist))
                            .is_ok()
                        {
                            met.insert((addr, j));
                            break;
                        }
                    }
                }
            }
            tvar_map.settle(env, hist)?;
            // a cell may carry several conjuncts of one shape (`'q:
            // Array<'p> + Array<'r>`, a method's own beside its trait's):
            // each that no bound paired with pairs with a bound that takes
            // it whole, its unions too
            for (k, (sig_tv, sig_tc)) in bounds() {
                for impl_tv in matched(tvar_map, sig_tv).iter() {
                    let addr = impl_tv.cell_addr();
                    for (j, c) in impl_tv.cell_constraints().iter().enumerate() {
                        if met.contains(&(addr, j)) || !tried_rest.insert((k, addr, j)) {
                            continue;
                        }
                        let whole = tvar_map.trial(|m| {
                            sig_tc.sig_matches_int(env, c, m, hist)?;
                            m.settle(env, hist)
                        });
                        if whole.is_ok() {
                            met.insert((addr, j));
                        }
                    }
                }
            }
            if tvar_map.paired_len() == start {
                break;
            }
        }
        let mut param_bounds = LPooled::take();
        self.for_each_part(&mut |t, _| param_bounds_of(env, t, &mut param_bounds));
        // A call through the signature is checked by the signature alone:
        // every conjunct of every impl cell, in the signature's terms, must
        // admit the signature's concrete choice, or follow from the
        // conjuncts of the signature variable it stands for. The
        // signature's variables are every caller's to pick: they fit only
        // through their own conjuncts.
        let mut sig_rigid: LPooled<Vec<RigidGate>> = LPooled::take();
        let mut held = LPooled::take();
        self.for_each_part(&mut |t, _| {
            super::contains::hold_rigid_through_bounds(t, &mut held, &mut sig_rigid)
        });
        // each implementation cell in the signature's terms: a paired one
        // is the signature's type at its place, another open one a copy
        // whose own conjuncts are in those terms
        let mut image: LPooled<AHashMap<usize, Type>> =
            tvar_map.paired_since(0).map(|(addr, t)| (addr, t.clone())).collect();
        let copies: LPooled<Vec<(&TVar, TVar)>> = impl_cells
            .iter()
            .filter(|(_, tv)| {
                !tv.is_bound() && !tv.is_rigid() && !image.contains_key(&tv.cell_addr())
            })
            .map(|(_, tv)| (tv, tv.fresh_copy()))
            .collect();
        for (tv, copy) in copies.iter() {
            image.insert(tv.cell_addr(), Type::TVar(copy.clone()));
        }
        let in_sig = |t: &Type| t.swap_cells(&|addr| image.get(&addr).cloned());
        for (tv, copy) in copies.iter() {
            tv.copy_bounds_into(copy, in_sig);
        }
        // the signature declares `impl_tc` of `sig_tv`: one of its bounds,
        // or a bound of the typedef parameter it fills, implies it
        let declared = |sig_tv: &TVar, impl_tc: &Type| -> Result<()> {
            if *impl_tc == Type::Any {
                return Ok(());
            }
            let mut sig_tcs = sig_tv.cell_constraints();
            if let Some(bounds) = param_bounds.get(&sig_tv.cell_addr()) {
                sig_tcs.extend(bounds.iter().cloned());
            }
            let in_sig = in_sig(impl_tc);
            // CR claude for claude: [bug/medium] An implementation bound of another shape
            // than the signature's pairs nothing, and this asks `implies` of a conjunct
            // that still holds an open implementation cell, which a probe lets stand for
            // a signature variable whatever its own bound says (Type::contains_tvars's
            // open pair holds a flexible cell's lower bounds of a rigid one, never its
            // conjuncts). `val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) -> 'b;` over
            // `let f = 'p: [i64, f64], 'q: ['p, `X, `Y] |x: 'q| -> 'q x` passes --check
            // with `m::f("s")` in main, and the run fails "an instance at fn(x: string)
            // -> string of a definition typed fn<'p: [i64, f64], ..". The same with `'b:
            // Array<'a>` over `'q: [Array<'p>, null]`, `'b: (i64, 'a)` over `'q: [(i64,
            // 'p), null]`, and with 'a at a position (`(x: Array<'a>) -> Array<'a>` over
            // `'q: [Array<'p>, null] |x: 'q|`). `'p: [i64, string]` in each is valid and
            // passes, as it must, and so does `'b: [i64, null]` over `'p: Number, 'q:
            // ['p, null]`: refusing every bounded open cell left in `in_sig` would refuse
            // those. Either the open pair holds the flexible cell's conjuncts of the
            // rigid one under RigidCheck, or this judges the cells `in_sig` leaves open
            // itself. The other way round, a union bound of the signature's length that
            // cannot pair is a hard refusal of a valid implementation: `'b: [`A(i64),
            // 'a]` over `'p: [i64, string], 'q: [`A(Number), 'p]` ("signature has i64,
            // implementation has [Int, Real]"). On base-r20 and lane 20d alike. probes:
            // ~/ptmp/cr29/scratch/r20/20d/ibo-rv/{f1,f2,g1,g3,g6,k4}/main.gx
            // (iface-bound-shape-01)
            // 2026-11-12 claude: Review of iface-same-name-01: the same open pair lets
            // the check accept through the Some(sig_type) arm of FnType::sig_bounds
            // (impl_tc.contains_with_flags under RigidCheck), not only through declared.
            // `val f: fn<'b: Array<'z>, 'z: Number>(x: Array<'b>) -> i64;` over `let f =
            // 'c: Array<'q>, 'q: Number |x: 'c| -> i64 0` passes --check with
            // `m::f([[1]])` in main, and the run fails "an instance at fn(x:
            // Array<Array<i64>>) -> i64 of a definition typed ..": the implementation
            // cell 'c stands for Array<'b>, and its flexible 'q (Number) takes the rigid
            // 'b (Array<'z>) without its conjuncts. The same with `'q: Array<'r>, 'r:
            // i64` called at `[[1.0]]`. Until iface-same-name-01 deleted the same-name
            // arm, the check refused that second program, by chance, when the
            // implementation spelled its variable 'b ("requires 'z: Array<'r>"); a fix
            // here pins it under both spellings. On base-r23 and lane 23g alike. probes:
            // ~/ptmp/cr29/scratch/r23/23g/iface-same-name-01/review/{r1,r2,r3,r4}/main.gx
            // 2026-11-12 claude: Partly fixed. The open pair: under the new
            // ContainsFlags::Sealed (Type::implies and FnType::sig_bounds' Some(sig_type)
            // arm) an open cell merges into a rigid one only where its own conjuncts
            // admit it, since a fixed signature's variable takes on no conjunct; the
            // signature is held rigid through its bounds to any depth
            // (contains::hold_rigid_through_bounds, in both judgments), and every
            // implementation conjunct is read in the signature's terms, a paired cell as
            // its signature type and another open one as a copy whose conjuncts are read
            // so (TVar::copy_bounds_into), in the Some arm too, which read the impl's own
            // cells as free. f1 g1 g3 g6 and the review's r1-r4 are refused at the check,
            // f2 and the valid twins hold; also refused now: a paired impl variable
            // inside an impl bound (s1, s3) and a signature variable three bounds deep
            // (t1, t2). Pins in lang::interfaces (an_unpaired_variable_*,
            // a_variable_inside_a_positions_bound_*,
            // a_variable_three_bounds_inside_a_position_*,
            // a_variable_inside_a_bounds_variables_bound_*, a_paired_variable_*): each
            // refusal pin fails with its part undone. Census 0/0 vs base-r23. Left: the
            // second half (k4, a bound union that cannot pair member by member, still a
            // hard refusal). Making that union pair nothing, as a bound of another shape
            // does, is small (the bounds pass's SigPairs::settle dropping a group that
            // cannot pair, which SigPairs::search leaves unpaired) but newly accepts an
            // unsound program (u1) through sig-unpaired-apart-01's hole, which base
            // reaches already by other shapes (u2): fix that first. probes:
            // ~/ptmp/cr29/scratch/r23/23g/iface-bound-shape-01/ (README)
            // 2026-11-12 claude: Review (lane 23g): the partial fix holds for the shapes
            // it names (f1 g1 g3 g6, r1-r4, s1 s3, t1 t2 refused at the check; f2 s2 t3
            // t4 u3 hold), and each part is pinned: with the Sealed merge undone 10
            // refusal pins fail, with holding through bounds undone 2, with the Some
            // arm's signature terms undone 2. It left a predicate conjunct on the
            // unpaired cell: a predicate read a rigid signature variable as it stands,
            // one member of no kind, so `'p: Singleton` (or NoError, OneNumber) stood for
            // `'a: [i64, string]`: `val f: fn<'a: [i64, string], 'b: ['a, `X]>(x: 'b) ->
            // 'b;` over `let f = 'p: Singleton, 'q: ['p, `X, `Y] |x: 'q| -> 'q x` passed
            // --check and the run failed at the instance with a `[i64, string]` argument.
            // Corrected: under Sealed a rigid cell holds a predicate only where one of
            // its conjuncts implies it (Type::contains_dispatch, by Type::implies), as a
            // paired cell's predicate is judged; pins
            // an_unpaired_variable_with_a_{singleton,no_error,one_number}_bound_* and
            // an_unpaired_variable_with_a_singleton_bound_against_a_position_* (each
            // refusal fails in all 4 modes without the correction, each twin holds both
            // ways). A paired cell's predicate against a concrete signature type is a new
            // CR, iface-predicate-member-01. Still left: k4, as noted. probes:
            // ~/ptmp/cr29/scratch/r23/23g/iface-bound-shape-01/review/ (README)
            for s in sig_tcs.iter() {
                if s.implies(env, &in_sig)? {
                    return Ok(());
                }
            }
            bail!(
                "the implementation requires {sig_tv}: {impl_tc}, which the signature \
                 does not declare"
            )
        };
        // CR claude for claude: [bug/medium] An implementation variable no position or
        // bound pairs is judged apart in each conjunct that names it, each judgment a
        // probe that lets it stand for another signature variable, and a bound that
        // holds no union of two cannot be both: `val f: fn<'a: [i64, string] +
        // Singleton, 'c: [i64, string] + Singleton, 'b: ['a, `X], 'd: ['c, `X]>(x: 'b, y:
        // 'd) -> i64;` over `let f = 'p: [i64, string] + Singleton, 'q: ['p, `X, `Y], 'r:
        // ['p, `X, `Y] |x: 'q, y: 'r| -> i64 0` passes --check with `m::f("s", 1)`, and
        // the run fails "an instance at fn(x: string, y: i64) -> i64 of a definition
        // typed ..". Without `+ Singleton` it is valid ('p is [i64, string]) and runs,
        // so committing the first judgment's choice would refuse a valid program: the
        // copy of 'p would take each signature variable it meets as a lower bound, and
        // its conjuncts judge their union. This hole holds iface-bound-shape-01's second
        // half: a bound union that cannot pair member by member is a hard refusal
        // (SigPairs::settle in the bounds pass), and pairing nothing there instead
        // reaches this hole (`'b: [`A(i64), 'a], 'd: [`A(i64), 'c]` over `'q: [`A(Number),
        // 'p], 'r: [`A(Number), 'p]`, the same Singletons, is refused on base only by
        // that hard refusal). On base-r23 and lane 23g alike. probes:
        // ~/ptmp/cr29/scratch/r23/23g/iface-bound-shape-01/{u1,u2,u3,k4}/main.gx (README)
        // (sig-unpaired-apart-01)
        // 2026-11-12 claude: Review of iface-bound-shape-01: the variable is judged apart
        // inside one conjunct too, since a probe merges nothing: `val f: fn<'a: [i64,
        // string] + Singleton, 'c: [i64, string] + Singleton, 'b: ('a, 'c)>(x: 'b) ->
        // i64;` over `let f = 'p: [i64, string] + Singleton, 'q: [('p, 'p), null] |x: 'q|
        // -> i64 0` passes --check with `m::f(("s", 1))` and the run fails at the
        // instance; so does `val f: fn(x: (i64, string)) -> i64;` over the same
        // implementation with `m::f((1, "s"))`, the variable meeting two concrete types.
        // On base-r23 and lane 23g alike. probes:
        // ~/ptmp/cr29/scratch/r23/23g/iface-bound-shape-01/review/{p1,p2}/main.gx
        for (_, tv) in impl_cells.iter() {
            match tvar_map.sig_type(tv.cell_addr()) {
                None => (),
                Some(Type::TVar(sig_tv)) => {
                    for impl_tc in tv.cell_constraints() {
                        declared(sig_tv, &impl_tc)?
                    }
                }
                Some(sig_type) => {
                    // CR claude for claude: [bug/medium] A paired implementation cell's
                    // predicate conjunct is asked of the signature's concrete type as that type
                    // stands, where a signature variable that is a member of its union does not
                    // count as the types its bounds admit, though the caller may pick any of
                    // them; the predicates asked of each signature variable inside (the list
                    // below: Discernible, Ordered, JsonData, TomlData, Concrete, Data) leave out
                    // NoError and OneNumber. `val f: fn<'a: Number>(x: ['a, `X]) -> i64;` over
                    // `let f = 'q: OneNumber |x: 'q| -> i64 0` passes --check with `m::f(v)`, `v:
                    // [i64, f64, `X]`, and the run fails "an instance at fn(x: [i64, f64, `X])
                    // -> i64 of a definition typed fn<'q: OneNumber>(..)"; the same with `'a:
                    // [i64, Error<`E>]` over `'q: NoError` and an error argument. Asking NoError
                    // of each signature variable that is a member of the union (not one below a
                    // constructor: an `Array<Error<..>>` is no error) closes the second; OneNumber
                    // and Singleton count members, so a variable there would count as its bounds'
                    // members. The open pair's case, a predicate on an unpaired cell standing
                    // for a bare signature variable, is judged by the variable's bounds (the
                    // Sealed arm of Type::contains_dispatch). On base-r23 and lane 23g alike.
                    // probes: ~/ptmp/cr29/scratch/r23/23g/iface-bound-shape-01/review/{p9,p9n}/main.gx
                    // (iface-predicate-member-01)

                    // a predicate that holds of a type only if it holds of
                    // every part asks it of each signature variable inside
                    for impl_tc in tv.cell_constraints() {
                        if matches!(
                            impl_tc,
                            Type::Bound(Bound::Discernible)
                                | Type::Bound(Bound::Ordered)
                                | Type::Bound(Bound::JsonData)
                                | Type::Bound(Bound::TomlData)
                                | Type::Bound(Bound::Concrete)
                                | Type::Bound(Bound::Data)
                        ) {
                            let mut occurrences = super::tvar::Occurrences::default();
                            sig_type.tvar_occurrences(&mut occurrences);
                            let vars = occurrences.vars.iter();
                            for v in vars.filter(|v| v.open_cell().is_some()) {
                                declared(v, &impl_tc)?
                            }
                        }
                    }
                    for impl_tc in tv.cell_constraints() {
                        let ok = in_sig(&impl_tc)
                            .contains_with_flags(
                                ContainsFlags::RigidCheck | ContainsFlags::Sealed,
                                env,
                                sig_type,
                            )
                            .unwrap_or(false);
                        if !ok {
                            bail!(
                                "signature has concrete type {sig_type}, which the \
                                 implementation constraint {impl_tc} does not admit"
                            )
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        let mut copies: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.cow_walk(|t| t.scope_refs_int(scope, &mut copies))
            .unwrap_or_else(|| self.clone())
    }
}

impl FnType {
    /// Suppress the `throws` clause when printing: only for the
    /// implicit no-throw shapes (inferred `Bottom`, a TVar bound to
    /// `Bottom`, an unbound auto cell), never for an explicit clause.
    fn suppress_throws(&self) -> bool {
        !self.explicit_throws
            && match &self.throws {
                Type::Bottom => true,
                Type::TVar(tv) => match tv.binding() {
                    Some(Type::Bottom) => true,
                    None => tv.name.starts_with('_'),
                    Some(_) => false,
                },
                _ => false,
            }
    }
}

/// Is this positional parameter a trait method's receiver (`self`
/// typed by the `self` variable or its application)? It prints as its
/// type alone.
fn is_self_param(a: &FnArgType) -> bool {
    let is_self = |t: &Type| matches!(t, Type::TVar(tv) if &*tv.name == "self");
    matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self")
        && match &a.typ {
            Type::App(c, _) => is_self(c),
            t => is_self(t),
        }
}

/// The quantifiers a signature prints: the declared ones, minus the
/// receiver `self` (implied by its trait) and the compiler-minted
/// `#arg` quantifiers of traits written in argument position (those
/// print as the trait at the argument). Where cells print as they stand
/// ([`crate::print_standing`]) a quantifier no call picks any more, bound
/// or one instance's, is left out too: it prints where it occurs, as an
/// inferred variable does. One entry per conjunct, a variable's conjuncts
/// adjacent.
fn printed_quantifiers(ft: &FnType) -> LPooled<Vec<(TVar, Type)>> {
    let mut v = ft.printed_view();
    let as_they_stand = crate::print_standing();
    v.retain(|(tv, _)| {
        !tv.unwritten() && (!as_they_stand || tv.heads_a_printed_signature())
    });
    v
}

/// How an argument's name is written before its type.
struct ArgPrefix<'a>(&'a FnArgKind);

impl fmt::Display for ArgPrefix<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FnArgKind::Labeled { name, has_default: true } => write!(f, "?#{name}: "),
            FnArgKind::Labeled { name, has_default: false } => write!(f, "#{name}: "),
            FnArgKind::Positional { name: Some(n) } => write!(f, "{n}: "),
            FnArgKind::Positional { name: None } => Ok(()),
        }
    }
}

/// Does `t` print as a function type, a variable printing its binding
/// included?
fn prints_as_fn(t: &Type) -> bool {
    match t {
        Type::Fn(_) => true,
        Type::ByRef(_, t) => prints_as_fn(t),
        Type::TVar(tv) if PRINT_FLAGS.get().contains(PrintFlag::DerefTVars) => {
            tv.binding().is_some_and(|t| prints_as_fn(&t))
        }
        _ => false,
    }
}

/// A return type as written after `->`: a function type (or a variable
/// printing one), bare or behind a reference, is parenthesized where a `throws` follows, which would
/// otherwise read as its own.
pub(crate) struct Ret<'a> {
    pub(crate) open: &'static str,
    pub(crate) typ: &'a dyn PrettyDisplay,
    pub(crate) close: &'static str,
}

impl<'a> Ret<'a> {
    pub(crate) fn of(t: &'a Type, throws_follows: bool) -> Self {
        let (by_ref, typ) = match t {
            Type::ByRef(m, t) => (Some(*m), &**t),
            t => (None, t),
        };
        let (open, close) = match (by_ref, throws_follows && prints_as_fn(typ)) {
            (None, false) => ("", ""),
            (None, true) => ("(", ")"),
            (Some(m), false) => (m.prefix(), ""),
            (Some(Mutability::Shared), true) => ("&(", ")"),
            (Some(Mutability::Mut), true) => ("&mut (", ")"),
        };
        Ret { open, typ, close }
    }

    /// Laid out over lines, a newline after.
    pub(crate) fn fmt_pretty(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write!(buf, "{}", self.open)?;
        self.typ.fmt_pretty(buf)?;
        if !self.close.is_empty() {
            buf.kill_newline();
            writeln!(buf, "{}", self.close)?;
        }
        Ok(())
    }
}

impl fmt::Display for Ret<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.open, self.typ, self.close)
    }
}

/// Bounds as written: a variable's adjacent bounds joined with ` + `.
pub(crate) fn write_bounds(
    f: &mut impl fmt::Write,
    bounds: &[(TVar, Type)],
) -> fmt::Result {
    for (i, (tv, t)) in bounds.iter().enumerate() {
        match i.checked_sub(1).map(|j| &bounds[j].0) {
            Some(prev) if prev.name == tv.name => write!(f, " + {t}")?,
            Some(_) => write!(f, ", '{}: {t}", tvar::printed_name(tv))?,
            None => write!(f, "'{}: {t}", tvar::printed_name(tv))?,
        }
    }
    Ok(())
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        tvar::quantifiers_apart(self, || self.write_flat(f))
    }
}

impl FnType {
    fn write_flat(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _scope = PartsDepth::enter();
        let constraints = printed_quantifiers(self);
        if constraints.len() == 0 {
            write!(f, "fn(")?;
        } else {
            write!(f, "fn<")?;
            write_bounds(f, &constraints)?;
            write!(f, ">(")?;
        }
        for (i, a) in self.args.iter().enumerate() {
            if is_self_param(a) {
                write!(f, "{}", a.typ)?;
            } else {
                write!(f, "{}{}", ArgPrefix(&a.kind), a.typ)?;
            }
            if i < self.args.len() - 1 || self.vargs.is_some() {
                write!(f, ", ")?;
            }
        }
        if let Some(vargs) = &self.vargs {
            write!(f, "@args: {}", vargs)?;
        }
        write!(f, ") -> {}", Ret::of(&self.rtype, !self.suppress_throws()))?;
        if self.suppress_throws() {
            Ok(())
        } else {
            write!(f, " throws {}", &self.throws)
        }
    }
}

impl PrettyDisplay for FnType {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        tvar::quantifiers_apart(self, || self.write_pretty(buf))
    }
}

impl FnType {
    fn write_pretty(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let _scope = PartsDepth::enter();
        let constraints = printed_quantifiers(self);
        if constraints.is_empty() {
            write!(buf, "fn(")?;
        } else {
            writeln!(buf, "fn<")?;
            buf.nested(|buf| {
                for (i, (tv, t)) in constraints.iter().enumerate() {
                    match i.checked_sub(1).map(|j| &constraints[j].0) {
                        Some(prev) if prev.name == tv.name => {
                            buf.kill_newline();
                            write!(buf, " + ")?;
                        }
                        Some(_) => {
                            buf.kill_newline();
                            writeln!(buf, ",")?;
                            write!(buf, "'{}: ", tvar::printed_name(tv))?;
                        }
                        None => write!(buf, "'{}: ", tvar::printed_name(tv))?,
                    }
                    buf.nested(|buf| t.fmt_pretty(buf))?;
                }
                Ok(())
            })?;
            write!(buf, ">(")?;
        }
        // with no arguments the return type follows `fn(`'s `)` on its line
        if self.args.is_empty() && self.vargs.is_none() {
            write!(buf, ") -> ")?;
            Ret::of(&self.rtype, !self.suppress_throws()).fmt_pretty(buf)?;
            return self.pretty_throws(buf);
        }
        writeln!(buf)?;
        buf.nested(|buf| {
            for (i, a) in self.args.iter().enumerate() {
                if is_self_param(a) {
                    writeln!(buf, "{}", a.typ)?;
                } else {
                    write!(buf, "{}", ArgPrefix(&a.kind))?;
                    a.typ.fmt_pretty(buf)?;
                }
                if i < self.args.len() - 1 || self.vargs.is_some() {
                    buf.kill_newline();
                    writeln!(buf, ",")?;
                }
            }
            if let Some(vargs) = &self.vargs {
                write!(buf, "@args: ")?;
                vargs.fmt_pretty(buf)?;
            }
            Ok(())
        })?;
        write!(buf, ") -> ")?;
        Ret::of(&self.rtype, !self.suppress_throws()).fmt_pretty(buf)?;
        self.pretty_throws(buf)
    }
}

impl FnType {
    fn pretty_throws(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if self.suppress_throws() {
            Ok(())
        } else {
            buf.kill_newline();
            write!(buf, " throws ")?;
            self.throws.fmt_pretty(buf)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::parser::parse_fn_type;
    use poolshark::local::LPooled;

    /// A walk that inlines a parameter's binding holding a cell named like
    /// the signature's quantifier keeps the quantifier the signature's:
    /// renamed apart where it stands in a position, the namesake renamed
    /// where it is held through a bound alone.
    #[test]
    fn an_inlined_binding_captures_no_quantifier() {
        use crate::typ::{TVar, Type};
        use ahash::AHashMap;
        let quantifier = |ft: &super::FnType, name: &str| {
            let mut out = None;
            ft.each_quantifier(|q, tv| {
                if &**q == name {
                    out = Some(tv.clone())
                }
            });
            out.unwrap()
        };
        let cell = |t: &Type| match t {
            Type::TVar(tv) => tv.cell_addr(),
            t => panic!("{t}"),
        };
        let element = |ft: &super::FnType| match &ft.args[0].typ {
            Type::Array(e) => cell(e),
            t => panic!("{t}"),
        };
        let walks = |ft: &super::FnType| {
            let p = match &ft.args[0].typ {
                Type::TVar(p) => p.clone(),
                t => panic!("{t}"),
            };
            p.bind(Type::Array(triomphe::Arc::new(Type::TVar(TVar::empty_named(
                "a".into(),
            )))));
            [
                ("instantiate_owned", ft.instantiate_owned(&mut AHashMap::default())),
                ("rename_with", ft.rename_with(&AHashMap::default())),
                ("resolve_tvars", ft.resolve_tvars()),
            ]
        };
        let ft = parse_fn_type("fn<'a: Number>(y: 'p, x: 'a) -> 'a").unwrap();
        for (walk, copy) in walks(&ft) {
            let q = quantifier(&copy, &copy.quantifiers[0]);
            assert_eq!(q.cell_addr(), cell(&copy.args[1].typ), "{walk}: {copy}");
            assert!(!q.cell_constraints().is_empty(), "{walk}: {copy}");
        }
        let ft =
            parse_fn_type("fn<'a: Any, 'b: ['a, null]>(y: 'p, x: 'b) -> 'b").unwrap();
        for (walk, copy) in walks(&ft) {
            assert_ne!(
                quantifier(&copy, "a").cell_addr(),
                element(&copy),
                "{walk}: {copy}"
            );
        }
    }

    /// The refusal names the first variable declared whose bound reaches
    /// it, by the shortest path back its bounds' names lead to first:
    /// what a breadth-first walk from each variable in turn finds.
    #[test]
    fn a_bound_cycle_is_the_first_a_walk_from_each_variable_finds() {
        use super::check_bounds_acyclic;
        use crate::typ::{TVar, Type};
        use std::collections::VecDeque;
        let name = |v: usize| format!("v{v}");
        // (variable, the variables its bound names), as declared
        let first_cycle = |decls: &[(usize, Vec<usize>)]| {
            let named_by = |v: usize| {
                let mut out = vec![];
                for (_, to) in decls.iter().filter(|(f, _)| *f == v) {
                    let mut to: Vec<usize> = to
                        .iter()
                        .copied()
                        .filter(|t| decls.iter().any(|(d, _)| d == t))
                        .collect();
                    to.sort_by_key(|t| name(*t));
                    to.dedup();
                    out.extend(to)
                }
                out
            };
            for (q, _) in decls {
                let mut seen = vec![];
                let mut todo: VecDeque<(usize, usize)> =
                    named_by(*q).into_iter().map(|n| (n, n)).collect();
                while let Some((n, via)) = todo.pop_front() {
                    if n == *q {
                        return Some((n, via));
                    }
                    if !seen.contains(&n) {
                        seen.push(n);
                        todo.extend(named_by(n).into_iter().map(|m| (m, via)));
                    }
                }
            }
            None
        };
        let mut seed = 0x9e37_79b9_7f4a_7c15u64;
        let mut rand = |n: usize| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed % n as u64) as usize
        };
        // the cases met: no cycle, a bound naming its variable, a cycle
        // through another
        let mut met = [0; 3];
        for _ in 0..4000 {
            let vars = 1 + rand(12);
            let decls: Vec<(usize, Vec<usize>)> = (0..1 + rand(14))
                .map(|_| (rand(vars), (0..rand(4)).map(|_| rand(vars + 2)).collect()))
                .collect();
            let constraints: Vec<(TVar, Type)> = decls
                .iter()
                .map(|(v, to)| {
                    let tv = |v: usize| TVar::empty_named(name(v).into());
                    let parts = to.iter().map(|t| Type::TVar(tv(*t)));
                    (tv(*v), Type::Tuple(parts.chain([Type::Bottom]).collect()))
                })
                .collect();
            let got = check_bounds_acyclic(&constraints).map_err(|e| e.to_string());
            match first_cycle(&decls) {
                None => {
                    met[0] += 1;
                    assert!(got.is_ok(), "{decls:?}: {got:?}")
                }
                Some((q, via)) => {
                    met[1 + (q != via) as usize] += 1;
                    let (q, via) = (name(q), name(via));
                    let want = match q == via {
                        true => format!("'{q}'s bound names '{q}:"),
                        false => format!("'{q}'s bound reaches '{q} through '{via}:"),
                    };
                    let got = got.unwrap_err();
                    assert!(got.starts_with(&want), "{decls:?}: {got}, want {want}");
                }
            }
        }
        assert!(met.iter().all(|n| *n > 400), "{met:?}");
    }

    /// `n` signatures, each but `leaf` holding the next as `wrap` writes it.
    fn nested(n: usize, leaf: &str, wrap: impl Fn(&str) -> String) -> String {
        (1..n).fold(String::from(leaf), |s, _| wrap(&s))
    }

    fn nested_quantified(n: usize) -> String {
        nested(n, "fn<'a: Any>(x: i64) -> 'a", |s| format!("fn<'a: Any>(x: {s}) -> 'a"))
    }

    /// Each signature the bound of the next one's variable.
    fn nested_by_bound(n: usize) -> String {
        nested(n, "fn<'a: Any>(x: 'a) -> 'a", |s| format!("fn<'a: {s}>(x: 'a) -> 'a"))
    }

    fn nested_unquantified(n: usize) -> String {
        nested(n, "fn(x: i64) -> i64", |s| format!("fn(x: {s}) -> i64"))
    }

    /// Every signature names the outermost's `'z` beside its own variable.
    fn nested_naming_outer(n: usize) -> String {
        let inner = nested(n, "fn<'a: Any>(x: 'z) -> 'a", |s| {
            format!("fn<'a: Any>(x: {s}, y: 'z) -> 'a")
        });
        format!("fn<'z: Any>(x: {inner}) -> 'z")
    }

    /// Each signature the bound of the next one's variable, every one
    /// naming the outermost's `'z`.
    fn nested_by_bound_naming_outer(n: usize) -> String {
        let inner = nested(n, "fn<'a: Any>(x: 'a, y: 'z) -> 'a", |s| {
            format!("fn<'a: {s}>(x: 'a, y: 'z) -> 'a")
        });
        format!("fn<'z: Any>(x: {inner}) -> 'z")
    }

    /// No signature declares a variable; every one names `'a`.
    fn nested_naming_a_variable(n: usize) -> String {
        nested(n, "fn(x: 'a) -> 'a", |s| format!("fn(x: {s}) -> 'a"))
    }

    fn walk_steps(f: impl FnOnce()) -> usize {
        super::TVAR_WALK_STEPS.with(|c| c.set(0));
        f();
        super::TVAR_WALK_STEPS.with(|c| c.get())
    }

    /// Parsing each shape 160 deep takes under six times the steps 40 deep
    /// does.
    fn assert_parse_in_linear_steps(shapes: [(&str, fn(usize) -> String); 3]) {
        for (name, shape) in shapes {
            let parse = |n| {
                let src = shape(n);
                walk_steps(|| drop(parse_fn_type(&src).unwrap()))
            };
            let (a, b) = (parse(40), parse(160));
            assert!(b < a * 6, "{name}: 40 deep: {a} steps, 160 deep: {b}");
        }
    }

    // Parsing a nest of signatures that name nothing outside themselves
    // walks each a bounded number of times, however they nest.
    #[test]
    fn nested_closed_signatures_parse_in_linear_steps() {
        assert_parse_in_linear_steps([
            ("argument", nested_quantified),
            ("bound", nested_by_bound),
            ("unquantified", nested_unquantified),
        ])
    }

    // Parsing a nest of signatures each naming a variable no signature
    // inside it declares walks each a bounded number of times: only the
    // declaration of a name walks down to it.
    #[test]
    fn nested_open_signatures_parse_in_linear_steps() {
        assert_parse_in_linear_steps([
            ("naming outer", nested_naming_outer),
            ("bound naming outer", nested_by_bound_naming_outer),
            ("unquantified", nested_naming_a_variable),
        ])
    }

    /// `(name, depth, cell)` of every variable `t` holds, by position and
    /// through each cell's conjuncts, a signature's one deeper than the
    /// signature holding it.
    fn cells_by_depth(t: &crate::typ::Type) -> Vec<(String, usize, usize)> {
        use crate::typ::Type;
        fn go(
            t: &Type,
            depth: usize,
            seen: &mut ahash::AHashSet<usize>,
            out: &mut Vec<(String, usize, usize)>,
        ) {
            match t {
                Type::TVar(tv) => {
                    out.push((tv.name.to_string(), depth, tv.cell_addr()));
                    if seen.insert(tv.cell_addr()) {
                        for c in tv.cell_constraints() {
                            go(&c, depth, seen, out)
                        }
                    }
                }
                Type::Fn(ft) => ft.for_each_type(&mut |t| go(t, depth + 1, seen, out)),
                t => t.for_each_child(&mut |c| go(c, depth, seen, out)),
            }
        }
        let mut out = vec![];
        go(t, 0, &mut ahash::AHashSet::default(), &mut out);
        out
    }

    // In a nest whose signatures name a variable declared outside them,
    // every occurrence is the declaring signature's cell, and each
    // signature's own variable is a cell of its own.
    #[test]
    fn an_open_nest_names_its_declarers_cells() {
        use crate::typ::Type;
        let shapes: [(&str, fn(usize) -> String, &str); 3] = [
            ("naming outer", nested_naming_outer, "z"),
            ("bound naming outer", nested_by_bound_naming_outer, "z"),
            ("unquantified", nested_naming_a_variable, "a"),
        ];
        for (name, shape, outer) in shapes {
            let t = Type::Fn(triomphe::Arc::new(parse_fn_type(&shape(6)).unwrap()));
            let cells = cells_by_depth(&t);
            let of = |n: &'static str| cells.iter().filter(move |(m, _, _)| m == n);
            let mut outer_cells = of(outer).map(|(_, _, c)| *c).collect::<Vec<_>>();
            outer_cells.sort();
            outer_cells.dedup();
            assert_eq!(outer_cells.len(), 1, "{name}: {t}");
            if outer == "z" {
                let mut own = of("a").map(|(_, d, c)| (*d, *c)).collect::<Vec<_>>();
                own.sort();
                own.dedup();
                let distinct =
                    own.iter().map(|(_, c)| *c).collect::<ahash::AHashSet<_>>();
                assert!(own.len() == 6 && distinct.len() == 6, "{name}: {own:?}: {t}");
            }
        }
    }

    // Comparing two nests of quantified signatures reads each signature's
    // variables a bounded number of times, the ones it names from outside
    // included.
    #[test]
    fn nested_quantified_types_compare_in_linear_steps() {
        let shapes: [(&str, fn(usize) -> String); 3] = [
            ("argument", nested_quantified),
            ("bound", nested_by_bound),
            ("naming outer", nested_naming_outer),
        ];
        for (name, shape) in shapes {
            let compare = |n| {
                let (a, b) = (
                    parse_fn_type(&shape(n)).unwrap(),
                    parse_fn_type(&shape(n)).unwrap(),
                );
                walk_steps(|| assert!(a == b && a.cmp(&b).is_eq()))
            };
            let (a, b) = (compare(40), compare(160));
            assert!(b < a * 6, "{name}: 40 deep: {a} steps, 160 deep: {b}");
        }
    }

    /// The cell of the variable `name` that `t`, a signature, reaches.
    fn cell_named(t: &crate::typ::Type, name: &str) -> usize {
        let crate::typ::Type::Fn(ft) = t else { panic!("{t} is no signature") };
        let mut cells = ahash::AHashMap::default();
        ft.collect_tvars_through_bounds(&mut cells);
        cells[name].cell_addr()
    }

    // A nested signature that names an enclosing signature's variable only
    // in a bound is not closed: the variable is the enclosing signature's
    // cell.
    #[test]
    fn a_bound_naming_an_outer_variable_is_the_outer_cell() {
        for inner in [
            "fn<'b: Array<'z>>(y: 'b) -> 'b",
            "fn<'b: Any>(y: fn<'c: Array<'z>>(v: 'c) -> 'c) -> 'b",
        ] {
            let src = format!("fn<'z: Number>(x: {inner}, w: 'z) -> 'z");
            let ft = parse_fn_type(&src).unwrap();
            let crate::typ::Type::TVar(outer) = &ft.rtype else { panic!("{src}") };
            assert_eq!(cell_named(&ft.args[0].typ, "z"), outer.cell_addr(), "{src}");
        }
    }

    // A nested signature's own variable stays apart from an enclosing
    // signature's of the same name.
    #[test]
    fn a_closed_signature_keeps_its_own_variable() {
        let src = "fn<'a: Number>(x: fn<'a: Any>(y: 'a) -> 'a, w: 'a) -> 'a";
        let ft = parse_fn_type(src).unwrap();
        let crate::typ::Type::TVar(outer) = &ft.rtype else { panic!("{src}") };
        assert_ne!(cell_named(&ft.args[0].typ, "a"), outer.cell_addr());
        assert_eq!(ft.to_string(), src);
    }

    // A signature that names nothing outside itself is the same signature
    // inside an enclosing one, whose declaration aliases none of its cells.
    #[test]
    fn a_closed_signature_nests_as_it_stands_alone() {
        let closed = "fn<'a: Number, 'z: Array<'a>>(x0: fn<'a: Any>(x1: 'z, q0: 'a) -> 'a, \
                      x1: 'z, q1: 'a) -> 'a";
        assert_eq!(parse_fn_type(closed).unwrap().to_string(), closed);
        let nested = format!("fn(x: {closed}) -> i64");
        assert_eq!(parse_fn_type(&nested).unwrap().to_string(), nested);
    }

    // A variable the bound of an enclosing signature's quantifier names is
    // the enclosing signature's, where a signature nested in it declares a
    // quantifier of the same name: the nest prints as written.
    #[test]
    fn a_nested_quantifier_takes_no_enclosing_bounds_variable() {
        let middle = |inner: &str| {
            format!(
                "fn<'a: Number, 'z: Array<'a>>(x0: {inner}, x1: 'z, q1: 'a, w: 'k) -> 'a"
            )
        };
        let rewritten = [
            format!(
                "fn(x: {}, k: 'k) -> i64",
                middle("fn<'a: Any>(x1: 'z, q0: 'a) -> 'a")
            ),
            format!(
                "fn(x: {}, k: 'k) -> i64",
                middle("fn<'a: Any>(x1: fn(y: 'z) -> i64, q0: 'a) -> 'a")
            ),
            format!(
                "fn<'q: Any>(x: {}, k: 'k) -> 'q",
                middle("fn<'a: Any>(x1: fn<'b: Any>(y: 'z, qb: 'b) -> 'b, q0: 'a) -> 'a")
            ),
            "fn<'b: Any>(qb: 'b) -> fn<'d: Array<'b>>(x: 'b, qd: 'd) -> \
             fn<'b: Number>(qb: 'b) -> 'd"
                .into(),
        ]
        .into_iter()
        .filter_map(|src| {
            let printed = parse_fn_type(&src).unwrap().to_string();
            (printed != src).then_some(printed)
        })
        .collect::<Vec<_>>();
        assert!(rewritten.is_empty(), "{rewritten:#?}");
    }

    // What a nested signature names from outside itself is read again
    // after a cell write: a conjunct added to a cell it names is seen.
    #[test]
    fn a_cell_write_drops_the_free_cells_read_before_it() {
        use crate::typ::{TVar, Type};
        let ft = parse_fn_type("fn(x: fn<'b: Any>(y: 'b, q: 'z) -> 'b) -> 'z").unwrap();
        let _scope = super::PartsDepth::enter();
        let mut before = ahash::AHashMap::default();
        ft.args[0].typ.collect_tvars(&mut before);
        assert!(before.contains_key("z") && !before.contains_key("w"));
        before["z"].add_cell_constraint(Type::TVar(TVar::empty_named("w".into())));
        let mut after = ahash::AHashMap::default();
        ft.args[0].typ.collect_tvars(&mut after);
        assert!(after.contains_key("w"));
    }

    /// The tvar-walk steps printing an `n`-deep nest of quantified function
    /// types takes, flat and pretty.
    fn print_steps(n: usize) -> usize {
        use crate::{expr::print::PrettyDisplay, typ::Type};
        let ft = triomphe::Arc::new(parse_fn_type(&nested_quantified(n)).unwrap());
        let t = Type::Fn(ft);
        super::TVAR_WALK_STEPS.with(|c| c.set(0));
        let flat = t.to_string();
        let pretty = t.to_string_pretty(80).to_string();
        assert!(flat.matches("fn<").count() == n && pretty.matches("fn<").count() == n);
        super::TVAR_WALK_STEPS.with(|c| c.get())
    }

    // Printing a nest of quantified function types walks each node a
    // bounded number of times: the steps grow linearly with the depth.
    #[test]
    fn nested_quantified_types_print_in_linear_steps() {
        let (a, b) = (print_steps(40), print_steps(160));
        assert!(b < a * 6, "40 deep: {a} steps, 160 deep: {b}");
    }

    /// The pretty pipeline keeps the user-written `'a` on both
    /// occurrences of a polymorphic sig.
    #[test]
    fn polymorphic_sig_preserves_tvar_names() {
        let ft = parse_fn_type("fn(a: Array<'a>, @args: 'a) -> Array<'a>").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let folded = ft.replace_auto_constrained();
        let resolved = folded.resolve_tvars();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(resolved)));
        assert!(
            s.contains("'a"),
            "named tvar 'a should survive pretty-printing, got: {s}"
        );
        assert!(
            !s.contains("'_"),
            "auto tvars should not leak into pretty output, got: {s}"
        );
    }

    /// An implicit throws slot (an unbound auto TVar) is not printed.
    #[test]
    fn unbound_auto_throws_is_hidden() {
        let ft = parse_fn_type("fn(a: Array<'a>) -> i64").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let folded = ft.replace_auto_constrained();
        let resolved = folded.resolve_tvars();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(resolved)));
        assert!(!s.contains("throws"), "unbound auto throws should not appear, got: {s}");
    }

    /// An explicit `throws T` is always printed, even for `Bottom` or
    /// an auto TVar.
    #[test]
    fn explicit_throws_always_shown() {
        let ft = parse_fn_type("fn(x: 'a) -> 'a throws `Boom").unwrap();
        ft.alias_tvars(&mut LPooled::take());
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(ft)));
        assert!(s.contains("throws"), "explicit throws should be shown, got: {s}");
        assert!(s.contains("`Boom"), "throws variant should be printed, got: {s}");
    }
}

impl FnType {
    /// The conjuncts travel with their cells: each TVar writes its own,
    /// or the image's shared cell definition holds them.
    fn shape_len(&self) -> usize {
        self.args.encoded_len()
            + self.vargs.encoded_len()
            + self.rtype.encoded_len()
            + self.throws.encoded_len()
            + self.explicit_throws.encoded_len()
            + self.quantifiers.encoded_len()
    }

    fn shape_encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        self.args.encode(buf)?;
        self.vargs.encode(buf)?;
        self.rtype.encode(buf)?;
        self.throws.encode(buf)?;
        self.explicit_throws.encode(buf)?;
        self.quantifiers.encode(buf)
    }

    fn shape_decode(
        buf: &mut impl Buf,
        own: Option<LambdaId>,
    ) -> Result<Self, PackError> {
        let args = <Arc<[FnArgType]> as Pack>::decode(buf)?;
        let vargs = <Option<Type> as Pack>::decode(buf)?;
        let rtype = <Type as Pack>::decode(buf)?;
        let throws = <Type as Pack>::decode(buf)?;
        let explicit_throws = <bool as Pack>::decode(buf)?;
        let quantifiers = <Arc<[ArcStr]> as Pack>::decode(buf)?;
        // Provenance only; excluded from FnType identity.
        let lambda_ids = LambdaIds::default();
        if let Some(id) = own {
            lambda_ids.set_id(id);
        }
        Ok(FnType {
            args,
            vargs,
            rtype,
            throws,
            explicit_throws,
            quantifiers,
            lambda_ids,
        })
    }
}

/// Under an image session a function type is an object carrying its
/// own lambda id, written once and referenced afterwards (a binding's
/// type and its definition share one).
impl Pack for FnType {
    fn encoded_len(&self) -> usize {
        if image::is_encoding() { image::fntype_len(self) } else { self.shape_len() }
    }

    fn encode(&self, buf: &mut impl BufMut) -> Result<(), PackError> {
        if image::is_encoding() {
            image::fntype_encode(self, buf, |buf| {
                self.lambda_ids.own().encode(buf)?;
                self.shape_encode(buf)
            })
        } else {
            self.shape_encode(buf)
        }
    }

    fn decode(buf: &mut impl Buf) -> Result<Self, PackError> {
        if image::is_decoding() {
            image::object_decode(
                buf,
                |buf| {
                    let own = <Option<LambdaId> as Pack>::decode(buf)?;
                    Self::shape_decode(buf, own)
                },
                |b| Self::decode(b),
            )
        } else {
            Self::shape_decode(buf, None)
        }
    }
}

/// The bounds the type references in `t` give the open variables passed
/// as their parameters, by cell: a call checks them where it expands the
/// reference, as it checks a variable's own conjuncts.
fn param_bounds_of(env: &Env, t: &Type, out: &mut AddrMap<SmallVec<[Type; 1]>>) {
    crate::stack::ensure_sufficient(|| match t {
        Type::Ref(tr) => {
            if let Some(resolved) = tr.resolve_in(env)
                && let Some(known) = resolved.bindings(&tr.params)
            {
                for ((_, bound), arg) in resolved.params().iter().zip(tr.params.iter()) {
                    match (bound, arg) {
                        (Some(bound), Type::TVar(tv)) if !tv.is_bound() => out
                            .entry(tv.cell_addr())
                            .or_default()
                            .push(bound.replace_tvars(&known)),
                        (Some(bound), arg) => tvar::handed_bounds(
                            std::slice::from_ref(bound),
                            arg,
                            &mut |tv, b| out.entry(tv.cell_addr()).or_default().push(b),
                        ),
                        (None, _) => (),
                    }
                }
            }
            for p in tr.params.iter() {
                param_bounds_of(env, p, out)
            }
        }
        Type::Fn(ft) => ft.for_each_part(&mut |t, _| param_bounds_of(env, t, out)),
        t => t.for_each_child(&mut |c| param_bounds_of(env, c, out)),
    })
}

/// Whether two members of a union are open cells, and not one cell.
fn two_open_cells(ms: &[Type]) -> bool {
    let mut open = ms.iter().filter_map(|m| match m {
        Type::TVar(tv) => tv.open_cell(),
        _ => None,
    });
    open.next().is_some_and(|first| open.any(|tv| !tv.same_cell(&first)))
}

/// Whether `t` holds a union two of whose members are two open cells,
/// which a containment of it decides only through their other positions.
fn holds_open_union(t: &Type) -> bool {
    fn go(t: &Type, seen: &mut AHashSet<usize>) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => seen.insert(tv.cell_addr()) && go(&b, seen),
                None => false,
            },
            Type::Set(ms) if two_open_cells(ms) => true,
            t if super::node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
            t => {
                let mut found = false;
                t.for_each_child(&mut |c| found = found || go(c, seen));
                found
            }
        })
    }
    go(t, &mut LPooled::take())
}

/// The open cells a pass of [`Type::place_cells`] binds.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Placed {
    /// Those whose bounds hold no open cell: binding one decides no other.
    Free,
    All,
}

impl Type {
    /// Bind each open cell of `self` to what stands at its place in `at`,
    /// ⊥ included, wherever the two types are built alike: first the cells
    /// whose bounds hold no other, so a cell a bound holds takes its own
    /// place, not what the bound's holder is placed at. A union's members
    /// have no places: a cell they alone hold is left open.
    pub fn place_cells(&self, env: &Env, at: &Self) {
        self.place(env, at, Placed::Free);
        self.place(env, at, Placed::All)
    }

    /// One pass of [`Self::place_cells`], over the cells `placed` names.
    fn place(&self, env: &Env, at: &Self, placed: Placed) {
        let decides_others = |tv: &TVar| {
            let open = |c: &Type| {
                super::contains::find_open_cell(c, true, &mut |_| true).is_some()
            };
            tv.cell_constraints().iter().any(open)
        };
        crate::stack::ensure_sufficient(|| match (self, at) {
            (Self::TVar(tv), _) => match tv.binding() {
                Some(b) => b.place(env, at, placed),
                None if placed == Placed::Free && decides_others(tv) => (),
                None if at.with_deref(|a| matches!(a, Some(Self::Bottom))) => {
                    tv.bind(Self::Bottom)
                }
                None => {
                    let _ = self.contains(env, at);
                }
            },
            (_, Self::TVar(tv)) => {
                if let Some(b) = tv.binding() {
                    self.place(env, &b, placed)
                }
            }
            (Self::Set(_), _) | (_, Self::Set(_)) => (),
            (Self::Fn(f0), Self::Fn(f1)) => f0.place(env, f1, placed),
            (Self::Ref(r0), Self::Ref(r1)) => {
                if r0.same_definition(r1, env) && r0.params.len() == r1.params.len() {
                    for (own, at) in r0.params.iter().zip(r1.params.iter()) {
                        own.place(env, at, placed)
                    }
                }
            }
            (own, at) => {
                any_part_pair(own, at, &mut |own, at| {
                    own.place(env, at, placed);
                    false
                });
            }
        })
    }
}

impl FnType {
    /// [`Type::place_cells`] over two signatures, position by position.
    fn place(&self, env: &Env, at: &Self, placed: Placed) {
        for (own, at) in self.align(at).0 {
            own.typ.place(env, &at.typ, placed)
        }
        if let (Some(own), Some(at)) = (&self.vargs, &at.vargs) {
            own.place(env, at, placed)
        }
        self.rtype.place(env, &at.rtype, placed);
        self.throws.place(env, &at.throws, placed)
    }

    /// [`Type::place_cells`] over two signatures: both passes over every
    /// position.
    pub fn place_cells(&self, env: &Env, at: &Self) {
        self.place(env, at, Placed::Free);
        self.place(env, at, Placed::All)
    }

    /// `self`, a definition's signature, as an instance at `instance`
    /// holds it, and the copy of each open cell by its address: every cell
    /// the instance's own and bound to the instance's type. Where the
    /// containment would have to choose a union's member before the
    /// positions that decide it, each cell takes what stands at its own
    /// place first ([`Type::place_cells`]); where it would still have to
    /// choose, a cell only bounds hold, which has no place, is first what
    /// its bounds settle it to ([`TVar::settle_bounds_first`]), as a
    /// call's check leaves one it did not decide. Where two undecided
    /// members of a union of the instance's could still take one of its
    /// types, the first does: the definition's check holds at either.
    pub fn held_at(
        &self,
        env: &Env,
        instance: &Self,
    ) -> Result<(Self, AHashMap<usize, TVar>)> {
        let mut known = AHashMap::default();
        let own = self.instantiate_owned(&mut known);
        if instance.check_contains(env, &own).is_ok() {
            return Ok((own, known));
        }
        known.clear();
        let own = self.instantiate_owned(&mut known);
        own.place_cells(env, instance);
        if instance.check_contains(env, &own).is_ok() {
            return Ok((own, known));
        }
        known.clear();
        let own = self.instantiate_owned(&mut known);
        let mut placed: LPooled<AddrSet> = LPooled::take();
        own.for_each_type(&mut |t| {
            super::contains::find_open_cell(t, true, &mut |tv| {
                placed.insert(tv.cell_addr());
                false
            });
        });
        let unplaced = known.values().filter(|tv| !placed.contains(&tv.cell_addr()));
        let unplaced = sorted_tvars(unplaced.map(|tv| (tv.name.clone(), tv.clone())));
        for (_, tv) in unplaced.iter() {
            tv.settle_bounds_first(env, &mut placed)
        }
        own.place_cells(env, instance);
        if instance.check_contains(env, &own).is_ok() {
            return Ok((own, known));
        }
        known.clear();
        let own = self.instantiate_owned(&mut known);
        own.place_cells(env, instance);
        instance.check_contains_ordered(env, &own)?;
        Ok((own, known))
    }

    /// [`Self::check_contains`] where a union's first member that holds a
    /// type takes it ([`Type::contains_ordered`]).
    fn check_contains_ordered(&self, env: &Env, other: &Self) -> Result<()> {
        let flags = ContainsFlags::Commit.into();
        if !self.contains_int(flags, env, &mut ContainsHist::ordered(), other)? {
            bail!("Fn type mismatch {self} does not contain {other}")
        }
        Ok(())
    }
}

impl Type {
    /// Whether this type holds a union two of whose members are two open
    /// cells: a type going into it decides neither, so an equality
    /// relates it as the contained side first.
    pub fn holds_open_union(&self) -> bool {
        holds_open_union(self)
    }
}

impl Type {
    /// A read's copy of the signatures this type holds of definitions whose
    /// gates are closed (a lambda held in a struct, tuple or array): each such
    /// function type copied as a read of a generalized binding copies its
    /// scheme ([`FnType::scheme`]), through one cell map, all but those `keep`
    /// shares, and each typedef application whose arguments hold such a cell
    /// ([`Type::copied_outside_functions`]) alike. `None` when it holds none.
    #[doc(hidden)]
    pub fn scheme_read(
        &self,
        open: &IntSet<LambdaId>,
        keep: &dyn Fn(&FnType) -> bool,
    ) -> Option<Type> {
        ReadCopies::default().read(self, open, keep)
    }

    /// [`Self::scheme_read`] taking one instance of each signature
    /// ([`FnType::one_instance`]): a read no binding names.
    #[doc(hidden)]
    pub fn instance_read(
        &self,
        open: &IntSet<LambdaId>,
        keep: &dyn Fn(&FnType) -> bool,
    ) -> Option<Type> {
        let mut copies = ReadCopies::default();
        ReadWalk { open, keep, copies: &mut copies, generic: false }.go(self)
    }
}

/// The copies reads take ([`Type::scheme_read`]) through one cell map:
/// reads through one `ReadCopies` take one copy of what they share, and a
/// type a read made is its own copy to a later read.
#[derive(Debug, Default)]
#[doc(hidden)]
pub struct ReadCopies {
    known: LPooled<AHashMap<usize, TVar>>,
    /// By node, a var by its cell, beside the node, which this keeps
    /// alive: its copy, `None` where the node is its own.
    memo: LPooled<AHashMap<usize, (Type, Option<Type>)>>,
}

impl ReadCopies {
    /// `t` as a read through these copies takes it ([`Type::scheme_read`]).
    #[doc(hidden)]
    pub fn read(
        &mut self,
        t: &Type,
        open: &IntSet<LambdaId>,
        keep: &dyn Fn(&FnType) -> bool,
    ) -> Option<Type> {
        ReadWalk { open, keep, copies: self, generic: true }.go(t)
    }
}

struct ReadWalk<'a> {
    open: &'a IntSet<LambdaId>,
    keep: &'a dyn Fn(&FnType) -> bool,
    copies: &'a mut ReadCopies,
    /// Copies are generic, a scheme's; else one instance's.
    generic: bool,
}

impl<'a> ReadWalk<'a> {
    fn how(&self) -> Fresh<'a> {
        match self.generic {
            true => Fresh::Scheme(self.open),
            false => Fresh::Instantiate(self.open),
        }
    }
}

impl ReadWalk<'_> {
    // CR claude for claude: [risk/low] A named type's copy is not its own copy to
    // a later read, against ReadCopies' contract ("a type a read made is its own
    // copy to a later read"): `node` gives a `Ref` no key, so neither the copy a
    // read makes of one nor one it rebuilt is remembered, and a later read
    // through the same ReadCopies that meets the copy copies it again wherever
    // its cells are ones a read copies, where a function type's copy is left alone.
    // No program is shown to tell the two apart: reads of one node map through
    // `known` to the same cells either way. Keying an applied `Ref` here as
    // node_addr does everywhere else (drop `node`'s `Ref` arm) would make the
    // contract hold; it changes which cells a nested holder's reads share, so it
    // wants a pin and a census. Found fixing chainbounds-elab-quadratic-01, which
    // gave node_addr the key. (read-copies-ref-01)
    fn node(t: &Type) -> Option<usize> {
        match t {
            Type::Ref(_) => None,
            t => super::node_addr(t),
        }
    }

    fn go(&mut self, t: &Type) -> Option<Type> {
        crate::stack::ensure_sufficient(|| {
            let addr = match t {
                Type::TVar(tv) => Some(tv.cell_addr()),
                t => Self::node(t),
            };
            if let Some(a) = addr
                && let Some((_, r)) = self.copies.memo.get(&a)
            {
                return r.clone();
            }
            let r = match t {
                Type::TVar(tv) => {
                    let b = tv.binding()?;
                    self.copies.memo.insert(tv.cell_addr(), (t.clone(), None));
                    self.go(&b).map(|r| tv.forwarded_as(r))
                }
                Type::Fn(ft)
                    if (self.keep)(ft) || !copied_beneath(t, self.open, true) =>
                {
                    None
                }
                Type::Fn(ft) => {
                    let how = self.how();
                    let fresh = ft.reset_tvars_int(&mut self.copies.known, how);
                    fresh.alias_tvars(&mut LPooled::take());
                    Some(Type::Fn(Arc::new(fresh)))
                }
                Type::Ref(_) if t.copied_outside_functions(self.open) => {
                    let how = self.how();
                    let fresh = t
                        .reset_tvars_int(&mut self.copies.known, how)
                        .unwrap_or_else(|| t.clone());
                    fresh.alias_tvars(&mut LPooled::take());
                    Some(fresh)
                }
                t => t.cow_children(&mut |c| self.go(c)),
            };
            if let Some(a) = addr {
                self.copies.memo.insert(a, (t.clone(), r.clone()));
            }
            if let Some(c) = &r
                && let Some(a) = Self::node(c)
            {
                self.copies.memo.insert(a, (c.clone(), None));
            }
            r
        })
    }
}

impl Type {
    /// Whether this type holds, outside every function type, an open cell
    /// a scheme's copy under the open gates `open` copies: a typedef's
    /// arguments, which stand for what its body holds.
    #[doc(hidden)]
    pub fn copied_outside_functions(&self, open: &IntSet<LambdaId>) -> bool {
        copied_beneath(self, open, false)
    }
}

/// What [`Type::copied_in_two_places`] learned of typedef applications
/// over one copy: the places (0, 1, or 2 for two or more) an application's
/// expansion holds, by its declaration and which of its arguments hold a
/// copied cell, anywhere and outside function types.
#[doc(hidden)]
pub type CopySites =
    AHashMap<(ModPath, SourcePosition, SmallVec<[(bool, bool); 4]>), usize>;

impl Type {
    /// Whether this type holds two places a copy takes apart: function
    /// types and typedef applications ([`Type::copied_outside_functions`])
    /// that hold a cell the open gates `open` copy, none inside another.
    /// An application counts the places its expansion holds, one where
    /// the expansion meets its own typedef.
    #[doc(hidden)]
    pub fn copied_in_two_places(
        &self,
        env: &Env,
        open: &IntSet<LambdaId>,
        memo: &mut CopySites,
    ) -> bool {
        struct Walk<'a> {
            env: &'a Env,
            open: &'a IntSet<LambdaId>,
            memo: &'a mut CopySites,
            sites: usize,
            empty: LPooled<AHashSet<usize>>,
            path: LPooled<AHashSet<usize>>,
            expanding: LPooled<Vec<(ModPath, SourcePosition)>>,
        }
        impl Walk<'_> {
            fn go(&mut self, t: &Type) -> bool {
                crate::stack::ensure_sufficient(|| {
                    // an application's sites depend on the definitions being
                    // expanded, so it is counted wherever it is met
                    let addr = match t {
                        Type::TVar(tv) => Some(tv.cell_addr()),
                        Type::Ref(_) => None,
                        t => super::node_addr(t),
                    };
                    if let Some(a) = addr
                        && (self.empty.contains(&a) || !self.path.insert(a))
                    {
                        return false;
                    }
                    let before = self.sites;
                    let done = match t {
                        Type::TVar(tv) => tv.binding().is_some_and(|b| self.go(&b)),
                        Type::Fn(_) => {
                            self.sites += copied_beneath(t, self.open, true) as usize;
                            self.sites > 1
                        }
                        Type::Ref(tr) if t.copied_outside_functions(self.open) => {
                            self.sites += self.application(t, tr);
                            self.sites > 1
                        }
                        Type::Ref(_) => false,
                        t => t
                            .try_for_each_child(&mut |c| match self.go(c) {
                                true => ControlFlow::Break(()),
                                false => ControlFlow::Continue(()),
                            })
                            .is_break(),
                    };
                    if let Some(a) = addr {
                        self.path.remove(&a);
                        if self.sites == before {
                            self.empty.insert(a);
                        }
                    }
                    done
                })
            }

            fn application(&mut self, t: &Type, tr: &super::TypeRef) -> usize {
                let Some(def) = tr.resolve_peek(self.env) else { return 1 };
                let decl = (def.canonical_scope.clone(), def.pos);
                if self.expanding.contains(&decl) {
                    return 1;
                }
                let args = tr
                    .params
                    .iter()
                    .map(|p| {
                        (
                            copied_beneath(p, self.open, true),
                            copied_beneath(p, self.open, false),
                        )
                    })
                    .collect();
                let key = (decl.0.clone(), decl.1, args);
                if let Some(n) = self.memo.get(&key) {
                    return *n;
                }
                let Some((_, body)) = t.lookup_ref_probe(self.env) else { return 1 };
                let outer = std::mem::replace(&mut self.sites, 0);
                self.expanding.push(decl);
                self.go(&body);
                self.expanding.pop();
                let n = std::mem::replace(&mut self.sites, outer).min(2);
                self.memo.insert(key, n);
                n
            }
        }
        let mut w = Walk {
            env,
            open,
            memo,
            sites: 0,
            empty: LPooled::take(),
            path: LPooled::take(),
            expanding: LPooled::take(),
        };
        w.go(self)
    }
}

/// Whether `t` reaches an open cell a scheme's copy under the open gates
/// `open` copies; inside a function type only where `functions`.
fn copied_beneath(t: &Type, open: &IntSet<LambdaId>, functions: bool) -> bool {
    fn go(
        t: &Type,
        open: &IntSet<LambdaId>,
        functions: bool,
        seen: &mut AHashSet<usize>,
    ) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                seen.insert(tv.cell_addr())
                    && match tv.binding() {
                        Some(b) => go(&b, open, functions, seen),
                        None => tv.level().copied_by(open),
                    }
            }
            Type::Fn(_) if !functions => false,
            t if super::node_addr(t).is_some_and(|n| !seen.insert(n)) => false,
            t => t
                .try_for_each_child(&mut |c| match go(c, open, functions, seen) {
                    true => ControlFlow::Break(()),
                    false => ControlFlow::Continue(()),
                })
                .is_break(),
        })
    }
    go(t, open, functions, &mut LPooled::take())
}
