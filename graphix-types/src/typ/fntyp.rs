use crate::{
    AddrMap, AddrSet, LambdaId, PRINT_FLAGS, PrintFlag, SourcePosition,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    image,
    typ::{
        Mutability, TVar, TVarsSeen, Type, key_text,
        matches::MatchHist,
        normalize::NormCx,
        tvar::{self, Fresh, Level},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::format_compact;
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
    ops::ControlFlow,
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

    // CR claude for claude: [bug/medium] `link` adds a `Weak` to both sides, and
    // nothing prunes a node's set except an `ids()` walk. A std `Weak` keeps the dead
    // node's allocation, so a long-lived signature that later compiles keep linking
    // grows by one dead node and one set entry each time. A dynamic module whose sig
    // declares a function val leaks about 100 bytes per reload this way, in both
    // engines: check_val's `sig_matches` links the sig's FnType to each load's lambda,
    // and massif shows `LambdaIds::default` under `Lambda::compile` plus this set as
    // the heap that grows. Prune dead links here (`strong_count() == 0`) whenever the
    // set has doubled since the last prune; that keeps `link` amortized O(1). The
    // leaks.rs reload pins assert `held()` only and cannot see this. probe:
    // design/review-2026-11-16/repro/x-leaks-reload-exported-fn.gx (x-leaks-03)
    pub fn link(&self, other: &LambdaIds) {
        self.0.write().links.insert(other.as_link());
        other.0.write().links.insert(self.as_link());
    }

    /// A node that reaches every id `nodes` reach, which none of them
    /// reaches back. It links what each node links as well as the node,
    /// so it outlives a node that is itself a join.
    pub fn joining(nodes: &[&LambdaIds]) -> LambdaIds {
        let joined = LambdaIds::default();
        {
            let mut inner = joined.0.write();
            for n in nodes {
                inner.links.insert(n.as_link());
                inner.links.extend(n.0.read().links.iter().cloned());
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
    /// The cells whose bounds `collect_held` is reading, apart from
    /// [`CONJUNCTS`], which the memo heeds. It cuts no walk the parse
    /// built: a walk reaches a cell again through its bounds only where a
    /// bound reaches its own cell, which is refused.
    static HOLDING: RefCell<AddrSet> = RefCell::new(AddrSet::default());
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
    /// signature holds from outside itself, as `collect_held` finds it,
    /// while this thread writes no cell.
    static FREE: RefCell<FreeCells> = RefCell::new(FreeCells::default());
    /// The signatures and cells the walks in progress entered
    /// [`CONSTRAINING`] and [`CONJUNCTS`] for, since the outermost
    /// [`free_cells`] read in progress began; empty where none is.
    static TOUCHED: RefCell<Vec<usize>> = const { RefCell::new(Vec::new()) };
    /// How many [`free_cells`] reads are in progress on this thread.
    static TOUCHING: Cell<usize> = const { Cell::new(0) };
    /// How many times a walk's guard has refused a signature or a cell.
    static CUTS: Cell<u64> = const { Cell::new(0) };
    /// How deep signature parses nest on this thread.
    static PARSE_DEPTH: Cell<usize> = const { Cell::new(0) };
    /// The variables of the signatures being parsed, innermost last.
    static NAMES: RefCell<Vec<SigNames>> = const { RefCell::new(Vec::new()) };
    /// For the outermost signature's parse: each signature declared inside
    /// it, with the cells it holds from outside itself as its declaration
    /// left them, which the parse's walks read rather than walk it again.
    static DECLARED: RefCell<AddrMap<(Arc<FnType>, Vec<(ArcStr, TVar)>)>> =
        RefCell::new(AddrMap::default());
}

#[derive(Default)]
struct FreeCells {
    /// [`tvar::cell_writes`] when the entries were read from the cells.
    writes: u64,
    of: AddrMap<FreeEntry>,
}

/// What `collect_held` found of a signature: the signature, the cells, and
/// the guard keys the walk entered, sorted, which it found them under.
type FreeEntry = (Arc<FnType>, Vec<(ArcStr, TVar)>, Vec<usize>);

impl FreeCells {
    /// The entries that still stand: none after a cell write.
    fn current(&mut self) -> &mut AddrMap<FreeEntry> {
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
        NAMES.with_borrow_mut(|n| n.push(SigNames::default()));
        ParseDepth
    }
}

/// One signature's variables while it parses: its own quantifiers, and in
/// the outermost every other name its parse writes.
struct SigNames {
    own: AHashMap<ArcStr, TVar>,
    /// While its quantifiers parse, the variables their bounds write by a
    /// name the signature has not declared yet: which variable each is
    /// waits for the declarations' end ([`quantifiers_end`]).
    waiting: Option<Vec<(ArcStr, TVar)>>,
}

impl Default for SigNames {
    fn default() -> Self {
        Self { own: AHashMap::default(), waiting: Some(Vec::new()) }
    }
}

/// The variable a signature's quantifier `name` is: the innermost
/// signature's own, whatever an enclosing one names alike. Outside a
/// signature's parse, a variable of its own.
pub fn declared_name(name: ArcStr) -> TVar {
    NAMES.with_borrow_mut(|n| match n.last_mut() {
        Some(sig) => sig
            .own
            .entry(name.clone())
            .or_insert_with(|| TVar::empty_generic(name))
            .clone(),
        None => TVar::empty_generic(name),
    })
}

/// The variable `name` written in a type is: the quantifier of the
/// innermost signature being parsed that declares it, else the outermost
/// signature's variable of that name. One written in a signature's bounds
/// before the signature's declarations end waits for them. Outside a
/// signature's parse, a variable of its own, which the compile names
/// ([`Type::scope_refs_named`]).
pub fn written_name(name: ArcStr) -> TVar {
    NAMES.with_borrow_mut(|n| {
        let len = n.len();
        name_in(n, len, name.clone(), || TVar::empty_generic(name))
    })
}

/// The variable `name` is among the signatures `n[..upto]`: `found`'s
/// handle where it waits there or is the first of its name.
fn name_in(
    n: &mut [SigNames],
    upto: usize,
    name: ArcStr,
    found: impl FnOnce() -> TVar,
) -> TVar {
    for sig in n[..upto].iter_mut().rev() {
        if let Some(tv) = sig.own.get(&name) {
            return tv.clone();
        }
        if let Some(waiting) = &mut sig.waiting {
            let tv = found();
            waiting.push((name, tv.clone()));
            return tv;
        }
    }
    match n.first_mut() {
        Some(outermost) => outermost.own.entry(name).or_insert_with(found).clone(),
        None => found(),
    }
}

/// The innermost signature's declarations are parsed: each variable its
/// bounds wrote by a name it had not declared yet is the variable that
/// name is now, its quantifier or an enclosing signature's.
pub fn quantifiers_end() {
    NAMES.with_borrow_mut(|n| {
        let Some(i) = n.len().checked_sub(1) else { return };
        for (name, tv) in n[i].waiting.take().unwrap_or_default() {
            let named = match n[i].own.get(&name) {
                Some(q) => q.clone(),
                None => name_in(n, i, name, || tv.clone()),
            };
            if !named.same_cell(&tv) {
                tv.point_at(&named)
            }
        }
    })
}

impl Drop for ParseDepth {
    fn drop(&mut self) {
        NAMES.with_borrow_mut(|n| {
            n.pop();
        });
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

/// `f` of the cells `ft` holds from outside itself, where it is a
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
/// holds from outside itself into `known`, and true.
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

/// The cells `ft` holds into `known`: those its positions name, and,
/// until nothing new arrives, those the bounds of its own quantifiers
/// among them name, so a quantifier held only through a bound of a bound,
/// or through a signature nested in it, is held with every cell its bounds
/// name. A cell declared outside `ft` has its bounds read in its
/// declarer's scope, never in `ft`'s ([`FnType::in_name_scope`]). Each
/// own cell's bounds are read once, under no guard the memo heeds, so the
/// free cells of a signature nested in them are read from the memo
/// ([`free_cells`]); a cell reached again while its bounds are read is cut
/// ([`HOLDING`]).
fn collect_held(ft: &FnType, known: &mut AHashMap<ArcStr, TVar>) {
    let _depth = PartsDepth::enter();
    ft.in_name_scope(&mut TVarsSeen::take(), |seen| {
        ft.for_each_type(&mut |t| t.collect_tvars_seen(known, seen));
        let mut todo: LPooled<Vec<TVar>> = LPooled::take();
        todo.extend(ft.quantifiers.iter().filter_map(|q| known.get(q).cloned()));
        let mut found: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        while let Some(tv) = todo.pop() {
            let Some(_holding) = Walking::enter_key(&HOLDING, tv.cell_addr()) else {
                continue;
            };
            for tc in tv.cell_constraints() {
                match tc.normalize_int(&mut seen.norm) {
                    Some(n) => n.collect_tvars_seen(&mut found, seen),
                    None => tc.collect_tvars_seen(&mut found, seen),
                }
            }
            for (name, tv) in found.drain() {
                if let Entry::Vacant(e) = known.entry(name) {
                    if ft.quantifiers.contains(e.key()) {
                        todo.push(tv.clone())
                    }
                    e.insert(tv);
                }
            }
        }
    })
}

/// The cells the quantified signature `ft` holds from outside itself
/// ([`collect_held`]), its own quantifiers' left out, into `known`:
/// memoized while a `for_each_part` walk is in progress. A walk no guard
/// cut is the same under every set of guards that holds none of the
/// signatures and cells it entered them for, and one that found nothing
/// finds nothing under any.
pub(super) fn free_cells(ft: &Arc<FnType>, known: &mut AHashMap<ArcStr, TVar>) {
    let key = Arc::as_ptr(ft) as usize;
    let hit = FREE.with_borrow_mut(|m| match m.current().get(&key) {
        Some((_, cells, entered)) if cells.is_empty() || !Walking::holds_any(entered) => {
            for (name, tv) in cells {
                known.entry(name.clone()).or_insert_with(|| tv.clone());
            }
            if TOUCHING.with(|t| t.get()) > 0 {
                TOUCHED.with_borrow_mut(|t| t.extend_from_slice(entered));
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
    let touching = Touching::begin();
    collect_held(ft, &mut inner);
    inner.retain(|name, _| !ft.quantifiers.contains(name));
    if PARTS_DEPTH.with(|d| d.get()) > 0 && CUTS.with(|c| c.get()) == cuts {
        let cells = inner.iter().map(|(n, tv)| (n.clone(), tv.clone())).collect();
        let entered = touching.keys();
        FREE.with_borrow_mut(|m| m.current().insert(key, (ft.clone(), cells, entered)));
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
        entered.then(|| Walking { set, key })
    }

    /// [`Self::enter_key`] on [`CONSTRAINING`] or [`CONJUNCTS`], the keys
    /// entered noted for the [`free_cells`] reads in progress, whether the
    /// guard admits the walk or not.
    fn enter_guard(set: &'static LocalKey<RefCell<AddrSet>>, key: usize) -> Option<Self> {
        if TOUCHING.with(|t| t.get()) > 0 {
            TOUCHED.with_borrow_mut(|t| t.push(key));
        }
        Self::enter_key(set, key)
    }

    /// Whether a guard of `CONSTRAINING` or `CONJUNCTS` holds one of `keys`.
    fn holds_any(keys: &[usize]) -> bool {
        let held = |set: &'static LocalKey<RefCell<AddrSet>>| {
            set.with_borrow(|s| !s.is_empty() && keys.iter().any(|k| s.contains(k)))
        };
        held(&CONSTRAINING) || held(&CONJUNCTS)
    }
}

/// One [`free_cells`] read in progress: the keys the guards were entered
/// for since it began are its walk's, and it forgets them when the
/// outermost read ends, unwind too.
struct Touching {
    start: usize,
}

impl Touching {
    fn begin() -> Self {
        TOUCHING.with(|t| t.set(t.get() + 1));
        Touching { start: TOUCHED.with_borrow(|t| t.len()) }
    }

    /// The keys entered since `begin`, sorted, and into the read around it.
    fn keys(&self) -> Vec<usize> {
        TOUCHED.with_borrow(|t| {
            let mut keys = t[self.start..].to_vec();
            keys.sort_unstable();
            keys.dedup();
            keys
        })
    }
}

impl Drop for Touching {
    fn drop(&mut self) {
        let outermost = TOUCHING.with(|t| {
            t.set(t.get() - 1);
            t.get() == 0
        });
        if outermost {
            TOUCHED.with_borrow_mut(|t| t.clear());
        }
    }
}

impl Drop for Walking {
    fn drop(&mut self) {
        self.set.with_borrow_mut(|s| s.remove(&self.key));
    }
}

/// The refusal of a bound of `'q` that reaches `'q`, as `path` says it does.
fn recursive_bound(q: &ArcStr, path: &str) -> anyhow::Error {
    anyhow!(
        "{path}: a bound may not reach its own variable; a recursive bound is a \
         recursive type (`type T = [i64, Array<T>]`, `'{q}: T`)"
    )
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
            return Err(recursive_bound(q, &path));
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

/// Seed each conjunct on its quantifier's cell in `known`, whose variables
/// the conjuncts' are already. A conjunct that reaches the cell it would
/// bound is refused: no cell holds itself through its bounds.
pub fn seed_conjuncts<'a>(
    known: &AHashMap<ArcStr, TVar>,
    constraints: impl IntoIterator<Item = (&'a TVar, Type)>,
) -> Result<()> {
    for (tv, tc) in constraints {
        let q = &known[&tv.name];
        let tc = tc.normalize();
        if tvar::would_cycle_inner(&q.cell(), &tc) {
            let path = format_compact!("'{0}'s bound reaches '{0}", q.name);
            return Err(recursive_bound(&q.name, &path));
        }
        q.add_cell_constraint(tc);
    }
    Ok(())
}

/// `known` and, until nothing new arrives, the cells the bounds of its
/// cells `ft`'s walk passes name ([`FnType::in_name_scope`]).
fn close_through_bounds(
    ft: &FnType,
    known: &mut AHashMap<ArcStr, TVar>,
    seen: &mut TVarsSeen,
) {
    ft.in_name_scope(seen, |seen| {
        let mut todo: LPooled<Vec<TVar>> = LPooled::take();
        todo.extend(known.values().cloned());
        let mut found: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        while let Some(tv) = todo.pop() {
            if !seen.passes(&tv) {
                continue;
            }
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
    })
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
    /// in source order (a `+`-bound variable is one quantifier), whose
    /// cells hold the conjuncts; refused where a conjunct reaches its own
    /// cell ([`seed_conjuncts`]). The signature's variables and its
    /// conjuncts' are the quantifiers' already: a parse names each written
    /// name once ([`written_name`]), a compile copies by name
    /// ([`Type::scope_refs_named`]). Inside another signature's parse
    /// ([`ParseDepth`]) the cells it holds from outside itself
    /// ([`collect_held`]) are recorded for the parse's walks.
    pub fn declaring(mut self, constraints: &[(TVar, Type)]) -> Result<Arc<Self>> {
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
        // CR claude for eric: [structure/medium] This arm, `Aliasing::Known`,
        // `DECLARED` and the readers of it (`declared`, `closed`, `declared_free`,
        // `aliases_into`, `meet_declared`) exist for a signature parsed inside another
        // signature. One that declares quantifiers there is refused by the check in
        // every position (`rewrite_trait_args_inner` reads every part and bound of a
        // signature with `scheme` false), so this bookkeeping and most of the
        // nest-parse tests below keep correct and linear a state the language forbids.
        // For Eric: refuse a quantified signature at parse depth above 1 in the
        // parser's `fnshared`, with the `QUANTIFIED_FN` text. A nested signature then
        // declares nothing, `declaring` returns it as it stands, the outermost
        // `Aliasing::Every` walk aliases every name, and the items named here go. Found
        // by reading; the refusals were run (35 positions), the deletion was not built.
        // (t-fntyp-05)
        // 2026-11-21 claude: the alias walk is gone: a parse names each
        // written name once per signature (`written_name`, `declared_name`),
        // so `Aliasing`, `aliases_into` and `meet_declared` went with it.
        // `DECLARED` and its readers (`declared`, `closed`, `declared_free`)
        // stand, as the memo of the walks over a nested signature; the
        // proposal stands as written.
        seed_conjuncts(&known, constraints.iter().map(|(tv, tc)| (tv, tc.clone())))?;
        if PARSE_DEPTH.with(|d| d.get()) > 1 {
            known.clear();
            collect_held(&ft, &mut known);
            let free = known.drain().filter(|(name, _)| !ft.quantifiers.contains(name));
            let free = (ft.clone(), free.collect());
            DECLARED.with_borrow_mut(|m| m.insert(Arc::as_ptr(&ft) as usize, free));
        }
        Ok(ft)
    }

    /// [`Self::collect_tvars`] and then, until nothing new arrives, the
    /// cells the bounds of the cells found name: a quantifier only the
    /// bound of a bound names is reached too.
    pub fn collect_tvars_through_bounds(&self, known: &mut AHashMap<ArcStr, TVar>) {
        let mut seen = TVarsSeen::take();
        self.collect_tvars_seen(known, &mut seen);
        close_through_bounds(self, known, &mut seen)
    }

    /// The variables the signature writes, in their one order: those its
    /// positions name and, through them, those the bounds of its own
    /// quantifiers name. A position left to inference writes none,
    /// whatever the bounds its check gave it name.
    pub fn written_tvars(&self) -> LPooled<Vec<(ArcStr, TVar)>> {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut seen = TVarsSeen::take();
        self.for_each_type(&mut |t| t.collect_tvars_seen(&mut known, &mut seen));
        if !self.quantifiers.is_empty() {
            close_through_bounds(self, &mut known, &mut seen);
        }
        sorted_tvars(known.drain().filter(|(name, _)| !name.starts_with('_')))
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
            close_through_bounds(self, &mut reached, &mut TVarsSeen::take());
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
    /// only a quantifier no position names takes the walk through the
    /// bounds of its own cells ([`collect_held`]).
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
            collect_held(self, &mut reached);
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
        self.for_each_part_passing(&mut (), |_, _| true, &mut |_, t, c| f(t, c))
    }

    /// [`Self::for_each_part`] over the walk state `cx`, passing the
    /// conjuncts only of the cells `pass` admits.
    fn for_each_part_passing<C>(
        &self,
        cx: &mut C,
        pass: impl Fn(&C, &TVar) -> bool,
        f: &mut impl FnMut(&mut C, &Type, bool),
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
            f(cx, &arg.typ, false)
        }
        if let Some(vargs) = vargs {
            f(cx, vargs, false)
        }
        f(cx, rtype, false);
        if let Some(_guard) =
            Walking::enter_guard(&CONSTRAINING, self as *const FnType as usize)
        {
            // the cells the positions name, each nested signature's own
            // conjunct pass left to that signature's walk
            let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            let mut seen: LPooled<AHashSet<usize>> = LPooled::take();
            self.for_each_type(&mut |t| t.collect_position_tvars(&mut cells, &mut seen));
            for (_, tv) in sorted_tvars(cells.drain()).drain(..) {
                if let Some(_cell) = Walking::enter_guard(&CONJUNCTS, tv.cell_addr())
                    && pass(cx, &tv)
                {
                    for tc in tv.cell_constraints() {
                        f(cx, &tc, true);
                    }
                }
            }
        }
        f(cx, throws, false);
    }

    pub fn reset_tvars(&self) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Copy)
    }

    /// [`Self::reset_tvars`] through `known`, the cells copied already:
    /// signatures copied through one map share each cell's copy.
    #[doc(hidden)]
    pub fn reset_tvars_with(&self, known: &mut AHashMap<usize, TVar>) -> Self {
        self.reset_tvars_int(known, Fresh::Copy)
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

    /// For display, each open auto variable (`'_23`) of the
    /// signature named a letter no variable of it uses and listed in its
    /// header, which prints it with its bounds: a variable the
    /// signature's positions share stays one variable. A cell of a
    /// declared quantifier, of `held` (a definition's own, which no call
    /// picks) and of an implicit `throws` slot keep their handles. Call on
    /// a resolved type ([`Type::resolve_tvars`]).
    pub fn name_auto_variables(&self, held: &[TVar]) -> Self {
        let mut all: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.collect_tvars(&mut all);
        let mut standing: LPooled<AHashSet<usize>> = LPooled::take();
        standing.extend(held.iter().filter_map(TVar::open_cell).map(|c| c.cell_addr()));
        standing_cells(&Type::Fn(Arc::new(self.clone())), &mut standing);
        let mut autos: LPooled<Vec<(ArcStr, usize)>> = LPooled::take();
        for (name, tv) in all.iter() {
            if let Some(open) = tv.open_cell()
                && !standing.contains(&open.cell_addr())
                && !autos.iter().any(|(_, a)| *a == open.cell_addr())
            {
                autos.push((name.clone(), open.cell_addr()));
            }
        }
        if autos.is_empty() {
            return self.clone();
        }
        let mut order: LPooled<Vec<usize>> = LPooled::take();
        self.for_each_type(&mut |t| occurrences(t, &mut order));
        autos.sort_unstable_by_key(|(name, addr)| {
            (order.iter().position(|o| o == addr), name.clone())
        });
        let mut taken: LPooled<AHashSet<ArcStr>> = LPooled::take();
        taken.extend(
            all.iter()
                .filter(|(_, tv)| {
                    tv.open_cell()
                        .is_none_or(|o| !autos.iter().any(|(_, a)| *a == o.cell_addr()))
                })
                .map(|(name, _)| name.clone()),
        );
        taken.extend(self.quantifiers.iter().cloned());
        let mut ft = self.clone();
        let mut quantifiers: Vec<ArcStr> = self.quantifiers.iter().cloned().collect();
        let mut next = 0usize;
        for (_, addr) in autos.drain(..) {
            let name = loop {
                let letter = char::from(b'a' + (next % 26) as u8);
                let n = match next / 26 {
                    0 => format_compact!("{letter}"),
                    round => format_compact!("{letter}{round}"),
                };
                let n = ArcStr::from(n.as_str());
                next += 1;
                if !taken.contains(&n) {
                    break n;
                }
            };
            taken.insert(name.clone());
            let mut memo = tvar::CopyMemo::default();
            let pick = |tv: &TVar| tv.open_cell().is_some_and(|o| o.cell_addr() == addr);
            if let Some(renamed) =
                ft.cow_walk(|t| t.renamed_cells(&pick, &name, &mut memo))
            {
                ft = renamed;
            }
            quantifiers.push(name);
        }
        ft.quantifiers = Arc::from(quantifiers);
        ft
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
        self.in_name_scope(&mut TVarsSeen::take(), |seen| {
            self.collect_tvars_seen(&mut held, seen);
            let mut todo: LPooled<Vec<TVar>> = held.values().cloned().collect();
            let mut found: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            while let Some(tv) = todo.pop() {
                if !seen.passes(&tv) {
                    continue;
                }
                for tc in tv.cell_constraints() {
                    match tc.normalize_int(&mut seen.norm) {
                        Some(n) => n.collect_tvars_seen(&mut found, seen),
                        None => tc.collect_tvars_seen(&mut found, seen),
                    }
                }
                for (name, tv) in found.drain() {
                    if !held.contains_key(&name) {
                        todo.push(tv.clone());
                        held.insert(name, tv);
                    }
                }
            }
        });
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

    /// A call's copy of the signature under the open gates `open`
    /// (`Fresh::Instantiate`).
    pub fn instantiate(&self, open: &IntSet<LambdaId>) -> Self {
        let mut known: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.reset_tvars_int(&mut known, Fresh::Instantiate(open))
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

    pub(super) fn collect_tvars_seen(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        seen: &mut TVarsSeen,
    ) {
        self.in_name_scope(seen, |seen| {
            self.for_each_part_passing(
                seen,
                TVarsSeen::passes,
                &mut |seen, t, constraint| match constraint {
                    true => match t.normalize_int(&mut seen.norm) {
                        Some(n) => n.collect_tvars_seen(known, seen),
                        None => t.collect_tvars_seen(known, seen),
                    },
                    false => t.collect_tvars_seen(known, seen),
                },
            )
        })
    }

    /// `f` of `seen` in the scope of a name walk over `self`
    /// ([`TVarsSeen::passes`]): its own quantifiers, or, where it declares
    /// none, the scope `seen` is in.
    fn in_name_scope<R>(
        &self,
        seen: &mut TVarsSeen,
        f: impl FnOnce(&mut TVarsSeen) -> R,
    ) -> R {
        if self.quantifiers.is_empty() {
            return f(seen);
        }
        let mut own: LPooled<AHashSet<ArcStr>> = LPooled::take();
        own.extend(self.quantifiers.iter().cloned());
        let enclosing = std::mem::replace(&mut seen.scope, Some(own));
        let r = f(seen);
        seen.scope = enclosing;
        r
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
        super::holds::holds_fn(env, self, t)
    }

    pub fn check_contains(&self, env: &Env, other: &Self) -> Result<()> {
        if !self.contains(env, other)? {
            bail!("Fn type mismatch {self} does not contain {other}")
        }
        Ok(())
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        let mut copies: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.scope_refs_in(Some(scope), &mut super::Copies::Cells(&mut copies))
    }

    /// [`Type::scope_refs_named`] of this signature's parts: its own
    /// quantifiers are names of `names` too, as its other variables are.
    pub fn scope_refs_named(
        &self,
        scope: &ModPath,
        names: &mut AHashMap<ArcStr, TVar>,
    ) -> Self {
        let mut copies = super::Copies::Names(names, LPooled::take());
        let mut fresh = self
            .cow_walk(|t| t.scope_refs_int(Some(scope), &mut copies))
            .unwrap_or_else(|| self.clone());
        fresh.lambda_ids = self.lambda_ids.instantiate();
        fresh
    }

    /// A signature of its own: each scoping of a written type is one
    /// compile's, and what unifies with it must not name another
    /// compile's definitions.
    pub(super) fn scope_refs_in(
        &self,
        scope: Option<&ModPath>,
        copies: &mut super::Copies,
    ) -> Self {
        let mut fresh = copies.within(&self.quantifiers, |copies| {
            self.cow_walk(|t| t.scope_refs_int(scope, copies))
                .unwrap_or_else(|| self.clone())
        });
        fresh.lambda_ids = self.lambda_ids.instantiate();
        fresh
    }
}

impl Type {
    /// [`FnType::name_auto_variables`] on each outermost function type this
    /// type holds.
    pub fn name_auto_variables(&self, held: &[TVar]) -> Self {
        crate::stack::ensure_sufficient(|| match self {
            Type::Fn(ft) => Type::Fn(Arc::new(ft.name_auto_variables(held))),
            t => t
                .cow_children(&mut |c| Some(c.name_auto_variables(held)))
                .unwrap_or_else(|| t.clone()),
        })
    }
}

/// The open cells `t` holds, in the order it prints them.
fn occurrences(t: &Type, cells: &mut Vec<usize>) {
    crate::stack::ensure_sufficient(|| match t {
        Type::TVar(tv) => cells.extend(tv.open_cell().map(|open| open.cell_addr())),
        t => t.for_each_child(&mut |c| occurrences(c, cells)),
    })
}

/// The cells of `t` that a name or a header already stands for: those of
/// the implicit `throws` slots, which print as nothing, and of the
/// quantifiers its function types declare.
fn standing_cells(t: &Type, cells: &mut AHashSet<usize>) {
    crate::stack::ensure_sufficient(|| {
        if let Type::Fn(ft) = t {
            if !ft.explicit_throws
                && let Type::TVar(tv) = &ft.throws
                && let Some(open) = tv.open_cell()
            {
                cells.insert(open.cell_addr());
            }
            ft.each_quantifier(|_, tv| {
                cells.extend(tv.open_cell().map(|open| open.cell_addr()))
            });
        }
        t.for_each_child(&mut |c| standing_cells(c, cells))
    })
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
            // CR claude for claude: [readability/low] A positional parameter with no
            // name (a pattern or `_` parameter) prints as its type alone, `fn((i64,
            // i64)) -> i64`, and the type parser refuses a positional parameter without
            // a name, so a type a diagnostic prints cannot be written back as an
            // annotation: `let f: fn((i64, i64)) -> i64 = ..` is a parse error, and so
            // is `fn(_: ..)`. Print it as `arg<i>: T`, the name
            // `rewrite_trait_args_inner` gives an unnamed trait parameter, so every
            // printed function type parses. Checked by running `--check` on the three
            // forms. (t-fntyp-09)
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

// CR claude for claude: [readability/low] The test module starts here, a thousand lines
// long, and 950 lines of production code follow it: the `Pack` impl, `param_bounds_of`,
// `Meetings`, `Places`, `place_in_order`, `held_at`, `ReadCopies` and
// `copied_in_two_places`. A reader takes the file's code as ended at `mod tests` and
// misses the walks the build's instance check runs. Move the tests to the end of the
// file, the placement walks to a file of their own and the read copies, which walk
// `Type` and not `FnType`, to another. Found by reading; not run. (t-fntyp-08)
// 2026-11-21 claude: Meetings, Places, place_in_order, held_at and sig_bounds are
// deleted (bounds consolidation); ReadCopies and copied_in_two_places still follow the
// test module.
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

    /// A bound that reaches the cell it is seeded on is refused, whatever
    /// the names said before the cells were aliased.
    #[test]
    fn a_seeded_bound_reaching_its_cell_is_refused() {
        use super::seed_conjuncts;
        use crate::{expr::parser::parse_type, typ::TVar};
        use ahash::AHashMap;
        let seed = |bounds: &[(&str, &str)]| {
            let declared: Vec<_> = bounds
                .iter()
                .map(|(q, b)| (TVar::empty_named((*q).into()), parse_type(b).unwrap()))
                .collect();
            let mut known: AHashMap<_, _> =
                declared.iter().map(|(q, _)| (q.name.clone(), q.clone())).collect();
            let bounds: Vec<_> =
                declared.iter().map(|(q, b)| (q, b.named_in(&mut known))).collect();
            seed_conjuncts(&known, bounds).map(|()| known)
        };
        for (bounds, q) in [
            (&[("a", "Array<'a>")][..], "a"),
            (&[("a", "fn(x: 'a) -> i64")], "a"),
            (&[("a", "Array<'b>"), ("b", "(i64, 'a)")], "b"),
            (&[("a", "'b"), ("b", "Number"), ("b", "fn() -> 'a")], "b"),
        ] {
            let msg = seed(bounds).unwrap_err().to_string();
            let want = format!("'{q}'s bound reaches '{q}: a bound may not reach");
            assert!(msg.starts_with(&want), "{bounds:?}: {msg}");
        }
        let known = seed(&[("a", "Array<'b>"), ("b", "Number"), ("c", "'a")]).unwrap();
        for q in ["a", "b", "c"] {
            assert_eq!(known[q].cell_constraints().len(), 1, "'{q}");
        }
    }

    /// Whether a bound of a cell that `ft` reaches, by position or through
    /// bounds, reaches that cell.
    fn holds_a_recursive_bound(ft: &super::FnType) -> bool {
        use crate::typ::{TVar, Type, tvar::would_cycle_inner};
        use ahash::AHashSet;
        fn cells(t: &Type, seen: &mut AHashSet<usize>, out: &mut Vec<TVar>) {
            match t {
                Type::TVar(tv) if seen.insert(tv.cell_addr()) => {
                    out.push(tv.clone());
                    for c in tv.cell_constraints() {
                        cells(&c, seen, out)
                    }
                }
                Type::TVar(_) => (),
                Type::Fn(ft) if seen.insert(triomphe::Arc::as_ptr(ft) as usize) => {
                    ft.for_each_type(&mut |t| cells(t, seen, out))
                }
                Type::Fn(_) => (),
                t => t.for_each_child(&mut |c| cells(c, seen, out)),
            }
        }
        let mut reached = vec![];
        ft.for_each_type(&mut |t| cells(t, &mut AHashSet::default(), &mut reached));
        reached.iter().any(|tv| {
            let cell = tv.cell();
            tv.cell_constraints().iter().any(|c| would_cycle_inner(&cell, c))
        })
    }

    /// A nest of quantified signatures is refused or parses to cells no
    /// bound of which reaches its own cell, so every walk of it returns:
    /// its print, and the parse of what encloses it. In these two no bound
    /// names its own variable.
    #[test]
    fn a_nest_parses_to_cells_no_bound_of_which_reaches_its_cell() {
        const PRINTED: &str = "fn<'c: fn<'c: i64>(x0: fn() -> 'a throws i64, q0: 'c) -> \
            i64>() -> fn<'b: 'a>(x0: fn<'a: 'c>(q0: 'a) -> 'b) -> i64";
        const PARSED: &str = "fn<'c: fn<'c: 'z>(q2: 'c) -> fn<'b: i64>(x1: 'z, q1: 'b) -> \
            i64>() -> fn<'b: fn<'a: 'z>(q0: 'a) -> i64>(x1: fn<'z: fn() -> fn<'b: \
            i64>(x2: 'c, q0: 'b) -> i64>(x0: 'b) -> i64 throws 'z) -> i64";
        for src in [PRINTED, PARSED] {
            if let Ok(ft) = parse_fn_type(src) {
                assert!(!holds_a_recursive_bound(&ft), "{src}");
                assert!(!ft.to_string().is_empty());
            }
        }
    }

    /// A nested signature's walk through bounds reads the bounds of its own
    /// quantifiers alone: the bound of an enclosing variable it names, in a
    /// position or through a bound, never brings in the enclosing `'z` under
    /// the name of its own, at any call, whatever order a map holds them in.
    #[test]
    fn a_walk_through_bounds_finds_its_own_quantifier() {
        use crate::typ::{TVar, Type};
        use ahash::AHashMap;
        use arcstr::ArcStr;
        for src in [
            "fn<'b: Array<'z>, 'z: Any>(x: fn<'d: 'z, 'z: i64>(y: 'd) -> 'b, w: 'z) -> 'b",
            "fn<'f: Array<'z>, 'c: 'f, 'z: Any>(x: fn<'d: 'e, 'e: 'z, 'z: i64>(y: 'd, \
             v: 'c) -> i64, w: 'z) -> 'c",
        ] {
            for _ in 0..16 {
                let ft = parse_fn_type(src).unwrap();
                let Type::Fn(nested) = &ft.args[0].typ else { panic!("{ft}") };
                let mut found: AHashMap<ArcStr, TVar> = AHashMap::default();
                nested.collect_tvars_through_bounds(&mut found);
                let bound = found["z"].cell_constraints();
                assert_eq!(
                    bound.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
                    ["i64"]
                );
            }
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

    /// Each signature the bound of a bound of the next one's variable, the
    /// innermost alone naming the outermost's `'z`, by position.
    fn nested_by_bound_of_bound_naming_outer(n: usize) -> String {
        let inner = nested(n, "fn<'a: Any>(x: 'a, y: 'z) -> 'a", |s| {
            format!("fn<'a: 'b, 'b: {s}>(x: 'a) -> 'a")
        });
        format!("fn<'z: Any>(x: {inner}, w: 'z) -> 'z")
    }

    /// Each signature's variable held only through the bound of the next
    /// signature's, which the bound of the variable after it names.
    fn nested_held_through_the_next_bound(n: usize) -> String {
        let leaf = format!("fn<'b: Array<'z{n}>>(y: 'b) -> 'b");
        let inner = (1..=n)
            .rev()
            .fold(leaf, |s, k| format!("fn<'z{k}: Array<'z{}>>(x: {s}) -> i64", k - 1));
        format!("fn<'z0: Any>(m: {inner}) -> i64")
    }

    /// Each signature's variable held only through the bound of the next
    /// signature's, which stands in the bound of a variable of its own.
    fn nested_held_beside_the_next_bound(n: usize) -> String {
        let leaf = format!("fn<'b: Array<'z{n}>>(y: 'b) -> 'b");
        let inner = (1..=n).rev().fold(leaf, |s, k| {
            format!("fn<'y{k}: {s}, 'z{k}: Array<'z{}>>(x: 'y{k}) -> i64", k - 1)
        });
        format!("fn<'z0: Any>(m: {inner}) -> i64")
    }

    fn walk_steps(f: impl FnOnce()) -> usize {
        super::TVAR_WALK_STEPS.with(|c| c.set(0));
        f();
        super::TVAR_WALK_STEPS.with(|c| c.get())
    }

    /// Parsing each shape 160 deep takes under six times the steps 40 deep
    /// does.
    fn assert_parse_in_linear_steps(shapes: &[(&str, fn(usize) -> String)]) {
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
        assert_parse_in_linear_steps(&[
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
        assert_parse_in_linear_steps(&[
            ("naming outer", nested_naming_outer),
            ("bound naming outer", nested_by_bound_naming_outer),
            ("unquantified", nested_naming_a_variable),
        ])
    }

    const HELD_THROUGH_BOUNDS: [(&str, fn(usize) -> String); 2] = [
        ("bound of a bound naming outer", nested_by_bound_of_bound_naming_outer),
        ("held through the next bound", nested_held_through_the_next_bound),
    ];

    // Parsing a nest whose signatures hold variables only through the
    // bounds of their own walks each a bounded number of times.
    #[test]
    fn nests_held_through_bounds_parse_in_linear_steps() {
        assert_parse_in_linear_steps(&HELD_THROUGH_BOUNDS)
    }

    // Outside a parse, which records nothing, reading the variables of a
    // nest held through bounds (its constraints, its cells, printing it,
    // comparing it, aliasing it) walks each signature a bounded number of
    // times.
    #[test]
    fn nests_held_through_bounds_walk_in_linear_steps() {
        use super::FnType;
        use crate::{expr::print::PrettyDisplay, typ::Type};
        let walks: [(&str, fn(&Type, &Type)); 6] = [
            ("constraint_view", |t, _| {
                let Type::Fn(ft) = t else { unreachable!() };
                drop(ft.constraint_view())
            }),
            ("collect_tvars", |t, _| t.collect_tvars(&mut ahash::AHashMap::default())),
            ("print", |t, _| drop(t.to_string())),
            ("pretty", |t, _| drop(t.to_string_pretty(80).to_string())),
            ("compare", |t, u| assert!(t == u && t.cmp(u).is_eq())),
            ("name", |t, _| drop(t.named_in(&mut ahash::AHashMap::default()))),
        ];
        let parse = |src: &str| -> Type {
            let ft: FnType = parse_fn_type(src).unwrap();
            Type::Fn(triomphe::Arc::new(ft))
        };
        for (name, shape) in HELD_THROUGH_BOUNDS {
            for (walk, f) in walks {
                let steps = |n| {
                    let (t, u) = (parse(&shape(n)), parse(&shape(n)));
                    walk_steps(|| f(&t, &u))
                };
                let (a, b) = (steps(8), steps(16));
                assert!(b < a * 3, "{name}, {walk}: 8 deep: {a} steps, 16 deep: {b}");
                let (a, b) = (steps(40), steps(160));
                assert!(b < a * 6, "{name}, {walk}: 40 deep: {a} steps, 160 deep: {b}");
            }
        }
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

    // Printing a nest whose variables are held through a signature in the
    // bound of a variable of their own reads each signature's cells a
    // bounded number of times: a variable's bounds are read in its own
    // signature's scope.
    #[test]
    fn nests_held_beside_a_bound_print_in_linear_steps() {
        use crate::{expr::print::PrettyDisplay, typ::Type};
        let steps = |n| {
            let ft = parse_fn_type(&nested_held_beside_the_next_bound(n)).unwrap();
            let t = Type::Fn(triomphe::Arc::new(ft));
            walk_steps(|| {
                drop(t.to_string());
                drop(t.to_string_pretty(80).to_string())
            })
        };
        let (a, b) = (steps(40), steps(160));
        assert!(b < a * 6, "40 deep: {a} steps, 160 deep: {b}");
    }

    // Comparing a nest whose signatures each name a variable from outside
    // themselves, and stand in the bound of a variable of the one around
    // them, reads each signature's free cells a bounded number of times,
    // under the guards of the walks around it too.
    #[test]
    fn nests_held_in_a_bound_compare_in_linear_steps() {
        use crate::typ::Type;
        let shapes: [(&str, fn(usize) -> String); 2] = [
            ("bound naming outer", nested_by_bound_naming_outer),
            ("held beside the next bound", nested_held_beside_the_next_bound),
        ];
        for (name, shape) in shapes {
            let steps = |n| {
                let parse =
                    || Type::Fn(triomphe::Arc::new(parse_fn_type(&shape(n)).unwrap()));
                let (t, u) = (parse(), parse());
                walk_steps(|| assert!(t == u && t.cmp(&u).is_eq()))
            };
            let (a, b) = (steps(40), steps(160));
            assert!(b < a * 6, "{name}: 40 deep: {a} steps, 160 deep: {b}");
        }
    }

    /// The distinct cells `t` holds under `name`.
    fn cells_named(t: &crate::typ::Type, name: &str) -> ahash::AHashSet<usize> {
        cells_by_depth(t)
            .into_iter()
            .filter(|(n, _, _)| n == name)
            .map(|(_, _, c)| c)
            .collect()
    }

    // A variable a signature holds only through the bound of a bound, its
    // own or one of a signature nested in it, is its declarer's cell, as
    // every other occurrence of its name is, and the nest prints as
    // written.
    #[test]
    fn a_bound_of_a_bound_names_the_declarers_cell() {
        use crate::typ::Type;
        let parse = |src: &str| Type::Fn(triomphe::Arc::new(parse_fn_type(src).unwrap()));
        let held = nested_held_through_the_next_bound(5);
        for src in [
            "fn<'z: Number>(x: fn<'b: 'c, 'c: Array<'z>>(y: 'b) -> 'b, w: 'z) -> 'z",
            "fn<'z: Number>(x: fn<'b: 'c, 'c: Array<'z>>(y: 'b) -> 'b) -> i64",
            "fn(x: fn<'b: 'c, 'c: Array<'z>>(y: 'b, v: 'z) -> 'b) -> 'z",
            "fn(x: fn<'b: 'c, 'c: fn(a: 'q) -> 'q>(y: 'b) -> 'b, k: 'q) -> 'q",
            "fn(m: fn<'z: Array<'a>>(x: fn<'b: 'c, 'c: Array<'z>>(y: 'b) -> 'b) -> i64, \
             k: 'a) -> 'a",
            "fn<'a: Any>(m: fn<'z: Array<'a>>(x: fn<'b: Array<'z>>(y: 'b) -> 'b) -> i64) -> i64",
            "fn<'a: Any>(m: fn<'z: Array<'a>>(x: fn<'b: 'c, 'c: Array<'z>>(y: 'b) -> 'b) -> \
             i64) -> i64",
            "fn(k: fn<'a: Any>(m: fn<'z: Array<'a>>(x: fn<'b: Array<'z>>(y: 'b) -> 'b) -> \
             i64) -> i64) -> i64",
            "fn(k: fn<'a: Any>(m: fn<'z: Array<'a>>(x: fn<'b: Array<'z>>(y: 'b) -> 'b) -> \
             i64, w: 'a) -> 'a) -> i64",
            "fn(@args: `V(List<'e>)) -> fn<'d: 'a, 'z: Number + Array<'d>>(x0: `V('z), \
             qz: 'z) -> 'a",
            held.as_str(),
        ] {
            let t = parse(src);
            let mut names =
                cells_by_depth(&t).into_iter().map(|(n, _, _)| n).collect::<Vec<_>>();
            names.sort();
            names.dedup();
            for name in names {
                let cells = cells_named(&t, &name);
                assert_eq!(cells.len(), 1, "{src}: '{name} is {} cells", cells.len());
            }
            assert_eq!(t.to_string(), src);
        }
        let src = nested_by_bound_of_bound_naming_outer(5);
        let t = parse(&src);
        assert_eq!(cells_named(&t, "z").len(), 1, "{src}");
        assert_eq!(t.to_string(), src);
    }

    // A bound that reaches its own variable through the bound of a bound of
    // a signature nested in it is refused, as one through the nested
    // signature's own bound is.
    #[test]
    fn a_bound_reaching_its_variable_through_a_bound_of_a_bound_is_refused() {
        for src in [
            "fn<'a: fn<'d: Array<'a>>(qd: 'd) -> 'd>(qa: 'a) -> i64",
            "fn<'a: fn<'d: 'z, 'z: Array<'a>>(qd: 'd) -> 'd>(qa: 'a) -> i64",
        ] {
            let e = format!("{:#}", parse_fn_type(src).unwrap_err());
            assert!(e.contains("a quantifier whose bound reaches it"), "{src}: {e}");
        }
    }

    // The bound of a variable held only through a nested signature's bound
    // names its declarer's scope, never a variable of the nested signature
    // that shadows a name in it.
    #[test]
    fn a_bound_held_through_a_nested_signature_keeps_its_scope() {
        use crate::typ::Type;
        let src = "fn<'q: Any>(m: fn<'z: Array<'q>>(x: fn<'q: 'z>(a: 'q) -> 'q) -> i64, \
                   w: 'q) -> 'q";
        let ft = parse_fn_type(src).unwrap();
        let Type::TVar(outer) = &ft.rtype else { panic!("{ft}") };
        let Type::Fn(middle) = &ft.args[0].typ else { panic!("{ft}") };
        let Type::Fn(inner) = &middle.args[0].typ else { panic!("{ft}") };
        let Type::TVar(inner_q) = &inner.args[0].typ else { panic!("{ft}") };
        let Type::TVar(z) = &inner_q.cell_constraints()[0] else { panic!("{ft}") };
        let Type::Array(elt) = &z.cell_constraints()[0] else { panic!("{ft}") };
        let Type::TVar(bound_q) = &**elt else { panic!("{ft}") };
        assert_eq!(bound_q.cell_addr(), outer.cell_addr(), "{ft}");
        assert_ne!(inner_q.cell_addr(), outer.cell_addr(), "{ft}");
        assert_eq!(ft.to_string(), src);
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
    // in a bound, or the bound of a bound, is not closed: the variable is
    // the enclosing signature's cell.
    #[test]
    fn a_bound_naming_an_outer_variable_is_the_outer_cell() {
        for inner in [
            "fn<'b: Array<'z>>(y: 'b) -> 'b",
            "fn<'b: Any>(y: fn<'c: Array<'z>>(v: 'c) -> 'c) -> 'b",
            "fn<'b: 'c, 'c: Array<'z>>(y: 'b) -> 'b",
            "fn<'b: Any>(y: fn<'c: 'd, 'd: Array<'z>>(v: 'c) -> 'c) -> 'b",
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
    // after a cell write: a conjunct added to its quantifier's cell is seen.
    #[test]
    fn a_cell_write_drops_the_free_cells_read_before_it() {
        use crate::typ::{TVar, Type};
        let ft = parse_fn_type("fn(x: fn<'b: Any>(y: 'b, q: 'z) -> 'b) -> 'z").unwrap();
        let Type::Fn(nested) = &ft.args[0].typ else { panic!("{ft}") };
        let _scope = super::PartsDepth::enter();
        let mut before = ahash::AHashMap::default();
        ft.args[0].typ.collect_tvars(&mut before);
        assert!(before.contains_key("z") && !before.contains_key("w"));
        let mut own = None;
        nested.each_quantifier(|_, tv| own = Some(tv.clone()));
        own.unwrap().add_cell_constraint(Type::TVar(TVar::empty_named("w".into())));
        let mut after = ahash::AHashMap::default();
        ft.args[0].typ.collect_tvars(&mut after);
        assert!(after.contains_key("w"));
    }

    // A refused entry leaves the walk that holds the key holding it: every
    // later entry is refused until that walk ends.
    #[test]
    fn a_refused_entry_leaves_the_key_held() {
        use super::{CONSTRAINING, Walking};
        let key = 8;
        let held = Walking::enter_key(&CONSTRAINING, key).unwrap();
        assert!(Walking::enter_key(&CONSTRAINING, key).is_none());
        assert!(Walking::enter_key(&CONSTRAINING, key).is_none());
        drop(held);
        assert!(Walking::enter_key(&CONSTRAINING, key).is_some());
    }

    // The held walk reads no bound of a cell whose bounds it is reading
    // already, whichever signature holds the cell: a normalized copy of a
    // signature is another signature holding the same cells.
    #[test]
    fn a_held_walk_cuts_a_cell_whose_bounds_it_is_reading() {
        use super::{FnType, HOLDING, Walking, collect_held};
        use crate::typ::Type;
        let ft = parse_fn_type("fn<'b: 'c, 'c: Array<'q>>(y: 'b) -> 'b").unwrap();
        let Type::TVar(b) = &ft.args[0].typ else { panic!("{ft}") };
        let Type::TVar(c) = &b.cell_constraints()[0] else { panic!("{ft}") };
        let mut held = ahash::AHashMap::default();
        collect_held(&ft, &mut held);
        assert!(held.contains_key("c") && held.contains_key("q"), "{held:?}");
        let copy: FnType = ft.clone();
        let _reading = Walking::enter_key(&HOLDING, c.cell_addr()).unwrap();
        let mut cut = ahash::AHashMap::default();
        collect_held(&copy, &mut cut);
        assert!(cut.contains_key("c") && !cut.contains_key("q"), "{cut:?}");
    }

    // The held walk reads a signature's positions in its own name scope: an
    // unquantified signature in a position never passes the bound of an
    // enclosing cell it names, so the enclosing `'d` that bound names is
    // never taken for the nested signature's own `'d`.
    #[test]
    fn a_held_walk_reads_its_positions_in_its_own_name_scope() {
        use super::collect_held;
        use crate::typ::Type;
        let ft = parse_fn_type(
            "fn<'w: Array<'d>, 'd: Array<'u>, 'u: Any, 'v: Any>(x: fn<'d: Array<'v>>(y: \
             fn(q: 'w) -> i64, z: 'd) -> 'd, a: 'w, b: 'v) -> 'd",
        )
        .unwrap();
        let Type::Fn(nested) = &ft.args[0].typ else { panic!("{ft}") };
        let mut held = ahash::AHashMap::default();
        collect_held(nested, &mut held);
        let mut names: Vec<_> = held.keys().map(|n| n.as_str()).collect();
        names.sort_unstable();
        assert_eq!(names, ["d", "v", "w"]);
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
        let folded = ft.name_auto_variables(&[]);
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

    /// A join of a join reaches the definitions the inner join reached
    /// after the inner join is gone.
    #[test]
    fn a_join_outlives_the_join_it_joins() {
        use super::LambdaIds;
        use crate::LambdaId;
        let defs: [LambdaIds; 3] = std::array::from_fn(|_| {
            let node = LambdaIds::default();
            node.set_id(LambdaId::new());
            node
        });
        let inner = LambdaIds::joining(&[&defs[0], &defs[1]]);
        let outer = LambdaIds::joining(&[&inner, &defs[2]]);
        drop(inner);
        let ids = outer.ids();
        assert!(defs.iter().all(|d| d.own().is_some_and(|id| ids.contains(&id))));
        assert!(defs.iter().all(|d| d.ids().len() == 1));
    }

    /// An implicit throws slot (an unbound auto TVar) is not printed.
    #[test]
    fn unbound_auto_throws_is_hidden() {
        let ft = parse_fn_type("fn(a: Array<'a>) -> i64").unwrap();
        let folded = ft.name_auto_variables(&[]);
        let resolved = folded.resolve_tvars();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(resolved)));
        assert!(!s.contains("throws"), "unbound auto throws should not appear, got: {s}");
    }

    /// An explicit `throws T` is always printed, even for `Bottom` or
    /// an auto TVar.
    #[test]
    fn explicit_throws_always_shown() {
        let ft = parse_fn_type("fn(x: 'a) -> 'a throws `Boom").unwrap();
        let s = format!("{}", crate::typ::Type::Fn(triomphe::Arc::new(ft)));
        assert!(s.contains("throws"), "explicit throws should be shown, got: {s}");
        assert!(s.contains("`Boom"), "throws variant should be printed, got: {s}");
    }

    /// A read's copy, of a typedef application (copied, or rebuilt around
    /// a function type it holds) as of a function type, is one copy to
    /// every read through the same copies, and its own to a later read,
    /// alone or held in a node the later read meets first.
    #[test]
    fn a_read_copy_is_its_own_to_a_later_read() {
        use crate::{
            expr::parser::parse_type,
            typ::{
                FnType, ReadCopies, Type, node_addr,
                tvar::{AtLevel, Level},
            },
        };
        use nohash::IntSet;
        let _generic = AtLevel::enter(Level::GENERIC);
        let open = IntSet::default();
        let keep = |_: &FnType| false;
        for src in ["F<'a>", "F<'a, i64>", "F<fn(x: 'a) -> 'a>", "fn(x: 'a) -> 'a"] {
            let t = parse_type(src).unwrap();
            let mut copies = ReadCopies::default();
            let copy = copies.read(&t, &open, &keep).expect(src);
            let again = copies.read(&t, &open, &keep).expect(src);
            assert_eq!(node_addr(&copy), node_addr(&again), "{src}: {copy}, {again}");
            assert!(copies.read(&copy, &open, &keep).is_none(), "{src}: {copy}");
            let held = Type::Array(triomphe::Arc::new(copy.clone()));
            assert!(copies.read(&held, &open, &keep).is_none(), "{src}: {held}");
        }
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
    fn go(&mut self, t: &Type) -> Option<Type> {
        crate::stack::ensure_sufficient(|| {
            let addr = match t {
                Type::TVar(tv) => Some(tv.cell_addr()),
                t => super::node_addr(t),
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
                    Some(Type::Fn(Arc::new(fresh)).named_in(&mut LPooled::take()))
                }
                Type::Ref(_) if t.copied_outside_functions(self.open) => {
                    let how = self.how();
                    let fresh = t
                        .reset_tvars_int(&mut self.copies.known, how)
                        .unwrap_or_else(|| t.clone());
                    Some(fresh.named_in(&mut LPooled::take()))
                }
                t => t.cow_children(&mut |c| self.go(c)),
            };
            if let Some(a) = addr {
                self.copies.memo.insert(a, (t.clone(), r.clone()));
            }
            if let Some(c) = &r
                && let Some(a) = super::node_addr(c)
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
