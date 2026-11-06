use crate::{
    AddrMap, AddrSet, LambdaId, PRINT_FLAGS, PrintFlag,
    env::Env,
    expr::{
        ModPath,
        print::{PrettyBuf, PrettyDisplay},
    },
    format_with_flags, image,
    typ::{
        Bound, Mutability, TVar, TVarsSeen, Type,
        contains::{ContainsFlags, ContainsHist},
        key_text,
        matches::{MatchHist, SigMatch, SigPairs},
        normalize::NormCx,
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
    collections::VecDeque,
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

    pub fn link(&self, other: &LambdaIds) {
        self.0.write().links.insert(other.as_link());
        other.0.write().links.insert(self.as_link());
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
    /// For the outermost `for_each_part` call: each nested signature's
    /// position cells, keyed by the signature it pins, so a walk collects
    /// each signature's once rather than once per signature above it.
    static POSITIONS: RefCell<AddrMap<(Arc<FnType>, Vec<(ArcStr, TVar)>)>> =
        RefCell::new(AddrMap::default());
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
            POSITIONS.with_borrow_mut(|m| m.clear());
        }
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
        set.with_borrow_mut(|s| s.insert(key)).then_some(Walking { set, key })
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
    let constraints: LPooled<Vec<(&TVar, Type)>> = constraints.into_iter().collect();
    for (_, tc) in constraints.iter() {
        tc.alias_tvars(known);
    }
    for (tv, tc) in constraints.iter() {
        known[&tv.name].add_cell_constraint(tc.normalize());
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
    /// quantifier's, whose cell holds the conjuncts.
    pub fn declaring(mut self, constraints: &[(TVar, Type)]) -> Self {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        let mut names: LPooled<Vec<ArcStr>> = LPooled::take();
        for (tv, _) in constraints.iter() {
            if !known.contains_key(&tv.name) {
                names.push(tv.name.clone());
                known.insert(tv.name.clone(), tv.clone());
            }
        }
        self.quantifiers = Arc::from_iter(names.drain(..));
        self.alias_tvars(&mut known);
        seed_conjuncts(&mut known, constraints.iter().map(|(tv, tc)| (tv, tc.clone())));
        self
    }

    /// The tvar cells reachable from args / vargs / rtype / throws,
    /// by name.
    pub(crate) fn sig_tvars(&self) -> LPooled<AHashMap<ArcStr, TVar>> {
        let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        self.for_each_type(&mut |t| t.collect_tvars(&mut known));
        known
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
        self.alias_tvars_in(known, &tvar::ConjunctsPassed::take())
    }

    pub(super) fn alias_tvars_in(
        &self,
        known: &mut AHashMap<ArcStr, TVar>,
        passed: &tvar::ConjunctsPassed,
    ) {
        self.for_each_part_passing(&mut |tv| passed.first(tv), &mut |t, _| {
            t.alias_tvars_in(known, passed)
        })
    }

    /// A reference's copy of the scheme (`Fresh::Scheme`) under the open
    /// gates `open`.
    #[doc(hidden)]
    pub fn scheme(&self, open: &IntSet<LambdaId>) -> Self {
        self.reset_tvars_int(&mut LPooled::take(), Fresh::Scheme(open))
    }

    // CR claude for claude: [readability/low] One instance keeps its scheme's
    // quantifier list, so a checker message prints a one-instance function type with a
    // quantifier header, which reads as generic and, written back as an annotation, is
    // refused by QUANTIFIED_FN: `let fs = array::map([1], |r| 'a: Number |x: 'a| -> 'a
    // x); let y = never(); y <- fs; y <- 1` says `y is Array<fn<'a: Number>(x: 'a ...)
    // -> 'a> and cannot hold i64`. The list stays because the builtin unread settle
    // (CallSite::settle_unread) reads quantifier names; printing could drop the header
    // of a type whose quantifiers are not generic cells instead. probe:
    // ~/ptmp/cr29/scratch/r16-Q/hint1.gx, hint2.gx (instance-quantifier-print-01)
    // 2026-11-05 claude: A second face of the same kept quantifier: in a module task,
    // two calls through a one-instance function a parent binding's projection gives
    // (main.gx `let fs = array::map([1], |r| 'a: Number |x: 'a| -> 'a x + x); mod m;`,
    // m.gx `let g = super::fs[0]$; let go = (g(1), g(1.5))`, m.gxi `val go: (i64,
    // f64)`) are refused, as the ML rule wants, but the message is "val go is declared
    // (i64, f64) but implemented ('_7814: unbound within Number & Singleton, ..)",
    // naming open cells and not fs: has_earlier_quantifier compares fs's kept
    // quantifier as a fresh copy in the module task, so each call's result stays open.
    // The sibling forms name fs ("annotate fs"). Whether dropping the list from one
    // instance (this fix) makes the calls decide the copy and name fs is untried. probe
    // ~/ptmp/cr29/scratch/review-r16/Q-r2-2/mq1/main.gx (run.sh mq1/main.gx).
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
            t.quantifiers_seen(&mut seen, &mut |_, tv| {
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

    // CR claude for claude: [bug/low] Which member of a union of two open cells (`x: ['a,
    // 'b]`) takes what is decided by an order, not a rule, and two orders exist: this
    // containment holds such a position after every other (instance_sig, a written
    // function type), while a call site's check relates its arguments first, deciding
    // `'a` from the argument. Under a generic caller the two disagree: `let f = |x: ['a,
    // 'b]| -> (Array<'a>, Array<'b>) ([], []); let g = |p, q| { let u = never(); let r =
    // f(u); u <- p; u <- q; r }; g(1, "s")` checks and runs by default (the instance is
    // substituted), and GRAPHIX_NO_SUBST=1 refuses it ("(Array<i64>, Array<string>)
    // does not contain (Array<'a: [i64, string]>, Array<'b: unbound>)"); main a084f9887
    // refuses the build in both. At the top level `let r: (Array<i64>, Array<string>) =
    // f(u)` with `u: [i64, string]` is refused by the call's check. The default path is
    // sound (a substituted instance is a valid one). Options: (a) the call site defers
    // an argument whose formal holds such a union to the statement's settle, after the
    // result's context relates (one order everywhere); (b) keep both orders: the
    // default path stays sound and GRAPHIX_NO_SUBST agrees on the pins only. Recommend
    // (a). probes: ~/ptmp/cr29/scratch/r6-B/w5/fw1.gx, fw6.gx (formal-union-order-01)
    // 2026-11-03 claude: Eric ruled (2026-11-03): D8 c in ~/ptmp/cr-discuss.md, under
    // his rule for type holes (reject and require an annotation, then a --check census:
    // it comes back to him only if the stdlib, the book's examples or netidx-admin would
    // need a new annotation): with D7's rule an argument part undecided at the call's
    // end is refused, so both orders refuse fw1 alike (annotate the let).
    // 2026-11-04 claude: Round 15 lane R built D7 (b) + D8 (c) on branch cr50-R-parked
    // (46a8eba9e..544862d33) and was parked: D7 held up (no argument order gives two
    // values), but D8's local rule failed review three times in new ways (a union not
    // widenable; then variant spellings; then, after judging per argument, invariant
    // positions such as Map<K, ['a, 'b]> and Error<['a, 'b]>). Main shares the remaining
    // splits: a generic parameter at the union beside a deciding sibling, a call's result
    // union decided by later writes, a callback parameter's union. Back to Eric as the
    // D7/D8 follow-up in ~/ptmp/cr29/next-discuss.md; probes
    // ~/ptmp/cr29/scratch/review-r15/R-r3-{1,2}/.
    // 2026-11-05 claude: Eric ruled (2026-11-05) on the D7/D8 follow-up: (c) then (b).
    // D8's part of cr50-R is dropped; after D7 lands, a written signature may not hold a
    // union of two or more bare type variables of its definition (also under
    // constructors), refused naming the annotation, census first. This CR stays open for
    // the inferred result-union split ((a), deferring, is dropped).
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
        positions.push(Some((&self.rtype, &t.rtype, flags)));
        positions.push(None);
        positions.push(Some((&self.throws, &t.throws, flags)));
        // a union of open cells is decided by the positions that decide
        // its members alone, wherever they are written: each position's
        // round is fixed before any position binds a cell
        let later: SmallVec<[bool; 8]> = positions
            .iter()
            .map(|p| {
                p.is_some_and(|(c, t, _)| holds_open_union(c) || holds_open_union(t))
            })
            .collect();
        for round in [false, true] {
            let in_round =
                positions.iter().zip(later.iter()).filter(|(_, l)| **l == round);
            for (p, _) in in_round {
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
        }
        Ok(true)
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
        for (s, t) in self.align(t).0 {
            t.typ.contains_int(ContainsFlags::Commit.into(), env, &mut hist, &s.typ)?;
        }
        Ok(())
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
        let impl_tvs = sorted_tvars(impl_fn.sig_tvars().drain());
        // every cell the walk may have mapped: an unannotated parameter's
        // is bound, and the cells its binding holds carry the conjuncts
        let impl_cells = {
            let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            // every cell, a variable named only inside another's bound too
            impl_fn
                .for_each_part(&mut |t, _| super::settle::position_cells(t, &mut cells));
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
        for (sig_tv, sig_tc) in self.constraint_view().iter() {
            if *sig_tc == Type::Any {
                continue;
            }
            for (addr, t) in tvar_map.paired_since(indexed) {
                if let Type::TVar(s) = t
                    && let Some(i) = place.get(&addr)
                {
                    paired_to.entry(s.cell_addr()).or_default().push(*i);
                }
            }
            indexed = tvar_map.paired_len();
            // the impl cells the signature's was matched with, else the
            // same-named variable: either form of a bound (`'c: C`, `c: C`)
            // names its variable its own way
            let mut matched: LPooled<Vec<&TVar>> =
                match paired_to.get_mut(&sig_tv.cell_addr()) {
                    None => LPooled::take(),
                    Some(places) => {
                        places.sort_unstable();
                        places.iter().map(|i| &impl_cells[*i].1).collect()
                    }
                };
            if matched.is_empty()
                && let Ok(i) = impl_tvs.binary_search_by(|(n, _)| n.cmp(&sig_tv.name))
            {
                matched.push(&impl_tvs[i].1);
            }
            for impl_tv in matched.iter() {
                for c in impl_tv.cell_constraints().iter() {
                    if tvar_map.trial(|m| sig_tc.sig_matches_int(env, c, m, hist)).is_ok()
                    {
                        break;
                    }
                }
            }
        }
        let mut param_bounds = LPooled::take();
        self.for_each_part(&mut |t, _| param_bounds_of(env, t, &mut param_bounds));
        // A call through the signature is checked by the signature alone:
        // every conjunct of every impl cell must admit the signature's
        // concrete choice, or follow from the conjuncts of the signature
        // variable it stands for.
        // the signature's variables held rigid while `declared` judges, from
        // its first call to the next judgment of another kind
        let sig_rigid: RefCell<Option<LPooled<Vec<RigidGate>>>> = RefCell::new(None);
        let release_sig = || drop(sig_rigid.borrow_mut().take());
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
            // the conjunct in the signature's terms, whose variables are
            // each caller's to pick
            let in_sig = impl_tc.swap_cells(&|a| tvar_map.sig_type(a).cloned());
            sig_rigid.borrow_mut().get_or_insert_with(|| {
                let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
                self.for_each_part(&mut |t, _| {
                    super::contains::hold_rigid(t, &mut gates)
                });
                gates
            });
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
        for (_, tv) in impl_cells.iter() {
            match tvar_map.sig_type(tv.cell_addr()) {
                None => (),
                Some(Type::TVar(sig_tv)) => {
                    for impl_tc in tv.cell_constraints() {
                        declared(sig_tv, &impl_tc)?
                    }
                }
                Some(sig_type) => {
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
                    release_sig();
                    for impl_tc in tv.cell_constraints() {
                        let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
                        // the signature's variables are every caller's to
                        // pick: they fit only through their own conjuncts
                        super::contains::hold_rigid(sig_type, &mut gates);
                        let ok = impl_tc
                            .contains_with_flags(
                                ContainsFlags::RigidCheck.into(),
                                env,
                                sig_type,
                            )
                            .unwrap_or(false);
                        drop(gates);
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

// CR claude for claude: [perf/low] Nested rank-2 function types print in
// time quadratic in their nesting: `let y: fn<'a: Any>(x: fn<'a: Any>(x: ..)
// -> 'a) -> 'a = never()` N levels deep costs 0.037 / 0.143 / 0.446 / 1.56 /
// 5.82 G instructions under `graphix fmt --stdout` at N = 14 / 40 / 80 / 160
// / 320. format::layout, format::guard and this call FnType::constraint_view
// at every level, and each call is collect_tvars over the whole subtree (80%
// of the run at 160). The nesting limit bounds it near 10-15 G; it is the
// only one of 70 nested-syntax shapes the review measured that is not flat.
// Main 968fa9bc0 the same. Found by review r14 of lane K (round 3). probe:
// ~/ptmp/cr29/scratch/review-r14/K-r2-1/q/fnqtyp_320.gx (genq.py fnqtyp N
// OUT), cg/fnq_160.out.gz (rank2-print-01)
// 2026-11-05 claude: Lane Q (cr51-Q): written nested quantified function types
// are now refused by the check (QUANTIFIED_FN at Type::rewrite_trait_args), so
// fnqtyp_320 no longer type-checks, but graphix fmt formats without checking
// and still reaches this printer path at the same cost; an inferred nest (a
// lambda declaring a variable, returned by a lambda declaring one, ...) also
// prints nested headers. The CR stands for the formatter.
/// The quantifiers a signature prints: the declared ones, minus the
/// receiver `self` (implied by its trait) and the compiler-minted
/// `#arg` quantifiers of traits written in argument position (those
/// print as the trait at the argument). One entry per conjunct, a
/// variable's conjuncts adjacent.
fn printed_quantifiers(ft: &FnType) -> LPooled<Vec<(TVar, Type)>> {
    let mut v = ft.constraint_view();
    v.retain(|(tv, _)| &*tv.name != "self" && !tv.name.starts_with('#'));
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
            Some(_) => write!(f, ", '{}: {t}", tv.name)?,
            None => write!(f, "'{}: {t}", tv.name)?,
        }
    }
    Ok(())
}

// CR claude for claude: [readability/low] Printing under PrintFlag::DerefTVars
// prints a parameter's binding in place, so a cell of the quantifier's name the binding
// holds prints as the quantifier: `'a: Number |y, x: 'a| -> 'a { let z = array::len(y);
// x }` passed for `k: fn<'c: Any>(y: Array<i64>, x: 'c) -> 'c` is refused with "does
// not contain fn<'a: Number>(y: Array<'a: i64>, x: 'a: unbound within Number)", the
// `'a: i64` being array::len's copied 'a, not the quantifier. Type::written_form inlines
// bindings the same way. The walks rename the quantifier apart (tvar::keep_quantifiers,
// resolve-capture-01); the printer could do the same over the printed signature. probe:
// ~/ptmp/cr29/scratch/r17-hk/s6.gx (print-capture-01)
impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                            write!(buf, "'{}: ", tv.name)?;
                        }
                        None => write!(buf, "'{}: ", tv.name)?,
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

/// Whether `t` holds a union two of whose members are open cells, which
/// a containment of it decides only through their other positions.
fn holds_open_union(t: &Type) -> bool {
    fn go(t: &Type, seen: &mut AHashSet<usize>) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::TVar(tv) => match tv.binding() {
                Some(b) => seen.insert(tv.cell_addr()) && go(&b, seen),
                None => false,
            },
            Type::Set(ms)
                if ms
                    .iter()
                    .filter(|m| matches!(m, Type::TVar(tv) if tv.open_cell().is_some()))
                    .nth(1)
                    .is_some() =>
            {
                true
            }
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
