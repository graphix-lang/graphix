use crate::{
    AddrMap, AddrSet,
    expr::WrittenAt,
    stack::ensure_sufficient,
    typ::{
        AbstractId, Mutability, NodeKey, TVar, Type,
        setops::{Welds, apart, strictly_identical, union_collapses, written_identical},
    },
};
use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    hash::{DefaultHasher, Hash, Hasher},
    iter,
    mem::{self, Discriminant},
};
use triomphe::Arc;

/// Per-pass [`Type::normalize`] state.
pub(super) struct NormCx {
    pub(super) visited: Visited,
    /// The live cells the pass's collapses paired.
    welds: Welds,
    /// What a welding pass visited inside function types.
    signatures: Option<Visited>,
}

/// What a pass visited: `cells` are the visited TVar cells (the first
/// visit normalizes the binding in place); `memo` is a cache of each
/// shared node's result ([`Type::part_key`]). Each entry pins the keyed
/// type: an in-place rebinding frees the old binding, and its address
/// must not be reused under its key.
pub(super) struct Visited {
    pub(super) cells: LPooled<AddrSet>,
    memo: LPooled<AHashMap<NodeKey, (Type, Option<Type>)>>,
    flat: FlatCx,
}

impl Visited {
    fn take() -> Self {
        Self { cells: LPooled::take(), memo: LPooled::take(), flat: FlatCx::default() }
    }
}

#[doc(hidden)]
pub type NormKey = (Discriminant<Type>, usize, usize);

impl NormCx {
    pub(super) fn take() -> Self {
        Self { visited: Visited::take(), welds: Welds::new(), signatures: None }
    }

    /// `f` over a function type's signature. A union standing there is
    /// the function's own, which its calls decide: a welding pass keeps its
    /// members as they stand, visiting what the signature reaches apart
    /// from what it welds outside.
    fn in_signature<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        if !matches!(self.visited.flat.members, Members::Welding) {
            return f(self);
        }
        let mut inner = self.signatures.take().unwrap_or_else(Visited::take);
        mem::swap(&mut self.visited, &mut inner);
        let r = f(self);
        mem::swap(&mut self.visited, &mut inner);
        self.signatures = Some(inner);
        r
    }
}

/// A flattening's (or a normalize pass's) memo of the nested unions its
/// merges compare, keyed by the address of their members, each entry
/// pinning its key: a union shared by many payloads is flattened once.
#[derive(Default)]
struct FlatCx {
    flat: Option<LPooled<AHashMap<usize, (Arc<[Type]>, Type)>>>,
    flattened: usize,
    members: Members,
    alts: SmallVec<[Type; 4]>,
}

/// How a flattening tells that two members of a union are one.
#[derive(Default, Clone, Copy)]
enum Members {
    /// Identical, cell by cell ([`strictly_identical`]): nothing merges
    /// the cells two function types would pair, so the two stay apart.
    #[default]
    AsTheyStand,
    /// As the source writes them, a variable by its name
    /// ([`written_identical`]).
    Written,
    /// Function types pair their cells as a renaming
    /// ([`union_collapses`]): the union that asked merges the live pairs.
    Welding,
}

impl FlatCx {
    /// Whether a union keeps `t0` in place of `t1`.
    fn collapses(&self, t0: &Type, t1: &Type, welds: &mut Welds) -> bool {
        match self.members {
            Members::AsTheyStand => strictly_identical(t0, t1),
            Members::Written => written_identical(t0, t1),
            Members::Welding => {
                let start = welds.len();
                let same = union_collapses(t0, t1, welds);
                if same && apart(&self.alts, t0, t1) && !strictly_identical(t0, t1) {
                    welds.truncate(start);
                    return false;
                }
                same
            }
        }
    }

    fn flat(&mut self, t: &Type) -> Type {
        let Type::Set(s) = t else { return t.clone() };
        let key = (**s).as_ptr() as usize;
        if let Some((_, f)) = self.flat.as_ref().and_then(|m| m.get(&key)) {
            return f.clone();
        }
        self.flattened += 1;
        let f = Type::flatten_set_with(s.iter().cloned(), self);
        self.flat.get_or_insert_with(LPooled::take).insert(key, (s.clone(), f.clone()));
        f
    }
}

/// Per-pass [`Type::resolve_tvars`] state: `done` holds completed
/// snapshots of bound cells, `fresh` the one fresh mint per unbound
/// source cell (preserving alias topology), `in_progress` the cycle
/// guard, `memo` a cache of each shared node's result
/// ([`Type::part_key`]).
pub(super) struct ResolveTvarsCx {
    done: LPooled<AddrMap<Type>>,
    fresh: LPooled<AddrMap<TVar>>,
    in_progress: LPooled<AddrSet>,
    memo: LPooled<AHashMap<NodeKey, Option<Type>>>,
    /// An open cell's copy keeps its predicate conjuncts (`Concrete`,
    /// `Function`), which name no cell.
    predicates: bool,
    /// An open inferred cell keeps its bounds, resolved,
    /// as a quantifier's copy does.
    bounded: bool,
    /// The open quantifiers of the function types met: their bounds are
    /// part of the type (`fn<'a: Function>(..)`), so a copy keeps them all.
    pub(super) quantifiers: LPooled<AddrSet>,
    /// How many copies of open cells the walk has made or met.
    opened: usize,
}

impl ResolveTvarsCx {
    pub(super) fn take() -> Self {
        Self {
            done: LPooled::take(),
            fresh: LPooled::take(),
            in_progress: LPooled::take(),
            memo: LPooled::take(),
            predicates: false,
            bounded: false,
            quantifiers: LPooled::take(),
            opened: 0,
        }
    }
}

impl TVar {
    /// The cell's binding resolved ([`Type::resolve_tvars`]) when the cell
    /// is proved to reach no open cell, else `None`; the cell keeps it while
    /// the proof stands, so it is resolved once however many ask.
    pub fn ground_resolution(&self) -> Option<Type> {
        self.ground_resolved(|b| {
            let mut cx = ResolveTvarsCx::take();
            let r = b.resolve_tvars_seen_int(&mut cx).unwrap_or_else(|| b.clone());
            (r, cx.opened == 0)
        })
    }
}

#[cfg(test)]
thread_local! {
    /// The nodes a normalize pass walked: those its memo did not hold.
    pub(super) static NORMALIZED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// The nodes a resolution walked: those its memo did not hold.
    static RESOLVED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// A composite's identity: its kind and its content allocation's address,
/// and a variant's tag's too, since two tags may wrap one payload
/// allocation. `None` for every other kind.
#[doc(hidden)]
pub fn norm_key(t: &Type) -> Option<NormKey> {
    let d = std::mem::discriminant(t);
    match t {
        Type::Set(a) | Type::Tuple(a) => Some((d, (**a).as_ptr() as usize, 0)),
        Type::Struct(a) => Some((d, (**a).as_ptr() as usize, 0)),
        Type::Fn(a) => Some((d, &**a as *const _ as usize, 0)),
        Type::Array(a) | Type::List(a) | Type::Error(a) => {
            Some((d, &**a as *const Type as usize, 0))
        }
        Type::ByRef(m, a) => Some((d, &**a as *const Type as usize, *m as usize)),
        Type::Map(m) => Some((d, &**m as *const _ as usize, 0)),
        Type::Variant(tag, ts, _) => {
            Some((d, (**ts).as_ptr() as usize, tag.as_ptr() as usize))
        }
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::Abstract { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Bound(_)
        | Type::Ref(_)
        | Type::TVar(_) => None,
    }
}

/// What a member can merge with beyond the unkeyed members: a variant
/// only with one of its tag and arity, a struct only with one of its
/// field names, a reference only with one to its definition (by its
/// written name while its cell is open) and an abstract type only with
/// itself, each of its arity, and an open cell only with itself, where the
/// source does not name its variables (two of one name are one cell then).
#[derive(PartialEq, Eq, Hash)]
enum MergeKey {
    Cell(usize),
    Variant(ArcStr, usize),
    Struct(u64, usize),
    Ref(u64, usize),
    Abstract(AbstractId, usize),
}

/// The open cells a product's first parts are, by address (0 where a part
/// is no open cell): two products with different cells at one position never
/// merge, since two distinct open cells are two members, so a merge is
/// tried only past this test.
#[derive(Default, Clone, Copy)]
struct CellSig([usize; 4]);

impl CellSig {
    fn of(t: &Type, members: &Members) -> Self {
        let mut sig = Self::default();
        if matches!(members, Members::Written) {
            return sig;
        }
        let mut set = |parts: &mut dyn Iterator<Item = &Type>| {
            for (slot, p) in sig.0.iter_mut().zip(parts) {
                if let Type::TVar(tv) = p
                    && let Some(c) = tv.open_cell()
                {
                    *slot = c.cell_addr()
                }
            }
        };
        match t {
            Type::Tuple(ts) | Type::Variant(_, ts, _) => set(&mut ts.iter()),
            Type::Struct(fs) => set(&mut fs.iter().map(|(_, t, _)| t)),
            _ => (),
        }
        sig
    }

    fn clashes(&self, other: &Self) -> bool {
        self.0.iter().zip(other.0).any(|(a, b)| *a != 0 && b != 0 && *a != b)
    }
}

impl MergeKey {
    fn of(t: &Type, members: &Members) -> Option<Self> {
        match t {
            Type::TVar(tv) if !matches!(members, Members::Written) => {
                tv.open_cell().map(|c| Self::Cell(c.cell_addr()))
            }
            Type::Variant(tag, args, _) => Some(Self::Variant(tag.clone(), args.len())),
            Type::Struct(fields) => {
                let mut h = DefaultHasher::new();
                fields.iter().for_each(|(n, _, _)| n.hash(&mut h));
                Some(Self::Struct(h.finish(), fields.len()))
            }
            Type::Ref(tr) => {
                let mut h = DefaultHasher::new();
                match tr.def_key() {
                    Some(k) => k.hash(&mut h),
                    None => (&tr.scope, &tr.name).hash(&mut h),
                }
                Some(Self::Ref(h.finish(), tr.params.len()))
            }
            Type::Abstract { id, params } => Some(Self::Abstract(*id, params.len())),
            _ => None,
        }
    }
}

/// A union's merge-saturated members in arrival order, indexed by
/// [`MergeKey`] so an incoming member is tried only against those it
/// could merge with: a union of N tags normalizes in linear time.
#[derive(Default)]
struct Kept {
    members: LPooled<Vec<Option<Type>>>,
    keyed: LPooled<AHashMap<MergeKey, SmallVec<[usize; 2]>>>,
    unkeyed: LPooled<Vec<usize>>,
    cells: LPooled<Vec<CellSig>>,
}

impl Kept {
    /// Take out the earliest kept member `t` merges with, and the merge.
    fn merge_with(
        &mut self,
        t: &Type,
        cx: &mut FlatCx,
        welds: &mut Welds,
    ) -> Option<Type> {
        let key = MergeKey::of(t, &cx.members);
        let keyed: &[usize] = match &key {
            Some(k) => self.keyed.get(k).map(|v| &v[..]).unwrap_or(&[]),
            None => &[],
        };
        let mut candidates: SmallVec<[usize; 8]> = SmallVec::new();
        match key {
            Some(_) => {
                candidates.extend(keyed.iter().copied());
                candidates.extend(self.unkeyed.iter().copied());
                candidates.sort_unstable();
            }
            None => candidates.extend(
                self.members
                    .iter()
                    .enumerate()
                    .filter(|(_, m)| m.is_some())
                    .map(|(i, _)| i),
            ),
        }
        let sig = CellSig::of(t, &cx.members);
        let (j, m, mut met) = candidates.iter().find_map(|&j| {
            if sig.clashes(&self.cells[j]) {
                return None;
            }
            let mut met = Welds::new();
            let kept = self.members[j].as_ref()?;
            t.merge(kept, cx, &mut met).map(|m| (j, m, met))
        })?;
        welds.extend(met.drain(..));
        let kept = self.members[j].take().expect("live");
        match MergeKey::of(&kept, &cx.members) {
            Some(k) => self.keyed.get_mut(&k).expect("indexed").retain(|i| *i != j),
            None => self.unkeyed.retain(|i| *i != j),
        }
        Some(m)
    }

    fn push(&mut self, t: Type, members: &Members) {
        let j = self.members.len();
        match MergeKey::of(&t, members) {
            Some(k) => self.keyed.entry(k).or_default().push(j),
            None => self.unkeyed.push(j),
        }
        self.cells.push(CellSig::of(&t, members));
        self.members.push(Some(t));
    }

    fn into_members(mut self) -> LPooled<Vec<Type>> {
        let mut out: LPooled<Vec<Type>> = LPooled::take();
        out.extend(self.members.drain(..).flatten());
        out
    }
}

impl Type {
    pub(crate) fn flatten_set(set: impl IntoIterator<Item = Self>) -> Self {
        Self::flatten_set_with(set, &mut FlatCx::default())
    }

    /// [`Self::flatten_set`] of the members the source writes in one union:
    /// a variable is told from another by its name, so two function types
    /// are one member only where they name the same variables.
    pub(crate) fn flatten_written(set: impl IntoIterator<Item = Self>) -> Self {
        let mut cx = FlatCx { members: Members::Written, ..FlatCx::default() };
        Self::flatten_set_with(set, &mut cx)
    }

    fn flatten_set_with(set: impl IntoIterator<Item = Self>, cx: &mut FlatCx) -> Self {
        Self::flatten_set_tracked(set, cx, &mut Welds::new()).0
    }

    /// [`Self::flatten_set`] reporting whether it changed anything
    /// relative to a `Type::Set` of the input members in input order.
    /// Conservative: `true` may be reported for an identical result.
    /// The live cells each collapse pairs go on `welds`.
    fn flatten_set_tracked(
        set: impl IntoIterator<Item = Self>,
        cx: &mut FlatCx,
        welds: &mut Welds,
    ) -> (Self, bool) {
        let mut leaves: LPooled<Vec<Self>> = LPooled::take();
        let mut nested: LPooled<Vec<(Arc<[Self]>, usize)>> = LPooled::take();
        let mut saw_bottom = false;
        let mut changed = false;
        let mut absorb = |t: Self, nested: &mut Vec<(Arc<[Self]>, usize)>| {
            match t {
                Type::Set(ref s) => {
                    changed = true;
                    nested.push((s.clone(), 0));
                }
                Type::Any => return false,
                // ⊥ ∪ X = X; an all-⊥ set is ⊥ (the exit match).
                Type::Bottom => {
                    changed = true;
                    saw_bottom = true
                }
                t => leaves.push(t),
            }
            true
        };
        for t in set {
            if !absorb(t, &mut nested) {
                return (Type::Any, true);
            }
            while let Some((members, i)) = nested.last_mut() {
                match members.get(*i) {
                    None => {
                        nested.pop();
                    }
                    Some(t) => {
                        *i += 1;
                        let t = t.clone();
                        if !absorb(t, &mut nested) {
                            return (Type::Any, true);
                        }
                    }
                }
            }
        }
        // merges are not confluent: saturating in sorted order makes the
        // result a function of the leaves, not of the order they were written
        if !leaves.is_sorted() {
            changed = true;
            leaves.sort();
        }
        let mut acc = Kept::default();
        for t in leaves.drain(..) {
            // `acc` is merge-saturated, so only the incoming element (or
            // its merge result) can enable a new merge.
            let mut incoming = t;
            while let Some(m) = acc.merge_with(&incoming, cx, welds) {
                changed = true;
                incoming = m;
            }
            acc.push(incoming, &cx.members);
        }
        let mut acc = acc.into_members();
        if !acc.is_sorted() {
            changed = true;
            acc.sort();
        }
        match &**acc {
            [] if saw_bottom => (Type::Bottom, true),
            [] => (Type::Primitive(BitFlags::empty()), true),
            [t] => (t.clone(), true),
            _ => (Type::Set(Arc::from_iter(acc.drain(..))), changed),
        }
    }

    /// Map `f` over a shared slice, rebuilding only if some element
    /// changed (`None` from `f` means unchanged). `None` when nothing
    /// changed.
    pub(crate) fn cow_slice<T: Clone>(
        orig: &[T],
        mut f: impl FnMut(&T) -> Option<T>,
    ) -> Option<Arc<[T]>> {
        let mut rebuilt: Option<LPooled<Vec<T>>> = None;
        for (i, x) in orig.iter().enumerate() {
            match f(x) {
                None => {
                    if let Some(v) = rebuilt.as_mut() {
                        v.push(x.clone())
                    }
                }
                Some(nx) => {
                    let v = rebuilt
                        .get_or_insert_with(|| orig[..i].iter().cloned().collect());
                    v.push(nx);
                }
            }
        }
        rebuilt.map(|mut v| Arc::from_iter(v.drain(..)))
    }

    /// Snapshot the type with every bound TVar replaced by its binding,
    /// recursively; unbound TVars become fresh cells, one per source
    /// cell, preserving alias topology. The result shares no cell with
    /// the original; TVar-free subtrees are returned shared.
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen_int(&mut ResolveTvarsCx::take())
            .unwrap_or_else(|| self.clone())
    }

    /// [`Self::resolve_tvars`], and the copy it made of the open cell of each
    /// of `cells`, which keeps the name of the cell given.
    pub(super) fn resolve_tvars_copying(
        &self,
        cells: &[TVar],
    ) -> (Self, SmallVec<[TVar; 2]>) {
        let mut cx = ResolveTvarsCx { bounded: true, ..ResolveTvarsCx::take() };
        for c in cells {
            if let Some(open) = c.open_cell() {
                cx.fresh.entry(open.cell_addr()).or_insert_with(|| c.fresh_copy());
            }
        }
        let typ = self.resolve_tvars_seen_int(&mut cx).unwrap_or_else(|| self.clone());
        let copies = cells
            .iter()
            .filter_map(|c| cx.fresh.get(&c.open_cell()?.cell_addr()).cloned())
            .collect();
        (typ, copies)
    }

    /// [`Self::resolve_tvars`] for tooling: an open cell keeps the
    /// predicates it must satisfy.
    pub fn snapshot(&self) -> Self {
        let mut cx = ResolveTvarsCx { predicates: true, ..ResolveTvarsCx::take() };
        self.resolve_tvars_seen_int(&mut cx).unwrap_or_else(|| self.clone())
    }

    /// `None` = no TVar anywhere beneath — the caller keeps the original.
    pub(super) fn resolve_tvars_seen_int(&self, cx: &mut ResolveTvarsCx) -> Option<Self> {
        ensure_sufficient(|| self.resolve_tvars_seen(cx))
    }

    fn resolve_tvars_seen(&self, cx: &mut ResolveTvarsCx) -> Option<Self> {
        let key = self.part_key();
        if let Some(k) = key
            && let Some(r) = cx.memo.get(&k)
        {
            return r.clone();
        }
        #[cfg(test)]
        RESOLVED.with(|n| n.set(n.get() + 1));
        let r = match self {
            Type::TVar(tv) => Some({
                let addr = tv.cell_addr();
                if let Some(t) = cx.done.get(&addr) {
                    return Some(tv.forwarded_as(t.clone()));
                }
                if let Some(fresh) = cx.fresh.get(&addr) {
                    cx.opened += 1;
                    return Some(Type::TVar(fresh.named_as(tv)));
                }
                if !cx.in_progress.insert(addr) {
                    cx.opened += 1;
                    return Some(Type::TVar(tv.fresh_copy()));
                }
                let resolve = |t: &Type, cx: &mut ResolveTvarsCx| {
                    t.resolve_tvars_seen_int(cx).unwrap_or_else(|| t.clone())
                };
                let ground = tv.ground_resolved(|t| {
                    let opened = cx.opened;
                    let r = resolve(t, cx);
                    (r, cx.opened == opened)
                });
                let r = match ground.or_else(|| tv.binding().map(|t| resolve(&t, cx))) {
                    Some(r) => {
                        cx.done.insert(addr, r.clone());
                        tv.forwarded_as(r)
                    }
                    None => {
                        cx.opened += 1;
                        let fresh = tv.fresh_copy();
                        cx.fresh.insert(addr, fresh.clone());
                        if cx.quantifiers.contains(&addr)
                            || (cx.bounded && tv.generated())
                        {
                            for c in tv.cell_constraints() {
                                let c = c.resolve_tvars_seen_int(cx).unwrap_or(c);
                                fresh.add_cell_constraint(c);
                            }
                        } else if cx.predicates {
                            for c in tv.cell_constraints() {
                                if c.is_predicate() {
                                    fresh.add_cell_constraint(c);
                                }
                            }
                        }
                        Type::TVar(fresh)
                    }
                };
                cx.in_progress.remove(&addr);
                r
            }),
            Type::Fn(ft) => {
                ft.resolve_tvars_seen_int(cx).map(|ft| Type::Fn(Arc::new(ft)))
            }
            t => t.cow_children(&mut |c| c.resolve_tvars_seen_int(cx)),
        };
        if let Some(k) = key {
            cx.memo.insert(k, r.clone());
        }
        r
    }

    /// Normalize structure and the bindings of every reachable cell;
    /// each cell is normalized once per pass.
    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut NormCx::take()).unwrap_or_else(|| self.clone())
    }

    /// [`Self::normalize`] for a union: the live cells each collapse pairs
    /// go on `welds`.
    pub(super) fn normalize_welding(&self, welds: &mut Welds) -> Self {
        let mut cx = NormCx::take();
        cx.visited.flat.members = Members::Welding;
        cx.visited.flat.alts = welds.alternatives().iter().cloned().collect();
        let r = self.normalize_int(&mut cx).unwrap_or_else(|| self.clone());
        welds.extend(cx.welds.drain(..));
        r
    }

    /// `None` when already normal. A `TVar` normalizes its binding in
    /// place inside the shared cell and is therefore always `None`.
    pub(super) fn normalize_int(&self, cx: &mut NormCx) -> Option<Self> {
        ensure_sufficient(|| self.normalize_int_inner(cx))
    }

    fn normalize_int_inner(&self, cx: &mut NormCx) -> Option<Self> {
        let key = self.part_key();
        if let Some(k) = key
            && let Some((_, r)) = cx.visited.memo.get(&k)
        {
            return r.clone();
        }
        #[cfg(test)]
        NORMALIZED.with(|n| n.set(n.get() + 1));
        let r = match self {
            Type::TVar(tv) => {
                tv.normalize_int(cx);
                None
            }
            Type::Set(s) => {
                let (flat, changed) = Self::normalize_members(s.iter(), cx);
                changed.then_some(flat)
            }
            Type::Fn(ft) => cx
                .in_signature(|cx| ft.normalize_int(cx))
                .map(|ft| Type::Fn(Arc::new(ft))),
            t => t.cow_children(&mut |c| c.normalize_int(cx)),
        };
        if let Some(k) = key {
            cx.visited.memo.insert(k, (self.clone(), r.clone()));
        }
        r
    }

    /// The normal form of the union of `members`, and whether it differs
    /// from a `Type::Set` of them in their order.
    fn normalize_members<'a>(
        members: impl Iterator<Item = &'a Self>,
        cx: &mut NormCx,
    ) -> (Self, bool) {
        let mut members_changed = false;
        let mut normal: LPooled<Vec<Self>> = LPooled::take();
        for t in members {
            match t.normalize_int(cx) {
                Some(n) => {
                    members_changed = true;
                    normal.push(n)
                }
                None => normal.push(t.clone()),
            }
        }
        let (flat, flat_changed) = Self::flatten_set_tracked(
            normal.drain(..),
            &mut cx.visited.flat,
            &mut cx.welds,
        );
        (flat, members_changed || flat_changed)
    }

    /// The normal form of the union of `members`, of one its own, in the
    /// pass `cx`, whose memo takes the members' nodes and not the union
    /// asked for.
    pub(super) fn normal_union<'a>(
        members: impl Iterator<Item = &'a Self> + Clone,
        cx: &mut NormCx,
    ) -> Self {
        let mut first_two = members.clone();
        match (first_two.next(), first_two.next()) {
            (Some(m), None) => m.normalize_int(cx).unwrap_or_else(|| m.clone()),
            _ => Self::normalize_members(members, cx).0,
        }
    }

    /// The one member `self` and `t` merge into; the live cells a
    /// collapse pairs go on `welds`, which a `None` leaves to be dropped.
    fn merge(&self, t: &Self, cx: &mut FlatCx, welds: &mut Welds) -> Option<Self> {
        ensure_sufficient(|| self.merge_inner(t, cx, welds))
    }

    fn merge_inner(&self, t: &Self, cx: &mut FlatCx, welds: &mut Welds) -> Option<Self> {
        // Equality modulo set-flattening at a nested position.
        fn flat_eq(t0: &Type, t1: &Type, cx: &mut FlatCx, welds: &mut Welds) -> bool {
            match (t0, t1) {
                (Type::Set(_), _) | (_, Type::Set(_)) => {
                    let (f0, f1) = (cx.flat(t0), cx.flat(t1));
                    cx.collapses(&f0, &f1, welds)
                }
                (t0, t1) => cx.collapses(t0, t1, welds),
            }
        }
        // Products merge component-wise only when at most one component
        // differs: `(A, X) ∪ (B, Y)` is not `([A, B], [X, Y])`.
        fn merge_one_differing<'a, T: 'a>(
            t0: impl IntoIterator<Item = &'a T>,
            t1: impl IntoIterator<Item = &'a T>,
            typ: impl Fn(&T) -> &Type,
            cx: &mut FlatCx,
            welds: &mut Welds,
        ) -> Option<Option<(usize, Type)>> {
            let mut differing = None;
            for (i, (a, b)) in t0.into_iter().zip(t1).enumerate() {
                let (a, b) = (typ(a), typ(b));
                if flat_eq(a, b, cx, welds) {
                    continue;
                }
                if differing.is_some() {
                    return None;
                }
                differing = Some((i, a.merge(b, cx, welds)?));
            }
            Some(differing)
        }
        // A bound constructor application is its filled type.
        if let Type::App(c, a) = self
            && let Some(filled) = Type::app_filled(c, a)
        {
            return filled.merge(t, cx, welds);
        }
        if let Type::App(c, a) = t
            && let Some(filled) = Type::app_filled(c, a)
        {
            return self.merge(&filled, cx, welds);
        }
        match (self, t) {
            (Type::Ref(_), Type::Ref(_)) => {
                if cx.collapses(self, t, welds) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Type::Ref(_), _) | (_, Type::Ref(_)) => None,
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Bound(_), _)
            | (_, Type::Bound(_)) => {
                if cx.collapses(self, t, welds) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Type::Bottom, t) | (t, Type::Bottom) => Some(t.clone()),
            (Type::Any, _) | (_, Type::Any) => Some(Type::Any),
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                Some(Type::Primitive(*s0 | *s1))
            }
            (Type::Primitive(p), t) | (t, Type::Primitive(p)) if p.is_empty() => {
                Some(t.clone())
            }
            (Type::Abstract { .. }, Type::Abstract { .. }) => {
                if cx.collapses(self, t, welds) { Some(self.clone()) } else { None }
            }
            (Type::Fn(_), Type::Fn(_)) => {
                if cx.collapses(self, t, welds) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Type::Array(t0), Type::Array(t1)) => {
                if flat_eq(t0, t1, cx, welds) {
                    Some(Type::Array(t0.clone()))
                } else {
                    None
                }
            }
            (Type::List(t0), Type::List(t1)) => {
                if flat_eq(t0, t1, cx, welds) {
                    Some(Type::List(t0.clone()))
                } else {
                    None
                }
            }
            (Type::Primitive(p), Type::Array(_))
            | (Type::Array(_), Type::Primitive(p)) => {
                if p.contains(Typ::Array) {
                    Some(Type::Primitive(*p))
                } else {
                    None
                }
            }
            (Type::Map(m0), Type::Map(m1)) => {
                if flat_eq(&m0.key, &m1.key, cx, welds)
                    && flat_eq(&m0.value, &m1.value, cx, welds)
                {
                    Some(Type::Map(m0.clone()))
                } else {
                    None
                }
            }
            (Type::Error(t0), Type::Error(t1)) => {
                if flat_eq(t0, t1, cx, welds) {
                    Some(Type::Error(t0.clone()))
                } else {
                    None
                }
            }
            (
                Type::ByRef(Mutability::Shared, t0),
                Type::ByRef(Mutability::Shared, t1),
            ) => t0
                .merge(t1, cx, welds)
                .map(|t| Type::ByRef(Mutability::Shared, Arc::new(t))),
            (Type::ByRef(m0, t0), Type::ByRef(m1, t1)) => {
                if m0 == m1 && flat_eq(t0, t1, cx, welds) {
                    Some(Type::ByRef(*m0, t0.clone()))
                } else {
                    None
                }
            }
            (Type::Set(s0), Type::Set(s1)) => Some(
                Self::flatten_set_tracked(
                    s0.iter().cloned().chain(s1.iter().cloned()),
                    cx,
                    welds,
                )
                .0,
            ),
            (Type::Set(s), t) | (t, Type::Set(s)) => Some(
                Self::flatten_set_tracked(
                    s.iter().cloned().chain(iter::once(t.clone())),
                    cx,
                    welds,
                )
                .0,
            ),
            (Type::Tuple(t0), Type::Tuple(t1)) if t0.len() == t1.len() => {
                match merge_one_differing(t0.iter(), t1.iter(), |t| t, cx, welds)? {
                    None => Some(self.clone()),
                    Some((i, m)) => {
                        let mut m = Some(m);
                        Some(Type::Tuple(Arc::from_iter(t0.iter().enumerate().map(
                            |(j, t)| if j == i { m.take().unwrap() } else { t.clone() },
                        ))))
                    }
                }
            }
            (Type::Variant(tag0, t0, at), Type::Variant(tag1, t1, _))
                if tag0 == tag1 && t0.len() == t1.len() =>
            {
                match merge_one_differing(t0.iter(), t1.iter(), |t| t, cx, welds)? {
                    None => Some(self.clone()),
                    Some((i, m)) => {
                        let mut m = Some(m);
                        let ts = Arc::from_iter(t0.iter().enumerate().map(|(j, t)| {
                            if j == i { m.take().unwrap() } else { t.clone() }
                        }));
                        Some(Type::Variant(tag0.clone(), ts, *at))
                    }
                }
            }
            (Type::Struct(t0), Type::Struct(t1))
                if t0.len() == t1.len()
                    && t0
                        .iter()
                        .zip(t1.iter())
                        .all(|((n0, _, _), (n1, _, _))| n0 == n1) =>
            {
                match merge_one_differing(t0.iter(), t1.iter(), |(_, t, _)| t, cx, welds)?
                {
                    None => Some(self.clone()),
                    Some((i, m)) => {
                        let mut m = Some(m);
                        let fs: Arc<[(ArcStr, Type, WrittenAt)]> = Arc::from_iter(
                            t0.iter().enumerate().map(|(j, (n, t, at))| {
                                let t =
                                    if j == i { m.take().unwrap() } else { t.clone() };
                                (n.clone(), t, *at)
                            }),
                        );
                        Some(Type::Struct(fs))
                    }
                }
            }
            // Strict tvar identity: two distinct unbound cells never merge.
            (t0v @ Type::TVar(_), t1v @ Type::TVar(_))
                if cx.collapses(t0v, t1v, welds) =>
            {
                Some(t0v.clone())
            }
            (Type::TVar(tv), t) => tv.binding().and_then(|b| b.merge(t, cx, welds)),
            (t, Type::TVar(tv)) => tv.binding().and_then(|b| t.merge(&b, cx, welds)),
            (Type::ByRef(..), _)
            | (_, Type::ByRef(..))
            | (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (_, Type::Map { .. })
            | (Type::Map { .. }, _)
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
            | (_, Type::Fn(_))
            | (Type::Fn(_), _)
            | (Type::Error(_), _)
            | (_, Type::Error(_)) => None,
        }
    }
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;
    use crate::{env::Env, expr::ModPath, typ::TypeRef};
    use std::cmp::Ordering;

    /// `S_k = [Error<S_k-1>, Error<T_k-1>, i64]` and `T_k` its twin with
    /// `string`: two unions per level over the two below, a DAG whose tree
    /// doubles at each level.
    fn error_ladder(depth: usize) -> (Type, Type) {
        let mut s = Type::Primitive(Typ::I64.into());
        let mut t = Type::Primitive(Typ::String.into());
        for _ in 0..depth {
            let (es, et) = (Type::Error(Arc::new(s)), Type::Error(Arc::new(t)));
            s = Type::Set(Arc::from_iter([
                es.clone(),
                et.clone(),
                Type::Primitive(Typ::I64.into()),
            ]));
            t = Type::Set(Arc::from_iter([es, et, Type::Primitive(Typ::String.into())]));
        }
        (s, t)
    }

    /// Merging `Error<S_n>` with `Error<T_n>` compares their payloads
    /// flattened, and each flattening merges the level below: every
    /// union of the ladder is flattened once.
    #[test]
    fn nested_unions_flatten_once_per_node() {
        let depth = 16;
        let (s, t) = error_ladder(depth);
        let mut cx = FlatCx::default();
        let members = [Type::Error(Arc::new(s)), Type::Error(Arc::new(t))];
        let flat = Type::flatten_set_with(members.iter().cloned(), &mut cx);
        assert!(matches!(&flat, Type::Set(m) if m.len() == 2), "{flat}");
        assert_eq!(cx.flattened, 2 * depth);
    }

    /// Two function types that differ by a renaming of their cells stay
    /// two members where nothing merges the cells: a normalize, a
    /// flattening. The union of alternatives, which does, makes them one.
    #[test]
    fn only_a_union_that_merges_cells_collapses_a_renaming() {
        let parsed = crate::expr::parser::parse_type(
            "(fn(x: 'c) -> i64, fn(x: 'd) -> i64, fn(x: 'c) -> i64)",
        );
        let t = parsed.unwrap();
        t.alias_tvars(&mut AHashMap::default());
        let Type::Tuple(fs) = &t else { unreachable!() };
        let members = |t: &Type| match t {
            Type::Set(ms) => ms.len(),
            _ => 1,
        };
        assert_eq!(members(&Type::Set(fs.clone()).normalize()), 2);
        assert_eq!(members(&Type::flatten_set(fs.iter().cloned())), 2);
        let alternatives = Type::union(&Env::default(), &[&fs[0], &fs[1]]).unwrap();
        assert_eq!(members(&alternatives), 1);
    }

    /// A union standing in a function type's signature keeps the members
    /// a renaming of the signature's cells would make one, live or
    /// generic, and the union of alternatives merges none of those cells.
    #[test]
    fn a_union_of_alternatives_keeps_a_signature_union_of_renamings() {
        let sig = "fn(y: 'c, z: 'd) -> [fn(q: 'c) -> i64, fn(q: 'd) -> i64]";
        for generic in [false, true] {
            let t = crate::expr::parser::parse_type(sig).unwrap();
            t.alias_tvars(&mut AHashMap::default());
            let Type::Fn(ft) = &t else { unreachable!() };
            if generic {
                ft.generalize(0)
            }
            let read = Type::union(&Env::default(), &[&t]).unwrap();
            let Type::Fn(sig) = &read else { unreachable!() };
            assert!(matches!(&sig.rtype, Type::Set(ms) if ms.len() == 2), "{read}");
            let (Type::TVar(c), Type::TVar(d)) = (&sig.args[0].typ, &sig.args[1].typ)
            else {
                unreachable!()
            };
            assert!(c.stands_for() != d.stands_for(), "{read}");
        }
    }

    /// One alternative joins nothing, however many times its node stands
    /// and whatever `⊥` stands beside it: a union it holds, at its top or
    /// under a constructor, keeps the members a renaming of two live
    /// cells would make one, and the cells stay two.
    #[test]
    fn a_union_of_one_alternative_keeps_its_union_of_renamings() {
        let env = Env::default();
        let written = "([fn(q: 'c) -> i64, fn(q: 'd) -> i64], 'c, 'd)";
        let t = crate::expr::parser::parse_type(written).unwrap();
        t.alias_tvars(&mut AHashMap::default());
        let Type::Tuple(fs) = &t else { unreachable!() };
        let (Type::TVar(c), Type::TVar(d)) = (&fs[1], &fs[2]) else { unreachable!() };
        let unions: [fn(&Env, &[&Type]) -> anyhow::Result<Type>; 4] = [
            Type::union,
            Type::union_exact,
            Type::union_of_alternatives,
            Type::union_exact_of_alternatives,
        ];
        for union in unions {
            for ts in [&[&fs[0]][..], &[&fs[0], &fs[0]], &[&Type::Bottom, &fs[0]]] {
                let read = union(&env, ts).unwrap();
                assert!(matches!(&read, Type::Set(ms) if ms.len() == 2), "{read}");
            }
            let read = union(&env, &[&t, &t]).unwrap();
            let Type::Tuple(parts) = &read else { unreachable!() };
            assert!(matches!(&parts[0], Type::Set(ms) if ms.len() == 2), "{read}");
            assert!(c.stands_for() != d.stands_for(), "{read}");
        }
    }

    /// A union one alternative of a join holds keeps its members as they
    /// stand, beside another alternative and beside a second union of the
    /// same shape, and members that are one type meet or collapse only
    /// across alternatives.
    #[test]
    fn a_join_of_alternatives_keeps_the_union_one_of_them_holds() {
        let env = Env::default();
        let written = "([fn(q: 'c) -> i64, fn(q: 'd) -> i64], 'c, 'd)";
        let t = crate::expr::parser::parse_type(written).unwrap();
        t.alias_tvars(&mut AHashMap::default());
        let Type::Tuple(fs) = &t else { unreachable!() };
        let (Type::TVar(c), Type::TVar(d)) = (&fs[1], &fs[2]) else { unreachable!() };
        let Type::Set(members) = &fs[0] else { unreachable!() };
        let null = Type::Primitive(Typ::Null.into());
        let unions: [fn(&Env, &[&Type]) -> anyhow::Result<Type>; 4] = [
            Type::union,
            Type::union_exact,
            Type::union_of_alternatives,
            Type::union_exact_of_alternatives,
        ];
        for union in unions {
            let read = union(&env, &[&fs[0], &null]).unwrap();
            assert!(matches!(&read, Type::Set(ms) if ms.len() == 3), "{read}");
            assert!(c.stands_for() != d.stands_for(), "{read}");
        }
        let (p, q) = (&members[0], &members[1]);
        let read = Type::union_exact_of_members(&env, &[p, q]).unwrap();
        assert!(matches!(&read, Type::Set(ms) if ms.len() == 2), "{read}");
        let read = Type::union_exact_of_parts(&env, &[p, q], &[0, 0]).unwrap();
        assert!(matches!(&read, Type::Set(ms) if ms.len() == 2), "{read}");
        assert!(c.stands_for() != d.stands_for(), "{read}");
        let read = Type::union_exact_of_parts(&env, &[p, q], &[0, 1]).unwrap();
        assert!(!matches!(&read, Type::Set(_)), "{read}");
    }

    /// A node shared by both sides compares equal without walking it.
    #[test]
    fn ordering_does_not_walk_a_shared_node() {
        let (s, t) = error_ladder(64);
        let members = [Type::Error(Arc::new(s)), Type::Error(Arc::new(t))];
        let a = Type::Set(Arc::from_iter(members.iter().cloned()));
        let b = Type::Set(Arc::from_iter(members.iter().cloned()));
        assert_eq!(a.cmp(&b), Ordering::Equal);
    }

    /// Collecting a capture type's open cells walks each shared node once.
    #[test]
    fn narrowing_collects_cells_once_per_node() {
        let (s, _) = error_ladder(64);
        s.narrow_per_member(&Env::default(), &[]).unwrap();
    }

    /// A cell of an arm's predicate that is a member's own part is one
    /// cell with the open parts of the other members; a cell of the arm's
    /// own takes their union and leaves them apart.
    #[test]
    fn narrowing_a_members_own_cell_makes_the_open_parts_one() {
        let env = Env::default();
        let cell = |n: &str| TVar::empty_named(n.into());
        let member = |tv: &TVar| {
            let parts = [Type::Primitive(Typ::I64.into()), Type::TVar(tv.clone())];
            Type::Tuple(Arc::from_iter(parts))
        };
        let (a, b, c) = (cell("a"), cell("b"), cell("c"));
        let members = [member(&a), member(&b), member(&c)];
        member(&a).narrow_per_member(&env, &members).unwrap();
        assert!(a.open_cell().is_some());
        assert!(a.stands_for() == b.stands_for() && a.stands_for() == c.stands_for());
        let (u, v, p) = (cell("u"), cell("v"), cell("p"));
        member(&p).narrow_per_member(&env, &[member(&u), member(&v)]).unwrap();
        assert!(p.open_cell().is_none());
        assert!(u.open_cell().is_some() && v.open_cell().is_some());
        assert!(u.stands_for() != v.stands_for());
    }

    /// An arm's cell that is one of the cells of the union at its part of
    /// a variant is one cell with the open ones and takes what a bound one
    /// holds, and what a member of that union that is no cell holds.
    #[test]
    fn narrowing_a_cell_of_a_payload_union_makes_its_cells_one() {
        let env = Env::default();
        let cell = |n: &str| TVar::empty_named(n.into());
        let prim = |t: Typ| Type::Primitive(t.into());
        let payload = |t: Type| {
            Type::Variant(
                "A".into(),
                Arc::from_iter([prim(Typ::I64), t]),
                WrittenAt::NOWHERE,
            )
        };
        let union = |ts: &[Type]| Type::Set(Arc::from_iter(ts.iter().cloned()));
        let (a, b, c) = (cell("a"), cell("b"), cell("c"));
        let tvs = [Type::TVar(a.clone()), Type::TVar(b.clone()), Type::TVar(c.clone())];
        let scrut = payload(union(&tvs));
        assert!(scrut.narrow_pattern(&env, &payload(tvs[0].clone())).unwrap());
        assert!(a.open_cell().is_some());
        assert!(a.stands_for() == b.stands_for() && a.stands_for() == c.stands_for());
        let (p, q) = (cell("p"), cell("q"));
        q.bind(prim(Typ::String));
        let tvs = [Type::TVar(p.clone()), Type::TVar(q.clone())];
        let scrut = payload(union(&tvs));
        assert!(scrut.narrow_pattern(&env, &payload(tvs[0].clone())).unwrap());
        assert_eq!(p.binding(), Some(prim(Typ::String)));
        let u = cell("u");
        let scrut = payload(union(&[Type::TVar(u.clone()), prim(Typ::Null)]));
        assert!(scrut.narrow_pattern(&env, &payload(Type::TVar(u.clone()))).unwrap());
        assert_eq!(Type::TVar(u).deref_cloned(), Some(prim(Typ::Null)));
    }

    /// A cell a member holds where the arm's predicate does is its own
    /// part of that member, never a copy merged back into it: each arm's
    /// narrowing leaves the cell the lower bounds it had.
    #[test]
    fn narrowing_a_members_own_cell_keeps_its_lower_bounds() {
        let env = Env::default();
        let x = TVar::empty_named("x".into());
        let cells: [TVar; 3] = std::array::from_fn(|_| {
            let c = TVar::empty_named("c".into());
            c.add_lower(Type::TVar(x.clone()));
            c
        });
        let members = cells.each_ref().map(|c| {
            let parts = [Type::Primitive(Typ::I64.into()), Type::TVar(c.clone())];
            Type::Tuple(Arc::from_iter(parts))
        });
        for m in members.iter() {
            m.narrow_per_member(&env, &members).unwrap();
        }
        let one = cells[0].open_cell().unwrap();
        assert!(cells.iter().all(|c| c.stands_for() == one.cell_addr()));
        assert!(one.cell_lower().len() <= cells.len());
    }

    /// The forward links from `tv` to the cell it stands for.
    fn links(tv: &TVar) -> usize {
        let mut at = tv.clone();
        let mut n = 0;
        while let Some(Type::TVar(next)) = &at.binding() {
            (at, n) = (next.clone(), n + 1)
        }
        n
    }

    /// An array of the union of `cell` standing `k` times: the predicate
    /// of a slice pattern of `k` positions once they are one cell.
    fn slice_of(cell: &TVar, k: usize) -> Type {
        let elems = std::iter::repeat_n(Type::TVar(cell.clone()), k);
        Type::Array(Arc::new(Type::Set(elems.collect())))
    }

    /// The one element cell slice arms leave an open scrutinee is alone
    /// at each member's elements, a union of it or a cell bound to it: no
    /// arm copies it, so none adds a link to it or a lower bound.
    #[test]
    fn narrowing_slice_arms_over_their_one_cell_copies_nothing() {
        let env = Env::default();
        let (c, e) = (TVar::empty_named("c".into()), TVar::empty_named("e".into()));
        c.add_lower(Type::Primitive(Typ::I64.into()));
        e.bind(Type::TVar(c.clone()));
        let mut arms: Vec<Type> = (2..=13).rev().map(|k| slice_of(&c, k)).collect();
        arms.push(Type::Array(Arc::new(Type::TVar(e))));
        let scrut = Type::TVar(TVar::empty_named("x".into()));
        let Type::TVar(x) = &scrut else { unreachable!() };
        x.bind(Type::Set(Arc::from_iter(arms.iter().cloned())));
        for arm in arms.iter() {
            assert!(scrut.narrow_pattern(&env, arm).unwrap());
        }
        assert_eq!(links(&c), 0);
        assert_eq!(c.cell_lower().len(), 1);
    }

    /// Arms that take apart a slice inside a slice (`[[a0, .., t..], u..]`)
    /// narrow against members that hold the inner cell beside an array of
    /// it, where an arm's copy takes the cell: each copy copies the cell,
    /// never the links to it, so the links grow by the pairs narrowed, not
    /// by a factor at each.
    #[test]
    fn narrowing_nested_slice_arms_copies_no_chain_of_links() {
        let env = Env::default();
        let n = 4;
        let (arms, cells): (Vec<Type>, Vec<Vec<TVar>>) = (1..=n)
            .rev()
            .map(|k| {
                let mut cells: Vec<TVar> = (0..=k + 1)
                    .map(|i| TVar::empty_named(arcstr::format!("c{k}_{i}")))
                    .collect();
                let rest = Type::TVar(cells.pop().unwrap());
                let inner = Type::Set(cells.iter().cloned().map(Type::TVar).collect());
                let elems =
                    Type::Set(Arc::from_iter([Type::Array(Arc::new(inner)), rest]));
                (Type::Array(Arc::new(elems)), cells)
            })
            .unzip();
        let scrut = Type::TVar(TVar::empty_named("x".into()));
        let union = Type::union_exact(&env, &arms.iter().collect::<Vec<_>>()).unwrap();
        assert!(union.contains_ordered(&env, &scrut).unwrap());
        for arm in arms.iter() {
            assert!(scrut.narrow_pattern(&env, arm).unwrap());
        }
        let most = cells.iter().flatten().map(links).max().unwrap();
        assert!(most <= 2 * n * n, "{most} links after {n} arms");
    }

    /// Slice arms over a scrutinee whose members are one type, as the first
    /// arm leaves the members of an open scrutinee the arms typed: each arm
    /// narrows against one of them, and its cell is the members' cell.
    #[test]
    fn narrowing_against_members_of_one_type_walks_one() {
        use crate::typ::tvar::NARROW_STEPS;
        let env = Env::default();
        let n = 24;
        let c = TVar::empty_named("c".into());
        let members: Vec<Type> = (1..=n).rev().map(|k| slice_of(&c, k)).collect();
        let scrut = Type::TVar(TVar::empty_named("x".into()));
        let Type::TVar(x) = &scrut else { unreachable!() };
        x.bind(Type::Set(Arc::from_iter(members)));
        let mut cells: Vec<TVar> = vec![];
        NARROW_STEPS.with(|s| s.set(0));
        for k in (1..=n).rev() {
            let at = cells.len();
            cells.extend((0..=k).map(|i| TVar::empty_named(arcstr::format!("a{k}_{i}"))));
            let elems = cells[at..].iter().cloned().map(Type::TVar);
            let arm = Type::Array(Arc::new(Type::Set(elems.collect())));
            assert!(scrut.narrow_pattern(&env, &arm).unwrap());
        }
        let steps = NARROW_STEPS.with(|s| s.get());
        for a in cells.iter() {
            assert!(a.open_cell().is_some_and(|a| a.same_cell(&c)), "{a}");
        }
        assert_eq!(steps, 348, "narrowings for {n} arms");
    }

    /// A member walked as a member before it is dropped: one type with it,
    /// a union's members but its open cells in order. An array of a union of
    /// two arrays one type with each other walks apart from an array of one
    /// of them.
    #[test]
    fn members_walked_as_one_before_are_dropped() {
        let (c, d) = (TVar::empty_named("c".into()), TVar::empty_named("d".into()));
        let (cs, ds) = (|k| slice_of(&c, k), |k| slice_of(&d, k));
        let mut members: SmallVec<[Type; 8]> =
            SmallVec::from_iter([cs(2), ds(1), cs(1), cs(3), ds(2)]);
        Type::drop_repeated(&mut members);
        assert_eq!(&members[..], &[cs(2), ds(1)]);
        let nested = |k| Type::Array(Arc::new(cs(k)));
        let two = Type::Array(Arc::new(Type::Set(Arc::from_iter([cs(1), cs(2)]))));
        let mut members: SmallVec<[Type; 8]> =
            SmallVec::from_iter([nested(1), two.clone(), nested(2)]);
        Type::drop_repeated(&mut members);
        assert_eq!(&members[..], &[nested(1), two]);
    }

    /// A union of named or abstract members normalizes in linear time:
    /// each is tried only against members it could merge with.
    #[test]
    fn named_and_abstract_members_are_keyed() {
        let abs = |name| Type::Abstract {
            id: AbstractId::of(&ModPath::root(), name),
            params: Arc::from_iter([]),
        };
        let named = |name: &str| {
            Type::Ref(Arc::new(TypeRef::synthetic(
                ModPath::root(),
                ModPath::from([name]),
                Arc::from_iter([]),
            )))
        };
        let key = |t: &Type| MergeKey::of(t, &Members::AsTheyStand).expect("keyed");
        assert!(key(&abs("A")) == key(&abs("A")));
        assert!(key(&abs("A")) != key(&abs("B")));
        assert!(key(&named("A")) == key(&named("A")));
        assert!(key(&named("A")) != key(&named("B")));
    }

    fn open_cell() -> Type {
        Type::TVar(TVar::empty_named(arcstr::literal!("c")))
    }

    /// A union of products holding distinct open cells at one position
    /// keeps every member without one identity walk of a pair.
    #[test]
    fn products_of_distinct_cells_are_not_walked_pairwise() {
        use crate::typ::setops::IDENTITY_STEPS;
        let i64t = Type::Primitive(Typ::I64.into());
        let tuple = |a, b| Type::Tuple(Arc::from_iter([a, b]));
        let variant = |a, b| {
            Type::Variant(
                arcstr::literal!("A"),
                Arc::from_iter([a, b]),
                WrittenAt::default(),
            )
        };
        for product in [&tuple as &dyn Fn(Type, Type) -> Type, &variant] {
            let members: Vec<Type> =
                (0..32).map(|_| product(i64t.clone(), open_cell())).collect();
            IDENTITY_STEPS.with(|s| s.set(0));
            let kept = Type::flatten_set(members);
            assert!(matches!(&kept, Type::Set(ms) if ms.len() == 32), "{kept:?}");
            assert_eq!(IDENTITY_STEPS.with(|s| s.get()), 0);
        }
    }

    /// Two products of one shape that hold different open cells at a
    /// position are two members; the same cell there lets them merge.
    #[test]
    fn products_merge_only_through_one_cell() {
        let (c0, c1) = (open_cell(), open_cell());
        let (i, s) =
            (Type::Primitive(Typ::I64.into()), Type::Primitive(Typ::String.into()));
        let pair =
            |a: &Type, b: &Type| Type::Tuple(Arc::from_iter([a.clone(), b.clone()]));
        let apart = Type::flatten_set([pair(&i, &c0), pair(&i, &c1)]);
        assert!(matches!(&apart, Type::Set(ms) if ms.len() == 2), "{apart:?}");
        let joined = Type::flatten_set([pair(&i, &c0), pair(&s, &c0)]);
        assert!(matches!(&joined, Type::Tuple(_)), "{joined:?}");
    }

    /// Cells `c_0 = i64`, `c_k = (c_k-1, c_k-1)`, proved ground: what
    /// `c_depth` holds.
    pub(in crate::typ) fn ground_pair_chain(depth: usize) -> Vec<TVar> {
        let mut cells: Vec<TVar> = vec![TVar::empty_named(arcstr::literal!("c"))];
        cells[0].bind(Type::Primitive(Typ::I64.into()));
        for _ in 0..depth {
            let last = Type::TVar(cells[cells.len() - 1].clone());
            let c = TVar::empty_named(arcstr::literal!("c"));
            c.bind(Type::Tuple(Arc::from_iter([last.clone(), last])));
            cells.push(c);
        }
        assert!(Type::TVar(cells[depth].clone()).prove_ground());
        cells
    }

    /// `measure` while `top`'s proof stands: a write to a bound cell, which
    /// a test on another thread may make, moves the ground epoch and drops
    /// what the cells keep, so `measure` runs again over a new proof until
    /// no such write fell inside it.
    pub(in crate::typ) fn while_ground<T>(
        top: &TVar,
        mut measure: impl FnMut() -> T,
    ) -> T {
        for _ in 0..100_000 {
            assert!(Type::TVar(top.clone()).prove_ground());
            let r = measure();
            if top.ground_now() {
                return r;
            }
        }
        panic!("the ground epoch moved under every measure")
    }

    /// A ground cell keeps what it resolved to: a chain of them resolves
    /// a link at a time once, then each link of it at once, over the
    /// first resolution's parts.
    #[test]
    fn a_ground_chain_resolves_once() {
        let depth = 64;
        let cells = ground_pair_chain(depth);
        let walked = |tv: &TVar| {
            RESOLVED.with(|n| n.set(0));
            let r = Type::TVar(tv.clone()).resolve_tvars();
            (r, RESOLVED.with(|n| n.get()))
        };
        let ((first, n), (again, n_again), (below, n_below)) =
            while_ground(&cells[depth], || {
                (walked(&cells[depth]), walked(&cells[depth]), walked(&cells[depth - 1]))
            });
        assert!(n <= 3 * depth + 2, "the first resolution walked {n} nodes");
        assert_eq!(n_again, 1, "a second resolution walked {n_again} nodes");
        assert!(again.shares_content(&first));
        assert_eq!(n_below, 1, "a link's resolution walked {n_below} nodes");
        let Type::Tuple(parts) = &first else { panic!("not a pair") };
        assert!(below.shares_content(&parts[0]));
    }
}
