use crate::{
    AddrMap, AddrSet,
    expr::WrittenAt,
    stack::ensure_sufficient,
    typ::{AbstractId, Mutability, TVar, Type, setops::union_identical},
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
    mem::Discriminant,
};
use triomphe::Arc;

/// Per-pass [`Type::normalize`] state: `cells` are the visited TVar
/// cells (the first visit normalizes the binding in place); `memo` is a
/// pointer-identity cache of composite results, keyed by (variant,
/// content Arc address) for variants whose Arcs are their whole content.
/// Each entry pins the keyed type: an in-place rebinding frees the old
/// binding, and its address must not be reused under its key.
pub(super) struct NormCx {
    pub(super) cells: LPooled<AddrSet>,
    memo: LPooled<AHashMap<NormKey, (Type, Option<Type>)>>,
}

#[doc(hidden)]
pub type NormKey = (Discriminant<Type>, usize, usize);

impl NormCx {
    pub(super) fn take() -> Self {
        Self { cells: LPooled::take(), memo: LPooled::take() }
    }
}

/// Per-pass [`Type::resolve_tvars`] state: `done` holds completed
/// snapshots of bound cells, `fresh` the one fresh mint per unbound
/// source cell (preserving alias topology), `in_progress` the cycle
/// guard, `memo` a pointer-identity cache of composite results.
pub(super) struct ResolveTvarsCx {
    done: LPooled<AddrMap<Type>>,
    fresh: LPooled<AddrMap<TVar>>,
    in_progress: LPooled<AddrSet>,
    memo: LPooled<AHashMap<NormKey, Option<Type>>>,
    /// An open cell's copy keeps its predicate conjuncts (`Concrete`,
    /// `Function`), which name no cell.
    predicates: bool,
    /// The open quantifiers of the function types met: their bounds are
    /// part of the type (`fn<'a: Function>(..)`), so a copy keeps them all.
    pub(super) quantifiers: LPooled<AddrSet>,
}

impl ResolveTvarsCx {
    pub(super) fn take() -> Self {
        Self {
            done: LPooled::take(),
            fresh: LPooled::take(),
            in_progress: LPooled::take(),
            memo: LPooled::take(),
            predicates: false,
            quantifiers: LPooled::take(),
        }
    }
}

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
        Type::Map { key, value } => {
            Some((d, &**key as *const Type as usize, &**value as *const Type as usize))
        }
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::Abstract { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Bound(_)
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::Variant(_, _, _) => None,
    }
}

/// What a member can merge with beyond the unkeyed members: a variant
/// only with one of its tag and arity, a struct only with one of its
/// field names, a reference only with one to its definition (by its
/// written name while its cell is open) and an abstract type only with
/// itself, each of its arity.
#[derive(PartialEq, Eq, Hash)]
enum MergeKey {
    Variant(ArcStr, usize),
    Struct(u64, usize),
    Ref(u64, usize),
    Abstract(AbstractId, usize),
}

impl MergeKey {
    fn of(t: &Type) -> Option<Self> {
        match t {
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
}

impl Kept {
    /// Take out the earliest kept member `t` merges with, and the merge.
    fn merge_with(&mut self, t: &Type) -> Option<Type> {
        let key = MergeKey::of(t);
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
        let (j, m) = candidates.iter().find_map(|&j| {
            self.members[j].as_ref().and_then(|kept| t.merge(kept)).map(|m| (j, m))
        })?;
        let kept = self.members[j].take().expect("live");
        match MergeKey::of(&kept) {
            Some(k) => self.keyed.get_mut(&k).expect("indexed").retain(|i| *i != j),
            None => self.unkeyed.retain(|i| *i != j),
        }
        Some(m)
    }

    fn push(&mut self, t: Type) {
        let j = self.members.len();
        match MergeKey::of(&t) {
            Some(k) => self.keyed.entry(k).or_default().push(j),
            None => self.unkeyed.push(j),
        }
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
        Self::flatten_set_tracked(set).0
    }

    /// [`Self::flatten_set`] reporting whether it changed anything
    /// relative to a `Type::Set` of the input members in input order.
    /// Conservative: `true` may be reported for an identical result.
    fn flatten_set_tracked(set: impl IntoIterator<Item = Self>) -> (Self, bool) {
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
            while let Some(m) = acc.merge_with(&incoming) {
                changed = true;
                incoming = m;
            }
            acc.push(incoming);
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
        let key = norm_key(self);
        if let Some(k) = key
            && let Some(r) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::TVar(tv) => Some({
                let addr = tv.cell_addr();
                if let Some(t) = cx.done.get(&addr) {
                    return Some(tv.forwarded_as(t.clone()));
                }
                if let Some(fresh) = cx.fresh.get(&addr) {
                    return Some(Type::TVar(fresh.named_as(tv)));
                }
                if !cx.in_progress.insert(addr) {
                    return Some(Type::TVar(tv.fresh_copy()));
                }
                let r = match tv.binding() {
                    Some(t) => {
                        let r = match t.resolve_tvars_seen_int(cx) {
                            Some(t) => t,
                            None => t,
                        };
                        cx.done.insert(addr, r.clone());
                        tv.forwarded_as(r)
                    }
                    None => {
                        let fresh = tv.fresh_copy();
                        cx.fresh.insert(addr, fresh.clone());
                        if cx.quantifiers.contains(&addr) {
                            for c in tv.cell_constraints() {
                                let c = c.resolve_tvars_seen_int(cx).unwrap_or(c);
                                fresh.add_cell_constraint(c);
                            }
                            if tv.is_formal_quantifier() {
                                fresh.mark_formal_quantifier()
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

    /// `None` when already normal. A `TVar` normalizes its binding in
    /// place inside the shared cell and is therefore always `None`.
    pub(super) fn normalize_int(&self, cx: &mut NormCx) -> Option<Self> {
        ensure_sufficient(|| self.normalize_int_inner(cx))
    }

    fn normalize_int_inner(&self, cx: &mut NormCx) -> Option<Self> {
        let key = norm_key(self);
        if let Some(k) = key
            && let Some((_, r)) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::TVar(tv) => {
                tv.normalize_int(cx);
                None
            }
            Type::Set(s) => {
                let mut members_changed = false;
                let mut members: LPooled<Vec<Self>> = LPooled::take();
                for t in s.iter() {
                    match t.normalize_int(cx) {
                        Some(n) => {
                            members_changed = true;
                            members.push(n)
                        }
                        None => members.push(t.clone()),
                    }
                }
                let (flat, flat_changed) = Self::flatten_set_tracked(members.drain(..));
                (members_changed || flat_changed).then_some(flat)
            }
            Type::Fn(ft) => ft.normalize_int(cx).map(|ft| Type::Fn(Arc::new(ft))),
            t => t.cow_children(&mut |c| c.normalize_int(cx)),
        };
        if let Some(k) = key {
            cx.memo.insert(k, (self.clone(), r.clone()));
        }
        r
    }

    fn merge(&self, t: &Self) -> Option<Self> {
        ensure_sufficient(|| self.merge_inner(t))
    }

    fn merge_inner(&self, t: &Self) -> Option<Self> {
        // Equality modulo set-flattening at a nested position.
        fn flat_eq(t0: &Type, t1: &Type) -> bool {
            match (t0, t1) {
                (Type::Set(_), _) | (_, Type::Set(_)) => {
                    let f = |t: &Type| match t {
                        Type::Set(s) => Type::flatten_set(s.iter().cloned()),
                        t => t.clone(),
                    };
                    union_identical(&f(t0), &f(t1))
                }
                (t0, t1) => union_identical(t0, t1),
            }
        }
        // Products merge component-wise only when at most one component
        // differs: `(A, X) ∪ (B, Y)` is not `([A, B], [X, Y])`.
        fn merge_one_differing<'a, T: 'a>(
            t0: impl IntoIterator<Item = &'a T>,
            t1: impl IntoIterator<Item = &'a T>,
            typ: impl Fn(&T) -> &Type,
        ) -> Option<Option<(usize, Type)>> {
            let mut differing = None;
            for (i, (a, b)) in t0.into_iter().zip(t1).enumerate() {
                let (a, b) = (typ(a), typ(b));
                if flat_eq(a, b) {
                    continue;
                }
                if differing.is_some() {
                    return None;
                }
                differing = Some((i, a.merge(b)?));
            }
            Some(differing)
        }
        // A bound constructor application is its filled type.
        if let Type::App(c, a) = self
            && let Some(filled) = Type::app_filled(c, a)
        {
            return filled.merge(t);
        }
        if let Type::App(c, a) = t
            && let Some(filled) = Type::app_filled(c, a)
        {
            return self.merge(&filled);
        }
        match (self, t) {
            (Type::Ref(_), Type::Ref(_)) => {
                if union_identical(self, t) {
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
                if union_identical(self, t) {
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
                if union_identical(self, t) { Some(self.clone()) } else { None }
            }
            (Type::Fn(_), Type::Fn(_)) => {
                if union_identical(self, t) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Type::Array(t0), Type::Array(t1)) => {
                if flat_eq(t0, t1) {
                    Some(Type::Array(t0.clone()))
                } else {
                    None
                }
            }
            (Type::List(t0), Type::List(t1)) => {
                if flat_eq(t0, t1) {
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
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                if flat_eq(k0, k1) && flat_eq(v0, v1) {
                    Some(Type::Map { key: k0.clone(), value: v0.clone() })
                } else {
                    None
                }
            }
            (Type::Error(t0), Type::Error(t1)) => {
                if flat_eq(t0, t1) {
                    Some(Type::Error(t0.clone()))
                } else {
                    None
                }
            }
            (
                Type::ByRef(Mutability::Shared, t0),
                Type::ByRef(Mutability::Shared, t1),
            ) => t0.merge(t1).map(|t| Type::ByRef(Mutability::Shared, Arc::new(t))),
            (Type::ByRef(m0, t0), Type::ByRef(m1, t1)) => {
                if m0 == m1 && flat_eq(t0, t1) {
                    Some(Type::ByRef(*m0, t0.clone()))
                } else {
                    None
                }
            }
            (Type::Set(s0), Type::Set(s1)) => {
                Some(Self::flatten_set(s0.iter().cloned().chain(s1.iter().cloned())))
            }
            (Type::Set(s), t) | (t, Type::Set(s)) => {
                Some(Self::flatten_set(s.iter().cloned().chain(iter::once(t.clone()))))
            }
            (Type::Tuple(t0), Type::Tuple(t1)) if t0.len() == t1.len() => {
                match merge_one_differing(t0.iter(), t1.iter(), |t| t)? {
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
                match merge_one_differing(t0.iter(), t1.iter(), |t| t)? {
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
                match merge_one_differing(t0.iter(), t1.iter(), |(_, t, _)| t)? {
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
            (t0v @ Type::TVar(_), t1v @ Type::TVar(_)) if union_identical(t0v, t1v) => {
                Some(t0v.clone())
            }
            (Type::TVar(tv), t) => tv.binding().and_then(|b| b.merge(t)),
            (t, Type::TVar(tv)) => tv.binding().and_then(|b| t.merge(&b)),
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
mod tests {
    use super::*;
    use crate::{expr::ModPath, typ::TypeRef};

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
        let key = |t: &Type| MergeKey::of(t).expect("keyed");
        assert!(key(&abs("A")) == key(&abs("A")));
        assert!(key(&abs("A")) != key(&abs("B")));
        assert!(key(&named("A")) == key(&named("A")));
        assert!(key(&named("A")) != key(&named("B")));
    }
}
