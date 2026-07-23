use crate::typ::{TVar, Type, TypeRef};
use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{iter, mem::Discriminant};
use triomphe::Arc;

/// Per-pass [`Type::normalize`] state. `cells` = visited TVar cells
/// (the first visit normalizes the binding in place; one cell is
/// reachable along many paths, and re-walking was exponential — soak
/// jul06h). `memo` = pointer-identity cache of composite results, so a
/// subtree SHARED along many paths is processed once per pass (a DAG
/// walk, not a tree walk). Keyed by (variant discriminant, content Arc
/// address(es)) and restricted to variants whose Arcs ARE the node's
/// whole content — `Ref`/`Variant` carry extra fields and are skipped
/// (their composite children still hit their own entries). Sound
/// per-pass: cell writes happen once per cell (`cells`), so a shared
/// subtree normalizes identically at every occurrence within the pass.
pub(super) struct NormCx {
    pub(super) cells: LPooled<nohash::IntSet<usize>>,
    memo: LPooled<AHashMap<NormKey, Option<Type>>>,
}

pub(crate) type NormKey = (Discriminant<Type>, usize, usize);

impl NormCx {
    pub(super) fn take() -> Self {
        Self { cells: LPooled::take(), memo: LPooled::take() }
    }
}

/// Per-pass [`Type::resolve_tvars`] state — the walk is a DAG traversal,
/// not a tree copy. `done` = completed snapshots of BOUND cells (one
/// cell reached along many paths through the shared expansion snapshots
/// once; re-walking materialized tree-scale copies — the widget-typed
/// instance signatures hit 40GB, 2026-07-13). `fresh` = the pass's ONE
/// fresh mint per UNBOUND source cell, preserving the source's alias
/// topology (the same discipline as `reset_tvars`' cell-keyed map —
/// per-occurrence minting was an artifact of the tree walk, not a
/// contract). `in_progress` = the cycle guard (a cell revisited while
/// its own binding expands snapshots as fresh unbound, as before).
/// `memo` = pointer-identity cache of composite results — sound because
/// cells resolve identically within the pass by construction.
pub(super) struct ResolveTvarsCx {
    done: LPooled<nohash::IntMap<usize, Type>>,
    fresh: LPooled<nohash::IntMap<usize, TVar>>,
    in_progress: LPooled<nohash::IntSet<usize>>,
    memo: LPooled<AHashMap<NormKey, Option<Type>>>,
}

impl ResolveTvarsCx {
    pub(super) fn take() -> Self {
        Self {
            done: LPooled::take(),
            fresh: LPooled::take(),
            in_progress: LPooled::take(),
            memo: LPooled::take(),
        }
    }
}

pub(crate) fn norm_key(t: &Type) -> Option<NormKey> {
    let d = std::mem::discriminant(t);
    match t {
        Type::Set(a) | Type::Tuple(a) => Some((d, (**a).as_ptr() as usize, 0)),
        Type::Struct(a) => Some((d, (**a).as_ptr() as usize, 0)),
        Type::Fn(a) => Some((d, &**a as *const _ as usize, 0)),
        Type::Array(a) | Type::Error(a) | Type::ByRef(a) => {
            Some((d, &**a as *const Type as usize, 0))
        }
        Type::Map { key, value } => {
            Some((d, &**key as *const Type as usize, &**value as *const Type as usize))
        }
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::Abstract { .. }
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::Variant(_, _) => None,
    }
}

impl Type {
    pub(crate) fn flatten_set(set: impl IntoIterator<Item = Self>) -> Self {
        Self::flatten_set_tracked(set).0
    }

    /// [`Self::flatten_set`] reporting whether it CHANGED anything
    /// relative to a `Type::Set` of the input members in input order
    /// (`false` lets [`Self::normalize_int`] keep the original shared
    /// members instead of rebuilding an identical set). Conservative:
    /// `true` may be reported for an ultimately-identical result.
    fn flatten_set_tracked(set: impl IntoIterator<Item = Self>) -> (Self, bool) {
        let init: Box<dyn Iterator<Item = Self>> = Box::new(set.into_iter());
        let mut iters: LPooled<Vec<Box<dyn Iterator<Item = Self>>>> =
            LPooled::from_iter([init]);
        let mut acc: LPooled<Vec<Self>> = LPooled::take();
        let mut saw_bottom = false;
        let mut changed = false;
        loop {
            match iters.last_mut() {
                None => break,
                Some(iter) => match iter.next() {
                    None => {
                        iters.pop();
                    }
                    Some(Type::Set(s)) => {
                        changed = true;
                        let v: SmallVec<[Self; 16]> =
                            s.iter().map(|t| t.clone()).collect();
                        iters.push(Box::new(v.into_iter()))
                    }
                    Some(Type::Any) => return (Type::Any, true),
                    // ⊥ ∪ X = X: a ⊥ member (a never() select arm, a
                    // ⊥-settled cell after resolve_tvars) contributes
                    // nothing to a union — drop it. An ALL-⊥ set is ⊥
                    // itself (see the exit match).
                    Some(Type::Bottom) => {
                        changed = true;
                        saw_bottom = true
                    }
                    Some(t) => {
                        // `acc` is merge-saturated (invariant maintained
                        // here), so only the INCOMING element — or the
                        // result of merging it — can enable a new merge:
                        // re-run for the merged value alone instead of
                        // restarting the whole sweep from (0, 0) (the
                        // restart made saturation cubic in set width).
                        let mut incoming = t;
                        'merge: loop {
                            for j in 0..acc.len() {
                                if let Some(m) = incoming.merge(&acc[j]) {
                                    changed = true;
                                    acc.remove(j);
                                    incoming = m;
                                    continue 'merge;
                                }
                            }
                            acc.push(incoming);
                            break;
                        }
                    }
                },
            }
        }
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
    /// changed (`None` from `f` = element unchanged). `None` = nothing
    /// changed, keep the original.
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

    /// Snapshot the type with every bound TVar replaced by its concrete
    /// binding (recursively). Unbound TVars become fresh named TVars —
    /// ONE fresh cell per source cell per pass, preserving the source's
    /// alias topology (the same discipline as `reset_tvars`). This
    /// produces a snapshot that is independent of the original TVar
    /// cells. The walk is a DAG traversal: TVar-FREE subtrees are
    /// returned SHARED (the original Arcs), bound cells snapshot once
    /// per pass, and shared composites are pointer-memoized — cell
    /// independence only concerns cells, and re-walking shared structure
    /// per path materialized tree-scale copies (40GB on widget-typed
    /// instance signatures, 2026-07-13). A cell revisited while its own
    /// binding expands snapshots as a fresh unbound (cycle guard —
    /// defense in depth: the cycle VECTORS are closed at
    /// `TVar::{alias, copy, settle}`, but a walk this hot must not hang
    /// on one that slips through).
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen(&mut ResolveTvarsCx::take())
            .unwrap_or_else(|| self.clone())
    }

    pub(super) fn resolve_tvars_seen_int(&self, cx: &mut ResolveTvarsCx) -> Option<Self> {
        self.resolve_tvars_seen(cx)
    }

    /// `None` = no TVar anywhere beneath — the caller keeps the original.
    fn resolve_tvars_seen(&self, cx: &mut ResolveTvarsCx) -> Option<Self> {
        let key = norm_key(self);
        if let Some(k) = key
            && let Some(r) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => None,
            Type::Abstract { id, params } => {
                Self::cow_slice(params, |t| t.resolve_tvars_seen(cx))
                    .map(|params| Type::Abstract { id: *id, params })
            }
            Type::Ref(tr) => Self::cow_slice(&tr.params, |t| t.resolve_tvars_seen(cx))
                .map(|params| Type::Ref(tr.with_params(params))),
            Type::TVar(tv) => Some({
                let addr = tv.cell_addr();
                if let Some(t) = cx.done.get(&addr) {
                    return Some(t.clone());
                }
                if let Some(fresh) = cx.fresh.get(&addr) {
                    return Some(Type::TVar(fresh.clone()));
                }
                if !cx.in_progress.insert(addr) {
                    return Some(Type::TVar(TVar::empty_named(tv.name.clone())));
                }
                let bound = tv.read().typ.read().typ.clone();
                let r = match bound {
                    Some(t) => {
                        let r = match t.resolve_tvars_seen(cx) {
                            Some(t) => t,
                            None => t,
                        };
                        cx.done.insert(addr, r.clone());
                        r
                    }
                    None => {
                        let fresh = TVar::empty_named(tv.name.clone());
                        cx.fresh.insert(addr, fresh.clone());
                        Type::TVar(fresh)
                    }
                };
                cx.in_progress.remove(&addr);
                r
            }),
            Type::Set(s) => {
                Self::cow_slice(s, |t| t.resolve_tvars_seen(cx)).map(Type::Set)
            }
            Type::Error(t) => t.resolve_tvars_seen(cx).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => t.resolve_tvars_seen(cx).map(|t| Type::Array(Arc::new(t))),
            Type::Map { key, value } => {
                match (key.resolve_tvars_seen(cx), value.resolve_tvars_seen(cx)) {
                    (None, None) => None,
                    (k, v) => Some(Type::Map {
                        key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                        value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                    }),
                }
            }
            Type::ByRef(t) => t.resolve_tvars_seen(cx).map(|t| Type::ByRef(Arc::new(t))),
            Type::Tuple(t) => {
                Self::cow_slice(t, |t| t.resolve_tvars_seen(cx)).map(Type::Tuple)
            }
            Type::Struct(t) => Self::cow_slice(t, |(n, t)| {
                t.resolve_tvars_seen(cx).map(|t| (n.clone(), t))
            })
            .map(Type::Struct),
            Type::Variant(tag, t) => Self::cow_slice(t, |t| t.resolve_tvars_seen(cx))
                .map(|t| Type::Variant(tag.clone(), t)),
            Type::Fn(ft) => {
                ft.resolve_tvars_seen_int(cx).map(|ft| Type::Fn(Arc::new(ft)))
            }
        };
        if let Some(k) = key {
            cx.memo.insert(k, r.clone());
        }
        r
    }

    /// Normalization walks CELLS as well as structure, and one cell can
    /// be reachable along many paths (aliased tvars share a cell;
    /// entangled fn-sig unions repeat cells across members), so the
    /// walk carries a visited set keyed by cell address: without it the
    /// re-walk was exponential in the sharing depth — a polymorphic
    /// builtin used as a first-class array element wedged the whole
    /// compile (soak jul06h), ASLR-order dependent via the Set sort.
    /// A revisited cell is returned as-is; the first visit normalizes
    /// its binding.
    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut NormCx::take()).unwrap_or_else(|| self.clone())
    }

    /// `None` = already normal — the caller keeps the original (shared).
    /// A `TVar` normalizes its binding IN PLACE inside the shared cell
    /// (`TVar::normalize_int`) and is therefore always `None` as a
    /// value: the node still wraps the same cell.
    pub(super) fn normalize_int(&self, cx: &mut NormCx) -> Option<Self> {
        let key = norm_key(self);
        if let Some(k) = key
            && let Some(r) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::Bottom | Type::Any | Type::Abstract { .. } | Type::Primitive(_) => None,
            Type::Ref(tr) => Self::cow_slice(&tr.params, |t| t.normalize_int(cx))
                .map(|params| Type::Ref(tr.with_params(params))),
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
            Type::Error(t) => t.normalize_int(cx).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => t.normalize_int(cx).map(|t| Type::Array(Arc::new(t))),
            Type::Map { key, value } => {
                match (key.normalize_int(cx), value.normalize_int(cx)) {
                    (None, None) => None,
                    (k, v) => Some(Type::Map {
                        key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                        value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                    }),
                }
            }
            Type::ByRef(t) => t.normalize_int(cx).map(|t| Type::ByRef(Arc::new(t))),
            Type::Tuple(t) => {
                Self::cow_slice(t, |t| t.normalize_int(cx)).map(Type::Tuple)
            }
            Type::Struct(t) => {
                Self::cow_slice(t, |(n, t)| t.normalize_int(cx).map(|t| (n.clone(), t)))
                    .map(Type::Struct)
            }
            Type::Variant(tag, t) => Self::cow_slice(t, |t| t.normalize_int(cx))
                .map(|t| Type::Variant(tag.clone(), t)),
            Type::Fn(ft) => ft.normalize_int(cx).map(|ft| Type::Fn(Arc::new(ft))),
        };
        if let Some(k) = key {
            cx.memo.insert(k, r.clone());
        }
        r
    }

    fn merge(&self, t: &Self) -> Option<Self> {
        // Equality modulo set-flattening at a NESTED position. Flatten
        // only when a side actually is a Set — flattening (and cloning)
        // both sides on every comparison dominated merge on large types.
        fn flat_eq(t0: &Type, t1: &Type) -> bool {
            match (t0, t1) {
                (Type::Set(_), _) | (_, Type::Set(_)) => {
                    let f = |t: &Type| match t {
                        Type::Set(s) => Type::flatten_set(s.iter().cloned()),
                        t => t.clone(),
                    };
                    f(t0) == f(t1)
                }
                (t0, t1) => t0 == t1,
            }
        }
        match (self, t) {
            (Type::Ref(t0), Type::Ref(t1)) => {
                if t0 == t1 {
                    Some(Type::Ref(t0.clone()))
                } else {
                    None
                }
            }
            (Type::Ref(TypeRef { .. }), _) | (_, Type::Ref(TypeRef { .. })) => None,
            (Type::Bottom, t) | (t, Type::Bottom) => Some(t.clone()),
            (Type::Any, _) | (_, Type::Any) => Some(Type::Any),
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                Some(Type::Primitive(*s0 | *s1))
            }
            (Type::Primitive(p), t) | (t, Type::Primitive(p)) if p.is_empty() => {
                Some(t.clone())
            }
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) => {
                if id0 == id1 && p0 == p1 {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                if f0 == f1 {
                    Some(Type::Fn(f0.clone()))
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
            (Type::ByRef(t0), Type::ByRef(t1)) => {
                t0.merge(t1).map(|t| Type::ByRef(Arc::new(t)))
            }
            (Type::Set(s0), Type::Set(s1)) => {
                Some(Self::flatten_set(s0.iter().cloned().chain(s1.iter().cloned())))
            }
            (Type::Set(s), Type::Primitive(p)) | (Type::Primitive(p), Type::Set(s))
                if p.is_empty() =>
            {
                Some(Type::Set(s.clone()))
            }
            (Type::Set(s), t) | (t, Type::Set(s)) => {
                Some(Self::flatten_set(s.iter().cloned().chain(iter::once(t.clone()))))
            }
            (Type::Tuple(t0), Type::Tuple(t1)) => {
                if t0.len() == t1.len() {
                    let mut t = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.merge(t1))
                        .collect::<Option<LPooled<Vec<Type>>>>()?;
                    Some(Type::Tuple(Arc::from_iter(t.drain(..))))
                } else {
                    None
                }
            }
            (Type::Variant(tag0, t0), Type::Variant(tag1, t1)) => {
                if tag0 == tag1 && t0.len() == t1.len() {
                    let t = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.merge(t1))
                        .collect::<Option<SmallVec<[Type; 8]>>>()?;
                    Some(Type::Variant(tag0.clone(), Arc::from_iter(t)))
                } else {
                    None
                }
            }
            (Type::Struct(t0), Type::Struct(t1)) => {
                if t0.len() == t1.len() {
                    let t = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|((n0, t0), (n1, t1))| {
                            if n0 != n1 {
                                None
                            } else {
                                t0.merge(t1).map(|t| (n0.clone(), t))
                            }
                        })
                        .collect::<Option<SmallVec<[(ArcStr, Type); 8]>>>()?;
                    Some(Type::Struct(Arc::from_iter(t)))
                } else {
                    None
                }
            }
            // STRICT tvar identity (`union_identical`, the setops
            // union rule): `TVar::eq` calls two distinct UNBOUND cells
            // equal (None == None) and default-minted cells share a
            // name, so the old `tv0 == tv1` guard merged a select
            // union's base-arm cell into the rec-return cell — the
            // dropped cell's future binding vanished and the survivor
            // terminal-settled ⊥ (jul17c katana divergence 000001:
            // `[i64, ⊥]` elem flattened to i64 and the kernel compared
            // a Fn element's payload bits as i64).
            (t0v @ Type::TVar(_), t1v @ Type::TVar(_))
                if super::setops::union_identical(t0v, t1v) =>
            {
                Some(t0v.clone())
            }
            (Type::TVar(tv), t) => {
                tv.read().typ.read().typ.as_ref().and_then(|tv| tv.merge(t))
            }
            (t, Type::TVar(tv)) => {
                tv.read().typ.read().typ.as_ref().and_then(|tv| t.merge(tv))
            }
            (Type::ByRef(_), _)
            | (_, Type::ByRef(_))
            | (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (_, Type::Map { .. })
            | (Type::Map { .. }, _)
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _), _)
            | (_, Type::Variant(_, _))
            | (_, Type::Fn(_))
            | (Type::Fn(_), _)
            | (Type::Error(_), _)
            | (_, Type::Error(_)) => None,
        }
    }
}
