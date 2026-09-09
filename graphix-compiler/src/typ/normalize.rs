use crate::typ::{TVar, Type, TypeRef};
use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{iter, mem::Discriminant};
use triomphe::Arc;

/// Per-pass [`Type::normalize`] state: `cells` are the visited TVar
/// cells (the first visit normalizes the binding in place); `memo` is a
/// pointer-identity cache of composite results, keyed by (variant,
/// content Arc address) for variants whose Arcs are their whole content.
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

/// Per-pass [`Type::resolve_tvars`] state: `done` holds completed
/// snapshots of bound cells, `fresh` the one fresh mint per unbound
/// source cell (preserving alias topology), `in_progress` the cycle
/// guard, `memo` a pointer-identity cache of composite results.
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
        Type::Array(a) | Type::List(a) | Type::Error(a) | Type::ByRef(a) => {
            Some((d, &**a as *const Type as usize, 0))
        }
        Type::Map { key, value } => {
            Some((d, &**key as *const Type as usize, &**value as *const Type as usize))
        }
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::Abstract { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::Variant(_, _) => None,
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
                    // ⊥ ∪ X = X; an all-⊥ set is ⊥ (the exit match).
                    Some(Type::Bottom) => {
                        changed = true;
                        saw_bottom = true
                    }
                    Some(t) => {
                        // `acc` is merge-saturated, so only the incoming
                        // element (or its merge result) can enable a new
                        // merge.
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
            Type::Bottom | Type::Any | Type::Primitive(_) | Type::Hole => None,
            Type::App(c, a) => match (c.resolve_tvars_seen(cx), a.resolve_tvars_seen(cx))
            {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
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
            Type::List(t) => t.resolve_tvars_seen(cx).map(|t| Type::List(Arc::new(t))),
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

    /// Normalize structure and the bindings of every reachable cell;
    /// each cell is normalized once per pass.
    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut NormCx::take()).unwrap_or_else(|| self.clone())
    }

    /// `None` when already normal. A `TVar` normalizes its binding in
    /// place inside the shared cell and is therefore always `None`.
    pub(super) fn normalize_int(&self, cx: &mut NormCx) -> Option<Self> {
        crate::stack::ensure_sufficient(|| self.normalize_int_inner(cx))
    }

    fn normalize_int_inner(&self, cx: &mut NormCx) -> Option<Self> {
        let key = norm_key(self);
        if let Some(k) = key
            && let Some(r) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::Bottom
            | Type::Any
            | Type::Abstract { .. }
            | Type::Primitive(_)
            | Type::Hole => None,
            Type::App(c, a) => match (c.normalize_int(cx), a.normalize_int(cx)) {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
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
            Type::List(t) => t.normalize_int(cx).map(|t| Type::List(Arc::new(t))),
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
        // Equality modulo set-flattening at a nested position.
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
            // A bound constructor application is its filled type.
            (Type::App(c, a), _) if Type::app_filled(c, a).is_some() => {
                Type::app_filled(c, a).unwrap().merge(t)
            }
            (_, Type::App(c, a)) if Type::app_filled(c, a).is_some() => {
                self.merge(&Type::app_filled(c, a).unwrap())
            }
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole) => {
                if self == t {
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
            // Strict tvar identity: two distinct unbound cells never merge.
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
            | (Type::List(_), _)
            | (_, Type::List(_))
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
