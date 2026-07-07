use crate::typ::{TVar, Type, TypeRef};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::publisher::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::iter;
use triomphe::Arc;

impl Type {
    pub(crate) fn flatten_set(set: impl IntoIterator<Item = Self>) -> Self {
        let init: Box<dyn Iterator<Item = Self>> = Box::new(set.into_iter());
        let mut iters: LPooled<Vec<Box<dyn Iterator<Item = Self>>>> =
            LPooled::from_iter([init]);
        let mut acc: LPooled<Vec<Self>> = LPooled::take();
        let mut saw_bottom = false;
        loop {
            match iters.last_mut() {
                None => break,
                Some(iter) => match iter.next() {
                    None => {
                        iters.pop();
                    }
                    Some(Type::Set(s)) => {
                        let v: SmallVec<[Self; 16]> =
                            s.iter().map(|t| t.clone()).collect();
                        iters.push(Box::new(v.into_iter()))
                    }
                    Some(Type::Any) => return Type::Any,
                    // ⊥ ∪ X = X: a ⊥ member (a never() select arm, a
                    // ⊥-settled cell after resolve_tvars) contributes
                    // nothing to a union — drop it. An ALL-⊥ set is ⊥
                    // itself (see the exit match).
                    Some(Type::Bottom) => saw_bottom = true,
                    Some(t) => {
                        acc.push(t);
                        let mut i = 0;
                        let mut j = 0;
                        while i < acc.len() {
                            while j < acc.len() {
                                if j == i {
                                    j += 1;
                                    continue;
                                }
                                match acc[i].merge(&acc[j]) {
                                    None => j += 1,
                                    Some(t) => {
                                        acc[i] = t;
                                        acc.remove(j);
                                        i = 0;
                                        j = 0;
                                    }
                                }
                            }
                            i += 1;
                            j = 0;
                        }
                    }
                },
            }
        }
        acc.sort();
        match &**acc {
            [] if saw_bottom => Type::Bottom,
            [] => Type::Primitive(BitFlags::empty()),
            [t] => t.clone(),
            _ => Type::Set(Arc::from_iter(acc.drain(..))),
        }
    }

    /// Deep-clone the type tree, replacing every bound TVar with its
    /// concrete binding (recursively). Unbound TVars are kept as fresh
    /// named TVars. This produces a snapshot that is independent of the
    /// original TVar cells. Carries a visited set keyed by cell address
    /// — a revisited cell snapshots as a fresh unbound (the same
    /// convention as unbound cells), so pathological cell sharing can't
    /// re-walk exponentially and a cyclic binding can't recurse forever
    /// (defense in depth: the cycle VECTORS are closed at
    /// `TVar::{alias, copy, settle}`, but a walk this hot must not hang
    /// on one that slips through).
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen(&mut LPooled::take())
    }

    pub(super) fn resolve_tvars_seen_int(
        &self,
        seen: &mut nohash::IntSet<usize>,
    ) -> Self {
        self.resolve_tvars_seen(seen)
    }

    fn resolve_tvars_seen(&self, seen: &mut nohash::IntSet<usize>) -> Self {
        match self {
            Type::Bottom | Type::Any | Type::Primitive(_) => self.clone(),
            Type::Abstract { id, params } => Type::Abstract {
                id: *id,
                params: Arc::from_iter(params.iter().map(|t| t.resolve_tvars_seen(seen))),
            },
            Type::Ref(tr) => Type::Ref(TypeRef {
                params: Arc::from_iter(
                    tr.params.iter().map(|t| t.resolve_tvars_seen(seen)),
                ),
                ..tr.clone()
            }),
            Type::TVar(tv) => {
                if !seen.insert(tv.cell_addr()) {
                    return Type::TVar(TVar::empty_named(tv.name.clone()));
                }
                let bound = tv.read().typ.read().typ.clone();
                let r = match bound {
                    Some(t) => t.resolve_tvars_seen(seen),
                    None => Type::TVar(TVar::empty_named(tv.name.clone())),
                };
                seen.remove(&tv.cell_addr());
                r
            }
            Type::Set(s) => {
                Type::Set(Arc::from_iter(s.iter().map(|t| t.resolve_tvars_seen(seen))))
            }
            Type::Error(t) => Type::Error(Arc::new(t.resolve_tvars_seen(seen))),
            Type::Array(t) => Type::Array(Arc::new(t.resolve_tvars_seen(seen))),
            Type::Map { key, value } => Type::Map {
                key: Arc::new(key.resolve_tvars_seen(seen)),
                value: Arc::new(value.resolve_tvars_seen(seen)),
            },
            Type::ByRef(t) => Type::ByRef(Arc::new(t.resolve_tvars_seen(seen))),
            Type::Tuple(t) => {
                Type::Tuple(Arc::from_iter(t.iter().map(|t| t.resolve_tvars_seen(seen))))
            }
            Type::Struct(t) => Type::Struct(Arc::from_iter(
                t.iter().map(|(n, t)| (n.clone(), t.resolve_tvars_seen(seen))),
            )),
            Type::Variant(tag, t) => Type::Variant(
                tag.clone(),
                Arc::from_iter(t.iter().map(|t| t.resolve_tvars_seen(seen))),
            ),
            Type::Fn(ft) => Type::Fn(Arc::new(ft.resolve_tvars_seen_int(seen))),
        }
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
        self.normalize_int(&mut LPooled::take())
    }

    pub(super) fn normalize_int(&self, seen: &mut nohash::IntSet<usize>) -> Self {
        match self {
            Type::Bottom | Type::Any | Type::Abstract { .. } | Type::Primitive(_) => {
                self.clone()
            }
            Type::Ref(tr) => {
                let params =
                    Arc::from_iter(tr.params.iter().map(|t| t.normalize_int(seen)));
                Type::Ref(TypeRef { params, ..tr.clone() })
            }
            Type::TVar(tv) => Type::TVar(tv.normalize_int(seen)),
            Type::Set(s) => Self::flatten_set(s.iter().map(|t| t.normalize_int(seen))),
            Type::Error(t) => Type::Error(Arc::new(t.normalize_int(seen))),
            Type::Array(t) => Type::Array(Arc::new(t.normalize_int(seen))),
            Type::Map { key, value } => {
                let key = Arc::new(key.normalize_int(seen));
                let value = Arc::new(value.normalize_int(seen));
                Type::Map { key, value }
            }
            Type::ByRef(t) => Type::ByRef(Arc::new(t.normalize_int(seen))),
            Type::Tuple(t) => {
                Type::Tuple(Arc::from_iter(t.iter().map(|t| t.normalize_int(seen))))
            }
            Type::Struct(t) => Type::Struct(Arc::from_iter(
                t.iter().map(|(n, t)| (n.clone(), t.normalize_int(seen))),
            )),
            Type::Variant(tag, t) => Type::Variant(
                tag.clone(),
                Arc::from_iter(t.iter().map(|t| t.normalize_int(seen))),
            ),
            Type::Fn(ft) => Type::Fn(Arc::new(ft.normalize_int(seen))),
        }
    }

    fn merge(&self, t: &Self) -> Option<Self> {
        macro_rules! flatten {
            ($t:expr) => {
                match $t {
                    Type::Set(et) => Self::flatten_set(et.iter().cloned()),
                    t => t.clone(),
                }
            };
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
                if flatten!(&**t0) == flatten!(&**t1) {
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
                if flatten!(&**k0) == flatten!(&**k1)
                    && flatten!(&**v0) == flatten!(&**v1)
                {
                    Some(Type::Map { key: k0.clone(), value: v0.clone() })
                } else {
                    None
                }
            }
            (Type::Error(t0), Type::Error(t1)) => {
                if flatten!(&**t0) == flatten!(&**t1) {
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
            (Type::TVar(tv0), Type::TVar(tv1)) if tv0.name == tv1.name && tv0 == tv1 => {
                Some(Type::TVar(tv0.clone()))
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
