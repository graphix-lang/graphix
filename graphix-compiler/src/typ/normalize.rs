use crate::typ::Type;
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
            [] => Type::Primitive(BitFlags::empty()),
            [t] => t.clone(),
            _ => Type::Set(Arc::from_iter(acc.drain(..))),
        }
    }

    pub(crate) fn normalize(&self) -> Self {
        match self {
            Type::Bottom | Type::Any | Type::Abstract { .. } | Type::Primitive(_) => {
                self.clone()
            }
            Type::Ref { scope, name, params } => {
                let params = Arc::from_iter(params.iter().map(|t| t.normalize()));
                Type::Ref { scope: scope.clone(), name: name.clone(), params }
            }
            Type::TVar(tv) => Type::TVar(tv.normalize()),
            Type::Set(s) => Self::flatten_set(s.iter().map(|t| t.normalize())),
            Type::Error(t) => Type::Error(Arc::new(t.normalize())),
            Type::Array(t) => Type::Array(Arc::new(t.normalize())),
            Type::Map { key, value } => {
                let key = Arc::new(key.normalize());
                let value = Arc::new(value.normalize());
                Type::Map { key, value }
            }
            Type::ByRef(t) => Type::ByRef(Arc::new(t.normalize())),
            Type::Tuple(t) => {
                Type::Tuple(Arc::from_iter(t.iter().map(|t| t.normalize())))
            }
            Type::Struct(t) => Type::Struct(Arc::from_iter(
                t.iter().map(|(n, t)| (n.clone(), t.normalize())),
            )),
            Type::Variant(tag, t) => Type::Variant(
                tag.clone(),
                Arc::from_iter(t.iter().map(|t| t.normalize())),
            ),
            Type::Fn(ft) => Type::Fn(Arc::new(ft.normalize())),
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
            (
                Type::Ref { scope: s0, name: r0, params: a0 },
                Type::Ref { scope: s1, name: r1, params: a1 },
            ) => {
                if s0 == s1 && r0 == r1 && a0 == a1 {
                    Some(Type::Ref {
                        scope: s0.clone(),
                        name: r0.clone(),
                        params: a0.clone(),
                    })
                } else {
                    None
                }
            }
            (Type::Ref { .. }, _) | (_, Type::Ref { .. }) => None,
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
                tv.read().typ.read().as_ref().and_then(|tv| tv.merge(t))
            }
            (t, Type::TVar(tv)) => {
                tv.read().typ.read().as_ref().and_then(|tv| t.merge(tv))
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
