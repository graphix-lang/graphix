use crate::{env::Env, typ::Type};
use anyhow::Result;
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use netidx::publisher::Typ;
use poolshark::local::LPooled;
use std::iter;
use triomphe::Arc;

impl Type {
    fn union_int(
        &self,
        env: &Env,
        hist: &mut FxHashMap<(usize, usize), Type>,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (
                Type::Ref { scope: s0, name: n0, params: p0 },
                Type::Ref { scope: s1, name: n1, params: p1 },
            ) if n0 == n1 && s0 == s1 && p0.len() == p1.len() => {
                let mut params = p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(p0, p1)| p0.union_int(env, hist, p1))
                    .collect::<Result<LPooled<Vec<_>>>>()?;
                let params = Arc::from_iter(params.drain(..));
                Ok(Self::Ref { scope: s0.clone(), name: n0.clone(), params })
            }
            (tr @ Type::Ref { .. }, t) => {
                let t0 = tr.lookup_ref(env)?;
                let t0_addr = (t0 as *const Type).addr();
                let t_addr = (t as *const Type).addr();
                match hist.get(&(t0_addr, t_addr)) {
                    Some(t) => Ok(t.clone()),
                    None => {
                        hist.insert((t0_addr, t_addr), tr.clone());
                        let r = t0.union_int(env, hist, t)?;
                        hist.insert((t0_addr, t_addr), r.clone());
                        Ok(r)
                    }
                }
            }
            (t, tr @ Type::Ref { .. }) => {
                let t1 = tr.lookup_ref(env)?;
                let t1_addr = (t1 as *const Type).addr();
                let t_addr = (t as *const Type).addr();
                match hist.get(&(t_addr, t1_addr)) {
                    Some(t) => Ok(t.clone()),
                    None => {
                        hist.insert((t_addr, t1_addr), tr.clone());
                        let r = t.union_int(env, hist, t1)?;
                        hist.insert((t_addr, t1_addr), r.clone());
                        Ok(r)
                    }
                }
            }
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) if id0 == id1 && p0 == p1 => Ok(self.clone()),
            (t0 @ Type::Abstract { .. }, t1) | (t0, t1 @ Type::Abstract { .. }) => {
                Ok(Type::Set(Arc::from_iter([t0.clone(), t1.clone()])))
            }
            (Type::Bottom, t) | (t, Type::Bottom) => Ok(t.clone()),
            (Type::Any, _) | (_, Type::Any) => Ok(Type::Any),
            (Type::Primitive(p), t) | (t, Type::Primitive(p)) if p.is_empty() => {
                Ok(t.clone())
            }
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                let mut s = *s0;
                s.insert(*s1);
                Ok(Type::Primitive(s))
            }
            (
                Type::Primitive(p),
                Type::Array(_) | Type::Struct(_) | Type::Tuple(_) | Type::Variant(_, _),
            )
            | (
                Type::Array(_) | Type::Struct(_) | Type::Tuple(_) | Type::Variant(_, _),
                Type::Primitive(p),
            ) if p.contains(Typ::Array) => Ok(Type::Primitive(*p)),
            (Type::Primitive(p), Type::Array(t))
            | (Type::Array(t), Type::Primitive(p)) => Ok(Type::Set(Arc::from_iter([
                Type::Primitive(*p),
                Type::Array(t.clone()),
            ]))),
            (t @ Type::Array(t0), u @ Type::Array(t1)) => {
                if t0 == t1 {
                    Ok(Type::Array(t0.clone()))
                } else {
                    Ok(Type::Set(Arc::from_iter([t.clone(), u.clone()])))
                }
            }
            (Type::Primitive(p), Type::Map { .. })
            | (Type::Map { .. }, Type::Primitive(p))
                if p.contains(Typ::Map) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Primitive(p), Type::Map { key, value })
            | (Type::Map { key, value }, Type::Primitive(p)) => {
                Ok(Type::Set(Arc::from_iter([
                    Type::Primitive(*p),
                    Type::Map { key: key.clone(), value: value.clone() },
                ])))
            }
            (
                t @ Type::Map { key: k0, value: v0 },
                u @ Type::Map { key: k1, value: v1 },
            ) => {
                if k0 == k1 && v0 == v1 {
                    Ok(Type::Map { key: k0.clone(), value: v0.clone() })
                } else {
                    Ok(Type::Set(Arc::from_iter([t.clone(), u.clone()])))
                }
            }
            (t @ Type::Map { .. }, u) | (u, t @ Type::Map { .. }) => {
                Ok(Type::Set(Arc::from_iter([t.clone(), u.clone()])))
            }
            (Type::Primitive(p), Type::Error(_))
            | (Type::Error(_), Type::Primitive(p))
                if p.contains(Typ::Error) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Error(e0), Type::Error(e1)) => {
                Ok(Type::Error(Arc::new(e0.union_int(env, hist, e1)?)))
            }
            (e @ Type::Error(_), t) | (t, e @ Type::Error(_)) => {
                Ok(Type::Set(Arc::from_iter([e.clone(), t.clone()])))
            }
            (t @ Type::ByRef(t0), u @ Type::ByRef(t1)) => {
                if t0 == t1 {
                    Ok(Type::ByRef(t0.clone()))
                } else {
                    Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
                }
            }
            (Type::Set(s0), Type::Set(s1)) => Ok(Type::Set(Arc::from_iter(
                s0.iter().cloned().chain(s1.iter().cloned()),
            ))),
            (Type::Set(s), t) | (t, Type::Set(s)) => Ok(Type::Set(Arc::from_iter(
                s.iter().cloned().chain(iter::once(t.clone())),
            ))),
            (u @ Type::Struct(t0), t @ Type::Struct(t1)) => {
                if t0.len() == t1.len() && t0 == t1 {
                    Ok(u.clone())
                } else {
                    Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
                }
            }
            (u @ Type::Struct(_), t) | (t, u @ Type::Struct(_)) => {
                Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
            }
            (u @ Type::Tuple(t0), t @ Type::Tuple(t1)) => {
                if t0 == t1 {
                    Ok(u.clone())
                } else {
                    Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
                }
            }
            (u @ Type::Tuple(_), t) | (t, u @ Type::Tuple(_)) => {
                Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
            }
            (u @ Type::Variant(tg0, t0), t @ Type::Variant(tg1, t1)) => {
                if tg0 == tg1 && t0.len() == t1.len() {
                    let mut typs = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.union_int(env, hist, t1))
                        .collect::<Result<LPooled<Vec<_>>>>()?;
                    Ok(Type::Variant(tg0.clone(), Arc::from_iter(typs.drain(..))))
                } else {
                    Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
                }
            }
            (u @ Type::Variant(_, _), t) | (t, u @ Type::Variant(_, _)) => {
                Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                if f0 == f1 {
                    Ok(Type::Fn(f0.clone()))
                } else {
                    Ok(Type::Set(Arc::from_iter([
                        Type::Fn(f0.clone()),
                        Type::Fn(f1.clone()),
                    ])))
                }
            }
            (f @ Type::Fn(_), t) | (t, f @ Type::Fn(_)) => {
                Ok(Type::Set(Arc::from_iter([f.clone(), t.clone()])))
            }
            (t0 @ Type::TVar(_), t1 @ Type::TVar(_)) => {
                if t0 == t1 {
                    Ok(t0.clone())
                } else {
                    Ok(Type::Set(Arc::from_iter([t0.clone(), t1.clone()])))
                }
            }
            (t0 @ Type::TVar(_), t1) | (t1, t0 @ Type::TVar(_)) => {
                Ok(Type::Set(Arc::from_iter([t0.clone(), t1.clone()])))
            }
            (t @ Type::ByRef(_), u) | (u, t @ Type::ByRef(_)) => {
                Ok(Type::Set(Arc::from_iter([t.clone(), u.clone()])))
            }
        }
    }

    pub fn union(&self, env: &Env, t: &Self) -> Result<Self> {
        Ok(self.union_int(env, &mut LPooled::take(), t)?.normalize())
    }

    fn diff_int(
        &self,
        env: &Env,
        hist: &mut FxHashMap<(usize, usize), Type>,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (
                Type::Ref { scope: s0, name: n0, .. },
                Type::Ref { scope: s1, name: n1, .. },
            ) if s0 == s1 && n0 == n1 => Ok(Type::Primitive(BitFlags::empty())),
            (t0 @ Type::Ref { .. }, t1) | (t0, t1 @ Type::Ref { .. }) => {
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                let t0_addr = (t0 as *const Type).addr();
                let t1_addr = (t1 as *const Type).addr();
                match hist.get(&(t0_addr, t1_addr)) {
                    Some(r) => Ok(r.clone()),
                    None => {
                        let r = Type::Primitive(BitFlags::empty());
                        hist.insert((t0_addr, t1_addr), r);
                        match t0.diff_int(env, hist, &t1) {
                            Ok(r) => {
                                hist.insert((t0_addr, t1_addr), r.clone());
                                Ok(r)
                            }
                            Err(e) => {
                                hist.remove(&(t0_addr, t1_addr));
                                Err(e)
                            }
                        }
                    }
                }
            }
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
            (Type::Set(s), t) => Ok(Self::flatten_set(
                s.iter()
                    .map(|s| s.diff_int(env, hist, t))
                    .collect::<Result<LPooled<Vec<_>>>>()?
                    .drain(..),
            )),
            (t, Type::Set(s)) => {
                let mut t = t.clone();
                for st in s.iter() {
                    t = t.diff_int(env, hist, st)?;
                }
                Ok(t)
            }
            (Type::Tuple(t0), Type::Tuple(t1)) => {
                if t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Struct(t0), Type::Struct(t1)) => {
                if t0.len() == t1.len() && t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Variant(tg0, t0), Type::Variant(tg1, t1)) => {
                if tg0 == tg1 && t0.len() == t1.len() && t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                if k0 == k1 && v0 == v1 {
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
            (Type::Primitive(p), Type::Map { key, value }) => {
                if **key == Type::Any && **value == Type::Any {
                    let mut p = *p;
                    p.remove(Typ::Map);
                    Ok(Type::Primitive(p))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                if f0 == f1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(Type::Fn(f0.clone()))
                }
            }
            (Type::TVar(tv0), Type::TVar(tv1)) => {
                if tv0.read().typ.as_ptr() == tv1.read().typ.as_ptr() {
                    return Ok(Type::Primitive(BitFlags::empty()));
                }
                Ok(match (&*tv0.read().typ.read(), &*tv1.read().typ.read()) {
                    (None, _) | (_, None) => Type::TVar(tv0.clone()),
                    (Some(t0), Some(t1)) => t0.diff_int(env, hist, t1)?,
                })
            }
            (Type::TVar(tv), t) => Ok(match &*tv.read().typ.read() {
                Some(tv) => tv.diff_int(env, hist, t)?,
                None => self.clone(),
            }),
            (t, Type::TVar(tv)) => Ok(match &*tv.read().typ.read() {
                Some(tv) => t.diff_int(env, hist, tv)?,
                None => self.clone(),
            }),
            (Type::Array(t0), Type::Array(t1)) => {
                if t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(Type::Array(Arc::new(t0.diff_int(env, hist, t1)?)))
                }
            }
            (Type::Primitive(p), Type::Array(t)) => {
                if &**t == &Type::Any {
                    let mut s = *p;
                    s.remove(Typ::Array);
                    Ok(Type::Primitive(s))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (
                Type::Array(_) | Type::Struct(_) | Type::Tuple(_) | Type::Variant(_, _),
                Type::Primitive(p),
            ) => {
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
                if &**e == &Type::Any {
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
                    Ok(Type::Error(Arc::new(e0.diff_int(env, hist, e1)?)))
                }
            }
            (Type::ByRef(t0), Type::ByRef(t1)) => {
                Ok(Type::ByRef(Arc::new(t0.diff_int(env, hist, t1)?)))
            }
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) if id0 == id1 && p0 == p1 => Ok(Type::Primitive(BitFlags::empty())),
            (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Fn(_), _)
            | (_, Type::Fn(_))
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _), _)
            | (_, Type::Variant(_, _))
            | (Type::ByRef(_), _)
            | (_, Type::ByRef(_))
            | (Type::Error(_), _)
            | (_, Type::Error(_))
            | (Type::Primitive(_), _)
            | (_, Type::Primitive(_))
            | (Type::Bottom, _)
            | (Type::Map { .. }, _) => Ok(self.clone()),
        }
    }

    pub fn diff(&self, env: &Env, t: &Self) -> Result<Self> {
        Ok(self.diff_int(env, &mut LPooled::take(), t)?.normalize())
    }
}
