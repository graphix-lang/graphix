use crate::{
    env::Env,
    typ::{RefHist, Type, TypeRef},
};
use ahash::AHashMap;
use anyhow::Result;
use enumflags2::BitFlags;
use netidx::publisher::Typ;
use poolshark::local::LPooled;
use std::iter;
use triomphe::Arc;

/// Structural identity for union-collapse decisions. Exactly
/// `Type::eq` EXCEPT that two unbound TVars are identical only when
/// they share a cell: `TVar::eq` calls None == None equal, and a
/// collapse on that verdict discards a cell whose future binding may
/// diverge from the survivor's. The direct (TVar, TVar) hole dropped a
/// select arm's `Error<_>` from the arm union (soak jul05 item 11);
/// the same hole one level down — derived `Type::eq` recursing into
/// `TVar::eq` — collapsed `{y: 'a} ∪ {y: 'b}` while both DynCall arms
/// were still unresolved, the string arm's type vanished, and the
/// fused field read leaked the string payload as an i64 (item 18).
/// `Fn` keeps plain equality: two structurally-equal signatures are
/// the same type (alpha equivalence), and fn-typed values don't fuse.
pub(super) fn union_identical(t0: &Type, t1: &Type) -> bool {
    match (t0, t1) {
        (Type::TVar(a), Type::TVar(b)) => {
            let ai = a.read();
            let bi = b.read();
            Arc::ptr_eq(&ai.typ, &bi.typ) || {
                let ab = ai.typ.read();
                let bb = bi.typ.read();
                match (&ab.typ, &bb.typ) {
                    (Some(x), Some(y)) => union_identical(x, y),
                    _ => false,
                }
            }
        }
        // A BOUND tvar against a bare type compares through the
        // binding — same deref the (TVar, TVar) arm already does for
        // two bound cells, and equally safe (a bound cell never
        // rebinds). An UNBOUND tvar stays non-identical to everything
        // but its own cell (items 11/18 above). Without this,
        // `'x: Array<'a: i64>` vs `Array<i64>` failed to collapse and
        // a select-arm union over equal concrete types survived.
        (Type::TVar(a), t) | (t, Type::TVar(a)) => {
            let ai = a.read();
            let ab = ai.typ.read();
            match &ab.typ {
                Some(x) => union_identical(x, t),
                None => false,
            }
        }
        (Type::Bottom, Type::Bottom) | (Type::Any, Type::Any) => true,
        (Type::Primitive(a), Type::Primitive(b)) => a == b,
        (
            Type::Abstract { id: i0, params: p0 },
            Type::Abstract { id: i1, params: p1 },
        ) => {
            i0 == i1
                && p0.len() == p1.len()
                && p0.iter().zip(p1.iter()).all(|(a, b)| union_identical(a, b))
        }
        (Type::Ref(r0), Type::Ref(r1)) => {
            r0.scope == r1.scope
                && r0.name == r1.name
                && r0.cells_agree(r1)
                && r0.params.len() == r1.params.len()
                && r0
                    .params
                    .iter()
                    .zip(r1.params.iter())
                    .all(|(a, b)| union_identical(a, b))
        }
        (Type::Set(s0), Type::Set(s1)) => {
            s0.len() == s1.len()
                && s0.iter().zip(s1.iter()).all(|(a, b)| union_identical(a, b))
        }
        (Type::Error(a), Type::Error(b)) => union_identical(a, b),
        (Type::Array(a), Type::Array(b)) => union_identical(a, b),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            union_identical(k0, k1) && union_identical(v0, v1)
        }
        (Type::ByRef(a), Type::ByRef(b)) => union_identical(a, b),
        (Type::Tuple(a), Type::Tuple(b)) => {
            a.len() == b.len()
                && a.iter().zip(b.iter()).all(|(x, y)| union_identical(x, y))
        }
        (Type::Struct(a), Type::Struct(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|((n0, x), (n1, y))| n0 == n1 && union_identical(x, y))
        }
        (Type::Variant(tg0, a), Type::Variant(tg1, b)) => {
            tg0 == tg1
                && a.len() == b.len()
                && a.iter().zip(b.iter()).all(|(x, y)| union_identical(x, y))
        }
        (Type::Fn(f0), Type::Fn(f1)) => f0 == f1,
        _ => false,
    }
}

impl Type {
    fn union_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), Type>>,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (Type::Ref(t0), Type::Ref(t1))
                if t0.name == t1.name
                    && t0.scope == t1.scope
                    && t0.cells_agree(t1)
                    && t0.params.len() == t1.params.len() =>
            {
                let mut params = t0
                    .params
                    .iter()
                    .zip(t1.params.iter())
                    .map(|(p0, p1)| p0.union_int(env, hist, p1))
                    .collect::<Result<LPooled<Vec<_>>>>()?;
                let params = Arc::from_iter(params.drain(..));
                Ok(Self::Ref(t0.with_params(params)))
            }
            (tr @ Type::Ref(TypeRef { .. }), t) => {
                let t0_id = hist.ref_id(tr, env);
                let t_id = hist.ref_id(t, env);
                let t0 = tr.lookup_ref(env)?;
                match hist.get(&(t0_id, t_id)) {
                    Some(t) => Ok(t.clone()),
                    None => {
                        hist.insert((t0_id, t_id), tr.clone());
                        let r = t0.union_int(env, hist, t);
                        hist.remove(&(t0_id, t_id));
                        r
                    }
                }
            }
            (t, tr @ Type::Ref(TypeRef { .. })) => {
                let t_id = hist.ref_id(t, env);
                let t1_id = hist.ref_id(tr, env);
                let t1 = tr.lookup_ref(env)?;
                match hist.get(&(t_id, t1_id)) {
                    Some(t) => Ok(t.clone()),
                    None => {
                        hist.insert((t_id, t1_id), tr.clone());
                        let r = t.union_int(env, hist, &t1);
                        hist.remove(&(t_id, t1_id));
                        r
                    }
                }
            }
            (t0 @ Type::Abstract { .. }, t1 @ Type::Abstract { .. })
                if union_identical(t0, t1) =>
            {
                Ok(self.clone())
            }
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
            (t @ Type::Array(t0), u @ Type::Array(_)) => {
                if union_identical(t, u) {
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
            (t @ Type::Map { key: k0, value: v0 }, u @ Type::Map { .. }) => {
                if union_identical(t, u) {
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
            (t @ Type::ByRef(t0), u @ Type::ByRef(_)) => {
                if union_identical(t, u) {
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
            (u @ Type::Struct(_), t @ Type::Struct(_)) => {
                if union_identical(u, t) {
                    Ok(u.clone())
                } else {
                    Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
                }
            }
            (u @ Type::Struct(_), t) | (t, u @ Type::Struct(_)) => {
                Ok(Type::Set(Arc::from_iter([u.clone(), t.clone()])))
            }
            (u @ Type::Tuple(_), t @ Type::Tuple(_)) => {
                if union_identical(u, t) {
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
                // See `union_identical` — a bare `t0 == t1` here
                // collapsed two DISTINCT unbound cells and dropped the
                // discarded cell's future binding (item 11).
                if union_identical(t0, t1) {
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
        Ok(self.union_int(env, &mut RefHist::new(LPooled::take()), t)?.normalize())
    }

    fn diff_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), Type>>,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (Type::Ref(tr0), Type::Ref(tr1))
                if tr0.scope == tr1.scope
                    && tr0.name == tr1.name
                    && tr0.cells_agree(tr1) =>
            {
                Ok(Type::Primitive(BitFlags::empty()))
            }
            (t0 @ Type::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Type::Ref(TypeRef { .. })) => {
                let t0_id = hist.ref_id(t0, env);
                let t1_id = hist.ref_id(t1, env);
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                match hist.get(&(t0_id, t1_id)) {
                    Some(r) => Ok(r.clone()),
                    None => {
                        let r = Type::Primitive(BitFlags::empty());
                        hist.insert((t0_id, t1_id), r);
                        let r = t0.diff_int(env, hist, &t1);
                        hist.remove(&(t0_id, t1_id));
                        r
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
            (Type::TVar(tv0), t1 @ Type::TVar(tv1)) => {
                if Arc::ptr_eq(&tv0.read().typ, &tv1.read().typ) {
                    return Ok(Type::Primitive(BitFlags::empty()));
                }
                Ok(match (&tv0.read().typ.read().typ, &tv1.read().typ.read().typ) {
                    (None, _) => Type::TVar(tv0.clone()),
                    (Some(t0), None) => t0.diff_int(env, hist, t1)?,
                    (Some(t0), Some(t1)) => t0.diff_int(env, hist, t1)?,
                })
            }
            (Type::TVar(tv), t) => Ok(match &tv.read().typ.read().typ {
                Some(tv) => tv.diff_int(env, hist, t)?,
                None => self.clone(),
            }),
            (t, Type::TVar(tv)) => Ok(match &tv.read().typ.read().typ {
                Some(tv) => t.diff_int(env, hist, tv)?,
                None => self.clone(),
            }),
            (Type::Array(t0), Type::Array(t1)) => {
                if t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    // An emptied element type empties the ARRAY type:
                    // leaving `Array<[]>` (uninhabited, but nonempty to
                    // the containment walk) in a select residue blocked
                    // catch-all narrowing for instantiated generics
                    // (`Array<'b:=i64>` subtracted from
                    // `[i64, Array<i64>]` left `[i64, Array<[]>]`).
                    match t0.diff_int(env, hist, t1)? {
                        Type::Primitive(p) if p.is_empty() => {
                            Ok(Type::Primitive(BitFlags::empty()))
                        }
                        d => Ok(Type::Array(Arc::new(d))),
                    }
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
                    match e0.diff_int(env, hist, e1)? {
                        Type::Primitive(p) if p.is_empty() => {
                            Ok(Type::Primitive(BitFlags::empty()))
                        }
                        d => Ok(Type::Error(Arc::new(d))),
                    }
                }
            }
            (Type::ByRef(t0), Type::ByRef(t1)) => match t0.diff_int(env, hist, t1)? {
                Type::Primitive(p) if p.is_empty() => {
                    Ok(Type::Primitive(BitFlags::empty()))
                }
                d => Ok(Type::ByRef(Arc::new(d))),
            },
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
        Ok(self.diff_int(env, &mut RefHist::new(LPooled::take()), t)?.normalize())
    }
}
