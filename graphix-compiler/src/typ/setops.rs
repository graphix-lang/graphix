use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{RefHist, RefPair, Type, TypeRef},
};
use ahash::AHashMap;
use anyhow::Result;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::iter;
use triomphe::Arc;

fn is_empty_ty(t: &Type) -> bool {
    match t {
        Type::Bottom => true,
        Type::Primitive(p) => p.is_empty(),
        _ => false,
    }
}

impl Type {
    /// No value inhabits this type: bottom, the empty primitive set, or
    /// an empty union (a `diff` residual with nothing left).
    pub(crate) fn is_uninhabited(&self) -> bool {
        match self {
            Type::Set(ts) => ts.is_empty(),
            t => is_empty_ty(t),
        }
    }
}

fn diff_already_normal(before: &Type, after: &Type) -> bool {
    match (before, after) {
        (_, Type::Primitive(p)) if p.is_empty() => true,
        (Type::Set(s), Type::Set(out)) => {
            let mut i = 0;
            for o in out.iter() {
                loop {
                    if i >= s.len() {
                        return false;
                    }
                    let j = i;
                    i += 1;
                    if s[j] == *o {
                        break;
                    }
                }
            }
            true
        }
        (Type::Set(s), t) => s.iter().any(|m| m == t),
        _ => false,
    }
}

/// Structural identity for union-collapse decisions: `Type::eq`,
/// except that two unbound TVars are identical only when they share a
/// cell (a collapse must not discard a cell whose future binding may
/// diverge). `Fn` keeps plain equality.
pub(super) fn union_identical(t0: &Type, t1: &Type) -> bool {
    ensure_sufficient(|| union_identical_inner(t0, t1))
}

fn union_identical_inner(t0: &Type, t1: &Type) -> bool {
    let all = |a: &[Type], b: &[Type]| {
        a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| union_identical(a, b))
    };
    match (t0, t1) {
        (Type::TVar(a), Type::TVar(b)) => {
            a.same_cell(b)
                || match (a.binding(), b.binding()) {
                    (Some(x), Some(y)) => union_identical(&x, &y),
                    _ => false,
                }
        }
        // A bound tvar compares through its binding; an unbound one
        // is identical only to its own cell.
        (Type::TVar(a), t) | (t, Type::TVar(a)) => {
            a.binding().is_some_and(|x| union_identical(&x, t))
        }
        (Type::Bottom, Type::Bottom) | (Type::Any, Type::Any) | (Type::Hole, Type::Hole) => {
            true
        }
        (Type::App(c0, a0), Type::App(c1, a1)) => {
            union_identical(c0, c1) && union_identical(a0, a1)
        }
        (Type::Primitive(a), Type::Primitive(b)) => a == b,
        (
            Type::Abstract { id: i0, params: p0 },
            Type::Abstract { id: i1, params: p1 },
        ) => i0 == i1 && all(p0, p1),
        (Type::Ref(r0), Type::Ref(r1)) => {
            r0.scope == r1.scope
                && r0.name == r1.name
                && r0.cells_agree(r1)
                && all(&r0.params, &r1.params)
        }
        (Type::Set(s0), Type::Set(s1)) => all(s0, s1),
        (Type::Error(a), Type::Error(b))
        | (Type::Array(a), Type::Array(b))
        | (Type::List(a), Type::List(b))
        | (Type::ByRef(a), Type::ByRef(b)) => union_identical(a, b),
        (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
            union_identical(k0, k1) && union_identical(v0, v1)
        }
        (Type::Tuple(a), Type::Tuple(b)) => all(a, b),
        (Type::Struct(a), Type::Struct(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|((n0, x, _), (n1, y, _))| n0 == n1 && union_identical(x, y))
        }
        (Type::Variant(tg0, a, _), Type::Variant(tg1, b, _)) => tg0 == tg1 && all(a, b),
        (Type::Fn(f0), Type::Fn(f1)) => f0 == f1,
        _ => false,
    }
}

/// Two same-named refs to one definition with identical params.
fn same_ref(r0: &TypeRef, r1: &TypeRef) -> bool {
    r0.scope == r1.scope
        && r0.name == r1.name
        && r0.cells_agree(r1)
        && r0.params.len() == r1.params.len()
        && r0.params.iter().zip(r1.params.iter()).all(|(a, b)| union_identical(a, b))
}

/// The two-member union of `a` and `b`.
fn pair(a: &Type, b: &Type) -> Type {
    Type::Set(Arc::from_iter([a.clone(), b.clone()]))
}

impl Type {
    fn union_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        t: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| self.union_inner(env, hist, t))
    }

    fn union_inner(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            // Same-named refs merge only with identical params: a
            // param-wise union is an upper bound only for a covariant
            // parameter, and with two it invents pairs.
            (Type::Ref(t0), Type::Ref(t1)) if same_ref(t0, t1) => Ok(self.clone()),
            (Type::Ref(t0), Type::Ref(t1))
                if t0.name == t1.name && t0.scope == t1.scope && t0.cells_agree(t1) =>
            {
                Ok(pair(self, t))
            }
            (tr @ Type::Ref(TypeRef { .. }), t) | (t, tr @ Type::Ref(TypeRef { .. })) => {
                let key = if matches!(self, Type::Ref(_)) {
                    (hist.ref_id(tr, env), hist.ref_id(t, env))
                } else {
                    (hist.ref_id(t, env), hist.ref_id(tr, env))
                };
                if let Some(u) = hist.get(&key) {
                    return Ok(u.clone());
                }
                let e = tr.lookup_ref(env)?;
                hist.insert(key, tr.clone());
                let r = if matches!(self, Type::Ref(_)) {
                    e.union_int(env, hist, t)
                } else {
                    t.union_int(env, hist, &e)
                };
                hist.remove(&key);
                r
            }
            (t0 @ Type::Abstract { .. }, t1 @ Type::Abstract { .. })
                if union_identical(t0, t1) =>
            {
                Ok(self.clone())
            }
            (t0 @ Type::Abstract { .. }, t1) | (t0, t1 @ Type::Abstract { .. }) => {
                Ok(pair(t0, t1))
            }
            (Type::Bottom, t) | (t, Type::Bottom) => Ok(t.clone()),
            (Type::Any, _) | (_, Type::Any) => Ok(Type::Any),
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole) => {
                if self == t {
                    Ok(self.clone())
                } else {
                    Ok(pair(self, t))
                }
            }
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
                Type::Array(_) | Type::Struct(_) | Type::Tuple(_) | Type::Variant(..),
            )
            | (
                Type::Array(_) | Type::Struct(_) | Type::Tuple(_) | Type::Variant(..),
                Type::Primitive(p),
            ) if p.contains(Typ::Array) => Ok(Type::Primitive(*p)),
            (Type::Primitive(p), Type::Map { .. })
            | (Type::Map { .. }, Type::Primitive(p))
                if p.contains(Typ::Map) =>
            {
                Ok(Type::Primitive(*p))
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
            (Type::Set(s0), Type::Set(s1)) => Ok(Type::Set(Arc::from_iter(
                s0.iter().cloned().chain(s1.iter().cloned()),
            ))),
            (Type::Set(s), t) | (t, Type::Set(s)) => Ok(Type::Set(Arc::from_iter(
                s.iter().cloned().chain(iter::once(t.clone())),
            ))),
            (u @ Type::Variant(tg0, t0, at), t @ Type::Variant(tg1, t1, _)) => {
                // Component-wise union is exact only when at most one
                // position differs: `P(A, X) ∪ `P(B, Y) is not
                // `P([A, B], [X, Y]) (that invents `P(A, Y)).
                let differing = || {
                    t0.iter()
                        .zip(t1.iter())
                        .filter(|(a, b)| !union_identical(a, b))
                        .count()
                };
                if tg0 == tg1 && t0.len() == t1.len() && differing() <= 1 {
                    let mut typs = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.union_int(env, hist, t1))
                        .collect::<Result<LPooled<Vec<_>>>>()?;
                    Ok(Type::Variant(tg0.clone(), Arc::from_iter(typs.drain(..)), *at))
                } else {
                    Ok(pair(u, t))
                }
            }
            (t0 @ Type::TVar(_), t1 @ Type::TVar(_)) if union_identical(t0, t1) => {
                Ok(t0.clone())
            }
            // A bound cell unions as its binding; an unbound cell
            // stays its own member.
            (Type::TVar(tv), t1) => match tv.binding() {
                Some(b) => b.union_int(env, hist, t1),
                None => Ok(pair(self, t1)),
            },
            (t0, Type::TVar(tv)) => match tv.binding() {
                Some(b) => t0.union_int(env, hist, &b),
                None => Ok(pair(t0, t)),
            },
            // Everything left of one variant is kept once if identical,
            // else as two members.
            (t0, t1)
                if std::mem::discriminant(t0) == std::mem::discriminant(t1)
                    && union_identical(t0, t1) =>
            {
                Ok(t0.clone())
            }
            (t0, t1) => Ok(pair(t0, t1)),
        }
    }

    pub fn union(env: &Env, ts: &[&Type]) -> Result<Self> {
        let mut iter = ts.iter().copied();
        let Some(first) = iter.next() else {
            return Ok(Type::Primitive(BitFlags::empty()));
        };
        let mut hist = RefHist::new();
        let mut acc = first.clone();
        for t in iter {
            acc = acc.union_int(env, &mut hist, t)?;
        }
        Ok(acc.normalize())
    }

    fn diff_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        t: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| self.diff_inner(env, hist, t))
    }

    fn diff_inner(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (Type::Ref(tr0), Type::Ref(tr1)) if same_ref(tr0, tr1) => {
                Ok(Type::Primitive(BitFlags::empty()))
            }
            (t0 @ Type::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Type::Ref(TypeRef { .. })) => {
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                if let Some(r) = hist.get(&key) {
                    return Ok(r.clone());
                }
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                hist.insert(key, Type::Primitive(BitFlags::empty()));
                let r = t0.diff_int(env, hist, &t1);
                hist.remove(&key);
                r
            }
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole) => Ok(if self == t {
                Type::Primitive(BitFlags::empty())
            } else {
                self.clone()
            }),
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
            (Type::Set(s), t) => {
                let mut diffs: LPooled<Vec<Type>> = LPooled::take();
                let mut partial = false;
                for m in s.iter() {
                    let d = m.diff_int(env, hist, t)?;
                    if !is_empty_ty(&d) && d != *m {
                        partial = true;
                    }
                    diffs.push(d);
                }
                if partial {
                    Ok(Self::flatten_set(diffs.drain(..)))
                } else {
                    diffs.retain(|d| !is_empty_ty(d));
                    Ok(match diffs.len() {
                        0 => Type::Primitive(BitFlags::empty()),
                        1 => diffs.pop().unwrap(),
                        _ => Type::Set(Arc::from_iter(diffs.drain(..))),
                    })
                }
            }
            (t, Type::Set(s)) => {
                let mut t = t.clone();
                for st in s.iter() {
                    t = t.diff_int(env, hist, st)?;
                }
                Ok(t)
            }
            (Type::Tuple(t0), Type::Tuple(t1)) => {
                if same_resolved(t0.iter(), t1.iter()) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Struct(t0), Type::Struct(t1)) => {
                let same = t0.len() == t1.len()
                    && t0.iter().zip(t1.iter()).all(|((n0, _, _), (n1, _, _))| n0 == n1)
                    && same_resolved(
                        t0.iter().map(|(_, t, _)| t),
                        t1.iter().map(|(_, t, _)| t),
                    );
                if same {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Variant(tg0, t0, _), Type::Variant(tg1, t1, _)) => {
                if tg0 == tg1 && same_resolved(t0.iter(), t1.iter()) {
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
            // Bindings are cloned out: the recursion may lock these cells.
            (Type::TVar(tv0), t1 @ Type::TVar(tv1)) => {
                if tv0.same_cell(tv1) {
                    return Ok(Type::Primitive(BitFlags::empty()));
                }
                Ok(match (tv0.binding(), tv1.binding()) {
                    (None, _) => Type::TVar(tv0.clone()),
                    (Some(t0), None) => t0.diff_int(env, hist, t1)?,
                    (Some(t0), Some(t1)) => t0.diff_int(env, hist, &t1)?,
                })
            }
            (Type::TVar(tv), t) => Ok(match tv.binding() {
                Some(tv) => tv.diff_int(env, hist, t)?,
                None => self.clone(),
            }),
            (t, Type::TVar(tv)) => Ok(match tv.binding() {
                Some(tv) => t.diff_int(env, hist, &tv)?,
                None => self.clone(),
            }),
            (Type::List(t0), Type::List(t1)) => {
                if t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    match t0.diff_int(env, hist, t1)? {
                        Type::Primitive(p) if p.is_empty() => {
                            Ok(Type::Primitive(BitFlags::empty()))
                        }
                        d => Ok(Type::List(Arc::new(d))),
                    }
                }
            }
            (Type::Array(t0), Type::Array(t1)) => {
                if t0 == t1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    // An emptied element type empties the array type;
                    // `Array<[]>` would be uninhabited but nonempty to
                    // the containment walk.
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
                Type::Array(_) | Type::Struct(_) | Type::Tuple(_) | Type::Variant(..),
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
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
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
        let r = self.diff_int(env, &mut RefHist::new(), t)?;
        if diff_already_normal(self, &r) { Ok(r) } else { Ok(r.normalize()) }
    }
}

/// Positional equality read through bound type variables. `Any` in
/// the subtrahend covers anything.
fn same_resolved<'a, 'b>(
    t0: impl IntoIterator<Item = &'a Type>,
    t1: impl IntoIterator<Item = &'b Type>,
) -> bool {
    let mut t0 = t0.into_iter();
    let mut t1 = t1.into_iter();
    loop {
        match (t0.next(), t1.next()) {
            (None, None) => return true,
            (Some(a), Some(b)) => {
                if a != b && *b != Type::Any && a.resolve_tvars() != b.resolve_tvars() {
                    return false;
                }
            }
            _ => return false,
        }
    }
}
