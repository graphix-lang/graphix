use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG,
    env::Env,
    errf,
    typ::{RefHist, Type, TypeRef},
};
use ahash::AHashSet;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::{BitFlags, bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::ValArray;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::iter;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum IsAFlags {
    /// When set, a `Type::Abstract` test accepts any RUST-BACKED
    /// abstract value whose wrapper UUID is not the type's
    /// path-derived one (`abstract_uuid`) — the lenient reading for
    /// packages that still register ad-hoc UUIDs — the stdlib registers
    /// path-derived ones (`abstract_wrapper!`), so this only covers a
    /// third-party package that has not. A Graphix-minted box always
    /// answers by its tag, and a non-abstract value never matches
    /// (`design/nominal_abstract_types.md`). Consumers: the `TVal`
    /// printer and INFERRED select predicates; an explicit `T as t` is
    /// strict.
    MatchAbstract,
    /// When set, the type-blind leaves — `Any`, `⊥`, and an unbound
    /// tvar — match NOTHING instead of everything. `is_a` answers
    /// "could v inhabit this type"; the blind leaves answer true for
    /// any value, which is right for dispatch but wrong for a walk
    /// asking "does this type DESCRIBE v" — the `TVal` printer's
    /// union-member selection uses this to prefer members whose every
    /// leaf positively matched, so a `never()` arm's ⊥-settled cell
    /// can't claim a value no matter how deeply it is nested (the
    /// top-level-only informative test missed `Array<Array<[i64, ⊥]>>`
    /// — aug04f divergence_000000).
    Strict,
}

impl Type {
    fn check_cast_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashSet<Option<usize>>>,
    ) -> Result<()> {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.check_cast_int(env, hist),
                None => bail!("can't cast a value to a type constructor"),
            },
            Type::Hole => bail!("can't cast a value to a type constructor"),
            Type::Primitive(_) | Type::Any => Ok(()),
            Type::Fn(_) => bail!("can't cast a value to a function"),
            Type::Bottom => bail!("can't cast a value to bottom"),
            Type::Set(s) => Ok(for t in s.iter() {
                t.check_cast_int(env, hist)?
            }),
            Type::Abstract { .. } => {
                bail!("can't cast a value to an abstract type; use its constructor")
            }
            Type::TVar(tv) => match &tv.read().typ.read().typ {
                Some(t) => t.check_cast_int(env, hist),
                None => bail!("can't cast a value to a free type variable"),
            },
            Type::Error(e) => e.check_cast_int(env, hist),
            Type::Array(et) => et.check_cast_int(env, hist),
            Type::List(et) => et.check_cast_int(env, hist),
            Type::Map { key, value } => {
                key.check_cast_int(env, hist)?;
                value.check_cast_int(env, hist)
            }
            Type::ByRef(_) => bail!("can't cast a reference"),
            Type::Tuple(ts) => Ok(for t in ts.iter() {
                t.check_cast_int(env, hist)?
            }),
            Type::Struct(ts) => Ok(for (_, t) in ts.iter() {
                t.check_cast_int(env, hist)?
            }),
            Type::Variant(_, ts) => Ok(for t in ts.iter() {
                t.check_cast_int(env, hist)?
            }),
            Type::Ref(TypeRef { .. }) => {
                let id = hist.ref_id(self, env);
                let t = self.lookup_ref(env)?;
                if hist.contains(&id) {
                    Ok(())
                } else {
                    hist.insert(id);
                    t.check_cast_int(env, hist)
                }
            }
        }
    }

    pub fn check_cast(&self, env: &Env) -> Result<()> {
        self.check_cast_int(env, &mut RefHist::new(LPooled::take()))
    }

    fn cast_value_int(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        v: Value,
    ) -> Result<Value> {
        if self.is_a_int(env, hist, BitFlags::empty(), &v) {
            return Ok(v);
        }
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.cast_value_int(env, hist, v),
                None => bail!("can't cast {v} to a type constructor"),
            },
            Type::Hole => bail!("can't cast {v} to a type constructor"),
            Type::Bottom => bail!("can't cast {v} to Bottom"),
            Type::Fn(_) => bail!("can't cast {v} to a function"),
            Type::Abstract { id: _, params: _ } => {
                bail!("can't cast {v} to an abstract type")
            }
            Type::ByRef(_) => bail!("can't cast {v} to a reference"),
            Type::Primitive(s) => s
                .iter()
                .find_map(|t| v.clone().cast(t))
                .ok_or_else(|| anyhow!("can't cast {v} to {self}")),
            Type::Any => Ok(v),
            Type::Error(e) => {
                let v = match v {
                    Value::Error(v) => (*v).clone(),
                    v => v,
                };
                Ok(Value::Error(e.cast_value_int(env, hist, v)?.into()))
            }
            Type::Array(et) => match v {
                Value::Array(elts) => {
                    let mut va = elts
                        .iter()
                        .map(|el| et.cast_value_int(env, hist, el.clone()))
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(Value::Array(ValArray::from_iter_exact(va.drain(..))))
                }
                v => Ok(Value::Array([et.cast_value_int(env, hist, v)?].into())),
            },
            // cast<List<T>>: a list value casts element-wise in place; a
            // plain array converts (from_array semantics); anything else
            // becomes a singleton — mirroring Array's rules over the
            // list rep.
            Type::List(et) => {
                use crate::node::collection::list;
                if list::is_list(&v) {
                    let mut elems = list::Iter::new(v.clone())
                        .map(|el| et.cast_value_int(env, hist, el))
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(list::from_iter(elems.drain(..)))
                } else {
                    match v {
                        Value::Array(elts) => {
                            let mut elems = elts
                                .iter()
                                .map(|el| et.cast_value_int(env, hist, el.clone()))
                                .collect::<Result<LPooled<Vec<Value>>>>()?;
                            Ok(list::from_iter(elems.drain(..)))
                        }
                        v => Ok(list::from_iter([et.cast_value_int(env, hist, v)?])),
                    }
                }
            }
            Type::Map { key, value } => match v {
                Value::Map(m) => {
                    let mut m = m
                        .into_iter()
                        .map(|(k, v)| {
                            Ok((
                                key.cast_value_int(env, hist, k.clone())?,
                                value.cast_value_int(env, hist, v.clone())?,
                            ))
                        })
                        .collect::<Result<LPooled<Vec<(Value, Value)>>>>()?;
                    Ok(Value::Map(Map::from_iter(m.drain(..))))
                }
                Value::Array(a) => {
                    let mut m = a
                        .iter()
                        .map(|a| match a {
                            Value::Array(a) if a.len() == 2 => Ok((
                                key.cast_value_int(env, hist, a[0].clone())?,
                                value.cast_value_int(env, hist, a[1].clone())?,
                            )),
                            _ => bail!("expected an array of pairs"),
                        })
                        .collect::<Result<LPooled<Vec<(Value, Value)>>>>()?;
                    Ok(Value::Map(Map::from_iter(m.drain(..))))
                }
                _ => bail!("can't cast {v} to {self}"),
            },
            Type::Tuple(ts) => match v {
                Value::Array(elts) => {
                    if elts.len() != ts.len() {
                        bail!("tuple size mismatch {self} with {}", Value::Array(elts))
                    }
                    let mut a = ts
                        .iter()
                        .zip(elts.iter())
                        .map(|(t, el)| t.cast_value_int(env, hist, el.clone()))
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(Value::Array(ValArray::from_iter_exact(a.drain(..))))
                }
                v => bail!("can't cast {v} to {self}"),
            },
            Type::Struct(ts) => match v {
                Value::Array(elts) => {
                    if elts.len() != ts.len() {
                        bail!("struct size mismatch {self} with {}", Value::Array(elts))
                    }
                    let is_pairs = elts.iter().all(|v| match v {
                        Value::Array(a) if a.len() == 2 => match &a[0] {
                            Value::String(_) => true,
                            _ => false,
                        },
                        _ => false,
                    });
                    if !is_pairs {
                        bail!("expected array of pairs, got {}", Value::Array(elts))
                    }
                    let mut elts_s: LPooled<Vec<&Value>> = elts.iter().collect();
                    elts_s.sort_by_key(|v| match v {
                        Value::Array(a) => match &a[0] {
                            Value::String(s) => s,
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    });
                    let keys_ok = ts.iter().zip(elts_s.iter()).fold(
                        Ok(true),
                        |acc: Result<_>, ((fname, t), v)| {
                            let kok = acc?;
                            let (name, v) = match v {
                                Value::Array(a) => match (&a[0], &a[1]) {
                                    (Value::String(n), v) => (n, v),
                                    _ => unreachable!(),
                                },
                                _ => unreachable!(),
                            };
                            Ok(kok
                                && name == fname
                                && t.contains(env, &Type::Primitive(Typ::get(v).into()))?)
                        },
                    )?;
                    if keys_ok {
                        let mut elts = ts
                            .iter()
                            .zip(elts_s.iter())
                            .map(|((n, t), v)| match v {
                                Value::Array(a) => {
                                    let a = [
                                        Value::String(n.clone()),
                                        t.cast_value_int(env, hist, a[1].clone())?,
                                    ];
                                    Ok(Value::Array(ValArray::from_iter_exact(
                                        a.into_iter(),
                                    )))
                                }
                                _ => unreachable!(),
                            })
                            .collect::<Result<LPooled<Vec<Value>>>>()?;
                        Ok(Value::Array(ValArray::from_iter_exact(elts.drain(..))))
                    } else {
                        drop(elts_s);
                        bail!("struct fields mismatch {self}, {}", Value::Array(elts))
                    }
                }
                v => bail!("can't cast {v} to {self}"),
            },
            Type::Variant(tag, ts) if ts.len() == 0 => match &v {
                Value::String(s) if s == tag => Ok(v),
                _ => bail!("variant tag mismatch expected {tag} got {v}"),
            },
            Type::Variant(tag, ts) => match &v {
                Value::Array(elts) => {
                    if ts.len() + 1 == elts.len() {
                        match &elts[0] {
                            Value::String(s) if s == tag => (),
                            v => bail!("variant tag mismatch expected {tag} got {v}"),
                        }
                        let mut a = iter::once(&Type::Primitive(Typ::String.into()))
                            .chain(ts.iter())
                            .zip(elts.iter())
                            .map(|(t, v)| t.cast_value_int(env, hist, v.clone()))
                            .collect::<Result<LPooled<Vec<Value>>>>()?;
                        Ok(Value::Array(ValArray::from_iter_exact(a.drain(..))))
                    } else if ts.len() == elts.len() {
                        let mut a = ts
                            .iter()
                            .zip(elts.iter())
                            .map(|(t, v)| t.cast_value_int(env, hist, v.clone()))
                            .collect::<Result<LPooled<Vec<Value>>>>()?;
                        a.insert(0, Value::String(tag.clone()));
                        Ok(Value::Array(ValArray::from_iter_exact(a.drain(..))))
                    } else {
                        bail!("variant length mismatch")
                    }
                }
                v => bail!("can't cast {v} to {self}"),
            },
            Type::Ref(TypeRef { .. }) => {
                let t = self.lookup_ref(env)?;
                t.cast_value_int(env, hist, v)
            }
            Type::Set(ts) => ts
                .iter()
                .find_map(|t| t.cast_value_int(env, hist, v.clone()).ok())
                .ok_or_else(|| anyhow!("can't cast {v} to {self}")),
            Type::TVar(tv) => match &tv.read().typ.read().typ {
                Some(t) => t.cast_value_int(env, hist, v.clone()),
                None => Ok(v),
            },
        }
    }

    pub fn cast_value(&self, env: &Env, v: Value) -> Value {
        match self.cast_value_int(env, &mut LPooled::take(), v) {
            Ok(v) => v,
            Err(e) => errf!(CAST_ERR_TAG, "{e:?}"),
        }
    }

    fn is_a_int(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        crate::stack::ensure_sufficient(|| self.is_a_int_inner(env, hist, flags, v))
    }

    fn is_a_int_inner(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        match self {
            // `hist` is the CURRENT PATH, not a visited set: the entry
            // comes back out on the way up.
            //
            // It exists to stop a name that expands without consuming
            // value structure (`type T = [T, i64]`) from recursing
            // forever, and a repeat on the path is exactly that. A
            // repeat OFF the path is not: `Type::Set` is a union tried
            // with `any`, so one member descending into a child and
            // failing is ordinary backtracking, and the next member
            // must get to check that same child. Left in the set, the
            // failed branch's entries answered "no match" for every
            // later member — so a select over a recursive ADT matched
            // NO arm, produced nothing, and the whole program wedged
            // idle at zero CPU (bench/symbolic.gx; the wedge needed a
            // union retry over a node a previous member had already
            // walked, which is why it turned on depth and shape).
            //
            // Latent until e86d18c1 made an inferred pattern predicate
            // load-bearing at runtime — before that nothing called
            // `is_a` on these patterns at all.
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.is_a_int(env, hist, flags, v),
                None => !flags.contains(IsAFlags::Strict),
            },
            Type::Hole => false,
            Type::Ref(TypeRef { scope, name, .. }) => match self.lookup_ref(env) {
                Err(_) => false,
                Ok(t) => {
                    let t_addr = (scope.as_ref() as *const _ as *const u8).addr()
                        ^ (name.as_ref() as *const _ as *const u8).addr();
                    let v_addr = (v as *const Value).addr();
                    let key = (t_addr, v_addr);
                    hist.insert(key) && {
                        let r = t.is_a_int(env, hist, flags, v);
                        hist.remove(&key);
                        r
                    }
                }
            },
            Type::Primitive(t) => t.contains(Typ::get(&v)),
            // A Graphix-minted box answers by its tag. A Rust-backed
            // value answers by its wrapper UUID (`abstract_uuid` of
            // the type's path) — or leniently, for packages whose
            // wrappers still carry an ad-hoc UUID.
            Type::Abstract { id, .. } => match v {
                Value::Abstract(a) => {
                    match a.downcast_ref::<crate::abstract_value::GxAbstract>() {
                        Some(g) => g.id == *id,
                        None => {
                            a.id().as_u64_pair().1 == id.inner()
                                || flags.contains(IsAFlags::MatchAbstract)
                        }
                    }
                }
                _ => false,
            },
            Type::Any => !flags.contains(IsAFlags::Strict),
            // Shallowified predicates (`shallow_discriminant`) test the
            // runtime class alone — without these two arms an
            // `Array<Any>`/`Map<Any, Any>` test still walks every
            // element to learn nothing.
            Type::Array(et)
                if matches!(&**et, Type::Any) && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Array(_))
            }
            Type::Array(et) => match v {
                Value::Array(a) => a.iter().all(|v| et.is_a_int(env, hist, flags, v)),
                _ => false,
            },
            // A list value: walk the spine iteratively (heads recurse).
            // The rep shapes as an array, so `Array<Any> as a` also
            // matches a list — inherent to the shared carrier, same as
            // the old variant rep.
            Type::List(et) => {
                use crate::node::collection::list;
                let mut cur = v;
                loop {
                    if list::is_nil(cur) {
                        break true;
                    }
                    match list::split(cur) {
                        Some((h, t)) => {
                            if !et.is_a_int(env, hist, flags, h) {
                                break false;
                            }
                            cur = t;
                        }
                        None => break false,
                    }
                }
            }
            Type::Map { key, value }
                if matches!(&**key, Type::Any)
                    && matches!(&**value, Type::Any)
                    && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Map(_))
            }
            Type::Map { key, value } => match v {
                Value::Map(m) => m.into_iter().all(|(k, v)| {
                    key.is_a_int(env, hist, flags, k)
                        && value.is_a_int(env, hist, flags, v)
                }),
                _ => false,
            },
            Type::Error(e) => match v {
                Value::Error(v) => e.is_a_int(env, hist, flags, v),
                _ => false,
            },
            Type::ByRef(_) => matches!(v, Value::U64(_) | Value::V64(_)),
            Type::Tuple(ts) => match v {
                Value::Array(elts) => {
                    elts.len() == ts.len()
                        && ts
                            .iter()
                            .zip(elts.iter())
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                _ => false,
            },
            Type::Struct(ts) => match v {
                Value::Array(elts) => {
                    elts.len() == ts.len()
                        && ts.iter().zip(elts.iter()).all(|((n, t), v)| match v {
                            Value::Array(a) if a.len() == 2 => match &a[..] {
                                [Value::String(key), v] => {
                                    n == key && t.is_a_int(env, hist, flags, v)
                                }
                                _ => false,
                            },
                            _ => false,
                        })
                }
                _ => false,
            },
            Type::Variant(tag, ts) if ts.len() == 0 => match &v {
                Value::String(s) => s == tag,
                _ => false,
            },
            Type::Variant(tag, ts) => match &v {
                Value::Array(elts) => {
                    ts.len() + 1 == elts.len()
                        && match &elts[0] {
                            Value::String(s) => s == tag,
                            _ => false,
                        }
                        && ts
                            .iter()
                            .zip(elts[1..].iter())
                            .all(|(t, v)| t.is_a_int(env, hist, flags, v))
                }
                _ => false,
            },
            Type::TVar(tv) => match &tv.read().typ.read().typ {
                None => !flags.contains(IsAFlags::Strict),
                Some(t) => t.is_a_int(env, hist, flags, v),
            },
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => true,
                _ => false,
            },
            Type::Bottom => !flags.contains(IsAFlags::Strict),
            Type::Set(ts) => ts.iter().any(|t| t.is_a_int(env, hist, flags, v)),
        }
    }

    /// return true if v is structurally compatible with the type
    pub fn is_a(&self, env: &Env, v: &Value) -> bool {
        self.is_a_int(env, &mut LPooled::take(), BitFlags::empty(), v)
    }

    /// return true if v is structurally compatible with the type, with flags
    pub fn is_a_with(&self, env: &Env, flags: BitFlags<IsAFlags>, v: &Value) -> bool {
        self.is_a_int(env, &mut LPooled::take(), flags, v)
    }

    /// The shallow discriminator for a select arm's INFERRED type
    /// predicate. `Some(shallow)` when telling `self`'s values apart
    /// from the OTHER members of `scrutinee` needs only each value's
    /// outermost shape: the returned type is `self` with every payload
    /// position replaced by `Any`, so `is_a` on it costs O(arity)
    /// instead of walking the VALUE (a `Cons('a, List<'a>)` predicate
    /// walked the whole remaining chain per consult — O(len) per
    /// match, quadratic per traversal; P2b's fold_list fixture,
    /// 2026-08-25). `None` = keep the full walk: the scrutinee's
    /// members can't be enumerated (Any, an unbound tvar, a Ref
    /// cycle), two members share an outermost shape (`[`A(i64),
    /// `A(string)]`, the e86d18c1 tuple-vs-array class), or nothing
    /// in the predicate carries a payload (the full walk is already
    /// O(1) and shallowing gains nothing).
    ///
    /// Soundness leans on the predicate being INFERRED: typecheck
    /// unified it against the scrutinee member it denotes, so when
    /// exactly one member overlaps a payload-carrying shape that
    /// member is the predicate's own, and dropping payload checks
    /// cannot change a verdict for any value the scrutinee's static
    /// type admits. Explicit predicates (`x as T`) are the user's
    /// claim and keep the strict deep test at the caller.
    pub fn shallow_discriminant(&self, env: &Env, scrutinee: &Type) -> Option<Type> {
        let mut scrut: LPooled<Vec<Type>> = LPooled::take();
        let mut seen: LPooled<Vec<(usize, usize)>> = LPooled::take();
        flatten_union_members(scrutinee, env, &mut scrut, &mut seen)?;
        let mut sfacts: LPooled<Vec<MemberFacts>> = LPooled::take();
        for m in scrut.iter() {
            sfacts.push(member_facts(m));
        }
        let mut preds: LPooled<Vec<Type>> = LPooled::take();
        seen.clear();
        flatten_union_members(self, env, &mut preds, &mut seen)?;
        let mut out: LPooled<Vec<Type>> = LPooled::take();
        let mut changed = false;
        for p in preds.iter() {
            let pf = member_facts(p);
            if pf.exact {
                out.push(p.clone());
                continue;
            }
            if let Some(pa) = &pf.arr {
                let n = sfacts
                    .iter()
                    .filter(|mf| mf.arr.as_ref().is_some_and(|ma| arr_overlap(pa, ma)))
                    .count();
                if n != 1 {
                    return None;
                }
            }
            if pf.map && sfacts.iter().filter(|mf| mf.map).count() != 1 {
                return None;
            }
            if pf.error && sfacts.iter().filter(|mf| mf.error).count() != 1 {
                return None;
            }
            out.push(shallowify(p));
            changed = true;
        }
        if !changed {
            return None;
        }
        Some(if out.len() == 1 {
            out.pop().unwrap()
        } else {
            Type::Set(triomphe::Arc::from(out.drain(..).collect::<Vec<_>>()))
        })
    }
}

/// A flattened union member's runtime footprint. Variants, tuples,
/// structs and arrays all inhabit `Value::Array`; `arr` is the
/// member's constraint within that class (the other classes are
/// disjoint by representation, so only same-class members can shadow
/// each other). `exact` = the member's full `is_a` already costs O(1)
/// (no payload walk), so it can neither gain from shallowing nor be
/// mis-claimed by it.
struct MemberFacts {
    arr: Option<(Option<ArcStr>, ArrCon)>,
    map: bool,
    error: bool,
    exact: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ArrCon {
    Len(usize),
    AnyLen,
}

fn member_facts(t: &Type) -> MemberFacts {
    let f = |arr, map, error, exact| MemberFacts { arr, map, error, exact };
    match t {
        Type::Primitive(bits) => f(
            bits.contains(Typ::Array).then_some((None, ArrCon::AnyLen)),
            bits.contains(Typ::Map),
            bits.contains(Typ::Error),
            true,
        ),
        Type::Variant(_, ps) if ps.is_empty() => f(None, false, false, true),
        Type::Variant(tag, ps) => {
            f(Some((Some(tag.clone()), ArrCon::Len(ps.len() + 1))), false, false, false)
        }
        Type::Tuple(ts) => f(Some((None, ArrCon::Len(ts.len()))), false, false, false),
        Type::Struct(fs) => f(Some((None, ArrCon::Len(fs.len()))), false, false, false),
        Type::Array(_) => f(Some((None, ArrCon::AnyLen)), false, false, false),
        // A list shapes as an array at runtime (nil = the empty array),
        // so beside an Array member the shallow test is ambiguous and
        // arr_overlap forces the deep walk — honest, never wrong.
        Type::List(_) => f(Some((None, ArrCon::AnyLen)), false, false, false),
        Type::Map { .. } => f(None, true, false, false),
        Type::Error(_) => f(None, false, true, false),
        Type::Abstract { .. } | Type::Fn(_) | Type::ByRef(_) | Type::Bottom => {
            f(None, false, false, true)
        }
        // `flatten_union_members` never yields these; exact = never
        // shallowed, never a footprint — inert either way.
        Type::Any
        | Type::Set(_)
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::App(..)
        | Type::Hole => f(None, false, false, true),
    }
}

fn arr_overlap(
    (ptag, pcon): &(Option<ArcStr>, ArrCon),
    (mtag, mcon): &(Option<ArcStr>, ArrCon),
) -> bool {
    match (pcon, mcon) {
        (ArrCon::AnyLen, _) | (_, ArrCon::AnyLen) => true,
        (ArrCon::Len(a), ArrCon::Len(b)) => {
            a == b
                && match (ptag, mtag) {
                    (Some(pt), Some(mt)) => pt == mt,
                    // a tuple/struct of the right length can shape
                    // like a variant (slot 0 a string) — conservative
                    _ => true,
                }
        }
    }
}

fn shallowify(t: &Type) -> Type {
    match t {
        Type::Variant(tag, ps) => Type::Variant(
            tag.clone(),
            triomphe::Arc::from(ps.iter().map(|_| Type::Any).collect::<Vec<_>>()),
        ),
        Type::Tuple(ts) => Type::Tuple(triomphe::Arc::from(
            ts.iter().map(|_| Type::Any).collect::<Vec<_>>(),
        )),
        Type::Struct(fs) => Type::Struct(triomphe::Arc::from(
            fs.iter().map(|(n, _)| (n.clone(), Type::Any)).collect::<Vec<_>>(),
        )),
        Type::Array(_) => Type::Array(triomphe::Arc::new(Type::Any)),
        Type::List(_) => Type::List(triomphe::Arc::new(Type::Any)),
        Type::Map { .. } => Type::Map {
            key: triomphe::Arc::new(Type::Any),
            value: triomphe::Arc::new(Type::Any),
        },
        Type::Error(_) => Type::Error(triomphe::Arc::new(Type::Any)),
        t => t.clone(),
    }
}

fn flatten_union_members(
    t: &Type,
    env: &Env,
    out: &mut LPooled<Vec<Type>>,
    seen: &mut LPooled<Vec<(usize, usize)>>,
) -> Option<()> {
    match t {
        Type::Set(ts) => {
            for t in ts.iter() {
                flatten_union_members(t, env, out, seen)?;
            }
            Some(())
        }
        Type::Ref(TypeRef { scope, name, .. }) => {
            let key = (
                (scope.as_ref() as *const _ as *const u8).addr(),
                (name.as_ref() as *const _ as *const u8).addr(),
            );
            if seen.contains(&key) {
                return None;
            }
            seen.push(key);
            let res = match t.lookup_ref(env) {
                Ok(t) => flatten_union_members(&t, env, out, seen),
                Err(_) => None,
            };
            seen.pop();
            res
        }
        Type::TVar(tv) => {
            let bound = tv.read().typ.read().typ.clone();
            match bound {
                Some(t) => flatten_union_members(&t, env, out, seen),
                None => None,
            }
        }
        Type::App(c, a) => match Type::app_filled(c, a) {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::Any | Type::Hole => None,
        t => {
            out.push(t.clone());
            Some(())
        }
    }
}
