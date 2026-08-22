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
    /// packages that still register ad-hoc UUIDs. A Graphix-minted box
    /// always answers by its tag, and a non-abstract value never
    /// matches (`design/nominal_abstract_types.md`). Consumers: the
    /// `TVal` printer and INFERRED select predicates; an explicit
    /// `T as t` is strict.
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
            Type::Array(et) => match v {
                Value::Array(a) => a.iter().all(|v| et.is_a_int(env, hist, flags, v)),
                _ => false,
            },
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
}
