use crate::{env::Env, errf, typ::Type, AbstractTypeRegistry, CAST_ERR_TAG};
use anyhow::{anyhow, bail, Result};
use arcstr::ArcStr;
use fxhash::FxHashSet;
use immutable_chunkmap::map::Map;
use netidx::publisher::{Typ, Value};
use netidx_value::ValArray;
use poolshark::local::LPooled;
use std::iter;
use triomphe::Arc;

impl Type {
    fn check_cast_int(&self, env: &Env, hist: &mut FxHashSet<usize>) -> Result<()> {
        match self {
            Type::Primitive(_) | Type::Any => Ok(()),
            Type::Fn(_) => bail!("can't cast a value to a function"),
            Type::Bottom => bail!("can't cast a value to bottom"),
            Type::Set(s) | Type::Abstract { id: _, params: s } => Ok(for t in s.iter() {
                t.check_cast_int(env, hist)?
            }),
            Type::TVar(tv) => match &*tv.read().typ.read() {
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
            Type::Ref { .. } => {
                let t = self.lookup_ref(env)?;
                let t_addr = (t as *const Type).addr();
                if hist.contains(&t_addr) {
                    Ok(())
                } else {
                    hist.insert(t_addr);
                    t.check_cast_int(env, hist)
                }
            }
        }
    }

    pub fn check_cast(&self, env: &Env) -> Result<()> {
        self.check_cast_int(env, &mut LPooled::take())
    }

    fn cast_value_int(
        &self,
        env: &Env,
        hist: &mut FxHashSet<(usize, usize)>,
        v: Value,
    ) -> Result<Value> {
        if self.is_a_int(env, hist, &v) {
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
                Ok(Value::Error(Arc::new(e.cast_value_int(env, hist, v)?)))
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
            Type::Ref { .. } => self.lookup_ref(env)?.cast_value_int(env, hist, v),
            Type::Set(ts) => ts
                .iter()
                .find_map(|t| t.cast_value_int(env, hist, v.clone()).ok())
                .ok_or_else(|| anyhow!("can't cast {v} to {self}")),
            Type::TVar(tv) => match &*tv.read().typ.read() {
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
        hist: &mut FxHashSet<(usize, usize)>,
        v: &Value,
    ) -> bool {
        match self {
            Type::Ref { .. } => match self.lookup_ref(env) {
                Err(_) => false,
                Ok(t) => {
                    let t_addr = (t as *const Type).addr();
                    let v_addr = (v as *const Value).addr();
                    !hist.contains(&(t_addr, v_addr)) && {
                        hist.insert((t_addr, v_addr));
                        t.is_a_int(env, hist, v)
                    }
                }
            },
            Type::Primitive(t) => t.contains(Typ::get(&v)),
            Type::Abstract { .. } => false,
            Type::Any => true,
            Type::Array(et) => match v {
                Value::Array(a) => a.iter().all(|v| et.is_a_int(env, hist, v)),
                _ => false,
            },
            Type::Map { key, value } => match v {
                Value::Map(m) => m.into_iter().all(|(k, v)| {
                    key.is_a_int(env, hist, k) && value.is_a_int(env, hist, v)
                }),
                _ => false,
            },
            Type::Error(e) => match v {
                Value::Error(v) => e.is_a_int(env, hist, v),
                _ => false,
            },
            Type::ByRef(_) => matches!(v, Value::U64(_) | Value::V64(_)),
            Type::Tuple(ts) => match v {
                Value::Array(elts) => {
                    elts.len() == ts.len()
                        && ts
                            .iter()
                            .zip(elts.iter())
                            .all(|(t, v)| t.is_a_int(env, hist, v))
                }
                _ => false,
            },
            Type::Struct(ts) => match v {
                Value::Array(elts) => {
                    elts.len() == ts.len()
                        && ts.iter().zip(elts.iter()).all(|((n, t), v)| match v {
                            Value::Array(a) if a.len() == 2 => match &a[..] {
                                [Value::String(key), v] => {
                                    n == key && t.is_a_int(env, hist, v)
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
                            .all(|(t, v)| t.is_a_int(env, hist, v))
                }
                _ => false,
            },
            Type::TVar(tv) => match &*tv.read().typ.read() {
                None => true,
                Some(t) => t.is_a_int(env, hist, v),
            },
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => true,
                _ => false,
            },
            Type::Bottom => true,
            Type::Set(ts) => ts.iter().any(|t| t.is_a_int(env, hist, v)),
        }
    }

    /// return true if v is structurally compatible with the type
    pub fn is_a(&self, env: &Env, v: &Value) -> bool {
        self.is_a_int(env, &mut LPooled::take(), v)
    }
}
