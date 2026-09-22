use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG,
    env::Env,
    errf,
    expr::ModPath,
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
use smallvec::SmallVec;
use std::iter;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum IsAFlags {
    /// A `Type::Abstract` test also accepts a Rust-backed abstract
    /// value whose wrapper UUID is not the type's path-derived one
    /// (packages registering ad-hoc UUIDs). A Graphix-minted box
    /// always answers by its tag. An explicit `T as t` is strict.
    MatchAbstract,
    /// The type-blind leaves (`Any`, `⊥`, an unbound tvar) match
    /// nothing instead of everything: "does this type describe v"
    /// rather than "could v inhabit it".
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
            Type::Struct(ts) => Ok(for (_, t, _) in ts.iter() {
                t.check_cast_int(env, hist)?
            }),
            Type::Variant(_, ts, _) => Ok(for t in ts.iter() {
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
        v: &Value,
    ) -> Result<Value> {
        crate::stack::ensure_sufficient(|| self.cast_value_inner(env, hist, v))
    }

    fn cast_value_inner(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        v: &Value,
    ) -> Result<Value> {
        if self.is_a_int(env, hist, BitFlags::empty(), v) {
            return Ok(v.clone());
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
            Type::Any => Ok(v.clone()),
            Type::Error(e) => {
                let inner = match v {
                    Value::Error(v) => &**v,
                    v => v,
                };
                Ok(Value::Error(e.cast_value_int(env, hist, inner)?.into()))
            }
            Type::Array(et) => match v {
                Value::Array(elts) => {
                    let mut va = elts
                        .iter()
                        .map(|el| et.cast_value_int(env, hist, el))
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(Value::Array(ValArray::from_iter_exact(va.drain(..))))
                }
                v => Ok(Value::Array([et.cast_value_int(env, hist, v)?].into())),
            },
            // A list casts element-wise, an array converts, anything
            // else becomes a singleton.
            Type::List(et) => {
                use crate::node::collection::list;
                if list::len(v).is_some() {
                    let mut elems = list::Iter::new(v.clone())
                        .map(|el| et.cast_value_int(env, hist, &el))
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(list::from_iter(elems.drain(..)))
                } else {
                    match v {
                        Value::Array(elts) => {
                            let mut elems = elts
                                .iter()
                                .map(|el| et.cast_value_int(env, hist, el))
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
                                key.cast_value_int(env, hist, k)?,
                                value.cast_value_int(env, hist, v)?,
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
                                key.cast_value_int(env, hist, &a[0])?,
                                value.cast_value_int(env, hist, &a[1])?,
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
                        bail!("tuple size mismatch {self} with {v}")
                    }
                    let mut a = ts
                        .iter()
                        .zip(elts.iter())
                        .map(|(t, el)| t.cast_value_int(env, hist, el))
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(Value::Array(ValArray::from_iter_exact(a.drain(..))))
                }
                v => bail!("can't cast {v} to {self}"),
            },
            Type::Struct(ts) => match v {
                Value::Array(elts) => {
                    if elts.len() != ts.len() {
                        bail!("struct size mismatch {self} with {v}")
                    }
                    let mut fields: SmallVec<[(&ArcStr, &Value); 8]> = elts
                        .iter()
                        .map(struct_field)
                        .collect::<Option<_>>()
                        .ok_or_else(|| anyhow!("expected array of pairs, got {v}"))?;
                    fields.sort_by_key(|(n, _)| *n);
                    if ts
                        .iter()
                        .zip(fields.iter())
                        .any(|((fname, _, _), (n, _))| n != &fname)
                    {
                        bail!("struct fields mismatch {self}, {v}")
                    }
                    let mut elts = ts
                        .iter()
                        .zip(fields.iter())
                        .map(|((n, t, _), (_, fv))| {
                            let a = [
                                Value::String(n.clone()),
                                t.cast_value_int(env, hist, fv)?,
                            ];
                            Ok(Value::Array(ValArray::from_iter_exact(a.into_iter())))
                        })
                        .collect::<Result<LPooled<Vec<Value>>>>()?;
                    Ok(Value::Array(ValArray::from_iter_exact(elts.drain(..))))
                }
                v => bail!("can't cast {v} to {self}"),
            },
            Type::Variant(tag, ts, _) if ts.len() == 0 => match v {
                Value::String(s) if s == tag => Ok(v.clone()),
                _ => bail!("variant tag mismatch expected {tag} got {v}"),
            },
            Type::Variant(tag, ts, _) => match v {
                Value::Array(elts) => {
                    if ts.len() + 1 == elts.len() {
                        match &elts[0] {
                            Value::String(s) if s == tag => (),
                            v => bail!("variant tag mismatch expected {tag} got {v}"),
                        }
                        let mut a = iter::once(&Type::Primitive(Typ::String.into()))
                            .chain(ts.iter())
                            .zip(elts.iter())
                            .map(|(t, v)| t.cast_value_int(env, hist, v))
                            .collect::<Result<LPooled<Vec<Value>>>>()?;
                        Ok(Value::Array(ValArray::from_iter_exact(a.drain(..))))
                    } else if ts.len() == elts.len() {
                        let mut a = ts
                            .iter()
                            .zip(elts.iter())
                            .map(|(t, v)| t.cast_value_int(env, hist, v))
                            .collect::<Result<LPooled<Vec<Value>>>>()?;
                        a.insert(0, Value::String(tag.clone()));
                        Ok(Value::Array(ValArray::from_iter_exact(a.drain(..))))
                    } else {
                        bail!("variant length mismatch")
                    }
                }
                v => bail!("can't cast {v} to {self}"),
            },
            Type::Ref(TypeRef { scope, name, .. }) => {
                let t = self.lookup_ref(env)?;
                let key = (ref_key(scope, name), (v as *const Value).addr());
                if !hist.insert(key) {
                    bail!(
                        "can't cast {v} to {self}: the type recurses without consuming it"
                    )
                }
                let r = t.cast_value_int(env, hist, v);
                hist.remove(&key);
                r
            }
            Type::Set(ts) => ts
                .iter()
                .find_map(|t| t.cast_value_int(env, hist, v).ok())
                .ok_or_else(|| anyhow!("can't cast {v} to {self}")),
            Type::TVar(tv) => match &tv.read().typ.read().typ {
                Some(t) => t.cast_value_int(env, hist, v),
                None => Ok(v.clone()),
            },
        }
    }

    pub fn cast_value(&self, env: &Env, v: Value) -> Value {
        match self.cast_value_int(env, &mut LPooled::take(), &v) {
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
            // `hist` is the current path, not a visited set: a repeat
            // on the path is a name expanding without consuming value
            // structure; a repeat off the path is union backtracking.
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.is_a_int(env, hist, flags, v),
                None => !flags.contains(IsAFlags::Strict),
            },
            Type::Hole => false,
            Type::Ref(TypeRef { scope, name, .. }) => match self.lookup_ref(env) {
                Err(_) => false,
                Ok(t) => {
                    let key = (ref_key(scope, name), (v as *const Value).addr());
                    hist.insert(key) && {
                        let r = t.is_a_int(env, hist, flags, v);
                        hist.remove(&key);
                        r
                    }
                }
            },
            Type::Primitive(t) => t.contains(Typ::get(&v)),
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
            // `Any` elements: test the runtime class without walking.
            Type::Array(et)
                if matches!(&**et, Type::Any) && !flags.contains(IsAFlags::Strict) =>
            {
                matches!(v, Value::Array(_))
            }
            Type::Array(et) => match v {
                Value::Array(a) => a.iter().all(|v| et.is_a_int(env, hist, flags, v)),
                _ => false,
            },
            // Walk the spine iteratively (heads recurse).
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
                        && ts.iter().zip(elts.iter()).all(|((n, t, _), v)| match v {
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
            Type::Variant(tag, ts, _) if ts.len() == 0 => match &v {
                Value::String(s) => s == tag,
                _ => false,
            },
            Type::Variant(tag, ts, _) => match &v {
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

    /// True if v is structurally compatible with the type.
    pub fn is_a(&self, env: &Env, v: &Value) -> bool {
        self.is_a_int(env, &mut LPooled::take(), BitFlags::empty(), v)
    }

    /// [`Self::is_a`] with flags.
    pub fn is_a_with(&self, env: &Env, flags: BitFlags<IsAFlags>, v: &Value) -> bool {
        self.is_a_int(env, &mut LPooled::take(), flags, v)
    }

    /// The shallow discriminator for a select arm's INFERRED type
    /// predicate: `self` with every payload position replaced by `Any`,
    /// when the outermost shape alone tells `self`'s values apart from
    /// the other members of `scrutinee`. `None` keeps the full walk
    /// (members not enumerable, two share a shape, or no payload).
    /// Sound only for inferred predicates, which typecheck unified
    /// against their member; explicit `x as T` stays strict.
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

/// A struct value's `[name, value]` pair.
fn struct_field(v: &Value) -> Option<(&ArcStr, &Value)> {
    match v {
        Value::Array(a) if a.len() == 2 => match &a[0] {
            Value::String(n) => Some((n, &a[1])),
            _ => None,
        },
        _ => None,
    }
}

/// The identity of a type name on the current walk's path.
fn ref_key(scope: &ModPath, name: &ModPath) -> usize {
    (scope.as_ref() as *const _ as *const u8).addr()
        ^ (name.as_ref() as *const _ as *const u8).addr()
}

/// A flattened union member's runtime footprint. Variants, tuples,
/// structs and arrays all inhabit `Value::Array`; `arr` is the
/// member's constraint within that class. `exact`: the full `is_a` is
/// already O(1).
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
        Type::Variant(_, ps, _) if ps.is_empty() => f(None, false, false, true),
        Type::Variant(tag, ps, _) => {
            f(Some((Some(tag.clone()), ArrCon::Len(ps.len() + 1))), false, false, false)
        }
        Type::Tuple(ts) => f(Some((None, ArrCon::Len(ts.len()))), false, false, false),
        Type::Struct(fs) => f(Some((None, ArrCon::Len(fs.len()))), false, false, false),
        Type::Array(_) => f(Some((None, ArrCon::AnyLen)), false, false, false),
        // A list shapes as an array at runtime, so beside an Array
        // member the deep walk is forced.
        Type::List(_) => f(Some((None, ArrCon::AnyLen)), false, false, false),
        Type::Map { .. } => f(None, true, false, false),
        Type::Error(_) => f(None, false, true, false),
        Type::Abstract { .. } | Type::Fn(_) | Type::ByRef(_) | Type::Bottom => {
            f(None, false, false, true)
        }
        // `flatten_union_members` never yields these.
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
                    // A tuple/struct of the right length can shape
                    // like a variant.
                    _ => true,
                }
        }
    }
}

fn shallowify(t: &Type) -> Type {
    match t {
        Type::Variant(tag, ps, at) => Type::Variant(
            tag.clone(),
            triomphe::Arc::from(ps.iter().map(|_| Type::Any).collect::<Vec<_>>()),
            *at,
        ),
        Type::Tuple(ts) => Type::Tuple(triomphe::Arc::from(
            ts.iter().map(|_| Type::Any).collect::<Vec<_>>(),
        )),
        Type::Struct(fs) => Type::Struct(triomphe::Arc::from(
            fs.iter().map(|(n, _, at)| (n.clone(), Type::Any, *at)).collect::<Vec<_>>(),
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
