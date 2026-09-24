use crate::{
    AbstractTypeRegistry, CAST_ERR_TAG,
    env::Env,
    errf,
    node::collection::list,
    stack::ensure_sufficient,
    typ::{Type, tval::NakedPrefix},
};
use ahash::AHashSet;
use anyhow::{Result, bail};
use arcstr::ArcStr;
use enumflags2::{BitFlags, bitflags};
use immutable_chunkmap::map::Map;
use netidx_value::{Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::fmt;
use triomphe::Arc;

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

/// A failed cast. Formatting is deferred to the report: a union tries
/// its members and discards every failure but the last.
#[derive(Debug)]
struct CastFail {
    why: &'static str,
    to: Type,
    v: Value,
}

impl fmt::Display for CastFail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "can't cast {} to {}: {}", NakedPrefix(&self.v), self.to, self.why)
    }
}

/// A cast level's result: `None` when the value already has the type.
type Cast = std::result::Result<Option<Value>, CastFail>;

/// The static source type at a cast position, with names, cells and
/// applications looked through; `None` when unknown or a union.
fn src_head(src: Option<&Type>, env: &Env) -> Option<Type> {
    let mut cur = src?.clone();
    loop {
        cur = match &cur {
            Type::TVar(_) | Type::App(..) => cur.deref_cloned()?,
            Type::Ref(_) => cur.lookup_ref(env).ok()?,
            Type::Set(_) | Type::Any | Type::Bottom | Type::Hole => return None,
            _ => return Some(cur),
        };
    }
}

/// `f` over the values of `elts`, rebuilt only if some value changed.
fn cast_elts<T>(
    elts: &[T],
    get: impl Fn(&T) -> &Value,
    mut f: impl FnMut(usize, &Value) -> Cast,
) -> std::result::Result<Option<LPooled<Vec<Value>>>, CastFail> {
    let mut out: Option<LPooled<Vec<Value>>> = None;
    for (i, e) in elts.iter().enumerate() {
        let v = get(e);
        match f(i, v)? {
            Some(c) => out
                .get_or_insert_with(|| elts[..i].iter().map(|e| get(e).clone()).collect())
                .push(c),
            None => {
                if let Some(o) = out.as_mut() {
                    o.push(v.clone())
                }
            }
        }
    }
    Ok(out)
}

impl Type {
    fn check_cast_int(&self, env: &Env, seen: &mut AHashSet<usize>) -> Result<()> {
        ensure_sufficient(|| self.check_cast_inner(env, seen))
    }

    fn check_cast_inner(&self, env: &Env, seen: &mut AHashSet<usize>) -> Result<()> {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.check_cast_int(env, seen),
                None => bail!("can't cast a value to a type constructor"),
            },
            Type::Hole => bail!("can't cast a value to a type constructor"),
            Type::Primitive(_) | Type::Any => Ok(()),
            Type::Fn(_) => bail!("can't cast a value to a function"),
            Type::Bottom => bail!("can't cast a value to bottom"),
            Type::Abstract { .. } => {
                bail!("can't cast a value to an abstract type; use its constructor")
            }
            Type::TVar(_) => match self.deref_cloned() {
                Some(t) => t.check_cast_int(env, seen),
                None => bail!("can't cast a value to a free type variable"),
            },
            Type::ByRef(_) => bail!("can't cast a reference"),
            Type::Ref(tr) => {
                let t = self.lookup_ref(env)?;
                match tr.def_key() {
                    Some(k) if !seen.insert(k) => Ok(()),
                    _ => t.check_cast_int(env, seen),
                }
            }
            t => {
                let mut r = Ok(());
                t.for_each_child(&mut |c| {
                    if r.is_ok() {
                        r = c.check_cast_int(env, seen)
                    }
                });
                r
            }
        }
    }

    pub fn check_cast(&self, env: &Env) -> Result<()> {
        self.check_cast_int(env, &mut LPooled::take())
    }

    fn cast_int(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        ensure_sufficient(|| self.cast_inner(env, hist, src, v))
    }

    fn cast_fail(&self, why: &'static str, v: &Value) -> CastFail {
        CastFail { why, to: self.clone(), v: v.clone() }
    }

    fn cast_inner(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        src: Option<&Type>,
        v: &Value,
    ) -> Cast {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.cast_int(env, hist, src, v),
                None => Ok(None),
            },
            Type::Hole => Err(self.cast_fail("a type constructor", v)),
            Type::Bottom | Type::Any => Ok(None),
            Type::Fn(_) => match v {
                Value::Abstract(a) if AbstractTypeRegistry::is_a(a, "lambda") => Ok(None),
                _ => Err(self.cast_fail("not a function", v)),
            },
            Type::Abstract { .. } => {
                if self.is_a(env, v) {
                    Ok(None)
                } else {
                    Err(self.cast_fail("not this abstract type", v))
                }
            }
            Type::ByRef(_) => match v {
                Value::U64(_) | Value::V64(_) => Ok(None),
                _ => Err(self.cast_fail("not a reference", v)),
            },
            Type::Primitive(s) => {
                if s.contains(Typ::get(v)) {
                    return Ok(None);
                }
                match s.iter().find_map(|t| v.clone().cast(t)) {
                    Some(v) => Ok(Some(v)),
                    None => Err(self.cast_fail("no primitive conversion", v)),
                }
            }
            Type::TVar(tv) => match tv.binding() {
                Some(t) => t.cast_int(env, hist, src, v),
                None => Ok(None),
            },
            Type::Error(e) => {
                let src = src_head(src, env);
                let src = match &src {
                    Some(Type::Error(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    Value::Error(inner) => Ok(e
                        .cast_int(env, hist, src, inner)?
                        .map(|c| Value::Error(c.into()))),
                    v => {
                        let c = e.cast_int(env, hist, src, v)?;
                        Ok(Some(Value::Error(c.unwrap_or_else(|| v.clone()).into())))
                    }
                }
            }
            // An array and a list share `Value::Array`: the source's
            // static type says which one the value is, the shape only
            // when the source is unknown.
            Type::Array(et) => {
                let src = src_head(src, env);
                let from_list = matches!(&src, Some(Type::List(_)));
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    _ if from_list => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in list::Iter::new(v.clone()) {
                            let c = et.cast_int(env, hist, src, &el)?;
                            out.push(c.unwrap_or(el));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    Value::Array(elts) => {
                        Ok(cast_elts(&elts[..], |v| v, |_, el| et.cast_int(env, hist, src, el))?
                            .map(|mut a| {
                                Value::Array(ValArray::from_iter_exact(a.drain(..)))
                            }))
                    }
                    v => {
                        let c = et.cast_int(env, hist, src, v)?;
                        Ok(Some(Value::Array([c.unwrap_or_else(|| v.clone())].into())))
                    }
                }
            }
            Type::List(et) => {
                let src = src_head(src, env);
                let spine = match &src {
                    Some(Type::List(_)) => true,
                    Some(Type::Array(_)) => false,
                    _ => list::len(v).is_some(),
                };
                let src = match &src {
                    Some(Type::Array(s) | Type::List(s)) => Some(&**s),
                    _ => None,
                };
                match v {
                    _ if spine => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        let mut changed = false;
                        for el in list::Iter::new(v.clone()) {
                            match et.cast_int(env, hist, src, &el)? {
                                Some(c) => {
                                    changed = true;
                                    out.push(c)
                                }
                                None => out.push(el),
                            }
                        }
                        Ok(changed.then(|| list::from_iter(out.drain(..))))
                    }
                    Value::Array(elts) => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        for el in elts.iter() {
                            let c = et.cast_int(env, hist, src, el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(list::from_iter(out.drain(..))))
                    }
                    v => {
                        let c = et.cast_int(env, hist, src, v)?;
                        Ok(Some(list::from_iter([c.unwrap_or_else(|| v.clone())])))
                    }
                }
            }
            Type::Map { key, value } => {
                let src = src_head(src, env);
                let (ks, vs) = match &src {
                    Some(Type::Map { key, value }) => (Some(&**key), Some(&**value)),
                    _ => (None, None),
                };
                let mut entry = |k: &Value, v: &Value| -> std::result::Result<_, CastFail> {
                    let ck = key.cast_int(env, hist, ks, k)?;
                    let cv = value.cast_int(env, hist, vs, v)?;
                    Ok((ck.is_some() || cv.is_some())
                        .then(|| (ck.unwrap_or_else(|| k.clone()), cv.unwrap_or_else(|| v.clone()))))
                };
                match v {
                    Value::Map(m) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        let mut changed = false;
                        for (k, v) in m.into_iter() {
                            match entry(k, v)? {
                                Some(kv) => {
                                    changed = true;
                                    out.push(kv)
                                }
                                None => out.push((k.clone(), v.clone())),
                            }
                        }
                        Ok(changed.then(|| Value::Map(Map::from_iter(out.drain(..)))))
                    }
                    Value::Array(a) => {
                        let mut out: LPooled<Vec<(Value, Value)>> = LPooled::take();
                        for p in a.iter() {
                            match p {
                                Value::Array(p) if p.len() == 2 => {
                                    let kv = entry(&p[0], &p[1])?;
                                    out.push(kv.unwrap_or_else(|| (p[0].clone(), p[1].clone())))
                                }
                                _ => return Err(self.cast_fail("expected an array of pairs", v)),
                            }
                        }
                        Ok(Some(Value::Map(Map::from_iter(out.drain(..)))))
                    }
                    _ => Err(self.cast_fail("not a map", v)),
                }
            }
            Type::Tuple(ts) => {
                let src = src_head(src, env);
                let ss = match &src {
                    Some(Type::Tuple(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                match v {
                    Value::Array(elts) if elts.len() == ts.len() => {
                        Ok(cast_elts(&elts[..], |v| v, |i, el| {
                            ts[i].cast_int(env, hist, ss.map(|ss| &ss[i]), el)
                        })?
                        .map(|mut a| Value::Array(ValArray::from_iter_exact(a.drain(..)))))
                    }
                    Value::Array(_) => Err(self.cast_fail("tuple size mismatch", v)),
                    _ => Err(self.cast_fail("not a tuple", v)),
                }
            }
            Type::Struct(ts) => {
                let Value::Array(elts) = v else {
                    return Err(self.cast_fail("not a struct", v));
                };
                if elts.len() != ts.len() {
                    return Err(self.cast_fail("struct size mismatch", v));
                }
                let mut fields: SmallVec<[(&ArcStr, &Value); 8]> =
                    match elts.iter().map(struct_field).collect::<Option<_>>() {
                        Some(f) => f,
                        None => return Err(self.cast_fail("expected an array of pairs", v)),
                    };
                let sorted = fields.is_sorted_by_key(|(n, _)| *n);
                if !sorted {
                    fields.sort_by_key(|(n, _)| *n);
                }
                if ts.iter().zip(fields.iter()).any(|((fname, _, _), (n, _))| n != &fname) {
                    return Err(self.cast_fail("struct fields mismatch", v));
                }
                let src = src_head(src, env);
                let ss = match &src {
                    Some(Type::Struct(ss)) if ss.len() == ts.len() => Some(ss),
                    _ => None,
                };
                let cast = cast_elts(&fields[..], |(_, fv)| *fv, |i, fv| {
                    ts[i].1.cast_int(env, hist, ss.map(|ss| &ss[i].1), fv)
                })?;
                if sorted && cast.is_none() {
                    return Ok(None);
                }
                let mut out: LPooled<Vec<Value>> = LPooled::take();
                for (i, (n, fv)) in fields.iter().enumerate() {
                    let fv = match &cast {
                        Some(c) => c[i].clone(),
                        None => (*fv).clone(),
                    };
                    let pair = [Value::String((*n).clone()), fv];
                    out.push(Value::Array(ValArray::from_iter_exact(pair.into_iter())));
                }
                Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
            }
            Type::Variant(tag, ts, _) if ts.is_empty() => match v {
                Value::String(s) if s == tag => Ok(None),
                _ => Err(self.cast_fail("variant tag mismatch", v)),
            },
            Type::Variant(tag, ts, _) => {
                let src = src_head(src, env);
                let ss = match &src {
                    Some(Type::Variant(stag, ss, _)) if stag == tag && ss.len() == ts.len() => {
                        Some(ss)
                    }
                    _ => None,
                };
                let payload = |i: usize| ss.map(|ss| &ss[i]);
                match v {
                    Value::Array(elts) if elts.len() == ts.len() + 1 => {
                        if !matches!(&elts[0], Value::String(s) if s == tag) {
                            return Err(self.cast_fail("variant tag mismatch", v));
                        }
                        Ok(cast_elts(&elts[1..], |v| v, |i, el| {
                            ts[i].cast_int(env, hist, payload(i), el)
                        })?
                        .map(|mut a| {
                            a.insert(0, Value::String(tag.clone()));
                            Value::Array(ValArray::from_iter_exact(a.drain(..)))
                        }))
                    }
                    Value::Array(elts) if elts.len() == ts.len() => {
                        let mut out: LPooled<Vec<Value>> = LPooled::take();
                        out.push(Value::String(tag.clone()));
                        for (i, el) in elts.iter().enumerate() {
                            let c = ts[i].cast_int(env, hist, payload(i), el)?;
                            out.push(c.unwrap_or_else(|| el.clone()));
                        }
                        Ok(Some(Value::Array(ValArray::from_iter_exact(out.drain(..)))))
                    }
                    Value::Array(_) => Err(self.cast_fail("variant length mismatch", v)),
                    _ => Err(self.cast_fail("not a variant", v)),
                }
            }
            // `hist` is the current path, not a visited set: a name met
            // again on the same value expands without consuming it.
            Type::Ref(tr) => {
                let t = match self.lookup_ref(env) {
                    Ok(t) => t,
                    Err(_) => return Err(self.cast_fail("undefined type", v)),
                };
                let key = (tr.def_key().unwrap_or(0), (v as *const Value).addr());
                if !hist.insert(key) {
                    return Err(self.cast_fail("the type recurses without consuming it", v));
                }
                let r = t.cast_int(env, hist, src, v);
                hist.remove(&key);
                r
            }
            // A member the value already inhabits wins; else the first
            // member it converts to.
            Type::Set(ts) => {
                let mut converted = None;
                for t in ts.iter() {
                    match t.cast_int(env, hist, src, v) {
                        Ok(None) => return Ok(None),
                        Ok(Some(c)) if converted.is_none() => converted = Some(c),
                        Ok(Some(_)) | Err(_) => (),
                    }
                }
                converted.map(Some).ok_or_else(|| self.cast_fail("no member admits it", v))
            }
        }
    }

    /// Cast `v` of unknown static type to this type; a failure is the
    /// `InvalidCast` error value. An array and a list are told apart by
    /// shape.
    pub fn cast_value(&self, env: &Env, v: Value) -> Value {
        self.cast_value_int(env, None, v)
    }

    /// [`Self::cast_value`] of a `v` statically of type `src`, which
    /// decides whether a value is an array or a list.
    pub fn cast_from(&self, env: &Env, src: &Type, v: Value) -> Value {
        self.cast_value_int(env, Some(src), v)
    }

    fn cast_value_int(&self, env: &Env, src: Option<&Type>, v: Value) -> Value {
        match self.cast_int(env, &mut LPooled::take(), src, &v) {
            Ok(None) => v,
            Ok(Some(v)) => v,
            Err(e) => errf!(CAST_ERR_TAG, "{e}"),
        }
    }

    fn is_a_int(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        ensure_sufficient(|| self.is_a_int_inner(env, hist, flags, v))
    }

    fn is_a_int_inner(
        &self,
        env: &Env,
        hist: &mut AHashSet<(usize, usize)>,
        flags: BitFlags<IsAFlags>,
        v: &Value,
    ) -> bool {
        match self {
            Type::App(c, a) => match Type::app_filled(c, a) {
                Some(t) => t.is_a_int(env, hist, flags, v),
                None => !flags.contains(IsAFlags::Strict),
            },
            Type::Hole => false,
            // `hist` is the current path, not a visited set: a repeat
            // on the path is a name expanding without consuming value
            // structure; a repeat off the path is union backtracking.
            Type::Ref(tr) => match self.lookup_ref(env) {
                Err(_) => false,
                Ok(t) => {
                    let key = (tr.def_key().unwrap_or(0), (v as *const Value).addr());
                    hist.insert(key) && {
                        let r = t.is_a_int(env, hist, flags, v);
                        hist.remove(&key);
                        r
                    }
                }
            },
            Type::Primitive(t) => t.contains(Typ::get(&v)),
            Type::Abstract { id, params } => match v {
                Value::Abstract(a) => {
                    match a.downcast_ref::<crate::abstract_value::GxAbstract>() {
                        Some(g) => {
                            g.id == *id
                                && g.params.len() == params.len()
                                && params.iter().zip(g.params.iter()).all(|(p, gp)| {
                                    p.contains_with_flags(BitFlags::empty(), env, gp)
                                        .unwrap_or(false)
                                })
                        }
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
            Type::TVar(tv) => match tv.binding() {
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
        let mut seen: LPooled<Vec<usize>> = LPooled::take();
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
            Type::Set(Arc::from_iter(out.drain(..)))
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

impl MemberFacts {
    const EXACT: Self = MemberFacts { arr: None, map: false, error: false, exact: true };

    fn arr(tag: Option<ArcStr>, con: ArrCon) -> Self {
        MemberFacts { arr: Some((tag, con)), map: false, error: false, exact: false }
    }
}

fn member_facts(t: &Type) -> MemberFacts {
    match t {
        Type::Primitive(bits) => MemberFacts {
            arr: bits.contains(Typ::Array).then_some((None, ArrCon::AnyLen)),
            map: bits.contains(Typ::Map),
            error: bits.contains(Typ::Error),
            exact: true,
        },
        Type::Variant(_, ps, _) if ps.is_empty() => MemberFacts::EXACT,
        Type::Variant(tag, ps, _) => {
            MemberFacts::arr(Some(tag.clone()), ArrCon::Len(ps.len() + 1))
        }
        Type::Tuple(ts) => MemberFacts::arr(None, ArrCon::Len(ts.len())),
        Type::Struct(fs) => MemberFacts::arr(None, ArrCon::Len(fs.len())),
        // A list shapes as an array at runtime, so beside an Array
        // member the deep walk is forced.
        Type::Array(_) | Type::List(_) => MemberFacts::arr(None, ArrCon::AnyLen),
        Type::Map { .. } => MemberFacts { map: true, exact: false, ..MemberFacts::EXACT },
        Type::Error(_) => MemberFacts { error: true, exact: false, ..MemberFacts::EXACT },
        Type::Abstract { .. } | Type::Fn(_) | Type::ByRef(_) | Type::Bottom => {
            MemberFacts::EXACT
        }
        // `flatten_union_members` never yields these.
        Type::Any
        | Type::Set(_)
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::App(..)
        | Type::Hole => MemberFacts::EXACT,
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
        Type::Variant(tag, ps, at) => {
            Type::Variant(tag.clone(), Arc::from_iter(ps.iter().map(|_| Type::Any)), *at)
        }
        Type::Tuple(ts) => Type::Tuple(Arc::from_iter(ts.iter().map(|_| Type::Any))),
        Type::Struct(fs) => Type::Struct(Arc::from_iter(
            fs.iter().map(|(n, _, at)| (n.clone(), Type::Any, *at)),
        )),
        Type::Array(_) => Type::Array(Arc::new(Type::Any)),
        Type::List(_) => Type::List(Arc::new(Type::Any)),
        Type::Map { .. } => {
            Type::Map { key: Arc::new(Type::Any), value: Arc::new(Type::Any) }
        }
        Type::Error(_) => Type::Error(Arc::new(Type::Any)),
        t => t.clone(),
    }
}

/// `seen` is the path of definitions being expanded.
fn flatten_union_members(
    t: &Type,
    env: &Env,
    out: &mut LPooled<Vec<Type>>,
    seen: &mut LPooled<Vec<usize>>,
) -> Option<()> {
    ensure_sufficient(|| match t {
        Type::Set(ts) => {
            for t in ts.iter() {
                flatten_union_members(t, env, out, seen)?;
            }
            Some(())
        }
        Type::Ref(tr) => {
            let t = t.lookup_ref(env).ok()?;
            let key = tr.def_key()?;
            if seen.contains(&key) {
                return None;
            }
            seen.push(key);
            let res = flatten_union_members(&t, env, out, seen);
            seen.pop();
            res
        }
        Type::TVar(tv) => match tv.binding() {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::App(c, a) => match Type::app_filled(c, a) {
            Some(t) => flatten_union_members(&t, env, out, seen),
            None => None,
        },
        Type::Any | Type::Hole => None,
        t => {
            out.push(t.clone());
            Some(())
        }
    })
}
