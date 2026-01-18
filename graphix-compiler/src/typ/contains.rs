use crate::{
    env::Env,
    format_with_flags,
    typ::{tvar::would_cycle_inner, AndAc, Type},
    PrintFlag,
};
use anyhow::{bail, Result};
use enumflags2::bitflags;
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use netidx::publisher::Typ;
use poolshark::local::LPooled;
use std::fmt::Debug;
use triomphe::Arc;

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u8)]
pub enum ContainsFlags {
    AliasTVars,
    InitTVars,
}

impl Type {
    pub fn check_contains(&self, env: &Env, t: &Self) -> Result<()> {
        if self.contains(env, t)? {
            Ok(())
        } else {
            format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                bail!("type mismatch {self} does not contain {t}")
            })
        }
    }

    pub(super) fn contains_int(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        hist: &mut FxHashMap<(usize, usize), bool>,
        t: &Self,
    ) -> Result<bool> {
        if (self as *const Type) == (t as *const Type) {
            return Ok(true);
        }
        match (self, t) {
            (
                Self::Ref { scope: s0, name: n0, params: p0 },
                Self::Ref { scope: s1, name: n1, params: p1 },
            ) if s0 == s1 && n0 == n1 => Ok(p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (t0 @ Self::Ref { .. }, t1) | (t0, t1 @ Self::Ref { .. }) => {
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                let t0_addr = (t0 as *const Type).addr();
                let t1_addr = (t1 as *const Type).addr();
                match hist.get(&(t0_addr, t1_addr)) {
                    Some(r) => Ok(*r),
                    None => {
                        hist.insert((t0_addr, t1_addr), true);
                        match t0.contains_int(flags, env, hist, t1) {
                            Ok(r) => {
                                hist.insert((t0_addr, t1_addr), r);
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
            (Self::TVar(t0), Self::Bottom) => {
                if let Some(_) = &*t0.read().typ.read() {
                    return Ok(true);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    *t0.read().typ.write() = Some(Self::Bottom);
                }
                Ok(true)
            }
            (Self::Bottom, Self::TVar(t0)) => {
                if let Some(Type::Bottom) = &*t0.read().typ.read() {
                    return Ok(true);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    *t0.read().typ.write() = Some(Self::Bottom);
                    return Ok(true);
                }
                Ok(false)
            }
            (Self::Bottom, Self::Bottom) => Ok(true),
            (Self::Bottom, _) => Ok(false),
            (_, Self::Bottom) => Ok(true),
            (Self::TVar(t0), Self::Any) => {
                if let Some(t0) = &*t0.read().typ.read() {
                    return t0.contains_int(flags, env, hist, t);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    *t0.read().typ.write() = Some(Self::Any);
                }
                Ok(true)
            }
            (Self::Any, _) => Ok(true),
            (
                Self::Abstract { id: id0, params: p0 },
                Self::Abstract { id: id1, params: p1 },
            ) => Ok(id0 == id1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::Primitive(p0), Self::Primitive(p1)) => Ok(p0.contains(*p1)),
            (
                Self::Primitive(p),
                Self::Array(_) | Self::Tuple(_) | Self::Struct(_) | Self::Variant(_, _),
            ) => Ok(p.contains(Typ::Array)),
            (Self::Array(t0), Self::Array(t1)) => t0.contains_int(flags, env, hist, t1),
            (Self::Array(t0), Self::Primitive(p)) if *p == BitFlags::from(Typ::Array) => {
                t0.contains_int(flags, env, hist, &Type::Any)
            }
            (Self::Map { key: k0, value: v0 }, Self::Map { key: k1, value: v1 }) => {
                Ok(k0.contains_int(flags, env, hist, k1)?
                    && v0.contains_int(flags, env, hist, v1)?)
            }
            (Self::Primitive(p), Self::Map { .. }) => Ok(p.contains(Typ::Map)),
            (Self::Map { key, value }, Self::Primitive(p))
                if *p == BitFlags::from(Typ::Map) =>
            {
                Ok(key.contains_int(flags, env, hist, &Type::Any)?
                    && value.contains_int(flags, env, hist, &Type::Any)?)
            }
            (Self::Primitive(p0), Self::Error(_)) => Ok(p0.contains(Typ::Error)),
            (Self::Error(e), Self::Primitive(p)) if *p == BitFlags::from(Typ::Error) => {
                e.contains_int(flags, env, hist, &Type::Any)
            }
            (Self::Error(e0), Self::Error(e1)) => e0.contains_int(flags, env, hist, e1),
            (Self::Tuple(t0), Self::Tuple(t1))
                if t0.as_ptr().addr() == t1.as_ptr().addr() =>
            {
                Ok(true)
            }
            (Self::Tuple(t0), Self::Tuple(t1)) => Ok(t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::Struct(t0), Self::Struct(t1))
                if t0.as_ptr().addr() == t1.as_ptr().addr() =>
            {
                Ok(true)
            }
            (Self::Struct(t0), Self::Struct(t1)) => {
                Ok(t0.len() == t1.len() && {
                    // struct types are always sorted by field name
                    t0.iter()
                        .zip(t1.iter())
                        .map(|((n0, t0), (n1, t1))| {
                            Ok(n0 == n1 && t0.contains_int(flags, env, hist, t1)?)
                        })
                        .collect::<Result<AndAc>>()?
                        .0
                })
            }
            (Self::Variant(tg0, t0), Self::Variant(tg1, t1))
                if tg0.as_ptr() == tg1.as_ptr()
                    && t0.as_ptr().addr() == t1.as_ptr().addr() =>
            {
                Ok(true)
            }
            (Self::Variant(tg0, t0), Self::Variant(tg1, t1)) => Ok(tg0 == tg1
                && t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::ByRef(t0), Self::ByRef(t1)) => t0.contains_int(flags, env, hist, t1),
            (Self::TVar(t0), Self::TVar(t1))
                if t0.addr() == t1.addr() || t0.read().id == t1.read().id =>
            {
                Ok(true)
            }
            (tt0 @ Self::TVar(t0), tt1 @ Self::TVar(t1)) => {
                #[derive(Debug)]
                enum Act {
                    RightCopy,
                    RightAlias,
                    LeftAlias,
                    LeftCopy,
                }
                if t0.would_cycle(tt1) || t1.would_cycle(tt0) {
                    return Ok(true);
                }
                let act = {
                    let t0 = t0.read();
                    let t1 = t1.read();
                    let addr0 = Arc::as_ptr(&t0.typ).addr();
                    let addr1 = Arc::as_ptr(&t1.typ).addr();
                    if addr0 == addr1 {
                        return Ok(true);
                    }
                    if would_cycle_inner(addr0, tt1) || would_cycle_inner(addr1, tt0) {
                        return Ok(true);
                    }
                    let t0i = t0.typ.read();
                    let t1i = t1.typ.read();
                    match (&*t0i, &*t1i) {
                        (Some(t0), Some(t1)) => {
                            return t0.contains_int(flags, env, hist, &*t1)
                        }
                        (None, None) => {
                            if t0.frozen && t1.frozen {
                                return Ok(true);
                            }
                            if t0.frozen {
                                Act::RightAlias
                            } else {
                                Act::LeftAlias
                            }
                        }
                        (Some(_), None) => Act::RightCopy,
                        (None, Some(_)) => Act::LeftCopy,
                    }
                };
                match act {
                    Act::RightCopy if flags.contains(ContainsFlags::InitTVars) => {
                        t1.copy(t0)
                    }
                    Act::RightAlias if flags.contains(ContainsFlags::AliasTVars) => {
                        t1.alias(t0)
                    }
                    Act::LeftAlias if flags.contains(ContainsFlags::AliasTVars) => {
                        t0.alias(t1)
                    }
                    Act::LeftCopy if flags.contains(ContainsFlags::InitTVars) => {
                        t0.copy(t1)
                    }
                    Act::RightCopy | Act::RightAlias | Act::LeftAlias | Act::LeftCopy => {
                        ()
                    }
                }
                Ok(true)
            }
            (Self::TVar(t0), t1) if !t0.would_cycle(t1) => {
                if let Some(t0) = &*t0.read().typ.read() {
                    return t0.contains_int(flags, env, hist, t1);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    *t0.read().typ.write() = Some(t1.clone());
                }
                Ok(true)
            }
            (t0, Self::TVar(t1)) if !t1.would_cycle(t0) => {
                if let Some(t1) = &*t1.read().typ.read() {
                    return t0.contains_int(flags, env, hist, t1);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    *t1.read().typ.write() = Some(t0.clone());
                }
                Ok(true)
            }
            (Self::Set(s0), Self::Set(s1))
                if s0.as_ptr().addr() == s1.as_ptr().addr() =>
            {
                Ok(true)
            }
            (t0, Self::Set(s)) => Ok(s
                .iter()
                .map(|t1| t0.contains_int(flags, env, hist, t1))
                .collect::<Result<AndAc>>()?
                .0),
            (Self::Set(s), t) => Ok(s
                .iter()
                .fold(Ok::<_, anyhow::Error>(false), |acc, t0| {
                    Ok(acc? || t0.contains_int(flags, env, hist, t)?)
                })?
                || t.iter_prims().fold(Ok::<_, anyhow::Error>(true), |acc, t1| {
                    Ok(acc?
                        && s.iter().fold(Ok::<_, anyhow::Error>(false), |acc, t0| {
                            Ok(acc? || t0.contains_int(flags, env, hist, &t1)?)
                        })?)
                })?),
            (Self::Fn(f0), Self::Fn(f1)) => {
                Ok(f0.as_ptr() == f1.as_ptr() || f0.contains_int(flags, env, hist, f1)?)
            }
            (_, Self::Any)
            | (Self::Abstract { .. }, _)
            | (_, Self::Abstract { .. })
            | (_, Self::TVar(_))
            | (Self::TVar(_), _)
            | (Self::Fn(_), _)
            | (Self::ByRef(_), _)
            | (_, Self::ByRef(_))
            | (_, Self::Fn(_))
            | (Self::Tuple(_), Self::Array(_))
            | (Self::Tuple(_), Self::Primitive(_))
            | (Self::Tuple(_), Self::Struct(_))
            | (Self::Tuple(_), Self::Variant(_, _))
            | (Self::Tuple(_), Self::Error(_))
            | (Self::Tuple(_), Self::Map { .. })
            | (Self::Array(_), Self::Primitive(_))
            | (Self::Array(_), Self::Tuple(_))
            | (Self::Array(_), Self::Struct(_))
            | (Self::Array(_), Self::Variant(_, _))
            | (Self::Array(_), Self::Error(_))
            | (Self::Array(_), Self::Map { .. })
            | (Self::Struct(_), Self::Array(_))
            | (Self::Struct(_), Self::Primitive(_))
            | (Self::Struct(_), Self::Tuple(_))
            | (Self::Struct(_), Self::Variant(_, _))
            | (Self::Struct(_), Self::Error(_))
            | (Self::Struct(_), Self::Map { .. })
            | (Self::Variant(_, _), Self::Array(_))
            | (Self::Variant(_, _), Self::Struct(_))
            | (Self::Variant(_, _), Self::Primitive(_))
            | (Self::Variant(_, _), Self::Tuple(_))
            | (Self::Variant(_, _), Self::Error(_))
            | (Self::Variant(_, _), Self::Map { .. })
            | (Self::Error(_), Self::Array(_))
            | (Self::Error(_), Self::Primitive(_))
            | (Self::Error(_), Self::Struct(_))
            | (Self::Error(_), Self::Variant(_, _))
            | (Self::Error(_), Self::Tuple(_))
            | (Self::Error(_), Self::Map { .. })
            | (Self::Map { .. }, Self::Array(_))
            | (Self::Map { .. }, Self::Primitive(_))
            | (Self::Map { .. }, Self::Struct(_))
            | (Self::Map { .. }, Self::Variant(_, _))
            | (Self::Map { .. }, Self::Tuple(_))
            | (Self::Map { .. }, Self::Error(_)) => Ok(false),
        }
    }

    pub fn contains(&self, env: &Env, t: &Self) -> Result<bool> {
        self.contains_int(
            ContainsFlags::AliasTVars | ContainsFlags::InitTVars,
            env,
            &mut LPooled::take(),
            t,
        )
    }

    pub fn contains_with_flags(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        t: &Self,
    ) -> Result<bool> {
        self.contains_int(flags, env, &mut LPooled::take(), t)
    }
}
