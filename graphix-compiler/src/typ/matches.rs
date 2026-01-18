use crate::{
    env::Env,
    format_with_flags,
    typ::{AbstractId, AndAc, Type},
    PrintFlag,
};
use anyhow::{bail, Result};
use enumflags2::BitFlags;
use fxhash::{FxHashMap, FxHashSet};
use netidx_value::Typ;
use poolshark::local::LPooled;

impl Type {
    fn could_match_int(
        &self,
        env: &Env,
        hist: &mut FxHashMap<(usize, usize), bool>,
        t: &Self,
    ) -> Result<bool> {
        let fl = BitFlags::empty();
        match (self, t) {
            (
                Self::Ref { scope: s0, name: n0, params: p0 },
                Self::Ref { scope: s1, name: n1, params: p1 },
            ) if s0 == s1 && n0 == n1 => Ok(p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
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
                        match t0.could_match_int(env, hist, t1) {
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
            (t0, Self::Primitive(s)) => {
                for t1 in s.iter() {
                    if t0.contains_int(fl, env, hist, &Type::Primitive(t1.into()))? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::Primitive(p), Type::Error(_)) => Ok(p.contains(Typ::Error)),
            (Type::Error(t0), Type::Error(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Array(t0), Type::Array(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Primitive(p), Type::Array(_)) => Ok(p.contains(Typ::Array)),
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                Ok(k0.could_match_int(env, hist, k1)?
                    && v0.could_match_int(env, hist, v1)?)
            }
            (Type::Primitive(p), Type::Map { .. }) => Ok(p.contains(Typ::Map)),
            (Type::Tuple(ts0), Type::Tuple(ts1)) => Ok(ts0.len() == ts1.len()
                && ts0
                    .iter()
                    .zip(ts1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::Struct(ts0), Type::Struct(ts1)) => Ok(ts0.len() == ts1.len()
                && ts0
                    .iter()
                    .zip(ts1.iter())
                    .map(|((n0, t0), (n1, t1))| {
                        Ok(n0 == n1 && t0.could_match_int(env, hist, t1)?)
                    })
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::Variant(n0, ts0), Type::Variant(n1, ts1)) => Ok(ts0.len()
                == ts1.len()
                && n0 == n1
                && ts0
                    .iter()
                    .zip(ts1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::ByRef(t0), Type::ByRef(t1)) => t0.could_match_int(env, hist, t1),
            (t0, Self::Set(ts)) => {
                for t1 in ts.iter() {
                    if t0.could_match_int(env, hist, t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::Set(ts), t1) => {
                for t0 in ts.iter() {
                    if t0.could_match_int(env, hist, t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::TVar(t0), t1) => match &*t0.read().typ.read() {
                Some(t0) => t0.could_match_int(env, hist, t1),
                None => Ok(true),
            },
            (t0, Type::TVar(t1)) => match &*t1.read().typ.read() {
                Some(t1) => t0.could_match_int(env, hist, t1),
                None => Ok(true),
            },
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) => Ok(id0 == id1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (_, Type::Bottom) => Ok(true),
            (Type::Bottom, _) => Ok(false),
            (Type::Any, _) | (_, Type::Any) => Ok(true),
            (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Fn(_), _)
            | (_, Type::Fn(_))
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _), _)
            | (_, Type::Variant(_, _))
            | (Type::ByRef(_), _)
            | (_, Type::ByRef(_))
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (_, Type::Map { .. })
            | (Type::Map { .. }, _) => Ok(false),
        }
    }

    pub fn could_match(&self, env: &Env, t: &Self) -> Result<bool> {
        self.could_match_int(env, &mut LPooled::take(), t)
    }

    pub fn sig_matches(
        &self,
        env: &Env,
        impl_type: &Self,
        adts: &mut FxHashMap<AbstractId, Type>,
    ) -> Result<()> {
        self.sig_matches_int(
            env,
            impl_type,
            &mut LPooled::take(),
            &mut LPooled::take(),
            adts,
        )
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_type: &Self,
        tvar_map: &mut FxHashMap<usize, Type>,
        hist: &mut FxHashSet<(usize, usize)>,
        adts: &mut FxHashMap<AbstractId, Type>,
    ) -> Result<()> {
        if (self as *const Type) == (impl_type as *const Type) {
            return Ok(());
        }
        match (self, impl_type) {
            (Self::Bottom, Self::Bottom) => Ok(()),
            (Self::Any, Self::Any) => Ok(()),
            (Self::Primitive(p0), Self::Primitive(p1)) if p0 == p1 => Ok(()),
            (
                Self::Ref { scope: s0, name: n0, params: p0 },
                Self::Ref { scope: s1, name: n1, params: p1 },
            ) if s0 == s1 && n0 == n1 && p0.len() == p1.len() => {
                for (t0, t1) in p0.iter().zip(p1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist, adts)?;
                }
                Ok(())
            }
            (t0 @ Self::Ref { .. }, t1) | (t0, t1 @ Self::Ref { .. }) => {
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                let t0_addr = (t0 as *const Type).addr();
                let t1_addr = (t1 as *const Type).addr();
                if hist.contains(&(t0_addr, t1_addr)) {
                    Ok(())
                } else {
                    hist.insert((t0_addr, t1_addr));
                    t0.sig_matches_int(env, t1, tvar_map, hist, adts)
                }
            }
            (Self::Fn(f0), Self::Fn(f1)) => {
                f0.sig_matches_int(env, f1, tvar_map, hist, adts)
            }
            (Self::Set(s0), Self::Set(s1)) if s0.len() == s1.len() => {
                for (t0, t1) in s0.iter().zip(s1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist, adts)?;
                }
                Ok(())
            }
            (Self::Error(e0), Self::Error(e1)) => {
                e0.sig_matches_int(env, e1, tvar_map, hist, adts)
            }
            (Self::Array(a0), Self::Array(a1)) => {
                a0.sig_matches_int(env, a1, tvar_map, hist, adts)
            }
            (Self::ByRef(b0), Self::ByRef(b1)) => {
                b0.sig_matches_int(env, b1, tvar_map, hist, adts)
            }
            (Self::Tuple(t0), Self::Tuple(t1)) if t0.len() == t1.len() => {
                for (t0, t1) in t0.iter().zip(t1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist, adts)?;
                }
                Ok(())
            }
            (Self::Struct(s0), Self::Struct(s1)) if s0.len() == s1.len() => {
                for ((n0, t0), (n1, t1)) in s0.iter().zip(s1.iter()) {
                    if n0 != n1 {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!("struct field name mismatch: {n0} vs {n1}")
                        })?
                    }
                    t0.sig_matches_int(env, t1, tvar_map, hist, adts)?;
                }
                Ok(())
            }
            (Self::Variant(tag0, t0), Self::Variant(tag1, t1))
                if tag0 == tag1 && t0.len() == t1.len() =>
            {
                for (t0, t1) in t0.iter().zip(t1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist, adts)?;
                }
                Ok(())
            }
            (Self::Map { key: k0, value: v0 }, Self::Map { key: k1, value: v1 }) => {
                k0.sig_matches_int(env, k1, tvar_map, hist, adts)?;
                v0.sig_matches_int(env, v1, tvar_map, hist, adts)
            }
            (Self::Abstract { .. }, Self::Abstract { .. }) => {
                bail!("abstract types must have a concrete definition in the implementation")
            }
            (Self::Abstract { id, params: _ }, t0) => match adts.get(id) {
                None => {
                    adts.insert(*id, t0.clone());
                    Ok(())
                }
                Some(t1) => {
                    if t0 != t1 {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!("abstract type mismatch {t0} != {t1}")
                        })?
                    }
                    Ok(())
                }
            },
            (Self::TVar(sig_tv), Self::TVar(impl_tv)) if sig_tv != impl_tv => {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!("signature type variable {sig_tv} does not match implementation {impl_tv}")
                })
            }
            (sig_type, Self::TVar(impl_tv)) => {
                let impl_tv_addr = impl_tv.inner_addr();
                match tvar_map.get(&impl_tv_addr) {
                    Some(prev_sig_type) => {
                        let matches = match (sig_type, prev_sig_type) {
                            (Type::TVar(tv0), Type::TVar(tv1)) => {
                                tv0.inner_addr() == tv1.inner_addr()
                            }
                            _ => sig_type == prev_sig_type,
                        };
                        if matches {
                            Ok(())
                        } else {
                            format_with_flags(PrintFlag::DerefTVars, || {
                                bail!(
                                    "type variable usage mismatch: expected {prev_sig_type}, got {sig_type}"
                                )
                            })
                        }
                    }
                    None => {
                        tvar_map.insert(impl_tv_addr, sig_type.clone());
                        Ok(())
                    }
                }
            }
            (Self::TVar(sig_tv), impl_type) => {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!("signature has type variable '{sig_tv} where implementation has {impl_type}")
                })
            }
            (sig_type, impl_type) => format_with_flags(PrintFlag::DerefTVars, || {
                bail!("type mismatch: signature has {sig_type}, implementation has {impl_type}")
            }),
        }
    }
}
