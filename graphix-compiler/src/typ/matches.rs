use crate::{
    PrintFlag,
    env::Env,
    format_with_flags,
    stack::ensure_sufficient,
    typ::{AndAc, RefHist, RefPair, Type, TypeRef, contains::ContainsHist},
};
use ahash::AHashSet;
use anyhow::{Result, bail};
use enumflags2::BitFlags;
use netidx_value::Typ;
use nohash::IntMap;
use poolshark::local::LPooled;
use std::ops::{Deref, DerefMut};

/// could_match's walk state: its pairs in progress (assumed to overlap),
/// and a memo of its own for the containment questions it asks.
pub(super) struct MatchHist {
    pub(super) hist: RefHist<AHashSet<RefPair>>,
    contains: ContainsHist,
}

impl MatchHist {
    pub(super) fn new() -> Self {
        MatchHist { hist: RefHist::new(), contains: ContainsHist::new() }
    }
}

impl Deref for MatchHist {
    type Target = RefHist<AHashSet<RefPair>>;

    fn deref(&self) -> &Self::Target {
        &self.hist
    }
}

impl DerefMut for MatchHist {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.hist
    }
}

impl Type {
    pub(super) fn could_match_int(
        &self,
        env: &Env,
        hist: &mut MatchHist,
        t: &Self,
    ) -> Result<bool> {
        ensure_sufficient(|| self.could_match_inner(env, hist, t))
    }

    fn could_match_inner(
        &self,
        env: &Env,
        hist: &mut MatchHist,
        t: &Self,
    ) -> Result<bool> {
        // A bound constructor application is its filled type.
        if let Type::App(c, a) = self
            && let Some(filled) = Type::app_filled(c, a)
        {
            return filled.could_match_int(env, hist, t);
        }
        if let Type::App(c, a) = t
            && let Some(filled) = Type::app_filled(c, a)
        {
            return self.could_match_int(env, hist, &filled);
        }
        match (self, t) {
            (Self::Ref(tr0), Self::Ref(tr1))
                if tr0.scope == tr1.scope
                    && tr0.name == tr1.name
                    && tr0.cells_agree(tr1) =>
            {
                Ok(tr0.params.len() == tr1.params.len()
                    && tr0
                        .params
                        .iter()
                        .zip(tr1.params.iter())
                        .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            (t0 @ Self::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Self::Ref(TypeRef { .. })) => {
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                if hist.contains(&key) {
                    return Ok(true);
                }
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                hist.insert(key);
                let r = t0.could_match_int(env, hist, &t1);
                hist.remove(&key);
                r
            }
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole) => Ok(self == t),
            (t0, Self::Primitive(s)) => {
                for t1 in s.iter() {
                    let t1 = Type::Primitive(t1.into());
                    if t0.contains_int(BitFlags::empty(), env, &mut hist.contains, &t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::Primitive(p), Type::Error(_)) => Ok(p.contains(Typ::Error)),
            (Type::Error(t0), Type::Error(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Array(t0), Type::Array(t1)) => t0.could_match_int(env, hist, t1),
            (Type::List(t0), Type::List(t1)) => t0.could_match_int(env, hist, t1),
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
                    .map(|((n0, t0, _), (n1, t1, _))| {
                        Ok(n0 == n1 && t0.could_match_int(env, hist, t1)?)
                    })
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::Variant(n0, ts0, _), Type::Variant(n1, ts1, _)) => Ok(ts0.len()
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
            // Bindings are cloned out: the recursion may lock these cells.
            (Type::TVar(t0), t1) => match t0.binding() {
                Some(t0) => t0.could_match_int(env, hist, t1),
                None => Ok(true),
            },
            (t0, Type::TVar(t1)) => match t1.binding() {
                Some(t1) => t0.could_match_int(env, hist, &t1),
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
            (Type::Fn(f0), Type::Fn(f1)) => f0.could_match_int(env, hist, f1),
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
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
            | (Type::ByRef(_), _)
            | (_, Type::ByRef(_))
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (_, Type::Map { .. })
            | (Type::Map { .. }, _) => Ok(false),
        }
    }

    pub fn could_match(&self, env: &Env, t: &Self) -> Result<bool> {
        self.could_match_int(env, &mut MatchHist::new(), t)
    }

    pub fn sig_matches(&self, env: &Env, impl_type: &Self) -> Result<()> {
        self.sig_matches_int(env, impl_type, &mut LPooled::take(), &mut RefHist::new())
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_type: &Self,
        tvar_map: &mut IntMap<usize, Type>,
        hist: &mut RefHist<AHashSet<RefPair>>,
    ) -> Result<()> {
        ensure_sufficient(|| self.sig_matches_inner(env, impl_type, tvar_map, hist))
    }

    fn sig_matches_inner(
        &self,
        env: &Env,
        impl_type: &Self,
        tvar_map: &mut IntMap<usize, Type>,
        hist: &mut RefHist<AHashSet<RefPair>>,
    ) -> Result<()> {
        if (self as *const Type) == (impl_type as *const Type) {
            return Ok(());
        }
        match (self, impl_type) {
            (Self::Bottom, Self::Bottom) => Ok(()),
            (Self::Any, Self::Any) => Ok(()),
            (Self::Primitive(p0), Self::Primitive(p1)) if p0 == p1 => Ok(()),
            (
                Self::Ref(TypeRef { scope: s0, name: n0, params: p0, .. }),
                Self::Ref(TypeRef { scope: s1, name: n1, params: p1, .. }),
            ) if s0 == s1 && n0 == n1 && p0.len() == p1.len() => {
                for (t0, t1) in p0.iter().zip(p1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (t0 @ Self::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Self::Ref(TypeRef { .. })) => {
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                if hist.contains(&key) {
                    return Ok(());
                }
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                hist.insert(key);
                let r = t0.sig_matches_int(env, &t1, tvar_map, hist);
                hist.remove(&key);
                r
            }
            (Self::Fn(f0), Self::Fn(f1)) => {
                f0.sig_matches_int(env, f1, tvar_map, hist)?;
                f0.lambda_ids.link(&f1.lambda_ids);
                Ok(())
            }
            (Self::Set(s0), Self::Set(s1)) if s0.len() == s1.len() => {
                for (t0, t1) in s0.iter().zip(s1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Error(e0), Self::Error(e1)) => {
                e0.sig_matches_int(env, e1, tvar_map, hist)
            }
            (Self::Array(a0), Self::Array(a1)) => {
                a0.sig_matches_int(env, a1, tvar_map, hist)
            }
            (Self::List(a0), Self::List(a1)) => {
                a0.sig_matches_int(env, a1, tvar_map, hist)
            }
            (Self::ByRef(b0), Self::ByRef(b1)) => {
                b0.sig_matches_int(env, b1, tvar_map, hist)
            }
            (Self::Tuple(t0), Self::Tuple(t1)) if t0.len() == t1.len() => {
                for (t0, t1) in t0.iter().zip(t1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Struct(s0), Self::Struct(s1)) if s0.len() == s1.len() => {
                for ((n0, t0, _), (n1, t1, _)) in s0.iter().zip(s1.iter()) {
                    if n0 != n1 {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!("struct field name mismatch: {n0} vs {n1}")
                        })?
                    }
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Variant(tag0, t0, _), Self::Variant(tag1, t1, _))
                if tag0 == tag1 && t0.len() == t1.len() =>
            {
                for (t0, t1) in t0.iter().zip(t1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Map { key: k0, value: v0 }, Self::Map { key: k1, value: v1 }) => {
                k0.sig_matches_int(env, k1, tvar_map, hist)?;
                v0.sig_matches_int(env, v1, tvar_map, hist)
            }
            (
                Self::Abstract { id: id0, params: p0 },
                Self::Abstract { id: id1, params: p1 },
            ) if id0 == id1 && p0.len() == p1.len() => {
                for (t0, t1) in p0.iter().zip(p1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::App(c0, a0), Self::App(c1, a1)) => {
                c0.sig_matches_int(env, c1, tvar_map, hist)?;
                a0.sig_matches_int(env, a1, tvar_map, hist)
            }
            (Self::Hole, Self::Hole) => Ok(()),
            // A bound signature var is its binding.
            (Self::TVar(sig_tv), impl_type) if let Some(b) = sig_tv.binding() => {
                b.sig_matches_int(env, impl_type, tvar_map, hist)
            }
            (sig_type, Self::TVar(impl_tv)) => {
                // A bound impl tvar is a solved fact: the signature's
                // concrete type must match its binding structurally.
                if let Some(b) = impl_tv.binding() {
                    return sig_type.sig_matches_int(env, &b, tvar_map, hist);
                }
                match tvar_map.get(&impl_tv.cell_addr()) {
                    Some(prev_sig_type) => {
                        let matches = match (sig_type, prev_sig_type) {
                            (Type::TVar(tv0), Type::TVar(tv1)) => tv0.same_cell(tv1),
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
                        tvar_map.insert(impl_tv.cell_addr(), sig_type.clone());
                        Ok(())
                    }
                }
            }
            (Self::TVar(sig_tv), impl_type) => {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "signature has type variable '{sig_tv} where implementation has {impl_type}"
                    )
                })
            }
            (sig_type, impl_type) => format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "type mismatch: signature has {sig_type}, implementation has {impl_type}"
                )
            }),
        }
    }
}
