use crate::{
    PrintFlag,
    env::Env,
    format_with_flags,
    typ::{AndAc, RefHist, Type, TypeRef, tvar::would_cycle_inner},
};
use ahash::AHashMap;
use anyhow::{Result, bail};
use enumflags2::{BitFlags, bitflags};
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

/// True iff binding `t` into the cell would satisfy EVERY conjunct of
/// the cell's constraint list. Probe flags: the check itself must not
/// bind or alias anything.
fn cell_constraints_ok(
    tv: &crate::typ::TVar,
    env: &Env,
    hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
    t: &Type,
) -> Result<bool> {
    let cons = tv.read().typ.read().constraints.clone();
    for c in cons.iter() {
        if !c.contains_int(BitFlags::empty(), env, hist, t)? {
            return Ok(false);
        }
    }
    Ok(true)
}

impl crate::typ::TVar {
    /// Bind a constrained-unbound cell to its conjunction's witness —
    /// the narrowest conjunct every other conjunct contains. Bound and
    /// unconstrained cells are left untouched. No witness means the
    /// conjunction is unsatisfiable: a type error naming the conjuncts.
    pub fn settle(&self, env: &Env) -> Result<()> {
        let cons = {
            let tv = self.read();
            let cell = tv.typ.read();
            if cell.typ.is_some() || cell.constraints.is_empty() {
                return Ok(());
            }
            cell.constraints.clone()
        };
        let mut hist = RefHist::new(LPooled::take());
        let mut witness = None;
        let addr = self.cell_addr();
        let mut all_self_referential = true;
        'cand: for c in cons.iter() {
            // The occurs check every BIND site has, which settle was
            // missing: a conjunct can reach THIS cell (name-aliased
            // fn-signature cells merge when a polymorphic builtin is
            // used as a first-class value), and binding the cell to a
            // witness containing itself creates a CYCLIC binding that
            // every later type walk recurses on forever — the compile
            // deadlocked (walks under non-reentrant cell guards) or
            // stack-overflowed (soak jul06h). An infinite type has no
            // materializable witness; skip the conjunct.
            if would_cycle_inner(addr, c) {
                continue;
            }
            all_self_referential = false;
            for o in cons.iter() {
                if !o.contains_int(BitFlags::empty(), env, &mut hist, c)? {
                    continue 'cand;
                }
            }
            witness = Some(c.clone());
            break;
        }
        // Every conjunct reaches the cell itself: no finite witness
        // exists. Leave the cell OPEN rather than erroring — writers
        // may still refine it, and an unrefined cell terminal-settles
        // to ⊥ (fusion refuses unbound cells; the node-walk is
        // type-tolerant).
        if witness.is_none() && all_self_referential {
            return Ok(());
        }
        match witness {
            Some(w) => {
                self.read().typ.write().typ = Some(w);
                Ok(())
            }
            None => {
                format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                    let mut cs: LPooled<String> = LPooled::take();
                    for (i, c) in cons.iter().enumerate() {
                        use std::fmt::Write;
                        if i > 0 {
                            cs.push_str(" & ");
                        }
                        write!(cs, "{c}")?;
                    }
                    bail!("unsatisfiable constraints on '{}: {}", self.name, &*cs)
                })
            }
        }
    }

    /// TERMINAL settle: like [`Self::settle`], but an UNCONSTRAINED
    /// unbound cell binds to ⊥. By terminal-settle time every writer
    /// has had the whole typecheck0 phase (args, annotations, connect
    /// targets) and the constrained path its witness — a cell still
    /// open with no constraints means nothing ever produced or bounded
    /// it, and the honest type of a value that never arrives is Bottom
    /// (`never()`'s result cell is the canonical case: its declared
    /// rtype is the LITERAL ⊥, which unifies without binding, so the
    /// call-site cell reaches here open). Only the terminal walk uses
    /// this; the tc0-time derived settle keeps plain `settle` so it
    /// can't foreclose writers that haven't typechecked yet.
    pub fn settle_or_bottom(&self, env: &Env) -> Result<()> {
        {
            let tv = self.read();
            let cell = tv.typ.read();
            if cell.typ.is_some() {
                return Ok(());
            }
            if cell.constraints.is_empty() {
                drop(cell);
                tv.typ.write().typ = Some(Type::Bottom);
                return Ok(());
            }
        }
        self.settle(env)
    }
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
        hist: &mut RefHist<AHashMap<(Option<usize>, Option<usize>), bool>>,
        t: &Self,
    ) -> Result<bool> {
        if (self as *const Type) == (t as *const Type) {
            return Ok(true);
        }
        match (self, t) {
            (
                Self::Ref(TypeRef { scope: s0, name: n0, params: p0, .. }),
                Self::Ref(TypeRef { scope: s1, name: n1, params: p1, .. }),
            ) if s0 == s1 && n0 == n1 => Ok(p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (t0 @ Self::Ref(TypeRef { .. }), t1)
            | (t0, t1 @ Self::Ref(TypeRef { .. })) => {
                let t0_id = hist.ref_id(t0, env);
                let t1_id = hist.ref_id(t1, env);
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                match hist.get(&(t0_id, t1_id)) {
                    Some(r) => Ok(*r),
                    None => {
                        hist.insert((t0_id, t1_id), true);
                        let r = t0.contains_int(flags, env, hist, &t1);
                        hist.remove(&(t0_id, t1_id));
                        r
                    }
                }
            }
            // ⊥ fits into anything an open cell may later become
            // (⊥ ⊆ T for every T), so binding here would gain no
            // information and FORECLOSE the cell's writers: with
            // never() typed ⊥, an eager bind broke both
            // `f(never(), i64:5)` (the shared 'a instance bound ⊥
            // first, then rejected the i64) and the connect-seed idiom
            // (`let res = never(); res <- v` — the binding shares the
            // call site's cell, and a ⊥-pinned binding rejects every
            // write). Accept and leave the cell open; writers refine it
            // during typecheck0, and a cell NOBODY refines defaults to
            // ⊥ at the terminal settle (`TVar::settle_or_bottom`) — so
            // the never-arm/union pollution still collapses, just one
            // phase later.
            (Self::TVar(_), Self::Bottom) => Ok(true),
            (Self::Bottom, Self::TVar(t0)) => {
                if let Some(Type::Bottom) = &t0.read().typ.read().typ {
                    return Ok(true);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    t0.read().typ.write().typ = Some(Self::Bottom);
                }
                Ok(true)
            }
            (Self::Bottom, Self::Bottom) => Ok(true),
            (Self::Bottom, _) => Ok(false),
            (_, Self::Bottom) => Ok(true),
            (Self::TVar(t0), Self::Any) => {
                // Clone the binding OUT of the guards before recursing
                // (here and in every deref arm below): an `if let` over
                // `&t0.read().typ.read().typ` keeps BOTH read guards
                // alive for the whole body, the recursion can revisit
                // THIS cell (entangled fn-sig cells appear at several
                // depths), and its bind arm write-locks it —
                // parking_lot locks are non-reentrant, so the thread
                // deadlocks ITSELF (soak jul06h: a polymorphic builtin
                // as a first-class array element wedged the compile,
                // ASLR-order dependent via the Set sort).
                let bound = t0.read().typ.read().typ.clone();
                if let Some(t0) = bound {
                    return t0.contains_int(flags, env, hist, t);
                }
                if !cell_constraints_ok(t0, env, hist, &Self::Any)? {
                    return Ok(false);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    t0.read().typ.write().typ = Some(Self::Any);
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
            (Self::Tuple(t0), Self::Tuple(t1)) if Arc::ptr_eq(t0, t1) => Ok(true),
            (Self::Tuple(t0), Self::Tuple(t1)) => Ok(t0.len() == t1.len()
                && t0
                    .iter()
                    .zip(t1.iter())
                    .map(|(t0, t1)| t0.contains_int(flags, env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Self::Struct(t0), Self::Struct(t1)) if Arc::ptr_eq(t0, t1) => Ok(true),
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
                if tg0.as_ptr() == tg1.as_ptr() && Arc::ptr_eq(t0, t1) =>
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
                // Both-bound recursion happens OUTSIDE the guard block:
                // recursing with these four guards held self-deadlocks
                // when the walk revisits either cell and binds it (see
                // the `(TVar, Any)` arm's note).
                enum ActOrRecurse {
                    Act(Act, Option<Type>),
                    Recurse(Type, Type),
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
                    match (&t0i.typ, &t1i.typ) {
                        (Some(t0), Some(t1)) => {
                            ActOrRecurse::Recurse(t0.clone(), t1.clone())
                        }
                        (None, None) => {
                            if t0.frozen && t1.frozen {
                                return Ok(true);
                            }
                            if t0.frozen {
                                ActOrRecurse::Act(Act::RightAlias, None)
                            } else {
                                ActOrRecurse::Act(Act::LeftAlias, None)
                            }
                        }
                        (Some(b), None) => {
                            ActOrRecurse::Act(Act::RightCopy, Some(b.clone()))
                        }
                        (None, Some(b)) => {
                            ActOrRecurse::Act(Act::LeftCopy, Some(b.clone()))
                        }
                    }
                };
                let (act, bound) = match act {
                    ActOrRecurse::Recurse(a, b) => {
                        return a.contains_int(flags, env, hist, &b);
                    }
                    ActOrRecurse::Act(act, bound) => (act, bound),
                };
                // A copy binds the RECEIVING cell to the source's
                // binding — the receiver's constraints must admit it
                // (checked lock-free on the cloned-out binding).
                match act {
                    Act::RightCopy if flags.contains(ContainsFlags::InitTVars) => {
                        let b = bound.as_ref().expect("copy without binding");
                        if !cell_constraints_ok(t1, env, hist, b)? {
                            return Ok(false);
                        }
                        t1.copy(t0)
                    }
                    Act::RightAlias if flags.contains(ContainsFlags::AliasTVars) => {
                        t1.alias(t0)
                    }
                    Act::LeftAlias if flags.contains(ContainsFlags::AliasTVars) => {
                        t0.alias(t1)
                    }
                    Act::LeftCopy if flags.contains(ContainsFlags::InitTVars) => {
                        let b = bound.as_ref().expect("copy without binding");
                        if !cell_constraints_ok(t0, env, hist, b)? {
                            return Ok(false);
                        }
                        t0.copy(t1)
                    }
                    Act::RightCopy | Act::RightAlias | Act::LeftAlias | Act::LeftCopy => {
                        ()
                    }
                }
                Ok(true)
            }
            (Self::TVar(t0), t1) if !t0.would_cycle(t1) => {
                // Deref-clone before recursing — see the `(TVar, Any)`
                // arm's guard-across-recursion note.
                let bound = t0.read().typ.read().typ.clone();
                if let Some(t0) = bound {
                    return t0.contains_int(flags, env, hist, t1);
                }
                // The cell's constraints must admit the binding — a
                // violation fails the unification HERE, at the site
                // that tried it, instead of baking a wide binding that
                // collides somewhere downstream.
                if !cell_constraints_ok(t0, env, hist, t1)? {
                    return Ok(false);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    if std::env::var("GRAPHIX_DBG_BIND").is_ok() {
                        eprintln!(
                            "BIND lhs '{}({:x}) := {t1:?}",
                            t0.name,
                            t0.cell_addr()
                        );
                    }
                    t0.read().typ.write().typ = Some(t1.clone());
                }
                Ok(true)
            }
            (t0, Self::TVar(t1)) if !t1.would_cycle(t0) => {
                // Deref-clone before recursing — see the `(TVar, Any)`
                // arm's guard-across-recursion note.
                let bound = t1.read().typ.read().typ.clone();
                if let Some(t1) = bound {
                    return t0.contains_int(flags, env, hist, &t1);
                }
                if !cell_constraints_ok(t1, env, hist, t0)? {
                    return Ok(false);
                }
                if flags.contains(ContainsFlags::InitTVars) {
                    if std::env::var("GRAPHIX_DBG_BIND").is_ok() {
                        eprintln!(
                            "BIND rhs '{}({:x}) := {t0:?}",
                            t1.name,
                            t1.cell_addr()
                        );
                    }
                    t1.read().typ.write().typ = Some(t0.clone());
                }
                Ok(true)
            }
            (Self::Set(s0), Self::Set(s1)) if Arc::ptr_eq(s0, s1) => Ok(true),
            (t0 @ Self::Set(_), t1 @ Self::Set(_)) if t0 == t1 => {
                if flags.contains(ContainsFlags::InitTVars) {
                    let mut known = LPooled::take();
                    t0.alias_tvars(&mut known);
                    t1.alias_tvars(&mut known);
                }
                Ok(true)
            }
            (t0, Self::Set(s)) => Ok(s
                .iter()
                .map(|t1| t0.contains_int(flags, env, hist, t1))
                .collect::<Result<AndAc>>()?
                .0),
            (Self::Set(s), t) => {
                let probe = BitFlags::empty();
                let whole_ok =
                    s.iter().fold(Ok::<_, anyhow::Error>(false), |acc, t0| {
                        Ok(acc? || t0.contains_int(probe, env, hist, t)?)
                    })?;
                let prims_ok =
                    t.iter_prims().fold(Ok::<_, anyhow::Error>(true), |acc, t1| {
                        Ok(acc?
                            && s.iter().fold(
                                Ok::<_, anyhow::Error>(false),
                                |acc, t0| {
                                    Ok(acc? || t0.contains_int(probe, env, hist, &t1)?)
                                },
                            )?)
                    })?;
                match (whole_ok, prims_ok) {
                    (false, false) => Ok(false),
                    // prefer prims when valid — narrowest TVar bindings
                    (_, true) => Ok(t.iter_prims().fold(
                        Ok::<_, anyhow::Error>(true),
                        |acc, t1| {
                            Ok(acc?
                                && s.iter().fold(
                                    Ok::<_, anyhow::Error>(false),
                                    |acc, t0| {
                                        Ok(acc?
                                            || t0.contains_int(flags, env, hist, &t1)?)
                                    },
                                )?)
                        },
                    )?),
                    (true, false) => {
                        Ok(s.iter().fold(Ok::<_, anyhow::Error>(false), |acc, t0| {
                            Ok(acc? || t0.contains_int(flags, env, hist, t)?)
                        })?)
                    }
                }
            }
            (Self::Fn(f0), Self::Fn(f1)) => {
                let same = Arc::ptr_eq(f0, f1);
                let r = same || f0.contains_int(flags, env, hist, f1)?;
                if r && !same && flags.contains(ContainsFlags::InitTVars) {
                    f0.lambda_ids.link(&f1.lambda_ids);
                }
                Ok(r)
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
            &mut RefHist::new(LPooled::take()),
            t,
        )
    }

    pub fn contains_with_flags(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env,
        t: &Self,
    ) -> Result<bool> {
        self.contains_int(flags, env, &mut RefHist::new(LPooled::take()), t)
    }
}
