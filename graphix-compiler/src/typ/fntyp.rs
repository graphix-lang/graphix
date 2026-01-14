use super::AndAc;
use crate::{
    env::Env,
    expr::{
        print::{PrettyBuf, PrettyDisplay},
        ModPath,
    },
    typ::{ContainsFlags, TVar, Type},
    Rt, UserEvent,
};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use fxhash::FxHashMap;
use parking_lot::RwLock;
use poolshark::local::LPooled;
use smallvec::{smallvec, SmallVec};
use std::{
    cmp::{Eq, Ordering, PartialEq},
    fmt::{self, Debug, Write},
};
use triomphe::Arc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnArgType {
    pub label: Option<(ArcStr, bool)>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct FnType {
    pub args: Arc<[FnArgType]>,
    pub vargs: Option<Type>,
    pub rtype: Type,
    pub constraints: Arc<RwLock<LPooled<Vec<(TVar, Type)>>>>,
    pub throws: Type,
    pub explicit_throws: bool,
}

impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            args: args0,
            vargs: vargs0,
            rtype: rtype0,
            constraints: constraints0,
            throws: th0,
            explicit_throws: _,
        } = self;
        let Self {
            args: args1,
            vargs: vargs1,
            rtype: rtype1,
            constraints: constraints1,
            throws: th1,
            explicit_throws: _,
        } = other;
        args0 == args1
            && vargs0 == vargs1
            && rtype0 == rtype1
            && &*constraints0.read() == &*constraints1.read()
            && th0 == th1
    }
}

impl Eq for FnType {}

impl PartialOrd for FnType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use std::cmp::Ordering;
        let Self {
            args: args0,
            vargs: vargs0,
            rtype: rtype0,
            constraints: constraints0,
            throws: th0,
            explicit_throws: _,
        } = self;
        let Self {
            args: args1,
            vargs: vargs1,
            rtype: rtype1,
            constraints: constraints1,
            throws: th1,
            explicit_throws: _,
        } = other;
        match args0.partial_cmp(&args1) {
            Some(Ordering::Equal) => match vargs0.partial_cmp(vargs1) {
                Some(Ordering::Equal) => match rtype0.partial_cmp(rtype1) {
                    Some(Ordering::Equal) => {
                        match constraints0.read().partial_cmp(&*constraints1.read()) {
                            Some(Ordering::Equal) => th0.partial_cmp(th1),
                            r => r,
                        }
                    }
                    r => r,
                },
                r => r,
            },
            r => r,
        }
    }
}

impl Ord for FnType {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Default for FnType {
    fn default() -> Self {
        Self {
            args: Arc::from_iter([]),
            vargs: None,
            rtype: Default::default(),
            constraints: Arc::new(RwLock::new(LPooled::take())),
            throws: Default::default(),
            explicit_throws: false,
        }
    }
}

impl FnType {
    pub(super) fn normalize(&self) -> Self {
        let Self { args, vargs, rtype, constraints, throws, explicit_throws } = self;
        let args = Arc::from_iter(
            args.iter()
                .map(|a| FnArgType { label: a.label.clone(), typ: a.typ.normalize() }),
        );
        let vargs = vargs.as_ref().map(|t| t.normalize());
        let rtype = rtype.normalize();
        let constraints = Arc::new(RwLock::new(
            constraints
                .read()
                .iter()
                .map(|(tv, t)| (tv.clone(), t.normalize()))
                .collect(),
        ));
        let throws = throws.normalize();
        let explicit_throws = *explicit_throws;
        FnType { args, vargs, rtype, constraints, throws, explicit_throws }
    }

    pub fn unbind_tvars(&self) {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws: _ } = self;
        for arg in args.iter() {
            arg.typ.unbind_tvars()
        }
        if let Some(t) = vargs {
            t.unbind_tvars()
        }
        rtype.unbind_tvars();
        for (tv, tc) in constraints.read().iter() {
            tv.unbind();
            tc.unbind_tvars()
        }
        throws.unbind_tvars();
    }

    pub fn constrain_known(&self) {
        let mut known = LPooled::take();
        self.collect_tvars(&mut known);
        let mut constraints = self.constraints.write();
        for (name, tv) in known.drain() {
            if let Some(t) = tv.read().typ.read().as_ref()
                && t != &Type::Bottom
                && t != &Type::Any
            {
                if !constraints.iter().any(|(tv, _)| tv.name == name) {
                    t.bind_as(&Type::Any);
                    constraints.push((tv.clone(), t.normalize()));
                }
            }
        }
    }

    pub fn reset_tvars(&self) -> Self {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws } = self;
        let args = Arc::from_iter(
            args.iter()
                .map(|a| FnArgType { label: a.label.clone(), typ: a.typ.reset_tvars() }),
        );
        let vargs = vargs.as_ref().map(|t| t.reset_tvars());
        let rtype = rtype.reset_tvars();
        let constraints = Arc::new(RwLock::new(
            constraints
                .read()
                .iter()
                .map(|(tv, tc)| (TVar::empty_named(tv.name.clone()), tc.reset_tvars()))
                .collect(),
        ));
        let throws = throws.reset_tvars();
        let explicit_throws = *explicit_throws;
        FnType { args, vargs, rtype, constraints, throws, explicit_throws }
    }

    pub fn replace_tvars(&self, known: &FxHashMap<ArcStr, Type>) -> Self {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws } = self;
        let args = Arc::from_iter(args.iter().map(|a| FnArgType {
            label: a.label.clone(),
            typ: a.typ.replace_tvars(known),
        }));
        let vargs = vargs.as_ref().map(|t| t.replace_tvars(known));
        let rtype = rtype.replace_tvars(known);
        let constraints = constraints.clone();
        let throws = throws.replace_tvars(known);
        let explicit_throws = *explicit_throws;
        FnType { args, vargs, rtype, constraints, throws, explicit_throws }
    }

    /// replace automatically constrained type variables with their
    /// constraint type. This is only useful for making nicer display
    /// types in IDEs and shells.
    pub fn replace_auto_constrained(&self) -> Self {
        let mut known: LPooled<FxHashMap<ArcStr, Type>> = LPooled::take();
        let Self { args, vargs, rtype, constraints, throws, explicit_throws } = self;
        let constraints: LPooled<Vec<(TVar, Type)>> = constraints
            .read()
            .iter()
            .filter_map(|(tv, ct)| {
                if tv.name.starts_with("_") {
                    known.insert(tv.name.clone(), ct.clone());
                    None
                } else {
                    Some((tv.clone(), ct.clone()))
                }
            })
            .collect();
        let constraints = Arc::new(RwLock::new(constraints));
        let args = Arc::from_iter(args.iter().map(|FnArgType { label, typ }| {
            FnArgType { label: label.clone(), typ: typ.replace_tvars(&known) }
        }));
        let vargs = vargs.as_ref().map(|t| t.replace_tvars(&known));
        let rtype = rtype.replace_tvars(&known);
        let throws = throws.replace_tvars(&known);
        let explicit_throws = *explicit_throws;
        Self { args, vargs, rtype, constraints, throws, explicit_throws }
    }

    pub fn has_unbound(&self) -> bool {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws: _ } = self;
        args.iter().any(|a| a.typ.has_unbound())
            || vargs.as_ref().map(|t| t.has_unbound()).unwrap_or(false)
            || rtype.has_unbound()
            || constraints
                .read()
                .iter()
                .any(|(tv, tc)| tv.read().typ.read().is_none() || tc.has_unbound())
            || throws.has_unbound()
    }

    pub fn bind_as(&self, t: &Type) {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws: _ } = self;
        for a in args.iter() {
            a.typ.bind_as(t)
        }
        if let Some(va) = vargs.as_ref() {
            va.bind_as(t)
        }
        rtype.bind_as(t);
        for (tv, tc) in constraints.read().iter() {
            let tv = tv.read();
            let mut tv = tv.typ.write();
            if tv.is_none() {
                *tv = Some(t.clone())
            }
            tc.bind_as(t)
        }
        throws.bind_as(t);
    }

    pub fn alias_tvars(&self, known: &mut FxHashMap<ArcStr, TVar>) {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws: _ } = self;
        for arg in args.iter() {
            arg.typ.alias_tvars(known)
        }
        if let Some(vargs) = vargs {
            vargs.alias_tvars(known)
        }
        rtype.alias_tvars(known);
        for (tv, tc) in constraints.read().iter() {
            Type::TVar(tv.clone()).alias_tvars(known);
            tc.alias_tvars(known);
        }
        throws.alias_tvars(known);
    }

    pub fn collect_tvars(&self, known: &mut FxHashMap<ArcStr, TVar>) {
        let FnType { args, vargs, rtype, constraints, throws, explicit_throws: _ } = self;
        for arg in args.iter() {
            arg.typ.collect_tvars(known)
        }
        if let Some(vargs) = vargs {
            vargs.collect_tvars(known)
        }
        rtype.collect_tvars(known);
        for (tv, tc) in constraints.read().iter() {
            Type::TVar(tv.clone()).collect_tvars(known);
            tc.collect_tvars(known);
        }
        throws.collect_tvars(known);
    }

    pub fn contains<R: Rt, E: UserEvent>(
        &self,
        env: &Env<R, E>,
        t: &Self,
    ) -> Result<bool> {
        self.contains_int(
            ContainsFlags::AliasTVars | ContainsFlags::InitTVars,
            env,
            &mut LPooled::take(),
            t,
        )
    }

    pub(super) fn contains_int<R: Rt, E: UserEvent>(
        &self,
        flags: BitFlags<ContainsFlags>,
        env: &Env<R, E>,
        hist: &mut FxHashMap<(usize, usize), bool>,
        t: &Self,
    ) -> Result<bool> {
        let mut sul = 0;
        let mut tul = 0;
        for (i, a) in self.args.iter().enumerate() {
            sul = i;
            match &a.label {
                None => {
                    break;
                }
                Some((l, _)) => match t
                    .args
                    .iter()
                    .find(|a| a.label.as_ref().map(|a| &a.0) == Some(l))
                {
                    None => return Ok(false),
                    Some(o) => {
                        if !o.typ.contains_int(flags, env, hist, &a.typ)? {
                            return Ok(false);
                        }
                    }
                },
            }
        }
        for (i, a) in t.args.iter().enumerate() {
            tul = i;
            match &a.label {
                None => {
                    break;
                }
                Some((l, opt)) => match self
                    .args
                    .iter()
                    .find(|a| a.label.as_ref().map(|a| &a.0) == Some(l))
                {
                    Some(_) => (),
                    None => {
                        if !opt {
                            return Ok(false);
                        }
                    }
                },
            }
        }
        let slen = self.args.len() - sul;
        let tlen = t.args.len() - tul;
        Ok(slen == tlen
            && t.args[tul..]
                .iter()
                .zip(self.args[sul..].iter())
                .map(|(t, s)| t.typ.contains_int(flags, env, hist, &s.typ))
                .collect::<Result<AndAc>>()?
                .0
            && match (&t.vargs, &self.vargs) {
                (Some(tv), Some(sv)) => tv.contains_int(flags, env, hist, sv)?,
                (None, None) => true,
                (_, _) => false,
            }
            && self.rtype.contains_int(flags, env, hist, &t.rtype)?
            && self
                .constraints
                .read()
                .iter()
                .map(|(tv, tc)| {
                    tc.contains_int(flags, env, hist, &Type::TVar(tv.clone()))
                })
                .collect::<Result<AndAc>>()?
                .0
            && t.constraints
                .read()
                .iter()
                .map(|(tv, tc)| {
                    tc.contains_int(flags, env, hist, &Type::TVar(tv.clone()))
                })
                .collect::<Result<AndAc>>()?
                .0
            && self.throws.contains_int(flags, env, hist, &t.throws)?)
    }

    pub fn check_contains<R: Rt, E: UserEvent>(
        &self,
        env: &Env<R, E>,
        other: &Self,
    ) -> Result<()> {
        if !self.contains(env, other)? {
            bail!("Fn type mismatch {self} does not contain {other}")
        }
        Ok(())
    }

    /// Return true if function signatures are contained. This is contains,
    /// but does not allow labeled argument subtyping.
    pub fn sig_contains<R: Rt, E: UserEvent>(
        &self,
        env: &Env<R, E>,
        other: &Self,
    ) -> Result<bool> {
        let Self {
            args: args0,
            vargs: vargs0,
            rtype: rtype0,
            constraints: constraints0,
            throws: tr0,
            explicit_throws: _,
        } = self;
        let Self {
            args: args1,
            vargs: vargs1,
            rtype: rtype1,
            constraints: constraints1,
            throws: tr1,
            explicit_throws: _,
        } = other;
        Ok(args0.len() == args1.len()
            && args0
                .iter()
                .zip(args1.iter())
                .map(
                    |(a0, a1)| Ok(a0.label == a1.label && a0.typ.contains(env, &a1.typ)?),
                )
                .collect::<Result<AndAc>>()?
                .0
            && match (vargs0, vargs1) {
                (None, None) => true,
                (None, _) | (_, None) => false,
                (Some(t0), Some(t1)) => t0.contains(env, t1)?,
            }
            && rtype0.contains(env, rtype1)?
            && constraints0
                .read()
                .iter()
                .map(|(tv, tc)| tc.contains(env, &Type::TVar(tv.clone())))
                .collect::<Result<AndAc>>()?
                .0
            && constraints1
                .read()
                .iter()
                .map(|(tv, tc)| tc.contains(env, &Type::TVar(tv.clone())))
                .collect::<Result<AndAc>>()?
                .0
            && tr0.contains(env, tr1)?)
    }

    pub fn check_sig_contains<R: Rt, E: UserEvent>(
        &self,
        env: &Env<R, E>,
        other: &Self,
    ) -> Result<()> {
        if !self.sig_contains(env, other)? {
            bail!("Fn signature {self} does not contain {other}")
        }
        Ok(())
    }

    pub fn sig_matches(&self, impl_fn: &Self) -> Result<()> {
        self.sig_matches_int(impl_fn, &mut LPooled::take())
    }

    pub(super) fn sig_matches_int(
        &self,
        impl_fn: &Self,
        tvar_map: &mut FxHashMap<usize, Type>,
    ) -> Result<()> {
        let Self {
            args: sig_args,
            vargs: sig_vargs,
            rtype: sig_rtype,
            constraints: sig_constraints,
            throws: sig_throws,
            explicit_throws: _,
        } = self;
        let Self {
            args: impl_args,
            vargs: impl_vargs,
            rtype: impl_rtype,
            constraints: impl_constraints,
            throws: impl_throws,
            explicit_throws: _,
        } = impl_fn;
        if sig_args.len() != impl_args.len() {
            bail!(
                "argument count mismatch: signature has {}, implementation has {}",
                sig_args.len(),
                impl_args.len()
            );
        }
        for (i, (sig_arg, impl_arg)) in sig_args.iter().zip(impl_args.iter()).enumerate()
        {
            if sig_arg.label != impl_arg.label {
                bail!(
                    "argument {} label mismatch: signature has {:?}, implementation has {:?}",
                    i,
                    sig_arg.label,
                    impl_arg.label
                );
            }
            sig_arg
                .typ
                .sig_matches_int(&impl_arg.typ, tvar_map)
                .with_context(|| format!("in argument {i}"))?;
        }
        match (sig_vargs, impl_vargs) {
            (None, None) => (),
            (Some(sig_va), Some(impl_va)) => {
                sig_va
                    .sig_matches_int(impl_va, tvar_map)
                    .context("in variadic argument")?;
            }
            (None, Some(_)) => {
                bail!("signature has no variadic args but implementation does")
            }
            (Some(_), None) => {
                bail!("signature has variadic args but implementation does not")
            }
        }
        sig_rtype.sig_matches_int(impl_rtype, tvar_map).context("in return type")?;
        sig_throws.sig_matches_int(impl_throws, tvar_map).context("in throws clause")?;
        let sig_cons = sig_constraints.read();
        let impl_cons = impl_constraints.read();
        for (sig_tv, sig_tc) in sig_cons.iter() {
            if !impl_cons
                .iter()
                .any(|(impl_tv, impl_tc)| sig_tv == impl_tv && sig_tc == impl_tc)
            {
                bail!("missing constraint {sig_tv}: {sig_tc} in implementation")
            }
        }
        for (impl_tv, impl_tc) in impl_cons.iter() {
            match tvar_map.get(&impl_tv.canonical_addr()).cloned() {
                None | Some(Type::TVar(_)) => (),
                Some(sig_type) => {
                    sig_type.sig_matches_int(impl_tc, tvar_map).with_context(|| {
                        format!(
                            "signature has concrete type {sig_type}, implementation constraint is {impl_tc}"
                        )
                    })?;
                }
            }
        }
        Ok(())
    }

    pub fn map_argpos(
        &self,
        other: &Self,
    ) -> LPooled<FxHashMap<ArcStr, (Option<usize>, Option<usize>)>> {
        let mut tbl: LPooled<FxHashMap<ArcStr, (Option<usize>, Option<usize>)>> =
            LPooled::take();
        for (i, a) in self.args.iter().enumerate() {
            match &a.label {
                None => break,
                Some((n, _)) => tbl.entry(n.clone()).or_default().0 = Some(i),
            }
        }
        for (i, a) in other.args.iter().enumerate() {
            match &a.label {
                None => break,
                Some((n, _)) => tbl.entry(n.clone()).or_default().1 = Some(i),
            }
        }
        tbl
    }

    pub fn scope_refs(&self, scope: &ModPath) -> Self {
        let vargs = self.vargs.as_ref().map(|t| t.scope_refs(scope));
        let rtype = self.rtype.scope_refs(scope);
        let args =
            Arc::from_iter(self.args.iter().map(|a| FnArgType {
                label: a.label.clone(),
                typ: a.typ.scope_refs(scope),
            }));
        let mut cres: SmallVec<[(TVar, Type); 4]> = smallvec![];
        for (tv, tc) in self.constraints.read().iter() {
            let tv = tv.scope_refs(scope);
            let tc = tc.scope_refs(scope);
            cres.push((tv, tc));
        }
        let throws = self.throws.scope_refs(scope);
        FnType {
            args,
            rtype,
            constraints: Arc::new(RwLock::new(cres.into_iter().collect())),
            vargs,
            throws,
            explicit_throws: self.explicit_throws,
        }
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let constraints = self.constraints.read();
        if constraints.len() == 0 {
            write!(f, "fn(")?;
        } else {
            write!(f, "fn<")?;
            for (i, (tv, t)) in constraints.iter().enumerate() {
                write!(f, "{tv}: {t}")?;
                if i < constraints.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">(")?;
        }
        for (i, a) in self.args.iter().enumerate() {
            match &a.label {
                Some((l, true)) => write!(f, "?#{l}: ")?,
                Some((l, false)) => write!(f, "#{l}: ")?,
                None => (),
            }
            write!(f, "{}", a.typ)?;
            if i < self.args.len() - 1 || self.vargs.is_some() {
                write!(f, ", ")?;
            }
        }
        if let Some(vargs) = &self.vargs {
            write!(f, "@args: {}", vargs)?;
        }
        match &self.rtype {
            Type::Fn(ft) => write!(f, ") -> ({ft})")?,
            Type::ByRef(t) => match &**t {
                Type::Fn(ft) => write!(f, ") -> &({ft})")?,
                t => write!(f, ") -> &{t}")?,
            },
            t => write!(f, ") -> {t}")?,
        }
        match &self.throws {
            Type::Bottom => Ok(()),
            Type::TVar(tv) if *tv.read().typ.read() == Some(Type::Bottom) => Ok(()),
            t => write!(f, " throws {t}"),
        }
    }
}

impl PrettyDisplay for FnType {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let constraints = self.constraints.read();
        if constraints.is_empty() {
            writeln!(buf, "fn(")?;
        } else {
            writeln!(buf, "fn<")?;
            buf.with_indent(2, |buf| {
                for (i, (tv, t)) in constraints.iter().enumerate() {
                    write!(buf, "{tv}: ")?;
                    buf.with_indent(2, |buf| t.fmt_pretty(buf))?;
                    if i < constraints.len() - 1 {
                        buf.kill_newline();
                        writeln!(buf, ",")?;
                    }
                }
                Ok(())
            })?;
            writeln!(buf, ">(")?;
        }
        buf.with_indent(2, |buf| {
            for (i, a) in self.args.iter().enumerate() {
                match &a.label {
                    Some((l, true)) => write!(buf, "?#{l}: ")?,
                    Some((l, false)) => write!(buf, "#{l}: ")?,
                    None => (),
                }
                buf.with_indent(2, |buf| a.typ.fmt_pretty(buf))?;
                if i < self.args.len() - 1 || self.vargs.is_some() {
                    buf.kill_newline();
                    writeln!(buf, ",")?;
                }
            }
            if let Some(vargs) = &self.vargs {
                write!(buf, "@args: ")?;
                buf.with_indent(2, |buf| vargs.fmt_pretty(buf))?;
            }
            Ok(())
        })?;
        match &self.rtype {
            Type::Fn(ft) => {
                write!(buf, ") -> (")?;
                ft.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ")")?;
            }
            Type::ByRef(t) => match &**t {
                Type::Fn(ft) => {
                    write!(buf, ") -> &(")?;
                    ft.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ")")?;
                }
                t => {
                    write!(buf, ") -> &")?;
                    t.fmt_pretty(buf)?;
                }
            },
            t => {
                write!(buf, ") -> ")?;
                t.fmt_pretty(buf)?;
            }
        }
        match &self.throws {
            Type::Bottom if !self.explicit_throws => Ok(()),
            Type::TVar(tv)
                if *tv.read().typ.read() == Some(Type::Bottom)
                    && !self.explicit_throws =>
            {
                Ok(())
            }
            t => {
                buf.kill_newline();
                write!(buf, " throws ")?;
                t.fmt_pretty(buf)
            }
        }
    }
}
