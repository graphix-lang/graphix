use super::{Expr, ModPath, Name, WrittenAt, parser, print::Literal};
use crate::{env::Env, node::MAX_ALIAS_DEPTH, print_as_written, typ::Type};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use netidx_derive::Pack;
use netidx_value::{Typ, Value};
use smallvec::{SmallVec, smallvec};
use std::fmt;
use triomphe::Arc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Pack)]
#[pack(unwrapped)]
pub enum StructurePattern {
    Ignore,
    Literal(Value),
    Bind(Name),
    Slice {
        /// true = a list pattern `[<..>]` over the native List; false
        /// = an array slice.
        list: bool,
        all: Option<Name>,
        binds: Arc<[StructurePattern]>,
    },
    SlicePrefix {
        /// true = `[<h, rest..>]` — `tail` binds the tail as a List, O(1).
        list: bool,
        all: Option<Name>,
        prefix: Arc<[StructurePattern]>,
        tail: Option<Name>,
    },
    SliceSuffix {
        all: Option<Name>,
        head: Option<Name>,
        suffix: Arc<[StructurePattern]>,
    },
    Tuple {
        all: Option<Name>,
        binds: Arc<[StructurePattern]>,
    },
    Variant {
        all: Option<Name>,
        tag: ArcStr,
        binds: Arc<[StructurePattern]>,
    },
    /// `T(p)` — destructure a value of the abstract type at `name`
    /// into its payload
    Abstract {
        all: Option<Name>,
        name: ModPath,
        bind: Arc<StructurePattern>,
    },
    Struct {
        exhaustive: bool,
        all: Option<Name>,
        binds: Arc<[(ArcStr, StructurePattern, WrittenAt)]>,
    },
    /// Or-alternatives `p1 | p2 | …`. Flat: ≥ 2 alternatives, none itself
    /// an `Or`. Every alternative binds the same names at the same types,
    /// so name-set walks read alternative 0.
    Or(Arc<[StructurePattern]>),
}

impl StructurePattern {
    pub fn single_bind(&self) -> Option<&ArcStr> {
        match self {
            Self::Bind(s) => Some(s),
            Self::Ignore
            | Self::Literal(_)
            | Self::Slice { .. }
            | Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. }
            | Self::Tuple { .. }
            | Self::Struct { .. }
            | Self::Variant { .. }
            | Self::Abstract { .. }
            | Self::Or(_) => None,
        }
    }

    pub fn with_names<'a>(&'a self, f: &mut impl FnMut(&'a ArcStr)) {
        crate::stack::ensure_sufficient(|| self.with_names_inner(f))
    }

    /// The `name@` capture of this pattern.
    fn all(&self) -> Option<&Name> {
        match self {
            Self::Slice { all, .. }
            | Self::SlicePrefix { all, .. }
            | Self::SliceSuffix { all, .. }
            | Self::Tuple { all, .. }
            | Self::Variant { all, .. }
            | Self::Abstract { all, .. }
            | Self::Struct { all, .. } => all.as_ref(),
            Self::Ignore | Self::Literal(_) | Self::Bind(_) | Self::Or(_) => None,
        }
    }

    fn with_names_inner<'a>(&'a self, f: &mut impl FnMut(&'a ArcStr)) {
        if let Some(n) = self.all() {
            f(n)
        }
        let (rest, subs): (Option<&Name>, &[Self]) = match self {
            Self::Bind(n) => return f(n),
            Self::Ignore | Self::Literal(_) => return,
            Self::Abstract { bind, .. } => return bind.with_names(f),
            Self::Or(alts) => return alts[0].with_names(f),
            Self::Struct { binds, .. } => {
                return binds.iter().for_each(|(_, t, _)| t.with_names(f));
            }
            Self::Slice { binds, .. }
            | Self::Tuple { binds, .. }
            | Self::Variant { binds, .. } => (None, binds),
            Self::SlicePrefix { prefix, tail, .. } => (tail.as_ref(), prefix),
            Self::SliceSuffix { head, suffix, .. } => (head.as_ref(), suffix),
        };
        if let Some(n) = rest {
            f(n)
        }
        subs.iter().for_each(|t| t.with_names(f))
    }

    pub fn binds_uniq(&self) -> bool {
        let mut names: SmallVec<[&ArcStr; 16]> = smallvec![];
        self.with_names(&mut |s| names.push(s));
        names.sort();
        let len = names.len();
        names.dedup();
        names.len() == len
    }

    pub fn infer_type_predicate(&self, env: &Env, scope: &ModPath) -> Result<Type> {
        crate::stack::ensure_sufficient(|| self.infer_type_predicate_inner(env, scope))
    }

    fn infer_type_predicate_inner(&self, env: &Env, scope: &ModPath) -> Result<Type> {
        match self {
            // `_` must match everything for exhaustiveness and dispatch;
            // select unifies through `Type::any_as_tvar` to narrow past it.
            Self::Ignore => Ok(Type::Any),
            Self::Bind(_) => Ok(Type::empty_tvar()),
            Self::Literal(v) => Ok(Type::Primitive(Typ::get(v).into())),
            Self::Tuple { all: _, binds } => {
                let a = binds
                    .iter()
                    .map(|p| p.infer_type_predicate(env, scope))
                    .collect::<Result<SmallVec<[_; 8]>>>()?;
                Ok(Type::Tuple(Arc::from_iter(a)))
            }
            Self::Variant { all: _, tag, binds } => {
                let a = binds
                    .iter()
                    .map(|p| p.infer_type_predicate(env, scope))
                    .collect::<Result<SmallVec<[_; 8]>>>()?;
                Ok(Type::Variant(tag.clone(), Arc::from_iter(a), WrittenAt::NOWHERE))
            }
            Self::Abstract { all: _, name, bind: _ } => {
                let td = env
                    .lookup_typedef(scope, name)?
                    .ok_or_else(|| anyhow!("unknown type {name}"))?;
                let Type::Abstract { id, params } = &td.typ else {
                    bail!("{name} is not an abstract type, so it has no constructor")
                };
                let params = Arc::from_iter(params.iter().map(|_| Type::empty_tvar()));
                Ok(Type::Abstract { id: *id, params })
            }
            Self::Slice { list, all: _, binds }
            | Self::SlicePrefix { list, all: _, prefix: binds, tail: _ } => {
                Self::infer_slice(env, scope, *list, binds)
            }
            Self::SliceSuffix { all: _, head: _, suffix } => {
                Self::infer_slice(env, scope, false, suffix)
            }
            Self::Struct { all: _, exhaustive: _, binds } => {
                let mut typs = binds
                    .iter()
                    .map(|(n, p, _)| {
                        let t = p.infer_type_predicate(env, scope)?;
                        Ok((n.clone(), t, WrittenAt::NOWHERE))
                    })
                    .collect::<Result<SmallVec<[(ArcStr, Type, WrittenAt); 8]>>>()?;
                typs.sort_by(|a, b| a.0.cmp(&b.0));
                Ok(Type::Struct(Arc::from_iter(typs.into_iter())))
            }
            Self::Or(alts) => {
                // The uncollapsed Set keeps one member per alternative so
                // `complete_type_predicate` can zip them.
                let a = alts
                    .iter()
                    .map(|p| p.infer_type_predicate(env, scope))
                    .collect::<Result<SmallVec<[_; 8]>>>()?;
                Ok(Type::Set(Arc::from_iter(a)))
            }
        }
    }

    /// The inferred type of a slice pattern over its element patterns.
    fn infer_slice(
        env: &Env,
        scope: &ModPath,
        list: bool,
        elems: &[Self],
    ) -> Result<Type> {
        let mut ts: SmallVec<[Type; 8]> = smallvec![Type::Bottom];
        for p in elems {
            ts.push(p.infer_type_predicate(env, scope)?);
        }
        let t = match Type::union(env, &ts.iter().collect::<SmallVec<[_; 8]>>())? {
            Type::Bottom => Type::empty_tvar(),
            t => t,
        };
        Ok(slice_type(list, t))
    }

    /// Complete a partial struct pattern's inferred type (`{x, ..}` infers
    /// `{x: 'a}`) against the scrutinee: the member the pattern can match,
    /// with the named fields replaced by the pattern's. Recurses through
    /// composites. `None` if unchanged; an error when the pattern can
    /// match more than one member.
    pub fn complete_type_predicate(
        &self,
        env: &Env,
        ptype: &Type,
        scrutinee: &Type,
    ) -> Result<Option<Type>> {
        crate::stack::ensure_sufficient(|| {
            self.complete_type_predicate_inner(env, ptype, scrutinee)
        })
    }

    /// `pt` completed against the scrutinee position `st`, and whether
    /// that changed it.
    fn complete_sub(&self, env: &Env, pt: &Type, st: &Type) -> Result<(bool, Type)> {
        Ok(match self.complete_type_predicate(env, pt, st)? {
            Some(t) => (true, t),
            None => (false, pt.clone()),
        })
    }

    /// Complete element patterns position by position.
    fn complete_elems(
        env: &Env,
        elems: &[Self],
        pts: &[Type],
        sts: &[Type],
    ) -> Result<Option<Arc<[Type]>>> {
        let mut changed = false;
        let mut out: SmallVec<[Type; 8]> = SmallVec::new();
        for ((p, pt), st) in elems.iter().zip(pts).zip(sts) {
            let (c, t) = p.complete_sub(env, pt, st)?;
            changed |= c;
            out.push(t)
        }
        Ok(changed.then(|| Arc::from_iter(out)))
    }

    /// The completion of the one scrutinee member `complete` answers
    /// for and the pattern could match.
    fn unique_completion(
        &self,
        env: &Env,
        scrutinee: &Type,
        mut complete: impl FnMut(&Type) -> Result<Option<Type>>,
    ) -> Result<Option<Type>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        union_members(env, scrutinee, &mut members)?;
        let mut found: Option<Type> = None;
        for m in members.iter() {
            let Some(t) = complete(m)? else { continue };
            if !t.could_match(env, m)? {
                continue;
            }
            match &found {
                None => found = Some(t),
                Some(f) if *f == t => (),
                Some(_) => bail!(
                    "the pattern {self} matches more than one member of {scrutinee}; \
                     annotate the member you mean, e.g. `T as {self}`"
                ),
            }
        }
        Ok(found)
    }

    fn complete_type_predicate_inner(
        &self,
        env: &Env,
        ptype: &Type,
        scrutinee: &Type,
    ) -> Result<Option<Type>> {
        match self {
            Self::Struct { all: _, exhaustive, binds } => {
                let Type::Struct(pfields) = ptype else { return Ok(None) };
                self.unique_completion(env, scrutinee, |m| {
                    let Type::Struct(sf) = m else { return Ok(None) };
                    let named = |n: &ArcStr| binds.iter().find(|(bn, _, _)| bn == n);
                    let fits = if *exhaustive {
                        sf.len() == binds.len()
                            && sf.iter().all(|(n, _, _)| named(n).is_some())
                    } else {
                        binds.iter().all(|(n, _, _)| sf.iter().any(|(sn, _, _)| sn == n))
                    };
                    if !fits {
                        return Ok(None);
                    }
                    let mut changed = !*exhaustive;
                    let mut fields: SmallVec<[(ArcStr, Type, WrittenAt); 8]> =
                        SmallVec::new();
                    for (sn, st, at) in sf.iter() {
                        let t = match named(sn) {
                            None => st.clone(),
                            Some((_, p, _)) => {
                                let pt = pfields
                                    .iter()
                                    .find(|(pn, _, _)| pn == sn)
                                    .map(|(_, pt, _)| pt)
                                    .expect("inferred field missing");
                                let (c, t) = p.complete_sub(env, pt, st)?;
                                changed |= c;
                                t
                            }
                        };
                        fields.push((sn.clone(), t, *at))
                    }
                    Ok(changed.then(|| Type::Struct(Arc::from_iter(fields))))
                })
            }
            Self::Tuple { all: _, binds } => {
                let Type::Tuple(pts) = ptype else { return Ok(None) };
                self.unique_completion(env, scrutinee, |m| match m {
                    Type::Tuple(sts) if sts.len() == binds.len() => {
                        Ok(Self::complete_elems(env, binds, pts, sts)?.map(Type::Tuple))
                    }
                    _ => Ok(None),
                })
            }
            Self::Variant { all: _, tag, binds } => {
                let Type::Variant(_, pts, at) = ptype else { return Ok(None) };
                self.unique_completion(env, scrutinee, |m| match m {
                    Type::Variant(t, sts, _) if t == tag && sts.len() == binds.len() => {
                        let ts = Self::complete_elems(env, binds, pts, sts)?;
                        Ok(ts.map(|ts| Type::Variant(tag.clone(), ts, *at)))
                    }
                    _ => Ok(None),
                })
            }
            Self::Slice { list, all: _, binds }
            | Self::SlicePrefix { list, all: _, prefix: binds, tail: _ } => {
                self.complete_slice(env, ptype, scrutinee, *list, binds)
            }
            Self::SliceSuffix { all: _, head: _, suffix } => {
                self.complete_slice(env, ptype, scrutinee, false, suffix)
            }
            Self::Or(alts) => {
                let Type::Set(pts) = ptype else { return Ok(None) };
                if pts.len() != alts.len() {
                    return Ok(None);
                }
                let mut changed = false;
                let mut out: SmallVec<[Type; 8]> = SmallVec::new();
                for (p, pt) in alts.iter().zip(pts.iter()) {
                    let (c, t) = p.complete_sub(env, pt, scrutinee)?;
                    changed |= c;
                    out.push(t)
                }
                Ok(changed.then(|| Type::Set(Arc::from_iter(out))))
            }
            Self::Abstract { .. } | Self::Ignore | Self::Bind(_) | Self::Literal(_) => {
                Ok(None)
            }
        }
    }

    /// A slice pattern's completion: the element type is the union of
    /// the element patterns' completions.
    fn complete_slice(
        &self,
        env: &Env,
        ptype: &Type,
        scrutinee: &Type,
        list: bool,
        elems: &[Self],
    ) -> Result<Option<Type>> {
        let elem = |t: &Type| match (list, t) {
            (true, Type::List(t)) | (false, Type::Array(t)) => Some(t.clone()),
            _ => None,
        };
        let Some(pt) = elem(ptype) else { return Ok(None) };
        self.unique_completion(env, scrutinee, |m| {
            let Some(st) = elem(m) else { return Ok(None) };
            let mut changed = false;
            let mut ts: SmallVec<[Type; 8]> = smallvec![Type::Bottom];
            for p in elems {
                let (c, t) = p.complete_sub(env, &pt, &st)?;
                changed |= c;
                ts.push(t)
            }
            if !changed {
                return Ok(None);
            }
            let t = Type::union(env, &ts.iter().collect::<SmallVec<[_; 8]>>())?;
            Ok(Some(slice_type(list, t)))
        })
    }
}

fn slice_type(list: bool, elem: Type) -> Type {
    if list { Type::List(Arc::new(elem)) } else { Type::Array(Arc::new(elem)) }
}

/// The concrete members of `t`: bound tvars dereferenced, aliases
/// expanded, unions flattened. An unbound tvar contributes none, and so
/// does an alias chain deeper than [`MAX_ALIAS_DEPTH`] (a cyclic typedef).
pub(crate) fn union_members(
    env: &Env,
    t: &Type,
    out: &mut SmallVec<[Type; 8]>,
) -> Result<()> {
    fn walk(
        env: &Env,
        t: &Type,
        depth: usize,
        out: &mut SmallVec<[Type; 8]>,
    ) -> Result<()> {
        if depth > MAX_ALIAS_DEPTH {
            return Ok(());
        }
        match t.deref_cloned() {
            None => Ok(()),
            Some(Type::Set(ts)) => {
                ts.iter().try_for_each(|t| walk(env, t, depth + 1, out))
            }
            Some(t @ Type::Ref(_)) => walk(env, &t.lookup_ref(env)?, depth + 1, out),
            Some(t) => {
                out.push(t);
                Ok(())
            }
        }
    }
    walk(env, t, 0, out)
}

// XCR claude for eric: a pattern PrettyDisplay is a printer feature (expr/print.rs,
// parse-print's), with layout rules to rule on: where a struct pattern breaks,
// whether a type predicate breaks with it. Recommend: struct/tuple/slice patterns
// break one field per line like their literals; left to the printer's owner.
impl fmt::Display for StructurePattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        crate::stack::ensure_sufficient(|| self.fmt_inner(f))
    }
}

impl StructurePattern {
    fn fmt_inner(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! with_sep {
            ($binds:expr) => {
                for (i, b) in $binds.iter().enumerate() {
                    write!(f, "{b}")?;
                    if i < $binds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
            };
        }
        if let Some(all) = self.all() {
            write!(f, "{all}@ ")?
        }
        match self {
            StructurePattern::Ignore => write!(f, "_"),
            StructurePattern::Literal(v) => write!(f, "{}", Literal(v)),
            StructurePattern::Bind(n) => write!(f, "{n}"),
            StructurePattern::Slice { list, all: _, binds } => {
                write!(f, "{}", if *list { "[<" } else { "[" })?;
                with_sep!(binds);
                write!(f, "{}", if *list { ">]" } else { "]" })
            }
            StructurePattern::SlicePrefix { list, all: _, prefix, tail } => {
                write!(f, "{}", if *list { "[<" } else { "[" })?;
                for b in prefix.iter() {
                    write!(f, "{b}, ")?
                }
                let close = if *list { ">]" } else { "]" };
                match tail {
                    None => write!(f, "..{close}"),
                    Some(name) => write!(f, "{name}..{close}"),
                }
            }
            StructurePattern::SliceSuffix { all: _, head, suffix } => {
                write!(f, "[")?;
                match head {
                    None => write!(f, ".., ")?,
                    Some(name) => write!(f, "{name}.., ")?,
                }
                with_sep!(suffix);
                write!(f, "]")
            }
            StructurePattern::Tuple { all: _, binds } => {
                write!(f, "(")?;
                with_sep!(binds);
                write!(f, ")")
            }
            StructurePattern::Variant { all: _, tag, binds } if binds.is_empty() => {
                write!(f, "`{tag}")
            }
            StructurePattern::Variant { all: _, tag, binds } => {
                write!(f, "`{tag}(")?;
                with_sep!(binds);
                write!(f, ")")
            }
            StructurePattern::Abstract { all: _, name, bind } => {
                write!(f, "{name}({bind})")
            }
            StructurePattern::Or(alts) => {
                for (i, p) in alts.iter().enumerate() {
                    write!(f, "{p}")?;
                    if i < alts.len() - 1 {
                        write!(f, " | ")?
                    }
                }
                Ok(())
            }
            StructurePattern::Struct { exhaustive, all: _, binds } => {
                let mut written: SmallVec<[_; 16]> = binds.iter().collect();
                if print_as_written() {
                    written.sort_by_key(|(_, _, at)| at.order());
                }
                write!(f, "{{ ")?;
                for (i, (name, pat, _)) in written.iter().enumerate() {
                    match pat {
                        StructurePattern::Bind(n)
                            if n.name == *name
                                && !parser::RESERVED_BINDING.contains(&name.as_str()) =>
                        {
                            write!(f, "{name}")?
                        }
                        pat => write!(f, "{name}: {pat}")?,
                    }
                    if !exhaustive || i < binds.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                if !exhaustive {
                    write!(f, "..")?
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Pack)]
#[pack(unwrapped)]
pub struct Pattern {
    pub type_predicate: Option<Type>,
    pub structure_predicate: StructurePattern,
    pub guard: Option<Expr>,
}
