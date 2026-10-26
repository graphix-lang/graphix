use super::{Expr, ModPath, Name, WrittenAt, WrittenForm, parser, print::Literal};
use crate::{env::Env, print_as_written, typ::Type};
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use netidx_derive::Pack;
use netidx_value::Value;
use smallvec::{SmallVec, smallvec};
use std::fmt;
use triomphe::Arc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Pack)]
#[pack(unwrapped)]
pub enum StructurePattern {
    Ignore,
    /// A literal, and for a string how it was delimited.
    Literal(Value, WrittenForm),
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
    /// an `Or`. Every alternative binds the same names (a shared name's
    /// type is the union of the alternatives'), so name-set walks read
    /// alternative 0.
    Or(Arc<[StructurePattern]>),
}

impl StructurePattern {
    /// A literal the compiler built.
    pub fn literal(v: Value) -> Self {
        Self::Literal(v, WrittenForm::default())
    }

    /// Each sub-pattern, in written order.
    pub fn for_each_sub<'a>(&'a self, f: &mut impl FnMut(&'a Self)) {
        match self {
            Self::Ignore | Self::Literal(..) | Self::Bind(_) => (),
            Self::Abstract { bind, .. } => f(bind),
            Self::Struct { binds, .. } => binds.iter().for_each(|(_, p, _)| f(p)),
            Self::Slice { binds: ps, .. }
            | Self::Tuple { binds: ps, .. }
            | Self::Variant { binds: ps, .. }
            | Self::SlicePrefix { prefix: ps, .. }
            | Self::SliceSuffix { suffix: ps, .. }
            | Self::Or(ps) => ps.iter().for_each(|p| f(p)),
        }
    }

    /// Whether a struct pattern that leaves fields out (`{x, ..}`) is
    /// here or in a sub-pattern.
    pub fn has_partial_struct(&self) -> bool {
        crate::stack::ensure_sufficient(|| {
            let mut partial = matches!(self, Self::Struct { exhaustive: false, .. });
            self.for_each_sub(&mut |p| partial |= p.has_partial_struct());
            partial
        })
    }

    pub fn single_bind(&self) -> Option<&ArcStr> {
        match self {
            Self::Bind(s) => Some(s),
            Self::Ignore
            | Self::Literal(..)
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
    pub(crate) fn all(&self) -> Option<&Name> {
        match self {
            Self::Slice { all, .. }
            | Self::SlicePrefix { all, .. }
            | Self::SliceSuffix { all, .. }
            | Self::Tuple { all, .. }
            | Self::Variant { all, .. }
            | Self::Abstract { all, .. }
            | Self::Struct { all, .. } => all.as_ref(),
            Self::Ignore | Self::Literal(..) | Self::Bind(_) | Self::Or(_) => None,
        }
    }

    fn with_names_inner<'a>(&'a self, f: &mut impl FnMut(&'a ArcStr)) {
        if let Some(n) = self.all() {
            f(n)
        }
        let (rest, subs): (Option<&Name>, &[Self]) = match self {
            Self::Bind(n) => return f(n),
            Self::Ignore | Self::Literal(..) => return,
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
            Self::Literal(v, _) => Type::of_literal(v),
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
                let Type::Abstract { id, params } = td.typ() else {
                    bail!("{name} is not an abstract type, so it has no constructor")
                };
                let params = Arc::from_iter(params.iter().map(|_| Type::empty_tvar()));
                Ok(Type::Abstract { id: *id, params })
            }
            Self::Slice { list, all: _, binds } => {
                Self::infer_slice(env, scope, *list, false, binds)
            }
            Self::SlicePrefix { list, all: _, prefix, tail: _ } => {
                Self::infer_slice(env, scope, *list, true, prefix)
            }
            Self::SliceSuffix { all: _, head: _, suffix } => {
                Self::infer_slice(env, scope, false, true, suffix)
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

    /// The inferred type of a slice pattern: its element type is a Set
    /// with one member per element pattern, in order, and with `rest` a
    /// cell last for the elements the rest stands over, so each element
    /// narrows against the scrutinee's element type on its own and the
    /// rest takes it whole.
    // XCR claude for claude: [bug/critical] Each element of a slice pattern is typed
    // apart here (one Set member per element), but PatternNode::shape_matches tests the
    // value against the whole Array<Set{..}>, that is, each element against the union
    // of the element types. Over an open or Any element type the uses bind the members
    // apart: `let f = |a| select a { [x, y] => str::len(x) + y, _ => 0 }; f([5,
    // "abc"])` passes the check, binds 5 as a string, and the fused run aborts on a
    // misaligned pointer dereference (UB in release). `[x, ..]` and `[<x, ..>]` over
    // Any admit anything through the open rest cell. Test each element against its own
    // position's type at run time, in both engines, as a tuple is, or type every
    // element bind as the whole element type. Pin the generic, Any and List forms.
    // probe:
    // design/review-2026-10-26/repro/x-typecheck-patterns-slice-elements-untested.gx
    // (x-typecheck-patterns-01)
    // 2026-10-26 claude: Each name a slice element binds now holds its part of every
    // member the element's structure admits (StructPatternNode::hold_slice_members,
    // called from PatternNode::bind_narrowed), so an open or Any element type no longer
    // lets uses type the elements apart; a literal's footprint is its own type so a
    // literal-discriminated element is not over-constrained. Pins:
    // lang::select::slice_element_binds_hold_every_member (Any, List and generic forms;
    // SIGSEGV with the call removed), slice_elements_share_an_open_type,
    // slice_nested_binds_hold_every_member. Still open: `[x, _]` over Any, whose `_`
    // member Any.narrow_pattern leaves open, closes with (c-pattern-03).
    fn infer_slice(
        env: &Env,
        scope: &ModPath,
        list: bool,
        rest: bool,
        elems: &[Self],
    ) -> Result<Type> {
        if elems.is_empty() {
            return Ok(slice_type(list, Type::empty_tvar()));
        }
        let mut ts: SmallVec<[Type; 8]> = SmallVec::new();
        for p in elems {
            ts.push(p.infer_type_predicate(env, scope)?);
        }
        if rest {
            ts.push(Type::empty_tvar())
        }
        Ok(slice_type(list, Type::Set(Arc::from_iter(ts))))
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
            Self::Abstract { .. } | Self::Ignore | Self::Bind(_) | Self::Literal(..) => {
                Ok(None)
            }
        }
    }

    /// A slice pattern's completion, member by member of its element
    /// Set.
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
        let pts = match elem(ptype).as_deref() {
            Some(Type::Set(pts)) if pts.len() >= elems.len() => pts.clone(),
            _ => return Ok(None),
        };
        self.unique_completion(env, scrutinee, |m| {
            let Some(st) = elem(m) else { return Ok(None) };
            let mut changed = false;
            let mut ts: SmallVec<[Type; 8]> = SmallVec::new();
            for (i, pt) in pts.iter().enumerate() {
                match elems.get(i) {
                    None => ts.push(pt.clone()),
                    Some(p) => {
                        let (c, t) = p.complete_sub(env, pt, &st)?;
                        changed |= c;
                        ts.push(t)
                    }
                }
            }
            Ok(changed.then(|| slice_type(list, Type::Set(Arc::from_iter(ts)))))
        })
    }
}

fn slice_type(list: bool, elem: Type) -> Type {
    if list { Type::List(Arc::new(elem)) } else { Type::Array(Arc::new(elem)) }
}

/// The concrete members of `t`: bound tvars dereferenced, aliases
/// expanded, unions flattened. An unbound tvar contributes none. An alias
/// chain ends: a typedef recursing through unions and aliases alone is
/// refused at its definition.
#[doc(hidden)]
pub fn union_members(env: &Env, t: &Type, out: &mut SmallVec<[Type; 8]>) -> Result<()> {
    crate::stack::ensure_sufficient(|| match t.deref_cloned() {
        None => Ok(()),
        Some(t) => match &t {
            Type::Set(ts) => ts.iter().try_for_each(|t| union_members(env, t, out)),
            Type::Ref(_) => union_members(env, &t.lookup_ref(env)?, out),
            _ => {
                out.push(t);
                Ok(())
            }
        },
    })
}

/// A struct pattern's fields in the order they print (as written when
/// printing as written), each with its pattern, or `None` for a pun.
pub(super) fn printed_fields(
    binds: &[(ArcStr, StructurePattern, WrittenAt)],
) -> SmallVec<[(&ArcStr, Option<&StructurePattern>); 16]> {
    let mut fields: SmallVec<[_; 16]> = binds
        .iter()
        .map(|(name, pat, at)| {
            let pun = matches!(pat, StructurePattern::Bind(n) if n.name == *name)
                && !parser::is_reserved_binding(name);
            (at, (name, (!pun).then_some(pat)))
        })
        .collect();
    if print_as_written() {
        fields.sort_by_key(|(at, _)| at.order());
    }
    fields.into_iter().map(|(_, f)| f).collect()
}

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
            StructurePattern::Literal(v @ Value::String(s), form)
                if print_as_written() =>
            {
                super::print::write_str_constant(f, v, s, form.0)
            }
            StructurePattern::Literal(v, _) => write!(f, "{}", Literal(v)),
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
                write!(f, "{{ ")?;
                for (i, (name, pat)) in printed_fields(binds).into_iter().enumerate() {
                    match pat {
                        None => write!(f, "{name}")?,
                        Some(pat) => write!(f, "{name}: {pat}")?,
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
    /// Where the pattern's text starts, if it was parsed (the guard
    /// excluded); decides nothing.
    pub pos: WrittenAt,
    /// Where it ends (exclusive), if it was parsed.
    pub end: WrittenAt,
}
