use super::{Expr, ModPath, Name, WrittenAt, WrittenForm, parser, print::Literal};
use crate::{
    PrintFlag,
    env::Env,
    format_with_flags, print_as_written,
    typ::{
        AbstractId, TVar, Type, Unfolded, settled_identical, strictly_identical,
        tvar::Level,
    },
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_derive::Pack;
use netidx_value::Value;
use poolshark::local::LPooled;
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
        /// where `name` was written
        at: WrittenAt,
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
        self.with_bound_names(&mut |n| f(&n.name))
    }

    /// Each name the pattern binds, with where it was written.
    pub fn with_bound_names<'a>(&'a self, f: &mut impl FnMut(&'a Name)) {
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

    fn with_names_inner<'a>(&'a self, f: &mut impl FnMut(&'a Name)) {
        if let Some(n) = self.all() {
            f(n)
        }
        let (rest, subs): (Option<&Name>, &[Self]) = match self {
            Self::Bind(n) => return f(n),
            Self::Ignore | Self::Literal(..) => return,
            Self::Abstract { bind, .. } => return bind.with_bound_names(f),
            Self::Or(alts) => return alts[0].with_bound_names(f),
            Self::Struct { binds, .. } => {
                return binds.iter().for_each(|(_, t, _)| t.with_bound_names(f));
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
        subs.iter().for_each(|t| t.with_bound_names(f))
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
            Self::Abstract { name, .. } => {
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
    /// match more than one member. Members told apart only by cells not
    /// decided yet complete alike, so they count as one: each such pair
    /// goes to `undecided`, for the check's end to judge
    /// ([`Undecided::judge`]).
    pub fn complete_type_predicate(
        &self,
        env: &Env,
        ptype: &Type,
        scrutinee: &Type,
        undecided: &mut Vec<Undecided>,
    ) -> Result<Option<Type>> {
        crate::stack::ensure_sufficient(|| {
            self.complete_type_predicate_inner(env, ptype, scrutinee, undecided)
        })
    }

    /// `pt` completed against the scrutinee position `st`, and whether
    /// that changed it.
    fn complete_at(
        &self,
        env: &Env,
        pt: &Type,
        st: &Type,
        undecided: &mut Vec<Undecided>,
    ) -> Result<(bool, Type)> {
        Ok(match self.complete_type_predicate(env, pt, st, undecided)? {
            Some(t) => (true, t),
            None => (false, pt.clone()),
        })
    }

    /// [`Self::complete_at`] a position inside the scrutinee's members,
    /// whose values reach the arm as their member's do.
    fn complete_sub(
        &self,
        env: &Env,
        pt: &Type,
        st: &Type,
        undecided: &mut Vec<Undecided>,
    ) -> Result<(bool, Type)> {
        let noted = undecided.len();
        let r = self.complete_at(env, pt, st, undecided)?;
        for u in &mut undecided[noted..] {
            u.member = None
        }
        Ok(r)
    }

    /// Complete element patterns position by position.
    fn complete_elems(
        env: &Env,
        elems: &[Self],
        pts: &[Type],
        sts: &[Type],
        undecided: &mut Vec<Undecided>,
    ) -> Result<Option<Arc<[Type]>>> {
        let mut changed = false;
        let mut out: SmallVec<[Type; 8]> = SmallVec::new();
        for ((p, pt), st) in elems.iter().zip(pts).zip(sts) {
            let (c, t) = p.complete_sub(env, pt, st, undecided)?;
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
        undecided: &mut Vec<Undecided>,
        mut complete: impl FnMut(&Type, &mut Vec<Undecided>) -> Result<Option<Type>>,
    ) -> Result<Option<Type>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        // an open scrutinee completes against its bound, whose values it holds
        match scrutinee.open_bound(env) {
            Some(bound) => union_members(env, &bound, &mut members)?,
            None => union_members(env, scrutinee, &mut members)?,
        }
        let mut found: Option<Type> = None;
        for m in members.iter() {
            let noted = undecided.len();
            let t = match complete(m, undecided)? {
                Some(t) if t.could_match(env, m)? => t,
                _ => {
                    undecided.truncate(noted);
                    continue;
                }
            };
            match &found {
                None => found = Some(t),
                Some(f) if *f == t => {
                    if !strictly_identical(f, &t) {
                        undecided.push(Undecided {
                            pattern: self.clone(),
                            scrutinee: scrutinee.clone(),
                            first: f.clone(),
                            other: t,
                            member: Some(m.clone()),
                        })
                    }
                }
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
        undecided: &mut Vec<Undecided>,
    ) -> Result<Option<Type>> {
        match self {
            Self::Struct { all: _, exhaustive, binds } => {
                let Type::Struct(pfields) = ptype else { return Ok(None) };
                self.unique_completion(env, scrutinee, undecided, |m, undecided| {
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
                                let (c, t) = p.complete_sub(env, pt, st, undecided)?;
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
                self.unique_completion(
                    env,
                    scrutinee,
                    undecided,
                    |m, undecided| match m {
                        Type::Tuple(sts) if sts.len() == binds.len() => {
                            Ok(Self::complete_elems(env, binds, pts, sts, undecided)?
                                .map(Type::Tuple))
                        }
                        _ => Ok(None),
                    },
                )
            }
            Self::Variant { all: _, tag, binds } => {
                let Type::Variant(_, pts, at) = ptype else { return Ok(None) };
                self.unique_completion(
                    env,
                    scrutinee,
                    undecided,
                    |m, undecided| match m {
                        Type::Variant(t, sts, _)
                            if t == tag && sts.len() == binds.len() =>
                        {
                            let ts =
                                Self::complete_elems(env, binds, pts, sts, undecided)?;
                            Ok(ts.map(|ts| Type::Variant(tag.clone(), ts, *at)))
                        }
                        _ => Ok(None),
                    },
                )
            }
            Self::Slice { list, all: _, binds }
            | Self::SlicePrefix { list, all: _, prefix: binds, tail: _ } => {
                self.complete_slice(env, ptype, scrutinee, *list, binds, undecided)
            }
            Self::SliceSuffix { all: _, head: _, suffix } => {
                self.complete_slice(env, ptype, scrutinee, false, suffix, undecided)
            }
            Self::Or(alts) => {
                let Type::Set(pts) = ptype else { return Ok(None) };
                if pts.len() != alts.len() {
                    return Ok(None);
                }
                let mut changed = false;
                let mut out: SmallVec<[Type; 8]> = SmallVec::new();
                for (p, pt) in alts.iter().zip(pts.iter()) {
                    let (c, t) = p.complete_at(env, pt, scrutinee, undecided)?;
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
        undecided: &mut Vec<Undecided>,
    ) -> Result<Option<Type>> {
        let elem = |t: &Type| match (list, t) {
            (true, Type::List(t)) | (false, Type::Array(t)) => Some(t.clone()),
            _ => None,
        };
        let pts = match elem(ptype).as_deref() {
            Some(Type::Set(pts)) if pts.len() >= elems.len() => pts.clone(),
            _ => return Ok(None),
        };
        self.unique_completion(env, scrutinee, undecided, |m, undecided| {
            let Some(st) = elem(m) else { return Ok(None) };
            let mut changed = false;
            let mut ts: SmallVec<[Type; 8]> = SmallVec::new();
            for (i, pt) in pts.iter().enumerate() {
                match elems.get(i) {
                    None => ts.push(pt.clone()),
                    Some(p) => {
                        let (c, t) = p.complete_sub(env, pt, &st, undecided)?;
                        changed |= c;
                        ts.push(t)
                    }
                }
            }
            Ok(changed.then(|| slice_type(list, Type::Set(Arc::from_iter(ts)))))
        })
    }
}

/// Two completions of a partial struct pattern that `==` took for one
/// member of `scrutinee` while cells not decided yet told them apart.
#[derive(Debug)]
pub struct Undecided {
    pattern: StructurePattern,
    scrutinee: Type,
    first: Type,
    other: Type,
    /// The member `other` completes, when it is one of the arm's
    /// scrutinee's, whose values an earlier arm may take.
    member: Option<Type>,
}

impl Undecided {
    /// Refused unless the two completions are one type once the check has
    /// decided what it will ([`settled_identical`]) or an earlier arm,
    /// covering `covered`, takes every value of the other member. `input` says whether a
    /// caller of an enclosing definition can hand an unbound cell a value
    /// ([`crate::typ::FnType::input_cells`]), every cell outside any
    /// definition; no other cell of theirs holds one. It reads the cells
    /// and decides none.
    pub fn judge(
        &self,
        env: &Env,
        covered: &[Type],
        input: &dyn Fn(&TVar) -> bool,
    ) -> Result<()> {
        if settled_identical(&self.first, &self.other, input)
            || self.taken(env, covered)?
        {
            return Ok(());
        }
        let Self { pattern, scrutinee, .. } = self;
        let apart = match self.open_in_a_definition() {
            true => "whose cells its calls may decide apart",
            false => "once its cells are decided",
        };
        format_with_flags(PrintFlag::DerefTVars, || {
            bail!(
                "the pattern {pattern} matches more than one member of {scrutinee} \
                 {apart}; annotate the member you mean, e.g. `T as {pattern}`, or name \
                 every field"
            )
        })
    }

    /// Whether a completion still holds a definition's open cell.
    fn open_in_a_definition(&self) -> bool {
        let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        self.first.reached_cells(&mut cells);
        self.other.reached_cells(&mut cells);
        cells.values().any(|tv| !tv.is_bound() && tv.level() != Level::TOP)
    }

    /// Whether one of `covered` holds the other member, both closed.
    fn taken(&self, env: &Env, covered: &[Type]) -> Result<bool> {
        let Some(m) = self.member.as_ref().filter(|m| !m.has_unbound()) else {
            return Ok(false);
        };
        for c in covered {
            if !c.has_unbound() && c.contains_with_flags(BitFlags::empty(), env, m)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

/// The abstract type each abstract pattern of a pattern names, as the
/// pattern's compile resolved it.
pub type AbstractOf<'a> = dyn Fn(&StructurePattern) -> Option<AbstractId> + 'a;

impl StructurePattern {
    /// [`AbstractOf`] by a lookup of each name at `scope`, which agrees
    /// with a compile at `scope` only while the env is the compile's.
    pub fn abstracts_at<'a>(
        env: &'a Env,
        scope: &'a ModPath,
    ) -> impl Fn(&StructurePattern) -> Option<AbstractId> + 'a {
        move |p| {
            let Self::Abstract { name, .. } = p else { return None };
            match env.lookup_typedef(scope, name).ok().flatten()?.typ() {
                Type::Abstract { id, .. } => Some(*id),
                _ => None,
            }
        }
    }

    /// The first type variable a definition holds rigid whose value this
    /// pattern, over a value of `typ`, tests or binds inside. Nothing
    /// types the parts of such a value, so only a name or `_` stands over
    /// it. At a union holding the variable beside other members, a part
    /// meets it where it may hold one of its values ([`Self::may_hold`]),
    /// inside a member the whole pattern may hold.
    pub fn meets_rigid(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
    ) -> Result<Option<TVar>> {
        Ok(self.meets(env, ids, typ, None)?.map(|(tv, _)| tv))
    }

    /// [`Self::meets_rigid`] under `written`, the written test over a value
    /// of `typ`: the first variable met where the test's part over it,
    /// returned with it, holds `_` or an open cell, so types nothing
    /// inside it.
    pub fn meets_rigid_written(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        written: &Type,
    ) -> Result<Option<(TVar, Type)>> {
        Ok(self
            .meets(env, ids, typ, Some(written))?
            .and_then(|(tv, w)| w.map(|w| (tv, w))))
    }

    fn meets(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        written: Option<&Type>,
    ) -> Result<Option<(TVar, Option<Type>)>> {
        crate::stack::ensure_sufficient(|| match self {
            Self::Ignore | Self::Bind(_) => Ok(None),
            Self::Or(alts) => {
                for a in alts.iter() {
                    if let Some(r) = a.meets(env, ids, typ, written)? {
                        return Ok(Some(r));
                    }
                }
                Ok(None)
            }
            _ => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let mut rigid: SmallVec<[TVar; 2]> = SmallVec::new();
                union_members_with(env, typ, &mut members, &mut |tv| {
                    if let Some(c) = tv.open_cell().filter(|c| c.is_rigid()) {
                        rigid.push(c)
                    }
                })?;
                for tv in rigid {
                    if members.is_empty() || self.may_hold_value_of(env, ids, &tv)? {
                        match written {
                            None => return Ok(Some((tv, None))),
                            Some(w) if w.has_free() || w.has_bottom() => {
                                return Ok(Some((tv, Some(w.clone()))));
                            }
                            Some(_) => (),
                        }
                    }
                }
                for m in members.iter() {
                    if !self.may_hold(env, ids, m)? {
                        continue;
                    }
                    let Some(parts) = self.parts_over(env, ids, m) else { continue };
                    let ws = match written {
                        None => None,
                        Some(w) => match self.written_parts(env, ids, w, parts.len())? {
                            Some(ws) => Some(ws),
                            None => continue,
                        },
                    };
                    for (i, (p, t)) in parts.iter().enumerate() {
                        let w = ws.as_ref().map(|ws| &ws[i]);
                        if let Some(r) = p.meets(env, ids, t, w)? {
                            return Ok(Some(r));
                        }
                    }
                }
                Ok(None)
            }
        })
    }

    /// The part of `written`, a written test, each of this pattern's `n`
    /// sub-patterns stands over: the union of its parts over each member
    /// of this pattern's shape; `written` itself for each where it holds
    /// an open cell or no member; `None` where no member is of the shape.
    fn written_parts(
        &self,
        env: &Env,
        ids: &AbstractOf,
        written: &Type,
        n: usize,
    ) -> Result<Option<SmallVec<[Type; 8]>>> {
        let mut members: SmallVec<[Type; 8]> = SmallVec::new();
        if union_members_open(env, written, &mut members)? || members.is_empty() {
            return Ok(Some((0..n).map(|_| written.clone()).collect()));
        }
        let mut parts: SmallVec<[SmallVec<[Type; 2]>; 8]> =
            (0..n).map(|_| SmallVec::new()).collect();
        let mut any = false;
        for m in members.iter() {
            let Some(ps) = self.parts_over(env, ids, m) else { continue };
            any = true;
            for (i, (_, t)) in ps.into_iter().enumerate() {
                parts[i].push(t)
            }
        }
        if !any {
            return Ok(None);
        }
        let parts = parts
            .into_iter()
            .map(|ts| match &ts[..] {
                [t] => Ok(t.clone()),
                ts => Type::union(env, &ts.iter().collect::<SmallVec<[&Type; 2]>>()),
            })
            .collect::<Result<_>>()?;
        Ok(Some(parts))
    }

    /// Whether a value of `typ` may pass this pattern: false only where no
    /// member of `typ` has the constructor, tag, arity or fields it tests,
    /// or the type of a literal it tests, here or below.
    pub fn may_hold(&self, env: &Env, ids: &AbstractOf, typ: &Type) -> Result<bool> {
        crate::stack::ensure_sufficient(|| match self {
            Self::Ignore | Self::Bind(_) => Ok(true),
            Self::Literal(v, _) => Type::of_literal(v)?.could_match(env, typ),
            Self::Or(alts) => {
                for a in alts.iter() {
                    if a.may_hold(env, ids, typ)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            _ => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                if union_members_open(env, typ, &mut members)? {
                    return Ok(true);
                }
                'members: for m in members.iter() {
                    if let Type::Any = m {
                        return Ok(true);
                    }
                    let Some(parts) = self.parts_over(env, ids, m) else { continue };
                    for (p, t) in parts {
                        if !p.may_hold(env, ids, &t)? {
                            continue 'members;
                        }
                    }
                    return Ok(true);
                }
                Ok(false)
            }
        })
    }

    /// Whether a value of `tv`, an open cell, may pass this pattern: a
    /// value of each of its typed bounds may, or it has none.
    fn may_hold_value_of(&self, env: &Env, ids: &AbstractOf, tv: &TVar) -> Result<bool> {
        for b in tv.typed_bounds(env).iter() {
            if !self.may_hold(env, ids, b)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Each sub-pattern with the part of `m`, a member of a type (a
    /// constructor at its head), it stands over; `None` where `m` is not
    /// of this pattern's shape. An abstract pattern holds the type `ids`
    /// gives it.
    fn parts_over<'a>(
        &'a self,
        env: &Env,
        ids: &AbstractOf,
        m: &Type,
    ) -> Option<SmallVec<[(&'a Self, Type); 8]>> {
        type Parts<'a> = SmallVec<[(&'a StructurePattern, Type); 8]>;
        fn each<'a>(ps: &'a [StructurePattern], ts: &[Type]) -> Option<Parts<'a>> {
            (ps.len() == ts.len()).then(|| ps.iter().zip(ts.iter().cloned()).collect())
        }
        fn elems<'a>(
            ps: &'a [StructurePattern],
            list: bool,
            m: &Type,
        ) -> Option<Parts<'a>> {
            let et = match (list, m) {
                (false, Type::Array(et)) | (true, Type::List(et)) => (**et).clone(),
                _ => return None,
            };
            Some(ps.iter().map(|p| (p, et.clone())).collect())
        }
        match (self, m) {
            (Self::Tuple { binds, .. }, Type::Tuple(ts)) => each(binds, ts),
            (Self::Variant { tag, binds, .. }, Type::Variant(t, ts, _)) if t == tag => {
                each(binds, ts)
            }
            (Self::Struct { exhaustive, binds, .. }, Type::Struct(fs))
                if !*exhaustive || fs.len() == binds.len() =>
            {
                binds
                    .iter()
                    .map(|(n, p, _)| {
                        fs.iter().find(|(f, _, _)| f == n).map(|(_, t, _)| (p, t.clone()))
                    })
                    .collect()
            }
            (Self::Slice { list, binds, .. }, _)
            | (Self::SlicePrefix { list, prefix: binds, .. }, _) => {
                elems(binds, *list, m)
            }
            (Self::SliceSuffix { suffix, .. }, _) => elems(suffix, false, m),
            (Self::Abstract { bind, .. }, Type::Abstract { id, params })
                if ids(self) == Some(*id) =>
            {
                let rep = env.abstract_reps.get(id)?;
                Some(smallvec![(&**bind, rep.instantiate_with(params))])
            }
            _ => None,
        }
    }
}

fn slice_type(list: bool, elem: Type) -> Type {
    if list { Type::List(Arc::new(elem)) } else { Type::Array(Arc::new(elem)) }
}

/// The concrete members of `t`: bound tvars dereferenced, aliases
/// expanded, unions flattened, in the order first met; an application met
/// again adds nothing, its members are in already. An unbound tvar
/// contributes none. An alias chain ends: a typedef recursing through
/// unions and aliases alone is refused at its definition.
#[doc(hidden)]
pub fn union_members(env: &Env, t: &Type, out: &mut SmallVec<[Type; 8]>) -> Result<()> {
    union_members_open(env, t, out).map(|_| ())
}

/// [`union_members`], and whether an unbound tvar was among them.
#[doc(hidden)]
pub fn union_members_open(
    env: &Env,
    t: &Type,
    out: &mut SmallVec<[Type; 8]>,
) -> Result<bool> {
    let mut open = false;
    union_members_with(env, t, out, &mut |_| open = true)?;
    Ok(open)
}

/// [`union_members`], handing each unbound tvar among them to `open`.
#[doc(hidden)]
pub fn union_members_with(
    env: &Env,
    t: &Type,
    out: &mut SmallVec<[Type; 8]>,
    open: &mut dyn FnMut(&TVar),
) -> Result<()> {
    fn go(
        env: &Env,
        t: &Type,
        out: &mut SmallVec<[Type; 8]>,
        unfolded: &mut Unfolded,
        open: &mut dyn FnMut(&TVar),
    ) -> Result<()> {
        crate::stack::ensure_sufficient(|| match t.deref_cloned() {
            None => {
                if let Type::TVar(tv) = t {
                    open(tv)
                }
                Ok(())
            }
            Some(t) => match &t {
                Type::Set(ts) => {
                    ts.iter().try_for_each(|t| go(env, t, out, unfolded, open))
                }
                Type::Ref(tr) => {
                    tr.resolve_in(env);
                    match unfolded.first(&t) {
                        true => go(env, &t.lookup_ref(env)?, out, unfolded, open),
                        false => Ok(()),
                    }
                }
                _ => {
                    out.push(t);
                    Ok(())
                }
            },
        })
    }
    go(env, t, out, &mut Unfolded::default(), open)
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
            StructurePattern::Abstract { name, bind, .. } => {
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
