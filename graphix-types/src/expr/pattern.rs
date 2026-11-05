use super::{Expr, ModPath, Name, WrittenAt, WrittenForm, parser, print::Literal};
use crate::{
    PrintFlag,
    env::Env,
    format_with_flags, print_as_written,
    typ::{AbstractId, TVar, Type, Unfolded, strictly_identical},
};
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
    /// match more than one member, members told apart only by cells not
    /// decided yet included: each is one type, equal only to itself.
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
    fn complete_at(&self, env: &Env, pt: &Type, st: &Type) -> Result<(bool, Type)> {
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
            let (c, t) = p.complete_at(env, pt, st)?;
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
        // an open scrutinee completes against its bound, whose values it holds
        match scrutinee.open_bound(env) {
            Some(bound) => union_members(env, &bound, &mut members)?,
            None => union_members(env, scrutinee, &mut members)?,
        }
        let mut found: Option<(Type, &Type)> = None;
        for m in members.iter() {
            let t = match complete(m)? {
                Some(t) if t.could_match(env, m)? => t,
                _ => continue,
            };
            match &found {
                None => found = Some((t, m)),
                Some((f, _)) if strictly_identical(f, &t) => (),
                Some((f, first)) if *f == t => {
                    let members = [(*first).clone(), m.clone()];
                    return Err(
                        UndecidedMembers { pattern: self.clone(), members }.into()
                    );
                }
                Some(_) => format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "the pattern {self} matches more than one member of {scrutinee}; \
                         annotate the member you mean, e.g. `T as {self}`"
                    )
                })?,
            }
        }
        Ok(found.map(|(t, _)| t))
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
                                let (c, t) = p.complete_at(env, pt, st)?;
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
                    let (c, t) = p.complete_at(env, pt, scrutinee)?;
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
                        let (c, t) = p.complete_at(env, pt, &st)?;
                        changed |= c;
                        ts.push(t)
                    }
                }
            }
            Ok(changed.then(|| slice_type(list, Type::Set(Arc::from_iter(ts)))))
        })
    }
}

/// A partial struct pattern's two members told apart only by cells not
/// decided where it is completed.
#[derive(Debug)]
pub struct UndecidedMembers {
    pub pattern: StructurePattern,
    pub members: [Type; 2],
}

impl fmt::Display for UndecidedMembers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { pattern, members: [a, b] } = self;
        format_with_flags(PrintFlag::DerefTVars, || {
            write!(
                f,
                "the pattern {pattern} matches two members, {a} and {b}, told apart by \
                 types not decided at this select; annotate those types, or the member \
                 you mean, e.g. `T as {pattern}`, or name every field"
            )
        })
    }
}

impl std::error::Error for UndecidedMembers {}

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
        Ok(self.meets(env, ids, typ, None, Met::Rigid)?.map(|(tv, _)| tv))
    }

    /// The first open cell `undecided` admits, never one held rigid, whose
    /// value this pattern, over a value of `typ`, tests or binds inside,
    /// met as [`Self::meets_rigid`] meets a rigid one.
    pub fn meets_undecided(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        undecided: &dyn Fn(&TVar) -> bool,
    ) -> Result<Option<TVar>> {
        let met = Met::Undecided(undecided);
        Ok(self.meets(env, ids, typ, None, met)?.map(|(tv, _)| tv))
    }

    /// The first cell `decided` admits, read as still open though bound,
    /// whose value this pattern, over a value of `typ`, tests or binds
    /// inside, met as [`Self::meets_undecided`] meets an open one.
    pub fn meets_decided(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        decided: &Decided,
    ) -> Result<Option<TVar>> {
        let met = Met::Decided(decided);
        Ok(self.meets(env, ids, typ, None, met)?.map(|(tv, _)| tv))
    }

    /// [`Self::meets_rigid`] under `written`, the written test over a value
    /// of `typ`: the first variable met where the test's part over it,
    /// returned with it, holds `_` (through the types it names too) or an
    /// open cell, so types nothing inside it.
    pub fn meets_rigid_written(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        written: &Type,
    ) -> Result<Option<(TVar, Type)>> {
        Ok(self
            .meets(env, ids, typ, Some(written), Met::Rigid)?
            .and_then(|(tv, w)| w.map(|w| (tv, w))))
    }

    // XCR claude for claude: [bug/high] The closed-written-test rule judges the test's
    // spelling: meets refuses a written part over the variable only when `has_free() ||
    // has_bottom()`, and neither looks inside a typedef's body (a `Type::Ref`'s children
    // are its parameters), while the pattern compile expands only the written type's top.
    // So `type BotA = `A(_)` (or `type Bot = _`) and `(BotA, i64) as (`A(z), n) => z + 1.5`
    // over `'b: [`A(i64), `B(i64)] |x: ('b, i64)|` passes: matching_parts types z as ⊥,
    // the JIT panics ("kernel param `z`: runtime I64(3) does not match the compiled
    // Scalar(F64) slot") and the node-walk prints 4.5; `-> 'b .. => z` returns 3 typed 'b
    // and the caller's select finds no arm. A typedef at the top (`BotA as `A(z)`) and a
    // typedef parameter (`Wr<_>`) are refused. Main (968fa9bc0) is the same (round 1's
    // high, which the closedness rule was meant to close); CLAUDE.md, the skill and
    // design/tvar_constraints.md say a written test holding `_` is refused. Fix: judge
    // what matching_parts gives each bind inside the rigid cell (refuse ⊥ or an open
    // cell), which catches any spelling, or walk the written part with typedefs expanded;
    // pin t1c, t13, a2, a3, a6, a10 with closed twins. Review r14 B-r3-1, B-r3-2. probe:
    // ~/ptmp/cr29/scratch/review-r14/B-r3-1/a/{t1,t1c,t12,t13}.gx,
    // ~/ptmp/cr29/scratch/review-r14/B-r3-2/td/{a2,a3,a6,a7,a10}.gx (d6-typedef-bottom-01)
    // 2026-11-05 claude: The written-test closedness check now looks for _ through the
    // typedefs a written part names (Type::holds_bottom, over Type::holds), so (BotA, i64)
    // as .., Array<BotA> as .., `A(T) as .. over type T = _ are refused like a spelled _.
    // Pins in lang::select:
    // written_test_typedef_of_bottom_inside_a_declared_variable_refused,
    // written_test_typedef_holding_bottom_inside_a_declared_variable_refused,
    // written_test_typedef_holding_bottom_at_an_element_refused,
    // written_test_typedef_holding_bottom_returning_the_variable_refused (all four fail
    // without the fix), closed twin written_test_closed_typedef_inside_a_declared_variable.
    // Census of both repos unchanged. An abstract type's hidden representation is not
    // walked (its values carry the same rep the bound names).
    fn meets(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        written: Option<&Type>,
        met: Met,
    ) -> Result<Option<(TVar, Option<Type>)>> {
        crate::stack::ensure_sufficient(|| match self {
            Self::Ignore | Self::Bind(_) => Ok(None),
            Self::Or(alts) if !matches!(met, Met::Decided(_)) => {
                for a in alts.iter() {
                    if let Some(r) = a.meets(env, ids, typ, written, met)? {
                        return Ok(Some(r));
                    }
                }
                Ok(None)
            }
            _ => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let mut rigid: SmallVec<[TVar; 2]> = SmallVec::new();
                let (stop, held) = met.decided();
                union_members_stopping(
                    env,
                    typ,
                    &mut members,
                    &stop,
                    &held,
                    &mut |tv| match met {
                        Met::Decided(_) => {
                            if stop(tv) {
                                rigid.push(tv.clone())
                            }
                        }
                        _ => {
                            if let Some(c) = tv.open_cell().filter(|c| met.counts(c)) {
                                rigid.push(c)
                            }
                        }
                    },
                )?;
                for tv in rigid {
                    // beside a member of its shape a destructure chooses
                    // among members, so tests what it decided
                    if let Met::Decided(_) = met
                        && self.destructures(env, ids, &Type::TVar(tv.clone()))?
                        && !members.iter().any(|m| self.parts_over(env, ids, m).is_some())
                    {
                        continue;
                    }
                    if members.is_empty() || self.may_hold_value_of(env, ids, &tv)? {
                        match written {
                            None => return Ok(Some((tv, None))),
                            Some(w) if w.has_free() || w.holds_bottom(env) => {
                                return Ok(Some((tv, Some(w.clone()))));
                            }
                            Some(_) => (),
                        }
                    }
                }
                if let Self::Or(alts) = self {
                    for a in alts.iter() {
                        if let Some(r) = a.meets(env, ids, typ, written, met)? {
                            return Ok(Some(r));
                        }
                    }
                    return Ok(None);
                }
                for m in members.iter() {
                    if !self.may_hold_met(env, ids, m, met)? {
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
                        if let Some(r) = p.meets(env, ids, t, w, met)? {
                            return Ok(Some(r));
                        }
                    }
                }
                Ok(None)
            }
        })
    }

    /// Whether this pattern only takes a value apart: a tuple, a struct
    /// naming every field, a slice or list with a rest, an abstract
    /// payload, holding only names, `_` and such patterns. It chooses no
    /// value, tag or length.
    pub fn only_destructures(&self) -> bool {
        crate::stack::ensure_sufficient(|| match self {
            Self::Ignore | Self::Bind(_) => true,
            Self::Literal(..)
            | Self::Variant { .. }
            | Self::Or(_)
            | Self::Slice { .. }
            | Self::Struct { exhaustive: false, .. } => false,
            Self::SlicePrefix { .. }
            | Self::SliceSuffix { .. }
            | Self::Tuple { .. }
            | Self::Struct { .. }
            | Self::Abstract { .. } => {
                let mut only = true;
                self.for_each_sub(&mut |p| only &= p.only_destructures());
                only
            }
        })
    }

    /// Whether this pattern [only destructures](Self::only_destructures)
    /// and takes apart every value of `typ`, each of its parts of the shape
    /// of every member there but open cells: it decides the shape of an
    /// open part as an annotation would, and tests nothing a later arm
    /// could be told apart by.
    fn destructures(&self, env: &Env, ids: &AbstractOf, typ: &Type) -> Result<bool> {
        fn fits(
            p: &StructurePattern,
            env: &Env,
            ids: &AbstractOf,
            typ: &Type,
        ) -> Result<bool> {
            if matches!(p, StructurePattern::Ignore | StructurePattern::Bind(_)) {
                return Ok(true);
            }
            crate::stack::ensure_sufficient(|| {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                // an open cell there, the arm's own, takes the shape it
                // destructures
                let open = union_members_open(env, typ, &mut members)?;
                if members.is_empty() {
                    return Ok(open);
                }
                for m in members.iter() {
                    let Some(parts) = p.parts_over(env, ids, m) else { return Ok(false) };
                    for (q, t) in parts {
                        if !fits(q, env, ids, &t)? {
                            return Ok(false);
                        }
                    }
                }
                Ok(true)
            })
        }
        Ok(self.only_destructures() && fits(self, env, ids, typ)?)
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
        self.may_hold_met(env, ids, typ, Met::Rigid)
    }

    /// [`Self::may_hold`], a cell `met` reads as decided taken as the open
    /// cell it was.
    fn may_hold_met(
        &self,
        env: &Env,
        ids: &AbstractOf,
        typ: &Type,
        met: Met,
    ) -> Result<bool> {
        let (stop, held) = met.decided();
        crate::stack::ensure_sufficient(|| match self {
            Self::Ignore | Self::Bind(_) => Ok(true),
            Self::Literal(..) if matches!(typ, Type::TVar(tv) if stop(tv)) => Ok(true),
            Self::Literal(v, _) => Type::of_literal(v)?.could_match(env, typ),
            Self::Or(alts) => {
                for a in alts.iter() {
                    if a.may_hold_met(env, ids, typ, met)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            _ => {
                let mut members: SmallVec<[Type; 8]> = SmallVec::new();
                let mut open = false;
                union_members_stopping(
                    env,
                    typ,
                    &mut members,
                    &stop,
                    &held,
                    &mut |_| open = true,
                )?;
                if open {
                    return Ok(true);
                }
                'members: for m in members.iter() {
                    if let Type::Any = m {
                        return Ok(true);
                    }
                    let Some(parts) = self.parts_over(env, ids, m) else { continue };
                    for (p, t) in parts {
                        if !p.may_hold_met(env, ids, &t, met)? {
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

    // CR claude for claude: [bug/high] D6 misses a pattern inside the element of a
    // constructor-variable collection: parts_over has no case for an unfilled
    // `Type::App` ('c<'b>, or the element quantifier of a `c: Collection` parameter), so
    // may_hold is false for that member, meets skips it and nothing inside the element is
    // judged. `|c: Collection| -> [string, null] select c { [`A(z), ..] => z, _ => null }`
    // over `[`A(3)]` prints "a string of length 0" fused and logs "no arm matches" with
    // --no-fusion (graphix-fuzz check: DIVERGENCE); `[`A(z), ..] => z + 1.5` panics the JIT
    // link fused and prints 4.5 node-walked; a list pattern and a tuple element do the
    // same. The written twin `Array<[`A(i64), `B(i64)]> as [`A(z), ..]` works. Main
    // (968fa9bc0) is the same. Fix: in parts_over, may_hold and written_parts, read an
    // unfilled App's argument as a slice pattern's element type, so the walk reaches the
    // rigid element; pin ct4 and ct13 with the written twin. Review r14 B-r3-1. probe:
    // ~/ptmp/cr29/scratch/review-r14/B-r3-1/i/{ct1,ct4,ct7,ct8,ct13,ct11}.gx
    // (d6-collection-element-01)
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

/// Which open cells [`StructurePattern::meets`] meets.
#[derive(Clone, Copy)]
enum Met<'a> {
    Rigid,
    Undecided(&'a dyn Fn(&TVar) -> bool),
    /// Bound cells, each met as the open cell it was.
    Decided(&'a Decided<'a>),
}

impl<'a> Met<'a> {
    /// The cells read as the open cells they were, and the bindings read
    /// as they were.
    fn decided(
        self,
    ) -> (impl Fn(&TVar) -> bool + 'a, impl Fn(&TVar) -> Option<Type> + 'a) {
        let d = match self {
            Self::Decided(d) => Some(d),
            Self::Rigid | Self::Undecided(_) => None,
        };
        (
            move |tv: &TVar| d.is_some_and(|d| (d.cell)(tv)),
            move |tv: &TVar| d.and_then(|d| (d.held)(tv)),
        )
    }

    fn counts(self, c: &TVar) -> bool {
        match self {
            Self::Rigid => c.is_rigid(),
            Self::Undecided(f) => !c.is_rigid() && f(c),
            Self::Decided(_) => false,
        }
    }
}

/// The cells a select's arms decided, read as they stood before.
pub struct Decided<'a> {
    /// A cell that was open.
    pub cell: &'a dyn Fn(&TVar) -> bool,
    /// The binding a cell held before a decision beneath rewrote it.
    pub held: &'a dyn Fn(&TVar) -> Option<Type>,
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
    union_members_stopping(env, t, out, &|_| false, &|_| None, open)
}

/// [`union_members_with`], where a tvar `stop` takes is handed to `open`
/// as it stands, bound or not, and one `held` reads is that binding.
fn union_members_stopping(
    env: &Env,
    t: &Type,
    out: &mut SmallVec<[Type; 8]>,
    stop: &dyn Fn(&TVar) -> bool,
    held: &dyn Fn(&TVar) -> Option<Type>,
    open: &mut dyn FnMut(&TVar),
) -> Result<()> {
    fn go(
        env: &Env,
        t: &Type,
        out: &mut SmallVec<[Type; 8]>,
        unfolded: &mut Unfolded,
        stop: &dyn Fn(&TVar) -> bool,
        held: &dyn Fn(&TVar) -> Option<Type>,
        open: &mut dyn FnMut(&TVar),
    ) -> Result<()> {
        if let Type::TVar(tv) = t {
            if stop(tv) {
                open(tv);
                return Ok(());
            }
            if let Some(b) = held(tv) {
                return go(env, &b, out, unfolded, stop, held, open);
            }
        }
        crate::stack::ensure_sufficient(|| match t.deref_cloned() {
            None => {
                if let Type::TVar(tv) = t {
                    open(tv)
                }
                Ok(())
            }
            Some(t) => match &t {
                Type::Set(ts) => ts
                    .iter()
                    .try_for_each(|t| go(env, t, out, unfolded, stop, held, open)),
                Type::Ref(tr) => {
                    tr.resolve_in(env);
                    match unfolded.first(&t) {
                        true => {
                            go(env, &t.lookup_ref(env)?, out, unfolded, stop, held, open)
                        }
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
    go(env, t, out, &mut Unfolded::default(), stop, held, open)
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
